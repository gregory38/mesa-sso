/*
 * Mesa 3-D graphics library
 *
 * Copyright (C) 2013-2013  Gregory Hainaut <gregory.hainaut@gmail.com>
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
 * GREGORY HAINAUT BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

/**
 * \file pipelineobj.c
 * \author Hainaut Gregory <gregory.hainaut@gmail.com>
 *
 * Implementation of pipeline object related API functions. Based on GL_ARB_separate_shader_objects
 * extension.
 *
 *
 * XXX things to do:
 * 1. Check that the right error code is generated for all _mesa_error() calls.
 * 2. Insert FLUSH_VERTICES calls in various places
 */

#include "main/glheader.h"
#include "main/context.h"
#include "main/dispatch.h"
#include "main/enums.h"
#include "main/hash.h"
#include "main/mfeatures.h"
#include "main/mtypes.h"
#include "main/pipelineobj.h"
#include "main/shaderapi.h"
#include "main/shaderobj.h"
#include "main/transformfeedback.h"
#include "main/uniforms.h"
#include "program/program.h"
#include "program/prog_parameter.h"
#include "ralloc.h"
#include <stdbool.h>
#include "../glsl/glsl_parser_extras.h"
#include "../glsl/ir_uniform.h"

/**
 * Delete a pipeline object.
 */
void
_mesa_delete_pipeline_object(struct gl_context *ctx, struct gl_pipeline_object *obj)
{
   _mesa_reference_shader_program(ctx, &obj->_CurrentFragmentProgram, NULL);
   _mesa_reference_shader_program(ctx, &obj->CurrentFragmentProgram, NULL);
   _mesa_reference_shader_program(ctx, &obj->CurrentVertexProgram, NULL);
   _mesa_reference_shader_program(ctx, &obj->CurrentGeometryProgram, NULL);
   _mesa_reference_shader_program(ctx, &obj->ActiveProgram, NULL);
   _glthread_DESTROY_MUTEX(obj->Mutex);
   ralloc_free(obj);
}

/**
 * Allocate and initialize a new pipeline object.
 */
static struct gl_pipeline_object *
_mesa_new_pipeline_object(struct gl_context *ctx, GLuint name)
{
   struct gl_pipeline_object *obj = rzalloc(NULL, struct gl_pipeline_object);
   if (obj) {
      obj->Name = name;
      _glthread_INIT_MUTEX(obj->Mutex);
      obj->RefCount = 1;
      obj->Flags = _mesa_get_shader_flags();
      obj->InfoLog = ralloc_strdup(obj, "");
   }

   return obj;
}

/**
 * Initialize pipeline object state for given context.
 */
void
_mesa_init_pipeline(struct gl_context *ctx)
{
   ctx->Pipeline.Objects = _mesa_NewHashTable();

   ctx->Pipeline.Current = NULL;

   /* Install a default Pipeline */
   ctx->Pipeline.Default = _mesa_new_pipeline_object(ctx, 0);
   _mesa_reference_pipeline_object(ctx, &ctx->_Shader, ctx->Pipeline.Default);
}


/**
 * Callback for deleting a pipeline object.  Called by _mesa_HashDeleteAll().
 */
static void
delete_pipelineobj_cb(GLuint id, void *data, void *userData)
{
   struct gl_pipeline_object *obj = (struct gl_pipeline_object *) data;
   struct gl_context *ctx = (struct gl_context *) userData;
   _mesa_delete_pipeline_object(ctx, obj);
}


/**
 * Free pipeline state for given context.
 */
void
_mesa_free_pipeline_data(struct gl_context *ctx)
{
   _mesa_HashDeleteAll(ctx->Pipeline.Objects, delete_pipelineobj_cb, ctx);
   _mesa_DeleteHashTable(ctx->Pipeline.Objects);

   _mesa_reference_pipeline_object(ctx, &ctx->_Shader, NULL);
   _mesa_delete_pipeline_object(ctx, ctx->Pipeline.Default);

}

/**
 * Look up the pipeline object for the given ID.
 *
 * \returns
 * Either a pointer to the pipeline object with the specified ID or \c NULL for
 * a non-existent ID.  The spec defines ID 0 as being technically
 * non-existent.
 */
static inline struct gl_pipeline_object *
lookup_pipeline_object(struct gl_context *ctx, GLuint id)
{
   if (id == 0)
      return NULL;
   else
      return (struct gl_pipeline_object *)
         _mesa_HashLookup(ctx->Pipeline.Objects, id);
}

/**
 * Add the given pipeline object to the pipeline object pool.
 */
static void
save_pipeline_object(struct gl_context *ctx, struct gl_pipeline_object *obj)
{
   if (obj->Name > 0) {
      _mesa_HashInsert(ctx->Pipeline.Objects, obj->Name, obj);
   }
}

/**
 * Remove the given pipeline object from the pipeline object pool.
 * Do not deallocate the pipeline object though.
 */
static void
remove_pipeline_object(struct gl_context *ctx, struct gl_pipeline_object *obj)
{
   if (obj->Name > 0) {
      _mesa_HashRemove(ctx->Pipeline.Objects, obj->Name);
   }
}

/**
 * Set ptr to obj w/ reference counting.
 * Note: this should only be called from the _mesa_reference_pipeline_object()
 * inline function.
 */
void
_mesa_reference_pipeline_object_(struct gl_context *ctx,
                                 struct gl_pipeline_object **ptr,
                                 struct gl_pipeline_object *obj)
{
   assert(*ptr != obj);

   if (*ptr) {
      /* Unreference the old pipeline object */
      GLboolean deleteFlag = GL_FALSE;
      struct gl_pipeline_object *oldObj = *ptr;

      _glthread_LOCK_MUTEX(oldObj->Mutex);
      ASSERT(oldObj->RefCount > 0);
      oldObj->RefCount--;
#if 0
      printf("obj %p %d DECR to %d\n",
            (void *) oldObj, oldObj->Name, oldObj->RefCount);
#endif
      deleteFlag = (oldObj->RefCount == 0);
      _glthread_UNLOCK_MUTEX(oldObj->Mutex);

      if (deleteFlag) {
         _mesa_delete_pipeline_object(ctx, oldObj);
      }

      *ptr = NULL;
   }
   ASSERT(!*ptr);

   if (obj) {
      /* reference new pipeline object */
      _glthread_LOCK_MUTEX(obj->Mutex);
      if (obj->RefCount == 0) {
         /* this pipeline's being deleted (look just above) */
         /* Not sure this can every really happen.  Warn if it does. */
         _mesa_problem(NULL, "referencing deleted pipeline object");
         *ptr = NULL;
      }
      else {
         obj->RefCount++;
#if 0
         printf("obj %p %d INCR to %d\n",
               (void *) obj, obj->Name, obj->RefCount);
#endif
         *ptr = obj;
      }
      _glthread_UNLOCK_MUTEX(obj->Mutex);
   }
}

/**
 * Bound program to severals stages of the pipeline
 */
void GLAPIENTRY
_mesa_UseProgramStages(GLuint pipeline, GLbitfield stages, GLuint program)
{
   GET_CURRENT_CONTEXT(ctx);

   struct gl_pipeline_object *pipe = lookup_pipeline_object(ctx, pipeline);
   struct gl_shader_program *shProg = NULL;

   if (!pipe) {
      _mesa_error(ctx, GL_INVALID_OPERATION, "glUseProgramStages(pipeline)");
      return;
   }

   /* Object is created by any Pipeline call but glGenProgramPipelines,
    * glIsProgramPipeline and GetProgramPipelineInfoLog
    */
   pipe->EverBound = GL_TRUE;

   /* NOT YET SUPPORTED:
    * GL_TESS_CONTROL_SHADER_BIT
    * GL_TESS_EVALUATION_SHADER_BIT
    * GL_COMPUTE_SHADER_BIT
    */
   GLbitfield any_valid_stages = GL_VERTEX_SHADER_BIT | GL_FRAGMENT_SHADER_BIT;
   if (_mesa_is_desktop_gl(ctx) && ctx->Extensions.ARB_geometry_shader4)
      any_valid_stages |= GL_GEOMETRY_SHADER_BIT;

   if (stages != GL_ALL_SHADER_BITS && (stages  & ~any_valid_stages) != 0) {
      _mesa_error(ctx, GL_INVALID_VALUE, "glUseProgramStages(Stages)");
      return;
   }

   /*
    *  An INVALID_OPERATION error is generated :
    *  by UseProgramStages if the program pipeline object it refers to is current
    *  and the current transform feedback object is active and not paused;
    */
   /*
    * 6a. Should the fragment shader program object be allowed to changed
    * within transform feedback mode?
    * RESOLVED:  No, this should generate an GL_INVALID_OPERATION error.
    */
   if (ctx->_Shader == pipe) {
      if (_mesa_is_xfb_active_and_unpaused(ctx)) {
         _mesa_error(ctx, GL_INVALID_OPERATION,
               "glUseProgramStages(transform feedback active)");
         return;
      }
   }

   if (program) {
      /* An INVALID_OPERATION error is generated if program is the name of a
       * shader object
       */
      struct gl_shader *sh = _mesa_lookup_shader(ctx, program);
      if (sh != NULL) {
         _mesa_error(ctx, GL_INVALID_OPERATION,
               "glUseProgramStages(progam is a shader object)");
         return;
      }

      /* An INVALID_VALUE error is generated if program is not the name of ei-
       * ther a program or shader object
       */
      shProg = _mesa_lookup_shader_program(ctx, program);
      if (shProg == NULL) {
         _mesa_error(ctx, GL_INVALID_VALUE,
               "glUseProgramStages(progam is not a program object)");
         return;
      }

      /* An INVALID_OPERATION error is generated if the program object named
       * by program was linked without the PROGRAM_SEPARABLE parameter set, has
       * not been linked, or was last linked unsuccessfully. The corresponding shader
       * stages in pipeline are not modified.
       */
      if (!shProg->LinkStatus) {
         _mesa_error(ctx, GL_INVALID_OPERATION,
               "glUseProgramStages(program not linked)");
         return;
      }
      if (!shProg->SeparateShader) {
         _mesa_error(ctx, GL_INVALID_OPERATION,
               "glUseProgramStages(program wasn't linked with the PROGRAM_SEPARABLE flag)");
         return;
      }
   }

   /*
    *  7.  What happens if you have a program object current for a shader stage,
    *    but the program object doesn't contain an executable for that stage?

    *    RESOLVED:  This is not an error; instead it is as though there were no
    *    program bound to that stage.  We have two different notions for
    *    programs bound to shader stages.  A program is "current" for a stage
    *    if it bound to that stage in the active program pipeline object.  A
    *    program is "active" for a stage if it is current and it has an
    *    executable for this stage.  In this case, the program would be current
    *    but not active.

    *    When no program is active for a stage, the stage will be replaced with
    *    fixed functionality logic (compatibility profile vertex and fragment),
    *    disabled (tessellation control and evaluation, geometry), or have
    *    undefined results (core profile vertex and fragment).
    */
   if (stages & GL_VERTEX_SHADER_BIT)
      _mesa_use_shader_program(ctx, GL_VERTEX_SHADER, shProg, pipe);
   if (stages & GL_FRAGMENT_SHADER_BIT)
      _mesa_use_shader_program(ctx, GL_FRAGMENT_SHADER, shProg, pipe);
   if (stages & GL_GEOMETRY_SHADER_BIT)
      _mesa_use_shader_program(ctx, GL_GEOMETRY_SHADER_ARB, shProg, pipe);

   /* Validation would need to be redone */
   pipe->Validated = GL_FALSE;
}

/**
 * Use the named shader program for subsequent glUniform calls (if pipeline bound)
 */
void GLAPIENTRY
_mesa_ActiveShaderProgram(GLuint pipeline, GLuint program)
{
   GET_CURRENT_CONTEXT(ctx);
   struct gl_shader_program *shProg = (program != 0)
      ? _mesa_lookup_shader_program_err(ctx, program, "glActiveShaderProgram(program)")
      : NULL;

   struct gl_pipeline_object *pipe = lookup_pipeline_object(ctx, pipeline);

   if (!pipe) {
      _mesa_error(ctx, GL_INVALID_OPERATION, "glActiveShaderProgram(pipeline)");
      return;
   }

   /* Object is created by any Pipeline call but glGenProgramPipelines,
    * glIsProgramPipeline and GetProgramPipelineInfoLog
    */
   pipe->EverBound = GL_TRUE;

   if ((shProg != NULL) && !shProg->LinkStatus) {
      _mesa_error(ctx, GL_INVALID_OPERATION,
            "glActiveShaderProgram(program %u not linked)", shProg->Name);
      return;
   }

   _mesa_reference_shader_program(ctx, &pipe->ActiveProgram, shProg);
}

/**
 * Make program of the pipeline current
 */
void GLAPIENTRY
_mesa_BindProgramPipeline(GLuint pipeline)
{
   GET_CURRENT_CONTEXT(ctx);
   struct gl_pipeline_object *newObj = NULL;

   if (ctx->_Shader->Name == pipeline)
      return;   /* rebinding the same pipeline object- no change */

   /*
    *  An INVALID_OPERATION error is generated :
    *  by BindProgramPipeline if the current transform feedback object is active
    *  and not paused;
    */
   if (_mesa_is_xfb_active_and_unpaused(ctx)) {
      _mesa_error(ctx, GL_INVALID_OPERATION,
            "glBindProgramPipeline(transform feedback active)");
      return;
   }

   /*
    * Get pointer to new pipeline object (newObj)
    */
   if (pipeline) {
      /* non-default pipeline object */
      newObj = lookup_pipeline_object(ctx, pipeline);
      if (!newObj) {
         _mesa_error(ctx, GL_INVALID_OPERATION, "glBindProgramPipeline(non-gen name)");
         return;
      }

      /* Object is created by any Pipeline call but glGenProgramPipelines,
       * glIsProgramPipeline and GetProgramPipelineInfoLog
       */
      newObj->EverBound = GL_TRUE;
   }

   /* First bind the Pipeline to pipeline binding point */
   _mesa_reference_pipeline_object(ctx, &ctx->Pipeline.Current, newObj);

   /* Spec say:
    * if any program is bound to the context, the current pipeline object is
    * ignored.
    */
   if (&ctx->Shader != ctx->_Shader) {
      if (pipeline) {
         /* Bound the pipeline to the current program and
          * restore the pipeline state
          */
         _mesa_reference_pipeline_object(ctx, &ctx->_Shader, newObj);
      } else {
         /* Unbind the pipeline */
         _mesa_reference_pipeline_object(ctx, &ctx->_Shader, ctx->Pipeline.Default);
      }
      FLUSH_VERTICES(ctx, _NEW_PROGRAM | _NEW_PROGRAM_CONSTANTS);
      /* FIXME */
      if (ctx->Driver.UseProgram)
         ctx->Driver.UseProgram(ctx, NULL);
   }
}

/**
 * Delete a set of pipeline objects.
 *
 * \param n      Number of pipeline objects to delete.
 * \param ids    pipeline of \c n pipeline object IDs.
 */
void GLAPIENTRY
_mesa_DeleteProgramPipelines(GLsizei n, const GLuint *pipelines)
{
   GET_CURRENT_CONTEXT(ctx);
   GLsizei i;

   if (n < 0) {
      _mesa_error(ctx, GL_INVALID_VALUE, "glDeleteProgramPipelines(n<0)");
      return;
   }

   for (i = 0; i < n; i++) {
      struct gl_pipeline_object *obj = lookup_pipeline_object(ctx, pipelines[i]);

      if (obj) {
         ASSERT(obj->Name == pipelines[i]);

         /* If the pipeline object is currently bound, the spec says "If an object that is
          * currently bound is deleted, the binding for that object
          * reverts to zero and no program pipeline object becomes current."
          */
         if (obj == ctx->Pipeline.Current) {
            _mesa_BindProgramPipeline(0);
         }

         /* The ID is immediately freed for re-use */
         remove_pipeline_object(ctx, obj);

         /* Unreference the pipeline object.
          * If refcount hits zero, the object will be deleted.
          */
         _mesa_reference_pipeline_object(ctx, &obj, NULL);
      }
   }
}

/**
 * Generate a set of unique pipeline object IDs and store them in \c pipelines.
 * \param n       Number of IDs to generate.
 * \param pipelines  pipeline of \c n locations to store the IDs.
 */
void GLAPIENTRY
_mesa_GenProgramPipelines(GLsizei n, GLuint *pipelines)
{
   GET_CURRENT_CONTEXT(ctx);

   GLuint first;
   GLint i;

   if (n < 0) {
      _mesa_error(ctx, GL_INVALID_VALUE, "glGenProgramPipelines(n<0)");
      return;
   }

   if (!pipelines) {
      return;
   }

   first = _mesa_HashFindFreeKeyBlock(ctx->Pipeline.Objects, n);

   for (i = 0; i < n; i++) {
      struct gl_pipeline_object *obj;
      GLuint name = first + i;

      obj = _mesa_new_pipeline_object(ctx, name);
      if (!obj) {
         _mesa_error(ctx, GL_OUT_OF_MEMORY, "glGenProgramPipelines");
         return;
      }

      save_pipeline_object(ctx, obj);
      pipelines[i] = first + i;
   }

}

/**
 * Determine if ID is the name of an pipeline object.
 *
 * \param id  ID of the potential pipeline object.
 * \return  \c GL_TRUE if \c id is the name of a pipeline object,
 *          \c GL_FALSE otherwise.
 */
GLboolean GLAPIENTRY
_mesa_IsProgramPipeline(GLuint pipeline)
{
   GET_CURRENT_CONTEXT(ctx);

   struct gl_pipeline_object *obj = lookup_pipeline_object(ctx, pipeline);
   if (obj == NULL)
      return GL_FALSE;

   return obj->EverBound;
}

/**
 * glGetProgramPipelineiv() - get pipeline shader state.
 */
void GLAPIENTRY
_mesa_GetProgramPipelineiv(GLuint pipeline, GLenum pname, GLint *params)
{
   GET_CURRENT_CONTEXT(ctx);
   struct gl_pipeline_object *pipe = lookup_pipeline_object(ctx, pipeline);

   /* Are geometry shaders available in this context?
    */
   const bool has_gs = _mesa_is_desktop_gl(ctx) && ctx->Extensions.ARB_geometry_shader4;

   if (!pipe) {
      _mesa_error(ctx, GL_INVALID_OPERATION, "glGetProgramPipelineiv(pipeline)");
      return;
   }

   /* Object is created by any Pipeline call but glGenProgramPipelines,
    * glIsProgramPipeline and GetProgramPipelineInfoLog
    */
   pipe->EverBound = GL_TRUE;

   switch (pname) {
   case GL_ACTIVE_PROGRAM:
      *params = pipe->ActiveProgram ? pipe->ActiveProgram->Name : 0;
      return;
   case GL_INFO_LOG_LENGTH:
      *params = pipe->Validated;
      return;
   case GL_VALIDATE_STATUS:
      *params = pipe->Validated;
      return;
   case GL_VERTEX_SHADER:
      *params = pipe->CurrentVertexProgram ? pipe->CurrentVertexProgram->Name : 0;
      return;
   case GL_TESS_EVALUATION_SHADER:
      /* NOT YET SUPPORTED */
      break;
   case GL_TESS_CONTROL_SHADER:
      /* NOT YET SUPPORTED */
      break;
   case GL_GEOMETRY_SHADER:
      if (!has_gs) break;
      *params = pipe->CurrentGeometryProgram ? pipe->CurrentGeometryProgram->Name : 0;;
      return;
   case GL_FRAGMENT_SHADER:
      *params = pipe->CurrentFragmentProgram ? pipe->CurrentFragmentProgram->Name : 0;;
      return;
   case GL_COMPUTE_SHADER:
      /* NOT YET SUPPORTED */
      break;
   default:
      break;
   }

   _mesa_error(ctx, GL_INVALID_ENUM, "glGetProgramPipelineiv(pname=%s)",
         _mesa_lookup_enum_by_nr(pname));
}

static GLboolean
ProgramEnabledEverywhere(struct gl_pipeline_object *pipe,
                         struct gl_shader_program *prog,
                         char *errMsg, size_t errMsgLength)
{
   if (!prog) return GL_TRUE;

   GLboolean status = GL_TRUE;

   if (prog->_LinkedShaders[MESA_SHADER_VERTEX]) {
      if (pipe->CurrentVertexProgram) {
         if (prog->Name != pipe->CurrentVertexProgram->Name) {
            status = GL_FALSE;
         }
      } else {
         status = GL_FALSE;
      }
   }

   if (prog->_LinkedShaders[MESA_SHADER_FRAGMENT]) {
      if (pipe->CurrentFragmentProgram) {
         if (prog->Name != pipe->CurrentFragmentProgram->Name) {
            status = GL_FALSE;
         }
      } else {
         status = GL_FALSE;
      }
   }

   if (prog->_LinkedShaders[MESA_SHADER_GEOMETRY]) {
      if (pipe->CurrentGeometryProgram) {
         if (prog->Name != pipe->CurrentGeometryProgram->Name) {
            status = GL_FALSE;
         }
      } else {
         status = GL_FALSE;
      }
   }

   if (!status) {
      _mesa_snprintf(errMsg, errMsgLength,
            "Program %d is not active for all shaders that was linked",
            prog->Name);
   }

   return status;
}

extern GLboolean
_mesa_validate_program_pipeline(struct gl_context* ctx,
                                struct gl_pipeline_object *pipe,
                                GLboolean IsBound)
{
   char errMsg[200] = "";
   const GLuint errMsgLength = 200;

   pipe->Validated = GL_FALSE;
   pipe->InfoLog = ralloc_strdup(pipe, "");

   /*
    * A program object is active for at least one, but not all of the shader
    * stages that were present when the program was linked.
    */
   if (!ProgramEnabledEverywhere(pipe, pipe->CurrentVertexProgram, errMsg, errMsgLength)) {
      if (IsBound)
         _mesa_error(ctx, GL_INVALID_OPERATION,
               "glValidateProgramPipeline failed to validate the pipeline");
      goto err;
   }
   if (!ProgramEnabledEverywhere(pipe, pipe->CurrentGeometryProgram, errMsg, errMsgLength)) {
      if (IsBound)
         _mesa_error(ctx, GL_INVALID_OPERATION,
               "glValidateProgramPipeline failed to validate the pipeline");
      goto err;
   }
   if (!ProgramEnabledEverywhere(pipe, pipe->CurrentFragmentProgram, errMsg, errMsgLength)) {
      if (IsBound)
         _mesa_error(ctx, GL_INVALID_OPERATION,
               "glValidateProgramPipeline failed to validate the pipeline");
      goto err;
   }

   /*
    * One program object is active for at least two shader stages and a second
    * program is active for a shader stage between two stages for which the
    * first program was active. The active compute shader is ignored for the
    * purposes of this test.
    */
   /* Without Tesselation, the only case is geometry shader between Fragment and Vertex */
   if (pipe->CurrentGeometryProgram && pipe->CurrentFragmentProgram
         && pipe->CurrentVertexProgram) {
      if (pipe->CurrentVertexProgram->Name == pipe->CurrentGeometryProgram->Name &&
          pipe->CurrentGeometryProgram->Name != pipe->CurrentVertexProgram->Name) {
         _mesa_snprintf(errMsg, errMsgLength,
               "Program %d is active for geometry stage between two"
               "stages for which another program %d is active",
               pipe->CurrentGeometryProgram->Name, pipe->CurrentVertexProgram->Name);
         if (IsBound)
            _mesa_error(ctx, GL_INVALID_OPERATION,
                  "glValidateProgramPipeline failed to validate the pipeline");
         goto err;
      }
   }

   /*
    * There is an active program for tessellation control, tessellation evaluation, or
    * geometry stages with corresponding executable shader, but there is no active
    * program with executable vertex shader.
    */
   if (!pipe->CurrentVertexProgram && pipe->CurrentGeometryProgram) {
         _mesa_snprintf(errMsg, errMsgLength, "Program miss a vertex shader");
      if (IsBound)
         _mesa_error(ctx, GL_INVALID_OPERATION,
               "glValidateProgramPipeline failed to validate the pipeline");
      goto err;
   }

   /*
    * There is no current program object specified by UseProgram, there is a cur-
    * rent program pipeline object, and the current program for any shader stage
    * has been relinked since being applied to the pipeline object via UsePro-
    * gramStages with the PROGRAM_SEPARABLE parameter set to FALSE.
    */
   if (pipe->CurrentVertexProgram && !pipe->CurrentVertexProgram->SeparateShader) {
      _mesa_snprintf(errMsg, errMsgLength,
            "Program %d was relinked without PROGRAM_SEPARABLE state",
            pipe->CurrentVertexProgram->Name);
      if (IsBound)
         _mesa_error(ctx, GL_INVALID_OPERATION,
               "glValidateProgramPipeline failed to validate the pipeline");
      goto err;
   }
   if (pipe->CurrentFragmentProgram && !pipe->CurrentFragmentProgram->SeparateShader) {
      _mesa_snprintf(errMsg, errMsgLength,
            "Program %d was relinked without PROGRAM_SEPARABLE state",
            pipe->CurrentFragmentProgram->Name);
      if (IsBound)
         _mesa_error(ctx, GL_INVALID_OPERATION,
               "glValidateProgramPipeline failed to validate the pipeline");
      goto err;
   }
   if (pipe->CurrentGeometryProgram && !pipe->CurrentGeometryProgram->SeparateShader) {
      _mesa_snprintf(errMsg, errMsgLength,
            "Program %d was relinked without PROGRAM_SEPARABLE state",
            pipe->CurrentGeometryProgram->Name);
      if (IsBound)
         _mesa_error(ctx, GL_INVALID_OPERATION,
               "glValidateProgramPipeline failed to validate the pipeline");
      goto err;
   }

   /*
    * The sum of the number of active samplers for each active program exceeds
    * the maximum number of texture image units allowed.
    *
    * Any two active samplers in the set of active program objects are of different
    * types, but refer to the same texture image unit.
    */
   if (!_mesa_sampler_uniforms_pipeline_are_valid(pipe, errMsg, errMsgLength)) {
      if (IsBound)
         _mesa_error(ctx, GL_INVALID_OPERATION,
               "glValidateProgramPipeline failed to validate the pipeline");
      goto err;
   }

   /*
    * The sum of the number of active shader storage blocks used by the current
    * program objects exceeds the combined limit on the number of active shader
    * storage blocks (the value of MAX_COMBINED_SHADER_STORAGE_BLOCKS).
    */
   /* NOT YET SUPPORTED */

   pipe->Validated = GL_TRUE;

err:
   if (!pipe->Validated) {
      /* update info log */
      if (pipe->InfoLog) {
         ralloc_free(pipe->InfoLog);
      }
      pipe->InfoLog = ralloc_strdup(pipe, errMsg);
   }

   return pipe->Validated;
}

/**
 * Check compatibility of pipeline's program
 */
void GLAPIENTRY
_mesa_ValidateProgramPipeline(GLuint pipeline)
{
   GET_CURRENT_CONTEXT(ctx);

   struct gl_pipeline_object *pipe = lookup_pipeline_object(ctx, pipeline);

   if (!pipe) {
      _mesa_error(ctx, GL_INVALID_OPERATION, "glValidateProgramPipeline(pipeline)");
      return;
   }

   _mesa_validate_program_pipeline(ctx, pipe, (ctx->_Shader->Name == pipe->Name));
}

void GLAPIENTRY
_mesa_GetProgramPipelineInfoLog(GLuint pipeline, GLsizei bufSize,
      GLsizei *length, GLchar *infoLog)
{
   GET_CURRENT_CONTEXT(ctx);

   struct gl_pipeline_object *pipe = lookup_pipeline_object(ctx, pipeline);

   if (!pipe) {
      _mesa_error(ctx, GL_INVALID_VALUE, "glGetProgramPipelineInfoLog(pipeline)");
      return;
   }

   if (bufSize < 0) {
      _mesa_error(ctx, GL_INVALID_VALUE, "glGetProgramPipelineInfoLog(bufSize)");
      return;
   }

   _mesa_copy_string(infoLog, bufSize, length, pipe->InfoLog);
}
