/* $Id: petscspai.h 62365 2018-10-17 13:40:43Z carniato $ */
/*
     Include file for the SPAI interface to PETSc. You should include
  this file if you wish to set SPAI options directly from your program.
*/
#if !defined(__SPAI_PACKAGE)
#define __SPAI_PACKAGE
#include <petscpc.h>

PETSC_EXTERN PetscErrorCode MatDumpSPAI(Mat,FILE*);
PETSC_EXTERN PetscErrorCode VecDumpSPAI(Vec,FILE*);

#endif



