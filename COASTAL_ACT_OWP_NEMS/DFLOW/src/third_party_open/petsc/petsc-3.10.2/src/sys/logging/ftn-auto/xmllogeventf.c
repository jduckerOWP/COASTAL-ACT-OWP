#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* xmllogevent.c */
/* Fortran interface file */

/*
* This file was generated automatically by bfort from the C source
* file.  
 */

#ifdef PETSC_USE_POINTER_CONVERSION
#if defined(__cplusplus)
extern "C" { 
#endif 
extern void *PetscToPointer(void*);
extern int PetscFromPointer(void *);
extern void PetscRmPointer(void*);
#if defined(__cplusplus)
} 
#endif 

#else

#define PetscToPointer(a) (*(PetscFortranAddr *)(a))
#define PetscFromPointer(a) (PetscFortranAddr)(a)
#define PetscRmPointer(a)
#endif

#include "petsclog.h"
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petsclogsetthreshold_ PETSCLOGSETTHRESHOLD
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petsclogsetthreshold_ petsclogsetthreshold
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void PETSC_STDCALL  petsclogsetthreshold_(PetscLogDouble *newThresh,PetscLogDouble *oldThresh, int *__ierr){
*__ierr = PetscLogSetThreshold(*newThresh,oldThresh);
}
#if defined(__cplusplus)
}
#endif
