#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* pipefgmres.c */
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

#include "petscksp.h"
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define ksppipefgmressetshift_ KSPPIPEFGMRESSETSHIFT
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define ksppipefgmressetshift_ ksppipefgmressetshift
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void PETSC_STDCALL  ksppipefgmressetshift_(KSP ksp,PetscScalar *shift, int *__ierr){
*__ierr = KSPPIPEFGMRESSetShift(
	(KSP)PetscToPointer((ksp) ),*shift);
}
#if defined(__cplusplus)
}
#endif
