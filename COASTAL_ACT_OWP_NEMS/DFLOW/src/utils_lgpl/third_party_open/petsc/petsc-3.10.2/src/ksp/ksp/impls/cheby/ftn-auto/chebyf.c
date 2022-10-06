#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* cheby.c */
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
#define kspchebyshevseteigenvalues_ KSPCHEBYSHEVSETEIGENVALUES
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define kspchebyshevseteigenvalues_ kspchebyshevseteigenvalues
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define kspchebyshevesteigset_ KSPCHEBYSHEVESTEIGSET
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define kspchebyshevesteigset_ kspchebyshevesteigset
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define kspchebyshevesteigsetusenoisy_ KSPCHEBYSHEVESTEIGSETUSENOISY
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define kspchebyshevesteigsetusenoisy_ kspchebyshevesteigsetusenoisy
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define kspchebyshevesteiggetksp_ KSPCHEBYSHEVESTEIGGETKSP
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define kspchebyshevesteiggetksp_ kspchebyshevesteiggetksp
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void PETSC_STDCALL  kspchebyshevseteigenvalues_(KSP ksp,PetscReal *emax,PetscReal *emin, int *__ierr){
*__ierr = KSPChebyshevSetEigenvalues(
	(KSP)PetscToPointer((ksp) ),*emax,*emin);
}
PETSC_EXTERN void PETSC_STDCALL  kspchebyshevesteigset_(KSP ksp,PetscReal *a,PetscReal *b,PetscReal *c,PetscReal *d, int *__ierr){
*__ierr = KSPChebyshevEstEigSet(
	(KSP)PetscToPointer((ksp) ),*a,*b,*c,*d);
}
PETSC_EXTERN void PETSC_STDCALL  kspchebyshevesteigsetusenoisy_(KSP ksp,PetscBool *use, int *__ierr){
*__ierr = KSPChebyshevEstEigSetUseNoisy(
	(KSP)PetscToPointer((ksp) ),*use);
}
PETSC_EXTERN void PETSC_STDCALL  kspchebyshevesteiggetksp_(KSP ksp,KSP *kspest, int *__ierr){
*__ierr = KSPChebyshevEstEigGetKSP(
	(KSP)PetscToPointer((ksp) ),kspest);
}
#if defined(__cplusplus)
}
#endif
