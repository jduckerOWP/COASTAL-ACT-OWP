#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* ispai.c */
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

#include "petscpc.h"
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcspaisetepsilon_ PCSPAISETEPSILON
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcspaisetepsilon_ pcspaisetepsilon
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcspaisetnbsteps_ PCSPAISETNBSTEPS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcspaisetnbsteps_ pcspaisetnbsteps
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcspaisetmax_ PCSPAISETMAX
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcspaisetmax_ pcspaisetmax
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcspaisetmaxnew_ PCSPAISETMAXNEW
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcspaisetmaxnew_ pcspaisetmaxnew
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcspaisetblocksize_ PCSPAISETBLOCKSIZE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcspaisetblocksize_ pcspaisetblocksize
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcspaisetcachesize_ PCSPAISETCACHESIZE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcspaisetcachesize_ pcspaisetcachesize
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcspaisetverbose_ PCSPAISETVERBOSE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcspaisetverbose_ pcspaisetverbose
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define pcspaisetsp_ PCSPAISETSP
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define pcspaisetsp_ pcspaisetsp
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void PETSC_STDCALL  pcspaisetepsilon_(PC pc,double *epsilon1, int *__ierr){
*__ierr = PCSPAISetEpsilon(
	(PC)PetscToPointer((pc) ),*epsilon1);
}
PETSC_EXTERN void PETSC_STDCALL  pcspaisetnbsteps_(PC pc,int *nbsteps1, int *__ierr){
*__ierr = PCSPAISetNBSteps(
	(PC)PetscToPointer((pc) ),*nbsteps1);
}
PETSC_EXTERN void PETSC_STDCALL  pcspaisetmax_(PC pc,int *max1, int *__ierr){
*__ierr = PCSPAISetMax(
	(PC)PetscToPointer((pc) ),*max1);
}
PETSC_EXTERN void PETSC_STDCALL  pcspaisetmaxnew_(PC pc,int *maxnew1, int *__ierr){
*__ierr = PCSPAISetMaxNew(
	(PC)PetscToPointer((pc) ),*maxnew1);
}
PETSC_EXTERN void PETSC_STDCALL  pcspaisetblocksize_(PC pc,int *block_size1, int *__ierr){
*__ierr = PCSPAISetBlockSize(
	(PC)PetscToPointer((pc) ),*block_size1);
}
PETSC_EXTERN void PETSC_STDCALL  pcspaisetcachesize_(PC pc,int *cache_size, int *__ierr){
*__ierr = PCSPAISetCacheSize(
	(PC)PetscToPointer((pc) ),*cache_size);
}
PETSC_EXTERN void PETSC_STDCALL  pcspaisetverbose_(PC pc,int *verbose, int *__ierr){
*__ierr = PCSPAISetVerbose(
	(PC)PetscToPointer((pc) ),*verbose);
}
PETSC_EXTERN void PETSC_STDCALL  pcspaisetsp_(PC pc,int *sp, int *__ierr){
*__ierr = PCSPAISetSp(
	(PC)PetscToPointer((pc) ),*sp);
}
#if defined(__cplusplus)
}
#endif
