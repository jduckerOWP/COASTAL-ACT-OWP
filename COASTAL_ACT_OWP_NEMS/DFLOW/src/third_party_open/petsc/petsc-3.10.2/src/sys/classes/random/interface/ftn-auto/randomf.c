#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* random.c */
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

#include "petscsys.h"
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscrandomgetvalue_ PETSCRANDOMGETVALUE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscrandomgetvalue_ petscrandomgetvalue
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscrandomgetvaluereal_ PETSCRANDOMGETVALUEREAL
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscrandomgetvaluereal_ petscrandomgetvaluereal
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscrandomgetinterval_ PETSCRANDOMGETINTERVAL
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscrandomgetinterval_ petscrandomgetinterval
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscrandomsetinterval_ PETSCRANDOMSETINTERVAL
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscrandomsetinterval_ petscrandomsetinterval
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void PETSC_STDCALL  petscrandomgetvalue_(PetscRandom r,PetscScalar *val, int *__ierr){
*__ierr = PetscRandomGetValue(
	(PetscRandom)PetscToPointer((r) ),val);
}
PETSC_EXTERN void PETSC_STDCALL  petscrandomgetvaluereal_(PetscRandom r,PetscReal *val, int *__ierr){
*__ierr = PetscRandomGetValueReal(
	(PetscRandom)PetscToPointer((r) ),val);
}
PETSC_EXTERN void PETSC_STDCALL  petscrandomgetinterval_(PetscRandom r,PetscScalar *low,PetscScalar *high, int *__ierr){
*__ierr = PetscRandomGetInterval(
	(PetscRandom)PetscToPointer((r) ),low,high);
}
PETSC_EXTERN void PETSC_STDCALL  petscrandomsetinterval_(PetscRandom r,PetscScalar *low,PetscScalar *high, int *__ierr){
*__ierr = PetscRandomSetInterval(
	(PetscRandom)PetscToPointer((r) ),*low,*high);
}
#if defined(__cplusplus)
}
#endif
