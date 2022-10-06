#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* dmplexts.c */
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

#include "petscdmplex.h"
#include "petscts.h"
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplextsgetgeometryfvm_ DMPLEXTSGETGEOMETRYFVM
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplextsgetgeometryfvm_ dmplextsgetgeometryfvm
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplextscomputerhsfunctionfvm_ DMPLEXTSCOMPUTERHSFUNCTIONFVM
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplextscomputerhsfunctionfvm_ dmplextscomputerhsfunctionfvm
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplextscomputeboundary_ DMPLEXTSCOMPUTEBOUNDARY
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplextscomputeboundary_ dmplextscomputeboundary
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplextscomputeifunctionfem_ DMPLEXTSCOMPUTEIFUNCTIONFEM
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplextscomputeifunctionfem_ dmplextscomputeifunctionfem
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define dmplextscomputeijacobianfem_ DMPLEXTSCOMPUTEIJACOBIANFEM
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define dmplextscomputeijacobianfem_ dmplextscomputeijacobianfem
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void PETSC_STDCALL  dmplextsgetgeometryfvm_(DM dm,Vec *facegeom,Vec *cellgeom,PetscReal *minRadius, int *__ierr){
*__ierr = DMPlexTSGetGeometryFVM(
	(DM)PetscToPointer((dm) ),facegeom,cellgeom,minRadius);
}
PETSC_EXTERN void PETSC_STDCALL  dmplextscomputerhsfunctionfvm_(DM dm,PetscReal *time,Vec locX,Vec F,void*user, int *__ierr){
*__ierr = DMPlexTSComputeRHSFunctionFVM(
	(DM)PetscToPointer((dm) ),*time,
	(Vec)PetscToPointer((locX) ),
	(Vec)PetscToPointer((F) ),user);
}
PETSC_EXTERN void PETSC_STDCALL  dmplextscomputeboundary_(DM dm,PetscReal *time,Vec locX,Vec locX_t,void*user, int *__ierr){
*__ierr = DMPlexTSComputeBoundary(
	(DM)PetscToPointer((dm) ),*time,
	(Vec)PetscToPointer((locX) ),
	(Vec)PetscToPointer((locX_t) ),user);
}
PETSC_EXTERN void PETSC_STDCALL  dmplextscomputeifunctionfem_(DM dm,PetscReal *time,Vec locX,Vec locX_t,Vec locF,void*user, int *__ierr){
*__ierr = DMPlexTSComputeIFunctionFEM(
	(DM)PetscToPointer((dm) ),*time,
	(Vec)PetscToPointer((locX) ),
	(Vec)PetscToPointer((locX_t) ),
	(Vec)PetscToPointer((locF) ),user);
}
PETSC_EXTERN void PETSC_STDCALL  dmplextscomputeijacobianfem_(DM dm,PetscReal *time,Vec locX,Vec locX_t,PetscReal *X_tShift,Mat Jac,Mat JacP,void*user, int *__ierr){
*__ierr = DMPlexTSComputeIJacobianFEM(
	(DM)PetscToPointer((dm) ),*time,
	(Vec)PetscToPointer((locX) ),
	(Vec)PetscToPointer((locX_t) ),*X_tShift,
	(Mat)PetscToPointer((Jac) ),
	(Mat)PetscToPointer((JacP) ),user);
}
#if defined(__cplusplus)
}
#endif
