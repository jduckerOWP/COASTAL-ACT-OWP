#include "petscsys.h"
#include "petscfix.h"
#include "petsc/private/fortranimpl.h"
/* fe.c */
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

#include "petscfe.h"
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscfesetfromoptions_ PETSCFESETFROMOPTIONS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscfesetfromoptions_ petscfesetfromoptions
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscfedestroy_ PETSCFEDESTROY
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscfedestroy_ petscfedestroy
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscfecreate_ PETSCFECREATE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscfecreate_ petscfecreate
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscfegetspatialdimension_ PETSCFEGETSPATIALDIMENSION
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscfegetspatialdimension_ petscfegetspatialdimension
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscfesetnumcomponents_ PETSCFESETNUMCOMPONENTS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscfesetnumcomponents_ petscfesetnumcomponents
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscfegetnumcomponents_ PETSCFEGETNUMCOMPONENTS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscfegetnumcomponents_ petscfegetnumcomponents
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscfesettilesizes_ PETSCFESETTILESIZES
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscfesettilesizes_ petscfesettilesizes
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscfegettilesizes_ PETSCFEGETTILESIZES
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscfegettilesizes_ petscfegettilesizes
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscfegetbasisspace_ PETSCFEGETBASISSPACE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscfegetbasisspace_ petscfegetbasisspace
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscfesetbasisspace_ PETSCFESETBASISSPACE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscfesetbasisspace_ petscfesetbasisspace
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscfegetdualspace_ PETSCFEGETDUALSPACE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscfegetdualspace_ petscfegetdualspace
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscfesetdualspace_ PETSCFESETDUALSPACE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscfesetdualspace_ petscfesetdualspace
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscfegetquadrature_ PETSCFEGETQUADRATURE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscfegetquadrature_ petscfegetquadrature
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscfesetquadrature_ PETSCFESETQUADRATURE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscfesetquadrature_ petscfesetquadrature
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscfegetfacequadrature_ PETSCFEGETFACEQUADRATURE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscfegetfacequadrature_ petscfegetfacequadrature
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscfesetfacequadrature_ PETSCFESETFACEQUADRATURE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscfesetfacequadrature_ petscfesetfacequadrature
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscfegetdimension_ PETSCFEGETDIMENSION
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscfegetdimension_ petscfegetdimension
#endif
#ifdef PETSC_HAVE_FORTRAN_CAPS
#define petscferefine_ PETSCFEREFINE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE) && !defined(FORTRANDOUBLEUNDERSCORE)
#define petscferefine_ petscferefine
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif
PETSC_EXTERN void PETSC_STDCALL  petscfesetfromoptions_(PetscFE fem, int *__ierr){
*__ierr = PetscFESetFromOptions(
	(PetscFE)PetscToPointer((fem) ));
}
PETSC_EXTERN void PETSC_STDCALL  petscfedestroy_(PetscFE *fem, int *__ierr){
*__ierr = PetscFEDestroy(fem);
}
PETSC_EXTERN void PETSC_STDCALL  petscfecreate_(MPI_Fint * comm,PetscFE *fem, int *__ierr){
*__ierr = PetscFECreate(
	MPI_Comm_f2c(*(comm)),fem);
}
PETSC_EXTERN void PETSC_STDCALL  petscfegetspatialdimension_(PetscFE fem,PetscInt *dim, int *__ierr){
*__ierr = PetscFEGetSpatialDimension(
	(PetscFE)PetscToPointer((fem) ),dim);
}
PETSC_EXTERN void PETSC_STDCALL  petscfesetnumcomponents_(PetscFE fem,PetscInt *comp, int *__ierr){
*__ierr = PetscFESetNumComponents(
	(PetscFE)PetscToPointer((fem) ),*comp);
}
PETSC_EXTERN void PETSC_STDCALL  petscfegetnumcomponents_(PetscFE fem,PetscInt *comp, int *__ierr){
*__ierr = PetscFEGetNumComponents(
	(PetscFE)PetscToPointer((fem) ),comp);
}
PETSC_EXTERN void PETSC_STDCALL  petscfesettilesizes_(PetscFE fem,PetscInt *blockSize,PetscInt *numBlocks,PetscInt *batchSize,PetscInt *numBatches, int *__ierr){
*__ierr = PetscFESetTileSizes(
	(PetscFE)PetscToPointer((fem) ),*blockSize,*numBlocks,*batchSize,*numBatches);
}
PETSC_EXTERN void PETSC_STDCALL  petscfegettilesizes_(PetscFE fem,PetscInt *blockSize,PetscInt *numBlocks,PetscInt *batchSize,PetscInt *numBatches, int *__ierr){
*__ierr = PetscFEGetTileSizes(
	(PetscFE)PetscToPointer((fem) ),blockSize,numBlocks,batchSize,numBatches);
}
PETSC_EXTERN void PETSC_STDCALL  petscfegetbasisspace_(PetscFE fem,PetscSpace *sp, int *__ierr){
*__ierr = PetscFEGetBasisSpace(
	(PetscFE)PetscToPointer((fem) ),sp);
}
PETSC_EXTERN void PETSC_STDCALL  petscfesetbasisspace_(PetscFE fem,PetscSpace sp, int *__ierr){
*__ierr = PetscFESetBasisSpace(
	(PetscFE)PetscToPointer((fem) ),
	(PetscSpace)PetscToPointer((sp) ));
}
PETSC_EXTERN void PETSC_STDCALL  petscfegetdualspace_(PetscFE fem,PetscDualSpace *sp, int *__ierr){
*__ierr = PetscFEGetDualSpace(
	(PetscFE)PetscToPointer((fem) ),sp);
}
PETSC_EXTERN void PETSC_STDCALL  petscfesetdualspace_(PetscFE fem,PetscDualSpace sp, int *__ierr){
*__ierr = PetscFESetDualSpace(
	(PetscFE)PetscToPointer((fem) ),
	(PetscDualSpace)PetscToPointer((sp) ));
}
PETSC_EXTERN void PETSC_STDCALL  petscfegetquadrature_(PetscFE fem,PetscQuadrature *q, int *__ierr){
*__ierr = PetscFEGetQuadrature(
	(PetscFE)PetscToPointer((fem) ),q);
}
PETSC_EXTERN void PETSC_STDCALL  petscfesetquadrature_(PetscFE fem,PetscQuadrature q, int *__ierr){
*__ierr = PetscFESetQuadrature(
	(PetscFE)PetscToPointer((fem) ),
	(PetscQuadrature)PetscToPointer((q) ));
}
PETSC_EXTERN void PETSC_STDCALL  petscfegetfacequadrature_(PetscFE fem,PetscQuadrature *q, int *__ierr){
*__ierr = PetscFEGetFaceQuadrature(
	(PetscFE)PetscToPointer((fem) ),q);
}
PETSC_EXTERN void PETSC_STDCALL  petscfesetfacequadrature_(PetscFE fem,PetscQuadrature q, int *__ierr){
*__ierr = PetscFESetFaceQuadrature(
	(PetscFE)PetscToPointer((fem) ),
	(PetscQuadrature)PetscToPointer((q) ));
}
PETSC_EXTERN void PETSC_STDCALL  petscfegetdimension_(PetscFE fem,PetscInt *dim, int *__ierr){
*__ierr = PetscFEGetDimension(
	(PetscFE)PetscToPointer((fem) ),dim);
}
PETSC_EXTERN void PETSC_STDCALL  petscferefine_(PetscFE fe,PetscFE *feRef, int *__ierr){
*__ierr = PetscFERefine(
	(PetscFE)PetscToPointer((fe) ),feRef);
}
#if defined(__cplusplus)
}
#endif
