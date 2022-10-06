#include <petsc/private/fortranimpl.h>
#include <petscdmda.h>

#if defined(PETSC_HAVE_FORTRAN_CAPS)
#define dmdagetprocessorsubset_ DMDAGETPROCESSORSUBSET
#define dmdagetprocessorsubsets_ DMDAGETPROCESSORSUBSETS
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE)
#define dmdagetprocessorsubset_ dmdagetprocessorsubset
#define dmdagetprocessorsubsets_ dmdagetprocessorsubsets
#endif

PETSC_EXTERN void PETSC_STDCALL  dmdagetprocessorsubset_(DM *da,DMDADirection *dir,PetscInt *gp,MPI_Fint *fcomm, int *__ierr )
{
  MPI_Comm comm;
  *__ierr = DMDAGetProcessorSubset(*da,*dir,*gp,&comm);
  *fcomm = MPI_Comm_c2f(comm);
}
PETSC_EXTERN void PETSC_STDCALL  dmdagetprocessorsubsets_(DM *da,DMDADirection *dir,MPI_Fint *subfcomm, int *__ierr )
{
  MPI_Comm subcomm;
  *__ierr = DMDAGetProcessorSubsets(*da,*dir,&subcomm);
  *subfcomm = MPI_Comm_c2f(subcomm);
}

