
static char help[] = "Tests MatCreateComposite()\n\n";

/*T
   Concepts: Mat^composite matrices
   Processors: n
T*/

/*
  Include "petscmat.h" so that we can use matrices.
  automatically includes:
     petscsys.h       - base PETSc routines   petscvec.h    - vectors
     petscmat.h    - matrices
     petscis.h     - index sets            petscviewer.h - viewers
*/
#include <petscmat.h>

int main(int argc,char **args)
{
  Mat            A[3],B;                       /* matrix */
  PetscViewer    fd;                      /* viewer */
  char           file[PETSC_MAX_PATH_LEN];            /* input file name */
  PetscErrorCode ierr;
  PetscBool      flg;
  Vec            x,y,z,work;
  PetscReal      rnorm;

  ierr = PetscInitialize(&argc,&args,(char*)0,help);if (ierr) return ierr;
  /*
     Determine files from which we read the two linear systems
     (matrix and right-hand-side vector).
  */
  ierr = PetscOptionsGetString(NULL,NULL,"-f",file,PETSC_MAX_PATH_LEN,&flg);CHKERRQ(ierr);
  if (!flg) SETERRQ(PETSC_COMM_WORLD,1,"Must indicate binary file with the -f option");

  /*
     Open binary file.  Note that we use FILE_MODE_READ to indicate
     reading from this file.
  */
  ierr = PetscViewerBinaryOpen(PETSC_COMM_WORLD,file,FILE_MODE_READ,&fd);CHKERRQ(ierr);

  /*
     Load the matrix; then destroy the viewer.
  */
  ierr = MatCreate(PETSC_COMM_WORLD,&A[0]);CHKERRQ(ierr);
  ierr = MatLoad(A[0],fd);CHKERRQ(ierr);
  ierr = PetscViewerDestroy(&fd);CHKERRQ(ierr);

  ierr = MatDuplicate(A[0],MAT_COPY_VALUES,&A[1]);CHKERRQ(ierr);
  ierr = MatDuplicate(A[0],MAT_COPY_VALUES,&A[2]);CHKERRQ(ierr);
  ierr = MatShift(A[1],1.0);CHKERRQ(ierr);
  ierr = MatShift(A[1],2.0);CHKERRQ(ierr);

  ierr = MatCreateVecs(A[0],&x,&y);CHKERRQ(ierr);
  ierr = VecDuplicate(y,&work);CHKERRQ(ierr);
  ierr = VecDuplicate(y,&z);CHKERRQ(ierr);

  ierr = VecSet(x,1.0);CHKERRQ(ierr);
  ierr = MatMult(A[0],x,z);CHKERRQ(ierr);
  ierr = MatMultAdd(A[1],x,z,z);CHKERRQ(ierr);
  ierr = MatMultAdd(A[2],x,z,z);CHKERRQ(ierr);

  ierr = MatCreateComposite(PETSC_COMM_WORLD,3,A,&B);CHKERRQ(ierr);
  ierr = MatMult(B,x,y);CHKERRQ(ierr);
  ierr = MatDestroy(&B);CHKERRQ(ierr);
  ierr = VecAXPY(y,-1.0,z);CHKERRQ(ierr);
  ierr = VecNorm(y,NORM_2,&rnorm);CHKERRQ(ierr);
  if (rnorm > 1.e-10) {
    ierr = PetscPrintf(PETSC_COMM_WORLD,"Error with composite add %g\n",(double)rnorm);CHKERRQ(ierr);
  }

  ierr = MatCreateComposite(PETSC_COMM_WORLD,3,A,&B);CHKERRQ(ierr);
  ierr = MatCompositeMerge(B);CHKERRQ(ierr);
  ierr = MatMult(B,x,y);CHKERRQ(ierr);
  ierr = MatDestroy(&B);CHKERRQ(ierr);
  ierr = VecAXPY(y,-1.0,z);CHKERRQ(ierr);
  ierr = VecNorm(y,NORM_2,&rnorm);CHKERRQ(ierr);
  if (rnorm > 1.e-10) {
    ierr = PetscPrintf(PETSC_COMM_WORLD,"Error with composite add after merge %g\n",(double)rnorm);CHKERRQ(ierr);
  }

  ierr = VecSet(x,1.0);CHKERRQ(ierr);
  ierr = MatMult(A[0],x,z);CHKERRQ(ierr);
  ierr = MatMult(A[1],z,work);CHKERRQ(ierr);
  ierr = MatMult(A[2],work,z);CHKERRQ(ierr);

  ierr = MatCreateComposite(PETSC_COMM_WORLD,3,A,&B);CHKERRQ(ierr);
  ierr = MatCompositeSetType(B,MAT_COMPOSITE_MULTIPLICATIVE);CHKERRQ(ierr);
  ierr = MatMult(B,x,y);CHKERRQ(ierr);
  ierr = MatDestroy(&B);CHKERRQ(ierr);
  ierr = VecAXPY(y,-1.0,z);CHKERRQ(ierr);
  ierr = VecNorm(y,NORM_2,&rnorm);CHKERRQ(ierr);
  if (rnorm > 1.e-10) {
    ierr = PetscPrintf(PETSC_COMM_WORLD,"Error with composite multiplicative %g\n",(double)rnorm);CHKERRQ(ierr);
  }

  ierr = MatCreateComposite(PETSC_COMM_WORLD,3,A,&B);CHKERRQ(ierr);
  ierr = MatCompositeSetType(B,MAT_COMPOSITE_MULTIPLICATIVE);CHKERRQ(ierr);
  ierr = MatCompositeMerge(B);CHKERRQ(ierr);
  ierr = MatMult(B,x,y);CHKERRQ(ierr);
  ierr = MatDestroy(&B);CHKERRQ(ierr);
  ierr = VecAXPY(y,-1.0,z);CHKERRQ(ierr);
  ierr = VecNorm(y,NORM_2,&rnorm);CHKERRQ(ierr);
  if (rnorm > 1.e-10) {
    ierr = PetscPrintf(PETSC_COMM_WORLD,"Error with composite multiplicative after merge %g\n",(double)rnorm);CHKERRQ(ierr);
  }

  /*
     Free work space.  All PETSc objects should be destroyed when they
     are no longer needed.
  */
  ierr = VecDestroy(&x);CHKERRQ(ierr);
  ierr = VecDestroy(&y);CHKERRQ(ierr);
  ierr = VecDestroy(&work);CHKERRQ(ierr);
  ierr = VecDestroy(&z);CHKERRQ(ierr);
  ierr = MatDestroy(&A[0]);CHKERRQ(ierr);
  ierr = MatDestroy(&A[1]);CHKERRQ(ierr);
  ierr = MatDestroy(&A[2]);CHKERRQ(ierr);

  ierr = PetscFinalize();
  return ierr;
}





/*TEST

   test:
      nsize: 2
      requires: datafilespath double !complex !define(PETSC_USE_64BIT_INDICES)
      args: -f ${DATAFILESPATH}/matrices/medium -viewer_binary_skip_info
      TODO: Need to develop comparison test

TEST*/
