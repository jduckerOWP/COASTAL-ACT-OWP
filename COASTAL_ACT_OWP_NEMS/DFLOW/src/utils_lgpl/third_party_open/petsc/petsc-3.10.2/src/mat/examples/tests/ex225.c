
static char help[] = "Test Hypre matrix APIs\n";

#include <petscmathypre.h>

int main(int argc,char **args)
{
  Mat                A, B, C;
  PetscReal          err;
  PetscInt           i,j,M = 20;
  PetscErrorCode     ierr;
  PetscMPIInt        NP;
  MPI_Comm           comm;
  PetscInt           *rows;

  ierr = PetscInitialize(&argc,&args,(char*)0,help);if (ierr) return ierr;
  comm = PETSC_COMM_WORLD;
  ierr = MPI_Comm_size(comm,&NP);CHKERRQ(ierr);
  ierr = PetscOptionsGetInt(NULL,NULL,"-M",&M,NULL);CHKERRQ(ierr);
  if (M < 6) SETERRQ(PETSC_COMM_WORLD,PETSC_ERR_SUP,"Matrix has to have more than 6 columns");
  /* Hypre matrix */
  ierr = MatCreate(comm,&B);CHKERRQ(ierr);
  ierr = MatSetSizes(B,PETSC_DECIDE,PETSC_DECIDE,M,M);CHKERRQ(ierr);
  ierr = MatSetType(B,MATHYPRE);CHKERRQ(ierr);
  ierr = MatHYPRESetPreallocation(B,9,NULL,9,NULL);CHKERRQ(ierr);

  /* PETSc AIJ matrix */
  ierr = MatCreate(comm,&A);CHKERRQ(ierr);
  ierr = MatSetSizes(A,PETSC_DECIDE,PETSC_DECIDE,M,M);CHKERRQ(ierr);
  ierr = MatSetType(A,MATAIJ);CHKERRQ(ierr);
  ierr = MatSeqAIJSetPreallocation(A,9,NULL);CHKERRQ(ierr);
  ierr = MatMPIAIJSetPreallocation(A,9,NULL,9,NULL);CHKERRQ(ierr);

  /*Set Values */
  for (i=0; i<M; i++) {
    PetscInt    cols[] = {0,1,2,3,4,5};
    PetscScalar vals[6] = {0};
    for (j=0; j<6; j++)
      vals[j] = ((PetscReal)j)/NP;
    PetscScalar value[] = {100};

    ierr = MatSetValues(B,1,&i,6,cols,vals,ADD_VALUES);CHKERRQ(ierr);
    ierr = MatSetValues(B,1,&i,1,&i,value,ADD_VALUES);CHKERRQ(ierr);
    ierr = MatSetValues(A,1,&i,6,cols,vals,ADD_VALUES);CHKERRQ(ierr);
    ierr = MatSetValues(A,1,&i,1,&i,value,ADD_VALUES);CHKERRQ(ierr);
  }

  /* MAT_FLUSH_ASSEMBLY currently not supported */
  ierr = MatAssemblyBegin(A,MAT_FINAL_ASSEMBLY);CHKERRQ(ierr);
  ierr = MatAssemblyEnd(A,MAT_FINAL_ASSEMBLY);CHKERRQ(ierr);
  ierr = MatAssemblyBegin(B,MAT_FINAL_ASSEMBLY);CHKERRQ(ierr);
  ierr = MatAssemblyEnd(B,MAT_FINAL_ASSEMBLY);CHKERRQ(ierr);

  /* Compare A and B */
  ierr = MatConvert(B,MATAIJ,MAT_INITIAL_MATRIX,&C);CHKERRQ(ierr);
  ierr = MatAXPY(C,-1.,A,SAME_NONZERO_PATTERN);CHKERRQ(ierr);
  ierr = MatNorm(C,NORM_INFINITY,&err);CHKERRQ(ierr);
  if (err > PETSC_SMALL) SETERRQ1(PetscObjectComm((PetscObject)B),PETSC_ERR_PLIB,"Error MatSetValues %g",err);
  ierr = MatDestroy(&C);CHKERRQ(ierr);

  /* MatZeroRows */
  ierr = PetscCalloc1(M, &rows);CHKERRQ(ierr);
  for (i=0; i<M; i++) rows[i] = i;
  ierr = MatZeroRows(B, M, rows, 10.0,NULL, NULL);CHKERRQ(ierr);
  ierr = MatSetOption(A, MAT_KEEP_NONZERO_PATTERN, PETSC_TRUE);CHKERRQ(ierr);
  ierr = MatZeroRows(A, M, rows, 10.0,NULL, NULL);CHKERRQ(ierr);
  ierr = MatConvert(B,MATAIJ,MAT_INITIAL_MATRIX,&C);CHKERRQ(ierr);
  ierr = MatAXPY(C,-1.,A,SAME_NONZERO_PATTERN);CHKERRQ(ierr);
  ierr = MatNorm(C,NORM_INFINITY,&err);CHKERRQ(ierr);
  if (err > PETSC_SMALL) SETERRQ1(PetscObjectComm((PetscObject)B),PETSC_ERR_PLIB,"Error MatZeroRows %g",err);
  ierr = MatDestroy(&C);CHKERRQ(ierr);
  ierr = PetscFree(rows);CHKERRQ(ierr);

  /* Test MatZeroEntries */
  ierr = MatZeroEntries(B);
  ierr = MatConvert(B,MATAIJ,MAT_INITIAL_MATRIX,&C);CHKERRQ(ierr);
  ierr = MatNorm(C,NORM_INFINITY,&err);CHKERRQ(ierr);
  if (err > PETSC_SMALL) SETERRQ1(PetscObjectComm((PetscObject)A),PETSC_ERR_PLIB,"Error MatZeroEntries %g",err);
  ierr = MatDestroy(&C);CHKERRQ(ierr);

  /* Insert Values */
  for (i=0; i<M; i++) {
    PetscInt    cols[] = {0,1,2,3,4,5};
    PetscScalar vals[6] = {0};
    for (j=0; j<6; j++)
      vals[j] = ((PetscReal)j)/NP;

    PetscScalar value[] = {100};

    ierr = MatSetValues(B,1,&i,6,cols,vals,INSERT_VALUES);CHKERRQ(ierr);
    ierr = MatSetValues(B,1,&i,1,&i,value,INSERT_VALUES);CHKERRQ(ierr);
    ierr = MatSetValues(A,1,&i,6,cols,vals,INSERT_VALUES);CHKERRQ(ierr);
    ierr = MatSetValues(A,1,&i,1,&i,value,INSERT_VALUES);CHKERRQ(ierr);
  }

  /* MAT_FLUSH_ASSEMBLY currently not supported */
  ierr = MatAssemblyBegin(A,MAT_FINAL_ASSEMBLY);CHKERRQ(ierr);
  ierr = MatAssemblyEnd(A,MAT_FINAL_ASSEMBLY);CHKERRQ(ierr);
  ierr = MatAssemblyBegin(B,MAT_FINAL_ASSEMBLY);CHKERRQ(ierr);
  ierr = MatAssemblyEnd(B,MAT_FINAL_ASSEMBLY);CHKERRQ(ierr);

  {
    const PetscInt *idxA,*idxB;
    const PetscScalar *vA, *vB;
    PetscInt rstart, rend, nzA, nzB, j;
    err = 0.0;
    ierr = MatGetOwnershipRange(A,&rstart,&rend);CHKERRQ(ierr);
    for (i=rstart; i<rend; i++) {
      ierr = MatGetRow(A,i,&nzA, &idxA,&vA);CHKERRQ(ierr);
      ierr = MatGetRow(B,i,&nzB, &idxB,&vB);CHKERRQ(ierr);
      if (nzA!=nzB) SETERRQ1(PETSC_COMM_SELF,PETSC_ERR_PLIB,"Error MatGetRow %d", nzA-nzB);
      for (j=0;j<nzA;j++) {
        err +=idxA[j]-idxB[j];
        err +=vA[j]-vA[j];
      }
      ierr = MatRestoreRow(A,i,&nzA, &idxA,&vA);CHKERRQ(ierr);
      ierr = MatRestoreRow(B,i,&nzB, &idxB,&vB);CHKERRQ(ierr);
    }
    if (err > PETSC_SMALL) SETERRQ1(PetscObjectComm((PetscObject)B),PETSC_ERR_PLIB,"Error MatGetRow %g",err);

    PetscInt    cols[] = {0,1,2,3,4,-5};
    PetscInt    *rows;
    PetscScalar *valuesA, *valuesB;
    ierr = MatGetOwnershipRange(A,&rstart,&rend);CHKERRQ(ierr);
    ierr = PetscCalloc3((rend-rstart)*6,&valuesA,(rend-rstart)*6,&valuesB,rend-rstart,&rows);CHKERRQ(ierr);
    for (i=rstart; i<rend; i++)
      rows[i-rstart] =i;

    ierr = MatGetValues(A,rend-rstart,rows,6,cols,valuesA);CHKERRQ(ierr);
    ierr = MatGetValues(B,rend-rstart,rows,6,cols,valuesB);CHKERRQ(ierr);

    err = 0;
    for (i=0; i<(rend-rstart); i++)
      for (j=0; j<6; j++)
        err += valuesA[i*6+j] - valuesB[i*6+j];

    ierr = PetscFree3(valuesA,valuesB,rows);CHKERRQ(ierr);

    if (err > PETSC_SMALL) SETERRQ1(PetscObjectComm((PetscObject)B),PETSC_ERR_PLIB,"Error MatGetValueS %g",err);
  }

  /* Compare A and B */
  ierr = MatConvert(B,MATAIJ,MAT_INITIAL_MATRIX,&C);CHKERRQ(ierr);
  ierr = MatAXPY(C,-1.,A,SAME_NONZERO_PATTERN);CHKERRQ(ierr);
  ierr = MatNorm(C,NORM_INFINITY,&err);CHKERRQ(ierr);
  if (err > PETSC_SMALL) SETERRQ1(PetscObjectComm((PetscObject)B),PETSC_ERR_PLIB,"Error MatSetValues with INSERT_VALUES %g",err);

  ierr = MatDestroy(&A);CHKERRQ(ierr);
  ierr = MatDestroy(&B);CHKERRQ(ierr);
  ierr = MatDestroy(&C);CHKERRQ(ierr);

  ierr = PetscFinalize();
  return ierr;
}


/*TEST

   build:
      requires: hypre

   test:
      suffix: 1
      requires: hypre

TEST*/
