
#include <petsc/private/matimpl.h>          /*I "petscmat.h" I*/

typedef struct {
  IS          isrow,iscol;      /* rows and columns in submatrix, only used to check consistency */
  Vec         left,right;       /* optional scaling */
  Vec         olwork,orwork;    /* work vectors outside the scatters, only touched by PreScale and only created if needed*/
  Vec         lwork,rwork;      /* work vectors inside the scatters */
  Vec         dshift;
  VecScatter  lrestrict,rprolong;
  Mat         A;
  PetscScalar vscale, axpy_vscale;
  PetscScalar vshift, axpy_vshift;
} Mat_SubVirtual;

static PetscErrorCode PreScaleLeft(Mat N,Vec x,Vec *xx)
{
  Mat_SubVirtual *Na = (Mat_SubVirtual*)N->data;
  PetscErrorCode ierr;

  PetscFunctionBegin;
  if (!Na->left) {
    *xx = x;
  } else {
    if (!Na->olwork) {
      ierr = VecDuplicate(Na->left,&Na->olwork);CHKERRQ(ierr);
    }
    ierr = VecPointwiseMult(Na->olwork,x,Na->left);CHKERRQ(ierr);
    *xx  = Na->olwork;
  }
  PetscFunctionReturn(0);
}

static PetscErrorCode PreScaleRight(Mat N,Vec x,Vec *xx)
{
  Mat_SubVirtual *Na = (Mat_SubVirtual*)N->data;
  PetscErrorCode ierr;

  PetscFunctionBegin;
  if (!Na->right) {
    *xx = x;
  } else {
    if (!Na->orwork) {
      ierr = VecDuplicate(Na->right,&Na->orwork);CHKERRQ(ierr);
    }
    ierr = VecPointwiseMult(Na->orwork,x,Na->right);CHKERRQ(ierr);
    *xx  = Na->orwork;
  }
  PetscFunctionReturn(0);
}

static PetscErrorCode PostScaleLeft(Mat N,Vec x)
{
  Mat_SubVirtual *Na = (Mat_SubVirtual*)N->data;
  PetscErrorCode ierr;

  PetscFunctionBegin;
  if (Na->left) {
    ierr = VecPointwiseMult(x,x,Na->left);CHKERRQ(ierr);
  }
  PetscFunctionReturn(0);
}

static PetscErrorCode PostScaleRight(Mat N,Vec x)
{
  Mat_SubVirtual *Na = (Mat_SubVirtual*)N->data;
  PetscErrorCode ierr;

  PetscFunctionBegin;
  if (Na->right) {
    ierr = VecPointwiseMult(x,x,Na->right);CHKERRQ(ierr);
  }
  PetscFunctionReturn(0);
}

/*
         Y = vscale*Y + diag(dshift)*X + vshift*X

         On input Y already contains A*x
*/
static PetscErrorCode MatSubmatShiftAndScale(Mat A,Vec X,Vec Y)
{
  Mat_SubVirtual *Na = (Mat_SubVirtual*)A->data;
  PetscErrorCode ierr;

  PetscFunctionBegin;
  if (Na->dshift) {          /* get arrays because there is no VecPointwiseMultAdd() */
    PetscInt          i,m;
    const PetscScalar *x,*d;
    PetscScalar       *y;
    ierr = VecGetLocalSize(X,&m);CHKERRQ(ierr);
    ierr = VecGetArrayRead(Na->dshift,&d);CHKERRQ(ierr);
    ierr = VecGetArrayRead(X,&x);CHKERRQ(ierr);
    ierr = VecGetArray(Y,&y);CHKERRQ(ierr);
    for (i=0; i<m; i++) y[i] = Na->vscale*y[i] + d[i]*x[i];
    ierr = VecRestoreArrayRead(Na->dshift,&d);CHKERRQ(ierr);
    ierr = VecRestoreArrayRead(X,&x);CHKERRQ(ierr);
    ierr = VecRestoreArray(Y,&y);CHKERRQ(ierr);
  } else {
    ierr = VecScale(Y,Na->vscale);CHKERRQ(ierr);
  }
  if (Na->vshift != 0.0) {ierr = VecAXPY(Y,Na->vshift,X);CHKERRQ(ierr);} /* if test is for non-square matrices */
  PetscFunctionReturn(0);
}

static PetscErrorCode MatScale_SubMatrix(Mat N,PetscScalar a)
{
  Mat_SubVirtual *Na = (Mat_SubVirtual*)N->data;
  PetscErrorCode ierr;

  PetscFunctionBegin;
  Na->vscale *= a;
  Na->vshift *= a;
  if (Na->dshift) {
    ierr = VecScale(Na->dshift,a);CHKERRQ(ierr);
  }
  Na->axpy_vscale *= a;
  PetscFunctionReturn(0);
}

static PetscErrorCode MatShift_SubMatrix(Mat N,PetscScalar a)
{
  Mat_SubVirtual *Na = (Mat_SubVirtual*)N->data;
  PetscErrorCode ierr;

  PetscFunctionBegin;
  if (Na->left || Na->right) {
    if (!Na->dshift) {
      ierr = VecDuplicate(Na->left ? Na->left : Na->right, &Na->dshift);CHKERRQ(ierr);
      ierr = VecSet(Na->dshift,a);CHKERRQ(ierr);
    } else {
      if (Na->left)  {ierr = VecPointwiseMult(Na->dshift,Na->dshift,Na->left);CHKERRQ(ierr);}
      if (Na->right) {ierr = VecPointwiseMult(Na->dshift,Na->dshift,Na->right);CHKERRQ(ierr);}
      ierr = VecShift(Na->dshift,a);CHKERRQ(ierr);
    }
    if (Na->left)  {ierr = VecPointwiseDivide(Na->dshift,Na->dshift,Na->left);CHKERRQ(ierr);}
    if (Na->right) {ierr = VecPointwiseDivide(Na->dshift,Na->dshift,Na->right);CHKERRQ(ierr);}
  } else Na->vshift += a;
  PetscFunctionReturn(0);
}

static PetscErrorCode MatDiagonalScale_SubMatrix(Mat N,Vec left,Vec right)
{
  Mat_SubVirtual *Na = (Mat_SubVirtual*)N->data;
  PetscErrorCode ierr;

  PetscFunctionBegin;
  if (left) {
    if (!Na->left) {
      ierr = VecDuplicate(left,&Na->left);CHKERRQ(ierr);
      ierr = VecCopy(left,Na->left);CHKERRQ(ierr);
    } else {
      ierr = VecPointwiseMult(Na->left,left,Na->left);CHKERRQ(ierr);
    }
  }
  if (right) {
    if (!Na->right) {
      ierr = VecDuplicate(right,&Na->right);CHKERRQ(ierr);
      ierr = VecCopy(right,Na->right);CHKERRQ(ierr);
    } else {
      ierr = VecPointwiseMult(Na->right,right,Na->right);CHKERRQ(ierr);
    }
  }
  PetscFunctionReturn(0);
}

static PetscErrorCode MatMult_SubMatrix(Mat N,Vec x,Vec y)
{
  Mat_SubVirtual *Na = (Mat_SubVirtual*)N->data;
  Vec            xx  = 0;
  PetscErrorCode ierr;

  PetscFunctionBegin;
  ierr = PreScaleRight(N,x,&xx);CHKERRQ(ierr);
  ierr = VecZeroEntries(Na->rwork);CHKERRQ(ierr);
  ierr = VecScatterBegin(Na->rprolong,xx,Na->rwork,INSERT_VALUES,SCATTER_FORWARD);CHKERRQ(ierr);
  ierr = VecScatterEnd  (Na->rprolong,xx,Na->rwork,INSERT_VALUES,SCATTER_FORWARD);CHKERRQ(ierr);
  ierr = MatMult(Na->A,Na->rwork,Na->lwork);CHKERRQ(ierr);
  ierr = VecScatterBegin(Na->lrestrict,Na->lwork,y,INSERT_VALUES,SCATTER_FORWARD);CHKERRQ(ierr);
  ierr = VecScatterEnd  (Na->lrestrict,Na->lwork,y,INSERT_VALUES,SCATTER_FORWARD);CHKERRQ(ierr);
  ierr = MatSubmatShiftAndScale(N,xx,y);CHKERRQ(ierr);
  ierr = PostScaleLeft(N,y);CHKERRQ(ierr);
  PetscFunctionReturn(0);
}

static PetscErrorCode MatMultAdd_SubMatrix(Mat N,Vec v1,Vec v2,Vec v3)
{
  Mat_SubVirtual *Na = (Mat_SubVirtual*)N->data;
  Vec            xx  = 0;
  PetscErrorCode ierr;

  PetscFunctionBegin;
  ierr = PreScaleRight(N,v1,&xx);CHKERRQ(ierr);
  ierr = VecZeroEntries(Na->rwork);CHKERRQ(ierr);
  ierr = VecScatterBegin(Na->rprolong,xx,Na->rwork,INSERT_VALUES,SCATTER_FORWARD);CHKERRQ(ierr);
  ierr = VecScatterEnd  (Na->rprolong,xx,Na->rwork,INSERT_VALUES,SCATTER_FORWARD);CHKERRQ(ierr);
  ierr = MatMult(Na->A,Na->rwork,Na->lwork);CHKERRQ(ierr);
  if (v2 == v3) {
    if (!Na->olwork) {ierr = VecDuplicate(v3,&Na->olwork);CHKERRQ(ierr);}
    ierr = VecScatterBegin(Na->lrestrict,Na->lwork,Na->olwork,INSERT_VALUES,SCATTER_FORWARD);CHKERRQ(ierr);
    ierr = VecScatterEnd  (Na->lrestrict,Na->lwork,Na->olwork,INSERT_VALUES,SCATTER_FORWARD);CHKERRQ(ierr);
    ierr = MatSubmatShiftAndScale(N,xx,Na->olwork);CHKERRQ(ierr);
    ierr = PostScaleLeft(N,Na->olwork);CHKERRQ(ierr);
    ierr = VecAXPY(v3,1.0,Na->olwork);CHKERRQ(ierr);
  } else {
    ierr = VecScatterBegin(Na->lrestrict,Na->lwork,v3,INSERT_VALUES,SCATTER_FORWARD);CHKERRQ(ierr);
    ierr = VecScatterEnd  (Na->lrestrict,Na->lwork,v3,INSERT_VALUES,SCATTER_FORWARD);CHKERRQ(ierr);
    ierr = MatSubmatShiftAndScale(N,xx,v3);CHKERRQ(ierr);
    ierr = PostScaleLeft(N,v3);CHKERRQ(ierr);
    ierr = VecAXPY(v3,1.0,v2);CHKERRQ(ierr);
  }
  PetscFunctionReturn(0);
}

static PetscErrorCode MatMultTranspose_SubMatrix(Mat N,Vec x,Vec y)
{
  Mat_SubVirtual *Na = (Mat_SubVirtual*)N->data;
  Vec            xx  = 0;
  PetscErrorCode ierr;

  PetscFunctionBegin;
  ierr = PreScaleLeft(N,x,&xx);CHKERRQ(ierr);
  ierr = VecZeroEntries(Na->lwork);CHKERRQ(ierr);
  ierr = VecScatterBegin(Na->lrestrict,xx,Na->lwork,INSERT_VALUES,SCATTER_REVERSE);CHKERRQ(ierr);
  ierr = VecScatterEnd  (Na->lrestrict,xx,Na->lwork,INSERT_VALUES,SCATTER_REVERSE);CHKERRQ(ierr);
  ierr = MatMultTranspose(Na->A,Na->lwork,Na->rwork);CHKERRQ(ierr);
  ierr = VecScatterBegin(Na->rprolong,Na->rwork,y,INSERT_VALUES,SCATTER_REVERSE);CHKERRQ(ierr);
  ierr = VecScatterEnd  (Na->rprolong,Na->rwork,y,INSERT_VALUES,SCATTER_REVERSE);CHKERRQ(ierr);
  ierr = MatSubmatShiftAndScale(N,xx,y);CHKERRQ(ierr);
  ierr = PostScaleRight(N,y);CHKERRQ(ierr);
  PetscFunctionReturn(0);
}

static PetscErrorCode MatMultTransposeAdd_SubMatrix(Mat N,Vec v1,Vec v2,Vec v3)
{
  Mat_SubVirtual *Na = (Mat_SubVirtual*)N->data;
  Vec            xx  = 0;
  PetscErrorCode ierr;

  PetscFunctionBegin;
  ierr = PreScaleLeft(N,v1,&xx);CHKERRQ(ierr);
  ierr = VecZeroEntries(Na->lwork);CHKERRQ(ierr);
  ierr = VecScatterBegin(Na->lrestrict,xx,Na->lwork,INSERT_VALUES,SCATTER_REVERSE);CHKERRQ(ierr);
  ierr = VecScatterEnd  (Na->lrestrict,xx,Na->lwork,INSERT_VALUES,SCATTER_REVERSE);CHKERRQ(ierr);
  ierr = MatMultTranspose(Na->A,Na->lwork,Na->rwork);CHKERRQ(ierr);
  if (v2 == v3) {
    if (!Na->orwork) {ierr = VecDuplicate(v3,&Na->orwork);CHKERRQ(ierr);}
    ierr = VecScatterBegin(Na->rprolong,Na->rwork,Na->orwork,INSERT_VALUES,SCATTER_REVERSE);CHKERRQ(ierr);
    ierr = VecScatterEnd  (Na->rprolong,Na->rwork,Na->orwork,INSERT_VALUES,SCATTER_REVERSE);CHKERRQ(ierr);
    ierr = MatSubmatShiftAndScale(N,xx,Na->orwork);CHKERRQ(ierr);
    ierr = PostScaleRight(N,Na->orwork);CHKERRQ(ierr);
    ierr = VecAXPY(v3,1.0,Na->orwork);CHKERRQ(ierr);
  } else {
    ierr = VecScatterBegin(Na->rprolong,Na->rwork,v3,INSERT_VALUES,SCATTER_REVERSE);CHKERRQ(ierr);
    ierr = VecScatterEnd  (Na->rprolong,Na->rwork,v3,INSERT_VALUES,SCATTER_REVERSE);CHKERRQ(ierr);
    ierr = MatSubmatShiftAndScale(N,xx,v3);CHKERRQ(ierr);
    ierr = PostScaleRight(N,v3);CHKERRQ(ierr);
    ierr = VecAXPY(v3,1.0,v2);CHKERRQ(ierr);
  }
  PetscFunctionReturn(0);
}

static PetscErrorCode MatDestroy_SubMatrix(Mat N)
{
  Mat_SubVirtual *Na = (Mat_SubVirtual*)N->data;
  PetscErrorCode ierr;

  PetscFunctionBegin;
  ierr = ISDestroy(&Na->isrow);CHKERRQ(ierr);
  ierr = ISDestroy(&Na->iscol);CHKERRQ(ierr);
  ierr = VecDestroy(&Na->left);CHKERRQ(ierr);
  ierr = VecDestroy(&Na->right);CHKERRQ(ierr);
  ierr = VecDestroy(&Na->olwork);CHKERRQ(ierr);
  ierr = VecDestroy(&Na->orwork);CHKERRQ(ierr);
  ierr = VecDestroy(&Na->lwork);CHKERRQ(ierr);
  ierr = VecDestroy(&Na->rwork);CHKERRQ(ierr);
  ierr = VecDestroy(&Na->dshift);CHKERRQ(ierr);
  ierr = VecScatterDestroy(&Na->lrestrict);CHKERRQ(ierr);
  ierr = VecScatterDestroy(&Na->rprolong);CHKERRQ(ierr);
  ierr = MatDestroy(&Na->A);CHKERRQ(ierr);
  ierr = PetscFree(N->data);CHKERRQ(ierr);
  PetscFunctionReturn(0);
}

/*@
   MatCreateSubMatrixVirtual - Creates a virtual matrix that acts as a submatrix

   Collective on Mat

   Input Parameters:
+  A - matrix that we will extract a submatrix of
.  isrow - rows to be present in the submatrix
-  iscol - columns to be present in the submatrix

   Output Parameters:
.  newmat - new matrix

   Level: developer

   Notes:
   Most will use MatCreateSubMatrix which provides a more efficient representation if it is available.

.seealso: MatCreateSubMatrix(), MatSubMatrixVirtualUpdate()
@*/
PetscErrorCode MatCreateSubMatrixVirtual(Mat A,IS isrow,IS iscol,Mat *newmat)
{
  Vec            left,right;
  PetscInt       m,n;
  Mat            N;
  Mat_SubVirtual *Na;
  PetscErrorCode ierr;

  PetscFunctionBegin;
  PetscValidHeaderSpecific(A,MAT_CLASSID,1);
  PetscValidHeaderSpecific(isrow,IS_CLASSID,2);
  PetscValidHeaderSpecific(iscol,IS_CLASSID,3);
  PetscValidPointer(newmat,4);
  *newmat = 0;

  ierr = MatCreate(PetscObjectComm((PetscObject)A),&N);CHKERRQ(ierr);
  ierr = ISGetLocalSize(isrow,&m);CHKERRQ(ierr);
  ierr = ISGetLocalSize(iscol,&n);CHKERRQ(ierr);
  ierr = MatSetSizes(N,m,n,PETSC_DETERMINE,PETSC_DETERMINE);CHKERRQ(ierr);
  ierr = PetscObjectChangeTypeName((PetscObject)N,MATSUBMATRIX);CHKERRQ(ierr);

  ierr      = PetscNewLog(N,&Na);CHKERRQ(ierr);
  N->data   = (void*)Na;
  ierr      = PetscObjectReference((PetscObject)A);CHKERRQ(ierr);
  ierr      = PetscObjectReference((PetscObject)isrow);CHKERRQ(ierr);
  ierr      = PetscObjectReference((PetscObject)iscol);CHKERRQ(ierr);
  Na->A     = A;
  Na->isrow = isrow;
  Na->iscol = iscol;
  Na->vscale = 1.0;
  Na->vshift = 0.0;

  N->ops->destroy          = MatDestroy_SubMatrix;
  N->ops->mult             = MatMult_SubMatrix;
  N->ops->multadd          = MatMultAdd_SubMatrix;
  N->ops->multtranspose    = MatMultTranspose_SubMatrix;
  N->ops->multtransposeadd = MatMultTransposeAdd_SubMatrix;
  N->ops->scale            = MatScale_SubMatrix;
  N->ops->diagonalscale    = MatDiagonalScale_SubMatrix;
  N->ops->shift            = MatShift_SubMatrix;

  ierr = MatSetBlockSizesFromMats(N,A,A);CHKERRQ(ierr);
  ierr = PetscLayoutSetUp(N->rmap);CHKERRQ(ierr);
  ierr = PetscLayoutSetUp(N->cmap);CHKERRQ(ierr);

  ierr = MatCreateVecs(A,&Na->rwork,&Na->lwork);CHKERRQ(ierr);
  ierr = VecCreate(PetscObjectComm((PetscObject)isrow),&left);CHKERRQ(ierr);
  ierr = VecCreate(PetscObjectComm((PetscObject)iscol),&right);CHKERRQ(ierr);
  ierr = VecSetSizes(left,m,PETSC_DETERMINE);CHKERRQ(ierr);
  ierr = VecSetSizes(right,n,PETSC_DETERMINE);CHKERRQ(ierr);
  ierr = VecSetUp(left);CHKERRQ(ierr);
  ierr = VecSetUp(right);CHKERRQ(ierr);
  ierr = VecScatterCreate(Na->lwork,isrow,left,NULL,&Na->lrestrict);CHKERRQ(ierr);
  ierr = VecScatterCreate(right,NULL,Na->rwork,iscol,&Na->rprolong);CHKERRQ(ierr);
  ierr = VecDestroy(&left);CHKERRQ(ierr);
  ierr = VecDestroy(&right);CHKERRQ(ierr);

  N->assembled = PETSC_TRUE;

  ierr = MatSetUp(N);CHKERRQ(ierr);

  *newmat      = N;
  PetscFunctionReturn(0);
}


/*@
   MatSubMatrixVirtualUpdate - Updates a submatrix

   Collective on Mat

   Input Parameters:
+  N - submatrix to update
.  A - full matrix in the submatrix
.  isrow - rows in the update (same as the first time the submatrix was created)
-  iscol - columns in the update (same as the first time the submatrix was created)

   Level: developer

   Notes:
   Most will use MatCreateSubMatrix which provides a more efficient representation if it is available.

.seealso: MatCreateSubMatrixVirtual()
@*/
PetscErrorCode  MatSubMatrixVirtualUpdate(Mat N,Mat A,IS isrow,IS iscol)
{
  PetscErrorCode ierr;
  PetscBool      flg;
  Mat_SubVirtual *Na;

  PetscFunctionBegin;
  PetscValidHeaderSpecific(N,MAT_CLASSID,1);
  PetscValidHeaderSpecific(A,MAT_CLASSID,2);
  PetscValidHeaderSpecific(isrow,IS_CLASSID,3);
  PetscValidHeaderSpecific(iscol,IS_CLASSID,4);
  ierr = PetscObjectTypeCompare((PetscObject)N,MATSUBMATRIX,&flg);CHKERRQ(ierr);
  if (!flg) SETERRQ(PetscObjectComm((PetscObject)A),PETSC_ERR_ARG_WRONG,"Matrix has wrong type");

  Na   = (Mat_SubVirtual*)N->data;
  ierr = ISEqual(isrow,Na->isrow,&flg);CHKERRQ(ierr);
  if (!flg) SETERRQ(PETSC_COMM_SELF,PETSC_ERR_ARG_INCOMP,"Cannot update submatrix with different row indices");
  ierr = ISEqual(iscol,Na->iscol,&flg);CHKERRQ(ierr);
  if (!flg) SETERRQ(PETSC_COMM_SELF,PETSC_ERR_ARG_INCOMP,"Cannot update submatrix with different column indices");

  ierr  = PetscObjectReference((PetscObject)A);CHKERRQ(ierr);
  ierr  = MatDestroy(&Na->A);CHKERRQ(ierr);
  Na->A = A;

  Na->vshift = 0.0;
  Na->vscale = 1.0;
  ierr       = VecDestroy(&Na->left);CHKERRQ(ierr);
  ierr       = VecDestroy(&Na->right);CHKERRQ(ierr);
  PetscFunctionReturn(0);
}
