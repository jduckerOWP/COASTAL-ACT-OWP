
/*
  Defines matrix-matrix product routines for pairs of MPIAIJ matrices
          C = A * B
*/
#include <../src/mat/impls/aij/seq/aij.h> /*I "petscmat.h" I*/
#include <../src/mat/utils/freespace.h>
#include <../src/mat/impls/aij/mpi/mpiaij.h>
#include <petscbt.h>
#include <../src/mat/impls/dense/mpi/mpidense.h>
#include <petsc/private/vecimpl.h>

#if defined(PETSC_HAVE_HYPRE)
PETSC_INTERN PetscErrorCode MatMatMultSymbolic_AIJ_AIJ_wHYPRE(Mat,Mat,PetscReal,Mat*);
#endif

PETSC_INTERN PetscErrorCode MatMatMult_MPIAIJ_MPIAIJ(Mat A,Mat B,MatReuse scall,PetscReal fill, Mat *C)
{
  PetscErrorCode ierr;
#if defined(PETSC_HAVE_HYPRE)
  const char     *algTypes[4] = {"scalable","nonscalable","seqmpi","hypre"};
  PetscInt       nalg = 4;
#else
  const char     *algTypes[3] = {"scalable","nonscalable","seqmpi"};
  PetscInt       nalg = 3;
#endif
  PetscInt       alg = 1; /* set nonscalable algorithm as default */
  MPI_Comm       comm;
  PetscBool      flg;

  PetscFunctionBegin;
  if (scall == MAT_INITIAL_MATRIX) {
    ierr = PetscObjectGetComm((PetscObject)A,&comm);CHKERRQ(ierr);
    if (A->cmap->rstart != B->rmap->rstart || A->cmap->rend != B->rmap->rend) SETERRQ4(comm,PETSC_ERR_ARG_SIZ,"Matrix local dimensions are incompatible, (%D, %D) != (%D,%D)",A->cmap->rstart,A->cmap->rend,B->rmap->rstart,B->rmap->rend);

    ierr = PetscObjectOptionsBegin((PetscObject)A);CHKERRQ(ierr);
    PetscOptionsObject->alreadyprinted = PETSC_FALSE; /* a hack to ensure the option shows in '-help' */
    ierr = PetscOptionsEList("-matmatmult_via","Algorithmic approach","MatMatMult",algTypes,nalg,algTypes[1],&alg,&flg);CHKERRQ(ierr);
    ierr = PetscOptionsEnd();CHKERRQ(ierr);

    if (!flg && B->cmap->N > 100000) { /* may switch to scalable algorithm as default */
      MatInfo     Ainfo,Binfo;
      PetscInt    nz_local;
      PetscBool   alg_scalable_loc=PETSC_FALSE,alg_scalable;

      ierr = MatGetInfo(A,MAT_LOCAL,&Ainfo);CHKERRQ(ierr);
      ierr = MatGetInfo(B,MAT_LOCAL,&Binfo);CHKERRQ(ierr);
      nz_local = (PetscInt)(Ainfo.nz_allocated + Binfo.nz_allocated);

      if (B->cmap->N > fill*nz_local) alg_scalable_loc = PETSC_TRUE;
      ierr = MPIU_Allreduce(&alg_scalable_loc,&alg_scalable,1,MPIU_BOOL,MPI_LOR,comm);CHKERRQ(ierr);

      if (alg_scalable) {
        alg  = 0; /* scalable algorithm would 50% slower than nonscalable algorithm */
        ierr = PetscInfo2(B,"Use scalable algorithm, BN %D, fill*nz_allocated %g\n",B->cmap->N,fill*nz_local);CHKERRQ(ierr);
      }
    }

    ierr = PetscLogEventBegin(MAT_MatMultSymbolic,A,B,0,0);CHKERRQ(ierr);
    switch (alg) {
    case 1:
      ierr = MatMatMultSymbolic_MPIAIJ_MPIAIJ_nonscalable(A,B,fill,C);CHKERRQ(ierr);
      break;
    case 2:
      ierr = MatMatMultSymbolic_MPIAIJ_MPIAIJ_seqMPI(A,B,fill,C);CHKERRQ(ierr);
      break;
#if defined(PETSC_HAVE_HYPRE)
    case 3:
      ierr = MatMatMultSymbolic_AIJ_AIJ_wHYPRE(A,B,fill,C);CHKERRQ(ierr);
      break;
#endif
    default:
      ierr = MatMatMultSymbolic_MPIAIJ_MPIAIJ(A,B,fill,C);CHKERRQ(ierr);
      break;
    }
    ierr = PetscLogEventEnd(MAT_MatMultSymbolic,A,B,0,0);CHKERRQ(ierr);
  }
  ierr = PetscLogEventBegin(MAT_MatMultNumeric,A,B,0,0);CHKERRQ(ierr);
  ierr = (*(*C)->ops->matmultnumeric)(A,B,*C);CHKERRQ(ierr);
  ierr = PetscLogEventEnd(MAT_MatMultNumeric,A,B,0,0);CHKERRQ(ierr);
  PetscFunctionReturn(0);
}

PetscErrorCode MatDestroy_MPIAIJ_MatMatMult(Mat A)
{
  PetscErrorCode ierr;
  Mat_MPIAIJ     *a    = (Mat_MPIAIJ*)A->data;
  Mat_PtAPMPI    *ptap = a->ptap;

  PetscFunctionBegin;
  ierr = PetscFree2(ptap->startsj_s,ptap->startsj_r);CHKERRQ(ierr);
  ierr = PetscFree(ptap->bufa);CHKERRQ(ierr);
  ierr = MatDestroy(&ptap->P_loc);CHKERRQ(ierr);
  ierr = MatDestroy(&ptap->P_oth);CHKERRQ(ierr);
  ierr = MatDestroy(&ptap->Pt);CHKERRQ(ierr);
  ierr = PetscFree(ptap->api);CHKERRQ(ierr);
  ierr = PetscFree(ptap->apj);CHKERRQ(ierr);
  ierr = PetscFree(ptap->apa);CHKERRQ(ierr);
  ierr = ptap->destroy(A);CHKERRQ(ierr);
  ierr = PetscFree(ptap);CHKERRQ(ierr);
  PetscFunctionReturn(0);
}

PetscErrorCode MatDuplicate_MPIAIJ_MatMatMult(Mat A, MatDuplicateOption op, Mat *M)
{
  PetscErrorCode ierr;
  Mat_MPIAIJ     *a    = (Mat_MPIAIJ*)A->data;
  Mat_PtAPMPI    *ptap = a->ptap;

  PetscFunctionBegin;
  ierr = (*ptap->duplicate)(A,op,M);CHKERRQ(ierr);

  (*M)->ops->destroy   = ptap->destroy;   /* = MatDestroy_MPIAIJ, *M doesn't duplicate A's special structure! */
  (*M)->ops->duplicate = ptap->duplicate; /* = MatDuplicate_MPIAIJ */
  PetscFunctionReturn(0);
}

PetscErrorCode MatMatMultNumeric_MPIAIJ_MPIAIJ_nonscalable(Mat A,Mat P,Mat C)
{
  PetscErrorCode ierr;
  Mat_MPIAIJ     *a  =(Mat_MPIAIJ*)A->data,*c=(Mat_MPIAIJ*)C->data;
  Mat_SeqAIJ     *ad =(Mat_SeqAIJ*)(a->A)->data,*ao=(Mat_SeqAIJ*)(a->B)->data;
  Mat_SeqAIJ     *cd =(Mat_SeqAIJ*)(c->A)->data,*co=(Mat_SeqAIJ*)(c->B)->data;
  PetscScalar    *cda=cd->a,*coa=co->a;
  Mat_SeqAIJ     *p_loc,*p_oth;
  PetscScalar    *apa,*ca;
  PetscInt       cm   =C->rmap->n;
  Mat_PtAPMPI    *ptap=c->ptap;
  PetscInt       *api,*apj,*apJ,i,k;
  PetscInt       cstart=C->cmap->rstart;
  PetscInt       cdnz,conz,k0,k1;
  MPI_Comm       comm;
  PetscMPIInt    size;

  PetscFunctionBegin;
  ierr = PetscObjectGetComm((PetscObject)A,&comm);CHKERRQ(ierr);
  ierr = MPI_Comm_size(comm,&size);CHKERRQ(ierr);

  /* 1) get P_oth = ptap->P_oth  and P_loc = ptap->P_loc */
  /*-----------------------------------------------------*/
  /* update numerical values of P_oth and P_loc */
  ierr = MatGetBrowsOfAoCols_MPIAIJ(A,P,MAT_REUSE_MATRIX,&ptap->startsj_s,&ptap->startsj_r,&ptap->bufa,&ptap->P_oth);CHKERRQ(ierr);
  ierr = MatMPIAIJGetLocalMat(P,MAT_REUSE_MATRIX,&ptap->P_loc);CHKERRQ(ierr);

  /* 2) compute numeric C_loc = A_loc*P = Ad*P_loc + Ao*P_oth */
  /*----------------------------------------------------------*/
  /* get data from symbolic products */
  p_loc = (Mat_SeqAIJ*)(ptap->P_loc)->data;
  p_oth = NULL;
  if (size >1) {
    p_oth = (Mat_SeqAIJ*)(ptap->P_oth)->data;
  }

  /* get apa for storing dense row A[i,:]*P */
  apa = ptap->apa;

  api = ptap->api;
  apj = ptap->apj;
  for (i=0; i<cm; i++) {
    /* compute apa = A[i,:]*P */
    AProw_nonscalable(i,ad,ao,p_loc,p_oth,apa);

    /* set values in C */
    apJ  = apj + api[i];
    cdnz = cd->i[i+1] - cd->i[i];
    conz = co->i[i+1] - co->i[i];

    /* 1st off-diagoanl part of C */
    ca = coa + co->i[i];
    k  = 0;
    for (k0=0; k0<conz; k0++) {
      if (apJ[k] >= cstart) break;
      ca[k0]      = apa[apJ[k]];
      apa[apJ[k++]] = 0.0;
    }

    /* diagonal part of C */
    ca = cda + cd->i[i];
    for (k1=0; k1<cdnz; k1++) {
      ca[k1]      = apa[apJ[k]];
      apa[apJ[k++]] = 0.0;
    }

    /* 2nd off-diagoanl part of C */
    ca = coa + co->i[i];
    for (; k0<conz; k0++) {
      ca[k0]      = apa[apJ[k]];
      apa[apJ[k++]] = 0.0;
    }
  }
  ierr = MatAssemblyBegin(C,MAT_FINAL_ASSEMBLY);CHKERRQ(ierr);
  ierr = MatAssemblyEnd(C,MAT_FINAL_ASSEMBLY);CHKERRQ(ierr);
  PetscFunctionReturn(0);
}

PetscErrorCode MatMatMultSymbolic_MPIAIJ_MPIAIJ_nonscalable(Mat A,Mat P,PetscReal fill,Mat *C)
{
  PetscErrorCode     ierr;
  MPI_Comm           comm;
  PetscMPIInt        size;
  Mat                Cmpi;
  Mat_PtAPMPI        *ptap;
  PetscFreeSpaceList free_space=NULL,current_space=NULL;
  Mat_MPIAIJ         *a        =(Mat_MPIAIJ*)A->data,*c;
  Mat_SeqAIJ         *ad       =(Mat_SeqAIJ*)(a->A)->data,*ao=(Mat_SeqAIJ*)(a->B)->data,*p_loc,*p_oth;
  PetscInt           *pi_loc,*pj_loc,*pi_oth,*pj_oth,*dnz,*onz;
  PetscInt           *adi=ad->i,*adj=ad->j,*aoi=ao->i,*aoj=ao->j,rstart=A->rmap->rstart;
  PetscInt           *lnk,i,pnz,row,*api,*apj,*Jptr,apnz,nspacedouble=0,j,nzi;
  PetscInt           am=A->rmap->n,pN=P->cmap->N,pn=P->cmap->n,pm=P->rmap->n;
  PetscBT            lnkbt;
  PetscScalar        *apa;
  PetscReal          afill;

  PetscFunctionBegin;
  ierr = PetscObjectGetComm((PetscObject)A,&comm);CHKERRQ(ierr);
  ierr = MPI_Comm_size(comm,&size);CHKERRQ(ierr);

  /* create struct Mat_PtAPMPI and attached it to C later */
  ierr = PetscNew(&ptap);CHKERRQ(ierr);

  /* get P_oth by taking rows of P (= non-zero cols of local A) from other processors */
  ierr = MatGetBrowsOfAoCols_MPIAIJ(A,P,MAT_INITIAL_MATRIX,&ptap->startsj_s,&ptap->startsj_r,&ptap->bufa,&ptap->P_oth);CHKERRQ(ierr);

  /* get P_loc by taking all local rows of P */
  ierr = MatMPIAIJGetLocalMat(P,MAT_INITIAL_MATRIX,&ptap->P_loc);CHKERRQ(ierr);

  p_loc  = (Mat_SeqAIJ*)(ptap->P_loc)->data;
  pi_loc = p_loc->i; pj_loc = p_loc->j;
  if (size > 1) {
    p_oth  = (Mat_SeqAIJ*)(ptap->P_oth)->data;
    pi_oth = p_oth->i; pj_oth = p_oth->j;
  } else {
    p_oth = NULL;
    pi_oth = NULL; pj_oth = NULL;
  }

  /* first, compute symbolic AP = A_loc*P = A_diag*P_loc + A_off*P_oth */
  /*-------------------------------------------------------------------*/
  ierr      = PetscMalloc1(am+2,&api);CHKERRQ(ierr);
  ptap->api = api;
  api[0]    = 0;

  /* create and initialize a linked list */
  ierr = PetscLLCondensedCreate(pN,pN,&lnk,&lnkbt);CHKERRQ(ierr);

  /* Initial FreeSpace size is fill*(nnz(A)+nnz(P)) */
  ierr = PetscFreeSpaceGet(PetscRealIntMultTruncate(fill,PetscIntSumTruncate(adi[am],PetscIntSumTruncate(aoi[am],pi_loc[pm]))),&free_space);CHKERRQ(ierr);
  current_space = free_space;

  ierr = MatPreallocateInitialize(comm,am,pn,dnz,onz);CHKERRQ(ierr);
  for (i=0; i<am; i++) {
    /* diagonal portion of A */
    nzi = adi[i+1] - adi[i];
    for (j=0; j<nzi; j++) {
      row  = *adj++;
      pnz  = pi_loc[row+1] - pi_loc[row];
      Jptr = pj_loc + pi_loc[row];
      /* add non-zero cols of P into the sorted linked list lnk */
      ierr = PetscLLCondensedAddSorted(pnz,Jptr,lnk,lnkbt);CHKERRQ(ierr);
    }
    /* off-diagonal portion of A */
    nzi = aoi[i+1] - aoi[i];
    for (j=0; j<nzi; j++) {
      row  = *aoj++;
      pnz  = pi_oth[row+1] - pi_oth[row];
      Jptr = pj_oth + pi_oth[row];
      ierr = PetscLLCondensedAddSorted(pnz,Jptr,lnk,lnkbt);CHKERRQ(ierr);
    }

    apnz     = lnk[0];
    api[i+1] = api[i] + apnz;

    /* if free space is not available, double the total space in the list */
    if (current_space->local_remaining<apnz) {
      ierr = PetscFreeSpaceGet(PetscIntSumTruncate(apnz,current_space->total_array_size),&current_space);CHKERRQ(ierr);
      nspacedouble++;
    }

    /* Copy data into free space, then initialize lnk */
    ierr = PetscLLCondensedClean(pN,apnz,current_space->array,lnk,lnkbt);CHKERRQ(ierr);
    ierr = MatPreallocateSet(i+rstart,apnz,current_space->array,dnz,onz);CHKERRQ(ierr);

    current_space->array           += apnz;
    current_space->local_used      += apnz;
    current_space->local_remaining -= apnz;
  }

  /* Allocate space for apj, initialize apj, and */
  /* destroy list of free space and other temporary array(s) */
  ierr = PetscMalloc1(api[am]+1,&ptap->apj);CHKERRQ(ierr);
  apj  = ptap->apj;
  ierr = PetscFreeSpaceContiguous(&free_space,ptap->apj);CHKERRQ(ierr);
  ierr = PetscLLDestroy(lnk,lnkbt);CHKERRQ(ierr);

  /* malloc apa to store dense row A[i,:]*P */
  ierr = PetscCalloc1(pN,&apa);CHKERRQ(ierr);

  ptap->apa = apa;

  /* create and assemble symbolic parallel matrix Cmpi */
  /*----------------------------------------------------*/
  ierr = MatCreate(comm,&Cmpi);CHKERRQ(ierr);
  ierr = MatSetSizes(Cmpi,am,pn,PETSC_DETERMINE,PETSC_DETERMINE);CHKERRQ(ierr);
  ierr = MatSetBlockSizesFromMats(Cmpi,A,P);CHKERRQ(ierr);

  ierr = MatSetType(Cmpi,MATMPIAIJ);CHKERRQ(ierr);
  ierr = MatMPIAIJSetPreallocation(Cmpi,0,dnz,0,onz);CHKERRQ(ierr);

  ierr = MatSetValues_MPIAIJ_CopyFromCSRFormat_Symbolic(Cmpi, apj, api);CHKERRQ(ierr);
  ierr = MatAssemblyBegin(Cmpi,MAT_FINAL_ASSEMBLY);CHKERRQ(ierr);
  ierr = MatAssemblyEnd(Cmpi,MAT_FINAL_ASSEMBLY);CHKERRQ(ierr);
  ierr = MatPreallocateFinalize(dnz,onz);CHKERRQ(ierr);

  ptap->destroy        = Cmpi->ops->destroy;
  ptap->duplicate      = Cmpi->ops->duplicate;
  Cmpi->ops->matmultnumeric = MatMatMultNumeric_MPIAIJ_MPIAIJ_nonscalable;
  Cmpi->ops->destroy   = MatDestroy_MPIAIJ_MatMatMult;
  Cmpi->ops->duplicate = MatDuplicate_MPIAIJ_MatMatMult;

  /* attach the supporting struct to Cmpi for reuse */
  c       = (Mat_MPIAIJ*)Cmpi->data;
  c->ptap = ptap;

  *C = Cmpi;

  /* set MatInfo */
  afill = (PetscReal)api[am]/(adi[am]+aoi[am]+pi_loc[pm]+1) + 1.e-5;
  if (afill < 1.0) afill = 1.0;
  Cmpi->info.mallocs           = nspacedouble;
  Cmpi->info.fill_ratio_given  = fill;
  Cmpi->info.fill_ratio_needed = afill;

#if defined(PETSC_USE_INFO)
  if (api[am]) {
    ierr = PetscInfo3(Cmpi,"Reallocs %D; Fill ratio: given %g needed %g.\n",nspacedouble,(double)fill,(double)afill);CHKERRQ(ierr);
    ierr = PetscInfo1(Cmpi,"Use MatMatMult(A,B,MatReuse,%g,&C) for best performance.;\n",(double)afill);CHKERRQ(ierr);
  } else {
    ierr = PetscInfo(Cmpi,"Empty matrix product\n");CHKERRQ(ierr);
  }
#endif
  PetscFunctionReturn(0);
}

PETSC_INTERN PetscErrorCode MatMatMult_MPIAIJ_MPIDense(Mat A,Mat B,MatReuse scall,PetscReal fill,Mat *C)
{
  PetscErrorCode ierr;

  PetscFunctionBegin;
  if (scall == MAT_INITIAL_MATRIX) {
    ierr = PetscLogEventBegin(MAT_MatMultSymbolic,A,B,0,0);CHKERRQ(ierr);
    ierr = MatMatMultSymbolic_MPIAIJ_MPIDense(A,B,fill,C);CHKERRQ(ierr);
    ierr = PetscLogEventEnd(MAT_MatMultSymbolic,A,B,0,0);CHKERRQ(ierr);
  }
  ierr = PetscLogEventBegin(MAT_MatMultNumeric,A,B,0,0);CHKERRQ(ierr);
  ierr = MatMatMultNumeric_MPIAIJ_MPIDense(A,B,*C);CHKERRQ(ierr);
  ierr = PetscLogEventEnd(MAT_MatMultNumeric,A,B,0,0);CHKERRQ(ierr);
  PetscFunctionReturn(0);
}

typedef struct {
  Mat         workB;
  PetscScalar *rvalues,*svalues;
  MPI_Request *rwaits,*swaits;
} MPIAIJ_MPIDense;

PetscErrorCode MatMPIAIJ_MPIDenseDestroy(void *ctx)
{
  MPIAIJ_MPIDense *contents = (MPIAIJ_MPIDense*) ctx;
  PetscErrorCode  ierr;

  PetscFunctionBegin;
  ierr = MatDestroy(&contents->workB);CHKERRQ(ierr);
  ierr = PetscFree4(contents->rvalues,contents->svalues,contents->rwaits,contents->swaits);CHKERRQ(ierr);
  ierr = PetscFree(contents);CHKERRQ(ierr);
  PetscFunctionReturn(0);
}

#include <petsc/private/vecscatterimpl.h>
/*
    This is a "dummy function" that handles the case where matrix C was created as a dense matrix
  directly by the user and passed to MatMatMult() with the MAT_REUSE_MATRIX option

  It is the same as MatMatMultSymbolic_MPIAIJ_MPIDense() except does not create C

  Developer Notes: This directly accesses information inside the VecScatter associated with the matrix-vector product
   for this matrix. This is not desirable..

*/
PetscErrorCode MatMatMultNumeric_MPIDense(Mat A,Mat B,Mat C)
{
  PetscErrorCode         ierr;
  PetscBool              flg;
  Mat_MPIAIJ             *aij = (Mat_MPIAIJ*) A->data;
  PetscInt               nz   = aij->B->cmap->n;
  PetscContainer         container;
  MPIAIJ_MPIDense        *contents;
  VecScatter             ctx   = aij->Mvctx;
  VecScatter_MPI_General *from = (VecScatter_MPI_General*) ctx->fromdata;
  VecScatter_MPI_General *to   = (VecScatter_MPI_General*) ctx->todata;

  PetscFunctionBegin;
  ierr = PetscObjectTypeCompare((PetscObject)B,MATMPIDENSE,&flg);CHKERRQ(ierr);
  if (!flg) SETERRQ(PETSC_COMM_SELF,PETSC_ERR_ARG_WRONG,"Second matrix must be mpidense");

  /* Handle case where where user provided the final C matrix rather than calling MatMatMult() with MAT_INITIAL_MATRIX*/
  ierr = PetscObjectTypeCompare((PetscObject)A,MATMPIAIJ,&flg);CHKERRQ(ierr);
  if (!flg) SETERRQ(PETSC_COMM_SELF,PETSC_ERR_ARG_WRONG,"First matrix must be MPIAIJ");

  C->ops->matmultnumeric = MatMatMultNumeric_MPIAIJ_MPIDense;

  ierr = PetscNew(&contents);CHKERRQ(ierr);
  /* Create work matrix used to store off processor rows of B needed for local product */
  ierr = MatCreateSeqDense(PETSC_COMM_SELF,nz,B->cmap->N,NULL,&contents->workB);CHKERRQ(ierr);
  /* Create work arrays needed */
  ierr = PetscMalloc4(B->cmap->N*from->starts[from->n],&contents->rvalues,
                      B->cmap->N*to->starts[to->n],&contents->svalues,
                      from->n,&contents->rwaits,
                      to->n,&contents->swaits);CHKERRQ(ierr);

  ierr = PetscContainerCreate(PetscObjectComm((PetscObject)A),&container);CHKERRQ(ierr);
  ierr = PetscContainerSetPointer(container,contents);CHKERRQ(ierr);
  ierr = PetscContainerSetUserDestroy(container,MatMPIAIJ_MPIDenseDestroy);CHKERRQ(ierr);
  ierr = PetscObjectCompose((PetscObject)C,"workB",(PetscObject)container);CHKERRQ(ierr);
  ierr = PetscContainerDestroy(&container);CHKERRQ(ierr);

  ierr = (*C->ops->matmultnumeric)(A,B,C);CHKERRQ(ierr);
  PetscFunctionReturn(0);
}

PetscErrorCode MatMatMultSymbolic_MPIAIJ_MPIDense(Mat A,Mat B,PetscReal fill,Mat *C)
{
  PetscErrorCode         ierr;
  Mat_MPIAIJ             *aij = (Mat_MPIAIJ*) A->data;
  PetscInt               nz   = aij->B->cmap->n;
  PetscContainer         container;
  MPIAIJ_MPIDense        *contents;
  VecScatter             ctx   = aij->Mvctx;
  VecScatter_MPI_General *from = (VecScatter_MPI_General*) ctx->fromdata;
  VecScatter_MPI_General *to   = (VecScatter_MPI_General*) ctx->todata;
  PetscInt               m     = A->rmap->n,n=B->cmap->n;

  PetscFunctionBegin;
  ierr = MatCreate(PetscObjectComm((PetscObject)B),C);CHKERRQ(ierr);
  ierr = MatSetSizes(*C,m,n,A->rmap->N,B->cmap->N);CHKERRQ(ierr);
  ierr = MatSetBlockSizesFromMats(*C,A,B);CHKERRQ(ierr);
  ierr = MatSetType(*C,MATMPIDENSE);CHKERRQ(ierr);
  ierr = MatMPIDenseSetPreallocation(*C,NULL);CHKERRQ(ierr);
  ierr = MatAssemblyBegin(*C,MAT_FINAL_ASSEMBLY);CHKERRQ(ierr);
  ierr = MatAssemblyEnd(*C,MAT_FINAL_ASSEMBLY);CHKERRQ(ierr);

  (*C)->ops->matmultnumeric = MatMatMultNumeric_MPIAIJ_MPIDense;

  ierr = PetscNew(&contents);CHKERRQ(ierr);
  /* Create work matrix used to store off processor rows of B needed for local product */
  ierr = MatCreateSeqDense(PETSC_COMM_SELF,nz,B->cmap->N,NULL,&contents->workB);CHKERRQ(ierr);
  /* Create work arrays needed */
  ierr = PetscMalloc4(B->cmap->N*from->starts[from->n],&contents->rvalues,
                      B->cmap->N*to->starts[to->n],&contents->svalues,
                      from->n,&contents->rwaits,
                      to->n,&contents->swaits);CHKERRQ(ierr);

  ierr = PetscContainerCreate(PetscObjectComm((PetscObject)A),&container);CHKERRQ(ierr);
  ierr = PetscContainerSetPointer(container,contents);CHKERRQ(ierr);
  ierr = PetscContainerSetUserDestroy(container,MatMPIAIJ_MPIDenseDestroy);CHKERRQ(ierr);
  ierr = PetscObjectCompose((PetscObject)(*C),"workB",(PetscObject)container);CHKERRQ(ierr);
  ierr = PetscContainerDestroy(&container);CHKERRQ(ierr);
  PetscFunctionReturn(0);
}

/*
    Performs an efficient scatter on the rows of B needed by this process; this is
    a modification of the VecScatterBegin_() routines.
*/
PetscErrorCode MatMPIDenseScatter(Mat A,Mat B,Mat C,Mat *outworkB)
{
  Mat_MPIAIJ             *aij = (Mat_MPIAIJ*)A->data;
  PetscErrorCode         ierr;
  PetscScalar            *b,*w,*svalues,*rvalues;
  VecScatter             ctx   = aij->Mvctx;
  VecScatter_MPI_General *from = (VecScatter_MPI_General*) ctx->fromdata;
  VecScatter_MPI_General *to   = (VecScatter_MPI_General*) ctx->todata;
  PetscInt               i,j,k;
  PetscInt               *sindices,*sstarts,*rindices,*rstarts;
  PetscMPIInt            *sprocs,*rprocs,nrecvs;
  MPI_Request            *swaits,*rwaits;
  MPI_Comm               comm;
  PetscMPIInt            tag  = ((PetscObject)ctx)->tag,ncols = B->cmap->N, nrows = aij->B->cmap->n,imdex,nrowsB = B->rmap->n;
  MPI_Status             status;
  MPIAIJ_MPIDense        *contents;
  PetscContainer         container;
  Mat                    workB;

  PetscFunctionBegin;
  ierr = PetscObjectGetComm((PetscObject)A,&comm);CHKERRQ(ierr);
  ierr = PetscObjectQuery((PetscObject)C,"workB",(PetscObject*)&container);CHKERRQ(ierr);
  if (!container) SETERRQ(comm,PETSC_ERR_PLIB,"Container does not exist");
  ierr = PetscContainerGetPointer(container,(void**)&contents);CHKERRQ(ierr);

  workB = *outworkB = contents->workB;
  if (nrows != workB->rmap->n) SETERRQ2(comm,PETSC_ERR_PLIB,"Number of rows of workB %D not equal to columns of aij->B %D",nrows,workB->cmap->n);
  sindices = to->indices;
  sstarts  = to->starts;
  sprocs   = to->procs;
  swaits   = contents->swaits;
  svalues  = contents->svalues;

  rindices = from->indices;
  rstarts  = from->starts;
  rprocs   = from->procs;
  rwaits   = contents->rwaits;
  rvalues  = contents->rvalues;

  ierr = MatDenseGetArray(B,&b);CHKERRQ(ierr);
  ierr = MatDenseGetArray(workB,&w);CHKERRQ(ierr);

  for (i=0; i<from->n; i++) {
    ierr = MPI_Irecv(rvalues+ncols*rstarts[i],ncols*(rstarts[i+1]-rstarts[i]),MPIU_SCALAR,rprocs[i],tag,comm,rwaits+i);CHKERRQ(ierr);
  }

  for (i=0; i<to->n; i++) {
    /* pack a message at a time */
    for (j=0; j<sstarts[i+1]-sstarts[i]; j++) {
      for (k=0; k<ncols; k++) {
        svalues[ncols*(sstarts[i] + j) + k] = b[sindices[sstarts[i]+j] + nrowsB*k];
      }
    }
    ierr = MPI_Isend(svalues+ncols*sstarts[i],ncols*(sstarts[i+1]-sstarts[i]),MPIU_SCALAR,sprocs[i],tag,comm,swaits+i);CHKERRQ(ierr);
  }

  nrecvs = from->n;
  while (nrecvs) {
    ierr = MPI_Waitany(from->n,rwaits,&imdex,&status);CHKERRQ(ierr);
    nrecvs--;
    /* unpack a message at a time */
    for (j=0; j<rstarts[imdex+1]-rstarts[imdex]; j++) {
      for (k=0; k<ncols; k++) {
        w[rindices[rstarts[imdex]+j] + nrows*k] = rvalues[ncols*(rstarts[imdex] + j) + k];
      }
    }
  }
  if (to->n) {ierr = MPI_Waitall(to->n,swaits,to->sstatus);CHKERRQ(ierr);}

  ierr = MatDenseRestoreArray(B,&b);CHKERRQ(ierr);
  ierr = MatDenseRestoreArray(workB,&w);CHKERRQ(ierr);
  ierr = MatAssemblyBegin(workB,MAT_FINAL_ASSEMBLY);CHKERRQ(ierr);
  ierr = MatAssemblyEnd(workB,MAT_FINAL_ASSEMBLY);CHKERRQ(ierr);
  PetscFunctionReturn(0);
}
extern PetscErrorCode MatMatMultNumericAdd_SeqAIJ_SeqDense(Mat,Mat,Mat);

PetscErrorCode MatMatMultNumeric_MPIAIJ_MPIDense(Mat A,Mat B,Mat C)
{
  PetscErrorCode ierr;
  Mat_MPIAIJ     *aij    = (Mat_MPIAIJ*)A->data;
  Mat_MPIDense   *bdense = (Mat_MPIDense*)B->data;
  Mat_MPIDense   *cdense = (Mat_MPIDense*)C->data;
  Mat            workB;

  PetscFunctionBegin;
  /* diagonal block of A times all local rows of B*/
  ierr = MatMatMultNumeric_SeqAIJ_SeqDense(aij->A,bdense->A,cdense->A);CHKERRQ(ierr);

  /* get off processor parts of B needed to complete the product */
  ierr = MatMPIDenseScatter(A,B,C,&workB);CHKERRQ(ierr);

  /* off-diagonal block of A times nonlocal rows of B */
  ierr = MatMatMultNumericAdd_SeqAIJ_SeqDense(aij->B,workB,cdense->A);CHKERRQ(ierr);
  ierr = MatAssemblyBegin(C,MAT_FINAL_ASSEMBLY);CHKERRQ(ierr);
  ierr = MatAssemblyEnd(C,MAT_FINAL_ASSEMBLY);CHKERRQ(ierr);
  PetscFunctionReturn(0);
}

PetscErrorCode MatMatMultNumeric_MPIAIJ_MPIAIJ(Mat A,Mat P,Mat C)
{
  PetscErrorCode ierr;
  Mat_MPIAIJ     *a   = (Mat_MPIAIJ*)A->data,*c=(Mat_MPIAIJ*)C->data;
  Mat_SeqAIJ     *ad  = (Mat_SeqAIJ*)(a->A)->data,*ao=(Mat_SeqAIJ*)(a->B)->data;
  Mat_SeqAIJ     *cd  = (Mat_SeqAIJ*)(c->A)->data,*co=(Mat_SeqAIJ*)(c->B)->data;
  PetscInt       *adi = ad->i,*adj,*aoi=ao->i,*aoj;
  PetscScalar    *ada,*aoa,*cda=cd->a,*coa=co->a;
  Mat_SeqAIJ     *p_loc,*p_oth;
  PetscInt       *pi_loc,*pj_loc,*pi_oth,*pj_oth,*pj;
  PetscScalar    *pa_loc,*pa_oth,*pa,valtmp,*ca;
  PetscInt       cm          = C->rmap->n,anz,pnz;
  Mat_PtAPMPI    *ptap       = c->ptap;
  PetscScalar    *apa_sparse = ptap->apa;
  PetscInt       *api,*apj,*apJ,i,j,k,row;
  PetscInt       cstart = C->cmap->rstart;
  PetscInt       cdnz,conz,k0,k1,nextp;
  MPI_Comm       comm;
  PetscMPIInt    size;

  PetscFunctionBegin;
  ierr = PetscObjectGetComm((PetscObject)A,&comm);CHKERRQ(ierr);
  ierr = MPI_Comm_size(comm,&size);CHKERRQ(ierr);

  /* 1) get P_oth = ptap->P_oth  and P_loc = ptap->P_loc */
  /*-----------------------------------------------------*/
  /* update numerical values of P_oth and P_loc */
  ierr = MatGetBrowsOfAoCols_MPIAIJ(A,P,MAT_REUSE_MATRIX,&ptap->startsj_s,&ptap->startsj_r,&ptap->bufa,&ptap->P_oth);CHKERRQ(ierr);
  ierr = MatMPIAIJGetLocalMat(P,MAT_REUSE_MATRIX,&ptap->P_loc);CHKERRQ(ierr);

  /* 2) compute numeric C_loc = A_loc*P = Ad*P_loc + Ao*P_oth */
  /*----------------------------------------------------------*/
  /* get data from symbolic products */
  p_loc = (Mat_SeqAIJ*)(ptap->P_loc)->data;
  pi_loc = p_loc->i; pj_loc = p_loc->j; pa_loc = p_loc->a;
  if (size >1) {
    p_oth = (Mat_SeqAIJ*)(ptap->P_oth)->data;
    pi_oth = p_oth->i; pj_oth = p_oth->j; pa_oth = p_oth->a;
  } else {
    p_oth = NULL; pi_oth = NULL; pj_oth = NULL; pa_oth = NULL;
  }

  api = ptap->api;
  apj = ptap->apj;
  for (i=0; i<cm; i++) {
    apJ = apj + api[i];

    /* diagonal portion of A */
    anz = adi[i+1] - adi[i];
    adj = ad->j + adi[i];
    ada = ad->a + adi[i];
    for (j=0; j<anz; j++) {
      row = adj[j];
      pnz = pi_loc[row+1] - pi_loc[row];
      pj  = pj_loc + pi_loc[row];
      pa  = pa_loc + pi_loc[row];
      /* perform sparse axpy */
      valtmp = ada[j];
      nextp  = 0;
      for (k=0; nextp<pnz; k++) {
        if (apJ[k] == pj[nextp]) { /* column of AP == column of P */
          apa_sparse[k] += valtmp*pa[nextp++];
        }
      }
      ierr = PetscLogFlops(2.0*pnz);CHKERRQ(ierr);
    }

    /* off-diagonal portion of A */
    anz = aoi[i+1] - aoi[i];
    aoj = ao->j + aoi[i];
    aoa = ao->a + aoi[i];
    for (j=0; j<anz; j++) {
      row = aoj[j];
      pnz = pi_oth[row+1] - pi_oth[row];
      pj  = pj_oth + pi_oth[row];
      pa  = pa_oth + pi_oth[row];
      /* perform sparse axpy */
      valtmp = aoa[j];
      nextp  = 0;
      for (k=0; nextp<pnz; k++) {
        if (apJ[k] == pj[nextp]) { /* column of AP == column of P */
          apa_sparse[k] += valtmp*pa[nextp++];
        }
      }
      ierr = PetscLogFlops(2.0*pnz);CHKERRQ(ierr);
    }

    /* set values in C */
    cdnz = cd->i[i+1] - cd->i[i];
    conz = co->i[i+1] - co->i[i];

    /* 1st off-diagoanl part of C */
    ca = coa + co->i[i];
    k  = 0;
    for (k0=0; k0<conz; k0++) {
      if (apJ[k] >= cstart) break;
      ca[k0]        = apa_sparse[k];
      apa_sparse[k] = 0.0;
      k++;
    }

    /* diagonal part of C */
    ca = cda + cd->i[i];
    for (k1=0; k1<cdnz; k1++) {
      ca[k1]        = apa_sparse[k];
      apa_sparse[k] = 0.0;
      k++;
    }

    /* 2nd off-diagoanl part of C */
    ca = coa + co->i[i];
    for (; k0<conz; k0++) {
      ca[k0]        = apa_sparse[k];
      apa_sparse[k] = 0.0;
      k++;
    }
  }
  ierr = MatAssemblyBegin(C,MAT_FINAL_ASSEMBLY);CHKERRQ(ierr);
  ierr = MatAssemblyEnd(C,MAT_FINAL_ASSEMBLY);CHKERRQ(ierr);
  PetscFunctionReturn(0);
}

/* same as MatMatMultSymbolic_MPIAIJ_MPIAIJ_nonscalable(), except using LLCondensed to avoid O(BN) memory requirement */
PetscErrorCode MatMatMultSymbolic_MPIAIJ_MPIAIJ(Mat A,Mat P,PetscReal fill,Mat *C)
{
  PetscErrorCode     ierr;
  MPI_Comm           comm;
  PetscMPIInt        size;
  Mat                Cmpi;
  Mat_PtAPMPI        *ptap;
  PetscFreeSpaceList free_space = NULL,current_space=NULL;
  Mat_MPIAIJ         *a         = (Mat_MPIAIJ*)A->data,*c;
  Mat_SeqAIJ         *ad        = (Mat_SeqAIJ*)(a->A)->data,*ao=(Mat_SeqAIJ*)(a->B)->data,*p_loc,*p_oth;
  PetscInt           *pi_loc,*pj_loc,*pi_oth,*pj_oth,*dnz,*onz;
  PetscInt           *adi=ad->i,*adj=ad->j,*aoi=ao->i,*aoj=ao->j,rstart=A->rmap->rstart;
  PetscInt           i,pnz,row,*api,*apj,*Jptr,apnz,nspacedouble=0,j,nzi,*lnk,apnz_max=0;
  PetscInt           am=A->rmap->n,pn=P->cmap->n,pm=P->rmap->n,lsize=pn+20;
  PetscReal          afill;
  PetscScalar        *apa;

  PetscFunctionBegin;
  ierr = PetscObjectGetComm((PetscObject)A,&comm);CHKERRQ(ierr);
  ierr = MPI_Comm_size(comm,&size);CHKERRQ(ierr);

  /* create struct Mat_PtAPMPI and attached it to C later */
  ierr = PetscNew(&ptap);CHKERRQ(ierr);

  /* get P_oth by taking rows of P (= non-zero cols of local A) from other processors */
  ierr = MatGetBrowsOfAoCols_MPIAIJ(A,P,MAT_INITIAL_MATRIX,&ptap->startsj_s,&ptap->startsj_r,&ptap->bufa,&ptap->P_oth);CHKERRQ(ierr);

  /* get P_loc by taking all local rows of P */
  ierr = MatMPIAIJGetLocalMat(P,MAT_INITIAL_MATRIX,&ptap->P_loc);CHKERRQ(ierr);

  p_loc  = (Mat_SeqAIJ*)(ptap->P_loc)->data;
  pi_loc = p_loc->i; pj_loc = p_loc->j;
  if (size > 1) {
    p_oth  = (Mat_SeqAIJ*)(ptap->P_oth)->data;
    pi_oth = p_oth->i; pj_oth = p_oth->j;
  } else {
    p_oth  = NULL;
    pi_oth = NULL; pj_oth = NULL;
  }

  /* first, compute symbolic AP = A_loc*P = A_diag*P_loc + A_off*P_oth */
  /*-------------------------------------------------------------------*/
  ierr      = PetscMalloc1(am+2,&api);CHKERRQ(ierr);
  ptap->api = api;
  api[0]    = 0;

  ierr = PetscLLCondensedCreate_Scalable(lsize,&lnk);CHKERRQ(ierr);

  /* Initial FreeSpace size is fill*(nnz(A)+nnz(P)) */
  ierr = PetscFreeSpaceGet(PetscRealIntMultTruncate(fill,PetscIntSumTruncate(adi[am],PetscIntSumTruncate(aoi[am],pi_loc[pm]))),&free_space);CHKERRQ(ierr);
  current_space = free_space;
  ierr = MatPreallocateInitialize(comm,am,pn,dnz,onz);CHKERRQ(ierr);
  for (i=0; i<am; i++) {
    /* diagonal portion of A */
    nzi = adi[i+1] - adi[i];
    for (j=0; j<nzi; j++) {
      row  = *adj++;
      pnz  = pi_loc[row+1] - pi_loc[row];
      Jptr = pj_loc + pi_loc[row];
      /* Expand list if it is not long enough */
      if (pnz+apnz_max > lsize) {
        lsize = pnz+apnz_max;
        ierr = PetscLLCondensedExpand_Scalable(lsize, &lnk);CHKERRQ(ierr);
      }
      /* add non-zero cols of P into the sorted linked list lnk */
      ierr = PetscLLCondensedAddSorted_Scalable(pnz,Jptr,lnk);CHKERRQ(ierr);
      apnz     = *lnk; /* The first element in the list is the number of items in the list */
      api[i+1] = api[i] + apnz;
      if (apnz > apnz_max) apnz_max = apnz;
    }
    /* off-diagonal portion of A */
    nzi = aoi[i+1] - aoi[i];
    for (j=0; j<nzi; j++) {
      row  = *aoj++;
      pnz  = pi_oth[row+1] - pi_oth[row];
      Jptr = pj_oth + pi_oth[row];
      /* Expand list if it is not long enough */
      if (pnz+apnz_max > lsize) {
        lsize = pnz + apnz_max;
        ierr = PetscLLCondensedExpand_Scalable(lsize, &lnk);CHKERRQ(ierr);
      }
      /* add non-zero cols of P into the sorted linked list lnk */
      ierr = PetscLLCondensedAddSorted_Scalable(pnz,Jptr,lnk);CHKERRQ(ierr);
      apnz     = *lnk;  /* The first element in the list is the number of items in the list */
      api[i+1] = api[i] + apnz;
      if (apnz > apnz_max) apnz_max = apnz;
    }
    apnz     = *lnk;
    api[i+1] = api[i] + apnz;
    if (apnz > apnz_max) apnz_max = apnz;

    /* if free space is not available, double the total space in the list */
    if (current_space->local_remaining<apnz) {
      ierr = PetscFreeSpaceGet(PetscIntSumTruncate(apnz,current_space->total_array_size),&current_space);CHKERRQ(ierr);
      nspacedouble++;
    }

    /* Copy data into free space, then initialize lnk */
    ierr = PetscLLCondensedClean_Scalable(apnz,current_space->array,lnk);CHKERRQ(ierr);
    ierr = MatPreallocateSet(i+rstart,apnz,current_space->array,dnz,onz);CHKERRQ(ierr);

    current_space->array           += apnz;
    current_space->local_used      += apnz;
    current_space->local_remaining -= apnz;
  }

  /* Allocate space for apj, initialize apj, and */
  /* destroy list of free space and other temporary array(s) */
  ierr = PetscMalloc1(api[am]+1,&ptap->apj);CHKERRQ(ierr);
  apj  = ptap->apj;
  ierr = PetscFreeSpaceContiguous(&free_space,ptap->apj);CHKERRQ(ierr);
  ierr = PetscLLCondensedDestroy_Scalable(lnk);CHKERRQ(ierr);

  /* create and assemble symbolic parallel matrix Cmpi */
  /*----------------------------------------------------*/
  ierr = MatCreate(comm,&Cmpi);CHKERRQ(ierr);
  ierr = MatSetSizes(Cmpi,am,pn,PETSC_DETERMINE,PETSC_DETERMINE);CHKERRQ(ierr);
  ierr = MatSetBlockSizesFromMats(Cmpi,A,P);CHKERRQ(ierr);
  ierr = MatSetType(Cmpi,MATMPIAIJ);CHKERRQ(ierr);
  ierr = MatMPIAIJSetPreallocation(Cmpi,0,dnz,0,onz);CHKERRQ(ierr);

  /* malloc apa for assembly Cmpi */
  ierr = PetscCalloc1(apnz_max,&apa);CHKERRQ(ierr);
  ptap->apa = apa;

  ierr = MatSetValues_MPIAIJ_CopyFromCSRFormat_Symbolic(Cmpi, apj, api);CHKERRQ(ierr);
  ierr = MatAssemblyBegin(Cmpi,MAT_FINAL_ASSEMBLY);CHKERRQ(ierr);
  ierr = MatAssemblyEnd(Cmpi,MAT_FINAL_ASSEMBLY);CHKERRQ(ierr);
  ierr = MatPreallocateFinalize(dnz,onz);CHKERRQ(ierr);

  ptap->destroy             = Cmpi->ops->destroy;
  ptap->duplicate           = Cmpi->ops->duplicate;
  Cmpi->ops->matmultnumeric = MatMatMultNumeric_MPIAIJ_MPIAIJ;
  Cmpi->ops->destroy        = MatDestroy_MPIAIJ_MatMatMult;
  Cmpi->ops->duplicate      = MatDuplicate_MPIAIJ_MatMatMult;

  /* attach the supporting struct to Cmpi for reuse */
  c       = (Mat_MPIAIJ*)Cmpi->data;
  c->ptap = ptap;
  *C = Cmpi;

  /* set MatInfo */
  afill = (PetscReal)api[am]/(adi[am]+aoi[am]+pi_loc[pm]+1) + 1.e-5;
  if (afill < 1.0) afill = 1.0;
  Cmpi->info.mallocs           = nspacedouble;
  Cmpi->info.fill_ratio_given  = fill;
  Cmpi->info.fill_ratio_needed = afill;

#if defined(PETSC_USE_INFO)
  if (api[am]) {
    ierr = PetscInfo3(Cmpi,"Reallocs %D; Fill ratio: given %g needed %g.\n",nspacedouble,(double)fill,(double)afill);CHKERRQ(ierr);
    ierr = PetscInfo1(Cmpi,"Use MatMatMult(A,B,MatReuse,%g,&C) for best performance.;\n",(double)afill);CHKERRQ(ierr);
  } else {
    ierr = PetscInfo(Cmpi,"Empty matrix product\n");CHKERRQ(ierr);
  }
#endif
  PetscFunctionReturn(0);
}

/* This function is needed for the seqMPI matrix-matrix multiplication.  */
/* Three input arrays are merged to one output array. The size of the    */
/* output array is also output. Duplicate entries only show up once.     */
static void Merge3SortedArrays(PetscInt  size1, PetscInt *in1,
                               PetscInt  size2, PetscInt *in2,
                               PetscInt  size3, PetscInt *in3,
                               PetscInt *size4, PetscInt *out)
{
  int i = 0, j = 0, k = 0, l = 0;

  /* Traverse all three arrays */
  while (i<size1 && j<size2 && k<size3) {
    if (in1[i] < in2[j] && in1[i] < in3[k]) {
      out[l++] = in1[i++];
    }
    else if(in2[j] < in1[i] && in2[j] < in3[k]) {
      out[l++] = in2[j++];
    }
    else if(in3[k] < in1[i] && in3[k] < in2[j]) {
      out[l++] = in3[k++];
    }
    else if(in1[i] == in2[j] && in1[i] < in3[k]) {
      out[l++] = in1[i];
      i++, j++;
    }
    else if(in1[i] == in3[k] && in1[i] < in2[j]) {
      out[l++] = in1[i];
      i++, k++;
    }
    else if(in3[k] == in2[j] && in2[j] < in1[i])  {
      out[l++] = in2[j];
      k++, j++;
    }
    else if(in1[i] == in2[j] && in1[i] == in3[k]) {
      out[l++] = in1[i];
      i++, j++, k++;
    }
  }

  /* Traverse two remaining arrays */
  while (i<size1 && j<size2) {
    if (in1[i] < in2[j]) {
      out[l++] = in1[i++];
    }
    else if(in1[i] > in2[j]) {
      out[l++] = in2[j++];
    }
    else {
      out[l++] = in1[i];
      i++, j++;
    }
  }

  while (i<size1 && k<size3) {
    if (in1[i] < in3[k]) {
      out[l++] = in1[i++];
    }
    else if(in1[i] > in3[k]) {
      out[l++] = in3[k++];
    }
    else {
      out[l++] = in1[i];
      i++, k++;
    }
  }

  while (k<size3 && j<size2)  {
    if (in3[k] < in2[j]) {
      out[l++] = in3[k++];
    }
    else if(in3[k] > in2[j]) {
      out[l++] = in2[j++];
    }
    else {
      out[l++] = in3[k];
      k++, j++;
    }
  }

  /* Traverse one remaining array */
  while (i<size1) out[l++] = in1[i++];
  while (j<size2) out[l++] = in2[j++];
  while (k<size3) out[l++] = in3[k++];

  *size4 = l;
}

/* This matrix-matrix multiplication algorithm divides the multiplication into three multiplications and  */
/* adds up the products. Two of these three multiplications are performed with existing (sequential)      */
/* matrix-matrix multiplications.  */
#undef __FUNCT__
#define __FUNCT__ "MatMatMultSymbolic_MPIAIJ_MPIAIJ_seqMPI"
PetscErrorCode MatMatMultSymbolic_MPIAIJ_MPIAIJ_seqMPI(Mat A, Mat P, PetscReal fill, Mat *C)
{
  PetscErrorCode     ierr;
  MPI_Comm           comm;
  PetscMPIInt        size;
  Mat                Cmpi;
  Mat_PtAPMPI        *ptap;
  PetscFreeSpaceList free_space_diag=NULL, current_space=NULL;
  Mat_MPIAIJ         *a        =(Mat_MPIAIJ*)A->data;
  Mat_SeqAIJ         *ad       =(Mat_SeqAIJ*)(a->A)->data,*ao=(Mat_SeqAIJ*)(a->B)->data,*p_loc;
  Mat_MPIAIJ         *p        =(Mat_MPIAIJ*)P->data;
  Mat_MPIAIJ         *c;
  Mat_SeqAIJ         *adpd_seq, *p_off, *aopoth_seq;
  PetscInt           adponz, adpdnz;
  PetscInt           *pi_loc,*dnz,*onz;
  PetscInt           *adi=ad->i,*adj=ad->j,*aoi=ao->i,rstart=A->rmap->rstart;
  PetscInt           *lnk,i, i1=0,pnz,row,*adpoi,*adpoj, *api, *adpoJ, *aopJ, *apJ,*Jptr, aopnz, nspacedouble=0,j,nzi,
                     *apj,apnz, *adpdi, *adpdj, *adpdJ, *poff_i, *poff_j, *j_temp, *aopothi, *aopothj;
  PetscInt           am=A->rmap->n,pN=P->cmap->N,pn=P->cmap->n,pm=P->rmap->n, p_colstart, p_colend;
  PetscBT            lnkbt;
  PetscScalar        *apa;
  PetscReal          afill;
  PetscMPIInt        rank;
  Mat                adpd, aopoth;

  PetscFunctionBegin;
  ierr = PetscObjectGetComm((PetscObject)A,&comm);CHKERRQ(ierr);
  ierr = MPI_Comm_size(comm,&size);CHKERRQ(ierr);
  ierr = MPI_Comm_rank(comm, &rank);CHKERRQ(ierr);
  ierr = MatGetOwnershipRangeColumn(P, &p_colstart, &p_colend); CHKERRQ(ierr);

  /* create struct Mat_PtAPMPI and attached it to C later */
  ierr = PetscNew(&ptap);CHKERRQ(ierr);

  /* get P_oth by taking rows of P (= non-zero cols of local A) from other processors */
  ierr = MatGetBrowsOfAoCols_MPIAIJ(A,P,MAT_INITIAL_MATRIX,&ptap->startsj_s,&ptap->startsj_r,&ptap->bufa,&ptap->P_oth);CHKERRQ(ierr);

  /* get P_loc by taking all local rows of P */
  ierr = MatMPIAIJGetLocalMat(P,MAT_INITIAL_MATRIX,&ptap->P_loc);CHKERRQ(ierr);


  p_loc  = (Mat_SeqAIJ*)(ptap->P_loc)->data;
  pi_loc = p_loc->i;

  /* Allocate memory for the i arrays of the matrices A*P, A_diag*P_off and A_offd * P */
  ierr      = PetscMalloc1(am+2,&api);CHKERRQ(ierr);
  ierr      = PetscMalloc1(am+2,&adpoi);CHKERRQ(ierr);

  adpoi[0]    = 0;
  ptap->api = api;
  api[0] = 0;

  /* create and initialize a linked list, will be used for both A_diag * P_loc_off and A_offd * P_oth */
  ierr = PetscLLCondensedCreate(pN,pN,&lnk,&lnkbt);CHKERRQ(ierr);
  ierr = MatPreallocateInitialize(comm,am,pn,dnz,onz);CHKERRQ(ierr);

  /* Symbolic calc of A_loc_diag * P_loc_diag */
  ierr = MatMatMultSymbolic_SeqAIJ_SeqAIJ(a->A, p->A, fill, &adpd);CHKERRQ(ierr);
  adpd_seq = (Mat_SeqAIJ*)((adpd)->data);
  adpdi = adpd_seq->i; adpdj = adpd_seq->j;
  p_off = (Mat_SeqAIJ*)((p->B)->data);
  poff_i = p_off->i; poff_j = p_off->j;

  /* j_temp stores indices of a result row before they are added to the linked list */
  ierr = PetscMalloc1(pN+2,&j_temp);CHKERRQ(ierr);


  /* Symbolic calc of the A_diag * p_loc_off */
  /* Initial FreeSpace size is fill*(nnz(A)+nnz(P)) */
  ierr = PetscFreeSpaceGet(PetscRealIntMultTruncate(fill,PetscIntSumTruncate(adi[am],PetscIntSumTruncate(aoi[am],pi_loc[pm]))),&free_space_diag);CHKERRQ(ierr);
  current_space = free_space_diag;

  for (i=0; i<am; i++) {
    /* A_diag * P_loc_off */
    nzi = adi[i+1] - adi[i];
    for (j=0; j<nzi; j++) {
      row  = *adj++;
      pnz  = poff_i[row+1] - poff_i[row];
      Jptr = poff_j + poff_i[row];
      for(i1 = 0; i1 < pnz; i1++) {
        j_temp[i1] = p->garray[Jptr[i1]];
      }
      /* add non-zero cols of P into the sorted linked list lnk */
      ierr = PetscLLCondensedAddSorted(pnz,j_temp,lnk,lnkbt);CHKERRQ(ierr);
    }

    adponz     = lnk[0];
    adpoi[i+1] = adpoi[i] + adponz;

    /* if free space is not available, double the total space in the list */
    if (current_space->local_remaining<adponz) {
      ierr = PetscFreeSpaceGet(PetscIntSumTruncate(adponz,current_space->total_array_size),&current_space);CHKERRQ(ierr);
      nspacedouble++;
    }

    /* Copy data into free space, then initialize lnk */
    ierr = PetscLLCondensedClean(pN,adponz,current_space->array,lnk,lnkbt);CHKERRQ(ierr);

    current_space->array           += adponz;
    current_space->local_used      += adponz;
    current_space->local_remaining -= adponz;
  }

  /* Symbolic calc of A_off * P_oth */
  ierr = MatMatMultSymbolic_SeqAIJ_SeqAIJ(a->B, ptap->P_oth, fill, &aopoth);CHKERRQ(ierr);
  aopoth_seq = (Mat_SeqAIJ*)((aopoth)->data);
  aopothi = aopoth_seq->i; aopothj = aopoth_seq->j;

  /* Allocate space for apj, adpj, aopj, ... */
  /* destroy lists of free space and other temporary array(s) */

  ierr = PetscMalloc1(aopothi[am] + adpoi[am] + adpdi[am]+2, &ptap->apj);CHKERRQ(ierr);
  ierr = PetscMalloc1(adpoi[am]+2, &adpoj);CHKERRQ(ierr);

  /* Copy from linked list to j-array */
  ierr = PetscFreeSpaceContiguous(&free_space_diag,adpoj);CHKERRQ(ierr);
  ierr = PetscLLDestroy(lnk,lnkbt);CHKERRQ(ierr);

  adpoJ = adpoj;
  adpdJ = adpdj;
  aopJ = aopothj;
  apj  = ptap->apj;
  apJ = apj; /* still empty */

  /* Merge j-arrays of A_off * P, A_diag * P_loc_off, and */
  /* A_diag * P_loc_diag to get A*P */
  for (i = 0; i < am; i++) {
    aopnz  =  aopothi[i+1] -  aopothi[i];
    adponz = adpoi[i+1] - adpoi[i];
    adpdnz = adpdi[i+1] - adpdi[i];

    /* Correct indices from A_diag*P_diag */
    for(i1 = 0; i1 < adpdnz; i1++) {
      adpdJ[i1] += p_colstart;
    }
    /* Merge j-arrays of A_diag * P_loc_off and A_diag * P_loc_diag and A_off * P_oth */
    Merge3SortedArrays(adponz, adpoJ, adpdnz, adpdJ, aopnz, aopJ, &apnz, apJ);
    ierr = MatPreallocateSet(i+rstart, apnz, apJ, dnz, onz); CHKERRQ(ierr);

    aopJ += aopnz;
    adpoJ += adponz;
    adpdJ += adpdnz;
    apJ += apnz;
    api[i+1] = api[i] + apnz;
  }

  /* malloc apa to store dense row A[i,:]*P */
  ierr = PetscCalloc1(pN+2,&apa);CHKERRQ(ierr);

  ptap->apa = apa;
  /* create and assemble symbolic parallel matrix Cmpi */
  ierr = MatCreate(comm,&Cmpi);CHKERRQ(ierr);
  ierr = MatSetSizes(Cmpi,am,pn,PETSC_DETERMINE,PETSC_DETERMINE);CHKERRQ(ierr);
  ierr = MatSetBlockSizesFromMats(Cmpi,A,P);CHKERRQ(ierr);

  ierr = MatSetType(Cmpi,MATMPIAIJ);CHKERRQ(ierr);
  ierr = MatMPIAIJSetPreallocation(Cmpi,0,dnz,0,onz);CHKERRQ(ierr);


  ierr = MatSetValues_MPIAIJ_CopyFromCSRFormat_Symbolic(Cmpi, apj, api);CHKERRQ(ierr);
  ierr = MatAssemblyBegin(Cmpi,MAT_FINAL_ASSEMBLY);CHKERRQ(ierr);
  ierr = MatAssemblyEnd(Cmpi,MAT_FINAL_ASSEMBLY);CHKERRQ(ierr);
  ierr = MatPreallocateFinalize(dnz,onz);CHKERRQ(ierr);


  ptap->destroy        = Cmpi->ops->destroy;
  ptap->duplicate      = Cmpi->ops->duplicate;
  Cmpi->ops->matmultnumeric = MatMatMultNumeric_MPIAIJ_MPIAIJ_nonscalable;
  Cmpi->ops->destroy   = MatDestroy_MPIAIJ_MatMatMult;
  Cmpi->ops->duplicate = MatDuplicate_MPIAIJ_MatMatMult;

  /* attach the supporting struct to Cmpi for reuse */
  c       = (Mat_MPIAIJ*)Cmpi->data;
  c->ptap = ptap;
  *C = Cmpi;

  /* set MatInfo */
  afill = (PetscReal)api[am]/(adi[am]+aoi[am]+pi_loc[pm]+1) + 1.e-5;
  if (afill < 1.0) afill = 1.0;
  Cmpi->info.mallocs           = nspacedouble;
  Cmpi->info.fill_ratio_given  = fill;
  Cmpi->info.fill_ratio_needed = afill;

#if defined(PETSC_USE_INFO)
  if (api[am]) {
    ierr = PetscInfo3(Cmpi,"Reallocs %D; Fill ratio: given %g needed %g.\n",nspacedouble,(double)fill,(double)afill);CHKERRQ(ierr);
    ierr = PetscInfo1(Cmpi,"Use MatMatMult(A,B,MatReuse,%g,&C) for best performance.;\n",(double)afill);CHKERRQ(ierr);
  } else {
    ierr = PetscInfo(Cmpi,"Empty matrix product\n");CHKERRQ(ierr);
  }
#endif

  ierr = MatDestroy(&aopoth);CHKERRQ(ierr);
  ierr = MatDestroy(&adpd);CHKERRQ(ierr);
  ierr = PetscFree(j_temp);CHKERRQ(ierr);
  ierr = PetscFree(adpoj);CHKERRQ(ierr);
  ierr = PetscFree(adpoi);CHKERRQ(ierr);
  PetscFunctionReturn(0);
}


/*-------------------------------------------------------------------------*/
PetscErrorCode MatTransposeMatMult_MPIAIJ_MPIAIJ(Mat P,Mat A,MatReuse scall,PetscReal fill,Mat *C)
{
  PetscErrorCode ierr;
  const char     *algTypes[3] = {"scalable","nonscalable","matmatmult"};
  PetscInt       aN=A->cmap->N,alg=1; /* set default algorithm */
  PetscBool      flg;

  PetscFunctionBegin;
  if (scall == MAT_INITIAL_MATRIX) {
    ierr = PetscObjectOptionsBegin((PetscObject)A);CHKERRQ(ierr);
    PetscOptionsObject->alreadyprinted = PETSC_FALSE; /* a hack to ensure the option shows in '-help' */
    ierr = PetscOptionsEList("-mattransposematmult_via","Algorithmic approach","MatTransposeMatMult",algTypes,3,algTypes[1],&alg,&flg);CHKERRQ(ierr);
    ierr = PetscOptionsEnd();CHKERRQ(ierr);

    ierr = PetscLogEventBegin(MAT_TransposeMatMultSymbolic,P,A,0,0);CHKERRQ(ierr);
    switch (alg) {
    case 1:
      if (!flg && aN > 100000) { /* may switch to scalable algorithm as default */
        MatInfo     Ainfo,Pinfo;
        PetscInt    nz_local;
        PetscBool   alg_scalable_loc=PETSC_FALSE,alg_scalable;
        MPI_Comm    comm;

        ierr = MatGetInfo(A,MAT_LOCAL,&Ainfo);CHKERRQ(ierr);
        ierr = MatGetInfo(P,MAT_LOCAL,&Pinfo);CHKERRQ(ierr);
        nz_local = (PetscInt)(Ainfo.nz_allocated + Pinfo.nz_allocated); /* estimated local nonzero entries */

        if (aN > fill*nz_local) alg_scalable_loc = PETSC_TRUE;
        ierr = PetscObjectGetComm((PetscObject)A,&comm);CHKERRQ(ierr);
        ierr = MPIU_Allreduce(&alg_scalable_loc,&alg_scalable,1,MPIU_BOOL,MPI_LOR,comm);CHKERRQ(ierr);

        if (alg_scalable) {
          alg  = 0; /* scalable algorithm would slower than nonscalable algorithm */
          ierr = MatTransposeMatMultSymbolic_MPIAIJ_MPIAIJ(P,A,fill,C);CHKERRQ(ierr);
          break;
        }
      }
      ierr = MatTransposeMatMultSymbolic_MPIAIJ_MPIAIJ_nonscalable(P,A,fill,C);CHKERRQ(ierr);
      break;
    case 2:
    {
      Mat         Pt;
      Mat_PtAPMPI *ptap;
      Mat_MPIAIJ  *c;
      ierr = MatTranspose(P,MAT_INITIAL_MATRIX,&Pt);CHKERRQ(ierr);
      ierr = MatMatMult(Pt,A,MAT_INITIAL_MATRIX,fill,C);CHKERRQ(ierr);
      c        = (Mat_MPIAIJ*)(*C)->data;
      ptap     = c->ptap;
      ptap->Pt = Pt;
      (*C)->ops->mattransposemultnumeric = MatTransposeMatMultNumeric_MPIAIJ_MPIAIJ_matmatmult;
      PetscFunctionReturn(0);
    }
      break;
    default: /* scalable algorithm */
      ierr = MatTransposeMatMultSymbolic_MPIAIJ_MPIAIJ(P,A,fill,C);CHKERRQ(ierr);
      break;
    }
    ierr = PetscLogEventEnd(MAT_TransposeMatMultSymbolic,P,A,0,0);CHKERRQ(ierr);
  }
  ierr = PetscLogEventBegin(MAT_TransposeMatMultNumeric,P,A,0,0);CHKERRQ(ierr);
  ierr = (*(*C)->ops->mattransposemultnumeric)(P,A,*C);CHKERRQ(ierr);
  ierr = PetscLogEventEnd(MAT_TransposeMatMultNumeric,P,A,0,0);CHKERRQ(ierr);
  PetscFunctionReturn(0);
}

/* This routine only works when scall=MAT_REUSE_MATRIX! */
PetscErrorCode MatTransposeMatMultNumeric_MPIAIJ_MPIAIJ_matmatmult(Mat P,Mat A,Mat C)
{
  PetscErrorCode ierr;
  Mat_MPIAIJ     *c=(Mat_MPIAIJ*)C->data;
  Mat_PtAPMPI    *ptap= c->ptap;
  Mat            Pt=ptap->Pt;

  PetscFunctionBegin;
  ierr = MatTranspose(P,MAT_REUSE_MATRIX,&Pt);CHKERRQ(ierr);
  ierr = MatMatMultNumeric(Pt,A,C);CHKERRQ(ierr);
  PetscFunctionReturn(0);
}

PetscErrorCode MatDuplicate_MPIAIJ_MatPtAP(Mat,MatDuplicateOption,Mat*);

/* This routine is modified from MatPtAPSymbolic_MPIAIJ_MPIAIJ() */
PetscErrorCode MatTransposeMatMultSymbolic_MPIAIJ_MPIAIJ_nonscalable(Mat P,Mat A,PetscReal fill,Mat *C)
{
  PetscErrorCode      ierr;
  Mat_PtAPMPI         *ptap;
  Mat_MPIAIJ          *p=(Mat_MPIAIJ*)P->data,*c;
  MPI_Comm            comm;
  PetscMPIInt         size,rank;
  Mat                 Cmpi;
  PetscFreeSpaceList  free_space=NULL,current_space=NULL;
  PetscInt            pn=P->cmap->n,aN=A->cmap->N,an=A->cmap->n;
  PetscInt            *lnk,i,k,nsend;
  PetscBT             lnkbt;
  PetscMPIInt         tagi,tagj,*len_si,*len_s,*len_ri,icompleted=0,nrecv;
  PetscInt            **buf_rj,**buf_ri,**buf_ri_k;
  PetscInt            len,proc,*dnz,*onz,*owners,nzi;
  PetscInt            nrows,*buf_s,*buf_si,*buf_si_i,**nextrow,**nextci;
  MPI_Request         *swaits,*rwaits;
  MPI_Status          *sstatus,rstatus;
  PetscLayout         rowmap;
  PetscInt            *owners_co,*coi,*coj;    /* i and j array of (p->B)^T*A*P - used in the communication */
  PetscMPIInt         *len_r,*id_r;    /* array of length of comm->size, store send/recv matrix values */
  PetscInt            *Jptr,*prmap=p->garray,con,j,Crmax;
  Mat_SeqAIJ          *a_loc,*c_loc,*c_oth;
  PetscTable          ta;

  PetscFunctionBegin;
  ierr = PetscObjectGetComm((PetscObject)A,&comm);CHKERRQ(ierr);
  ierr = MPI_Comm_size(comm,&size);CHKERRQ(ierr);
  ierr = MPI_Comm_rank(comm,&rank);CHKERRQ(ierr);

  /* create symbolic parallel matrix Cmpi */
  ierr = MatCreate(comm,&Cmpi);CHKERRQ(ierr);
  ierr = MatSetType(Cmpi,MATMPIAIJ);CHKERRQ(ierr);

  Cmpi->ops->mattransposemultnumeric = MatTransposeMatMultNumeric_MPIAIJ_MPIAIJ_nonscalable;

  /* create struct Mat_PtAPMPI and attached it to C later */
  ierr = PetscNew(&ptap);CHKERRQ(ierr);
  ptap->reuse = MAT_INITIAL_MATRIX;

  /* (0) compute Rd = Pd^T, Ro = Po^T  */
  /* --------------------------------- */
  ierr = MatTranspose_SeqAIJ(p->A,MAT_INITIAL_MATRIX,&ptap->Rd);CHKERRQ(ierr);
  ierr = MatTranspose_SeqAIJ(p->B,MAT_INITIAL_MATRIX,&ptap->Ro);CHKERRQ(ierr);

  /* (1) compute symbolic A_loc */
  /* ---------------------------*/
  ierr = MatMPIAIJGetLocalMat(A,MAT_INITIAL_MATRIX,&ptap->A_loc);CHKERRQ(ierr);

  /* (2-1) compute symbolic C_oth = Ro*A_loc  */
  /* ------------------------------------ */
  ierr = MatMatMultSymbolic_SeqAIJ_SeqAIJ(ptap->Ro,ptap->A_loc,fill,&ptap->C_oth);CHKERRQ(ierr);

  /* (3) send coj of C_oth to other processors  */
  /* ------------------------------------------ */
  /* determine row ownership */
  ierr = PetscLayoutCreate(comm,&rowmap);CHKERRQ(ierr);
  rowmap->n  = pn;
  rowmap->bs = 1;
  ierr   = PetscLayoutSetUp(rowmap);CHKERRQ(ierr);
  owners = rowmap->range;

  /* determine the number of messages to send, their lengths */
  ierr = PetscMalloc4(size,&len_s,size,&len_si,size,&sstatus,size+2,&owners_co);CHKERRQ(ierr);
  ierr = PetscMemzero(len_s,size*sizeof(PetscMPIInt));CHKERRQ(ierr);
  ierr = PetscMemzero(len_si,size*sizeof(PetscMPIInt));CHKERRQ(ierr);

  c_oth = (Mat_SeqAIJ*)ptap->C_oth->data;
  coi   = c_oth->i; coj = c_oth->j;
  con   = ptap->C_oth->rmap->n;
  proc  = 0;
  for (i=0; i<con; i++) {
    while (prmap[i] >= owners[proc+1]) proc++;
    len_si[proc]++;               /* num of rows in Co(=Pt*A) to be sent to [proc] */
    len_s[proc] += coi[i+1] - coi[i]; /* num of nonzeros in Co to be sent to [proc] */
  }

  len          = 0; /* max length of buf_si[], see (4) */
  owners_co[0] = 0;
  nsend        = 0;
  for (proc=0; proc<size; proc++) {
    owners_co[proc+1] = owners_co[proc] + len_si[proc];
    if (len_s[proc]) {
      nsend++;
      len_si[proc] = 2*(len_si[proc] + 1); /* length of buf_si to be sent to [proc] */
      len         += len_si[proc];
    }
  }

  /* determine the number and length of messages to receive for coi and coj  */
  ierr = PetscGatherNumberOfMessages(comm,NULL,len_s,&nrecv);CHKERRQ(ierr);
  ierr = PetscGatherMessageLengths2(comm,nsend,nrecv,len_s,len_si,&id_r,&len_r,&len_ri);CHKERRQ(ierr);

  /* post the Irecv and Isend of coj */
  ierr = PetscCommGetNewTag(comm,&tagj);CHKERRQ(ierr);
  ierr = PetscPostIrecvInt(comm,tagj,nrecv,id_r,len_r,&buf_rj,&rwaits);CHKERRQ(ierr);
  ierr = PetscMalloc1(nsend+1,&swaits);CHKERRQ(ierr);
  for (proc=0, k=0; proc<size; proc++) {
    if (!len_s[proc]) continue;
    i    = owners_co[proc];
    ierr = MPI_Isend(coj+coi[i],len_s[proc],MPIU_INT,proc,tagj,comm,swaits+k);CHKERRQ(ierr);
    k++;
  }

  /* (2-2) compute symbolic C_loc = Rd*A_loc */
  /* ---------------------------------------- */
  ierr = MatMatMultSymbolic_SeqAIJ_SeqAIJ(ptap->Rd,ptap->A_loc,fill,&ptap->C_loc);CHKERRQ(ierr);
  c_loc = (Mat_SeqAIJ*)ptap->C_loc->data;

  /* receives coj are complete */
  for (i=0; i<nrecv; i++) {
    ierr = MPI_Waitany(nrecv,rwaits,&icompleted,&rstatus);CHKERRQ(ierr);
  }
  ierr = PetscFree(rwaits);CHKERRQ(ierr);
  if (nsend) {ierr = MPI_Waitall(nsend,swaits,sstatus);CHKERRQ(ierr);}

  /* add received column indices into ta to update Crmax */
  a_loc = (Mat_SeqAIJ*)(ptap->A_loc)->data;

  /* create and initialize a linked list */
  ierr = PetscTableCreate(an,aN,&ta);CHKERRQ(ierr); /* for compute Crmax */
  MatRowMergeMax_SeqAIJ(a_loc,ptap->A_loc->rmap->N,ta);

  for (k=0; k<nrecv; k++) {/* k-th received message */
    Jptr = buf_rj[k];
    for (j=0; j<len_r[k]; j++) {
      ierr = PetscTableAdd(ta,*(Jptr+j)+1,1,INSERT_VALUES);CHKERRQ(ierr);
    }
  }
  ierr = PetscTableGetCount(ta,&Crmax);CHKERRQ(ierr);
  ierr = PetscTableDestroy(&ta);CHKERRQ(ierr);

  /* (4) send and recv coi */
  /*-----------------------*/
  ierr   = PetscCommGetNewTag(comm,&tagi);CHKERRQ(ierr);
  ierr   = PetscPostIrecvInt(comm,tagi,nrecv,id_r,len_ri,&buf_ri,&rwaits);CHKERRQ(ierr);
  ierr   = PetscMalloc1(len+1,&buf_s);CHKERRQ(ierr);
  buf_si = buf_s;  /* points to the beginning of k-th msg to be sent */
  for (proc=0,k=0; proc<size; proc++) {
    if (!len_s[proc]) continue;
    /* form outgoing message for i-structure:
         buf_si[0]:                 nrows to be sent
               [1:nrows]:           row index (global)
               [nrows+1:2*nrows+1]: i-structure index
    */
    /*-------------------------------------------*/
    nrows       = len_si[proc]/2 - 1; /* num of rows in Co to be sent to [proc] */
    buf_si_i    = buf_si + nrows+1;
    buf_si[0]   = nrows;
    buf_si_i[0] = 0;
    nrows       = 0;
    for (i=owners_co[proc]; i<owners_co[proc+1]; i++) {
      nzi = coi[i+1] - coi[i];
      buf_si_i[nrows+1] = buf_si_i[nrows] + nzi;  /* i-structure */
      buf_si[nrows+1]   = prmap[i] -owners[proc]; /* local row index */
      nrows++;
    }
    ierr = MPI_Isend(buf_si,len_si[proc],MPIU_INT,proc,tagi,comm,swaits+k);CHKERRQ(ierr);
    k++;
    buf_si += len_si[proc];
  }
  for (i=0; i<nrecv; i++) {
    ierr = MPI_Waitany(nrecv,rwaits,&icompleted,&rstatus);CHKERRQ(ierr);
  }
  ierr = PetscFree(rwaits);CHKERRQ(ierr);
  if (nsend) {ierr = MPI_Waitall(nsend,swaits,sstatus);CHKERRQ(ierr);}

  ierr = PetscFree4(len_s,len_si,sstatus,owners_co);CHKERRQ(ierr);
  ierr = PetscFree(len_ri);CHKERRQ(ierr);
  ierr = PetscFree(swaits);CHKERRQ(ierr);
  ierr = PetscFree(buf_s);CHKERRQ(ierr);

  /* (5) compute the local portion of Cmpi      */
  /* ------------------------------------------ */
  /* set initial free space to be Crmax, sufficient for holding nozeros in each row of Cmpi */
  ierr          = PetscFreeSpaceGet(Crmax,&free_space);CHKERRQ(ierr);
  current_space = free_space;

  ierr = PetscMalloc3(nrecv,&buf_ri_k,nrecv,&nextrow,nrecv,&nextci);CHKERRQ(ierr);
  for (k=0; k<nrecv; k++) {
    buf_ri_k[k] = buf_ri[k]; /* beginning of k-th recved i-structure */
    nrows       = *buf_ri_k[k];
    nextrow[k]  = buf_ri_k[k] + 1;  /* next row number of k-th recved i-structure */
    nextci[k]   = buf_ri_k[k] + (nrows + 1); /* poins to the next i-structure of k-th recved i-structure  */
  }

  ierr = MatPreallocateInitialize(comm,pn,an,dnz,onz);CHKERRQ(ierr);
  ierr = PetscLLCondensedCreate(Crmax,aN,&lnk,&lnkbt);CHKERRQ(ierr);
  for (i=0; i<pn; i++) {
    /* add C_loc into Cmpi */
    nzi  = c_loc->i[i+1] - c_loc->i[i];
    Jptr = c_loc->j + c_loc->i[i];
    ierr = PetscLLCondensedAddSorted(nzi,Jptr,lnk,lnkbt);CHKERRQ(ierr);

    /* add received col data into lnk */
    for (k=0; k<nrecv; k++) { /* k-th received message */
      if (i == *nextrow[k]) { /* i-th row */
        nzi  = *(nextci[k]+1) - *nextci[k];
        Jptr = buf_rj[k] + *nextci[k];
        ierr = PetscLLCondensedAddSorted(nzi,Jptr,lnk,lnkbt);CHKERRQ(ierr);
        nextrow[k]++; nextci[k]++;
      }
    }
    nzi = lnk[0];

    /* copy data into free space, then initialize lnk */
    ierr = PetscLLCondensedClean(aN,nzi,current_space->array,lnk,lnkbt);CHKERRQ(ierr);
    ierr = MatPreallocateSet(i+owners[rank],nzi,current_space->array,dnz,onz);CHKERRQ(ierr);
  }
  ierr = PetscFree3(buf_ri_k,nextrow,nextci);CHKERRQ(ierr);
  ierr = PetscLLDestroy(lnk,lnkbt);CHKERRQ(ierr);
  ierr = PetscFreeSpaceDestroy(free_space);CHKERRQ(ierr);

  /* local sizes and preallocation */
  ierr = MatSetSizes(Cmpi,pn,an,PETSC_DETERMINE,PETSC_DETERMINE);CHKERRQ(ierr);
  ierr = MatSetBlockSizes(Cmpi,PetscAbs(P->cmap->bs),PetscAbs(P->cmap->bs));CHKERRQ(ierr);
  ierr = MatMPIAIJSetPreallocation(Cmpi,0,dnz,0,onz);CHKERRQ(ierr);
  ierr = MatPreallocateFinalize(dnz,onz);CHKERRQ(ierr);

  /* members in merge */
  ierr = PetscFree(id_r);CHKERRQ(ierr);
  ierr = PetscFree(len_r);CHKERRQ(ierr);
  ierr = PetscFree(buf_ri[0]);CHKERRQ(ierr);
  ierr = PetscFree(buf_ri);CHKERRQ(ierr);
  ierr = PetscFree(buf_rj[0]);CHKERRQ(ierr);
  ierr = PetscFree(buf_rj);CHKERRQ(ierr);
  ierr = PetscLayoutDestroy(&rowmap);CHKERRQ(ierr);

  /* attach the supporting struct to Cmpi for reuse */
  c = (Mat_MPIAIJ*)Cmpi->data;
  c->ptap         = ptap;
  ptap->duplicate = Cmpi->ops->duplicate;
  ptap->destroy   = Cmpi->ops->destroy;

  /* Cmpi is not ready for use - assembly will be done by MatPtAPNumeric() */
  Cmpi->assembled        = PETSC_FALSE;
  Cmpi->ops->destroy     = MatDestroy_MPIAIJ_PtAP;
  Cmpi->ops->duplicate   = MatDuplicate_MPIAIJ_MatPtAP;
  *C                     = Cmpi;
  PetscFunctionReturn(0);
}

PetscErrorCode MatTransposeMatMultNumeric_MPIAIJ_MPIAIJ_nonscalable(Mat P,Mat A,Mat C)
{
  PetscErrorCode    ierr;
  Mat_MPIAIJ        *p=(Mat_MPIAIJ*)P->data,*c=(Mat_MPIAIJ*)C->data;
  Mat_SeqAIJ        *c_seq;
  Mat_PtAPMPI       *ptap = c->ptap;
  Mat               A_loc,C_loc,C_oth;
  PetscInt          i,rstart,rend,cm,ncols,row;
  const PetscInt    *cols;
  const PetscScalar *vals;

  PetscFunctionBegin;
  ierr = MatZeroEntries(C);CHKERRQ(ierr);

  if (ptap->reuse == MAT_REUSE_MATRIX) {
    /* These matrices are obtained in MatTransposeMatMultSymbolic() */
    /* 1) get R = Pd^T, Ro = Po^T */
    /*----------------------------*/
    ierr = MatTranspose_SeqAIJ(p->A,MAT_REUSE_MATRIX,&ptap->Rd);CHKERRQ(ierr);
    ierr = MatTranspose_SeqAIJ(p->B,MAT_REUSE_MATRIX,&ptap->Ro);CHKERRQ(ierr);

    /* 2) compute numeric A_loc */
    /*--------------------------*/
    ierr = MatMPIAIJGetLocalMat(A,MAT_REUSE_MATRIX,&ptap->A_loc);CHKERRQ(ierr);
  }

  /* 3) C_loc = Rd*A_loc, C_oth = Ro*A_loc */
  A_loc = ptap->A_loc;
  ierr = ((ptap->C_loc)->ops->matmultnumeric)(ptap->Rd,A_loc,ptap->C_loc);CHKERRQ(ierr);
  ierr = ((ptap->C_oth)->ops->matmultnumeric)(ptap->Ro,A_loc,ptap->C_oth);CHKERRQ(ierr);
  C_loc = ptap->C_loc;
  C_oth = ptap->C_oth;

  /* add C_loc and Co to to C */
  ierr = MatGetOwnershipRange(C,&rstart,&rend);CHKERRQ(ierr);

  /* C_loc -> C */
  cm    = C_loc->rmap->N;
  c_seq = (Mat_SeqAIJ*)C_loc->data;
  cols = c_seq->j;
  vals = c_seq->a;
  for (i=0; i<cm; i++) {
    ncols = c_seq->i[i+1] - c_seq->i[i];
    row = rstart + i;
    ierr = MatSetValues(C,1,&row,ncols,cols,vals,ADD_VALUES);CHKERRQ(ierr);
    cols += ncols; vals += ncols;
  }

  /* Co -> C, off-processor part */
  cm    = C_oth->rmap->N;
  c_seq = (Mat_SeqAIJ*)C_oth->data;
  cols  = c_seq->j;
  vals  = c_seq->a;
  for (i=0; i<cm; i++) {
    ncols = c_seq->i[i+1] - c_seq->i[i];
    row = p->garray[i];
    ierr = MatSetValues(C,1,&row,ncols,cols,vals,ADD_VALUES);CHKERRQ(ierr);
    cols += ncols; vals += ncols;
  }
  ierr = MatAssemblyBegin(C,MAT_FINAL_ASSEMBLY);CHKERRQ(ierr);
  ierr = MatAssemblyEnd(C,MAT_FINAL_ASSEMBLY);CHKERRQ(ierr);

  ptap->reuse = MAT_REUSE_MATRIX;
  PetscFunctionReturn(0);
}

PetscErrorCode MatTransposeMatMultNumeric_MPIAIJ_MPIAIJ(Mat P,Mat A,Mat C)
{
  PetscErrorCode      ierr;
  Mat_Merge_SeqsToMPI *merge;
  Mat_MPIAIJ          *p =(Mat_MPIAIJ*)P->data,*c=(Mat_MPIAIJ*)C->data;
  Mat_SeqAIJ          *pd=(Mat_SeqAIJ*)(p->A)->data,*po=(Mat_SeqAIJ*)(p->B)->data;
  Mat_PtAPMPI         *ptap;
  PetscInt            *adj;
  PetscInt            i,j,k,anz,pnz,row,*cj,nexta;
  MatScalar           *ada,*ca,valtmp;
  PetscInt            am  =A->rmap->n,cm=C->rmap->n,pon=(p->B)->cmap->n;
  MPI_Comm            comm;
  PetscMPIInt         size,rank,taga,*len_s;
  PetscInt            *owners,proc,nrows,**buf_ri_k,**nextrow,**nextci;
  PetscInt            **buf_ri,**buf_rj;
  PetscInt            cnz=0,*bj_i,*bi,*bj,bnz,nextcj;  /* bi,bj,ba: local array of C(mpi mat) */
  MPI_Request         *s_waits,*r_waits;
  MPI_Status          *status;
  MatScalar           **abuf_r,*ba_i,*pA,*coa,*ba;
  PetscInt            *ai,*aj,*coi,*coj,*poJ,*pdJ;
  Mat                 A_loc;
  Mat_SeqAIJ          *a_loc;

  PetscFunctionBegin;
  ierr = PetscObjectGetComm((PetscObject)C,&comm);CHKERRQ(ierr);
  ierr = MPI_Comm_size(comm,&size);CHKERRQ(ierr);
  ierr = MPI_Comm_rank(comm,&rank);CHKERRQ(ierr);

  ptap  = c->ptap;
  merge = ptap->merge;

  /* 2) compute numeric C_seq = P_loc^T*A_loc */
  /*------------------------------------------*/
  /* get data from symbolic products */
  coi    = merge->coi; coj = merge->coj;
  ierr   = PetscCalloc1(coi[pon]+1,&coa);CHKERRQ(ierr);
  bi     = merge->bi; bj = merge->bj;
  owners = merge->rowmap->range;
  ierr   = PetscCalloc1(bi[cm]+1,&ba);CHKERRQ(ierr);

  /* get A_loc by taking all local rows of A */
  A_loc = ptap->A_loc;
  ierr  = MatMPIAIJGetLocalMat(A,MAT_REUSE_MATRIX,&A_loc);CHKERRQ(ierr);
  a_loc = (Mat_SeqAIJ*)(A_loc)->data;
  ai    = a_loc->i;
  aj    = a_loc->j;

  for (i=0; i<am; i++) {
    anz = ai[i+1] - ai[i];
    adj = aj + ai[i];
    ada = a_loc->a + ai[i];

    /* 2-b) Compute Cseq = P_loc[i,:]^T*A[i,:] using outer product */
    /*-------------------------------------------------------------*/
    /* put the value into Co=(p->B)^T*A (off-diagonal part, send to others) */
    pnz = po->i[i+1] - po->i[i];
    poJ = po->j + po->i[i];
    pA  = po->a + po->i[i];
    for (j=0; j<pnz; j++) {
      row = poJ[j];
      cj  = coj + coi[row];
      ca  = coa + coi[row];
      /* perform sparse axpy */
      nexta  = 0;
      valtmp = pA[j];
      for (k=0; nexta<anz; k++) {
        if (cj[k] == adj[nexta]) {
          ca[k] += valtmp*ada[nexta];
          nexta++;
        }
      }
      ierr = PetscLogFlops(2.0*anz);CHKERRQ(ierr);
    }

    /* put the value into Cd (diagonal part) */
    pnz = pd->i[i+1] - pd->i[i];
    pdJ = pd->j + pd->i[i];
    pA  = pd->a + pd->i[i];
    for (j=0; j<pnz; j++) {
      row = pdJ[j];
      cj  = bj + bi[row];
      ca  = ba + bi[row];
      /* perform sparse axpy */
      nexta  = 0;
      valtmp = pA[j];
      for (k=0; nexta<anz; k++) {
        if (cj[k] == adj[nexta]) {
          ca[k] += valtmp*ada[nexta];
          nexta++;
        }
      }
      ierr = PetscLogFlops(2.0*anz);CHKERRQ(ierr);
    }
  }

  /* 3) send and recv matrix values coa */
  /*------------------------------------*/
  buf_ri = merge->buf_ri;
  buf_rj = merge->buf_rj;
  len_s  = merge->len_s;
  ierr   = PetscCommGetNewTag(comm,&taga);CHKERRQ(ierr);
  ierr   = PetscPostIrecvScalar(comm,taga,merge->nrecv,merge->id_r,merge->len_r,&abuf_r,&r_waits);CHKERRQ(ierr);

  ierr = PetscMalloc2(merge->nsend+1,&s_waits,size,&status);CHKERRQ(ierr);
  for (proc=0,k=0; proc<size; proc++) {
    if (!len_s[proc]) continue;
    i    = merge->owners_co[proc];
    ierr = MPI_Isend(coa+coi[i],len_s[proc],MPIU_MATSCALAR,proc,taga,comm,s_waits+k);CHKERRQ(ierr);
    k++;
  }
  if (merge->nrecv) {ierr = MPI_Waitall(merge->nrecv,r_waits,status);CHKERRQ(ierr);}
  if (merge->nsend) {ierr = MPI_Waitall(merge->nsend,s_waits,status);CHKERRQ(ierr);}

  ierr = PetscFree2(s_waits,status);CHKERRQ(ierr);
  ierr = PetscFree(r_waits);CHKERRQ(ierr);
  ierr = PetscFree(coa);CHKERRQ(ierr);

  /* 4) insert local Cseq and received values into Cmpi */
  /*----------------------------------------------------*/
  ierr = PetscMalloc3(merge->nrecv,&buf_ri_k,merge->nrecv,&nextrow,merge->nrecv,&nextci);CHKERRQ(ierr);
  for (k=0; k<merge->nrecv; k++) {
    buf_ri_k[k] = buf_ri[k]; /* beginning of k-th recved i-structure */
    nrows       = *(buf_ri_k[k]);
    nextrow[k]  = buf_ri_k[k]+1;  /* next row number of k-th recved i-structure */
    nextci[k]   = buf_ri_k[k] + (nrows + 1); /* poins to the next i-structure of k-th recved i-structure  */
  }

  for (i=0; i<cm; i++) {
    row  = owners[rank] + i; /* global row index of C_seq */
    bj_i = bj + bi[i];  /* col indices of the i-th row of C */
    ba_i = ba + bi[i];
    bnz  = bi[i+1] - bi[i];
    /* add received vals into ba */
    for (k=0; k<merge->nrecv; k++) { /* k-th received message */
      /* i-th row */
      if (i == *nextrow[k]) {
        cnz    = *(nextci[k]+1) - *nextci[k];
        cj     = buf_rj[k] + *(nextci[k]);
        ca     = abuf_r[k] + *(nextci[k]);
        nextcj = 0;
        for (j=0; nextcj<cnz; j++) {
          if (bj_i[j] == cj[nextcj]) { /* bcol == ccol */
            ba_i[j] += ca[nextcj++];
          }
        }
        nextrow[k]++; nextci[k]++;
        ierr = PetscLogFlops(2.0*cnz);CHKERRQ(ierr);
      }
    }
    ierr = MatSetValues(C,1,&row,bnz,bj_i,ba_i,INSERT_VALUES);CHKERRQ(ierr);
  }
  ierr = MatAssemblyBegin(C,MAT_FINAL_ASSEMBLY);CHKERRQ(ierr);
  ierr = MatAssemblyEnd(C,MAT_FINAL_ASSEMBLY);CHKERRQ(ierr);

  ierr = PetscFree(ba);CHKERRQ(ierr);
  ierr = PetscFree(abuf_r[0]);CHKERRQ(ierr);
  ierr = PetscFree(abuf_r);CHKERRQ(ierr);
  ierr = PetscFree3(buf_ri_k,nextrow,nextci);CHKERRQ(ierr);
  PetscFunctionReturn(0);
}

PetscErrorCode MatTransposeMatMultSymbolic_MPIAIJ_MPIAIJ(Mat P,Mat A,PetscReal fill,Mat *C)
{
  PetscErrorCode      ierr;
  Mat                 Cmpi,A_loc,POt,PDt;
  Mat_PtAPMPI         *ptap;
  PetscFreeSpaceList  free_space=NULL,current_space=NULL;
  Mat_MPIAIJ          *p=(Mat_MPIAIJ*)P->data,*a=(Mat_MPIAIJ*)A->data,*c;
  PetscInt            *pdti,*pdtj,*poti,*potj,*ptJ;
  PetscInt            nnz;
  PetscInt            *lnk,*owners_co,*coi,*coj,i,k,pnz,row;
  PetscInt            am  =A->rmap->n,pn=P->cmap->n;
  MPI_Comm            comm;
  PetscMPIInt         size,rank,tagi,tagj,*len_si,*len_s,*len_ri;
  PetscInt            **buf_rj,**buf_ri,**buf_ri_k;
  PetscInt            len,proc,*dnz,*onz,*owners;
  PetscInt            nzi,*bi,*bj;
  PetscInt            nrows,*buf_s,*buf_si,*buf_si_i,**nextrow,**nextci;
  MPI_Request         *swaits,*rwaits;
  MPI_Status          *sstatus,rstatus;
  Mat_Merge_SeqsToMPI *merge;
  PetscInt            *ai,*aj,*Jptr,anz,*prmap=p->garray,pon,nspacedouble=0,j;
  PetscReal           afill  =1.0,afill_tmp;
  PetscInt            rstart = P->cmap->rstart,rmax,aN=A->cmap->N,Armax;
  PetscScalar         *vals;
  Mat_SeqAIJ          *a_loc,*pdt,*pot;
  PetscTable          ta;

  PetscFunctionBegin;
  ierr = PetscObjectGetComm((PetscObject)A,&comm);CHKERRQ(ierr);
  /* check if matrix local sizes are compatible */
  if (A->rmap->rstart != P->rmap->rstart || A->rmap->rend != P->rmap->rend) SETERRQ4(comm,PETSC_ERR_ARG_SIZ,"Matrix local dimensions are incompatible, A (%D, %D) != P (%D,%D)",A->rmap->rstart,A->rmap->rend,P->rmap->rstart,P->rmap->rend);

  ierr = MPI_Comm_size(comm,&size);CHKERRQ(ierr);
  ierr = MPI_Comm_rank(comm,&rank);CHKERRQ(ierr);

  /* create struct Mat_PtAPMPI and attached it to C later */
  ierr = PetscNew(&ptap);CHKERRQ(ierr);

  /* get A_loc by taking all local rows of A */
  ierr = MatMPIAIJGetLocalMat(A,MAT_INITIAL_MATRIX,&A_loc);CHKERRQ(ierr);

  ptap->A_loc = A_loc;
  a_loc       = (Mat_SeqAIJ*)(A_loc)->data;
  ai          = a_loc->i;
  aj          = a_loc->j;

  /* determine symbolic Co=(p->B)^T*A - send to others */
  /*----------------------------------------------------*/
  ierr = MatTransposeSymbolic_SeqAIJ(p->A,&PDt);CHKERRQ(ierr);
  pdt  = (Mat_SeqAIJ*)PDt->data;
  pdti = pdt->i; pdtj = pdt->j;

  ierr = MatTransposeSymbolic_SeqAIJ(p->B,&POt);CHKERRQ(ierr);
  pot  = (Mat_SeqAIJ*)POt->data;
  poti = pot->i; potj = pot->j;

  /* then, compute symbolic Co = (p->B)^T*A */
  pon    = (p->B)->cmap->n; /* total num of rows to be sent to other processors
                         >= (num of nonzero rows of C_seq) - pn */
  ierr   = PetscMalloc1(pon+1,&coi);CHKERRQ(ierr);
  coi[0] = 0;

  /* set initial free space to be fill*(nnz(p->B) + nnz(A)) */
  nnz           = PetscRealIntMultTruncate(fill,PetscIntSumTruncate(poti[pon],ai[am]));
  ierr          = PetscFreeSpaceGet(nnz,&free_space);CHKERRQ(ierr);
  current_space = free_space;

  /* create and initialize a linked list */
  ierr = PetscTableCreate(A->cmap->n + a->B->cmap->N,aN,&ta);CHKERRQ(ierr);
  MatRowMergeMax_SeqAIJ(a_loc,am,ta);
  ierr = PetscTableGetCount(ta,&Armax);CHKERRQ(ierr);

  ierr = PetscLLCondensedCreate_Scalable(Armax,&lnk);CHKERRQ(ierr);

  for (i=0; i<pon; i++) {
    pnz = poti[i+1] - poti[i];
    ptJ = potj + poti[i];
    for (j=0; j<pnz; j++) {
      row  = ptJ[j]; /* row of A_loc == col of Pot */
      anz  = ai[row+1] - ai[row];
      Jptr = aj + ai[row];
      /* add non-zero cols of AP into the sorted linked list lnk */
      ierr = PetscLLCondensedAddSorted_Scalable(anz,Jptr,lnk);CHKERRQ(ierr);
    }
    nnz = lnk[0];

    /* If free space is not available, double the total space in the list */
    if (current_space->local_remaining<nnz) {
      ierr = PetscFreeSpaceGet(PetscIntSumTruncate(nnz,current_space->total_array_size),&current_space);CHKERRQ(ierr);
      nspacedouble++;
    }

    /* Copy data into free space, and zero out denserows */
    ierr = PetscLLCondensedClean_Scalable(nnz,current_space->array,lnk);CHKERRQ(ierr);

    current_space->array           += nnz;
    current_space->local_used      += nnz;
    current_space->local_remaining -= nnz;

    coi[i+1] = coi[i] + nnz;
  }

  ierr = PetscMalloc1(coi[pon]+1,&coj);CHKERRQ(ierr);
  ierr = PetscFreeSpaceContiguous(&free_space,coj);CHKERRQ(ierr);
  ierr = PetscLLCondensedDestroy_Scalable(lnk);CHKERRQ(ierr); /* must destroy to get a new one for C */

  afill_tmp = (PetscReal)coi[pon]/(poti[pon] + ai[am]+1);
  if (afill_tmp > afill) afill = afill_tmp;

  /* send j-array (coj) of Co to other processors */
  /*----------------------------------------------*/
  /* determine row ownership */
  ierr = PetscNew(&merge);CHKERRQ(ierr);
  ierr = PetscLayoutCreate(comm,&merge->rowmap);CHKERRQ(ierr);

  merge->rowmap->n  = pn;
  merge->rowmap->bs = 1;

  ierr   = PetscLayoutSetUp(merge->rowmap);CHKERRQ(ierr);
  owners = merge->rowmap->range;

  /* determine the number of messages to send, their lengths */
  ierr = PetscCalloc1(size,&len_si);CHKERRQ(ierr);
  ierr = PetscMalloc1(size,&merge->len_s);CHKERRQ(ierr);

  len_s        = merge->len_s;
  merge->nsend = 0;

  ierr = PetscMalloc1(size+2,&owners_co);CHKERRQ(ierr);
  ierr = PetscMemzero(len_s,size*sizeof(PetscMPIInt));CHKERRQ(ierr);

  proc = 0;
  for (i=0; i<pon; i++) {
    while (prmap[i] >= owners[proc+1]) proc++;
    len_si[proc]++;  /* num of rows in Co to be sent to [proc] */
    len_s[proc] += coi[i+1] - coi[i];
  }

  len          = 0; /* max length of buf_si[] */
  owners_co[0] = 0;
  for (proc=0; proc<size; proc++) {
    owners_co[proc+1] = owners_co[proc] + len_si[proc];
    if (len_si[proc]) {
      merge->nsend++;
      len_si[proc] = 2*(len_si[proc] + 1);
      len         += len_si[proc];
    }
  }

  /* determine the number and length of messages to receive for coi and coj  */
  ierr = PetscGatherNumberOfMessages(comm,NULL,len_s,&merge->nrecv);CHKERRQ(ierr);
  ierr = PetscGatherMessageLengths2(comm,merge->nsend,merge->nrecv,len_s,len_si,&merge->id_r,&merge->len_r,&len_ri);CHKERRQ(ierr);

  /* post the Irecv and Isend of coj */
  ierr = PetscCommGetNewTag(comm,&tagj);CHKERRQ(ierr);
  ierr = PetscPostIrecvInt(comm,tagj,merge->nrecv,merge->id_r,merge->len_r,&buf_rj,&rwaits);CHKERRQ(ierr);
  ierr = PetscMalloc1(merge->nsend+1,&swaits);CHKERRQ(ierr);
  for (proc=0, k=0; proc<size; proc++) {
    if (!len_s[proc]) continue;
    i    = owners_co[proc];
    ierr = MPI_Isend(coj+coi[i],len_s[proc],MPIU_INT,proc,tagj,comm,swaits+k);CHKERRQ(ierr);
    k++;
  }

  /* receives and sends of coj are complete */
  ierr = PetscMalloc1(size,&sstatus);CHKERRQ(ierr);
  for (i=0; i<merge->nrecv; i++) {
    PetscMPIInt icompleted;
    ierr = MPI_Waitany(merge->nrecv,rwaits,&icompleted,&rstatus);CHKERRQ(ierr);
  }
  ierr = PetscFree(rwaits);CHKERRQ(ierr);
  if (merge->nsend) {ierr = MPI_Waitall(merge->nsend,swaits,sstatus);CHKERRQ(ierr);}

  /* add received column indices into table to update Armax */
  /* Armax can be as large as aN if a P[row,:] is dense, see src/ksp/ksp/examples/tutorials/ex56.c! */
  for (k=0; k<merge->nrecv; k++) {/* k-th received message */
    Jptr = buf_rj[k];
    for (j=0; j<merge->len_r[k]; j++) {
      ierr = PetscTableAdd(ta,*(Jptr+j)+1,1,INSERT_VALUES);CHKERRQ(ierr);
    }
  }
  ierr = PetscTableGetCount(ta,&Armax);CHKERRQ(ierr);
  /* printf("Armax %d, an %d + Bn %d = %d, aN %d\n",Armax,A->cmap->n,a->B->cmap->N,A->cmap->n+a->B->cmap->N,aN); */

  /* send and recv coi */
  /*-------------------*/
  ierr   = PetscCommGetNewTag(comm,&tagi);CHKERRQ(ierr);
  ierr   = PetscPostIrecvInt(comm,tagi,merge->nrecv,merge->id_r,len_ri,&buf_ri,&rwaits);CHKERRQ(ierr);
  ierr   = PetscMalloc1(len+1,&buf_s);CHKERRQ(ierr);
  buf_si = buf_s;  /* points to the beginning of k-th msg to be sent */
  for (proc=0,k=0; proc<size; proc++) {
    if (!len_s[proc]) continue;
    /* form outgoing message for i-structure:
         buf_si[0]:                 nrows to be sent
               [1:nrows]:           row index (global)
               [nrows+1:2*nrows+1]: i-structure index
    */
    /*-------------------------------------------*/
    nrows       = len_si[proc]/2 - 1;
    buf_si_i    = buf_si + nrows+1;
    buf_si[0]   = nrows;
    buf_si_i[0] = 0;
    nrows       = 0;
    for (i=owners_co[proc]; i<owners_co[proc+1]; i++) {
      nzi               = coi[i+1] - coi[i];
      buf_si_i[nrows+1] = buf_si_i[nrows] + nzi;  /* i-structure */
      buf_si[nrows+1]   = prmap[i] -owners[proc]; /* local row index */
      nrows++;
    }
    ierr = MPI_Isend(buf_si,len_si[proc],MPIU_INT,proc,tagi,comm,swaits+k);CHKERRQ(ierr);
    k++;
    buf_si += len_si[proc];
  }
  i = merge->nrecv;
  while (i--) {
    PetscMPIInt icompleted;
    ierr = MPI_Waitany(merge->nrecv,rwaits,&icompleted,&rstatus);CHKERRQ(ierr);
  }
  ierr = PetscFree(rwaits);CHKERRQ(ierr);
  if (merge->nsend) {ierr = MPI_Waitall(merge->nsend,swaits,sstatus);CHKERRQ(ierr);}
  ierr = PetscFree(len_si);CHKERRQ(ierr);
  ierr = PetscFree(len_ri);CHKERRQ(ierr);
  ierr = PetscFree(swaits);CHKERRQ(ierr);
  ierr = PetscFree(sstatus);CHKERRQ(ierr);
  ierr = PetscFree(buf_s);CHKERRQ(ierr);

  /* compute the local portion of C (mpi mat) */
  /*------------------------------------------*/
  /* allocate bi array and free space for accumulating nonzero column info */
  ierr  = PetscMalloc1(pn+1,&bi);CHKERRQ(ierr);
  bi[0] = 0;

  /* set initial free space to be fill*(nnz(P) + nnz(AP)) */
  nnz           = PetscRealIntMultTruncate(fill,PetscIntSumTruncate(pdti[pn],PetscIntSumTruncate(poti[pon],ai[am])));
  ierr          = PetscFreeSpaceGet(nnz,&free_space);CHKERRQ(ierr);
  current_space = free_space;

  ierr = PetscMalloc3(merge->nrecv,&buf_ri_k,merge->nrecv,&nextrow,merge->nrecv,&nextci);CHKERRQ(ierr);
  for (k=0; k<merge->nrecv; k++) {
    buf_ri_k[k] = buf_ri[k]; /* beginning of k-th recved i-structure */
    nrows       = *buf_ri_k[k];
    nextrow[k]  = buf_ri_k[k] + 1;  /* next row number of k-th recved i-structure */
    nextci[k]   = buf_ri_k[k] + (nrows + 1); /* points to the next i-structure of k-th recieved i-structure  */
  }

  ierr = PetscLLCondensedCreate_Scalable(Armax,&lnk);CHKERRQ(ierr);
  ierr = MatPreallocateInitialize(comm,pn,A->cmap->n,dnz,onz);CHKERRQ(ierr);
  rmax = 0;
  for (i=0; i<pn; i++) {
    /* add pdt[i,:]*AP into lnk */
    pnz = pdti[i+1] - pdti[i];
    ptJ = pdtj + pdti[i];
    for (j=0; j<pnz; j++) {
      row  = ptJ[j];  /* row of AP == col of Pt */
      anz  = ai[row+1] - ai[row];
      Jptr = aj + ai[row];
      /* add non-zero cols of AP into the sorted linked list lnk */
      ierr = PetscLLCondensedAddSorted_Scalable(anz,Jptr,lnk);CHKERRQ(ierr);
    }

    /* add received col data into lnk */
    for (k=0; k<merge->nrecv; k++) { /* k-th received message */
      if (i == *nextrow[k]) { /* i-th row */
        nzi  = *(nextci[k]+1) - *nextci[k];
        Jptr = buf_rj[k] + *nextci[k];
        ierr = PetscLLCondensedAddSorted_Scalable(nzi,Jptr,lnk);CHKERRQ(ierr);
        nextrow[k]++; nextci[k]++;
      }
    }
    nnz = lnk[0];

    /* if free space is not available, make more free space */
    if (current_space->local_remaining<nnz) {
      ierr = PetscFreeSpaceGet(PetscIntSumTruncate(nnz,current_space->total_array_size),&current_space);CHKERRQ(ierr);
      nspacedouble++;
    }
    /* copy data into free space, then initialize lnk */
    ierr = PetscLLCondensedClean_Scalable(nnz,current_space->array,lnk);CHKERRQ(ierr);
    ierr = MatPreallocateSet(i+owners[rank],nnz,current_space->array,dnz,onz);CHKERRQ(ierr);

    current_space->array           += nnz;
    current_space->local_used      += nnz;
    current_space->local_remaining -= nnz;

    bi[i+1] = bi[i] + nnz;
    if (nnz > rmax) rmax = nnz;
  }
  ierr = PetscFree3(buf_ri_k,nextrow,nextci);CHKERRQ(ierr);

  ierr      = PetscMalloc1(bi[pn]+1,&bj);CHKERRQ(ierr);
  ierr      = PetscFreeSpaceContiguous(&free_space,bj);CHKERRQ(ierr);
  afill_tmp = (PetscReal)bi[pn]/(pdti[pn] + poti[pon] + ai[am]+1);
  if (afill_tmp > afill) afill = afill_tmp;
  ierr = PetscLLCondensedDestroy_Scalable(lnk);CHKERRQ(ierr);
  ierr = PetscTableDestroy(&ta);CHKERRQ(ierr);

  ierr = MatDestroy(&POt);CHKERRQ(ierr);
  ierr = MatDestroy(&PDt);CHKERRQ(ierr);

  /* create symbolic parallel matrix Cmpi - why cannot be assembled in Numeric part   */
  /*----------------------------------------------------------------------------------*/
  ierr = PetscCalloc1(rmax+1,&vals);CHKERRQ(ierr);

  ierr = MatCreate(comm,&Cmpi);CHKERRQ(ierr);
  ierr = MatSetSizes(Cmpi,pn,A->cmap->n,PETSC_DETERMINE,PETSC_DETERMINE);CHKERRQ(ierr);
  ierr = MatSetBlockSizes(Cmpi,PetscAbs(P->cmap->bs),PetscAbs(A->cmap->bs));CHKERRQ(ierr);
  ierr = MatSetType(Cmpi,MATMPIAIJ);CHKERRQ(ierr);
  ierr = MatMPIAIJSetPreallocation(Cmpi,0,dnz,0,onz);CHKERRQ(ierr);
  ierr = MatPreallocateFinalize(dnz,onz);CHKERRQ(ierr);
  ierr = MatSetBlockSize(Cmpi,1);CHKERRQ(ierr);
  for (i=0; i<pn; i++) {
    row  = i + rstart;
    nnz  = bi[i+1] - bi[i];
    Jptr = bj + bi[i];
    ierr = MatSetValues(Cmpi,1,&row,nnz,Jptr,vals,INSERT_VALUES);CHKERRQ(ierr);
  }
  ierr = MatAssemblyBegin(Cmpi,MAT_FINAL_ASSEMBLY);CHKERRQ(ierr);
  ierr = MatAssemblyEnd(Cmpi,MAT_FINAL_ASSEMBLY);CHKERRQ(ierr);
  ierr = PetscFree(vals);CHKERRQ(ierr);

  merge->bi        = bi;
  merge->bj        = bj;
  merge->coi       = coi;
  merge->coj       = coj;
  merge->buf_ri    = buf_ri;
  merge->buf_rj    = buf_rj;
  merge->owners_co = owners_co;

  /* attach the supporting struct to Cmpi for reuse */
  c = (Mat_MPIAIJ*)Cmpi->data;

  c->ptap     = ptap;
  ptap->api   = NULL;
  ptap->apj   = NULL;
  ptap->merge = merge;
  ptap->apa   = NULL;
  ptap->destroy   = Cmpi->ops->destroy;
  ptap->duplicate = Cmpi->ops->duplicate;

  Cmpi->ops->mattransposemultnumeric = MatTransposeMatMultNumeric_MPIAIJ_MPIAIJ;
  Cmpi->ops->destroy                 = MatDestroy_MPIAIJ_PtAP;
  Cmpi->ops->duplicate               = MatDuplicate_MPIAIJ_MatPtAP;

  *C = Cmpi;
#if defined(PETSC_USE_INFO)
  if (bi[pn] != 0) {
    ierr = PetscInfo3(Cmpi,"Reallocs %D; Fill ratio: given %g needed %g.\n",nspacedouble,(double)fill,(double)afill);CHKERRQ(ierr);
    ierr = PetscInfo1(Cmpi,"Use MatTransposeMatMult(A,B,MatReuse,%g,&C) for best performance.\n",(double)afill);CHKERRQ(ierr);
  } else {
    ierr = PetscInfo(Cmpi,"Empty matrix product\n");CHKERRQ(ierr);
  }
#endif
  PetscFunctionReturn(0);
}
