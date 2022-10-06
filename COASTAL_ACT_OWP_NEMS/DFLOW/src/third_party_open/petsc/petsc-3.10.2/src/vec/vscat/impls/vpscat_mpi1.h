
/*
     Defines the methods VecScatterBegin/End_1,2,......
     This is included by vpscat.c with different values for BS

     This is a terrible way of doing "templates" in C.
*/
#define PETSCMAP1_a(a,b)  a ## _ ## b
#define PETSCMAP1_b(a,b)  PETSCMAP1_a(a,b)
#define PETSCMAP1(a)      PETSCMAP1_b(a,BS)

PetscErrorCode PETSCMAP1(VecScatterBeginMPI1)(VecScatter ctx,Vec xin,Vec yin,InsertMode addv,ScatterMode mode)
{
  VecScatter_MPI_General *to,*from;
  PetscScalar            *xv,*yv,*svalues;
  MPI_Request            *rwaits,*swaits;
  PetscErrorCode         ierr;
  PetscInt               i,*indices,*sstarts,nrecvs,nsends,bs;
#if defined(PETSC_HAVE_VECCUDA)
  PetscBool              is_cudatype = PETSC_FALSE;
#endif

  PetscFunctionBegin;
  if (mode & SCATTER_REVERSE) {
    to     = (VecScatter_MPI_General*)ctx->fromdata;
    from   = (VecScatter_MPI_General*)ctx->todata;
    rwaits = from->rev_requests;
    swaits = to->rev_requests;
  } else {
    to     = (VecScatter_MPI_General*)ctx->todata;
    from   = (VecScatter_MPI_General*)ctx->fromdata;
    rwaits = from->requests;
    swaits = to->requests;
  }
  bs      = to->bs;
  svalues = to->values;
  nrecvs  = from->n;
  nsends  = to->n;
  indices = to->indices;
  sstarts = to->starts;
#if defined(PETSC_HAVE_VECCUDA)
  ierr = PetscObjectTypeCompareAny((PetscObject)xin,&is_cudatype,VECSEQCUDA,VECMPICUDA,VECCUDA,"");CHKERRQ(ierr);
  if (is_cudatype) {
    VecCUDAAllocateCheckHost(xin);
    if (xin->valid_GPU_array == PETSC_OFFLOAD_GPU) {
      if (xin->spptr && ctx->spptr) {
        ierr = VecCUDACopyFromGPUSome_Public(xin,(PetscCUDAIndices)ctx->spptr);CHKERRQ(ierr);
      } else {
        ierr = VecCUDACopyFromGPU(xin);CHKERRQ(ierr);
      }
    }
    xv = *((PetscScalar**)xin->data);
  } else
#endif
  {
    ierr = VecGetArrayRead(xin,(const PetscScalar**)&xv);CHKERRQ(ierr);
  }

  if (xin != yin) {ierr = VecGetArray(yin,&yv);CHKERRQ(ierr);}
  else yv = xv;

  if (!(mode & SCATTER_LOCAL)) {
    /* post receives since they were not previously posted    */
    if (nrecvs) {ierr = MPI_Startall_irecv(from->starts[nrecvs]*bs,nrecvs,rwaits);CHKERRQ(ierr);}

    /* this version packs and sends one at a time */
    for (i=0; i<nsends; i++) {
      if (to->memcpy_plan.optimized[i]) { /* use memcpy instead of indivisual load/store */
        ierr = VecScatterMemcpyPlanExecute_Pack(i,xv,&to->memcpy_plan,svalues+bs*sstarts[i],INSERT_VALUES,bs);CHKERRQ(ierr);
      } else {
        PETSCMAP1(Pack_MPI1)(sstarts[i+1]-sstarts[i],indices + sstarts[i],xv,svalues + bs*sstarts[i],bs);
      }
      ierr = MPI_Start_isend((sstarts[i+1]-sstarts[i])*bs,swaits+i);CHKERRQ(ierr);
    }
  }

  /* take care of local scatters */
  if (to->local.n) {
    if (to->local.memcpy_plan.optimized[0] && addv == INSERT_VALUES) {
      /* do copy when it is not a self-to-self copy */
      if (!(xv == yv && to->local.memcpy_plan.same_copy_starts)) { ierr = VecScatterMemcpyPlanExecute_Scatter(0,xv,&to->local.memcpy_plan,yv,&from->local.memcpy_plan,addv);CHKERRQ(ierr); }
    } else if (to->local.memcpy_plan.optimized[0]) {
      ierr = VecScatterMemcpyPlanExecute_Scatter(0,xv,&to->local.memcpy_plan,yv,&from->local.memcpy_plan,addv);CHKERRQ(ierr);
    } else {
      if (xv == yv && addv == INSERT_VALUES && to->local.nonmatching_computed) {
        /* only copy entries that do not share identical memory locations */
        ierr = PETSCMAP1(Scatter_MPI1)(to->local.n_nonmatching,to->local.slots_nonmatching,xv,from->local.slots_nonmatching,yv,addv,bs);CHKERRQ(ierr);
      } else {
        ierr = PETSCMAP1(Scatter_MPI1)(to->local.n,to->local.vslots,xv,from->local.vslots,yv,addv,bs);CHKERRQ(ierr);
      }
    }
  }
  ierr = VecRestoreArrayRead(xin,(const PetscScalar**)&xv);CHKERRQ(ierr);
  if (xin != yin) {ierr = VecRestoreArray(yin,&yv);CHKERRQ(ierr);}
  PetscFunctionReturn(0);
}

/* --------------------------------------------------------------------------------------*/

PetscErrorCode PETSCMAP1(VecScatterEndMPI1)(VecScatter ctx,Vec xin,Vec yin,InsertMode addv,ScatterMode mode)
{
  VecScatter_MPI_General *to,*from;
  PetscScalar            *rvalues,*yv;
  PetscErrorCode         ierr;
  PetscInt               nrecvs,nsends,*indices,count,*rstarts,bs;
  PetscMPIInt            imdex;
  MPI_Request            *rwaits,*swaits;
  MPI_Status             xrstatus,*sstatus;

  PetscFunctionBegin;
  if (mode & SCATTER_LOCAL) PetscFunctionReturn(0);
  ierr = VecGetArray(yin,&yv);CHKERRQ(ierr);

  to      = (VecScatter_MPI_General*)ctx->todata;
  from    = (VecScatter_MPI_General*)ctx->fromdata;
  rwaits  = from->requests;
  swaits  = to->requests;
  sstatus = to->sstatus;    /* sstatus and rstatus are always stored in to */
  if (mode & SCATTER_REVERSE) {
    to     = (VecScatter_MPI_General*)ctx->fromdata;
    from   = (VecScatter_MPI_General*)ctx->todata;
    rwaits = from->rev_requests;
    swaits = to->rev_requests;
  }
  bs      = from->bs;
  rvalues = from->values;
  nrecvs  = from->n;
  nsends  = to->n;
  indices = from->indices;
  rstarts = from->starts;

  /* unpack one at a time */
  count = nrecvs;
  while (count) {
    ierr = MPI_Waitany(nrecvs,rwaits,&imdex,&xrstatus);CHKERRQ(ierr);
    /* unpack receives into our local space */
    if (from->memcpy_plan.optimized[imdex]) {
      ierr = VecScatterMemcpyPlanExecute_Unpack(imdex,rvalues+bs*rstarts[imdex],yv,&from->memcpy_plan,addv,bs);CHKERRQ(ierr);
    } else {
      ierr = PETSCMAP1(UnPack_MPI1)(rstarts[imdex+1] - rstarts[imdex],rvalues + bs*rstarts[imdex],indices + rstarts[imdex],yv,addv,bs);CHKERRQ(ierr);
    }
    count--;
  }

  /* wait on sends */
  if (nsends) {ierr = MPI_Waitall(nsends,swaits,sstatus);CHKERRQ(ierr);}
  ierr = VecRestoreArray(yin,&yv);CHKERRQ(ierr);
  PetscFunctionReturn(0);
}

#undef PETSCMAP1_a
#undef PETSCMAP1_b
#undef PETSCMAP1
#undef BS
