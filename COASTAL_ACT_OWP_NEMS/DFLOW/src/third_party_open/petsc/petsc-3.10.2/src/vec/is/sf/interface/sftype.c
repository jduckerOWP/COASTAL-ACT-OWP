#include <petsc/private/sfimpl.h>

#if !defined(PETSC_HAVE_MPI_TYPE_GET_ENVELOPE)
#define MPI_Type_get_envelope(datatype,num_ints,num_addrs,num_dtypes,combiner) (*(num_ints)=0,*(num_addrs)=0,*(num_dtypes)=0,*(combiner)=0,1);SETERRQ(PETSC_COMM_SELF,PETSC_ERR_SUP_SYS,"Need an MPI-2 implementation")
#define MPI_Type_get_contents(datatype,num_ints,num_addrs,num_dtypes,ints,addrs,dtypes) (*(ints)=0,*(addrs)=0,*(dtypes)=0,1);SETERRQ(PETSC_COMM_SELF,PETSC_ERR_SUP_SYS,"Need an MPI-2 implementation")
#endif
#if !defined(PETSC_HAVE_MPI_COMBINER_DUP) && !defined(MPI_COMBINER_DUP)  /* We have no way to interpret output of MPI_Type_get_envelope without this. */
#  define MPI_COMBINER_DUP   0
#endif
#if !defined(PETSC_HAVE_MPI_COMBINER_NAMED) && !defined(MPI_COMBINER_NAMED)
#define MPI_COMBINER_NAMED -2
#endif
#if !defined(PETSC_HAVE_MPI_COMBINER_CONTIGUOUS) && !defined(MPI_COMBINER_CONTIGUOUS) && MPI_VERSION < 2
#  define MPI_COMBINER_CONTIGUOUS -1
#endif

static PetscErrorCode MPIPetsc_Type_free(MPI_Datatype *a)
{
  PetscMPIInt    nints,naddrs,ntypes,combiner;
  PetscErrorCode ierr;

  PetscFunctionBegin;
  ierr = MPI_Type_get_envelope(*a,&nints,&naddrs,&ntypes,&combiner);CHKERRQ(ierr);

  if (combiner != MPI_COMBINER_NAMED) {
    ierr = MPI_Type_free(a);CHKERRQ(ierr);
  }

  *a = MPI_DATATYPE_NULL;
  PetscFunctionReturn(0);
}

PetscErrorCode MPIPetsc_Type_unwrap(MPI_Datatype a,MPI_Datatype *atype,PetscBool *flg)
{
  PetscMPIInt    nints,naddrs,ntypes,combiner;
  PetscErrorCode ierr;

  PetscFunctionBegin;
  *flg = PETSC_FALSE;
  *atype = a;
  if (a == MPIU_INT || a == MPIU_REAL || a == MPIU_SCALAR) PetscFunctionReturn(0);
  ierr = MPI_Type_get_envelope(a,&nints,&naddrs,&ntypes,&combiner);CHKERRQ(ierr);
  if (combiner == MPI_COMBINER_DUP) {
    PetscMPIInt  ints[1];
    MPI_Aint     addrs[1];
    MPI_Datatype types[1];
    if (nints != 0 || naddrs != 0 || ntypes != 1) SETERRQ(PETSC_COMM_SELF,PETSC_ERR_LIB,"Unexpected returns from MPI_Type_get_envelope()");
    ierr   = MPI_Type_get_contents(a,0,0,1,ints,addrs,types);CHKERRQ(ierr);
    /* Recursively unwrap dupped types. */
    ierr   = MPIPetsc_Type_unwrap(types[0],atype,flg);CHKERRQ(ierr);
    if (*flg) {
      /* If the recursive call returns a new type, then that means that atype[0] != types[0] and we're on the hook to
       * free types[0].  Note that this case occurs if combiner(types[0]) is MPI_COMBINER_DUP, so we're safe to
       * directly call MPI_Type_free rather than MPIPetsc_Type_free here. */
      ierr = MPI_Type_free(&(types[0]));CHKERRQ(ierr);
    }
    /* In any case, it's up to the caller to free the returned type in this case. */
    *flg = PETSC_TRUE;
  }
  PetscFunctionReturn(0);
}

PetscErrorCode MPIPetsc_Type_compare(MPI_Datatype a,MPI_Datatype b,PetscBool *match)
{
  PetscErrorCode ierr;
  MPI_Datatype   atype,btype;
  PetscMPIInt    aintcount,aaddrcount,atypecount,acombiner;
  PetscMPIInt    bintcount,baddrcount,btypecount,bcombiner;
  PetscBool      freeatype, freebtype;

  PetscFunctionBegin;
  ierr   = MPIPetsc_Type_unwrap(a,&atype,&freeatype);CHKERRQ(ierr);
  ierr   = MPIPetsc_Type_unwrap(b,&btype,&freebtype);CHKERRQ(ierr);
  *match = PETSC_FALSE;
  if (atype == btype) {
    *match = PETSC_TRUE;
    goto free_types;
  }
  ierr = MPI_Type_get_envelope(atype,&aintcount,&aaddrcount,&atypecount,&acombiner);CHKERRQ(ierr);
  ierr = MPI_Type_get_envelope(btype,&bintcount,&baddrcount,&btypecount,&bcombiner);CHKERRQ(ierr);
  if (acombiner == bcombiner && aintcount == bintcount && aaddrcount == baddrcount && atypecount == btypecount && (aintcount > 0 || aaddrcount > 0 || atypecount > 0)) {
    PetscMPIInt  *aints,*bints;
    MPI_Aint     *aaddrs,*baddrs;
    MPI_Datatype *atypes,*btypes;
    PetscInt     i;
    PetscBool    same;
    ierr = PetscMalloc6(aintcount,&aints,bintcount,&bints,aaddrcount,&aaddrs,baddrcount,&baddrs,atypecount,&atypes,btypecount,&btypes);CHKERRQ(ierr);
    ierr = MPI_Type_get_contents(atype,aintcount,aaddrcount,atypecount,aints,aaddrs,atypes);CHKERRQ(ierr);
    ierr = MPI_Type_get_contents(btype,bintcount,baddrcount,btypecount,bints,baddrs,btypes);CHKERRQ(ierr);
    ierr = PetscMemcmp(aints,bints,aintcount*sizeof(aints[0]),&same);CHKERRQ(ierr);
    if (same) {
      ierr = PetscMemcmp(aaddrs,baddrs,aaddrcount*sizeof(aaddrs[0]),&same);CHKERRQ(ierr);
      if (same) {
        /* Check for identity first */
        ierr = PetscMemcmp(atypes,btypes,atypecount*sizeof(atypes[0]),&same);CHKERRQ(ierr);
        if (!same) {
          /* If the atype or btype were not predefined data types, then the types returned from MPI_Type_get_contents
           * will merely be equivalent to the types used in the construction, so we must recursively compare. */
          for (i=0; i<atypecount; i++) {
            ierr = MPIPetsc_Type_compare(atypes[i],btypes[i],&same);CHKERRQ(ierr);
            if (!same) break;
          }
        }
      }
    }
    for (i=0; i<atypecount; i++) {
      ierr = MPIPetsc_Type_free(&(atypes[i]));CHKERRQ(ierr);
      ierr = MPIPetsc_Type_free(&(btypes[i]));CHKERRQ(ierr);
    }
    ierr = PetscFree6(aints,bints,aaddrs,baddrs,atypes,btypes);CHKERRQ(ierr);
    if (same) *match = PETSC_TRUE;
  }
free_types:
  if (freeatype) {
    ierr = MPIPetsc_Type_free(&atype);CHKERRQ(ierr);
  }
  if (freebtype) {
    ierr = MPIPetsc_Type_free(&btype);CHKERRQ(ierr);
  }
  PetscFunctionReturn(0);
}

/* Check whether a was created via MPI_Type_contiguous from b
 *
 */
PetscErrorCode MPIPetsc_Type_compare_contig(MPI_Datatype a,MPI_Datatype b,PetscInt *n)
{
  PetscErrorCode ierr;
  MPI_Datatype   atype,btype;
  PetscMPIInt    aintcount,aaddrcount,atypecount,acombiner;
  PetscBool      freeatype,freebtype;
  PetscFunctionBegin;
  ierr = MPIPetsc_Type_unwrap(a,&atype,&freeatype);CHKERRQ(ierr);
  ierr = MPIPetsc_Type_unwrap(b,&btype,&freebtype);CHKERRQ(ierr);
  *n = PETSC_FALSE;
  if (atype == btype) {
    *n = 1;
    goto free_types;
  }
  ierr = MPI_Type_get_envelope(atype,&aintcount,&aaddrcount,&atypecount,&acombiner);CHKERRQ(ierr);
  if (acombiner == MPI_COMBINER_CONTIGUOUS && aintcount >= 1) {
    PetscMPIInt  *aints;
    MPI_Aint     *aaddrs;
    MPI_Datatype *atypes;
    PetscInt      i;
    PetscBool     same;
    ierr = PetscMalloc3(aintcount,&aints,aaddrcount,&aaddrs,atypecount,&atypes);CHKERRQ(ierr);
    ierr = MPI_Type_get_contents(atype,aintcount,aaddrcount,atypecount,aints,aaddrs,atypes);CHKERRQ(ierr);
    /* Check for identity first. */
    if (atypes[0] == btype) {
      *n = aints[0];
    } else {
      /* atypes[0] merely has to be equivalent to the type used to create atype. */
      ierr = MPIPetsc_Type_compare(atypes[0],btype,&same);CHKERRQ(ierr);
      if (same) *n = aints[0];
    }
    for (i=0; i<atypecount; i++) {
      ierr = MPIPetsc_Type_free(&(atypes[i]));CHKERRQ(ierr);
    }
    ierr = PetscFree3(aints,aaddrs,atypes);CHKERRQ(ierr);
  }
free_types:
  if (freeatype) {
    ierr = MPIPetsc_Type_free(&atype);CHKERRQ(ierr);
  }
  if (freebtype) {
    ierr = MPIPetsc_Type_free(&btype);CHKERRQ(ierr);
  }
  PetscFunctionReturn(0);
}
