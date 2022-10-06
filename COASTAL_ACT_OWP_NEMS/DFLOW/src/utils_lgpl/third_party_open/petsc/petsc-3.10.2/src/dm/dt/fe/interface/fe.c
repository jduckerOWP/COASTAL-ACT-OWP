/* Basis Jet Tabulation

We would like to tabulate the nodal basis functions and derivatives at a set of points, usually quadrature points. We
follow here the derviation in http://www.math.ttu.edu/~kirby/papers/fiat-toms-2004.pdf. The nodal basis $\psi_i$ can
be expressed in terms of a prime basis $\phi_i$ which can be stably evaluated. In PETSc, we will use the Legendre basis
as a prime basis.

  \psi_i = \sum_k \alpha_{ki} \phi_k

Our nodal basis is defined in terms of the dual basis $n_j$

  n_j \cdot \psi_i = \delta_{ji}

and we may act on the first equation to obtain

  n_j \cdot \psi_i = \sum_k \alpha_{ki} n_j \cdot \phi_k
       \delta_{ji} = \sum_k \alpha_{ki} V_{jk}
                 I = V \alpha

so the coefficients of the nodal basis in the prime basis are

   \alpha = V^{-1}

We will define the dual basis vectors $n_j$ using a quadrature rule.

Right now, we will just use the polynomial spaces P^k. I know some elements use the space of symmetric polynomials
(I think Nedelec), but we will neglect this for now. Constraints in the space, e.g. Arnold-Winther elements, can
be implemented exactly as in FIAT using functionals $L_j$.

I will have to count the degrees correctly for the Legendre product when we are on simplices.

We will have three objects:
 - Space, P: this just need point evaluation I think
 - Dual Space, P'+K: This looks like a set of functionals that can act on members of P, each n is defined by a Q
 - FEM: This keeps {P, P', Q}
*/
#include <petsc/private/petscfeimpl.h> /*I "petscfe.h" I*/
#include <petscdmplex.h>

PetscBool FEcite = PETSC_FALSE;
const char FECitation[] = "@article{kirby2004,\n"
                          "  title   = {Algorithm 839: FIAT, a New Paradigm for Computing Finite Element Basis Functions},\n"
                          "  journal = {ACM Transactions on Mathematical Software},\n"
                          "  author  = {Robert C. Kirby},\n"
                          "  volume  = {30},\n"
                          "  number  = {4},\n"
                          "  pages   = {502--516},\n"
                          "  doi     = {10.1145/1039813.1039820},\n"
                          "  year    = {2004}\n}\n";

PetscClassId PETSCFE_CLASSID = 0;

PetscFunctionList PetscFEList              = NULL;
PetscBool         PetscFERegisterAllCalled = PETSC_FALSE;

/*@C
  PetscFERegister - Adds a new PetscFE implementation

  Not Collective

  Input Parameters:
+ name        - The name of a new user-defined creation routine
- create_func - The creation routine itself

  Notes:
  PetscFERegister() may be called multiple times to add several user-defined PetscFEs

  Sample usage:
.vb
    PetscFERegister("my_fe", MyPetscFECreate);
.ve

  Then, your PetscFE type can be chosen with the procedural interface via
.vb
    PetscFECreate(MPI_Comm, PetscFE *);
    PetscFESetType(PetscFE, "my_fe");
.ve
   or at runtime via the option
.vb
    -petscfe_type my_fe
.ve

  Level: advanced

.keywords: PetscFE, register
.seealso: PetscFERegisterAll(), PetscFERegisterDestroy()

@*/
PetscErrorCode PetscFERegister(const char sname[], PetscErrorCode (*function)(PetscFE))
{
  PetscErrorCode ierr;

  PetscFunctionBegin;
  ierr = PetscFunctionListAdd(&PetscFEList, sname, function);CHKERRQ(ierr);
  PetscFunctionReturn(0);
}

/*@C
  PetscFESetType - Builds a particular PetscFE

  Collective on PetscFE

  Input Parameters:
+ fem  - The PetscFE object
- name - The kind of FEM space

  Options Database Key:
. -petscfe_type <type> - Sets the PetscFE type; use -help for a list of available types

  Level: intermediate

.keywords: PetscFE, set, type
.seealso: PetscFEGetType(), PetscFECreate()
@*/
PetscErrorCode PetscFESetType(PetscFE fem, PetscFEType name)
{
  PetscErrorCode (*r)(PetscFE);
  PetscBool      match;
  PetscErrorCode ierr;

  PetscFunctionBegin;
  PetscValidHeaderSpecific(fem, PETSCFE_CLASSID, 1);
  ierr = PetscObjectTypeCompare((PetscObject) fem, name, &match);CHKERRQ(ierr);
  if (match) PetscFunctionReturn(0);

  if (!PetscFERegisterAllCalled) {ierr = PetscFERegisterAll();CHKERRQ(ierr);}
  ierr = PetscFunctionListFind(PetscFEList, name, &r);CHKERRQ(ierr);
  if (!r) SETERRQ1(PetscObjectComm((PetscObject) fem), PETSC_ERR_ARG_UNKNOWN_TYPE, "Unknown PetscFE type: %s", name);

  if (fem->ops->destroy) {
    ierr              = (*fem->ops->destroy)(fem);CHKERRQ(ierr);
    fem->ops->destroy = NULL;
  }
  ierr = (*r)(fem);CHKERRQ(ierr);
  ierr = PetscObjectChangeTypeName((PetscObject) fem, name);CHKERRQ(ierr);
  PetscFunctionReturn(0);
}

/*@C
  PetscFEGetType - Gets the PetscFE type name (as a string) from the object.

  Not Collective

  Input Parameter:
. fem  - The PetscFE

  Output Parameter:
. name - The PetscFE type name

  Level: intermediate

.keywords: PetscFE, get, type, name
.seealso: PetscFESetType(), PetscFECreate()
@*/
PetscErrorCode PetscFEGetType(PetscFE fem, PetscFEType *name)
{
  PetscErrorCode ierr;

  PetscFunctionBegin;
  PetscValidHeaderSpecific(fem, PETSCFE_CLASSID, 1);
  PetscValidPointer(name, 2);
  if (!PetscFERegisterAllCalled) {
    ierr = PetscFERegisterAll();CHKERRQ(ierr);
  }
  *name = ((PetscObject) fem)->type_name;
  PetscFunctionReturn(0);
}

/*@C
  PetscFEView - Views a PetscFE

  Collective on PetscFE

  Input Parameter:
+ fem - the PetscFE object to view
- v   - the viewer

  Level: developer

.seealso PetscFEDestroy()
@*/
PetscErrorCode PetscFEView(PetscFE fem, PetscViewer v)
{
  PetscErrorCode ierr;

  PetscFunctionBegin;
  PetscValidHeaderSpecific(fem, PETSCFE_CLASSID, 1);
  if (!v) {ierr = PetscViewerASCIIGetStdout(PetscObjectComm((PetscObject) fem), &v);CHKERRQ(ierr);}
  if (fem->ops->view) {ierr = (*fem->ops->view)(fem, v);CHKERRQ(ierr);}
  PetscFunctionReturn(0);
}

/*@
  PetscFESetFromOptions - sets parameters in a PetscFE from the options database

  Collective on PetscFE

  Input Parameter:
. fem - the PetscFE object to set options for

  Options Database:
. -petscfe_num_blocks  the number of cell blocks to integrate concurrently
. -petscfe_num_batches the number of cell batches to integrate serially

  Level: developer

.seealso PetscFEView()
@*/
PetscErrorCode PetscFESetFromOptions(PetscFE fem)
{
  const char    *defaultType;
  char           name[256];
  PetscBool      flg;
  PetscErrorCode ierr;

  PetscFunctionBegin;
  PetscValidHeaderSpecific(fem, PETSCFE_CLASSID, 1);
  if (!((PetscObject) fem)->type_name) {
    defaultType = PETSCFEBASIC;
  } else {
    defaultType = ((PetscObject) fem)->type_name;
  }
  if (!PetscFERegisterAllCalled) {ierr = PetscFERegisterAll();CHKERRQ(ierr);}

  ierr = PetscObjectOptionsBegin((PetscObject) fem);CHKERRQ(ierr);
  ierr = PetscOptionsFList("-petscfe_type", "Finite element space", "PetscFESetType", PetscFEList, defaultType, name, 256, &flg);CHKERRQ(ierr);
  if (flg) {
    ierr = PetscFESetType(fem, name);CHKERRQ(ierr);
  } else if (!((PetscObject) fem)->type_name) {
    ierr = PetscFESetType(fem, defaultType);CHKERRQ(ierr);
  }
  ierr = PetscOptionsInt("-petscfe_num_blocks", "The number of cell blocks to integrate concurrently", "PetscSpaceSetTileSizes", fem->numBlocks, &fem->numBlocks, NULL);CHKERRQ(ierr);
  ierr = PetscOptionsInt("-petscfe_num_batches", "The number of cell batches to integrate serially", "PetscSpaceSetTileSizes", fem->numBatches, &fem->numBatches, NULL);CHKERRQ(ierr);
  if (fem->ops->setfromoptions) {
    ierr = (*fem->ops->setfromoptions)(PetscOptionsObject,fem);CHKERRQ(ierr);
  }
  /* process any options handlers added with PetscObjectAddOptionsHandler() */
  ierr = PetscObjectProcessOptionsHandlers(PetscOptionsObject,(PetscObject) fem);CHKERRQ(ierr);
  ierr = PetscOptionsEnd();CHKERRQ(ierr);
  ierr = PetscFEViewFromOptions(fem, NULL, "-petscfe_view");CHKERRQ(ierr);
  PetscFunctionReturn(0);
}

/*@C
  PetscFESetUp - Construct data structures for the PetscFE

  Collective on PetscFE

  Input Parameter:
. fem - the PetscFE object to setup

  Level: developer

.seealso PetscFEView(), PetscFEDestroy()
@*/
PetscErrorCode PetscFESetUp(PetscFE fem)
{
  PetscErrorCode ierr;

  PetscFunctionBegin;
  PetscValidHeaderSpecific(fem, PETSCFE_CLASSID, 1);
  if (fem->setupcalled) PetscFunctionReturn(0);
  fem->setupcalled = PETSC_TRUE;
  if (fem->ops->setup) {ierr = (*fem->ops->setup)(fem);CHKERRQ(ierr);}
  PetscFunctionReturn(0);
}

/*@
  PetscFEDestroy - Destroys a PetscFE object

  Collective on PetscFE

  Input Parameter:
. fem - the PetscFE object to destroy

  Level: developer

.seealso PetscFEView()
@*/
PetscErrorCode PetscFEDestroy(PetscFE *fem)
{
  PetscErrorCode ierr;

  PetscFunctionBegin;
  if (!*fem) PetscFunctionReturn(0);
  PetscValidHeaderSpecific((*fem), PETSCFE_CLASSID, 1);

  if (--((PetscObject)(*fem))->refct > 0) {*fem = 0; PetscFunctionReturn(0);}
  ((PetscObject) (*fem))->refct = 0;

  if ((*fem)->subspaces) {
    PetscInt dim, d;

    ierr = PetscDualSpaceGetDimension((*fem)->dualSpace, &dim);CHKERRQ(ierr);
    for (d = 0; d < dim; ++d) {ierr = PetscFEDestroy(&(*fem)->subspaces[d]);CHKERRQ(ierr);}
  }
  ierr = PetscFree((*fem)->subspaces);CHKERRQ(ierr);
  ierr = PetscFree((*fem)->invV);CHKERRQ(ierr);
  ierr = PetscFERestoreTabulation((*fem), 0, NULL, &(*fem)->B, &(*fem)->D, NULL /*&(*fem)->H*/);CHKERRQ(ierr);
  ierr = PetscFERestoreTabulation((*fem), 0, NULL, &(*fem)->Bf, &(*fem)->Df, NULL /*&(*fem)->Hf*/);CHKERRQ(ierr);
  ierr = PetscFERestoreTabulation((*fem), 0, NULL, &(*fem)->F, NULL, NULL);CHKERRQ(ierr);
  ierr = PetscSpaceDestroy(&(*fem)->basisSpace);CHKERRQ(ierr);
  ierr = PetscDualSpaceDestroy(&(*fem)->dualSpace);CHKERRQ(ierr);
  ierr = PetscQuadratureDestroy(&(*fem)->quadrature);CHKERRQ(ierr);
  ierr = PetscQuadratureDestroy(&(*fem)->faceQuadrature);CHKERRQ(ierr);

  if ((*fem)->ops->destroy) {ierr = (*(*fem)->ops->destroy)(*fem);CHKERRQ(ierr);}
  ierr = PetscHeaderDestroy(fem);CHKERRQ(ierr);
  PetscFunctionReturn(0);
}

/*@
  PetscFECreate - Creates an empty PetscFE object. The type can then be set with PetscFESetType().

  Collective on MPI_Comm

  Input Parameter:
. comm - The communicator for the PetscFE object

  Output Parameter:
. fem - The PetscFE object

  Level: beginner

.seealso: PetscFESetType(), PETSCFEGALERKIN
@*/
PetscErrorCode PetscFECreate(MPI_Comm comm, PetscFE *fem)
{
  PetscFE        f;
  PetscErrorCode ierr;

  PetscFunctionBegin;
  PetscValidPointer(fem, 2);
  ierr = PetscCitationsRegister(FECitation,&FEcite);CHKERRQ(ierr);
  *fem = NULL;
  ierr = PetscFEInitializePackage();CHKERRQ(ierr);

  ierr = PetscHeaderCreate(f, PETSCFE_CLASSID, "PetscFE", "Finite Element", "PetscFE", comm, PetscFEDestroy, PetscFEView);CHKERRQ(ierr);

  f->basisSpace    = NULL;
  f->dualSpace     = NULL;
  f->numComponents = 1;
  f->subspaces     = NULL;
  f->invV          = NULL;
  f->B             = NULL;
  f->D             = NULL;
  f->H             = NULL;
  f->Bf            = NULL;
  f->Df            = NULL;
  f->Hf            = NULL;
  ierr = PetscMemzero(&f->quadrature, sizeof(PetscQuadrature));CHKERRQ(ierr);
  ierr = PetscMemzero(&f->faceQuadrature, sizeof(PetscQuadrature));CHKERRQ(ierr);
  f->blockSize     = 0;
  f->numBlocks     = 1;
  f->batchSize     = 0;
  f->numBatches    = 1;

  *fem = f;
  PetscFunctionReturn(0);
}

/*@
  PetscFEGetSpatialDimension - Returns the spatial dimension of the element

  Not collective

  Input Parameter:
. fem - The PetscFE object

  Output Parameter:
. dim - The spatial dimension

  Level: intermediate

.seealso: PetscFECreate()
@*/
PetscErrorCode PetscFEGetSpatialDimension(PetscFE fem, PetscInt *dim)
{
  DM             dm;
  PetscErrorCode ierr;

  PetscFunctionBegin;
  PetscValidHeaderSpecific(fem, PETSCFE_CLASSID, 1);
  PetscValidPointer(dim, 2);
  ierr = PetscDualSpaceGetDM(fem->dualSpace, &dm);CHKERRQ(ierr);
  ierr = DMGetDimension(dm, dim);CHKERRQ(ierr);
  PetscFunctionReturn(0);
}

/*@
  PetscFESetNumComponents - Sets the number of components in the element

  Not collective

  Input Parameters:
+ fem - The PetscFE object
- comp - The number of field components

  Level: intermediate

.seealso: PetscFECreate()
@*/
PetscErrorCode PetscFESetNumComponents(PetscFE fem, PetscInt comp)
{
  PetscFunctionBegin;
  PetscValidHeaderSpecific(fem, PETSCFE_CLASSID, 1);
  fem->numComponents = comp;
  PetscFunctionReturn(0);
}

/*@
  PetscFEGetNumComponents - Returns the number of components in the element

  Not collective

  Input Parameter:
. fem - The PetscFE object

  Output Parameter:
. comp - The number of field components

  Level: intermediate

.seealso: PetscFECreate()
@*/
PetscErrorCode PetscFEGetNumComponents(PetscFE fem, PetscInt *comp)
{
  PetscFunctionBegin;
  PetscValidHeaderSpecific(fem, PETSCFE_CLASSID, 1);
  PetscValidPointer(comp, 2);
  *comp = fem->numComponents;
  PetscFunctionReturn(0);
}

/*@
  PetscFESetTileSizes - Sets the tile sizes for evaluation

  Not collective

  Input Parameters:
+ fem - The PetscFE object
. blockSize - The number of elements in a block
. numBlocks - The number of blocks in a batch
. batchSize - The number of elements in a batch
- numBatches - The number of batches in a chunk

  Level: intermediate

.seealso: PetscFECreate()
@*/
PetscErrorCode PetscFESetTileSizes(PetscFE fem, PetscInt blockSize, PetscInt numBlocks, PetscInt batchSize, PetscInt numBatches)
{
  PetscFunctionBegin;
  PetscValidHeaderSpecific(fem, PETSCFE_CLASSID, 1);
  fem->blockSize  = blockSize;
  fem->numBlocks  = numBlocks;
  fem->batchSize  = batchSize;
  fem->numBatches = numBatches;
  PetscFunctionReturn(0);
}

/*@
  PetscFEGetTileSizes - Returns the tile sizes for evaluation

  Not collective

  Input Parameter:
. fem - The PetscFE object

  Output Parameters:
+ blockSize - The number of elements in a block
. numBlocks - The number of blocks in a batch
. batchSize - The number of elements in a batch
- numBatches - The number of batches in a chunk

  Level: intermediate

.seealso: PetscFECreate()
@*/
PetscErrorCode PetscFEGetTileSizes(PetscFE fem, PetscInt *blockSize, PetscInt *numBlocks, PetscInt *batchSize, PetscInt *numBatches)
{
  PetscFunctionBegin;
  PetscValidHeaderSpecific(fem, PETSCFE_CLASSID, 1);
  if (blockSize)  PetscValidPointer(blockSize,  2);
  if (numBlocks)  PetscValidPointer(numBlocks,  3);
  if (batchSize)  PetscValidPointer(batchSize,  4);
  if (numBatches) PetscValidPointer(numBatches, 5);
  if (blockSize)  *blockSize  = fem->blockSize;
  if (numBlocks)  *numBlocks  = fem->numBlocks;
  if (batchSize)  *batchSize  = fem->batchSize;
  if (numBatches) *numBatches = fem->numBatches;
  PetscFunctionReturn(0);
}

/*@
  PetscFEGetBasisSpace - Returns the PetscSpace used for approximation of the solution

  Not collective

  Input Parameter:
. fem - The PetscFE object

  Output Parameter:
. sp - The PetscSpace object

  Level: intermediate

.seealso: PetscFECreate()
@*/
PetscErrorCode PetscFEGetBasisSpace(PetscFE fem, PetscSpace *sp)
{
  PetscFunctionBegin;
  PetscValidHeaderSpecific(fem, PETSCFE_CLASSID, 1);
  PetscValidPointer(sp, 2);
  *sp = fem->basisSpace;
  PetscFunctionReturn(0);
}

/*@
  PetscFESetBasisSpace - Sets the PetscSpace used for approximation of the solution

  Not collective

  Input Parameters:
+ fem - The PetscFE object
- sp - The PetscSpace object

  Level: intermediate

.seealso: PetscFECreate()
@*/
PetscErrorCode PetscFESetBasisSpace(PetscFE fem, PetscSpace sp)
{
  PetscErrorCode ierr;

  PetscFunctionBegin;
  PetscValidHeaderSpecific(fem, PETSCFE_CLASSID, 1);
  PetscValidHeaderSpecific(sp, PETSCSPACE_CLASSID, 2);
  ierr = PetscSpaceDestroy(&fem->basisSpace);CHKERRQ(ierr);
  fem->basisSpace = sp;
  ierr = PetscObjectReference((PetscObject) fem->basisSpace);CHKERRQ(ierr);
  PetscFunctionReturn(0);
}

/*@
  PetscFEGetDualSpace - Returns the PetscDualSpace used to define the inner product

  Not collective

  Input Parameter:
. fem - The PetscFE object

  Output Parameter:
. sp - The PetscDualSpace object

  Level: intermediate

.seealso: PetscFECreate()
@*/
PetscErrorCode PetscFEGetDualSpace(PetscFE fem, PetscDualSpace *sp)
{
  PetscFunctionBegin;
  PetscValidHeaderSpecific(fem, PETSCFE_CLASSID, 1);
  PetscValidPointer(sp, 2);
  *sp = fem->dualSpace;
  PetscFunctionReturn(0);
}

/*@
  PetscFESetDualSpace - Sets the PetscDualSpace used to define the inner product

  Not collective

  Input Parameters:
+ fem - The PetscFE object
- sp - The PetscDualSpace object

  Level: intermediate

.seealso: PetscFECreate()
@*/
PetscErrorCode PetscFESetDualSpace(PetscFE fem, PetscDualSpace sp)
{
  PetscErrorCode ierr;

  PetscFunctionBegin;
  PetscValidHeaderSpecific(fem, PETSCFE_CLASSID, 1);
  PetscValidHeaderSpecific(sp, PETSCDUALSPACE_CLASSID, 2);
  ierr = PetscDualSpaceDestroy(&fem->dualSpace);CHKERRQ(ierr);
  fem->dualSpace = sp;
  ierr = PetscObjectReference((PetscObject) fem->dualSpace);CHKERRQ(ierr);
  PetscFunctionReturn(0);
}

/*@
  PetscFEGetQuadrature - Returns the PetscQuadrature used to calculate inner products

  Not collective

  Input Parameter:
. fem - The PetscFE object

  Output Parameter:
. q - The PetscQuadrature object

  Level: intermediate

.seealso: PetscFECreate()
@*/
PetscErrorCode PetscFEGetQuadrature(PetscFE fem, PetscQuadrature *q)
{
  PetscFunctionBegin;
  PetscValidHeaderSpecific(fem, PETSCFE_CLASSID, 1);
  PetscValidPointer(q, 2);
  *q = fem->quadrature;
  PetscFunctionReturn(0);
}

/*@
  PetscFESetQuadrature - Sets the PetscQuadrature used to calculate inner products

  Not collective

  Input Parameters:
+ fem - The PetscFE object
- q - The PetscQuadrature object

  Level: intermediate

.seealso: PetscFECreate()
@*/
PetscErrorCode PetscFESetQuadrature(PetscFE fem, PetscQuadrature q)
{
  PetscInt       Nc, qNc;
  PetscErrorCode ierr;

  PetscFunctionBegin;
  PetscValidHeaderSpecific(fem, PETSCFE_CLASSID, 1);
  ierr = PetscFEGetNumComponents(fem, &Nc);CHKERRQ(ierr);
  ierr = PetscQuadratureGetNumComponents(q, &qNc);CHKERRQ(ierr);
  if ((qNc != 1) && (Nc != qNc)) SETERRQ2(PetscObjectComm((PetscObject) fem), PETSC_ERR_ARG_SIZ, "FE components %D != Quadrature components %D and non-scalar quadrature", Nc, qNc);
  ierr = PetscFERestoreTabulation(fem, 0, NULL, &fem->B, &fem->D, NULL /*&(*fem)->H*/);CHKERRQ(ierr);
  ierr = PetscQuadratureDestroy(&fem->quadrature);CHKERRQ(ierr);
  fem->quadrature = q;
  ierr = PetscObjectReference((PetscObject) q);CHKERRQ(ierr);
  PetscFunctionReturn(0);
}

/*@
  PetscFEGetFaceQuadrature - Returns the PetscQuadrature used to calculate inner products on faces

  Not collective

  Input Parameter:
. fem - The PetscFE object

  Output Parameter:
. q - The PetscQuadrature object

  Level: intermediate

.seealso: PetscFECreate()
@*/
PetscErrorCode PetscFEGetFaceQuadrature(PetscFE fem, PetscQuadrature *q)
{
  PetscFunctionBegin;
  PetscValidHeaderSpecific(fem, PETSCFE_CLASSID, 1);
  PetscValidPointer(q, 2);
  *q = fem->faceQuadrature;
  PetscFunctionReturn(0);
}

/*@
  PetscFESetFaceQuadrature - Sets the PetscQuadrature used to calculate inner products on faces

  Not collective

  Input Parameters:
+ fem - The PetscFE object
- q - The PetscQuadrature object

  Level: intermediate

.seealso: PetscFECreate()
@*/
PetscErrorCode PetscFESetFaceQuadrature(PetscFE fem, PetscQuadrature q)
{
  PetscErrorCode ierr;

  PetscFunctionBegin;
  PetscValidHeaderSpecific(fem, PETSCFE_CLASSID, 1);
  ierr = PetscFERestoreTabulation(fem, 0, NULL, &fem->Bf, &fem->Df, NULL /*&(*fem)->Hf*/);CHKERRQ(ierr);
  ierr = PetscQuadratureDestroy(&fem->faceQuadrature);CHKERRQ(ierr);
  fem->faceQuadrature = q;
  ierr = PetscObjectReference((PetscObject) q);CHKERRQ(ierr);
  PetscFunctionReturn(0);
}

/*@C
  PetscFEGetNumDof - Returns the number of dofs (dual basis vectors) associated to mesh points on the reference cell of a given dimension

  Not collective

  Input Parameter:
. fem - The PetscFE object

  Output Parameter:
. numDof - Array with the number of dofs per dimension

  Level: intermediate

.seealso: PetscFECreate()
@*/
PetscErrorCode PetscFEGetNumDof(PetscFE fem, const PetscInt **numDof)
{
  PetscErrorCode ierr;

  PetscFunctionBegin;
  PetscValidHeaderSpecific(fem, PETSCFE_CLASSID, 1);
  PetscValidPointer(numDof, 2);
  ierr = PetscDualSpaceGetNumDof(fem->dualSpace, numDof);CHKERRQ(ierr);
  PetscFunctionReturn(0);
}

/*@C
  PetscFEGetDefaultTabulation - Returns the tabulation of the basis functions at the quadrature points

  Not collective

  Input Parameter:
. fem - The PetscFE object

  Output Parameters:
+ B - The basis function values at quadrature points
. D - The basis function derivatives at quadrature points
- H - The basis function second derivatives at quadrature points

  Note:
$ B[(p*pdim + i)*Nc + c] is the value at point p for basis function i and component c
$ D[((p*pdim + i)*Nc + c)*dim + d] is the derivative value at point p for basis function i, component c, in direction d
$ H[(((p*pdim + i)*Nc + c)*dim + d)*dim + e] is the value at point p for basis function i, component c, in directions d and e

  Level: intermediate

.seealso: PetscFEGetTabulation(), PetscFERestoreTabulation()
@*/
PetscErrorCode PetscFEGetDefaultTabulation(PetscFE fem, PetscReal **B, PetscReal **D, PetscReal **H)
{
  PetscInt         npoints;
  const PetscReal *points;
  PetscErrorCode   ierr;

  PetscFunctionBegin;
  PetscValidHeaderSpecific(fem, PETSCFE_CLASSID, 1);
  if (B) PetscValidPointer(B, 2);
  if (D) PetscValidPointer(D, 3);
  if (H) PetscValidPointer(H, 4);
  ierr = PetscQuadratureGetData(fem->quadrature, NULL, NULL, &npoints, &points, NULL);CHKERRQ(ierr);
  if (!fem->B) {ierr = PetscFEGetTabulation(fem, npoints, points, &fem->B, &fem->D, NULL/*&fem->H*/);CHKERRQ(ierr);}
  if (B) *B = fem->B;
  if (D) *D = fem->D;
  if (H) *H = fem->H;
  PetscFunctionReturn(0);
}

PetscErrorCode PetscFEGetFaceTabulation(PetscFE fem, PetscReal **Bf, PetscReal **Df, PetscReal **Hf)
{
  PetscErrorCode   ierr;

  PetscFunctionBegin;
  PetscValidHeaderSpecific(fem, PETSCFE_CLASSID, 1);
  if (Bf) PetscValidPointer(Bf, 2);
  if (Df) PetscValidPointer(Df, 3);
  if (Hf) PetscValidPointer(Hf, 4);
  if (!fem->Bf) {
    const PetscReal  xi0[3] = {-1., -1., -1.};
    PetscReal        v0[3], J[9], detJ;
    PetscQuadrature  fq;
    PetscDualSpace   sp;
    DM               dm;
    const PetscInt  *faces;
    PetscInt         dim, numFaces, f, npoints, q;
    const PetscReal *points;
    PetscReal       *facePoints;

    ierr = PetscFEGetDualSpace(fem, &sp);CHKERRQ(ierr);
    ierr = PetscDualSpaceGetDM(sp, &dm);CHKERRQ(ierr);
    ierr = DMGetDimension(dm, &dim);CHKERRQ(ierr);
    ierr = DMPlexGetConeSize(dm, 0, &numFaces);CHKERRQ(ierr);
    ierr = DMPlexGetCone(dm, 0, &faces);CHKERRQ(ierr);
    ierr = PetscFEGetFaceQuadrature(fem, &fq);CHKERRQ(ierr);
    if (fq) {
      ierr = PetscQuadratureGetData(fq, NULL, NULL, &npoints, &points, NULL);CHKERRQ(ierr);
      ierr = PetscMalloc1(numFaces*npoints*dim, &facePoints);CHKERRQ(ierr);
      for (f = 0; f < numFaces; ++f) {
        ierr = DMPlexComputeCellGeometryFEM(dm, faces[f], NULL, v0, J, NULL, &detJ);CHKERRQ(ierr);
        for (q = 0; q < npoints; ++q) CoordinatesRefToReal(dim, dim-1, xi0, v0, J, &points[q*(dim-1)], &facePoints[(f*npoints+q)*dim]);
      }
      ierr = PetscFEGetTabulation(fem, numFaces*npoints, facePoints, &fem->Bf, &fem->Df, NULL/*&fem->Hf*/);CHKERRQ(ierr);
      ierr = PetscFree(facePoints);CHKERRQ(ierr);
    }
  }
  if (Bf) *Bf = fem->Bf;
  if (Df) *Df = fem->Df;
  if (Hf) *Hf = fem->Hf;
  PetscFunctionReturn(0);
}

PetscErrorCode PetscFEGetFaceCentroidTabulation(PetscFE fem, PetscReal **F)
{
  PetscErrorCode   ierr;

  PetscFunctionBegin;
  PetscValidHeaderSpecific(fem, PETSCFE_CLASSID, 1);
  PetscValidPointer(F, 2);
  if (!fem->F) {
    PetscDualSpace  sp;
    DM              dm;
    const PetscInt *cone;
    PetscReal      *centroids;
    PetscInt        dim, numFaces, f;

    ierr = PetscFEGetDualSpace(fem, &sp);CHKERRQ(ierr);
    ierr = PetscDualSpaceGetDM(sp, &dm);CHKERRQ(ierr);
    ierr = DMGetDimension(dm, &dim);CHKERRQ(ierr);
    ierr = DMPlexGetConeSize(dm, 0, &numFaces);CHKERRQ(ierr);
    ierr = DMPlexGetCone(dm, 0, &cone);CHKERRQ(ierr);
    ierr = PetscMalloc1(numFaces*dim, &centroids);CHKERRQ(ierr);
    for (f = 0; f < numFaces; ++f) {ierr = DMPlexComputeCellGeometryFVM(dm, cone[f], NULL, &centroids[f*dim], NULL);CHKERRQ(ierr);}
    ierr = PetscFEGetTabulation(fem, numFaces, centroids, &fem->F, NULL, NULL);CHKERRQ(ierr);
    ierr = PetscFree(centroids);CHKERRQ(ierr);
  }
  *F = fem->F;
  PetscFunctionReturn(0);
}

/*@C
  PetscFEGetTabulation - Tabulates the basis functions, and perhaps derivatives, at the points provided.

  Not collective

  Input Parameters:
+ fem     - The PetscFE object
. npoints - The number of tabulation points
- points  - The tabulation point coordinates

  Output Parameters:
+ B - The basis function values at tabulation points
. D - The basis function derivatives at tabulation points
- H - The basis function second derivatives at tabulation points

  Note:
$ B[(p*pdim + i)*Nc + c] is the value at point p for basis function i and component c
$ D[((p*pdim + i)*Nc + c)*dim + d] is the derivative value at point p for basis function i, component c, in direction d
$ H[(((p*pdim + i)*Nc + c)*dim + d)*dim + e] is the value at point p for basis function i, component c, in directions d and e

  Level: intermediate

.seealso: PetscFERestoreTabulation(), PetscFEGetDefaultTabulation()
@*/
PetscErrorCode PetscFEGetTabulation(PetscFE fem, PetscInt npoints, const PetscReal points[], PetscReal **B, PetscReal **D, PetscReal **H)
{
  DM               dm;
  PetscInt         pdim; /* Dimension of FE space P */
  PetscInt         dim;  /* Spatial dimension */
  PetscInt         comp; /* Field components */
  PetscErrorCode   ierr;

  PetscFunctionBegin;
  if (!npoints) {
    if (B) *B = NULL;
    if (D) *D = NULL;
    if (H) *H = NULL;
    PetscFunctionReturn(0);
  }
  PetscValidHeaderSpecific(fem, PETSCFE_CLASSID, 1);
  PetscValidPointer(points, 3);
  if (B) PetscValidPointer(B, 4);
  if (D) PetscValidPointer(D, 5);
  if (H) PetscValidPointer(H, 6);
  ierr = PetscDualSpaceGetDM(fem->dualSpace, &dm);CHKERRQ(ierr);
  ierr = DMGetDimension(dm, &dim);CHKERRQ(ierr);
  ierr = PetscDualSpaceGetDimension(fem->dualSpace, &pdim);CHKERRQ(ierr);
  ierr = PetscFEGetNumComponents(fem, &comp);CHKERRQ(ierr);
  if (B) {ierr = DMGetWorkArray(dm, npoints*pdim*comp, MPIU_REAL, B);CHKERRQ(ierr);}
  if (!dim) {
    if (D) *D = NULL;
    if (H) *H = NULL;
  } else {
    if (D) {ierr = DMGetWorkArray(dm, npoints*pdim*comp*dim, MPIU_REAL, D);CHKERRQ(ierr);}
    if (H) {ierr = DMGetWorkArray(dm, npoints*pdim*comp*dim*dim, MPIU_REAL, H);CHKERRQ(ierr);}
  }
  ierr = (*fem->ops->gettabulation)(fem, npoints, points, B ? *B : NULL, D ? *D : NULL, H ? *H : NULL);CHKERRQ(ierr);
  PetscFunctionReturn(0);
}

PetscErrorCode PetscFERestoreTabulation(PetscFE fem, PetscInt npoints, const PetscReal points[], PetscReal **B, PetscReal **D, PetscReal **H)
{
  DM             dm;
  PetscErrorCode ierr;

  PetscFunctionBegin;
  PetscValidHeaderSpecific(fem, PETSCFE_CLASSID, 1);
  ierr = PetscDualSpaceGetDM(fem->dualSpace, &dm);CHKERRQ(ierr);
  if (B && *B) {ierr = DMRestoreWorkArray(dm, 0, MPIU_REAL, B);CHKERRQ(ierr);}
  if (D && *D) {ierr = DMRestoreWorkArray(dm, 0, MPIU_REAL, D);CHKERRQ(ierr);}
  if (H && *H) {ierr = DMRestoreWorkArray(dm, 0, MPIU_REAL, H);CHKERRQ(ierr);}
  PetscFunctionReturn(0);
}

PETSC_EXTERN PetscErrorCode PetscFECreatePointTrace(PetscFE fe, PetscInt refPoint, PetscFE *trFE)
{
  PetscSpace     bsp, bsubsp;
  PetscDualSpace dsp, dsubsp;
  PetscInt       dim, depth, numComp, i, j, coneSize, order;
  PetscFEType    type;
  DM             dm;
  DMLabel        label;
  PetscReal      *xi, *v, *J, detJ;
  PetscQuadrature origin, fullQuad, subQuad;
  PetscErrorCode ierr;

  PetscFunctionBegin;
  PetscValidHeaderSpecific(fe,PETSCFE_CLASSID,1);
  PetscValidPointer(trFE,3);
  ierr = PetscFEGetBasisSpace(fe,&bsp);CHKERRQ(ierr);
  ierr = PetscFEGetDualSpace(fe,&dsp);CHKERRQ(ierr);
  ierr = PetscDualSpaceGetDM(dsp,&dm);CHKERRQ(ierr);
  ierr = DMGetDimension(dm,&dim);CHKERRQ(ierr);
  ierr = DMPlexGetDepthLabel(dm,&label);CHKERRQ(ierr);
  ierr = DMLabelGetValue(label,refPoint,&depth);CHKERRQ(ierr);
  ierr = PetscCalloc1(depth,&xi);CHKERRQ(ierr);
  ierr = PetscMalloc1(dim,&v);CHKERRQ(ierr);
  ierr = PetscMalloc1(dim*dim,&J);CHKERRQ(ierr);
  for (i = 0; i < depth; i++) xi[i] = 0.;
  ierr = PetscQuadratureCreate(PETSC_COMM_SELF,&origin);CHKERRQ(ierr);
  ierr = PetscQuadratureSetData(origin,depth,0,1,xi,NULL);CHKERRQ(ierr);
  ierr = DMPlexComputeCellGeometryFEM(dm,refPoint,origin,v,J,NULL,&detJ);CHKERRQ(ierr);
  /* CellGeometryFEM computes the expanded Jacobian, we want the true jacobian */
  for (i = 1; i < dim; i++) {
    for (j = 0; j < depth; j++) {
      J[i * depth + j] = J[i * dim + j];
    }
  }
  ierr = PetscQuadratureDestroy(&origin);CHKERRQ(ierr);
  ierr = PetscDualSpaceGetPointSubspace(dsp,refPoint,&dsubsp);CHKERRQ(ierr);
  ierr = PetscSpaceCreateSubspace(bsp,dsubsp,v,J,NULL,NULL,PETSC_OWN_POINTER,&bsubsp);CHKERRQ(ierr);
  ierr = PetscSpaceSetUp(bsubsp);CHKERRQ(ierr);
  ierr = PetscFECreate(PetscObjectComm((PetscObject)fe),trFE);CHKERRQ(ierr);
  ierr = PetscFEGetType(fe,&type);CHKERRQ(ierr);
  ierr = PetscFESetType(*trFE,type);CHKERRQ(ierr);
  ierr = PetscFEGetNumComponents(fe,&numComp);CHKERRQ(ierr);
  ierr = PetscFESetNumComponents(*trFE,numComp);CHKERRQ(ierr);
  ierr = PetscFESetBasisSpace(*trFE,bsubsp);CHKERRQ(ierr);
  ierr = PetscFESetDualSpace(*trFE,dsubsp);CHKERRQ(ierr);
  ierr = PetscFEGetQuadrature(fe,&fullQuad);CHKERRQ(ierr);
  ierr = PetscQuadratureGetOrder(fullQuad,&order);CHKERRQ(ierr);
  ierr = DMPlexGetConeSize(dm,refPoint,&coneSize);CHKERRQ(ierr);
  if (coneSize == 2 * depth) {
    ierr = PetscDTGaussTensorQuadrature(depth,1,(order + 1)/2,-1.,1.,&subQuad);CHKERRQ(ierr);
  } else {
    ierr = PetscDTGaussJacobiQuadrature(depth,1,(order + 1)/2,-1.,1.,&subQuad);CHKERRQ(ierr);
  }
  ierr = PetscFESetQuadrature(*trFE,subQuad);CHKERRQ(ierr);
  ierr = PetscFESetUp(*trFE);CHKERRQ(ierr);
  ierr = PetscQuadratureDestroy(&subQuad);CHKERRQ(ierr);
  ierr = PetscSpaceDestroy(&bsubsp);CHKERRQ(ierr);
  PetscFunctionReturn(0);
}

PetscErrorCode PetscFECreateHeightTrace(PetscFE fe, PetscInt height, PetscFE *trFE)
{
  PetscInt       hStart, hEnd;
  PetscDualSpace dsp;
  DM             dm;
  PetscErrorCode ierr;

  PetscFunctionBegin;
  PetscValidHeaderSpecific(fe,PETSCFE_CLASSID,1);
  PetscValidPointer(trFE,3);
  *trFE = NULL;
  ierr = PetscFEGetDualSpace(fe,&dsp);CHKERRQ(ierr);
  ierr = PetscDualSpaceGetDM(dsp,&dm);CHKERRQ(ierr);
  ierr = DMPlexGetHeightStratum(dm,height,&hStart,&hEnd);CHKERRQ(ierr);
  if (hEnd <= hStart) PetscFunctionReturn(0);
  ierr = PetscFECreatePointTrace(fe,hStart,trFE);CHKERRQ(ierr);
  PetscFunctionReturn(0);
}


/*@
  PetscFEGetDimension - Get the dimension of the finite element space on a cell

  Not collective

  Input Parameter:
. fe - The PetscFE

  Output Parameter:
. dim - The dimension

  Level: intermediate

.seealso: PetscFECreate(), PetscSpaceGetDimension(), PetscDualSpaceGetDimension()
@*/
PetscErrorCode PetscFEGetDimension(PetscFE fem, PetscInt *dim)
{
  PetscErrorCode ierr;

  PetscFunctionBegin;
  PetscValidHeaderSpecific(fem, PETSCFE_CLASSID, 1);
  PetscValidPointer(dim, 2);
  if (fem->ops->getdimension) {ierr = (*fem->ops->getdimension)(fem, dim);CHKERRQ(ierr);}
  PetscFunctionReturn(0);
}

/*
Purpose: Compute element vector for chunk of elements

Input:
  Sizes:
     Ne:  number of elements
     Nf:  number of fields
     PetscFE
       dim: spatial dimension
       Nb:  number of basis functions
       Nc:  number of field components
       PetscQuadrature
         Nq:  number of quadrature points

  Geometry:
     PetscFEGeom[Ne] possibly *Nq
       PetscReal v0s[dim]
       PetscReal n[dim]
       PetscReal jacobians[dim*dim]
       PetscReal jacobianInverses[dim*dim]
       PetscReal jacobianDeterminants
  FEM:
     PetscFE
       PetscQuadrature
         PetscReal   quadPoints[Nq*dim]
         PetscReal   quadWeights[Nq]
       PetscReal   basis[Nq*Nb*Nc]
       PetscReal   basisDer[Nq*Nb*Nc*dim]
     PetscScalar coefficients[Ne*Nb*Nc]
     PetscScalar elemVec[Ne*Nb*Nc]

  Problem:
     PetscInt f: the active field
     f0, f1

  Work Space:
     PetscFE
       PetscScalar f0[Nq*dim];
       PetscScalar f1[Nq*dim*dim];
       PetscScalar u[Nc];
       PetscScalar gradU[Nc*dim];
       PetscReal   x[dim];
       PetscScalar realSpaceDer[dim];

Purpose: Compute element vector for N_cb batches of elements

Input:
  Sizes:
     N_cb: Number of serial cell batches

  Geometry:
     PetscReal v0s[Ne*dim]
     PetscReal jacobians[Ne*dim*dim]        possibly *Nq
     PetscReal jacobianInverses[Ne*dim*dim] possibly *Nq
     PetscReal jacobianDeterminants[Ne]     possibly *Nq
  FEM:
     static PetscReal   quadPoints[Nq*dim]
     static PetscReal   quadWeights[Nq]
     static PetscReal   basis[Nq*Nb*Nc]
     static PetscReal   basisDer[Nq*Nb*Nc*dim]
     PetscScalar coefficients[Ne*Nb*Nc]
     PetscScalar elemVec[Ne*Nb*Nc]

ex62.c:
  PetscErrorCode PetscFEIntegrateResidualBatch(PetscInt Ne, PetscInt numFields, PetscInt field, PetscQuadrature quad[], const PetscScalar coefficients[],
                                               const PetscReal v0s[], const PetscReal jacobians[], const PetscReal jacobianInverses[], const PetscReal jacobianDeterminants[],
                                               void (*f0_func)(const PetscScalar u[], const PetscScalar gradU[], const PetscReal x[], PetscScalar f0[]),
                                               void (*f1_func)(const PetscScalar u[], const PetscScalar gradU[], const PetscReal x[], PetscScalar f1[]), PetscScalar elemVec[])

ex52.c:
  PetscErrorCode IntegrateLaplacianBatchCPU(PetscInt Ne, PetscInt Nb, const PetscScalar coefficients[], const PetscReal jacobianInverses[], const PetscReal jacobianDeterminants[], PetscInt Nq, const PetscReal quadPoints[], const PetscReal quadWeights[], const PetscReal basisTabulation[], const PetscReal basisDerTabulation[], PetscScalar elemVec[], AppCtx *user)
  PetscErrorCode IntegrateElasticityBatchCPU(PetscInt Ne, PetscInt Nb, PetscInt Ncomp, const PetscScalar coefficients[], const PetscReal jacobianInverses[], const PetscReal jacobianDeterminants[], PetscInt Nq, const PetscReal quadPoints[], const PetscReal quadWeights[], const PetscReal basisTabulation[], const PetscReal basisDerTabulation[], PetscScalar elemVec[], AppCtx *user)

ex52_integrateElement.cu
__global__ void integrateElementQuadrature(int N_cb, realType *coefficients, realType *jacobianInverses, realType *jacobianDeterminants, realType *elemVec)

PETSC_EXTERN PetscErrorCode IntegrateElementBatchGPU(PetscInt spatial_dim, PetscInt Ne, PetscInt Ncb, PetscInt Nbc, PetscInt Nbl, const PetscScalar coefficients[],
                                                     const PetscReal jacobianInverses[], const PetscReal jacobianDeterminants[], PetscScalar elemVec[],
                                                     PetscLogEvent event, PetscInt debug, PetscInt pde_op)

ex52_integrateElementOpenCL.c:
PETSC_EXTERN PetscErrorCode IntegrateElementBatchGPU(PetscInt spatial_dim, PetscInt Ne, PetscInt Ncb, PetscInt Nbc, PetscInt N_bl, const PetscScalar coefficients[],
                                                     const PetscReal jacobianInverses[], const PetscReal jacobianDeterminants[], PetscScalar elemVec[],
                                                     PetscLogEvent event, PetscInt debug, PetscInt pde_op)

__kernel void integrateElementQuadrature(int N_cb, __global float *coefficients, __global float *jacobianInverses, __global float *jacobianDeterminants, __global float *elemVec)
*/

/*@C
  PetscFEIntegrate - Produce the integral for the given field for a chunk of elements by quadrature integration

  Not collective

  Input Parameters:
+ fem          - The PetscFE object for the field being integrated
. prob         - The PetscDS specifying the discretizations and continuum functions
. field        - The field being integrated
. Ne           - The number of elements in the chunk
. cgeom        - The cell geometry for each cell in the chunk
. coefficients - The array of FEM basis coefficients for the elements
. probAux      - The PetscDS specifying the auxiliary discretizations
- coefficientsAux - The array of FEM auxiliary basis coefficients for the elements

  Output Parameter
. integral     - the integral for this field

  Level: developer

.seealso: PetscFEIntegrateResidual()
@*/
PetscErrorCode PetscFEIntegrate(PetscFE fem, PetscDS prob, PetscInt field, PetscInt Ne, PetscFEGeom *cgeom,
                                const PetscScalar coefficients[], PetscDS probAux, const PetscScalar coefficientsAux[], PetscScalar integral[])
{
  PetscErrorCode ierr;

  PetscFunctionBegin;
  PetscValidHeaderSpecific(fem, PETSCFE_CLASSID, 1);
  PetscValidHeaderSpecific(prob, PETSCDS_CLASSID, 2);
  if (fem->ops->integrate) {ierr = (*fem->ops->integrate)(fem, prob, field, Ne, cgeom, coefficients, probAux, coefficientsAux, integral);CHKERRQ(ierr);}
  PetscFunctionReturn(0);
}

/*@C
  PetscFEIntegrateBd - Produce the integral for the given field for a chunk of elements by quadrature integration

  Not collective

  Input Parameters:
+ fem          - The PetscFE object for the field being integrated
. prob         - The PetscDS specifying the discretizations and continuum functions
. field        - The field being integrated
. obj_func     - The function to be integrated
. Ne           - The number of elements in the chunk
. fgeom        - The face geometry for each face in the chunk
. coefficients - The array of FEM basis coefficients for the elements
. probAux      - The PetscDS specifying the auxiliary discretizations
- coefficientsAux - The array of FEM auxiliary basis coefficients for the elements

  Output Parameter
. integral     - the integral for this field

  Level: developer

.seealso: PetscFEIntegrateResidual()
@*/
PetscErrorCode PetscFEIntegrateBd(PetscFE fem, PetscDS prob, PetscInt field,
                                  void (*obj_func)(PetscInt, PetscInt, PetscInt,
                                                   const PetscInt[], const PetscInt[], const PetscScalar[], const PetscScalar[], const PetscScalar[],
                                                   const PetscInt[], const PetscInt[], const PetscScalar[], const PetscScalar[], const PetscScalar[],
                                                   PetscReal, const PetscReal[], const PetscReal[], PetscInt, const PetscScalar[], PetscScalar[]),
                                  PetscInt Ne, PetscFEGeom *geom, const PetscScalar coefficients[], PetscDS probAux, const PetscScalar coefficientsAux[], PetscScalar integral[])
{
  PetscErrorCode ierr;

  PetscFunctionBegin;
  PetscValidHeaderSpecific(fem, PETSCFE_CLASSID, 1);
  PetscValidHeaderSpecific(prob, PETSCDS_CLASSID, 2);
  if (fem->ops->integratebd) {ierr = (*fem->ops->integratebd)(fem, prob, field, obj_func, Ne, geom, coefficients, probAux, coefficientsAux, integral);CHKERRQ(ierr);}
  PetscFunctionReturn(0);
}

/*@C
  PetscFEIntegrateResidual - Produce the element residual vector for a chunk of elements by quadrature integration

  Not collective

  Input Parameters:
+ fem          - The PetscFE object for the field being integrated
. prob         - The PetscDS specifying the discretizations and continuum functions
. field        - The field being integrated
. Ne           - The number of elements in the chunk
. cgeom        - The cell geometry for each cell in the chunk
. coefficients - The array of FEM basis coefficients for the elements
. coefficients_t - The array of FEM basis time derivative coefficients for the elements
. probAux      - The PetscDS specifying the auxiliary discretizations
. coefficientsAux - The array of FEM auxiliary basis coefficients for the elements
- t            - The time

  Output Parameter
. elemVec      - the element residual vectors from each element

  Note:
$ Loop over batch of elements (e):
$   Loop over quadrature points (q):
$     Make u_q and gradU_q (loops over fields,Nb,Ncomp) and x_q
$     Call f_0 and f_1
$   Loop over element vector entries (f,fc --> i):
$     elemVec[i] += \psi^{fc}_f(q) f0_{fc}(u, \nabla u) + \nabla\psi^{fc}_f(q) \cdot f1_{fc,df}(u, \nabla u)

  Level: developer

.seealso: PetscFEIntegrateResidual()
@*/
PetscErrorCode PetscFEIntegrateResidual(PetscFE fem, PetscDS prob, PetscInt field, PetscInt Ne, PetscFEGeom *cgeom,
                                        const PetscScalar coefficients[], const PetscScalar coefficients_t[], PetscDS probAux, const PetscScalar coefficientsAux[], PetscReal t, PetscScalar elemVec[])
{
  PetscErrorCode ierr;

  PetscFunctionBegin;
  PetscValidHeaderSpecific(fem, PETSCFE_CLASSID, 1);
  PetscValidHeaderSpecific(prob, PETSCDS_CLASSID, 2);
  if (fem->ops->integrateresidual) {ierr = (*fem->ops->integrateresidual)(fem, prob, field, Ne, cgeom, coefficients, coefficients_t, probAux, coefficientsAux, t, elemVec);CHKERRQ(ierr);}
  PetscFunctionReturn(0);
}

/*@C
  PetscFEIntegrateBdResidual - Produce the element residual vector for a chunk of elements by quadrature integration over a boundary

  Not collective

  Input Parameters:
+ fem          - The PetscFE object for the field being integrated
. prob         - The PetscDS specifying the discretizations and continuum functions
. field        - The field being integrated
. Ne           - The number of elements in the chunk
. fgeom        - The face geometry for each cell in the chunk
. coefficients - The array of FEM basis coefficients for the elements
. coefficients_t - The array of FEM basis time derivative coefficients for the elements
. probAux      - The PetscDS specifying the auxiliary discretizations
. coefficientsAux - The array of FEM auxiliary basis coefficients for the elements
- t            - The time

  Output Parameter
. elemVec      - the element residual vectors from each element

  Level: developer

.seealso: PetscFEIntegrateResidual()
@*/
PetscErrorCode PetscFEIntegrateBdResidual(PetscFE fem, PetscDS prob, PetscInt field, PetscInt Ne, PetscFEGeom *fgeom,
                                          const PetscScalar coefficients[], const PetscScalar coefficients_t[], PetscDS probAux, const PetscScalar coefficientsAux[], PetscReal t, PetscScalar elemVec[])
{
  PetscErrorCode ierr;

  PetscFunctionBegin;
  PetscValidHeaderSpecific(fem, PETSCFE_CLASSID, 1);
  if (fem->ops->integratebdresidual) {ierr = (*fem->ops->integratebdresidual)(fem, prob, field, Ne, fgeom, coefficients, coefficients_t, probAux, coefficientsAux, t, elemVec);CHKERRQ(ierr);}
  PetscFunctionReturn(0);
}

/*@C
  PetscFEIntegrateJacobian - Produce the element Jacobian for a chunk of elements by quadrature integration

  Not collective

  Input Parameters:
+ fem          - The PetscFE object for the field being integrated
. prob         - The PetscDS specifying the discretizations and continuum functions
. jtype        - The type of matrix pointwise functions that should be used
. fieldI       - The test field being integrated
. fieldJ       - The basis field being integrated
. Ne           - The number of elements in the chunk
. cgeom        - The cell geometry for each cell in the chunk
. coefficients - The array of FEM basis coefficients for the elements for the Jacobian evaluation point
. coefficients_t - The array of FEM basis time derivative coefficients for the elements
. probAux      - The PetscDS specifying the auxiliary discretizations
. coefficientsAux - The array of FEM auxiliary basis coefficients for the elements
. t            - The time
- u_tShift     - A multiplier for the dF/du_t term (as opposed to the dF/du term)

  Output Parameter
. elemMat      - the element matrices for the Jacobian from each element

  Note:
$ Loop over batch of elements (e):
$   Loop over element matrix entries (f,fc,g,gc --> i,j):
$     Loop over quadrature points (q):
$       Make u_q and gradU_q (loops over fields,Nb,Ncomp)
$         elemMat[i,j] += \psi^{fc}_f(q) g0_{fc,gc}(u, \nabla u) \phi^{gc}_g(q)
$                      + \psi^{fc}_f(q) \cdot g1_{fc,gc,dg}(u, \nabla u) \nabla\phi^{gc}_g(q)
$                      + \nabla\psi^{fc}_f(q) \cdot g2_{fc,gc,df}(u, \nabla u) \phi^{gc}_g(q)
$                      + \nabla\psi^{fc}_f(q) \cdot g3_{fc,gc,df,dg}(u, \nabla u) \nabla\phi^{gc}_g(q)
  Level: developer

.seealso: PetscFEIntegrateResidual()
@*/
PetscErrorCode PetscFEIntegrateJacobian(PetscFE fem, PetscDS prob, PetscFEJacobianType jtype, PetscInt fieldI, PetscInt fieldJ, PetscInt Ne, PetscFEGeom *cgeom,
                                        const PetscScalar coefficients[], const PetscScalar coefficients_t[], PetscDS probAux, const PetscScalar coefficientsAux[], PetscReal t, PetscReal u_tshift, PetscScalar elemMat[])
{
  PetscErrorCode ierr;

  PetscFunctionBegin;
  PetscValidHeaderSpecific(fem, PETSCFE_CLASSID, 1);
  if (fem->ops->integratejacobian) {ierr = (*fem->ops->integratejacobian)(fem, prob, jtype, fieldI, fieldJ, Ne, cgeom, coefficients, coefficients_t, probAux, coefficientsAux, t, u_tshift, elemMat);CHKERRQ(ierr);}
  PetscFunctionReturn(0);
}

/*@C
  PetscFEIntegrateBdJacobian - Produce the boundary element Jacobian for a chunk of elements by quadrature integration

  Not collective

  Input Parameters:
+ fem          = The PetscFE object for the field being integrated
. prob         - The PetscDS specifying the discretizations and continuum functions
. fieldI       - The test field being integrated
. fieldJ       - The basis field being integrated
. Ne           - The number of elements in the chunk
. fgeom        - The face geometry for each cell in the chunk
. coefficients - The array of FEM basis coefficients for the elements for the Jacobian evaluation point
. coefficients_t - The array of FEM basis time derivative coefficients for the elements
. probAux      - The PetscDS specifying the auxiliary discretizations
. coefficientsAux - The array of FEM auxiliary basis coefficients for the elements
. t            - The time
- u_tShift     - A multiplier for the dF/du_t term (as opposed to the dF/du term)

  Output Parameter
. elemMat              - the element matrices for the Jacobian from each element

  Note:
$ Loop over batch of elements (e):
$   Loop over element matrix entries (f,fc,g,gc --> i,j):
$     Loop over quadrature points (q):
$       Make u_q and gradU_q (loops over fields,Nb,Ncomp)
$         elemMat[i,j] += \psi^{fc}_f(q) g0_{fc,gc}(u, \nabla u) \phi^{gc}_g(q)
$                      + \psi^{fc}_f(q) \cdot g1_{fc,gc,dg}(u, \nabla u) \nabla\phi^{gc}_g(q)
$                      + \nabla\psi^{fc}_f(q) \cdot g2_{fc,gc,df}(u, \nabla u) \phi^{gc}_g(q)
$                      + \nabla\psi^{fc}_f(q) \cdot g3_{fc,gc,df,dg}(u, \nabla u) \nabla\phi^{gc}_g(q)
  Level: developer

.seealso: PetscFEIntegrateJacobian(), PetscFEIntegrateResidual()
@*/
PetscErrorCode PetscFEIntegrateBdJacobian(PetscFE fem, PetscDS prob, PetscInt fieldI, PetscInt fieldJ, PetscInt Ne, PetscFEGeom *fgeom,
                                          const PetscScalar coefficients[], const PetscScalar coefficients_t[], PetscDS probAux, const PetscScalar coefficientsAux[], PetscReal t, PetscReal u_tshift, PetscScalar elemMat[])
{
  PetscErrorCode ierr;

  PetscFunctionBegin;
  PetscValidHeaderSpecific(fem, PETSCFE_CLASSID, 1);
  if (fem->ops->integratebdjacobian) {ierr = (*fem->ops->integratebdjacobian)(fem, prob, fieldI, fieldJ, Ne, fgeom, coefficients, coefficients_t, probAux, coefficientsAux, t, u_tshift, elemMat);CHKERRQ(ierr);}
  PetscFunctionReturn(0);
}

PetscErrorCode PetscFEGetHeightSubspace(PetscFE fe, PetscInt height, PetscFE *subfe)
{
  PetscSpace      P, subP;
  PetscDualSpace  Q, subQ;
  PetscQuadrature subq;
  PetscFEType     fetype;
  PetscInt        dim, Nc;
  PetscErrorCode  ierr;

  PetscFunctionBegin;
  PetscValidHeaderSpecific(fe, PETSCFE_CLASSID, 1);
  PetscValidPointer(subfe, 3);
  if (height == 0) {
    *subfe = fe;
    PetscFunctionReturn(0);
  }
  ierr = PetscFEGetBasisSpace(fe, &P);CHKERRQ(ierr);
  ierr = PetscFEGetDualSpace(fe, &Q);CHKERRQ(ierr);
  ierr = PetscFEGetNumComponents(fe, &Nc);CHKERRQ(ierr);
  ierr = PetscFEGetFaceQuadrature(fe, &subq);CHKERRQ(ierr);
  ierr = PetscDualSpaceGetDimension(Q, &dim);CHKERRQ(ierr);
  if (height > dim || height < 0) {SETERRQ2(PETSC_COMM_SELF, PETSC_ERR_ARG_OUTOFRANGE, "Asked for space at height %D for dimension %D space", height, dim);}
  if (!fe->subspaces) {ierr = PetscCalloc1(dim, &fe->subspaces);CHKERRQ(ierr);}
  if (height <= dim) {
    if (!fe->subspaces[height-1]) {
      PetscFE sub;

      ierr = PetscSpaceGetHeightSubspace(P, height, &subP);CHKERRQ(ierr);
      ierr = PetscDualSpaceGetHeightSubspace(Q, height, &subQ);CHKERRQ(ierr);
      ierr = PetscFECreate(PetscObjectComm((PetscObject) fe), &sub);CHKERRQ(ierr);
      ierr = PetscFEGetType(fe, &fetype);CHKERRQ(ierr);
      ierr = PetscFESetType(sub, fetype);CHKERRQ(ierr);
      ierr = PetscFESetBasisSpace(sub, subP);CHKERRQ(ierr);
      ierr = PetscFESetDualSpace(sub, subQ);CHKERRQ(ierr);
      ierr = PetscFESetNumComponents(sub, Nc);CHKERRQ(ierr);
      ierr = PetscFESetUp(sub);CHKERRQ(ierr);
      ierr = PetscFESetQuadrature(sub, subq);CHKERRQ(ierr);
      fe->subspaces[height-1] = sub;
    }
    *subfe = fe->subspaces[height-1];
  } else {
    *subfe = NULL;
  }
  PetscFunctionReturn(0);
}

/*@
  PetscFERefine - Create a "refined" PetscFE object that refines the reference cell into smaller copies. This is typically used
  to precondition a higher order method with a lower order method on a refined mesh having the same number of dofs (but more
  sparsity). It is also used to create an interpolation between regularly refined meshes.

  Collective on PetscFE

  Input Parameter:
. fe - The initial PetscFE

  Output Parameter:
. feRef - The refined PetscFE

  Level: developer

.seealso: PetscFEType, PetscFECreate(), PetscFESetType()
@*/
PetscErrorCode PetscFERefine(PetscFE fe, PetscFE *feRef)
{
  PetscSpace       P, Pref;
  PetscDualSpace   Q, Qref;
  DM               K, Kref;
  PetscQuadrature  q, qref;
  const PetscReal *v0, *jac;
  PetscInt         numComp, numSubelements;
  PetscErrorCode   ierr;

  PetscFunctionBegin;
  ierr = PetscFEGetBasisSpace(fe, &P);CHKERRQ(ierr);
  ierr = PetscFEGetDualSpace(fe, &Q);CHKERRQ(ierr);
  ierr = PetscFEGetQuadrature(fe, &q);CHKERRQ(ierr);
  ierr = PetscDualSpaceGetDM(Q, &K);CHKERRQ(ierr);
  /* Create space */
  ierr = PetscObjectReference((PetscObject) P);CHKERRQ(ierr);
  Pref = P;
  /* Create dual space */
  ierr = PetscDualSpaceDuplicate(Q, &Qref);CHKERRQ(ierr);
  ierr = DMRefine(K, PetscObjectComm((PetscObject) fe), &Kref);CHKERRQ(ierr);
  ierr = PetscDualSpaceSetDM(Qref, Kref);CHKERRQ(ierr);
  ierr = DMDestroy(&Kref);CHKERRQ(ierr);
  ierr = PetscDualSpaceSetUp(Qref);CHKERRQ(ierr);
  /* Create element */
  ierr = PetscFECreate(PetscObjectComm((PetscObject) fe), feRef);CHKERRQ(ierr);
  ierr = PetscFESetType(*feRef, PETSCFECOMPOSITE);CHKERRQ(ierr);
  ierr = PetscFESetBasisSpace(*feRef, Pref);CHKERRQ(ierr);
  ierr = PetscFESetDualSpace(*feRef, Qref);CHKERRQ(ierr);
  ierr = PetscFEGetNumComponents(fe,    &numComp);CHKERRQ(ierr);
  ierr = PetscFESetNumComponents(*feRef, numComp);CHKERRQ(ierr);
  ierr = PetscFESetUp(*feRef);CHKERRQ(ierr);
  ierr = PetscSpaceDestroy(&Pref);CHKERRQ(ierr);
  ierr = PetscDualSpaceDestroy(&Qref);CHKERRQ(ierr);
  /* Create quadrature */
  ierr = PetscFECompositeGetMapping(*feRef, &numSubelements, &v0, &jac, NULL);CHKERRQ(ierr);
  ierr = PetscQuadratureExpandComposite(q, numSubelements, v0, jac, &qref);CHKERRQ(ierr);
  ierr = PetscFESetQuadrature(*feRef, qref);CHKERRQ(ierr);
  ierr = PetscQuadratureDestroy(&qref);CHKERRQ(ierr);
  PetscFunctionReturn(0);
}

/*@C
  PetscFECreateDefault - Create a PetscFE for basic FEM computation

  Collective on DM

  Input Parameters:
+ comm      - The MPI comm
. dim       - The spatial dimension
. Nc        - The number of components
. isSimplex - Flag for simplex reference cell, otherwise its a tensor product
. prefix    - The options prefix, or NULL
- qorder    - The quadrature order

  Output Parameter:
. fem - The PetscFE object

  Level: beginner

.keywords: PetscFE, finite element
.seealso: PetscFECreate(), PetscSpaceCreate(), PetscDualSpaceCreate()
@*/
PetscErrorCode PetscFECreateDefault(MPI_Comm comm, PetscInt dim, PetscInt Nc, PetscBool isSimplex, const char prefix[], PetscInt qorder, PetscFE *fem)
{
  PetscQuadrature q, fq;
  DM              K;
  PetscSpace      P;
  PetscDualSpace  Q;
  PetscInt        order, quadPointsPerEdge;
  PetscBool       tensor = isSimplex ? PETSC_FALSE : PETSC_TRUE;
  PetscErrorCode  ierr;

  PetscFunctionBegin;
  /* Create space */
  ierr = PetscSpaceCreate(comm, &P);CHKERRQ(ierr);
  ierr = PetscObjectSetOptionsPrefix((PetscObject) P, prefix);CHKERRQ(ierr);
  ierr = PetscSpacePolynomialSetTensor(P, tensor);CHKERRQ(ierr);
  ierr = PetscSpaceSetNumComponents(P, Nc);CHKERRQ(ierr);
  ierr = PetscSpaceSetNumVariables(P, dim);CHKERRQ(ierr);
  ierr = PetscSpaceSetFromOptions(P);CHKERRQ(ierr);
  ierr = PetscSpaceSetUp(P);CHKERRQ(ierr);
  ierr = PetscSpaceGetDegree(P, &order, NULL);CHKERRQ(ierr);
  ierr = PetscSpacePolynomialGetTensor(P, &tensor);CHKERRQ(ierr);
  /* Create dual space */
  ierr = PetscDualSpaceCreate(comm, &Q);CHKERRQ(ierr);
  ierr = PetscDualSpaceSetType(Q,PETSCDUALSPACELAGRANGE);CHKERRQ(ierr);
  ierr = PetscObjectSetOptionsPrefix((PetscObject) Q, prefix);CHKERRQ(ierr);
  ierr = PetscDualSpaceCreateReferenceCell(Q, dim, isSimplex, &K);CHKERRQ(ierr);
  ierr = PetscDualSpaceSetDM(Q, K);CHKERRQ(ierr);
  ierr = DMDestroy(&K);CHKERRQ(ierr);
  ierr = PetscDualSpaceSetNumComponents(Q, Nc);CHKERRQ(ierr);
  ierr = PetscDualSpaceSetOrder(Q, order);CHKERRQ(ierr);
  ierr = PetscDualSpaceLagrangeSetTensor(Q, tensor);CHKERRQ(ierr);
  ierr = PetscDualSpaceSetFromOptions(Q);CHKERRQ(ierr);
  ierr = PetscDualSpaceSetUp(Q);CHKERRQ(ierr);
  /* Create element */
  ierr = PetscFECreate(comm, fem);CHKERRQ(ierr);
  ierr = PetscObjectSetOptionsPrefix((PetscObject) *fem, prefix);CHKERRQ(ierr);
  ierr = PetscFESetFromOptions(*fem);CHKERRQ(ierr);
  ierr = PetscFESetBasisSpace(*fem, P);CHKERRQ(ierr);
  ierr = PetscFESetDualSpace(*fem, Q);CHKERRQ(ierr);
  ierr = PetscFESetNumComponents(*fem, Nc);CHKERRQ(ierr);
  ierr = PetscFESetUp(*fem);CHKERRQ(ierr);
  ierr = PetscSpaceDestroy(&P);CHKERRQ(ierr);
  ierr = PetscDualSpaceDestroy(&Q);CHKERRQ(ierr);
  /* Create quadrature (with specified order if given) */
  qorder = qorder >= 0 ? qorder : order;
  ierr = PetscObjectOptionsBegin((PetscObject)*fem);CHKERRQ(ierr);
  ierr = PetscOptionsInt("-petscfe_default_quadrature_order","Quadrature order is one less than quadture points per edge","PetscFECreateDefault",qorder,&qorder,NULL);CHKERRQ(ierr);
  ierr = PetscOptionsEnd();CHKERRQ(ierr);
  quadPointsPerEdge = PetscMax(qorder + 1,1);
  if (isSimplex) {
    ierr = PetscDTGaussJacobiQuadrature(dim,   1, quadPointsPerEdge, -1.0, 1.0, &q);CHKERRQ(ierr);
    ierr = PetscDTGaussJacobiQuadrature(dim-1, 1, quadPointsPerEdge, -1.0, 1.0, &fq);CHKERRQ(ierr);
  }
  else {
    ierr = PetscDTGaussTensorQuadrature(dim,   1, quadPointsPerEdge, -1.0, 1.0, &q);CHKERRQ(ierr);
    ierr = PetscDTGaussTensorQuadrature(dim-1, 1, quadPointsPerEdge, -1.0, 1.0, &fq);CHKERRQ(ierr);
  }
  ierr = PetscFESetQuadrature(*fem, q);CHKERRQ(ierr);
  ierr = PetscFESetFaceQuadrature(*fem, fq);CHKERRQ(ierr);
  ierr = PetscQuadratureDestroy(&q);CHKERRQ(ierr);
  ierr = PetscQuadratureDestroy(&fq);CHKERRQ(ierr);
  PetscFunctionReturn(0);
}

