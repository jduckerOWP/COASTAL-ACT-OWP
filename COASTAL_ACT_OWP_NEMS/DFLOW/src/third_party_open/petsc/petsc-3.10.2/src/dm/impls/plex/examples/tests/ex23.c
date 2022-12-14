static char help[] = "Test for function and field projection\n\n";

#include <petscdmplex.h>
#include <petscds.h>

typedef struct {
  PetscInt  dim;         /* The topological mesh dimension */
  PetscBool cellSimplex; /* Flag for simplices */
  PetscBool submesh;     /* Try with submesh */
  PetscBool auxfield;    /* Try with auxiliary fields */
} AppCtx;

static PetscErrorCode linear(PetscInt dim, PetscReal time, const PetscReal x[], PetscInt Nc, PetscScalar *u, void *ctx)
{
  PetscInt c;
  for (c = 0; c < Nc; ++c) u[c] = (x[0] + x[1])*Nc + c;
  return 0;
}

static void linear_vector(PetscInt dim, PetscInt Nf, PetscInt NfAux,
                          const PetscInt uOff[], const PetscInt uOff_x[], const PetscScalar u[], const PetscScalar u_t[], const PetscScalar u_x[],
                          const PetscInt aOff[], const PetscInt aOff_x[], const PetscScalar a[], const PetscScalar a_t[], const PetscScalar a_x[],
                          PetscReal t, const PetscReal x[], PetscInt numConstants, const PetscScalar constants[], PetscScalar f[])
{
  PetscInt d;
  for (d = uOff[0]; d < uOff[1]; ++d) f[d-uOff[0]] = u[d];
}

static void linear_scalar(PetscInt dim, PetscInt Nf, PetscInt NfAux,
                          const PetscInt uOff[], const PetscInt uOff_x[], const PetscScalar u[], const PetscScalar u_t[], const PetscScalar u_x[],
                          const PetscInt aOff[], const PetscInt aOff_x[], const PetscScalar a[], const PetscScalar a_t[], const PetscScalar a_x[],
                          PetscReal t, const PetscReal x[], PetscInt numConstants, const PetscScalar constants[], PetscScalar f[])
{
  f[0] = u[uOff[1]];
}

static PetscErrorCode ProcessOptions(AppCtx *options)
{
  PetscErrorCode ierr;

  PetscFunctionBegin;
  options->dim         = 2;
  options->cellSimplex = PETSC_TRUE;
  options->submesh     = PETSC_FALSE;
  options->auxfield    = PETSC_FALSE;

  ierr = PetscOptionsBegin(PETSC_COMM_SELF, "", "Meshing Problem Options", "DMPLEX");CHKERRQ(ierr);
  ierr = PetscOptionsInt("-dim", "The topological mesh dimension", "ex23.c", options->dim, &options->dim, NULL);CHKERRQ(ierr);
  ierr = PetscOptionsBool("-cellSimplex", "Flag for simplices", "ex23.c", options->cellSimplex, &options->cellSimplex, NULL);CHKERRQ(ierr);
  ierr = PetscOptionsBool("-submesh", "Flag for trying submesh", "ex23.c", options->submesh, &options->submesh, NULL);CHKERRQ(ierr);
  ierr = PetscOptionsBool("-auxfield", "Flag for trying auxiliary fields", "ex23.c", options->auxfield, &options->auxfield, NULL);CHKERRQ(ierr);
  ierr = PetscOptionsEnd();CHKERRQ(ierr);
  PetscFunctionReturn(0);
}

static PetscErrorCode CreateMesh(MPI_Comm comm, AppCtx *user, DM *dm)
{
  PetscInt       dim      = user->dim;
  PetscBool      simplex  = user->cellSimplex;
  PetscInt       cells[3] = {1, 1, 1};
  PetscErrorCode ierr;

  PetscFunctionBegin;
  ierr = DMPlexCreateBoxMesh(comm, dim, simplex, cells, NULL, NULL, NULL, PETSC_TRUE, dm);CHKERRQ(ierr);
  {
    DM ddm = NULL;

    ierr = DMPlexDistribute(*dm, 0, NULL, &ddm);CHKERRQ(ierr);
    if (ddm) {
      ierr = DMDestroy(dm);CHKERRQ(ierr);
      *dm  = ddm;
    }
  }
  ierr = DMSetFromOptions(*dm);CHKERRQ(ierr);
  ierr = DMViewFromOptions(*dm, NULL, "-orig_dm_view");CHKERRQ(ierr);
  PetscFunctionReturn(0);
}

static PetscErrorCode SetupDiscretization(DM dm, PetscInt dim, PetscBool simplex, AppCtx *user)
{
  PetscFE        fe;
  PetscDS        prob;
  MPI_Comm       comm;
  PetscErrorCode ierr;

  PetscFunctionBeginUser;
  ierr = PetscObjectGetComm((PetscObject) dm, &comm);CHKERRQ(ierr);
  ierr = DMGetDS(dm, &prob);CHKERRQ(ierr);
  ierr = PetscFECreateDefault(comm, dim, dim, simplex, "velocity_", -1, &fe);CHKERRQ(ierr);
  ierr = PetscObjectSetName((PetscObject) fe, "velocity");CHKERRQ(ierr);
  ierr = PetscDSSetDiscretization(prob, 0, (PetscObject) fe);CHKERRQ(ierr);
  ierr = PetscFEDestroy(&fe);CHKERRQ(ierr);
  ierr = PetscFECreateDefault(comm, dim, 1, simplex, "pressure_", -1, &fe);CHKERRQ(ierr);
  ierr = PetscObjectSetName((PetscObject) fe, "pressure");CHKERRQ(ierr);
  ierr = PetscDSSetDiscretization(prob, 1, (PetscObject) fe);CHKERRQ(ierr);
  ierr = PetscFEDestroy(&fe);CHKERRQ(ierr);
  PetscFunctionReturn(0);
}

static PetscErrorCode CreateBoundaryMesh(DM dm, DMLabel *bdLabel, DM *subdm, AppCtx *user)
{
  DMLabel        label;
  PetscInt       dim;
  PetscErrorCode ierr;

  PetscFunctionBeginUser;
  ierr = DMLabelCreate("sub", &label);CHKERRQ(ierr);
  ierr = DMPlexMarkBoundaryFaces(dm, 1, label);CHKERRQ(ierr);
  ierr = DMPlexLabelComplete(dm, label);CHKERRQ(ierr);
  ierr = DMPlexCreateSubmesh(dm, label, 1, PETSC_TRUE, subdm);CHKERRQ(ierr);
  ierr = DMGetDimension(*subdm, &dim);CHKERRQ(ierr);
  ierr = SetupDiscretization(*subdm, dim, user->cellSimplex, user);CHKERRQ(ierr);
  ierr = PetscObjectSetName((PetscObject) *subdm, "boundary");CHKERRQ(ierr);
  ierr = DMViewFromOptions(*subdm, NULL, "-sub_dm_view");CHKERRQ(ierr);
  if (bdLabel) *bdLabel = label;
  else         ierr = DMLabelDestroy(&label);CHKERRQ(ierr);
  PetscFunctionReturn(0);
}

static PetscErrorCode CreateAuxiliaryData(DM dm, DM *auxdm, Vec *la, AppCtx *user)
{
  PetscErrorCode (**afuncs)(PetscInt, PetscReal, const PetscReal [], PetscInt, PetscScalar *, void *);
  PetscInt          dim, Nf, f;
  PetscErrorCode    ierr;

  PetscFunctionBeginUser;
  ierr = DMGetDimension(dm, &dim);CHKERRQ(ierr);
  ierr = DMGetNumFields(dm, &Nf);CHKERRQ(ierr);
  ierr = PetscMalloc1(Nf, &afuncs);CHKERRQ(ierr);
  for (f = 0; f < Nf; ++f) afuncs[f]  = linear;
  ierr = DMClone(dm, auxdm);CHKERRQ(ierr);
  ierr = SetupDiscretization(*auxdm, dim, user->cellSimplex, user);CHKERRQ(ierr);
  ierr = DMCreateLocalVector(*auxdm, la);CHKERRQ(ierr);
  ierr = DMProjectFunctionLocal(dm, 0.0, afuncs, NULL, INSERT_VALUES, *la);CHKERRQ(ierr);
  ierr = VecViewFromOptions(*la, NULL, "-local_aux_view");CHKERRQ(ierr);
  ierr = PetscFree(afuncs);CHKERRQ(ierr);
  PetscFunctionReturn(0);
}

static PetscErrorCode TestFunctionProjection(DM dm, DM auxdm, DMLabel label, Vec la, const char name[], AppCtx *user)
{
  PetscErrorCode (**funcs)(PetscInt, PetscReal, const PetscReal [], PetscInt, PetscScalar *, void *);
  Vec               x, lx;
  PetscInt          Nf, f;
  PetscInt          val[1] = {1};
  char              lname[PETSC_MAX_PATH_LEN];
  PetscErrorCode    ierr;

  PetscFunctionBeginUser;
  if (auxdm) {
    ierr = PetscObjectCompose((PetscObject) dm, "dmAux", (PetscObject) auxdm);CHKERRQ(ierr);
    ierr = PetscObjectCompose((PetscObject) dm, "A", (PetscObject) la);CHKERRQ(ierr);
  }
  ierr = DMGetNumFields(dm, &Nf);CHKERRQ(ierr);
  ierr = PetscMalloc1(Nf, &funcs);CHKERRQ(ierr);
  for (f = 0; f < Nf; ++f) funcs[f] = linear;
  ierr = DMGetGlobalVector(dm, &x);CHKERRQ(ierr);
  ierr = PetscStrcpy(lname, "Function ");CHKERRQ(ierr);
  ierr = PetscStrcat(lname, name);CHKERRQ(ierr);
  ierr = PetscObjectSetName((PetscObject) x, lname);CHKERRQ(ierr);
  if (!label) {ierr = DMProjectFunction(dm, 0.0, funcs, NULL, INSERT_VALUES, x);CHKERRQ(ierr);}
  else        {ierr = DMProjectFunctionLabel(dm, 0.0, label, 1, val, 0, NULL, funcs, NULL, INSERT_VALUES, x);CHKERRQ(ierr);}
  ierr = VecViewFromOptions(x, NULL, "-func_view");CHKERRQ(ierr);
  ierr = DMRestoreGlobalVector(dm, &x);CHKERRQ(ierr);
  ierr = DMGetLocalVector(dm, &lx);CHKERRQ(ierr);
  ierr = PetscStrcpy(lname, "Local Function ");CHKERRQ(ierr);
  ierr = PetscStrcat(lname, name);CHKERRQ(ierr);
  ierr = PetscObjectSetName((PetscObject) lx, lname);CHKERRQ(ierr);
  if (!label) {ierr = DMProjectFunctionLocal(dm, 0.0, funcs, NULL, INSERT_VALUES, lx);CHKERRQ(ierr);}
  else        {ierr = DMProjectFunctionLabelLocal(dm, 0.0, label, 1, val, 0, NULL, funcs, NULL, INSERT_VALUES, lx);CHKERRQ(ierr);}
  ierr = VecViewFromOptions(lx, NULL, "-local_func_view");CHKERRQ(ierr);
  ierr = DMRestoreLocalVector(dm, &lx);CHKERRQ(ierr);
  ierr = PetscFree(funcs);CHKERRQ(ierr);
  if (auxdm) {
    ierr = PetscObjectCompose((PetscObject) dm, "dmAux", NULL);CHKERRQ(ierr);
    ierr = PetscObjectCompose((PetscObject) dm, "A", NULL);CHKERRQ(ierr);
  }
  PetscFunctionReturn(0);
}

static PetscErrorCode TestFieldProjection(DM dm, DM auxdm, DMLabel label, Vec la, const char name[], AppCtx *user)
{
  PetscErrorCode (**afuncs)(PetscInt, PetscReal, const PetscReal [], PetscInt, PetscScalar *, void *);
  void           (**funcs)(PetscInt, PetscInt, PetscInt,
                           const PetscInt[], const PetscInt[], const PetscScalar[], const PetscScalar[], const PetscScalar[],
                           const PetscInt[], const PetscInt[], const PetscScalar[], const PetscScalar[], const PetscScalar[],
                           PetscReal, const PetscReal[], PetscInt, const PetscScalar[], PetscScalar[]);
  Vec               lx, lu;
  PetscInt          Nf, f;
  PetscInt          val[1] = {1};
  char              lname[PETSC_MAX_PATH_LEN];
  PetscErrorCode    ierr;

  PetscFunctionBeginUser;
  if (auxdm) {
    ierr = PetscObjectCompose((PetscObject) dm, "dmAux", (PetscObject) auxdm);CHKERRQ(ierr);
    ierr = PetscObjectCompose((PetscObject) dm, "A", (PetscObject) la);CHKERRQ(ierr);
  }
  ierr = DMGetNumFields(dm, &Nf);CHKERRQ(ierr);
  ierr = PetscMalloc2(Nf, &funcs, Nf, &afuncs);CHKERRQ(ierr);
  for (f = 0; f < Nf; ++f) afuncs[f]  = linear;
  funcs[0] = linear_vector;
  funcs[1] = linear_scalar;
  ierr = DMGetLocalVector(dm, &lu);CHKERRQ(ierr);
  ierr = PetscStrcpy(lname, "Local Field Input ");CHKERRQ(ierr);
  ierr = PetscStrcat(lname, name);CHKERRQ(ierr);
  ierr = PetscObjectSetName((PetscObject) lu, lname);CHKERRQ(ierr);
  if (!label) {ierr = DMProjectFunctionLocal(dm, 0.0, afuncs, NULL, INSERT_VALUES, lu);CHKERRQ(ierr);}
  else        {ierr = DMProjectFunctionLabelLocal(dm, 0.0, label, 1, val, 0, NULL, afuncs, NULL, INSERT_VALUES, lu);CHKERRQ(ierr);}
  ierr = VecViewFromOptions(lu, NULL, "-local_input_view");CHKERRQ(ierr);
  ierr = DMGetLocalVector(dm, &lx);CHKERRQ(ierr);
  ierr = PetscStrcpy(lname, "Local Field ");CHKERRQ(ierr);
  ierr = PetscStrcat(lname, name);CHKERRQ(ierr);
  ierr = PetscObjectSetName((PetscObject) lx, lname);CHKERRQ(ierr);
  if (!label) {ierr = DMProjectFieldLocal(dm, 0.0, lu, funcs, INSERT_VALUES, lx);CHKERRQ(ierr);}
  else        {ierr = DMProjectFieldLabelLocal(dm, 0.0, label, 1, val, 0, NULL, lu, funcs, INSERT_VALUES, lx);CHKERRQ(ierr);}
  ierr = VecViewFromOptions(lx, NULL, "-local_field_view");CHKERRQ(ierr);
  ierr = DMRestoreLocalVector(dm, &lx);CHKERRQ(ierr);
  ierr = DMRestoreLocalVector(dm, &lu);CHKERRQ(ierr);
  ierr = PetscFree2(funcs, afuncs);CHKERRQ(ierr);
  if (auxdm) {
    ierr = PetscObjectCompose((PetscObject) dm, "dmAux", NULL);CHKERRQ(ierr);
    ierr = PetscObjectCompose((PetscObject) dm, "A", NULL);CHKERRQ(ierr);
  }
  PetscFunctionReturn(0);
}

int main(int argc, char **argv)
{
  DM             dm, subdm, auxdm;
  Vec            la;
  AppCtx         user;
  PetscErrorCode ierr;

  ierr = PetscInitialize(&argc, &argv, NULL,help);if (ierr) return ierr;
  ierr = ProcessOptions(&user);CHKERRQ(ierr);
  ierr = CreateMesh(PETSC_COMM_WORLD, &user, &dm);CHKERRQ(ierr);
  ierr = SetupDiscretization(dm, user.dim, user.cellSimplex, &user);CHKERRQ(ierr);
  /* Volumetric Mesh Projection */
  ierr = TestFunctionProjection(dm, NULL, NULL, NULL, "Volumetric Primary", &user);CHKERRQ(ierr);
  ierr = TestFieldProjection(dm, NULL, NULL, NULL, "Volumetric Primary", &user);CHKERRQ(ierr);
  if (user.auxfield) {
    /* Volumetric Mesh Projection with Volumetric Data */
    ierr = CreateAuxiliaryData(dm, &auxdm, &la, &user);CHKERRQ(ierr);
    ierr = TestFunctionProjection(dm, auxdm, NULL, la, "Volumetric Primary and Volumetric Auxiliary", &user);CHKERRQ(ierr);
    ierr = TestFieldProjection(dm, auxdm, NULL, la, "Volumetric Primary and Volumetric Auxiliary", &user);CHKERRQ(ierr);
    ierr = VecDestroy(&la);CHKERRQ(ierr);
    /* Update of Volumetric Auxiliary Data with primary Volumetric Data */
    ierr = DMGetLocalVector(dm, &la);CHKERRQ(ierr);
    ierr = VecSet(la, 1.0);CHKERRQ(ierr);
    ierr = TestFieldProjection(auxdm, dm, NULL, la, "Volumetric Auxiliary Update with Volumetric Primary", &user);CHKERRQ(ierr);
    ierr = DMRestoreLocalVector(dm, &la);CHKERRQ(ierr);
    ierr = DMDestroy(&auxdm);CHKERRQ(ierr);
  }
  if (user.submesh) {
    DMLabel bdLabel;

    /* Boundary Mesh Projection */
    ierr = CreateBoundaryMesh(dm, &bdLabel, &subdm, &user);CHKERRQ(ierr);
    ierr = TestFunctionProjection(subdm, NULL, NULL, NULL, "Boundary Primary", &user);CHKERRQ(ierr);
    ierr = TestFieldProjection(subdm, NULL, NULL, NULL, "Boundary Primary", &user);CHKERRQ(ierr);
    if (user.auxfield) {
      /* Boundary Mesh Projection with Boundary Data */
      ierr = CreateAuxiliaryData(subdm, &auxdm, &la, &user);CHKERRQ(ierr);
      ierr = TestFunctionProjection(subdm, auxdm, NULL, la, "Boundary Primary and Boundary Auxiliary", &user);CHKERRQ(ierr);
      ierr = TestFieldProjection(subdm, auxdm, NULL, la, "Boundary Primary and Boundary Auxiliary", &user);CHKERRQ(ierr);
      ierr = VecDestroy(&la);CHKERRQ(ierr);
      ierr = DMDestroy(&auxdm);CHKERRQ(ierr);
      /* Volumetric Mesh Projection with Boundary Data */
      ierr = CreateAuxiliaryData(subdm, &auxdm, &la, &user);CHKERRQ(ierr);
      ierr = TestFunctionProjection(dm, auxdm, bdLabel, la, "Volumetric Primary and Boundary Auxiliary", &user);CHKERRQ(ierr);
      ierr = TestFieldProjection(dm, auxdm, bdLabel, la, "Volumetric Primary and Boundary Auxiliary", &user);CHKERRQ(ierr);
      ierr = VecDestroy(&la);CHKERRQ(ierr);
      ierr = DMDestroy(&auxdm);CHKERRQ(ierr);
    }
    ierr = DMLabelDestroy(&bdLabel);CHKERRQ(ierr);
    ierr = DMDestroy(&subdm);CHKERRQ(ierr);
  }
  ierr = DMDestroy(&dm);CHKERRQ(ierr);
  ierr = PetscFinalize();
  return ierr;
}

/*TEST

  test:
    suffix: 0
    requires: triangle
    args: -dim 2 -func_view -local_func_view -local_input_view -local_field_view
  test:
    suffix: 1
    requires: triangle
    args: -dim 2 -velocity_petscspace_degree 1 -velocity_petscfe_default_quadrature_order 2 -pressure_petscspace_degree 2 -pressure_petscfe_default_quadrature_order 2 -func_view -local_func_view -local_input_view -local_field_view -submesh -auxfield

TEST*/

/*
  Post-processing wants to project a function of the fields into some FE space
  - This is DMProjectField()
  - What about changing the number of components of the output, like displacement to stress? Aux vars

  Update of state variables
  - This is DMProjectField(), but solution must be the aux var
*/
