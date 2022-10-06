#include <petsc/private/dmpleximpl.h>   /*I      "petscdmplex.h"   I*/

#include <petsc/private/petscfeimpl.h>

static PetscErrorCode DMProjectPoint_Func_Private(DM dm, PetscDS prob, PetscReal time, PetscFEGeom *fegeom, PetscFVCellGeom *fvgeom, PetscBool isFE[], PetscDualSpace sp[],
                                                  PetscErrorCode (**funcs)(PetscInt, PetscReal, const PetscReal [], PetscInt, PetscScalar *, void *), void **ctxs,
                                                  PetscScalar values[])
{
  PetscInt       coordDim, Nf, *Nc, f, totDim, spDim, d, v, tp;
  PetscBool      isAffine;
  PetscErrorCode ierr;

  PetscFunctionBeginHot;
  ierr = DMGetCoordinateDim(dm,&coordDim);CHKERRQ(ierr);
  ierr = PetscDSGetNumFields(prob, &Nf);CHKERRQ(ierr);
  ierr = PetscDSGetComponents(prob, &Nc);CHKERRQ(ierr);
  ierr = PetscDSGetTotalDimension(prob, &totDim);CHKERRQ(ierr);
  /* Get values for closure */
  isAffine = fegeom->isAffine;
  for (f = 0, v = 0, tp = 0; f < Nf; ++f) {
    void * const ctx = ctxs ? ctxs[f] : NULL;

    if (!sp[f]) continue;
    ierr = PetscDualSpaceGetDimension(sp[f], &spDim);CHKERRQ(ierr);
    if (funcs[f]) {
      if (isFE[f]) {
        PetscQuadrature   allPoints;
        PetscInt          q, dim, numPoints;
        const PetscReal   *points;
        PetscScalar       *pointEval;
        PetscReal         *x;
        DM                dm;

        ierr = PetscDualSpaceGetDM(sp[f],&dm);CHKERRQ(ierr);
        ierr = PetscDualSpaceGetAllPoints(sp[f], &allPoints);CHKERRQ(ierr);
        ierr = PetscQuadratureGetData(allPoints,&dim,NULL,&numPoints,&points,NULL);CHKERRQ(ierr);
        ierr = PetscDualSpaceGetDM(sp[f],&dm);CHKERRQ(ierr);
        ierr = DMGetWorkArray(dm,numPoints*Nc[f],MPIU_SCALAR,&pointEval);CHKERRQ(ierr);
        ierr = DMGetWorkArray(dm,coordDim,MPIU_REAL,&x);CHKERRQ(ierr);
        for (q = 0; q < numPoints; q++, tp++) {
          const PetscReal *v0;

          if (isAffine) {
            CoordinatesRefToReal(coordDim, fegeom->dim, fegeom->xi, fegeom->v, fegeom->J, &points[q*dim], x);
            v0 = x;
          } else {
            v0 = &fegeom->v[tp*coordDim];
          }
          ierr = (*funcs[f])(coordDim,time,v0, Nc[f], &pointEval[Nc[f]*q], ctx);CHKERRQ(ierr);
        }
        ierr = PetscDualSpaceApplyAll(sp[f], pointEval, &values[v]);CHKERRQ(ierr);
        ierr = DMRestoreWorkArray(dm,coordDim,MPIU_REAL,&x);CHKERRQ(ierr);
        ierr = DMRestoreWorkArray(dm,numPoints*Nc[f],MPIU_SCALAR,&pointEval);CHKERRQ(ierr);
        v += spDim;
      } else {
        for (d = 0; d < spDim; ++d, ++v) {
          ierr = PetscDualSpaceApplyFVM(sp[f], d, time, fvgeom, Nc[f], funcs[f], ctx, &values[v]);CHKERRQ(ierr);
        }
      }
    } else {
      for (d = 0; d < spDim; d++, v++) {values[v] = 0.0;}
    }
  }
  PetscFunctionReturn(0);
}

static PetscErrorCode DMProjectPoint_Field_Private(DM dm, PetscDS prob, DM dmAux, PetscDS probAux, PetscReal time, Vec localU, Vec localA, PetscFEGeom *fegeom, PetscDualSpace sp[], PetscInt p, PetscInt Ncc, const PetscInt comps[],
                                                   PetscReal **basisTab, PetscReal **basisDerTab, PetscReal **basisTabAux, PetscReal **basisDerTabAux,
                                                   void (**funcs)(PetscInt, PetscInt, PetscInt,
                                                                  const PetscInt[], const PetscInt[], const PetscScalar[], const PetscScalar[], const PetscScalar[],
                                                                  const PetscInt[], const PetscInt[], const PetscScalar[], const PetscScalar[], const PetscScalar[],
                                                                  PetscReal, const PetscReal[], PetscInt, const PetscScalar[], PetscScalar[]), void **ctxs,
                                                   PetscScalar values[])
{
  PetscSection       section, sectionAux = NULL;
  PetscScalar       *u, *u_t = NULL, *u_x, *a = NULL, *a_t = NULL, *a_x = NULL, *refSpaceDer, *refSpaceDerAux = NULL, *bc;
  PetscScalar       *coefficients   = NULL, *coefficientsAux   = NULL;
  PetscScalar       *coefficients_t = NULL, *coefficientsAux_t = NULL;
  const PetscScalar *constants;
  PetscReal         *x;
  PetscInt          *uOff, *uOff_x, *aOff = NULL, *aOff_x = NULL, *Nb, *Nc, *NbAux = NULL, *NcAux = NULL;
  const PetscInt     dE = fegeom->dimEmbed;
  PetscInt           dimAux = 0, numConstants, Nf, NfAux = 0, f, spDim, d, v, tp = 0;
  PetscBool          isAffine;
  PetscErrorCode     ierr;

  PetscFunctionBeginHot;
  ierr = PetscDSGetNumFields(prob, &Nf);CHKERRQ(ierr);
  ierr = PetscDSGetDimensions(prob, &Nb);CHKERRQ(ierr);
  ierr = PetscDSGetComponents(prob, &Nc);CHKERRQ(ierr);
  ierr = PetscDSGetComponentOffsets(prob, &uOff);CHKERRQ(ierr);
  ierr = PetscDSGetComponentDerivativeOffsets(prob, &uOff_x);CHKERRQ(ierr);
  ierr = PetscDSGetEvaluationArrays(prob, &u, &bc /*&u_t*/, &u_x);CHKERRQ(ierr);
  ierr = PetscDSGetRefCoordArrays(prob, &x, &refSpaceDer);CHKERRQ(ierr);
  ierr = PetscDSGetConstants(prob, &numConstants, &constants);CHKERRQ(ierr);
  ierr = DMGetSection(dm, &section);CHKERRQ(ierr);
  ierr = DMPlexVecGetClosure(dm, section, localU, p, NULL, &coefficients);CHKERRQ(ierr);
  if (dmAux) {
    DMLabel  spmap;
    PetscInt subp = p;

    /* If dm is a submesh, do not get subpoint */
    ierr = DMPlexGetSubpointMap(dm, &spmap);CHKERRQ(ierr);
    if (!spmap) {ierr = DMPlexGetSubpoint(dmAux, p, &subp);CHKERRQ(ierr);}
    ierr = PetscDSGetSpatialDimension(probAux, &dimAux);CHKERRQ(ierr);
    ierr = PetscDSGetNumFields(probAux, &NfAux);CHKERRQ(ierr);
    ierr = PetscDSGetDimensions(probAux, &NbAux);CHKERRQ(ierr);
    ierr = PetscDSGetComponents(probAux, &NcAux);CHKERRQ(ierr);
    ierr = DMGetSection(dmAux, &sectionAux);CHKERRQ(ierr);
    ierr = PetscDSGetComponentOffsets(probAux, &aOff);CHKERRQ(ierr);
    ierr = PetscDSGetComponentDerivativeOffsets(probAux, &aOff_x);CHKERRQ(ierr);
    ierr = PetscDSGetEvaluationArrays(probAux, &a, NULL /*&a_t*/, &a_x);CHKERRQ(ierr);
    ierr = PetscDSGetRefCoordArrays(probAux, NULL, &refSpaceDerAux);CHKERRQ(ierr);
    ierr = DMPlexVecGetClosure(dmAux, sectionAux, localA, subp, NULL, &coefficientsAux);CHKERRQ(ierr);
  }
  /* Get values for closure */
  isAffine = fegeom->isAffine;
  for (f = 0, v = 0; f < Nf; ++f) {
    PetscQuadrature   allPoints;
    PetscInt          q, dim, numPoints;
    const PetscReal   *points;
    PetscScalar       *pointEval;
    DM                dm;

    if (!sp[f]) continue;
    ierr = PetscDualSpaceGetDimension(sp[f], &spDim);CHKERRQ(ierr);
    if (!funcs[f]) {
      for (d = 0; d < spDim; d++, v++) values[v] = 0.;
      continue;
    }
    ierr = PetscDualSpaceGetDM(sp[f],&dm);CHKERRQ(ierr);
    ierr = PetscDualSpaceGetAllPoints(sp[f], &allPoints);CHKERRQ(ierr);
    ierr = PetscQuadratureGetData(allPoints,&dim,NULL,&numPoints,&points,NULL);CHKERRQ(ierr);
    ierr = DMGetWorkArray(dm,numPoints*Nc[f],MPIU_SCALAR,&pointEval);CHKERRQ(ierr);
    for (q = 0; q < numPoints; ++q, ++tp) {
      const PetscReal *v0;
      const PetscReal *invJ;

      if (isAffine) {
        CoordinatesRefToReal(dE, dim, fegeom->xi, fegeom->v, fegeom->J, &points[q*dim], x);
        v0 = x;
        invJ = fegeom->invJ;
      } else {
        v0 = &fegeom->v[tp*dE];
        invJ = &fegeom->invJ[tp*dE*dE];
      }
      EvaluateFieldJets(dim, Nf, Nb, Nc, tp, basisTab, basisDerTab, refSpaceDer, invJ, coefficients, coefficients_t, u, u_x, u_t);
      if (probAux) {EvaluateFieldJets(dimAux, NfAux, NbAux, NcAux, tp, basisTabAux, basisDerTabAux, refSpaceDerAux, invJ, coefficientsAux, coefficientsAux_t, a, a_x, a_t);}
      (*funcs[f])(dE, Nf, NfAux, uOff, uOff_x, u, u_t, u_x, aOff, aOff_x, a, a_t, a_x, time, v0, numConstants, constants, &pointEval[Nc[f]*q]);
    }
    ierr = PetscDualSpaceApplyAll(sp[f], pointEval, &values[v]);CHKERRQ(ierr);
    ierr = DMRestoreWorkArray(dm,numPoints*Nc[f],MPIU_SCALAR,&pointEval);CHKERRQ(ierr);
    v += spDim;
  }
  ierr = DMPlexVecRestoreClosure(dm, section, localU, p, NULL, &coefficients);CHKERRQ(ierr);
  if (dmAux) {ierr = DMPlexVecRestoreClosure(dmAux, sectionAux, localA, p, NULL, &coefficientsAux);CHKERRQ(ierr);}
  PetscFunctionReturn(0);
}

static PetscErrorCode DMProjectPoint_Private(DM dm, PetscDS prob, PetscFEGeom *fegeom, DM dmAux, PetscDS probAux, PetscInt effectiveHeight, PetscReal time, Vec localU, Vec localA, PetscBool hasFE, PetscBool hasFV, PetscBool isFE[],
                                             PetscDualSpace sp[], PetscInt p, PetscInt Ncc, const PetscInt comps[],
                                             PetscReal **basisTab, PetscReal **basisDerTab, PetscReal **basisTabAux, PetscReal **basisDerTabAux,
                                             DMBoundaryConditionType type, void (**funcs)(void), void **ctxs, PetscBool fieldActive[], PetscScalar values[])
{
  PetscFVCellGeom fvgeom;
  PetscInt        dim, dimEmbed;
  PetscErrorCode  ierr;

  PetscFunctionBeginHot;
  ierr = DMGetDimension(dm, &dim);CHKERRQ(ierr);
  ierr = DMGetCoordinateDim(dm, &dimEmbed);CHKERRQ(ierr);
  if (hasFV) {ierr = DMPlexComputeCellGeometryFVM(dm, p, &fvgeom.volume, fvgeom.centroid, NULL);CHKERRQ(ierr);}
  switch (type) {
  case DM_BC_ESSENTIAL:
  case DM_BC_NATURAL:
    ierr = DMProjectPoint_Func_Private(dm, prob, time, fegeom, &fvgeom, isFE, sp, (PetscErrorCode (**)(PetscInt, PetscReal, const PetscReal [], PetscInt, PetscScalar *, void *)) funcs, ctxs, values);CHKERRQ(ierr);break;
  case DM_BC_ESSENTIAL_FIELD:
  case DM_BC_NATURAL_FIELD:
    ierr = DMProjectPoint_Field_Private(dm, prob, dmAux, probAux, time, localU, localA, fegeom, sp, p, Ncc, comps,
                                        basisTab, basisDerTab, basisTabAux, basisDerTabAux,
                                        (void (**)(PetscInt, PetscInt, PetscInt,
                                                   const PetscInt[], const PetscInt[], const PetscScalar[], const PetscScalar[], const PetscScalar[],
                                                   const PetscInt[], const PetscInt[], const PetscScalar[], const PetscScalar[], const PetscScalar[],
                                                   PetscReal, const PetscReal[], PetscInt, const PetscScalar[], PetscScalar[])) funcs, ctxs, values);CHKERRQ(ierr);break;
  default: SETERRQ1(PetscObjectComm((PetscObject) dm), PETSC_ERR_ARG_WRONG, "Unknown boundary condition type: %d", (int) type);
  }
  PetscFunctionReturn(0);
}

static PetscErrorCode PetscDualSpaceGetAllPointsUnion(PetscInt Nf, PetscDualSpace *sp, PetscInt dim, void (**funcs)(void), PetscQuadrature *allPoints)
{
  PetscReal      *points;
  PetscInt       f, numPoints;
  PetscErrorCode ierr;

  PetscFunctionBegin;
  numPoints = 0;
  for (f = 0; f < Nf; ++f) {
    if (funcs[f]) {
      PetscQuadrature fAllPoints;
      PetscInt        fNumPoints;

      ierr = PetscDualSpaceGetAllPoints(sp[f],&fAllPoints);CHKERRQ(ierr);
      ierr = PetscQuadratureGetData(fAllPoints, NULL, NULL, &fNumPoints, NULL, NULL);CHKERRQ(ierr);
      numPoints += fNumPoints;
    }
  }
  ierr = PetscMalloc1(dim*numPoints,&points);CHKERRQ(ierr);
  numPoints = 0;
  for (f = 0; f < Nf; ++f) {
    PetscInt spDim;

    ierr = PetscDualSpaceGetDimension(sp[f], &spDim);CHKERRQ(ierr);
    if (funcs[f]) {
      PetscQuadrature fAllPoints;
      PetscInt        qdim, fNumPoints, q;
      const PetscReal *fPoints;

      ierr = PetscDualSpaceGetAllPoints(sp[f],&fAllPoints);CHKERRQ(ierr);
      ierr = PetscQuadratureGetData(fAllPoints, &qdim, NULL, &fNumPoints, &fPoints, NULL);CHKERRQ(ierr);
      if (qdim != dim) SETERRQ2(PETSC_COMM_SELF, PETSC_ERR_ARG_SIZ, "Spatial dimension %D for dual basis does not match input dimension %D", qdim, dim);
      for (q = 0; q < fNumPoints*dim; ++q) points[numPoints*dim+q] = fPoints[q];
      numPoints += fNumPoints;
    }
  }
  ierr = PetscQuadratureCreate(PETSC_COMM_SELF,allPoints);CHKERRQ(ierr);
  ierr = PetscQuadratureSetData(*allPoints,dim,0,numPoints,points,NULL);CHKERRQ(ierr);
  PetscFunctionReturn(0);
}

/*
  This function iterates over a manifold, and interpolates the input function/field using the basis provided by the DS in our DM

  There are several different scenarios:

  1) Volumetric mesh with volumetric auxiliary data

     Here minHeight=0 since we loop over cells.

  2) Boundary mesh with boundary auxiliary data

     Here minHeight=1 since we loop over faces. This normally happens since we hang cells off of our boundary meshes to facilitate computation.

  3) Volumetric mesh with boundary auxiliary data

     Here minHeight=1 and auxbd=PETSC_TRUE since we loop over faces and use data only supported on those faces. This is common when imposing Dirichlet boundary conditions.

  The maxHeight is used to support enforcement of constraints in DMForest.

  If localU is given and not equal to localX, we call DMPlexInsertBoundaryValues() to complete it.

  If we are using an input field (DM_BC_ESSENTIAL_FIELD or DM_BC_NATURAL_FIELD), we need to evaluate it at all the quadrature points of the dual basis functionals.
    - We use effectiveHeight to mean the height above our incoming DS. For example, if the DS is for a submesh then the effective height is zero, whereas if the DS
      is for the volumetric mesh, but we are iterating over a surface, then the effective height is nonzero. When th effective height is nonzero, we need to extract
      dual spaces for the boundary from our input spaces.
    - After extracting all quadrature points, we tabulate the input fields and auxiliary fields on them.

  We check that the #dof(closure(p)) == #dual basis functionals(p) for a representative p in the iteration

  If we have a label, we iterate over those points. This will probably break the maxHeight functionality since we do not check the height of those points.
*/
static PetscErrorCode DMProjectLocal_Generic_Plex(DM dm, PetscReal time, Vec localU,
                                                  PetscInt Ncc, const PetscInt comps[], DMLabel label, PetscInt numIds, const PetscInt ids[],
                                                  DMBoundaryConditionType type, void (**funcs)(void), void **ctxs,
                                                  InsertMode mode, Vec localX)
{
  DM              dmAux = NULL;
  PetscDS         prob, probAux = NULL;
  Vec             localA = NULL;
  PetscSection    section;
  PetscDualSpace *sp, *cellsp;
  PetscReal     **basisTab = NULL, **basisDerTab = NULL, **basisTabAux = NULL, **basisDerTabAux = NULL;
  PetscInt       *Nc;
  PetscInt        dim, dimEmbed, depth, minHeight, maxHeight, h, Nf, NfAux = 0, f;
  PetscBool      *isFE, hasFE = PETSC_FALSE, hasFV = PETSC_FALSE, auxBd = PETSC_FALSE;
  DMField         coordField;
  DMLabel         depthLabel;
  PetscQuadrature allPoints = NULL;
  PetscErrorCode  ierr;

  PetscFunctionBegin;
  ierr = PetscObjectQuery((PetscObject) dm, "dmAux", (PetscObject *) &dmAux);CHKERRQ(ierr);
  ierr = PetscObjectQuery((PetscObject) dm, "A", (PetscObject *) &localA);CHKERRQ(ierr);
  ierr = DMGetDimension(dm, &dim);CHKERRQ(ierr);
  ierr = DMPlexGetVTKCellHeight(dm, &minHeight);CHKERRQ(ierr);
  /* Auxiliary information can only be used with interpolation of field functions */
  if (type == DM_BC_ESSENTIAL_FIELD || type == DM_BC_NATURAL_FIELD) {
    if (!minHeight && dmAux) {
      DMLabel spmap;

      /* If dmAux is a surface, then force the projection to take place over a surface */
      ierr = DMPlexGetSubpointMap(dmAux, &spmap);CHKERRQ(ierr);
      if (spmap) {minHeight = 1; auxBd = PETSC_TRUE;}
    }
  }
  ierr = DMPlexGetMaxProjectionHeight(dm, &maxHeight);CHKERRQ(ierr);
  maxHeight = PetscMax(maxHeight, minHeight);
  if (maxHeight < 0 || maxHeight > dim) {SETERRQ2(PETSC_COMM_SELF, PETSC_ERR_ARG_OUTOFRANGE, "Maximum projection height %d not in [0, %d)\n", maxHeight, dim);}
  ierr = DMGetDS(dm, &prob);CHKERRQ(ierr);
  ierr = DMGetCoordinateDim(dm, &dimEmbed);CHKERRQ(ierr);
  ierr = DMGetSection(dm, &section);CHKERRQ(ierr);
  ierr = PetscSectionGetNumFields(section, &Nf);CHKERRQ(ierr);
  if (dmAux) {
    ierr = DMGetDS(dmAux, &probAux);CHKERRQ(ierr);
    ierr = PetscDSGetNumFields(probAux, &NfAux);CHKERRQ(ierr);
  }
  ierr = PetscDSGetComponents(prob, &Nc);CHKERRQ(ierr);
  ierr = PetscMalloc2(Nf, &isFE, Nf, &sp);CHKERRQ(ierr);
  if (maxHeight > 0) {ierr = PetscMalloc1(Nf, &cellsp);CHKERRQ(ierr);}
  else               {cellsp = sp;}
  if (localU && localU != localX) {ierr = DMPlexInsertBoundaryValues(dm, PETSC_TRUE, localU, time, NULL, NULL, NULL);CHKERRQ(ierr);}
  /* Get cell dual spaces */
  for (f = 0; f < Nf; ++f) {
    PetscObject  obj;
    PetscClassId id;

    ierr = DMGetField(dm, f, &obj);CHKERRQ(ierr);
    ierr = PetscObjectGetClassId(obj, &id);CHKERRQ(ierr);
    if (id == PETSCFE_CLASSID) {
      PetscFE fe = (PetscFE) obj;

      hasFE   = PETSC_TRUE;
      isFE[f] = PETSC_TRUE;
      ierr  = PetscFEGetDualSpace(fe, &cellsp[f]);CHKERRQ(ierr);
    } else if (id == PETSCFV_CLASSID) {
      PetscFV fv = (PetscFV) obj;

      hasFV   = PETSC_TRUE;
      isFE[f] = PETSC_FALSE;
      ierr = PetscFVGetDualSpace(fv, &cellsp[f]);CHKERRQ(ierr);
    } else SETERRQ1(PetscObjectComm((PetscObject) dm), PETSC_ERR_ARG_WRONG, "Unknown discretization type for field %d", f);
  }
  ierr = DMGetCoordinateField(dm,&coordField);CHKERRQ(ierr);
  if (type == DM_BC_ESSENTIAL_FIELD || type == DM_BC_NATURAL_FIELD) {
    PetscInt         effectiveHeight = auxBd ? minHeight : 0;
    PetscFE          fem, subfem;
    const PetscReal *points;
    PetscInt         numPoints;

    if (maxHeight > minHeight) SETERRQ(PetscObjectComm((PetscObject) dm), PETSC_ERR_SUP, "Field projection not supported for face interpolation");
    for (f = 0; f < Nf; ++f) {
      if (!effectiveHeight) {sp[f] = cellsp[f];}
      else                  {ierr = PetscDualSpaceGetHeightSubspace(cellsp[f], effectiveHeight, &sp[f]);CHKERRQ(ierr);}
    }
    ierr = PetscDualSpaceGetAllPointsUnion(Nf,sp,dim-effectiveHeight,funcs,&allPoints);CHKERRQ(ierr);
    ierr = PetscQuadratureGetData(allPoints,NULL,NULL,&numPoints,&points,NULL);CHKERRQ(ierr);
    ierr = PetscMalloc4(Nf, &basisTab, Nf, &basisDerTab, NfAux, &basisTabAux, NfAux, &basisDerTabAux);CHKERRQ(ierr);
    for (f = 0; f < Nf; ++f) {
      if (!isFE[f]) continue;
      ierr = PetscDSGetDiscretization(prob, f, (PetscObject *) &fem);CHKERRQ(ierr);
      if (!effectiveHeight) {subfem = fem;}
      else                  {ierr = PetscFEGetHeightSubspace(fem, effectiveHeight, &subfem);CHKERRQ(ierr);}
      ierr = PetscFEGetTabulation(subfem, numPoints, points, &basisTab[f], &basisDerTab[f], NULL);CHKERRQ(ierr);
    }
    for (f = 0; f < NfAux; ++f) {
      ierr = PetscDSGetDiscretization(probAux, f, (PetscObject *) &fem);CHKERRQ(ierr);
      if (!effectiveHeight || auxBd) {subfem = fem;}
      else                           {ierr = PetscFEGetHeightSubspace(fem, effectiveHeight, &subfem);CHKERRQ(ierr);}
      ierr = PetscFEGetTabulation(subfem, numPoints, points, &basisTabAux[f], &basisDerTabAux[f], NULL);CHKERRQ(ierr);
    }
  }
  ierr = DMPlexGetDepth(dm,&depth);CHKERRQ(ierr);
  ierr = DMPlexGetDepthLabel(dm,&depthLabel);CHKERRQ(ierr);
  /* Note: We make no attempt to optimize for height. Higher height things just overwrite the lower height results. */
  for (h = minHeight; h <= maxHeight; h++) {
    PetscInt     effectiveHeight = h - (auxBd ? 0 : minHeight);
    PetscDS      probEff         = prob;
    PetscScalar *values;
    PetscBool   *fieldActive;
    PetscInt     maxDegree;
    PetscInt     pStart, pEnd, p, spDim, totDim, numValues;
    IS           heightIS;

    if (effectiveHeight) {ierr = PetscDSGetHeightSubspace(prob, effectiveHeight, &probEff);CHKERRQ(ierr);}
    ierr = DMPlexGetHeightStratum(dm, h, &pStart, &pEnd);CHKERRQ(ierr);
    ierr = DMLabelGetStratumIS(depthLabel,depth - h,&heightIS);CHKERRQ(ierr);
    if (!h) {
      PetscInt cEndInterior;

      ierr = DMPlexGetHybridBounds(dm, &cEndInterior, NULL, NULL, NULL);CHKERRQ(ierr);
      pEnd = cEndInterior < 0 ? pEnd : cEndInterior;
    }
    if (pEnd <= pStart) {
      ierr = ISDestroy(&heightIS);CHKERRQ(ierr);
      continue;
    }
    /* Compute totDim, the number of dofs in the closure of a point at this height */
    totDim = 0;
    for (f = 0; f < Nf; ++f) {
      if (!effectiveHeight) {
        sp[f] = cellsp[f];
      } else {
        ierr = PetscDualSpaceGetHeightSubspace(cellsp[f], effectiveHeight, &sp[f]);CHKERRQ(ierr);
        if (!sp[f]) continue;
      }
      ierr = PetscDualSpaceGetDimension(sp[f], &spDim);CHKERRQ(ierr);
      totDim += spDim;
    }
    ierr = DMPlexVecGetClosure(dm, section, localX, pStart, &numValues, NULL);CHKERRQ(ierr);
    if (numValues != totDim) SETERRQ2(PETSC_COMM_SELF, PETSC_ERR_ARG_SIZ, "The section point closure size %d != dual space dimension %d", numValues, totDim);
    if (!totDim) {
      ierr = ISDestroy(&heightIS);CHKERRQ(ierr);
      continue;
    }
    /* Loop over points at this height */
    ierr = DMGetWorkArray(dm, numValues, MPIU_SCALAR, &values);CHKERRQ(ierr);
    ierr = DMGetWorkArray(dm, Nf, MPI_INT, &fieldActive);CHKERRQ(ierr);
    for (f = 0; f < Nf; ++f) fieldActive[f] = (funcs[f] && sp[f]) ? PETSC_TRUE : PETSC_FALSE;
    if (label) {
      PetscInt i;

      for (i = 0; i < numIds; ++i) {
        IS              pointIS, isectIS;
        const PetscInt *points;
        PetscInt        n;
        PetscFEGeom  *fegeom = NULL, *chunkgeom = NULL;
        PetscQuadrature quad = NULL;

        ierr = DMLabelGetStratumIS(label, ids[i], &pointIS);CHKERRQ(ierr);
        if (!pointIS) continue; /* No points with that id on this process */
        ierr = ISIntersect(pointIS,heightIS,&isectIS);CHKERRQ(ierr);
        ierr = ISDestroy(&pointIS);CHKERRQ(ierr);
        if (!isectIS) continue;
        ierr = ISGetLocalSize(isectIS, &n);CHKERRQ(ierr);
        ierr = ISGetIndices(isectIS, &points);CHKERRQ(ierr);
        ierr = DMFieldGetDegree(coordField,isectIS,NULL,&maxDegree);CHKERRQ(ierr);
        if (maxDegree <= 1) {
          ierr = DMFieldCreateDefaultQuadrature(coordField,isectIS,&quad);CHKERRQ(ierr);
        }
        if (!quad) {
          if (!h && allPoints) {
            quad = allPoints;
            allPoints = NULL;
          } else {
            ierr = PetscDualSpaceGetAllPointsUnion(Nf,sp,dim-h,funcs,&quad);CHKERRQ(ierr);
          }
        }
        ierr = DMFieldCreateFEGeom(coordField,isectIS,quad,PETSC_FALSE,&fegeom);CHKERRQ(ierr);
        for (p = 0; p < n; ++p) {
          const PetscInt  point = points[p];

          ierr = PetscMemzero(values, numValues * sizeof(PetscScalar));CHKERRQ(ierr);
          ierr = PetscFEGeomGetChunk(fegeom,p,p+1,&chunkgeom);CHKERRQ(ierr);
          ierr = DMProjectPoint_Private(dm, probEff, chunkgeom, dmAux, probAux, effectiveHeight, time, localU, localA, hasFE, hasFV, isFE, sp, point, Ncc, comps, basisTab, basisDerTab, basisTabAux, basisDerTabAux, type, funcs, ctxs, fieldActive, values);
          if (ierr) {
            PetscErrorCode ierr2;
            ierr2 = DMRestoreWorkArray(dm, numValues, MPIU_SCALAR, &values);CHKERRQ(ierr2);
            ierr2 = DMRestoreWorkArray(dm, Nf, MPI_INT, &fieldActive);CHKERRQ(ierr2);
            CHKERRQ(ierr);
          }
          ierr = DMPlexVecSetFieldClosure_Internal(dm, section, localX, fieldActive, point, Ncc, comps, values, mode);CHKERRQ(ierr);
        }
        ierr = PetscFEGeomRestoreChunk(fegeom,p,p+1,&chunkgeom);CHKERRQ(ierr);
        ierr = PetscFEGeomDestroy(&fegeom);CHKERRQ(ierr);
        ierr = PetscQuadratureDestroy(&quad);CHKERRQ(ierr);
        ierr = ISRestoreIndices(isectIS, &points);CHKERRQ(ierr);
        ierr = ISDestroy(&isectIS);CHKERRQ(ierr);
      }
    } else {
      PetscFEGeom    *fegeom = NULL, *chunkgeom = NULL;
      PetscQuadrature quad = NULL;
      IS              pointIS;

      ierr = ISCreateStride(PETSC_COMM_SELF,pEnd-pStart,pStart,1,&pointIS);CHKERRQ(ierr);
      ierr = DMFieldGetDegree(coordField,pointIS,NULL,&maxDegree);CHKERRQ(ierr);
      if (maxDegree <= 1) {
        ierr = DMFieldCreateDefaultQuadrature(coordField,pointIS,&quad);CHKERRQ(ierr);
      }
      if (!quad) {
        if (!h && allPoints) {
          quad = allPoints;
          allPoints = NULL;
        } else {
          ierr = PetscDualSpaceGetAllPointsUnion(Nf,sp,dim-h,funcs,&quad);CHKERRQ(ierr);
        }
      }
      ierr = DMFieldCreateFEGeom(coordField,pointIS,quad,PETSC_FALSE,&fegeom);CHKERRQ(ierr);
      for (p = pStart; p < pEnd; ++p) {
        ierr = PetscMemzero(values, numValues * sizeof(PetscScalar));CHKERRQ(ierr);
        ierr = PetscFEGeomGetChunk(fegeom,p-pStart,p-pStart+1,&chunkgeom);CHKERRQ(ierr);
        ierr = DMProjectPoint_Private(dm, probEff, chunkgeom, dmAux, probAux, effectiveHeight, time, localU, localA, hasFE, hasFV, isFE, sp, p, Ncc, comps, basisTab, basisDerTab, basisTabAux, basisDerTabAux, type, funcs, ctxs, fieldActive, values);
        if (ierr) {
          PetscErrorCode ierr2;
          ierr2 = DMRestoreWorkArray(dm, numValues, MPIU_SCALAR, &values);CHKERRQ(ierr2);
          ierr2 = DMRestoreWorkArray(dm, Nf, MPI_INT, &fieldActive);CHKERRQ(ierr2);
          CHKERRQ(ierr);
        }
        ierr = DMPlexVecSetFieldClosure_Internal(dm, section, localX, fieldActive, p, Ncc, comps, values, mode);CHKERRQ(ierr);
      }
      ierr = PetscFEGeomRestoreChunk(fegeom,p-pStart,pStart-p+1,&chunkgeom);CHKERRQ(ierr);
      ierr = PetscFEGeomDestroy(&fegeom);CHKERRQ(ierr);
      ierr = PetscQuadratureDestroy(&quad);CHKERRQ(ierr);
      ierr = ISDestroy(&pointIS);CHKERRQ(ierr);
    }
    ierr = ISDestroy(&heightIS);CHKERRQ(ierr);
    ierr = DMRestoreWorkArray(dm, numValues, MPIU_SCALAR, &values);CHKERRQ(ierr);
    ierr = DMRestoreWorkArray(dm, Nf, MPI_INT, &fieldActive);CHKERRQ(ierr);
  }
  /* Cleanup */
  if (type == DM_BC_ESSENTIAL_FIELD || type == DM_BC_NATURAL_FIELD) {
    PetscInt effectiveHeight = auxBd ? minHeight : 0;
    PetscFE  fem, subfem;

    for (f = 0; f < Nf; ++f) {
      if (!isFE[f]) continue;
      ierr = PetscDSGetDiscretization(prob, f, (PetscObject *) &fem);CHKERRQ(ierr);
      if (!effectiveHeight) {subfem = fem;}
      else                  {ierr = PetscFEGetHeightSubspace(fem, effectiveHeight, &subfem);CHKERRQ(ierr);}
      ierr = PetscFERestoreTabulation(subfem, 0, NULL, &basisTab[f], &basisDerTab[f], NULL);CHKERRQ(ierr);
    }
    for (f = 0; f < NfAux; ++f) {
      ierr = PetscDSGetDiscretization(probAux, f, (PetscObject *) &fem);CHKERRQ(ierr);
      if (!effectiveHeight || auxBd) {subfem = fem;}
      else                           {ierr = PetscFEGetHeightSubspace(fem, effectiveHeight, &subfem);CHKERRQ(ierr);}
      ierr = PetscFERestoreTabulation(subfem, 0, NULL, &basisTabAux[f], &basisDerTabAux[f], NULL);CHKERRQ(ierr);
    }
    ierr = PetscFree4(basisTab, basisDerTab, basisTabAux, basisDerTabAux);CHKERRQ(ierr);
  }
  ierr = PetscQuadratureDestroy(&allPoints);CHKERRQ(ierr);
  ierr = PetscFree2(isFE, sp);CHKERRQ(ierr);
  if (maxHeight > 0) {ierr = PetscFree(cellsp);CHKERRQ(ierr);}
  PetscFunctionReturn(0);
}

PetscErrorCode DMProjectFunctionLocal_Plex(DM dm, PetscReal time, PetscErrorCode (**funcs)(PetscInt, PetscReal, const PetscReal [], PetscInt, PetscScalar *, void *), void **ctxs, InsertMode mode, Vec localX)
{
  PetscErrorCode ierr;

  PetscFunctionBegin;
  ierr = DMProjectLocal_Generic_Plex(dm, time, localX, 0, NULL, NULL, 0, NULL, DM_BC_ESSENTIAL, (void (**)(void)) funcs, ctxs, mode, localX);CHKERRQ(ierr);
  PetscFunctionReturn(0);
}

PetscErrorCode DMProjectFunctionLabelLocal_Plex(DM dm, PetscReal time, DMLabel label, PetscInt numIds, const PetscInt ids[], PetscInt Ncc, const PetscInt comps[], PetscErrorCode (**funcs)(PetscInt, PetscReal, const PetscReal [], PetscInt, PetscScalar *, void *), void **ctxs, InsertMode mode, Vec localX)
{
  PetscErrorCode ierr;

  PetscFunctionBegin;
  ierr = DMProjectLocal_Generic_Plex(dm, time, localX, Ncc, comps, label, numIds, ids, DM_BC_ESSENTIAL, (void (**)(void)) funcs, ctxs, mode, localX);CHKERRQ(ierr);
  PetscFunctionReturn(0);
}

PetscErrorCode DMProjectFieldLocal_Plex(DM dm, PetscReal time, Vec localU,
                                        void (**funcs)(PetscInt, PetscInt, PetscInt,
                                                       const PetscInt[], const PetscInt[], const PetscScalar[], const PetscScalar[], const PetscScalar[],
                                                       const PetscInt[], const PetscInt[], const PetscScalar[], const PetscScalar[], const PetscScalar[],
                                                       PetscReal, const PetscReal[], PetscInt, const PetscScalar[], PetscScalar[]),
                                        InsertMode mode, Vec localX)
{
  PetscErrorCode ierr;

  PetscFunctionBegin;
  ierr = DMProjectLocal_Generic_Plex(dm, time, localU, 0, NULL, NULL, 0, NULL, DM_BC_ESSENTIAL_FIELD, (void (**)(void)) funcs, NULL, mode, localX);CHKERRQ(ierr);
  PetscFunctionReturn(0);
}

PetscErrorCode DMProjectFieldLabelLocal_Plex(DM dm, PetscReal time, DMLabel label, PetscInt numIds, const PetscInt ids[], PetscInt Ncc, const PetscInt comps[], Vec localU,
                                             void (**funcs)(PetscInt, PetscInt, PetscInt,
                                                            const PetscInt[], const PetscInt[], const PetscScalar[], const PetscScalar[], const PetscScalar[],
                                                            const PetscInt[], const PetscInt[], const PetscScalar[], const PetscScalar[], const PetscScalar[],
                                                            PetscReal, const PetscReal[], PetscInt, const PetscScalar[], PetscScalar[]),
                                             InsertMode mode, Vec localX)
{
  PetscErrorCode ierr;

  PetscFunctionBegin;
  ierr = DMProjectLocal_Generic_Plex(dm, time, localU, Ncc, comps, label, numIds, ids, DM_BC_ESSENTIAL_FIELD, (void (**)(void)) funcs, NULL, mode, localX);CHKERRQ(ierr);
  PetscFunctionReturn(0);
}
