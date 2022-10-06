#include <petsc/private/petscfeimpl.h> /*I "petscfe.h" I*/

PetscErrorCode PetscFEGeomCreate(PetscQuadrature quad, PetscInt numCells, PetscInt dimEmbed, PetscBool faceData, PetscFEGeom **geom)
{
  PetscFEGeom     *g;
  PetscInt        dim, Nq, N;
  const PetscReal *p;
  PetscErrorCode  ierr;

  PetscFunctionBegin;
  ierr = PetscQuadratureGetData(quad,&dim,NULL,&Nq,&p,NULL);CHKERRQ(ierr);
  ierr = PetscNew(&g);CHKERRQ(ierr);
  g->xi        = p;
  g->numCells  = numCells;
  g->numPoints = Nq;
  g->dim       = dim;
  g->dimEmbed  = dimEmbed;
  N = numCells * Nq;
  ierr = PetscCalloc3(N * dimEmbed, &g->v, N * dimEmbed * dimEmbed, &g->J, N, &g->detJ);CHKERRQ(ierr);
  if (faceData) {
    ierr = PetscCalloc4(numCells, &g->face, N * dimEmbed, &g->n, N * dimEmbed * dimEmbed, &(g->suppInvJ[0]), N * dimEmbed * dimEmbed, &(g->suppInvJ[1]));CHKERRQ(ierr);
  } else {
    ierr = PetscCalloc1(N * dimEmbed * dimEmbed, &g->invJ);CHKERRQ(ierr);
  }
  *geom = g;
  PetscFunctionReturn(0);
}

PetscErrorCode PetscFEGeomDestroy(PetscFEGeom **geom)
{
  PetscErrorCode ierr;

  PetscFunctionBegin;
  if (!*geom) PetscFunctionReturn(0);
  ierr = PetscFree3((*geom)->v,(*geom)->J,(*geom)->detJ);CHKERRQ(ierr);
  ierr = PetscFree((*geom)->invJ);CHKERRQ(ierr);
  ierr = PetscFree4((*geom)->face,(*geom)->n,(*geom)->suppInvJ[0],(*geom)->suppInvJ[1]);CHKERRQ(ierr);
  ierr = PetscFree(*geom);CHKERRQ(ierr);
  PetscFunctionReturn(0);
}

PetscErrorCode PetscFEGeomGetChunk(PetscFEGeom *geom, PetscInt cStart, PetscInt cEnd, PetscFEGeom **chunkGeom)
{
  PetscInt       Nq;
  PetscInt       dE;
  PetscErrorCode ierr;

  PetscFunctionBegin;
  PetscValidPointer(geom,1);
  PetscValidPointer(chunkGeom,2);
  if (!(*chunkGeom)) {
    ierr = PetscNew(chunkGeom);CHKERRQ(ierr);
  }
  Nq = geom->numPoints;
  dE= geom->dimEmbed;
  (*chunkGeom)->dim = geom->dim;
  (*chunkGeom)->dimEmbed = geom->dimEmbed;
  (*chunkGeom)->numPoints = geom->numPoints;
  (*chunkGeom)->numCells = cEnd - cStart;
  (*chunkGeom)->xi = geom->xi;
  (*chunkGeom)->v = &geom->v[Nq*dE*cStart];
  (*chunkGeom)->J = &geom->J[Nq*dE*dE*cStart];
  (*chunkGeom)->invJ = (geom->invJ) ? &geom->invJ[Nq*dE*dE*cStart] : NULL;
  (*chunkGeom)->detJ = &geom->detJ[Nq*cStart];
  (*chunkGeom)->n = geom->n ? &geom->n[Nq*dE*cStart] : NULL;
  (*chunkGeom)->face = geom->face ? &geom->face[cStart] : NULL;
  (*chunkGeom)->suppInvJ[0] = geom->suppInvJ[0] ? &geom->suppInvJ[0][Nq*dE*dE*cStart] : NULL;
  (*chunkGeom)->suppInvJ[1] = geom->suppInvJ[1] ? &geom->suppInvJ[1][Nq*dE*dE*cStart] : NULL;
  (*chunkGeom)->isAffine = geom->isAffine;
  PetscFunctionReturn(0);
}

PetscErrorCode PetscFEGeomRestoreChunk(PetscFEGeom *geom, PetscInt cStart, PetscInt cEnd, PetscFEGeom **chunkGeom)
{
  PetscErrorCode ierr;

  PetscFunctionBegin;
  ierr = PetscFree(*chunkGeom);CHKERRQ(ierr);
  PetscFunctionReturn(0);
}

PetscErrorCode PetscFEGeomComplete(PetscFEGeom *geom)
{
  PetscInt i, j, N, dE;

  PetscFunctionBeginHot;
  N = geom->numPoints * geom->numCells;
  dE = geom->dimEmbed;
  switch (dE) {
  case 3:
    for (i = 0; i < N; i++) {
      DMPlex_Det3D_Internal(&geom->detJ[i], &geom->J[dE*dE*i]);
      if (geom->invJ) DMPlex_Invert3D_Internal(&geom->invJ[dE*dE*i], &geom->J[dE*dE*i], geom->detJ[i]);
    }
    break;
  case 2:
    for (i = 0; i < N; i++) {
      DMPlex_Det2D_Internal(&geom->detJ[i], &geom->J[dE*dE*i]);
      if (geom->invJ) DMPlex_Invert2D_Internal(&geom->invJ[dE*dE*i], &geom->J[dE*dE*i], geom->detJ[i]);
    }
    break;
  case 1:
    for (i = 0; i < N; i++) {
      geom->detJ[i] = PetscAbsReal(geom->J[i]);
      if (geom->invJ) geom->invJ[i] = 1. / geom->J[i];
    }
    break;
  }
  if (geom->n) {
    for (i = 0; i < N; i++) {
      for (j = 0; j < dE; j++) {
        geom->n[dE*i + j] = geom->J[dE*dE*i + dE*j + dE-1] * ((dE == 2) ? -1. : 1.);
      }
    }
  }
  PetscFunctionReturn(0);
}

