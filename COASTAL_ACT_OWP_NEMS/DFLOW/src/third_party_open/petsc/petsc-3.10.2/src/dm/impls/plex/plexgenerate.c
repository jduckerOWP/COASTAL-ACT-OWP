#include <petsc/private/dmpleximpl.h>   /*I      "petscdmplex.h"   I*/

PetscErrorCode DMPlexInvertCell_Internal(PetscInt dim, PetscInt numCorners, PetscInt cone[])
{
  int tmpc;

  PetscFunctionBegin;
  if (dim != 3) PetscFunctionReturn(0);
  switch (numCorners) {
  case 4:
    tmpc    = cone[0];
    cone[0] = cone[1];
    cone[1] = tmpc;
    break;
  case 8:
    tmpc    = cone[1];
    cone[1] = cone[3];
    cone[3] = tmpc;
    break;
  default: break;
  }
  PetscFunctionReturn(0);
}

/*@C
  DMPlexInvertCell - This flips tetrahedron and hexahedron orientation since Plex stores them internally with outward normals. Other cells are left untouched.

  Input Parameters:
+ numCorners - The number of vertices in a cell
- cone - The incoming cone

  Output Parameter:
. cone - The inverted cone (in-place)

  Level: developer

.seealso: DMPlexGenerate()
@*/
PetscErrorCode DMPlexInvertCell(PetscInt dim, PetscInt numCorners, int cone[])
{
  int tmpc;

  PetscFunctionBegin;
  if (dim != 3) PetscFunctionReturn(0);
  switch (numCorners) {
  case 4:
    tmpc    = cone[0];
    cone[0] = cone[1];
    cone[1] = tmpc;
    break;
  case 8:
    tmpc    = cone[1];
    cone[1] = cone[3];
    cone[3] = tmpc;
    break;
  default: break;
  }
  PetscFunctionReturn(0);
}


/*@C
  DMPlexTriangleSetOptions - Set the options used for the Triangle mesh generator

  Not Collective

  Inputs Parameters:
+ dm - The DMPlex object
- opts - The command line options

  Level: developer

.keywords: mesh, points
.seealso: DMPlexTetgenSetOptions(), DMPlexGenerate()
@*/
PetscErrorCode DMPlexTriangleSetOptions(DM dm, const char *opts)
{
  DM_Plex       *mesh = (DM_Plex*) dm->data;
  PetscErrorCode ierr;

  PetscFunctionBegin;
  PetscValidHeaderSpecific(dm, DM_CLASSID, 1);
  PetscValidPointer(opts, 2);
  ierr = PetscFree(mesh->triangleOpts);CHKERRQ(ierr);
  ierr = PetscStrallocpy(opts, &mesh->triangleOpts);CHKERRQ(ierr);
  PetscFunctionReturn(0);
}

/*@C
  DMPlexTetgenSetOptions - Set the options used for the Tetgen mesh generator

  Not Collective

  Inputs Parameters:
+ dm - The DMPlex object
- opts - The command line options

  Level: developer

.keywords: mesh, points
.seealso: DMPlexTriangleSetOptions(), DMPlexGenerate()
@*/
PetscErrorCode DMPlexTetgenSetOptions(DM dm, const char *opts)
{
  DM_Plex       *mesh = (DM_Plex*) dm->data;
  PetscErrorCode ierr;

  PetscFunctionBegin;
  PetscValidHeaderSpecific(dm, DM_CLASSID, 1);
  PetscValidPointer(opts, 2);
  ierr = PetscFree(mesh->tetgenOpts);CHKERRQ(ierr);
  ierr = PetscStrallocpy(opts, &mesh->tetgenOpts);CHKERRQ(ierr);
  PetscFunctionReturn(0);
}

/*
   Contains the list of registered DMPlexGenerators routines
*/
PetscFunctionList DMPlexGenerateList = NULL;

struct _n_PetscFunctionList {
  PetscErrorCode    (*generate)(DM, PetscBool, DM*);
  PetscErrorCode    (*refine)(DM,double*, DM*);
  char              *name;               /* string to identify routine */
  PetscInt          dim;
  PetscFunctionList next;                /* next pointer */
};

/*@C
  DMPlexGenerate - Generates a mesh.

  Not Collective

  Input Parameters:
+ boundary - The DMPlex boundary object
. name - The mesh generation package name
- interpolate - Flag to create intermediate mesh elements

  Output Parameter:
. mesh - The DMPlex object

  Level: intermediate

.keywords: mesh, elements
.seealso: DMPlexCreate(), DMRefine()
@*/
PetscErrorCode DMPlexGenerate(DM boundary, const char name[], PetscBool interpolate, DM *mesh)
{
  PetscInt          dim;
  char              genname[1024];
  PetscBool         flg;
  PetscErrorCode    ierr;
  PetscFunctionList fl;

  PetscFunctionBegin;
  PetscValidHeaderSpecific(boundary, DM_CLASSID, 1);
  PetscValidLogicalCollectiveBool(boundary, interpolate, 2);
  ierr = DMGetDimension(boundary, &dim);CHKERRQ(ierr);
  ierr = PetscOptionsGetString(((PetscObject) boundary)->options,((PetscObject) boundary)->prefix, "-dm_plex_generator", genname, 1024, &flg);CHKERRQ(ierr);
  if (flg) name = genname;

  fl = DMPlexGenerateList;
  if (name) {
    while (fl) {
      ierr = PetscStrcmp(fl->name,name,&flg);CHKERRQ(ierr);
      if (flg) {
        (*fl->generate)(boundary,interpolate,mesh);CHKERRQ(ierr);
        PetscFunctionReturn(0);
      }
      fl = fl->next;
    }
    SETERRQ1(PETSC_COMM_SELF,PETSC_ERR_ARG_OUTOFRANGE,"Grid generator %g not registered",name);CHKERRQ(ierr);
  } else {
    while (fl) {
      if (boundary->dim == fl->dim) {
        (*fl->generate)(boundary,interpolate,mesh);CHKERRQ(ierr);
        PetscFunctionReturn(0);
      }
      fl = fl->next;
    }
    SETERRQ1(PETSC_COMM_SELF,PETSC_ERR_ARG_OUTOFRANGE,"No grid generator of dimension %D registered",boundary->dim);CHKERRQ(ierr);
  }
  PetscFunctionReturn(0);
}

/*@C
  DMPlexGenerateRegister -  Adds a grid generator to DMPlex

   Not Collective

   Input Parameters:
+  name_solver - name of a new user-defined grid generator
.  fnc - generator function
.  rfnc - refinement function
-  dim - dimension of boundary of domain

   Notes:
   DMPlexGenerateRegister() may be called multiple times to add several user-defined solvers.

   Sample usage:
.vb
   DMPlexGenerateRegister("my_generator",MyGeneratorCreate,MyGeneratorRefiner,dim);
.ve

   Then, your generator can be chosen with the procedural interface via
$     DMPlexGenerate(dm,"my_generator",...)
   or at runtime via the option
$     -dm_plex_generator my_generator

   Level: advanced

.keywords: DMPlexGenerate, register

.seealso: DMPlexGenerateRegisterAll(), DMPlexGenerate(), DMPlexGenerateRegisterDestroy()

@*/
PetscErrorCode  DMPlexGenerateRegister(const char sname[],PetscErrorCode (*fnc)(DM, PetscBool,DM*), PetscErrorCode (*rfnc)(DM, double*,DM*),PetscInt dim)
{
  PetscErrorCode    ierr;
  PetscFunctionList entry;

  PetscFunctionBegin;
  ierr            = PetscNew(&entry);CHKERRQ(ierr);
  ierr            = PetscStrallocpy(sname,&entry->name);CHKERRQ(ierr);
  entry->generate = fnc;
  entry->refine   = rfnc;
  entry->dim      = dim;
  entry->next     = NULL;
  if (!DMPlexGenerateList) DMPlexGenerateList = entry;
  else {
    PetscFunctionList fl = DMPlexGenerateList;
    while (fl->next) fl = fl->next;
    fl->next = entry;
  }
  PetscFunctionReturn(0);
}

extern PetscBool DMPlexGenerateRegisterAllCalled;

PetscErrorCode  DMPlexGenerateRegisterDestroy(void)
{
  PetscFunctionList next,fl;
  PetscErrorCode    ierr;

  PetscFunctionBegin;
  next = fl =  DMPlexGenerateList;
    while (next) {
    next = fl ? fl->next : NULL;
    if (fl) {ierr = PetscFree(fl->name);CHKERRQ(ierr);}
    ierr = PetscFree(fl);CHKERRQ(ierr);
    fl = next;
  }
  DMPlexGenerateList              = NULL;
  DMPlexGenerateRegisterAllCalled = PETSC_FALSE;
  PetscFunctionReturn(0);
}
