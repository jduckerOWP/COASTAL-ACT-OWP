
#include <petsc/private/tsimpl.h>        /*I "petscts.h"  I*/

PetscFunctionList TSTrajectoryList              = NULL;
PetscBool         TSTrajectoryRegisterAllCalled = PETSC_FALSE;
PetscClassId      TSTRAJECTORY_CLASSID;
PetscLogEvent     TSTrajectory_Set, TSTrajectory_Get;

/*@C
  TSTrajectoryRegister - Adds a way of storing trajectories to the TS package

  Not Collective

  Input Parameters:
+ name        - the name of a new user-defined creation routine
- create_func - the creation routine itself

  Notes:
  TSTrajectoryRegister() may be called multiple times to add several user-defined tses.

  Level: developer

.keywords: TS, trajectory, timestep, register

.seealso: TSTrajectoryRegisterAll()
@*/
PetscErrorCode TSTrajectoryRegister(const char sname[],PetscErrorCode (*function)(TSTrajectory,TS))
{
  PetscErrorCode ierr;

  PetscFunctionBegin;
  ierr = PetscFunctionListAdd(&TSTrajectoryList,sname,function);CHKERRQ(ierr);
  PetscFunctionReturn(0);
}

PetscErrorCode TSTrajectorySet(TSTrajectory tj,TS ts,PetscInt stepnum,PetscReal time,Vec X)
{
  PetscErrorCode ierr;

  PetscFunctionBegin;
  if (!tj) PetscFunctionReturn(0);
  ierr = PetscLogEventBegin(TSTrajectory_Set,tj,ts,0,0);CHKERRQ(ierr);
  ierr = (*tj->ops->set)(tj,ts,stepnum,time,X);CHKERRQ(ierr);
  ierr = PetscLogEventEnd(TSTrajectory_Set,tj,ts,0,0);CHKERRQ(ierr);
  PetscFunctionReturn(0);
}

PetscErrorCode TSTrajectoryGet(TSTrajectory tj,TS ts,PetscInt stepnum,PetscReal *time)
{
  PetscErrorCode ierr;

  PetscFunctionBegin;
  if (!tj) SETERRQ(PetscObjectComm((PetscObject)ts),PETSC_ERR_ARG_WRONGSTATE,"TS solver did not save trajectory");
  if (stepnum < 0) SETERRQ(PetscObjectComm((PetscObject)tj),PETSC_ERR_PLIB,"Requesting negative step number");
  ierr = PetscLogEventBegin(TSTrajectory_Get,tj,ts,0,0);CHKERRQ(ierr);
  ierr = (*tj->ops->get)(tj,ts,stepnum,time);CHKERRQ(ierr);
  ierr = PetscLogEventEnd(TSTrajectory_Get,tj,ts,0,0);CHKERRQ(ierr);
  PetscFunctionReturn(0);
}

/*@C
    TSTrajectoryView - Prints information about the trajectory object

    Collective on TSTrajectory

    Input Parameters:
+   tj - the TSTrajectory context obtained from TSTrajectoryCreate()
-   viewer - visualization context

    Options Database Key:
.   -ts_trajectory_view - calls TSTrajectoryView() at end of TSAdjointStep()

    Notes:
    The available visualization contexts include
+     PETSC_VIEWER_STDOUT_SELF - standard output (default)
-     PETSC_VIEWER_STDOUT_WORLD - synchronized standard
         output where only the first processor opens
         the file.  All other processors send their
         data to the first processor to print.

    The user can open an alternative visualization context with
    PetscViewerASCIIOpen() - output to a specified file.

    Level: developer

.keywords: TS, trajectory, timestep, view

.seealso: PetscViewerASCIIOpen()
@*/
PetscErrorCode  TSTrajectoryView(TSTrajectory tj,PetscViewer viewer)
{
  PetscErrorCode ierr;
  PetscBool      iascii;

  PetscFunctionBegin;
  PetscValidHeaderSpecific(tj,TSTRAJECTORY_CLASSID,1);
  if (!viewer) {
    ierr = PetscViewerASCIIGetStdout(PetscObjectComm((PetscObject)tj),&viewer);CHKERRQ(ierr);
  }
  PetscValidHeaderSpecific(viewer,PETSC_VIEWER_CLASSID,2);
  PetscCheckSameComm(tj,1,viewer,2);

  ierr = PetscObjectTypeCompare((PetscObject)viewer,PETSCVIEWERASCII,&iascii);CHKERRQ(ierr);
  if (iascii) {
    ierr = PetscObjectPrintClassNamePrefixType((PetscObject)tj,viewer);CHKERRQ(ierr);
    ierr = PetscViewerASCIIPrintf(viewer,"  total number of recomputations for adjoint calculation = %D\n",tj->recomps);CHKERRQ(ierr);
    ierr = PetscViewerASCIIPrintf(viewer,"  disk checkpoint reads = %D\n",tj->diskreads);CHKERRQ(ierr);
    ierr = PetscViewerASCIIPrintf(viewer,"  disk checkpoint writes = %D\n",tj->diskwrites);CHKERRQ(ierr);
    if (tj->ops->view) {
      ierr = PetscViewerASCIIPushTab(viewer);CHKERRQ(ierr);
      ierr = (*tj->ops->view)(tj,viewer);CHKERRQ(ierr);
      ierr = PetscViewerASCIIPopTab(viewer);CHKERRQ(ierr);
    }
  }
  PetscFunctionReturn(0);
}

/*@C
   TSTrajectorySetVariableNames - Sets the name of each component in the solution vector so that it may be saved with the trajectory

   Collective on TSTrajectory

   Input Parameters:
+  tr - the trajectory context
-  names - the names of the components, final string must be NULL

   Level: intermediate

   Note: Fortran interface is not possible because of the string array argument

.keywords: TS, TSTrajectory, vector, monitor, view

.seealso: TSTrajectory, TSGetTrajectory()
@*/
PetscErrorCode  TSTrajectorySetVariableNames(TSTrajectory ctx,const char * const *names)
{
  PetscErrorCode    ierr;

  PetscFunctionBegin;
  ierr = PetscStrArrayDestroy(&ctx->names);CHKERRQ(ierr);
  ierr = PetscStrArrayallocpy(names,&ctx->names);CHKERRQ(ierr);
  PetscFunctionReturn(0);
}

/*@C
   TSTrjactorySetTransform - Solution vector will be transformed by provided function before being saved to disk

   Collective on TSLGCtx

   Input Parameters:
+  tj - the TSTrajectory context
.  transform - the transform function
.  destroy - function to destroy the optional context
-  ctx - optional context used by transform function

   Level: intermediate

.keywords: TSTrajectory,  vector, monitor, view

.seealso:  TSTrajectorySetVariableNames(), TSTrajectory, TSMonitorLGSetTransform()
@*/
PetscErrorCode  TSTrajectorySetTransform(TSTrajectory tj,PetscErrorCode (*transform)(void*,Vec,Vec*),PetscErrorCode (*destroy)(void*),void *tctx)
{
  PetscFunctionBegin;
  tj->transform        = transform;
  tj->transformdestroy = destroy;
  tj->transformctx     = tctx;
  PetscFunctionReturn(0);
}


/*@
  TSTrajectoryCreate - This function creates an empty trajectory object used to store the time dependent solution of an ODE/DAE

  Collective on MPI_Comm

  Input Parameter:
. comm - the communicator

  Output Parameter:
. tj   - the trajectory object

  Level: developer

  Notes:
    Usually one does not call this routine, it is called automatically when one calls TSSetSaveTrajectory().

.keywords: TS, trajectory, create

.seealso: TSTrajectorySetUp(), TSTrajectoryDestroy(), TSTrajectorySetType(), TSTrajectorySetVariableNames(), TSGetTrajectory(), TSTrajectorySetKeepFiles()
@*/
PetscErrorCode  TSTrajectoryCreate(MPI_Comm comm,TSTrajectory *tj)
{
  TSTrajectory   t;
  PetscErrorCode ierr;

  PetscFunctionBegin;
  PetscValidPointer(tj,2);
  *tj = NULL;
  ierr = TSInitializePackage();CHKERRQ(ierr);

  ierr = PetscHeaderCreate(t,TSTRAJECTORY_CLASSID,"TSTrajectory","Time stepping","TS",comm,TSTrajectoryDestroy,TSTrajectoryView);CHKERRQ(ierr);
  t->setupcalled = PETSC_FALSE;
  t->keepfiles   = PETSC_TRUE;
  *tj  = t;
  ierr = TSTrajectorySetDirname(t,"SA-data");CHKERRQ(ierr);
  ierr = TSTrajectorySetFiletemplate(t,"SA-%06D.bin");CHKERRQ(ierr);
  PetscFunctionReturn(0);
}

/*@C
  TSTrajectorySetType - Sets the storage method to be used as in a trajectory

  Collective on TS

  Input Parameters:
+ tj   - the TSTrajectory context
. ts   - the TS context
- type - a known method

  Options Database Command:
. -ts_trajectory_type <type> - Sets the method; use -help for a list of available methods (for instance, basic)

   Level: developer

.keywords: TS, trajectory, timestep, set, type

.seealso: TS, TSTrajectoryCreate(), TSTrajectorySetFromOptions(), TSTrajectoryDestroy()

@*/
PetscErrorCode  TSTrajectorySetType(TSTrajectory tj,TS ts,TSTrajectoryType type)
{
  PetscErrorCode (*r)(TSTrajectory,TS);
  PetscBool      match;
  PetscErrorCode ierr;

  PetscFunctionBegin;
  PetscValidHeaderSpecific(tj,TSTRAJECTORY_CLASSID,1);
  ierr = PetscObjectTypeCompare((PetscObject)tj,type,&match);CHKERRQ(ierr);
  if (match) PetscFunctionReturn(0);

  ierr = PetscFunctionListFind(TSTrajectoryList,type,&r);CHKERRQ(ierr);
  if (!r) SETERRQ1(PETSC_COMM_SELF,PETSC_ERR_ARG_UNKNOWN_TYPE,"Unknown TSTrajectory type: %s",type);
  if (tj->ops->destroy) {
    ierr = (*(tj)->ops->destroy)(tj);CHKERRQ(ierr);

    tj->ops->destroy = NULL;
  }
  ierr = PetscMemzero(tj->ops,sizeof(*tj->ops));CHKERRQ(ierr);

  ierr = PetscObjectChangeTypeName((PetscObject)tj,type);CHKERRQ(ierr);
  ierr = (*r)(tj,ts);CHKERRQ(ierr);
  PetscFunctionReturn(0);
}

PETSC_EXTERN PetscErrorCode TSTrajectoryCreate_Basic(TSTrajectory,TS);
PETSC_EXTERN PetscErrorCode TSTrajectoryCreate_Singlefile(TSTrajectory,TS);
PETSC_EXTERN PetscErrorCode TSTrajectoryCreate_Memory(TSTrajectory,TS);
PETSC_EXTERN PetscErrorCode TSTrajectoryCreate_Visualization(TSTrajectory,TS);

/*@C
  TSTrajectoryRegisterAll - Registers all of the trajectory storage schecmes in the TS package.

  Not Collective

  Level: developer

.keywords: TS, trajectory, register, all

.seealso: TSTrajectoryRegister()
@*/
PetscErrorCode  TSTrajectoryRegisterAll(void)
{
  PetscErrorCode ierr;

  PetscFunctionBegin;
  if (TSTrajectoryRegisterAllCalled) PetscFunctionReturn(0);
  TSTrajectoryRegisterAllCalled = PETSC_TRUE;

  ierr = TSTrajectoryRegister(TSTRAJECTORYBASIC,TSTrajectoryCreate_Basic);CHKERRQ(ierr);
  ierr = TSTrajectoryRegister(TSTRAJECTORYSINGLEFILE,TSTrajectoryCreate_Singlefile);CHKERRQ(ierr);
  ierr = TSTrajectoryRegister(TSTRAJECTORYMEMORY,TSTrajectoryCreate_Memory);CHKERRQ(ierr);
  ierr = TSTrajectoryRegister(TSTRAJECTORYVISUALIZATION,TSTrajectoryCreate_Visualization);CHKERRQ(ierr);
  PetscFunctionReturn(0);
}

/*@
   TSTrajectoryReset - Resets a trajectory context

   Collective on TSTrajectory

   Input Parameter:
.  tj - the TSTrajectory context obtained from TSTrajectoryCreate()

   Level: developer

.keywords: TS, trajectory, timestep, reset

.seealso: TSTrajectoryCreate(), TSTrajectorySetUp()
@*/
PetscErrorCode TSTrajectoryReset(TSTrajectory tj)
{
  PetscErrorCode ierr;

  PetscFunctionBegin;
  if (!tj) PetscFunctionReturn(0);
  PetscValidHeaderSpecific(tj,TSTRAJECTORY_CLASSID,1);

  if (tj->ops->reset) {
    ierr = (*tj->ops->reset)(tj);CHKERRQ(ierr);
  }
  tj->setupcalled = PETSC_FALSE;
  ierr = PetscFree(tj->dirfiletemplate);CHKERRQ(ierr);
  PetscFunctionReturn(0);
}

/*@
   TSTrajectoryDestroy - Destroys a trajectory context

   Collective on TSTrajectory

   Input Parameter:
.  tj - the TSTrajectory context obtained from TSTrajectoryCreate()

   Level: developer

.keywords: TS, trajectory, timestep, destroy

.seealso: TSTrajectoryCreate(), TSTrajectorySetUp()
@*/
PetscErrorCode TSTrajectoryDestroy(TSTrajectory *tj)
{
  PetscErrorCode ierr;

  PetscFunctionBegin;
  if (!*tj) PetscFunctionReturn(0);
  PetscValidHeaderSpecific((*tj),TSTRAJECTORY_CLASSID,1);
  if (--((PetscObject)(*tj))->refct > 0) {*tj = 0; PetscFunctionReturn(0);}

  ierr = TSTrajectoryReset((*tj));CHKERRQ(ierr);

  if ((*tj)->transformdestroy) {ierr = (*(*tj)->transformdestroy)((*tj)->transformctx);CHKERRQ(ierr);}
  if ((*tj)->ops->destroy) {ierr = (*(*tj)->ops->destroy)((*tj));CHKERRQ(ierr);}
  ierr = PetscViewerDestroy(&(*tj)->monitor);CHKERRQ(ierr);
  ierr = PetscStrArrayDestroy(&(*tj)->names);CHKERRQ(ierr);
  ierr = PetscFree((*tj)->dirname);CHKERRQ(ierr);
  ierr = PetscFree((*tj)->filetemplate);CHKERRQ(ierr);
  ierr = PetscHeaderDestroy(tj);CHKERRQ(ierr);
  PetscFunctionReturn(0);
}

/*
  TSTrajectorySetTypeFromOptions_Private - Sets the type of ts from user options.

  Collective on TSTrajectory

  Input Parameter:
+ tj - the TSTrajectory context
- ts - the TS context

  Options Database Keys:
. -ts_trajectory_type <type> - TSTRAJECTORYBASIC, TSTRAJECTORYMEMORY, TSTRAJECTORYSINGLEFILE, TSTRAJECTORYVISUALIZATION

  Level: developer

.keywords: TS, trajectory, set, options, type

.seealso: TSTrajectorySetFromOptions(), TSTrajectorySetType()
*/
static PetscErrorCode TSTrajectorySetTypeFromOptions_Private(PetscOptionItems *PetscOptionsObject,TSTrajectory tj,TS ts)
{
  PetscBool      opt;
  const char     *defaultType;
  char           typeName[256];
  PetscBool      flg;
  PetscErrorCode ierr;

  PetscFunctionBegin;
  if (((PetscObject)tj)->type_name) defaultType = ((PetscObject)tj)->type_name;
  else defaultType = TSTRAJECTORYBASIC;

  ierr = TSTrajectoryRegisterAll();CHKERRQ(ierr);
  ierr = PetscOptionsFList("-ts_trajectory_type","TSTrajectory method","TSTrajectorySetType",TSTrajectoryList,defaultType,typeName,256,&opt);CHKERRQ(ierr);
  if (opt) {
    ierr = PetscStrcmp(typeName,TSTRAJECTORYMEMORY,&flg);CHKERRQ(ierr);
    ierr = TSTrajectorySetType(tj,ts,typeName);CHKERRQ(ierr);
  } else {
    ierr = TSTrajectorySetType(tj,ts,defaultType);CHKERRQ(ierr);
  }
  PetscFunctionReturn(0);
}

/*@
   TSTrajectorySetMonitor - Monitor the schedules generated by the checkpointing controller

   Collective on TSTrajectory

   Input Arguments:
+  tj - the TSTrajectory context
-  flg - PETSC_TRUE to active a monitor, PETSC_FALSE to disable

   Options Database Keys:
.  -ts_trajectory_monitor - print TSTrajectory information

   Level: developer

.keywords: TS, trajectory, set, monitor

.seealso: TSTrajectoryCreate(), TSTrajectoryDestroy(), TSTrajectorySetUp()
@*/
PetscErrorCode TSTrajectorySetMonitor(TSTrajectory tj,PetscBool flg)
{
  PetscErrorCode ierr;

  PetscFunctionBegin;
  PetscValidHeaderSpecific(tj,TSTRAJECTORY_CLASSID,1);
  PetscValidLogicalCollectiveBool(tj,flg,2);
  if (flg) {
    if (!tj->monitor) {ierr = PetscViewerASCIIOpen(PetscObjectComm((PetscObject)tj),"stdout",&tj->monitor);CHKERRQ(ierr);}
  } else {
    ierr = PetscViewerDestroy(&tj->monitor);CHKERRQ(ierr);
  }
  PetscFunctionReturn(0);
}

/*@
   TSTrajectorySetKeepFiles - Keep the files generated by the TSTrajectory

   Collective on TSTrajectory

   Input Arguments:
+  tj - the TSTrajectory context
-  flg - PETSC_TRUE to save, PETSC_FALSE to disable

   Options Database Keys:
.  -ts_trajectory_keep_files - have it keep the files

   Notes:
    By default the TSTrajectory used for adjoint computations, TSTRAJECTORYBASIC, removes the files it generates at the end of the run. This causes the files to be kept.

   Level: advanced

.keywords: TS, trajectory, set, monitor

.seealso: TSTrajectoryCreate(), TSTrajectoryDestroy(), TSTrajectorySetUp(), TSTrajectorySetMonitor()
@*/
PetscErrorCode TSTrajectorySetKeepFiles(TSTrajectory tj,PetscBool flg)
{
  PetscFunctionBegin;
  PetscValidHeaderSpecific(tj,TSTRAJECTORY_CLASSID,1);
  PetscValidLogicalCollectiveBool(tj,flg,2);
  tj->keepfiles = flg;
  PetscFunctionReturn(0);
}

/*@C
   TSTrajectorySetDirname - Specify the name of the directory where disk checkpoints are stored.

   Collective on TSTrajectory

   Input Arguments:
+  tj      - the TSTrajectory context
-  dirname - the directory name

   Options Database Keys:
.  -ts_trajectory_dirname - set the directory name

   Notes:
    The final location of the files is determined by dirname/filetemplate where filetemplate was provided by TSTrajectorySetFiletemplate()

   Level: developer

.keywords: TS, trajectory, set

.seealso: TSTrajectorySetFiletemplate(),TSTrajectorySetUp()
@*/
PetscErrorCode TSTrajectorySetDirname(TSTrajectory tj,const char dirname[])
{
  PetscErrorCode ierr;
  PetscFunctionBegin;
  PetscValidHeaderSpecific(tj,TSTRAJECTORY_CLASSID,1);
  if (tj->dirfiletemplate) SETERRQ(PetscObjectComm((PetscObject)tj),PETSC_ERR_ARG_WRONGSTATE,"Cannot set directoryname after it TSTrajectory has been setup");
  ierr = PetscFree(tj->dirname);CHKERRQ(ierr);
  ierr = PetscStrallocpy(dirname,&tj->dirname);CHKERRQ(ierr);
  PetscFunctionReturn(0);
}

/*@C
   TSTrajectorySetFiletemplate - Specify the name template for the files storing checkpoints.

   Collective on TSTrajectory

   Input Arguments:
+  tj      - the TSTrajectory context
-  filetemplate - the template

   Options Database Keys:
.  -ts_trajectory_file_template - set the file name template

   Notes:
    The name template should be of the form, for example filename-%06D.bin It should not begin with a leading /

   The final location of the files is determined by dirname/filetemplate where dirname was provided by TSTrajectorySetDirname(). The %06D is replaced by the 
   timestep counter

   Level: developer

.keywords: TS, trajectory, set

.seealso: TSTrajectorySetDirname(),TSTrajectorySetUp()
@*/
PetscErrorCode TSTrajectorySetFiletemplate(TSTrajectory tj,const char filetemplate[])
{
  PetscErrorCode ierr;
  const char     *ptr,*ptr2;

  PetscFunctionBegin;
  PetscValidHeaderSpecific(tj,TSTRAJECTORY_CLASSID,1);
  if (tj->dirfiletemplate) SETERRQ(PetscObjectComm((PetscObject)tj),PETSC_ERR_ARG_WRONGSTATE,"Cannot set filetemplate after TSTrajectory has been setup");

  if (!filetemplate[0]) SETERRQ(PetscObjectComm((PetscObject)tj),PETSC_ERR_USER,"-ts_trajectory_file_template requires a file name template, e.g. filename-%%06D.bin");
  /* Do some cursory validation of the input. */
  ierr = PetscStrstr(filetemplate,"%",(char**)&ptr);CHKERRQ(ierr);
  if (!ptr) SETERRQ(PetscObjectComm((PetscObject)tj),PETSC_ERR_USER,"-ts_trajectory_file_template requires a file name template, e.g. filename-%%06D.bin");
  for (ptr++; ptr && *ptr; ptr++) {
    ierr = PetscStrchr("DdiouxX",*ptr,(char**)&ptr2);CHKERRQ(ierr);
    if (!ptr2 && (*ptr < '0' || '9' < *ptr)) SETERRQ(PetscObjectComm((PetscObject)tj),PETSC_ERR_USER,"Invalid file template argument to -ts_trajectory_file_template, should look like filename-%%06D.bin");
    if (ptr2) break;
  }
  ierr = PetscFree(tj->filetemplate);CHKERRQ(ierr);
  ierr = PetscStrallocpy(filetemplate,&tj->filetemplate);CHKERRQ(ierr);
  PetscFunctionReturn(0);
}

/*@
   TSTrajectorySetFromOptions - Sets various TSTrajectory parameters from user options.

   Collective on TSTrajectory

   Input Parameter:
+  tj - the TSTrajectory context obtained from TSTrajectoryCreate()
-  ts - the TS context

   Options Database Keys:
+  -ts_trajectory_type <type> - TSTRAJECTORYBASIC, TSTRAJECTORYMEMORY, TSTRAJECTORYSINGLEFILE, TSTRAJECTORYVISUALIZATION
.  -ts_trajectory_keep_files <true,false> - keep the files generated by the code after the program ends. This is true by default for TSTRAJECTORYSINGLEFILE, TSTRAJECTORYVISUALIZATION
-  -ts_trajectory_monitor - print TSTrajectory information

   Level: developer

   Notes:
    This is not normally called directly by users

.keywords: TS, trajectory, timestep, set, options, database

.seealso: TSSetSaveTrajectory(), TSTrajectorySetUp()
@*/
PetscErrorCode  TSTrajectorySetFromOptions(TSTrajectory tj,TS ts)
{
  PetscBool      set,flg;
  char           dirname[PETSC_MAX_PATH_LEN],filetemplate[PETSC_MAX_PATH_LEN];
  PetscErrorCode ierr;

  PetscFunctionBegin;
  PetscValidHeaderSpecific(tj,TSTRAJECTORY_CLASSID,1);
  PetscValidHeaderSpecific(ts,TS_CLASSID,2);
  ierr = PetscObjectOptionsBegin((PetscObject)tj);CHKERRQ(ierr);
  ierr = TSTrajectorySetTypeFromOptions_Private(PetscOptionsObject,tj,ts);CHKERRQ(ierr);
  ierr = PetscOptionsBool("-ts_trajectory_monitor","Print checkpointing schedules","TSTrajectorySetMonitor",tj->monitor ? PETSC_TRUE:PETSC_FALSE,&flg,&set);CHKERRQ(ierr);
  if (set) {ierr = TSTrajectorySetMonitor(tj,flg);CHKERRQ(ierr);}

  ierr = PetscOptionsBool("-ts_trajectory_keep_files","Keep any trajectory files generated during the run","TSTrajectorySetKeepFiles",tj->keepfiles,&flg,&set);CHKERRQ(ierr);
  if (set) {ierr = TSTrajectorySetKeepFiles(tj,flg);CHKERRQ(ierr);}

  ierr = PetscOptionsString("-ts_trajectory_dirname","Directory name for TSTrajectory file","TSTrajectorySetDirname",0,dirname,PETSC_MAX_PATH_LEN-14,&set);CHKERRQ(ierr);
  if (set) {
    ierr = TSTrajectorySetDirname(tj,dirname);CHKERRQ(ierr);
  }

  ierr = PetscOptionsString("-ts_trajectory_file_template","Template for TSTrajectory file name, use filename-%06D.bin","TSTrajectorySetFiletemplate",0,filetemplate,PETSC_MAX_PATH_LEN,&set);CHKERRQ(ierr);
  if (set) {
    ierr = TSTrajectorySetFiletemplate(tj,filetemplate);CHKERRQ(ierr);
  }

  /* Handle specific TSTrajectory options */
  if (tj->ops->setfromoptions) {
    ierr = (*tj->ops->setfromoptions)(PetscOptionsObject,tj);CHKERRQ(ierr);
  }
  ierr = PetscOptionsEnd();CHKERRQ(ierr);
  PetscFunctionReturn(0);
}

/*@
   TSTrajectorySetUp - Sets up the internal data structures, e.g. stacks, for the later use
   of a TS trajectory.

   Collective on TS

   Input Parameter:
+  ts - the TS context obtained from TSCreate()
-  tj - the TS trajectory context

   Level: developer

.keywords: TS, trajectory, setup

.seealso: TSSetSaveTrajectory(), TSTrajectoryCreate(), TSTrajectoryDestroy()
@*/
PetscErrorCode  TSTrajectorySetUp(TSTrajectory tj,TS ts)
{
  PetscErrorCode ierr;
  size_t         s1,s2;

  PetscFunctionBegin;
  if (!tj) PetscFunctionReturn(0);
  PetscValidHeaderSpecific(tj,TSTRAJECTORY_CLASSID,1);
  PetscValidHeaderSpecific(ts,TS_CLASSID,2);
  if (tj->setupcalled) PetscFunctionReturn(0);

  if (!((PetscObject)tj)->type_name) {
    ierr = TSTrajectorySetType(tj,ts,TSTRAJECTORYBASIC);CHKERRQ(ierr);
  }
  if (tj->ops->setup) {
    ierr = (*tj->ops->setup)(tj,ts);CHKERRQ(ierr);
  }

  tj->setupcalled = PETSC_TRUE;

  /* Set the counters to zero */
  tj->recomps    = 0;
  tj->diskreads  = 0;
  tj->diskwrites = 0;
  ierr = PetscStrlen(tj->dirname,&s1);CHKERRQ(ierr);
  ierr = PetscStrlen(tj->filetemplate,&s2);CHKERRQ(ierr);
  ierr = PetscMalloc((s1 + s2 + 10)*sizeof(char),&tj->dirfiletemplate);CHKERRQ(ierr);
  ierr = PetscSNPrintf(tj->dirfiletemplate,s1+s2+10,"%s/%s",tj->dirname,tj->filetemplate);CHKERRQ(ierr);
  PetscFunctionReturn(0);
}
