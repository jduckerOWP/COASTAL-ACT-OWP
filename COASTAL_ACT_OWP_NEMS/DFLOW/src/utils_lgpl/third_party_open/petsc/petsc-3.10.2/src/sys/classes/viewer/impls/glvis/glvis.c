#define PETSC_DESIRE_FEATURE_TEST_MACROS /* for fdopen() */

#include <petsc/private/viewerimpl.h> /*I   "petscviewer.h" I*/
#include <petsc/private/petscimpl.h>  /*I   "petscsys.h"    I*/
#include <petsc/private/glvisviewerimpl.h>

/* we may eventually make this function public */
static PetscErrorCode PetscViewerASCIISocketOpen(MPI_Comm,const char*,PetscInt,PetscViewer*);

struct _n_PetscViewerGLVis {
  PetscViewerGLVisStatus status;
  PetscViewerGLVisType   type;                                                  /* either PETSC_VIEWER_GLVIS_DUMP or PETSC_VIEWER_GLVIS_SOCKET */
  char                   *name;                                                 /* prefix for filename, or hostname, depending on the type */
  PetscInt               port;                                                  /* used just for the socket case */
  PetscReal              pause;                                                 /* if positive, calls PetscSleep(pause) after each VecView_GLVis call */
  PetscViewer            meshwindow;                                            /* used just by the ASCII dumping */
  PetscObject            dm;                                                    /* DM as passed by PetscViewerGLVisSetDM_Private(): should contain discretization info */
  PetscInt               nwindow;                                               /* number of windows/fields to be visualized */
  PetscViewer            *window;
  char                   **windowtitle;
  char                   **fec_type;                                            /* type of elements to be used for visualization, see FiniteElementCollection::Name() */
  PetscErrorCode         (*g2lfield)(PetscObject,PetscInt,PetscObject[],void*); /* global to local operation for generating dofs to be visualized */
  PetscInt               *spacedim;                                             /* geometrical space dimension (just used to initialize the scene) */
  PetscObject            *Ufield;                                               /* work vectors for visualization */
  PetscInt               snapid;                                                /* snapshot id, use PetscViewerGLVisSetSnapId to change this value*/
  void                   *userctx;                                              /* User context, used by g2lfield */
  PetscErrorCode         (*destroyctx)(void*);                                  /* destroy routine for userctx */
  char*                  fmt;                                                   /* format string for FP values */
};
typedef struct _n_PetscViewerGLVis *PetscViewerGLVis;

/*@
     PetscViewerGLVisSetPrecision - Set the number of digits for floating point values

  Not Collective

  Input Parameters:
+  viewer - the PetscViewer
-  prec   - the number of digits required

  Level: beginner

.seealso: PetscViewerGLVisOpen(), PetscViewerGLVisSetFields(), PetscViewerCreate(), PetscViewerSetType()
@*/
PetscErrorCode PetscViewerGLVisSetPrecision(PetscViewer viewer, PetscInt prec)
{
  PetscErrorCode ierr;

  PetscFunctionBegin;
  PetscValidHeaderSpecific(viewer,PETSC_VIEWER_CLASSID,1);
  ierr = PetscTryMethod(viewer,"PetscViewerGLVisSetPrecision_C",(PetscViewer,PetscInt),(viewer,prec));CHKERRQ(ierr);
  PetscFunctionReturn(0);
}

static PetscErrorCode PetscViewerGLVisSetPrecision_GLVis(PetscViewer viewer, PetscInt prec)
{
  PetscErrorCode   ierr;
  PetscViewerGLVis socket = (PetscViewerGLVis)viewer->data;

  PetscFunctionBegin;
  ierr = PetscFree(socket->fmt);CHKERRQ(ierr);
  if (prec > 0) {
    ierr = PetscMalloc1(16,&socket->fmt);CHKERRQ(ierr);
    ierr = PetscSNPrintf(socket->fmt,16," %%.%De",prec);CHKERRQ(ierr);
  } else {
    ierr = PetscStrallocpy(" %g",&socket->fmt);CHKERRQ(ierr);
  }
  PetscFunctionReturn(0);
}

/*@
     PetscViewerGLVisSetSnapId - Set the snapshot id. Only relevant when the viewer is of type PETSC_VIEWER_GLVIS_DUMP

  Logically Collective on PetscViewer

  Input Parameters:
+  viewer - the PetscViewer
-  id     - the current snapshot id in a time-dependent simulation

  Level: beginner

.seealso: PetscViewerGLVisOpen(), PetscViewerGLVisSetFields(), PetscViewerCreate(), PetscViewerSetType()
@*/
PetscErrorCode PetscViewerGLVisSetSnapId(PetscViewer viewer, PetscInt id)
{
  PetscErrorCode ierr;

  PetscFunctionBegin;
  PetscValidHeaderSpecific(viewer,PETSC_VIEWER_CLASSID,1);
  PetscValidLogicalCollectiveInt(viewer,id,2);
  ierr = PetscTryMethod(viewer,"PetscViewerGLVisSetSnapId_C",(PetscViewer,PetscInt),(viewer,id));CHKERRQ(ierr);
  PetscFunctionReturn(0);
}

static PetscErrorCode PetscViewerGLVisSetSnapId_GLVis(PetscViewer viewer, PetscInt id)
{
  PetscViewerGLVis socket = (PetscViewerGLVis)viewer->data;

  PetscFunctionBegin;
  socket->snapid = id;
  PetscFunctionReturn(0);
}

/*@C
     PetscViewerGLVisSetFields - Sets the required information to visualize different fields from a vector.

  Logically Collective on PetscViewer

  Input Parameters:
+  viewer     - the PetscViewer
.  nf         - number of fields to be visualized
.  fec_type   - the type of finite element to be used to visualize the data (see FiniteElementCollection::Name() in MFEM)
.  dim        - array of space dimension for field vectors (used to initialize the scene)
.  g2lfields  - User routine to compute the local field vectors to be visualized; PetscObject is used in place of Vec on the prototype
.  Vfield     - array of work vectors, one for each field
.  ctx        - User context to store the relevant data to apply g2lfields
-  destroyctx - Destroy function for userctx

  Notes:
    g2lfields is called on the vector V to be visualized in order to extract the relevant dofs to be put in Vfield[], as
.vb
  g2lfields((PetscObject)V,nfields,(PetscObject*)Vfield[],ctx).
.ve
  For vector spaces, the block size of Vfield[i] represents the vector dimension. It misses the Fortran bindings.
  The names of the Vfield vectors will be displayed in the window title.

  Level: intermediate

.seealso: PetscViewerGLVisOpen(), PetscViewerCreate(), PetscViewerSetType(), PetscObjectSetName()
@*/
PetscErrorCode PetscViewerGLVisSetFields(PetscViewer viewer, PetscInt nf, const char* fec_type[], PetscInt dim[], PetscErrorCode(*g2l)(PetscObject,PetscInt,PetscObject[],void*), PetscObject Vfield[], void* ctx, PetscErrorCode(*destroyctx)(void*))
{
  PetscErrorCode ierr;

  PetscFunctionBegin;
  PetscValidHeaderSpecific(viewer,PETSC_VIEWER_CLASSID,1);
  PetscValidLogicalCollectiveInt(viewer,nf,2);
  if (!fec_type) SETERRQ(PetscObjectComm((PetscObject)viewer),PETSC_ERR_SUP,"You need to provide the FiniteElementCollection names for the fields");
  PetscValidPointer(fec_type,3);
  PetscValidPointer(dim,4);
  PetscValidPointer(Vfield,6);
  ierr = PetscTryMethod(viewer,"PetscViewerGLVisSetFields_C",(PetscViewer,PetscInt,const char*[],PetscInt[],PetscErrorCode(*)(PetscObject,PetscInt,PetscObject[],void*),PetscObject[],void*,PetscErrorCode(*)(void*)),(viewer,nf,fec_type,dim,g2l,Vfield,ctx,destroyctx));CHKERRQ(ierr);
  PetscFunctionReturn(0);
}

static PetscErrorCode PetscViewerGLVisSetFields_GLVis(PetscViewer viewer, PetscInt nfields, const char* fec_type[], PetscInt dim[], PetscErrorCode(*g2l)(PetscObject,PetscInt,PetscObject[],void*), PetscObject Vfield[], void* ctx, PetscErrorCode(*destroyctx)(void*))
{
  PetscViewerGLVis socket = (PetscViewerGLVis)viewer->data;
  PetscInt         i;
  PetscErrorCode   ierr;

  PetscFunctionBegin;
  if (socket->nwindow && socket->nwindow != nfields) SETERRQ2(PetscObjectComm((PetscObject)viewer),PETSC_ERR_USER,"Cannot set number of fields %D with number of windows %D",nfields,socket->nwindow);
  if (!socket->nwindow) {
    socket->nwindow = nfields;

    ierr = PetscCalloc5(nfields,&socket->window,nfields,&socket->windowtitle,nfields,&socket->fec_type,nfields,&socket->spacedim,nfields,&socket->Ufield);CHKERRQ(ierr);
    for (i=0;i<nfields;i++) {
      const char     *name;

      ierr = PetscObjectGetName(Vfield[i],&name);CHKERRQ(ierr);
      ierr = PetscStrallocpy(name,&socket->windowtitle[i]);CHKERRQ(ierr);
      ierr = PetscStrallocpy(fec_type[i],&socket->fec_type[i]);CHKERRQ(ierr);
      ierr = PetscObjectReference(Vfield[i]);CHKERRQ(ierr);
      socket->Ufield[i] = Vfield[i];
      socket->spacedim[i] = dim[i];
    }
  }
  /* number of fields are not allowed to vary */
  if (nfields != socket->nwindow) SETERRQ2(PetscObjectComm((PetscObject)viewer),PETSC_ERR_SUP,"Cannot visualize %D fields using %D socket windows",nfields,socket->nwindow);
  socket->g2lfield = g2l;
  if (socket->destroyctx && socket->userctx) { ierr = (*socket->destroyctx)(socket->userctx);CHKERRQ(ierr); }
  socket->userctx = ctx;
  socket->destroyctx = destroyctx;
  PetscFunctionReturn(0);
}

static PetscErrorCode PetscViewerGLVisInfoDestroy_Private(void *ptr)
{
  PetscViewerGLVisInfo info = (PetscViewerGLVisInfo)ptr;
  PetscErrorCode       ierr;

  PetscFunctionBegin;
  ierr = PetscFree(info->fmt);CHKERRQ(ierr);
  ierr = PetscFree(info);CHKERRQ(ierr);
  PetscFunctionReturn(0);
}

/* we can decide to prevent specific processes from using the viewer */
static PetscErrorCode PetscViewerGLVisAttachInfo_Private(PetscViewer viewer, PetscViewer window)
{
  PetscViewerGLVis     socket = (PetscViewerGLVis)viewer->data;
  PetscErrorCode       ierr;
  PetscContainer       container;
  PetscViewerGLVisInfo info;

  PetscFunctionBegin;
  ierr = PetscObjectQuery((PetscObject)window,"_glvis_info_container",(PetscObject*)&container);CHKERRQ(ierr);
  if (!container) {
    ierr = PetscNew(&info);CHKERRQ(ierr);
    info->enabled = PETSC_TRUE;
    info->init    = PETSC_FALSE;
    info->pause   = socket->pause;
    ierr = PetscContainerCreate(PetscObjectComm((PetscObject)window),&container);CHKERRQ(ierr);
    ierr = PetscContainerSetPointer(container,(void*)info);CHKERRQ(ierr);
    ierr = PetscContainerSetUserDestroy(container,PetscViewerGLVisInfoDestroy_Private);CHKERRQ(ierr);
    ierr = PetscObjectCompose((PetscObject)window,"_glvis_info_container",(PetscObject)container);CHKERRQ(ierr);
    ierr = PetscContainerDestroy(&container);CHKERRQ(ierr);
  } else {
    ierr = PetscContainerGetPointer(container,(void**)&info);CHKERRQ(ierr);
  }
  ierr = PetscFree(info->fmt);CHKERRQ(ierr);
  ierr = PetscStrallocpy(socket->fmt,&info->fmt);CHKERRQ(ierr);
  PetscFunctionReturn(0);
}

static PetscErrorCode PetscViewerGLVisGetNewWindow_Private(PetscViewer viewer,PetscViewer *view)
{
  PetscViewerGLVis socket = (PetscViewerGLVis)viewer->data;
  PetscViewer      window = NULL;
  PetscBool        ldis,dis;
  PetscErrorCode   ierr;

  PetscFunctionBegin;
  ierr = PetscViewerASCIISocketOpen(PETSC_COMM_SELF,socket->name,socket->port,&window);
  /* if we could not estabilish a connection the first time,
     we disable the socket viewer */
  ldis = ierr ? PETSC_TRUE : PETSC_FALSE;
  ierr = MPI_Allreduce(&ldis,&dis,1,MPIU_BOOL,MPI_LOR,PetscObjectComm((PetscObject)viewer));CHKERRQ(ierr);
  if (dis) {
    socket->status = PETSCVIEWERGLVIS_DISABLED;
    ierr  = PetscViewerDestroy(&window);CHKERRQ(ierr);
  }
  *view = window;
  PetscFunctionReturn(0);
}

PetscErrorCode PetscViewerGLVisPause_Private(PetscViewer viewer)
{
  PetscViewerGLVis socket = (PetscViewerGLVis)viewer->data;
  PetscErrorCode   ierr;

  PetscFunctionBegin;
  if (socket->pause > 0) {
    ierr = PetscSleep(socket->pause);CHKERRQ(ierr);
  }
  PetscFunctionReturn(0);
}

/* DM specific support */
PetscErrorCode PetscViewerGLVisSetDM_Private(PetscViewer viewer, PetscObject dm)
{
  PetscErrorCode   ierr;
  PetscViewerGLVis socket  = (PetscViewerGLVis)viewer->data;

  PetscFunctionBegin;
  if (socket->dm && socket->dm != dm) SETERRQ(PetscObjectComm((PetscObject)viewer),PETSC_ERR_SUP,"Cannot change DM associated with the GLVis viewer");
  if (!socket->dm) {
    PetscErrorCode (*setupwithdm)(PetscObject,PetscViewer) = NULL;

    ierr = PetscObjectQueryFunction(dm,"DMSetUpGLVisViewer_C",&setupwithdm);CHKERRQ(ierr);
    if (setupwithdm) {
      ierr = (*setupwithdm)(dm,viewer);CHKERRQ(ierr);
    } else SETERRQ1(PetscObjectComm(dm),PETSC_ERR_SUP,"No support for DM type %s",dm->type_name);
    ierr = PetscObjectReference(dm);CHKERRQ(ierr);
    socket->dm = dm;
  }
  PetscFunctionReturn(0);
}

PetscErrorCode PetscViewerGLVisGetDMWindow_Private(PetscViewer viewer,PetscViewer* view)
{
  PetscViewerGLVis socket = (PetscViewerGLVis)viewer->data;
  PetscErrorCode   ierr;

  PetscFunctionBegin;
  if (!socket->meshwindow) {
    if (socket->type == PETSC_VIEWER_GLVIS_SOCKET) {
      ierr = PetscViewerGLVisGetNewWindow_Private(viewer,&socket->meshwindow);CHKERRQ(ierr);
    } else {
      size_t    len;
      PetscBool isstdout;

      ierr = PetscStrlen(socket->name,&len);CHKERRQ(ierr);
      ierr = PetscStrcmp(socket->name,"stdout",&isstdout);CHKERRQ(ierr);
      if (!socket->name || !len || isstdout) {
        ierr = PetscViewerASCIIOpen(PETSC_COMM_SELF,"stdout",&socket->meshwindow);CHKERRQ(ierr);
      } else {
        PetscMPIInt rank;
        char        filename[PETSC_MAX_PATH_LEN];
        ierr = MPI_Comm_rank(PetscObjectComm((PetscObject)viewer),&rank);CHKERRQ(ierr);
        ierr = PetscSNPrintf(filename,PETSC_MAX_PATH_LEN,"%s-mesh.%06d",socket->name,rank);CHKERRQ(ierr);
        ierr = PetscViewerASCIIOpen(PETSC_COMM_SELF,filename,&socket->meshwindow);CHKERRQ(ierr);
      }
    }
    if (socket->meshwindow) {
      ierr = PetscViewerPushFormat(socket->meshwindow,PETSC_VIEWER_ASCII_GLVIS);CHKERRQ(ierr);
    }
  }
  if (socket->meshwindow) {
    ierr = PetscViewerGLVisAttachInfo_Private(viewer,socket->meshwindow);CHKERRQ(ierr);
  }
  *view = socket->meshwindow;
  PetscFunctionReturn(0);
}

PetscErrorCode PetscViewerGLVisGetType_Private(PetscViewer viewer,PetscViewerGLVisType *type)
{
  PetscViewerGLVis socket = (PetscViewerGLVis)viewer->data;

  PetscFunctionBegin;
  PetscValidPointer(type,2);
  *type = socket->type;
  PetscFunctionReturn(0);
}

/* This function is only relevant in the SOCKET_GLIVS case. The status is computed the first time it is requested, as GLVis currently has issues when connecting the first time through the socket */
PetscErrorCode PetscViewerGLVisGetStatus_Private(PetscViewer viewer, PetscViewerGLVisStatus *sockstatus)
{
  PetscViewerGLVis socket = (PetscViewerGLVis)viewer->data;

  PetscFunctionBegin;
  PetscValidPointer(sockstatus,2);
  if (socket->type == PETSC_VIEWER_GLVIS_DUMP) {
    socket->status = PETSCVIEWERGLVIS_DISCONNECTED;
  } else if (socket->status == PETSCVIEWERGLVIS_DISCONNECTED && socket->nwindow) {
    PetscInt       i;
    PetscBool      lconn,conn;
    PetscErrorCode ierr;

    for (i=0,lconn=PETSC_TRUE;i<socket->nwindow;i++)
      if (!socket->window[i])
        lconn = PETSC_FALSE;

    ierr = MPI_Allreduce(&lconn,&conn,1,MPIU_BOOL,MPI_LAND,PetscObjectComm((PetscObject)viewer));CHKERRQ(ierr);
    if (conn) socket->status = PETSCVIEWERGLVIS_CONNECTED;
  }
  *sockstatus = socket->status;
  PetscFunctionReturn(0);
}

PetscErrorCode PetscViewerGLVisGetDM_Private(PetscViewer viewer, PetscObject* dm)
{
  PetscViewerGLVis socket = (PetscViewerGLVis)viewer->data;

  PetscFunctionBegin;
  *dm = socket->dm;
  PetscFunctionReturn(0);
}

PetscErrorCode PetscViewerGLVisGetFields_Private(PetscViewer viewer, PetscInt* nfield, const char **fec[], PetscInt *spacedim[], PetscErrorCode(**g2lfield)(PetscObject,PetscInt,PetscObject[],void*), PetscObject *Ufield[], void **userctx)
{
  PetscViewerGLVis socket = (PetscViewerGLVis)viewer->data;

  PetscFunctionBegin;
  if (nfield)   *nfield   = socket->nwindow;
  if (fec)      *fec      = (const char**)socket->fec_type;
  if (spacedim) *spacedim = socket->spacedim;
  if (g2lfield) *g2lfield = socket->g2lfield;
  if (Ufield)   *Ufield   = socket->Ufield;
  if (userctx)  *userctx  = socket->userctx;
  PetscFunctionReturn(0);
}

/* accessor routines for the viewer windows:
   PETSC_VIEWER_GLVIS_DUMP   : it returns a new viewer every time
   PETSC_VIEWER_GLVIS_SOCKET : it returns the socket, and creates it if not yet done.
*/
PetscErrorCode PetscViewerGLVisGetWindow_Private(PetscViewer viewer,PetscInt wid,PetscViewer* view)
{
  PetscViewerGLVis       socket = (PetscViewerGLVis)viewer->data;
  PetscViewerGLVisStatus status;
  PetscErrorCode         ierr;

  PetscFunctionBegin;
  PetscValidLogicalCollectiveInt(viewer,wid,2);
  PetscValidPointer(view,3);
  if (wid < 0 || wid > socket->nwindow-1) SETERRQ2(PetscObjectComm((PetscObject)viewer),PETSC_ERR_USER,"Cannot get window id %D: allowed range [0,%D)",wid,socket->nwindow-1);
  status = socket->status;
  if (socket->type == PETSC_VIEWER_GLVIS_DUMP && socket->window[wid]) SETERRQ1(PETSC_COMM_SELF,PETSC_ERR_USER,"Window %D is already in use",wid);
  switch (status) {
    case PETSCVIEWERGLVIS_DISCONNECTED:
      if (socket->window[wid]) SETERRQ(PETSC_COMM_SELF,PETSC_ERR_USER,"This should not happen");
      if (socket->type == PETSC_VIEWER_GLVIS_DUMP) {
        size_t    len;
        PetscBool isstdout;

        ierr = PetscStrlen(socket->name,&len);CHKERRQ(ierr);
        ierr = PetscStrcmp(socket->name,"stdout",&isstdout);CHKERRQ(ierr);
        if (!socket->name || !len || isstdout) {
          ierr = PetscViewerASCIIOpen(PETSC_COMM_SELF,"stdout",&socket->window[wid]);CHKERRQ(ierr);
        } else {
          PetscMPIInt rank;
          char        filename[PETSC_MAX_PATH_LEN];

          ierr = MPI_Comm_rank(PetscObjectComm((PetscObject)viewer),&rank);CHKERRQ(ierr);
          ierr = PetscSNPrintf(filename,PETSC_MAX_PATH_LEN,"%s-%s-%d.%06d",socket->name,socket->windowtitle[wid],socket->snapid,rank);CHKERRQ(ierr);
          ierr = PetscViewerASCIIOpen(PETSC_COMM_SELF,filename,&socket->window[wid]);CHKERRQ(ierr);
        }
      } else {
        ierr = PetscViewerGLVisGetNewWindow_Private(viewer,&socket->window[wid]);CHKERRQ(ierr);
      }
      if (socket->window[wid]) {
        ierr = PetscViewerPushFormat(socket->window[wid],PETSC_VIEWER_ASCII_GLVIS);CHKERRQ(ierr);
      }
      *view = socket->window[wid];
      break;
    case PETSCVIEWERGLVIS_CONNECTED:
      *view = socket->window[wid];
      break;
    case PETSCVIEWERGLVIS_DISABLED:
      *view = NULL;
      break;
    default:
      SETERRQ1(PetscObjectComm((PetscObject)viewer),PETSC_ERR_SUP,"Unhandled socket status %d\n",(int)status);
      break;
  }
  if (*view) {
    ierr = PetscViewerGLVisAttachInfo_Private(viewer,*view);CHKERRQ(ierr);
  }
  PetscFunctionReturn(0);
}

/* Restore the window viewer
   PETSC_VIEWER_GLVIS_DUMP  : destroys the temporary created ASCII viewer used for dumping
   PETSC_VIEWER_GLVIS_SOCKET: - if the returned window viewer is not NULL, just zeros the pointer.
                 - it the returned window viewer is NULL, assumes something went wrong
                   with the socket (i.e. SIGPIPE when a user closes the popup window)
                   and that the caller already handled it (see VecView_GLVis).
*/
PetscErrorCode PetscViewerGLVisRestoreWindow_Private(PetscViewer viewer,PetscInt wid, PetscViewer* view)
{
  PetscViewerGLVis socket = (PetscViewerGLVis)viewer->data;
  PetscErrorCode   ierr;

  PetscFunctionBegin;
  PetscValidLogicalCollectiveInt(viewer,wid,2);
  PetscValidPointer(view,3);
  if (wid < 0 || wid > socket->nwindow-1) SETERRQ2(PetscObjectComm((PetscObject)viewer),PETSC_ERR_USER,"Cannot restore window id %D: allowed range [0,%D)",wid,socket->nwindow);
  if (*view && *view != socket->window[wid]) SETERRQ(PetscObjectComm((PetscObject)viewer),PETSC_ERR_USER,"Viewer was not obtained from PetscViewerGLVisGetWindow");
  if (*view) {
    ierr = PetscViewerFlush(*view);CHKERRQ(ierr);
    ierr = PetscBarrier((PetscObject)viewer);CHKERRQ(ierr);
  }
  if (socket->type == PETSC_VIEWER_GLVIS_DUMP) { /* destroy the viewer, as it is associated with a single time step */
    ierr = PetscViewerDestroy(&socket->window[wid]);CHKERRQ(ierr);
  } else if (!*view) { /* something went wrong (SIGPIPE) so we just zero the private pointer */
    socket->window[wid] = NULL;
  }
  *view = NULL;
  PetscFunctionReturn(0);
}

/* default window appearance in the PETSC_VIEWER_GLVIS_SOCKET case */
PetscErrorCode PetscViewerGLVisInitWindow_Private(PetscViewer viewer, PetscBool mesh, PetscInt dim, const char *name)
{
  PetscErrorCode       ierr;
  PetscViewerGLVisInfo info;
  PetscContainer       container;

  PetscFunctionBegin;
  ierr = PetscObjectQuery((PetscObject)viewer,"_glvis_info_container",(PetscObject*)&container);CHKERRQ(ierr);
  if (!container) SETERRQ(PETSC_COMM_SELF,PETSC_ERR_USER,"Viewer was not obtained from PetscGLVisViewerGetNewWindow_Private");
  ierr = PetscContainerGetPointer(container,(void**)&info);CHKERRQ(ierr);
  if (info->init) {
    if (info->pause < 0) {
      ierr = PetscViewerASCIIPrintf(viewer,"pause\n");CHKERRQ(ierr); /* pause */
    }
    PetscFunctionReturn(0);
  }
  ierr = PetscViewerASCIIPrintf(viewer,"window_size 800 800\n");CHKERRQ(ierr);
  if (name) {
    ierr = PetscViewerASCIIPrintf(viewer,"window_title '%s'\n",name);CHKERRQ(ierr);
  }
  if (mesh) {
    switch (dim) {
    case 1:
      break;
    case 2:
      ierr = PetscViewerASCIIPrintf(viewer,"keys cmeeppppp\n");CHKERRQ(ierr); /* show colorbar, mesh and ranks */
      break;
    case 3: /* TODO: decide default view in 3D */
      break;
    }
  } else {
    ierr = PetscViewerASCIIPrintf(viewer,"keys cm\n");CHKERRQ(ierr); /* show colorbar and mesh */
    switch (dim) {
    case 1:
      ierr = PetscViewerASCIIPrintf(viewer,"keys RRj\n");CHKERRQ(ierr); /* set to 1D (side view) and turn off perspective */
      break;
    case 2:
      ierr = PetscViewerASCIIPrintf(viewer,"keys Rjl\n");CHKERRQ(ierr); /* set to 2D (top view), turn off perspective and light */
      break;
    case 3:
      break;
    }
    ierr = PetscViewerASCIIPrintf(viewer,"autoscale value\n");CHKERRQ(ierr); /* update value-range; keep mesh-extents fixed */
    if (info->pause == 0) {
      ierr = PetscViewerASCIIPrintf(viewer,"pause\n");CHKERRQ(ierr); /* pause */
    }
  }
  info->init = PETSC_TRUE;
  PetscFunctionReturn(0);
}

static PetscErrorCode PetscViewerDestroy_GLVis(PetscViewer viewer)
{
  PetscViewerGLVis socket = (PetscViewerGLVis)viewer->data;
  PetscInt         i;
  PetscErrorCode   ierr;

  PetscFunctionBegin;
  for (i=0;i<socket->nwindow;i++) {
    ierr = PetscViewerDestroy(&socket->window[i]);CHKERRQ(ierr);
    ierr = PetscFree(socket->windowtitle[i]);CHKERRQ(ierr);
    ierr = PetscFree(socket->fec_type[i]);CHKERRQ(ierr);
    ierr = PetscObjectDestroy(&socket->Ufield[i]);CHKERRQ(ierr);
  }
  ierr = PetscFree(socket->name);CHKERRQ(ierr);
  ierr = PetscFree5(socket->window,socket->windowtitle,socket->fec_type,socket->spacedim,socket->Ufield);CHKERRQ(ierr);
  ierr = PetscFree(socket->fmt);CHKERRQ(ierr);
  ierr = PetscViewerDestroy(&socket->meshwindow);CHKERRQ(ierr);
  ierr = PetscObjectDestroy(&socket->dm);CHKERRQ(ierr);
  if (socket->destroyctx && socket->userctx) { ierr = (*socket->destroyctx)(socket->userctx);CHKERRQ(ierr); }

  ierr = PetscObjectComposeFunction((PetscObject)viewer,"PetscViewerGLVisSetPrecision_C",NULL);CHKERRQ(ierr);
  ierr = PetscObjectComposeFunction((PetscObject)viewer,"PetscViewerGLVisSetSnapId_C",NULL);CHKERRQ(ierr);
  ierr = PetscObjectComposeFunction((PetscObject)viewer,"PetscViewerGLVisSetFields_C",NULL);CHKERRQ(ierr);
  ierr = PetscObjectComposeFunction((PetscObject)viewer,"PetscViewerFileSetName_C",NULL);CHKERRQ(ierr);
  ierr = PetscFree(socket);CHKERRQ(ierr);
  viewer->data = NULL;
  PetscFunctionReturn(0);
}

static PetscErrorCode PetscViewerSetFromOptions_GLVis(PetscOptionItems *PetscOptionsObject,PetscViewer v)
{
  PetscErrorCode   ierr;
  PetscViewerGLVis socket = (PetscViewerGLVis)v->data;

  PetscFunctionBegin;
  ierr = PetscOptionsHead(PetscOptionsObject,"GLVis PetscViewer Options");CHKERRQ(ierr);
  ierr = PetscOptionsReal("-viewer_glvis_pause","-1 to pause after each visualization, otherwise sleeps for given seconds",NULL,socket->pause,&socket->pause,NULL);CHKERRQ(ierr);
  ierr = PetscOptionsTail();CHKERRQ(ierr);
  PetscFunctionReturn(0);
}

static PetscErrorCode PetscViewerSetFileName_GLVis(PetscViewer viewer, const char name[])
{
  char             *sport;
  PetscViewerGLVis socket = (PetscViewerGLVis)viewer->data;
  PetscErrorCode   ierr;

  PetscFunctionBegin;
  socket->type = PETSC_VIEWER_GLVIS_DUMP;
  /* we accept localhost^port */
  ierr = PetscFree(socket->name);CHKERRQ(ierr);
  ierr = PetscStrallocpy(name,&socket->name);CHKERRQ(ierr);
  ierr = PetscStrchr(socket->name,'^',&sport);CHKERRQ(ierr);
  if (sport) {
    PetscInt port = 19916;
    size_t   len;

    *sport++ = 0;
    ierr = PetscStrlen(sport,&len);CHKERRQ(ierr);
    if (len) ierr = PetscOptionsStringToInt(sport,&port);
    if (!ierr) {
      socket->port = (port != PETSC_DECIDE && port != PETSC_DEFAULT) ? port : 19916;
    } else {
      socket->port = 19916;
    }
    socket->type = PETSC_VIEWER_GLVIS_SOCKET;
  }
  PetscFunctionReturn(0);
}

/*
     PetscViewerGLVisOpen - Opens a GLVis type viewer

  Collective on comm

  Input Parameters:
+  comm      - the MPI communicator
.  type      - the viewer type: PETSC_VIEWER_GLVIS_SOCKET for real-time visualization or PETSC_VIEWER_GLVIS_DUMP for dumping to disk
.  name      - either the hostname where the GLVis server is running or the base filename for dumping the data for subsequent visualizations
-  port      - socket port where the GLVis server is listening. Not referenced when type is PETSC_VIEWER_GLVIS_DUMP

  Output Parameters:
-  viewer    - the PetscViewer object

  Notes:
    misses Fortran binding

  Level: beginner

.seealso: PetscViewerCreate(), PetscViewerSetType(), PetscViewerGLVisType
*/
PETSC_EXTERN PetscErrorCode PetscViewerGLVisOpen(MPI_Comm comm, PetscViewerGLVisType type, const char* name, PetscInt port, PetscViewer* viewer)
{
  PetscViewerGLVis socket;
  PetscErrorCode   ierr;

  PetscFunctionBegin;
  ierr = PetscViewerCreate(comm,viewer);CHKERRQ(ierr);
  ierr = PetscViewerSetType(*viewer,PETSCVIEWERGLVIS);CHKERRQ(ierr);

  socket       = (PetscViewerGLVis)((*viewer)->data);
  socket->type = type;
  if (type == PETSC_VIEWER_GLVIS_DUMP || name) {
    ierr = PetscFree(socket->name);CHKERRQ(ierr);
    ierr = PetscStrallocpy(name,&socket->name);CHKERRQ(ierr);
  }
  socket->port = (!port || port == PETSC_DETERMINE || port == PETSC_DECIDE) ? 19916 : port;

  ierr = PetscViewerSetFromOptions(*viewer);CHKERRQ(ierr);
  PetscFunctionReturn(0);
}

/*
  PETSC_VIEWER_GLVIS_ - Creates an GLVIS PetscViewer shared by all processors in a communicator.

  Collective on MPI_Comm

  Input Parameter:
. comm - the MPI communicator to share the GLVIS PetscViewer

  Level: intermediate

  Notes:
    misses Fortran bindings

  Environmental variables:
+ PETSC_VIEWER_GLVIS_FILENAME : output filename (if specified dump to disk, and takes precedence on PETSC_VIEWER_GLVIS_HOSTNAME)
. PETSC_VIEWER_GLVIS_HOSTNAME : machine where the GLVis server is listening (defaults to localhost)
- PETSC_VIEWER_GLVIS_PORT     : port opened by the GLVis server (defaults to 19916)

  Notes:
  Unlike almost all other PETSc routines, PETSC_VIEWER_GLVIS_ does not return
  an error code.  The GLVIS PetscViewer is usually used in the form
$       XXXView(XXX object, PETSC_VIEWER_GLVIS_(comm));

.seealso: PetscViewerGLVISOpen(), PetscViewerGLVisType, PetscViewerCreate(), PetscViewerDestroy()
*/
PETSC_EXTERN PetscViewer PETSC_VIEWER_GLVIS_(MPI_Comm comm)
{
  PetscErrorCode       ierr;
  PetscBool            flg;
  PetscViewer          viewer;
  PetscViewerGLVisType type;
  char                 fname[PETSC_MAX_PATH_LEN],sport[16];
  PetscInt             port = 19916; /* default for GLVis */

  PetscFunctionBegin;
  ierr = PetscOptionsGetenv(comm,"PETSC_VIEWER_GLVIS_FILENAME",fname,PETSC_MAX_PATH_LEN,&flg);
  if (ierr) {PetscError(PETSC_COMM_SELF,__LINE__,"PETSC_VIEWER_GLVIS_",__FILE__,PETSC_ERR_PLIB,PETSC_ERROR_INITIAL," ");PetscFunctionReturn(0);}
  if (!flg) {
    type = PETSC_VIEWER_GLVIS_SOCKET;
    ierr = PetscOptionsGetenv(comm,"PETSC_VIEWER_GLVIS_HOSTNAME",fname,PETSC_MAX_PATH_LEN,&flg);
    if (ierr) {PetscError(PETSC_COMM_SELF,__LINE__,"PETSC_VIEWER_GLVIS_",__FILE__,PETSC_ERR_PLIB,PETSC_ERROR_INITIAL," ");PetscFunctionReturn(0);}
    if (!flg) {
      ierr = PetscStrcpy(fname,"localhost");
      if (ierr) {PetscError(PETSC_COMM_SELF,__LINE__,"PETSC_VIEWER_GLVIS_",__FILE__,PETSC_ERR_PLIB,PETSC_ERROR_INITIAL," ");PetscFunctionReturn(0);}
    }
    ierr = PetscOptionsGetenv(comm,"PETSC_VIEWER_GLVIS_PORT",sport,16,&flg);
    if (ierr) {PetscError(PETSC_COMM_SELF,__LINE__,"PETSC_VIEWER_GLVIS_",__FILE__,PETSC_ERR_PLIB,PETSC_ERROR_INITIAL," ");PetscFunctionReturn(0);}
    if (flg) {
      ierr = PetscOptionsStringToInt(sport,&port);
      if (ierr) {PetscError(PETSC_COMM_SELF,__LINE__,"PETSC_VIEWER_GLVIS_",__FILE__,PETSC_ERR_PLIB,PETSC_ERROR_INITIAL," ");PetscFunctionReturn(0);}
    }
  } else {
    type = PETSC_VIEWER_GLVIS_DUMP;
  }
  ierr = PetscViewerGLVisOpen(comm,type,fname,port,&viewer);
  if (ierr) {PetscError(PETSC_COMM_SELF,__LINE__,"PETSC_VIEWER_GLVIS_",__FILE__,PETSC_ERR_PLIB,PETSC_ERROR_INITIAL," ");PetscFunctionReturn(0);}
  ierr = PetscObjectRegisterDestroy((PetscObject)viewer);
  if (ierr) {PetscError(PETSC_COMM_SELF,__LINE__,"PETSC_VIEWER_GLVIS_",__FILE__,PETSC_ERR_PLIB,PETSC_ERROR_INITIAL," ");PetscFunctionReturn(0);}
  PetscFunctionReturn(viewer);
}

PETSC_EXTERN PetscErrorCode PetscViewerCreate_GLVis(PetscViewer viewer)
{
  PetscViewerGLVis socket;
  PetscErrorCode   ierr;

  PetscFunctionBegin;
  ierr = PetscNewLog(viewer,&socket);CHKERRQ(ierr);

  /* defaults to socket viewer */
  ierr = PetscStrallocpy("localhost",&socket->name);CHKERRQ(ierr);
  socket->port  = 19916; /* GLVis default listening port */
  socket->type  = PETSC_VIEWER_GLVIS_SOCKET;
  socket->pause = 0; /* just pause the first time */

  /* defaults to full precision */
  ierr = PetscStrallocpy(" %g",&socket->fmt);CHKERRQ(ierr);

  viewer->data                = (void*)socket;
  viewer->ops->destroy        = PetscViewerDestroy_GLVis;
  viewer->ops->setfromoptions = PetscViewerSetFromOptions_GLVis;

  ierr = PetscObjectComposeFunction((PetscObject)viewer,"PetscViewerGLVisSetPrecision_C",PetscViewerGLVisSetPrecision_GLVis);CHKERRQ(ierr);
  ierr = PetscObjectComposeFunction((PetscObject)viewer,"PetscViewerGLVisSetSnapId_C",PetscViewerGLVisSetSnapId_GLVis);CHKERRQ(ierr);
  ierr = PetscObjectComposeFunction((PetscObject)viewer,"PetscViewerGLVisSetFields_C",PetscViewerGLVisSetFields_GLVis);CHKERRQ(ierr);
  ierr = PetscObjectComposeFunction((PetscObject)viewer,"PetscViewerFileSetName_C",PetscViewerSetFileName_GLVis);CHKERRQ(ierr);
  PetscFunctionReturn(0);
}

/* this is a private implementation of a SOCKET with ASCII data format
   GLVis does not currently handle binary socket streams */
#if defined(PETSC_HAVE_UNISTD_H)
#include <unistd.h>
#endif

#if !defined(PETSC_HAVE_WINDOWS_H)
static PetscErrorCode (*PetscViewerDestroy_ASCII)(PetscViewer);

static PetscErrorCode PetscViewerDestroy_ASCII_Socket(PetscViewer viewer)
{
  FILE *stream;
  PetscErrorCode ierr = 0;
  PetscFunctionBegin;
  ierr = PetscViewerASCIIGetPointer(viewer,&stream);CHKERRQ(ierr);
  if (stream) {
    ierr = fclose(stream);
    if (ierr) SETERRQ(PETSC_COMM_SELF,PETSC_ERR_SYS,"fclose() failed on stream");
  }
  ierr = PetscViewerDestroy_ASCII(viewer);CHKERRQ(ierr);
  PetscFunctionReturn(0);
}
#endif

static PetscErrorCode PetscViewerASCIISocketOpen(MPI_Comm comm,const char* hostname,PetscInt port,PetscViewer* viewer)
{
#if defined(PETSC_HAVE_WINDOWS_H)
  PetscFunctionBegin;
  SETERRQ(comm,PETSC_ERR_SUP,"Not implemented for Windows");
#else
  FILE           *stream = NULL;
  int            fd;
  PetscErrorCode ierr;

  PetscFunctionBegin;
  PetscValidPointer(hostname,2);
  PetscValidPointer(viewer,4);
#if defined(PETSC_USE_SOCKET_VIEWER)
  ierr = PetscOpenSocket(hostname,port,&fd);
#else
  SETERRQ(comm,PETSC_ERR_SUP,"Missing Socket viewer");
#endif
  if (ierr) {
    PetscInt sierr = ierr;
    char     err[1024];

    ierr    = PetscSNPrintf(err,1024,"Cannot connect to socket on %s:%D. Socket visualization is disabled\n",hostname,port);CHKERRQ(ierr);
    ierr    = PetscInfo(NULL,err);CHKERRQ(ierr);
    *viewer = NULL;
    PetscFunctionReturn(sierr);
  } else {
    char msg[1024];

    ierr = PetscSNPrintf(msg,1024,"Successfully connect to socket on %s:%D. Socket visualization is enabled\n",hostname,port);CHKERRQ(ierr);
    ierr = PetscInfo(NULL,msg);CHKERRQ(ierr);
  }
  stream = fdopen(fd,"w"); /* Not possible on Windows */
  if (!stream) SETERRQ2(PETSC_COMM_SELF,PETSC_ERR_SYS,"Cannot open stream from socket %s:%d",hostname,port);
  ierr = PetscViewerASCIIOpenWithFILE(PETSC_COMM_SELF,stream,viewer);CHKERRQ(ierr);
  PetscViewerDestroy_ASCII = (*viewer)->ops->destroy;
  (*viewer)->ops->destroy = PetscViewerDestroy_ASCII_Socket;
#endif
  PetscFunctionReturn(0);
}
