#include <petsc/private/fortranimpl.h>
#include <petsc/private/f90impl.h>
#include <petsc/private/taoimpl.h>


#if defined(PETSC_HAVE_FORTRAN_CAPS)
#define taosetobjectiveroutine_             TAOSETOBJECTIVEROUTINE
#define taosetgradientroutine_              TAOSETGRADIENTROUTINE
#define taosetobjectiveandgradientroutine_  TAOSETOBJECTIVEANDGRADIENTROUTINE
#define taosethessianroutine_               TAOSETHESSIANROUTINE
#define taosetseparableobjectiveroutine_    TAOSETSEPARABLEOBJECTIVEROUTINE
#define taosetjacobianroutine_              TAOSETJACOBIANROUTINE
#define taosetjacobianstateroutine_         TAOSETJACOBIANSTATEROUTINE
#define taosetjacobiandesignroutine_        TAOSETJACOBIANDESIGNROUTINE
#define taosetjacobianinequalityroutine_    TAOSETJACOBIANINEQUALITYROUTINE
#define taosetjacobianequalityroutine_      TAOSETJACOBIANEQUALITYROUTINE
#define taosetinequalityconstraintsroutine_ TAOSETINEQUALITYCONSTRAINTSROUTINE
#define taosetequalityconstraintsroutine_   TAOSETEQUALITYCONSTRAINTSROUTINE
#define taosetvariableboundsroutine_        TAOSETVARIABLEBOUNDSROUTINE
#define taosetconstraintsroutine_           TAOSETCONSTRAINTSROUTINE
#define taosetmonitor_                      TAOSETMONITOR
#define taosettype_                         TAOSETTYPE
#define taoview_                            TAOVIEW
#define taogetconvergencehistory_           TAOGETCONVERGENCEHISTORY
#define taosetconvergencetest_              TAOSETCONVERGENCETEST
#define taogetoptionsprefix_                TAOGETOPTIONSPREFIX
#define taosetoptionsprefix_                TAOSETOPTIONSPREFIX
#define taoappendoptionsprefix_             TAOAPPENDOPTIONSPREFIX
#define taogettype_                         TAOGETTYPE
#elif !defined(PETSC_HAVE_FORTRAN_UNDERSCORE)

#define taosetobjectiveroutine_             taosetobjectiveroutine
#define taosetgradientroutine_              taosetgradientroutine
#define taosetobjectiveandgradientroutine_  taosetobjectiveandgradientroutine
#define taosethessianroutine_               taosethessianroutine
#define taosetseparableobjectiveroutine_    taosetseparableobjectiveroutine
#define taosetjacobianroutine_              taosetjacobianroutine
#define taosetjacobianstateroutine_         taosetjacobianstateroutine
#define taosetjacobiandesignroutine_        taosetjacobiandesignroutine
#define taosetjacobianinequalityroutine_    taosetjacobianinequalityroutine
#define taosetjacobianequalityroutine_      taosetjacobianequalityroutine
#define taosetinequalityconstraintsroutine_ taosetinequalityconstraintsroutine
#define taosetequalityconstraintsroutine_   taosetequalityconstraintsroutine
#define taosetvariableboundsroutine_        taosetvariableboundsroutine
#define taosetconstraintsroutine_           taosetconstraintsroutine
#define taosetmonitor_                      taosetmonitor
#define taosettype_                         taosettype
#define taoview_                            taoview
#define taogetconvergencehistory_           taogetconvergencehistory
#define taosetconvergencetest_              taosetconvergencetest
#define taogetoptionsprefix_                taogetoptionsprefix
#define taosetoptionsprefix_                taosetoptionsprefix
#define taoappendoptionsprefix_             taoappendoptionsprefix
#define taogettype_                         taogettype
#endif

static struct {
  PetscFortranCallbackId obj;
  PetscFortranCallbackId grad;
  PetscFortranCallbackId objgrad;
  PetscFortranCallbackId hess;
  PetscFortranCallbackId sepobj;
  PetscFortranCallbackId jac;
  PetscFortranCallbackId jacstate;
  PetscFortranCallbackId jacdesign;
  PetscFortranCallbackId bounds;
  PetscFortranCallbackId mon;
  PetscFortranCallbackId mondestroy;
  PetscFortranCallbackId convtest;
  PetscFortranCallbackId constraints;
  PetscFortranCallbackId jacineq;
  PetscFortranCallbackId jaceq;
  PetscFortranCallbackId conineq;
  PetscFortranCallbackId coneq;
  PetscFortranCallbackId nfuncs;
#if defined(PETSC_HAVE_F90_2PTR_ARG)
  PetscFortranCallbackId function_pgiptr;
#endif
} _cb;

static PetscErrorCode ourtaoobjectiveroutine(Tao tao, Vec x, PetscReal *f, void *ctx)
{
    PetscObjectUseFortranCallback(tao,_cb.obj,(Tao*,Vec*,PetscReal*,void*,PetscErrorCode*),(&tao,&x,f,_ctx,&ierr));
}

static PetscErrorCode ourtaogradientroutine(Tao tao, Vec x, Vec g, void *ctx)
{
    PetscObjectUseFortranCallback(tao,_cb.grad,(Tao*,Vec*,Vec*,void*,PetscErrorCode*),(&tao,&x,&g,_ctx,&ierr));
}

static PetscErrorCode ourtaoobjectiveandgradientroutine(Tao tao, Vec x, PetscReal *f, Vec g, void* ctx)
{
    PetscObjectUseFortranCallback(tao,_cb.objgrad,(Tao*,Vec*,PetscReal*,Vec*,void*,PetscErrorCode*),(&tao,&x,f,&g,_ctx,&ierr));
}

static PetscErrorCode ourtaohessianroutine(Tao tao, Vec x, Mat H, Mat Hpre, void *ctx)
{
    PetscObjectUseFortranCallback(tao,_cb.hess,(Tao*,Vec*,Mat*,Mat*,void*,PetscErrorCode*),(&tao,&x,&H,&Hpre,_ctx,&ierr));
}

static PetscErrorCode ourtaojacobianroutine(Tao tao, Vec x, Mat H, Mat Hpre, void *ctx)
{
    PetscObjectUseFortranCallback(tao,_cb.jac,(Tao*,Vec*,Mat*,Mat*,void*,PetscErrorCode*),(&tao,&x,&H,&Hpre,_ctx,&ierr));
}

static PetscErrorCode ourtaojacobianstateroutine(Tao tao, Vec x, Mat H, Mat Hpre, Mat Hinv, void *ctx)
{
    PetscObjectUseFortranCallback(tao,_cb.jacstate,(Tao*,Vec*,Mat*,Mat*,Mat*,void*,PetscErrorCode*),(&tao,&x,&H,&Hpre,&Hinv,_ctx,&ierr));
}

static PetscErrorCode ourtaojacobiandesignroutine(Tao tao, Vec x, Mat H, void *ctx)
{
    PetscObjectUseFortranCallback(tao,_cb.jacdesign,(Tao*,Vec*,Mat*,void*,PetscErrorCode*),(&tao,&x,&H,_ctx,&ierr));
}

static PetscErrorCode ourtaoboundsroutine(Tao tao, Vec xl, Vec xu, void *ctx)
{
    PetscObjectUseFortranCallback(tao,_cb.bounds,(Tao*,Vec*,Vec*,void*,PetscErrorCode*),(&tao,&xl,&xu,_ctx,&ierr));
}
static PetscErrorCode ourtaoseparableobjectiveroutine(Tao tao, Vec x, Vec f, void *ctx)
{
    PetscObjectUseFortranCallback(tao,_cb.sepobj,(Tao*,Vec*,Vec*,void*,PetscErrorCode*),(&tao,&x,&f,_ctx,&ierr));
}

static PetscErrorCode ourtaomonitor(Tao tao, void *ctx)
{
    PetscObjectUseFortranCallback(tao,_cb.mon,(Tao *,void*,PetscErrorCode*),(&tao,_ctx,&ierr));
}

static PetscErrorCode ourtaomondestroy(void **ctx)
{
    Tao tao = (Tao)*ctx;
    PetscObjectUseFortranCallback(tao,_cb.mondestroy,(void*,PetscErrorCode*),(_ctx,&ierr));
}
static PetscErrorCode ourtaoconvergencetest(Tao tao, void *ctx)
{
    PetscObjectUseFortranCallback(tao,_cb.convtest,(Tao *,void*,PetscErrorCode*),(&tao,_ctx,&ierr));
}

static PetscErrorCode ourtaoconstraintsroutine(Tao tao, Vec x, Vec c, void *ctx)
{
    PetscObjectUseFortranCallback(tao,_cb.constraints,(Tao*,Vec*,Vec*,void*,PetscErrorCode*),(&tao,&x,&c,_ctx,&ierr));
}

static PetscErrorCode ourtaojacobianinequalityroutine(Tao tao, Vec x, Mat J, Mat Jpre, void *ctx)
{
    PetscObjectUseFortranCallback(tao,_cb.jacineq,(Tao*,Vec*,Mat*,Mat*,void*,PetscErrorCode*),(&tao,&x,&J,&Jpre,_ctx,&ierr));
}

static PetscErrorCode ourtaojacobianequalityroutine(Tao tao, Vec x, Mat J, Mat Jpre, void *ctx)
{
    PetscObjectUseFortranCallback(tao,_cb.jaceq,(Tao*,Vec*,Mat*,Mat*,void*,PetscErrorCode*),(&tao,&x,&J,&Jpre,_ctx,&ierr));
}

static PetscErrorCode ourtaoinequalityconstraintsroutine(Tao tao, Vec x, Vec c, void *ctx)
{
    PetscObjectUseFortranCallback(tao,_cb.conineq,(Tao*,Vec*,Vec*,void*,PetscErrorCode*),(&tao,&x,&c,_ctx,&ierr));
}

static PetscErrorCode ourtaoequalityconstraintsroutine(Tao tao, Vec x, Vec c, void *ctx)
{
    PetscObjectUseFortranCallback(tao,_cb.coneq,(Tao*,Vec*,Vec*,void*,PetscErrorCode*),(&tao,&x,&c,_ctx,&ierr));
}

EXTERN_C_BEGIN

PETSC_EXTERN void PETSC_STDCALL taosetobjectiveroutine_(Tao *tao, void (PETSC_STDCALL *func)(Tao*, Vec *, PetscReal *, void *, PetscErrorCode *), void *ctx, PetscErrorCode *ierr)
{
    CHKFORTRANNULLFUNCTION(func);
    *ierr = PetscObjectSetFortranCallback((PetscObject)*tao,PETSC_FORTRAN_CALLBACK_CLASS,&_cb.obj,(PetscVoidFunction)func,ctx);
    if(!*ierr) *ierr = TaoSetObjectiveRoutine(*tao,ourtaoobjectiveroutine,ctx);
}

PETSC_EXTERN void PETSC_STDCALL taosetgradientroutine_(Tao *tao, void (PETSC_STDCALL *func)(Tao*, Vec *, Vec *, void *, PetscErrorCode *), void *ctx, PetscErrorCode *ierr)
{
    CHKFORTRANNULLFUNCTION(func);
    *ierr = PetscObjectSetFortranCallback((PetscObject)*tao,PETSC_FORTRAN_CALLBACK_CLASS,&_cb.grad,(PetscVoidFunction)func,ctx);
    if(!*ierr) *ierr = TaoSetGradientRoutine(*tao,ourtaogradientroutine,ctx);
}

PETSC_EXTERN void PETSC_STDCALL taosetobjectiveandgradientroutine_(Tao *tao, void (PETSC_STDCALL *func)(Tao*, Vec *, PetscReal *, Vec *, void *, PetscErrorCode *), void *ctx, PetscErrorCode *ierr)
{
    CHKFORTRANNULLFUNCTION(func);
    *ierr = PetscObjectSetFortranCallback((PetscObject)*tao,PETSC_FORTRAN_CALLBACK_CLASS,&_cb.objgrad,(PetscVoidFunction)func,ctx);
    if(!*ierr) *ierr = TaoSetObjectiveAndGradientRoutine(*tao,ourtaoobjectiveandgradientroutine,ctx);
}

PETSC_EXTERN void PETSC_STDCALL taosetseparableobjectiveroutine_(Tao *tao, Vec *F, void (PETSC_STDCALL *func)(Tao*, Vec *, Vec *, void *, PetscErrorCode *), void *ctx, PetscErrorCode *ierr)
{
    CHKFORTRANNULLFUNCTION(func);
    *ierr = PetscObjectSetFortranCallback((PetscObject)*tao,PETSC_FORTRAN_CALLBACK_CLASS,&_cb.sepobj,(PetscVoidFunction)func,ctx);
    if(!*ierr) *ierr = TaoSetSeparableObjectiveRoutine(*tao,*F,ourtaoseparableobjectiveroutine,ctx);
}

PETSC_EXTERN void PETSC_STDCALL taosetjacobianroutine_(Tao *tao, Mat *J, Mat *Jp, void (PETSC_STDCALL *func)(Tao*, Vec *, Mat *, Mat *, void *, PetscErrorCode *), void *ctx, PetscErrorCode *ierr)
{
    CHKFORTRANNULLFUNCTION(func);
    *ierr = PetscObjectSetFortranCallback((PetscObject)*tao,PETSC_FORTRAN_CALLBACK_CLASS,&_cb.jac,(PetscVoidFunction)func,ctx);
    if(!*ierr) *ierr = TaoSetJacobianRoutine(*tao,*J,*Jp,ourtaojacobianroutine,ctx);
}

PETSC_EXTERN void PETSC_STDCALL taosetjacobianstateroutine_(Tao *tao, Mat *J, Mat *Jp, Mat*Jinv, void (PETSC_STDCALL *func)(Tao*, Vec *, Mat *, Mat *, Mat*, void *, PetscErrorCode *), void *ctx, PetscErrorCode *ierr)
{
    CHKFORTRANNULLFUNCTION(func);
    *ierr = PetscObjectSetFortranCallback((PetscObject)*tao,PETSC_FORTRAN_CALLBACK_CLASS,&_cb.jacstate,(PetscVoidFunction)func,ctx);
    if(!*ierr) *ierr = TaoSetJacobianStateRoutine(*tao,*J,*Jp,*Jinv,ourtaojacobianstateroutine,ctx);
}

PETSC_EXTERN void PETSC_STDCALL taosetjacobiandesignroutine_(Tao *tao, Mat *J, void (PETSC_STDCALL *func)(Tao*, Vec *, Mat *, void *, PetscErrorCode *), void *ctx, PetscErrorCode *ierr)
{
    CHKFORTRANNULLFUNCTION(func);
    *ierr = PetscObjectSetFortranCallback((PetscObject)tao,PETSC_FORTRAN_CALLBACK_CLASS,&_cb.jacdesign,(PetscVoidFunction)func,ctx);
    if(!*ierr) *ierr = TaoSetJacobianDesignRoutine(*tao,*J,ourtaojacobiandesignroutine,ctx);
}

PETSC_EXTERN void PETSC_STDCALL taosethessianroutine_(Tao *tao, Mat *J, Mat *Jp, void (PETSC_STDCALL *func)(Tao*, Vec *, Mat *, Mat *,void *, PetscErrorCode *), void *ctx, PetscErrorCode *ierr)
{
    CHKFORTRANNULLFUNCTION(func);
    *ierr = PetscObjectSetFortranCallback((PetscObject)*tao,PETSC_FORTRAN_CALLBACK_CLASS,&_cb.hess,(PetscVoidFunction)func,ctx);
    if(!*ierr) *ierr = TaoSetHessianRoutine(*tao,*J, *Jp, ourtaohessianroutine,ctx);
}

PETSC_EXTERN void PETSC_STDCALL taosetvariableboundsroutine_(Tao *tao, void (PETSC_STDCALL *func)(Tao*,Vec*,Vec*,void*,PetscErrorCode*),void *ctx, PetscErrorCode *ierr)
{
    CHKFORTRANNULLFUNCTION(func);
    *ierr = PetscObjectSetFortranCallback((PetscObject)*tao,PETSC_FORTRAN_CALLBACK_CLASS,&_cb.bounds,(PetscVoidFunction)func,ctx);
    if(!*ierr) *ierr = TaoSetVariableBoundsRoutine(*tao,ourtaoboundsroutine,ctx);
}

PETSC_EXTERN void PETSC_STDCALL taosetmonitor_(Tao *tao, void (PETSC_STDCALL *func)(Tao*,void*,PetscErrorCode*),void *ctx, void (PETSC_STDCALL *mondestroy)(void*,PetscErrorCode*),PetscErrorCode *ierr)
{
    CHKFORTRANNULLFUNCTION(mondestroy);
    *ierr = PetscObjectSetFortranCallback((PetscObject)*tao,PETSC_FORTRAN_CALLBACK_CLASS,&_cb.mon,(PetscVoidFunction)func,ctx); if (*ierr) return;
    *ierr = PetscObjectSetFortranCallback((PetscObject)*tao,PETSC_FORTRAN_CALLBACK_CLASS,&_cb.mondestroy,(PetscVoidFunction)mondestroy,ctx); if (*ierr) return;
    *ierr = TaoSetMonitor(*tao,ourtaomonitor,*tao,ourtaomondestroy);
}

PETSC_EXTERN void PETSC_STDCALL taosetconvergencetest_(Tao *tao, void (PETSC_STDCALL *func)(Tao*,void*,PetscErrorCode*),void *ctx, PetscErrorCode *ierr)
{
    CHKFORTRANNULLFUNCTION(func);
    *ierr = PetscObjectSetFortranCallback((PetscObject)*tao,PETSC_FORTRAN_CALLBACK_CLASS,&_cb.convtest,(PetscVoidFunction)func,ctx);
    if(!*ierr) *ierr = TaoSetConvergenceTest(*tao,ourtaoconvergencetest,ctx);
}

PETSC_EXTERN void PETSC_STDCALL taosetconstraintsroutine_(Tao *tao, Vec *C, void (PETSC_STDCALL *func)(Tao*, Vec *, Vec *, void *, PetscErrorCode *), void *ctx, PetscErrorCode *ierr)
{
    CHKFORTRANNULLFUNCTION(func);
    *ierr = PetscObjectSetFortranCallback((PetscObject)*tao,PETSC_FORTRAN_CALLBACK_CLASS,&_cb.constraints,(PetscVoidFunction)func,ctx);
    if(!*ierr) *ierr = TaoSetConstraintsRoutine(*tao,*C,ourtaoconstraintsroutine,ctx);
}

PETSC_EXTERN void PETSC_STDCALL taosettype_(Tao *tao, char* type_name PETSC_MIXED_LEN(len), PetscErrorCode *ierr PETSC_END_LEN(len))
{
    char *t;

    FIXCHAR(type_name,len,t);
    *ierr = TaoSetType(*tao,t);
    FREECHAR(type_name,t);

}

PETSC_EXTERN void PETSC_STDCALL taoview_(Tao *tao, PetscViewer *viewer, PetscErrorCode *ierr)
{
    PetscViewer v;
    PetscPatchDefaultViewers_Fortran(viewer,v);
    *ierr = TaoView(*tao,v);
}

PETSC_EXTERN void PETSC_STDCALL taogetconvergencehistory_(Tao *tao, PetscInt *nhist, PetscErrorCode *ierr)
{
  *ierr = TaoGetConvergenceHistory(*tao,NULL,NULL,NULL,NULL,nhist);
}

PETSC_EXTERN void PETSC_STDCALL taogetoptionsprefix_(Tao *tao, char* prefix PETSC_MIXED_LEN(len), PetscErrorCode *ierr PETSC_END_LEN(len))
{
  const char *name;
  *ierr = TaoGetOptionsPrefix(*tao,&name);
  *ierr = PetscStrncpy(prefix,name,len); if (*ierr) return;
  FIXRETURNCHAR(PETSC_TRUE,prefix,len);

}

PETSC_EXTERN void PETSC_STDCALL taoappendoptionsprefix_(Tao *tao, char* prefix PETSC_MIXED_LEN(len),PetscErrorCode *ierr PETSC_END_LEN(len))
{
  char *name;
  FIXCHAR(prefix,len,name);
  *ierr = TaoAppendOptionsPrefix(*tao,name);
  FREECHAR(prefix,name);
}

PETSC_EXTERN void PETSC_STDCALL taosetoptionsprefix_(Tao *tao, char* prefix PETSC_MIXED_LEN(len),PetscErrorCode *ierr PETSC_END_LEN(len))
{
  char *t;
  FIXCHAR(prefix,len,t);
  *ierr = TaoSetOptionsPrefix(*tao,t);
  FREECHAR(prefix,t);
}

PETSC_EXTERN void PETSC_STDCALL taogettype_(Tao *tao, char* name PETSC_MIXED_LEN(len), PetscErrorCode *ierr  PETSC_END_LEN(len))
{
  const char *tname;
  *ierr = TaoGetType(*tao,&tname);
  *ierr = PetscStrncpy(name,tname,len); if (*ierr) return;
  FIXRETURNCHAR(PETSC_TRUE,name,len);

}

PETSC_EXTERN void PETSC_STDCALL taosetjacobianinequalityroutine_(Tao *tao, Mat *J, Mat *Jp, void (PETSC_STDCALL *func)(Tao*, Vec *, Mat *, Mat *,void *, PetscErrorCode *), void *ctx, PetscErrorCode *ierr)
{
    CHKFORTRANNULLFUNCTION(func);
    *ierr = PetscObjectSetFortranCallback((PetscObject)*tao,PETSC_FORTRAN_CALLBACK_CLASS,&_cb.jacineq,(PetscVoidFunction)func,ctx);
    if(!*ierr) *ierr = TaoSetJacobianInequalityRoutine(*tao,*J,*Jp,ourtaojacobianinequalityroutine,ctx);
}

PETSC_EXTERN void PETSC_STDCALL taosetjacobianequalityroutine_(Tao *tao, Mat *J, Mat *Jp, void (PETSC_STDCALL *func)(Tao*, Vec *, Mat *, Mat *, void *, PetscErrorCode *), void *ctx, PetscErrorCode *ierr)
{
    CHKFORTRANNULLFUNCTION(func);
    *ierr = PetscObjectSetFortranCallback((PetscObject)*tao,PETSC_FORTRAN_CALLBACK_CLASS,&_cb.jaceq,(PetscVoidFunction)func,ctx);
    if(!*ierr) *ierr = TaoSetJacobianEqualityRoutine(*tao,*J,*Jp,ourtaojacobianequalityroutine,ctx);
}

PETSC_EXTERN void PETSC_STDCALL taosetinequalityconstraintsroutine_(Tao *tao, Vec *C, void (PETSC_STDCALL *func)(Tao*, Vec *, Vec *, void *, PetscErrorCode *), void *ctx, PetscErrorCode *ierr)
{
    CHKFORTRANNULLFUNCTION(func);
    *ierr = PetscObjectSetFortranCallback((PetscObject)*tao,PETSC_FORTRAN_CALLBACK_CLASS,&_cb.conineq,(PetscVoidFunction)func,ctx);
    if(!*ierr) *ierr = TaoSetInequalityConstraintsRoutine(*tao,*C,ourtaoinequalityconstraintsroutine,ctx);
}

PETSC_EXTERN void PETSC_STDCALL taosetequalityconstraintsroutine_(Tao *tao, Vec *C, void (PETSC_STDCALL *func)(Tao*, Vec *, Vec *, void *, PetscErrorCode *), void *ctx, PetscErrorCode *ierr)
{
    CHKFORTRANNULLFUNCTION(func);
    *ierr = PetscObjectSetFortranCallback((PetscObject)*tao,PETSC_FORTRAN_CALLBACK_CLASS,&_cb.coneq,(PetscVoidFunction)func,ctx);
    if(!*ierr) *ierr = TaoSetEqualityConstraintsRoutine(*tao, *C, ourtaoequalityconstraintsroutine,ctx);
}

EXTERN_C_END


