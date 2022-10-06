
static char help[] = "Adjoint and tangent linear sensitivity analysis of the basic equation for generator stability analysis.\n";

/*F

\begin{eqnarray}
                 \frac{d \theta}{dt} = \omega_b (\omega - \omega_s)
                 \frac{2 H}{\omega_s}\frac{d \omega}{dt} & = & P_m - P_max \sin(\theta) -D(\omega - \omega_s)\\
\end{eqnarray}

F*/

/*
  This code demonstrate the sensitivity analysis interface to a system of ordinary differential equations with discontinuities.
  It computes the sensitivities of an integral cost function
  \int c*max(0,\theta(t)-u_s)^beta dt
  w.r.t. initial conditions and the parameter P_m.
  Backward Euler method is used for time integration.
  The discontinuities are detected with TSEvent.
 */

#include <petscts.h>

typedef struct {
  PetscScalar H,D,omega_b,omega_s,Pmax,Pmax_ini,Pm,E,V,X,u_s,c;
  PetscInt    beta;
  PetscReal   tf,tcl;
} AppCtx;

typedef enum {SA_ADJ, SA_TLM} SAMethod;
static const char *const SAMethods[] = {"ADJ","TLM","SAMethod","SA_",0};

/* Event check */
PetscErrorCode EventFunction(TS ts,PetscReal t,Vec X,PetscScalar *fvalue,void *ctx)
{
  AppCtx        *user=(AppCtx*)ctx;

  PetscFunctionBegin;
  /* Event for fault-on time */
  fvalue[0] = t - user->tf;
  /* Event for fault-off time */
  fvalue[1] = t - user->tcl;

  PetscFunctionReturn(0);
}

PetscErrorCode PostEventFunction(TS ts,PetscInt nevents,PetscInt event_list[],PetscReal t,Vec X,PetscBool forwardsolve,void* ctx)
{
  AppCtx *user=(AppCtx*)ctx;

  PetscFunctionBegin;

  if (event_list[0] == 0) {
    if (forwardsolve) user->Pmax = 0.0; /* Apply disturbance - this is done by setting Pmax = 0 */
    else user->Pmax = user->Pmax_ini; /* Going backward, reversal of event */
  } else if(event_list[0] == 1) {
    if (forwardsolve) user->Pmax = user->Pmax_ini; /* Remove the fault  - this is done by setting Pmax = Pmax_ini */
    else user->Pmax = 0.0; /* Going backward, reversal of event */
  }
  PetscFunctionReturn(0);
}

PetscErrorCode PostStepFunction(TS ts)
{
  PetscErrorCode    ierr;
  Vec               U;
  PetscReal         t;
  const PetscScalar *u;

  PetscFunctionBegin;
  ierr = TSGetTime(ts,&t);CHKERRQ(ierr);
  ierr = TSGetSolution(ts,&U);CHKERRQ(ierr);
  ierr = VecGetArrayRead(U,&u);CHKERRQ(ierr);
  ierr = PetscPrintf(PETSC_COMM_SELF,"delta(%3.2f) = %8.7f\n",(double)t,(double)u[0]);CHKERRQ(ierr);
  ierr = VecRestoreArrayRead(U,&u);CHKERRQ(ierr);

  PetscFunctionReturn(0);
}

/*
     Defines the ODE passed to the ODE solver
*/
static PetscErrorCode RHSFunction(TS ts,PetscReal t,Vec U,Vec F,AppCtx *ctx)
{
  PetscErrorCode    ierr;
  PetscScalar       *f,Pmax;
  const PetscScalar *u;

  PetscFunctionBegin;
  /*  The next three lines allow us to access the entries of the vectors directly */
  ierr = VecGetArrayRead(U,&u);CHKERRQ(ierr);
  ierr = VecGetArray(F,&f);CHKERRQ(ierr);
  Pmax = ctx->Pmax;
  f[0] = ctx->omega_b*(u[1] - ctx->omega_s);
  f[1] = ctx->omega_s/(2.0*ctx->H)*(ctx->Pm - Pmax*PetscSinScalar(u[0]) - ctx->D*(u[1] - ctx->omega_s));

  ierr = VecRestoreArrayRead(U,&u);CHKERRQ(ierr);
  ierr = VecRestoreArray(F,&f);CHKERRQ(ierr);
  PetscFunctionReturn(0);
}

/*
     Defines the Jacobian of the ODE passed to the ODE solver. See TSSetRHSJacobian() for the meaning of a and the Jacobian.
*/
static PetscErrorCode RHSJacobian(TS ts,PetscReal t,Vec U,Mat A,Mat B,AppCtx *ctx)
{
  PetscErrorCode    ierr;
  PetscInt          rowcol[] = {0,1};
  PetscScalar       J[2][2],Pmax;
  const PetscScalar *u;

  PetscFunctionBegin;
  ierr    = VecGetArrayRead(U,&u);CHKERRQ(ierr);
  Pmax    = ctx->Pmax;
  J[0][0] = 0;
  J[0][1] = ctx->omega_b;
  J[1][0] = -ctx->omega_s/(2.0*ctx->H)*Pmax*PetscCosScalar(u[0]);
  J[1][1] = -ctx->omega_s/(2.0*ctx->H)*ctx->D;
  ierr    = MatSetValues(B,2,rowcol,2,rowcol,&J[0][0],INSERT_VALUES);CHKERRQ(ierr);
  ierr    = VecRestoreArrayRead(U,&u);CHKERRQ(ierr);

  ierr = MatAssemblyBegin(A,MAT_FINAL_ASSEMBLY);CHKERRQ(ierr);
  ierr = MatAssemblyEnd(A,MAT_FINAL_ASSEMBLY);CHKERRQ(ierr);
  if (A != B) {
    ierr = MatAssemblyBegin(B,MAT_FINAL_ASSEMBLY);CHKERRQ(ierr);
    ierr = MatAssemblyEnd(B,MAT_FINAL_ASSEMBLY);CHKERRQ(ierr);
  }
  PetscFunctionReturn(0);
}

static PetscErrorCode RHSJacobianP(TS ts,PetscReal t,Vec X,Mat A,void *ctx0)
{
  PetscErrorCode ierr;
  PetscInt       row[] = {0,1},col[] = {0};
  PetscScalar    *x,J[2][1];
  AppCtx         *ctx = (AppCtx*)ctx0;

  PetscFunctionBeginUser;
  ierr    = VecGetArray(X,&x);CHKERRQ(ierr);
  J[0][0] = 0;
  J[1][0] = ctx->omega_s/(2.0*ctx->H);
  ierr    = MatSetValues(A,2,row,1,col,&J[0][0],INSERT_VALUES);CHKERRQ(ierr);

  ierr = MatAssemblyBegin(A,MAT_FINAL_ASSEMBLY);CHKERRQ(ierr);
  ierr = MatAssemblyEnd(A,MAT_FINAL_ASSEMBLY);CHKERRQ(ierr);
  PetscFunctionReturn(0);
}

static PetscErrorCode CostIntegrand(TS ts,PetscReal t,Vec U,Vec R,AppCtx *ctx)
{
  PetscErrorCode    ierr;
  PetscScalar       *r;
  const PetscScalar *u;

  PetscFunctionBegin;
  ierr = VecGetArrayRead(U,&u);CHKERRQ(ierr);
  ierr = VecGetArray(R,&r);CHKERRQ(ierr);
  r[0] = ctx->c*PetscPowScalarInt(PetscMax(0., u[0]-ctx->u_s),ctx->beta);CHKERRQ(ierr);
  ierr = VecRestoreArray(R,&r);CHKERRQ(ierr);
  ierr = VecRestoreArrayRead(U,&u);CHKERRQ(ierr);
  PetscFunctionReturn(0);
}

static PetscErrorCode DRDYFunction(TS ts,PetscReal t,Vec U,Vec *drdy,AppCtx *ctx)
{
  PetscErrorCode    ierr;
  PetscScalar       *ry;
  const PetscScalar *u;

  PetscFunctionBegin;
  ierr = VecGetArrayRead(U,&u);CHKERRQ(ierr);
  ierr = VecGetArray(drdy[0],&ry);CHKERRQ(ierr);
  ry[0] = ctx->c*ctx->beta*PetscPowScalarInt(PetscMax(0., u[0]-ctx->u_s),ctx->beta-1);CHKERRQ(ierr);
  ierr  = VecRestoreArray(drdy[0],&ry);CHKERRQ(ierr);
  ierr = VecRestoreArrayRead(U,&u);CHKERRQ(ierr);
  PetscFunctionReturn(0);
}

static PetscErrorCode DRDPFunction(TS ts,PetscReal t,Vec U,Vec *drdp,AppCtx *ctx)
{
  PetscErrorCode    ierr;
  PetscScalar       *rp;
  const PetscScalar *u;

  PetscFunctionBegin;
  ierr = VecGetArrayRead(U,&u);CHKERRQ(ierr);
  ierr = VecGetArray(drdp[0],&rp);CHKERRQ(ierr);
  rp[0] = 0.;
  ierr = VecRestoreArray(drdp[0],&rp);CHKERRQ(ierr);
  ierr = VecGetArrayRead(U,&u);CHKERRQ(ierr);
  PetscFunctionReturn(0);
}

PetscErrorCode ComputeSensiP(Vec lambda,Vec mu,PetscScalar *val,AppCtx *ctx)
{
  PetscErrorCode    ierr;
  const PetscScalar *x,*y;

  PetscFunctionBegin;
  ierr = VecGetArrayRead(lambda,&x);CHKERRQ(ierr);
  ierr = VecGetArrayRead(mu,&y);CHKERRQ(ierr);
  val[0] = 1./PetscSqrtScalar(1.-(ctx->Pm/ctx->Pmax)*(ctx->Pm/ctx->Pmax))/ctx->Pmax*x[0]+y[0];
  ierr = VecRestoreArrayRead(lambda,&x);CHKERRQ(ierr);
  ierr = VecRestoreArrayRead(mu,&y);CHKERRQ(ierr);
  PetscFunctionReturn(0);
}

int main(int argc,char **argv)
{
  TS             ts;            /* ODE integrator */
  Vec            U;             /* solution will be stored here */
  Mat            A;             /* Jacobian matrix */
  Mat            Jacp;          /* JacobianP matrix */
  PetscErrorCode ierr;
  PetscMPIInt    size;
  PetscInt       n = 2;
  AppCtx         ctx;
  PetscScalar    *u;
  PetscReal      du[2] = {0.0,0.0};
  PetscBool      ensemble = PETSC_FALSE,flg1,flg2;
  PetscReal      ftime;
  PetscInt       steps;
  PetscScalar    *x_ptr,*y_ptr,*s_ptr;
  Vec            lambda[1],q,mu[1];
  PetscInt       direction[2];
  PetscBool      terminate[2];
  Vec            qgrad[1];      /* Forward sesivitiy */
  Mat            sp;            /* Forward sensitivity matrix */
  SAMethod       sa;

  /* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     Initialize program
     - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
  ierr = PetscInitialize(&argc,&argv,(char*)0,help);if (ierr) return ierr;
  ierr = MPI_Comm_size(PETSC_COMM_WORLD,&size);CHKERRQ(ierr);
  if (size > 1) SETERRQ(PETSC_COMM_WORLD,PETSC_ERR_SUP,"Only for sequential runs");

  /* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    Create necessary matrix and vectors
    - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
  ierr = MatCreate(PETSC_COMM_WORLD,&A);CHKERRQ(ierr);
  ierr = MatSetSizes(A,n,n,PETSC_DETERMINE,PETSC_DETERMINE);CHKERRQ(ierr);
  ierr = MatSetType(A,MATDENSE);CHKERRQ(ierr);
  ierr = MatSetFromOptions(A);CHKERRQ(ierr);
  ierr = MatSetUp(A);CHKERRQ(ierr);

  ierr = MatCreateVecs(A,&U,NULL);CHKERRQ(ierr);

  ierr = MatCreate(PETSC_COMM_WORLD,&Jacp);CHKERRQ(ierr);
  ierr = MatSetSizes(Jacp,PETSC_DECIDE,PETSC_DECIDE,2,1);CHKERRQ(ierr);
  ierr = MatSetFromOptions(Jacp);CHKERRQ(ierr);
  ierr = MatSetUp(Jacp);CHKERRQ(ierr);

  /* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    Set runtime options
    - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
  ierr = PetscOptionsBegin(PETSC_COMM_WORLD,NULL,"Swing equation options","");CHKERRQ(ierr);
  {
    ctx.beta    = 2;
    ctx.c       = 10000.0;
    ctx.u_s     = 1.0;
    ctx.omega_s = 1.0;
    ctx.omega_b = 120.0*PETSC_PI;
    ctx.H       = 5.0;
    ierr        = PetscOptionsScalar("-Inertia","","",ctx.H,&ctx.H,NULL);CHKERRQ(ierr);
    ctx.D       = 5.0;
    ierr        = PetscOptionsScalar("-D","","",ctx.D,&ctx.D,NULL);CHKERRQ(ierr);
    ctx.E       = 1.1378;
    ctx.V       = 1.0;
    ctx.X       = 0.545;
    ctx.Pmax    = ctx.E*ctx.V/ctx.X;
    ctx.Pmax_ini = ctx.Pmax;
    ierr        = PetscOptionsScalar("-Pmax","","",ctx.Pmax,&ctx.Pmax,NULL);CHKERRQ(ierr);
    ctx.Pm      = 1.1;
    ierr        = PetscOptionsScalar("-Pm","","",ctx.Pm,&ctx.Pm,NULL);CHKERRQ(ierr);
    ctx.tf      = 0.1;
    ctx.tcl     = 0.2;
    ierr        = PetscOptionsReal("-tf","Time to start fault","",ctx.tf,&ctx.tf,NULL);CHKERRQ(ierr);
    ierr        = PetscOptionsReal("-tcl","Time to end fault","",ctx.tcl,&ctx.tcl,NULL);CHKERRQ(ierr);
    ierr        = PetscOptionsBool("-ensemble","Run ensemble of different initial conditions","",ensemble,&ensemble,NULL);CHKERRQ(ierr);
    if (ensemble) {
      ctx.tf      = -1;
      ctx.tcl     = -1;
    }

    ierr = VecGetArray(U,&u);CHKERRQ(ierr);
    u[0] = PetscAsinScalar(ctx.Pm/ctx.Pmax);
    u[1] = 1.0;
    ierr = PetscOptionsRealArray("-u","Initial solution","",u,&n,&flg1);CHKERRQ(ierr);
    n    = 2;
    ierr = PetscOptionsRealArray("-du","Perturbation in initial solution","",du,&n,&flg2);CHKERRQ(ierr);
    u[0] += du[0];
    u[1] += du[1];
    ierr = VecRestoreArray(U,&u);CHKERRQ(ierr);
    if (flg1 || flg2) {
      ctx.tf      = -1;
      ctx.tcl     = -1;
    }
    sa = SA_ADJ;
    ierr = PetscOptionsEnum("-sa_method","Sensitivity analysis method (adj or tlm)","",SAMethods,(PetscEnum)sa,(PetscEnum*)&sa,NULL);CHKERRQ(ierr);
  }
  ierr = PetscOptionsEnd();CHKERRQ(ierr);

  /* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     Create timestepping solver context
     - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
  ierr = TSCreate(PETSC_COMM_WORLD,&ts);CHKERRQ(ierr);
  ierr = TSSetProblemType(ts,TS_NONLINEAR);CHKERRQ(ierr);
  ierr = TSSetType(ts,TSBEULER);CHKERRQ(ierr);
  ierr = TSSetRHSFunction(ts,NULL,(TSRHSFunction) RHSFunction,&ctx);CHKERRQ(ierr);
  ierr = TSSetRHSJacobian(ts,A,A,(TSRHSJacobian)RHSJacobian,&ctx);CHKERRQ(ierr);

  /* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     Set initial conditions
   - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
  ierr = TSSetSolution(ts,U);CHKERRQ(ierr);

  /*   Set RHS JacobianP */
  ierr = TSSetRHSJacobianP(ts,Jacp,RHSJacobianP,&ctx);CHKERRQ(ierr);
  if (sa == SA_ADJ) {
    /* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      Save trajectory of solution so that TSAdjointSolve() may be used
     - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
    ierr = TSSetSaveTrajectory(ts);CHKERRQ(ierr);

    ierr = MatCreateVecs(A,&lambda[0],NULL);CHKERRQ(ierr);
    ierr = MatCreateVecs(Jacp,&mu[0],NULL);CHKERRQ(ierr);
    ierr = TSSetCostGradients(ts,1,lambda,mu);CHKERRQ(ierr);
    ierr = TSSetCostIntegrand(ts,1,NULL,(PetscErrorCode (*)(TS,PetscReal,Vec,Vec,void*))CostIntegrand,
                                        (PetscErrorCode (*)(TS,PetscReal,Vec,Vec*,void*))DRDYFunction,
                                        (PetscErrorCode (*)(TS,PetscReal,Vec,Vec*,void*))DRDPFunction,PETSC_TRUE,&ctx);CHKERRQ(ierr);
  }

  if (sa == SA_TLM) {
    PetscScalar val[2];
    PetscInt    row[]={0,1},col[]={0};

    ierr = VecCreate(PETSC_COMM_WORLD,&qgrad[0]);CHKERRQ(ierr);
    ierr = VecSetSizes(qgrad[0],PETSC_DECIDE,1);CHKERRQ(ierr);
    ierr = VecSetFromOptions(qgrad[0]);CHKERRQ(ierr);

    ierr = MatCreateDense(PETSC_COMM_WORLD,PETSC_DECIDE,PETSC_DECIDE,2,1,NULL,&sp);CHKERRQ(ierr);
    ierr = TSForwardSetSensitivities(ts,1,sp);CHKERRQ(ierr);
    ierr = TSForwardSetIntegralGradients(ts,1,qgrad);CHKERRQ(ierr);
    ierr = TSSetCostIntegrand(ts,1,NULL,(PetscErrorCode (*)(TS,PetscReal,Vec,Vec,void*))CostIntegrand,
                                        (PetscErrorCode (*)(TS,PetscReal,Vec,Vec*,void*))DRDYFunction,
                                        (PetscErrorCode (*)(TS,PetscReal,Vec,Vec*,void*))DRDPFunction,PETSC_TRUE,&ctx);CHKERRQ(ierr);
    val[0] = 1./PetscSqrtScalar(1.-(ctx.Pm/ctx.Pmax)*(ctx.Pm/ctx.Pmax))/ctx.Pmax;
    val[1] = 0.0;
    ierr = MatSetValues(sp,2,row,1,col,val,INSERT_VALUES);CHKERRQ(ierr);
    ierr = MatAssemblyBegin(sp,MAT_FINAL_ASSEMBLY);CHKERRQ(ierr);
    ierr = MatAssemblyEnd(sp,MAT_FINAL_ASSEMBLY);CHKERRQ(ierr);
  }

  /* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     Set solver options
   - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
  ierr = TSSetMaxTime(ts,1.0);CHKERRQ(ierr);
  ierr = TSSetExactFinalTime(ts,TS_EXACTFINALTIME_MATCHSTEP);CHKERRQ(ierr);
  ierr = TSSetTimeStep(ts,0.03125);CHKERRQ(ierr);
  ierr = TSSetFromOptions(ts);CHKERRQ(ierr);

  direction[0] = direction[1] = 1;
  terminate[0] = terminate[1] = PETSC_FALSE;

  ierr = TSSetEventHandler(ts,2,direction,terminate,EventFunction,PostEventFunction,(void*)&ctx);CHKERRQ(ierr);

  /* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     Solve nonlinear system
     - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
  if (ensemble) {
    for (du[1] = -2.5; du[1] <= .01; du[1] += .1) {
      ierr = VecGetArray(U,&u);CHKERRQ(ierr);
      u[0] = PetscAsinScalar(ctx.Pm/ctx.Pmax);
      u[1] = ctx.omega_s;
      u[0] += du[0];
      u[1] += du[1];
      ierr = VecRestoreArray(U,&u);CHKERRQ(ierr);
      ierr = TSSetTimeStep(ts,0.03125);CHKERRQ(ierr);
      ierr = TSSolve(ts,U);CHKERRQ(ierr);
    }
  } else {
    ierr = TSSolve(ts,U);CHKERRQ(ierr);
  }
  ierr = TSGetSolveTime(ts,&ftime);CHKERRQ(ierr);
  ierr = TSGetStepNumber(ts,&steps);CHKERRQ(ierr);

  if (sa == SA_ADJ) {
    PetscScalar grad[1];
    /* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       Adjoint model starts here
       - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
    /*   Set initial conditions for the adjoint integration */
    ierr = VecGetArray(lambda[0],&y_ptr);CHKERRQ(ierr);
    y_ptr[0] = 0.0; y_ptr[1] = 0.0;
    ierr = VecRestoreArray(lambda[0],&y_ptr);CHKERRQ(ierr);

    ierr = VecGetArray(mu[0],&x_ptr);CHKERRQ(ierr);
    x_ptr[0] = 0.0;
    ierr = VecRestoreArray(mu[0],&x_ptr);CHKERRQ(ierr);

    ierr = TSAdjointSolve(ts);CHKERRQ(ierr);

    ierr = PetscPrintf(PETSC_COMM_WORLD,"\n lambda: d[Psi(tf)]/d[phi0]  d[Psi(tf)]/d[omega0]\n");CHKERRQ(ierr);
    ierr = VecView(lambda[0],PETSC_VIEWER_STDOUT_WORLD);CHKERRQ(ierr);
    ierr = PetscPrintf(PETSC_COMM_WORLD,"\n mu: d[Psi(tf)]/d[pm]  d[Psi(tf)]/d[pm]\n");CHKERRQ(ierr);
    ierr = VecView(mu[0],PETSC_VIEWER_STDOUT_WORLD);CHKERRQ(ierr);
    ierr = TSGetCostIntegral(ts,&q);CHKERRQ(ierr);
    ierr = VecGetArray(q,&x_ptr);CHKERRQ(ierr);
    ierr = PetscPrintf(PETSC_COMM_WORLD,"\n cost function=%g\n",(double)(x_ptr[0]-ctx.Pm));CHKERRQ(ierr);
    ierr = VecRestoreArray(q,&x_ptr);CHKERRQ(ierr);
    ierr = ComputeSensiP(lambda[0],mu[0],grad,&ctx);CHKERRQ(ierr);
    ierr = PetscPrintf(PETSC_COMM_WORLD,"\n gradient=%g\n",(double)grad[0]);CHKERRQ(ierr);
    ierr = VecDestroy(&lambda[0]);CHKERRQ(ierr);
    ierr = VecDestroy(&mu[0]);CHKERRQ(ierr);
  }

  if (sa == SA_TLM) {
    ierr = PetscPrintf(PETSC_COMM_WORLD,"\n trajectory sensitivity: d[Psi(tf)]/d[pm]  d[Psi(tf)]/d[pm]\n");CHKERRQ(ierr);
    ierr = MatView(sp,PETSC_VIEWER_STDOUT_WORLD);CHKERRQ(ierr);
    ierr = TSGetCostIntegral(ts,&q);CHKERRQ(ierr);
    ierr = VecGetArray(q,&s_ptr);CHKERRQ(ierr);
    ierr = PetscPrintf(PETSC_COMM_WORLD,"\n cost function=%g\n",(double)(s_ptr[0]-ctx.Pm));CHKERRQ(ierr);
    ierr = VecRestoreArray(q,&s_ptr);CHKERRQ(ierr);
    ierr = VecGetArray(qgrad[0],&s_ptr);CHKERRQ(ierr);
    ierr = PetscPrintf(PETSC_COMM_WORLD,"\n gradient=%g\n",(double)s_ptr[0]);CHKERRQ(ierr);
    ierr = VecRestoreArray(qgrad[0],&s_ptr);CHKERRQ(ierr);
    ierr = VecDestroy(&qgrad[0]);CHKERRQ(ierr);
    ierr = MatDestroy(&sp);CHKERRQ(ierr);
  }
  /* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     Free work space.  All PETSc objects should be destroyed when they are no longer needed.
   - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
  ierr = MatDestroy(&A);CHKERRQ(ierr);
  ierr = MatDestroy(&Jacp);CHKERRQ(ierr);
  ierr = VecDestroy(&U);CHKERRQ(ierr);
  ierr = TSDestroy(&ts);CHKERRQ(ierr);
  ierr = PetscFinalize();
  return ierr;
}


/*TEST

   build:
      requires: !complex !single

   test:
      args: -sa_method adj -viewer_binary_skip_info -ts_type cn -pc_type lu

   test:
      suffix: 2
      args: -sa_method tlm -ts_type cn -pc_type lu

   test:
      suffix: 3
      args: -sa_method adj -ts_type rk -ts_rk_type 2a -ts_adapt_type dsp

TEST*/
