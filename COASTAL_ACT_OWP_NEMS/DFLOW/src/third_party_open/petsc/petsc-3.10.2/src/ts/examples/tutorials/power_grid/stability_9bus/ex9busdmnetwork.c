
static char help[] = "Power grid stability analysis of a large power system.\n\
The base case is the WECC 9 bus system based on the example given in the book Power\n\
Systems Dynamics and Stability (Chapter 7) by P. Sauer and M. A. Pai.\n\
The base power grid in this example consists of 9 buses (nodes), 3 generators,\n\
3 loads, and 9 transmission lines. The network equations are written\n\
in current balance form using rectangular coordiantes. \n\
DMNetwork is used to manage the variables and equations of the entire system.\n\
Input parameters include:\n\
  -nc : number of copies of the base case\n\n";

/* T
   Concepts: DMNetwork
   Concepts: PETSc TS solver

   This example was contributed by Bikiran Guha and Jianqiao Huang, Illinois Institute of Technology, 2017.
*/

#include <petscts.h>
#include <petscdmnetwork.h>

#define FREQ 60
#define W_S (2*PETSC_PI*FREQ)
#define NGEN    3   /* No. of generators in the 9 bus system */
#define NLOAD   3   /* No. of loads in the 9 bus system */
#define NBUS    9   /* No. of buses in the 9 bus system */
#define NBRANCH 9   /* No. of branches in the 9 bus system */

typedef struct {
  PetscInt    id;    /* Bus Number or extended bus name*/
  PetscScalar mbase; /* MVA base of the machine */
  PetscScalar PG;    /* Generator active power output */
  PetscScalar QG;    /* Generator reactive power output */

  /* Generator constants */
  PetscScalar H;    /* Inertia constant */
  PetscScalar Rs;   /* Stator Resistance */
  PetscScalar Xd;   /* d-axis reactance */
  PetscScalar Xdp;  /* d-axis transient reactance */
  PetscScalar Xq;   /* q-axis reactance Xq(1) set to 0.4360, value given in text 0.0969 */
  PetscScalar Xqp;  /* q-axis transient reactance */
  PetscScalar Td0p; /* d-axis open circuit time constant */
  PetscScalar Tq0p; /* q-axis open circuit time constant */
  PetscScalar M;    /* M = 2*H/W_S */
  PetscScalar D;    /* D = 0.1*M */
  PetscScalar TM;   /* Mechanical Torque */

  /* Exciter system constants */
  PetscScalar KA ;   /* Voltage regulator gain constant */
  PetscScalar TA;    /* Voltage regulator time constant */
  PetscScalar KE;    /* Exciter gain constant */
  PetscScalar TE;    /* Exciter time constant */
  PetscScalar KF;    /* Feedback stabilizer gain constant */
  PetscScalar TF;    /* Feedback stabilizer time constant */
  PetscScalar k1,k2; /* calculating the saturation function SE = k1*exp(k2*Efd) */
  PetscScalar Vref;  /* Voltage regulator voltage setpoint */
} Gen;

typedef struct {
  PetscInt     id;      /* node id */
  PetscInt     nofgen;  /* Number of generators at the bus*/
  PetscInt     nofload; /*  Number of load at the bus*/
  PetscScalar  yff[2]; /* yff[0]= imaginary part of admittance, yff[1]=real part of admittance*/
  PetscScalar  vr;     /* Real component of bus voltage */
  PetscScalar  vi;     /* Imaginary component of bus voltage */
} Bus;

  /* Load constants
  We use a composite load model that describes the load and reactive powers at each time instant as follows
  P(t) = \sum\limits_{i=0}^ld_nsegsp \ld_alphap_i*P_D0(\frac{V_m(t)}{V_m0})^\ld_betap_i
  Q(t) = \sum\limits_{i=0}^ld_nsegsq \ld_alphaq_i*Q_D0(\frac{V_m(t)}{V_m0})^\ld_betaq_i
  where
    id                  - index of the load
    ld_nsegsp,ld_nsegsq - Number of individual load models for real and reactive power loads
    ld_alphap,ld_alphap - Percentage contribution (weights) or loads
    P_D0                - Real power load
    Q_D0                - Reactive power load
    Vm(t)              - Voltage magnitude at time t
    Vm0                - Voltage magnitude at t = 0
    ld_betap, ld_betaq  - exponents describing the load model for real and reactive part

    Note: All loads have the same characteristic currently.
  */
typedef struct {
  PetscInt    id;           /* bus id */
  PetscInt    ld_nsegsp,ld_nsegsq;
  PetscScalar PD0, QD0;
  PetscScalar ld_alphap[3]; /* ld_alphap=[1,0,0], an array, not a value, so use *ld_alphap; */
  PetscScalar ld_betap[3],ld_alphaq[3],ld_betaq[3];
} Load;

typedef struct {
  PetscInt    id;     /* node id */
  PetscScalar yft[2]; /* yft[0]= imaginary part of admittance, yft[1]=real part of admittance*/
} Branch;

typedef struct {
  PetscReal   tfaulton,tfaultoff; /* Fault on and off times */
  PetscReal   t;
  PetscReal   t0,tmax;            /* initial time and final time */
  PetscInt    faultbus;           /* Fault bus */
  PetscScalar Rfault;             /* Fault resistance (pu) */
  PetscScalar *ybusfault;
  PetscBool   alg_flg;
} Userctx;

/* Used to read data into the DMNetwork components */
PetscErrorCode read_data(PetscInt nc, Gen **pgen,Load **pload,Bus **pbus, Branch **pbranch, PetscInt **pedgelist)
{
  PetscErrorCode    ierr;
  PetscInt          i,j,row[1],col[2];
  PetscInt          *edgelist;
  PetscInt          nofgen[9] = {1,1,1,0,0,0,0,0,0}; /* Buses at which generators are incident */
  PetscInt          nofload[9] = {0,0,0,0,1,1,0,1,0}; /* Buses at which loads are incident */
  PetscScalar       *varr,M[3],D[3];
  Bus               *bus;
  Branch            *branch;
  Gen               *gen;
  Load              *load;
  Mat               Ybus;
  Vec               V0;

  /*10 parameters*/
  /* Generator real and reactive powers (found via loadflow) */
  static const PetscScalar PG[3] = {0.716786142395021,1.630000000000000,0.850000000000000};
  static const PetscScalar QG[3] = {0.270702180178785,0.066120127797275,-0.108402221791588};

  /* Generator constants */
  static const PetscScalar H[3]    = {23.64,6.4,3.01};   /* Inertia constant */
  static const PetscScalar Rs[3]   = {0.0,0.0,0.0}; /* Stator Resistance */
  static const PetscScalar Xd[3]   = {0.146,0.8958,1.3125};  /* d-axis reactance */
  static const PetscScalar Xdp[3]  = {0.0608,0.1198,0.1813}; /* d-axis transient reactance */
  static const PetscScalar Xq[3]   = {0.4360,0.8645,1.2578}; /* q-axis reactance Xq(1) set to 0.4360, value given in text 0.0969 */
  static const PetscScalar Xqp[3]  = {0.0969,0.1969,0.25}; /* q-axis transient reactance */
  static const PetscScalar Td0p[3] = {8.96,6.0,5.89}; /* d-axis open circuit time constant */
  static const PetscScalar Tq0p[3] = {0.31,0.535,0.6}; /* q-axis open circuit time constant */

  /* Exciter system constants (8 parameters)*/
  static const PetscScalar KA[3] = {20.0,20.0,20.0};  /* Voltage regulartor gain constant */
  static const PetscScalar TA[3] = {0.2,0.2,0.2};     /* Voltage regulator time constant */
  static const PetscScalar KE[3] = {1.0,1.0,1.0};     /* Exciter gain constant */
  static const PetscScalar TE[3] = {0.314,0.314,0.314}; /* Exciter time constant */
  static const PetscScalar KF[3] = {0.063,0.063,0.063};  /* Feedback stabilizer gain constant */
  static const PetscScalar TF[3] = {0.35,0.35,0.35};    /* Feedback stabilizer time constant */
  static const PetscScalar k1[3] = {0.0039,0.0039,0.0039};
  static const PetscScalar k2[3] = {1.555,1.555,1.555};  /* k1 and k2 for calculating the saturation function SE = k1*exp(k2*Efd) */

  /* Load constants */
   static const PetscScalar       PD0[3]       = {1.25,0.9,1.0};
   static const PetscScalar       QD0[3]       = {0.5,0.3,0.35};
   static const PetscScalar       ld_alphaq[3] = {1,0,0};
   static const PetscScalar       ld_betaq[3]  = {2,1,0};
   static const PetscScalar       ld_betap[3]  = {2,1,0};
   static const PetscScalar       ld_alphap[3] = {1,0,0};
   PetscInt                       ld_nsegsp[3] = {3,3,3};
   PetscInt                       ld_nsegsq[3] = {3,3,3};
   PetscViewer                    Xview,Ybusview;
   PetscInt                       neqs_net,m,n;

   PetscFunctionBeginUser;
   /* Read V0 and Ybus from files */
   ierr = PetscViewerBinaryOpen(PETSC_COMM_SELF,"X.bin",FILE_MODE_READ,&Xview);CHKERRQ(ierr);
   ierr = PetscViewerBinaryOpen(PETSC_COMM_SELF,"Ybus.bin",FILE_MODE_READ,&Ybusview);CHKERRQ(ierr);
   ierr = VecCreate(PETSC_COMM_SELF,&V0);CHKERRQ(ierr);
   ierr = VecLoad(V0,Xview);CHKERRQ(ierr);

   ierr = MatCreate(PETSC_COMM_SELF,&Ybus);CHKERRQ(ierr);
   ierr = MatSetType(Ybus,MATBAIJ);CHKERRQ(ierr);
   ierr = MatLoad(Ybus,Ybusview);CHKERRQ(ierr);

   /* Destroy unnecessary stuff */
   ierr = PetscViewerDestroy(&Xview);CHKERRQ(ierr);
   ierr = PetscViewerDestroy(&Ybusview);CHKERRQ(ierr);

   ierr = MatGetLocalSize(Ybus,&m,&n);CHKERRQ(ierr);
   neqs_net = 2*NBUS; /* # eqs. for network subsystem   */
   if (m != neqs_net || n != neqs_net) SETERRQ(PETSC_COMM_SELF,0,"matrix Ybus is in wrong sizes");

   M[0] = 2*H[0]/W_S;
   M[1] = 2*H[1]/W_S;
   M[2] = 2*H[2]/W_S;
   D[0] = 0.1*M[0];
   D[1] = 0.1*M[1];
   D[2] = 0.1*M[2];

   /* Alocate memory for total number of buses, generators, loads and branches */
   ierr = PetscCalloc4(NBUS*nc,&bus,NGEN*nc,&gen,NLOAD*nc,&load,NBRANCH*nc+(nc-1),&branch);CHKERRQ(ierr);

   ierr = VecGetArray(V0,&varr);CHKERRQ(ierr);

   /* read bus data */
   for (i = 0; i < nc; i++) {
     for (j = 0; j < NBUS; j++) {
       bus[i*9+j].id      = i*9+j;
       bus[i*9+j].nofgen  = nofgen[j];
       bus[i*9+j].nofload = nofload[j];
       bus[i*9+j].vr      = varr[2*j];
       bus[i*9+j].vi      = varr[2*j+1];
       row[0]             = 2*j;
       col[0]             = 2*j;
       col[1]             = 2*j+1;
       /* real and imaginary part of admittance from Ybus into yff */
       ierr = MatGetValues(Ybus,1,row,2,col,bus[i*9+j].yff);CHKERRQ(ierr);
     }
   }

   /* read generator data */
   for (i = 0; i<nc; i++){
     for (j = 0; j < NGEN; j++) {
       gen[i*3+j].id   = i*3+j;
       gen[i*3+j].PG   = PG[j];
       gen[i*3+j].QG   = QG[j];
       gen[i*3+j].H    = H[j];
       gen[i*3+j].Rs   = Rs[j];
       gen[i*3+j].Xd   = Xd[j];
       gen[i*3+j].Xdp  = Xdp[j];
       gen[i*3+j].Xq   = Xq[j];
       gen[i*3+j].Xqp  = Xqp[j];
       gen[i*3+j].Td0p = Td0p[j];
       gen[i*3+j].Tq0p = Tq0p[j];
       gen[i*3+j].M    = M[j];
       gen[i*3+j].D    = D[j];

       /* exciter system */
       gen[i*3+j].KA = KA[j];
       gen[i*3+j].TA = TA[j];
       gen[i*3+j].KE = KE[j];
       gen[i*3+j].TE = TE[j];
       gen[i*3+j].KF = KF[j];
       gen[i*3+j].TF = TF[j];
       gen[i*3+j].k1 = k1[j];
       gen[i*3+j].k2 = k2[j];
     }
   }

   /* read load data */
   for (i = 0; i<nc; i++){
     for (j = 0; j < NLOAD; j++) {
       load[i*3+j].id        = i*3+j;
       load[i*3+j].PD0       = PD0[j];
       load[i*3+j].QD0       = QD0[j];
       load[i*3+j].ld_nsegsp = ld_nsegsp[j];

       load[i*3+j].ld_alphap[0] = ld_alphap[0];
       load[i*3+j].ld_alphap[1] = ld_alphap[1];
       load[i*3+j].ld_alphap[2] = ld_alphap[2];

       load[i*3+j].ld_alphaq[0] = ld_alphaq[0];
       load[i*3+j].ld_alphaq[1] = ld_alphaq[1];
       load[i*3+j].ld_alphaq[2] = ld_alphaq[2];

       load[i*3+j].ld_betap[0] = ld_betap[0];
       load[i*3+j].ld_betap[1] = ld_betap[1];
       load[i*3+j].ld_betap[2] = ld_betap[2];
       load[i*3+j].ld_nsegsq   = ld_nsegsq[j];

       load[i*3+j].ld_betaq[0] = ld_betaq[0];
       load[i*3+j].ld_betaq[1] = ld_betaq[1];
       load[i*3+j].ld_betaq[2] = ld_betaq[2];
     }
   }
   ierr = PetscCalloc1(2*NBRANCH*nc+2*(nc-1),&edgelist);CHKERRQ(ierr);

   /* read edgelist */
   for (i = 0; i<nc; i++){
     for (j = 0; j < NBRANCH; j++) {
       switch (j) {
       case 0:
         edgelist[i*18+2*j]    = 0+9*i;
         edgelist[i*18+2*j+1]  = 3+9*i;
         break;
       case 1:
         edgelist[i*18+2*j]    = 1+9*i;
         edgelist[i*18+2*j+1]  = 6+9*i;
         break;
       case 2:
         edgelist[i*18+2*j]    = 2+9*i;
         edgelist[i*18+2*j+1]  = 8+9*i;
         break;
       case 3:
         edgelist[i*18+2*j]    = 3+9*i;
         edgelist[i*18+2*j+1]  = 4+9*i;
         break;
       case 4:
         edgelist[i*18+2*j]    = 3+9*i;
         edgelist[i*18+2*j+1]  = 5+9*i;
         break;
       case 5:
         edgelist[i*18+2*j]    = 4+9*i;
         edgelist[i*18+2*j+1]  = 6+9*i;
         break;
       case 6:
         edgelist[i*18+2*j]    = 5+9*i;
         edgelist[i*18+2*j+1]  = 8+9*i;
         break;
       case 7:
         edgelist[i*18+2*j]     = 6+9*i;
         edgelist[i*18+2*j+1]   = 7+9*i;
         break;
       case 8:
         edgelist[i*18+2*j]     = 7+9*i;
         edgelist[i*18+2*j+1]   = 8+9*i;
         break;
       default:
         break;
       }
     }
   }

   /* for connecting last bus of previous network(9*i-1) to first bus of next network(9*i), the branch admittance=-0.0301407+j17.3611 */
    for (i = 1; i<nc; i++){
        edgelist[18*nc+2*(i-1)]   = 8+(i-1)*9;
        edgelist[18*nc+2*(i-1)+1] = 9*i;

        /* adding admittances to the off-diagonal elements */
        branch[9*nc+(i-1)].id     = 9*nc+(i-1);
        branch[9*nc+(i-1)].yft[0] = 17.3611;
        branch[9*nc+(i-1)].yft[1] = -0.0301407;

        /* subtracting admittances from the diagonal elements */
        bus[9*i-1].yff[0] -= 17.3611;
        bus[9*i-1].yff[1] -= -0.0301407;

        bus[9*i].yff[0]   -= 17.3611;
        bus[9*i].yff[1]   -= -0.0301407;
    }

    /* read branch data */
    for (i = 0; i<nc; i++){
      for (j = 0; j < NBRANCH; j++) {
        branch[i*9+j].id  = i*9+j;

        row[0] = edgelist[2*j]*2;
        col[0] = edgelist[2*j+1]*2;
        col[1] = edgelist[2*j+1]*2+1;
        ierr = MatGetValues(Ybus,1,row,2,col,branch[i*9+j].yft);CHKERRQ(ierr);/*imaginary part of admittance*/
      }
    }

   *pgen      = gen;
   *pload     = load;
   *pbus      = bus;
   *pbranch   = branch;
   *pedgelist = edgelist;

   ierr = VecRestoreArray(V0,&varr);CHKERRQ(ierr);

   /* Destroy unnecessary stuff */
   ierr = MatDestroy(&Ybus);CHKERRQ(ierr);
   ierr = VecDestroy(&V0);CHKERRQ(ierr);
   PetscFunctionReturn(0);
}

PetscErrorCode SetInitialGuess(DM networkdm, Vec X)
{
  PetscErrorCode ierr;
  Bus            *bus;
  Gen            *gen;
  PetscInt       v,vStart,vEnd,offset;
  PetscInt       key,numComps,j;
  PetscInt       idx=0;
  PetscBool      ghostvtex;
  Vec            localX;
  PetscScalar    *xarr;
  PetscScalar    Vr=0,Vi=0,Vm,Vm2;  /* Terminal voltage variables */
  PetscScalar    IGr, IGi;          /* Generator real and imaginary current */
  PetscScalar    Eqp,Edp,delta;     /* Generator variables */
  PetscScalar    Efd,RF,VR;         /* Exciter variables */
  PetscScalar    Vd,Vq;             /* Generator dq axis voltages */
  PetscScalar    Id,Iq;             /* Generator dq axis currents */
  PetscScalar    theta;             /* Generator phase angle */
  PetscScalar    SE;
  void*          component;

  PetscFunctionBegin;
  ierr = DMNetworkGetVertexRange(networkdm,&vStart,&vEnd);CHKERRQ(ierr);
  ierr = DMGetLocalVector(networkdm,&localX);CHKERRQ(ierr);

  ierr = VecSet(X,0.0);CHKERRQ(ierr);
  ierr = DMGlobalToLocalBegin(networkdm,X,INSERT_VALUES,localX);CHKERRQ(ierr);
  ierr = DMGlobalToLocalEnd(networkdm,X,INSERT_VALUES,localX);CHKERRQ(ierr);

  ierr = VecGetArray(localX,&xarr);CHKERRQ(ierr);

  for (v = vStart; v < vEnd; v++) {
    ierr = DMNetworkIsGhostVertex(networkdm,v,&ghostvtex);CHKERRQ(ierr);
    if (ghostvtex) continue;

    ierr = DMNetworkGetVariableOffset(networkdm,v,&offset);CHKERRQ(ierr);
    ierr = DMNetworkGetNumComponents(networkdm,v,&numComps);CHKERRQ(ierr);
    for (j=0; j < numComps; j++) {
      ierr = DMNetworkGetComponent(networkdm,v,j,&key,&component);CHKERRQ(ierr);
      if (key == 1) {
        bus = (Bus*)(component);

        xarr[offset]   = bus->vr;
        xarr[offset+1] = bus->vi;

        Vr = bus->vr;
        Vi = bus->vi;
      } else if(key == 2) {
        gen = (Gen*)(component);

        Vm  = PetscSqrtScalar(Vr*Vr + Vi*Vi);
        Vm2 = Vm*Vm;
        /* Real part of gen current */
        IGr = (Vr*gen->PG + Vi*gen->QG)/Vm2;
        /* Imaginary part of gen current */
        IGi = (Vi*gen->PG - Vr*gen->QG)/Vm2;

        /* Machine angle */
        delta = atan2(Vi+gen->Xq*IGr,Vr-gen->Xq*IGi);
        theta = PETSC_PI/2.0 - delta;

        /* d-axis stator current */
        Id = IGr*PetscCosScalar(theta) - IGi*PetscSinScalar(theta);

        /* q-axis stator current */
        Iq = IGr*PetscSinScalar(theta) + IGi*PetscCosScalar(theta);

        Vd = Vr*PetscCosScalar(theta) - Vi*PetscSinScalar(theta);
        Vq = Vr*PetscSinScalar(theta) + Vi*PetscCosScalar(theta);

        /* d-axis transient EMF */
        Edp = Vd + gen->Rs*Id - gen->Xqp*Iq;

        /* q-axis transient EMF */
        Eqp = Vq + gen->Rs*Iq + gen->Xdp*Id;

        gen->TM = gen->PG;
        idx     = offset+2;

        xarr[idx]   = Eqp;
        xarr[idx+1] = Edp;
        xarr[idx+2] = delta;
        xarr[idx+3] = W_S;
        xarr[idx+4] = Id;
        xarr[idx+5] = Iq;

        /* Exciter */
        Efd = Eqp + (gen->Xd - gen->Xdp)*Id;
        SE  = gen->k1*PetscExpScalar(gen->k2*Efd);
        VR  = gen->KE*Efd + SE;
        RF  = gen->KF*Efd/gen->TF;

        xarr[idx+6] = Efd;
        xarr[idx+7] = RF;
        xarr[idx+8] = VR;

        gen->Vref = Vm + (VR/gen->KA);
      }
    }
  }
  ierr = VecRestoreArray(localX,&xarr);CHKERRQ(ierr);
  ierr = DMLocalToGlobalBegin(networkdm,localX,ADD_VALUES,X);CHKERRQ(ierr);
  ierr = DMLocalToGlobalEnd(networkdm,localX,ADD_VALUES,X);CHKERRQ(ierr);
  ierr = DMRestoreLocalVector(networkdm,&localX);CHKERRQ(ierr);
  PetscFunctionReturn(0);
 }

 /* Converts from machine frame (dq) to network (phase a real,imag) reference frame */
PetscErrorCode dq2ri(PetscScalar Fd,PetscScalar Fq,PetscScalar delta,PetscScalar *Fr,PetscScalar *Fi)
{
  PetscFunctionBegin;
  *Fr =  Fd*PetscSinScalar(delta) + Fq*PetscCosScalar(delta);
  *Fi = -Fd*PetscCosScalar(delta) + Fq*PetscSinScalar(delta);
  PetscFunctionReturn(0);
}

/* Converts from network frame ([phase a real,imag) to machine (dq) reference frame */
PetscErrorCode ri2dq(PetscScalar Fr,PetscScalar Fi,PetscScalar delta,PetscScalar *Fd,PetscScalar *Fq)
{
  PetscFunctionBegin;
  *Fd =  Fr*PetscSinScalar(delta) - Fi*PetscCosScalar(delta);
  *Fq =  Fr*PetscCosScalar(delta) + Fi*PetscSinScalar(delta);
  PetscFunctionReturn(0);
}

/* Computes F(t,U,U_t) where F() = 0 is the DAE to be solved. */
PetscErrorCode FormIFunction(TS ts,PetscReal t,Vec X,Vec Xdot,Vec F,Userctx *user)
{
  PetscErrorCode                     ierr;
  DM                                 networkdm;
  Vec                                localX,localXdot,localF;
  PetscInt                           vfrom,vto,offsetfrom,offsetto;
  PetscInt                           v,vStart,vEnd,e;
  PetscScalar                        *farr;
  PetscScalar                        Vd,Vq,SE;
  const PetscScalar                  *xarr,*xdotarr;
  void*                              component;

  PetscFunctionBegin;
  ierr = VecSet(F,0.0);CHKERRQ(ierr);

  ierr = TSGetDM(ts,&networkdm);CHKERRQ(ierr);
  ierr = DMGetLocalVector(networkdm,&localF);CHKERRQ(ierr);
  ierr = DMGetLocalVector(networkdm,&localX);CHKERRQ(ierr);
  ierr = DMGetLocalVector(networkdm,&localXdot);CHKERRQ(ierr);
  ierr = VecSet(localF,0.0);CHKERRQ(ierr);

  /* update ghost values of localX and localXdot */
  ierr = DMGlobalToLocalBegin(networkdm,X,INSERT_VALUES,localX);CHKERRQ(ierr);
  ierr = DMGlobalToLocalEnd(networkdm,X,INSERT_VALUES,localX);CHKERRQ(ierr);

  ierr = DMGlobalToLocalBegin(networkdm,Xdot,INSERT_VALUES,localXdot);CHKERRQ(ierr);
  ierr = DMGlobalToLocalEnd(networkdm,Xdot,INSERT_VALUES,localXdot);CHKERRQ(ierr);

  ierr = VecGetArrayRead(localX,&xarr);CHKERRQ(ierr);
  ierr = VecGetArrayRead(localXdot,&xdotarr);CHKERRQ(ierr);
  ierr = VecGetArray(localF,&farr);CHKERRQ(ierr);

  ierr = DMNetworkGetVertexRange(networkdm,&vStart,&vEnd);CHKERRQ(ierr);

  for (v=vStart; v < vEnd; v++) {
    PetscInt     i,j,offset,key;
    PetscScalar  Vr, Vi;
    Bus          *bus;
    Gen          *gen;
    Load         *load;
    PetscBool    ghostvtex;
    PetscInt     numComps;
    PetscScalar  Yffr,Yffi; /* Real and imaginary fault admittances */
    PetscScalar  Vm,Vm2,Vm0;
    PetscScalar  Vr0=0,Vi0=0;
    PetscScalar  PD,QD;

    ierr = DMNetworkIsGhostVertex(networkdm,v,&ghostvtex);CHKERRQ(ierr);
    ierr = DMNetworkGetNumComponents(networkdm,v,&numComps);CHKERRQ(ierr);
    ierr = DMNetworkGetVariableOffset(networkdm,v,&offset);CHKERRQ(ierr);

    for (j = 0; j < numComps; j++) {
      ierr = DMNetworkGetComponent(networkdm,v,j,&key,&component);CHKERRQ(ierr);
      if (key == 1) {
        PetscInt       nconnedges;
        const PetscInt *connedges;

        bus = (Bus*)(component);
        if (!ghostvtex) {
          Vr   = xarr[offset];
          Vi   = xarr[offset+1];

          Yffr = bus->yff[1];
          Yffi = bus->yff[0];

          if (user->alg_flg){
            Yffr += user->ybusfault[bus->id*2+1];
            Yffi += user->ybusfault[bus->id*2];
          }
          Vr0 = bus->vr;
          Vi0 = bus->vi;

          /* Network current balance residual IG + Y*V + IL = 0. Only YV is added here.
           The generator current injection, IG, and load current injection, ID are added later
           */
          farr[offset] +=  Yffi*Vr + Yffr*Vi; /* imaginary current due to diagonal elements */
          farr[offset+1] += Yffr*Vr - Yffi*Vi; /* real current due to diagonal elements */
        }

        ierr = DMNetworkGetSupportingEdges(networkdm,v,&nconnedges,&connedges);CHKERRQ(ierr);

        for (i=0; i < nconnedges; i++) {
          Branch         *branch;
          PetscInt       keye;
          PetscScalar    Yfti, Yftr, Vfr, Vfi, Vtr, Vti;
          const PetscInt *cone;

          e = connedges[i];
          ierr = DMNetworkGetComponent(networkdm,e,0,&keye,(void**)&branch);CHKERRQ(ierr);

          Yfti = branch->yft[0];
          Yftr = branch->yft[1];

          ierr = DMNetworkGetConnectedVertices(networkdm,e,&cone);CHKERRQ(ierr);

          vfrom = cone[0];
          vto   = cone[1];

          ierr = DMNetworkGetVariableOffset(networkdm,vfrom,&offsetfrom);CHKERRQ(ierr);
          ierr = DMNetworkGetVariableOffset(networkdm,vto,&offsetto);CHKERRQ(ierr);

          /* From bus and to bus real and imaginary voltages */
          Vfr     = xarr[offsetfrom];
          Vfi     = xarr[offsetfrom+1];
          Vtr	    = xarr[offsetto];
          Vti     = xarr[offsetto+1];

          if (vfrom == v) {
            farr[offsetfrom]   += Yftr*Vti + Yfti*Vtr;
            farr[offsetfrom+1] += Yftr*Vtr - Yfti*Vti;
          } else {
            farr[offsetto]   += Yftr*Vfi + Yfti*Vfr;
            farr[offsetto+1] += Yftr*Vfr - Yfti*Vfi;
          }
        }
      } else if (key == 2){
        if (!ghostvtex) {
          PetscScalar    Eqp,Edp,delta,w; /* Generator variables */
          PetscScalar    Efd,RF,VR; /* Exciter variables */
          PetscScalar    Id,Iq;  /* Generator dq axis currents */
          PetscScalar    IGr,IGi,Zdq_inv[4],det;
          PetscScalar    Xd,Xdp,Td0p,Xq,Xqp,Tq0p,TM,D,M,Rs; /* Generator parameters */
          PetscScalar    k1,k2,KE,TE,TF,KA,KF,Vref,TA; /* Generator parameters */
          PetscInt       idx;

          gen = (Gen*)(component);
          idx = offset + 2;

          /* Generator state variables */
          Eqp   = xarr[idx];
          Edp   = xarr[idx+1];
          delta = xarr[idx+2];
          w     = xarr[idx+3];
          Id    = xarr[idx+4];
          Iq    = xarr[idx+5];
          Efd   = xarr[idx+6];
          RF    = xarr[idx+7];
          VR    = xarr[idx+8];

          /* Generator parameters */
          Xd   = gen->Xd;
          Xdp  = gen->Xdp;
          Td0p = gen->Td0p;
          Xq   = gen->Xq;
          Xqp  = gen->Xqp;
          Tq0p = gen->Tq0p;
          TM   = gen->TM;
          D    = gen->D;
          M    = gen->M;
          Rs   = gen->Rs;
          k1   = gen->k1;
          k2   = gen->k2;
          KE   = gen->KE;
          TE   = gen->TE;
          TF   = gen->TF;
          KA   = gen->KA;
          KF   = gen->KF;
          Vref = gen->Vref;
          TA   = gen->TA;

          /* Generator differential equations */
          farr[idx]   = (Eqp + (Xd - Xdp)*Id - Efd)/Td0p + xdotarr[idx];
          farr[idx+1] = (Edp - (Xq - Xqp)*Iq)/Tq0p  + xdotarr[idx+1];
          farr[idx+2] = -w + W_S + xdotarr[idx+2];
          farr[idx+3] = (-TM + Edp*Id + Eqp*Iq + (Xqp - Xdp)*Id*Iq + D*(w - W_S))/M  + xdotarr[idx+3];

          Vr = xarr[offset]; /* Real part of generator terminal voltage */
          Vi = xarr[offset+1]; /* Imaginary part of the generator terminal voltage */

          ierr = ri2dq(Vr,Vi,delta,&Vd,&Vq);CHKERRQ(ierr);

          /* Algebraic equations for stator currents */
          det = Rs*Rs + Xdp*Xqp;

          Zdq_inv[0] = Rs/det;
          Zdq_inv[1] = Xqp/det;
          Zdq_inv[2] = -Xdp/det;
          Zdq_inv[3] = Rs/det;

          farr[idx+4] = Zdq_inv[0]*(-Edp + Vd) + Zdq_inv[1]*(-Eqp + Vq) + Id;
          farr[idx+5] = Zdq_inv[2]*(-Edp + Vd) + Zdq_inv[3]*(-Eqp + Vq) + Iq;

          /* Add generator current injection to network */
          ierr = dq2ri(Id,Iq,delta,&IGr,&IGi);CHKERRQ(ierr);

          farr[offset]   -= IGi;
          farr[offset+1] -= IGr;

          Vm = PetscSqrtScalar(Vd*Vd + Vq*Vq);
          SE = k1*PetscExpScalar(k2*Efd);

          /* Exciter differential equations */
          farr[idx+6] = (KE*Efd + SE - VR)/TE + xdotarr[idx+6];
          farr[idx+7] = (RF - KF*Efd/TF)/TF + xdotarr[idx+7];
          farr[idx+8] = (VR - KA*RF + KA*KF*Efd/TF - KA*(Vref - Vm))/TA + xdotarr[idx+8];
        }
      } else if (key ==3){
        if (!ghostvtex) {
          PetscInt    k;
          PetscInt    ld_nsegsp;
          PetscInt    ld_nsegsq;
          PetscScalar *ld_alphap;
          PetscScalar *ld_betap,*ld_alphaq,*ld_betaq,PD0, QD0, IDr,IDi;

          load = (Load*)(component);

          /* Load Parameters */
          ld_nsegsp = load->ld_nsegsp;
          ld_alphap = load->ld_alphap;
          ld_betap  = load->ld_betap;
          ld_nsegsq = load->ld_nsegsq;
          ld_alphaq = load->ld_alphaq;
          ld_betaq  = load->ld_betaq;
          PD0       = load->PD0;
          QD0       = load->QD0;

          Vr  = xarr[offset]; /* Real part of generator terminal voltage */
          Vi  = xarr[offset+1]; /* Imaginary part of the generator terminal voltage */
          Vm  = PetscSqrtScalar(Vr*Vr + Vi*Vi); 
          Vm2 = Vm*Vm;
          Vm0 = PetscSqrtScalar(Vr0*Vr0 + Vi0*Vi0);
          PD  = QD = 0.0;
          for (k=0; k < ld_nsegsp; k++) PD += ld_alphap[k]*PD0*PetscPowScalar((Vm/Vm0),ld_betap[k]);
          for (k=0; k < ld_nsegsq; k++) QD += ld_alphaq[k]*QD0*PetscPowScalar((Vm/Vm0),ld_betaq[k]);

          /* Load currents */
          IDr = (PD*Vr + QD*Vi)/Vm2;
          IDi = (-QD*Vr + PD*Vi)/Vm2;

          farr[offset]   += IDi;
          farr[offset+1] += IDr;
        }
      }
    }
  }

  ierr = VecRestoreArrayRead(localX,&xarr);CHKERRQ(ierr);
  ierr = VecRestoreArrayRead(localXdot,&xdotarr);CHKERRQ(ierr);
  ierr = VecRestoreArray(localF,&farr);CHKERRQ(ierr);
  ierr = DMRestoreLocalVector(networkdm,&localX);CHKERRQ(ierr);
  ierr = DMRestoreLocalVector(networkdm,&localXdot);CHKERRQ(ierr);

  ierr = DMLocalToGlobalBegin(networkdm,localF,ADD_VALUES,F);CHKERRQ(ierr);
  ierr = DMLocalToGlobalEnd(networkdm,localF,ADD_VALUES,F);CHKERRQ(ierr);
  ierr = DMRestoreLocalVector(networkdm,&localF);CHKERRQ(ierr);
  PetscFunctionReturn(0);
}

/* This function is used for solving the algebraic system only during fault on and
   off times. It computes the entire F and then zeros out the part corresponding to
   differential equations
 F = [0;g(y)];
*/
PetscErrorCode AlgFunction (SNES snes, Vec X, Vec F, void *ctx)
{
  PetscErrorCode ierr;
  DM             networkdm;
  Vec            localX,localF;
  PetscInt       vfrom,vto,offsetfrom,offsetto;
  PetscInt       v,vStart,vEnd,e;
  PetscScalar    *farr;
  Userctx        *user=(Userctx*)ctx;
  const PetscScalar *xarr;
  void*          component;

  PetscFunctionBegin;
  ierr = VecSet(F,0.0);CHKERRQ(ierr);
  ierr = SNESGetDM(snes,&networkdm);CHKERRQ(ierr);
  ierr = DMGetLocalVector(networkdm,&localF);CHKERRQ(ierr);
  ierr = DMGetLocalVector(networkdm,&localX);CHKERRQ(ierr);
  ierr = VecSet(localF,0.0);CHKERRQ(ierr);

  /* update ghost values of locaX and locaXdot */
  ierr = DMGlobalToLocalBegin(networkdm,X,INSERT_VALUES,localX);CHKERRQ(ierr);
  ierr = DMGlobalToLocalEnd(networkdm,X,INSERT_VALUES,localX);CHKERRQ(ierr);

  ierr = VecGetArrayRead(localX,&xarr);CHKERRQ(ierr);
  ierr = VecGetArray(localF,&farr);CHKERRQ(ierr);

  ierr = DMNetworkGetVertexRange(networkdm,&vStart,&vEnd);CHKERRQ(ierr);

  for (v=vStart; v < vEnd; v++) {
    PetscInt      i,j,offset,key,numComps;
    PetscScalar   Vr, Vi, Yffr, Yffi, Vm, Vm2, Vm0, Vr0=0, Vi0=0, PD, QD;
    Bus           *bus;
    Gen           *gen;
    Load          *load;
    PetscBool     ghostvtex;

    ierr = DMNetworkIsGhostVertex(networkdm,v,&ghostvtex);CHKERRQ(ierr);
    ierr = DMNetworkGetNumComponents(networkdm,v,&numComps);CHKERRQ(ierr);
    ierr = DMNetworkGetVariableOffset(networkdm,v,&offset);CHKERRQ(ierr);

    for (j = 0; j < numComps; j++) {
      ierr = DMNetworkGetComponent(networkdm,v,j,&key,&component);CHKERRQ(ierr);
      if (key == 1) {
        PetscInt       nconnedges;
        const PetscInt *connedges;

        bus = (Bus*)(component);
        if (!ghostvtex) {
          Vr = xarr[offset];
          Vi = xarr[offset+1];

          Yffr = bus->yff[1];
          Yffi = bus->yff[0];
          if (user->alg_flg){
            Yffr += user->ybusfault[bus->id*2+1];
            Yffi += user->ybusfault[bus->id*2];
          }
          Vr0 = bus->vr;
          Vi0 = bus->vi;

          farr[offset]   += Yffi*Vr + Yffr*Vi;
          farr[offset+1] += Yffr*Vr - Yffi*Vi;
        }
        ierr = DMNetworkGetSupportingEdges(networkdm,v,&nconnedges,&connedges);CHKERRQ(ierr);

        for (i=0; i < nconnedges; i++) {
          Branch         *branch;
          PetscInt       keye;
          PetscScalar    Yfti, Yftr, Vfr, Vfi, Vtr, Vti;
          const PetscInt *cone;

          e = connedges[i];
          ierr   = DMNetworkGetComponent(networkdm,e,0,&keye,(void**)&branch);CHKERRQ(ierr);

          Yfti = branch->yft[0];
          Yftr = branch->yft[1];

          ierr  = DMNetworkGetConnectedVertices(networkdm,e,&cone);CHKERRQ(ierr);
          vfrom = cone[0];
          vto   = cone[1];

          ierr = DMNetworkGetVariableOffset(networkdm,vfrom,&offsetfrom);CHKERRQ(ierr);
          ierr = DMNetworkGetVariableOffset(networkdm,vto,&offsetto);CHKERRQ(ierr);

          /*From bus and to bus real and imaginary voltages */
          Vfr = xarr[offsetfrom];
          Vfi = xarr[offsetfrom+1];
          Vtr = xarr[offsetto];
          Vti = xarr[offsetto+1];

          if (vfrom == v) {
            farr[offsetfrom]   += Yftr*Vti + Yfti*Vtr;
            farr[offsetfrom+1] += Yftr*Vtr - Yfti*Vti;
          } else {
            farr[offsetto]   += Yftr*Vfi + Yfti*Vfr;
            farr[offsetto+1] += Yftr*Vfr - Yfti*Vfi;
          }
        }
      } else if (key == 2){
        if (!ghostvtex) {
          PetscScalar    Eqp,Edp,delta;   /* Generator variables */
          PetscScalar    Id,Iq;           /* Generator dq axis currents */
          PetscScalar    Vd,Vq,IGr,IGi,Zdq_inv[4],det;
          PetscInt       idx;
          PetscScalar    Xdp,Xqp,Rs;      /* Generator parameters */

          gen = (Gen*)(component);
          idx = offset + 2;

          /* Generator state variables */
          Eqp   = xarr[idx];
          Edp   = xarr[idx+1];
          delta = xarr[idx+2];
          /* w     = xarr[idx+3]; not being used */
          Id    = xarr[idx+4];
          Iq    = xarr[idx+5];

          /* Generator parameters */
          Xdp  = gen->Xdp;
          Xqp  = gen->Xqp;
          Rs   = gen->Rs;

          /* Set generator differential equation residual functions to zero */
          farr[idx]   = 0;
          farr[idx+1] = 0;
          farr[idx+2] = 0;
          farr[idx+3] = 0;

          Vr = xarr[offset]; /* Real part of generator terminal voltage */
          Vi = xarr[offset+1]; /* Imaginary part of the generator terminal voltage */

          ierr = ri2dq(Vr,Vi,delta,&Vd,&Vq);CHKERRQ(ierr);

          /* Algebraic equations for stator currents */
          det = Rs*Rs + Xdp*Xqp;

          Zdq_inv[0] = Rs/det;
          Zdq_inv[1] = Xqp/det;
          Zdq_inv[2] = -Xdp/det;
          Zdq_inv[3] = Rs/det;

          farr[idx+4] = Zdq_inv[0]*(-Edp + Vd) + Zdq_inv[1]*(-Eqp + Vq) + Id;
          farr[idx+5] = Zdq_inv[2]*(-Edp + Vd) + Zdq_inv[3]*(-Eqp + Vq) + Iq;

          /* Add generator current injection to network */
          ierr = dq2ri(Id,Iq,delta,&IGr,&IGi);CHKERRQ(ierr);

          farr[offset]   -= IGi;
          farr[offset+1] -= IGr;

          /* Vm = PetscSqrtScalar(Vd*Vd + Vq*Vq);*/ /* a compiler warning: "Value stored to 'Vm' is never read" - comment out by Hong Zhang */

          /* Set exciter differential equation residual functions equal to zero*/
          farr[idx+6] = 0;
          farr[idx+7] = 0;
          farr[idx+8] = 0;
        }
      } else if (key ==3){
        if (!ghostvtex) {
          PetscInt    k,ld_nsegsp,ld_nsegsq;
          PetscScalar *ld_alphap,*ld_betap,*ld_alphaq,*ld_betaq,PD0,QD0,IDr,IDi;

          load = (Load*)(component);

          /* Load Parameters */
          ld_nsegsp = load->ld_nsegsp;
          ld_alphap = load->ld_alphap;
          ld_betap  = load->ld_betap;
          ld_nsegsq = load->ld_nsegsq;
          ld_alphaq = load->ld_alphaq;
          ld_betaq  = load->ld_betaq;

          PD0 = load->PD0;
          QD0 = load->QD0;

          Vr  = xarr[offset]; /* Real part of generator terminal voltage */
          Vi  = xarr[offset+1]; /* Imaginary part of the generator terminal voltage */
          Vm  = PetscSqrtScalar(Vr*Vr + Vi*Vi);
          Vm2 = Vm*Vm;
          Vm0 = PetscSqrtScalar(Vr0*Vr0 + Vi0*Vi0);
          PD  = QD = 0.0;
          for (k=0; k < ld_nsegsp; k++) PD += ld_alphap[k]*PD0*PetscPowScalar((Vm/Vm0),ld_betap[k]);
          for (k=0; k < ld_nsegsq; k++) QD += ld_alphaq[k]*QD0*PetscPowScalar((Vm/Vm0),ld_betaq[k]);

          /* Load currents */
          IDr = (PD*Vr + QD*Vi)/Vm2;
          IDi = (-QD*Vr + PD*Vi)/Vm2;

          farr[offset]   += IDi;
          farr[offset+1] += IDr;
        }
      }
    }
  }

  ierr = VecRestoreArrayRead(localX,&xarr);CHKERRQ(ierr);
  ierr = VecRestoreArray(localF,&farr);CHKERRQ(ierr);
  ierr = DMRestoreLocalVector(networkdm,&localX);CHKERRQ(ierr);

  ierr = DMLocalToGlobalBegin(networkdm,localF,ADD_VALUES,F);CHKERRQ(ierr);
  ierr = DMLocalToGlobalEnd(networkdm,localF,ADD_VALUES,F);CHKERRQ(ierr);
  ierr = DMRestoreLocalVector(networkdm,&localF);CHKERRQ(ierr);
  PetscFunctionReturn(0);
}

int main(int argc,char ** argv)
{
  PetscErrorCode ierr;
  PetscInt       i,j,*edgelist= NULL,eStart,eEnd,vStart,vEnd;
  PetscInt       genj,loadj,componentkey[4];
  PetscInt       nc = 1;    /* No. of copies (default = 1) */
  PetscMPIInt    size,rank;
  Vec            X,F_alg;
  TS             ts;
  SNES           snes_alg,snes;
  Bus            *bus;
  Branch         *branch;
  Gen            *gen;
  Load           *load;
  DM             networkdm;
  PetscLogStage  stage1;
  Userctx        user;
  KSP            ksp;
  PC             pc;
  PetscInt       numEdges=0,numVertices=0,NumEdges=PETSC_DETERMINE,NumVertices=PETSC_DETERMINE;

  ierr = PetscInitialize(&argc,&argv,"ex9busnetworkops",help);CHKERRQ(ierr);
  ierr = PetscOptionsGetInt(NULL,NULL,"-nc",&nc,NULL);CHKERRQ(ierr);
  ierr = MPI_Comm_size(PETSC_COMM_WORLD,&size);CHKERRQ(ierr);
  ierr = MPI_Comm_rank(PETSC_COMM_WORLD,&rank);CHKERRQ(ierr);

  /* Read initial voltage vector and Ybus */
  if (!rank) {
    ierr = read_data(nc,&gen,&load,&bus,&branch,&edgelist);CHKERRQ(ierr);
  }

  ierr = DMNetworkCreate(PETSC_COMM_WORLD,&networkdm);CHKERRQ(ierr);
  ierr = DMNetworkRegisterComponent(networkdm,"branchstruct",sizeof(Branch),&componentkey[0]);CHKERRQ(ierr);
  ierr = DMNetworkRegisterComponent(networkdm,"busstruct",sizeof(Bus),&componentkey[1]);CHKERRQ(ierr);
  ierr = DMNetworkRegisterComponent(networkdm,"genstruct",sizeof(Gen),&componentkey[2]);CHKERRQ(ierr);
  ierr = DMNetworkRegisterComponent(networkdm,"loadstruct",sizeof(Load),&componentkey[3]);CHKERRQ(ierr);

  ierr = PetscLogStageRegister("Create network",&stage1);CHKERRQ(ierr);
  ierr = PetscLogStagePush(stage1);CHKERRQ(ierr);

  /* Set local number of nodes and edges */
  if (!rank){
    numVertices = NBUS*nc; numEdges = NBRANCH*nc+(nc-1);
  }
  ierr = DMNetworkSetSizes(networkdm,1,0,&numVertices,&numEdges,&NumVertices,&NumEdges);CHKERRQ(ierr);

  /* Add edge connectivity */
  ierr = DMNetworkSetEdgeList(networkdm,&edgelist,NULL);CHKERRQ(ierr);

  /* Set up the network layout */
  ierr = DMNetworkLayoutSetUp(networkdm);CHKERRQ(ierr);

  if (!rank) {
    ierr = PetscFree(edgelist);CHKERRQ(ierr);
  }

   /* Add network components: physical parameters of nodes and branches */
  if (!rank) {
     ierr = DMNetworkGetEdgeRange(networkdm,&eStart,&eEnd);CHKERRQ(ierr);
     genj=0; loadj=0;
     for (i = eStart; i < eEnd; i++) {
       ierr = DMNetworkAddComponent(networkdm,i,componentkey[0],&branch[i-eStart]);CHKERRQ(ierr);
     }

     ierr = DMNetworkGetVertexRange(networkdm,&vStart,&vEnd);CHKERRQ(ierr);

     for (i = vStart; i < vEnd; i++) {
       ierr = DMNetworkAddComponent(networkdm,i,componentkey[1],&bus[i-vStart]);CHKERRQ(ierr);
       /* Add number of variables */
       ierr = DMNetworkAddNumVariables(networkdm,i,2);CHKERRQ(ierr);
       if (bus[i-vStart].nofgen) {
         for (j = 0; j < bus[i-vStart].nofgen; j++) {
           ierr = DMNetworkAddComponent(networkdm,i,componentkey[2],&gen[genj++]);CHKERRQ(ierr);
           ierr = DMNetworkAddNumVariables(networkdm,i,9);CHKERRQ(ierr);
         }
       }
       if (bus[i-vStart].nofload) {
         for (j=0; j < bus[i-vStart].nofload; j++) {
           ierr = DMNetworkAddComponent(networkdm,i,componentkey[3],&load[loadj++]);CHKERRQ(ierr);
         }
       }
     }
  }

  ierr = DMSetUp(networkdm);CHKERRQ(ierr);

  if (!rank) {
    ierr = PetscFree4(bus,gen,load,branch);CHKERRQ(ierr);
  }

  /* for parallel options: Network partitioning and distribution of data */
  if (size > 1) {
    ierr = DMNetworkDistribute(&networkdm,0);CHKERRQ(ierr);
  }
  ierr = PetscLogStagePop();CHKERRQ(ierr);

  ierr = DMCreateGlobalVector(networkdm,&X);CHKERRQ(ierr);

  ierr = SetInitialGuess(networkdm,X);CHKERRQ(ierr);

  /* Options for fault simulation */
  ierr = PetscOptionsBegin(PETSC_COMM_WORLD,NULL,"Transient stability fault options","");CHKERRQ(ierr);
  user.tfaulton  = 0.02;
  user.tfaultoff = 0.05;
  user.Rfault    = 0.0001;
  user.faultbus  = 8;
  ierr           = PetscOptionsReal("-tfaulton","","",user.tfaulton,&user.tfaulton,NULL);CHKERRQ(ierr);
  ierr           = PetscOptionsReal("-tfaultoff","","",user.tfaultoff,&user.tfaultoff,NULL);CHKERRQ(ierr);
  ierr           = PetscOptionsInt("-faultbus","","",user.faultbus,&user.faultbus,NULL);CHKERRQ(ierr);
  user.t0        = 0.0;
  user.tmax      = 0.1;
  ierr           = PetscOptionsReal("-t0","","",user.t0,&user.t0,NULL);CHKERRQ(ierr);
  ierr           = PetscOptionsReal("-tmax","","",user.tmax,&user.tmax,NULL);CHKERRQ(ierr);

  ierr = PetscMalloc1(18*nc,&user.ybusfault);CHKERRQ(ierr);
  for (i = 0; i < 18*nc; i++) {
    user.ybusfault[i] = 0;
  }
  user.ybusfault[user.faultbus*2+1] = 1/user.Rfault;
  ierr = PetscOptionsEnd();CHKERRQ(ierr);

  /* Setup TS solver                                           */
  /*--------------------------------------------------------*/
  ierr = TSCreate(PETSC_COMM_WORLD,&ts);CHKERRQ(ierr);
  ierr = TSSetDM(ts,(DM)networkdm);CHKERRQ(ierr);
  ierr = TSSetType(ts,TSCN);CHKERRQ(ierr);

  ierr = TSGetSNES(ts,&snes);CHKERRQ(ierr);
  ierr = SNESGetKSP(snes,&ksp);CHKERRQ(ierr);
  ierr = KSPGetPC(ksp,&pc);CHKERRQ(ierr);
  ierr = PCSetType(pc,PCBJACOBI);CHKERRQ(ierr);

  ierr = TSSetIFunction(ts,NULL,(TSIFunction) FormIFunction,&user);CHKERRQ(ierr);
  ierr = TSSetMaxTime(ts,user.tfaulton);CHKERRQ(ierr);
  ierr = TSSetExactFinalTime(ts,TS_EXACTFINALTIME_STEPOVER);CHKERRQ(ierr);
  ierr = TSSetTimeStep(ts,0.01);CHKERRQ(ierr);
  ierr = TSSetFromOptions(ts);CHKERRQ(ierr);

  /*user.alg_flg = PETSC_TRUE is the period when fault exists. We add fault admittance to Ybus matrix. 
    eg, fault bus is 8. Y88(new)=Y88(old)+Yfault. */
  user.alg_flg = PETSC_FALSE;

  /* Prefault period */
  if (!rank) {
    ierr = PetscPrintf(PETSC_COMM_SELF,"... (1) Prefault period ... \n");CHKERRQ(ierr);
  }

  ierr = TSSetSolution(ts,X);CHKERRQ(ierr);
  ierr = TSSetUp(ts);CHKERRQ(ierr);
  ierr = TSSolve(ts,X);CHKERRQ(ierr);

  /* Create the nonlinear solver for solving the algebraic system */
  ierr = VecDuplicate(X,&F_alg);CHKERRQ(ierr);

  ierr = SNESCreate(PETSC_COMM_WORLD,&snes_alg);CHKERRQ(ierr);
  ierr = SNESSetDM(snes_alg,(DM)networkdm);CHKERRQ(ierr);
  ierr = SNESSetFunction(snes_alg,F_alg,AlgFunction,&user);CHKERRQ(ierr);
  ierr = SNESSetOptionsPrefix(snes_alg,"alg_");CHKERRQ(ierr);
  ierr = SNESSetFromOptions(snes_alg);CHKERRQ(ierr);

  /* Apply disturbance - resistive fault at user.faultbus */
  /* This is done by adding shunt conductance to the diagonal location
     in the Ybus matrix */
  user.alg_flg = PETSC_TRUE;

  /* Solve the algebraic equations */
  if (!rank) {
    ierr = PetscPrintf(PETSC_COMM_SELF,"\n... (2) Apply disturbance, solve algebraic equations ... \n");CHKERRQ(ierr);
  }
  ierr = SNESSolve(snes_alg,NULL,X);CHKERRQ(ierr);

  /* Disturbance period */
  ierr = TSSetTime(ts,user.tfaulton);CHKERRQ(ierr);
  ierr = TSSetMaxTime(ts,user.tfaultoff);CHKERRQ(ierr);
  ierr = TSSetExactFinalTime(ts,TS_EXACTFINALTIME_STEPOVER);CHKERRQ(ierr);
  ierr = TSSetIFunction(ts,NULL,(TSIFunction) FormIFunction,&user);CHKERRQ(ierr);

  user.alg_flg = PETSC_TRUE;
  if (!rank) {
    ierr = PetscPrintf(PETSC_COMM_SELF,"\n... (3) Disturbance period ... \n");CHKERRQ(ierr);
  }
  ierr = TSSolve(ts,X);CHKERRQ(ierr);

  /* Remove the fault */
  ierr = SNESSetFunction(snes_alg,F_alg,AlgFunction,&user);CHKERRQ(ierr);

  user.alg_flg = PETSC_FALSE;
  /* Solve the algebraic equations */
  if (!rank) {
    ierr = PetscPrintf(PETSC_COMM_SELF,"\n... (4) Remove fault, solve algebraic equations ... \n");CHKERRQ(ierr);
  }
  ierr = SNESSolve(snes_alg,NULL,X);CHKERRQ(ierr);
  ierr = SNESDestroy(&snes_alg);CHKERRQ(ierr);

  /* Post-disturbance period */
  ierr = TSSetTime(ts,user.tfaultoff);CHKERRQ(ierr);
  ierr = TSSetMaxTime(ts,user.tmax);CHKERRQ(ierr);
  ierr = TSSetExactFinalTime(ts,TS_EXACTFINALTIME_STEPOVER);CHKERRQ(ierr);
  ierr = TSSetIFunction(ts,NULL,(TSIFunction) FormIFunction,&user);CHKERRQ(ierr);

  user.alg_flg = PETSC_FALSE;
  if (!rank) {
    ierr = PetscPrintf(PETSC_COMM_SELF,"\n... (5) Post-disturbance period ... \n");CHKERRQ(ierr);
  }
  ierr = TSSolve(ts,X);CHKERRQ(ierr);

  ierr = PetscFree(user.ybusfault);CHKERRQ(ierr);
  ierr = VecDestroy(&F_alg);CHKERRQ(ierr);
  ierr = VecDestroy(&X);CHKERRQ(ierr);
  ierr = DMDestroy(&networkdm);CHKERRQ(ierr);
  ierr = TSDestroy(&ts);CHKERRQ(ierr);
  ierr = PetscFinalize();
  return ierr;
 }

/*TEST

   build:
      requires: double !complex !define(PETSC_USE_64BIT_INDICES)

   test:
      args: -ts_monitor -snes_converged_reason -alg_snes_converged_reason
      localrunfiles: X.bin Ybus.bin ex9busnetworkops

   test:
      suffix: 2
      nsize: 2
      args: -ts_monitor -snes_converged_reason -alg_snes_converged_reason
      localrunfiles: X.bin Ybus.bin ex9busnetworkops

TEST*/
