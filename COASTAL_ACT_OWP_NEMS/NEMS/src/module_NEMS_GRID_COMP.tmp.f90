















!-----------------------------------------------------------------------
!
      MODULE module_NEMS_GRID_COMP
!
!-----------------------------------------------------------------------
!***  This module contains codes directly related to the NEMS component.
!-----------------------------------------------------------------------
!
!***  The NEMS component lies in the hierarchy seen here:
!
!          Main program
!               |
!               |
!          NEMS component
!               |     |________________________.
!               |                              |
!          EARTH component        Ensemble Coupler component
!              /|!             / | !          ATM/OCN/ICE/WAV/LND/IPM/HYD .. components
!          |    |
!          |    |
!          |    |
!          |    (MOM5, HYCOM, etc.)
!          |
!          CORE component (GSM, NMM, FIM, GEN, etc.)
!
!-----------------------------------------------------------------------
!  2011-05-11  Theurich & Yang  - Modified for using the ESMF 5.2.0r_beta_snapshot_07.
!  2011-10-04  Yang  - Modified for using the ESMF 5.2.0r library.
!  2013-07     Theurich - Macro based ESMF error handling
!-----------------------------------------------------------------------
!
      USE ESMF
!
      USE module_NEMS_INTERNAL_STATE,ONLY: NEMS_INTERNAL_STATE          &
                                          ,WRAP_NEMS_INTERNAL_STATE
!
      USE ENS_CplComp_ESMFMod,ONLY: ENS_CplCompSetServices
!
      USE module_EARTH_GRID_COMP
!
      USE module_NEMS_UTILS,ONLY: ERR_MSG,MESSAGE_CHECK
!
!-----------------------------------------------------------------------
!
      IMPLICIT NONE
!
!-----------------------------------------------------------------------
!
      PRIVATE
!
      PUBLIC :: NEMS_REGISTER
!
!-----------------------------------------------------------------------
!
      INTEGER :: MEMBER_ID                                              &
                ,TOTAL_MEMBER
!
      INTEGER :: HH_INCREASE                                            &
                ,HH_START                                               &
                ,HH_FINAL
!
      INTEGER :: NUMBER_START                                           &
                ,NUMBER_FINAL
!
      INTEGER,DIMENSION(:),   ALLOCATABLE :: PE_MEMBER                     !<-- Tasks for each member
      INTEGER,DIMENSION(:, :),ALLOCATABLE :: PETLIST                       !<-- Task list for each member
!
      LOGICAL :: ENS_SPS                                                   !<-- Control of Stochastic Perturbation Scheme (SPS)
!
      CHARACTER(ESMF_MAXSTR),DIMENSION(:),ALLOCATABLE :: IMP_EARTH_NAME    !<-- Import state name of the EARTH components
      CHARACTER(ESMF_MAXSTR),DIMENSION(:),ALLOCATABLE :: EXP_EARTH_NAME    !<-- Export state name of the EARTH components
      CHARACTER(ESMF_MAXSTR),DIMENSION(:),ALLOCATABLE :: GC_EARTH_NAME     !<-- Name of the EARTH component
!
      TYPE(NEMS_INTERNAL_STATE),POINTER,SAVE :: NEMS_INT_STATE
      TYPE(WRAP_NEMS_INTERNAL_STATE)   ,SAVE :: WRAP
!
      TYPE(ESMF_Clock), SAVE :: CLOCK_NEMS                                 !<-- The ESMF Clock of the NEMS component
      TYPE(ESMF_Config),SAVE :: CF_NEMS                                    !<-- The configure object of the NEMS component
      TYPE(ESMF_VM),    SAVE :: VM_GLOBAL
      TYPE(ESMF_TIME),  SAVE :: STARTTIME, CURRTIME
!
      TYPE(ESMF_CplComp),SAVE :: ENS_CPL_COMP                              !<-- Ensemble Coupler components
      TYPE(ESMF_State),  SAVE :: ENS_CPL_IMP_STATE                         !<-- Import state of the Ensemble Coupler component
      TYPE(ESMF_State),  SAVE :: ENS_CPL_EXP_STATE                         !<-- Export state of the Ensemble Coupler component
!
      TYPE(ESMF_GridComp),DIMENSION(:),ALLOCATABLE,SAVE :: EARTH_GRID_COMP !<-- EARTH components for each member
      TYPE(ESMF_State),   DIMENSION(:),ALLOCATABLE,SAVE :: EARTH_IMP_STATE !<-- Import state of the EARTH component
      TYPE(ESMF_State),   DIMENSION(:),ALLOCATABLE,SAVE :: EARTH_EXP_STATE !<-- Export state of the EARTH component
!
!-----------------------------------------------------------------------
!
      CONTAINS

!-----------------------------------------------------------------------
!#######################################################################
!-----------------------------------------------------------------------
!
      SUBROUTINE NEMS_REGISTER(NEMS_GRID_COMP,RC_REG)
!
!-----------------------------------------------------------------------
!
!------------------------
!***  Argument Variables
!------------------------
!
      TYPE(ESMF_GridComp) :: NEMS_GRID_COMP                                !<-- The NEMS gridded component
!
      INTEGER,INTENT(OUT) :: RC_REG                                        !<-- Error return code
!
!---------------------
!***  Local Variables
!---------------------
!
      INTEGER :: RC
!
!-----------------------------------------------------------------------
!***********************************************************************
!-----------------------------------------------------------------------
!
      RC_REG = ESMF_SUCCESS
!
!-----------------------------------------------------------------------
!***  Register the NEMS Initialize, Run, and Finalize routines.
!-----------------------------------------------------------------------
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      MESSAGE_CHECK="Set Entry Point for NEMS Initialize"
!     CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOGMSG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
      CALL ESMF_GridCompSetEntryPoint(NEMS_GRID_COMP                    &  !<-- The NEMS component
                                     ,ESMF_METHOD_INITIALIZE            &  !<-- Subroutine type (Initialize)
                                     ,NEMS_INITIALIZE                   &  !<-- User's subroutine name
                                     ,phase=1                           &
                                     ,rc=RC)
      if (ESMF_LogFoundError(RC, msg="Breaking out of subroutine", line=140, file="module_NEMS_GRID_COMP.F90", rcToReturn=RC_REG)) return
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
!-----------------------------------------------------------------------
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      MESSAGE_CHECK="Set Entry Point for NEMS Run"
!     CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOGMSG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
      CALL ESMF_GridCompSetEntryPoint(NEMS_GRID_COMP                    &  !<-- The NEMS component
                                     ,ESMF_METHOD_RUN                   &  !<-- Subroutine type (Run)
                                     ,NEMS_RUN                          &  !<-- User's subroutine name
                                     ,phase=1                           &
                                     ,rc=RC)
      if (ESMF_LogFoundError(RC, msg="Breaking out of subroutine", line=155, file="module_NEMS_GRID_COMP.F90", rcToReturn=RC_REG)) return
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
!-----------------------------------------------------------------------
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      MESSAGE_CHECK="Set Entry Point for NEMS Finalize"
!     CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOGMSG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
      CALL ESMF_GridCompSetEntryPoint(NEMS_GRID_COMP                    &  !<-- The NEMS component
                                     ,ESMF_METHOD_FINALIZE              &  !<-- Subroutine type (Finalize)
                                     ,NEMS_FINALIZE                     &  !<-- User's subroutine name
                                     ,phase=1                           &
                                     ,rc=RC)
      if (ESMF_LogFoundError(RC, msg="Breaking out of subroutine", line=170, file="module_NEMS_GRID_COMP.F90", rcToReturn=RC_REG)) return
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
      END SUBROUTINE NEMS_REGISTER
!
!-----------------------------------------------------------------------
!#######################################################################
!-----------------------------------------------------------------------
!
      SUBROUTINE NEMS_INITIALIZE(NEMS_GRID_COMP                         &
                                ,IMP_STATE                              &
                                ,EXP_STATE                              &
                                ,CLOCK_MAIN                             &
                                ,RC_INIT)
!
!-----------------------------------------------------------------------
!
!------------------------
!***  Argument Variables
!------------------------
!
      TYPE(ESMF_GridComp) :: NEMS_GRID_COMP                                !<-- The NEMS component
!
      TYPE(ESMF_State) :: IMP_STATE                                     &  !<-- The NEMS import state
                         ,EXP_STATE                                        !<-- The NEMS export state
!
      TYPE(ESMF_Clock) :: CLOCK_MAIN                                       !<-- The main Clock
!
      INTEGER,INTENT(OUT) :: RC_INIT                                       !<-- Error return code
!
!-----------------------------------------------------------------------
!
!---------------------
!***  Local Variables
!---------------------
!
      TYPE(ESMF_TimeInterval) :: RUNDURATION, restartOffset
!
      CHARACTER(20) :: PELAB
!
      CHARACTER(ESMF_MAXSTR),DIMENSION(:),ALLOCATABLE :: EARTH_COMP_NAME &  !<-- Names of each member's EARTH component
                                                        ,IMP_EARTH_NAME  &  !<-- Import state name of the EARTH components
                                                        ,EXP_EARTH_NAME     !<-- Export state name of the EARTH components
!
      INTEGER :: I,IJ,J,RC,RC_USER
!
      INTEGER :: MYPE_GLOBAL                                            &
                ,NHOURS_FCST                                            &
                ,NSECONDS_FCST                                          &
                ,PE_MAX                                                 &
                ,TASKS                                                  &
                ,fhrot
!
      INTEGER,DIMENSION(:,:),ALLOCATABLE :: PETLIST                        !<-- Task list for each ensemble member
!
!-----------------------------------------------------------------------
!***********************************************************************
!-----------------------------------------------------------------------
!
      RC_INIT = ESMF_SUCCESS
!
!-----------------------------------------------------------------------
!***  For the moment, use a direct copy of the main Clock within
!***  the NEMS component.
!-----------------------------------------------------------------------
!
      CLOCK_NEMS=CLOCK_MAIN
!
!-----------------------------------------------------------------------
!***  What is the start time on the NEMS clock?
!-----------------------------------------------------------------------

! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      MESSAGE_CHECK = "Extract the start time of the NEMS clock"
!     CALL ESMF_LogWrite(MESSAGE_CHECK, ESMF_LOGMSG_INFO, rc = RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

      CALL ESMF_ClockGet(clock     = CLOCK_NEMS                         &
                        ,startTime = STARTTIME                          &
                        ,rc = RC)

      if (ESMF_LogFoundError(RC, msg="Breaking out of subroutine", line=251, file="module_NEMS_GRID_COMP.F90", rcToReturn=RC_INIT)) return
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
!-----------------------------------------------------------------------
!***  Allocate the NEMS component's internal state, point at it,
!***  and attach it to the NEMS component.
!-----------------------------------------------------------------------
!
      ALLOCATE(NEMS_INT_STATE,stat=RC)
      wrap%NEMS_INT_STATE=>NEMS_INT_STATE
!
      CALL ESMF_GridCompSetInternalState(NEMS_GRID_COMP                 &  !<--The NEMS component
                                        ,WRAP                           &  !<-- Pointer to the NEMS internal state
                                        ,RC)     
      if (ESMF_LogFoundError(RC, msg="Breaking out of subroutine", line=265, file="module_NEMS_GRID_COMP.F90", rcToReturn=RC_INIT)) return
!
!-----------------------------------------------------------------------
!***  Get the global VM (Virtual Machine).
!***  Obtain the total task count and the local task ID.
!-----------------------------------------------------------------------
!
      CALL ESMF_VMGetGlobal(vm = VM_GLOBAL                              &  !<-- The ESMF global Virtual Machine
                           ,rc = RC)
      if (ESMF_LogFoundError(RC, msg="Breaking out of subroutine", line=274, file="module_NEMS_GRID_COMP.F90", rcToReturn=RC_INIT)) return
!
      CALL ESMF_VMGet(vm       = VM_GLOBAL                              &  !<-- The ESMF global Virtual Machine
                     ,pecount  = TASKS                                  &  !<-- Total # of MPI tasks
                     ,localpet = MYPE_GLOBAL                            &  !<-- This task's global rank
                     ,rc       = RC)
      if (ESMF_LogFoundError(RC, msg="Breaking out of subroutine", line=280, file="module_NEMS_GRID_COMP.F90", rcToReturn=RC_INIT)) return

!jm 20140818
!jm   call rsl_lite_error_dup1( mype_global )                              !<-- Enable this for per-MPI task output
!jm                                                                        !<-- obtain other bits from JM

!
!-----------------------------------------------------------------------
!***  Create and load the Configure object which will hold the contents
!***  of the NEMS configure file.
!-----------------------------------------------------------------------
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      MESSAGE_CHECK="Create/Load the NEMS Configure Object"
!     CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOGMSG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
      CF_NEMS=ESMF_ConfigCreate(rc=RC)
!
      CALL ESMF_ConfigLoadFile(config   = CF_NEMS                       &  !<-- The configure object
                              ,filename = 'model_configure'             &  !<-- The name of the configure file
                              ,rc       = RC)
      if (ESMF_LogFoundError(RC, msg="Breaking out of subroutine", line=302, file="module_NEMS_GRID_COMP.F90", rcToReturn=RC_INIT)) return
!-----------------------------------------------------------------------
!***  Get the ensemble stochastic coupling flag from the config file.
!-----------------------------------------------------------------------
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      MESSAGE_CHECK="Extract the Ensemble Stochastic Coupling Flag from Config File"
!     CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOGMSG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
      CALL ESMF_ConfigGetAttribute(config = CF_NEMS                     &  !<-- The NEMS configure object
                                  ,value  = ENS_SPS                     &  !<-- Value of control flag for 
                                                                           !    stochastic perturbation scheme
                                  ,label  = 'ENS_SPS:'                  &  !<-- Flag's label in configure file
                                  ,default= .false.                     &
                                  ,rc     = RC)
      if (ESMF_LogFoundError(RC, msg="Breaking out of subroutine", line=318, file="module_NEMS_GRID_COMP.F90", rcToReturn=RC_INIT)) return
!
!-----------------------------------------------------------------------
!***  Reset the run duration in the NEMS Clock for the ensemble 
!***  stochastic perturbation case.
!-----------------------------------------------------------------------
!
      IF(ENS_SPS) THEN
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
        MESSAGE_CHECK="NEMS: Extract Forecast Length from Config File"
!       CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOGMSG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
        CALL ESMF_ConfigGetAttribute(config=CF_NEMS                     &
                                    ,value =NHOURS_FCST                 &
                                    ,label ='nhours_fcst1:'             &
                                    ,rc    =RC)
        if (ESMF_LogFoundError(RC, msg="Breaking out of subroutine", line=336, file="module_NEMS_GRID_COMP.F90", rcToReturn=RC_INIT)) return
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
        NSECONDS_FCST=NHOURS_FCST*3600                                     !<-- The forecast length (sec) (Integer)
!
!-----------------------------------------------------------------------
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
        MESSAGE_CHECK="NEMS: Set the Forecast Length"
!       CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOGMSG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
        CALL ESMF_TimeIntervalSet(timeinterval=RUNDURATION              &  !<-- The forecast length (s) (ESMF)
                                 ,s           =NSECONDS_FCST            &  !<-- The forecast length (s) (Integer)
                                 ,rc          =RC)
        if (ESMF_LogFoundError(RC, msg="Breaking out of subroutine", line=351, file="module_NEMS_GRID_COMP.F90", rcToReturn=RC_INIT)) return
!
        CALL ESMF_ClockSet(clock       = CLOCK_NEMS                     &  !<-- The NEMS Clock
                          ,runDuration = RUNDURATION                    &  !<-- The forecast length (s) (ESMF)
                          ,rc          = RC)
        if (ESMF_LogFoundError(RC, msg="Breaking out of subroutine", line=356, file="module_NEMS_GRID_COMP.F90", rcToReturn=RC_INIT)) return
!
      END IF
!
!-----------------------------------------------------------------------
!***  Extract the total number of EARTH ensemble members.
!-----------------------------------------------------------------------
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      MESSAGE_CHECK="Extract the Total Number of the EARTH Members from Config File"
!     CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOGMSG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
      CALL ESMF_ConfigGetAttribute(config = CF_NEMS                     &  !<-- The NEMS configure object
                                  ,value  = TOTAL_MEMBER                &  !<-- Total # of ensemble members
                                  ,label  = 'total_member:'             &  !<-- Flag in configure file 
                                  ,default= 1                           &
                                  ,rc     = RC)
      if (ESMF_LogFoundError(RC, msg="Breaking out of subroutine", line=374, file="module_NEMS_GRID_COMP.F90", rcToReturn=RC_INIT)) return
!
!-----------------------------------------------------------------------
!***  Allocate a standard set of arrays for each ensemble member.
!-----------------------------------------------------------------------
!
      ALLOCATE(EARTH_GRID_COMP (TOTAL_MEMBER))
      ALLOCATE(EARTH_IMP_STATE (TOTAL_MEMBER))
      ALLOCATE(EARTH_EXP_STATE (TOTAL_MEMBER))
      ALLOCATE(EARTH_COMP_NAME (TOTAL_MEMBER))
      ALLOCATE(IMP_EARTH_NAME  (TOTAL_MEMBER))
      ALLOCATE(EXP_EARTH_NAME  (TOTAL_MEMBER))
      ALLOCATE(PE_MEMBER       (TOTAL_MEMBER))
!
!-----------------------------------------------------------------------
!***  For each member create the names of the EARTH components and 
!***  ESMF states then fill in the task information.
!-----------------------------------------------------------------------
!
      PE_MEMBER = 0
!
      DO I = 1, TOTAL_MEMBER
!
        WRITE(EARTH_COMP_NAME(I), '("EARTH grid component", I2.2)') I
        WRITE(IMP_EARTH_NAME (I), '("EARTH import state",   I2.2)') I
        WRITE(EXP_EARTH_NAME (I), '("EARTH export state",   I2.2)') I
!
        WRITE(PELAB, '("PE_MEMBER", I2.2, ":")') I
! 
        CALL ESMF_ConfigGetAttribute(config = CF_NEMS                   &
                                    ,value  = PE_MEMBER(I)              &
                                    ,label  = TRIM(PELAB)               &
                                    ,rc     = RC)
        if (ESMF_LogFoundError(RC, msg="Breaking out of subroutine", line=407, file="module_NEMS_GRID_COMP.F90", rcToReturn=RC_INIT)) return
!
        IF(PE_MEMBER(I) == 0) PE_MEMBER(i) = TASKS / TOTAL_MEMBER
!
      END DO
!
      PE_MAX = 1
!
      DO I = 1, TOTAL_MEMBER
        PE_MAX = MAX(PE_MAX, PE_MEMBER(I))
      END DO
!
!-----------------------------------------------------------------------
!***  Set up the PE list.
!-----------------------------------------------------------------------
!
      ALLOCATE(PETLIST(1:PE_MAX, 1:TOTAL_MEMBER))
!
      IJ = 0
!
      DO J = 1, TOTAL_MEMBER
!
        DO I = 1, PE_MEMBER(J)
          PETLIST(I, J) = IJ
!
          IF(MYPE_GLOBAL == IJ) THEN
            MEMBER_ID = J
          END IF
!
          IJ = IJ + 1
        END DO
!
      END DO
!
!-----------------------------------------------------------------------
!***  Get the clock parameters for the ensemble stochastic perturbation 
!***  cycles from the config file.
!-----------------------------------------------------------------------
!
      IF(ENS_SPS) THEN 
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
        MESSAGE_CHECK="Extract the Ensemble Clock Parameters from Config File"
!       CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOGMSG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
        CALL ESMF_ConfigGetAttribute(config = CF_NEMS                   &
                                    ,value  = hh_increase               &
                                    ,label  = 'HH_INCREASE:'            &
                                    ,rc     = RC)
        if (ESMF_LogFoundError(RC, msg="Breaking out of subroutine", line=457, file="module_NEMS_GRID_COMP.F90", rcToReturn=RC_INIT)) return
!
        CALL ESMF_ConfigGetAttribute(config = CF_NEMS                   &
                                    ,value  = hh_start                  &
                                    ,label  = 'HH_START:'               &
                                    ,rc     = RC)
        if (ESMF_LogFoundError(RC, msg="Breaking out of subroutine", line=463, file="module_NEMS_GRID_COMP.F90", rcToReturn=RC_INIT)) return
!
        CALL ESMF_ConfigGetAttribute(config = CF_NEMS                   &
                                    ,value  = hh_final                  &
                                    ,label  = 'HH_FINAL:'               &
                                    ,rc     = RC)
        if (ESMF_LogFoundError(RC, msg="Breaking out of subroutine", line=469, file="module_NEMS_GRID_COMP.F90", rcToReturn=RC_INIT)) return
!
        NUMBER_START = HH_START / HH_INCREASE + 1
        NUMBER_FINAL = HH_FINAL / HH_INCREASE - 1
!
        PRINT *, 'ENS Coup ', NUMBER_START, NUMBER_FINAL                &
               , HH_START, HH_FINAL, HH_INCREASE
!
      END IF
!
!-----------------------------------------------------------------------
!***  Create the EARTH grid components.
!-----------------------------------------------------------------------
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      MESSAGE_CHECK="Create EARTH grid Components"
!     CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOGMSG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
      DO I = 1, TOTAL_MEMBER
        EARTH_GRID_COMP(i) = ESMF_GridCompCreate (                      &
                             name         = EARTH_COMP_NAME(I)          &  !<-- Name of element I of the EARTH component array
                            ,petlist      = PETLIST(1:PE_MEMBER(I), I)  &  !<-- Element I's PE list
                            ,config       = CF_NEMS                     &  !<-- Associate the NEMS config object with this element
                            ,rc           = RC)
        if (ESMF_LogFoundError(RC, msg="Breaking out of subroutine", line=494, file="module_NEMS_GRID_COMP.F90", rcToReturn=RC_INIT)) return
      END DO
!
!-----------------------------------------------------------------------
!***  Create the Ensemble Coupler component inside of
!***  the NEMS internal state.
!-----------------------------------------------------------------------
!
      IF(ENS_SPS) THEN
        ENS_CPL_COMP=ESMF_CplCompCreate(name = "ENS Cpl component"      &  
                                       ,rc   = RC)
        if (ESMF_LogFoundError(RC, msg="Breaking out of subroutine", line=505, file="module_NEMS_GRID_COMP.F90", rcToReturn=RC_INIT)) return
      END IF
!
!-----------------------------------------------------------------------
!***  Register the Initialize, Run, and Finalize routines of
!***  each element in the EARTH component array.
!-----------------------------------------------------------------------
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      MESSAGE_CHECK="Register EARTH Init, Run, Finalize"
!     CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOGMSG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
      DO I = 1, TOTAL_MEMBER
        CALL ESMF_GridCompSetServices(EARTH_GRID_COMP(I)                &  !<-- The EARTH gridded components
                                     ,EARTH_REGISTER                    &  !<-- User's name for the Register routine
                                     ,rc=RC)
        if (ESMF_LogFoundError(RC, msg="Breaking out of subroutine", line=522, file="module_NEMS_GRID_COMP.F90", rcToReturn=RC_INIT)) return
      END DO
!
!-----------------------------------------------------------------------
!***  Register the Initialize, Run, and Finalize routines of
!***  the Ensemble Coupler component.
!-----------------------------------------------------------------------
!
      IF(ENS_SPS) THEN
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
        MESSAGE_CHECK="Register Ensemble Coupler Init, Run, Finalize"
!       CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOGMSG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
        CALL ESMF_CplCompSetServices(ENS_CPL_COMP                       &
                                    ,ENS_CplCompSetServices             &  !<-- The user's name for the Register routine
                                    ,rc=RC)
        if (ESMF_LogFoundError(RC, msg="Breaking out of subroutine", line=540, file="module_NEMS_GRID_COMP.F90", rcToReturn=RC_INIT)) return
!
      END IF
!
!-----------------------------------------------------------------------
!***  Create the EARTH import and export states.
!-----------------------------------------------------------------------
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      MESSAGE_CHECK="Create the EARTH import states"
!     CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOGMSG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
      DO I = 1,TOTAL_MEMBER
        EARTH_IMP_STATE(I) = ESMF_StateCreate(                          &
                                         name = IMP_EARTH_NAME(I)  &
                                        ,stateintent = ESMF_STATEINTENT_IMPORT  &
                                        ,rc        = RC)
        if (ESMF_LogFoundError(RC, msg="Breaking out of subroutine", line=558, file="module_NEMS_GRID_COMP.F90", rcToReturn=RC_INIT)) return
      END DO
!
!-----------------------------------------------------------------------
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      MESSAGE_CHECK="Create the EARTH export states"
!     CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOGMSG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
      DO I = 1,TOTAL_MEMBER
        EARTH_EXP_STATE(I) = ESMF_StateCreate(                                 &
                                         name   = EXP_EARTH_NAME(I)       &
                                        ,stateintent = ESMF_STATEINTENT_EXPORT &
                                        ,rc          = RC)
        if (ESMF_LogFoundError(RC, msg="Breaking out of subroutine", line=573, file="module_NEMS_GRID_COMP.F90", rcToReturn=RC_INIT)) return
      END DO
!
!-----------------------------------------------------------------------
!***  Create the Ensemble Coupler import and export states.
!-----------------------------------------------------------------------
!
      IF(ENS_SPS) THEN
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
        MESSAGE_CHECK="Create the Ensemble Coupler import state"
!       CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOGMSG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
        ENS_CPL_IMP_STATE=ESMF_StateCreate(name   = "ENS_CPL_Import"         &
                                          ,stateintent = ESMF_STATEINTENT_IMPORT  &
                                          ,rc          = RC)
        if (ESMF_LogFoundError(RC, msg="Breaking out of subroutine", line=590, file="module_NEMS_GRID_COMP.F90", rcToReturn=RC_INIT)) return
!
!-----------------------------------------------------------------------
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
        MESSAGE_CHECK="Create the Ensemble Coupler export state"
!       CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOGMSG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
        ENS_CPL_EXP_STATE=ESMF_StateCreate(name = "ENS_CPL_Export"           &
                                          ,stateintent = ESMF_STATEINTENT_EXPORT  &
                                          ,rc        = RC)
        if (ESMF_LogFoundError(RC, msg="Breaking out of subroutine", line=602, file="module_NEMS_GRID_COMP.F90", rcToReturn=RC_INIT)) return
!
!-----------------------------------------------------------------------
!***  Nest the EARTH export/import states into the import/export states
!***  of the ensemble coupler.
!-----------------------------------------------------------------------
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
        MESSAGE_CHECK= "Add the EARTH states into the ENS_CPL states"
!       CALL ESMF_LogWrite(MESSAGE_CHECK, ESMF_LOGMSG_INFO, rc = RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
! 
        DO I = 1, TOTAL_MEMBER
          IF(MEMBER_ID == I) THEN
            CALL ESMF_StateAddReplace(ENS_CPL_IMP_STATE,(/EARTH_EXP_STATE(I)/), rc = RC)
            if (ESMF_LogFoundError(RC, msg="Breaking out of subroutine", line=617, file="module_NEMS_GRID_COMP.F90", rcToReturn=RC_INIT)) return
            CALL ESMF_StateAddReplace(ENS_CPL_EXP_STATE,(/EARTH_IMP_STATE(I)/), rc = RC)
            if (ESMF_LogFoundError(RC, msg="Breaking out of subroutine", line=619, file="module_NEMS_GRID_COMP.F90", rcToReturn=RC_INIT)) return
          END IF
        END DO
!
      END IF

!
!-----------------------------------------------------------------------
!***  Adjust the currTime of the NEMS clock: CLOCK_NEMS
!***  if the fhrot is > 0
!***  This will correctly set the NEMS and EARTH clocks in case of
!***  Restart-From-History.
!-----------------------------------------------------------------------

      CALL ESMF_ConfigGetAttribute(config = CF_NEMS &
                                   ,value  = fhrot &
                                   ,label  = 'fhrot:' &
                                   ,default = 0 &
                                   ,rc     = RC)
      if (ESMF_LogFoundError(RC, msg="Breaking out of subroutine", line=638, file="module_NEMS_GRID_COMP.F90", rcToReturn=RC_INIT)) return
      if (fhrot > 0) then
        CALL ESMF_TimeIntervalSet(restartOffset, h=fhrot,rc=RC)
        if (ESMF_LogFoundError(RC, msg="Breaking out of subroutine", line=641, file="module_NEMS_GRID_COMP.F90", rcToReturn=RC_INIT)) return
        CURRTIME = STARTTIME + restartOffset
        call ESMF_ClockSet(CLOCK_NEMS, currTime=CURRTIME, rc=RC)
        if (ESMF_LogFoundError(RC, msg="Breaking out of subroutine", line=644, file="module_NEMS_GRID_COMP.F90", rcToReturn=RC_INIT)) return
      endif
!
!-----------------------------------------------------------------------
!***  Execute the Initialize step of each element of the EARTH
!***  component array.
!-----------------------------------------------------------------------
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      MESSAGE_CHECK="Execute the Initialize step of the EARTH component"
!     CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOGMSG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
      DO I = 1, TOTAL_MEMBER
!
        IF(MEMBER_ID == I) THEN
          CALL ESMF_GridCompInitialize(gridcomp    = EARTH_GRID_COMP(I)  &
                                      ,importState = EARTH_IMP_STATE(I)  &
                                      ,exportState = EARTH_EXP_STATE(I)  &
                                      ,clock       = CLOCK_NEMS          &
                                      ,phase       = 1                   &
                                      ,userRc      = RC_USER             &
                                      ,rc          = RC)
          if (ESMF_LogFoundError(RC, msg="Breaking out of subroutine", line=667, file="module_NEMS_GRID_COMP.F90", rcToReturn=RC_INIT)) return
          if (ESMF_LogFoundError(RC_USER, msg="Breaking out of subroutine", line=668, file="module_NEMS_GRID_COMP.F90", rcToReturn=RC_INIT)) return
        END IF
!
      END DO
!
!-----------------------------------------------------------------------
!***  Execute the Initialize step of the Ensemble Coupler component.
!-----------------------------------------------------------------------
!
      IF(ENS_SPS) THEN
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
        MESSAGE_CHECK="Execute the Initialize step of the Ensemble Coupler component"
!       CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOGMSG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
        CALL ESMF_CplCompInitialize(cplcomp    =ENS_CPL_COMP            &
                                   ,importState=ENS_CPL_IMP_STATE       &
                                   ,exportState=ENS_CPL_EXP_STATE       &
                                   ,clock      =CLOCK_NEMS              &
                                   ,phase      =1                       &
                                   ,userRc     =RC_USER                 &
                                   ,rc         =RC)
        if (ESMF_LogFoundError(RC, msg="Breaking out of subroutine", line=691, file="module_NEMS_GRID_COMP.F90", rcToReturn=RC_INIT)) return
        if (ESMF_LogFoundError(RC_USER, msg="Breaking out of subroutine", line=692, file="module_NEMS_GRID_COMP.F90", rcToReturn=RC_INIT)) return
!
      END IF
!
!-----------------------------------------------------------------------
!
      END SUBROUTINE NEMS_INITIALIZE
!
!-----------------------------------------------------------------------
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!-----------------------------------------------------------------------
!
      SUBROUTINE NEMS_RUN(NEMS_GRID_COMP                                &
                         ,IMP_STATE                                     &
                         ,EXP_STATE                                     &
                         ,CLOCK_MAIN                                    &
                         ,RC_RUN)
!
!-----------------------------------------------------------------------
!
!------------------------
!***  Argument Variables
!------------------------
!
      TYPE(ESMF_GridComp) :: NEMS_GRID_COMP                                !<-- The NEMS component
!
      TYPE(ESMF_State) :: IMP_STATE                                     &  !<-- The NEMS import state
                         ,EXP_STATE                                        !<-- The NEMS export state
!
      TYPE(ESMF_Clock) :: CLOCK_MAIN                                       !<-- The main Clock
!
      INTEGER,INTENT(OUT) :: RC_RUN                                        !<-- Error return code
!
!---------------------
!***  Local Variables
!---------------------
!
      INTEGER :: HH,I,J,RC,RC_USER
!
      TYPE(ESMF_Time) :: CURRTIME
!
      TYPE(ESMF_TimeInterval) :: RUNDURATION
!
!-----------------------------------------------------------------------
!***********************************************************************
!-----------------------------------------------------------------------
!
      RC_RUN = ESMF_SUCCESS
!
!-----------------------------------------------------------------------
!***  For the moment, use a direct copy of the main Clock within
!***  the NEMS component.
!-----------------------------------------------------------------------
!
      CLOCK_NEMS=CLOCK_MAIN
!
!-----------------------------------------------------------------------
!***  Execute the Run step of each element in the EARTH component
!***  array.
!-----------------------------------------------------------------------
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      MESSAGE_CHECK="Execute the Run step of the EARTH components"
!     CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOGMSG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
      DO I = 1, TOTAL_MEMBER
!
        IF(MEMBER_ID == I) THEN
          CALL ESMF_GridCompRun(gridcomp    = EARTH_GRID_COMP(I)        &
                               ,importState = EARTH_IMP_STATE(I)        &
                               ,exportState = EARTH_EXP_STATE(I)        &
                               ,clock       = CLOCK_NEMS                &
                               ,phase       = 1                         &
                               ,userRc      = RC_USER                   &
                               ,rc          = RC)
          if (ESMF_LogFoundError(RC, msg="Breaking out of subroutine", line=768, file="module_NEMS_GRID_COMP.F90", rcToReturn=RC_RUN)) return
          if (ESMF_LogFoundError(RC_USER, msg="Breaking out of subroutine", line=769, file="module_NEMS_GRID_COMP.F90", rcToReturn=RC_RUN)) return
        END IF
!
      END DO
!
!-----------------------------------------------------------------------
!***  Execute the Run step of the Ensemble Coupler component.
!-----------------------------------------------------------------------
!
      sps: IF(ENS_SPS) THEN
!
!-----------------------------------------------------------------------
!
        DO I = NUMBER_START, NUMBER_FINAL
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
          MESSAGE_CHECK="Execute the Run step of the Ensemble Coupler component"
!         CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOGMSG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
          CALL ESMF_CplCompRun(cplcomp    =ENS_CPL_COMP                 &
                              ,importState=ENS_CPL_IMP_STATE            &
                              ,exportState=ENS_CPL_EXP_STATE            &
                              ,clock      =CLOCK_NEMS                   &
                              ,phase      =1                            &
                              ,userRc     =RC_USER                      &
                              ,rc         =RC)
          if (ESMF_LogFoundError(RC, msg="Breaking out of subroutine", line=796, file="module_NEMS_GRID_COMP.F90", rcToReturn=RC_RUN)) return
          if (ESMF_LogFoundError(RC_USER, msg="Breaking out of subroutine", line=797, file="module_NEMS_GRID_COMP.F90", rcToReturn=RC_RUN)) return
!
          CALL ESMF_VMBarrier(vm = VM_GLOBAL, rc = RC)
          if (ESMF_LogFoundError(RC, msg="Breaking out of subroutine", line=800, file="module_NEMS_GRID_COMP.F90", rcToReturn=RC_RUN)) return
!
!-----------------------------------------------------------------------
!***  Adjust the ESMF clock for the next run cycle.
!-----------------------------------------------------------------------

! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
          MESSAGE_CHECK = "Update the current time of the NEMS clock"
!         CALL ESMF_LogWrite(MESSAGE_CHECK, ESMF_LOGMSG_INFO, rc = RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
          CALL ESMF_ClockGet(clock       = CLOCK_NEMS                   &
                            ,runDuration = RUNDURATION                  &
                            ,rc          = RC)
          if (ESMF_LogFoundError(RC, msg="Breaking out of subroutine", line=814, file="module_NEMS_GRID_COMP.F90", rcToReturn=RC_RUN)) return
!
          CURRTIME = STARTTIME + RUNDURATION
!
          CALL ESMF_ClockSet(clock    = CLOCK_NEMS                      &
                            ,currTime = CURRTIME                        &
                            ,rc       = RC)
          if (ESMF_LogFoundError(RC, msg="Breaking out of subroutine", line=821, file="module_NEMS_GRID_COMP.F90", rcToReturn=RC_RUN)) return
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
          MESSAGE_CHECK = "Adjust clock - add one more cycle run duration"
!         CALL ESMF_LogWrite(MESSAGE_CHECK, ESMF_LOGMSG_INFO, rc = RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
          CALL ESMF_TimeIntervalGet(timeInterval = RUNDURATION          &
                                   ,h            = HH                   &
                                   ,rc           = RC)
          if (ESMF_LogFoundError(RC, msg="Breaking out of subroutine", line=831, file="module_NEMS_GRID_COMP.F90", rcToReturn=RC_RUN)) return
!
          HH = HH + HH_INCREASE
!
          CALL ESMF_TimeIntervalSet(timeInterval = RUNDURATION          &
                                   ,h            = hh                   &
                                   ,rc           = RC)
          if (ESMF_LogFoundError(RC, msg="Breaking out of subroutine", line=838, file="module_NEMS_GRID_COMP.F90", rcToReturn=RC_RUN)) return
!
          CALL ESMF_ClockSet(clock       = CLOCK_NEMS                   &
                            ,runDuration = RUNDURATION                  &
                            ,rc = RC)
          if (ESMF_LogFoundError(RC, msg="Breaking out of subroutine", line=843, file="module_NEMS_GRID_COMP.F90", rcToReturn=RC_RUN)) return
!
!-----------------------------------------------------------------------
!***  Execute the Run step of each element in the EARTH component
!***  array.
!-----------------------------------------------------------------------
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
          MESSAGE_CHECK="Execute the Run step of the EARTH component"
!         CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOGMSG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
          DO J = 1, TOTAL_MEMBER
!
            IF(MEMBER_ID == J) THEN
                CALL ESMF_GridCompRun(gridcomp    = EARTH_GRID_COMP(J)  &
                                     ,importState = EARTH_IMP_STATE(J)  &
                                     ,exportState = EARTH_EXP_STATE(J)  &
                                     ,clock       = CLOCK_NEMS          &
                                     ,phase       = 1                   &
                                     ,userRc      = RC_USER             &
                                     ,rc          = RC)
                if (ESMF_LogFoundError(RC, msg="Breaking out of subroutine", line=865, file="module_NEMS_GRID_COMP.F90", rcToReturn=RC_RUN)) return
                if (ESMF_LogFoundError(RC_USER, msg="Breaking out of subroutine", line=866, file="module_NEMS_GRID_COMP.F90", rcToReturn=RC_RUN)) return
            END IF
!
          END DO
!
          PRINT*, 'Complete EARTH Run Cycle ', I + 1
        END DO
!
!-----------------------------------------------------------------------
!
      ELSE sps
!
         CALL ESMF_ClockGet(clock       = CLOCK_NEMS                    &
                           ,runDuration = RUNDURATION                   &
                           ,rc          = RC)
         if (ESMF_LogFoundError(RC, msg="Breaking out of subroutine", line=881, file="module_NEMS_GRID_COMP.F90", rcToReturn=RC_RUN)) return
!
!-----------------------------------------------------------------------
!
      END IF sps
!
!-----------------------------------------------------------------------
!
      END SUBROUTINE NEMS_RUN
!
!-----------------------------------------------------------------------
!#######################################################################
!-----------------------------------------------------------------------
!
      SUBROUTINE NEMS_FINALIZE(NEMS_GRID_COMP                           &
                              ,IMP_STATE                                &
                              ,EXP_STATE                                &
                              ,CLOCK_MAIN                               &
                              ,RC_FINALIZE)
!
!-----------------------------------------------------------------------
!
!------------------------
!***  Argument Variables
!------------------------
!
      TYPE(ESMF_GridComp) :: NEMS_GRID_COMP                                !<-- The NEMS component
!
      TYPE(ESMF_State) :: IMP_STATE                                     &  !<-- The NEMS import state
                         ,EXP_STATE                                        !<-- The NEMS export state
!
      TYPE(ESMF_Clock) :: CLOCK_MAIN                                       !<-- The main Clock
!
      INTEGER,INTENT(OUT) :: RC_FINALIZE                                   !<-- Error return code
!
!---------------------
!***  Local Variables
!---------------------
!
      INTEGER :: I,RC,RC_USER
!
!-----------------------------------------------------------------------
!***********************************************************************
!-----------------------------------------------------------------------
!
      RC_FINALIZE = ESMF_SUCCESS
!
!-----------------------------------------------------------------------
!***  Execute the Finalize step of each element of the 
!***  EARTH component array.
!-----------------------------------------------------------------------
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      MESSAGE_CHECK="Execute the Finalize step of the EARTH component"
!     CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOGMSG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
      DO I = 1, TOTAL_MEMBER
!
        IF(MEMBER_ID == I) THEN
          CALL ESMF_GridCompFinalize(gridcomp    = EARTH_GRID_COMP(I)   &
                                    ,importState = EARTH_IMP_STATE(I)   &
                                    ,exportState = EARTH_EXP_STATE(I)   &
                                    ,clock       = CLOCK_NEMS           &
                                    ,phase       = 1                    &
                                    ,userRc      = RC_USER              &
                                    ,rc          = RC)
          if (ESMF_LogFoundError(RC, msg="Breaking out of subroutine", line=948, file="module_NEMS_GRID_COMP.F90", rcToReturn=RC_FINALIZE)) return
          if (ESMF_LogFoundError(RC_USER, msg="Breaking out of subroutine", line=949, file="module_NEMS_GRID_COMP.F90", rcToReturn=RC_FINALIZE)) return
        END IF
!
      END DO
!
!-----------------------------------------------------------------------
!
      IF(ENS_SPS) THEN
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
        MESSAGE_CHECK="Execute the Finalize step of the Ensemble Coupler component"
!       CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOGMSG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
        CALL ESMF_CplCompFinalize(cplcomp    =ENS_CPL_COMP              &
                                 ,importState=ENS_CPL_IMP_STATE         &
                                 ,exportState=ENS_CPL_EXP_STATE         &
                                 ,clock      =CLOCK_NEMS                &
                                 ,phase      =1                         &
                                 ,userRc     =RC_USER                   &
                                 ,rc         =RC)
        if (ESMF_LogFoundError(RC, msg="Breaking out of subroutine", line=970, file="module_NEMS_GRID_COMP.F90", rcToReturn=RC_FINALIZE)) return
        if (ESMF_LogFoundError(RC_USER, msg="Breaking out of subroutine", line=971, file="module_NEMS_GRID_COMP.F90", rcToReturn=RC_FINALIZE)) return
!
      END IF
!
!-----------------------------------------------------------------------
!
      END SUBROUTINE NEMS_FINALIZE
!
!-----------------------------------------------------------------------
!
      END MODULE module_NEMS_GRID_COMP
!
!-----------------------------------------------------------------------
