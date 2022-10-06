#define FILENAME "NWM_Hydraulic_Gluecode"
#define MODNAME "NWM_Hydraulic_Gluecode.F90"
#include "NWM_NUOPC_Macros.h"

#define DEBUG=on

module NWM_HYC_Gluecode
! !MODULE: NWM Coastal Hydraulic Engine_Gluecode
!
! !DESCRIPTION:
!   This module connects initialize, advance,
!   and finalize to DFlow.
!
! !REVISION HISTORY:
!  13Oct15    Dan Rosen - Initial design and development
!  12/12/2020  Beheen Trimble - calls DFlow standalone directly
!
  use ESMF
  use NUOPC
  use NWM_ESMF_Extensions
  use NWM_ESMF_Utility

  ! dflow model
  ! avoid namespace collision with network
  use unstruc_api
  use network_data
  use m_flow
  use m_wind
  use dfm_error
  use m_flowgeom
  use m_flowtimes           ! time_user -- Next time of external forcings update (steps increment by dt_user).
                            ! dt_user -- User specified time step (s) for external forcing update.
                            ! dt_max -- Computational timestep limit by user.
                            ! tstart_user -- User specified time start (s) w.r.t.  refdat
                            ! tstop_user -- User specified time stop (s) w.r.t. refdat
                            ! dts -- internal computational timestep (s)
                            ! dtsc --  max timstep of limiting point kkcflmx, zero if larger than dt_max

  use unstruc_model
  use unstruc_files
  use m_partitioninfo       ! DFM_COMM_DFMWORLD, ja_mpi_init_by_fm, my_rank, numranks, sdmn, jampi
  use mpi
  use m_flowexternalforcings
  use m_netstore
  implicit none

  private

  public :: NWM_HYC_Init
  public :: locstream_data
  public :: locstream_discharge
  public :: locstream_lateral_discharge
  public :: locstream_lat
  public :: locstream_lon
  public :: locstream_waterlevel
  public :: locstream_velx
  public :: locstream_vely
  public :: NWM_HYCMeshCreate
  public :: NWM_HYCMeshUGRIDCreate
  public :: NWM_HYC_Run
  public :: NWM_HYC_Fin
  public :: NWM_HYCField
  public :: NWM_HYCFieldList
  public :: NWM_HYCFieldDictionaryAdd
  public :: NWM_HYCFieldCreate
  public :: NWM_HYCClock
  public :: printClock
  public :: NWM_Read_HUCS_agg
  public :: DFLOW_VBC_derivation

  type NWM_HYCField
    character(len=64)   :: stdname        = ' '  ! human readable name
    character(len=64)   :: shortname      = ' '  ! variable of the field
    character(len=64)   :: desc           = ' '
    character(len=10)   :: units          = ' '
    character(len=64)   :: transferOffer  = 'will provide'
    logical             :: adImport       = .FALSE.
    logical             :: realizedImport = .FALSE.
    logical             :: adExport       = .FALSE.
    logical             :: realizedExport = .FALSE.
    logical             :: assoc          = .FALSE. 
    real(ESMF_KIND_R8), dimension(:), pointer :: farrayPtr => null()
  end type NWM_HYCField

  type(NWM_HYCField),dimension(12) :: NWM_HYCFieldList = (/ & 

    NWM_HYCField( & !(1)
      stdname='link', units='NA', &
      desc='Link ID (NHDFlowline_network COMID)', shortname='feature id', &
      adImport=.TRUE.,adExport=.FALSE.), &

    NWM_HYCField( & !(2)
      stdname='lat', units='Degrees', &
      desc='Latitude of streamflow link', shortname='latitude', &
      adImport=.FALSE.,adExport=.TRUE.), &

    NWM_HYCField( & !(3)
      stdname='lon', units='Degrees', &
      desc='Longitue of streamflow link', shortname='longitude', &
      adImport=.FALSE.,adExport=.TRUE.), &

    NWM_HYCField( & !(4)
      stdname='flow_rate', units='m3 s-1', &
      desc='volume of fluid passing by some location through an area during a period of time.', shortname='streamflow', &
      adImport=.TRUE.,adExport=.FALSE.), &

    NWM_HYCField( & !(5)
      stdname='lateral_flow_rate', units='m3 s-1', &
      desc='total combined inflow coming into a channel reach', shortname='lateral_streamflow', &
      adImport=.TRUE.,adExport=.FALSE.), &

    NWM_HYCField( & !(6) 
      stdname='eastward_wind', units='m s-1', &
      desc='UGRD, 10-m eastward wind', shortname='u10', &
      adImport=.TRUE.,adExport=.FALSE.), &

    NWM_HYCField( & !(7) V_PHY     (XSTART:XEND,KDS:KDE,YSTART:YEND) )  ! 3D V wind component [m/s]
      stdname='northward_wind', units='m s-1', &
      desc='VGRD, 10-m northward wind', shortname='v10', &
      adImport=.TRUE.,adExport=.FALSE.), &

    NWM_HYCField( & !(8) 
      stdname='air_pressure', units='Pa', &   !atm pmsl
      desc='surface pressure at sea level.', shortname='msl', &    
      adImport=.TRUE.,adExport=.FALSE.), &

    NWM_HYCField( & !(9 ADCIRC waterlevel advertized name)
      stdname="sea_surface_height_above_sea_level_dflow",  units='m', &
      desc='waterlevel', shortname='zeta_dflow', &
      adImport=.TRUE.,adExport=.FALSE.), &

    NWM_HYCField( & !(10 ADCIRC current velocity x advertized name)
      stdname="surface_eastward_sea_water_velocity_dflow",  units='m s-1', &
      desc='ucx', shortname='velx_dflow', &
      adImport=.TRUE.,adExport=.FALSE.), &

    NWM_HYCField( & !(11 ADCIRC current velocity y advertized name)
      stdname="surface_northward_sea_water_velocity_dflow",  units='m s-1', &
      desc='ucy', shortname='vely_dflow', &
      adImport=.TRUE.,adExport=.FALSE.), &

    NWM_HYCField( & !(12 ADCIRC current velocity y advertized name)
      stdname="eastward_wave_radiation_stress",  units='m s-1', &
      desc='ucy', shortname='sxx', &
      adImport=.FALSE.,adExport=.TRUE.) /)



  ! Added to consider the adaptive time step from driver.
  real                  :: dt0        = UNINITIALIZED
  real                  :: dtrt_ter0  = UNINITIALIZED
  real                  :: dtrt_ch0   = UNINITIALIZED
  integer               :: dt_factor0 = UNINITIALIZED
  integer               :: dt_factor  = UNINITIALIZED
  ! Added to track the driver clock
  character(len=19)     :: startTimeStr = "0000-00-00_00:00:00"

  character(len=512)    :: logMsg

  ! Global variables needed in module to initalize HUCS ID
  ! communication for lateral discharge
  character(len=10), dimension(:), allocatable   :: NWM_row
  real, dimension(:), allocatable   :: NWM_CommonID
  real, dimension(:), allocatable   :: NWM_lon
  real, dimension(:), allocatable   :: NWM_lat
  real, dimension(:), allocatable   :: HUCS_ID
  real, dimension(:), allocatable   :: HUCS_unique_ID
  real(ESMF_KIND_R8), dimension(:), allocatable   :: NWM_HUCS_agg_streamflow

  ! Variables needed calculate Normal and Tangential velocity components for
  ! NUOPC cap
  integer :: VBC_count = 0
  integer :: VBC_start = 1
  integer :: VBC_end = 0
  integer :: DFLOW_Q_FILE = 1000
  integer :: DFLOW_LATQ_FILE = 2000
  integer :: DFLOW_WL_FILE = 3000
 
  real(ESMF_KIND_R8), dimension(:), allocatable   :: DFLOW_VBC_lat
  real(ESMF_KIND_R8), dimension(:), allocatable   :: DFLOW_VBC_lon
  real(ESMF_KIND_R8), dimension(:), allocatable   :: DFLOW_VBC_lat_left
  real(ESMF_KIND_R8), dimension(:), allocatable   :: DFLOW_VBC_lon_left
  real(ESMF_KIND_R8), dimension(:), allocatable   :: DFLOW_VBC_lat_right
  real(ESMF_KIND_R8), dimension(:), allocatable   :: DFLOW_VBC_lon_right
  integer, dimension(:), allocatable              :: DFLOW_VBC_ID

  real(ESMF_KIND_R8), dimension(:), allocatable   :: DFLOW_VBC_UTANy
  real(ESMF_KIND_R8), dimension(:), allocatable   :: DFLOW_VBC_UTANx
  real(ESMF_KIND_R8), dimension(:), allocatable   :: DFLOW_VBC_UTANy_left
  real(ESMF_KIND_R8), dimension(:), allocatable   :: DFLOW_VBC_UTANx_left
  real(ESMF_KIND_R8), dimension(:), allocatable   :: DFLOW_VBC_UTANy_right
  real(ESMF_KIND_R8), dimension(:), allocatable   :: DFLOW_VBC_UTANx_right
  real(ESMF_KIND_R8), dimension(:), allocatable   :: DFLOW_VBC_UNORx
  real(ESMF_KIND_R8), dimension(:), allocatable   :: DFLOW_VBC_UNORy
  real(ESMF_KIND_R8), dimension(:), allocatable   :: DFLOW_VBC_UNORx_left
  real(ESMF_KIND_R8), dimension(:), allocatable   :: DFLOW_VBC_UNORy_left
  real(ESMF_KIND_R8), dimension(:), allocatable   :: DFLOW_VBC_UNORx_right
  real(ESMF_KIND_R8), dimension(:), allocatable   :: DFLOW_VBC_UNORy_right
  real(ESMF_KIND_R8), dimension(:), allocatable   :: DFLOW_VBC_distance
  real(ESMF_KIND_R8), dimension(:), allocatable   :: DFLOW_VBC_distance_left
  real(ESMF_KIND_R8), dimension(:), allocatable   :: DFLOW_VBC_distance_right

  real(ESMF_KIND_R8), dimension(:), allocatable   :: DFLOW_zbndq_t0, DFLOW_zbndq_t1, DFLOW_zbndq_temp_interp
  real(ESMF_KIND_R8), dimension(:), allocatable   :: DFLOW_zbndz_t0, DFLOW_zbndz_t1, DFLOW_zbndz_temp_interp
  real(ESMF_KIND_R8), dimension(:), allocatable   :: DFLOW_zbndt_t0, DFLOW_zbndt_t1, DFLOW_zbndt_temp_interp
  real(ESMF_KIND_R8), dimension(:), allocatable   :: DFLOW_zbndn_t0, DFLOW_zbndn_t1, DFLOW_zbndn_temp_interp

  public :: NWM_row, NWM_CommonID, NWM_lon, NWM_lat, HUCS_ID, HUCS_unique_ID, NWM_HUCS_agg_streamflow
  public :: VBC_count, VBC_start, VBC_end, DFLOW_VBC_lat, DFLOW_VBC_lon, DFLOW_VBC_ID
  public :: DFLOW_VBC_lat_left, DFLOW_VBC_lon_left, DFLOW_VBC_lat_right, DFLOW_VBC_lon_right
  public :: DFLOW_VBC_UTANy, DFLOW_VBC_UTANx, DFLOW_VBC_UNORy, DFLOW_VBC_UNORx, DFLOW_VBC_distance
  public :: DFLOW_VBC_UTANy_left, DFLOW_VBC_UTANx_left, DFLOW_VBC_UNORy_left, DFLOW_VBC_UNORx_left, DFLOW_VBC_distance_left
  public :: DFLOW_VBC_UTANy_right, DFLOW_VBC_UTANx_right, DFLOW_VBC_UNORy_right, DFLOW_VBC_UNORx_right, DFLOW_VBC_distance_right
  public :: DFLOW_Q_FILE, DFLOW_LATQ_FILE, DFLOW_WL_FILE
  public :: DFLOW_zbndq_t0, DFLOW_zbndz_t0, DFLOW_zbndt_t0, DFLOW_zbndn_t0
  public :: DFLOW_zbndq_t1, DFLOW_zbndz_t1, DFLOW_zbndt_t1, DFLOW_zbndn_t1
  public :: DFLOW_zbndq_temp_interp, DFLOW_zbndz_temp_interp, DFLOW_zbndt_temp_interp, DFLOW_zbndn_temp_interp
  !-----------------------------------------------------------------------------
  ! Model Glue Code
  !-----------------------------------------------------------------------------
contains

#undef METHOD
#define METHOD "NWM_HYC_Init"

  subroutine NWM_HYC_Init(vm, nemsclock, rc)

    use unstruc_display, only : jaGUI

#ifdef HAVE_MPI
    use mpi
#endif

    ! arguments
    type(ESMF_VM),intent(in)       :: vm
    !integer, intent(in)            :: nuopc_comm
    type(ESMF_Clock),intent(in)    :: nemsclock
    integer                        :: rc 


    ! Local variables
    ! read from mdu file - tstart, tstop, tunit, time_user
    character(20)               :: tunit, refdate 
    character(*),parameter      :: filename = "FlowFM.mdu"

    integer                     :: localPet       ! current process number
    integer                     :: stat       
    integer                     :: esmf_comm, dflow_comm
    character(20)               :: starttime_str="1111"
    character(10)                :: RANK_Q, RANK_LATQ, RANK_WL
    ! for dflow model
    integer :: j, i, inerr  ! number of the initialisation error
    logical :: mpi_initd

#ifdef DEBUG
    character(ESMF_MAXSTR)      :: logMsg
#endif
    WRITE(*,*) "Entered NWM HYC Init"
#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": entered "//METHOD, ESMF_LOGMSG_INFO)
#endif
    WRITE(*,*) "ESMF SUCCESS"
    rc = ESMF_SUCCESS
    WRITE(*,*) "Setting mpiCommunicator for dflow"
    ! Set mpiCommunicator for dflow
    !call ESMF_VMGet(vm, localPet=localPet, mpiCommunicator=esmf_comm, rc=rc)
    call ESMF_VMGet(vm, mpiCommunicator=esmf_comm, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    !call NWM_HYCVMPrint(vm)
    write(*,*) "duplicate mpi communicator"
    call MPI_Comm_dup(esmf_comm, dflow_comm, rc)
    ! Duplicate the MPI communicator not to interfere with ESMF communications.
    ! The duplicate MPI communicator can be used in any MPI call in the user
    ! code. Here the MPI_Barrier() routine is called.
    !write(*,*) "mpi barrier"
    call MPI_Barrier(dflow_comm, rc) 
     
    !write(*,*) "mpi initalized"
    call mpi_initialized(mpi_initd, inerr)
    if (.not. mpi_initd) then
      ja_mpi_init_by_fm = 1
      call mpi_init(rc)
    else
      ja_mpi_init_by_fm = 0
    end if

    ! Assign D-Flow common world variable to ESMF communicator for D-Flow
    ! threads
    DFM_COMM_DFMWORLD = dflow_comm
    call mpi_comm_rank(DFM_COMM_DFMWORLD,my_rank,rc)
    call mpi_comm_size(DFM_COMM_DFMWORLD,numranks,rc)

    !DFM_COMM_DFMWORLD = esmf_comm
    !call mpi_comm_rank(DFM_COMM_DFMWORLD,my_rank,rc)
    !call mpi_comm_size(DFM_COMM_DFMWORLD,numranks,rc)

    WRITE(*,*) "my rank"
    WRITE(*,*) my_rank
    WRITE(*,*) "numranks"
    WRITE(*,*) numranks
    if (numranks.le.1 ) then
      jampi = 0
    end if
    !write(*,*) "jampi"
    !write(*,*) jampi
    write(*,*) "DFM COMM DFMWORLD"
    write(*,*) DFM_COMM_DFMWORLD
   ! ! make domain number string as soon as possible
    write(sdmn, '(I4.4)') my_rank
    !print*, "Beheen - my_rank: ", sdmn

    ! do this until default has changed
    jaGUI = 0





    !$OMP CRITICAL
    !DFLOW_Q_FILE = DFLOW_Q_FILE + my_rank
    !DFLOW_LATQ_FILE = DFLOW_LATQ_FILE + my_rank
    !DFLOW_WL_FILE = DFLOW_WL_FILE + my_rank
    !WRITE(RANK_Q,'(i4)') DFLOW_Q_FILE
    !WRITE(RANK_LATQ,'(i4)') DFLOW_LATQ_FILE
    !WRITE(RANK_WL,'(i4)') DFLOW_WL_FILE

    !OPEN(DFLOW_Q_FILE,file='DFLOW_Q_BND_'//TRIM(RANK_Q)//'.txt',status='new')
    !OPEN(DFLOW_LATQ_FILE,file='DFLOW_LATQ_BND_'//TRIM(RANK_LATQ)//'.txt',status='new')
    !OPEN(DFLOW_WL_FILE,file='DFLOW_WL_BND_'//TRIM(RANK_WL)//'.txt',status='new')

    ! Initalize NWM HUCS global variables for each NWM thread individually
    call NWM_Read_HUCS_agg(my_rank)
   
    !$OMP END CRITICAL 

    !write(*,*) "enter start"
    !< dflowfm_kernel/src/rest.F90:1326
    ! init diagnostic files, version info, 
    !call start() ! required because of initprogram, which calls initsysenv
                 ! unstruc_files.f90:376 - inidia(basename)
                 ! rest.F90 - FIRSTLIN(MRGF)
                 ! unstruc_startup.f90:48 - initProgram()
    call inidat() ! net.F90:37
    !write(*,*) "enter api loadmodel"
    write(*,*) 'Initializing model', trim(filename)
    
    call api_loadmodel(trim(filename))      ! unstruc_api.F90:197 
              ! resetFullFlowModel()
              ! unstruc_model.f90:416 loadmodel(file_name)
              ! resetModel(),setmd_ident(filename),readMDUFile(filename, istat)
              ! loadCachingFile(md_ident, md_netfile, md_usecaching) 
              ! load_network_from_flow1d(md_1dfiles,found_1d_network)  OR
              ! loadNetwork(md_netfile, istat, jadoorladen)
              ! unc_read_net(filename, K0, L0, NUMKN, NUMLN, istat)
              ! unc_read_net_ugrid(filename, numk_keep, numl_keep, numk_read, numl_read, ierr)
              ! admin_network(network, iDumk, iDuml), CLOSEWORLD()                

    ! write(*,*) 'model initialized: ', numk, ' nodes', sdmn, ' pet'

    !PETSC must be called AFTER reading the mdu file, so the icgsolver option is
    !known to startpetsc
    write(*,*) "entering startpetsc"
    call startpetsc()
    write(*,*) "entering flow init"
    rc = flowinit()  !< unstruc_api.F90: 212
    
    !tstart_user .ne. starttime tstop_user .ne. stoptime
    print*, "Beheen irefdate: ", irefdate, refdat

    do j=0,numranks
      if (my_rank == j) then
        print*, "tstart_user: ", tstart_user
        print*, "tstop_user : ", tstop_user
        print*, "time_user  : ", time_user
        print*, "dt_init, dt_max: ", dt_init, dt_max
        print*, "dts, dt_user: ", dts, dt_user
        call printClock(nemsclock)
      endif
      call MPI_Barrier(dflow_comm, rc)
      if(ESMF_STDERRORCHECK(rc)) return
    enddo

    time_user = tstart_user  !< modules.f90:4408 -  m_flowtimes

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": leaving "//METHOD, ESMF_LOGMSG_INFO)
#endif


  end subroutine

  
  subroutine printClock(clock)

    type(ESMF_Clock) :: clock

    ! time_step, start and stop times
    type(ESMF_TimeInterval) :: timestep, currsimtime, prevsimtime, runduration
    type(ESMF_Time) :: starttime, stoptime, reftime, currtime, prevtime
    integer(ESMF_KIND_I4) :: smonth, sday, shour, smint, iref_year
    integer :: iref_month, iref_day
    character(8) :: ref_year, ref_month, ref_day
    integer(ESMF_KIND_I4) :: emonth, eday, ehour, emint
    integer(ESMF_KIND_I4) :: dt
    integer(ESMF_KIND_I8) :: syear, ssec, ref_sec
    integer(ESMF_KIND_I8) :: eyear, esec, advancecount
    real(ESMF_KIND_R8)    :: runtimestepcount
    integer :: rc, alarmcount, timezone, stat
    type(ESMF_Calendar)      :: calendar
    type(ESMF_CalKind_Flag)  :: calkindflag
    type(ESMF_Direction_Flag):: direction
    character (20)           :: name
 
    rc = ESMF_SUCCESS
    
    ref_year=refdat(1:4)      !< dflow Reference date (e.g., '20090101').
    ref_month=refdat(5:6)
    ref_day=refdat(7:8)
    read(ref_year,*,iostat=stat) iref_year
    read(ref_month,*,iostat=stat) iref_month
    read(ref_day,*,iostat=stat) iref_day
    
    call ESMF_TimeSet(reftime,yy=iref_year,mm=iref_month,dd=iref_day)
    

    ! 1 day = 24 hr * 3600 sec = 86400, timestep 6 min * 60 sec = 360 sec 
    ! Get a clock properites 
    call ESMF_ClockGet(clock, &
         timeStep=timestep, startTime=starttime, stopTime=stoptime, &
         runDuration=runduration, runTimeStepCount=runtimestepcount,& 
         refTime=reftime, currTime=currtime, prevTime=prevtime, &
         currSimTime=currsimtime, prevSimTime=prevsimtime, calendar=calendar, &
         calkindflag=calkindflag, timeZone=timezone, &
         advanceCount=advancecount, alarmCount=alarmcount, direction=direction, &
         name=name, rc=rc)

    call ESMF_ClockGet(clock,timeStep=timestep,startTime=starttime,stopTime=stoptime,rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out

    call ESMF_TimeIntervalGet(timestep, s=dt, rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out
   
    call ESMF_TimeGet(starttime,yy_i8=syear,mm=smonth,dd=sday,h=shour)
    call ESMF_TimeGet(stoptime,yy_i8=eyear,mm=emonth,dd=eday,h=ehour)
    call ESMF_TimeGet(starttime,s_i8=ssec)
    call ESMF_TimeGet(stoptime,s_i8=esec)

    print*, "timestep sec: ",dt
    print*, "start date: ",syear,smonth,sday,shour
    print*, "stop  date: ",eyear,emonth,eday,ehour
    print*, "start/stop in secs : ", ssec, esec

    if (reftime == starttime) then
      print*, "Beheen they are equal"
      time_user = tstart_user
    end if   
    
    call ESMF_TimePrint(reftime, options="string", rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out

    call ESMF_TimePrint(starttime, options="string", rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out

    call ESMF_TimePrint(stoptime, options="string", rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out


  end subroutine
  !-----------------------------------------------------------------------------
  ! command per timestep.
  !-----------------------------------------------------------------------------

#undef METHOD
#define METHOD "NWM_HYC_Run"
  subroutine NWM_HYC_Run(esmfnext,importState,exportState,rc)
   
    !type(ESMF_VM), intent(in)               :: vm      ! see if we need this
    !type(ESMF_Clock),intent(in)             :: clock
    real(ESMF_KIND_R8)                      :: esmfnext
    type(ESMF_State),intent(inout)          :: importState
    type(ESMF_State),intent(inout)          :: exportState
    integer, intent(out)                    :: rc
   
    ! local variables
    integer :: jastop=0, iresult = DFM_NOERR

    rc = ESMF_SUCCESS
#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": entered "//METHOD, ESMF_LOGMSG_INFO)
#endif

    ! set the DFLOWFM clock at the same stop time....
    ! TODO check, assuming seconds here...
    tstop_user = esmfnext
    WRITE(*,*) "NWM HYC Run results"
    WRITE(*,*) time_user
    WRITE(*,*) tstop_user
    do while (time_user .lt. tstop_user .and. jastop.eq.0 .and. iresult==DFM_NOERR)  ! time loop
      ! do computational flowsteps until timeuser
      !WRITE(*,*) "flowstep called"
      call flowstep(jastop,iresult)  ! jastop(1) = stop, not=0
                                ! starttimer(ITOTAL), 
                                ! flow_usertimestep(key, iresult)-- one user_step consists of several flow computational time steps 
                                ! timstrt('User time loop', handle_user)
                                ! flow_init_usertimestep(iresult)
                                ! flow_run_usertimestep(key,iresult)
                                ! flow_finalize_usertimestep(iresult)
                                ! timstop(handle_user)
      write(*,*) "time user iteration"
      write(*,*) time_user
      if(nbndz > 0) then
      write(*,*) "water level id and EC Module update"
      write(*,*) idbndz(1:2)
      write(*,*) zbndz(1:2)
      endif
      if(numlatsg > 0) then
      write(*,*) "lateral dischage EC module update"
      write(*,*) qplat(1)
      endif
    end do
    if (iresult /= DFM_NOERR) then
      WRITE(*,*) "Timestep error in D-Flow"
      WRITE(*,*) time_user
      WRITE(*,*) tstop_user
      WRITE(*,*) "TIME STEP, END STEP DO NOT MATCH"
      call mess(LEVEL_WARN, 'Error during computation time loop. Details follow:')
      call dfm_strerror(msgbuf, iresult)
      call warn_flush()
    end if
    
#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": leaving "//METHOD, ESMF_LOGMSG_INFO)
#endif

  end subroutine

  !-----------------------------------------------------------------------------
  ! At the end of number of iterations, stops the program.
  !-----------------------------------------------------------------------------
#undef METHOD
#define METHOD "NWM_HYC_Fin"

  subroutine NWM_HYC_Fin(rc)

    use unstruc_api

    ! ARGUMENTES
    integer, intent(out)        :: rc

    ! LOCAL VARIABLES
    integer                     :: stat

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": entered "//METHOD, ESMF_LOGMSG_INFO)
#endif

    rc = ESMF_SUCCESS

    !< dflowfm_kernel/src/dflowfm_io/writesomefinaloutput.F90
    call writesomefinaloutput()

    !< dflowfm_kernel/src/dflowfm_manager/unstruc_api.F90:354
    call flowfinalize()

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": leaving "//METHOD, ESMF_LOGMSG_INFO)
#endif

  end subroutine

  !-----------------------------------------------------------------------------
  ! Dictionary Utility
  !-----------------------------------------------------------------------------

#undef METHOD
#define METHOD "NWM_HYCFieldDictionaryAdd"

  subroutine NWM_HYCFieldDictionaryAdd(fd, rc)
    ! ARGUMENTS
    character(len=64), optional :: fd
    integer,intent(out)         :: rc

    ! LOCAL VARIABLES
    integer                    :: fIndex
    logical                    :: isPresent

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": entered "//METHOD, ESMF_LOGMSG_INFO)
#endif

    rc = ESMF_SUCCESS

    if (present(fd)) then
        ! read the field file and keep a local import/export field
        ! TO DO
    else
      do fIndex=1,size(NWM_HYCFieldList)
        isPresent = NUOPC_FieldDictionaryHasEntry( &
          trim(NWM_HYCFieldList(fIndex)%stdname), &
          rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return
 
        if (.not.isPresent) then
          call NUOPC_FieldDictionaryAddEntry( &
            trim(NWM_HYCFieldList(fIndex)%stdname), &
            trim(NWM_HYCFieldList(fIndex)%units), &
            rc=rc)
          if (ESMF_STDERRORCHECK(rc)) return
        endif
      enddo
    endif

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": leaving "//METHOD, ESMF_LOGMSG_INFO)
#endif

  end subroutine 



#undef METHOD
#define METHOD "locstream_lateral_discharge"

  subroutine locstream_lateral_discharge(vm, rc,locstream_lateral)
    use m_inquire_flowgeom
    use m_meteo
    use m_ec_module
    use m_wind
    ! Arguments
    !type(ESMF_Mesh), intent(in)             :: mesh
    integer, intent(out)                    :: rc
    type(ESMF_VM)                           :: vm

    ! Return value
    type(ESMF_LocStream), intent(inout)       :: locstream_lateral

    ! Local variables
    integer              :: gblElmCnt     ! total number of reaches elements
    integer              :: linkls_start  ! current pet start id (i.e reach fid)
    integer              :: linkls_end    ! current pet end id (i.e reach fid)
    integer              :: i, j, numlocations
    integer, allocatable :: arbSeqIndexList(:)
    type(ESMF_LocStream) :: locstream_init
    type(ESMF_DistGrid)  :: distgrid
    real, allocatable    :: pli_lat(:), pli_lon(:), pli_lat_all(:), pli_lon_all(:), pli_comp(:)
    real(ESMF_KIND_R8), allocatable :: lat(:), lon(:)
    integer(ESMF_KIND_I4), allocatable :: bc_ids(:), mask(:), link(:)
    integer :: esmf_comm, localPet, petCount
    type(tEcElementSet), pointer            :: elementSetPtr !< ElementSet corresponding to elementSetId
    type(tEcInstance),   pointer            :: instancePtr   !< intent(in)
    type(tEcItem), pointer                  :: targetItemPtr
    type(tEcConnection), pointer            :: connectionPtr
    type(tEcItem),       pointer            :: sourceItemPtr
    type(tEcFileReader), pointer            :: FileReaderPtr
    character(len=20) :: id_test
    character(len=256), allocatable :: ids_discharge(:), pli_files(:), ids_final(:)

    integer :: ii, ic, js, ncoord, minp, pli_count, pli_coord_count, pli_index, t1, t2, t3, HUC_size !< loop counter
    !character(len=maxMessageLen) :: line

#ifdef DEBUG
    character(ESMF_MAXSTR)  :: logMsg
    call ESMF_LogWrite(MODNAME//": entered "//METHOD, ESMF_LOGMSG_INFO)
#endif

    rc = ESMF_SUCCESS

    !-------------------------------------------------------------------
    ! Get parallel information. Here petCount is the total number of
    ! running PETs, and localPet is the number of this particular PET.
    !-------------------------------------------------------------------
    !call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, mpiCommunicator=esmf_comm, rc=rc)

    if(numlatsg > 0) then
       WRITE(*,*) "HUC ID"
       WRITE(*,*) lat_ids(1)
       WRITE(*,*) numlatsg
       WRITE(*,*) size(lat_ids)
    endif
    HUC_size = size(NWM_CommonID)
    !if(numlatsg > 0) then
    if(HUC_size > 0) then
    ! create local element list
        allocate(arbSeqIndexList(HUC_size))
        allocate(lat(HUC_size))
        allocate(lon(HUC_size))
        allocate(mask(HUC_size))
        numlocations = HUC_size
        do i = 1, numlocations
            !WRITE(*,*) "HUC ID"
            !write(*,*) lat_ids(1)
            arbSeqIndexList(i) = i
            lat(i) = NWM_CommonID(i)
            lon(i) = NWM_CommonID(i)

            !read(lat_ids(i),*) lat(i)
            !read(lat_ids(i),*) lon(i)
          
            !read(srcname(i),*) lat(i)
            !read(srcname(i),*) lon(i)
            !read(idbndu(i)(9:index(idbndu(i),'_')-1),*) link(i)
            !WRITE(*,*) "lateral lat id"
            !WRITE(*,*) lat(i)
            !WRITE(*,*) "geomXLat"
            !WRITE(*,*) lon(i)
            ! Data is available everywhere, so mask set to False
            mask(i) = 0
        end do
     ! PET does not contain a boundary condtion point within its sliced mesh
     ! grid, so we assign this PET a masked boundary condition point
     else
        allocate(arbSeqIndexList(1))
        allocate(lat(1))
        allocate(lon(1))
        allocate(mask(1))
        numlocations = 1
        do i = 1, 1
            arbSeqIndexList(i) = i
            !lat(i) = yu(i)
            !lon(i) = xu(i)
            lat(i) = 0.0
            lon(i) = 0.0
            ! Data is available everywhere, so mask set to False
            mask(i) = 1
        end do

      endif

    distgrid = ESMF_DistGridCreate(arbSeqIndexList=arbSeqIndexList, rc=rc)
    call ESMF_LogWrite("Initalizing locstream", ESMF_LOGMSG_INFO)
    !-------------------------------------------------------------------
    ! Create the LocStream:  Allocate space for the LocStream object,
    ! define the number and distribution of the locations.
    !-------------------------------------------------------------------
    locstream_lateral=ESMF_LocStreamCreate(name='lateral_flow_rate',   &
                                             !distgrid=distgrid, &
                                             localCount=numlocations, &
                                             !localCount=nbndu, &
                                             indexflag=ESMF_INDEX_DELOCAL, &
                                             coordSys=ESMF_COORDSYS_SPH_DEG, &
                                             !coordSys=ESMF_COORDSYS_CART, &
                                             rc=rc)

    !-------------------------------------------------------------------
    ! Add key data (internally allocating memory).
    !-------------------------------------------------------------------
    call ESMF_LocStreamAddKey(locstream_lateral,        &
                             keyName="ESMF:Lat",          &
                             farray=lat,                   &
                             datacopyflag=ESMF_DATACOPY_VALUE, &
                             keyUnits="Degrees",     &
                             keyLongName="Latitude", rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return

    call ESMF_LocStreamAddKey(locstream_lateral,        &
                             keyName="ESMF:Lon",          &
                             farray=lon,               &
                             datacopyflag=ESMF_DATACOPY_VALUE, &
                             keyUnits="Degrees",     &
                             keyLongName="Longitude", rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return

    call ESMF_LocStreamAddKey(locstream_lateral,        &
                             keyName="ESMF:Mask",          &
                             farray=mask,               &
                             datacopyflag=ESMF_DATACOPY_VALUE, &
                             keyLongName="ESMF Mask", rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return

    deallocate(arbSeqIndexList)

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": leaving "//METHOD, ESMF_LOGMSG_INFO)
#endif

    end subroutine locstream_lateral_discharge

#undef METHOD
#define METHOD "locstream_data"

  subroutine locstream_data(vm, rc,locstream_discharge, locstream_link)
    use m_inquire_flowgeom
    use m_meteo
    use m_ec_module
    ! Arguments
    !type(ESMF_Mesh), intent(in)             :: mesh
    integer, intent(out)                    :: rc
    type(ESMF_VM)                           :: vm

    ! Return value
    type(ESMF_LocStream), intent(inout)       :: locstream_discharge
    type(ESMF_LocStream), intent(inout)       :: locstream_link

    ! Local variables
    integer              :: gblElmCnt     ! total number of reaches elements
    integer              :: linkls_start  ! current pet start id (i.e reach fid)
    integer              :: linkls_end    ! current pet end id (i.e reach fid)
    integer              :: i, j, numlocations
    integer, allocatable :: arbSeqIndexList(:)
    type(ESMF_LocStream) :: locstream_init
    type(ESMF_DistGrid)  :: distgrid
    real, allocatable    :: pli_lat(:), pli_lon(:), pli_lat_all(:), pli_lon_all(:), pli_comp(:), test(:), pli_lat_final(:), pli_lon_final(:), pli_files_final(:)
    real(ESMF_KIND_R8), allocatable :: lat(:), lon(:)
    integer(ESMF_KIND_I4), allocatable :: bc_ids(:), mask(:), link(:)
    integer :: esmf_comm, localPet, petCount
    type(tEcElementSet), pointer            :: elementSetPtr !< ElementSet corresponding to elementSetId
    type(tEcInstance),   pointer            :: instancePtr   !< intent(in)
    type(tEcItem), pointer                  :: targetItemPtr
    type(tEcConnection), pointer            :: connectionPtr
    type(tEcItem),       pointer            :: sourceItemPtr
    type(tEcFileReader), pointer            :: FileReaderPtr
    character(len=20) :: id_test
    character(len=256), allocatable :: ids_discharge(:), pli_files(:), ids_final(:)
    integer :: ii, ic, js, ncoord, minp, pli_count, pli_coord_count, pli_index, t1, t2, t3 !< loop counter
    integer :: loop1, loop2
    logical :: duplicate
    !character(len=maxMessageLen) :: line

#ifdef DEBUG
    character(ESMF_MAXSTR)  :: logMsg
    call ESMF_LogWrite(MODNAME//": entered "//METHOD, ESMF_LOGMSG_INFO)
#endif

    rc = ESMF_SUCCESS

    !-------------------------------------------------------------------
    ! Get parallel information. Here petCount is the total number of
    ! running PETs, and localPet is the number of this particular PET.
    !-------------------------------------------------------------------
    !call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, mpiCommunicator=esmf_comm, rc=rc)

    ! Allocate array for Cap to use within inital locstream data
    ALLOCATE(DFLOW_zbndq_t0(nbndu*2))
    ALLOCATE(DFLOW_zbndq_t1(nbndu*2))
    ALLOCATE(DFLOW_zbndq_temp_interp(nbndu*2))

    write(*,*) "nbndu"
    write(*,*) nbndu
    if(nbndu > 0) then
    ! create local element list
        allocate(arbSeqIndexList(nbndu))
        !allocate(lat(nbndu))
        !allocate(lon(nbndu))
        !allocate(mask(nbndu))
        !numlocations = nbndu
        do i = 1, nbndu
            arbSeqIndexList(i) = i
            !lat(i) = test(i)
            !lon(i) = test(i)
            !lat(i) = ybndu(i)
            !lon(i) = xbndu(i)
            write(*,*) "idbndu(i)"
            write(*,*) idbndu(i)
            write(*,*) "output from read statement"
            write(*,*) idbndu(i)(1:index(idbndu(i),'_')-1)
            !read(idbndu(i)(1:index(idbndu(i),'_')-1),*) lat(i)
            !read(idbndu(i)(1:index(idbndu(i),'_')-1),*) lon(i)

            !write(*,*) "idbndu slice"
            !write(*,*) idbndu(i)(5:index(idbndu(i),'0000')-1)
            !read(idbndu(i)(5:index(idbndu(i),'0000')-1),*) lat(i)
            !read(idbndu(i)(5:index(idbndu(i),'0000')-1),*) lon(i)

            !write(*,*) "lat final"
            !write(*,*) lat(i)
            ! Data is available everywhere, so mask set to False
            !mask(i) = 0
        end do
        allocate(lat(nbndu*2))
        allocate(lon(nbndu*2))
        allocate(mask(nbndu*2))
        numlocations = nbndu*2
        j = 1
        do i = 1, nbndu
        read(idbndu_left(i)(1:index(idbndu_left(i),'_')-1),*) lat(j)
        read(idbndu_right(i)(1:index(idbndu_right(i),'_')-1),*) lat(j+1)
        read(idbndu_left(i)(1:index(idbndu_left(i),'_')-1),*) lon(j)
        read(idbndu_right(i)(1:index(idbndu_right(i),'_')-1),*) lon(j+1)
        mask(j) = 0
        mask(j+1) = 0
        j = j + 2
        end do

     ! PET does not contain a boundary condtion point within its sliced mesh
     ! grid, so we assign this PET a masked boundary condition point
     else
        allocate(arbSeqIndexList(1))
        allocate(lat(1))
        allocate(lon(1))
        allocate(mask(1))
        numlocations = 1
        do i = 1, 1
            arbSeqIndexList(i) = i
            !lat(i) = yu(i)
            !lon(i) = xu(i)
            lat(i) = 0.0
            lon(i) = 0.0
            ! Data is available everywhere, so mask set to False
            mask(i) = 1
        end do

      endif

    distgrid = ESMF_DistGridCreate(arbSeqIndexList=arbSeqIndexList, rc=rc)
    call ESMF_LogWrite("Initalizing locstream", ESMF_LOGMSG_INFO)
    !-------------------------------------------------------------------
    ! Create the LocStream:  Allocate space for the LocStream object,
    ! define the number and distribution of the locations.
    !-------------------------------------------------------------------
    locstream_discharge=ESMF_LocStreamCreate(name='flow_rate',   &
                                             !distgrid=distgrid, &
                                             localCount=numlocations, &
                                             !localCount=nbndu, &
                                             indexflag=ESMF_INDEX_DELOCAL, &
                                             coordSys=ESMF_COORDSYS_SPH_DEG, &
                                             !coordSys=ESMF_COORDSYS_CART, &
                                             rc=rc)

    !-------------------------------------------------------------------
    ! Add key data (internally allocating memory).
    !-------------------------------------------------------------------
    call ESMF_LocStreamAddKey(locstream_discharge,        &
                             keyName="ESMF:Lat",          &
                             farray=lat,                   &
                             datacopyflag=ESMF_DATACOPY_VALUE, &                        
                             keyLongName="Latitude", rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return

    call ESMF_LocStreamAddKey(locstream_discharge,        &
                             keyName="ESMF:Lon",          &
                             farray=lat,               &
                             datacopyflag=ESMF_DATACOPY_VALUE, &
                             keyLongName="Longitude", rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return

    call ESMF_LocStreamAddKey(locstream_discharge,        &
                             keyName="ESMF:Mask",          &
                             farray=mask,               &
                             datacopyflag=ESMF_DATACOPY_VALUE, &
                             keyLongName="ESMF Mask", rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return

    locstream_link=ESMF_LocStreamCreate(name='link',   &
                                             !distgrid=distgrid, &
                                             localCount=numlocations, &
                                             !localCount=nbndu, &
                                             indexflag=ESMF_INDEX_DELOCAL, &
                                             coordSys=ESMF_COORDSYS_SPH_DEG, &
                                             !coordSys=ESMF_COORDSYS_CART, &
                                             rc=rc)

    !-------------------------------------------------------------------
    ! Add key data (internally allocating memory).
    !-------------------------------------------------------------------
    call ESMF_LocStreamAddKey(locstream_link,        &
                             keyName="ESMF:Lat",          &
                             farray=lat,                   &
                             datacopyflag=ESMF_DATACOPY_VALUE, &
                             keyLongName="Latitude", rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return

    call ESMF_LocStreamAddKey(locstream_link,        &
                             keyName="ESMF:Lon",          &
                             farray=lat,               &
                             datacopyflag=ESMF_DATACOPY_VALUE, &
                             keyLongName="Longitude", rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return

    call ESMF_LocStreamAddKey(locstream_link,        &
                             keyName="ESMF:Mask",          &
                             farray=mask,               &
                             datacopyflag=ESMF_DATACOPY_VALUE, &
                             keyLongName="ESMF Mask", rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return

    deallocate(arbSeqIndexList)

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": leaving "//METHOD, ESMF_LOGMSG_INFO)
#endif

    end subroutine locstream_data

#undef METHOD
#define METHOD "locstream_discharge"

  function locstream_discharge(vm, rc)
    use m_inquire_flowgeom
    use m_meteo
    ! Return value
    type(ESMF_LocStream)       :: locstream_discharge

    ! Arguments
    !type(ESMF_Mesh), intent(in)             :: mesh
    integer, intent(out)                    :: rc
    type(ESMF_VM)                           :: vm

    ! Local variables
    integer              :: gblElmCnt     ! total number of reaches elements
    integer              :: linkls_start  ! current pet start id (i.e reach fid)
    integer              :: linkls_end    ! current pet end id (i.e reach fid)
    integer              :: locElmCnt     ! number of points (i.e. reaches) on each pet
    integer              :: i, j, numlocations
    integer, allocatable :: arbSeqIndexList(:)
    type(ESMF_LocStream) :: locstream_init
    type(ESMF_DistGrid)  :: distgrid
    real(ESMF_KIND_R8), allocatable    :: lat(:),lon(:)
    real, allocatable :: pli_lat(:), pli_lon(:), pli_lat_all(:), pli_lon_all(:), pli_comp(:)
    integer(ESMF_KIND_I4), allocatable :: bc_ids(:), mask(:), link(:)
    integer :: esmf_comm, localPet, petCount
    type(tEcElementSet), pointer            :: elementSetPtr !< ElementSet corresponding to elementSetId
    type(tEcInstance),   pointer            :: instancePtr   !< intent(in)
    type(tEcItem), pointer                  :: targetItemPtr
    type(tEcConnection), pointer            :: connectionPtr
    type(tEcItem),       pointer            :: sourceItemPtr
    type(tEcFileReader), pointer            :: FileReaderPtr
    character(len=20) :: id_test
    character(len=256), allocatable :: ids_discharge(:), pli_files(:), ids_final(:)


    integer :: ii, ic, js, ncoord, minp, pli_count, pli_coord_count, pli_index, t1, t2 !< loop counter
    character(len=maxMessageLen) :: line

#ifdef DEBUG
    character(ESMF_MAXSTR)  :: logMsg
    call ESMF_LogWrite(MODNAME//": entered "//METHOD, ESMF_LOGMSG_INFO)
#endif

    rc = ESMF_SUCCESS

    !-------------------------------------------------------------------
    ! Get parallel information. Here petCount is the total number of
    ! running PETs, and localPet is the number of this particular PET.
    !-------------------------------------------------------------------
    !call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, mpiCommunicator=esmf_comm, rc=rc)

    write(*,*) "nbndu"
    write(*,*) nbndu
    if(nbndu > 0) then
       ! create local element list
       allocate(arbSeqIndexList(nbndu))
       allocate(lat(nbndu))
       allocate(lon(nbndu))
       allocate(mask(nbndu))
       numlocations = nbndu
       do i = 1, nbndu
          arbSeqIndexList(i) = i
          !lat(i) = test(i)
          !lon(i) = test(i)
          !lat(i) = ybndu(i)
          !lon(i) = xbndu(i)
          write(*,*) "idbndu(i)"
          write(*,*) idbndu(i)
          write(*,*) "output from read statement"
          write(*,*) idbndu(i)(1:index(idbndu(i),'_')-1)
          read(idbndu(i)(1:index(idbndu(i),'_')-1),*) lat(i)
          read(idbndu(i)(1:index(idbndu(i),'_')-1),*) lon(i)

          !write(*,*) "idbndu slice"
          !write(*,*) idbndu(i)(5:index(idbndu(i),'0000')-1)
          !read(idbndu(i)(5:index(idbndu(i),'0000')-1),*) lat(i)
          !read(idbndu(i)(5:index(idbndu(i),'0000')-1),*) lon(i)

          write(*,*) "lat final"
          write(*,*) lat(i)
          ! Data is available everywhere, so mask set to False
          mask(i) = 0
       end do
    else
       allocate(arbSeqIndexList(1))
       allocate(lat(1))
       allocate(lon(1))
       allocate(mask(1))
       numlocations = 1
       do i = 1, 1
          arbSeqIndexList(i) = i
          lat(i) = 0.0
          lon(i) = 0.0
          ! Data is available everywhere, so mask set to False
          mask(i) = 1
       end do

      endif

    ! create DistGrid
    distgrid = ESMF_DistGridCreate(arbSeqIndexList=arbSeqIndexList, rc=rc)
    call ESMF_LogWrite("Initalizing locstream", ESMF_LOGMSG_INFO) 
    !-------------------------------------------------------------------
    ! Create the LocStream:  Allocate space for the LocStream object, 
    ! define the number and distribution of the locations. 
    !-------------------------------------------------------------------
    locstream_discharge=ESMF_LocStreamCreate(name='flow_rate',   &
                                             !distgrid=distgrid, &
                                             localCount=numlocations, &
                                             !localCount=nbndu, &
                                             indexflag=ESMF_INDEX_DELOCAL, &
                                             coordSys=ESMF_COORDSYS_SPH_DEG, &
                                             rc=rc)


    call ESMF_LogWrite("Initalizing locstream keys", ESMF_LOGMSG_INFO)
    !-------------------------------------------------------------------
    ! Add key data (internally allocating memory).
    !-------------------------------------------------------------------
    call ESMF_LocStreamAddKey(locstream_discharge,        &
                             keyName="ESMF:Lat",          &
                             farray=lat,                   &
                             datacopyflag=ESMF_DATACOPY_VALUE, &
                             keyLongName="Latitude", rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return

    call ESMF_LocStreamAddKey(locstream_discharge,        &
                             keyName="ESMF:Lon",          &
                             farray=lon,               &
                             datacopyflag=ESMF_DATACOPY_VALUE, &
                             keyLongName="Longitude", rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return

    call ESMF_LocStreamAddKey(locstream_discharge,        &
                             keyName="ESMF:Mask",          &
                             farray=mask,               &
                             datacopyflag=ESMF_DATACOPY_VALUE, &
                             keyLongName="ESMF Mask", rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return

    deallocate(arbSeqIndexList)

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": leaving "//METHOD, ESMF_LOGMSG_INFO)
#endif


    !locstream_discharge=ESMF_LocStreamCreate(locstream_init, &
    !              background=mesh, rc=rc)
    end function




#undef METHOD
#define METHOD "locstream_lat"

  function locstream_lat(vm, rc)
    use m_flowexternalforcings, only: nbndz,nbndu, zbndz, zbndq, xbndu, ybndu, xbndz, ybndz
    ! Return value
    type(ESMF_LocStream)       :: locstream_lat

    ! Arguments
    !type(ESMF_Mesh), intent(in)             :: mesh
    integer, intent(out)                    :: rc
    type(ESMF_VM)                           :: vm

    ! Local variables
    integer              :: gblElmCnt     ! total number of reaches elements
    integer              :: linkls_start  ! current pet start id (i.e reach fid)
    integer              :: linkls_end    ! current pet end id (i.e reach fid)
    integer              :: locElmCnt     ! number of points (i.e. reaches) on
    integer              :: i, j, numlocations
    integer, allocatable :: arbSeqIndexList(:)
    type(ESMF_LocStream) :: locstream_init
    type(ESMF_DistGrid)  :: distgrid
    real(ESMF_KIND_R8), allocatable    :: lat(:),lon(:)
    integer(ESMF_KIND_I4), allocatable :: bc_ids(:), mask(:)
    integer :: esmf_comm, localPet, petCount


#ifdef DEBUG
    character(ESMF_MAXSTR)  :: logMsg
    call ESMF_LogWrite(MODNAME//": entered "//METHOD, ESMF_LOGMSG_INFO)
#endif

    rc = ESMF_SUCCESS

    !-------------------------------------------------------------------
    ! Get parallel information. Here petCount is the total number of
    ! running PETs, and localPet is the number of this particular PET.
    !-------------------------------------------------------------------
    !call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, mpiCommunicator=esmf_comm, rc=rc)

    if(nbndu > 0) then
    ! create local element list
        allocate(arbSeqIndexList(nbndu))
        allocate(lat(nbndu))
        allocate(lon(nbndu))
        allocate(mask(nbndu))
        numlocations = nbndu
        do i = 1, nbndu
            arbSeqIndexList(i) = i
            !lat(i) = yu(i)
            !lon(i) = xu(i)
            lat(i) = ybndu(i)
            lon(i) = xbndu(i)
            ! Data is available everywhere, so mask set to False
            mask(i) = 0
        end do
     ! PET does not contain a boundary condtion point within its sliced mesh
     ! grid, so we assign this PET a masked boundary condition point
     else
        allocate(arbSeqIndexList(1))
        allocate(lat(1))
        allocate(lon(1))
        allocate(mask(1))
        numlocations = 1
        do i = 1, 1
            arbSeqIndexList(i) = i
            !lat(i) = yu(i)
            !lon(i) = xu(i)
            lat(i) = 0.0
            lon(i) = 0.0
            ! Data is available everywhere, so mask set to False
            mask(i) = 1
        end do

      endif

    !WRITE(*,*) "yk"
    !WRITE(*,*) yk
    !write(*,*) size(q1)
    !write(*,*) size(xz)
    !write(*,*) size(yz)
    ! create DistGrid
    distgrid = ESMF_DistGridCreate(arbSeqIndexList=arbSeqIndexList, rc=rc)
    call ESMF_LogWrite("Initalizing locstream", ESMF_LOGMSG_INFO)
    !-------------------------------------------------------------------
    ! Create the LocStream:  Allocate space for the LocStream object,
    ! define the number and distribution of the locations.
    !-------------------------------------------------------------------
    locstream_lat=ESMF_LocStreamCreate(name='lat',   &
                                             !distgrid=distgrid, &
                                             localCount=numlocations, &
                                             !localCount=nbndu, &
                                             indexflag=ESMF_INDEX_DELOCAL, &
                                             coordSys=ESMF_COORDSYS_SPH_DEG, &
                                             rc=rc)


    call ESMF_LogWrite("Initalizing locstream keys", ESMF_LOGMSG_INFO)
    !-------------------------------------------------------------------
    ! Add key data (internally allocating memory).
    !-------------------------------------------------------------------
    call ESMF_LocStreamAddKey(locstream_lat,        &
                             keyName="ESMF:Lat",          &
                             farray=lat,                   &
                             datacopyflag=ESMF_DATACOPY_VALUE, &
                             keyUnits="Degrees",     &
                             keyLongName="Latitude", rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return

    call ESMF_LocStreamAddKey(locstream_lat,        &
                             keyName="ESMF:Lon",          &
                             farray=lon,               &
                             datacopyflag=ESMF_DATACOPY_VALUE, &
                             keyUnits="Degrees",     &
                             keyLongName="Longitude", rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return

    call ESMF_LocStreamAddKey(locstream_lat,        &
                             keyName="ESMF:Mask",          &
                             farray=mask,               &
                             datacopyflag=ESMF_DATACOPY_VALUE, &
                             keyLongName="ESMF Mask", rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return

    deallocate(arbSeqIndexList)

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": leaving "//METHOD, ESMF_LOGMSG_INFO)
#endif


    !locstream_discharge=ESMF_LocStreamCreate(locstream_init, &
    !              background=mesh, rc=rc)
    end function

#undef METHOD
#define METHOD "locstream_lon"

  function locstream_lon(vm, rc)
    use m_flowexternalforcings, only: nbndz,nbndu, zbndz, zbndq, xbndu, ybndu, xbndz, ybndz
    ! Return value
    type(ESMF_LocStream)       :: locstream_lon

    ! Arguments
    !type(ESMF_Mesh), intent(in)             :: mesh
    integer, intent(out)                    :: rc
    type(ESMF_VM)                           :: vm

    ! Local variables
    integer              :: gblElmCnt     ! total number of reaches elements
    integer              :: linkls_start  ! current pet start id (i.e reach fid)
    integer              :: linkls_end    ! current pet end id (i.e reach fid)
    integer              :: locElmCnt     ! number of points (i.e. reaches) on
    integer              :: i, j, numlocations
    integer, allocatable :: arbSeqIndexList(:)
    type(ESMF_LocStream) :: locstream_init
    type(ESMF_DistGrid)  :: distgrid
    real(ESMF_KIND_R8), allocatable    :: lat(:),lon(:)
    integer(ESMF_KIND_I4), allocatable :: bc_ids(:), mask(:)
    integer :: esmf_comm, localPet, petCount


#ifdef DEBUG
    character(ESMF_MAXSTR)  :: logMsg
    call ESMF_LogWrite(MODNAME//": entered "//METHOD, ESMF_LOGMSG_INFO)
#endif

    rc = ESMF_SUCCESS

    !-------------------------------------------------------------------
    ! Get parallel information. Here petCount is the total number of
    ! running PETs, and localPet is the number of this particular PET.
    !-------------------------------------------------------------------
    !call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, mpiCommunicator=esmf_comm, rc=rc)

    if(nbndu > 0) then
    ! create local element list
        allocate(arbSeqIndexList(nbndu))
        allocate(lat(nbndu))
        allocate(lon(nbndu))
        allocate(mask(nbndu))
        numlocations = nbndu
        do i = 1, nbndu
            arbSeqIndexList(i) = i
            !lat(i) = yu(i)
            !lon(i) = xu(i)
            lat(i) = ybndu(i)
            lon(i) = xbndu(i)
            ! Data is available everywhere, so mask set to False
            mask(i) = 0
        end do
     ! PET does not contain a boundary condtion point within its sliced mesh
     ! grid, so we assign this PET a masked boundary condition point
     else
        allocate(arbSeqIndexList(1))
        allocate(lat(1))
        allocate(lon(1))
        allocate(mask(1))
        numlocations = 1
        do i = 1, 1
            arbSeqIndexList(i) = i
            !lat(i) = yu(i)
            !lon(i) = xu(i)
            lat(i) = 0.0
            lon(i) = 0.0
            ! Data is available everywhere, so mask set to False
            mask(i) = 1
        end do

      endif

    !WRITE(*,*) "yk"
    !WRITE(*,*) yk
    !write(*,*) size(q1)
    !write(*,*) size(xz)
    !write(*,*) size(yz)
    ! create DistGrid
    distgrid = ESMF_DistGridCreate(arbSeqIndexList=arbSeqIndexList, rc=rc)
    call ESMF_LogWrite("Initalizing locstream", ESMF_LOGMSG_INFO)
    !-------------------------------------------------------------------
    ! Create the LocStream:  Allocate space for the LocStream object,
    ! define the number and distribution of the locations.
    !-------------------------------------------------------------------
    locstream_lon=ESMF_LocStreamCreate(name='lon',   &
                                             !distgrid=distgrid, &
                                             localCount=numlocations, &
                                             !localCount=nbndu, &
                                             indexflag=ESMF_INDEX_DELOCAL, &
                                             coordSys=ESMF_COORDSYS_SPH_DEG, &
                                             rc=rc)


    call ESMF_LogWrite("Initalizing locstream keys", ESMF_LOGMSG_INFO)
    !-------------------------------------------------------------------
    ! Add key data (internally allocating memory).
    !-------------------------------------------------------------------
    call ESMF_LocStreamAddKey(locstream_lon,        &
                             keyName="ESMF:Lat",          &
                             farray=lat,                   &
                             datacopyflag=ESMF_DATACOPY_VALUE, &
                             keyUnits="Degrees",     &
                             keyLongName="Latitude", rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return

    call ESMF_LocStreamAddKey(locstream_lon,        &
                             keyName="ESMF:Lon",          &
                             farray=lon,               &
                             datacopyflag=ESMF_DATACOPY_VALUE, &
                             keyUnits="Degrees",     &
                             keyLongName="Longitude", rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return

    call ESMF_LocStreamAddKey(locstream_lon,        &
                             keyName="ESMF:Mask",          &
                             farray=mask,               &
                             datacopyflag=ESMF_DATACOPY_VALUE, &
                             keyLongName="ESMF Mask", rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return

    deallocate(arbSeqIndexList)

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": leaving "//METHOD, ESMF_LOGMSG_INFO)
#endif


    !locstream_discharge=ESMF_LocStreamCreate(locstream_init, &
    !              background=mesh, rc=rc)
    end function


#undef METHOD
#define METHOD "locstream_waterlevel"

  function locstream_waterlevel(localPet, rc)
    use m_inquire_flowgeom
    use m_meteo
    use m_ec_module
    ! Return value
    type(ESMF_LocStream)       :: locstream_waterlevel

    ! Arguments
    !type(ESMF_Mesh), intent(in)             :: mesh
    integer, intent(in)                     :: localPet
    integer, intent(out)                    :: rc
    !type(ESMF_VM)                           :: vm

    ! Local variables
    integer              :: gblElmCnt     ! total number of reaches elements
    integer              :: linkls_start  ! current pet start id (i.e reach fid)
    integer              :: linkls_end    ! current pet end id (i.e reach fid)
    integer              :: locElmCnt     ! number of points (i.e. reaches) on
    integer              :: a, i, j, numlocations
    integer, allocatable :: arbSeqIndexList(:)
    type(ESMF_LocStream) :: locstream_init
    type(ESMF_DistGrid)  :: distgrid
    real(ESMF_KIND_R8), allocatable    :: lat(:),lon(:), lat_final(:), lon_final(:), x(:), y(:), z(:)
    real(ESMF_KIND_R8) :: theta, phi
    real, allocatable :: pli_lat(:), pli_lon(:), pli_lat_all(:), pli_lon_all(:), pli_comp(:)
    integer(ESMF_KIND_I4), allocatable :: bc_ids(:), mask(:)
    !integer :: esmf_comm, localPet, petCount
    type(tEcElementSet), pointer            :: elementSetPtr !< ElementSet corresponding to elementSetId
    type(tEcInstance),   pointer            :: instancePtr   !< intent(in)
    type(tEcItem), pointer                  :: targetItemPtr
    type(tEcConnection), pointer            :: connectionPtr
    type(tEcItem),       pointer            :: sourceItemPtr
    type(tEcFileReader), pointer            :: FileReaderPtr
    character(len=20) :: id_test
    character(len=256), allocatable :: ids_waterlevel(:), pli_files(:), ids_final(:)
    integer :: ii, ic, js, ncoord, minp, pli_count, pli_coord_count, pli_index, t1, t2 !< loop counter
    character(len=maxMessageLen) :: line

#ifdef DEBUG
    character(ESMF_MAXSTR)  :: logMsg
    call ESMF_LogWrite(MODNAME//": entered "//METHOD, ESMF_LOGMSG_INFO)
#endif

    rc = ESMF_SUCCESS

    !-------------------------------------------------------------------
    ! Get parallel information. Here petCount is the total number of
    ! running PETs, and localPet is the number of this particular PET.
    !-------------------------------------------------------------------
    !call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, mpiCommunicator=esmf_comm, rc=rc)

    ! Allocate array for Cap to use within inital locstream data
    ALLOCATE(DFLOW_zbndz_t0(nbndz*2))
    ALLOCATE(DFLOW_zbndz_t1(nbndz*2))
    ALLOCATE(DFLOW_zbndz_temp_interp(nbndz*2))

    WRITE(*,*) "nbndz size"
    WRITE(*,*) nbndz
    if(nbndz > 0) then
    ! create local element list
        allocate(arbSeqIndexList(nbndz))
        !allocate(lat(nbndz))
        !allocate(lon(nbndz))
        !allocate(mask(nbndz))
        allocate(x(nbndz))
        allocate(y(nbndz))
        allocate(z(nbndz))
        !numlocations = nbndz
        do i = 1, nbndz
            arbSeqIndexList(i) = i
            !lat(i) = ybndz(i)
            !lon(i) = xbndz(i)
            !lat(i) = wl_lat(i)
            !lon(i) = wl_lon(i)
            !WRITE(*,*) "waterlevel coordinates"
            !WRITE(*,*) lat(i)
            !WRITE(*,*) lon(i)
            x(i) = 6378206.4 * SIN(ybndz(i) * 2 * 3.14159265359 / 360) * COS(xbndz(i) * 2 * 3.14159265359 / 360)
            y(i) = 6378206.4 * SIN(ybndz(i) * 2 * 3.14159265359 / 360) * SIN(xbndz(i) * 2 * 3.14159265359 / 360)
            !z(i) = 6367 * SIN(ybndz(i) * 2 * 3.14159265359 / 360)
            !theta = (3.14159265359/2) - (ybndz(i) * 2 * 3.14159265359 / 360)
            !phi = (xbndz(i) * 2 * 3.14159265359 / 360)
            !x(i) = 6371 * SIN(theta) * COS(phi)
            !y(i) = 6371 * SIN(theta) * SIN(phi)
            !z(i) = 6371 * COS(theta)

            ! Data is available everywhere, so mask set to False
            !mask(i) = 0
        end do

        i = 1
        allocate(lat(nbndz*2))
        allocate(lon(nbndz*2))
        allocate(mask(nbndz*2))
        numlocations = nbndz*2
        do j = 1, nbndz
        lat(i) = wl_lat_left(j)
        lon(i) = wl_lon_left(j)
        lat(i+1) = wl_lat_right(j)
        lon(i+1) = wl_lon_right(j)
        mask(i) = 0
        mask(i+1) = 0
        i = i + 2
        end do
        
     
     ! PET does not contain a boundary condtion point within its sliced mesh
     ! grid, so we assign this PET a masked boundary condition point
     else
        allocate(arbSeqIndexList(1))
        allocate(lat(1))
        allocate(lon(1))
        allocate(mask(1))
        allocate(x(1))
        allocate(y(1))
        allocate(z(1))
        numlocations = 1
        do i = 1, 1
            arbSeqIndexList(i) = i
            lat(i) = 0.0
            lon(i) = 0.0
            mask(i) = 1
            x(i) = 0.0
            y(i) = 0.0
            z(i) = 0.0
        end do

      endif
    ! create DistGrid
    distgrid = ESMF_DistGridCreate(arbSeqIndexList=arbSeqIndexList, rc=rc)
    call ESMF_LogWrite("Initalizing locstream", ESMF_LOGMSG_INFO)
    !-------------------------------------------------------------------
    ! Create the LocStream:  Allocate space for the LocStream object,
    ! define the number and distribution of the locations.
    !-------------------------------------------------------------------
    locstream_waterlevel=ESMF_LocStreamCreate(name='zeta_dflow',   &
                                             !distgrid=distgrid, &
                                             localCount=numlocations, &
                                             indexflag=ESMF_INDEX_DELOCAL, &
                                             coordSys=ESMF_COORDSYS_SPH_DEG, &
                                             !coordSys=ESMF_COORDSYS_CART, &
                                             rc=rc)


    call ESMF_LogWrite("Initalizing locstream keys", ESMF_LOGMSG_INFO)
    !-------------------------------------------------------------------
    ! Add key data (internally allocating memory).
    !-------------------------------------------------------------------
    !call ESMF_LocStreamAddKey(locstream_waterlevel,        &
    !                         keyName="ESMF:Y",          &
    !                         farray=y,                   &
    !                         datacopyflag=ESMF_DATACOPY_VALUE, rc=rc)
    !if (ESMF_STDERRORCHECK(rc)) return

    !call ESMF_LocStreamAddKey(locstream_waterlevel,        &
    !                         keyName="ESMF:X",          &
    !                         farray=x,               &
    !                         datacopyflag=ESMF_DATACOPY_VALUE, rc=rc)
    !if (ESMF_STDERRORCHECK(rc)) return

    !call ESMF_LocStreamAddKey(locstream_waterlevel,        &
    !                         keyName="ESMF:Z",          &
    !                         farray=z,               &
    !                         datacopyflag=ESMF_DATACOPY_VALUE, rc=rc)
    !if (ESMF_STDERRORCHECK(rc)) return

    call ESMF_LocStreamAddKey(locstream_waterlevel,        &
                             keyName="ESMF:Lat",          &
                             farray=lat,                   &
                             datacopyflag=ESMF_DATACOPY_VALUE,rc=rc)
     if (ESMF_STDERRORCHECK(rc)) return

    call ESMF_LocStreamAddKey(locstream_waterlevel,        &
                             keyName="ESMF:Lon",          &
                             farray=lon,               &
                             datacopyflag=ESMF_DATACOPY_VALUE,rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return

    call ESMF_LocStreamAddKey(locstream_waterlevel,        &
                             keyName="ESMF:Mask",          &
                             farray=mask,               &
                             datacopyflag=ESMF_DATACOPY_VALUE, &
                             keyLongName="ESMF Mask", rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return

    deallocate(arbSeqIndexList)

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": leaving "//METHOD, ESMF_LOGMSG_INFO)
#endif


    !locstream_discharge=ESMF_LocStreamCreate(locstream_init, &
    !              background=mesh, rc=rc)
    end function

#undef METHOD
#define METHOD "locstream_velx"

  function locstream_velx(vm, rc)
    use m_ec_module
    use m_inquire_flowgeom
    use m_meteo
    ! Return value
    type(ESMF_LocStream)       :: locstream_velx

    ! Arguments
    !type(ESMF_Mesh), intent(in)             :: mesh
    integer, intent(out)                    :: rc
    type(ESMF_VM)                           :: vm

    ! Local variables
    integer              :: gblElmCnt     ! total number of reaches elements
    integer              :: linkls_start  ! current pet start id (i.e reach fid)
    integer              :: linkls_end    ! current pet end id (i.e reach fid)
    integer              :: locElmCnt     ! number of points (i.e. reaches) on
    integer              :: a, i, j, numlocations
    integer, allocatable :: arbSeqIndexList(:)
    type(ESMF_LocStream) :: locstream_init
    type(ESMF_DistGrid)  :: distgrid
    real(ESMF_KIND_R8), allocatable    :: lat(:),lon(:), lat_final(:), lon_final(:)
    real, allocatable :: pli_lat(:), pli_lon(:), pli_lat_all(:), pli_lon_all(:), pli_comp(:)
    integer(ESMF_KIND_I4), allocatable :: bc_ids(:), mask(:)
    !integer :: esmf_comm, localPet, petCount
    type(tEcElementSet), pointer            :: elementSetPtr !< ElementSet corresponding to elementSetId
    type(tEcInstance),   pointer            :: instancePtr   !< intent(in)
    type(tEcItem), pointer                  :: targetItemPtr
    type(tEcConnection), pointer            :: connectionPtr
    type(tEcItem),       pointer            :: sourceItemPtr
    type(tEcFileReader), pointer            :: FileReaderPtr
    character(len=20) :: id_test
    character(len=256), allocatable :: ids_waterlevel(:), pli_files(:), ids_final(:)
    integer :: ii, ic, js, ncoord, minp, pli_count, pli_coord_count, pli_index, t1, t2 !< loop counter
    integer :: vbc_size
    character(len=maxMessageLen) :: line

#ifdef DEBUG
    character(ESMF_MAXSTR)  :: logMsg
    call ESMF_LogWrite(MODNAME//": entered "//METHOD, ESMF_LOGMSG_INFO)
#endif

    rc = ESMF_SUCCESS
    !-------------------------------------------------------------------
    ! Get parallel information. Here petCount is the total number of
    ! running PETs, and localPet is the number of this particular PET.
    !-------------------------------------------------------------------
    !call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, mpiCommunicator=esmf_comm, rc=rc)
    
    ! Allocate arrays to be usef for D-Flow interpolation methods
    ALLOCATE(DFLOW_zbndt_t0(nbndt*2))
    ALLOCATE(DFLOW_zbndt_t1(nbndt*2))
    ALLOCATE(DFLOW_zbndn_t0(nbndn*2))
    ALLOCATE(DFLOW_zbndn_t1(nbndn*2))

    ALLOCATE(DFLOW_zbndt_temp_interp(nbndt*2))
    ALLOCATE(DFLOW_zbndn_temp_interp(nbndn*2))

   
 
    vbc_size = size(DFLOW_VBC_lat)
    WRITE(*,*) "current velocity x dimension"
    WRITE(*,*) vbc_size
    if(vbc_size > 0) then
       ! create local element list
       allocate(arbSeqIndexList(vbc_size))
       allocate(lat(vbc_size*2))
       allocate(lon(vbc_size*2))
       allocate(mask(vbc_size*2))
       numlocations = vbc_size*2
       j = 1
       do i = 1, vbc_size
          arbSeqIndexList(i) = i
          lat(j) = DFLOW_VBC_lat_left(i)
          lon(j) = DFLOW_VBC_lon_left(i)
          lat(j+1) = DFLOW_VBC_lat_right(i)
          lon(j+1) = DFLOW_VBC_lon_right(i)
          ! Data is available everywhere, so mask set to False
          mask(j) = 0
          mask(j+1) = 0
          j = j + 2
       end do
     ! PET does not contain a boundary condtion point within its sliced mesh
     ! grid, so we assign this PET a masked boundary condition point
     else
        allocate(arbSeqIndexList(1))
        allocate(lat(1))
        allocate(lon(1))
        allocate(mask(1))
        numlocations = 1
        do i = 1, 1
            arbSeqIndexList(i) = i
            lat(i) = 0.0
            lon(i) = 0.0
            mask(i) = 1
        end do
      endif

    call ESMF_LogWrite("Initalizing locstream", ESMF_LOGMSG_INFO)
    !-------------------------------------------------------------------
    ! Create the LocStream:  Allocate space for the LocStream object,
    ! define the number and distribution of the locations.
    !-------------------------------------------------------------------
    locstream_velx=ESMF_LocStreamCreate(name='velx_dflow',   &
                                             !distgrid=distgrid, &
                                             localCount=numlocations, &
                                             indexflag=ESMF_INDEX_DELOCAL, &
                                             coordSys=ESMF_COORDSYS_SPH_DEG, &
                                             rc=rc)


    call ESMF_LogWrite("Initalizing locstream keys", ESMF_LOGMSG_INFO)
    !-------------------------------------------------------------------
    ! Add key data (internally allocating memory).
    !-------------------------------------------------------------------
    call ESMF_LocStreamAddKey(locstream_velx,        &
                             keyName="ESMF:Lat",          &
                             farray=lat,                   &
                             datacopyflag=ESMF_DATACOPY_VALUE, &
                             keyUnits="Degrees",     &
                             keyLongName="Latitude", rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return

    call ESMF_LocStreamAddKey(locstream_velx,        &
                             keyName="ESMF:Lon",          &
                             farray=lon,               &
                             datacopyflag=ESMF_DATACOPY_VALUE, &
                             keyUnits="Degrees",     &
                             keyLongName="Longitude", rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return

    call ESMF_LocStreamAddKey(locstream_velx,        &
                             keyName="ESMF:Mask",          &
                             farray=mask,               &
                             datacopyflag=ESMF_DATACOPY_VALUE, &
                             keyLongName="ESMF Mask", rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return

    deallocate(arbSeqIndexList)

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": leaving "//METHOD, ESMF_LOGMSG_INFO)
#endif


    !locstream_discharge=ESMF_LocStreamCreate(locstream_init, &
    !              background=mesh, rc=rc)
    end function

#undef METHOD
#define METHOD "locstream_vely"

  function locstream_vely(vm, rc)
    use m_ec_module
    use m_inquire_flowgeom
    use m_meteo
    ! Return value
    type(ESMF_LocStream)       :: locstream_vely

    ! Arguments
    !type(ESMF_Mesh), intent(in)             :: mesh
    integer, intent(out)                    :: rc
    type(ESMF_VM)                           :: vm

    ! Local variables
    integer              :: gblElmCnt     ! total number of reaches elements
    integer              :: linkls_start  ! current pet start id (i.e reach fid)
    integer              :: linkls_end    ! current pet end id (i.e reach fid)
    integer              :: locElmCnt     ! number of points (i.e. reaches) on
    integer              :: a, i, j, numlocations
    integer, allocatable :: arbSeqIndexList(:)
    type(ESMF_LocStream) :: locstream_init
    type(ESMF_DistGrid)  :: distgrid
    real(ESMF_KIND_R8), allocatable    :: lat(:),lon(:), lat_final(:), lon_final(:)
    real, allocatable :: pli_lat(:), pli_lon(:), pli_lat_all(:), pli_lon_all(:), pli_comp(:)
    integer(ESMF_KIND_I4), allocatable :: bc_ids(:), mask(:)
    !integer :: esmf_comm, localPet, petCount
    type(tEcElementSet), pointer            :: elementSetPtr !< ElementSet corresponding to elementSetId
    type(tEcInstance),   pointer            :: instancePtr   !< intent(in)
    type(tEcItem), pointer                  :: targetItemPtr
    type(tEcConnection), pointer            :: connectionPtr
    type(tEcItem),       pointer            :: sourceItemPtr
    type(tEcFileReader), pointer            :: FileReaderPtr
    character(len=20) :: id_test
    character(len=256), allocatable :: ids_waterlevel(:), pli_files(:), ids_final(:)
    integer :: ii, ic, js, ncoord, minp, pli_count, pli_coord_count, pli_index, t1, t2 !< loop counter
    integer :: vbc_size
    character(len=maxMessageLen) :: line

#ifdef DEBUG
    character(ESMF_MAXSTR)  :: logMsg
    call ESMF_LogWrite(MODNAME//": entered "//METHOD, ESMF_LOGMSG_INFO)
#endif

    rc = ESMF_SUCCESS
    !-------------------------------------------------------------------
    ! Get parallel information. Here petCount is the total number of
    ! running PETs, and localPet is the number of this particular PET.
    !-------------------------------------------------------------------
    !call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, mpiCommunicator=esmf_comm, rc=rc)

    ! Allocate array for Cap to use within inital locstream data
    vbc_size = size(DFLOW_VBC_lat)
    WRITE(*,*) "current velocity x dimension"
    WRITE(*,*) vbc_size
    if(vbc_size > 0) then
       ! create local element list
       allocate(arbSeqIndexList(vbc_size))
       allocate(lat(vbc_size*2))
       allocate(lon(vbc_size*2))
       allocate(mask(vbc_size*2))
       numlocations = vbc_size*2
       j = 1
       do i = 1, vbc_size
          arbSeqIndexList(i) = i
          lat(j) = DFLOW_VBC_lat_left(i)
          lon(j) = DFLOW_VBC_lon_left(i)
          lat(j+1) = DFLOW_VBC_lat_right(i)
          lon(j+1) = DFLOW_VBC_lon_right(i)
          ! Data is available everywhere, so mask set to False
          mask(j) = 0
          mask(j+1) = 0
          j = j + 2
        end do
     else
        allocate(arbSeqIndexList(1))
        allocate(lat(1))
        allocate(lon(1))
        allocate(mask(1))
        numlocations = 1
        do i = 1, 1
            arbSeqIndexList(i) = i
            lat(i) = 0.0
            lon(i) = 0.0
            mask(i) = 1
        end do
      endif
 
    call ESMF_LogWrite("Initalizing locstream", ESMF_LOGMSG_INFO)
    !-------------------------------------------------------------------
    ! Create the LocStream:  Allocate space for the LocStream object,
    ! define the number and distribution of the locations.
    !-------------------------------------------------------------------
    locstream_vely=ESMF_LocStreamCreate(name='vely_dflow',   &
                                             !distgrid=distgrid, &
                                             localCount=numlocations, &
                                             indexflag=ESMF_INDEX_DELOCAL, &
                                             coordSys=ESMF_COORDSYS_SPH_DEG, &
                                             rc=rc)


    call ESMF_LogWrite("Initalizing locstream keys", ESMF_LOGMSG_INFO)
    !-------------------------------------------------------------------
    ! Add key data (internally allocating memory).
    !-------------------------------------------------------------------
    call ESMF_LocStreamAddKey(locstream_vely,        &
                             keyName="ESMF:Lat",          &
                             farray=lat,                   &
                             datacopyflag=ESMF_DATACOPY_VALUE, &
                             keyUnits="Degrees",     &
                             keyLongName="Latitude", rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return

    call ESMF_LocStreamAddKey(locstream_vely,        &
                             keyName="ESMF:Lon",          &
                             farray=lon,               &
                             datacopyflag=ESMF_DATACOPY_VALUE, &
                             keyUnits="Degrees",     &
                             keyLongName="Longitude", rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return

    call ESMF_LocStreamAddKey(locstream_vely,        &
                             keyName="ESMF:Mask",          &
                             farray=mask,               &
                             datacopyflag=ESMF_DATACOPY_VALUE, &
                             keyLongName="ESMF Mask", rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return

    deallocate(arbSeqIndexList)

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": leaving "//METHOD, ESMF_LOGMSG_INFO)
#endif


    !locstream_discharge=ESMF_LocStreamCreate(locstream_init, &
    !              background=mesh, rc=rc)
    end function



  !-----------------------------------------------------------------------------
#undef METHOD
#define METHOD "NWM_HYCMeshUGRIDCreate()"

  function NWM_HYCMeshUGRIDCreate(rc)
    ! Return value
    type(ESMF_Mesh)            :: NWM_HYCMeshUGRIDCreate
    integer                    :: rc

    ! Local variables
    character(*), parameter    :: filename = "Continental_Mesh_v1_net.nc"
    type(ESMF_FileFormat_Flag) :: fileformat
    ! -- The following arguments require argument keyword syntax (e.g. rc=rc). --
    type(ESMF_MeshLoc)         :: maskFlag
    character(20)              :: varname
    ! A Distgrid describing the user-specified distribution of the nodes across
    ! the PETs
    type(ESMF_DistGrid)        :: nodalDistgrid   
    ! A Distgrid describing the user-specified distribution of the elements
    ! across the PETs
    type(ESMF_DistGrid)        :: elementDistgrid

    integer :: parametricDim
    integer :: spatialDim
    integer :: nodeCount
    integer, pointer :: nodeIds(:)
    real(ESMF_KIND_R8), pointer :: nodeCoords(:)
    integer, pointer :: nodeOwners(:)
    logical :: nodeMaskIsPresent
    integer, pointer :: nodeMask(:)
    integer :: elementCount
    integer, pointer :: elementIds(:)
    integer, pointer :: elementTypes(:)
    integer :: elementConnCount
    integer, pointer :: elementConn(:)
    logical :: elementMaskIsPresent
    integer, pointer :: elementMask(:)
    logical :: elementAreaIsPresent
    real(ESMF_KIND_R8), pointer :: elementArea(:)
    logical :: elementCoordsIsPresent
    real(ESMF_KIND_R8), pointer :: elementCoords(:)
    logical :: nodalDistgridIsPresent
    logical :: elementDistgridIsPresent
    integer :: numOwnedNodes
    real(ESMF_KIND_R8), pointer :: ownedNodeCoords(:)
    integer :: numOwnedElements
    real(ESMF_KIND_R8), pointer :: ownedElemCoords(:)
#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": entered "//METHOD, ESMF_LOGMSG_INFO)
#endif

    rc = ESMF_SUCCESS

    ! When creating a ESMF Mesh from a UGRID file, the user has to provide the
    ! mesh topology variable name to ESMF_MeshCreate().
    NWM_HYCMeshUGRIDCreate = ESMF_MeshCreate(trim(filename), &
            fileformat=ESMF_FILEFORMAT_UGRID, &
            rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return
    
    call ESMF_MeshGet(NWM_HYCMeshUGRIDCreate, nodalDistgrid=nodalDistgrid, &
                      ownedNodeCoords=ownedNodeCoords,numOwnedNodes=numOwnedNodes)
    !call ESMF_MeshGet(NWM_HYCMeshUGRIDCreate, parametricDim, spatialDim, &
    !                  nodeCount, nodeIds, nodeCoords, nodeOwners, &
    !                  nodeMaskIsPresent, nodeMask,&
    !                  elementCount, elementIds, elementTypes, &
    !                  elementConnCount, elementConn, &
    !                  elementMaskIsPresent,elementMask, &
    !                  elementAreaIsPresent, elementArea, &
    !                  elementCoordsIsPresent, elementCoords, &
    !                  nodalDistgridIsPresent, nodalDistgrid, &
    !                  elementDistgridIsPresent, elementDistgrid, &
    !                  numOwnedNodes, ownedNodeCoords, &
    !                  numOwnedElements, ownedElemCoords, rc=rc)
    !WRITE(*,*) "q1 size"
    !WRITE(*,*) size(q1)
    !WRITE(*,*) size(xu)
    !WRITE(*,*) xu(1:numk)
    !WRITE(*,*) size(ownedNodeCoords)
    !WRITE(*,*) ownedNodeCoords(1)
    !WRITE(*,*) ownedNodeCoords(2)
    !WRITE(*,*) "END"
    
#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": leaving "//METHOD, ESMF_LOGMSG_INFO)
#endif

  end function

  !-----------------------------------------------------------------------------
#undef METHOD
#define METHOD "NWM_HYCMeshCreate()"
  function  NWM_HYCMeshCreate(rc)
    ! Return value
    type(ESMF_Mesh)        :: NWM_HYCMeshCreate

    type(ESMF_DistGrid)    :: distgrid ! For mpi

    integer, intent(out)   :: rc

    ! Grid administration

    ! Use the same variable names as in the ESMF docs
    ! Dimensions and counting

    ! Dimension of the topology of the Mesh. (E.g. a mesh constructed
    ! of squares would have a parametric dimension of 2, whereas a
    ! Mesh constructed of cubes would have one of 3.)
    integer                      :: parametricDim = 2
    ! The number of coordinate dimensions needed to describe the
    ! locations of the nodes making up the Mesh. For a manifold, the
    ! spatial dimesion can be larger than the parametric dim (e.g. the
    ! 2D surface of a sphere in 3D space), but it can't be smaller.
    integer                      :: spatialDim = 2

    integer                      :: numNodes
    integer                      :: numQuadElems
    integer                      :: numTriElems
    integer                      :: numTotElems

    ! Variables An array containing the physical coordinates of the
    ! nodes to be created on this PET. This input consists of a 1D
    ! array the size of the number of nodes on this PET times the
    ! Mesh's spatial dimension (spatialDim). The coordinates in this
    ! array are ordered so that the coordinates for a node lie in
    ! sequence in memory. (e.g. for a Mesh with spatial dimension 2,
    ! the coordinates for node 1 are in nodeCoords(0) and
    ! nodeCoords(1), the coordinates for node 2 are in nodeCoords(2)
    ! nodeCoords(1), the coordinates for node 2 are in nodeCoords(2)
    ! and nodeCoords(3), etc.).
    real(ESMF_KIND_R8), allocatable :: nodeCoords(:)

    ! An array containing the global ids of the nodes to be created on
    ! this PET. This input consists of a 1D array the size of the
    ! number of nodes on this PET.
    integer, allocatable         :: nodeIds(:)
    integer, allocatable         :: nodeOwners(:)

    ! An array containing the global ids of the elements to be created
    ! on this PET. This input consists of a 1D array the size of the
    ! number of elements on this PET.
    integer, allocatable         :: elementIds(:)

    ! An array containing the types of the elements to be created on
    ! this PET. The types used must be appropriate for the parametric
    ! dimension of the Mesh. Please see Section 29.2.1 for the list of
    ! options. This input consists of a 1D array the size of the
    ! number of elements on this PET.
    integer, allocatable         :: elementTypes(:)

    ! An array containing the indexes of the sets of nodes to be
    ! connected together to form the elements to be created on this
    ! PET. The entries in this list are NOT node global ids, but
    ! rather each entry is a local index (1 based) into the list of
    ! nodes which were created on this PET by the previous
    ! ESMF_MeshAddNodes() call. In other words, an entry of 1
    ! indicates that this element contains the node described by
    ! nodeIds(1), nodeCoords(1), etc. passed into the
    ! ESMF_MeshAddNodes() call on this PET. It is also important to
    ! note that the order of the nodes in an element connectivity list
    ! matters. Please see Section 29.2.1 for diagrams illustrating the
    ! correct order of nodes in a element. This input consists of a 1D
    ! array with a total size equal to the sum of the number of nodes
    ! in each element on this PET. The number of nodes in each element
    ! is implied by its element type in elementTypes. The nodes for
    ! each element are in sequence in this array (e.g. the nodes for
    ! element 1 are elementConn(1), elementConn(2), etc.).
    integer, allocatable         :: elementConn(:) !  4*numQuadElems+3*numTriElems

    ! For coupling only these elements are supported.
    ! Cell types
    integer :: TRI = ESMF_MESHELEMTYPE_TRI
    integer :: QUAD = ESMF_MESHELEMTYPE_QUAD
    ! iters
    integer :: i,j,k

#ifdef DEBUG
    character(ESMF_MAXSTR)      :: logMsg
#endif

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": entered "//METHOD, ESMF_LOGMSG_INFO)
#endif

    rc = ESMF_SUCCESS

    write(*,*) 'Step 1'
    ! Create a Mesh as a 3 step process (dims, nodes, elements)
    NWM_HYCMeshCreate = ESMF_MeshCreate(parametricDim=parametricDim,spatialDim=spatialDim, &
                         coordSys=ESMF_COORDSYS_SPH_DEG,rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return
    
    ! Create the nodes...
    write(*,*) 'Making grid with ', numk, ' nodes', sdmn, ' rank'
    numNodes = numk
   
    ! Fill the indices
    allocate(nodeIds(numNodes))
    forall (i=1:numNodes:1) nodeIds(i) = i

    ! nodeCoords=(/ xk(1), yk(1), xk(2), yk(2), ....
    allocate(nodeCoords(2*numNodes))
    nodeCoords(1:(2*numNodes):2) = xk(1:numNodes)
    nodeCoords(2:(2*numNodes):2) = yk(1:numNodes)

    ! Set all nodes owned to pet0
    allocate(nodeOwners(numNodes))
    nodeOwners=0

    write(*,*) 'Sizes: netcell', shape(netcell)
    write(*,*) 'Sizes: numnodes', numnodes
    write(*,*) 'Sizes: nump', nump, '*'
    write(*,*) 'Sizes: numk', numk, '*'
    write(*,*) 'Sizes: ndx', ndx
    write(*,*) 'Sizes: s1', size(s1), '*'
    write(*,*) 'Sizes: q1', size(q1), '*'
    write(*,*) 'Sizes: tnod', size(nd), '*'

    write(*,*) 'Step 2'
    call ESMF_MeshAddNodes(NWM_HYCMeshCreate, nodeIds, nodeCoords, nodeOwners, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return

    ! Let's define the elements
    ! Following example of the reference manual
    numTotElems = nump
    numQuadElems = 0
    numTriElems = 0
    ! This is almost similar to the VTK data structure
    allocate(elementTypes(numTotElems))
    allocate(elementIds(numTotElems))

    do k=1,numTotElems
       ! Use the netcells.
       select case(size(nd(k)%nod))
       !select case(3)
       case(3)
          elementTypes(k) = TRI
          numTriElems = numTriElems + 1
       case(4)
          elementTypes(k) = QUAD
          numQuadElems = numQuadElems + 1
       case(5)
          ! Pentagon shape
          elementTypes(k) = 5
          numQuadElems = numQuadElems + 1          
       case(6)
          ! Hexagon shape
          elementTypes(k) = 6
          numQuadElems = numQuadElems + 1
       case default
          write(*,*) 'Assertion failed expecting elements of 3,4 nodes, got', nd(k)%nod, ' for ', k
       end select
    end do
    ! check...
    if (.not. (numTriElems + numQuadElems) .eq. numTotElems) rc=10
    ! A list of all nodes (without the count that vtk uses)
    allocate(elementConn(4*numQuadElems+3*numTriElems))

    ! Just the counters. (1 based)
    forall (i=1:numTotElems:1) elementIds(i) = i

    ! Setup the connections
    j = 1
    do k=1,numTotElems
       ! Use the netcells. (TODO check)
       !select case(size(nd(k)%nod))
       select case (3)
       case(3)
          elementConn(j:(j+3)) = nd(k)%nod(1:3)
          j = j+3
       case(4)
          elementConn(j:(j+4)) = nd(k)%nod(1:4)
          j = j+4
       case default
          write(*,*) 'Assertion failed expecting elements of 3,4 nodes, got', nd(k)%nod, ' for ', k
       end select
    end do
    write(*,*) 'Step 3'

    ! TODO - Beheen - this give error in ESMF  
    !call ESMF_MeshAddElements(NWM_HYCMeshCreate, elementIds, elementTypes, elementConn, rc=rc)
    !if (ESMF_STDERRORCHECK(rc)) return

    ! Cleanup...
    deallocate(nodeIds)
    deallocate(nodeCoords)
    deallocate(nodeOwners)
    deallocate(elementIds)
    deallocate(elementTypes) 
    deallocate(elementConn)

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": leaving "//METHOD, ESMF_LOGMSG_INFO)
#endif

  end function


  !-----------------------------------------------------------------------------
  ! Create field using internal memory
  !-----------------------------------------------------------------------------

#undef METHOD
#define METHOD "NWM_HYCFieldCreate"

  function NWM_HYCFieldCreate(stdName,mesh,locstream,rc)

    type(ESMF_Field) :: NWM_HYCFieldCreate

    character(*), intent(in)                :: stdName
    type(ESMF_Mesh), intent(in)             :: mesh
    type(ESMF_LocStream), intent(in)        :: locstream
    integer, intent(out)                    :: rc

    type(ESMF_VM) :: vm
    integer       :: numOwnedElements, numOwnedNodes

    type(ESMF_Field)         :: field
    type(ESMF_TypeKind_Flag) :: typekind
    type(ESMF_MeshLoc)       :: meshloc
    type(ESMF_LocStream)     :: locstream_discharge_array
    integer                  :: nelements


    real(ESMF_KIND_R8), dimension(:), pointer :: farrayPtr => null()
    real(ESMF_KIND_R8), dimension(:), pointer :: link_farrayPtr => null()
    !integer, dimension(:), pointer :: link_farrayPtr => null()
    character(ESMF_MAXSTR)                    :: name


#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": entered "//METHOD, ESMF_LOGMSG_INFO)
#endif

    rc = ESMF_SUCCESS

    ! Add all the fields here... Pointers should update automaticly....
    ! (don't copy data)
    call ESMF_LogWrite(trim(stdName), ESMF_LOGMSG_INFO)
    SELECT CASE (trim(stdName))
      CASE ('sea_surface_height_above_sea_level_dflow')

          if(nbndz > 0) then
              allocate(farrayPtr(nbndz*2))
          else
              allocate(farrayPtr(1))
          endif

          NWM_HYCFieldCreate = ESMF_FieldCreate(locstream, &
                                                farrayPtr, &
                                                ESMF_INDEX_DELOCAL, &
                                                datacopyflag=ESMF_DATACOPY_REFERENCE,&
                                                name=trim(stdName), rc=rc)
          if(ESMF_STDERRORCHECK(rc)) return ! bail out



      CASE ('eastward_wave_radiation_stress')

          if(nbndz > 0) then
              allocate(farrayPtr(nbndz))
          else
              allocate(farrayPtr(1))
          endif
          NWM_HYCFieldCreate = ESMF_FieldCreate(locstream, &
                                                farrayPtr, &
                                                ESMF_INDEX_DELOCAL, &
                                                datacopyflag=ESMF_DATACOPY_REFERENCE,&
                                                name=trim(stdName), rc=rc)
          if(ESMF_STDERRORCHECK(rc)) return ! bail out


      CASE ('flow_rate')

          if(nbndu > 0) then
              allocate(farrayPtr(nbndu*2))
          else
              allocate(farrayPtr(1))
          endif

          NWM_HYCFieldCreate = ESMF_FieldCreate(locstream, &
                                                farrayPtr, &
                                                ESMF_INDEX_DELOCAL, &
                                                datacopyflag=ESMF_DATACOPY_REFERENCE,&
                                                name=trim(stdName), rc=rc)
          if(ESMF_STDERRORCHECK(rc)) return ! bail out



      CASE ('lateral_flow_rate')

          if(size(NWM_CommonID) > 0) then
              allocate(farrayPtr(size(NWM_CommonID)))
          else
              allocate(farrayPtr(1))
          endif

          NWM_HYCFieldCreate = ESMF_FieldCreate(locstream, &
                                                farrayPtr, &
                                                ESMF_INDEX_DELOCAL, &
                                                datacopyflag=ESMF_DATACOPY_REFERENCE,&
                                                name=trim(stdName), rc=rc)
          if(ESMF_STDERRORCHECK(rc)) return ! bail out


      CASE ('surface_northward_sea_water_velocity_dflow')

          if(size(DFLOW_VBC_lat) > 0) then
              allocate(farrayPtr(size(DFLOW_VBC_lat)*2))
          else
              allocate(farrayPtr(1))
          endif

          NWM_HYCFieldCreate = ESMF_FieldCreate(locstream, &
                                                farrayPtr, &
                                                ESMF_INDEX_DELOCAL, &
                                                datacopyflag=ESMF_DATACOPY_REFERENCE,&
                                                name=trim(stdName), rc=rc)
          if(ESMF_STDERRORCHECK(rc)) return ! bail out


      CASE ('surface_eastward_sea_water_velocity_dflow')

          if(size(DFLOW_VBC_lat) > 0) then
              allocate(farrayPtr(size(DFLOW_VBC_lat)*2))
          else
              allocate(farrayPtr(1))
          endif

          NWM_HYCFieldCreate = ESMF_FieldCreate(locstream, &
                                                farrayPtr, &
                                                ESMF_INDEX_DELOCAL, &
                                                datacopyflag=ESMF_DATACOPY_REFERENCE,&
                                                name=trim(stdName), rc=rc)
          if(ESMF_STDERRORCHECK(rc)) return ! bail out

      CASE ('link')
          if(nbndu > 0) then
              allocate(link_farrayPtr(nbndu*2))
          else
              allocate(link_farrayPtr(1))
          endif

          NWM_HYCFieldCreate = ESMF_FieldCreate(locstream, &
                                                link_farrayPtr, &
                                                ESMF_INDEX_DELOCAL, &
                                                datacopyflag=ESMF_DATACOPY_REFERENCE,&
                                                name=trim(stdName), rc=rc)
          if(ESMF_STDERRORCHECK(rc)) return ! bail out

      CASE ('lat')
          if(nbndu > 0) then
              allocate(farrayPtr(nbndu))
          else
              allocate(farrayPtr(1))
          endif

          NWM_HYCFieldCreate = ESMF_FieldCreate(locstream, &
                                                farrayPtr, &
                                                ESMF_INDEX_DELOCAL, &
                                                datacopyflag=ESMF_DATACOPY_REFERENCE,&
                                                name=trim(stdName), rc=rc)
          if(ESMF_STDERRORCHECK(rc)) return ! bail out

      CASE ('lon')

          if(nbndu > 0) then
              allocate(farrayPtr(nbndu))
          else
              allocate(farrayPtr(1))
          endif

          NWM_HYCFieldCreate = ESMF_FieldCreate(locstream, &
                                                farrayPtr, &
                                                ESMF_INDEX_DELOCAL, &
                                                datacopyflag=ESMF_DATACOPY_REFERENCE,&
                                                name=trim(stdName), rc=rc)
          if(ESMF_STDERRORCHECK(rc)) return ! bail out
    
      CASE DEFAULT
        call ESMF_LogSetError(ESMF_RC_ARG_OUTOFRANGE, &
          msg=METHOD//": Field hookup missing: "//trim(stdName), &
                                      file=FILENAME,rcToReturn=rc)
        return  ! bail out
    END SELECT

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": leaving "//METHOD, ESMF_LOGMSG_INFO)
#endif

  end function


  subroutine make_unstruc_fieldbundle(mesh, stateintent, fieldbundle, rc)

    type(ESMF_Mesh), intent(inout)          :: mesh
    type(ESMF_StateIntent_Flag), intent(in) :: stateintent
    type(ESMF_FieldBundle), intent(out)     :: fieldbundle
    integer, intent(out)                    :: rc
 
    type(ESMF_Field)         :: field
    type(ESMF_TypeKind_Flag) :: typekind
    type(ESMF_MeshLoc)       :: meshloc
    integer                  :: nelements

    real(ESMF_KIND_R8), dimension(:), pointer :: farrayPtr
    character(ESMF_MAXSTR)                    :: name


    if (stateintent .eq. ESMF_STATEINTENT_EXPORT) then

      ! Add all the fields here... Pointers should update automaticly....
      ! (don't copy data)
      name="s1"

      if (allocated(s1)) then
        farrayPtr => s1(1:numk)
        field = ESMF_FieldCreate(mesh, farrayPtr=farrayPtr, meshloc=ESMF_MESHLOC_NODE, name=name, rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return
         
        call ESMF_FieldBundleAdd(fieldbundle, fieldList=(/field/), rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return
          
      else
        write(*,*) 's1 not allocated skipping'
      end if

      name="ucx"
      if (allocated(ucx)) then
        farrayPtr => ucx(1:numk)
        field = ESMF_FieldCreate(mesh, farrayPtr=farrayPtr, meshloc=ESMF_MESHLOC_NODE, name=name, rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return
          
        call ESMF_FieldBundleAdd(fieldbundle, fieldList=(/field/), rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return
      
      else
        write(*,*) 'ucx not allocated skipping'
      end if

      name="ucy"
      if (allocated(ucy)) then
        farrayPtr => ucy(1:numk)
        field = ESMF_FieldCreate(mesh, farrayPtr=farrayPtr,meshloc=ESMF_MESHLOC_NODE, name=name, rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return
     
        call ESMF_FieldBundleAdd(fieldbundle, fieldList=(/field/),rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return
    
      else
        write(*,*) 'ucy not allocated skipping'
      end if
      ! TODO extend with edge location

      name="hs"
      if (allocated(hs)) then
        farrayPtr => hs(1:numk)
        field = ESMF_FieldCreate(mesh, farrayPtr=farrayPtr,meshloc=ESMF_MESHLOC_NODE, name=name, rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return
       
        call ESMF_FieldBundleAdd(fieldbundle, fieldList=(/field/),rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return
     
      else
        write(*,*) 'hs not allocated skipping'
      end if

      ! TODO Wait for ESMF for best approach....
      ! test with edges...
      ! name="u1"
      ! farrayPtr => u1
      ! field = ESMF_FieldCreate(mesh,farrayPtr=farrayPtr,meshloc=ESMF_MESHLOC_ELEMENT, name=name, rc=rc)
      ! if (ESMF_STDERRORCHECK(rc)) return
      ! call ESMF_FieldBundleAdd(fieldbundle, fieldList=(/field/),rc=rc)
      ! if (ESMF_STDERRORCHECK(rc)) return

      end if


  end subroutine make_unstruc_fieldbundle
                        

#undef METHOD
#define METHOD "NWM_HYCClock"

  subroutine NWM_HYCClock(dt,rc)
    ! ARGUMENTS
    real                          :: dt
    integer, intent(out)          :: rc

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": entered "//METHOD, ESMF_LOGMSG_INFO)
#endif

    rc = ESMF_SUCCESS

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": leaving "//METHOD, ESMF_LOGMSG_INFO)
#endif

  end subroutine


#undef METHOD
#define METHOD "NWM_Read_HUCS_agg"

  subroutine NWM_Read_HUCS_agg(rank)
  ! Use module variables NWM_CommonID, NWM_lat, NWM_lon, HUCS_ID

  ! Let each thread use their own unqiue file id in order to not mix up files
  integer,intent(in)                   :: rank

  integer :: nlines, row, io, i
  real(ESMF_KIND_R8) :: min_val, max_val
  real(ESMF_KIND_R8), dimension(:), allocatable :: unique

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": entered "//METHOD, ESMF_LOGMSG_INFO)
#endif

  ! Initalize nlines to find in csv file
  nlines = 0

  ! Open csv file to read
  !OPEN (rank, file = 'NWMIDs_USATL_GoM_HUC12.csv',action="read",status="old")
  OPEN (115, file = 'USAtl_NWM_streams_interior_HUC.csv',action="read",status="old",form="formatted")
  ! Loop through csv file to find number of rows
  DO
     READ(115,*,iostat=io)
     IF (io/=0) EXIT
     nlines = nlines + 1
  END DO

  ! Rewind csv file to read again
  REWIND(115)

  ! Allocate arrays to read csv data as global variables for NWM NUOPC Cap
  ALLOCATE(NWM_row(nlines))
  ALLOCATE(NWM_CommonID(nlines))
  ALLOCATE(NWM_lat(nlines))
  ALLOCATE(NWM_lon(nlines))
  ALLOCATE(HUCS_ID(nlines))

  ! Loop through csv file and read data to global variables
  DO row = 1, nlines
     READ(115,*) NWM_row(row), NWM_CommonID(row), NWM_lon(row), NWM_lat(row), HUCS_ID(row)
  END DO

  CLOSE(115)

  ! Now find the unique HUC ID values within csv file
  ALLOCATE(unique(nlines))
  min_val = minval(HUCS_ID)-1
  max_val = maxval(HUCS_ID)
  i = 0
  do while (min_val < max_val)
     i = i + 1
     min_val = minval(HUCS_ID, mask=HUCS_ID>min_val)
     unique(i) = min_val
  end do

  ! Now allocate unique HUCS ID and assign data
  allocate(HUCS_unique_ID(i), source=unique(1:i))

  ! Allocate NWM streamflow array based on HUCS unique ID length
  allocate(NWM_HUCS_agg_streamflow(i))

  write(*,*) "NWM Common ID"
  WRITE(*,*) NWM_CommonID(1:10)
  write(*,*) "HUCS ID"
  write(*,*) HUCS_ID(1:10)
  write(*,*) "unique amount of HUCS ID"
  write(*,*) i
  write(*,*) "HUCS unique ID"
  write(*,*) HUCS_unique_ID(1:10)   

  DEALLOCATE(unique)
#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": leaving "//METHOD, ESMF_LOGMSG_INFO)
#endif

  end subroutine

#undef METHOD
#define METHOD "DFLOW_VBC_derivation"

  subroutine DFLOW_VBC_derivation(vm,rc)

  integer, intent(out)                    :: rc
  type(ESMF_VM)                           :: vm

  integer                                 :: esmf_comm, ierr
  integer                     :: pet_loop, nbndt_count, thread_count, nbndt_total
  real(ESMF_KIND_R8), dimension(:), allocatable   :: agg_lat, lat_total, lat_gather, lat_unique
  real(ESMF_KIND_R8), dimension(:), allocatable   :: agg_lon, lon_total, lon_gather
  real(ESMF_KIND_R8), dimension(:), allocatable   :: agg_lon_left, lon_total_left, lon_gather_left
  real(ESMF_KIND_R8), dimension(:), allocatable   :: agg_lon_right, lon_total_right, lon_gather_right
  real(ESMF_KIND_R8), dimension(:), allocatable   :: agg_lat_left, lat_total_left, lat_gather_left
  real(ESMF_KIND_R8), dimension(:), allocatable   :: agg_lat_right, lat_total_right, lat_gather_right
  integer, dimension(:), allocatable :: agg_id, id_total, id_gather
  real(ESMF_KIND_R8) :: lat_temp, lon_temp, lat_temp_right, lat_temp_left, lon_temp_right, lon_temp_left
  integer :: id_temp, i, j, unique, unique_index, vel_loop, coord_loop


#ifdef DEBUG
   call ESMF_LogWrite(MODNAME//": entering "//METHOD, ESMF_LOGMSG_INFO)
#endif


  ! details Get MPI_communicator from ESMF VM.
  call ESMF_VMGet(vm, mpiCommunicator=esmf_comm, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
  line=__LINE__, &
  file=__FILE__)) &
  return  ! bail out

  !$OMP PARALLEL
  !$OMP BARRIER
  call MPI_ALLREDUCE(nbndt,VBC_count,1,MPI_INTEGER,MPI_MAX,esmf_comm,ierr)
  

  ! Get number of threads used for D-Flow
  call MPI_Comm_size(esmf_comm, thread_count, ierr)


  ALLOCATE(agg_lat(VBC_count))
  ALLOCATE(agg_lon(VBC_count))
  ALLOCATE(agg_id(VBC_count))

  ALLOCATE(agg_lat_left(VBC_count))
  ALLOCATE(agg_lon_left(VBC_count))
  ALLOCATE(agg_lat_right(VBC_count))
  ALLOCATE(agg_lon_right(VBC_count))

  agg_lat(:) = 0.0
  agg_lon(:) = 0.0
  agg_lat_right(:) = 0.0
  agg_lon_right(:) = 0.0
  agg_lat_left(:) = 0.0
  agg_lon_left(:) = 0.0
  agg_id(:) = 0

  coord_loop = 1
  if(nbndt > 0) then
   DO pet_loop = 1, nbndt
     agg_lon(pet_loop) = xbndt(pet_loop)
     agg_lat(pet_loop) = ybndt(pet_loop)
     agg_lon_left(pet_loop) = wl_lon_left(pet_loop)
     agg_lat_left(pet_loop) = wl_lat_left(pet_loop)
     agg_lon_right(pet_loop) = wl_lon_right(pet_loop)
     agg_lat_right(pet_loop) = wl_lat_right(pet_loop)
     read(idbndz(pet_loop)(10:),*) agg_id(pet_loop)
   ENDDO
  endif

  ALLOCATE(lat_total(VBC_count*thread_count))
  ALLOCATE(lon_total(VBC_count*thread_count))
  ALLOCATE(lat_total_left(VBC_count*thread_count))
  ALLOCATE(lon_total_left(VBC_count*thread_count))
  ALLOCATE(lat_total_right(VBC_count*thread_count))
  ALLOCATE(lon_total_right(VBC_count*thread_count))
  ALLOCATE(id_total(VBC_count*thread_count))
  !$OMP BARRIER
  call MPI_ALLGATHER(agg_lat,VBC_count,MPI_REAL8,lat_total,VBC_count,MPI_REAL8,esmf_comm,ierr)
  call MPI_ALLGATHER(agg_lon,VBC_count,MPI_REAL8,lon_total,VBC_count,MPI_REAL8,esmf_comm,ierr)
  call MPI_ALLGATHER(agg_lat_left,VBC_count,MPI_REAL8,lat_total_left,VBC_count,MPI_REAL8,esmf_comm,ierr)
  call MPI_ALLGATHER(agg_lon_left,VBC_count,MPI_REAL8,lon_total_left,VBC_count,MPI_REAL8,esmf_comm,ierr)
  call MPI_ALLGATHER(agg_lat_right,VBC_count,MPI_REAL8,lat_total_right,VBC_count,MPI_REAL8,esmf_comm,ierr)
  call MPI_ALLGATHER(agg_lon_right,VBC_count,MPI_REAL8,lon_total_right,VBC_count,MPI_REAL8,esmf_comm,ierr)
  call MPI_ALLGATHER(agg_id,VBC_count,MPI_INTEGER,id_total,VBC_count,MPI_INTEGER,esmf_comm,ierr)

  nbndt_count = 0
  do pet_loop = 1, VBC_count * thread_count
   if(lat_total(pet_loop) .ne. 0.0) then
     nbndt_count = nbndt_count + 1
   endif
  enddo

  !WRITE(*,*) "nbndt result after MPI gather"
  !WRITE(*,*) nbndt_count

  !$OMP END PARALLEL


  ! Now gather nonzero data aggregated from threads
  ALLOCATE(lat_gather(nbndt_count))
  ALLOCATE(lon_gather(nbndt_count))
  ALLOCATE(lat_gather_left(nbndt_count))
  ALLOCATE(lon_gather_left(nbndt_count))
  ALLOCATE(lat_gather_right(nbndt_count))
  ALLOCATE(lon_gather_right(nbndt_count))
  ALLOCATE(id_gather(nbndt_count))
  nbndt_total = 1
  do pet_loop = 1, VBC_count * thread_count
   if(lat_total(pet_loop) .ne. 0.0) then
     lat_gather(nbndt_total) = lat_total(pet_loop)
     lon_gather(nbndt_total) = lon_total(pet_loop)
     lat_gather_left(nbndt_total) = lat_total_left(pet_loop)
     lon_gather_left(nbndt_total) = lon_total_left(pet_loop)
     lat_gather_right(nbndt_total) = lat_total_right(pet_loop)
     lon_gather_right(nbndt_total) = lon_total_right(pet_loop)
     id_gather(nbndt_total) = id_total(pet_loop)
     nbndt_total = nbndt_total + 1
   endif
  enddo


  ! We have duplicates of boundaries in dataset, so we need to find unique
  ! values only and allocate the data to the global arrays

  ! We need to first find the amount of unique indices we have in arrays
  ALLOCATE(lat_unique(size(lat_gather)))
  unique = 1
  lat_unique(1) = lat_gather(1)
  first : do i = 2, size(lat_gather)
   do j = 1, unique
     if(lat_unique(j) .eq. lat_gather(i)) then
       cycle first
     endif
   end do
   unique = unique + 1
   lat_unique(unique) = lat_gather(i)
  end do first


  !write(*,*) "Number of unique values"
  !write(*,*) unique

  !Now allocate global arrays based on number of unique indices and read the
  !unique data only to the global arrays
  ALLOCATE(DFLOW_VBC_lat(unique))
  ALLOCATE(DFLOW_VBC_lon(unique))
  ALLOCATE(DFLOW_VBC_lat_left(unique))
  ALLOCATE(DFLOW_VBC_lon_left(unique))
  ALLOCATE(DFLOW_VBC_lat_right(unique))
  ALLOCATE(DFLOW_VBC_lon_right(unique))
  ALLOCATE(DFLOW_VBC_ID(unique))
  unique_index = 1
  DFLOW_VBC_lat(1) = lat_gather(1)
  DFLOW_VBC_lon(1) = lon_gather(1)
  DFLOW_VBC_lat_left(1) = lat_gather_left(1)
  DFLOW_VBC_lon_left(1) = lon_gather_left(1)
  DFLOW_VBC_lat_right(1) = lat_gather_right(1)
  DFLOW_VBC_lon_right(1) = lon_gather_right(1)
  DFLOW_VBC_ID(1) = id_gather(1)
  next : do i = 2, size(lat_gather)
   do j = 1, unique_index
     if(DFLOW_VBC_lat(j) .eq. lat_gather(i)) then
       cycle next
     endif
   end do
   unique_index = unique_index + 1
   DFLOW_VBC_lat(unique_index) = lat_gather(i)
   DFLOW_VBC_lon(unique_index) = lon_gather(i)
   DFLOW_VBC_lat_left(unique_index) = lat_gather_left(i)
   DFLOW_VBC_lon_left(unique_index) = lon_gather_left(i)
   DFLOW_VBC_lat_right(unique_index) = lat_gather_right(i)
   DFLOW_VBC_lon_right(unique_index) = lon_gather_right(i)
   DFLOW_VBC_ID(unique_index) = id_gather(i)
  end do next


  ! Now we need to sort the data based on  descending IDs of boundaries
  DO i = 1, unique - 1
   DO j = 1, unique
     IF(DFLOW_VBC_ID(i) .LT. DFLOW_VBC_ID(j)) THEN
      id_temp = DFLOW_VBC_ID(i)
      lat_temp = DFLOW_VBC_lat(i)
      lon_temp = DFLOW_VBC_lon(i)
      lat_temp_left = DFLOW_VBC_lat_left(i)
      lon_temp_left = DFLOW_VBC_lon_left(i)
      lat_temp_right = DFLOW_VBC_lat_right(i)
      lon_temp_right = DFLOW_VBC_lon_right(i)

      DFLOW_VBC_ID(i) = DFLOW_VBC_ID(j)
      DFLOW_VBC_lat(i) = DFLOW_VBC_lat(j)
      DFLOW_VBC_lon(i) = DFLOW_VBC_lon(j)
      DFLOW_VBC_lat_left(i) = DFLOW_VBC_lat_left(j)
      DFLOW_VBC_lon_left(i) = DFLOW_VBC_lon_left(j)
      DFLOW_VBC_lat_right(i) = DFLOW_VBC_lat_right(j)
      DFLOW_VBC_lon_right(i) = DFLOW_VBC_lon_right(j)

      DFLOW_VBC_ID(j) = id_temp
      DFLOW_VBC_lat(j) = lat_temp
      DFLOW_VBC_lon(j) = lon_temp
      DFLOW_VBC_lat_left(j) = lat_temp_left
      DFLOW_VBC_lon_left(j) = lon_temp_left
      DFLOW_VBC_lat_right(j) = lat_temp_right
      DFLOW_VBC_lon_right(j) = lon_temp_right
     ENDIF
   ENDDO
  ENDDO

  WRITE(*,*) "sorting results"
  WRITE(*,*) DFLOW_VBC_ID(1:12)
  WRITE(*,*) "sorting lats"
  WRITE(*,*) DFLOW_VBC_lat(1:12)
  WRITE(*,*) "sorting lons"
  WRITE(*,*) DFLOW_VBC_lon(1:12)

  ! Now allocate the Normal, Tangential, and Distance vectors to calculate
  ! the Directional Vectors using the aligned geospatial coordinates
  ALLOCATE(DFLOW_VBC_distance(unique))
  ALLOCATE(DFLOW_VBC_distance_left(unique))
  ALLOCATE(DFLOW_VBC_distance_right(unique))
  ALLOCATE(DFLOW_VBC_UTANx(unique))
  ALLOCATE(DFLOW_VBC_UTANy(unique))
  ALLOCATE(DFLOW_VBC_UTANx_left(unique))
  ALLOCATE(DFLOW_VBC_UTANy_left(unique))
  ALLOCATE(DFLOW_VBC_UTANx_right(unique))
  ALLOCATE(DFLOW_VBC_UTANy_right(unique))
  ALLOCATE(DFLOW_VBC_UNORx(unique))
  ALLOCATE(DFLOW_VBC_UNORy(unique))
  ALLOCATE(DFLOW_VBC_UNORx_left(unique))
  ALLOCATE(DFLOW_VBC_UNORy_left(unique))
  ALLOCATE(DFLOW_VBC_UNORx_right(unique))
  ALLOCATE(DFLOW_VBC_UNORy_right(unique))

  ! first calculate the distance between two consecutive vertices along the
  ! offshort boundaries
  do vel_loop = 1, unique-1
   DFLOW_VBC_distance(vel_loop) = SQRT(((DFLOW_VBC_lat(vel_loop+1)-DFLOW_VBC_lat(vel_loop))**2+(DFLOW_VBC_lon(vel_loop+1)-DFLOW_VBC_lon(vel_loop))**2))
   DFLOW_VBC_distance_left(vel_loop) = SQRT(((DFLOW_VBC_lat_right(vel_loop)-DFLOW_VBC_lat_left(vel_loop))**2+(DFLOW_VBC_lon_right(vel_loop)-DFLOW_VBC_lon_left(vel_loop))**2))
   DFLOW_VBC_distance_right(vel_loop) = SQRT(((DFLOW_VBC_lat_right(vel_loop+1)-DFLOW_VBC_lat_right(vel_loop))**2+(DFLOW_VBC_lon_right(vel_loop+1)-DFLOW_VBC_lon_right(vel_loop))**2))
  end do
  DFLOW_VBC_distance(unique) = DFLOW_VBC_distance(unique-1)
  DFLOW_VBC_distance_left(unique) = DFLOW_VBC_distance_left(unique-1)
  DFLOW_VBC_distance_right(unique) = DFLOW_VBC_distance_right(unique-1)
  ! Calculate the UTAN and UNOR components
  do vel_loop = 1, unique-1
   DFLOW_VBC_UTANx(vel_loop) = (DFLOW_VBC_lon(vel_loop+1) - DFLOW_VBC_lon(vel_loop))/DFLOW_VBC_distance(vel_loop)
   DFLOW_VBC_UTANy(vel_loop) = (DFLOW_VBC_lat(vel_loop+1) - DFLOW_VBC_lat(vel_loop))/DFLOW_VBC_distance(vel_loop)
   DFLOW_VBC_UTANx_left(vel_loop) = (DFLOW_VBC_lon_right(vel_loop) - DFLOW_VBC_lon_left(vel_loop))/DFLOW_VBC_distance_left(vel_loop)
   DFLOW_VBC_UTANy_left(vel_loop) = (DFLOW_VBC_lat_right(vel_loop) - DFLOW_VBC_lat_left(vel_loop))/DFLOW_VBC_distance_left(vel_loop)
   DFLOW_VBC_UTANx_right(vel_loop) = (DFLOW_VBC_lon_right(vel_loop+1) - DFLOW_VBC_lon_right(vel_loop))/DFLOW_VBC_distance_right(vel_loop)
   DFLOW_VBC_UTANy_right(vel_loop) = (DFLOW_VBC_lat_right(vel_loop+1) - DFLOW_VBC_lat_right(vel_loop))/DFLOW_VBC_distance_right(vel_loop)

  end do
  DFLOW_VBC_UTANx(unique) = DFLOW_VBC_UTANx(unique-1)
  DFLOW_VBC_UTANy(unique) = DFLOW_VBC_UTANy(unique-1)
  DFLOW_VBC_UTANx_left(unique) = DFLOW_VBC_UTANx_left(unique-1)
  DFLOW_VBC_UTANy_left(unique) = DFLOW_VBC_UTANy_left(unique-1)
  DFLOW_VBC_UTANx_right(unique) = DFLOW_VBC_UTANx_right(unique-1)
  DFLOW_VBC_UTANy_right(unique) = DFLOW_VBC_UTANy_right(unique-1)


  DFLOW_VBC_UNORx(:) = DFLOW_VBC_UTANy*-1.0
  DFLOW_VBC_UNORy(:) = DFLOW_VBC_UTANx
  DFLOW_VBC_UNORx_left(:) = DFLOW_VBC_UTANy_left*-1.0
  DFLOW_VBC_UNORy_left(:) = DFLOW_VBC_UTANx_left
  DFLOW_VBC_UNORx_right(:) = DFLOW_VBC_UTANy_right*-1.0
  DFLOW_VBC_UNORy_right(:) = DFLOW_VBC_UTANx_right



  write(*,*) "distance"
  write(*,*) DFLOW_VBC_distance(1:12)
  write(*,*) "UTANx"
  write(*,*) DFLOW_VBC_UTANx(1:12)
  write(*,*) "UTANy"
  write(*,*) DFLOW_VBC_UTANy(1:12)
  write(*,*) "UNORx"
  write(*,*) DFLOW_VBC_UNORx(1:12)
  write(*,*) "UNORy"
  write(*,*) DFLOW_VBC_UNORy(1:12)
  !write(*,*) "Directional coordinates"
  !write(*,*) DFLOW_VBC_UNORx(1:20)
  !write(*,*) DFLOW_VBC_UTANy(1:20)


#ifdef DEBUG
   call ESMF_LogWrite(MODNAME//": leaving "//METHOD, ESMF_LOGMSG_INFO)
#endif

  end subroutine



end module NWM_HYC_Gluecode


