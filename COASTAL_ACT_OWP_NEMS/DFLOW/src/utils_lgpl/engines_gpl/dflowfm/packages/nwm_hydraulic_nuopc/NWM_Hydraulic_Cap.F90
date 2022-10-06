#define FILENAME "NWM_Hydraulic_Cap"
#define MODNAME "NWM_Hydraulic_Cap"
#include "NWM_NUOPC_Macros.h"

#define DEBUG=on

!export ESMF_RUNTIME_COMPLIANCECHECK=ON

! module NWM Hydraulic Coastal Engine
module NWM_HYC_Cap      
  use ESMF
  use NUOPC
  use NUOPC_Model, &
    model_routine_SS        => SetServices, &
    model_label_DataInitialize => label_DataInitialize, &
    model_label_SetClock    => label_SetClock, &
    model_label_CheckImport => label_CheckImport, &
    model_label_Advance     => label_Advance, &
    model_label_Finalize    => label_Finalize

  use unstruc_api
  use network_data
  use m_flow
  use dfm_error
  use m_flowgeom
  use m_flowtimes   
  use timespace_data
  use NWM_HYC_Gluecode
  use NWM_ESMF_Extensions
  use m_flowexternalforcings
  implicit none

  private

  public SetServices

  CHARACTER(LEN=*), PARAMETER :: label_InternalState = 'InternalState'

  type type_InternalStateStruct
    integer                  :: nfields       = size(NWM_HYCFieldList)
    integer                  :: timeSlice     = 0 ! total timesteps in unit of hours, calc. based on KDAY/KHOUR [RUNDURATION in NEMS]    
    integer                  :: timeStepInt   = 0 ! 1 timestep per hour

    type (ESMF_Clock)        :: clock(1)          ! same as model clock 
    type (ESMF_TimeInterval) :: stepTimer(1)      ! for displying timestep 
    type(ESMF_State)         :: NStateImp(1)
    type(ESMF_State)         :: NStateExp(1)
    ! added to track the driver clock
    character(len=19)        :: startTimeStr = "0000-00-00_00:00:00"

  end type

  type type_InternalState
    type(type_InternalStateStruct), pointer :: wrap
  end type

  ! some temporary debug variables
  character(len=ESMF_MAXSTR) :: logMsg

  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------
  !! The NUOPC specialized subroutines are registered during SetServices
  !-----------------------------------------------------------------------------
#undef METHOD
#define METHOD "SetServices"

  subroutine SetServices(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc

    ! local variables
    integer                    :: stat
    type(type_InternalState)   :: is

#ifdef DEBUG
    call ESMF_LogSet(flush=.true., rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    call ESMF_LogWrite(MODNAME//": entered "//METHOD, ESMF_LOGMSG_INFO)
#endif

    rc = ESMF_SUCCESS

    ! Allocate memory for this internal state and set it in the component
    allocate(is%wrap, stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg='NWM HYC: Allocation of internal state memory failed.', &
      file=FILENAME, rcToReturn=rc)) return ! bail out
!
    call ESMF_UserCompSetInternalState(gcomp, label_InternalState, is, rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

!
    ! The NUOPC model component will register the generic methods
    call NUOPC_CompDerive(gcomp, model_routine_SS, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
!
    ! switching to IPD versions
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
                           userRoutine=InitializeP0, phase=0, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    call ESMF_LogWrite("Out InitalizeP0, Entering P1", ESMF_LOGMSG_INFO)

    ! Set entry point for methods that require specific implementation
    call NUOPC_CompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv03p1"/), userRoutine=InitializeP1, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    call ESMF_LogWrite("Out InitalizeP1, Entering P3", ESMF_LOGMSG_INFO)

    call NUOPC_CompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv03p3"/), userRoutine=InitializeP3, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    call ESMF_LogWrite("Out InitalizeP3, Entering DataInitalize", ESMF_LOGMSG_INFO)
    ! Attach specializing method(s)
    call NUOPC_CompSpecialize(gcomp, specLabel=model_label_DataInitialize, &
                                          specRoutine=DataInitialize, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    call ESMF_LogWrite("Out DataInitalize, Entering model label setclock", ESMF_LOGMSG_INFO)

    call NUOPC_CompSpecialize(gcomp, speclabel=model_label_SetClock, &
                                          specRoutine=SetClock, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
!
    call ESMF_MethodRemove(gcomp, label=model_label_CheckImport, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
!
    call NUOPC_CompSpecialize(gcomp, specLabel=model_label_CheckImport, &
                                          specRoutine=CheckImport, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
!
    call NUOPC_CompSpecialize(gcomp, speclabel=model_label_Advance, &
                                     specRoutine=ModelAdvance, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
!
    call NUOPC_CompSpecialize(gcomp, specLabel=model_label_Finalize, &
                                     specRoutine=ModelFinalize, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    call ESMF_LogWrite("Done with set services", ESMF_LOGMSG_INFO)
#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": leaving "//METHOD, ESMF_LOGMSG_INFO)
#endif

  end subroutine

  !-----------------------------------------------------------------------------

#undef METHOD
#define METHOD "InitializeP0"
  
  !! phase 0: (REQUIRED, NUOPC PROVIDED)
  !! Initialize the InitializePhaseMap Attribute according to the NUOPC 
  !! Initialize Phase Definition (IPD) version 00 
  !! During initialize phase 0 the runtime configuration is read in from 
  !! model attributes and the initialization phase definition version is 
  !! set to IPDv03.
  subroutine InitializeP0(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)   :: gcomp
    type(ESMF_State)      :: importState, exportState
    type(ESMF_Clock)      :: clock
    integer, intent(out)  :: rc

    ! local variables
    character(32)              :: cname
    integer                    :: stat
    logical                    :: configIsPresent
    type(ESMF_Config)          :: config
    type(NUOPC_FreeFormat)     :: attrFF
    type(type_InternalState)   :: is
    character(len=64)          :: value
 
#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": entered "//METHOD, ESMF_LOGMSG_INFO)
#endif

    rc = ESMF_SUCCESS

    ! Query component for name, gcomp was created through driver
    call ESMF_GridCompGet(gcomp, name=cname, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    !! Beheen - at this time timeStep=runDuration=259200

    ! Query Component for its internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(gcomp, label_InternalState, is, rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
!
    ! Check gcomp for config - where this is coming from -- driver??
    call ESMF_GridCompGet(gcomp, configIsPresent=configIsPresent, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    if (configIsPresent) then

      ! Read and ingest free format component attributes
      call ESMF_GridCompGet(gcomp, config=config, rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out
     
      !call  PRINT_ESMF_CONFIG(config,label="EARTH_attributes::")

      attrFF = NUOPC_FreeFormatCreate(config, &
        label=trim(cname)//"_attributes::", relaxedflag=.true., rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out

      call NUOPC_CompAttributeIngest(gcomp, attrFF, addFlag=.true., rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out

      call NUOPC_FreeFormatDestroy(attrFF, rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    endif
!
!
    ! Time step initialization
    call ESMF_AttributeGet(gcomp, name="time_step", value=value, defaultValue="0", &
                           convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    
    !! at this time time step is set to 0, clock is not changed
    read (value,*,iostat=stat) is%wrap%timeStepInt    !keeps track of time

    if (stat /= 0) then
      call ESMF_LogSetError(ESMF_FAILURE, &
        msg=METHOD//": Cannot convert "//trim(value)//" to integer.", &
        line=__LINE__,file=__FILE__,rcToReturn=rc)
      return  ! bail out
    endif
!

    ! Add the import/export fields into global field dict.
    call NWM_HYCFieldDictionaryAdd(rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    call ESMF_LogWrite("Before NUOPC_CompFilterPhaseMap", ESMF_LOGMSG_INFO)
    ! Switch to IPDv03 by filtering all other phaseMap entries
    call NUOPC_CompFilterPhaseMap(gcomp, ESMF_METHOD_INITIALIZE, &
               acceptStringList=(/"IPDv03p"/), rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    call ESMF_LogWrite("After NUOPC_CompFilterPhaseMap", ESMF_LOGMSG_INFO)

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": leaving "//METHOD, ESMF_LOGMSG_INFO)
#endif

  end subroutine

  !-----------------------------------------------------------------------------

#undef METHOD
#define METHOD "InitializeP1"

  !! IPDv03p - P1 model/mediator/driver advertise their import and export 
  !! Fields and set the TransferOfferGeomObject Attribute
  !! During phase1 initialization, the model init method is called and 
  !! the import and export fields are advertised 

  subroutine InitializeP1(gcomp, importState, exportState, clock, rc)
    use mpi

    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc
        
    ! Local variables
    character(32)               :: cname
    character(32)               :: tmpName
    type(type_InternalState)    :: is
    type(ESMF_VM)               :: vm
    integer                     :: fIndex, esmf_comm, dflow_comm, ierr
    logical                     :: vmIsPresent, configIsPresent, clockIsPresent
    integer                     :: pet_loop, pet_start, pet_end
    integer                     :: root = 0
    integer                     :: nbndt_count, thread_count, nbndt_total
    real(ESMF_KIND_R8), dimension(:), allocatable   :: agg_lat, lat_total, lat_gather, lat_unique
    real(ESMF_KIND_R8), dimension(:), allocatable   :: agg_lon, lon_total, lon_gather
    integer, dimension(:), allocatable :: agg_id, id_total, id_gather
    real(ESMF_KIND_R8) :: lat_temp, lon_temp
    integer :: id_temp, i, j, unique, unique_index
    
#ifdef DEBUG
    character(ESMF_MAXSTR)      :: logMsg
#endif

    call ESMF_LogWrite("Entered P1", ESMF_LOGMSG_INFO)

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": entered "//METHOD, ESMF_LOGMSG_INFO)
#endif


    rc = ESMF_SUCCESS

    ! Query component for name
    call ESMF_GridCompGet(gcomp, name=cname, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    ! Query Component for its internal State
    nullify(is%wrap)
    ! copies initial internal state values into this local is
    call ESMF_UserCompGetInternalState(gcomp, label_InternalState, is, rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    
    ! Query VM to pass to model, vm was created in driver during
    ! child (model component) creation
    call ESMF_GridCompGet(gcomp, vm=vm, rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out

    ! details Get current ESMF VM.
    !call ESMF_VMGetCurrent(vm, rc=rc)
    !if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    !  line=__LINE__, &
    !  file=__FILE__)) &
    !  return  ! bail out

    ! details Get MPI_communicator from ESMF VM.
    call ESMF_VMGet(vm, mpiCommunicator=esmf_comm, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    !call MPI_Comm_dup(esmf_comm, dflow_comm, ierr)
    ! Duplicate the MPI communicator not to interfere with ESMF communications.
    ! The duplicate MPI communicator can be used in any MPI call in the user
    ! code. Here the MPI_Barrier() routine is called.
    !call MPI_Barrier(dflow_comm, ierr)
    !Initialize adcirc before setting up fields
    !WRITE(*,*) "Finished initalizing communicator, entering init"

    ! Beheen - at this time timeStep=3600 comes from reading nems.configure  
    ! EARTH_GRID_COMP SetRunSequence happens prior to P1
    ! Initialize mesh, get data needed for nuopc
    ! from initialization and save it in internal state variable
    call NWM_HYC_Init(vm, clock, rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out

    is%wrap%NStateImp(1) = importState
    is%wrap%NStateExp(1) = exportState

    ! Advertise import and export fields
    do fIndex = 1, size(NWM_HYCFieldList)
      tmpName = trim(NWM_HYCFieldList(fIndex)%stdname)
      if (NWM_HYCFieldList(fIndex)%adImport) then
        !if(trim(NWM_HYCFieldList(fIndex)%stdname)=="sea_surface_height_above_sea_level") then
        !if(trim(NWM_HYCFieldList(fIndex)%stdname)=="flow_rate") then
        !call NUOPC_Advertise(is%wrap%NStateImp(1), &
        !  standardName=trim(NWM_HYCFieldList(fIndex)%stdname), &
        !  name=trim(NWM_HYCFieldList(fIndex)%stdname), &
        !  TransferOfferGeomObject="cannot provide", &
        !  rc=rc)
        !print *, "Adding import: ", tmpName, fIndex
        !if (ESMF_STDERRORCHECK(rc)) return  ! bail out
        !else
        call NUOPC_Advertise(is%wrap%NStateImp(1), &
          standardName=trim(NWM_HYCFieldList(fIndex)%stdname), &
          name=trim(NWM_HYCFieldList(fIndex)%stdname), &
          rc=rc)
        !print *, "Adding import: ", tmpName, fIndex
        if (ESMF_STDERRORCHECK(rc)) return  ! bail out
        !endif
      endif

      if (NWM_HYCFieldList(fIndex)%adExport) then
        call NUOPC_Advertise(is%wrap%NStateExp(1), &
          standardName=trim(NWM_HYCFieldList(fIndex)%stdname), &
          name=trim(NWM_HYCFieldList(fIndex)%stdname), &
          rc=rc)
        print *, "Adding export: ", tmpName, fIndex
        if (ESMF_STDERRORCHECK(rc)) return  ! bail out
      endif
    enddo


#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": leaving "//METHOD, ESMF_LOGMSG_INFO)
#endif

  end subroutine

  !-----------------------------------------------------------------------------

#undef METHOD
#define METHOD "InitializeP3"
  !! IPDv03 - P3 model/mediator/driver realize their "connected" 
  !! import and export Fields that have TransferActionGeomObject 
  !! equal to "provide".
  !!
  !! During initialize phase 3 import and export fields are
  !! realized, if they are connected through NUOPC. 
  !! Realized fields are created on the mesh object. 

  subroutine InitializeP3(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)         :: gcomp
    type(ESMF_State)            :: importState, exportState
    type(ESMF_Clock)            :: clock
    integer, intent(out)        :: rc

    ! local variables
    character(32)              :: cname
    type(type_InternalState)   :: is
    type(ESMF_Mesh)            :: NWM_HYCMesh
    type(ESMF_LocStream)       :: DFlow_LocStream, link_locstream, lat_locstream, lon_locstream 
    type(ESMF_LocStream)       :: waterlevel_locstream, velx_locstream, vely_locstream, lateral_locstream
    type(ESMF_FieldBundle)     :: fieldbundle
    type(ESMF_Field)           :: field
    type(ESMF_VM)              :: vm
    logical                    :: importConnected, exportConnected
    integer                    :: fIndex
    integer                    :: localPet, petCount

    ! test
    real(ESMF_KIND_R8),pointer :: dataSF(:)
    real(ESMF_KIND_R8),pointer :: dataWL(:)
    real(ESMF_KIND_R8),pointer :: dataCoord(:)
    type(ESMF_Mesh)            :: mesh     
    ! end test


#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": entered "//METHOD, ESMF_LOGMSG_INFO)
#endif

    rc = ESMF_SUCCESS

    ! Query component for name
    call ESMF_GridCompGet(gcomp, name=cname, vm=vm, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    ! Query Component for its internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(gcomp, label_InternalState, is, rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    !> \details Get current ESMF VM.
    call ESMF_VMGetCurrent(vm, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! Get query local pet information for handeling global node information
    call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=rc)

    !NWM_HYCMesh = NWM_HYCMeshUGRIDCreate(rc=rc)          ! NWM_HYCMeshCreate(rc=rc)
    !call ESMF_LogWrite("testing NWM MESH U GRID CREATE", ESMF_LOGMSG_INFO)
    NWM_HYCMesh =  NWM_HYCMeshCreate(rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out
    !!$OMP CRITICAL

    ! call D-Flow VBC directional derivation to initalize geospatial attributes
    ! for the locstream current velocities to ingest
    call DFLOW_VBC_derivation(vm,rc)


    call locstream_data(vm, rc, DFLOW_locstream,link_locstream)
    !DFLOW_locstream = locstream_discharge( vm, rc)
    !link_locstream = locstream_link( vm, rc)
    !lat_locstream = locstream_lat( vm, rc)
    !lon_locstream = locstream_lon( vm, rc)

    waterlevel_locstream = locstream_waterlevel(localpet, rc)
    call locstream_lateral_discharge(vm, rc, lateral_locstream)
    velx_locstream = locstream_velx( vm, rc)
    vely_locstream = locstream_vely( vm, rc)



    !call ESMF_MeshWrite(NWM_HYCMesh, filename="Dflow_mesh.nc", rc=rc)
    !if(ESMF_STDERRORCHECK(rc)) return ! bail out

    !NWM_ReachStream = NWM_ReachStreamCreate(is%wrap%did, vm=vm, rc=rc)
    !if(ESMF_STDERRORCHECK(rc)) return ! bail out

    do fIndex = 1, size(NWM_HYCFieldList)
      call ESMF_LogWrite("Looping through Field List",ESMF_LOGMSG_INFO)
      !! the model checks to see if fields are connected via NUOPC_IsConnected.
      !! If the field is connected then the field is realized.
      !! The model doesn't realize all fields because it's wasteful
      if (NWM_HYCFieldList(fIndex)%adImport) then
        importConnected = NUOPC_IsConnected(is%wrap%NStateImp(1), &
                         fieldName=NWM_HYCFieldList(fIndex)%stdname)
      else
        importConnected = .FALSE.
      endif

      if (importConnected) then
        NWM_HYCFieldList(fIndex)%realizedImport = .TRUE.

        if(NWM_HYCFieldList(fIndex)%stdname .eq. 'flow_rate') then
            field = NWM_HYCFieldCreate(NWM_HYCFieldList(fIndex)%stdname, &
                                       mesh=NWM_HYCMesh,locstream=DFLOW_locstream,rc=rc)
            if (ESMF_STDERRORCHECK(rc)) return  ! bail out
              call NUOPC_Realize(is%wrap%NStateImp(1), field=field, rc=rc)
              if (ESMF_STDERRORCHECK(rc)) return
        elseif(NWM_HYCFieldList(fIndex)%stdname .eq. 'lateral_flow_rate') then
            field = NWM_HYCFieldCreate(NWM_HYCFieldList(fIndex)%stdname, &
                                       mesh=NWM_HYCMesh,locstream=lateral_locstream,rc=rc)
            if (ESMF_STDERRORCHECK(rc)) return  ! bail out
              call NUOPC_Realize(is%wrap%NStateImp(1), field=field, rc=rc)
              if (ESMF_STDERRORCHECK(rc)) return
        elseif(NWM_HYCFieldList(fIndex)%stdname .eq. 'link') then
            field = NWM_HYCFieldCreate(NWM_HYCFieldList(fIndex)%stdname, &
                                       mesh=NWM_HYCMesh,locstream=link_locstream,rc=rc)
            if (ESMF_STDERRORCHECK(rc)) return  ! bail out
              call NUOPC_Realize(is%wrap%NStateImp(1), field=field, rc=rc)
              if (ESMF_STDERRORCHECK(rc)) return
        !elseif(NWM_HYCFieldList(fIndex)%stdname .eq. 'lat') then
        !    field = NWM_HYCFieldCreate(NWM_HYCFieldList(fIndex)%stdname, &
        !                               mesh=NWM_HYCMesh,locstream=lat_locstream,rc=rc)
        !    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
        !      call NUOPC_Realize(is%wrap%NStateImp(1), field=field, rc=rc)
        !      if (ESMF_STDERRORCHECK(rc)) return
        !elseif(NWM_HYCFieldList(fIndex)%stdname .eq. 'lon') then
        !    field = NWM_HYCFieldCreate(NWM_HYCFieldList(fIndex)%stdname, &
        !                               mesh=NWM_HYCMesh,locstream=lon_locstream,rc=rc)
        !    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
        !      call NUOPC_Realize(is%wrap%NStateImp(1), field=field, rc=rc)
        !      if (ESMF_STDERRORCHECK(rc)) return
        elseif(NWM_HYCFieldList(fIndex)%stdname .eq. 'sea_surface_height_above_sea_level_dflow') then
            field = NWM_HYCFieldCreate(NWM_HYCFieldList(fIndex)%stdname, &
                                       mesh=NWM_HYCMesh,locstream=waterlevel_locstream,rc=rc)
            if (ESMF_STDERRORCHECK(rc)) return  ! bail out
              call NUOPC_Realize(is%wrap%NStateImp(1), field=field, rc=rc)
              !call ESMF_StateReconcile(is%wrap%NStateImp(1),vm,rc=rc)
              if (ESMF_STDERRORCHECK(rc)) return
        elseif(NWM_HYCFieldList(fIndex)%stdname .eq. 'surface_eastward_sea_water_velocity_dflow') then
            field = NWM_HYCFieldCreate(NWM_HYCFieldList(fIndex)%stdname, &
                                       mesh=NWM_HYCMesh,locstream=velx_locstream,rc=rc)
            if (ESMF_STDERRORCHECK(rc)) return  ! bail out
              call NUOPC_Realize(is%wrap%NStateImp(1), field=field, rc=rc)
              if (ESMF_STDERRORCHECK(rc)) return
        elseif(NWM_HYCFieldList(fIndex)%stdname .eq. 'surface_northward_sea_water_velocity_dflow') then
            field = NWM_HYCFieldCreate(NWM_HYCFieldList(fIndex)%stdname, &
                                       mesh=NWM_HYCMesh,locstream=vely_locstream,rc=rc)
            if (ESMF_STDERRORCHECK(rc)) return  ! bail out
              call NUOPC_Realize(is%wrap%NStateImp(1), field=field, rc=rc)
              if (ESMF_STDERRORCHECK(rc)) return
        elseif(NWM_HYCFieldList(fIndex)%stdname .eq. 'eastward_wave_radiation_stress') then
            field = NWM_HYCFieldCreate(NWM_HYCFieldList(fIndex)%stdname, &
                                       mesh=NWM_HYCMesh,locstream=waterlevel_locstream,rc=rc)
            if (ESMF_STDERRORCHECK(rc)) return  ! bail out
              call NUOPC_Realize(is%wrap%NStateImp(1), field=field, rc=rc)
              if (ESMF_STDERRORCHECK(rc)) return

        endif

      elseif(NWM_HYCFieldList(fIndex)%adImport) then
        print*, "Beheen - import not connected"
        call ESMF_StateRemove(is%wrap%NStateImp(1), &
           (/trim(NWM_HYCFieldList(fIndex)%stdname)/), &
                          relaxedflag=.true.,rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return
      endif

      ! export fields
      if (NWM_HYCFieldList(fIndex)%adExport) then
        print*, NWM_HYCFieldList(fIndex)%stdname
        print*, "CONNECTED?"
        exportConnected = NUOPC_IsConnected(is%wrap%NStateExp(1), &
                        fieldName=NWM_HYCFieldList(fIndex)%stdname)
        print*, exportConnected
      else
        exportConnected = .FALSE.
      endif

      if (exportConnected) then
        NWM_HYCFieldList(fIndex)%realizedExport = .TRUE.
        if(NWM_HYCFieldList(fIndex)%stdname .eq. 'flow_rate') then
            call ESMF_LogWrite("D-Flow streamflow export connected",ESMF_LOGMSG_INFO)
            field = NWM_HYCFieldCreate(NWM_HYCFieldList(fIndex)%stdname, &
                                       mesh=NWM_HYCMesh,locstream=DFLOW_locstream,rc=rc)
            if (ESMF_STDERRORCHECK(rc)) return  ! bail out
              call NUOPC_Realize(is%wrap%NStateExp(1), field=field, rc=rc)
              if (ESMF_STDERRORCHECK(rc)) return
        endif
        if(NWM_HYCFieldList(fIndex)%stdname .eq. 'sea_surface_height_above_sea_level_dflow') then
            call ESMF_LogWrite("D-Flow waterlevel export connected",ESMF_LOGMSG_INFO)
            field = NWM_HYCFieldCreate(NWM_HYCFieldList(fIndex)%stdname, &
                                       mesh=NWM_HYCMesh,locstream=waterlevel_locstream,rc=rc)
            if (ESMF_STDERRORCHECK(rc)) return  ! bail out
              call NUOPC_Realize(is%wrap%NStateExp(1), field=field, rc=rc)
              if (ESMF_STDERRORCHECK(rc)) return
        endif

      elseif(NWM_HYCFieldList(fIndex)%adExport) then
        print*, "Beheen - export not connected"
        call ESMF_StateRemove(is%wrap%NStateExp(1), &
        (/trim(NWM_HYCFieldList(fIndex)%stdname)/), &
                            relaxedflag=.true.,rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return
      endif

      ! TODO: Initialize the value in the pointer to 0 after proper restart is
      ! setup
      !if(associated(NWM_HYCFieldList(fIndex)%farrayPtr) )
      !NWM_HYCFieldList(fIndex)%farrayPtr = 0.0
      ! remove a not connected Field from State

    enddo
         
    !call ESMF_StateReconcile(is%wrap%NStateImp(1),vm,rc=rc)
    !!$OMP END CRITICAL


!    Model has initialized its own field memory so don't fill state.
!    call NUOPC_FillState(is%wrap%NStateImp(1),0,rc=rc)
!    if (ESMF_STDERRORCHECK(rc)) return
!    call NUOPC_FillState(is%wrap%NStateExp(1),0,rc=rc)
!    if (ESMF_STDERRORCHECK(rc)) return


#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": leaving "//METHOD, ESMF_LOGMSG_INFO)
#endif

  end subroutine

  !-----------------------------------------------------------------------------

#undef METHOD
#define METHOD "DataInitialize"

  !! During data initialize this cap checks the timestamp of all import fields
  !! dependent on a coupled model.  Once all dependent import fields have been
  !! initialized this cap is marked initalized.
  !!
  subroutine DataInitialize(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc

    ! local variables
    character(32)                          :: cname
    type(type_InternalState)               :: is
    type(ESMF_Clock)                       :: modelClock
    integer                                :: iIndex
    integer                                :: itemCount
    character(len=64),allocatable          :: itemNameList(:)
    type(ESMF_StateItem_Flag), allocatable :: itemTypeList(:)
    type(ESMF_Field)                       :: field
    type(ESMF_VM)                          :: vm
    integer                                :: stat

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": entered "//METHOD, ESMF_LOGMSG_INFO)
#endif

    rc = ESMF_SUCCESS

    !> \details Get current ESMF VM.
    call ESMF_VMGetCurrent(vm, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! Query component for name
    call ESMF_GridCompGet(gcomp, name=cname, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    ! Query Component for its internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(gcomp, label_InternalState, is, rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    

    ! Query the Component for its clock
    call NUOPC_ModelGet(gcomp, modelClock=modelClock, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    ! Initialize import and export fields
    ! No initialization. Fields remain set to initial value

    call ESMF_StateGet(is%wrap%NStateImp(1),itemCount=itemCount, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return ! bail out

    allocate( &
      itemNameList(itemCount), &
      itemTypeList(itemCount), &
      stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg="Allocation of state item list memory failed.", &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_StateGet(is%wrap%NStateImp(1),itemNameList=itemNameList, &
                                        itemTypeList=itemTypeList,rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return

    do iIndex=1, itemCount
      if ( itemTypeList(iIndex) == ESMF_STATEITEM_FIELD) then
        call ESMF_StateGet(is%wrap%NStateImp(1),field=field, &
          itemName=itemNameList(iIndex),rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return
        call NUOPC_SetAttribute(field, name="Updated", value="true", rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return  ! bail out
        call ESMF_StateReconcile(is%wrap%NStateImp(1),vm,rc=rc)
      endif
    enddo

    deallocate(itemNameList, itemTypeList, stat=stat)
    if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
      msg="Deallocation of state item list memory failed.", &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out


    call ESMF_StateGet(is%wrap%NStateExp(1),itemCount=itemCount, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return ! bail out

    allocate( &
      itemNameList(itemCount), &
      itemTypeList(itemCount), &
      stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg="Allocation of state item list memory failed.", &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_StateGet(is%wrap%NStateExp(1),itemNameList=itemNameList, &
                                        itemTypeList=itemTypeList,rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return

    do iIndex=1, itemCount
      if ( itemTypeList(iIndex) == ESMF_STATEITEM_FIELD) then
        call ESMF_StateGet(is%wrap%NStateExp(1),field=field, &
          itemName=itemNameList(iIndex),rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return
        call NUOPC_SetAttribute(field, name="Updated", value="true", rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return  ! bail out
      endif
    enddo

    deallocate(itemNameList, itemTypeList, stat=stat)
    if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
      msg="Deallocation of state item list memory failed.", &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! ESMF State Reconcile to obtain a common view of all objects contained in
    ! ESMF import and and export state for the model component
    !call ESMF_StateReconcile(is%wrap%NStateImp(1),vm,rc=rc)
    !call ESMF_StateReconcile(is%wrap%NStateExp(1),vm,rc=rc)


    ! set InitializeDataComplete Attribute to "true", indicating to the
    ! generic code that all inter-model data dependencies are satisfied
    call NUOPC_CompAttributeSet(gcomp, name="InitializeDataComplete", value="true", rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    !if (is%wrap%verbosity >= VERBOSITY_LV3) &
    !  call NWM_FieldListLog(label=trim(cname)) ! TO DO

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": leaving "//METHOD, ESMF_LOGMSG_INFO)
#endif

  end subroutine 

  !-----------------------------------------------------------------------------
  !! During set clock the cap creates a new clock using the timestep configured
  !! in the NWM configuration file. The restart write time step is also 
  !! created and the restart write time accumulation tracker is reset to zero.
  !! NOTE:
  !! setClock timeStep=3600s is not a divisor of runDuration=30s
  !! stepCount = 5 = NTIME and stepTime = 30.00 = noah_timestep 

#undef METHOD
#define METHOD "SetClock"

  subroutine SetClock(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc

    ! local variables
    character(32)              :: cname
    type(type_InternalState)   :: is
    integer                    :: dt           ! timestep in seconds
    type(ESMF_Clock)           :: modelclock
    type(ESMF_TimeInterval)    :: timestep     ! in/out
    type(ESMF_Time)            :: starttime
    type(ESMF_Time)            :: stoptime
    

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": entered "//METHOD, ESMF_LOGMSG_INFO)
#endif

    rc = ESMF_SUCCESS

    ! Query component for name
    call ESMF_GridCompGet(gcomp, name=cname, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    ! Query component for its internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(gcomp, label_InternalState, is, rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    ! Query component for its clock
    call NUOPC_ModelGet(gcomp, modelClock=modelclock, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    ! Query model clock for its timestep
    call ESMF_ClockGet(modelclock, timeStep=timestep, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    ! Query driver timestep for seconds 
    call ESMF_TimeIntervalGet(timestep,s=dt,rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    
    ! override timestep
    if (is%wrap%timeStepInt /= 0) then
        call ESMF_TimeIntervalSet(timestep, s=is%wrap%timeStepInt, rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return  ! bail out

        !call NWM_SetTimestep(real(is%wrap%timeStepInt),rc)
        !if (ESMF_STDERRORCHECK(rc)) return  ! bail out

        call ESMF_ClockSet(modelClock, timeStep=timestep, rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    else
        print*, "Later - todo"
        !call NWM_SetTimestep(real(dt),rc)
        !if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    endif

    ! Initialize and set the internal Clock of a GridComp,
    ! could be any external clock as well.
    call NUOPC_CompSetClock(gcomp, modelclock, timestep, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    ! Save model clock in internal state
    is%wrap%clock(1) = modelclock

    ! Reset Timer
    call ESMF_TimeIntervalSet(is%wrap%stepTimer(1), &
                         s_r8=0._ESMF_KIND_R8, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out


#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": leaving "//METHOD, ESMF_LOGMSG_INFO)
#endif

  end subroutine


  !-----------------------------------------------------------------------------
  !! During check import the import data is checked to verify that it is at
  !! the beginning or end of the timestep. By default the current time is 
  !! checked for each field and if it doesn't match the current time of the
  !! model advance then the model fails, overriding this default check.
  !! This happens before each model advance phase.
  !-----------------------------------------------------------------------------
#undef METHOD
#define METHOD "CheckImport"

subroutine CheckImport(gcomp, rc)
    type(ESMF_GridComp) :: gcomp
    integer,intent(out) :: rc

    ! local variables
    character(32)               :: cname
    type(type_InternalState)    :: is
    integer                     :: nIndex
    character(len=10)           :: sStr
    type(ESMF_Clock)            :: modelClock
    type(ESMF_Time)             :: modelCurrTime
    logical                     :: allCurrTime

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": entered "//METHOD, ESMF_LOGMSG_INFO)
#endif

    rc = ESMF_SUCCESS

    ! Query component for name
    call ESMF_GridCompGet(gcomp, name=cname, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    ! Query Component for its internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(gcomp, label_InternalState, is, rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    ! Query the Component for its clock
    call NUOPC_ModelGet(gcomp, modelClock=modelClock, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    ! Get the curr time out of the clock
    call ESMF_ClockGet(modelClock, currTime=modelCurrTime, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
 
    ! Check that Fields in the importState show correct timestamp
    allCurrTime = NUOPC_IsAtTime(is%wrap%NStateImp(1), modelCurrTime, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    WRITE(*,*) "Time Check"
    WRITE(*,*) allCurrTime
    if (.not.allCurrTime) then
      call ESMF_LogWrite(trim(cname)//": DFLOW NUOPC INCOMPATIBILITY DETECTED: "// &
        "Import Fields not at correct time", &
        ESMF_LOGMSG_WARNING)
    endif

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": leaving "//METHOD, ESMF_LOGMSG_INFO)
#endif

  end subroutine

  !-----------------------------------------------------------------------------
  !! Calls NWM advance for the configured domain per NWM noah timestep
  !-----------------------------------------------------------------------------
#undef METHOD
#define METHOD "ModelAdvance"

  subroutine ModelAdvance(gcomp, rc)

    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc

    ! local variables
    character(32)               :: cname
    type(type_InternalState)    :: is
    character(len=10)           :: sStr
    type(ESMF_Clock)            :: modelClock
    type(ESMF_State)           :: importState, exportState
    type(ESMF_Time)             :: currTime, advEndTime
    character(len=32)           :: currTimeStr, advEndTimeStr, values
    type(ESMF_TimeInterval)     :: timeStep, timeinterval
    integer(ESMF_KIND_I8)       :: advanceCount
    type(ESMF_VM)               :: vm
    type(ESMF_Field)                       :: field, field2
    type(ESMF_LocStream)                   :: locstream, locstream_NWM
    integer                                :: itemCount
    integer                                :: iIndex, unique_index, NWM_index, csv_index, NWM_loop, latq_index
    integer                                :: stat, x
    character(len=64),allocatable          :: itemNameList(:)
    type(ESMF_StateItem_Flag), allocatable :: itemTypeList(:)

    real(ESMF_KIND_R8) :: esmfstep, esmft, esmfnext

    ! imported variables
    real(ESMF_KIND_R8), pointer:: link(:), dataPtr_streamflow(:), lat(:), lon(:), NWM_lat(:), NWM_lon(:), NWM_link(:), waterlevel_ptr(:), velx_ptr(:), vely_ptr(:), lateral_ptr(:), lateral_id(:)
    real(ESMF_KIND_R8), dimension(:), allocatable :: latq_ids
    !integer(ESMF_KIND_I4), pointer :: link(:)
    !integer, pointer :: link(:)

    ! variables used to calculate D-Flow velocities from ADCIRC velocity
    ! components
    real(ESMF_KIND_R8), dimension(:), allocatable :: Distance, UTANx, UTANy, UNORx, UNORy, TanV, NorV
    integer :: vel_loop, dir_loop, HUC_count, NWM_count, velx_loop, vely_loop
#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": entered "//METHOD, ESMF_LOGMSG_INFO)
#endif

    rc = ESMF_SUCCESS

    ! Query component for name
    call ESMF_GridCompGet(gcomp, name=cname, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    
    call ESMF_GridCompGet(gcomp, vm=vm, rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out

    ! Query component for its internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(gcomp, label_InternalState, is, rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    is%wrap%timeSlice = is%wrap%timeSlice + 1   

    if (is%wrap%timeSlice > 999999999) then
      sStr = '999999999+'
    else
      write (sStr,"(I0)") is%wrap%timeSlice
    endif

    ! Query the component for its clock
    call NUOPC_ModelGet(gcomp,modelClock=modelClock,importState=importState,exportState=exportState, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out


    !> \details Get current ESMF VM.
    call ESMF_VMGetCurrent(vm, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! ESMF State Reconcile to obtain a common view of all objects contained in
    ! ESMF import and and export state for the model component
    !call ESMF_StateReconcile(importState,vm,rc=rc)
    !call ESMF_StateReconcile(exportState,vm,rc=rc)

    ! Query the clock for its current time and timestep
    call ESMF_ClockGet(modelClock, currTime=currTime, timeStep=timeStep, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    advEndTime = currTime + timeStep

    call ESMF_TimeGet(currTime, timeString=currTimeStr, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    call ESMF_TimeGet(advEndTime, timeString=advEndTimeStr, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
        
    !WRITE(*,*) is%wrap%NStateImp(1)
    ! Write import files here
    !call ESMF_StateWrite(is%wrap%NStateImp(1), 'dflowfmimport.nc', rc)
    !call NUOPC_Write(is%wrap%NStateImp(1), fileNamePrefix='dflowfmimport.nc', rc=rc)
    is%wrap%stepTimer(1) = is%wrap%stepTimer(1) + timeStep

    ! below from here using timeStep of clock(1)
    call ESMF_ClockGet(is%wrap%clock(1),timeStep=timeStep, &
                       advanceCount=advanceCount, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out



    call ESMF_StateGet(is%wrap%NStateImp(1),itemCount=itemCount, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return ! bail out

    allocate( &
      itemNameList(itemCount), &
      itemTypeList(itemCount), &
      stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg="Allocation of state item list memory failed.", &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_StateGet(is%wrap%NStateImp(1),itemNameList=itemNameList, &
                                        itemTypeList=itemTypeList,rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return

    do iIndex=1, itemCount
      if ( itemTypeList(iIndex) == ESMF_STATEITEM_FIELD) then
        call ESMF_StateGet(is%wrap%NStateImp(1),field=field, &
          itemName=itemNameList(iIndex),rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return
        
        if(itemNameList(iIndex)=='flow_rate') then
                write(*,*) "Transferring NEMS discharge data"
                call State_getFldPtr(ST=importState,fldname='flow_rate',fldptr=dataPtr_streamflow,rc=rc)
                if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, &
                file=__FILE__)) &
                return  ! bail out

                
                if(nbndu > 0) then
                    !write(*,*) "discharge before NEMS"
                    !write(*,*) zbndq(1:5)
                    !zbndq(1:nbndu) = dataPtr_streamflow(1:nbndu)
                    write(*,*) "Discharge min and max"
                    write(*,*) minval(zbndq(1:nbndu))
                    write(*,*) maxval(zbndq(1:nbndu))
                endif

        elseif(itemNameList(iIndex)=='lateral_flow_rate') then
                !write(*,*) "Transferring NEMS lateral discharge data"
                call State_getFldPtr(ST=importState,fldname='lateral_flow_rate',fldptr=lateral_ptr,rc=rc)
                if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, &
                file=__FILE__)) &
                return  ! bail out

                ! Grab NWM common ID from locstream to use to aggregate NWM
                ! streamflow data based on HUC ID basis
                call ESMF_StateGet(importState, itemName='lateral_flow_rate', field=field2,rc=rc)
                if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
                call ESMF_FieldGet(field2, locstream=locstream, rc=rc)
                if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,line=__LINE__, file=__FILE__)) return

                call ESMF_LocStreamGetKey(locstream, "ESMF:Lat", farray=lateral_id,rc=rc)
                if (ESMF_STDERRORCHECK(rc)) return

                !! Reset NWM HUCS agg streamflow fields
                NWM_HUCS_agg_streamflow(:) = 0.0
                
                
                 do unique_index = 1, size(HUCS_unique_ID)
                    !WRITE(*,*) HUCS_unique_ID(unique_index)
                    HUC_count = 0
                    do csv_index = 1, size(HUCS_ID)
                        !WRITE(HUCS_ID(csv_index)
                        if(HUCS_ID(csv_index) .eq. HUCS_unique_ID(unique_index)) then
                           do NWM_loop = 1, size(lateral_id)
                              if(lateral_id(NWM_loop) .eq. NWM_commonID(csv_index)) then
                                 NWM_index = NWM_loop
                                 HUC_count = HUC_count + 1
                                 exit
                              endif
                           end do
                           !WRITE(*,*) "NWM index"
                           !WRITE(*,*) NWM_index
                           !write(*,*) "NWM Common IDS match"
                           !write(*,*) lat(NWM_index)
                           !write(*,*) NWM_commonID(csv_index)
                           !if(HUCS_unique_ID(unique_index) .eq. 30801010105.0)  then
                           !write(*,*) "HUCS unqiue ID NWM agg count"
                           !write(*,*) HUC_count
                           !write(*,*) "NWM ID and data"
                           !write(*,*) lateral_id(NWM_index)
                           !write(*,*) lateral_ptr(NWM_index)
                           !endif
                           NWM_HUCS_agg_streamflow(unique_index) = NWM_HUCS_agg_streamflow(unique_index) + lateral_ptr(NWM_index)
                        endif
                    !WRITE(*,*) "HUC ID agg count"
                    !WRITE(*,*) HUCS_unique_ID(unique_index)
                    !write(*,*) HUC_count
                    end do
                !if(HUCS_unique_ID(unique_index) .eq. 30801010105.0) then
                !write(*,*) "HUCS unique ID and latq agg"
                !write(*,*) HUCS_unique_ID(unique_index) 
                !write(*,*) NWM_HUCS_agg_streamflow(unique_index)
                !endif

                 !WRITE(*,*) "NWM streamflow HUCS agg"
                 !WRITE(*,*) NWM_HUCS_agg_streamflow(unique_index)
                 end do


                allocate(latq_ids(numlatsg))
                
                !write(*,*) "NWM count"
                !write(*,*) numlatsg
                NWM_count = 0
                if(numlatsg > 0) then
                    !write(*,*) "lateral discharge before NEMS"
                    !write(*,*) qplat(1:20)
                    !write(*,*) "qplat maxval"
                    !write(*,*) maxval(qplat)
                    do latq_index = 1, numlatsg
                       read(lat_ids(latq_index),*) latq_ids(latq_index)
                       inner : do unique_index = 1, size(HUCS_unique_ID)
                          if(latq_ids(latq_index) .eq. HUCS_unique_ID(unique_index)) then 
                             !qplat(latq_index) = NWM_HUCS_agg_streamflow(unique_index)
                             NWM_count = NWM_count + 1
                             if(qplat(latq_index) .eq. 0.00) then
                             write(*,*) "found lateral discharge with zero latq"
                             write(*,*) HUCS_unique_ID(unique_index)
                             write(*,*) NWM_HUCS_agg_streamflow(unique_index)
                             !write(*,*) qplat(latq_index)
                             endif
                             exit inner
                          end if
                       end do inner
                    end do
                    !WRITE(*,*) "HUCS IDS"
                    !WRITE(*,*) lat_ids(1:5)
                    !write(*,*) "lateral discharge after NEMS"
                    !write(*,*) maxval(qplat)
                    !write(*,*) minval(qplat)
                    !write(*,*) "maxval after NEMS"
                    !write(*,*) maxval(qplat)
                endif
                !write(*,*) "NWM count after agg"
                !write(*,*) NWM_count

                if(numlatsg > 0) then
                    !write(*,*) "lateral discharge ids"
                    !write(*,*) latq_ids(30:46)
                    !qplat(1:numlatsg) = lateral_ptr(1:numlatsg)
                    !write(*,*) "lateral discharge after NEMS"
                    !write(*,*) qplat(30:36)
                    !write(*,*) "lateral max and min values"
                    !write(*,*) maxval(qplat)
                    !write(*,*) minval(qplat)
                    write(*,*) "Lateral discharge min and max"
                    write(*,*) minval(qplat(1:numlatsg))
                    write(*,*) maxval(qplat(1:numlatsg))
                endif

                deallocate(latq_ids)  

        elseif(itemNameList(iIndex)=='sea_surface_height_above_sea_level_dflow') then
                write(*,*) "Transferring NEMS water level data"
                call State_getFldPtr(ST=importState,fldname='sea_surface_height_above_sea_level_dflow',fldptr=waterlevel_ptr,rc=rc)
                if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, &
                file=__FILE__)) &
                return  ! bail out


                call ESMF_StateGet(importState, itemName='sea_surface_height_above_sea_level_dflow', field=field,rc=rc)
                if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
                call ESMF_FieldGet(field, locstream=locstream, rc=rc)
                if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,line=__LINE__, file=__FILE__)) return

                call ESMF_LocStreamGetKey(locstream, "ESMF:Lat", farray=lat,rc=rc)
                if (ESMF_STDERRORCHECK(rc)) return

                call ESMF_LocStreamGetKey(locstream, "ESMF:Lon", farray=lon,rc=rc)
                if (ESMF_STDERRORCHECK(rc)) return

               if(nbndz > 0) then
               !     write(*,*) "waterlevels before NEMS"
               !     write(*,*) zbndz(1:nbndz)
                    !zbndz(1:nbndz) = waterlevel_ptr(1:nbndz)
                    !write(*,*) "waterlevel coordinates match?"
                    !write(*,*) ybndz(1:10)
                    !write(*,*) lat(1:10)
                    !write(*,*) "waterlevel IDS"
                    !write(*,*) idbndz(1:10)
               !     write(*,*) "D-Flow interp lats"
               !     write(*,*) lat(1:6)
               !     write(*,*) "D-Flow interp lons"
               !     write(*,*) lon(1:6)
                    write(*,*) "Waterlevel min and max"
                    write(*,*) minval(zbndz(1:nbndz))
                    write(*,*) maxval(zbndz(1:nbndz))
                endif

        elseif(itemNameList(iIndex)=='surface_eastward_sea_water_velocity_dflow') then
                write(*,*) "Transferring NEMS velx data"
                call State_getFldPtr(ST=importState,fldname='surface_eastward_sea_water_velocity_dflow',fldptr=velx_ptr,rc=rc)
                if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, &
                file=__FILE__)) &
                return  ! bail out

                ! Grab bnd.pli coordinates from locstream to calculate distances
                ! between vertices along D-Flow offshore boundaries
                call ESMF_StateGet(importState, itemName='surface_eastward_sea_water_velocity_dflow', field=field,rc=rc)
                if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
                call ESMF_FieldGet(field, locstream=locstream, rc=rc)
                if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,line=__LINE__, file=__FILE__)) return

                call ESMF_LocStreamGetKey(locstream, "ESMF:Lat", farray=lat,rc=rc)
                if (ESMF_STDERRORCHECK(rc)) return

                call ESMF_LocStreamGetKey(locstream, "ESMF:Lon", farray=lon,rc=rc)
                if (ESMF_STDERRORCHECK(rc)) return


        elseif(itemNameList(iIndex)=='surface_northward_sea_water_velocity_dflow') then
                write(*,*) "Transferring NEMS vely data"
                call State_getFldPtr(ST=importState,fldname='surface_northward_sea_water_velocity_dflow',fldptr=vely_ptr,rc=rc)
                if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, &
                file=__FILE__)) &
                return  ! bail out

                ! Grab bnd.pli coordinates from locstream to calculate distances
                ! between vertices along D-Flow offshore boundaries
                call ESMF_StateGet(importState, itemName='surface_northward_sea_water_velocity_dflow', field=field,rc=rc)
                if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
                call ESMF_FieldGet(field, locstream=locstream, rc=rc)
                if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,line=__LINE__, file=__FILE__)) return

                call ESMF_LocStreamGetKey(locstream, "ESMF:Lat", farray=lat,rc=rc)
                if (ESMF_STDERRORCHECK(rc)) return

                call ESMF_LocStreamGetKey(locstream, "ESMF:Lon", farray=lon,rc=rc)
                if (ESMF_STDERRORCHECK(rc)) return

        endif
      endif
    enddo

    deallocate(itemNameList, itemTypeList, stat=stat)
    if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
      msg="Deallocation of state item list memory failed.", &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out


    !If D-Flow cap is connected with ADCIRC and recieved velocity components,
    !then add them sequentially to the D-Flow boundary array for advection
    !velocity
    if(nbnduxy > 0) then
    ! set index locations of velocity vectors
    velx_loop = 1
    vely_loop = 2
    do vel_loop = 1, nbnduxy/2
    zbnduxy(velx_loop) = velx_ptr(velx_loop)
    zbnduxy(vely_loop) = vely_ptr(vely_loop)
    velx_loop = velx_loop + 2
    vely_loop = vely_loop + 2
    enddo
    !WRITE(*,*) "zbnduxy product"
    !WRITE(*,*) zbnduxy(1:5)
    endif

    ! If D-Flow cap is connected with ADCIRC and recieved velocity components,
    ! calculate normal and tangenetial velocity
    if(nbndn > 0) then
       ! Allocate arrays to calculate Normal and Tangential velocities
       !allocate(Distance(nbndn))
       !allocate(UTANx(nbndn))
       !allocate(UTANy(nbndn))
       !allocate(UNORx(nbndn))
       !allocate(UNORy(nbndn))
       allocate(TanV(nbndn))
       allocate(NorV(nbndn))
       ! After extracting the x and y velocity components from ADCIRC, we need to
       ! first calculate the distance between two consecutive vertices along the
       ! offshort boundaries
       !do vel_loop = 1, nbndn-1
       !   Distance(vel_loop) = SQRT((lat(vel_loop+1)+lat(vel_loop))**2+(lon(vel_loop+1)+lon(vel_loop))**2)
       !end do
       !Distance(nbndn) = Distance(nbndn-1)
       ! Calculate the UTAN and UNOR components       
       !do vel_loop = 1, nbndn-1
       !   UTANx(vel_loop) = (lon(vel_loop+1) - lon(vel_loop))/Distance(vel_loop)
       !   UTANy(vel_loop) = (lat(vel_loop+1) - lat(vel_loop))/Distance(vel_loop)
       !end do
       !UTANx(nbndn) = UTANx(nbndn-1)
       !UTANy(nbndn) = UTANy(nbndn-1)
   
       !UNORx(:) = UTANy*-1.0
       !UNORy(:) = UTANx

       ! Calculate the Normal and Tangential velocity
       vel : do vel_loop = 1, nbndn
          dir : do dir_loop = 1, size(DFLOW_VBC_UTANx)
             if(ybndt(vel_loop) .eq. DFLOW_VBC_lat(dir_loop)) then
                TanV(vel_loop) = (DFLOW_VBC_UTANx(dir_loop) * velx_ptr(dir_loop)) + (DFLOW_VBC_UTANy(dir_loop) * vely_ptr(dir_loop))
                NorV(vel_loop) = (DFLOW_VBC_UNORx(dir_loop) * velx_ptr(dir_loop)) + (DFLOW_VBC_UNORy(dir_loop) * vely_ptr(dir_loop))
                exit dir
             endif
          enddo dir
        enddo vel
       !Assign latest velocity boundary condition data from ADCIRC to D-Flow
       
       ! Normal Velocity
       !write(*,*) "Normal Velocity before NEMS connection"
       !write(*,*) zbndn(1:10)
       zbndn(1:nbndn) = NorV(1:nbndn)
       !write(*,*) "Velocity IDs"
       !write(*,*) idbndz(1:10)
       !write(*,*) "Normal Velocity after NEMS connection"
       !write(*,*) zbndn(1:10)
       !write(*,*) "ADCIRC velx"
       !write(*,*) velx_ptr(1:10)
       !write(*,*) "ADCIRC vely"
       !write(*,*) vely_ptr(1:10)
       !write(*,*) "velocity lat"
       !write(*,*) ybndt(1:10)
       !write(*,*) "waterlevel lat"
       !write(*,*) ybndz(1:10)

       ! Tangential Velocity
       !write(*,*) "Tangential Velocity before NEMS connection"
       !write(*,*) zbndt(1:2)
       zbndt(1:nbndt) = TanV(1:nbndt)
       !write(*,*) "Tangential Velocity after NEMS connection"
       !write(*,*) zbndt(1:20)

       write(*,*) "Velocity components min and max"
       write(*,*) minval(zbndt(1:nbndt))
       write(*,*) maxval(zbndt(1:nbndt))
       write(*,*) minval(zbndn(1:nbndn))
       write(*,*) maxval(zbndn(1:nbndn))
       ! Deallocate arrays after velocity calculations
       !deallocate(Distance)
       !deallocate(UTANx)
       !deallocate(UTANy)
       !deallocate(UNORx)
       !deallocate(UNORy)
       deallocate(TanV)
       deallocate(NorV)

    endif

    ! Call and assign imported NWM discharge boundary conditions and 
    ! assign it to global array in DFlow

    !call NUOPC_Realize(is%wrap%NStateImp(1),fieldName='flow_rate', field=field, rc=rc)
    !call ESMF_FieldGet(field, farrayPtr=dataPtr_streamflow, rc=rc)
    !-----------------------------------------
    ! <<<<< RECEIVE and UN-PACK SYY

    !!$OMP CRITICAL
    !call State_getFldPtr(ST=importState,fldname='flow_rate',fldptr=dataPtr_streamflow,rc=rc)
    !if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    !   line=__LINE__, &
    !   file=__FILE__)) &
    !   return  ! bail out

    !call State_getFldPtr(ST=importState,fldname='link',fldptr=link,rc=rc)
    !if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    !   line=__LINE__, &
    !   file=__FILE__)) &
    !   return  ! bail out

    !call State_getFldPtr(ST=importState,fldname='lat',fldptr=lat,rc=rc)
    !if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    !   line=__LINE__, &
    !   file=__FILE__)) &
    !   return  ! bail out


    !call State_getFldPtr(ST=importState,fldname='lon',fldptr=lon,rc=rc)
    !if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    !   line=__LINE__, &
    !   file=__FILE__)) &
    !   return  ! bail out


    !call State_getFldPtr(ST=importState,fldname='sea_surface_height_above_sea_level',fldptr=waterlevel_ptr,rc=rc)
    !if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    !   line=__LINE__, &
    !   file=__FILE__)) &
    !   return  ! bail out

    !call State_getFldPtr(ST=importState,fldname='surface_eastward_sea_water_velocity',fldptr=velx_ptr,rc=rc)
    !if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    !   line=__LINE__, &
    !   file=__FILE__)) &
   !   return  ! bail out

    !call State_getFldPtr(ST=importState,fldname='surface_northward_sea_water_velocity',fldptr=vely_ptr,rc=rc)
    !if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    !   line=__LINE__, &
    !   file=__FILE__)) &
    !   return  ! bail out

    !call State_getFldPtr(ST=importState,fldname='eastward_wave_radiation_stress',fldptr=sxx_ptr,rc=rc)
    !if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    !   line=__LINE__, &
    !   file=__FILE__)) &
    !   return  ! bail out





    !call ESMF_StateGet(importState, itemName='lateral_flow_rate', field=field2, rc=rc)
    !if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
    !call ESMF_FieldGet(field2, locstream=locstream, rc=rc)
    !if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,line=__LINE__, file=__FILE__)) return

    !call ESMF_LocStreamGetKey(locstream, "ESMF:Lat", farray=lat,rc=rc)
    !if (ESMF_STDERRORCHECK(rc)) return

    !call ESMF_LocStreamGetKey(locstream, "ESMF:Lon", farray=lon,rc=rc)
    !if (ESMF_STDERRORCHECK(rc)) return

    !if(nbndu > 0) then
    !    zbndq(1:nbndu) = dataPtr_streamflow(1:nbndu)
    !endif

    !if(nbndz > 0) then
    !    zbndz(1:nbndz) = waterlevel_ptr(1:nbndz)
    !endif

     !if(nbndu > 0) then
     !if(nbndz > 0) then
     !   WRITE(*,*) "NWM bc baseline"
     !   WRITE(*,*) zbndz(1)
     !   WRITE(*,*) zbndq(1:nbndu)
    !WRITE(*,*) maxval(dataPtr_streamflow)
    !Reassign imported streamflwo data to global array
    !zbndz(1:nbndz) = waterlevel_ptr(1:nbndz)
    !zbndq(1:nbndu) = dataPtr_streamflow(1:nbndu)
    !zbndq(1:nbndu) = 0.0
      !  WRITE(*,*) "NEMS waterlevel"
      !  WRITE(*,*)  waterlevel_ptr(1)
     !   WRITE(*,*) "NEMS  streamflow"
     !   WRITE(*,*) dataPtr_streamflow
     !   do x = 1, nbndu
     !   if(link(x) - lat(x) .ne. 0.0) then
     !   WRITE(*,*) "Issue found between NWM DFLOW regridding"
     !   WRITE(*,*) "NWM Link ID corresponing to bc"
     !   WRITE(*,*) link(x)
     !   WRITE(*,*) "NEMS Link ID"
     !   WRITE(*,*) lat(x)
     !   WRITE(*,*) "NWM data"
     !   WRITE(*,*) zbndq(x)
     !   WRITE(*,*) "NEMS data"
     !   WRITE(*,*) dataPtr_streamflow(x)
     !   end if
     !   end do
     !   WRITE(*,*) idbndu(1:nbndu)
     !   WRITE(*,*) "NEMS coordinates"
     !   WRITE(*,*) lat(1:nbndz)
     !   WRITE(*,*) "NEMS ids"
     !   WRITE(*,*) idbndz(1:nbndz)
        !WRITE(*,*) lon(1:nbndu)
        !WRITE(*,*) "END"
        !WRITE(*,*) "NWM lon coordinates"
        !WRITE(*,*) lon(1:nbndu)
        !WRITE(*,*) "DFlow lat coordinates"
        !WRITE(*,*) ybndu(1:nbndu)
        !WRITE(*,*) "DFlow lon coordinates"
        !WRITE(*,*) xbndu(1:nbndu)

        !WRITE(*,*) "DFLOW waterlevel"
        !WRITE(*,*) zbndz(:)
        !WRITE(*,*) "ADCIRC waterlevel"
        !WRITE(*,*) waterlevel_ptr(:)
        !WRITE(*,*) "ADCIRC current velocity x"
        !WRITE(*,*) velx_ptr(:)
        !WRITE(*,*) "ADCIRC current velocity y"
        !WRITE(*,*) vely_ptr(:)
        !WRITE(*,*) "WW3 eastward strees"
        !WRITE(*,*) sxx_ptr(:)

    !endif

    !WRITE(*,*) "After"
    !WRITE(*,*) q1(100)


    ! this is same as itime = 1 hour for nwm exec method
    if ( MOD(advanceCount, 24) .eq. 0) then
      is%wrap%timeStepInt = is%wrap%timeStepInt + 1
    endif
   
    ! Synchronize the clocks

    ! ESMF clock
    !       ESMFT -----> ESMFNEXT
    !         .             .
    !          <-ESMFSTEP-->

    ! DFLOW clock
    !     time_user --> tstop_user

    ! Get the simulation time (since tref) from esmf
    call ESMF_ClockGet(modelclock, currSimTime=timeinterval, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    call ESMF_TimeIntervalGet(timeinterval, s_r8=esmft, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    ! Get the timestep from esmf, the coupled timestep
    call ESMF_ClockGet(modelclock, timestep=timeinterval, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    call ESMF_TimeIntervalGet(timeinterval, s_r8=esmfstep, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out


    ! Run until we reach esmfnext
    !esmfnext = (esmft+esmfstep)
    esmfnext = (tstart_user+esmfstep+esmft)
    !if(nbndu > 0) then
    !    WRITE(*,*) "BEFORE"
   !    WRITE(*,*) zbndq(1:nbndu)
    !endif
    !do while (is%wrap%stepTimer(1) >= timeStep .and. jastop.eq.0 ) ! time loop ! run
    call NWM_HYC_Run(esmfnext,is%wrap%NStateImp(1),is%wrap%NStateExp(1),rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out


               if(nbndz > 0) then
                    write(*,*) "waterlevel min max after NEMS"
                    write(*,*) maxval(zbndz(1:nbndz))
                    write(*,*) minval(zbndz(1:nbndz))
               endif

    !do x=1, numsrc
    !if(qstss(x) - lateral_ptr(x) .ne. 0.0) then
    !write(*,*) "qstss mismatch"
    !WRITE(*,*) "qplat after model time step"
    !WRITE(*,*) qplat(x)
    !WRITE(*,*) qstss(1:4)
    !WRITE(*,*) "NEMS qplat"
    !WRITE(*,*) lateral_ptr(1:4)
    !WRITE(*,*) "qplat pli"
    !WRITE(*,*) srcname(1:4)
    !WRITE(*,*) "qsrc"
    !WRITE(*,*) qsrc(1:4)
    !endif
    !enddo
    !if(numlatsg > 0) then
    !write(*,*) "qplat"
    !write(*,*) qplat(1:3)
    !write(*,*) "balat"
    !write(*,*) balat(1:3)
    !write(*,*) "q ids"
    !write(*,*) lat_ids(1:3)
    !endif
    !if(nbndu > 0) then
    !    do x = 1, nbndu
    !    if(dataPtr_streamflow(x) .eq. 0.0) then
    !    if(zbndq(x) - dataPtr_streamflow(x) .ne. 0.0) then
    !    WRITE(*,*) "Issue found between NWM DFLOW regridding"
    !    WRITE(*,*) "NWM Link ID corresponing to bc"
    !    WRITE(*,*) link(x)
        !WRITE(*,*) "NEMS Link ID"
    !    WRITE(*,*) lat(x)
    !    !WRITE(*,*) "lat"
    !    !WRITE(*,*) ybndu(x)
    !    !WRITE(*,*) "lon"
    !    !WRITE(*,*) xbndu(x)
    !    WRITE(*,*) "NWM data"
    !    WRITE(*,*) zbndq(x)
    !    WRITE(*,*) "NEMS data"
    !    WRITE(*,*) dataPtr_streamflow(x)
    !    end if

    !    if(zbndq(x) - dataPtr_streamflow(x) .eq. 0.0 .and. link(x) .eq. 9064392.0) then
    !    write(*,*) "Link 9064392 has already been used!"
    !    endif
    !    end do
    !endif
    !write(*,*) "esmf timestep"
    !write(*,*) esmfnext
    !if(nbndz > 0) then
    !    do x = 1, nbndz
        !if(abs(zbndz(x) - waterlevel_ptr(x))/zbndz(x)*100 .gt. 2.0) then
    !    if(trim(idbndz(x)) .eq. 'CONT_BND_0052' .OR. trim(idbndz(x)) .eq. 'CONT_BND_0255' .OR. trim(idbndz(x)) .eq. 'CONT_BND_1180') then
    !    WRITE(*,*) "Issue found between NWM DFLOW regridding"
    !    WRITE(*,*) "pli id"
    !    WRITE(*,*) trim(idbndz(x))
    !    WRITE(*,*) "D-Flow lat"
    !    WRITE(*,*) ybndz(x)
    !    WRITE(*,*) "D-Flow lon"
    !    WRITE(*,*) xbndz(x)
    !    WRITE(*,*) "NEMS lat"
    !    WRITE(*,*) lat(x)
    !    WRITE(*,*) "NEMS lon"
    !    WRITE(*,*) lon(x)
    !    WRITE(*,*) "NWM data"
    !    WRITE(*,*) zbndz(x)
    !    WRITE(*,*) "NEMS data"
    !    WRITE(*,*) waterlevel_ptr(x)
        !WRITE(*,*) "NEMS lat data"
        !WRITE(*,*) lat
        !WRITE(*,*) "NEMS pli data"
        !WRITE(*,*) idbndz
    !    end if
    !    if(waterlevel_ptr(x) .eq. 0.0) then
    !    write(*,*) "NEMS waterlevel ptr is 0.0!!!"
    !    write(*,*) waterlevel_ptr(x)
    !    endif

    !    if(zbndq(x) - dataPtr_streamflow(x) .eq. 0.0 .and. link(x) .eq.
    !    9064392.0) then
    !    write(*,*) "Link 9064392 has already been used!"
    !    endif
    !    end do
    !endif


    !!$OMP END CRITICAL

     ! is%wrap%stepTimer(1) = is%wrap%stepTimer(1) - timeStep
    !enddo

    ! Write export files here
    !call ESMF_StateWrite(is%wrap%NStateExp(1), 'dflowfmexport.nc', rc)


#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": leaving "//METHOD, ESMF_LOGMSG_INFO)
#endif

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine unstruc_run(gridcomp, importState, exportState, clock, rc)
    use unstruc_api

    type(ESMF_GridComp) :: gridcomp
    type(ESMF_State)    :: importState, exportState
    type(ESMF_Clock)    :: clock
    integer, intent(out)               :: rc

    type(ESMF_TimeInterval) :: timeinterval
    real(ESMF_KIND_R8) :: esmfstep, esmft, esmfnext

    integer :: jastop, iresult ! uggh

    call ESMF_StateWrite(importState, 'dflowfmimport.nc', rc)


    ! Synchronize the clocks

    ! ESMF clock
    !       ESMFT -----> ESMFNEXT
    !         .             .
    !          <-ESMFSTEP-->

    ! DFLOW clock
    !     time_user --> tstop_user

    ! Get the simulation time (since tref) from esmf
    call ESMF_ClockGet(clock, currSimTime=timeinterval, rc=rc)
    call ESMF_TimeIntervalGet(timeinterval, s_r8=esmft, rc=rc)

    ! Get the timestep from esmf, the coupled timestep
    call ESMF_ClockGet(clock, timestep=timeinterval, rc=rc)
    call ESMF_TimeIntervalGet(timeinterval, s_r8=esmfstep, rc=rc)


    ! Run until we reach esmfnext
    esmfnext = (esmft+esmfstep)
    ! set the DFLOWFM clock at the same stop time....
    ! TODO check, assuming seconds here...
    tstop_user = esmfnext
    do while (time_user .lt. tstop_user .and. jastop.eq.0 ) ! time loop
      call flowstep(jastop,iresult)
    end do


    ! State export:
    call ESMF_StateWrite(exportState, 'dflowfmexport.nc', rc)
  end subroutine unstruc_run


#undef METHOD
#define METHOD "ModelFinalize"

  subroutine ModelFinalize(gcomp,rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc

    ! Local Variables
    character(32)              :: cname
    type(type_InternalState)   :: is
    integer                    :: stat
    type(ESMF_Clock)           :: modelClock
    type(ESMF_Time)            :: currTime
    character(len=32)          :: currTimeStr

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": entered "//METHOD, ESMF_LOGMSG_INFO)
#endif

    rc = ESMF_SUCCESS

    ! Query component for name
    call ESMF_GridCompGet(gcomp, name=cname, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    ! query Component for its internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(gcomp, label_InternalState, is, rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    ! query the Component for its clock
    call NUOPC_ModelGet(gcomp, modelClock=modelClock, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    call ESMF_ClockGet(modelClock, currTime=currTime, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    call ESMF_TimeGet(currTime, timeString=currTimeStr, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    call NWM_HYC_Fin(rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    deallocate(is%wrap, stat=stat)
    if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
      msg='NWM: Deallocation of internal state memory failed.', &
      file=FILENAME,rcToReturn=rc)) return ! bail out


#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": leaving "//METHOD, ESMF_LOGMSG_INFO)
#endif

  end subroutine

  !-----------------------------------------------------------------------------
  ! Log Utilities
  !-----------------------------------------------------------------------------

#undef METHOD
#define METHOD "LogAdvertised"

  subroutine LogAdvertised(label)
    character(len=*),intent(in) :: label

    ! local variables
    integer                    :: cntImp
    integer                    :: cntExp
    integer                    :: fIndex
    character(ESMF_MAXSTR)     :: logMsg
    integer                    :: rc

    ! Count advertised import and export fields

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": leaving "//METHOD, ESMF_LOGMSG_INFO)
#endif

  end subroutine

  !-----------------------------------------------------------------------------

#undef METHOD
#define METHOD "LogRealized"

  subroutine LogRealized(label)
    character(len=*),intent(in) :: label

    ! local variables
    integer                    :: cntImp
    integer                    :: cntExp
    integer                    :: fIndex
    character(ESMF_MAXSTR)     :: logMsg
    integer                    :: rc

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": entered "//METHOD, ESMF_LOGMSG_INFO)
#endif

    ! Count advertised import and export fields

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": leaving "//METHOD, ESMF_LOGMSG_INFO)
#endif

  end subroutine

  !-----------------------------------------------------------------------------

#undef METHOD
#define METHOD "LogAttributes"

  subroutine LogAttributes(label,gcomp)
    character(len=*), intent(in)  :: label
    type(ESMF_GridComp)           :: gcomp

    ! local variables

    type(type_InternalState)   :: is
    character(ESMF_MAXSTR)     :: logMsg
    character(len=64)          :: modeStr
    integer                    :: rc

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": entered "//METHOD, ESMF_LOGMSG_INFO)
#endif

    ! query Component for its internal State
    nullify(is%wrap)

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": leaving "//METHOD, ESMF_LOGMSG_INFO)
#endif

  end subroutine

  !-----------------------------------------------------------------------------

#undef METHOD
#define METHOD "LogMode"

  subroutine LogMode(label,gcomp)
    character(len=*), intent(in)  :: label
    type(ESMF_GridComp)           :: gcomp

    ! local variables

    type(type_InternalState)   :: is
    character(ESMF_MAXSTR)     :: logMsg
    character(len=64)          :: modeStr
    integer                    :: rc

    ! query Component for its internal State
    nullify(is%wrap)

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": leaving "//METHOD, ESMF_LOGMSG_INFO)
#endif

  end subroutine

  !-----------------------------------------------------------------------------

#undef METHOD
#define METHOD "LogClock"

  subroutine LogClock(label,gcomp)
    character(len=*), intent(in) :: label
    type(ESMF_GridComp)          :: gcomp

    ! local variables
    type(type_InternalState)   :: is
    character(ESMF_MAXSTR)     :: logMsg
    type(ESMF_Time)            :: currTime
    type(ESMF_TimeInterval)    :: timestep
    character(len=64)          :: currTimeStr
    character(len=64)          :: timestepStr
    integer                    :: rc

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": entering "//METHOD, ESMF_LOGMSG_INFO)
#endif

    ! query Component for its internal State
    nullify(is%wrap)

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": leaving "//METHOD, ESMF_LOGMSG_INFO)
#endif

  end subroutine

  !> Retrieve a pointer to a field's data array from inside an ESMF_State
  !object.
  !!
  !! @param ST the ESMF_State object
  !! @param fldname name of the fields
  !! @param fldptr pointer to 1D array
  !! @param rc return code
  subroutine State_GetFldPtr(ST, fldname, fldptr, rc)
    type(ESMF_State), intent(in) :: ST
    character(len=*), intent(in) :: fldname
    real(ESMF_KIND_R8), pointer, intent(in) :: fldptr(:)
    integer, intent(out), optional :: rc

    ! local variables
    type(ESMF_Field) :: lfield
    integer :: lrc
    character(len=*),parameter :: subname='(WAV:State_GetFldPtr)'

    call ESMF_StateGet(ST, itemName=trim(fldname), field=lfield, rc=lrc)
    if (ESMF_LogFoundError(rcToCheck=lrc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
    call ESMF_FieldGet(lfield, farrayPtr=fldptr, rc=lrc)
    if (ESMF_LogFoundError(rcToCheck=lrc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
    if (present(rc)) rc = lrc
  end subroutine State_GetFldPtr

  subroutine State_GetFldPtr2(ST, fldname, fldptr, rc)
    type(ESMF_State), intent(in) :: ST
    character(len=*), intent(in) :: fldname
    !integer(ESMF_KIND_I4), pointer, intent(in) :: fldptr(:)
    integer, pointer, intent(in) :: fldptr(:)
    integer, intent(out), optional :: rc

    ! local variables
    type(ESMF_Field) :: lfield
    integer :: lrc
    character(len=*),parameter :: subname='(WAV:State_GetFldPtr)'

    call ESMF_StateGet(ST, itemName=trim(fldname), field=lfield, rc=lrc)
    if (ESMF_LogFoundError(rcToCheck=lrc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
    call ESMF_FieldGet(lfield, farrayPtr=fldptr, rc=lrc)
    if (ESMF_LogFoundError(rcToCheck=lrc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
    if (present(rc)) rc = lrc
  end subroutine State_GetFldPtr2

end module
