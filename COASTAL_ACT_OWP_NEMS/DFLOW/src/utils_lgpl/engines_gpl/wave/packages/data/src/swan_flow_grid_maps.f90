module swan_flow_grid_maps
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2022.                                
!                                                                               
!  This program is free software: you can redistribute it and/or modify         
!  it under the terms of the GNU General Public License as published by         
!  the Free Software Foundation version 3.                                      
!                                                                               
!  This program is distributed in the hope that it will be useful,              
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU General Public License for more details.                                 
!                                                                               
!  You should have received a copy of the GNU General Public License            
!  along with this program.  If not, see <http://www.gnu.org/licenses/>.        
!                                                                               
!  contact: delft3d.support@deltares.nl                                         
!  Stichting Deltares                                                           
!  P.O. Box 177                                                                 
!  2600 MH Delft, The Netherlands                                               
!                                                                               
!  All indications and logos of, and references to, "Delft3D" and "Deltares"    
!  are registered trademarks of Stichting Deltares, and remain the property of  
!  Stichting Deltares. All rights reserved.                                     
!                                                                               
!-------------------------------------------------------------------------------
!  $Id: swan_flow_grid_maps.f90 140657 2022-01-24 13:27:19Z nabi $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/wave/packages/data/src/swan_flow_grid_maps.f90 $
!!--description-----------------------------------------------------------------
! NONE
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision_basics
    !
    type grid_map
       logical                                  :: sferic          ! spherical coordinate system (t/f)
       logical                                  :: ext_mapper      ! .true.: The mapping is done by an external program e.g. ESMF_RegridWeightGen
       logical                                  :: grids_linked    ! .true.: flow maps to wave using ESMF
       integer                                  :: n_b             ! ext_mapper: n_b from ESMF_RegridWeight file: dimension of destination grid
       integer                                  :: n_s             ! ext_mapper: n_s from ESMF_RegridWeight file: dimension of weight vectors
       integer                                  :: n_surr_points   ! number of surrounding points (4 for curvilin., 3 for triangular grids)
       integer                                  :: msurpnts        ! minimum number of surrounding valid source-points for a target-point to be covered.
                                                                   ! = 3: (default)
                                                                   ! = 4: when comparing to Delft3D
       integer                                  :: npts_provider   ! number of points provider grid
       integer                                  :: npts_receiver   ! number of points receiver grid
       integer, dimension(:,:), pointer         :: ref_table       ! reference table from client grid point
                                                                   ! to surrounding provider grid points
       integer, dimension(:)  , pointer         :: col             ! ext_mapper: col from ESMF_RegridWeight file: source indices
       integer, dimension(:)  , pointer         :: row             ! ext_mapper: row from ESMF_RegridWeight file: destination indices
       real   , dimension(:,:), pointer         :: weight_table    ! weights of surrounding provider grid points
       real(kind=hp), dimension(:)  , pointer   :: s               ! ext_mapper: S from ESMF_RegridWeight file: weights
       real(kind=hp), dimension(:)  , pointer   :: frac_b          ! ext_mapper: frac_b from ESMF_RegridWeight file: weights
       character (256)                          :: provider_name   ! name of provider grid
       character (256)                          :: receiver_name   ! name of receiver grid
       character (256)                          :: p_tmp_filename  ! ext_mapper: source file, based on provider, in expected format
       character (256)                          :: r_tmp_filename  ! ext_mapper: destination file, based on receiver, in expected format
       character (256)                          :: w_tmp_filename  ! ext_mapper: weight file, generated by external program
    end type grid_map
    !
    type grid
       integer                                  :: mmax            ! number of columns
       integer                                  :: nmax            ! number of rows
       integer                                  :: kmax            ! number of layers
       integer                                  :: npts            ! number of points
       integer                                  :: numenclpts      ! total number of enclosure points
       integer                                  :: numenclparts    ! number of parts of grid enclosure (exteriors+interior rings)
       integer, dimension(:,:), pointer         :: kcs             ! mask-array inactive points
       integer, dimension(:,:), pointer         :: covered         ! mask-array points covered by "other" program
       integer, dimension(:)  , pointer         :: numenclptsppart ! number of enclosure points per enclosure part
       real(kind=hp)                            :: xymiss          ! missing value
       real(kind=hp), dimension(:,:), pointer   :: x               ! x-coordinates cell center
       real(kind=hp), dimension(:,:), pointer   :: y               ! y-coordinates cell center
       real(kind=hp), dimension(:), pointer     :: bndx            ! x-coordinates boundary link corners
       real(kind=hp), dimension(:), pointer     :: bndy            ! y-coordinates boundary link corners
       real   , dimension(:,:), pointer         :: alfas           ! grid direction cell center
       real   , dimension(:,:), pointer         :: guu             ! grid size u-cell wall
       real   , dimension(:,:), pointer         :: gvv             ! grid size v-cell wall
       logical                                  :: sferic          ! spherical coordinate system (t/f)
       character (256)                          :: grid_name       ! name of grid
       character (37)                           :: tmp_name        ! temporary filename
       character (4)                            :: grid_file_type  ! type of grid file (SWAN/FLOW/COM/TRIM)
       character (6)                            :: xy_loc          ! location of xy coords (CORNER/CENTER)
       character (16)                           :: layer_model     ! Vertical layering type: "Sigma-model" or "Z-model"
    end type   grid
    !
    type input_fields
       integer                                  :: mmax            ! number of columns
       integer                                  :: nmax            ! number of rows
       integer                                  :: npts            ! number of points
       integer, dimension(:,:), pointer         :: kfu             ! mask array u-velocity points
       integer, dimension(:,:), pointer         :: kfv             ! mask array v-velocity points
       real   , dimension(:,:), pointer         :: s1              ! water level
       real   , dimension(:,:), pointer         :: u1              ! u-velocity
       real   , dimension(:,:), pointer         :: v1              ! v-velocity
       real   , dimension(:,:), pointer         :: dps             ! depth in water level points
       real   , dimension(:,:), pointer         :: windu           ! wind velocity component in x direction
       real   , dimension(:,:), pointer         :: windv           ! wind velocity component in y direction
       real   , dimension(:,:), pointer         :: ice_frac        ! sea_ice_area_fraction
       real   , dimension(:,:), pointer         :: floe_dia        ! floe_diameter
       real   , dimension(:,:), pointer         :: s1mud           ! mud level
       real   , dimension(:,:), pointer         :: dpsmud          ! mud related depth in water level points
       real   , dimension(:,:), pointer         :: veg             ! vegetation map - densities of plants per m2
       real   , dimension(:,:), pointer         :: diaveg          ! vegetation map - stem diameter of plants [m]
       real   , dimension(:,:), pointer         :: veg_stemheight  ! vegetation map - stem height of plants [m]
       real   , dimension(:,:), pointer         :: s1veg           ! vegetation level - dummy field of zeros
    end type input_fields                       
    !                                           
    type output_fields                          
       integer                                  :: mmax            ! number of columns
       integer                                  :: nmax            ! number of rows
       integer                                  :: npts            ! number of points
       integer                                  :: n_outpars       ! number of additional output parameters
       real, dimension(:,:), pointer            :: hs              !
       real, dimension(:,:), pointer            :: dir             !
       real, dimension(:,:), pointer            :: dirc            !
       real, dimension(:,:), pointer            :: dirs            !
       real, dimension(:,:), pointer            :: period          !
       real, dimension(:,:), pointer            :: depth           !
       real, dimension(:,:), pointer            :: fx              !
       real, dimension(:,:), pointer            :: fy              !
       real, dimension(:,:), pointer            :: wsbodyu         !
       real, dimension(:,:), pointer            :: wsbodyv         !
       real, dimension(:,:), pointer            :: mx              !
       real, dimension(:,:), pointer            :: my              !
       real, dimension(:,:,:), pointer          :: dissip          !
       real, dimension(:,:), pointer            :: ubot            !
       real, dimension(:,:), pointer            :: steep           !
       real, dimension(:,:), pointer            :: wlen            !
       real, dimension(:,:), pointer            :: u               !
       real, dimension(:,:), pointer            :: v               !
       real, dimension(:,:), pointer            :: dspr            !
       real, dimension(:,:), pointer            :: rleak           !
       real, dimension(:,:), pointer            :: qb              !
       real, dimension(:,:), pointer            :: x               !
       real, dimension(:,:), pointer            :: y               !
       real, dimension(:,:), pointer            :: rtp             !
       real, dimension(:,:), pointer            :: hrms            !
       real, dimension(:,:), pointer            :: tp              !
       real, dimension(:,:), pointer            :: pdir            !
       real, dimension(:,:), pointer            :: windu           !
       real, dimension(:,:), pointer            :: windv           !
       real, dimension(:,:), pointer            :: tps             !
       real, dimension(:,:), pointer            :: tm02            !
       real, dimension(:,:), pointer            :: tmm10           !
       real, dimension(:,:), pointer            :: dhsign          !
       real, dimension(:,:), pointer            :: drtm01          !
       real, dimension(:,:), pointer            :: setup           !
       real, dimension(:,:,:), pointer          :: add_out_vals    !
       character(7), dimension(:), pointer      :: add_out_names  !
    end type output_fields
    !
    type (grid)         , dimension(:)  , pointer, save :: swan_grids         ! pointer array swan grids
    type (grid)         , dimension(:)  , pointer, save :: flow_grids         ! pointer array flow grids
    type (grid_map)     , dimension(:,:), pointer, save :: flow2swan_maps     ! mappers flow->swan
    type (grid_map)     , dimension(:,:), pointer, save :: swan2flow_maps     ! mappers swan->flow
    type (input_fields)                          , save :: swan_input_fields  ! pointer to input fields, swan grid
    type (output_fields)                         , save :: swan_output_fields ! pointer array to output fields, swan grid
    type (output_fields), dimension(:)  , pointer, save :: flow_output_fields ! pointer array to output fields, flow grid
    !
contains
!
!
!==============================================================================
subroutine init_grids (n_swan_grids, n_flow_grids)
   implicit none
   !
   ! parameters
   integer, intent(in) :: n_swan_grids
   integer, intent(in) :: n_flow_grids
   !
   ! body
   allocate (swan_grids(n_swan_grids))
   allocate (flow_grids(n_flow_grids))
   allocate (swan2flow_maps(n_swan_grids,n_flow_grids))
   allocate (flow2swan_maps(n_swan_grids,n_flow_grids))
   allocate (flow_output_fields(n_flow_grids))
end subroutine Init_grids
!
!
!==============================================================================
subroutine alloc_and_get_grid(i_grid, g,grid_name,grid_file_type,xy_loc, flowLinkConnectivity, filename)
   use read_grids
   !
   implicit none
   !
   integer                   :: i_grid
   type(grid)                :: g
   character (*), intent(in) :: grid_name       ! name of grid
   character (4), intent(in) :: grid_file_type  ! type of grid file (SWAN/FLOW/COM/TRIM)
   character (6), intent(in) :: xy_loc          ! location of xy coords (CORNER/CENTER)
   logical      , intent(in) :: flowLinkConnectivity
   character (*), optional   :: filename
   !
   ! Assign attributes to grid structure
   !
   g%grid_name      = grid_name
   g%grid_file_type = grid_file_type
   g%xy_loc         = xy_loc
   !
   ! Obtain grid dimensions
   !
   select case(grid_file_type)
      case ('COM')
         !
         ! This is a FLOW grid
         ! read data from com-file
         !
         call get_gri(g%grid_name ,g%x   ,g%y      ,g%guu  ,g%gvv , &
             &        g%alfas     ,g%kcs    ,g%covered     ,g%mmax ,g%nmax, &
             &        g%kmax      ,g%xymiss ,g%layer_model )
         !
         ! In case of DomainDecomposition, some adaptions are necessary
         !
         call grid_dd_corrections(g%alfas, g%kcs,g%mmax, g%nmax)
         !
         ! array covered is only used for SWAN grids
         ! Currently it is also generated for FLOW grids
         !
      case ('FLOW')
         !
         ! This is a SWAN grid
         ! read data from grd-file
         !
         call read_grd(g%grid_name, g%x   ,g%y    ,g%kcs ,g%covered, g%mmax, g%nmax ,g%sferic, g%xymiss)
      case ('NC')
          !
          ! This is an unstructured grid from D-FLOWFM
          !
          if (.not.present(filename)) then
             write(*,'(a)') '*** ERROR: on calling read_netcdf_grd: filename not specified.'
             call wavestop(1, '*** ERROR: on calling read_netcdf_grd: filename not specified.')
          endif
          call read_netcdf_grd(i_grid, g%grid_name, g%x   ,g%y    ,g%kcs ,g%covered, g%mmax, g%nmax ,g%kmax , g%sferic, g%xymiss, g%bndx, g%bndy, g%numenclpts, g%numenclparts, g%numenclptsppart, filename, flowLinkConnectivity)
      case default
         ! grid type not supported
         write(*,'(3a)') '*** ERROR: Grid type ''',trim(grid_file_type), ''' not supported.'
         call wavestop(1, '*** ERROR: Grid type '''//trim(grid_file_type)//''' not supported.')
   endselect
   g%npts = g%mmax*g%nmax
end subroutine alloc_and_get_grid
!
!
!==============================================================================
subroutine grid_dd_corrections(alfas, kcs, mmax, nmax)
    implicit none
    !
    ! global parameters
    integer                      , intent(in)  :: mmax
    integer                      , intent(in)  :: nmax
    integer, dimension(mmax,nmax), intent(in)  :: kcs
    real   , dimension(mmax,nmax)              :: alfas
    !
    ! local parameters
    !
    integer :: m
    integer :: mNeighbour
    integer :: n
    integer :: nNeighbour
    !
    ! body
    !
    do m = 1, mmax
       do n = 1, nmax
          if (kcs(m,n) == 3) then
             !
             ! alfa is undefined; copy from one of the 4 neighbours with kcs=1
             !
             mNeighbour = min(m+1,mmax)
             nNeighbour = n
             if (kcs(mNeighbour,nNeighbour) == 1) then
                alfas(m,n) = alfas(mNeighbour,nNeighbour)
                cycle
             endif
             mNeighbour = max(m-1,1)
             nNeighbour = n
             if (kcs(mNeighbour,nNeighbour) == 1) then
                alfas(m,n) = alfas(mNeighbour,nNeighbour)
                cycle
             endif
             mNeighbour = m
             nNeighbour = min(n+1,nmax)
             if (kcs(mNeighbour,nNeighbour) == 1) then
                alfas(m,n) = alfas(mNeighbour,nNeighbour)
                cycle
             endif
             mNeighbour = m
             nNeighbour = max(n-1,1)
             if (kcs(mNeighbour,nNeighbour) == 1) then
                alfas(m,n) = alfas(mNeighbour,nNeighbour)
                cycle
             endif
             !
             ! The following statement should not be reached
             !
             write(*,'(a,i0,a,i0,a)') '*** ERROR: alfas(m=', m, ',n=', n, ') is undefined' 
             call wavestop(1, 'alfas is undefined')
          endif
       enddo
    enddo
end subroutine grid_dd_corrections
!
!
!==============================================================================
subroutine make_grid_map(i1, i2, g1, g2, gm, external_mapper)
   use system_utils, only: ARCH
   use netcdf
   use m_polygon
   use m_tpoly
   implicit none
   !
   ! Parameters
   integer           :: i1
   integer           :: i2
   type(grid)        :: g1
   type(grid)        :: g2
   type(grid_map)    :: gm
   logical, optional :: external_mapper
   !
   ! Locals
   !
   logical                                         :: b
   logical                                         :: keepExisting
   integer                                         :: i
   integer                                         :: j
   integer                                         :: n
   integer                                         :: ierror
   integer                                         :: in
   integer                                         :: ind
   integer                                         :: iprint
   integer                                         :: idfile
   integer                                         :: iddim_nb
   integer                                         :: iddim_ns
   integer                                         :: idvar_col
   integer                                         :: idvar_fracb
   integer                                         :: idvar_row
   integer                                         :: idvar_s
   integer                                         :: nvert
   integer                                         :: numpli
   integer, dimension(:), allocatable              :: iflag,nrin,nrx,nry
   integer, dimension(:), allocatable              :: vertex
   integer, dimension(:,:), allocatable            :: ncontrib
   real(kind=hp)   , dimension(:)  , allocatable   :: xs
   real(kind=hp)   , dimension(:)  , allocatable   :: ys
   real            , dimension(:,:), allocatable   :: testfield
   character(1024)                                 :: command
   character(1024)                                 :: tmpstr
   character(50)                                   :: searchstring
   character(NF90_MAX_NAME)                        :: string
   type(tpoly),       dimension(:), allocatable    :: pli_loc
   !
   ! body
   if (present(external_mapper)) then
      gm%ext_mapper = external_mapper
   else
      gm%ext_mapper = .false.
   endif
   gm%sferic       = g1%sferic
   gm%grids_linked = .false.
   !
   if (gm%ext_mapper) then
      gm%grids_linked = .true.
      !
      ! The following weights filename will do as long as there is only one Flow-source file involved
      !
      gm%w_tmp_filename = trim(gm%r_tmp_filename)
      write(searchstring,'(a,i4.4,a)') "destination_", i2, ".nc"
      ind = index(gm%w_tmp_filename, trim(searchstring), back = .true.)
      if (ind > 0) then
         tmpstr = gm%w_tmp_filename(1:ind-1) ! Cannot write and read from w_tmp_filename at the same time.
         write(gm%w_tmp_filename,'(2a,i4.4,a,i4.4,a)') tmpstr(1:ind-1), 'weights_', i1, 'to', i2, '.nc'
      else
         write(*,'(4a)') '*** ERROR: unable to locate "',trim(searchstring), '" in "', trim(gm%w_tmp_filename), '"'
         call wavestop(1, 'unable to locate "'//trim(searchstring)//'" in "'//trim(gm%w_tmp_filename)// '"')
      endif
      write(*,'(a)') '<<Run ESMF_RegridWeightGen...'
      if (ARCH == 'linux') then
         write(*,'(a)')'>>...Check file esmf_sh.log'
         write(command, '(a)') 'ESMF_RegridWeightGen_in_Delft3D-WAVE.sh'
      else
         write(*,'(a)')'>>...Check file esmf_bat.log'
         write(command, '(a)') 'ESMF_RegridWeightGen_in_Delft3D-WAVE.bat'
      endif
      command = trim(command) // ' ' // trim(gm%p_tmp_filename) // ' ' // trim(gm%r_tmp_filename) // ' ' // trim(gm%w_tmp_filename)
      if (.not. g1%sferic) then
         write(command,'(2a)') trim(command), " CARTESIAN"
      endif
      write(*,'(3a)') 'Executing command: "', trim(command), '"'
      !
      ! util_system needs some spaces at the end of command!
      !
      call util_system(command(1:len_trim(command)+5))
      write(*,'(a)')'>>...End of ESMF_RegridWeightGen run'
      !
      ! Read weights from ESMF_Regrid file
      !
      ierror = nf90_open(gm%w_tmp_filename, NF90_NOWRITE, idfile); call nc_check_err(ierror, "opening file", gm%w_tmp_filename)
      if (ierror /= 0) then
         write(*,'(3a)') "ERROR Unable to open file """, trim(gm%w_tmp_filename), """."
         call wavestop(1, "Unable to open file """//trim(gm%w_tmp_filename)//""".")
      endif
      !
      ierror = nf90_inq_dimid(idfile, 'n_b', iddim_nb); call nc_check_err(ierror, "inq_dimid n_b", gm%w_tmp_filename)
      ierror = nf90_inq_dimid(idfile, 'n_s', iddim_ns); call nc_check_err(ierror, "inq_dimid n_s", gm%w_tmp_filename)
      ierror = nf90_inquire_dimension(idfile, iddim_nb, string, gm%n_b); call nc_check_err(ierror, "inq_dim n_b", gm%w_tmp_filename)
      ierror = nf90_inquire_dimension(idfile, iddim_ns, string, gm%n_s); call nc_check_err(ierror, "inq_dim n_s", gm%w_tmp_filename)
      ierror = nf90_inq_varid(idfile, 'col', idvar_col); call nc_check_err(ierror, "inq_varid col", gm%w_tmp_filename)
      ierror = nf90_inq_varid(idfile, 'row', idvar_row); call nc_check_err(ierror, "inq_varid row", gm%w_tmp_filename)
      ierror = nf90_inq_varid(idfile, 'S', idvar_s); call nc_check_err(ierror, "inq_varid S", gm%w_tmp_filename)
      ierror = nf90_inq_varid(idfile, 'frac_b', idvar_fracb); call nc_check_err(ierror, "inq_varid frac_b", gm%w_tmp_filename)
      !
      ! ESMF_regridder, Cartesian grid with bilinear method:
      ! Introduced in version 7.0.0.beta.snapshot38:
      ! 2D Cartesian grids => n_b = mmax * nmax
      !
      if (gm%n_b /= g2%mmax*g2%nmax) then
         write(*,'(a,i0,a,i0,a)') "ERROR dimension n_b (",gm%n_b, ") from ESMF_RegridWeight file does not match the WAVE grid dimension (", (g2%mmax-1)*(g2%nmax-1), ")."
         call wavestop(1, "Dimension n_b from ESMF_RegridWeight file does not match the WAVE grid dimension.")
      endif
      if (gm%n_s == 0) then
         !write(*,'(3a)') "ERROR dimension n_s from ESMF_RegridWeight is zero."
         !call wavestop(1, "Dimension n_s from ESMF_RegridWeight is zero.")
         write(*,'(a,i0,a,i0,a)') "WARNING Flow grid ", i1, " is not overlapping with WAVE grid ", i2,". Mapper not filled with weights."
         gm%grids_linked = .false.
         return
      endif
      allocate (gm%col   (gm%n_s), stat=ierror)
      allocate (gm%row   (gm%n_s), stat=ierror)
      allocate (gm%s     (gm%n_s), stat=ierror)
      allocate (gm%frac_b(gm%n_b), stat=ierror)
      if (ierror /= 0) then
         write(*,'(a)') "ERROR allocating in make_grid_map"
         call wavestop(1, "ERROR allocating in make_grid_map")
      endif
      ierror = nf90_get_var(idfile, idvar_col  , gm%col   , start=(/1/), count=(/gm%n_s/)); call nc_check_err(ierror, "get_var col", gm%w_tmp_filename)
      ierror = nf90_get_var(idfile, idvar_row  , gm%row   , start=(/1/), count=(/gm%n_s/)); call nc_check_err(ierror, "get_var row", gm%w_tmp_filename)
      ierror = nf90_get_var(idfile, idvar_s    , gm%s     , start=(/1/), count=(/gm%n_s/)); call nc_check_err(ierror, "get_var s", gm%w_tmp_filename)
      ierror = nf90_get_var(idfile, idvar_fracb, gm%frac_b, start=(/1/), count=(/gm%n_b/)); call nc_check_err(ierror, "get_var frac_b", gm%w_tmp_filename)
      ierror = nf90_close(idfile); call nc_check_err(ierror, "closing file", gm%w_tmp_filename)
      !
      ! Check coverage of g2 grid
      !
      allocate (testfield(g2%mmax,g2%nmax), stat=ierror)
      allocate (ncontrib(g2%mmax,g2%nmax), stat=ierror)
      if (ierror /= 0) then
         write(*,'(a)') "ERROR allocating in make_grid_map"
         call wavestop(1, "ERROR allocating in make_grid_map")
      endif
      testfield = 0.0_sp
      ncontrib  = 0
      if (g2%sferic) then
         do n=1, gm%n_s
            i = floor(real(gm%row(n)-1)/real(g2%nmax)) + 1
            j = gm%row(n) - g2%nmax*(i-1)
            testfield(i,j) = testfield(i,j) + gm%s(n)
            ncontrib(i,j)  = ncontrib(i,j) + 1
         enddo
      else
         do n=1, gm%n_s
            ! Skip contributions from points at z=1 (instead z=0):
            !if (gm%col(n) > g1%npts) cycle
            ! Skip updates of points at z=1:
            !if (gm%row(n) > g2%npts) cycle
            j = floor(real(gm%row(n)-1)/real(g2%mmax)) + 1
            i = gm%row(n) - g2%mmax*(j-1)
            testfield(i,j) = testfield(i,j) + gm%s(n)
            ncontrib(i,j)  = ncontrib(i,j) + 1
         enddo    
      endif 
      !
      ! Make polygon administration of domain enclosure
      !
      ! safety
      call savepol()
      !
      ! saved in m_polygon::xpl, ypl, zpl
      call makedomainbndpol(g1%bndx, g1%bndy, g1%numenclpts, g1%numenclparts, g1%numenclptsppart)
      keepExisting = .false.
      !
      ! poly with bounding box
      call pol_to_tpoly(numpli, pli_loc, keepExisting)
      !
      do i=1, g2%mmax
         do j=1, g2%nmax
            ! This point is only covered when it is surrounded by at least msurpnts valid points
            ! Also, check if the point was not covered by another partition
            if (ncontrib(i,j) < gm%msurpnts .and. .not.(g2%covered(i,j)>0)) then
               g2%covered(i,j) = 0
            else
               ! old serial way:
               !g2%covered(i,j) = nint(testfield(i,j))
               !endif
               ! Parallel AND serial approach FM:
               in = -1    ! init, needed
               call dbpinpol_tpolies(pli_loc, g2%x(i,j), g2%y(i,j), in)
               if (in==1) then
                  g2%covered(i,j) = i1
               endif
            endif
         enddo
      enddo
      !
      call restorepol()
      !     
      deallocate(testfield, stat=ierror)
      deallocate(ncontrib, stat=ierror)
      !
      ! gets allocated in pol_to_tpoly
      deallocate(pli_loc, stat=ierror)
   else
      ! No external mapper
      !
      ! Allocate memory local work arrays
      !
      allocate (xs   (g2%npts), stat=ierror)
      allocate (ys   (g2%npts), stat=ierror)
      allocate (iflag(g2%npts), stat=ierror)
      allocate (nrin (g2%npts), stat=ierror)
      allocate (nrx  (g2%npts), stat=ierror)
      allocate (nry  (g2%npts), stat=ierror)
      if (ierror /= 0) then
         write(*,'(a)')   "ERROR allocating in make_grid_map"
         call wavestop(1, "ERROR allocating in make_grid_map")
      endif
      !
      ! Set gridmap attributes
      !
      gm%provider_name = g1%grid_name
      gm%receiver_name = g2%grid_name
      gm%npts_provider = g1%npts
      gm%npts_receiver = g2%npts
      gm%n_surr_points = 4
      !
      ! Allocate memory reference and weight tables
      !
      allocate(gm%ref_table    (gm%n_surr_points,g2%npts))
      allocate(gm%weight_table (gm%n_surr_points,g2%npts))
      !
      ! print option
      !
      iprint=0
      call mkmap (g1%kcs, g1%x ,  g1%y  , g1%mmax, g1%nmax,     &
                & g2%x  , g2%y , g2%npts, xs     ,ys      ,     &
                & nrx   , nry  , iflag  , nrin   ,              &
                & gm%weight_table     , gm%ref_table      ,     &
                & iprint, g2%covered, g1%xymiss)
      !
      ! Deallocate memory local work arrays
      !
      deallocate (xs   , stat=ierror)
      deallocate (ys   , stat=ierror)
      deallocate (iflag, stat=ierror)
      deallocate (nrin , stat=ierror)
      deallocate (nrx  , stat=ierror)
      deallocate (nry  , stat=ierror)
   endif
end subroutine make_grid_map
!
!
!==============================================================================
subroutine alloc_input_fields (g, inpfld, mode, ice)
   use wave_data
   implicit none
   !
   ! arguments
   integer                 :: mode
   type(grid)              :: g
   type(input_fields)      :: inpfld
   integer, optional       :: ice
   !
   ! locals
   integer :: ice_
   !
   ! body
   if (present(ice)) then
      ice_ = ice
   else
      ice_ = 0
   endif
   !
   ! Assign grid dimensions
   !
   inpfld%mmax = g%mmax
   inpfld%nmax = g%nmax
   inpfld%npts = g%npts
   !
   ! Allocate variables
   !
   allocate (inpfld%s1   (inpfld%mmax,inpfld%nmax))
   allocate (inpfld%u1   (inpfld%mmax,inpfld%nmax))
   allocate (inpfld%v1   (inpfld%mmax,inpfld%nmax))
   allocate (inpfld%kfu  (inpfld%mmax,inpfld%nmax))
   allocate (inpfld%kfv  (inpfld%mmax,inpfld%nmax))
   allocate (inpfld%dps  (inpfld%mmax,inpfld%nmax))
   allocate (inpfld%windu(inpfld%mmax,inpfld%nmax))
   allocate (inpfld%windv(inpfld%mmax,inpfld%nmax))
   allocate (inpfld%veg  (inpfld%mmax,inpfld%nmax))
   allocate (inpfld%diaveg        (inpfld%mmax,inpfld%nmax))
   allocate (inpfld%veg_stemheight(inpfld%mmax,inpfld%nmax))
   allocate (inpfld%s1veg         (inpfld%mmax,inpfld%nmax))
   !
   ! Initialise arrays
   !
   inpfld%s1    = 0.
   inpfld%u1    = 0.
   inpfld%v1    = 0.
   inpfld%dps   = 0.
   inpfld%veg   = 0.
   inpfld%diaveg         = 0.
   inpfld%veg_stemheight = 0.
   inpfld%s1veg = 0.
   inpfld%kfu   = 0
   inpfld%windu = 0.
   inpfld%windv = 0.
   !
   ! Interaction with Fluid Mud
   !
   if (mode == flow_mud_online) then
      allocate (inpfld%s1mud   (inpfld%mmax,inpfld%nmax))
      allocate (inpfld%dpsmud  (inpfld%mmax,inpfld%nmax))
      inpfld%s1mud    = 0.0
      inpfld%dpsmud   = 0.0
   endif
   if (ice_ > 0) then
      allocate (inpfld%ice_frac(inpfld%mmax,inpfld%nmax))
      allocate (inpfld%floe_dia(inpfld%mmax,inpfld%nmax))
      inpfld%ice_frac = 0.0
      inpfld%floe_dia = 0.0
   endif
end subroutine alloc_input_fields
!
!
!==============================================================================
subroutine init_input_fields (inpfld,sr,itide)
   use swan_input
   !
   implicit none
   !
   type(input_fields)      :: inpfld
   type(swan_type)         :: sr
   integer                 :: itide
   !
   ! locals
   !
   real :: pi
   real :: d2r
   real :: cartDir
   ! body
   !
   ! Initialise arrays with overall values from swan input
   !
   inpfld%s1 = sr%zeta(itide)   
   inpfld%u1 = sr%ux0(itide)
   inpfld%v1 = sr%uy0(itide)
   pi           = 4.0 * atan(1.0)
   d2r          = pi / 180.0
   if (sr%nautconv) then
      cartDir = 270.0 - sr%wdir(itide)
   else
      cartDir = sr%wdir(itide)
   endif
   cartDir = cartDir*d2r
   inpfld%windu = sr%wvel(itide) * cos(cartDir)
   inpfld%windv = sr%wvel(itide) * sin(cartDir)
end subroutine init_input_fields
!
!
!==============================================================================
subroutine dealloc_input_fields (inpfld, mode, ice)
   use wave_data
   implicit none
   !
   integer                 :: ierr
   integer                 :: mode
   type(input_fields)      :: inpfld
   integer, optional       :: ice
   !
   ! locals
   integer :: ice_
   !
   ! body
   if (present(ice)) then
      ice_ = ice
   else
      ice_ = -1
   endif
   !
   deallocate (inpfld%s1   , stat=ierr)
   deallocate (inpfld%u1   , stat=ierr)
   deallocate (inpfld%v1   , stat=ierr)
   deallocate (inpfld%kfu  , stat=ierr)
   deallocate (inpfld%kfv  , stat=ierr)
   deallocate (inpfld%dps  , stat=ierr)
   deallocate (inpfld%veg  , stat=ierr)
   deallocate (inpfld%diaveg        , stat=ierr)
   deallocate (inpfld%veg_stemheight, stat=ierr)
   deallocate (inpfld%s1veg, stat=ierr)
   deallocate (inpfld%windu, stat=ierr)
   deallocate (inpfld%windv, stat=ierr)
   if (mode == flow_mud_online) then
      deallocate (inpfld%s1mud , stat=ierr)
      deallocate (inpfld%dpsmud, stat=ierr)
   endif
   if (ice_ > 0) then
      deallocate (inpfld%ice_frac , stat=ierr)
      deallocate (inpfld%floe_dia , stat=ierr)
   endif
end subroutine dealloc_input_fields
!
!
!==============================================================================
subroutine dealloc_output_fields (outfld)
   implicit none
   !
   type(output_fields) :: outfld
   integer             :: ierr
   !
   ! Deallocate variables
   !
   deallocate (outfld%hs    , stat=ierr)
   deallocate (outfld%dir   , stat=ierr)
   deallocate (outfld%dirc  , stat=ierr)
   deallocate (outfld%dirs  , stat=ierr)
   deallocate (outfld%period, stat=ierr)
   deallocate (outfld%depth , stat=ierr)
   deallocate (outfld%fx    , stat=ierr)
   deallocate (outfld%fy    , stat=ierr)
   deallocate (outfld%wsbodyu, stat=ierr)
   deallocate (outfld%wsbodyv, stat=ierr)
   deallocate (outfld%mx    , stat=ierr)
   deallocate (outfld%my    , stat=ierr)
   deallocate (outfld%dissip, stat=ierr)
   deallocate (outfld%ubot  , stat=ierr)
   deallocate (outfld%steep , stat=ierr)
   deallocate (outfld%wlen  , stat=ierr)
   deallocate (outfld%u     , stat=ierr)
   deallocate (outfld%v     , stat=ierr)
   deallocate (outfld%dspr  , stat=ierr)
   deallocate (outfld%rleak , stat=ierr)
   deallocate (outfld%qb    , stat=ierr)
   deallocate (outfld%x     , stat=ierr)
   deallocate (outfld%y     , stat=ierr)
   deallocate (outfld%rtp   , stat=ierr)
   deallocate (outfld%hrms  , stat=ierr)
   deallocate (outfld%tp    , stat=ierr)
   deallocate (outfld%pdir  , stat=ierr)
   deallocate (outfld%windu , stat=ierr)
   deallocate (outfld%windv , stat=ierr)
   deallocate (outfld%tps   , stat=ierr)
   deallocate (outfld%tm02  , stat=ierr)
   deallocate (outfld%tmm10 , stat=ierr)
   deallocate (outfld%dhsign, stat=ierr)
   deallocate (outfld%drtm01, stat=ierr)
   deallocate (outfld%setup , stat=ierr)
   if (associated(outfld%add_out_vals)) then
      deallocate (outfld%add_out_vals , stat=ierr)
      deallocate (outfld%add_out_names , stat=ierr)
   endif
end subroutine dealloc_output_fields
!
!
!==============================================================================
subroutine alloc_output_fields (g,outfld)
   implicit none
   !
   type(grid)              :: g
   type(output_fields)     :: outfld
   !
   ! Assign grid dimensions
   !
   outfld%mmax = g%mmax
   outfld%nmax = g%nmax
   outfld%npts = g%npts
   !
   ! Allocate variables
   !
   allocate (outfld%hs    (outfld%mmax,outfld%nmax))
   allocate (outfld%dir   (outfld%mmax,outfld%nmax))
   allocate (outfld%dirc  (outfld%mmax,outfld%nmax))
   allocate (outfld%dirs  (outfld%mmax,outfld%nmax))
   allocate (outfld%period(outfld%mmax,outfld%nmax))
   allocate (outfld%depth (outfld%mmax,outfld%nmax))
   allocate (outfld%fx    (outfld%mmax,outfld%nmax))
   allocate (outfld%fy    (outfld%mmax,outfld%nmax))
   allocate (outfld%wsbodyu(outfld%mmax,outfld%nmax))
   allocate (outfld%wsbodyv(outfld%mmax,outfld%nmax))
   allocate (outfld%mx    (outfld%mmax,outfld%nmax))
   allocate (outfld%my    (outfld%mmax,outfld%nmax))
   allocate (outfld%dissip(outfld%mmax,outfld%nmax,4))
   allocate (outfld%ubot  (outfld%mmax,outfld%nmax))
   allocate (outfld%steep (outfld%mmax,outfld%nmax))
   allocate (outfld%wlen  (outfld%mmax,outfld%nmax))
   allocate (outfld%u     (outfld%mmax,outfld%nmax))
   allocate (outfld%v     (outfld%mmax,outfld%nmax))
   allocate (outfld%dspr  (outfld%mmax,outfld%nmax))
   allocate (outfld%rleak (outfld%mmax,outfld%nmax))
   allocate (outfld%qb    (outfld%mmax,outfld%nmax))
   allocate (outfld%x     (outfld%mmax,outfld%nmax))
   allocate (outfld%y     (outfld%mmax,outfld%nmax))
   allocate (outfld%rtp   (outfld%mmax,outfld%nmax))
   allocate (outfld%hrms  (outfld%mmax,outfld%nmax))
   allocate (outfld%tp    (outfld%mmax,outfld%nmax))
   allocate (outfld%pdir  (outfld%mmax,outfld%nmax))
   allocate (outfld%windu (outfld%mmax,outfld%nmax))
   allocate (outfld%windv (outfld%mmax,outfld%nmax))
   allocate (outfld%tps   (outfld%mmax,outfld%nmax))
   allocate (outfld%tm02  (outfld%mmax,outfld%nmax))
   allocate (outfld%tmm10 (outfld%mmax,outfld%nmax))
   allocate (outfld%dhsign(outfld%mmax,outfld%nmax))
   allocate (outfld%drtm01(outfld%mmax,outfld%nmax))
   allocate (outfld%setup (outfld%mmax,outfld%nmax))
   if (outfld%n_outpars > 0) then
      allocate (outfld%add_out_vals (outfld%mmax,outfld%nmax,outfld%n_outpars))
      allocate (outfld%add_out_names (outfld%n_outpars))
   endif
   !
   ! Initialise arrays
   !
   outfld%hs      = 0.
   outfld%dir     = 0.
   outfld%dirc    = 0.
   outfld%dirs    = 0.
   outfld%period  = 0.
   outfld%depth   = 0.
   outfld%fx      = 0.
   outfld%fy      = 0.
   outfld%wsbodyu = 0.
   outfld%wsbodyv = 0.
   outfld%mx      = 0.
   outfld%my      = 0.
   outfld%dissip  = 0.
   outfld%ubot    = 0.
   outfld%steep   = 0.
   outfld%wlen    = 0.
   outfld%u       = 0.
   outfld%v       = 0.
   outfld%dspr    = 0.
   outfld%rleak   = 0.
   outfld%qb      = 0.
   outfld%x       = 0.
   outfld%y       = 0.
   outfld%rtp     = 0.
   outfld%hrms    = 0.
   outfld%tp      = 0.
   outfld%pdir    = 0.
   outfld%windu   = 0.
   outfld%windv   = 0.
   outfld%tps     = 0.
   outfld%tm02    = 0.
   outfld%tmm10   = 0.
   outfld%dhsign  = 0.
   outfld%drtm01  = 0.
   outfld%setup   = 0.
   if (outfld%n_outpars > 0) then
      outfld%add_out_vals   = 0.
      outfld%add_out_names   = ' '
   endif
end subroutine alloc_output_fields
!
!
!==============================================================================
! write netboundary data to polygons
subroutine makedomainbndpol(bndx, bndy, numenclpts, numenclparts, numenclptsppart)
   use geometry_module
   use m_polygon
   use m_missing
   !
   implicit none
   !
   ! Parameters
   !
   integer, intent(in)                         :: numenclpts
   integer, intent(in)                         :: numenclparts
   integer         , dimension(:), pointer     :: numenclptsppart
   double precision, dimension(:), pointer     :: bndx
   double precision, dimension(:), pointer     :: bndy
   !
   ! Locals
   !
   integer                                     :: ipt
   integer                                     :: localErr
   integer                                     :: pcount
   !
   ! Body
   !
   ! Defined in module geometry_module: xpl, ypl, zpl, npl, xymis
   if (allocated(xpl)) deallocate(xpl, ypl, zpl, stat = localErr)
   allocate(xpl(1), ypl(1), zpl(1), stat = localErr)
   xpl = XYMIS
   ypl = XYMIS
   zpl = XYMIS
   npl = 0
   !
   pcount = 1
   do ipt = 1, numenclparts
      ! assert size: previous pols (if any) + 1 dmiss + new polyline
      call increasepol(npl + numenclptsppart(ipt) + 1, 1)
      ! safety on indexing
      if (numenclptsppart(ipt)>0) then
         xpl(npl+1:npl+numenclptsppart(ipt)) = bndx(pcount:pcount+numenclptsppart(ipt)-1)
         ypl(npl+1:npl+numenclptsppart(ipt)) = bndy(pcount:pcount+numenclptsppart(ipt)-1)
         npl      = npl    + numenclptsppart(ipt) + 1
         pcount   = pcount + numenclptsppart(ipt) 
      endif
      xpl(npl) = dmiss
      ypl(npl) = dmiss
   enddo
end subroutine makedomainbndpol 

end module swan_flow_grid_maps
