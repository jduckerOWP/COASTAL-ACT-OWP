!----- AGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2017-2022.                                
!                                                                               
!  This file is part of Delft3D (D-Flow Flexible Mesh component).               
!                                                                               
!  Delft3D is free software: you can redistribute it and/or modify              
!  it under the terms of the GNU Affero General Public License as               
!  published by the Free Software Foundation version 3.                         
!                                                                               
!  Delft3D  is distributed in the hope that it will be useful,                  
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU Affero General Public License for more details.                          
!                                                                               
!  You should have received a copy of the GNU Affero General Public License     
!  along with Delft3D.  If not, see <http://www.gnu.org/licenses/>.             
!                                                                               
!  contact: delft3d.support@deltares.nl                                         
!  Stichting Deltares                                                           
!  P.O. Box 177                                                                 
!  2600 MH Delft, The Netherlands                                               
!                                                                               
!  All indications and logos of, and references to, "Delft3D",                  
!  "D-Flow Flexible Mesh" and "Deltares" are registered trademarks of Stichting 
!  Deltares, and remain the property of Stichting Deltares. All rights reserved.
!                                                                               
!-------------------------------------------------------------------------------

! $Id: volume_table.f90 140629 2022-01-13 14:47:46Z noort $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_kernel/compute/volume_table.f90 $

!> Module for using volume tables at 1d nodes for the computation of the total volume of water in a node.
module m_VolumeTables
   
   use messageHandling
   use m_GlobalParameters

   implicit none

   private

   character(len=11), parameter :: volumeTableFileType = 'volumeTable'
   integer, parameter :: VolumeTableFileMajorVersion      = 1
   integer, parameter :: VolumeTableFileMinorVersion      = 0


   public makeVolumeTables

   interface dealloc
      module procedure deallocVolTables
   end interface
   
   !> Derived type for storing multiple tables relevant for volume calculations.
   !! Includes type-bound procedures for accessing the tables.
   type, public :: t_voltable
      integer :: count                                              !< Number of levels in the volume table. 
      logical :: hasSummerdike                                      !< Indicates whether on 1 or more links a summerdike is attached. 
                                                                    !< A summerdike has a hysteresis. As a result the array vol contains
                                                                    !< the volumes corresponding to the rising part of the hysteresis
      logical :: hasDecreasingWidths                                  !< In case of a simulation with the Nested Newton solver, a distinction
                                                                    !< between a non-decreasing part of the cross section is made and a
                                                                    !< non-increasing part. In case of Nested Newton and 1 or more 
                                                                    !< surrounding closed cross sections, volDecreasing is allocated and
                                                                    !< filled.
      logical :: hysteresis                                         !< hysteresis value for summerdike
      double precision :: bedLevel                                  !< The bed level at the location of the volume table.
      double precision :: topLevel                                  !< Highest level (w.r.t. the bed level) of the surrounding cross sections
      double precision, allocatable, dimension(:) :: vol            !< Volume at each level of the table.
      double precision, allocatable, dimension(:) :: sur            !< Surface area at each level of the table.
      double precision, allocatable, dimension(:) :: volSummerdike  !< Volume table for decreasing levels behind summer dikes (for hysteresis)
      double precision, allocatable, dimension(:) :: surSummerdike  !< Surface area table for decreasing levels behind summer dikes (for hysteresis)
      double precision, allocatable, dimension(:) :: volDecreasing  !< Volume table for decreasing widths (Nested Newton)
      double precision, allocatable, dimension(:) :: surDecreasing  !< Surface area table for decreasing widths (Nested Newton)
   contains
      procedure, pass :: alloc               => allocVoltable                !< Allocates the allocatable arrays in this structure
      procedure, pass :: dealloc             => deallocVoltable              !< Deallocates the allocatable arrays in this structure
      procedure, pass :: getVolume           => getVolumeVoltable            !< Returns the volume for a given water level
      procedure, pass :: getSurface          => getSurfaceVoltable           !< Returns the surface area for a given water level
      procedure, pass :: getVolumeDecreasing => getVolumeDecreasingVoltable  !< Returns the volume which is the result of a decreasing width for a given water level
      procedure, pass :: getSurfaceDecreasing=> getSurfaceDecreasingVoltable !< Returns the decreasing surface area for a given water level
   end type
   
   type(t_voltable),       public, allocatable, dimension(:)   :: vltb  !< 1D Volume tables

   contains
   
   !> Allocate the volume table arrays and initialize to 0
   subroutine allocVoltable(this)
      use m_flowparameters
   
      class(t_voltable) :: this
      
      if (this%count < 0) then
         return
      end if

      allocate(this%vol(this%count))
      allocate(this%sur(this%count))
      this%vol   = 0.0d0
      this%sur   = 0.0d0

      if (this%hasDecreasingWidths) then
         allocate(this%volDecreasing(this%count))
         allocate(this%surDecreasing(this%count))
         this%volDecreasing   = 0.0d0
         this%surDecreasing   = 0.0d0
      endif
   end subroutine allocVoltable
   
   !> Deallocate the volume table arrays
   subroutine deallocVoltable(this)
      class(t_voltable) :: this
      
      if (allocated(this%vol))            deallocate(this%vol)
      if (allocated(this%sur))            deallocate(this%sur)
      if (this%hasDecreasingWidths) then
         if (allocated(this%volDecreasing))  deallocate(this%volDecreasing)
         if (allocated(this%surDecreasing))  deallocate(this%surDecreasing)
      endif
   end subroutine deallocVoltable

   !> Retrieve the volume for given volume table and water level
   double precision function getVolumeVoltable(this, level)
      use unstruc_channel_flow
      class(t_voltable)             :: this
      double precision, intent(in)  :: level    !< water level
      
      integer           :: index
      double precision  :: heightIncrement
      index = min(int( max(0d0,level-this%bedLevel)/ tableIncrement)+1,this%count)
      
      heightIncrement = max(0d0, ( (level-this%bedLevel) - dble(index-1) * tableIncrement ))
      
      getVolumeVoltable = this%vol(index) + this%sur(index) * heightIncrement
      
   end function getVolumeVoltable
   
   !> Retrieve the surface area for given volume table and water level
   double precision function getSurfaceVoltable(this, level)
      use unstruc_channel_flow
      
      class(t_voltable)             :: this
      double precision, intent(in)  :: level    !< water level
      
      integer           :: index
      index = min(int( max(0d0,level-this%bedLevel)/ tableIncrement)+1,this%count)
      
      getSurfaceVoltable = this%sur(index)
      
   end function getSurfaceVoltable

   !> Returns the volume which is the result of a decreasing width for a given water level
   double precision function getVolumeDecreasingVoltable(this, level)
      use unstruc_channel_flow
   
      class(t_voltable)             :: this
      double precision, intent(in)  :: level    !< water level
      
      integer           :: index
      double precision  :: heightIncrement
      index = min(int( max(0d0,level-this%bedLevel)/ tableIncrement)+1,this%count)
      
      heightIncrement = ( (level-this%bedLevel) - dble(index-1) * tableIncrement )
      
      getVolumeDecreasingVoltable = this%volDecreasing(index) + this%surDecreasing(index) * heightIncrement
      
   end function getVolumeDecreasingVoltable
   
   !> Retrieve the surface area for given volume table and water level
   double precision function getSurfaceDecreasingVoltable(this, level)
      use unstruc_channel_flow
   
      class(t_voltable)             :: this
      double precision, intent(in)  :: level    !< water level
      
      integer           :: index
      index = min(int( max(0d0,level-this%bedLevel)/ tableIncrement)+1,this%count)
      
      getSurfaceDecreasingVoltable = this%surDecreasing(index)
      
   end function getSurfaceDecreasingVoltable

   !> Generate the volume tables, by using GetCSParsTotal.
   subroutine makeVolumeTables()

      use unstruc_channel_flow
      use unstruc_files
      use m_flowparameters
      use m_flowgeom
      use m_GlobalParameters
      use m_Storage
      use m_flow
      use m_missing

      integer :: ndx1d
      integer :: nstor
      integer :: nod
      integer :: n
      integer :: LL, L
      integer :: i, j
      integer :: index
      integer :: jacustombnd1d, ibndsect

      double precision :: height
      double precision :: level
      double precision :: dxL
      double precision :: area, areadecr
      double precision :: width, widthdecr
      double precision :: topLevel
      double precision :: bobAboveBedLevel

      type(t_chainage2cross), dimension(:,:), pointer :: line2cross
      type(t_CrossSection), pointer, dimension(:)     :: cross
      type(t_storage), dimension(:), pointer          :: stors
      if (useVolumeTableFile) then
         volumeTableFile = defaultFilename('volumeTables')
         if (readVolumeTables()) then
            return
         endif
      endif

      line2cross => network%adm%line2cross
      cross => network%crs%cross
      stors => network%storS%stor

      ndx1d = ndx - ndx2d     ! include also the 2d boundary nodes.
      allocate(vltb(ndx1d))
      do n = 1, ndx1d
         vltb(n)%count = 0
         vltb(n)%topLevel = 0d0
      enddo

      ! determine the highest level for the storage nodes
      nstor = network%stors%count
      if (nstor > 0) then
         stors => network%stors%stor
         do i = 1, nstor
            nod = stors(i)%gridPoint 
            n = nod-ndx2d
            if (n > 0) then
               vltb(n)%topLevel = max(vltb(n)%topLevel, getTopLevel(stors(i))) - bl(nod)
            endif
         enddo
      endif
      
      do n = 1, ndx1d
         vltb(n)%hasDecreasingWidths = nonlin1D >= 2
         nod = n+ndx2d
         vltb(n)%bedLevel = bl(nod)
         
         ! determine highest level (characteristic height) of all incoming and outgoing links to node nod
         do LL = 1, nd(nod)%lnx
            L = nd(nod)%ln(LL)
            index = 2
            if (L < 0) then
               ! link from this flow node
               bobAboveBedLevel = bob0(1,-L) - bl(nod) 
            else
               ! link to this node
               bobAboveBedLevel = bob0(2,L) - bl(nod) 
            endif
            L = iabs(L)
            if (iabs(kcu(L)) ==1) then
               if (L > lnxi) then                      ! for 1D boundary links, refer to attached link
                  L = LBND1D(L)
               endif
               if (line2cross(L,index)%c1 > 0) then
                  ! The toplevel is the highest point w.r.t. the bedlevel in that point.
                  ! Since the bed level and the bob of the link can be different, the actual highest point of the 
                  ! cross section is the characteristic height + bob - bl
                  topLevel = cross(line2cross(L,index)%c1)%charheight + bobAboveBedLevel
                  if (topLevel > vltb(n)%topLevel) then
                     vltb(n)%topLevel = topLevel
                  endif
                  topLevel = cross(line2cross(L,index)%c2)%charheight + bobAboveBedLevel
                  if (topLevel > vltb(n)%topLevel) then
                     vltb(n)%topLevel = topLevel
                  endif
               endif
            endif
         enddo

         ! Make sure the volume table consists of at least two levels
         vltb(n)%count = max(2,int(vltb(n)%topLevel / tableIncrement) + 2)
         call vltb(n)%alloc()
      enddo

      ! Compute the contribution of all the storage nodes to the volume table of the corresponding node
      do i = 1, nstor
         nod = stors(i)%gridPoint 
         n = nod-ndx2d
         if (n > 0) then

            do j = 1, vltb(n)%count
               level = bl(nod) + (j-1)*tableIncrement
               vltb(n)%vol(j) = vltb(n)%vol(j) + getVolume(stors(i), level)
               ! NOTE: %sur follows at the end as an average for each level
            enddo
            vltb(n)%sur(vltb(n)%count) = vltb(n)%sur(vltb(n)%count) + GetSurface(stors(i), level)
         endif
         
      enddo
     
      ! Compute the contributions of all incoming and outgoing links to the volume table of the corresponding node
      do n = 1, ndx1d

         nod = n+ndx2d
            
         ! compute volumes, NOTE the volume at the first level is 0 by design
         do j = 2, vltb(n)%count
            height = (j-1)*tableIncrement
            do LL = 1, nd(nod)%lnx
               L = iabs(nd(nod)%ln(LL))
               if (nd(nod)%ln(LL) < 0) then
                  ! link from this flow node
                  bobAboveBedLevel = bob0(1,L) - bl(nod)
               else
                  ! link to this node
                  bobAboveBedLevel = bob0(2,L) - bl(nod)
               endif
               L = iabs(nd(nod)%ln(LL))
               if (iabs(kcu(L))==1) then                      ! for 1D boundary links, refer to attached link

                  if (dxDoubleAt1DEndNodes .and. nd(nod)%lnx == 1 ) then
                     dxL = dx(L)
                  else 
                     dxL = 0.5d0*dx(L)
                  endif

                  jacustombnd1d = 0
                  if (kcu(L) == -1 .and. allocated(bndWidth1D)) then
                     ibndsect = lnxbnd(L-lnxi)
                     if (ibndsect > 0) then
                        if (bndWidth1D(ibndsect) /= dmiss) then
                           jacustombnd1d = 1
                        end if
                     end if
                  end if

                  if (jacustombnd1d == 1) then ! This link is a 1D bnd *and* has a custom width.
                     width = bndwidth1D(ibndsect)
                     area = (height-bobAboveBedLevel)*width
                     ! Use the water level at the inner point of the boundary link

                     if (vltb(n)%hasDecreasingWidths) then
                        widthdecr = 0d0
                        areadecr  = 0d0
                     end if
                  else 
                     if (L > lnxi) then                      ! for 1D boundary links, refer to attached link
                        L = LBND1D(L)
                     endif
                  
                     ! The bed level is the lowest point of all flow links and possibly storage nodes. 
                     ! In order to take this difference into account the variable bobAboveBedLevel is used
                     call GetCSParsTotal(line2cross(L, 2), cross, height-bobAboveBedLevel, area, width, CSCalculationOption)

                     if (vltb(n)%hasDecreasingWidths) then
                        call GetCSParsTotal(network%adm%line2cross(L, 2), cross, height-bobAboveBedLevel, areadecr, widthdecr, CS_TYPE_MIN)
                     end if

                  endif
                  
                  vltb(n)%vol(j) = vltb(n)%vol(j) + dxL*area
                  
                  if (j==vltb(n)%count) then
                     ! water surface at the highest level is equal to the width*dx of the cross section at the highest level.
                     vltb(n)%sur(vltb(n)%count) = vltb(n)%sur(vltb(n)%count) + dxL*width
                  endif
                  ! compute the decreasing volumes and areas
                  if (vltb(n)%hasDecreasingWidths) then
                     vltb(n)%volDecreasing(j) = vltb(n)%volDecreasing(j) + dxL*areadecr
                     if (j==vltb(n)%count) then
                        ! water surface at the highest level is equal to the width*dx of the cross section at the highest level.
                        vltb(n)%surDecreasing(vltb(n)%count) = vltb(n)%surDecreasing(vltb(n)%count) + dxL*widthdecr
                     endif
                  endif
               endif
            enddo
            ! compute water surface area
            vltb(n)%sur(j-1) = (vltb(n)%vol(j) - vltb(n)%vol(j-1))/tableIncrement
            if (vltb(n)%hasDecreasingWidths) then
               vltb(n)%surDecreasing(j-1) = (vltb(n)%volDecreasing(j) - vltb(n)%volDecreasing(j-1))/tableIncrement
            endif
         enddo
      enddo

      if (useVolumeTableFile) then
         call writeVolumeTables()
      endif

   end subroutine makeVolumeTables

   !> Deallocate all volume tables.
   subroutine deallocVolTables()

      integer i
      
      if (allocated(vltb)) then
         do i = 1, size(vltb)
            call vltb(i)%dealloc()
         enddo
         deallocate(vltb)
      endif
      
   end subroutine deallocVolTables
   
   !> write the volume table to a binary file.
   subroutine writeVolumeTables()

      use m_flowgeom
      use m_flowparameters
      use m_GlobalParameters
      use unstruc_channel_flow

      integer :: ibin
      integer :: i, n, istat
      integer :: ndx1d
      integer :: count

      open(newunit=ibin, file=volumeTableFile, form='unformatted', access='stream', iostat=istat)
      if (istat/=0) then
         call SetMessage(LEVEL_WARN, 'Something went wrong during the opening of binary volume table file: '// trim(volumeTableFile))
         return
      endif

      write(ibin) volumeTableFileType
      write(ibin) VolumeTableFileMajorVersion, VolumeTableFileMinorVersion

      ndx1d = ndx - ndx2d
      write(ibin) ndx1d, nonlin1D

      do n = 1, ndx1d
         count = vltb(n)%count
         write(ibin) count
         write(ibin) vltb(n)%hasSummerdike
         write(ibin) vltb(n)%hasDecreasingWidths
         write(ibin) vltb(n)%hysteresis
         write(ibin) vltb(n)%bedLevel
         write(ibin) vltb(n)%topLevel
         write(ibin) (vltb(n)%vol(i), i = 1, count)
         write(ibin) (vltb(n)%sur(i), i = 1, count)
         if (vltb(n)%hasSummerdike) then
            write(ibin) (vltb(n)%volSummerdike(i), i = 1, count)
            write(ibin) (vltb(n)%surSummerdike(i), i = 1, count)
         endif
         if (vltb(n)%hasDecreasingWidths) then
            write(ibin) (vltb(n)%volDecreasing(i), i = 1, count)
            write(ibin) (vltb(n)%surDecreasing(i), i = 1, count)
         endif
      enddo
      
      close(ibin)

   end subroutine writeVolumeTables

   !> Read the volume tables from a previously saved binary file.
   !> The function returns .true. when succesfull, otherwise .false. is returned
   logical function readVolumeTables()

      use m_flowgeom
      use m_GlobalParameters
      use unstruc_channel_flow
      use m_flowparameters
      use messageHandling

      integer :: ibin
      integer :: i, n, istat
      integer :: ndx1d
      integer :: count
      integer :: nonlin1D_file
      integer :: majorVersion, minorVersion
      logical :: fileExist
      character(len=11) :: fileType_

      readVolumeTables = .false.
      inquire(file=volumeTableFile, exist=fileExist)

      if (.not. fileExist) then
         call SetMessage(LEVEL_INFO, 'Volume table file: '//trim(volumeTableFile)//' does not exist, generating volume tables from scratch.')
         return
      endif

      open(newunit=ibin, file=volumeTableFile, status='old', form='unformatted', access='stream', action='read', iostat=istat)

      if (istat/=0) then
         call SetMessage(LEVEL_WARN, 'Something went wrong during the opening of binary volume table file: '// trim(volumeTableFile))
         return
      endif

      read(ibin, iostat = istat) fileType_
      if (trim(fileType_) /= volumeTableFileType .or. istat/=0) then
         call SetMessage(LEVEL_WARN, trim(volumeTableFile) // ' is not a volume table file, the file type on the file is '''//trim(fileType_)// '''.')
         close(ibin)
         return
      endif

      read(ibin) majorVersion, minorVersion
      if (majorVersion /= VolumeTableFileMajorVersion) then
         write(msgbuf,'(''The major version of '' , a,  '' = '', i0, ''. This is not compatible with the current version'', i0)') &
                  trim(volumeTableFile), majorVersion, VolumeTableFileMajorVersion
         call warn_flush()
         close(ibin)
         return
      endif
      read(ibin) ndx1d, nonlin1D_file
      
      if (ndx1d /= ndx - ndx2d) then
         call SetMessage(LEVEL_WARN, trim(volumeTableFile)//' is not compatible with the current model, the number of 1d cells are different.')
         return
      endif

      if (nonlin1D_file /= nonlin1d) then
         msgbuf = 'The selected method for solving the nonlinear iteration (='''// trim(getNonlin(nonlin1d))//'''), and the nonlinear iteration method '
         call warn_flush()
         msgbuf = 'on ''' // trim(volumeTableFile)// ''' are different (='''//trim(getNonlin(nonlin1D_file))//''').'
         call warn_flush()
         close(ibin)
         return
      endif

      allocate(vltb(ndx1d))

      do n = 1, ndx1d
         read(ibin) count
         vltb(n)%count = count
         read(ibin) vltb(n)%hasSummerdike
         read(ibin) vltb(n)%hasDecreasingWidths
         read(ibin) vltb(n)%hysteresis
         read(ibin) vltb(n)%bedLevel
         read(ibin) vltb(n)%topLevel
         call vltb(n)%alloc()
         read(ibin) (vltb(n)%vol(i), i = 1, count)
         read(ibin) (vltb(n)%sur(i), i = 1, count)
         if (vltb(n)%hasSummerdike) then
            read(ibin) (vltb(n)%volSummerdike(i), i = 1, count)
            read(ibin) (vltb(n)%surSummerdike(i), i = 1, count)
         endif
         if (vltb(n)%hasDecreasingWidths) then
            read(ibin) (vltb(n)%volDecreasing(i), i = 1, count)
            read(ibin) (vltb(n)%surDecreasing(i), i = 1, count)
         endif
      enddo
      
      close(ibin)
      readVolumeTables = .true.

   end function readVolumeTables

   function getNonlin(nlin1D) result(string)

      character(len=30) :: string
      integer, intent(in) ::nlin1d

      select case(nlin1d)
      case(1)
         string = 'Preismann'
      case(2)
         string = 'Nested Newton'
      case(3)
         string= 'Improved Nested Newton'
      case default
         string = 'Unknown'   
      end select
   end function

end module m_volumeTables