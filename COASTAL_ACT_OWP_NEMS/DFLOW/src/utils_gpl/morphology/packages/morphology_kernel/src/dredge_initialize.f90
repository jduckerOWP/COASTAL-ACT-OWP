module m_dredge_initialize
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
!  $Id: dredge_initialize.f90 140618 2022-01-12 13:12:04Z klapwijk $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/utils_gpl/morphology/packages/morphology_kernel/src/dredge_initialize.f90 $
!-------------------------------------------------------------------------------
    private
    
    public dredge_initialize
    
    contains

subroutine dredge_initialize(dadpar, idomain, ndomains, lundia, error, comm)
!!--declarations----------------------------------------------------------------
    use precision
    use m_alloc
    use message_module, only: write_error
    use dredge_data_module
    !
    implicit none
!
! Global variables
!
    type (dredge_type)       , target        :: dadpar
    integer                  , intent(in)    :: idomain
    integer                  , intent(in)    :: ndomains
    integer                  , intent(in)    :: lundia
    logical                  , intent(out)   :: error
!
    interface
       subroutine comm(a, n, error, msgstr)
           use precision
           integer               , intent(in)    :: n      ! length of real array
           real(fp), dimension(n), intent(inout) :: a      ! real array to be accumulated
           logical               , intent(out)   :: error  ! error flag
           character(*)          , intent(out)   :: msgstr ! string to pass message
       end subroutine comm
    end interface
!
! Local variables
!
    real(fp)      , dimension(:)   , pointer :: globalareadred
    real(fp)      , dimension(:)   , pointer :: localareadump
    real(fp)      , dimension(:)   , pointer :: globalareadump
    integer                        , pointer :: dredge_domainnr
    integer                        , pointer :: dredge_ndomains
    integer                        , pointer :: nadred
    integer                        , pointer :: nadump
    integer                        , pointer :: nasupl
    logical                        , pointer :: firstdredge
    type (dredtype), dimension(:)  , pointer :: dredge_prop
    type (dumptype), dimension(:)  , pointer :: dump_prop
    !
    integer                         :: i
    integer                         :: ia
    integer                         :: ib
    integer                         :: id
    integer                         :: istat
    integer                         :: j
    integer                         :: np
    integer                         :: in_ndomains
    integer                         :: npnt
    integer                         :: npnt_global
    integer                         :: npnt_halo
    integer                         :: localoffset
    integer, dimension(:), pointer  :: tmp_nmglob
    integer, dimension(:), pointer  :: tmp_nm
    real(fp), dimension(:), pointer :: numpoints
    real(fp), dimension(:), pointer :: nmglobf
    character(80)                   :: msgstr
    type(dredtype),         pointer :: pdredge
    type(dumptype),         pointer :: pdump
!
!! executable statements -------------------------------------------------------
!
    globalareadred      => dadpar%globalareadred
    localareadump       => dadpar%localareadump
    globalareadump      => dadpar%globalareadump
    dredge_domainnr     => dadpar%dredge_domainnr
    dredge_ndomains     => dadpar%dredge_ndomains
    nadred              => dadpar%nadred
    nadump              => dadpar%nadump
    nasupl              => dadpar%nasupl
    firstdredge         => dadpar%firstdredge
    dredge_prop         => dadpar%dredge_prop
    dump_prop           => dadpar%dump_prop
    error               =  .false.
    !
    if (.not.firstdredge) return
    !
    globalareadump = localareadump
    if (ndomains > 1) then
       !
       ! Start communication with other domains
       ! Determine number of domains that use dredging
       ! Determine "rank" of current domain (use 1-based number instead of
       ! the 0-based number returned by C routine)
       !
       dredge_domainnr = idomain
       dredge_ndomains = ndomains
       !
       ! For all dredge and dump areas count the global number of points
       ! Due to the way comm is implemented we need to
       ! communicate via floating point array.
       !
       allocate(numpoints(dredge_ndomains*2), stat = istat)
       !
       ! For each dredge area count the global number of points
       !
       do ia = 1, nadred+nasupl
          pdredge => dredge_prop(ia)
          if (dredge_prop(ia)%itype == DREDGETYPE_NOURISHMENT) cycle
          !
          numpoints = 0.0_fp
          numpoints(                dredge_domainnr) = real(pdredge%npnt,fp)
          numpoints(dredge_ndomains+dredge_domainnr) = real(size(pdredge%nm,1),fp)
          !
          call comm(numpoints, 2*dredge_ndomains, error, msgstr)
          if (error) goto 999
          !
          in_ndomains = 0
          npnt_global = 0
          localoffset = 0
          do id = 1,  dredge_ndomains
             np = nint(numpoints(id))
             if (numpoints(dredge_ndomains+id)>0.0_fp) in_ndomains = in_ndomains + 1
             if (np>0) then
                npnt_global = npnt_global + np
                if (id<dredge_domainnr) localoffset = localoffset + np
             endif
          enddo
          if (in_ndomains <= 1) then
             pdredge%in1domain = .true.
          else
             npnt         = pdredge%npnt
             pdredge%npnt = npnt_global
             !
             ! Reallocate and shift
             !
             istat = 0
             call reallocP(pdredge%area         ,npnt_global,fill=0.0_fp,shift=localoffset,stat=istat)
             call comm(pdredge%area, npnt_global, error, msgstr)
             if (error) goto 999
             !
             npnt_halo = size(pdredge%nmglob,1) - npnt
             allocate(tmp_nmglob(npnt+npnt_halo), tmp_nm(npnt+npnt_halo), stat=istat)
             if (istat==0) then
                tmp_nmglob = pdredge%nmglob
                tmp_nm     = pdredge%nm
                call reallocP(pdredge%nmglob       ,npnt_global,fill=0,keepExisting=.false.,stat=istat)
                call reallocP(pdredge%nm           ,npnt_global,fill=0,keepExisting=.false.,stat=istat)
                allocate(nmglobf(npnt_global), stat=istat)
             endif
             if (istat/=0) then
                error  = .true.
                msgstr = 'Dredge: memory realloc error'
                goto 999
             endif
             !
             nmglobf = 0.0_fp
             do i = 1, npnt
                 nmglobf(localoffset+i)    = real(tmp_nmglob(i),fp)
                 pdredge%nm(localoffset+i) = tmp_nm(i)
             enddo
             call comm(nmglobf, npnt_global, error, msgstr)
             if (error) goto 999
             !
             pdredge%nmglob = nint(nmglobf)
             do i = npnt+1, npnt+npnt_halo
                do j = 1, npnt_global
                    if (tmp_nmglob(i) == pdredge%nmglob(j)) then
                        pdredge%nm(j) = tmp_nm(i)
                    endif
                enddo
             enddo
             deallocate(nmglobf, tmp_nm, tmp_nmglob)
             !
             call reallocP(pdredge%hdune        ,npnt_global      ,shift=localoffset,stat=istat)
             call reallocP(pdredge%dz_dredge    ,npnt_global      ,shift=localoffset,stat=istat)
             call reallocP(pdredge%reflevel     ,npnt_global      ,shift=localoffset,stat=istat)
             call reallocP(pdredge%dunetoplevel ,npnt_global      ,shift=localoffset,stat=istat)
             call reallocP(pdredge%triggerlevel ,npnt_global      ,shift=localoffset,stat=istat)
             call reallocP(pdredge%bedlevel     ,npnt_global      ,shift=localoffset,stat=istat)
             call reallocP(pdredge%troughlevel  ,npnt_global      ,shift=localoffset,stat=istat)
             call reallocP(pdredge%sedimentdepth,npnt_global      ,shift=localoffset,stat=istat)
             call reallocP(pdredge%sortvar      ,npnt_global      ,shift=localoffset,stat=istat)
             call reallocP(pdredge%inm          ,npnt_global      ,shift=localoffset,stat=istat)
             ! nm(i)=0 for points outside this domain is used in this subroutine
             call reallocP(pdredge%triggered    ,npnt_global      ,shift=localoffset,stat=istat)
             !
             if (istat/=0) then
                error  = .true.
                msgstr = 'Dredge: memory realloc error'
                goto 999
             endif
             !
             globalareadred(ia) = 0.0_fp
             do i = 1,npnt_global
                pdredge%inm(i) = i
                globalareadred(ia) = globalareadred(ia) + pdredge%area(i)
             enddo
          endif
       enddo
       !
       ! For each dump area count the global number of points
       !
       if (.not.error) then
          do ib = 1, nadump
             pdump => dump_prop(ib)
             !
             numpoints = 0.0_fp
             numpoints(                dredge_domainnr) = real(pdump%npnt,fp)
             numpoints(dredge_ndomains+dredge_domainnr) = real(size(pdump%nm,1),fp)
             !
             call comm(numpoints, dredge_ndomains, error, msgstr)
             if (error) goto 999
             !
             in_ndomains = 0
             npnt_global = 0
             localoffset = 0
             do id = 1,  dredge_ndomains
                np = nint(numpoints(id))
                if (numpoints(dredge_ndomains+id)>0.0_fp) in_ndomains = in_ndomains + 1
                if (np>0) then
                   npnt_global = npnt_global + np
                   if (id<dredge_domainnr) localoffset = localoffset + np
                endif
             enddo
             if (in_ndomains <= 1) then
                pdump%in1domain = .true.
             else
                pdump%npnt = npnt_global
                !
                ! Reallocate and shift
                !
                istat = 0
                call reallocP(pdump%area    ,npnt_global,fill=0.0_fp,shift=localoffset,stat=istat)
                call comm(pdump%area, npnt_global, error, msgstr)
                if (error) goto 999
                !
                npnt_halo = size(pdump%nmglob,1) - npnt
                allocate(tmp_nmglob(npnt+npnt_halo), tmp_nm(npnt+npnt_halo), stat=istat)
                if (istat==0) then
                   tmp_nmglob = pdump%nmglob
                   tmp_nm     = pdump%nm
                   call reallocP(pdump%nmglob       ,npnt_global,fill=0,keepExisting=.false.,stat=istat)
                   call reallocP(pdump%nm           ,npnt_global,fill=0,keepExisting=.false.,stat=istat)
                   allocate(nmglobf(npnt_global), stat=istat)
                endif
                if (istat/=0) then
                   error  = .true.
                   msgstr = 'Dredge: memory realloc error'
                   goto 999
                endif
                !
                nmglobf = 0.0_fp
                do i = 1, npnt
                    nmglobf(localoffset+i)  = real(tmp_nmglob(i),fp)
                    pdump%nm(localoffset+i) = tmp_nm(i)
                enddo
                call comm(nmglobf, npnt_global, error, msgstr)
                if (error) goto 999
                !
                pdump%nmglob = nint(nmglobf)
                do i = npnt+1, npnt+npnt_halo
                   do j = 1, npnt_global
                       if (tmp_nmglob(i) == pdump%nmglob(j)) then
                           pdump%nm(j) = tmp_nm(i)
                       endif
                   enddo
                enddo
                deallocate(nmglobf, tmp_nm, tmp_nmglob)
                !
                call reallocP(pdump%hdune   ,npnt_global      ,shift=localoffset,stat=istat)
                call reallocP(pdump%reflevel,npnt_global      ,shift=localoffset,stat=istat)
                call reallocP(pdump%bedlevel,npnt_global      ,shift=localoffset,stat=istat)
                call reallocP(pdump%dz_dump ,npnt_global      ,shift=localoffset,stat=istat)
                call reallocP(pdump%sortvar ,npnt_global      ,shift=localoffset,stat=istat)
                call reallocP(pdump%inm     ,npnt_global      ,shift=localoffset,stat=istat)
                ! nm(i)=0 for points outside this domain is used in this subroutine
                call reallocP(pdump%nm      ,npnt_global      ,fill=0,shift=localoffset,stat=istat)
                !
                if (istat/=0) then
                   error  = .true.
                   msgstr = 'Dredge: memory realloc error'
                   goto 999
                endif
                !
                do i = 1,npnt_global
                   pdump%inm(i) = i
                enddo
             endif
          enddo
       endif
       !
       deallocate(numpoints, stat = istat)
       !
       ! Communicate dump areas with other domains
       !
       if (.not.error) call comm(globalareadump, nadump, error, msgstr)
    else
       !
       ! Only one domain, so no exchange needed for any dredge or dump area
       !
       dredge_domainnr = 1
       do ia = 1, nadred+nasupl
          dredge_prop(ia)%in1domain = .true.
       enddo
       do ib = 1, nadump
          dump_prop(ib)%in1domain = .true.
       enddo
    endif
    !
999 if (error) then
       call write_error(msgstr, unit=lundia)
    else
       firstdredge = .false.
    endif
end subroutine dredge_initialize

end module m_dredge_initialize