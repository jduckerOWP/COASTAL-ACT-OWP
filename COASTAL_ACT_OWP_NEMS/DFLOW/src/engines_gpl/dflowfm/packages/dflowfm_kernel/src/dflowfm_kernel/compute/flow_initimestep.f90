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

! $Id: flow_initimestep.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_kernel/compute/flow_initimestep.f90 $

 subroutine flow_initimestep(jazws0, iresult)                     ! intialise flow timestep, also called once after flowinit
 use timers
 use m_flowtimes
 use m_flow
 use m_flowgeom
 use unstruc_model, only: md_ident, md_restartfile
 use m_xbeach_data, only: swave, Lwave, uin, vin, cgwav, instat
 use unstruc_channel_flow
 use m_1d_structures, only: initialize_structures_actual_params, set_u0isu1_structures
 use dfm_error
 use MessageHandling
 use m_partitioninfo
 implicit none

 integer              :: jazws0
 integer, intent(out) :: iresult !< Error status, DFM_NOERR==0 if succesful.
 integer              :: k, n, LL
 integer              :: ierror

 iresult = DFM_GENERICERROR

 call timstrt('Initialise timestep', handle_inistep)

 if (jazws0.eq.0)  s0 = s1                           ! progress water levels

 call bathyupdate()                                  ! only if jamorf == 1

 if(jazws0.eq.1 .and. len_trim(md_restartfile)>0) then
    ! ini timestep right after reading a rst file: prepare derived hs/ucx/etc.
    ! for writing of first map file snapshot, i.e., based on the s0/u0 values.
    hs = s0 - bl
 else
    hs = s1 - bl                                     ! total water height
 endif

! due to tolerance in poshcheck, hs may be smaller than 0 (but larger than -1e-10)
 hs = max(hs,0d0)


 if (nshiptxy > 0) then  ! quick fix only for ships
     call setdt()
 endif

 tim1bnd = max(time0+dts, tim1bnd)

 call timstrt('Set boundaries      ', handle_extra(38)) ! Start bnd
 call flow_setexternalforcingsonboundaries(tim1bnd , iresult)  ! boundary forcings
 call timstop(handle_extra(38)) ! End bnd

 if (iresult /= DFM_NOERR) then
    write (msgbuf,*) ' Error found in EC-module ' ; call err_flush()
    if (jampi == 1) then
       write(msgbuf,*) 'Error occurs on one or more processes when setting external forcings on boundaries at time=', tim1bnd;
       call err_flush()
       ! Terminate all MPI processes
       call abort_all()
    endif
    goto 888
 end if

 if (tlfsmo > 0d0 ) then
    alfsmo  = (tim1bnd - tstart_user) / tlfsmo
 endif

! apply XBeach wave boundary conditions
 if (jawave .eq. 4) then
    if ( swave.eq.1 ) then
       call xbeach_wave_bc()
       call xbeach_apply_wave_bc()
       if (.not.(trim(instat)=='stat') .and. .not.(trim(instat)=='stat_table')) then
          call xbeach_wave_compute_celerities()        ! for setdt
       else
          call xbeach_wave_compute_statcelerities(iresult)
       endif
    else
       uin = 0d0
       vin = 0d0
    endif
    call xbeach_flow_bc()
 end if

 call timstrt('u0u1        ', handle_extra(42)) ! Start u0u1
 if (jazws0.eq.0) then
    u0 = u1                           ! progress velocities
    call set_u0isu1_structures(network%sts)
 endif
 call timstop(handle_extra(42)) ! End u0u1


 advi = 0d0
 adve = 0d0

 call timstrt('Sethuau     ', handle_extra(39)) ! Start huau
 call sethu(jazws0)

 call setau()                                        ! set au and cfuhi for conveyance after limited h upwind at u points
 call timstop(handle_extra(39)) ! End huau

 call timstrt('Setumod     ', handle_extra(43)) ! Start setumod
 if (newcorio == 1) then
    call setumodnew(jazws0)
 else
    call setumod(jazws0)                             ! set cell center velocities, should be here as prior to 2012 orso
 endif
 call timstop(handle_extra(43)) ! End setumod

 call timstrt('Set conveyance       ', handle_extra(44)) ! Start cfuhi
 call setcfuhi()                                     ! set frictioncoefficient
 call timstop(handle_extra(44)) ! End cfuhi

 if (kmx == 0 .and. javeg > 0) then                  ! overwrite cfuhi in 2D with veg in plant area's
    call setbaptist()
 endif

 call timstrt('Set structures actual parameters', handle_extra(45)) ! Start structactual
 call initialize_structures_actual_params(network%sts)
 call timstop(handle_extra(45)) ! Start structactual

 if (japillar == 1 .or. japillar == 3) then
    call pillar_upd()
 endif

 ! TIDAL TURBINES: Insert equivalent calls to updturbine and applyturbines here

 call timstrt('setdt', handle_extra(40)) ! Start setdt
 if (jazws0.eq.0 .and. nshiptxy == 0)  then
    call setdt()                                     ! set computational timestep dt based on active hu's,
 end if
 call timstop(handle_extra(40)) ! End setdt

 if (nshiptxy > 0) then
     call setship()                                  ! in initimestep
 endif

 call timstrt('Compute advection term', handle_extra(41)) ! Start advec
 call advecdriver()                                  ! advec limiting for depths below chkadvdp, so should be called after all source terms such as spiralforce
 call timstop(handle_extra(41)) ! End advec

 if (jazws0.eq.1)  then
    call makeq1qaAtStart()
    call setkfs()
 endif

 if ( jaimplicit.eq.1 ) then
    call fillsystem_advec(ierror)
    if ( ierror.ne.0 ) goto 888
 end if

 if (jatem > 1 .and. jaheat_eachstep == 1) then
    call heatu(tim1bnd/3600d0)                                  ! from externalforcings
 endif

 call timstop(handle_inistep)

 iresult = DFM_NOERR

 return ! Return with success

888 continue
 ! Error
   end subroutine flow_initimestep
