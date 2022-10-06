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

! $Id: flow_initwaveforcings_runtime.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_kernel/prepost/flow_initwaveforcings_runtime.f90 $

function flow_initwaveforcings_runtime() result(retval)              ! This is the general hook-up to wave conditions

 use m_flowparameters
 use m_flowtimes                                     ! Two stages: 1 = collect elsets for which data is provided
 use m_flowgeom                                      !             2 = add relations between elsets and their providers
 use unstruc_model
 use unstruc_messages
 use unstruc_files
 use timespace
 use m_missing
 use m_waves
 use m_alloc
 use m_meteo

 implicit none

 logical               :: retval !< Whether init was successful or not

 integer               :: ierr
 integer               :: filetype_l
 integer               :: method_l
 character(len=1)      :: operand_l
 character(len=256)    :: qid_l

 if (extfor_wave_initialized) then
    retval = .true.
    return
 end if


 filetype_l = 14  ! netcdf
 method_l   = 7   ! only time interpolation, extrapolation allowed (online WAVE)
 operand_l  = 'O' ! Override
 kx = 1           ! default vectormax = 1
 !
call realloc(kcw, ndx, stat=ierr)
call aerr('kcw(ndx)', ierr, ndx)
kcw = 1

 qid_l = 'hrms'
 if (.not. allocated(hwavcom) ) then
    allocate ( hwavcom(ndx), stat=ierr)
    call aerr('hwavcom(ndx)', ierr, ndx)
    hwavcom = hwavuni
 endif
 success = ec_addtimespacerelation(qid_l, xz(1:ndx), yz(1:ndx), kcw, kx, md_wavefile, filetype_l, method_l, operand_l, quiet=.true.)
 if (.not.success) then
    !
    ! Most commonly, WAVE data has not been written to the com-file yet.
    ! Just try it the next timestep again
    !
    retval = .false.
    goto 888
 endif
 !
 if (jatpwav == TPWAVSMOOTH) then
    ! take smoothed peak wave period. Arjen: "Deze parameter is beter"
    qid_l = 'tps'
 elseif (jatpwav == TPWAVRELATIVE) then
    ! take relative peak wave period. Bas; scale factor required!!
    qid_l = 'rtp'
 else
    qid_l = 'tp'
 endif
 if (.not. allocated(twav) ) then
    allocate ( twav(ndx), stat=ierr)
    call aerr('twav(ndx)', ierr, ndx)
    twav = 0.0
 endif
 success = ec_addtimespacerelation(qid_l, xz(1:ndx), yz(1:ndx), kcw, kx, md_wavefile, filetype_l, method_l, operand_l, quiet=.true.)
 !
 qid_l = 'dir'
 if (.not. allocated(phiwav) ) then
    allocate ( phiwav(ndx), stat=ierr)
    call aerr('phiwav(ndx)', ierr, ndx)
    phiwav = 0.0
 endif
 success = ec_addtimespacerelation(qid_l, xz(1:ndx), yz(1:ndx), kcw, kx, md_wavefile, filetype_l, method_l, operand_l, quiet=.true.)
 !
 qid_l = 'dissurf'
 if (.not. allocated(dsurf) ) then
    allocate ( dsurf(ndx), stat=ierr)
    call aerr('dsurf(ndx)', ierr, ndx)
    dsurf = 0.0
 endif
 success = ec_addtimespacerelation(qid_l, xz(1:ndx), yz(1:ndx), kcw, kx, md_wavefile, filetype_l, method_l, operand_l, quiet=.true.)
 !
 qid_l = 'diswcap'
 if (.not. allocated(dwcap) ) then
    allocate ( dwcap(ndx), stat=ierr)
    call aerr('dwcap(ndx)', ierr, ndx)
    dwcap = 0.0
 endif
 success = ec_addtimespacerelation(qid_l, xz(1:ndx), yz(1:ndx), kcw, kx, md_wavefile, filetype_l, method_l, operand_l, quiet=.true.)
 !
 qid_l = 'fx'
 if (.not. allocated(sxwav) ) then
    allocate ( sxwav(ndx), stat=ierr)
    call aerr('sxwav(ndx)', ierr, ndx)
    sxwav = 0.0
 endif
 success = ec_addtimespacerelation(qid_l, xz(1:ndx), yz(1:ndx), kcw, kx, md_wavefile, filetype_l, method_l, operand_l, quiet=.true.)
 !
 qid_l = 'fy'
 if (.not. allocated(sywav) ) then
    allocate ( sywav(ndx), stat=ierr)
    call aerr('sywav(ndx)', ierr, ndx)
    sywav = 0.0
 endif
 success = ec_addtimespacerelation(qid_l, xz(1:ndx), yz(1:ndx), kcw, kx, md_wavefile, filetype_l, method_l, operand_l, quiet=.true.)
 !
 qid_l = 'wsbu'
 if (.not. allocated(sbxwav) ) then
    allocate ( sbxwav(ndx), stat=ierr)
    call aerr('sbxwav(ndx)', ierr, ndx)
    sbxwav = 0.0
 endif
 success = ec_addtimespacerelation(qid_l, xz(1:ndx), yz(1:ndx), kcw, kx, md_wavefile, filetype_l, method_l, operand_l, quiet=.true.)
 !
 qid_l = 'wsbv'
 if (.not. allocated(sbywav) ) then
    allocate ( sbywav(ndx), stat=ierr)
    call aerr('sbywav(ndx)', ierr, ndx)
    sbywav = 0.0
 endif
 success = ec_addtimespacerelation(qid_l, xz(1:ndx), yz(1:ndx), kcw, kx, md_wavefile, filetype_l, method_l, operand_l, quiet=.true.)
 !
 qid_l = 'mx'
 if (.not. allocated(mxwav) ) then
    allocate ( mxwav(ndx), stat=ierr)
    call aerr('mxwav(ndx)', ierr, ndx)
    mxwav = 0.0
 endif
 success = ec_addtimespacerelation(qid_l, xz(1:ndx), yz(1:ndx), kcw, kx, md_wavefile, filetype_l, method_l, operand_l, quiet=.true.)
 !
 qid_l = 'my'
 if (.not. allocated(mywav) ) then
    allocate ( mywav(ndx), stat=ierr)
    call aerr('mywav(ndx)', ierr, ndx)
    mywav = 0.0
 endif
 success = ec_addtimespacerelation(qid_l, xz(1:ndx), yz(1:ndx), kcw, kx, md_wavefile, filetype_l, method_l, operand_l, quiet=.true.)
 !
 qid_l = 'ubot'
 if (.not. allocated(uorbwav) ) then
    allocate ( uorbwav(ndx), stat=ierr)
    call aerr('uorbwav(ndx)', ierr, ndx)
    uorbwav = 0.0
 endif
 success = ec_addtimespacerelation(qid_l, xz(1:ndx), yz(1:ndx), kcw, kx, md_wavefile, filetype_l, method_l, operand_l, quiet=.true.)
 !
 retval = success

888 continue
 extfor_wave_initialized = retval ! Becomes .true. or .false., depending on whether the timespace relations have been created succesfully.

end function flow_initwaveforcings_runtime
