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

! $Id: xbeachwaves.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_kernel/compute_waves/surfbeat/xbeachwaves.f90 $

subroutine xbeach_wave_input
!! Start logging
!! Read input from params.txt
   use m_flowgeom
   use m_xbeach_data
   use m_xbeach_readkey
   use m_xbeach_filefunctions

   implicit none

   logical, save                     :: init = .false.

   if (.not. init) then
      !! Start logging
      call start_logfiles(0)
      call writelog_startup()
      call xbeach_all_input()
      call writelog('ls','','Initializing .....')
   else
      call writelog_startup()
      call xbeach_all_input()
      call writelog('ls','','Reinitialized model .....')
   end if
   init = .true.
end subroutine xbeach_wave_input 


subroutine xbeach_all_input()
   use m_physcoef
   use m_flowgeom
   use m_xbeach_data
   use m_xbeach_readkey
   use m_xbeach_filefunctions
   use m_xbeach_errorhandling
   use m_xbeach_paramsconst
   use m_flowtimes
   use m_sediment, only: stm_included
   use m_samples
   use m_missing
   use m_wind, only: jawind
   use unstruc_model

   implicit none

   character(slen)                                     :: testc,line
   character(slen)                                     :: dummystring
   character(slen), dimension(:), allocatable          :: allowednames,oldnames

   integer                                             :: filetype,mmax,ier,ic
   integer                                             :: minp0, jdla, nm, ibnd, kb, ki
   logical                                             :: comment
   logical                                             :: fe1,fe2

   call writelog('sl','','Reading input parameters: ')
   !
   ! Check params.txt exists
   !
   call check_file_exist(md_surfbeatfile)
   !
   !
   ! Physical processes
   call writelog('l','','--------------------------------')
   call writelog('l','','Physical processes: ')
   swave       = readkey_int (md_surfbeatfile,'swave',         1,        0,     1, strict=.true.)
   lwave       = readkey_int (md_surfbeatfile,'lwave',         1,        0,     1, strict=.true.)
   windmodel   = readkey_int (md_surfbeatfile,'windmodel',     0,        0,     1, strict=.true.)
   !
   ! Grid parameters
   call writelog('l','','--------------------------------')
   call writelog('l','','Directional wave grid parameters: ')
   thetamin = readkey_dbl (md_surfbeatfile,'thetamin', -90.d0,    -180.d0,  180.d0,required=(swave==1))
   thetamax = readkey_dbl (md_surfbeatfile,'thetamax',  90.d0,    -180.d0,  180.d0,required=(swave==1))
   dtheta   = readkey_dbl (md_surfbeatfile,'dtheta',    10.d0,      0.1d0,   20.d0,required=(swave==1))
   thetanaut= readkey_int (md_surfbeatfile,'thetanaut',    0,        0,     1)
   !
   !
   ! Wave boundary condition parameters
   call writelog('l','','--------------------------------')
   call writelog('l','','Wave boundary condition parameters: ')
   allocate(allowednames(12),oldnames(12))
   allowednames=(/'stat        ','bichrom     ','ts_1        ','ts_2        ','jons        ','swan        ', &
      'vardens     ','reuse       ','off         ','stat_table  ','jons_table  '/)
   oldnames=(/'0 ','1 ','2 ','3 ','4 ','5 ','6 ','7 ','9 ','40','41'/)
   !             function =   file         key      default  n allowed  n old allowed  allowed names  old allowed names
   instat  = readkey_str(md_surfbeatfile, 'instat', 'bichrom', 11, 11, allowednames, oldnames, required=(swave==1))
   deallocate(allowednames,oldnames)
   !if (  trim(instat)=='jons' .or. &
   !   trim(instat)=='swan' .or. &
   !   trim(instat)=='vardens'.or. &
   !   trim(instat)=='stat_table' .or. &
   !   trim(instat)=='jons_table' &
   !   )then
   !bcfile = readkey_name(md_surfbeatfile,'bcfile')
   !call check_file_exist(bcfile)
   !call checkbcfilelength(tstop_user-tstart_user,instat,bcfile, nspectrumloc, filetype)
   !!filetype = 0
   !elseif (trim(instat)=='reuse') then
   !   ! TO DO: check file length is done after recomputation of tstop due to morfacopt
   !   ! at the end of this subroutine.
   !   ! JRE: TO DO: implement reuse bc
   !   inquire(file='ebcflist.bcf',exist=fe1)
   !   inquire(file='qbcflist.bcf',exist=fe2)
   !
   !
   !   if (.not. (fe1 .and. fe2)) then
   !      call writelog('lswe','', &
   !         'If ''instat=reuse'' the model directory may not contain sufficient boundary definition files.')
   !      if (.not. fe1) then
   !         call writelog('lswe','','Model currently missing ebcflist.bcf')
   !      elseif (.not. fe2) then
   !         call writelog('lswe','','Model currently missing qbcflist.bcf')
   !      endif
   !      call xbeach_errorhandler()
   !   else
   !      call writelog('lswe','','If ''instat=reuse'' the model directory must contain boundary definition files.')
   !      call writelog('lswe','','Use ebcflist.bcf and qbcflist.bcf')
   !      call xbeach_errorhandler()
   !   endif
   !else
   !   filetype=-1
   !endif
   taper    = readkey_dbl (md_surfbeatfile,'taper',   100.d0,      0.0d0, 1000.d0)
   nwavmax  = readkey_dbl (md_surfbeatfile,'nmax',    0.8d0,       0.5d0, 1.d0)
   dir0     = readkey_dbl (md_surfbeatfile,'dir0',    270.d0,    0d0,   360.d0)
   if (trim(instat) == 'stat') then
      Hrms  = readkey_dbl (md_surfbeatfile,'Hrms',      1.d0,      0.d0,    10.d0)
      Tm01  = readkey_dbl (md_surfbeatfile,'Tm01',     10.d0,      1.d0,    20.d0)
      Trep  = readkey_dbl (md_surfbeatfile,'Trep',     Tm01,   1.d0,    20.d0)
      m     = readkey_int (md_surfbeatfile,'m',        10,         2,      128)
   elseif (trim(instat) == 'bichrom') then
      Hrms  = readkey_dbl (md_surfbeatfile,'Hrms',      1.d0,      0.d0,    10.d0)
      Tm01  = readkey_dbl (md_surfbeatfile,'Tm01',     10.d0,      1.d0,    20.d0)
      Trep  = readkey_dbl (md_surfbeatfile,'Trep',     Tm01,   1.d0,    20.d0)
      Tlong = readkey_dbl (md_surfbeatfile,'Tlong',    80.d0,     20.d0,   300.d0)
      m     = readkey_int (md_surfbeatfile,'m',        10,         2,      128)
   elseif (trim(instat) == 'ts_1' .or. trim(instat) == 'ts_2') then
      Hrms  = readkey_dbl (md_surfbeatfile,'Hrms',      1.d0,      0.d0,    10.d0)
      Tm01  = readkey_dbl (md_surfbeatfile,'Tm01',     10.d0,      1.d0,    20.d0)
      Trep  = readkey_dbl (md_surfbeatfile,'Trep',     Tm01,   1.d0,    20.d0)
      m     = readkey_int (md_surfbeatfile,'m',        10,         2,      128)
      call check_file_exist('bc/gen.ezs')
   endif
   !
   !
   ! Wave-spectrum boundary condition parameters
   if (    trim(instat) == 'jons'          .or.    &
      trim(instat) == 'swan'          .or.    &
      trim(instat) == 'vardens'       .or.    &
      trim(instat) == 'jons_table'                ) then

   call writelog('l','','--------------------------------')
   call writelog('l','','Wave-spectrum boundary condition parameters: ')

   random          = readkey_int (md_surfbeatfile,'random',       1,          0,          1       , strict=.true.)
   fcutoff         = readkey_dbl (md_surfbeatfile,'fcutoff',      0.d0,       0.d0,       40.d0   )
   nspr            = readkey_int (md_surfbeatfile,'nspr',         0,          0,          1       )
   trepfac         = readkey_dbl (md_surfbeatfile,'trepfac',      0.01d0,     0.d0,       1.d0    )
   sprdthr         = readkey_dbl (md_surfbeatfile,'sprdthr',      0.08d0,     0.d0,       1.d0    )
   correctHm0      = readkey_int (md_surfbeatfile,'correctHm0',   1,          0,          1       )
   Tm01switch      = readkey_int (md_surfbeatfile,'Tm01switch',   0,          0,          1       )
   swkhmin         = readkey_dbl (md_surfbeatfile,'swkhmin',      -0.01d0,   -0.01d0,     0.35d0  )

   nspectrumloc    = readkey_int (md_surfbeatfile,'nspectrumloc',   1,          1,       10000 )

   endif
   !
   if (  trim(instat)=='jons' .or. &
      trim(instat)=='swan' .or. &
      trim(instat)=='vardens'.or. &
      trim(instat)=='stat_table' .or. &
      trim(instat)=='jons_table' &
      )then
   filetype = 0   ! JRE to check
   bcfile = readkey_name(md_surfbeatfile,'bcfile')
   call check_file_exist(bcfile)
   call checkbcfilelength(tstop_user-tstart_user,instat,bcfile, nspectrumloc, filetype)

   elseif (trim(instat)=='reuse') then
      ! TO DO: check file length is done after recomputation of tstop due to morfacopt
      ! at the end of this subroutine.
      ! JRE: TO DO: implement reuse bc
      inquire(file='ebcflist.bcf',exist=fe1)
      inquire(file='qbcflist.bcf',exist=fe2)

      if (.not. (fe1 .and. fe2)) then
         call writelog('lswe','', &
            'If ''instat=reuse'' the model directory may not contain sufficient boundary definition files.')
         if (.not. fe1) then
            call writelog('lswe','','Model currently missing ebcflist.bcf')
         elseif (.not. fe2) then
            call writelog('lswe','','Model currently missing qbcflist.bcf')
         endif
         call xbeach_errorhandler()
      else
         call writelog('lswe','','If ''instat=reuse'' the model directory must contain boundary definition files.')
         call writelog('lswe','','Use ebcflist.bcf and qbcflist.bcf')
         call xbeach_errorhandler()
      endif
   else
      filetype=-1
   endif
   !
   if (filetype==0) then
      rt          = readkey_dbl(md_surfbeatfile,'rt',   min(3600.d0,tstop_user),    1200.d0,    7200.d0 ) !! to do
      dtbc        = readkey_dbl(md_surfbeatfile,'dtbc',          1.0d0,      0.1d0,      2.0d0   )
   endif

   if (trim(instat)=='swan') then
      dthetaS_XB  = readkey_dbl (md_surfbeatfile,'dthetaS_XB',   0.0d0,      -360.d0,    360.0d0 )
   endif
   !
   !
   ! Flow boundary condition parameters
   ! front
   call writelog('l','','--------------------------------')
   call writelog('l','','Flow boundary condition parameters: ')
   ARC         = readkey_int (md_surfbeatfile,'ARC',      1,              0,       1       )
   order       = readkey_dbl (md_surfbeatfile,'order',    2.d0,           1.d0,    2.d0    )
   freewave    = readkey_int (md_surfbeatfile,'freewave', 0,    0,       1       )
   epsi        = readkey_dbl (md_surfbeatfile,'epsi',     -1.d0,          -1.d0,   0.2d0   )
   hminlw      = readkey_dbl (md_surfbeatfile,'hmin',    0.2d0,     0.001d0,      1.d0)
   allocate(allowednames(2),oldnames(0))
   allowednames=(/'abs_1d','abs_2d'/)
   absgentype  = readkey_str(md_surfbeatfile,'absgentype','abs_1d',2,0,allowednames,oldnames)
   if (allocated(allowednames)) deallocate(allowednames, oldnames)
   allocate(allowednames(2),oldnames(0))
   allowednames=(/'instant ','velocity'/)
   tidetype= readkey_str(md_surfbeatfile,'tidetype','velocity',2,0,allowednames,oldnames)
   deallocate(allowednames,oldnames)
   !
   ! Wave field initialization parameters
   Trepini       = readkey_dbl (md_surfbeatfile,'Trepini',    1.d-5,         1.d-5,    1.d3   )
   Eini          = readkey_dbl (md_surfbeatfile,'Eini',      1.0d-5,         1.d-5,    1.d10  )
   
   ! Wave breaking parameters

   if (swave==1) then
      call writelog('l','','--------------------------------')
      call writelog('l','','Wave dissipation parameters: ')
      allocate(allowednames(5),oldnames(5))
      allowednames  =(/'roelvink1    ','baldock      ','roelvink2    ','roelvink_daly','janssen      '/)
      oldnames      =(/'1','2','3','4','5'/)
      if (trim(instat) == 'stat' .or. trim(instat) == 'stat_table') then
         break      = readkey_str (md_surfbeatfile,'break','baldock',5,5,allowednames,oldnames)
         gamma      = readkey_dbl (md_surfbeatfile,'gamma',   0.78d0,     0.4d0,     0.9d0)
      else
         break      = readkey_str (md_surfbeatfile,'break','roelvink2',5,5,allowednames,oldnames)
         gamma      = readkey_dbl (md_surfbeatfile,'gamma',   0.55d0,     0.4d0,     0.9d0)
      endif
      deallocate(allowednames,oldnames)
      if (trim(break)=='roelvink_daly') then
         gamma2     = readkey_dbl (md_surfbeatfile,'gamma2',   0.3d0,     0.0d0,     0.5d0)
      endif
      alpha         = readkey_dbl (md_surfbeatfile,'alpha',   1.0d0,     0.5d0,     2.0d0)
      nroelvink     = readkey_dbl (md_surfbeatfile,'n',       10.0d0,     5.0d0,    20.0d0)
      gammaxxb      = readkey_dbl (md_surfbeatfile,'gammax',   2.d0,      .4d0,      5.d0)
      deltaH        = readkey_dbl (md_surfbeatfile,'delta',   0.0d0,     0.0d0,     1.0d0)
      wavefricfile  = readkey_name(md_surfbeatfile,'fwfile')
      wavefricval   = readkey_dbl (md_surfbeatfile,'fw',       0.d0,   0d0,      1.0d0)
      fwcutoff      = readkey_dbl (md_surfbeatfile,'fwcutoff',  1000.d0,   0d0,      1000.d0)
      !breakerdelay  = readkey_int (md_surfbeatfile,'breakerdelay',    1,   0,      1)
      !
      !
      ! Roller parameters
      call writelog('l','','--------------------------------')
      call writelog('l','','Roller parameters: ')
      roller           = readkey_int (md_surfbeatfile,'roller',     1,        0,     1, strict=.true.)
      beta             = readkey_dbl (md_surfbeatfile,'beta',    0.10d0,     0.05d0,   0.3d0)
      rfb              = readkey_int (md_surfbeatfile,'rfb',        0,        0,     1, strict=.true.)
      !
      !
      ! Wave-current interaction parameters
      call writelog('l','','--------------------------------')
      call writelog('l','','Wave-current interaction parameters: ')
      wci      = readkey_int (md_surfbeatfile,'wci',        0,        0,     1, strict=.true.)
      hwci     = readkey_dbl (md_surfbeatfile,'hwci',   0.1d0,   0.001d0,      1.d0)
      hwcimax  = readkey_dbl (md_surfbeatfile,'hwcimax',   100.d0,   0.01d0,      100.d0)
      cats     = readkey_dbl (md_surfbeatfile,'cats',   4.d0,     1.d0,      50.d0)
   endif
   !
   !
   ! Wave numerics parameters
   call writelog('l','','--------------------------------')
   call writelog('l','','Wave numerics parameters: ')
   if (trim(instat) == 'stat' .or. trim(instat) == 'stat_table') then
       wavint     = readkey_dbl (md_surfbeatfile,'wavint',    600.d0,      1.d0,  3600.d0)
       maxerror   = readkey_dbl (md_surfbeatfile,'maxerror', 0.001d0, 0.00001d0, 1d0)
       maxiter    = readkey_int (md_surfbeatfile,'maxiter',    500,         2,      1000)
       dtmaximp   = readkey_dbl (md_surfbeatfile,'dtmax',    1000d0,         1d0,      2500d0)
       d_relaxfac = readkey_dbl (md_surfbeatfile,'relaxfac',    1d0,         0d0,      1d0)
   endif
   waveps     = readkey_dbl(md_surfbeatfile,'waveps',     0.005d0,   0.001d0,      0.1d0)
   !
   !
   ! Windmodel paramaters
   if (windmodel .eq. 1) then
      call writelog('l','','--------------------------------')
      call writelog('l','','Wind source parameters: ')
      mwind       = readkey_dbl (md_surfbeatfile,'mwind',   1.d0,    0.5d0,   1.d0)
      jawsource   = readkey_int (md_surfbeatfile,'windsource',   0,    0,   1, required=(swave==1 .and. jawind==1), strict=.true.)
      jagradcg    = readkey_int (md_surfbeatfile,'jagradcg',   1,    0,   1, required=((swave==1 .and. jawind==1) .and. jawsource==1), strict=.true.)
      advecmod    = readkey_int (md_surfbeatfile,'advecmod',    1,         1,      2)
      ndissip     = readkey_dbl (md_surfbeatfile,'ndissip',  3.d0,         1.d0,      10.d0)      
      coefdispT   = readkey_dbl (md_surfbeatfile,'coefdispT',  3.5d0,         0.d0,      1000.d0)  
      coefdispk   = readkey_dbl (md_surfbeatfile,'coefdispk',  1.d0,         0.d0,      1000.d0)  
   endif
   !
   !
   ! Roller turbulence parameters
   call writelog('l','','--------------------------------')
   call writelog('l','','Roller turbulence parameters: ')

   BRfac    = readkey_dbl (md_surfbeatfile,'BRfac',    1.0d0,       0.d0, 1.d0)
   call setallowednames('none',              TURB_NONE,           &
                        'wave_averaged',     TURB_WAVE_AVERAGED,  &
                        'bore_averaged',     TURB_BORE_AVERAGED)
   call setoldnames('0','1','2')
   call parmapply('turb',3, turb)

   Tbfac    = readkey_dbl (md_surfbeatfile,'Tbfac  ',1.0d0,     0.00d0,   1.0d0)
   !
   !
   ! Finish
   call writelog('l','','--------------------------------')
   call writelog('sl','','Finished reading input parameters')
   call writelog('l','','--------------------------------')
   !
   !
   ! -------------------   Post-input processing -------------------------
   !
   !
   ! Set taper to non-zero
   taper    = max(taper,1.d-6)
   !
   !
   ! Only allow Baldock in stationary mode and Roelvink in non-stationary
   if (trim(instat) == 'stat' .or. trim(instat) == 'stat_table') then
      if (trim(break) .ne. 'baldock' .and. trim(break) .ne. 'janssen') then
         if(trim(break)=='roelvink_daly') then
            call writelog('lwse','','Error: Roelvink-Daly formulations not implemented in stationary wave mode,')
            call writelog('lwse','','         use Baldock or Janssen formulation.')
            call xbeach_errorhandler()
         else
            call writelog('lwse','','Error: Roelvink formulations not implemented in stationary wave mode,')
            call writelog('lwse','','         use Baldock or Janssen formulation.')
            call xbeach_errorhandler()
         endif
      endif
   else
      if (trim(break)=='baldock') then
         call writelog('lwse','','Error: Baldock formulation not allowed in non-stationary mode, use a Roelvink')
         call writelog('lwse','','       formulation.')
         call xbeach_errorhandler()      
      endif
      if (trim(break)=='janssen') then
         call writelog('lwse','','Error: Janssen formulation not allowed in non-stationary mode, use a Roelvink')
         call writelog('lwse','','       formulation.')
         call xbeach_errorhandler()   
      endif
   endif
   !
   if (trim(instat)=='stat' .or. trim(instat)=='stat_table') then
      thetanaut = 1
      thetamin = dir0-90d0
      thetamax = dir0+90d0
      call writelog('lws','','Warning: Calculating with stationary bc. Thetamin and thetamax reset to -/+90 degrees from dir0.')
   endif   
   !facmax = 0.25d0*sqrt(ag)*rhomean*gamma**2
   !
   !
   ! Wave-current interaction with non-stationary waves still experimental
   !if ((trim(instat)/='stat' .and. trim(instat)/='stat_table') .and. wci.ne.0) then
   !   call writelog('lws','','Warning: Wave-current interaction with non-stationary waves is still')
   !   call writelog('lws','','         experimental, continue with computation nevertheless')
   !endif
   if (wci .ne. 0) then
      call writelog('lws','','Warning: Wave-current interaction is not operational yet. Switched off.')
      wci = 0
   end if
   !
   ! Check for unknown parameters
   call readkey(md_surfbeatfile,'checkparams',dummystring)

   !   check swave and Lwave
   if ( swave.eq.0 ) then
      lwave = 0
      call writelog('lws','','Warning: swave is 0, so lwave set to 0.')
   endif

end subroutine xbeach_all_input


subroutine xbeach_wave_init()
   use m_flowgeom
   use m_flowexternalforcings
   use m_xbeach_data
   use m_sferic, only: pi, twopi
   use m_physcoef
   use network_data

   implicit none

   integer, allocatable, dimension(:)     :: idum, kcstore
   integer, allocatable, dimension(:,:)   :: kp

   integer                            :: itheta, i, k, L, ierror
   integer, parameter                 :: np=12

   if ( trim(instat)=='jons' .or. &
      trim(instat)=='jons_table' .or. &
      trim(instat)=='swan' .or. &
      trim(instat)=='vardens' .or. &
      trim(instat)=='reuse' &
      ) Trep=10.d0

   if ( trim(instat)=='jons' .or. &
      trim(instat)=='jons_table' .or. &
      trim(instat)=='swan' .or. &
      trim(instat)=='vardens') then
      call xbeach_spectral_wave_init()
   endif

   if ( ntheta.gt.0 ) then
      ! dispersion
      if (windmodel .eq. 1) then   
         tt1 = Trepini
         sigt = twopi / tt1  
         ee1 = Eini 
      else          
         do itheta=1,ntheta
            sigt(itheta,:) = twopi/Trep
         end do
      endif 
      
      if (windmodel.eq.0) then
      do k = 1, ndx    ! stack overflow
          sigmwav(k) = sum(sigt(:,k), dim=1)/dble(ntheta)
          L0(k) = 2*pi*ag/(sigmwav(k)**2)
          L1(k) = L0(k)
          Ltemp(k) = L0(k)
      end do
      else
          L0t = 2*pi*ag/(sigt**2)
          L1t = L0t
          Ltempt = L0t      
      endif
      
      ! initialize celerities
      if (windmodel .eq. 1) then
          call xbeach_dispersion_windmodel()
      else 
          call xbeach_dispersion()    
      endif

   end if

   ! map boundary types to each other's positions
   if ( allocated(kbndu2kbndw) ) deallocate(kbndu2kbndw)
   allocate(kbndu2kbndw(nbndu))

   if ( allocated(kbndw2kbndu) ) deallocate(kbndw2kbndu)
   allocate(kbndw2kbndu(nbndw))

   if ( allocated(kbndz2kbndw) ) deallocate(kbndz2kbndw)
   allocate(kbndz2kbndw(nbndz))

   allocate(idum(Lnx))

   idum = 0

   !< Map velocity to wave
   do i=1,nbndw
      L = kbndw(3,i)
      idum(L) = i
   end do

   do i=1,nbndu
      L = kbndu(3,i)
      kbndu2kbndw(i) = idum(L)
   end do

   !< map wl to wave
   do i=1,nbndz
      L = kbndz(3,i)
      kbndz2kbndw(i) = idum(L)
   end do

   idum = 0

   !< Map wave to velocity
   do i=1,nbndu
      L = kbndu(3,i)
      idum(L) = i
   end do

   do i=1,nbndw
      L = kbndw(3,i)
      kbndw2kbndu(i) = idum(L)
   end do
   
   if (.not. allocated(uave)) then
      maxnumbnds=100
      allocate(uave(maxnumbnds),vave(maxnumbnds),dlengthrm(maxnumbnds), stat=ierror)
      allocate(umeanrm(maxnumbnds), vmeanrm(maxnumbnds), stat = ierror)
      uave = 0d0
      vave = 0d0
      dlengthrm = 0d0
      umeanrm = 0d0
      vmeanrm = 0d0
   end if
   
   if ( windmodel.eq.1) then
      if (jawsource.eq.1) then
   !define source term coefficients
      CE1 = 8d0/(aa1*aa1*bb1 ) * (16d0/(aa1*aa1 ) )**(1d0/(2d0* bb1) -1d0 )
         CE2 = 1d0/(2d0* bb1) -1d0
      CT1 = 1d0/(aa2*bb2 ) * (1d0/(aa2 ) )**(1d0/bb2 -1d0 )
         CT2 = 1d0/bb2 -1d0
   endif
   !map wind field to cell centers
          call xbeach_map_wind_field(wx, wy, mwind, wmagcc, windspreadfac)
   endif
   
   if (trim(instat)=='stat' .or. trim(instat)=='stat_table') then
      !
      if (allocated(kp)) deallocate(kp,stat=ierror)
      allocate(kp(numk,np), stat=ierror)
      kp=0
      !
      if (.not. allocated(nb)) then
         if (allocated(kcstore)) deallocate(kcstore)
         allocate(kcstore(numk))
         kcstore=kc
         kc = 1
         call MAKENETNODESCODING()
         kc=kcstore
      endif   
      !
      ! set inner mask
      !where (nb==1)
         inner = .true.
      !endwhere 
      !
      call getbndwzcornerpts(ierror)        ! sets wmask, nmmask, seapoints
      ! get nodes per netcell
      call fill_connected_nodes(ierror)
      ! Find calculation kernel around points
      call fm_surrounding_points(xk,yk,numk,connected_nodes,no_connected_nodes,nump,kp, ierror)
      ! Find upwind neighbours for each grid point and wave direction
      call find_upwind_neighbours(xk,yk,numk,thetabin,ntheta,kp,np,w,prev,ds,ierror)
      ! set e01
      call xbeach_wave_bc()     
      ! compute advection and refraction velocities in net nodes
      call xbeach_wave_compute_statcelerities(ierror)      
   end if

   if ( allocated(idum) ) deallocate(idum)
   if ( allocated(kp) ) deallocate(kp)

   return
   end subroutine xbeach_wave_init

   !> make the thetagrid, in init_flowgeom
   subroutine xbeach_makethetagrid()
   use m_flowgeom 
   use m_xbeach_data
   use m_sferic
   use m_alloc
   implicit none

   integer                                     :: itheta, ierr, k
   double precision                            :: thetaminloc
   
   if (swave==1) then
      theta0=(1.5d0*pi)-dir0*atan(1.d0)/45d0
      do while(theta0<-2d0*pi)
         theta0=theta0+2.d0*pi
      enddo
      do while(theta0>2d0*pi)
         theta0=theta0-2.d0*pi
      enddo
      
      if (thetanaut==1) then
         thetaminloc = thetamin
         thetamin=(270.d0-thetamax)*dg2rd
         thetamax=(270.d0-thetaminloc)*dg2rd
      else
         thetamin=thetamin*dg2rd
         thetamax=thetamax*dg2rd
      endif
      
      thetamin = mod(thetamin,2.d0*pi)
      thetamax = mod(thetamax,2.d0*pi)
      
      if(thetamin>=thetamax) then
         if (thetamax>=0.d0) then
            do while(thetamin>=thetamax)
               thetamin = thetamin-2.d0*pi
            enddo
         else
            do while(thetamin>thetamax)
               thetamax = thetamax+2.d0*pi
            enddo
         endif
      elseif(thetamax>thetamin+2.d0*pi) then
         do while(thetamax>thetamin+2.d0*pi) 
            thetamin = thetamin+2.d0*pi
         enddo
      endif
      
      dtheta=dtheta*dg2rd
      if ( dtheta.gt.0d0 ) then
         ntheta = max(nint((thetamax-thetamin)/dtheta),1)
      else
         ntheta = 1
         dtheta = thetamax-thetamin
      end if
   else
      dtheta=2d0*pi
      ntheta = 1
   endif
   
   call realloc(csx, ntheta, stat=ierr, keepExisting = .false., fill = 0d0)
   call aerr('csx  (ntheta)', ierr, ntheta)
   call realloc(snx, ntheta, stat=ierr, keepExisting = .false., fill = 0d0)
   call aerr('snx  (ntheta)', ierr, ntheta)
   call realloc(thet, (/ntheta,ndx/), stat=ierr, keepExisting = .false., fill = 0d0)
   call aerr('thet  (ntheta,ndx)', ierr, ntheta*ndx)
   call realloc(costh, (/ntheta,ndx/), stat=ierr, keepExisting = .false., fill = 0d0)
   call aerr('costh  (ntheta,ndx)', ierr, ntheta*ndx)
   call realloc(sinth, (/ntheta,ndx/), stat=ierr, keepExisting = .false., fill = 0d0)
   call aerr('sinth  (ntheta,ndx)', ierr, ntheta*ndx)
   call realloc(thetabin, ntheta, stat=ierr, keepExisting = .false., fill = 0d0)
   call aerr('thetabin  (ntheta)', ierr, ntheta)
   
   do itheta=1,ntheta
      thetabin(itheta)=thetamin+dtheta/2d0+dtheta*(itheta-1)
   end do

   do itheta=1,ntheta
      csx(itheta)     = cos(thetabin(itheta))
      snx(itheta)     = sin(thetabin(itheta))
      do k = 1, ndx
         thet(itheta,k)  = thetabin(itheta)          ! JRE to do: necessary?
         costh(itheta,k) = cos(thetabin(itheta))
         sinth(itheta,k) = sin(thetabin(itheta))
      enddo
   enddo

   end subroutine xbeach_makethetagrid

   subroutine xbeach_dispersion()
   use m_xbeach_filefunctions
   use m_flowgeom
   use m_flow, only: s1, hu
   use m_flowparameters, only: epshu, epshs
   use m_sferic, only: pi
   use m_xbeach_data, only: hdisp, deltaH, H, waveps, sigmwav, L0, L1, Ltemp, cwav, nwav, cgwav, kwav
   use m_physcoef, only: ag
   use m_flowtimes, only: time0
   use m_flowexternalforcings

   implicit none

   integer                                          :: i,j,j1,j2,k,L,k1,k2
   double precision                                 :: kh, hh
   double precision, external                       :: iteratedispersion
   
   do k=1,ndx
      hh = max(s1(k)-bl(k),epshs)
      if (hh > epshs) then
         hdisp(k) = max(hh + deltaH*H(k), waveps)
         L0(k) = 2*pi*ag/(sigmwav(k)**2)
      else
         hdisp(k) = waveps
         L0(k)    = waveps
      end if
   end do
   L1=L0
   
   do k=1,ndxi
      if(hdisp(k).ge.waveps) then
         if (2*pi/L0(k)*hdisp(k) > 5d0) then
            Ltemp(k) = L0(k)
         else
            !Ltemp(k) = (2d0*pi*ag/(sigmwav(k)**2))*(1-exp(-(sigmwav(k)*sqrt(hdisp(k)/ag))**(5d0/2d0)))**(2d0/5d0)
            Ltemp(k) = iteratedispersion(L0(k),Ltemp(k),pi,hdisp(k))
            if (Ltemp(k)<0.d0) then   ! this is an error from iteratedispersion
               Ltemp(k) = -Ltemp(k)
               call writelog('lws','','Warning: no convergence in dispersion relation iteration at t = ', &
                  time0)
            endif
         endif
         L1(k)=Ltemp(k)
      endif
   end do
   
   do L=1,nbndz
      k1=kbndz(1,L); k2=kbndz(2,L)
      L1(k1) = L1(k2)
   end do
   
   do L=1,nbndu
      k1=kbndu(1,L); k2=kbndu(2,L)
      L1(k1) = L1(k2)
   end do
   
   do k=1,ndx
      kwav(k)  = 2*pi/max(L1(k),waveps)
      cwav(k)  = sigmwav(k)/kwav(k)
      kh   = min(kwav(k)*hdisp(k),10.0d0)
      nwav(k)=0.5d0+kh/max(sinh(2d0*kh),waveps)
      cgwav(k)=cwav(k)*nwav(k)
   end do
   
   where ((s1-bl)<epshs)
      kwav=0d0
   end where
   
   end subroutine xbeach_dispersion

    
   function iteratedispersion(L0,Lestimate,px,h) result(L)

   implicit none
   ! input
   double precision,intent(in)    :: L0
   double precision,intent(in)    :: Lestimate
   double precision,intent(in)    :: px
   double precision,intent(in)    :: h
   ! output
   double precision               :: L
   ! internal
   double precision               :: L1,L2,hs1,hs2
   integer                        :: iter
   double precision               :: err
   double precision,parameter     :: aphi = 1.d0/(((1.0d0 + sqrt(5.0d0))/2)+1)
   double precision,parameter     :: bphi = ((1.0d0 + sqrt(5.0d0))/2)/(((1.0d0 + sqrt(5.0d0))/2)+1)
   integer,parameter              :: itermax = 150
   double precision,parameter     :: errmax = 0.00001d0


   err = huge(0.0d0)
   iter = 0
   L1 = Lestimate
   do while (err > errmax .and. iter < itermax)
      iter  = iter+1
      L2    = L0*tanh(2*px*h/L1)
      L1    = (L1*aphi + L2*bphi)          ! Golden ratio
      err   = abs(L2 - L1)
   end do

   if (iter<=itermax) then
      L = L1
   else
      ! signal this went wrong
      L = -L1
   endif

   end function iteratedispersion



   subroutine dhsdxdhsdy(dhsdx, dhsdy)
   use m_flow
   use m_flowgeom
   use m_netw
   use m_alloc
   use m_sferic, only: jsferic
   use geometry_module, only: getdx, getdy

   implicit none

   integer                                        :: L, k1, k2, k3, k4, kkk, nwalls, kb, ki, ierr
   double precision                               :: dxx, dyy, wuL, cs, sn, hs1, hs2
   double precision, intent(out), dimension(ndx)  :: dhsdx, dhsdy
   !double precision, allocatable                  :: dbdx(:), dbdy(:), dsdx(:), dsdy(:)

   !allocate(dbdx(1:ndx), dbdy(1:ndx), dsdx(1:ndx), dsdy(1:ndx), stat = ierr)
   !dbdx = 0d0
   !dbdy = 0d0
   !dsdx = 0d0
   !dsdy = 0d0
   !
   !do L = 1,lnx
   !   k1 = ln(1,L)
   !   k2 = ln(2,L)
   !   k3 = lncn(1,L)
   !   k4 = lncn(2,L)
   !
   !   dxx = getdx(xk(k3),yk(k3),xk(k4),yk(k4),jsferic)
   !   dyy = getdy(xk(k3),yk(k3),xk(k4),yk(k4),jsferic)
   !
   !   dbdx(k1) = dbdx(k1)+.5d0*(zk(k3)+zk(k4))*dyy  
   !   dbdy(k1) = dbdy(k1)-.5d0*(zk(k3)+zk(k4))*dxx
   !   dbdx(k2) = dbdx(k2)-.5d0*(zk(k3)+zk(k4))*dyy
   !   dbdy(k2) = dbdy(k2)+.5d0*(zk(k3)+zk(k4))*dxx
   !end do
   !
   !nwalls = 0
   !do nwalls=1,mxwalls
   !   k1 = walls(1,nwalls)
   !   
   !   if (k1==7420) then
   !      continue
   !   end if
   !   
   !   k2 = walls(2,nwalls)
   !   k3 = walls(3,nwalls)
   !
   !   cs = walls(7,nwalls)
   !   sn = walls(8,nwalls)
   !   wuL = walls(9,nwalls)
   !
   !   dbdx(k1) = dbdx(k1)+0.5*(zk(k3)+zk(k2))*wuL*sn
   !   dbdy(k1) = dbdy(k1)-0.5*(zk(k3)+zk(k2))*wuL*cs
   !end do
   !
   !nwalls = 0
   !do nwalls=1, nthd
   !   k1 = thindam(1,nwalls)
   !   
   !   if (k1==7488) then
   !      continue
   !   end if
   !   
   !   k2 = thindam(2,nwalls)
   !   k3 = thindam(3,nwalls)
   !
   !   cs  = thindam(4,nwalls)
   !   sn  = thindam(5,nwalls)
   !   wuL = thindam(6,nwalls)
   !
   !   dbdx(k1) = dbdx(k1)+0.5*(zk(k3)+zk(k2))*wuL*sn
   !   dbdy(k1) = dbdy(k1)-0.5*(zk(k3)+zk(k2))*wuL*cs
   !      
   !end do
   !
   !do kkk=1,ndxi
   !   dbdx(kkk) =dbdx(kkk)*bai(kkk)
   !   dbdy(kkk) =dbdy(kkk)*bai(kkk)
   !end do

   ! Tegeltjesdiepte approach is eenvoudiger en onnauwkeuriger, maar werkt altijd, ook met morfologie
   dhsdx = 0d0
   dhsdy = 0d0
   do L = 1,Lnx
      if (hu(L) > epshu) then                            ! link flows
         k1 = ln(1,L)
         k2 = ln(2,L)
         hs1 = s1(k1)-bl(k1)
         hs2 = s1(k2)-bl(k2)

         dhsdx(k1) = dhsdx(k1) + wcx1(L)*(hs2 - hs1) * dxi(L) ! dimension m/m
         dhsdy(k1) = dhsdy(k1) + wcy1(L)*(hs2 - hs1) * dxi(L)
         dhsdx(k2) = dhsdx(k2) + wcx2(L)*(hs2 - hs1) * dxi(L)
         dhsdy(k2) = dhsdy(k2) + wcy2(L)*(hs2 - hs1) * dxi(L)
      endif
   enddo

   do kkk  = 1,nbndu
      kb = kbndu(1,kkk)
      ki = kbndu(2,kkk)
      dhsdx(kb) = dhsdx(ki)
      dhsdy(kb) = dhsdy(ki)
   enddo
   
   do kkk  = 1,nbndz
      kb = kbndz(1,kkk)
      ki = kbndz(2,kkk)
      dhsdx(kb) = dhsdx(ki)
      dhsdy(kb) = dhsdy(ki)
   enddo

   !deallocate(dbdx, dbdy, dsdx, dsdy, stat = ierr)
end subroutine dhsdxdhsdy

subroutine xbeach_instationary()
   use m_sferic, only:pi,rd2dg, twopi
   use m_physcoef, only: rhog, ag
   use m_flowgeom
   use m_flow, only: s1, epshu, vol1, rhomean, epshs, plotlin
   use m_flowparameters, only:limtypw
   use m_flowexternalforcings, only: nbndw, zbndw
   use m_xbeach_data
   use m_xbeach_paramsconst
   use m_partitioninfo
   use m_timer
   use m_alloc
   use m_waves, only: hwav, twav, phiwav, ustokes, vstokes, rlabda, uorb, jauorb
   use m_flowtimes, only:dts

   implicit none

   integer                        :: k, itheta, ierr, L, k1, k2, kb, ki, nwalls
   double precision, allocatable  :: hh(:), ddlok(:,:), dd(:,:), wete(:,:), drr(:,:)
   double precision, allocatable  :: uwf(:), vwf(:), ustr(:), urf(:), vrf(:), ustw(:), dfac(:)
   double precision, allocatable  :: Tdeplim(:)

   double precision               :: fsqrtt, ee_eps, tt_eps

   allocate(hh(1:ndx), ddlok(1:ntheta, 1:ndx), dd(1:ntheta, 1:ndx), wete(1:ntheta, 1:ndx), drr(1:ntheta,1:ndx), stat = ierr)
   allocate(ustw(1:ndx), uwf(1:ndx), vwf(1:ndx), ustr(1:ndx), stat = ierr)
   allocate(urf(1:ndx), vrf(1:ndx), dfac(1:ndx), stat = ierr)
   allocate(Tdeplim(1:ndx), stat = ierr)
   
   xb_started = 1
   ee_eps = 0.00001d0 !Eini!1d0
   tt_eps = waveps    !important to limit wave celerities to 1 in case of cells for which hs<epshs

   hh   = 0.d0
   ddlok = 0.d0
   wete = 0.d0
   drr = 0.d0
   ustw = 0d0
   uwf = 0d0
   vwf = 0d0
   ustr = 0d0
   urf = 0d0
   vrf = 0d0
   horadvec=0d0
   horadvec2=0d0
   thetaadvec=0d0
   thetaadvec2=0d0                          
   
   call xbeach_wave_compute_celerities()      

   hh = max(s1-bl, epshs)

   do k=1,ndx   ! stack
      thetamean(k) = sum(ee1(:,k)*thet(:,k),dim=1)/max(sum(ee1(:,k),dim=1),0.00001d0) ! energy weighted wave direction
      sigmwav(k) = max((sum(sigt(:,k),1)/dble(ntheta)),0.01d0)
   end do
   
   !  construct and solve system
   
   if (windmodel.eq.1) then
      if (advecmod.eq.1) then
          !define
           ma=ee1/sigt
           mb=ee1 
      elseif (advecmod.eq.2) then
          !define moments
          ma=ee1
          mb=sigt*ee1  
      endif
      
      call advec_horz_windmodel(dtmaxwav, snx, csx, limtypw, ma, cgwavt, horadvec)
      call advec_horz_windmodel(dtmaxwav, snx, csx, limtypw, mb, cgwavt, horadvec2)     
      call advec_dir(ma, ctheta, thetaadvec)
      call advec_dir(mb, ctheta, thetaadvec2) 

      do k = 1,ndxi
         do itheta = 1,ntheta
            if ( vol1(k) > epshs*ba(k) ) then
                  ma(itheta,k) = ma(itheta,k) - dtmaxwav*(horadvec(itheta,k)  * bai(k) + thetaadvec(itheta,k))       
                  mb(itheta,k) = mb(itheta,k) - dtmaxwav*(horadvec2(itheta,k)  * bai(k) + thetaadvec2(itheta,k)) 
               else
                  ma(itheta,k)=ee_eps*tt_eps/twopi
                  mb(itheta,k)=ee_eps
               endif   
            enddo
      enddo
      
      if (advecmod.eq.1) then               
         sigt=min(mb/ma,twopi/tt_eps)
         ee1=max(mb,ee_eps)     
         tt1=twopi/sigt
     else  
        sigt=min(mb/ma,twopi/tt_eps)
        ee1=max(ma,ee_eps)
        tt1=twopi/sigt         
      endif
      
      call xbeach_wave_compute_celerities()

   else !regular xbeach approach, fixed period
       
      ee1 = ee1/sigt      
      call advec_horz(dts, snx, csx, limtypw, ee1, cgwav, horadvec)
      call advec_dir(ee1, ctheta, thetaadvec)

      do k = 1,ndxi
         do itheta = 1,ntheta
            if ( vol1(k) > epshs*ba(k) ) then
               ee1(itheta,k) = ee1(itheta,k) - dts*(horadvec(itheta,k)*bai(k) + thetaadvec(itheta,k))
            else
               ee1(itheta,k) = 0d0
            endif
         enddo
      enddo
   
      ee1 = ee1*sigt                   ! Back to wave energy
      ee1=max(ee1,0.0d0)

   endif 
   !
   !   Energy integrated over wave directions,Hrms, depth limitation on energy
   !
   do k=1,ndx
       E(k)=sum(ee1(:,k),dim=1)*dtheta
       H(k)=sqrt(8.d0*E(k)/rhomean/ag)

       do itheta=1,ntheta
           ee1(itheta,k)=ee1(itheta,k)/max(1.d0,(H(k)/(gammaxxb*hh(k)))**2)
       enddo

       H(k)=min(H(k),gammaxxb*hh(k))
       E(k)=rhomean*ag*(H(k)**2)/8.d0

   end do
   
   !   Breaker dissipation
   call xbeach_wave_breaker_dissipation(dts, break, DeltaH, waveps, kwav, km, gamma, gamma2, nroelvink, QB, alpha, Trep, cwav, thetamean, E, D, sigmwav, wci, windmodel)

   !   Dissipation by bed friction
   dfac = 2.d0*fw*rhomean/(3.d0*pi)
   do k=1,Ndx
      urms_cc(k) = H(k) * sigmwav(k) / 2d0 / sinh(min(max(kwav(k),0.01d0)*max(hh(k),deltaH*H(k)),10.0d0))   ! uorb uit XBeach
      Df(k)=dfac(k)*urms_cc(k)**3
   end do
   
   if (jauorb==0) then       ! old d3d convention
      urms_cc = urms_cc*sqrt(pi)/2d0    ! only on hrms derived value, not on SWAN read uorb
   end if
   
   where (hh>fwcutoff)
      Df = 0.d0
   end where
   !
   !   Distribution of total dissipation over directions
   !
   do itheta=1,ntheta
      ddlok(itheta,:)=ee1(itheta,:)*D/max(E,0.00001d0)                     ! breaking
      dd(itheta,:)   = ddlok(itheta,:) + ee1(itheta,:)*Df/max(E,0.00001d0)   ! breaking plus friction
   enddo

   if (windmodel.eq.1) then
     ! wave period depth limitation 
      call xbeach_wave_compute_period_depth_limitation( 1.d0/8.d0*rhog*(gammaxxb*hh**2) , Tdeplim)  
      do itheta=1,ntheta
         tt1(itheta,:) = min(tt1(itheta,:) , Tdeplim )
      enddo
      sigt=twopi/tt1
      sigmwav=sum(sigt,dim=1)/ntheta  
      
      ! wave period breaker dissipation 
      call xbeach_wave_period_breaker_dissipation( D, E, sigmwav, cgwav, kwav, ddT)        
      
      !  Wind source term
      if (jawsource.eq.1) then
         call xbeach_windsource(ee1, E, tt1, sigmwav , cgwavt, cgwav, hh, dtmaxwav, wsorE, wsorT,egradcg,SwE,SwT)    
      else
          wsorE=0.d0
          wsorT=0.d0
          SwE=0.d0
          SwT=0.d0
      endif
   endif ! windmodel
   
   
   do itheta = 1, ntheta
      where (hh+deltaH*H>epshs)
         wete(itheta,:)=1d0
      elsewhere
         wete(itheta,:)=0d0
      end where
   enddo

   !!  Roller energy balance
   call advec_horz(dts, snx, csx, limtypw, rr, cwav, rrhoradvec)
   call advec_dir(rr,ctheta,rrthetaadvec)

   do k = 1,ndxi
      do itheta = 1,ntheta
         if ( vol1(k) > epshs*ba(k) ) then
            rr(itheta,k) = rr(itheta,k) - dts*(rrhoradvec(itheta,k)  * bai(k) + rrthetaadvec(itheta,k))
         else
            rr(itheta,k) = 0d0
         endif
      enddo
   enddo

   rr=max(rr,0.0d0)
   
   !  euler step roller energy dissipation
   if (windmodel.eq.1) then
       do k = 1,ndx  
         do itheta=1,ntheta
            if (wete(itheta, k)==1) then 
               ee1(itheta,k) = ee1(itheta, k) + min( dtmaxwav * (wsorE(itheta, k) -  dd(itheta, k) )  , ee1(itheta, k) ) 
               tt1(itheta,k) = tt1(itheta, k) + min( dtmaxwav * (wsorT(itheta, k) -  ddT(k) ) , tt1(itheta, k) )
               ! 
               if(roller==1) then
                     drr(itheta, k) = 2*ag*BR(k)*max(rr(itheta, k),0.0d0)/ cwav(k)
                     rr(itheta, k)=rr(itheta, k)+dtmaxwav*(ddlok(itheta, k) -drr(itheta, k))
               else if (roller==0) then
                  rr(itheta, k)  = 0.0d0
                  drr(itheta, k) = 0.0d0
               endif
               !
               ee1(itheta, k)    = max(ee1(itheta, k),ee_eps)
               tt1(itheta, k)    = max(tt1(itheta, k), tt_eps)
               rr(itheta, k)     = max(rr(itheta, k),0.0d0)
            elseif(wete(itheta, k)==0) then
               ee1(itheta, k)    = ee_eps
               tt1(itheta, k)    = tt_eps
               rr(itheta, k)     = 0.0d0
            end if
         end do
       end do
       
       tt1=max(tt1,tt_eps)
       ee1=max(ee1,ee_eps)
       sigt=twopi/tt1
       
   else !if windmodel=0
       
      do k = 1,ndx  ! ndx
         do itheta=1,ntheta
            if(wete(itheta, k)==1) then
               ee1(itheta,k)=ee1(itheta, k)-dts*dd(itheta, k)                ! totale dissipatie
               if(roller==1) then
                  drr(itheta, k) = 2.0*ag*BR(k)*max(rr(itheta, k),0.0d0)/    &
                                   cwav(k)
                  rr(itheta, k)=rr(itheta, k)+dts*(ddlok(itheta, k)   &      ! only wave breaker dissipation
                                -drr(itheta, k))
               else if (roller==0) then
                  rr(itheta, k)  = 0.0d0
                  drr(itheta, k) = 0.0d0
               endif
               ee1(itheta, k)    = max(ee1(itheta, k),0.0d0)
               rr(itheta, k)     = max(rr(itheta, k),0.0d0)
            elseif(wete(itheta, k)==0) then
               ee1(itheta, k)    = 0.0d0
               rr(itheta, k)     = 0.0d0
            end if ! wete
         end do
      end do
   endif !windmodel

   if ( jampi.eq.1 ) then
      write(6,*) 'my_rank=', my_rank
      if ( jatimer.eq.1 ) call starttimer(IXBEACH)
      call update_ghosts(ITYPE_Sall, Ntheta, Ndx, ee1, ierr)
      call update_ghosts(ITYPE_Sall, Ntheta, Ndx, rr,  ierr)
      if ( jatimer.eq.1 ) call stoptimer(IXBEACH)
   end if

   ! Orbital velocity
   fsqrtt = sqrt(0.5d0) ! 1 / sqrt(2.0)
   do L=1,Lnx
      k1 = ln(1,L)
      k2 = ln(2,L)
      urms(L) = (acL(L)*urms_cc(k1) + (1d0-acl(L))*urms_cc(k2)) * fsqrtt
   end do

   call xbeach_apply_wave_bc()

   !   OUTPUT Bulk quantities
   do k=1,ndx   ! stack
       E(k)  = sum(ee1(:,k),dim=1)*dtheta
       R(k)  = sum(rr(:,k),dim=1)*dtheta
       if (roller==1) then
           DR(k) = sum(drr(:,k),dim=1)*dtheta
       else
           DR(k) = D(k)
           R(k) = DR(k)*cwav(k)/2d0/ag/BR(k)
       end if
       H(k)  = sqrt(8.d0*E(k)/rhomean/ag)
       thetamean(k)=(sum(ee1(:,k)*thet(:,k),dim=1)/dble(ntheta))/(max(sum(ee1(:,k),dim=1),0.00001d0)/dble(ntheta))
   end do
   Dtot = D+Df

   ! Stokes drift
   ustw= E/max(cwav,sqrt(hminlw*ag))/rhomean/max(hh,hminlw) !waves
   ustr=2d0*R/max(cwav,sqrt(hminlw*ag))/rhomean/max(hh,hminlw) !roller
   uwf = ustw*dcos(thetamean)                    !! Cartesian decomposition
   vwf = ustw*dsin(thetamean)
   urf = ustr*dcos(thetamean)
   vrf = ustr*dsin(thetamean)

   do L=1,lnx                                    !! facenormal decomposition
      k1 = ln(1,L); k2 = ln(2,L)
      ust(L) = acL(L)*(csu(L)*(uwf(k1)+urf(k1))+snu(L)*(vwf(k1)+vrf(k1))) + &
         (1d0-acL(L))*(csu(L)*(uwf(k2)+urf(k2))+snu(L)*(vwf(k2)+vrf(k2)))


      vst(L) = acL(L)*(-snu(L)*(uwf(k1)+urf(k1))+csu(L)*(vwf(k1)+vrf(k1))) + &
         (1d0-acL(L))*(-snu(L)*(uwf(k2)+urf(k2))+csu(L)*(vwf(k2)+vrf(k2)))
   enddo
   
   if (roller.eq.1 .and. turb.ne.TURB_NONE) then
      call borecharacter()                   ! calculates BR and Tbore using Rieneck&Fenton approach   
   end if
   
   ! En voor de uniformiteit van de golfkoppelingetjes:
   hwav = H
   twav = 2.0*pi/sigmwav
   phiwav = thetamean*rd2dg
   rlabda = L1
   uorb = urms_cc
   ustokes = ust              ! will be overwritten for 3D in getustwav
   vstokes = vst   

   deallocate(hh, ddlok, wete, drr, stat = ierr)
   deallocate(ustw, ustr, uwf, vwf, urf, vrf, stat = ierr)
   deallocate(Tdeplim, stat=ierr)
   end subroutine xbeach_instationary


subroutine xbeach_wave_compute_flowforcing2D()
   use m_flowgeom
   use m_flow
   use m_xbeach_data
   use m_flowgeom
   use m_partitioninfo
   use m_timer

   implicit none

   integer                              :: k, n, L, k1, k2
   double precision                     :: cs, sn, wul
   iNtEGeR                              :: ierror
   double precision, allocatable, save  :: dsxxdx(:),dsyydy(:),dsxydx(:),dsxydy(:)

   if (.not. allocated(dsxxdx)) then
      allocate(dsxxdx(1:ndx),dsyydy(1:ndx),dsxydx(1:ndx),dsxydy(1:ndx))
   endif 

   !   Radiation stresses
   nwav = cgwav/max(cwav,1d-10)
   do k=1, ndx   ! stack
      Sxx(k)=(nwav(k)*sum((1.d0+costh(:,k)**2)*ee1(:,k),dim=1)-.5d0*sum(ee1(:,k),dim=1))*dtheta     ! wave energy contribution
      Syy(k)=(nwav(k)*sum((1.d0+sinth(:,k)**2)*ee1(:,k),dim=1)-.5d0*sum(ee1(:,k),dim=1))*dtheta
      Sxy(k)= nwav(k)*sum(sinth(:,k)*costh(:,k)*ee1(:,k),dim=1)*dtheta
      
      Sxx(k) = Sxx(k) + sum((costh(:,k)**2)*rr(:,k),dim=1)*dtheta                    ! Roller contribution
      Syy(k) = Syy(k) + sum((sinth(:,k)**2)*rr(:,k),dim=1)*dtheta
      Sxy(k) = Sxy(k) + sum( sinth(:,k)*costh(:,k)*rr(:,k),dim=1)*dtheta
   end do

   !   Wave forces Fx, Fy, value on links
   Fx_cc = 0d0
   Fy_cc = 0d0
   dsxxdx = 0d0
   dsyydy = 0d0
   dsxydy = 0d0
   dsxydx = 0d0
   ! Jipjanneke
   do L=1,lnx
      k1 = ln(1,L)
      k2 = ln(2,L)
      dsxxdx(k1) = dsxxdx(k1) + wcx1(L)*(Sxx(k2)-Sxx(k1))*dxi(L)
      dsxxdx(k2) = dsxxdx(k2) + wcx2(L)*(Sxx(k2)-Sxx(k1))*dxi(L)
   
      dsyydy(k1) = dsyydy(k1) + wcy1(L)*(Syy(k2)-Syy(k1))*dxi(L)
      dsyydy(k2) = dsyydy(k2) + wcy2(L)*(Syy(k2)-Syy(k1))*dxi(L)
   
      dsxydx(k1) = dsxydx(k1) + wcx1(L)*(Sxy(k2)-Sxy(k1))*dxi(L)
      dsxydx(k2) = dsxydx(k2) + wcx2(L)*(Sxy(k2)-Sxy(k1))*dxi(L)
   
      dsxydy(k1) = dsxydy(k1) + wcy1(L)*(Sxy(k2)-Sxy(k1))*dxi(L)
      dsxydy(k2) = dsxydy(k2) + wcy2(L)*(Sxy(k2)-Sxy(k1))*dxi(L)
   enddo
   
   do k = 1,ndx
      Fx_cc(k) = -dsxxdx(k)-dsxydy(k)
      Fy_cc(k) = -dsxydx(k)-dsyydy(k) 
   enddo

   ! Open boundaries: Neumann
   do n = 1,nbndw                     ! not necessary, no wave bnd without open flow bnd
      k1 = kbndw(1,n); k2=kbndw(2,n)
      Fx_cc(k1) = Fx_cc(k2)
      Fy_cc(k1) = Fy_cc(k2)
   end do

   do n = 1, nbndz
      k1 = kbndz(1,n); k2=kbndz(2,n)
      Fx_cc(k1) = Fx_cc(k2)
      Fy_cc(k1) = Fy_cc(k2)
   end do

   do n = 1, nbndu
      k1 = kbndu(1,n); k2=kbndu(2,n)
      Fx_cc(k1) = Fx_cc(k2)
      Fy_cc(k1) = Fy_cc(k2)
   end do

   if (jampi.eq.1) then
      if ( jatimer.eq.1 ) call starttimer(IXBEACH)
      call update_ghosts(ITYPE_SALL, 1, ndx, Fx_cc, ierror)
      call update_ghosts(ITYPE_SALL, 1, ndx, Fy_cc, ierror)
      if ( jatimer.eq.1 ) call stoptimer(IXBEACH)
   endif

   do L = 1,Lnx
      k1 = ln(1,L); k2 = ln(2,L)
      Fx(L)   = ( acL(L)*Fx_cc(k1) + (1d0-acL(L))*Fx_cc(k2) )
      Fy(L)   = ( acL(L)*Fy_cc(k1) + (1d0-acL(L))*Fy_cc(k2) )
      wavfu(L) =  Fx(L)*csu(L)+Fy(L)*snu(L)
      wavfv(L) = -Fx(L)*snu(L)+Fy(L)*csu(L)
   enddo

   where (hu<=epshu)
      Fx    = 0d0
      Fy    = 0d0
      wavfu = 0d0
      wavfv = 0d0
   endwhere

   end subroutine xbeach_wave_compute_flowforcing2D

   subroutine xbeach_wave_maxtimestep()
   use m_flowtimes
   use m_flow
   use m_flowgeom
   use m_xbeach_data
   use m_partitioninfo

   implicit none

   integer           :: k, k1, k2, kk, L, itheta
   double precision  :: dum, cgwavL, cwuL, dt

   dtmaxwav = huge(0d0)

   ! Calculate max CFL based timestep for wave calculation
   do k = 1, ndx
      do itheta = 1, ntheta
         dum = 0.d0
         do kk = 1, nd(k)%lnx
            L = iabs(nd(k)%ln(kk))
            k1 = ln(1,L)
            k2 = ln(2,L)
            
            if (windmodel .eq. 0) then
               cgwavL = acL(L)*cgwav(k1) + (1-acL(L))*cgwav(k2)
               cwuL    = cgwavL*( csu(L)*csx(itheta) + snu(L)*snx(itheta) )
            else
               cgwavL = acL(L)*cgwavt(itheta,k1) + (1-acL(L))*cgwavt(itheta,k2)
               cwuL    = cgwavL*( csu(L)*csx(itheta) + snu(L)*snx(itheta) ) 
            endif
            

            if (ln(2,L) .eq. k) cwuL = -cwuL

            if (cwuL .ge. 0.) then        ! outgoing velocities only
               dum = dum + cwuL*wu(L)
            end if
         end do
         if (dum > tiny(0d0)) then
            dt = cflmx*ba(k) / dum
            if ( dt.lt.dtmaxwav ) then
               dtmaxwav = dt
            end if
         end if
         dum = ctheta(itheta, k)/dtheta
         if (dum > tiny(0d0)) then
            dt = cflmx / dum
            if ( dt.lt.dtmaxwav ) then
               dtmaxwav = dt
            end if
         end if
      end do
   end do
   !
   if ( jampi.eq.1 ) then
      call reduce_double_min(dtmaxwav)
   end if
   !
   if (dtmaxwav > dts) dtmaxwav = dts
   dtmaxwav = dts/ceiling(dts/dtmaxwav)
   ! 
   if (trim(instat) .ne. 'stat' .and. trim(instat) .ne. 'stat_table') then
      dts = dtmaxwav
      dti = 1d0/dts
   end if
   ! 
   end subroutine xbeach_wave_maxtimestep

   subroutine xbeach_wave_compute_celerities
   use m_flow
   use m_flowgeom
   use m_flowparameters, only: epshu
   use m_flowtimes
   use m_sferic, only: pi
   use m_xbeach_data

   implicit none

   integer                              :: k, k1, k2, kb, ki, itheta, iw, L, ierr
   double precision                     :: factime, ducxdn, ducydn
   double precision, allocatable,save   :: hh(:), sinh2kh(:)
   double precision, allocatable,save   :: wcifacucx(:), wcifacucy(:), vel(:)
   double precision, allocatable,save   :: dkmydx(:), dkmxdy(:)
   double precision, allocatable,save   :: arg(:), cgxm(:), cgym(:), fac(:), kmxwav(:), kmywav(:), wm(:), wmadvec(:)

   if (.not. allocated(hh)) then
      allocate(hh(1:ndx), sinh2kh(1:ndx), stat = ierr)
      allocate(wcifacucx(1:ndx), wcifacucy(1:ndx),  stat = ierr)
   end if

   if (wci.ne.0 .and. .not. allocated(vel)) then
      allocate(vel(1:ndx), stat = ierr)
      allocate(dkmydx(1:ndx), dkmxdy(1:ndx), arg(1:ndx), cgxm(1:ndx),cgym(1:ndx),stat = ierr)
      allocate(fac(1:ndx), kmxwav(1:ndx), kmywav(1:ndx),wm(1:ndx), wmadvec(1:ndx), stat = ierr)
   end if
   !
   hh        = 0d0
   !wcifacucx = 0d0
   !wcifacucy = 0d0
   xbducxdx    = 0d0
   xbducxdy    = 0d0
   xbducydx    = 0d0
   xbducydy    = 0d0
   
   hh = max(s1-bl,waveps)
   do k=1, ndx ! stack
      thetamean(k)=(sum(ee1(:,k)*thet(:,k),dim=1))/(max(sum(ee1(:,k),1),0.00001d0))
   end do
   
   if (windmodel .eq. 0) then
      do itheta=1,ntheta
         sigt(itheta,:) = 2*pi/Trep
      end do
   endif

   !if (wci .ne. 0) then
   !   wci = 1                ! safety in case user uses not 1 to activate this, beware: integer!
   !end if

   wcifacucx=0d0    !ucx*dble(wci)*min(min(hs/hwci,1.d0),min(1.d0,1.d0-hs/hwcimax))
   wcifacucy=0d0    !ucy*dble(wci)*min(min(hs/hwci,1.d0),min(1.d0,1.d0-hs/hwcimax))

   !if (wci .ne. 0) then    ! wave current interaction
   !
   !   if (time1==0d0) then
   !      sigmwav = max(sum(sigt,dim = 1)/dble(ntheta), epshs)
   !      call xbeach_dispersion()
   !      umwci = 0.d0
   !      vmwci = 0.d0
   !      zswci = s1
   !      km=kwav
   !   endif
   !   !
   !   do iw=1,nbndw
   !      kb = kbndw(1,iw)
   !      km(kb) = kwav(kb)                      ! boundary condition (assuming no current at the boundary)
   !   end do
   !   !
   !if (windmodel .eq.0) then
   !   factime = 1.d0/cats/Trep*dts              ! smoothing over no of wave periods, dts iso dtmaxwav, which is zero at t0
   !else
   !   factime = 1.d0/cats*maxval(sigmwav)/2.d0/pi*dts   
   !endif
   
   !   umwci   = factime*ucx + (1-factime)*umwci
   !   vmwci   = factime*ucy + (1-factime)*vmwci
   !   zswci   = factime*s1  + (1-factime)*zswci
   !   arg     = min(100.0d0,km*max(hh,deltaH*H))
   !   !
   !   do iw=1,nbndw
   !      kb = kbndw(1,iw)
   !      sigmwav(kb) = sqrt( ag*km(kb)*tanh(arg(kb)) )
   !   end do
   !   !
   !   kmxwav = km*dcos(thetamean)
   !   kmywav = km*dsin(thetamean)
   !   wm = sigmwav + kmxwav*umwci*dble(wci)*min(&                                           ! only in depths between hwci and hwcimax
   !   min((zswci-bl)/hwci,1.d0), &
   !      min(1.d0,(1.d0-(zswci-bl)/hwcimax)) &
   !      )+ &
   !      kmywav*vmwci*dble(wci)*min(&
   !      min((zswci-bl)/hwci,1.d0), &
   !      min(1.d0,(1.d0-(zswci-bl)/hwcimax)) &
   !      )
   !   cgym = cgwav*dsin(thetamean) + vmwci*min(min((zswci-bl)/hwci,1.d0),min(1.d0,(1.d0-(zswci-bl)/hwcimax)))
   !   cgxm = cgwav*dcos(thetamean) + umwci*min(min((zswci-bl)/hwci,1.d0),min(1.d0,(1.d0-(zswci-bl)/hwcimax)))
   !   !
   !   ! Calculate wave number gradients in cell centres
   !   !
   !   dkmydx = 0d0
   !   dkmxdy = 0d0
   !   do L = 1,Lnx
   !      if (hu(L) > epshu) then
   !         k1 = ln(1,L)
   !         k2 = ln(2,L)
   !         dkmydx(k1) = dkmydx(k1) + wcx1(L)*(kmywav(k2) - kmywav(k1)) * dxi(L)
   !         dkmxdy(k1) = dkmxdy(k1) + wcy1(L)*(kmxwav(k2) - kmxwav(k1)) * dxi(L)
   !         dkmydx(k2) = dkmydx(k2) + wcx2(L)*(kmywav(k2) - kmywav(k1)) * dxi(L)
   !         dkmxdy(k2) = dkmxdy(k2) + wcy2(L)*(kmxwav(k2) - kmxwav(k1)) * dxi(L)
   !      endif
   !   enddo
   !   !
   !   ! solve eikonal equation
   !   vel = 1d0
   !   call advec_upw_bulk(thetamean, wm, vel, wmadvec)
   !   kmxwav = kmxwav - dtmaxwav*wmadvec*bai - dtmaxwav*cgym*(dkmydx-dkmxdy)    ! to check: vector components on the radian frequency
   !   kmywav = kmywav - dtmaxwav*wmadvec*bai - dtmaxwav*cgxm*(dkmydx-dkmxdy)
   !
   !   do L = lnxi+1, lnx
   !      kb      = ln(1,L); ki      = ln(2,L)
   !      kmxwav(kb) = kmxwav(ki); kmywav(kb) = kmywav(ki)
   !   end do
   !
   !   ! update km
   !   km = sqrt(kmxwav**2+kmywav**2)
   !   ! non-linear dispersion
   !   arg = min(100.0d0,km*((zswci-bl)+deltaH*H))
   !   arg = max(arg,0.0001)
   !   !       fac = ( 1.d0 + ((km*H/2.d0)**2)*( (8.d0+(cosh(min(4.d0*arg,10.0d0)))**1.d0-2.d0*(tanh(arg))**2.d0 ) /(8.d0*(sinh(arg))**4.d0) ) )
   !   fac = ( 1.d0 + ((km*H/2.d0)**2))  ! use deep water correction instead of expression above (waves are short near blocking point anyway)
   !   !       fac = 1.d0    ! Linear
   !   sigmwav = sqrt( ag*km*tanh(arg)*fac )
   !   !
   !   !  update intrinsic frequency
   !   do itheta=1,ntheta
   !      sigt(itheta,:) = sigmwav
   !   enddo
   !   where(km>0.01d0)
   !      cwav  = sigmwav/km
   !      !          cg = c*(0.5d0+arg/sinh(2.0d0*arg))    ! Linear
   !      cgwav = cwav*(0.5d0+arg/sinh(2*arg))*sqrt(fac)  ! &  to include more
   !      !                 + km*(H/2)**2*sqrt(max(par%g*km*tanh(arg),0.001d0))/sqrt(max(fac,0.001d0)) ! include wave steepness
   !      nwav=0.5d0+km*hh/sinh(2*max(km,0.00001d0)*hh)
   !   elsewhere
   !      cwav  = 0.01d0
   !      cgwav = 0.01d0
   !      nwav  = 1.d0
   !   endwhere
   !   !  update k
   !   km = min(km,25.d0) ! limit to gravity waves
   !   kwav = km
   !
   !else                     ! no wave current interaction
      
      
      if (windmodel .eq. 1) then
          call xbeach_dispersion_windmodel()
      else
         do k = 1, ndx   ! stack
            sigmwav(k) = max(sum(sigt(:,k),dim = 1)/dble(ntheta), 0.01d0)
         end do
    
         call xbeach_dispersion()    
      endif
      
   !end if

   ! slopes of the water depth
   call dhsdxdhsdy(dhsdx, dhsdy)

   ! slope of velocities
   ! JRE TO DO: what with 3D situation? See ucxucyucxuucyu, quantities delivered to swan
   !
   !if (wci .ne. 0) then
   !   do L = 1,lnx
   !      k1 = ln(1,L)
   !      k2 = ln(2,L)
   !      ducxdn = dxi(L)*( ucx(k2) - ucx(k1) )
   !      ducydn = dxi(L)*( ucy(k2) - ucy(k1) )
   !      xbducxdx(k1) = xbducxdx(k1) + wcx1(L)*ducxdn
   !      xbducxdy(k1) = xbducxdy(k1) + wcy1(L)*ducxdn
   !      xbducxdx(k2) = xbducxdx(k2) + wcx2(L)*ducxdn
   !      xbducxdy(k2) = xbducxdy(k2) + wcy2(L)*ducxdn
   !
   !      xbducydx(k1) = xbducydx(k1) + wcx1(L)*ducydn
   !      xbducydy(k1) = xbducydy(k1) + wcy1(L)*ducydn
   !      xbducydx(k2) = xbducydx(k2) + wcx2(L)*ducydn
   !      xbducydy(k2) = xbducydy(k2) + wcy2(L)*ducydn
   !   enddo
   !endif

   where(2d0*hh*kwav<=3000.d0)
      sinh2kh=sinh(min(2d0*kwav*hh,10.0d0))
   elsewhere
      sinh2kh = 3000.d0
   endwhere

   do k=1, ndx
      cgwav(k) = sqrt( (cgwav(k)*dcos(thetamean(k))+wcifacucx(k))**2 + (cgwav(k)*dsin(thetamean(k))+wcifacucy(k))**2 )
      cwav(k)  = sqrt( (cwav(k)*dcos(thetamean(k)) +wcifacucx(k))**2 + (cwav(k)*dsin(thetamean(k)) +wcifacucy(k))**2 )
   end do

   if (ntheta > 1) then
      ! compute refraction velocity
      if (windmodel.eq.1) then
         do k = 1, ndx
            do itheta=1,ntheta
               ctheta(itheta, k) =                                           &
                  sigt(itheta,k)/max(sinh(min(2d0*kwavt(itheta,k)*hh(k),10.0d0)),1d-10)*(dhsdx(k)*snx(itheta)-dhsdy(k)*csx(itheta))! + &
            enddo
         enddo
         ctheta=sign(1.d0,ctheta)*min(abs(ctheta), .25d0 * sigt)
      else
         do k = 1, ndx
            do itheta=1,ntheta
               ctheta(itheta, k) =                                           &
                  sigmwav(k)/max(sinh2kh(k),1d-10)*(dhsdx(k)*snx(itheta)-dhsdy(k)*csx(itheta))! + &
                  !dble(wci)*( csx(itheta) * (snx(itheta)*xbducxdx(k)-csx(itheta)*xbducxdy(k))          + &
                  !snx(itheta) * (snx(itheta)*xbducydx(k)-csx(itheta)*xbducydy(k)) )
                  !ctheta=sign(1.d0,ctheta)*min(abs(ctheta),.5*pi/Trep)
             enddo
         enddo
         ctheta=sign(1.d0,ctheta)*min(abs(ctheta),.5*pi/Trep)
      endif

      do itheta=1, ntheta
         where (s1-bl<waveps)
            ctheta(itheta,:) = 0d0
         end where
      end do
   end if

   deallocate(hh, sinh2kh, stat=ierr)

   end subroutine xbeach_wave_compute_celerities


   !> compute wave boundary conditions
   subroutine xbeach_wave_bc()
   use m_flowgeom
   use m_xbeach_data
   use m_flowexternalforcings
   use wave_boundary_main_module
   use m_flowtimes, only: time0, time1, tstop_user
   use m_physcoef, only: rhomean, ag
   use m_sferic, only: pi
   use m_flowparameters, only: epshs, epshu
   use m_flow, only:hs, u1, v, plotlin, s1
   use m_alloc
   use m_xbeach_filefunctions
   use wave_boundary_datastore
   use interp
   use m_partitioninfo

   implicit none

   integer, save                                         :: nt
   integer, save                                         :: old
   integer, save                                         :: curline
   integer                                               :: i, itheta, j, E_idx, ier, ier2, ierror, clock,idum(nwbnd)
   integer, save                                         :: bctype
   double precision                                      :: E1,ei,dum,Hm0, dum1, spreadpar, bcdur, dum2, dthetarad, cgwavin
   double precision, save                                :: bcendtime,bcstarttime
   double precision                                      :: em,tshifted,tnew,fac,hboundary(nwbnd)
   double precision, save                                :: Emean,Llong
   double precision                                      :: hh, ht
   character(len=1)                                      :: bline
   character(slen)                                       :: ebcfname,qbcfname,fname
   logical                                               :: startbcf

   double precision, allocatable, save                   :: dist(:), factor(:)

   double precision                                      :: E0
   double precision                                      :: bl1, bl2
   double precision, dimension(nbndw)                    :: qxbc,qybc
   double precision, dimension(nbndw, ntheta)            :: eeout

   double precision                                      :: Hbc,Tbc,Dbc

   logical                                               :: isRecomputed

   integer                                               :: k, kb, ki, Lb, LL, Lw, L, nw, k2
   integer                                               :: LL1, LL2, n, lunfil

   !logical, save                                         :: bccreated=.false.  ! now in module

   ierror = 1
   if (.not. allocated(dist)) allocate(dist(1:ntheta),factor(1:ntheta), e01(1:ntheta))
   !
   eeout = 0d0
   uin   = 0d0
   vin   = 0d0
   qxbc  = 0d0
   qybc  = 0d0
   !
   !  note: also in xbeach_spectral_wave_init
   call get_hboundary(hboundary)
   !
   startbcf=.false.
   !
   if(  .not. (trim(instat).eq.'stat' .or. &
               trim(instat).eq.'bichrom' .or. &
               trim(instat).eq.'ts_1' .or. &
               trim(instat).eq.'ts_2' .or. &
               trim(instat).eq.'stat_table' &
      ))then

      select case (trim(instat))
         case('jons')
            bctype = 4
         case('jons_table')
            bctype = 11
      end select
            
      ! Regenerate random seed
      if (random==1) then
         do i=1,nwbnd
            call system_clock(count=clock)
            randomseed(i)=clock
         end do
         !
         if (jampi==1) then
            idum = randomseed
            call reduce_int_max(nwbnd, idum)
            randomseed=idum
         end if
      else
         randomseed=-999
      end if
      !
      do n = 1,nwbnd
         LL1 = L1wbnd(n)
         LL2 = L2wbnd(n)
         if (jampi==1) then
            !k = ln(2,LL1)
            !if (.not.(idomain(k)==my_rank) .or. LL2==0) cycle     ! then not a boundary domain, second check is safety
            if (LL2==0) cycle
         endif
         waveBoundaryParameters(n)%hboundary=hboundary(n)
         waveBoundaryParameters(n)%randomseed=randomseed(n)
         call create_incident_waves_surfbeat(LL2-LL1+1, n, xbndw(LL1:LL2),ybndw(LL1:LL2),&
                                             waveBoundaryParameters(n)%ntheta,waveBoundaryParameters(n)%dtheta,waveBoundaryParameters(n)%theta,time0, &
                                             bctype,bcfile, &
                                             waveBoundaryParameters(n)%x0,waveBoundaryParameters(n)%y0,waveBoundaryParameters(n)%hboundary, &
                                             waveBoundaryParameters(n)%randomseed, &
                                             eeout(LL1:LL2,:),qxbc(LL1:LL2),qybc(LL1:LL2), &
                                             Hbc,Tbc,Dbc,isRecomputed,nspr=nspr,sprdthr=sprdthr, &
                                             trepfac=trepfac,nmax=nwavmax,fcutoff=fcutoff,rho=rhomean, &
                                             Tm01switch=Tm01switch,swkhmin=swkhmin)         
      end do
      Trep      = Tbc
      !
      do i=1,nbndu       !! for absgen bnd's
         nw = kbndu2kbndw(i)
         if ( nw.gt.0 ) then
            hh = max(s1(kbndw(1,nw))-bl(kbndw(1,nw)),epshs)
            uin(nw) = qxbc(nw)/hh             ! x and y, are oriented later according to link direction in absgen_bc
            vin(nw) = qybc(nw)/hh
         end if
      end do
      !
      do i=1,nbndw
         zbndw(1:ntheta,i) = eeout(i,1:ntheta)
      end do
           
   else !! instat = stat, stat_table, ts_1, ts_2, bichrom
      if(.not. bccreated ) then        
         call writelog('ls','','Setting up boundary conditions')
         bccreated=.true.
         startbcf=.true.                     ! trigger read from bcf for instat 3,4,5,7
         bcendtime=huge(0.0d0)               ! initial assumption for instat 3,4,5,7
         newstatbc=1

         call get_refpoint(xref0, yref0)

         if (trim(instat)=='ts_1') then
            open( newunit=lunfil, file='bc/gen.ezs')
5           continue
            read(lunfil,'(a)',iostat=ier) bline
            if (ier .ne. 0) then
               call report_file_read_error('bc/gen.ezs')
            endif
            if(bline.eq.'*') goto 5
            read(lunfil,*,iostat=ier) nt    ! no of timesteps
            if (ier .ne. 0) then
               call report_file_read_error('bc/gen.ezs')
            endif

            allocate(dataE  (nt))
            allocate(tE     (nt))
            do i=1,nt
               read(lunfil,*,iostat=ier) tE(i),dum,dataE(i)
               if (ier .ne. 0) then
                  call report_file_read_error('bc/gen.ezs')
               endif
            end do
            close(lunfil)
            Emean=sum(dataE)/nt

         elseif (trim(instat)=='ts_2') then
            open( newunit=lunfil, file='bc/gen.ezs')
6           continue
            read(lunfil,'(a)',iostat=ier)bline
            if (ier .ne. 0) then
               call report_file_read_error('bc/gen.ezs')
            endif
            if(bline.eq.'*') goto 6
            read(lunfil,*,iostat=ier)nt
            if (ier .ne. 0) then
               call report_file_read_error('bc/gen.ezs')
            endif

            allocate(dataE  (nt))
            allocate(databi (nt))
            allocate(tE     (nt))
            do i=1,nt
               read(lunfil,*,iostat=ier) tE(i),databi(i),dataE(i)
               if (ier .ne. 0) then
                  call report_file_read_error('bc/gen.ezs')
               endif
            end do
            close(lunfil)
            Emean=sum(dataE)/nt
         elseif (trim(instat)=='stat_table') then
            open( newunit=lunfil, file=bcfile)
            read(lunfil,*,iostat=ier) Hm0, Trep, dir0, dum1, spreadpar, bcendtime, dum2
            if (ier .ne. 0) then
               call report_file_read_error(bcfile)
            endif
            Hrms = Hm0/sqrt(2.d0)
            m = 2.0d0*spreadpar
            theta0=(1.5d0*pi) - dir0*atan(1.d0)/45.d0
            if (theta0>pi) theta0=theta0-2d0*pi
            if (theta0<-pi) theta0=theta0+2d0*pi
            newstatbc=1

            do itheta=1,ntheta
               sigt(itheta,:) = 2.d0*pi/Trep
            end do
            sigmwav = max(sum(sigt,1)/dble(ntheta),waveps)
            call xbeach_dispersion()
         endif
         !
         ! Directional distribution
         !
         dist=(cos(thetabin-theta0))**m
         do i=1,ntheta
            if(cos(thetabin(i)-theta0)<0.d0) then
               dist(i)=0.0d0
            end if
         end do
         if (trim(instat)=='ts_1' .or. trim(instat)=='ts_2') then
            Hrms=sqrt(8d0*Emean/(rhomean*ag))
         endif
         E0=0.125d0*ag*rhomean*Hrms**2

         ! energy density distribution
         if (sum(dist)>0.d0) then
            factor = (dist/sum(dist))/dtheta
         else
            factor=0.d0
         endif
         e01    = factor*E0;                            ! 1:ntheta ding
         e01    = max(e01,0.0d0);

         if ( jampi.eq.0 ) then
            if ( nbndw.gt.0 ) then
               Llong=Tlong*maxval(cgwav(kbndw(1,1:nbndw)))                   !! cg at some boundary point, xbeach_dispersion(). This implies that this value is the same everywhere!!
            else
               Llong = -huge(0d0)                                            !! Llong only for bichrom waves 
            end if
         else
            if ( nbndw.gt.0 ) then                                           ! may give different results for parallel runs
               Llong=Tlong*maxval(cgwav(kbndw(1,1:nbndw)))
            else
               Llong = -huge(0d0)
            end if
            call reduce_double_max(Llong)
         end if

         call writelog('sl','','Boundary conditions complete, starting computation')
      end if


      if (time0 .ge. bcendtime) then  ! Recalculate bcf-file
         if (trim(instat)=='stat_table') then
            call writelog('ls','','Reading new wave conditions')
            read(lunfil,*,iostat=ier) Hm0, Trep, dir0, dum1, spreadpar, bcdur, dum2
            if (ier .ne. 0) then
               call report_file_read_error(bcfile)
            endif
            Hrms = Hm0/sqrt(2.d0)
            taper = 0.d0
            m = 2.0d0*spreadpar
            bcendtime=bcendtime+bcdur
            theta0=(1.5d0*pi)-dir0*atan(1.d0)/45.d0

            if (theta0>2d0*pi) theta0=theta0-2d0*pi
            if (theta0<-2d0*pi) theta0=theta0+2d0*pi
            newstatbc=1                    

            if (windmodel==0) then
               do itheta=1,ntheta
                  sigt(itheta,:) = 2d0*pi/Trep
               end do
            endif

            do k = 1,nbndw
                kb = kbndw(1,k)
                do itheta = 1,ntheta
                    sigt(itheta,kb) = 2.d0 * pi / Trep
                end do
            enddo
            
            sigmwav = max(sum(sigt,1)/dble(ntheta), epshs)
            call xbeach_dispersion()

            dist=(cos(thetabin-theta0))**m
            do i=1,ntheta
               if(abs(thetabin(i)-theta0)>pi/2.d0) then
                  dist(i)=0
               end if
            end do
            E0=0.125d0*ag*rhomean*Hrms**2

            ! energy density distribution

            if (sum(dist)>0.d0) then
               factor = (dist/sum(dist))/dtheta
            else
               factor=0.d0
            endif
            e01    = factor*E0;
            e01    = max(e01,0.0d0);
         elseif (trim(instat)=='reuse') then
            close(71)                          ! to do, newlun
            close(72)
            startbcf=.true.
            if (time0 <= (tstop_user-time0)) then
               curline = curline + 1
            end if
         end if
         !
      end if
      !
      !!> Calculate boundary wave energy bc
      if (trim(instat)=='stat' .or. trim(instat)=='stat_table') then
         if (newstatbc==1) then
            !do itheta = 1, ntheta
            !   ee1(itheta,:) = e01(itheta)
            !end do
            ee1=0d0
         end if
         do L = 1, nbndw
            kb = kbndw(1,L)
            if (windmodel.eq.1) then
               zbndw(:,L)=max(e01,Eini) 
            else
               if (taper>tiny(0d0)) then
                  zbndw(:,L)=e01*min(time0/taper,1.0d0)
               else
                  zbndw(:,L)=e01   
               endif
            endif  
            bi(L) = 0.0d0
         end do

         if (nbndu .gt. 0) then
            do i=1,nbndw
               if (kbndw2kbndu(i) .ne. 0) then
                  uin(kbndw2kbndu(i)) = 0d0
                  vin(kbndw2kbndu(i)) = 0d0
               end if
            end do
         end if

         ! to check: MPI compliancy - okay for xref0, yref0
      elseif (trim(instat)=='bichrom') then
         theta0=(1.5d0*pi)-dir0*atan(1.d0)/45.d0
         do L = 1, nbndw
            kb = kbndw(1,L)
            zbndw(:,L)=e01*0.5d0 * &
               (1.d0+cos(2*pi*(time0/Tlong-( sin(theta0)*(ybndw(L)-yref0) &
               +cos(theta0)*(xbndw(L) - xref0))/Llong))) * &
               min(time0/taper,1.d0)
            if (nbndu .gt. 0) then
               Lb = kbndw2kbndu(L)
               bl1 = bl(kb); bl2 = bl(ki); 
               ht = max(zbndu(Lb) - 0.5d0*(bl1+bl2), epshu)   ! mean depth, not instantaneous
               em = (sum(0.5d0*e01))*dtheta *min(time0/taper,1.d0)
               ei =  sum(zbndw(:,L), dim=1)*dtheta
               bi(L) = -(2d0*cgwav(kb)/cwav(kb)-0.5d0)*(em-ei)/(cgwav(kb)**2-ag*ht)/rhomean
               uin(kbndw2kbndu(L)) = cgwav(kb)*bi(L)/ht*cos(theta0)
               vin(kbndw2kbndu(L)) = cgwav(kb)*bi(L)/ht*sin(theta0)
            end if
         end do

      elseif (trim(instat)=='ts_1') then
         theta0=(1.5d0*pi)-dir0*atan(1.d0)/45.d0
         do L = 1, nbndw
            kb = kbndw(1,L)
            !
            if (abs(theta0)<1e-3) then
               call linear_interp(tE,dataE,nt,time0,E1,E_idx)
            else
               if (jampi .eq. 0) then
                  cgwavin = maxval(cgwav(kbndw(1,1:nbndw)))
               else
                  if ( nbndw.gt.0 ) then    ! to check for different results for parallel runs
                     cgwavin = maxval(cgwav(kbndw(1,1:nbndw)))
                  else
                     cgwavin = -huge(0d0)
                  end if
                  call reduce_double_max(cgwavin)
               end if
               !
               tshifted = max(time0-(ybndw(L)-ybndw(1))*sin(theta0)/cgwav(kbndw(1,1)) &
                  -(xbndw(L)-xbndw(1))*cos(theta0)/cgwavin,0.d0)
               call linear_interp(tE,dataE,nt,tshifted,E1,E_idx)
            endif

            if (windmodel .eq. 1) then
               zbndw(:,L)=max(e01*E1/max(Emean,0.000001d0)*min(time0/taper,1.d0),Eini)                
            else 
               zbndw(:,L)=e01*E1/max(Emean,0.000001d0)*min(time0/taper,1.d0)
            endif
            
            if (nbndu .gt. 0) then
               Lb = kbndw2kbndu(L)
               bl1 = bl(kb); bl2 = bl(ki); 
               ht = max(zbndu(Lb) - 0.5d0*(bl1+bl2), epshu)   ! mean depth, not instantaneous
               em = Emean*min(time0/taper,1.d0)
               ei = sum(zbndw(:,L), dim=1)*dtheta
               bi(L) = -(2.0*cgwav(kb)/cwav(kb)-0.5d0)*(em-ei)/(cgwav(kb)**2-ag*ht)/rhomean
               uin(kbndw2kbndu(L)) = cgwav(kb)*bi(L)/ht*cos(theta0)
               vin(kbndw2kbndu(L)) = cgwav(kb)*bi(L)/ht*sin(theta0)
            end if
         end do

      elseif (trim(instat)=='ts_2') then
         ! debug
         !open(newunit=rmfilno, file='uin.txt', access='append')
         !\debug
         theta0=(1.5d0*pi)-dir0*atan(1.d0)/45.d0
         do L = 1,nbndw
            kb = kbndw(1,L)
            ki = kbndw(2,L)

            if (abs(theta0)<1e-3) then                             ! perpendicularly incoming
               call linear_interp(tE,dataE,nt,time0,E1,E_idx)
               call linear_interp(tE,databi,nt,time0,bi(L),E_idx)
            else

               if (jampi .eq. 0) then
                  cgwavin = maxval(cgwav(kbndw(1,1:nbndw)))
               else
                  if ( nbndw.gt.0 ) then    ! to check for different results for parallel runs
                     cgwavin = maxval(cgwav(kbndw(1,1:nbndw)))
                  else
                     cgwavin = -huge(0d0)
                  end if
                  call reduce_double_max(cgwavin)
               end if

               tshifted = max(time0-(ybndw(L)-ybndw(1))*sin(theta0)/cgwav(kbndw(1,1)) &
                            -(xbndw(L)-xbndw(1))*cos(theta0)/cgwavin,0.d0)
               call linear_interp(tE,dataE,nt,tshifted,E1,E_idx)
               call linear_interp(tE,databi,nt,tshifted,bi(L),E_idx)
            endif

            zbndw(:,L)=e01*E1/max(Emean,0.000001d0)*min(time0/taper,1.d0)
            if (nbndu .gt. 0) then
               Lb = kbndw2kbndu(L)
               bl1 = bl(kb); bl2 = bl(ki); 
               ht = max(zbndu(Lb) - 0.5d0*(bl1+bl2), epshu)   ! mean depth, not instantaneous
               if (freewave == 1) then
                  uin(kbndw2kbndu(L)) = sqrt(ag/ht)*bi(L)
                  vin(kbndw2kbndu(L)) = 0d0                      ! for completeness
               else
                  uin(kbndw2kbndu(L)) = cgwav(kb)*bi(L)/ht*cos(theta0)*min(time0/taper,1.d0)
                  vin(kbndw2kbndu(L)) = cgwav(kb)*bi(L)/ht*sin(theta0)*min(time0/taper,1.d0)
               end if
               ! debug
               !write(rmfilno,"(E15.7,I3,2E15.7)") time1, kbndw2kbndu(L), uin(kbndw2kbndu(L)), vin(kbndw2kbndu(L))
               !\debug   
            end if

         end do
         !close(rmfilno)
      end if
   end if

   ! safety on processes included
   if (allocated(uin)) uin   = lwave*(order - 1)*uin
   if (allocated(vin)) vin   = lwave*(order - 1)*vin
   if (allocated(zbndw)) zbndw = swave*zbndw

   ierror = 0

1234 continue
   return
   end subroutine xbeach_wave_bc

   !> apply computed boundary conditions
   subroutine xbeach_apply_wave_bc()
   use m_sferic
   use m_flowgeom
   use m_flowexternalforcings
   use m_xbeach_data
   use m_physcoef
   use m_flow, only:hs, u1, v, plotlin

   implicit none

   integer                             :: k, itheta, kb, ki, L
   integer                             :: i


   ! initially: all boundaries have Neumann boundary conditions
   do L=Lnxi+1,Lnx
      kb = ln(1,L)
      ki = ln(2,L)
      do itheta=1,ntheta
         ee1(itheta,kb) = ee1(itheta,ki)
         rr(itheta,kb) = rr(itheta,ki)
         
         if (windmodel.eq.1) then
             tt1(itheta,kb) = tt1(itheta,ki)
         endif
         
      end do
   end do

   do k  = 1,nbndw
      ! overwrite with stored boundary conditions
      kb = kbndw(1,k)
      do itheta = 1,ntheta
         ee1(itheta,kb) = zbndw(itheta,k)
         
         if (windmodel.eq.1) then
            tt1(itheta,kb) = Trep
         endif
         
      enddo
   enddo

   if (windmodel.eq.1) then
       sigt  = twopi/tt1
   else
       sigt      = twopi/Trep
   endif
   
end subroutine xbeach_apply_wave_bc


subroutine xbeach_wave_breaker_dissipation(dtmaxwav, break, deltaH, waveps, kwav, km, gamma, gamma2, nroelvink, QB, alpha, Trep, cwav, thetamean, E, D, sigmwav, wci,windmodel)
   use m_flow
   use m_flowgeom
   use m_sferic, only: pi
   use m_physcoef, only: rhomean
   use m_xerf
   use m_xbeach_typesandkinds, only: slen
   
   implicit none
   
   double precision,                 intent(in)     :: dtmaxwav
   character(len=slen),              intent(inout)  :: break
   double precision,                 intent(inout)  :: deltaH
   double precision,                 intent(inout)  :: waveps
   double precision, dimension(Ndx), intent(in)     :: kwav
   double precision, dimension(Ndx), intent(in)     :: km
   double precision,                 intent(in)     :: gamma
   double precision,                 intent(in)     :: gamma2
   double precision,                 intent(in)     :: nroelvink
   double precision, dimension(Ndx), intent(inout)  :: QB
   double precision,                 intent(in)     :: alpha
   double precision,                 intent(in)     :: Trep
   double precision, dimension(Ndx), intent(in)     :: cwav
   double precision, dimension(Ndx), intent(in)     :: thetamean
   double precision, dimension(Ndx), intent(in)     :: E
   double precision, dimension(Ndx), intent(out)    :: D
   double precision, dimension(Ndx), intent(in)     :: sigmwav
   integer                         , intent(in)     :: wci
   integer                         , intent(in)     :: windmodel
   
   integer                                          :: ierr, i, k
   double precision, allocatable                    :: hh(:), hr(:), kmr(:), arg(:), kh(:), Hb(:), Qb_advec(:), ka(:), f(:), gam(:), H(:), R(:)

   call realloc(hh,       ndx, stat=ierr, fill=0d0, keepExisting=.false.)
   call realloc(hr,       ndx, stat=ierr, fill=0d0, keepExisting=.false.)
   call realloc(kmr,      ndx, stat=ierr, fill=0d0, keepExisting=.false.)
   call realloc(arg,      ndx, stat=ierr, fill=0d0, keepExisting=.false.)
   call realloc(kh,       ndx, stat=ierr, fill=0d0, keepExisting=.false.)
   call realloc(Hb,       ndx, stat=ierr, fill=0d0, keepExisting=.false.)
   call realloc(Qb_advec, ndx, stat=ierr, fill=0d0, keepExisting=.false.)
   call realloc(ka,       ndx, stat=ierr, fill=0d0, keepExisting=.false.)
   call realloc(f,        ndx, stat=ierr, fill=0d0, keepExisting=.false.)
   call realloc(gam,      ndx, stat=ierr, fill=0d0, keepExisting=.false.)
   call realloc(H,        ndx, stat=ierr, fill=0d0, keepExisting=.false.)
   call realloc(R,        ndx, stat=ierr, fill=0d0, keepExisting=.false.)

   break = trim(break)
   hh = max(s1-bl, waveps)

   if (break == 'roelvink1') then                  ! Dissipation according to Roelvink (1993)
      H   = sqrt(8.d0*E/rhomean/ag)
      hr  = hh + deltaH*H
      kmr = min(max(kwav, 0.01d0), 100.d0)
      !
      if (wci.ne.0 .or. windmodel.eq.1) then
         arg = -( H / (gamma*tanh(kmr*hr)/kmr))**nroelvink
      else
         arg = -( H / (gamma*hr              ))**nroelvink
      endif
      !
      Qb = min(1.d0 - exp(max(arg,-100.d0)), 1.d0)
      D = Qb * 2.d0 * alpha * E
      !
      if (wci.ne.0 .or. windmodel.eq.1) then
         D = D * sigmwav/2.d0/pi;
      else
         D = D / Trep
      endif

   elseif (break == 'baldock') then                ! Dissipation according to Baldock et al. (1998), only in stationary mode
      if (wci.ne.0 ) then
         f = sigmwav / 2.d0 / pi
         ka = km
      elseif ( windmodel.eq.2) then
         f = sigmwav / 2.d0 / pi
         ka = kwav
      else
         f = 1.d0 / Trep
         ka = kwav
      endif

      kh  = ka * (hh + deltaH*H)

      if (wci.ne.0) then
         gam = 0.76d0*kh + 0.29d0
      else
         gam = gamma
      endif

      H   = sqrt(8.d0/rhomean/ag*E)
      Hb  = tanh(gam*kh/0.88d0)*(0.88d0/max(kwav,1e-10))
      R   = Hb/max(H,0.00001d0)

      Qb   = exp(-R**2)
      D   = 0.25d0 * alpha * f * rhomean * ag * (Hb**2+H**2) * Qb

   elseif (break == 'roelvink2') then
      H   = sqrt(8.d0*E/rhomean/ag)
      hr  = hh + deltaH*H
      kmr = min(max(kwav, 0.01d0), 100.d0)
      !
      if (wci.ne.0) then
         arg = -( H / (gamma*tanh(kmr*hr)/kmr))**nroelvink
      else
         arg = -( H / (gamma*hr              ))**nroelvink
      endif
      !
      Qb  = min(1.d0 - exp(max(arg,-100.d0)), 1.d0)
      D = Qb * 2.d0 * alpha * E
      !
      if (wci.ne.0 .or. windmodel.eq.1) then
         D = D * sigmwav/2.d0/pi * H/hh
      else
         D = D / Trep * H/hh
      endif

   elseif (trim(break) == 'roelvink_daly') then
      H   = sqrt(8.d0*E/rhomean/ag)
      call advec_upw_bulk(thetamean, Qb,cwav,Qb_advec) ! first order upwind, with mean direction
      do k = 1, ndxi
         Qb(k) = Qb(k) - dtmaxwav * Qb_advec(k) * bai(k)
      end do
      hr  = hh + deltaH*H
      kmr = min(max(kwav, 0.01d0), 100.d0)
      where (H > gamma * hr)   Qb = 1.d0
      where (H < gamma2 * hr)  Qb = 0.d0
      Qb = max(Qb, 0.d0)
      D = Qb * 2.d0 * alpha * E
      !
      if (wci.ne.0 .or. windmodel.eq.1) then
         D = D * sigmwav/2.d0/pi * H/hh
      else
         D = D / Trep * H/hh
      endif

   elseif (break == 'janssen') then                 ! Dissipation according to Janssen and Battjes (2007)
      H   = sqrt(8.d0*E/rhomean/ag)
      if (wci.ne.0) then
         f = sigmwav / 2.d0 / pi
         ka = km
      elseif ( windmodel.eq.1) then
         f = sigmwav / 2.d0 / pi
         ka = kwav
      else
         f = 1.d0 / Trep
         ka = kwav
      endif

      kh  = ka * (hh + deltaH*H)
      Hb  = tanh(gamma*kh/0.88d0)*(0.88d0/kwav)
      R   = Hb/max(H,0.00001d0)

      Qb  = 1 + 4/(3*sqrt(pi)) * (R**3 + 3/2*R) * exp(-R**2) - xerf(R)
      D   = 3d0*sqrt(pi)/16d0 * alpha*f*rhomean*ag* (H**3)/hh * Qb    ! alpha is B from the paper, same as Roelvink 1993
   endif

   deallocate(hh, hr, kmr, arg, kh, Hb, Qb_advec, H, R, stat = ierr)

end subroutine xbeach_wave_breaker_dissipation


subroutine advec_horz(dtmaxwav, snx, csx, limtypw, quant, veloc, advec)
   use m_sferic
   use m_physcoef
   use m_flowgeom
   use m_flowparameters, only:eps10
   
   implicit none
   
   integer                                         :: L, k, k1, k2, itheta, ku, kl2s, kl2, kl1, kd, is, ip
   double precision                                :: velocL, qds, qst, half, fluxvel1, waku, sl1, sl2, sl3
   double precision                                :: cf, ds2, ds1, ds, cwuL
   double precision, intent(in)                    :: dtmaxwav
   double precision, intent(in), dimension(ntheta) :: snx, csx
   integer,          intent(in)                    :: limtypw
   double precision, intent(in), dimension(ndx)    :: veloc
   double precision, intent(in), dimension(ntheta,ndx) :: quant
   double precision, intent(out), dimension(ntheta, ndx)  :: advec
   double precision, external                      :: dslim
   
   double precision                                :: cs, sn, wuL
   
   integer                                         :: nwalls
   
   advec = 0d0
   do L  = 1,lnx                                                              ! upwind (supq) + limited high order (dsq), loop over link
        k1  = ln(1,L) ; k2 = ln(2,L)                                       ! linker en rechtercelnr geassocieerd aan de links
        velocL = acL(L)*veloc(k1) + (1d0-acL(L))*veloc(k2)
        
        do itheta = 1,ntheta
            cwuL    = velocL*( csu(L)*csx(itheta) + snu(L)*snx(itheta) )   ! *au(L)   met cwi: u1(L) + cg*( csu(L)*csx(itheta) + snu(L)*snx(itheta) )
                                                                           ! inproduct cgx*csu+cgy*snu

            if (cwuL > 0) then                                              !   ->      ds1   ds2
                k = k1 ; kd = k2 ; is =  1 ; half = 1d0 - acl(L) ; ip = 0   !   ->   ku     k     kd
            else                                                            !   <-      ds2   ds1
                k = k2 ; kd = k1 ; is = -1 ; half = acl(L)       ; ip = 3   !   <-   kd     k     ku
            endif                                                           ! acL = linkse dx fractie van afstand tussen flownodes (slide 83)

            fluxvel1  = is*cwuL*wu(L)                                       ! snelheidsbijdrage linkse cel
            qst = fluxvel1*quant(itheta,k)                                  ! cg*E voor link L, sector itheta
            advec(itheta,kd) = advec(itheta,kd) - qst                       ! downwind cel krijgt bijdrage
            advec(itheta,k)  = advec(itheta,k)  + qst                       ! centrale cel verliest bijdrage

            if (limtypw > 0 ) then                                          ! hogere orde, tijdstapafhankelijk!
                ku  = klnup(1+ip,L)                                         ! pointer upwind cel horende bij link L

                if (ku .ne. 0 ) then
                    kl2s = klnup(2+ip,L) ; kl2 = iabs(kl2s)                 ! 

                    if (ku < 0) then
                        waku = quant(itheta,abs(ku))                        ! pointer naar cel negatief?
                    else
                        kl1  = ku
                        sl1  = slnup(1+ip,L) ; sl2  = slnup(2+ip,L)             ! link upwind cell weight
                        waku  = quant(itheta,kl1)*sl1 + quant(itheta,kl2)*sl2   ! gewogen gemiddelde upwind waarden
                    endif  

                    sl3 = slnup(3+ip,L)
                    cf  =  dtmaxwav*abs(cwuL)*dxi(L)                  
                    cf  =  half*max( 0d0,1d0-cf )                    
                    ds2  =  quant(itheta,kd) - quant(itheta,k)        ! ds1 = voorlopende slope, ds2 = eigen slope
                    ds1  = (quant(itheta,k)  - waku )*sl3

                    if (abs(ds2)  > eps10 .and. abs(ds1) > eps10) then
                        ds  =  cf*dslim(ds1, ds2, limtypw)                  ! reconstructie van totale slope volgens 1 van de 4 schema's                                            ! centraal schema

                        if (abs(ds) > eps10) then                           ! als celgemiddelde niet volstaat
                            qds      =  ds*fluxvel1                         ! slope * linkse celbijdrage
                            advec(itheta,kd) =  advec(itheta,kd) - qds        ! downwind cel krijgt bijdrage
                            advec(itheta,k ) =  advec(itheta,k ) + qds        ! cel verliest bijdrage
                        endif
                    endif
                endif
            endif
        enddo ! directions
    enddo ! links
    
    
!  account for outflow at closed boundaries   
   do nwalls=1,mxwalls
     k1 = walls(1,nwalls)
     
     cs =  walls(8,nwalls) ! outward positive
     sn = -walls(7,nwalls)
     wuL = walls(9,nwalls)
     
     do itheta = 1,ntheta
         cwuL    = veloc(k1)*( cs*csx(itheta) + sn*snx(itheta) )   ! *au(L)   met cwi: u1(L) + cg*( csu(L)*csx(itheta) + snu(L)*snx(itheta) )
         fluxvel1 = cwuL*wuL
         
         if ( fluxvel1.gt.0 ) then
           advec(itheta,k1) = advec(itheta,k1) + fluxvel1*quant(itheta,k1)
         end if
      end do
   end do
   
! account for thin dams
   do nwalls=1,nthd
     k1 = thindam(1,nwalls)
     
     if (k1==7488) then
        continue
     end if
     
     cs = thindam(5,nwalls) 
     sn = -thindam(4,nwalls)
     wuL = thindam(6,nwalls)
     
     do itheta = 1,ntheta
         cwuL    = veloc(k1)*( cs*csx(itheta) + sn*snx(itheta) )   ! *au(L)   met cwi: u1(L) + cg*( csu(L)*csx(itheta) + snu(L)*snx(itheta) )
         fluxvel1 = cwuL*wuL
         
         if ( fluxvel1.gt.0 ) then
           advec(itheta,k1) = advec(itheta,k1) + fluxvel1*quant(itheta,k1)
         end if
      end do
   end do

end subroutine advec_horz


subroutine advec_upw_bulk(thetamean, quant, veloc, advec)
   use m_sferic
   use m_physcoef
   use m_flowgeom
   use m_flow
   
   
   implicit none
   
   integer                                        :: L, k, k1, k2, itheta, kd, is, ip, nwalls
   double precision                               :: velocL, qst, half, fluxvel, cs, sn, wul
   double precision                               :: cwuL, fluxvel1
   double precision, intent(in), dimension(Ndx)   :: thetamean
   double precision, intent(in), dimension(ndx)   :: veloc
   double precision, intent(in), dimension(ndx)   :: quant
   double precision, intent(out), dimension(ndx)  :: advec
   
   advec = 0d0
   do L  = 1,lnx                                                              ! upwind (supq) + limited high order (dsq), loop over link
      k1  = ln(1,L) ; k2 = ln(2,L)                                            ! linker en rechtercelnr geassocieerd aan de links
      velocL = acL(L)*veloc(k1) + (1d0-acL(L))*veloc(k2)
   
      cwuL    = velocL*( csu(L)*dcos(thetamean(k1)) + snu(L)*dsin(thetamean(k1)))    ! met cwi: u1(L) + cg*( csu(L)*csx(itheta) + snu(L)*snx(itheta) )
                                                                            ! inproduct cgx*csu+cgy*snu
   
      if (cwuL > 0) then                                              !   ->      ds1   ds2
         k = k1 ; kd = k2 ; is =  1 ; half = 1d0 - acl(L) ; ip = 0    !   ->   ku     k     kd
      else                                                            !   <-      ds2   ds1
         k = k2 ; kd = k1 ; is = -1 ; half = acl(L)       ; ip = 3    !   <-   kd     k     ku
      endif                                                           ! acL = linkse dx fractie van afstand tussen flownodes (slide 83)
   
      fluxvel  = is*cwuL*wu(L)                                       ! snelheidsbijdrage linkse cel
      qst = fluxvel*quant(k)                                  ! cg*E voor link L, sector itheta
      advec(kd) = advec(kd) - qst                       ! downwind cel krijgt bijdrage
      advec(k)  = advec(k)  + qst                       ! centrale cel verliest bijdrage
   enddo
   
   do nwalls=1,mxwalls
     k1 = walls(1,nwalls)
     
     cs =  walls(8,nwalls) ! outward positive
     sn = -walls(7,nwalls)
     wuL = walls(9,nwalls)
     
     cwuL    = veloc(k1)*( cs*dcos(thetamean(k1)) + sn*dsin(thetamean(k1)) )   ! *au(L)   met cwi: u1(L) + cg*( csu(L)*csx(itheta) + snu(L)*snx(itheta) )
     fluxvel1 = cwuL*wuL
     
     if ( fluxvel1.gt.0 ) then
       advec(k1) = advec(k1) + fluxvel1*quant(k1)
     end if
   end do
   
   ! account for thin dams
   do nwalls=1,nthd
      k1 = thindam(1,nwalls)

      cs = thindam(5,nwalls) 
      sn = -thindam(4,nwalls)
      wuL = thindam(6,nwalls)

      cwuL    = veloc(k1)*( cs*dcos(thetamean(k1)) + sn*dsin(thetamean(k1))  )   ! *au(L)   met cwi: u1(L) + cg*( csu(L)*csx(itheta) + snu(L)*snx(itheta) )
      fluxvel1 = cwuL*wuL

      if ( fluxvel1.gt.0 ) then
         advec(k1) = advec(k1) + fluxvel1*quant(k1)
      end if
   end do 

end subroutine advec_upw_bulk


subroutine advec_dir(quan, veloc, advec)
   use m_sferic
   use m_physcoef
   use m_flowgeom
   use m_flow
   use m_xbeach_data

   implicit none

   integer                                             :: k, itheta
   double precision                                    :: ctheta_between, eeup
   double precision, dimension(ntheta)                 :: fluxtheta
   double precision, dimension(ntheta,ndx), intent(in) :: veloc, quan
   double precision, dimension(ntheta,ndx), intent(out):: advec

   advec = 0d0
   if (ntheta > 1) then
      do k = 1, ndx
         do itheta = 2, ntheta - 2
            ctheta_between = 0.5d0 * (veloc(itheta,k) + veloc(itheta+1,k))
            if (ctheta_between>0) then
               eeup=1.5d0*quan(itheta, k)-.5*quan(itheta-1, k)
               if (eeup<0.d0) then
                  eeup=quan(itheta, k)
               endif
               fluxtheta(itheta)=eeup*ctheta_between
            else
               eeup=1.5d0*quan(itheta+1, k)-.5*quan(itheta+2, k)
               if (eeup<0.d0) then
                  eeup=quan(itheta+1, k)
               endif
               fluxtheta(itheta)=eeup*ctheta_between
            endif
         enddo

         itheta=1                                                    ! only compute for itheta==1
         ctheta_between=.5*(veloc(itheta+1, k)+veloc(itheta, k))
         if (ctheta_between>0) then
            fluxtheta(itheta)=quan(itheta,k)*ctheta_between
         else
            eeup=1.5d0*quan(itheta+1, k)-.5*quan(itheta+2, k)
            if (eeup<0.d0) eeup=quan(itheta+1, k)
            fluxtheta(itheta)=eeup*ctheta_between
         endif

         itheta=ntheta-1                                              ! only compute for itheta==ntheta-1
         ctheta_between=.5*(veloc(itheta+1, k)+veloc(itheta, k))
         if (ctheta_between>0) then
            eeup=1.5d0*quan(itheta, k)-.5*quan(itheta-1, k)
            if (eeup<0.d0) eeup=quan(itheta, k)
            fluxtheta(itheta)=eeup*ctheta_between
         else
            eeup=quan(itheta+1, k)
            fluxtheta(itheta)=eeup*ctheta_between
         endif

         advec(1, k)=(fluxtheta(1)-0.d0)/dtheta                 ! No flux across lower boundary theta grid
         do itheta=2,ntheta-1
            advec(itheta, k)=(fluxtheta(itheta)-fluxtheta(itheta-1))/dtheta
         enddo
         advec(ntheta, k)=(0.d0-fluxtheta(ntheta-1))/dtheta    ! No flux across upper boundary theta grid
      enddo
   endif

end subroutine advec_dir

subroutine advec_horzho_bulk(thetamean, quant, veloc, advec)
   ! advection with velocity u1 of wave turbulence, cfl with dt = dts
   use m_sferic
   use m_physcoef
   use m_flowgeom
   use m_flow
   use m_flowtimes

   implicit none

   integer                                        :: L, k, k1, k2, ku, kl2s, kl2, kl1, kd, is, ip, limtypt, nwalls
   double precision                               :: velocL, qds, qst, half, fluxvel1, waku, sl1, sl2, sl3, wul
   double precision                               :: cf, ds2, ds1, ds, cwuL, cs, sn
   double precision, intent(in), dimension(lnx)   :: veloc
   double precision, intent(in), dimension(ndx)   :: quant
   double precision, intent(in), dimension(ndx)   :: thetamean
   double precision, intent(out), dimension(ndx)  :: advec
   double precision, external                     :: dslim

   advec = 0d0
   do L  = 1,lnx                                                              ! upwind (supq) + limited high order (dsq), loop over link
      k1  = ln(1,L) ; k2 = ln(2,L)                                       ! linker en rechtercelnr geassocieerd aan de links

      if (veloc(L) > 0) then                                              !   ->      ds1   ds2
         k = k1 ; kd = k2 ; is =  1 ; half = 1d0 - acl(L) ; ip = 0   !   ->   ku     k     kd
      else                                                            !   <-      ds2   ds1
         k = k2 ; kd = k1 ; is = -1 ; half = acl(L)       ; ip = 3   !   <-   kd     k     ku
      endif                                                           ! acL = linkse dx fractie van afstand tussen flownodes (slide 83)

      fluxvel1  = is*veloc(L)*wu(L)                                       ! snelheidsbijdrage linkse cel
      qst       = fluxvel1*quant(k)                                  ! cg*E voor link L, sector itheta
      advec(kd) = advec(kd) - qst                       ! downwind cel krijgt bijdrage
      advec(k)  = advec(k)  + qst                       ! centrale cel verliest bijdrage
      limtypt = 4                                       ! Mon Central
      if (limtypt > 0 ) then                                          ! hogere orde, tijdstapafhankelijk door cfl
         ku  = klnup(1+ip,L)                                         ! pointer upwind cel horende bij link L

         if (ku .ne. 0 ) then
            kl2s = klnup(2+ip,L) ; kl2 = iabs(kl2s)                 !

            if (ku < 0) then
               waku = quant(iabs(ku))                        ! pointer naar cel negatief?
            else
               kl1  = ku
               sl1  = slnup(1+ip,L) ; sl2  = slnup(2+ip,L)             ! link upwind cell weight
               waku  = quant(kl1)*sl1 + quant(kl2)*sl2   ! gewogen gemiddelde upwind waarden
            endif

            sl3 = slnup(3+ip,L)
            cf  =  dts*abs(veloc(L))*dxi(L)
            cf  =  half*max( 0d0,1d0-cf )                     ! cf  =  half* (1d0-cf)
            ds2  =  quant(kd) - quant(k)                      ! ds1 = voorlopende slope, ds2 = eigen slope
            ds1  = (quant(k)  - waku )*sl3

            if (abs(ds2)  > eps10 .and. abs(ds1) > eps10) then
               ds  =  cf*dslim(ds1, ds2, limtypt)                  ! reconstructie van totale slope volgens 1 van de 4 schema's

               if (abs(ds) > eps10) then                           ! als celgemiddelde niet volstaat
                  qds      =  ds*fluxvel1                               ! slope * linkse celbijdrage
                  advec(kd) =  advec(kd) - qds        ! downwind cel krijgt bijdrage
                  advec(k ) =  advec(k ) + qds        ! cel verliest bijdrage
               endif
            endif
         endif
      endif
   enddo ! links

   !  account for outflow at closed boundaries
   do nwalls=1,mxwalls
      k1 = walls(1,nwalls)
      cs = walls(8,nwalls)
      sn = -walls(7, nwalls)

      wuL = walls(9,nwalls)
      cwuL    = veloc(k1)*( cs*dcos(thetamean(k1)) + sn*dsin(thetamean(k1)) )  ! *au(L)   met cwi: u1(L) + cg*( csu(L)*csx(itheta) + snu(L)*snx(itheta) )
      fluxvel1 = cwuL*wuL

      if ( fluxvel1.gt.0 ) then
         advec(k1) = advec(k1) + fluxvel1*quant(k1)
      end if
   end do
   
! account for thin dams
   do nwalls=1,nthd
      k1 = thindam(1,nwalls)

      cs = thindam(5,nwalls) 
      sn = -thindam(4,nwalls)
      wuL = thindam(6,nwalls)


      cwuL    = veloc(k1)*( cs*dcos(thetamean(k1)) + sn*dsin(thetamean(k1))  )   ! *au(L)   met cwi: u1(L) + cg*( csu(L)*csx(itheta) + snu(L)*snx(itheta) )
      fluxvel1 = cwuL*wuL

      if ( fluxvel1.gt.0 ) then
         advec(k1) = advec(k1) + fluxvel1*quant(k1)
      end if
   end do   

end subroutine advec_horzho_bulk


   !> reset XBeach wave data
   subroutine xbeach_reset()
   use m_xbeach_readkey ! for reset_paramfile
   implicit none

   call reset_paramfile()

   end subroutine xbeach_reset


   !> compute flow boundary conditions
   subroutine xbeach_flow_bc()
   use m_flowexternalforcings, only: nbndu
   use m_partitioninfo, only: jampi
   implicit none

   integer               :: ierror

   ierror = 1

   if ( nbndu.lt.1 .and. jampi.eq.0 ) goto 1234

   call xbeach_absgen_bc()

   ierror = 0
1234 continue

   return
   end subroutine xbeach_flow_bc

   
!> initialize wave spectra
subroutine xbeach_spectral_wave_init()
   
   use m_xbeach_filefunctions
   use wave_boundary_datastore
   use m_xbeach_data
   use m_flowexternalforcings
   use m_flowgeom
   use m_xbeach_errorhandling
   use m_polygon
   use m_missing
   use m_sferic, only:twopi,jsferic, jasfer3D
   use timespace_triangle
   use m_flowparameters, only: epshs
   use m_flow, only: hs
   use m_flowtimes, only: time0
   use m_partitioninfo
   use m_alloc
   use sorting_algorithms, only: indexx
   use geometry_module, only: dbdistance

   implicit none
   
   character(len=256)                        :: dum

   integer                                   :: fid,err
   integer                                   :: i, itheta, ii, clock
   integer,dimension(nwbnd)                  :: minlocation, idum
   character(slen)                           :: testline
   integer,         dimension(:),allocatable :: iperm, kpl, kL, kR, kLspec
   double precision,dimension(:),allocatable :: drL,        wL, wR, wLspec
   double precision                          :: mindistr
   double precision, dimension(nwbnd)        :: hboundary
   double precision                          :: fac
   double precision                          :: xa, ya, xb, yb, xt, yt
   double precision                          :: disall, dis, xn, yn, rL, darc
   double precision                          :: dum1, dum2
   
   double precision, dimension(:), allocatable :: dist
   integer,          dimension(:), allocatable :: ibndspec
   double precision, dimension(:), allocatable :: xx,yy

   integer                                   :: ibnd, minp, ip, ja
   integer                                   :: k, L, j, k2, LL
   integer                                   :: ierr
   integer                                   :: LL1, LL2
   integer                                   :: kkL, kkR
   double precision                          :: wwL, wwR
   
   type(filenames), dimension(:), allocatable :: tempspecfiles

   !logical, save                             :: bccreated = .false.

   ierr = 1

   ! TODO
   ! randomseed should be set by internal clock time for true
   ! random series. Integer randomseed needs to be distributed
   ! across all domains.
   if (.not. bccreated) then
      bccreated = .true.
      allocate(waveBoundaryParameters(nwbnd)    , stat = ierr)
      if (ierr==0) allocate(randomseed(nwbnd)                , stat = ierr)
      if (ierr==0) allocate(waveBoundaryAdministration(nwbnd), stat = ierr)
      if (ierr==0) allocate(waveBoundaryTimeSeries    (nwbnd), stat = ierr)
      if (ierr==0) allocate(waveSpectrumAdministration(nwbnd), stat = ierr)
   end if
   
   if (random==1) then
      do i=1,nwbnd
         call system_clock(count=clock)
         randomseed(i)=clock
      end do
      !
      if (jampi==1) then
         idum = randomseed
         call reduce_int_max(nwbnd, idum)
         randomseed=idum
      end if
   else
      randomseed=-999
   end if
   
   call get_hboundary(hboundary)

   call get_refpoint(xref0, yref0)
   
!  determine which boundary belongs to a spectrum
   allocate(dist(nspectrumloc))
   allocate(ibndspec(nspectrumloc))
   allocate(xx(nspectrumloc))
   allocate(yy(nspectrumloc))
   dist = 1d99
   ibndspec = 0
   xx = dmiss
   yy = dmiss
   
   
   open(newunit=fid, file=bcfile)
   ! check for LOCLIST
   read(fid,*)testline
   if (trim(testline)=='LOCLIST') then
      !
      do i=1,nspectrumloc
         !        read x, y once
         read(fid,*,IOSTAT=err) xt,yt,dum
         if (err /= 0) then
            ! something has gone wrong during the read of this file
            call writelog('lswe','a,i0,a,a)','error reading line ',i+1,' of file ',bcfile)
            call writelog('lswe','','check file for format errors and ensure the number of  ',&
               'lines is equal to nspectrumloc')
            call xbeach_errorhandler()
         endif
         xx(i) = xt
         yy(i) = yt
      enddo
      !
      do ibnd=1,nwbnd
         !           read boundary polyline
         call oldfil(minp, fnamwbnd(ibnd))
         call delpol()
         call reapol(minp,0)
         
         do i=1,nspectrumloc
            !           determine distance to boundary polyline
            do ip=1,NPL-1
               xt = xx(i)
               yt = yy(i)
               xa = XPL(ip)
               ya = YPL(ip)
               xb = XPL(ip+1)
               yb = YPL(ip+1)
               if ( xa.ne.dmiss .and. xb.ne.dmiss ) then
                  call dlinedis3(xt,yt,xa,ya,xb,yb,ja,dis,xn,yn,rL)
                  if ( abs(dis).lt.abs(dist(i)) ) then ! new closest boundary polygon found for this spectrum
                     dist(i) = abs(dis)
                     ibndspec(i) = ibnd
                  end if
               end if
            end do
         end do
      end do
   end if
   close(fid)

   do ibnd = 1, nwbnd
      LL1 = L1wbnd(ibnd)
      LL2 = L2wbnd(ibnd)

      waveBoundaryParameters(ibnd)%masterFileName = bcfile
      waveBoundaryParameters(ibnd)%np = LL2-LL1+1
      waveBoundaryParameters(ibnd)%ntheta = ntheta
      waveBoundaryParameters(ibnd)%dtheta = dtheta
      waveBoundaryParameters(ibnd)%x0 = xref0
      waveBoundaryParameters(ibnd)%y0 = yref0
      waveBoundaryParameters(ibnd)%hboundary = hboundary(ibnd)

      if(allocated(waveBoundaryParameters(ibnd)%xb)) deallocate(waveBoundaryParameters(ibnd)%xb)
      if(allocated(waveBoundaryParameters(ibnd)%yb)) deallocate(waveBoundaryParameters(ibnd)%yb)
      if(allocated(waveBoundaryParameters(ibnd)%theta)) deallocate(waveBoundaryParameters(ibnd)%theta)

      ! Now allocate arrays to the correct size and set values
      allocate(waveBoundaryParameters(ibnd)%xb(waveBoundaryParameters(ibnd)%np))
      allocate(waveBoundaryParameters(ibnd)%yb(waveBoundaryParameters(ibnd)%np))
      allocate(waveBoundaryParameters(ibnd)%theta(waveBoundaryParameters(ibnd)%ntheta))
      if ( nbndw.gt.0 ) then
         waveBoundaryParameters(ibnd)%xb = xbndw(LL1:LL2)
         waveBoundaryParameters(ibnd)%yb = ybndw(LL1:LL2)
      end if
      waveBoundaryParameters(ibnd)%theta = thetabin

      ! Ensure all theta directions are between 0 and 2pi, required for some trig. on some compilers
      do itheta=1,ntheta
         waveBoundaryParameters(ibnd)%theta(itheta) = mod(waveBoundaryParameters(ibnd)%theta(itheta)+twopi,8.d0*atan(1.d0))
      enddo

      ! Allocate space for the random seed. This seed is set to 40 integers and
      ! should be identical on all processes
      waveBoundaryParameters(ibnd)%randomseed = randomseed(ibnd)

      if (.not.waveBoundaryAdministration(ibnd)%initialized) then

         call writelog('l','','--------------------------------')
         call writelog('l','','Initializing spectral wave boundary conditions for boundary ', ibnd)
         ! Initialize that wave boundary conditions need to be calculated (first time at least)
         ! Stored and defined in spectral_wave_bc_module
         waveSpectrumAdministration(ibnd)%repeatwbc = .false.
         ! Initialize the number of times wave boundary conditions have been generated.
         ! Stored and defined in spectral_wave_bc_module
         waveSpectrumAdministration(ibnd)%bccount  = 0
         ! Initialize bcendtime to zero.
         ! Stored and defined in spectral_wave_bc_module
         waveSpectrumAdministration(ibnd)%spectrumendtime = 0.d0
         ! Initialise lastwaveheight to zero
         ! Stored and defined in wave_boundary_main_module
         allocate(waveSpectrumAdministration(ibnd)%lastwaveelevation(waveBoundaryParameters(ibnd)%np,&
                  waveBoundaryParameters(ibnd)%ntheta))


         if (nspectrumloc<1) then
            call writelog('ewls','','number of boundary spectra (''nspectrumloc'') may not be less than 1')
            call xbeach_errorhandler()
         endif

         ! open location list file
         open(newunit=fid, file=bcfile)
         ! check for LOCLIST
         read(fid,*)testline
         if (trim(testline)=='LOCLIST') then
            allocate(wavespectrumadministration(ibnd)%kL(LL2-LL1+1))       
            allocate(wavespectrumadministration(ibnd)%wL(LL2-LL1+1))
            allocate(wavespectrumadministration(ibnd)%kR(LL2-LL1+1))
            allocate(wavespectrumadministration(ibnd)%wR(LL2-LL1+1))
            
            ii = 0
            do i=1,nspectrumloc
               if ( ibndspec(i).ne.ibnd ) then
                  read(fid,*,IOSTAT=err) dum1, dum2, dum    ! line should be read anyway
                  cycle
               end if
               
               ii = ii + 1
               waveSpectrumAdministration(ibnd)%nspectra = ii
               call realloc(waveSpectrumAdministration(ibnd)%ispectra,ii, keepExisting=.true.,fill=-999)
               call realloc(waveSpectrumAdministration(ibnd)%xspec,ii, keepExisting=.true.,fill=-999d0)
               call realloc(waveSpectrumAdministration(ibnd)%yspec,ii, keepExisting=.true.,fill=-999d0)
!              ugly as hell, but no realloc of derived types available
!              as number of locations usually small, this should not kill performance               
               if (ii==1) then
                  allocate(waveSpectrumAdministration(ibnd)%bcfiles(ii))
               else
                  allocate(tempspecfiles(ii-1))
                  tempspecfiles = waveSpectrumAdministration(ibnd)%bcfiles
                  deallocate(waveSpectrumAdministration(ibnd)%bcfiles)
                  allocate(waveSpectrumAdministration(ibnd)%bcfiles(ii))
                  waveSpectrumAdministration(ibnd)%bcfiles(1:ii-1) = tempspecfiles
                  deallocate(tempspecfiles)
               end if
               waveSpectrumAdministration(ibnd)%ispectra(ii) = i
!              x, y and file name per location
               read(fid,*,IOSTAT=err)wavespectrumadministration(ibnd)%xspec(ii),wavespectrumadministration(ibnd)%yspec(ii),wavespectrumadministration(ibnd)%bcfiles(ii)%fname
               wavespectrumadministration(ibnd)%bcfiles(ii)%listline = 0
               
               if (err /= 0) then
                  ! something has gone wrong during the read of this file
                  call writelog('lswe','a,i0,a,a)','error reading line ',i+1,' of file ',bcfile)
                  call writelog('lswe','','check file for format errors and ensure the number of  ',&
                                          'lines is equal to nspectrumloc')
                  call xbeach_errorhandler()
               endif
               
            enddo

            !     sort spectra in increasing arclength along the wave-energy boundary
            allocate(drL(waveSpectrumAdministration(ibnd)%nspectra),iperm(waveSpectrumAdministration(ibnd)%nspectra),kpl(waveSpectrumAdministration(ibnd)%nspectra))
            allocate(kL(LL2-LL1+1),kR(LL2-LL1+1),wL(LL2-LL1+1),wR(LL2-LL1+1))
            !allocate(kLspec(nspectrumloc),wLspec(nspectrumloc))

            call oldfil(minp, fnamwbnd(ibnd))
            call delpol()
            call reapol(minp,0)
            
            do LL=LL1,LL2
               L = kbndw(3,LL)
               i = LL-LL1+1
               call polyindexweight(xu(L),yu(L),xy2bndw(1,LL),xy2bndw(2,LL),   &
                                    xpl, ypl, (/ (1, k=1,NPL) /), NPL, &
                                    kL(i), wL(i), kR(i), wR(i))
            end do
            
!           project spectrum locations on polyline
            drL = 1d99
            do i=1,waveSpectrumAdministration(ibnd)%nspectra
               
!              find nearest point on polyline
               disall = 1d99
               darc = 0d0
               do ip=1,NPL-1
                  xa = XPL(ip)
                  ya = YPL(ip)
                  xb = XPL(ip+1)
                  yb = YPL(ip+1)
                  xt = wavespectrumadministration(ibnd)%xspec(i)
                  yt = wavespectrumadministration(ibnd)%yspec(i)
                  if ( xa.ne.dmiss .and. xb.ne.dmiss ) then
                     call dlinedis3(xt,yt,xa,ya,xb,yb,ja,dis,xn,yn,rL)
                     if ( dis.lt.disall ) then
                        disall = dis
                        drL(i) = darc + dbdistance(xa,ya,xn,yn, jsferic, jasfer3D, dmiss)
                        !kLspec(i) = ip
                        !wLspec(i) = rL
                     end if
                  end if
            
                  darc = darc + dbdistance(xa,ya,xb,yb, jsferic, jasfer3D, dmiss)
               end do      ! ip
            end do         ! i
   
            call indexx(waveSpectrumAdministration(ibnd)%nspectra,drL,iperm)
            
            !     compute weights from mesh to spectrum locations
            do i=1,LL2-LL1+1
            !  determine arc length along polyline
               darc = 0d0
               do ip=1,kL(i)-1
                  xa = XPL(ip)
                  ya = YPL(ip)
                  xb = XPL(ip+1)
                  yb = YPL(ip+1)
                  darc = darc + dbdistance(xa,ya,xb,yb, jsferic, jasfer3D, dmiss)
               end do
               ip = kL(i)
               xa = XPL(ip)
               ya = YPL(ip)
               xb = XPL(ip+1)
               yb = YPL(ip+1)
               darc = darc + wR(i)*dbdistance(xa,ya,xb,yb, jsferic, jasfer3D, dmiss)
            
            !  determine weights from spectrum locations to boundary links
               j = 1
               do while ( drL(iperm(j)).lt.darc .and. j.lt.waveSpectrumAdministration(ibnd)%nspectra )
                  j=j+1   ! j is right pointer
               end do
               if ( j.gt.1 ) then
                  j=j-1  ! j is left pointer
               end if
            
               !wavespectrumadministration(ibnd)%kL(i) = wavespectrumadministration(ibnd)%ispectra(iperm(j))
               wavespectrumadministration(ibnd)%kL(i) = iperm(j)
               !wavespectrumadministration(ibnd)%kR(i) = wavespectrumadministration(ibnd)%ispectra(iperm(j))
               wavespectrumadministration(ibnd)%kR(i) = iperm(j)
               wavespectrumadministration(ibnd)%wL(i) = 1d0
               wavespectrumadministration(ibnd)%wR(i) = 0d0
               if ( j+1.le.waveSpectrumAdministration(ibnd)%nspectra ) then
                  wavespectrumadministration(ibnd)%kR(i) = iperm(j+1)
                  wavespectrumadministration(ibnd)%wL(i) = min(max( 1d0-(darc-drL(iperm(j))) / (drL(iperm(j+1))-drL(iperm(j))), 0d0), 1d0)
                  wavespectrumadministration(ibnd)%wR(i) = 1d0 - wavespectrumadministration(ibnd)%wL(i)
               end if
            end do         ! i
            
            deallocate(drL,iperm,kpl)
            deallocate(kL,kR,wL,wR)
            
         else      ! no LOCLIST
            if (nspectrumloc==1) then
               allocate(waveSpectrumAdministration(ibnd)%bcfiles(nspectrumloc))
               allocate(waveSpectrumAdministration(ibnd)%xspec(nspectrumloc))
               allocate(waveSpectrumAdministration(ibnd)%yspec(nspectrumloc))
               waveSpectrumAdministration(ibnd)%xspec = xref0
               waveSpectrumAdministration(ibnd)%yspec = yref0
               waveSpectrumAdministration(ibnd)%nspectra = nspectrumloc
               waveSpectrumAdministration(ibnd)%bcfiles(1)%fname = bcfile
               waveSpectrumAdministration(ibnd)%bcfiles(1)%listline = 0     ! for files that have multiple lines, set listline to 0
               allocate(wavespectrumadministration(ibnd)%kL(nbndw))
               allocate(wavespectrumadministration(ibnd)%wL(nbndw))
               allocate(wavespectrumadministration(ibnd)%kR(nbndw))
               allocate(wavespectrumadministration(ibnd)%wR(nbndw))
               wavespectrumadministration(ibnd)%kL = 1
               wavespectrumadministration(ibnd)%wL = 1d0
               wavespectrumadministration(ibnd)%kR = 1
               wavespectrumadministration(ibnd)%wR = 0d0
            else
               call writelog('ewls','','if nspectrumloc>1 then bcfile should contain spectra locations with LOCLIST header')
               close(fid)
               call xbeach_errorhandler()
            endif
         endif

         waveBoundaryAdministration(ibnd)%initialized = .true.

         close(fid)

      end if

      ! Set time to recompute new boundary condition time series to
      ! now so boundary conditions are generated in first time step
      waveBoundaryAdministration(ibnd)%startComputeNewSeries = time0

      call writelog('l','','--------------------------------')
      call delpol()
   end do

   ierr = 0
1234 continue
   return
   end subroutine xbeach_spectral_wave_init


   !> get reference point for wave energy bc
   subroutine get_refpoint(xref0, yref0)
   use m_flowexternalforcings
   use m_partitioninfo
   implicit none

   double precision, intent(out) :: xref0, yref0

   xref0 = huge(0d0)
   yref0 = huge(0d0)
   if ( nbndw.gt.0 ) then
      xref0 = minval(xbndw(1:nbndw))
      yref0 = minval(ybndw(1:nbndw))
   end if
   if ( jampi.eq.1 ) then
      call reduce_double_min(xref0)
      call reduce_double_min(yref0)
   end if

   if ( xref0.eq.huge(0d0) ) then   ! nbndw=0 for all subdomains, or in sequential run
      xref0 = 0d0
      yref0 = 0d0
   end if
   end subroutine get_refpoint


   !> determine average height along wave energy boundary
   subroutine get_hboundary(hboundary)
   use m_flow
   use m_flowgeom
   use m_flowparameters
   use m_xbeach_data
   use m_partitioninfo
   implicit none

   double precision, dimension(nwbnd), intent(out) :: hboundary

   double precision, dimension(nwbnd)              :: dlength

   double precision, dimension(2,nwbnd)            :: dum

   integer                                         :: i, k, k2
   integer                                         :: LL1, LL2, n

   hboundary = 0d0
   dlength   = 0d0
   if ( jampi.eq.0 ) then
      do n=1,nwbnd
         !     integrate along wave boundary
         LL1 = L1wbnd(n)
         LL2 = L2wbnd(n)
         do i=LL1,LL2
            hboundary(n) = hboundary(n) + max(s1(kbndw(1,i))-bl(kbndw(1,i)),epshs) * wu(kbndw(3,i))
            dlength(n)   = dlength(n)   + wu(kbndw(3,i))
         enddo

         !        compute average
         if ( dlength(n).gt.0d0 ) then
            hboundary(n) = hboundary(n) / dlength(n)
         else
            hboundary(n) = 0d0
         end if
      end do
   else
      !     integrate along wave boundary
      do n=1,nwbnd
         LL1 = L1wbnd(n)
         LL2 = L2wbnd(n)
         do i=LL1,LL2
            k2 = kbndw(2,i)
            if ( idomain(k2).eq.my_rank ) then
               hboundary(n) = hboundary(n) + max(s1(kbndw(1,i))-bl(kbndw(1,i)),epshs) * wu(kbndw(3,i))
               dlength(n)   = dlength(n)   + wu(kbndw(3,i))
            end if
         enddo
      end do

      !     global reduction
      do n=1,nwbnd
         dum(1,n) = hboundary(n)
         dum(2,n) = dlength(n)
      end do
      call reduce_sum(2*nwbnd,dum)

      !     compute average
      k=0
      do k=1,nwbnd
         if ( dum(2,k).gt.0d0 ) then
            hboundary(k) = dum(1,k)/dum(2,k)
         else
            hboundary(k) = 0d0
         end if
      end do
   end if

   return
   end subroutine get_hboundary


subroutine xbeach_mombalance
   ! calculates some terms to construct momentum balances
   use m_xbeach_data
   use m_flowparameters, only: epshu
   use m_flowgeom, only: dxi, wcx1, wcy1, wcx2, wcy2, lnx, ln
   use m_flow, only: s1, hu, ucx, ucy
   implicit none

   integer                           :: L, k1, k2, ierr
   double precision                  :: ducxdn, ducydn

   xbdsdx = 0d0; xbdsdy=0d0; xbducxdx=0d0; xbducydx=0d0; xbducxdy=0d0; xbducydy=0d0

   do L = 1,Lnx
      if (hu(L) > epshu) then                            ! link flows
         k1 = ln(1,L)
         k2 = ln(2,L)
         xbdsdx(k1) = xbdsdx(k1) + wcx1(L)*(s1(k2) - s1(k1)) * dxi(L) ! dimension m4/m
         xbdsdy(k1) = xbdsdy(k1) + wcy1(L)*(s1(k2) - s1(k1)) * dxi(L)
         xbdsdx(k2) = xbdsdx(k2) + wcx2(L)*(s1(k2) - s1(k1)) * dxi(L)
         xbdsdy(k2) = xbdsdy(k2) + wcy2(L)*(s1(k2) - s1(k1)) * dxi(L)
      endif
   enddo

   if (.not. (wci>0 .or. trim(absgentype)=='abs_2d')) then   ! then already done
      do L = 1,lnx
         k1 = ln(1,L)
         k2 = ln(2,L)
         ducxdn = dxi(L)*( ucx(k2) - ucx(k1) )
         ducydn = dxi(L)*( ucy(k2) - ucy(k1) )
         xbducxdx(k1) = xbducxdx(k1) + wcx1(L)*ducxdn
         xbducxdy(k1) = xbducxdy(k1) + wcy1(L)*ducxdn
         xbducxdx(k2) = xbducxdx(k2) + wcx2(L)*ducxdn
         xbducxdy(k2) = xbducxdy(k2) + wcy2(L)*ducxdn
         
         xbducydx(k1) = xbducydx(k1) + wcx1(L)*ducydn
         xbducydy(k1) = xbducydy(k1) + wcy1(L)*ducydn
         xbducydx(k2) = xbducydx(k2) + wcx2(L)*ducydn
         xbducydy(k2) = xbducydy(k2) + wcy2(L)*ducydn
      enddo
   endif
   
1234 continue
   return
end subroutine xbeach_mombalance


!>  determine sweep order
!>    it is assumed that the advection velocity c_g is
!>      c_g = veloc (cos(theta), sin(theta)),
!>      with veloc some nonnegative number
!subroutine determine_sweep_order(csx, snx, ctheta, isweep)
!   use m_flowgeom
!   use unstruc_messages
!   use m_xbeach_data, only: ee1
!   implicit none
!   
!   double precision, dimension(ntheta),         intent(in)  :: csx, snx    !< advection direction in (x,y)
!   double precision, dimension(ntheta,Ndx),     intent(in)  :: ctheta      !< advection in theta-direction
!   integer,          dimension(2,ntheta*Ndxi),  intent(out) :: isweep
!   
!   integer,          dimension(:,:),            allocatable :: kmask 
!   
!   integer,                                     parameter   :: Nmax=100
!   integer,          dimension(Nmax)                        :: inxy, outxy, intheta, outtheta
!                                              
!   double precision                                         :: cs, sn
!   double precision                                         :: ctL, ctR
!   double precision                                         :: dir
!                                              
!   integer                                                  :: i, iter, LL, L, Li, itheta, n
!   integer                                                  :: k, ku, kd, jaactive      !  cell-based mask, 0: inactive, >0: active, -1: candidate
!   integer                                                  :: num, numold, numadd
!   integer                                                  :: jastalled
!   
!   integer                                                  :: klast, ithetalast, idx
!   
!   double precision, parameter :: dtol = 1d-14
!   
!   isweep = 0
!   
!   allocate(kmask(ntheta,Ndx))
!   kmask = 0
!   
!   num = 0
!   klast = 0
!   ithetalast = 0
!   
!!  set initial front
!!  mark open boundary links when appropriate
!   do L=Lnxi+1,Lnx
!      do itheta=1,ntheta
!         if ( csu(L)*csx(itheta) + snu(L)*snx(itheta) .ge. 0d0 ) then
!            ku = ln(1,L)
!            kd = ln(2,L)
!            kmask(itheta,ku) = huge(1)
!            kmask(itheta,kd) = -1
!         end if
!      end do
!   end do
!   
!! make cells adjacent to closed boundaries candidates
!   do n=1,mxwalls
!      k = walls(1,n)
!         
!      cs = -walls(8,n) ! inner normal positive
!      sn = walls(7,n)
!      
!      do itheta=1,ntheta
!         if ( cs*csx(itheta) + sn*snx(itheta) .ge. 0d0 ) then
!            kmask(itheta,k) = -1
!         end if
!      end do
!   end do
!   
!!  make cells adjacent to theta-boundaries candidate
!   do k=1,Ndxi
!      kmask(1,k) = -1
!      kmask(ntheta,k) = -1
!   end do
!   
!!  BEGIN DEBUG
!   ithetalast = 8
!   klast = 484
!!  END DEBUG
!   
!      
!!  advance front
!   jastalled = 0
!mainloop: do iter=1,ntheta*Ndxi
!      numold = num
!      numadd = 0  ! candidates added
!      do itheta=1,ntheta
!         do k=1,Ndxi
!            if ( num.ge.ntheta*Ndxi ) exit mainloop
!            
!            if ( jastalled.eq.1 .and. k.eq.2445 .and. itheta.eq.10 ) then
!               continue
!            end if
!            
!            if ( kmask(itheta,k).ne.-1 ) cycle ! candidate cells only
!            
!            if ( jastalled.eq.0 ) then
!               call get_inoutflow_faces(k, itheta, nd(k)%lnx, csx, snx, ctheta(1,k), inxy, intheta, outxy, outtheta)
!            else
!               call get_inoutflow_faces(k, itheta, nd(k)%lnx, csx, snx, ctheta(1,k), inxy, intheta, outxy, outtheta)
!            end if
!            
!!           check if whole stencil is active
!            jaactive = 1
!            
!!           (x,y)-dir            
!            do LL=1,nd(k)%lnx
!               ku = inxy(LL)
!               if ( ku.gt.0 ) then
!                  if ( kmask(itheta,ku).le.0 ) then
!                     jaactive = 0
!                     klast = k
!                     ithetalast = itheta
!                     exit
!                  end if
!               end if
!            end do
!            
!            if ( jaactive.eq.1 .and. intheta(1).gt.0 ) then  ! proceed in theta-dir, left flux
!               if ( kmask(itheta-1,k).le.0 ) then
!                  jaactive = 0
!                  klast = k
!                  ithetalast = itheta
!               end if
!            end if
!               
!            if ( jaactive.eq.1 .and. intheta(2).gt.0 ) then  ! proceed in theta-dir, right flux
!               if ( kmask(itheta+1,k).le.0 ) then
!                  jaactive = 0
!                     klast = k
!                     ithetalast = itheta
!               end if
!            end if
!               
!            if ( jaactive.eq.1 ) then
!!              activate cells
!               num = num+1
!               kmask(itheta,k) = num
!               
!!              set new candidate(s)
!               do LL=1,nd(k)%lnx
!                  kd = outxy(LL)
!                  if ( kd.gt.0 ) then
!                     if ( kmask(itheta,kd).eq.0 ) then
!                        kmask(itheta,kd) = -1 ! candidate
!                        numadd = numadd+1
!                     end if
!                  end if
!               end do
!               
!               if ( outtheta(1).gt.0 ) then
!                  if ( kmask(itheta-1,k).eq.0 ) then
!                        kmask(itheta-1,k) = -1 ! candidate
!                        numadd = numadd+1
!                  end if
!               end if
!               
!               if ( outtheta(2).gt.0 ) then
!                  if ( kmask(itheta+1,k).eq.0 ) then
!                        kmask(itheta+1,k) = -1 ! candidate
!                        numadd = numadd+1
!                  end if
!               end if
!            end if   ! jaactive.eq.1
!            
!         end do
!      end do
!      
!      if ( jastalled.eq.1 ) then
!         continue
!      end if
!      
!      if ( num.eq.numold .and. numadd.eq.0 ) then
!         jastalled = 1
!         
!!        add last cell
!!         kmask(ithetalast,klast) = 1
!!         num = num+1
!!         jastalled = 0
!         exit mainloop
!      end if
!   end do mainloop
!   
!   
!!!  BEGIN DEBUG
!!   if ( jastalled.eq.1 ) then
!!      k = klast
!!      itheta = ithetalast
!!      idx = 0
!!      call print_unsat_downwind(k, itheta, csx, snx, ctheta, kmask, klast, ithetalast, idx)
!!   end if
!!  END DEBUG
!   
!!  fill isweep
!   isweep = 0
!   do k=1,Ndxi
!      do itheta=1,ntheta
!         i = kmask(itheta,k)
!         if (i.gt.0 ) then 
!            isweep(1,i) = k
!            isweep(2,i) = itheta
!         else
!!            call mess(LEVEL_ERROR, 'sweep-order error')
!         end if
!      end do
!   end do
!   
!!!  BEGIN DEBUG
!!   ee1 = 0d0
!!   do k=1,Ndxi
!!      do itheta=1,ntheta
!!         ee1(itheta,k) = dble(kmask(itheta,k))
!!      end do
!!   end do
!!!  END DEBUG
!   
!   if ( allocated(kmask) ) deallocate(kmask)
!   
!   return
!end subroutine determine_sweep_order

!> get inflow and outflow faces
!subroutine get_inoutflow_faces(k, itheta, N, csx, snx, ctheta, inxy, intheta, outxy, outtheta)
!   use m_flowgeom
!   implicit none
!   
!   integer,                                 intent(in)  :: k
!   integer,                                 intent(in)  :: itheta
!   integer,                                 intent(in)  :: N        !< array size (x,y) fluxes
!   double precision, dimension(ntheta),     intent(in)  :: csx      !< normalised advection in x-direction
!   double precision, dimension(ntheta),     intent(in)  :: snx      !< normalised advection in y-direction
!   double precision, dimension(ntheta),     intent(in)  :: ctheta   !< advection in theta-direction
!   integer,          dimension(N),          intent(out) :: inxy     !< (x,y) face inflow (>0) or not (0)
!   integer,          dimension(2),          intent(out) :: intheta  !< theta face inflow (1)  or not (0), order: L,Rinteger, dimension(Nhor) :: inxy    !< (x,y) face inflow (1) or not (0)
!   integer,          dimension(N),          intent(out) :: outxy    !< (x,y) face inflow (>0) or not (0)
!   integer,          dimension(2),          intent(out) :: outtheta !< theta face inflow (1)  or not (0), order: L,R
!   
!   double precision                      :: dir, ctL, ctR
!   
!   integer                               :: LL, L
!   integer                               :: ku, kd
!   
!   double precision, parameter           :: dtol = 1d-8
!   
!   inxy = 0
!   outxy = 0
!   intheta = 0
!   outtheta = 0
!            
!!  (x,y)-dir            
!   do LL=1,nd(k)%lnx
!      L = iabs(nd(k)%ln(LL))
!      dir = csu(L)*csx(itheta) + snu(L)*snx(itheta)
!      
!!     check inflow (x,y) with tolerance      
!      ku = 0
!      kd = 0
!      
!      if ( dir .gt. dtol ) then
!         ku = ln(1,L)
!         kd = ln(2,L)
!      else if ( dir .lt. -dtol ) then
!         ku = ln(2,L)
!         kd = ln(1,L)
!      end if
!      
!      if ( kd.eq.k ) then
!         inxy(LL) = ku
!      end if
!      
!!     check outflow (x,y) with tolerance  
!      ku = 0
!      kd = 0
!      
!      if ( dir .le. -dtol ) then
!         ku = ln(1,L)
!         kd = ln(2,L)
!      else if ( dir .ge. dtol ) then
!         ku = ln(2,L)
!         kd = ln(1,L)
!      end if
!      
!      if ( ku.eq.k ) then
!         outxy(LL) = kd
!      end if
!   end do
!   
!!  check inflow theta, with tolerance   
!   if ( itheta.gt.1 ) then
!      if ( ctheta(itheta-1)*ctheta(itheta).gt.0d0 ) then
!         ctL = 0.5d0*(ctheta(itheta-1)+ctheta(itheta))
!         if ( ctL.gt.dtol ) then
!            intheta(1) = 1
!         end if
!      end if
!   end if
!      
!   if ( itheta.lt.ntheta ) then
!      if ( ctheta(itheta)*ctheta(itheta+1).gt.0d0 ) then
!         ctR = 0.5d0*(ctheta(itheta)+ctheta(itheta+1))
!         if ( ctR.lt.-dtol ) then
!            intheta(2) = 1
!         end if
!      end if
!   end if
!   
!!  check outflow theta, with tolerance   
!   if ( itheta.gt.1 ) then
!      ctL = 0.5d0*(ctheta(itheta-1)+ctheta(itheta))
!      if ( ctL.le.-dtol ) then
!         outtheta(1) = 1
!      end if
!   end if
!      
!   if ( itheta.lt.ntheta ) then
!      ctR = 0.5d0*(ctheta(itheta)+ctheta(itheta+1))
!      if ( ctR.ge.dtol ) then
!         outtheta(2) = 1
!      end if
!   end if
!   
!   return
!end subroutine get_inoutflow_faces

!recursive subroutine print_unsat_downwind(k, itheta, csx, snx, ctheta, kmask, kstart, ithetastart, idx)
!   use m_flowgeom
!   implicit none
!   
!   integer,                                 intent(in)    :: k
!   integer,                                 intent(in)    :: itheta
!   double precision, dimension(ntheta),     intent(in)    :: csx      !< normalised advection in x-direction
!   double precision, dimension(ntheta),     intent(in)    :: snx      !< normalised advection in y-direction
!   double precision, dimension(ntheta,Ndx), intent(in)    :: ctheta   !< advection in theta-direction
!   integer,          dimension(ntheta,Ndx), intent(in)    :: kmask
!   integer,                                 intent(in)    :: kstart
!   integer,                                 intent(in)    :: ithetastart
!   integer,                                 intent(inout) :: idx
!   
!   integer,                                 parameter   :: N=100
!   integer,          dimension(N)                       :: inxy     !< (x,y) face inflow (>0) or not (0)
!   integer,          dimension(2)                       :: intheta  !< theta face inflow (1)  or not (0), order: L,Rinteger, dimension(Nhor) :: inxy    !< (x,y) face inflow (1) or not (0)
!   integer,          dimension(N)                       :: outxy    !< (x,y) face inflow (>0) or not (0)
!   integer,          dimension(2)                       :: outtheta !< theta face inflow (1)  or not (0), order: L,R
!   
!   integer                                              :: LL, ku, kd
!   
!   if ( k.eq.kstart .and. itheta.eq.ithetastart .and. idx.gt.0 ) then
!      write(6,"('circular')")
!      return
!   end if
!   
!   if ( idx.eq.576 ) then
!      continue
!   end if
!   
!   call get_inoutflow_faces(k, itheta, nd(k)%lnx, csx, snx, ctheta, inxy, intheta, outxy, outtheta)
!   write(6,"(I5, ':', 3I7)") idx, k, itheta, kmask(itheta,k)
!   do LL=1,nd(k)%lnx
!      ku = inxy(LL)
!      if ( ku.gt.0 ) then
!         if ( kmask(itheta,ku).le.0 ) then
!            idx = idx+1
!            call print_unsat_downwind(ku, itheta, csx, snx, ctheta, kmask, kstart, ithetastart, idx)
!         end if
!      end if
!   end do
!   
!   if ( intheta(1).gt.0 ) then
!      if ( kmask(itheta-1,k).le.0 ) then
!         idx = idx+1
!         call print_unsat_downwind(k, itheta-1, csx, snx, ctheta, kmask, kstart, ithetastart, idx)
!      end if
!   end if
!   
!   if ( intheta(2).gt.0 ) then
!      if ( kmask(itheta+1,k).le.0 ) then
!         call print_unsat_downwind(k, itheta+1, csx, snx, ctheta, kmask, kstart, ithetastart, idx)
!      end if
!   end if
!   
!   return
!end subroutine print_unsat_downwind

!> solve div(cg E) = 0 by first-order upwind sweeping
!subroutine sweep_xytheta(isweep,quant,veloc,snx,csx,ctheta,rhs,dt)
!   use m_flowgeom
!   use unstruc_messages
!   implicit none
!   
!   integer,          dimension(2,ntheta*Ndxi), intent(in)     :: isweep !< sweep order
!   double precision, dimension(ntheta,Ndx),    intent(inout)  :: quant  !< quantity
!   double precision, dimension(Ndx),           intent(in)     :: veloc  !< magnitude of velocity in (x,y)-dir
!   double precision, dimension(ntheta),        intent(in)     :: snx
!   double precision, dimension(ntheta),        intent(in)     :: csx
!   double precision, dimension(ntheta,Ndx),    intent(in)     :: ctheta !< velocity in theta-dir
!   double precision, dimension(ntheta,Ndx),    intent(in)     :: rhs    !< right-hand side
!   double precision,                           intent(in)     :: dt     !< time step
!   
!   double precision                                           :: velocL, cwuL, ct
!   double precision                                           :: dti
!   double precision                                           :: Ai, Ae
!   double precision                                           :: cs, sn, wuL
!                                                
!   integer                                                    :: kk, k, k1, k2, ku, kd
!   integer                                                    :: LL, L
!   integer                                                    :: itheta
!   integer                                                    :: i
!   integer                                                    :: nwalls
!                                                
!   double precision, parameter                                :: dtol = 0d0
!  
!!!  BEGIN DEBUG
!!   do k=Ndxi+1,Ndx
!!      do itheta=1,ntheta
!!         quant(itheta,k) = yz(k) + xz(k)
!!      end do
!!   end do
!!!  END DEBUG
!   
!   dti = 1d0/dt
!   
!   do i=1,ntheta*Ndxi
!      k = isweep(1,i)
!      itheta = isweep(2,i)
!         
!      if ( k.eq.0 .or. itheta.eq.0 ) then
!         exit
!      end if
!      
!      Ae = -quant(itheta,k)*dti
!      Ai = dti
!      
!      do LL=1,nd(k)%lnx
!         L = iabs(nd(k)%ln(LL))
!                                                          
!         k1 = ln(1,L)
!         k2 = ln(2,L)
!         velocL = acL(L)*veloc(k1) + (1d0-acL(L))*veloc(k2)
!            
!!        BEGIN DEBUG
!!         velocl = 1d0
!!        END DEBUG
!        
!         cwuL   = (velocL*( csu(L)*csx(itheta) + snu(L)*snx(itheta) )) * wu(L)
!
!         if ( cwuL.ge.0d0 ) then
!             ku = k1
!             kd = k2
!         else
!             ku = k2
!             kd = k1
!         endif
!
!         if ( k.eq.ku ) then
!            Ai = Ai + abs(cwuL)*bai(k)
!         else
!            Ae = Ae - abs(cwuL)*bai(k) * quant(itheta,ku)
!         end if
!      end do
!      
!!     closed boundaries
!      do nwalls=1,mxwalls
!         k1 = walls(1,nwalls)
!     
!         if ( k1.ne.k) then
!            cycle
!         end if
!     
!         cs =  walls(8,nwalls) ! outward positive
!         sn = -walls(7,nwalls)
!         wuL = walls(9,nwalls)
!         
!         cwuL   = veloc(k1)*( cs*csx(itheta) + sn*snx(itheta) ) * wuL
!
!         if ( cwuL.gt.0d0 ) then
!            Ai = Ai + cwuL*bai(k)
!         end if
!      end do
!      
!!     theta-dir
!      if ( itheta.gt.1 ) then
!         ct = 0.5d0*(ctheta(itheta-1,k)+ctheta(itheta,k))
!         if ( ct.gt.0d0 ) then
!            Ae = Ae - ct/dtheta*quant(itheta-1,k)
!         else
!            Ai = Ai - ct/dtheta
!         end if
!      end if
!      
!      if ( itheta.lt.ntheta) then
!         ct = 0.5d0*(ctheta(itheta,k)+ctheta(itheta+1,k))
!         if ( ct.gt.0d0 ) then
!            Ai = Ai + ct/dtheta
!         else
!            Ae = Ae + ct/dtheta*quant(itheta+1,k) 
!         end if
!      end if
!      
!      if ( Ai.gt.dtol ) then
!         quant(itheta,k) = -Ae/Ai
!      else if ( Ae.gt.dtol ) then
!         call mess(LEVEL_ERROR, 'sweep: division by zero')
!      end if
!   end do
!   
!   return
!end subroutine sweep_xytheta

!> determine maximum time step for theta advection
!subroutine xbeach_maxdt_dir(sigma, ctheta, dtmax)
!   use m_flowgeom
!   implicit none
!   
!   double precision,                         intent(in)  :: sigma    !< Courant number
!   double precision, dimension(ntheta, Ndx), intent(in)  :: ctheta   !< directional velocity
!   double precision,                         intent(out) :: dtmax    !< maximum time step
!   
!   double precision                                      :: cmax
!   integer                                               :: k, itheta
!   
!   cmax = 0d0
!   do k=1,Ndx
!      do itheta=1,ntheta
!         cmax = max(cmax, ctheta(itheta,k))
!      end do
!   end do
!   
!   if ( cmax.gt.0d0 ) then
!      dtmax = sigma*dtheta/cmax
!   else
!      dtmax = huge(1d0)
!   end if
!   
!   return
!end subroutine xbeach_maxdt_dir

!> initialize solver
subroutine xbeach_inisolver(solver, NDIM, ierror)
   use m_flowgeom
   use m_solver
   implicit none
   
   type(tsolver),               intent(inout) :: solver    !< solver
   integer,                     intent(in)    :: NDIM      !< number of unknowns per flow node
   integer,                     intent(out)   :: ierror    !< error (1) or not (0)
   
   integer,       dimension(:), allocatable   :: numbndlinks   ! number of non-existing boundary links
   
   integer                                    :: kk, kkother, LL
   integer                                    :: i, j
   integer                                    :: ipoint, irow, icolumn
   integer                                    :: numnonzeros
   
   ierror = 1
   
!  count number of non-zeros
   numnonzeros = 0
   do kk=1,Ndx
      numnonzeros = numnonzeros + (1 + nd(kk)%lnx)*NDIM + 2*(NDIM-1)
   end do
   
!  initialize
   solver%numrows            = Ndx*NDIM
   solver%numnonzeros        = numnonzeros
   solver%numnonzerosprecond = 30*solver%numrows
   solver%nwork              = 2*solver%numnonzerosprecond
   
!  allocate
   call xbeach_allocsolver(solver, ierror)
   
   if ( ierror.ne.0 ) goto 1234
   
!  settings
   !!   ipar(1) = 0               ! initialized in "itaux"
   solver%ipar(2) = 1               ! no (0), left (1), right (2), both (3) precond
   solver%ipar(3) = 1               ! stopping criteria
   solver%ipar(4) = solver%nwork    ! number of elems in array 'wk'
   solver%ipar(5) = 10              ! size of Krylov subspace in GMRES and variants
   solver%ipar(6) = 100000            ! max number of mat-vec multiplies

   solver%fpar(1) = 0.0D-16         ! relative tolerance ('exact' solve, except
   solver%fpar(2) = 1.0d-14         ! absolute tolerance

   solver%lfil  = 3
   solver%alpha = 1d0
   !solver%tol   = 0.50D-2
   solver%tol   = 0.1d-2
   
   solver%jabcgstab = 1
   
!  fill CRS administration
   ipoint = 0
   irow = 0
   icolumn = 0
   do kk=1,Ndx ! includes boundaries
      do i=1,NDIM
         irow = irow+1
         
!        diagonal entry         
         ipoint = ipoint+1
         icolumn = irow
         solver%ia(irow) = ipoint
         solver%ja(ipoint) = icolumn
         
!        off-diagonals
         do j=1,nd(kk)%lnx
            ipoint  = ipoint+1
            LL      = iabs(nd(kk)%ln(j))
            kkother = ln(1,LL)+ln(2,LL)-kk
            icolumn = (kkother-1)*NDIM + i
            solver%ja(ipoint) = icolumn
         end do
         
!        i-dir, i-1
         if ( i.gt.1 ) then
            ipoint = ipoint+1
            icolumn = (kk-1)*NDIM + i-1
            solver%ja(ipoint) = icolumn
         end if
         
!        i-dir, i+1
         if ( i.lt.NDIM ) then
            ipoint = ipoint+1
            icolumn = (kk-1)*NDIM + i+1
            solver%ja(ipoint) = icolumn
         end if
         
         solver%ia(irow+1) = ipoint
      end do
   end do
   
   solver%ia(irow+1) = ipoint+1
   
   ierror = 0
1234 continue
   
   return
end subroutine xbeach_inisolver

!> (re)allocate solver
!>    it is assumed that number of rows, number of non-zero entries, number of non-zero entries in preconditioner and size of work array are set
subroutine xbeach_allocsolver(solver, ierror)
   use m_solver
   use unstruc_messages
   use m_alloc
   implicit none
   
   type(tsolver), intent(inout) :: solver   !< solver
   integer,       intent(inout) :: ierror   !< error (1) or not (0)
   
   ierror = 1
   
!  check sizes   
   if ( solver%numrows.le.0 .or. &
        solver%numnonzeros.le.0  .or.  &
        solver%numnonzerosprecond.le.0 .or.  &
        solver%nwork.le.0 ) then
       goto 1234
   end if
   
   call realloc(solver%a,     solver%numnonzeros,        keepExisting=.false., fill=0d0)
   call realloc(solver%ia,    solver%numrows+1,          keepExisting=.false., fill=0)
   call realloc(solver%ja,    solver%numnonzeros,        keepExisting=.false., fill=0)
   
   call realloc(solver%rhs,   solver%numrows,            keepExisting=.false., fill=0d0)

   call realloc(solver%alu,   solver%numnonzerosprecond, keepExisting=.false., fill=0d0)
   call realloc(solver%ju,    solver%numrows,            keepExisting=.false., fill=0)
   call realloc(solver%jlu,   solver%numnonzerosprecond, keepExisting=.false., fill=0)
   
   call realloc(solver%work,  solver%nwork,              keepExisting=.false., fill=0d0)
   call realloc(solver%jw,  2*solver%numnonzeros,        keepExisting=.false., fill=0)
   
   ierror = 0
1234 continue

   if ( ierror.ne.0 ) then
      call mess(LEVEL_ERROR, 'alloc_solver: error')
      call xbeach_deallocsolver(solver)
   end if

   return
end subroutine xbeach_allocsolver

!> deallocate solver
subroutine xbeach_deallocsolver(solver)
   use m_solver
   use m_alloc
   implicit none
   
   type(tsolver), intent(inout) :: solver   !< solver
   
   if ( allocated(solver%a)     ) deallocate(solver%a)
   if ( allocated(solver%ia)    ) deallocate(solver%ia)
   if ( allocated(solver%ja)    ) deallocate(solver%ja)
   
   if ( allocated(solver%rhs)   ) deallocate(solver%rhs)
   
   if ( allocated(solver%alu)   ) deallocate(solver%alu)
   if ( allocated(solver%ju)    ) deallocate(solver%ju)
   if ( allocated(solver%jlu)   ) deallocate(solver%jlu)
   
   if ( allocated(solver%work)  ) deallocate(solver%work)
   if ( allocated(solver%jw)    ) deallocate(solver%jw)
   
   solver%numrows = 0
   solver%numnonzeros = 0
   solver%numnonzerosprecond = 0
   solver%nwork = 0
   
   return
end subroutine xbeach_deallocsolver


!> fill matrix entries and right-hand side
subroutine xbeach_fillsystem(solver,NDIM,quant,src_coeff,src_expl,veloc,csx,snx,ci,delta,dt,nbnd,kbnd,zbnd,jawritesystem,ierror)
   use m_flowgeom
   use m_solver
   use m_flow, only: hu, epshu, hs, epshs
   use m_missing
   implicit none
   
   type(tsolver),                          intent(inout) :: solver !< solver
   integer,                                intent(in)    :: NDIM   !< number of unknows per flow node
   double precision, dimension(NDIM,Ndx),  intent(inout) :: quant  !< quantity
   double precision, dimension(NDIM,Ndx),  intent(in)    :: src_coeff !< coefficient of sources
   double precision, dimension(NDIM,Ndx),  intent(in)    :: src_expl  !< explicit sources
   double precision, dimension(Ndx),       intent(in)    :: veloc  !< magnitude of velocity in (x,y)-dir
   double precision, dimension(NDIM),      intent(in)    :: csx    !< advection direction
   double precision, dimension(NDIM),      intent(in)    :: snx    !< advection direction
   double precision, dimension(NDIM,Ndx),  intent(in)    :: ci     !< velocity in i-direction
                                           
                                           
   double precision,                       intent(in)    :: delta  !< mesh width in i-direction
                                           
   double precision,                       intent(in)    :: dt     !< time step
                                           
   integer,                                intent(in)    :: nbnd   !< number of Dirichlet boundary conditions
   integer,          dimension(nbnd),      intent(in)    :: kbnd   !< Dirichlet boundary condition cell numbers
   double precision, dimension(NDIM,nbnd), intent(in)    :: zbnd   !< boundary values
   integer,                                intent(in)    :: jawritesystem  !< write system for debug (1) or not (0)
   
   integer,                                intent(inout) :: ierror !< error (1) or not (0)
                                           
   double precision, dimension(:,:),       allocatable   :: dfluxfac
   double precision, dimension(:,:),       allocatable   :: bndval
                                           
   double precision                                      :: dti, dti_loc
   double precision                                      :: cwuL, ct
   double precision                                      :: rowsum
   double precision                                      :: velocL
   double precision                                      :: cs, sn, wuL
                                                       
   integer                                               :: ipointdiag
   integer                                               :: i, j, n
   integer                                               :: kk, kkother
   integer                                               :: k1, k2
   integer                                               :: L, LL, irow, icol, ipoint, lunfil
                                           
   double precision,                       parameter     :: dtol = 1d-10                                
   
   ierror = 1
   
!  allocate
   allocate(dfluxfac(2,NDIM*Lnx))
   if ( Ndx.gt.Ndxi ) then
      allocate(bndval(NDIM,Ndx-Ndxi))
      bndval=DMISS
   end if
   
   dti = 1d0/dt
   
!  initialize
   solver%a = 0d0
   solver%rhs = 0d0
   dfluxfac = 0d0
   
!  compute fluxes
   do LL=1,Lnx
!      if ( hu(LL).gt.epshu ) then
         k1 = ln(1,LL)
         k2 = ln(2,LL)
         velocL = acL(LL)*veloc(k1) + (1d0-acL(LL))*veloc(k2)
         
         do i=1,NDIM
            cwuL = velocL*(csu(LL)*csx(i) + snu(LL)*snx(i))*wu(LL)
            
            L = (LL-1)*NDIM+i
            dfluxfac(1,L) = max(cwuL,0d0)   
            dfluxfac(2,L) = min(cwuL,0d0)

         end do
!      end if
   end do
   
!  fill matrix entries   
   irow = 0
   ipoint = 0
   do kk=1,Ndxi   ! internal
      do i=1,NDIM
         irow = irow+1
         
         ipoint = ipoint+1
         ipointdiag = ipoint
         do j=1,nd(kk)%lnx
            LL = iabs(nd(kk)%ln(j))
            
            ipoint = ipoint+1
            
!           check row number
            kkother = ln(1,LL) + ln(2,LL) - kk
            if ( solver%ja(ipoint).ne.(kkother-1)*NDIM+i ) then
!               call qnerror(' ', ' ', ' ')
               ierror = 1
               goto 1234
            end if
            
            L = (LL-1)*NDIM+i
 
            if ( ln(1,LL).eq.kk ) then
               solver%a(ipointdiag) = solver%a(ipointdiag) + dfluxfac(1,L)/ba(kk)
               solver%a(ipoint)     = solver%a(ipoint)     + dfluxfac(2,L)/ba(kk)
            else if ( ln(2,LL).eq.kk ) then
               solver%a(ipointdiag) = solver%a(ipointdiag) - dfluxfac(2,L)/ba(kk)
               solver%a(ipoint)     = solver%a(ipoint)     - dfluxfac(1,L)/ba(kk)
            else
               ierror=1
               goto 1234
            end if
         end do
         
!        i-dir, i-1
         if ( i.gt.1 ) then
            ct = 0.5d0*(ci(i-1,kk)+ci(i,kk))
         
            ipoint=ipoint+1
            solver%a(ipoint) = solver%a(ipoint) - max(ct,0d0)/delta
            solver%a(ipointdiag) = solver%a(ipointdiag) - min(ct,0d0)/delta
            !solver%a(ipoint) = solver%a(ipoint) - min(ct,0d0)/delta
            !solver%a(ipointdiag) = solver%a(ipointdiag) - max(ct,0d0)/delta
         else
!            ct = ci(i,kk)
!         
!            ipoint=ipoint+1
!            solver%a(ipointdiag) = solver%a(ipointdiag) - min(ct,0d0)/delta
         end if
         
!        i-dir, i+1
         if ( i.lt.NDIM ) then
            ct = 0.5d0*(ci(i,kk)+ci(i+1,kk))
         
            ipoint=ipoint+1
            solver%a(ipoint) = solver%a(ipoint) + min(ct,0d0)/delta
            solver%a(ipointdiag) = solver%a(ipointdiag) + max(ct,0d0)/delta 
            !solver%a(ipoint) = solver%a(ipoint) + max(ct,0d0)/delta
            !solver%a(ipointdiag) = solver%a(ipointdiag) + min(ct,0d0)/delta
         else
!            ct = ci(i,kk)
!         
!            solver%a(ipointdiag) = solver%a(ipointdiag) + max(ct,0d0)/delta
         end if
      end do
   end do
   
!  unspecified boundary conditions: homogeneous Dirichlet
   do LL=Lnxi+1,Lnx
      kk = ln(1,LL)
      do i=1,NDIM
         irow = (kk-1)*NDIM+i
         ipoint = solver%ia(irow)
         
         L = (LL-1)*NDIM+i
!         if ( dfluxfac(1,L).gt.0d0 ) then  ! inflow
!            solver%a(ipoint)   =  1d0  ! diagonal entry
!!           off-diagonal entries
!            do ipoint=solver%ia(irow)+1,solver%ia(irow+1)-1
!               solver%a(ipoint) = 0d0
!            end do
!            solver%rhs(irow)   =  0d0
!         else
            solver%a(ipoint)   =  1d0  ! diagonal entry
            solver%a(ipoint+1) = -1d0  ! off-diagonal entry
            solver%rhs(irow)   =  0d0
!         end if
      end do
   end do
   
!  Dirichlet boundary conditions
   do n=1,nbnd
      kk = kbnd(n)
      do i=1,NDIM
         irow = (kk-1)*NDIM + i
!        diagonal entry
         ipoint = solver%ia(irow)
         solver%a(ipoint) = 1d0
!        off-diagonal entries
         do ipoint=solver%ia(irow)+1,solver%ia(irow+1)-1
            solver%a(ipoint) = 0d0
         end do
         solver%rhs(irow) = zbnd(i,n)
      end do
   end do
   
!  closed boundaries
   do n=1,mxwalls
      kk = walls(1,n)
   
      cs =  walls(8,n) ! outward positive
      sn = -walls(7,n)
      wuL = walls(9,n)
      
      do i=1,NDIM
         cwuL   = veloc(kk)*( cs*csx(i) + sn*snx(i) ) * wuL
         
         if ( cwuL.gt.0d0 ) then
!           get row number
            irow = (kk-1)*NDIM + i
!           get location of diagonal entry
            ipoint = solver%ia(irow)
!           add boundary flux            
            solver%a(ipoint) = solver%a(ipoint) + cwuL/ba(kk)
         end if
      end do
   end do
   
! thin dams
   do n=1,nthd
      kk = thindam(1,n)
   
      cs =  thindam(5,n) ! outward positive
      sn = -thindam(4,n)
      wuL = thindam(6,n)
      
      do i=1,NDIM
         cwuL   = veloc(kk)*( cs*csx(i) + sn*snx(i) ) * wuL
         
         if ( cwuL.gt.0d0 ) then
!           get row number
            irow = (kk-1)*NDIM + i
!           get location of diagonal entry
            ipoint = solver%ia(irow)
!           add boundary flux            
            solver%a(ipoint) = solver%a(ipoint) + cwuL/ba(kk)
         end if
      end do
   end do
   
!  add time derivative and sources
   do kk=1,Ndxi
      do i=1,NDIM
         irow = (kk-1)*NDIM + i
         ipoint = solver%ia(irow)
         solver%a(ipoint) = solver%a(ipoint) + dti - src_coeff(i,kk)
         solver%rhs(irow) = solver%rhs(irow) + dti*quant(i,kk) + src_expl(i,kk)
      end do
   end do
   
!!  check diagonal entries   
!   do irow=1,Ndxi*NDIM
!      ipoint = solver%ia(irow)
!      if ( abs(solver%a(ipoint)).lt.dtol ) then
!         solver%a(ipoint) = 1d0
!         do i=ipoint+1,solver%ia(irow+1)-1
!            solver%a(i) = 0d0
!         end do
!         solver%rhs(irow) = 0d0
!      end if
!   end do
   
   if ( jawritesystem.eq.1 ) then
!     write matrix
      open(newunit=lunfil,file='system.m')
      write(lunfil,"('dum = [')")
      do irow=1,solver%numrows
         do j=solver%ia(irow),solver%ia(irow+1)-1
            icol = solver%ja(j)
            write(lunfil,"(2I7,E15.5)") irow, icol, solver%a(j)
         end do
      end do
      write(lunfil,"('];')")
      write(lunfil,"('A=sparse(dum(:,1), dum(:,2), dum(:,3));')")
      
!     write rhs
      write(lunfil,"('rhs = [')")
      do irow=1,solver%numrows
         write(lunfil,"(E15.5)") solver%rhs(irow)
      end do
      write(lunfil,"('];')")
      
      close(lunfil)
   end if
   
   ierror = 0
1234 continue   

!  deallocate
   if ( allocated(dfluxfac) ) deallocate(dfluxfac)
   if ( allocated(bndval)   ) deallocate(bndval)
   
   return
end subroutine xbeach_fillsystem

    
!> solve linear system
subroutine xbeach_solvesystem(solver,sol,iters,ierror)
   use m_solver
   use m_alloc
   use unstruc_messages
   use m_saad, only : ilud, runrc2
   implicit none
   
   type(tsolver),                               intent(inout) :: solver !< solver
   double precision, dimension(solver%numrows), intent(inout) :: sol    !< solution vector
   integer,                                     intent(out)   :: iters  !< number of iterations
   integer,                                     intent(inout) :: ierror !< error (1) or not (0)
   
   integer                                                    :: irealloc
   
   logical                                                    :: Lredo
   
   double precision,                            parameter     :: REALLOCFAC = 1.2d0
   integer,                                     parameter     :: MAXREALLOC = 15
   
   ierror = 1
   
!  compute preconditioner
   irealloc = 0
   
   Lredo = .true.
   do while ( Lredo )
      irealloc = irealloc+1
      call ilud(solver%numrows,solver%a,solver%ja,solver%ia,solver%alpha,solver%tol,solver%alu,solver%jlu,solver%ju,solver%numnonzerosprecond,solver%work,solver%jw,ierror,solver%numnonzeros)
      
      Lredo = .false.
      if ( irealloc.lt.MAXREALLOC .and. ierror.eq.-2 ) then
         Lredo = .true.
         solver%numnonzerosprecond = 1 + int(REALLOCFAC*dble(solver%numnonzerosprecond))
         call realloc(solver%alu,   solver%numnonzerosprecond, keepExisting=.false., fill=0d0)
         call realloc(solver%jlu,   solver%numnonzerosprecond, keepExisting=.false., fill=0)
      end if
   end do
   if ( ierror.ne.0 ) goto 1234
   
!  solve system   
   call runrc2(solver%numrows,solver%rhs,sol,solver%ipar,solver%fpar,solver%work,solver%a,solver%ja,solver%ia,solver%alu,solver%jlu,solver%ju,iters,solver%eps,solver%jabcgstab,ierror,solver%numnonzerosprecond)
   if ( ierror.ne.0 ) goto 1234
   
   ierror=0
1234 continue

   if ( ierror.ne.0 ) then
      call mess(LEVEL_ERROR, 'xbeach_solvesystem gave error')
   end if
   
   return
end subroutine xbeach_solvesystem



!> update energy field and roller energy for stationary case
subroutine update_ee1rr(dtmaxwav, sigt, cgwav, ctheta, horadvec, thetaadvec, E, H, thet, thetamean,   &
                        sigmwav, gammax, hh, &
                        fw, break, deltaH, waveps, kwav, km, gamma, gamma2, nroelvink, QB, alpha, trep, R, cwav, D,   &
                        roller, br, &
                        urms_cc, fwcutoff, Df, DDlok, wete, rrhoradvec, rrthetaadvec,  jawsource, mwind, &
                        snx, csx, limtypw, &
                        ee1, rr, drr, wci, rhs, solver, nbndw, kbndw, zbndw, d_relaxfac)
   use m_flowgeom, only: ntheta, Ndxi, Ndx, Lnx, ba, bai, dtheta, thetabin, xz, yz
   use m_flowparameters, only: epshs
   use m_flow, only: vol1 
   use m_physcoef, only: rhog, rhomean, ag
   use m_xbeach_typesandkinds, only: slen
   use m_wind, only: jawind, wx, wy
   use m_solver
   use m_sferic, only: pi
   implicit none
   
   double precision,                          intent(in)     :: dtmaxwav     !< time step
   double precision, dimension(ntheta,Ndx),   intent(in)     :: sigt         !< relative frequency
   double precision, dimension(Ndx),          intent(inout)  :: cgwav        !< group velocity
   double precision, dimension(ntheta,Ndx),   intent(inout)  :: ctheta       !< refraction velocity
   double precision, dimension(ntheta,Ndx),   intent(inout)  :: horadvec     !< horizontal advection (work array)
   double precision, dimension(ntheta,Ndx),   intent(inout)  :: thetaadvec   !< directional advection (work array)
   double precision, dimension(Ndx),          intent(inout)  :: E            !< bulk energy (work array)
   double precision, dimension(Ndx),          intent(inout)  :: H            !< significant wave height (work array)
   double precision, dimension(ntheta,Ndx),   intent(inout)  :: thet         !< significant wave height (work array)
   double precision, dimension(Ndx),          intent(inout)  :: thetamean
   double precision, dimension(Ndx),          intent(inout)  :: sigmwav
   double precision,                          intent(in)     :: gammax
   double precision, dimension(Ndx),          intent(inout)  :: hh
   double precision, dimension(Ndx),          intent(in)     :: fw
                                              
   character(len=slen),                       intent(inout)  :: break
   double precision,                          intent(inout)  :: DeltaH
   double precision,                          intent(inout)  :: waveps
   double precision, dimension(Ndx),          intent(in)     :: kwav
   double precision, dimension(Ndx),          intent(in)     :: km
   double precision,                          intent(in)     :: gamma
   double precision,                          intent(in)     :: gamma2
   double precision,                          intent(in)     :: nroelvink
   double precision, dimension(Ndx),          intent(inout)  :: QB
   double precision,                          intent(in)     :: alpha
   double precision,                          intent(in)     :: Trep
   double precision,                          intent(in)     :: mwind
   double precision, dimension(Ndx),          intent(inout)  :: R
   double precision, dimension(Ndx),          intent(in)     :: cwav
   double precision, dimension(Ndx),          intent(inout)  :: D
                                              
   integer,                                   intent(in)     :: roller
   integer,                                   intent(in)     :: wci
   integer,                                   intent(in)     :: jawsource
   double precision, dimension(Ndx),          intent(in)     :: br
   double precision, dimension(Ndx),          intent(inout)  :: urms_cc
   double precision,                          intent(inout)  :: fwcutoff
   double precision, dimension(Ndx),          intent(inout)  :: Df
   double precision, dimension(ntheta,Ndx),   intent(inout)  :: DDlok
   double precision, dimension(ntheta,Ndx),   intent(inout)  :: wete
   double precision, dimension(ntheta,Ndx),   intent(inout)  :: rrhoradvec
   double precision, dimension(ntheta,Ndx),   intent(inout)  :: rrthetaadvec
                                              
   double precision, dimension(ntheta),       intent(in)     :: snx, csx
   
   integer,                                   intent(in)     :: limtypw
                                              
   double precision, dimension(ntheta,Ndx),   intent(inout)  :: ee1          !< energy field
   double precision, dimension(ntheta,Ndx),   intent(inout)  :: rr           !< roller energy
   double precision, dimension(ntheta,Ndx),   intent(inout)  :: drr          !< roller energy dissipation     

   double precision, dimension(ntheta,Ndx),   intent(inout)  :: rhs          !< right-hand side, work array
   type(tsolver),                             intent(inout)  :: solver       !< solver
   integer,                                   intent(in)     :: nbndw        !< number of Dirichlet boundary nodes
   integer,          dimension(nbndw),        intent(in)     :: kbndw        !< boundary nodes
   double precision, dimension(ntheta,nbndw), intent(inout)  :: zbndw        !< boundary values
   double precision,                          intent(in)     :: d_relaxfac
   
   double precision, dimension(:,:),          allocatable    :: src_coeff    ! coefficient of sources
   double precision, dimension(:,:),          allocatable    :: src_expl     ! explicit sources
   double precision, dimension(:),            allocatable    :: Dprev        ! explicit sources
   
   double precision                                          :: dfac
   double precision                                          :: dis
   integer                                                   :: k, itheta
   integer                                                   :: n
   integer                                                   :: iters, ierror, lunfil
   
   integer, save :: jaoutput=0
      
!  allocate and initialize
   allocate(src_coeff(ntheta,Ndx))
   src_coeff = 0d0
   allocate(src_expl(ntheta,Ndx))
   src_expl  = 0d0
   !allocate(Dprev(Ndx))
   !Dprev = 0d0
   
   thetamean=(sum(ee1*thet,1)/dble(ntheta))/(max(sum(ee1,1),0.00001d0)/dble(ntheta)) ! energy weighted wave direction
   sigmwav = max((sum(sigt,1)/dble(ntheta)),epshs)

!
!  Energy integrated over wave directions,Hrms
!
   E=sum(ee1,dim=1)*dtheta
   H=sqrt(8.d0*E/rhomean/ag)

!  formulate limitation of wave energy as source
   if ( dtmaxwav.gt.0d0 ) then
      do k=1,Ndx
         do itheta=1,ntheta
            src_coeff(itheta,k) = -(max(1.d0,(H(k)/(gammax*hh(k)))**2) - 1d0) / dtmaxwav
         end do
      end do
   end if

   H=min(H,gammax*hh)
   E=1.d0/8.d0*rhomean*ag*(H**2)   

!  Breaker dissipation
   !Dprev = D
   call xbeach_wave_breaker_dissipation(dtmaxwav, break, DeltaH, waveps, kwav, km, gamma, gamma2, nroelvink, QB, alpha, Trep, cwav, thetamean, E, D, sigmwav, wci, 0)
   !D = Dprev*(1d0-d_relaxfac) + d_relaxfac*D
   
!  Dissipation by bed friction
   do k=1,Ndx
      dfac = 0.28d0*fw(k)*rhomean
!      urms_cc(k) = pi * H(k) / Trep / sinh(min(max(kwav(k),0.01d0)*max(hh(k),deltaH*H(k)),10.0d0))
      urms_cc(k) = pi * H(k) / sigmwav(k) / 2.d0 / pi / sinh(min(max(kwav(k),0.01d0)*max(hh(k),deltaH*H(k)),10.0d0))
      Df(k)=dfac*urms_cc(k)**3
   end do

   do k=1,Ndx
      if ( hh(k).gt.fwcutoff ) then
         Df(k) = 0d0
      end if
   end do
   
!  construct and solve system
   
!  scale wave energy to wave action 
   ee1 = ee1/sigt   
   
!  scale boundary conditions to wave action
   do n=1,nbndw
      do itheta=1,ntheta
         k = kbndw(n)
         zbndw(itheta,n) = zbndw(itheta,n) / sigt(itheta,k)
      end do
   end do          
   
   do k=1,Ndx
      do itheta=1,ntheta
         dis = (D(k)+Df(k))/max(E(k),1d-10)
         src_coeff(itheta,k) = (src_coeff(itheta,k) - dis) / sigt(itheta,k)
!         src_expl(itheta,k) = src_expl(itheta,k) 
      end do
   end do  
   

   call xbeach_fillsystem(solver,ntheta,ee1,src_coeff,src_expl,cgwav,csx,snx,ctheta,dtheta,dtmaxwav,nbndw,kbndw,zbndw,jaoutput,ierror)
   write(6,*) 'Fill wave energy system:: ierror=', ierror
   
   call xbeach_solvesystem(solver,ee1,iters,ierror)
   write(6,*) 'Solve wave energy system:: ierror=', ierror, ', no of iters=',iters
   
   if ( jaoutput.eq.1 ) then
      open(newunit=lunfil,file='tmp.m')
      write(lunfil,"('ee1= [', $)")
      do k=1,Ndx
         do itheta=1,ntheta
            write(lunfil,"(E15.5, $)") ee1(itheta,k)
         end do
      end do
      write(lunfil,"('];')")
   
      write(lunfil,"('x= [', $)")
      do k=1,Ndx
            write(lunfil,"(E15.5, $)") xz(k)
      end do
      write(lunfil,"('];')")
   
      write(lunfil,"('y= [', $)")
      do k=1,Ndx
            write(lunfil,"(E15.5, $)") yz(k)
      end do
      write(lunfil,"('];')")
   
      write(lunfil,"('Ndxi=', I0, ';')") Ndxi
   
      close(lunfil)
      
      jaoutput = 0
   end if
   
   do k=1,Ndx
      if ( vol1(k).lt.epshs*ba(k) ) then
         do itheta=1,ntheta
            ee1(itheta,k) = 0d0
         end do
      end if
   end do

!  scale wave action to wave energy
   ee1 = ee1*sigt                   ! Back to wave energy
   
!  scale boundary conditions to wave energy (safety)
   do n=1,nbndw
      do itheta=1,ntheta
         k = kbndw(n)
         zbndw(itheta,n) = zbndw(itheta,n) * sigt(itheta,k)
      end do
   end do
   
   if ( roller.eq.1 ) then
      
      ! Roller balance
      do k=1,Ndx
         do itheta=1,ntheta
            src_coeff(itheta,k) =  -2*ag*BR(k)/cwav(k)
            src_expl(itheta,k) = ee1(itheta,k)*D(k)/max(E(k),1d-10)
         end do
      end do
   
      call xbeach_fillsystem(solver,ntheta,rr,src_coeff,src_expl,cwav,csx,snx,ctheta,dtheta,dtmaxwav,0, (/ 0 /), (/ 0d0 /), 0, ierror)
      write(6,*) 'Fill roller energy system:: ierror=', ierror
      
      call xbeach_solvesystem(solver,rr,iters,ierror)
      write(6,*) 'Solve roller energy system:: ierror=', ierror, ', no of iters=',iters
      
      do k=1,Ndx
         if ( vol1(k).lt.epshs*ba(k) ) then
            do itheta=1,ntheta
               rr(itheta,k) = 0d0
            end do
         end if
      end do
      
   else
      rr = 0d0
   end if

   rr=max(rr,0.0d0)
!
   do itheta = 1, ntheta
      where (hh+deltaH*H>epshs) 
           wete(itheta,:)=1d0
      elsewhere
           wete(itheta,:)=0d0
      end where
   enddo
   
   do k = 1,Ndx
      do itheta=1,ntheta
         if(wete(itheta, k)==1) then
            ee1(itheta, k)    = max(ee1(itheta, k),0.0d0)
            rr(itheta, k)     = max(rr(itheta, k),0.0d0)
            drr(itheta,k)     = max(-src_coeff(itheta,k)*rr(itheta,k),0.0d0)
         elseif(wete(itheta, k)==0) then
            ee1(itheta, k)    = 0.0d0
            rr(itheta, k)     = 0.0d0
            drr(itheta,k)     = 0.0d0
         end if
      end do
   end do
   
!  deallocate
   if ( allocated(src_coeff) ) deallocate(src_coeff)
   if ( allocated(src_expl)  ) deallocate(src_expl)
   
   return
end subroutine update_ee1rr
   
subroutine xbeach_waves(ierr)
   use m_flowtimes
   use m_xbeach_data
   use m_xbeach_netcdf
   use m_flowparameters
      
   implicit none
   
   integer, intent(out)   :: ierr
   
   integer          :: num, itheta
   double precision :: wave_tnow, wave_tstop
   
   if (trim(instat)=='stat' .or. trim(instat)=='stat_table') then
      if ((abs(mod(time1,wavint))<1d-6) .or. newstatbc==1) then
         !call xbeach_stationary()                                          ! Sander's nieuwe solver
         call xbeach_wave_compute_statcelerities(ierr)
         call xbeach_solve_wave_stationary(ierr)
         newstatbc   = 0
      endif

   !elseif (single_dir==1) then                                            ! to do
   !   call update_means_wave_directions()
   !  
   !   if ((abs(mod(par%t,par%wavint))<0.000001d0) .or. newstatbc==1 .or. time0==dtmaxwav) then
   !      call wave_directions()
   !      newstatbc   = 0
   !   endif
   !   s%newstatbc       = 0
   !   call wave_instationary()

   else
      newstatbc  = 0
      wave_tnow  = time0
      wave_tstop = time1
      num = 0
      do while (wave_tnow < wave_tstop)
        num = num+1
        call xbeach_instationary()   
        if (jaavgwavquant .eq. 1) then
           call xbeach_makeaverages(dts)          ! time-averaged stats
        end if
        wave_tnow = wave_tnow + dts
        !call xbeach_wave_maxtimestep()                ! get new wave timestep based on updated wave field
      end do
   endif
   
end subroutine xbeach_waves

subroutine xbeach_stationary()
   use m_sferic, only:pi, rd2dg
   use m_physcoef, only: rhog, ag
   use m_flowgeom
   use m_flow, only: s1, epshu, vol1, rhomean, epshs, plotlin, nplot
   use m_flowparameters, only:limtypw
   use m_flowtimes
   use m_flowexternalforcings, only: nbndw, zbndw, kbndw
   use m_xbeach_data
   use m_xbeach_paramsconst
   use m_partitioninfo
   use m_timer
   use m_alloc
   use m_waves, only: hwav, twav, phiwav, ustokes, vstokes, rlabda, uorb
   use unstruc_display, only: jaGUI 
   
   implicit none
   
   integer                        :: k, itheta, ierr, L, k1, k2, kb, ki, nwalls
   double precision, allocatable  :: hh(:), Dbottom(:), ddlok(:,:), wete(:,:), drr(:,:)
   double precision, allocatable  :: uwf(:), vwf(:), ustr(:), urf(:), vrf(:), ustw(:)

   double precision               :: dfac, fsqrtt
   
   double precision               :: dtmax
   double precision               :: res_loc, res
     
   integer                        :: iter   ! pseudo time-step
   integer                        :: i
   integer                        :: key=3 
   integer                        :: k_maxres
   integer                        :: itheta_maxres
   
   integer                        :: ierror, iters
   
   !double precision, parameter    :: dtol=1d0

   allocate(hh(1:ndx), Dbottom(1:ndx), ddlok(1:ntheta, 1:ndx), wete(1:ntheta, 1:ndx), drr(1:ntheta,1:ndx), stat = ierr)
   allocate(ustw(1:ndx), uwf(1:ndx), vwf(1:ndx), ustr(1:ndx), stat = ierr)
   allocate(urf(1:ndx), vrf(1:ndx), stat = ierr)
   
   hh   = 0.d0
   Dbottom   = 0.d0
   ddlok = 0.d0
   wete = 0.d0
   drr = 0.d0
   ustw = 0d0
   uwf = 0d0
   vwf = 0d0
   ustr = 0d0
   urf = 0d0
   vrf = 0d0

   BR = beta
   hh = max(s1-bl, epshs)
     
   call xbeach_wave_compute_celerities()
   call xbeach_apply_wave_bc()
   
   do iter=1, maxiter
       write(*,*) 'Iteration loop nr: ', iter
       ee0 = ee1
       !call xbeach_apply_wave_bc()
          
       if (windmodel.eq.1) then
          !ML TODO: incorporate wind source term, now only does windmodel, not jawsource
          call update_ee1rr_windmodel(dtmaximp, sigt, tt1, cgwavt, ctheta, horadvec, thetaadvec, E, H, thet, thetamean,   &
                      sigmwav, gammaxxb, hh, &
                      fw, break, deltaH, waveps, cgwav, kwav, km, gamma, gamma2, nroelvink, Qb, alpha, Trep, R, cwav, D,   &
                      roller, br, &
                      urms_cc, fwcutoff, Dbottom, DDlok, wete, rrhoradvec, rrthetaadvec, jawsource, wsorE, wsorT, egradcg, mwind, &
                      snx, csx, limtypw, &
                      ee1, rr, drr, wci, rhs, solver, nbndw, kbndw(1,:), zbndw)
          call xbeach_wave_compute_celerities()
       else
          call update_ee1rr(dtmaximp, sigt, cgwav, ctheta, horadvec, thetaadvec, E, H, thet, thetamean,   &
                         sigmwav, gammaxxb, hh, &
                         fw, break, deltaH, waveps, kwav, km, gamma, gamma2, nroelvink, Qb, alpha, Trep, R, cwav, D,   &
                         roller, br, &
                         urms_cc, fwcutoff, Dbottom, DDlok, wete, rrhoradvec, rrthetaadvec, jawsource, mwind, &
                         snx, csx, limtypw, &
                         ee1, rr, drr, wci, rhs, solver, nbndw, kbndw(1,:), zbndw, d_relaxfac)
              
       endif
       
       !if (jaGUI==1) then
       !   key=3
       !   call drawnu(key)
       !endif 
       if ( jampi.eq.1 ) then
          if ( jatimer.eq.1 ) call starttimer(IXBEACH)
          call update_ghosts(ITYPE_Sall, Ntheta, Ndx, ee1, ierr)
          call update_ghosts(ITYPE_Sall, Ntheta, Ndx, rr,  ierr)
          if ( jatimer.eq.1 ) call stoptimer(IXBEACH)
       end if 
       
!      compute residual
       res=0d0
       k_maxres = 1  ! safety
       itheta_maxres = 1
       do k=1,Ndxi
          if ( hh(k).lt.1d-2 ) cycle
          do itheta=1,ntheta
             res_loc = abs(ee1(itheta,k)-ee0(itheta,k)+1d-10)/dtmaximp
             if ( res_loc.gt.res ) then
                k_maxres = k
                itheta_maxres = itheta
                res = res_loc
             end if
          end do
       end do
              
!      stopping criterion, wave action change per second
       write(6,*) 'Implicit iteration' , iter, ', max residual: ' , res, ', at k = ', k_maxres,' .'
       nplot = k_maxres
       if ( res.lt.maxerror ) then
          exit
       end if
       
       !call qnerror(' ', ' ', ' ')
    end do
    
    if (time0 .gt. 0d0) then
       xb_started = 1
    end if
    
    ! Orbital velocity
    fsqrtt = sqrt(0.5d0) ! 1 / sqrt(2.0)
    do L=1,Lnx
       k1 = ln(1,L)
       k2 = ln(2,L)
       urms(L) = (acL(L) * urms_cc(k1) + (1d0-acl(L))*urms_cc(k2)) * fsqrtt
    end do
    
    ! Safety
    call xbeach_apply_wave_bc()    
    
    E  = min(sum(ee1,dim=1)*dtheta,1d0/8d0*rhomean*ag*gamma*gamma*hh*hh)
    R  = sum(rr,dim=1)*dtheta
    if (roller==1) then
       DR = sum(drr,dim=1)*dtheta
    else
       DR = D
       R = DR*cwav/2d0/ag/BR
    end if
    H  = sqrt(8.d0*E/rhomean/ag)
    thetamean=(sum(ee1*thet,dim=1)/dble(ntheta))/(max(sum(ee1,dim=1),0.00001d0)/dble(ntheta))

!   Wave mass flux and Stokes drift
    do k=1,ndx    
       ustw(k)= E(k)/max(cwav(k),sqrt(epshs*ag))/rhomean/max(hh(k),epshs) !waves
       ustr(k)=2d0*R(k)/max(cwav(k),sqrt(epshs*ag))/rhomean/max(hh(k),epshs) !roller
       uwf(k) = ustw(k)*dcos(thetamean(k))                    !! Cartesian decomposition
       vwf(k) = ustw(k)*dsin(thetamean(k))
       urf(k) = ustr(k)*dcos(thetamean(k))
       vrf(k) = ustr(k)*dsin(thetamean(k))
       ustx_cc(k) = uwf(k)+urf(k); usty_cc(k)=vwf(k)+vrf(k)
    end do

    do L=1,lnx                                    !! facenormal decomposition
       k1 = ln(1,L); k2 = ln(2,L)
       ust(L) = acL(L)*(csu(L)*(uwf(k1)+urf(k1))+snu(L)*(vwf(k1)+vrf(k1))) + &
          (1d0-acL(L))*(csu(L)*(uwf(k2)+urf(k2))+snu(L)*(vwf(k2)+vrf(k2)))
    
    
       vst(L) = acL(L)*(-snu(L)*(uwf(k1)+urf(k1))+csu(L)*(vwf(k1)+vrf(k1))) + &
          (1d0-acL(L))*(-snu(L)*(uwf(k2)+urf(k2))+csu(L)*(vwf(k2)+vrf(k2)))
    enddo
    
   if (roller.eq.1 .and. turb.ne.TURB_NONE) then
      call borecharacter()                   ! calculates BR and Tbore using Rieneck&Fenton approach   
   end if
   
   ! Debug
   ! En voor de uniformiteit van de golfkoppelingetjes:
   hwav = H
   twav = 2.0*pi/sigmwav
   phiwav = thetamean*rd2dg
   rlabda = L1
   uorb = urms_cc
   ustokes = ust
   vstokes = vst   
   ! \Debug

   deallocate(hh, Dbottom, ddlok, wete, drr, stat = ierr)
   deallocate(ustw, ustr, uwf, vwf, urf, vrf, stat = ierr)

   end subroutine xbeach_stationary
   
   !> compute bc for absorbing generating boundary
   subroutine xbeach_absgen_bc()
   use m_sferic
   use m_xbeach_data
   use m_flowgeom
   use m_flow
   use m_physcoef, only: ag, rhomean
   use m_flowexternalforcings
   use m_alloc
   use unstruc_messages
   use m_xbeach_errorhandling
   use m_missing
   use m_partitioninfo
   use m_flowtimes, only: dts, time1
   use m_waves, only: ustokes, vstokes
   use network_data
   use geometry_module
   
   implicit none
   
   integer :: ierror

   integer, parameter                  :: MAXLNX=100
   double precision, dimension(MAXLNX) :: wgradx, wgrady
   
   integer                             :: numbnd
   integer                             :: idum(1)
   double precision, allocatable       :: idum2(:,:)
   
   double precision :: uin_loc, vin_loc, hum, cgbound, cg0, c, umean, vmean, dum
   double precision :: factime
   double precision :: hsk
   double precision :: ht(2)
   
   double precision :: un, Fn, Fwin, Ftau, ux, uy, ur, dyy, dxx, betaki, vert, betak1, betak2 
   double precision :: dhdn, dvds, dbetads, dbetadn, dbetadt, betanp1
   double precision :: alpha2, alphanew, thetai
   
   integer :: n, Lb, L, kb, ki, k1, k2, k3, k4, i, jj
   integer :: NLNX, nw
   
   ! debug
   !integer :: lunfil
   !\debug
   
   ierror = 1
   
   if (windmodel .eq. 0) then
      factime = 1d0/cats/Trep*dts
   else
      factime = 1d0/cats/minval(sigmwav)/2d0/pi*dts       
   endif
      
!  compute boundary-averaged velocities
   numbnd = 0
   uave = 0d0
   vave = 0d0
   dlengthrm = 0d0
   
   do n=1,nbndu      
      if ( kbndu(4,n) .eq. 5 ) then
         Lb = kbndu(3,n)
         numbnd = kbndu(5,n)
         if ( numbnd.gt.maxnumbnds ) then
            maxnumbnds = max(int(1.2d0*numbnd),maxnumbnds+1)
            ! 
            if (jampi==1) then
               idum(1) = maxnumbnds
               call reduce_int_max(1,idum)
               maxnumbnds=idum(1)
            end if
            !
            call realloc(uave, maxnumbnds, keepExisting=.true., fill=0d0)
            call realloc(vave, maxnumbnds, keepExisting=.true., fill=0d0)
            call realloc(dlengthrm, maxnumbnds, keepExisting=.true., fill=0d0)
            call realloc(umeanrm, maxnumbnds, keepExisting=.true., fill=0d0)
            call realloc(vmeanrm, maxnumbnds, keepExisting=.true., fill=0d0)
         end if
         ! 
         if (jampi==0) then
            uave(numbnd) = uave(numbnd) + wu(Lb)*u1rm(n)
            vave(numbnd) = vave(numbnd) + wu(Lb)*v(Lb)     ! for now 1d
            dlengthrm(numbnd) = dlengthrm(numbnd) + wu(Lb)
         else
            if (idomain(kbndu(2,n))==my_rank) then
                uave(numbnd) = uave(numbnd) + wu(Lb)*u1rm(n)
                vave(numbnd) = vave(numbnd) + wu(Lb)*v(Lb)    
                dlengthrm(numbnd) = dlengthrm(numbnd) + wu(Lb)  
            end if
         end if
         !
      end if
   end do

   if (jampi==1) then
      if (nubnd .gt. 0) then
         allocate(idum2(3,nubnd))
         idum2(1,:) = uave
         idum2(2,:) = vave
         idum2(3,:) = dlengthrm
         call reduce_sum(3*nubnd,idum2)
         
         uave      = idum2(1,:)
         vave      = idum2(2,:)
         dlengthrm = idum2(3,:)
      end if
   end if
   
   if (nubnd .gt. 0) then       
      uave = uave/max(dlengthrm,1d-16)
      vave = vave/max(dlengthrm,1d-16)
      umeanrm = factime*uave + (1d0-factime)*umeanrm
      vmeanrm = factime*vave + (1d0-factime)*vmeanrm
   end if

   do n=1,nbndu
      if ( kbndu(4,n).eq. 5 ) then  ! absgen boundary
         kb     = kbndu(1,n)
         ki     = kbndu(2,n)
         Lb     = kbndu(3,n)
         numbnd = kbndu(5,n)

         NLNX   = nd(ki)%lnx

         nw = kbndu2kbndw(n)  ! wave-boundary index

         if ( nw.gt.0 ) then
            uin_loc = uin(nw)*csu(Lb) + vin(nw)*snu(Lb)
            vin_loc = vin(nw)*csu(Lb) - uin(nw)*snu(Lb)
         else
            uin_loc = 0d0
            vin_loc = 0d0
         end if

         !  check array size
         if ( NLNX.gt.MAXLNX ) then
            call mess(LEVEL_ERROR, 'xbeach_absgen_bc: array size error')
            call xbeach_errorhandler()
         end if

         if (trim(absgentype)=='abs_1d') then
            ! zbndu for absgen bc is slowly varying tide+surge water level
            hsk = s1(ki) - bl(ki)
            u1(Lb) = (1d0+sqrt(ag*hsk)/cgwav(ki))*uin_loc - sqrt(ag/hsk)*(s1(ki) - zbndu(n)) + umeanrm(numbnd)
            s0(kb) = s0(ki)
            s1(kb) = s1(ki)

            u1rm(n) = u1(Lb)
         endif
         
         if (trim(absgentype)=='abs_2d') then
            !
            ht(1)=zbndu(n)-bl(kb)
            ht(2)=zbndu(n)-bl(ki)
            hum  = max(eps10,0.5d0*(ht(1)+ht(2)))
            !
            umean = umeanrm(numbnd)
            vmean = vmeanrm(numbnd)
            !
            if (ARC==0) then
               u1(Lb)  = (order-1d0)*uin_loc + umean
               s1(kb)  = s1(ki)
               u1rm(n) = u1(Lb) 
               cycle              ! all done
            endif
            !
            if ( hu(Lb)<epshu ) then
               s1(Lb) = max(zbndu(n),0.5d0*(bl(ki)+bl(kb)))
               u1(Lb) = ucx(ki)*csu(Lb) + ucy(ki)*snu(Lb)
               cycle
            end if
            !
            xbducxdx   = 0d0
            xbducxdy   = 0d0
            xbducydx   = 0d0
            xbducydy   = 0d0
            dbetadx = 0d0
            dbetady = 0d0            
            !
            do i=1,NLNX
               L = iabs(nd(ki)%ln(i))
               !
               k1 = ln(1,L)
               k2 = ln(2,L)
               !
               ! Pragmatic way
               betak1 = ucx(k1)*csu(Lb) + ucy(k1)*snu(Lb) - 2d0*sqrt(ag*(s1(k1)-bl(k1)))
               betak2 = ucx(k2)*csu(Lb) + ucy(k2)*snu(Lb) - 2d0*sqrt(ag*(s1(k2)-bl(k2)))
               !
               dbetadx(k1) = dbetadx(k1) + wcx1(L)*(betak2-betak1)*dxi(L) 
               dbetadx(k2) = dbetadx(k2) + wcx2(L)*(betak2-betak1)*dxi(L)
               dbetady(k1) = dbetady(k1) + wcy1(L)*(betak2-betak1)*dxi(L)
               dbetady(k2) = dbetady(k2) + wcy2(L)*(betak2-betak1)*dxi(L)
               !
               xbducxdx(k1)   = xbducxdx(k1) + wcx1(L)*(ucx(k2)-ucx(k1))*dxi(L)
               xbducxdx(k2)   = xbducxdx(k2) + wcx2(L)*(ucx(k2)-ucx(k1))*dxi(L)
               xbducxdy(k1)   = xbducxdy(k1) + wcy1(L)*(ucx(k2)-ucx(k1))*dxi(L)
               xbducxdy(k2)   = xbducxdy(k2) + wcy2(L)*(ucx(k2)-ucx(k1))*dxi(L) 
               !
               xbducydx(k1)   = xbducydx(k1) + wcx1(L)*(ucy(k2)-ucy(k1))*dxi(L)
               xbducydx(k2)   = xbducydx(k2) + wcx2(L)*(ucy(k2)-ucy(k1))*dxi(L)
               xbducydy(k1)   = xbducydy(k1) + wcy1(L)*(ucy(k2)-ucy(k1))*dxi(L)
               xbducydy(k2)   = xbducydy(k2) + wcy2(L)*(ucy(k2)-ucy(k1))*dxi(L)  
            end do
            
            dbetadn =  csu(Lb) * dbetadx(ki) + snu(Lb) * dbetady(ki) ! 1-2 direction
            dbetads = -snu(Lb) * dbetadx(ki) + csu(Lb) * dbetady(ki) ! 3-4 direction
            !
            dvds    = -snu(Lb) * (-snu(Lb) * xbducxdx(ki)   + csu(Lb) * xbducxdy(ki)) +  &
               csu(Lb) * (-snu(Lb) * xbducydx(ki)   + csu(Lb) * xbducydy(ki))
            !
            dhdn    = ( ht(2)-ht(1) ) * dxi(Lb)   ! 1-2 direction (inward positive)
            !
            Fn      = csu(Lb) * Fx(Lb) + snu(Lb) * Fy(Lb)   ! 1-2 direction (inward positive)
            !  compute bed friction
            Ftau    =  cfuhi(Lb) * sqrt((u1(Lb)-ustokes(Lb))**2+(v(Lb)-vstokes(Lb))**2) * ( u1(Lb)-ustokes(Lb) )  ! formally, not exactly correct, since we also need u1L (see furu). JRE: but we can use u1rm old timelevel
            !
            if (jawind>0) then
               Fwin = wdsu(Lb)*huvli(Lb)
            else
               Fwin = 0d0
            endif   
            c = sqrt(ag*hu(Lb))
            !
            !
            dbetadt = - (u1(Lb)-c)*dbetadn - v(Lb)*dbetads + c*dvds + ag*dhdn + Fn/(rhomean*hu(Lb)) - Ftau + Fwin
            beta = u1(Lb) - 2d0*sqrt(ag*hu(Lb))
            !
            thetai = atan2(vin_loc, uin_loc)   ! cartesian angle wrt X
            !
            betanp1   = beta + dbetadt*dts
            alpha2    = (270d0-dir0)*dg2rd     ! first guess, theta0 not set for spectral bc as dir0 not defined 
            alphanew  = 0.d0
            !
            cg0 = dsqrt(ag*hum)
            !
            do jj=1,50
               !
               if (freewave==1) then    ! assuming incoming long wave propagates at sqrt(g*h) (free wave)
                  ur = cos(alpha2)/(cos(alpha2)+1.d0)  &
                     *(betanp1-umean+2.d0*cg0 &
                     -uin_loc*(cos(thetai)-1.d0)/cos(thetai))
               else                     ! assuming incoming long wave propagates at group velocity (bound wave)
                  cgbound = max(0.5d0*(cgwav(kb)+cgwav(ki)),eps10)
                  dum = uin_loc*(cgbound*cos(thetai)-cg0)/ (cgbound*cos(thetai))
                  ur = cos(alpha2)/(cos(alpha2)+1.d0)  &
                     *(betanp1-umean+2.d0*cg0 - dum)
               endif
               !
               vert = v(Lb) - vmean - vin_loc ! tangential component along cell face
               !
               alphanew = atan2(vert,(ur+1.d-16))
               if (alphanew .gt. (pi*0.5d0)) alphanew=alphanew-pi
               if (alphanew .le. (-pi*0.5d0)) alphanew=alphanew+pi
               !
               if (abs(alphanew - alpha2) < 1d-3) then
                  exit
               endif      
               alpha2 = alphanew
            end do    
            !
            u1(Lb)   = (order-1.d0)*uin_loc + ur + umean
            u1rm(n) = u1(Lb)
            !
            ! try from cell centre, uses value at old time level anyhow
            betaki   = ucx(ki)*csu(Lb) + ucy(ki)*snu(Lb) - 2d0*sqrt(ag*(s1(ki)-bl(ki)))
            un       = ucx(ki)*csu(Lb) + ucy(ki)*snu(Lb)
            s1(kb)   = 1.5d0*((betanp1-u1rm(n))**2/4.d0/ag+.5d0*(bl(kb)+bl(ki)))- &
                       0.5d0*((betaki-un)**2/4.d0/ag+bl(ki))
         endif
         
      end if   ! riemannpuntje
   end do ! loop snelheidslinks
   
   ierror = 0
   
1234 continue
   return
end subroutine xbeach_absgen_bc
   
subroutine rollerturbulence(k)
   use m_xbeach_data
   use m_xbeach_paramsconst
   use m_waves
   use m_physcoef
   use m_sferic
   use m_flow
   use m_flowgeom
   use m_flowparameters
   
   implicit none
   
   integer, intent(in)       :: k
   
   double precision          :: disrol, rol, Tw, Tb, cw, ktrb, hloc
   double precision          :: dcf, dcfin, ML, twothird
   
   if (jawave==3 .or. jawave==6) then
      disrol = dsurf(k)
      cw     = rlabda(k)/max(1d-1,twav(k))
      rol    = disrol*cw/2d0/ag/0.10d0          ! assume something for roller slope
      Tw     = twav(k)
      Tb     = twav(k)
   end if
   
   if (jawave .eq. 4) then
      disrol = DR(k)
      rol    = R(k)
      cw     = cwav(k)
      Tw     = 2*pi/sigmwav(k)
      if (turb==TURB_BORE_AVERAGED) then
         Tb     = Tbore(k)
      else 
!         Tb = Trep
         Tb = 2.d0 * pi / sigmwav(k)
      end if
   end if
   
   twothird = 2d0/3d0
   ktrb = (disrol/rhomean)**twothird           ! See Battjes, 1975 / 1985

   hloc = max(s1(k)-bl(k),1d-2)
   ! compute mixing length
   ML = dsqrt(2*rol*Tw/(rhomean*cw)) 
   ML = min(ML, hloc);
   ! exponential decay turbulence over depth
   dcfin = exp(min(100.d0,hloc/max(ML,1d-2)))
   dcf = min(1.d0,1.d0/(dcfin-1.d0))
   !
   ktb(k) = ktrb*dcf*Tw/max(1d-1,Tb)

end subroutine rollerturbulence 
   
subroutine borecharacter()
   use m_xbeach_data
   use m_flow, only: s1, epshs
   use m_flowgeom, only: ndx, bl
   use m_physcoef
   use m_sferic, only:pi
   
   implicit none
    
   integer                          :: nh, nt, k, ierr
   integer                          :: ih0, it0, ih1, it1
   double precision                 :: p, q
   double precision                 :: f0, f1, f2, f3
   double precision                 :: t0fac
   double precision                 :: duddtmax, dudtmax, detadxmean, siguref, detadxmax, duddtmean, dudtmean
   double precision                 :: dh, dt
   double precision, allocatable    :: h0(:), t0(:), hh(:)
   
   include 'RF.inc'
   
   if (.not. allocated(h0)) then
      allocate(h0(1:ndx), stat=ierr)
      allocate(t0(1:ndx), stat=ierr)
      allocate(hh(1:ndx), stat=ierr)
   end if
   
   dh = 0.03d0
   dt = 1.25d0
   nh = floor(0.99d0/dh);
   nt = floor(50.d0/dt);
   hh = max(s1-bl,epshs)
   
   ! compute dimensionless wave height and wave period in each grid point..
      h0 = min(nh*dh,max(dh,     min(H,hh)/max(hh,epshs)))
!      t0 = min(nt*dt,max(dt,Trep*sqrt(ag/max(hs, epshs))))
      t0 = min(nt*dt,max(dt,2d0*pi/sigmwav*sqrt(ag/max(hh, epshs))))        
      do k=1,ndx
         if (hh(k).lt.epshs) then      ! some sensible defaults
!            Tbore(k)=Trep
            Tbore(k)=2.d0 * pi / sigmwav(k)
            BR(k) = beta
            cycle
         end if
         ih0=floor(h0(k)/dh);
         it0=floor(t0(k)/dt);
         ih1=min(ih0+1,nh);
         it1=min(it0+1,nt);
         p=(h0(k)-ih0*dh)/dh;
         q=(T0(k)-it0*dt)/dt;
         
         f0=(1-p)*(1-q);
         f1=p*(1-q);
         f2=q*(1-p);
         f3=p*q;
                  
         if (t0(k)==50.d0) then
!            t0fac = 50.d0/max((Trep*sqrt(ag/max(hs(k),epshs))),50.d0)
            t0fac = 50.d0/max((2.d0 * pi / sigmwav(k) *sqrt(ag/max(hh(k),epshs))),50.d0)            
         elseif (t0(k)==1.25)then
!            t0fac = 1.25d0/min((Trep*sqrt(ag/max(hs(k),epshs))),1.25d0)
            t0fac = 1.25d0/min((2.d0 * pi /sigmwav(k) *sqrt(ag/max(hh(k),epshs))),1.25d0)
         else
            t0fac = 1.d0
         endif
         !
         duddtmax = f0*RF(3,ih0,it0)+f1*RF(3,ih1,it0)+ f2*RF(3,ih0,it1)+f3*RF(3,ih1,it1)
         siguref = f0*RF(4,ih0,it0)+f1*RF(4,ih1,it0)+ f2*RF(4,ih0,it1)+f3*RF(4,ih1,it1)
         !
         dudtmax = urms_cc(k)/sqrt(2.0) / max(waveps,siguref)* sqrt(ag/max(hh(k), epshs)) * t0fac * duddtmax    ! urms_cc is uorb, not urms
         detadxmax = dudtmax*sinh(min(kwav(k)*hh(k),10d0))/max(max(cwav(k),sqrt(H(k)*ag)),1d-10)/sigmwav(k)
         !
         if (rfb==1) then
            duddtmean = f0*RF(5,ih0,it0)+f1*RF(5,ih1,it0)+ f2*RF(5,ih0,it1)+f3*RF(5,ih1,it1)
            dudtmean = urms_cc(k)/sqrt(2.0) / max(waveps,siguref) * sqrt(ag/max(hh(k), epshs))*t0fac*duddtmean
            detadxmean = dudtmean*sinh(min(kwav(k)*hh(k),10d0))/max(max(cwav(k),sqrt(H(k)*ag)),1d-10)/sigmwav(k)
            BR(k) = BRfac*sin(atan(detadxmean))
         endif
      enddo

!      Tbore = Tbfac*max(Trep/25.d0,min(Trep/4.d0,H/(max(max(cwav,sqrt(H*ag)),1d-10)*max(detadxmax,waveps))))
      Tbore = Tbfac*max(2.d0 * pi / sigmwav /25.d0,min(2.d0 * pi / sigmwav /4.d0,H/(max(max(cwav,sqrt(H*ag)),1d-10)*max(detadxmax,waveps))))  
      deallocate(h0, t0, stat=ierr)

   end subroutine borecharacter
   
   subroutine xbeach_absgen_maxtimestep()
       use m_flowexternalforcings
       use m_flow
       use m_flowtimes, only: dts, dti
       use m_flowgeom
       use m_partitioninfo
       use m_timer
       
       implicit none
       
       integer                                   :: nb, k1, k2, ierr
       double precision                          :: ql, dtsc
       double precision, allocatable             :: sql(:)
       
       if (nbndu==0) return          ! nothing to do here, fly away...
       
       ierr = 1
       
       if (.not. allocated(sql)) allocate(sql(1:ndx), stat = ierr)
       sql = 0d0
       do nb = 1, nbndu
          if (kbndu(4,nb)==5) then   ! absgen linkert
             k1 = ln(1,nb); k2 = ln(2,nb)   

             ql = 2d0*sqrt(hu(nb)*ag)*Au(nb)
             !ql = 1.5d0*sqrt(hu(nb)*ag)*Au(nb)
             !sql(k1) =  sql(k1) + max(q1(nb)+ql,0d0)
             sql(k2) =  sql(k2) - min(q1(nb)-ql,0d0)        ! met Sander, 5/4/2017
          end if
       end do
       
       do nb = 1, nbndu
          if (kbndu(4,nb)==5) then   ! absgen linkert
             k1 = ln(2,nb)   
             if (sql(k1) > eps10) then                   ! outflow only
                if (s1(k1)-bl(k1) > epshu) then
                   dtsc = cflmx*vol1(k1)/sql(k1)
                   if (dtsc < dts) then
                      dts = dtsc
                   endif
                endif
             endif
          end if
       end do
       
       if ( jampi.eq.1) then
          if ( jatimer.eq.1 ) call starttimer(IMPIREDUCE)
          call reduce_double_min(dts)
          if ( jatimer.eq.1 ) call stoptimer(IMPIREDUCE)
       end if
       
       dti = 1d0/dts
       
       ierr = 0
   
   end subroutine xbeach_absgen_maxtimestep
   
   subroutine xbeach_fillsystem_windmodel(solver,NDIM,quant,src_coeff,src_expl,veloc,csx,snx,ci,delta,dt,nbnd,kbnd,zbnd,jawritesystem,ierror)
   use m_flowgeom
   use m_solver
   use m_flow, only: hu, epshu, hs, epshs
   use m_missing
   implicit none
   
   type(tsolver),                          intent(inout) :: solver !< solver
   integer,                                intent(in)    :: NDIM   !< number of unknows per flow node
   double precision, dimension(NDIM,Ndx),  intent(inout) :: quant  !< quantity
   double precision, dimension(NDIM,Ndx),  intent(in)    :: src_coeff !< coefficient of sources
   double precision, dimension(NDIM,Ndx),  intent(in)    :: src_expl  !< explicit sources
   double precision, dimension(NDIM,Ndx),  intent(in)    :: veloc  !< magnitude of velocity in (x,y)-dir
   double precision, dimension(NDIM),      intent(in)    :: csx    !< advection direction
   double precision, dimension(NDIM),      intent(in)    :: snx    !< advection direction
   double precision, dimension(NDIM,Ndx),  intent(in)    :: ci     !< velocity in i-direction
                                           
                                           
   double precision,                       intent(in)    :: delta  !< mesh width in i-direction
                                           
   double precision,                       intent(in)    :: dt     !< time step
                                           
   integer,                                intent(in)    :: nbnd   !< number of Dirichlet boundary conditions
   integer,          dimension(nbnd),      intent(in)    :: kbnd   !< Dirichlet boundary condition cell numbers
   double precision, dimension(NDIM,nbnd), intent(in)    :: zbnd   !< boundary values
   integer,                                intent(in)    :: jawritesystem  !< write system for debug (1) or not (0)
   
   integer,                                intent(inout) :: ierror !< error (1) or not (0)
                                           
   double precision, dimension(:,:),       allocatable   :: dfluxfac
   double precision, dimension(:,:),       allocatable   :: bndval
                                           
   double precision                                      :: dti, dti_loc
   double precision                                      :: cwuL, ct
   double precision                                      :: rowsum
   double precision                                      :: velocL
   double precision                                      :: cs, sn, wuL
                                                       
   integer                                               :: ipointdiag
   integer                                               :: i, j, n
   integer                                               :: kk, kkother
   integer                                               :: k1, k2
   integer                                               :: L, LL, irow, icol, ipoint, lunfil
                                           
   double precision,                       parameter     :: dtol = 1d-10                                
   
   ierror = 1
   
!  allocate
   allocate(dfluxfac(2,NDIM*Lnx))
   if ( Ndx.gt.Ndxi ) then
      allocate(bndval(NDIM,Ndx-Ndxi))
      bndval=DMISS
   end if
   
   dti = 1d0/dt
   
!  initialize
   solver%a = 0d0
   solver%rhs = 0d0
   dfluxfac = 0d0
   
!  compute fluxes
   do LL=1,Lnx
!      if ( hu(LL).gt.epshu ) then
         k1 = ln(1,LL)
         k2 = ln(2,LL)
         
         do i=1,NDIM
            velocL = acL(LL)*veloc(i,k1) + (1d0-acL(LL))*veloc(i,k2)
            cwuL = velocL*(csu(LL)*csx(i) + snu(LL)*snx(i))*wu(LL)
            
            L = (LL-1)*NDIM+i
            dfluxfac(1,L) = max(cwuL,0d0)   
            dfluxfac(2,L) = min(cwuL,0d0)

         end do
!      end if
   end do
   
!  fill matrix entries   
   irow = 0
   ipoint = 0
   do kk=1,Ndxi   ! internal
      do i=1,NDIM
         irow = irow+1
         
         ipoint = ipoint+1
         ipointdiag = ipoint
         do j=1,nd(kk)%lnx
            LL = iabs(nd(kk)%ln(j))
            
            ipoint = ipoint+1
            
!           check row number
            kkother = ln(1,LL) + ln(2,LL) - kk
            if ( solver%ja(ipoint).ne.(kkother-1)*NDIM+i ) then
!               call qnerror(' ', ' ', ' ')
               ierror = 1
               goto 1234
            end if
            
            L = (LL-1)*NDIM+i
 
            if ( ln(1,LL).eq.kk ) then
               solver%a(ipointdiag) = solver%a(ipointdiag) + dfluxfac(1,L)/ba(kk)
               solver%a(ipoint)     = solver%a(ipoint)     + dfluxfac(2,L)/ba(kk)
            else if ( ln(2,LL).eq.kk ) then
               solver%a(ipointdiag) = solver%a(ipointdiag) - dfluxfac(2,L)/ba(kk)
               solver%a(ipoint)     = solver%a(ipoint)     - dfluxfac(1,L)/ba(kk)
            else
               ierror=1
               goto 1234
            end if
         end do
         
!        i-dir, i-1
         if ( i.gt.1 ) then
            ct = 0.5d0*(ci(i-1,kk)+ci(i,kk))
         
            ipoint=ipoint+1
            solver%a(ipoint) = solver%a(ipoint) - max(ct,0d0)/delta
            solver%a(ipointdiag) = solver%a(ipointdiag) - min(ct,0d0)/delta
            !solver%a(ipoint) = solver%a(ipoint) - min(ct,0d0)/delta
            !solver%a(ipointdiag) = solver%a(ipointdiag) - max(ct,0d0)/delta
         else
!            ct = ci(i,kk)
!         
!            ipoint=ipoint+1
!            solver%a(ipointdiag) = solver%a(ipointdiag) - min(ct,0d0)/delta
         end if
         
!        i-dir, i+1
         if ( i.lt.NDIM ) then
            ct = 0.5d0*(ci(i,kk)+ci(i+1,kk))
         
            ipoint=ipoint+1
            solver%a(ipoint) = solver%a(ipoint) + min(ct,0d0)/delta
            solver%a(ipointdiag) = solver%a(ipointdiag) + max(ct,0d0)/delta 
            !solver%a(ipoint) = solver%a(ipoint) + max(ct,0d0)/delta
            !solver%a(ipointdiag) = solver%a(ipointdiag) + min(ct,0d0)/delta
         else
!            ct = ci(i,kk)
!         
!            solver%a(ipointdiag) = solver%a(ipointdiag) + max(ct,0d0)/delta
         end if
      end do
   end do
   
!  unspecified boundary conditions: homogeneous Dirichlet
   do LL=Lnxi+1,Lnx
      kk = ln(1,LL)
      do i=1,NDIM
         irow = (kk-1)*NDIM+i
         ipoint = solver%ia(irow)
         
         L = (LL-1)*NDIM+i
!         if ( dfluxfac(1,L).gt.0d0 ) then  ! inflow
!            solver%a(ipoint)   =  1d0  ! diagonal entry
!!           off-diagonal entries
!            do ipoint=solver%ia(irow)+1,solver%ia(irow+1)-1
!               solver%a(ipoint) = 0d0
!            end do
!            solver%rhs(irow)   =  0d0
!         else
            solver%a(ipoint)   =  1d0  ! diagonal entry
            solver%a(ipoint+1) = -1d0  ! off-diagonal entry
            solver%rhs(irow)   =  0d0
!         end if
      end do
   end do
   
!  Dirichlet boundary conditions 
   do n=1,nbnd
      kk = kbnd(n)
      do i=1,NDIM
         irow = (kk-1)*NDIM + i
!        diagonal entry
         ipoint = solver%ia(irow)
         solver%a(ipoint) = 1d0
!        off-diagonal entries
         do ipoint=solver%ia(irow)+1,solver%ia(irow+1)-1
            solver%a(ipoint) = 0d0
         end do
         solver%rhs(irow) = zbnd(i,n)
      end do
   end do
   
!  closed boundaries
   do n=1,mxwalls
      kk = walls(1,n)
   
      cs =  walls(8,n) ! outward positive
      sn = -walls(7,n)
      wuL = walls(9,n)
      
      do i=1,NDIM
         cwuL   = veloc(i,kk)*( cs*csx(i) + sn*snx(i) ) * wuL
         
         if ( cwuL.gt.0d0 ) then
!           get row number
            irow = (kk-1)*NDIM + i
!           get location of diagonal entry
            ipoint = solver%ia(irow)
!           add boundary flux            
            solver%a(ipoint) = solver%a(ipoint) + cwuL/ba(kk)
         end if
      end do
   end do

!    do n=1,mxwalls
!         kk = walls(1,n)
!         do i=1,NDIM
!            irow = (kk-1)*NDIM + i
!   !        diagonal entry
!            ipoint = solver%ia(irow)
!            solver%a(ipoint) = 1d0
!   !        off-diagonal entries
!            do ipoint=solver%ia(irow)+1,solver%ia(irow+1)-1
!               solver%a(ipoint) = 0d0
!            end do
!            solver%rhs(irow) = 0d0
!         end do
!    end do

   
! thin dams
   do n=1,nthd
      kk = thindam(1,n)
   
      cs =  thindam(5,n) ! outward positive
      sn = -thindam(4,n)
      wuL = thindam(6,n)
      
      do i=1,NDIM
         cwuL   = veloc(i,kk)*( cs*csx(i) + sn*snx(i) ) * wuL
         
         if ( cwuL.gt.0d0 ) then
!           get row number
            irow = (kk-1)*NDIM + i
!           get location of diagonal entry
            ipoint = solver%ia(irow)
!           add boundary flux            
            solver%a(ipoint) = solver%a(ipoint) + cwuL/ba(kk)
         end if
      end do
   end do
   
!  add time derivative and sources
   do kk=1,Ndxi
      do i=1,NDIM
         irow = (kk-1)*NDIM + i
         ipoint = solver%ia(irow)
         solver%a(ipoint) = solver%a(ipoint) + dti - src_coeff(i,kk)
         solver%rhs(irow) = solver%rhs(irow) + dti*quant(i,kk) + src_expl(i,kk)
      end do
   end do
   
!  check diagonal entries   
   do irow=1,Ndxi*NDIM
      ipoint = solver%ia(irow)
      if ( abs(solver%a(ipoint)).lt.dtol ) then
         solver%a(ipoint) = 1d0
         do i=ipoint+1,solver%ia(irow+1)-1
            solver%a(i) = 0d0
         end do
         solver%rhs(irow) = 0d0
      end if
   end do
   
   if ( jawritesystem.eq.1 ) then
!     write matrix
      open(newunit=lunfil,file='system.m')
      write(lunfil,"('dum = [')")
      do irow=1,solver%numrows
         do j=solver%ia(irow),solver%ia(irow+1)-1
            icol = solver%ja(j)
            write(lunfil,"(2I7,E15.5)") irow, icol, solver%a(j)
         end do
      end do
      write(lunfil,"('];')")
      write(lunfil,"('A=sparse(dum(:,1), dum(:,2), dum(:,3));')")
      
!     write rhs
      write(lunfil,"('rhs = [')")
      do irow=1,solver%numrows
         write(lunfil,"(E15.5)") solver%rhs(irow)
      end do
      write(lunfil,"('];')")
      
      close(lunfil)
   end if
   
   ierror = 0
1234 continue   

!  deallocate
   if ( allocated(dfluxfac) ) deallocate(dfluxfac)
   if ( allocated(bndval)   ) deallocate(bndval)
   
   return
   end subroutine xbeach_fillsystem_windmodel

      subroutine xbeach_map_wind_field(wx, wy, mwind, wmagcc, windspreadfac)
   use m_flowgeom, only: ln, wcl, lnx, ndx, thetabin, ntheta, dtheta

   implicit none
                                                 
   double precision, dimension(lnx)        , intent(in) :: wx
   double precision, dimension(lnx)        , intent(in) :: wy
   double precision                        , intent(in) :: mwind

   double precision, dimension(ndx)        , intent(inout):: wmagcc
   double precision, dimension(ntheta,ndx) , intent(inout):: windspreadfac
   
   integer                                          :: ierr, L, k1, k2, itheta, k
   double precision, dimension(:), allocatable      :: wxcc            !  [m/s] x-component windspeed cell centered
   double precision, dimension(:), allocatable      :: wycc            !  [m/s] y-component windspeed cell centered
   double precision, dimension(:), allocatable      :: wdir            !  [rad] wind speed direction cell centered
   double precision, dimension(:,:), allocatable    :: dist2           !< temp array for windspreadfac
   double precision, dimension(:), allocatable      :: dist0           !< temp array for windspreadfac

   ierr = 1
 
   if (.not.allocated(dist2))   allocate(dist2(1:ntheta , 1:ndx),  stat = ierr)  
   if (.not.allocated(dist0)) allocate(dist0(1:ntheta), stat = ierr)
   if (.not.allocated(wxcc)) allocate(wxcc(1:ndx), stat = ierr)
   if (.not.allocated(wycc)) allocate(wycc(1:ndx), stat = ierr)
   if (.not.allocated(wdir))    allocate(wdir(1:ndx),              stat = ierr)   
   
   wxcc=0d0
   wycc=0d0
   wdir=0d0
   wmagcc=0d0
   dist2=0d0
   dist0=0d0   
   windspreadfac=0d0
   
   do L = 1, lnx                                                              ! interpolate face values to cell centered values 
      k1 = ln(1,L); k2 = ln(2,L)
      wxcc(k1) = wxcc(k1) + wcl(1,L)*wx(L)
      wxcc(k2) = wxcc(k2) + wcl(2,L)*wx(L)
      wycc(k1) = wycc(k1) + wcl(1,L)*wy(L)
      wycc(k2) = wycc(k2) + wcl(2,L)*wy(L)
   end do
   
   wdir = atan2(wycc, wxcc)                               ! assume  cartesian CCW
   !etilde = rhomean*ag*3.6d-3
   do k = 1, ndx
      do itheta = 1,ntheta
       dist2(itheta, k)=(cos(thetabin(itheta)-wdir(k)))**mwind   
         if(cos(thetabin(itheta)-wdir(k))<0.d0) then
          dist2(itheta,k)=0.0d0
         end if
      end do
    if (sum(dist2(:,k))>0.d0) then
       dist0 = dist2(:,k)
       windspreadfac(:,k) = (dist0/sum(dist0))/dtheta
    else
       windspreadfac(:,k)=0.d0
    endif            
   end do       
   
   do k = 1, ndx
      wmagcc(k)=sqrt(wxcc(k) * wxcc(k) + wycc(k) * wycc(k)) 
   end do
   
   ierr = 0
   
1234 continue
   deallocate(dist2,dist0, stat=ierr)  
   deallocate(wxcc,wycc,wdir, stat=ierr)       
   return
   
  end subroutine xbeach_map_wind_field    
    
  subroutine xbeach_windsource(ee1, E, tt1, sigmwav , cgwavt, cgwav, hh, dtmaxwav, wsorE, wsorT,egradcg,SwE ,SwT )
   use m_flowgeom, only: ndx, ndxi, lnx, wcl, ln, thetabin,ntheta, dtheta, bai
   use m_xbeach_data, only: mwind, Eini, Trepini, snx, csx, wmagcc, windspreadfac, Eful, Tful,CE1, CT1, CE2, CT2, jagradcg
   use m_physcoef, only: rhomean, ag, rhog
   use m_sferic, only: twopi, pi
!   use m_growth_curves

   implicit none
                                                                             
   double precision, dimension(ntheta, ndx), intent(in) :: ee1              !<   wave energy/rad 
   double precision, dimension(ndx)        , intent(in) :: E                !<   nodal wave energy
   double precision, dimension(ntheta, ndx), intent(in) :: tt1              !<   wave period in directional bin
   double precision, dimension(ndx)        , intent(in) :: sigmwav             !<   nodal wave period
   double precision, dimension(ntheta, ndx), intent(in) :: cgwavt           !<   group celerity per bin
   double precision, dimension(ndx)        , intent(in) :: cgwav            !<   nodal group celerity
   double precision, dimension(ndx)        , intent(in) :: hh               !<   water depth
   double precision,                         intent(in) :: dtmaxwav         !<   time step

   double precision, dimension(ntheta, ndx), intent(out):: wsorT            !<   wind input period per second
   double precision, dimension(ntheta, ndx), intent(out):: wsorE            !<   wind input energy per second
   double precision, dimension(ntheta, ndx), intent(out):: egradcg            !<   wind input energy per second
   double precision, dimension(ndx),         intent(out):: SwE              !<   nodal wind input energy per second
   double precision, dimension(ndx),         intent(out):: SwT              !<   nodal wind input period per second
   
   integer                                          :: ierr
   integer                                          :: itheta, k, k1, k2, L
  
   double precision                                 :: Edmlss, Tdmlss, cgdmlss, Ddmlss, wsorTdlss, wsorEdlss, dtdmlss
   
   double precision                                 :: dir0
   double precision                                 :: fE, fT, dE, dT, dEful, dTful 
   double precision,  allocatable                   :: gradcg(:,:)
   double precision                                 :: tgradcg
         

   ierr = 1
    
   allocate( gradcg( 1:ntheta, 1:ndx), stat = ierr)
   fE=0d0; fT=0d0; dE=0d0; dT=0d0;
   wsorE=0d0; wsorT=0d0;
   gradcg=0d0; tgradcg=0d0; gradcg=0d0; 
      
  
   ! velocity gradient operator       
   call advec_horz_cg(dtmaxwav, snx, csx, cgwavt, gradcg)  
       
   do k = 1, ndxi
        
        dEful = (Tful / (4.0d0 * pi)) / (CE1 * Eful ** CE2) !d
        
        do itheta = 1, ntheta     
            
           !compute dimensionless wave state
           !Edmlss  =   ag / rhomean / wmagcc(k)**4 * E(k)    
           !Tdmlss  =   ag / wmagcc(k) * tt1(itheta,k)
           !cgdmlss =   cgwavt(itheta ,k) / wmagcc(k)  
           
!          if decoupled growth in each wave bin replace with:
           Edmlss=ag / rhomean / wmagcc(k)**4 * ee1(itheta,k) / windspreadfac(itheta,k)   
           Tdmlss= ag / wmagcc(k) * tt1(itheta,k)
           cgdmlss= cgwavt(itheta,k) / wmagcc(k)  
           
           
           ! dimensionless magnitude of source terms        
           fE =        CE1* Edmlss**CE2
           dE =        cgdmlss/ fE
           
           fT =        CT1* Tdmlss**CT2
           dT =        cgdmlss/ fT 
                                     
           wsorEdlss = min(dE , dEful)  
           wsorTdlss = dT !max(dT,dTful) !windspreadfac(itheta,k)  * dT * dtheta !max(dT , dTful) 
           
           SwE(k)= max(wmagcc(k)**3 * rhomean * wsorEdlss, 0.d0) !
           SwT(k)= max(wsorTdlss , 0.d0) !         
        
           !distribute growth over the wave bins, add gradcg component and make dimensional
           
            if (jagradcg .eq. 1) then 
              ! egradcg = max(-windspreadfac(itheta,k) * ee0(itheta,k) / windspreadfac(itheta,k) * bai(k) * gradcg(itheta,k) , 0d0) ! * Etaper  perhaps use gradcg(nodal)?                   
              egradcg(itheta,k) = max(- ee1(itheta, k) * bai(k) * gradcg(itheta,k) , 0.d0)! 
              !tgradcg = max(-windspreadfac(itheta,k) * twopi / sigmwav(k) * bai(k) * gradcg(itheta,k)  , 0d0) 
            else
                egradcg(itheta,k) = 0.d0
                !tgardcg = 0.d0
           endif
           
           wsorE(itheta,k) = max(windspreadfac(itheta,k) * SwE(k)   + egradcg(itheta,k), 0.d0 )
           wsorT(itheta,k) = max(windspreadfac(itheta,k) * dtheta * SwT(k) , 0.d0 ) 
 
        enddo
    
    end do           
     
   ierr = 0
   
1234 continue
    deallocate(gradcg, stat = ierr )
     
   return
end subroutine xbeach_windsource
   
subroutine advec_horz_cg(dtmaxwav, snx, csx, veloc, gradcg)
   use m_sferic
   use m_physcoef
   use m_flowgeom
   use m_flowparameters, only:eps10
   
   implicit none
   
   integer                                                  :: L, k, k1, k2, itheta, ku, kl2s, kl2, kl1, kd, is, ip
   double precision                                         :: velocL, qds, qst, half, fluxvel1, waku, sl1, sl2, sl3
   double precision                                         :: cf, ds2, ds1, ds, cwuL
   double precision, intent(in)                             :: dtmaxwav
   double precision, intent(in), dimension(ntheta)          :: snx, csx
   double precision, intent(in), dimension(ntheta, ndx)     :: veloc
   double precision, intent(out), dimension(ntheta, ndx)    :: gradcg
   double precision, external                               :: dslim
   
   double precision                                         :: cs, sn, wuL
   
   integer                                                  :: nwalls
   
   gradcg = 0d0
   velocL = 0d0
   cwuL   = 0d0
   
   do L  = 1,lnx                                                              ! upwind (supq) + limited high order (dsq), loop over link
        k1  = ln(1,L) ; k2 = ln(2,L)                                          ! linker en rechtercelnr geassocieerd aan de links

        do itheta = 1,ntheta

            velocL = acL(L)*veloc(itheta,k1) + (1d0-acL(L))*veloc(itheta,k2)                       
          
            cwuL    = velocL * wu(L) * ( csu(L)*csx(itheta) + snu(L)*snx(itheta) )   ! *au(L)   met cwi: u1(L) + cg*( csu(L)*csx(itheta) + snu(L)*snx(itheta) )
                                                                                     ! inproduct cgx*csu+cgy*snu                                                                                    
            gradcg(itheta,k1) = gradcg(itheta,k1) - cwul                       ! left cell outward facing normal
            gradcg(itheta,k2) = gradcg(itheta,k2)  + cwul                       ! right cell inward facing normal

        enddo ! directions
    enddo ! links
    
   
!  account for outflow at closed boundaries   
   do nwalls=1,mxwalls
     k1 = walls(1,nwalls)
     
     if (k1==7420) then
        continue
     end if

     cs =  walls(8,nwalls) ! outward positive
     sn = -walls(7,nwalls)
     wuL = walls(9,nwalls)
     
     do itheta = 1,ntheta
         cwuL    = veloc(itheta,k1) * wuL* ( cs*csx(itheta) + sn*snx(itheta) )   ! *au(L)   met cwi: u1(L) + cg*( csu(L)*csx(itheta) + snu(L)*snx(itheta) )
         
         gradcg(itheta,k1) = gradcg(itheta,k1) - cwuL ! minus because of outward positive, like Left adjacent cell
      end do
   end do
  
! account for thin dams
   do nwalls=1,nthd
     k1 = thindam(1,nwalls)
     
     cs = thindam(5,nwalls)  ! outward facing positive? Check with JRE
     sn = -thindam(4,nwalls)
     wuL = thindam(6,nwalls)
     
     do itheta = 1,ntheta
         cwuL    = veloc(itheta,k1)* wuL * ( cs*csx(itheta) + sn*snx(itheta) )   ! *au(L)   met cwi: u1(L) + cg*( csu(L)*csx(itheta) + snu(L)*snx(itheta) )
         
         gradcg(itheta,k1) = gradcg(itheta,k1) - cwuL
     end do
   end do

end subroutine advec_horz_cg    

subroutine xbeach_wave_period_breaker_dissipation( Df, E, sigmwav, cgwav, kwav, DtotT)
   use m_flowgeom, only: ndx
   use m_xbeach_data, only: ndissip, coefdispT, coefdispk
   use m_physcoef, only: rhomean, ag, rhog
   use m_sferic, only: twopi
   implicit none

   double precision, dimension(ndx)        , intent(in)  :: Df
   double precision, dimension(ndx)        , intent(in)  :: E
   double precision, dimension(ndx)        , intent(in)  :: sigmwav
   double precision, dimension(ndx)        , intent(in)  :: cgwav
   double precision, dimension(ndx)        , intent(in)  :: kwav   
   double precision, dimension(ndx)        , intent(out) :: DtotT
   
   DtotT = - coefdispT * tanh(coefdispk * kwav) * 1.d0 /(1.d0 -ndissip) * (twopi) / sigmwav / sigmwav * cgwav * kwav / E * Df              
      
1234 continue 
   return
   
end subroutine xbeach_wave_period_breaker_dissipation    

subroutine xbeach_wave_compute_period_depth_limitation(E, Tmaxdep )
   use m_flowgeom, only: ndx
   use m_xbeach_data, only: wmagcc, aa1, aa2, bb1, bb2
   use m_physcoef, only: rhomean, ag 
   
   implicit none

   double precision, dimension(ndx)        , intent(in)  :: E
   double precision, dimension(ndx)        , intent(out) :: Tmaxdep
   
   integer                                               :: k
   integer                                               :: ierr
   
   double precision, allocatable                         :: Edls(:)
   double precision, allocatable                         :: Tdls(:)
   
   allocate(Edls(1:ndx), Tdls(1:ndx), stat = ierr)
   
   Edls=ag / rhomean / wmagcc**4d0 * E
   Tdls=aa2*(16d0* Edls / (aa1*aa1) )**(bb2/(2*bb1)) 
   Tmaxdep = wmagcc * Tdls / ag
      
1234 continue
   deallocate(Edls, Tdls, stat=ierr)  
   return   
 end subroutine xbeach_wave_compute_period_depth_limitation 
     
   
 subroutine advec_horz_windmodel(dtmaxwav, snx, csx, limtypw, quant, veloc, advec)
   use m_sferic
   use m_physcoef
   use m_flowgeom
   use m_flowparameters, only:eps10
   
   implicit none
   
   integer                                                :: L, k, k1, k2, itheta, ku, kl2s, kl2, kl1, kd, is, ip
   double precision                                       :: velocL, qds, qst, half, fluxvel1, waku, sl1, sl2, sl3
   double precision                                       :: cf, ds2, ds1, ds, cwuL
   double precision, intent(in)                           :: dtmaxwav
   double precision, intent(in), dimension(ntheta)        :: snx, csx
   integer,          intent(in)                           :: limtypw
   double precision, intent(in), dimension(ntheta, ndx)   :: veloc
   double precision, intent(in), dimension(ntheta,ndx)    :: quant
   double precision, intent(out), dimension(ntheta, ndx)  :: advec
   double precision, external                             :: dslim
   
   double precision                                       :: cs, sn, wuL
                                                          
   integer                                                :: nwalls
   
   advec = 0d0
   do L  = 1,lnx                                                              ! upwind (supq) + limited high order (dsq), loop over link
        k1  = ln(1,L) ; k2 = ln(2,L)                                       ! linker en rechtercelnr geassocieerd aan de links
        
        do itheta = 1,ntheta
            velocL  = acL(L)*veloc(itheta,k1) + (1d0-acL(L))*veloc(itheta,k2)
            cwuL    = velocL*( csu(L)*csx(itheta) + snu(L)*snx(itheta) )   ! *au(L)   met cwi: u1(L) + cg*( csu(L)*csx(itheta) + snu(L)*snx(itheta) )
                                                                           ! inproduct cgx*csu+cgy*snu
            if (cwuL > 0) then                                              !   ->      ds1   ds2
                k = k1 ; kd = k2 ; is =  1 ; half = 1d0 - acl(L) ; ip = 0   !   ->   ku     k     kd
            else                                                            !   <-      ds2   ds1
                k = k2 ; kd = k1 ; is = -1 ; half = acl(L)       ; ip = 3   !   <-   kd     k     ku
            endif                                                           ! acL = linkse dx fractie van afstand tussen flownodes (slide 83)

            fluxvel1  = is*cwuL*wu(L)                                       ! snelheidsbijdrage linkse cel
            qst = fluxvel1*quant(itheta,k)                                  ! cg*E voor link L, sector itheta
            advec(itheta,kd) = advec(itheta,kd) - qst                       ! downwind cel krijgt bijdrage
            advec(itheta,k)  = advec(itheta,k)  + qst                       ! centrale cel verliest bijdrage

            if (limtypw > 0 ) then                                          ! hogere orde, tijdstapafhankelijk!
                ku  = klnup(1+ip,L)                                         ! pointer upwind cel horende bij link L
            
                if (ku .ne. 0 ) then
                    kl2s = klnup(2+ip,L) ; kl2 = iabs(kl2s)                 ! 
            
                    if (ku < 0) then
                        waku = quant(itheta,abs(ku))                        ! pointer naar cel negatief?
                    else
                        kl1  = ku
                        sl1  = slnup(1+ip,L) ; sl2  = slnup(2+ip,L)             ! link upwind cell weight
                        waku  = quant(itheta,kl1)*sl1 + quant(itheta,kl2)*sl2   ! gewogen gemiddelde upwind waarden
                    endif  
            
                    sl3 = slnup(3+ip,L)
                    cf  =  dtmaxwav*abs(cwuL)*dxi(L)                  
                    cf  =  half*max( 0d0,1d0-cf )                    
                    ds2  =  quant(itheta,kd) - quant(itheta,k)        ! ds1 = voorlopende slope, ds2 = eigen slope
                    ds1  = (quant(itheta,k)  - waku )*sl3
            
                    if (abs(ds2)  > eps10 .and. abs(ds1) > eps10) then
                        ds  =  cf*dslim(ds1, ds2, limtypw)                  ! reconstructie van totale slope volgens 1 van de 4 schema's                                            ! centraal schema
            
                        if (abs(ds) > eps10) then                           ! als celgemiddelde niet volstaat
                            qds      =  ds*fluxvel1                         ! slope * linkse celbijdrage
                            advec(itheta,kd) =  advec(itheta,kd) - qds        ! downwind cel krijgt bijdrage
                            advec(itheta,k ) =  advec(itheta,k ) + qds        ! cel verliest bijdrage
                        endif
                    endif
                endif
            endif
        enddo ! directions
    enddo ! links
    
    
!  account for outflow at closed boundaries   
   do nwalls=1,mxwalls
     k1 = walls(1,nwalls)     
     cs =  walls(8,nwalls) ! outward positive
     sn = -walls(7,nwalls)
     wuL = walls(9,nwalls)
     
     do itheta = 1,ntheta
         cwuL    = veloc(itheta, k1)*( cs*csx(itheta) + sn*snx(itheta) )   ! *au(L)   met cwi: u1(L) + cg*( csu(L)*csx(itheta) + snu(L)*snx(itheta) )
         fluxvel1 = cwuL*wuL
         
         if ( fluxvel1.gt.0 ) then
           advec(itheta,k1) = advec(itheta,k1) + fluxvel1*quant(itheta,k1)
         end if
      end do
   end do
   
! account for thin dams
   do nwalls=1,nthd
     k1 = thindam(1,nwalls)     
     cs = thindam(5,nwalls) 
     sn = -thindam(4,nwalls)
     wuL = thindam(6,nwalls)
     
     do itheta = 1,ntheta
         cwuL    = veloc(itheta, k1)*( cs*csx(itheta) + sn*snx(itheta) )   ! *au(L)   met cwi: u1(L) + cg*( csu(L)*csx(itheta) + snu(L)*snx(itheta) )
         fluxvel1 = cwuL*wuL
         
         if ( fluxvel1.gt.0 ) then
           advec(itheta,k1) = advec(itheta,k1) + fluxvel1*quant(itheta,k1)
         end if
      end do
   end do

end subroutine advec_horz_windmodel

subroutine update_ee1rr_windmodel(dtmaxwav, sigt, tt1, cgwavt, ctheta, horadvec, thetaadvec, E, H, thet, thetamean,   &
                        sigmwav, gammax, hh, &
                        fw, break, deltaH, waveps,cgwav, kwav, km, gamma, gamma2, nroelvink, QB, alpha, trep, R, cwav, D,   &
                        roller, br, &
                        urms_cc, fwcutoff, Df, DDlok, wete, rrhoradvec, rrthetaadvec,  jawsource, wsorE, wsorT, gradcg, mwind, &
                        snx, csx, limtypw, &
                         ee1, rr, drr, wci, rhs, solver, nbndw, kbndw, zbndw)

   use m_flowgeom, only: ntheta, Ndxi, Ndx, Lnx, ba, bai, dtheta, thetabin
   use m_flowparameters, only: epshs
   use m_flow, only: vol1 
   use m_physcoef, only: rhog, rhomean, ag
   use m_xbeach_typesandkinds, only: slen
   use m_wind, only: jawind, wx, wy
   use m_solver
   use m_sferic, only: pi, twopi
   use m_xbeach_data, only: advecmod, Trepini, Eini
   implicit none
   
   double precision,                          intent(in)     :: dtmaxwav     !< time step
   double precision, dimension(ntheta,Ndx),   intent(inout)  :: sigt         !< relative frequency
   double precision, dimension(ntheta,Ndx),   intent(inout)  :: tt1          !< wave period
   double precision, dimension(ntheta,Ndx),   intent(inout)  :: cgwavt        !< group velocity
   double precision, dimension(ntheta,Ndx),   intent(inout)  :: ctheta       !< refraction velocity
   double precision, dimension(ntheta,Ndx),   intent(inout)  :: horadvec     !< horizontal advection (work array)
   double precision, dimension(ntheta,Ndx),   intent(inout)  :: thetaadvec   !< directional advection (work array)
   double precision, dimension(Ndx),          intent(inout)  :: E            !< bulk energy (work array)
   double precision, dimension(Ndx),          intent(inout)  :: H            !< significant wave height (work array)
   double precision, dimension(ntheta,Ndx),   intent(inout)  :: thet         !< significant wave height (work array)
   double precision, dimension(Ndx),          intent(inout)  :: thetamean
   double precision, dimension(Ndx),          intent(inout)  :: sigmwav
   double precision,                          intent(in)     :: gammax
   double precision, dimension(Ndx),          intent(inout)  :: hh
   double precision, dimension(Ndx),          intent(in)     :: fw
                                              
   character(len=slen),                       intent(inout)  :: break
   double precision,                          intent(inout)  :: DeltaH
   double precision,                          intent(inout)  :: waveps
   double precision, dimension(Ndx),          intent(in)     :: cgwav   
   double precision, dimension(Ndx),          intent(in)     :: kwav
   double precision, dimension(Ndx),          intent(in)     :: km
   double precision,                          intent(in)     :: gamma
   double precision,                          intent(in)     :: gamma2
   double precision,                          intent(in)     :: nroelvink
   double precision, dimension(Ndx),          intent(inout)  :: QB
   double precision,                          intent(in)     :: alpha
   double precision,                          intent(in)     :: Trep
   double precision,                          intent(in)     :: mwind
   double precision, dimension(Ndx),          intent(inout)  :: R
   double precision, dimension(Ndx),          intent(in)     :: cwav
   double precision, dimension(Ndx),          intent(inout)  :: D
                                              
   integer,                                   intent(in)     :: roller
   integer,                                   intent(in)     :: wci
   integer,                                   intent(in)     :: jawsource
   double precision, dimension(Ndx),          intent(in)     :: br
   double precision, dimension(Ndx),          intent(inout)  :: urms_cc
   double precision,                          intent(inout)  :: fwcutoff
   double precision, dimension(Ndx),          intent(inout)  :: Df
   double precision, dimension(ntheta,Ndx),   intent(inout)  :: DDlok
   double precision, dimension(ntheta,Ndx),   intent(inout)  :: wete
   double precision, dimension(ntheta,Ndx),   intent(inout)  :: rrhoradvec
   double precision, dimension(ntheta,Ndx),   intent(inout)  :: rrthetaadvec
   double precision, dimension(ntheta,Ndx),   intent(inout)  :: wsorE
   double precision, dimension(ntheta,Ndx),   intent(inout)  :: wsorT
   double precision, dimension(ntheta,Ndx),   intent(inout)  :: gradcg
   
   double precision, dimension(ntheta),       intent(in)     :: snx, csx
   
   integer,                                   intent(in)     :: limtypw
                                              
   double precision, dimension(ntheta,Ndx),   intent(inout)  :: ee1          !< energy field
   double precision, dimension(ntheta,Ndx),   intent(inout)  :: rr           !< roller energy
   double precision, dimension(ntheta,Ndx),   intent(inout)  :: drr          !< roller energy dissipation     

   double precision, dimension(ntheta,Ndx),   intent(inout)  :: rhs          !< right-hand side, work array
   type(tsolver),                             intent(inout)  :: solver       !< solver
   integer,                                   intent(in)     :: nbndw        !< number of Dirichlet boundary nodes
   integer,          dimension(nbndw),        intent(in)     :: kbndw        !< boundary nodes
   double precision, dimension(ntheta,nbndw), intent(inout)  :: zbndw        !< boundary values 

   double precision, dimension(:,:),          allocatable    :: ma           !< moment a
   double precision, dimension(:,:),          allocatable    :: mb           !< moment b
   double precision, dimension(:,:),          allocatable    :: src_coeff    !< coefficient of sources
   double precision, dimension(:,:),          allocatable    :: src_expl     !< explicit sources
   double precision, dimension(:,:),          allocatable    :: src_coeffa   !< coefficient of sources
   double precision, dimension(:,:),          allocatable    :: src_expla    !< explicit sources
   double precision, dimension(:,:),          allocatable    :: src_coeffb   !< coefficient of sources
   double precision, dimension(:,:),          allocatable    :: src_explb    !< explicit sources
   double precision, dimension(:,:),          allocatable    :: zbndwa       !< 
   double precision, dimension(:,:),          allocatable    :: zbndwb       !<    
   double precision                                          :: dfac
   double precision                                          :: dis
   integer                                                   :: k, itheta
   integer                                                   :: n
   integer                                                   :: iters, ierror
      
!  allocate and initialize
   allocate(ma(ntheta,Ndx))
   ma = 0d0
   allocate(mb(ntheta,Ndx))
   mb = 0d0
   allocate(src_coeff(ntheta,Ndx))
   src_coeff = 0d0
   allocate(src_expl(ntheta,Ndx))
   src_expl  = 0d0
   allocate(src_coeffa(ntheta,Ndx))
   src_coeffa = 0d0
   allocate(src_expla(ntheta,Ndx))
   src_expla  = 0d0   
   allocate(src_coeffb(ntheta,Ndx))
   src_coeffb = 0d0
   allocate(src_explb(ntheta,Ndx))
   src_explb  = 0d0   
   allocate(zbndwa(ntheta,Ndx))
   zbndwa = 0d0
   allocate(zbndwb(ntheta,Ndx))
   zbndwb  = 0d0   

!  Energy integrated over wave directions,Hrms
!
   E=sum(ee1,dim=1)*dtheta
   H=sqrt(8.d0*E/rhomean/ag)
   thetamean=sum(ee1*thet,1)/max(sum(ee1,1),0.00001d0) ! energy weighted wave direction
   sigmwav = max(sum(ee1*sigt,1),0.00001d0)/(max(sum(ee1,1),0.00001d0))  
   
   
!  formulate limitation of wave energy as source
   if ( dtmaxwav.gt.0d0 ) then
      do k=1,Ndx
         do itheta=1,ntheta
            src_coeff(itheta,k) = -(max(1.d0,(H(k)/(gammax*hh(k)))**2) - 1d0) / dtmaxwav !why not distributed over wave directions?
         end do
      end do
   end if

   H=min(H,gammax*hh)
   E=1.d0/8.d0*rhomean*ag*(H**2)   

!  Breaker dissipation
   call xbeach_wave_breaker_dissipation(dtmaxwav, break, DeltaH, waveps, kwav, km, gamma, gamma2, nroelvink, QB, alpha, Trep, cwav, thetamean, E, D, sigmwav, wci, 1)
 !  D=min(D,10d0);
!  Dissipation by bed friction
   do k=1,Ndx
      dfac = 0.28d0*fw(k)*rhomean
!      urms_cc(k) = pi * H(k) / Trep / sinh(min(max(kwav(k),0.01d0)*max(hh(k),deltaH*H(k)),10.0d0))
!      urms_cc(k) = pi * H(k) / Trepini / sinh(min(max(kwav(k),0.01d0)*max(hh(k),deltaH*H(k)),10.0d0))
      urms_cc(k) = pi * H(k) * sigmwav(k) / 2.d0 / pi / sinh(min(max(kwav(k),0.01d0)*max(hh(k),deltaH*H(k)),10.0d0))
      Df(k)=dfac*urms_cc(k)**3
   end do

   do k=1,Ndx
      if ( hh(k).gt.fwcutoff ) then
         Df(k) = 0d0
      end if
   end do
   
! Wind source term
   if (jawind > 0 .and. jawsource>0) then
       ! TODO: write source term routine that computes dA/dt instead of dT/dt since matrix is developed for A and E and not E and T
!      call xbeach_windsource(ee1, E, tt1, sigmwav , cgwavt, cgwav, hh, dtmaxwav, wsorE, wsorT,gradcg) !!
       wsorE = 0.d0
       wsorT = 0.d0
   else
      wsorE= 0d0
      wsorT= 0d0
   end if
   
   do k=1,Ndx
           do itheta=1,ntheta
              dis = (D(k)+Df(k)) /max(E(k),1d-10) !because coefficient is by definition multiplied with current ee1 we don't need to multiply by ee1(itheta,k)
              src_coeff(itheta,k) = src_coeff(itheta,k)  - dis 
              src_expl(itheta,k) = (src_expl(itheta,k) + wsorE(itheta,k))   !this is going to become much more complex if the source term needs to be expressed in terms of A....
           end do
   end do   
   
!  construct and solve system
   
   if (advecmod.eq.1) then
       !define
        ma=ee1/sigt
        mb=ee1
        
        !  scale boundary conditions to moments definition
        do n=1,nbndw
           do itheta=1,ntheta
              k = kbndw(n)
              zbndwa(itheta,n) = zbndw(itheta,n) / sigt(itheta,k)
              zbndwb(itheta,n) = zbndw(itheta,n) 
           end do
        end do 
        
        do k=1,Ndx
                do itheta=1,ntheta
                   src_coeffa(itheta,k) = src_coeff(itheta,k) / sigt(itheta,k)
                   src_expla(itheta,k) = src_expl(itheta,k) / sigt(itheta,k)        
        
                   src_coeffb(itheta,k) = src_coeff(itheta,k)
                   src_explb(itheta,k) = src_expl(itheta,k)            
                end do
        end do          
        
   elseif (advecmod.eq.2) then
       !define moments
       ma=ee1
       mb=sigt*ee1
       
       !  scale boundary conditions to moments definition       
       do n=1,nbndw
          do itheta=1,ntheta
             k = kbndw(n)
             zbndwa(itheta,n) = zbndw(itheta,n)           
             zbndwb(itheta,n) = zbndw(itheta,n) * sigt(itheta,k)
          end do
       end do 
       
        do k=1,Ndx
           do itheta=1,ntheta
              src_coeffa(itheta,k) = src_coeff(itheta,k)
              src_expla(itheta,k)  = src_expl(itheta,k) 
              
              src_coeffb(itheta,k) = src_coeff(itheta,k) * sigt(itheta,k)
              src_explb(itheta,k)  = src_expl(itheta,k)  * sigt(itheta,k)
           end do
        end do         
       
   endif
       
! moment a 
   call xbeach_fillsystem_windmodel(solver,ntheta,ma,src_coeffa,src_expla,cgwavt,csx,snx,ctheta,dtheta,dtmaxwav,nbndw,kbndw,zbndwa,0,ierror)
   !write(6,*) 'Fill wave energy system:: ierror=', ierror      
   call xbeach_solvesystem(solver,ma,iters,ierror)
   !write(6,*) 'Solve wave energy system:: ierror=', ierror, ', no of iters=',iters
      
! momentb
   call xbeach_fillsystem_windmodel(solver,ntheta,mb,src_coeffb,src_explb,cgwavt,csx,snx,ctheta,dtheta,dtmaxwav,nbndw,kbndw,zbndwb,0,ierror)
   !write(6,*) 'Fill wave energy system:: ierror=', ierror      
   call xbeach_solvesystem(solver,mb,iters,ierror)
   !write(6,*) 'Solve wave energy system:: ierror=', ierror, ', no of iters=',iters
 
!  update the wave energy and frequency
   if (advecmod.eq.1) then   
      do k=1,Ndx
         if ( vol1(k).lt.epshs*ba(k) ) then
            do itheta=1,ntheta
               ma(itheta,k)=Eini*Trepini/twopi
               mb(itheta,k)=Eini
            end do
         end if
      end do
      ee1=max(mb,Eini)
      sigt=min(mb/ma,twopi/Trepini)
      tt1=twopi/sigt
   elseif (advecmod.eq.2) then   
        do k=1,Ndx
         if ( vol1(k).lt.epshs*ba(k) ) then
            do itheta=1,ntheta     
               ma(itheta,k)=Eini
               mb(itheta,k)=Eini/Trepini*2.d0*pi       
            end do
         endif
        enddo
        ee1=max(ma,Eini)
        sigt=min(mb/ma,twopi/Trepini)
        tt1=twopi/sigt
   endif
   
   
   if ( roller.eq.1 ) then
      
      ! Roller balance
      do k=1,Ndx
         do itheta=1,ntheta
            src_coeff(itheta,k) =  -2*ag*BR(k)/cwav(k)
            src_expl(itheta,k) = ee1(itheta,k)*D(k)/max(E(k),1d-10)
         end do
      end do
   
      call xbeach_fillsystem(solver,ntheta,rr,src_coeff,src_expl,cwav,csx,snx,ctheta,dtheta,dtmaxwav,0, (/ 0 /), (/ 0d0 /), 0, ierror)
      !write(6,*) 'Fill roller energy system:: ierror=', ierror
      
      call xbeach_solvesystem(solver,rr,iters,ierror)
      !write(6,*) 'Solve roller energy system:: ierror=', ierror, ', no of iters=',iters
      
      do k=1,Ndx
         if ( vol1(k).lt.epshs*ba(k) ) then
            do itheta=1,ntheta
               rr(itheta,k) = 0d0
            end do
         end if
      end do
      
   else
      rr = 0d0
   end if

   rr=max(rr,0.0d0)
!
   do itheta = 1, ntheta
      where (hh+deltaH*H>epshs) 
           wete(itheta,:)=1d0
      elsewhere
           wete(itheta,:)=0d0
      end where
   enddo
   
   do k = 1,Ndx
      do itheta=1,ntheta
         if(wete(itheta, k)==1) then
            ee1(itheta, k)    = max(ee1(itheta, k),0.0d0)
            rr(itheta, k)     = max(rr(itheta, k),0.0d0)
            drr(itheta,k)     = max(-src_coeff(itheta,k)*rr(itheta,k),0.0d0)
         elseif(wete(itheta, k)==0) then
            ee1(itheta, k)    = 0.0d0
            rr(itheta, k)     = 0.0d0
            drr(itheta,k)     = 0.0d0
         end if
      end do
   end do
   
!  deallocate
   if ( allocated(src_coeff) ) deallocate(src_coeff)
   if ( allocated(src_expl)  ) deallocate(src_expl)
   if ( allocated(src_coeffa) ) deallocate(src_coeffa)
   if ( allocated(src_expla)  ) deallocate(src_expla)
   if ( allocated(src_coeffb) ) deallocate(src_coeffb)
   if ( allocated(src_explb)  ) deallocate(src_explb)
   if ( allocated(ma) ) deallocate(ma)
   if ( allocated(mb)  ) deallocate(mb)   
   if ( allocated(zbndwa) ) deallocate(zbndwa)
   if ( allocated(zbndwb)  ) deallocate(zbndwb)  
   return
end subroutine update_ee1rr_windmodel
 
subroutine xbeach_dispersion_windmodel()
   use m_xbeach_filefunctions
   use m_flowgeom
   use m_flow, only: s1, hu
   use m_flowparameters, only: epshu, epshs
   use m_sferic, only: pi
   use m_xbeach_data, only: hdisp, deltaH, H, waveps, sigt, sigmwav, L0t, L1t, Ltempt, cwavt, nwavt, cgwavt, kwavt, cwav, nwav, cgwav, kwav, ee1
   use m_physcoef, only: ag
   use m_flowtimes, only: time0
   use m_flowexternalforcings

   implicit none

   integer                                          :: i,j,j1,j2,k,L,k1,k2,itheta
   double precision                                 :: kh, hh
   double precision, external                       :: iteratedispersion
   
       
   do k=1,ndx
      hh=max(s1(k)-bl(k),epshs)
      if (hh > epshs) then
         hdisp(k) = max(hh + deltaH*H(k), waveps)
         do itheta = 1,ntheta
            L0t(itheta,k) = 2*pi*ag/(sigt(itheta,k)**2)
         enddo
      else
         hdisp(k) = waveps
         do itheta=1,ntheta
            L0t(itheta,k)    = waveps
         enddo
      end if
   enddo
   L1t=L0t
   
   do k=1,ndxi
      if(hdisp(k).ge.waveps) then
          do itheta = 1,ntheta
             if (2*pi/L0t(itheta,k)*hdisp(k) > 5d0) then
                 Ltempt(itheta,k) = L0t(itheta,k)
              else
                 !Ltempt(k) = (2d0*pi*ag/(sigt(itheta,k)**2))*(1-exp(-(sigt(itheta,k)*sqrt(hdisp(k)/ag))**(5d0/2d0)))**(2d0/5d0)
                 Ltempt(itheta,k) = iteratedispersion(L0t(itheta,k),Ltempt(itheta,k),pi,hdisp(k))
                 if (Ltempt(itheta,k)<0.d0) then   ! this is an error from iteratedispersion
                    Ltempt(itheta,k) = -Ltempt(itheta,k)
                    call writelog('lws','','Warning: no convergence in dispersion relation iteration at t = ', &
                       time0)
                 endif
              endif
              L1t(itheta,k)=Ltempt(itheta,k)
          enddo    
      endif
   end do
   
   do L=1,nbndz
      k1=kbndz(1,L); k2=kbndz(2,L)
      do itheta=1,ntheta
         L1t(itheta,k1) = L1t(itheta,k2)
      enddo
   end do
   
   do L=1,nbndu
      k1=kbndu(1,L); k2=kbndu(2,L)
      do itheta=1,ntheta
         L1t(itheta,k1) = L1t(itheta,k2)
      enddo
   end do
   
   do k=1,ndx
       do itheta=1,ntheta
         kwavt(itheta,k)  = 2*pi/max(L1t(itheta,k),waveps)
         cwavt(itheta,k)  = sigt(itheta,k)/kwavt(itheta,k)
         kh   = min(kwavt(itheta,k)*hdisp(k),10.0d0)
         nwavt(itheta,k)=0.5d0+kh/max(sinh(2d0*kh),waveps)
         cgwavt(itheta,k)=cwavt(itheta,k)*nwavt(itheta,k)
      enddo
   end do
   
   do k=1,ndx
      hh=s1(k)-bl(k)
      if (hh<epshs) then
         do itheta=1,ntheta
            kwavt(itheta,k)=0d0
         enddo
      endif
   enddo
   
   ! define thetabin-energy-weighted average values for propagationspeeds and wave numbers
   cgwav = max(sum(ee1*cgwavt,1),1d-6)/max(sum(ee1,1),1d-6)
   cwav  = max(sum(ee1*cwavt,1),1d-6) /max(sum(ee1,1),1d-6)
   nwav  = max(sum(ee1*nwavt,1),1d-6) /max(sum(ee1,1),1d-6)
   kwav  = max(sum(ee1*kwavt,1),1d-6) /max(sum(ee1,1),1d-6)
   
   end subroutine xbeach_dispersion_windmodel
   
! ======================   
! Stationary solver
! ====================== 
   
subroutine xbeach_solve_wave_stationary(ierr)
   ! Based on wave energy balance for directionally spread waves, single representative frequency
   ! 4-sweep implicit method
   ! At the main level arrays are two-dimensional in 
   ! space but at lower level they are 1D.
   !
   ! (c) 2020 Dano Roelvink, Johan Reyns IHE Delft
   !
   use m_xbeach_paramsconst, only: TURB_NONE
   use m_xbeach_data
   use m_flowgeom
   use m_flow, only: s1, epshs
   use network_data, only: xk, yk, numk
   use m_sferic, only: pi, dg2rd, rd2dg
   use m_physcoef, only: ag, rhomean
   use m_waves, only: hwav, twav, phiwav, ustokes, vstokes, uorb, rlabda
   
   implicit none
   
   integer, intent(out)   :: ierr
   
   ! Local variables
   integer                                             :: k, k1, k2, L
   integer                                             :: itheta
   double precision                                    :: sigm            ! angular frequency
   double precision                                    :: wavdir          ! incident angle in radians
   double precision                                    :: t0,t1,t2,t3,t4  ! timers
   !
   double precision, allocatable, dimension(:)         :: costemp, costemp2, sintemp, sintemp2, hh
   double precision, allocatable, dimension(:)         :: uwf, vwf, ustr, urf, vrf, ustw
   !
   ierr = 1
   !
   if (allocated(costemp)) deallocate(costemp, costemp2, sintemp, sintemp2, hh, &
                                      ustw, uwf, vwf, ustr, urf, vrf, stat=ierr)
   allocate(costemp(1:ndx), costemp2(1:ndx), sintemp(1:ndx), sintemp2(1:ndx), hh(1:ndx), stat=ierr)
   allocate(ustw(1:ndx), uwf(1:ndx), vwf(1:ndx), ustr(1:ndx), stat = ierr)
   allocate(urf(1:ndx), vrf(1:ndx), stat = ierr)
   costemp  = 0d0
   costemp2 = 0d0 
   sintemp  = 0d0
   sintemp2 = 0d0
   ustw     = 0d0
   uwf      = 0d0
   vwf      = 0d0
   ustr     = 0d0
   urf      = 0d0
   vrf      = 0d0
   !
   call timer(t0)
   wavdir = (270d0-dir0)*dg2rd
   !
   hh = max(s1-bl, epshs)
   !
   call flownod2corner(fw, ndx, fwstat, numk, ierr)
   where (hhstat>fwcutoff)
      fw = 0d0   
   endwhere   
   !
   call timer(t1)
   !
   ! Solve the directional wave energy balance on an unstructured grid
   call solve_energy_balance2Dstat (xk,yk,numk,w,ds,inner,prev,seapts,noseapts,nmmask,       &
                                    e01,thetabin,ntheta,wavdir,                                    &
                                    hhstat,kwavstat,cgstat,cthetastat,fwstat,Trep,dtmaximp,rhomean,alpha,gamma,                     &
                                    Hstat,Dwstat,Dfstat,thetam,uorbstat,eestat)
   call timer(t2)
   !
   ! copy values to flow nodes
   call corner2flownod(Hstat, numk, H, ndxi, ndx, .false., ierr)
   E = 1d0/8d0*rhomean*ag*H**2   ! assumes H already depth limited
   call corner2flownod(Dwstat, numk, D, ndxi, ndx, .false., ierr)
   call corner2flownod(Dfstat, numk, Df, ndxi, ndx, .false., ierr)
   call corner2flownod(uorbstat, numk, urms_cc, ndxi, ndx, .false., ierr)
   urms_cc = urms_cc/sqrt(2d0)
   call corner2flownod(kwavstat, numk, kwav, ndxi, ndx, .false., ierr)
   !
   do itheta = 1, ntheta
      costemp = eestat(:,itheta)  ! stack
      call corner2flownod(costemp, numk, costemp2, ndxi, ndx, .true., ierr)   
      ee1(itheta,:) = costemp2
   enddo 
   costemp=0d0; costemp2=0d0
   call xbeach_apply_wave_bc()   ! fill boundary nodes
   !
   ! compute urms
   do L=1,Lnx
      k1 = ln(1,L)
      k2 = ln(2,L)
      urms(L) = (acL(L) * urms_cc(k1) + (1d0-acl(L))*urms_cc(k2))
   end do
   !
   ! get value for roller energy
   call corner2flownod(cstat, numk, cwav, ndxi, ndx, .false., ierr)
   R = D*cwav/2d0/ag/beta
   !
   ! get mean wave direction
   costemp = cos(thetam)
   sintemp = sin(thetam)
   call corner2flownod(costemp, numk, costemp2, ndxi, ndx, .false., ierr)
   call corner2flownod(sintemp, numk, sintemp2, ndxi, ndx, .false., ierr)
   thetamean = atan2(sintemp2, costemp2)
   !
   ! compute stokes drift contributions
   do k=1,ndx    
      ustw(k)= E(k)/max(cwav(k),sqrt(epshs*ag))/rhomean/hh(k)      ! waves
      ustr(k)= 2d0*R(k)/max(cwav(k),sqrt(epshs*ag))/rhomean/hh(k)  ! roller
      uwf(k) = ustw(k)*cos(thetamean(k))                           ! Cartesian decomposition
      vwf(k) = ustw(k)*sin(thetamean(k))
      urf(k) = ustr(k)*cos(thetamean(k))
      vrf(k) = ustr(k)*sin(thetamean(k))
      ustx_cc(k) = uwf(k)+urf(k); usty_cc(k)=vwf(k)+vrf(k)
   end do
   !
   do L=1,lnx                                   
      k1 = ln(1,L); k2 = ln(2,L)
      ust(L) = acL(L)*(csu(L)*(uwf(k1)+urf(k1))+snu(L)*(vwf(k1)+vrf(k1))) + &
         (1d0-acL(L))*(csu(L)*(uwf(k2)+urf(k2))+snu(L)*(vwf(k2)+vrf(k2)))
   
   
      vst(L) = acL(L)*(-snu(L)*(uwf(k1)+urf(k1))+csu(L)*(vwf(k1)+vrf(k1))) + &
         (1d0-acL(L))*(-snu(L)*(uwf(k2)+urf(k2))+csu(L)*(vwf(k2)+vrf(k2)))
   enddo
   ! 
   if (roller.eq.1 .and. turb.ne.TURB_NONE) then
      call borecharacter()                   ! calculates BR and Tbore using Rieneck&Fenton approach   
   end if 
   !
   ! Assign values to wave communication arrays
   hwav = H
   twav = 2.0*pi/sigmwav
   phiwav = thetamean*rd2dg
   rlabda = 2d0*pi/kwav
   uorb = urms_cc
   ustokes = ust
   vstokes = vst 
   !
   call timer(t3)
   !
   write(*,*) 'stationary wave computation:               ',t2-t1,' seconds'
   write(*,*) 'copy corner values to flow nodes:          ',t3-t2,' seconds'
   write(*,*) 'total computation time solver:             ',t3-t0,' seconds'
   !
   ierr = 0
1234 continue
     return
end subroutine xbeach_solve_wave_stationary
!
subroutine find_upwind_neighbours(x,y,mn,theta,ntheta,kp,np,w,prev,ds,ierr)

   implicit none

   integer, intent(in)                                    :: mn,ntheta           ! length of x,y; length of theta
   integer, intent(in)                                    :: np                  ! max no surrounding points
   double precision,  dimension(mn),          intent(in)  :: x,y                 ! x, y coordinates of grid
   integer,           dimension(mn,np),       intent(in)  :: kp                  ! grid indices of surrounding points per grid point
   double precision,  dimension(ntheta),      intent(in)  :: theta               ! array of wave angles 
   double precision,  dimension(mn,ntheta,2), intent(out) :: w                   ! per grid point and direction, weight of upwind points
   integer,           dimension(mn,ntheta,2), intent(out) :: prev                ! per grid point and direction, indices of upwind points
   double precision,  dimension(mn,ntheta),   intent(out) :: ds                  ! upwind distance to intersection point for each direction
   integer,                                   intent(out) :: ierr
   
   double precision,  dimension(2)                        :: xsect,ysect,ww
   double precision                                       :: pi,dss,xi,yi
   integer                                                :: ind1,ind2
   integer                                                :: ip,nploc
   integer                                                :: k, itheta
   
   ierr = 1
   
   ! find upwind neighbours for each cell in an unstructured grid x,y (1d
   ! vectors) given vector of directions theta

   pi=4d0*atan(1.d0)

   do k=1,mn
      call findlocpos(kp(k,:),np,0,nploc)
      nploc=nploc-1
      if (kp(k,1)/=0) then
         do itheta=1,ntheta
            do ip=1,nploc-1
               ind1=kp(k,ip)
               ind2=kp(k,ip+1)
               xsect=[x(ind1),x(ind2)]
               ysect=[y(ind1),y(ind2)]
               call intersect_angle(x(k),y(k),theta(itheta)+pi,xsect,ysect,ww,dss,xi,yi)
               if (dss/=0) then
                  w(k,itheta,1)=ww(1)
                  w(k,itheta,2)=ww(2)
                  ds(k,itheta)=dss
                  prev(k,itheta,1)=kp(k,ip)
                  prev(k,itheta,2)=kp(k,ip+1)
                  exit
               endif
            enddo
            if (dss==0) then
               prev(k,itheta,1)=1
               prev(k,itheta,2)=1
               w(k,itheta,1)=0
               w(k,itheta,2)=0
            endif
         enddo
      endif
   enddo
   
   ierr = 0
1234 continue
     return

end subroutine find_upwind_neighbours
!
subroutine intersect_angle(x0,y0,phi,x,y,W,ds,xi,yi)

   implicit none

   double precision, intent(in)               :: x0,y0,phi
   double precision, dimension(2),intent(in)  :: x,y
   double precision, dimension(2),intent(out) :: W
   double precision, intent(out)              :: ds,xi,yi
   
   double precision                           :: m,a,b,n,L,d1,d2
   double precision                           :: err
   double precision, parameter                :: eps = 1d-3
   
   if (abs(x(2)-x(1))>eps) then
      m=(y(2)-y(1))/(x(2)-x(1))
      a=y(1)-m*x(1)
      n=tan(phi)
      b=y0-n*x0
      xi=(b-a)/(m-n)
      yi=a+m*xi
   else
      yi=(x(1)-x0)*tan(phi)+y0
      xi=x(1)
   endif

   L=hypot(x(2)-x(1),y(2)-y(1))
   d1=hypot(xi-x(1),yi-y(1))
   d2=hypot(xi-x(2),yi-y(2))
   ds=hypot(xi-x0,yi-y0)
   err=hypot((x0-xi)+ds*cos(phi),(y0-yi)+ds*sin(phi))
   if (abs(L-d1-d2)<eps .and. err<eps) then
      W(1)=d2/L
      W(2)=d1/L
   else
      W(1)=0.d0
      W(2)=0.d0
      ds=0.d0
   endif

end subroutine intersect_angle
!
subroutine solve_energy_balance2Dstat(x,y,mn,w,ds,inner,prev,seapts,noseapts,neumannconnected,       &
                                      ee0,theta,ntheta,thetamean,                                    &
                                      hh,kwav,cg,ctheta,fw,T,dt,rho,alfa,gamma,                      &
                                      H,Dw,Df,thetam,uorb,ee)
   
   implicit none

   ! In/output variables and arrays
   integer, intent(in)                        :: mn,ntheta              ! number of grid points, number of directions
   real*8, dimension(mn),intent(in)           :: x,y                    ! x,y coordinates of grid
   real*8,  dimension(mn,ntheta,2),intent(in) :: w                      ! weights of upwind grid points, 2 per grid point and per wave direction
   real*8, dimension(mn,ntheta), intent(in)   :: ds                     ! distance to interpolated upwind point, per grid point and direction
   real*8, intent(in)                         :: thetamean              ! mean offshore wave direction (rad)
   real*8, dimension(ntheta), intent(in)      :: ee0,theta              ! distribution of wave angles and offshore wave energy density
   logical, dimension(mn), intent(in)         :: inner                  ! mask of inner grid points (not on boundary)
   integer, dimension(mn,ntheta,2),intent(in) :: prev                   ! two upwind grid points per grid point and wave direction
   integer, intent(in)                        :: noseapts               ! number of offshore wave boundary points
   integer, dimension(noseapts)               :: seapts                 ! indices of offshore wave boundary points
   integer, dimension(mn),intent(in)          :: neumannconnected       ! number of neumann boundary point if connected to inner point
   real*8, dimension(mn), intent(in)          :: hh                     ! water depth
   real*8, dimension(mn), intent(in)          :: kwav                   ! wave number
   real*8, dimension(mn), intent(in)          :: cg                     ! group velocity
   real*8, dimension(mn,ntheta), intent(in)   :: ctheta                 ! refraction speed
   real*8, dimension(mn), intent(in)          :: fw                     ! wave friction factor
   real*8, intent(in)                         :: T                      ! wave period
   real*8, intent(in)                         :: dt                     ! time step (s)
   real*8, intent(in)                         :: rho                    ! water density
   real*8, intent(in)                         :: alfa,gamma             ! coefficients in Baldock wave breaking dissipation
   real*8, dimension(mn), intent(out)         :: H                      ! wave height
   real*8, dimension(mn), intent(out)         :: Dw                     ! wave breaking dissipation
   real*8, dimension(mn), intent(out)         :: Df                     ! wave friction dissipation
   real*8, dimension(mn), intent(out)         :: thetam                 ! mean wave direction
   real*8, dimension(mn), intent(out)         :: uorb                   ! orbital velocity
   real*8, dimension(mn,ntheta), intent(out)  :: ee                     ! wave energy distribution

   ! Local variables and arrays
   integer, dimension(:), allocatable         :: ok                     ! mask for fully iterated points
   real*8                                     :: eemax,dtheta           ! maximum wave energy density, directional resolution
   integer                                    :: sweep,niter            ! sweep number, number of iterations
   integer                                    :: k,k1,k2,i,ind(1)       ! counters (k is grid index)
   integer, dimension(:,:), allocatable       :: indx                   ! index for grid sorted per sweep direction
   real*8, dimension(:,:), allocatable        :: eeold               ! wave energy density, energy density previous iteration
   real*8, dimension(:,:), allocatable        :: dee                    ! difference with energy previous iteration
   real*8, dimension(:), allocatable          :: eeprev, cgprev         ! energy density and group velocity at upwind intersection point
   real*8, dimension(:,:),allocatable         :: A,B,C,R                ! coefficients in the tridiagonal matrix solved per point
   real*8, dimension(:), allocatable          :: DoverE                 ! ratio of mean wave dissipation over mean wave energy
   real*8, dimension(:), allocatable          :: E                      ! mean wave energy
   real*8, dimension(:), allocatable          :: diff                   ! maximum difference of wave energy relative to previous iteration
   real*8, dimension(:), allocatable          :: ra                     ! coordinate in sweep direction
   integer, dimension(4)                      :: shift
   integer                                    :: iter
   integer                                    :: count
   integer                                    :: itheta
   real*8                                     :: percok
   real*8                                     :: error
   real*8,parameter                           :: pi=4.d0*atan(1.d0)
   real*8,parameter                           :: g=9.81d0
   real*8,parameter                           :: hmin=0.1d0             ! minimum water depth
   real*8,parameter                           :: fac=0.25d0             ! underrelaxation factor for DoverE
   real*8,parameter                           :: crit=0.0001            ! relative accuracy

   ! Allocate local arrays
   allocate(ok(mn))
   allocate(indx(mn,4))
   !allocate(ee(mn,ntheta)); ee=0d0
   allocate(eeold(mn,ntheta)); eeold=0d0
   allocate(dee(mn,ntheta)); dee=0d0
   allocate(eeprev(ntheta)); eeprev=0d0
   allocate(cgprev(ntheta)); cgprev=0d0
   allocate(A(mn,ntheta)); A=0d0
   allocate(B(mn,ntheta)); B=0d0
   allocate(C(mn,ntheta)); C=0d0
   allocate(R(mn,ntheta)); R=0d0
   allocate(DoverE(mn)); DoverE=0d0
   allocate(E(mn)); E=0d0
   allocate(diff(mn)); diff=0d0
   allocate(ra(mn)); ra=0d0

   ok=0
   indx=0
   ee=0d0
   eemax=1.d0;
   dtheta=theta(2)-theta(1)
   niter=400
   
   ! Sort coordinates in sweep directions
   shift=[0,1,-1,2]
   do sweep=1,4
      ra=x*cos(thetamean+shift(sweep)*pi/2.d0)+y*sin(thetamean+shift(sweep)*pi/2.d0)
      call hpsort_eps_epw (mn, ra , indx(:,sweep), 1.d-6)
   enddo

   ! Boundary condition at sea side (uniform)
   do i=1,noseapts
      k=seapts(i)
      ee(k,:)=ee0
   enddo
   
   ! Start iteration
   do iter=1,niter
      sweep=mod(iter,4)
      if (sweep==0) then
         sweep=4;
      endif
      if (sweep==1) then
         eeold=ee;
      endif
      !  Loop over all points depending on sweep direction
      do count=1,mn
         k=indx(count,sweep)
         if (inner(k)) then
            if (hh(k)>1.1*hmin) then
               if (ok(k) == 0) then
                  ! Only perform computations on wet inner points that are not yet converged (ok)
                  do itheta=1,ntheta
                     k1=prev(k,itheta,1)
                     k2=prev(k,itheta,2)
                     eeprev(itheta)=w(k,itheta,1)*ee(k1,itheta)+w(k,itheta,2)*ee(k2,itheta)
                     cgprev(itheta)=w(k,itheta,1)*cg(k1)+w(k,itheta,2)*cg(k2)
                  enddo
                  do itheta=2,ntheta-1
                     A(k,itheta)=-ctheta(k,itheta-1)/2/dtheta
                     B(k,itheta)=1/dt+cg(k)/ds(k,itheta)+DoverE(k)
                     C(k,itheta)=ctheta(k,itheta+1)/2/dtheta
                     R(k,itheta)=ee(k,itheta)/dt+cgprev(itheta)*eeprev(itheta)/ds(k,itheta)
                  enddo
                  if (ctheta(k,1)<0) then
                     A(k,1)=0.d0
                     B(k,1)=1/dt-ctheta(k,1)/dtheta+cg(k)/ds(k,1)+DoverE(k)
                     C(k,1)=ctheta(k,2)/dtheta
                     R(k,1)=ee(k,1)/dt+cgprev(itheta)*eeprev(1)/ds(k,1)
                  else
                     A(k,1)=0.d0
                     B(k,1)=1.d0/dt
                     C(k,1)=0.d0
                     R(k,1)=0.d0
                  endif
                  if (ctheta(k,ntheta)>0) then
                     A(k,ntheta)=-ctheta(k,ntheta-1)/dtheta
                     B(k,ntheta)=1/dt+ctheta(k,ntheta)/dtheta+cg(k)/ds(k,ntheta)+DoverE(k)
                     C(k,ntheta)=0d0
                     R(k,ntheta)=ee(k,ntheta)/dt+cgprev(itheta)*eeprev(ntheta)/ds(k,ntheta)
                  else
                     A(k,ntheta)=0.d0
                     B(k,ntheta)=1.d0/dt
                     C(k,ntheta)=0.d0
                     R(k,ntheta)=0.d0
                  endif
                  ! Solve tridiagonal system per point
                  call solve_tridiag(A(k,:),B(k,:),C(k,:),R(k,:),ee(k,:),ntheta)
                  ee(k,:)=max(ee(k,:),0.d0)
                  E(k)=sum(ee(k,:))*dtheta
                  H(k)=sqrt(8*E(k)/rho/g)
                  H(k)=min(H(k),gamma*hh(k))
                  E(k)=0.125d0*rho*g*H(k)**2
                  call baldock(g,rho,alfa,gamma,kwav(k),hh(k),H(k),T,1,Dw(k))
                  uorb(k)=pi*H(k)/T/sinh(kwav(k)*hh(k))
                  Df(k)=0.28d0*rho*fw(k)*uorb(k)**3
                  DoverE(k)=(1.d0-fac)*DoverE(k)+fac*(Dw(k)+Df(k))/max(E(k),1.d-6)
                  do itheta=1,ntheta
                     B(k,itheta)=1/dt+cg(k)/ds(k,itheta)+DoverE(k)
                  enddo
                  ! Solve tridiagonal system per point
                  call solve_tridiag(A(k,:),B(k,:),C(k,:),R(k,:),ee(k,:),ntheta)
                  ee(k,:)=max(ee(k,:),0.d0)
               endif
            else
               ee(k,:)=0
            endif
            if (neumannconnected(k)/=0) then
                ee(neumannconnected(k),:)=ee(k,:)
            endif
            
         endif

      enddo
      !      
      do k=1,mn
         ! Compute directionally integrated paraneters
         ee(k,:)=max(ee(k,:),0.d0)
         E(k)=sum(ee(k,:))*dtheta
         H(k)=sqrt(8*E(k)/rho/g)
         call baldock(g,rho,alfa,gamma,kwav(k),hh(k),H(k),T,1,Dw(k))
         uorb(k)=pi*H(k)/T/sinh(kwav(k)*hh(k))
         Df(k)=0.28d0*rho*fw(k)*uorb(k)**3
         DoverE(k)=(1.d0-fac)*DoverE(k)+fac*(Dw(k)+Df(k))/max(E(k),1.d-6)
         thetam(k)=atan2(sum(ee(k,:)*sin(theta)),sum(ee(k,:)*cos(theta)))
      enddo
      if (sweep==4) then
         ! Check convergence after all 4 sweeps
         do k=1,mn
            dee(k,:)=ee(k,:)-eeold(k,:)
            diff(k)=maxval(abs(dee(k,:)))
            if (diff(k)/eemax<crit) ok(k)=1
         enddo
         ! Percentage of converged points
         percok=sum(ok)/dble(mn)*100.d0;
         eemax=maxval(ee)
         ! Relative maximum error
         error=maxval(diff)/eemax
         write(*,'(a,i6,a,f10.5,a,f7.2)')'iteration ',iter/4 ,' error = ',error,'   %ok = ',percok
         if (error<crit .and. iter>4) then
            write(*,*) 'Solver converged, exiting...'
            exit
         endif
      endif
   enddo

end subroutine solve_energy_balance2Dstat
!
!
subroutine solve_tridiag(a,b,c,d,x,n)
   implicit none
   !	 a - sub-diagonal (means it is the diagonal below the main diagonal)
   !	 b - the main diagonal
   !	 c - sup-diagonal (means it is the diagonal above the main diagonal)
   !	 d - right part
   !	 x - the answer
   !	 n - number of equations

   integer,                      intent(in)    :: n
   double precision,dimension(n),intent(in)    :: a,b,c,d
   double precision,dimension(n),intent(out)   :: x
   double precision,dimension(n)               :: cp,dp
   double precision                            :: m
   
   integer                                     :: i
   integer,parameter                           :: r8 = kind(1.d0)

   ! initialize c-prime and d-prime
   cp(1) = c(1)/b(1)
   dp(1) = d(1)/b(1)
   ! solve for vectors c-prime and d-prime
   do i = 2,n
      m = b(i)-cp(i-1)*a(i)
      cp(i) = c(i)/m
      dp(i) = (d(i)-dp(i-1)*a(i))/m
   end do
   ! initialize x
   x(n) = dp(n)
   ! solve for x from the vectors c-prime and d-prime
   do i = n-1, 1, -1
      x(i) = dp(i)-cp(i)*x(i+1)
   end do

end subroutine solve_tridiag
!
subroutine baldock (rho,g,alfa,gamma,k,hh,H,T,opt,Dw)

  implicit none  

  double precision, intent(in)                :: rho
  double precision, intent(in)                :: g
  double precision, intent(in)                :: alfa
  double precision, intent(in)                :: gamma
  double precision, intent(in)                :: k
  double precision, intent(in)                :: hh
  double precision, intent(in)                :: H
  double precision, intent(in)                :: T
  integer, intent(in)                         :: opt
  double precision, intent(out)               :: Dw
  
  double precision                            :: dtol
  double precision                            :: Hmax

  dtol = 1d-8
  if (H<dtol) then
     Dw = 0d0
     return
  endif   
   ! Compute dissipation according to Baldock
   Hmax=0.88/k*tanh(gamma*k*hh/0.88d0)
   if (opt==1) then
      Dw=0.25*alfa*rho*g/T*exp(-(Hmax/H)**2)*(Hmax**2+H**2)
   else
      Dw=0.25*alfa*rho*g/T*exp(-(Hmax/H)**2)*(Hmax**3+H**3)/gamma/hh
   endif
end subroutine baldock
!
!
! Copyright (C) 2010-2016 Samuel Ponce', Roxana Margine, Carla Verdi, Feliciano Giustino
! Copyright (C) 2007-2009 Jesse Noffsinger, Brad Malone, Feliciano Giustino
!
! This file is distributed under the terms of the GNU General Public
! License. See the file `LICENSE' in the root directory of the
! present distribution, or http://www.gnu.org/copyleft.gpl.txt .
!
! Adapted from flib/hpsort_eps
!---------------------------------------------------------------------
subroutine hpsort_eps_epw (n, ra, ind, eps)
   !---------------------------------------------------------------------
   ! sort an array ra(1:n) into ascending order using heapsort algorithm,
   ! and considering two elements being equal if their values differ
   ! for less than "eps".
   ! n is input, ra is replaced on output by its sorted rearrangement.
   ! create an index table (ind) by making an exchange in the index array
   ! whenever an exchange is made on the sorted data array (ra).
   ! in case of equal values in the data array (ra) the values in the
   ! index array (ind) are used to order the entries.
   ! if on input ind(1)  = 0 then indices are initialized in the routine,
   ! if on input ind(1) != 0 then indices are assumed to have been
   !                initialized before entering the routine and these
   !                indices are carried around during the sorting process
   !
   ! no work space needed !
   ! free us from machine-dependent sorting-routines !
   !
   ! adapted from Numerical Recipes pg. 329 (new edition)
   !
   implicit none
   
   !-input/output variables
   integer,                        intent(in)    :: n
   double precision,               intent(in)    :: eps
   integer, dimension(n),          intent(inout) :: ind
   double precision, dimension(n), intent(inout) :: ra
   
   !-local variables
   integer          :: i, ir, j, l, iind
   double precision :: rra
   !
   ! initialize index array
   IF (ind (1) .eq.0) then
      DO i = 1, n
         ind (i) = i
      ENDDO
   ENDIF
   ! nothing to order
   IF (n.lt.2) return
   ! initialize indices for hiring and retirement-promotion phase
   l = n / 2 + 1

   ir = n

   sorting: do

      ! still in hiring phase
      IF ( l .gt. 1 ) then
         l    = l - 1
         rra  = ra (l)
         iind = ind (l)
         ! in retirement-promotion phase.
      ELSE
         ! clear a space at the end of the array
         rra  = ra (ir)
         !
         iind = ind (ir)
         ! retire the top of the heap into it
         ra (ir) = ra (1)
         !
         ind (ir) = ind (1)
         ! decrease the size of the corporation
         ir = ir - 1
         ! done with the last promotion
         IF ( ir .eq. 1 ) then
            ! the least competent worker at all !
            ra (1)  = rra
            !
            ind (1) = iind
            exit sorting
         ENDIF
      ENDIF
      ! wheter in hiring or promotion phase, we
      i = l
      ! set up to place rra in its proper level
      j = l + l
      !
      DO while ( j .le. ir )
         IF ( j .lt. ir ) then
            ! compare to better underling
            IF ( hslt( ra (j),  ra (j + 1) ) ) then
               j = j + 1
            ENDIF
         ENDIF
         ! demote rra
         IF ( hslt( rra, ra (j) ) ) then
            ra (i) = ra (j)
            ind (i) = ind (j)
            i = j
            j = j + j
         ELSE
            ! set j to terminate do-while loop
            j = ir + 1
         ENDIF
      ENDDO
      ra (i) = rra
      ind (i) = iind

   END DO sorting
contains

   !  internal function
   !  compare two real number and return the result

   logical function hslt( a, b )
      double precision :: a, b
      IF( abs(a-b) <  eps ) then
         hslt = .false.
      ELSE
         hslt = ( a < b )
      end if
   end function hslt
   !
end subroutine hpsort_eps_epw
!
!
subroutine disper_approx(h,T,k,n,C,Cg,mn)
   integer, intent(in)                :: mn
   double precision, dimension(mn), intent(in)  :: h
   double precision, intent(in)                 :: T
   double precision, dimension(mn), intent(out) :: k,n,C,Cg

   double precision                             :: sigma,g,pi
   g=9.81d0
   pi=4.d0*atan(1.d0)
   sigma=2.d0*pi/T
   k = sigma**2/g*(1-exp(-(sigma*sqrt(h/g))**(2.5)))**(-0.4)
   C= sigma/k
   n = 0.5d0+k*h/sinh(2*k*h)
   Cg=n*C

end subroutine disper_approx
!
subroutine fm_surrounding_points(xn,yn,no_nodes,connected_nodes,no_connected_nodes, no_cells, kp, ierr)
   use m_flowgeom
   use m_flow, only: hs, hu, epshu
   
   implicit none

   double precision,  dimension(no_nodes),          intent(in)  :: xn,yn              ! coordinates of network nodes
   integer,                                         intent(in)  :: no_nodes           ! number of network nodes
   integer,                                         intent(in)  :: no_connected_nodes ! max node numbers connected to each cell
   integer, dimension(no_cells,no_connected_nodes), intent(in)  :: connected_nodes    ! node numbers connected to each cell
   integer,                                         intent(in)  :: no_cells           ! number of cells
   integer, dimension(no_nodes,12),                 intent(out) :: kp                 ! sorted surrounding node numbers for each node
   integer,                                         intent(out) :: ierr
   
   ! Local variables
   integer, dimension(:),   allocatable         :: no_connected_cells
   integer, dimension(:,:), allocatable         :: connected_cells
   integer, dimension(4)                        :: kpts, edge
   integer, dimension(20)                       :: surr_points
   integer, dimension(12)                       :: surr_pts
   double precision,  dimension(12)             :: xp,yp,zp              ! x,y,z of sorted surrounding nodes for each node

   integer                                      :: k, k1, k2, k3, k4
   integer                                      :: ip, j, jj
   integer                                      :: inode, knode, kcell, kn, isp, isp2
   integer                                      :: next
   integer                                      :: L
   double precision                             :: dhds
   
   ierr = 1
   
   if (allocated(no_connected_cells)) deallocate(no_connected_cells, connected_cells)
   allocate(no_connected_cells(no_nodes))
   allocate(connected_cells(no_nodes,12))
   no_connected_cells=0
   connected_cells=0
   kp=0

   do k=1,no_cells
       do inode=1,no_connected_nodes
           knode=connected_nodes(k,inode)
           if (knode >0) then
               no_connected_cells(knode)=no_connected_cells(knode)+1
               connected_cells(knode,no_connected_cells(knode))=k
           endif
       enddo
   enddo

   do kn=1,no_nodes                                ! for each node (numk)
       surr_points=0
       surr_pts=0
       isp=1
       do kcell=1,no_connected_cells(kn)           ! for each cell connected to node
           k=connected_cells(kn,kcell)             ! get cell number
           kpts=connected_nodes(k,:)               ! all nodes in that cell
           if (kpts(4)==0) then
               jj=0
               do j=1,3
                   if (kpts(j)/=kn) then
                       jj=jj+1
                       edge(jj)=kpts(j)
                   endif
               enddo
               surr_points(isp:isp+1)=edge(1:2)
               isp=isp+2
           else
               ip=minloc(abs(kpts-kn),1)
               if (ip==1) then
                   edge=[kpts(2),kpts(3),kpts(3),kpts(4)]
               elseif (ip==2) then
                   edge=[kpts(3),kpts(4),kpts(4),kpts(1)]
               elseif (ip==3) then
                   edge=[kpts(4),kpts(1),kpts(1),kpts(2)]
               else
                   edge=[kpts(1),kpts(2),kpts(2),kpts(3)]
               endif
               surr_points(isp:isp+3)=edge(1:4)
               isp=isp+4
           endif
       enddo
       isp=isp-1  ! number of surrounding points
   !   now connect the edges
       if (isp>=2) then
          surr_pts(1:2)=surr_points(1:2)
          surr_points=[surr_points(3:),0,0]
          isp2=2
          isp=isp-2
          do while (isp>=2)
               call findlocpos(surr_points,isp,surr_pts(isp2),next)
               if (next/=-1) then
                  if (mod(next,2)==1) then
                      surr_pts(1:isp2+1)=[surr_pts(1:isp2),surr_points(next+1)]
                      surr_points=[surr_points(1:next-1),surr_points(next+2:)]
                  else
                      surr_pts(1:isp2+1)=[surr_pts(1:isp2),surr_points(next-1)]
                      surr_points=[surr_points(1:next-2),surr_points(next+1:)]
                  endif
               else
                  call findlocpos(surr_points,isp,surr_pts(1),next)
                  if (mod(next,2)==1) then
                      surr_pts(1:isp2+1)=[surr_points(next+1),surr_pts(1:isp2)]
                      surr_points=[surr_points(1:next-1),surr_points(next+2:)]
                  else
                      surr_pts(1:isp2+1)=[surr_points(next-1),surr_pts(1:isp2)]
                      surr_points=[surr_points(1:next-2),surr_points(next+1:)]
                  endif
   
               endif
            
               isp2=isp2+1      
               isp=isp-2
           end do
       else
           surr_pts=0
       endif
       kp(kn,:)=surr_pts
    
   end do

   ierr = 0
1234 continue
     return
   
end
!    
subroutine findlocpos(a,n,b,indx)

   implicit none

   integer, intent(in)               :: n,b
   integer, intent(out)              :: indx
   integer, dimension(n), intent(in) :: a
   
   integer                           :: i
   
   indx=-1
   do i=1,n
       if (a(i)==b) then
           indx=i
           return
       endif
   enddo
end   
   
subroutine fill_connected_nodes(ierr)
   use network_data
   use m_xbeach_data
   
   implicit none
   
   integer, intent(out)    :: ierr
   
   integer                                :: L, k, kk
   
   ierr = 1
   ! get maximum number of connected nodes over model domain
   no_connected_nodes = 0
   do k = 1, nump
      no_connected_nodes = max(no_connected_nodes,netcell(k)%n)
   enddo
   
   no_connected_nodes = no_connected_nodes+1                     ! possible ghost node
   if (allocated(connected_nodes)) deallocate(connected_nodes)
   allocate(connected_nodes(nump,no_connected_nodes))
   connected_nodes = 0
   
   ! fill array with netnodes per netcell
   do k = 1, nump
      do kk = 1, netcell(k)%n
         connected_nodes(k,kk) = netcell(k)%nod(kk)   
      end do
   enddo
   
   ierr = 0
1234 continue
   return  

   end subroutine fill_connected_nodes
   
   !> Interpolate zbndw values to boundary net nodes
   !  Not needed for now, e01 is available and spatially uniform
   !  This needs to be written when I change the wave bc module to accept anything
!   subroutine apply_stat_wavebc(ee, wmask, zbndw, nbndw, numk, ntheta, ierr)
!      
!      implicit none
!      
!      integer, intent(in)                   :: numk
!      integer, intent(in)                   :: ntheta
!      integer, intent(in)                   :: nbndw
!      double precision, dimension(ntheta)
!      
!      double precision
!      
!      integer                            :: k, kb, ki, L
!      
!      ierr = 1
!      
!      
!      ierr = 0
!1234  continue
!      return
!      
!   end subroutine apply_stat_wavebc
   
   subroutine getbndwzcornerpts(ierr)
      use m_flowexternalforcings
      use m_flowgeom
      use network_data
      use m_xbeach_data, only: kbndu2kbndw, kbndz2kbndw, noseapts, seapts, wmask, nmmask, inner
      use gridoperations, only: othernode
   
      implicit none
   
      integer                                   , intent(out)     :: ierr
                                                  
      ! local variables                           
      integer                                                     :: cn1, cn2
      integer                                                     :: k, ko
      integer                                                     :: L, LL
      integer                                                     :: cnt
      integer                                                     :: iwalls
                                                                  
      double precision, dimension(:), allocatable                 :: sumw
      
      ierr = 1
      ! 1: weight 1 of attached wave bnd wmask(2,L), 3: weight 2 of attached bnd of attached wave bnd wmask(4,L)
      wmask = 0
      
      ! bnd point connected to inner point of neumann bnd;
      nmmask = 0
      
      if (allocated(sumw)) deallocate(sumw)
      allocate(sumw(numk))
      sumw = 0d0
      
      !> wave energy boundaries
      !  set mask
      do k = 1,nbndw
         L   = kbndw(3,k)
         cn1 = lncn(1,L)
         cn2 = lncn(2,L)
         wmask(1,cn1) = wcLn(1,L); wmask(2,cn1)=k
         wmask(3,cn2) = wcLn(2,L); wmask(4,cn2)=k
         ! debug
         inner(cn1) = .false.
         inner(cn2) = .false.
         !\ debug
      enddo
      !
      ! Normalize weights
      do k = 1, numk
         sumw = wmask(1,k) + wmask(3,k)
         if (sumw(k)==0d0) cycle
         wmask(1,k) = wmask(1,k)/sumw(k)
         wmask(3,k) = wmask(3,k)/sumw(k)
      enddo
      !
      !> get indexes of edge nodes with wave boundary
      noseapts = 0
      do k = 1, numk
         if (wmask(2,k)>0 .or. wmask(4,k)>0) noseapts = noseapts+1    
      enddo
      !
      ! allocate seapts array and fill
      if (allocated(seapts)) deallocate(seapts)
      allocate(seapts(noseapts))
      cnt = 0
      do k = 1, numk
         if (wmask(2,k)>0 .or. wmask(4,k)>0) then
            cnt = cnt+1
            seapts(cnt) = k
         endif   
      enddo        
 
      !> non-wave open u and z boundaries get neumann flag
      do k = 1,nbndz
         if (kbndz2kbndw(k)/=0) cycle      ! then wave dirichlet bnd
         L   = kbndz(3,k)
         cn1 = lncn(1,L)
         cn2 = lncn(2,L)
         !
         ! Find internal node attached to bnd node (implicitly assumes no triangular borders, if so takes last one)
         !! network_data::NB values: 1=INTERN, 2=RAND, 3=HOEK, 0/-1=DOET NIET MEE OF 1D
         !  (KN(3,L) == 2) 2D
         do LL = 1, size(nod(cn1)%lin)
            if (kn(3,abs(nod(cn1)%lin(LL)))/=2) cycle       ! not 2d link
            call othernode(cn1,abs(nod(cn1)%lin(LL)),ko)
            if (nb(ko) /= 1) cycle                          ! not inner net node
            nmmask(ko) = cn1
         enddo
         !
         do LL = 1, size(nod(cn2)%lin)
            if (kn(3,abs(nod(cn2)%lin(LL)))/=2) cycle
            call othernode(cn2,abs(nod(cn2)%lin(LL)),ko)
            if (nb(ko) /= 1) cycle
            nmmask(ko) = cn2
         enddo
      enddo
      !
      do k = 1,nbndu
         if (kbndu2kbndw(k)/=0) cycle      ! then wave dirichlet bnd
         L = kbndu(3,k)
         cn1 = lncn(1,L)
         cn2 = lncn(2,L)
         !
         ! Find internal node
         do LL = 1, size(nod(cn1)%lin)
            if (kn(3,abs(nod(cn1)%lin(LL)))/=2) cycle
            call othernode(cn1,abs(nod(cn1)%lin(LL)),ko)
            if (nb(ko) /= 1) cycle
            nmmask(ko) = cn1
         enddo
         !
         do LL = 1, size(nod(cn2)%lin)
            if (kn(3,abs(nod(cn2)%lin(LL)))/=2) cycle
            call othernode(cn2,abs(nod(cn2)%lin(LL)),ko)
            if (nb(ko) /= 1) cycle
            nmmask(ko) = cn2
         enddo
      enddo
      !
      ! walls: could be simpler using attached links, but not here for consistency
!      do iwalls = 1, mxwalls
!         cn1  = walls(2,iwalls)                              ! first corner
!         cn2  = walls(3,iwalls)                              ! second corner
!         ! Find internal node
!         if (wmask(2,cn1)>0 .or. wmask(4,cn1)>0) goto 10     ! do next node
!         do LL = 1, size(nod(cn1)%lin)
!            if (kn(3,abs(nod(cn1)%lin(LL)))/=2) cycle
!            call othernode(cn1,abs(nod(cn1)%lin(LL)),ko)
!            if (nb(ko) /= 1) cycle
!            nmmask(ko) = cn1
!         enddo
!         !
!10       if (wmask(2,cn2)>0 .or. wmask(4,cn2)>0) cycle    
!         do LL = 1, size(nod(cn2)%lin)
!            if (kn(3,abs(nod(cn2)%lin(LL)))/=2) cycle
!            call othernode(cn2,abs(nod(cn2)%lin(LL)),ko)
!            if (nb(ko) /= 1) cycle
!            nmmask(ko) = cn2
!         enddo
!      enddo   
      
      ierr = 0
1234  continue
      if (allocated(sumw)) deallocate(sumw)
      return
   end subroutine getbndwzcornerpts
   
   subroutine allocstatsolverarrays(ierr)
      use m_alloc
      use m_xbeach_data
      use m_flowgeom, only: ntheta
      use network_data, only: numk
       
      implicit none
      
      integer                  :: ierr
      
      ierr = 1
            
      call realloc(w, (/numk,ntheta,2/), stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('w  (numk,ntheta,2)', ierr, ntheta*numk*2)
      call realloc(ds, (/numk,ntheta/), stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('ds  (numk,ntheta,2)', ierr, ntheta*numk)
      call realloc(inner, numk, stat=ierr, keepExisting = .false., fill = .false.)
      call aerr('inner  (numk)', ierr, numk)
      call realloc(prev, (/numk,ntheta,2/), stat=ierr, keepExisting = .false., fill = 0)
      call aerr('prev  (numk,ntheta,2)', ierr, ntheta*numk*2)      
      call realloc(hhstat, numk, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('hhstat  (numk)', ierr, numk)       
      call realloc(kwavstat, numk, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('kwavstat  (numk)', ierr, numk)         
      call realloc(cgstat, numk, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('cgstat  (numk)', ierr, numk)      
      call realloc(cstat, numk, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('cstat  (numk)', ierr, numk)
      call realloc(cthetastat, (/numk,ntheta/), stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('cthetastat  (numk,ntheta)', ierr, numk*ntheta)
      call realloc(eestat, (/numk,ntheta/), stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('eestat  (numk,ntheta)', ierr, numk*ntheta)        
      call realloc(fwstat, numk, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('fwstat  (numk)', ierr, numk)   
      call realloc(Hstat, numk, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('Hstat  (numk)', ierr, numk)       
      call realloc(Dwstat, numk, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('Dwstat  (numk)', ierr, numk)   
      call realloc(Dfstat, numk, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('Dfstat  (numk)', ierr, numk)       
      call realloc(thetam, numk, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('thetam  (numk)', ierr, numk)  
      call realloc(uorbstat, numk, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('uorbstat  (numk)', ierr, numk)       
      call realloc(dhdxstat, numk, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('dhdxstat  (numk)', ierr, numk) 
      call realloc(dhdystat, numk, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('dhdystat  (numk)', ierr, numk)  
      call realloc(dhdystat, numk, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('dhdystat  (numk)', ierr, numk)
      call realloc(wmask, (/4,numk/), stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('wmask  (4,numk)', ierr, 4*numk) 
      call realloc(nmmask, numk, stat=ierr, keepExisting = .false., fill = 0)
      call aerr('nmmask  (numk)', ierr, numk)
      if (ierr>0) goto 1234
      ierr = 0
1234  continue
      if (ierr>0) write(*,*) 'allocstatsolverarrays :: Error.'
      return 
   end subroutine allocstatsolverarrays
   
   subroutine corner2flownod(quantin, numk, quantout, ndxi, ndx, skipnbndw, ierr)
      use m_flowgeom, only: banf, ba, mxban, nban
      use m_flowexternalforcings
      use m_xbeach_data, only: kbndz2kbndw, kbndu2kbndw
   
      implicit none
      
      logical                          , intent(in)   :: skipnbndw
      integer                          , intent(in)   :: numk
      integer                          , intent(in)   :: ndxi
      integer                          , intent(in)   :: ndx
      double precision, dimension(numk), intent(in)   :: quantin
      double precision, dimension(ndx) , intent(out)  :: quantout
      integer, intent(out)                            :: ierr
      
      ! local variables
      integer                                         :: k, kb, ki
      integer                                         :: n, mx
      
      ierr = 1
      
      quantout = 0d0
      do mx = 1, mxban
         k   = nban(1,mx)    ! corner
         n   = nban(2,mx)    ! cell centre
         !
         quantout(n) = quantout(n) + banf(mx)*quantin(k)
      enddo   
      
      do k = 1, ndxi
         quantout(k) = quantout(k)/ba(k)
      enddo
      
      ! open boundaries that are not wave energy bnds get inner domain value
      do k = 1, nbndz
         if (skipnbndw .and. allocated(zbndw)) then
            if (kbndz2kbndw(k)>0) cycle
         endif   
         kb = kbndz(1,k)
         ki = kbndz(2,k)
         quantout(kb) = quantout(ki)
      enddo 
      
      do k = 1, nbndu
         if (skipnbndw .and. allocated(zbndw)) then
            if (kbndu2kbndw(k)>0) cycle
         endif
         kb = kbndu(1,k)
         ki = kbndu(2,k)
         quantout(kb) = quantout(ki)
      enddo 
      
      ierr = 0
1234  continue
      return
   
   end subroutine corner2flownod
   
   subroutine flownod2corner(quantin, ndx, quantout, numk, ierr)
      use m_flowgeom, only: ban, banf, mxban, nban
   
      implicit none
      
      integer,                           intent(in)    :: ndx
      integer,                           intent(in)    :: numk
      double precision, dimension(ndx),  intent(in)    :: quantin
      double precision, dimension(numk), intent(out)   :: quantout
      integer,                           intent(out)   :: ierr

      ! local variables
      integer                                         :: k, n, mx
      
      ierr = 1
      
      quantout = 0d0
      do mx = 1, mxban
         k   = nban(1,mx)    ! corner
         n   = nban(2,mx)    ! cell centre
         !
         quantout(k) = quantout(k) + banf(mx)*quantin(n)
      enddo 
      
      do k = 1, numk
         quantout(k) = quantout(k)/ban(k)
      enddo      
      
      ierr = 0
1234  continue
      return
   
   end subroutine flownod2corner
   
   subroutine getwaterdepthgradient(ierr)
      use network_data
      use m_flowgeom
      use m_flow, only: hu, s1, epshu
      use m_xbeach_data, only: dhdxstat, dhdystat
   
      implicit none
      
      integer, intent(out)   :: ierr
      
      ! local variables
      integer            :: k1, k2, k3, k4
      integer            :: L
      double precision   :: dhds, hs1, hs2
      
      ierr = 1
      
      ! Calculate water depth gradient in net nodes
      dhdxstat = 0d0
      dhdystat = 0d0
      do L = 1,lnx
         if (hu(L)<epshu) cycle
         k1 = ln(1,L); k2 = ln(2,L)
         k3 = lncn(1,L); k4 = lncn(2,L)
         hs1=s1(k1)-bl(k1); hs2=s1(k2)-bl(k2)
         dhds = (hs2 - hs1)*dxi(L)  ! dhds
         dhdxstat(k3) = dhdxstat(k3) + wcnx3(L)*dhds
         dhdxstat(k4) = dhdxstat(k4) + wcnx4(L)*dhds
         dhdystat(k3) = dhdystat(k3) + wcny3(L)*dhds
         dhdystat(k4) = dhdystat(k4) + wcny4(L)*dhds
      enddo
      
      ierr = 0
1234  continue
      return
      
   end subroutine getwaterdepthgradient

   subroutine xbeach_wave_compute_statcelerities(ierr)
      use m_xbeach_data
      use m_flow, only: s1
      use m_flowgeom
      use network_data
      use m_sferic, only: pi
      
      implicit none
      
      integer, intent(out)      :: ierr
      
      ! local variables
      integer                                        :: itheta
      double precision                               :: sigm
      double precision, allocatable, dimension(:)    :: nwavstat
      double precision, allocatable, dimension(:)    :: hh
      double precision, dimension(ntheta)            :: sint
      double precision, dimension(ntheta)            :: cost
      
      ierr = 1
      
      if (allocated(nwavstat)) deallocate(nwavstat, stat=ierr)
      allocate(nwavstat(numk))
      nwavstat = 0d0
      
      if (allocated(hh)) deallocate(hh, stat=ierr)
      allocate(hh(ndx))
      hh = s1-bl
      
      call getwaterdepthgradient(ierr)
      call flownod2corner(hh, ndx, hhstat, numk, ierr)
      hhstat=max(hhstat,hminlw)
      ! 
      ! Compute celerities and refraction speed
      call disper_approx(hhstat,Trep,kwavstat,nwavstat,cstat,cgstat,numk)
      sint=sin(thetabin)
      cost=cos(thetabin)
      sigm=2.d0*pi/Trep
      do itheta=1,ntheta
         cthetastat(:,itheta)= sigm/sinh(2.d0*kwavstat*hhstat)*(dhdxstat*sint(itheta)-dhdystat*cost(itheta))
      enddo      
      
      ierr = 0
1234  continue
      return
   end subroutine xbeach_wave_compute_statcelerities
   
   subroutine timer(t)
      double precision,intent(out)               :: t
      integer                                    :: count,count_rate,count_max
      call system_clock (count,count_rate,count_max)
      t = dble(count)/count_rate
   end subroutine timer
