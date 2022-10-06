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

! $Id: setsigmabnds.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_kernel/compute/setsigmabnds.f90 $

 subroutine setsigmabnds()
    use m_netw
    use m_flowgeom
    use m_flow
    use m_sediment, only: stm_included
    implicit none

    integer          :: i, k, ki, kb, kt, itrac, isf


 !   if (layertype == 2) return

    if ( kmx.eq.0 ) then   ! 2D, set dummy values
       if ( allocated(sigmabnds)  ) sigmabnds  = 0.5d0
       if ( allocated(sigmabndTM) ) sigmabndTM = 0.5d0
!      if ( allocated(sigmabndtr) ) sigmabndtr = 0.5d0
       if ( allocated(sigmabndu) ) sigmabndu = 0.5d0
    else                   ! 3D
       do i  = 1, nbnds
          ki = kbnds(2,i)
          call getkbotktop(ki,kb,kt)
             do k = kb, kt
             sigmabnds(kmx*(i-1)+k-kb+1) = (0.5d0*(zws(k-1)+zws(k))-zws(kb-1)) / max(epshs, (zws(kt)-zws(kb-1)) )
             end do
!            SPvdP: fill remainder
             do k=kt+1,kb+kmx-1
                sigmabnds(kmx*(i-1)+k-kb+1) = 1d0
             end do

          ! if ( zws(kt)-zws(kb-1) .gt. epshs ) then
          !   do k = kb, kt
          !      sigmabnds(kmx*(i-1)+k-kb+1) = (0.5d0*(zws(k-1)+zws(k))-zws(kb-1)) / (zws(kt)-zws(kb-1))
          !   end do
          !else  ! fix for dry points
          !   do k = kb, kt
          !     sigmabnds(kmx*(i-1)+k-kb+1) = dble(k-kb)/dble(kt-kb)  ! hk: this goes wrong if kt==kb
          !   end do
          !end if

       end do

       do i  = 1, nbndTM
          ki = kbndTM(2,i)
          call getkbotktop(ki,kb,kt)
             do k = kb, kt
             sigmabndTM(kmx*(i-1)+k-kb+1) = (0.5d0*(zws(k-1)+zws(k))-zws(kb-1)) / max(epshs, (zws(kt)-zws(kb-1)) )
             end do
!            SPvdP: fill remainder
             do k=kt+1,kb+kmx-1
                sigmabndTM(kmx*(i-1)+k-kb+1) = 1d0
             end do

          ! if ( zws(kt)-zws(kb-1) .gt. epshs ) then
          !   do k = kb, kt
          !      sigmabndTM(kmx*(i-1)+k-kb+1) = (0.5d0*(zws(k-1)+zws(k))-zws(kb-1)) / (zws(kt)-zws(kb-1))
          !   end do
          ! else  ! fix for dry points
          !   do k = kb, kt
          !     sigmabndTM(kmx*(i-1)+k-kb+1) = dble(k-kb)/dble(kt-kb)
          !   end do
          ! end if
       end do

       do i  = 1, nbndu
          ki = kbndu(2,i)
          call getkbotktop(ki,kb,kt)
          do k = kb, kt
          sigmabndu(kmx*(i-1)+k-kb+1) = (0.5d0*(zws(k-1)+zws(k))-zws(kb-1)) / max(epshs, (zws(kt)-zws(kb-1)) )
          end do
          do k=kt+1,kb+kmx-1
             sigmabndu(kmx*(i-1)+k-kb+1) = 1d0
          end do
       end do

       do i  = 1, nbnduxy
          ki = kbnduxy(2,i)
          call getkbotktop(ki,kb,kt)
             do k = kb, kt
             sigmabnduxy(kmx*(i-1)+k-kb+1) = (0.5d0*(zws(k-1)+zws(k))-zws(kb-1)) / max(epshs, (zws(kt)-zws(kb-1)) )
             end do
!            SPvdP: fill remainder
             do k=kt+1,kb+kmx-1
                sigmabnduxy(kmx*(i-1)+k-kb+1) = 1d0
             end do

          ! if ( zws(kt)-zws(kb-1) .gt. epshs ) then
          !   do k = kb, kt
          !      sigmabnduxy(kmx*(i-1)+k-kb+1) = (0.5d0*(zws(k-1)+zws(k))-zws(kb-1)) / (zws(kt)-zws(kb-1))
          !   end do
          ! else  ! fix for dry points
          !   do k = kb, kt
          !     sigmabnduxy(kmx*(i-1)+k-kb+1) = dble(k-kb)/dble(kt-kb)
          !   end do
          ! end if
       end do

       do itrac=1,numtracers
          do i=1,nbndtr(itrac)
             ki = bndtr(itrac)%k(2,i)
             call getkbotktop(ki,kb,kt)
             do k=kb,kt
                bndtr(itrac)%sigma(kmx*(i-1)+k-kb+1) = (0.5d0*(zws(k-1)+zws(k))-zws(kb-1)) / max(epshs, (zws(kt)-zws(kb-1)) )
             end do
!            SPvdP: fill remainder
             do k=kt+1,kb+kmx-1
                bndtr(itrac)%sigma(kmx*(i-1)+k-kb+1) = 1d0
             end do
          end do
       end do

       do i  = 1, nbndsd
          ki = kbndsd(2,i)
          call getkbotktop(ki,kb,kt)
             do k = kb, kt
                sigmabndsd(kmx*(i-1)+k-kb+1) = (0.5d0*(zws(k-1)+zws(k))-zws(kb-1)) / max(epshs, (zws(kt)-zws(kb-1)) )
             end do
             do k=kt+1,kb+kmx-1
                sigmabndsd(kmx*(i-1)+k-kb+1) = 1d0
             end do
       end do

       if ( stm_included ) then
          ! sediment boundaries
          do isf=1,numfracs
             do i=1,nbndsf(isf)
                ki = bndsf(isf)%k(2,i)
                call getkbotktop(ki,kb,kt)
                do k=kb,kt
                   bndsf(isf)%sigma(kmx*(i-1)+k-kb+1) = (0.5d0*(zws(k-1)+zws(k))-zws(kb-1)) / max(epshs, (zws(kt)-zws(kb-1)) )
                end do
                !            SPvdP: fill remainder
                do k=kt+1,kb+kmx-1
                   bndsf(isf)%sigma(kmx*(i-1)+k-kb+1) = 1d0
                end do
             end do
          end do
       end if
    end if

 end subroutine setsigmabnds
