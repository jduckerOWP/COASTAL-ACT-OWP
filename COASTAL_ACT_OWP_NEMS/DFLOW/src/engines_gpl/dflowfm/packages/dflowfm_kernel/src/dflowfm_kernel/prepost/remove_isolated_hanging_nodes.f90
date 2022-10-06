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

! $Id: remove_isolated_hanging_nodes.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_kernel/prepost/remove_isolated_hanging_nodes.f90 $

!> delete hanging nodes on net boundary
!>   and update netcell admin (no need for setnodadm)
subroutine remove_isolated_hanging_nodes(linkbrother, num)
   use m_netw
   use unstruc_messages

   implicit none

   integer, dimension(numL), intent(inout) :: linkbrother   !< brotherlink, that shares a (hanging) node, dim: numL
   integer,                  intent(out)   :: num           !< number of removed isolated hanging nodes

   integer                                 :: ierror  ! error (1) or not (0)

   integer                                 :: L, Lother, k, kother
   integer                                 :: ic, i, ii, ik, iL, kk, LL

   character(len=128)                      :: msg
   ierror = 1

   num = 0  ! number of removed hanging nodes

   do L=1,numL
!     check if link is 2D
      if ( kn(3,L).eq.2 ) then
         Lother = Linkbrother(L)
         if ( Lother.gt.0 ) then
!           check if other link is 2D
            if ( kn(3,L).eq.2 ) then

!              find common node
               call find_common_node(L,Lother,k)

!              check if node exists and if it is connected by two links only (an isolated hanging node)
               if ( k.gt.0) then
                 if (nmk(k).eq.2 ) then
!                 update netcell admin
                  do ii=1,lnn(L)
                     ic = lne(ii,L)

!                    safety check
                     if ( ic.ne.lne(1,Lother) .and. ic.ne.lne(min(2,lnn(Lother)),Lother) ) then
                        call mess(LEVEL_ERROR,'remove_isolated_hanging_nodes: error')
                        goto 1234
                     end if

                     iL = 0
                     ik = 0
                     do i=1,netcell(ic)%N
                        LL = netcell(ic)%lin(i)
                        if ( LL.ne.Lother ) then
                           iL = iL+1
                           netcell(ic)%lin(iL) = LL
                        end if

                        kk = netcell(ic)%nod(i)
                        if ( kk.ne.k ) then
                           ik = ik+1
                           netcell(ic)%nod(ik) = kk
                        end if
                     end do
                     netcell(ic)%N = netcell(ic)%N-1

   !                 safety check
                     if ( netcell(ic)%N.ne.iL .or. netcell(ic)%N.ne.ik ) then
                        call mess(LEVEL_ERROR,'remove_isolated_hanging_nodes: error')
                        goto 1234
                     end if
                  end do

!                 update lin admin
                  kother = kn(1,Lother) + kn(2,Lother) - k
                  if ( kn(1,L).eq.k ) then
                     kn(1,L) = kother
                  else
                     kn(2,L) = kother
                  end if

!                 change nod adm of other node
                  do ii=1,nmk(kother)
                     if ( nod(kother)%lin(ii).eq.Lother ) then
                        nod(kother)%lin(ii) = L
                        exit
                     end if
                  end do

!                 delete node
!                  call delnode(k)
                  nmk(k) = 0

!                 delete other link
                  kn(:,Lother) = 0
                  linkbrother(Lother) = 0
                  lnn(Lother) = 0

                  num = num+1
                 end if
               end if
            end if
         end if
      end if
   end do

   if ( num.gt.0 ) then
      write(msg,"('removed ', I0, ' isolated hanging nodes')") num
      call mess(LEVEL_INFO,trim(msg))

   end if

   ierror = 0

1234 continue

   return
end subroutine remove_isolated_hanging_nodes
