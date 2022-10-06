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

! $Id: m_fm_update_crosssections.f90 140618 2022-01-12 13:12:04Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_kernel/compute_sediment/m_fm_update_crosssections.f90 $

   module m_fm_update_crosssections
   implicit none
   contains

   !> Updates 1D cross-section profiles
   !!
   !! Updates cross-section based on main average bed level change.
   !! Returns bed level change for lowest point in cross-section.
   subroutine fm_update_crosssections(blchg)
   use precision
   use m_flowgeom, only: ndxi, kcs, dx, wu, nd, wu_mor, ba_mor, bai_mor, bl, ndx, acl, ndx2d
   use m_oned_functions, only:gridpoint2cross
   use unstruc_channel_flow, only: network, t_node, nt_LinkNode
   use m_CrossSections, only: t_CSType, CS_TABULATED
   use m_flow, only: s1
   use MessageHandling

   real(fp), dimension(:), intent(inout) :: blchg         !< Bed level change (> 0 = sedimentation, < 0 = erosion)

   integer :: i
   integer :: inod
   integer :: iref
   integer :: j
   integer :: l
   integer :: nm
   integer :: nm2
   integer :: nmm
   integer :: c
   integer :: ctype
   integer :: LL
   double precision :: aref
   double precision :: blmin
   double precision :: da
   double precision :: ds
   double precision :: dvol
   double precision :: fac
   double precision :: href
   double precision :: w_active
   type(t_CSType), pointer :: cdef
   type(t_node), pointer :: pnod
   !
   ! upon entry blchg contains the bed level change averaged over the total cell area
   !
   do nm = ndx2D + 1, ndxi ! only for internal 1D nodes
         do j = 1, gridpoint2cross(nm)%num_cross_sections
            c = gridpoint2cross(nm)%cross(j)
            if (c == -999) cycle
            cdef => network%crs%cross(c)%tabdef
            ctype = cdef%crosstype
            ds = fm_get_ds(nm,j)
            if (ctype == CS_TABULATED) then
               !
               ! determine the reference height href and the cross sectional area  below that level
               !
               iref = cdef%levelscount
               do i = 2, cdef%levelscount - 1
                  if ( comparereal(cdef%flowWidth(i+1), cdef%plains(1)) == 1 ) then ! or cdef%height(i)>s1(nm)
                     iref = i
                     exit
                  endif
               enddo
               aref = 0d0
               do i = 2, iref
                  aref = aref + (cdef%flowWidth(i) + cdef%flowWidth(i-1))*(cdef%height(i)-cdef%height(i-1))*0.5d0
               enddo
               href = cdef%height(iref)
               w_active = cdef%flowWidth(iref)
               !
               ! use blchg as bed level change over the total cell area (can be partly dry)
               ! to compute the total volume deposited inside the cell
               !
               dvol = blchg(nm)*ds*cdef%plains(1) ! relative volume (at links)
               !
               ! compute the deposited area per unit length, i.e. the area by which the cross section should be adjusted
               !
               da   = dvol/ds
               !
               ! compute the factor by which the cross sectional area changes
               !
               if (cdef%levelscount == 1) then
                  !
                  ! single level: horizontal bed, shift up/down uniformly
                  !
                  cdef%height(1) = href + (da/w_active)
               elseif (da<aref) then
                  !
                  ! raise/lower proportional to the depth relative to reference level
                  !
                  fac = 1d0 - (da/aref)
                  do i = 1, iref-1
                     cdef%height(i) = href - (href-cdef%height(i))*fac
                  enddo
               else
                  !
                  ! fill uniformly above reference level
                  !
                  do i = iref, cdef%levelscount-1
                     da = da - aref
                     aref = (cdef%flowWidth(i+1) + cdef%flowWidth(i))*(cdef%height(i+1)-cdef%height(i))*0.5d0
                     if (da<aref) then
                        exit
                     else
                        iref = iref+1
                        href = cdef%height(iref)
                     endif
                  enddo
                  !
                  ! remove obsolete levels
                  !
                  do i = iref, cdef%levelscount
                     cdef%flowWidth(i-iref+1) = cdef%flowWidth(i)
                     cdef%height(i-iref+1) = cdef%height(i)
                  enddo
                  cdef%levelscount = cdef%levelscount - iref + 1
                  !
                  ! fill proportional to the depth relative to reference level
                  !
                  fac = 1d0 - (da/aref)
                  do i = 1, iref-1
                     cdef%height(i) = href - (href-cdef%height(i))*fac
                  enddo
               endif
               !
               ! set blchg to bed level change for deepest point
               !
               network%crs%cross(c)%surfaceLevel = cdef%height(cdef%levelscount)
               if (gridpoint2cross(nm)%num_cross_sections == 1) then
                  blchg(nm) = cdef%height(1) - network%crs%cross(c)%bedLevel
               endif
               network%crs%cross(c)%bedLevel     = cdef%height(1) !TODO: check if we need to include network%crs%cross(c)%shift
               network%crs%cross(c)%charheight   = network%crs%cross(c)%surfaceLevel - network%crs%cross(c)%bedLevel
            else
               write(msgbuf,'(a,i5)') 'Bed level updating has not yet implemented for cross section type ',ctype
               call err_flush()
            endif
         enddo
   enddo
   !
   ! set blchg to bed level change for lowest point of incoming branches
   !
   ! loop over connection nodes
   do inod = 1, network%nds%Count
      pnod => network%nds%node(inod)
      if (pnod%nodeType == nt_LinkNode) then  ! connection node
         nm = pnod%gridnumber ! TODO: Not safe in parallel models (check gridpointsseq as introduced in UNST-5013)
         blmin = 999999d0
         do j = 1, gridpoint2cross(nm)%num_cross_sections
            c = gridpoint2cross(nm)%cross(j)
            if (c == -999) cycle
            cdef => network%crs%cross(c)%tabdef
            !LL = nd(nm)%ln(j)
            !L = iabs(LL)
            !if (LL < 0) then
            !   ds = dx(L)*acl(L)
            !else
            !   ds = dx(L)*(1d0 - acl(L))
            !endif
            blmin = min(blmin, network%crs%cross(c)%bedLevel)
         enddo
         blchg(nm) = blmin - bl(nm)
      endif
   enddo
   !
   ! upon exit blchg contains the bed level change for deepest point
   !
   end subroutine fm_update_crosssections

   !> Returns local grid cell length at a 1D computational node.
   !!
   !! Two possibilities:
   !! * For a point within a branch, between midpoints of the two neighbouring flowlinks.
   !! * At a connection node with multiple attached branches, the half-length of the
   !!   user-specified flow link index (i.e., only the requested branch).
   function fm_get_ds(nm, j) result (ds)
   use precision
   use m_oned_functions, only:gridpoint2cross
   use m_flowgeom, only: acl, dx, ln, nd ! lnx, lnx1d, lnxi, lnx1Db, wu, wu_mor, LBND1D, bai, ba_mor, bai_mor, ndx, ndx2D, ndx1Db

   integer, intent(in) :: nm   !< flow node index
   integer, intent(in) :: j    !< local link (branch) index at flow node nm (only used on points with multiple branches)
   double precision    :: ds   !< Resulting grid cell length.

   integer i
   integer LL
   integer L

   if (gridpoint2cross(nm)%num_cross_sections == 1) then
      !
      ! compute the total cell length
      !
      ds = 0d0
      do i = 1,nd(nm)%lnx
         LL = nd(nm)%ln(i)
         L = iabs(LL)
         if (LL < 0) then
            ds = ds+dx(L)*acl(L)
         else
            ds = ds+dx(L)*(1d0 - acl(L))
         endif
      enddo
   else
      LL = nd(nm)%ln(j)
      L = iabs(LL)
      if (LL < 0) then
         ds = dx(L)*acl(L)
      else
         ds = dx(L)*(1d0 - acl(L))
      endif
   endif

   end function fm_get_ds


   subroutine fm_update_mor_width_area()
   use m_flowgeom, only: lnx, lnx1d, lnxi, lnx1Db, wu, wu_mor, LBND1D, bai, ba_mor, bai_mor, ndx, dx, ln, acl, ndx2D, ndx1Db
   use m_cell_geometry, only: ba
   use unstruc_channel_flow, only: network
   use m_CrossSections, only: t_CSType, CS_TABULATED

   type(t_CSType), pointer :: cdef1, cdef2
   integer :: icrs1, icrs2
   integer :: L, LL, k, k1, k2
   double precision :: factor

   ! Set all morphologically active widths to general flow width.
   do L = 1, lnx
      wu_mor(L) = wu(L)
   enddo

   ! Set all morphologically active areas to general flow area and similar for the inverse
   do k = 1, ndx
      ba_mor(k) = ba(k)
      bai_mor(k) = bai(k)
   enddo

   if (network%loaded) then
      ! Replace morphologically active 1d widths by main channel width from cross-section.
      ! This could also be chosen as the minimum of the main channel width and the
      ! flow width (this is apparently how it was implemented in Sobek-RE).
      do L = 1, lnx1d
         factor = network%adm%line2cross(L,2)%f
         icrs1 = network%adm%line2cross(L,2)%c1
         icrs2 = network%adm%line2cross(L,2)%c2
         cdef1 => network%crs%cross(icrs1)%tabdef
         cdef2 => network%crs%cross(icrs2)%tabdef
         if (cdef1%crosstype == CS_TABULATED .and. cdef2%crosstype == CS_TABULATED) then
            wu_mor(L) = (1.0d0 - factor)*cdef1%plains(1) + factor*cdef2%plains(1)
         else
            wu_mor(L) = wu(L)
         endif
      enddo

      ! Overwrite boundary link morphologically active widths by morphologically active width on inside
      do L = lnxi+1, lnx1Db
         LL = LBND1D(L)
         wu_mor(L) = wu_mor(LL)
      enddo

      ! Compute morphologically active areas
      ba_mor(ndx2D+1:ndx1Db) = 0d0

      do L = 1, lnx1d
         k1 = ln(1,L)
         k2 = ln(2,L)
         ba_mor(k1) = ba_mor(k1) + dx(L)*wu_mor(L)*acl(L)
         ba_mor(k2) = ba_mor(k2) + dx(L)*wu_mor(L)*(1d0 - acl(L))
      enddo

      ! Compute morphologically active areas at boundary links
      do L = lnxi+1, lnx1Db
         k1 = ln(1,L)
         k2 = ln(2,L)
         ba_mor(k1) = dx(L)*wu_mor(L) ! area point outside
         ba_mor(k2) = ba_mor(k2) + dx(L)*wu_mor(L)*(1d0 - acl(L)) ! area point inside (added to loop above)
      enddo

      ! Compute inverse of morphologically active areas
      do k = ndx2D+1, ndx1Db
         bai_mor(k) = 1d0/ba_mor(k)
      enddo

   endif

   end subroutine fm_update_mor_width_area


   subroutine fm_update_mor_width_mean_bedlevel()
   use m_flowgeom, only: ndxi, bl, bl_ave, ndx, kcs, ndx2d, ba_mor, ndx1Db
   use m_oned_functions, only:gridpoint2cross
   use m_CrossSections, only: t_CSType, CS_TABULATED
   use m_flow, only: s1
   use precision
   use MessageHandling
   use unstruc_channel_flow, only: network
   use m_CrossSections, only: t_CSType, CS_TABULATED
   type(t_CSType), pointer :: cdef

   integer :: c
   integer :: ctype
   integer :: i
   integer :: iref
   integer :: j
   integer :: nm
   double precision :: blref
   double precision :: href
   double precision :: ba_mor_tot
   double precision :: href_tot
   double precision :: ds
   !
   ! Generate level change averaged over the main channel
   !
   do nm = ndx2D + 1, ndxi ! only for internal 1D nodes
         href_tot = 0d0
         ba_mor_tot = 0d0
         do j = 1, gridpoint2cross(nm)%num_cross_sections
            c = gridpoint2cross(nm)%cross(j)
            if (c == -999) cycle
            cdef => network%crs%cross(c)%tabdef
            ctype = cdef%crosstype
            if (ctype == CS_TABULATED) then
               ds = fm_get_ds(nm,j)
               !
               ! determine the reference height href and the cross sectional area below that level
               !
               iref = cdef%levelscount
               do i = 2, cdef%levelscount - 1
                  if ( comparereal(cdef%flowWidth(i+1), cdef%plains(1)) == 1 ) then ! or cdef%height(i)>s1(nm)
                     iref = i
                     exit
                  endif
               enddo
               blref = cdef%flowWidth(1)*cdef%height(1)
               do i = 2, iref
                  blref = blref + (cdef%flowWidth(i) - cdef%flowWidth(i-1))*(cdef%height(i)+cdef%height(i-1))*0.5d0
               enddo
               href_tot = href_tot + blref*ds
               ba_mor_tot = ba_mor_tot + cdef%flowWidth(iref)*ds
            else
               write(msgbuf,'(a,i5)') 'Bed level averaging for main channel is not implemented for cross section type ',ctype
               call err_flush()
            endif
         enddo
         bl_ave(nm) = href_tot/ba_mor_tot
   enddo

   do nm = 1, ndx2D  ! internal 2d nodes
      bl_ave(nm) = bl(nm)
   enddo

   do nm = ndx1Db, ndx ! boundary 2d nodes
      bl_ave(nm) = bl(nm)
   enddo

   end subroutine fm_update_mor_width_mean_bedlevel

   end module m_fm_update_crosssections
