!!  Copyright (C)  Stichting Deltares, 2021-2022.
!!
!!  This program is free software: you can redistribute it and/or modify
!!  it under the terms of the GNU General Public License version 3,
!!  as published by the Free Software Foundation.
!!
!!  This program is distributed in the hope that it will be useful,
!!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
!!  GNU General Public License for more details.
!!
!!  You should have received a copy of the GNU General Public License
!!  along with this program. If not, see <http://www.gnu.org/licenses/>.
!!
!!  contact: delft3d.support@deltares.nl
!!  Stichting Deltares
!!  P.O. Box 177
!!  2600 MH Delft, The Netherlands
!!
!!  All indications and logos of, and references to registered trademarks
!!  of Stichting Deltares remain the property of Stichting Deltares. All
!!  rights reserved.

      module domain_mod

      ! module contains everything for the domain discription
      ! created March 2005 by Jan van Beek

      implicit none

      integer, parameter, private               :: NAME_SIZE  =  256      ! size of descriptive names
      integer, parameter, private               :: MAX_NUM    =    5      ! allocated per bunch

      integer, parameter                        :: DD_RIGHT_LEFT = 1      ! direction of dd_boundary
      integer, parameter                        :: DD_LEFT_RIGHT = 2      ! direction of dd_boundary
      integer, parameter                        :: DD_TOP_BOTTOM = 3      ! direction of dd_boundary
      integer, parameter                        :: DD_BOTTOM_TOP = 4      ! direction of dd_boundary

      type t_domain
         character(len=NAME_SIZE)               :: name                   ! name of domain
         integer                                :: mmax                   ! domain size in the m direction
         integer                                :: nmax                   ! domain size in the n direction
         character(len=NAME_SIZE)               :: aggr                   ! aggregation file name
      end type t_domain

      type t_domain_coll
         type(t_domain), pointer                :: domain_pnts(:)         ! pointer to the domains
         integer                                :: maxsize                ! maximum size of the current array
         integer                                :: cursize                ! filled up to this size
      end type t_domain_coll

      type t_dd_bound
         character(len=NAME_SIZE)               :: name1                  ! name of domain 1
         integer                                :: i_domain1              ! index of domain 1
         integer                                :: refine1                ! refinement for domain 1 side
         integer                                :: m_begin1               ! begin m of boundary in domain 1
         integer                                :: n_begin1               ! begin n of boundary in domain 1
         integer                                :: m_end1                 ! end m of boundary in domain 1
         integer                                :: n_end1                 ! end n of boundary in domain 1
         character(len=NAME_SIZE)               :: name2                  ! name of domain 2
         integer                                :: i_domain2              ! index of domain 2
         integer                                :: refine2                ! refinement for domain 2 side
         integer                                :: m_begin2               ! begin m of boundary in domain 2
         integer                                :: n_begin2               ! begin n of boundary in domain 2
         integer                                :: m_end2                 ! end m of boundary in domain 2
         integer                                :: n_end2                 ! end n of boundary in domain 2
         integer                                :: direction              ! direction can be either DD_RIGHT_LEFT DD_LEFT_RIGHT DD_TOP_BOTTOM DD_BOTTOM_TOP
      end type t_dd_bound

      type t_dd_bound_coll
         type(t_dd_bound), pointer              :: dd_bound_pnts(:)       ! pointer to the dd_bound's
         integer                                :: maxsize                ! maximum size of the current array
         integer                                :: cursize                ! filled up to this size
      end type t_dd_bound_coll

      contains

      ! function to find a domain name in a collection, case sensitive at the moment

      function domain_coll_find( domain_coll, name ) result ( iret )

         type(t_domain_coll)                    :: domain_coll            ! collection of domains
         character(LEN=*)                       :: name                   ! name of domain to be found
         integer                                :: iret                   ! result index in collection or 0 if not found

         integer                                :: i                      ! loop counter

         iret = 0
         do i = 1 , domain_coll%cursize
            if ( domain_coll%domain_pnts(i)%name .eq. name ) then
               iret = i
               return
            endif
         end do

      end function domain_coll_find

      ! function to add to a collection of domains (copy)

      function domain_coll_add( domain_coll , domain ) result ( cursize )

         type(t_domain_coll)                    :: domain_coll            ! collection of domains
         type(t_domain)                         :: domain                 ! domain to be added
         integer                                :: cursize                ! return value the new current collection size
                                                                          ! and the index of the added domain

         type(t_domain), pointer                :: domain_pnts(:)         ! pointer for the resize operation
         integer                                :: i                      ! loop counter

         if ( domain_coll%cursize .eq. domain_coll%maxsize ) then

            ! resize, allocate new array

            allocate ( domain_pnts ( domain_coll%maxsize + MAX_NUM ) )

            ! copy the domains into the new array

            do i = 1 , domain_coll%maxsize
               domain_pnts(i) = domain_coll%domain_pnts(i)   ! copies the domains
            enddo

            ! deallocate the old array and attach the new array to the collection

            if ( domain_coll%maxsize .ne. 0 ) deallocate ( domain_coll%domain_pnts )
            domain_coll%domain_pnts => domain_pnts
            domain_coll%maxsize = domain_coll%maxsize + MAX_NUM

         endif

         domain_coll%cursize = domain_coll%cursize + 1
         domain_coll%domain_pnts(domain_coll%cursize ) = domain
         cursize = domain_coll%cursize
         return

      end function domain_coll_add

      ! function to add to a collection of dd_bound (copy)

      function dd_bound_coll_add( dd_bound_coll , dd_bound ) result ( cursize )

         type(t_dd_bound_coll)                  :: dd_bound_coll          ! collection of dd_bounds
         type(t_dd_bound)                       :: dd_bound               ! dd_bound to be added
         integer                                :: cursize                ! return value the new current collection size
                                                                          ! and the index of the added dd_bound

         type(t_dd_bound), pointer              :: dd_bound_pnts(:)       ! pointer for the resize operation
         integer                                :: i                      ! loop counter

         if ( dd_bound_coll%cursize .eq. dd_bound_coll%maxsize ) then

            ! resize, allocate new array

            allocate ( dd_bound_pnts ( dd_bound_coll%maxsize + MAX_NUM ) )

            ! copy the dd_bounds into the new array

            do i = 1 , dd_bound_coll%maxsize
               dd_bound_pnts(i) = dd_bound_coll%dd_bound_pnts(i)   ! copies the dd_bounds
            enddo

            ! deallocate the old array and attach the new array to the collection

            if ( dd_bound_coll%maxsize .ne. 0 ) deallocate ( dd_bound_coll%dd_bound_pnts )
            dd_bound_coll%dd_bound_pnts => dd_bound_pnts
            dd_bound_coll%maxsize = dd_bound_coll%maxsize + MAX_NUM

         endif

         dd_bound_coll%cursize = dd_bound_coll%cursize + 1
         dd_bound_coll%dd_bound_pnts(dd_bound_coll%cursize ) = dd_bound
         cursize = dd_bound_coll%cursize
         return

      end function dd_bound_coll_add

      end module domain_mod
