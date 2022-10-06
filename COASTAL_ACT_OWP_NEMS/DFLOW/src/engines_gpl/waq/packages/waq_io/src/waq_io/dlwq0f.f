!!  Copyright (C)  Stichting Deltares, 2012-2022.
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

      subroutine dlwq0f ( noq1   , noq2   , noq3   , noseg  , ipoint ,
     &                    nomat  )

!       Deltares Software Centre

!>\file
!>            Compute size of the fast solver matrix
!>
!>    KHT      note that open boundaries (negative pointers) do not yield/n
!>    KHT      off-diagonal elements. This is correct for the moment but needs/n
!>    KHT      to be changed (in future) for (advanced) domain decomposition purposes/n

!     Created             : September 1996 by Leo Postma

!     Modified            : May       2011 by Leo Postma  : Fortran90 look and feel

!     Logical unitnumberS : none

!     Subroutines called  : none

      use timers       !   performance timers

      implicit none

!     Parameters         :

!     kind         function         name                Descriptipon

      integer( 4), intent(in   ) :: noq1              !< nr of exchanges first direction
      integer( 4), intent(in   ) :: noq2              !< nr of exchanges second direction
      integer( 4), intent(in   ) :: noq3              !< nr of exchanges third direction
      integer( 4), intent(in   ) :: noseg             !< nr of computational volumes
      integer( 4), intent(in   ) :: ipoint(4,noq1+noq2+noq3)  !< exchange pointer
      integer( 4), intent(  out) :: nomat             !< size of the fast solver matrix

!     Local

      integer( 4) iq           ! loop counter exchanges
      integer( 4) ifrom, ito   ! help variables exchanges
      integer(4) :: ithndl = 0
      if (timon) call timstrt( "dlwq0f", ithndl )


!         compute number of offdiagonals to be stored in the matrix

      nomat = 0
      do iq = 1, noq1+noq2
         ifrom = ipoint(1,iq)
         ito   = ipoint(2,iq)
         if ( ifrom .eq. 0 .or. ito .eq. 0 ) cycle
         if ( ifrom .gt. 0 ) nomat = nomat + 1
         if ( ito   .gt. 0 ) nomat = nomat + 1
      enddo

!         see if there is a third direction

      if ( noq3 .ne. 0 ) nomat = nomat + 2*noseg

      if (timon) call timstop( ithndl )
      return
      end
