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

      subroutine opt2 ( iopt2  , array  , nitem  , nvals  , nscale ,
     &                  iwidth , lun1   , ioutpt , ierr   )

!       Deltares Software Centre

!>\file
!>                          Reads a block with constant data with and without defaults
!>\par  Description:
!>                          Function depends on value of iopt2
!>                          - if 1:
!>                            - nscale scale values
!>                            - nitem sets of nvals values
!>                          - if 2:
!>                            - nscale scale values
!>                            - nvals dfault values
!>                            - number of overridings
!>                            - that many integers + sets of nvals values
!>                          If lun1 is positive the array are written

!     Created            : April 1988   BY  M.E.Sileon

!     Modified           : April 1997 by R. Bruinsma : Tokenized input data file reading added
!                          May   2011 by Leo Postma  : Fortran90 look and feel
!                                                      Removed workspace from parameter list

!     Subroutines called : scale  : to scale the matrix with the sacle factors

!     Functions            gettok : tokenized input data file reading

!     Logical units      : LUNIN = unit input file
!                          LUNUT = unit formatted output file
!                          LUN1  = unit intermediate file ( system )
      use timers       !   performance timers
      use rd_token

      implicit none

!     Parameters

!     kind           function         name         Descriptipon

      integer  ( 4), intent(in   ) :: iopt2      !< input option
      real      (4), intent(  out) :: array(nvals,nitem) !< array for the values
      integer  ( 4), intent(in   ) :: nitem      !< number of items to read
      integer  ( 4), intent(in   ) :: nvals      !< number of values per item
      integer  ( 4), intent(in   ) :: nscale     !< number of scale values
      integer  ( 4), intent(in   ) :: iwidth     !< width of the output file
      integer  ( 4), intent(in   ) :: lun1       !< output unit number
      integer  ( 4), intent(in   ) :: ioutpt     !< how extensive the output ?
      integer  ( 4), intent(inout) :: ierr       !< cumulative rror counter

!     local decalations

      real   (4), allocatable :: factor( : )       !  array for scale factors
      real   (4)                 value             !  help variable values
      integer(4)                 nover             !  number of overridings
      integer(4)                 iscal             !  loop counter scale values
      integer(4)                 item              !  loop counter items
      integer(4)                 ival              !  loop counter values
      integer(4)                 iw                !  loop counter print blocks
      integer(4)                 iover             !  loop counter overridings
      integer(4)                 ie1, ie2          !  limits of print blocks
      integer(4)                 ierr2             !  local error variable
      integer(4) :: ithndl = 0

      if ( nitem .eq. 0 ) return                   !  no items specified
      if (timon) call timstrt( "read_opt2", ithndl )

      allocate ( factor(nvals), stat=ierr2 )
      if ( ierr2 .ne. 0 ) then
         write ( lunut, 2000 ) nvals
         goto 100
      endif

      do iscal = 1, nscale
         if ( gettoken( factor(iscal), ierr2 ) .gt. 0 ) goto 100
      enddo

      select case ( iopt2 )

         case ( 1 )                    !   read constant items without defaults
            write ( lunut, 2010 )
            do item = 1, nitem
               do ival = 1, nvals
                  if ( gettoken( array(ival,item), ierr2 ) .gt. 0 ) goto 100
               enddo
            enddo
            if ( ioutpt .lt. 4 ) write ( lunut, 2020 )
            do iw = 1, nvals, iwidth
               ie1 = min(iw+iwidth-1,nscale)
               ie2 = min(iw+iwidth-1,nvals )
               if ( ioutpt .ge. 4 ) then
                  write ( lunut, 2030 )          (        ival      , ival = iw, ie1 )
                  write ( lunut, 2040 )          ( factor(ival)     , ival = iw, ie1 )
                  write ( lunut, 2050 )
                  do item = 1, nitem
                     write ( lunut, 2060 ) item, ( array (ival,item), ival = iw, ie2 )
                  enddo
               endif
            enddo

         case ( 2 )                    !   Read constant items with defaults
            write ( lunut, 2070 )
            do ival = 1, nvals
               if ( gettoken( array(ival,1), ierr2 ) .gt. 0 ) goto 100
            enddo
            if ( ioutpt .lt. 3 ) write ( lunut, 2080 )
            do iw = 1, nvals, iwidth
               ie1 = min(iw+iwidth-1,nscale)
               ie2 = min(iw+iwidth-1,nvals )
               if ( ioutpt .ge. 3 ) then
                  write ( lunut, 2030 ) (         ival   , ival = iw, ie1 )
                  write ( lunut, 2040 ) (  factor(ival  ), ival = iw, ie1 )
                  write ( lunut, 2090 )
                  write ( lunut, 2040 ) (  array (ival,1), ival = iw, ie2 )
               endif
            enddo
            do ival = 1,nvals
               value = array( ival, 1 )
               do item = 2, nitem
                  array( ival, item ) = value
               enddo
            enddo

!           Read overridings of the constant values

            if ( gettoken( nover, ierr2 ) .gt. 0 ) goto 100
            write ( lunut, 2100 ) nover
            if ( nover .gt. 0 .and. ioutpt .ge. 3) write ( lunut, 2110 )
            do iover = 1, nover
               if ( gettoken( item, ierr2 ) .gt. 0 ) goto 100
               if ( item .lt. 1 .or. item .gt. nitem ) then
                  if ( ioutpt .ge. 3 ) write ( lunut, 2120 ) item, 1, nitem
                  ierr = ierr + 1
                  do ival = 1, nvals
                     if ( gettoken( value, ierr2 ) .gt. 0 ) goto 100
                  enddo
               else
                  do ival = 1, nvals
                     if ( gettoken( array(ival,item), ierr2 ) .gt. 0 ) goto 100
                  enddo
                  if ( ioutpt .ge. 3 )
     &                write ( lunut, 2130 ) item,(array(ival,item),ival=1,nvals)
               endif
            enddo

      end select

!     Scale the values

      if ( nscale .eq. 1 ) then
         do iscal = 2, nvals
            factor(iscal) = factor(1)
         enddo
      endif
      call scale ( array , factor , nitem , nvals )

!     Write if unit specified

      if ( lun1 .gt. 0 ) write ( lun1 ) array
      if (timon) call timstop( ithndl )
      return

!     Errors and ends

  100 ierr = ierr + 1
      write ( lunut , 2140 )
      if (timon) call timstop( ithndl )
      return

!       Output formats

 2000 format (  /' ERROR allocating real array space of:', i10,' values.' )
 2010 format (   ' Constant values without defaults ',/ )
 2020 format (   ' Values will be printed for output option 4 and higher !' )
 2030 format (   ' Scale factors:',/,6X,10I12)
 2040 format (       12X,1P,10E12.4 )
 2050 format (   ' Unscaled values:' )
 2060 format (    I10,2X,1P,10E12.4 )
 2070 format (   ' Constant values with defaults ',/ )
 2080 format (   ' Values will be printed for output option 3 and higher !' )
 2090 format (   ' Unscaled default values:' )
 2100 format (   ' Number of overridings :',I5 )
 2110 format (   ' Number         values' )
 2120 format (     I6, ' ERROR, number too big or too small must be between ',I5,' and ',I5,' !!!' )
 2130 format (     I10,2X,1P,10E12.4,/(12X,10E12.4) )
 2140 format (   ' ERROR reading input!' )

      end
