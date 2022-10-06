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

! $Id: setpillars.f90 140692 2022-02-02 10:19:45Z goede $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_kernel/prepost/setpillars.f90 $

  ! =================================================================================================
  ! =================================================================================================
  subroutine setpillars()
    use m_flowgeom            , only: ndx, lnx, ba, wu, nd
    use m_flowexternalforcings, only: pillar, Cpil
    use m_vegetation          , only: rnveg, diaveg, stemheight
    use gridoperations
    use m_flowparameters      , only: japillar
    use m_crspath
    implicit none
    integer                                     :: i, j, k, L, Lf, La, m, n
    double precision                            :: pi
    integer         , dimension(:), allocatable :: npil
    double precision, dimension(:), allocatable :: cdeq
    double precision, dimension(:), allocatable :: Aeff
    integer         , dimension(:), allocatable :: linktype
    integer                                     :: nPath
    type (tcrspath) , dimension(:), allocatable :: Path
    integer         , dimension(:), allocatable :: idum

    if (allocated(Cpil)) deallocate( Cpil )
    if (japillar == 1) then
      allocate( Cpil(ndx) )
    else if (japillar == 3) then
      allocate( Cpil(lnx) )
    endif

    if (allocated(idum)) deallocate(idum)
    allocate(idum(1))
    idum = 0

    pi = 4.0d0 * atan( 1d0 )

    if( japillar == 2 ) then
      if( allocated( cdeq ) ) deallocate( cdeq, npil )
      allocate( cdeq(ndx), npil(ndx) )
      cdeq = 0d0
      npil = 0
      do m = 1,size(pillar)
        do i = 1,pillar(m)%np
          if( pillar(m)%dia(i) == -999d0 .or. pillar(m)%cd(i) == -999d0 ) cycle
          call incells( pillar(m)%xcor(i), pillar(m)%ycor(i), j )
          if( j == 0 ) cycle
          rnveg(j) = rnveg(j) + pillar(m)%dia(i)**2 * pi * 0.25d0 / ba(j)
          cdeq(j)  = cdeq(j)  + pillar(m)%cd(i) * pillar(m)%dia(i)
          npil(j) = npil(j) + 1
        enddo
      enddo
      do j = 1,ndx
        if( npil(j) == 0 ) cycle
        diaveg(j)  = diaveg(j) + cdeq(j) / npil(j)
        stemheight(j) = 1d30
      enddo
      deallocate( cdeq )
      deallocate( npil )

    elseif( japillar == 1 ) then   ! Delft3D implimentation, but modified version on flow cells
      if (allocated(Aeff) ) deallocate( Aeff, cdeq )
      allocate( Aeff(ndx), cdeq(ndx) )
      do j = 1,ndx
        Aeff(j) = ba(j)
      enddo
      cdeq = 0d0
      do m = 1,size(pillar)
        do i = 1,pillar(m)%np
          if (pillar(m)%dia(i) == -999d0 .or. pillar(m)%cd(i) == -999d0) cycle
          call incells( pillar(m)%xcor(i), pillar(m)%ycor(i), j )
          if (j == 0) cycle
          cdeq(j) = cdeq(j) + pillar(m)%cd(i) * pillar(m)%dia(i)
          Aeff(j) = Aeff(j) - pillar(m)%dia(i)**2 * pi * 0.25d0
        enddo
      enddo
      Cpil = 0d0
      do j = 1,ndx
        if( cdeq(j) == 0 ) cycle
        if( Aeff(j) <= 0d0 ) then
          Cpil(j) = 1d30
          cycle
        endif
        Cpil(j) = cdeq(j) * 0.25d0 / Aeff(j) * sqrt( ba(j) * pi )
      enddo
      deallocate( Aeff )
      deallocate( cdeq )

    else if (japillar == 3) then       ! Based on D3D approach on flow links
      if (allocated(Aeff) ) deallocate( Aeff, cdeq, linktype )
      allocate( Aeff(lnx), cdeq(lnx), linktype(lnx) )
      linktype = 0
      Aeff = wu
      cdeq = 0d0
      do m = 1,size(pillar)
        call pol_to_flowlinks(pillar(m)%xcor, pillar(m)%ycor, pillar(m)%xcor*0d0, pillar(m)%np, nPath, Path)
        do n = 1,nPath
          call crspath_on_flowgeom(Path(n),1,0,1,idum,0,1)
          do L = 1,Path(n)%lnx
            Lf = Path(n)%ln(L)
            La = iabs(Lf)
            linktype(La) = 1
          enddo
        enddo
        do i = 1,pillar(m)%np
          if (pillar(m)%dia(i) == -999d0 .or. pillar(m)%cd(i) == -999d0) cycle
          call incells( pillar(m)%xcor(i), pillar(m)%ycor(i), k )
          if( k == 0 ) cycle
          do L = 1,nd(k)%lnx
            Lf = nd(k)%ln(L)
            La = iabs(Lf)
            if (linktype(La) /= 1) cycle
            cdeq(La) = cdeq(La) + pillar(m)%cd(i) * pillar(m)%dia(i)
            Aeff(La) = Aeff(La) - pillar(m)%dia(i)
          enddo
        enddo
      enddo
      Cpil = 0d0
      do L = 1,lnx
        if( cdeq(L) == 0 ) cycle
        if( Aeff(L) <= 0d0 ) then
          Cpil(L) = 1d30
          cycle
        endif
        Cpil(L)  = cdeq(L) * 0.5d0 * wu(L) / Aeff(L)**2
      enddo
      deallocate( Aeff )
      deallocate( cdeq )
      deallocate( linktype )
    endif

  end subroutine setpillars
