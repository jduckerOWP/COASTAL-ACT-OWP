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

module partur_mod
!
!  module declarations
!
!  data definition module(s)
!
use precision_part               ! single/double precision
      use timers
!
!  module procedure(s)
!
use mudrel_mod              ! explicit interface
!
implicit none               ! force explicit typing
!
contains
      subroutine partur ( itime  , noudef , iutime  , mpart   , npart  , &
                          kpart  , xpart  , ypart   , zpart   , wpart  , &
                          iptime , nopart , lgrid   , nmax    , mmax   , &
                          amasud , ipnt   , sname   , nosubs  , nolay  , &
                          nocont , ndprt  , nodye   , lun     , buffer , &
                          volume , aconud , uscal   , isub    , finam  , &
                          iftime , ifopt  , nosyss  , isfil   , nosubud, &
                          snamud   )
!
!
!                   Deltares (former: Deltares)
!
!                        d e l p a r    v3.30
!
!
!     system administration : r.j. vos
!
!
!     created               : july 1996, by r.j. vos
!
!     modified              : jan  1997, by r.j. vos  (version 3.23)
!                             general use for all modtyp's
!                             array isub is used to indicate substances
!
!     function              : adds user defined release to bunch of particles.
!
!     note                  : none.
!
!
!     logical unit numbers  : lun
!
!
!     subroutines called    : none
!
!
!     parameters            :
!
!     name    kind     length     funct.  description
!     ====    ====     ======     ======  ===========
!     aconud   real nosubs*noudef in/out  released mass per particle per load
!     amasud  real      noudef    input   released mass
!     buffer  real  nmax*mmax*nolay in/out scratch array
!     finam   char*256  noudef    input   name of the file for ud release
!     kpart   integer   nopart    output  k-values particles
!     ifopt   integer   noudef    input   when 1 from restart file/ 0 from mapfile
!     iftime  integer   noudef    input   time in seconds for file reading
!     ipnt    integer   nosegm    output  scratch array
!     iptime  integer   nopart    output  particle age
!     isub    integer   nosubs    input   substance numbers user def. releases
!     itime   integer     1       input   simulation time
!     iutime  integer   noudef    in/out  time of user defined release
!     lgrid   integer  nmax*mmax  input   active grid
!     lun     integer     1       input   unit report file
!     mpart   integer   nopart    output  m-values particles
!     nmax    integer     1       input   dimension of lgrid
!     nocont  integer     1       input   nr of dye release points
!     nocons  real        1       input   number of constants from input
!     nodye  integer     1       input   nr of continuous release points
!     nosyss   integer     1       input   number of substances on release file
!     noudef  integer     1       input   nr of user defined release points
!     nolay   integer     1       input   number of comp. layers
!     nopart  integer     1       in/out  number of active particles
!     nosubs  integer     1       input   number of substances
!     npart   integer   nopart    output  n-values particles
!     sname   char*20   nosubs    input   delpar substance names
!     volume  real nmax*mmmax*nolay input voluems of sgements
!     uscal   real      noudef    input   scale factor ud release
!     wpart   real      nopart    output  weight of the particles
!     xpart   real      nopart    output  x-in-cell of particles
!     ypart   real      nopart    output  y-in-cell of particles
!     zpart   real      nopart    output  z-in-cell of particles
!
!
!     save values between invocations
!
      save first, noemax
!
!     dimensioning
!
      integer(ip),dimension(:)    :: iftime , ifopt , nosyss, ipnt
      integer(ip),dimension(:)    :: iptime , kpart
      integer(ip),dimension(:)    :: iutime
      integer(ip),dimension(:)    :: ndprt
      integer(ip),dimension(:)    :: npart , mpart
      integer(ip),dimension(:,:)  :: lgrid
!
      real   (sp),dimension(:)    :: buffer, volume, uscal
      real   (sp),dimension(:)    :: xpart , ypart , zpart
      real   (sp),dimension(:,:)  :: aconud
      real   (sp),dimension(:,:)  :: amasud
      real   (sp),dimension(:,:)  :: wpart
!
      integer(ip),dimension(:)    :: isub  , isfil
!
      character(len=256), dimension(:)  :: finam
      character(len= 20), dimension(:)  :: sname
      character(len= 20), dimension(:)  :: snamud
!
!     parameters
!
      logical :: first  = .true.
!
!     local scalars
!
      integer(ip) ::  nmax
      integer(ip) ::  is     , isout  , itime
      integer(ip) ::  iu     , iwt    , lun    , mmax
      integer(ip) ::  nocont , nodye  , noemax , nolay   , nopart
      integer(ip) ::  nosegm , noudef , npold
      integer(ip) ::  nosubs , nosubud
      integer(4) ithndl              ! handle to time this subroutine
      data       ithndl / 0 /
      if ( timon ) call timstrt( "partur", ithndl )
!
      if(first) then
         first = .false.
         noemax = nocont + nodye
!
         aconud = 0.0  ! whole array assignment
         amasud = 0.0  ! whole array assignment
!
      endif
!
!  =============================================================
!
!   read the cell density from a delwaq restart file or map file
!
      nosegm = nolay*nmax*mmax
!
!     loop over the number of user defined releases
!
      do 100 iu = 1, noudef
!
        iwt = iutime(iu)
        if (iwt  /=  -999) then
          if (itime  >=  iwt) then
!
!           user defined release, to be activated, found
!
            isout = isub(iu)
            npold = nopart
!..
            call mudrel(  xpart     , ypart       , zpart      , mpart           , npart     , &
                          kpart     , lgrid       , mmax       , nmax            , nolay     , &
                          iu        , buffer      , iftime(iu) , ndprt(noemax+iu), nosegm    , &
                          finam(iu) , ifopt(iu)   , isfil(iu)  , iutime(iu)      , nosyss(iu) , &
                          aconud    , wpart       , nosubs     , uscal(iu)       , amasud    , &
                          nopart    , sname(isout), isout      , iptime          , lun       , &
                          ipnt      , volume      , nosubud    , snamud )
!
            iutime(iu) = -999
!
            aconud(isout,iu) = amasud(isout,iu) / ndprt(noemax+iu)
!
!  mass integration
!
            write (lun, 99003) iu, (amasud(is, iu),is = 1, nosubs)
            write (lun, 99004) ndprt(noemax+iu), iwt/86400,              &
                               mod(itime, 86400)/3600, mod(itime, 3600)  &
                               /60, mod(itime, 60)
            write (*  , 99004) ndprt(noemax+iu), iwt/86400,              &
                               mod(itime, 86400)/3600, mod(itime, 3600)  &
                               /60, mod(itime, 60)
!
          endif
        endif
!
!       end of loop
!
  100 continue
!
      if ( timon ) call timstop ( ithndl )
      return
!
99003 format(/'  User defined release    : ',i3,' activated !'   &
             /'  user defined mass (per subst)  : ', 8(e13.4,2x) )
99004 format(/'  Nr of particles released ',i8,' at : '          &
             /'  model time     : ',i3,'d-',i2.2,'h-',i2.2,'m-', &
                 i2.2,'s.'/)
!
      end subroutine
end module

