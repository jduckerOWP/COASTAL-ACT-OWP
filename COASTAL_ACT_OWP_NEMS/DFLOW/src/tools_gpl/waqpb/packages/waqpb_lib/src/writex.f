!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2022.                                
!                                                                               
!  This program is free software: you can redistribute it and/or modify         
!  it under the terms of the GNU General Public License as published by         
!  the Free Software Foundation version 3.                                      
!                                                                               
!  This program is distributed in the hope that it will be useful,              
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU General Public License for more details.                                 
!                                                                               
!  You should have received a copy of the GNU General Public License            
!  along with this program.  If not, see <http://www.gnu.org/licenses/>.        
!                                                                               
!  contact: delft3d.support@deltares.nl                                         
!  Stichting Deltares                                                           
!  P.O. Box 177                                                                 
!  2600 MH Delft, The Netherlands                                               
!                                                                               
!  All indications and logos of, and references to, "Delft3D" and "Deltares"    
!  are registered trademarks of Stichting Deltares, and remain the property of  
!  Stichting Deltares. All rights reserved.                                     
!                                                                               
!-------------------------------------------------------------------------------
!  $Id: writex.f 140618 2022-01-12 13:12:04Z klapwijk $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/tools_gpl/waqpb/packages/waqpb_lib/src/writex.f $

      subroutine writex

      integer lu(15)
c
c     Subprogram to write tables for TRM in LaTeX format
c     Noodzakelijke nabewerking
c     % moet nog worden \%
c     _ moet nog worden \_
c     & moet nog worden \&
c     < moet nog worden $<$
c     > moet nog worden $>$
c     colsep moet nog worden & 
c
c     Include data structures for tables
      include 'data.inc'
      logical done  , defflg, makflg

      integer i     , jndex , iexch , isubs , iitem , iinpu , ioutp ,
     j        ioutf , istoc , ivelo , idisp

c     Table 3.1

      open ( newunit = lu(1) , file = 'tabel301.pptex' )
!      write ( lu(1) , 1000 )
      write ( lu(1) , 1010 )
     j (procid(i),procnm(i),procfo(i),i=1,nproc)
      close ( lu(1) )

c     Table 3.2


      open ( newunit = lu(2) , file = 'tabel302.pptex' )
!      write ( lu(2) , 1020 )
      do 100 isubs = 1,nsubs
          do 90 istoc = 1,nstoc
              call zoek (stocsu(istoc),1,subsid(isubs),10,jndex)
              if ( jndex .eq. 1 ) then
                  call zoek (stocfl(istoc),noutf,outffl,10,ioutf)
                  if ( ioutf .le. 0 ) goto 999
                  call zoek (stocfl(istoc),nitem,itemid,10,iitem)
                  if ( iitem .le. 0 ) goto 999
                  write ( lu(2) , 1030 ) subsid(isubs),itemnm(iitem),
     j                                itemun(iitem),outfpr(ioutf)
              endif
   90     continue
  100 continue
      close ( lu(2) )

c     Table 3.3

      open ( newunit = lu(3) , file = 'tabel303.pptex' )
!s      write ( lu(3) , 1040 )
      do 120 isubs = 1,nsubs
          do 110 ivelo = 1,nvelo
              call zoek (velosu(ivelo),1,subsid(isubs),10,jndex)
              if ( jndex .eq. 1 ) then
                  call zoek (veloit(ivelo),noutp,outpit,10,ioutp)
                  if ( ioutp .le. 0 ) goto 999
                  call zoek (veloit(ivelo),nitem,itemid,10,iitem)
                  if ( iitem .le. 0 ) goto 999
                  write ( lu(3) , 1030 ) subsid(isubs),itemnm(iitem),
     j                                itemun(iitem),outppr(ioutp)
              endif
  110     continue
  120 continue
      close ( lu(3) )

c     Table 3.4

      open ( newunit = lu(4) , file = 'tabel304.pptex' )
!      write ( lu(4) , 1050 )
      do 140 isubs = 1,nsubs
          do 130 idisp = 1,ndisp
              call zoek (dispsu(idisp),1,subsid(isubs),10,jndex)
              if ( jndex .eq. 1 ) then
                  call zoek (dispit(idisp),noutp,outpit,10,ioutp)
                  if ( ioutp .le. 0 ) goto 999
                  call zoek (dispit(idisp),nitem,itemid,10,iitem)
                  if ( iitem .le. 0 ) goto 999
                  write ( lu(4) , 1030 ) subsid(isubs),itemnm(iitem),
     j                                itemun(iitem),outppr(ioutp)
              endif
  130     continue
  140 continue
      close ( lu(4) )

c     Table 3.5

      open ( newunit = lu(5) , file = 'tabel305.pptex' )
!      write ( lu(5) , 1060 )
      do 200 istoc = 1,nstoc
          call zoek (stocfl(istoc),noutf,outffl,10,ioutf)
          if ( ioutf .le. 0 ) goto 999
          call zoek (stocfl(istoc),nitem,itemid,10,iitem)
          if ( iitem .le. 0 ) goto 999
          write ( lu(5) , 1070 ) stocfl(istoc),itemnm(iitem),
     j                        itemun(iitem),stocsc(istoc),
     j                        stocsu(istoc),outfpr(ioutf)
  200 continue
      close ( lu(5) )

c     Table 3.6

      open ( newunit = lu(6) , file = 'tabel306.pptex' )
!      write ( lu(6) , 1080 )
      do 210 ivelo = 1,nvelo
          call zoek (veloit(ivelo),noutp,outpit,10,ioutp)
          if ( ioutp .le. 0 ) goto 999
          call zoek (veloit(ivelo),nitem,itemid,10,iitem)
          if ( iitem .le. 0 ) goto 999
          write ( lu(6) , 1070 ) veloit(ivelo),itemnm(iitem),
     j                        itemun(iitem),velosc(ivelo),
     j                        velosu(ivelo),outpit(ioutp)
  210 continue
      close ( lu(6) )

c     Table 3.7

      open ( newunit = lu(7) , file = 'tabel307.pptex' )
!      write ( lu(7) , 1090 )
      do 220 idisp = 1,ndisp
          call zoek (dispit(idisp),noutp,outpit,10,ioutp)
          if ( ioutp .le. 0 ) goto 999
          call zoek (dispit(idisp),nitem,itemid,10,iitem)
          if ( iitem .le. 0 ) goto 999
          write ( lu(7) , 1070 ) dispit(idisp),itemnm(iitem),
     j                        itemun(iitem),dispsc(idisp),
     j                        dispsu(idisp),outpit(ioutp)
  220 continue
      close ( lu(7) )

c     Tables 3.8/3.9/3.10/3.11/3.12/3.13
c     Tables 3.14/3.15

      open ( newunit = lu(8) , file = 'tabel308.pptex' )
      open ( newunit = lu(9) , file = 'tabel309.pptex' )
      open ( newunit = lu(10) , file = 'tabel310.pptex' )
      open ( newunit = lu(11) , file = 'tabel311.pptex' )
      open ( newunit = lu(12) , file = 'tabel312.pptex' )
      open ( newunit = lu(13) , file = 'tabel313.pptex' )
      open ( newunit = lu(14) , file = 'tabel314.pptex' )
      open ( newunit = lu(15) , file = 'tabel315.pptex' )

!      write ( lu(8)  , 1100 )
!      write ( lu(9)  , 1100 )
!      write ( lu(10) , 1110 )
!      write ( lu(11) , 1110 )
!      write ( lu(12) , 1120 )
!      write ( lu(13) , 1120 )
!      write ( lu(14) , 1150 )
!      write ( lu(15) , 1150 )

      do 240 iitem = 1,nitem

c         Is it an input item?
          call zoek (itemid(iitem),ninpu,inpuit,10,iinpu)
          if ( iinpu .gt. 0 ) then

c             find segment/exchange
              if ( inpusx(iinpu) .eq. 1 ) then
                  iexch = 0
              else
                  iexch = 1
              endif

c             Does it have a default?
              defflg = .false.
              if ( itemde(iitem) .gt. -998. ) then

                  defflg = .true.
c                 write in table 3.10/3.11
                  write ( lu(10+iexch) , 1130 ) inpuit(iinpu),
     j              itemnm(iitem),itemun(iitem),itemde(iitem)
              endif

c             Can it be made by another process?
              makflg = .false.
              call zoek (itemid(iitem),noutp,outpit,10,ioutp)
              if ( ioutp .gt. 0 ) then
                  makflg = .true.

c                 write in table 3.8/3.9
                  write ( lu(8+iexch) , 1030 )
     j                            inpuit(iinpu),itemnm(iitem),
     j                            itemun(iitem),outppr(ioutp)
                  done = .false.
c                 scan for other processes!
  230             continue
                  call zoek (itemid(iitem),noutp-ioutp,
     j                       outpit(ioutp+1),10,jndex)
                  if ( jndex .le. 0 ) then
                      done = .true.
                  else
                      ioutp = ioutp + jndex
                      write ( lu(8+iexch) , 1140 )
     j                            inpuit(iinpu),itemnm(iitem),
     j                            itemun(iitem),outppr(ioutp)
                  endif
                  if ( .not. done ) goto 230
              endif

c             No default and not makeable
              if ( .not.makflg .and. .not.defflg ) then

c                 write in table 3.12/3.13
                  write ( lu(12+iexch) , 1031 )
     j            inpuit(iinpu),itemnm(iitem),itemun(iitem)
              endif
          else
c             No input item
              call zoek (itemid(iitem),noutp,outpit,10,ioutp)
              if ( ioutp .gt. 0 ) then
                  call zoek (itemid(iitem),nvelo,veloit,10,ivelo)
                  call zoek (itemid(iitem),ndisp,dispit,10,idisp)
                  if ( ivelo .le. 0 .and. idisp .le. 0 ) then
                      if ( outpsx(ioutp) .eq. 1 ) then
                          iexch = 0
                      else
                          iexch = 1
                      endif
c                     write in table 3.14/3.15
                      write ( lu(14+iexch) , 1030 )
     j                        itemid(iitem),itemnm(iitem),
     j                        itemun(iitem),outppr(ioutp)
                  endif
              endif
          endif
  240 continue

      close ( lu(8) )
      close ( lu(9) )
      close ( lu(10) )
      close ( lu(11) )
      close ( lu(12) )
      close ( lu(13) )
      close ( lu(14) )
      close ( lu(15) )

      return
  999 stop 'Inconsistent database structure'

 1000 format ('%"Process & Description & Documented under.."')
 1010 format (a10,' colsep ',a50,' colsep ',a10,' \\')
 1020 format ('%"Substance & Description flux & Unit flux & Process"')
 1030 format (a10,' colsep ',a50,' colsep ',a20,' colsep ',a10,' \\')
 1031 format (a10,' colsep ',a50,' colsep ',a20,' \\')
 1040 format ('%"Substance & Description velocity & Unit velocity"',
     j        ',"Process"')
 1050 format ('%"Substance & Description dispersion & Unit dispersion"',
     j        ',"Process"')
 1060 format ('%"Flux & Description flux & Unit flux & Stoch. & Sub."',
     j        ',"Process"')
 1070 format (a10,' colsep ',a50,' colsep ',a20,' colsep ',f8.2,
     j            ' colsep ',a10,' colsep ',a10,' \\')
 1080 format ('%"Flux & Description velocity & Unit velocity"',
     j        ',"Stoch. & Sub. & Process"')
 1090 format ('%"Flux & Description dispersion & Unit dispersion"',
     j        ',"Stoch. & Sub. & Process"')
 1100 format ('%"Process input & Input description & Input unit"',
     j        ',"Process"')
 1110 format ('%"Process input & Input description & Input unit"',
     j        ',"Default"')
 1120 format ('%"Process input & Input description & Input unit"')
 1130 format (a10,' colsep ',a50,' colsep ',a20,' colsep ',f15.3, 
     j        ' \\')       
 1140 format (a10,' colsep ',a50,' colsep ',a20,' colsep ',a10,' \\')
 1150 format ('%"Output item & Output description & Output unit"',
     j        ',"Process"')

      end
