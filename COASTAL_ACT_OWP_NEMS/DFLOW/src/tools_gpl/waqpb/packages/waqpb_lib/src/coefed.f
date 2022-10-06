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
!  $Id: coefed.f 140618 2022-01-12 13:12:04Z klapwijk $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/tools_gpl/waqpb/packages/waqpb_lib/src/coefed.f $

      subroutine coefed(serial,itmswi)
c
c     Create COEFEDIT.DAT file (Sobek only)
c
c     Include data structures for tables
      include 'data.inc'

      integer serial, lu_out, iitem, niteml
      logical itmswi(nitemm)

      niteml = 0
      do iitem = 1,nitem
          if ( itmswi(iitem) ) niteml = niteml + 1
      enddo

      open (newunit=lu_out,file='coefedit.dat')

      write (lu_out, '(i10)' ) serial
      write (lu_out, '(''NC'')' ) 
      write (lu_out, '(i6)' ) niteml
      write (lu_out, '(''COEFFICIENTS'')' ) 
      do iitem = 1,nitem
          if ( itmswi(iitem) )
c     More significant digits!!!! JvG Jan 2011
c     j    write (lu_out,'(''"'',a10,''",'',e10.3,'',1,1,1'')' ) 
     j    write (lu_out,'(''"'',a10,''",'',g15.7,'',1,1,1'')' ) 
     j    itemid(iitem),itemde(iitem)
      enddo
      write ( lu_out,'(''NG''/''1''/''GROUPS''/
     j        ''1,"Process parameters",0,0'')' )


      close (lu_out)

      return
      end
