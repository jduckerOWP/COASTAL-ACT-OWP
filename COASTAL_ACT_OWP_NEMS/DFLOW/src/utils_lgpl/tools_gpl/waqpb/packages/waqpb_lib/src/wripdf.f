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
!  $Id: wripdf.f 140618 2022-01-12 13:12:04Z klapwijk $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/tools_gpl/waqpb/packages/waqpb_lib/src/wripdf.f $

      subroutine wripdf ( procid, procnm, procco, procfo, lun   )

      character*50 procnm
      character*10 procid, procfo
      integer      procco, lun   , i
      include 'pdf.inc'

      write ( lun , '(a10,20x,a50)' ) procid,procnm
      write ( lun , '(a10,''; module name'')' ) procfo
      write ( lun , '(i3,''       ; TRswitch'')' ) procco

      write ( lun , '(i10,
     j      ''; # input items for segments'')' ) ins
      if ( ins .gt. 0 )
     jwrite ( lun , '(a10,2x,g15.6,1x,a1,1x,a50)' )
     j(ins_id(i),ins_va(i),ins_do(i),ins_nm(i),i=1,ins)

      write ( lun , '(i10,
     j      ''; # input items for exchanges'')' ) ine
      if ( ine .gt. 0 )
     jwrite ( lun , '(a10,2x,g15.6,1x,a1,1x,a50)' )
     j(ine_id(i),ine_va(i),ine_do(i),ine_nm(i),i=1,ine)

      write ( lun , '(i10,
     j      ''; # output items for segments'')' ) ous
      if ( ous .gt. 0 )
     jwrite ( lun , '(a10,18x,a1,1x,a50)' )
     j(ous_id(i),ous_do(i),ous_nm(i),i=1,ous)

      write ( lun , '(i10,
     j      ''; # output items for exchanges'')' ) oue
      if ( oue .gt. 0 )
     jwrite ( lun , '(a10,18x,a1,1x,a50)' )
     j(oue_id(i),oue_do(i),oue_nm(i),i=1,oue)

      write ( lun , '(i10,''; # fluxes'')' ) flu
      if ( flu .gt. 0 )
     jwrite ( lun , '(a10,18x,a1,1x,a50)' )
     j(flu_id(i),flu_do(i),flu_nm(i),i=1,flu)

      write ( lun , '(i10,''; # stoichiometry lines'')' )sto
      if ( sto .gt. 0 )
     jwrite ( lun , '(a10,2x,a10,2x,f10.5)' )
     j(sto_su(i),sto_fl(i),sto_sc(i),i=1,sto)

      write ( lun , '(i10,
     j   ''; # stoichiometry lines dispersion arrays'')' ) dis
      if ( dis .gt. 0 )
     jwrite ( lun , '(a10,2x,a10,2x,f10.5)' )
     j(dis_su(i),dis_it(i),dis_sc(i),i=1,dis)

      write ( lun , '(i10,
     j   ''; # stoichiometry lines velocity arrays'')' ) vel
      if ( vel .gt. 0 )
     jwrite ( lun , '(a10,2x,a10,2x,f10.5)' )
     j(vel_su(i),vel_it(i),vel_sc(i),i=1,vel)

      write ( lun , '(''END'')')
      return
      end


      subroutine wripdn ( procid, procnm, procco, procfo, lun   )

      character*50 procnm
      character*10 procid, procfo
      integer      procco, lun   , i
      include 'pdf.inc'

      write ( lun , '(a10,20x,a50)' ) procid,procnm
      write ( lun , '(a10,''; module name'')' ) procfo
      write ( lun , '(i3,''       ; TRswitch'')' ) procco

      write ( lun , '(i10,
     j      ''; # input items for segments'')' ) ins
      if ( ins .gt. 0 )
     jwrite ( lun , '(a10,2x,g15.6,1x,a1,1x,a50,5x,a20)' )
     j(ins_id(i),ins_va(i),ins_do(i),ins_nm(i),ins_un(i), i=1,ins)

      write ( lun , '(i10,
     j      ''; # input items for exchanges'')' ) ine
      if ( ine .gt. 0 )
     jwrite ( lun , '(a10,2x,g15.6,1x,a1,1x,a50,5x,a20)' )
     j(ine_id(i),ine_va(i),ine_do(i),ine_nm(i),ine_un(i),i=1,ine)

      write ( lun , '(i10,
     j      ''; # output items for segments'')' ) ous
      if ( ous .gt. 0 )
     jwrite ( lun , '(a10,18x,a1,1x,a50,5x,a20)' )
     j(ous_id(i),ous_do(i),ous_nm(i),ous_un(i),i=1,ous)

      write ( lun , '(i10,
     j      ''; # output items for exchanges'')' ) oue
      if ( oue .gt. 0 )
     jwrite ( lun , '(a10,18x,a1,1x,a50,5x,a20)' )
     j(oue_id(i),oue_do(i),oue_nm(i),oue_un(i),i=1,oue)

      write ( lun , '(i10,''; # fluxes'')' ) flu
      if ( flu .gt. 0 )
     jwrite ( lun , '(a10,18x,a1,1x,a50,5x,a20)' )
     j(flu_id(i),flu_do(i),flu_nm(i),flu_un(i),i=1,flu)

      write ( lun , '(i10,''; # stoichiometry lines'')' )sto
      if ( sto .gt. 0 )
     jwrite ( lun , '(a10,2x,a10,2x,f10.5)' )
     j(sto_su(i),sto_fl(i),sto_sc(i),i=1,sto)

      write ( lun , '(i10,
     j   ''; # stoichiometry lines dispersion arrays'')' ) dis
      if ( dis .gt. 0 )
     jwrite ( lun , '(a10,2x,a10,2x,f10.5)' )
     j(dis_su(i),dis_it(i),dis_sc(i),i=1,dis)

      write ( lun , '(i10,
     j ''; # stoichiometry lines velocity arrays'')' ) vel
      if ( vel .gt. 0 )
     jwrite ( lun , '(a10,2x,a10,2x,f10.5)' )
     j(vel_su(i),vel_it(i),vel_sc(i),i=1,vel)

      write ( lun , '(''END'')')
      return
      end


