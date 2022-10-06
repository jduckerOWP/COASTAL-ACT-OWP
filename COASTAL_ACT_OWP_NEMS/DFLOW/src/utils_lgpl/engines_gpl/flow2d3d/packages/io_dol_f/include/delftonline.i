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
!  $Id: delftonline.i 140618 2022-01-12 13:12:04Z klapwijk $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/engines_gpl/flow2d3d/packages/io_dol_f/include/delftonline.i $
!--------------------------------------------------------------------------------
!   DelftOnline -- Constant Definitions for Fortran
!
!   Irv.Elshoff@deltares.nl
!   17 may 06
!--------------------------------------------------------------------------------


integer, parameter ::   DOL_OPAQUE          = 0
integer, parameter ::   DOL_INTEGER         = 1
integer, parameter ::   DOL_REAL            = 2
integer, parameter ::   DOL_DOUBLE          = 3
integer, parameter ::   DOL_DOUBLECOMPLEX   = 4
integer, parameter ::   DOL_COMPLEX         = 5
integer, parameter ::   DOL_LOGICAL         = 6
integer, parameter ::   DOL_CHARACTER       = 7

integer, parameter ::   DOL_IN              = 1
integer, parameter ::   DOL_OUT             = 2
integer, parameter ::   DOL_INOUT           = 3

