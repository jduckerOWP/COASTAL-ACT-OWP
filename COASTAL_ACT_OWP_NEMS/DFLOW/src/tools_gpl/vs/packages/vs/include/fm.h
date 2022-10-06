//---- GPL ---------------------------------------------------------------------
//
// Copyright (C)  Stichting Deltares, 2011-2022.
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation version 3.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
//
// contact: delft3d.support@deltares.nl
// Stichting Deltares
// P.O. Box 177
// 2600 MH Delft, The Netherlands
//
// All indications and logos of, and references to, "Delft3D" and "Deltares"
// are registered trademarks of Stichting Deltares, and remain the property of
// Stichting Deltares. All rights reserved.
//
//------------------------------------------------------------------------------
// $Id: fm.h 140618 2022-01-12 13:12:04Z klapwijk $
// $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/tools_gpl/vs/packages/vs/include/fm.h $

#ifndef _FM_INCLUDED
#   define _FM_INCLUDED
#   define MAX_FILE_NAME 256

    extern BVoid   FM_close_nefis_files        ( BVoid ) ;
    extern BInt4   FM_open_nefis_files         ( const BText,
                                                 const BText ) ;
    extern BInt4   FM_get_datafile_descriptor  ( BVoid ) ;
    extern BInt4   FM_get_deftfile_descriptor  ( BVoid ) ;
    extern BVoid   FM_display_nefis_file_status( const BText ) ;
#endif
