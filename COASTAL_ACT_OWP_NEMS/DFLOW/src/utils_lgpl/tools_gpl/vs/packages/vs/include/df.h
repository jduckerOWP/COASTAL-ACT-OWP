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
// $Id: df.h 140618 2022-01-12 13:12:04Z klapwijk $
// $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/tools_gpl/vs/packages/vs/include/df.h $

#ifndef _DF_INCLUDED

    extern struct St_def *DF_find_definition_in_tree (
                            struct St_def *, const BText ) ;
    extern void           DF_remove_definition_branche (
                            struct St_def * ) ;
    extern struct St_def * DF_add_definition_to_tree (
                            struct St_def *, const BText ) ;

#   define _DF_INCLUDED
#endif
