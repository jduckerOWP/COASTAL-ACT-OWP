//---- LGPL --------------------------------------------------------------------
//
// Copyright (C)  Stichting Deltares, 2011-2022.
//
// This library is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation version 2.1.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this library; if not, see <http://www.gnu.org/licenses/>.
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
// $Id: delftonline_version.cpp 140618 2022-01-12 13:12:04Z klapwijk $
// $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/utils_lgpl/delftonline/src/delftonline/delftonline_version.cpp $
#include <stdio.h>
#include "delftonline_version.h"

static char delftonline_version_id [] = {"@(#)Deltares, " delftonline_program " Version " delftonline_major "." delftonline_minor "." delftonline_revision "." delftonline_build ", " __DATE__ ", " __TIME__ ""};

char * getfullversionstring_delftonline(void)
{
    return delftonline_version_id;
}
