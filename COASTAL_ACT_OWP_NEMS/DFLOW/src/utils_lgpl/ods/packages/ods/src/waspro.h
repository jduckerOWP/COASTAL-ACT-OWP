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
/*
 *  waspro.h  -  ODS function prototypes for WASPRO files
 *
 *   Bart Adriaanse
 */

/*
 *  $Author: Markus $
 *  $Date: 11/15/00 3:52p $
 *  $Source: /u/cvsroot/gpp/libsrc/ods/waspro.h,v $
*/
/*
 *
 */

void ODSGetLocWas  ( char *fname,  char *locdef,TInt4 maxdef,  char *loclst,
                    TInt4 maxlst, TInt4 *nrlst, TInt4 *ierror) ;

void ODSGetLocWs1  ( char *fname,  char *locdef,TInt4 maxdef,  char *loclst,
                    TInt4 maxlst, TInt4 *nrlst, TInt4 *ierror) ;

void ODSGetParWas  ( char *fname,  char *pardef, TInt4 maxdef,  char *parlst,
                     char *paruni,TInt4 maxlst,  TInt4 *nrlst, TInt4 *ierror) ;

void ODSGetParWs1  ( char *fname,  char *pardef, TInt4 maxdef,  char *parlst,
                     char *paruni,TInt4 maxlst,  TInt4 *nrlst, TInt4 *ierror) ;

void ODSGetTmeWas  ( char *fname,  double *timdef,TInt4 maxdef,  double *timlst,
                    TInt4 maxlst, TInt4 *nrlst, TInt4 *ierror) ;

void ODSGetTmeWs1  ( char *fname,  double *timdef,TInt4 maxdef,  double *timlst,
                    TInt4 maxlst, TInt4 *nrlst, TInt4 *ierror) ;

void ODSGetValWs1  ( char *fname,  char *locin,    char *parin,  double *timin,
                    TInt4 maxilo, TInt4 maxipa,   TInt4 maxiti,  float misval,
                     char *loc,    char *par,      double *tim,  float *values,
                    TInt4 maxolo, TInt4 maxopa,   TInt4 maxoti, TInt4 *nrloc ,
                    TInt4 *nrpar ,TInt4 *nrtim ,  TInt4 *ierror) ;

void ODSGetValWas  ( char *fname,  char *locin,    char *parin,  double *timin,
                    TInt4 maxilo, TInt4 maxipa,   TInt4 maxiti,  float misval,
                     char *loc,    char *par,      double *tim,  float *values,
                    TInt4 maxolo, TInt4 maxopa,   TInt4 maxoti, TInt4 *nrloc ,
                    TInt4 *nrpar ,TInt4 *nrtim ,  TInt4 *ierror) ;
