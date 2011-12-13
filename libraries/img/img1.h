/*
 *+
 * Name:
 *    img1.h
 *
 * Purpose:
 *    Header file for defining internal IMG function prototypes.
 *
 * Language:
 *    ANSI C
 *
*  Copyright:
*     Copyright (C) 1996 Central Laboratory of the Research Councils.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

 * Authors:
 *    PDRAPER: P.W. Draper (STARLINK - Durham University)
 *    {enter_new_authors_here}
 *
 * History:
 *    17-MAY-1996 (PDRAPER):
 *       Original version.
 *    {enter_further_changes_here}
 *
 *-
 */

#define MAXFSTRING 132     /*  Maximum length of Fortran string */

void img1StoreArrayString( const char *string, const char *id,
                           const int position,  char *stringarray[],
                           int *status );

void img1FreeStringArray( const char *id, int *status );

void img1ExtractParam( const char *string, const int n, char *value,
                       int *status );

int img1CountParams( const char *string, int *status );

/* $Id$ */
