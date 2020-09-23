#if !defined( LPG_INCLUDED )  /* Include this file only once */
#define LPG_INCLUDED
/*
*+
*  Name:
*     lpg.h

*  Purpose:
*     Define the C interface to the LPG library.

*  Description:
*     This module defines the C interface to the functions of the LPG
*     library. The file lpg.c contains C wrappers for the Fortran LPG
*     routines.

*  Copyright:
*     Copyright (C) 2020 East Asian Observatory.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     DSB: David S Berry (EAO)
*     {enter_new_authors_here}

*  History:
*     22-SEP-2020 (DSB):
*        Original version.
*     {enter_changes_here}

*-
*/

void lpgAssoc( const char *param, const char *mode, int *indf, int *status );
void lpgProp( int indf1, const char *clist, const char *param, int *indf2, int *status );

#endif
