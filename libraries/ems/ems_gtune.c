/*
*+
*  Name:
*     ems_gtune

*  Purpose:
*     Fortran callable routine

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

*  Copyright:
*     Copyright (C) 2008 Science and Technology Facilities Council.
*     All Rights Reserved.

*  Authors:
*     {enter_new_authors_here}

*-
*/
#define MAXKEYSZ  6                    /* Maximum key length */

#include "ems.h"                       /* ems_ function prototypes */
#include "f77.h"                       /* CNF macros and prototypes */

#include "ems_f77.h"

F77_SUBROUTINE (ems_gtune)( CHARACTER(key), INTEGER(value), INTEGER(status)
                            TRAIL(key) )
{
    char ckey[MAXKEYSZ+1];   /* Imported keyword */

    GENPTR_CHARACTER(key)
    GENPTR_INTEGER(value)
    GENPTR_INTEGER(status)

    cnfImpn( key, key_length, MAXKEYSZ, ckey );

    *value = emsGtune( ckey, status );

    return;
}
