/*
 *+
 *  Name:
 *     emsTune

 *  Purpose:
 *     Set an EMS tuning parameter, deprecated version, use emsStune.

 *  Language:
 *     Starlink ANSI C

 *  Invocation:
 *     emsTune( key, value, status )

 *  Arguments:
 *     key = char* (Given)
 *        The name of the tuning parameter to be set
 *     value = int (Given)
 *        The desired value.
 *     status = int* (Given and Returned)
 *        The global status.

 *  Copyright:
 *     Copyright (C) 2008 Science and Technology Facilities Council.
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

 *  Authors:
 *     PWD: Peter W. Draper (JAC, Durham University)
 *     {enter_new_authors_here}

 *  History:
 *     31-JUL-2008 (PWD):
 *        Deprecated in favour of emsStune. Now calls emsStune.
 *     {enter_changes_here}

 *  Bugs:
 *     {note_any_bugs_here}

 *-
 */

#include "ems.h"                     /* EMS_ function prototypes */

void emsTune( const char *key, const int value, int *status )
{
    (void) emsStune( key, value, status );
}
