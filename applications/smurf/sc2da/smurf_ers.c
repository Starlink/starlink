/*
*+
*  Name:
*     smurf_ers.c

*  Purpose:
*     DRAMA Ers emulation routines for SMURF

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Description:
*     These functions provide a wrapper around the Starlink MERS/EMS
*     library designed to look like the standard DRAMA Ers routines.
*     It is needed because the data acquisition library is written
*     to use DRAMA whereas SMURF requires Starlink error handling.
*     Since both DRAMA and ADAM use inherited status the wrappers are
*     straightforward.

*  Authors:
*     Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2004-09-16 (BDK):
*        Original version. Wrapper around printf.
*     2005-11-09 (TIMJ):
*        Initial ADAM version. ErsRep.
*     {enter_further_changes_here}

*  Notes:
*     No attempt is made (currently) to translate a DRAMA error status
*     to a Starlink equivalent.

*  See Also:
*     - DRAMA ErsRep

*  Copyright:
*     Copyright (C) 2005 Particle Physics and Astronomy Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public
*     License along with this program; if not, write to the Free
*     Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
*     MA 02110-1301, USA

*  Bugs:
*     {note_any_bugs_here}
*-
*/

#include "Ers.h"
#include "mers.h"

void ErsRep ( int flags __attribute__((unused)), int *status, const char *string )
{
  /* Currently ignore flags argument */
  errRep( "ErsRep", string, status );
}
