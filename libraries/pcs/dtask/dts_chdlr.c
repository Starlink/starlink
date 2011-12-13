/*
*+
*  Name:
*     DTASK_CHDLR

*  Purpose:
*     C Version AST handler for timed reschedules

*  Language:
*     C

*  Invocation:
*     Invoked by timer completion
*     interrupts.

*  Description:
*     Is called by the C ATIMER system and  in turn calls the Fortran
*     DTASK_ASTHDLR routine.

*  Arguments:
*     ASTPARM=INTEGER (given)
*           The timer id

*  Algorithm:
*     Call the Fortran handler with its argument a  pointer to the
*     argument of this routine.

*  Copyright:
*     Copyright (C) 1994 Science & Engineering Research Council. All
*     Rights Reserved.

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
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     AJC: A.J.Chipperfield (STARLINK, RAL)
*     {enter_new_authors_here}

*  History:
*     28-JUN-1994 (AJC):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/
#include "f77.h"

extern void F77_EXTERNAL_NAME(dtask_asthdlr)( INTEGER(id) );

F77_SUBROUTINE(dtask_chdlr)( int id )
{
F77_LOCK( F77_CALL(dtask_asthdlr)( INTEGER_ARG(&id) ); )
}
