/*
*+
*  Name:
*     ERR_FLBEL

*  Purpose:
*     Deliver an ASCII BEL and flush the current error context.

*  Language:
*     Starlink ANSI C (callable from Fortran)

*  Invocation:
*     CALL ERR_FLBEL( STATUS )

*  Description:
*     An ASCII BEL character is delivered to the user and then all
*     pending error messages in the current error context are delivered
*     to the user using a call to ERR_FLUSH. On successful completion,
*     the error context is annulled and the status argument reset to
*     SAI__OK; if an error occurs during output of the error messages, the
*     error context is not annulled and the status argument is returned
*     set to ERR__OPTER.

*  Arguments:
*     STATUS = INTEGER (Returned)
*        The global status: it is set to SAI__OK on return if the
*        error message output is successful; if not, it is set to
*        ERR__OPTER.

*  Copyright:
*     Copyright (C) 2008 Science and Technology Facilities Council.
*     Copyright (C) 1993 Science & Engineering Research Council.
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
*     PCTR: P.C.T. Rees (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     DSB: David S Berry(JAC, UCLan)
*     {enter_new_authors_here}

*  History:
*     4-OCT-1993 (PCTR):
*        Original version.
*     26-JUL-2008 (TIMJ):
*        Call ERR1_FLUSH not ERR_FLUSH. No longer need common block.
*     3-AUG-2008 (TIMJ):
*        Call errFlbel. Now in C.
*     1-SEP-2008 (DSB):
*        Export *STATUS correctly.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

#include "f77.h"
#include "merswrap.h"
#include "mers_f77.h"

F77_SUBROUTINE(err_flbel)( INTEGER(STATUS) ) {
  int status;
  errFlush( &status );
  F77_EXPORT_INTEGER( status, *STATUS );
}

