/*
*+
*  Name:
*     MSG_BLANKIF

*  Purpose:
*     Conditionally output a blank line.

*  Language:
*    Starlink ANSI C (Callable from Fortran)

*  Invocation:
*     CALL MSG_BLANK( PRIOR, STATUS )

*  Description:
*     Depending upon the given value of the given message priority and
*     the message filtering level set using msgIfset, a blank line is
*     either output to the user or discarded. If the status argument is not
*     set to SAI__OK on entry, no action is taken. If an output error
*     occurs, an error report is made and the status argument returned
*     set to MSG__OPTER.

*  Arguments:
*     PRIOR = INTEGER (Given)
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     -  Calls msgBlankif

*  Copyright:
*     Copyright (C) 2008 Science and Technology Facilities Council.
*     Copyright (C) 1990, 1991, 1992 Science & Engineering Research Council.
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
*     {enter_new_authors_here}

*  History:
*     24-SEP-1990 (PCTR):
*        Original version.
*     24-JAN-1991 (PCTR):
*        Changed to use MSG1_PRINT (i.e. environment independent).
*     26-AUG-1992 (PCTR):
*        Output the blank line conditionally, assuming MSG__NORM to be
*        the priority.
*     10-SEP-2008 (TIMJ):
*        Call msgBlank. Now in C.
*     23-DEC-2008 (TIMJ):
*        Calls msgBlankif.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

#include "f77.h"
#include "mers_f77.h"
#include "merswrap.h"


F77_SUBROUTINE(msg_blankif)( INTEGER(PRIOR), INTEGER(STATUS) ) {
  int status;
  int prior;
  F77_IMPORT_INTEGER( *STATUS, status );
  F77_IMPORT_INTEGER( *PRIOR, prior );
  msgBlankif( prior, &status );
  F77_EXPORT_INTEGER( status, *STATUS );
}
