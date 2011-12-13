/*
*+
*  Name:
*     MSG_TUNE

*  Purpose:
*     Set an MSG tuning parameter

*  Language:
*     Starlink ANSI C (Callable from Fortran)

*  Description:
*     The value of the MSG tuning parameter is set appropriately, according
*     to the value given. MSG_TUNE may be called multiple times for the same
*     parameter.
*
*     The given value can be overridden by setting an environment variable,
*     MSG_PARAM (where PARAM is the tuning parameter name in upper case),
*     at run time.

*  Invocation:
*     CALL MSG_TUNE( PARAM, VALUE, STATUS )

*  Arguments:
*     PARAM = CHARACTER*(*) (Given)
*        The tuning parameter to be set (case insensitive).
*     VALUE = INTEGER (Given)
*        The desired value (see Notes).
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     1. The following values of PARAM may be used:
*
*        'FILTER' Specifies the required MSG conditional message reporting
*            level. VALUE may be 1, 2, 3 or 4, corresponding with quiet, normal
*            (the default), verbose and debug levels respectively.
*
*        'SZOUT' Specifies a maximum line length to be used in the line wrapping
*            process. By default the message output by MSG is split into chunks
*            of no more than the maximum line length, and each chunk is written
*            on a new line. The split is made at word boundaries if possible.
*            The default maximum line length is 79 characters.
*
*            If VALUE is set to 0, no wrapping will occur. If it is set greater
*            than 0, it specifies the maximum output line length.
*
*        'STREAM' Specifies whether or not MSG should treat its output
*            unintelligently as a stream of characters.
*            If VALUE is set to 0 (the default) all non-printing characters are
*            replaced by blanks, and line wrapping occurs (subject to SZOUT).
*            If VALUE is set to 1, no cleaning or line wrapping occurs.
*
*        'ENVIRONMENT' This is not a true tuning parameter name but causes
*            the environment variables associated with all the true tuning
*            parameters to be used if set. If the environment variable is
*            not set, the tuning parameter is not altered. The VALUE argument
*            is not used.
*
*     2. The tuning parameters for MSG and ERR operate partially at the EMS
*        level and may conflict in their requirements of EMS.
*
*     3. The use of SZOUT and STREAM may be affected by the message delivery
*        system in use. For example there may be a limit on the the size of a
*        line output by a Fortran WRITE and automatic line wrapping may occur.
*        In particular, a NULL character will terminate a message delivered by
*        the ADAM message system.

*  Copyright:
*     Copyright (C) 2008 Science and Technology Facilities Council.
*     Copyright (C) 1999, 2001 Central Laboratory of the Research Councils.
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
*     AJC: A.J. Chipperfield (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     21-JUL-1999 (AJC):
*        Original version.
*      3-SEP-1999 (AJC):
*        Added 'ENVIRONMENT' parameter
*     22-SEP-1999 (AJC):
*        Added FILTER parameter
*        Improve error messages
*     20-FEB-2001 (AJC):
*        EMS1_TUNE renamed EMS_TUNE
*     02-MAY-2008 (TIMJ):
*        Add MSG__DEBUG level.
*     24-JUL-2008 (TIMJ):
*        Use common block setter
*     15-SEP-2008 (TIMJ):
*        Call errTune. Now in C.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

#include "f77.h"
#include "merswrap.h"
#include "star/mem.h"
#include "mers_f77.h"

F77_SUBROUTINE(msg_tune)( CHARACTER(PARAM), INTEGER(VALUE), INTEGER(STATUS)
                          TRAIL(PARAM) ) {

      int value;
      int status;
      char *param;

      GENPTR_CHARACTER(PARAM);

      F77_IMPORT_INTEGER( *VALUE, value );
      F77_IMPORT_INTEGER( *STATUS, status );
      param = starMallocAtomic( PARAM_length + 1 );
      F77_IMPORT_CHARACTER( PARAM, PARAM_length, param );

      msgTune( param, value, &status );

      starFree(param);
      F77_EXPORT_INTEGER( status, *STATUS );
}
