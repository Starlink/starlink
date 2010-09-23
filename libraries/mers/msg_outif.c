/*
*+
*  Name:
*     MSG_OUTIF

*  Purpose:
*     Conditionally deliver the text of a message to the user.

*  Language:
*     Starlink ANSI C (Callable from Fortran)

*  Invocation:
*     CALL MSG_OUTIF( PRIOR, PARAM, TEXT, STATUS )

*  Description:
*     Depending upon the given value of the given message priority and
*     the message filtering level set using MSG_IFSET, the message
*     text is either expanded and output to the user or discarded.
*     The values of any existing message tokens are always annulled by
*     a call to MSG_OUTIF. If an output error occurs, an error is
*     reported and the status argument returned set to MSG__OPTER.

*  Arguments:
*     PRIOR = INTEGER (Given)
*        Message output filter. This may be one of three values:
*
*           -  MSG__QUIET = always output the message, regardless of the
*           output filter setting;
*           -  MSG__NORM = output the message if the current output
*           filter is set to either MSG__NORM or MSG__VERB or MSG__DEBUG;
*           -  MSG__VERB = output the message only if the current
*           output filter is set to MSG__VERB or MSG__DEBUG;
*           -  MSG__DEBUG = out the message only if the current
*           output filter is set to MSG__DEBUG.
*
*        Here, the collating sequence:
*
*           MSG__QUIET < MSG__NORM < MSG__VERB < MSG__DEBUG
*
*        may be assumed. Any other value will result in an error report
*        and the status being returned set to MSG__INVIF: no further
*        action will be taken.
*     PARAM = CHARACTER * ( * ) (Given)
*        The message name.
*     TEXT = CHARACTER * ( * ) (Given)
*        The message text.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2008 Science and Technology Facilities Council.
*     Copyright (C) 1991, 1992 Science & Engineering Research Council.
*     Copyright (C) 1996, 1999, 2001 Central Laboratory of the Research Councils.
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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     PCTR: P.C.T. Rees (STARLINK)
*     AJC: A. J. Chipperfield (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     EC: Ed Chapin (UBC)
*     {enter_new_authors_here}

*  History:
*     10-JUN-1991 (PCTR):
*        Original version.
*     26-AUG-1992 (PCTR):
*        Call MSG1_FORM and MSG1_PRINT directly, instead of MSG_OUT
*        (MSG_OUT no calls MSG_OUTIF with PRIOR set to MSG__NORM).
*     25-JAN-1996 (AJC):
*        re-format CHARACTER declarations
*     15-SEP-1999 (AJC):
*        Add CLEAN argument to call MSG1_FORM
*     22-FEB-2001 (AJC):
*        Use MSG1_KTOK not EMS1_KTOK
*     02-MAY-2008 (EC):
*        Fixed logic for MSG__DEBUG
*     24-JUL-2008 (TIMJ):
*        Use common block accessor
*     10-SEP-2008 (TIMJ):
*        Call msgOutif
*     2010-09-23 (TIMJ):
*        cnfImpn requires the size without the trailing nul.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

#include "f77.h"
#include "mers_f77.h"
#include "merswrap.h"
#include "msg_par.h"
#include "err_par.h"

F77_SUBROUTINE(msg_outif)( INTEGER(PRIOR),
                           CHARACTER(PARAM),
                           CHARACTER(TEXT),
                           INTEGER(STATUS)
                           TRAIL(PARAM)
                           TRAIL(TEXT) ) {
  char param[ERR__SZPAR+1];
  char text[MSG__SZMSG+1];
  int prior;
  int status;

  cnfImpn( PARAM, PARAM_length, ERR__SZPAR, param );
  cnfImpn( TEXT, TEXT_length, MSG__SZMSG, text );

  F77_IMPORT_INTEGER( *STATUS, status );
  F77_IMPORT_INTEGER( *PRIOR, prior );

  msgOutif( prior, param, text, &status );

  F77_EXPORT_INTEGER( status, *STATUS );

}
