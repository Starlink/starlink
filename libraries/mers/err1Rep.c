/*
*+
*  Name:
*     err1Rep

*  Purpose:
*     Report an error message with optional C-style formatting.

*  Language:
*     Starlink ANSI C

*  Invocation:
*     err1Rep( const char * param, const char * text, int useformat,
*              va_list args, int * status );

*  Description:
*     Report an error message. According to the error context, the
*     error message is either sent to the user or retained in the
*     error table. The latter case allows the application to take
*     further action before deciding if the user should receive the
*     message. On exit the values associated with any existing message
*     tokens are left undefined. On successful completion, the global
*     status is returned unchanged; if the status argument is set to
*     SAI__OK on entry, an error report to this effect is made on behalf
*     of the application and the status argument is returned set to
*     ERR__BADOK; the given message is still reported with an associated
*     status of ERR__UNSET.
*     If an output error occurs, the status argument is
*     returned set to ERR__OPTER. The status argument may also be returned
*     set to an EMS_ fault error value, indicating an error occuring
*     within the error reporting software.

*  Arguments:
*     param = const char * (Given)
*        The error message name.
*     text = const char * (Given)
*        The error message text.
*     useformat = Logical (Given)
*        If true, "text" is processed using sprintf to expand format
*        specifiers using the supplied "args".
*     args = va_list (Given)
*        Variadic arguments for sprintf processing.
*     status = int * (Given & Returned)
*        The global status: it is left unchanged on successful completion,
*        or is set an appropriate error value if an internal error has
*        occurred.

*  Notes:
*     If "format" is true, printf-style formatting will be applied using
*     the supplied va_list argument. Formatting is applied after token
*     replacement. Tokens containing "%" will not be treated as format
*     specifiers. Keyword associations will be disabled since they also
*     use "%".
*
*     Using printf formatting can be useful for simplifying code that
*     does not require deferred token handling. See also msgFmt() for
*     formatting tokens.

*  Algorithm:
*     -  Use msg1Form to create the complete message text.
*     -  Use emsRep to store the message in the error table.

*  Copyright:
*     Copyright (C) 2008, 2009 Science and Technology Facilities Council.
*     Copyright (C) 1983, 1989-1991, 1994 Science & Engineering
*     Research Council.
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
*     JRG: Jack Giddings (UCL)
*     SLW: Sid Wright (UCL)
*     RFWS: R.F. Warren-Smith (STARLINK)
*     PCTR: P.C.T. Rees (STARLINK)
*     AJC: A.J. Chipperfield (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     3-JAN-1983 (JRG):
*        Original version.
*     17-APR-1983 (SLW):
*        Added MARK and RELEASE mods.
*     7-AUG-1989 (RFWS):
*        Converted to new prologue layout and added comments.
*     12-SEP-1989 (PCTR):
*        Completed code tidy-up.
*     13-DEC-1989 (PCTR):
*        Converted to use EMS_ calls.
*     19-MAR-1990 (PCTR):
*        Changed handling of status returned from ERR_FLUSH.
*     25-SEP-1990 (PCTR):
*        Changed call from EMS1_IELEV to EMS_LEVEL.
*     22-JAN-1991 (PCTR):
*        Removed default level behaviour (it now exists in EMS1_ESTOR).
*     10-JUN-1994 (AJC):
*        Associate ERR__BADOK with warning message and ERR__UNSET with
*        the given message if STATUS is given as SAI__OK.
*     15-SEP-1999 (AJC):
*        Add CLEAN argument to call MSG1_FORM
*     21-FEB-2001 (AJC):
*        Use EMS_REP not EMS1_ESTOR
*     31-JUL-2008 (TIMJ):
*        Use common accessor rather than COMMON directly.
*     09-SEP-2008 (TIMJ):
*        Rewrite in C
*     23-DEC-2008 (TIMJ):
*        Copy from errRep to allow printf formatting to be enabled/disabled.
*     12-JAN-2009 (TIMJ):
*        Fix err1Gtstm logic for msg1Form (was inverted)
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

#include "sae_par.h"
#include "err_err.h"
#include "err_par.h"
#include "ems_err.h"
#include "star/util.h"

#include "ems.h"
#include "mers.h"
#include "mers1.h"

#include <string.h>
#include <stdarg.h>

void err1Rep( const char * param, const char * text, int useformat,
              va_list args, int * status ) {

  char tstr[ERR__SZMSG];    /* Intermediate error message text */
  char mstr[ERR__SZMSG];     /* Final error message text */
  char pstr[ERR__SZPAR];    /* Local error name text */

  int strstat = 1;          /* status from star_strappend */
  int istat = SAI__OK;      /* Internal status */
  int mlen = 0;             /* Length of final error message text */
  size_t lpos = 0;             /* string position pointer */
  int tokpos = 0;           /* Position of ^ in string */

  /* Terminate strings */
  pstr[0] = '\0';
  mstr[0] = '\0';
  pstr[0] = '\0';

  /*  Check the inherited status: if it is SAI__OK, then set status to
   *  ERR__BADOK and store an additional message in the error table. */
  if (*status == SAI__OK) {

    /*     Set the report status equal to ERR__BADOK.*/
    *status = ERR__BADOK;

    /*     Make an additional error report. */
    star_strlcpy( pstr, "ERR_REP_BADOK", sizeof(pstr) );
    star_strlcpy( mstr, "STATUS not set in call to errRep "
                  "(improper use of errRep)", sizeof(mstr) );

    /*     Store the additional message in the error table (first create a new
     *     error reporting context to avoid loss of tokens in the base level).
     *     Associate status ERR__BADOK with the additional message.
     *     If emsRep returns an error status it will be ignored but will
     *     almost certainly be repeated later with the given message. */
    emsMark();
    istat = ERR__BADOK;
    emsRep( pstr, mstr, &istat );
    pstr[0] = '\0';
    mstr[0] = '\0';
    /*     Release the error context. */
    emsRlse();

    /*     Set the given message status to ERR__UNSET */
    istat = ERR__UNSET;
  } else {

    /* Else, a normal bad status is given - set ISTAT to the given status value */
    istat = *status;
  }

  /*  Now form the given error message.
   *  Status is not altered by this routine. */
  msg1Form( param, text, !err1Gtstm(), useformat, sizeof(tstr), tstr, &istat );

  /*  Any double ^ will now be single - we must protect it from EMS_REP */
  lpos = 0;
  mlen = -1;
  ems1Gesc( "^", tstr, &tokpos );

  while ( tokpos >= 0 ) {

    /* Copy everything from lpos to tokpos and append to mstr
     - put in a null and then copy in two escape characters and reset nul */
    tstr[tokpos] = '\0';
    strstat = star_strappend( mstr, &tstr[lpos], sizeof(mstr));
    strstat = star_strappend( mstr, "^^", sizeof(mstr) );
    if (!strstat) break;
    tstr[tokpos] = ' ';
    lpos = tokpos + 1;
    ems1Gesc( "^", tstr, &tokpos );
  }

  /*  Now copy the remainder of the string */
  if (lpos <= strlen(tstr) ) {
    strstat = star_strappend( mstr, &tstr[lpos], sizeof(mstr) );
  }

  /*  Report the already constructed message with emsRep */
  if (useformat) {
    emsRepv( param, mstr, args, status );
  } else {
    emsRep( param, mstr, status );
  }

  /*  Check the returned status for message output errors and attempt to
   *  report an additional error in the case of failure - but only on the
   *  first occasion. */
  if (istat == EMS__OPTER && *status != ERR__OPTER) {
    *status = ERR__OPTER;
    istat = ERR__OPTER;
    star_strlcpy(pstr, "ERR_REP_OPTER", sizeof(pstr) );
    star_strlcpy(mstr, "errRep: Error encountered during message output",
                sizeof(mstr));
    emsRep( pstr, mstr, &istat );
  }

}
