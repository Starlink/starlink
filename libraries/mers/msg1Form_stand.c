/*
*+
*  Name:
*     msg1Form

*  Purpose:
*     Form a message from its text and components.

*  Language:
*     Starlink ANSI C

*  Invocation:
*     msg1Form( const char *param, const char *text, int clean,
*               int useformat, size_t msglen, char *msgstr, int * status );

*  Description:
*     Construct the final text, "msgstr", of a message
*     using the text in "text", and available message tokens.

*  Arguments:
*     param = const char * (Given)
*        The message parameter name.
*     text = const char * (Given)
*        The input message text, with any tokens.
*     clean = int (Given)
*        If the string is to be 'cleaned'
*     useformat = int (Given)
*        Boolean indicating whether the supplied text needs to take account
*        of sprintf formatting.
*     msglen = size_t (Given)
*        Allocated size of buffer "msgstr".
*     msgstr = char * (Returned)
*        Resultant message text, with parsed tokens.
*     status = int * (Given)
*        The global status.

*  Implementation Notes:
*     -  This subroutine is the STANDALONE version of msg1Form.

*  Notes:
*     The standalone version does not treat "%" as a special escape
*     code but still respects the "format" argument to decide whether
*     sprintf processing is to be performed.

*  Algorithm:
*     -  Call emsExpnd.

*  Copyright:
*     Copyright (C) 1982, 1984, 1985, 1989 Science & Engineering Research
*     Council. Copyright (C) 1999, 2001 Central Laboratory of the Research
*     Councils. Copyrigh (C) 2008, 2009 Science and Technology Facilities
*     Council. All Rights Reserved.

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
*     BDK: Dennis Kelly (ROE)
*     AJC: Alan Chipperfield (STARLINK)
*     PCTR: P.C.T. Rees (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     3-JAN-1982 (JRG):
*        Original version.
*     13-NOV-1984 (BDK):
*        ADAM version.
*     11-JUN-1985 (BDK):
*        Discard leading % from error message.
*     5-JUN-1989 (AJC):
*        Check whole string for equality with escape.
*     13-SEP_1989 (PCTR):
*        Converted to new prologue and layout.
*     15-SEP-1999 (AJC):
*        Add CLEAN argument.
*     26-FEB-2001 (AJC):
*        Use EMS_EXPND not EMS1_FORM
*     09-SEP-2008 (TIMJ):
*        Rewrite in C
*     23-DEC-2008 (TIMJ):
*        Add useformat argument.
*     12-JAN-2009 (TIMJ):
*        Remember to adjust EMS STREAM tuning parameter.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

/* If we're not using GNU C, elide __attribute__ */
#ifndef __GNUC__
#  define  __attribute__(x)  /*NOTHING*/
#endif

#include "sae_par.h"
#include "ems.h"

#include "mers1.h"

void msg1Form ( const char * param __attribute__((unused)),
                const char * text, int clean,
                int useformat, size_t msglen, char * msgstr,
                int * status __attribute__((unused)) ) {

  int lstat = SAI__OK;   /* Local status */
  int retlen = 0;
  int oldstm;

  /* Since we use EMS for expansion we have to override the STREAM
     tuning control */
  oldstm = emsStune( "STREAM", !clean, &lstat );

  /*  Call emsExpnd. */
  emsExpnd( text, msgstr, msglen, useformat, &retlen, &lstat );

  /* reset tuning */
  (void) emsStune( "STREAM", oldstm, &lstat );

}
