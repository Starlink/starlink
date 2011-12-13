/*
*+
*  Name:
*     errTune

*  Purpose:
*     Set an ERR tuning parameter

*  Language:
*     Starlink ANSI C

*  Description:
*     The value of the ERR tuning parameter is set appropriately, according
*     to the value given. errTune may be called multiple times for the same
*     parameter.
*
*     The given value can be overridden by setting an environment variable,
*     ERR_<PARAM> (where <PARAM> is the tuning parameter name in upper case),
*     at run time.
*
*     The routine will attempt to execute regardless of the given value of
*     STATUS. If the given value is not SAI__OK, then it is left unchanged,
*     even if the routine fails to complete. If the STATUS is SAI__OK on
*     entry and the routine fails to complete, STATUS will be set and an
*     error report made.

*  Invocation:
*     errTune( const char * param, int value, int * status );

*  Arguments:
*     param = const char * (Given)
*        The tuning parameter to be set (case insensitive).
*     value = int (Given)
*        The desired value (see Notes).
*     status = int * (Given and Returned)
*        The global status.

*  Notes:
*     1. The following values of PARAM may be used
*
*        'SZOUT' Specifies a maximum line length to be used in the line wrapping
*            process. By default the message to be output is split into chunks
*            of no more than the maximum line length, and each chunk is written
*            on a new line. The split is made at word boundaries if possible.
*            The default maximum line length is 79 characters.
*
*            If VALUE is set to 0, no wrapping will occur. If it is set greater
*            than 6, it specifies the maximum output line length. Note that the
*            minimum VALUE is 7, to allow for exclamation marks and indentation.
*
*        'STREAM' Specifies whether or not ERR should treat its output
*            unintelligently as a stream of characters.
*            If VALUE is set to 0 (the default) all non-printing characters are
*            replaced by blanks, and line wrapping occurs (subject to SZOUT).
*            If VALUE is set to 1, no cleaning or line wrapping occurs.
*
*        'REVEAL' Allows the user to display all error messages cancelled
*            when ERR_ANNUL is called. This is a diagnostic tool which enables
*            the programmer to see all error reports, even those 'handled'
*            by the program. If VALUE is set to 0 (the default) annulling
*            occurs in the normal way. If VALUE is set to 1, the message
*            will be displayed.
*
*        'ENVIRONMENT' This is not a true tuning parameter name but causes
*            the environment variables associated with all the true tuning
*            parameters to be used if set.  If the environment variable is
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
*
*     4. With REVEAL, messages are displayed at the time of the ANNUL.
*        As REVEAL operates at the EMS level they are displayed with C
*        printf() statements so, depending upon the delivery mechanism for
*        normal messages, they may appear out of order.

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
*        Improve error messages
*     20-FEB-2001 (AJC):
*        EMS1_TUNE now EMS_TUNE
*     31-JUL-2008 (TIMJ):
*        Use accessor functions to update global state.
*        No longer call EMS_TUNE for STREAM or SZOUT.
*     12-SEP-2008 (TIMJ):
*        Rewrite in C
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

#include "sae_par.h"
#include "err_par.h"
#include "err_err.h"
#include "merswrap.h"
#include "mers1.h"
#include "ems.h"

#include <string.h>

#define MIN(aa,bb) ((aa)<(bb)?(aa):(bb))

void errTune( const char * param, int value, int * status ) {

  const char * parnames[] = { "SZOUT", "STREAM", "REVEAL", NULL };
  const char * thispar = NULL;   /* Selected parameter */

  int i;
  int set;                 /* Value has been set */
  int istat = SAI__OK;     /* Internal status */
  int env = 0;             /* Are we using the environment? */
  int npars = 0;           /* Number of parameters to read */
  int useval = 0;          /* Tuning value to actually use */
  int envval;              /* Tuning value from environment */
  int fromenv = 0;         /* Value came from environment */
  int ltune;               /* Actual tuning value used */

  /* Check for 'ENVIRONMENT' */
  if  (strcasecmp( param, "ENVIRONMENT" ) == 0) {
    env = 1;
    /* work it out ourselves */
    npars = 0;
    while ( parnames[npars] ) {
      npars++;
    }
  } else {
    env = 0;
    npars = 1;
    thispar = param;
  }

  /* Now for each required parameter */
  i = 0;
  while ( istat == SAI__OK && i < npars) {

    /* Select next par name if we are in env mode */
    if (env) {
      thispar = parnames[i];
      set = 0;
    } else {
      set = 1;
    }
    i++;

    /*     See if the associated environment variable is set
     *     If so, override the given VALUE - make sure we do not
     *     clear tokens by starting a new context. */
    emsMark();
    fromenv = 0;
    envval = mers1Getenv( 0, thispar, &istat );
    emsRlse();

    if (envval == -1) {
      /* everything okay but no environment variable */
      useval = value;
    } else if (envval > 0 ) {
      set = 1;
      useval = envval;
      fromenv = 1;
    }

    if (istat == SAI__OK && set) {
      ltune = -1;

      /*        Check that the given parameter name is acceptable
       *        and handle it. */
      if (strcasecmp( "SZOUT", thispar) == 0) {
        if (useval == 0) {
          ltune = ERR__SZMSG;
        } else if ( useval > 6 ) {
          ltune = MIN( useval, ERR__SZMSG );
        } else {
          istat = ERR__BTUNE;
        }
        if (ltune != -1) {
          err1Ptwsz( ltune );
        }

      } else if (strcasecmp( "STREAM", thispar) == 0 ) {

        if (useval == 0) {
          ltune = 0;
        } else if (useval == 1) {
          ltune = 1;
        } else {
          istat = ERR__BTUNE;
        }
        if (ltune != -1) err1Ptstm( ltune );

      } else if (strcasecmp( "REVEAL", thispar ) == 0 ) {

        if (useval == 0) {
          ltune = 0;
        } else if (useval == 1) {
          ltune = 1;
        } else {
          istat = ERR__BTUNE;
        }
        if (ltune != -1) {
          emsTune( "REVEAL", ltune, &istat );
          err1Ptrvl( ltune );
        }

      } else {

        /*           The given tuning parameter was not in the available set.
         *           Set status and report an error message.
         *           We  mark and rlse to prevent possible token name clash */
        emsMark();
        istat = ERR__BDPAR;
        emsSetc( "PARAM", thispar );
        emsRep( "ERR_TUNE_PAR",
                "errTune: Invalid tuning parameter: ^PARAM", &istat );
        emsRlse();
      }

      if (istat == ERR__BTUNE) {

        /*           The given tuning parameter value was invalid
         *           Report an error message
         *           We  mark and rlse to prevent posible token name clash */
        emsMark();
        emsSetc( "PARAM", thispar );
        emsSeti( "VALUE", useval );
        if (fromenv) {
          emsSetc( "SOURCE", "from environment variable" );
        } else {
          emsSetc( "SOURCE", " " );
        }
        emsRep( "ERR_TUNE_INV",
                "errTune: ^PARAM invalid value ^VALUE ^SOURCE", &istat);
        emsRlse();
      }
    }
  }

  /*  Set return status */
  if (*status == SAI__OK) *status = istat;
}
