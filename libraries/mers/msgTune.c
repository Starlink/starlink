/*
*+
*  Name:
*     msgTune

*  Purpose:
*     Set an MSG tuning parameter

*  Language:
*     Starlink ANSI C

*  Description:
*     The value of the MSG tuning parameter is set appropriately, according
*     to the value given. msgTune may be called multiple times for the same
*     parameter.
*
*     The given value can be overridden by setting an environment variable,
*     MSG_PARAM (where PARAM is the tuning parameter name in upper case),
*     at run time.

*  Invocation:
*     msgTune( const char * param, int value, int *status );

*  Arguments:
*     param = const char * (Given)
*        The tuning parameter to be set (case insensitive).
*     value = int (Given)
*        The desired value (see Notes).
*     status = int * (Given and Returned)
*        The global status.

*  Notes:
*     1. The following values of PARAM may be used:
*
*        'FILTER' Specifies the required MSG conditional message reporting
*            level. VALUE may be any of the defined msglev_t values MSG__NONE
*            to MSG__ALL,
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
*            is not used. The MSG_FILTER will be read using msgIfgetenv
*            to allow symbolic names for messaging levels.
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
*     Copyright (C) 2008, 2009 Science and Technology Facilities Council.
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
*        Rewrite in C.
*     09-JAN-2009 (TIMJ):
*        FILTER values can now be MSG__NONE to MSG__ALL and assume that the
*        integer argument is one of type msglev_t.
*     27-JUL-2009 (TIMJ):
*        Use msgIfgetenv when reading MSG_FILTER environment variable since
*        that routine is much more flexible and can handle a string.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

#include "sae_par.h"
#include "msg_par.h"
#include "msg_err.h"
#include "merswrap.h"
#include "mers1.h"
#include "ems.h"

#include <string.h>

#define MIN(aa,bb) ((aa)<(bb)?(aa):(bb))

void msgTune( const char * param, int value, int * status ) {

  const char * parnames[] = { "SZOUT", "STREAM", NULL };
  const char * thispar = NULL;   /* Selected parameter */

  int i;
  int set;                 /* Value has been set */
  int env = 0;             /* Are we using the environment? */
  int npars = 0;           /* Number of parameters to read */
  int useval = 0;          /* Tuning value to actually use */
  int envval;              /* Tuning value from environment */
  int fromenv = 0;         /* Value came from environment */
  int ltune;               /* Actual tuning value used */

  if (*status != SAI__OK) return;

  /* Check for 'ENVIRONMENT' */
  if  (strcasecmp( param, "ENVIRONMENT" ) == 0) {
    env = 1;
    /* work it out ourselves */
    npars = 0;
    while ( parnames[npars] ) {
      npars++;
    }

    /* MSG_FILTER is special-cased because it can be
       a string. Override status to indicate a tuning error. */
    msgIfgetenv( status );

    if (*status != SAI__OK) {
      *status = MSG__BTUNE;
      emsRep( "MSG_TUNE_INV",
	      "msgTune: FILTER invalid value from environment variable", status );
    }

  } else {
    env = 0;
    npars = 1;
    thispar = param;
  }

  /* Now for each required parameter */
  i = 0;
  while ( *status == SAI__OK && i < npars) {

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
    envval = mers1Getenv( 1, thispar, status );
    emsRlse();

    if (envval == -1) {
      /* everything okay but no environment variable */
      useval = value;
    } else if (envval > 0 ) {
      set = 1;
      useval = envval;
      fromenv = 1;
    }

    if (*status == SAI__OK && set) {
      ltune = -1;

      /*        Check that the given parameter name is acceptable
       *        and handle it. */
      if (strcasecmp( "SZOUT", thispar) == 0) {
                if (useval == 0) {
          ltune = MSG__SZMSG;
        } else if ( useval > 0 ) {
          ltune = MIN( useval, MSG__SZMSG );
        } else {
          *status = MSG__BTUNE;
        }
        if (ltune != -1) {
          msg1Ptwsz( ltune );
        }

      } else if (strcasecmp( "STREAM", thispar) == 0 ) {

        if (useval == 0) {
          ltune = 0;
        } else if (useval == 1) {
          ltune = 1;
        } else {
          *status = MSG__BTUNE;
        }
        if (ltune != -1) msg1Ptstm( ltune );

      } else if (strcasecmp( "FILTER", thispar ) == 0 ) {
        /* This should really allow a string QUIET, VERBOSE etc
           but that would have to be a different API */
        if ( useval >= MSG__NONE && useval <= MSG__ALL ) {
          /* Assume that the supplied int is a valid msglev_t */
          msgIfset( useval, status );
        } else {
          *status = MSG__BTUNE;
        }

      } else {

        /*           The given tuning parameter was not in the available set.
         *           Set status and report an error message.
         *           We  mark and rlse to prevent possible token name clash */
        emsMark();
        *status = MSG__BDPAR;
        emsSetc( "PARAM", thispar );
        emsRep( "MSG_TUNE_PAR",
                "msgTune: Invalid tuning parameter: ^PARAM", status );
        emsRlse();
      }

      if (*status == MSG__BTUNE) {

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
        emsRep( "MSG_TUNE_INV",
                "msgTune: ^PARAM invalid value ^VALUE ^SOURCE", status);
        emsRlse();
      }
    }
  }

}

