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
*     Construct the final text of a message using the text in "text",
*     and available message tokens, keywords and parameter-object associations.
*     Clean any non-printing characters from the final string if "clean"
*     is true.
*
*     Keyword associations are disabled if "useformat" is true.

*  Arguments:
*     param = const char * (Given)
*        The message parameter name.
*     text = const char * (Given)
*        The input message text, with any tokens.
*     clean = int (Given)
*        If the string is to be 'cleaned'
*     useformat = int (Given)
*        Boolean indicating whether the supplied text needs to take account
*        of sprintf formatting. See notes for additional information.
*     msglen = size_t (Given)
*        Allocated size of buffer "msgstr".
*     msgstr = char * (Returned)
*        Resultant message text, with parsed tokens.
*     status = int * (Given)
*        The global status.

*  Implementation Notes:
*     -  This subroutine is the ADAM version of msg1Form

*  Notes:
*     If "useformat" is true Keyword association is disabled because the
*     "%KEYWORD" syntax is hard to disambiguate from a sprintf %x specifier.
*     Additionally, token replacement will be modified to replace any %
*     in tokens with %% in preparation for a sprintf call.

*  Algorithm:
*     -  Attempt to get a message text from the parameter system,
*     otherwise use "text".
*     -  Parse the message text and copy it into "msgstr", making
*     translations for parameter, status and token escapes.
*     -  The string will be null terminated.

*  Copyright:
*     Copyright (C) 1982, 1984, 1985, 1989-1991, 1994 Science & Engineering
*     Research Council.
*     Copyright (C) 1999, 2001 Central Laboratory of the Research Councils.
*     Copyright (C) 2008 Science and Technology Facilities Council.
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
*     BDK: Dennis Kelly (ROE)
*     AJC: Alan Chipperfield (STARLINK)
*     PCTR: P.C.T. Rees (STARLINK)
*     BKM: B.K.McIlwrath (STARLINK)
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
*     13-MAR-1990 (PCTR):
*        Converted to use EMS_ calls and changed subroutine name.
*     9-APR-1990 (PCTR):
*        Replaced DO WHILE construct with ANSI Fortran 77 equivalent.
*     9-OCT-1991 (PCTR):
*        New token parsing algorithm.
*     24-OCT-1991 (PCTR):
*        Removed bugs from new token parsing algorithm.
*     12-AUG-1994 (BKM):
*        Change reserved token STATUS to call EMS_FACER (from EMS_SYSER)
*     15-SEP-1999 (AJC):
*        Add argument CLEAN
*     21-FEB-2001 (AJC):
*        Remove use of internal EMS routines
*        Use EMS_EXPND instead of EMS1_GTOK
*        EMS1_PUTC is now MSG1_PUTC
*        Use MSG1_KTOK not EMS1_KTOK
*     19-OCT-2001 (AJC):
*        Correct finding ^STATUS if previous token in message
*     05-SEP-2008 (TIMJ):
*        Rewrite in C.
*     26-NOV-2008 (TIMJ):
*        Fix ^STATUS expansion.
*     23-DEC-2008 (TIMJ):
*        Disable keyword associations if sprintf formats are required.
*     12-JAN-2008 (TIMJ):
*        Fix logic with STREAM mode and with % escaping.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

/* for strtok_r in standards compliance mode */
#if HAVE_STRTOK_R
#define _POSIX_C_SOURCE 200112L
#endif

#if HAVE_CONFIG_H
#  include <config.h>
#endif

#include "sae_par.h"
#include "ems.h"
#include "msg_par.h"
#include "msg_sys.h"

#include "star/util.h"

#include "mers1.h"

#include <ctype.h>
#include <string.h>

/* Name of token representing STATUS error string */
#define STATSTR "STATUS"

void msg1Form ( const char * param, const char * text, int clean,
                int useformat, size_t msglen, char * msgstr, int * status ) {

  /*  Local Variables: */
  int define;          /* Whether a token is defined */
  int hasdouble = 0;   /* Whether a double escape */
  int literl;          /* Whether a literal token escape */

  int iposn;           /* Position in output string */
  int curpos;          /* Current character position */
  int lstat = SAI__OK; /* Local status */
  int lstpos;          /* Previous value of curpos */
  int namlen;          /* Name string length */
  int oldstm;          /* Initial EMS STREAM setting */
  int pstat = SAI__OK; /* Local status */
  int texlen;          /* message text length */
  int tkvlen;          /* Token value length */

  char escape[2];      /* Current escape character */
  char escstr[3];      /* Escape string */
  char namstr[MSG__SZMSG]; /* Token name string */
  char stmsg[MSG__SZTOK];  /* STATUS message */
  char prevec[2];      /* Previous escape character */
  char texst0[MSG__SZMSG];
  char texst1[MSG__SZMSG]; /* Message text string */
  char texst2[MSG__SZMSG]; /* Part expanded text string */
  char tokval[MSG__SZTOK]; /* Token value string */

  char *tokctx = NULL;   /* Context argument for strtok_r */
  char *token = NULL;    /* Pointer to token in string */

  size_t szst;           /* Length of STATUS string */
  ptrdiff_t offset;      /* offset into string */

  /* Can not do anything if we have a null pointer */
  if (!msgstr || msglen == 0) return;

  /*  Operate regardless of STATUS - use local status */
  lstat = SAI__OK;

  /*  Select the message text from the interface module or program. */
  if ( !msg1Genv( param, texst1, sizeof(texst1) ) ) {
    star_strlcpy( texst1, text, sizeof(texst1) );
  }

  /*  Initialise the returned message. */
  msgstr[0] = '\0';

  /*  Check for an empty string. */
  if (strlen(texst1)) {

    /* Check for the STATUS token in text If it's there, substitute it
     * here and now.  We must do this to avoid concatenation if
     * ^STATUS is used after RENEW.  However, it means that ^STATUS is
     * now truly reserved and cannot be preset for concatenation.  We
     * assume there is not more than one occurrence of ^STATUS within
     * TEXT */

    /* Since sttrok twiddles with the string we need to take a copy */
    star_strlcpy( texst0, texst1, sizeof(texst0) );

    /* Find a token */
#if HAVE_STRTOK_R
    token = strtok_r( texst1, "^", &tokctx );
#else
    token = strtok( texst1, "^" );
#endif

    /* Get the length of the token string we are looking for */
    szst = strlen( STATSTR );

    while (token) {

      /* Is it STATUS?  - noting that ^STATUSX will also match.
       Note that first time round the loop strtok will return the start
       of string so we have to check that we do not match a bare STATUS
       at the start (there can never be a token as first character since
       a ^ would point us to the second character). */
      if (token != texst1 && strncmp( token, STATSTR, szst ) == 0) {

        /*       It is STATUS
         *       Find the associated message */
        emsMark();
        emsFacer( STATSTR, *status );
        emsExpnd( "^" STATSTR, stmsg, sizeof(stmsg), useformat, &tkvlen,
                  &lstat );
        emsRlse();

        /* Need to put this text back into original */

        /* Save the text that follows ^STATUS - it has to be copied
         from texst0 since token aka texst1 is modified by strok on the
         first call. */
        offset = token - texst1 + szst;
        star_strlcpy( texst2, &(texst0[offset]), sizeof(texst2) );

        /* Work out how far into the buffer this ^STATUS was found, accounting
         for the fact that "token" reports a value one larger than the position
         of the ^ */
        offset = token - texst1 - 1;

        /* Append the text from the STATUS token and the subsequent text */
        texst0[offset] = '\0';
        star_strappend( texst0, stmsg, sizeof(texst0) );
        star_strappend( texst0, texst2, sizeof(texst0) );

        break;

      }

      /* Keep on looking */
#if HAVE_STRTOK_R
      token = strtok_r( NULL, "^", &tokctx );
#else
      token = strtok_r( NULL, "^" );
#endif
    }

    /*     Make a first pass to expand plain ^ tokens
     *     This will also kill tokens  - note that we have to force
     *     STREAM mode in EMS so that we can control it ourselves
     *     later on */
    oldstm = emsStune( "STREAM", !clean, &lstat );
    emsExpnd( texst0, texst2, sizeof(texst2), useformat, &texlen, &lstat);
    (void) emsStune( "STREAM", oldstm, &lstat );

    /*     Initialise token escape state flag. */
    literl = 0;

    /*     Initialise the text pointers and local status. */
    curpos = -1;
    lstpos = -1;
    iposn = 0;
    lstat = SAI__OK;
    pstat = SAI__OK;

    /*     Initialise the escape string and previous escape character.
           Note that KEY escapes are not included if useformat is true and
           if it happens to be a "%" that we use for params.
     */
    star_strlcpy( escstr, MSG__REFEC, sizeof(escstr) );
    if (!(useformat && strcmp(MSG__KEYEC, "%") == 0)) {
      star_strlcat( escstr, MSG__KEYEC, sizeof(escstr) );
    }
    prevec[0] = '\0';
    escape[0] = '\0';
    prevec[1] = '\0';
    escape[1] = '\0';

    /*     Parse and translate the returned message text. */
    while (1) {

      if (pstat == SAI__OK && curpos < texlen) {

        /*        Check if a double or paired escape character sequence has
         *        occurred. */
        if (prevec[0] == escape[0] ) {
          hasdouble = 1;
        } else {
          hasdouble = 0;
        }

      } else {

        /*        No paired token escapes, so annul the DOUBLE flag */
        hasdouble = 0;

      }

      /*     Find the next occurrence of an escape character. */
      ems1Gesc( escstr, texst2, &curpos );

      /*     Append any text prior to the escape character to the returned
       *     string. */
      if (curpos == -1) {

        /*        No more escape characters have been found, so append all
         *        the text that remains to the returned message text and exit
         *        the loop. */
        pstat = star_strappend( msgstr, &(texst2[lstpos+1]), msglen );
        break;
      } else {

        /*        A token escape has been found, so get which escape character
         *        has occurred. */
        escape[0] = texst2[curpos];

        /*        Check if it is a double token escape. */
        if (literl && curpos == lstpos + 1 ) {

          /*    Check if a double escape character sequence has occurred. */
          if (escape[0] == prevec[0]) {
            hasdouble = 1;
          } else {
            hasdouble = 0;
          }

        } else {

          /*  No paired token escapes, so annul the DOUBLE flag */
          hasdouble = 0;
        }

        /*           Act. */
        if (hasdouble) {

          /*           A double token escape, so do nothing except update
           *              CURPOS and reset the literal token escape flag. */
          literl = 0;

          /*           Assign a "null" ESCAPE. */
          escape[0] = '\0';

        } else {

          /*           Reset the literal token escape flag. */
          literl = 0;

          /*           Append any text prior to the escape character. */
          if (curpos >= 0) {
            texst2[curpos] = '\0';
            star_strappend( msgstr, &texst2[lstpos+1], msglen);
            texst2[curpos] = escape[0];
          }

          /*           Find the token name - curpos is updated. */
          ems1Gnam( texst2, &curpos, namstr, &namlen, &lstat );

          /*           Check that a token name exists. */
          if (lstat != SAI__OK) {
            /*              The name string has been over-run, so indicate this
             *              in the message text. */
            star_strappend( msgstr, escape, msglen);
            star_strappend( msgstr, "<", msglen);
            star_strappend( msgstr, namstr, msglen);
            star_strappend( msgstr, ">", msglen);

            /*              Reset the local status. */
            lstat = SAI__OK;

          } else if (namlen > 0) {
            define = 0;
            if ( strcmp(escape, MSG__REFEC) == 0) {

              /*                 Get a value for the object name token. */
              define = msg1Gref( namstr, tokval, sizeof(tokval) );

            } else if (strcmp(escape, MSG__KEYEC) == 0) {

              /*                 Get a value for the keyword token.*/
              define = msg1Gkey( namstr, tokval, sizeof(tokval) );
            }

            /*          Check if the token value was defined. */
            if (define) {
              /*            Append the token value to the returned string. */
              star_strappend( msgstr, tokval, msglen );
            } else {
              /*                 A value has not been found, so append the
               *                 "undefined" token string. */
              star_strappend( msgstr, escape, msglen);
              star_strappend( msgstr, "<", msglen);
              star_strappend( msgstr, namstr, msglen);
              star_strappend( msgstr, ">", msglen);
            }

            /*              Assign a "null" ESCAPE. */
            escape[0] = '\0';

          } else {
            /*              There is an isolated token escape character in the
             *              string, so set the literal token escape flag. */
            literl = 1;
            star_strappend( msgstr, escape, msglen );

          }
        }

        /*        Update LSTPOS and PREVEC. */
        lstpos = curpos;
        prevec[0] = escape[0];
      }

    }

    /*  Clean the returned message string unless in STREAM mode. */
    if (clean) {
      size_t i;
      size_t len;
      len = strlen(msgstr);
      for (i = 0; i < len; i++) {
        if (!isprint( msgstr[i] )) {
          msgstr[i] = ' ';
        }
      }
    }

  }
}
