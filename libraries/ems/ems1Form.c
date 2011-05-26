/*
 *+
 *  Name:
 *     ems1Form

 *  Purpose:
 *     Form an error message from its text and components.

 *  Language:
 *     Starlink ANSI

 *  Invocation:
 *     ems1Form( text, maxlen, esctokval, clean, opstr, oplen, status )

 *  Description:
 *     Construct the final text, OPSTR( 1 : OPLEN ), of a message
 *     using the text in TEXT, and available message tokens only.

 *  Arguments:
 *     text = char* (Given)
 *        The input message text, with any tokens.
 *     maxlen = const int (Given)
 *        Maximum length of output string
 *     esctokval = Logical (Given)
 *        if true, token values containing % will be escaped into %%
 *        to enable sprintf processing to be performed on the expanded
 *        string subsequently.
 *     clean = logical (Given)
 *        if the string is to be 'cleaned'
 *     opstr = char* (Returned)
 *        Resultant message text, with parsed tokens.
 *     oplen = int* (Returned)
 *        The filled length of OPSTR.
 *     status = int* (Returned)
 *        The global status.

 *  Algorithm:
 *     -  Parse the message text, TEXT, and copy it into OPSTR, making
 *     translations for token escapes.
 *     -  The message length is returned in OPLEN.

 *  Notes:
 *     The code assumes "%" is the escape character to be doubled up in
 *     token values. This is done for simplicity and can easily be made
 *     generic by changing esctokval to be a const char *.

 *  Copyright:
 *     Copyright (C) 1982 Science & Engineering Research Council.
 *     Copyright (C) 2001 Central Laboratory of the Research Councils.
 *     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
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
 *     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
 *     02111-1307, USA

 *  Authors:
 *     JRG: Jack Giddings (UCL)
 *     BDK: Dennis Kelly (ROE)
 *     AJC: Alan Chipperfield (STARLINK)
 *     PCTR: P.C.T. Rees (STARLINK)
 *     RTP: R.T. Platon (STARLINK)
 *     TIMJ: Tim Jenness (JAC, Hawaii)
 *     PWD: Peter W. Draper (JAC, Durham University)
 *     {enter_new_authors_here}

 *  History:
 *     3-JAN-1982 (JRG):
 *        Original FORTRAN version.
 *     14-FEB-2001 (RTP)
 *        Rewritten in C based on the Fortran routine EMS1_PFORM
 *      5-MAR-2001 (AJC):
 *        Set returned value OPLEN
 *        Use a temporary buffer for text amnipulation
 *        Use ems1Putc to append to string
 *        Add 'maxlen' argument
 *        Remove 'param' argument
 *     13-AUG-2001 (AJC):
 *        #include stdlib.h and ems1.h
 *     23-FEB-2006 (TIMJ):
 *        Use starMem
 *     15-MAY-2008 (PWD):
 *        Remove ems_msgtb.h include, not needed.
 *     23-DEC-2008 (TIMJ):
 *        use isprint() to filter out non-printable characters. Rather
 *        than checking for char < 32. Add argument to enable escaping
 *        of % characters in token values. Required for safe sprintf processing.
 *     {enter_further_changes_here}

 *  Bugs:
 *     {note_any_bugs_here}
 */

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <ctype.h>

#include "sae_par.h"                 /* Standard SAE constants */
#include "ems_par.h"                 /* EMS_ public constants */
#include "ems_sys.h"                 /* EMS_ private constants */
#include "ems1.h"                    /* EMS1 function prototypes */
#include "ems.h"
#include "star/mem.h"

void ems1Form( const char *text, const int maxlen, Logical esctokval,
               Logical clean, char *opstr, int *oplen,
               int *status __attribute__((unused)) )
{
    Logical literl;              /* Whether a literal token escape */

    int curpos;                  /* Current character pointer */
    int lstat;                   /* Local status */
    int lstpos;                  /* Previous value of CURPOS */
    int oppos;                   /* Position in OP string */
    int namlen;                  /* Token name length */
    int pstat;                   /* Local status */
    int texlen;                  /* TEXSTR length */
    int tkvlen;                  /* TOKVAL length */
    int j;
    int i;

    char namstr[ EMS__SZMSG + 1 ] = ""; /* Token name string */
    char tokval[ EMS__SZTOK + 1 ] = ""; /* Token value string */
    char tstr[EMS__SZTOK + 1 ];  /* Escaped token value temp string buffer */
    const char tokescval = '%';  /* Escape value to look for in token values */
    char *texbuf;                /* Ptr to temporary text buffer */

    TRACE("ems1Form");

    /*  Initialise the returned message. */
    opstr[0] = '\0';
    *oplen = 0;

    /*  Get the length of the given string. */
    if (!text) {
      strcpy( opstr, "Internal malloc failure. Abort!" );
      *oplen = strlen( opstr );
      /*  Clear the message token table. */
      ems1Ktok();
      return;
    }
    texlen = strlen( text );

    /*  Check for an empty string. */
    if ( texlen > 0 ) {

        /*  Use a temporary text buffer which can be modified */
        texbuf = starMalloc( texlen + 1 );
        if (!texbuf) {
          strcpy( opstr, "Internal malloc failure. Abort!" );
          *oplen = strlen( opstr );
          /*  Clear the message token table. */
          ems1Ktok();
          return;
        }
        strcpy( texbuf, text );

        /*  Initialise the text pointers and local status. */
        literl = FALSE;
        curpos = -1;
        lstpos = -1;
        oppos = -1;
        lstat = SAI__OK;
        pstat = SAI__OK;

        /*  Parse and expand the message text.
         *  DO WHILE loop.
         */
        while ( pstat == SAI__OK && curpos < texlen ) {

            /*  Find the next occurrence of an escape character. */
            ems1Gesc( EMS__TOKEC, texbuf, &curpos );

            if ( curpos == -1 ) {

                /*  No more escape characters have been found, so append all
                 *  the text that remains to the returned message text and
                 *  exit the loop.
                 */
                ems1Putc( &texbuf[ lstpos+1 ], maxlen, opstr, &oppos, &pstat );
                break;

            } else {
                /*  A token escape has been found, so check if it is a double
                 *  token escape.
                 */
                if ( literl && curpos == lstpos + 1 ) {
                    /*  A double token escape, so do nothing except update
                     *  CURPOS and reset the literal token escape flag.
                     */
                    literl = FALSE;

                } else {

                    /*  Reset the literal token escape flag. */
                    literl = FALSE;

                    /*  Append any text prior to the escape character. */
                    /*  Terminate the string to be copied at the escape */
                    if ( curpos - lstpos > 0 ) {
                        texbuf[ curpos ] = '\0';
                        (void)ems1Putc( &texbuf[ lstpos+1 ], maxlen, opstr,
                                        &oppos, &pstat );

                        /*  Overwrite the terminator - actual character
                         *  doesn't matter */
                        texbuf[ curpos ] = '^';
                    }

                    /*  Find the token name. */
                    ems1Gnam( texbuf, &curpos, namstr, &namlen, &lstat );

                    /*  Check that a token name exists. */
                    if ( lstat != SAI__OK ) {

                        /*  The name string has been over-run, so indicate
                         *  this in the message text. */
                        ems1Putc( EMS__TOKEC, maxlen, opstr, &oppos, &pstat );
                        ems1Putc( "<", maxlen, opstr, &oppos, &pstat );
                        ems1Putc( namstr, maxlen, opstr, &oppos, &pstat );
                        ems1Putc( ">", maxlen, opstr, &oppos, &pstat );

                        /*  Reset the local status. */
                        lstat = SAI__OK;

                    } else if ( namlen > 0 ) {

                        /*  There is a name, so get the token value. */
                        if ( ems1Gtok( namstr, tokval, &tkvlen ) ) {

                          /* if there is an escape character in the token
                             itself we need to double it up. This is used
                             to enable a % in the token to be retained
                             after sprintf processing */
                          if ( esctokval ) {
                            j=0;
                            for ( i = 0; i < tkvlen; i++) {
                              /* if we have found the escape char we
                                 insert an additional one */
                              if (tokval[i] == tokescval) {
                                tstr[j] = tokescval;
                                j++;
                              }
                              /* copy the character that was tested */
                              tstr[j] = tokval[i];
                              j++;
                            }
                            tkvlen = j;
                            tstr[j] = '\0';
                          } else {
                            strncpy( tstr, tokval, sizeof(tstr));
                            tstr[tkvlen] = '\0';
                          }

                            /*  Append the token value to the expanded message
                             *  text. */
                            ems1Putc( tstr, maxlen, opstr, &oppos, &pstat );

                        } else {
                            /*  The message token is not defined, so indicate
                             *  this in the message text. */
                            ems1Putc( EMS__TOKEC, maxlen, opstr, &oppos,
                                      &pstat );
                            ems1Putc( "<", maxlen, opstr, &oppos, &pstat );
                            ems1Putc( namstr, maxlen, opstr, &oppos, &pstat );
                            ems1Putc( ">", maxlen, opstr, &oppos, &pstat );
                        }

                    } else {

                        /*  There is an isolated token escape character in the
                         *  string, so set the literal token escape flag and
                         *  append the character. */
                        literl = TRUE;
                        ems1Putc( EMS__TOKEC, maxlen, opstr, &oppos, &pstat );
                    }
                }

                /*  Update LSTPOS. */
                lstpos = curpos;
            }
        }
        starFree( texbuf );
    }

    /*  Get the length of the string and, if CLEAN is TRUE, ensure the
     *  returned message string contains no unprintable characters.
     */
    *oplen = strlen( opstr );
    if ( clean ) {
        int i;
        for ( i = 0; i < *oplen; i++ ) {
          if ( !isprint(opstr[ i ]) ) opstr[ i ] = ' ';
        }
    }

    DEBUG( "ems1Form", "Error Message: '%s'\n", opstr );
#if PRINT_ERRMESS
    fprintf( OP_STREAM, "! %s\n", opstr );
    fflush( OP_STREAM );
#endif

    /*  Clear the message token table. */
    ems1Ktok();

    return;
}
