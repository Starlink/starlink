#include <stdlib.h>
#include <string.h>
#include "sae_par.h"
#include "ndf_err.h"
#include "ndf1.h"
#include "mers.h"

void ndf1Vclst( const char *text, int ncomp, const char *cnames[],
                int cflags[], int *nset, int *status ){
/*
*+
*  Name:
*     ndf1Vclst

*  Purpose:
*     Validate a comma-separated list of NDF component names.

*  Synopsis:
*     void ndf1Vclst( const char *text, int ncomp, const char *cnames[],
*                     int cflags[], int *nset, int *status )

*  Description:
*     This function checks each word in a supplied comma-separated
*     character string. If a word is a valid abbreviation for one of the
*     NDF components included in the supplied "cnames" array, then the
*     corresponding element in "cflags" is set non-zero on return. If a
*     word is "*" then all flags in the returned "cflags" array are set
*     non-zero If a word is anything else, an error is reported.

*  Parameters:
*     text
*        Pointer to a null terminated string holding the supplied comma-
*        separated list of words.
*     ncomp
*        The length of the "cnames" and "cflags" arrays.
*     cnames
*        Pointer to an array of pointers to null terminated strings. Each
*        element should holds the full upper case name of an acceptable
*        NDF component. The supplied "cnames" array should have at least
*        "ncomp" elements.
*     cflags
*        On exit, each element is set non-zero if and only if the NDF
*        component named in the corresponding element of "cnames" was
*        included in the list of components supplied via "text". The
*        supplied "cflags" array should have at least "ncomp" elements.
*     *nset
*        Returned holding the on exits, holds the number of non-zero values
*        in "cflags".
*     *status
*        The global status.

*  Copyright:
*     Copyright (C) 2018 East Asian Observatory
*     All rights reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or modify
*     it under the terms of the GNU General Public License as published by
*     the Free Software Foundation; either version 2 of the License, or (at
*     your option) any later version.
*
*     This program is distributed in the hope that it will be useful,but
*     WITHOUT ANY WARRANTY; without even the implied warranty of
*     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
*     General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     RFWS: R."f". Warren-Smith (STARLINK)
*     DSB: David S. Berry (EAO)

*  History:
*     xxx (DSB):
*        Original version, based on equivalent Fortran function by RFWS.

*-
*/

/* Local Variables: */
   char *word=NULL;      /* Dynamic memory holding current word */
   const char *pend;     /* Pointer to next comma */
   int icomp;            /* Index of allowed component name */
   int ok;               /* Is current word an allowed component name? */
   size_t clen;          /* Length of supplied string */
   size_t end;           /* Index of end of word maybe with spaces */
   size_t nc;            /* Number of characters in word */
   size_t start;         /* Index of start of word maybe with spaces */

/* Set an initial value for the "nset" parameter. */
   *nset = 0;

/* Check the inherited status */
   if( *status != SAI__OK ) return;

/* Initialise all returned flags to zero */
   for( icomp = 0; icomp < ncomp; icomp++ ){
      cflags[ icomp ] = 0;
   }

/* Get the length of the supplied string. */
   clen = strlen( text );

/* Initialise the index within "text" of the start of the current word. */
   start = 0;

/* Loop until the whole string has been read or an error occurs. */
   while( start < clen && *status == SAI__OK ){

/* Find the next comma following the word start. */
      pend = strchr( text + start, ',' );

/* If no comma was found, the current word extends to the end of the string */
      if( !pend ) {
         end = clen - 1;

/* If a comma was found, the current word extends to the character just
   before the comma. */
      } else {
         end = pend - text - 1;
      }

/* Extract the word into a dynamic buffer, removing leading and trailing
   spaces. */
      word = ndf1Strip( word, text, start, end, &nc, NULL, status );
      if( *status == SAI__OK ) {

/* Report an error if the word is blank. */
         if( nc == 0 ) {
            *status = NDF__CNMIN;
            errRep( " ", "Invalid blank array component name specified "
                    "(possible programming error).", status );

/* Otherwise, if the word consists of a single asterisk, set all returned
   flags non-zero. Continue looping to check any remaining words for
   validity. */
         } else {
            if( !strcmp( word, "*" ) ) {
               for( icomp = 0; icomp < ncomp; icomp++ ){
                  cflags[ icomp ] = 1;
               }
               *nset = ncomp;

/* Otherwise, compare the word to each of the allowed component names */
            } else {
               ok = 0;
               for( icomp = 0; icomp < ncomp; icomp++ ){
                  if( ndf1Simlr( word, 1, 0, cnames[ icomp ], NDF__MINAB ) ) {

/* If the current word matches the current component name, set the
   returned flag non-zero and increment the number of selected components,
   so long as the component has not already been selected. Set a flag
   indicating that the current word is legal. */
                     if( !cflags[ icomp ] ) {
                        cflags[ icomp ] = 1;
                        (*nset)++;
                     }
                     ok = 1;
                  }
               }

/* Report an error if the current word was not an allowed component name. */
               if( !ok ) {
                  *status = NDF__CNMIN;
                  msgSetc( "BADCOMP", word );
                  errRep( " ", "Invalid or inappropriate NDF component "
                          "name '^BADCOMP' specified (possible programming "
                          "error).", status );
               }
            }
         }
      }

/* Move on one character from the end of the current word to reach the
   comma (if any) following the word, then move on one more character to
   get to the start of the next word. */
      start = end + 2;
   }

/* Free the word buffer. */
   word = astFree( word );

/* Call error tracing function and exit. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1Vclst", status );

}

