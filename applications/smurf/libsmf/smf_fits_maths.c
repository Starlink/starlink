/*
*+
*  Name:
*     smf_fits_maths

*  Purpose:
*     Get the value of an expression in which variables are FITS headers.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     AstKeyMap *smf_fits_maths( AstKeyMap *maths, const char *exp,
*                                smfHead *hdr, double *value, int *status )

*  Arguments:
*     maths = AstKeyMap * (Given)
*        A KeyMap containing a previously compiled expression, and the
*        names of the FITS keywords used as input to the expression. May be
*        NULL if no expression has yet been compiled.
*     exp = const char * (Given)
*        A character string holding an algebraic expression in which the
*        variables are FITS keyword names. Only used if "maths" is NULL.
*     hdr = smfhead (Given)
*        The smfHead containing the FITS headers to be used when
*        evaluating the expression.
*     value = double (Returned)
*        The value of the expression. The supplied value is returned
*        unchanged if an error occurs.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Returned Value:
*     If "maths" is supplied NULL, then "exp" is compiled and a
*     pointer to a new KeyMap is returned. If "maths" is not NULL,
*     the supplied "maths" pointer is returned as the function
*     value. If "hdr" is NULL, then a NULL value is always returned.

*  Description:
*     The first call to this function should supply a NULL value for
*     "maths" and non-NULL values for "exp" and "hdr". It will identify
*     the FITS keyword names within "exp", reporting an error if any
*     keywords are  used that are not present within the supplied smfHead.
*     It will then create an AST MathMap from the expression and list of
*     FITS keyword names. It will then use this MathMap to evaluate the
*     expression, using the keyword values in the supplied smfHead. This
*     value is returned in "*value". A KeyMap containing the MathMap
*     and the list of keyword names is then created and a pointer to it
*     is returned as the function value.
*
*     On the second and subsequent calls, the KeyMap returned by the
*     first call should be supplied for "maths", and "exp" will be ignored.
*     The values for the keywords listed in the KeyMap are obtained
*     from the supplied smfHead, and the MathMap in the KeyMap is then
*     used to evaluate the expression value, which is returned in "*value".
*     The supplied "maths" pointer is returned as the function value.
*
*     The KeyMap pointer should be annulled using astAnnul when no longer
*     needed.

*  Authors:
*     David Berry (JAC)
*     {enter_new_authors_here}

*  History:
*     7-OCT-2013 (DSB):
*        Original version.

*  Copyright:
*     Copyright (C) 2013 Science and Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 3 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public
*     License along with this program; if not, write to the Free
*     Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
*     MA 02110-1301, USA

*  Bugs:
*     {note_any_bugs_here}
*-
*/

/* Starlink includes */
#include "mers.h"
#include "ast.h"
#include "sae_par.h"

/* SMURF includes */
#include "libsmf/smf.h"

#define CARDLEN 80
#define KEYLEN 8
#define MAXKEY 100

AstKeyMap *smf_fits_maths( AstKeyMap *maths, const char *exp,
                           smfHead *hdr, double *value, int *status ){

/* Local Variables: */
   AstKeyMap *result = NULL;
   AstMathMap *mathmap = NULL;
   char *fwd = NULL;
   char *keyname = NULL;
   char *names_buffer = NULL;
   char *newexp = NULL;
   char *p;
   char *token = NULL;
   char *ucexp = NULL;
   char card[ CARDLEN + 1 ];
   char template[ 20 ];
   const char **names = NULL;
   const char **tokens = NULL;
   const char *tmp;
   double *in = NULL;
   int first;
   int i;
   int ival;
   int nname;
   dim_t namlen;

/* Check inherited status. */
   if( *status != SAI__OK ) return result;

/* If no KeyMap was supplied, create one now from the supplied expression. */
   if( !maths ) {

/* Get an upper-case copy of the expression. Skip over any leading quotes. */
      ucexp = astStringCase( exp + strspn( exp, "'\""), 1 );

/* Remove any trailing quotes or spaces. */
      p = ucexp + strlen( ucexp ) - 1;
      while( p >= ucexp && ( *p == ' ' || *p == '\'' || *p == '"' ) ) *(p--) = 0;

/* Initialise. */
      names = NULL;
      nname = 0;

/* Get a list of all the FITS keyword names in the header. Loop round all
   cards in the header. */
      astClear( hdr->fitshdr, "Card" );
      while( astFindFits( hdr->fitshdr, "%f", card, 1 ) ) {

/* Ignore cards with no keyword. */
         if( card[ 0 ] == 0 || card[ 0 ] == ' ' ) continue;

/* Find the number of characters before the first space in the card.
   Restrict it to be no more than 8 characters. */
         namlen = strcspn( card, " " );
         if( namlen > KEYLEN ) namlen = KEYLEN;

/* See if this keyword name occurs in the expression. Since FITS keyword
   names are allowed to contain minus signs, and so would confuse MathMap,
   we replace the keyword name in the expression with a unique token of
   the form "TOK<n>". Loop round in case the keyword is used more than
   once within the expression. */
         sprintf( template, "(%.*s)", (int) namlen, card );
         if( !token ) {
            token = astMalloc( 10 );
            if( *status == SAI__OK ) sprintf( token, "TOK%d", nname );
         }
         first = 1;
         while( *status == SAI__OK ) {

/* Attempt to substitute the token for the FITS keyword name within the
   expression. */
            tmp = token;
            newexp = astChrSub( ucexp, template, &tmp, 1 );

/* If the expression contained the keyword, use the modified expression
   in place of the original. */
            if( newexp ) {
               (void) astFree( ucexp );
               ucexp = newexp;

/* Store the keyword name in the list of keyword in the KeyMap. We only
   do this once. */
               if( first ) {
                  keyname = astStore( NULL, card, namlen + 1 );
                  if( *status == SAI__OK ) keyname[ namlen ] = 0;
                  ival = nname++;
                  names = astGrow( names, nname, sizeof(*names) );
                  if( *status == SAI__OK ) names[ ival ] = keyname;

/* We will need a list of the corresponding tokens that occur in the
   expression passed to the MathMap constructor. */
                  tokens = astGrow( tokens, nname, sizeof(*tokens) );
                  if( *status == SAI__OK ) tokens[ ival ] = token;

/* Indicate we have found at least one occurrence of the keyword within
   the expression. */
                  first = 0;
               }

/* If the FITS keyword was not found in the expression, leave the loop. */
            } else {
               break;
            }
         }

/* If the keyword was found in the expression, we need to retain the
   memory currently pointer to by "token" sinc it is recorded within the
   "tokens" array. So nullify the token pointer, which will force new
   memory to be allocated for token when we pass on to the next FITS
   keyword. */
         if( !first ) token = NULL;
      }

/* Construct the MathMap. This will report an error if the expression
   contained any keywords that were not present in the header. Re-report
   any error using the supplied expression containing the original FITS
   keyword names, rather than the modified expression containing token
   names. */
      fwd = astMalloc( strlen( exp ) + 10 );
      if( *status == SAI__OK ) {
         sprintf( fwd, "value=%s", ucexp );
         tmp = fwd;
         mathmap = astMathMap( nname, 1, 1, &tmp, nname, tokens, " " );
         if( *status != SAI__OK ) {
            errAnnul( status );
            *status = SAI__ERROR;
            errRepf( " ", " Syntax error or unknown FITS keyword in "
                     "CHUNKWEIGHT expression '%s'.", status, exp );
         }
         fwd = astFree( fwd );
      }

/* Create the returned KeyMap. */
      result = astKeyMap( " " );
      astMapPut0A( result, "MATHMAP", mathmap, NULL );
      astMapPut1C( result, "NAMES", nname, names, NULL );

/* Free resources. */
      ucexp = astFree( ucexp );
      token = astFree( token );
      if( tokens ) {
         #pragma GCC diagnostic ignored "-Wcast-qual"
         for( i = 0; i < nname; i++ ) tokens[ i ] = astFree( (void *) tokens[ i ] );
         #pragma GCC diagnostic pop
         tokens = astFree( tokens );
      }

/* If a KeyMap was supplied, we also return it. Extract the required
   items from it. */
   } else {
      result = maths;
      astMapGet0A( result, "MATHMAP", &mathmap );
      nname = astMapLength( result, "NAMES" );
      names = astMalloc( nname*sizeof( *names ) );
      names_buffer = astMalloc( nname*( KEYLEN + 1 ) );
      astMapGet1C( result, "NAMES", KEYLEN + 1, nname, &i, names_buffer );
      for( i = 0; i < nname; i++ ) {
         names[ i ] = names_buffer + i*( KEYLEN + 1 );
      }
   }

/* Check we can use result safely. */
   if( *status == SAI__OK ) {

/* Allocate memory to hold the values of the required FITS keywords. */
      in = astMalloc( nname*sizeof( *in ) );

/* Get the values of the required FITS keywords from the header. */
      for( i = 0; i < nname && *status == SAI__OK; i++ ) {
         in[ i ] = VAL__BADD;
         smf_getfitsd( hdr, names[ i ], in + i, status );
         if( in[ i ] == VAL__BADD && *status == SAI__OK ) {
            *status = SAI__ERROR;
            errRepf( " ", "The FITS keyword '%s' has an undefined value.",
                     status, names[ i ] );
         }
      }

/* Transform the values using the MathMap. */
      astTranN( mathmap, 1, nname, 1, in, 1, 1, 1, value );
   }

/* Free resouces. */
   in = astFree( in );

   if( maths ) {
      names = astFree( names );
      names_buffer = astFree( names_buffer );
   } else if( names ){
      for( i = 0; i < nname; i++ ) {
         #pragma GCC diagnostic ignored "-Wcast-qual"
         names[ i ] = astFree( (void *) names[ i ] );
         #pragma GCC diagnostic pop
      }
      names = astFree( names );
   }

   mathmap = astAnnul( mathmap );

/* Report a context message if anything went wrong. */
   if( *status != SAI__OK ) {
      errRepf( " ", " Failed to get a value for CHUNKWEIGHT expression "
               "'%s'.", status, exp );
   }

   return result;
}


