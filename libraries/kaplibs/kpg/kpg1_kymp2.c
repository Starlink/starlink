#include "f77.h"
#include "mers.h"
#include "sae_par.h"
#include "ast.h"
#include "kaplibs_private.h"
#include <string.h>
#include <ctype.h>

/* Local Constants: */ 
#define MAX_COMPLEN 100

void kpg1Kymp2( const char *string, AstKeyMap *keymap, int *status ){
/*
*+
*  Name:
*     kpg1Kymp2

*  Purpose:
*     Parse a "keyword = value" string for kpg1Kymp1 and add to a KeyMap.

*  Language:
*     C.

*  Synopsis:
*     void kpg1Kymp2( const char *string, AstKeyMap *keymap, int *status );

*  Description:
*     This is a service function for kps1Kymp1. It parses the supplied 
*     "keyword = value" string into a keyword and value. It then parses the 
*     keyword into a list of dot-separated component names. It then adds
*     the value into the supplied KeyMap at the correct point.

*  Parameters
*     string
*        The null-terminated "keyword=value" string to be parsed.
*     keymap
*        A pointer to the KeyMap in which to store the value.
*     status
*        Pointer to the global status variable.

*  Notes:
*     - Any equals signs that are included in the value string must be doubled.
*     - Component names must contain only alphanumerical characters,
*     underscores, plus and minus signs [a-zA-Z0-9_+\-], 
*     - Any lower case characters contained in a component name will be
*     translated to the upper case equivalent.

*  Copyright:
*     Copyright (C) 2005 Particle Physics & Astronomy Research Council.
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
*     DSB: David S. Berry
*     {enter_new_authors_here}

*  History:
*     30-SEP-2005 (DSB):
*        Original version.
*     17-MAY-2006 (DSB):
*        Report an error if the value string contains any isolated equals
*        signs. 
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

/* Local Variables: */ 
   AstObject *obj;              /* Pointer to generic AST Object */
   AstKeyMap *compmap;          /* KeyMap holding components of 1st component */
   const char *c;               /* Pointer to next char to examine */
   const char *cend;            /* Pointer to 1st char after component name */
   const char *dot;             /* Pointer to 1st dot */   
   const char *equals;          /* Pointer to 1st equals sign */   
   char buf[ 10 ];              /* Text buffer for message symbols */
   char comp[ MAX_COMPLEN + 1 ];/* Buffer for component name */
   int alert;                   /* Was previous character an equals sign? */
   int clen;                    /* Length of component name */

/* Check the inherited status. */
   if( *status != SAI__OK ) return;

/* Locate the first equals sign. */
   equals = strchr( string, '=' );  

/* Report an error if no equals sign was found. */
   if( !equals ) {
      *status = SAI__ERROR;
      msgSetc( "S", string );
      errRep( "KPG1KYMP2_ERR1", "No equals sign found in \"^S\".",
               status );
      goto L999;
   }

/* Report an error if the equals sign is the last non-blank character. */
   if( astChrLen( equals + 1 ) == 0 ) {
      *status = SAI__ERROR;
      msgSetc( "S", string );
      errRep( "KPG1KYMP2_ERR2", "No value found in \"^S\".", status );
      goto L999;
   }

/* Report an error if the value part of the string contains any equals signs 
   that are not doubled. */
   alert = 0;
   c = equals;
   while( *(++c) ) {
      if( *c == '=' ) {
         if( alert ) {
            alert = 0;
         } else {
            alert = 1;
         }

      } else if( alert ) {
         *status = SAI__ERROR;
         msgSetc( "S", string );
         errRep( "KPG1KYMP2_ERR6", "Missing commas between values "
                 "in \"^S\".", status );
         goto L999;

      } else {
         alert = 0;
      }
   }         

/* Locate the first dot. */
   dot = strchr( string, '.' );  

/* The end of the first component is the earlier of the first equals sign
   and the first dot. */
   cend = ( !dot || equals < dot ) ? equals : dot;

/* Copy the first component name into a local string, checking that it
   contains no illegal characters, converting lower case to upper case and
   ignoring white space. */
   clen = 0;
   for( c = string; c < cend; c++ ) {
      if( !isspace( *c ) ) {
         if( !isalnum( *c ) && *c != '_' && *c != '-' && *c != '+' ) {
            *status = SAI__ERROR;
            buf[ 0 ] = *c;
            buf[ 1 ] = 0;
            msgSetc( "C", buf );
            msgSetc( "S", string );
            errRep( "KPG1KYMP2_ERR3", "Keyword contains illegal character "
                     "\"^C\" in \"^S\".", status );
            goto L999;

         } else if( clen == MAX_COMPLEN ) {
            *status = SAI__ERROR;
            msgSetc( "S", string );
            errRep( "KPG1KYMP2_ERR4", "Keyword component is too long "
                     "in \"^S\".", status );
            goto L999;

         } else {
            comp[ clen++ ] = islower( *c ) ? toupper( *c ) : *c;
         }
      }
   }

/* Check the first component name is not null. */
   if( clen == 0 ) {
      *status = SAI__ERROR;
      msgSetc( "S", string );
      errRep( "KPG1KYMP2_ERR5", "Keyword contains null name in \"^S\".",
               status );
      goto L999;
   }

/* Terminate the copy of the component name */
   comp[ clen ] = 0;

/* If the supplied keyword name contained only a single component, we will
   add the value to the supplied keymap. */
   if( cend == equals ) {
      astMapPut0C( keymap, comp, equals + 1, NULL );

/* Otherwise, get another KeyMap in which to store the components
   following this first component. If the supplied KeyMap already
   contains such a KeyMap, use it. Otherwise create a new one and store
   it in the supplied KeyMap. */
   } else {

      if( astMapGet0A( keymap, comp, &obj ) ) {
         compmap = (AstKeyMap *) obj;
      } else {
         compmap = astKeyMap( "" );
         astMapPut0A( keymap, comp, compmap, NULL );
      } 

/* Call this function recursively to store the component value. */
      kpg1Kymp2( dot + 1, compmap, status );

/* Free the component KeyMap pointer. */
      compmap = astAnnul( compmap );

   }

/* Finish */
L999:;

}
