#include "f77.h"
#include "mers.h"
#include "sae_par.h"
#include "ast.h"
#include "kaplibs_private.h"
#include <string.h>
#include <ctype.h>

/* Local Constants: */
#define MAX_COMPLEN 100

void kpg1Kymp2( const char *string, const char *ind, AstKeyMap *keymap,
                int *status ){
/*
*+
*  Name:
*     kpg1Kymp2

*  Purpose:
*     Parses a "keyword = value" string for kpg1Kymp1 and add to a KeyMap.

*  Language:
*     C.

*  Invocation:
*     void kpg1Kymp2( const char *string, const char *ind, AstKeyMap *keymap,
*                     int *status );

*  Description:
*     This is a service function for kps1Kymp1. It parses the supplied
*     "keyword = value" string into a keyword and value. It then parses the
*     keyword into a list of dot-separated component names. It then adds
*     the value into the supplied KeyMap at the correct point.

*  Arguments:
*     string
*        The null-terminated "keyword=value" string to be parsed.
*     ind
*        The GRP indirection character - only used in error messages.
*     keymap
*        A pointer to the KeyMap in which to store the value.
*     status
*        Pointer to the global status variable.

*  Notes:
*     - If the value is a comma-separated list of values, enclosed in
*     parentheses, then a vector entry will be added to the KeyMap.
*     Otherwise, a scalar entry will be created.
*     - To include a comma or a closing parenthesis literally in a vector
*     value, preceed it with a backslash.
*     - If the string has the form "keyword=<def>" (case insensitive), then
*     any entry for the specified keyword is removed from the KeyMap.
*     - Component names must contain only alphanumerical characters,
*     underscores, parentheses, plus and minus signs [a-zA-Z0-9_+\-],
*     - Any lower case characters contained in a component name will be
*     translated to the upper case equivalent.

*  Copyright:
*     Copyright (C) 2005 Particle Physics & Astronomy Research Council.
*     Copyright (C) 2008-2010 Science & Technology Facilities Council.
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
*     DSB: David S. Berry
*     EC: Ed Chapin (UBC)
*     {enter_new_authors_here}

*  History:
*     30-SEP-2005 (DSB):
*        Original version.
*     17-MAY-2006 (DSB):
*        Report an error if the value string contains any isolated equals
*        signs.
*     13-JUN-2008 (DSB):
*        Add support for vector values.
*     29-SEP-2009 (EC):
*        Use c instead of equals+1 as pointer to value in scalar case
*     25-FEB-2010 (DSB):
*        Allow "keyword=<def>" syntax to be used for clearing a parameter.
*     25-FEB-2010 (DSB):
*        Allow "keyword=<undef>" syntax to be used to force a keyword to
*        be in an undefined state.
*     29-MAY-2012 (DSB):
*        Added "ind" argument.
*     3-APR-2020 (DSB):
*        Parentheses are legal in component names (e.g. they are used in
*        the names of graphics attributes).
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

/* Local Variables: */
   AstKeyMap *compmap = NULL;   /* KeyMap holding components of 1st component */
   AstObject *obj = NULL;       /* Pointer to generic AST Object */
   char *d = NULL;              /* Pointer to next char to read */
   char *e = NULL;              /* Pointer to next char to write */
   char *val = NULL;            /* Pointer to copy of vector element */
   char buf[ 10 ];              /* Text buffer for message symbols */
   char comp[ MAX_COMPLEN + 1 ];/* Buffer for component name */
   const char **vector = NULL;  /* Pointer to array of vector elements */
   const char *c = NULL;        /* Pointer to next char to examine */
   const char *cend = NULL;     /* Pointer to 1st char after component name */
   const char *dot = NULL;      /* Pointer to 1st dot */
   const char *equals = NULL;   /* Pointer to 1st equals sign */
   const char *start = NULL;    /* Pointer to start of next vector element */
   int clen;                    /* Length of component name */
   int ivec;                    /* Index of vector element */
   int vallen;                  /* Length of vector value */
   int veclen;                  /* Number of elements in vector value */

/* Check the inherited status. */
   if( *status != SAI__OK ) return;

/* Locate the first equals sign. */
   equals = strchr( string, '=' );

/* Report an error if no equals sign was found. If an indirection
   character is supplied, the error may actually be a missing indirection
   character rather than a missing equals sign. */
   if( !equals ) {
      *status = SAI__ERROR;
      msgSetc( "S", string );
      if( ind ) {
         msgSetc( "I", ind );
         errRepf( "KPG1KYMP2_ERR1", "No indirection character (^I) or "
                  "equals sign found in \"^S\".", status );
      } else {
         errRepf( "KPG1KYMP2_ERR1", "No equals sign found in \"^S\".",
                  status );
      }
      goto L999;
   }

/* Report an error if the equals sign is the last non-blank character. */
   if( astChrLen( equals + 1 ) == 0 ) {
      *status = SAI__ERROR;
      msgSetc( "S", string );
      errRep( "KPG1KYMP2_ERR2", "No value found in \"^S\".", status );
      goto L999;
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
         if( !isalnum( *c ) && *c != '_' && *c != '-' && *c != '+' &&
             *c != '(' && *c != ')' ) {
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

/* If the first non-blank character in the value is an opening parenthesis,
   we have a vector value. */
      c = equals;
      while( isspace( *(++c) ) );
      if( *c == '(' ) {

/* Loop reading elements from the vector. */
         veclen = 0;
         while( 1 ) {

/* Search for the next unescaped comma, or the next unescaped closing
   parenthesis, or the end of the string. This marks the end of the vector
   element. */
            start = ++c;
            while( ( *(++c) != ',' || c[-1] == '\\' ) &&
                   ( *c != ')' || c[-1] == '\\' ) && *c );

/* Store a null terminated copy of the vector element. */
            vector = astGrow( vector, veclen + 1, sizeof( char *) );
            vallen = c - start;
            val = astStore( NULL, start, vallen + 1 );
            if( astOK ) {
               val[ vallen ] = 0;
               vector[ veclen++ ] = val;

/* Remove any backslashes that preceed a comma or closing parenthesis. */
               d = val - 1;
               e = val;
               while( *(++d) ) {
                  if( *d == ',' || *d == ')' ) e--;
                  *(e++) = *d;
               }
               *e = 0;

/* Report an error if then end of the string has been reached without
   finding a closing parenthesis. */
               if( ! *c ) {
                  *status = SAI__ERROR;
                  msgSetc( "S", string );
                  errRep( "KPG1KYMP2_ERR3", "Unmatched \"(\" in \"^S\".", status );
                  break;

/* Report an error if there are further non-blank characters following
   the closing parenthesis. */
               } else if( *c == ')' ) {
                  if( astChrLen( c + 1 ) > 0 ) {
                     *status = SAI__ERROR;
                     msgSetc( "S", string );
                     errRep( "KPG1KYMP2_ERR3", "Unexpected text following \")\" in \"^S\".", status );
                  }
                  break;
               }
            }
         }

/* Store the vector in the KeyMap. */
         astMapPut1C( keymap, comp, veclen, vector, NULL );

/* Free resources */
         if( vector ) {
            for( ivec = 0; ivec < veclen; ivec++ ) {
               vector[ ivec ] = astFree( (char *) vector[ ivec ] );
            }
            vector = astFree( vector );
         }

/* If the value string is "<def>" (case insensitive) then remove any
   value from the KeyMap. */
      } else if( astChrMatch( c, "<def>" ) ) {
         astMapRemove( keymap, comp );

/* If the value string is "<undef>" (case insensitive) force the
   value to be undefined */
      } else if( astChrMatch( c, "<undef>" ) ) {
        astMapPutU( keymap, comp, NULL );

/* Now store scalar values. */
      } else {
         astMapPut0C( keymap, comp, c, NULL );
      }

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
      kpg1Kymp2( dot + 1, ind, compmap, status );

/* Free the component KeyMap pointer. */
      compmap = astAnnul( compmap );
   }

/* Finish */
L999:;

}
