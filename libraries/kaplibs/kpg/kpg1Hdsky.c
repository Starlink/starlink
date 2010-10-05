#include "star/hds.h"
#include "star/atl.h"
#include "f77.h"
#include "ast.h"
#include "mers.h"
#include "kaplibs.h"
#include "sae_par.h"

void kpg1Hdsky( const HDSLoc *loc, AstKeyMap *keymap, int old, int new,
                int *status ){
/*
*  Name:
*     kpg1Hdsky

*  Purpose:
*     Append a primitive HDS object to an AST KeyMap.

*  Language:
*     C.

*  Invocation:
*     void kpg1Hdsky( const HDSLoc *loc, AstKeyMap *keymap, int old, int new,
*                     int *status )

*  Description:
*     This function stores the vectorised data values in the supplied HDS
*     object (which must be primitive) in the supplied KeyMap. The key for
*     the KeyMap entry is the name of the HDS object. If the KeyMap already
*     contains an entry with this name, then what happens is specified
*     by "old", Likewise, if the KeyMap does not already contain an entry
*     with this name, then what happens is specified by "new".

*  Arguments:
*     loc
*        An HDS locator for a primitive scalar or array object.
*     keymap
*        An AST pointer to an existing KeyMap.
*     old
*        Specifies what happens if the supplied KeyMap already contains
*        an entry with the name of the supplied HDS object.
*
*        1 - Append the new vectorised array values read from the HDS
*        object to the end of the values already in the KeyMap. The HDS
*        values will be converted to the data type of the values already
*        in the KeyMap (an error will be reported if this is not possible).
*
*        2 - Replace the existing KeyMap entry with a new entry holding
*        the vectorised array values read from the HDS object.
*
*        3 - Do nothing. The KeyMap is returned unchanged, and no error
*        is reported.
*
*        4 - Report an error. The KeyMap is returned unchanged, and an error
*        is reported.
*     new
*        Specifies what happens if the supplied KeyMap does not already
*        contain an entry with the name of the supplied HDS object.
*
*        1 - Create a new entry holding the vectorised array values read from
*        the HDS object.
*
*        2 - Do nothing. The KeyMap is returned unchanged, and no error
*        is reported.
*
*        3 - Report an error. The KeyMap is returned unchanged, and an error
*        is reported.
*     status
*        The inherited status.

*  Notes:
*     - An error is reported if the supplied HDS object is a structure.

*  Copyright:
*     Copyright (C) 2008 Science & Technology Facilities Council.
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
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     10-MAR-2008 (DSB):
*        Original version.
*     2010-10-04 (TIMJ):
*        Add support for Short/Word
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}
*/

/* Local Varianles: */
   char name[ DAT__SZNAM + 1 ];
   char type[ DAT__SZTYP + 1 ];
   const char *hdstype = NULL;
   int append;
   int haskey;
   int kmtype;
   int newlen;
   int oldlen;
   int prim;
   int replace;
   size_t el;
   size_t elsize;
   void *data;
   void *pntr;

/* Check inherited status */
   if( *status != SAI__OK ) return;

/* Get the name and data type for the supplied HDS object. */
   datName( loc, name, status );
   datType( loc, type, status );

/* Check the supplied HDS object is primitive. */
   datPrim( loc, &prim, status );
   if( !prim && *status == SAI__OK ) {
      msgSetc( "N", name );
      *status = SAI__ERROR;
      errRep( "", "KPG1_HDSKY: Supplied HDS Object \"^N\" is not primitive "
              "(programming error).", status );
   }

/* Check the "old" and "new" values are within bounds. */
   if( old < 1 || old > 4 ) {
      if( *status == SAI__OK ) {
         *status = SAI__ERROR;
         msgSeti( "OLD", old );
         errRep( "", "KPG1_HDSKY: Supplied value for OLD (^OLD) is illegal "
                 "(programming error).", status );
      }

   } else if( new < 1 || new > 3 ) {
      if( *status == SAI__OK ) {
         *status = SAI__ERROR;
         msgSeti( "NEW", new );
         errRep( "", "KPG1_HDSKY: Supplied value for NEW (^NEW) is illegal "
                 "(programming error).", status );
      }
   }

/* See if the keymap already has an entry with the same name. */
   haskey = astMapHasKey( keymap, name );

/* If required, report an error. */
   if( haskey && old == 4 ){
      if( *status == SAI__OK ) {
         *status = SAI__ERROR;
         msgSetc( "N", name );
         errRep( "", "KPG1_HDSKY: The name \"^N\" has already been used "
                 "(programming error).", status );
      }

   } else if( !haskey && new == 3 ){
      if( *status == SAI__OK ) {
         *status = SAI__ERROR;
         msgSetc( "N", name );
         errRep( "", "KPG1_HDSKY: The name \"^N\" does not exist "
                 "(programming error).", status );
      }
   }

/* See if we are to append the HDS values to the end of values already in
   the KeyMap. */
   append = ( haskey && old == 1 );

/* See if we are to store the HDS values as a new entry in the KeyMap. */
   replace = ( haskey && old == 2 ) || ( !haskey && new == 1 );

/* Skip to the end if we are doing neither of these. */
   if( append || replace ) {

/* Get the data type for the KeyMap entry. If we are appending values to
   an existing entry, then use the data type of the existing entry,
   otherwise use a data type that corresponds to the HDS data type. Also,
   get the corresponding HDS type. */
      if( append ) {
         kmtype = astMapType( keymap, name );

         if( kmtype == AST__INTTYPE ) {
            hdstype = "_INTEGER";

         } else if( kmtype == AST__SINTTYPE ) {
            hdstype = "_WORD";

         } else if( kmtype == AST__DOUBLETYPE ) {
            hdstype = "_DOUBLE";

         } else if( kmtype == AST__FLOATTYPE ) {
            hdstype = "_REAL";

         } else if( kmtype == AST__STRINGTYPE ) {
            hdstype = "_CHAR";

         } else if( *status == SAI__OK ) {
            *status = SAI__ERROR;
            msgSetc( "N", name );
            errRep( "", "KPG1_HDSKY: Cannot append HDS values to KeyMap "
                    "entry \"^N\" - KeyMap data type has no HDS equivalent.",
                    status );
         }

      } else if( !strcmp( type, "_WORD" ) ) {
         kmtype = AST__SINTTYPE;
         hdstype = "_WORD";

      } else if( !strcmp( type, "_REAL" ) ) {
         kmtype = AST__FLOATTYPE;
         hdstype = "_REAL";

      } else if( !strcmp( type, "_DOUBLE" ) ) {
         kmtype = AST__DOUBLETYPE;
         hdstype = "_DOUBLE";

      } else if( !strncmp( type, "_CHAR", 5 ) ) {
         kmtype = AST__STRINGTYPE;
         hdstype = "_CHAR";

      } else {
         kmtype = AST__INTTYPE;
         hdstype = "_INTEGER";
      }

/* Get the number of bytes needed to store a single value. */
      if( kmtype == AST__INTTYPE ) {
         elsize = sizeof( int );

      } else if( kmtype == AST__SINTTYPE ) {
         elsize = sizeof( short );

      } else if( kmtype == AST__DOUBLETYPE ) {
         elsize = sizeof( double );

      } else if( kmtype == AST__FLOATTYPE ) {
         elsize = sizeof( float );

      } else if( !strncmp( type, "_CHAR", 5 ) ) {
         datLen( loc, &elsize, status );

      } else {
         datClen( loc, &elsize, status );
      }

/* Map the HDS object as a vector using the data type selected above. */
      datMapV( loc, hdstype, "READ", &pntr, &el, status );

/* If we are appending the new values to the old values, get the number of
   old values. */
      oldlen = append ? astMapLength( keymap, name ) : 0;

/* Get the new length of the KeyMap data. */
      newlen = oldlen + el;

/* Allocate an array to store the data values. */
      data = astMalloc( elsize*newlen );
      if( *status == SAI__OK ) {

/* Check each supported data type in turn. */
         if( kmtype == AST__INTTYPE ) {

/* If required, store any existing data values at the start of this array. */
            if( oldlen ) (void) astMapGet1I( keymap, name, oldlen, &oldlen,
                                             (int *) data );

/* Copy any new values to the end of the array. */
            memcpy( ( (int *) data ) + oldlen, pntr, el*elsize );

/* Put the array into the KeyMap, erasing any pre-existing entry with the
   same name. */
            astMapPut1I( keymap, name, newlen, (int *) data, NULL );

/* Do the same for the other numerical types. */
         } else if( kmtype == AST__SINTTYPE ) {
            if( oldlen ) (void) astMapGet1S( keymap, name, oldlen, &oldlen,
                                             (short *) data );
            memcpy( ( (short *) data ) + oldlen, pntr, el*elsize );
            astMapPut1S( keymap, name, newlen, (short *) data, NULL );

         } else if( kmtype == AST__FLOATTYPE ) {
            if( oldlen ) (void) astMapGet1F( keymap, name, oldlen, &oldlen,
                                             (float *) data );
            memcpy( ( (float *) data ) + oldlen, pntr, el*elsize );
            astMapPut1F( keymap, name, newlen, (float *) data, NULL );

         } else if( kmtype == AST__DOUBLETYPE ) {
            if( oldlen ) (void) astMapGet1D( keymap, name, oldlen, &oldlen,
                                             (double *) data );
            memcpy( ( (double *) data ) + oldlen, pntr, el*elsize );
            astMapPut1D( keymap, name, newlen, (double *) data, NULL );

/* For string types, datMapV returns a pointer to a concatenated list of
   fixed length strings. So we use wrappers in the ATL library that
   convert to and fro between concatenated fixed length strings and
   null-terminated strings. */
         } else {
            if( oldlen ) (void) atlMapGet1C( keymap, name, elsize*oldlen,
                                             elsize, &oldlen, (char *) data,
                                             status );
            memcpy( ( (char *) data ) + elsize*oldlen, pntr, el*elsize );
            atlMapPut1C( keymap, name, (char *) data, elsize, newlen, NULL,
                         status );

         }
      }

/* Free resources. */
      data = astFree( data );
      datUnmap( loc, status );
   }

}
