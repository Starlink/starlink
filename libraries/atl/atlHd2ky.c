#include "star/hds.h"
#include "atl.h"
#include "ast.h"
#include "mers.h"
#include "sae_par.h"


#define MAXDIM 7
#define NATTR 3

void atlHd2ky( HDSLoc *loc, AstKeyMap *keymap, int *status ){
/*
*  Name:
*     atlHd2ky

*  Purpose:
*     Copies values from an HDS Object to an AST KeyMap.

*  Invocation:
*     void atlHd2ky( HDSLoc *loc, AstKeyMap *keymap, int *status )

*  Description:
*     This routine add a single entry describing the supplied HDS object
*     to the supplied KeyMap. The new KeyMap entry has a key equal to the
*     name of the supplied HDS Object. If the supplied HDS object is a
*     scalar then the new KeyMap entry will also be a scalar. If the
*     supplied HDS object is a 1D vector then the new KeyMap entry will
*     also be a 1D vector. Multi-dimensional HDS objects are silently
*     ignored (i.e. no entry is added to the KeyMap and no error is
*     reported). If the HDS object holds primitive values then the new
*     KeyMap entry will hold corresponding primitive values. If the HDS
*     object is a structure, then the new KeyMap entry will be another
*     nested KeyMap in which each entry holds a single component in the
*     supplied HDS structure.

*  Arguments:
*     loc
*        A locator for the HDS object.
*     keymap
*        An AST pointer to the KeyMap into which the KeyMap contents
*        are to be copied.
*     status
*        The inherited status.

*  Notes;
*     - Only primitive values with the following HDS types are copied
*     (all others are silently ignored): _INTEGER, _REAL, _DOUBLE,
*      _WORD, _UBYTE, _CHAR[*N].

*  Copyright:
*     Copyright (C) 2012 Science & Technology Facilities Council.
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
*     {enter_new_authors_here}

*  History:
*     14-SEP-2012 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}
*/

/* Local Varianles: */
   AstKeyMap *subkm;
   AstObject **objArray = NULL;
   HDSLoc *cloc = NULL;
   HDSLoc *dloc = NULL;
   char *buf;
   char *cval = NULL;
   char name[ DAT__SZNAM + 1 ];
   char type[ DAT__SZTYP + 1 ];
   const char *attr_names[] = { "KeyCase", "KeyError", "SortBy" };
   const char *attrs;
   hdsdim dims[ MAXDIM ];
   hdsdim icell;
   int *oldstat = NULL;
   int actdim;
   int defined;
   int iattr;
   int icomp;
   int nc;
   int ncomp;
   int prim;
   size_t el;
   size_t len;
   size_t size;
   void *pntr = NULL;

/* Check inherited status */
   if( *status != SAI__OK ) return;

/* Make sure that we are checking AST status */
   oldstat = astWatch( status );

/* Get the name of the HDS object. */
   datName( loc, name, status );

/* Get the type of the HDS object. */
   datType( loc, type, status );

/* Get the shape of the HDS object */
   datShape( loc, MAXDIM, dims, &actdim, status );

/* Does the HDS object contain primitive values? */
   datPrim( loc, &prim, status);

/* Get a string describing the attributes of the supplied KeyMap. */
   buf = NULL;
   nc = 0;
   for( iattr = 0; iattr < NATTR; iattr++ ) {
      if( astTest( keymap, attr_names[ iattr ] ) ) {
         if( nc ) buf = astAppendString( buf, &nc, "," );
         buf = astAppendString( buf, &nc, attr_names[ iattr ] );
         buf = astAppendString( buf, &nc, "=" );
         buf = astAppendString( buf, &nc,
                                  astGetC( keymap, attr_names[ iattr ] ));
      }
   }
   attrs = buf ? buf : "";

/* If the HDS Object is a scalar... */
   if( actdim == 0 ) {

/* If it is a scalar primitive, store it in the KeyMap directly. */
      if( prim ) {

/* Does the primitive have a defined value? */
         datState( loc, &defined, status );
         if( defined ) {

/* Map the HDS object as a vector. */
            datMapV( loc, type, "READ", &pntr, &el, status );

/* Store the value in the KeyMap with the corresponding KeyMap data
   type. */
            if( !strcmp( type, "_INTEGER" ) ) {
               astMapPut0I( keymap, name, *((int *) pntr), NULL );

            } else if( !strcmp( type, "_REAL" ) ) {
               astMapPut0F( keymap, name, *((float *) pntr), NULL );

            } else if( !strcmp( type, "_DOUBLE" ) ) {
               astMapPut0D( keymap, name, *((double *) pntr), NULL );

            } else if( !strcmp( type, "_WORD" ) ) {
               astMapPut0S( keymap, name, *((short *) pntr), NULL );

            } else if( !strcmp( type, "_UBYTE" ) ) {
               astMapPut0B( keymap, name, *((unsigned char *) pntr), NULL );

            } else if( !strncmp( type, "_CHAR", 5 ) ) {
               datLen( loc, &len, status );
               cval = astStore( NULL, pntr, len + 1 );
               cval[ len ] = 0;
               astMapPut0C( keymap, name, cval, NULL );
               cval = astFree( cval );
            }

/* Unmap the HDS object. */
            datUnmap( loc, status );

/* If the primitive has no value, store an "UNDEF" in the keymap. */
         } else {
            astMapPutU( keymap, name, NULL );
         }

/* If it is a scalar structure, create a new KeyMap and copy each component
   of the structure into the new KeyMap by calling this function
   recursively. Then store the new LeyMap in the supplied KeyMap. */
      } else {
         subkm = astKeyMap( attrs  );
         datNcomp( loc, &ncomp, status );
         for( icomp = 1; icomp <= ncomp; icomp++ ) {
            datIndex( loc, icomp, &cloc, status );
            atlHd2ky( cloc, subkm, status );
            datAnnul( &cloc, status );
         }
         astMapPut0A( keymap, name, subkm, NULL );
         subkm = astAnnul( subkm );
      }

/* If the HDS Object is a 1-D vector... */
   } else if( actdim == 1 ) {

/* If it is a vector primitive, store it in the KeyMap directly. */
      if( prim ) {

/* Does the primitive have a defined value? */
         datState( loc, &defined, status );
         if( defined ) {

/* Map the HDS object as a vector. */
            datMapV( loc, type, "READ", &pntr, &el, status );

/* Store the value in the KeyMap with the corresponding KeyMap data
   type. */
            if( !strcmp( type, "_INTEGER" ) ) {
               astMapPut1I( keymap, name, el, (const int *) pntr, NULL );

            } else if( !strcmp( type, "_REAL" ) ) {
               astMapPut1F( keymap, name, el, (const float *) pntr, NULL );

            } else if( !strcmp( type, "_DOUBLE" ) ) {
               astMapPut1D( keymap, name, el, (const double *) pntr, NULL );

            } else if( !strcmp( type, "_WORD" ) ) {
               astMapPut1S( keymap, name, el, (const short *) pntr, NULL );

            } else if( !strcmp( type, "_UBYTE" ) ) {
               astMapPut1B( keymap, name, el, (const unsigned char *) pntr, NULL );

            } else if( !strncmp( type, "_CHAR", 5 ) ) {
               datLen( loc, &len, status );
               datSize( loc, &size, status );
               atlMapPut1C( keymap, name, (const char *) pntr, (int) len,
                            (int) size, NULL, status );
            }

/* Unmap the HDS object. */
            datUnmap( loc, status );

/* If the primitive has no value, store an "UNDEF" in the keymap. */
         } else {
            astMapPutU( keymap, name, NULL );
         }

/* Otherwise, create a vector of KeyMaps and store each cell of the array
   in a keymap by calling this function recursively. */
      } else {

/* Create an array to hold the required number of KeyMap pointers. */
         objArray = astCalloc( dims[ 0 ], sizeof( *objArray ) );
         if( *status == SAI__OK ) {

/* Loop round each element of the HDS vector. */
            for( icell = 1; icell <= dims[ 0 ]; icell++ ) {

/* Get a locator to the HDS cell. */
               datCell( loc, 1, &icell, &cloc, status );

/* Create a KeyMap to hold the contents of the HDS cell. */
               subkm = astKeyMap( attrs );
               objArray[ icell - 1 ] = (AstObject *) subkm;

/* We cannot just add the cell locator into the KeyMap as this would
   introduce an extra redudunt level in the KeyMap heirarchy. Instead, loop
   round creating an entry in the above KeyMap for each component of the
   structure. */
               datNcomp( cloc, &ncomp, status );
               for( icomp = 1; icomp <= ncomp; icomp++ ) {
                  datIndex( cloc, icomp, &dloc, status );
                  atlHd2ky( dloc, subkm, status );
                  datAnnul( &dloc, status );
               }

/* Annul the cell locator. */
               datAnnul( &cloc, status );
            }

/* Store the array of KeyMaps in the supplied KeyMap. */
            astMapPut1A( keymap, name, dims[ 0 ], objArray, NULL );

/* Free resources. */
            for( icell = 0; icell < dims[ 0 ]; icell++ ) {
               objArray[ icell ] = astAnnul( objArray[ icell ] );
            }
         }
         objArray = astFree( objArray );
      }
   }

/* Free resources. */
   buf = astFree( buf );

/* Reset AST status */
   astWatch( oldstat );
}
