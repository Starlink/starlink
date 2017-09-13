#include "star/hds.h"
#include "atl.h"
#include "ast.h"
#include "mers.h"
#include "sae_par.h"

void atlKy2hd( AstKeyMap *keymap, HDSLoc *loc, int *status ){
/*
*  Name:
*     atlKy2hd

*  Purpose:
*     Copies values from an AST KeyMap to a primitive HDS object.

*  Language:
*     C.

*  Invocation:
*     void atlKy2hd( AstKeyMap *keymap, HDSLoc *loc, int *status )

*  Description:
*     This routine copies the contents of an AST KeyMap into a supplied
*     HDS structure.

*  Arguments:
*     keymap
*        An AST pointer to the KeyMap.
*     loc
*        A locator for the HDS object into which the KeyMap contents
*        are to be copied. A new component is added to the HDS object for
*        each entry in the KeyMap.
*     status
*        The inherited status.

*  Copyright:
*     Copyright (C) 2008, 2010, 2012 Science & Technology Facilities Council.
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
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     29-APR-2008 (DSB):
*        Original version.
*     2010-09-23 (TIMJ):
*        Fix arrays of strings.
*     2010-09-30 (TIMJ):
*        Make sure that we are using the correct status pointer in AST.
*     2010-10-01 (TIMJ):
*        Sort the keys when writing to HDS structured.
*     2010-10-04 (TIMJ):
*        Support Short ints in keymap
*     14-SEP-2012 (DSB):
*        Moved from kaplibs to atl.
*     17-SEP-2012 (DSB):
*        Add support for undefined values.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}
*/

/* Local Varianles: */
   AstObject **objArray = NULL;
   AstObject *obj = NULL;
   HDSLoc *cloc = NULL;
   HDSLoc *dloc = NULL;
   const char *cval = NULL;
   const char *key;
   double dval;
   float fval;
   int i;
   int ival;
   hdsdim j;
   int lenc;
   int nval;
   char *oldsortby;
   int *oldstat = NULL;
   int size;
   int type;
   hdsdim veclen;
   size_t el;
   void *pntr = NULL;

/* Check inherited status */
   if( *status != SAI__OK ) return;

/* Make sure that we are checking AST status */
   oldstat = astWatch( status );

/* If set, save the old SortBy value and then ensure alphabetical sorting.
   We need to take a copy of the original string since the buffer in which
   the string is stored may be re-used by subsequent incocations of astGetC.  */
   if( astTest( keymap, "SortBy" ) ) {
      int nc = 0;
      oldsortby = astAppendString( NULL, &nc, astGetC( keymap, "SortBy" ) );
   } else {
      oldsortby = NULL;
   }
   astSet( keymap, "SortBy=KeyUp" );

/* Loop round each entry in the KeyMap. */
   size = astMapSize( keymap );
   for( i = 0; i < size; i++ ) {

     if (*status != SAI__OK) break;

/* Get the key. the data type and the vector length for the current
   KeyMap entry. */
      key = astMapKey( keymap, i );
      type = astMapType( keymap, key );
      veclen = astMapLength( keymap, key );

/* If the current entry holds one or more nested KeyMaps, then we call
   this function recursively to add them into a new HDS component. */
      if( type == AST__OBJECTTYPE ) {

/* First deal with scalar entries holding a single KeyMap. */
         if( veclen == 1 ) {
            datNew( loc, key, "KEYMAP_ENTRY", 0, NULL, status );
            datFind( loc, key, &cloc, status );

            (void) astMapGet0A( keymap, key, &obj );

            if( astIsAKeyMap( obj ) ) {
               atlKy2hd( (AstKeyMap *) obj, cloc, status );

            } else if( *status == SAI__OK ) {
               *status = SAI__ERROR;
               errRep( "", "atlKy2hd: Supplied KeyMap contains unusable AST "
                       "objects (programming error).", status );
            }

            datAnnul( &cloc, status );

/* Now deal with vector entries holding multiple KeyMaps. */
         } else {
            datNew( loc, key, "KEYMAP_ENTRY", 1, &veclen, status );
            datFind( loc, key, &cloc, status );

            objArray = astMalloc( sizeof( AstObject *) * (size_t)veclen );
            if( objArray ) {
               (void) astMapGet1A( keymap, key, veclen, &nval, objArray );

               for( j = 1; j <= veclen; j++ ) {
                  datCell( cloc, 1, &j, &dloc, status );

                  if( astIsAKeyMap( objArray[ j - 1 ] ) ) {
                     atlKy2hd( (AstKeyMap *) objArray[ j - 1 ], dloc, status );

                  } else if( *status == SAI__OK ) {
                     *status = SAI__ERROR;
                     errRep( "", "atlKy2hd: Supplied KeyMap contains unusable AST "
                             "objects (programming error).", status );
                  }

                  datAnnul( &dloc, status );
               }

               objArray = astFree( objArray );
            }

            datAnnul( &cloc, status );
         }

/* For primitive types... */
      } else if( type == AST__INTTYPE ){
         if( veclen == 1 ) {
            datNew0I( loc, key, status );
            datFind( loc, key, &cloc, status );
            (void) astMapGet0I( keymap, key, &ival );
            datPut0I( cloc, ival, status );
            datAnnul( &cloc, status );

         } else {
            datNew1I( loc, key, veclen, status );
            datFind( loc, key, &cloc, status );
            datMapV( cloc, "_INTEGER", "WRITE", &pntr, &el, status );
            (void) astMapGet1I( keymap, key, veclen, &nval, (int *) pntr );
            datUnmap( cloc, status );
            datAnnul( &cloc, status );
         }

      } else if( type == AST__SINTTYPE ){
         short sval = 0;
         if( veclen == 1 ) {
            datNew0W( loc, key, status );
            datFind( loc, key, &cloc, status );
            (void) astMapGet0S( keymap, key, &sval );
            datPut0W( cloc, sval, status );
            datAnnul( &cloc, status );

         } else {
            datNew1W( loc, key, veclen, status );
            datFind( loc, key, &cloc, status );
            datMapV( cloc, "_WORD", "WRITE", &pntr, &el, status );
            (void) astMapGet1S( keymap, key, veclen, &nval, (short *) pntr );
            datUnmap( cloc, status );
            datAnnul( &cloc, status );
         }

      } else if( type == AST__DOUBLETYPE ){
         if( veclen == 1 ) {
            datNew0D( loc, key, status );
            datFind( loc, key, &cloc, status );
            (void) astMapGet0D( keymap, key, &dval );
            datPut0D( cloc, dval, status );
            datAnnul( &cloc, status );

         } else {
            datNew1D( loc, key, veclen, status );
            datFind( loc, key, &cloc, status );
            datMapV( cloc, "_DOUBLE", "WRITE", &pntr, &el, status );
            (void) astMapGet1D( keymap, key, veclen, &nval, (double *) pntr );
            datUnmap( cloc, status );
            datAnnul( &cloc, status );
         }

      } else if( type == AST__FLOATTYPE ){
         if( veclen == 1 ) {
            datNew0R( loc, key, status );
            datFind( loc, key, &cloc, status );
            (void) astMapGet0F( keymap, key, &fval );
            datPut0R( cloc, fval, status );
            datAnnul( &cloc, status );

         } else {
            datNew1R( loc, key, veclen, status );
            datFind( loc, key, &cloc, status );
            datMapV( cloc, "_REAL", "WRITE", &pntr, &el, status );
            (void) astMapGet1F( keymap, key, veclen, &nval, (float *) pntr );
            datUnmap( cloc, status );
            datAnnul( &cloc, status );
         }

      } else if( type == AST__STRINGTYPE ){
         lenc = astMapLenC( keymap, key );

         if( veclen == 1 ) {
            datNew0C( loc, key, lenc, status );
            datFind( loc, key, &cloc, status );
            (void) astMapGet0C( keymap, key, &cval );
            datPut0C( cloc, cval, status );
            datAnnul( &cloc, status );

         } else {
            datNew1C( loc, key, lenc, veclen, status );
            datFind( loc, key, &cloc, status );
            datMapV( cloc, "_CHAR", "WRITE", &pntr, &el, status );
            (void) atlMapGet1C( keymap, key, veclen*lenc, lenc, &nval,
                                (char *) pntr, status );
            datUnmap( cloc, status );
            datAnnul( &cloc, status );
         }

/* KeyMap "UNDEF" values are always scalar and have no corresponding HDS
   data type. So arbitrarily use an "_INTEGER" primitive with no defined
   value to represent a KeyMap UNDEF value. */
      } else if( type == AST__UNDEFTYPE ){
         datNew0L( loc, key, status );

/* Unknown or unsupported data types. */
      } else if( *status == SAI__OK ) {
         *status = SAI__ERROR;
         msgSeti( "T", type );
         errRep( "", "atlKy2hd: Supplied KeyMap contains entries with "
                 "unusable data type (^T) (programming error).", status );
      }
   }

/* If it was originally set, re-instate the old SortBy value in the KeyMap,
   then free the memory. Otherwise, clear the SortBy attribute. */
   if( oldsortby ) {
      astSetC( keymap, "SortBy", oldsortby );
      oldsortby = astFree( oldsortby );
   } else {
      astClear( keymap, "SortBy" );
   }

/* Reset AST status */
   astWatch( oldstat );

}
