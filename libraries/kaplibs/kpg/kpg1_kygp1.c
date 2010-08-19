#include "f77.h"
#include "mers.h"
#include "sae_par.h"
#include "ast.h"
#include "prm_par.h"
#include "star/grp.h"
#include "kaplibs_private.h"

void kpg1Kygp1( AstKeyMap *keymap, Grp **igrp, const char *prefix,
                int *status ){
/*
*+
*  Name:
*     kpg1Kygp1

*  Purpose:
*     Create a GRP group holding keyword/value pairs read from an AST KeyMap

*  Language:
*     C.

*  Invocation:
*     void kpg1Kygp1( AstKeyMap *keymap, Grp **igrp, const char *prefix,
*                     int *status )

*  Description:
*     This function is the inverse of kpg1Kymp1. It extracts the values
*     from the supplied AST KeyMap and creates a set of "name=value" strings
*     which it appends to a supplied group (or creates a new group). If
*     the KeyMap contains nested KeyMaps, then the "name" associated with
*     each primitive value stored in the returned group is a hierarchical
*     list of component names separated by dots.

*  Arguments:
*     keymap
*        A pointer to the KeyMap. Numerical entries which have bad values
*        (VAL__BADI for integer entries or VAL__BADD for floating point
*        entries) are not copied into the group.
*     igrp
*        A location at which is stored a pointer to the Grp structure
*        to which the name=value strings are to be appended. A new group is
*        created and a pointer to it is returned if the supplied Grp
*        structure is not valid.
*     prefix
*        A string to append to the start of each key extracted from the
*        supplied KeyMap. If NULL, no prefix is used.
*     status
*        Pointer to the inherited status value.

*  Notes:
*     - This function provides a private implementation for the public
*     KPG1_KYGRP Fortran routine and kpg1Kygrp C function.
*     - Entries will be stored in the group in alphabetical order

*  Copyright:
*     Copyright (C) 2008,2010 Science & Technology Facilities Council.
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
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     7-NOV-2005 (DSB):
*        Original version.
*     15-JUL-2008 (TIMJ):
*        Tweak to GRP C API.
*     2010-07-19 (TIMJ):
*        Handle vector keymap entries.
*     2010-08-12 (TIMJ):
*        Store entries in group in alphabetical order.
*     13-AUG-2010 (DSB):
*        Re-instate the original SortBy value before exiting.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

/* Local Variables: */
   AstObject *obj;              /* Pointer to nested AST Object */
   char *oldsortby;             /* The old value of the KeyMap's SortBy attribute */
   char *text;                  /* Sum of concatenated strings */
   const char *key;             /* Key string for current entry in KeyMap */
   const char *value;           /* Value of current entry in KeyMap */
   double dval;                 /* Double value */
   int *old_status;             /* Pointer to original status variable */
   int bad;                     /* Is the numerical entry value bad? */
   int i;                       /* Index into supplied KeyMap */
   int ival;                    /* Integer value */
   int n;                       /* Number of entries in the KeyMap */
   int nc;                      /* Length of "text" excluding trailing null */
   int type;                    /* Data type of current entry in KeyMap */
   int valid;                   /* Is the supplied GRP structure valid? */

/* Check the inherited status. */
   if( *status != SAI__OK ) return;

/* Make AST use the Fortran status variable. */
   old_status = astWatch( status );

/* Create a new GRP group if required. */
   valid = grpValid( *igrp, status );
   if( !valid ) *igrp = grpNew( "Created by kpg1_Kygp1", status );

/* If set, save the old SortBy value and then ensure alphabetical sorting.
   We need to take a copy of the original string since the buffer in which
   the string is stored may be re-used by subsequent incocations of astGetC.  */
   if( astTest( keymap, "SortBy" ) ) {
      nc = 0;
      oldsortby = astAppendString( NULL, &nc, astGetC( keymap, "SortBy" ) );
   } else {
      oldsortby = NULL;
   }
   astSet( keymap, "SortBy=KeyUp" );

/* Get the number of entries in the KeyMap. */
   n = astMapSize( keymap );

/* Loop round all the entries in the KeyMap.*/
   for( i = 0; i < n; i++ ) {

/* Get the name and type of the current KeyMap entry. */
      key = astMapKey( keymap, i );
      type = astMapType( keymap, key );

/* If the entry is an AST Object, get a pointer to it.*/
      if( type == AST__OBJECTTYPE && astMapGet0A( keymap, key, &obj ) ) {

/* If it is a nested KeyMap, update the prefix and call this function
   recursively. We ignore other forms of AST Objects. */
         if( astIsAKeyMap( obj ) ) {
            nc = 0;
            text = astAppendString( NULL, &nc, prefix );
            text = astAppendString( text, &nc, key );
            text = astAppendString( text, &nc, "." );
            kpg1Kygp1( (AstKeyMap *) obj, igrp, text, status );
            text = astFree( text );
         }

/* If it is a primitive, format it and add it to the group. */
      } else {

/* If it is a numerical type, see if it has a bad value. */
         bad = 0;
         if( type == AST__INTTYPE && astMapGet0I( keymap, key, &ival ) ){
            if( ival == VAL__BADI ) bad = 1;
         } else if( type == AST__DOUBLETYPE && astMapGet0D( keymap, key, &dval ) ){
            if( dval == VAL__BADD ) bad = 1;
         }

/* If it not bad, get its formatted value. We also make sure that astMapGet0C returns
   true because we intend to skip undefined values. */
         if( !bad && astMapGet0C( keymap, key, &value ) ) {
            size_t length;

/* Write the key and equals sign to a buffer */
            nc = 0;
            text = astAppendString( NULL, &nc, prefix );
            text = astAppendString( text, &nc, key );
            text = astAppendString( text, &nc, "=" );

            length = astMapLength( keymap, key );

            if( length > 1 ) {
/* Vector so we need to use  (a,b,c) syntax */
               char thiselem[GRP__SZNAM+1];
               size_t l;

               text = astAppendString( text, &nc, "(");
               for ( l = 0; l < length; l++) {
                  if( astMapGetElemC( keymap, key, sizeof(thiselem), l, thiselem ) ) {
                     text = astAppendString( text, &nc, thiselem );
                  }
/* always deal with the comma. Even if value was undef we need to put in the comma */
                  if( l < (length - 1) ) {
                     text = astAppendString( text, &nc, "," );
                  }
               }
               text = astAppendString( text, &nc, ")");

            } else {
/* Scalar */
               text = astAppendString( text, &nc, value );
            }
/* Put it in the group. */
            grpPut1( *igrp, text, 0, status );
            text = astFree( text );
         }
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

/* Make AST use its original status variable. */
   astWatch( old_status );

}
