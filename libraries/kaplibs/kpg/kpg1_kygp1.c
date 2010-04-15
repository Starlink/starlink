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

*  Synopsis:
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
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     7-NOV-2005 (DSB):
*        Original version.
*     15-JUL-2008 (TIMJ):
*        Tweak to GRP C API.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

/* Local Variables: */
   AstObject *obj;              /* Pointer to nested AST Object */
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

/* If it not bad, get its formatted value. */
         if( !bad && astMapGet0C( keymap, key, &value ) ) {

/* Put it in the group. */
            nc = 0;
            text = astAppendString( NULL, &nc, prefix );
            text = astAppendString( text, &nc, key );
            text = astAppendString( text, &nc, "=" );
            text = astAppendString( text, &nc, value );
            grpPut1( *igrp, text, 0, status );
            text = astFree( text );
         }
      }
   }

/* Make AST use its original status variable. */
   astWatch( old_status );

}
