/*
 *+
 *  Name:
 *     atlMapCopy

 *  Purpose:
 *     Copy one keymap into another

 *  Language:
 *     Starlink ANSI C

 *  Type of Module:
 *     Library routine

 *  Invocation:
 *     atlMapCopy( AstKeyMap * destkm, AstKeyMap * srckm, int * status );

 *  Arguments:
 *     destkm = AstKeyMap * (Given and Returned)
 *        Keymap to receive items from srckm
 *     srckm = AstKeyMap * (Given)
 *        Name of primary key to use for merging.
 *     status = int* (Given and Returned)
 *        Pointer to global status.

 *  Description:
 *     Copy all the items from one keymap into another keymap, overwriting
 *     any in the destination that have the same key value.

 *  Notes:
 *     Does not work with vector items.

 *  Authors:
 *     Tim Jenness (JAC, Hawaii)
 *     {enter_new_authors_here}

 *  History:
 *     2010-01-11 (TIMJ):
 *        Initial Version

 *  Copyright:
 *     Copyright (C) 2010 Science and Technology Facilities Council.
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
 *     Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
 *     MA 02111-1307, USA

 *  Bugs:
 *     {note_any_bugs_here}
 *-
 */

#include "atl.h"
#include "ast.h"
#include "sae_par.h"

void atlMapCopy( AstKeyMap * destkm, AstKeyMap * srckm, int * status ) {
  size_t i;
  size_t nkeys;      /* Number of keys in source */

  printf(">>>>>> ENTER ATL\n");

  if (*status != SAI__OK) return;

  /* Loop through all the keys in srckm */
  nkeys = astMapSize( srckm );

  for (i = 0; i < nkeys; i++ ) {
    int ival;
    double dval;
    float fval;
    AstObject * aval = NULL;
    const char * cval = NULL;
    void * pval = NULL;
    int itype = AST__BADTYPE;
    const char * ikey = astMapKey( srckm, i );
    if (!ikey) break;

    printf("Copying key %s<<<<<<<<<<<<<<<<<<<<\n", ikey );

    /* now we need to know its type */
    itype = astMapType( srckm, ikey );

    switch ( itype ) {
    case AST__INTTYPE:
      astMapGet0I( srckm, ikey, &ival );
      astMapPut0I( destkm, ikey, ival, NULL );
      break;
    case AST__FLOATTYPE:
      astMapGet0F( srckm, ikey, &fval );
      astMapPut0F( destkm, ikey, fval, NULL );
      break;
    case AST__DOUBLETYPE:
      astMapGet0D( srckm, ikey, &dval );
      astMapPut0D( destkm, ikey, dval, NULL );
      break;
    case AST__STRINGTYPE:
      astMapGet0C( srckm, ikey, &cval );
      astMapPut0C( destkm, ikey, cval, NULL);
      break;
    case AST__OBJECTTYPE:
      astMapGet0A( srckm, ikey, &aval );
      astMapPut0A( destkm, ikey, aval, NULL );
      aval = astAnnul( aval );
      break;
    case AST__POINTERTYPE:
      astMapGet0P( srckm, ikey, &pval );
      astMapPut0P( destkm, ikey, pval, NULL );
      break;
    default:
      /* do nothing */
      break;
    }

  }

}
