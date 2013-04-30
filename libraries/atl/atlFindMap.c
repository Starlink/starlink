#include "atl.h"
#include "ast.h"
#include "sae_par.h"

AstMapping *atlFindMap( AstMapping *this, const char *ident, AstMapping **map1,
                        AstMapping **map2, int *status ){
/*
*  Name:
*     atlFindMap

*  Purpose:
*     Search a series CmpMap for a component Mapping with a given Ident.

*  Invocation:
*     AstMapping *atlFindMap( AstMapping *this, const char *ident,
*                             AstMapping **map1, AstMapping **map2,
*                             int *status )

*  Description:
*     The supplied Mapping is decomposed into a list of Mappings in
*     series, and the list is searched for a component that has an
*     "Ident" attribute value equal to the supplied value. If found,
*     a pointer to the Mapping is returned as the function value.
*     Any Mappings before the returned Mapping are then combined in
*     series and returned as "*map1" (NULL is returned for "*map1" if
*     the returned Mapping is the first component Mapping in "this").
*     Likewise, any Mappings after the returned Mapping are combined
*     together in series and returned as "*map2".
*
*     The returned function value is NULL if the requested Mapping
*     is not found in "this" ("*map1" and "*map2" are also returned
*     as NULL in this case).
*
*     If "this" contains more than one component Mapping with the
*     required Ident value, then the first one found is returned.

*  Arguments:
*     this
*        The Mapping to search.
*     ident
*        The value of the "Ident" attribute for the required Mapping.
*     map1
*        Pointer to a location at which to return the pointer to a deep
*        copy of the total Mapping that comes before the required Mapping
*        within "this". Returned as NULL if the requied Mapping is the
*        first Mapping in "this", or is not found within "this".
*     map2
*        Pointer to a location at which to return the pointer to a deep
*        copy of the total Mapping that comes after the required Mapping
*        within "this". Returned as NULL if the requied Mapping is the
*        last Mapping in "this", or is not found within "this".
*     status
*        The inherited status.

*  Returned Value:
*     A pointer to the required Mapping, or NULL if it is not found in
*     "this".

*  Copyright:
*     Copyright (C) 2013 Science & Technology Facilities Council.
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
*     30-APR-2013 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}
*/

/* Local Variables: */
   AstMapping *cmap1;
   AstMapping *cmap2;
   AstMapping *dcmap;
   AstMapping *result;
   AstMapping *tmap1;
   AstMapping *tmap2;
   const char *this_ident;
   int invert1;
   int invert2;
   int oldinv1;
   int oldinv2;
   int series;

/* Initialise returned values. */
   *map1 = NULL;
   *map2 = NULL;
   result = NULL;

/* Check the inherited status. Also check a Mapping has been supplied. */
   if( *status != SAI__OK || !this ) return result;

/* If the supplied Mapping is the required Mapping, return a deep copy of
   "this" as the function value and retain the NULL pointers in "*map1"
   and "*map2". */
   this_ident = astGetC( this, "Ident" );
   if( astOK ){
      if( !strcmp( this_ident, ident ) ){
         result = astCopy( this );

/* Otherwise, attempt to decompose the supplied Mapping into two component
   Mappings. */
      } else {
         astDecompose( this, &cmap1, &cmap2, &series, &invert1, &invert2 );

/* If the supplied Mapping is a parallel CmpMap, or cannot be decomposed,
   return with all NULL pointers to indicate the required Mapping
   cannot be found. */
         if( series && cmap1 && cmap2 ) {

/* Temporarily reset the Invert flags for the component Mappings to the
   values they had when the CmpMap was created. */
            oldinv1 = astGetI( cmap1, "Invert" );
            oldinv2 = astGetI( cmap2, "Invert" );
            astSetI( cmap1, "Invert", invert1 );
            astSetI( cmap2, "Invert", invert2 );

/* Call this function recursively to search the first component Mapping for
   the required Mapping. */
            result = atlFindMap( cmap1, ident, &tmap1, &tmap2, status );

/* If it was found, form the total pre- and post- Mappings to return. */
            if( result ) {
               if( tmap1 ) *map1 = astClone( tmap1 );
               if( tmap2 ) {
                  dcmap = astCopy( cmap2 );
                  *map2 = (AstMapping *) astCmpMap( tmap2, dcmap, 1, " " );
                  dcmap = astAnnul( dcmap );
               } else {
                  *map2 = astCopy( cmap2 );
               }

/* Free any Mapping references returned by atlFindMap. */
               if( tmap1 ) tmap1 = astAnnul( tmap1 );
               if( tmap2 ) tmap2 = astAnnul( tmap2 );

/* If it was not found, search the second component of the supplied
   Mapping ("this") in the same way. */
            } else {
               result = atlFindMap( cmap2, ident, &tmap1, &tmap2, status );

/* If it was found, form the total pre- and post- Mappings to return. */
               if( result ) {
                  if( tmap1 ) {
                     dcmap = astCopy( cmap1 );
                     *map1 = (AstMapping *) astCmpMap( dcmap, tmap1, 1, " " );
                     dcmap = astAnnul( dcmap );
                  } else {
                     *map1 = astCopy( cmap1 );
                  }
                  if( tmap2 ) *map2 = astClone( tmap2 );

/* Free any Mapping references returned by atlFindMap. */
                  if( tmap1 ) tmap1 = astAnnul( tmap1 );
                  if( tmap2 ) tmap2 = astAnnul( tmap2 );

               }
            }

/* Re-instate the original values of the Invert flags in the component
   Mappings. */
            astSetI( cmap1, "Invert", oldinv1 );
            astSetI( cmap2, "Invert", oldinv2 );
         }

/* Free any Mapping references returned by astDecompose. */
         if( cmap1 ) cmap1 = astAnnul( cmap1 );
         if( cmap2 ) cmap2 = astAnnul( cmap2 );
      }
   }

/* Return the result. */
   return result;

}

