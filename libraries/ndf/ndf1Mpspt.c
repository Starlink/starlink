#include "sae_par.h"
#include "ndf1.h"
#include "ndf_ast.h"

void ndf1Mpspt( AstMapping *map, AstMapping **map0, AstMapping **map1,
                AstMapping **map2, int inprm[], int outprm[], int *status ){
/*
*+
*  Name:
*     ndf1Mpspt

*  Purpose:
*     Split a Mapping up into two parallel Mappings with and without
*     inverse transformations.

*  Synopsis:
*     void ndf1Mpspt( AstMapping *map, AstMapping **map0,
*                     AstMapping **map1, AstMapping **map2, int inprm[],
*                     int outprm[], int *status )

*  Description:
*     This function attempts to partition the inputs of the supplied
*     Mapping into two groups; a group that can be re-constructed from a
*     corresponding group of Mapping outputs, and a group that cannot be
*     re-constructed because the required inverse transformation is not
*     defined. A Mapping is returned for each group, together with the
*     permutation arrays needed to permute the inputs and outputs of these
*     Mappings back into their original order.

*  Parameters:
*     map
*        AST pointer to the Ampping to be analysed.
*     *map0
*        Returned holding a pointer to a copy of the supplied Mapping that
*        has been modified, if necessary, to ensure that an inverse
*        transformation is defined. If the supplied Mapping did not itself
*        have a defined inverse transformation, then an inverse
*        transformation will be supplied that simply returns bad values for
*        the axes that did not originally have an inverse.
*     *map1
*        Returned holding a pointer to a Mapping that transforms a subset
*        of the inputs of "map" into a subset of the outputs of "map". The
*        subset contains all inputs for which the Mapping has an inverse
*        transformation (i.e. all inputs that can be re-generated from the
*        corresponding output values). AST__NULL will be returned if no
*        unique subset of inputs can be found that has a defined inverse.
*     *map2
*        Returned holding a pointer to a Mapping that transforms a subset
*        of the inputs of "map" into a subset of the outputs of "map". The
*        subset contains all inputs for which the Mapping does not have an
*        inverse transformation (i.e. all inputs that cannot be re-
*        generated from the corresponding output values). AST__NULL will be
*        returned if all the inputs have defined inverse transformations.
*     inprm
*        The index into this array is the index of an input to "map". The
*        values returned in the array are the indices of the corresponding
*        inputs within a compound Mapping formed by combining "map1" and
*        "map2" in parallel. The supplied "inprm" array should have at
*        least "NDF__MXDIM" elements.
*     outprm
*        The index into this array is the index of an output within a
*        compound Mapping formed by combining "map1" and "map2". The values
*        returned in the array are the indices of the corresponding outputs
*        within "map". The supplied "outprm" array should have at least
*        "NDF__MXDIM" elements.
*     *status
*        The global status.

*  Copyright:
*     Copyright (C) 2018 East Asian Observatory
*     All rights reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or modify
*     it under the terms of the GNU General Public License as published by
*     the Free Software Foundation; either version 2 of the License, or (at
*     your option) any later version.
*
*     This program is distributed in the hope that it will be useful,but
*     WITHOUT ANY WARRANTY; without even the implied warranty of
*     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
*     General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     DSB: David S. Berry (EAO)

*  History:
*     3-APR-2019 (DSB):
*        Original version, based on equivalent Fortran function by RFWS.

*-
*/

/* Local Variables: */
   AstCmpMap *tmap;
   AstMapping *maps[ NDF__MXDIM ];
   int hasinv[ NDF__MXDIM ];
   int i;
   int iin1;
   int iin2;
   int inind[ NDF__MXDIM ];
   int inmap[ NDF__MXDIM ];
   int iout1;
   int iout2;
   int j;
   int nin;
   int nmap;
   int nout;
   int outind[ NDF__MXDIM ];
   int outmap[ NDF__MXDIM ];

/* Initialise the returned values. */
   *map0 = NULL;
   *map1 = NULL;
   *map2 = NULL;

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Get the number of inputs and outputs for the supplied Mapping. */
   nin = astGetI( map, "Nin" );
   nout = astGetI( map, "Nout" );

/* Split the supplied Mapping up into the largest possible number
   of parallel Mappings. */
   ndf1Mpanl( map, &nmap, maps, hasinv, inmap, inind, outmap, outind, map0,
              status );

/* Loop round all the component Mappings found above, accumulating all
   the component Mappings that have, and do not have, defined inverse
   transformations within the supplied Mapping. */
   for( i = 0; i < nmap; i++ ){

/* If this component Mapping has an inverse transformation in the
   supplied Mapping, add it into the current "map1" Mapping. */
      if( hasinv[ i ] ) {
         if( !*map1 ) {
            *map1 = astClone( maps[ i ] );
         } else {
            tmap = astCmpMap( *map1, maps[ i ], 0, " " );
            *map1 = astAnnul( *map1 );
            *map1 = (AstMapping *) tmap;
         }

/* If this component Mapping has no inverse transformation in the
   supplied Mapping, add it into the current "map2" Mapping. */
      } else {
         if( !*map2 ) {
            *map2 = astClone( maps[ i ] );
         } else {
            tmap = astCmpMap( *map2, maps[ i ], 0, " " );
            *map2 = astAnnul( *map2 );
            *map2 = (AstMapping *) tmap;
         }
      }

   }

/* Get the number of inputs and outputs for "map1" and MAP2 */
   if( *map1 ) {
      iin2 = astGetI( *map1, "Nin" );
      iout2 = astGetI( *map1, "Nout" );
   } else {
      iin2 = 0;
      iout2 = 0;
   }
   iin1 = 0;
   iout1 = 0;

/* Now loop round all the component Mappings again. */
   for( i = 0; i < nmap; i++ ){

/* Find all inputs of the supplied Mapping that feed the current component
   Mapping. For each, store the index of the corresponding input within a
   parallel CmpMap holding "map1" and "map2". */
      for( j = 0; j < nin; j++ ){
         if( inmap[ j ] == i ) {

            if( hasinv[ i ] ) {
               inprm[ j ] = iin1 + inind[ j ];
            } else {
               inprm[ j ] = iin2 + inind[ j ];
            }

         }
      }

/* Find all outputs of the supplied Mapping that are fed by the current
   component Mapping. For each, store the index of the corresponding
   output within a parallel CmpMap holding "map1" and "map2". */
      for( j = 0; j < nout; j++ ){
         if( outmap[ j ] == i ) {

            if( hasinv[ i ] ) {
               outprm[ iout1 + outind[ j ] - 1 ] = j + 1;
            } else {
               outprm[ iout2 + outind[ j ] - 1 ] = j + 1;
            }

         }
      }

/* Update the number of inputs and outputs used for each of "map1" and "map2". */
      if( hasinv[ i ] ) {
         iin1 += astGetI( maps[ i ], "Nin" );
         iout1 += astGetI( maps[ i ], "Nout" );
      } else {
         iin2 += astGetI( maps[ i ], "Nin" );
         iout2 += astGetI( maps[ i ], "Nout" );
      }

/* Free the Mapping pointer since it is no longer needed. */
      maps[ i ] = astAnnul( maps[ i ] );

   }

/* Call error tracing function and exit. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1Mpspt", status );

}

