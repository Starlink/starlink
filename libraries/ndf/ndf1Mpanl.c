#include <string.h>
#include "sae_par.h"
#include "ndf1.h"
#include "ndf_err.h"
#include "ndf_ast.h"
#include "mers.h"

void ndf1Mpanl( AstMapping *mapin, int *nmap, AstMapping *maps[],
                int hasinv[], int inmap[], int inind[], int outmap[],
                int outind[], AstMapping **map, int *status ){
/*
*+
*  Name:
*     ndf1Mpanl

*  Purpose:
*     Analyse a Mapping into a set of parallel Mappings.

*  Synopsis:
*     void ndf1Mpanl( AstMapping *mapin, int *nmap, AstMapping *maps[],
*                     int hasinv[], int inmap[], int inind[], int outmap[],
*                     int outind[], AstMapping **map, int *status )

*  Description:
*     This function returns a set of Mappings that, when applied in
*     parallel, are equivalent to the supplied Mapping. Each returned
*     Mapping has the fewest possible number of inputs. Thus, the supplied
*     Mapping will be split up into the largest possible number of
*     Mappings.

*  Parameters:
*     mapin
*        AST pointer to the supplied Mapping.
*     *nmap
*        The number of Mappings returned.
*     maps
*        An array of "*nmap" returned Mapping pointers. Each of these
*        Mappings will have a defined inverse transformation. If the
*        supplied Mapping does not include an inverse for a particular
*        input, then the Mapping for that input will be a TranMap that
*        encapsulated the supplied forward Mapping and an inverse Mapping
*        that generates AST__BAD values. The supplied "maps" array should
*        have at least "NDF__MXDIM" elements.
*     hasinv
*        An array of "*nmap" returned flags. Each one is set non-zero if the
*        correspnding Mapping in "maps" has an inverse transformation that
*        was inherited from the supplied Mapping, or zero if the inverse
*        transformation was created by this function. The supplied "hasinv"
*        array should have at least "NDF__MXDIM" elements.
*     inmap
*        Element "i" is returned holding the index into the "maps" array
*        that holds the Mapping used to transforms input "i+1" in the
*        supplied Mapping (note, Mapping input and output indices are
*        1-based). The supplied "inmap" array should have at least
*        "NDF__MXDIM" elements.
*     inind
*        Element "i" is returned holding the (1-based) index of the input of
*        the Mapping identified by "inmap[i]" that corresponds to (1-based)
*        input "i+1" in the supplied Mapping. The supplied "inind" array
*        should have at least "NDF__MXDIM" elements.
*     outmap
*        Element "i" is returned holding the index into the "maps" array
*        that holds the Mapping that generates values for (1-based) output
*        index "i+1" in the supplied Mapping. The supplied "outmap" array
*        should have at least "NDF__MXDIM" elements.
*     outind
*        Element "i" is returned holding the (1-based) index of the output of
*        the Mapping identified by "outmap[i]" that corresponds to (1-based)
*        output "i+1" in the supplied Mapping. The supplied "outind" array
*        should have at least "NDF__MXDIM" elements.
*     *map
*        Returned holding a pointer to the full Mapping. This is
*        constructed by joining all the parallel Mappings back together
*        again, and so should always have an inverse transformation (so
*        long as the supplied Mapping can be split succesfully). If the
*        supplied Mapping cannot be split, a clone of the supplied Mapping
*        is returned.
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
*     xxx (DSB):
*        Original version, based on equivalent Fortran function by RFWS.

*-
*/

/* Local Variables: */
   AstMapping *omap;
   AstPermMap *invmap;
   AstPermMap *pmap;
   AstMapping *tmap;
   double conout[ NDF__MXDIM ];
   double zero[ NDF__MXDIM ];
   int i;
   int ignore;
   int iin;
   int imap;
   int inprm[ NDF__MXDIM ];
   int iout;
   int j;
   int more;
   int mpax;
   int ndone;
   int needpm;
   int nin;
   int nout;
   int out[ NDF__MXDIM ];
   int outprm[ NDF__MXDIM ];
   int ovflow;
   int p[ NDF__MXDIM ];
   int perm[ NDF__MXDIM ];
   int tnin;
   int tnout;
   int used;

/* Initialise the returned values. */
   *nmap = 0;
   for( j = 0; j < NDF__MXDIM; j++ ){
      maps[ j ] = NULL;
      hasinv[ j ] = 0;
      inmap[ j ] = -1;
      inind[ j ] = 0;
      outmap[ j ] = -1;
      outind[ j ] = 0;
      inprm[ j ] = 0;
      outprm[ j ] = 0;
      perm[ j ] = 0;
   }
   *map = NULL;

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Get the number of inputs and outputs for the Mapping. */
   nin = astGetI( mapin, "Nin" );
   nout = astGetI( mapin, "Nout" );

/* We first try to split the Mapping up into a set of parallel Mappings
   each of which has only a single input (i.e. a separate Mapping for
   each input). If any inputs remain we then try splitting them up
   into a set of parallel Mappings that have two inputs. We continue like
   this, increasing the number of axes in each parallel Mapping, until all
   inputs have been assigned to a parallel Mapping. */
   ndone = 0;
   for( mpax = 1; mpax <= nin; mpax++ ){

/* Initialise the array holding the current selection of 1-based input
   indices. */
      for( j = 0; j < mpax; j++ ){
         p[ j ] = 1;
      }

/* We now loop through all possible combinations of "mpax" inputs. */
      more = ( ndone < nin );
      while( more ){

/* If the current input selection includes any axes that have
   already been assigned to a Mapping, or if it contains any duplicated
   axes, we ignore it. */
         ignore = 0;
         for( j = 0; j < mpax; j++ ){
            if( inmap[ p[ j ] - 1 ] != -1 ) ignore = 1;
         }

         for( j = 0; j < mpax - 1; j++ ){
            for( i = j + 1; i < mpax; i++ ){
               if( p[ j ] == p[ i ] ) ignore = 1;
            }
         }

/* If we can use the current selection of inputs, test to see if these
   inputs can be split off from the complete Mapping. */
         if( !ignore ) {
            astMapSplit( mapin, mpax, p, out, &omap );

/* If they can, increment the number of returned Mappings. */
            if( omap ) {
               (*nmap)++;

/* See if the Mapping has an inverse transformation. If not, replace the
   Mapping with a TranMap that uses the original Mapping to define the
   forward transformation, and a PermMap that generates bad values to
   define the inverse transformation. */
               tnout = astGetI( omap, "Nout" );

               if( !astGetL( omap, "TranInverse" ) ) {
                  hasinv[ *nmap - 1 ] = 0;
                  memset( perm, 0, sizeof( perm ) );
                  invmap = astPermMap( mpax, perm, tnout, perm, NULL, " " );
                  tmap = (AstMapping *) astTranMap( omap, invmap, " " );
                  omap = astAnnul( omap );
                  invmap = astAnnul( invmap );
                  omap = tmap;
               } else {
                  hasinv[ *nmap - 1 ] = 1;
               }

/* Store the Mapping and the values needed to associate each input
   with an input of the returned Mapping. */
               maps[ *nmap - 1 ] = omap;

               for( j = 0; j < mpax; j++ ){
                  inmap[ p[ j ] - 1 ] = *nmap - 1;
                  inind[ p[ j ] - 1 ] = j + 1;
               }

               for( j = 0; j < tnout; j++ ){
                  outmap[ out[ j ] - 1 ] = *nmap - 1;
                  outind[ out[ j ] - 1 ] = j + 1;
               }

/* Record the number of inputs that have been assigned to a Mapping.
   If all inputs have been assigned we can abort the loop. */
               ndone += mpax;
               if( ndone == nin ) more = 0;

            }
         }

/* Move on to the next set of inputs. */
         j = 0;
         ovflow = 1;
         while( ovflow ){
            p[ j ]++;
            if( p[ j ] > nin ) {
               p[ j ] = 1;
               j++;
               if( j >= mpax ) {
                  more = 0;
                  ovflow = 0;
               }
            } else {
               ovflow = 0;
            }
         }
      }
   }

/* See how many inputs have been used. */
   used = 0;
   for( j = 0; j < nin; j++ ){
      if( inmap[ j ] != -1 ) used++;
   }

/* If any inputs have not been used, we cannot split the supplied
   Mapping up so use the Mapping as supplied for all inputs. */
   if( used < nin ) {

/* Annul AST objects for any inputs that have been used. */
      for( j = 0; j < nin; j++ ){
         if( maps[ j ] ) maps[ j ] = astAnnul( maps[ j ] );
      }

/* Return the supplied Mapping. */
      maps[ 0 ] = astClone( mapin );
      hasinv[ 0 ] = astGetL( mapin, "TranInverse" );
      *map = astClone( mapin );

/* Reset the number of Mappings being returned to 1. */
      *nmap = 1;

/* Ensure all inputs use the same Mapping. */
      for( j = 0; j < nin; j++ ){
         inmap[ j ] = 0;
         inind[ j ] = j + 1;
      }

/* Ensure all outputs use the same Mapping. */
      for( j = 0; j < nout; j++ ){
         outmap[ j ] = 0;
         outind[ j ] = j + 1;
      }

/* If the Mapping was split succesfully, join all the parallel Mappings
   back together again to create the complete Mapping. We do this to
   create a Mapping that we know will have an inverse transformation
   (because each of the individual parallel Mappings has an inverse). */
   } else {

/* Loop round each of the returned Mappings. */
      iin = 1;
      iout = 1;
      for( imap = 0; imap < *nmap; imap++ ){

/* If this is the first Mapping, just take a clone of it. Otherwise, join
   it in parallel with the current total Mapping. */
         if( imap == 0 ) {
            *map = astClone( maps[ 0 ] );
         } else {
            tmap = (AstMapping *) astCmpMap( *map, maps[ imap ], 0, " " );
            *map = astAnnul( *map );
            *map = tmap;
         }

/* Update the array holding the input input that corresponds to each
   input of the current total Mapping. Loop round each input of the
   returned Mapping just added into the total Mapping. */
         tnin = astGetI( maps[ imap ], "Nin" );
         for( i = 1; i <= tnin; i++ ){

/* Search through all the inputs, looking for the one that feeds
   input "i" of Mapping "imap". When found, store its index in the "inprm"
   array. */
            for( j = 0; j < nin; j++ ){
               if( inmap[ j ] == imap && inind[ j ] == i ) {
                  inprm[ iin - 1 ] = j + 1;
                  iin++;
               }
            }
         }

/* Update the array holding the output index that corresponds to each
   output of the current total Mapping. Loop round each output of the
   returned Mapping just added into the total Mapping. */
         tnout = astGetI( maps[ imap ], "Nout" );
         for( i = 1; i <= tnout; i++ ){

/* Search through all the outputs of the supplied Mapping, looking for the
   one that corresponds to output "I" of Mapping "IMAP". When found, store
   its index in the "outprm" array. */
            for( j = 0; j < nout; j++ ){
               if( outmap[ j ] == imap && outind[ j ] == i ) {
                  outprm[ iout - 1 ] = j + 1;
                  iout++;
               }
            }
         }
      }

/* Sanity check... */
      if( *status == SAI__OK ) {
         if( iin != nin + 1 ) {
            *status = NDF__FATIN;
            msgSeti( "IIN", iin );
            msgSeti( "NP", nin + 1 );
            errRep( " ", "ndf1Mpanl: 'iin' (^IIN) is not 'nin+1' (^NP) "
                    "(internal programming error).", status );
         }
      }

/* If required, add a PermMap to the start of the total Mapping that
   permutes the input indices into the order required by the total
   Mapping. */
      needpm = 0;

      for( i = 0; i < nin; i++ ){
         perm[ inprm[ i ] - 1 ] = i + 1;
         if( inprm[ i ] != i + 1 ) needpm = 1;
      }

      if( needpm ) {
         pmap = astPermMap( nin, perm, nin, inprm, NULL, " " );
         tmap = (AstMapping *) astCmpMap( pmap, *map, 1, " " );
         pmap = astAnnul( pmap );
         *map = astAnnul( *map );
         *map = tmap;
      }

/* If required, add a PermMap to the end of the total Mapping that
   permutes the output indices from the order produced by the total
   Mapping to the order in the supplied Mapping. Also, add in constants
   values for any outputs which are not created by any of the returned
   Mappings. First check if the returned Mappings do not have the same
   number of outputs as the supplied Mapping... */
      if( iout - 1 != nout ) {

/* If so, we will definitely require a PermMap. */
         needpm = 1;

/* Initialise the PermMap inputs corresponding to each PermMap output. */
         for( i = 0; i < nout; i++ ){
            perm[ i ] = 0;
         }

/* Transform the input position (0,0,0,...) into the output using the
   supplied Mapping. This gives us the constant values to use for the
   missing outputs (in "conout"). */
         for( i = 0; i < nin; i++ ){
            zero[ i ] = 0.0;
         }
         astTranN( mapin, 1, nin, 1, zero, 1, nout, 1, conout );

/* If we have got the right number of outputs, assume we do not need to
   use the PermMap. */
      } else {
         needpm = 0;
      }

/* Now check each output from each of the returned Mappings. */
      for( i = 0; i < iout - 1; i++ ){

/* Set up a one to one correspondance between the PermMap input and the
   required PermMap output. */
         perm[ outprm[ i ] - 1 ] = i + 1;

/* If the corresponding input and output do not both have the same index,
   we will need to use the PermMap. */
         if( outprm[ i ] != i + 1 ) needpm = 1;
      }

/* Only proceed if we need to use the PermMap. */
      if( needpm ) {

/* Replace any zero axis indices with the (negated) index of the
   corresponding constant output value. */
         for( i = 0; i < nout; i++ ){
            if( perm[ i ] == 0 ) perm[ i ] = -(i + 1);
         }

/* Create the PermMap. */
         pmap = astPermMap( iout - 1, outprm, nout, perm, conout, " " );

/* Put in series with the returned Mapping. */
         tmap = (AstMapping *) astCmpMap( *map, pmap, 1, " " );
         pmap = astAnnul( pmap );
         *map = astAnnul( *map );
         *map = tmap;
      }

   }

/* Call error tracing function and exit. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1Mpanl", status );
}

