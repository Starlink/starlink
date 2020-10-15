/*
*+
*  Name:
*     smf_freepolbins

*  Purpose:
*     Frees the memory used to describe a set of polarisation angle bins.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     ptime = smf_freepolbins( int nndf, int npbin, double **pangle,
*                              dim_t ***ptime, int *status )

*  Arguments:
*     nndf = int (Given)
*        The number of input NDFs.
*     npbin = int (Given)
*        The number of polarisation angle bins.
*     pangle = double ** (Given)
*        Pointer to a location holding a pointer to an array of "npbin"
*        polarisation angles. The memory used to hold this array is freed
*        and a NULL pointer is placed at *pangle.
*     ptime = dim_t *** (Given)
*        Pointer to an array holding lists of time slice indices for each
*        polarisation angle and input NDF, as created by smf_choosepolbins.
*        The memory used to store these lists (and the associated arrays
*        of pointers) is freed.
*     status = int * (Given and Returned)
*        Inherited status value. This function attempts to execute even
*        if status is set to an error value on entry.

*  Returned Value:
*     A NULL pointer is always returned, and should be assigned to the
*     "ptime" pointer to avoid accidental re-use of the freed pointer value.

*  Description:
*     This function frees the resources allopcated by smf_choosetiles to
*     describe a set of polarisation angle bins.

*  Authors:
*     David S Berry (JAC, UCLan)
*     {enter_new_authors_here}

*  History:
*     12-OCT-2007 (DSB):
*        Initial version.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2007 Science & Technology Facilities Council.
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
*     Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
*     MA 02110-1301, USA

*  Bugs:
*     {note_any_bugs_here}
*-
*/

/* Starlink includes */
#include "ast.h"
#include "sae_par.h"

/* SMURF includes */
#include "libsmf/smf.h"

dim_t ***smf_freepolbins( int nndf, int npbin, double **pangle, dim_t ***ptime,
                        int *status __attribute__((unused)) ){

/* Local Variables */
   int i;
   int j;

/* Free the memory holding the angle for each bin. */
   if( pangle ) *pangle = astFree( *pangle );

/* Loop round each input NDF. */
   if( ptime ) {
      for( i = 0; i < nndf; i++ ) {

/* Loop round each polarisation angle bin. */
         if( ptime[ i ] ) {
            for( j = 0; j < npbin; j++ ) {

/* Free the array used to hold time slice indices for this bin and input
   NDF. */
               ptime[ i ][ j ] = astFree( ptime[ i ][ j ] );
            }

/* Free the array used to hold the pointers for this input NDF. */
            ptime[ i ] = astFree( ptime[ i ] );
         }
      }

/* Free the array used to hold the pointers for all NDFs. */
      ptime = astFree( ptime );

   }

/* Return a NULL pointer. */
   return NULL;
}
