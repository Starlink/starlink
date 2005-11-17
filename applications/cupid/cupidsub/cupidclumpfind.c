#include "sae_par.h"
#include "mers.h"
#include "cupid.h"
#include "ast.h"

void cupidClumpFind( int type, int ndim, int *slbnd, int *subnd, 
                     void *ipd, void *ipv, AstKeyMap *config, int velax, 
                     int ilevel ){
/*
*  Name:
*     cupidClumpFind

*  Purpose:
*     Identify clumps of emission within a 2 or 3 dimensional NDF using
*     the CLUMPFIND algorithm.

*  Synopsis:
*     void cupidClumpFind( type, int ndim, int *slbnd, int *subnd, 
*                          void *ipd, void *ipv, AstKeyMap *config, 
*                          int velax, int ilevel )

*  Description:
*     This function identifies clumps within a 2 or 3 dimensional data
*     array using the CLUMPFIND algorithm, described by Williams et al 
*     (1994, ApJ 428, 693). This algorithm works by first contouring the 
*     data at a multiple of the noise, then searches for peaks of emission 
*     which locate the clumps, and then follows them down to lower 
*     intensities. No a priori clump profile is assumed. In this algorithm, 
*     clumps never overlap.

*  Parameters:
*     type
*        An integer identifying the data type of the array values pointed to 
*        by "ipd". Must be either CUPID__DOUBLE or CUPID__FLOAT (defined in
*        cupid.h).
*     ndim
*        The number of dimensions in the data array. Must be 2 or 3.
*     slbnd
*        Pointer to an array holding the lower pixel index bound of the
*        data array on each axis.
*     subnd
*        Pointer to an array holding the upper pixel index bound of the
*        data array on each axis.
*     ipd
*        Pointer to the data array. The elements should be stored in
*        Fortran order. The data type of this array is given by "itype".
*     ipv
*        Pointer to the input Variance array, or NULL if there is no Variance
*        array. The elements should be stored in Fortran order. The data 
*        type of this array is given by "itype".
*     config
*        An AST KeyMap holding tuning parameters for the algorithm.
*     velax
*        The index of the velocity axis in the data array (if any). Only
*        used if "ndim" is 3. 
*     ilevel
*        Amount of scren information to display (in range zero to 3).

*  Authors:
*     DSB: David S. Berry
*     {enter_new_authors_here}

*  History:
*     29-SEP-2005 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}
*/      

/* Local Variables: */

/* Abort if an error has already occurred. */
   if( *status != SAI__OK ) return;


   *status = SAI__ERROR;
   errRep( "CLUMPFIND_ERR1", "The ClumpFind algorithm has not yet "
           "been implemented.", status );


}

