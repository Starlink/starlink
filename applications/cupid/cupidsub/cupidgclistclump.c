#include "sae_par.h"
#include "cupid.h"
#include "mers.h"

void cupidGCListClump( int iclump, int ndim, double *par, double chisq,
                       int *lbnd, int ilevel, double rms ){
/*
*  Name:
*     cupidGCListClump

*  Purpose:
*     Add a clump to the output list. 

*  Synopsis:
*     void cupidGCListClump( int iclump, int ndim, double *par, double chisq, 
*                            int *lbnd, int ilevel, double rms )

*  Description:
*     This function adds a clump to the output list. 

*  Parameters:
*     iclump
*        The (1-based) index of the clump to be added to the list.
*     ndim
*        The number of pixel axes in the NDF.
*     par
*        The parameters describing the Gaussian fit to the clump (see
*        cupidGCFit.cgen for a description)..
*     lbnd
*        Lower pixel bounds of supplied data array.
*     chisq
*        The chi-squared associated with the fit.
*     ilevel
*        The amount of information to display to standard output.
*     rms
*        The RMS noise level.

*  Authors:
*     DSB: David S. Berry
*     {enter_new_authors_here}

*  History:
*     5-OCT-2005 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}
*/      

/* Local Variables: */
   int np;              /* Number of parameters */

/* Abort if an error has already occurred. */
   if( *status != SAI__OK ) return;

/* Determine the number of significant parameters for the Gaussian model. */
   if( ndim == 1 ) {
      np = CUPID__GCNP1;
   } else if( ndim == 2 ) {
      np = CUPID__GCNP2;
   } else {
      np = CUPID__GCNP3;
   }

/* Report information to standard output if requested. */
   if( ilevel == 2 || ilevel == 3 ) {
      if(  ilevel == 3 ) msgBlank( status );
      msgSeti( "N", iclump );
      msgOut( "", "Clump ^N:", status );
   } else if( ilevel > 3 ) {
      msgSeti( "N", iclump );
      msgOut( "", "   Storing clump ^N:", status );
   }

   if( ilevel > 2 ) {
      msgSetd( "V", chisq );
      msgOut( "", "   Chi-squared: ^V", status );

      msgSetd( "V", par[ 0 ]*rms );
      msgOut( "", "   Peak intensity: ^V", status );
      msgSetd( "V", par[ 1 ]*rms );
      msgOut( "", "   Constant background: ^V", status );
      msgSetd( "V", par[ 2 ] + lbnd[ 0 ] - 1.5 );
      msgOut( "", "   Centre on 1st axis: ^V", status );
      msgSetd( "V", par[ 3 ] );
      msgOut( "", "   FWHM on 1st axis: ^V", status );

      if( ndim > 1 ) {
         msgSetd( "V", par[ 4 ] + lbnd[ 1 ] - 1.5 );
         msgOut( "", "   Centre on 2nd axis: ^V", status );
         msgSetd( "V", par[ 5 ] );
         msgOut( "", "   FWHM on 2nd axis: ^V", status );
         msgSetd( "V", par[ 6 ] );
         msgOut( "", "   Position angle: ^V", status );

         if( ndim > 2 ) {
            msgSetd( "V", par[ 7 ] + lbnd[ 2 ] - 1.5 );
            msgOut( "", "   Centre on vel axis: ^V", status );
            msgSetd( "V", par[ 8 ] );
            msgOut( "", "   FWHM on vel axis: ^V", status );
            msgSetd( "V", par[ 9 ] );
            msgOut( "", "   Vel gradient on 1st axis: ^V", status );
            msgSetd( "V", par[ 10 ] );
            msgOut( "", "   Vel gradient on 2nd axis: ^V", status );
         }
      }
   }
}

