/*+
 *  Name:
 *     userradii
 *
 *  Purpose:
 *     Compute average radii at user supplied surface brightness
 *     levels (or plain intensities).
 *  
 *  Authors:
 *     (PWD): Peter W. Draper (Starlink, Durham University)
 *
 *  History:
 *     15-FEB-2000 (PWD):
 *        Original version.
 *- 
 */
#include "userradii.h"

void userradii( picstruct *field, picstruct *dfield, objstruct *obj,
                pliststruct *pixel )
{
   pliststruct	*pixt;
   PIXTYPE      *thresht;
   PIXTYPE      threshs[NRAD];
   double       dval;
   float        *mu_rad;

   /*  If these measurements are required */
   if ( FLAG(obj.mu_rad[0]) ) {
 
      /*  Measurement and detection images may be same */
      if ( ! dfield ) {
         dfield = field;
      }
      for ( i=0; i < NRAD; i++ ) {

         /*  Correct for zero point of surface brightness */
         dval = prefs.radthresh[0] - prefs.radthresh[1];
         
         /*  And generate threshold intensities */
         for( i = 0; i < NRAD; i++ ) {
            threshs[i] = dfield->pixscale * dfield->pixscale * 
               pow( 10.0, -0.4 * dval );
            dval += prefs.radthresh[2]; /*  Increment to next
                                            threshold */
            fprintf( stdout, "Threshold(%d) = %f\n", i, threshs[i] );
         }
      }

      /*  If image intensities are photographic */
      if ( prefs.detect_type == PHOTO ) {
         for( i = 0; i < NRAD; i++ ) {
            threshs[i] = exp( threshs[i] / ngamma );
            fprintf( stdout, "Photo threshold(%d) = %f\n", i, threshs[i] );
         }
      }

      /*  Look at all pixels in the current object, counting those
          that lie above each of the thresholds */
      for( pixt  = pixel + obj->firstpix; 
           pixt >= pixel;
           pixt  = pixel + PLIST( pixt, nextpix ) ) {
         for ( i = NRAD, mu_rad = obj->mu_rad, thresht = threshs;
               i-- && PLIST( pixt, value ) > *thresht; 
               mu_rad++, thresht++ ) {
            (*mu_rad)++;
         }
      }

      /*  Convert counts into radii */
      for( i = 0, mu_rad = obj->mu_rad; i < NRAD; i++ ) {
         mu_rad = sqrt( mu_rad / PI );
         mu_rad++;
      }      
   }
}
