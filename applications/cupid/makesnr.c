#include "sae_par.h" 
#include "mers.h" 
#include "ndf.h" 
#include "star/ndg.h" 
#include "star/kaplibs.h" 
#include "star/grp.h" 
#include "par.h" 
#include "prm_par.h"
#include "cupid.h" 
#include <math.h> 
#include <string.h> 
#include <stdio.h>

void makesnr() {
/*
*+
*  Name:
*     MAKESNR

*  Purpose:
*     Create a signal-to-noise NDF.

*  Language:
*     C

*  Type of Module:
*     ADAM A-task

*  Synopsis:
*     void makesnr();

*  Description:
*     This application creates a new NDF from an existing NDF by dividing
*     the Data component of the input NDF by the square root of its
*     Variance component. The Data array in the output NDF thus measures 
*     the signal to noise ratio in the input NDF. 
*
*     Anomalously small values in the input Variance array can cause very
*     large spurious values in the output signal to noise array. To avoid 
*     this, pixels that have a Variance value below a given threshold are 
*     set bad in the output NDF.

*  Usage:
*     makesnr in out minvar

*  ADAM Parameters:
*     IN = NDF (Update)
*        The input NDF. An error is reported if this NDF does not have a
*        Variance array.
*     MINVAR = _REAL (Read)
*        The minimum Variance value to be used. Input pixels that have
*        Variance values smaller than this value will be set bad in the 
*        output. The suggested default is determined by first forming a
*        histogram of the logarithm of the Variance values. The highest
*        peak is then found in this histogram. The algorithm then moves
*        down from this peak towards lower Variance values until the
*        histogram has dropped to a value equal to the square root of 
*        the peak value, or a significant minima is encountered in the
*        histogram. The corresponding Variance value is used as the
*        suggested default. []
*     OUT = NDF (Write)
*        The output signal to noise NDF. The Variance component of this NDF 
*        will be filled with the value 1.0 (except that bad Data values will 
*        also have bad Variance values).

*  Authors:
*     DSB: David S. Berry
*     {enter_new_authors_here}

*  History:
*     6-APR-2006 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/      

/* Local Constants: */
#define NBIN 200

/* Local Variables: */
   Grp *grp;                    /* GRP identifier for configuration settings */
   double *din;                 /* Pointer to next Data element */
   double *dout;                /* Pointer to next output Data element */
   double *ipdin;               /* Pointer to Data array */
   double *ipdout;              /* Pointer to output Data array */
   double *ipvin;               /* Pointer to input Variance array */
   double *ipvout;              /* Pointer to input Variance array */
   double *vin;                 /* Pointer to next input Variance element */
   double *vout;                /* Pointer to next input Variance element */
   double bin_width;            /* Width of a log10(var) histogram bin */
   double hmax;                 /* Maximum log10(var) value in histogram */
   double hmin;                 /* Minimum log10(var) value in histogram */
   double maxv;                 /* Maximum variance value */   
   double minv;                 /* Minimum variance value */   
   double minvar;               /* Smallest usable Variance value */
   double target;               /* Square root of maximum bin polulation */
   int el;                      /* Number of array elements mapped */
   int hist[ NBIN ];            /* Log10(var) histogram populations */
   int i;                       /* Loop count */
   int ibin;                    /* Histohgram bin index */
   int indf2;                   /* Identifier for output NDF */
   int indf;                    /* Identifier for input NDF */
   int maxbin;                  /* Index of bin with largest population */
   int maxpop;                  /* The largest bin population in the histogram */
   int minbin;                  /* Index of bin with smallest population */
   int minpop;                  /* Smallest bin population found so far */
   int nbad;                    /* Number of rejected pixels */
   int size;                    /* Size of GRP group */

/* Abort if an error has already occurred. */
   if( *status != SAI__OK ) return;

/* Start an NDF context */
   ndfBegin();

/* Get an identifier for the input NDF. We use NDG (via kpg1_Rgndf)
   instead of calling ndfAssoc directly since NDF/HDS has problems with
   file names containing spaces, which NDG does not have. */
   kpg1Rgndf( "IN", 1, 1, "", &grp, &size, status );
   ndgNdfas( grp, 1, "READ", &indf, status );
   grpDelet( &grp, status );
   
/* Map the input Variance component as an array of doubles. */
   ndfMap( indf, "Variance", "_DOUBLE", "READ", (void *) &ipvin, &el, status );

/* Find the maximum and minimum good, non-zero Variance value. */
   if( *status == SAI__OK ) {
      minv = VAL__MAXD;
      maxv = VAL__MIND;
      vin = ipvin;
      for( i = 0; i < el; i++, vin++ ) {
         if( *vin != VAL__BADD && *vin > 0.0 ) {
            if( *vin < minv ) minv = *vin;
            if( *vin > maxv ) maxv = *vin;
         }
      }            

/* Report an error if there are no usable Variances */
      if( maxv < minv ) {
         *status = SAI__ERROR;
         errRep( "", "No usable Variance values found", status );

/* Otherwise, form a histogram of log10(variance) values. */
      } else {
         hmin = log10( minv );
         hmax = log10( maxv );
         bin_width = ( hmax - hmin )/NBIN;

         for( ibin = 0; ibin < NBIN; ibin++ ) hist[ ibin ] = 0;

         vin = ipvin;
         for( i = 0; i < el; i++, vin++ ) {
            if( *vin != VAL__BADD && *vin > 0.0 ) {
               ibin = (int) ( ( log10( *vin ) - hmin )/ bin_width );
               if( ibin < 0 ) {
                  ibin = 0;
               } else if( ibin == el ) {
                  ibin--;
               }
               hist[ ibin ]++;
            }
         }            

/* Find the bin with the largest population. */
         maxpop = -1;
         for( ibin = 0; ibin < NBIN; ibin++ ) {
            if( hist[ ibin ] > maxpop ) {
               maxpop = hist[ ibin ];
               maxbin = ibin;
            }
         }

/* Starting at this peak bin, work down to smaller variances until a bin
   is found that has a population less than the square root of the maximum
   population, or a significant minimum is encountered (that is, a pixel
   that is lower than the subsequent 4 pixels ). */
         target = sqrt( maxpop );
         minpop = maxpop;
         minvar = minv;
         for( ibin = maxbin; ibin >= 0; ibin-- ) {
            if( hist[ ibin ] < target ) {
               minvar = pow( 10.0, ( ibin + 0.5 )*bin_width + hmin );
               break;

            } else if( hist[ ibin ] < minpop ) {
               minpop = hist[ ibin ];
               minbin = ibin;
           
            } else if( minbin - ibin == 4 ) {
               minvar = pow( 10.0, ( minbin + 0.5 )*bin_width + hmin );
               break;
            }
         }

/* Get a value for the minimum allowed Variance value using the value
   found above as the dynamic default. */
         parDef0d( "MINVAR", minvar, status );
         parGet0d( "MINVAR", &minvar, status );
      }
   }

/* Create the output by propagating everything except the Unit, Data and
   Variance arrays. */
   ndfProp( indf, "AXIS,WCS,QUALITY", "OUT", &indf2, status );

/* Map the input Data component, and the output Data and Variance components. */
   ndfMap( indf, "Data", "_DOUBLE", "READ", (void *) &ipdin, &el, status );
   ndfMap( indf2, "Data", "_DOUBLE", "WRITE", (void *) &ipdout, &el, status );
   ndfMap( indf2, "Variance", "_DOUBLE", "WRITE", (void *) &ipvout, &el, status );

/* Store appropriate values in the output Data and Variance components. */
   if( *status == SAI__OK ) {
      din = ipdin;
      dout = ipdout;
      vin = ipvin;
      vout = ipvout;
      nbad = 0;

      for( i = 0; i < el; i++, vin++, vout++, din++, dout++ ) {
         if( *din != VAL__BADD && *vin != VAL__BADD ) {
            if( *vin >= minvar ) {
               *dout = *din/sqrt( *vin );
               *vout = 1.0;
            } else {
               nbad++;
               *dout = VAL__BADD;
               *vout = VAL__BADD;
            }

         } else {
            *dout = VAL__BADD;
            *vout = VAL__BADD;
         }
      }            

      if( nbad >0 ) {
         msgBlank( status );
         if( nbad == 1 ) {
            msgSetd( "L", minvar );
            msgOut( "", "   1 pixel with Variance below ^L was set bad", status );
         } else {
            msgSetd( "L", minvar );
            msgSeti( "N", nbad );
            msgOut( "", "   ^N pixels with Variance below ^L were set bad", status );
         }
         msgBlank( status );
      }
   }

/* End the NDF context */
   ndfEnd( status );

/* If an error has occurred, issue another error report identifying the 
   program which has failed (i.e. this one). */
   if( *status != SAI__OK ) {
      errRep( "MAKESNR_ERR", "MAKESNR: Failed to create a signal-to-noise "
              "NDF.", status );
   }

}

