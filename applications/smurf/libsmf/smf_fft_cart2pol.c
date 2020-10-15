/*
*+
*  Name:
*     smf_fft_cart2pol

*  Purpose:
*     Convert between cartesian and polar representations of fft'd data

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     SMURF subroutine

*  Invocation:
*     void smf_fft_cart2pol( ThrWorkForce *wf, smfData *data, int inverse,
*                            int power, int *status );

*  Arguments:
*     wf = ThrWorkForce * (Given)
*        Pointer to a pool of worker threads
*     data = smfData * (Given)
*        smfData to convert
*     inverse = int (Given)
*        If set convert polar --> cartesian. Otherwise cartesian --> polar.
*     power = int (Given)
*        If set, magnitudes in polar form are squared and then divided by
*        the frequency spacing df to get power spectral density units
*        (e.g. pW / Hz)
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Convert between cartesian representation of the transform (real,
*     imaginary) and polar form (amplitude, argument). The argument is
*     stored in radians in the range (-PI,PI).

*  Authors:
*     EC: Ed Chapin (UBC)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     DSB: David Berry (JAC, Hawaii)
*     {enter_new_authors_here}

*  Notes:
*     Currently the only check for a valid input storage form is on the
*     range of the argument (-pi,pi) in the case of polar-->cartesian
*     conversion. Otherwise it is up to the caller to know what form
*     their data are stored in.

*  History:
*     2008-09-18 (EC):
*        Initial version.
*     2009-10-13 (TIMJ):
*        Handle case where one of baseI or baseR are bad but not both.
*     2011-09-23 (EC):
*        Handle 2D FFTs
*     2011-09-28 (EC):
*        Handle normalization and units of 2-d PSDs properly
*     2012-05-24 (DSB):
*        Multi-thread.

*  Copyright:
*     Copyright (C) 2009, 2012 Science & Technology Facilities Council.
*     Copyright (C) 2008,2011 University of British Columbia.
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

#if HAVE_CONFIG_H
#include <config.h>
#endif

/* System includes */
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>

/* Starlink includes */
#include "sae_par.h"
#include "mers.h"
#include "ndf.h"
#include "prm_par.h"
#include "star/ndg.h"
#include "star/grp.h"
#include "msg_par.h"
#include "star/one.h"

/* SMURF routines */
#include "smf.h"
#include "smf_typ.h"
#include "smf_err.h"

/* Prototypes for local static functions. */
static void smf1_fft_cart2pol( void *job_data_ptr, int *status );

/* Local data types */
typedef struct smfFFTCart2PolData {
   dim_t b1;
   dim_t b2;
   dim_t nf;
   double *dat_data;
   double df;
   int inverse;
   int power;
   dim_t ntransforms;
} SmfFFTCart2PolData;

#define FUNC_NAME "smf_fft_cart2pol"

void smf_fft_cart2pol( ThrWorkForce *wf, smfData *data, int inverse, int power,
                       int *status ) {

  double df;                    /* Product of df all axes */
  double df_data[2]={1,1};      /* frequency steps in Hz, or (1/arcsec)^2 */
  dim_t fdims[2];               /* Lengths of frequency-space axes */
  dim_t i;                      /* Loop counter */
  dim_t nbolo=0;                /* Number of detectors  */
  int ndims;                    /* Number of real-space dimensions */
  dim_t ntransforms;            /* Number of transforms in the data */
  dim_t nf=0;                   /* Number of frequencies in FFT */
  dim_t rdims[2];               /* Lengths of real-space axes */
  int nw;                       /* Number of worker threads */
  int iw;                       /* Thread index */
  SmfFFTCart2PolData *pdata;    /* Pointer to data for one job */
  SmfFFTCart2PolData *job_data = NULL; /* Pointer to data for all jobs */
  dim_t transtep;               /* No. of transforms per thread */

  if( *status != SAI__OK ) return;

  if( data == NULL ) {
    *status = SAI__ERROR;
    errRep( "", FUNC_NAME ": NULL smfData pointer provided"
            " (possible programming error)", status);
  }

  if( !smf_isfft(data, rdims, &nbolo, fdims, df_data, &ndims, status) ) {
    *status = SAI__ERROR;
    errRep( "", FUNC_NAME ": smfData provided is not FFT data"
            " (possible programming error)", status);
  }

  if( *status != SAI__OK ) return;

  nf = fdims[0];
  df = df_data[0];
  for( i=1; i<ndims; i++ ) {
    nf *= fdims[i];
    df *= df_data[i];
  }

  /* Loop over bolometer if time-series, or only a single pass if we
     are looking at the FFT of a map. */
  ntransforms = (ndims == 1) ? nbolo : 1;

  /* How many threads do we get to play with */
  nw = wf ? wf->nworker : 1;

  /* Find how many transformations to process in each worker thread. */
  transtep = ntransforms/nw;
  if( transtep == 0 ) {
     transtep = 1;
     nw = (int) ntransforms;
  }

  /* Allocate job data for threads, and store the range of bolos to be
     processed by each one. Ensure that the last thread picks up any
     left-over bolos. */
  job_data = astCalloc( nw, sizeof(*job_data) );
  if( *status == SAI__OK ) {

    for( iw = 0; iw < nw; iw++ ) {
      pdata = job_data + iw;
      pdata->b1 = iw*transtep;
      if( iw < nw - 1 ) {
         pdata->b2 = pdata->b1 + transtep - 1;
      } else {
         pdata->b2 = ntransforms - 1 ;
      }

      /* Store other values common to all jobs. */
      pdata->dat_data = data->pntr[0];
      pdata->df = df;
      pdata->inverse = inverse;
      pdata->power = power;
      pdata->nf = nf;
      pdata->ntransforms = ntransforms;

      /* Submit the job to the workforce. */
      thrAddJob( wf, 0, pdata, smf1_fft_cart2pol, 0, NULL, status );
    }

    /* Wait for all jobs to complete. */
    thrWait( wf, status );

    /* Free the job data. */
    job_data = astFree( job_data );
  }

  /* Convert the units and labels of the axes using AST */
  if( data->hdr && power && (data->hdr->units[0] != '\0') ) {
    AstFrame *unitframe = NULL;
    char newunits[SMF__CHARLABEL];
    char label[SMF__CHARLABEL];
    char *psd_forward=NULL;
    char *psd_inverse=NULL;
    char psd_forward_1[] = ")**2/Hz";
    char psd_inverse_1[] = "*Hz)**0.5";
    char psd_forward_2[] = ")**2*arcsec**2";
    char psd_inverse_2[] = "/arcsec**2)**0.5";


    if( ndims == 1 ) {
      psd_forward = psd_forward_1;
      psd_inverse = psd_inverse_1;
    } else if( ndims == 2 ) {
      psd_forward = psd_forward_2;
      psd_inverse = psd_inverse_2;
    } else {
      msgOut( "", FUNC_NAME
              ": WARNING! Don't currently modify units for map PSDs "
              "correctly", status );
    }

    if( psd_forward && psd_inverse ) {
      /* Use a frame to store the modified units which AST will then simplify */
      unitframe = astFrame( 1, " " );

      /* Get the original units */
      one_strlcpy( newunits, "(", sizeof(newunits), status );
      one_strlcat( newunits, data->hdr->units, sizeof(newunits), status );

      if( inverse ) {
        /* Undo PSD units */
        one_strlcat( newunits, psd_inverse, sizeof(newunits), status );
        one_strlcpy( label, "Signal", sizeof(label), status );
      } else {
        /* Change to PSD units */
        one_strlcat( newunits, psd_forward, sizeof(newunits), status );
        one_strlcpy( label, "PSD", sizeof(label), status );
      }

      /* Simplify the units and store */
      astSetC( unitframe, "Unit(1)", newunits );
      smf_set_clabels( NULL, label, astGetC( unitframe, "NormUnit(1)" ),
                       data->hdr, status );

      /* Clean up */
      unitframe = astAnnul( unitframe );
    }

  }
}
















static void smf1_fft_cart2pol( void *job_data_ptr, int *status ) {
/*
*  Name:
*     smf1_fft_cart2pol

*  Purpose:
*     Executed in a worker thread to do various calculations for
*     smf_fft_cart2pol.

*  Invocation:
*     smf1_fft_cart2pol( void *job_data_ptr, int *status )

*  Arguments:
*     job_data_ptr = SmfFFTCart2PolData * (Given)
*        Data structure describing the job to be performed by the worker
*        thread.
*     status = int * (Given and Returned)
*        Inherited status.

*/

/* Local Variables: */
   SmfFFTCart2PolData *pdata;
   dim_t ibolo;
   dim_t ifreq;
   double *baseI;
   double *baseR;
   double amp;
   double imag;
   double real;
   double theta;

/* Check inherited status */
   if( *status != SAI__OK ) return;

/* Get a pointer that can be used for accessing the required items in the
   supplied structure. */
   pdata = (SmfFFTCart2PolData *) job_data_ptr;

/* Pointers to components for the first bolo. */
   baseR = pdata->dat_data;
   baseR += pdata->b1*pdata->nf;
   baseI = baseR + pdata->nf*pdata->ntransforms;

/* Do each bolo in turn. */
   for( ibolo = pdata->b1; ibolo <= pdata->b2 && *status == SAI__OK; ibolo++ ) {

/* Inverse transformation (polar to Cartesian). */
      if( pdata->inverse ) {

         for( ifreq = 0; ifreq < pdata->nf; ifreq++ ) {
            if( baseR[ ifreq ] != VAL__BADD && baseI[ ifreq ] != VAL__BADD ) {


/* Check for valid argument */
               if( fabs( baseI[ ifreq ] ) > AST__DPI ) {
                  *status = SAI__ERROR;
                  errRep( "", FUNC_NAME ": abs(argument) > PI. FFT data "
                          "may not be in polar form.", status);
                  break;

               } else {

/* Convert polar --> cartesian */
                  if( pdata->power ) {

/* Converting from PSD. Check for sqrt of negative number */
                     if( baseR[ ifreq ] < 0 ) {
                        *status = SAI__ERROR;
                        errRep( "", FUNC_NAME ": amplitude^2 < 0. FFT data "
                                "may not be in correct form", status);
                        break;

                     } else {
                        amp = sqrt( baseR[ ifreq ]*pdata->df );
                     }

                  } else {
                     amp = baseR[ ifreq ];
                  }

                  real = amp*cos( baseI[ ifreq ]);
                  imag = amp*sin( baseI[ ifreq ]);
                  baseR[ ifreq ] = real;
                  baseI[ ifreq ] = imag;
               }

            } else {
               baseR[ ifreq ] = VAL__BADD;
               baseI[ ifreq ] = VAL__BADD;
            }
         }

/* Forward transformation (Cartesian to polar). */
      } else {

         for( ifreq = 0; ifreq < pdata->nf; ifreq++ ) {

/* Convert cartesian --> polar */
            if( baseR[ ifreq ] != VAL__BADD && baseI[ ifreq ] != VAL__BADD ) {
               amp = baseR[ ifreq ]*baseR[ ifreq ] + baseI[ ifreq ]*baseI[ ifreq ];

/* Calculate power spectral density */
               if( pdata->power ) {
                  amp /= pdata->df;

/* Normal polar form */
               } else {
                  amp = sqrt( amp );
               }

               theta = atan2( baseI[ ifreq ], baseR[ ifreq ] );
               baseR[ ifreq ] = amp;
               baseI[ ifreq ] = theta;

            } else {
               baseR[ ifreq ] = VAL__BADD;
               baseI[ ifreq ] = VAL__BADD;
            }
         }
      }

/* Prepare for next bolometer. */
      baseR += pdata->nf;
      baseI += pdata->nf;
   }
}
