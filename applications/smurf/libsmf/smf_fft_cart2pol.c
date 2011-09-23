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
*     void smf_fft_cart2pol( smfData *data, int inverse, int power,
*                            int *status );

*  Arguments:
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
*     Ed Chapin (UBC)
*     TIMJ: Tim Jenness (JAC, Hawaii)
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

*  Copyright:
*     Copyright (C) 2009 Science & Technology Facilities Council.
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
*     Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
*     MA 02111-1307, USA

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

#define FUNC_NAME "smf_fft_cart2pol"

void smf_fft_cart2pol( smfData *data, int inverse, int power, int *status ) {

  double amp=0;                 /* Amplitude coeff */
  double *baseR=NULL;           /* base pointer to real/amplitude coeff */
  double *baseI=NULL;           /* base pointer to imag/argument coeff */
  double df=1;                  /* frequency steps in Hz */
  dim_t fdims[2];               /* Lengths of frequency-space axes */
  size_t i;                     /* Loop counter */
  double imag;                  /* Imaginary coeff */
  size_t j;                     /* Loop counter */
  dim_t nbolo=0;                /* Number of detectors  */
  size_t ndims;                 /* Number of real-space dimensions */
  size_t ntransforms;           /* Number of transforms in the data */
  dim_t nf=0;                   /* Number of frequencies in FFT */
  dim_t rdims[2];               /* Lengths of real-space axes */
  double real;                  /* Real coeff */
  double theta;                 /* Argument */

  if( *status != SAI__OK ) return;

  if( data == NULL ) {
    *status = SAI__ERROR;
    errRep( "", FUNC_NAME ": NULL smfData pointer provided"
            " (possible programming error)", status);
  }

  if( !smf_isfft(data, rdims, &nbolo, fdims, &ndims, status) ) {
    *status = SAI__ERROR;
    errRep( "", FUNC_NAME ": smfData provided is not FFT data"
            " (possible programming error)", status);
  }

  if( *status != SAI__OK ) return;

  nf = 1;
  for( i=0; i<ndims; i++ ) nf *= fdims[i];

  /* Need to know df to get normalization right when dealing with PSDs */
  if( power ) {
    /* Time-series cube */
    if( ndims == 1 ) {
      if( data->hdr && data->hdr->steptime ) {
        df = 1. / (data->hdr->steptime * (double) rdims[0] );
      } else {
        msgOut( "", FUNC_NAME ": *** Warning *** no valid steptime "
                "encountered, so setting frequency bin width df=1 "
                "(PSD units will be meaningless)", status );
      }
    } else {
        msgOut( "", FUNC_NAME
                ": WARNING! Don't currently normalize map PSDs correctly",
                status );
    }
  }


  /* Loop over bolometer if time-series, or only a single pass if we
     are looking at the FFT of a map. */

  ntransforms = (ndims == 1) ? nbolo : 1;

  for( i=0; (*status==SAI__OK)&&(i<ntransforms); i++ ) {
    /* Pointers to components of this bolo */
    baseR = data->pntr[0];
    baseR += i*nf;
    baseI = baseR + nf*ntransforms;

    if( inverse ) {

      for( j=0; (*status==SAI__OK)&&(j<nf); j++ ) {
        if( (baseR[j]!=VAL__BADD) && (baseI[j]!=VAL__BADD) ) {
          if( fabs(baseI[j]) > AST__DPI ) {
            /* Check for valid argument */
            *status = SAI__ERROR;
            errRep( "", FUNC_NAME
                    ": abs(argument) > PI. FFT data may not be in polar form.",
                    status);
          } else {
            /* Convert polar --> cartesian */
            if( power ) {
              /* Converting from PSD */
              if( baseR[j] < 0 ) {
                /* Check for sqrt of negative number */
                *status = SAI__ERROR;
                errRep( "", FUNC_NAME
                        ": amplitude^2 < 0. FFT data may not be in correct "
                        "form", status);
              } else {
                amp = sqrt(baseR[j]*df);
              }
            } else {
              amp = baseR[j];
            }

            real = amp*cos(baseI[j]);
            imag = amp*sin(baseI[j]);
            baseR[j] = real;
            baseI[j] = imag;
          }
        } else {
          baseR[j] = VAL__BADD;
          baseI[j] = VAL__BADD;
        }
      }
    } else {
      for( j=0; j<nf; j++ ) {
        /* Convert cartesian --> polar */
        if( (baseR[j]!=VAL__BADD)&&
            (baseI[j]!=VAL__BADD) ) {

          amp = baseR[j]*baseR[j] + baseI[j]*baseI[j];

          if( power ) {
            /* Calculate power spectral density */
            amp /= df;
          } else {
            /* Normal polar form */
            amp = sqrt(amp);
          }
          theta = atan2( baseI[j], baseR[j] );
          baseR[j] = amp;
          baseI[j] = theta;
        } else {
          baseR[j] = VAL__BADD;
          baseI[j] = VAL__BADD;
        }
      }
    }
  }

  /* Convert the units and labels of the axes using AST */
  if( data->hdr && power && (data->hdr->units[0] != '\0') ) {
    if( ndims == 1 ) {
      AstFrame *unitframe = NULL;
      char newunits[SMF__CHARLABEL];
      char label[SMF__CHARLABEL];

      /* Use a frame to store the modified units which AST will then simplify */
      unitframe = astFrame( 1, " " );

      /* Get the original units */
      one_strlcpy( newunits, "(", sizeof(newunits), status );
      one_strlcat( newunits, data->hdr->units, sizeof(newunits), status );

      if( inverse ) {
        /* Undo PSD units */
        one_strlcat( newunits, "*Hz)**0.5", sizeof(newunits), status );
        one_strlcpy( label, "Signal", sizeof(label), status );

      } else {
        /* Change to PSD units */
        one_strlcat( newunits, ")**2/Hz", sizeof(newunits), status );
        one_strlcpy( label, "PSD", sizeof(label), status );
      }

      /* Simplify the units and store */
      astSetC( unitframe, "Unit(1)", newunits );
      smf_set_clabels( NULL, label, astGetC( unitframe, "NormUnit(1)" ),
                       data->hdr, status );

      /* Clean up */
      unitframe = astAnnul( unitframe );
    } else {
      msgOut( "", FUNC_NAME
              ": WARNING! Don't currently modify units for map PSDs correctly",
              status );
    }
  }
}
