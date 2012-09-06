/*
*+
*  Name:
*     FTS2SPECTRUM

*  Purpose:
*     Computes the spectrum of the interferograms.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     ADAM TASK

*  Invocation:
*     smurf_fts2_spectrum(status);

*  Arguments:
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Computes the spectrum of the interferograms.

*  ADAM Parameters:
*     IN = NDF (Read)
*          Input files to be transformed.
*     OUT = NDF (Write)
*          Output files.
*     ZEROPAD = LOGICAL (Read)
*          Determines whether to zeropad.
*     RESOLUTION = _INTEGER (Read)
*          Spectral Grid Resolution.
*          0 : Low Resolution
*          1 : Medium Resolution
*          Any other value : High Resolution
*          Default behaviour is the High Resolution

*  Authors:
*     COBA: Coskun Oba (UoL)

*  History :
*     2011-06-24 (COBA):
*        Original version.
*     2011-08-16 (COBA):
*        Add Zero-Padding.
*     2011-10-18 (COBA):
*        Add subarray check.

*  Copyright:
*     Copyright (C) 2010 Science and Technology Facilities Council.
*     Copyright (C) 2010 University of Lethbridge. All Rights Reserved.

*  License:
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

/* STANDARD includes */
#include <string.h>
#include <stdio.h>

/* STARLINK includes */
#include "ast.h"
#include "ndf.h"
#include "mers.h"
#include "par.h"
#include "sae_par.h"
#include "star/ndg.h"
#include "star/grp.h"
#include "star/one.h"

/* SMURF includes */
#include "smurflib.h"
#include "libsmf/smf.h"
#include "libsc2fts/fts2.h"

/* FFTW includes */
#include <fftw3.h>

#define FUNC_NAME "smurf_fts2_spectrum"
#define TASK_NAME "FTS2SPECTRUM"

void smurf_fts2_spectrum(int* status)
{
  if( *status != SAI__OK ) { return; }

  const char*  dataLabel    = "Spectrum";     /* Data label */
  Grp* gIn                  = NULL;           /* Input group */
  Grp* gOut                 = NULL;           /* Output group */
  smfData* inData           = NULL;           /* Pointer to input data */
  smfData* outData          = NULL;           /* Pointer to output data */
  smfData* zpdData          = NULL;           /* Pointer to ZPD data */
  int zeropad               = 1;              /* Determines whether to zeropad */
  int resolution            = 2;              /* Spectral Resolution, 0=LOW, 1=MEDIUM, *=HIGH */
  int i                     = 0;              /* Counter */
  int j                     = 0;              /* Counter */
  int k                     = 0;              /* Counter */
  double fNyquist           = 0.0;            /* Nyquist frequency */
  double dSigma             = 0.0;            /* Spectral Sampling Interval */
  double* IFG               = NULL;           /* Interferogram */
  fftw_complex* SPEC        = NULL;           /* Spectrum */
  fftw_plan plan            = NULL;           /* fftw plan */

  size_t nFiles             = 0;              /* Size of the input group */
  size_t nOutFiles          = 0;              /* Size of the output group */
  size_t fIndex             = 0;              /* File index */
  size_t nWidth             = 0;              /* Data cube width */
  size_t nHeight            = 0;              /* Data cube height */
  size_t nFrames            = 0;              /* Data cube depth */
  size_t nPixels            = 0;              /* Number of bolometers in the subarray */

  double dIntensity         = 0;
  int N                     = 0;
  int N2                    = 0;
  int bolIndex              = 0;
  int badPixel              = 0;
  int indexZPD              = 0;

  /* Get Input & Output groups */
  kpg1Rgndf("IN", 0, 1, "", &gIn, &nFiles, status);
  kpg1Wgndf("OUT", gOut, nFiles, nFiles, "Equal number of input and output files expected!", &gOut, &nOutFiles, status);

  /* Read in ADAM parameters */
  parGet0i("ZEROPAD", &zeropad, status);
  parGet0i("RESOLUTION", &resolution, status);
  switch(resolution) {
    case 0:
      dSigma = SMF__FTS2_LOWRES_SSI;
      break;
    case 1:
      dSigma = SMF__FTS2_MEDRES_SSI;
      break;
    default:
      dSigma = SMF__FTS2_HIGHRES_SSI;
      break;
  }

  /* BEGIN NDF */
  ndfBegin();

  /* Loop through each input file */
  for(fIndex = 1; fIndex <= nFiles; fIndex++) {
    /* Open Observation file */
    smf_open_file(gIn, fIndex, "READ", 0, &inData, status);
    if(*status != SAI__OK) {
      *status = SAI__ERROR;
      errRep(FUNC_NAME, "Unable to open the source file!", status);
      goto CLEANUP;
    }

    /* Data cube dimensions */
    nWidth  = inData->dims[0];
    nHeight = inData->dims[1];
    nFrames = inData->dims[2];
    nPixels = nWidth * nHeight;

    /* Check if the file is initialized for FTS2 processing */
    if(!(inData->fts) || !(inData->fts->zpd)) {
      *status = SAI__ERROR;
      errRep( FUNC_NAME, "The file is NOT initialized for FTS2 data reduction!", status);
      goto CLEANUP;
    }

    /* Read in ZPD 2D array */
    zpdData = inData->fts->zpd;

    /* Read in the Nyquist frequency from FITS component */
    smf_fits_getD(inData->hdr, "FNYQUIST", &fNyquist, status);
    if(*status != SAI__OK) {
      *status = SAI__ERROR;
      errRep(FUNC_NAME, "Unable to find the Nyquist frequency in FITS component!", status);
      goto CLEANUP;
    }

    N2 = 0;
    if(zeropad) {
      N2 = ceil(fNyquist / dSigma);
    } else {
      N2 = nFrames / 2;
      dSigma = fNyquist / N2;
    }
    N = 2 * N2;

    /* Save wavenumber factor to FITS extension */
    smf_fits_updateD(inData->hdr, "WNFACT", dSigma, "Wavenumber factor cm^-1", status);

    /* Copy input data into output data */
    outData = smf_deepcopy_smfData(inData, 0, SMF__NOCREATE_DATA, 0, 0, status);
    outData->dtype   = SMF__DOUBLE;
    outData->ndims   = 3;
    outData->dims[0] = nWidth;
    outData->dims[1] = nHeight;
    outData->dims[2] = N2 + 1;
    outData->pntr[0] = (double*) astMalloc((nPixels * (N2 + 1)) * sizeof(double));
    if (dataLabel) { one_strlcpy(outData->hdr->dlabel, dataLabel, sizeof(outData->hdr->dlabel), status ); }

    IFG  = astCalloc(N,  sizeof(*IFG));
    SPEC = fftw_malloc((N2 + 1) * sizeof(*SPEC));

    for(i = 0; i < nWidth; i++) {
      for(j = 0; j < nHeight; j++) {
        bolIndex = i + j * nWidth;

        badPixel = 0;
        for(k = 0; k < nFrames; k++) {
          dIntensity = *((double*)(inData->pntr[0]) + (bolIndex + k * nPixels));
          if(dIntensity == VAL__BADD) {
            badPixel = 1;
            break;
          }
        }
        /* If this is a bad pixel, go to next */
        if(badPixel) {
          for(k = 0; k <= N2; k++) {
            *((double*)(outData->pntr[0]) + (bolIndex + nPixels * k)) = VAL__BADD;
          }
          continue;
        }

        /* Get ZPD index */
        indexZPD = *((int*)(zpdData->pntr[0]) + bolIndex);

        /* Double-Sided interferogram */
        for(k = indexZPD; k < nFrames; k++) {
          IFG[k - indexZPD] = *((double*)(inData->pntr[0]) + (bolIndex + k * nPixels));
        }
        for(k = 0; k < indexZPD; k++) {
          IFG[N - indexZPD + k] =  *((double*)(inData->pntr[0]) + (bolIndex + k * nPixels));
        }

        /* FFT butterflied interferogram */
        plan = fftw_plan_dft_r2c_1d(N, IFG, SPEC, FFTW_ESTIMATE);
        fftw_execute(plan);

        /* Write out the real component of the spectrum */
        for(k = 0; k <= N2; k++) {
          *((double*)(outData->pntr[0]) + (bolIndex + nPixels * k)) = SPEC[k][0];
        }
      }
    }

    if(IFG)  { IFG = astFree(IFG); }
    if(SPEC) { fftw_free(SPEC); SPEC = NULL; }

    /* Close the file */
    if(inData) { smf_close_file(&inData, status); }

    /* Write output */
    smf_write_smfData(outData, NULL, NULL, gOut, fIndex, 0, MSG__VERB, status);
    smf_close_file(&outData, status);
  }

  CLEANUP:
  if(IFG)  { IFG = astFree(IFG); }
  if(SPEC) { fftw_free(SPEC); SPEC = NULL; }

  /* Close files if still open */
  if(inData) { smf_close_file(&inData, status); }

  /* END NDF */
  ndfEnd(status);

  /* Delete Groups */
  grpDelet(&gIn, status);
  grpDelet(&gOut, status);
}