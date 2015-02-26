/*
*+
*  Name:
*     FTS2INIT

*  Purpose:
*     Prepares the input to be processed by the FTS2 Data Reduction tasks

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     ADAM TASK

*  Invocation:
*     smurf_fts2_init(status);

*  Arguments:
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Prepares the input to be processed by the FTS2 Data Reduction tasks

*  ADAM Parameters:
*     CENTRE = REAL (Read)
*          Moving mirror position at centre of travel (mm).
*     IN = NDF (Read)
*          Input files to be transformed.
*     OUT = NDF (Write)
*          Output files.
*     ZPD = NDF (Read)
*          ZPD calibration file.
*     FNYQUIST = DOUBLE (Read)
*          Override the default Nyquist frequency
*          (default scalculated to be 50.0 at SCANVEL of 4.2 mm/sec) (Optional)

*  Notes:
*     - The CENTRE parameter should be the same value as was used when
*     producing the supplied ZPD file in order to prevent the introduction
*     of an unwanted offset.

*  Authors:
*     COBA: Coskun Oba (UoL)
*     MSHERWOOD: Matt Sherwood (UofL)

*  History :
*     23-NOV-2010 (COBA):
*        Original version.
*     2011-05-05 (COBA):
*        - Get mirror positions via fts2_getmirrorpositions
*        - Fixed possible memory leaks
*        - Removed redundancies
*     2012-12-12 (MSHERWOOD):
*        Put back adjustment for non-centered mirror starting position.
*     2012-12-21 (MSHERWOOD)
*        Removed unneccessary sort (fts2_validatemirrorpositions now reverses list when needed)
*     2013-04-04 (MSHERWOOD)
*        - Document the previous conversion of ZPD values from mechanical to optical units
*     2013-04-12 (MSHERWOOD)
*        Adjust OPD for each bolometer's measured ZPD position
*        Ensure that interpolation onto evenly spaced OPD grid stays within bounds
*     2013-06-05 (MS)
*        Adjust debug output
*     2013-05-19 (MS)
*        Set the step time to a nominal value to produce uniformly sized zero-padded spectra
*     2013-08-27 (MS)
*        Temporary fix for segfault due to incorrect array sizes
*     2013-11-08 (MS)
*        Add RTS delay interpolation
*        Due to a delay discovered between the real time signal (RTS) timing pulses
*        and the corresponding mirror positions of ~6.3 ms,
*        interpolation of raw interferograms has been added.
*        An optional parameter: RTSDELAY accepts a double value representing time in ms.
*     2015-02-20 (MS):
*        Added new smfFts fields for quality statistics

*  Copyright:
*     Copyright (C) 2008 Science and Technology Facilities Council.
*     Copyright (C) 2008 University of Lethbridge. All Rights Reserved.

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

#include <string.h>
#include <stdio.h>
#include <gsl/gsl_spline.h>

/* STARLINK includes */
#include "ast.h"
#include "star/ndg.h"
#include "star/grp.h"
#include "ndf.h"
#include "mers.h"
#include "prm_par.h"
#include "sae_par.h"
#include "msg_par.h"
#include "par_err.h"
#include "par.h"

/* SMURF includes */
#include "smurf_par.h"
#include "libsmf/smf.h"
#include "smurflib.h"
#include "libsmf/smf_err.h"
#include "sc2da/sc2store.h"
#include "libsc2fts/fts2.h"

#define FUNC_NAME "smurf_fts2_init"
#define TASK_NAME "FTS2INIT"

void smurf_fts2_init(int* status)
{
  if( *status != SAI__OK ) { return; }

  double STAGE_CENTER       = 0.0;      /* mm */
  double deltaT             = 0.0;      /* sec */
  Grp* gIn                  = NULL;     /* Input group */
  Grp* gOut                 = NULL;     /* Output group */
  Grp* gZpd                 = NULL;     /* ZPD group */
  smfData* inData           = NULL;     /* Pointer to input data */
  smfData* outData          = NULL;     /* Pointer to output data */
  smfData* zpdData          = NULL;     /* Pointer to ZPD data */
  smfData* zpd              = NULL;     /* Pointer to ZPD index data */
  smfData* fpm              = NULL;     /* Pointer polynomial fit parameters */
  smfData* sigma            = NULL;
  smfData* dead             = NULL;     /* Dead pixel flag m x n array */
  smfData* a                = NULL;     /* Pointer to a band (1/f low frequency) integrated powers */
  smfData* b                = NULL;     /* Pointer to b band (in band signal) integrated powers */
  smfData* c                = NULL;     /* Pointer to c band (noise) integrated powers */
  smfData* d                = NULL;     /* Pointer to d band (first harmonic) integrated powers */
  smfData* phaseFit         = NULL;     /* Pointer to Phase X^2 goodness of fit measures */
  smfData* cosmicRays       = NULL;     /* Pointer to numbers of cosmic rays occuring */
  smfData* fluxJumps        = NULL;     /* Pointer to numbers of flux jumps occuring */
  int nMirPos               = 0;        /* Number of frames where the mirror actually moves */
  int nStart                = 0;        /* Frame index where the mirror starts moving */
  int nStop                 = 0;        /* Frame index where the mirror stops */
  int i                     = 0;        /* Counter */
  int j                     = 0;        /* Counter */
  int k                     = 0;        /* Counter */
  double fNyquist           = 0.0;      /* Nyquist frequency */
  double fNyquistOverride   = 0.0;      /* Nyquist frequency command line override */
  double dz                 = 0.0;      /* Step size in evenly spaced OPD grid */
  double dx                 = 0.0;      /* Step size in evenly spaced mirror position grid */
  double* MIRPOS            = NULL;     /* Mirror positions */
  double* MIRRTS            = NULL;     /* Mirror times */
  double* IFG               = NULL;     /* Interferogram */
  double* IFGT              = NULL;     /* Interferogram time shifted*/
  double* OPD               = NULL;     /* Optical Path Difference */
  double* OPD_EVEN          = NULL;     /* Optical Path Difference, evenly spaced */
  gsl_interp_accel* ACC     = NULL;     /* OPD IFG interpolator */
  gsl_interp_accel* ACCT    = NULL;     /* Time shifted IFG interpolator */
  gsl_spline* SPLINE        = NULL;     /* OPD IFG interpolation spline */
  gsl_spline* SPLINET       = NULL;     /* Time shifted IFG interpolation spline */

  size_t nFiles             = 0;        /* Size of the input group */
  size_t nOutFiles          = 0;        /* Size of the output group */
  size_t nZPDFiles          = 0;        /* Size of the ZPD group */
  size_t fIndex             = 0;        /* File index */
  size_t nWidth             = 0;        /* Data cube width */
  size_t nHeight            = 0;        /* Data cube height */
  size_t nFrames            = 0;        /* Data cube depth */
  size_t nPixels            = 0;        /* Number of bolometers in the subarray */
  smfSortInfo* SORTINFO     = NULL;

  char object[SZFITSTR];
  char subarray[SZFITSTR];
  char obsID[SZFITSTR];
  char scanMode[SZFITSTR];

  double scanVel            = 0.0;      /* Mirror speed in mm/sec */
  double stepTime           = 0.0;      /* RTS step time, average sample rate */
  double minOPD             = 0;        /* OPD minimum */
  double maxOPD             = 0;        /* OPD maximum */
  double ZPD                = 0;
  int nTmp                  = 0;
  int nMax                  = 0;
  int nOPD                  = 0;
  int bolIndex              = 0;
  int index                 = 0;
  int badPixel              = 0;
  int k0                    = 0;
  int indexZPD              = 0;

  double lenLeft,
         lenRight,
         minLenLeft,
         minLenRight,
         minLen,
         minZPD,
         maxZPD,
         midZPD             = 0.0;      /* Mirror position half side measures */
  int midZPDPos             = 0;        /* Middle ZPD position in mirror position array */

#define STEPTIME              1.0/168.0 /* Nominal step time chosen to produce a fNyquist of 50.0
                                           at the usual SCANVEL of 4.2 mmm/sec */

  /* Get Input, Output and ZPD groups */
  kpg1Rgndf("IN", 0, 1, "", &gIn, &nFiles, status);
  kpg1Wgndf("OUT", gOut, nFiles, nFiles, "Equal number of input and output files expected!", &gOut, &nOutFiles, status);
  kpg1Gtgrp("ZPD", &gZpd, &nZPDFiles, status);

  /* BEGIN NDF */
  ndfBegin();

  /* Get moving mirror centre position. */
  parGet0d("CENTRE", &STAGE_CENTER, status);
  if (*status != SAI__OK) {
    errRep(FUNC_NAME, "Could not read parameters", status);
    goto CLEANUP;
  }

  /* RTS delay */
  parGet0d("RTSDELAY", &deltaT, status);
  if (*status != SAI__OK) {
      errRep(FUNC_NAME, "Could not read parameters", status);
      goto CLEANUP;
  }

  /* fNyquist */
  parGet0d("FNYQUIST", &fNyquistOverride, status);
  if (*status != SAI__OK) {
      errRep(FUNC_NAME, "Could not read fNyqust parameter", status);
      goto CLEANUP;
  }

  /* Open the ZPD calibration file */
  smf_open_file(NULL, gZpd, 1, "READ", SMF__NOCREATE_QUALITY, &zpdData, status);
  if(*status != SAI__OK) {
    *status = SAI__ERROR;
    errRep(FUNC_NAME, "Unable to open the ZPD calibration file!", status);
    goto CLEANUP;
  }

  /* Loop through each input file */
  for(fIndex = 1; fIndex <= nFiles; fIndex++) {
    /* Open Observation file */
    smf_open_and_flatfield(NULL, gIn, gOut, fIndex, NULL, NULL, NULL, &inData, status);
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

    /* Mirror positions in mm and times in sec*/
    nTmp = nFrames;
    MIRPOS = astCalloc(nFrames, sizeof(*MIRPOS));
    MIRRTS = astCalloc(nFrames, sizeof(*MIRRTS));
    fts2_getmirrorpositions(inData, MIRPOS, MIRRTS, &nTmp, status); /* (mm) */
    if(*status != SAI__OK) {
      *status = SAI__ERROR;
      errRep( FUNC_NAME, "Unable to get the mirror positions!", status);
      goto CLEANUP;
    }
    fts2_validatemirrorpositions(MIRPOS, MIRRTS, nFrames, &nStart, &nStop, inData, status);
    if(*status != SAI__OK) {
      *status = SAI__ERROR;
      errRep( FUNC_NAME, "Unable to validate the mirror positions!", status);
      goto CLEANUP;
    }

    /* Shift mirror times by RTSDELAY (convert from ms to s, and shift to days) */
    if(deltaT) for(k = 0; k < nFrames; k++) { MIRRTS[k] -= deltaT/(1000.0*24*3600); }

    /* Transform mirror positions from measured coordinates to [-225, 225] */
    for(k = 0; k < nFrames; k++) { MIRPOS[k] -= STAGE_CENTER; }

    /* The number of mirror positions with unique values */
    nMirPos = nStop - nStart + 1;

    /* Initialize minimum left/right lengths to maximum mirror track length (mm) */
    minLenLeft = minLenRight = STAGE_CENTER * 2;

    /* Find the minimum distance between the set of ZPD values and the nearest mirror track endpoints */
    for(i = 0; i < nWidth; i++) {
        for(j = 0; j < nHeight; j++) {
            bolIndex = i + j * nWidth;
            /* ZPD position in OPD grid */
            ZPD = *((double*) (zpdData->pntr[0]) + bolIndex);
            /* Measure the length of the left and right half of the MIRPOS track position, relative to ZPD offset */
            lenLeft = fabs(MIRPOS[nStart]) + ZPD;
            /*printf("smurf_fts2_init: lenLeft = fabs(MIRPOS[nStart(%d)] (%f)) + ZPD (%f) = %f\n", nStart, fabs(MIRPOS[nStart]), ZPD, lenLeft);*/
            lenRight = fabs(MIRPOS[nStop]) - ZPD;
            /*printf("smurf_fts2_init: lenRight = fabs(MIRPOS[nStop(%d)] (%f)) - ZPD (%f) = %f\n", nStop, fabs(MIRPOS[nStop]), ZPD, lenRight);*/
            /* Remember the shortest half */
            if(minLenLeft > lenLeft) {
                minLenLeft = lenLeft;
                minZPD = ZPD;
                /*printf("smurf_fts2_init: minLenLeft = MIRPOS[nStart(%d)] (%f) + ZPD (%f) = %f\n", nStart, MIRPOS[nStart], ZPD, minLenLeft);*/
            } else  if(minLenRight > lenRight) {
                minLenRight = lenRight;
                maxZPD = ZPD;
                /*printf("smurf_fts2_init: minLenRight = MIRPOS[nStop(%d)] (%f) - ZPD (%f) = %f\n", nStop, MIRPOS[nStop], ZPD, minLenRight);*/
            }
        }
    }

    /* Set the minimum half of the mirror track length to the lesser of the left or right minimum */
    if(minLenLeft < minLenRight) {
        minLen = minLenLeft;
    } else {
        minLen = minLenRight;
    }
    /*printf("smurf_fts2_init: minLen = %f\n", minLen);*/

    /* Set the middle ZPD value to the average of the minimum and maximum ZPDs found */
    midZPD = (minZPD + maxZPD) / 2;
    /*printf("smurf_fts2_init: minZPD=%f, maxZPD=%f, midZPD=%f\n", minZPD, maxZPD, midZPD);*/

    /* Find the middle ZPD position in the mirror position array */
    for(k=nStart; k<=nStop; k++) {
        if(MIRPOS[k] <= midZPD) {
            midZPDPos = k;
        } else break;
    }
    /*printf("smurf_fts2_init: Adjusted midZPDPos=%d, MIRPOS[midZPD]=%f\n", midZPDPos, MIRPOS[midZPDPos]);*/

    /* Adjust mirror position array start and stop values to agree with midZPD +/- minLen */
    for(k=nStart; k<=midZPDPos; k++) {
        if(MIRPOS[midZPDPos] - MIRPOS[k] > minLen) {
            nStart = k;
        } else break;
    }
    for(k=nStop; k>midZPDPos; k--) {
        if(MIRPOS[k] - MIRPOS[midZPDPos] > minLen) {
            nStop = k;
        } else break;
    }

    /* Adjust the number of mirror positions remaining */
    nMirPos = nStop - nStart + 1;
    /*printf("smurf_fts2_init: Adjusted nStart=%d, nStop=%d, nMirPos=%d\n", nStart, nStop, nMirPos);*/
    /*printf("smurf_fts2_init: Adjusted MIRPOS[%d]=%f, MIRPOS[%d]=%f\n", nStart, MIRPOS[nStart], nStop, MIRPOS[nStop]);*/

    /* Corresponding OPD values */
    OPD = astCalloc(nMirPos, sizeof(*OPD));
    /* Adjust mirror position array to be centered about ZPD */
    /* Subtract maxZPD to ensure OPD_EVEN range will fit */
    for(k=nStart; k <=nStop; k++) {
        OPD[k-nStart] = 4.0 * (MIRPOS[k] - maxZPD) / 10.0;
    }
    /*printf("smurf_fts2_init: OPD[0]=%f, OPD[%d]=%f\n", OPD[0], nMirPos-1, OPD[nMirPos-1]);*/

    scanVel   = 0.0;
    stepTime  = 0.0;
    smf_fits_getS(inData->hdr, "OBJECT", object, sizeof(object), status);
    smf_fits_getS(inData->hdr, "SUBARRAY", subarray, sizeof(subarray), status);
    smf_fits_getS(inData->hdr, "OBSID", obsID, sizeof(obsID), status);
    smf_fits_getS(inData->hdr, "FTS_MODE", scanMode, sizeof(scanMode), status);
    smf_fits_getD(inData->hdr, "SCANVEL", &scanVel, status);
    smf_fits_getD(inData->hdr, "STEPTIME", &stepTime, status);

    /* Set the step time to a nominal value to produce uniformly sized zero-padded spectra */
    stepTime = STEPTIME;

    /* Nyquist frequency */
    if(fNyquistOverride > 0.0) {
        fNyquist = fNyquistOverride;
    } else {
        fNyquist = 10.0 / (8.0 * scanVel * stepTime);
    }
    dz = 1.0 / (2.0 * fNyquist);

    /* Time shifted IFG */
    if(deltaT) {
        /* Allocate memory for arrays */
        IFGT = astCalloc(nFrames, sizeof(*IFGT));
        /* Prepare GSL interpolator */
        ACCT   = gsl_interp_accel_alloc();
        SPLINET= gsl_spline_alloc(gsl_interp_cspline, nFrames);

        /* Interpolate interferograms in each pixel onto a time shifted grid */
        for(i = 0; i < nWidth; i++) {
            for(j = 0; j < nHeight; j++) {
                bolIndex = i + j * nWidth;

                badPixel = 0;
                /* Read in interferogram */
                for(k = nStart; k <= nStop; k++) {
                    index = bolIndex + nPixels * k;

                    IFGT[k] = *((double*) (inData->pntr[0]) + index);

                    /* See if this is a bad pixel */
                    if(IFGT[k] == VAL__BADD) { badPixel = 1; break; }
                }
                /* If this is a bad pixel, go to next */
                if(badPixel) {
                    for(k = 0; k < nOPD; k++) {
                        index = bolIndex + k * nPixels;
                        *((double*)(inData->pntr[0]) + index) = VAL__BADD;
                    }
                    continue;
                }

                /* Interpolate interferogram onto time shifted grid */
                gsl_spline_init(SPLINET, MIRRTS, IFGT, nFrames);

                /* Update the time shifted IFG */
                for(k = 0; k < nFrames; k++) {
                    index = bolIndex + nPixels * k;
                    *((double*)(inData->pntr[0]) + index) = gsl_spline_eval(SPLINET, MIRRTS[k], ACCT);
                }
            }
        }
    }

    /* Evenly spaced OPD grid */
    minOPD = OPD[0];
    maxOPD = OPD[nMirPos - 1];
    /* Set the maximum OPD to the smallest magnitude endpoint */
    if(fabs(minOPD) < fabs(maxOPD)) maxOPD = fabs(minOPD);
    /* Set the minumum OPD to the negative of the maximum */
    minOPD = -maxOPD;
    /* Values in half the evenly spaced grid */
    nMax = (int) (fabs(maxOPD) / dz);
    /* Reduce the number of values in the evenly spaced grid
      if the sum of their values plus any additional ZPD shift would exceed an ODP endpoint */
    /*printf("smurf_fts2_init: nMax=%d, dz=%f, nMax*dz+midZPD/2=%f, fabs(maxOPD)=%f\n", nMax, dz, nMax*dz+midZPD/2, fabs(maxOPD));*/
    /*printf("smurf_fts2_init: nMax=%d, dz=%f, nMax*dz=%f, OPD[nMirPos-1]=%f\n", nMax, dz, nMax*dz, OPD[nMirPos-1]);*/
    while((nMax * dz) > fabs(OPD[nMirPos-1])) {
        nMax--;
        /*printf("smurf_fts2_init: Reduced nMax=%d\n", nMax);*/
    }
    /* Values in the whole grid */
    nOPD = 2 * nMax;
    /*printf("%s: nOPD=%d\n", TASK_NAME, nOPD);*/

    OPD_EVEN = astCalloc(nOPD, sizeof(*OPD_EVEN));
    for(k = 1; k <= nOPD; k++) {
      OPD_EVEN[k - 1] = (k < nMax) ? -(nMax - k) : (k - nMax);
      OPD_EVEN[k - 1] *= dz;
    }
    /*printf("smurf_fts2_init: OPD_EVEN[%d]=%E\n", nOPD/2-1, OPD_EVEN[nOPD/2-1]);*/

    /* Update FITS component */
    smf_fits_updateD(inData->hdr, "STEPTIME", stepTime, "Nominal RTS step time", status);
    smf_fits_updateD(inData->hdr, "FNYQUIST", fNyquist, "Nyquist frequency (cm^-1)", status);
    smf_fits_updateI(inData->hdr, "MIRSTART", nStart, "Frame index in which the mirror starts moving", status);
    smf_fits_updateI(inData->hdr, "MIRSTOP", nStop, "Frame index in which the mirror stops moving", status);
    smf_fits_updateD(inData->hdr, "OPDMIN", OPD_EVEN[0], "Minimum OPD", status);
    smf_fits_updateD(inData->hdr, "OPDSTEP", dz, "OPD step size", status);

    /* Copy input data into output data */
    outData = smf_deepcopy_smfData( NULL, inData, 0, SMF__NOCREATE_DATA
            | SMF__NOCREATE_FTS | SMF__NOCREATE_QUALITY, 0, 0, status);
    outData->dtype   = SMF__DOUBLE;
    outData->ndims   = 3;
    outData->dims[0] = nWidth;
    outData->dims[1] = nHeight;
    outData->dims[2] = nOPD;
    outData->pntr[0] = (double*) astMalloc((nPixels * nOPD) * sizeof(double));

    /* Create a 2D ZPD index array and store it in the file */
    zpd = smf_create_smfData(SMF__NOCREATE_DA | SMF__NOCREATE_FTS, status);
    zpd->dtype   = SMF__INTEGER;
    zpd->ndims   = 2;
    zpd->dims[0] = nWidth;
    zpd->dims[1] = nHeight;
    zpd->pntr[0] = (int*) astCalloc(nPixels,  sizeof(int));
    /* By default set ZPD indices to a bad value */
    for(i = 0; i < nWidth; i++) {
      for(j = 0; j < nHeight; j++) {
        bolIndex = i + j * nWidth;
        *((int*) (zpd->pntr[0]) + bolIndex) = VAL__BADI;
      }
    }

    /* Allocate memory for arrays */
    IFG = astCalloc(nMirPos, sizeof(*IFG));

    /* Prepare GSL interpolator */
    ACC    = gsl_interp_accel_alloc();
    SPLINE = gsl_spline_alloc(gsl_interp_cspline, nMirPos);

    /* Interpolate interferograms in each pixel onto an evenly spaced grid */
    for(i = 0; i < nWidth; i++) {
      for(j = 0; j < nHeight; j++) {
        bolIndex = i + j * nWidth;

        /* ZPD position in OPD grid */
        ZPD = *((double*) (zpdData->pntr[0]) + bolIndex);
        /* Convert from mechanical mm (x 4) to optical cm (/ 10) */
        /* Adjust for bolometer's measured ZPD position */
        for(k = nStart; k <=nStop; k++) {
            OPD[k - nStart] = 4.0 * (MIRPOS[k] - ZPD) / 10.0;
          /*if(i==16 && j==25) {
                printf("smurf_fts2_init: Pixel [%d,%d]: OPD[%d]=%f for MIRPOS[%d]=%f with ZPD=%F\n", i, j, (k-nStart), OPD[k-nStart], k, MIRPOS[k], ZPD);
            }*/
        }

        badPixel = 0;
        /* Read in interferogram */
        for(k = nStart; k <= nStop; k++) {
          index = bolIndex + nPixels * k;

          k0 = k - nStart;
          IFG[k0] = *((double*) (inData->pntr[0]) + index);

          /* See if this is a bad pixel */
          if(IFG[k0] == VAL__BADD) { badPixel = 1; break; }
        }
        /* If this is a bad pixel, go to next */
        if(badPixel) {
          for(k = 0; k < nOPD; k++) {
            index = bolIndex + k * nPixels;
            *((double*)(outData->pntr[0]) + index) = VAL__BADD;
          }
          continue;
        }

        /* Interpolate time shifted interferogram onto OPD grid */
        gsl_spline_init(SPLINE, OPD, IFG, nMirPos);

        /* Update the output and OPD
           Also determine where the ZPD index is and update the 2D ZPD array */
        indexZPD = 0;
        for(k = 0; k < nOPD; k++) {
          index = bolIndex + nPixels * k;
          /* Prevent interpolation errors by not exceeding OPD bounds */
          if(OPD_EVEN[k] >= OPD[0] && OPD_EVEN[k] <= OPD[nMirPos-1]){
            *((double*)(outData->pntr[0]) + index) = gsl_spline_eval(SPLINE,  OPD_EVEN[k], ACC);
            /* Update the ZPD index position */
            if(OPD_EVEN[k] <= ZPD) { indexZPD = k;}
          }
        }
        *((int*) (zpd->pntr[0]) + bolIndex) = indexZPD;
      }
    }

    /* Deallocate memory used by arrays */
    if(IFGT)    { IFGT      = astFree(IFGT); }
    if(IFG)     { IFG       = astFree(IFG); }
    if(OPD)     { OPD       = astFree(OPD); }
    if(OPD_EVEN){ OPD_EVEN  = astFree(OPD_EVEN); }
    if(MIRPOS)  { MIRPOS    = astFree(MIRPOS); }
    if(ACCT)    { gsl_interp_accel_free(ACCT);  ACCT    = NULL; }
    if(SPLINET) { gsl_spline_free(SPLINET);     SPLINET = NULL; }
    if(ACC)     { gsl_interp_accel_free(ACC);   ACC     = NULL; }
    if(SPLINE)  { gsl_spline_free(SPLINE);      SPLINE  = NULL; }

    /* Close the file */
    smf_close_file( NULL,&inData, status);

    /* Create a 3D empty fpm array */
    fpm = smf_create_smfData(SMF__NOCREATE_DA | SMF__NOCREATE_FTS, status);
    fpm->dtype   = SMF__DOUBLE;
    fpm->ndims   = 3;
    fpm->dims[0] = 1;
    fpm->dims[1] = 1;
    fpm->dims[2] = 1;
    fpm->pntr[0] = (double*) astCalloc(1, sizeof(double));

    /* Create a 2D empty sigma array */
    sigma = smf_create_smfData(SMF__NOCREATE_DA | SMF__NOCREATE_FTS, status);
    sigma->dtype   = SMF__DOUBLE;
    sigma->ndims   = 2;
    sigma->dims[0] = 1;
    sigma->dims[1] = 1;
    sigma->pntr[0] = (double*) astCalloc(1, sizeof(double));

    /* Create a 2D empty dead pixel array */
    dead = smf_create_smfData(SMF__NOCREATE_DA | SMF__NOCREATE_FTS, status);
    dead->dtype   = SMF__INTEGER;
    dead->ndims   = 2;
    dead->dims[0] = nWidth;
    dead->dims[1] = nHeight;
    dead->pntr[0] = (int*) astCalloc(nPixels, sizeof(int));

    /* Create a 2D empty a band array */
    a = smf_create_smfData(SMF__NOCREATE_DA | SMF__NOCREATE_FTS, status);
    a->dtype   = SMF__DOUBLE;
    a->ndims   = 2;
    a->dims[0] = nWidth;
    a->dims[1] = nHeight;
    a->pntr[0] = (double*) astCalloc(nPixels, sizeof(double));

    /* Create a 2D empty b band array */
    b = smf_create_smfData(SMF__NOCREATE_DA | SMF__NOCREATE_FTS, status);
    b->dtype   = SMF__DOUBLE;
    b->ndims   = 2;
    b->dims[0] = nWidth;
    b->dims[1] = nHeight;
    b->pntr[0] = (double*) astCalloc(nPixels, sizeof(double));

    /* Create a 2D empty c band array */
    c = smf_create_smfData(SMF__NOCREATE_DA | SMF__NOCREATE_FTS, status);
    c->dtype   = SMF__DOUBLE;
    c->ndims   = 2;
    c->dims[0] = nWidth;
    c->dims[1] = nHeight;
    c->pntr[0] = (double*) astCalloc(nPixels, sizeof(double));

    /* Create a 2D empty d band array */
    d = smf_create_smfData(SMF__NOCREATE_DA | SMF__NOCREATE_FTS, status);
    d->dtype   = SMF__DOUBLE;
    d->ndims   = 2;
    d->dims[0] = nWidth;
    d->dims[1] = nHeight;
    d->pntr[0] = (double*) astCalloc(nPixels, sizeof(double));

    /* Create a 2D empty phaseFit array */
    phaseFit = smf_create_smfData(SMF__NOCREATE_DA | SMF__NOCREATE_FTS, status);
    phaseFit->dtype   = SMF__DOUBLE;
    phaseFit->ndims   = 2;
    phaseFit->dims[0] = nWidth;
    phaseFit->dims[1] = nHeight;
    phaseFit->pntr[0] = (double*) astCalloc(nPixels, sizeof(double));

    /* Create a 2D empty cosmicRays array */
    cosmicRays = smf_create_smfData(SMF__NOCREATE_DA | SMF__NOCREATE_FTS, status);
    cosmicRays->dtype   = SMF__INTEGER;
    cosmicRays->ndims   = 2;
    cosmicRays->dims[0] = nWidth;
    cosmicRays->dims[1] = nHeight;
    cosmicRays->pntr[0] = (int*) astCalloc(nPixels, sizeof(int));

    /* Create a 2D empty fluxJumps array */
    fluxJumps = smf_create_smfData(SMF__NOCREATE_DA | SMF__NOCREATE_FTS, status);
    fluxJumps->dtype   = SMF__INTEGER;
    fluxJumps->ndims   = 2;
    fluxJumps->dims[0] = nWidth;
    fluxJumps->dims[1] = nHeight;
    fluxJumps->pntr[0] = (int*) astCalloc(nPixels, sizeof(int));

    // Initialize FTS2 values to bad
    for(i=0;i<nPixels;i++) {
        *((int*) dead->pntr[0] + i) = VAL__BADI;
        *((double*) a->pntr[0] + i) = VAL__BADD;
        *((double*) b->pntr[0] + i) = VAL__BADD;
        *((double*) c->pntr[0] + i) = VAL__BADD;
        *((double*) d->pntr[0] + i) = VAL__BADD;
        *((double*) phaseFit->pntr[0] + i) = VAL__BADD;
        *((int*) cosmicRays->pntr[0] + i) = VAL__BADI;
        *((int*) fluxJumps->pntr[0] + i) = VAL__BADI;
    }

    /* Write to output */
    outData->fts = smf_construct_smfFts(NULL, zpd, fpm, sigma, dead, a, b, c, d, phaseFit, cosmicRays, fluxJumps, status);
    smf_write_smfData(NULL, outData, NULL, NULL, gOut, fIndex, 0, MSG__VERB, 0, status);
    smf_close_file( NULL,&outData, status);
  }
  CLEANUP:
  /* Deallocate memory used by arrays */
  if(IFGT)    { IFGT      = astFree(IFGT); }
  if(IFG)     { IFG       = astFree(IFG); }
  if(OPD)     { OPD       = astFree(OPD); }
  if(OPD_EVEN){ OPD_EVEN  = astFree(OPD_EVEN); }
  if(ACC)     { gsl_interp_accel_free(ACC);   ACC     = NULL; }
  if(ACCT)    { gsl_interp_accel_free(ACCT);  ACCT    = NULL; }
  if(SPLINET) { gsl_spline_free(SPLINET);     SPLINET = NULL; }
  if(SPLINE)  { gsl_spline_free(SPLINE);      SPLINE  = NULL; }
  if(inData)  { smf_close_file( NULL,&inData, status); }
  if(outData) { smf_close_file( NULL,&outData, status); }

  /* END NDF */
  ndfEnd(status);

  /* Delete groups */
  if(gIn)     { grpDelet(&gIn, status); }
  if(gOut)    { grpDelet(&gOut, status); }
  if(gZpd)    { grpDelet(&gZpd, status); }
}
