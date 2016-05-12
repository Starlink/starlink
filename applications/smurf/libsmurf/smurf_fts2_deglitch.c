/*
*+
*  Name:
*     FTS2DEGLITCH

*  Purpose:
*     Removes the glitches from the source.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     ADAM TASK

*  Invocation:
*     smurf_fts2_deglitch(status);

*  Arguments:
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Removes the glitches from the source.

*  ADAM Parameters:
*     CCSIZE = _INTEGER (Read)
*          Core cluster size.
*     DEGLITCHMODE = _INTEGER (Read)
*          Deglitching mode, 1=CORE, 2=TAIL and any other value means ALL
*     DSHALFLENGTH = _INTEGER (Read)
*          Double-Sided interferogram half length.
*     IN = NDF (Read)
*          Input files to be transformed.
*     OUT = NDF (Write)
*          Output files.
*     TCSIGMA = _DOUBLE (Read)
*          Tail cluster standard deviation percentage.
*     TCSIGMAMUL = _DOUBLE (Read)
*          Tail cluster standard deviation multiplier.
*     TCSIZE = _INTEGER (Read)
*          Tail cluster size.

*  Authors:
*     COBA: Coskun Oba (UoL)

*  History :
*     2010-08-28 (COBA):
*        Original version.
*     2010-09-21 (COBA):
*        Updated prologue with ADAM params
*     2010-10-04 (COBA):
*        - Replaced single ZPD value input with 2D ZPD array input
*        - Added ADAM param to control deglitching mode
*     2010-11-26 (COBA):
*        - Read ZPD index from smfFts->zpd insted of ZPD calibration file
*     2011-05-10 (COBA):
*        - Deglitching of core and tails done within the function
*        - Look for glitches in core and tail sections of the interferogram
*     2011-10-18 (COBA):
*        - Update sorting

*  Copyright:
*     Copyright (C) 2010 Science and Technology Facilities Council.
*     Copyright (C) 2010 University of Lethbridge. All Rights Reserved.

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

/* STANDARD INCLUDES */
#include <math.h>
#include <stdio.h>
#include <stdlib.h>

/* STARLINK INCLUDES */
#include "ast.h"
#include "ndf.h"
#include "mers.h"
#include "par.h"
#include "sae_par.h"
#include "star/ndg.h"
#include "star/grp.h"

/* SMURF INCLUDES */
#include "smurflib.h"
#include "libsmf/smf.h"
#include "libsc2fts/fts2.h"

#define FUNC_NAME "smurf_fts2_deglitch"
#define TASK_NAME "FTS2DEGLITCH"


void smurf_fts2_deglitch(int* status)
{
  if(*status != SAI__OK) { return; }

  // ADAM VARIABLES
  int ccSize              = 0;    // Core cluster size
  int tcSize              = 0;    // Tail cluster size
  int dsLength2           = 0;    // Double-sided interferogram half-length
  int mode                = 0;    // Deglitching mode
  double tcSigma          = 0.0;  // Tail cutoff standard deviation percentage
  double tcSigmaMul       = 0.0;  // Tail cutoff standard deviation multiplier

  // INTERNAL VARIABLES
  Grp* grpInput           = NULL; // Input group
  Grp* grpOutput          = NULL; // output group
  int i                   = 0;    // Loop counter
  int j                   = 0;    // Loop counter
  int k                   = 0;    // Loop counter
  int ii                  = 0;    // Loop counter
  int jj                  = 0;    // Loop counter
  int index               = 0;    // Index
  int zpdIndex            = 0;    // ZPD Index
  int srcH                = 0;    // Height of the subarray
  int srcW                = 0;    // Width of the subarray
  int srcN                = 0;    // Time series length of the input data
  int bolIndex            = 0;    // Current bolometer index
  int numBol              = 0;    // Number of bolometers in the subarray
  int numCluster          = 0;    // Number of clusters
  int START               = 0;    // Start index
  int STOP                = 0;    // End index
  double MEAN             = 0.0;  // Mean
  double min              = 0.0;  // Minimum
  double max              = 0.0;  // Maximum
  double* IFG             = NULL; // Single bolometer interferogram
  double* SDEVF           = NULL; // Array of forward sdev
  double* SDEVR           = NULL; // Array of reverse sdev
  double* cMEAN           = NULL; // Array of cluster mean
  double* cSDEV           = NULL; // Array of cluster sdev
  size_t fileIndex        = 0;    // File loop counter
  size_t numInputFile     = 0;    // Size of the input group
  size_t numOutputFile    = 0;    // Size of the output group
  smfData* inputData      = NULL; // Pointer to input data
  smfData* zpdData        = NULL; // Pointer to ZPD data

  // GROUPS
  kpg1Rgndf("IN", 0, 1, "", &grpInput, &numInputFile, status);
  kpg1Wgndf("OUT", grpOutput, numInputFile, numInputFile,
            "Equal number of input and output files expected!",
            &grpOutput, &numOutputFile, status);

  // GET PARAMS
  parGet0i("CCSIZE", &ccSize, status);
  parGet0i("TCSIZE", &tcSize, status);
  parGet0d("TCSIGMA", &tcSigma, status);
  parGet0d("TCSIGMAMUL", &tcSigmaMul, status);
  parGet0i("DSHALFLENGTH", &dsLength2, status);
  parGet0i("DEGLITCHMODE", &mode, status);

  // BEGIN NDF
  ndfBegin();

  // ===========================================================================
  // LOOP THROUGH EACH NDF FILE IN THE INPUT GROUP
  // ===========================================================================
  for(fileIndex = 1; fileIndex <= numInputFile; fileIndex++) {
    smf_open_and_flatfield(NULL, grpInput, grpOutput, fileIndex, NULL, NULL, NULL, &inputData, status);
    if(*status != SAI__OK) {
      *status = SAI__ERROR;
      errRep(FUNC_NAME, "Unable to open source file!", status);
      goto CLEANUP;
    }

    // CHECK IF INPUT FILE IS INITIALIZED
    if(inputData->fts && inputData->fts->zpd) {
      zpdData = inputData->fts->zpd;
    } else {
      *status = SAI__ERROR;
      errRep(FUNC_NAME, "Input file is NOT initialized for FTS2 data reduction!", status);
      goto CLEANUP;
    }

    // INPUT FILE DIMENSIONS
    srcW = inputData->dims[0];
    srcH = inputData->dims[1];
    srcN = inputData->dims[2];
    numBol = srcW * srcH;

    // LOOP THROUGH THE SUBARRAY
    IFG = astMalloc(srcN * sizeof(*IFG));
    for(i = 0; i < srcH; i++) {
      for(j = 0; j < srcW; j++) {
        bolIndex = i + j * srcH;
        zpdIndex = *((int*)zpdData->pntr[0] + bolIndex);

        // GET INTERFEROGRAM
        for(k = 0; k < srcN; k++) {
          index = bolIndex + numBol * k;
          IFG[k] = *((double*)inputData->pntr[0] + index);
        }

        // =====================================================================
        // REMOVE GLITCHES FROM CORE
        // =====================================================================
        if(mode == SMF__DEGLITCH_CORE || mode == SMF__DEGLITCH_ALL) {
          int nFORWARD    = 0;
          int nREVERSE    = 0;
          int ccSize2     = 0;    // Half length of core cluster size
          double Fc       = 0.0;  // Cutoff Frequency
          double Ampc     = 0.0;  // Cutoff amplitude
          double maxSDEV  = NUM__MIND;
          START = zpdIndex - dsLength2;
          STOP  = zpdIndex + dsLength2;
          // COMPUTE DOUBLE-SIDED INTERFEROGRAM MEAN
          MEAN = 0.0;
          for(ii = START; ii <= STOP; ii++) {
            MEAN += IFG[ii];
          }
          MEAN /= (STOP - START + 1);
          // COMPUTE CUTOFF AMPLITUDE FROM THE CLUSTER AT ZPD
          min = NUM__MAXD;
          max = NUM__MIND;
          ccSize2 = ccSize >> 1;
          START = zpdIndex - ccSize2;
          STOP 	= zpdIndex + ccSize2;
          for(ii = START; ii <= STOP; ii++) {
            if(IFG[ii] > max) { max = IFG[ii]; }
            if(IFG[ii] < min) { min = IFG[ii]; }
          }
          Ampc = 0.75 * (max - min);
          // COMPUTE STANDARD DEVIATION FOR EACH CLUSTER
          numCluster  = dsLength2 / ccSize + 1;
          SDEVF = astMalloc(numCluster * sizeof(*SDEVF));
          SDEVR = astMalloc(numCluster * sizeof(*SDEVR));
          maxSDEV = NUM__MIND;
          for(ii = 0; ii < numCluster; ii++) {
            double t = 0.0;
            double tFORWARD = 0.0;
            double tREVERSE	= 0.0;
            START	= (ii - 1) * ccSize;
            STOP	= ii * ccSize;
            for(jj = START; jj < STOP; jj++) {
              t = (IFG[zpdIndex + jj] - MEAN);
              tFORWARD += (t * t);

              t = (IFG[zpdIndex - jj] - MEAN);
              tREVERSE += (t * t);
            }
            SDEVF[ii] = sqrt(tFORWARD / ccSize);
            SDEVR[ii] = sqrt(tREVERSE / ccSize);
            maxSDEV = SDEVF[ii];

            if(SDEVF[ii] > maxSDEV) {
              maxSDEV = SDEVF[ii];
            }
          }
          // CORE LENGTH - FORWARD
          nFORWARD = (numCluster - 1) * ccSize;
          for(ii = 2; ii < numCluster; ii++) {
            if(SDEVF[ii] < 0.1 * maxSDEV) {
              nFORWARD = (ii - 1) * ccSize;
              break;
            }
          }
          // CORE LENGTH - REVERSE
          nREVERSE = (numCluster - 1) * ccSize;
          for(ii = 2; ii < numCluster; ii++) {
            if(SDEVR[ii] < 0.1 * maxSDEV) {
              nREVERSE = (ii - 1) * ccSize;
              break;
            }
          }
          // COMPUTE THE CUTOFF FREQUENCY
          Fc = (nFORWARD < nREVERSE) ? (10.0 / nFORWARD) : (10.0 / nREVERSE);
          START = zpdIndex - dsLength2;
          STOP 	= zpdIndex + dsLength2;
          for(ii = START; ii <= STOP; ii++) {
            double t = abs((ii - zpdIndex) * Fc);
            double y = (t < AST__DPI / 2.0) ? ((t > SMF__DEGLITCH_THRESHOLD) ?
                                                    Ampc * sin(t) / t :
                                                    Ampc) :
                                              (Ampc / t);
            double ERROR = IFG[ii] - MEAN;
            if(abs(ERROR) > 2.0 * y) {
              if(ERROR > 0) IFG[ii] = MEAN + y;
              if(ERROR < 0) IFG[ii] = MEAN - y;
            }
          }
          if(SDEVF) { astFree(SDEVF); SDEVF = NULL; }
          if(SDEVR) { astFree(SDEVR); SDEVR = NULL; }
        }

        // =====================================================================
        // REMOVE GLITCHES FROM TAILS
        // =====================================================================
        if(mode == SMF__DEGLITCH_TAIL || mode == SMF__DEGLITCH_ALL) {
          int TAILSTART = 0;
          double avgSDEV = 0.0;
          double cutoffSigma = 0.0;
          //
          // CLEANUP LEFT TAIL
          //======================================================================
          TAILSTART = 0;
          numCluster = (zpdIndex - dsLength2) / tcSize;
          cMEAN = astMalloc(numCluster * sizeof(*cMEAN));
          cSDEV = astMalloc(numCluster * sizeof(*cSDEV));
          for(ii = 0; ii < numCluster; ii++) {
            START = TAILSTART + ii * tcSize;
            STOP  = START + tcSize;
            // CLUSTER MEAN
            cMEAN[ii] = 0.0;
            for(jj = START; jj < STOP; jj++) {
            	cMEAN[ii] += IFG[jj];
            }
            cMEAN[ii] /= tcSize;
            // CLUSTER STANDARD DEVIATION, SIGMA
            cSDEV[ii] = 0.0;
            for(jj = START; jj < STOP; jj++) {
              double t = (IFG[jj] - cMEAN[ii]);
              cSDEV[ii] += (t * t);
            }
            cSDEV[ii] = sqrt(cSDEV[ii] / tcSize);
          }
          // SORT CLUSTER STANDARD DEVIATIONS IN ASCENDING ORDER
          fts2_arrayquicksort(cSDEV, numCluster, 0, numCluster - 1, 1, status);
          if(*status != SAI__OK) {
            *status = SAI__ERROR;
            errRep(FUNC_NAME, "Unable to sort cluster!", status);
            goto CLEANUP;
          }
          avgSDEV = 0.0;
          START = 0;
          STOP  = numCluster * tcSigma;
          for(ii = START; ii < STOP; ii++) {
            avgSDEV += cSDEV[ii];
          }
          avgSDEV /= (STOP - START);
          cutoffSigma = avgSDEV * tcSigmaMul;
          MEAN = 0.0;
          for(ii = 0; ii < numCluster; ii++) {
            MEAN = cMEAN[ii];
            START = TAILSTART + ii * tcSize;
            STOP = START + tcSize;
            for(jj = START; jj < STOP; jj++) {
              if(IFG[jj] > (MEAN + cutoffSigma)) {
                IFG[jj] = MEAN + cutoffSigma;
              }
              if(IFG[jj] < (MEAN - cutoffSigma)) {
                IFG[jj] = MEAN - cutoffSigma;
              }
            }
          }
          START = TAILSTART + numCluster * tcSize;
          STOP = zpdIndex - dsLength2;
          if(START < STOP) {
            MEAN = 0.0;
            for(jj = START; jj < STOP; jj++) {
              MEAN += IFG[jj];
            }
            MEAN /= (STOP - START);
            for(jj= START; jj < STOP; jj++) {
              if(IFG[jj] > (MEAN + cutoffSigma)) {
                IFG[jj] = MEAN + cutoffSigma;
              }
              if(IFG[jj] < (MEAN - cutoffSigma)) {
                IFG[jj] = MEAN - cutoffSigma;
              }
            }
          }
          if(cMEAN) { astFree(cMEAN); cMEAN = NULL;}
          if(cSDEV) { astFree(cSDEV); cSDEV = NULL;}

          //
          // CLEANUP RIGHT TAIL
          // =====================================================================
          TAILSTART = zpdIndex + dsLength2;
          numCluster = (srcN - TAILSTART) / tcSize;
          cMEAN = astMalloc(numCluster * sizeof(*cMEAN));
          cSDEV = astMalloc(numCluster * sizeof(*cSDEV));
          for(ii = 0; ii < numCluster; ii++) {
            START = TAILSTART + ii * tcSize;
            STOP  = START + tcSize;
            // CLUSTER MEAN
            cMEAN[ii] = 0.0;
            for(jj = START; jj < STOP; jj++) {
            	cMEAN[ii] += IFG[jj];
            }
            cMEAN[ii] /= tcSize;
            // CLUSTER STANDARD DEVIATION, SIGMA
            cSDEV[ii] = 0.0;
            for(jj = START; jj < STOP; jj++) {
              double t = (IFG[jj] - cMEAN[ii]);
              cSDEV[ii] += (t * t);
            }
            cSDEV[ii] = sqrt(cSDEV[ii] / tcSize);
          }
          // SORT CLUSTER STANDARD DEVIATIONS IN ASCENDING ORDER
          fts2_arrayquicksort(cSDEV, numCluster, 0, numCluster - 1, 1, status);
          if(*status != SAI__OK) {
            *status = SAI__ERROR;
            errRep(FUNC_NAME, "Unable to sort cluster!", status);
            goto CLEANUP;
          }
          avgSDEV = 0.0;
          START = 0;
          STOP  = numCluster * tcSigma;
          for(ii = START; ii < STOP; ii++) {
            avgSDEV += cSDEV[ii];
          }
          avgSDEV /= (STOP - START);
          cutoffSigma = avgSDEV * tcSigmaMul;
          MEAN = 0.0;
          for(ii = 0; ii < numCluster; ii++) {
            MEAN = cMEAN[ii];
            START = TAILSTART + ii * tcSize;
            STOP = START + tcSize;
            for(jj = START; jj < STOP; jj++) {
              if(IFG[jj] > (MEAN + cutoffSigma)) {
                IFG[jj] = MEAN + cutoffSigma;
              }
              if(IFG[jj] < (MEAN - cutoffSigma)) {
                IFG[jj] = MEAN - cutoffSigma;
              }
            }
          }
          START = TAILSTART + numCluster * tcSize;
          STOP = srcN;
          if(START < STOP) {
            MEAN = 0.0;
            for(jj = START; jj < STOP; jj++) {
              MEAN += IFG[jj];
            }
            MEAN /= (STOP - START);
            for(jj= START; jj < STOP; jj++) {
              if(IFG[jj] > (MEAN + cutoffSigma)) {
                IFG[jj] = MEAN + cutoffSigma;
              }
              if(IFG[jj] < (MEAN - cutoffSigma)) {
                IFG[jj] = MEAN - cutoffSigma;
              }
            }
          }
          if(cMEAN) { astFree(cMEAN); cMEAN = NULL;}
          if(cSDEV) { astFree(cSDEV); cSDEV = NULL;}
        }

        // UPDATE INTERFEROGRAM
        for(k = 0; k < srcN; k++) {
          index = bolIndex + numBol * k;
          *((double*)inputData->pntr[0] + index) = IFG[k];
        }
      }
    } // END LOOP THROUGH BOLOMETERS

    if(IFG) { astFree(IFG); IFG = NULL; }
    if(inputData) { smf_close_file( NULL,&inputData, status); }
  } // END FILE LOOP

  CLEANUP:
    // DELETE ARRAYS
    if(IFG)   { astFree(IFG);   IFG = NULL; }
    if(SDEVF) { astFree(SDEVF); SDEVF = NULL; }
    if(SDEVR) { astFree(SDEVR); SDEVR = NULL; }
    if(cMEAN) { astFree(cMEAN); cMEAN = NULL;}
    if(cSDEV) { astFree(cSDEV); cSDEV = NULL;}

    // CLOSE FILES
    if(inputData) { smf_close_file( NULL,&inputData, status); }

    // END NDF
    ndfEnd(status);

    // DELETE GROUPS
    grpDelet(&grpInput, status);
    grpDelet(&grpOutput, status);
}
