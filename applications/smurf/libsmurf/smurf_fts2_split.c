/*
 * +
 *  Name:
 *     FTS2SPLIT
 *
 *  Purpose:
 *     Convert multi scan NDFs for use in FTS-2 data reduction pipeline
 *
 *  Language:
 *     Starlink ANSI C
 *
 *  Type of Module:
 *     ADAM TASK
 *
 *  Invocation:
 *     smurf_fts2_split(status);
 *
 *  Arguments:
 *     status = int* (Given and Returned)
 *        Pointer to global status.
 *
 *  Description:
 *     Split out multiple unidirectional scans from a single NDF file into separate single scan NDF files
 *     The filename scan numbers are temporarily derived from the concatenation file's scan number
 *     This is intended to be changed to just append a uniqe sub-scan number.
 *
 *  ADAM Parameters:
 *     IN = NDF (Read)
 *          Input files to be transformed.
 *     OUT = NDF (Write)
 *          Output files.
 *     OUTFILES = LITERAL (Write)
 *          The name of text file to create, in which to put the names of
 *          all the output NDFs created by this application (one per
 *          line). If a NULL (!) value is supplied no file is created. [!]
 *
 *  Authors:
 *     MSHERWOOD: Matt Sherwood (UofL)
 *
 *  History :
 *     2013-05-16 (MSHERWOOD)
 *        Initial version
 *     2013-05-21 (MS)
 *        Skip scans that are too short
 *
 *  Copyright:
 *     Copyright (C) 2013 University of Lethbridge. All Rights Reserved.
 *
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
 *
 *  Bugs:
 *     {note_any_bugs_here}
 *-
 */

#if HAVE_CONFIG_H
#include <config.h>
#endif

#include <string.h>
#include <stdio.h>

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
#include "star/one.h"
#include "one_err.h"

/* SMURF includes */
#include "smurf_par.h"
#include "libsmf/smf.h"
#include "smurflib.h"
#include "libsmf/smf_err.h"
#include "sc2da/sc2store.h"
#include "libsc2fts/fts2.h"

#define FUNC_NAME "smurf_fts2_split"
#define TASK_NAME "FTS2SPLIT"

void smurf_fts2_split(int* status)
{
  if( *status != SAI__OK ) { return; }

  const double STAGE_CENTER = 225.0;    /* mm */
  Grp* gIn                  = NULL;     /* Input group */
  Grp* gOut                 = NULL;     /* Output group */
  Grp* gTmp                 = NULL;     /* Temporary group */
  smfData* inData           = NULL;     /* Pointer to input data */
  smfData* outData          = NULL;     /* Pointer to output data */
  double* outData_pntr      = NULL;     /* Pointer to output data values array */
  int nMirPos               = 0;        /* Number of frames where the mirror actually moves */
  int nStart                = 0;        /* Frame index where the mirror starts moving */
  int nStartNext            = 0;        /* Frame index where the mirror starts moving in the next scan */
  int nStop                 = 0;        /* Frame index where the mirror stops */
  int i                     = 0;        /* Counter */
  int j                     = 0;        /* Counter */
  int k                     = 0;        /* Counter */
  int n                     = 0;        /* Counter */
  double fNyquist           = 0.0;      /* Nyquist frequency */
  double dz                 = 0.0;      /* Step size in evenly spaced OPD grid */
  double* MIRPOS            = NULL;     /* Mirror positions */

  size_t nFiles             = 0;        /* Size of the input group */
  size_t nOutFiles          = 0;        /* Size of the output group */
  size_t fIndex             = 0;        /* File index */
  size_t nWidth             = 0;        /* Data cube width */
  size_t nHeight            = 0;        /* Data cube height */
  size_t nFrames            = 0;        /* Data cube depth in input file */
  size_t nFramesOut         = 0;        /* Data cube depth in output file */
  size_t nFramesOutPrev     = 0;        /* Data cube depth in previous output file */
  size_t nPixels            = 0;        /* Number of bolometers in the subarray */

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
  int indexIn               = 0;
  int indexOut              = 0;
  int badPixel              = 0;
  int k0                    = 0;
  int indexZPD              = 0;
  int done                  = 0;        /* Track completion of extracting multiple scans */
  int outDataCount          = 0;        /* The number of output data files being written */

  double lenLeft,
         lenRight,
         minLenLeft,
         minLenRight,
         minLen,
         minZPD,
         maxZPD,
         midZPD             = 0.0;      /* Mirror position half side measures */
  int midZPDPos             = 0;        /* Middle ZPD position in mirror position array */

  double EPSILON            = 0.0;
  char fileName[SMF_PATH_MAX+1];
  char scanNumStr[5+1];                 /* String form of scan number of the input file */
  int scanNum               = 0;        /* Scan number of the input file */
  int conNum                = 0;        /* Concatenation number of the input file (left shifted scanNum) */
  int scanDir               = 0;        /* Scan direction: 1 -> back to front (positive), -1 -> front to back (negative) */
  JCMTState *allState       = NULL;     /* Temporary buffer for reduced header allState array data */


  /* Get Input, Output groups */
  kpg1Rgndf("IN", 0, 1, "", &gIn, &nFiles, status);
  kpg1Wgndf("OUT", gOut, nFiles, nFiles, "More output files expected!", &gOut, &nOutFiles, status);

  /* Eliminate the first record in the output group, since it will be replaced later */
  gTmp = grpCopy(gOut, 1, 1, 1, status);
  grpDelet(&gOut, status);
  gOut = gTmp;

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

    smf_fits_getS(inData->hdr, "OBJECT", object, sizeof(object), status);
    smf_fits_getS(inData->hdr, "SUBARRAY", subarray, sizeof(subarray), status);
    smf_fits_getS(inData->hdr, "OBSID", obsID, sizeof(obsID), status);
    smf_fits_getS(inData->hdr, "FTS_MODE", scanMode, sizeof(scanMode), status);
    smf_fits_getD(inData->hdr, "SCANVEL", &scanVel, status);
    smf_fits_getD(inData->hdr, "STEPTIME", &stepTime, status);

    /* Nyquist frequency */
    fNyquist = 10.0 / (8.0 * scanVel * stepTime);
    dz = 1.0 / (2.0 * fNyquist);
    EPSILON = scanVel * stepTime / 2;

    /* Extract the scan number from the input file to be incremented in the output files */
    one_strlcpy(scanNumStr, &(inData->file->name[strlen(inData->file->name) - 8]),
               astMIN(SMF_PATH_MAX + 1, 5), status);
    if (*status == ONE__TRUNC) {
        errRep(FUNC_NAME, "Error extracting scanNumStr!", status);
        errAnnul(status);
    }

    /* Create a temporary base file name from input file name - DEBUG */
    one_strlcpy(fileName, inData->file->name,
                astMIN(SMF_PATH_MAX + 1, strlen(inData->file->name) - 7), status);
    if (*status == ONE__TRUNC) {
        errRep(FUNC_NAME, "Error extracting base fileName!", status);
        errAnnul(status);
    }
    scanNum = (int) one_strtod(scanNumStr, status);
    if (*status != SAI__OK) {
        errRep(FUNC_NAME, "Error extracting scanNum!", status);
        errAnnul(status);
    }

    /* Left shift scanNum to conNum as a prefix to make output scan number unique */
    if(scanNum < 100) {
      conNum = scanNum * 100;
    } else if(scanNum < 1000) {
      conNum = scanNum * 10;
    }

    /*printf("%s: Processing file: %s, having basename: %s and scanNumStr: %s, scanNum: %04d\n",
           TASK_NAME, inData->file->name, fileName, scanNumStr, scanNum);*/

    /* Data cube dimensions */
    nWidth  = inData->dims[0];
    nHeight = inData->dims[1];
    nFrames = inData->dims[2];
    nPixels = nWidth * nHeight;

    /* Mirror positions in mm */
    nTmp = nFrames;
    MIRPOS = astCalloc(nFrames, sizeof(*MIRPOS));
    fts2_getmirrorpositions(inData, MIRPOS, &nTmp, status); // (mm)
    if(*status != SAI__OK) {
      *status = SAI__ERROR;
      errRep( FUNC_NAME, "Unable to get the mirror positions!", status);
      goto CLEANUP;
    }

    nStart = -1;
    nStop = -1;
    nStartNext = 0;
    outDataCount = 0;
    done = 0;
    do {
        /* Find the next range of single scan mirror positions for which to extract corresponding NDF data */
        for(n=nStartNext; n<nFrames-1; n++){
            if(nStart < 0 && fabs(MIRPOS[n+1] - MIRPOS[n]) >= EPSILON) {
                nStart = n;
            }
            if(nStart >= 0 && nStop < 0 && (fabs(MIRPOS[n+1] - MIRPOS[n]) < EPSILON || n+1 == nFrames-1) ) {
                nStop = n+1;
                nFramesOutPrev = nFramesOut;
                nFramesOut = nStop - nStart + 1;
                outDataCount++;
                /*printf("%s: Split: %d of %d frames found at nStart=%d, nStop=%d\n",
                       TASK_NAME, outDataCount, nFramesOut, nStart, nStop);*/
                break;
            }
        }

        /* Determine scan direction */
        if(MIRPOS[nStart] < MIRPOS[nStop]) {
            scanDir = 1;    /* Positive */
        } else {
            scanDir = -1;   /* Negative */
        }

        /* Check for end of data condition */
        if(nStop < nStart  || nStop >= nFrames-1) {
            nStop = nFrames-1;
            done = 1;
        }

        /* Output scan if there is a start and stop position found,
           and for the last scan if it's the only one
           and if it's not too short (compared to the previous one) */
        if(nStart >=0 && nStop > 0 &&
            (!done || (done && nFramesOutPrev == 0) ||
              (nFramesOutPrev > 0 && nFramesOut > 0 && nFramesOut/nFramesOutPrev >= 0.5))) {
            /* Copy single scan NDF data from input to output */
            outData = smf_deepcopy_smfData(inData, 0, SMF__NOCREATE_DATA | SMF__NOCREATE_FTS, 0, 0, status);
            outData->dtype   = SMF__DOUBLE;
            outData->ndims   = 3;
            outData->dims[0] = nWidth;
            outData->dims[1] = nHeight;
            outData->dims[2] = nFramesOut;
            outData_pntr = (double*) astMalloc((nPixels * nFramesOut) * sizeof(*outData_pntr));
            outData->pntr[0] = outData_pntr;
            outData->hdr->nframes = nFramesOut;

            for(i=0; i<nWidth; i++) {
                for(j=0; j<nHeight; j++) {
                    bolIndex = i + j * nWidth;
                    for(k=nStart; k<=nStop; k++) {
                        indexIn = bolIndex + k * nPixels;
                        indexOut = bolIndex + (k-nStart) * nPixels;
                        *((double*)(outData->pntr[0]) + indexOut) = *((double*)(inData->pntr[0]) + indexIn);
                    }
                }
            }

            /* Update the FITS headers */
            outData->fts = smf_create_smfFts(status);
            /* Update FITS component */
            smf_fits_updateD(outData->hdr, "FNYQUIST", fNyquist, "Nyquist frequency (cm^-1)", status);
            smf_fits_updateI(outData->hdr, "MIRSTART", 1, "Frame index in which the mirror starts moving", status);
            smf_fits_updateI(outData->hdr, "MIRSTOP", nFramesOut, "Frame index in which the mirror stops moving", status);
            smf_fits_updateI(outData->hdr, "SCANDIR", scanDir, "Scan direction", status);
            smf_fits_updateD(outData->hdr, "OPDMIN", 0.0, "Minimum OPD", status);
            smf_fits_updateD(outData->hdr, "OPDSTEP", 0.0, "OPD step size", status);

            /* Update the JCMTSTATE header */
            /* Reallocate outData header array memory to reduced size */
            allState = (JCMTState*) astRealloc(outData->hdr->allState, nFramesOut * sizeof(*(outData->hdr->allState)));
            if(*status == SAI__OK && allState) {
                outData->hdr->allState = allState;
            } else {
                errRepf(TASK_NAME, "Error reallocating allState JCMTState header", status);
                goto CLEANUP;
            }
            for(k=nStart; k<=nStop; k++) {
                /* Copy over JCMTstate */
                /*printf("%s: memcpy allState: %d to: %p from: %p size: %d\n",TASK_NAME, k,
                       (void *) &(outData->hdr->allState[k-nStart]), (void *) &(inData->hdr->allState[k]), sizeof(*(outData->hdr->allState)) );*/
                memcpy( (void *) &(outData->hdr->allState[k-nStart]), (void *) &(inData->hdr->allState[k]), sizeof(*(outData->hdr->allState)) );

                /*printf("%s: Scan: %d index: %d rts_num: %d\n", TASK_NAME, outDataCount, k-nStart, outData->hdr->allState[k-nStart].rts_num);*/
                /*printf("%s: Scan: %d index: %d fts_pos: %f\n", TASK_NAME, outDataCount, k-nStart, outData->hdr->allState[k-nStart].fts_pos);*/
            }

            /* Write output */
            /* Append unique suffix to fileName */
            /* This must be modified by the concatenation file scan number to improve uniqueness */
            n = one_snprintf(outData->file->name, SMF_PATH_MAX, "%s%04d_scn.sdf", status, fileName, conNum+outDataCount);
            if(n < 0 || n >= SMF_PATH_MAX) {
                errRepf(TASK_NAME, "Error creating outData->file->name", status);
                goto CLEANUP;
            }
            /* Update the list of output _scn file names */
            grpPut1(gOut, outData->file->name, 0, status);
            if(*status != SAI__OK) {
                errRepf(TASK_NAME, "Error saving outData file name", status);
                goto CLEANUP;
            }
            smf_write_smfData(outData, NULL, outData->file->name, gOut, fIndex, 0, MSG__VERB, 0, status);
            if(*status != SAI__OK) {
                errRepf(TASK_NAME, "Error writing outData file", status);
                goto CLEANUP;
            }
            smf_close_file(&outData, status);
            if(*status != SAI__OK) {
                errRepf(TASK_NAME, "Error closing outData file", status);
                goto CLEANUP;
            }
            if(*status != SAI__OK) {
                errRepf(TASK_NAME, "Error closing outData file", status);
                goto CLEANUP;
            }
        }

        /* Prepare for next iteration */
        nStartNext = nStop + 1;
        nStart = -1;
        nStop = -1;

    } while (!done);


    /* Deallocate memory used by arrays */
    if(MIRPOS)  { MIRPOS    = astFree(MIRPOS); }

    /* Close the file */
    smf_close_file(&inData, status);

  }
  CLEANUP:
  /* Deallocate memory used by arrays */
  if(inData)  { smf_close_file(&inData, status); }
  if(outData) { smf_close_file(&outData, status); }

  /* END NDF */
  ndfEnd(status);

  /* Write out the list of output NDF names, annulling the error if a null
     parameter value is supplied. */
  if( *status == SAI__OK ) {
      grpList( "OUTFILES", 0, 0, NULL, gOut, status );
          if( *status == PAR__NULL ) {
              errRep(FUNC_NAME, "Error writing OUTFILES!", status);
              errAnnul( status );
          }
  }

  /* Delete groups */
  if(gIn)     { grpDelet(&gIn, status);  }
  if(gOut)    { grpDelet(&gOut, status); }
}
