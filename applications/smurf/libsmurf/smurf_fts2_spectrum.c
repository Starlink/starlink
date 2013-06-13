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
*     MSHERWOOD: Matt Sherwood (UofL)

*  History :
*     2011-06-24 (COBA):
*        Original version.
*     2011-08-16 (COBA):
*        Add Zero-Padding.
*     2011-10-18 (COBA):
*        Add subarray check.
*     2013-03-12 (MSHERWOOD)
*        Beginning to correct zeropadding
*     2013-03-12 (MS)
*        Aligned with OPD grid
*     2013-05-21 (MS)
*        Reimplemented zero padding
*     2013-05-22 (MS)
*        Do not change nyquist frequency when zero padding
*        to avoid shifting the resulting spectrum
*     2013-05-23 (MS)
*        Reindent spectrum code
*     2013-05-23 (MS)
*        Normalize spectrum
*     2013-05-31 (MS)
*        Add debug output
*     2013-05-31 (MS)
*        Cleanup array indexing
*     2013-05-31 (MS)
*        Cleanup fftw plan
*     2013-06-05 (MS)
*        Adjust debug output
*     2013-06-05 (MS)
*        Error check file writing and closures
*     2013-06-05 (MS)
*        Correct off-by-one ZPD index error causing attenuation at half nyquist

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
    int zeropad               = 1;              /* Determines whether to zeropad */
    double resolution         = 0.0;            /* Spectral Resolution */
    double resolutionin       = 0.0;            /* Spectral Resolution input */
    double resolutionzp       = 0.0;            /* Spectral Resolution zero padded */
    int i                     = 0;              /* Counter */
    int j                     = 0;              /* Counter */
    int k                     = 0;              /* Counter */
    int l                     = 0;              /* Counter */
    double fNyquist           = 0.0;            /* Nyquist frequency */
    double fNyquistin         = 0.0;            /* Nyquist frequency input */
    double fNyquistzp         = 0.0;            /* Nyquist frequency zero padded */
    double dSigma             = 0.0;            /* Spectral Sampling Interval */
    double dSigmain           = 0.0;            /* Spectral Sampling Interval zero padded */
    double dSigmazp           = 0.0;            /* Spectral Sampling Interval zero padded */
    double* IFG               = NULL;           /* Interferogram */
    double* DS                = NULL;           /* Double Sided Interferogram */
    fftw_complex* DSIN        = NULL;           /* Double-Sided interferogram, FFT input */
    fftw_complex* SPEC        = NULL;           /* Spectrum */
    fftw_plan plan            = NULL;           /* fftw plan */
    int pland                 = 0;              /* fftw plan destroyed? */

    size_t nFiles             = 0;              /* Size of the input group */
    size_t nOutFiles          = 0;              /* Size of the output group */
    size_t fIndex             = 0;              /* File index */
    size_t nWidth             = 0;              /* Data cube width */
    size_t nHeight            = 0;              /* Data cube height */
    size_t nFrames            = 0;              /* Data cube depth */
    size_t nPixels            = 0;              /* Number of bolometers in the subarray */

    double dIntensity         = 0;
    int N                     = 0;
    int Nin                   = 0;                /* N input */
    int Nzp                   = 0;                /* N zero padded */
    int N2                    = 0;
    int N2in                  = 0;                /* N/2 input */
    int N2zp                  = 0;                /* N/2 zero padded */
    int bolIndex              = 0;
    int badPixel              = 0;
    int indexZPD              = 0;
    int indexZPDin            = 0;
    int indexZPDzp            = 0;
    int pad                   = 0;               /* zero padding (difference between input and zero padded interferogram length) */
    int pad2                  = 0;               /* zero padding / 2 */
    double dx                 = 0.0;             /* Delta x */
    double dxin               = 0.0;             /* Delta x input */
    double dxzp               = 0.0;             /* Delta x zero padded */
    double OPDMax             = 0.0;             /* OPD max in cm */
    double OPDMaxin           = 0.0;             /* OPD max in cm input */
    double OPDMaxzp           = 0.0;             /* OPD max in cm zero padded */

#define DEBUG 0

    /* Get Input & Output groups */
    kpg1Rgndf("IN", 0, 1, "", &gIn, &nFiles, status);
    kpg1Wgndf("OUT", gOut, nFiles, nFiles, "Equal number of input and output files expected!", &gOut, &nOutFiles, status);

    /* Read in ADAM parameters */
    parGet0i("ZEROPAD", &zeropad, status);

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

        /*printf("%s: nWidth=%d, nHeight=%d, nPixels=%d, nFrames=%d\n", TASK_NAME, nWidth, nHeight, nPixels, nFrames);*/

        /* Check if the file is initialized for FTS2 processing */
        if(!(inData->fts)) {
            *status = SAI__ERROR;
            errRep( FUNC_NAME, "The file is NOT initialized for FTS2 data reduction!", status);
            goto CLEANUP;
        }

        /* Read in the Nyquist frequency from FITS component */
        smf_fits_getD(inData->hdr, "FNYQUIST", &fNyquist, status);
        if(*status != SAI__OK) {
            *status = SAI__ERROR;
            errRep(FUNC_NAME, "Unable to find the Nyquist frequency in FITS component!", status);
            goto CLEANUP;
        }

        fNyquistin = fNyquistzp = 0.0;
        dx = dxin = dxzp = 0.0;
        N2 = N2in = N2zp = 0;
        indexZPD = indexZPDin = indexZPDzp = 0;
        N = Nin = Nzp = 0;
        dSigma = dSigmain = dSigmazp = 0.0;

        fNyquistin = fNyquist;
        dxin = (1/(2*fNyquistin));
        N2in = (nFrames / 2);
        indexZPDin = N2in - 1;
        Nin = 2 * N2in;
        OPDMaxin = N2in * dxin;
        resolution = 1 / (2 * OPDMaxin);
        resolutionin = resolution;
        dSigmain = fNyquistin / N2in;

        if(zeropad) {
            if(DEBUG) {
                /* Make the zero-padded array twice the size of the input */
                fNyquistzp = fNyquist;
                Nzp = N2in * 4;
                N2zp = Nzp / 2;
                dxzp = 1 / (2 * fNyquistzp);
                OPDMaxzp = N2zp * dxzp;
                dSigmazp = fNyquistzp / N2zp;
                resolutionzp = 1 / (2 * OPDMaxzp);
            } else {
                /* Round Nyquist frequency down to nearest integer, for calculation convenience
                fNyquistzp = floor(fNyquist);

                smf_fits_updateD(inData->hdr, "FNYQUIST", fNyquistzp, "Nyquist frequency (cm^-1)", status);*/

                /* Never change nyquist when zero padding */
                fNyquistzp = fNyquist;

                /* If resolution > 0.05, then round down to nearest 0.05 value, else set to 0.005 */
                /* Calculate resolution as 1 / (2*OPDMax) */
                /* Calculate OPDMax as N2 * dx */

                if(resolution > 0.05) {
                    resolutionzp = floor(resolution/0.05) * 0.05;
                } else {
                    resolutionzp = 0.005;
                }

                /* Calculate OPDMaxOut  as 1 / (2 * resolutionzp) */
                OPDMaxzp = 1 / (2 * resolutionzp);

                /* Calculate N2 */
                dxzp = (1/(2*fNyquistzp));
                N2zp = (OPDMaxzp / dxzp);
                indexZPDzp = N2zp - 1;
                Nzp = 2 * N2zp;
                dSigmazp = fNyquistzp / N2zp;
            }
        }

      /*printf("%s: Nin=%d, Nzp=%d, N2in=%d, N2zp=%d, indexZPDin=%d, indexZPDzp=%d, dSigmain=%f, dSigmazp=%f, fNyquistin=%f, fNyquistzp=%f, dxin=%f, dxzp=%f, OPDMaxin=%f, OPDMaxzp=%f, resolutionin=%f, resolutionzp=%f\n",
               TASK_NAME, Nin, Nzp, N2in, N2zp, indexZPDin, indexZPDzp, dSigmain, dSigmazp, fNyquistin, fNyquistzp, dxin, dxzp, OPDMaxin, OPDMaxzp, resolutionin, resolutionzp);*/

        if(zeropad) {
            N = Nzp;
            N2 = N2zp;
            indexZPD = indexZPDzp;
            dSigma = dSigmazp;
            fNyquist = fNyquistzp;
            dx = dxzp;
            OPDMax = OPDMaxzp;
            resolution = resolutionzp;
        } else {
            N = Nin;
            N2 = N2in;
            indexZPD = indexZPDin;
            dSigma = dSigmain;
            fNyquist = fNyquistin;
            dx = dxin;
            OPDMax = OPDMaxin;
            resolution = resolutionin;
        }

        /* Save wavenumber factor to FITS extension */
        smf_fits_updateD(inData->hdr, "WNFACT", dSigma, "Wavenumber factor cm^-1", status);

        /* TODO: Update mirror positions */
        smf_fits_updateI(inData->hdr, "MIRSTART", 0, "Frame index in which the mirror starts moving", status);
        smf_fits_updateI(inData->hdr, "MIRSTOP", N2, "Frame index in which the mirror stops moving", status);
      /*smf_fits_updateD(inData->hdr, "OPDMIN", OPD_EVEN[0], "Minimum OPD", status);
        smf_fits_updateD(inData->hdr, "OPDSTEP", dx, "OPD step size", status);*/


        /* Copy input data into output data */
        outData = smf_deepcopy_smfData(inData, 0, SMF__NOCREATE_DATA, 0, 0, status);
        outData->dtype   = SMF__DOUBLE;
        outData->ndims   = 3;
        outData->dims[0] = nWidth;
        outData->dims[1] = nHeight;
        outData->dims[2] = N2+1;
        outData->pntr[0] = (double*) astMalloc((nPixels * (N2+1)) * sizeof(double));
        if (dataLabel) { one_strlcpy(outData->hdr->dlabel, dataLabel, sizeof(outData->hdr->dlabel), status ); }

        IFG  = astCalloc(N,  sizeof(*IFG));
        DS   = astCalloc(N, sizeof(*DS));
        DSIN = fftw_malloc(N * sizeof(*DSIN));
        SPEC = fftw_malloc(N * sizeof(*SPEC));

        /* Initialize arrays */
        for(k = 0; k < N; k++) { SPEC[k][0] = SPEC[k][1] = DSIN[k][0] = DSIN[k][1] = DS[k] = IFG[k] = 0.0; }

        for(i = 0; i < nWidth; i++) {
            for(j = 0; j < nHeight; j++) {
                bolIndex = i + j * nWidth;

                badPixel = 0;
                for(k = 0; k < Nin; k++) {
                    dIntensity = *((double*)(inData->pntr[0]) + (bolIndex + k * nPixels));
                    if(dIntensity == VAL__BADD) {
                        badPixel = 1;
                        break;
                    }
                }
                /* If this is a bad pixel, go to next */
                if(badPixel) {
                    for(k = 0; k <= N2in; k++) {
                        *((double*)(outData->pntr[0]) + (bolIndex + k * nPixels)) = VAL__BADD;
                    }
                    continue;
                }

                /* Double-Sided interferogram */
                if(zeropad) {
                    pad = Nzp - Nin;
                    pad2 = pad / 2;
                    /* Copy the right half of the input into the left half of this IFG, zero padded in the middle */
                    for(k=indexZPDin; k<Nin; k++) {
                        /*printf("%s: IFG: indexZPDin=%d, indexZPDzp=%d, Nin=%d, Nzp=%d, k=%d, l=%d\n", TASK_NAME, indexZPDin, indexZPDzp, Nin, Nzp, k, l);*/
                        IFG[k - indexZPDin] = *((double*)(inData->pntr[0]) + (bolIndex + k * nPixels));
                      /*if(i==16 && j==25) {
                            printf("%s: Pixel[%d,%d]: (L<-R) IFG[k(%d)-indexZPDin(%d)=%d] = inData->pntr[bolIndex(%d)+k(%d)*nPixels(%d)=%d] = %g\n",
                                   TASK_NAME, i, j, k, indexZPDin, (k - indexZPDin), bolIndex, k, nPixels, (bolIndex + k * nPixels), IFG[k - indexZPDin]);
                        }*/
                    }
                    /* Copy the left half of the input into the right half of this IFG, zero padded in the middle */
                    for(k=0,l=0; k<indexZPDin; k++) {
                        IFG[Nzp - indexZPDin + k] =  *((double*)(inData->pntr[0]) + (bolIndex + k * nPixels));
                      /*if(i==16 && j==25) {
                            printf("%s: Pixel[%d,%d]: (L->R) IFG[Nzp(%d)-indexZPDin(%d)+k(%d)=%d] = inData->pntr[bolIndex(%d)+k(%d)*nPixels(%d)=%d] = %g\n",
                                   TASK_NAME, i, j, Nzp, indexZPDin, k, (Nzp-indexZPDin+k), bolIndex, k, nPixels, (bolIndex+k*nPixels), IFG[Nzp-indexZPDin+k]);
                        }*/
                    }
                } else {
                    /* Copy the right half of the input into the left half of this IFG */
                    for(k=indexZPD; k<N; k++) {
                        IFG[k - indexZPD] = *((double*)(inData->pntr[0]) + (bolIndex + k * nPixels));
                      /*if(i==16 && j==25) {
                            printf("%s: Pixel[%d,%d]: (L<-R) IFG[k(%d)-indexZPD(%d)=%d] = inData->pntr[bolIndex(%d)+k(%d)*nPixels(%d)=%d] = %f\n",
                                     TASK_NAME, i, j, k, indexZPD, (k - indexZPD), bolIndex, k, nPixels, (bolIndex + k * nPixels), IFG[k - indexZPD]);
                        }*/
                    }
                    /* Copy the left half of the input into the right half of this IFG */
                    for(k=0; k<indexZPD; k++) {
                        IFG[N - indexZPD + k] =  *((double*)(inData->pntr[0]) + (bolIndex + k * nPixels));
                      /*if(i==16 && j==25) {
                              printf("%s: Pixel[%d,%d]: (L->R) IFG[N(%d)-indexZPD(%d)+k(%d)=%d] = inData->pntr[bolIndex(%d)+k(%d)*nPixels(%d)=%d] = %f\n",
                                     TASK_NAME, i, j, N, indexZPD, k, (N - indexZPD + k), bolIndex, k, nPixels, (bolIndex + k * nPixels), IFG[N - indexZPD + k]);
                        }*/
                    }
                }

                /* DEBUG: Write out input data
                for(k = 0; k < Nin; k++) {
                    *((double*)(outData->pntr[0]) + (bolIndex + nPixels * k)) =
                    *((double*)( inData->pntr[0]) + (bolIndex + nPixels * k));
                    if(i==16 && j==25) {
                        printf("%s: inData[%d,%d,%d]=%g\n",TASK_NAME, i, j, k, *((double*)( inData->pntr[0]) + (bolIndex + nPixels * k)));
                    }
                } */

                /* DEBUG: Write out the shifted IFG
                for(k = 0; k < N; k++) {
                    *((double*)(outData->pntr[0]) + (bolIndex + k* nPixels)) = IFG[k];
                    if(i==16 && j==25) {
                        printf("%s: IFG[%d,%d,%d]=%g\n",TASK_NAME, i, j, k, IFG[k]);
                    }
                } */

                /* Convert real-valued interferogram to complex-valued interferogram */
                for(k = 0; k < N; k++) { DSIN[k][0] = IFG[k]; DSIN[k][1] = 0.0; }

                /* DEBUG: Write out DSIN
                for(k = 0; k < N; k++) {
                    *((double*)(outData->pntr[0]) + (bolIndex + k * nPixels)) = DSIN[k][0];
                    if(i==16 && j==25) {
                        printf("%s: DSIN[%d,%d,%d]=%g\n",TASK_NAME, i, j, k, DSIN[k][0]);
                    }
                } */

                /* FFT Double-sided complex-valued interferogram */
                plan = fftw_plan_dft_1d(N, DSIN, SPEC, FFTW_FORWARD, FFTW_ESTIMATE);
                fftw_execute(plan);

                /* Write out the positive real component of the spectrum and normalize */
                for(k = 0; k <= N2; k++) {
                    *((double*)(outData->pntr[0]) + (bolIndex + k * nPixels)) = (SPEC[k][0] / (double)(N * resolution));
                  /*if(i==16 && j==25) {
                        printf("%s: SPEC[%d,%d,%d]=%E\n",TASK_NAME, i, j, k, SPEC[k][0] / (double)(N * resolution));
                    }*/
                }
            }
        }

        if(IFG)  { IFG = astFree(IFG); }
        if(DS)   { DS = astFree(DS); }
        if(plan) { fftw_destroy_plan(plan); pland = 1; }
        if(DSIN) { fftw_free(DSIN); DSIN = NULL; }
        if(SPEC) { fftw_free(SPEC); SPEC = NULL; }

        /* Close the file */
        if(inData) {
            smf_close_file(&inData, status);
            if(*status != SAI__OK) {
                *status = SAI__ERROR;
                errRep(FUNC_NAME, "Unable to close the input file!", status);
                goto CLEANUP;
            }
        }

        /* Write output */
        smf_write_smfData(outData, NULL, NULL, gOut, fIndex, 0, MSG__VERB, 0, status);
        if(*status != SAI__OK) {
            *status = SAI__ERROR;
            errRep(FUNC_NAME, "Unable to write the output file!", status);
            goto CLEANUP;
        }
        smf_close_file(&outData, status);
        if(*status != SAI__OK) {
            *status = SAI__ERROR;
            errRep(FUNC_NAME, "Unable to close the output file!", status);
            goto CLEANUP;
        }
    }

CLEANUP:
    if(IFG)  { IFG = astFree(IFG); }
    if(DS)   { DS = astFree(DS); }
    if(plan && !pland) { fftw_destroy_plan(plan); }
    if(DSIN) { fftw_free(DSIN); DSIN = NULL; }
    if(SPEC) { fftw_free(SPEC); SPEC = NULL; }

    /* Close files if still open */
    if(inData) {
        smf_close_file(&inData, status);
        if(*status != SAI__OK) {
            *status = SAI__ERROR;
            errRep(FUNC_NAME, "CLEANUP: Unable to close the input file!", status);
        }
    }
    if(outData) {
        smf_close_file(&outData, status);
        if(*status != SAI__OK) {
            *status = SAI__ERROR;
            errRep(FUNC_NAME, "CLEANUP: Unable to close the output file!", status);
        }
    }

    /* END NDF */
    ndfEnd(status);

    /* Delete Groups */
    grpDelet(&gIn, status);
    grpDelet(&gOut, status);
}
