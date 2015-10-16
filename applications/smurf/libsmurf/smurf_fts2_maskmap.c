/*
*+
*  Name:
*     FTS2MASKMAP

*  Purpose:
*     Mask time series data for a SCUBA-2 map with FTS-2 in the beam.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     smurf_fts2_maskmap(int *status);

*  Arguments:
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This routine masks one or more SCUBA-2 time series cubes taken
*     with FTS-2 in the beam.

*  ADAM Parameters:
*     FTSPORT = _CHAR (Read)
*          The FTS-2 port to use in calculating the mapping to sky
*          coordinates. This parameter should be "tracking" or
*          "image". [TRACKING]
*     IN = NDF (Read)
*          A group of existing time series data cubes.
*     OUT = NDF (Write)
*          A group of output NDFs into which the masked time series data
*          will be written.

*  Related Applications:
*     SMURF: MAKEMAP

*  Authors:
*     Graham Bell (JAC)
*     {enter_new_authors_here}

*  History:
*     11-MAR-2013 (GSB):
*        Original version.
*     04-APR-2013 (GSB):
*        Implemented filtering by offset position.

*  Copyright:
*     Copyright (C) 2013 Science and Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 3 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public
*     License along with this program; if not, write to the Free
*     Software Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston,
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

/* STARLINK includes */
#include "ast.h"
#include "mers.h"
#include "par.h"
#include "par_par.h"
#include "prm_par.h"
#include "ndf.h"
#include "sae_par.h"
#include "star/hds.h"
#include "star/ndg.h"
#include "star/grp.h"
#include "star/atl.h"
#include "star/kaplibs.h"


/* SMURF includes */
#include "smurf_par.h"
#include "smurflib.h"
#include "libsmf/smf.h"

#define FUNC_NAME "smurf_fts2_maskmap"
#define TASK_NAME "FTS2MASKMAP"
#define LEN__METHOD 20

void smurf_fts2_maskmap(int* status) {

/* Local Variables */
   AstFrameSet *slicefrm = NULL;
   Grp* igrp = NULL;          /* Group of input template files */
   Grp* ogrp = NULL;          /* Group containing output file */
   dim_t i;                   /* Index of time slice */
   dim_t r;
   dim_t c;
   dim_t iel;                 /* Index of element */
   int icd;
   dim_t nrows;
   dim_t ncols;
   dim_t ndata;               /* Number of elements in array */
   dim_t ntslice;             /* Number of time slices in array */
   double* pd = NULL;         /* Pointer to next element */
   int32_t* pi = NULL;        /* Pointer to element (INTEGER) */
   int flag;                  /* Was the group expression flagged? */
   fts2Port fts_port;
   char fts_port_name[10];
   size_t ifile;              /* Input file index */
   int indfin;                /* Input template cube NDF identifier */
   int indfout;               /* Output cube NDF identifier */
   int isInteger = 0;         /* Data is INTEGER (non-flatfielded) */
   size_t outsize;            /* Number of files in output group */
   size_t size;               /* Number of files in input group */
   size_t bstride;            /* Bolometer stride */
   size_t tstride;            /* Timeslice stride */
   smfData* odata = NULL;     /* Pointer to output data struct */
   smfHead *hdr = NULL;       /* Pointer to data header */
   double coord[2 * 32 * 40];
   int lbnd[] = {1, 1};
   int ubnd[] = {32, 40};
   double limit = pow(AST__DD2R * 100 / 3600, 2);
   double tol = AST__DD2R * 5 / 3600;

/* Check inherited status */
   if (*status != SAI__OK) return;

/* Begin an AST context */
   astBegin;

/* Begin an NDF context. */
   ndfBegin();

/* Get a group of reference time series files to use as templates for
   the output time series files.*/
   ndgAssoc("IN", 1, &igrp, &size, &flag, status);

/* Get output file(s) */
   kpg1Wgndf("OUT", igrp, size, size, "More output files required...",
             &ogrp, &outsize, status);

/* Determine FTS-2 port. */
    parChoic("FTSPORT", "", "TRACKING,IMAGE", 0, fts_port_name, 10, status);
    if (*status == PAR__NULL) {
        errAnnul(status);
        fts_port = NO_FTS;
    }
    else {
        if (! strcmp("TRACKING", fts_port_name)) {
            fts_port = FTS_TRACKING;
        }
        else {
            fts_port = FTS_IMAGE;
        }
    }

/* Loop round all the template time series files. */
   for (ifile = 0; ifile < size && *status == SAI__OK; ifile ++) {

/* Start a new NDF context. */
      ndfBegin();

/* Create the output NDF by propagating everything from the input, except
   for quality and variance. */
      ndgNdfas(igrp, ifile + 1, "READ", &indfin, status);

      ndgNdfpr(indfin, "DATA,HISTORY,LABEL,TITLE,WCS,UNITS,EXTENSION(*)",
               ogrp, ifile + 1, &indfout, status);
      ndfAnnul(&indfin, status);
      ndfAnnul(&indfout, status);

/* We now re-open the output NDF and then modify its data values. */
      smf_open_file(NULL, ogrp, ifile + 1, "UPDATE", SMF__NOTTSERIES, &odata, status);

/* Issue a suitable message and abort if anything went wrong. */
      if (*status != SAI__OK) {
         errRep(FUNC_NAME, "Could not open input file.", status);
         break;

      }
      else {
         if (odata->file == NULL) {
            *status = SAI__ERROR;
            errRep(FUNC_NAME, "No smfFile associated with smfData.",
                   status);
            break;
         }
         else if ((hdr = odata->hdr) == NULL) {
            *status = SAI__ERROR;
            errRep(FUNC_NAME, "No smfHead associated with smfData.",
                   status);
            break;
         }
      }

/* Check the reference time series contains double precision or integer
 * values. */
     if (smf_dtype_check(odata, NULL, SMF__DOUBLE, status)) {
       /* Flatfielded data. */
       isInteger = 0;
     }
     else if (smf_dtype_check_fatal(odata, NULL, SMF__INTEGER, status)) {
       /* Raw (un-flatfielded) data. */
       isInteger = 1;
     }


/* Get the total number of data elements, and the number of time slices. */
     smf_get_dims(odata, &nrows, &ncols, NULL, &ntslice, &ndata,
                  &bstride, &tstride, status);

     if (*status == SAI__OK && (nrows != 40 || ncols != 32)) {
       *status = SAI__ERROR;
       errRep(FUNC_NAME, "Subarray isn't the right size.", status);
     }

/* Fill the output with bad values. */
      if (*status == SAI__OK) {
        if (isInteger) {
          pi = odata->pntr[0];
        }
        else {
          pd = odata->pntr[0];
        }

        for (i = 0; i < ntslice; i ++) {
          /* Limit coordinate conversions to only certain timeslices
           * to avoid excess computation. */
          if (! (i % 25)) {
            smf_tslice_ast(odata, i, 1, fts_port, status);
            if (*status != SAI__OK) {
              break;
            }
            slicefrm = astCopy(hdr->wcs);
            smf_set_moving( (AstFrame *) slicefrm, NULL, status);
            astTranGrid(slicefrm, 2, lbnd, ubnd, /*Tol:*/ tol,
                        /*Maxpix:*/ 1000, /*Forward:*/ 1, 2, 32 * 40, coord);
          }

          for (c = 0; c < 32; c ++) {
            for (r = 0; r < 40; r ++ ) {
              iel = (tstride * i) + (bstride * (c + r * 32));
              icd = c + r * 32;

              if ((pow(c - 15.5, 2) + pow(r - 23.5, 2) > 256.0)
                  || (pow(coord[icd], 2) + pow(coord[icd + 40 * 32], 2)
                         > limit)) {
                if (pi) {
                  pi[iel] = VAL__BADI;
                }
                else {
                  pd[iel] = VAL__BADD;
                }
              }
            }
          }
        }
      }

/* Close the output time series file. */
      smf_close_file( NULL,&odata, status);

/* End the NDF context. */
      ndfEnd(status);
   }

/* Close any input data file that is still open due to an early exit from
   the above loop. */
   if (odata != NULL) {
      smf_close_file( NULL,&odata, status);
      odata = NULL;
   }

/* Free remaining resources. */
   if (igrp != NULL) grpDelet(&igrp, status);
   if (ogrp != NULL) grpDelet(&ogrp, status);

/* End the NDF context. */
   ndfEnd(status);

/* End the tile's AST context. */
   astEnd;

/* Issue a status indication.*/
   if (*status == SAI__OK) {
      msgOutif(MSG__VERB," ",TASK_NAME " succeeded, time series written.", status);
   }
   else {
      msgOutif(MSG__VERB," ",TASK_NAME " failed.", status);
   }
}
