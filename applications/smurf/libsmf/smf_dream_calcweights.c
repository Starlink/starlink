/*
 *+
 *  Name:
 *     smf_dream_calcweights

 *  Purpose:
 *     Routine to calculate the DREAM weights arrays

 *  Language:
 *     Starlink ANSI C

 *  Type of Module:
 *     Subroutine

 *  Invocation:
 *     smf_dream_calcweights( smfData *data, const Grp *ogrp, const int index,
 *                           const double gridstep, const int ngrid,
 *                           const int *gridminmax, int gridpts[][2],
 *                           int *status);

 *  Arguments:
 *     data = smfData * (Given and Returned)
 *        Input data
 *     ogrp = const Grp * (Given)
 *        NDG group identifier
 *     index = const int (Given)
 *        Index corresponding to required file in group
 *     gridstep = const double (Given)
 *        Size of DREAM grid step in arcsec
 *     ngrid = const int (Given)
 *        Number of points in the reconstruction grid
 *     gridminmax = const int * (Given)
 *        Pointer to array containing extent of reconstruction grid
 *     gridpts[][2] = int (Returned)
 *        Array of X, Y positions in reconstruction grid
 *     status = int* (Given and Returned)
 *        Pointer to global status.

 *  Description:
 *     This routine is responsible for calculating the weights arrays
 *     for DREAM observations with SCUBA-2. A new NDF file is created
 *     which contains the weights arrays as extensions under the
 *     .MORE.DREAM hierarchy. The grid weights array is stored as
 *     .GRIDWTS and the inverse matrix as .INVMATX. Other relevant grid
 *     parameters are also stored in the file.
 *
 *     The routine currently returns with good status if the data are
 *     not from a DREAM observation, but returns with an error if the
 *     data are not in time series format.

 *  Notes:

 *  Authors:
 *     Andy Gibb (UBC)
 *     {enter_new_authors_here}

 *  History:
 *     2006-09-15 (AGG):
 *        Initial version, based on calcmapwt.c
 *     2006-10-11 (AGG):
 *        - Bring prologue up to date
 *        - Update call to smf_open_newfile due to API change
 *     2006-10-26 (AGG):
 *        Add GRIDSTEP keyword
 *     2007-04-05 (AGG):
 *        Change OBSMODE to SAM_MODE
 *     2007-07-05 (AGG):
 *        Add status check before accessing gridext pointer to ensure
 *        smooth error reporting
 *     2007-12-18 (AGG):
 *        Update to use new smf_free behaviour
 *     2008-07-11 (AGG):
 *        Hand off heavy lifting to calcmapwt routine in sc2math, allow
 *        for lower-case obsmode string
 *     2008-07-14 (AGG):
 *        Remove unnecessary call to calculate jiggrid array
 *     2008-07-18 (TIMJ):
 *        Use smf_find_subarray
 *     2008-07-24 (TIMJ):
 *        Use hdr->obsmode instead of SAM_MODE.
 *     2008-07-29 (TIMJ):
 *        Steptime is now in smfHead.
 *     {enter_further_changes_here}

 *  Copyright:
 *     Copyright (C) 2008 Science and Technology Facilities Council.
 *     Copyright (C) 2006-2008 University of British Columbia. All
 *     Rights Reserved.

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

/* Standard includes */
#include <stdio.h>
#include <string.h>

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
#include "star/kaplibs.h"

/* SMURF includes */
#include "smf.h"
#include "smurf_par.h"

/* SC2DA includes */
#include "sc2da/sc2store_par.h"
#include "sc2da/sc2math.h"
#include "sc2da/sc2store.h"
#include "sc2da/sc2ast.h"
#include "sc2da/dream_par.h"

/* Simple default string for errRep */
#define FUNC_NAME "smf_dream_calcweights"
#define SUB__MAXNAM 8 /* Maximum length for subarray name string */

void smf_dream_calcweights( smfData *data, const Grp *ogrp, const int index,
                            const double gridstep, const int ngrid,
                            const int *gridminmax, int gridpts[][2],
                            int *status) {

  /* Local Variables */
  int conv_shape = CONV__SINCTAP;/* Code for convolution function */
  double conv_sig = 1.0;        /* Convolution function parameter */
  smfDream *dream = NULL;       /* DREAM parameters obtained from input data file */
  char filename[GRP__SZNAM+1];  /* Input filename, derived from GRP */
  double *gridwts = NULL;       /* Pointer to array of grid weights */
  dim_t gridwtsdim[DREAM__MXGRID];/* Dimensions of grid weights array*/
  smfHead *hdr = NULL;          /* Header for input data */
  double *invmat = NULL;        /* Pointer to inverted matrix */
  int invmatdim;                /* Size of inverted matrix */
  int jigext[4] = {-1, 1, -1, 1}; /* Extents in the X, Y directions of the
                                     jiggle pattern */
  int nbolx;                    /* Number of bolometers in X direction */
  int nboly;                    /* Number of bolometers in Y direction */
  dim_t nsampcycle;             /* Number of samples in a jiggle cycle */
  char *pname = NULL;           /* Pointer to filename string */
  int *qual = NULL;             /* True/false `quality' array (not NDF quality) */
  int qualdim[2];               /* Dimensions of quality array */
  int smu_move = 8;             /* Code for SMU motion*/
  double smu_offset = 0.0;      /* SMU timing offset in ms */
  char subarray[SUB__MAXNAM+1]; /* Name of subarray */
  double tsamp;                 /* Sample/step time in seconds */
  int tvert;                    /* Time between vertices in ms (aka leg length) */
  int windext[4];               /* Size of the window for constructing a DREAM image */

  if ( *status != SAI__OK) return;

  /* Check it's 3-d time series */
  if ( data->ndims == 3) {

    /* Check we have a DREAM observation */
    hdr = data->hdr;
    if ( hdr->obsmode == SMF__OBS_DREAM ) {
      /* OK we have DREAM data */
      dream = data->dream;
      /* Read DREAM parameters from input file */
      smf_find_subarray( hdr, subarray, sizeof(subarray), NULL, status );
      tsamp = hdr->steptime;

      /* Convert steptime into millisec */
      tsamp *= 1000.0;
      nsampcycle = dream->nsampcycle;
      tvert = tsamp*nsampcycle/(dream->nvert);

      msgSeti("I",index);
      msgOutif(MSG__VERB," ", "Beginning weights calculation for file ^I: this will take some time (~5-10 mins)", status);

      /* Some useful shortcuts. Maybe. */
      nbolx = (int)(data->dims)[0];
      nboly = (int)(data->dims)[1];

      /* Allocate quality array */
      qualdim[0] = nbolx;
      qualdim[1] = nboly;
      qual = astCalloc( qualdim[0]*qualdim[1], sizeof(*qual) );


      sc2math_calcmapwt( subarray, nbolx, nboly,
                         qual, conv_shape, conv_sig, gridstep,
                         dream->nvert, tvert, tsamp,
                         dream->jigvert, dream->jigscal, dream->jigscal, smu_move,
                         smu_offset, ngrid, gridpts, &(gridwtsdim[0]), &gridwts,
                         &invmatdim, &invmat, status);

      /* Dummy definition of window - this must be derived somehow TBD */
      windext[0] = 0;
      windext[1] = (int)(data->dims)[0] - 1;
      windext[2] = 0;
      windext[3] = (int)(data->dims)[1] - 1;

      /* Obtain filename to write weights into */
      pname = filename;
      grpGet( ogrp, index, 1, &pname, SMF_PATH_MAX, status);
      if ( *status != SAI__OK ) {
        errRep(FUNC_NAME, "Unable to retrieve file name in which to store weights solution", status);
      }

      /* Store DREAM weights in output file */
      sc2store_cremapwts ( pname, windext, gridminmax, gridstep, jigext,
                           dream->jigscal, gridwtsdim, gridwts, invmatdim, invmat,
                           qualdim, qual, status );

    } else {
      msgSeti("I", index);
      msgOutif(MSG__VERB," ",
               "Input file ^I is not a DREAM observation - ignoring", status);
    }
  } else {
    if ( *status == SAI__OK ) {
      *status = SAI__ERROR;
      errRep(FUNC_NAME, "Data must be in timeseries format to calculate DREAM weights", status);
    }
  }
}

