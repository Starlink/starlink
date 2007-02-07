/*
*+
*  Name:
*     smf_calcmap

*  Purpose:
*     Calculate the map by accumulating flux from a cleaned data file

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     smf_calcmap( smfData *res, AstKeyMap *keymap, 
*                  double *map, double *mapvar, smfData *weights, int msize,
*                  int flags, int *status);

*  Arguments:
*     res = smfData * (Given and Returned)
*        The residual signal from previously calculated model components
*     keymap = AstKeyMap * (Given)
*        Parameters that control the iterative map-maker
*     map = double * (Given)
*        Buffer containing current estimate of the map (must match the LUT
*        in the mapcoord extension of the res data structure)
*     mapvar = double * (Given)
*        Buffer containing current variance estimate corresponding to map
*     weights = double * (Given)
*        Buffer containing current weights corresponding to map
*     msize = int (Given)
*        Number of pixels in map
*     flags = int (Given )
*        Control flags. Set to AST__REBININIT if accumulating the first 
*        data file, and AST__REBINEND if this is the last data file to
*        re-normalize the answer.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Accumulate flux in the map. 

*  Notes:

*  Authors:
*     Edward Chapin (UBC)
*     {enter_new_authors_here}

*  History:
*     2006-11-02 (EC):
*        Initial Version
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2005-2006 Particle Physics and Astronomy Research Council.
*     University of British Columbia.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
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

/* Starlink includes */
#include "mers.h"
#include "ndf.h"
#include "sae_par.h"
#include "star/ndg.h"
#include "prm_par.h"
#include "par_par.h"

/* SMURF includes */
#include "libsmf/smf.h"

#define FUNC_NAME "smf_calcmap"

void smf_calcmap( smfData *res, AstKeyMap *keymap, 
		  double *map, double *mapvar, double *weights, int msize, 
		  int flags, int *status) {

  /* Local Variables */
  int *lut=NULL;                /* Pointing lookup table */
  dim_t i;                      /* Loop counter */
  dim_t ndata;                  /* Total number of data points */
  double *res_data=NULL;        /* Pointer to DATA component of res */
  double *res_var=NULL;         /* Pointer to VARIANCE component of res */

  /* Main routine */
  if (*status != SAI__OK) return;

  /* Get pointers to DATA components */
  res_data = (double *)(res->pntr)[0];
  res_var = (double *)(res->pntr)[1];  /* It's OK if this is NULL */
  lut = res->lut;

  if( (res_data == NULL) || (lut == NULL) ) {
    *status = SAI__ERROR;
    errRep(FUNC_NAME, "Null data / pointing LUT in inputs", status);      
  } else {    
    /* Get the raw data dimensions */
    ndata = (res->dims)[0] * (res->dims)[1] * (res->dims)[2];

    /* Add the previous estimate of the astronomical signal back in */
    for( i=0; i<ndata; i++ ) {
      res_data[i] += last_ast_data[i];
    }

    /* Rebin the data. At this stage the input signal should have all of
       the modelled signal components removed except for the astronomical
       signal (since it was just added back in) */
    smf_simplerebinmap( res_data, res_var, lut, ndata, flags, map,
			mapvar, msize, status );
  }
}
