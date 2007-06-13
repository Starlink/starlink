/*
*+
*  Name:
*     smf_calcmodel_ast

*  Purpose:
*     Calculate the ASTronomical model signal component

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     smf_calcmodel_ast( smfData *res, AstKeyMap *keymap, int *lut,
*                        double *map, double *mapvar, smfData *model, 
*                        int flags, int *status);

*  Arguments:
*     res = smfData * (Given and Returned)
*        The residual signal from previously calculated model components
*     keymap = AstKeyMap * (Given)
*        Parameters that control the iterative map-maker
*     lut = int * (Given)
*        Lookup table for map pixel of each data point
*     map = double * (Given)
*        Buffer containing current estimate of the map (must match the LUT
*        in the mapcoord extension of the res data structure)
*     mapvar = double * (Given)
*        Buffer containing current variance estimate corresponding to map
*     model = smfData * (Returned)
*        The data structure that will store the calculated model parameters
*     flags = int (Given )
*        Control flags: 
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     A special model component that assumes that the map is currently the 
*     best rebinned estimate of the sky and projects that signal into the
*     time domain using the LUT.

*  Notes:
*     The model pointer is ignored and should be set to NULL.

*  Authors:
*     Edward Chapin (UBC)
*     {enter_new_authors_here}

*  History:
*     2006-07-10 (EC):
*        Initial Version
*     2006-11-02 (EC):
*        Updated to correctly modify cumulative and residual models
*     2007-03-05 (EC)
*        Modified bit flags
*     2007-05-23 (EC)
*        Removed CUM calculation
*     2007-06-13 (EC)
*        pointing lut supplied as extra parameter to accomodate 
*        new DIMM file format
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

#define FUNC_NAME "smf_calcmodel_ast"

void smf_calcmodel_ast( smfData *res, AstKeyMap *keymap, int *lut, 
			double *map, double *mapvar, smfData *model, 
			int flags, int *status) {

  /* Local Variables */
  dim_t i;                      /* Loop counter */
  double *model_data=NULL;      /* Pointer to DATA component of model */
  dim_t ndata;                  /* Number of data points */
  double *res_data=NULL;        /* Pointer to DATA component of res */
  double sigma;                 /* Array standard deviation */ 
  
  /* Main routine */
  if (*status != SAI__OK) return;

  /* Get pointers to DATA components */
  res_data = (double *)(res->pntr)[0];
  model_data = (double *)(model->pntr)[0];

  if( (res_data == NULL) || (lut == NULL) || (model_data == NULL) ) {
    *status = SAI__ERROR;
    errRep(FUNC_NAME, "Null data / pointing LUT in inputs", status);      
  } else {

    /* Get the raw data dimensions */
    ndata = (res->dims)[0] * (res->dims)[1] * (res->dims)[2];

    /* Loop over data points */ 
    for( i=0; i<ndata; i++ ) {
      if( lut[i] != VAL__BADI ) {
	/* Add previous iteration of the model back into the residual */
	res_data[i] += model_data[i];
	
	/* calculate new model using the map/LUT */
	model_data[i] = map[lut[i]];
	
	/* update the residual model */
	res_data[i] -= model_data[i];
      }
    }    
  }
}
