/*
*+
*  Name:
*     smf_correct_extinction

*  Purpose:
*     Low-level EXTINCTION correction function

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     smf_correct_extinction( smfData *data, float tau, int *status) {

*  Arguments:
*     data = smfData*
*        smfData struct
*     tau = float (Given)
*        Optical depth at this wavelength
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This is the main routine implementing the EXTINCTION task.


*  Authors:
*     Andy Gibb (UBC)
*     {enter_new_authors_here}

*  History:
*     2005-09-27 (AGG):
*        Initial test version
*     2005-11-14 (AGG):
*        Update API to accept a smfData struct
*     2005-12-20 (AGG):
*        Store the current coordinate system on entry and reset on return.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2005 Particle Physics and Astronomy Research Council.
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

#include <stdio.h>

#include "smf.h"
#include "sae_par.h"
#include "ast.h"
#include "mers.h"
#include "msg_par.h"
#include "prm_par.h"
#include "smurf_par.h"
#include "smurf_typ.h"

/* Fortran prototypes */
#include "f77.h"
F77_DOUBLE_FUNCTION(sla_airmas)( double * );

void smf_correct_extinction(smfData *data, float tau, int *status) {

  /* Local variables */
  double airmass;     /* Airmass */
  dim_t curslice;     /* Index for current time slice */
  dim_t i;            /* Loop counter */
  dim_t index;        /* index into vectorized data array */
  dim_t j;            /* Loop counter */
  int nslice;         /* Number of points in a time slice */
  double xin;         /* X coordinate of input mapping */
  double xout;        /* X coordinate of output */
  double yin;         /* Y coordinate of input */
  double yout;        /* Y coordinate of output */
  double zd;          /* Zenith distance */
  double *indata;     /* Pointer to data array */
  AstFrameSet *wcs;   /* Pointer to AST frameset */

  smfHead *hdr;
  const char *origsystem; /* Character string to store the coordinate
			     system on entry */

  if (*status != SAI__OK) return;

  /* Store header */
  hdr = data->hdr;
  wcs = hdr->wcs;

  /*astShow(wcs);*/

  /* Check current frame and store it */
  origsystem = astGetC( wcs, "SYSTEM");

  /* Select the AZEL system */
  if (wcs != NULL) 
    astSetC( wcs, "SYSTEM", "AZEL" );

  if (!astOK) {
    if (*status == SAI__OK) {
      *status = SAI__ERROR;
      errRep( "extinction", "Error from AST", status);
    }
  }

  /* Assign pointer to input data array */
  indata = (data->pntr)[0]; 

  /* Assume 2 dimensions. Start counting at 1 since this is the GRID
     coordinate frame */
  nslice = (data->dims)[0] * (data->dims)[1];
  curslice = hdr->curslice;
  /*  printf(" curslice: %"DIM_T_FMT"\n", curslice);*/
  for (j = 1; j <= (data->dims)[1]; j++) {
    for (i = 1; i <= (data->dims)[0]; i++) {
      
      index = nslice*curslice + (j-1)*(data->dims)[0] + (i-1);
      if (indata[index] != VAL__BADR) {

	xin = (double)i;
	yin = (double)j;
	astTran2( wcs, 1, &xin, &yin, 1, &xout, &yout );

	zd = M_PI_2 - yout;
	airmass = F77_CALL(sla_airmas)( &zd );
	/*	printf( "Zenith distance: %f, Airmass: %f\n",zd, airmass);
	printf("Index: %" DIM_T_FMT "  Data: %f  Correction: %f\n",
	index, indata[index], (exp(airmass*(double)tau)));*/
	indata[index] *= exp(airmass*(double)tau);
      }
    }
  }

  /* Restore frame to original */
  astSetC( wcs, "SYSTEM", origsystem );

}
