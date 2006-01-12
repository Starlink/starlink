/*
*+
*  Name:
*     smf_correct_extinction

*  Purpose:
*     Low-level EXTINCTION correction function

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Subroutine

*  Invocation:
*     smf_correct_extinction( smfData *data, float tau, int *status) {

*  Arguments:
*     data = smfData* (Given)
*        smfData struct
*     tau = float (Given)
*        Optical depth from the command line
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
*     2005-12-21 (AGG):
*        Use the curslice as a frame offset into the timeseries data
*     2005-12-22 (AGG):
*        Deprecate use of curslice, make use of virtual flag instead
*     2006-01-11 (AGG):
*        API updated: tau no longer needed as it is retrieved from the
*        header. For now, image data uses the MEANWVM keyword from the
*        FITS header
*     2006-01-12 (AGG):
*        API update again: tau will be needed in the case the user
*        supplies it at the command line
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2005-2006 Particle Physics and Astronomy Research
*     Council. University of British Columbia. All Rights Reserved.

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

/* Standard includes */
#include <stdio.h>

/* Starlink includes */
#include "sae_par.h"
#include "ast.h"
#include "mers.h"
#include "msg_par.h"
#include "prm_par.h"

/* SMURF includes */
#include "smf.h"
#include "smurf_par.h"
#include "smurf_typ.h"

/* Fortran prototypes */
#include "f77.h"
F77_DOUBLE_FUNCTION(sla_airmas)( double * );

/* Simple default string for errRep */
#define FUNC_NAME "smf_correct_extinction"

void smf_correct_extinction(smfData *data, float tau, int *status) {

  /* Local variables */
  double airmass;          /* Airmass */
  int filter;              /* The subarray SUBSYSNR from the FITS header */
  AstFitsChan *fitshdr;    /* Pointer to AST FITS chan */
  smfHead *hdr;            /* Pointer to full header struct */
  dim_t i;                 /* Loop counter */
  double *indata;          /* Pointer to data array */
  dim_t index;             /* index into vectorized data array */
  dim_t j;                 /* Loop counter */
  const char *origsystem; /* Character string to store the coordinate
			     system on entry */
  float tauwvm;            /* WVM optical depth from the header */
  sc2head *thead;          /* Pointer to the sc2head header data */
  AstFrameSet *wcs;        /* Pointer to AST WCS frameset */
  double xin;              /* X coordinate of input mapping */
  double xout;             /* X coordinate of output */
  double yin;              /* Y coordinate of input */
  double yout;             /* Y coordinate of output */
  double zd;               /* Zenith distance */

  double junk;          /* tmp var */

  /* Check status */
  if (*status != SAI__OK) return;

  /* Check if data are in suitable format */
  if ( !(data->virtual) && ( (data->dims)[2] != 1 )) {
    if ( *status == SAI__OK) {
      *status = SAI__ERROR;
      errRep(FUNC_NAME, "Data are not in a 2-D format", status);
    }
  }

  /* Retrieve header info */
  hdr = data->hdr;
  wcs = hdr->wcs;
  fitshdr = hdr->fitshdr;

  /*astShow(wcs);*/

  /* Check current frame and store it */
  origsystem = astGetC( wcs, "SYSTEM");

  /* Select the AZEL system */
  if (wcs != NULL) 
    astSetC( wcs, "SYSTEM", "AZEL" );

  if (!astOK) {
    if (*status == SAI__OK) {
      *status = SAI__ERROR;
      errRep( FUNC_NAME, "Error from AST", status);
    }
  }

  /* Assign pointer to input data array */
  indata = (data->pntr)[0]; 

  /* Retrieve WVM tau from header for a timeslice, else the FITS
     header for an image */
  if ( data->virtual == 1 ) {
    thead = hdr->sc2head;
    junk = thead->tcs_az_bc1;
    printf("JUNK = %g\n",junk);
    tauwvm = thead->wvm_time;
    tauwvm = 0.1;
  } else {
    smf_fits_getF( hdr, "MEANWVM", &tauwvm, status );
  }

  /* Determine the wavelength from the subarray name (SUBSYSNR for now) */
  smf_fits_getI( hdr, "SUBSYSNR", &filter, status );
  /*  printf("filter = %d\n",filter);*/

  /* Calculate tau from the WVM (225 GHz) tau and the wavelength */
  tau = smf_scale_tau(tauwvm, filter, status);

  /* Loop over data in time slice. Start counting at 1 since this is
     the GRID coordinate frame */
  for (j = 1; j <= (data->dims)[1]; j++) {
    for (i = 1; i <= (data->dims)[0]; i++) {
      index = (j-1)*(data->dims)[0] + (i-1);
      if (indata[index] != VAL__BADR) {
	xin = (double)i;
	yin = (double)j;
	astTran2( wcs, 1, &xin, &yin, 1, &xout, &yout );
	zd = M_PI_2 - yout;
	airmass = F77_CALL(sla_airmas)( &zd );
	/*	printf( "Zenith distance: %f, Airmass: %f\n",zd, airmass);*/
	/*	printf("Index: %" DIM_T_FMT "  Data: %f  Correction: %f\n",
	  index, indata[index], (exp(airmass*(double)tau)));*/
	indata[index] *= exp(airmass*(double)tau);
      }
    }
  }

  /* Restore frame to original */
  astSetC( wcs, "SYSTEM", origsystem );
  /* Check AST status */
  if (!astOK) {
    if (*status == SAI__OK) {
      *status = SAI__ERROR;
      errRep( FUNC_NAME, "Error from AST", status);
    }
  }

}
