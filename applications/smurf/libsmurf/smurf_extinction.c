/*
*+
*  Name:
*     smurf_extinction

*  Purpose:
*     Top-level EXTINCTION implementation

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     smurf_extinction( int *status );

*  Arguments:
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This is the main routine implementing the EXTINCTION task.

*  ADAM Parameters:


*  Authors:
*     Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2005-09-27 (TIMJ):
*        Initial test version
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
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public
*     License along with this program; if not, write to the Free
*     Software Foundation, Inc., 59 Temple Place,Suite 330, Boston,
*     MA 02111-1307, USA

*  Bugs:
*     {note_any_bugs_here}
*-
*/

#if HAVE_CONFIG_H
#include <config.h>
#endif

#if HAVE_MATH_H
#  include <math.h>
#endif
#if HAVE_GSL_GSL_MATH_H
#  include <gsl/gsl_math.h>
#endif

#ifndef M_PI_2
#  ifdef M_PI
#    define M_PI_2  (M_PI/2)
#  else
error can not determine PI
#  endif
#endif

#include "prm_par.h"
#include "sae_par.h"
#include "ast.h"
#include "ndf.h"
#include "mers.h"
#include "smurflib.h"

/* Fortran prototypes */
#include "f77.h"
F77_DOUBLE_FUNCTION(sla_airmas)( double * );

void smurf_extinction( int * status ) {

  /* Local Variables */
  double airmass;     /* Local airmass */
  int i;              /* loop counter */
  void * indataArr[1];  /* Pointer to input data */
  float * indata;
  int index;          /* index into vectorized data array */
  int indf;           /* Input NDF identifier */
  int indims[NDF__MXDIM]; /* Dimensions of input NDF */
  int j;              /* Loop counter */
  int ndims;          /* Number of active dimensions in input */
  int nin;            /* Number of input data points */
  int nout;           /* Number of output data points */
  void * outdataArr[1];    /* Pointer to output data */
  float * outdata;
  int outndf;         /* Output NDF identifier */
  float tau;          /* tau at this wavelength */
  double xin;         /* X coordinate of input mapping */
  double xout;        /* X coordinate of output */
  double yin;         /* Y coordinate of input */
  double yout;        /* Y coordinate of output */
  AstFrameSet * wcs;  /* Pointer to frame set */
  double zd;          /* Zenith distance */

  tau = 0.5;

  /* Main routine */

  ndfBegin();

  msgOut("smurf_extinction","Inside EXTINCTION", status );

  /* Read the input and output files */
  ndfAssoc( "IN", "READ", &indf, status );
  ndfProp( indf, "WCS", "OUT", &outndf, status );

  /* Check type of input data array */

  /* Obtain pointers to data array */
  ndfMap( indf, "DATA", "_REAL", "READ", &indataArr, &nin, status);
  ndfMap( outndf, "DATA", "_REAL", "WRITE", &outdataArr, &nout, status );

  /* Check for existence of VARIANCE array */


  /* Check for covariance */
  /* smf_find_extension( indf, "COVAR", &cndf, status ); */


  indata = indataArr[0];
  outdata = outdataArr[0];

  if (*status != SAI__OK) {
    if ( nin != nout) {
      *status = SAI__ERROR;
      msgSeti( "NIN", nin);
      msgSeti( "NOUT", nout);
      errRep( "smurf_extinction", "Number of input pixels not equal to the number of output pixels (^NIN != ^NOUT)",status);
    }
  }

  /* Need the dimensions of the data array */
  ndfDim( indf, NDF__MXDIM, indims, &ndims, status );

  /* Get the world coordinate information */
  ndfGtwcs( indf, &wcs, status );

  /* Select the AZEL system */
  astSetC( wcs, "SYSTEM", "AZEL" );

  if (!astOK) {
    if (*status == SAI__OK) {
      *status = SAI__ERROR;
      errRep( "extinction", "Error from AST", status);
    }
  }

  /* Assume 2 dimensions. Start counting at 1 since this is the GRID
     coordinate frame */
  if (*status == SAI__OK) {
    for (j = 1; j <= indims[1]; j++) {
      for (i = 1; i <= indims[0]; i++) {

	index = (j-1)*indims[0] + (i-1);
	if (indata[index] != VAL__BADR) {

	  xin = (double)i;
	  yin = (double)j;
	  astTran2( wcs, 1, &xin, &yin, 1, &xout, &yout );

	  zd = M_PI_2 - yout;
	  airmass = F77_CALL(sla_airmas)( &zd );
	  printf( "Airmass: %f\n",airmass);

	  printf("Index: %d  Data: %f  Correction: %f\n",
		 index, indata[index], (expf((float)airmass*tau)));
	  outdata[index] = indata[index] * expf((float)airmass*tau);
	} else {
	  outdata[index] = indata[index];
	}
      }
    }
  }

  ndfEnd( status );

}
