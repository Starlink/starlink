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
*     Andy Gibb (UBC)
*     Edward Chapin (UBC)
*     {enter_new_authors_here}

*  History:
*     2005-09-27 (TIMJ):
*        Initial test version
*     2005-09-27 (AGG):
*        Now uses smurf_par.h
*        Factored out extinction correction
*     2005-09-27 (EC)
*        Trap memmove when status is bad
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

#include <string.h>

#include "smurf_par.h"
#include "prm_par.h"
#include "sae_par.h"
#include "ast.h"
#include "ndf.h"
#include "mers.h"

#include "libsmf/smf.h"
#include "smurflib.h"

void smurf_extinction( int * status ) {

  /* Local Variables */
  void * indataArr[1];  /* Pointer to input data */
  float * indata = NULL;
  int indf = 0;       /* Input NDF identifier */
  dim_t indims[2];    /* Copy of the NDF dimensions */
  int ndfdims[NDF__MXDIM]; /* Dimensions of input NDF */
  int ndims;          /* Number of active dimensions in input */
  int nin;            /* Number of input data points */
  int nout;           /* Number of output data points */
  void * outdataArr[1];    /* Pointer to output data */
  float * outdata = NULL;
  int outndf = 0;         /* Output NDF identifier */
  float tau;          /* tau at this wavelength */
  AstFrameSet * wcs = NULL;  /* Pointer to frame set */

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


  if ( *status != SAI__OK ) {
    if ( nin != nout) {
      *status = SAI__ERROR;
      msgSeti( "NIN", nin);
      msgSeti( "NOUT", nout);
      errRep( "smurf_extinction", "Number of input pixels not equal to the number of output pixels (^NIN != ^NOUT)",status);
    }
  }

  /* Check for existence of VARIANCE array */

  /* Check for covariance */
  /* smf_find_extension( indf, "COVAR", &cndf, status ); */

  if ( *status == SAI__OK ) {
    indata = indataArr[0];
    outdata = outdataArr[0];
  }

  /* Need the dimensions of the data array */
  ndfDim( indf, NDF__MXDIM, ndfdims, &ndims, status );

  if ( *status == SAI__OK ) {
    indims[0] = (dim_t)ndfdims[0];
    indims[1] = (dim_t)ndfdims[1];
  }
  
  /* Get the world coordinate information */
  ndfGtwcs( indf, &wcs, status );

  /* Select the AZEL system */
  if ( (wcs != NULL) || (*status == SAI__OK) ) 
    astSetC( wcs, "SYSTEM", "AZEL" );

  if (!astOK) {
    if (*status == SAI__OK) {
      *status = SAI__ERROR;
      errRep( "extinction", "Error from AST", status);
    }
  }

  if (*status == SAI__OK) 
    memmove( outdata, indata, (size_t)(nin*sizeof(indata[0])) );

  smf_correct_extinction( wcs, indims, tau, outdata, status);

  ndfEnd( status );

}
