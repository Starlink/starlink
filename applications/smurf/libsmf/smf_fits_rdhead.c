/*
*+
*  Name:
*     smf_fits_rdhead

*  Purpose:
*     Helper routine for reading FITS header information from NDFs

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     smf_fits_rdhead( int indf, AstFitsChan **fchan, int *status );

*  Arguments:
*     indf = int (Given)
*        NDF identifier
*     fchan = AstFitsChan** (Returned)
*        AST Fits Chan containing the FITS information
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This is the main routine for reading FITS information from an NDF


*  Authors:
*     Andy Gibb (UBC)
*     {enter_new_authors_here}

*  History:
*     2005-11-04 (AGG):
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

#include "ast.h"
#include "ndf.h"
#include "star/hds.h"
#include "sae_par.h"
#include <stdlib.h>
#include "mers.h"
#include "smf.h"

void smf_fits_rdhead( int indf, AstFitsChan ** fchan, int *status) {

  char fitsloc[DAT__SZLOC];
  int ndim;
  int fitsdim[NDF__MXDIM];
  int nfits;
  char *fitsrec = NULL;

  hdsdim dims[1];

  if ( *status != SAI__OK ) return;

  ndfXloc( indf, "FITS", "READ", fitsloc, status);

  datShape( fitsloc, NDF__MXDIM, fitsdim, &ndim, status);

  if (ndim != 1) {
    if ( *status == SAI__OK) {
      *status = SAI__ERROR;
      msgSeti("NDIM", ndim);
      errRep("smf_fits_rdhead","Number of dimensions = ^NDIM; should be 2", status);
    }
  }

  datSize( fitsloc, &nfits, status );

  printf("%d \n",nfits);

  if ( *status == SAI__OK) {
    fitsrec = malloc( sizeof(char) * nfits * 80 + 1);
  }

  if (fitsrec != NULL) {
    dims[0] = nfits;
    datGetC( fitsloc, 1, dims, fitsrec, 80, status);
    fitsrec[nfits*80] = '\0';
  } else {
    if ( *status == SAI__OK ) {
      *status = SAI__ERROR;
      errRep("smf_fits_rdhead","Unable to allocate memory for FITS header", status);
    }
  }
  smf_fits_crchan( nfits, fitsrec, fchan, status);
  free(fitsrec);
  datAnnul(fitsloc, status);

  printf("%s \n",fitsrec);

  return;
}
