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
*     This is the main routine for reading FITS information from an
*     NDF. The header is stored as a 1-D array of characters and
*     individual FITS records are determined by stepping through the
*     array in chunks of 80 or 81 depending on whether each record is
*     null-terminated. Returns with
*     

*  Authors:
*     Andy Gibb (UBC)
*     Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2005-11-04 (AGG):
*        Initial test version
*     2005-11-09 (AGG):
*        Add comments and fix error message
*     2005-11-10 (AGG):
*        Add test for existence of FITS extension
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

  char fitsloc[DAT__SZLOC]; /* HDS locator for FITS header */
  int ndim;                 /* Dimensions of FITS header array*/
  int fitsdim[NDF__MXDIM];  /* Number of elements in each dimension */
  int nfits;                /* Number of FITS records in header */
  char *fitsrec = NULL;     /* Pointer to the FITS header array */
  int hasfits;              /* Flag to indicate whether FITS extension exists */

  hdsdim dims[1];

  if ( *status != SAI__OK ) return;

  /* Test whether FITS extension is present */
  ndfXstat( indf, "FITS", &hasfits, status);

  if ( hasfits == 0) {
    if ( *status == SAI__OK) {
      msgOut("smf_fits_rdhead", "Input file does not have a FITS extension", status);
    }
    return;
  }

  /* Find the HDS locator to the FITS component */
  ndfXloc( indf, "FITS", "READ", fitsloc, status);

  /* Determine the dimensionality of the FITS component */
  datShape( fitsloc, NDF__MXDIM, fitsdim, &ndim, status);

  printf("ndim = %d \n",ndim);

  /*  Check dimensions */
  if (ndim != 1) {
    if ( *status == SAI__OK) {
      *status = SAI__ERROR;
      msgSeti("NDIM", ndim);
      errRep("smf_fits_rdhead","Number of dimensions = ^NDIM; should be 1", status);
    }
  }

  /* Get number of FITS header entries: HDS `knows' it's reading
     80-char strings rather than a simple array of characters */
  datSize( fitsloc, &nfits, status );

  printf("nfits = %d \n",nfits);

  /* Allocate the memory for the FITS header */
  if ( *status == SAI__OK) {
    fitsrec = malloc( sizeof(char) * nfits * 80 + 1);
  }

  /* Proceed with retrieving the FITS header */
  if (fitsrec != NULL) {
    dims[0] = nfits;
    /* Retrieve nfits records each of length 80 and store in fitsrec */
    datGetC( fitsloc, 1, dims, fitsrec, 80, status);
    /* Add string terminator */
    fitsrec[nfits*80] = '\0';
  } else {
    if ( *status == SAI__OK ) {
      *status = SAI__ERROR;
      errRep("smf_fits_rdhead","Unable to allocate memory for FITS header", status);
    }
  }
  /* Create an AstFitsChan from the FITS header */
  smf_fits_crchan( nfits, fitsrec, fchan, status);
  /* Free up the memory */
  free(fitsrec);
  /* Free up the NDF */
  datAnnul(fitsloc, status);

  printf("fitsrec: %s \n",fitsrec);

  return;
}
