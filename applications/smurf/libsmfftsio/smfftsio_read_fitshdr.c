/*
*+
*  Name:
*     smfftsio_read_fitshdr

*  Purpose:
*     Populate a FitsChan from a FITS file opened with CFITSIO

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     void smfftsio_read_fitshdr( fitsfile *fptr, AstFitsChan * fitschan, int * status );

*  Arguments:
*     fptr = fitsfile * (Given)
*        CFITSIO pointer
*     fitschan = AstFitsChan * (Given and Returned)
*        FitsChan to be populated from the FITS file.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Reads all the header cards from the current CFITSIO HDU and puts them
*     into the supplied AST FitsChan.

*  Authors:
*     TIMJ: Tim Jenness (Cornell)
*     {enter_new_authors_here}

*  Notes:
*     - The currently selected HDU will be read.
*     - The routine is not instrument-specific.
*     - We may want to consider adding this routine to ATL (which would
*       require a new cfitsio dependency on it but it could be a separate
*       library for linking purposes).

*  History:
*     2014-03-20 (TIMJ):
*        Initial version
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2014 Cornell University
*     All Rights Reserved.

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
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
*     MA 02110-1301, USA.

*  Bugs:
*     {note_any_bugs_here}
*-
*/

#include "smfftsio.h"

#include "ast.h"
#include "fitsio.h"
#include "sae_par.h"

void
smfftsio_read_fitshdr( fitsfile *fptr, AstFitsChan * fitschan, int * status ) {

  int fitsStatus = 0;
  int nkeys = 0;
  int i;
  char card[81];

  if (*status != SAI__OK) return;

  CALLCFITSIO(fits_get_hdrspace( fptr, &nkeys, NULL, &fitsStatus ), "Error determining header size" );
  if (*status != SAI__OK) return;

  for (i=1; i <= nkeys; i++) {
    CALLCFITSIO(fits_read_record( fptr, i, card, &fitsStatus ), "Error reading header card" );
    astPutFits( fitschan, card, 0 );
  }

  return;
}

