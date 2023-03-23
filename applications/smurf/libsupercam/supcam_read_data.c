/*
*+
*  Name:
*     supcam_read_data

*  Purpose:
*     Read spectral data from SuperCam SDFITS file

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     data = supcam_read_data( fitsfile * fits, size_t * numRows, size_t * numChans, int * status );

*  Arguments:
*     fits = fitsfile * (Given)
*        CFITSIO pointer to open file.
*     numRows = size_t * (Returned)
*        Number of rows in returned data array.
*     numChans = size_t * (Returned)
*        Number of channels in each spectrum.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Read the spectra from the data file and store in a buffer of size
*     sizeof(float) * numRows * numChans. The buffer is allocated by this
*     routine and should be freed using astFree().

*  Authors:
*     TIMJ: Tim Jenness (Cornell)
*     {enter_new_authors_here}

*  Notes:
*     - Returned buffer must be freed with astFree().

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

#include "libsmfftsio/smfftsio.h"
#include "supercam.h"
#include "fitsio.h"

#include "sae_par.h"
#include "prm_par.h"

#pragma GCC diagnostic ignored "-Wcast-qual"

float *
supcam_read_data( fitsfile * fits, size_t * numRows, size_t * numChans, int * status ) {

  int colnum = 0;    /* DATA column */
  int fitsStatus = 0; /* CFITSIO status */
  long nRows = 0;
  long nChans = 0;
  long dummy;
  float * data = NULL;
  int typecode = 0;

  /* Initialise the return values */
  *numChans = 0;
  *numRows = 0;

  if (*status != SAI__OK) return NULL;

  /* Prepare to read the Binary Table */
  /* Check number of rows */
  CALLCFITSIO( fits_get_num_rows(fits, &nRows, &fitsStatus), "Error getting number of rows" );

  /* Find DATA column and check its repeat count */
  /* cast because cfitsio does not use const */
  CALLCFITSIO( fits_get_colnum(fits, CASEINSEN, (char *)"DATA", &colnum, &fitsStatus),
               "Error getting column number for DATA" );
  CALLCFITSIO( fits_get_coltype(fits, colnum, &typecode, &nChans, &dummy, &fitsStatus),
               "Error getting column information" );
  if (*status != SAI__OK) return NULL;

  /* Get some memory */
  data = astMalloc( nChans * nRows * sizeof(*data) );

  /* Read DATA column */
  CALLCFITSIO( fits_read_col(fits, TFLOAT, colnum, 1, 1,
                             nChans*nRows, NULL, data, NULL, &fitsStatus),
               "Error reading spectra from Binary Column DATA" );

  if (*status == SAI__OK) {

    /* It looks like there is a bad value of (-1)*VAL__BADI */
    size_t i;
    size_t nelems = nChans * nRows;
    for (i = 0; i < nelems; i++ ) {
      if (data[i] >= (float)(VAL__MAXI - 1)) {
        data[i] = VAL__BADR;
      }
    }

    *numChans = nChans;
    *numRows = nRows;
  }

  return data;
}
