/*
*+
*  Name:
*     supcam_read_tabmetadata

*  Purpose:
*     Read important metadata from binary table in SDFITS

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     hdr = supcam_read_tabmetadata( fitsfile * fits, size_t maxRows, int * status );

*  Arguments:
*     fits = fitsfile * (Given)
*        CFITSIO pointer to open file.
*     maxRows = size_t (Given)
*        Number of expected rows.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Read the per-spectra information from the relevant binary table columns
*     and store it in an array of structs of type SupercamSpecHdr.

*  Returned Value:
*     SuperCamHdr *
*        Array of maxRows structs. Returns NULL on error.

*  Authors:
*     TIMJ: Tim Jenness (Cornell)
*     {enter_new_authors_here}

*  Notes:
*     - Returned buffer must be freed with astFree().
*     - See also ACSISSpecHdr struct in specwrite.h

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

SupercamSpecHdr *
supcam_read_tabmetadata( fitsfile * fits, size_t maxRows, int * status ) {

  int colnum = 0;    /* DATA column */
  int fitsStatus = 0; /* CFITSIO status */
  long nRows = 0;

  SupercamSpecHdr * hdr = NULL;
  double * coldata = NULL;

  if (*status != SAI__OK) return NULL;

  /* Prepare to read the Binary Table */
  /* Check number of rows */
  CALLCFITSIO( fits_get_num_rows(fits, &nRows, &fitsStatus), "Error getting number of rows" );

  if (*status == SAI__OK && (size_t)nRows != maxRows) {
    *status = SAI__ERROR;
    errRepf("", "Found %ld rows but expected %zu", status, nRows, maxRows);
    return NULL;
  }

  /* Allocate some structs to receive the column information
     but we also need somewhere to store a single column
     (and they are all doubles). */
  coldata = astMalloc( maxRows * sizeof(*coldata) );
  hdr = astMalloc( maxRows * sizeof(*hdr) );

/* This code is repetitive so use a macro */
#define READCOLUMN(COLUMN,STRUCT)                                       \
  /* Find relevant column */                                            \
  /* cast because cfitsio does not use const */                         \
  CALLCFITSIO( fits_get_colnum(fits, CASEINSEN, (char *)COLUMN, &colnum, &fitsStatus), \
               "Error getting number of column " COLUMN " in table" );  \
                                                                        \
  /* Read column */                                                     \
  CALLCFITSIO( fits_read_col(fits, TDOUBLE, colnum, 1, 1,               \
                             nRows, NULL, coldata, NULL, &fitsStatus),  \
               "Error reading spectra from Binary table column " COLUMN );    \
                                                                        \
  if (*status == SAI__OK) {                                             \
    /* Copy from column to struct */                                    \
    size_t i;                                                           \
    for (i = 0; i < maxRows; i++ ) {                                    \
      hdr[i].STRUCT = coldata[i];                                       \
    }                                                                   \
  }

  #pragma GCC diagnostic ignored "-Wcast-qual"                          \
  READCOLUMN("CDELT2",offx);
  READCOLUMN("CDELT3",offy);
  READCOLUMN("TSYS",tsys);
  READCOLUMN("TRX",trx);
  READCOLUMN("INTTIME",inttime);
  READCOLUMN("IFPOWER",ifpower);
  #pragma GCC diagnostic pop

  /* Fill in the receptor name */
  if (*status == SAI__OK) {
    size_t i;
    for (i=0; i < maxRows; i++) {
      hdr[i].recepname = supcam_beam_num_to_name( i+1, status );
    }
  }

  /* Clean up */
  coldata = astFree( coldata );

  if (*status != SAI__OK) {
    hdr = astFree(hdr);
  }

  return hdr;
}
