/*
*+
*  Name:
*     smfftsio_table_to_fitschan

*  Purpose:
*     Read row from table and put into fitschan

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     smfftsio_table_to_fitschan( fitsfile *fptr, size_t rownum, AstFitsChan * fitschan, int * status );

*  Arguments:
*     fptr = fitsfile * (Given)
*        CFITSIO pointer
*     rownum = size_t (Given)
*        Row number. First row is row 1. Last row can be specified as Row 0).
*     fitschan = AstFitsChan * (Given and Returned)
*        FitsChan to be populated from the FITS file.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Fills a FitsChan with all the scalar items from a specified row in the currently
*     selected FITS binary table.


*  Authors:
*     TIMJ: Tim Jenness (Cornell)
*     {enter_new_authors_here}

*  Notes:
*

*  History:
*     2014-04-08 (TIMJ):
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
#include "fitsio.h"

#include "sae_par.h"
#include "star/one.h"
#include "star/atl.h"
#include "ast.h"

void
smfftsio_table_to_fitschan( fitsfile *fptr, size_t rownum, AstFitsChan * fitschan, int * status ) {

  int fitsStatus = 0; /* CFITSIO status */
  long nRows = 0;
  int nCols = 0;
  int col;

  if (*status != SAI__OK) return;

  /* Prepare to read the Binary Table by first getting the row count */
  CALLCFITSIO( fits_get_num_rows(fptr, &nRows, &fitsStatus), "Error getting number of rows" );

  if ( (long)rownum > nRows) {
    if (*status == SAI__OK) {
      *status = SAI__ERROR;
      errRepf("", "Requested row number (%zu) exceeds number of rows in file (%zu)",
              status, rownum, (size_t)nRows );
    }
  }
  if (rownum == 0) rownum = nRows;

  /* ...and the column count */
  CALLCFITSIO( fits_get_num_cols(fptr, &nCols, &fitsStatus), "Error getting number of columns" );

  /* Read an element from each column */
  for (col=1; col<= nCols; col++) {
    /* To get the column name we need to convert the column number to a string */
    char templt[10];
    char colname[32];
    int colnum;
    int typecode;
    long repeat = 0;
    long width = 0;

    /* Column information */
    one_snprintf(templt, sizeof(templt), "%d", status, col );
    CALLCFITSIO( fits_get_colname( fptr, CASEINSEN, templt, colname, &colnum, &fitsStatus), "Error getting column name");
    CALLCFITSIO( fits_get_eqcoltype( fptr, col, &typecode, &repeat, &width, &fitsStatus), "Error getting column data type");

    /* Can not store vectors in FitsChan but strings are special */
    if (repeat > 1 && typecode != TSTRING) continue;

    switch (typecode) {
      int ivalue[1];
      double dvalue[1];
      char * svalue;
      char * stemp[1];
      char nullarray[1];
      int anynul[1];
      int usedtype;
    case TINT:
    case TLONG:
    case TULONG:
    case TBYTE:
    case TSHORT:
    case TUSHORT:
    case TUINT:
      usedtype = TINT;
      CALLCFITSIO(fits_read_colnull( fptr, usedtype, col, rownum, 1, 1,
                                     ivalue, nullarray, anynul, &fitsStatus ), "Error reading int column");
      if ( nullarray[0] == 1 ) {
        astSetFitsU( fitschan, colname, "", 0 );
      } else {
        atlPtfti( fitschan, colname, ivalue[0], "", status );
      }
      break;

    case TFLOAT:
    case TDOUBLE:
      usedtype = TDOUBLE;
      CALLCFITSIO(fits_read_colnull( fptr, usedtype, col, rownum, 1, 1,
                                     dvalue, nullarray, anynul, &fitsStatus ), "Error reading float column");
      if ( nullarray[0] == 1 ) {
        astSetFitsU( fitschan, colname, "", 0 );
      } else {
        atlPtftd( fitschan, colname, dvalue[0], "", status );
      }
      break;

    case TSTRING:
      /* allocate buffer for the string column */
      svalue = astMalloc( 1 + (sizeof(*svalue) * width * repeat));
      svalue[0] = '\0';
      stemp[0] = svalue;
      CALLCFITSIO(fits_read_colnull( fptr, typecode, col, rownum, 1, 1,
                                     stemp, nullarray, anynul, &fitsStatus ),
                  "Error reading float column");
      if ( nullarray[0] == 1 ) {
        astSetFitsU( fitschan, colname, "", 0 );
      } else {
        atlPtfts( fitschan, colname, svalue, "", status );
      }
      svalue = astFree(svalue);
      break;

    default:
      if (*status == SAI__OK) {
        *status = SAI__ERROR;
        errRepf("", "Unhandled FITS table data type: %d", status, typecode);
      }

    }

  }

}
