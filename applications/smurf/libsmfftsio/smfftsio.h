/*
*+
*  Name:
*     smfftsio.h

*  Purpose:
*     Include file defining SMURF CFITSIO helper routines

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Header file

*  Authors:
*     TIMJ: Tim Jenness (Cornell)
*     {enter_new_authors_here}

*  History:
*     2014-04-01 (TIMJ):
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

#ifndef SMFFITSIO_DEFINED
#define SMFFITSIO_DEFINED

#include "mers.h"
#include "ast.h"
#include "fitsio.h"

/* If we're not using GNU C, elide __attribute__ */
#ifndef __GNUC__
#  define  __attribute__(x)  /*NOTHING*/
#endif


/* API */

float *
smfftsio_read_data( fitsfile * fits, size_t * numRows, size_t * numChans, int * status );

void
smfftsio_read_fitshdr( fitsfile *fptr, AstFitsChan * fitschan, int * status );

void
smfftsio_table_to_fitschan( fitsfile *fptr, size_t rownum, AstFitsChan * fitschan, int * status );

/* Macro to wrap calls to cfitsio so that Starlink error status is handled.
   Assumes that Starlink is *status and FITS is fitsStatus.
   Will add messages from the CFITSIO message stack.
   If errmsg=NULL no additional message will be added.
 */
#define CALLCFITSIO(X, errmsg )                                         \
  if (*status == SAI__OK) {                                             \
    X;                                                                  \
    if ( fitsStatus != 0 ) {                                            \
      char xxerrmsg[FLEN_ERRMSG];                                       \
      *status = SAI__ERROR;                                             \
      fits_get_errstatus( fitsStatus, xxerrmsg );                       \
      errRepf( "", "FITSIO: %d -- %s", status, fitsStatus, xxerrmsg );  \
      while ( fits_read_errmsg(xxerrmsg) ) errRepf("", "FITSIO: %s", status, xxerrmsg ); \
      if (errmsg) errRep( "", errmsg, status );                         \
    }                                                                   \
  }


#endif
