#if !defined( CVG_INCLUDED )  /* Include this file only once */
#define CVG_INCLUDED
/*
*  Name:
*     cvg.h

*  Purpose:
*     Define the C interface to the CVG library.

*  Description:
*     This module defines the C interface to the functions of the CVG
*     library. The file cvg.c contains C wrappers for the Fortran
*     CVG routines.

*  Authors:
*     DSB: David S. Berry (JAC)

*  History:
*     14-NOV-2013 (DSB):
*        Original version.

*  Copyright:
*     Copyright (C) 2013 Science and Technology Facilities Council.
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
*     Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
*     MA 02110-1301, USA

*/

#include "mers.h"
#include "star/ndg.h"
#include "fitsio.h"

/* Public Constants */
/* ---------------- */

/* An illegal Fortran logical unit number. */
#define CVG__NOLUN -999

/* Maximum length of a file path. */
#define CVG__MXPTH 255

/* Private macros */
/* ============== */

#define CVG_EXPORT_FITS(fptr,funit) \
   if( fptr ) { \
      funit = CFITS2Unit( fptr ); \
      if( funit == 0 && *status == SAI__OK ) { \
         *status = SAI__ERROR; \
         errRep( " ", "Failed to export a fitsfile pointer", status ); \
      } \
   } else { \
      funit = CVG__NOLUN; \
   }

#define CVG_IMPORT_FITS(funit,fptr) \
   if( funit != CVG__NOLUN ) { \
      fptr = CUnit2FITS( funit ); \
      if( fptr == 0 && *status == SAI__OK ) { \
         *status = SAI__ERROR; \
         errRep( " ", "Failed to import a fitsfile pointer", status ); \
      } \
   } else { \
      fptr = NULL; \
   }

/* Public function prototypes */
/* -------------------------- */
void cvgAssoc( const char *param, const char *mode, fitsfile **fptr, int *blockf, int *status );
void cvgClean( AstFitsChan *fc, int *status );
void cvgClose( fitsfile **fptr, int *status );
void cvgCreat( const char *param, int blockf, int ovrwrt, fitsfile **fptr, int *status );
void cvgFt2bt( AstFitsTable *table, fitsfile *fptr, const char *extnam, int astver, int mkchdu, int *status );
void cvgNew( const char *path, int blockf, int ovrwrt, fitsfile **fptr, int *status );
void cvgPcadc( NdgProvenance *prov, fitsfile *fptr, int *status );
void cvgShowHeader( fitsfile *fptr, int all, int *status );
void cvgWhisr( int ndf, fitsfile *fptr, int *status );
void cvgOpen( const char *path, const char *mode, fitsfile **fptr, int *blockf, int *status );
void cvgFc2hd( AstFitsChan *fc, int clear, fitsfile *fptr, int *status );
void cvgHd2fc( fitsfile *fptr, AstFitsChan *fc, int *status );
void cvgBt2ft( fitsfile *fptr, const char *extnam, int extver, int extlevel, AstFitsTable **table, int *status );

#endif
