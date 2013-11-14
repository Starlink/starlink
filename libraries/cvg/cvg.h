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

#include "star/ndg.h"

/* Public Constants */
/* ---------------- */

/* An illegal Fortran logical unit number. */
#define CVG__NOLUN -999

/* Maximum length of a file path. */
#define CVG__MXPTH 255

/* Public function prototypes */
/* -------------------------- */
void cvgClose( int *funit, int *status );
void cvgCreat( const char *param, int blockf, int ovrwrt, int *funit, int *status );
void cvgFt2bt( AstFitsTable *table, int funit, const char *extnam, int astver, int *status );
void cvgNew( const char *path, int blockf, int ovrwrt, int *funit, int *status );
void cvgScadc( NdgProvenance *prov, const char *param, int *status );

#endif
