#if !defined( ATL_INCLUDED )  /* Include this file only once */
#define ATL_INCLUDED
/*
*  Name:
*     atl.h

*  Purpose:
*     Define the C interface to the ATL library.

*  Description:
*     This module defines the C interface to the functions of the ATL
*     library. The file atl.c contains C wrappers for the Fortran 
*     ATL routines.

*  Authors:
*     DSB: David S. Berry (JAC)

*  History:
*     26-MAY-2006 (DSB):
*        Original version.

*  Copyright:
*     Copyright (C) 2006 Particle Physics and Astronomy Research Council.
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

*/

#include "f77.h"
#include "ast.h"

/* Public Constants */
/* ---------------- */

/* Maximum number of axes to consider. This should be equal to 
   NDF__MXDIM. */
enum { ATL__MXDIM  = 7 };


/* Public function prototypes */
/* -------------------------- */
void atlAxtrm( AstFrameSet *, int *, int *, int *, double *, int * );
void atlMklut( int, int, int, int, AstFrame *, double *, AstMapping **, int * );

#endif
