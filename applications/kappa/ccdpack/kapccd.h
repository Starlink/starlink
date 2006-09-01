#if !defined( KAPCCD_INCLUDED )  /* Include this file only once */
#define KAPCCD_INCLUDED
/*
*+
*  Name:
*     kapccd.h

*  Purpose:
*     Defines the C interface to the standalone CCDPACK routines used
*     by KAPPA.

*  Description:
*     This module defines the C interface to the wrappers for the F77
*     routines in kappa.ccdpack. These are Fortran routines that were 
*     originally written as part of CCDPACK but are now part of the KAPPA 
*     source distribution. 

*  Copyright:
*     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*     
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*     
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     DSB: David .S. Berry

*  History:
*     1-SEP-2006 (DSB):
*        Original version.

*-
*/


/* Prototypes for public functions */

void ccg1_cm3dd( double *stack, int npix, int nlines, double *vars, 
                 double *coords, double *widths, int imeth, int minpix, 
                 int niter, float nsigma, float alpha, float rmin, float rmax, 
                 double *result, int *coind, double *wrk1, double *wrk2, 
                 double *ncon, int *point, int *used, int *status );

#endif
