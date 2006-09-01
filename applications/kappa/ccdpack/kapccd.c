/*
*+
*  Name:
*     kapccd.c

*  Purpose:
*     Implement the C interface to the standalone CCDPACK routines used
*     by KAPPA.

*  Description:
*     This module implements C-callable wrappers for the routines in 
*     kappa.ccdpack. These are Fortran routines that were originally
*     written as part of CCDPACK but are now part of the KAPPA source 
*     distribution. The interface to these wrappers is defined in kapccd.h.

*  Notes:
*     - Given the number of routines, providing a complete C interface is 
*     probably not worth the effort. Instead, I suggest that people who 
*     want to use kapccd from C extend this file (and kapccd.h) to include 
*     any functions which they need but which are not already included.

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
*     DSB: David S Berry
*     {enter_new_authors_here}

*  History:
*     1-SEP-2006 (DSB):
*        Original version.
*     {enter_further_changes_here}

*-
*/

/* Header files. */
/* ============= */
#include "f77.h"
#include "sae_par.h"
#include "kapccd.h"

/* Wrapper function implementations. */
/* ================================= */
F77_SUBROUTINE(ccg1_cm3dd)( DOUBLE_ARRAY(STACK), 
                            INTEGER(NPIX), 
                            INTEGER(NLINES), 
                            DOUBLE_ARRAY(VARS), 
                            DOUBLE_ARRAY(COORDS), 
                            DOUBLE_ARRAY(WIDTHS),
                            INTEGER(IMETH), 
                            INTEGER(MINPIX), 
                            INTEGER(NITER), 
                            REAL(NSIGMA), 
                            REAL(ALPHA), 
                            REAL(RMIN),
                            REAL(RMAX), 
                            DOUBLE_ARRAY(RESULT), 
                            INTEGER_ARRAY(COIND), 
                            DOUBLE_ARRAY(WRK1), 
                            DOUBLE_ARRAY(WRK2), 
                            DOUBLE_ARRAY(NCON),
                            INTEGER_ARRAY(POINT), 
                            LOGICAL_ARRAY(USED), 
                            INTEGER(STATUS) );

void ccg1_cm3dd( double *stack, int npix, int nlines, double *vars, 
                 double *coords, double *widths, int imeth, int minpix, 
                 int niter, float nsigma, float alpha, float rmin, float rmax, 
                 double *result, int *coind, double *wrk1, double *wrk2, 
                 double *ncon, int *point, int *used, int *status ){

   DECLARE_DOUBLE_ARRAY_DYN(STACK); 
   DECLARE_INTEGER(NPIX); 
   DECLARE_INTEGER(NLINES); 
   DECLARE_DOUBLE_ARRAY_DYN(VARS); 
   DECLARE_DOUBLE_ARRAY_DYN(COORDS); 
   DECLARE_DOUBLE_ARRAY_DYN(WIDTHS);
   DECLARE_INTEGER(IMETH); 
   DECLARE_INTEGER(MINPIX); 
   DECLARE_INTEGER(NITER); 
   DECLARE_REAL(NSIGMA); 
   DECLARE_REAL(ALPHA); 
   DECLARE_REAL(RMIN);
   DECLARE_REAL(RMAX); 
   DECLARE_DOUBLE_ARRAY_DYN(RESULT); 
   DECLARE_INTEGER_ARRAY_DYN(COIND); 
   DECLARE_DOUBLE_ARRAY_DYN(WRK1); 
   DECLARE_DOUBLE_ARRAY_DYN(WRK2); 
   DECLARE_DOUBLE_ARRAY_DYN(NCON);
   DECLARE_INTEGER_ARRAY_DYN(POINT); 
   DECLARE_LOGICAL_ARRAY_DYN(USED); 
   DECLARE_INTEGER(STATUS);
   int nel;

   nel = npix*nlines;

   F77_CREATE_DOUBLE_ARRAY(STACK, nel );
   F77_CREATE_DOUBLE_ARRAY(VARS, nlines ); 
   F77_CREATE_DOUBLE_ARRAY(COORDS, nel ); 
   F77_CREATE_DOUBLE_ARRAY(WIDTHS, nel );
   F77_CREATE_DOUBLE_ARRAY(RESULT, npix ); 
   F77_CREATE_INTEGER_ARRAY(COIND, npix ); 
   F77_CREATE_DOUBLE_ARRAY(WRK1, nlines ); 
   F77_CREATE_DOUBLE_ARRAY(WRK2, nlines ); 
   F77_CREATE_DOUBLE_ARRAY(NCON, nlines );
   F77_CREATE_INTEGER_ARRAY(POINT, nlines ); 
   F77_CREATE_LOGICAL_ARRAY(USED, nlines ); 

   F77_EXPORT_DOUBLE_ARRAY(stack, STACK, nel ); 
   F77_EXPORT_INTEGER(npix, NPIX); 
   F77_EXPORT_INTEGER(nlines, NLINES); 
   F77_EXPORT_DOUBLE_ARRAY(vars, VARS, nlines ); 
   F77_EXPORT_DOUBLE_ARRAY(coords, COORDS, nel ); 
   F77_EXPORT_DOUBLE_ARRAY(widths, WIDTHS, nel );
   F77_EXPORT_INTEGER(imeth, IMETH); 
   F77_EXPORT_INTEGER(minpix, MINPIX); 
   F77_EXPORT_INTEGER(niter, NITER); 
   F77_EXPORT_REAL(nsigma, NSIGMA); 
   F77_EXPORT_REAL(alpha, ALPHA); 
   F77_EXPORT_REAL(rmin, RMIN);
   F77_EXPORT_REAL(rmax, RMAX); 
   F77_ASSOC_DOUBLE_ARRAY(result, RESULT ); 
   F77_ASSOC_INTEGER_ARRAY(coind, COIND ); 
   F77_ASSOC_DOUBLE_ARRAY(wrk1, WRK1 ); 
   F77_ASSOC_DOUBLE_ARRAY(wrk2, WRK2 ); 
   F77_ASSOC_DOUBLE_ARRAY(ncon, NCON );
   F77_ASSOC_INTEGER_ARRAY(point, POINT ); 
   F77_ASSOC_LOGICAL_ARRAY(used, USED ); 
   F77_EXPORT_INTEGER(*status, STATUS);

   F77_CALL(ccg1_cm3dd)( DOUBLE_ARRAY_ARG(STACK), 
                         INTEGER_ARG(&NPIX), 
                         INTEGER_ARG(&NLINES), 
                         DOUBLE_ARRAY_ARG(VARS), 
                         DOUBLE_ARRAY_ARG(COORDS), 
                         DOUBLE_ARRAY_ARG(WIDTHS),
                         INTEGER_ARG(&IMETH), 
                         INTEGER_ARG(&MINPIX), 
                         INTEGER_ARG(&NITER), 
                         REAL_ARG(&NSIGMA), 
                         REAL_ARG(&ALPHA), 
                         REAL_ARG(&RMIN),
                         REAL_ARG(&RMAX), 
                         DOUBLE_ARRAY_ARG(RESULT), 
                         INTEGER_ARRAY_ARG(COIND), 
                         DOUBLE_ARRAY_ARG(WRK1), 
                         DOUBLE_ARRAY_ARG(WRK2), 
                         DOUBLE_ARRAY_ARG(NCON),
                         INTEGER_ARRAY_ARG(POINT), 
                         LOGICAL_ARRAY_ARG(USED), 
                         INTEGER_ARG(&STATUS) );

   F77_IMPORT_INTEGER(STATUS, *status);
   F77_IMPORT_DOUBLE_ARRAY(STACK, stack, nel ); 
   F77_IMPORT_INTEGER(NPIX, npix); 
   F77_IMPORT_INTEGER(NLINES, nlines); 
   F77_IMPORT_DOUBLE_ARRAY(VARS, vars, nlines ); 
   F77_IMPORT_DOUBLE_ARRAY(COORDS, coords, nel ); 
   F77_IMPORT_DOUBLE_ARRAY(WIDTHS, widths, nel );
   F77_IMPORT_INTEGER(IMETH, imeth); 
   F77_IMPORT_INTEGER(MINPIX, minpix); 
   F77_IMPORT_INTEGER(NITER, niter); 
   F77_IMPORT_REAL(NSIGMA, nsigma); 
   F77_IMPORT_REAL(ALPHA, alpha); 
   F77_IMPORT_REAL(RMIN, rmin);
   F77_IMPORT_REAL(RMAX, rmax); 
   F77_IMPORT_DOUBLE_ARRAY(RESULT, result, npix ); 
   F77_IMPORT_INTEGER_ARRAY(COIND, coind, npix ); 
   F77_IMPORT_DOUBLE_ARRAY(WRK1, wrk1, nlines ); 
   F77_IMPORT_DOUBLE_ARRAY(WRK2, wrk2, nlines ); 
   F77_IMPORT_DOUBLE_ARRAY(NCON, ncon, nlines );
   F77_IMPORT_INTEGER_ARRAY(POINT, point, nlines ); 
   F77_IMPORT_LOGICAL_ARRAY(USED, used, nlines ); 


   F77_FREE_DOUBLE(STACK);
   F77_FREE_DOUBLE(VARS); 
   F77_FREE_DOUBLE(COORDS); 
   F77_FREE_DOUBLE(WIDTHS);
   F77_FREE_DOUBLE(RESULT); 
   F77_FREE_INTEGER(COIND); 
   F77_FREE_DOUBLE(WRK1); 
   F77_FREE_DOUBLE(WRK2); 
   F77_FREE_DOUBLE(NCON);
   F77_FREE_INTEGER(POINT); 
   F77_FREE_LOGICAL(USED); 
}

/* ------------------------------- */

