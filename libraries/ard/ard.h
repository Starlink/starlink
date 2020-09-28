#if !defined( ARD_INCLUDED )  /* Include this file only once */
#define ARD_INCLUDED
/*
*+
*  Name:
*     ard.h

*  Purpose:
*     Define the C interface to the ARD library.

*  Description:
*     This module defines the C interface to the functions of the ARD
*     library. The file ard.c contains C wrappers for the Fortran
*     ARD routines.

*  Authors:
*     PWD: Peter W. Draper (JAC, Durham University)

*  History:
*     07-JUL-2006 (DSB):
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
*     Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
*     MA 02110-1301, USA

*-
*/

#include "star/grp.h"

/* Note these use Grp pointers, not integer identifiers. */

void ardWork( const Grp *grp,
              int ndim,
              const int *lbnd,
              const int *ubnd,
              const float *trcoef,
              int concat,
              int *regval,
              int *mask,
              int *lbndi,
              int *ubndi,
              int *lbnde,
              int *ubnde,
              int *status );

void ardWork8( const Grp *grp,
              int ndim,
              const int64_t *lbnd,
              const int64_t *ubnd,
              const float *trcoef,
              int concat,
              int *regval,
              int *mask,
              int64_t *lbndi,
              int64_t *ubndi,
              int64_t *lbnde,
              int64_t *ubnde,
              int *status );

void ardGrpex( const char *desc,
               const Grp *grp1,
               Grp **grp2,
               int *flag,
               int *status );

#endif /* !defined( ARD_INCLUDED ) */
