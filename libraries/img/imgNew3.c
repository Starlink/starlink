/*
 *+
 *  Name:
 *     imgNew3

 *  Purpose:
 *     Defines default routines that create a new 2-D image.

 *  Language:
 *     ANSI C

 *  Invocation:
 *     imgNew3[x]( param, nx, ny, nz, ip, status )

 *  Description:
 *     This routine creates all the imgNew[x] routines from the
 *     generic stubs.

 *  Arguments:
 *     param = char * (Given)
 *        Parameter name. (case insensitive).
 *     nx = int * (Given)
 *        Size of first dimension of the image (in pixels).
 *     ny = int * (Given)
 *        Size of second dimension of the image (in pixels).
 *     nz = int * (Given)
 *        Size of third dimension of the image (in pixels).
 *     ip = ? ** (Returned)
 *        Pointer to image data.
 *     status = int * (Given and Returned)
 *        The global status.

*  Copyright:
*     Copyright (C) 1996 Central Laboratory of the Research Councils.
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
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

 *  Authors:
 *     PDRAPER: Peter W. Draper (STARLINK - Durham University)
 *     {enter_new_authors_here}

 *  History:
 *     28-May-1996 (PDRAPER):
 *        Original version
 *     10-JUN-1996 (PDRAPER):
 *        Converted to use more C-like names.
 *     {enter_changes_here}

 *-
 */
#include <string.h>
#include <stdlib.h>
#include "cnf.h"
#include "f77.h"
#include "img1.h"

/*  Define the various names of the subroutines. Note we use two
    macros that join the parts to the type because of use of ##
    needs to be deferred a while!
    */

#define XIMG_NEW3(type)  F77_SUBROUTINE(img_new3 ## type)
#define IMG_NEW3(type)   XIMG_NEW3(type)

#define XIMGNEW3(type)  void imgNew3 ## type
#define IMGNEW3(type)   XIMGNEW3(type)

#define XIMGNEW3_CALL(type)  F77_CALL(img_new3 ## type)
#define IMGNEW3_CALL(type)   XIMGNEW3_CALL(type)

/*  Define the macros for each of the data types for each of the
    modules, then include the generic code to create the actual
    modules. */

/*  Default type information */
#define IMG_F77_TYPE
#define IMG_SHORT_C_TYPE
#define IMG_FULL_C_TYPE float
#include "imgNew3Gen.h"

/*  Byte */
#undef IMG_F77_TYPE
#undef IMG_SHORT_C_TYPE
#undef IMG_FULL_C_TYPE

#define IMG_F77_TYPE b
#define IMG_SHORT_C_TYPE B
#define IMG_FULL_C_TYPE signed char
#include "imgNew3Gen.h"

/*  Unsigned Byte */
#undef IMG_F77_TYPE
#undef IMG_SHORT_C_TYPE
#undef IMG_FULL_C_TYPE

#define IMG_F77_TYPE ub
#define IMG_SHORT_C_TYPE UB
#define IMG_FULL_C_TYPE unsigned char
#include "imgNew3Gen.h"

/*  Word */
#undef IMG_F77_TYPE
#undef IMG_SHORT_C_TYPE
#undef IMG_FULL_C_TYPE

#define IMG_F77_TYPE w
#define IMG_SHORT_C_TYPE S
#define IMG_FULL_C_TYPE short int
#include "imgNew3Gen.h"

/*  Unsigned word */
#undef IMG_F77_TYPE
#undef IMG_SHORT_C_TYPE
#undef IMG_FULL_C_TYPE

#define IMG_F77_TYPE uw
#define IMG_SHORT_C_TYPE US
#define IMG_FULL_C_TYPE unsigned short
#include "imgNew3Gen.h"

/*  Integer */
#undef IMG_F77_TYPE
#undef IMG_SHORT_C_TYPE
#undef IMG_FULL_C_TYPE

#define IMG_F77_TYPE i
#define IMG_SHORT_C_TYPE I
#define IMG_FULL_C_TYPE int
#include "imgNew3Gen.h"

/*  Real */
#undef IMG_F77_TYPE
#undef IMG_SHORT_C_TYPE
#undef IMG_FULL_C_TYPE

#define IMG_F77_TYPE r
#define IMG_SHORT_C_TYPE F
#define IMG_FULL_C_TYPE float
#include "imgNew3Gen.h"

/*  Double precision */
#undef IMG_F77_TYPE
#undef IMG_SHORT_C_TYPE
#undef IMG_FULL_C_TYPE

#define IMG_F77_TYPE d
#define IMG_SHORT_C_TYPE D
#define IMG_FULL_C_TYPE double
#include "imgNew3Gen.h"

/* $Id$ */
