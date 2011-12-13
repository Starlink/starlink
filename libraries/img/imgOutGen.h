/*
 *+
 *  Name:
 *     imgOutGen

 *  Purpose:
 *     Creates an output image with a specific type.

 *  Language:
 *     ANSI C

 *  Invocation:
 *     imgOut?( param1, param2, ip, status )

 *  Description:
 *     This C function sets up the required arguments and calls the
 *     Fortran subroutine img_out[x].
 *     On return, values are converted back to C form if necessary.
 *
 *     This version is the generic form for the float, double, int,
 *     short, unsigned short, char and unsigned char versions. Just
 *     include this in the appropriate stub after setting the
 *     values of the macros:
 *
 *        IMG_F77_TYPE   = (r|d|l|i|w|uw|b|ub)
 *        IMG_FULL_C_TYPE   = (float|double|short etc.)
 *        IMG_SHORT_C_TYPE   = (F|D|I|S|US|B|UB)
 *
 *     The IMG_F77_TYPE essentially names the fortran version of this
 *     routine to invoke.

 *  Arguments:
 *     param1 = char * (Given)
 *        Parameter name for the input image (case insensitive).
 *     param2 = char * (Given)
 *        Parameter name for the output image (case insensitive).
 *     ip = ? ** (Returned)
 *        Pointer to the mapped output data.
 *     status = int * (Given and Returned)
 *        The global status.

*  Copyright:
*     Copyright (C) 1996, 1999 Central Laboratory of the Research Councils.
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
 *     The orginal version was generated automatically from the
 *     Fortran source of img_out by the Perl script fcwrap.
 *     PDRAPER: Peter W. Draper (STARLINK - Durham University)
 *     {enter_new_authors_here}

 *  History:
 *     17-May-1996 (fcwrap):
 *        Original version
 *     24-May-1996 (PDRAPER):
 *        Added code to handle pointer arrays correctly. Made into
 *        generic include file.
 *     10-JUN-1996 (PDRAPER):
 *        Converted to use more C-like names.
 *     21-APR-1999 (PDRAPER):
 *        Changed to export pointer array using CNF macros (64 bit
 *        changes).
 *     {enter_changes_here}
 *-
 */

#include <string.h>
#include "f77.h"
#include "cnf.h"

IMG_OUT( IMG_F77_TYPE )( CHARACTER(param1),
                         CHARACTER(param2),
                         POINTER_ARRAY(ip),
                         INTEGER(status)
                         TRAIL(param1)
                         TRAIL(param2) );

IMGOUT( IMG_SHORT_C_TYPE ) ( char *param1,
                             char *param2,
                             IMG_FULL_C_TYPE **ip,
                             int *status ) {

   DECLARE_CHARACTER_DYN( fparam1 );
   DECLARE_CHARACTER_DYN( fparam2 );
   DECLARE_POINTER_ARRAY_DYN( fip );
   int nparam;

   /*  Count the number of output parameters and create enough space for
       the corresponding Fortran pointers */
   nparam = img1CountParams( param2, status );
   F77_CREATE_POINTER_ARRAY( fip, nparam );
   F77_ASSOC_POINTER_ARRAY( fip, ip );

   F77_CREATE_CHARACTER( fparam1, strlen( param1 ) );
   F77_EXPORT_CHARACTER( param1, fparam1, fparam1_length );
   F77_CREATE_CHARACTER( fparam2, strlen( param2 ) );
   F77_EXPORT_CHARACTER( param2, fparam2, fparam2_length );

   IMGOUT_CALL( IMG_F77_TYPE ) ( CHARACTER_ARG(fparam1),
                                 CHARACTER_ARG(fparam2),
                                 POINTER_ARRAY_ARG(fip),
                                 INTEGER_ARG(status)
                                 TRAIL_ARG(fparam1)
                                 TRAIL_ARG(fparam2) );

   /*  Now copy the addresses back to to C pointers */
   F77_IMPORT_POINTER_ARRAY( fip, ip, nparam );

   F77_FREE_POINTER( fip );
   F77_FREE_CHARACTER(fparam1);
   F77_FREE_CHARACTER(fparam2);

   return;
}
/* $Id$ */
