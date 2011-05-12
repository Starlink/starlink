/*+
 *  Name:
 *     hdrCopy

 *  Purpose:
 *    Copy header information from one image to another.

 *  Language:
 *     ANSI C

 *  Invocation:
 *     hdrCopy( param1, xname1, param2, xname2, status );

 *  Description:
 *     This C function sets up the required arguments and calls the
 *     Fortran subroutine hdr_copy.

 *  Arguments:
 *     param1 = char * (Given)
 *        Parameter name of the image containing the input source of
 *        header information (case insensitive).
 *     xname1 = char * (Given)
 *        The name of the extension to be copied ("FITS" or " " for FITS).
 *     param2 = char * (Given)
 *        Parameter name of the image that you want to copy a header
 *        source into (case insensitive).
 *     xname2 = char * (Given)
 *        The name of the destination header source ("FITS" or " " for
 *        FITS, must be FITS if xname1 is FITS).
 *     status = int * (Given and Returned)
 *        The global status. If a header source or destination is FITS
 *        and the other isn't then IMG__BDEXT will be returned.

*  Copyright:
*     Copyright (C) 2000 Central Laboratory of the Research Councils.
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
 *     FCWRAP: Automatic fortran wrapper script.
 *     PDRAPER: Peter Draper (STARLINK - Durham University)
 *     {enter_new_authors_here}

 *  History:
 *     24-OCT-2000 (FCWRAP):
 *        Original version.
 *     24-OCT-2000 (PDRAPER):
 *        Added prologue.
 *     {enter_further_changes_here}
 *-
 */
#include <string.h>
#include "f77.h"
#include "img.h"
F77_SUBROUTINE(hdr_copy)( CHARACTER(param1),
                          CHARACTER(xname1),
                          CHARACTER(param2),
                          CHARACTER(xname2),
                          INTEGER(status)
                          TRAIL(param1)
                          TRAIL(xname1)
                          TRAIL(param2)
                          TRAIL(xname2) );

void hdrCopy( char *param1,
              char *xname1,
              char *param2,
              char *xname2,
              int *status ) {

    DECLARE_CHARACTER_DYN(fparam1);
    DECLARE_CHARACTER_DYN(fxname1);
    DECLARE_CHARACTER_DYN(fparam2);
    DECLARE_CHARACTER_DYN(fxname2);
    DECLARE_INTEGER(fstatus);

    F77_CREATE_CHARACTER(fparam1,strlen( param1 ));
    F77_EXPORT_CHARACTER(param1,fparam1,fparam1_length);
    F77_CREATE_CHARACTER(fxname1,strlen( xname1 ));
    F77_EXPORT_CHARACTER(xname1,fxname1,fxname1_length);
    F77_CREATE_CHARACTER(fparam2,strlen( param2 ));
    F77_EXPORT_CHARACTER(param2,fparam2,fparam2_length);
    F77_CREATE_CHARACTER(fxname2,strlen( xname2 ));
    F77_EXPORT_CHARACTER(xname2,fxname2,fxname2_length);
    F77_EXPORT_INTEGER(*status,fstatus);

    F77_LOCK( F77_CALL(hdr_copy)( CHARACTER_ARG(fparam1),
                        CHARACTER_ARG(fxname1),
                        CHARACTER_ARG(fparam2),
                        CHARACTER_ARG(fxname2),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(fparam1)
                        TRAIL_ARG(fxname1)
                        TRAIL_ARG(fparam2)
                        TRAIL_ARG(fxname2) ); )

    F77_FREE_CHARACTER(fparam1);
    F77_FREE_CHARACTER(fxname1);
    F77_FREE_CHARACTER(fparam2);
    F77_FREE_CHARACTER(fxname2);
    F77_IMPORT_INTEGER(fstatus,*status);

    return;
}
/* $Id$ */
