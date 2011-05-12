/*
 *+
 *  Name:
 *     imgName

 *  Purpose:
 *     Return the image name.

 *  Language:
 *     ANSI C

 *  Invocation:
 *     imgName( param, value, value_length, status )

 *  Description:
 *     This C function sets up the required arguments and calls the
 *     Fortran subroutine img_name.
 *     On return, values are converted back to C form if necessary.

 *  Arguments:
 *     param = char * (Given)
 *        Parameter name of the image (case insensitive).
 *     value = char * (Returned)
 *        The name of the image.
 *     value_length = int (Given)
 *        The maximum length of value
 *     status = (Given and Returned)
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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

 *  Authors:
 *     The orginal version was generated automatically from the
 *     Fortran source of img_name by the Perl script fcwrap.
 *     PDRAPER: Peter W. Draper (STARLINK - Durham University)
 *     {enter_new_authors_here}

 *  History:
 *     22-Aug-1996 (FCWRAP):
 *        Original version
 *     22-AUG-1996 (PDRAPER):
 *        Added string.h, bit of a tidy up.
 *     {enter_changes_here}

 *-
 */
#include "cnf.h"
#include "f77.h"
#include <string.h>

F77_SUBROUTINE(img_name)( CHARACTER(param),
                          CHARACTER(value),
                          INTEGER(status)
                          TRAIL(param)
                          TRAIL(value) );

void imgName( char *param,
              char *value,
              int value_length,
              int *status ) {

  DECLARE_CHARACTER_DYN(fparam);
  DECLARE_CHARACTER_DYN(fvalue);

  F77_CREATE_CHARACTER(fparam,strlen( param ));
  cnf_exprt( param, fparam, fparam_length );
  F77_CREATE_CHARACTER(fvalue,value_length);

  F77_LOCK( F77_CALL(img_name)( CHARACTER_ARG(fparam),
                      CHARACTER_ARG(fvalue),
                      INTEGER_ARG(status)
                      TRAIL_ARG(fparam)
                      TRAIL_ARG(fvalue) ); )

  F77_FREE_CHARACTER(fparam);
  cnf_imprt( fvalue, fvalue_length, value );
  F77_FREE_CHARACTER(fvalue);

  return;
}

/* $Id$ */
