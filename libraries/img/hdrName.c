/*
 *+
 *  Name:
 *     hdrName

 *  Purpose:
 *     Returns a header item name.

 *  Language:
 *     ANSII C

 *  Invocation:
 *     hdrName( param,
 *              xname,
 *              n,
 *              item,
 *              status )

 *  Description:
 *     This C function sets up the required arguments and calls the
 *     Fortran subroutine hdr_name.
 *     On return, values are converted back to C form if necessary.

 *  Arguments:
 *     param = char * (Given)
 *        Parameter name of the image (case insensitive).
 *     xname = char * (Given)
 *        The name of the extension ('FITS' or ' ' for FITS).
 *     n = int  (Given)
 *        The index of the item.
 *     item = char * (Returned)
 *        The name of the extension item (blank when no item with the
 *        given index exists).
 *     item_length = int (Given)
 *        The maximum length of item
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
 *     The orginal version was generated automatically from the
 *     Fortran source of hdr_name by the Perl script fcwrap.
 *     PDRAPER: Peter W. Draper (STARLINK - Durham University)
 *     {enter_new_authors_here}

 *  History:
 *     17-May-1996 (fcwrap):
 *        Original version
 *     4-JUN-1996 (PDRAPER):
 *        Added code to deal with BLANK keyword.
 *     {enter_changes_here}

 *-
 */
#include <string.h>
#include "cnf.h"
#include "f77.h"

F77_SUBROUTINE(hdr_name)( CHARACTER(param),
                          CHARACTER(xname),
                          INTEGER(n),
                          CHARACTER(item),
                          INTEGER(status)
                          TRAIL(param)
                          TRAIL(xname)
                          TRAIL(item) );

void hdrName( char *param,
              char *xname,
              int n,
              char *item,
              int item_length,
              int *status ) {

  DECLARE_CHARACTER_DYN(fparam);
  DECLARE_CHARACTER_DYN(fxname);
  DECLARE_CHARACTER_DYN(fitem);

  F77_CREATE_CHARACTER(fparam,strlen( param ));
  cnf_exprt( param, fparam, fparam_length );
  F77_CREATE_CHARACTER(fxname,strlen( xname ));
  cnf_exprt( xname, fxname, fxname_length );
  F77_CREATE_CHARACTER(fitem,item_length);

  F77_LOCK( F77_CALL(hdr_name)( CHARACTER_ARG(fparam),
                      CHARACTER_ARG(fxname),
                      INTEGER_ARG(&n),
                      CHARACTER_ARG(fitem),
                      INTEGER_ARG(status)
                      TRAIL_ARG(fparam)
                      TRAIL_ARG(fxname)
                      TRAIL_ARG(fitem) ); )

  F77_FREE_CHARACTER(fparam);
  F77_FREE_CHARACTER(fxname);

  cnf_imprt( fitem, fitem_length, item );

  /*  The special name " " is possible. Deal with this correctly */
  if ( item[0] == '\0' && fitem[0] == ' ' &&
       ( xname[0] == 'F' || xname[0] == ' ' ) ) {
    item[0] = ' ';
    item[1] = '\0';
  }
  F77_FREE_CHARACTER(fitem);

  return;
}

/* $Id$ */
