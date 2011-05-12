/*
 *+
 *  Name:
 *     hdrOutC

 *  Purpose:
 *     Writes character header items.

 *  Language:
 *     ANSI C

 *  Invocation:
 *     hdrOutC( param, xname, item, commen, value, value_length, status )

 *  Description:
 *     This C function sets up the required arguments and calls the
 *     Fortran subroutine hdr_outc.
 *     On return, values are converted back to C form if necessary.

 *  Arguments:
 *     param = char * (Given)
 *        Parameter name of the image (case insensitive).
 *     xname = char * (Given)
 *        Name of the extension ('FITS' or ' ' for FITS).
 *     item = char * (Given)
 *        Name of the header item.
 *     commen = char * (Given)
 *        If XNAME is 'FITS' then this is used as a comment to enter
 *        with the record. Otherwise this is not used.
 *     value = char * (Given)
 *        The value. This can also be a pointer to the first character
 *        of a fixed size array. In which case the length of the
 *        strings in the array are given as value_length.
 *     value_length = int (Given)
 *        Either 0, which indicates that a single string has been
 *        passed or the length of the elements in a fixed size
 *        character array. The number of elements is assumed equal to
 *        the number of parameters in param.
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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

 *  Authors:
 *     The orginal version was generated automatically from the
 *     Fortran source of hdr_out by the Perl script fcwrap.
 *     PDRAPER: Peter W. Draper (STARLINK - Durham University).
 *     {enter_new_authors_here}

 *  History:
 *     17-May-1996 (fcwrap):
 *        Original version
 *     23-May-1996 (PDRAPER):
 *        Changed to deal with input character array.
 *     {enter_changes_here}

 *-
 */
#include "cnf.h"
#include "f77.h"
#include "string.h"
#include "img1.h"

F77_SUBROUTINE(hdr_outc)( CHARACTER(param),
                          CHARACTER(xname),
                          CHARACTER(item),
                          CHARACTER(commen),
                          CHARACTER_ARRAY(value),
                          INTEGER(status)
                          TRAIL(param)
                          TRAIL(xname)
                          TRAIL(item)
                          TRAIL(commen)
                          TRAIL(value) );

void hdrOutC( char *param,
              char *xname,
              char *item,
              char *commen,
              char *value,
              int value_length,
              int *status ) {

  DECLARE_CHARACTER_DYN(fparam);
  DECLARE_CHARACTER_DYN(fxname);
  DECLARE_CHARACTER_DYN(fitem);
  DECLARE_CHARACTER_DYN(fcommen);
  DECLARE_CHARACTER_DYN(fvalue);
  int lens;
  int nparam;
  int i;
  F77_CHARACTER_TYPE *ptr1;
  char *ptr2;

  F77_CREATE_CHARACTER(fparam,strlen( param ));
  cnf_exprt( param, fparam, fparam_length );
  F77_CREATE_CHARACTER(fxname,strlen( xname ));
  cnf_exprt( xname, fxname, fxname_length );
  F77_CREATE_CHARACTER(fitem,strlen( item ));
  cnf_exprt( item, fitem, fitem_length );
  F77_CREATE_CHARACTER(fcommen,strlen( commen ));
  cnf_exprt( commen, fcommen, fcommen_length );

  /*  Need to copy the input strings in value into an Fortran
      character array. Note if string length is zero then we must work
      out the length ourselves. */
  nparam = img1CountParams( param, status );
  if ( value_length == 0 ) {
    lens = (int) strlen( value );
  } else {
    lens = value_length;
  }
  F77_CREATE_CHARACTER( fvalue, nparam*lens );
  fvalue_length = lens;
  ptr1 = fvalue;
  ptr2 = value;
  for( i=0; i < nparam; i++ ) {
    cnf_exprt( ptr2, ptr1, fvalue_length );
    ptr1 += lens;
    ptr2 += lens;
  }

  F77_LOCK( F77_CALL(hdr_outc)( CHARACTER_ARG(fparam),
                      CHARACTER_ARG(fxname),
                      CHARACTER_ARG(fitem),
                      CHARACTER_ARG(fcommen),
                      CHARACTER_ARRAY_ARG(fvalue),
                      INTEGER_ARG(status)
                      TRAIL_ARG(fparam)
                      TRAIL_ARG(fxname)
                      TRAIL_ARG(fitem)
                      TRAIL_ARG(fcommen)
                      TRAIL_ARG(fvalue) ); )

  F77_FREE_CHARACTER(fparam);
  F77_FREE_CHARACTER(fxname);
  F77_FREE_CHARACTER(fitem);
  F77_FREE_CHARACTER(fcommen);
  F77_FREE_CHARACTER(fvalue);

  return;
}

/* $Id$ */
