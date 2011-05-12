/*
 *+
 *  Name:
 *     hdrInC

 *  Purpose:
 *     Reads character header items.

 *  Language:
 *     ANSI C

 *  Invocation:
 *     hdrInC( param, xname, item, comp, value, value_length, status )

 *  Description:
 *     This C function sets up the required arguments and calls the
 *     Fortran subroutine hdr_in.
 *     On return, values are converted back to C form if necessary.
 *
 *     See the hdr_inc documentation for what the subroutine does.

 *  Arguments:
 *     param = char * (Given)
 *        Parameter name of the image (case insensitive).
 *     xname = char * (Given)
 *        Name of the extension ('FITS' or ' ' for FITS headers).
 *     item = char * (Given)
 *        Name of the header item.
 *     comp = int  (Given)
 *        The component of a multiple FITS header item ('HISTORY' and
 *        'COMMENT' items often have many occurrences). The number of
 *        components may be queried using the HDR_NUMB routine.
 *     value = char * (Given and Returned)
 *        Pointer to first element of an array of chars.
 *        These are unmodified if the items doesn't exist.
 *     value_length = int (Given)
 *        Length of the elements of array pointed to by value.
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
 *     Fortran source of hdr_in by the Perl script fcwrap.
 *
 *     PDRAPER: Peter W. Draper (STARLINK - Durham University)
 *     {enter_new_authors_here}

 *  History:
 *     17-May-1996 (fcwrap):
 *        Original version
 *     21-MAY-1996 (PDRAPER):
 *        Added code to deal with character arrays.
 *     {enter_changes_here}

 *-
 */
#include <string.h>
#include "cnf.h"
#include "f77.h"
#include "sae_par.h"
#include "img1.h"

F77_SUBROUTINE(hdr_inc)( CHARACTER(param),
                        CHARACTER(xname),
                        CHARACTER(item),
                        INTEGER(comp),
                        CHARACTER_ARRAY(value),
                        INTEGER(status)
                        TRAIL(param)
                        TRAIL(xname)
                        TRAIL(item)
                        TRAIL(value) );

void hdrInC( char *param,
             char *xname,
             char *item,
             int comp,
             char *value,
             int value_length,
             int *status ) {

  /*  Declarations: */
  DECLARE_CHARACTER_DYN(fparam);
  DECLARE_CHARACTER_DYN(fxname);
  DECLARE_CHARACTER_DYN(fitem);
  DECLARE_CHARACTER_DYN(fvalue);
  int nparam;
  char *ptr1;
  char *ptr2;
  int i;

  /*  Count the number of images that we are to return values
      for. This is also the number of value_length elements in the
      array pointed to by value. */
  nparam = img1CountParams( param, status );

  /*  Create the Fortran strings needed and export the C versions to
      them */
  F77_CREATE_CHARACTER( fparam, strlen( param ));
  cnf_exprt( param, fparam, fparam_length );

  F77_CREATE_CHARACTER( fxname, strlen( xname ));
  cnf_exprt( xname, fxname, fxname_length );

  F77_CREATE_CHARACTER( fitem, strlen( item ));
  cnf_exprt( item, fitem, fitem_length );


  /*  Create a character string for containing the return values. Note
      this is really a character array, so the length of a string is
      value_length-1 not (value_length-1) * nparam. Note the -1
      retains space for the terminating null character.
      */
  F77_CREATE_CHARACTER( fvalue, (value_length-1) * nparam );
  fvalue_length = value_length-1;

  F77_LOCK( F77_CALL(hdr_inc)( CHARACTER_ARG(fparam),
                     CHARACTER_ARG(fxname),
                     CHARACTER_ARG(fitem),
                     INTEGER_ARG(&comp),
                     CHARACTER_ARRAY_ARG(fvalue),
                     INTEGER_ARG(status)
                     TRAIL_ARG(fparam)
                     TRAIL_ARG(fxname)
                     TRAIL_ARG(fitem)
                     TRAIL_ARG(fvalue) ); )

  F77_FREE_CHARACTER(fparam);
  F77_FREE_CHARACTER(fxname);
  F77_FREE_CHARACTER(fitem);

  /*  Deal with the return values. Each element of the array needs to
      be copied into the input array.
      */

  ptr1 = fvalue;
  ptr2 = value;
  for( i=0; i<nparam; i++ ) {
    cnf_imprt( ptr1, fvalue_length, ptr2 );
    ptr1 += fvalue_length;
    ptr2 += value_length;
  }
  F77_FREE_CHARACTER(fvalue);
  return;
}

/* $Id$ */
