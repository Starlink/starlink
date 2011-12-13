#include <stdio.h>
#include <string.h>
#include "f77.h"
#include "img.h"

/*+
 * Name:
 *    HDRLIST

 *  Purpose:
 *     List all the header items in a named source.

 *  Description:
 *     This routine is part of the IMG example suite. It accesses an
 *     existing image with a known header information source (such as
 *     "FITS"). It then lists the names of all the items in the named
 *     source and their values.

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

*  Copyright:
*     Copyright (C) 1998 Central Laboratory of the Research Councils.
*     All Rights Reserved.

 *  Authors:
 *     PWD: Peter Draper (Starlink)

 *  History:
 *     03-JUN-1998 (PWD):
 *         Original Version

 *  Notes:
 *     As this handles the special FITS header items "COMMENT", "HISTORY"
 *     and " ", it is a little more complex than the documentation in
 *     SUN/160 suggests.

 *-
 */

F77_SUBROUTINE(hdrlist)(INTEGER(istat))
{
  /*  Local Variables: */
  char source[30], item[30];
  char value[80];
  int doneblank=0, donecomment=0, donehistory=0;
  int list, n, ncomp;
  int i, j;
  int type;

  /*  Define types of value that we might get back. */
  enum { PLAIN, COMMENT, HISTORY, BLANK };

  /*  Get the name of the source to read the header items from. */
  printf( "SOURCE - Source of header information > " );
  scanf( "%s", source );

  /*  See how many items are present (this also accesses the image). */
  hdrNumb( "IN", source, "*", &n, istat );
  if ( n > 0 ) {

    /*  Get the names of the items one-by-one. Then read the value
     *  of the item. This will fail for FITS multiple items unless
     *  we take special action. */
    for( i=1; i <= n; i++ ) {

      /*  Get the name of the I'th header item. */
      hdrName( "IN", source, i, item, 30, istat );

      /*  Check if this is a special item. */
      type = PLAIN;
      if ( strcmp( item, "HISTORY" ) == 0 ) {
        type = HISTORY;
      } else if ( strcmp( item, "COMMENT" ) == 0 ) {
        type = COMMENT;
      } else if ( strcmp( item, " " ) == 0 ) {
        type = BLANK;
      }
      if ( type == PLAIN ) {

        /*  It's just an ordinary header item, so get it's value. */
        hdrInC( "IN", source, item, 1, value, 80, istat );
        printf( "%s = %s \n", item, value );

      } else {

        /*  Special keyword. List all occurences of this type if we've
            not already done so. */
        list = 0;
        if ( type == HISTORY && !donehistory ) {
          list = 1;
          donehistory = 1;
        } else if ( type == COMMENT && !donecomment ) {
          list = 1;
          donecomment = 1;
        } else if ( type == BLANK && !doneblank ) {
          list = 1;
          doneblank = 1;
        }
        if ( list ) {
          hdrNumb( "IN", source, item, &ncomp, istat );
          for( j=1; j <= ncomp; j++ ) {
            hdrInC( "IN", source, item, j, value, 80, istat );
            printf( "%s = %s \n", item, value );
          }
        }
      }
    }
  } else {

    /*  The number of items in the source is zero. */
    printf( "Header source doesn''t exist or contains no valid items.\n" );
  }

  /**  Free the input image. */
  imgFree( "IN", istat );
}

/* $Id$ */
