/*
 *+
 *  Name:
 *     utiltest.c

 *  Purpose:
 *     Test Starlink C utility functions.

 *  Language:
 *     Starlink ANSI C

 *  Type of Module:
 *     C program

 *  History:
 *     2008-09-04 (TIMJ):
 *        Initial version.

 *  Copyright:
 *     Copyright (C) 2008 Science and Technology Facilities Council.
 *     All Rights Reserved.

 *  Licence:
 *     This program is free software; you can redistribute it and/or
 *     modify it under the terms of the GNU General Public License as
 *     published by the Free Software Foundation; either version 3 of
 *     the License, or (at your option) any later version.
 *
 *     This program is distributed in the hope that it will be
 *     useful, but WITHOUT ANY WARRANTY; without even the implied
 *     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
 *     PURPOSE. See the GNU General Public License for more details.
 *
 *     You should have received a copy of the GNU General Public
 *     License along with this program; if not, write to the Free
 *     Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
 *     MA 02111-1307, USA

 *  Bugs:
 *     {note_any_bugs_here}

 *-
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "./util.h"

#define ONEBUFSIZ 15

int
main ( void ) {

  size_t len;
  char src1[] = "0123456789";
  char src2[] = "01234567890123456789";
  char dest1[ONEBUFSIZ];
  int exstat = EXIT_SUCCESS;

  dest1[0] = '\0';
  
  /* Test strlcpy */

  len = star_strlcpy( dest1, src1, ONEBUFSIZ );
  printf("Attempt to copy %d characters to destination\n", (int)len);
  if (len != strlen(src1)) {
    printf("Wrong number of characters copied\n");
    exstat = EXIT_FAILURE;
  }

  /* truncation */
  len = star_strlcpy( dest1, src2, ONEBUFSIZ );
  if (len >= sizeof(dest1)) {
    printf("Did truncate as expected\n");
  } else {
    printf("Should have truncated\n");
    exstat = EXIT_FAILURE;
  }

  /* Appending */
  len = star_strlcpy( dest1, src1, ONEBUFSIZ);
  len = star_strlcat( dest1, "XX", ONEBUFSIZ);
  if (len != strlen(dest1)) {
    printf("Did not append string correctly\n");
    exstat = EXIT_FAILURE;
  }


  len = star_strlcat( dest1, src2, ONEBUFSIZ);
  if (len >= sizeof(dest1)) {
    printf("Did get truncation correctly\n");
  } else {
    printf("Did not get truncation status. Copied %d characters\n",
           (int)len);
    exstat = EXIT_FAILURE;
  }

  return exstat;

}
