#include <stdio.h>
#include "f77.h"
#include "img.h"

/*+
 * Name:
 *    HDRWRITE
 
 *  Purpose:
 *     Writes a new header item.
 
 *  Description:
 *     This routine is part of the IMG example suite. It accesses an
 *     existing image and creates a copy as an output image. It then
 *     writes a new header item to the output image.
 
 *  Notes:
 *     This could be simplified by just modifying the input image rather
 *     than creating a copy. To do this just comment out the imgIn and
 *     imgOut lines. You might also want to change the hdrOut line to
 *     access the image via parameter "in".

 *-
 */

F77_SUBROUTINE(hdrwrite)(INTEGER(istat))
{

  /*  Local Variables: */
  char item[8];
  char comment[30], value[30];
  int nx, ny;
  float *ipin, *ipout;

  /*  Access the input image. */
  imgIn( "in", &nx, &ny, &ipin, istat );

  /*  Copy this to an output image. */
  imgOut( "in", "out", &ipout, istat );

  /*  Now get the name of the header item. */
  printf( "ITEM - Name of header item > " );
  scanf( "%8s", item );

  /*  And its value.*/
  printf( "VALUE - Header item value > " );
  scanf( "%30s", value );

  /*  And a comment for it. */
  printf( "COMMENT - Item description > " );
  scanf( "%30s", comment );

  /*  Now write it. */
  hdrOut( "out", " ", item, comment, value, 30, istat );

  /*  Free all the images. */
  imgFree( "*", istat );

}

/* $Id$ */
