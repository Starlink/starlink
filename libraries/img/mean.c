#include <stdio.h>
#include <string.h>
#include "f77.h"
#include "img.h"

/*+
 * Name:
 *    mean
 
 *  Purpose:
 *     Calculates and reports the mean value of an image.
 
 *  Description:
 *     This is a demonstration routine for IMG. It accesses an existing
 *     image and calculates the mean value which it then writes to the
 *     terminal.
 
 *-
 */

F77_SUBROUTINE(mean)(INTEGER(istat))
{
  /* Local variables: */
  float *ip;
  int nx, ny;
  float sum;
  int i;

  /*  Access an input image. */
  imgIn( "in", &nx, &ny, &ip, istat );

  /*  Derive the mean and write it out. */
  sum = 0.0f;
  for( i=0; i < nx*ny; i++ ) sum += ip[i];
  printf ("Mean value = %f\n", sum/(nx*ny) );

  /*  Free the input image. */
  imgFree( "in", istat );
}

/* $Id$ */
