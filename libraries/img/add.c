#include <stdio.h>
#include <string.h>
#include "f77.h"
#include "img.h"

/*+
 * Name:
 *    add
 
 *  Purpose:
 *     Adds a constant value to all the elements of an image.
 
 *  Description:
 *     This is a demonstration routine for IMG. It creates a copy of an
 *     existing image and then adds a specified constant to all the
 *     image elements.
 
 *  Notes:
 *     This routine could also be implemented to just modify the input
 *     image, rather than creating a new copy.
 *
 *     The PAR routines should be used to access the data value when
 *     they are available with a C interface.
 
 *-
 */

F77_SUBROUTINE(add)(INTEGER(status))
{
  int nx, ny;
  float *ptrIn, *ptrOut;
  float value;
  int i;
 
  /*  Access an existing image */
  imgIn( "IN", &nx, &ny, &ptrIn, status );

  /*  Copy this to an output image */
  imgOut( "IN", "OUT", &ptrOut, status );

  /*  Get the value to add. */
  /*  parGet0r( "CONSTANT", value, status ); */
  printf("CONSTANT - value to add to image> ");
  scanf( "%e", &value );

  /*  And do the work. */
  for( i=0; i <nx*ny; i++ ) { 
    ptrOut[i] = value + ptrIn[i];
  }

  /*  Free the input and output images. */
  imgFree( "*", status );
}

/* $Id$ */
