#include <stdio.h>
#include "f77.h"
#include "img.h"

/*+
 * Name:
 *    flat
 
 *  Purpose:
 *     Creates a false flatfield.
 
 *  Description:
 *     This is a demonstration routine for IMG. It creates a new image
 *     and fills it with ones.
 
 *-
 */

F77_SUBROUTINE(flat)(INTEGER(istat))
{

  /*  Local variables: */
  float *ip;
  int i;

  /*  Create a new image. */
  imgNew( "out", 416, 578, &ip, istat );

  /*  Set all its elements to the value 1.0. */
  for( i=0; i < 416*578; i++ ) { 
    ip[i] = 1.0f;
  }

  /*  Free the new image. */
  imgFree( "out", istat );

}      

/* $Id$ */
