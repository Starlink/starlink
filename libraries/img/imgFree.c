/*
 *  Name:
 *     imgFree
 
 *  Purpose:
 *     Releases resources associated with an image.
 
 *  Language:
 *     ANSI C
 
 *  Invocation:
 *     imgFree( param,
 *              status )
 
 *  Description:
 *     This C function sets up the required arguments and calls the
 *     Fortran subroutine img_free.
 *     On return, values are converted back to C form if necessary.
 
 *  Arguments:
 *     param = char * (Given)
 *        Parameter name, specifying either the individual image to be
 *        released, or '*', indicating that all images should be
 *        released.
 *     status = int * (Given and Returned)
 *        The global status.
 
 *  Authors:
 *     The orginal version was generated automatically from the
 *     Fortran source of img_free by the Perl script fcwrap.
 *     PDRAPER: Peter W. Draper (STARLINK - Durham University)
 *     {enter_new_authors_here}
 
 *  History:
 *     17-May-1996 (fcwrap):
 *        Original version
 *     22-May-1996 (PDRAPER):
 *        Added code to release dynamic strings.
 *        (later removed).
 *     {enter_changes_here}
 
 *-
 */
#include <string.h>
#include "cnf.h"
#include "f77.h"
#include "img1.h"

F77_SUBROUTINE(img_free)( CHARACTER(param),
                          INTEGER(status)
                          TRAIL(param) );

void imgFree( char *param,
              int *status ) {

  DECLARE_CHARACTER_DYN(fparam);
  F77_CREATE_CHARACTER( fparam, strlen( param ) );
  F77_EXPORT_CHARACTER( param, fparam, fparam_length );
  
  F77_CALL(img_free)( CHARACTER_ARG(fparam),
                      INTEGER_ARG(status)
                      TRAIL_ARG(fparam) );
  
  F77_FREE_CHARACTER(fparam);
  return;
}

/* $Id$ */
