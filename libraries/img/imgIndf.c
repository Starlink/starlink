/*
 *  Name:
 *     imgIndf
 
 *  Purpose:
 *     Obtains an NDF identifier for an image.
 
 *  Language:
 *     ANSI C
 
 *  Invocation:
 *     imgIndf( param,
 *              indf,
 *              status )
 
 *  Description:
 *     This C function sets up the required arguments and calls the
 *     Fortran subroutine img_indf.
 *     On return, values are converted back to C form if necessary.

 *  Arguments:
 *     param = char * (Given)
 *        Parameter name (case insensitive).
 *     indf = int * (Returned)
 *        NDF identifier for the image.
 *     status = int * (Given and Returned)
 *        The global status.

 *  Authors:
 *     The orginal version was generated automatically from the
 *     Fortran source of img_indf by the Perl script fcwrap.
 *     {enter_new_authors_here}
 
 *  History:
 *     17-May-1996 (fcwrap):
 *        Original version
 *     {enter_changes_here}
 
 *-
 */
#include "cnf.h"
#include "f77.h"
#include "string.h"

F77_SUBROUTINE(img_indf)( CHARACTER(param),
                          INTEGER(indf),
                          INTEGER(status)
                          TRAIL(param) );

void imgIndf( char *param,
              int *indf,
              int *status ) {
  
  DECLARE_CHARACTER_DYN(fparam);

  F77_CREATE_CHARACTER(fparam,strlen( param ));
  cnf_exprt( param, fparam, fparam_length );

  F77_CALL(img_indf)( CHARACTER_ARG(fparam),
                      INTEGER_ARG(indf),
                      INTEGER_ARG(status)
                      TRAIL_ARG(fparam) );

  F77_FREE_CHARACTER(fparam);
  
  return;
}

/* $Id$ */
