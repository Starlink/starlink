/*
 *  Name:
 *     imgDelet
 
 *  Purpose:
 *     Deletes an image.
 
 *  Language:
 *     ANSI C
 
 *  Invocation:
 *     imgDelet( param,
 *               status )
 
 *  Description:
 *     This C function sets up the required arguments and calls the
 *     Fortran subroutine img_delet.
 *     On return, values are converted back to C form if necessary.
 
 *  Arguments:
 *     param = char * (Given)
 *        Parameter name specifying the image to be deleted.
 *     status = int * (Given and Returned)
 *        The global status.
 
 *  Authors:
 *     The orginal version was generated automatically from the
 *     Fortran source of img_delet by the Perl script fcwrap.
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

F77_SUBROUTINE(img_delet)( CHARACTER(param),
                           INTEGER(status)
                           TRAIL(param) );

void imgDelet( char *param,
               int *status ) {
  
  DECLARE_CHARACTER_DYN(fparam);

  F77_CREATE_CHARACTER(fparam,strlen( param ));
  cnf_exprt( param, fparam, fparam_length );

  F77_CALL(img_delet)( CHARACTER_ARG(fparam),
                       INTEGER_ARG(status)
                       TRAIL_ARG(fparam) );

  F77_FREE_CHARACTER(fparam);

  return;
}

/* $Id$ */
