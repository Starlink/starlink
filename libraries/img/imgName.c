/*
 *  Name:
 *     imgName

 *  Purpose:
 *     Return the image name.
 
 *  Language:
 *     ANSI C
 
 *  Invocation:
 *     imgName( param, value, value_length, status )
 
 *  Description:
 *     This C function sets up the required arguments and calls the
 *     Fortran subroutine img_name.
 *     On return, values are converted back to C form if necessary.
 
 *  Arguments:
 *     param = char * (Given)
 *        Parameter name of the image (case insensitive).
 *     value = char * (Returned)
 *        The name of the image.
 *     value_length = int (Given)
 *        The maximum length of value
 *     status = (Given and Returned)
 *        The global status.
 
 *  Authors:
 *     The orginal version was generated automatically from the
 *     Fortran source of img_name by the Perl script fcwrap.
 *     PDRAPER: Peter W. Draper (STARLINK - Durham University)
 *     {enter_new_authors_here}
 
 *  History:
 *     22-Aug-1996 (FCWRAP):
 *        Original version
 *     22-AUG-1996 (PDRAPER):
 *        Added string.h, bit of a tidy up.
 *     {enter_changes_here}
 
 *-
 */
#include "cnf.h"
#include "f77.h"
#include <string.h>

F77_SUBROUTINE(img_name)( CHARACTER(param),
                          CHARACTER(value),
                          INTEGER(status)
                          TRAIL(param)
                          TRAIL(value) );

void imgName( char *param,
              char *value,
              int value_length,
              int *status ) {
  
  DECLARE_CHARACTER_DYN(fparam);
  DECLARE_CHARACTER_DYN(fvalue);

  F77_CREATE_CHARACTER(fparam,strlen( param ));
  cnf_exprt( param, fparam, fparam_length );
  F77_CREATE_CHARACTER(fvalue,value_length);
  
  F77_CALL(img_name)( CHARACTER_ARG(fparam),
                      CHARACTER_ARG(fvalue),
                      INTEGER_ARG(status)
                      TRAIL_ARG(fparam)
                      TRAIL_ARG(fvalue) );
  
  F77_FREE_CHARACTER(fparam);
  cnf_imprt( fvalue, fvalue_length, value );
  F77_FREE_CHARACTER(fvalue);
  
  return;
}

/* $Id$ */
