/*
*  Name:
*     imgCheck

*  Purpose:
*     Checks the current internal state of IMG.

*  Language:
*     ANSII C

*  Invocation:
*     imgCheck( status )

*  Description:
*     This C function sets up the required arguments and calls the
*     Fortran subroutine img_check.
*     On return, values are converted back to C form if necessary.

*  Arguments:
*     status = int * (Given and Returned)
*        The global status.

*  Authors:
*     The orginal version was generated automatically from the
*     Fortran source of img_check by the Perl script fcwrap.
*     {enter_new_authors_here}

*  History:
*     17-May-1996 (fcwrap):
*        Original version
*     {enter_changes_here}

*-
*/
#include "cnf.h"
#include "f77.h"

F77_SUBROUTINE(img_check)( INTEGER(status) );

void imgCheck( int *status ) {

   F77_CALL(img_check)( INTEGER_ARG(status) );

   return;
}

/* $Id$ */
