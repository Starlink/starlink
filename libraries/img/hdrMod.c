/*
*  Name:
*     hdrMod

*  Purpose:
*     Opens an image allowing modification of any header items.

*  Language:
*     ANSII C

*  Invocation:
*     hdrMod( param,
*             status )

*  Description:
*     This C function sets up the required arguments and calls the
*     Fortran subroutine hdr_mod.
*     On return, values are converted back to C form if necessary.

*  Arguments:
*     param = char * (Given)
*        Parameter name of the image (case insensitive).
*     status = int * (Given and Returned)
*        The global status.

*  Authors:
*     The orginal version was generated automatically from the
*     Fortran source of hdr_mod by the Perl script fcwrap.
*     {enter_new_authors_here}

*  History:
*     17-May-1996 (fcwrap):
*        Original version
*     {enter_changes_here}

*-
*/
#include <string.h>
#include "cnf.h"
#include "f77.h"

F77_SUBROUTINE(hdr_mod)( CHARACTER(param),
                         INTEGER(status)
                         TRAIL(param) );

void hdrMod( char *param,
             int *status ) {

DECLARE_CHARACTER_DYN(fparam);

   F77_CREATE_CHARACTER(fparam,strlen( param ));
   cnf_exprt( param, fparam, fparam_length );

   F77_CALL(hdr_mod)( CHARACTER_ARG(fparam),
                      INTEGER_ARG(status)
                      TRAIL_ARG(fparam) );

   F77_FREE_CHARACTER(fparam);

   return;
}

/* $Id$ */
