/*
 *  Name:
 *     hdrNumb
 
 *  Purpose:
 *     Returns a header item count.
 
 *  Language:
 *     ANSI C

 *  Invocation:
 *     hdrNumb( param,
 *              xname,
 *              item,
 *              n,
 *              status )

 *  Description:
 *     This C function sets up the required arguments and calls the
 *     Fortran subroutine hdr_numb.
 *     On return, values are converted back to C form if necessary.

 *  Arguments:
 *     param = char * (Given)
 *        Parameter name of the image (case insensitive).
 *     xname = char * (Given)
 *        The name of the extension ('FITS' or ' ' for FITS).
*     item = char * (Given)
 *        The name of an item or '*'. If this is '*' then a count of all
 *        the items in the extension is returned, otherwise the number of
 *        occurrences of the named item is returned.
 *     n = int * (Returned)
 *        The number of header items or components of an item.
 *     status = int * (Given and Returned)
 *        The global status.

 *  Authors:
 *     The orginal version was generated automatically from the
 *     Fortran source of hdr_numb by the Perl script fcwrap.
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

F77_SUBROUTINE(hdr_numb)( CHARACTER(param),
                          CHARACTER(xname),
                          CHARACTER(item),
                          INTEGER(n),
                          INTEGER(status)
                          TRAIL(param)
                          TRAIL(xname)
                          TRAIL(item) );

void hdrNumb( char *param,
              char *xname,
              char *item,
              int *n,
              int *status ) {

  DECLARE_CHARACTER_DYN(fparam);
  DECLARE_CHARACTER_DYN(fxname);
  DECLARE_CHARACTER_DYN(fitem);
  
  F77_CREATE_CHARACTER(fparam,strlen( param ));
  cnf_exprt( param, fparam, fparam_length );
  F77_CREATE_CHARACTER(fxname,strlen( xname ));
  cnf_exprt( xname, fxname, fxname_length );
  F77_CREATE_CHARACTER(fitem,strlen( item ));
  cnf_exprt( item, fitem, fitem_length );
  
  F77_CALL(hdr_numb)( CHARACTER_ARG(fparam),
                      CHARACTER_ARG(fxname),
                      CHARACTER_ARG(fitem),
                      INTEGER_ARG(n),
                      INTEGER_ARG(status)
                      TRAIL_ARG(fparam)
                      TRAIL_ARG(fxname)
                      TRAIL_ARG(fitem) );
  
  F77_FREE_CHARACTER(fparam);
  F77_FREE_CHARACTER(fxname);
  F77_FREE_CHARACTER(fitem);
   
  return;
}

/* $Id$ */
