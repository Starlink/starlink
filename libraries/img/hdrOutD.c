/*
 *  Name:
 *     hdrOutD
 
 *  Purpose:
 *     Writes a header item using a specific type.
 
 *  Language:
 *     ANSI C

 *  Invocation:
 *     hdrOutD( param,
 *              xname,
 *              item,
 *              commen,
 *              value,
 *              status )
 
 *  Description:
 *     This C function sets up the required arguments and calls the
 *     Fortran subroutine hdr_outd.
 *     On return, values are converted back to C form if necessary.
 
 *  Arguments:
 *     param = char * (Given)
 *        Parameter name of the image (case insensitive).
 *     xname = char * (Given)
 *        Name of the extension ('FITS' or ' ' for FITS).
 *     item = char * (Given)
 *        Name of the header item.
 *     commen = char * (Given)
 *        If XNAME is 'FITS' then this is used as a comment to enter
 *        with the record. Otherwise this is not used.
 *     value = double * (Given)
 *        The value.
 *     status = int * (Given and Returned)
 *        The global status.
 
 *  Authors:
 *     The orginal version was generated automatically from the
 *     Fortran source of hdr_outd by the Perl script fcwrap.
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

F77_SUBROUTINE(hdr_outd)( CHARACTER(param),
                          CHARACTER(xname),
                          CHARACTER(item),
                          CHARACTER(commen),
                          DOUBLE_ARRAY(value),
                          INTEGER(status)
                          TRAIL(param)
                          TRAIL(xname)
                          TRAIL(item)
                          TRAIL(commen) );

void hdrOutD( char *param,
              char *xname,
              char *item,
              char *commen,
              double *value,
              int *status ) {
  
  DECLARE_CHARACTER_DYN(fparam);
  DECLARE_CHARACTER_DYN(fxname);
  DECLARE_CHARACTER_DYN(fitem);
  DECLARE_CHARACTER_DYN(fcommen);
  
  F77_CREATE_CHARACTER(fparam,strlen( param ));
  cnf_exprt( param, fparam, fparam_length );
  F77_CREATE_CHARACTER(fxname,strlen( xname ));
  cnf_exprt( xname, fxname, fxname_length );
  F77_CREATE_CHARACTER(fitem,strlen( item ));
  cnf_exprt( item, fitem, fitem_length );
  F77_CREATE_CHARACTER(fcommen,strlen( commen ));
  cnf_exprt( commen, fcommen, fcommen_length );
  
  F77_CALL(hdr_outd)( CHARACTER_ARG(fparam),
                      CHARACTER_ARG(fxname),
                      CHARACTER_ARG(fitem),
                      CHARACTER_ARG(fcommen),
                      DOUBLE_ARRAY_ARG(value),
                      INTEGER_ARG(status)
                      TRAIL_ARG(fparam)
                      TRAIL_ARG(fxname)
                      TRAIL_ARG(fitem)
                      TRAIL_ARG(fcommen) );
  
  F77_FREE_CHARACTER(fparam);
  F77_FREE_CHARACTER(fxname);
  F77_FREE_CHARACTER(fitem);
  F77_FREE_CHARACTER(fcommen);
  
  return;
}

/* $Id$ */
