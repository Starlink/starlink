/*
 *   Name:
 *      rtdDtrn
 *
 *   Purpose:
 *      Wrapper routine for calling RTD1_DTRN
 *
 *   Language:
 *      C
 *
 *  Copyright:
 *     Copyright (C) 1998 Central Laboratory of the Research Councils
 *
 *   History:
 *      20-OCT-1997 (PWD):
 *         Original version.
 *      {enter_changes_here}
 */

#include "sae_par.h"
#include "cnf.h"
#include "f77.h"

extern void F77_EXTERNAL_NAME(rtd1_dtrn)( DOUBLE_ARRAY(xold),
                                          DOUBLE_ARRAY(yold),
                                          DOUBLE_ARRAY(xnew),
                                          DOUBLE_ARRAY(ynew),
                                          INTEGER(npoints),
                                          INTEGER(ifit),
                                          DOUBLE_ARRAY(tr),
					  DOUBLE(resid),
                                          INTEGER(status));

int rtdDtrn( int fittype, double *xold, double *yold, double *xnew,
             double *ynew, int npoints, double *tr, double *resid )
{
    int result = 0;

    DECLARE_INTEGER(status);          /* Global status */
    DECLARE_DOUBLE_ARRAY_DYN(ipxold);
    DECLARE_DOUBLE_ARRAY_DYN(ipyold);
    DECLARE_DOUBLE_ARRAY_DYN(ipxnew);
    DECLARE_DOUBLE_ARRAY_DYN(ipynew);
    DECLARE_DOUBLE_ARRAY_DYN(iptr);
    DECLARE_DOUBLE(fresid);

    F77_CREATE_DOUBLE_ARRAY( ipxold, npoints );
    F77_EXPORT_DOUBLE_ARRAY( xold, ipxold, npoints );

    F77_CREATE_DOUBLE_ARRAY( ipyold, npoints );
    F77_EXPORT_DOUBLE_ARRAY( yold, ipyold, npoints );

    F77_CREATE_DOUBLE_ARRAY( ipxnew, npoints );
    F77_EXPORT_DOUBLE_ARRAY( xnew, ipxnew, npoints );

    F77_CREATE_DOUBLE_ARRAY( ipynew, npoints );
    F77_EXPORT_DOUBLE_ARRAY( ynew, ipynew, npoints );

    F77_EXPORT_DOUBLE( *resid, fresid );

    F77_CREATE_DOUBLE_ARRAY( iptr, 6 );
    F77_EXPORT_DOUBLE_ARRAY( tr, iptr, 6 );

    status = SAI__OK;
    F77_CALL(rtd1_dtrn)( DOUBLE_ARRAY_ARG(ipxnew),
                         DOUBLE_ARRAY_ARG(ipynew),
                         DOUBLE_ARRAY_ARG(ipxold),
                         DOUBLE_ARRAY_ARG(ipyold),
                         INTEGER_ARG(&npoints),
                         INTEGER_ARG(&fittype),
                         DOUBLE_ARRAY_ARG(iptr),
                         DOUBLE_ARG(&fresid),
                         INTEGER_ARG(&status) );

    /* If status is OK then success. */
    if ( status == SAI__OK ) {
        F77_IMPORT_DOUBLE_ARRAY( iptr, tr, 6 );
        F77_IMPORT_DOUBLE( fresid, *resid );
        result = 1;
    }
    F77_FREE_DOUBLE( ipxold );
    F77_FREE_DOUBLE( ipyold );
    F77_FREE_DOUBLE( ipxnew );
    F77_FREE_DOUBLE( ipynew );
    F77_FREE_DOUBLE( iptr );

    return result;
}
