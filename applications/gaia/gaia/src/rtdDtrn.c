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
 *      20-OCT-1997 (PDRAPER):
 *         Original version.
 *      {enter_changes_here}
 */

#include "sae_par.h"
#include "cnf.h"
#include "f77.h"

extern void F77_EXTERNAL_NAME(rtd1_dtrn)( POINTER(xold),
                                          POINTER(yold),
                                          POINTER(xnew),
                                          POINTER(ynew),
                                          INTEGER(npoints),
                                          INTEGER(ifit),
                                          POINTER(tr),
					  DOUBLE(resid),
                                          INTEGER(status));

int rtdDtrn( int fittype, double *xold, double *yold, double *xnew,
             double *ynew, int npoints, double *tr, double *resid )
{
  DECLARE_INTEGER(status);          /* Global status */
  DECLARE_POINTER(*ipxold);
  DECLARE_POINTER(*ipyold);
  DECLARE_POINTER(*ipxnew);
  DECLARE_POINTER(*ipynew);
  DECLARE_POINTER(*iptr);
  DECLARE_DOUBLE(fresid);

  ipxold = (F77_POINTER_TYPE *) xold;
  ipyold = (F77_POINTER_TYPE *) yold;
  ipxnew = (F77_POINTER_TYPE *) xnew;
  ipynew = (F77_POINTER_TYPE *) ynew;
  iptr   = (F77_POINTER_TYPE *) tr;
  status = SAI__OK;
  F77_CALL(rtd1_dtrn) (POINTER_ARG(ipxnew), POINTER_ARG(ipynew),
                       POINTER_ARG(ipxold), POINTER_ARG(ipyold), 
                       INTEGER_ARG(&npoints), INTEGER_ARG(&fittype),
                       POINTER_ARG(iptr), DOUBLE_ARG(&fresid), 
		       INTEGER_ARG(&status));
  
  /* If status is OK then success. */
  if ( status == SAI__OK ) {
    *resid = (double) fresid;
    return 1;
  } else {
    return 0;
  }
}
