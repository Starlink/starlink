/*
 * RAL GKS SYSTEM
 *
 * gk0xwc_: Sets the ww foreground colour for the Xlib workstation.
 *
 * Type of Routine:  Part of WORKSTATION DRIVER
 * Author:           TAW
 *
 * Copyright (C) SERC 1988
 *
 * Maintenance Log:
 *
 *  20/09/88	TAW	Copied as is from ../sun/gk9swc_.c.
 */

#include "../../system/include/f77_type.h"	/* For FORTRAN type matching */
#include <wwinfo.h>				/* For ww */

f77_integer gk0xwc_(colind)
  f77_integer *colind;
{

  f77_integer
    gk0xcc_();				/* Maps GKS colour -> Xlib colour */

  /* Set the ww foreground colour index to the mapped supplied value */

  dd->d_fore = (int)gk0xcc_(colind);

  return ((f77_integer)0);
}
