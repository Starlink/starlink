/*
 * RAL GKS SYSTEM
 *
 * gk9swc_: Sets the ww foreground colour for the Sun workstation.
 *
 * Type of Routine:  Part of WORKSTATION DRIVER
 * Author:           TAW
 *
 * Copyright (C) SERC 1987
 *
 * Maintenance Log:
 *
 *  ??/??/87  TAW   Created.
 *  03/11/87  PJWR  Added header and put into standard format.
 *  08/12/87  PJWR  Changed to use new colour mapping scheme.
 */

#include "../../system/include/f77_type.h"	/* For FORTRAN type matching */
#include "./varinc/wwinfo.h"				/* For ww */

f77_integer gk9swc_(colind)
  f77_integer *colind;
{

  f77_integer
    gk9scc_();				/* Maps GKS colour -> SunView colour */

  /* Set the ww foreground colour index to the mapped supplied value */

  dd->d_fore = (int)gk9scc_(colind);

  return ((f77_integer)0);
}
