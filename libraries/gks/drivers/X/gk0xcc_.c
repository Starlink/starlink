/*
 * RAL GKS SYSTEM
 *
 * gk0xcc_: Map a GKS colour index to a X/ww colour index.
 *
 * Type of Routine:  Part of WORKSTATION DRIVER
 * Author:           PJWR
 *
 * Copyright (C) SERC 1987
 *
 * Maintenance Log:
 *
 *  08/12/87  PJWR  Created gk9scc_().
 *  10/08/88  TAW   Copied from ../sun/gk9scc_.
 *  02/11/88  TAW   Modified for X colour table.
 */

#include "../../system/include/f77_type.h"	/* For FORTRAN type matching */
#include "../../system/include/gkdt.h"		/* Needed by ... */
#include "../../system/include/gkwca.h"		/* For WS Communication Area */
#include "../../system/include/gkwdt.h"		/* For WS Description Table */


f77_integer gk0xcc_(index)
  f77_integer *index;		/* Colour index to be mapped (In) */
{
      return(*index);
}
