/*
 * RAL GKS SYSTEM
 *
 * gk9scc_: Map a GKS colour index to a SunView/ww colour index.
 *
 * Type of Routine:  Part of WORKSTATION DRIVER
 * Author:           PJWR
 *
 * Copyright (C) SERC 1987
 *
 * Maintenance Log:
 *
 *  08/12/87  PJWR  Created.
 *  26/10/89  RMK   For input values outside the workstation's colour
 *                  table, cycle round the colour table (S350).
 */

#include "../../system/include/f77_type.h"	/* For FORTRAN type matching */
#include "../../system/include/gkdt.h"		/* Needed by ... */
#include "../../system/include/gkwca.h"		/* For WS Communication Area */
#include "../../system/include/gkwdt.h"		/* For WS Description Table */

/*
 * Comments:
 *
 *  To get compatibility between monochrome and colour windows,  SunView maps
 *  a monochrome index of 1 to the top entry in the colourmap of a colour
 *  window.  We must follow suit.
 *
 *  The GKS standard says that if an index used in a cell array is not present
 *  in the colour table on a workstation, a workstation dependent index is
 *  used on that workstation.  We simply cycle round the colour table.
 *  (This routine is also used for pattern fill, but the colour indices in the
 *  pattern definition will have been checked already in GKSRPA.)
 */

f77_integer gk9scc_(index)
  f77_integer *index;		/* Colour index to be mapped (In) */
{
  f77_integer entry;		/* Colour index inside workstation colour table */

  entry = *index % gkywdt_.kpci[gkywca_.kwkix - 1];

  switch(entry)
  {
    case 0:
      return((f77_integer)0);

    case 1:
      return(gkywdt_.kpci[gkywca_.kwkix - 1] - 1);

    default:
      return(entry - 1);
  }
}
