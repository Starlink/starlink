/*
 * RAL GKS SYSTEM
 *
 * gk9scd_: Clear display surface using WW.
 *
 * Type of Routine:  Part of WS DRIVER
 * Author:           PJWR
 *
 * Copyright (C) SERC 1987
 *
 * Maintenance Log:
 *
 *  12/01/87  PJWR  Created.
 *  02/04/87  PJWR  Created version 1.0.
 *  06/05/87  PJWR  Corrected to use GKS drawing area of bitmap rather than
 *                  entire bitmap.
 */

#include <stdio.h>
#include "./varinc/wwinfo.h"
#include "../../system/include/f77_type.h"
#include "../../system/include/gkdt.h"
#include "../../system/include/gkwca.h"
#include "../../system/include/gkwdt.h"

/*
 * Comments:
 *
 *  Assumes that the default pointers have been properly configured with
 *  gk9sww_() or an equivalent.
 */

f77_integer gk9scd_()
{
  box frame;			/* For use by bmbox() */

  /* Initialise frame to match the GKS drawing area */

  frame.b_left = 0;
  frame.b_top = 0;
  frame.b_right = gkywdt_.kdsrx[gkywca_.kwkix - 1] - 1;
  frame.b_bottom = gkywdt_.kdsry[gkywca_.kwkix - 1] - 1;

  /* Clear default bitmap */

  bmbox(frame, BMCLEARALL);

  return((f77_integer)0);
}
