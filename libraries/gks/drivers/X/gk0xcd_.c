/*
 * RAL GKS SYSTEM
 *
 * gk0xcd_: Clear display surface using WW.
 *
 * Type of Routine:  Part of WS DRIVER
 * Author:           TAW
 *
 * Copyright (C) SERC 1988
 *
 * Maintenance Log:
 *
 *  10/08/88  TAW   Copied from ../sun/gk0xcd_.c.
 */

#include <stdio.h>
#include <wwinfo.h>
#include "../../system/include/f77_type.h"
#include "../../system/include/gkdt.h"
#include "../../system/include/gkwca.h"
#include "../../system/include/gkwdt.h"

/*
 * Comments:
 *
 *  Assumes that the default pointers have been properly configured with
 *  gk0xww_() or an equivalent.
 */

f77_integer gk0xcd_()
{
  box frame;			/* For use by bmbox() */

  /* Initialise frame to match the GKS drawing area */

  frame.b_left = 0;
  frame.b_top = 0;
  frame.b_right = gkywdt_.kdsrx[gkywca_.kwkix - 1] - 1;
  frame.b_bottom = gkywdt_.kdsry[gkywca_.kwkix - 1] - 1;

  /* Clear default bitmap */

  gk0xbmbox(frame, BMCLEARALL);

  return((f77_integer)0);
}
