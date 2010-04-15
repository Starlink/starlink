/*
 * RAL GKS SYSTEM
 *
 * gk0xpa_: Performs INQUIRE PIXEL ARRAY for the Xlib WS driver.
 *
 * Type of Routine:  Part of WS DRIVER
 * Author:           TAW
 *
 * Copyright (C) SERC 1988
 *
 * Maintenance Log:
 * 20/09/88	TAW	Dummy routine.
 *
*/

#include <wwinfo.h>
#include "../../system/include/f77_type.h"
#include "../../system/include/fa.h"
#include "../../system/include/gks.h"
#include "../../system/include/gkdt.h"
#include "../../system/include/gkerr.h"
#include "../../system/include/gkwca.h"
#include "../../system/include/gkwdt.h"
#include "../../system/include/gkwkd.h"
#include "../../system/include/gkwsl.h"

/*
 * Comments:
 *
 *  This routine will require modification when NeWS appears,  as it uses
 *  pixrects.
 */

f77_integer gk0xpa_(x0, y0, data)
  f77_integer *x0;		/* X coord. of TLC of pixel array (Input) */
  f77_integer *y0;		/* Y coord. of TLC of pixel array (Input) */
  f77_integer **data;		/* Pixel array (Output) */
{

  gkyerr_.kerror = -9999;
  return((f77_integer)0);
}
