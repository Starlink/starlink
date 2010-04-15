/*
 * RAL GKS SYSTEM
 *
 * gk9ssr_:  Resets polyline with width and style 1.
 *
 * Type of Routine:  Part of WORKSTATION DRIVER
 * Author:           TAW
 *
 * Copyright (C) SERC 1988
 *
 * Maintenance Log:
 *
 *  11/11/88  TAW   Version 1 stabilised.


 */
#include <pixrect/pixrect_hs.h>			/* For pixrect structures */
#include "../../system/include/f77_type.h"	/* For f77_integer */

/*
 * Errors:
 *
 * Comments:
 *   Called at end of polyline entry point to reset the width to 1 and the
 *   style to be NULL.
 */
extern Pr_texture      *style;
extern Pr_brush
    brush;
extern int cached_style;

f77_integer gk9ssr_()
{
    cached_style = 1;
    style = (Pr_texture *)0;    /* Texture pointer (implies solid line) */
    brush.width = 1;		/* Width cache */
}
