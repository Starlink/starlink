/*
 * gk9swd.h: C definitions and macros for the Sun workstation driver.
 *
 * Maintenance Log:
 *
 *  03/04/87  PJWR  Created to simplify maintenance.  The initial contents
 *                  are from ACA's code.
 *  04/11/87  PJWR  Revised for second release.
 */

/*
 * Definitions for Input Functions:
 */

#define break_char '\032'		/* GKS input break character */

/*
 * Macros for Input Functions:
 *
 * gk9sbu()	Macro to wait for mouse buttons to be up. Called in each
 *              input tool routine before waiting for any other input.
 */

#define gk9sbu() { while (dd->d_buttons != 0) ipwait(); }

void gk9soe();				/* Open echo area */
void gk9sce();				/* Close echo area */

/*
 * Define structure used during input functions to select operation on PET
 */
struct ecb {
  int (*open)(),(*echo)(),(*erase)(),(*close)()};

/*
 * Workstation workspace offsets for raster font details.  These must be one
 * less than the correspondingly named PARAMETER in gk9swd.f
 */

#define ICHSIZ	0	/* Font size key in integer workspace */
#define ICHHT	2	/* Geometry details keys from real workspace */
#define ICHWD	3
#define	ICHCT	4
#define	ICHBB	5
#define	ICHGS	6

/*
 * Workstation workspace offsets for line style/width details.  These must
 * be one less than the correspondingly named PARAMETER in gk9swd.f
 */

#define ILNTY	5	/* Line type in integer workspace */
#define ILNWD	6	/* Line width (pixels) in integer workspace */

/*
 * Function codes for gk9sww_.  These should be the same as the values of the
 * correspondingly named PARAMETERs in gk9swd.f.
 */

#define ICREAT	1	/* Create workstation */
#define ISELCT	2	/* Select workstation display surface */
#define IDELET	3	/* Destroy workstation */
#define IUPDAT	4	/* Update workstation display surface */

/*
 * Workstation workspace offsets for display update area.  These should be one
 * less than the correspondingly named PARAMETERs in gk9swd.f
 */

#define ILEFT	1	/* Left bound in integer workspace */
#define IRIGHT	2	/* Right bound in integer workspace */
#define ITOP	3	/* Top bound in integer workspace */
#define IBOTT	4	/* Bottom bound in integer workspace */
