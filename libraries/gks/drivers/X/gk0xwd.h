/*
 * gk0xwd.h: C definitions and macros for the X workstation driver.
 *
 * Maintenance Log:
 *
 *  10/08/88  TAW   Based on ../sun/gk9swd.h modified for Xlib.
 */

/*
 * Definitions for Input Functions:
 */

#define break_char '\032'		/* GKS input break character */

/*
 * Macros for Input Functions:
 *
 * gk0xbu()	Macro to wait for mouse buttons to be up. Called in each
 *              input tool routine before waiting for any other input.
 */

#define gk0xbu() { while (dd->d_buttons != 0) gk0xipwait(); }

void gk0xoe();				/* Open echo area */
void gk0xce();				/* Close echo area */

/*
 * Define structure used during input functions to select operation on PET
 */
struct ecb {
  int (*open)(),(*echo)(),(*erase)(),(*close)()};

/*
 * Workstation workspace offsets for raster font details.  These must be one
 * less than the correspondingly named PARAMETER in gk0xwd.f
 */

#define ICHSIZ	0	/* Font size key in integer workspace */
#define ICHHT	2	/* Geometry details keys from real workspace */
#define ICHWD	3
#define	ICHCT	4
#define	ICHBB	5
#define	ICHGS	6

/*
 * Workstation workspace offsets for line style/width details.  These must
 * be one less than the correspondingly named PARAMETER in gk0xwd.f
 */

#define ILNTY	5	/* Line type in integer workspace */
#define ILNWD	6	/* Line width (pixels) in integer workspace */

/*
 * Function codes for gk0xww_.  These should be the same as the values of the
 * correspondingly named PARAMETERs in gk0xwd.f.
 */

#define ICREAT	1	/* Create workstation */
#define ISELCT	2	/* Select workstation display surface */
#define IDELET	3	/* Destroy workstation */
#define IUPDAT	4	/* Update workstation display surface */

/*
 * Workstation workspace offsets for display update area.  These should be one
 * less than the correspondingly named PARAMETERs in gk0xwd.f
 */

#define ILEFT	1	/* Left bound in integer workspace */
#define IRIGHT	2	/* Right bound in integer workspace */
#define ITOP	3	/* Top bound in integer workspace */
#define IBOTT	4	/* Bottom bound in integer workspace */
