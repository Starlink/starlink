/*
*+
*  Name:
*     gwm_err.h
*
*  Purpose:
*     Symbolic names for GWM errors
*
*  Language:
*     C
*
*  Type of Module:
*     C include file
*
*  Description:
*     Symbolic names for GWM errors
*
*  Authors:
*     DLT: D.L. Terrett (RAL)
*     NE: Nick Eaton (Durham University)
*     {enter_new_authors_here}
*
*  History:
*      9-AUG-1991 (DLT):
*        Original version.
*      2-OCT-1991 (NE):
*        Added FORTRAN level codes.
*     {enter_further_changes_here}
*
*  Bugs:
*     {note_any_bugs_here}
*-
*/

/*
*     Low level codes.
*
*     NOTE: These are not ADAM/EMS error numbers and GWM does not report
*     any errors with EMS or ERR. This is because GWM is used by very low
*     level software such as GKS device handlers. The FORTRAN interface to
*     GWM will translate the error numbers to EMS codes and report errors
*/

#define GWM_SUCCESS		0
#define GWM_WINDOW_EXISTS	1   /* Window already exists		    */
#define GWM_MEM_ALLOC		2   /* Memory allocation failure 	    */
#define GWM_COL_ALLOC		3   /* Colour cell allocation failed	    */
#define GWM_WIN_CREATE		4   /* Window creation failed		    */
#define GWM_PIX_CREATE		5   /* Pixmap creation failed		    */
#define GWM_WIN_NAME		6   /* Window has no name property	    */
#define GWM_WIN_NOEXIST		7   /* Window does not exist		    */
#define GWM_NOT_GWMWIN		8   /* Not a SWM window			    */
#define GWM_WRONG_NAME		9   /* Window has wrong name		    */
#define GWM_NO_PIXMAP		10  /* No pixmap id on window		    */
#define GWM_NO_OFFSET		11  /* No offsets on window		    */
#define GWM_INV_WINID		12  /* Invalid window id		    */
#define GWM_INV_PIXID		13  /* Invalid pixmap id		    */
#define GWM_NO_WIN_NAME		14  /* No window name			    */
#define GWM_CHILD_DEAD		15  /* Child process died		    */
#define GWM_SS_ERR		16  /* System service error		    */
#define GWM_BADOPT		17  /* Bad command option		    */
#define GWM_DUP_NAME		18  /* Duplicate window name		    */
#define GWM_NO_COLTAB		19  /* No colour table		   	    */
#define GWM_NO_DISPLAY		20  /* Unable to open display		    */
#define GWM_BAD_COLOUR		21  /* Bad colour specification		    */
#define GWM_OVNOTSUP		22  /* Overlays not supported 		    */
#define GWM_INVOV		23  /* Invalid number of overlays	    */
#define GWM_NO_OVMASK		24  /* No overlay mask on window	    */
#define GWM_NO_OVOFFSET		25  /* No overlay offset on window	    */
#define GWM_NO_FOREGROUND	26  /* No foreground colour property	    */
#define GWM_NO_BACKGROUND	27  /* No background colour property	    */
/*
*     FORTRAN level codes.
*
*     These are constructed from the equation
*     CODE = 134250498 + 65536 * <fac> + 8 * <mes>
*     where <mes> is the message number in the range 1 to 4095
*     and <fac> is the facility number for GWM (1512).
*/

#define GWM__ERROR       233340938  /* Undefined error condition */
#define GWM__INOPT       233340946  /* Invalid option */
#define GWM__NODIS       233340954  /* Unable to open display */
#define GWM__NOWIN       233340962  /* Unable to open window */
#define GWM__WINNF       233340970  /* Window not found */
#define GWM__XERR        233340978  /* X error */

