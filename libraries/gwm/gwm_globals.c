#include <X11/Xlib.h>
#include "gwm_for.h"
/*
*+
*  Name:
*     gwm_globals.c
*
*  Purpose:
*     Global variables for the GWM FORTRAN interface
*
*  Language:
*     C
*
*  Type of Module:
*     C 
*
*  Description:
*     Global variables for the GWM FORTRAN interface. The definition in
*     this file must match those in gwm_for.h
*
*  Authors:
*     DLT: David Terrett (Starlink RAL)
*     {enter_new_authors_here}
*
*  History:
*      7-NOV-1991 (DLT):
*        Original version.
*      14-NOV-1991 (DLT):
*        Add overlay support
*      14-NOV-1991 (DLT):
*        Add negative origin support
*      18-NOV_1991 (DLT):
*        Add borderwidth
*      06-APR-1992 (DLT):
*        Add iconic
*     {enter_further_changes_here}
*
*  Bugs:
*     {note_any_bugs_here}
*-
*/

/* Display identifier */
Display *GWM_display_id;

/* X error handler */
int (*X_error_handler)();

/* Structure for defining window characteristics. */
/* The types are I - integer, L - logical, C - character string */
/* The names of the characteristics are defined in the initialisation. */
/* The initialised values must be in upper case. */
/* The yesno array indicates if a characteristic has been set. */
/* The values array contains the set value of the characteristic. */
/* These are coerced into being integers for ease of storage. */
struct wc GWM_wc = { "I","I","I","I","I","C","C","L","C","L","L","L","C","I",
                     "I","I","L",
                     "WIDTH","HEIGHT","XORIGIN","YORIGIN","COLOURS",
                     "BACKGROUND","FOREGROUND","INTERACTIVE","TITLE",
		     "NOINTERACTIVE","OVERLAY","NOOVERLAY","OVCOLOUR",
		     "XSIGN", "YSIGN", "BORDERWIDTH", "ICONIC"};

