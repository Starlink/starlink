/*
*+
*  Name:
*     gwm_for.h
*
*  Purpose:
*     Global variables for the GWM FORTRAN interface
*
*  Language:
*     C
*
*  Type of Module:
*     C include file
*
*  Description:
*     Global variables for the GWM FORTRAN interface.
*
*  Authors:
*     DLT: David Terrett (Starlink RAL)
*     {enter_new_authors_here}
*
*  History:
*      20-OCT-1993 (DLT):
*        Original version.
*     {enter_further_changes_here}
*
*  Bugs:
*     {note_any_bugs_here}
*-
*/

/* Display identifier */
extern Display *GWM_display_id;

/* X error handler */
extern int (*X_error_handler)();

extern struct wc GWM_wc;

