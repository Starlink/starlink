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
*  Copyright:
*     Copyright (C) 1991, 1992 Science & Engineering Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

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

