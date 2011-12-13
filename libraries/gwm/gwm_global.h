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
*  Copyright:
*     Copyright (C) 1993 Science & Engineering Research Council.
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

