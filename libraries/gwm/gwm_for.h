/*
*+
*  Name:
*     gwm_for.h
*
*  Purpose:
*     Structure and macro definitions for the GWM FORTRAN interface
*
*  Language:
*     C
*
*  Type of Module:
*     C include file
*
*  Description:
*     Global variables for the GWM FORTRAN interface. The variable
*     definitions in this file must match those in gwm_globals.c except
*     that the definitions in that file do not have extern.
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
*     NE: Nick Eaton (Durham University)
*     DLT: David Terrett (Starlink RAL)
*     {enter_new_authors_here}
*
*  History:
*      2-OCT-1991 (NE):
*        Original version.
*      7-NOV-1991 (DLT):
*        Add extern to global variable definitions and remove initialisation
*      14-NOV-1991 (DLT):
*        Add overlay support
*      14-NOV-1991 (DLT):
*        Add negative origin support
*      18-NOV-1991 (DLT):
*        Add borderwidth
*      06-APR-1992 (DLT):
*        Add iconic
*     {enter_further_changes_here}
*
*  Bugs:
*     {note_any_bugs_here}
*-
*/

/* The number of window characteristics defined */
#define wc_nchars      17

/* Structure for defining window characteristics. */
/* The yesno array indicates if a characteristic has been set. */
/* The values array contains the set value of the characteristic. */
struct wc
   {
   char *types[wc_nchars];
   char *names[wc_nchars];
   int yesno[wc_nchars];
   union
   {
   	int ival;
	char *cval;
   } values[wc_nchars];
   };

/* Definition of the position of the characteristic in the arrays */
#define wc_width          0
#define wc_height         1
#define wc_xorigin        2
#define wc_yorigin        3
#define wc_colours        4
#define wc_background     5
#define wc_foreground     6
#define wc_interactive    7
#define wc_title          8
#define wc_nointeractive  9
#define wc_overlay       10
#define wc_nooverlay     11
#define wc_ovcolour      12
#define wc_xsign         13
#define wc_ysign         14
#define wc_borderwidth   15
#define wc_iconic        16
