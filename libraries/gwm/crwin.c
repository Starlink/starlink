#include <X11/Xlib.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "sae_par.h"
#include "gwm.h"
#include "gwm_err.h"
#include "gwm_for.h"
#include "cnf.h"
#include "f77.h"
#include "ems.h"

/******************************************************************************/

F77_SUBROUTINE(gwm_crwin) ( CHARACTER(wname), INTEGER(status) TRAIL(wname) )

/*
*+
*  Name:
*     GWM_CRWIN
*
*  Purpose:
*     Create a GWM window.
*
*  Language:
*     C
*
*  Invocation:
*     CALL GWM_CRWIN( WNAME, STATUS )
*
*  Description:
*     Create a GWM window. A window with the given name is created on
*     the current X display. The window is created with the default
*     characteristics unless they have been set using the GWM_WSETx
*     routines. After the window has been created any set characteristics
*     are reset to their default values.
*
*  Arguments:
*     WNAME = CHARACTER * ( * ) (Given)
*        The window name.
*     STATUS = INTEGER (Given and Returned)
*        The global status.
*
*  Copyright:
*     Copyright (C) 1191, 1991, 1992 Science & Engineering Research Council.
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
*     DLT: David Terrett (Rutherford Appleton Laboratory)
*     {enter_new_authors_here}
*
*  History:
*      2-OCT-1991 (NE):
*        Orignal version
*      1-NOV-1191 (DLT):
*        Make qualifier values seperate arguments
*      7-NOV-1991 (DLT):
*        global variable definitions moved to function body
*      14-NOV-1991 (DLT):
*        Add overlay support
*      15-NOV-1991 (DLT):
*        Allow -0 for origin specification
*      18-NOV-1991 (DLT):
*        Add borderwidth
*      06-APR-1992 (DLT):
*        Add iconic
*     {enter_changes_here}
*
*  Bugs:
*     {note_any_bugs_here}
*-
*/

{
#include "gwm_global.h"

GENPTR_CHARACTER(wname)
GENPTR_INTEGER(status)

/* Local variables */
int i, lstat, narg, usegeom, sign;
char *argv[wc_nchars*2], bg[32], cols[32], fg[32], geom[32], bw[32],
     lname[32], *lwname, temp[32], title[256], ovcolour[32];

/* Check status on entry */
if ( *status != SAI__OK ) return;

/* Copy the FORTRAN string to a local C string */
lwname = cnf_creim( wname, wname_length );

/* Define the arguments to create the window */
argv[0] = "GWM";
argv[1] = lwname;
narg = 2;

/* Create a string containing the number of colours */
if ( GWM_wc.yesno[wc_colours] == 1 )
   {
   argv[narg++] = "-colours";
   sprintf( cols, "%d", GWM_wc.values[wc_colours].ival );
   argv[narg++] = cols;
   }

/* Create a string containing the foreground colour */
if ( GWM_wc.yesno[wc_foreground] == 1 )
   {
   argv[narg++] = "-foreground";
   sprintf( fg, "%s", GWM_wc.values[wc_foreground].cval );
   argv[narg++] = fg;
   }

/* Create a string containing the background colour */
if ( GWM_wc.yesno[wc_background] == 1 )
   {
   argv[narg++] = "-background";
   sprintf( bg, "%s", GWM_wc.values[wc_background].cval );
   argv[narg++] = bg;
   }

/* Create a string containing the interactive option however */
/* if the value is zero do not set the option */
if ( ( GWM_wc.yesno[wc_interactive] == 1 ) &&
     ( GWM_wc.values[wc_interactive].ival == 1 ) )
   {
   argv[narg++] = "-interactive";
   }

/* Create a string containing the nointeractive option however */
/* if the value is zero do not set the option */
if ( ( GWM_wc.yesno[wc_nointeractive] == 1 ) &&
     ( GWM_wc.values[wc_nointeractive].ival == 1 ) )
   {
   argv[narg++] = "-nointeractive";
   }

/* Create a string containing the title option */
if ( GWM_wc.yesno[wc_title] == 1 )
   {
   argv[narg++] = "-title";
   sprintf( title, "%s", GWM_wc.values[wc_title].cval );
   argv[narg++] = title;
   }

/* Create a string containing the border width */
if ( GWM_wc.yesno[wc_borderwidth] == 1 )
   {
   argv[narg++] = "-borderwidth";
   sprintf( bw, "%d", GWM_wc.values[wc_borderwidth].ival );
   argv[narg++] = bw;
   }

/* Build up a string containing the geometry */
usegeom = 0;
geom[0] = '\0';
if ( GWM_wc.yesno[wc_width] == 1 )
   {
   sprintf( temp, "%d", GWM_wc.values[wc_width].ival );
   strcat( geom, temp );
   usegeom = 1;
   }
if ( GWM_wc.yesno[wc_height] == 1 )
   {
   sprintf( temp, "x%d", GWM_wc.values[wc_height].ival );
   strcat( geom, temp );
   usegeom = 1;
   }
if ( GWM_wc.yesno[wc_xorigin] == 1 )
   {
   if ( GWM_wc.yesno[wc_xsign] == 1 )
      sign = GWM_wc.values[wc_xsign].ival >= 0;
   else
      sign = GWM_wc.values[wc_xorigin].ival >= 0;
   if (sign)
      sprintf( temp, "+%d", abs(GWM_wc.values[wc_xorigin].ival) );
   else
      sprintf( temp, "-%d", abs(GWM_wc.values[wc_xorigin].ival) );
   strcat( geom, temp );
   usegeom = 1;
   }
if ( GWM_wc.yesno[wc_yorigin] == 1 )
   {
   if ( GWM_wc.yesno[wc_ysign] == 1 )
      sign = GWM_wc.values[wc_ysign].ival >= 0;
   else
      sign = GWM_wc.values[wc_yorigin].ival >= 0;
   if (sign)
      sprintf( temp, "+%d", abs(GWM_wc.values[wc_yorigin].ival) );
   else
      sprintf( temp, "-%d", abs(GWM_wc.values[wc_yorigin].ival) );
   strcat( geom, temp );
   usegeom = 1;
   }
if ( usegeom == 1 )
   {
   argv[narg++] = "-geometry";
   argv[narg++] = geom;
   }

/* Create a string containing the overlay option however */
/* if the value is zero do not set the option */
if ( ( GWM_wc.yesno[wc_overlay] == 1 ) &&
     ( GWM_wc.values[wc_overlay].ival == 1 ) )
   {
   argv[narg++] = "-overlay";
   }

/* Create a string containing the nooverlay option however */
/* if the value is zero do not set the option */
if ( ( GWM_wc.yesno[wc_nooverlay] == 1 ) &&
     ( GWM_wc.values[wc_nooverlay].ival == 1 ) )
   {
   argv[narg++] = "-nooverlay";
   }

/* Create a string containing the overlay colour */
/* if the value is zero do not set the option */
if ( GWM_wc.yesno[wc_ovcolour] == 1 )
   {
   argv[narg++] = "-ovcolour";
   sprintf( ovcolour, "%s", GWM_wc.values[wc_ovcolour].cval );
   argv[narg++] = ovcolour;
   }

/* Create a string containing the iconic option however */
/* if the value is zero do not set the option */
if ( ( GWM_wc.yesno[wc_iconic] == 1 ) &&
     ( GWM_wc.values[wc_iconic].ival == 1 ) )
   {
   argv[narg++] = "-iconic";
   }

/* Terminate the arguments with a null */
argv[narg] = NULL;

/* Create the window */
lstat = GWM_CreateWindow( narg, argv, &GWM_display_id, lname );

/* Free the CNF resources */
cnf_free( lwname );

/* Reset the window characteristics to accept the defaults */
for ( i = 0; i < wc_nchars; i++ ) GWM_wc.yesno[i] = 0;

/* Return an error if the GWM function failed */
if ( lstat != GWM_SUCCESS )
   {
   *status = GWM__NOWIN;
   emsSetnc( "TOKEN", lwname, wname_length );
   emsRep( "GWM_CRWIN_NOWIN", "Unable to create GWM window : ^TOKEN",
              status );
   return;
   }

return;
}
