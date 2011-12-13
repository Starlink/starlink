/*
*+
*  Name:
*     grf_ccdpack.c

*  Purpose:
*     Implement the grf module for CCDPACK.

*  Language:
*     Starlink C

*  Description:
*     This file implements the low level graphics functions required
*     by the rest of AST, by calling suitable PGPLOT functions (the
*     FORTRAN PGPLOT interface is used).
*
*     This file is derived from, and follows very closely, the default
*     PGPLOT implementation in grf_pgplot.c distributed with AST.
*     It differs in its handling of text background colour.

*  Copyright:
*     Copyright (C) 2000 Central Laboratory of the Research Councils

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     DSB: David S. Berry (Starlink)
*     RFWS: R.F. Warren-Smith (Starlink)
*     MBT: Mark Taylor (Starlink)
*     {enter_new_authors_here}

*  History:
*     27-JUN-1996 (DSB):
*        Original version.
*     13-NOV-1996 (DSB):
*        Use C wrappers for PGPLOT functions.
*     15-NOV-1996 (RFWS):
*        Merged the C interface to PGPLOT into this file so that the
*        interface functions can be static.
*     7-OCT-1997 (DSB):
*        Corrected astGText and astGTxExt, by including a check for
*        reversed axes. Previously, the up-vector was used as supplied
*        even if the axes had been reversed.
*     15-OCT-1997 (DSB):
*        o  Corrected astGText and astGTxExt to take account of non-equal
*        scales on the two axes.
*        o  Modified astGTxExt so that it includes any leading or trailing
*        spaces in the returned box.
*        o  Added astGAxScale.
*     28-OCT-1998 (DSB):
*        o  Changed interpretation of the Width attribute from inches, to
*        a multiple of a small line width.
*        o  Wrapper for pgplot F77 subroutine PGQVSZ added.
*     10-JAN-2001 (MBT):
*        Renamed to grf_ccdpack.c and modified text background colour
*        handling.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

/* Macros */
/* ====== */
#define astCLASS                 /* Make protected functions available */
#define MXSTRLEN 80              /* String length at which truncation starts
                                    within pgqtxt and pgptxt. */

/* Header files. */
/* ============= */
/* Interface definitions. */
/* ---------------------- */
#include "cnf.h"                 /* FORTRAN <-> C interface macros (SUN/209) */
#include "ast.h"                 /* AST constants */
#include "grf.h"                 /* Interface to this module */

/* C header files. */
/* --------------- */
#include <float.h>
#include <math.h>
#include <string.h>

/* Macro implementation of astStringExport function. */
/* ------------------------------------------------- */
#define astStringExport(src,dest,len) cnfExprt(src,dest,len)

/* Function Prototypes. */
/* ==================== */
/* These define a local C interface to the PGPLOT library. */
static void ccpgline(int n, float xpts[], float ypts[] );
static void ccpgpt(int n, float xpts[], float ypts[], int symbol);
static void ccpgptxt(float x, float y, float angle, float fjust, char *text );
static void ccpgqcf(int *cf);
static void ccpgqch(float *ch);
static void ccpgqci(int *ci);
static void ccpglen(int units, char *text, float *xl, float *yl);
static void ccpgqcs(int units, float *xch, float *ych);
static void ccpgqls(int *ls);
static void ccpgqlw(int *lw);
static void ccpgqtbg(int *tbci);
static void ccpgqtxt(float x, float y, float angle, float fjust, char *text, float xbox[], float ybox[]);
static void ccpgqvp(int units, float *x1, float *x2, float *y1, float *y2);
static void ccpgqvsz(int units, float *x1, float *x2, float *y1, float *y2);
static void ccpgqwin(float *x1, float *x2, float *y1, float *y2);
static void ccpgscf(int cf);
static void ccpgsch(float ch);
static void ccpgsci(int ci);
static void ccpgsls(int ls);
static void ccpgslw(int lw);
static void ccpgstbg(int tbci);
static void ccpgupdt( void );

/* These describe the native Fortran interface to the PGPLOT library. The
   macros used come from the "f77.h" include file. */
F77_SUBROUTINE(pgline)( INTEGER(n), REAL_ARRAY(x), REAL_ARRAY(y) );
F77_SUBROUTINE(pgpt)( INTEGER(n), REAL_ARRAY(x), REAL_ARRAY(y), INTEGER(TYPE) );
F77_SUBROUTINE(pgptxt)( REAL(x), REAL(y), REAL(angle), REAL(fjust), CHARACTER(text) TRAIL(text) );
F77_SUBROUTINE(pgqcf)( INTEGER(ival) );
F77_SUBROUTINE(pgqch)( REAL(rval) );
F77_SUBROUTINE(pgqci)( INTEGER(ival) );
F77_SUBROUTINE(pgqcs)( INTEGER(units), REAL(chv), REAL(chh) );
F77_SUBROUTINE(pglen)( INTEGER(units), CHARACTER(text), REAL(xl), REAL(yl) TRAIL(text) );
F77_SUBROUTINE(pgqls)( INTEGER(ival) );
F77_SUBROUTINE(pgqlw)( INTEGER(ival) );
F77_SUBROUTINE(pgqtbg)( INTEGER(tbg) );
F77_SUBROUTINE(pgqtxt)( REAL(x), REAL(y), REAL(angle), REAL(fjust), CHARACTER(text), REAL_ARRAY(xbox), REAL_ARRAY(ybox) TRAIL(text) );
F77_SUBROUTINE(pgqvp)( INTEGER(units), REAL(vx1), REAL(vx2), REAL(vy1), REAL(vy2) );
F77_SUBROUTINE(pgqvsz)( INTEGER(units), REAL(x1), REAL(x2), REAL(y1), REAL(y2) );
F77_SUBROUTINE(pgqwin)( REAL(wx1), REAL(wx2), REAL(wy1), REAL(wy2) );
F77_SUBROUTINE(pgscf)( INTEGER(ival) );
F77_SUBROUTINE(pgsch)( REAL(rval) );
F77_SUBROUTINE(pgsci)( INTEGER(ival) );
F77_SUBROUTINE(pgsls)( INTEGER(ival) );
F77_SUBROUTINE(pgslw)( INTEGER(ival) );
F77_SUBROUTINE(pgstbg)( INTEGER(tbg) );
F77_SUBROUTINE(pgupdt)( );

/* Externally visible functions. */
/* ============================= */
/* These implement the "grf" interface in terms of the local C interface
   to PGPLOT. */
int astGFlush( void ){
/*
*+
*  Name:
*     astGFlush

*  Purpose:
*     Flush all pending graphics to the output device.

*  Language:
*     Starlink C

*  Description:
*     This function ensures that the display device is up-to-date,
*     by flushing any pending graphics to the output device.

*  Returned Value:
*     A value of 0 is returned if an error occurs, and 1 is returned
*     otherwise.

*  Parameters:
*     None.

*  Synopsis:
*     #include "grf.h"
*     int astGFlush( void )

*  Copyright:
*     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     {original_author_entry}

*  History:
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

   ccpgupdt();
   return 1;
}

int astGLine( int n, const float *x, const float *y ){
/*
*+
*  Name:
*     astGLine

*  Purpose:
*     Draw a polyline (i.e. a set of connected lines).

*  Language:
*     Starlink C

*  Description:
*     This function displays lines joining the given positions.

*  Returned Value:
*     A value of 0 is returned if an error occurs, and 1 is returned
*     otherwise.

*  Notes:
*     -  Nothing is done if "n" is less than 2, or if a NULL pointer is
*     given for either "x" or "y".

*  Parameters:
*     n
*        The number of positions to be joined together.
*     x
*        A pointer to an array holding the "n" x values.
*     y
*        A pointer to an array holding the "n" y values.

*  Synopsis:
*     #include "grf.h"
*     int astGLine( int n, const float *x, const float *y )

*  Copyright:
*     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     {original_author_entry}

*  History:
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

   if( n > 1 && x && y ) ccpgline( n, (float *) x, (float *) y );
   return 1;
}

int astGMark( int n, const float *x, const float *y, int type ){
/*
*+
*  Name:
*     astGMark

*  Purpose:
*     Draw a set of markers.

*  Language:
*     Starlink C

*  Description:
*     This function displays markers at the given positions.

*  Returned Value:
*     A value of 0 is returned if an error occurs, and 1 is returned
*     otherwise.

*  Notes:
*     -  Nothing is done if "n" is less than 1, or if a NULL pointer is
*     given for either "x" or "y".

*  Parameters:
*     n
*        The number of markers to draw.
*     x
*        A pointer to an array holding the "n" x values.
*     y
*        A pointer to an array holding the "n" y values.
*     type
*        An integer which can be used to indicate the type of marker symbol
*        required.

*  Synopsis:
*     #include "grf.h"
*     int astGMark( int n, const float *x, const float *y, int type )

*  Copyright:
*     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     {original_author_entry}

*  History:
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

   if( n > 0 && x && y ) ccpgpt( n, (float *) x, (float *) y, type );
   return 1;
}

int astGText( const char *text, float x, float y, const char *just,
              float upx, float upy ){
/*
*+
*  Name:
*     astGText

*  Purpose:
*     Draw a character string.

*  Language:
*     Starlink C

*  Description:
*     This function displays a character string at a given position
*     using a specified justification and up-vector.

*  Returned Value:
*     A value of 0 is returned if an error occurs, and 1 is returned
*     otherwise.

*  Notes:
*     -  Any graphics within the rotated box enclosing the text are erased.
*     -  A NULL value for "just" causes a value of "CC" to be used.
*     -  Both "upx" and "upy" being zero causes an error.
*     -  Any unrecognised character in "just" causes an error.

*  Parameters:
*     text
*        Pointer to a null-terminated character string to be displayed.
*     x
*        The reference x coordinate.
*     y
*        The reference y coordinate.
*     just
*        A character string which specifies the location within the
*        text string which is to be placed at the reference position
*        given by x and y. The first character may be 'T' for "top",
*        'C' for "centre", or 'B' for "bottom", and specifies the
*        vertical location of the reference position. Note, "bottom"
*        corresponds to the base-line of normal text. Some characters
*        (eg "y", "g", "p", etc) descend below the base-line. The second
*        character may be 'L' for "left", 'C' for "centre", or 'R'
*        for "right", and specifies the horizontal location of the
*        reference position. If the string has less than 2 characters
*        then 'C' is used for the missing characters.
*     upx
*        The x component of the up-vector for the text, in graphics world
*        coordinates. If necessary the supplied value should be negated
*        to ensure that positive values always refer to displacements from
*        left to right on the screen.
*     upy
*        The y component of the up-vector for the text, in graphics world
*        coordinates. If necessary the supplied value should be negated
*        to ensure that positive values always refer to displacements from
*        bottom to top on the screen.

*  Synopsis:
*     #include "grf.h"
*     int astGText( const char *text, float x, float y, const char *just,
*                   float upx, float upy )

*  Copyright:
*     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     {original_author_entry}

*  History:
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

/* Local Variables: */
   char lj[ 2 ];
   float uplen, xbox[ 4 ], ybox[ 4 ];
   float angle, fjust, hu, test, alpha, beta;
   int i, tbg;

/* Check that there is something to draw. */
   if( text && text[ 0 ] != 0 ){

/* Fill in any missing parts of the justification string. */
      if( just ){
         if( just[ 0 ] == 'T' || just[ 0 ] == 'C' || just[ 0 ] == 'B' ){
            lj[ 0 ] = just[ 0 ];
         } else {
            astError( AST__GRFER, "astGText: Justification string '%s' is "
                      "invalid.", just );
            return 0;
         }

         if( just[ 1 ] == 'L' || just[ 1 ] == 'C' || just[ 1 ] == 'R' ){
            lj[ 1 ] = just[ 1 ];
         } else {
            astError( AST__GRFER, "astGText: Justification string '%s' "
                      "is invalid.", just );
            return 0;
         }

      } else {
         lj[ 0 ] = 'C';
         lj[ 1 ] = 'C';
      }

/* Find the conversion factors between increment in world coordinate axes,
   and the corresponding increments in millimetres ( Xmm = alpha*Xworld,
   Ymm = beta*Yworld ). */
      if( !astGAxScale( &alpha, &beta ) ) return 0;

/* If either axis is reversed, reverse the supplied up-vector components
   so that they refer to the world-coordinates axes. */
      if( alpha < 0.0 ) upx = -upx;
      if( beta < 0.0 ) upy = -upy;

/* Get the angle between the text base-line and horizontal. */
      angle = atan2( -(double) upx*alpha, (double) upy*beta )*AST__DR2D;

/* Get the fractional horizontal justification as needed by PGPLOT. */
      if( lj[ 1 ] == 'L' ) {
         fjust = 0.0;
      } else if( lj[ 1 ] == 'R' ) {
         fjust = 1.0;
      } else {
         fjust = 0.5;
      }

/* Unless the requested justification is "Bottom", we need to adjust
   the supplied reference position before we use it with PGPLOT because
   PGPLOT assumes "Bottom" justification. */
      if( lj[0] != 'B' ) {

/* Get the bounding box of the string. Note, only the size of the box is
   significant here, not its position. Also note, leading and trailing
   spaces are not included in the bounding box. */
         ccpgqtxt( x, y, angle, fjust, (char *) text, xbox, ybox );

/* Normalise the up-vector in world coordinates. */
         uplen = sqrt( (double) (upx*upx + upy*upy) );
         if( uplen > 0.0 ){
            upx /= uplen;
            upy /= uplen;
         } else {
            astError( AST__GRFER, "astGText: Zero length up-vector supplied.");
            return 0;
         }

/* Find the height of the text above the base-line. Note, the PGPLOT
   manual is not clear about the order of the corners returned by
   pgqtxt, so we have to find the largest distance between
   the corners in the direction of the supplied up-vector. */
         hu = 0.0;
         for( i = 0; i < 4; i++ ){
            test = upx*( xbox[ i ] - x ) + upy*( ybox[ i ] - y );
            if( test > hu ) hu = test;
         }

/* Adjust the vertical position of the reference point, since PGPLOT
   requires it to be at the bottom of the text. */
         if( lj[ 0 ] == 'T' ){
            x -= upx*hu;
            y -= upy*hu;
         } else if( lj[ 0 ] == 'C' ){
            x -= 0.5*upx*hu;
            y -= 0.5*upy*hu;
         }
      }

/* Display the text, using the currently set PGPLOT background colour. */
      ccpgptxt( x, y, angle, fjust, (char *) text );
   }

/* Return. */
   return 1;
}

int astGAxScale( float *alpha, float *beta ){
/*
*+
*  Name:
*     astGAxScale

*  Purpose:
*     Get the axis scales.

*  Language:
*     Starlink C

*  Description:
*     This function returns two values (one for each axis) which scale
*     increments on the corresponding axis into a "normal" coordinate
*     system in which:
*        1 - The axes have equal scale in terms of (for instance)
*            millimetres per unit distance.
*        2 - X values increase from left to right.
*        3 - Y values increase from bottom to top.

*  Returned Value:
*     A value of 0 is returned if an error occurs, and 1 is returned
*     otherwise.

*  Parameters:
*     alpha
*        A pointer to the location at which to return the scale for the
*        X axis (i.e. Xnorm = alpha*Xworld).
*     beta
*        A pointer to the location at which to return the scale for the
*        Y axis (i.e. Ynorm = beta*Yworld).

*  Synopsis:
*     #include "grf.h"
*     int astGAxScale( float *alpha, float *beta )

*  Copyright:
*     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     {original_author_entry}

*  History:
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

/* Local Variables: */
   float nx1, nx2, ny1, ny2, wx1, wx2, wy1, wy2;
   int ret;

/* Find the conversion factors between increment in world coordinate axes,
   and the corresponding increments in millimetres ( Xmm = alpha*Xworld,
   Ymm = beta*Yworld ). */
   ccpgqvp( 2, &nx1, &nx2, &ny1, &ny2 );
   ccpgqwin( &wx1, &wx2, &wy1, &wy2 );

   if( wx2 != wx1 && wy2 != wy1 &&
       nx2 != nx1 && ny2 != ny1 ) {
      *alpha= ( nx2 - nx1 ) / ( wx2 - wx1 );
      *beta = ( ny2 - ny1 ) / ( wy2 - wy1 );
      ret = 1;
   } else {
      astError( AST__GRFER, "astGAxScale: The graphics window or viewport has zero size." );
      ret = 0;
   }

   return ret;
}

int astGTxExt( const char *text, float x, float y, const char *just,
               float upx, float upy, float *xb, float *yb ){
/*
*+
*  Name:
*     astGTxExt

*  Purpose:
*     Get the extent of a character string.

*  Language:
*     Starlink C

*  Description:
*     This function returns the corners of a box which would enclose the
*     supplied character string if it were displayed using astGText.
*
*     The returned box INCLUDES any leading or trailing spaces.

*  Returned Value:
*     A value of 0 is returned if an error occurs, and 1 is returned
*     otherwise.

*  Notes:
*     -  The order of the corners is anti-clockwise (in world coordinates)
*        starting at the bottom left.
*     -  A NULL value for "just" causes a value of "CC" to be used.
*     -  Both "upx" and "upy" being zero causes an error.
*     -  Any unrecognised character in "just" causes an error.
*     -  Zero is returned for all bounds of the box if an error occurs.

*  Parameters:
*     text
*        Pointer to a null-terminated character string to be displayed.
*     x
*        The reference x coordinate.
*     y
*        The reference y coordinate.
*     just
*        A character string which specifies the location within the
*        text string which is to be placed at the reference position
*        given by x and y. The first character may be 'T' for "top",
*        'C' for "centre", or 'B' for "bottom", and specifies the
*        vertical location of the reference position. Note, "bottom"
*        corresponds to the base-line of normal text. Some characters
*        (eg "y", "g", "p", etc) descend below the base-line. The second
*        character may be 'L' for "left", 'C' for "centre", or 'R'
*        for "right", and specifies the horizontal location of the
*        reference position. If the string has less than 2 characters
*        then 'C' is used for the missing characters.
*     upx
*        The x component of the up-vector for the text, in graphics world
*        coordinates. If necessary the supplied value should be negated
*        to ensure that positive values always refer to displacements from
*        left to right on the screen.
*     upy
*        The y component of the up-vector for the text, in graphics world
*        coordinates. If necessary the supplied value should be negated
*        to ensure that positive values always refer to displacements from
*        bottom to top on the screen.
*     xb
*        An array of 4 elements in which to return the x coordinate of
*        each corner of the bounding box.
*     yb
*        An array of 4 elements in which to return the y coordinate of
*        each corner of the bounding box.

*  Synopsis:
*     #include "grf.h"
*     int astGTxExt( const char *text, float x, float y, const char *just,
*                   float upx, float upy, float *xb, float *yb )

*  Copyright:
*     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     {original_author_entry}

*  History:
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

/* Local Variables: */
   char lj[ 2 ];
   float udx, udy, vdx, vdy, vx, vy, uplen, xbox[ 4 ],
         ybox[ 4 ], uxu, uyu, uxd, uyd, ux, uy;
   float angle, width, test, xl, yl;
   float alpha, beta, xc, yc, hu, hd, a, b;
   int i;

/* Initialise the returned values to indicate no box available. */
   for( i = 0; i < 4; i++ ){
      xb[ i ] = 0.0;
      yb[ i ] = 0.0;
   }

/* Check that there is something to draw. */
   if( text && text[ 0 ] != 0 ){

/* Fill in any missing parts of the justification string. */
      if( just ){
         if( just[ 0 ] == 'T' || just[ 0 ] == 'C' || just[ 0 ] == 'B' ){
            lj[ 0 ] = just[ 0 ];
         } else {
            astError( AST__GRFER, "astGTxExt: Justification string '%s' is "
                      "invalid.", just );
            return 0;
         }

         if( just[ 1 ] == 'L' || just[ 1 ] == 'C' || just[ 1 ] == 'R' ){
            lj[ 1 ] = just[ 1 ];
         } else {
            astError( AST__GRFER, "astGTxExt: Justification string '%s' is "
                      "invalid.", just );
            return 0;
         }

      } else {
         lj[ 0 ] = 'C';
         lj[ 1 ] = 'C';
      }

/* Find the conversion factors between increment in world coordinate axes,
   and the corresponding increments in millimetres ( Xmm = alpha*Xworld,
   Ymm = beta*Yworld ). */
      if( !astGAxScale( &alpha, &beta ) ) return 0;

/* If either axis is reversed, reverse the supplied up-vector components
   so that they refer to the world-coordinates axes. */
      if( alpha < 0.0 ) upx = -upx;
      if( beta < 0.0 ) upy = -upy;

/* Convert the up-vector into millimetres. */
      ux = alpha*upx;
      uy = beta*upy;

/* Normalise the up-vector to a length of 1 millimetre. */
      uplen = sqrt( (double) (ux*ux + uy*uy) );
      if( uplen > 0.0 ){
         ux /= uplen;
         uy /= uplen;
      } else {
         astError( AST__GRFER, "astGText: Zero length up-vector supplied.");
         return 0;
      }

/* Form the base-line vector by rotating the up-vector by 90 degrees
   clockwise. */
      vx = uy;
      vy = -ux;

/* Get the angle between the text base-line and horizontal. */
      angle = atan2( (double) vy, (double) vx )*AST__DR2D;

/* Get the bounding box of the string drawn with its bottom left corner
   at the origin. */
      ccpgqtxt( 0.0, 0.0, angle, 0.0, (char *) text, xbox, ybox );

/* Convert the returned bounding box world coordinates into millimetres. */
      for( i = 0; i < 4; i++ ){
         xbox[ i ] *= alpha;
         ybox[ i ] *= beta;
      }

/* Find the height of the bounding box, in millimetres. Note,
   the PGPLOT manual is not clear about the order of the corners
   returned by pgqtxt, so we have to find the largest distance between
   the corners in the direction of the supplied up-vector. The reference
   point is on the text base-line which is not usually at the bottom of
   the bounding box (some letters - like "y" - extend below the base-line).
   Find the distance from the base-line to the top (hu) and bottom (hd)
   of the bounding box. */
      hu = -FLT_MAX;
      hd = FLT_MAX;
      for( i = 0; i < 4; i++ ){
         test = ux*xbox[ i ] + uy*ybox[ i ];
         if( test > hu ) hu = test;
         if( test < hd ) hd = test;
      }

/* Get an up and a down vector scaled to the height/depth of the
   bounding box above/below the text base-line . */
      uxu = ux*hu;
      uyu = uy*hu;
      uxd = ux*hd;
      uyd = uy*hd;

/* The bounding box returned by pgqtxt does not include any leading or
   trailing spaces. We need to include such spaces in the returned box.
   To do this we get the length of the text string in millimetres
   using pglen instead of using the bounding box returned by pgqtxt. */
      ccpglen( 2, (char *) text, &xl, &yl );

/* The abolute width of the string in millimetres may depend on the
   up-vector. The values returned by pglen are for horizontal and
   vertical text. Find the width using the supplied up-vector. */
      a = uy*xl;
      b = ux*yl;
      width = sqrt( a*a + b*b );

/* The pglen function returns a value which is slightly smaller than
   the area cleared to hold the text when written using pgptxt. Increase
   the text width so that it is about equal to the area cleared. */
      width += 0.2*hu;

/* Scale the base-line vector so that its length is equal to the width
   of the bounding box (including spaces). */
      vx *= width;
      vy *= width;

/* Convert the base-line vector back into world coordinates. */
      vx /= alpha;
      vy /= beta;

/* Convert the up and down vectors into world coordinates. */
      uxu /= alpha;
      uyu /= beta;
      uxd /= alpha;
      uyd /= beta;

/* Find the coordinates at the centre of the bounding box in world
   coordinates. */
      xc = x;
      yc = y;

      if( lj[0] == 'B' ) {
         xc += 0.5*uxu;
         yc += 0.5*uyu;
      } else if( lj[0] == 'T' ) {
         xc -= 0.5*uxu;
         yc -= 0.5*uyu;
      }

      if( lj[1] == 'L' ) {
         xc += 0.5*vx;
         yc += 0.5*vy;
      } else if( lj[1] == 'R' ) {
         xc -= 0.5*vx;
         yc -= 0.5*vy;
      }

/* Get the corners of the bounding box. */
      vdx = 0.5*vx;
      vdy = 0.5*vy;
      udx = 0.5*uxu;
      udy = 0.5*uyu;

/* Bottom left corner... */
      xb[ 0 ] = xc - vdx - udx + uxd;
      yb[ 0 ] = yc - vdy - udy + uyd;

/* Bottom right corner... */
      xb[ 1 ] = xc + vdx - udx + uxd;
      yb[ 1 ] = yc + vdy - udy + uyd;

/* Top right corner... */
      xb[ 2 ] = xc + vdx + udx;
      yb[ 2 ] = yc + vdy + udy;

/* Top left corner... */
      xb[ 3 ] = xc - vdx + udx;
      yb[ 3 ] = yc - vdy + udy;

   }

/* Return. */
   return 1;
}

int astGQch( float *chv, float *chh ){
/*
*+
*  Name:
*     astGQch

*  Purpose:
*     Return the character height in world coordinates.

*  Language:
*     Starlink C

*  Description:
*     This function returns the heights of characters drawn vertically and
*     horizontally in world coordinates.

*  Returned Value:
*     A value of 0 is returned if an error occurs, and 1 is returned
*     otherwise.

*  Parameters:
*     chv
*        A pointer to the double which is to receive the height of
*        characters drawn vertically. This will be an increment in the X
*        axis.
*     chh
*        A pointer to the double which is to receive the height of
*        characters drawn vertically. This will be an increment in the Y
*        axis.

*  Synopsis:
*     #include "grf.h"
*     int astGQch( float *chv, float *chh )

*  Copyright:
*     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     {original_author_entry}

*  History:
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

/* Local Variables: */
   float vx1,vx2,vy1,vy2,wx1,wx2,wy1,wy2;

/* Get the character height in normalised device coordinates */
   ccpgqcs( 0, chv, chh );

/* Get the bounds of the PGPLOT viewport in normalised device
   coordinates. */
   ccpgqvp( 0, &vx1, &vx2, &vy1, &vy2 );

/* Get the bounds of the PGPLOT window in world coordinates. */
   ccpgqwin( &wx1, &wx2, &wy1, &wy2 );

/* Convert the text height from normalised device coordinates into world
   coordinates for vertical text. Print an error message if the viewport
   has zero size. */
   if( vx1 != vx2 ){
      *chv *= ( wx2 - wx1 )/( vx2 - vx1 );

   } else {
      astError( AST__GRFER, "astGQch: The graphics viewport has zero size "
                "in the X direction.");
      return 0;
   }

/* Convert the text height from normalised device coordinates into world
   coordinates for horizontal text. Print an error message if the viewport
   has zero size. */
   if( vy1 != vy2 ){
      *chh *= ( wy2 - wy1 )/( vy2 - vy1 );
   } else {
      astError( AST__GRFER, "astGQch: The graphics viewport has zero size "
                "in the Y direction.");
      return 0;
   }

/* Return. */
   return 1;
}

int astGAttr( int attr, double value, double *old_value, int prim ){
/*
*+
*  Name:
*     astGAttr

*  Purpose:
*     Enquire or set a graphics attribute value.

*  Language:
*     Starlink C

*  Description:
*     This function returns the current value of a specified graphics
*     attribute, and optionally establishes a new value. The supplied
*     value is converted to an integer value if necessary before use.

*  Returned Value:
*     A value of 0 is returned if an error occurs, and 1 is returned
*     otherwise.

*  Parameters:
*     attr
*        An integer value identifying the required attribute. The
*        following symbolic values are defined in grf.h:
*
*           GRF__STYLE  - Line style.
*           GRF__WIDTH  - Line width.
*           GRF__SIZE   - Character and marker size scale factor.
*           GRF__FONT   - Character font.
*           GRF__COLOUR - Colour index.
*     value
*        A new value to store for the attribute. If this is AST__BAD
*        no value is stored.
*     old_value
*        A pointer to a double in which to return the attribute value.
*        If this is NULL, no value is returned.
*     prim
*        The sort of graphics primitive to be drawn with the new attribute.
*        Identified by the following values defined in grf.h:
*           GRF__LINE
*           GRF__MARK
*           GRF__TEXT

*  Synopsis:
*     #include "grf.h"
*     int int astGAttr( int attr, double value, double *old_value, int prim )

*  Copyright:
*     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     {original_author_entry}

*  History:
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

   int ival;
   float rval, dx, dy, deflw, x1, x2, y1, y2;

/* If required retrieve the current line style, and set a new line style. */
   if( attr == GRF__STYLE ){
      ccpgqls( &ival );
      if( old_value ) *old_value = (double) ival;

      if( value != AST__BAD ){
         ival = (int) ( value + 0.5 );
         if( value < 0.0 ) ival -= 1;

         ival = ( ival - 1 ) % 5;
         ival += ( ival < 0 ) ? 6 : 1;

         ccpgsls( ival );
      }

/* If required retrieve the current line width, and set a new line width.
   Line width is stored in Plot as a scale factor (1.0 for the default line
   width which is a fixed fraction of the diagonal of the view surface), but
   pgplot stores it in units of 0.005 of an inch. */
   } else if( attr == GRF__WIDTH ){

/* Get the bounds of the view surface in inches. */
      ccpgqvsz( 1, &x1, &x2, &y1, &y2 );

/* Find the default line width in inches (i.e. 0.0005 of the length
   of the view surface diagonal). */
      dx = ( x1 - x2 );
      dy = ( y1 - y2 );
      deflw = 0.0005*sqrt( (double )( dx*dx + dy*dy ) );

/* Get the current pgplot line width in units of 0.005 of an inch. */
      ccpgqlw( &ival );

/* If required, return the factor by which this exceeds the default line
   width found above. */
      if( old_value ) *old_value = (double)( ival )/( 200.0 * deflw );

/* If a new line width has been provided, the pgplot line width needs to
   be set to the corresponding absolute value. */
      if( value != AST__BAD ){
         ival = (int) ( 200.0*value*deflw );
         if( ival < 1 ) {
            ival = 1;
         } else if( ival > 201 ){
            ival = 201;
         }
         ccpgslw( ival );
      }

/* If required retrieve the current character size, and set a new size.
   The attribute value should be a factor by which to multiply the
   default character size. */
   } else if( attr == GRF__SIZE ){
      ccpgqch( &rval );
      if( old_value ) *old_value = (double) rval;

      if( value != AST__BAD ){
         ccpgsch( (float) value );
      }

/* If required retrieve the current character font, and set a new font. */
   } else if( attr == GRF__FONT ){
      ccpgqcf( &ival );
      if( old_value ) *old_value = (double) ival;

      if( value != AST__BAD ){
         ival = (int) ( value + 0.5 );
         if( value < 0.0 ) ival -= 1;

         ival = ( ival - 1 ) % 4;
         ival += ( ival < 0 ) ? 5 : 1;
         ccpgscf( ival );
      }

/* If required retrieve the current colour index, and set a new colour
   index. */
   } else if( attr == GRF__COLOUR ){
      ccpgqci( &ival );
      if( old_value ) *old_value = (double) ival;

      if( value != AST__BAD ){
         ival = (int) ( value + 0.5 );
         if( ival < 0 ) ival = 1;
         ccpgsci( ival );
      }

/* Give an error message for any other attribute value. */
   } else {
      astError( AST__GRFER, "astGAttr: Unknown graphics attribute '%d' "
                "requested.", attr );
      return 0;
   }

/* Return. */
   return 1;
}

/* Local Functions. */
/* ================ */
/* These implement the local C interface to PGPLOT in terms of its
   native Fortran interface. Only those PGPLOT functions used within
   this module are included. */
static void ccpgline(int n, float xpts[], float ypts[] ){
   F77_INTEGER_TYPE N;
   F77_REAL_TYPE *XX;
   F77_REAL_TYPE *YY;
   int i;

   XX = (F77_REAL_TYPE *) astMalloc( sizeof( F77_REAL_TYPE )*(size_t) n );
   YY = (F77_REAL_TYPE *) astMalloc( sizeof( F77_REAL_TYPE )*(size_t) n );

   if( astOK ){

      for( i = 0; i < n; i++ ){
         XX[ i ] = (F77_REAL_TYPE) xpts[ i ];
         YY[ i ] = (F77_REAL_TYPE) ypts[ i ];
      }

      N = (F77_INTEGER_TYPE) n;

      F77_CALL(pgline)( INTEGER_ARG(&N), REAL_ARRAY_ARG(XX),
                        REAL_ARRAY_ARG(YY) );

      XX = (F77_REAL_TYPE *) astFree( (void *) XX );
      YY = (F77_REAL_TYPE *) astFree( (void *) YY );
   }
}

static void ccpgpt(int n, float xpts[], float ypts[], int symbol){
   F77_INTEGER_TYPE N;
   F77_REAL_TYPE *XX;
   F77_REAL_TYPE *YY;
   F77_INTEGER_TYPE SYMBOL;
   int i;

   XX = (F77_REAL_TYPE *) astMalloc( sizeof( F77_REAL_TYPE )*(size_t) n );
   YY = (F77_REAL_TYPE *) astMalloc( sizeof( F77_REAL_TYPE )*(size_t) n );

   if( astOK ){

      for( i = 0; i < n; i++ ){
         XX[ i ] = (F77_REAL_TYPE) xpts[ i ];
         YY[ i ] = (F77_REAL_TYPE) ypts[ i ];
      }

      N = (F77_INTEGER_TYPE) n;
      SYMBOL = (F77_INTEGER_TYPE) symbol;


      F77_CALL(pgpt)( INTEGER_ARG(&N), REAL_ARRAY_ARG(XX),
                      REAL_ARRAY_ARG(YY), INTEGER_ARG(&SYMBOL) );

      XX = (F77_REAL_TYPE *) astFree( (void *) XX );
      YY = (F77_REAL_TYPE *) astFree( (void *) YY );
   }
}

static void ccpgptxt(float x, float y, float angle, float fjust, char *text ){
   F77_REAL_TYPE X;
   F77_REAL_TYPE Y;
   F77_REAL_TYPE ANGLE;
   F77_REAL_TYPE FJUST;
   DECLARE_CHARACTER(LTEXT,MXSTRLEN);
   int ftext_length;

   X = (F77_REAL_TYPE) x;
   Y = (F77_REAL_TYPE) y;
   ANGLE = (F77_REAL_TYPE) angle;
   FJUST = (F77_REAL_TYPE) fjust;

   ftext_length = strlen( text );
   if( ftext_length > LTEXT_length ) ftext_length = LTEXT_length;
   astStringExport( text, LTEXT, ftext_length );

   F77_CALL(pgptxt)( REAL_ARG(&X), REAL_ARG(&Y), REAL_ARG(&ANGLE),
                     REAL_ARG(&FJUST), CHARACTER_ARG(LTEXT)
                     TRAIL_ARG(ftext) );
}

static void ccpgqtxt(float x, float y, float angle, float fjust, char *text,
                     float xbox[], float ybox[]){
   F77_REAL_TYPE X;
   F77_REAL_TYPE Y;
   F77_REAL_TYPE ANGLE;
   F77_REAL_TYPE FJUST;
   DECLARE_CHARACTER(LTEXT,MXSTRLEN);
   F77_REAL_TYPE XBOX[ 4 ];
   F77_REAL_TYPE YBOX[ 4 ];
   int i;
   int ftext_length;

   X = (F77_REAL_TYPE) x;
   Y = (F77_REAL_TYPE) y;
   ANGLE = (F77_REAL_TYPE) angle;
   FJUST = (F77_REAL_TYPE) fjust;

   ftext_length = strlen( text );
   if( ftext_length > LTEXT_length ) ftext_length = LTEXT_length;
   astStringExport( text, LTEXT, ftext_length );

   F77_CALL(pgqtxt)( REAL_ARG(&X), REAL_ARG(&Y), REAL_ARG(&ANGLE),
                     REAL_ARG(&FJUST), CHARACTER_ARG(LTEXT),
                     REAL_ARRAY_ARG(XBOX), REAL_ARRAY_ARG(YBOX)
                     TRAIL_ARG(ftext) );

   for( i = 0; i < 4; i++ ){
      xbox[ i ] = (float) XBOX[ i ];
      ybox[ i ] = (float) YBOX[ i ];
   }

}

static void ccpgqtbg(int *tbci){
   F77_INTEGER_TYPE TBCI;
   F77_CALL(pgqtbg)( INTEGER_ARG(&TBCI) );
   *tbci = (int) TBCI;
}

static void ccpgstbg(int tbci){
   F77_INTEGER_TYPE TBCI;
   TBCI = (F77_INTEGER_TYPE) tbci;
   F77_CALL(pgstbg)( INTEGER_ARG(&TBCI) );
}

static void ccpgqcs(int units, float *xch, float *ych){
   F77_INTEGER_TYPE UNITS;
   F77_REAL_TYPE XCH;
   F77_REAL_TYPE YCH;
   UNITS = (F77_INTEGER_TYPE) units;

   F77_CALL(pgqcs)( INTEGER_ARG(&UNITS), REAL_ARG(&XCH), REAL_ARG(&YCH) );

   *xch = (float) XCH;
   *ych = (float) YCH;
}

static void ccpglen(int units, char *text, float *xl, float *yl ){
   F77_INTEGER_TYPE UNITS;
   F77_REAL_TYPE XL;
   F77_REAL_TYPE YL;
   DECLARE_CHARACTER(LTEXT,MXSTRLEN);
   int ftext_length;

   UNITS = (F77_INTEGER_TYPE) units;


   ftext_length = strlen( text );
   if( ftext_length > LTEXT_length ) ftext_length = LTEXT_length;
   astStringExport( text, LTEXT, ftext_length );

   F77_CALL(pglen)( INTEGER_ARG(&UNITS), CHARACTER_ARG(LTEXT),
                    REAL_ARG(&XL), REAL_ARG(&YL) TRAIL_ARG(ftext) );

   *xl = (float) XL;
   *yl = (float) YL;
}

static void ccpgqvp(int units, float *x1, float *x2, float *y1, float *y2){
   F77_INTEGER_TYPE UNITS;
   F77_REAL_TYPE X1;
   F77_REAL_TYPE X2;
   F77_REAL_TYPE Y1;
   F77_REAL_TYPE Y2;

   UNITS = (F77_INTEGER_TYPE) units;
   F77_CALL(pgqvp)( INTEGER_ARG(&UNITS), REAL_ARG(&X1), REAL_ARG(&X2),
                    REAL_ARG(&Y1), REAL_ARG(&Y2) );
   *x1 = (float) X1;
   *x2 = (float) X2;
   *y1 = (float) Y1;
   *y2 = (float) Y2;
}

static void ccpgqvsz(int units, float *x1, float *x2, float *y1, float *y2){
   F77_INTEGER_TYPE UNITS;
   F77_REAL_TYPE X1;
   F77_REAL_TYPE X2;
   F77_REAL_TYPE Y1;
   F77_REAL_TYPE Y2;

   UNITS = (F77_INTEGER_TYPE) units;
   F77_CALL(pgqvsz)( INTEGER_ARG(&UNITS), REAL_ARG(&X1), REAL_ARG(&X2),
                    REAL_ARG(&Y1), REAL_ARG(&Y2) );
   *x1 = (float) X1;
   *x2 = (float) X2;
   *y1 = (float) Y1;
   *y2 = (float) Y2;
}

static void ccpgqwin(float *x1, float *x2, float *y1, float *y2){
   F77_REAL_TYPE X1;
   F77_REAL_TYPE X2;
   F77_REAL_TYPE Y1;
   F77_REAL_TYPE Y2;

   F77_CALL(pgqwin)( REAL_ARG(&X1), REAL_ARG(&X2), REAL_ARG(&Y1),
                     REAL_ARG(&Y2) );
   *x1 = (float) X1;
   *x2 = (float) X2;
   *y1 = (float) Y1;
   *y2 = (float) Y2;
}

static void ccpgqls(int *ls){
   F77_INTEGER_TYPE LS;
   F77_CALL(pgqls)( INTEGER_ARG(&LS) );
   *ls = (int) LS;
}

static void ccpgsls(int ls){
   F77_INTEGER_TYPE LS;
   LS = (F77_INTEGER_TYPE) ls;
   F77_CALL(pgsls)( INTEGER_ARG(&LS) );
}

static void ccpgqlw(int *lw){
   F77_INTEGER_TYPE LW;
   F77_CALL(pgqlw)( INTEGER_ARG(&LW) );
   *lw = (int) LW;
}

static void ccpgslw(int lw){
   F77_INTEGER_TYPE LW;
   LW = (F77_INTEGER_TYPE) lw;
   F77_CALL(pgslw)( INTEGER_ARG(&LW) );
}

static void ccpgqch(float *ch){
   F77_REAL_TYPE CH;
   F77_CALL(pgqch)( REAL_ARG(&CH) );
   *ch = (float) CH;
}

static void ccpgsch(float ch){
   F77_REAL_TYPE CH;
   CH = (F77_REAL_TYPE) ch;
   F77_CALL(pgsch)( REAL_ARG(&CH) );
}

static void ccpgqcf(int *cf){
   F77_INTEGER_TYPE CF;
   F77_CALL(pgqcf)( INTEGER_ARG(&CF) );
   *cf = (int) CF;
}

static void ccpgscf(int cf){
   F77_INTEGER_TYPE CF;
   CF = (F77_INTEGER_TYPE) cf;
   F77_CALL(pgscf)( INTEGER_ARG(&CF) );
}

static void ccpgqci(int *ci){
   F77_INTEGER_TYPE CI;
   F77_CALL(pgqci)( INTEGER_ARG(&CI) );
   *ci = (int) CI;
}

static void ccpgsci(int ci){
   F77_INTEGER_TYPE CI;
   CI = (F77_INTEGER_TYPE) ci;
   F77_CALL(pgsci)( INTEGER_ARG(&ci) );
}

static void ccpgupdt( void ){
   F77_CALL(pgupdt)();
}
