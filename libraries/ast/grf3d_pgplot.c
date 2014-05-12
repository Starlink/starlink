/*
*  Name:
*     grf3d_pgplot.c

*  Purpose:
*     Implement the grf3d interface using the PGPLOT graphics system.

*  Description:
*     This file implements the low level 3D graphics functions required
*     by the rest of AST, by calling suitable PGPLOT functions (the
*     FORTRAN PGPLOT interface is used).
*
*     This file can be used as a template for the development of
*     similar implementations based on other graphics systems.
*
*     Unlike world coordinates used by the 2D grf interface, the 3D world
*     coordinates used by the grf3D interface are assume to be equally scaled
*     (that is, they are assumed to have the same units). Therefore this
*     module has no equivalent to the astGScales function defined by the
*     2D grf interface.

*  Copyright:
*     Copyright (C) 2007 Science & Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software: you can redistribute it and/or
*     modify it under the terms of the GNU Lesser General Public
*     License as published by the Free Software Foundation, either
*     version 3 of the License, or (at your option) any later
*     version.
*     
*     This program is distributed in the hope that it will be useful,
*     but WITHOUT ANY WARRANTY; without even the implied warranty of
*     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
*     GNU Lesser General Public License for more details.
*     
*     You should have received a copy of the GNU Lesser General
*     License along with this program.  If not, see
*     <http://www.gnu.org/licenses/>.

*  Authors:
*     DSB: David S. Berry (Starlink)

*  History:
*     20-JUN-2007 (DSB):
*        Original version.
*/


/* Macros */
/* ====== */
#define MXDEV 16                 /* Max no of concurrent PGPLOT devices */
#define MXSTRLEN 80              /* Max PGPLOT string length */
#define CAMERA_OK 123456789      /* Flags that a Camera has been initialised */
#define TWOPI 6.28318530718      /* 2*PI */
#define MXSIDE 32                /* Max no of sides in a marker polygon */


/* Header files. */
/* ============= */
/* AST header files */
#include "grf3d.h"               /* The grf3D interface definition */
#include "pg3d.h"                /* Other public functions in this module */
#include "f77.h"                 /* FORTRAN <-> C interface macros (SUN/209) */
#include "c2f77.h"               /* C to FORTRAN interface functions */
#include "memory.h"              /* Memory allocation facilities */
#include "error.h"               /* Error reporting facilities */
#include "pointset.h"            /* Defines AST__BAD */
#include "ast_err.h"             /* AST error codes */

/* System header files */
#include <string.h>
#include <math.h>
#include <float.h>
#include <limits.h>
#include <stdio.h>


/* Type definitions. */
/* ================= */
/* Structure defining the position and orientation of the camera in 3D
   world coords. This is specific to the PGPLOT implementation. Other
   implementations need not include any equivalent to this structure. */
typedef struct camera {
   float eye_vector[3];
   float target_vector[3];
   float up_vector[3];
   float w2c_matrix[9];
   float screen_distance;
   int ok_flag;
} Camera;


/* Module variables. */
/* ================= */
/* One camera structure for each PGPLOT device identifier. PGPLOT allows
   a maximum of 8 concurrent devices at the moment. Make the array twice
   this size to allow for some future expansion in PGPLOT. Again, this is
   specific to the PGPLOT implementation of the grf3D interface. */
static Camera cameras[ MXDEV ];

/* Function templates. */
/* =================== */
/* Templates for functions that are private to this module. */
static Camera *getCamera( int );
static float getCharHeight( void );
static int Polygon( int, float *, float *, float *, float[3], float[3], float[3], float[3] );
static int Text( int *, int, float[3], const char *, float[3], float[3], float[3]  );
static int TextCam( Camera *, float[3], float[3], float[3], float[3] );
static int TxExt( int *, int, float[3], const char *, float[3], float[3], float[3], float *, float *, float *, float[3] );
static int getTextAxes( float[3], float[3], float[3], const char *, float[3], float[3], float[3], char[3] );
static int transform( Camera *, int, float *, float *, float *, float *, float * );
static int vectorNorm( float * );
static float vectorModulus( float * );
static void getSymbolList( const char *, int, int *, int * );
static void vectorProduct( float *, float *, float * );
static float dotProduct( float *, float * );
static void vectorSub( float *, float *, float * );

/* Templates for private functions that wrap PGPLOT Fortran routines. */
static void ccgrsyds( int *, int *, const char *, int, int );
static void ccgrsymk( int, int, int * );
static void ccgrsyxd( int, int *, int * );
static void ccpgline( int, float[], float[] );
static void ccpgpoly( int, float[], float[] );
static void ccpgqcf( int * );
static void ccpgqcf(int *);
static void ccpgqch( float * );
static void ccpgqci( int * );
static void ccpgqclp( int * );
static void ccpgqid( int * );
static void ccpgqls( int * );
static void ccpgqlw( int * );
static void ccpgqvsz( int, float *, float *, float *, float * );
static void ccpgqwin( float *, float *, float *, float * );
static void ccpgscf( int );
static void ccpgsch( float );
static void ccpgsci( int );
static void ccpgsclp( int );
static void ccpgsls( int );
static void ccpgslw( int );
static void ccpgswin( float, float, float, float );
static void ccpgupdt( void );


/* Templates for Fortran PGPLOT routines needed by this module. */
F77_SUBROUTINE(grsyds)( INTEGER_ARRAY(list), INTEGER(nlist), CHARACTER(text), INTEGER(font) TRAIL(text) );
F77_SUBROUTINE(grsymk)( INTEGER(type), INTEGER(font), INTEGER(symbol) );
F77_SUBROUTINE(grsyxd)( INTEGER(symbol), INTEGER_ARRAY(xygrid), INTEGER(unused) );
F77_SUBROUTINE(pgline)( INTEGER(N), REAL_ARRAY(X), REAL_ARRAY(Y) );
F77_SUBROUTINE(pgpoly)( INTEGER(N), REAL_ARRAY(X), REAL_ARRAY(Y) );
F77_SUBROUTINE(pgqcf)( INTEGER(ival) );
F77_SUBROUTINE(pgqch)( REAL(rval) );
F77_SUBROUTINE(pgqci)( INTEGER(ival) );
F77_SUBROUTINE(pgqclp)( INTEGER(clip) );
F77_SUBROUTINE(pgqid)( INTEGER(id) );
F77_SUBROUTINE(pgqls)( INTEGER(ival) );
F77_SUBROUTINE(pgqlw)( INTEGER(ival) );
F77_SUBROUTINE(pgqvsz)( INTEGER(units), REAL(x1), REAL(x2), REAL(y1), REAL(y2) );
F77_SUBROUTINE(pgqwin)( REAL(wx1), REAL(wx2), REAL(wy1), REAL(wy2) );
F77_SUBROUTINE(pgscf)( INTEGER(ival) );
F77_SUBROUTINE(pgsch)( REAL(rval) );
F77_SUBROUTINE(pgsci)( INTEGER(ival) );
F77_SUBROUTINE(pgsclp)( INTEGER(clip) );
F77_SUBROUTINE(pgsls)( INTEGER(ival) );
F77_SUBROUTINE(pgslw)( INTEGER(ival) );
F77_SUBROUTINE(pgswin)( REAL(X1), REAL(X2), REAL(Y1), REAL(Y2) );
F77_SUBROUTINE(pgupdt)( void );


/* Public functions defined by the grf3D interface. */
/* ================================================ */
/* All implementations of the grf3d interface must provide implementations
   of all the functions in this block. The corresponding templates are in
   grf3d.h */


int astG3DAttr( int attr, double value, double *old_value, int prim ){
/*
*+
*  Name:
*     astG3DAttr

*  Purpose:
*     Enquire or set a 3D graphics attribute value.

*  Synopsis:
*     #include "grf3d.h"
*     int int astG3DAttr( int attr, double value, double *old_value, int prim )

*  Description:
*     This function returns the current value of a specified 3D graphics
*     attribute, and optionally establishes a new value. The supplied
*     value is converted to an integer value if necessary before use.

*  Parameters:
*     attr
*        An integer value identifying the required attribute. The
*        following symbolic values are defined in grf3d.h:
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

*  Returned Value:
*     A value of 0 is returned if an error occurs, and 1 is returned
*     otherwise.

*  Notes:

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
      astError( AST__GRFER, "astG3DAttr: Unknown graphics attribute '%d' "
                "requested.", attr );
      return 0;
   }

/* Return. */
   return 1;
}

int astG3DCap( int cap, int value ){
/*
*+
*  Name:
*     astG3DCap

*  Purpose:
*     Indicate if this grf3d module has a given capability.

*  Synopsis:
*     #include "grf3d.h"
*     int astG3DCap( int cap, int value )

*  Description:
*     This function is called by the AST Plot class to determine if the
*     grf3d module has a given capability, as indicated by the "cap"
*     argument.

*  Parameters:
*     cap
*        The capability being inquired about. This will be one of the
*        following constants defined in grf3d.h:
*
*        GRF3D__ESC: This function should return a non-zero value if the
*        astG3DText and astG3DTxExt functions can recognise and interpret
*        graphics escape sequences within the supplied string. These
*        escape sequences are described below. Zero should be returned
*        if escape sequences cannot be interpreted (in which case the
*        Plot class will interpret them itself if needed). The supplied
*        "value" argument should be ignored only if escape sequences cannot
*        be interpreted by astG3DText and astG3DTxExt. Otherwise, "value"
*        indicates whether astG3DText and astG3DTxExt should interpret escape
*        sequences in subsequent calls. If "value" is non-zero then
*        escape sequences should be interpreted by astG3DText and
*        astG3DTxExt. Otherwise, they should be drawn as literal text.

*  Returned Value:
*     The return value, as described above. Zero should be returned if
*     the supplied capability is not recognised.

*  Escape Sequences:
*     Escape sequences are introduced into the text string by a percent
*     "%" character. The following escape sequences are currently recognised
*     ("..." represents a string of one or more decimal digits):
*
*       %%      - Print a literal "%" character (type GRF__ESPER ).
*
*       %^...+  - Draw subsequent characters as super-scripts. The digits
*                 "..." give the distance from the base-line of "normal"
*                 text to the base-line of the super-script text, scaled
*                 so that a value of "100" corresponds to the height of
*                 "normal" text (type GRF__ESSUP ).
*       %^+     - Draw subsequent characters with the normal base-line.
*
*       %v...+  - Draw subsequent characters as sub-scripts. The digits
*                 "..." give the distance from the base-line of "normal"
*                 text to the base-line of the sub-script text, scaled
*                 so that a value of "100" corresponds to the height of
*                 "normal" text (type GRF__ESSUB ).
*
*       %v+     - Draw subsequent characters with the normal base-line
*                 (equivalent to %^+).
*
*       %>...+  - Leave a gap before drawing subsequent characters.
*                 The digits "..." give the size of the gap, scaled
*                 so that a value of "100" corresponds to the height of
*                 "normal" text (type GRF__ESGAP ).
*
*       %<...+  - Move backwards before drawing subsequent characters.
*                 The digits "..." give the size of the movement, scaled
*                 so that a value of "100" corresponds to the height of
*                 "normal" text (type GRF_ESBAC).
*
*       %s...+  - Change the Size attribute for subsequent characters. The
*                 digits "..." give the new Size as a fraction of the
*                 "normal" Size, scaled so that a value of "100" corresponds
*                 to 1.0  (type GRF__ESSIZ ).
*
*       %s+     - Reset the Size attribute to its "normal" value.
*
*       %w...+  - Change the Width attribute for subsequent characters. The
*                 digits "..." give the new width as a fraction of the
*                 "normal" Width, scaled so that a value of "100" corresponds
*                 to 1.0  (type GRF__ESWID ).
*
*       %w+     - Reset the Size attribute to its "normal" value.
*
*       %f...+  - Change the Font attribute for subsequent characters. The
*                 digits "..." give the new Font value  (type GRF__ESFON ).
*
*       %f+     - Reset the Font attribute to its "normal" value.
*
*       %c...+  - Change the Colour attribute for subsequent characters. The
*                 digits "..." give the new Colour value  (type GRF__ESCOL ).
*
*       %c+     - Reset the Colour attribute to its "normal" value.
*
*       %t...+  - Change the Style attribute for subsequent characters. The
*                 digits "..." give the new Style value  (type GRF__ESSTY ).
*
*       %t+     - Reset the Style attribute to its "normal" value.
*
*       %-      - Push the current graphics attribute values onto the top of
*                 the stack - see "%+" (type GRF__ESPSH).
*
*       %+      - Pop attributes values of the top the stack - see "%-". If
*                 the stack is empty, "normal" attribute values are restored
*                 (type GRF__ESPOP).
*
*     The astFindEscape function (in libast.a) can be used to locate escape
*     sequences within a text string. It has the following signature:
*
*     #include "plot.h"
*     int astFindEscape( const char *text, int *type, int *value, int *nc )
*
*     Parameters:
*        text
*           Pointer to the string to be checked.
*        type
*           Pointer to a location at which to return the type of escape
*           sequence. Each type is identified by a symbolic constant defined
*           in grf.h and is indicated in the above section. The returned value
*           is undefined if the supplied text does not begin with an escape
*           sequence.
*        value
*           Pointer to a lcation at which to return the integer value
*           associated with the escape sequence. All usable values will be
*           positive. Zero is returned if the escape sequence has no associated
*           integer. A value of -1 indicates that the attribute identified by
*           "type" should be reset to its "normal" value (as established using
*           the astG3DAttr function, etc). The returned value is undefined if
*           the supplied text does not begin with an escape sequence.
*        nc
*           Pointer to a location at which to return the number of
*           characters read by this call. If the text starts with an escape
*           sequence, the returned value will be the number of characters in
*           the escape sequence. Otherwise, the returned value will be the
*           number of characters prior to the first escape sequence, or the
*           length of the supplied text if no escape sequence is found.

*     Returned Value:
*        A non-zero value is returned if the supplied text starts with a
*        graphics escape sequence, and zero is returned otherwise.

*-
*/

   return 0;
}

int astG3DFlush( void ){
/*
*+
*  Name:
*     astG3DFlush

*  Purpose:
*     Flush all pending graphics to the output device.

*  Synopsis:
*     #include "grf3d.h"
*     int astG3DFlush( void )

*  Description:
*     This function ensures that the display device is up-to-date,
*     by flushing any pending graphics to the output device.

*  Parameters:
*     None.

*  Returned Value:
*     A value of 0 is returned if an error occurs, and 1 is returned
*     otherwise.

*-
*/

   ccpgupdt();
   return 1;
}

int astG3DLine( int n, float *x, float *y, float *z ){
/*
*+
*  Name:
*     astG3DLine

*  Purpose:
*     Draw a polyline (i.e. a set of connected lines).

*  Synopsis:
*     #include "grf3d.h"
*     int astG3DLine( int n, float *x, float *y, float *z )

*  Description:
*     This function displays lines joining the given positions.

*  Parameters:
*     n
*        The number of positions to be joined together.
*     x
*        A pointer to an array holding the "n" x values.
*     y
*        A pointer to an array holding the "n" y values.
*     z
*        A pointer to an array holding the "n" z values.

*  Returned Value:
*     A value of 0 is returned if an error occurs, and 1 is returned
*     otherwise.

*  Notes:
*     - A camera must have been established prior to calling this
*     function using either astG3DSetCamera or astG3DAutoCamera.
*     -  Nothing is done if "n" is less than 2, or if a NULL pointer is
*     given for either "x", "y" or "z".

*-
*/

/* Local Variables: */
   int clip;
   int result = 0;
   float *h, *r;

/* Do nothing if we have less than 2 points, but do not indicate an error. */
   if( n < 2 ){
      result = 1;

/* Check the pointers. */
   } else if( x && y && z ) {

/* Save the current clipping flag, and ensure clipping is off. */
      ccpgqclp( &clip );
      ccpgsclp( 0 );

/* Allocate work space for the 2D world coordinate positions. */
      h = astMalloc( sizeof( float )*(size_t) n );
      r = astMalloc( sizeof( float )*(size_t) n );
      if( astOK ) {

/* Convert the supplied points from 3D world coordinates to 2D world
   (i.e. screen) coordinates. If succesful, plot the lines. */
         if( transform( NULL, n, x, y, z, h, r ) ) {
            ccpgline( n, (float *) h, (float *) r );
            result = 1;
         }
      }

/* Free work space. */
      h = astFree( h );
      r = astFree( r );

/* Re-instate original clipping flag. */
      ccpgsclp( clip );

   }
   return result;
}

int astG3DMark( int n, float *x, float *y, float *z, int type,
                float norm[3] ){
/*
*+
*  Name:
*     astG3DMark

*  Purpose:
*     Draw a set of markers.

*  Synopsis:
*     #include "grf.h"
*     int astG3DMark( int n, float *x, float *y, float *z, int type,
*                     float norm[3] )

*  Description:
*     This function draws markers centred at the given positions, on a
*     plane with a specified normal vector.

*  Parameters:
*     n
*        The number of markers to draw.
*     x
*        A pointer to an array holding the "n" x values.
*     y
*        A pointer to an array holding the "n" y values.
*     z
*        A pointer to an array holding the "n" z values.
*     type
*        An integer which can be used to indicate the type of marker symbol
*        required. See the description of routine PGPT in the PGPLOT manual.
*     norm
*        The (x,y,z) components of a vector that is normal to the plane
*        containing the marker. The given vector passes through the marker
*        from the back to the front. If all components of this vector are
*        zero, then a normal vector pointing from the position of the
*        first marker towards the camera eye is used.

*  Returned Value:
*     A value of 0 is returned if an error occurs, and 1 is returned
*     otherwise.

*  Notes:
*     -  Nothing is done if "n" is less than 1, or if a NULL pointer is
*     given for "x", "y" or "z".

*-
*/

/* local Variables: */
   char just[3];
   float ref[3];
   float up[3];
   float tx[3], ty[3], tz[3];
   float vx[ MXSIDE ], vy[ MXSIDE ], vz[ MXSIDE ];
   float ch, ang, dang;
   int clip;
   int font;
   int i;
   int nlist;
   int symnum;
   int ns;

/* Return if any of the coordinate pointers is NULL. */
   if( !x || !y || !z ) return 1;

/* Initialise */
   ns = 0;

/* Unless the "norm" vector is parallel to the z axis, we use an up vector
   that is parallel to the z axis. Otherwise we use an up vector paralle
   to the x axis. */
   if( norm[ 0 ] != 0.0 || norm[ 1 ] != 0.0 ) {
      up[ 0 ] = 0.0;
      up[ 1 ] = 0.0;
      up[ 2 ] = 1.0;
   } else {
      up[ 0 ] = 1.0;
      up[ 1 ] = 0.0;
      up[ 2 ] = 0.0;
   }

/* Create unit vectors along the three axes of the text plane
   coordinate system. */
   ref[ 0 ] = x[ 0 ];
   ref[ 1 ] = y[ 0 ];
   ref[ 2 ] = z[ 0 ];
   if( !getTextAxes( ref, up, norm, "CC", tx, ty, tz, just ) ) return 0;

/* Calculate the pgplot symbol number for the given marker type. */
   if( type > 0 ) {
      if( type > 127 ) {
         symnum = type;
      } else {
         ccpgqcf( &font );
         ccgrsymk( type, font, &symnum );
      }

   } else if( type > -3 ) {
      getSymbolList( ".", 1, &symnum, &nlist );

/* Regular polygons - create an array of text plane coordinates for the
   vertices of the polygon. */
   } else {
      symnum = type;

/* Get the character height in world coordinate units. A PGPLOT
   character height of 1.0 corresponds to 1/40 of the 2D window height. */
      ch = getCharHeight();

/* Limit the number of sides that can be produced. */
      ns = -type;
      if( ns > MXSIDE ) ns = MXSIDE;

/* Calculate the angle subtended by each edge of the polygon. */
      dang = TWOPI/ns;
      ang = 0.0;

/* Loop round each vertex. */
      for( i = 0; i < ns; i++ ) {
         vx[ i ] = ch*sin( ang );
         vy[ i ] = ch*cos( ang );
         vz[ i ] = 0.0;
         ang += dang;
      }
   }

/* Save the current clipping flag, and ensure clipping is off. */
   ccpgqclp( &clip );
   ccpgsclp( 0 );

/* Draw each marker in turn. */
   for( i = 0; i < n; i++ ) {

/* Store the centre world coords */
      ref[ 0 ] = x[ i ];
      ref[ 1 ] = y[ i ];
      ref[ 2 ] = z[ i ];

/* Draw the symbol, and return if anything goes wrong. */
      if( symnum >= 0 ) {
         if( !Text( &symnum, 1, ref, "CC", tx, ty, tz ) ) return 0;

      } else {
         if( !Polygon( ns, vx, vy, vz, ref, tx, ty, tz ) ) return 0;

      }

   }

/* Re-instate original clipping flag. */
   ccpgsclp( clip );

/* If we arrive here we have been succesful, so return a non-zero value. */
   return 1;
}

int astG3DQch( float *ch ){
/*
*+
*  Name:
*     astG3DQch

*  Purpose:
*     Return the character height in world coordinates.

*  Synopsis:
*     #include "grf3d.h"
*     int astG3DQch( float *ch )

*  Description:
*     This function returns the height of characters drawn using astG3DText.

*  Parameters:
*     ch
*        A pointer to the double which is to receive the height of
*        characters drawn with astG3DText.

*  Returned Value:
*     A value of 0 is returned if an error occurs, and 1 is returned
*     otherwise.

*  Notes:
*     - Since the 3D world coordinate axes are assumed to be equally
*     scaled, the height of text in world coordinate units is independent
*     of the orientation of the text. Therefore, this function returns
*     only one height value, unlike the equivalent 2D astGQch function
*     that returns two heights.
*-
*/
   *ch = getCharHeight();
   return 1;
}

int astG3DText( const char *text, float ref[3], const char *just, float up[3],
                float norm[3]  ){
/*
*+
*  Name:
*     astG3DText

*  Purpose:
*     Draw a character string.

*  Synopsis:
*     #include "grf3d.h"
*     int astG3DText( const char *text, float ref[3], const char *just,
*                     float up[3], float norm[3] )

*  Description:
*     This function displays a character string at a given position
*     on a given plane in 3D world coords, using a specified
*     justification and up-vector.

*  Parameters:
*     text
*        Pointer to a null-terminated character string to be displayed.
*     ref
*        The reference (x,y,z) coordinates.
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
*     up
*        The (x,y,z) up-vector for the text. The actual up vector used is
*        the projection of the supplied vector onto the plane specified by
*        "norm".
*     norm
*        The (x,y,z) components of a vector that is normal to the plane
*        containing the text. The given vector passes through the text
*        from the back to the front. If all components of this vector are
*        zero, then a normal vector pointing towards the camera eye is used.

*  Returned Value:
*     A value of 0 is returned if an error occurs, and 1 is returned
*     otherwise.

*  Notes:
*     -  This routine does not recognise PGPLOT escape sequences.
*     -  A NULL value for "just" causes a value of "CC" to be used.
*-
*/

/* Local Constants: */
#define MXLEN 256

/* Local Variables: */
   char newjust[3];
   float tx[3], ty[3], tz[3];
   int list[ MXLEN ];
   int nlist;

/* Convert the supplied string into a list of PGPLOT symbol numbers */
   getSymbolList( text, MXLEN, &nlist, list );

/* Create unit vectors along the three axes of the text plane
   coordinate system. */
   if( !getTextAxes( ref, up, norm, just, tx, ty, tz, newjust ) ) return 0;

/* Draw the text. */
   return Text( list, nlist, ref, newjust, tx, ty, tz );

/* Clear local constants. */
#undef MXLEN
}

int astG3DTxExt( const char *text, float ref[3], const char *just,
                 float up[3], float norm[3], float *xb, float *yb,
                 float *zb, float bl[3] ){
/*
*+
*  Name:
*     astG3DTxExt

*  Purpose:
*     Get the extent of a character string.

*  Synopsis:
*     #include "grf3d.h"
*     int astG3DTxExt( const char *text, float ref[3], const char *just,
*                      float up[3], float norm[3], float *xb, float *yb,
*                      float *zb, float bl[3] )

*  Description:
*     This function returns the corners of a box which would enclose the
*     supplied character string if it were displayed using astG3DText.
*
*     The returned box INCLUDES any leading or trailing spaces.

*  Parameters:
*     text
*        Pointer to a null-terminated character string to be displayed.
*     ref
*        The reference (x,y,z) coordinates.
*     just
*        A character string which specifies the location within the
*        text string which is to be placed at the reference position
*        given by x and y. The first character may be 'T' for "top",
*        'C' for "centre", 'B' for "baseline", or "M" for "bottom", and
*        specifies the vertical location of the reference position. Note,
*        "baseline" corresponds to the base-line of normal text. Some
*        characters (eg "y", "g", "p", etc) descend below the base-line,
*        and so "M" and "B" will produce different effects for such
*        characters. The second character may be 'L' for "left", 'C' for
*        "centre", or 'R' for "right", and specifies the horizontal
*        location of the reference position. If the string has less than
*        2 characters then 'C' is used for the missing characters.
*     up
*        The (x,y,z) up-vector for the text. The actual up vector used is
*        the projection of the supplied vector onto the plane specified by
*        "norm".
*     norm
*        The (x,y,z) components of a vector that is normal to the plane
*        containing the text. The given vector passes through the text
*        from the back to the front. If all components of this vector are
*        zero, then a normal vector pointing towards the camera eye is used.
*     xb
*        An array of 4 elements in which to return the x coordinate of
*        each corner of the bounding box.
*     yb
*        An array of 4 elements in which to return the y coordinate of
*        each corner of the bounding box.
*     zb
*        An array of 4 elements in which to return the z coordinate of
*        each corner of the bounding box.
*     bl
*        The 3D world coordinates at the left hand end of the text
*        baseline.

*  Returned Value:
*     A value of 0 is returned if an error occurs, and 1 is returned
*     otherwise.

*  Notes:
*     -  The order of the corners is anti-clockwise starting at the
*        bottom left when viewing the text normally (i.e. face on).
*     -  This routine does not recognise PGPLOT escape sequences.
*     -  A NULL value for "just" causes a value of "CC" to be used.
*-
*/

/* Local Constants: */
#define MXLEN 256

/* Local Variables: */
   char newjust[3];
   int i;
   int list[ MXLEN ];
   int nlist;
   float tx[3], ty[3], tz[3];

/* Initialise the returned values to indicate no box available. */
   for( i = 0; i < 4; i++ ){
      xb[ i ] = 0.0;
      yb[ i ] = 0.0;
      zb[ i ] = 0.0;
   }

/* Convert the supplied string into a list of symbol numbers */
   getSymbolList( text, MXLEN, &nlist, list );

/* Create unit vectors along the three axes of the text plane
   coordinate system. */
   if( !getTextAxes( ref, up, norm, just, tx, ty, tz, newjust ) ) return 0;

/* Find the bounding box of this list of symbols. */
   return TxExt( list, nlist, ref, newjust, tx, ty, tz, xb, yb, zb, bl );

/* Clear local constants. */
#undef MXLEN
}


/* Public functions specific to this PGPLOT implementation. */
/* ======================================================== */
/* Other implementations of the grf3d interface can ignore the following
   functions. They provide control of the 3D view. */

int PG3DSetCamera( float eye[3], float target[3], float up[3], float screen ){
/*
*+
*  Name:
*     PG3DSetCamera

*  Purpose:
*     Store new camera settings for the current PGPLOT device.

*  Synopsis:
*     #include "grf3d.h"
*     int PG3DSetCamera( float eye[3], float target[3], float up[3],
*                        float screen )

*  Description:
*     This function stores new camera settings for the current PGPLOT
*     device.
*
*     A "camera" describes the projection of the 3D world coordinate
*     space onto a 2D "screen". This screen corresponds to the 2D viewing
*     surface used by PGPLOT. The 2D window used by PGPLOT (as set by
*     PGSWIN, etc) defines the bounds of the screen area that is visible
*     in the PGPLOT viewport.
*
*     The 3D world coordinate axes (x,y,z) are such that if "z" is
*     vertically upwards and "x" points to the right, then "y" goes
*     out of the paper away from you. All 3 axes are assume to have equal
*     scale.
*
*     A camera defines a second set of 3D axes (called "(u,v,w)") with
*     origin at the 3D world coordinates given by "eye":
*
*     -  the "w" axis points towards the position given by "target"
*     -  the "v" axis is perpendicular to the "w" axis and is in the plane
*        spanned by the "w" axis and the supplied "up" vector
*     -  the "u" axis is perpendicular to both "w" and "v" and points to
*        the left when looking from the eye along the w axis with the v
*        axis upwards
*
*     Thus the "v" axis is parallel to "vertically up" on the 2D screen,
*     "u" is parallel to "horizontally to the left", and "w" is
*     perpendicular to the screen, pointing towards the target.
*
*     The screen is a plane perpendicular to the "w" axis, at the "w" axis
*     value given by "screen". A 2D cartesian coordinate system (h,r) is
*     defined on the screen, with origin at the point where the "w" axis
*     intersects the screen. The "h" (horizontal) axis is parallel to the
*     "u" axis but points in the opposite direction (to the left), and the
*     "r" (vertical) axis is parallel to the "v" axis. The (h,r) system is
*     taken to be the same as the PGPLOT 2D world coordinate system, and
*     PGSWIN can therefore be used to specify the rectangular area on the
*     screen that is mapped onto the PGPLOT viewport.
*
*     It is assumed that all axes (x,y,z), (u,v,w) and (h,r) are measured
*     in the same units.

*  Parameters:
*     eye
*        The position vector of the camera's "eye", in 3D world coordinates.
*     target
*        The position vector of a point in 3D world coordinates that is
*        at the centre of the camera's view. In other words, the camera is
*        looking towards this point. Zero will be returned if the target
*        is the same position as the eye.
*     up
*        A vector in 3D world coordinates that will appear vertically
*        upwards when projected onto the screen. Zero will be returned if
*        the up vector has zero length or is parallel to the line joining
*        the eye and the target.
*     screen
*        The distance from the camera's eye to the projection screen. If
*        this is zero, then an orthographic projection is used.

*  Returned Value:
*     A value of 0 is returned if an error occurs, and 1 is returned
*     otherwise.

*  Notes:
*     - Zero is returned if no PGPLOT device has been opened prior to
*     calling this function.
*-
*/

/* Local Variables: */
   Camera *cam;
   float *u, *v, *w;
   int result = 0;

/* Get a pointer to the Camera structure for the current PGPLOT device.
   Return without action if no PGPLOT device is open. */
   cam = getCamera( 0 );
   if( cam ) {
      result = 1;

/* Store the supplied values in the camera. */
      memcpy( cam->target_vector, target, 3*sizeof( float ) );
      memcpy( cam->eye_vector, eye, 3*sizeof( float ) );
      cam->screen_distance = screen;

/* Get pointers to the three rows of the w2c_matrix. This is a 3x3 matrix that
   rotates vectors in the (x,y,z) system into vectors in the (u,v,w)
   system. Each row in the matrix is a unit vector along the u, v or w
   axes. */
      u = cam->w2c_matrix;
      v = u + 3;
      w = v + 3;

/* The "w" axis points form the eye to the target, so get the vector from
   the eye to the target and normalise it. */
      vectorSub( target, eye, w );
      if( ! vectorNorm( w ) ) result = 0;

/* The "v" vector is in the plane spanned by the "w" axis and the "up"
   vector. Get the normal to this plane, storing the result temporarily
   in the "u" vector. . */
      vectorProduct( w, up, u );

/* The "v" vector is normal to the vector found above and is also normal
   to the "w" axis. Get this vector and normalise it.  */
      vectorProduct( u, w, v );
      if( ! vectorNorm( v ) ) result = 0;

/* The "u" vector is perpendicualr to both the "w" and "v" vectors. */
   vectorProduct( v, w, u );
      if( ! vectorNorm( u ) ) result = 0;

/* Use "v" as the stored up vector (the supplied "up" vector is not
   necesarily the same as "v"). */
      memcpy( cam->up_vector, v, 3*sizeof( float ) );

/* Se a flag that indicates that the Camera is usable. */
      cam->ok_flag = result ? CAMERA_OK : CAMERA_OK/2;
   }

   return result;
}

int PG3DSetEye( float eye[3] ){
/*
*+
*  Name:
*     PG3DSetEye

*  Purpose:
*     Store a new camera eye position for the current PGPLOT device.

*  Synopsis:
*     #include "grf3d.h"
*     int PG3DSetEye( float eye[3] )

*  Description:
*     This function modifies the camera eye position for the current
*     PGPLOT device. Other camera settings are left unchanged. See
*     PG3DSetCamera for more details.

*  Parameters:
*     eye
*        The position vector of the camera's "eye", in 3D world coordinates.
*        Zero is returned if the new eye position is the same as the
*        existing camera target position.

*  Returned Value:
*     A value of 0 is returned if an error occurs, and 1 is returned
*     otherwise.

*  Notes:
*     - Zero is returned if no PGPLOT device has been opened prior to
*     calling this function.
*     - This function can only be called to modify an existing Camera.
*     Consequently it returns zero if a camera has not already been set
*     for the current PGPLOT device by calling PG3DSetCamera.
*-
*/

/* Local Variables: */
   Camera *cam;
   int result = 0;

/* Get a pointer to the Camera structure for the current PGPLOT device.
   Return without action if no PGPLOT device is open. */
   cam = getCamera( 1 );
   if( cam ) {

/* If so, modify the camera values, using the supplied eye position but
   retaining the other camera settings. */
      result = PG3DSetCamera( eye, cam->target_vector, cam->up_vector,
                                cam->screen_distance );
   }

   return result;
}

int PG3DRotateEye( int dir, float angle ){
/*
*+
*  Name:
*     PG3DRotateEye

*  Purpose:
*     Move the eye on a great circle around the current target position.

*  Synopsis:
*     #include "grf3d.h"
*     int PG3DRotateEye( int dir, float angle )

*  Description:
*     This function modifies the camera eye position for the current
*     PGPLOT device. Other camera settings are left unchanged. See
*     PG3DSetCamera for more details.
*
*     The eye is moved by a gven distance along an arc of a great circle
*     centred on the current target position. The target position itself
*     is left unchanged.

*  Parameters:
*     dir
*        The direction in which to move the eye position:
*        1 - Move eye upwards
*        2 - Move eye downwards
*        3 - Move eye left
*        4 - Move eye right
*     angle
*        The arc-distance, in degrees, by which to move the eye.

*  Returned Value:
*     A value of 0 is returned if an error occurs, and 1 is returned
*     otherwise.

*  Notes:
*     - Zero is returned if no PGPLOT device has been opened prior to
*     calling this function.
*     - This function can only be called to modify an existing Camera.
*     Consequently it returns zero if a camera has not already been set
*     for the current PGPLOT device by calling PG3DSetCamera.
*-
*/

/* Local Variables: */
   Camera *cam;
   int result = 0;
   int i;
   float e[3], f[3], emod, neweye[3], sina, cosa;

/* Get a pointer to the Camera structure for the current PGPLOT device.
   Return without action if no PGPLOT device is open. */
   cam = getCamera( 1 );
   if( cam ) {

/* Get the cos and sine of the supplied angle. */
      cosa = cos( angle*TWOPI/360 );
      sina = sin( angle*TWOPI/360 );

/* Get the vector from the target to the eye, get its modulus. */
      vectorSub( cam->eye_vector, cam->target_vector, e );
      emod = vectorModulus( e );

/* If we are moving the eye upwards, find the new eye position. */
      if( dir == 1 ) {
         for( i = 0; i < 3; i++ ) {
            neweye[ i ] = e[ i ]*cosa + emod*cam->up_vector[ i ]*sina +
                          cam->target_vector[ i ];
         }

/* If we are moving the eye downwards, find the new eye position. */
      } else if( dir == 2 ) {
         for( i = 0; i < 3; i++ ) {
            neweye[ i ] = e[ i ]*cosa - emod*cam->up_vector[ i ]*sina +
                          cam->target_vector[ i ];
         }

/* If we are moving the eye left or right we need a vector in the plane
   of rotation that is at right angles to "e", and points to the right
   of the eye. */
      } else {
         vectorProduct( cam->up_vector, e, f );
         vectorNorm( f );

/* Get the new eye position. */
         if( dir == 3 ) {
            for( i = 0; i < 3; i++ ) {
               neweye[ i ] = e[ i ]*cosa - emod*f[ i ]*sina + cam->target_vector[ i ];
            }

         } else {
            for( i = 0; i < 3; i++ ) {
               neweye[ i ] = e[ i ]*cosa + emod*f[ i ]*sina + cam->target_vector[ i ];
            }
         }
      }

/* Modify the camera eye vector, retaining the other camera settings. */
      result = PG3DSetCamera( neweye, cam->target_vector, cam->up_vector,
                              cam->screen_distance );
   }

   return result;
}

int PG3DForward( float distance ){
/*
*+
*  Name:
*     PG3DForward

*  Purpose:
*     Move the eye forward towards the target.

*  Synopsis:
*     #include "grf3d.h"
*     int PG3DForward( float distance )

*  Description:
*     This function modifies the camera eye position for the current
*     PGPLOT device. Other camera settings are left unchanged. See
*     PG3DSetCamera for more details.
*
*     The eye is moved forward by a given distance towards the target
*     point, and the target point is also moved forward so that the
*     distance between eye and target remains unchanged.

*  Parameters:
*     distance
*        The distance to move the eye and target, given as a fraction of
*        the distance between the eye and the target.

*  Returned Value:
*     A value of 0 is returned if an error occurs, and 1 is returned
*     otherwise.

*  Notes:
*     - Zero is returned if no PGPLOT device has been opened prior to
*     calling this function.
*     - This function can only be called to modify an existing Camera.
*     Consequently it returns zero if a camera has not already been set
*     for the current PGPLOT device by calling PG3DSetCamera.
*-
*/

/* Local Variables: */
   Camera *cam;
   int result = 0;
   int i;
   float e[3], newtarg[3], neweye[3];

/* Get a pointer to the Camera structure for the current PGPLOT device.
   Return without action if no PGPLOT device is open. */
   cam = getCamera( 1 );
   if( cam ) {

/* Get the vector from the eye to the target. */
      vectorSub( cam->target_vector, cam->eye_vector, e );

/* Find the new eye and target positions. */
      for( i = 0; i < 3; i++ ){
         neweye[ i ] = cam->eye_vector[ i ] + e[ i ]*distance;
         newtarg[ i ] = cam->target_vector[ i ] + e[ i ]*distance;
      }

/* Modify the camera eye and target vectors, retaining the other camera
   settings. */
      result = PG3DSetCamera( neweye, newtarg, cam->up_vector,
                              cam->screen_distance );
   }

   return result;
}


int PG3DFindNearest( int n, float *x, float *y, float *z, int *iclose ){
/*
*+
*  Name:
*     PG3DForward

*  Purpose:
*     Find the closest point to the eye.

*  Synopsis:
*     #include "grf3d.h"
*     int PG3DFindNearest( int n, float *x, float *y, float *z, int *iclose )

*  Description:
*     This function checks every supplied point and returns the index of
*     the point that is closest to the eye.

*  Parameters:
*     n
*        The number of points to check.
*     x
*        Pointer to an array of "n" X values.
*     y
*        Pointer to an array of "n" Y values.
*     z
*        Pointer to an array of "n" Z values.
*     iclose
*        Pointer to an int in which to return the index of hte nearest
*        point.

*  Returned Value:
*     A value of 0 is returned if an error occurs, and 1 is returned
*     otherwise.

*  Notes:
*     - Zero is returned if no PGPLOT device has been opened prior to
*     calling this function.
*-
*/

/* Local Variables: */
   Camera *cam;
   int result = 0;
   int i;
   float c[3], v[3];
   float d;
   float dmin;

   *iclose = 0;

/* Get a pointer to the Camera structure for the current PGPLOT device.
   Return without action if no PGPLOT device is open. */
   cam = getCamera( 1 );
   if( cam ) {
      result = 1;

/*  Loop through all the supplied positions. */
      dmin = FLT_MAX;
      for( i = 0; i < n; i++ ) {

/* Get the required distance. */
         v[ 0 ] = x[ i ];
         v[ 1 ] = y[ i ];
         v[ 2 ] = z[ i ];
         vectorSub( v, cam->eye_vector, c );
         d = vectorModulus( c );

/* If this is the smallest distance so far, remember it. */
         if( d < dmin ) {
            dmin = d;
            *iclose = i;
         }
      }
   }

   return result;
}


int PG3DSetTarget( float target[3] ){
/*
*+
*  Name:
*     PG3DSetTarget

*  Purpose:
*     Store a new camera target position for the current PGPLOT device.

*  Synopsis:
*     #include "grf3d.h"
*     int PG3DSetTarget( float target[3] )

*  Description:
*     This function modifies the camera target position for the current
*     PGPLOT device. Other camera settings are left unchanged. See
*     PG3DSetCamera for more details.

*  Parameters:
*     target
*        The position vector of the camera's "target", in 3D world coordinates.
*        Zero is returned if the new target position is the same as the
*        existing camera eye position.

*  Returned Value:
*     A value of 0 is returned if an error occurs, and 1 is returned
*     otherwise.

*  Notes:
*     - Zero is returned if no PGPLOT device has been opened prior to
*     calling this function.
*     - This function can only be called to modify an existing Camera.
*     Consequently it returns zero if a camera has not already been set
*     for the current PGPLOT device by calling PG3DSetCamera.
*-
*/

/* Local Variables: */
   Camera *cam;
   int result = 0;

/* Get a pointer to the Camera structure for the current PGPLOT device.
   Return without action if no PGPLOT device is open. */
   cam = getCamera( 1 );
   if( cam ) {

/* If so, modify the camera values, using the supplied target position but
   retaining the other camera settings. */
      result = PG3DSetCamera( cam->eye_vector, target, cam->up_vector,
                                cam->screen_distance );
   }

   return result;
}


int PG3DSetUp( float up[3] ){
/*
*+
*  Name:
*     PG3DSetUp

*  Purpose:
*     Store a new camera up vector for the current PGPLOT device.

*  Synopsis:
*     #include "grf3d.h"
*     int PG3DSetUp( float up[3] )

*  Description:
*     This function modifies the camera up vector for the current
*     PGPLOT device. Other camera settings are left unchanged. See
*     PG3DSetCamera for more details.

*  Parameters:
*     up
*        The new up vector, in 3D world coordinates. Zero is returned if
*        the new up vector is parallel to the line joining the eye and
*        the target positions.

*  Returned Value:
*     A value of 0 is returned if an error occurs, and 1 is returned
*     otherwise.

*  Notes:
*     - Zero is returned if no PGPLOT device has been opened prior to
*     calling this function.
*     - This function can only be called to modify an existing Camera.
*     Consequently it returns zero if a camera has not already been set
*     for the current PGPLOT device by calling PG3DSetCamera.
*-
*/

/* Local Variables: */
   Camera *cam;
   int result = 0;

/* Get a pointer to the Camera structure for the current PGPLOT device.
   Return without action if no PGPLOT device is open. */
   cam = getCamera( 1 );
   if( cam ) {

/* If so, modify the camera values, using the supplied up vector but
   retaining the other camera settings. */
      result = PG3DSetCamera( cam->eye_vector, cam->target_vector, up,
                                cam->screen_distance );
   }

   return result;
}


int PG3DSetScreen( float screen ){
/*
*+
*  Name:
*     PG3DSetScreen

*  Purpose:
*     Store a new camera screen distance for the current PGPLOT device.

*  Synopsis:
*     #include "grf3d.h"
*     int PG3DSetScreen( float screen )

*  Description:
*     This function modifies the camera screen distance for the current
*     PGPLOT device. Other camera settings are left unchanged. See
*     PG3DSetCamera for more details.

*  Parameters:
*     screen
*        The distance from the camera's eye to the projection screen in
*        3D world coordinate units. If this is zero, then an orthographic
*        projection is used.

*  Returned Value:
*     A value of 0 is returned if an error occurs, and 1 is returned
*     otherwise.

*  Notes:
*     - Zero is returned if no PGPLOT device has been opened prior to
*     calling this function.
*     - This function can only be called to modify an existing Camera.
*     Consequently it returns zero if a camera has not already been set
*     for the current PGPLOT device by calling PG3DSetCamera.
*-
*/

/* Local Variables: */
   Camera *cam;
   int result = 0;

/* Get a pointer to the Camera structure for the current PGPLOT device.
   Return without action if no PGPLOT device is open. */
   cam = getCamera( 1 );
   if( cam ) {

/* If so, modify the camera values, using the supplied screen distance but
   retaining the other camera settings. */
      result = PG3DSetCamera( cam->eye_vector, cam->target_vector,
                                cam->up_vector, screen );
   }

   return result;
}

int PG3DAutoCamera( float lbnd[3], float ubnd[3] ){
/*
*+
*  Name:
*     PG3DAutoCamera

*  Purpose:
*     Set up a default camera to view a given box of 3D world coords.

*  Synopsis:
*     #include "grf3d.h"
*     int PG3DAutoCamera( float lbnd[3], float ubnd[3] )

*  Description:
*     This function sets up the camera and the 2D PGPLOT window for the
*     current device so that it produces a default view of a specified
*     volume of 3D world coordinate space.

*  Parameters:
*     lbnd
*        The lower bounds of the volume of 3D world coordinates that
*        is to be visible using the camera and 2D PGPLOT window.
*     ubnd
*        The upper bounds of the volume of 3D world coordinates that
*        is to be visible using the camera and 2D PGPLOT window.

*  Returned Value:
*     A value of 0 is returned if an error occurs, and 1 is returned
*     otherwise.

*  Notes:
*     - Zero is returned if no PGPLOT device has been opened prior to
*     calling this function.
*-
*/

/* Local Variables: */
   float target[3], eye[3], up[3], screen, dx, dy, dz, hlo, hhi, rlo, rhi;
   float x[8], y[8], z[8], h[8], r[8];
   Camera *cam;
   int result = 0;
   int i;

/* Get a pointer to the Camera structure for the current PGPLOT device.
   Return without action if no PGPLOT device is open. */
   cam = getCamera( 0 );
   if( cam ) {

/* The target position (i.e. the position towards which the camera is
   looking) is the middle of the volume. */
      target[ 0 ] = 0.5*( lbnd[ 0 ] + ubnd[ 0 ] );
      target[ 1 ] = 0.5*( lbnd[ 1 ] + ubnd[ 1 ] );
      target[ 2 ] = 0.5*( lbnd[ 2 ] + ubnd[ 2 ] );

/* The eye is slightly offset from a corner view. */
      eye[ 0 ] = 0.85*ubnd[ 0 ] + 0.15*lbnd[ 0 ];
      eye[ 1 ] = 0.75*ubnd[ 1 ] + 0.25*lbnd[ 1 ];
      eye[ 2 ] = 0.75*ubnd[ 2 ] + 0.25*lbnd[ 2 ];

/* The eye is seven times the size of the box away from the box centre. */
      eye[ 0 ] = 7*(eye[ 0 ] - target[ 0 ] ) + target[ 0 ];
      eye[ 1 ] = 7*(eye[ 1 ] - target[ 1 ] ) + target[ 1 ];
      eye[ 2 ] = 7*(eye[ 2 ] - target[ 2 ] ) + target[ 2 ];

/* The up vector is paralle to the Z axis. */
      up[ 0 ] = 0.0;
      up[ 1 ] = 0.0;
      up[ 2 ] = 1.0;

/* The screen is at the centre of the box. */
      dx = eye[ 0 ] - target[ 0 ];
      dy = eye[ 1 ] - target[ 1 ];
      dz = eye[ 2 ] - target[ 2 ];
      screen = sqrtf( dx*dx + dy*dy + dz*dz );

/* Set the camera. */
      if( PG3DSetCamera( eye, target, up, screen ) ) {

/* Get the 3D World coords at the corners of the volume. */
         x[ 0 ] = ubnd[ 0 ];
         x[ 1 ] = ubnd[ 0 ];
         x[ 2 ] = lbnd[ 0 ];
         x[ 3 ] = lbnd[ 0 ];
         x[ 4 ] = ubnd[ 0 ];
         x[ 5 ] = ubnd[ 0 ];
         x[ 6 ] = lbnd[ 0 ];
         x[ 7 ] = lbnd[ 0 ];

         y[ 0 ] = lbnd[ 1 ];
         y[ 1 ] = ubnd[ 1 ];
         y[ 2 ] = ubnd[ 1 ];
         y[ 3 ] = lbnd[ 1 ];
         y[ 4 ] = lbnd[ 1 ];
         y[ 5 ] = ubnd[ 1 ];
         y[ 6 ] = ubnd[ 1 ];
         y[ 7 ] = lbnd[ 1 ];

         z[ 0 ] = lbnd[ 2 ];
         z[ 1 ] = lbnd[ 2 ];
         z[ 2 ] = lbnd[ 2 ];
         z[ 3 ] = lbnd[ 2 ];
         z[ 4 ] = ubnd[ 2 ];
         z[ 5 ] = ubnd[ 2 ];
         z[ 6 ] = ubnd[ 2 ];
         z[ 7 ] = ubnd[ 2 ];

/* Transform these into screen coordinates. */
         if( transform( cam, 8, x, y, z, h, r ) ) {

/* Find the bounds in h and r of the projection of the volume. */
            hlo = FLT_MAX;
            hhi = -FLT_MAX;
            rlo = FLT_MAX;
            rhi = -FLT_MAX;

            for( i = 0; i < 8; i++ ) {
               if( h[ i ] < hlo ) hlo = h[ i ];
               if( h[ i ] > hhi ) hhi = h[ i ];
               if( r[ i ] < rlo ) rlo = r[ i ];
               if( r[ i ] > rhi ) rhi = r[ i ];
            }

/* Extend these bounds by 5% at each end */
            dx = 0.05*( hhi - hlo );
            hhi += dx;
            hlo -= dx;

            dy = 0.05*( rhi - rlo );
            rhi += dy;
            rlo -= dy;

/* If the box has non-zero area, set it as the 2D PGPLOT window, and
   indicate success. */
            if( rlo < rhi && hlo < hhi ) {
               ccpgswin( hlo, hhi, rlo, rhi );
               result = 1;
            }
         }
      }
   }
   return result;
}





/* Private functions for this module */
/* ================================= */

static int TextCam( Camera *textcam, float ref[3], float tx[3], float ty[3],
                    float tz[3] ){
/*
*  Name:
*     TextCam

*  Purpose:
*     Create a Camera that converts 3D text plane coordinates into 2D world
*     coordinates.

*  Synopsis:
*     #include "grf3d.h"
*     int TextCam( Camera *textcam, float ref[3], float tx[3], float ty[3],
*                  float tz[3] )

*  Description:
*     This function initialises the contents of a supplied Camera
*     structure so that the Camera describes the transformation from 3D
*     "text plane" coordinates to 2D PGPLOT world coordinates. The text
*     plane coordinate system is defined by three vectors along its x, y
*     and z axes, and an origin position.
*
*     Text plane coordinates describe a plane upon which 2D graphics such
*     as text is drawn. The X axis is parallel to the text base line, the
*     Y axis is the text up vector, and the Z axis is perpendicular to
*     the text, passing from the back of the text to the front of the text.

*  Parameters:
*     textcam
*        The Camera structure which is to be modified.
*     ref
*        The (x,y,z) coordinates at the text plane origin.
*     tx
*        A unit vector (expressed in 3D world coords) along the text plane
*        X axis. This is parallel to the text base line.
*     ty
*        A unit vector (expressed in 3D world coords) along the text plane
*        Y axis. This is parallel to the projectionof ht eup vector on to
*        the text plane.
*     tz
*        A unit vector (expressed in 3D world coords) along the text plane
*        Z axis. This is perpendicular to the text plane, passing from
*        the back of the text to the front of the text.

*  Returned Value:
*     A value of 0 is returned if an error occurs, and 1 is returned
*     otherwise.

*/


/* Local Variables: */
   Camera *cam;
   float dx, dy, dz;
   int i;
   float a, b, c;

/* Get the Camera for the current device identifier. Abort if no camera
   is available. This camera describes the transformation from 3D world
   coordinates (x,y,z) to 2D world coordinates (screen coordinates) (h,r). */
   cam = getCamera( 1 );
   if( !cam ) return 0;

/* Create a Camera structure that describes the transformation from
   text plane coordinates to 2D world coords, putting the origin of text
   plane coordinates at the given reference position. */
   dx = cam->eye_vector[ 0 ] - ref[ 0 ];
   dy = cam->eye_vector[ 1 ] - ref[ 1 ];
   dz = cam->eye_vector[ 2 ] - ref[ 2 ];

   textcam->eye_vector[ 0 ] = tx[ 0 ]*dx + tx[ 1 ]*dy + tx[ 2 ]*dz;
   textcam->eye_vector[ 1 ] = ty[ 0 ]*dx + ty[ 1 ]*dy + ty[ 2 ]*dz;
   textcam->eye_vector[ 2 ] = tz[ 0 ]*dx + tz[ 1 ]*dy + tz[ 2 ]*dz;

   for( i = 0; i < 8; i += 3 ) {
      a = cam->w2c_matrix[ i ];
      b = cam->w2c_matrix[ i + 1 ];
      c = cam->w2c_matrix[ i + 2 ];
      textcam->w2c_matrix[ i     ] = a*tx[ 0 ] + b*tx[ 1 ] + c*tx[ 2 ];
      textcam->w2c_matrix[ i + 1 ] = a*ty[ 0 ] + b*ty[ 1 ] + c*ty[ 2 ];
      textcam->w2c_matrix[ i + 2 ] = a*tz[ 0 ] + b*tz[ 1 ] + c*tz[ 2 ];
   }

   textcam->screen_distance = cam->screen_distance;
   textcam->ok_flag = CAMERA_OK;

   return 1;
}

static int Polygon( int nside, float *vx, float *vy, float *vz, float ref[3],
                    float tx[3], float ty[3], float tz[3]  ){
/*
*  Name:
*     Polygon

*  Purpose:
*     Draw a regular polygon.

*  Synopsis:
*     #include "grf3d.h"
*     int Polygon( int nside, float *vx, float *vy, float *vz, float ref[3],
*                  float tx[3], float ty[3], float tz[3] )

*  Description:
*     This function draws a polygon centred at a given position on a
*     given  plane in 3D world coords, using a specified up-vector. The
*     polygon vertices are specified in text plane coordinates via vx,
*     vy and vz.

*  Parameters:
*     nside
*        Number of sides for the polygon. Numbers higher than 32 are
*        treated as 32.
*     vx
*        Pointer to an array of "nside" text plane X axis values.
*     vy
*        Pointer to an array of "nside" text plane Y axis values.
*     vz
*        Pointer to an array of "nside" text plane Z axis values.
*     ref
*        The (x,y,z) coordinates at the polygon centre.
*     tx
*        A unit vector (expressed in 3D world coords) along the text plane
*        X axis. This is parallel to the text base line.
*     ty
*        A unit vector (expressed in 3D world coords) along the text plane
*        Y axis. This is parallel to the projectionof ht eup vector on to
*        the text plane.
*     tz
*        A unit vector (expressed in 3D world coords) along the text plane
*        Z axis. This is perpendicular to the text plane, passing from
*        the back of the text to the front of the text.

*  Returned Value:
*     A value of 0 is returned if an error occurs, and 1 is returned
*     otherwise.

*/


/* Local Variables: */
   Camera *cam;
   Camera newcam;
   float h[ MXSIDE ], r[ MXSIDE ];

/* Get the Camera for the current device identifier. Abort if no camera
   is available. */
   cam = getCamera( 1 );
   if( !cam ) return 0;

/* Check the number of points. */
   if( nside > MXSIDE) return 0;

/* Create a Camera structure that describes the transformation from
   text plane coordinates to 2D world coords, putting the origin of text
   plane coordinates at the given reference position. */
   if( !TextCam( &newcam, ref, tx, ty, tz ) ) return 0;

/* Transform the given text plane coordinates into 2D world coordinates. */
   if( !transform( &newcam, nside, vx, vy, vz, h, r ) ) return 0;

/* Draw the polygon. */
   ccpgpoly( nside, h, r );

/* If we get here we have succeeded so return a non-zero value. */
   return 1;
}

static int Text( int *list, int nlist, float ref[3], const char *just,
                 float tx[3], float ty[3], float tz[3] ){
/*
*  Name:
*     Text

*  Purpose:
*     Draw a character string.

*  Synopsis:
*     #include "grf3d.h"
*     int Text( int *list, int nlist, float ref[3], const char *just,
*               float tx[3], float ty[3], float tz[3] )

*  Description:
*     This function displays a symbol list at a given position on a given
*     plane in 3D world coords, using a specified justification and up-vector.

*  Parameters:
*     list
*        Pointer to an array of pgplot symbol values.
*     nlist
*        Length of the "list" array.
*     ref
*        The reference (x,y,z) coordinates.
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
*     tx
*        A unit vector (expressed in 3D world coords) along the text plane
*        X axis. This is parallel to the text base line.
*     ty
*        A unit vector (expressed in 3D world coords) along the text plane
*        Y axis. This is parallel to the projection of the up vector on to
*        the text plane.
*     tz
*        A unit vector (expressed in 3D world coords) along the text plane
*        Z axis. This is perpendicular to the text plane, passing from
*        the back of the text to the front of the text.

*  Returned Value:
*     A value of 0 is returned if an error occurs, and 1 is returned
*     otherwise.

*  Notes:
*     -  This routine does not recognise PGPLOT escape sequences.
*     -  A NULL value for "just" causes a value of "CC" to be used.
*/

/* Local Constants: */
#define MXLEN 256

/* Local Variables: */
   Camera *cam;
   Camera newcam;
   float ch;
   float tm, txc, tyc;
   float txleft, tybase;
   float xt[ 150 ], yt[ 150 ], zt[ 150 ], h[ 150 ], r[ 150 ];
   float xb[3], yb[3], zb[3], bl[3];
   int clip;
   int i;
   int j;
   int k;
   int unused;
   int xygrid[ 300 ];

/* If there is nothing to plot return without error. */
   if( nlist == 0 ) return 1;

/* Find the 3D world coordinates at the left hand end of the text
   baseline. */
   if( !TxExt( list, nlist, ref, just, tx, ty, tz, xb, yb, zb, bl ) ) return 0;

/* Get the Camera for the current device identifier. Abort if no camera
   is available. */
   if( ! (cam = getCamera( 1 ) ) ) return 0;

/* Create a Camera structure that describes the transformation from
   text plane coordinates to 2D world coords. */
   if( !TextCam( &newcam, ref, tx, ty, tz ) ) return 0;

/* Save the current clipping flag, and ensure clipping is off. */
   ccpgqclp( &clip );
   ccpgsclp( 0 );

/* Calculate the text plane X coord of the left hand edge of the first
   character. */
   txleft = tx[ 0 ]*( bl[ 0 ] - ref[ 0 ] ) +
            tx[ 1 ]*( bl[ 1 ] - ref[ 1 ] ) +
            tx[ 2 ]*( bl[ 2 ] - ref[ 2 ] );

/* Calculate the text plane Y coord at the text baseline. */
   tybase = ty[ 0 ]*( bl[ 0 ] - ref[ 0 ] ) +
            ty[ 1 ]*( bl[ 1 ] - ref[ 1 ] ) +
            ty[ 2 ]*( bl[ 2 ] - ref[ 2 ] );

/* Get the character height in world coordinate units. A PGPLOT
   character height of 1.0 corresponds to 1/40 of the 2D window height. */
   ch = getCharHeight();

/* Get the polylines that correspond to the first symbol. */
   ccgrsyxd( list[ 0 ], xygrid, &unused );

/* Create a linear transformation that maps the font grid coordinate
   system used by grsyxd onto text plane coordinates. This transformation
   will be different for each character in the string. The initial
   transformation set up now is appropriate for the first character. The
   mapping is:

      Text_x = txc + tm*Font_x
      Text_y = tyc + tm*Font_y

*/
   tm = ch/( xygrid[ 2 ] - xygrid[ 1 ] );
   tyc = tybase - tm*xygrid[ 1 ];
   txc = txleft - tm*xygrid[ 3 ];

/* Loop round each symbol. */
   for( i = 0; i < nlist; i++ ) {

/* Loop round each polyline that forms a segment of the character */
      k = 5;
      while( 1 ) {

/* Map the polyline vertices into text plane coordinates. */
         j = 0;
         while( j < 150  ){
             if( xygrid[ k ] != -64 ) {
                xt[ j ] = txc + tm*xygrid[ k++ ];
                yt[ j ] = tyc + tm*xygrid[ k++ ];
                zt[ j++ ] = 0.0;
             } else {
                break;
             }
         }

/* Map the text plane coordinates into 2D world coordinates. */
         if( j > 0 ) {
            (void) transform( &newcam, j, xt, yt, zt, h, r );

/* Draw the polyline. */
            ccpgline( j, h, r );
         }

/* If this is the last segment in the character, pass on to the next
   character. */
         if( xygrid[ k + 1 ] == -64 ) break;

/* Otherwise, skip over the end markers in the xygrid array, and go on to
   plot the next polyline segment. */
         k += 2;
      }

/* If this is not the last symbol... */
      if( i != nlist - 1 ) {

/* Set the text x value at which to place the left edge of the next
   character. This is the right hand edge of the character just drawn. */
         txleft += tm*( xygrid[ 4 ] - xygrid[ 3 ] );

/* Get the polylines that correspond to the next symbol. */
         ccgrsyxd( list[ i + 1 ], xygrid, &unused );

/* Modify the transformation from font grid coords to text plane coords
   so that it is appropriate for the next character in the string. */
         txc = txleft - tm*xygrid[ 3 ];
      }

/* Next symbol. */
   }

/* Re-instate original clipping flag. */
   ccpgsclp( clip );

/* If we arrive here, we have been successful, so return a non-zero
   value. */
   return 1;

/* Clear local constants. */
#undef MXLEN
}

static int TxExt( int *list, int nlist, float ref[3], const char *just,
                  float tx[3], float ty[3], float tz[3], float *xb, float *yb,
                  float *zb, float bl[3] ){
/*
*  Name:
*     TxExt

*  Purpose:
*     Get the extent of a character string.

*  Synopsis:
*     #include "grf3d.h"
*     int TxExt( int *list, int nlist, float ref[3], const char *just,
*                float tx[3], float ty[3], float tz[3], float *xb, float *yb,
*                float *zb, float bl[3] )

*  Description:
*     This function returns the corners of a box which would enclose the
*     supplied symbol list if it were displayed using Text.
*
*     The returned box includes any leading or trailing spaces.

*  Parameters:
*     list
*        Pointer to an array of pgplot symbol numbers.
*     nlist
*        The length of the "list" array.
*     ref
*        The reference (x,y,z) coordinates.
*     just
*        A character string which specifies the location within the
*        text string which is to be placed at the reference position
*        given by x and y. The first character may be 'T' for "top",
*        'C' for "centre", 'B' for "baseline", or "M" for "bottom", and
*        specifies the vertical location of the reference position. Note,
*        "baseline" corresponds to the base-line of normal text. Some
*        characters (eg "y", "g", "p", etc) descend below the base-line,
*        and so "M" and "B" will produce different effects for such
*        characters. The second character may be 'L' for "left", 'C' for
*        "centre", or 'R' for "right", and specifies the horizontal
*        location of the reference position. If the string has less than
*        2 characters then 'C' is used for the missing characters.
*     tx
*        A unit vector (expressed in 3D world coords) along the text plane
*        X axis. This is parallel to the text base line.
*     ty
*        A unit vector (expressed in 3D world coords) along the text plane
*        Y axis. This is parallel to the projectionof ht eup vector on to
*        the text plane.
*     tz
*        A unit vector (expressed in 3D world coords) along the text plane
*        Z axis. This is perpendicular to the text plane, passing from
*        the back of the text to the front of the text.
*     xb
*        An array of 4 elements in which to return the x coordinate of
*        each corner of the bounding box.
*     yb
*        An array of 4 elements in which to return the y coordinate of
*        each corner of the bounding box.
*     zb
*        An array of 4 elements in which to return the z coordinate of
*        each corner of the bounding box.
*     bl
*        The 3D world coordinates at the left hand end of the text
*        baseline.

*  Returned Value:
*     A value of 0 is returned if an error occurs, and 1 is returned
*     otherwise.

*  Notes:
*     -  The order of the corners is anti-clockwise starting at the
*        bottom left when viewing the text normally (i.e. face on).
*     -  This routine does not recognise PGPLOT escape sequences.
*     -  A NULL value for "just" causes a value of "CC" to be used.
*/

/* Local Constants: */
#define MXLEN 256

/* Local Variables: */
   float ch;
   float txlo, txhi, tylo, tyhi, tyzero;
   float tm;
   float w;
   int i;
   int unused;
   int xygrid[ 300 ];
   int gylo, gyhi, width;

/* Initialise the returned values to indicate no box available. */
   for( i = 0; i < 4; i++ ){
      xb[ i ] = 0.0;
      yb[ i ] = 0.0;
      zb[ i ] = 0.0;
   }

/* If there is nothing to plot return without error. */
   if( nlist == 0 ) return 1;

/* We now find the bounding box of the text in "text plane coordinates".
   These are (tx,ty,tz) axes that span the plane upon which the text is
   writtens. The origin of (tx,ty,tz) is at the supplied 3D reference
   position, the X axis increases along the text baseline, and Y axis
   increases along the text up vector, and Z increases from the back
   of the text to the front of the text  (all are measured in 3D world
   coord units). We first find the bounds of the text in these text plane
   coordinates, assuming that the bottom left of the text baseline is
   placed at the given reference position (i.e. at the origiin of text
   plane coordinates). */

/* Get the character height in world coordinate units. A PGPLOT
   character height of 1.0 corresponds to 1/40 of the 2D window height. */
   ch = getCharHeight();

/* Initialise the Y bounds of the text bounding box in grid coords. */
   gylo = INT_MAX;
   gyhi = -INT_MAX;

/* Initialise things. */
   width = 0;
   tm = 1.0;

/* Loop round each symbol. */
   for( i = 0; i < nlist; i++ ) {

/* Get the polylines that correspond to this symbol. */
      ccgrsyxd( list[ i ], xygrid, &unused );

/* If this is the first symbol, set the scaling factor that converts
   grid units to text plane units. */
      if( i == 0 ) tm = ch/( xygrid[ 2 ] - xygrid[ 1 ] );

/* Note the highest and lowest y grid value. */
      w = xygrid[ 2 ] - xygrid[ 1 ];
      if( w > gyhi ) gyhi = w;

      w = xygrid[ 0 ] - xygrid[ 1 ];
      if( w < gylo ) gylo = w;

/* Increment the total width of the string in grid units. */
      width += xygrid[ 4 ] - xygrid[ 3 ];
   }

/* Set up the bounding box in text plane coordinates. */
   txlo = 0.0;
   txhi = width*tm;
   tylo = gylo*tm;
   tyhi = gyhi*tm;
   tyzero = 0.0;

/* Adjust the text plane bounding box to take account of the specified
   text justification. The above process implicitly assumed a
   justifiation of "BL". */
   if( !just || just[ 0 ] == 'C' || just[ 0 ] == 0 ){
      w = 0.5*( tyhi + tylo );
      tylo -= w;
      tyhi -= w;
      tyzero -= w;

   } else if( just[ 0 ] == 'T' ){
      w = tyhi;
      tylo -= w;
      tyhi -= w;
      tyzero -= w;

   } else if( just[ 0 ] == 'M' ){
      w = -tylo;
      tylo += w;
      tyhi += w;
      tyzero += w;

   } else if( just[ 0 ] != 'B' ) {
      astError( AST__GRFER, "astG3DTxExt: Justification string '%s' "
                "is invalid.", just );
      return 0;
   }

   if( !just || just[ 1 ] == 'C' || just[ 1 ] == 0 ){
      w = 0.5*( txhi + txlo );
      txlo -= w;
      txhi -= w;

   } else if( just[ 1 ] == 'R' ){
      w = txhi;
      txlo -= w;
      txhi -= w;

   } else if( just[ 1 ] == 'L' ){
      w = txlo;
      txlo -= w;
      txhi -= w;

   } else {
      astError( AST__GRFER, "astG3DTxExt: Justification string '%s' "
                "is invalid.", just );
      return 0;
   }

/* Use the supplied text plane axis vectors to transform the corners of
   the text plane bounding box into 3D world coordinates. */
   xb[ 0 ] = tx[ 0 ]*txlo + ty[ 0 ]*tylo + ref[ 0 ];
   yb[ 0 ] = tx[ 1 ]*txlo + ty[ 1 ]*tylo + ref[ 1 ];
   zb[ 0 ] = tx[ 2 ]*txlo + ty[ 2 ]*tylo + ref[ 2 ];

   xb[ 1 ] = tx[ 0 ]*txhi + ty[ 0 ]*tylo + ref[ 0 ];
   yb[ 1 ] = tx[ 1 ]*txhi + ty[ 1 ]*tylo + ref[ 1 ];
   zb[ 1 ] = tx[ 2 ]*txhi + ty[ 2 ]*tylo + ref[ 2 ];

   xb[ 2 ] = tx[ 0 ]*txhi + ty[ 0 ]*tyhi + ref[ 0 ];
   yb[ 2 ] = tx[ 1 ]*txhi + ty[ 1 ]*tyhi + ref[ 1 ];
   zb[ 2 ] = tx[ 2 ]*txhi + ty[ 2 ]*tyhi + ref[ 2 ];

   xb[ 3 ] = tx[ 0 ]*txlo + ty[ 0 ]*tyhi + ref[ 0 ];
   yb[ 3 ] = tx[ 1 ]*txlo + ty[ 1 ]*tyhi + ref[ 1 ];
   zb[ 3 ] = tx[ 2 ]*txlo + ty[ 2 ]*tyhi + ref[ 2 ];

/* Also transform the text plane coordinates at the bottom left of the
   text baseline into 3D world coordinates. */
   bl[ 0 ] = tx[ 0 ]*txlo + ty[ 0 ]*tyzero + ref[ 0 ];
   bl[ 1 ] = tx[ 1 ]*txlo + ty[ 1 ]*tyzero + ref[ 1 ];
   bl[ 2 ] = tx[ 2 ]*txlo + ty[ 2 ]*tyzero + ref[ 2 ];

/* If we get here, we have been succesful, so return a non-zero value. */
   return 1;

/* Clear local constants. */
#undef MXLEN
}

static float getCharHeight( void ){
/*
*  Name:
*     getCharHeight

*  Purpose:
*     Get the current text height setting.

*  Synopsis:
*     #include "grf3d.h"
*     float getCharHeight( void )

*  Description:
*     This function returns the PGPLOT character height, scaled into
*     world coordinate units.

*  Returned Value:
*    The character height, in world coordinate units.

*/

/* Local Variables: */
   float wx1, wx2, wy1, wy2;
   float ch;

/* Get the bounds of the PGPLTO 2D window. */
   ccpgqwin( &wx1, &wx2, &wy1, &wy2 );

/* Get the normalised PGPLOT character height. */
   ccpgqch( &ch );

/* A PGPLOT character height of 1.0 corresponds to 1/40 of the 2D window
   height. Scale the normalised character height into world coordinate
   units, and return it. */
   return ch*fabs( wy1 - wy2 )/40.0;

}

static int getTextAxes( float ref[3], float up[3], float norm[3],
                        const char *just, float tx[3], float ty[3],
                        float tz[3], char newjust[3] ){
/*
*  Name:
*     getTextAxes

*  Purpose:
*     Get unit vectors along the text plane coordinate axes.

*  Synopsis:
*     #include "grf3d.h"
*     int getTextAxes( float ref[3], float up[3], float norm[3],
*                      const char *just, float tx[3], float ty[3],
*                      float tz[3], char newjust[3] )

*  Description:
*     This function returns three unit vectors that define the axes of a
*     3D Cartesian coordinate system known as "text plane coordinates".
*     These axes span the plane upon which text (or other graphics) is to
*     be written. The origin is at the supplied 3D reference position, the
*     X axis increases along the text baseline, and Y axis increases along
*     the text up vector, and Z increases from the back of the text to the
*     front of the text (all are measured in 3D world  coord units).
*
*     The returned vectors are reversed if this will result in text
*     appearing more "normal" (i.e. viewed from the front rather than
*     the back, and viewed upright rather thna upside down). If the
*     vectors are reversed, the justification string is also changed so
*     that the text occupies the requested area on the screen.

*  Parameters:
*     ref
*        The reference (x,y,z) coordinates.
*     up
*        The (x,y,z) up-vector for the text. The actual up vector used is
*        the projection of the supplied vector onto the plane specified by
*        "norm".
*     norm
*        The (x,y,z) components of a vector that is normal to the plane
*        containing the text. The given vector passes through the text
*        from the back to the front. If all components of this vector are
*        zero, then a normal vector pointing towards the camera eye is used.
*     just
*        The requested text justification, as supplied to astG3DText.
*     tx
*        A unit vector along the text plane X axis.
*     ty
*        A unit vector along the text plane X axis.
*     tz
*        A unit vector along the text plane X axis.
*     newjust
*        The text justification to use.

*  Returned Value:
*     A value of 0 is returned if an error occurs, and 1 is returned
*     otherwise.
*/

/* Local Variables: */
   Camera *cam;
   float eye[3];

/* Initialise the returned justification to equal the supplied
   justification, supplying defaults if required. . */
   if( just ) {
      strncpy( newjust, just, 2 );
      if( !newjust[ 0 ] ) newjust[ 0 ] = 'C';
      if( !newjust[ 1 ] ) newjust[ 1 ] = 'C';
      newjust[ 2 ] = 0;
   } else {
      strcpy( newjust, "CC" );
   }

/* Get the Camera for the current device identifier. Abort if no camera
   is available. */
   if( !( cam = getCamera( 1 ) ) ) return 0;

/* Calculate the vector from the reference position to the eye, and store
   it in "eye". */
   vectorSub( cam->eye_vector, ref, eye );

/* Create unit vectors along the three axes of the text plane coordinate
   system. These unit vectors are represented in terms of the 3D world
   coordinate axes. The text Z axis is parallel to the supplied "norm"
   vector. */
   tz[ 0 ] = norm[ 0 ];
   tz[ 1 ] = norm[ 1 ];
   tz[ 2 ] = norm[ 2 ];

/* Attempt to normalise the "tz" vector. If it has zero length, use the
   offset from the reference point to the eye. */
   if( ! vectorNorm( tz ) ){

/* Use the "eye" vector calculated above as the text plane Z axis. */
      tz[ 0 ] = eye[ 0 ];
      tz[ 1 ] = eye[ 1 ];
      tz[ 2 ] = eye[ 2 ];
   }

/* Find vectors along the text plane x and y axes. */
   vectorProduct( up, tz, tx );
   vectorProduct( tz, tx, ty );

/* Normalise the three text plane axis vectors. If any vector has zero
   length, abort. */
   if( !vectorNorm( tx ) || !vectorNorm( ty ) || !vectorNorm( tz ) ) return 0;

/* We now reverse text plane vectors if this will help ther text to be
   viewed "normally" on the screen. If the existing vectors cause the
   text to be viewed from the back rather than the front, reverse the tx
   and tz vectors so that he text is viewed from the front. */
   if( dotProduct( tz, eye ) < 0.0 ) {
      tz[ 0 ] = -tz[ 0 ];
      tz[ 1 ] = -tz[ 1 ];
      tz[ 2 ] = -tz[ 2 ];
      tx[ 0 ] = -tx[ 0 ];
      tx[ 1 ] = -tx[ 1 ];
      tx[ 2 ] = -tx[ 2 ];

/* The text will have spun around the up vector (i.e. the ty axis), so
   modify the horizontal justification  so that thex text occupies the same
   area on the screen. */
      if( newjust[ 1 ] == 'L' ) {
         newjust[ 1 ] = 'R';
      } else if( newjust[ 1 ] == 'R' ) {
         newjust[ 1 ] = 'L';
      }
   }

/* If the existing vectors cause the text to be viewed upside down, reverse
   the tx and ty vectors so that he text is viewed right way up. */
   if( dotProduct( ty, cam->up_vector ) < 0.0 ) {
      ty[ 0 ] = -ty[ 0 ];
      ty[ 1 ] = -ty[ 1 ];
      ty[ 2 ] = -ty[ 2 ];
      tx[ 0 ] = -tx[ 0 ];
      tx[ 1 ] = -tx[ 1 ];
      tx[ 2 ] = -tx[ 2 ];

/* The text will have spun around the tz vector (i.e. the viewing vector),
   so modify both vertical and horizontal justification so that the text
   occupies the same area on the screen. */
      if( newjust[ 0 ] == 'B' || newjust[ 0 ] == 'M' ) {
         newjust[ 0 ] = 'T';
      } else if( newjust[ 0 ] == 'T' ) {
         newjust[ 0 ] = 'M';
      }

      if( newjust[ 1 ] == 'L' ) {
         newjust[ 1 ] = 'R';
      } else if( newjust[ 1 ] == 'R' ) {
         newjust[ 1 ] = 'L';
      }
   }

/* If we arraive here we have been succesful, so return a non-zero value. */
   return 1;
}

static void getSymbolList( const char *text, int mxlen, int *nlist, int *list ){
/*
*  Name:
*     getSymbolList

*  Purpose:
*     Get the extent of a character string.

*  Synopsis:
*     #include "grf3d.h"
*     void getSymbolList( const char *text, int mxlen, int *nlist, int *list )

*  Description:
*     This function converts a supplied text string into a list of PGPLOT
*     symbol numbers for the current PGPLOT font.

*  Parameters:
*     text
*        Pointer to a null-terminated character string.
*     mxlen
*        The length of the "list" array.
*     nlist
*        Pointer to an integer in which to place the number of symbol
*        values stored in the "list" array. This will be returned equal
*        to zero if there are no non-blank characters in the supplied
*        string. If there is one or more non-blank characters in "text",
*        then the returned list will include any trailing spaces.
*     list
*        Pointer to an array in which to return the symbol numbers. The
*        array should be at least "mxlen" elements long.
*/


/* Local Variables: */
   int font;
   int tlen;

/* Assume we have no symbols. */
   *nlist = 0;

/* Check there is something to plot. */
   if( astChrLen( text ) > 0 ) {

/* Find the length of text that can be displayed. */
      tlen = strlen( text );
      if( tlen > mxlen ) tlen = mxlen;

/* Get the current PGPLOT font. */
      ccpgqcf( &font );

/* Convert the supplied string into a list of symbol numbers */
      ccgrsyds( list, nlist, text, tlen, font );
   }
}

static Camera *getCamera( int check ){
/*
*+
*  Name:
*     getCamera

*  Purpose:
*     Return a pointer to the Camera structure for the current PGPLOT
*     device.

*  Synopsis:
*     #include "grf3d.h"
*     Camera getCamera( int check )

*  Description:
*     This function returns a pointer to a static structure that defines the
*     position and orientation of the camera in 3D world coords. It can
*     be used to transform positions from 3D world coordinates (x,y,z) to
*     2D screen coordinates (h,r).

*  Parameters:
*     check
*        If non-zero, a check will be made that the Camera has been
*        initialised, and NULL will be returned if the Camera has not
*        been initialsied. If "check" is zero, a pointer to the Camera
*        is returned even if it has not been initialised.

*  Returned Value:
*     Pointer to the Camera, or NULL if an error occurs.
*-
*/

/* Local Variables: */
   int id;
   Camera *cam = NULL;

/* Get the pgplot current device identifier. Return NULL if no device is
   currently open. */
   ccpgqid( &id );
   if( id > 0 && id <= MXDEV ) {

/* Get a pointer to the required Camera structure. */
      cam = cameras + id - 1;

/* If required, check that the structure has been initialised. */
      if( check && cam->ok_flag != CAMERA_OK ) cam = NULL;
   }

   return cam;
}

static int transform( Camera *cam, int n, float *x, float *y, float *z,
                      float *h, float *r ){
/*
*+
*  Name:
*     transform

*  Purpose:
*     Transform positions from 3D world coords to 2D screen cooords.

*  Synopsis:
*     #include "grf3d.h"
*     int transform( Camera *cam, int n, float *x, float *y, float *z,
*                    float *h, float *r )

*  Description:
*     This function transforms a set of positions from 3D world
*     coordinates (x,y,z) to 2D screen coordinates (h,r), using the
*     supplied camera.

*  Parameters:
*     cam
*        Pointer to a structure descibing the projection from 3D world
*        coords to 2D screen coords. If NULL, the camera for the current
*        PGPLOT device is used.
*     n
*        The number of positions to transform.
*     x
*        An array of "n" values for the "x" axis of the 3D world
*        coordinate system.
*     y
*        An array of "n" values for the "y" axis of the 3D world
*        coordinate system.
*     z
*        An array of "n" values for the "z" axis of the 3D world
*        coordinate system.
*     h
*        An array to receive the "n" values for the "h" axis of the 2D
*        screen coordinate system.
*     r
*        An array to receive the "n" values for the "r" axis of the 2D
*        screen coordinate system.

*  Returned Value:
*     Zero if an error occurs. One otherwise.

*-
*/

/* Local Variables: */
   float dx, dy, dz, u, v, w, f;
   int i;
   int result = 0;

/* If no camera was supplied use the camera for the current PGPLOT
   device. */
   if( ! cam ) cam = getCamera( 0 );

/* Check we now have a usable camera */
   if( cam && cam->ok_flag == CAMERA_OK ) {
      result = 1;

/* Loop round each position. */
      for( i = 0; i < n; i++ ) {

/* Offset from supplied position to the camera eye. */
         dx = x[ i ] - (cam->eye_vector)[ 0 ];
         dy = y[ i ] - (cam->eye_vector)[ 1 ];
         dz = z[ i ] - (cam->eye_vector)[ 2 ];

/* Get the representation of this vector in the (u,v,w) system. */
         u = (cam->w2c_matrix)[ 0 ]*dx +
             (cam->w2c_matrix)[ 1 ]*dy +
             (cam->w2c_matrix)[ 2 ]*dz;

         v = (cam->w2c_matrix)[ 3 ]*dx +
             (cam->w2c_matrix)[ 4 ]*dy +
             (cam->w2c_matrix)[ 5 ]*dz;

         w = (cam->w2c_matrix)[ 6 ]*dx +
             (cam->w2c_matrix)[ 7 ]*dy +
             (cam->w2c_matrix)[ 8 ]*dz;

/* Find the screen coords, using either a tangent plane or an
   orothograhic projection. */
         if( cam->screen_distance != 0.0 ) {
            if( w != 0.0 ) {
               f = cam->screen_distance/w;
               h[ i ] = -f*u;
               r[ i ] = f*v;
            } else {
               h[ i ] = FLT_MAX;
               r[ i ] = FLT_MAX;
            }
         } else {
            h[ i ] = -u;
            r[ i ] = v;
         }

      }
   }
   return result;
}


/* Dot product of a pair of 3-vectors "a" and "b". */
static float dotProduct( float *a, float *b ){
   return a[ 0 ]*b[ 0 ] + a[ 1 ]*b[ 1 ] + a[ 2 ]*b[ 2 ];
}

/* Vector product of a pair of 3-vectors "a" and "b". */
static void vectorProduct( float *a, float *b, float *c ){
   c[ 0 ] = a[ 1 ]*b[ 2 ] - a[ 2 ]*b[ 1 ];
   c[ 1 ] = a[ 2 ]*b[ 0 ] - a[ 0 ]*b[ 2 ];
   c[ 2 ] = a[ 0 ]*b[ 1 ] - a[ 1 ]*b[ 0 ];
}

/* Vector from "b" to "a" (i.e. a minus b) . */
static void vectorSub( float *a, float *b, float *c ){
   c[ 0 ] = a[ 0 ] - b[ 0 ];
   c[ 1 ] = a[ 1 ] - b[ 1 ];
   c[ 2 ] = a[ 2 ] - b[ 2 ];
}

/* Normalises a vector to a unit length. Returns zero if the vector has
   zero length, and 1 otherwise. */
static int vectorNorm( float *a ){
   float d;
   d = vectorModulus( a );
   if( d > 0.0 ) {
      a[ 0 ] /= d;
      a[ 1 ] /= d;
      a[ 2 ] /= d;
      return 1;
   } else {
      return 0;
   }
}

/* Return the length of a vector. */
static float vectorModulus( float *a ){
   return sqrtf( a[ 0 ]*a[ 0 ] + a[ 1 ]*a[ 1 ] + a[ 2 ]*a[ 2 ] );
}







/* PGPLOT interface functions */
/* ========================== */
static void ccpgqclp(int *clip){
   F77_INTEGER_TYPE CLIP;
   F77_CALL(pgqclp)( INTEGER_ARG(&CLIP) );
   *clip = (int) CLIP;
}

static void ccpgsclp(int clip){
   F77_INTEGER_TYPE CLIP;
   CLIP = (F77_INTEGER_TYPE) clip;
   F77_CALL(pgsclp)( INTEGER_ARG(&CLIP) );
}

static void ccpgqid(int *id){
   F77_INTEGER_TYPE ID;
   F77_CALL(pgqid)( INTEGER_ARG(&ID) );
   *id = (int) ID;
}


static void ccpgswin(float x1, float x2, float y1, float y2){
   F77_REAL_TYPE X1;
   F77_REAL_TYPE X2;
   F77_REAL_TYPE Y1;
   F77_REAL_TYPE Y2;

   X1 = x1;
   X2 = x2;
   Y1 = y1;
   Y2 = y2;

   F77_CALL(pgswin)( REAL_ARG(&X1), REAL_ARG(&X2), REAL_ARG(&Y1),
                     REAL_ARG(&Y2) );
}

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

static void ccpgpoly(int n, float xpts[], float ypts[] ){
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

      F77_CALL(pgpoly)( INTEGER_ARG(&N), REAL_ARRAY_ARG(XX),
                        REAL_ARRAY_ARG(YY) );

      XX = (F77_REAL_TYPE *) astFree( (void *) XX );
      YY = (F77_REAL_TYPE *) astFree( (void *) YY );
   }
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

static void ccpgqch(float *ch){
   F77_REAL_TYPE CH;
   F77_CALL(pgqch)( REAL_ARG(&CH) );
   *ch = (float) CH;
}

static void ccpgqcf(int *cf){
   F77_INTEGER_TYPE CF;
   F77_CALL(pgqcf)( INTEGER_ARG(&CF) );
   *cf = (int) CF;
}

static void ccgrsyds( int *list, int *nlist, const char *text, int tlen,
                      int font ){
   F77_INTEGER_TYPE *LIST;
   F77_INTEGER_TYPE NLIST;
   DECLARE_CHARACTER(LTEXT,MXSTRLEN);
   F77_INTEGER_TYPE FONT;
   int ftext_length;
   int i;

   ftext_length = tlen;
   if( ftext_length > LTEXT_length ) ftext_length = LTEXT_length;
   astStringExport( text, LTEXT, ftext_length );

   LIST = (F77_INTEGER_TYPE *) astMalloc( sizeof( F77_INTEGER_TYPE )*(size_t) ftext_length );

   if( astOK ){

      FONT = (F77_INTEGER_TYPE) font;

      F77_CALL(grsyds)( INTEGER_ARRAY_ARG(LIST), INTEGER_ARG(&NLIST),
                        CHARACTER_ARG(LTEXT), INTEGER_ARG(&FONT)
                        TRAIL_ARG(ftext) );

      *nlist = (int) NLIST;
      for( i = 0; i < ftext_length; i++ ){
         list[ i ] = (int) LIST[ i ];
      }

      LIST = (F77_INTEGER_TYPE *) astFree( (void *) LIST );
   }
}

static void ccgrsymk( int type, int font, int *symbol ){
   F77_INTEGER_TYPE TYPE;
   F77_INTEGER_TYPE FONT;
   F77_INTEGER_TYPE SYMBOL;

   TYPE = (F77_INTEGER_TYPE) type;
   FONT = (F77_INTEGER_TYPE) font;
   F77_CALL(grsymk)( INTEGER_ARG(&TYPE), INTEGER_ARG(&FONT),
                     INTEGER_ARG(&SYMBOL) );
   *symbol = (int) SYMBOL;
}


static void ccgrsyxd( int symbol, int *xygrid, int *unused ){
   F77_INTEGER_TYPE SYMBOL;
   DECLARE_INTEGER_ARRAY(XYGRID,300);
   F77_LOGICAL_TYPE UNUSED;
   int i;

   SYMBOL = (F77_INTEGER_TYPE) symbol;
   F77_CALL(grsyxd)( INTEGER_ARG(&SYMBOL), INTEGER_ARRAY_ARG(XYGRID),
                     LOGICAL_ARG(&UNUSED) );

   *unused = ( UNUSED == F77_TRUE );
   for( i = 0; i < 5; i++ ) xygrid[ i ] = (int) XYGRID[ i ];
   for( ; i < 300; i++ ){
      xygrid[ i ] = (int) XYGRID[ i ];
      i++;
      if( ( xygrid[ i ] = (int) XYGRID[ i ] ) == -64 ) break;
   }
}

static void ccpgupdt( void ){
   F77_CALL(pgupdt)();
}

static void ccpgqci(int *ci){
   F77_INTEGER_TYPE CI;
   F77_CALL(pgqci)( INTEGER_ARG(&CI) );
   *ci = (int) CI;
}

static void ccpgqls(int *ls){
   F77_INTEGER_TYPE LS;
   F77_CALL(pgqls)( INTEGER_ARG(&LS) );
   *ls = (int) LS;
}

static void ccpgqlw(int *lw){
   F77_INTEGER_TYPE LW;
   F77_CALL(pgqlw)( INTEGER_ARG(&LW) );
   *lw = (int) LW;
}

static void ccpgscf(int cf){
   F77_INTEGER_TYPE CF;
   CF = (F77_INTEGER_TYPE) cf;
   F77_CALL(pgscf)( INTEGER_ARG(&CF) );
}

static void ccpgsch(float ch){
   F77_REAL_TYPE CH;
   CH = (F77_REAL_TYPE) ch;
   F77_CALL(pgsch)( REAL_ARG(&CH) );
}

static void ccpgsci(int ci){
   F77_INTEGER_TYPE CI;
   CI = (F77_INTEGER_TYPE) ci;
   F77_CALL(pgsci)( INTEGER_ARG(&ci) );
}

static void ccpgsls(int ls){
   F77_INTEGER_TYPE LS;
   LS = (F77_INTEGER_TYPE) ls;
   F77_CALL(pgsls)( INTEGER_ARG(&LS) );
}

static void ccpgslw(int lw){
   F77_INTEGER_TYPE LW;
   LW = (F77_INTEGER_TYPE) lw;
   F77_CALL(pgslw)( INTEGER_ARG(&LW) );
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



/* Fortran interfaces for public functions in this module. */
/* ======================================================= */


F77_LOGICAL_FUNCTION(pg3d_findnearest)( INTEGER(N),
                                        REAL_ARRAY(X),
                                        REAL_ARRAY(Y),
                                        REAL_ARRAY(Z),
                                        INTEGER(ICLOSE) ){
   GENPTR_INTEGER(N)
   GENPTR_REAL_ARRAY(X)
   GENPTR_REAL_ARRAY(Y)
   GENPTR_REAL_ARRAY(Z)
   GENPTR_INTEGER(ICLOSE)
   return PG3DFindNearest( *N, X, Y, Z, ICLOSE ) ? F77_TRUE : F77_FALSE;
}



F77_LOGICAL_FUNCTION(pg3d_setcamera)( REAL_ARRAY(EYE),
                                        REAL_ARRAY(TARGET),
                                        REAL_ARRAY(UP),
                                        REAL(SCREEN) ){
   GENPTR_REAL_ARRAY(EYE)
   GENPTR_REAL_ARRAY(TARGET)
   GENPTR_REAL_ARRAY(UP)
   GENPTR_REAL(SCREEN)
   return PG3DSetCamera( EYE, TARGET, UP, *SCREEN ) ? F77_TRUE : F77_FALSE;
}


F77_LOGICAL_FUNCTION(pg3d_autocamera)( REAL_ARRAY(LBND),
                                         REAL_ARRAY(UBND) ){
   GENPTR_REAL_ARRAY(LBND)
   GENPTR_REAL_ARRAY(UBND)
   return PG3DAutoCamera( LBND, UBND ) ? F77_TRUE : F77_FALSE;
}

F77_LOGICAL_FUNCTION(pg3d_seteye)( REAL_ARRAY(EYE) ){
   GENPTR_REAL_ARRAY(EYE)
   return PG3DSetEye( EYE ) ? F77_TRUE : F77_FALSE;
}

F77_LOGICAL_FUNCTION(pg3d_setup)( REAL_ARRAY(UP) ){
   GENPTR_REAL_ARRAY(UP)
   return PG3DSetUp( UP ) ? F77_TRUE : F77_FALSE;
}

F77_LOGICAL_FUNCTION(pg3d_rotateeye)( INTEGER(DIR), REAL(ANGLE) ){
   GENPTR_INTEGER(DIR)
   GENPTR_REAL(ANGLE)
   return PG3DRotateEye( *DIR, *ANGLE ) ? F77_TRUE : F77_FALSE;
}

F77_LOGICAL_FUNCTION(pg3d_forward)( REAL(DISTANCE) ){
   GENPTR_REAL(DISTANCE)
   return PG3DForward( *DISTANCE ) ? F77_TRUE : F77_FALSE;
}

F77_LOGICAL_FUNCTION(ast_g3dmark)( INTEGER(N),
                                   REAL_ARRAY(X),
                                   REAL_ARRAY(Y),
                                   REAL_ARRAY(Z),
                                   INTEGER(TYPE),
                                   REAL_ARRAY(NORM)){
   GENPTR_INTEGER(N)
   GENPTR_REAL_ARRAY(X)
   GENPTR_REAL_ARRAY(Y)
   GENPTR_REAL_ARRAY(Z)
   GENPTR_INTEGER(TYPE)
   GENPTR_REAL_ARRAY(NORM)
   return astG3DMark( *N, X, Y, Z, *TYPE, NORM ) ? F77_TRUE : F77_FALSE;

}

F77_LOGICAL_FUNCTION(ast_g3dline)( INTEGER(N),
                                   REAL_ARRAY(X),
                                   REAL_ARRAY(Y),
                                   REAL_ARRAY(Z) ){
   GENPTR_INTEGER(N)
   GENPTR_REAL_ARRAY(X)
   GENPTR_REAL_ARRAY(Y)
   GENPTR_REAL_ARRAY(Z)
   return astG3DLine( *N, X, Y, Z ) ? F77_TRUE : F77_FALSE;

}


F77_INTEGER_FUNCTION(ast_g3dtext)( CHARACTER(TEXT),
                                   REAL_ARRAY(REF),
                                   CHARACTER(JUST),
                                   REAL_ARRAY(UP),
                                   REAL_ARRAY(NORM)
                                   TRAIL(TEXT)
                                   TRAIL(JUST) ){
   GENPTR_CHARACTER(TEXT)
   GENPTR_REAL_ARRAY(REF)
   GENPTR_CHARACTER(JUST)
   GENPTR_REAL_ARRAY(UP)
   GENPTR_REAL_ARRAY(NORM)
   F77_INTEGER_TYPE(RESULT);
   char *text, *just, *p;

   text = astString( TEXT, TEXT_length );
   just = astString( JUST, JUST_length );

/* Ignore trailing spaces in the text */
   p = text + TEXT_length;
   while( !*p || *p == ' ' ) *(p--) = 0;

   if( astOK ) {
      RESULT = (F77_INTEGER_TYPE) astG3DText( text, REF, just, UP, NORM );
   } else {
      RESULT = 0;
   }

   (void) astFree( text );
   (void) astFree( just );

   return RESULT;
}

F77_INTEGER_FUNCTION(ast_g3dtxext)( CHARACTER(TEXT),
                                    REAL_ARRAY(REF),
                                    CHARACTER(JUST),
                                    REAL_ARRAY(UP),
                                    REAL_ARRAY(NORM),
                                    REAL_ARRAY(XB),
                                    REAL_ARRAY(YB),
                                    REAL_ARRAY(ZB),
                                    REAL_ARRAY(BL)
                                    TRAIL(TEXT)
                                    TRAIL(JUST) ){
   GENPTR_CHARACTER(TEXT)
   GENPTR_REAL_ARRAY(REF)
   GENPTR_CHARACTER(JUST)
   GENPTR_REAL_ARRAY(UP)
   GENPTR_REAL_ARRAY(NORM)
   GENPTR_REAL_ARRAY(XB)
   GENPTR_REAL_ARRAY(YB)
   GENPTR_REAL_ARRAY(ZB)
   GENPTR_REAL_ARRAY(BL)
   F77_INTEGER_TYPE(RESULT);
   char *text, *just, *p;

   text = astString( TEXT, TEXT_length );
   just = astString( JUST, JUST_length );

/* Ignore trailing spaces in the text */
   p = text + TEXT_length;
   while( !*p || *p == ' ' ) *(p--) = 0;

   if( astOK ) {
      RESULT = (F77_INTEGER_TYPE) astG3DTxExt( text, REF, just, UP, NORM,
                                               XB, YB, ZB, BL );
   } else {
      RESULT = 0;
   }

   (void) astFree( text );
   (void) astFree( just );

   return RESULT;
}


