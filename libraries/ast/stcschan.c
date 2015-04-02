/*
*class++
*  Name:
*     StcsChan

*  Purpose:
*     I/O Channel using STC-S to represent Objects.

*  Constructor Function:
c     astStcsChan
f     AST_STCSCHAN

*  Description:
*     A StcsChan is a specialised form of Channel which supports STC-S
*     I/O operations. Writing an Object to an StcsChan (using
c     astWrite) will, if the Object is suitable, generate an
f     AST_WRITE) will, if the Object is suitable, generate an
*     STC-S description of that Object, and reading from an StcsChan will
*     create a new Object from its STC-S description.
*
*     When an STC-S description is read using
c     astRead,
f     AST_READ,
*     the returned AST Object may be 1) a PointList describing the STC
*     AstroCoords (i.e. a single point of interest within the coordinate frame
*     described by the STC-S description), or 2) a Region describing the STC
*     AstrCoordsArea (i.e. an area or volume of interest within the coordinate
*     frame described by the STC-S description), or 3) a KeyMap
*     containing the uninterpreted property values read form the STC-S
*     description, or 4) a KeyMap containing any combination of the first
*     3 options. The attributes StcsArea, StcsCoords and StcsProps
*     control which of the above is returned by
c     astRead.
f     AST_READ.
*
*     When an STC-S description is created from an AST Object using
c     astWrite,
f     AST_WRITE,
*     the AST Object must be either a Region or a KeyMap. If it is a
*     Region, it is assumed to define the AstroCoordsArea or (if the
*     Region is a single point) the AstroCoords to write to the STC-S
*     description. If the Object is a KeyMap, it may contain an entry
*     with the key "AREA", holding a Region to be used to define the
*     AstroCoordsArea. It may also contain an entry with the key "COORDS",
*     holding a Region (a PointList) to be used to create the
*     AstroCoords. It may also contain an entry with key "PROPS", holding
*     a KeyMap that contains uninterpreted property values to be used as
*     defaults for any STC-S properties that are not determined by the
*     other supplied Regions. In addition, a KeyMap supplied to
c     astWrite
f     AST_WRITE
*     may itself hold the default STC-S properties (rather than defaults
*     being held in a secondary KeyMap, stored as the "PROPS" entry in the
*     supplied KeyMap).
*
*     The
c     astRead and astWrite
f     AST_READ and AST_WRITE
*     functions work together so that any Object returned by
c     astRead can immediately be re-written using astWrite.
f     AST_READ can immediately be re-written using AST_WRITE.
*
*     Normally, when you use an StcsChan, you should provide "source"
c     and "sink" functions which connect it to an external data store
c     by reading and writing the resulting text. These functions
f     and "sink" routines which connect it to an external data store
f     by reading and writing the resulting text. These routines
*     should perform any conversions needed between external character
c     encodings and the internal ASCII encoding. If no such functions
f     encodings and the internal ASCII encoding. If no such routines
*     are supplied, a Channel will read from standard input and write
*     to standard output.
*
*     Alternatively, an XmlChan can be told to read or write from
*     specific text files using the SinkFile and SourceFile attributes,
*     in which case no sink or source function need be supplied.
*
*     Support for STC-S is currently based on the IVOA document "STC-S:
*     Space-Time Coordinate (STC) Metadata Linear String Implementation",
*     version 1.30 (dated 5th December 2007), available at
*     http://www.ivoa.net/Documents/latest/STC-S.html. Note, this
*     document is a recommednation only and does not constitute an accepted
*     IVOA standard.
*
*     The full text of version 1.30 is supported by the StcsChan class,
*     with the following exceptions and provisos:
*
*     - When reading an STC-S phrase, case is ignored except when reading
*     units strings.
*     - There is no support for multiple intervals specified within a
*     TimeInterval, PositionInterval, SpectralInterval or RedshiftInterval.
*     - If the ET timescale is specified, TT is used instead.
*     - If the TEB timescale is specified, TDB is used instead.
*     - The LOCAL timescale is not supported.
*     - The AST TimeFrame and SkyFrame classes do not currently allow a
*     reference position to be specified. Consequently, any <refpos>
*     specified within the Time or Space sub-phrase of an STC-S document
*     is ignored.
*     - The Convex identifier for the space sub-phrase is not supported.
*     - The GEO_C and GEO_D space frames are not supported.
*     - The UNITSPHERE and SPHER3 space flavours are not supported.
*     - If any Error values are supplied in a space sub-phrase, then the
*     number of values supplied should equal the number of spatial axes,
*     and the values are assumed to specify an error box (i.e. error
*     circles, ellipses, etc, are not supported).
*     - The spectral and redshift sub-phrases do not support the
*     following <refpos> values: LOCAL_GROUP_CENTER, UNKNOWNRefPos,
*     EMBARYCENTER, MOON, MERCURY, VENUS, MARS, JUPITER, SATURN, URANUS,
*     NEPTUNE, PLUTO.
*     - Error values are supported but error ranges are not.
*     - Resolution, PixSize and Size values are ignored.
*     - Space velocity sub-phrases are ignored.

*  Inheritance:
*     The StcsChan class inherits from the Channel class.

*  Attributes:
*     In addition to those attributes common to all Channels, every
*     StcsChan also has the following attributes:
*
*     - StcsArea: Return the CoordinateArea component after reading an STC-S?
*     - StcsCoords: Return the Coordinates component after reading an STC-S?
*     - StcsLength: Controls output buffer length
*     - StcsProps: Return the STC-S properties after reading an STC-S?

*  Functions:
c     The StcsChan class does not define any new functions beyond those
f     The StcsChan class does not define any new routines beyond those
*     which are applicable to all Channels.

*  Copyright:
*     Copyright (C) 2009 Science & Technology Facilities Council.
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
*     DSB: David Berry (Starlink)

*  History:
*     18-DEC-2008 (DSB):
*        Original version.
*     22-MAY-2008 (DSB):
*        Retain default Equinox values in SkyFrame when reading an STC-S.
*     30-OCT-2009 (DSB):
*        Make case insensitive (except for units strings).
*     21-FEB-2014 (DSB):
*        Split long properties up into words when writing out an STC-S
*        description.
*     26-MAR-2015 (DSB):
*        Guard against seg faults if an error has already occured.
*class--
*/

/* Module Macros. */
/* ============== */
/* Set the name of the class we are implementing. This indicates to
   the header files that define class interfaces that they should make
   "protected" symbols available. */
#define astCLASS StcsChan

/* Values identifying particular forms of CoordArea */
#define NULL_ID                1
#define TIME_INTERVAL_ID       2
#define START_TIME_ID          3
#define STOP_TIME_ID           4
#define POSITION_INTERVAL_ID   5
#define ALLSKY_ID             6
#define CIRCLE_ID              7
#define ELLIPSE_ID             8
#define BOX_ID                 9
#define POLYGON_ID            10
#define CONVEX_ID             11
#define POSITION_ID           12
#define TIME_ID               13
#define SPECTRAL_INTERVAL_ID  14
#define SPECTRAL_ID           15
#define REDSHIFT_INTERVAL_ID  16
#define REDSHIFT_ID           17
#define VELOCITY_INTERVAL_ID  18
#define UNION_ID  	      19
#define INTERSECTION_ID       20
#define DIFFERENCE_ID         21
#define NOT_ID                22
#define VELOCITY_ID           23

/* The number of words used to form an extract from an STC-S description
   for use in an error message. */
#define NEWORD 10

/* Max length of string returned by GetAttrib */
#define GETATTRIB_BUFF_LEN 50

/* Include files. */
/* ============== */
/* Interface definitions. */
/* ---------------------- */

#include "frame.h"               /* Generic cartesian coordinate systems */
#include "globals.h"             /* Thread-safe global data access */
#include "error.h"               /* Error reporting facilities */
#include "memory.h"              /* Memory allocation facilities */
#include "object.h"              /* Base Object class */
#include "channel.h"             /* Interface for parent class */
#include "stcschan.h"            /* Interface definition for this class */
#include "loader.h"              /* Interface to the global loader */
#include "skyframe.h"            /* Celestial coordinate systems */
#include "timeframe.h"           /* Time coordinate systems */
#include "specframe.h"           /* Spectral coordinate systems */
#include "wcsmap.h"              /* PI-related constants */
#include "region.h"              /* Abstract regions */
#include "interval.h"            /* Axis intervals */
#include "unitmap.h"             /* Unit mappings */
#include "nullregion.h"          /* Boundless regions */
#include "cmpregion.h"           /* Compound regions */
#include "box.h"                 /* Box regions */
#include "prism.h"               /* Prism regions */
#include "circle.h"              /* Circle regions */
#include "ellipse.h"             /* Ellipse regions */
#include "polygon.h"             /* Polygon regions */
#include "pointlist.h"           /* Lists of points */
#include "keymap.h"              /* KeyMap interface */


/* Error code definitions. */
/* ----------------------- */
#include "ast_err.h"             /* AST error codes */

/* C header files. */
/* --------------- */
#include <ctype.h>
#include <float.h>
#include <limits.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* Module Types. */
/* ============= */
typedef struct WordContext {
   char *line;
   char *wnext;
   char *e;
   char f;
   int done;
   char *words[ NEWORD ];
   int next;
   int close;
   int open;
} WordContext;

/* Module Variables. */
/* ================= */

/* Pointers to parent class methods which are extended by this class. */
static const char *(* parent_getattrib)( AstObject *, const char *, int * );
static int (* parent_testattrib)( AstObject *, const char *, int * );
static void (* parent_clearattrib)( AstObject *, const char *, int * );
static void (* parent_setattrib)( AstObject *, const char *, int * );
static int (* parent_getindent)( AstChannel *, int * );

/* Address of this static variable is used as a unique identifier for
   member of this class. */
static int class_check;

/* Define macros for accessing each item of thread specific global data. */
#ifdef THREAD_SAFE

/* Define how to initialise thread-specific globals. */
#define GLOBAL_inits \
   globals->GetAttrib_Buff[ 0 ] = 0; \
   globals->Class_Init = 0;

/* Create the function that initialises global data for this module. */
astMAKE_INITGLOBALS(StcsChan)

/* Define macros for accessing each item of thread specific global data. */
#define getattrib_buff astGLOBAL(StcsChan,GetAttrib_Buff)
#define class_init astGLOBAL(StcsChan,Class_Init)
#define class_vtab astGLOBAL(StcsChan,Class_Vtab)


/* If thread safety is not needed, declare and initialise globals at static
   variables. */
#else

/* Define the class virtual function table and its initialisation flag
   as static variables. */
static AstStcsChanVtab class_vtab;   /* Virtual function table */
static int class_init = 0;       /* Virtual function table initialised? */

/* Buffer returned by GetAttrib. */
static char getattrib_buff[ GETATTRIB_BUFF_LEN + 1 ];

#endif


/* External Interface Function Prototypes. */
/* ======================================= */
/* The following functions have public prototypes only (i.e. no
   protected prototypes), so we must provide local prototypes for use
   within this module. */
AstStcsChan *astStcsChanForId_( const char *(*)( void ),
                                char *(*)( const char *(*)( void ), int * ),
                                void (*)( const char * ),
                                void (*)( void (*)( const char * ), const char *, int * ),
                                const char *, ... );
AstStcsChan *astStcsChanId_( const char *(* source)( void ),
                             void (* sink)( const char * ),
                             const char *options, ... );

/* Prototypes for Private Member Functions. */
/* ======================================== */
static AstKeyMap *ReadProps( AstStcsChan *, int * );
static AstObject *Read( AstChannel *, int * );
static AstPointList *SinglePointList( AstFrame *, double *, AstRegion *, int *);
static AstRegion *MakeSpaceRegion( AstKeyMap *, AstFrame *, double, int * );
static char *AddItem( AstStcsChan *, AstKeyMap *, const char *, const char *, char *, int *, int *, int, int * );
static char *ContextFragment( WordContext *, char **, int * );
static char *PutRegionProps( AstStcsChan *, AstKeyMap *, const char *, int, char *, int *, int *, int, int * );
static char *SourceWrap( const char *(*)( void ), int * );
static const char *GetNextWord( AstStcsChan *, WordContext *, int * );
static const char *ReadSpaceArgs( AstStcsChan *, const char *, int, int, WordContext *, AstKeyMap *, int * );
static double *BoxCorners( AstFrame *, const double[2], const double[2], int * );
static int GetIndent( AstChannel *, int * );
static int GetRegionProps( AstStcsChan *, AstRegion *, AstKeyMap *, int, int, double, int, int * );
static int SpaceId( const char *, int * );
static int Write( AstChannel *, AstObject *, int * );
static int WriteRegion( AstStcsChan *, AstRegion *, AstKeyMap *, int * );
static void Dump( AstObject *, AstChannel *, int * );
static void FreeContext( WordContext *, int * );
static void GetFmt( const char *, AstKeyMap *, int, int, char *, int * );
static void MapPut0C( AstKeyMap *, const char *, const char *, const char *, int, int * );
static void MapPut0D( AstKeyMap *, const char *, double, double, int, int * );
static void SetUnc( AstRegion *, AstRegion *, AstFrame *, int, double, double *, int, int * );
static void SinkWrap( void (*)( const char * ), const char *, int * );
static void WriteProps( AstStcsChan *, AstKeyMap *, int * );

static int GetStcsArea( AstStcsChan *, int * );
static int TestStcsArea( AstStcsChan *, int * );
static void ClearStcsArea( AstStcsChan *, int * );
static void SetStcsArea( AstStcsChan *, int, int * );

static int GetStcsCoords( AstStcsChan *, int * );
static int TestStcsCoords( AstStcsChan *, int * );
static void ClearStcsCoords( AstStcsChan *, int * );
static void SetStcsCoords( AstStcsChan *, int, int * );

static int GetStcsProps( AstStcsChan *, int * );
static int TestStcsProps( AstStcsChan *, int * );
static void ClearStcsProps( AstStcsChan *, int * );
static void SetStcsProps( AstStcsChan *, int, int * );

static void ClearAttrib( AstObject *, const char *, int * );
static const char *GetAttrib( AstObject *, const char *, int * );
static void SetAttrib( AstObject *, const char *, int * );
static int TestAttrib( AstObject *, const char *, int * );

static int TestStcsLength( AstStcsChan *, int * );
static void ClearStcsLength( AstStcsChan *, int * );
static void SetStcsLength( AstStcsChan *, int, int * );
static int GetStcsLength( AstStcsChan *, int * );

/* Member functions. */
/* ================= */

static char *AddItem( AstStcsChan *this, AstKeyMap *km, const char *key,
                      const char *prefix, char *line, int *nc, int *crem,
                      int linelen, int *status ){
/*
*  Name:
*     AddItem

*  Purpose:
*     Add an STC-S property item to a buffer.

*  Type:
*     Private function.

*  Synopsis:
*     #include "stcschan.h"
*     char *AddItem( AstStcsChan *this, AstKeyMap *km, const char *key,
*                    const char *prefix, char *line, int *nc, int *crem,
*                    int linelen, int *status )

*  Class Membership:
*     StcsChan member function

*  Description:
*     This function appends text describing a singlke STC-S property to
*     a supplied text buffer, handling the splitting of text into lines.

*  Parameters:
*     this
*        The StcsChan.
*     km
*        Pointer to a KeyMap containing the STC-S properties.
*     key
*        The key name associated with the property to be checked.
*     prefix
*        if not NULL, this is a string that is to be written out before
*        the property value. It should usually include a trailing space.
*     line
*        Pointer to the buffer to recieve the prefix and property value.
*     nc
*        Pointer to an int in which to store the number of characters in
*        the buffer. Updated on exit.
*     crem
*        Pointer to an int in which to store the maximum number of
*        characters before a new line. Ignored if linelen is zero. Updated
*        on exit.
*     linelen
*        The maximum number of character per line, or zero if all text is
*        to be included in a single line.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     A pointer to the buffer. This will usually be "line", but may be
*     different to "line" if it was necessary to expand the memory to make
*     room for the new property.

*/

/* Local Variables: */
   char *result;          /* Returned pointer */
   char **words;          /* All words */
   const char *text;      /* Property value */
   const char *word;      /* Single word */
   int iw;                /* Word index */
   int len;               /* Length of new text */
   int nw;                /* Number of words in property */

/* Initialise */
   result = line;
   len = 0;

/* Check the global error status. */
   if ( !astOK ) return result;

/* If the KeyMap contains the required property... */
   if( astMapGet0C( km, key, &text ) ) {

/* Add any supplied prefix to the returned buffer. */
      if( prefix ) {
         len = strlen( prefix );
         if( len > *crem && len < linelen ) {
            astPutNextText( this, result );
            *nc = 0;
            result = astAppendString( result, nc, "   " );
            *crem = linelen - 3;
         }
         result = astAppendString( result, nc, prefix );
         *crem -= len;
      }

/* Split the property into words. */
      words = astChrSplit( text, &nw );

/* Append each word to the buffer. */
      for( iw = 0; iw < nw; iw++ ) {
         word = words[ iw ];

/* If required, get the number of characters to be added to the buffer. */
         if( linelen ) {
            len = strlen( word );

/* If there is insufficient room left, write out the text through the
   Channel sink function, and start a new line with three spaces. Then
   reset the number of character remaining in the line. */
            if( len > *crem && len < linelen ) {
               astPutNextText( this, result );
               *nc = 0;
               result = astAppendString( result, nc, "   " );
               *crem = linelen - 3;
            }

/* Reduce crem to account for the text that is about to be added to the
   line. */
            *crem -= len;
         }

/* Add the property value to the returned buffer. */
         result = astAppendString( result, nc, word );

/* Add a traling space to the returned buffer, if there is room. */
         if( !linelen || *crem > 0 ) {
            result = astAppendString( result, nc, " " );
            (*crem)--;
         }
      }

/* Free the words buffer. */
      if( words ) {
         for( iw = 0; iw < nw; iw++ ) words[ iw ] = astFree( words[ iw ] );
         words = astFree( words );
      }
   }

/* Return the buffer pointer. */
   return result;
}

static double *BoxCorners( AstFrame *frm, const double centre[2],
                           const double bsize[2], int *status ) {
/*
*  Name:
*     BoxCorners

*  Purpose:
*     Determine the positions of the corners of an STC Box.

*  Type:
*     Private function.

*  Synopsis:
*     #include "stcschan.h"
*     double *BoxCorners( AstFrame *frm, const double centre[2],
*                         const double bsize[2], int *status )

*  Class Membership:
*     StcsChan member function

*  Description:
*     This function returns a pointer to a dynamically allocated array
*     holding the positions of the corners of the STC Box defined by the
*     supplied "centre" and "bsize" arrays.

*  Parameters:
*     frm
*        Pointer to the Frame in which the Box is defined. Must be 2-D.
*     centre
*        Two element array holding the Frame co-ordinates at the centre
*        of the Box.
*     bsize
*        Two element array holding the full width and height of the Box.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     A pointer to a dynamically allocated array holding the axis values
*     at the four corners, in a form suitable for passing to the
*     astPolygon constructor function. NULL is returned if an error has
*     already occurred, of if this function fails for any reason.
*/

/* Local Variables: */
   double *result;         /* Returned pointer. */
   double bh1[ 2 ];        /* A first point on the bottom horizontal edge */
   double bh2[ 2 ];        /* A second point on the bottom horizontal edge */
   double blc[ 2 ];        /* Position of bottom left corner */
   double brc[ 2 ];        /* Position of bottom right corner */
   double lv1[ 2 ];        /* A first point on the left vertical edge */
   double lv2[ 2 ];        /* A second point on the left vertical edge */
   double pa;              /* Position angle of great circle/straight line */
   double rv1[ 2 ];        /* A first point on the right vertical edge */
   double rv2[ 2 ];        /* A second point on the right vertical edge */
   double th1[ 2 ];        /* A first point on the top horizontal edge */
   double th2[ 2 ];        /* A second point on the top horizontal edge */
   double tlc[ 2 ];        /* Position of top left corner */
   double trc[ 2 ];        /* Position of top right corner */

/* Initialise. */
   result = NULL;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Check the Frame is 2-dimensional. */
   if( astGetNaxes( frm ) != 2 ) {
      astError( AST__BADIN, "astRead(StcsChan): Supplied space frame has "
                "%d axes.", status, astGetNaxes( frm ) );
      astError( AST__BADIN, "astRead(StcsChan): Can only use STC Box regions "
                "with 2-dimensional space frames.", status );
   }

/* Offset away from the centre by half the Box width along a great circle
   initially parallel to the positive first frame axis (i.e. position
   angle +pi/2). The end position goes in "rv1" and the position angle of
   the great circle (or straight line) at that point is returned as the
   function value. NOTE, the use of the words "left" and "right" below is
   vague because it depends on whether we are using a SkyFrame (which has
   a reversed first axis) or a basic Frame. In general, the choice of "left"
   and "right" below is appropriate for a basic Frame. */
   pa = astOffset2( frm, centre, AST__DPIBY2, bsize[ 0 ]/2, rv1 );

/* Turn by 90 degrees and offset away by half the box height. This is done
   so that we have a second point (rv2) to define the great circle (or
   straight line) that forms the first vertical edge of the Box (i.e. the
   great circle or straight line through rv1 and rv2). Note, for spherical
   Frames (i.e. SkyFrames) "rv2" is not necessarily a corner of the box. */
   (void) astOffset2( frm, rv1, pa + AST__DPIBY2, bsize[ 1 ]/2, rv2 );

/* In the same way, get two points on the second vertical Box edge. */
   pa = astOffset2( frm, centre, -AST__DPIBY2, bsize[ 0 ]/2, lv1 );
   (void) astOffset2( frm, lv1, pa + AST__DPIBY2, bsize[ 1 ]/2, lv2 );

/* In the same way, get two points on the top horizontal Box edge. */
   pa = astOffset2( frm, centre, 0.0, bsize[ 1 ]/2, th1 );
   (void) astOffset2( frm, th1, pa + AST__DPIBY2, bsize[ 0 ]/2, th2 );

/* In the same way, get two points on the bottom horizontal Box edge. */
   pa = astOffset2( frm, centre, AST__DPI, bsize[ 1 ]/2, bh1 );
   (void) astOffset2( frm, bh1, pa + AST__DPIBY2, bsize[ 0 ]/2, bh2 );

/* The first corner of the Box is at the intersection of the first
   vertical and top horizontal edges. */
   astIntersect( frm, lv1, lv2, th1, th2, tlc );

/* The top right corner of the Box is at the intersection of the right
   vertical and top horizontal edges. */
   astIntersect( frm, rv1, rv2, th1, th2, trc );

/* The bottom left corner of the Box is at the intersection of the left
   vertical and bottom horizontal edges. */
   astIntersect( frm, lv1, lv2, bh1, bh2, blc );

/* The bottom right corner of the Box is at the intersection of the right
   vertical and bottom horizontal edges. */
   astIntersect( frm, rv1, rv2, bh1, bh2, brc );

/* Gather the corners together into an array suitable for use with
   astPolygon. Make sure the vertices are traversed in an ant-clockwise
   sense whether in a SkyFrame or a basic Frame. */
   result = astMalloc( 8*sizeof( *result ) );
   if( result ) {
      if( astIsASkyFrame( frm ) ) {
         result[ 0 ] = tlc[ 0 ];
         result[ 1 ] = trc[ 0 ];
         result[ 2 ] = brc[ 0 ];
         result[ 3 ] = blc[ 0 ];
         result[ 4 ] = tlc[ 1 ];
         result[ 5 ] = trc[ 1 ];
         result[ 6 ] = brc[ 1 ];
         result[ 7 ] = blc[ 1 ];
      } else {
         result[ 3 ] = tlc[ 0 ];
         result[ 2 ] = trc[ 0 ];
         result[ 1 ] = brc[ 0 ];
         result[ 0 ] = blc[ 0 ];
         result[ 7 ] = tlc[ 1 ];
         result[ 6 ] = trc[ 1 ];
         result[ 5 ] = brc[ 1 ];
         result[ 4 ] = blc[ 1 ];
      }

   }

/* Return the pointer. */
   return result;
}

static void ClearAttrib( AstObject *this_object, const char *attrib, int *status ) {
/*
*  Name:
*     ClearAttrib

*  Purpose:
*     Clear an attribute value for a StcsChan.

*  Type:
*     Private function.

*  Synopsis:
*     #include "stcschan.h"
*     void ClearAttrib( AstObject *this, const char *attrib, int *status )

*  Class Membership:
*     StcsChan member function (over-rides the astClearAttrib protected
*     method inherited from the Channel class).

*  Description:
*     This function clears the value of a specified attribute for a
*     StcsChan, so that the default value will subsequently be used.

*  Parameters:
*     this
*        Pointer to the StcsChan.
*     attrib
*        Pointer to a null terminated string specifying the attribute
*        name.  This should be in lower case with no surrounding white
*        space.
*     status
*        Pointer to the inherited status variable.
*/

/* Local Variables: */
   AstStcsChan *this;              /* Pointer to the StcsChan structure */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain a pointer to the StcsChan structure. */
   this = (AstStcsChan *) this_object;

/* Check the attribute name and clear the appropriate attribute. */

   if ( !strcmp( attrib, "stcsarea" ) ) {
      astClearStcsArea( this );

   } else if ( !strcmp( attrib, "stcscoords" ) ) {
      astClearStcsCoords( this );

   } else if ( !strcmp( attrib, "stcsprop" ) ) {
      astClearStcsProps( this );

   } else if ( !strcmp( attrib, "stcslength" ) ) {
      astClearStcsLength( this );

/* If the attribute is still not recognised, pass it on to the parent
   method for further interpretation. */
   } else {
      (*parent_clearattrib)( this_object, attrib, status );
   }
}

static char *ContextFragment( WordContext *con, char **buf, int *status ){
/*
*  Name:
*     ContextFragment

*  Purpose:
*     Returns a string holding a fragment of the document being read.

*  Type:
*     Private function.

*  Synopsis:
*     #include "stcschan.h"
*     char *ContextFragment( WordContext *con, char **buf, int *status )

*  Class Membership:
*     StcsChan member function

*  Description:
*     This function returns a pointer to a string that holds a fragment
*     of the STC-S document currently being read. The fragment ends at
*     the last word read by function GetNextWord, and starts a certain
*     number of words earlier in the document, as specified by the NEWORD
*     macro.

*  Parameters:
*     con
*        Pointer to the context structure, managed by GetNextWord.
*     buf
*        Address of a pointer to a dynamically allocated buffer. This
*        pointer should be NULL on the first call to this function, and
*        will be updated by this function. The pointer should be freed
*        using astFree when no longer needed.
*     status
*        Address of the inherited status value.

*  Returned Value:
*     A pointer to the buffer.
*/

/* Local Variables: */
   int i;                     /* Word count */
   int j;                     /* Word index */
   int nc;                    /* Text length */

/* Initialise the number of characters written to the buffer. */
   nc = 0;

/* Get the index of the first word to add to the buffer. The "next"
   component of the context structure holds the index at which the word
   returned by the next call to GetNextWord will be stored. So at the
   moment, this is the index of the oldest word in the cyclic list. */
   j = con->next;

/* Loop round all non-NULL words in the cyclic list. */
   for( i = 0; i < NEWORD; i++ ) {
      if( con->words[ j ] ) {

/* Append this word to the buffer, extending the buffer size as
   necessary. */
         *buf = astAppendString( *buf, &nc, con->words[ j ] );

/* Append a trailingh space. */
         *buf = astAppendString( *buf, &nc, " " );
      }

/* Increment the index of the next word to use in the cyclic list. Wrap
   back to zerp when the end of the list is reached. */
      if( ++j == NEWORD ) j = 0;
   }

/* Remove the final trailing space. */
   if( nc ) (*buf)[ nc - 1 ] = 0;

/* Return a pointer to the supplied buffer. */
   return *buf;
}

static void FreeContext( WordContext *con, int *status ){
/*
*  Name:
*     FreeContext

*  Purpose:
*     Free the resources used by a word-reading context structure.

*  Type:
*     Private function.

*  Synopsis:
*     #include "stcschan.h"
*     voidFreeContext( WordContext *con, int *status );

*  Class Membership:
*     StcsChan member function

*  Description:
*     This function frees the resources used by the supplied WordContext
*     structure. This structure is used by GetNextWord to keep track of
*     which word to return next.
*
*     This function frees the dynamic memory pointers stored within the
*     WordContext structure, but does not free the memory holding the
*     WordContext structure itself.

*  Parameters:
*     con
*        Pointer to a structure holding the context.
*     status
*        Pointer to the inherited status variable.

*/

/* Local Variables: */
   int i;                         /* Word index */

/* Check the supplied pointer. */
   if ( !con ) return;

/* Free the resources. */
   con->line = astFree( con->line );

   for( i = 0; i < NEWORD; i++ ) {
      con->words[ i ] = astFree( con->words[ i ] );
   }

}

static const char *GetAttrib( AstObject *this_object, const char *attrib, int *status ) {
/*
*  Name:
*     GetAttrib

*  Purpose:
*     Get the value of a specified attribute for a StcsChan.

*  Type:
*     Private function.

*  Synopsis:
*     #include "stcschan.h"
*     const char *GetAttrib( AstObject *this, const char *attrib, int *status )

*  Class Membership:
*     StcsChan member function (over-rides the protected astGetAttrib
*     method inherited from the Channel class).

*  Description:
*     This function returns a pointer to the value of a specified
*     attribute for a StcsChan, formatted as a character string.

*  Parameters:
*     this
*        Pointer to the StcsChan.
*     attrib
*        Pointer to a null terminated string containing the name of
*        the attribute whose value is required. This name should be in
*        lower case, with all white space removed.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     - Pointer to a null terminated string containing the attribute
*     value.

*  Notes:
*     - The returned string pointer may point at memory allocated
*     within the StcsChan, or at static memory. The contents of the
*     string may be over-written or the pointer may become invalid
*     following a further invocation of the same function or any
*     modification of the StcsChan. A copy of the string should
*     therefore be made if necessary.
*     - A NULL pointer will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*/

/* Local Variables: */
   astDECLARE_GLOBALS            /* Declare the thread specific global data */
   AstStcsChan *this;            /* Pointer to the StcsChan structure */
   const char *result;           /* Pointer value to return */
   int ival;                     /* Integer attribute value */

/* Initialise. */
   result = NULL;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Get a pointer to the structure holding thread-specific global data. */
   astGET_GLOBALS(this_object);

/* Obtain a pointer to the StcsChan structure. */
   this = (AstStcsChan *) this_object;

/* Compare "attrib" with each recognised attribute name in turn,
   obtaining the value of the required attribute. If necessary, write
   the value into "getattrib_buff" as a null terminated string in an appropriate
   format.  Set "result" to point at the result string. */

/* StcsArea. */
/* --------- */
   if ( !strcmp( attrib, "stcsarea" ) ) {
      ival = astGetStcsArea( this );
      if ( astOK ) {
         (void) sprintf( getattrib_buff, "%d", ival );
         result = getattrib_buff;
      }

/* StcsCoords. */
/* ----------- */
   } else if ( !strcmp( attrib, "stcscoords" ) ) {
      ival = astGetStcsCoords( this );
      if ( astOK ) {
         (void) sprintf( getattrib_buff, "%d", ival );
         result = getattrib_buff;
      }


/* StcsProps. */
/* ---------- */
   } else if ( !strcmp( attrib, "stcsprops" ) ) {
      ival = astGetStcsProps( this );
      if ( astOK ) {
         (void) sprintf( getattrib_buff, "%d", ival );
         result = getattrib_buff;
      }

/* StcsLength */
/* --------- */
   } else if ( !strcmp( attrib, "stcslength" ) ) {
      ival = astGetStcsLength( this );
      if ( astOK ) {
         (void) sprintf( getattrib_buff, "%d", ival );
         result = getattrib_buff;
      }

/* If the attribute name was not recognised, pass it on to the parent
   method for further interpretation. */
   } else {
      result = (*parent_getattrib)( this_object, attrib, status );
   }

/* Return the result. */
   return result;

}

static void GetFmt( const char *key, AstKeyMap *props, int i, int defdigs,
                    char *fmt, int *status ){
/*
*  Name:
*     GetFmt

*  Purpose:
*     Decide how many digits to use when formatting a numerical STC-S
*     property value.

*  Type:
*     Private function.

*  Synopsis:
*     #include "stcschan.h"
*     void GetFmt( const char *key, AstKeyMap *props, int i,
*                  int defdigs, char *fmt, int *status )

*  Class Membership:
*     StcsChan member function

*  Description:
*     This function locates the named property in the supplied KeyMap. If
*     it is found, a printf format specifier is generated that matches
*     the value is determined and returned. Otherwise, a default format
*     specified based on the supplied default number of digits is returned.

*  Parameters:
*     key
*        The key name associated with the property.
*     km
*        Pointer to a KeyMap containing the STC-S properties.
*     i
*        For vector values, this is the index of the vector element to be
*        checked. Should be zero for scalar values. If "i" is greater
*        than the number of values in the vector, then the number of digits
*        in the first element is found and returned.
*     defdigs
*        The value to return if the KeyMap does not contain an entry with
*        the supplied key.
*     fmt
*        Pointer to a string in which to return the format specifier.
*     status
*        Pointer to the inherited status variable.

*/

/* Local Variables: */
   const char *dot;       /* Pointer to decimal point */
   const char *p;         /* Pointer to next character */
   const char *word;      /* Property value */
   int after0;            /* Digits after the decimal point in first word */
   int after;             /* Digits after the decimal point in current word */
   int before0;           /* Digits before the decimal point in first word */
   int before;            /* Digits before the decimal point in current word */
   int exp0;              /* Was an exponent found in first word? */
   int exp;               /* Was an exponent found in current word? */
   int j;                 /* Index of current word */

/* Check the global error status. */
   if ( !astOK ) return;

/* Initialise. */
   exp = 1;
   before = defdigs;
   after = 0;
   exp0 = 0;
   before0 = 0;
   after0 = 0;

/* If the KeyMap contains the required property... */
   if( astMapGet0C( props, key, &word ) ) {

/* Skip over the words in the string. */
      p = word;
      for( j = 0; j <= i; j++ ) {

/* Find the next space or terminating null at the end of the current word.
   Also count the number of digits before and after the decimal point and
   see if the word includes an exponent. */
         exp = 0;
         before = 0;
         after = 0;
         dot = NULL;

         while( *p != 0 && *p != ' ' ) {
            if( ! exp ) {
               if( isdigit( *p ) ) {
                  if( dot ) {
                     after++;
                  } else {
                     before++;
                  }

               } else if( *p == '.' ) {
                  dot = p;

               } else if( *p == 'e' || *p == 'E' ) {
                  exp = 1;
               }
            }
            p++;
         }

/* Note the values for the first word. */
         if( j == 0 ) {
            exp0 = exp;
            before0 = before;
            after0 = after;
         }

/* Find the following non-space marking the start of the next word,
   or the terminating null. */
         while( *p != 0 && *p == ' ' ) p++;

/* If we find the terminating null before we have found the i'th word,
   break out of the loop using the first word instead of the i'th word. */
         if( *p == 0 ) {
            exp = exp0;
            before = before0;
            after = after0;
            break;
         }
      }
   }

   if( exp ) {
      sprintf( fmt, "%%.%dg", before + after );
   } else {
      sprintf( fmt, "%%.%df", after );
   }
}

static int GetIndent( AstChannel *this, int *status ) {
/*
*  Name:
*     GetIndent

*  Purpose:
*     Get the value of the Indent attribute for a StcsChan.

*  Type:
*     Private function.

*  Synopsis:
*     #include "stcschan.h"
*     int GetIndent( AstChannel *this, int *status )

*  Class Membership:
*     StcsChan member function (over-rides the protected astGetIndent
*     method inherited from the Channel class).

*  Description:
*     This function returns the value of the Indent attribute, supplying
*     a default value appropriate to an StcsChan.

*  Parameters:
*     this
*        Pointer to the StcsChan.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     - The Indent value to use.

*/

/* If the attribute is set, return its value. Otherwise return a value of
   zero. */
   return astTestIndent( this ) ? (*parent_getindent)( this, status ) : 0;
}

static const char *GetNextWord( AstStcsChan *this, WordContext *con,
                                int *status ){
/*
*  Name:
*     GetNextWord

*  Purpose:
*     Get a pointer to the next input word read from an STC-S source.

*  Type:
*     Private function.

*  Synopsis:
*     #include "stcschan.h"
*     const char *GetNextWord( AstStcsChan *this, WordContext *con,
*                              int *status )

*  Class Membership:
*     StcsChan member function

*  Description:
*     This function returns a pointer to the next word of an STC-S
*     description.

*  Parameters:
*     this
*        Pointer to the StcsChan, or NULL (to initialise "con").
*     con
*        Pointer to a structure holding context. The structure should be
*        initialised by calling this function with a NULL "this" pointer
*        before making further use of this function. When finished, it
*        should be released using FreeContext.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     A pointer to the new word. NULL is returned if an error has already
*     occurred, of if "this" is NULL.
*/

/* Local Variables: */
   const char *result;            /* Returned pointer. */
   int i;                         /* Word index */
   size_t len;                    /* Word length */

/* Initialise. */
   result = NULL;

/* Check the global error status. */
   if ( !astOK ) return result;

/* If no StcChan was supplied, initialise the supplied WordContext. */
   if( !this ) {
      con->e = NULL;
      con->line = NULL;
      con->done = 0;
      con->next = 0;
      con->wnext = NULL;
      con->close = 0;
      con->open = 0;
      for( i = 0; i < NEWORD; i++ ) con->words[ i ] = NULL;

/* Words that end with an opening parenthesis are treated as two words. If the
   previous word ended in an opening parenthesis, it will have been removed by
   the previous call to this function and the "con->open" flag set. In
   this case, we just return a pointer to the second of the two words - a
   single "(" character - and clear the "con->open" flag. */
   } else if( con->open && ! con->done ) {
      con->open = 0;
      result = "(";

/* Likewise deal with words that end with a closing parenthesis. */
   } else if( con->close && ! con->done ) {
      con->close = 0;
      result = ")";

/* Words that begin with an opening parenthesis are treated as two words. If
   the previous word was such an opening parenthesis, the rest of the word
   will have been removed by the previous call to this function and the
   "con->wnext" pointer set to the start of the remaining word. In
   this case, re-instate the original character that was replaced by a
   terminating null when the previous word was returned, return the
   "con->wnext" pointer, and then clear the pointer. */
   } else if( con->wnext && ! con->done ) {
      *(con->wnext) = con->f;
      result = con->wnext;
      con->wnext = NULL;

/* Otherwise... */
   } else {

/* If the previous invocation of this function converted a space
   character into a null character, change it back again. */
      if( con->e ) *(con->e) = ' ';

/* Get a pointer to the next non-white character in the current line of
   input text. */
      result = con->e;
      if( result ) {
         while( *result && isspace( *result ) ) result++;
      }

/* If we have exhausted the current line, get the next line by invoking
   the source function. We loop until we read a line that is not entirely
   blank. */
      while( ( !result || ! *result ) && astOK ) {

/* First free the memory holding the previous line. */
         if( con->line ) con->line = astFree( con->line );
         con->e = NULL;

/* Get the next line of text from the source function. */
         con->line = astGetNextText( this );
         result = con->line;

/* Break when we reach the end of the input text. */
         if( !result ) break;

/* Get a pointer to the first non-white character in the new line. */
         while( *result && isspace( *result ) ) result++;
      }

/* Find the end of the word. */
      if( result && *result ) {
         con->e = (char *) result + 1;
         while( *(con->e) && !isspace( *(con->e) ) ) (con->e)++;

/* If the word is already null-terminated, nullify the "e" pointer to
   indicate this. Otherwise, change the white-space character into a
   null. */
         if( *(con->e) ) {
            *(con->e) = 0;
            len = con->e - result;
         } else {
            con->e = NULL;
            len = strlen( result );
         }

/* Add the word into the cyclic list of words used to form a document
   fragment to include in error and warning messages. */
         con->words[ con->next ] = astStore( con->words[ con->next ],
                                             result, len + 1 );
         if( ++(con->next) == NEWORD ) con->next = 0;

/* Deal with words that include an opening or closing parenthesis at
   start or end. These words must have 2 or more characters. */
         if( len > 1 ) {

/* If the word ends with an opening parenthesis, replace the parenthesis
   with a null character and set a flag indicating that the next word
   returned should consist of just an opening parenthesis. */
            if( result[ len - 1 ] == '(' ) {
               ((char *) result)[ len - 1 ] = 0;
               con->open = 1;

/* If the word ends with a closing parenthesis, replace the parenthesis
   with a null character and set a flag indicating that the next word
   returned should consist of just a closing parenthesis. */
            } else if( result[ len - 1 ] == ')' ) {
               ((char *) result)[ len - 1 ] = 0;
               con->close = 1;

/* If the word starts with an opening parenthesis, replace the parenthesis
   with a null character and set a flag indicating that the next word
   returned should consist of just a closing parenthesis. */
            } else if( result[ 0 ] == '(' ) {
               con->wnext = ( (char *) result ) + 1;
               con->f = *(con->wnext);
               *(con->wnext) = 0;
            }
         }

/* If we have run out of input words, but we have not yet finished
   interpreting the previous word returned, return a null string, rather
   than a null pointer in order to allow further interpretation of the
   previous word. */
      } else if( ! con->done ) {
         result = "";
      }
   }

/* Return the pointer to the next word. */
   return result;
}

static int GetRegionProps( AstStcsChan *this, AstRegion *spreg,
                           AstKeyMap *spprops, int nspace, int defdigs,
                           double scale, int issky, int *status ) {
/*
*  Name:
*     GetRegionProps

*  Purpose:
*     Create STC-S properties to describe a given Region and store in a
*     KeyMap.

*  Type:
*     Private function.

*  Synopsis:
*     #include "stcschan.h"
*     int GetRegionProps( AstStcsChan *this, AstRegion *spreg,
*                         AstKeyMap *spprops, int *status )

*  Class Membership:
*     StcsChan member function

*  Description:
*     This function creates a set of STC-S properties to describe the
*     supplied spatial (2D) Region, and stores them in the supplied KeyMap.

*  Parameters:
*     this
*        The StcsChan being used.
*     spreg
*        The 2-D spatial Region to be described.
*     spprops
*        A KeyMap in which to store the created properties.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     Returns the integer code for the spatial region, or NULL_ID if the
*     properties could not be created for any reason.

*/


/* Local Variables: */
   AstKeyMap *new_props;   /* KeyMap holding component Region properties */
   AstMapping *sreg;       /* Simplified Region */
   AstRegion **reg_list;   /* Array of component Regioon pointers */
   char *prop;             /* Formatted property string */
   char buf[ 100 ];        /* Buffer for formatted values */
   char fmt[ 10 ];         /* Buffer for format specifier */
   double *p;              /* Pointer to next axis value */
   double *points;         /* Pointer to array of Region axis values */
   double a;               /* Circle or ellipse radius */
   double angle;           /* Ellipse position angle */
   double b;               /* Ellipse radius */
   double centre[ 3 ];     /* Circle or ellipse centre */
   double lbnd[ 3 ];       /* Region lower bounds */
   double ubnd[ 3 ];       /* Region upper bounds */
   int i;                  /* Loop index */
   int j;                  /* Loop index */
   int nc;                 /* Number of characters in "prop" string */
   int np;                 /* Number of points defining the Region */
   int nreg;               /* Number of component Regions */
   int ok;                 /* Can the Region be written out? */
   int oper;               /* Code for CmpRegion boolean operator */
   int spaceid;            /* Identifier for STC-S spatial region type */

/* Check inherited status */
   if( !astOK ) return NULL_ID;

/* Initialise */
   spaceid = NULL_ID;
   ok = 1;
   prop = NULL;

/* If the Region has been negated, temporarily negate the Region, and
   write its properties into a new KeyMap by calling this function
   recursively. Then store the new KeyMap in the supplied KeyMap. */
   if( astGetNegated( spreg ) ) {
      spaceid = NOT_ID;
      astNegate( spreg );
      new_props = astKeyMap( " ", status );

      if( GetRegionProps( this, spreg, new_props, nspace, defdigs,
                          scale, issky, status ) == NULL_ID ) ok = 0;

      astMapPut0C( spprops, "ID", "Not", NULL );
      astMapPut0A( spprops, "REGION1", new_props, NULL );
      astMapPut0I( spprops, "NREG", 1, NULL );
      astNegate( spreg );

/* Store properties that are specific to AllSky sub-phrases (i.e. none)... */
   } else if( astIsANullRegion( spreg ) && astGetNegated( spreg ) ) {
      spaceid = ALLSKY_ID;
      astMapPut0C( spprops, "ID", "AllSky", NULL );

/* Store properties that are specific to Circle sub-phrases... */
   } else if( astIsACircle( spreg ) ) {
      spaceid = CIRCLE_ID;
      astMapPut0C( spprops, "ID", "Circle", NULL );

/* Get the geometric parameters of the Circle. */
      astCirclePars( spreg, centre, &a, NULL );

/* Create a string holding the formatted centre axis values, scaling
   to the required units. Use the Frame's Digits attribute to specify
   how many digits to use when formatting the axis values. */
      nc = 0;
      for( i = 0; i < nspace; i++ ) {
         if( centre[ i ] != AST__BAD ) {
            GetFmt( "CENTRE", spprops, i, defdigs, fmt, status );
            (void) sprintf( buf, fmt, scale*centre[ i ] );
            prop = astAppendString( prop, &nc, buf );
            prop = astAppendString( prop, &nc, " " );

         } else {
            ok = 0;
            astAddWarning( this, 1, "The supplied Circle contains "
                           "one or more bad centre axis values.",
                           "astWrite", status );
            break;
         }
      }

/* Remove the trailing space, and store the property value in the KeyMap. */
      prop[ nc - 1 ] = 0;
      astMapPut0C( spprops, "CENTRE", prop, NULL );

/* Scale, format and store the radius. */
      if( a != AST__BAD ) {
         GetFmt( "RADIUS", spprops, 0, defdigs, fmt, status );
         (void) sprintf( buf, fmt, scale*a );
         astMapPut0C( spprops, "RADIUS", buf, NULL );
      } else {
         ok = 0;
         astAddWarning( this, 1, "The supplied Circle has an "
                        "undefined radius.", "astWrite", status );
      }

/* Store properties that are specific to PositionInterval sub-phrases... */
   } else if( astIsAInterval( spreg ) || astIsABox( spreg ) ) {
      spaceid = POSITION_INTERVAL_ID;
      astMapPut0C( spprops, "ID", "PositionInterval", NULL );

/* Get the bounds of the Region. */
      astGetRegionBounds( spreg, lbnd, ubnd );

/* Create a string holding the formatted low limits, scaling to the
   required units. Use the Frame's Digits attribute to specify how
   many digits to use when formatting the axis values. */
      nc = 0;
      for( i = 0; i < nspace; i++ ) {
         if( lbnd[ i ] == AST__BAD || lbnd[ i ] == DBL_MAX ||
             lbnd[ i ] == -DBL_MAX ) {
            astAddWarning( this, 1, "Spatial axis %d has an undefined "
                           "lower limit.", "astWrite", status, i + 1 );
            ok = 0;
            break;
         } else {
            GetFmt( "LOLIMIT", spprops, i, defdigs, fmt, status );
            (void) sprintf( buf, fmt, scale*lbnd[ i ] );
            prop = astAppendString( prop, &nc, buf );
            prop = astAppendString( prop, &nc, " " );
         }
      }

/* Remove the trailing space, and store the property value in the KeyMap. */
      prop[ nc - 1 ] = 0;
      astMapPut0C( spprops, "LOLIMIT", prop, NULL );

/* Do the same for the upper limits. */
      nc = 0;
      for( i = 0; i < nspace; i++ ) {
         if( ubnd[ i ] == AST__BAD || ubnd[ i ] == DBL_MAX ||
             ubnd[ i ] == -DBL_MAX ) {
            astAddWarning( this, 1, "Spatial axis %d has an undefined "
                           "upper limit.", "astWrite", status, i + 1 );
            ok = 0;
            break;
         } else {
            GetFmt( "HILIMIT", spprops, i, defdigs, fmt, status );
            (void) sprintf( buf, fmt, scale*ubnd[ i ] );
            prop = astAppendString( prop, &nc, buf );
            prop = astAppendString( prop, &nc, " " );
         }
      }

/* Remove the trailing space, and store the property value in the KeyMap. */
      prop[ nc - 1 ] = 0;
      astMapPut0C( spprops, "HILIMIT", prop, NULL );

/* Store properties that are specific to Ellipse sub-phrases... */
   } else if( astIsAEllipse( spreg ) ) {
      spaceid = ELLIPSE_ID;
      astMapPut0C( spprops, "ID", "Ellipse", NULL );

/* Get the geometric parameters of the Ellipse. */
      astEllipsePars( spreg, centre, &a, &b, &angle, NULL, NULL );

/* Create a string holding the formatted centre axis values, scaling
   to the required units. Use the Frame's Digits attribute to specify
   how many digits to use when formatting the axis values. */
      nc = 0;
      for( i = 0; i < nspace; i++ ) {
         if( centre[ i ] != AST__BAD ) {
            GetFmt( "CENTRE", spprops, i, defdigs, fmt, status );
            (void) sprintf( buf, fmt, scale*centre[ i ] );
            prop = astAppendString( prop, &nc, buf );
            prop = astAppendString( prop, &nc, " " );

         } else {
            ok = 0;
            astAddWarning( this, 1, "The supplied Ellipse contains "
                           "one or more bad centre axis values.",
                           "astWrite", status );
            break;
         }
      }

/* Remove the trailing space, and store the property value in the KeyMap. */
      prop[ nc - 1 ] = 0;
      astMapPut0C( spprops, "CENTRE", prop, NULL );

/* Scale, format and store the two radii. */
      if( a != AST__BAD && b != AST__BAD && angle != AST__BAD ) {
         GetFmt( "RADIUS1", spprops, 0, defdigs, fmt, status );
         (void) sprintf( buf, fmt, scale*a );
         astMapPut0C( spprops, "RADIUS1", buf, NULL );

         GetFmt( "RADIUS2", spprops, 0, defdigs, fmt, status );
         (void) sprintf( buf, fmt, scale*b );
         astMapPut0C( spprops, "RADIUS2", buf, NULL );

/* Convert the angle to degrees in the direction required by STC-S,
   format and store. */
         angle *= AST__DR2D;
         if( !issky )  angle = 90 - angle;
         while( angle < 0.0 ) angle += 360.0;
         while( angle >= 360.0 ) angle -= 360.0;

         GetFmt( "POSANGLE", spprops, 0, defdigs, fmt, status );
         (void) sprintf( buf, fmt, angle );
         astMapPut0C( spprops, "POSANGLE", buf, NULL );

      } else {
         astAddWarning( this, 1, "The gemeotric parameters of the "
                        "supplied Ellipse are undefined.",
                        "astWrite", status );
         ok = 0;
      }

/* Store properties that are specific to Polygon sub-phrases... */
   } else if( astIsAPolygon( spreg ) ) {
      spaceid = POLYGON_ID;
      astMapPut0C( spprops, "ID", "Polygon", NULL );

/* Get an array holding the axis values at the polygon vertices. */
      astGetRegionPoints( spreg, 0, 0, &np, NULL );
      points = astMalloc( sizeof( double )*np*nspace );
      astGetRegionPoints( spreg, np, nspace, &np, points );

/* Create a string holding the formatted vertex axis values, scaling
   to the required units. Use the Frame's Digits attribute to specify
   how many digits to use when formatting the axis values. */
      GetFmt( "VERTICES", spprops, 0, defdigs, fmt, status );
      nc = 0;
      for( j = 0; j < np; j++ ) {
         p = points + j;
         for( i = 0; i < nspace; i++ ) {
            if( *p != AST__BAD ) {
               (void) sprintf( buf, fmt, scale*(*p) );
               prop = astAppendString( prop, &nc, buf );
               prop = astAppendString( prop, &nc, " " );
               p += np;
            } else {
               astAddWarning( this, 1, "The supplied Polygon contains "
                              "one or more bad axis values.", "astWrite",
                              status );
               ok = 0;
               break;
            }
         }
      }

/* Remove the trailing space, and store the property value in the KeyMap. */
      prop[ nc - 1 ] = 0;
      astMapPut0C( spprops, "VERTICES", prop, NULL );

/* Free resources. */
      points = astFree( points );

/* Store properties that are specific to Position sub-phrases... */
   } else if( astIsAPointList( spreg ) ) {
      spaceid = POSITION_ID;
      astMapPut0C( spprops, "ID", "Position", NULL );

/* Check the PointList contains only a single point. */
      astGetRegionPoints( spreg, 0, 0, &np, NULL );
      if( np > 1 ) {
         astAddWarning( this, 1, "The supplied PointList contains "
                        "more than one position.", "astWrite", status );
         ok = 0;

/* If so, get the axis values at the point. */
      } else {
         astGetRegionPoints( spreg, 1, nspace, &np, centre );

/* Create a string holding the formatted axis values, scaling to the
   required units. Use the Frame's Digits attribute to specify how many
   digits to use when formatting the axis values. */
         nc = 0;
         for( i = 0; i < nspace; i++ ) {
            if( centre[ i ] != AST__BAD ) {
               GetFmt( "POSITION", spprops, i, defdigs, fmt, status );
               (void) sprintf( buf, fmt, scale*centre[ i ] );
               prop = astAppendString( prop, &nc, buf );
               prop = astAppendString( prop, &nc, " " );

            } else {
               astAddWarning( this, 1, "The supplied PointList contains "
                              "one or more bad axis values.", "astWrite",
                              status );
               ok = 0;
               break;
            }
         }

/* Remove the trailing space, and store the property value in the KeyMap. */
         prop[ nc - 1 ] = 0;
         astMapPut0C( spprops, "POSITION", prop, NULL );
      }

/* Store properties that are specific to compound Position sub-phrases... */
   } else {

/* If the Region is not a CmpRegion (e.g. a Prism?) see if simplifying it
   produces a CmpRegion. */
      if( !astIsACmpRegion( spreg ) ) {
         sreg = astSimplify( spreg );
      } else {
         sreg = astClone( spreg );
      }

/* If we now have a CmpRegion, write its properties into a new KeyMap by
   calling this function recursively. Then store the new KeyMap in the
   supplied KeyMap. */
      if( astIsACmpRegion( sreg ) ) {

/* Get the list of Regions that the CmpRegion combines together. This
   also returns the boolean operator with which they are combined. */
         nreg = 0;
         reg_list = NULL;
         oper = astCmpRegionList( (AstCmpRegion *) sreg, &nreg, &reg_list );

/* Store compound region type in the supplied KeyMap. */
         if( oper == AST__AND ) {
            spaceid = INTERSECTION_ID;
            astMapPut0C( spprops, "ID", "Intersection", NULL );
         } else if( oper == AST__OR ) {
            spaceid = UNION_ID;
            astMapPut0C( spprops, "ID", "Union", NULL );
         } else {
            spaceid = DIFFERENCE_ID;
            astMapPut0C( spprops, "ID", "Difference", NULL );
         }

/* Loop round each of the combined Regions. */
         for( i = 0; i < nreg; i++ ) {

/* Create a new KeyMap, and then call this function recursively to store
   the properties of the i'th component Region in the new KeyMap. */
            if( ok ) {
               new_props = astKeyMap( " ", status );
               if( GetRegionProps( this, reg_list[ i ], new_props, nspace,
                                   defdigs, scale, issky, status )
                   == NULL_ID ) ok = 0;

/* Store the new KeyMap in the supplied KeyMap. */
               sprintf( buf, "REGION%d", i + 1 );
               astMapPut0A( spprops, buf, new_props, NULL );

/* Free resources. */
               new_props = astAnnul( new_props );
            }
            reg_list[ i ] = astAnnul( reg_list[ i ] );
         }
         reg_list = astFree( reg_list );
         astMapPut0I( spprops, "NREG", nreg, NULL );

/* All other classes of Region are unsupported. */
      } else {
         astAddWarning( this, 1, "The supplied %s cannot be written "
                        "out since STC-S does not support %s regions.",
                        "astWrite", status, astGetClass( spreg ),
                        astGetClass( spreg ) );
         ok = 0;
      }

/* Free resources. */
      sreg = astAnnul( sreg );
   }

   if( prop ) prop = astFree( prop );

/* If an error has occurred, return NULL_ID. */
   if( !ok || !astOK ) spaceid = NULL_ID;

/* Return the identifier for the STC-S spatial region type. */
   return spaceid;
}

void astInitStcsChanVtab_(  AstStcsChanVtab *vtab, const char *name, int *status ) {
/*
*+
*  Name:
*     astInitStcsChanVtab

*  Purpose:
*     Initialise a virtual function table for an StcsChan.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "stcschan.h"
*     void astInitStcsChanVtab( AstStcsChanVtab *vtab, const char *name )

*  Class Membership:
*     StcsChan vtab initialiser.

*  Description:
*     This function initialises the component of a virtual function
*     table which is used by the StcsChan class.

*  Parameters:
*     vtab
*        Pointer to the virtual function table. The components used by
*        all ancestral classes will be initialised if they have not already
*        been initialised.
*     name
*        Pointer to a constant null-terminated character string which contains
*        the name of the class to which the virtual function table belongs (it
*        is this pointer value that will subsequently be returned by the Object
*        astClass function).
*-
*/

/* Local Variables: */
   astDECLARE_GLOBALS            /* Pointer to thread-specific global data */
   AstObjectVtab *object;        /* Pointer to Object component of Vtab */
   AstChannelVtab *channel;      /* Pointer to Channel component of Vtab */

/* Check the local error status. */
   if ( !astOK ) return;

/* Get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(NULL);

/* Initialize the component of the virtual function table used by the
   parent class. */
   astInitChannelVtab( (AstChannelVtab *) vtab, name );

/* Store a unique "magic" value in the virtual function table. This
   will be used (by astIsAStcsChan) to determine if an object belongs
   to this class.  We can conveniently use the address of the (static)
   class_check variable to generate this unique value. */
   vtab->id.check = &class_check;
   vtab->id.parent = &(((AstChannelVtab *) vtab)->id);

/* Initialise member function pointers. */
/* ------------------------------------ */

   vtab->ClearStcsArea = ClearStcsArea;
   vtab->GetStcsArea = GetStcsArea;
   vtab->SetStcsArea = SetStcsArea;
   vtab->TestStcsArea = TestStcsArea;

   vtab->ClearStcsCoords = ClearStcsCoords;
   vtab->GetStcsCoords = GetStcsCoords;
   vtab->SetStcsCoords = SetStcsCoords;
   vtab->TestStcsCoords = TestStcsCoords;

   vtab->ClearStcsProps = ClearStcsProps;
   vtab->GetStcsProps = GetStcsProps;
   vtab->SetStcsProps = SetStcsProps;
   vtab->TestStcsProps = TestStcsProps;

   vtab->SetStcsLength = SetStcsLength;
   vtab->ClearStcsLength = ClearStcsLength;
   vtab->TestStcsLength = TestStcsLength;
   vtab->GetStcsLength = GetStcsLength;

/* Save the inherited pointers to methods that will be extended, and
   replace them with pointers to the new member functions. */
   object = (AstObjectVtab *) vtab;
   channel = (AstChannelVtab *) vtab;

   parent_clearattrib = object->ClearAttrib;
   object->ClearAttrib = ClearAttrib;
   parent_getattrib = object->GetAttrib;
   object->GetAttrib = GetAttrib;
   parent_setattrib = object->SetAttrib;
   object->SetAttrib = SetAttrib;
   parent_testattrib = object->TestAttrib;
   object->TestAttrib = TestAttrib;

   channel->Write = Write;
   channel->Read = Read;

   parent_getindent = channel->GetIndent;
   channel->GetIndent = GetIndent;

/* Declare the Dump function for this class. There is no destructor or
   copy constructor. */
   astSetDump( vtab, Dump, "StcsChan", "STC-S I/O Channel" );

/* If we have just initialised the vtab for the current class, indicate
   that the vtab is now initialised, and store a pointer to the class
   identifier in the base "object" level of the vtab. */
   if( vtab == &class_vtab ) {
      class_init = 1;
      astSetVtabClassIdentifier( vtab, &(vtab->id) );
   }
}

static AstRegion *MakeSpaceRegion( AstKeyMap *props, AstFrame *frm,
                                   double scale, int *status ){
/*
*  Name:
*     MakeSpaceRegion

*  Purpose:
*     Create a Region to describe the space coverage of the STC-S
*     description being read.

*  Type:
*     Private function.

*  Synopsis:
*     #include "stcschan.h"
*     AstRegion *MakeSpaceRegion( AstKeyMap *props, AstFrame *frm,
*                                 double scale, int *status )

*  Class Membership:
*     StcsChan member function

*  Description:
*     This function returns a pointer to a new Region that describes the
*     spatial coverage of an STC-S description.

*  Parameters:
*     props
*        A KeyMap holding properties read from the STC-S space sub-phrase.
*     frm
*        The Frame in which the Region is to be defined.
*     scale
*        A factor that must be applied to the raw axis values read from the
*        STC-S description in order to convert them into the units used by
*        the supplied Frame.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     The Region pointer.

*/


/* Local Variables: */
   AstKeyMap *reg_props;       /* KeyMap holding argument properties */
   AstRegion *reg;             /* Current argument Region */
   AstRegion *result;          /* Returned Region */
   AstRegion *tmp;             /* Temporary Region pointer */
   char key[ 20 ];             /* Key for argument region */
   const char *id;             /* Sub-phrase identifier */
   double *p;                  /* Pointer to next axis value */
   double *temp;               /* Pointer to array of reordered polygon vertex axis values */
   double *vertices;           /* Pointer to array of polygon vertex axis values */
   double val1;                /* Scalar value read from KeyMap */
   double val2;                /* Scalar value read from KeyMap */
   double val3;                /* Scalar value read from KeyMap */
   double vec1[ 10 ];          /* Vector read from KeyMap */
   double vec2[ 10 ];          /* Vector read from KeyMap */
   int iaxis;                  /* Axis index */
   int ireg;                   /* Index of argument regions */
   int ivert;                  /* Vertex index */
   int naxes;                  /* Number of spatial axes */
   int nreg;                   /* Number of argument regions */
   int nval;                   /* Number of values read from KeyMap */
   int nvert;                  /* Number of vertices */
   int spaceid;                /* Integer identifier for spatial shape */
   int oper;                   /* Boolean operator code for CmpRegion */

/* Initialise */
   result = NULL;

/* Check inherited status */
   if( !astOK ) return result;

/* Temporarily ensure that an error is reported if an attempt is made to
   access a non-existent KeyMap entry. */
   astSetKeyError( props, 1 );

/* Get the space sub-phrase identifier from the properties KeyMap, and
   find the corresponding integer identifier. */

   astMapGet0C( props, "ID", &id );
   spaceid = SpaceId( id, status );

/* Get the number of axes in the Frame. */
   naxes = astGetNaxes( frm );

/* Create a suitable Region to enclose the space positions. This
   includes scaling the supplied axis values to the units used by
   the Frame. */
   if( spaceid == POSITION_INTERVAL_ID ) {
      astMapGet1D( props, "DLOLIMIT", naxes, &nval, vec1 );
      astMapGet1D( props, "DHILIMIT", naxes, &nval, vec2 );

      for( iaxis = 0; iaxis < naxes; iaxis++ ) {
         vec1[ iaxis ] *= scale;
         vec2[ iaxis ] *= scale;
      }

      result = (AstRegion *) astBox( frm, 1, vec1, vec2, NULL, " ", status );

   } else if( spaceid == ALLSKY_ID ) {
      result = (AstRegion *) astNullRegion( frm, NULL, "Negated=1", status );

   } else if( spaceid == CIRCLE_ID ) {
      astMapGet1D( props, "DCENTRE", naxes, &nval, vec1 );
      astMapGet0D( props, "RADIUS", &val1 );
      for( iaxis = 0; iaxis < naxes; iaxis++ ) vec1[ iaxis ] *= scale;
      val1 *= scale;
      result = (AstRegion *) astCircle( frm, 1, vec1, &val1, NULL, " ",
                                        status );

   } else if( spaceid == ELLIPSE_ID ) {
      astMapGet1D( props, "DCENTRE", naxes, &nval, vec1 );
      astMapGet0D( props, "RADIUS1", &val1 );
      astMapGet0D( props, "RADIUS2", &val2 );
      astMapGet0D( props, "POSANGLE", &val3 );
      for( iaxis = 0; iaxis < naxes; iaxis++ ) vec1[ iaxis ] *= scale;
      vec2[ 0 ] = val1*scale;
      vec2[ 1 ] = val2*scale;
      if( !astIsASkyFrame( frm ) ) val3 = 90.0 - val3;
      val3 *= AST__DD2R;
      result = (AstRegion *) astEllipse( frm, 1, vec1, vec2, &val3, NULL, " ",
                                         status );

   } else if( spaceid == BOX_ID ) {
      astMapGet1D( props, "DCENTRE", naxes, &nval, vec1 );
      astMapGet1D( props, "DBSIZE", naxes, &nval, vec2 );

      for( iaxis = 0; iaxis < naxes; iaxis++ ) {
         vec1[ iaxis ] *= scale;
         vec2[ iaxis ] *= scale;
      }

      vertices = BoxCorners( frm, vec1, vec2, status );
      result = (AstRegion *) astPolygon( frm, 4, 4, vertices, NULL, " ",
                                         status );
      vertices = astFree( vertices );

   } else if( spaceid == POLYGON_ID ) {
      nval = astMapLength( props, "DVERTICES" );
      temp = astMalloc( sizeof( double )*nval );
      astMapGet1D( props, "DVERTICES", nval, &nval, temp );

/* An STC-S polygon description holds the vertex axis values in the wrong
   order for the AstPolygon constructor. Therefore, transpose the temp
   array (scale them at the same time). */
      vertices = astMalloc( sizeof( double )*nval );
      if( astOK ) {
         nvert = nval/naxes;
         p = temp;
         for( ivert = 0; ivert < nvert; ivert++ ) {
            for( iaxis = 0; iaxis < naxes; iaxis++,p++ ) {
               vertices[ iaxis*nvert + ivert ] = *p*scale;
            }
         }

         result = (AstRegion *) astPolygon( frm, nvert, nvert, vertices, NULL,
                                            " ", status );
      }

      vertices = astFree( vertices );
      temp = astFree( temp );

   } else if( spaceid == POSITION_ID ) {
      astMapGet1D( props, "DPOSITION", naxes, &nval, vec1 );
      for( iaxis = 0; iaxis < naxes; iaxis++ ) vec1[ iaxis ] *= scale;
      result = (AstRegion *) SinglePointList( frm, vec1, NULL, status );

   } else if( spaceid == CONVEX_ID ) {
      astError( AST__INTER, "astRead(StcsChan): No support for Convex in "
                "MakeSpaceRegion (internal AST programming error).", status );

/* All remaining valid space id values are compound - their arguments are held
   within separate KeyMaps nested inside the supplied KeyMap. */
   } else if( spaceid != NULL_ID ) {

/* The number of arguments is defined in the NREG entry. */
      astMapGet0I( props, "NREG", &nreg );

/* Get the CmpRegion operator code. */
      if( spaceid == UNION_ID ) {
         oper = AST__OR;
      } else if( spaceid == INTERSECTION_ID ) {
         oper = AST__AND;
      } else if( spaceid == DIFFERENCE_ID ) {
         oper = AST__XOR;
      } else {
         oper = 0;  /* To avoid compiler warnings */
      }

/* Loop over all argument Regions. */
      for( ireg = 0; ireg < nreg; ireg++ ) {

/* Get the KeyMap holding the STC-S properties of the current argument
   region. */
         sprintf( key, "REGION%d", ireg + 1 );
         astMapGet0A( props, key, &reg_props );

/* Construct an AST Region from this list of STC-S properties. */
         reg = MakeSpaceRegion( reg_props, frm, scale, status );

/* If we are creating a "Not" element, just negate the argument region
   and return it. */
         if( spaceid == NOT_ID ) {
            astNegate( reg );
            result = astClone( reg );

/* If we are creating a "Union", "Difference" or "Intersection" element,
   combine the first two arguments into a CmpRegion, and then add in each
   subsequent argument. */
         } else {
            if( ireg == 0 ) {
               result = astClone( reg );
            } else {
               tmp = (AstRegion *) astCmpRegion( result, reg, oper, " ",
                                                 status );
               (void) astAnnul( result );
               result = tmp;
            }
         }

/* Free resources */
         reg = astAnnul( reg );
         reg_props = astAnnul( reg_props );
      }
   }

/* Ensure that no error is reported if an attempt is made to access a
   non-existent KeyMap entry. */
   astSetKeyError( props, 0 );

/* Return the Region. */
   return result;
}

static void MapPut0C( AstKeyMap *km, const char *key, const char *value,
                      const char *def, int defs, int *status ){
/*
*  Name:
*     MapPut0C

*  Purpose:
*     Store a text STC-S property in the supplied keymap.

*  Type:
*     Private function.

*  Synopsis:
*     #include "stcschan.h"
*     void MapPut0C( AstKeyMap *km, const char *key, const char *value,
*                    const char *def, int defs, int *status )

*  Class Membership:
*     StcsChan member function.

*  Description:
*     This function stors the supplied value in the given KeyMap,
*     handling default values.

*  Parameters:
*     km
*        Pointer to the KeyMap in which to store the value.
*     key
*        Pointer to a string holding the property name associated with
*        the value.
*     value
*        The property value. If this is NULL then the function
*        returns without action.
*     def
*        The default property value.
*     defs
*        If zero, then the value is not stored in the KeyMap if the value
*        is equal to the default value.
*     status
*        Pointer to the inherited status variable.

*/

/* Check the inherited status */
   if( !astOK ) return;

/* If the value is NULL, ignore the entry. */
   if( value ) {

/* If the value is equal to the default value, and we are NOT storing
   default values, ensure the KeyMap has no entry for the given key. */
      if( astChrMatch( value, def ) && !defs ) {
         astMapRemove( km, key );

/* Otherwise, store the value. */
      } else {
         astMapPut0C( km, key, value, NULL );
      }
   }
}

static void MapPut0D( AstKeyMap *km, const char *key, double value, double def,
                      int defs, int *status ){
/*
*  Name:
*     MapPut0D

*  Purpose:
*     Store a floating point STC-S property in the supplied keymap.

*  Type:
*     Private function.

*  Synopsis:
*     #include "stcschan.h"
*     void MapPut0D( AstKeyMap *km, const char *key, double value, double def,
*                    int defs, int *status )

*  Class Membership:
*     StcsChan member function.

*  Description:
*     This function stors the supplied value in the given KeyMap,
*     handling default values.

*  Parameters:
*     km
*        Pointer to the KeyMap in which to store the value.
*     key
*        Pointer to a string holding the property name associated with
*        the value.
*     value
*        The property value. If this is AST__BAD then the function
*        returns without action.
*     def
*        The default property value.
*     defs
*        If zero, then the value is not stored in the KeyMap if the value
*        is equal to the default value.
*     status
*        Pointer to the inherited status variable.

*/

/* Check the inherited status */
   if( !astOK ) return;

/* If the value is bad, ignore the entry. */
   if( value != AST__BAD ) {

/* If the value is equal to the default value, and we are NOT storing
   default values, ensure the KeyMap has no entry for the given key. */
      if( value == def && !defs ) {
         astMapRemove( km, key );

/* Otherwise, store the value. */
      } else {
         astMapPut0D( km, key, value, NULL );
      }
   }
}

static char *PutRegionProps( AstStcsChan *this, AstKeyMap *km, const char *id,
                             int indent, char *line, int *nc, int *crem,
                             int linelen, int *status ){
/*
*  Name:
*     PutRegionProps

*  Purpose:
*     Append STC-S space sub-phrase properties to the end of a string.

*  Type:
*     Private function.

*  Synopsis:
*     #include "stcschan.h"
*     char *PutRegionProps( AstStcsChan *this, AstKeyMap *km, const char *id,
*                           int indent, char *line, int *nc, int *crem,
*                           int linelen, int *status )

*  Class Membership:
*     StcsChan member function

*  Description:
*     This function converts the STC-S properties for the space sub-phrase
*     supplied in a KeyMap into text, and appends them to the supplied
*     line of text in the order required by STC-S.
*
*     It is assumed that the sub-phrase identifier has already been put
*     into the string.

*  Parameters:
*     this
*        The StcsChan.
*     km
*        Pointer to a KeyMap containing the STC-S properties.
*     id
*        Pointer to the sub-phrase identifier.
*     indent
*        If greater than or equal to zero, then it gives the number of
*        spaces indentation to place before the first word (also indicates
*        that a new-line should follow the last word of the argument). If
*        negative, never use indentation.
*     line
*        Pointer to the buffer to receive the property values.
*     nc
*        Pointer to an int in which to store the number of characaters in
*        the buffer. Updated on exit.
*     crem
*        Pointer to an int in which to store the maximum number of
*        characters before a new line. Ignored if zero. Updated on exit.
*     linelen
*        The maximum number of character per line, or zero if all text is
*        to be included in a single line.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     A pointer to the buffer. This will usually be "line", but may be
*     different to "line" if it was necessary to expand the memory to make
*     room for new properties.

*/

/* Local Variables: */
   AstKeyMap *reg_props;
   char *result;
   char key[ 20 ];
   int i;
   int ireg;
   int nreg;
   int spaceid;

/* Initialise */
   result = line;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Temporarily ensure that an error is reported if an attempt is made to
   access a non-existent KeyMap entry. */
   astSetKeyError( km, 1 );

/* Get the integer code for the space sub-phrase identifier. */
   spaceid = SpaceId( id, status );

/* Do each type of space sub-phrase. */
   if( spaceid == NULL_ID ) {
      astError( AST__INTER, "astWrite(StcsChan): Illegal 'spaceid' value "
                "in function PutRegionProps (internal AST programming "
                "error).", status );

   } else if( spaceid == POSITION_INTERVAL_ID ) {
      result = AddItem( this, km, "LOLIMIT", NULL, result, nc, crem, linelen, status );
      result = AddItem( this, km, "HILIMIT", NULL, result, nc, crem, linelen, status );

   } else if( spaceid == ALLSKY_ID ) {

   } else if( spaceid == CIRCLE_ID ) {
      result = AddItem( this, km, "CENTRE", NULL, result, nc, crem, linelen, status );
      result = AddItem( this, km, "RADIUS", NULL, result, nc, crem, linelen, status );

   } else if( spaceid == ELLIPSE_ID ) {
      result = AddItem( this, km, "CENTRE", NULL, result, nc, crem, linelen, status );
      result = AddItem( this, km, "RADIUS1", NULL, result, nc, crem, linelen, status );
      result = AddItem( this, km, "RADIUS2", NULL, result, nc, crem, linelen, status );
      result = AddItem( this, km, "POSANGLE", NULL, result, nc, crem, linelen, status );

   } else if( spaceid == BOX_ID ) {
      result = AddItem( this, km, "CENTRE", NULL, result, nc, crem, linelen, status );
      result = AddItem( this, km, "BSIZE", NULL, result, nc, crem, linelen, status );

   } else if( spaceid == POLYGON_ID ) {
      result = AddItem( this, km, "VERTICES", NULL, result, nc, crem, linelen, status );

   } else if( spaceid == CONVEX_ID ) {
      astError( AST__INTER, "astWrite(StcsChan): No Convex support yet "
                "(internal AST programming error).", status );

   } else if( spaceid == POSITION_ID ) {
      result = AddItem( this, km, "POSITION", NULL, result, nc, crem, linelen, status );

/* All remaining space id values are compound regions. */
   } else {

/* Append an opening parenthesis. */
      result = astAppendString( result, nc, "( " );

/* If required, write out the text through the Channel sink function,
   and start a new line. */
      if( indent >= 0 ) {
         astPutNextText( this, result );
         *nc = 0;
         *crem = linelen;
      }

/* Set the indentation for the next level down. */
      if( indent == 0 ) {
         indent = 6;
      } else if( indent > 0 ){
         indent += 3;
      }

/* Loop round all argument Regions. */
      astMapGet0I( km, "NREG", &nreg );
      for( ireg = 0; ireg < nreg; ireg++ ) {
         sprintf( key, "REGION%d", ireg + 1 );
         astMapGet0A( km, key, &reg_props );

/* Put any required indentation at the start of the line. */
         if( indent > 0 ) {
            for( i = 0; i < indent; i++ ) {
               result = astAppendString( result, nc, " " );
            }
            *crem -= indent;
         }

/* Append the identifier for the next argument to the string. */
         result = AddItem( this, reg_props, "ID", NULL, result, nc, crem,
                           linelen, status );

/* Append the arguments to the string. */
         astMapGet0C( reg_props, "ID", &id );
         result = PutRegionProps( this, reg_props, id, indent, result, nc,
                                  crem, linelen, status );

/* Write the text out to the sink function, and start a new line. */
         if( indent > 0 ) {
            astPutNextText( this, result );
            *nc = 0;
            *crem = linelen;
         }

/* Free resources. */
         reg_props = astAnnul( reg_props );
      }

/* Decrease any indentation, and then append a closing parenthesis. */
      if( indent > 2 ) {
         indent -= 3;
         for( i = 0; i < indent; i++ ) {
            result = astAppendString( result, nc, " " );
         }
      }
      result = astAppendString( result, nc, ") " );

/* If we are about to return fomr the top-level, start a new line. */
      if( indent > 0 && indent < 6 ) {
         astPutNextText( this, result );
         *nc = 0;
         for( i = 0; i < indent; i++ ) {
            result = astAppendString( result, nc, " " );
         }
         *crem = linelen - indent;
      }
   }

/* Ensure that no error is reported if an attempt is made to access a
   non-existent KeyMap entry. */
   astSetKeyError( km, 0 );

/* Return the buffer pointer. */
   return result;
}

static AstObject *Read( AstChannel *this_channel, int *status ) {
/*
*  Name:
*     Read

*  Purpose:
*     Read an Object from a Channel.

*  Type:
*     Private function.

*  Synopsis:
*     #include "stcschan.h"
*     AstObject *Read( AstChannel *this_channel, int *status )

*  Class Membership:
*     StcsChan member function (over-rides the astRead method
*     inherited from the Channel class).

*  Description:
*     This function reads an Object from an StcsChan.

*  Parameters:
*     this
*        Pointer to the StcsChan.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     A pointer to the new Object.
*/

/* Local Variables: */
   AstFrame *spacefrm;       /* Pointer to SpaceFrame for space sub-phrase */
   AstFrameSet *fs;          /* Temporary FrameSet */
   AstKeyMap *full_props;    /* KeyMap holding all sub-phrase properties */
   AstKeyMap *props;         /* KeyMap holding current sub-phrase properties */
   AstObject *new;           /* Pointer to returned Object */
   AstObject *obj;           /* Pointer to Object extracted from a KeyMap */
   AstPrism *tr;             /* Temporary Region pointer */
   AstRegion *full_co;       /* Region describing full coord position */
   AstRegion *full_enc;      /* Region describing full enclosure */
   AstRegion *red_co;        /* Region describing red-shift coord */
   AstRegion *red_enc;       /* Region describing red-shift enclosure */
   AstRegion *space_co;      /* Region describing space coord */
   AstRegion *space_enc;     /* Region describing space enclosure */
   char **words;             /* Array of pointers to individual words */
   int nword;                /* Number of words returned */
   AstRegion *spec_co;       /* Region describing spectral coord */
   AstRegion *spec_enc;      /* Region describing spectral enclosure */
   AstRegion *time_co;       /* Region describing time coord */
   AstRegion *time_enc;      /* Region describing time enclosure */
   AstSpecFrame *redfrm;     /* Pointer to SpecFrame for redshift sub-phrase */
   AstSpecFrame *specfrm;    /* Pointer to SpecFrame for spectral sub-phrase */
   AstStcsChan *this;        /* Pointer to the StcsChan structure */
   AstStdOfRestType sor;     /* Standard of rest */
   AstSystemType sys;        /* Frame System attribute value */
   AstTimeFrame *tf1;        /* Temporary TimeFrame */
   AstTimeFrame *timefrm;    /* Pointer to TimeFrame for time sub-phrase */
   AstTimeScaleType ts;      /* TimeFrame TimeScale attribute value */
   WordContext con;          /* Context for finding next source word */
   char *fbuf;               /* Pointer to buffer holding document fragment */
   const char *new_ts;       /* Time scale string */
   double epoch;             /* Value to use for the Epoch attribue */
   double fill;              /* Filling factor */
   double hilim;             /* Axis upper limit */
   double lolim;             /* Axis lower limit */
   double scale;             /* Units scaling factor */
   int nval;                 /* No. of values read from KeyMap */
   double vals[ 10 ];        /* Values read from KeyMap */
   double start;             /* Start time */
   double stop;              /* Stop time */
   double time;              /* Time value */
   double time_origin;       /* Value to use as TimeFrame TimeOrigin*/
   double value;             /* Axis value */
   int iaxis;                /* Axis index */
   int is_skyframe;          /* Is the space frame a SkyFrame? */
   int level;                /* Warning reporting level */
   int naxes;                /* No. of space Frame axes */
   int nwant;                /* Number of objects to return */
   int use_co;               /* Do we have a full coordinate position? */
   int use_enc;              /* Do we have a full enclosure? */
   int want_co;              /* Is the Coordinates component wanted? */
   int want_enc;             /* Is the enclosure region wanted? */
   int want_props;           /* Are the STC-S properties wanted? */
   const char *cval;         /* Pointer to property value */
   const char *type;         /* Type of redshift axis */

/* Initialise. */
   new = NULL;

/* Check the global error status. */
   if ( !astOK ) return new;

/* Obtain a pointer to the StcsChan structure. */
   this = (AstStcsChan *) this_channel;

/* Initialise. */
   epoch = AST__BAD;
   start = AST__BAD;
   stop = AST__BAD;
   time = AST__BAD;
   time_co = NULL;
   time_enc = NULL;
   space_co = NULL;
   space_enc = NULL;
   spacefrm = NULL;
   spec_co = NULL;
   spec_enc = NULL;
   red_co = NULL;
   red_enc = NULL;
   use_co = 1;
   use_enc = 0;
   scale = 1.0;

/* Read the STC-S description from the external source, parse it, and
   create a KeyMap containing the parsed property values. */
   full_props = ReadProps( this, status );

/* If the STC-S description contained a time sub-phrase, get the KeyMap
   containing the proprties of the time sub-phrase, and then create AST
   Regions describing the time coordinate value and its enclosing Region. */
   if( astMapGet0A( full_props, "TIME_PROPS", &obj ) ) {
      props = (AstKeyMap *) obj;

/* Create the default TimeFrame */
      timefrm = astTimeFrame( " ", status );

/* Get the TIMESCALE property from the KeyMap, and identify the corresponding
   AST TimeScale. */
      ts = AST__BADTS;
      new_ts = NULL;
      level = 3;

      if( astMapGet0C( props, "TIMESCALE", &cval ) ) {

         if( astChrMatch( cval, "TT" ) ) {
            ts = AST__TT;

         } else if( astChrMatch( cval, "TDT" ) ) {
            ts = AST__TT;
            new_ts = "TT";

         } else if( astChrMatch( cval, "ET" ) ) {
            ts = AST__TT;
            new_ts = "TT";

         } else if( astChrMatch( cval, "TAI" ) ) {
            ts = AST__TAI;

         } else if( astChrMatch( cval, "IAT" ) ) {
            ts = AST__TAI;
            new_ts = "TAI";

         } else if( astChrMatch( cval, "UTC" ) ) {
            ts = AST__UTC;

         } else if( astChrMatch( cval, "TEB" ) ) {
            ts = AST__TDB;
            new_ts = "TDB";
            level = 1;

         } else if( astChrMatch( cval, "TDB" ) ) {
            ts = AST__TDB;

         } else if( astChrMatch( cval, "TCG" ) ) {
            ts = AST__TCG;

         } else if( astChrMatch( cval, "TCB" ) ) {
            ts = AST__TCB;

         } else if( astChrMatch( cval, "LST" ) ) {
            ts = AST__LMST;

         } else if( astChrMatch( cval, "nil" ) ) {
            astAddWarning( this, 2, "Time scale defaulting to 'TAI'.",
                           "astRead", status );

         } else if( astOK ){
            astError( AST__BADIN, "astRead(StcsChan): Unknown time scale '%s'.",
                      status, cval );
         }

      } else {
         astAddWarning( this, 2, "Time scale defaulting to 'TAI'.",
                        "astRead", status );
      }

/* Issue a warning if a different time-scale was substituted for the supplied
   time-scale. */
      if( new_ts ) {
         astAddWarning( this, level, "AST does not support the '%s' time "
                        "scale. The '%s' timescale is being used instead.",
                        "astRead", status, cval, new_ts );
      }

/* If we got a time scale, set the TimeScale attribute in the TimeFrame
   to the same value. */
      if( ts != AST__BADTS ) astSetTimeScale( timefrm, ts );

/* The AST TimeFrame class has no reference position, so allow any reference
   position but issue a warning for anything other than "TOPOCENTER" and
   "UNKNOWNRefPos". */
      if( !astMapGet0C( props, "REFPOS", &cval ) ) cval = "UNKNOWNRefPos";
      if( !astChrMatch( cval, "TOPOCENTER" ) ) {
         astAddWarning( this, 1, "AST only supports topocentric time frames, "
                        "so 'TOPOCENTER' will be used in place of '%s'.",
                        "astRead", status, cval );
      }

/* Get the times describes by the time sub-phrase as MJD values. */
      astMapGet0D( props, "MJDSTART", &start );
      astMapGet0D( props, "MJDTIME", &time );
      astMapGet0D( props, "MJDSTOP", &stop );

/* Get the earliest time represented by the time sub-phrase. We use this
   as the TimeOrigin for the TimeFrame, and also as the Epoch for all
   frames. */
      time_origin = start;
      if( time_origin == AST__BAD ) time_origin = time;
      if( time_origin == AST__BAD ) time_origin = stop;
      epoch = time_origin;

/* Store the TimeOrigin value in the TimeFrame, modifying the time values
   accordingly. */
      if( time_origin != AST__BAD ) {
         astSetTimeOrigin( timefrm, time_origin );
         if( start != AST__BAD ) start -= time_origin;
         if( stop != AST__BAD ) stop -= time_origin;
         if( time != AST__BAD ) time -= time_origin;
      }

/* Convert the epoch to TDB. */
      if( epoch != AST__BAD && ts != AST__TDB ) {
         tf1 = astCopy( timefrm );
         astSetTimeScale( tf1, AST__TDB );
         fs = astConvert( timefrm, tf1, "" );
         astTran1( fs, 1, &epoch, 1, &epoch );
         fs = astAnnul( fs );
         tf1 = astAnnul( tf1 );
      }

/* Store the epoch value in the TimeFrame. */
      if( epoch != AST__BAD ) astSetEpoch( timefrm, epoch );

/* Create a suitable Region to describe the enclosure for the time coords */
      if( start != AST__BAD || stop != AST__BAD ) {
         time_enc = (AstRegion *) astInterval( timefrm, &start, &stop,
                                               NULL, "", status );
         use_enc = 1;
      }

/* Create a suitable Region to describe the time coords contained within
   the above enclosure. */
      if( time != AST__BAD ) {
         time_co = (AstRegion *) SinglePointList( (AstFrame *) timefrm,
                                                   &time, NULL, status);
      } else {
         use_co = 0;
      }

/* If no enclosure Region was created for the time sub-phrase, use a
   copy of any coordinate region. This is because each sub-phrase needs
   to have an enclosure of some sort if they are to be combined in parallel
   into an enclose for the whole CmpFrame. */
      if( ! time_enc && time_co ) time_enc = astCopy( time_co );

/* Set the filling factor. */
      if( time_enc && astMapGet0D( props, "FILLFACTOR", &fill ) ) {
         astSetFillFactor( time_enc, fill );
      }

/* Get the units in which the time error values are given, and get the
   scaling factor that converts them into days. */
      if( astMapGet0C( props, "UNIT", &cval ) ) {
         if( !strcmp( cval, "s" ) ) {
            scale = 1.0/86400.0;

         } else if( !strcmp( cval, "d" ) ) {
            scale = 1.0;

         } else if( !strcmp( cval, "a" ) ) {
            scale = 365.25;

         } else if( !strcmp( cval, "yr" ) ) {
            scale = 365.25;

         } else if( !strcmp( cval, "cy" ) ) {
            scale = 36525.0;

         } else if( astOK ) {
            astError( AST__BADIN, "astRead(StcsChan): Unsupported "
                      "units (%s) for the time axis within an "
                      "STC-S description.", status, cval );
         }

      } else {
         scale = 1.0/86400.0;
      }

/* Associate an uncertainty with the two Regions. */
      if( astMapGet1D( props, "DERROR", 2, &nval, vals ) ) {
         if( nval > 1 ) {
            astAddWarning( this, 1, "An STC-S time sub-phrase contains an "
                           "Error range. AST does not support error ranges "
                           "so the mid value will be used as the error.",
                           "astRead", status );
            vals[ 0 ] = 0.5*( vals[ 0 ] + vals[ 1 ] );
         }

         SetUnc( time_enc, time_co, (AstFrame *) timefrm, 0, scale, vals, 1,
                 status );
      }

/* Free resources */
      props = astAnnul( props );
      timefrm = astAnnul( timefrm );
   }

/* If the STC-S description contained a space sub-phrase, get the KeyMap
   containing the proprties of the space sub-phrase, and then create AST
   Regions describing the spatial position and its enclosing Region. */
   if( astMapGet0A( full_props, "SPACE_PROPS", &obj ) ) {
      props = (AstKeyMap *) obj;

/* The class of Frame (SkyFrame or basic Frame) is determined by the
   "FLAVOR". */
      is_skyframe = 0;
      if( astMapGet0C( props, "FLAVOUR", &cval ) ) {

         if( astChrMatch( cval, "SPHER2" ) ) {
            spacefrm = (AstFrame *) astSkyFrame( "", status );
            is_skyframe = 1;

         } else if( astChrMatch( cval, "CART1" ) ) {
            spacefrm = astFrame( 1, "", status );

         } else if( astChrMatch( cval, "CART2" ) ) {
            spacefrm = astFrame( 2, "", status );

         } else if( astChrMatch( cval, "CART3" ) ) {
            spacefrm = astFrame( 3, "", status );

         } else {
            astError( AST__BADIN, "astRead(StcsChan): Unsupported "
                      "space 'Flavor' (%s) found in STC-S description.",
                      status, cval );
         }

      } else {
         spacefrm = (AstFrame *) astSkyFrame( "", status );
         is_skyframe = 1;
      }

/* Consider each supported space frame. Report an error for frames
   not supported by AST. */
      if( astMapGet0C( props, "FRAME", &cval ) ) {
         if( astChrMatch( cval, "ICRS" ) ) {
            sys = AST__ICRS;

         } else if( astChrMatch( cval, "FK5" ) ) {
            sys = AST__FK5;

         } else if( astChrMatch( cval, "FK4" ) ) {
            sys = AST__FK4;

         } else if( astChrMatch( cval, "J2000" ) ) {
            sys = AST__FK5;

         } else if( astChrMatch( cval, "B1950" ) ) {
            sys = AST__FK4;

         } else if( astChrMatch( cval, "ECLIPTIC" ) ) {
            sys = AST__ECLIPTIC;

         } else if( astChrMatch( cval, "GALACTIC" ) ) {
            sys = AST__GALACTIC;

         } else if( astChrMatch( cval, "GALACTIC_II" ) ) {
            sys = AST__GALACTIC;

         } else if( astChrMatch( cval, "SUPER_GALACTIC" ) ) {
            sys = AST__SUPERGALACTIC;

         } else if( astChrMatch( cval, "UNKNOWNFrame" ) ) {
            sys = AST__UNKNOWN;

         } else {
            sys = AST__UNKNOWN;
            astAddWarning( this, 1, "'UNKNOWNFrame' being used in place of "
                           "unsupported frame '%s' in an STC-S description.",
                           "astRead", status, cval );
         }

      } else {
         cval = "UNKNOWNFrame";
         sys = AST__UNKNOWN;
         astAddWarning( this, 1, "Space frame defaulting to 'UNKNOWNFrame' "
                        "in an STC-S description.", "astRead", status );
      }

/* We can set the System (only needed for SkyFrames). */
      if( is_skyframe ) {
         astSetSystem( spacefrm, sys );

/* If we have a basic Frame, set the Domain equal to the STC-S frame value. */
      } else {
         astSetDomain( spacefrm, cval );
      }

/* Set the epoch of the space frame. */
      if( epoch != AST__BAD ) astSetEpoch( spacefrm, epoch );

/* The AST Frame and SkyFrame class has no reference position, so for
   SkyFrames we consider "TOPOCENTER" and "UNKNOWN" acceptable and all
   other unsupported. For other Frames we allow any reference position. */
      if( !astMapGet0C( props, "REFPOS", &cval ) ) cval = "UNKNOWNRefPos";
      if( is_skyframe && !astChrMatch( cval, "TOPOCENTER" ) ) {
         astAddWarning( this, 1, "AST only supports topocentric sky frames, "
                        "so 'TOPOCENTER' will be used in place of '%s'.",
                        "astRead", status, cval );
      }

/* Get the number of spatial axes. */
      naxes = astGetNaxes( spacefrm );

/* Get the units strings. */
      if( !astMapGet0C( props, "UNIT", &cval ) ) {
         if( is_skyframe ) {
            cval = "deg";
         } else {
            cval = "m";
         }
      }

/* In AST, SkyFrames always use radians, so set up a scaling factor to
   convert supplied axis values into radians. */
      if( is_skyframe ) {

         if( !strcmp( cval, "deg" ) || !strcmp( cval, "deg deg" ) ) {
            scale = AST__DD2R;

         } else if( !strcmp( cval, "arcmin" ) || !strcmp( cval, "arcmin arcmin" ) ) {
            scale = AST__DD2R/60.0;

         } else if( !strcmp( cval, "arcsec" ) || !strcmp( cval, "arcsec arcsec" ) ) {
            scale = AST__DD2R/3600.0;

         } else if( astOK ) {
            astError( AST__BADIN, "astRead(StcsChan): Unsupported "
                      "units (%s) for a spherical co-ordinate system "
                      "within an STC-S description: '%s'.", status,
                      cval, ContextFragment( &con, &fbuf, status ) );
         }

/* Basic Frames can use any of the allowed units, so use a scale factor of
   1.0. Also set the active unit flag in the space frame to enable intelligent
   units conversion by astConvert etc. */
      } else {
         scale = 1.0;
         astSetActiveUnit( spacefrm, 1 );

/* Basic Frames can have different units on different axes. So split the
   units property up into separate words. */
         words = astChrSplit( cval, &nword );

/* Set values for the Unit attributes of the Frame. Replicate the last
   supplied unit string for any extra axes. */
         for( iaxis = 0; iaxis < naxes; iaxis++ ) {
            if( iaxis < nword ) {
               astSetUnit( spacefrm, iaxis, words[ iaxis ] );
            } else {
               astSetUnit( spacefrm, iaxis, words[ nword - 1 ] );
            }
         }

/* Free resources. */
         for( iaxis = 0; iaxis < nword; iaxis++ ) {
            words[ iaxis ] = astFree( words[ iaxis ] );
         }
         words = astFree( words );
      }

/* Create a suitable Region to enclose the space positions. This
   includes scaling the supplied axis values to the units used by
   the Frame. */
      space_enc = MakeSpaceRegion( props, spacefrm, scale, status );
      if( space_enc ) use_enc = 1;

/* Create a suitable Region to describe the space coords contained within
   the above enclosure. If any sub-phrase has no coordinate value, then
   we cannot produce a PointList describing the complete coordinate set. */
      if( astMapGet1D( props, "DPOSITION", naxes, &nval, vals ) ) {
         for( iaxis = 0; iaxis < nval; iaxis++ ) vals[ iaxis ] *= scale;
         space_co = (AstRegion *) SinglePointList( spacefrm, vals, NULL,
                                                   status);
      } else {
         use_co = 0;
      }

/* If no enclosure Region was created for the space sub-phrase, use a
   copy of any coordinate region. This is because each sub-phrase needs
   to have an enclosure of some sort if they are to be combined in parallel
   into an enclose for the whole CmpFrame. */
      if( ! space_enc && space_co ) space_enc = astCopy( space_co );

/* Set the filling factor. */
      if( space_enc && astMapGet0D( props, "FILLFACTOR", &fill ) ) {
         astSetFillFactor( space_enc, fill );
      }

/* Associate an uncertainty with the two Regions. */
      if( astMapGet1D( props, "DERROR", 2*naxes, &nval, vals ) ) {
         if( nval > naxes ) {
            astAddWarning( this, 1, "An STC-S space sub-phrase contains an "
                           "Error range. AST does not support error ranges "
                           "so the mid value will be used as the error.",
                           "astRead", status );
            for( iaxis = 0; iaxis < naxes; iaxis++ ) {
               vals[ iaxis ] = 0.5*( vals[ iaxis ] + vals[ iaxis + naxes ] );
            }

/* If insufficient error values have been supplied, replicate the last
   one. */
         } else {
            for( iaxis = nval; iaxis < naxes; iaxis++ ) {
               vals[ iaxis ] = vals[ nval - 1 ];
            }
         }

/* Set the uncertainty in the two space regions. */
         SetUnc( space_enc, space_co, (AstFrame *) spacefrm, is_skyframe,
                 scale, vals, naxes, status );
      }

/* Free resources */
      props = astAnnul( props );
      spacefrm = astAnnul( spacefrm );
   }



/* If the STC-S description contained a velocity sub-phrase, issue a
   warning. */
   if( astMapGet0A( full_props, "VELOCITY_PROPS", &obj ) ) {
      astAddWarning( this, 1, "Ignoring a velocity sub-phrase found in "
                           "an STC-S description.", "astRead", status );
      obj = astAnnul( obj );
   }


/* If the STC-S description contained a spectral sub-phrase, get the KeyMap
   containing the proprties of the spectral sub-phrase, and then create AST
   Regions describing the spectral coordinate value and its enclosing Region. */
   if( astMapGet0A( full_props, "SPECTRAL_PROPS", &obj ) ) {
      props = (AstKeyMap *) obj;

/* Create the default SpecFrame */
      specfrm = astSpecFrame( " ", status );

/* Get the REFPOS property from the KeyMap, and identify the corresponding
   AST StdOfRest. */
      sor = AST__BADSOR;
      if( astMapGet0C( props, "REFPOS", &cval ) ) {

         if( astChrMatch( cval, "GEOCENTER" ) ) {
            sor = AST__GESOR;

         } else if( astChrMatch( cval, "BARYCENTER" ) ) {
            sor = AST__BYSOR;

         } else if( astChrMatch( cval, "HELIOCENTER" ) ) {
            sor = AST__HLSOR;

         } else if( astChrMatch( cval, "TOPOCENTER" ) ) {
            sor = AST__TPSOR;

         } else if( astChrMatch( cval, "LSR" ) ||
                    astChrMatch( cval, "LSRK" ) ) {
            sor = AST__LKSOR;

         } else if( astChrMatch( cval, "LSRD" ) ) {
            sor = AST__LDSOR;

         } else if( astChrMatch( cval, "GALACTIC_CENTER" ) ) {
            sor = AST__GLSOR;

         } else {
            astAddWarning( this, 1, "Using 'HELIOCENTER' in place of "
                           "unsupported spectral reference position '%s' "
                           "found in an STC-S description.", "astRead",
                           status, cval );
         }

      } else {
         astAddWarning( this, 2, "Spectral reference position defaulting to "
                        "'HELIOCENTER' in an STC-S description.", "astRead",
                        status );
      }

/* If we got a ref pos, set the StdOfRest attribute in the SpecFrame. */
      if( sor != AST__BADSOR ) astSetStdOfRest( specfrm, sor );

/* Get the units. */
      if( !astMapGet0C( props, "UNIT", &cval ) ) cval = "Hz";


/* Set the spectral system implied by the unit string. */
      if( !cval || !strcmp( cval, "Hz" ) || !strcmp( cval, "MHz" ) ||
          !strcmp( cval, "GHz" ) ) {
         astSetSystem( specfrm, AST__FREQ );

      } else if( !strcmp( cval, "m" ) || !strcmp( cval, "mm" ) ||
                 !strcmp( cval, "um" ) || !strcmp( cval, "nm" ) ||
                 !strcmp( cval, "Angstrom" ) ) {
         astSetSystem( specfrm, AST__WAVELEN );

      } else if( !strcmp( cval, "eV" ) || !strcmp( cval, "keV" ) ||
                 !strcmp( cval, "MeV" ) ) {
         astSetSystem( specfrm, AST__ENERGY );

      } else if( astOK ) {
         astError( AST__BADIN, "astRead(StcsChan): Unsupported spectral "
                   "units (%s) found within an STC-S description.",
                   status, cval );
      }

/* Set the units. */
      astSetUnit( specfrm, 0, cval );

/* Set the epoch */
      if( epoch != AST__BAD ) astSetEpoch( specfrm, epoch );

/* Create a suitable Region to describe the enclosure for the spectral
   coords */
      if( astMapGet0D( props, "LOLIMIT", &lolim ) ) {
         astMapGet0D( props, "HILIMIT", &hilim );
         spec_enc = (AstRegion *) astInterval( specfrm, &lolim, &hilim,
                                               NULL, "", status );
         use_enc = 1;
      }

/* Create a suitable Region to describe the spectral coords contained within
   the above enclosure. If any sub-phrase has no coordinate value, then
   we cannot produce a PointList describing the complete coordinate set. */
      if( astMapGet0D( props, "SPECTRAL", &value ) ) {
         spec_co = (AstRegion *) SinglePointList( (AstFrame *) specfrm,
                                                   &value, NULL, status);
      } else {
         use_co = 0;
      }

/* If no enclosure Region was created for the spectral sub-phrase, use a
   copy of any coordinate region. This is because each sub-phrase needs
   to have an enclosure of some sort if they are to be combined in parallel
   into an enclose for the whole CmpFrame. */
      if( ! spec_enc && spec_co ) spec_enc = astCopy( spec_co );

/* Set the filling factor. */
      if( spec_enc && astMapGet0D( props, "FILLFACTOR", &fill ) ) {
         astSetFillFactor( spec_enc, fill );
      }


/* Associate an uncertainty with the two Regions. */
      if( astMapGet1D( props, "DERROR", 2, &nval, vals ) ) {
         if( nval > 1 ) {
            astAddWarning( this, 1, "An STC-S spectral sub-phrase contains an "
                           "Error range. AST does not support error ranges "
                           "so the mid value will be used as the error.",
                           "astRead", status );
            vals[ 0 ] = 0.5*( vals[ 0 ] + vals[ 1 ] );
         }

         SetUnc( spec_enc, spec_co, (AstFrame *) specfrm, 0, 1.0, vals, 1,
                 status );
      }

/* Free resources */
      props = astAnnul( props );
      specfrm = astAnnul( specfrm );
   }




/* If the STC-S description contained a redshift sub-phrase, get the KeyMap
   containing the properties of the redshift sub-phrase, and then create AST
   Regions describing the redshift coordinate value and its enclosing Region. */
   if( astMapGet0A( full_props, "REDSHIFT_PROPS", &obj ) ) {
      props = (AstKeyMap *) obj;

/* Create the default SpecFrame */
      redfrm = astSpecFrame( "Domain=REDSHIFT", status );

/* Get the REFPOS property from the KeyMap, and identify the corresponding
   AST StdOfRest. */
      sor = AST__BADSOR;
      if( astMapGet0C( props, "REFPOS", &cval ) ) {

         if( astChrMatch( cval, "GEOCENTER" ) ) {
            sor = AST__GESOR;

         } else if( astChrMatch( cval, "BARYCENTER" ) ) {
            sor = AST__BYSOR;

         } else if( astChrMatch( cval, "HELIOCENTER" ) ) {
            sor = AST__HLSOR;

         } else if( astChrMatch( cval, "TOPOCENTER" ) ) {
            sor = AST__TPSOR;

         } else if( astChrMatch( cval, "LSR" ) ||
                    astChrMatch( cval, "LSRK" ) ) {
            sor = AST__LKSOR;

         } else if( astChrMatch( cval, "LSRD" ) ) {
            sor = AST__LDSOR;

         } else if( astChrMatch( cval, "GALACTIC_CENTER" ) ) {
            sor = AST__GLSOR;

         } else {
            astAddWarning( this, 1, "Using 'HELIOCENTER' in place of "
                           "unsupported redshift reference position '%s' "
                           "found in an STC-S description.", "astRead",
                           status, cval );
         }

      } else {
         astAddWarning( this, 2, "Redshift reference position defaulting to "
                        "'HELIOCENTER' in an STC-S description.", "astRead",
                        status );
      }

/* If we got a ref pos, set the StdOfRest attribute in the SpecFrame. */
      if( sor != AST__BADSOR ) astSetStdOfRest( redfrm, sor );

/* Get the redshift type. */
      if( !astMapGet0C( props, "TYPE", &type ) ) type = "REDSHIFT";

/* Now get the velocity definition, and set the equivalent SpecFrame
   System value. AST only supports optical redshift, so report an error
   or a warning for unsupported combinations. */
      if( astMapGet0C( props, "DOPPLERDEF", &cval ) ){

         if( astChrMatch( cval, "OPTICAL" ) ) {
            if( astChrMatch( type, "VELOCITY" ) ){
               astSetSystem( redfrm, AST__VOPTICAL );
            } else {
               astSetSystem( redfrm, AST__REDSHIFT );
            }

         } else if( astChrMatch( cval, "RADIO" ) ) {
            if( astChrMatch( type, "VELOCITY" ) ){
               astSetSystem( redfrm, AST__VRADIO );
            } else {
               astSetSystem( redfrm, AST__REDSHIFT );
               astAddWarning( this, 1, "STC-S RADIO redshift not supported. "
                              "Assuming OPTICAL redshift instead.", "astRead",
                              status );
            }

         } else if( astChrMatch( cval, "RELATIVISTIC" ) ) {
            if( astChrMatch( type, "VELOCITY" ) ){
               astSetSystem( redfrm, AST__VREL );
            } else {
               astSetSystem( redfrm, AST__REDSHIFT );
               astAddWarning( this, 1, "STC-S RELATIVISTIC redshift not supported. "
                              "Assuming OPTICAL redshift instead.", "astRead",
                              status );
            }

         } else {
            if( astChrMatch( type, "VELOCITY" ) ){
               astSetSystem( redfrm, AST__VOPTICAL );
               astAddWarning( this, 1, "Doppler velocity definition defaulting"
                              " to 'OPTICAL' in an STC-S description.",
                              "astRead", status );

            } else {
               astSetSystem( redfrm, AST__REDSHIFT );
            }
         }
      }

/* Set the units. */
      if( astChrMatch( type, "VELOCITY" ) ){
         if( astMapGet0C( props, "UNIT", &cval ) ) {
            astSetUnit( redfrm, 0, cval );
         } else {
            astSetUnit( redfrm, 0, "km/s" );
         }

      } else if( astMapGet0C( props, "UNIT", &cval ) ) {
         astAddWarning( this, 1, "Ignoring units (%s) specified for REDSHIFT "
                        "in an STC-S description.", "astRead", status, cval );
      }

/* Set the epoch */
      if( epoch != AST__BAD ) astSetEpoch( redfrm, epoch );

/* Create a suitable Region to describe the enclosure for the redshift
   coords */
      if( astMapGet0D( props, "LOLIMIT", &lolim ) ) {
         astMapGet0D( props, "HILIMIT", &hilim );
         red_enc = (AstRegion *) astInterval( redfrm, &lolim, &hilim,
                                               NULL, "", status );
         use_enc = 1;
      }

/* Create a suitable Region to describe the redshift coords contained within
   the above enclosure. If any sub-phrase has no coordinate value, then
   we cannot produce a PointList describing the complete coordinate set. */
      if( astMapGet0D( props, "REDSHIFT", &value ) ) {
         red_co = (AstRegion *) SinglePointList( (AstFrame *) redfrm,
                                                   &value, NULL, status);
      } else {
         use_co = 0;
      }

/* If no enclosure Region was created for the redshift sub-phrase, use a
   copy of any coordinate region. This is because each sub-phrase needs
   to have an enclosure of some sort if they are to be combined in parallel
   into an enclose for the whole CmpFrame. */
      if( ! red_enc && red_co ) red_enc = astCopy( red_co );

/* Set the filling factor. */
      if( red_enc && astMapGet0D( props, "FILLFACTOR", &fill ) ) {
         astSetFillFactor( red_enc, fill );
      }

/* Associate an uncertainty with the two Regions. */
      if( astMapGet1D( props, "DERROR", 2, &nval, vals ) ) {
         if( nval > 1 ) {
            astAddWarning( this, 1, "An STC-S redshift sub-phrase contains an "
                           "Error range. AST does not support error ranges "
                           "so the mid value will be used as the error.",
                           "astRead", status );
            vals[ 0 ] = 0.5*( vals[ 0 ] + vals[ 1 ] );
         }

         SetUnc( red_enc, red_co, (AstFrame *) redfrm, 0, 1.0, vals, 1,
                 status );
      }

/* Free resources */
      props = astAnnul( props );
      redfrm = astAnnul( redfrm );
   }

/* If a particular position was specified by the STC_S document, create the
   full position from the individual sub-phrase position */
   if( use_co ) {
      new = time_co ? astClone( time_co ) : NULL;

      if( space_co ) {
         if( new ) {
            tr = astPrism( new, space_co, "", status );
            (void) astAnnul( new );
            new = (AstObject *) tr;
         } else {
            new = astClone( space_co );
         }
      }

      if( spec_co ) {
         if( new ) {
            tr = astPrism( new, spec_co, "", status );
            (void) astAnnul( new );
            new = (AstObject *) tr;
         } else {
            new = astClone( spec_co );
         }
      }

      if( red_co ) {
         if( new ) {
            tr = astPrism( new, red_co, "", status );
            (void) astAnnul( new );
            new = (AstObject *) tr;
         } else {
            new = astClone( red_co );
         }
      }

      if( new ) {
         full_co = astSimplify( new );
         new = astAnnul( new );
      } else {
         full_co = NULL;
      }

   } else {
      full_co = NULL;
   }

/* If an enclosing volume was specified by the STC_S document, create the
   full enclosure Region from the individual sub-phrase enclosure Regions. */
   if( use_enc ) {
      new = time_enc ? astClone( time_enc ) : NULL;

      if( space_enc ) {
         if( new ) {
            tr = astPrism( new, space_enc, "", status );
            (void) astAnnul( new );
            new = (AstObject *) tr;
         } else {
            new = astClone( space_enc );
         }
      }

      if( spec_enc ) {
         if( new ) {
            tr = astPrism( new, spec_enc, "", status );
            (void) astAnnul( new );
            new = (AstObject *) tr;
         } else {
            new = astClone( spec_enc );
         }
      }

      if( red_enc ) {
         if( new ) {
            tr = astPrism( new, red_enc, "", status );
            (void) astAnnul( new );
            new = (AstObject *) tr;
         } else {
            new = astClone( red_enc );
         }
      }
      full_enc = astSimplify( new );
      new = astAnnul( new );

   } else {
      full_enc = NULL;
   }

/* See which, and how many, items are to be returned. */
   nwant = 0;
   if( ( want_enc = astGetStcsArea( this ) ) ) nwant++;
   if( ( want_co = astGetStcsCoords( this ) ) ) nwant++;
   if( ( want_props = astGetStcsProps( this ) ) ) nwant++;

/* If one, and only one, of the three items is to be returned, return it. */
   new = NULL;
   if( nwant == 1 ) {
      if( want_enc && full_enc ) {
         new = astClone( full_enc );
      } else if( want_co && full_co ) {
         new = astClone( full_co );
      } else if( want_props && full_props ){
         new = astClone( full_props );
      }

/* If more than one item is to be returned, put them into a KeyMap and
   return the KeyMap. */
   } else if( nwant > 1 ) {
      new = (AstObject *) astKeyMap( " ", status );
      if( want_enc && full_enc ) astMapPut0A( new, "AREA", full_enc, NULL );
      if( want_co && full_co ) astMapPut0A( new, "COORDS", full_co, NULL );
      if( want_props && full_props ) astMapPut0A( new, "PROPS", full_props, NULL );

/* Report an error if nothing is to be returned. */
   } else if( astOK ){
      astError( AST__ATTIN, "astRead(StcsChan): The StcsArea, StcsCoords "
                "and StcsProps attributes indicate that nothing is to be "
                "returned (possible programming error).", status );
   }

/* Free resources */
   if( space_enc ) space_enc = astAnnul( space_enc );
   if( spec_enc ) spec_enc = astAnnul( spec_enc );
   if( time_enc ) time_enc = astAnnul( time_enc );
   if( red_enc ) red_enc = astAnnul( red_enc );
   if( space_co ) space_co = astAnnul( space_co );
   if( spec_co ) spec_co = astAnnul( spec_co );
   if( time_co ) time_co = astAnnul( time_co );
   if( red_co ) red_co = astAnnul( red_co );
   if( full_enc ) full_enc = astAnnul( full_enc );
   if( full_co ) full_co = astAnnul( full_co );
   if( full_props ) full_props = astAnnul( full_props );

/* If an error occurred, clean up by deleting the new Object and
   return a NULL pointer. */
   if ( !astOK ) new = astDelete( new );

/* Return the pointer to the new Object. */
   return new;
}

static AstKeyMap *ReadProps( AstStcsChan *this, int *status ) {
/*
*  Name:
*     ReadProps

*  Purpose:
*     Read STC-S properties from the source and store in a KeyMap.

*  Type:
*     Private function.

*  Synopsis:
*     #include "stcschan.h"
*     AstKeyMap *ReadProps( AstStcsChan *this, int *status )

*  Class Membership:
*     StcsChan member function

*  Description:
*     This function parses the list of space-separated words read from the
*     source function, identifies the purpose of each word within the STC-S
*     description, and stores the words in a returned KeyMap.

*  Parameters:
*     this
*        Pointer to the StcsChan.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     A pointer to the new KeyMap. This will contain up to five entries
*     with any or all of the following keys: TIME_PROPS, SPACE_PROPS,
*     VELOCITY_PROPS, SPECTRAL_PROPS, REDSHIFT_PROPS. If an entry is absent,
*     it means the STC-S description did not contain the corresponding
*     sub-phrase. The value associated with each of these entries will be a
*     KeyMap. These will contain values for the sub-phrase proprties read
*     from the STC-S description. Properties that are not specified in
*     the STC-S description will not be present in the KeyMap. The values
*     stored in the KeyMap are the words read form the STC-S description
*     without any conversion or other processing.
*/

/* Local Constants: */
#define MAXVAL 6

/* Local Variables: */
   AstKeyMap *props;         /* KeyMap holding current sub-phrase properties */
   AstKeyMap *result;        /* Returned KeyMap holding all properties */
   AstTimeFrame *timefrm;    /* Used for unformatting ISO date-times */
   WordContext con;          /* Context for finding next source word */
   char *fbuf;               /* Pointer to buffer holding document fragment */
   char *prop;               /* String holding complete property value */
   const char *subphrase;    /* Name of current sub phrase */
   const char *t;            /* Temporary character string pointer */
   const char *word;         /* Pointer to next source word */
   double val[ MAXVAL ];     /* Array of numerical property values */
   double start;             /* Start time (MJD) */
   double stop;              /* Stop time (MJD) */
   double time;              /* Time value (MJD) */
   double value;             /* Axis value */
   int iaxis;                /* Axis index */
   int is_jd;                /* Is time value a JD rather than an MJD? */
   int nunit;                /* Number of units strings supplied */
   int nval;                 /* Number of numerical values read */
   int naxes;                /* No. of space Frame axes */
   int nc;                   /* Number of characters written to string */
   int new_word;             /* Get a new word at the end of the pass? */
   int redid;                /* Redshift sub-phrase component identifier */
   int spaceid;              /* Space sub-phrase component identifier */
   int specid;               /* Spectral sub-phrase component identifier */
   int timeid;               /* Time sub-phrase component identifier */
   int velid;                /* Velocity sub-phrase component identifier */

/* The stage reached in the parsing of the STC-S description is indicated
   by the "look_for" variable. This variable is allowed the following
   values, indicating the item that is to be checked for next. */
   enum look_for_type {
      ERROR,
      FILL_FACTOR,
      FLAVOUR,
      FRAME,
      LIMITS,
      PIX_SIZE,
      POSITION,
      POSITION_INTERVAL,
      REDSHIFT_IDENTIFIER,
      RED_SPEC_LABEL,
      RED_SPEC_VALUE,
      REFPOS,
      RESOLUTION,
      SIZE,
      SPACE_IDENTIFIER,
      SPECTRAL_IDENTIFIER,
      START,
      STOP,
      TIME,
      TIME_IDENTIFIER,
      TIME_LABEL,
      TIME_SCALE,
      TYPE_DOPPLER,
      UNIT,
      VELOCITY_IDENTIFIER,
      VELOCITY
   } look_for;

/* Check the global error status. */
   if ( !astOK ) return NULL;

/* Create the returned KeyMap. */
   result = astKeyMap( " ", status );

/* Initialise the word search context. */
   (void) GetNextWord( NULL, &con, status );

/* Get a pointer to the first word in the STC-S description. */
   word = GetNextWord( this, &con, status );

/* Indicate we are currently looking for the time sub-phrase (the first
   item in an STC-S description). */
   look_for = TIME_IDENTIFIER;

/* Initialise everything else. */
   fbuf = NULL;
   naxes = 0;
   prop = NULL;
   props = NULL;
   redid = NULL_ID;
   spaceid = NULL_ID;
   specid = NULL_ID;
   subphrase = NULL;
   t = NULL;
   timeid = NULL_ID;
   velid = NULL_ID;
   timefrm = NULL;

/* Loop until all words in the STC-S description have been interpreted or
   an error has occurred. */
   while( word && astOK ) {

/* Initialise a flag to indicate that we have interpreted the current word
   sucesfully and so will need to get a new word before the next pass through
   this loop. If it turns out that we cannot interpret the current word
   in this pass, then this flag will be set to zero at some point, thus
   preventing a new word from being acquired and causing another attempt to
   re-interpret the current word in a different context. */
      new_word = 1;

/* If we are currently looking for the time sub-phrase, see if the current
   word is any of the known time sub-phrase identifiers. Is so, move on
   to read the associated sub-phrase component. */
      if( look_for == TIME_IDENTIFIER ) {
/* ------------------------------------------------------------------ */

/* Assume that we will be moving on to read the fill factor (most time
   sub-phrases start with the fill factor ). */
         look_for = FILL_FACTOR;

/* Now check the word to see if it a known time sub-phrase identifier. */
         if( astChrMatch( word, "TimeInterval" ) ) {
            timeid = TIME_INTERVAL_ID;

         } else if( astChrMatch( word, "StartTime" ) ) {
            timeid = START_TIME_ID;

         } else if( astChrMatch( word, "StopTime" ) ) {
            timeid = STOP_TIME_ID;

         } else if( astChrMatch( word, "Time" ) ) {
            look_for = TIME_SCALE;  /* After "Time", we move on to find the
            timeid = TIME_ID;          time-scale, not the fill factor */

/* If the word is not a known time sub-phrase identifier, indicate that we
   should attempt to re-interpret the current word as a space sub-phrase
   identifier, rather than getting a new word. */
         } else {
            look_for = SPACE_IDENTIFIER;
            new_word = 0;
         }

/* If we have found a time sub-phrase identifier, create a KeyMap to hold
   the properties of the time sub-phrase, and store the time sub-phrase
   identifier in the new KeyMap. */
         if( timeid != NULL_ID ) {
            subphrase = "time";
            props = astKeyMap( " ", status );
            astMapPut0A( result, "TIME_PROPS", props, NULL );
            astMapPut0C( props, "ID", word, NULL );
            naxes = 1;
         }



/* If we are currently looking for the space sub-phrase, see if the current
   word is any of the known space sub-phrase identifiers. Is so, move on
   to read the associated sub-phrase component. */
      } else if( look_for == SPACE_IDENTIFIER ) {
/* ------------------------------------------------------------------ */

/* Indicate we have finished any preceding time sub-phrase. */
         timeid = NULL_ID;

/* Now check the word to see if it a known space sub-phrase identifier. */
         spaceid = SpaceId( word, status );

/* Decide what to look for next. */
         if( spaceid == POSITION_ID ) {
            look_for = FRAME;

         } else if( spaceid != NULL_ID ) {
            look_for = FILL_FACTOR;

/* If the word is not a known space sub-phrase identifier, move on to
   re-interpret it as a Spectral sub-phrase identifier. */
         } else {
            look_for = SPECTRAL_IDENTIFIER;
            new_word = 0;
         }

/* If we have found a space sub-phrase identifier, create a KeyMap to hold
   the properties of the space sub-phrase, and store the space sub-phrase
   identifier in the new KeyMap. */
         if( spaceid != NULL_ID ) {
            subphrase = "space";
            if( props ) props = astAnnul( props );
            props = astKeyMap( " ", status );
            astMapPut0A( result, "SPACE_PROPS", props, NULL );
            astMapPut0C( props, "ID", word, NULL );
         }



/* If we are currently looking for the velocity sub-phrase, see if the current
   word is any of the known velocity sub-phrase identifiers. Is so, move on
   to read the associated sub-phrase component.  */
      } else if( look_for == VELOCITY_IDENTIFIER ) {
/* ------------------------------------------------------------------ */

/* Indicate we have finished any preceding space sub-phrase. */
         spaceid = NULL_ID;

/* Now check the word to see if it a known velocity sub-phrase identifier. */
         if( astChrMatch( word, "VelocityInterval" ) ) {
            velid = VELOCITY_INTERVAL_ID;
            look_for = FILL_FACTOR;

         } else if( astChrMatch( word, "Velocity" ) ) {
            velid = VELOCITY_ID;
            look_for = VELOCITY;

/* If the word is not a known velocity sub-phrase identifier, move on to
   re-interpret it as a Spectral sub-phrase identifier. */
         } else {
            look_for = SPECTRAL_IDENTIFIER;
            new_word = 0;
         }

/* If we have found a velocity sub-phrase identifier, create a KeyMap to
   hold the properties of the velocity sub-phrase, and store the velocity
   sub-phrase identifier in the new KeyMap. */
         if( velid != NULL_ID ) {
            subphrase = "velocity";
            if( props ) props = astAnnul( props );
            props = astKeyMap( " ", status );
            astMapPut0A( result, "VELOCITY_PROPS", props, NULL );
            astMapPut0C( props, "ID", word, NULL );
         }



/* If we are currently looking for the spectral sub-phrase, see if the
   word is any of the known spectral sub-phrase identifiers. Is so, move
   on to read the associated sub-phrase component. */
      } else if( look_for == SPECTRAL_IDENTIFIER ) {
/* ------------------------------------------------------------------ */

/* Indicate we have finished any preceding velocity sub-phrase. */
         velid = NULL_ID;

/* Now check the word to see if it a known spectral sub-phrase identifier. */
         if( astChrMatch( word, "SpectralInterval" ) ) {
            look_for = FILL_FACTOR;         /* Move on to find the fill factor */
            specid = SPECTRAL_INTERVAL_ID;

         } else if( astChrMatch( word, "Spectral" ) ) {
            look_for = REFPOS;              /* Move on to find the refpos */
            specid = SPECTRAL_ID;

/* If the word is not a known spectral sub-phrase identifier, move on to
   look for the Redshift sub-phrase. */
         } else {
            look_for = REDSHIFT_IDENTIFIER;
            new_word = 0;
         }

/* If we have found a spectral sub-phrase identifier, create a KeyMap to
   hold the properties of the spectral sub-phrase, and store the spectral
   sub-phrase identifier in the new KeyMap. */
         if( specid != NULL_ID ) {
            subphrase = "spectral";
            if( props ) props = astAnnul( props );
            props = astKeyMap( " ", status );
            astMapPut0A( result, "SPECTRAL_PROPS", props, NULL );
            astMapPut0C( props, "ID", word, NULL );
            naxes = 1;
         }



/* If we are currently looking for the redshift sub-phrase, see if the
   word is any of the known redshift sub-phrase identifiers. Is so, move
   on to read the associated sub-phrase component. */
      } else if( look_for == REDSHIFT_IDENTIFIER ) {
/* ------------------------------------------------------------------ */

/* Indicate we have finished any preceding spectral sub-phrase. */
         specid = NULL_ID;

/* Now check the word to see if it a known spectral sub-phrase identifier. */
         if( astChrMatch( word, "RedshiftInterval" ) ) {
            look_for = FILL_FACTOR;       /* Move on to find the fill factor */
            redid = REDSHIFT_INTERVAL_ID;

         } else if( astChrMatch( word, "Redshift" ) ) {
            look_for = REFPOS;            /* Move on to find the refpos */
            redid = REDSHIFT_ID;

/* If the word is not a known redshift sub-phrase identifier, report a
   warning. */
         } else if( word[ 0 ] && astOK ) {
            astError( AST__BADIN, "astRead(%s): Unsupported or irrelevant "
                      "word '%s' found in STC-S %s sub-phrase: '%s'.", status,
                      astGetClass( this ), word, subphrase,
                      ContextFragment( &con, &fbuf, status ) );
            new_word = 0;
         }

/* If we have found a redshift sub-phrase identifier, create a KeyMap to
   hold the properties of the redshift sub-phrase, and store the redshift
   sub-phrase identifier in the new KeyMap. */
         if( redid != NULL_ID ) {
            subphrase = "redshift";
            if( props ) props = astAnnul( props );
            props = astKeyMap( " ", status );
            astMapPut0A( result, "REDSHIFT_PROPS", props, NULL );
            astMapPut0C( props, "ID", word, NULL );
            naxes = 1;
         }

/* Indicate we can now end when we run out of input words. */
         con.done = 1;



/* If we are currently looking for a fill factor... */
      } else if( look_for == FILL_FACTOR ) {
/* ------------------------------------------------------------------ */

/* If the current word is "fillfactor" attempt to read the numerical filling
   factor from the next word. If this fails, or if the current word is
   not "fillfactor", indicate that we will be re-interpreting the current
   word in a new context and so do not need a new word. */
         if( astChrMatch( word, "fillfactor" ) ) {
            word = GetNextWord( this, &con, status );
            if( astChr2Double( word ) == AST__BAD ) {
               astError( AST__BADIN, "astRead(StcsChan): Expected a numerical "
                         "filling factor, but found '%s' in the %s "
                         "sub-phrase of STC-S description: '%s'.", status,
                          word, subphrase, ContextFragment( &con, &fbuf,
                                                            status ) );
               new_word = 0;
            }
         } else {
            new_word = 0;
         }

/* If we are reading a time sub-phrase, move on to read the timescale. */
         if( timeid != NULL_ID ) {
            look_for = TIME_SCALE;

/* If we are reading a space sub-phrase, move on to read the frame. */
         } else if( spaceid != NULL_ID ) {
            look_for = FRAME;

/* If we are reading a velocity sub-phrase, move on to read the limits. */
         } else if( velid != NULL_ID ) {
            look_for = LIMITS;

/* Otherwise (i.e. for spectral and redshift sub-phrases) move on to read
   the refpos. */
         } else {
            look_for = REFPOS;
         }

/* If the word was usable, record it as the fillfactor property. */
         if( new_word ) astMapPut0C( props, "FILLFACTOR", word, NULL );



/* If we are currently looking for a time scale... */
      } else if( look_for == TIME_SCALE ) {
/* ------------------------------------------------------------------ */

/* If the current word is a recognised STC-S timescale, store it in the
   props KeyMap. Otherwise, indicate that the word can be re-used in the
   next context. */
         if( astChrMatch( word, "TT" )  ||
             astChrMatch( word, "TDT" ) ||
             astChrMatch( word, "ET" )  ||
             astChrMatch( word, "TAI" ) ||
             astChrMatch( word, "IAT" ) ||
             astChrMatch( word, "UTC" ) ||
             astChrMatch( word, "TEB" ) ||
             astChrMatch( word, "TDB" ) ||
             astChrMatch( word, "TCG" ) ||
             astChrMatch( word, "TCB" ) ||
             astChrMatch( word, "LST" ) ||
             astChrMatch( word, "nil" ) ) {

            astMapPut0C( props, "TIMESCALE", word, NULL );

         } else {
            new_word = 0;
         }

/* Move on to look for a refpos */
         look_for = REFPOS;



/* If we are currently looking for a space frame... */
      } else if( look_for == FRAME ) {
/* ------------------------------------------------------------------ */

/* If the current word is a recognised STC-S spatial frame, store it in
   the props KeyMap. Otherwise, indicate that the word can be re-used. */
         if( astChrMatch( word, "ICRS" ) ||
             astChrMatch( word, "FK5" ) ||
             astChrMatch( word, "FK4" ) ||
             astChrMatch( word, "J2000" ) ||
             astChrMatch( word, "B1950" ) ||
             astChrMatch( word, "ECLIPTIC" ) ||
             astChrMatch( word, "GALACTIC" ) ||
             astChrMatch( word, "GALACTIC_II" ) ||
             astChrMatch( word, "SUPER_GALACTIC" ) ||
             astChrMatch( word, "GEO_C" ) ||
             astChrMatch( word, "GEO_D" ) ||
             astChrMatch( word, "UNKNOWNFrame" ) ) {

            astMapPut0C( props, "FRAME", word, NULL );

         } else {
            new_word = 0;
         }

/* Move on to look for a refpos */
         look_for = REFPOS;



/* If we are currently looking for a refpos... */
      } else if( look_for == REFPOS ) {
/* ------------------------------------------------------------------ */

/* If the current word is a recognised STC-S reference position, store it in
   the props KeyMap. Otherwise, indicate that the word can be re-used. The
   first group of reference positions apply to all sub-phrases. */
         if( astChrMatch( word, "GEOCENTER" ) ||
             astChrMatch( word, "BARYCENTER" ) ||
             astChrMatch( word, "HELIOCENTER" ) ||
             astChrMatch( word, "TOPOCENTER" ) ||
             astChrMatch( word, "GALACTIC_CENTER" ) ||
             astChrMatch( word, "EMBARYCENTER" ) ||
             astChrMatch( word, "MOON" ) ||
             astChrMatch( word, "MERCURY" ) ||
             astChrMatch( word, "VENUS" ) ||
             astChrMatch( word, "MARS" ) ||
             astChrMatch( word, "JUPITER" ) ||
             astChrMatch( word, "SATURN" ) ||
             astChrMatch( word, "URANUS" ) ||
             astChrMatch( word, "NEPTUNE" ) ||
             astChrMatch( word, "PLUTO" ) ||
             astChrMatch( word, "UNKNOWNRefPos" ) ) {

            astMapPut0C( props, "REFPOS", word, NULL );

/* This group of reference positions apply only to spectral and redshift
   sub-phrases. */
         } else if( astChrMatch( word, "LSR" ) ||
                    astChrMatch( word, "LSRK" ) ||
                    astChrMatch( word, "LSRD" ) ||
                    astChrMatch( word, "LOCAL_GROUP_CENTER" ) ) {

            if( specid != NULL_ID || redid != NULL_ID ) {
               astMapPut0C( props, "REFPOS", word, NULL );

            } else if( astOK ) {
               astError( AST__BADIN, "astRead(StcsChan): Illegal reference "
                         "position '%s' found in the %s sub-phrase of "
                         "STC-S description: '%s'.", status, word,
                         subphrase, ContextFragment( &con, &fbuf, status ) );
               new_word = 0;
            }

         } else {
            new_word = 0;
         }

/* Choose what to look for next on the basis of the type of sub-phrase
   currently being interpreted. */
         if( timeid == TIME_INTERVAL_ID ){
            look_for = START;   /* Move on to find the start time */

         } else if( timeid == START_TIME_ID ){
            look_for = START;   /* Move on to find the start time */

         } else if( timeid == STOP_TIME_ID ){
            look_for = STOP;    /* Move on to find the stop time */

         } else if( timeid == TIME_ID ){
            look_for = TIME;    /* Move on to find the time */

         } else if( spaceid != NULL_ID ){
            look_for = FLAVOUR; /* Move on to find the spatial flavour */

         } else if( specid == SPECTRAL_INTERVAL_ID ) {
            look_for = LIMITS;  /* Move on to find the spectral limits */

         } else if( specid == SPECTRAL_ID ) {
            look_for = RED_SPEC_VALUE; /* Move on to find the spectral value */

         } else if( redid == REDSHIFT_INTERVAL_ID ) {
            look_for = TYPE_DOPPLER;   /* Move on to find the redshift type */

         } else if( redid == REDSHIFT_ID ) {
            look_for = TYPE_DOPPLER;   /* Move on to find the redshift type */

         } else if( astOK ) {  /* Should never happen */
             astError( AST__INTER, "astRead(StcsChan): Sanity check 1 fails in "
                       "function ReadProps (AST internal programming error).",
                       status );
            new_word = 0;
         }





/* If we are currently looking for a start time... */
      } else if( look_for == START ) {
/* ------------------------------------------------------------------ */

/* Save the current word as the start of the START value. */
         nc = 0;
         prop = astAppendString( prop, &nc, word );

/* If the current word is "JD" or "MJD", the following word should be
   numerical. */
         is_jd = astChrMatch( word, "JD" );
         if( is_jd || astChrMatch( word, "MJD" ) ) {
            word = GetNextWord( this, &con, status );
            value = astChr2Double( word );
            if( value == AST__BAD && astOK ) {
               astError( AST__BADIN, "astRead(StcsChan): Expected numerical "
                         "value in Start time, but found '%s %s' in STC-S "
                         "description: '%s'.", status, prop, word,
                         ContextFragment( &con, &fbuf, status ) );

/* Append the second word to the first word. */
            } else {
               prop = astAppendString( prop, &nc, " " );
               prop = astAppendString( prop, &nc, word );
            }

/* Convert JD to MJD if required. */
            start = is_jd ? value - 2400000.5 : value;

/* Otherwise, the current word should be an ISO date. Use a TimeFrame
   to check the string. */
         } else {
            if( !timefrm ) timefrm = astTimeFrame( " ", status );
            if( !astUnformat( timefrm, 0, word, &start ) && astOK ) {
               astError( AST__BADIN, "astRead(StcsChan): Expected ISO date "
                         "string Start time, but found '%s' in an STC-S "
                         "description: '%s'.", status, word,
                         ContextFragment( &con, &fbuf, status ) );
            }
         }

/* Record the START property. */
         astMapPut0C( props, "START", prop, NULL );
         astMapPut0D( props, "MJDSTART", start, NULL );

/* Decide what to do next. */
         if( timeid == TIME_INTERVAL_ID ){
            look_for = STOP;        /* Move on to find the stop time */

         } else if( timeid == START_TIME_ID ){
            look_for = TIME_LABEL;  /* Move on to find the "coord" time */

         }



/* If we are currently looking for a stop time... */
      } else if( look_for == STOP ) {
/* ------------------------------------------------------------------ */

/* Save the current word as the start of the STOP value. */
         nc = 0;
         prop = astAppendString( prop, &nc, word );

/* If the current word is "JD" or "MJD", the following word should be
   numerical. */
         is_jd = astChrMatch( word, "JD" );
         if( is_jd || astChrMatch( word, "MJD" ) ) {
            word = GetNextWord( this, &con, status );
            value = astChr2Double( word );
            if( value == AST__BAD && astOK ) {
               astError( AST__BADIN, "astRead(StcsChan): Expected numerical "
                         "value in Stop time, but found '%s %s' in STC-S "
                         "description: '%s'.", status, prop, word,
                         ContextFragment( &con, &fbuf, status ) );

/* Append the second word to the first word. */
            } else {
               prop = astAppendString( prop, &nc, " " );
               prop = astAppendString( prop, &nc, word );
            }

/* Convert JD to MJD if required. */
            stop = is_jd ? value - 2400000.5 : value;

/* Otherwise, the current word should be an ISO date. Use a TimeFrame
   to check the string. */
         } else {
            if( !timefrm ) timefrm = astTimeFrame( " ", status );
            if( !astUnformat( timefrm, 0, word, &stop ) && astOK ) {
               astError( AST__BADIN, "astRead(StcsChan): Expected ISO date "
                         "string Stop time, but found '%s' in an STC-S "
                         "description: '%s'.", status, word,
                         ContextFragment( &con, &fbuf, status ) );
            }
         }

/* Record the STOP property. */
         astMapPut0C( props, "STOP", prop, NULL );
         astMapPut0D( props, "MJDSTOP", stop, NULL );

/* Move on to find the "coord" time. */
         look_for = TIME_LABEL;



/* If we are currently looking for the label before a time coord value... */
      } else if( look_for == TIME_LABEL ) {
/* ------------------------------------------------------------------ */
         if( astChrMatch( word, "Time" ) ) {
            look_for = TIME;
         } else {
            new_word = 0;
            look_for = UNIT;
         }



/* If we are currently looking for a time... */
      } else if( look_for == TIME ) {
/* ------------------------------------------------------------------ */

/* Save the current word as the start of the TIME value. */
         nc = 0;
         prop = astAppendString( prop, &nc, word );

/* If the current word is "JD" or "MJD", the following word should be
   numerical. */
         is_jd = astChrMatch( word, "JD" );
         if( is_jd || astChrMatch( word, "MJD" ) ) {
            word = GetNextWord( this, &con, status );
            value = astChr2Double( word );
            if( value == AST__BAD && astOK ) {
               astError( AST__BADIN, "astRead(StcsChan): Expected numerical "
                         "value in Time value, but found '%s %s' in STC-S "
                         "description: '%s'.", status, prop, word,
                         ContextFragment( &con, &fbuf, status ) );

/* Append the second word to the first word. */
            } else {
               prop = astAppendString( prop, &nc, " " );
               prop = astAppendString( prop, &nc, word );
            }

/* Convert JD to MJD if required. */
            time = is_jd ? value - 2400000.5 : value;

/* Otherwise, the current word should be an ISO date. Use a TimeFrame
   to check the string. */
         } else {
            if( !timefrm ) timefrm = astTimeFrame( " ", status );
            if( !astUnformat( timefrm, 0, word, &time ) && astOK ) {
               astError( AST__BADIN, "astRead(StcsChan): Expected ISO date "
                         "string Time value, but found '%s' in an STC-S "
                         "description: '%s'.", status, word,
                         ContextFragment( &con, &fbuf, status ) );
            }
         }

/* Record the TIME property. */
         astMapPut0C( props, "TIME", prop, NULL );
         astMapPut0D( props, "MJDTIME", time, NULL );

/* Move on to look for the units. */
         look_for = UNIT;



/* If we are currently looking for a space "flavor"... */
      } else if( look_for == FLAVOUR ) {
/* ------------------------------------------------------------------ */

/* If the current word is a recognised flavour value, note how many axis
   values are required to specify a position. Otherwise, indicate that
   the word can be re-used. */
         if( astChrMatch( word, "SPHER2" ) ) {
            naxes = 2;

         } else if( astChrMatch( word, "UNITSPHER" ) ) {
            naxes = 2;

         } else if( astChrMatch( word, "CART1" ) ) {
            naxes = 1;

         } else if( astChrMatch( word, "CART2" ) ) {
            naxes = 2;

         } else if( astChrMatch( word, "CART3" ) ) {
            naxes = 3;

         } else if( astChrMatch( word, "SPHER3" ) ) {
            naxes = 3;

         } else {
            naxes = 2;
            new_word = 0;
         }

/* If the word was recognised as a flavour, store it in the porperties
   KeyMap. */
         if( new_word ) {
            astMapPut0C( props, "FLAVOR", word, NULL );
            astMapPut0C( props, "FLAVOUR", word, NULL );
         }

/* The next set of words to be read from the source function will specify
   the arguments of the region enclosing the spatial positions. This may
   contain nested regions, so use a recursive function to read the
   arguments and store them in the properties KeyMap. */
         if( new_word ) word = GetNextWord( this, &con, status );
         word = ReadSpaceArgs( this, word, spaceid, naxes, &con, props,
                               status );
         new_word = 0;

/* Move on to the next look_for (following the region argument list read
   by ReadSpaceArgs). */
         if( spaceid == POSITION_ID ) {
            look_for = UNIT;
         } else {
            look_for = POSITION;
         }



/* If we are currently looking for interval "lolimit"and "hilimit" ... */
      } else if( look_for == LIMITS ) {
/* ------------------------------------------------------------------ */
         if( velid != NULL_ID ) {
            t = "velocity";
            look_for = VELOCITY;

         } else if( specid != NULL_ID ) {
            t = "spectral";
            look_for = RED_SPEC_LABEL;

         } else {
            t = "redshift";
            look_for = RED_SPEC_LABEL;
         }

/* The current word should be a numerical value (the low limit ). */
         if( astChr2Double( word ) == AST__BAD && astOK ) {
            astError( AST__BADIN, "astRead(StcsChan): Expected a numerical "
                      "value for a %s lolimit, but found '%s' in an STC-S "
                      "description: '%s'.", status, t, word,
                      ContextFragment( &con, &fbuf, status ) );
         } else {
            astMapPut0C( props, "LOLIMIT", word, NULL );
         }

/* The next word should be a numerical value (the high limit ). */
         word = GetNextWord( this, &con, status );
         if( astChr2Double( word ) == AST__BAD && astOK ) {
            astError( AST__BADIN, "astRead(StcsChan): Expected a numerical "
                      "value for a %s hilimit, but found '%s' in an STC-S "
                      "description: '%s'.", status, t, word,
                      ContextFragment( &con, &fbuf, status ) );
         } else {
            astMapPut0C( props, "HILIMIT", word, NULL );
         }



/* If we are currently looking for the label before a spectral or redshift
   value... */
      } else if( look_for == RED_SPEC_LABEL ) {
/* ------------------------------------------------------------------ */
         if( specid != NULL_ID && astChrMatch( word, "Spectral" ) ) {
            look_for = RED_SPEC_VALUE;

         } else if( redid != NULL_ID && astChrMatch( word, "Redshift" ) ) {
            look_for = RED_SPEC_VALUE;

         } else {
            new_word = 0;
            look_for = UNIT;
         }



/* If we are currently looking for an spectral or redshift value. */
      } else if( look_for == RED_SPEC_VALUE ) {
/* ------------------------------------------------------------------ */

         t = ( specid != NULL_ID ) ? "spectral" : "redshift";
         if( astChr2Double( word ) == AST__BAD && astOK ) {
            astError( AST__BADIN, "astRead(StcsChan): Expected a numerical "
                      "%s value, but found '%s' in an STC-S "
                      "description: '%s'.", status, t, word,
                      ContextFragment( &con, &fbuf, status ) );
         } else {
            astMapPut0C( props, ( specid != NULL_ID ) ? "SPECTRAL" : "REDSHIFT",
                         word, NULL );
         }

/* Decide what to do next. */
         look_for = UNIT;



/* If we are currently looking for information needed to create a spatial
   Position ... */
      } else if( look_for == POSITION ) {
/* ------------------------------------------------------------------ */

/* Check the current word is "Position". If so, get the next word. */
         if( astChrMatch( word, "Position" ) ) {
            word = GetNextWord( this, &con, status );

/* Get a value for every space axis. */
            nc = 0;
            for( iaxis = 0; iaxis < naxes; iaxis++ ) {
               val[ iaxis ] = astChr2Double( word );
               if( val[ iaxis ] == AST__BAD && astOK ) {
                  astError( AST__BADIN, "astRead(StcsChan): Expected another "
                            "axis value for a space Position, but found "
                            "'%s' in an STC-S description: '%s'.", status,
                            word, ContextFragment( &con, &fbuf, status ) );
               }
               prop = astAppendString( prop, &nc, word );
               prop = astAppendString( prop, &nc, " " );
               word = GetNextWord( this, &con, status );
            }

/* Remove the trailing space, and store the property value in the KeyMap. */
            prop[ nc - 1 ] = 0;
            astMapPut0C( props, "POSITION", prop, NULL );
            astMapPut1D( props, "DPOSITION", naxes, val, NULL );
         }

/* Move on to read the "unit" item. */
         new_word = 0;
         look_for = UNIT;



/* If we are currently looking for the redshift type and doppler
   definition ... */
      } else if( look_for == TYPE_DOPPLER ) {
/* ------------------------------------------------------------------ */

         if( astChrMatch( word, "VELOCITY" ) ||
             astChrMatch( word, "REDSHIFT" ) ) {
            astMapPut0C( props, "TYPE", word, NULL );
            word = GetNextWord( this, &con, status );
         }

         if( astChrMatch( word, "OPTICAL" ) ||
             astChrMatch( word, "RADIO" ) ||
             astChrMatch( word, "RELATIVISTIC" ) ) {
            astMapPut0C( props, "DOPPLERDEF", word, NULL );
         } else {
            new_word = 0;
         }

/* Decide what to do next. */
         look_for = ( redid == REDSHIFT_INTERVAL_ID ) ? LIMITS : RED_SPEC_VALUE;



/* If we are currently looking for a velocity label and value... */
      } else if( look_for == VELOCITY ) {
/* ------------------------------------------------------------------ */

         if( astChrMatch( word, "Velocity" ) ) {
            word = GetNextWord( this, &con, status );
            if( astChr2Double( word ) == AST__BAD && astOK ) {
               astError( AST__BADIN, "astRead(StcsChan): Expected a "
                         "numerical Velocity value but found 'Velocity %s' "
                         "in an STC-S description: '%s'.", status, word,
                         ContextFragment( &con, &fbuf, status ) );
            }

         } else {
            new_word = 0;
         }

         look_for = UNIT;



/* If we are currently looking for a "unit" string... */
      } else if( look_for == UNIT ) {
/* ------------------------------------------------------------------ */

/* See if the current word is "unit". If so, read the next word (which
   will be the unit string itself). Otherwise, indicate the current word
   can be re-used. */
         if( astChrMatch( word, "unit" ) ) {
            word = GetNextWord( this, &con, status );
         } else {
            new_word = 0;
         }

/* If we have a unit string... */
         if( new_word ) {

/* Check that the unit string is one of the allowed values (different
   values are allowed for different sub-phrases). Space frames can have
   multiple units strings (one for each axis) so loop round until a string
   is found which is not a valid unit string. */
            nc = 0;
            nunit = 0;
            while( ( timeid != NULL_ID && (  !strcmp( word, "s" ) ||
                                          !strcmp( word, "d" ) ||
                                          !strcmp( word, "a" ) ||
                                          !strcmp( word, "yr" ) ||
                                          !strcmp( word, "cy" ) ) ) ||

                ( spaceid != NULL_ID && ( !strcmp( word, "deg" ) ||
                                          !strcmp( word, "arcmin" ) ||
                                          !strcmp( word, "arcsec" ) ||
                                          !strcmp( word, "m" ) ||
                                          !strcmp( word, "mm" ) ||
                                          !strcmp( word, "m" ) ||
                                          !strcmp( word, "km" ) ||
                                          !strcmp( word, "AU" ) ||
                                          !strcmp( word, "pc" ) ||
                                          !strcmp( word, "kpc" ) ||
                                          !strcmp( word, "Mpc" ) ) ) ||

                ( velid != NULL_ID && (   !strcmp( word, "deg" ) ||
                                          !strcmp( word, "arcmin" ) ||
                                          !strcmp( word, "arcsec" ) ||
                                          !strcmp( word, "m" ) ||
                                          !strcmp( word, "mm" ) ||
                                          !strcmp( word, "km" ) ||
                                          !strcmp( word, "AU" ) ||
                                          !strcmp( word, "pc" ) ||
                                          !strcmp( word, "kpc" ) ||
                                          !strcmp( word, "Mpc" ) ) ) ||

                (                         !strcmp( word, "Hz" ) ||
                                          !strcmp( word, "MHz" ) ||
                                          !strcmp( word, "GHz" ) ||
                                          !strcmp( word, "m" ) ||
                                          !strcmp( word, "mm" ) ||
                                          !strcmp( word, "um" ) ||
                                          !strcmp( word, "nm" ) ||
                                          !strcmp( word, "Angstrom" ) ||
                                          !strcmp( word, "eV" ) ||
                                          !strcmp( word, "keV" ) ||
                                          !strcmp( word, "MeV" ) ) ) {

               prop = astAppendString( prop, &nc, word );
               prop = astAppendString( prop, &nc, " " );
               nunit++;
               word = GetNextWord( this, &con, status );
            }

/* Report an error if an inappropriate number of valid unit strings was
   found. */
            if( nunit == 0 && astOK ) {
               astError( AST__BADIN, "astRead(StcsChan): Unsupported "
                         "units (%s) for the %s sub-phrase within an "
                         "STC-S description: '%s'.", status, word, subphrase,
                         ContextFragment( &con, &fbuf, status ) );

            } else if( nunit != 1 && nunit != naxes && astOK ) {
               astError( AST__BADIN, "astRead(StcsChan): Incorrect number of "
                         "units string (%d) supplied for the %s sub-phrase within an "
                         "STC-S description: '%s'.", status, nunit, subphrase,
                         ContextFragment( &con, &fbuf, status ) );

/* Otherwise, remove the trailing space, and store the property value in the
   KeyMap. */
            } else {
               prop[ nc - 1 ] = 0;
               astMapPut0C( props, "UNIT", prop, NULL );
            }

/* The current word is the first word that was not a valid unit string,
   and so can be re-used. */
            new_word = 0;
         }

/* Move on to find the errors. */
         look_for = ERROR;



/* If we are currently looking for an "Error" string... */
      } else if( look_for == ERROR ) {
/* ------------------------------------------------------------------ */

/* If the current word is "Error" read all subsequent words until the first
   non-numerical value is encountered. */
         if( astChrMatch( word, "Error" ) ) {
            word = GetNextWord( this, &con, status );
            value = astChr2Double( word );

            nc = 0;
            nval = 0;
            while( value != AST__BAD ) {
               if( nval < MAXVAL ) {
                  val[ nval++ ] = value;
                  prop = astAppendString( prop, &nc, word );
                  prop = astAppendString( prop, &nc, " " );
                  word = GetNextWord( this, &con, status );
                  value = astChr2Double( word );
               } else {
                  astError( AST__BADIN, "astRead(StcsChan): Too many (more "
                            "than %d) numerical values found for the Error "
                            "property of the %s sub-phrase within an STC-S "
                            "description: '%s'.", status, MAXVAL, subphrase,
                            ContextFragment( &con, &fbuf, status ) );
                  break;
               }
            }

/* Report an error if no numerical error values were found. */
            if( nval == 0 && astOK ) {
               astError( AST__BADIN, "astRead(StcsChan): Expected a "
                         "numerical error value but found 'Error %s' "
                         "for the %s sub-phrase within an "
                         "STC-S description: '%s'.", status, word, subphrase,
                         ContextFragment( &con, &fbuf, status ) );

/* Otherwise, remove the trailing space and store the concatenated
   string of formatted values in the properties KeyMap. Also store a
   corresponding vector of floating point values in the KeyMap. */
            } else {
               prop[ nc - 1 ] = 0;
               astMapPut0C( props, "ERROR", prop, NULL );
               astMapPut1D( props, "DERROR", nval, val, NULL );
            }
         }

/* Indicate that we do not need to get a new word (we can re-use the last
   one that turned out not to be a numerical value above). */
         new_word = 0;

/* Next look for Resolution */
         look_for = RESOLUTION;



/* If we are currently looking for a "Resolution" string... */
      } else if( look_for == RESOLUTION ) {
/* ------------------------------------------------------------------ */

/* If the current word is "Resolution" read all subsequent words until the
   first non-numerical value is encountered. */
         if( astChrMatch( word, "Resolution" ) ) {
            word = GetNextWord( this, &con, status );
            value = astChr2Double( word );

            nc = 0;
            nval = 0;
            while( value != AST__BAD ) {
               if( nval < MAXVAL ) {
                  val[ nval++ ] = value;
                  prop = astAppendString( prop, &nc, word );
                  prop = astAppendString( prop, &nc, " " );
                  word = GetNextWord( this, &con, status );
                  value = astChr2Double( word );
               } else {
                  astError( AST__BADIN, "astRead(StcsChan): Too many (more "
                            "than %d) numerical values found for the Resolution "
                            "property of the %s sub-phrase within an STC-S "
                            "description: '%s'.", status, MAXVAL, subphrase,
                            ContextFragment( &con, &fbuf, status ) );
                  break;
               }
            }

/* Report an error if no numerical values were found. */
            if( nval == 0 && astOK ) {
               astError( AST__BADIN, "astRead(StcsChan): Expected a "
                         "numerical resolution value but found 'Resolution %s' "
                         "for the %s sub-phrase within an STC-S description:"
                         " '%s'.", status, word, subphrase,
                         ContextFragment( &con, &fbuf, status ) );

/* Otherwise, remove the trailing space and store the concatenated
   string of formatted values in the properties KeyMap. Also store a
   corresponding vector of floating point values in the KeyMap. */
            } else {
               prop[ nc - 1 ] = 0;
               astMapPut0C( props, "RESOLUTION", prop, NULL );
               astMapPut1D( props, "DRESOLUTION", nval, val, NULL );
            }
         }

/* Indicate that we do not need to get a new word (we can re-use the last
   one that turned out not to be a numerical value above). */
         new_word = 0;

/* Next look for Size. */
         look_for = SIZE;



/* If we are currently looking for a spatial "Size" string... */
      } else if( look_for == SIZE ) {
/* ------------------------------------------------------------------ */

/* If the current word is "Size" read all subsequent words until the
   first non-numerical value is encountered. */
         if( astChrMatch( word, "Size" ) ) {
            word = GetNextWord( this, &con, status );
            value = astChr2Double( word );

            nc = 0;
            nval = 0;
            while( value != AST__BAD ) {
               if( nval < MAXVAL ) {
                  val[ nval++ ] = value;
                  prop = astAppendString( prop, &nc, word );
                  prop = astAppendString( prop, &nc, " " );
                  word = GetNextWord( this, &con, status );
                  value = astChr2Double( word );
               } else {
                  astError( AST__BADIN, "astRead(StcsChan): Too many (more "
                            "than %d) numerical values found for the Size "
                            "property of the %s sub-phrase within an STC-S "
                            "description: '%s'.", status, MAXVAL, subphrase,
                            ContextFragment( &con, &fbuf, status ) );
                  break;
               }
            }

/* Report an error if no numerical values were found. */
            if( nval == 0 && astOK ) {
               astError( AST__BADIN, "astRead(StcsChan): Expected a "
                         "numerical size value but found 'Size %s' "
                         "for the %s sub-phrase within an STC-S description:"
                         " '%s'.", status, word, subphrase,
                         ContextFragment( &con, &fbuf, status ) );

/* Otherwise, remove the trailing space and store the concatenated
   string of formatted values in the properties KeyMap. Also store a
   corresponding vector of floating point values in the KeyMap. */
            } else {
               prop[ nc - 1 ] = 0;
               astMapPut0C( props, "SIZE", prop, NULL );
               astMapPut1D( props, "DSIZE", nval, val, NULL );
            }
         }

/* Indicate that we do not need to get a new word (we can re-use the last
   one that turned out not to be a numerical value above). */
         new_word = 0;

/* Next look for PixSize. */
         look_for = PIX_SIZE;



/* If we are currently looking for a "PixSize" string... */
      } else if( look_for == PIX_SIZE ) {
/* ------------------------------------------------------------------ */

/* If the current word is "PixSize" read all subsequent words until the
   first non-numerical value is encountered. */
         if( astChrMatch( word, "PixSize" ) ) {
            word = GetNextWord( this, &con, status );
            value = astChr2Double( word );

            nc = 0;
            nval = 0;
            while( value != AST__BAD ) {
               if( nval < MAXVAL ) {
                  val[ nval++ ] = value;
                  prop = astAppendString( prop, &nc, word );
                  prop = astAppendString( prop, &nc, " " );
                  word = GetNextWord( this, &con, status );
                  value = astChr2Double( word );
               } else {
                  astError( AST__BADIN, "astRead(StcsChan): Too many (more "
                            "than %d) numerical values found for the PixSize "
                            "property of the %s sub-phrase within an STC-S "
                            "description: '%s'.", status, MAXVAL, subphrase,
                            ContextFragment( &con, &fbuf, status ) );
                  break;
               }
            }

/* Report an error if no numerical values were found. */
            if( nval == 0 && astOK ) {
               astError( AST__BADIN, "astRead(StcsChan): Expected a "
                         "numerical pixel size but found 'PixSize %s' "
                         "for the %s sub-phrase within an STC-S description:"
                         " '%s'.", status, word, subphrase,
                         ContextFragment( &con, &fbuf, status ) );

/* Otherwise, remove the trailing space and store the concatenated
   string of formatted values in the properties KeyMap. Also store a
   corresponding vector of floating point values in the KeyMap. */
            } else {
               prop[ nc - 1 ] = 0;
               astMapPut0C( props, "PIXSIZE", prop, NULL );
               astMapPut1D( props, "DPIXSIZE", nval, val, NULL );
            }
         }

/* Indicate that we do not need to get a new word (we can re-use the last
   one that turned out not to be a numerical value above). */
         new_word = 0;

/* Next look for the next sub-phrase. */
         if( timeid != NULL_ID ) {
            look_for = SPACE_IDENTIFIER;

         } else if( spaceid != NULL_ID ) {
            look_for = VELOCITY_IDENTIFIER;

         } else if( velid != NULL_ID ) {
            look_for = SPECTRAL_IDENTIFIER;

         } else if( specid != NULL_ID ) {
            look_for = REDSHIFT_IDENTIFIER;

         } else {
            break;
         }




/* Report an error for any unknown look_for. */
/* ------------------------------------------------------------------ */
      } else if( astOK ) {
         astError( AST__INTER, "astRead(StcsChan): Illegal look_for value "
                   "(%d) encountered (internal AST programming error).",
                   status, look_for );
      }

/* If required, get the next word in the STC-S description. */
      if( new_word ) word = GetNextWord( this, &con, status );
   }

/* Free resources stored in the GetNextWord context structure. */
   con.done = 1;
   (void) GetNextWord( this, &con, status );
   FreeContext( &con, status );

/* Free other resources */
   if( fbuf ) fbuf = astFree( fbuf );
   if( prop ) prop = astFree( prop );
   if( props ) props = astAnnul( props );
   if( timefrm ) timefrm = astAnnul( timefrm );

/* If an error occurred, clean up by deleting the new Object and
   return a NULL pointer. */
   if ( !astOK ) result = astDelete( result );

/* Return the pointer to the properties KeyMap. */
   return result;

/* Undefine Local Constants: */
#undef MAXVAL
}

static const char *ReadSpaceArgs( AstStcsChan *this, const char *word,
                                  int spaceid, int naxes, WordContext *con,
                                  AstKeyMap *props, int *status ){
/*
*  Name:
*     ReadSpaceArgs

*  Purpose:
*     Read space region arguments from an STC-S description.

*  Type:
*     Private function.

*  Synopsis:
*     #include "stcschan.h"
*     const char *ReadSpaceArgs( AstStcsChan *this, const char *word,
*                                int spaceid, int naxes, WordContext *con,
*                                AstKeyMap *props, int *status )

*  Class Membership:
*     StcsChan member function

*  Description:
*     This function parses the list of space-separated words that form
*     the argument list of a spatial region. These words are read from the
*     source function, and stored in the supplied KeyMap using keys that
*     identify their purpose.
*
*     This function calls itself recursively to handle compound regions.

*  Parameters:
*     this
*        Pointer to the StcsChan.
*     word
*        The first word of the argument list.
*     spaceid
*        An integer identifier for the type of spatial region for which
*        arguments are being read.
*     naxes
*        Number of axes in the space frame.
*     con
*        Pointer to a structure holding context for use with the
*        GetNextWord function. On exit, the next word returned by the
*        GetNextWord function will be the first word following the
*        argument list.
*     props
*        Pointer to the KeyMap in which the argument values should be
*        stored.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     Pointer to the next wpord to be interpreted.

*/


/* Local Variables: */
   AstKeyMap *new_props;   /* KeyMap holding properties of an argument region */
   char *fbuf;             /* Pointer to buffer holding document fragment */
   char *prop;             /* String property value */
   char key[ 20 ];         /* Key for argument region */
   double *p;              /* Pointer to next polygon vertex axis value */
   double *temp;           /* Array of polygon vertex axis values */
   double val;             /* Single numerical value */
   double vals[ 6 ];       /* List of numerical values */
   int iaxis;              /* Axis index */
   int nc;                 /* Used length of string */
   int new_spaceid;        /* Type of next argument region */
   int nreg;               /* Number of argument regions found */
   int nvert;              /* Number of vertices in polygon */

/* Check inherited status */
   if( !astOK ) return word;

/* Initialise. */
   fbuf = NULL;
   prop = NULL;
   nc = 0;

/* If we are looking for information needed to create a spatial
   Interval... */
   if( spaceid == POSITION_INTERVAL_ID ) {

/* Get a lolimit value for every space axis. */
      for( iaxis = 0; iaxis < naxes; iaxis++ ) {
         vals[ iaxis ] = astChr2Double( word );
         if( vals[ iaxis ] == AST__BAD && astOK ) {
            astError( AST__BADIN, "astRead(StcsChan): Expected another "
                   "'lolimit' value for a PositionInterval, but found "
                   "'%s' in an STC-S description: '%s'.", status, word,
                   ContextFragment( con, &fbuf, status ) );
         }
         prop = astAppendString( prop, &nc, word );
         prop = astAppendString( prop, &nc, " " );
         word = GetNextWord( this, con, status );
      }

/* Remove the trailing space, and store the property value in the KeyMap. */
      if( prop && nc > 0 ) {
         prop[ nc - 1 ] = 0;
         astMapPut0C( props, "LOLIMIT", prop, NULL );
         astMapPut1D( props, "DLOLIMIT", naxes, vals, NULL );
      }

/* Get a hilimit value for every space axis. */
      nc = 0;
      for( iaxis = 0; iaxis < naxes; iaxis++ ) {
         vals[ iaxis ] = astChr2Double( word );
         if( vals[ iaxis ] == AST__BAD && astOK ) {
            astError( AST__BADIN, "astRead(StcsChan): Expected another "
                   "'hilimit' value for a PositionInterval, but found "
                   "'%s' in an STC-S description: '%s'.", status, word,
                   ContextFragment( con, &fbuf, status ) );
         }
         prop = astAppendString( prop, &nc, word );
         prop = astAppendString( prop, &nc, " " );
         word = GetNextWord( this, con, status );
      }

/* Remove the trailing space, and store the property value in the KeyMap. */
      if( prop && nc > 0 ) {
         prop[ nc - 1 ] = 0;
         astMapPut0C( props, "HILIMIT", prop, NULL );
         astMapPut1D( props, "DLOLIMIT", naxes, vals, NULL );
      }

/* If we are currently looking for information needed to create a spatial
   AllSky ... */
   } else if( spaceid == ALLSKY_ID ) {



/* If we are currently looking for information needed to create a spatial
   Circle ... */
   } else if( spaceid == CIRCLE_ID ) {

/* Get a centre value for every space axis. */
      nc = 0;
      for( iaxis = 0; iaxis < naxes; iaxis++ ) {
         vals[ iaxis ] = astChr2Double( word );
         if( vals[ iaxis ] == AST__BAD && astOK ) {
            astError( AST__BADIN, "astRead(StcsChan): Expected another "
                   "'centre' value for a Circle, but found "
                   "'%s' in an STC-S description: '%s'.", status, word,
                   ContextFragment( con, &fbuf, status ) );
         }
         prop = astAppendString( prop, &nc, word );
         prop = astAppendString( prop, &nc, " " );
         word = GetNextWord( this, con, status );
      }

/* Remove the trailing space, and store the property value in the KeyMap. */
      if( prop && nc > 0 ) {
         prop[ nc - 1 ] = 0;
         astMapPut0C( props, "CENTRE", prop, NULL );
         astMapPut1D( props, "DCENTRE", naxes, vals, NULL );
      }

/* Get the radius value. */
      val = astChr2Double( word );
      if( val == AST__BAD && astOK ) {
         astError( AST__BADIN, "astRead(StcsChan): Expected a radius "
                   "value for a Circle, but found '%s' in an STC-S "
                   "description: '%s'.", status, word,
                   ContextFragment( con, &fbuf, status ) );
      }

/* Store the property value in the KeyMap. */
      astMapPut0C( props, "RADIUS", word, NULL );

/* Get the next word. */
      word = GetNextWord( this, con, status );



/* If we are currently looking for information needed to create a spatial
   Ellipse ... */
   } else if( spaceid == ELLIPSE_ID ) {

/* Get a centre value for every space axis. */
      nc = 0;
      for( iaxis = 0; iaxis < naxes; iaxis++ ) {
         vals[ iaxis ] = astChr2Double( word );
         if( vals[ iaxis ] == AST__BAD && astOK ) {
            astError( AST__BADIN, "astRead(StcsChan): Expected another "
                   "centre value for an Ellipse, but found "
                   "'%s' in an STC-S description: '%s'.", status, word,
                   ContextFragment( con, &fbuf, status ) );
         }
         prop = astAppendString( prop, &nc, word );
         prop = astAppendString( prop, &nc, " " );
         word = GetNextWord( this, con, status );
      }

/* Remove the trailing space, and store the property value in the KeyMap. */
      if( prop && nc > 0 ) {
         prop[ nc - 1 ] = 0;
         astMapPut0C( props, "CENTRE", prop, NULL );
         astMapPut1D( props, "DCENTRE", naxes, vals, NULL );
      }

/* Get the first radius value . */
      val = astChr2Double( word );
      if( val == AST__BAD && astOK ) {
         astError( AST__BADIN, "astRead(StcsChan): Expected the first "
                   "radius value for an Ellipse, but found "
                   "'%s' in an STC-S description: '%s'.", status, word,
                   ContextFragment( con, &fbuf, status ) );
      }

/* Store the property value in the KeyMap. */
      astMapPut0C( props, "RADIUS1", word, NULL );

/* Get the second radius value . */
      word = GetNextWord( this, con, status );
      val = astChr2Double( word );
      if( val == AST__BAD && astOK ) {
         astError( AST__BADIN, "astRead(StcsChan): Expected the second "
                   "radius value for an Ellipse, but found "
                   "'%s' in an STC-S description: '%s'.", status, word,
                   ContextFragment( con, &fbuf, status ) );
      }

/* Store the property value in the KeyMap. */
      astMapPut0C( props, "RADIUS2", word, NULL );

/* Get the position angle value. */
      word = GetNextWord( this, con, status );
      val = astChr2Double( word );
      if( val == AST__BAD && astOK ) {
         astError( AST__BADIN, "astRead(StcsChan): Expected the position "
                   "angle value for an Ellipse, but found "
                   "'%s' in an STC-S description: '%s'.", status, word,
                   ContextFragment( con, &fbuf, status ) );
      }

/* Store the property value in the KeyMap. */
      astMapPut0C( props, "POSANGLE", word, NULL );

/* Get the next word. */
      word = GetNextWord( this, con, status );



/* If we are currently looking for information needed to create a spatial
   Box ... */
   } else if( spaceid == BOX_ID ) {

/* Get a centre value for every space axis. */
      nc = 0;
      for( iaxis = 0; iaxis < naxes; iaxis++ ) {
         vals[ iaxis ] = astChr2Double( word );
         if( vals[ iaxis ] == AST__BAD && astOK ) {
            astError( AST__BADIN, "astRead(StcsChan): Expected another "
                      "centre value for a Box, but found "
                      "'%s' in an STC-S description: '%s'.", status,
                      word, ContextFragment( con, &fbuf, status ) );
         }
         prop = astAppendString( prop, &nc, word );
         prop = astAppendString( prop, &nc, " " );
         word = GetNextWord( this, con, status );
      }

/* Remove the trailing space, and store the property value in the KeyMap. */
      if( prop && nc > 0 ) {
         prop[ nc - 1 ] = 0;
         astMapPut0C( props, "CENTRE", prop, NULL );
         astMapPut1D( props, "DCENTRE", naxes, vals, NULL );
      }

/* Get bsize value for every space axis. */
      nc = 0;
      for( iaxis = 0; iaxis < naxes; iaxis++ ) {
         vals[ iaxis ] = astChr2Double( word );
         if( vals[ iaxis ] == AST__BAD && astOK ) {
            astError( AST__BADIN, "astRead(StcsChan): Expected another "
                      "'bsize' value for a Box, but found "
                      "'%s' in an STC-S description: '%s'.", status,
                      word, ContextFragment( con, &fbuf, status ) );
         }
         prop = astAppendString( prop, &nc, word );
         prop = astAppendString( prop, &nc, " " );
         word = GetNextWord( this, con, status );
      }

/* Remove the trailing space, and store the property value in the KeyMap. */
      if( prop && nc > 0 ) {
         prop[ nc - 1 ] = 0;
         astMapPut0C( props, "BSIZE", prop, NULL );
         astMapPut1D( props, "DBSIZE", naxes, vals, NULL );
      }


/* If we are currently looking for information needed to create a spatial
   Polygon ... */
   } else if( spaceid == POLYGON_ID ) {

/* Read the first vertex into a dynamically allocated array. */
      temp = astMalloc( sizeof( *temp )*naxes );
      if( temp ) {
         nc = 0;
         p = temp;
         for( iaxis = 0; iaxis < naxes; iaxis++,p++ ) {
            val = astChr2Double( word );
            if( val == AST__BAD && astOK ) {
               astError( AST__BADIN, "astRead(StcsChan): Expected another "
                      "vertex value for a Polygon, but found "
                      "'%s' in an STC-S description: '%s'.", status,
                      word, ContextFragment( con, &fbuf, status ) );
            } else {
               *p = val;
            }
            prop = astAppendString( prop, &nc, word );
            prop = astAppendString( prop, &nc, " " );
            word = GetNextWord( this, con, status );
         }

/* Loop round reading remaining vertices, expanding the array as needed. */
         nvert = 1;
         val = astChr2Double( word );
         while( val != AST__BAD && astOK ) {

            temp = astGrow( temp, naxes*( nvert + 1 ), sizeof( *temp ) );
            if( astOK ) {
               p = temp + naxes*nvert;

               for( iaxis = 0; iaxis < naxes; iaxis++, p++ ) {
                  if( val == AST__BAD && astOK ) {
                     astError( AST__BADIN, "astRead(StcsChan): Expected "
                               "another vertex value for a Polygon, but "
                               "found '%s' in an STC-S description: '%s'.",
                               status, word, ContextFragment( con, &fbuf,
                                                              status ) );
                  } else {
                     *p = val;
                  }
                  prop = astAppendString( prop, &nc, word );
                  prop = astAppendString( prop, &nc, " " );
                  word = GetNextWord( this, con, status );
                  val = astChr2Double( word );
               }
               nvert++;
            }
         }

/* Remove the trailing space, and store the property value in the KeyMap. */
         if( prop && nc > 0 ) {
            prop[ nc - 1 ] = 0;
            astMapPut0C( props, "VERTICES", prop, NULL );
            astMapPut1D( props, "DVERTICES", naxes*nvert, temp, NULL );
         }
         temp = astFree( temp );
      }



/* If we are currently looking for information needed to create a spatial
   Convex ... */
   } else if( spaceid == CONVEX_ID ) {
      astError( AST__BADIN, "astRead(StcsChan): A Convex was found "
                "within an STC-S description ('Convex' regions "
                "are not yet supported by AST): %s", status,
                ContextFragment( con, &fbuf, status ) );



/* If we are currently looking for information needed to create a spatial
   Position ... */
   } else if( spaceid == POSITION_ID ) {

/* Get a value for every space axis. */
      nc = 0;
      for( iaxis = 0; iaxis < naxes; iaxis++ ) {
         vals[ iaxis ] = astChr2Double( word );
         if( vals[ iaxis ] == AST__BAD && astOK ) {
            astError( AST__BADIN, "astRead(StcsChan): Expected another "
                      "axis value for a space Position, but found "
                      "'%s' in an STC-S description: '%s'.", status,
                      word, ContextFragment( con, &fbuf, status ) );
         }
         prop = astAppendString( prop, &nc, word );
         prop = astAppendString( prop, &nc, " " );
         word = GetNextWord( this, con, status );
      }

/* Remove the trailing space, and store the property value in the KeyMap. */
      if( prop && nc > 0 ) {
         prop[ nc - 1 ] = 0;
         astMapPut0C( props, "POSITION", prop, NULL );
         astMapPut1D( props, "DPOSITION", naxes, vals, NULL );
      }


/* All remaining space id values require the argument list to be enclosed
   in parentheses. Report an error if the current word does not start
   with an opening parenthesis. */
   } else if( *word != '(' && astOK ) {
      astError( AST__BADIN, "astRead(StcsChan): Expected an opening "
                "parenthesis but found '%s' in an STC-S description: '%s'.",
                status, word, ContextFragment( con, &fbuf, status ) );

/* Skip over the opening parenthesis. If the first word consists of just the
   opening parenthesis, get the next word.  */
   } else {
      if( *(++word) == 0 ) word = GetNextWord( this, con, status );

/* Loop round all regions included in the compound region. */
      nreg = 0;
      while( astOK ) {

/* If the next word starts with a closing parenthesis, we have reached
   the end of the argument list. */
         if( *word == ')' ) {

/* Skip over the closing parenthesis. If the word consists of just the
   closing parenthesis, get the next word.  */
            if( *(++word) == 0 ) word = GetNextWord( this, con, status );

/* Leave the loop. */
            break;
         }

/* Identify the region type from the current word. */
         new_spaceid = SpaceId( word, status );
         if( new_spaceid == NULL_ID && astOK ) {
            astError( AST__BADIN, "astRead(StcsChan): Expected a "
                      "CoordinateArea or a closing parenthesis but found "
                      "'%s' in an STC-S description: '%s'.", status, word,
                      ContextFragment( con, &fbuf, status ) );
         }

/* Create a new KeyMap to store the properties of the new region. Store
   this new KeyMap in the supplied KeyMap using a key of the form
   "REGION<n>". */
         new_props = astKeyMap( " ", status );
         astMapPut0C( new_props, "ID", word, NULL );
         sprintf( key, "REGION%d", ++nreg );
         astMapPut0A( props, key, new_props, NULL );

/* Get the next word (i.e. the first word of the argument list for the
   region). */
         word = GetNextWord( this, con, status );

/* Call this function recursively to read the argument list. */
         word = ReadSpaceArgs( this, word, new_spaceid, naxes, con,
                               new_props, status );

/* Free resources. */
         new_props = astAnnul( new_props );
      }

/* Store the number of regions in the supplied KeyMap. */
      astMapPut0I( props, "NREG", nreg, NULL );

/* Report an error if an in appropriate number of argument Regions were
   supplied. */
      if( spaceid == UNION_ID ) {
         if( nreg < 2 && astOK ){
            astError( AST__BADIN, "astRead(StcsChan): Less than two "
                      "CoordinateAreas found within a 'Union' element in an "
                      "STC-S description: '%s'.", status,
                      ContextFragment( con, &fbuf, status ) );
         }

      } else if( spaceid == INTERSECTION_ID ) {
         if( nreg < 2 && astOK ){
            astError( AST__BADIN, "astRead(StcsChan): Less than two "
                      "CoordinateAreas found within an 'Intersection' element "
                      "in an STC-S description: '%s'.", status,
                      ContextFragment( con, &fbuf, status ) );
         }

      } else if( spaceid == DIFFERENCE_ID ) {
         if( nreg != 2 && astOK ){
            astError( AST__BADIN, "astRead(StcsChan): %d CoordinateArea(s) "
                      "found within a 'Difference' element in an STC-S "
                      "description: '%s'.", status, nreg,
                      ContextFragment( con, &fbuf, status ) );
         }


      } else if( spaceid == NOT_ID ) {
         if( nreg != 1 && astOK ){
            astError( AST__BADIN, "astRead(StcsChan): %d CoordinateAreas "
                      "found within a 'Not' element in an STC-S description: "
                      "'%s'.", status, nreg,
                      ContextFragment( con, &fbuf, status ) );
         }

/* Report an error for unknown spaceid values */
      } else if( astOK ) {
         astError( AST__INTER, "astRead(StcsChan): Illegal 'spaceid' value "
                   "passed to function ReadSpaceArgs (internal AST "
                   "programming error).", status );
      }
   }

/* Free resources */
   if( prop ) prop = astFree( prop );

/* Return a pointer to the next word to be interpreted. */
   return word;
}

static void SetAttrib( AstObject *this_object, const char *setting, int *status ) {
/*
*  Name:
*     SetAttrib

*  Purpose:
*     Set an attribute value for a StcsChan.

*  Type:
*     Private function.

*  Synopsis:
*     #include "stcschan.h"
*     void SetAttrib( AstObject *this, const char *setting )

*  Class Membership:
*     StcsChan member function (over-rides the astSetAttrib protected
*     method inherited from the Channel class).

*  Description:
*     This function assigns an attribute value for a StcsChan, the
*     attribute and its value being specified by means of a string of
*     the form:
*
*        "attribute= value "
*
*     Here, "attribute" specifies the attribute name and should be in
*     lower case with no white space present. The value to the right
*     of the "=" should be a suitable textual representation of the
*     value to be assigned and this will be interpreted according to
*     the attribute's data type.  White space surrounding the value is
*     only significant for string attributes.

*  Parameters:
*     this
*        Pointer to the StcsChan.
*     setting
*        Pointer to a null terminated string specifying the new attribute
*        value.
*/

/* Local Variables: */
   AstStcsChan *this;          /* Pointer to the StcsChan structure */
   int ival;                   /* Integer attribute value */
   int len;                    /* Length of setting string */
   int nc;                     /* Number of characters read by "astSscanf" */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain a pointer to the StcsChan structure. */
   this = (AstStcsChan *) this_object;

/* Obtain the length of the setting string. */
   len = (int) strlen( setting );

/* Test for each recognised attribute in turn, using "astSscanf" to parse
   the setting string and extract the attribute value (or an offset to
   it in the case of string values). In each case, use the value set
   in "nc" to check that the entire string was matched. Once a value
   has been obtained, use the appropriate method to set it. */

/* StcsArea. */
/* --------- */
   if ( nc = 0,
               ( 1 == astSscanf( setting, "stcsarea= %d %n", &ival, &nc ) )
               && ( nc >= len ) ) {
      astSetStcsArea( this, ival );

/* StcsCoords. */
/* ----------- */
   } else if ( nc = 0,
               ( 1 == astSscanf( setting, "stcscoords= %d %n", &ival, &nc ) )
               && ( nc >= len ) ) {
      astSetStcsCoords( this, ival );

/* StcsProps. */
/* ----------- */
   } else if ( nc = 0,
               ( 1 == astSscanf( setting, "stcsprops= %d %n", &ival, &nc ) )
               && ( nc >= len ) ) {
      astSetStcsProps( this, ival );

/* StcsLength */
/* ----------*/
   } else if ( nc = 0,
        ( 1 == astSscanf( setting, "stcslength= %d %n", &ival, &nc ) )
        && ( nc >= len ) ) {
      astSetStcsLength( this, ival );

/* If the attribute is still not recognised, pass it on to the parent
   method for further interpretation. */
   } else {
      (*parent_setattrib)( this_object, setting, status );
   }
}

static void SetUnc( AstRegion *reg1, AstRegion *reg2, AstFrame *frm,
                    int is_skyframe, double scale, double *error, int nax,
                    int *status ){
/*
*  Name:
*     SetUnc

*  Purpose:
*     Store an uncertainty Box with a supplied Region.

*  Type:
*     Private function.

*  Synopsis:
*     #include "stcschan.h"
*     void SetUnc( AstRegion *reg1, AstRegion *reg2, AstFrame *frm,
*                  int is_skyframe, double scale, double *error, int nax,
*                  int *status )

*  Class Membership:
*     StcsChan member function

*  Description:
*     This function creates a new Box with dimensions specified by the
*     values in the "error" array, centred on a representative position
*     within one of the supplied Regions, and then stores the Box as the
*     uncertainty Region within both the supplied Regions.

*  Parameters:
*     reg1
*        Pointer to a Region to which the error values relate.
*     reg2
*        Pointer to another Region to which the error values relate.
*     frm
*        Pointer to the Frame encapsulated by both Regions.
*     is_skyframe
*        Should be non-zero if "frm" is a SkyFrame.
*     scale
*        A scale factor to apply to the error values before using them.
*     error
*        Pointer to an array of RMS error values, one for each axis in
*        "frm". These are modified on exit. For a SkyFrame, both values
*        (including the longitude axis value) should be given as an
*        arc-distance. This function will convert the arc-distance to
*        a longitude increment using a representative latitude for the
*        region.
*     nax
*        The numner of axes in "frm".
*     status
*        Pointer to the inherited status variable.

*/

/* Local Variables: */
   AstBox *unc;           /* Uncertainty box */
   double dist;           /* Diagonal length of Region bounding box */
   double lbnd[ 6 ];      /* Lower bounds of Region bounding box */
   double spos1[ 6 ];     /* A representative position in the Region */
   double spos2[ 6 ];     /* A second position in the Region */
   double ubnd[ 6 ];      /* Upper bounds of Region bounding box */
   int i;                 /* Axis index */

/* Check the global error status. Also check an error value was supplied,
   and at least one of the Region pointers is not NULL. */
   if ( !astOK || error[ 0 ] == AST__BAD || ( !reg1 && !reg2 ) ) return;

/* We need a representative position within the region. First get the
   coordinates at opposite corners of the region bounding box. */
   astRegBaseBox( reg1 ? reg1 : reg2, lbnd, ubnd );

/* Find the diagonal length of the bounding box. */
   dist = astDistance( frm, lbnd, ubnd );

/* Offset away from one corner towards the other by half the diagonal
   length. The resulting position returned in spos1 is our representative
   position for the region. */
   astOffset( frm, lbnd, ubnd, dist/2, spos1 );

/* Scale the error values */
   for( i = 0; i < nax; i++ ) error[ i ] *= scale;

/* If the region is defined within a SkyFrame, the supplied longitude
   error value will be an arc-distance value. But we need a longitude
   increment to create an uncertainty Region, so do the conversion. */
   if( is_skyframe ) {

/* Offset away from the representative position found above along the
   first (i.e. longitude) axis by an arc-distance given by the Error
   value. */
      (void) astOffset2( frm, spos1, AST__DPIBY2, error[ 0 ], spos2 );

/* Find the positive axis increment along the first axis. */
      error[ 0 ] = astAxDistance( frm, 1, spos1[ 0 ], spos2[ 0 ] );
      if( error[ 0 ] != AST__BAD ) error[ 0 ] = fabs( error[ 0 ] );
   }

/* The uncertainty Region will be a Box centred at the representative
   position found above. Modify the "error" array to hold the corner
   axis values. */
   for( i = 0; i < nax; i++ ) error[ i ] += spos1[ i ];

/* Create the box, and store it as the uncertainty Region in the supplied
   Region. */
   unc = astBox( frm, 0, spos1, error, NULL, " ", status );
   if( reg1 ) astSetUnc( reg1, unc );
   if( reg2 ) astSetUnc( reg2, unc );

/* Free resources. */
   unc = astAnnul( unc );
}

static AstPointList *SinglePointList( AstFrame *frm, double *pos,
                                      AstRegion *unc, int *status){
/*
*  Name:
*     SinglePointList

*  Purpose:
*     Create a PointList holding a single point.

*  Type:
*     Private function.

*  Synopsis:
*     #include "stcschan.h"
*     AstPointList *SinglePointList( AstFrame *frm, double *pos,
*                                    AstRegion *unc, int *status )

*  Class Membership:
*     StcsChan member function

*  Description:
*     This function creates a new PointList holding a single supplied
*     position.

*  Parameters:
*     frm
*        Pointer to the Frame in which the PointList is defined.
*     pos
*        Array holding the position. The length of this array must equal
*        the number of axes in "frm".
*     unc
*        Pointer to an uncertainty Region to associate with the new
*        PointList, or NULL.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     A pointer to the new PointList. NULL is returned if an error has
*     already occurred, of if this function fails for any reason.
*/

/* Local Variables: */
   AstPointList *result;   /* Returned pointer. */
   AstPointSet *pset;      /* PointSet holding axis values */
   double **ptr;           /* Pointer to PointSet data arrays */
   int i;                  /* Axis index */
   int nax;                /* Number of axes */

/* Initialise. */
   result = NULL;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Get he number of axes. */
   nax = astGetNaxes( frm );

/* Create a PointSet to hold the supplied point, and get a pointer to its
   data arrays. */
   pset = astPointSet( 1, nax, "", status );
   ptr = astGetPoints( pset );
   if( astOK ) {

/* Copy the supplied axis values into the PointSet data arrays. */
      for( i = 0; i < nax; i++ ) ptr[ i ][ 0 ] = pos[ i ];

/* Create the PointList. */
      result = astPointList( frm, pset, unc, "", status );
   }

/* Free resources */
   pset = astAnnul( pset );

/* Return the result. */
   return result;
}

static void SinkWrap( void (* sink)( const char * ), const char *line, int *status ) {
/*
*  Name:
*     SinkWrap

*  Purpose:
*     Wrapper function to invoke a C StcsChan sink function.

*  Type:
*     Private function.

*  Synopsis:
*     #include "stcschan.h"
*     void SinkWrap( void (* sink)( const char * ), const char *line, int *status )

*  Class Membership:
*     StcsChan member function.

*  Description:
*     This function invokes the sink function whose pointer is
*     supplied in order to write an output line to an external data
*     store.

*  Parameters:
*     sink
*        Pointer to a sink function, whose single parameter is a
*        pointer to a const, null-terminated string containing the
*        text to be written, and which returns void. This is the form
*        of StcsChan sink function employed by the C language interface
*        to the AST library.
*     status
*        Pointer to the inherited status variable.
*/

/* Check the global error status. */
   if ( !astOK ) return;

/* Invoke the sink function. */
   ( *sink )( line );
}

static char *SourceWrap( const char *(* source)( void ), int *status ) {
/*
*  Name:
*     SourceWrap

*  Purpose:
*     Wrapper function to invoke a C StcsChan source function.

*  Type:
*     Private function.

*  Synopsis:
*     #include "stcschan.h"
*     char *SourceWrap( const char *(* source)( void ), int *status )

*  Class Membership:
*     StcsChan member function.

*  Description:
*     This function invokes the source function whose pointer is
*     supplied in order to read the next input line from an external
*     data store. It then returns a pointer to a dynamic string
*     containing a copy of the text that was read.

*  Parameters:
*     source
*        Pointer to a source function, with no parameters, that
*        returns a pointer to a const, null-terminated string
*        containing the text that it read. This is the form of StcsChan
*        source function employed by the C language interface to the
*        AST library.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     A pointer to a dynamically allocated, null terminated string
*     containing a copy of the text that was read. This string must be
*     freed by the caller (using astFree) when no longer required.
*
*     A NULL pointer will be returned if there is no more input text
*     to read.

*  Notes:
*     - A NULL pointer value will be returned if this function is
*     invoked with the global error status set or if it should fail
*     for any reason.
*/

/* Local Variables: */
   char *result;                 /* Pointer value to return */
   const char *line;             /* Pointer to input line */

/* Initialise. */
   result = NULL;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Invoke the source function to read the next input line and return a
   pointer to the resulting string. */
   line = ( *source )();

/* If a string was obtained, make a dynamic copy of it and save the
   resulting pointer. */
   if ( line ) result = astString( line, (int) strlen( line ) );

/* Return the result. */
   return result;
}

static int SpaceId( const char *word, int *status ){
/*
*  Name:
*     SpaceId

*  Purpose:
*     Return the integer identifier for a given textual space identifier.

*  Type:
*     Private function.

*  Synopsis:
*     #include "stcschan.h"
*     int SpaceId( const char *word, int *status )

*  Class Membership:
*     StcsChan member function

*  Description:
*     This function returns an integer identifier for the given space
*     identifier.

*  Parameters:
*     word
*        The word holding the textual space identifier.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     The integer space identifier, or NULL_ID if the supplied word was
*     not a known space identifier.

*/


/* Local Variables: */
   int spaceid;            /* Returned identifier */

/* Check inherited status */
   if( !astOK ) return NULL_ID;

   if( astChrMatch( word, "PositionInterval" ) ) {
      spaceid = POSITION_INTERVAL_ID;

   } else if( astChrMatch( word, "AllSky" ) ) {
      spaceid = ALLSKY_ID;

   } else if( astChrMatch( word, "Circle" ) ) {
      spaceid = CIRCLE_ID;

   } else if( astChrMatch( word, "Ellipse" ) ) {
      spaceid = ELLIPSE_ID;

   } else if( astChrMatch( word, "Box" ) ) {
      spaceid = BOX_ID;

   } else if( astChrMatch( word, "Polygon" ) ) {
      spaceid = POLYGON_ID;

   } else if( astChrMatch( word, "Convex" ) ) {
      spaceid = CONVEX_ID;

   } else if( astChrMatch( word, "Union" ) ) {
      spaceid = UNION_ID;

   } else if( astChrMatch( word, "Intersection" ) ) {
      spaceid = INTERSECTION_ID;

   } else if( astChrMatch( word, "Difference" ) ) {
      spaceid = DIFFERENCE_ID;

   } else if( astChrMatch( word, "Not" ) ) {
      spaceid = NOT_ID;

   } else if( astChrMatch( word, "Position" ) ) {
      spaceid = POSITION_ID;

   } else {
      spaceid = NULL_ID;
   }

/* Return the integer space identifier. */
   return spaceid;
}

static void StoreTimeProp( AstKeyMap *props, AstTimeFrame *frm,
                           const char *key, double value, int *status ){
/*
*  Name:
*     StoreTimeProp

*  Purpose:
*     Store a time value as an STC-S property, using the existing format.

*  Type:
*     Private function.

*  Synopsis:
*     #include "stcschan.h"
*     void StoreTimeProp( AstKeyMap *props, AstTimeFrame *frm,
*                         const char *key, double value, int *status )

*  Class Membership:
*     StcsChan member function.

*  Description:
*     This function formats the supplied time value and stores it in
*     the "props" KeyMap, using the supplied key name. If the KeyMap
*     already contains an entry for the given key, the new value is
*     written using the same format. Otherwise, the new value is written
*     as an ISO date and time string.

*  Parameters:
*     props
*        Pointer to the KeyMap in which to store the time value.
*     frm
*        Pointer to a TimeFrame that can be used to format the time value.
*     key
*        Pointer to a string holding the property name associated with
*        the time value.
*     value
*        The time value, in the system described by "frm".
*     status
*        Pointer to the inherited status variable.

*/

/* Local Variables: */
   AstFrame *fmtfrm;    /* Frame defining Format/System for formatted value */
   AstFrame *fs;        /* FrameSet connecting Frames */
   const char *fmttxt;  /* Formatted text */
   const char *oldval;  /* Pointer to old formatted time value */
   const char *p;       /* Pointer to next character in formatted value */
   double fmtval;       /* The time value in the formatting system */
   int ndp;             /* Number of decimal places in formatted value */

/* Check the global error status. */
   if ( !astOK ) return;

/* We want a TimeFrame (fmtfrm) that describes how to format the time
   value. If the Format attribute of the supplied TimeFrame has been
   set, use it (and the current System). So just take a clone of the
   supplied frame pointer. */
   if( astTestFormat( frm, 0 ) ) {
      fmtfrm = astClone( frm );

/* If the Format attribute has not been set, we create a copy of the
   supplied TimeFrame, and set its System and Format attributes to
   produce the required format. */
   } else {
      fmtfrm = astCopy( frm );

/* If the KeyMap contains an entry for the specified key, determine the
   format of the time string it contains. */
      if( astMapGet0C( props, key, &oldval ) && oldval ) {

/* See how many digits there are after the decimal place */
         p = strchr( oldval, '.' );
         ndp = 0;
         if( p ) {
            while( *(++p) ) {
               if( isdigit( *p ) ) {
                  ndp++;
               } else {
                  break;
               }
            }
         }

/* If the string starts with "JD", the time is formatted as a numerical
   Julian date. */
         if( !strncmp( oldval, "JD", 2 ) ) {
            astSetSystem( fmtfrm, AST__JD );
            if( ndp > 0 ) {
               astSet( fmtfrm, "Format=JD %%.%df", status, ndp );
            } else {
               astSetFormat( fmtfrm, 0, "JD %d" );
            }

/* If the string starts with "MJD", the time is formatted as a numerical
   Modified Julian date. */
         } else if( !strncmp( oldval, "MJD", 3 ) ) {
            astSetSystem( fmtfrm, AST__MJD );
            if( ndp > 0 ) {
               astSet( fmtfrm, "Format=MJD %%.%df", status, ndp );
            } else {
               astSetFormat( fmtfrm, 0, "MJD %d" );
            }

/* Otherwise, the current word should be an ISO date. See how many
   decimal paces in the seconds field there are (if any). */
         } else {
            astSet( fmtfrm, "Format=iso.%dT", status, ndp );
         }

/* If the KeyMap does not contain an entry for the specified key, an
   ISO date/time string with 1 decimal place in the seconds field
   is used. */
      } else {
         astSetFormat( fmtfrm, 0, "iso.1T" );
      }
   }

/* Ensure the displayed value is an abolute value. */
   astClearTimeOrigin( fmtfrm );

/* Convert the supplied time value into the required system. */
   fs = astConvert( frm, fmtfrm, "" );
   astTran1( fs, 1, &value, 1, &fmtval );

/* Format the value. */
   fmttxt = astFormat( fmtfrm, 0, fmtval );

/* Store it in the KeyMap. */
   astMapPut0C( props, key, fmttxt, NULL );

/* Free resources. */
   fs = astAnnul( fs );
   fmtfrm = astAnnul( fmtfrm );
}

static int TestAttrib( AstObject *this_object, const char *attrib, int *status ) {
/*
*  Name:
*     TestAttrib

*  Purpose:
*     Test if a specified attribute value is set for a StcsChan.

*  Type:
*     Private function.

*  Synopsis:
*     #include "stcschan.h"
*     int TestAttrib( AstObject *this, const char *attrib, int *status )

*  Class Membership:
*     StcsChan member function (over-rides the astTestAttrib protected
*     method inherited from the Object class).

*  Description:
*     This function returns a boolean result (0 or 1) to indicate whether
*     a value has been set for one of a StcsChan's attributes.

*  Parameters:
*     this
*        Pointer to the StcsChan.
*     attrib
*        Pointer to a null terminated string specifying the attribute
*        name.  This should be in lower case with no surrounding white
*        space.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     One if a value has been set, otherwise zero.

*  Notes:
*     - A value of zero will be returned if this function is invoked
*     with the global status set, or if it should fail for any reason.
*/

/* Local Variables: */
   AstStcsChan *this;            /* Pointer to the StcsChan structure */
   int result;                   /* Result value to return */

/* Initialise. */
   result = 0;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Obtain a pointer to the StcsChan structure. */
   this = (AstStcsChan *) this_object;

/* Check the attribute name and test the appropriate attribute. */

   if ( !strcmp( attrib, "stcsarea" ) ) {
      result = astTestStcsArea( this );

   } else if ( !strcmp( attrib, "stcscoords" ) ) {
      result = astTestStcsCoords( this );

   } else if ( !strcmp( attrib, "stcsprops" ) ) {
      result = astTestStcsProps( this );

   } else if ( !strcmp( attrib, "stcslength" ) ) {
      result = astTestStcsLength( this );

/* If the attribute is still not recognised, pass it on to the parent
   method for further interpretation. */
   } else {
      result = (*parent_testattrib)( this_object, attrib, status );
   }

/* Return the result, */
   return result;
}

static int Write( AstChannel *this_channel, AstObject *object, int *status ) {
/*
*  Name:
*     Write

*  Purpose:
*     Write an Object to a StcsChan.

*  Type:
*     Private function.

*  Synopsis:
*     #include "stcschan.h"
*     int Write( AstChannel *this, AstObject *object, int *status )

*  Class Membership:
*     StcsChan member function (over-rides the astWrite method
*     inherited from the Channel class).

*  Description:
*     This function writes an Object to a StcsChan.

*  Parameters:
*     this
*        Pointer to the StcsChan.
*     object
*        Pointer to the Object which is to be written.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     The number of Objects written to the StcsChan by this invocation of
*     astWrite.

*  Notes:
*     - A value of zero will be returned if this function is invoked
*     with the AST error status set, or if it should fail for any
*     reason.
*/

/* Local Variables: */
   AstFrame *frm;            /* AREA Frame */
   AstFrameSet *fs;          /* FrameSet connecting AREA and COORDS */
   AstKeyMap *props;         /* A KeyMap holding the STC-S properties list */
   AstMapping *map;          /* Mapping connecting AREA and COORDS */
   AstObject *obj;           /* A temporary Object pointer */
   AstRegion *area;          /* The Region representing the STC CoordArea */
   AstRegion *coords;        /* The Region representing the STC Coords */
   AstRegion *new_coords;    /* COORDS Region mapped into frame of AREA */
   AstStcsChan *this;        /* Pointer to the StcsChan structure */
   astDECLARE_GLOBALS        /* Declare the thread specific global data */
   const char *class;        /* Pointer to string holding object class */
   const char *errclass;     /* Type of the failed entry */
   const char *errname;      /* Name of the failed entry */
   const char *method;       /* Pointer to string holding calling method */
   const char *wantclass;    /* The expected type */
   int ret;                  /* Number of objects read */

/* Initialise. */
   ret = 0;

/* Check the global error status. */
   if ( !astOK ) return ret;

/* Get a pointer to the structure holding thread-specific global data. */
   astGET_GLOBALS(this_channel);

/* Obtain a pointer to the StcsChan structure. */
   this = (AstStcsChan *) this_channel;

/* Store the calling method, and object class. */
   method = "astWrite";
   class = astGetClass( this );

/* Initialise */
   area = NULL;
   coords = NULL;
   props = NULL;

/* If the supplied Object is a Region, we will use it to define the AREA
   properties. */
   if( astIsARegion( object ) ) {
      area = (AstRegion *) astClone( object );

/* If the supplied Object is a KeyMap... */
   } else if( astIsAKeyMap( object ) ) {
      errname = NULL;
      wantclass = NULL;
      errclass = NULL;

/* If the supplied KeyMap contains an entry with key "AREA", and if it is
   a Region, use it to define the AREA properties. */
      if( astMapGet0A( (AstKeyMap *) object, "AREA", &obj ) ) {
         if( astIsARegion( obj ) ) {
            area = (AstRegion *) obj;
         } else {
            wantclass = "Region";
            errclass = astGetClass( obj );
            errname = "AREA";
            obj = astAnnul( obj );
         }
      }

/* If the supplied KeyMap contains an entry with key "COORDS", and if it is
   a Region, use it to define the COORDS properties. */
      if( astMapGet0A( (AstKeyMap *) object, "COORDS", &obj ) ) {
         if( astIsARegion( obj ) ) {
            coords = (AstRegion *) obj;
         } else {
            wantclass = "Region";
            errclass = astGetClass( obj );
            errname = "COORDS";
            obj = astAnnul( obj );
         }
      }

/* If the supplied KeyMap contains an entry with key "PROPS", and if it is
   a KeyMap, use it to define values for the properties that cannot be
   determined from the supplied Regions (Resolution, PixSize, etc). */
      if( astMapGet0A( (AstKeyMap *) object, "PROPS", &obj ) ) {
         if( astIsAKeyMap( obj ) ) {
            props = (AstKeyMap *) obj;
         } else {
            wantclass = "KeyMap";
            errclass = astGetClass( obj );
            errname = "PROPS";
            obj = astAnnul( obj );
         }
      }

/* If the supplied KeyMap contains an entry with any of the keys
   "TIME_PROPS", "SPACE_PROPS", "SPECTRAL_PROPS" or "REDSHIFT_PROPS",
   use the supplied KeyMap to define values for all properties. */
      if( astMapGet0A( (AstKeyMap *) object, "TIME_PROPS", &obj ) ||
          astMapGet0A( (AstKeyMap *) object, "SPACE_PROPS", &obj ) ||
          astMapGet0A( (AstKeyMap *) object, "SPECTRAL_PROPS", &obj ) ||
          astMapGet0A( (AstKeyMap *) object, "REDSHIFT_PROPS", &obj ) ) {
         props = astClone( object );
      }

/* Report an error if the Object in the keymap has the wrong type. */
      if( errname && astOK ) {
         astAddWarning( this, 1, "The supplied KeyMap contains a %s "
                        "called '%s'. But '%s' should be a %s "
                        "(programming error).", method, status,
                        errclass, errname, errname, wantclass );
      }

/* Report an error if the keymap contains none of the above. */
      if( !area && !coords && !props && astOK ) {
         astAddWarning( this, 1, "The supplied KeyMap does not "
                        "contains anything that can be written out "
                        "through a %s.", method, status, class );
      }

/* If both COORDS and AREA were supplied, ensure they are in the same
   Frame by mapping the COORDS Region into the Frame of the AREA Region. */
      if( area && coords ) {
         fs = astConvert( coords, area, " " );
         if( fs ) {
            map = astGetMapping( fs, AST__BASE, AST__CURRENT );
            frm = astGetFrame( fs, AST__CURRENT );

            new_coords = astMapRegion( coords, map, frm );

            map = astAnnul( map );
            frm = astAnnul( frm );
            coords = astAnnul( coords );
            fs = astAnnul( fs );

            coords = new_coords;

         } else if( astOK ){
            astAddWarning( this, 1, "Cannot convert between the co-ordinate "
                           "frame of the COORDS Region and the co-ordinate "
                           "frame of the AREA Region.", method, status );
         }
      }

/* Report an error if the supplied object is neither a KeyMap nor a
   Region. */
   } else if( astOK ) {
      astAddWarning( this, 1, "Failed to write out a %s through a %s. "
                     "The %s class cannot be used to write out a %s.",
                     method, status, astGetClass( object ), class, class,
                     astGetClass( object ) );
   }


/* If we do not have a KeyMap in which to store the STC-S properties,
   create one now. */
   if( astOK ) {
      if( ! props ) props = astKeyMap ( " ", status );

/* Determine the set of STC-S properties that describe the COORDS Region,
   and add them into the properties keymap, over-writing any values for the
   same properties that are already in the props keymap. */
      ret = coords ? WriteRegion( this, coords, props, status ) : 1;

/* Determine the set of STC-S properties that describe the AREA Region,
   and add them into the properties keymap, over-writing any values for the
   same properties that are already in the props keymap. NB, we need to
   do AREA after COORDS so that the sub-phrase identifier implied by the
   AREA is used in preference to that implied by the COORDS. */
      if( area && ret ) ret = WriteRegion( this, area, props, status );

/* Convert the properties list into text and write it out through the
   parent Channel's sink function. */
      if( ret ) WriteProps( this, props, status );
   }

/* Free resources. */
   if( area ) area = astAnnul( area );
   if( coords ) coords = astAnnul( coords );
   if( props ) props = astAnnul( props );

/* If an error has occurred, return zero. */
   if( !astOK ) ret = 0;

/* Return the answer. */
   return ret;
}

static void WriteProps( AstStcsChan *this, AstKeyMap *props, int *status ){
/*
*  Name:
*     WriteProps

*  Purpose:
*     Write out a set of STC-S properties to the sink function.

*  Type:
*     Private function.

*  Synopsis:
*     #include "stcschan.h"
*     void WriteProps( AstStcsChan *this, AstKeyMap *props, int *status )

*  Class Membership:
*     StcsChan member function

*  Description:
*     This function converts the STC-S properties supplied in a KeyMap
*     into text, and writes the text out through the sink function associated
*     with the parent Channel.

*  Parameters:
*     this
*        Pointer to the StcsChan.
*     props
*        Pointer to the KeyMap holding the STC-S properties.
*     status
*        Pointer to the inherited status variable.

*/

/* Local Variables: */
   AstKeyMap *spprops;     /* Sub-phrase properties */
   AstObject *obj;         /* Generic Object pointer */
   char *line;             /* Dynamically allocated buffer for output text */
   const char *id;         /* Sub-phrase identifier */
   const char *prefix;     /* Prefix for property value */
   int nc;                 /* Number of characters in "line" */
   int pretty;             /* Include new-lines and indentation in returned text? */
   int crem;               /* Character remaining on current output line */
   int linelen;            /* Line length */

/* Check the global error status. */
   if ( !astOK ) return;

/* Initialise things. */
   nc = 0;
   line = NULL;

/* See if indentation and new-lines are to be added to the output text to
   make it look pretty. */
   pretty = astGetIndent( this );

/* If so, get the line length to use, and initialise the number of
   remaining characters in the current output line. */
   if( pretty ) {
      linelen = astGetStcsLength( this );
   } else {
      linelen = 0;
   }
   crem = linelen;

/* Add each word in the time sub-phrase into the output buffer, in the
   order defined by the STC-S standard. */
   if( astMapGet0A( props, "TIME_PROPS", &obj ) ) {
      spprops = (AstKeyMap *) obj;

      line = AddItem( this, spprops, "ID", NULL, line, &nc, &crem, linelen, status );
      astMapGet0C( spprops, "ID", &id );

      line = AddItem( this, spprops, "FILLFACTOR", "fillfactor ", line, &nc, &crem, linelen, status );
      line = AddItem( this, spprops, "TIMESCALE", NULL, line, &nc, &crem, linelen, status );
      line = AddItem( this, spprops, "REFPOS", NULL, line, &nc, &crem, linelen, status );
      line = AddItem( this, spprops, "START", NULL, line, &nc, &crem, linelen, status );
      line = AddItem( this, spprops, "STOP", NULL, line, &nc, &crem, linelen, status );

      prefix = !astChrMatch( id, "Time" ) ? "Time " : NULL;
      line = AddItem( this, spprops, "TIME", prefix, line, &nc, &crem, linelen, status );

      line = AddItem( this, spprops, "UNIT", "unit ", line, &nc, &crem, linelen, status );
      line = AddItem( this, spprops, "ERROR", "Error ", line, &nc, &crem, linelen, status );
      line = AddItem( this, spprops, "RESOLUTION", "Resolution ", line, &nc, &crem, linelen, status );
      line = AddItem( this, spprops, "PIXSIZE", "PixSize ", line, &nc, &crem, linelen, status );

      spprops = astAnnul( spprops );

/* Write out the time sub-phrase text through the Channel sink function. */
      if( pretty && astChrLen( line ) ) {
         astPutNextText( this, line );
         nc = 0;
         crem = linelen;
      }
   }

/* Add each word in the space sub-phrase into the output buffer, in the
   order defined by the STC-S standard. */
   if( astMapGet0A( props, "SPACE_PROPS", &obj ) ) {
      spprops = (AstKeyMap *) obj;

      line = AddItem( this, spprops, "ID", NULL, line, &nc, &crem, linelen, status );
      astMapGet0C( spprops, "ID", &id );

      line = AddItem( this, spprops, "FILLFACTOR", "fillfactor ", line, &nc, &crem, linelen, status );
      line = AddItem( this, spprops, "FRAME", NULL, line, &nc, &crem, linelen, status );
      line = AddItem( this, spprops, "REFPOS", NULL, line, &nc, &crem, linelen, status );
      line = AddItem( this, spprops, "FLAVOUR", NULL, line, &nc, &crem, linelen, status );

      line = PutRegionProps( this, spprops, id, (pretty ? 0 : -1), line, &nc,
                             &crem, linelen, status );

      prefix = !astChrMatch( id, "Position" ) ? "Position " : NULL;
      line = AddItem( this, spprops, "POSITION", prefix, line, &nc, &crem, linelen, status );
      line = AddItem( this, spprops, "UNIT", "unit ", line, &nc, &crem, linelen, status );
      line = AddItem( this, spprops, "ERROR", "Error ", line, &nc, &crem, linelen, status );
      line = AddItem( this, spprops, "RESOLUTION", "Resolution ", line, &nc, &crem, linelen, status );
      line = AddItem( this, spprops, "SIZE", "Size ", line, &nc, &crem, linelen, status );
      line = AddItem( this, spprops, "PIXSIZE", "PixSize ", line, &nc, &crem, linelen, status );

      spprops = astAnnul( spprops );

/* Write out the spatial sub-phrase text through the Channel sink function. */
      if( pretty && astChrLen( line ) ) {
         astPutNextText( this, line );
         nc = 0;
         crem = linelen;
      }
   }

/* Add each word in the spectral sub-phrase into the output buffer, in the
   order defined by the STC-S standard. */
   if( astMapGet0A( props, "SPECTRAL_PROPS", &obj ) ) {
      spprops = (AstKeyMap *) obj;

      line = AddItem( this, spprops, "ID", NULL, line, &nc, &crem, linelen, status );
      astMapGet0C( spprops, "ID", &id );

      line = AddItem( this, spprops, "FILLFACTOR", "fillfactor ", line, &nc, &crem, linelen, status );
      line = AddItem( this, spprops, "REFPOS", NULL, line, &nc, &crem, linelen, status );
      line = AddItem( this, spprops, "LOLIMIT", NULL, line, &nc, &crem, linelen, status );
      line = AddItem( this, spprops, "HILIMIT", NULL, line, &nc, &crem, linelen, status );

      prefix = !astChrMatch( id, "Spectral" ) ? "Spectral " : NULL;
      line = AddItem( this, spprops, "SPECTRAL", prefix, line, &nc, &crem, linelen, status );

      line = AddItem( this, spprops, "UNIT", "unit ", line, &nc, &crem, linelen, status );
      line = AddItem( this, spprops, "ERROR", "Error ", line, &nc, &crem, linelen, status );
      line = AddItem( this, spprops, "RESOLUTION", "Resolution ", line, &nc, &crem, linelen, status );
      line = AddItem( this, spprops, "PIXSIZE", "PixSize ", line, &nc, &crem, linelen, status );

      spprops = astAnnul( spprops );

/* Write out the spectral sub-phrase text through the Channel sink function. */
      if( pretty && astChrLen( line ) ) {
         astPutNextText( this, line );
         nc = 0;
         crem = linelen;
      }
   }

/* Add each word in the redshift sub-phrase into the output buffer, in the
   order defined by the STC-S standard. */
   if( astMapGet0A( props, "REDSHIFT_PROPS", &obj ) ) {
      spprops = (AstKeyMap *) obj;

      line = AddItem( this, spprops, "ID", NULL, line, &nc, &crem, linelen, status );
      astMapGet0C( spprops, "ID", &id );

      line = AddItem( this, spprops, "FILLFACTOR", "fillfactor ", line, &nc, &crem, linelen, status );
      line = AddItem( this, spprops, "REFPOS", NULL, line, &nc, &crem, linelen, status );
      line = AddItem( this, spprops, "TYPE", NULL, line, &nc, &crem, linelen, status );
      line = AddItem( this, spprops, "DOPPLERDEF", NULL, line, &nc, &crem, linelen, status );
      line = AddItem( this, spprops, "LOLIMIT", NULL, line, &nc, &crem, linelen, status );
      line = AddItem( this, spprops, "HILIMIT", NULL, line, &nc, &crem, linelen, status );

      prefix = !astChrMatch( id, "Redshift" ) ? "Redshift " : NULL;
      line = AddItem( this, spprops, "REDSHIFT", prefix, line, &nc, &crem, linelen, status );

      line = AddItem( this, spprops, "UNIT", "unit ", line, &nc, &crem, linelen, status );
      line = AddItem( this, spprops, "ERROR", "Error ", line, &nc, &crem, linelen, status );
      line = AddItem( this, spprops, "RESOLUTION", "Resolution ", line, &nc, &crem, linelen, status );
      line = AddItem( this, spprops, "PIXSIZE", "PixSize ", line, &nc, &crem, linelen, status );

      spprops = astAnnul( spprops );

/* Write out the redshift sub-phrase text through the Channel sink function. */
      if( pretty && astChrLen( line ) ) {
         astPutNextText( this, line );
         nc = 0;
         crem = linelen;
      }
   }

/* Write out any remaining text through the Channel sink function. */
   if( nc && astChrLen( line ) ) astPutNextText( this, line );

/* Free resources. */
   line = astFree( line );

}

static int WriteRegion( AstStcsChan *this, AstRegion *reg, AstKeyMap *props,
                        int *status ){
/*
*  Name:
*     WriteRegion

*  Purpose:
*     Convert a Region into a set of STC-S properties and store them in a
*     KeyMap.

*  Type:
*     Private function.

*  Synopsis:
*     #include "stcschan.h"
*     int WriteRegion( AstStcsChan *this, AstRegion *reg, AstKeyMap *props,
*                      int *status )

*  Class Membership:
*     StcsChan member function

*  Description:
*     This function attempts to convert the supplied Region nto a set of
*     STC-S properties, and stores them in the supplied KeyMap.

*  Parameters:
*     this
*        Pointer to the StcsChan being used.
*     reg
*        Pointer to the region to be converted.
*     props
*        Pointer to the KeyMap in which to store the STC-S properties.
*        On exit, each STC-S sub-phrase has an entry in this KeyMap,
*        and each of these entries has a value that is another KeyMap
*        holding the properties for the sub-phrase.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     A non-zero value is returned if the conversion was succesful, and
*     zero is returned otherwise.
*/

/* Local Variables: */
   AstFrame *efrm;         /* Pointer to encapsulated Frame */
   AstFrame *pfrm;         /* Pointer to primary Frame cntaining an axis */
   AstFrame *spfrm;        /* The sub-phrase Frame */
   AstKeyMap *spprops;     /* Sub-phrase properties */
   AstMapping *map;        /* Base->current Region Mapping */
   AstMapping *sreg;       /* Simplified Region */
   AstObject *obj;         /* Generic object pointer */
   AstRegion *spreg;       /* The sub-phrase Region */
   AstRegion *treg;        /* Temporary Region pointer */
   AstRegion *unc;         /* Uncertainty region */
   AstRegion *unca;        /* Adaptive uncertainty region */
   AstStdOfRestType sor;   /* StdOfRest attribute value */
   AstSystemType sys;      /* System attribute value */
   char *prop;             /* Formatted property string */
   char *unit1;            /* Pointer to string holding first axis unit */
   char buf[ 100 ];        /* Buffer for formatted values */
   char fmt[ 10 ];         /* Buffer for format specifier */
   const char *class;      /* Class name */
   const char *dom;        /* Domain name */
   const char *dopdef;     /* DopplerDef value */
   const char *flavour;    /* The STC-S flavour for the space frame */
   const char *q;          /* Pointer to next character */
   const char *tfrm;       /* STC-S string for Frame */
   const char *tsor;       /* STC-S string for RefPos */
   const char *tts;        /* Time scale label */
   const char *type;       /* Redshift Type value */
   const char *unit;       /* Unit string */
   double *pcen;           /* Pointer to Circle or ellipse centre */
   double equinox;         /* The required equinox value */
   double error;           /* Axis error value */
   double fill;            /* Fill factor */
   double lbnd[ 3 ];       /* Region lower bounds */
   double lim;             /* Unlimited bounds value */
   double p1[ 2 ];         /* End point of error line */
   double scale;           /* Factor for scaling Region values into required units */
   double ubnd[ 3 ];       /* Region upper bounds */
   int allthesame;         /* Do all axes have the same units? */
   int defdigs;            /* Default number of digits */
   int defs;               /* Include default values in output STC-S? */
   int i;                  /* Loop index */
   int issky;              /* Do the space axes form a SkyFrame? */
   int nax;                /* The number of axes */
   int nc;                 /* Number of characters in "prop" string */
   int nspace;             /* Number of space axes */
   int ok;                 /* Can the Region be written out? */
   int pax;                /* Index of axis in primary Frame */
   int redax;              /* The index of the redshift axis */
   int retain_units;       /* Retain the units/system in properties KeyMap? */
   int spaceax[ 3 ];       /* Indicies of the space axes */
   int spaceid;            /* Code for space sub-phrase identifier */
   int specax;             /* The index of the spectral axis */
   int timeax;             /* Index of time axis */
   int ts;                 /* Time scale identifier */

/* Check the global error status. */
   if ( !astOK ) return 0;

/* Initialise things to avoid comiler warnings. */
   sys = AST__BADSYSTEM;

/* Assume we can do the conversion. */
   ok = 1;

/* See if default values are to be included in the output. */
   defs = ( astGetFull( this ) > 0 );

/* STC-S requires that the spatial shape (circle, box. etc) refers to
   the coordinate system described by the STC-S. This is not quite like
   AST, in that the AST class type (Circle, Box, etc) defines the
   shape of the region in the base Frame, rather than the current Frame.
   So we can only write the Region out using STC-S if the shape in the
   current Frame is the same as the shape in the base Frame. This is the
   case if the simplified Mapping connecting base and current Frames is
   a UnitMap. Get the base->current Mapping from the Region. */
   map = astRegMapping( reg );

/* If it is not UnitMap, see if simplifying the whole Region results in
   the base->current Mapping in the simplified Region being a UnitMap. */
   if( !astIsAUnitMap( map ) ) {
      map = astAnnul( map );
      sreg = astSimplify( reg );
      map = astRegMapping( sreg );

/* If it is still not UnitMap, we cannot write out the region. */
      if( !astIsAUnitMap( map ) ) {
         astAddWarning( this, 1, "The supplied Region does not have a "
                        "supported shape within its current coordinate "
                        "system.", "astWrite", status );
         ok = 0;
      }

   } else {
      sreg = astClone( reg );
   }
   map = astAnnul( map );

/* Store a safe value that can be used to test unbounded axes. */
   lim = sqrt( DBL_MAX );

/* First job is to identify the Time, Space, Spectral and Redshift axes
   in the supplied Region.
   ------------------------------------------------------------------- */

/* Initialise things. */
   timeax = -1;
   nspace = 0;
   issky = 0;
   specax = -1;
   redax = -1;
   prop = NULL;

/* Get a pointer to the Frame encapsulated by the Region. */
   efrm = astRegFrame( sreg );

/* Loop round all axes. */
   nax = astGetNaxes( sreg );
   for( i = 0; i < nax; i++ ) {

/* Get the primary Frame that defines the current axis of the Region. */
      astPrimaryFrame( efrm, i, &pfrm, &pax );

/* Get its class and domain. */
      class = astGetClass( pfrm );
      dom = astGetDomain( pfrm );
      if( astOK ) {

/* The time axis is described by a TimeFrame with any domain. */
         if( !strcmp( class, "TimeFrame" ) ) {
            if( timeax == -1 ) {
               timeax = i;
            } else {
               astAddWarning( this, 1, "More than one time axis found. "
                              "Extra axis (axis %d) will be ignored.",
                              "astWrite", status, i + 1 );
            }

/* The space axes are described by a SkyFrame or a basic Frame. If a
   mixture of both types are found, report a warning and ignore the later
   axes. */
         } else if( !strcmp( class, "SkyFrame" ) ) {
            if( issky || nspace == 0 ) {
               if( nspace < 2 ) {
                  spaceax[ nspace++ ] = i;
                  issky = 1;
               } else {
                  astAddWarning( this, 1, "More than two sky frame axes "
                                 "found. Extra axis (axis %d) will be ignored.",
                                 "astWrite", status, i + 1 );
               }

            } else {
               astAddWarning( this, 1, "Mixture of basic and sky frame "
                              "axes found. Sky frame axis %d will be "
                              "ignored.", "astWrite", status, i + 1 );
            }

         } else if( !strcmp( class, "Frame" ) ) {
            if( !issky ) {
               if( nspace < 3 ) {
                  spaceax[ nspace++ ] = i;
               } else {
                  astAddWarning( this, 1, "More than three basic space frame axes "
                                 "found. Extra axis (axis %d) will be ignored.",
                                 "astWrite", status, i + 1 );
               }

            } else {
               astAddWarning( this, 1, "Mixture of basic and sky frame "
                              "axes found. Basic frame axis %d will be "
                              "ignored.", "astWrite", status, i + 1 );
            }

/* The spectral axis is described by a SpecFrame with domain SPECTRUM. */
         } else if( !strcmp( class, "SpecFrame" ) &&
                    !strcmp( dom, "SPECTRUM" ) ) {
            if( specax == -1 ) {
               specax = i;
            } else {
               astAddWarning( this, 1, "More than one spectral axis found. "
                              "Extra axis (axis %d) will be ignored.",
                              "astWrite", status, i + 1 );
            }

/* The redshift axis is described by a SpecFrame with domain REDSHIFT. */
         } else if( !strcmp( class, "SpecFrame" ) &&
                    !strcmp( dom, "REDSHIFT" ) ) {
            if( redax == -1 ) {
               redax = i;
            } else {
               astAddWarning( this, 1, "More than one redshift axis found. "
                              "Extra axis (axis %d) will be ignored.",
                              "astWrite", status, i + 1 );
            }

/* Warn about unused axes. */
         } else {
            astAddWarning( this, 1, "Could not classify axis %d (class=%s "
                           "domain=%s). It will be ignored.", "astWrite", status,
                           i + 1, class, dom );
         }
      }

/* Free resources. */
      pfrm = astAnnul( pfrm );
   }
   efrm = astAnnul( efrm );

/* Set a flag indicating if there is anything to convert. */
   ok = ok && ( timeax != -1 || nspace > 0 || specax != -1 || redax != -1 );


/* Now we have identified the axes, we convert each available STC-S
   sub-phrase, starting with the time sub-phrase.
   ---------------------------------------------------------------- */
   if( timeax != -1 ) {

/* Create a Region by picking the time axis from the supplied Region. */
      spreg = astPickAxes( sreg, 1, &timeax, NULL );

/* Check it is a Region. If not, we cannot convert anything. */
      if( !astIsARegion( spreg ) ) {
         astAddWarning( this, 1, "Cannot determine the region covered by "
                        "the time axis.", "astWrite", status );
         ok = 0;

/* Otherwise we add a description of the time sub-phrase to the
   properties keymap. */
      } else {

/* Get a pointer to the Region's time phrase property KeyMap, creating
   one if necessary. */
         if( astMapGet0A( props, "TIME_PROPS", &obj ) ) {
            spprops = (AstKeyMap *) obj;
         } else {
            spprops = astKeyMap( " ", status );
            astMapPut0A( props, "TIME_PROPS", spprops, NULL );
         }

/* Get the Region's fill factor. */
         fill = astGetFillFactor( spreg );

/* Ensure the TimeFrame represents MJD. If not, take a deep copy (to
   avoid changing the supplied Region), and set its system to MJD. */
         if( astGetSystem( spreg ) != AST__MJD ) {
            treg = astCopy( spreg );
            (void) astAnnul( spreg );
            spreg = treg;
            astSetAdaptive( spreg, 1 );
            astSetSystem( spreg, AST__MJD );
         }

/* Get the bounds of the Region (i.e. the time axis coverage). */
         astGetRegionBounds( spreg, lbnd, ubnd );

/* Get a pointer to the time Region's encapsulated Frame. */
         spfrm = astRegFrame( spreg );

/* Report a warning if the sub-phrase Frame is not a TimeFrame */
         if( !astIsATimeFrame( spfrm ) ) {
            ok = 0;
            astAddWarning( this, 1, "The time sub-phrase in the supplied "
                           "KeyMap is not described using an AST TimeFrame.",
                           "astWrite", status );

/* Store properties that are specific to Time moments... */
         } else if( lbnd[ 0 ] == ubnd[ 0 ] ) {
            astMapPut0C( spprops, "ID", "Time", NULL );
            StoreTimeProp( spprops,  (AstTimeFrame *) spfrm, "TIME", lbnd[ 0 ], status );
            fill = AST__BAD;

/* Store properties that are specific to Time intervals... */
         } else if( lbnd[ 0 ] > -lim && ubnd[ 0 ] < lim ) {
            astMapPut0C( spprops, "ID", "TimeInterval", NULL );
            StoreTimeProp( spprops,  (AstTimeFrame *) spfrm, "START", lbnd[ 0 ], status );
            StoreTimeProp( spprops,  (AstTimeFrame *) spfrm, "STOP", ubnd[ 0 ], status );

/* Store properties that are specific to Start times... */
         } else if( lbnd[ 0 ] > -lim ) {
            astMapPut0C( spprops, "ID", "StartTime", NULL );
            StoreTimeProp( spprops,  (AstTimeFrame *) spfrm, "START", lbnd[ 0 ], status );

/* Store properties that are specific to Stop times... */
         } else {
            astMapPut0C( spprops, "ID", "StopTime", NULL );
            StoreTimeProp( spprops,  (AstTimeFrame *) spfrm, "STOP", ubnd[ 0 ], status );

         }

/* Store properties that are common to all time sub-phrase types. First the
   fill factor. */
         MapPut0D( spprops, "FILLFACTOR", fill, 1.0, defs, status );

/* Now the time scale. */
         ts = astGetTimeScale( spfrm );
         if( ts == AST__TT ) {
            tts = "TT";

         } else if( ts == AST__TAI ) {
            tts = "TAI";

         } else if( ts == AST__UTC ) {
            tts = "UTC";

         } else if( ts == AST__TDB ) {
            tts = "TDB";

         } else if( ts == AST__TCG ) {
            tts = "TCG";

         } else if( ts == AST__TCB ) {
            tts = "TCB";

         } else if( ts == AST__LMST ) {
            tts = "LST";

         } else {
            tts = "nil";
            astAddWarning( this, 1, "Timescale '%s' is unsupported by "
                           "STC-S.", "astWrite", status,
                           astGetC( spfrm, "TimeScale" ) );
            ok = 0;
         }

         MapPut0C( spprops, "TIMESCALE", tts, "nil", defs, status );

/* RefPos. The AST TimeFrame class has no reference position, we leave
   unchanged any refpos already in the keymap. If there is no refpos in the
   keymap, we use "TOPOCENTER". */
         if( !astMapHasKey( spprops, "REFPOS" ) ) {
	    astMapPut0C( spprops, "REFPOS", "TOPOCENTER", NULL );
         }

/* That's it for the time sub-phrase, unless the supplied Region has an
   explicit (non-default) uncertainty. */
         unc = astGetUnc( spreg, 0 );
         if( unc ) {

/* See if the supplied properties KeyMap contains any item that refers to
   the Unit included in the STC-S description, but which is not updated by
   this function. If it does, we need to retain any units specified
   within the KeyMap. */
            retain_units = ( astMapHasKey( spprops, "RESOLUTION" ) ||
                             astMapHasKey( spprops, "PIXSIZE" ) ||
                             astMapHasKey( spprops, "SIZE" ) );

            if( retain_units ) {
               if( !astMapGet0C( spprops, "UNIT", &unit ) ) unit = "s";
            } else {
               unit = "s";
            }

/* Store the units string */
            MapPut0C( spprops, "UNIT", unit, "s", defs, status );

/* If necessary, map the uncertainty region into the requied units. Take
   a deep copy to avoid changing the supplied Region. */
            if( strcmp( unit, astGetUnit( unc, 0 ) ) ) {
               unca = astCopy( unc );
               astSetAdaptive( unca, 0 );
               astSetUnit( unca, 0, unit );
            } else {
               unca = astClone( unc );
            }

/* Get the bounds of the uncertainty. */
            astGetRegionBounds( unca, lbnd, ubnd );

/* The error is half the width of the bounding box. */
            astMapPut0D( spprops, "ERROR", 0.5*( ubnd[ 0 ] - lbnd[ 0 ] ), NULL );

/* Free resources. */
            unca = astAnnul( unca );
            unc = astAnnul( unc );
         }

/* Free resources. */
         spfrm = astAnnul( spfrm );
         spprops = astAnnul( spprops );
      }

/* Free resources. */
      spreg = astAnnul( spreg );

   }


/* Now convert the space sub-phrase.
   ---------------------------------------------------------------- */
   if( nspace > 0 && ok ) {

/* Create a Region by picking the space axes from the supplied Region. */
      spreg = astPickAxes( sreg, nspace, spaceax, NULL );

/* Check it is a Region. If not, we cannot convert anything. */
      if( ! astIsARegion( spreg ) ) {
         astAddWarning( this, 1, "Cannot determine the region covered by "
                        "the space axes.", "astWrite", status );
         ok = 0;

/* Otherwise we add a description of the space sub-phrase to the
   properties keymap. */
      } else {

/* Get a pointer to the Region's space phrase property KeyMap, creating
   one if necessary. */
         if( astMapGet0A( props, "SPACE_PROPS", &obj ) ) {
            spprops = (AstKeyMap *) obj;
         } else {
            spprops = astKeyMap( " ", status );
            astMapPut0A( props, "SPACE_PROPS", spprops, NULL );
         }

/* If the space frame is a SkyFrame, ensure it refers to a coodinate
   system that is supported by STC-S. Take a deep copy before changing
   anything. */
         if( issky ) {
            sys = astGetSystem( spreg );
            if( sys != AST__FK4 &&
                sys != AST__FK5 &&
                sys != AST__ICRS &&
                sys != AST__ECLIPTIC &&
                sys != AST__GALACTIC &&
                sys != AST__SUPERGALACTIC &&
                sys != AST__UNKNOWN ) {
               treg = astCopy( spreg );
               (void) astAnnul( spreg );
               spreg = treg;
               astSetAdaptive( spreg, 1 );
               astSetSystem( spreg, AST__ICRS );
            }
         }

/* Get a pointer to the Region's encapsulated Frame. */
         spfrm = astRegFrame( spreg );

/* If the supplied Region is defined in a SkyFrame, choose the units to
   use when storing radius, error, etc in the KeyMap. If the props KeyMap
   already contains a unit specification, we use it. Otherwise we use the
   default (degrees). AST uses radians internally, so find the scaling
   factor. */
         if( issky ) {
            if( astMapGet0C( spprops, "UNIT", &unit ) ) {
               if( !strcmp( unit, "arcmin" ) ) {
                  scale = AST__DR2D*60.0;
               } else if( !strcmp( unit, "arcsec" ) ) {
                  scale = AST__DR2D*3600.0;
               } else {
                  unit = "deg";
                  scale = AST__DR2D;
               }
            } else {
               unit = "deg";
               scale = AST__DR2D;
            }

/* Store the units string */
            MapPut0C( spprops, "UNIT", unit, "deg", defs, status );

/* If the supplied Region is not defined in a SkyFrame, we will arrange
   that the Region and the KeyMap use the same units, so set a scale
   factor of 1.0. */
         } else {
            scale = 1.0;

/* See if the supplied properties KeyMap contains any item that refers to
   the Unit included in the STC-S description, but which is not updated by
   this function. If it does, we need to retain any units specified
   within the KeyMap. */
            retain_units = ( astMapHasKey( spprops, "RESOLUTION" ) ||
                             astMapHasKey( spprops, "PIXSIZE" ) ||
                             astMapHasKey( spprops, "SIZE" ) );

/* If so, and if the properties KeyMap already contains a Unit
   specification, we convert the Region to the same units. Take a deep
   copy of the Region first to avoid modifying the supplied Region. */
            if( retain_units ) {
               if( !astMapGet0C( spprops, "UNIT", &unit ) ) unit = "deg";

               treg = astCopy( spreg );
               (void) astAnnul( spreg );
               spreg = treg;

               for( i = 0; i < nspace; i++ ) {
                  astSetUnit( spreg, i, unit );

/* Space frames can have different units on different axes. So look for
   the start of the next word in the Unit propert. This will be the unit
   for the next axis. If there are no more words in the Unit property,
   re-use the last unit value. */
                  q = unit;
                  while( *q && !isspace( *q ) ) q++;
                  while( *q && isspace( *q ) ) q++;
                  if( *q ) unit = q;
               }

/* If we are not retaining the units specified in the properties KeyMap, we
   retain the existing Region units instead, and store these units in the
   properties KeyMap. We also check that these units are supported by
   STC-S. */
            } else {

               nc = 0;
               allthesame = 1;
               unit1 = NULL;

               for( i = 0; i < nspace; i++ ) {
                  unit = astGetUnit( spreg, i );

                  if( !unit1 ) {
                     unit1 = astStore( NULL, unit, strlen( unit ) + 1 );
                  } else {
                     if( strcmp( unit, unit1 ) ) allthesame = 0;
                  }

                  if( strcmp( unit, "deg" ) &&
                      strcmp( unit, "arcmin" ) &&
                      strcmp( unit, "arcsec" ) &&
                      strcmp( unit, "m" ) &&
                      strcmp( unit, "mm" ) &&
                      strcmp( unit, "km" ) &&
                      strcmp( unit, "AU" ) &&
                      strcmp( unit, "pc" ) &&
                      strcmp( unit, "kpc" ) &&
                      strcmp( unit, "Mpc" ) ) {
                     astAddWarning( this, 1, "Cannot use spatial units '%s'.",
                                    "astWrite", status, unit );
                     ok = 0;
                     break;
                  }
                  prop = astAppendString( prop, &nc, unit );
                  prop = astAppendString( prop, &nc, " " );
               }

/* Remove the trailing space, and store the property value in the KeyMap. */
               if( ! allthesame ) {
                  if( prop && nc > 0 ) {
                     prop[ nc - 1 ] = 0;
                     astMapPut0C( spprops, "UNIT", prop, NULL );
                  }
               } else {
                  astMapPut0C( spprops, "UNIT", unit1, NULL );
               }

               unit1 = astFree( unit1 );

            }
         }

/* Get the fill factor. */
         fill = astGetFillFactor( spreg );

/* Get the default number of digits. This is only used if the supplied
   properties KeyMap does not have a value for the item being stored. If
   it does, the number of digits is inherited form the value int he KeyMap. */
         defdigs = astGetDigits( spfrm );

/* Store properties that are specific to the particular type of Region. */
         spaceid = GetRegionProps( this, spreg, spprops, nspace, defdigs,
                                   scale, issky, status );
         if( spaceid == NULL_ID ) ok = 0;

/* If the above went OK, store values for the properties that are common
   to all types of space sub-phrase. */
         if( ok ) {

/* First the fill factor. */
            if( spaceid != POSITION_ID ) {
               MapPut0D( spprops, "FILLFACTOR", fill, 1.0, defs, status );
            }

/* Now the coordinate frame. */
            tfrm = NULL;
            sys = astGetSystem( spfrm );
            if( issky ) {
               if( sys == AST__FK4 ){
                  tfrm = "B1950";
                  equinox = 1950.0;

               } else if( sys == AST__FK5 ){
                  tfrm = "J2000";
                  equinox = 2000.0;

               } else if( sys == AST__ICRS ){
                  tfrm = "ICRS";
                  equinox = AST__BAD;

               } else if( sys == AST__ECLIPTIC ){
                  tfrm = "ECLIPTIC";
                  equinox = 2000.0;

               } else if( sys == AST__GALACTIC ){
                  tfrm = "GALACTIC";
                  equinox = AST__BAD;

               } else if( sys == AST__SUPERGALACTIC ){
                  tfrm = "SUPER_GALACTIC";
                  equinox = AST__BAD;

               } else if( sys == AST__UNKNOWN ){
                  tfrm = NULL;
                  equinox = AST__BAD;

               } else {
                  tfrm = NULL;
                  astAddWarning( this, 1, "Sky system '%s' is "
                                 "unsupported by STC-S.", "astWrite",
                                 status, astGetC( spfrm, "System" ) );
                  ok = 0;
               }

               if( tfrm && equinox != AST__BAD ) {
                  if( astGetD( spfrm, "Equinox" ) != equinox ) {
                     astAddWarning( this, 1, "STC-S requires an equinox "
                                    "of %g for the %s frame, but the "
                                    "supplied %s equinox is %g.", "astWrite",
                                    status, equinox, tfrm,
                                    astGetClass( spfrm ),
                                    astGetD( spfrm, "Equinox" ) );
                     ok = 0;
                     tfrm = NULL;
                  }
               }
            }

/* If we do not yet have a Frame, use the Domain value if it is set (and
   is a legal STC-S Frame). */
            if( ! tfrm ) {
               if( astTestDomain( spfrm ) ) {
                  tfrm = astGetDomain( spfrm );
                  if( strcmp( tfrm, "ICRS" ) &&
                      strcmp( tfrm, "FK5" ) &&
                      strcmp( tfrm, "FK4" ) &&
                      strcmp( tfrm, "J2000" ) &&
                      strcmp( tfrm, "B1950" ) &&
                      strcmp( tfrm, "ECLIPTIC" ) &&
                      strcmp( tfrm, "GALACTIC" ) &&
                      strcmp( tfrm, "GALACTIC_II" ) &&
                      strcmp( tfrm, "SUPER_GALACTIC" ) &&
                      strcmp( tfrm, "GEO_C" ) &&
                      strcmp( tfrm, "GEO_D" ) ){
                     astAddWarning( this, 1, "'UNKNOWNFrame' being used in "
                                    "place of unsupported frame '%s'.",
                                     "astWrite", status, tfrm );
                     tfrm = NULL;
                  }
               }
            }

/* Store the Frame name in the props keymap. */
            if( !tfrm ) tfrm = "UNKNOWNFrame";
            astMapPut0C( spprops, "FRAME", tfrm, NULL );

/* RefPos. The AST SkyFrame and Frame classes have no reference position, so
   we leave unchanged any refpos already in the props keymap. If there is
   no refpos in the keymap, we use "TOPOCENTER". */
            if( !astMapHasKey( spprops, "REFPOS" ) ) {
               astMapPut0C( spprops, "REFPOS", "TOPOCENTER", NULL );
            }

/* Flavour. */
            if( issky ) {
               flavour = "SPHER2";
            } else if( nspace == 1 ){
               flavour = "CART1";
            } else if( nspace == 2 ){
               flavour = "CART2";
            } else {
               flavour = "CART3";
            }
            MapPut0C( spprops, "FLAVOUR", flavour, "SPHER2", defs, status );

/* That's it for the space sub-phrase, unless the supplied Region has an
   explicit (non-default) uncertainty. */
            unc = astGetUnc( spreg, 0 );
            if( unc ) {

/* Get the bounds of the uncertainty. */
               astGetRegionBounds( unc, lbnd, ubnd );

/* If its a sky frame, find the position of the centre of the uncertainty
   region. */
               pcen = issky ? astRegCentre( unc, NULL, NULL, 0,
                                            AST__CURRENT ) : NULL;

/* Find the half-width of the bounding box for each space axis, and
   concatenate their formatted values into a string. If any bound is
   undefined, quit the axis loop with nc=0. We need to convert longitude
   axis values from lingitude increments to arc-distance.  */
               nc = 0;
               defdigs = astGetDigits( unc );

               for( i = 0; i < nspace; i++ ) {
                  if( ubnd[ i ] != AST__BAD && lbnd[ i ] != AST__BAD ){

                     if( ! issky ) {
                        error = 0.5*( ubnd[ i ] - lbnd[ i ] );
                     } else {
                        if( i == 0 ) {
                           p1[ 0 ] = ubnd[ 0 ];
                           p1[ 1 ] = pcen[ 1 ];
                        } else {
                           p1[ 0 ] = pcen[ 0 ];
                           p1[ 1 ] = ubnd[ 1 ];
                        }
                        error = astDistance( spfrm, pcen, p1 );
                     }

                     GetFmt( "ERROR", spprops, i, defdigs, fmt, status );
                     (void) sprintf( buf, fmt, scale*error );
                     prop = astAppendString( prop, &nc, buf );
                     prop = astAppendString( prop, &nc, " " );

                  } else {
                     nc = 0;
                     break;
                  }
               }

/* If the bounds were all good, store the string holding the formatted
   error values in the properties KeyMap. */
               if( prop && nc > 0 ) {
                  prop[ nc - 1 ] = 0;
                  astMapPut0C( spprops, "ERROR", prop, NULL );
               }

/* Free resources. */
               pcen = astFree( pcen );
               unc = astAnnul( unc );
            }
         }

/* Free resources. */
         spfrm = astAnnul( spfrm );
         spprops = astAnnul( spprops );
      }

/* Free resources. */
      spreg = astAnnul( spreg );

   }



/* Convert the spectral sub-phrase.
   ---------------------------------------------------------------- */
   if( specax != -1 ) {

/* Create a Region by picking the spectral axis from the supplied Region. */
      spreg = astPickAxes( sreg, 1, &specax, NULL );

/* Check it is a Region. If not, we cannot convert anything. */
      if( !astIsARegion( spreg ) ) {
         astAddWarning( this, 1, "Cannot determine the region covered by "
                        "the spectral axis.", "astWrite", status );
         ok = 0;

/* Otherwise we add a description of the spectral sub-phrase to the
   properties keymap. */
      } else {

/* Get a pointer to the Region's spectral phrase property KeyMap, creating
   one if necessary. */
         if( astMapGet0A( props, "SPECTRAL_PROPS", &obj ) ) {
            spprops = (AstKeyMap *) obj;
         } else {
            spprops = astKeyMap( " ", status );
            astMapPut0A( props, "SPECTRAL_PROPS", spprops, NULL );
         }

/* See if the supplied properties KeyMap contains any item that refers to
   the Unit included in the STC-S description, but which is not updated by
   this function. If it does, we need to retain any units specified
   within the KeyMap. */
         retain_units = ( astMapHasKey( spprops, "RESOLUTION" ) ||
                          astMapHasKey( spprops, "PIXSIZE" ) ||
                          astMapHasKey( spprops, "SIZE" ) );

/* If so, and if the properties KeyMap already contains a Unit specification,
   we convert the Region to the same units and system. Determine the
   required system and units. */
         if( retain_units ) {
            if( !astMapGet0C( spprops, "UNIT", &unit ) ) unit = "Hz";

            if( !strcmp( unit, "Hz" ) ||
                !strcmp( unit, "MHz" ) ||
                !strcmp( unit, "GHz" ) ) {
               sys = AST__FREQ;

            } else if( !strcmp( unit, "m" ) ||
                       !strcmp( unit, "mm" ) ||
                       !strcmp( unit, "um" ) ||
                       !strcmp( unit, "nm" ) ||
                       !strcmp( unit, "Angstrom" ) ) {
               sys = AST__WAVELEN;

            } else if( !strcmp( unit, "eV" ) ||
                       !strcmp( unit, "keV" ) ||
                       !strcmp( unit, "MeV" ) ) {
               sys = AST__ENERGY;

            } else {
               astAddWarning( this, 1, "Illegal STC-S units '%s' found in "
                              "supplied KeyMap", "astWrite", status, unit );
               ok = 0;
            }

/* If we do not need to retain the units implied by the supplied KeyMap,
   use the Units and system in the supplied Region so long as they are
   supported by STC-S. If not, use a related supported system instead. */
         } else {
            sys = astGetSystem( spreg );
            unit = astGetUnit( spreg, 0 );

            if( sys == AST__ENERGY ) {
               sys = AST__ENERGY;
               if( strcmp( unit, "eV" ) &&
                   strcmp( unit, "keV" ) &&
                   strcmp( unit, "MeV" ) ) unit = "eV";

            } else if( sys == AST__WAVELEN || sys == AST__AIRWAVE ||
                       sys == AST__VOPTICAL || sys == AST__REDSHIFT ){
               sys = AST__WAVELEN;
               if( strcmp( unit, "m" ) &&
                   strcmp( unit, "mm" ) &&
                   strcmp( unit, "um" ) &&
                   strcmp( unit, "nm" ) &&
                   strcmp( unit, "Angstrom" ) ) unit = "m";

            } else {
               sys = AST__FREQ;
               if( strcmp( unit, "Hz" ) &&
                   strcmp( unit, "MHz" ) &&
                   strcmp( unit, "GHz" ) ) unit = "Hz";

            }
         }

/* Store the units string */
         MapPut0C( spprops, "UNIT", unit, "Hz", defs, status );

/* If either the System or Unit needs to be changed in the Region, take a
   deep copy first in order to avoid changing the supplied Region. */
         if( sys != astGetSystem( spreg ) ||
             ( unit && strcmp( unit, astGetUnit( spreg, 0 ) ) ) ) {
            treg = astCopy( spreg );
            (void) astAnnul( spreg );
            spreg = treg;
            astSetAdaptive( spreg, 1 );
            astSetSystem( spreg, sys );
            astSetUnit( spreg, 0, unit );
         }

/* Get the Region's fill factor. */
         fill = astGetFillFactor( spreg );

/* Get the bounds of the Region (i.e. the spectral axis coverage). */
         astGetRegionBounds( spreg, lbnd, ubnd );

/* Get a pointer to the spectral Region's encapsulated Frame. */
         spfrm = astRegFrame( spreg );

/* Report a warning if the sub-phrase Frame is not a SpecFrame */
         if( !astIsASpecFrame( spfrm ) ) {
            ok = 0;
            astAddWarning( this, 1, "The spectral sub-phrase in the supplied "
                           "KeyMap is not described using an AST SpecFrame.",
                           "astWrite", status );

/* Store properties that are specific to spectral positions... */
         } else if( lbnd[ 0 ] == ubnd[ 0 ] ) {
            astMapPut0C( spprops, "ID", "Spectral", NULL );
            astMapPut0D( spprops, "SPECTRAL", lbnd[ 0 ], NULL );
            fill = AST__BAD;

/* Store properties that are specific to Spectral intervals... */
         } else if( lbnd[ 0 ] > -lim && ubnd[ 0 ] < lim ) {
            astMapPut0C( spprops, "ID", "SpectralInterval", NULL );
            astMapPut0D( spprops, "LOLIMIT", lbnd[ 0 ], NULL );
            astMapPut0D( spprops, "HILIMIT", ubnd[ 0 ], NULL );

         } else {
            ok = 0;
            astAddWarning( this, 1, "Cannot write out an unbounded "
                           "spectral interval.", "astWrite", status );
         }

/* Store properties that are common to all spectral sub-phrase types. First the
   fill factor. */
         MapPut0D( spprops, "FILLFACTOR", fill, 1.0, defs, status );

/* Now the reference position. */
         sor = astGetStdOfRest( spfrm );
         if( sor == AST__GESOR ) {
            tsor = "GEOCENTER";

         } else if( sor == AST__BYSOR ) {
            tsor = "BARYCENTER";

         } else if( sor == AST__HLSOR ) {
            tsor = "HELIOCENTER";

         } else if( sor == AST__TPSOR ) {
            tsor = "TOPOCENTER";

         } else if( sor == AST__LKSOR ) {
            tsor = "LSRK";

         } else if( sor == AST__LDSOR ) {
            tsor = "LSRD";

         } else if( sor == AST__GLSOR ) {
            tsor = "GALACTIC_CENTER";

         } else {
            tsor = NULL;
         }

         if( !tsor ) tsor = "UNKNOWNRefPos";
         MapPut0C( spprops, "REFPOS", tsor, "UNKNOWNRefPos", defs,
                   status );

/* Now the unit string. */
         MapPut0C( spprops, "UNIT", unit, "Hz", defs, status );

/* That's it for the spectral sub-phrase, unless the supplied Region has an
   explicit (non-default) uncertainty. */
         unc = astGetUnc( spreg, 0 );
         if( unc ) {

/* Get the bounds of the uncertainty. */
            astGetRegionBounds( unc, lbnd, ubnd );

/* The error is half the width of the bounding box. */
            astMapPut0D( spprops, "ERROR", 0.5*( ubnd[ 0 ] - lbnd[ 0 ] ), NULL );

/* Free resources. */
            unc = astAnnul( unc );
         }

/* Free resources. */
         spfrm = astAnnul( spfrm );
         spprops = astAnnul( spprops );
      }

/* Free resources. */
      spreg = astAnnul( spreg );

   }



/* Convert the redshift sub-phrase.
   ---------------------------------------------------------------- */
   if( redax != -1 ) {

/* Create a Region by picking the redshift axis from the supplied Region. */
      spreg = astPickAxes( sreg, 1, &redax, NULL );

/* Check it is a Region. If not, we cannot convert anything. */
      if( !astIsARegion( spreg ) ) {
         astAddWarning( this, 1, "Cannot determine the region covered by "
                        "the redshift axis.", "astWrite", status );
         ok = 0;

/* Otherwise we add a description of the redshift sub-phrase to the
   properties keymap. */
      } else {

/* Get a pointer to the Region's redshift phrase property KeyMap, creating
   one if necessary. */
         if( astMapGet0A( props, "REDSHIFT_PROPS", &obj ) ) {
            spprops = (AstKeyMap *) obj;
         } else {
            spprops = astKeyMap( " ", status );
            astMapPut0A( props, "REDSHIFT_PROPS", spprops, NULL );
         }

/* See if the supplied properties KeyMap contains any item that refers to
   the system included in the STC-S description, but which is not updated by
   this function. If it does, we need to retain any system specified
   within the KeyMap. */
         retain_units = ( astMapHasKey( spprops, "RESOLUTION" ) ||
                          astMapHasKey( spprops, "PIXSIZE" ) ||
                          astMapHasKey( spprops, "SIZE" ) );

/* If so, and if the properties KeyMap already contains a DopplerDef or
   Type specification, we convert the Region to the same system. */
         if( retain_units ){
            if( !astMapGet0C( spprops, "DOPPLERDEF", &dopdef ) ) dopdef = "OPTICAL";
            if( !astMapGet0C( spprops, "TYPE", &type ) ) type = "VELOCITY";

            if( astChrMatch( type, "VELOCITY" ) ) {
               if( astChrMatch( dopdef, "OPTICAL" ) ) {
                  sys = AST__VOPTICAL;
               } else if( astChrMatch( dopdef, "RADIO" ) ) {
                  sys = AST__VRADIO;
               } else if( astChrMatch( dopdef, "RELATIVISTIC" ) ) {
                  sys = AST__VREL;
               } else {
                  astAddWarning( this, 1, "Illegal STC-S DopplerDef '%s' "
                                 "found in supplied KeyMap", "astWrite", status,
                                 dopdef );
                  ok = 0;
               }

            } else if( astChrMatch( type, "REDSHIFT" ) ) {
               if( astChrMatch( dopdef, "OPTICAL" ) ) {
                  sys = AST__REDSHIFT;
               } else {
                  astAddWarning( this, 1, "Unsupported combination of "
                                 "DopplerDef='%s' and Type='%s' found in "
                                 "supplied KeyMap", "astWrite", status, dopdef,
                                 type );
                  ok = 0;
               }

            } else {
               astAddWarning( this, 1, "Illegal STC-S Redshift Type '%s' "
                              "found in supplied KeyMap", "astWrite", status,
                              type );
               ok = 0;
            }

/* If the supplied KeyMap does not imply the required system, use the
   system in the supplied Region. */
         } else {
            sys = astGetSystem( spreg );
         }

/* Choose the requied units. */
         unit = ( sys == AST__REDSHIFT ) ? "": "km/s";

/* Store the units string */
         MapPut0C( spprops, "UNIT", unit, unit, defs, status );

/* If either the System or Unit needs to be changed in the Region, take a
   deep copy first in order to avoid changing the supplied Region. */
         if( sys != astGetSystem( spreg ) ||
             ( unit && strcmp( unit, astGetUnit( spreg, 0 ) ) ) ) {
            treg = astCopy( spreg );
            (void) astAnnul( spreg );
            spreg = treg;
            astSetAdaptive( spreg, 1 );
            astSetSystem( spreg, sys );
            astSetUnit( spreg, 0, unit );
         }

/* Get the Region's fill factor. */
         fill = astGetFillFactor( spreg );

/* Get the bounds of the Region (i.e. the redshift axis coverage). */
         astGetRegionBounds( spreg, lbnd, ubnd );

/* Get a pointer to the spectral Region's encapsulated Frame. */
         spfrm = astRegFrame( spreg );

/* Report a warning if the sub-phrase Frame is not a SpecFrame */
         if( !astIsASpecFrame( spfrm ) ) {
            ok = 0;
            astAddWarning( this, 1, "The redshift sub-phrase in the supplied "
                           "KeyMap is not described using an AST SpecFrame.",
                           "astWrite", status );

/* Store properties that are specific to redshift positions... */
         } else if( lbnd[ 0 ] == ubnd[ 0 ] ) {
            astMapPut0C( spprops, "ID", "Redshift", NULL );
            astMapPut0D( spprops, "REDSHIFT", lbnd[ 0 ], NULL );
            fill = AST__BAD;

/* Store properties that are specific to Redshift intervals... */
         } else if( lbnd[ 0 ] > -lim && ubnd[ 0 ] < lim ) {
            astMapPut0C( spprops, "ID", "RedshiftInterval", NULL );
            astMapPut0D( spprops, "LOLIMIT", lbnd[ 0 ], NULL );
            astMapPut0D( spprops, "HILIMIT", ubnd[ 0 ], NULL );

         } else {
            ok = 0;
            astAddWarning( this, 1, "Cannot write out an unbounded "
                           "redshift interval.", "astWrite", status );
         }

/* Store properties that are common to all redshift sub-phrase types. First the
   fill factor. */
         MapPut0D( spprops, "FILLFACTOR", fill, 1.0, defs, status );

/* Now the reference position. */
         sor = astGetStdOfRest( spfrm );

         if( sor == AST__GESOR ) {
            tsor = "GEOCENTER";

         } else if( sor == AST__BYSOR ) {
            tsor = "BARYCENTER";

         } else if( sor == AST__HLSOR ) {
            tsor = "HELIOCENTER";

         } else if( sor == AST__TPSOR ) {
            tsor = "TOPOCENTER";

         } else if( sor == AST__LKSOR ) {
            tsor = "LSRK";

         } else if( sor == AST__LDSOR ) {
            tsor = "LSRD";

         } else if( sor == AST__GLSOR ) {
            tsor = "GALACTIC_CENTER";

         } else {
            tsor = NULL;
         }

         if( !tsor ) tsor = "UNKNOWNRefPos";
         MapPut0C( spprops, "REFPOS", tsor, "UNKNOWNRefPos", defs,
                   status );

/* Type and DopplerDef. */
         if( sys == AST__VOPTICAL ) {
            type = "VELOCITY";
            dopdef = "OPTICAL";

         } else if( sys == AST__VRADIO ) {
            type = "VELOCITY";
            dopdef = "RADIO";

         } else if( sys == AST__VREL ) {
            type = "VELOCITY";
            dopdef = "RELATIVISTIC";

         } else {
            type = "REDSHIFT";
            dopdef = "OPTICAL";
         }
         astMapPut0C( spprops, "DOPPLERDEF", dopdef, NULL );
         MapPut0C( spprops, "TYPE", type, "REDSHIFT", defs, status );

/* Now the unit string. */
         MapPut0C( spprops, "UNIT", unit, unit, defs, status );

/* That's it for the redshift sub-phrase, unless the supplied Region has an
   explicit (non-default) uncertainty. */
         unc = astGetUnc( spreg, 0 );
         if( unc ) {

/* Get the bounds of the uncertainty. */
            astGetRegionBounds( unc, lbnd, ubnd );

/* The error is half the width of the bounding box. */
            astMapPut0D( spprops, "ERROR", 0.5*( ubnd[ 0 ] - lbnd[ 0 ] ), NULL );

/* Free resources. */
            unc = astAnnul( unc );
         }

/* Free resources. */
         spfrm = astAnnul( spfrm );
         spprops = astAnnul( spprops );
      }

/* Free resources. */
      spreg = astAnnul( spreg );

   }

/* Free resources */
   if( sreg ) sreg = astAnnul( sreg );
   if( prop ) prop = astFree( prop );

/* Return the result. */
   return ok;
}

/* Functions which access class attributes. */
/* ---------------------------------------- */
/* Implement member functions to access the attributes associated with
   this class using the macros defined for this purpose in the
   "object.h" file. For a description of each attribute, see the class
   interface (in the associated .h file). */

/*
*att++
*  Name:
*     StcsArea

*  Purpose:
*     Return the CoordinateArea component when reading an STC-S document?

*  Type:
*     Public attribute.

*  Synopsis:
*     Integer (boolean).

*  Description:
*     This is a boolean attribute which controls what is returned
*     by the
c     astRead
f     AST_READ
*     function when it is used to read from an StcsChan.
*     If StcsArea is set non-zero (the default), then a Region
*     representing the STC CoordinateArea will be returned by
c     astRead.
f     AST_READ.
*     If StcsArea is set to zero, then the STC CoordinateArea
*     will not be returned.

*  Notes:
*     - Other attributes such as StcsCoords and StcsProps can be used to
*     specify other Objects to be returned by
c     astRead.
f     AST_READ.
*     If more than one of these attributes is set non-zero, then the
*     actual Object returned by
c     astRead
f     AST_READ
*     will be a KeyMap, containing the requested Objects. In this
*     case, the Region representing the STC CoordinateArea will be
*     stored in the returned KeyMap using the key "AREA". If StcsArea
*     is the only attribute to be set non-zero, then the Object returned by
c     astRead
f     AST_READ
*     will be the CoordinateArea Region itself.
*     - The class of Region used to represent the CoordinateArea for each
*     STC-S sub-phrase is determined by the first word in the
*     sub-phrase (the "sub-phrase identifier"). The individual sub-phrase
*     Regions are combined into a single Prism, which is then simplified
c     using astSimplify
f     using AST_SIMPLIFY
*     to form the returned region.
*     - Sub-phrases that represent a single value ( that is, have
*     identifiers "Time", "Position", "Spectral" or "Redshift" ) are
*     considered to be be part of the STC CoordinateArea component.
*     - The TimeFrame used to represent a time STC-S sub-phrase will have
*     its TimeOrigin attribute set to the sub-phrase start time. If no
*     start time is specified by the sub-phrase, then the stop time will be
*     used instead. If no stop time is specified by the sub-phrase, then
*     the single time value specified in the sub-phrase will be used
*     instead. Subsequently clearing the TimeOrigin attribute (or setting
*     its value to zero) will cause the TimeFrame to reprsent absolute times.
*     - The Epoch attribute for the returned Region is set in the same
*     way as the TimeOrigin attribute (see above).

*  Applicability:
*     StcsChan
*        All StcsChans have this attribute.
*att--
*/

/* This ia a boolean value (0 or 1) with a value of -INT_MAX when
   undefined but yielding a default of 1. */
astMAKE_CLEAR(StcsChan,StcsArea,stcsarea,-INT_MAX)
astMAKE_GET(StcsChan,StcsArea,int,1,( this->stcsarea != -INT_MAX ? this->stcsarea : 1 ))
astMAKE_SET(StcsChan,StcsArea,int,stcsarea,( value != 0 ))
astMAKE_TEST(StcsChan,StcsArea,( this->stcsarea != -INT_MAX ))

/*
*att++
*  Name:
*     StcsCoords

*  Purpose:
*     Return the Coordinates component when reading an STC-S document?

*  Type:
*     Public attribute.

*  Synopsis:
*     Integer (boolean).

*  Description:
*     This is a boolean attribute which controls what is returned
*     by the
c     astRead
f     AST_READ
*     function when it is used to read from an StcsChan.
*     If StcsCoords is set non-zero, then a PointList
*     representing the STC Coordinates will be returned by
c     astRead.
f     AST_READ.
*     If StcsCoords is set to zero (the default), then the STC
*     Coordinates will not be returned.

*  Notes:
*     - Other attributes such as StcsArea and StcsProps can be used to
*     specify other Objects to be returned by
c     astRead.
f     AST_READ.
*     If more than one of these attributes is set non-zero, then the
*     actual Object returned by
c     astRead
f     AST_READ
*     will be a KeyMap, containing the requested Objects. In this
*     case, the PointList representing the STC Coordinates will be
*     stored in the returned KeyMap using the key "COORDS". If StcsCoords
*     is the only attribute to be set non-zero, then the Object returned by
c     astRead
f     AST_READ
*     will be the Coordinates PointList itself.
*     - The Coordinates component is specified by the additional axis
*     values embedded within the body of each STC-S sub-phrase that
*     represents an extended area. Sub-phrases that represent a single
*     value ( that is, have identifiers "Time", "Position", "Spectral"
*     or "Redshift" ) are not considered to be be part of the STC
*     Coordinates component.
*     - If the STC-S documents does not contain a Coordinates component,
*     then a NULL object pointer
f     (AST__NULL)
*     will be returned by
c     astRead
f     AST_READ
*     if the Coordinates component is the only object being returned. If
*     other objects are also being returned (see attributes StcsProps and
*     StcsArea), then the returned KeyMap will contain a "COORDS" key
*     only if the Coordinates component is read succesfully.
*     - The TimeFrame used to represent a time STC-S sub-phrase will have
*     its TimeOrigin attribute set to the sub-phrase start time. If no
*     start time is specified by the sub-phrase, then the stop time will be
*     used instead. If no stop time is specified by the sub-phrase, then
*     the single time value specified in the sub-phrase will be used
*     instead. Subsequently clearing the TimeOrigin attribute (or setting
*     its value to zero) will cause the TimeFrame to reprsent absolute times.
*     - The Epoch attribute for the returned Region is set in the same
*     way as the TimeOrigin attribute (see above).

*  Applicability:
*     StcsChan
*        All StcsChans have this attribute.
*att--
*/

/* This ia a boolean value (0 or 1) with a value of -INT_MAX when
   undefined but yielding a default of zero. */
astMAKE_CLEAR(StcsChan,StcsCoords,stcscoords,-INT_MAX)
astMAKE_GET(StcsChan,StcsCoords,int,0,( this->stcscoords != -INT_MAX ? this->stcscoords : 0 ))
astMAKE_SET(StcsChan,StcsCoords,int,stcscoords,( value != 0 ))
astMAKE_TEST(StcsChan,StcsCoords,( this->stcscoords != -INT_MAX ))

/*
*att++
*  Name:
*     StcsProps

*  Purpose:
*     Return all properties when reading an STC-S document?

*  Type:
*     Public attribute.

*  Synopsis:
*     Integer (boolean).

*  Description:
*     This is a boolean attribute which controls what is returned
*     by the
c     astRead
f     AST_READ
*     function when it is used to read from an StcsChan.
*     If StcsProps is set non-zero, then a KeyMap containing all the
*     properties read from the STC-S document will be returned by
c     astRead.
f     AST_READ.
*     If StcsProps is set to zero (the default), then the properties
*     will not be returned.

*  Notes:
*     - Other attributes such as StcsCoords and StcsArea can be used to
*     specify other Objects to be returned by
c     astRead.
f     AST_READ.
*     If more than one of these attributes is set non-zero, then the
*     actual Object returned by
c     astRead
f     AST_READ
*     will be a KeyMap containing the requested Objects. In this
*     case, the properties KeyMap will be stored in the returned KeyMap
*     using the key "PROPS". If StcsProps is the only attribute to be
*     set non-zero, then the Object returned by
c     astRead
f     AST_READ
*     will be the properties KeyMap itself.
*     - The KeyMap containing the properties will have entries for one or
*     more of the following keys: "TIME_PROPS", "SPACE_PROPS", "SPECTRAL_PROPS"
*     and "REDSHIFT_PROPS". Each of these entries will be another KeyMap
*     containing the properties of the corresponding STC-S sub-phrase.

*  Applicability:
*     StcsChan
*        All StcsChans have this attribute.
*att--
*/

/* This ia a boolean value (0 or 1) with a value of -INT_MAX when
   undefined but yielding a default of zero. */
astMAKE_CLEAR(StcsChan,StcsProps,stcsprops,-INT_MAX)
astMAKE_GET(StcsChan,StcsProps,int,0,( this->stcsprops != -INT_MAX ? this->stcsprops : 0 ))
astMAKE_SET(StcsChan,StcsProps,int,stcsprops,( value != 0 ))
astMAKE_TEST(StcsChan,StcsProps,( this->stcsprops != -INT_MAX ))

/*
*att++
*  Name:
*     StcsLength

*  Purpose:
*     Controls output line length.

*  Type:
*     Public attribute.

*  Synopsis:
*     Integer.

*  Description:
*     This attribute specifies the maximum length to use when writing out
*     text through the sink function supplied when the StcsChan was created.
*     It is ignored if the Indent attribute is zero (in which case the text
*     supplied to the sink function can be of any length). The default value
*     is 70.
*
*     The number of characters in each string written out through the sink
*     function will not usually be greater than the value of this attribute
*     (but may be less). However, if any single word in the STC-S
*     description exceeds the specified length, then the word will be
*     written out as a single line.
*
f     Note, the default value of zero is unlikely to be appropriate when
f     an StcsChan is used within Fortran code. In this case, StcsLength
f     should usually be set to the size of the CHARACTER variable used to
f     receive the text returned by AST_GETLINE within the sink function.
f     In addition, the Indent attribute should be set non-zero. This
f     avoids the possibility of long lines being truncated invisibly
f     within AST_GETLINE.

*  Applicability:
*     StcsChan
*        All StcsChans have this attribute.
*att--
*/
astMAKE_CLEAR(StcsChan,StcsLength,stcslength,-INT_MAX)
astMAKE_GET(StcsChan,StcsLength,int,70,( ( this->stcslength != -INT_MAX ) ? this->stcslength : 70 ))
astMAKE_SET(StcsChan,StcsLength,int,stcslength,(value<0?0:value))
astMAKE_TEST(StcsChan,StcsLength,( this->stcslength != -INT_MAX ))

/* Copy constructor. */
/* ----------------- */

/* Destructor. */
/* ----------- */

/* Dump function. */
/* -------------- */

static void Dump( AstObject *this_object, AstChannel *channel, int *status ) {
/*
*  Name:
*     Dump

*  Purpose:
*     Dump function for StcsChan objects.

*  Type:
*     Private function.

*  Synopsis:
*     void Dump( AstObject *this, AstChannel *channel, int *status )

*  Description:
*     This function implements the Dump function which writes out data
*     for the StcsChan class to an output Channel.

*  Parameters:
*     this
*        Pointer to the Object (an StcsChan) whose data are being written.
*     channel
*        Pointer to the Channel to which the data are being written.
*     status
*        Pointer to the inherited status variable.
*/

/* Local Variables: */
   AstStcsChan *this;            /* Pointer to the StcsChan structure */
   int ival;                     /* Integer value */
   int set;                      /* Attribute value set? */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain a pointer to the StcsChan structure. */
   this = (AstStcsChan *) this_object;

/* Write out values representing the instance variables for the
   StcsChan class.  Accompany these with appropriate comment strings,
   possibly depending on the values being written.*/

/* In the case of attributes, we first use the appropriate (private)
   Test...  member function to see if they are set. If so, we then use
   the (private) Get... function to obtain the value to be written
   out.

   For attributes which are not set, we use the astGet... method to
   obtain the value instead. This will supply a default value
   (possibly provided by a derived class which over-rides this method)
   which is more useful to a human reader as it corresponds to the
   actual default attribute value.  Since "set" will be zero, these
   values are for information only and will not be read back. */

/* StcsArea. */
/* --------- */
   set = TestStcsArea( this, status );
   ival = set ? GetStcsArea( this, status ) : astGetStcsArea( this );
   astWriteInt( channel, "StcsArea", set, 0, ival,
                ival ? "Read the STC CoordinatesArea component" :
                       "Do not read the STC CoordinatesArea component" );

/* StcsCoords. */
/* ----------- */
   set = TestStcsCoords( this, status );
   ival = set ? GetStcsCoords( this, status ) : astGetStcsCoords( this );
   astWriteInt( channel, "StcsCoords", set, 0, ival,
                ival ? "Read the STC Coordinates component" :
                       "Do not read the STC Coordinates component" );

/* StcsProps. */
/* ---------- */
   set = TestStcsProps( this, status );
   ival = set ? GetStcsProps( this, status ) : astGetStcsProps( this );
   astWriteInt( channel, "StcsProps", set, 0, ival,
                ival ? "Read the STC-S properties" :
                       "Do not read the STC-S properties" );

/* StcsLength */
/* ---------- */
      set = TestStcsLength( this, status );
      ival = set ? GetStcsLength( this, status ) : astGetStcsLength( this );
      astWriteInt( channel, "StcsLen", set, 0, ival, "STC-S buffer length" );

}

/* Standard class functions. */
/* ========================= */
/* Implement the astIsAStcsChan and astCheckStcsChan functions using the macros
   defined for this purpose in the "object.h" header file. */
astMAKE_ISA(StcsChan,Channel)
astMAKE_CHECK(StcsChan)

AstStcsChan *astStcsChan_( const char *(* source)( void ),
                           void (* sink)( const char * ),
                           const char *options, int *status, ...) {
/*
*++
*  Name:
c     astStcsChan
f     AST_STCSCHAN

*  Purpose:
*     Create an StcsChan.

*  Type:
*     Public function.

*  Synopsis:
c     #include "stcschan.h"
c     AstStcsChan *astStcsChan( const char *(* source)( void ),
c                               void (* sink)( const char * ),
c                               const char *options, ... )
f     RESULT = AST_STCSCHAN( SOURCE, SINK, OPTIONS, STATUS )

*  Class Membership:
*     StcsChan constructor.

*  Description:
*     This function creates a new StcsChan and optionally initialises
*     its attributes.
*
*     A StcsChan is a specialised form of Channel which supports STC-S
*     I/O operations. Writing an Object to an StcsChan (using
c     astWrite) will, if the Object is suitable, generate an
f     AST_WRITE) will, if the Object is suitable, generate an
*     STC-S description of that Object, and reading from an StcsChan will
*     create a new Object from its STC-S description.
*
*     Normally, when you use an StcsChan, you should provide "source"
c     and "sink" functions which connect it to an external data store
c     by reading and writing the resulting text. These functions
f     and "sink" routines which connect it to an external data store
f     by reading and writing the resulting text. These routines
*     should perform any conversions needed between external character
c     encodings and the internal ASCII encoding. If no such functions
f     encodings and the internal ASCII encoding. If no such routines
*     are supplied, a Channel will read from standard input and write
*     to standard output.
*
*     Alternatively, an XmlChan can be told to read or write from
*     specific text files using the SinkFile and SourceFile attributes,
*     in which case no sink or source function need be supplied.

*  Parameters:
c     source
f     SOURCE = SUBROUTINE (Given)
c        Pointer to a source function that takes no arguments and
c        returns a pointer to a null-terminated string.  If no value
c        has been set for the SourceFile attribute, this function
c        will be used by the StcsChan to obtain lines of input text. On
c        each invocation, it should return a pointer to the next input
c        line read from some external data store, and a NULL pointer
c        when there are no more lines to read.
c
c        If "source" is NULL and no value has been set for the SourceFile
c        attribute, the StcsChan will read from standard input instead.
f        A source routine, which is a subroutine which takes a single
f        integer error status argument.   If no value has been set
f        for the SourceFile attribute, this routine will be used by
f        the StcsChan to obtain lines of input text. On each
f        invocation, it should read the next input line from some
f        external data store, and then return the resulting text to
f        the AST library by calling AST_PUTLINE. It should supply a
f        negative line length when there are no more lines to read.
f        If an error occurs, it should set its own error status
f        argument to an error value before returning.
f
f        If the null routine AST_NULL is suppied as the SOURCE value,
f        and no value has been set for the SourceFile attribute,
f        the StcsChan will read from standard input instead.
c     sink
f     SINK = SUBROUTINE (Given)
c        Pointer to a sink function that takes a pointer to a
c        null-terminated string as an argument and returns void.
c        If no value has been set for the SinkFile attribute, this
c        function will be used by the StcsChan to deliver lines of
c        output text. On each invocation, it should deliver the
c        contents of the string supplied to some external data store.
c
c        If "sink" is NULL, and no value has been set for the SinkFile
c        attribute, the StcsChan will write to standard output instead.
f        A sink routine, which is a subroutine which takes a single
f        integer error status argument.  If no value has been set
f        for the SinkFile attribute, this routine will be used by
f        the StcsChan to deliver lines of output text. On each
f        invocation, it should obtain the next output line from the
f        AST library by calling AST_GETLINE, and then deliver the
f        resulting text to some external data store.  If an error
f        occurs, it should set its own error status argument to an
f        error value before returning.
f
f        If the null routine AST_NULL is suppied as the SINK value,
f        and no value has been set for the SinkFile attribute,
f        the StcsChan will write to standard output instead.
c     options
f     OPTIONS = CHARACTER * ( * ) (Given)
c        Pointer to a null-terminated string containing an optional
c        comma-separated list of attribute assignments to be used for
c        initialising the new StcsChan. The syntax used is identical to
c        that for the astSet function and may include "printf" format
c        specifiers identified by "%" symbols in the normal way.
f        A character string containing an optional comma-separated
f        list of attribute assignments to be used for initialising the
f        new StcsChan. The syntax used is identical to that for the
f        AST_SET routine.
c     ...
c        If the "options" string contains "%" format specifiers, then
c        an optional list of additional arguments may follow it in
c        order to supply values to be substituted for these
c        specifiers. The rules for supplying these are identical to
c        those for the astSet function (and for the C "printf"
c        function).
f     STATUS = INTEGER (Given and Returned)
f        The global status.

*  Returned Value:
c     astStcsChan()
f     AST_STCSCHAN = INTEGER
*        A pointer to the new StcsChan.

*  Notes:
f     - The names of the routines supplied for the SOURCE and SINK
f     arguments should appear in EXTERNAL statements in the Fortran
f     routine which invokes AST_STCSCHAN. However, this is not generally
f     necessary for the null routine AST_NULL (so long as the AST_PAR
f     include file has been used).
*     - If the external data source or sink uses a character encoding
*     other than ASCII, the supplied source and sink functions should
*     translate between the external character encoding and the internal
*     ASCII encoding used by AST.
*     - A null Object pointer (AST__NULL) will be returned if this
*     function is invoked with the AST error status set, or if it
*     should fail for any reason.
f     - Note that the null routine AST_NULL (one underscore) is
f     different to AST__NULL (two underscores), which is the null Object
f     pointer.
*--
*/

/* Local Variables: */
   astDECLARE_GLOBALS            /* Pointer to thread-specific global data */
   AstStcsChan *new;             /* Pointer to new StcsChan */
   va_list args;                 /* Variable argument list */

/* Get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(NULL);

/* Check the global status. */
   if ( !astOK ) return NULL;

/* Initialise the StcsChan, allocating memory and initialising the
   virtual function table as well if necessary. This interface is for
   use by other C functions within AST, and uses the standard "wrapper"
   functions included in this class. */
   new = astInitStcsChan( NULL, sizeof( AstStcsChan ), !class_init,
                          &class_vtab, "StcsChan", source, SourceWrap,
                          sink, SinkWrap );

/* If successful, note that the virtual function table has been
   initialised. */
   if ( astOK ) {
      class_init = 1;

/* Obtain the variable argument list and pass it along with the
   options string to the astVSet method to initialise the new
   StcsChan's attributes. */
      va_start( args, status );
      astVSet( new, options, NULL, args );
      va_end( args );

/* If an error occurred, clean up by deleting the new object. */
      if ( !astOK ) new = astDelete( new );
   }

/* Return a pointer to the new StcsChan. */
   return new;
}

AstStcsChan *astStcsChanId_( const char *(* source)( void ),
                             void (* sink)( const char * ),
                             const char *options, ... ) {
/*
*  Name:
*     astStcsChanId_

*  Purpose:
*     Create an StcsChan.

*  Type:
*     Private function.

*  Synopsis:
*     #include "stcschan.h"
*     AstStcsChan *astStcsChanId_( const char *(* source)( void ),
*                                  void (* sink)( const char * ),
*                                  const char *options, ... )

*  Class Membership:
*     StcsChan constructor.

*  Description:
*     This function implements the external (public) C interface to the
*     astStcsChan constructor function. Another function (astStcsChanForId)
*     should be called to create an StcsChan for use within other languages.
*     Both functions return an ID value (instead of a true C pointer) to
*     external users, and must be provided because astStcsChan_ has a variable
*     argument list which cannot be encapsulated in a macro (where this conversion would otherwise
*     occur).
*
*     The variable argument list also prevents this function from
*     invoking astStcsChan_ directly, so it must be a re-implementation
*     of it in all respects, except for the final conversion of the
*     result to an ID value.

*  Parameters:
*     As for astStcsChan_.

*  Returned Value:
*     The ID value associated with the new StcsChan.
*/

/* Local Variables: */
   astDECLARE_GLOBALS            /* Pointer to thread-specific global data */
   AstStcsChan *new;             /* Pointer to new StcsChan */
   va_list args;                 /* Variable argument list */

   int *status;                  /* Pointer to inherited status value */

/* Get a pointer to the inherited status value. */
   status = astGetStatusPtr;

/* Get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(NULL);

/* Check the global status. */
   if ( !astOK ) return NULL;

/* Initialise the StcsChan, allocating memory and initialising the
   virtual function table as well if necessary. This interface is for
   use by external C functions and uses the standard "wrapper"
   functions included in this class. */
   new = astInitStcsChan( NULL, sizeof( AstStcsChan ), !class_init,
                          &class_vtab, "StcsChan", source, SourceWrap,
                          sink, SinkWrap );

/* If successful, note that the virtual function table has been
   initialised. */
   if ( astOK ) {
      class_init = 1;

/* Obtain the variable argument list and pass it along with the
   options string to the astVSet method to initialise the new
   StcsChan's attributes. */
      va_start( args, options );
      astVSet( new, options, NULL, args );
      va_end( args );

/* If an error occurred, clean up by deleting the new object. */
      if ( !astOK ) new = astDelete( new );
   }

/* Return an ID value for the new StcsChan. */
   return astMakeId( new );
}

AstStcsChan *astStcsChanForId_( const char *(* source)( void ),
                                char *(* source_wrap)( const char *(*)( void ), int * ),
                                void (* sink)( const char * ),
                                void (* sink_wrap)( void (*)( const char * ),
                                                    const char *, int * ),
                                const char *options, ... ) {
/*
*+
*  Name:
*     astStcsChanFor

*  Purpose:
*     Initialise an StcsChan from a foreign language interface.

*  Type:
*     Public function.

*  Synopsis:
*     #include "stcschan.h"
*     AstStcsChan *astStcsChanFor( const char *(* source)( void ),
*                                  char *(* source_wrap)( const char *(*)
*                                                         ( void ), int * ),
*                                  void (* sink)( const char * ),
*                                  void (* sink_wrap)( void (*)( const char * ),
*                                                      const char *, int * ),
*                                  const char *options, ... )

*  Class Membership:
*     StcsChan constructor.

*  Description:
*     This function creates a new StcsChan from a foreign language
*     interface and optionally initialises its attributes.
*
*     A StcsChan is a specialised form of Channel which supports STC-S
*     I/O operations. Writing an Object to an StcsChan (using
c     astWrite) will, if the Object is suitable, generate an
f     AST_WRITE) will, if the Object is suitable, generate an
*     STC-S description of that Object, and reading from an StcsChan will
*     create a new Object from its STC-S description.
*
*     Normally, when you use an StcsChan, you should provide "source"
c     and "sink" functions which connect it to an external data store
c     by reading and writing the resulting text. These functions
f     and "sink" routines which connect it to an external data store
f     by reading and writing the resulting text. These routines
*     should perform any conversions needed between external character
c     encodings and the internal ASCII encoding. If no such functions
f     encodings and the internal ASCII encoding. If no such routines
*     are supplied, a Channel will read from standard input and write
*     to standard output.

*  Parameters:
*     source
*        Pointer to a "source" function which will be used to obtain
*        lines of input text. Generally, this will be obtained by
*        casting a pointer to a source function which is compatible
*        with the "source_wrap" wrapper function (below). The pointer
*        should later be cast back to its original type by the
*        "source_wrap" function before the function is invoked.
*
*        If "source" is NULL, the StcsChan will read from standard
*        input instead.
*     source_wrap
*        Pointer to a function which can be used to invoke the
*        "source" function supplied (above). This wrapper function is
*        necessary in order to hide variations in the nature of the
*        source function, such as may arise when it is supplied by a
*        foreign (non-C) language interface.
*
*        The single parameter of the "source_wrap" function is a
*        pointer to the "source" function, and it should cast this
*        function pointer (as necessary) and invoke the function with
*        appropriate arguments to obtain the next line of input
*        text. The "source_wrap" function should then return a pointer
*        to a dynamically allocated, null terminated string containing
*        the text that was read. The string will be freed (using
*        astFree) when no longer required and the "source_wrap"
*        function need not concern itself with this. A NULL pointer
*        should be returned if there is no more input to read.
*
*        If "source_wrap" is NULL, the StcsChan will read from standard
*        input instead.
*     sink
*        Pointer to a "sink" function which will be used to deliver
*        lines of output text. Generally, this will be obtained by
*        casting a pointer to a sink function which is compatible with
*        the "sink_wrap" wrapper function (below). The pointer should
*        later be cast back to its original type by the "sink_wrap"
*        function before the function is invoked.
*
*        If "sink" is NULL, the StcsChan will write to standard output
*        instead.
*     sink_wrap
*        Pointer to a function which can be used to invoke the "sink"
*        function supplied (above). This wrapper function is necessary
*        in order to hide variations in the nature of the sink
*        function, such as may arise when it is supplied by a foreign
*        (non-C) language interface.
*
*        The first parameter of the "sink_wrap" function is a pointer
*        to the "sink" function, and the second parameter is a pointer
*        to a const, null-terminated character string containing the
*        text to be written.  The "sink_wrap" function should cast the
*        "sink" function pointer (as necessary) and invoke the
*        function with appropriate arguments to deliver the line of
*        output text. The "sink_wrap" function then returns void.
*
*        If "sink_wrap" is NULL, the Channel will write to standard
*        output instead.
*     options
*        Pointer to a null-terminated string containing an optional
*        comma-separated list of attribute assignments to be used for
*        initialising the new StcsChan. The syntax used is identical to
*        that for the astSet function and may include "printf" format
*        specifiers identified by "%" symbols in the normal way.
*     ...
*        If the "options" string contains "%" format specifiers, then
*        an optional list of additional arguments may follow it in
*        order to supply values to be substituted for these
*        specifiers. The rules for supplying these are identical to
*        those for the astSet function (and for the C "printf"
*        function).

*  Returned Value:
*     astStcsChanFor()
*        A pointer to the new StcsChan.

*  Notes:
*     - A null Object pointer (AST__NULL) will be returned if this
*     function is invoked with the global error status set, or if it
*     should fail for any reason.
*     - This function is only available through the public interface
*     to the StcsChan class (not the protected interface) and is
*     intended solely for use in implementing foreign language
*     interfaces to this class.
*-

*  Implememtation Notes:
*     - This function behaves exactly like astStcsChanId_, in that it
*     returns ID values and not true C pointers, but it has two
*     additional arguments. These are pointers to the "wrapper
*     functions" which are needed to accommodate foreign language
*     interfaces.
*/

/* Local Variables: */
   astDECLARE_GLOBALS            /* Pointer to thread-specific global data */
   AstStcsChan *new;             /* Pointer to new StcsChan */
   va_list args;                 /* Variable argument list */
   int *status;                  /* Pointer to inherited status value */

/* Get a pointer to the inherited status value. */
   status = astGetStatusPtr;

/* Check the global status. */
   if ( !astOK ) return NULL;

/* Get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(NULL);

/* Initialise the StcsChan, allocating memory and initialising the
   virtual function table as well if necessary. */
   new = astInitStcsChan( NULL, sizeof( AstStcsChan ), !class_init,
                          &class_vtab, "StcsChan", source, source_wrap,
                          sink, sink_wrap );

/* If successful, note that the virtual function table has been
   initialised. */
   if ( astOK ) {
      class_init = 1;

/* Obtain the variable argument list and pass it along with the
   options string to the astVSet method to initialise the new
   StcsChan's attributes. */
      va_start( args, options );
      astVSet( new, options, NULL, args );
      va_end( args );

/* If an error occurred, clean up by deleting the new object. */
      if ( !astOK ) new = astDelete( new );
   }

/* Return an ID value for the new StcsChan. */
   return astMakeId( new );
}

AstStcsChan *astInitStcsChan_( void *mem, size_t size, int init,
                               AstStcsChanVtab *vtab, const char *name,
                               const char *(* source)( void ),
                               char *(* source_wrap)( const char *(*)( void ), int * ),
                               void (* sink)( const char * ),
                               void (* sink_wrap)( void (*)( const char * ),
                                                   const char *, int * ), int *status ) {
/*
*+
*  Name:
*     astInitStcsChan

*  Purpose:
*     Initialise an StcsChan.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "stcschan.h"
*     AstStcsChan *astInitStcsChan( void *mem, size_t size, int init,
*                                   AstStcsChanVtab *vtab, const char *name,
*                                   const char *(* source)( void ),
*                                   char *(* source_wrap)( const char *(*)( void ), int * ),
*                                   void (* sink)( const char * ),
*                                   void (* sink_wrap)( void (*)( const char * ),
*                                                     const char *, int * ) )

*  Class Membership:
*     StcsChan initialiser.

*  Description:
*     This function is provided for use by class implementations to
*     initialise a new StcsChan object. It allocates memory (if
*     necessary) to accommodate the StcsChan plus any additional data
*     associated with the derived class.  It then initialises a
*     StcsChan structure at the start of this memory. If the "init"
*     flag is set, it also initialises the contents of a virtual
*     function table for an StcsChan at the start of the memory passed
*     via the "vtab" parameter.

*  Parameters:
*     mem
*        A pointer to the memory in which the StcsChan is to be
*        initialised.  This must be of sufficient size to accommodate
*        the StcsChan data (sizeof(StcsChan)) plus any data used by the
*        derived class. If a value of NULL is given, this function
*        will allocate the memory itself using the "size" parameter to
*        determine its size.
*     size
*        The amount of memory used by the StcsChan (plus derived class
*        data).  This will be used to allocate memory if a value of
*        NULL is given for the "mem" parameter. This value is also
*        stored in the StcsChan structure, so a valid value must be
*        supplied even if not required for allocating memory.
*     init
*        A boolean flag indicating if the StcsChan's virtual function
*        table is to be initialised. If this value is non-zero, the
*        virtual function table will be initialised by this function.
*     vtab
*        Pointer to the start of the virtual function table to be
*        associated with the new StcsChan.
*     name
*        Pointer to a constant null-terminated character string which
*        contains the name of the class to which the new object
*        belongs (it is this pointer value that will subsequently be
*        returned by the astGetClass method).
*     source
*        Pointer to a "source" function which will be used to obtain
*        lines of text. Generally, this will be obtained by
*        casting a pointer to a source function which is compatible
*        with the "source_wrap" wrapper function (below). The pointer
*        should later be cast back to its original type by the
*        "source_wrap" function before the function is invoked.
*
*        If "source" is NULL, the Channel will read from standard
*        input instead.
*     source_wrap
*        Pointer to a function which can be used to invoke the
*        "source" function supplied (above). This wrapper function is
*        necessary in order to hide variations in the nature of the
*        source function, such as may arise when it is supplied by a
*        foreign (non-C) language interface.
*
*        The single parameter of the "source_wrap" function is a
*        pointer to the "source" function, and it should cast this
*        function pointer (as necessary) and invoke the function with
*        appropriate arguments to obtain the next line of input
*        text. The "source_wrap" function should then return a pointer
*        to a dynamically allocated, null terminated string containing
*        the text that was read. The string will be freed (using
*        astFree) when no longer required and the "source_wrap"
*        function need not concern itself with this. A NULL pointer
*        should be returned if there is no more input to read.
*
*        If "source_wrap" is NULL, the Channel will read from standard
*        input instead.
*     sink
*        Pointer to a "sink" function which will be used to deliver
*        lines of text. Generally, this will be obtained by
*        casting a pointer to a sink function which is compatible with
*        the "sink_wrap" wrapper function (below). The pointer should
*        later be cast back to its original type by the "sink_wrap"
*        function before the function is invoked.
*
*        If "sink" is NULL, the contents of the StcsChan will not be
*        written out before being deleted.
*     sink_wrap
*        Pointer to a function which can be used to invoke the "sink"
*        function supplied (above). This wrapper function is necessary
*        in order to hide variations in the nature of the sink
*        function, such as may arise when it is supplied by a foreign
*        (non-C) language interface.
*
*        The first parameter of the "sink_wrap" function is a pointer
*        to the "sink" function, and the second parameter is a pointer
*        to a const, null-terminated character string containing the
*        text to be written.  The "sink_wrap" function should cast the
*        "sink" function pointer (as necessary) and invoke the
*        function with appropriate arguments to deliver the line of
*        output text. The "sink_wrap" function then returns void.
*
*        If "sink_wrap" is NULL, the Channel will write to standard
*        output instead.

*  Returned Value:
*     A pointer to the new StcsChan.

*  Notes:
*     - A null pointer will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*-
*/

/* Local Variables: */
   AstStcsChan *new;              /* Pointer to new StcsChan */

/* Check the global status. */
   if ( !astOK ) return NULL;

/* If necessary, initialise the virtual function table. */
   if ( init ) astInitStcsChanVtab( vtab, name );

/* Initialise a Channel structure (the parent class) as the first
   component within the StcsChan structure, allocating memory if
   necessary. */
   new = (AstStcsChan *) astInitChannel( mem, size, 0,
                                         (AstChannelVtab *) vtab, name,
                                         source, source_wrap, sink,
                                         sink_wrap );

   if ( astOK ) {

/* Initialise the StcsChan data. */
/* ---------------------------- */
      new->stcsarea = -INT_MAX;
      new->stcscoords = -INT_MAX;
      new->stcsprops = -INT_MAX;
      new->stcslength = -INT_MAX;

/* If an error occurred, clean up by deleting the new object. */
      if ( !astOK ) new = astDelete( new );
   }

/* Return a pointer to the new object. */
   return new;
}

AstStcsChan *astLoadStcsChan_( void *mem, size_t size,
                               AstStcsChanVtab *vtab, const char *name,
                               AstChannel *channel, int *status ) {
/*
*+
*  Name:
*     astLoadStcsChan

*  Purpose:
*     Load an StcsChan.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "stcschan.h"
*     AstStcsChan *astLoadStcsChan( void *mem, size_t size,
*                                   AstStcsChanVtab *vtab, const char *name,
*                                   AstChannel *channel )

*  Class Membership:
*     StcsChan loader.

*  Description:
*     This function is provided to load a new StcsChan using data read
*     from a Channel. It first loads the data used by the parent class
*     (which allocates memory if necessary) and then initialises a
*     StcsChan structure in this memory, using data read from the input
*     Channel.
*
*     If the "init" flag is set, it also initialises the contents of a
*     virtual function table for an StcsChan at the start of the memory
*     passed via the "vtab" parameter.

*  Parameters:
*     mem
*        A pointer to the memory into which the StcsChan is to be
*        loaded.  This must be of sufficient size to accommodate the
*        StcsChan data (sizeof(StcsChan)) plus any data used by derived
*        classes. If a value of NULL is given, this function will
*        allocate the memory itself using the "size" parameter to
*        determine its size.
*     size
*        The amount of memory used by the StcsChan (plus derived class
*        data).  This will be used to allocate memory if a value of
*        NULL is given for the "mem" parameter. This value is also
*        stored in the StcsChan structure, so a valid value must be
*        supplied even if not required for allocating memory.
*
*        If the "vtab" parameter is NULL, the "size" value is ignored
*        and sizeof(AstStcsChan) is used instead.
*     vtab
*        Pointer to the start of the virtual function table to be
*        associated with the new StcsChan. If this is NULL, a pointer
*        to the (static) virtual function table for the StcsChan class
*        is used instead.
*     name
*        Pointer to a constant null-terminated character string which
*        contains the name of the class to which the new object
*        belongs (it is this pointer value that will subsequently be
*        returned by the astGetClass method).
*
*        If the "vtab" parameter is NULL, the "name" value is ignored
*        and a pointer to the string "StcsChan" is used instead.

*  Returned Value:
*     A pointer to the new StcsChan.

*  Notes:
*     - A null pointer will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*-
*/

/* Local Variables: */
   astDECLARE_GLOBALS           /* Pointer to thread-specific global data */
   AstStcsChan *new;            /* Pointer to the new StcsChan */

/* Initialise. */
   new = NULL;

/* Check the global error status. */
   if( !astOK ) return new;

/* Get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(channel);

/* If a NULL virtual function table has been supplied, then this is
   the first loader to be invoked for this StcsChan. In this case the
   StcsChan belongs to this class, so supply appropriate values to be
   passed to the parent class loader (and its parent, etc.). */
   if ( !vtab ) {
      size = sizeof( AstStcsChan );
      vtab = &class_vtab;
      name = "StcsChan";

/* If required, initialise the virtual function table for this class. */
      if ( !class_init ) {
         astInitStcsChanVtab( vtab, name );
         class_init = 1;
      }
   }

/* Invoke the parent class loader to load data for all the ancestral
   classes of the current one, returning a pointer to the resulting
   partly-built StcsChan. */
   new = astLoadChannel( mem, size, (AstChannelVtab *) vtab, name,
                         channel );

   if ( astOK ) {

/* Read input data. */
/* ================ */
/* Request the input Channel to read all the input data appropriate to
   this class into the internal "values list". */
      astReadClassData( channel, "StcsChan" );

/* Now read each individual data item from this list and use it to
   initialise the appropriate instance variable(s) for this class. */

/* In the case of attributes, we first read the "raw" input value,
   supplying the "unset" value as the default. If a "set" value is
   obtained, we then use the appropriate (private) Set... member
   function to validate and set the value properly. */

/* StcsArea. */
/* --------- */
      new->stcsarea = astReadInt( channel, "stcsarea", -INT_MAX );
      if ( TestStcsArea( new, status ) ) SetStcsArea( new, new->stcsarea, status );

/* StcsCoords. */
/* ----------- */
      new->stcscoords = astReadInt( channel, "stcscoords", -INT_MAX );
      if ( TestStcsCoords( new, status ) ) SetStcsCoords( new, new->stcscoords, status );

/* StcsProps. */
/* ---------- */
      new->stcsprops = astReadInt( channel, "stcsprops", -INT_MAX );
      if ( TestStcsProps( new, status ) ) SetStcsProps( new, new->stcsprops, status );

/* StcsLength */
/* ---------- */
      new->stcslength = astReadInt( channel, "stcslen", -INT_MAX );

   }

/* If an error occurred, clean up by deleting the new StcsChan. */
   if ( !astOK ) new = astDelete( new );

/* Return the new StcsChan pointer. */
   return new;
}

/* Virtual function interfaces. */
/* ============================ */
/* These provide the external interface to the virtual functions defined by
   this class. Each simply checks the global error status and then locates and
   executes the appropriate member function, using the function pointer stored
   in the object's virtual function table (this pointer is located using the
   astMEMBER macro defined in "object.h").

   Note that the member function may not be the one defined here, as it may
   have been over-ridden by a derived class. However, it should still have the
   same interface. */








