/*
*class++
*  Name:
*     Frame

*  Purpose:
*     Coordinate system description.

*  Constructor Function:
c     astFrame
f     AST_FRAME

*  Description:
*     This class is used to represent coordinate systems. It does this
*     in rather the same way that a frame around a graph describes the
*     coordinate space in which data are plotted. Consequently, a
*     Frame has a Title (string) attribute, which describes the
*     coordinate space, and contains axes which in turn hold
*     information such as Label and Units strings which are used for
*     labelling (e.g.) graphical output. In general, however, the
*     number of axes is not restricted to two.
*
*     Functions are available for converting Frame coordinate values
*     into a form suitable for display, and also for calculating
*     distances and offsets between positions within the Frame.
*
*     Frames may also contain knowledge of how to transform to and
*     from related coordinate systems.

*  Inheritance:
*     The Frame class inherits from the Mapping class.

*  Attributes:
*     In addition to those attributes common to all Mappings, every
*     Frame also has the following attributes (if the Frame has only one
*     axis, the axis specifier can be omited from the following attribute
*     names):
*
*     - AlignSystem: Coordinate system used to align Frames
*     - Bottom(axis): Lowest axis value to display
*     - Digits/Digits(axis): Number of digits of precision
*     - Direction(axis): Display axis in conventional direction?
*     - Domain: Coordinate system domain
*     - Dut1: Difference between the UT1 and UTC timescale
*     - Epoch: Epoch of observation
*     - Format(axis): Format specification for axis values
*     - Label(axis): Axis label
*     - MatchEnd: Match trailing axes?
*     - MaxAxes: Maximum number of Frame axes to match
*     - MinAxes: Minimum number of Frame axes to match
*     - Naxes: Number of Frame axes
*     - NormUnit(axis): Normalised axis physical units
*     - ObsAlt: Geodetic altitude of observer
*     - ObsLat: Geodetic latitude of observer
*     - ObsLon: Geodetic longitude of observer
*     - Permute: Permute axis order?
*     - PreserveAxes: Preserve axes?
*     - Symbol(axis): Axis symbol
*     - System: Coordinate system used to describe the domain
*     - Title: Frame title
*     - Top(axis): Highest axis value to display
*     - Unit(axis): Axis physical units

*  Functions:
c     In addition to those functions applicable to all Mappings, the
c     following functions may also be applied to all Frames:
f     In addition to those routines applicable to all Mappings, the
f     following routines may also be applied to all Frames:
*
c     - astAngle: Calculate the angle subtended by two points at a third point
c     - astAxAngle: Find the angle from an axis, to a line through two points
c     - astAxDistance: Calculate the distance between two axis values
c     - astAxOffset: Calculate an offset along an axis
c     - astConvert: Determine how to convert between two coordinate systems
c     - astDistance: Calculate the distance between two points in a Frame
c     - astFindFrame: Find a coordinate system with specified characteristics
c     - astFormat: Format a coordinate value for a Frame axis
c     - astGetActiveUnit: Determines how the Unit attribute will be used
c     - astIntersect: Find the intersection between two geodesic curves
c     - astMatchAxes: Find any corresponding axes in two Frames
c     - astNorm: Normalise a set of Frame coordinates
c     - astOffset: Calculate an offset along a geodesic curve
c     - astOffset2: Calculate an offset along a geodesic curve in a 2D Frame
c     - astPermAxes: Permute the order of a Frame's axes
c     - astPickAxes: Create a new Frame by picking axes from an existing one
c     - astResolve: Resolve a vector into two orthogonal components
c     - astSetActiveUnit: Specify how the Unit attribute should be used
c     - astUnformat: Read a formatted coordinate value for a Frame axis
f     - AST_ANGLE: Find the angle subtended by two points at a third point
f     - AST_AXANGLE: Find the angle from an axis, to a line through two points
f     - AST_AXDISTANCE: Calculate the distance between two axis values
f     - AST_AXOFFSET: Calculate an offset along an axis
f     - AST_CONVERT: Determine how to convert between two coordinate systems
f     - AST_DISTANCE: Calculate the distance between two points in a Frame
f     - AST_FINDFRAME: Find a coordinate system with specified characteristics
f     - AST_FORMAT: Format a coordinate value for a Frame axis
f     - AST_GETACTIVEUNIT: Determines how the Unit attribute will be used
f     - AST_INTERSECT: Find the intersection between two geodesic curves
f     - AST_MATCHAXES: Find any corresponding axes in two Frames
f     - AST_NORM: Normalise a set of Frame coordinates
f     - AST_OFFSET: Calculate an offset along a geodesic curve
f     - AST_OFFSET2: Calculate an offset along a geodesic curve in a 2D Frame
f     - AST_PERMAXES: Permute the order of a Frame's axes
f     - AST_PICKAXES: Create a new Frame by picking axes from an existing one
f     - AST_RESOLVE: Resolve a vector into two orthogonal components
f     - AST_SETACTIVEUNIT: Specify how the Unit attribute should be used
f     - AST_UNFORMAT: Read a formatted coordinate value for a Frame axis

*  Notes:
*     - When used as a Mapping, a Frame implements a unit (null)
*     transformation in both the forward and inverse directions
*     (equivalent to a UnitMap). The Nin and Nout attribute values are
*     both equal to the number of Frame axes.

*  Copyright:
*     Copyright (C) 1997-2006 Council for the Central Laboratory of the
*     Research Councils

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public Licence as
*     published by the Free Software Foundation; either version 2 of
*     the Licence, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public Licence for more details.
*
*     You should have received a copy of the GNU General Public Licence
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     RFWS: R.F. Warren-Smith (Starlink)
*     DSB: B.S. Berry (Starlink)

*  History:
*     1-MAR-1996 (RFWS):
*        Original version.
*     4-JUN-1996 (RFWS):
*        Added the CleanDomain function to fold all Domain strings to
*        upper case and remove white space.
*     12-JUL-1996 (RFWS):
*        Over-ride the astReportPoints method to provide
*        Frame-specific formatting.
*     11-SEP-1996 (RFWS):
*        Added astGap (written by DSB).
*     10-JUN-1997 (RFWS):
*        Re-implemented astConvert and astFindFrame.
*     1-SEP-1997 (RFWS):
*        Added missing return statement in astAbbrev_.
*     14-NOV-1997 (RFWS):
*        Fixed wrong amount of memory allocated in ValidateAxisSelection.
*     20-NOV-1997 (RFWS):
*        Updated astConvert prologue.
*     22-DEC-1997 (RFWS):
*        Updated astConvert prologue again.
*     15-FEB-1998 (RFWS):
*        Added astUnformat.
*     2-MAR-1999 (RFWS);
*        Fixed missing STATUS arguments in examples for AST_FINDFRAME
*        prologue.
*     18-JUL-1999 (RFWS):
*        Fixed memory leak in ConvertX.
*     21-JUN-2001 (DSB):
*        Added methods astAngle and astOffset2.
*     29-AUG-2001 (DSB):
*        Added methods astAxDistance and astAxOffset.
*     4-SEP-2001 (DSB):
*        Added method astResolve.
*     9-SEP-2001 (DSB):
*        Added method astBear.
*     21-SEP-2001 (DSB):
*        Replaced astBear with astAxAngle.
*     10-OCT-2002 (DSB):
*        Added Top and Bottom.
*     15-NOV-2002 (DSB):
*        Moved the System and Epoch attributes from the SkyFrame class to
*        this class. Added virtual method astValidateSystem, astSystemString,
*        astSystemCode. Added attribute AlignSystem.
*     17-DEC-2002 (DSB):
*        Added the GetActiveUnit, TestActiveUnit and SetActiveUnit functions.
*     8-JAN-2003 (DSB):
*        Changed private InitVtab method to protected astInitFrameVtab
*        method.
*     15-SEP-2003 (DSB):
*        Allow Frame attribute names to include an axis specifier within
*        GetAttrib, SetAttrib, TestAttrib and ClearAttrib (eg "Domain(1)"
*        is now accepted as equivalent to "Domain").
*     24-JAN-2004 (DSB):
*        o  Added astFields.
*        o  Added argument "fmt" to Abbrev.
*     24-MAR-2004 (DSB):
*        Add protected function astIsUnitFrame.
*     7-SEP-2004 (DSB):
*        Modified SetUnit to exclude any trailing spaces
*     8-SEP-2004 (DSB):
*        - Added astResolvePoints.
*        - Override astEqual.
*     29-NOV-2004 (DSB):
*        - Set/Get/Test/ClearAttrib: Allow axis specifier to be omitted from
*        axis attribute names if the Frame only has one axis.
*     2-FEB-2005 (DSB):
*        - Avoid using astStore to allocate more storage than is supplied
*        in the "data" pointer. This can cause access violations since
*        astStore will then read beyond the end of the "data" area.
*     17-FEB-2005 (DSB):
*        - Change use of ActiveUnit flag so that both target and template
*        Frames must have active units in order for the Mapping to take
*        account of differences in units. Previously, the test was based
*        on the template Frame alone.
*     23-MAR-2005 (DSB):
*        - GetActiveUnit: Always return zero if the Frame contains any
*        SkyAxes.
*     5-APR-2005 (DSB):
*        Correct error checking in Clear/Get/Set/TestAttrib.
*     12-MAY-2005 (DSB):
*        Added astNormBox method.
*     16-JUN-2005 (DSB):
*        Added documentation for the TimeFrame class.
*     12-AUG-2005 (DSB):
*        Added ObsLat and ObsLon attributes.
*     1-MAR-2006 (DSB):
*        Replace astSetPermMap within DEBUG blocks by astBeginPM/astEndPM.
*     15-MAY-2006 (DSB):
*        Remove unused global variable parent_equal.
*     26-JUN-2006 (DSB):
*        Document the use of the Direction attribute by the Plot class.
*     30-JUN-2006 (DSB):
*        Allow astAbbrev to have a null "str1" value.
*     16-AUG-2006 (DSB):
*        Correct "Class Applicability" to "Applicability".
*     5-OCT-2006 (DSB):
*        Increase the number of digits used when formating a ObsLon or
*        ObsLat value in GetAttrib.
*     14-OCT-2006 (DSB):
*        - Add Dut1 attribute
*     26-JAN-2007 (DSB):
*        Fix bug in NewUnit that causes segvio when changing axis symbols
*        to accomodate changes in units.
*     17-MAY-2007 (DSB):
*        Added read-only attribute NormUnit.
*     21-MAY-2007 (DSB):
*        Use rather than ignore the value returned by astTestAxisDigits in
*        TestAttrib.
*     25-JUN-2007 (DSB):
*        Documentation typos.
*     26-NOV-2007 (DSB):
*        In Clear/Get/Set/TestAttrib, include any appropriate axis index in
*        the attribute name when attempting to get the attribute value from
*        the primary frame
*     17-NOV-2008 (DSB):
*        Correct parent class in invocation of astMAKE_ISA.
*     14-JAN-2009 (DSB):
*        Added astIntersect.
*     18-MAR-2009 (DSB):
*        Fixed bug in LineCrossing.
*     18-JUN-2000 (DSB):
*        Added ObsAlt attribute.
*     28-SEP-2009 (DSB):
*        Added astMatchAxes method.
*     22-MAR-2011 (DSB):
*        Add astFrameGrid method.
*     29-APR-2011 (DSB):
*        Prevent astFindFrame from matching a subclass template against a
*        superclass target.
*class--
*/

/* Module Macros. */
/* ============== */
/* Set the name of the class we are implementing. This indicates to
   the header files that define class interfaces that they should make
   "protected" symbols available. */
#define astCLASS Frame

/* Define numerical constants for use in this module. */
#define LABEL_BUFF_LEN 100       /* Max length of default axis Label string */
#define SYMBOL_BUFF_LEN 50       /* Max length of default axis Symbol string */
#define TITLE_BUFF_LEN 100       /* Max length of default title string */
#define GETATTRIB_BUFF_LEN 50    /* Max length of string returned by GetAttrib */
#define ASTFMTDECIMALYR_BUFF_LEN 50    /* Max length of string returned by GetAttrib */
#define ASTFORMATID_MAX_STRINGS 50     /* Number of string values buffer by astFormatID*/


/* Define the first and last acceptable System values. */
#define FIRST_SYSTEM AST__CART
#define LAST_SYSTEM AST__CART

/* Define macros to implement methods for accessing axis attributes. */
/*
*  Name:
*     MAKE_CLEAR

*  Purpose:
*     Implement a method to clear an attribute value for a Frame axis.

*  Type:
*     Private macro.

*  Synopsis:
*     #include "frame.h"
*     MAKE_CLEAR(attribute)

*  Class Membership:
*     Defined by the Frame class.

*  Description:
*     This macro expands to an implementation of a private member function of
*     the form:
*
*        static void Clear<Attribute>( AstFrame *this, int axis )
*
*     and an external interface function of the form:
*
*        void astClear<Attribute>_( AstFrame *this, int axis )
*
*     which implement a method for clearing an attribute value for a specified
*     axis of a Frame.

*  Parameters:
*     attribute
*        The name of the attribute to be cleared, as it appears in the
*        function name (e.g. Label in "astClearLabel").

*  Notes:
*     -  This macro assumes the existence of a method of the form:
*
*        void astClearAxis<Attribute>( AstAxis *this )
*
*     which clears the required attribute for an Axis object.
*     -  To avoid problems with some compilers, you should not leave any white
*     space around the macro arguments.
*/

/* Define the macro. */
#define MAKE_CLEAR(attribute) \
\
/* Private member function. */ \
/* ------------------------ */ \
static void Clear##attribute( AstFrame *this, int axis, int *status ) { \
   AstAxis *ax;                  /* Pointer to Axis object */ \
\
/* Check the global error status. */ \
   if ( !astOK ) return; \
\
/* Validate the axis index and obtain a pointer to the required Axis. */ \
   (void) astValidateAxis( this, axis, "astClear" #attribute ); \
   ax = astGetAxis( this, axis ); \
\
/* Clear the Axis attribute. */ \
   astClearAxis##attribute( ax ); \
\
/* Annul the Axis pointer. */ \
   ax = astAnnul( ax ); \
} \
\
/* External interface. */ \
/* ------------------- */ \
void astClear##attribute##_( AstFrame *this, int axis, int *status ) { \
\
/* Check the global error status. */ \
   if ( !astOK ) return; \
\
/* Invoke the required method via the virtual function table. */ \
   (**astMEMBER(this,Frame,Clear##attribute))( this, axis, status ); \
}

/*
*  Name:
*     MAKE_GET

*  Purpose:
*     Implement a method to get an attribute value for a Frame axis.

*  Type:
*     Private macro.

*  Synopsis:
#     #include "frame.h"
*     MAKE_GET(attribute,type,bad_value,default,assign_default)

*  Class Membership:
*     Defined by the Frame class.

*  Description:
*     This macro expands to an implementation of a private member function of
*     the form:
*
*        static <Type> Get<Attribute>( AstFrame *this, int axis )
*
*     and an external interface function of the form:
*
*        Type astGet<Attribute>_( AstFrame *this, int axis )
*
*     which implement a method for getting an attribute value for a specified
*     axis of a Frame.

*  Parameters:
*     attribute
*        The name of the attribute whose value is to be obtained, as
*        it appears in the function name (e.g. Label in
*        "astGetLabel").
*     type
*        The C type of the attribute.
*     bad_value
*        A constant value to return if the global error status is set, or if
*        the function fails.
*     default
*        A boolean (int) constant that indicates whether a new default value
*        should be returned by the method if the requested attribute has not
*        been set for the axis. If this value is zero, the axis default will
*        be used instead.
*     assign_default
*        An expression that evaluates to the new default value to be assigned.
*        This value is ignored if "default" is zero, but a valid (e.g.
*        constant) value should nevertheless be supplied.

*  Notes:
*     -  This macro assumes the existence of a method of the form:
*
*           <Type> astGetAxis<Attribute>( AstAxis *this )
*
*     which gets the required attribute for an Axis object.
*     -  To avoid problems with some compilers, you should not leave any white
*     space around the macro arguments.
*/

/* Define the macro. */
#define MAKE_GET(attribute,type,bad_value,default,assign_default) \
\
/* Private member function. */ \
/* ------------------------ */ \
static type Get##attribute( AstFrame *this, int axis, int *status ) { \
   AstAxis *ax;                  /* Pointer to Axis object */ \
   int digits_set;               /* Axis Digits attribute set? */ \
   int old_axis;                 /* Original (un-permuted) axis index */ \
   type result;                  /* Result to be returned */ \
\
/* Initialise. */ \
   result = (bad_value); \
\
/* Check the global error status. */ \
   if ( !astOK ) return result; \
\
/* Validate and permute the axis index and obtain a pointer to the required \
   Axis. */ \
   old_axis = axis; \
   axis = astValidateAxis( this, axis, "astGet" #attribute ); \
   ax = astGetAxis( this, old_axis ); \
\
/* Since the Axis is "managed" by the enclosing Frame, we next test if any \
   Axis attributes which may affect the result are undefined (i.e. have not \
   been explicitly set). If so, we over-ride them, giving them temporary \
   values dictated by the Frame. Only the Digits attribute is relevant \
   here. */ \
   digits_set = astTestAxisDigits( ax ); \
   if ( !digits_set ) astSetAxisDigits( ax, astGetDigits( this ) ); \
\
/* If the default value is to be over-ridden, test if the Axis attribute has \
   been set. Then, if required, obtain the attribute value from the Axis. */ \
   if ( !(default) || astTestAxis##attribute( ax ) ) { \
      result = astGetAxis##attribute( ax ); \
\
/* If required, assign the new default value. */ \
   } else { \
      result = (assign_default); \
   } \
\
/* Clear Axis attributes which were temporarily over-ridden above and annul \
   the Axis pointer. */ \
   if ( !digits_set ) astClearAxisDigits( ax ); \
   ax = astAnnul( ax ); \
\
/* If an error occurred, clear the result value. */ \
   if ( !astOK ) result = (bad_value); \
\
/* Return the result. */ \
   return result; \
} \
\
/* External interface. */ \
/* ------------------- */ \
type astGet##attribute##_( AstFrame *this, int axis, int *status ) { \
\
/* Check the global error status. */ \
   if ( !astOK ) return (bad_value); \
\
/* Invoke the required method via the virtual function table. */ \
   return (**astMEMBER(this,Frame,Get##attribute))( this, axis, status ); \
}

/*
*  Name:
*     MAKE_SET

*  Purpose:
*     Implement a method to set an attribute value for a Frame axis.

*  Type:
*     Private macro.

*  Synopsis:
*     #include "frame.h"
*     MAKE_SET(attribute,type)

*  Class Membership:
*     Defined by the Frame class.

*  Description:
*     This macro expands to an implementation of a private member function of
*     the form:
*
*        static void Set<Attribute>( AstFrame *this, int axis, <Type> value )
*
*     and an external interface function of the form:
*
*        void astSet<Attribute>_( AstFrame *this, int axis, <Type> value )
*
*     which implement a method for setting an attribute value for a specified
*     axis of a Frame.

*  Parameters:
*      attribute
*         The name of the attribute to be set, as it appears in the
*         function name (e.g. Label in "astSetLabel").
*      type
*         The C type of the attribute.

*  Notes:
*     -  This macro assumes the existence of a method of the form:
*
*           void astSetAxis<Attribute>( AstAxis *this, <Type> value )
*
*     which sets the required attribute for an Axis object.
*     -  To avoid problems with some compilers, you should not leave any white
*     space around the macro arguments.
*/

/* Define the macro. */
#define MAKE_SET(attribute,type) \
\
/* Private member function. */ \
/* ------------------------ */ \
static void Set##attribute( AstFrame *this, int axis, type value, int *status ) { \
   AstAxis *ax;                  /* Pointer to Axis object */ \
\
/* Check the global error status. */ \
   if ( !astOK ) return; \
\
/* Validate the axis index and obtain a pointer to the required Axis. */ \
   (void) astValidateAxis( this, axis, "astSet" #attribute ); \
   ax = astGetAxis( this, axis ); \
\
/* Set the Axis attribute value. */ \
   astSetAxis##attribute( ax, value ); \
\
/* Annul the Axis pointer. */ \
   ax = astAnnul( ax ); \
} \
\
/* External interface. */ \
/* ------------------- */ \
void astSet##attribute##_( AstFrame *this, int axis, type value, int *status ) { \
\
/* Check the global error status. */ \
   if ( !astOK ) return; \
\
/* Invoke the required method via the virtual function table. */ \
   (**astMEMBER(this,Frame,Set##attribute))( this, axis, value, status ); \
}

/*
*  Name:
*     MAKE_TEST

*  Purpose:
*     Implement a method to test if an attribute has been set for a Frame axis.

*  Type:
*     Private macro.

*  Synopsis:
*     #include "frame.h"
*     MAKE_TEST(attribute)

*  Class Membership:
*     Defined by the Frame class.

*  Description:
*     This macro expands to an implementation of a private member function of
*     the form:
*
*        static int Test<Attribute>( AstFrame *this, int axis )
*
*     and an external interface function of the form:
*
*        int astTest<Attribute>_( AstFrame *this, int axis )
*
*     which implement a method for testing if an attribute has been set for a
*     specified axis of a Frame.

*  Parameters:
*      attribute
*         The name of the attribute to be tested, as it appears in the
*         function name (e.g. Label in "astTestLabel").

*  Notes:
*     -  This macro assumes the existence of a method of the form:
*
*           void astTestAxis<Attribute>( AstAxis *this )
*
*     which tests the required attribute for an Axis object.
*     -  To avoid problems with some compilers, you should not leave any white
*     space around the macro arguments.
*/

/* Define the macro. */
#define MAKE_TEST(attribute) \
\
/* Private member function. */ \
/* ------------------------ */ \
static int Test##attribute( AstFrame *this, int axis, int *status ) { \
   AstAxis *ax;                  /* Pointer to Axis object */ \
   int result;                   /* Value to be returned */ \
\
/* Check the global error status. */ \
   if ( !astOK ) return 0; \
\
/* Validate the axis index and obtain a pointer to the required Axis. */ \
   (void) astValidateAxis( this, axis, "astTest" #attribute ); \
   ax = astGetAxis( this, axis ); \
\
/* Test if the attribute has been set. */ \
   result = astTestAxis##attribute( ax ); \
\
/* Annul the Axis pointer. */ \
   ax = astAnnul( ax ); \
\
/* If an error occurred, clear the result value. */ \
   if ( !astOK ) result = 0; \
\
/* Return the result. */ \
   return result; \
} \
\
/* External interface. */ \
/* ------------------- */ \
int astTest##attribute##_( AstFrame *this, int axis, int *status ) { \
\
/* Check the global error status. */ \
   if ( !astOK ) return 0; \
\
/* Invoke the required method via the virtual function table. */ \
   return (**astMEMBER(this,Frame,Test##attribute))( this, axis, status ); \
}

/* Header files. */
/* ============= */
/* Interface definitions. */
/* ---------------------- */

#include "globals.h"             /* Thread-safe global data access */
#include "error.h"               /* Error reporting facilities */
#include "memory.h"              /* Memory allocation facilities */
#include "object.h"              /* Base Object class */
#include "mapping.h"             /* Coordinate mappings (parent class) */
#include "pointset.h"            /* Sets of points */
#include "unitmap.h"             /* Unit Mapping */
#include "permmap.h"             /* Coordinate permutation Mapping */
#include "cmpmap.h"              /* Compound Mappings */
#include "axis.h"                /* Coordinate Axis */
#include "skyaxis.h"             /* Sky coordinate axes */
#include "skyframe.h"            /* Celestial coordinate frames */
#include "channel.h"             /* I/O channels */
#include "frame.h"               /* Interface definition for this class */
#include "frameset.h"            /* Collections of Frames */
#include "cmpframe.h"            /* Compound Frames */
#include "pal.h"                 /* SLALIB library interface */
#include "unit.h"                /* Units identification and mapping */
#include "globals.h"             /* Thread-safe global data access */

/* Error code definitions. */
/* ----------------------- */
#include "ast_err.h"             /* AST error codes */

/* C header files. */
/* --------------- */
#include <ctype.h>
#include <limits.h>
#include <math.h>
#include <stddef.h>
#include <stdio.h>
#include <string.h>

/* Module Variables. */
/* ================= */

/* Address of this static variable is used as a unique identifier for
   member of this class. */
static int class_check;

/* Pointers to parent class methods which are extended by this class. */
static const char *(* parent_getattrib)( AstObject *, const char *, int * );
static int (* parent_testattrib)( AstObject *, const char *, int * );
static void (* parent_clearattrib)( AstObject *, const char *, int * );
static void (* parent_setattrib)( AstObject *, const char *, int * );
static void (* parent_cleanattribs)( AstObject *, int * );

#if defined(THREAD_SAFE)
static int (* parent_managelock)( AstObject *, int, int, AstObject **, int * );
#endif

/* Define a variable to hold a SkyFrame which will be used for formatting
   and unformatting ObsLat and ObsLon values. */
static AstSkyFrame *skyframe;

/* Define macros for accessing each item of thread specific global data. */
#ifdef THREAD_SAFE

/* Define how to initialise thread-specific globals. */
#define GLOBAL_inits \
   globals->Class_Init = 0; \
   globals->GetAttrib_Buff[ 0 ] = 0; \
   globals->AstFormatID_Init = 0; \
   globals->AstFormatID_Istr = 0; \
   globals->Label_Buff[ 0 ] = 0; \
   globals->Symbol_Buff[ 0 ] = 0; \
   globals->Title_Buff[ 0 ] = 0; \
   globals->AstFmtDecimalYr_Buff[ 0 ] = 0;

/* Create the function that initialises global data for this module. */
astMAKE_INITGLOBALS(Frame)

#define class_init astGLOBAL(Frame,Class_Init)
#define class_vtab astGLOBAL(Frame,Class_Vtab)
#define getattrib_buff astGLOBAL(Frame,GetAttrib_Buff)
#define astformatid_strings astGLOBAL(Frame,AstFormatID_Strings)
#define astformatid_istr astGLOBAL(Frame,AstFormatID_Istr)
#define astformatid_init astGLOBAL(Frame,AstFormatID_Init)
#define label_buff astGLOBAL(Frame,Label_Buff)
#define symbol_buff astGLOBAL(Frame,Symbol_Buff)
#define title_buff astGLOBAL(Frame,Title_Buff)
#define astfmtdecimalyr_buff astGLOBAL(Frame,AstFmtDecimalYr_Buff)



/* If thread safety is not needed, declare and initialise globals at static
   variables. */
#else

/* Buffer returned by GetAttrib. */
static char getattrib_buff[ GETATTRIB_BUFF_LEN + 1 ];

/* Strings returned by astFormatID */
static char *astformatid_strings[ ASTFORMATID_MAX_STRINGS ];

/* Offset of next string in "AstFormatID_Strings" */
static int astformatid_istr;

/* "AstFormatID_Strings" array initialised? */
static int astformatid_init;

/* Default Label string buffer */
static char label_buff[ LABEL_BUFF_LEN + 1 ];

/* Default Symbol buffer */
static char symbol_buff[ SYMBOL_BUFF_LEN + 1 ];

/* Default Title string buffer */
static char title_buff[ TITLE_BUFF_LEN + 1 ];

/* Buffer for result string */
static char astfmtdecimalyr_buff[ ASTFMTDECIMALYR_BUFF_LEN + 1 ];


/* Define the class virtual function table and its initialisation flag
   as static variables. */
static AstFrameVtab class_vtab;   /* Virtual function table */
static int class_init = 0;       /* Virtual function table initialised? */

#endif


/* Prototypes for Private Member Functions. */
/* ======================================== */
static AstAxis *GetAxis( AstFrame *, int, int * );
static AstFrame *PickAxes( AstFrame *, int, const int[], AstMapping **, int * );
static AstFrameSet *Convert( AstFrame *, AstFrame *, const char *, int * );
static AstFrameSet *ConvertX( AstFrame *, AstFrame *, const char *, int * );
static AstFrameSet *FindFrame( AstFrame *, AstFrame *, const char *, int * );
static void MatchAxes( AstFrame *, AstFrame *, int *, int * );
static void MatchAxesX( AstFrame *, AstFrame *, int *, int * );
static AstLineDef *LineDef( AstFrame *, const double[2], const double[2], int * );
static AstPointSet *FrameGrid( AstFrame *, int, const double *, const double *, int * );
static AstPointSet *ResolvePoints( AstFrame *, const double [], const double [], AstPointSet *, AstPointSet *, int * );
static AstPointSet *Transform( AstMapping *, AstPointSet *, int, AstPointSet *, int * );
static char *CleanDomain( char *, int * );
static const char *Abbrev( AstFrame *, int, const char *, const char *, const char *, int * );
static const char *Format( AstFrame *, int, double, int * );
static const char *GetAttrib( AstObject *, const char *, int * );
static const char *GetDefaultLabel( int, int * );
static const char *GetDefaultSymbol( AstFrame *, int, int * );
static const char *GetDefaultTitle( AstFrame *, int * );
static const char *GetDomain( AstFrame *, int * );
static const char *GetFormat( AstFrame *, int, int * );
static const char *GetLabel( AstFrame *, int, int * );
static const char *GetNormUnit( AstFrame *, int, int * );
static const char *GetSymbol( AstFrame *, int, int * );
static const char *GetTitle( AstFrame *, int * );
static const char *GetUnit( AstFrame *, int, int * );
static const int *GetPerm( AstFrame *, int * );
static double Angle( AstFrame *, const double[], const double[], const double[], int * );
static double AxAngle( AstFrame *, const double[], const double[], int, int * );
static double AxDistance( AstFrame *, int, double, double, int * );
static double AxOffset( AstFrame *, int, double, double, int * );
static double Distance( AstFrame *, const double[], const double[], int * );
static double Gap( AstFrame *, int, double, int *, int * );
static double Offset2( AstFrame *, const double[2], double, double, double[2], int * );
static int AxIn( AstFrame *, int, double, double, double, int, int * );
static int ConsistentMaxAxes( AstFrame *, int, int * );
static int ConsistentMinAxes( AstFrame *, int, int * );
static int DefaultMaxAxes( AstFrame *, int * );
static int DefaultMinAxes( AstFrame *, int * );
static int Equal( AstObject *, AstObject *, int * );
static int Fields( AstFrame *, int, const char *, const char *, int, char **, int *, double *, int * );
static int GetDigits( AstFrame *, int * );
static int GetDirection( AstFrame *, int, int * );
static int GetIsLinear( AstMapping *, int * );
static int GetIsSimple( AstMapping *, int * );
static int LineContains( AstFrame *, AstLineDef *, int, double *, int * );
static int LineCrossing( AstFrame *, AstLineDef *, AstLineDef *, double **, int * );
static void CleanAttribs( AstObject *, int * );
static void LineOffset( AstFrame *, AstLineDef *, double, double, double[2], int * );

static double GetTop( AstFrame *, int, int * );
static int TestTop( AstFrame *, int, int * );
static void ClearTop( AstFrame *, int, int * );
static void SetTop( AstFrame *, int, double, int * );

static double GetBottom( AstFrame *, int, int * );
static int TestBottom( AstFrame *, int, int * );
static void ClearBottom( AstFrame *, int, int * );
static void SetBottom( AstFrame *, int, double, int * );

static AstSystemType GetSystem( AstFrame *, int * );
static int TestSystem( AstFrame *, int * );
static void ClearSystem( AstFrame *, int * );
static void SetSystem( AstFrame *, AstSystemType, int * );

static AstSystemType GetAlignSystem( AstFrame *, int * );
static int TestAlignSystem( AstFrame *, int * );
static void ClearAlignSystem( AstFrame *, int * );
static void SetAlignSystem( AstFrame *, AstSystemType, int * );

static double GetEpoch( AstFrame *, int * );
static int TestEpoch( AstFrame *, int * );
static void ClearEpoch( AstFrame *, int * );
static void SetEpoch( AstFrame *, double, int * );

static double GetObsLat( AstFrame *, int * );
static int TestObsLat( AstFrame *, int * );
static void ClearObsLat( AstFrame *, int * );
static void SetObsLat( AstFrame *, double, int * );

static double GetObsLon( AstFrame *, int * );
static int TestObsLon( AstFrame *, int * );
static void ClearObsLon( AstFrame *, int * );
static void SetObsLon( AstFrame *, double, int * );

static double GetObsAlt( AstFrame *, int * );
static int TestObsAlt( AstFrame *, int * );
static void ClearObsAlt( AstFrame *, int * );
static void SetObsAlt( AstFrame *, double, int * );

static double GetDut1( AstFrame *, int * );
static int TestDut1( AstFrame *, int * );
static void ClearDut1( AstFrame *, int * );
static void SetDut1( AstFrame *, double, int * );

static int GetActiveUnit( AstFrame *, int * );
static int TestActiveUnit( AstFrame *, int * );
static void SetActiveUnit( AstFrame *, int, int * );

static int GetFrameFlags( AstFrame *, int * );
static int *MapSplit( AstMapping *, int, const int *, AstMapping **, int * );
static int GetMatchEnd( AstFrame *, int * );
static int GetMaxAxes( AstFrame *, int * );
static int GetMinAxes( AstFrame *, int * );
static int GetNaxes( AstFrame *, int * );
static int GetNin( AstMapping *, int * );
static int GetNout( AstMapping *, int * );
static int GetPermute( AstFrame *, int * );
static int GetPreserveAxes( AstFrame *, int * );
static int Match( AstFrame *, AstFrame *, int, int **, int **, AstMapping **, AstFrame **, int * );
static int SubFrame( AstFrame *, AstFrame *, int, const int *, const int *, AstMapping **, AstFrame **, int * );
static int TestAttrib( AstObject *, const char *, int * );
static int TestDigits( AstFrame *, int * );
static int TestDirection( AstFrame *, int, int * );
static int TestDomain( AstFrame *, int * );
static int TestFormat( AstFrame *, int, int * );
static int TestLabel( AstFrame *, int, int * );
static int TestMatchEnd( AstFrame *, int * );
static int TestMaxAxes( AstFrame *, int * );
static int TestMinAxes( AstFrame *, int * );
static int TestPermute( AstFrame *, int * );
static int TestPreserveAxes( AstFrame *, int * );
static int TestSymbol( AstFrame *, int, int * );
static int TestTitle( AstFrame *, int * );
static int TestUnit( AstFrame *, int, int * );
static int IsUnitFrame( AstFrame *, int * );
static int Unformat( AstFrame *, int, const char *, double *, int * );
static int ValidateAxis( AstFrame *, int, const char *, int * );
static AstSystemType ValidateSystem( AstFrame *, AstSystemType, const char *, int * );
static AstSystemType SystemCode( AstFrame *, const char *, int * );
static const char *SystemString( AstFrame *, AstSystemType, int * );
static void AddUnderscores( char *, int * );
static void CheckPerm( AstFrame *, const int *, const char *, int * );
static void ClearAttrib( AstObject *, const char *, int * );
static void ClearDigits( AstFrame *, int * );
static void ClearDirection( AstFrame *, int, int * );
static void ClearDomain( AstFrame *, int * );
static void ClearFormat( AstFrame *, int, int * );
static void ClearLabel( AstFrame *, int, int * );
static void ClearMatchEnd( AstFrame *, int * );
static void ClearMaxAxes( AstFrame *, int * );
static void ClearMinAxes( AstFrame *, int * );
static void ClearPermute( AstFrame *, int * );
static void ClearPreserveAxes( AstFrame *, int * );
static void ClearSymbol( AstFrame *, int, int * );
static void ClearTitle( AstFrame *, int * );
static void ClearUnit( AstFrame *, int, int * );
static void Copy( const AstObject *, AstObject *, int * );
static void Delete( AstObject *, int * );
static void Dump( AstObject *, AstChannel *, int * );
static void Intersect( AstFrame *, const double[2], const double[2], const double[2], const double[2], double[2], int * );
static void Norm( AstFrame *, double[], int * );
static void NormBox( AstFrame *, double[], double[], AstMapping *, int * );
static void Offset( AstFrame *, const double[], const double[], double, double[], int * );
static void Overlay( AstFrame *, const int *, AstFrame *, int * );
static void PermAxes( AstFrame *, const int[], int * );
static void PrimaryFrame( AstFrame *, int, AstFrame **, int *, int * );
static void ReportPoints( AstMapping *, int, AstPointSet *, AstPointSet *, int * );
static void Resolve( AstFrame *, const double [], const double [], const double [], double [], double *, double *, int * );
static void SetAttrib( AstObject *, const char *, int * );
static void SetAxis( AstFrame *, int, AstAxis *, int * );
static void SetDigits( AstFrame *, int, int * );
static void SetDirection( AstFrame *, int, int, int * );
static void SetDomain( AstFrame *, const char *, int * );
static void SetFormat( AstFrame *, int, const char *, int * );
static void SetFrameFlags( AstFrame *, int, int * );
static void SetLabel( AstFrame *, int, const char *, int * );
static void SetMatchEnd( AstFrame *, int, int * );
static void SetMaxAxes( AstFrame *, int, int * );
static void SetMinAxes( AstFrame *, int, int * );
static void SetPermute( AstFrame *, int, int * );
static void SetPreserveAxes( AstFrame *, int, int * );
static void SetSymbol( AstFrame *, int, const char *, int * );
static void SetTitle( AstFrame *, const char *, int * );
static void SetUnit( AstFrame *, int, const char *, int * );
static void NewUnit( AstAxis *, const char *, const char *, const char *, const char *, int * );
static void ValidateAxisSelection( AstFrame *, int, const int *, const char *, int * );

#if defined(THREAD_SAFE)
static int ManageLock( AstObject *, int, int, AstObject **, int * );
#endif

/* Member functions. */
/* ================= */
static const char *Abbrev( AstFrame *this, int axis,  const char *fmt,
                           const char *str1, const char *str2, int *status ) {
/*
*+
*  Name:
*     astAbbrev

*  Purpose:
*     Abbreviate a formatted Frame axis value by skipping leading fields.

*  Type:
*     Protected virtual function.

*  Synopsis:
*     #include "frame.h"
*     const char *astAbbrev( AstFrame *this, int axis, const char *fmt,
*                            const char *str1, const char *str2 )

*  Class Membership:
*     Frame method.

*  Description:
*     This function compares two Frame axis values that have been
*     formatted (using astFormat) and determines if they have any
*     redundant leading fields (i.e. leading fields in common which
*     can be suppressed when tabulating the values or plotting them on
*     the axis of a graph).

*  Parameters:
*     this
*        Pointer to the Frame.
*     axis
*        The number of the Frame axis for which the values have been
*        formatted (axis numbering starts at zero for the first axis).
*     fmt
*        Pointer to a constant null-terminated string containing the
*        format specification used to format the two values.
*     str1
*        Pointer to a constant null-terminated string containing the
*        first formatted value. If this is null, the returned pointer
*        points to the start of the final field in str2.
*     str2
*        Pointer to a constant null-terminated string containing the
*        second formatted value.

*  Returned Value:
*     A pointer into the "str2" string which locates the first
*     character in the first field that differs between the two
*     formatted values.
*
*     If the two values have no leading fields in common, the returned
*     value will point at the start of string "str2". If the two
*     values are equal, it will point at the terminating null at the
*     end of this string.

*  Notes:
*     - This function assumes that the format specification used was
*     the same when both values were formatted and that they both
*     apply to the same Frame axis.
*     - A pointer to the start of "str2" will be returned if this
*     function is invoked with the global error status set, or if it
*     should fail for any reason.
*-
*/

/* Local Variables: */
   AstAxis *ax;                  /* Pointer to Axis object */
   const char *result;           /* Result pointer to return */

/* Initialise. */
   result = str2;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Validate the axis index and obtain a pointer to the required
   Axis. */
   (void) astValidateAxis( this, axis, "astAbbrev" );
   ax = astGetAxis( this, axis );

/* Invoke the Axis astAxisAbbrev method to perform the processing. */
   result = astAxisAbbrev( ax, fmt, str1, str2 );

/* Annul the Axis pointer. */
   ax = astAnnul( ax );

/* If an error occurred, clear the returned value. */
   if ( !astOK ) result = str2;

/* Return the result. */
   return result;
}

static void AddUnderscores( char *string, int *status ) {
/*
*  Name:
*     AddUnderscores

*  Purpose:
*     Add underscores to a string in place of white space.

*  Type:
*     Private function.

*  Synopsis:
*     #include "frame.h"
*     void AddUnderscores( char *string, int *status )

*  Class Membership:
*     Frame member function.

*  Description:
*     This function changes all white space characters in a string into
*     the underscore character '_'.

*  Parameters:
*     this
*        Pointer to the Frame.
*     string
*        Pointer to the null terminated string to be processed.
*     status
*        Pointer to the inherited status variable.
*/

/* Local Variables. */
   int i;                        /* Loop counter for characters */

/* Inspect each character in the string. */
   for ( i = 0; string[ i ]; i++ ) {

/* If it is a white space character, replace it with an underscore. */
      if ( isspace( string[ i ] ) ) string[ i ] = '_';
   }
}

static double Angle( AstFrame *this, const double a[],
                     const double b[], const double c[], int *status ) {
/*
*++
*  Name:
c     astAngle
f     AST_ANGLE

*  Purpose:
*     Calculate the angle subtended by two points at a third point.

*  Type:
*     Public virtual function.

*  Synopsis:
c     #include "frame.h"
c     double astAngle( AstFrame *this, const double a[], const double b[],
c                      const double c[] )
f     RESULT = AST_ANGLE( THIS, A, B, C, STATUS )

*  Class Membership:
*     Frame method.

*  Description:
c     This function
f     This routine
*     finds the angle at point B between the line joining points A and B,
*     and the line joining points C and B. These lines will in fact be
*     geodesic curves appropriate to the Frame in use. For instance, in
*     SkyFrame, they will be great circles.

*  Parameters:
c     this
f     THIS = INTEGER (Given)
*        Pointer to the Frame.
c     a
f     A( * ) = DOUBLE PRECISION (Given)
c        An array of double, with one element for each Frame axis
f        An array with one element for each Frame axis
*        (Naxes attribute) containing the coordinates of the first point.
c     b
f     B( * ) = DOUBLE PRECISION (Given)
c        An array of double, with one element for each Frame axis
f        An array with one element for each Frame axis
*        (Naxes attribute) containing the coordinates of the second point.
c     c
f     C( * ) = DOUBLE PRECISION (Given)
c        An array of double, with one element for each Frame axis
f        An array with one element for each Frame axis
*        (Naxes attribute) containing the coordinates of the third point.
f     STATUS = INTEGER (Given and Returned)
f        The global status.

*  Returned Value:
c     astAngle
f     AST_ANGLE = DOUBLE PRECISION
*        The angle in radians, from the line AB to the line CB. If the
*        Frame is 2-dimensional, it will be in the range $\pm \pi$,
*        and positive rotation is in the same sense as rotation from
*        the positive direction of axis 2 to the positive direction of
*        axis 1. If the Frame has more than 2 axes, a positive value will
*        always be returned in the range zero to $\pi$.

*  Notes:
*     - A value of AST__BAD will also be returned if points A and B are
*     co-incident, or if points B and C are co-incident.
*     - A value of AST__BAD will also be returned if this function is
c     invoked with the AST error status set, or if it should fail for
f     invoked with STATUS set to an error value, or if it should fail for
*     any reason.
*--
*/

/* Local Variables: */
   double *ab;                   /* Pointer to vector AB */
   double *cb;                   /* Pointer to vector CB */
   double cos;                   /* cosine of required angle */
   double anga;                  /* Angle from +ve Y to the line BA */
   double angc;                  /* Angle from +ve Y to the line BC */
   double result;                /* Result value to return */
   double sla;                   /* Squared length of vector AB */
   double slc;                   /* Squared length of vector CB */
   double sp;                    /* Scalar product of AB and CB */
   int axis;                     /* Axis index */
   int naxes;                    /* Number of Frame axes */
   int ok;                       /* Supplied points OK? */

/* Initialise. */
   result = AST__BAD;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Assume everything is ok */
   ok = 1;

/* Obtain the number of Frame axes. */
   naxes = astGetNaxes( this );

/* Obtain workspace. */
   ab = (double *) astMalloc( sizeof(double)*naxes );
   cb = (double *) astMalloc( sizeof(double)*naxes );

/* Check all positions are good, and form the vectors from b to a, and
   from b to c. Also find the squared length of each vector. */
   if( astOK ) {
      sla = 0.0;
      slc = 0.0;
      for( axis = 0; axis < naxes; axis++ ) {
         if( a[ axis ] == AST__BAD || b[ axis ] == AST__BAD ||
             c[ axis ] == AST__BAD ) {
            ok = 0;
            break;
         } else {
            ab[ axis ] = a[ axis ] - b[ axis ];
            cb[ axis ] = c[ axis ] - b[ axis ];
            sla += ( ab[ axis ] )*( ab[ axis ] );
            slc += ( cb[ axis ] )*( cb[ axis ] );
         }
      }

/* Check that neither of the vectors have zero length. */
      if( sla == 0 || slc == 0 ) ok = 0;

/* Only proceed if these checks were passed. */
      if ( ok ) {

/* Deal first with 2-dimensional Frames. */
         if( naxes == 2 ) {

/* Find the angle from +ve Y to the line BA. */
            anga = atan2( ab[ 0 ], ab[ 1 ] );

/* Find the angle from +ve Y to the line BC. */
            angc = atan2( cb[ 0 ], cb[ 1 ] );

/* Find the difference, folded into the range +/- PI. */
            result = palSlaDrange( angc - anga );

/* Now deal with Frames with more than 2 axes. */
         } else {

/* Form the scalar product of the two vectors. */
            sp = 0.0;
            for( axis = 0; axis < naxes; axis++ ) {
               sp += ab[ axis ]*cb[ axis ];
            }

/* Derive the required angle from the normalized scalar product. */
            cos = sp/sqrt( sla*slc );
            if( cos > 1.0 ) {
               cos = 1.0;
            } else if( cos < -1.0 ) {
               cos = -1.0;
            }
            result =acos( cos );
         }
      }
   }

/* Free the work space. */
   ab = (double *) astFree( (void *) ab );
   cb = (double *) astFree( (void *) cb );

/* Return the result. */
   return result;
}

static double AxAngle( AstFrame *this, const double a[], const double b[], int axis, int *status ) {
/*
*++
*  Name:
c     astAxAngle
f     AST_AXANGLE

*  Purpose:
*     Returns the angle from an axis, to a line through two points.

*  Type:
*     Public virtual function.

*  Synopsis:
c     #include "frame.h"
c     double astAxAngle( AstFrame *this, const double a[], const double b[], int axis )
f     RESULT = AST_AXANGLE( THIS, A, B, AXIS, STATUS )

*  Class Membership:
*     Frame method.

*  Description:
c     This function
f     This routine
*     finds the angle, as seen from point A, between the positive
*     direction of a specified axis, and the geodesic curve joining point
*     A to point B.

*  Parameters:
c     this
f     THIS = INTEGER (Given)
*        Pointer to the Frame.
c     a
f     A( * ) = DOUBLE PRECISION (Given)
c        An array of double, with one element for each Frame axis
f        An array with one element for each Frame axis
*        (Naxes attribute) containing the coordinates of the first point.
c     b
f     B( * ) = DOUBLE PRECISION (Given)
c        An array of double, with one element for each Frame axis
f        An array with one element for each Frame axis
*        (Naxes attribute) containing the coordinates of the second point.
c     axis
f     AXIS = INTEGER (Given)
*        The number of the Frame axis from which the angle is to be
*        measured (axis numbering starts at 1 for the first axis).
f     STATUS = INTEGER (Given and Returned)
f        The global status.

*  Returned Value:
c     astAxAngle
f     AST_AXANGLE = DOUBLE PRECISION
*        The angle in radians, from the positive direction of the
*        specified axis, to the line AB. If the Frame is 2-dimensional,
*        it will be in the range [-PI/2,+PI/2], and positive rotation is in
*        the same sense as rotation from the positive direction of axis 2
*        to the positive direction of axis 1. If the Frame has more than 2
*        axes, a positive value will always be returned in the range zero
*        to PI.

*  Notes:
c     - The geodesic curve used by this function is the path of
f     - The geodesic curve used by this routine is the path of
*     shortest distance between two points, as defined by the
c     astDistance function.
f     AST_DISTANCE function.
*     - This function will return "bad" coordinate values (AST__BAD)
*     if any of the input coordinates has this value, or if the require
*     position angle is undefined.
*--
*/

/* Local Variables: */
   double *aa;                   /* Pointer to third point */
   double ab;                    /* Absolute value of component */
   double mxab;                  /* Largest absolute value of component */
   double result;                /* The returned value */
   int iaxis;                    /* Axis index */
   int naxes;                    /* Number of Frame axes */
   int ok;                       /* Are values ok to use? */

/* Initialise. */
   result = AST__BAD;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Validate the axis index. */
   (void) astValidateAxis( this, axis - 1, "astAxAngle" );

/* Obtain the number of Frame axes. */
   naxes = astGetNaxes( this );

/* Obtain workspace. */
   aa = (double *) astMalloc( sizeof(double)*naxes );

/* Create a position which is offset slightly from point A in the
   positive direction of the specified axis. Also get the largest absolute
   value of any component of the vector AB. */
   if( astOK ) {
      ok = 1;
      mxab = 0.0;

      for( iaxis = 0; iaxis < naxes; iaxis++ ) {
         if( a[ iaxis ] != AST__BAD && b[ iaxis ] != AST__BAD ) {
            aa[ iaxis ] = a[ iaxis ];
            ab = fabs( a[ iaxis ] - b[ iaxis ] );
            if( ab > mxab ) mxab = ab;
         } else {
            ok = 0;
            break;
         }
      }

      if( ok ) {

         if( a[ axis - 1 ] != 0.0 ) {
            aa[ axis - 1 ] += 10000.0*DBL_EPSILON*fabs( a[ axis - 1 ] );

         } else if( b[ axis - 1 ] != 0.0 ) {
            aa[ axis - 1 ] = 10000.0*DBL_EPSILON*fabs( b[ iaxis - 1 ] );

         } else if( mxab != 0.0 ) {
            aa[ axis - 1 ] = 10000.0*DBL_EPSILON*mxab;

         } else {
            aa[ axis - 1 ] = 1.0;
         }

/* Use astAngle to get the required angle. */
         result = astAngle( this, aa, a, b );
      }
   }

/* Free the workspace. */
   aa = (double *) astFree( (void *) aa );

/* Return the result. */
   return result;

}

static double AxDistance( AstFrame *this, int axis, double v1, double v2, int *status ) {
/*
*++
*  Name:
c     astAxDistance
f     AST_AXDISTANCE

*  Purpose:
*     Find the distance between two axis values.

*  Type:
*     Public virtual function.

*  Synopsis:
c     #include "frame.h"
c     double astAxDistance( AstFrame *this, int axis, double v1, double v2 )
f     RESULT = AST_AXDISTANCE( THIS, AXIS, V1, V2, STATUS )

*  Class Membership:
*     Frame method.

*  Description:
c     This function returns a signed value representing the axis increment
f     This routine returns a signed value representing the axis increment
*     from axis value v1 to axis value v2.
*
*     For a simple Frame, this is a trivial operation returning the
*     difference between the two axis values. But for other derived classes
*     of Frame (such as a SkyFrame) this is not the case.

*  Parameters:
c     this
f     THIS = INTEGER (Given)
*        Pointer to the Frame.
c     axis
f     AXIS = INTEGER (Given)
*        The index of the axis to which the supplied values refer. The
*        first axis has index 1.
c     v1
f     V1 = DOUBLE PRECISION (Given)
*        The first axis value.
c     v2
f     V2 = DOUBLE PRECISION (Given)
*        The second axis value.
f     STATUS = INTEGER (Given and Returned)
f        The global status.

*  Returned Value:
c     astAxDistance
f     AST_AXDISTANCE = DOUBLE PRECISION
*        The distance from the first to the second axis value.

*  Notes:
*     - This function will return a "bad" result value (AST__BAD) if
*     any of the input values has this value.
*     - A "bad" value will also be returned if this function is
c     invoked with the AST error status set, or if it should fail for
f     invoked with STATUS set to an error value, or if it should fail for
*     any reason.
*--

*  Implementation Deficiencies;
*     - The protected interface for this function uses 1-based axis
*     numbering (like the public interface), rather than the more usual
*     zero-based system used by all other protected interfaces. There is
*     no real reason for this, and it should be changed at some time.

*/

/* Local Variables: */
   AstAxis *ax;                  /* Pointer to Axis object */
   double result;                /* The returned answer */

/* Initialise. */
   result = AST__BAD;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Validate the axis index and obtain a pointer to the required Axis. */
   (void) astValidateAxis( this, axis - 1, "astAxDistance" );
   ax = astGetAxis( this, axis - 1 );

/* Use the AxisDistance method associated with the Axis. */
   if( astOK ) result = astAxisDistance( ax, v1, v2 );

/* Annul the Axis pointer. */
   ax = astAnnul( ax );

/* Return the result. */
   return result;

}

static int AxIn( AstFrame *this, int axis, double lo, double hi, double val,
                 int closed, int *status ){
/*
*+
*  Name:
*     astAxIn

*  Purpose:
*     Test if an axis value lies within a given interval.

*  Type:
*     Protected virtual function.

*  Synopsis:
*     #include "frame.h"
*     int astAxIn( AstFrame *this, int axis, double lo, double hi, double val,
*                  int closed )

*  Class Membership:
*     Frame member function.

*  Description:
*     This function returns non-zero if a given axis values lies within a
*     given axis interval.

*  Parameters:
*     this
*        Pointer to the Frame.
*     axis
*        The index of the axis. The first axis has index 0.
*     lo
*        The lower axis limit of the interval.
*     hi
*        The upper axis limit of the interval.
*     val
*        The axis value to be tested.
*     closed
*        If non-zero, then the lo and hi axis values are themselves
*        considered to be within the interval. Otherwise they are outside.

*  Returned Value:
*     Non-zero if the test value is inside the interval.

*  Applicability:
*     Frame
*        Uses simple Euclidean test
*     SkyFrame
*        All angles which are numerically between "lo" and "hi" are within
*        the interval. Angle outside this range are also within the interval
*        if they can be brought into the range by addition or subtraction
*        of a multiple of 2.PI.
*-
*/

/* Local Variables: */
   AstAxis *ax;                  /* Pointer to Axis object */
   int result;                   /* Returned value */

/* For speed, omit the astOK check and axis validation (since this is
   protected code, AST should get it right). Obtain a pointer to the
   required Axis. */
   ax = astGetAxis( this, axis );

/* Use the AxisIn method associated with the Axis. */
   result = ax ? astAxisIn( ax, lo, hi, val, closed ) : 0;

/* Annul the Axis pointer. */
   ax = astAnnul( ax );

/* Return the result. */
   return result;
}

static double AxOffset( AstFrame *this, int axis, double v1, double dist, int *status ) {
/*
*++
*  Name:
c     astAxOffset
f     AST_AXOFFSET

*  Purpose:
*     Add an increment onto a supplied axis value.

*  Type:
*     Public virtual function.

*  Synopsis:
c     #include "frame.h"
c     double astAxOffset( AstFrame *this, int axis, double v1, double dist )
f     RESULT = AST_AXOFFSET( THIS, AXIS, V1, DIST, STATUS )

*  Class Membership:
*     Frame method.

*  Description:
c     This function returns an axis value formed by adding a signed axis
f     This routine returns an axis value formed by adding a signed axis
*     increment onto a supplied axis value.
*
*     For a simple Frame, this is a trivial operation returning the
*     sum of the two supplied values. But for other derived classes
*     of Frame (such as a SkyFrame) this is not the case.

*  Parameters:
c     this
f     THIS = INTEGER (Given)
*        Pointer to the Frame.
c     axis
f     AXIS = INTEGER (Given)
*        The index of the axis to which the supplied values refer. The
*        first axis has index 1.
c     v1
f     V1 = DOUBLE PRECISION (Given)
*        The original axis value.
c     dist
f     DIST = DOUBLE PRECISION (Given)
*        The axis increment to add to the original axis value.
f     STATUS = INTEGER (Given and Returned)
f        The global status.

*  Returned Value:
c     astAxOffset
f     AST_AXOFFSET = DOUBLE PRECISION
*        The incremented axis value.

*  Notes:
*     - This function will return a "bad" result value (AST__BAD) if
*     any of the input values has this value.
*     - A "bad" value will also be returned if this function is
c     invoked with the AST error status set, or if it should fail for
f     invoked with STATUS set to an error value, or if it should fail for
*     any reason.
*--
*/

/* Local Variables: */
   AstAxis *ax;                  /* Pointer to Axis object */
   double result;                /* The returned answer */

/* Initialise. */
   result = AST__BAD;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Validate the axis index and obtain a pointer to the required Axis. */
   (void) astValidateAxis( this, axis - 1, "astAxOffset" );
   ax = astGetAxis( this, axis - 1 );

/* Use the AxisOffset method associated with the Axis. */
   if( astOK ) result = astAxisOffset( ax, v1, dist );

/* Annul the Axis pointer. */
   ax = astAnnul( ax );

/* Return the result. */
   return result;

}

static void CheckPerm( AstFrame *this, const int *perm, const char *method, int *status ) {
/*
*+
*  Name:
*     astCheckPerm

*  Purpose:
*     Check that an array contains a valid permutation.

*  Type:
*     Protected virtual function.

*  Synopsis:
*     #include "frame.h"
*     void astCheckPerm( AstFrame *this, const int *perm, const char *method )

*  Class Membership:
*     Frame method.

*  Description:
*     This function checks the validity of a permutation array that
*     will be used to permute the order of a Frame's axes. If the
*     permutation specified by the array is not valid, an error is
*     reported and the global error status is set. Otherwise, the
*     function returns without further action.

*  Parameters:
*     this
*        Pointer to the Frame.
*     perm
*        Pointer to an array of integers with the same number of
*        elements as there are axes in the Frame. For each axis, the
*        corresponding integer gives the (zero based) axis index to be
*        used to identify the information for that axis (using the
*        un-permuted axis numbering). To be valid, the integers in
*        this array should therefore all lie in the range zero to
*        (naxes-1) inclusive, where "naxes" is the number of Frame
*        axes, and each value should occur exactly once.
*     method
*        Pointer to a constant null-terminated character string
*        containing the name of the method that invoked this function
*        to validate a permutation array. This method name is used
*        solely for constructing error messages.

*  Notes:
*     - Error messages issued by this function refer to the external
*     (public) numbering system used for axes (which is one-based),
*     whereas zero-based axis indices are used internally.
*-
*/

/* Local Variables: */
   int *there;                   /* Pointer to temporary array */
   int axis;                     /* Loop counter for axes */
   int naxes;                    /* Number of Frame axes */
   int valid;                    /* Permutation array is valid? */

/* Check the global error status. */
   if ( !astOK ) return;

/* Initialise. */
   valid = 1;

/* Obtain the number of Frame axes and allocate a temporary array of integers
   with the same number of elements. */
   naxes = astGetNaxes( this );
   there = astMalloc( sizeof( int ) * (size_t) naxes );
   if ( astOK ) {

/* Initialise the temporary array to zero. */
      for ( axis = 0; axis < naxes; axis++ ) there[ axis ] = 0;

/* Scan the permutation array, checking that each permuted axis index it
   contains is within the correct range. Note an error and quit checking
   if an invalid value is found. */
      for ( axis = 0; axis < naxes; axis++ ) {
         if ( ( perm[ axis ] < 0 ) || ( perm[ axis ] >= naxes ) ) {
            valid = 0;
            break;

/* Use the temporary array to count how many times each valid axis index
   occurs. */
	 } else {
            there[ perm[ axis ] ]++;
	 }
      }

/* If all the axis indices were within range, check to ensure that each value
   occurred only once. */
      if ( valid ) {
         for ( axis = 0; axis < naxes; axis++ ) {

/* Note an error and quit checking if any value did not occur exactly once. */
            if ( there[ axis ] != 1 ) {
               valid = 0;
               break;
	    }
	 }
      }
   }

/* Free the temporary array. */
   there = astFree( there );

/* If an invalid permutation was detected and no other error has
   occurred, then report an error (note we convert to one-based axis
   numbering in the error message). */
   if ( !valid && astOK ) {
      astError( AST__PRMIN, "%s(%s): Invalid axis permutation array.", status,
                method, astGetClass( this ) );
      astError( AST__PRMIN, "Each axis index should lie in the range 1 to %d "
                "and should occur only once.", status, naxes );
   }
}

static void CleanAttribs( AstObject *this_object, int *status ) {
/*
*  Name:
*     CleanAttribs

*  Purpose:
*     Clear any invalid set attribute values.

*  Type:
*     Private function.

*  Synopsis:
*     #include "frame.h"
*     void CleanAttribs( AstObject *this_object, int *status )

*  Class Membership:
*     Frame member function (over-rides the protected astCleanAttribs
*     method inherited from the Object class).

*  Description:
*     This function clears any attributes that are currently set to
*     invalid values in thr supplied object.

*  Parameters:
*     this
*        Pointer to the Object to be cleaned.

*/

/* Local Variables; */
   AstAxis *ax;
   AstFrame *this;
   int i;
   int nax;
   int reporting;

/* Check inherited status */
   if( !astOK ) return;

/* Get a pointer to the Frame structure. */
   this = (AstFrame *) this_object;

/* Defer error reporting, as required by the astCLEAN_ATTRIB macro. */
   reporting = astReporting( 0 );

/* Clean attributes in any Objects contained within "this". */
   nax = astGetNaxes( this );
   for( i = 0; i < nax; i++ ) {
      ax = astGetAxis( this, i );
      astCleanAttribs( ax );
      ax = astAnnul( ax );
   }

/* Clean attributes of this class. */
   astCLEAN_ATTRIB(System)
   astCLEAN_ATTRIB(AlignSystem)

/* Re-establish error reporting. */
   astReporting( reporting );

/* Invoke the method inherited form the parent to clean attributes
   defined by the parent class. */
   (*parent_cleanattribs)( this_object, status );
}

static char *CleanDomain( char *domain, int *status ) {
/*
*  Name:
*     CleanDomain

*  Purpose:
*     Clean a Domain string and convert to upper case.

*  Type:
*     Private function.

*  Synopsis:
*     #include "frame.h"
*     char *CleanDomain( char *domain, int *status )

*  Class Membership:
*     Frame member function.

*  Description:
*     This function removes all white space from a string and converts
*     other characters to upper case. It is intended for cleaning up
*     values supplied for the Domain attribute of a Frame.

*  Parameters:
*     domain
*        Pointer to the null terminated Domain string to be modified.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     The pointer value "domain" is always returned (even under error
*     conditions).
*/

/* Local Variables: */
   int i;                        /* Loop counter for characters */
   int j;                        /* Good character count */

/* Check the global error status. */
   if ( !astOK ) return domain;

/* Eliminate white space characters and convert the rest to upper
   case. */
   for ( i = j = 0; domain[ i ]; i++ ) {
      if ( !isspace( domain[ i ] ) ) domain[ j++ ] = toupper( domain[ i ] );
   }
   domain[ j ] = '\0';

/* Return the string pointer. */
   return domain;
}

static void ClearAttrib( AstObject *this_object, const char *attrib, int *status ) {
/*
*  Name:
*     ClearAttrib

*  Purpose:
*     Clear an attribute value for a Frame.

*  Type:
*     Private function.

*  Synopsis:
*     #include "frame.h"
*     void ClearAttrib( AstObject *this, const char *attrib, int *status )

*  Class Membership:
*     Frame member function (over-rides the astClearAttrib protected
*     method inherited from the Mapping class).

*  Description:
*     This function clears the value of a specified attribute for a
*     Frame, so that the default value will subsequently be used.

*  Parameters:
*     this
*        Pointer to the Frame.
*     attrib
*        Pointer to a null terminated string specifying the attribute
*        name.  This should be in lower case with no surrounding white
*        space.
*     status
*        Pointer to the inherited status variable.

*  Notes:
*     - This function uses one-based axis numbering so that it is
*     suitable for external (public) use.
*/

/* Local Variables: */
   AstAxis *ax;                  /* Pointer to Axis */
   AstFrame *pfrm;               /* Pointer to primary Frame containing axis */
   AstFrame *this;               /* Pointer to the Frame structure */
   char pfrm_attrib[ 100 ];      /* Primary Frame attribute */
   char *axis_attrib;            /* Pointer to axis attribute name */
   const char *old_attrib;       /* Pointer to supplied attribute name string */
   int axis;                     /* Frame axis number */
   int axis_nc;                  /* No. characters in axis attribute name */
   int free_axis_attrib;         /* Should axis_attrib be freed? */
   int has_axis;                 /* Does attrib name include axis specifier? */
   int len;                      /* Length of attrib string */
   int nc;                       /* No. characters read by astSscanf */
   int oldrep;                   /* Original error reporting state */
   int paxis;                    /* Axis index within primary frame */
   int used;                     /* Could the setting string be used? */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain a pointer to the Frame structure. */
   this = (AstFrame *) this_object;

/* Set a flag indicating if the attribute name includes an axis
   specifier. */
   has_axis = ( strchr( attrib, '(' ) != NULL );

/* A flag indicating that we do not need to free the axis_attrib memory. */
   free_axis_attrib = 0;

/* Initialise things to avoid compiler warnings. */
   axis_attrib = NULL;
   old_attrib = NULL;

/* Jump back to here if we are trying the same attribute but with an explicit
   axis "(1)" added to the end of the name. */
L1:

/* Obtain the length of the "attrib" string. */
   len = strlen( attrib );

/* Check the attribute name and clear the appropriate attribute. */

/* Digits. */
/* ------- */
   if ( !strcmp( attrib, "digits" ) ) {
      astClearDigits( this );

/* Digits(axis). */
/* ------------- */
   } else if ( nc = 0,
               ( 1 == astSscanf( attrib, "digits(%d)%n", &axis, &nc ) )
               && ( nc >= len ) ) {

/* There is no function to clear the Digits attribute for an axis
   directly, so obtain a pointer to the Axis and use this to clear the
   attribute. */
      (void) astValidateAxis( this, axis - 1, "astClearDigits(axis)" );
      ax = astGetAxis( this, axis - 1 );
      astClearAxisDigits( ax );
      ax = astAnnul( ax );

/* Direction(axis). */
/* ---------------- */
   } else if ( nc = 0,
               ( 1 == astSscanf( attrib, "direction(%d)%n", &axis, &nc ) )
               && ( nc >= len ) ) {
      astClearDirection( this, axis - 1 );

/* Epoch. */
/* ------ */
   } else if ( !strcmp( attrib, "epoch" ) ) {
      astClearEpoch( this );

/* Top(axis). */
/* ---------- */
   } else if ( nc = 0,
               ( 1 == astSscanf( attrib, "top(%d)%n", &axis, &nc ) )
               && ( nc >= len ) ) {
      astClearTop( this, axis - 1 );

/* Bottom(axis). */
/* ------------- */
   } else if ( nc = 0,
               ( 1 == astSscanf( attrib, "bottom(%d)%n", &axis, &nc ) )
               && ( nc >= len ) ) {
      astClearBottom( this, axis - 1 );

/* Domain. */
/* ------- */
   } else if ( !strcmp( attrib, "domain" ) ) {
      astClearDomain( this );

/* Format(axis). */
/* ------------- */
   } else if ( nc = 0,
               ( 1 == astSscanf( attrib, "format(%d)%n", &axis, &nc ) )
               && ( nc >= len ) ) {
      astClearFormat( this, axis - 1 );

/* Label(axis). */
/* ------------ */
   } else if ( nc = 0,
               ( 1 == astSscanf( attrib, "label(%d)%n", &axis, &nc ) )
               && ( nc >= len ) ) {
      astClearLabel( this, axis - 1 );

/* MatchEnd. */
/* --------- */
   } else if ( !strcmp( attrib, "matchend" ) ) {
      astClearMatchEnd( this );

/* MaxAxes. */
/* -------- */
   } else if ( !strcmp( attrib, "maxaxes" ) ) {
      astClearMaxAxes( this );

/* MinAxes. */
/* -------- */
   } else if ( !strcmp( attrib, "minaxes" ) ) {
      astClearMinAxes( this );

/* Permute. */
/* -------- */
   } else if ( !strcmp( attrib, "permute" ) ) {
      astClearPermute( this );

/* PreserveAxes. */
/* ------------- */
   } else if ( !strcmp( attrib, "preserveaxes" ) ) {
      astClearPreserveAxes( this );

/* Symbol(axis). */
/* ------------- */
   } else if ( nc = 0,
               ( 1 == astSscanf( attrib, "symbol(%d)%n", &axis, &nc ) )
               && ( nc >= len ) ) {
      astClearSymbol( this, axis - 1 );

/* System. */
/* ------- */
   } else if ( !strcmp( attrib, "system" ) ) {
      astClearSystem( this );

/* AlignSystem. */
/* ------------ */
   } else if ( !strcmp( attrib, "alignsystem" ) ) {
      astClearAlignSystem( this );

/* Title. */
/* ------ */
   } else if ( !strcmp( attrib, "title" ) ) {
      astClearTitle( this );

/* Unit(axis). */
/* ----------- */
   } else if ( nc = 0,
               ( 1 == astSscanf( attrib, "unit(%d)%n", &axis, &nc ) )
               && ( nc >= len ) ) {
      astClearUnit( this, axis - 1 );

/* ObsLat. */
/* ------- */
   } else if ( !strcmp( attrib, "obslat" ) ) {
      astClearObsLat( this );

/* ObsLon. */
/* ------- */
   } else if ( !strcmp( attrib, "obslon" ) ) {
      astClearObsLon( this );

/* ObsAlt. */
/* ------- */
   } else if ( !strcmp( attrib, "obsalt" ) ) {
      astClearObsAlt( this );

/* Dut1 */
/* --- */
   } else if ( !strcmp( attrib, "dut1" ) ) {
      astClearDut1( this );

/* Read-only attributes. */
/* --------------------- */
/* Test if the attribute name matches any of the read-only attributes
   of this class. If it does, then report an error. */
   } else if ( !strcmp( attrib, "naxes" ) ||
               !strncmp( attrib, "normunit", 8 ) ) {
      astError( AST__NOWRT, "astClear: Invalid attempt to clear the \"%s\" "
                "value for a %s.", status, attrib, astGetClass( this ) );
      astError( AST__NOWRT, "This is a read-only attribute." , status);

/* Other axis attributes. */
/* ---------------------- */
/* If the attribute was not identified above, but appears to refer to
   a Frame axis, then it may refer to an Axis object of a derived type
   (which has additional attributes not recognised here). */
   } else if( !free_axis_attrib && ( nc = 0,
                 ( 1 == astSscanf( attrib, "%*[^()]%n(%d)%n",
                                      &axis_nc, &axis, &nc ) )
               && ( nc >= len ) ) ) {

/* Validate the axis index and extract the attribute name. */
      (void) astValidateAxis( this, axis - 1, "astClear" );
      axis_attrib = astString( attrib, axis_nc );

/* Obtain a pointer to the Axis object. */
      ax = astGetAxis( this, axis - 1 );
      if( astOK ) {

/* Assume that we will be able to use the attribute name. */
         used = 1;

/* Temporarily switch off error reporting so that if the following attempt
   to access the axis attribute fails, we can try to interpret the
   attribute name as an attribute of the primary Frame containing the
   specified axis. Any errors reported in this context will simply be
   ignored, in particularly they are not deferred for later delivery. */
         oldrep = astReporting( 0 );

/* Use the Axis astClearAttrib method to clear the attribute value. */
         astClearAttrib( ax, axis_attrib );

/* If the above call failed with a status of AST__BADAT, indicating that
   the attribute name was not recognised, clear the status so that we can
   try to interpret the attribute name as an attribute of the primary Frame
   containing the specified axis. */
         if( astStatus == AST__BADAT ) {
            astClearStatus;

/* Find the primary Frame containing the specified axis. */
            astPrimaryFrame( this, axis - 1, &pfrm, &paxis );

/* Only attempt to use the primary Frame if it is not the same as "this"
   - otherwise we could end up in an infinite loop. */
            if( pfrm != this ) {

/* Modify the attribute name to refer to the axis numbering of the
   primary frame. */
               sprintf( pfrm_attrib, "%s(%d)", axis_attrib, paxis + 1 );

/* Attempt to clear the attribute as an attribute of the primary Frame. */
               astClearAttrib( pfrm, pfrm_attrib );

/* If this failed, clear the status and indicate that we have not managed to
   use the attribute name. */
               if( !astOK ) {
                  astClearStatus;
                  used = 0;
               }

            } else {
               used = 0;
            }

/* If not found attempt to clear the attribute value in the Axis, omitting
   the axis index. */
            if( ! used ) {
               astClearAttrib( pfrm, axis_attrib );
               if( !astOK ) {
                  astClearStatus;
               } else {
                  used = 1;
               }
            }

/* Annul the primary Frame pointer. */
            pfrm = astAnnul( pfrm );
         }

/* Re-instate the original error reporting state. */
         astReporting( oldrep );

/* If we could not use the attribute name, attempt to clear the axis
   attribute again, this time retaining the error report. This is done
   to ensure the user gets an appropriate error message. */
         if( !used ) astClearAttrib( ax, axis_attrib );
      }

/* Annul the Axis pointer and free the memory holding the attribute
   name. */
      ax = astAnnul( ax );
      axis_attrib = astFree( axis_attrib );

/* Not recognised. */
/* --------------- */
/* If the attribute is still not recognised, and the Frame has only 1 axis,
   and the attribute name does not already include an axis specifier, try
   again after appending "(1)" to the end of the attribute name. */
   } else if( !has_axis && astGetNaxes( this ) == 1 ) {

/* Take a copy of the supplied name, allowing 3 extra characters for the
   axis specifier "(1)". */
      axis_attrib = astMalloc( len + 4 );
      if( axis_attrib ) memcpy( axis_attrib, attrib, len );

/* Indicate we should free the axis_attrib memory. */
      free_axis_attrib = 1;

/* Add in the axis specifier. */
      strcpy( axis_attrib + len, "(1)" );

/* Use the new attribute name instead of the supplied name. */
      old_attrib = attrib;
      attrib = axis_attrib;

/* Indicate the attribute name now has an axis specifier. */
      has_axis = 1;

/* Jump back to try interpreting the new attribute name. */
      goto L1;

/* Not recognised. */
/* --------------- */
/* If the attribute name is still not recognised, pass it on to the parent
   method for further interpretation. First re-instate the original attrib
   name string if it was changed above. */
   } else {
      if( free_axis_attrib ) {
         attrib = old_attrib;
         axis_attrib = astFree( axis_attrib );
      }
      (*parent_clearattrib)( this_object, attrib, status );
   }
}

static void ClearUnit( AstFrame *this, int axis, int *status ) {
/*
*  Name:
*     ClearUnit

*  Purpose:
*     Clear the Unit attribute of a Frame.

*  Type:
*     Protected virtual function.

*  Synopsis:
*     #include "frame.h"
*     void ClearUnit( AstFrame *this, int axis, int *status )

*  Class Membership:
*     Frame method.

*  Description:
*     This function clears the Unit value for an axis of a Frame.

*  Parameters:
*     this
*        Pointer to the Frame.
*     axis
*        The number of the axis (zero-based) for which the Unit value is to
*        be cleared.
*     unit
*        The new value to be set.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     void.
*/

/* Local Variables: */
   AstAxis *ax;                  /* Pointer to Axis object */
   const char *units;            /* Pointer to units string */
   char *old_units;              /* Pointer to copy of original units */

/* Check the global error status. */
   if ( !astOK ) return;

/* Validate the axis index. */
   (void) astValidateAxis( this, axis, "astSetUnit" );

/* Do nothing more if the attribute is already cleared. */
   if( astTestUnit( this, axis ) ) {

/* Obtain a pointer to the required Axis. */
      ax = astGetAxis( this, axis );

/* Save a copy of the old units. */
      units = astGetAxisUnit( ax );
      old_units = astStore( NULL, units, strlen( units ) + 1 );

/* Clear the Axis Unit attribute value, and then get a pointer to the
   new default Units string. */
      astClearAxisUnit( ax );
      units = astGetUnit( this, axis );

/* The new unit may require the Label and/or Symbol to be changed, but
   only if the Frames ActiveUnit flag is set. */
      if( astGetActiveUnit( this ) ) NewUnit( ax, old_units, units,
                                              "astSetUnit", astGetClass( this ), status );

/* Free resources. */
      old_units = astFree( old_units );

/* Annul the Axis pointer. */
      ax = astAnnul( ax );
   }
}

static int ConsistentMaxAxes( AstFrame *this, int value, int *status ) {
/*
*  Name:
*     ConsistentMaxAxes

*  Purpose:
*     Ensure a consistent value when setting the MaxAxes attribute.

*  Type:
*     Private function.

*  Synopsis:
*     #include "frame.h"
*     int ConsistentMaxAxes( AstFrame *this, int value, int *status )

*  Class Membership:
*     Frame member function.

*  Description:
*     This function accepts a value which is to be set for a Frame's MaxAxes
*     attribute and returns an appropriately adjusted value to be assigned
*     to the Frame structure's max_axes component. If necessary, the Frame's
*     MinAxes attribute is adjusted to remain consistent with the new MaxAxes
*     value (but note that the MaxAxes value itself is not assigned by this
*     function).

*  Parameters:
*     this
*        Pointer to the Frame.
*     value
*        The new value being set for the MaxAxes attribute.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     The value to be assigned to the max_axes component.

*  Notes:
*     - A value of -INT_MAX will be returned if this function is
*     invoked with the global error status set, or if it should fail
*     for any reason.
*/

/* Local Variables: */
   int result;                   /* Result to be returned */

/* Check the global error status. */
   if ( !astOK ) return -INT_MAX;

/* Ensure that the result value isn't negative. */
   result = ( value >= 0 ) ? value : 0;

/* Check if the MinAxes attribute is set. If not, its default value will be
   consistent with the MaxAxes value (the DefaultMinAxes function ensures
   this). Otherwise, obtain its value to check for consistency. */
   if ( astTestMinAxes( this ) ) {

/* If necessary, set a new MinAxes value to prevent it exceeding the MaxAxes
   value about to be returned. */
      if ( astGetMinAxes( this ) > result ) astSetMinAxes( this, result );
   }

/* If an error occurred, clear the result value. */
   if ( !astOK ) result = -INT_MAX;

/* Return the result. */
   return result;
}

static int ConsistentMinAxes( AstFrame *this, int value, int *status ) {
/*
*  Name:
*     ConsistentMinAxes

*  Purpose:
*     Ensure a consistent value when setting the MinAxes attribute.

*  Type:
*     Private function.

*  Synopsis:
*     #include "frame.h"
*     int ConsistentMinAxes( AstFrame *this, int value, int *status )

*  Class Membership:
*     Frame member function.

*  Description:
*     This function accepts a value which is to be set for a Frame's MinAxes
*     attribute and returns an appropriately adjusted value to be assigned
*     to the Frame structure's min_axes component. If necessary, the Frame's
*     MaxAxes attribute is adjusted to remain consistent with the new MinAxes
*     value (but note that the MinAxes value itself is not assigned by this
*     function).

*  Parameters:
*     this
*        Pointer to the Frame.
*     value
*        The new value being set for the MinAxes attribute.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     The value to be assigned to the min_axes component.

*  Notes:
*     - A value of -INT_MAX will be returned if this function is
*     invoked with the global error status set, or if it should fail
*     for any reason.
*/

/* Local Variables: */
   int result;                   /* Result to be returned */

/* Check the global error status. */
   if ( !astOK ) return -INT_MAX;

/* Ensure that the result value isn't negative. */
   result = ( value >= 0 ) ? value : 0;

/* Check if the MaxAxes attribute is set. If not, its default value will be
   consistent with the MinAxes value (the DefaultMaxAxes function ensures
   this). Otherwise, obtain its value to check for consistency. */
   if ( astTestMaxAxes( this ) ) {

/* If necessary, set a new MaxAxes value to prevent it being less than the
   MinAxes value about to be returned. */
      if ( astGetMaxAxes( this ) < result ) astSetMaxAxes( this, result );
   }

/* If an error occurred, clear the result value. */
   if ( !astOK ) result = -INT_MAX;

/* Return the result. */
   return result;
}

static AstFrameSet *Convert( AstFrame *from, AstFrame *to,
                             const char *domainlist, int *status ) {
/*
*++
*  Name:
c     astConvert
f     AST_CONVERT

*  Purpose:
*     Determine how to convert between two coordinate systems.

*  Type:
*     Public virtual function.

*  Synopsis:
c     #include "frame.h"
c     AstFrameSet *astConvert( AstFrame *from, AstFrame *to,
c                              const char *domainlist )
f     RESULT = AST_CONVERT( FROM, TO, DOMAINLIST, STATUS )

*  Class Membership:
*     Frame method.

*  Description:
*     This function compares two Frames and determines whether it is
*     possible to convert between the coordinate systems which they
*     represent. If conversion is possible, it returns a FrameSet
*     which describes the conversion and which may be used (as a
*     Mapping) to transform coordinate values in either direction.
*
*     The same function may also be used to determine how to convert
*     between two FrameSets (or between a Frame and a FrameSet, or
*     vice versa). This mode is intended for use when (for example)
*     two images have been calibrated by attaching a FrameSet to each.
c     astConvert might then be used to search for a
f     AST_CONVERT might then be used to search for a
*     celestial coordinate system that both images have in common, and
*     the result could then be used to convert between the pixel
*     coordinates of both images -- having effectively used their
*     celestial coordinate systems to align them.
*
*     When using FrameSets, there may be more than one possible
*     intermediate coordinate system in which to perform the
*     conversion (for instance, two FrameSets might both have
*     celestial coordinates, detector coordinates, pixel coordinates,
*     etc.). A comma-separated list of coordinate system domains may
*     therefore be given which defines a priority order to use when
*     selecting the intermediate coordinate system.  The path used for
*     conversion must go via an intermediate coordinate system whose
*     Domain attribute matches one of the domains given. If conversion
*     cannot be achieved using the first domain, the next one is
*     considered, and so on, until success is achieved.

*  Parameters:
c     from
f     FROM = INTEGER (Given)
*        Pointer to a Frame which represents the "source" coordinate
*        system.  This is the coordinate system in which you already
*        have coordinates available.
*
*        If a FrameSet is given, its current Frame (as determined by
*        its Current attribute) is taken to describe the source
*        coordinate system. Note that the Base attribute of this
*        FrameSet may be modified by this function to indicate which
*        intermediate coordinate system was used (see under
*        "FrameSets" in the "Applicability" section for details).
c     to
f     TO = INTEGER (Given)
*        Pointer to a Frame which represents the "destination"
*        coordinate system. This is the coordinate system into which
*        you wish to convert your coordinates.
*
*        If a FrameSet is given, its current Frame (as determined by
*        its Current attribute) is taken to describe the destination
*        coordinate system. Note that the Base attribute of this
*        FrameSet may be modified by this function to indicate which
*        intermediate coordinate system was used (see under
*        "FrameSets" in the "Applicability" section for details).
c     domainlist
f     DOMAINLIST = CHARACTER * ( * ) (Given)
c        Pointer to a null-terminated character string containing a
f        A character string containing a
*        comma-separated list of Frame domains. This may be used to
*        define a priority order for the different intermediate
*        coordinate systems that might be used to perform the
*        conversion.
*
*        The function will first try to obtain a conversion by making
*        use only of an intermediate coordinate system whose Domain
*        attribute matches the first domain in this list. If this
*        fails, the second domain in the list will be used, and so on,
*        until conversion is achieved. A blank domain (e.g. two
*        consecutive commas) indicates that all coordinate systems
*        should be considered, regardless of their domains.
*
*        This list is case-insensitive and all white space is ignored.
*        If you do not wish to restrict the domain in this way,
c        you should supply an empty string. This is normally
f        you should supply a blank string. This is normally
*        appropriate if either of the source or destination coordinate
*        systems are described by Frames (rather than FrameSets),
*        since there is then usually only one possible choice of
*        intermediate coordinate system.
f     STATUS = INTEGER (Given and Returned)
f        The global status.

*  Returned Value:
c     astConvert()
f     AST_CONVERT = INTEGER
*        If the requested coordinate conversion is possible, the
*        function returns a pointer to a FrameSet which describes the
*        conversion. Otherwise, a null Object pointer (AST__NULL) is
*        returned without error.
*
*        If a FrameSet is returned, it will contain two Frames. Frame
*        number 1 (its base Frame) will describe the source coordinate
c        system, corresponding to the "from" parameter. Frame number 2
f        system, corresponding to the FROM argument. Frame number 2
*        (its current Frame) will describe the destination coordinate
c        system, corresponding to the "to" parameter. The Mapping
f        system, corresponding to the TO argument. The Mapping
*        which inter-relates these two Frames will perform the
*        required conversion between their respective coordinate
*        systems.
*
*        Note that a FrameSet may be used both as a Mapping and as a
*        Frame.  If the result is used as a Mapping (e.g. with
c        astTran2), then it provides a means of converting coordinates
f        AST_TRAN2), then it provides a means of converting coordinates
*        from the source to the destination coordinate system (or
*        vice versa if its inverse transformation is selected). If it
*        is used as a Frame, its attributes will describe the
*        destination coordinate system.

*  Applicability:
*     DSBSpecFrame
*        If the AlignSideBand attribute is non-zero, alignment occurs in the
*        upper sideband expressed within the spectral system and standard of
*        rest given by attributes AlignSystem and AlignStdOfRest. If
*        AlignSideBand is zero, the two DSBSpecFrames are aligned as if
*        they were simple SpecFrames (i.e. the SideBand is ignored).
*     Frame
*        This function applies to all Frames. Alignment occurs within the
*        coordinate system given by attribute AlignSystem.
*     FrameSet
c        If either of the "from" or "to" parameters is a pointer to a
f        If either of the FROM or TO arguments is a pointer to a
c        FrameSet, then astConvert will attempt to convert from the
f        FrameSet, then AST_CONVERT will attempt to convert from the
c        coordinate system described by the current Frame of the "from"
f        coordinate system described by the current Frame of the FROM
c        FrameSet to that described by the current Frame of the "to"
f        FrameSet to that described by the current Frame of the TO
*        FrameSet.
*
*        To achieve this, it will consider all of the Frames within
*        each FrameSet as a possible way of reaching an intermediate
*        coordinate system that can be used for the conversion. There
*        is then the possibility that more than one conversion path
*        may exist and, unless the choice is sufficiently restricted
c        by the "domainlist" string, the sequence in which the Frames
f        by the DOMAINLIST string, the sequence in which the Frames
*        are considered can be important. In this case, the search
*        for a conversion path proceeds as follows:
c        - Each field in the "domainlist" string is considered in turn.
f        - Each field in the DOMAINLIST string is considered in turn.
*        - The Frames within each FrameSet are considered in a
*        specific order: (1) the base Frame is always considered
*        first, (2) after this come all the other Frames in
*        Frame-index order (but omitting the base and current Frames),
*        (3) the current Frame is always considered last.  However, if
*        either FrameSet's Invert attribute is set to a non-zero value
*        (so that the FrameSet is inverted), then its Frames are
*        considered in reverse order. (Note that this still means that
*        the base Frame is considered first and the current Frame
*        last, because the Invert value will also cause these Frames
*        to swap places.)
*        - All source Frames are first considered (in the appropriate
*        order) for conversion to the first destination Frame. If no
*        suitable intermediate coordinate system emerges, they are
*        then considered again for conversion to the second
*        destination Frame (in the appropriate order), and so on.
*        - Generally, the first suitable intermediate coordinate
*        system found is used. However, the overall Mapping between
*        the source and destination coordinate systems is also
*        examined.  Preference is given to cases where both the
*        forward and inverse transformations are defined (as indicated
*        by the TranForward and TranInverse attributes). If only one
*        transformation is defined, the forward one is preferred.
*        - If the domain of the intermediate coordinate system matches
c        the current "domainlist" field, the conversion path is
f        the current DOMAINLIST field, the conversion path is
c        accepted. Otherwise, the next "domainlist" field is considered
f        accepted. Otherwise, the next DOMAINLIST field is considered
*        and the process repeated.
*
*        If conversion is possible, the Base attributes of the two
*        FrameSets will be modified on exit to identify the Frames
*        used to access the intermediate coordinate system which was
*        finally accepted.
*
*        Note that it is possible to force a particular Frame within a
*        FrameSet to be used as the basis for the intermediate
*        coordinate system, if it is suitable, by (a) focussing
*        attention on
c        it by specifying its domain in the "domainlist" string, or (b)
f        it by specifying its domain in the DOMAINLIST string, or (b)
*        making it the base Frame, since this is always considered
*        first.
*     SpecFrame
*        Alignment occurs within the spectral system and standard of rest
*        given by attributes AlignSystem and AlignStdOfRest.
*     TimeFrame
*        Alignment occurs within the time system and time scale given by
*        attributes AlignSystem and AlignTimeScale.

*  Examples:
c     cvt = astConvert( a, b, "" );
f     CVT = AST_CONVERT( A, B, ' ', STATUS )
*        Attempts to convert between the coordinate systems represented
c        by "a" and "b" (assumed to be Frames). If successful, a FrameSet
f        by A and B (assumed to be Frames). If successful, a FrameSet
c        is returned via the "cvt" pointer which may be used to apply the
f        is returned via the CVT pointer which may be used to apply the
c        conversion to sets of coordinates (e.g. using astTran2).
f        conversion to sets of coordinates (e.g. using AST_TRAN2).
c     cvt = astConvert( astSkyFrame(""), astSkyFrame("Equinox=2005"), "" );
f     CVT = AST_CONVERT( AST_SKYFRAME( ' ', STATUS ), AST_SKYFRAME( 'Equinox=2005', STATUS ), ' ', STATUS )
*        Creates a FrameSet which describes precession in the default
*        FK5 celestial coordinate system between equinoxes J2000 (also
c        the default) and J2005. The returned "cvt" pointer may then
f        the default) and J2005. The returned CVT pointer may then
c        be passed to astTran2 to apply this precession correction to
f        be passed to AST_TRAN2 to apply this precession correction to
*        any number of coordinate values given in radians.
*
*        Note that the returned FrameSet also contains information
*        about how to format coordinate values. This means that
*        setting its Report attribute to 1 is a simple way to obtain
*        printed output (formatted in sexagesimal notation) to show
*        the coordinate values before and after conversion.
c     cvt = astConvert( a, b, "sky,detector," );
f     CVT = AST_CONVERT( A, B, 'SKY,DETECTOR,', STATUS )
*        Attempts to convert between the coordinate systems
c        represented by the current Frames of "a" and "b"
f        represented by the current Frames of A and B
*        (now assumed to be FrameSets), via the intermediate "SKY"
*        coordinate system.  This, by default, is the Domain
*        associated with a celestial coordinate system represented by
*        a SkyFrame.
*
*        If this fails (for example, because either FrameSet lacks
*        celestial coordinate information), then the user-defined
*        "DETECTOR" coordinate system is used instead. If this also
*        fails, then all other possible ways of achieving conversion
*        are considered before giving up.
*
c        The returned pointer "cvt" indicates whether conversion was
f        The returned pointer CVT indicates whether conversion was
*        possible and will have the value AST__NULL if it was not. If
c        conversion was possible, "cvt" will point at a new FrameSet
f        conversion was possible, CVT will point at a new FrameSet
*        describing the conversion.
*
*        The Base attributes of the two FrameSets
c        will be set by astConvert to indicate which of their Frames was
f        will be set by AST_CONVERT to indicate which of their Frames was
*        used for the intermediate coordinate system. This means that
*        you can subsequently determine which coordinate system was
*        used by enquiring the Domain attribute of either base Frame.

*  Notes:
*     -  The Mapping represented by the returned FrameSet results in
*     alignment taking place in the coordinate system specified by the
c     AlignSystem attribute of the "to" Frame. See the description of the
f     AlignSystem attribute of the TO Frame. See the description of the
*     AlignSystem attribute for further details.
*     - When aligning (say) two images, which have been calibrated by
*     attaching FrameSets to them, it is usually necessary to convert
*     between the base Frames (representing "native" pixel
*     coordinates) of both FrameSets. This may be achieved by
*     inverting the FrameSets (e.g. using astInvert) so as to
*     interchange their base and current Frames before using
*     astConvert.
*     - A null Object pointer (AST__NULL) will be returned if this
c     function is invoked with the AST error status set, or if it
f     function is invoked with STATUS set to an error value, or if it
*     should fail for any reason.
*--

* Implementation Notes:
*    This function is simply a wrap-up for the protected astConvertX
*    method which performs the required processing but swaps the order
*    of the first two arguments. This is a trick to allow the
*    astConvert method to be over-ridden by derived classes on the
*    basis of the class of either of the first two arguments.
*/

/* Check the global error status. */
   if ( !astOK ) return NULL;

/* Invoke the "astConvertX" method with the first two arguments
   swapped. */
   return astConvertX( to, from, domainlist );
}

static AstFrameSet *ConvertX( AstFrame *to, AstFrame *from,
                              const char *domainlist, int *status ) {
/*
*+
*  Name:
*     astConvertX

*  Purpose:
*     Determine how to convert between two coordinate systems.

*  Type:
*     Protected virtual function.

*  Synopsis:
*     #include "frame.h"
*     AstFrameSet *astConvertX( AstFrame *to, AstFrame *from,
*                               const char *domainlist )

*  Class Membership:
*     Frame method.

*  Description:
*     This function performs the processing for the public astConvert
*     method and has exactly the same interface except that the order
*     of the first two arguments is swapped. This is a trick to allow
*     the astConvert method to be over-ridden by derived classes on
*     the basis of the class of either of its first two arguments.
*
*     See the astConvert method for details of the interface.
*-

*  Implementation Deficiencies:
*     - This function's job is basically to negotiate with two Frames
*     to try and find a mutually agreeable coordinate system for
*     conversion.  Ideally, it should be able to juggle the number of
*     axes, etc. to do this.  At present, however, the implementation
*     is much simpler. This is adequate for the Frame classes which
*     exist at the time of writing, but the implementation may need
*     beefing up in future.
*     - One likely problem is with attributes which default in both
*     the source and destination Frames. This means they also default
*     in the common coordinate system. If these default values were to
*     differ when matching different target Frames, however, we would
*     be in trouble, because the common coordinate system would not
*     then be remaining constant. The longer-term solution to this is
*     probably to provide some mechanism to "fix" all attribute values
*     for a Frame, by taking any attributes that are un-set and
*     explicitly setting a firm value (equal to the default) so they
*     cannot then change.
*/

/* Local Variables: */
   AstFrameSet *result;          /* Pointer to Mapping to be returned */
   AstFrame *ftmp;                /* Pointer to returned Frame */
   AstMapping **map1_address;    /* Location of first Mapping pointer */
   AstMapping **map2_address;    /* Location of second Mapping pointer */
   AstMapping *common0;          /* Initial common coordinate system */
   AstMapping *common1;          /* Improved common coordinate system */
   AstMapping *common2;          /* Final common coordinate system */
   AstMapping *frame1;           /* Pointer to Frame for first match */
   AstMapping *frame2;           /* Pointer to Frame for second match */
   AstMapping *from_map;         /* Pointer to "from" Mapping */
   AstMapping *map;              /* Pointer to conversion Mapping */
   AstMapping *result_map;       /* Pointer to result Mapping */
   AstMapping *tmp;              /* Temporary Mapping pointer */
   AstMapping *to_map;           /* Pointer to "to" Mapping */
   char *domain;                 /* Pointer to result domain */
   char *domain_end;             /* Pointer to null at end of domain */
   char *domainlist_copy;        /* Pointer to copy of domainlist */
   int *axes1;                   /* Pointer to axis assignments */
   int *axes2;                   /* Pointer to axis assignments */
   int best_score;               /* Score assigned to best match */
   int icom;                     /* Common coordinate system loop counter */
   int match1;                   /* First match succeeded? */
   int match2;                   /* Second match succeeded? */
   int match;                    /* Overall match found? */
   int perfect;                  /* Perfect match found? */
   int score;                    /* Score assigned to match */

/* Initialise. */
   result = NULL;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Further initialisation. */
   result_map = NULL;

/* Make a temporary copy of the domains list. */
   domainlist_copy = astStore( NULL, domainlist,
                               strlen( domainlist ) + (size_t) 1 );
   if ( astOK ) {

/* Loop to inspect each comma-separated field in the domains list
   until an error occurs, all the domains are used up, or a match is
   found. */
      domain = domainlist_copy;
      match = 0;
      while ( astOK && domain && !match ) {

/* Change the comma at the end of each field to a null to terminate
   the domain. Then convert the domain to upper case and eliminate
   white space. */
         if ( ( domain_end = strchr( domain, ',' ) ) ) *domain_end = '\0';
         CleanDomain( domain, status );

/* For any given domain, we will ignore imperfect matches in favour of
   better ones by assigning a score to each match. Initialise the best
   score value for the current domain. */
         best_score = -1;

/* Loop to consider both the "to" and "from" Frames in turn as the
   basis of a possible common coordinate system. Quit looping early if
   an error occurs or a perfect match is found. */
         perfect = 0;
         for ( icom = 0; astOK && !perfect && ( icom <= 1 ); icom++ ) {

/* Make a copy of the Frame representing the initial guess at a common
   coordinate system. We will use this to probe the other
   Frame. Ensure that axes are not preserved (so that we convert to
   the common axis number/order). */
            common0 = astCopy( icom ? from : to );
            astSetPreserveAxes( common0, 0 );

/* Also, if the current domain is not blank, set the Domain attribute (so
   we will only find coordinate systems which match the current
   "domainlist" field). */
            if ( *domain ) astSetDomain( common0, domain );

/* Obtain a pointer to the other Frame. */
            frame1 = astClone( icom ? to : from );

/* Set the address at which to store the resulting Mapping pointer. */
            map1_address = icom ? &to_map : &from_map;

/* See if conversion from "frame1" to the common coordinate system is
   possible.  If successful, this results in a new approximation
   ("common1") to the possible common coordinate system. */
            match1 = astMatch( common0, frame1, 1, &axes1, &axes2,
                               map1_address, &ftmp );
            common1 = (AstMapping *) ftmp;

/* If successful, free memory allocated for the axis association
   arrays, which are not needed. */
            if ( astOK && match1 ) {
               axes1 = astFree( axes1 );
               axes2 = astFree( axes2 );

/* Using the improved approximation to the common coordinate system,
   now test if conversion from the alternative Frame "frame2" is
   possible. */
               frame2 = astClone( icom ? from : to );
               map2_address = icom ? &from_map : &to_map;
               astSetPreserveAxes( common1, 0 );
               match2 = astMatch( common1, frame2, 1, &axes1, &axes2,
                                  map2_address, &ftmp );
               common2 = (AstMapping *) ftmp;

/* If successful, free memory allocated for the axis association
   arrays, which are not needed. */
               if ( astOK && match2 ) {
                  axes1 = astFree( axes1 );
                  axes2 = astFree( axes2 );

/* Invert the "to" Mapping and concatenate the two Mappings to
   describe the conversion between the "from" and "to" Frames. Then
   simplify the result. */
                  astInvert( to_map );
                  tmp = (AstMapping *) astCmpMap( from_map, to_map, 1, "", status );
                  map = astSimplify( tmp );
                  tmp = astAnnul( tmp );

/* Assign a score that favours Mappings with both transformations
   available over those with only one, and Mappings with only a
   forward transformation over those with only an inverse
   transformation. */
                  score = ( astGetTranForward( map ) ? 2 : 0 ) +
                          ( astGetTranInverse( map ) ? 1 : 0 );

/* If the new score is better than the previous one (or is the first
   one), note that we have a possible match. */
                  if ( astOK && ( score > best_score ) ) {
                     match = 1;

/* Update the best score and note if it indicates a perfect match (in
   which case we can stop searching at this point). */
                     best_score = score;
                     perfect = ( best_score >= 3 );

/* Annul any previous result Mapping pointer and replace it with this
   better one. */
                     if ( result_map ) result_map = astAnnul( result_map );
                     result_map = astClone( map );
                  }

/* Annul pointers to all the intermediate Objects. */
                  map = astAnnul( map );
                  common2 = astAnnul( common2 );
                  *map2_address = astAnnul( *map2_address );
               }
               frame2 = astAnnul( frame2 );
               common1 = astAnnul( common1 );
               *map1_address = astAnnul( *map1_address );
            }
            frame1 = astAnnul( frame1 );
            common0 = astAnnul( common0 );
         }

/* Go on to consider the next field in the domains list. */
         domain = domain_end ? domain_end + 1 : NULL;
      }
   }

/* Free the domain list copy. */
   domainlist_copy = astFree( domainlist_copy );

/* If returning a result, build the result FrameSet. Then annul the
   result Mapping pointer. */
   if ( result_map ) {
      result = astFrameSet( from, "", status );
      astAddFrame( result, AST__BASE, result_map, to );
      result_map = astAnnul( result_map );
   }

/* If an error occurred, annul the result FrameSet pointer. */
   if ( !astOK ) result = astAnnul( result );

/* Return the result. */
   return result;
}

static int DefaultMaxAxes( AstFrame *this, int *status ) {
/*
*  Name:
*     DefaultMaxAxes

*  Purpose:
*     Obtain the MaxAxes attribute from a Frame structure, with defaulting.

*  Type:
*     Private function.

*  Synopsis:
*     #include "frame.h"
*     int DefaultMaxAxes( AstFrame *this, int *status )

*  Class Membership:
*     Frame member function.

*  Description:
*     This function inspects the max_axes component of a Frame structure and
*     derives a value for the MaxAxes attribute. If the component's value
*     indicates that the attribute has not been set, a suitable default is
*     returned which is consistent with the state of the Frames's MinAxes
*     attribute.

*  Parameters:
*     this
*        Pointer to the Frame.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     The value to be used for the MaxAxes attribute.
*/

/* Local Variables. */
   int result;                   /* Result to be returned */
   int min_axes;                 /* Value of MinAxes attribute */

/* Check the global error status. */
   if ( !astOK ) return 0;

/* If the Frame's max_axes component is set, return its value. */
   if ( this->max_axes != -INT_MAX ) {
      result = this->max_axes;

/* Otherwise, the preferred default value is the number of Frame axes. */
   } else {
      result = astGetNaxes( this );

/* Before returning this value, check if the MinAxes attribute is set. If it
   is, obtain its value. */
      if ( astTestMinAxes( this ) ) {
         min_axes = astGetMinAxes( this );

/* If necessary, increase the MaxAxes default value so that it is not less than
   MinAxes. */
         if ( result < min_axes ) result = min_axes;
      }
   }

/* If an error occurred, clear the result value. */
   if ( !astOK ) result = 0;

/* Return the result. */
   return result;
}

static int DefaultMinAxes( AstFrame *this, int *status ) {
/*
*  Name:
*     DefaultMinAxes

*  Purpose:
*     Obtain the MinAxes attribute from a Frame structure, with defaulting.

*  Type:
*     Private function.

*  Synopsis:
*     #include "frame.h"
*     int DefaultMinAxes( AstFrame *this, int *status )

*  Class Membership:
*     Frame member function.

*  Description:
*     This function inspects the min_axes component of a Frame structure and
*     derives a value for the MinAxes attribute. If the component's value
*     indicates that the attribute has not been set, a suitable default is
*     returned which is consistent with the state of the Frames's MaxAxes
*     attribute.

*  Parameters:
*     this
*        Pointer to the Frame.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     The value to be used for the MinAxes attribute.
*/

/* Local Variables: */
   int result;                   /* Result to be returned */
   int max_axes;                 /* Value of MaxAxes attribute */

/* Check the global error status. */
   if ( !astOK ) return 0;

/* If the Frame's min_axes component is set, return its value. */
   if ( this->min_axes != -INT_MAX ) {
      result = this->min_axes;

/* Otherwise, the preferred default value is the number of Frame axes. */
   } else {
      result = astGetNaxes( this );

/* Before returning this value, check if the MaxAxes attribute is set. If it
   is, obtain its value. */
      if ( astTestMaxAxes( this ) ) {
         max_axes = astGetMaxAxes( this );

/* If necessary, reduce the MinAxes default value so that it does not exceed
   MaxAxes. */
         if ( result > max_axes ) result = max_axes;
      }
   }

/* If an error occurred, clear the result value. */
   if ( !astOK ) result = 0;

/* Return the result. */
   return result;
}

static double Distance( AstFrame *this,
                        const double point1[], const double point2[], int *status ) {
/*
*++
*  Name:
c     astDistance
f     AST_DISTANCE

*  Purpose:
*     Calculate the distance between two points in a Frame.

*  Type:
*     Public virtual function.

*  Synopsis:
c     #include "frame.h"
c     double astDistance( AstFrame *this,
c                         const double point1[], const double point2[] )
f     RESULT = AST_DISTANCE( THIS, POINT1, POINT2, STATUS )

*  Class Membership:
*     Frame method.

*  Description:
*     This function finds the distance between two points whose Frame
*     coordinates are given. The distance calculated is that along
*     the geodesic curve that joins the two points.
*
*     For example, in a basic Frame, the distance calculated will be
*     the Cartesian distance along the straight line joining the two
*     points. For a more specialised Frame describing a sky coordinate
*     system, however, it would be the distance along the great circle
*     passing through two sky positions.

*  Parameters:
c     this
f     THIS = INTEGER (Given)
*        Pointer to the Frame.
c     point1
f     POINT1( * ) = DOUBLE PRECISION (Given)
c        An array of double, with one element for each Frame axis
f        An array with one element for each Frame axis
*        (Naxes attribute) containing the coordinates of the first point.
c     point2
f     POINT2( * ) = DOUBLE PRECISION (Given)
c        An array of double, with one element for each Frame axis
f        An array with one element for each Frame axis
*        containing the coordinates of the second point.
f     STATUS = INTEGER (Given and Returned)
f        The global status.

*  Returned Value:
c     astDistance
f     AST_DISTANCE = DOUBLE PRECISION
*        The distance between the two points.

*  Notes:
*     - This function will return a "bad" result value (AST__BAD) if
*     any of the input coordinates has this value.
*     - A "bad" value will also be returned if this function is
c     invoked with the AST error status set, or if it should fail for
f     invoked with STATUS set to an error value, or if it should fail for
*     any reason.
*--
*/

/* Local Variables: */
   double delta;                 /* Separation along an axis */
   double result;                /* Result value to return */
   int axis;                     /* Loop counter for axes */
   int naxes;                    /* Number of Frame axes */

/* Initialise. */
   result = AST__BAD;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Obtain the number of Frame axes. */
   naxes = astGetNaxes( this );
   if ( astOK ) {

/* Loop to determine the Cartesian distance between the points. */
      result = 0.0;
      for ( axis = 0; axis < naxes; axis++ ) {

/* If any of the coordinates supplied is bad, set the distance to be
   bad and quit looping. */
         if ( ( point1[ axis ] == AST__BAD ) ||
              ( point2[ axis ] == AST__BAD ) ) {
            result = AST__BAD;
            break;

/* Otherwise, accumulate the sum of squared separations along each
   axis. */
         } else {
            delta = point1[ axis ] - point2[ axis ];
            result += ( delta * delta );
         }
      }

/* Take the square root to find the distance (if valid). */
      if ( result != AST__BAD ) result = sqrt( result );
   }

/* Return the result. */
   return result;
}

static int Equal( AstObject *this_object, AstObject *that_object, int *status ) {
/*
*  Name:
*     Equal

*  Purpose:
*     Test if two Frames are equivalent.

*  Type:
*     Private function.

*  Synopsis:
*     #include "frame.h"
*     int Equal( AstObject *this, AstObject *that, int *status )

*  Class Membership:
*     Frame member function (over-rides the astEqual protected
*     method inherited from the Mapping class).

*  Description:
*     This function returns a boolean result (0 or 1) to indicate whether
*     two Frames are equivalent.

*  Parameters:
*     this
*        Pointer to the first Frame.
*     that
*        Pointer to the second Frame.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     One if the Frames are equivalent, zero otherwise.

*  Notes:
*     - The two Frames are considered equivalent if the Mapping between
*     them is a UnitMap.
*     - A value of zero will be returned if this function is invoked
*     with the global status set, or if it should fail for any reason.
*/

/* Local Variables: */
   AstFrame *that;            /* Pointer to the second Frame structure */
   AstFrame *this;            /* Pointer to the first Frame structure */
   AstFrameSet *fs;           /* FrameSet connecting the two Frames */
   AstMapping *map1;          /* Mapping connecting the two Frames */
   AstMapping *map2;          /* Simplified mapping connecting two Frames */
   int result;                /* Result value to return */

/* Initialise. */
   result = 0;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Checks that the second object is of the same class as the first . */
   if( !strcmp( astGetClass( this_object ), astGetClass( that_object ) ) ){

/* Obtain pointers to the two Frame structures. */
      this = (AstFrame *) this_object;
      that = (AstFrame *) that_object;

/* Get the Mapping between them, and see if it is a UnitMap. */
      fs = astConvert( that, this, "" );
      if( fs ) {
         map1 = astGetMapping( fs, AST__BASE, AST__CURRENT );
         map2 = astSimplify( map1 );
         result = astIsAUnitMap( map2 );
         map1 = astAnnul( map1 );
         map2 = astAnnul( map2 );
         fs = astAnnul( fs );
      }
   }

/* If an error occurred, clear the result value. */
   if ( !astOK ) result = 0;

/* Return the result, */
   return result;
}

static int Fields( AstFrame *this, int axis, const char *fmt,
                   const char *str, int maxfld, char **fields,
                   int *nc, double *val, int *status ) {
/*
*+
*  Name:
*     astFields

*  Purpose:
*     Identify numerical fields within a formatted Axis value.

*  Type:
*     Protected virtual function.

*  Synopsis:
*     #include "frame.h"
*     int astFields( AstFrame *this, int axis, const char *fmt,
*                    const char *str, int maxfld, char **fields,
*                    int *nc, double *val )

*  Class Membership:
*     Frame method.

*  Description:
*     This function identifies the numerical fields within a Frame axis
*     value that has been formatted using astAxisFormat. It assumes that
*     the value was formatted using the supplied format string. It also
*     returns the equivalent floating point value.

*  Parameters:
*     this
*        Pointer to the Frame.
*     axis
*        The number of the Frame axis for which the values have been
*        formatted (axis numbering starts at zero for the first axis).
*     fmt
*        Pointer to a constant null-terminated string containing the
*        format used when creating "str".
*     str
*        Pointer to a constant null-terminated string containing the
*        formatted value.
*     maxfld
*        The maximum number of fields to identify within "str".
*     fields
*        A pointer to an array of at least "maxfld" character pointers.
*        Each element is returned holding a pointer to the start of the
*        corresponding field  in "str" (in the order in which they occur
*        within "str"), or NULL if no corresponding field can be found.
*     nc
*        A pointer to an array of at least "maxfld" integers. Each
*        element is returned holding the number of characters in the
*        corresponding field, or zero if no corresponding field can be
*        found.
*     val
*        Pointer to a location at which to store the value
*        equivalent to the returned field values. If this is NULL,
*        it is ignored.

*  Returned Value:
*     The number of fields succesfully identified and returned.

*  Notes:
*     - Leading and trailing spaces are ignored.
*     - If the formatted value is not consistent with the supplied format
*     string, then a value of zero will be returned, "fields" will be
*     returned holding NULLs, "nc" will be returned holding zeros, and
*     "val" is returned holding VAL__BAD.
*     - Fields are counted from the start of the formatted string. If the
*     string contains more than "maxfld" fields, then trailing fields are
*     ignored.
*     - If this function is invoked with the global error status set, or
*     if it should fail for any reason, then a value of zero will be returned
*     as the function value, and "fields", "nc" and "val"  will be returned
*     holding their supplied values
*-
*/

/* Local Variables: */
   AstAxis *ax;                  /* Pointer to Axis object */
   int result;                   /* Result field count to return */

/* Initialise. */
   result = 0;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Validate the axis index and obtain a pointer to the required
   Axis. */
   (void) astValidateAxis( this, axis, "astFields" );
   ax = astGetAxis( this, axis );

/* Invoke the Axis astAxisFields method to perform the processing. */
   result = astAxisFields( ax, fmt, str, maxfld, fields, nc, val );

/* Annul the Axis pointer. */
   ax = astAnnul( ax );

/* If an error occurred, clear the returned value. */
   if ( !astOK ) result = 0;

/* Return the result. */
   return result;
}

static AstFrameSet *FindFrame( AstFrame *target, AstFrame *template,
                               const char *domainlist, int *status ) {
/*
*++
*  Name:
c     astFindFrame
f     AST_FINDFRAME

*  Purpose:
*     Find a coordinate system with specified characteristics.

*  Type:
*     Public virtual function.

*  Synopsis:
c     #include "frame.h"
c     AstFrameSet *astFindFrame( AstFrame *target, AstFrame *template,
c                                const char *domainlist )
f     RESULT = AST_FINDFRAME( TARGET, TEMPLATE, DOMAINLIST, STATUS )

*  Class Membership:
*     Frame method.

*  Description:
*     This function uses a "template" Frame to search another Frame
*     (or FrameSet) to identify a coordinate system which has a
*     specified set of characteristics. If a suitable coordinate
*     system can be found, the function returns a pointer to a
*     FrameSet which describes the required coordinate system and how
*     to convert coordinates to and from it.
*
*     This function is provided to help answer general questions about
*     coordinate systems, such as typically arise when coordinate
*     information is imported into a program as part of an initially
*     unknown dataset. For example:
*     - Is there a wavelength scale?
*     - Is there a 2-dimensional coordinate system?
*     - Is there a celestial coordinate system?
*     - Can I plot the data in ecliptic coordinates?
*
*     You can also use this function as a means of reconciling a
*     user's preference for a particular coordinate system (for
*     example, what type of axes to draw) with what is actually
*     possible given the coordinate information available.
*
*     To perform a search, you supply a "target" Frame (or FrameSet)
*     which represents the set of coordinate systems to be searched.
*     If a basic Frame is given as the target, this set of coordinate
*     systems consists of the one described by this Frame, plus all
*     other "virtual" coordinate systems which can potentially be
*     reached from it by applying built-in conversions (for example,
*     any of the celestial coordinate conversions known to the AST
*     library would constitute a "built-in" conversion). If a FrameSet
*     is given as the target, the set of coordinate systems to be
*     searched consists of the union of those represented by all the
*     individual Frames within it.
*
*     To select from this large set of possible coordinate systems,
*     you supply a "template" Frame which is an instance of the type
*     of Frame you are looking for. Effectively, you then ask the
*     function to "find a coordinate system that looks like this".
*
*     You can make your request more or less specific by setting
*     attribute values for the template Frame. If a particular
*     attribute is set in the template, then the function will only
*     find coordinate systems which have exactly the same value for
*     that attribute.  If you leave a template attribute un-set,
*     however, then the function has discretion about the value the
*     attribute should have in any coordinate system it finds. The
*     attribute will then take its value from one of the actual
*     (rather than virtual) coordinate systems in the target. If the
*     target is a FrameSet, its Current attribute will be modified to
*     indicate which of its Frames was used for this purpose.
*
*     The result of this process is a coordinate system represented by
*     a hybrid Frame which acquires some attributes from the template
*     (but only if they were set) and the remainder from the
*     target. This represents the "best compromise" between what you
*     asked for and what was available. A Mapping is then generated
*     which converts from the target coordinate system to this hybrid
*     one, and the returned FrameSet encapsulates all of this
*     information.

*  Parameters:
c     target
f     TARGET = INTEGER (Given)
*        Pointer to the target Frame (or FrameSet).
*
*        Note that if a FrameSet is supplied (and a suitable
*        coordinate system is found), then its Current attribute will
*        be modified to indicate which Frame was used to obtain
*        attribute values which were not specified by the template.
*        This Frame will, in some sense, represent the "closest"
*        non-virtual coordinate system to the one you requested.
c     template
f     TEMPLATE = INTEGER (Given)
*        Pointer to the template Frame, which should be an instance of
*        the type of Frame you wish to find. If you wanted to find a
*        Frame describing a celestial coordinate system, for example,
*        then you might use a SkyFrame here. See the "Examples"
*        section for more ideas.
c     domainlist
f     DOMAINLIST = CHARACTER * ( * ) (Given)
c        Pointer to a null-terminated character string containing a
f        A character string containing a
*        comma-separated list of Frame domains. This may be used to
*        establish a priority order for the different types of
*        coordinate system that might be found.
*
*        The function will first try to find a suitable coordinate
*        system whose Domain attribute equals the first domain in this
*        list. If this fails, the second domain in the list will be
*        used, and so on, until a result is obtained. A blank domain
*        (e.g. two consecutive commas) indicates that any coordinate
*        system is acceptable (subject to the template) regardless of
*        its domain.
*
*        This list is case-insensitive and all white space is ignored.
*        If you do not wish to restrict the domain in this way,
c        you should supply an empty string.
f        you should supply a blank string.
f     STATUS = INTEGER (Given and Returned)
f        The global status.

*  Returned Value:
c     astFindFrame()
f     AST_FINDFRAME = INTEGER
*        If the search is successful, the function returns a pointer
*        to a FrameSet which contains the Frame found and a
*        description of how to convert to (and from) the coordinate
*        system it represents. Otherwise, a null Object pointer
*        (AST__NULL) is returned without error.
*
*        If a FrameSet is returned, it will contain two Frames. Frame
*        number 1 (its base Frame) represents the target coordinate
*        system and will be the same as the (base Frame of the)
*        target. Frame number 2 (its current Frame) will be a Frame
*        representing the coordinate system which the function
*        found. The Mapping which inter-relates these two Frames will
*        describe how to convert between their respective coordinate
*        systems.
*
*        Note that a FrameSet may be used both as a Mapping and as a
*        Frame. If the result is used as a Mapping (e.g. with
*        astTran2), then it provides a means of converting coordinates
*        from the target coordinate system into the new coordinate
*        system that was found (and vice versa if its inverse
*        transformation is selected). If it is used as a Frame, its
*        attributes will describe the new coordinate system.

*  Applicability:
*     Frame
*        This function applies to all Frames.
*     FrameSet
*        If the target is a FrameSet, the possibility exists that
*        several of the Frames within it might be matched by the
*        template.  Unless the choice is sufficiently restricted by
c        the "domainlist" string, the sequence in which Frames are
f        the DOMAINLIST string, the sequence in which Frames are
*        searched can then become important. In this case, the search
*        proceeds as follows:
c        - Each field in the "domainlist" string is considered in turn.
f        - Each field in the DOMAINLIST string is considered in turn.
*        - An attempt is made to match the template to each of the
*        target's Frames in the order: (1) the current Frame, (2) the
*        base Frame, (3) each remaining Frame in the order of being
*        added to the target FrameSet.
*        - Generally, the first match found is used. However, the
*        Mapping between the target coordinate system and the
*        resulting Frame is also examined. Preference is given to
*        cases where both the forward and inverse transformations are
*        defined (as indicated by the TranForward and TranInverse
*        attributes). If only one transformation is defined, the
*        forward one is preferred.
*        - If a match is found and the domain of the resulting Frame also
c        matches the current "domainlist" field, it is
f        matches the current DOMAINLIST field, it is
c        accepted. Otherwise, the next "domainlist" field is considered
f        accepted. Otherwise, the next DOMAINLIST field is considered
*        and the process repeated.
*
*        If a suitable coordinate system is found, the Current
*        attribute of the target FrameSet will be modified on exit to
*        identify the Frame whose match with the target was eventually
*        accepted.

*  Examples:
c     result = astFindFrame( target, astFrame( 3, "" ), "" );
f     RESULT = AST_FINDFRAME( TARGET, AST_FRAME( 3, ' ', STATUS ), ' ', STATUS )
*        Searches for a 3-dimensional coordinate system in the target
*        Frame (or FrameSet). No attributes have been set in the
c        template Frame (created by astFrame), so no restriction has
f        template Frame (created by AST_FRAME), so no restriction has
*        been placed on the required coordinate system, other than
*        that it should have 3 dimensions. The first suitable Frame
c        found will be returned as part of the "result" FrameSet.
f        found will be returned as part of the RESULT FrameSet.
c     result = astFindFrame( target, astSkyFrame( "" ), "" );
f     RESULT = AST_FINDFRAME( TARGET, AST_SKYFRAME( ' ', STATUS ), ' ', STATUS )
*        Searches for a celestial coordinate system in the target
*        Frame (or FrameSet). The type of celestial coordinate system
c        is unspecified, so astFindFrame will return the first one
f        is unspecified, so AST_FINDFRAME will return the first one
c        found as part of the "result" FrameSet. If the target is
f        found as part of the RESULT FrameSet. If the target is
*        a FrameSet, then its Current attribute will be updated to
*        identify the Frame that was used.
*
*        If no celestial coordinate system can be found, a value of
*        AST__NULL will be returned without error.
c     result = astFindFrame( target, astSkyFrame( "MaxAxes=100" ), "" );
f     RESULT = AST_FINDFRAME( TARGET, AST_SKYFRAME( 'MaxAxes=100', STATUS ), ' ', STATUS )
*        This is like the last example, except that in the event of the
*        target being a CmpFrame, the component Frames encapsulated by the
*        CmpFrame will be searched for a SkyFrame. If found, the returned
*        Mapping will included a PermMap which selects the required axes
*        from the target CmpFrame.
*
*        This is acomplished by setting the MaxAxes attribute of the
*        template SkyFrame to a large number (larger than or equal to the
*        number of axes in the target CmpFrame). This allows the SkyFrame
*        to be used as a match for Frames containing from 2 to 100 axes.
c     result = astFindFrame( target, astSkyFrame( "System=FK5" ), "" );
f     RESULT = AST_FINDFRAME( TARGET, AST_SKYFRAME( 'System=FK5', STATUS ), ' ', STATUS )
*        Searches for an equatorial (FK5) coordinate system in the
*        target. The Equinox value for the coordinate system has not
*        been specified, so will be obtained from the target. If the
*        target is a FrameSet, its Current attribute will be updated
*        to indicate which SkyFrame was used to obtain this value.
c     result = astFindFrame( target, astFrame( 2, "" ), "sky,pixel," );
f     RESULT = AST_FINDFRAME( TARGET, AST_FRAME( 2, ' ', STATUS ), 'SKY,PIXEL,', STATUS )
*        Searches for a 2-dimensional coordinate system in the
*        target. Initially, a search is made for a suitable coordinate
*        system whose Domain attribute has the value "SKY". If this
*        search fails, a search is then made for one with the domain
*        "PIXEL". If this also fails, then any 2-dimensional
c        coordinate system is returned as part of the "result"
f        coordinate system is returned as part of the RESULT
*        FrameSet.
*
*        Only if no 2-dimensional coordinate systems can be reached by
*        applying built-in conversions to any of the Frames in the
*        target will a value of AST__NULL be returned.
c     result = astFindFrame( target, astFrame( 1, "Domain=WAVELENGTH" ), "" );
f     RESULT = AST_FINDFRAME( TARGET, AST_FRAME( 1, 'Domain=WAVELENGTH', STATUS ), ' ', STATUS )
*        Searches for any 1-dimensional coordinate system in the
*        target which has the domain "WAVELENGTH".
c     result = astFindFrame( target, astFrame( 1, "" ), "wavelength" );
f     RESULT = AST_FINDFRAME( TARGET, AST_FRAME( 1, ' ', STATUS ), 'WAVELENGTH', STATUS )
*        This example has exactly the same effect as that above. It
*        illustrates the equivalence of the template's Domain attribute
c        and the fields in the "domainlist" string.
f        and the fields in the DOMAINLIST string.
c     result = astFindFrame( target, astFrame( 1, "MaxAxes=3" ), "" );
f     RESULT = AST_FINDFRAME( TARGET, AST_FRAME( 1, 'MaxAxes=3', STATUS ), ' ', STATUS )
*        This is a more advanced example which will search for any
*        coordinate system in the target having 1, 2 or 3
c        dimensions. The Frame returned (as part of the "result"
f        dimensions. The Frame returned (as part of the RESULT
*        FrameSet) will always be 1-dimensional, but will be related
*        to the coordinate system that was found by a suitable Mapping
*        (e.g. a PermMap) which simply extracts the first axis.
*
*        If we had wanted a Frame representing the actual (1, 2 or
*        3-dimensional) coordinate system found, we could set the
*        PreserveAxes attribute to a non-zero value in the template.
c     result = astFindFrame( target, astSkyFrame( "Permute=0" ), "" );
f     RESULT = AST_FINDFRAME( TARGET, AST_SKYFRAME( 'Permute=0', STATUS ), ' ', STATUS )
*        Searches for any celestial coordinate system in the target,
*        but only finds one if its axes are in the conventional
*        (longitude,latitude) order and have not been permuted
c        (e.g. with astPermAxes).
f        (e.g. with AST_PERMAXES).

*  Notes:
*     -  The Mapping represented by the returned FrameSet results in
*     alignment taking place in the coordinate system specified by the
c     AlignSystem attribute of the "template" Frame. See the description
f     AlignSystem attribute of the TEMPLATE Frame. See the description
*     of the AlignSystem attribute for further details.
*     - Beware of setting the Domain attribute of the template and then
c     using a "domainlist" string which does not include the template's domain
f     using a DOMAINLIST string which does not include the template's domain
*     (or a blank field). If you do so, no coordinate system will be
*     found.
*     - A null Object pointer (AST__NULL) will be returned if this
c     function is invoked with the AST error status set, or if it
f     function is invoked with STATUS set to an error value, or if it
*     should fail for any reason.

*  More on Using Templates:
*     A Frame (describing a coordinate system) will be found by this
*     function if (a) it is "matched" by the template you supply, and
c     (b) the value of its Domain attribute appears in the "domainlist"
f     (b) the value of its Domain attribute appears in the DOMAINLIST
*     string (except that a blank field in this string permits any
*     domain). A successful match by the template depends on a number
*     of criteria, as outlined below:
*     - In general, a template will only match another Frame which
*     belongs to the same class as the template, or to a derived (more
*     specialised) class. For example, a SkyFrame template will match
*     any other SkyFrame, but will not match a basic
*     Frame. Conversely, a basic Frame template will match any class
*     of Frame.
*     - The exception to this is that a Frame of any class can be used to
*     match a CmpFrame, if that CmpFrame contains a Frame of the same
*     class as the template. Note however, the MaxAxes and MinAxes
*     attributes of the template must be set to suitable values to allow
*     it to match the CmpFrame. That is, the MinAxes attribute must be
*     less than or equal to the number of axes in the target, and the MaxAxes
*     attribute must be greater than or equal to the number of axes in
*     the target.
*     - If using a CmpFrame as a template frame, the MinAxes and MaxAxes
*     for the template are determined by the MinAxes and MaxAxes values of
*     the component Frames within the template. So if you want a template
*     CmpFrame to be able to match Frames with different numbers of axes,
*     then you must set the MaxAxes and/or MinAxes attributes in the component
*     template Frames, before combining them together into the template
*     CmpFrame.
*     - If a template has a value set for any of its main attributes, then
*     it will only match Frames which have an identical value for that
*     attribute (or which can be transformed, using a built-in
*     conversion, so that they have the required value for that
*     attribute). If any attribute in the template is un-set, however,
*     then Frames are matched regardless of the value they may have
*     for that attribute. You may therefore make a template more or
*     less specific by choosing the attributes for which you set
*     values. This requirement does not apply to 'descriptive' attributes
*     such as titles, labels, symbols, etc.
*     - An important application of this principle involves the Domain
*     attribute. Setting the Domain attribute of the template has the
*     effect of restricting the search to a particular type of Frame
*     (with the domain you specify).  Conversely, if the Domain
*     attribute is not set in the template, then the domain of the
*     Frame found is not relevant, so all Frames are searched.  Note
*     that the
c     "domainlist" string provides an alternative way of restricting the
f     DOMAINLIST string provides an alternative way of restricting the
*     search in the same manner, but is a more convenient interface if
*     you wish to search automatically for another domain if the first
*     search fails.
*     - Normally, a template will only match a Frame which has the
*     same number of axes as itself. However, for some classes of
*     template, this default behaviour may be changed by means of the
*     MinAxes, MaxAxes and MatchEnd attributes. In addition, the
*     behaviour of a template may be influenced by its Permute and
*     PreserveAxes attributes, which control whether it matches Frames
*     whose axes have been permuted, and whether this permutation is
*     retained in the Frame which is returned (as opposed to returning
*     the axes in the order specified in the template, which is the
*     default behaviour). You should consult the descriptions of these
*     attributes for details of this more advanced use of templates.
*--
*/

/* Local Variables: */
   AstFrame *frame;              /* Pointer to result Frame */
   AstFrameSet *result;          /* Pointer to result FrameSet */
   AstMapping *map;              /* Pointer to result Mapping */
   AstMapping *tmp;              /* Temporary Mapping pointer */
   char *domain_copy;            /* Pointer to copy of result domain */
   char *domainlist_copy;        /* Pointer to copy of domains list */
   const char *domain;           /* Pointer to result Domain value */
   int *target_axes;             /* Pointer to target axis assignments */
   int *template_axes;           /* Pointer to template axis assignments */
   int i;                        /* Loop counter for characters */
   int j;                        /* Character index */
   int match;                    /* Template matched target? */

/* Initialise. */
   result = NULL;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Allocate space to store a copy of the domains list, with added
   commas. */
   domainlist_copy = astMalloc( strlen( domainlist ) + (size_t) 3 );
   if ( astOK ) {

/* Make a copy of the domains list, with an extra comma added at each
   end. Also remove all white space and convert to upper case. */
      domainlist_copy[ 0 ] = ',';
      for ( i = 0, j = 1; domainlist[ i ]; i++ ) {
         if ( !isspace( domainlist[ i ] ) ) {
            domainlist_copy[ j++ ] = toupper( domainlist[ i ] );
         }
      }
      domainlist_copy[ j++ ] = ',';
      domainlist_copy[ j ] = '\0';

/* Invoke the protected astMatch method associated with the template
   Frame. This matches the template to the target and returns
   information about how to convert between the target and the Frame
   it found (if any). */
      match = astMatch( template, target, 0,
                        &template_axes, &target_axes, &map, &frame );

/* If successful, obtain a pointer to the Domain string of the result
   Frame. Allocate space for a copy of this string, with added
   commas. */
      if ( match && astOK ) {
         domain = astGetDomain( frame );
         if ( astOK ) {
            domain_copy = astMalloc( strlen( domain ) + (size_t) 3 );
            if ( astOK ) {

/* Make a copy of the domain, adding an extra comma at each end. */
               domain_copy[ 0 ] = ',';
               for ( i = 0, j = 1; domain[ i ]; i++ ) {
                  domain_copy[ j++ ] = domain[ i ];
               }
               domain_copy[ j++ ] = ',';
               domain_copy[ j ] = '\0';

/* Test if the domain appears in the domains list (with added
   commas). If not, test if a blank domain (which permits the result
   Frame to have any Domain) appears instead. */
               if ( strstr( domainlist_copy, domain_copy ) ||
                    strstr( domainlist_copy, ",," ) ) {

/* If the result Frame is acceptable, simplify the result Mapping. */
                  tmp = astSimplify( map );
                  map = astAnnul( map );
                  map = tmp;

/* Build the result FrameSet. */
                  result = astFrameSet( target, "", status );
                  astAddFrame( result, AST__BASE, map, frame );
               }
            }

/* Free the copy of the result domain. */
            domain_copy = astFree( domain_copy );
         }

/* Free space and annul pointers allocated by astMatch. */
         template_axes = astFree( template_axes );
         target_axes = astFree( target_axes );
         map = astAnnul( map );
         frame = astAnnul( frame );
      }
   }

/* Free the copy of the domains list. */
   domainlist_copy = astFree( domainlist_copy );

/* If an error occurred, annul any result pointer. */
   if ( !astOK && result ) result = astAnnul( result );

/* Return the result. */
   return result;
}

const char *astFmtDecimalYr_( double year, int digits, int *status ) {
/*
*+
*  Name:
*     astFmtDecimalYr

*  Purpose:
*     Format an epoch expressed in years as a decimal string.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "frame.h"
*     const char *astFmtDecimalYr( double year, int digits )

*  Class Membership:
*     Frame member function.

*  Description:
*     This function formats an epoch expressed in years as a decimal string
*     and returns a pointer to the result. It is intended for formatting
*     Frame Epoch values, etc, for display.

*  Parameters:
*     year
*        The epoch to be formatted.
*     digits
*        The number of digits of precision required.

*  Returned Value:
*     Pointer to a null terminated string containing the formatted value.

*  Notes:
*     - The result string is stored in static memory and may be
*     over-written by a subsequent invocation of this function.
*     - A NULL pointer is returned if this function is invoked with
*     the global error status set or if it should fail for any reason.
*-
*/

/* Local Variables: */
   astDECLARE_GLOBALS            /* Declare the thread specific global data */
   const char *result;           /* Pointer value to return */
   int nc;                       /* Number of characters in buffer */

/* Check the global error status. */
   if ( !astOK ) return NULL;

/* Get a pointer to the structure holding thread-specific global data. */
   astGET_GLOBALS(NULL);

/* Limit the precision to what is meaningful. */
   digits = ( digits > DBL_DIG ) ? DBL_DIG : digits;

/* Format the year value. Use "g" format to avoid buffer overflow and
   to get useful diagnostic output if a silly value is given. */
   nc = sprintf( astfmtdecimalyr_buff, "%#.*g", digits, year );

/* Set the result value. */
   result = astfmtdecimalyr_buff;

/* Loop to remove redundant zeros from the end of the result. */
   while ( astfmtdecimalyr_buff[ --nc ] == '0' ) astfmtdecimalyr_buff[ nc ] = '\0';

/* If the last character is now a decimal point, put back one zero. */
   if ( astfmtdecimalyr_buff[ nc ] == '.' ) {
      astfmtdecimalyr_buff[ ++nc ] = '0';
      astfmtdecimalyr_buff[ ++nc ] = '\0';
   }

/* Return the result. */
   return astfmtdecimalyr_buff;
}

static const char *Format( AstFrame *this, int axis, double value, int *status ) {
/*
*+
*  Name:
*     astFormat

*  Purpose:
*     Format a coordinate value for a Frame axis.

*  Type:
*     Protected virtual function.

*  Synopsis:
*     #include "frame.h"
*     const char *astFormat( AstFrame *this, int axis, double value )

*  Class Membership:
*     Frame method.

*  Description:
*     This function returns a pointer to a string containing the
*     formatted (character) version of a coordinate value for a Frame
*     axis. The formatting applied is determined by the Frame's
*     attributes and, in particular, by any Format attribute string
*     that has been set for the axis. A suitable default format will
*     be applied if necessary.

*  Parameters:
*     this
*        Pointer to the Frame.
*     axis
*        The number of the Frame axis for which formatting is to be
*        performed (axis numbering starts at zero for the first axis).
*     value
*        The coordinate value to be formatted.

*  Returned Value:
*     A pointer to a null-terminated string containing the formatted
*     value.

*  Notes:
*     - The returned string pointer may point at memory allocated
*     within the Frame, or at static memory. The contents of the
*     string may be over-written or the pointer may become invalid
*     following a further invocation of the same function or any
*     modification of the Frame. A copy of the string should
*     therefore be made if necessary.
*     - A NULL pointer will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*-

*  Implementation Notes:
*     - This function implements the basic astFormat method available
*     via the protected interface to the Frame class. The public
*     interface to this method is provided by the astFormatId_
*     function.
*/

/* Local Variables: */
   AstAxis *ax;                  /* Pointer to Axis object */
   const char *result;           /* Pointer value to return */
   int digits_set;               /* Axis Digits attribute set? */

/* Check the global error status. */
   if ( !astOK ) return NULL;

/* Validate the axis index and obtain a pointer to the required Axis. */
   (void) astValidateAxis( this, axis, "astFormat" );
   ax = astGetAxis( this, axis );

/* Test if any Axis attributes which may affect the result are undefined (i.e.
   have not been explicitly set). If so, we over-ride them, giving them
   temporary values dictated by the Frame. Only the Digits attribute is
   relevant here. */
   digits_set = astTestAxisDigits( ax );
   if ( !digits_set ) astSetAxisDigits( ax, astGetDigits( this ) );

/* Format the value. */
   result = astAxisFormat( ax, value );

/* Clear any Axis attributes that were temporarily over-ridden. */
   if ( !digits_set ) astClearAxisDigits( ax );

/* Annul the Axis pointer. */
   ax = astAnnul( ax );

/* If an error occurred, clear the result value. */
   if ( !astOK ) result = NULL;

/* Return the result. */
   return result;
}

static AstPointSet *FrameGrid( AstFrame *this, int size, const double *lbnd,
                               const double *ubnd, int *status ){
/*
*+
*  Name:
*     astFrameGrid

*  Purpose:
*     Return a grid of points covering a rectangular area of a Frame.

*  Type:
*     Protected virtual function.

*  Synopsis:
*     #include "frame.h"
*     AstPointSet *astFrameGrid( AstFrame *this_frame, int size,
*                                const double *lbnd, const double *ubnd )

*  Class Membership:
*     Frame method.

*  Description:
*     This function returns a PointSet containing positions spread
*     approximately evenly throughtout a specified rectangular area of
*     the Frame.

*  Parameters:
*     this
*        Pointer to the Frame.
*     size
*        The preferred number of points in the returned PointSet. The
*        actual number of points in the returned PointSet may be
*        different, but an attempt is made to stick reasonably closely to
*        the supplied value.
*     lbnd
*        Pointer to an array holding the lower bound of the rectangular
*        area on each Frame axis. The array should have one element for
*        each Frame axis.
*     ubnd
*        Pointer to an array holding the upper bound of the rectangular
*        area on each Frame axis. The array should have one element for
*        each Frame axis.

*  Returned Value:
*     A pointer to a new PointSet holding the grid of points.

*  Notes:
*     - A NULL pointer is returned if an error occurs.
*-
*/

/* Local Variables: */
   AstPointSet *result;
   const char *unit;
   double **ptr;
   double *gmean;
   double *step;
   int *maxi;
   int *nsame;
   int *ntick;
   int *pi;
   int bad;
   int iax;
   int ipp;
   int jax;
   int naxes;
   int np;
   int ntick0;

/* Initialise. */
   result = NULL;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Get the number of axes in the Frame. */
   naxes = astGetNaxes( this );

/* Allocate an array to hold the number of ticks along each axis. */
   ntick = astMalloc( sizeof(int)*naxes );

/* Allocate an array to hold the geometric mean of the lengths of the
   axes that have the same units as the current axis. */
   gmean = astMalloc( naxes*sizeof(double) );

/* Allocate an array to hold the number of axes that share the same unit. */
   nsame = astMalloc( naxes*sizeof(int) );
   if( astOK ) {

/* For each axis, find the total number of axes in the Frame that have
   the same unit string. Also, find the product of the lengths of these
   axes. */
      bad = 0;
      for( iax = 0; iax < naxes; iax++ ) {
         nsame[ iax ] = 1;

         if( ubnd[ iax ] == AST__BAD &&
             lbnd[ iax ] == AST__BAD ) {
            bad = 1;
            break;
         }

         gmean[ iax ] = ubnd[ iax ] - lbnd[ iax ];
         unit = astGetUnit( this, iax );
         for( jax = 0; jax < naxes; jax++ ) {
            if( jax != iax ) {
               if( astOK && !strcmp( unit, astGetUnit( this, jax ) ) ) {
                  nsame[ iax ]++;
                  gmean[ iax ] *= ubnd[ jax ] - lbnd[ jax ];
               }
            }
         }
      }

/* Do nothing if any bad bounds were supplied, or if the size is less
   than 1. */
      if( !bad && size >= 1 ) {

/* Get the nominal number of ticks per axis. */
         ntick0 = pow( size, 1.0/(double)naxes );
         if( ntick0 < 2 ) ntick0 = 2;

/* Convert the dimension products into geometric means. */
         for( iax = 0; iax < naxes; iax++ ) {
            gmean[ iax ] = pow( fabs(gmean[ iax ]), 1.0/(double)nsame[ iax ] );
         }

/* The number of ticks to use on each axis is equal to the nominal number
   multiplied by the ratio of the axis length to the geometric mean of the
   axis lengths that sahare the same unit string. This gives more ticks
   on the longer axes within any group of common-unit axes, whilst
   retaining the overall number of ticks (roughly). Also find the total
   number of points. */
         np = 1;
         for( iax = 0; iax < naxes; iax++ ) {
            ntick[ iax ] = ntick0*( ubnd[ iax ] - lbnd[ iax ] )/gmean[ iax ];
            if( ntick[ iax ] < 2 ) ntick[ iax ] = 2;
            np *= ntick[ iax ];
         }

/* Create a PointSet large enough to hold this many points. */
         result = astPointSet( np, naxes, " ", status );
         ptr = astGetPoints( result );

/* Allocate memory to hold the max indices on each axis. */
         maxi = astMalloc( sizeof( int )*(size_t) naxes );

/* Allocate memory to hold the indices of the current position.*/
         pi = astMalloc( sizeof( int )*(size_t) naxes );

/* Allocate memory to hold the step size for each axis. */
         step = astMalloc( sizeof( double )*(size_t) naxes );
         if( astOK ) {

/* For every axis, set up the step size, initialise the current position to
   the lower bound, and store a modified upper limit which includes some
   safety marging to allow for rounding errors. */
            for( iax = 0; iax < naxes; iax++ ) {
               step[ iax ] = ( ubnd[ iax ] - lbnd[ iax ] )/( ntick[ iax ] - 1 );
               pi[ iax ] = 0;
               maxi[ iax ] = ntick[ iax ] - 1;
            }

/* Initialise the index of the next position to store. */
            ipp = 0;

/* Loop round adding points to the array until the whole volume has been
   done. */
            iax = 0;
            while( iax < naxes ) {

/* Add the current point to the supplied array, and increment the index of
   the next point to add. */
               for( iax = 0; iax < naxes; iax++ ) {
                  ptr[ iax ][ ipp ] = lbnd[ iax ] + pi[ iax ]*step[ iax ];
               }
               ipp++;

/* We now move the current position on to the next sample */
               iax = 0;
               while( iax < naxes ) {
                  pi[ iax ]++;
                  if( pi[ iax ] > maxi[ iax ] ) {
                     pi[ iax ] = 0;
                     iax++;
                  } else {
                     break;
                  }
               }
            }
         }

/* Free resources. */
         maxi = astFree( maxi );
         pi = astFree( pi );
         step = astFree( step );

/* Report error if supplied values were bad. */
      } else if( astOK ) {
         if( bad ) {
            astError( AST__ATTIN, "astFrameGrid(%s): One of more of the "
                      "supplied bounds is AST__BAD (programming error).",
                      status, astGetClass( this ) );
         } else if( size < 1 ) {
            astError( AST__ATTIN, "astFrameGrid(%s): The supplied grid "
                      "size (%d) is invalid (programming error).",
                      status, astGetClass( this ), size );
         }
      }
   }

/* Free resources. */
   ntick = astFree( ntick );
   nsame = astFree( nsame );
   gmean = astFree( gmean );

/* Annul the returned PointSet if an error has occurred. */
   if( !astOK ) result = astAnnul( result );

/* Return the PointSet holding the grid. */
   return result;
}

static double Gap( AstFrame *this, int axis, double gap, int *ntick, int *status ) {
/*
*+
*  Name:
*     astGap

*  Purpose:
*     Find a "nice" gap for tabulating Frame axis values.

*  Type:
*     Protected virtual function.

*  Synopsis:
*     #include "frame.h"
*     double astGap( AstFrame *this, int axis, double gap, int *ntick )

*  Class Membership:
*     Frame method.

*  Description:
*     This function returns a gap size which produces a nicely spaced
*     series of formatted values for a Frame axis, the returned gap
*     size being as close as possible to the supplied target gap
*     size. It also returns a convenient number of divisions into
*     which the gap can be divided.

*  Parameters:
*     this
*        Pointer to the Frame.
*     axis
*        The number of the axis (zero-based) for which a gap is to be found.
*     gap
*        The target gap size.
*     ntick
*        Address of an int in which to return a convenient number of
*        divisions into which the gap can be divided.

*  Returned Value:
*     The nice gap size.

*  Notes:
*     - A value of zero is returned if the target gap size is zero.
*     - A negative gap size is returned if the supplied gap size is negative.
*     - A value of zero will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*-
*/

/* Local Variables: */
   AstAxis *ax;                  /* Pointer to Axis object */
   double result;                /* The nice gap value */

/* Check the global error status. */
   if ( !astOK ) return 0.0;

/* Validate the axis index and obtain a pointer to the required
   Axis. */
   (void) astValidateAxis( this, axis, "astGap" );
   ax = astGetAxis( this, axis );

/* Find the gap. */
   result = astAxisGap( ax, gap, ntick );

/* Annul the Axis pointer. */
   ax = astAnnul( ax );

/* If an error occurred, clear the result value. */
   if ( !astOK ) result = 0.0;

/* Return the result. */
   return result;
}

static int GetActiveUnit( AstFrame *this, int *status ){
/*
*++
*  Name:
c     astGetActiveUnit
f     AST_GETACTIVEUNIT

*  Purpose:
*     Determines how the Unit attribute will be used.

*  Type:
*     Public virtual function.

*  Synopsis:
c     #include "frame.h"
c     int astGetActiveUnit( AstFrame *this )
f     RESULT = AST_GETACTIVEUNIT( THIS, STATUS )

*  Class Membership:
*     Frame method.

*  Description:
c     This function
f     This routine
*     returns the current value of the ActiveUnit flag for a Frame. See
c     the description of the astSetActiveUnit function
f     the description of the AST_SETACTIVEUNIT routine
*     for a description of the ActiveUnit flag.

*  Parameters:
c     this
f     THIS = INTEGER (Given)
*        Pointer to the Frame.
f     STATUS = INTEGER (Given and Returned)
f        The global status.

*  Returned Value:
c     astGetActiveUnit
f     AST_GETACTIVEUNIT = LOGICAL
*        The current value of the ActiveUnit flag.

*  Notes:
c     - A zero value will be returned if this function is
c     invoked with the AST error status set, or if it should fail for
f     - A value of .FALSE. will be returned if this function is
f     invoked with STATUS set to an error value, or if it should fail for
*     any reason.
*--
*/

/* Local Variables: */
   AstAxis *ax;        /* Pointer to axis structure */
   int i;              /* Index of axis in Frame */
   int has_skyaxis;    /* Does Frame contain any SkyAxes? */
   int nax;            /* Number of axes in Frame */
   int result;         /* The returned value */

/* Initialise. */
   result = 0;

/* Check the global error status. */
   if ( !astOK ) return result;

/* See if the Frame contains a SkyAxis. */
   has_skyaxis = 0;
   nax = astGetNaxes( this );
   for( i = 0; i < nax; i++ ) {
      ax = astGetAxis( this, i );
      if( astIsASkyAxis( ax ) ) has_skyaxis = 1;
      ax = astAnnul( ax );
   }

/* If the Frame contains a SkyAxis the ActiveUnit flag is always zero. */
   if( !has_skyaxis ) {

/* Otherwise, get the value from the Frame. If it has not yet been assigned a
   value return the value zero. */
      result = this->active_unit;
      if( result == -INT_MAX ) result = 0;
   }

/* Return the result. */
   return result;
}

static const char *GetAttrib( AstObject *this_object, const char *attrib, int *status ) {
/*
*  Name:
*     GetAttrib

*  Purpose:
*     Get the value of a specified attribute for a Frame.

*  Type:
*     Private function.

*  Synopsis:
*     #include "frame.h"
*     const char *GetAttrib( AstObject *this, const char *attrib, int *status )

*  Class Membership:
*     Frame member function (over-rides the protected astGetAttrib
*     method inherited from the Mapping class).

*  Description:
*     This function returns a pointer to the value of a specified
*     attribute for a Frame, formatted as a character string.

*  Parameters:
*     this
*        Pointer to the Frame.
*     attrib
*        Pointer to a null-terminated string containing the name of
*        the attribute whose value is required. This name should be in
*        lower case, with all white space removed.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     - Pointer to a null-terminated string containing the attribute
*     value.

*  Notes:
*     - This function uses one-based axis numbering so that it is
*     suitable for external (public) use.
*     - The returned string pointer may point at memory allocated
*     within the Frame, or at static memory. The contents of the
*     string may be over-written or the pointer may become invalid
*     following a further invocation of the same function or any
*     modification of the Frame. A copy of the string should
*     therefore be made if necessary.
*     - A NULL pointer will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*/

/* Local Variables: */
   astDECLARE_GLOBALS            /* Declare the thread specific global data */
   AstAxis *ax;                  /* Pointer to Axis */
   AstFrame *pfrm;               /* Pointer to primary Frame containing axis */
   AstFrame *this;               /* Pointer to the Frame structure */
   AstSystemType system;         /* System code */
   char pfrm_attrib[ 100 ];      /* Primary Frame attribute */
   char *axis_attrib;            /* Pointer to axis attribute name */
   const char *old_attrib;       /* Pointer to supplied attribute name string */
   const char *result;           /* Pointer value to return */
   double dval;                  /* Double attibute value */
   double epoch;                 /* Epoch attribute value (as MJD) */
   int axis;                     /* Frame axis number */
   int axis_nc;                  /* No. characters in axis attribute name */
   int digits;                   /* Digits attribute value */
   int direction;                /* Direction attribute value */
   int free_axis_attrib;         /* Should axis_attrib be freed? */
   int has_axis;                 /* Does attrib name include axis specifier? */
   int len;                      /* Length of attrib string */
   int match_end;                /* MatchEnd attribute value */
   int max_axes;                 /* MaxAxes attribute value */
   int min_axes;                 /* MinAxes attribute value */
   int naxes;                    /* Naxes attribute value */
   int nc;                       /* No. characters read by astSscanf */
   int oldrep;                   /* Original error reporting state */
   int paxis;                    /* Axis index within primary frame */
   int permute;                  /* Permute attribute value */
   int preserve_axes;            /* PreserveAxes attribute value */
   int used;                     /* Could the setting string be used? */

/* Initialise. */
   result = NULL;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Get a pointer to the structure holding thread-specific global data. */
   astGET_GLOBALS(this_object);

/* Obtain a pointer to the Frame structure. */
   this = (AstFrame *) this_object;

/* Set a flag indicating if the attribute name includes an axis
   specifier. */
   has_axis = ( strchr( attrib, '(' ) != NULL );

/* A flag indicating that we do not need to free the axis_attrib memory. */
   free_axis_attrib = 0;

/* Initialise things to avoid compiler warnings. */
   axis_attrib = NULL;
   old_attrib = NULL;

/* Jump back to here if we are trying the same attribute but with an explicit
   axis "(1)" added to the end of the name. */
L1:

/* Obtain the length of the attrib string. */
   len = strlen( attrib );

/* Save the number of axes in the Frame for later use. */
   naxes = astGetNaxes( this );

/* Compare "attrib" with each recognised attribute name in turn,
   obtaining the value of the required attribute. If necessary, write
   the value into "getattrib_buff" as a null-terminated string in an appropriate
   format.  Set "result" to point at the result string. */

/* Digits. */
/* ------- */
   if ( !strcmp( attrib, "digits" ) ) {
      digits = astGetDigits( this );
      if ( astOK ) {
         (void) sprintf( getattrib_buff, "%d", digits );
         result = getattrib_buff;
      }

/* Digits(axis). */
/* ------------- */
   } else if ( nc = 0,
               ( 1 == astSscanf( attrib, "digits(%d)%n", &axis, &nc ) )
               && ( nc >= len ) ) {

/* There is no function to obtain the Digits attribute value for an
   axis directly, so obtain a pointer to the Axis and use this to
   obtain the value. Use the Frame's Digits attribute instead if the
   Axis attribute value is not set. */
      (void) astValidateAxis( this, axis - 1, "astGetDigits(axis)" );
      ax = astGetAxis( this, axis - 1 );
      if ( astTestAxisDigits( ax ) ) {
         digits = astGetAxisDigits( ax );
      } else {
         digits = astGetDigits( this );
      }
      ax = astAnnul( ax );
      if ( astOK ) {
         (void) sprintf( getattrib_buff, "%d", digits );
         result = getattrib_buff;
      }


/* Direction(axis). */
/* ---------------- */
   } else if ( nc = 0,
               ( 1 == astSscanf( attrib, "direction(%d)%n", &axis, &nc ) )
               && ( nc >= len ) ) {
      direction = astGetDirection( this, axis - 1 );
      if ( astOK ) {
         (void) sprintf( getattrib_buff, "%d", direction );
         result = getattrib_buff;
      }

/* Epoch. */
/* ------ */
   } else if ( !strcmp( attrib, "epoch" ) ) {
      epoch = astGetEpoch( this );
      if ( astOK ) {

/* Format the Epoch as decimal years. Use a Besselian epoch if it will
   be less than 1984.0, otherwise use a Julian epoch. */
         result = astFmtDecimalYr( ( epoch < palSlaEpj2d( 1984.0 ) ) ?
                                   palSlaEpb( epoch ) : palSlaEpj( epoch ), DBL_DIG );
      }

/* Top(axis). */
/* ---------- */
   } else if ( nc = 0,
               ( 1 == astSscanf( attrib, "top(%d)%n", &axis, &nc ) )
               && ( nc >= len ) ) {
      dval = astGetTop( this, axis -1 );
      if ( astOK ) {
         (void) sprintf( getattrib_buff, "%.*g", DBL_DIG, dval );
         result = getattrib_buff;
      }

/* Bottom(axis). */
/* ---------- */
   } else if ( nc = 0,
               ( 1 == astSscanf( attrib, "bottom(%d)%n", &axis, &nc ) )
               && ( nc >= len ) ) {
      dval = astGetBottom( this, axis -1 );
      if ( astOK ) {
         (void) sprintf( getattrib_buff, "%.*g", DBL_DIG, dval );
         result = getattrib_buff;
      }

/* Domain. */
/* ------- */
   } else if ( !strcmp( attrib, "domain" ) ) {
      result = astGetDomain( this );

/* Format(axis). */
/* ------------- */
   } else if ( nc = 0,
               ( 1 == astSscanf( attrib, "format(%d)%n", &axis, &nc ) )
               && ( nc >= len ) ) {
      result = astGetFormat( this, axis - 1 );

/* Label(axis). */
/* ------------ */
   } else if ( nc = 0,
               ( 1 == astSscanf( attrib, "label(%d)%n", &axis, &nc ) )
               && ( nc >= len ) ) {
      result = astGetLabel( this, axis - 1 );

/* MatchEnd. */
/* --------- */
   } else if ( !strcmp( attrib, "matchend" ) ) {
      match_end = astGetMatchEnd( this );
      if ( astOK ) {
         (void) sprintf( getattrib_buff, "%d", match_end );
         result = getattrib_buff;
      }

/* MaxAxes. */
/* -------- */
   } else if ( !strcmp( attrib, "maxaxes" ) ) {
      max_axes = astGetMaxAxes( this );
      if ( astOK ) {
         (void) sprintf( getattrib_buff, "%d", max_axes );
         result = getattrib_buff;
      }

/* MinAxes. */
/* -------- */
   } else if ( !strcmp( attrib, "minaxes" ) ) {
      min_axes = astGetMinAxes( this );
      if ( astOK ) {
         (void) sprintf( getattrib_buff, "%d", min_axes );
         result = getattrib_buff;
      }

/* Naxes. */
/* -----_ */
   } else if ( !strcmp( attrib, "naxes" ) ) {
      (void) sprintf( getattrib_buff, "%d", naxes );
      result = getattrib_buff;

/* Permute. */
/* -------- */
   } else if ( !strcmp( attrib, "permute" ) ) {
      permute = astGetPermute( this );
      if ( astOK ) {
         (void) sprintf( getattrib_buff, "%d", permute );
         result = getattrib_buff;
      }

/* PreserveAxes. */
/* ------------- */
   } else if ( !strcmp( attrib, "preserveaxes" ) ) {
      preserve_axes = astGetPreserveAxes( this );
      if ( astOK ) {
         (void) sprintf( getattrib_buff, "%d", preserve_axes );
         result = getattrib_buff;
      }

/* Symbol(axis). */
/* ------------- */
   } else if ( nc = 0,
               ( 1 == astSscanf( attrib, "symbol(%d)%n", &axis, &nc ) )
               && ( nc >= len ) ) {
      result = astGetSymbol( this, axis - 1 );

/* AlignSystem. */
/* ------------ */
/* Obtain the AlignSystem code and convert to a string. */
   } else if ( !strcmp( attrib, "alignsystem" ) ) {
      system = astGetAlignSystem( this );
      if ( astOK ) {
         result = astSystemString( this, system );

/* Report an error if the value was not recognised. */
         if ( !result ) {
            astError( AST__SCSIN,
                     "astGetAttrib(%s): Corrupt %s contains invalid "
                     "AlignSystem identification code (%d).", status,
                     astGetClass( this ), astGetClass( this ), (int) system );
         }
      }

/* System. */
/* ------- */
/* Obtain the System code and convert to a string. */
   } else if ( !strcmp( attrib, "system" ) ) {
      system = astGetSystem( this );
      if ( astOK ) {
         result = astSystemString( this, system );

/* Report an error if the value was not recognised. */
         if ( !result ) {
            astError( AST__SCSIN,
                     "astGetAttrib(%s): Corrupt %s contains invalid "
                     "System identification code (%d).", status,
                     astGetClass( this ), astGetClass( this ), (int) system );
         }
      }

/* Title. */
/* ------ */
   } else if ( !strcmp( attrib, "title" ) ) {
      result = astGetTitle( this );

/* Unit(axis). */
/* ----------- */
   } else if ( nc = 0,
               ( 1 == astSscanf( attrib, "unit(%d)%n", &axis, &nc ) )
               && ( nc >= len ) ) {
      result = astGetUnit( this, axis - 1 );

/* NormUnit(axis). */
/* --------------- */
   } else if ( nc = 0,
               ( 1 == astSscanf( attrib, "normunit(%d)%n", &axis, &nc ) )
               && ( nc >= len ) ) {
      result = astGetNormUnit( this, axis - 1 );

/* ObsLat. */
/* ------- */
   } else if ( !strcmp( attrib, "obslat" ) ) {
      dval = astGetObsLat( this );
      if ( astOK ) {

/* If not already created, create an FK5 J2000 SkyFrame which will be used
   for formatting and unformatting ObsLon and ObsLat values. */
         if( !skyframe ) {
            astBeginPM;
            skyframe = astSkyFrame("system=FK5,equinox=J2000,format(2)=dms.2", status );
            astEndPM;
         }

/* Display absolute value preceeded by "N" or "S" as appropriate. */
         if( dval < 0 ) {
            (void) sprintf( getattrib_buff, "S%s",  astFormat( skyframe, 1, -dval ) );
         } else {
            (void) sprintf( getattrib_buff, "N%s",  astFormat( skyframe, 1, dval ) );
         }
         result = getattrib_buff;
      }

/* ObsLon. */
/* ------- */
   } else if ( !strcmp( attrib, "obslon" ) ) {
      dval = astGetObsLon( this );
      if ( astOK ) {

/* Put into range +/- PI. */
         dval = palSlaDrange( dval );

/* If not already created, create an FK5 J2000 SkyFrame which will be used
   for formatting and unformatting ObsLon and ObsLat values. */
         if( !skyframe ) {
            astBeginPM;
            skyframe = astSkyFrame( "system=FK5,equinox=J2000,format(2)=dms.2", status );
            astEndPM;
         }

/* Display absolute value preceeded by "E" or "W" as appropriate. */
         if( dval < 0 ) {
            (void) sprintf( getattrib_buff, "W%s",  astFormat( skyframe, 1, -dval ) );
         } else {
            (void) sprintf( getattrib_buff, "E%s",  astFormat( skyframe, 1, dval ) );
         }
         result = getattrib_buff;

      }

/* ObsAlt. */
/* ------- */
   } else if ( !strcmp( attrib, "obsalt" ) ) {
      dval = astGetObsAlt( this );
      if ( astOK ) {
         (void) sprintf( getattrib_buff, "%.*g", DBL_DIG, dval );
         result = getattrib_buff;
      }

/* Dut1. */
/* ---- */
   } else if ( !strcmp( attrib, "dut1" ) ) {
      dval = astGetDut1( this );
      if ( astOK ) {
         (void) sprintf( getattrib_buff, "%.*g", DBL_DIG, dval );
         result = getattrib_buff;
      }

/* Other axis attributes. */
/* ---------------------- */
/* If the attribute was not identified above, but appears to refer to
   a Frame axis, then it may refer to an Axis object of a derived type
   (which has additional attributes not recognised here). */
   } else if ( !free_axis_attrib && ( nc = 0,
               ( 1 == astSscanf( attrib, "%*[^()]%n(%d)%n",
                                      &axis_nc, &axis, &nc ) )
               && ( nc >= len ) ) ) {

/* Validate the axis index and extract the attribute name. */
      (void) astValidateAxis( this, axis - 1, "astGet" );
      axis_attrib = astString( attrib, axis_nc );

/* Obtain a pointer to the Axis object. */
      ax = astGetAxis( this, axis - 1 );
      if( astOK ) {

/* Assume that we will be able to use the attribute name. */
         used = 1;

/* Temporarily switch off error reporting so that if the following attempt
   to access the axis attribute fails, we can try to interpret the
   attribute name as an attribute of the primary Frame containing the
   specified axis. Any errors reported in this context will simply be
   ignored, in particularly they are not deferred for later delivery. */
         oldrep = astReporting( 0 );

/* Use the Axis astGetAttrib method to obtain the result. */
         result = astGetAttrib( ax, axis_attrib );

/* If the above call failed with a status of AST__BADAT, indicating that
   the attribute name was not recognised, clear the status so that we can
   try to interpret the attribute name as an attribute of the primary Frame
   containing the specified axis. */
         if( astStatus == AST__BADAT ) {
            astClearStatus;

/* Find the primary Frame containing the specified axis. */
            astPrimaryFrame( this, axis - 1, &pfrm, &paxis );

/* Only attempt to use the primary Frame if it is not the same as "this"
   - otherwise we could end up in an infinite loop. */
            if( pfrm != this ) {

/* Modify the attribute name to refer to the axis numbering of the
   primary frame. */
               sprintf( pfrm_attrib, "%s(%d)", axis_attrib, paxis + 1 );

/* Attempt to use the Axis astGetAttrib method to obtain the result. */
               result = astGetAttrib( pfrm, pfrm_attrib );

/* If this failed, clear the status and indicate that we have not managed to
   use the attribute name. */
               if( !astOK ) {
                  astClearStatus;
                  used = 0;
               }

            } else {
               used =  0;
            }

/* If not found attempt to get the attribute value from the Axis, omitting
   the axis index. */
            if( ! used ) {
               result = astGetAttrib( pfrm, axis_attrib );
               if( !astOK ) {
                  astClearStatus;
               } else {
                  used = 1;
               }
            }

/* Annul the primary Frame pointer. */
            pfrm = astAnnul( pfrm );
         }

/* Re-instate the original error reporting state. */
         astReporting( oldrep );

/* If we could not use the attribute name, attempt to get the axis
   attribute again, this time retaining the error report. This is done
   to ensure the user gets an appropriate error message. */
         if( !used ) result = astGetAttrib( ax, axis_attrib );
      }

/* Annul the Axis pointer and free the memory holding the attribute
   name. */
      ax = astAnnul( ax );
      axis_attrib = astFree( axis_attrib );

/* Not recognised. */
/* --------------- */
/* If the attribute is still not recognised, and the Frame has only 1 axis,
   and the attribute name does not already include an axis specifier, try
   again after appending "(1)" to the end of the attribute name. */
   } else if( !has_axis && naxes == 1 ) {

/* Take a copy of the supplied name, allowing 3 extra characters for the
   axis specifier "(1)". */
      axis_attrib = astMalloc( len + 4 );
      if( axis_attrib ) memcpy( axis_attrib, attrib, len );

/* Indicate we should free the axis_attrib memory. */
      free_axis_attrib = 1;

/* Add in the axis specifier. */
      strcpy( axis_attrib + len, "(1)" );

/* Use the new attribute name instead of the supplied name. */
      old_attrib = attrib;
      attrib = axis_attrib;

/* Indicate the attribute name now has an axis specifier. */
      has_axis = 1;

/* Jump back to try interpreting the new attribute name. */
      goto L1;

/* Not recognised. */
/* --------------- */
/* If the attribute name is still not recognised, pass it on to the parent
   method for further interpretation. First re-instate the original attrib
   name string if it was changed above. */
   } else {
      if( free_axis_attrib ) {
         attrib = old_attrib;
         axis_attrib = astFree( axis_attrib );
      }
      result = (*parent_getattrib)( this_object, attrib, status );
   }

/* Return the result. */
   return result;
}

static AstAxis *GetAxis( AstFrame *this, int axis, int *status ) {
/*
*+
*  Name:
*     astGetAxis

*  Purpose:
*     Obtain a pointer to a specified Axis from a Frame.

*  Type:
*     Protected virtual function.

*  Synopsis:
*     #include "frame.h"
*     AstAxis *astGetAxis( AstFrame *this, int axis )

*  Class Membership:
*     Frame method.

*  Description:
*     This function returns a pointer to the Axis object associated
*     with one of the axes of a Frame. This object describes the
*     quantity which is represented along that axis.

*  Parameters:
*     this
*        Pointer to the Frame.
*     axis
*        The number of the axis (zero-based) for which an Axis pointer is
*        required.

*  Returned Value:
*     A pointer to the requested Axis object.

*  Notes:
*     - The reference count of the requested Axis object will be
*     incremented by one to reflect the additional pointer returned by
*     this function.
*     - A NULL pointer will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*-
*/

/* Local Variables: */
   AstAxis *result;              /* Pointer to Axis */

/* Check the global error status. */
   if ( !astOK ) return NULL;

/* Initialise. */
   result = NULL;

/* Validate and permute the axis index. */
   axis = astValidateAxis( this, axis, "astGetAxis" );

/* If OK, clone the required Axis pointer. */
   if ( astOK ) result = astClone( this->axis[ axis ] );

/* Return the result. */
   return result;
}

static const char *GetDefaultLabel( int axis, int *status ) {
/*
*  Name:
*     GetDefaultLabel

*  Purpose:
*     Return a pointer to a default axis Label string.

*  Type:
*     Private function.

*  Synopsis:
*     #include "frame.h"
*     const char *GetDefaultLabel( int axis, int *status )

*  Class Membership:
*     Frame member function

*  Description:
*     This function returns a pointer to a string holding a default axis
*     Label value.

*  Parameters:
*     axis
*        Zero based axis index.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     - Pointer to a static null-terminated string containing the attribute
*     value.

*/

/* Local Variables: */
   astDECLARE_GLOBALS            /* Declare the thread specific global data */

/* Get a pointer to the structure holding thread-specific global data. */
   astGET_GLOBALS(NULL);

/* Format the axis index, putting the string in a global buffer. */
   (void) sprintf( label_buff, "Axis %d", axis + 1 );

/* Return a pointer to the global buffer. */
   return label_buff;
}

static const char *GetDefaultSymbol( AstFrame *this, int axis, int *status ) {
/*
*  Name:
*     GetDefaultSymbol

*  Purpose:
*     Return a pointer to a default axis Symbol string.

*  Type:
*     Private function.

*  Synopsis:
*     #include "frame.h"
*     const char *GetDefaultSymbol( AstFrame *this, int axis, int *status )

*  Class Membership:
*     Frame member function

*  Description:
*     This function returns a pointer to a string holding a default axis
*     Symbol value.

*  Parameters:
*     this
*        Pointer to the Frame.
*     axis
*        Zero based axis index.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     - Pointer to a static null-terminated string containing the attribute
*     value.

*/

/* Local Variables: */
   astDECLARE_GLOBALS            /* Declare the thread specific global data */

/* Get a pointer to the structure holding thread-specific global data. */
   astGET_GLOBALS(this);

/* Note we use "sprintf" once to determine how many characters are
   produced by the "%d" format string and then limit the number of
   characters used from the Domain string in the second invocation of
   "sprintf" so that the total length of the default Symbol string
   does not exceed SYMBOL_BUFF_LEN characters. */
   (void) sprintf( symbol_buff, "%.*s%d",
                   SYMBOL_BUFF_LEN - sprintf( symbol_buff, "%d", axis + 1 ),
                   astTestDomain( this ) ? astGetDomain( this ) : "x",
                   axis + 1 );

/* Use the AddUnderscores function to replace any white space in the Symbol
   string with underscore characters. */
   AddUnderscores( symbol_buff, status );

/* Return a pointer to the global buffer. */
   return symbol_buff;
}

static const char *GetDefaultTitle( AstFrame *this, int *status ) {
/*
*  Name:
*     GetDefaultTitle

*  Purpose:
*     Return a pointer to a default Title string.

*  Type:
*     Private function.

*  Synopsis:
*     #include "frame.h"
*     const char *GetDefaultTitle( AstFrame *this, int *status )

*  Class Membership:
*     Frame member function

*  Description:
*     This function returns a pointer to a string holding a default Title value.

*  Parameters:
*     this
*        Pointer to the Frame.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     - Pointer to a static null-terminated string containing the attribute
*     value.

*/

/* Local Variables: */
   astDECLARE_GLOBALS            /* Declare the thread specific global data */

/* Get a pointer to the structure holding thread-specific global data. */
   astGET_GLOBALS(this);

/* Create the Title value and put it in the global buffer. */
   (void) sprintf( title_buff, "%d-d coordinate system", astGetNaxes( this ) );

/* Return a pointer to the global buffer. */
   return title_buff;
}

static int GetFrameFlags( AstFrame *this, int *status ){
/*
*+
*  Name:
*     astGetFrameFlags

*  Purpose:
*     Return the bit mask of flags associated with a Frame.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "frame.h"
*     int *astGetFrameFlags( astFrame *this )

*  Class Membership:
*     Frame virtual function.

*  Description:
*     This function returns a bit mask holding the current set of flags
*     associated with a Frame. See astSetFrameFlags for details of these
*     flags.

*  Parameters:
*     this
*        The Frame.

*  Returned Value:
*     The bit mask.

*  Notes:
*     - Zero is returned if this function is invoked with
*     the global error status set or if it should fail for any reason.
*-
*/

/* Check the global error status. */
   if ( !astOK ) return 0;

/* Return the result. */
   return this->flags;
}

static int GetIsLinear( AstMapping *this_mapping, int *status ){
/*
*  Name:
*     GetIsLinear

*  Purpose:
*     Return the value of the IsLinear attribute for a Frame.

*  Type:
*     Private function.

*  Synopsis:
*     #include "mapping.h"
*     void GetIsLinear( AstMapping *this, int *status )

*  Class Membership:
*     Frame member function (over-rides the protected astGetIsLinear
*     method inherited from the Mapping class).

*  Description:
*     This function returns the value of the IsLinear attribute for a
*     Frame, which is always one because a Frame is treated like a UnitMap.

*  Parameters:
*     this
*        Pointer to the Frame.
*     status
*        Pointer to the inherited status variable.
*/
   return 1;
}

static int GetIsSimple( AstMapping *this_mapping, int *status ){
/*
*  Name:
*     GetIsSimple

*  Purpose:
*     Return the value of the IsSimple attribute for a Frame.

*  Type:
*     Private function.

*  Synopsis:
*     #include "mapping.h"
*     void GetIsSimple( AstMapping *this, int *status )

*  Class Membership:
*     Frame member function (over-rides the protected astGetIsSimple
*     method inherited from the Mapping class).

*  Description:
*     This function returns the value of the IsSimple attribute for a
*     Frame, which is always zero because Frames are not immutable (unlike
*     non-Frame Mappings).

*  Parameters:
*     this
*        Pointer to the Frame.
*     status
*        Pointer to the inherited status variable.
*/
   return 0;
}

static int GetNaxes( AstFrame *this, int *status ) {
/*
*+
*  Name:
*     astGetNaxes

*  Purpose:
*     Determine how many axes a Frame has.

*  Type:
*     Protected virtual function.

*  Synopsis:
*     #include "frame.h"
*     int astGetNaxes( AstFrame *this )

*  Class Membership:
*     Frame method.

*  Description:
*     This function returns the number of axes for a Frame.

*  Parameters:
*     this
*        Pointer to the Frame.

*  Returned Value:
*     The number of Frame axes.

*  Notes:
*     - A value of zero will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*-
*/

/* Check the global error status. */
   if ( !astOK ) return 0;

/* Return the number of Frame axes. */
   return this->naxes;
}

static int GetNin( AstMapping *this_mapping, int *status ) {
/*
*  Name:
*     GetNin

*  Purpose:
*     Get the number of input coordinates for a Frame.

*  Type:
*     Private function.

*  Synopsis:
*     #include "frame.h"
*     int GetNin( AstMapping *this, int *status )

*  Class Membership:
*     Frame member function (over-rides the astGetNin method inherited
*     from the Mapping class).

*  Description:
*     This function returns the number of input coordinate values
*     required per point by a Frame, when used as a Mapping (i.e. the
*     number of dimensions of the space in which input points reside).

*  Parameters:
*     this
*        Pointer to the Frame.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     Number of coordinate values required.

*  Notes:
*     - A value of zero will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*/

/* Local Variables: */
   AstFrame *this;               /* Pointer to Frame structure */
   int result;                   /* Result value to return */

/* Initialise. */
   result = 0;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Obtain a pointer to the Frame structure. */
   this = (AstFrame *) this_mapping;

/* Return the number of Frame axes. */
   result = astGetNaxes( this );

/* Return the result. */
   return result;
}

static int GetNout( AstMapping *this_mapping, int *status ) {
/*
*  Name:
*     GetNout

*  Purpose:
*     Get the number of output coordinates for a Frame.

*  Type:
*     Private function.

*  Synopsis:
*     #include "frame.h"
*     int GetNout( AstMapping *this, int *status )

*  Class Membership:
*     Frame member function (over-rides the astGetNout method
*     inherited from the Mapping class).

*  Description:
*     This function returns the number of output coordinate values
*     generated per point by a Frame, when used as a Mapping (i.e. the
*     number of dimensions of the space in which output points
*     reside).

*  Parameters:
*     this
*        Pointer to the Frame.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     Number of coordinate values generated.

*  Notes:
*     - A value of zero will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*/

/* Local Variables: */
   AstFrame *this;               /* Pointer to Frame structure */
   int result;                   /* Result value to return */

/* Initialise. */
   result = 0;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Obtain a pointer to the Frame structure. */
   this = (AstFrame *) this_mapping;

/* Return the number of Frame axes. */
   result = astGetNaxes( this );

/* Return the result. */
   return result;
}

static const int *GetPerm( AstFrame *this, int *status ) {
/*
*+
*  Name:
*     astGetPerm

*  Purpose:
*     Access the axis permutation array for a Frame.

*  Type:
*     Protected virtual function.

*  Synopsis:
*     #include "frame.h"
*     const int *astGetPerm( AstFrame *this )

*  Class Membership:
*     Frame method.

*  Description:
*     This function returns a pointer to the axis permutation array
*     for a Frame. This array constitutes a lookup-table that converts
*     between an axis number supplied externally and the corresponding
*     index in the Frame's internal axis arrays.

*  Parameters:
*     this
*        Pointer to the Frame.

*  Returned Value:
*     Pointer to the Frame's axis permutation array (a constant array
*     of int).  Each element of this contains the (zero-based)
*     internal axis index to be used in place of the external index
*     which is used to address the permutation array. If the Frame has
*     zero axes, this pointer will be NULL.

*  Notes:
*     - This protected method is provided to assist class
*     implementations which need to implement axis-dependent
*     extensions to Frame methods, and which therefore need to know
*     how a Frames's external axis index is converted for internal
*     use.
*     - The pointer returned by this function gives direct access to
*     data internal to the Frame object. It remains valid only so long
*     as the Frame exists. The permutation array contents may be
*     modified by other functions which operate on the Frame and this
*     may render the returned pointer invalid.
*     - A NULL pointer will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*-
*/

/* Check the global error status. */
   if ( !astOK ) return NULL;

/* Return a pointer to the axis permutation array. */
   return this->perm;
}

void astInitFrameVtab_(  AstFrameVtab *vtab, const char *name, int *status ) {
/*
*+
*  Name:
*     astInitFrameVtab

*  Purpose:
*     Initialise a virtual function table for a Frame.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "frame.h"
*     void astInitFrameVtab( AstFrameVtab *vtab, const char *name )

*  Class Membership:
*     Frame vtab initialiser.

*  Description:
*     This function initialises the component of a virtual function
*     table which is used by the Frame class.

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
   AstMappingVtab *mapping;      /* Pointer to Mapping component of Vtab */

/* Check the local error status. */
   if ( !astOK ) return;

/* Get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(NULL);

/* Initialize the component of the virtual function table used by the
   parent class. */
   astInitMappingVtab( (AstMappingVtab *) vtab, name );

/* Store a unique "magic" value in the virtual function table. This
   will be used (by astIsAFrame ) to determine if an object belongs
   to this class.  We can conveniently use the address of the (static)
   class_check variable to generate this unique value. */
   vtab->id.check = &class_check;
   vtab->id.parent = &(((AstMappingVtab *) vtab)->id);

/* Initialise member function pointers. */
/* ------------------------------------ */
/* Store pointers to the member functions (implemented here) that provide
   virtual methods for this class. */
   vtab->Abbrev = Abbrev;
   vtab->CheckPerm = CheckPerm;
   vtab->ClearDigits = ClearDigits;
   vtab->ClearDirection = ClearDirection;
   vtab->ClearDomain = ClearDomain;
   vtab->ClearFormat = ClearFormat;
   vtab->ClearLabel = ClearLabel;
   vtab->ClearMatchEnd = ClearMatchEnd;
   vtab->ClearMaxAxes = ClearMaxAxes;
   vtab->ClearMinAxes = ClearMinAxes;
   vtab->ClearPermute = ClearPermute;
   vtab->ClearPreserveAxes = ClearPreserveAxes;
   vtab->ClearSymbol = ClearSymbol;
   vtab->ClearTitle = ClearTitle;
   vtab->ClearUnit = ClearUnit;
   vtab->Convert = Convert;
   vtab->ConvertX = ConvertX;
   vtab->Angle = Angle;
   vtab->Distance = Distance;
   vtab->Fields = Fields;
   vtab->FindFrame = FindFrame;
   vtab->MatchAxes = MatchAxes;
   vtab->MatchAxesX = MatchAxesX;
   vtab->Format = Format;
   vtab->Gap = Gap;
   vtab->GetAxis = GetAxis;
   vtab->GetDigits = GetDigits;
   vtab->GetDirection = GetDirection;
   vtab->GetDomain = GetDomain;
   vtab->GetFormat = GetFormat;
   vtab->GetLabel = GetLabel;
   vtab->GetMatchEnd = GetMatchEnd;
   vtab->GetMaxAxes = GetMaxAxes;
   vtab->GetMinAxes = GetMinAxes;
   vtab->GetNaxes = GetNaxes;
   vtab->GetPerm = GetPerm;
   vtab->GetPermute = GetPermute;
   vtab->GetPreserveAxes = GetPreserveAxes;
   vtab->GetSymbol = GetSymbol;
   vtab->GetTitle = GetTitle;
   vtab->GetUnit = GetUnit;
   vtab->GetNormUnit = GetNormUnit;
   vtab->Intersect = Intersect;
   vtab->IsUnitFrame = IsUnitFrame;
   vtab->Match = Match;
   vtab->Norm = Norm;
   vtab->NormBox = NormBox;
   vtab->AxDistance = AxDistance;
   vtab->AxOffset = AxOffset;
   vtab->AxIn = AxIn;
   vtab->AxAngle = AxAngle;
   vtab->FrameGrid = FrameGrid;
   vtab->Offset = Offset;
   vtab->Offset2 = Offset2;
   vtab->Resolve = Resolve;
   vtab->ResolvePoints = ResolvePoints;
   vtab->LineDef = LineDef;
   vtab->LineContains = LineContains;
   vtab->LineCrossing = LineCrossing;
   vtab->LineOffset = LineOffset;
   vtab->Overlay = Overlay;
   vtab->PermAxes = PermAxes;
   vtab->PickAxes = PickAxes;
   vtab->PrimaryFrame = PrimaryFrame;
   vtab->SetAxis = SetAxis;
   vtab->SetDigits = SetDigits;
   vtab->SetDirection = SetDirection;
   vtab->SetDomain = SetDomain;
   vtab->SetFormat = SetFormat;
   vtab->SetLabel = SetLabel;
   vtab->SetMatchEnd = SetMatchEnd;
   vtab->SetMaxAxes = SetMaxAxes;
   vtab->SetMinAxes = SetMinAxes;
   vtab->SetPermute = SetPermute;
   vtab->SetPreserveAxes = SetPreserveAxes;
   vtab->SetSymbol = SetSymbol;
   vtab->SetTitle = SetTitle;
   vtab->SetUnit = SetUnit;
   vtab->SubFrame = SubFrame;
   vtab->TestDigits = TestDigits;
   vtab->TestDirection = TestDirection;
   vtab->TestDomain = TestDomain;
   vtab->TestFormat = TestFormat;
   vtab->TestLabel = TestLabel;
   vtab->TestMatchEnd = TestMatchEnd;
   vtab->TestMaxAxes = TestMaxAxes;
   vtab->TestMinAxes = TestMinAxes;
   vtab->TestPermute = TestPermute;
   vtab->TestPreserveAxes = TestPreserveAxes;
   vtab->TestSymbol = TestSymbol;
   vtab->TestTitle = TestTitle;
   vtab->TestUnit = TestUnit;
   vtab->Unformat = Unformat;
   vtab->ValidateAxis = ValidateAxis;
   vtab->ValidateAxisSelection = ValidateAxisSelection;
   vtab->ValidateSystem = ValidateSystem;
   vtab->SystemString = SystemString;
   vtab->SystemCode = SystemCode;

   vtab->GetFrameFlags = GetFrameFlags;
   vtab->SetFrameFlags = SetFrameFlags;

   vtab->TestActiveUnit = TestActiveUnit;
   vtab->GetActiveUnit = GetActiveUnit;
   vtab->SetActiveUnit = SetActiveUnit;

   vtab->ClearSystem = ClearSystem;
   vtab->GetSystem = GetSystem;
   vtab->SetSystem = SetSystem;
   vtab->TestSystem = TestSystem;

   vtab->ClearAlignSystem = ClearAlignSystem;
   vtab->GetAlignSystem = GetAlignSystem;
   vtab->SetAlignSystem = SetAlignSystem;
   vtab->TestAlignSystem = TestAlignSystem;

   vtab->ClearTop = ClearTop;
   vtab->GetTop = GetTop;
   vtab->SetTop = SetTop;
   vtab->TestTop = TestTop;

   vtab->ClearBottom = ClearBottom;
   vtab->GetBottom = GetBottom;
   vtab->SetBottom = SetBottom;
   vtab->TestBottom = TestBottom;

   vtab->ClearEpoch = ClearEpoch;
   vtab->GetEpoch = GetEpoch;
   vtab->SetEpoch = SetEpoch;
   vtab->TestEpoch = TestEpoch;

   vtab->ClearObsLat = ClearObsLat;
   vtab->TestObsLat = TestObsLat;
   vtab->GetObsLat = GetObsLat;
   vtab->SetObsLat = SetObsLat;

   vtab->ClearObsLon = ClearObsLon;
   vtab->TestObsLon = TestObsLon;
   vtab->GetObsLon = GetObsLon;
   vtab->SetObsLon = SetObsLon;

   vtab->ClearObsAlt = ClearObsAlt;
   vtab->TestObsAlt = TestObsAlt;
   vtab->GetObsAlt = GetObsAlt;
   vtab->SetObsAlt = SetObsAlt;

   vtab->ClearDut1 = ClearDut1;
   vtab->GetDut1 = GetDut1;
   vtab->SetDut1 = SetDut1;
   vtab->TestDut1 = TestDut1;

/* Save the inherited pointers to methods that will be extended, and
   replace them with pointers to the new member functions. */
   object = (AstObjectVtab *) vtab;

   parent_clearattrib = object->ClearAttrib;
   object->ClearAttrib = ClearAttrib;
   parent_getattrib = object->GetAttrib;
   object->GetAttrib = GetAttrib;
   parent_setattrib = object->SetAttrib;
   object->SetAttrib = SetAttrib;
   parent_testattrib = object->TestAttrib;
   object->TestAttrib = TestAttrib;

   parent_cleanattribs = object->CleanAttribs;
   object->CleanAttribs = CleanAttribs;

#if defined(THREAD_SAFE)
   parent_managelock = object->ManageLock;
   object->ManageLock = ManageLock;
#endif

/* Store replacement pointers for methods which will be over-ridden by
   new member functions implemented here. */
   mapping = (AstMappingVtab *) vtab;

   object->Equal = Equal;
   mapping->GetIsLinear = GetIsLinear;
   mapping->GetIsSimple = GetIsSimple;
   mapping->GetNin = GetNin;
   mapping->GetNout = GetNout;
   mapping->ReportPoints = ReportPoints;
   mapping->Transform = Transform;
   mapping->MapSplit = MapSplit;

/* Declare the copy constructor, destructor and class dump
   function. */
   astSetCopy( vtab, Copy );
   astSetDelete( vtab, Delete );
   astSetDump( vtab, Dump, "Frame", "Coordinate system description" );

/* If we have just initialised the vtab for the current class, indicate
   that the vtab is now initialised, and store a pointer to the class
   identifier in the base "object" level of the vtab. */
   if( vtab == &class_vtab ) {
      class_init = 1;
      astSetVtabClassIdentifier( vtab, &(vtab->id) );
   }
}

static void Intersect( AstFrame *this, const double a1[2],
                       const double a2[2], const double b1[2],
                       const double b2[2], double cross[2],
                       int *status ) {
/*
*++
*  Name:
c     astIntersect
f     AST_INTERSECT

*  Purpose:
*     Find the point of intersection between two geodesic curves.

*  Type:
*     Public virtual function.

*  Synopsis:
c     #include "frame.h"
c     void astIntersect( AstFrame *this, const double a1[2],
c                        const double a2[2], const double b1[2],
c                        const double b2[2], double cross[2] )
f     CALL AST_INTERSECT( THIS, A1, A2, B1, B2, CROSS, STATUS )

*  Class Membership:
*     Frame method.

*  Description:
c     This function
f     This routine
*     finds the coordinate values at the point of intersection between
*     two geodesic curves. Each curve is specified by two points on
*     the curve.  It can only be used with 2-dimensional Frames.
*
*     For example, in a basic Frame, it will find the point of
*     intersection between two straight lines. But for a SkyFrame it
*     will find an intersection of two great circles.

*  Parameters:
c     this
f     THIS = INTEGER (Given)
*        Pointer to the Frame.
c     a1
f     A1( 2 ) = DOUBLE PRECISION (Given)
c        An array of double, with one element for each Frame axis
f        An array with one element for each Frame axis
*        (Naxes attribute). This should contain the coordinates of the
*        first point on the first geodesic curve.
c     a2
f     A2( 2 ) = DOUBLE PRECISION (Given)
c        An array of double, with one element for each Frame axis
f        An array with one element for each Frame axis
*        (Naxes attribute). This should contain the coordinates of a
*        second point on the first geodesic curve. It should not be
*        co-incident with the first point.
c     b1
f     B1( 2 ) = DOUBLE PRECISION (Given)
c        An array of double, with one element for each Frame axis
f        An array with one element for each Frame axis
*        (Naxes attribute). This should contain the coordinates of the
*        first point on the second geodesic curve.
c     b2
f     B2( 2 ) = DOUBLE PRECISION (Given)
c        An array of double, with one element for each Frame axis
f        An array with one element for each Frame axis
*        (Naxes attribute). This should contain the coordinates of a
*        second point on the second geodesic curve. It should not be
*        co-incident with the first point.
c     cross
f     CROSS( 2 ) = DOUBLE PRECISION (Returned)
c        An array of double, with one element for each Frame axis
f        An array with one element for each Frame axis
*        in which the coordinates of the required intersection will
*        be returned.
f     STATUS = INTEGER (Given and Returned)
f        The global status.

*  Notes:
*     - For SkyFrames each curve will be a great circle, and in general
*     each pair of curves will intersect at two diametrically opposite
*     points on the sky. The returned position is the one which is
*     closest to point
c     "a1".
f     A1.
*     - This function will return "bad" coordinate values (AST__BAD)
*     if any of the input coordinates has this value, or if the two
*     points defining either geodesic are co-incident, or if the two
*     curves do not intersect.
c     - The geodesic curve used by this function is the path of
f     - The geodesic curve used by this routine is the path of
*     shortest distance between two points, as defined by the
c     astDistance function.
f     AST_DISTANCE function.
*     - An error will be reported if the Frame is not 2-dimensional.
*--
*/

/* Local Variables: */
   double ca;                    /* Y axis intercept of line a */
   double cb;                    /* Y axis intercept of line b */
   double dxa;                   /* X range spanned by line a */
   double dxb;                   /* X range spanned by line b */
   double ma;                    /* Gradient of line a */
   double mb;                    /* Gradient of line b */
   int naxes;                    /* Number of Frame axes */

/* Check the global error status. */
   if ( !astOK ) return;

/* Initialize bad values. */
   cross[ 0 ] = AST__BAD;
   cross[ 1 ] = AST__BAD;

/* Determine the number of Frame axes. */
   naxes = astGetNaxes( this );

/* Report an error if the Frame is not 2 dimensional. */
   if( naxes != 2 && astOK ) {
      astError( AST__NAXIN, "astIntersect(%s): Invalid number of Frame axes (%d)."
                " astIntersect can only be used with 2 dimensonal Frames.", status,
                astGetClass( this ), naxes );
   }

/* Check that all supplied values are OK. */
   if ( ( a1[ 0 ] != AST__BAD ) && ( a1[ 1 ] != AST__BAD ) &&
        ( a2[ 0 ] != AST__BAD ) && ( a2[ 1 ] != AST__BAD ) &&
        ( b1[ 0 ] != AST__BAD ) && ( b1[ 1 ] != AST__BAD ) &&
        ( b2[ 0 ] != AST__BAD ) && ( b2[ 1 ] != AST__BAD ) ) {

/* Find the x increments spanned by the two lines. */

/* Check the first line is not vertical. */
      dxa = a2[ 0 ] - a1[ 0 ];
      dxb = b2[ 0 ] - b1[ 0 ];
      if( dxa != 0.0 ) {

/* Find the gradient and Y axis intercept of the first line. */
         ma = ( a2[ 1 ] - a1[ 1 ] )/dxa;
         ca = a1[ 1 ] - a1[ 0 ]*ma;

/* Check the second line is not vertical. */
         if( dxb != 0.0 ) {

/* Find the gradient and Y axis intercept of the second line. */
            mb = ( b2[ 1 ] - b1[ 1 ] )/dxb;
            cb = b1[ 1 ] - b1[ 0 ]*mb;

/* Check the lines are not parallel. */
            if( ma != mb ) {

/* Find the intersection of the two lines. */
               cross[ 0 ] = ( cb -ca )/( ma - mb );
               cross[ 1 ] = ( ( ma + mb )*cross[ 0 ] + ca + cb )/2;
            }

/* If the second line is vertical but the first is not. */
         } else if( b1[ 1 ] != b2[ 1 ] ){
            cross[ 0 ] = b1[ 0 ];
            cross[ 1 ] = ma*b1[ 0 ] + ca;
         }

/* First line is vertical but second is not. */
      } else if( dxb != 0.0 && a1[ 1 ] != a2[ 1 ] ){

/* Find the gradient and Y axis intercept of the second line. */
         mb = ( b2[ 1 ] - b1[ 1 ] )/dxb;
         cb = b1[ 1 ] - b1[ 0 ]*mb;

/* Find the intercection. */
         cross[ 0 ] = a1[ 0 ];
         cross[ 1 ] = mb*a1[ 0 ] + cb;
      }
   }
}

static int IsUnitFrame( AstFrame *this, int *status ){
/*
*+
*  Name:
*     astIsUnitFrame

*  Purpose:
*     Is this Frame equivalent to a UnitMap?

*  Type:
*     Protected virtual function.

*  Synopsis:
*     #include "frame.h"
*     int astIsUnitFrame( AstFrame *this )

*  Class Membership:
*     Frame method.

*  Description:
*     This function returns a flag indicating if the supplied Frame is
*     equivalent to a UnitMap when treated as a Mapping (note, the Frame
*     class inherits from Mapping and therefore every Frame is also a Mapping).

*  Parameters:
*     this
*        Pointer to the Frame.

*  Returned Value:
*     A non-zero value is returned if the supplied Frame is equivalent to
*     a UnitMap when treated as a Mapping.

*-
*/

/* Check the local error status. */
   if( !astOK ) return 0;

/* The base Frame class is always equivalent to a UnitMap. */
   return 1;
}

static int LineContains( AstFrame *this, AstLineDef *l, int def, double *point, int *status ) {
/*
*+
*  Name:
*     astLineContains

*  Purpose:
*     Determine if a line contains a point.

*  Type:
*     Protected virtual function.

*  Synopsis:
*     #include "frame.h"
*     int astLineContains( AstFrame *this, AstLineDef *l, int def, double *point )

*  Class Membership:
*     Frame method.

*  Description:
*     This function determines if the supplied point is on the supplied
*     line within the supplied Frame. The start point of the line is
*     considered to be within the line, but the end point is not. The tests
*     are that the point of closest approach of the line to the point should
*     be between the start and end, and that the distance from the point to
*     the point of closest aproach should be less than 1.0E-7 of the length
*     of the line.

*  Parameters:
*     this
*        Pointer to the Frame.
*     l
*        Pointer to the structure defining the line.
*     def
*        Should be set non-zero if the "point" array was created by a
*        call to astLineCrossing (in which case it may contain extra
*        information following the axis values),and zero otherwise.
*     point
*        Point to an array containing the axis values of the point to be
*        tested, possibly followed by extra cached information (see "def").

*  Returned Value:
*     A non-zero value is returned if the line contains the point.

*  Notes:
*     - The pointer supplied for "l" should have been created using the
*     astLineDef method. These structures contained cached information about
*     the lines which improve the efficiency of this method when many
*     repeated calls are made. An error will be reported if the structure
*     does not refer to the Frame specified by "this".
*     - Zero will be returned if this function is invoked with the global
*     error status set, or if it should fail for any reason.
*-
*/

/* Local Variables: */
   int result;
   double dx, dy, p;

/* Initialise. */
   result = 0;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Check that the line refers to the supplied Frame. */
   if( l->frame != this ) {
      astError( AST__INTER, "astLineContains(%s): The supplied line does "
                "not relate to the supplied %s (AST internal programming "
                "error).", status, astGetClass( this ), astGetClass( this ) );

/* If the point is good, find the offsets from the start of the line. */
   } else if( point[ 0 ] != AST__BAD && point[ 1 ] != AST__BAD ) {
      dx = point[ 0 ] - l->start[ 0 ];
      dy = point[ 1 ] - l->start[ 1 ];

/* Check the nearest point on the line is between the start and end.
   Exclude the end point. */
      p = dx*l->dir[ 0 ] + dy*l->dir[ 1 ];
      if( p >= 0.0 && p < l->length ) {

/* Check the distance from the point to the nearest point on the line is not
   further than 1.0E-7 of the length of the line. */
         if( fabs( dx*l->q[ 0 ] + dy*l->q[ 1 ] ) <= 1.0E-7*l->length ) {
            result = 1;
         }
      }
   }

/* Return zero if an error occurred. */
   if( !astOK ) result = 0;

/* Return a pointer to the output structure. */
   return result;
}

static int LineCrossing( AstFrame *this, AstLineDef *l1, AstLineDef *l2,
                         double **cross, int *status ) {
/*
*+
*  Name:
*     astLineCrossing

*  Purpose:
*     Determine if two lines cross.

*  Type:
*     Protected virtual function.

*  Synopsis:
*     #include "frame.h"
*     int astLineCrossing( AstFrame *this, AstLineDef *l1, AstLineDef *l2,
*                          double **cross )

*  Class Membership:
*     Frame method.

*  Description:
*     This function determines if the two suplied line segments cross,
*     and if so returns the axis values at the point where they cross.
*     A flag is also returned indicating if the crossing point occurs
*     within the length of both line segments, or outside one or both of
*     the line segments.

*  Parameters:
*     this
*        Pointer to the Frame.
*     l1
*        Pointer to the structure defining the first line.
*     l2
*        Pointer to the structure defining the second line.
*     cross
*        Pointer to a location at which to put a pointer to a dynamically
*        alocated array containing the axis values at the crossing. If
*        NULL is supplied no such array is returned. Otherwise, the returned
*        array should be freed using astFree when no longer needed. If the
*        lines are parallel (i.e. do not cross) then AST__BAD is returned for
*        all axis values. Note usable axis values are returned even if the
*        lines cross outside the segment defined by the start and end points
*        of the lines. The order of axes in the returned array will take
*        account of the current axis permutation array if appropriate. Note,
*        sub-classes such as SkyFrame may append extra values to the end
*        of the basic frame axis values. A NULL pointer is returned if an
*        error occurs.

*  Returned Value:
*     A non-zero value is returned if the lines cross at a point which is
*     within the [start,end) segment of the lines that are flagged as
*     finite (if a line is marked as infinite any crossing is assumed to
*     be within the bounds of the line). If the crossing point is outside
*     this segment on either (inifinte) line, or if the lines are parallel,
*     zero is returned. Note, the start point is considered to be inside
*     the length of the segment, but the end point is outside.

*  Notes:
*     - The pointers supplied for "l1" and "l2" should have been created
*     using the astLineDef method. These structures contained cached
*     information about the lines which improve the efficiency of this method
*     when many repeated calls are made. An error will be reported if
*     either structure does not refer to the Frame specified by "this".
*     - Zero will be returned if this function is invoked with the global
*     error status set, or if it should fail for any reason.
*-
*/

/* Local Variables: */
   double *crossing;          /* Returned array */
   double den;                /* Denominator */
   double dx;                 /* Offset in start X values */
   double dy;                 /* Offset in start Y values */
   double t1;                 /* Distance from start of line 1 to crossing */
   double t2;                 /* Distance from start of line 2 to crossing */
   int result;                /* Returned value */

/* Check the global error status. */
   if ( !astOK ) return 0;

/* Initialise. */
   result = 0;
   crossing = astMalloc( sizeof(double)*2 );

/* Check that both lines refer to the supplied Frame. */
   if( l1->frame != this ) {
      astError( AST__INTER, "astLineCrossing(%s): First supplied line does "
                "not relate to the supplied %s (AST internal programming "
                "error).", status, astGetClass( this ), astGetClass( this ) );

   } else if( l2->frame != this ) {
      astError( AST__INTER, "astLineCrossing(%s): Second supplied line does "
                "not relate to the supplied %s (AST internal programming "
                "error).", status, astGetClass( this ), astGetClass( this ) );

   } else if( crossing ){

/* Each of the lines can be represented as "p = start + t.v" where start is
   the start position, v is the unit vector pointing from start to end,
   and t is the scalar distance from the start position. So to find the
   intersection put "start1 + t1.v1 = start2 + t2.v2" and solve for t1
   and t2. */
      den = (l1->dir[ 0 ])*(l2->dir[ 1 ]) - (l2->dir[ 0 ])*(l1->dir[ 1 ]);
      if( den != 0.0 ) {
         dx = l2->start[ 0 ] - l1->start[ 0 ];
         dy = l2->start[ 1 ] - l1->start[ 1 ];
         t1 = ( l2->dir[ 1 ]*dx - l2->dir[ 0 ]*dy )/den;
         t2 = ( l1->dir[ 1 ]*dx - l1->dir[ 0 ]*dy )/den;

/* Store the crossing point, using the smaller t value to redue error. */
         if( fabs( t1 ) < fabs( t2 ) ) {
            crossing[ 0 ] = l1->start[ 0 ] + t1*l1->dir[ 0 ];
            crossing[ 1 ] = l1->start[ 1 ] + t1*l1->dir[ 1 ];
         } else {
            crossing[ 0 ] = l2->start[ 0 ] + t2*l2->dir[ 0 ];
            crossing[ 1 ] = l2->start[ 1 ] + t2*l2->dir[ 1 ];
         }

/* See if the intersection is within the length of both lines (excluding
   the end points). If a line is flagged as infinite, set the "t" parameter
   to zero to make it look like the crossing is within the line. */
         if( l1->infinite ) t1 = 0.0;
         if( l2->infinite ) t2 = 0.0;

         if( t1 >= 0.0 && t1 < l1->length &&
             t2 >= 0.0 && t2 < l2->length ) result = 1;

      } else {
         crossing[ 0 ] = AST__BAD;
         crossing[ 1 ] = AST__BAD;
      }
   }

/* Return zero if an error occurred. */
   if( !astOK ) {
      crossing = astFree( crossing );
      result = 0;
   }

/* Return the crossing pointer. */
   if( cross ) {
      *cross = crossing;
   } else if( crossing ){
      crossing = astFree( crossing );
   }

/* Return a pointer to the output structure. */
   return result;
}

static AstLineDef *LineDef( AstFrame *this, const double start[2],
                            const double end[2], int *status ) {
/*
*+
*  Name:
*     astLineDef

*  Purpose:
*     Creates a structure describing a line segment in a 2D Frame.

*  Type:
*     Protected virtual function.

*  Synopsis:
*     #include "frame.h"
*     AstLineDef *astLineDef( AstFrame *this, const double start[2],
*                             const double end[2] )

*  Class Membership:
*     Frame method.

*  Description:
*     This function creates a structure containing information describing a
*     given line segment within the supplied 2D Frame. This may include
*     information which allows other methods such as astLineCrossing to
*     function more efficiently. Thus the returned structure acts as a
*     cache to store intermediate values used by these other methods.

*  Parameters:
*     this
*        Pointer to the Frame. Must have 2 axes.
*     start
*        An array of 2 doubles marking the start of the line segment.
*     end
*        An array of 2 doubles marking the end of the line segment.

*  Returned Value:
*     Pointer to the memory structure containing the description of the
*     line. This structure should be freed using astFree when no longer
*     needed. A NULL pointer is returned (without error) if any of the
*     supplied axis values are AST__BAD.

*  Notes:
*     - A null pointer will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*-
*/

/* Local Variables: */
   AstLineDef *result;           /* Pointer to output structure */

/* Initialise. */
   result = NULL;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Check the Frame has 2 axes. */
   if( astGetNaxes( this ) != 2 ) {
      astError( AST__INTER, "astLineDef(%s): The supplied %s is not 2 "
                "dimensional (internal AST proramming error).", status,
                 astGetClass( this ), astGetClass( this ) );
   }

/* Check the axis values are good */
   if( start[ 0 ] != AST__BAD && start[ 1 ] != AST__BAD &&
       end[ 0 ] != AST__BAD && end[ 1 ] != AST__BAD ) {

/* Allocate memory for the returned structure. */
      result = astMalloc( sizeof( AstLineDef ) );
      if( result ) {

/* Store the supplied axis values in the returned structure. */
         result->start[ 0 ] = start[ 0 ];
         result->start[ 1 ] = start[ 1 ];
         result->end[ 0 ] = end[ 0 ];
         result->end[ 1 ] = end[ 1 ];

/* Store the length of the line segment. */
         result->length = astDistance( this, start, end );

/* Store a unit vector pointing from the start to the end. */
         if( result->length > 0.0 ) {
            result->dir[ 0 ] = ( end[ 0 ] - start[ 0 ] )/result->length;
            result->dir[ 1 ] = ( end[ 1 ] - start[ 1 ] )/result->length;
         } else {
            result->dir[ 0 ] = 1.0;
            result->dir[ 1 ] = 0.0;
         }

/* Store a unit vector perpendicular to the line, such that the vector
   points to the left, as vewied from the observer, when moving from the
   start to the end of the line. */
         result->q[ 0 ] = -result->dir[ 1 ];
         result->q[ 1 ] = result->dir[ 0 ];

/* Store a pointer to the defining Frame. */
         result->frame = this;

/* Indicate that the line is considered to be terminated at the start and
   end points. */
         result->infinite = 0;
      }
   }

/* Free the returned pointer if an error occurred. */
   if( !astOK ) result = astFree( result );

/* Return a pointer to the output structure. */
   return result;
}

static void LineOffset( AstFrame *this, AstLineDef *line, double par,
                        double prp, double point[2], int *status ){
/*
*+
*  Name:
*     astLineOffset

*  Purpose:
*     Find a position close to a line.

*  Type:
*     Protected virtual function.

*  Synopsis:
*     #include "frame.h"
*     void LineOffset( AstFrame *this, AstLineDef *line, double par,
*                      double prp, double point[2] )

*  Class Membership:
*     Frame method.

*  Description:
*     This function returns a position formed by moving a given distance along
*     the supplied line, and then a given distance away from the supplied line.

*  Parameters:
*     this
*        Pointer to the Frame.
*     line
*        Pointer to the structure defining the line.
*     par
*        The distance to move along the line from the start towards the end.
*     prp
*        The distance to move at right angles to the line. Positive
*        values result in movement to the left of the line, as seen from
*        the observer, when moving from start towards the end.

*  Notes:
*     - The pointer supplied for "line" should have been created using the
*     astLineDef method. This structure contains cached information about the
*     line which improves the efficiency of this method when many repeated
*     calls are made. An error will be reported if the structure does not
*     refer to the Frame specified by "this".
*-
*/

/* Check the global error status. */
   if ( !astOK ) return;

/* Check that the line refers to the supplied Frame. */
   if( line->frame != this ) {
      astError( AST__INTER, "astLineOffset(%s): The supplied line does "
                "not relate to the supplied %s (AST internal programming "
                "error).", status, astGetClass( this ), astGetClass( this ) );

/* This implementation uses simple flat geometry. */
   } else {
      point[ 0 ] = line->start[ 0 ] + par*line->dir[ 0 ] + prp*line->q[ 0 ];
      point[ 1 ] = line->start[ 1 ] + par*line->dir[ 1 ] + prp*line->q[ 1 ];
   }
}

#if defined(THREAD_SAFE)
static int ManageLock( AstObject *this_object, int mode, int extra,
                       AstObject **fail, int *status ) {
/*
*  Name:
*     ManageLock

*  Purpose:
*     Manage the thread lock on an Object.

*  Type:
*     Private function.

*  Synopsis:
*     #include "object.h"
*     AstObject *ManageLock( AstObject *this, int mode, int extra,
*                            AstObject **fail, int *status )

*  Class Membership:
*     CmpMap member function (over-rides the astManageLock protected
*     method inherited from the parent class).

*  Description:
*     This function manages the thread lock on the supplied Object. The
*     lock can be locked, unlocked or checked by this function as
*     deteremined by parameter "mode". See astLock for details of the way
*     these locks are used.

*  Parameters:
*     this
*        Pointer to the Object.
*     mode
*        An integer flag indicating what the function should do:
*
*        AST__LOCK: Lock the Object for exclusive use by the calling
*        thread. The "extra" value indicates what should be done if the
*        Object is already locked (wait or report an error - see astLock).
*
*        AST__UNLOCK: Unlock the Object for use by other threads.
*
*        AST__CHECKLOCK: Check that the object is locked for use by the
*        calling thread (report an error if not).
*     extra
*        Extra mode-specific information.
*     fail
*        If a non-zero function value is returned, a pointer to the
*        Object that caused the failure is returned at "*fail". This may
*        be "this" or it may be an Object contained within "this". Note,
*        the Object's reference count is not incremented, and so the
*        returned pointer should not be annulled. A NULL pointer is
*        returned if this function returns a value of zero.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*    A local status value:
*        0 - Success
*        1 - Could not lock or unlock the object because it was already
*            locked by another thread.
*        2 - Failed to lock a POSIX mutex
*        3 - Failed to unlock a POSIX mutex
*        4 - Bad "mode" value supplied.

*  Notes:
*     - This function attempts to execute even if an error has already
*     occurred.
*/

/* Local Variables: */
   AstFrame *this;       /* Pointer to Frame structure */
   int i;                /* Loop count */
   int result;           /* Returned status value */

/* Initialise */
   result = 0;

/* Check the supplied pointer is not NULL. */
   if( !this_object ) return result;

/* Obtain a pointers to the Frame structure. */
   this = (AstFrame *) this_object;

/* Invoke the ManageLock method inherited from the parent class. */
   if( !result ) result = (*parent_managelock)( this_object, mode, extra,
                          fail, status );

/* Invoke the astManageLock method on any Objects contained within
   the supplied Object. */
   for( i = 0; i < this->naxes; i++ ) {
      if( !result ) result = astManageLock( this->axis[ i ], mode, extra,
                                            fail );
   }

   return result;

}
#endif

static int *MapSplit( AstMapping *this_map, int nin, const int *in, AstMapping **map, int *status ){
/*
*  Name:
*     MapSplit

*  Purpose:
*     Create a Mapping representing a subset of the inputs of an existing
*     Frame.

*  Type:
*     Private function.

*  Synopsis:
*     #include "frame.h"
*     int *MapSplit( AstMapping *this, int nin, const int *in, AstMapping **map, int *status )

*  Class Membership:
*     Frame method (over-rides the protected astMapSplit method
*     inherited from the Mapping class).

*  Description:
*     This function creates a new Mapping by picking specified inputs from
*     an existing Frame. This is only possible if the specified inputs
*     correspond to some subset of the Frame outputs. That is, there
*     must exist a subset of the Frame outputs for which each output
*     depends only on the selected Frame inputs, and not on any of the
*     inputs which have not been selected. If this condition is not met
*     by the supplied Frame, then a NULL Mapping is returned.

*  Parameters:
*     this
*        Pointer to the Frame to be split (the Frame is not actually
*        modified by this function).
*     nin
*        The number of inputs to pick from "this".
*     in
*        Pointer to an array of indices (zero based) for the inputs which
*        are to be picked. This array should have "nin" elements. If "Nin"
*        is the number of inputs of the supplied Frame, then each element
*        should have a value in the range zero to Nin-1.
*     map
*        Address of a location at which to return a pointer to the new
*        Mapping. This Mapping will have "nin" inputs (the number of
*        outputs may be different to "nin"). A NULL pointer will be
*        returned if the supplied Frame has no subset of outputs which
*        depend only on the selected inputs.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     A pointer to a dynamically allocated array of ints. The number of
*     elements in this array will equal the number of outputs for the
*     returned Mapping. Each element will hold the index of the
*     corresponding output in the supplied Frame. The array should be
*     freed using astFree when no longer needed. A NULL pointer will
*     be returned if no output Mapping can be created.

*  Notes:
*     - If this function is invoked with the global error status set,
*     or if it should fail for any reason, then NULL values will be
*     returned as the function value and for the "map" pointer.
*/

/* Local Variables: */
   int *result;         /* Returned pointer */

/* Initialise */
   result = NULL;
   *map = NULL;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Pick the selected axes from the Frame. */
   *map = (AstMapping *) astPickAxes( (AstFrame *) this_map, nin, in, NULL );

/* Return a copy of the supplied axis array.*/
   result = astStore( NULL, in, sizeof( int )*(size_t) nin );

/* Free returned resources if an error has occurred. */
   if( !astOK ) {
      result = astFree( result );
      *map = astAnnul( *map );
   }

/* Return the list of output indices. */
   return result;
}

static int Match( AstFrame *template, AstFrame *target, int matchsub,
                  int **template_axes, int **target_axes,
                  AstMapping **map, AstFrame **result, int *status ) {
/*
*+
*  Name:
*     astMatch

*  Purpose:
*     Determine if conversion is possible between two coordinate systems.

*  Type:
*     Protected virtual function.

*  Synopsis:
*     #include "frame.h"
*     int astMatch( AstFrame *template, AstFrame *target, int matchsub,
*                   int **template_axes, int **target_axes,
*                   AstMapping **map, AstFrame **result )

*  Class Membership:
*     Frame method.

*  Description:
*     This function matches a "template" frame to a "target" frame and
*     determines whether it is possible to convert coordinates between
*     them.  If it is, a mapping that performs the transformation is
*     returned along with a new Frame that describes the coordinate
*     system that results when this mapping is applied to the "target"
*     coordinate system. In addition, information is returned to allow
*     the axes in this "result" Frame to be associated with the
*     corresponding axes in the "target" and "template" Frames from
*     which they are derived.

*  Parameters:
*     template
*        Pointer to the template Frame. This describes the coordinate
*        system (or set of possible coordinate systems) into which we
*        wish to convert our coordinates.
*     target
*        Pointer to the target Frame. This describes the coordinate
*        system in which we already have coordinates.
*     matchsub
*        If zero then a match only occurs if the template is of the same
*        class as the target, or of a more specialised class. If non-zero
*        then a match can occur even if this is not the case (i.e. if the
*        target is of a more specialised class than the template). In
*        this latter case, the target is cast down to the class of the
*        template. NOTE, this argument is handled by the global method
*        wrapper function "astMatch_", rather than by the class-specific
*        implementations of this method.
*     template_axes
*        Address of a location where a pointer to int will be returned
*        if the requested coordinate conversion is possible. This
*        pointer will point at a dynamically allocated array of
*        integers with one element for each axis of the "result" Frame
*        (see below). It must be freed by the caller (using astFree)
*        when no longer required.
*
*        For each axis in the result Frame, the corresponding element
*        of this array will return the (zero-based) index of the
*        template Frame axis from which it is derived. If it is not
*        derived from any template frame axis, a value of -1 will be
*        returned instead.
*     target_axes
*        Address of a location where a pointer to int will be returned
*        if the requested coordinate conversion is possible. This
*        pointer will point at a dynamically allocated array of
*        integers with one element for each axis of the "result" Frame
*        (see below). It must be freed by the caller (using astFree)
*        when no longer required.
*
*        For each axis in the result Frame, the corresponding element
*        of this array will return the (zero-based) index of the
*        target Frame axis from which it is derived. If it is not
*        derived from any target Frame axis, a value of -1 will be
*        returned instead.
*     map
*        Address of a location where a pointer to a new Mapping will
*        be returned if the requested coordinate conversion is
*        possible. If returned, the forward transformation of this
*        Mapping may be used to convert coordinates between the
*        "target" Frame and the "result" Frame (see below) and the
*        inverse transformation will convert in the opposite
*        direction.
*     result
*        Address of a location where a pointer to a new Frame will be
*        returned if the requested coordinate conversion is
*        possible. If returned, this Frame describes the coordinate
*        system that results from applying the returned Mapping
*        (above) to the "target" coordinate system. In general, this
*        Frame will combine attributes from (and will therefore be
*        more specific than) both the target and the template
*        Frames. In particular, when the template allows the
*        possibility of transformaing to any one of a set of
*        alternative coordinate systems, the "result" Frame will
*        indicate which of the alternatives was used.

*  Returned Value:
*     A non-zero value is returned if the requested coordinate
*     conversion is possible. Otherwise zero is returned (this will
*     not in itself result in an error condition).

*  Notes:
*     - By default, the "result" frame will have its number of axes
*     and axis order determined by the "template" Frame. However, if
*     the PreserveAxes attribute of the template frame is non-zero,
*     then the axis count and axis order of the "target" frame will be
*     used instead.
*     - The template_axes and target_axes arrays are provided so that
*     if the caller needs to permute the target and/or template axes
*     before invoking this function, it is possible to deduce how the
*     result axes should be permuted so as to correspond with the
*     original template/target axis order.
*     - For result axes that do not correspond with a template and/or
*     target axis (where a value of -1 is returned in the
*     template_axes and/or target_axes arrays), the caller has no
*     clear way of knowing where these axes should appear in any
*     permuted order. In this case, the relative position of these
*     axes within the result Frame (with respect to axes that do have
*     template/target axis associations) will be used to convey this
*     information. Such axes should be taken to be associated either
*     with the next preceding or following axis (depending on the
*     AST__MATCHEND flag of the template frame) which does have an
*     association.
*     - If the result Frame has zero axes, then NULL pointer values
*     will be returned for *template_axes and *target_axes.
*     - A value of zero will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*-

*  Implementation Notes:
*     This implementation addresses the matching of a Frame class
*     object to other types of Frames (i.e. the target may be from a
*     derived class). A Frame will match any other type of Frame with
*     an acceptable number of axes but will not distinguish axis order
*     (i.e. it will match the axes in whatever order they are
*     given). If the template Frame has a value set for its Domain
*     attribute, then it will only match another Frame with the same
*     Domain.
*/

/* Local Variables: */
   char *template_domain;        /* Pointer to copy of template domain */
   const char *ptr;              /* Pointer to domain string */
   const char *target_domain;    /* Pointer to target domain string */
   int match;                    /* Template matches target? */
   int match_end;                /* Match final axes of target? */
   int max_axes;                 /* Maximum acceptable number of axes */
   int min_axes;                 /* Minimum acceptable nu,ber of axes */
   int preserve_axes;            /* Preserve target axes? */
   int result_axis;              /* Loop counter for result axes */
   int result_naxes;             /* Number of result axes */
   int target_naxes;             /* Number of target axes */
   int template_naxes;           /* Number of template axes */

/* Initialise the returned values. */
   *template_axes = NULL;
   *target_axes = NULL;
   *map = NULL;
   *result = NULL;
   match = 0;

/* Check the global error status. */
   if ( !astOK ) return match;

/* The first requirement for a match is that the target object is a Frame. This
   is already known to be true, as it forms part of the argument validation
   for this function. */

/* The second requirement is that the number of target axes is acceptable.
   Obtain the number of target axes and the minimum and maximum number of axes
   that the template will match. */
   target_naxes = astGetNaxes( target );
   min_axes = astGetMinAxes( template );
   max_axes = astGetMaxAxes( template );

/* Test if the number of target axes is acceptable. */
   if ( astOK ) {
      match = ( ( target_naxes >= min_axes ) && ( target_naxes <= max_axes ) );
   }

/* The third requirement is that if the template has its Domain
   attribute defined, then the target must also have the same Domain
   (although it need not be set - the default will do). First check if
   the template has a domain. */
   if ( astOK && match ) {
      if ( astTestDomain( template ) ) {

/* Obtain a pointer to the template domain. Then allocate memory and
   make a copy of it (this is necessary as we will next inquire the
   domain of the target and may over-write the buffer holding the
   template's domain). */
         ptr = astGetDomain( template );
         if ( astOK ) {
            template_domain = astStore( NULL, ptr,
                                        strlen( ptr ) + (size_t) 1 );

/* Obtain a pointer to the target domain. */
            target_domain = astGetDomain( target );

/* Compare the domain strings for equality. Then free the memory
   allocated above. */
            match = astOK && !strcmp( template_domain, target_domain );
            template_domain = astFree( template_domain );
         }
      }
   }

/* If the template matches, obtain the values of the template's PreserveAxes
   and MatchEnd attributes and determine the number of template axes. */
   if ( astOK && match ) {
      preserve_axes = astGetPreserveAxes( template );
      match_end = astGetMatchEnd( template );
      template_naxes = astGetNaxes( template );

/* If the PreserveAxes attribute is non-zero, the target axes should be
   preserved, so the number of result axes equals the number of target axes.
   Otherwise the number of template axes is used. */
      result_naxes = preserve_axes ? target_naxes : template_naxes;

/* Allocate memory for the arrays of axis associations to be returned. */
      *template_axes = astMalloc( sizeof( int ) * (size_t) result_naxes );
      *target_axes = astMalloc( sizeof( int ) * (size_t) result_naxes );
      if ( astOK ) {

/* Loop through each of the result axes. */
         for ( result_axis = 0; result_axis < result_naxes; result_axis++ ) {

/* Set up the axis associations. By default, associate the first result axis
   with the first template/target axis. */
            (*template_axes)[ result_axis ] = result_axis;
            (*target_axes)[ result_axis ] = result_axis;

/* However, if the MatchEnd attribute is non-zero, associate the last result
   axis with the last template/target axis (this only makes a difference if
   there is a difference in the number of axes). */
            if ( match_end ) {
               (*template_axes)[ result_axis ] +=
                                               template_naxes - result_naxes;
               (*target_axes)[ result_axis ] += target_naxes - result_naxes;
            }

/* If any of the associations would be with a template/target axis that doesn't
   exist, then use an axis index of -1 for the association instead. */
            if ( ( (*template_axes)[ result_axis ] < 0 ) ||
                 ( (*template_axes)[ result_axis ] >= template_naxes ) ) {
               (*template_axes)[ result_axis ] = -1;
	    }
            if ( ( (*target_axes)[ result_axis ] < 0 ) ||
                 ( (*target_axes)[ result_axis ] >= target_naxes ) ) {
               (*target_axes)[ result_axis ] = -1;
	    }
         }

/* Use the target's astSubFrame method to select the required axes from it,
   overlaying the template's attributes on to the resulting Frame. This process
   also generates the required Mapping between the target and result Frames. */
         match = astSubFrame( target, template,
                              result_naxes, *target_axes, *template_axes,
                              map, result );
      }
   }

/* If an error occurred, free any allocated memory and reset the result. */
   if ( !astOK || !match ) {
      *template_axes = astFree( *template_axes );
      *target_axes = astFree( *target_axes );
      match = 0;
   }

/* Return the result. */
   return match;
}

static void MatchAxes( AstFrame *frm1, AstFrame *frm2, int *axes,
                       int *status ) {
/*
*++
*  Name:
c     astMatchAxes
f     AST_MATCHAXES

*  Purpose:
*     Find any corresponding axes in two Frames.

*  Type:
*     Public virtual function.

*  Synopsis:
c     #include "frame.h"
c     void astMatchAxes( AstFrame *frm1, AstFrame *frm2, int *axes )
f     CALL AST_MATCHAXES( FRM1, FRM2, AXES, STATUS )

*  Class Membership:
*     Frame method.

*  Description:
*     This function looks for corresponding axes within two supplied
*     Frames. An array of integers is returned that contains an element
*     for each axis in the second supplied Frame. An element in this array
*     will be set to zero if the associated axis within the second Frame
*     has no corresponding axis within the first Frame. Otherwise, it
*     will be set to the index (a non-zero positive integer) of the
*     corresponding axis within the first supplied Frame.

*  Parameters:
c     frm1
f     FRM1 = INTEGER (Given)
*        Pointer to the first Frame.
c     frm2
f     FRM2 = INTEGER (Given)
*        Pointer to the second Frame.
c     axes
f     AXES = INTEGER( * ) (Returned)
c        Pointer to an
f        An
*        integer array in which to return the indices of the axes (within
*        the first Frame) that correspond to each axis within the second
*        Frame. Axis indices start at 1. A value of zero will be stored
*        in the returned array for each axis in the second Frame that has
*        no corresponding axis in the first Frame.
*
*        The number of elements in this array must be greater than or
*        equal to the number of axes in the second Frame.
f     STATUS = INTEGER (Given and Returned)
f        The global status.

*  Applicability:
*     Frame
*        This function applies to all Frames.

*  Notes:
*     -  Corresponding axes are identified by the fact that a Mapping can
*     be found between them using
c     astFindFrame or astConvert.
f     AST_FINDFRAME or AST_CONVERT.
*     Thus, "corresponding axes" are not necessarily identical. For
*     instance, SkyFrame axes in two Frames will match even if they
*     describe different celestial coordinate systems
*--

* Implementation Notes:
*    This function is simply a wrap-up for the protected astMatchAxesX
*    method which performs the required processing but swaps the order
*    of the first two arguments. This is a trick to allow the
*    astMatchAxes method to be over-ridden by derived classes on the
*    basis of the class of either of the first two arguments.
*
*    In practice, each class that represents an encapsulated Frame (e.g.
*    FrameSet, Region, etc) should over-ride this method, extracting a
*    Frame from the supplied "frm1" pointer, and then invoking
*    astMatchAxesX.

*/

/* Check the global error status. */
   if ( !astOK ) return;

/* Invoke the "astMatchAxesX" method with the first two arguments
   swapped. */
   astMatchAxesX( frm2, frm1, axes );
}

static void MatchAxesX( AstFrame *frm2, AstFrame *frm1, int *axes,
                        int *status ) {
/*
*+
*  Name:
*     astMatchAxesX

*  Purpose:
*     Find any corresponding axes in two Frames.

*  Type:
*     Protected virtual function.

*  Synopsis:
*     #include "frame.h"
*     void astMatchAxesX( AstFrame *frm2, AstFrame *frm1, int *axes )

*  Class Membership:
*     Frame method.

*  Description:
*     This function performs the processing for the public astMatchAxes
*     method and has exactly the same interface except that the order
*     of the first two arguments is swapped. This is a trick to allow
*     the astMatchAxes method to be over-ridden by derived classes on
*     the basis of the class of either of its first two arguments.
*
*     See the astMatchAxes method for details of the interface.
*-
*/

/* Local Variables: */
   AstFrame *pfrm;
   AstFrame *resfrm;
   AstMapping *resmap;
   int *frm1_axes;
   int *pfrm_axes;
   int ifirst;
   int max_axes;
   int min_axes;
   int nax2;
   int pax;
   int preserve_axes;

/* Check the global error status. */
   if ( !astOK ) return;

/* Temporarily ensure that the PreserveAxes attribute is non-zero in
   the second supplied Frame. This means thte result Frame returned by
   astMatch below will have the axis count and order of the target Frame
   (i.e. "pfrm"). */
   if( astTestPreserveAxes( frm1 ) ) {
      preserve_axes = astGetPreserveAxes( frm1 ) ? 1 : 0;
   } else {
      preserve_axes = -1;
   }
   astSetPreserveAxes( frm1, 1 );

/* Temporarily ensure that the MaxAxes and MinAxes attributes in the
   second supplied Frame are set so the Frame can be used as a template
   in astMatch for matching any number of axes. */
   if( astTestMaxAxes( frm1 ) ) {
      max_axes = astGetMaxAxes( frm1 );
   } else {
      max_axes = -1;
   }
   astSetMaxAxes( frm1, 10000 );

   if( astTestMinAxes( frm1 ) ) {
      min_axes = astGetMinAxes( frm1 );
   } else {
      min_axes = -1;
   }
   astSetMinAxes( frm1, 1 );

/* Get the number of axes in the frm2 Frame. */
   nax2 = astGetNaxes( frm2 );

/* Loop round the axes in the frm2 Frame. */
   for( ifirst = 0; ifirst < nax2; ifirst++ ) {

/* Identify the primary Frame defining the current axis in the frm2
   Frame. */
      astPrimaryFrame( frm2, ifirst, &pfrm, &pax );

/* Attempt to find a sub-frame within the frm1 Frame that corresponds to
   this primary Frame. */
      if( astMatch( frm1, pfrm, 1, &frm1_axes, &pfrm_axes, &resmap, &resfrm ) ) {

/* Store the one-based index within "frm1" of the corresponding axis. */
         axes[ ifirst ] = frm1_axes[ pax ] + 1;

/* Free resources */
         frm1_axes = astFree( frm1_axes );
         pfrm_axes = astFree( pfrm_axes );
         resmap = astAnnul( resmap );
         resfrm = astAnnul( resfrm );

/* If no corresponding axis was found store zero in the returned array. */
      } else {
         axes[ ifirst ] = 0;
      }

/* Free resouces. */
      pfrm = astAnnul( pfrm );
   }

/* Re-instate the original attribute values in the frm1 Frame. */
   if( preserve_axes == -1 ) {
      astClearPreserveAxes( frm1 );
   } else {
      astSetPreserveAxes( frm1, preserve_axes );
   }

   if( max_axes == -1 ) {
      astClearMaxAxes( frm1 );
   } else {
      astSetMaxAxes( frm1, max_axes );
   }

   if( min_axes == -1 ) {
      astClearMinAxes( frm1 );
   } else {
      astSetMinAxes( frm1, min_axes );
   }
}

static void NewUnit( AstAxis *ax, const char *old_units, const char *new_units,
                     const char *method, const char *class, int *status ) {
/*
*  Name:
*     NewUnit

*  Purpose:
*     Modify an Axis Label and Symbol to reflect a new Unit value.

*  Type:
*     Private function.

*  Synopsis:
*     #include "frame.h"
*     void NewUnit( AstAxis *ax, const char *old_units, const char *new_units,
*                   const char *method, const char *class )

*  Class Membership:
*     Frame method.

*  Description:
*     This function modifies the Label and Symbol attributes of an Axis
*     to reflect a new Unit value. This function should only be called if
*     the ActiveUnit flag of the parent Frame is non-zero (this is not
*     checked within this function).
*
*     If the axis has a set label, then we may be able to modify it to
*     correctly describe the axis in the supplied new units. For instance,
*     if the original units were "Hz", the original label was "frequency",
*     and the new units are "log(Hz)", then the label is modified to become
*     "log( frequency )".
*
*     The Axis Format attribute is cleared if the supplied units are
*     different to the old units (because any set format is probably not
*     going to be appropriate for a new system of units.

*  Parameters:
*     ax
*        Pointer to the Axis.
*     old_units
*        The original units value.
*     new_units
*        The new units value.
*     method
*        Pointer to a constant null-terminated character string
*        containing the name of the method that invoked this function
*        to validate an axis selection. This method name is used
*        solely for constructing error messages.
*     class
*        Pointer to a constant null-terminated character string
*        containing the name of the class upon which this function
*        was invoked. This is used solely for constructing error messages.

*  Returned Value:
*     void.
*/

/* Local Variables: */
   AstMapping *map;              /* Pointer to units Mapping */
   char *new_lab;                /* Pointer to new axis label */
   char *new_sym;                /* Pointer to new axis symbol */

/* Check the global error status. */
   if ( !astOK ) return;

/* Check that the axis label is set. We relay on sub-classes to return
   appropriate default labels if the label is not set. */
   if( astTestAxisLabel( ax ) ) {

/* See if it is possible to map the old units into the new units.
   If it is, then a Mapping is returned together with an appropriately
   modified label. */
      map = astUnitMapper( old_units, new_units, astGetAxisLabel( ax ),
                           &new_lab );

/* If succesfull, annul the Mapping (which we do not need), and store the
   modified label in the Axis, finally freeing the memory used to hold
   the modified label. */
      if( map ) {
         map = astAnnul( map );
         if( new_lab ) {
            astSetAxisLabel( ax, new_lab );
            new_lab = astFree( new_lab );
         }
      }
   }

/* Do the same for the axis symbol. */
   if( astTestAxisSymbol( ax ) ) {
      map = astUnitMapper( old_units, new_units, astGetAxisSymbol( ax ),
                           &new_sym );
      if( map ) {
         map = astAnnul( map );
         if( new_sym ) {
            astSetAxisSymbol( ax, new_sym );
            new_sym = astFree( new_sym );
         }
      }
   }

/* If succesful, clear the axis format if the new and old units are
   different. */
   if( astOK ) {
      if( strcmp( old_units, new_units ) ) astClearAxisFormat( ax );
   }

}

static void Norm( AstFrame *this, double value[], int *status ) {
/*
*++
*  Name:
c     astNorm
f     AST_NORM

*  Purpose:
*     Normalise a set of Frame coordinates.

*  Type:
*     Public virtual function.

*  Synopsis:
c     #include "frame.h"
c     void astNorm( AstFrame *this, double value[] )
f     CALL AST_NORM( THIS, VALUE, STATUS )

*  Class Membership:
*     Frame method.

*  Description:
c     This function normalises a set of Frame coordinate values which
f     This routine normalises a set of Frame coordinate values which
*     might be unsuitable for display (e.g. may lie outside the
*     expected range) into a set of acceptable values suitable for
*     display.

*  Parameters:
c     this
f     THIS = INTEGER (Given)
*        Pointer to the Frame.
c     value
f     VALUE( * ) = DOUBLE PRECISION (Given and Returned)
c        An array of double, with one element for each Frame axis
f        An array with one element for each Frame axis
*        (Naxes attribute). Initially, this should contain a set of
*        coordinate values representing a point in the space which the
*        Frame describes. If these values lie outside the expected
*        range for the Frame, they will be replaced with more
*        acceptable (normalised) values. Otherwise, they will be
*        returned unchanged.
f     STATUS = INTEGER (Given and Returned)
f        The global status.

*  Notes:
*     - For some classes of Frame, whose coordinate values are not
*     constrained, this function will never modify the values
*     supplied. However, for Frames whose axes represent cyclic
*     quantities (such as angles or positions on the sky), coordinates
*     will typically be wrapped into an appropriate standard range,
*     such as zero to 2*pi.
*     - The NormMap class is a Mapping which can be used to normalise a
*     set of points using the
c     astNorm function
f     AST_NORM routine
*     of a specified Frame.
*     - It is intended to be possible to put any set of coordinates
*     into a form suitable for display by using this function to
*     normalise them, followed by appropriate formatting
c     (using astFormat).
f     (using AST_FORMAT).
*--
*/

/* Local Variables: */
   AstAxis *ax;                  /* Pointer to Axis object */
   int axis;                     /* Loop counter for axes */
   int naxes;                    /* Number of Frame axes */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain the number of Frame axes. */
   naxes = astGetNaxes( this );

/* Loop to process the coordinate for each axis in turn. */
   for ( axis = 0; axis < naxes; axis++ ) {

/* Obtain a pointer to the relevant Frame Axis. */
      ax = astGetAxis( this, axis );

/* Normalise the coordinate for this axis. */
      astAxisNorm( ax, value + axis );

/* Annul the pointer to the Axis. */
      ax = astAnnul( ax );

/* Quit looping if an error occurs. */
      if ( !astOK ) break;
   }
}

static void NormBox( AstFrame *this, double lbnd[], double ubnd[],
                     AstMapping *reg, int *status ) {
/*
*+
*  Name:
*     astNormBox

*  Purpose:
*     Extend a box to include effect of any singularities in the Frame.

*  Type:
*     Protected virtual function.

*  Synopsis:
*     #include "frame.h"
*     void astNormBox( AstFrame *this, double lbnd[], double ubnd[],
*                      AstMapping *reg )

*  Class Membership:
*     Frame method.

*  Description:
*     This function modifies a supplied box to include the effect of any
*     singularities in the co-ordinate system represented by the Frame.
*     For a normal Cartesian coordinate system, the box will be returned
*     unchanged. Other classes of Frame may do other things. For instance,
*     a SkyFrame will check to see if the box contains either the north
*     or south pole and extend the box appropriately.

*  Parameters:
*     this
*        Pointer to the Frame.
*     lbnd
*        An array of double, with one element for each Frame axis
*        (Naxes attribute). Initially, this should contain a set of
*        lower axis bounds for the box. They will be modified on exit
*        to include the effect of any singularities within the box.
*     ubnd
*        An array of double, with one element for each Frame axis
*        (Naxes attribute). Initially, this should contain a set of
*        upper axis bounds for the box. They will be modified on exit
*        to include the effect of any singularities within the box.
*     reg
*        A Mapping which should be used to test if any singular points are
*        inside or outside the box. The Mapping should leave an input
*        position unchanged if the point is inside the box, and should
*        set all bad if the point is outside the box.
*-
*/

/* This base class returns the box limits unchanged. */
}

static double Offset2( AstFrame *this, const double point1[2], double angle,
                     double offset, double point2[2], int *status ){
/*
*++
*  Name:
c     astOffset2
f     AST_OFFSET2

*  Purpose:
*     Calculate an offset along a geodesic curve in a 2D Frame.

*  Type:
*     Public virtual function.

*  Synopsis:
c     #include "frame.h"
c     double astOffset2( AstFrame *this, const double point1[2], double angle,
c                        double offset, double point2[2] );
f     RESULT = AST_OFFSET2( THIS, POINT1, ANGLE, OFFSET, POINT2, STATUS )

*  Class Membership:
*     Frame method.

*  Description:
c     This function finds the Frame coordinate values of a point which
f     This routine finds the Frame coordinate values of a point which
*     is offset a specified distance along the geodesic curve at a
*     given angle from a specified starting point. It can only be
*     used with 2-dimensional Frames.
*
*     For example, in a basic Frame, this offset will be along the
*     straight line joining two points. For a more specialised Frame
*     describing a sky coordinate system, however, it would be along
*     the great circle passing through two sky positions.

*  Parameters:
c     this
f     THIS = INTEGER (Given)
*        Pointer to the Frame.
c     point1
f     POINT1( * ) = DOUBLE PRECISION (Given)
c        An array of double, with one element for each Frame axis
f        An array with one element for each Frame axis
*        (Naxes attribute). This should contain the coordinates of the
*        point marking the start of the geodesic curve.
c     angle
f     ANGLE = DOUBLE PRECISION (Given)
*        The angle (in radians) from the positive direction of the second
*        axis, to the direction of the required position, as seen from
*        the starting position. Positive rotation is in the sense of
*        rotation from the positive direction of axis 2 to the positive
*        direction of axis 1.
c     offset
f     OFFSET = DOUBLE PRECISION
*        The required offset from the first point along the geodesic
*        curve. If this is positive, it will be in the direction of the
*        given angle. If it is negative, it will be in the opposite
*        direction.
c     point2
f     POINT2( * ) = DOUBLE PRECISION (Returned)
c        An array of double, with one element for each Frame axis
f        An array with one element for each Frame axis
*        in which the coordinates of the required point will be returned.
f     STATUS = INTEGER (Given and Returned)
f        The global status.

*  Returned Value:
c     astOffset2
f     AST_OFFSET2 = DOUBLE PRECISION
*        The direction of the geodesic curve at the end point. That is, the
*        angle (in radians) between the positive direction of the second
*        axis and the continuation of the geodesic curve at the requested
*        end point. Positive rotation is in the sense of rotation from
*        the positive direction of axis 2 to the positive direction of axis
*        1.

*  Notes:
c     - The geodesic curve used by this function is the path of
f     - The geodesic curve used by this routine is the path of
*     shortest distance between two points, as defined by the
c     astDistance function.
f     AST_DISTANCE function.
*     - An error will be reported if the Frame is not 2-dimensional.
*     - This function will return "bad" coordinate values (AST__BAD)
*     if any of the input coordinates has this value.
*--
*/

/* Local Variables: */
   int naxes;                    /* Number of Frame axes */
   double result;                /* Returned value */

/* Check the global error status. */
   result = AST__BAD;
   if ( !astOK ) return result;

/* Initialize bad values. */
   point2[ 0 ] = AST__BAD;
   point2[ 1 ] = AST__BAD;

/* Determine the number of Frame axes. */
   naxes = astGetNaxes( this );

/* Report an error if the Frame is not 2 dimensional. */
   if( naxes != 2 && astOK ) {
      astError( AST__NAXIN, "astOffset2(%s): Invalid number of Frame axes (%d)."
                " astOffset2 can only be used with 2 dimensonal Frames.", status,
                astGetClass( this ), naxes );
   }

/* Check the supplied values. */
   if ( astOK && point1[ 0 ] != AST__BAD && point1[ 1 ] != AST__BAD &&
        angle != AST__BAD && offset != AST__BAD ) {

/* Store the results. */
      point2[ 0 ] = point1[ 0 ] + sin( angle )*offset;
      point2[ 1 ] = point1[ 1 ] + cos( angle )*offset;

/* The position angle of the curve does not vary in cartesian coordinates */
      result = angle;

   }

/* Return the result. */
   return result;

}

static void Offset( AstFrame *this, const double point1[],
                    const double point2[], double offset, double point3[], int *status ) {
/*
*++
*  Name:
c     astOffset
f     AST_OFFSET

*  Purpose:
*     Calculate an offset along a geodesic curve.

*  Type:
*     Public virtual function.

*  Synopsis:
c     #include "frame.h"
c     void astOffset( AstFrame *this,
c                     const double point1[], const double point2[],
c                     double offset, double point3[] )
f     CALL AST_OFFSET( THIS, POINT1, POINT2, OFFSET, POINT3, STATUS )

*  Class Membership:
*     Frame method.

*  Description:
c     This function finds the Frame coordinate values of a point which
f     This routine finds the Frame coordinate values of a point which
*     is offset a specified distance along the geodesic curve between
*     two other points.
*
*     For example, in a basic Frame, this offset will be along the
*     straight line joining two points. For a more specialised Frame
*     describing a sky coordinate system, however, it would be along
*     the great circle passing through two sky positions.

*  Parameters:
c     this
f     THIS = INTEGER (Given)
*        Pointer to the Frame.
c     point1
f     POINT1( * ) = DOUBLE PRECISION (Given)
c        An array of double, with one element for each Frame axis
f        An array with one element for each Frame axis
*        (Naxes attribute). This should contain the coordinates of the
*        point marking the start of the geodesic curve.
c     point2
f     POINT2( * ) = DOUBLE PRECISION (Given)
c        An array of double, with one element for each Frame axis
f        An array with one element for each Frame axis.
*        This should contain the coordinates of the point marking the
*        end of the geodesic curve.
c     offset
f     OFFSET = DOUBLE PRECISION
*        The required offset from the first point along the geodesic
*        curve. If this is positive, it will be towards the second
*        point. If it is negative, it will be in the opposite
*        direction. This offset need not imply a position lying
*        between the two points given, as the curve will be
*        extrapolated if necessary.
c     point3
f     POINT3( * ) = DOUBLE PRECISION (Returned)
c        An array of double, with one element for each Frame axis
f        An array with one element for each Frame axis
*        in which the coordinates of the required point will be returned.
f     STATUS = INTEGER (Given and Returned)
f        The global status.

*  Notes:
c     - The geodesic curve used by this function is the path of
f     - The geodesic curve used by this routine is the path of
*     shortest distance between two points, as defined by the
c     astDistance function.
f     AST_DISTANCE function.
*     - This function will return "bad" coordinate values (AST__BAD)
*     if any of the input coordinates has this value.
*     - "Bad" coordinate values will also be returned if the two
*     points supplied are coincident (or otherwise fail to uniquely
*     specify a geodesic curve) but the requested offset is non-zero.
*--
*/

/* Local Variables: */
   double delta;                 /* Displacement along axis */
   double dist;                  /* Distance between points */
   double fract;                 /* Fraction of distance required */
   int axis;                     /* Loop counter for axes */
   int naxes;                    /* Number of Frame axes */

/* Check the global error status. */
   if ( !astOK ) return;

/* Determine the number of Frame axes. */
   naxes = astGetNaxes( this );
   if ( astOK ) {

/* Loop to determine the Cartesian distance between points 1 and 2. */
      dist = 0.0;
      for ( axis = 0; axis < naxes; axis++ ) {

/* If any of the coordinates supplied is bad, set the distance to be
   bad and quit looping. */
         if ( ( point1[ axis ] == AST__BAD ) ||
              ( point2[ axis ] == AST__BAD ) ) {
            dist = AST__BAD;
            break;

/* Otherwise, accumulate the sum of squared displacements along each
   axis. */
         } else {
            delta = point1[ axis ] - point2[ axis ];
            dist += ( delta * delta );
         }
      }

/* Take the square root to find the distance (if valid). */
      if ( dist != AST__BAD ) dist = sqrt( dist );

/* If the distance between the points cannot be found, or the distance
   is zero but the required offset is non-zero, then set the result
   coordinates to be bad. */
      if ( ( dist == AST__BAD ) ||
           ( ( dist == 0.0 ) && ( offset != 0.0 ) ) ) {
         for ( axis = 0; axis < naxes; axis++ ) {
            point3[ axis ] = AST__BAD;
         }

/* Otherwise, calculate what fraction of the distance between the
   points we need to move, and apply this fraction of the displacement
   along each axis. */
      } else {
         fract = ( dist == 0.0 ) ? 0.0 : offset / dist;
         for ( axis = 0; axis < naxes; axis++ ) {
            point3[ axis ] = point1[ axis ] +
                             fract * ( point2[ axis ] - point1[ axis ] );
         }
      }
   }
}

static void Overlay( AstFrame *template, const int *template_axes,
                     AstFrame *result, int *status ) {
/*
*+
*  Name:
*     astOverlay

*  Purpose:
*     Overlay the attributes of a template Frame on to another Frame.

*  Type:
*     Protected virtual function.

*  Synopsis:
*     #include "frame.h"
*     void astOverlay( AstFrame *template, const int *template_axes,
*                      AstFrame *result )

*  Class Membership:
*     Frame method.

*  Description:
*     This function overlays attributes of one Frame (the "template") on to
*     another Frame, so as to over-ride selected attributes of that second
*     Frame. Normally only those attributes which have been specifically set
*     in the template will be transferred. This implements a form of
*     defaulting, in which a Frame acquires attributes from the template, but
*     retains its original attributes (as the default) if new values have not
*     previously been explicitly set in the template.

*  Parameters:
*     template
*        Pointer to the template Frame, for which values should have been
*        explicitly set for any attribute which is to be transferred.
*     template_axes
*        Pointer to an array of int, with one element for each axis of the
*        "result" Frame (see below). For each axis in the result frame, the
*        corresponding element of this array should contain the (zero-based)
*        index of the template axis to which it corresponds. This array is used
*        to establish from which template axis any axis-dependent attributes
*        should be obtained.
*
*        If any axis in the result Frame is not associated with a template
*        axis, the corresponding element of this array should be set to -1.
*
*        If a NULL pointer is supplied, the template and result axis
*        indicies are assumed to be identical.
*     result
*        Pointer to the Frame which is to receive the new attribute values.
*-
*/

/* Local Variables: */
   AstAxis *result_ax;           /* Pointer to result Axis object */
   AstAxis *template_ax;         /* Pointer to template Axis object */
   AstSystemType sys;            /* System value */
   int result_axis;              /* Loop counter for result Frame axes */
   int result_naxes;             /* Number of result Frame axes */
   int template_axis;            /* Index of template Frame axis */
   int template_naxes;           /* Number of template Frame axes */

/* Check the global error status. */
   if ( !astOK ) return;

/* Define a macro that tests whether an attribute is set in the template and,
   if so, transfers its value to the target. */
#define OVERLAY(attribute) \
   if ( astTest##attribute( template ) ) { \
      astSet##attribute( result, astGet##attribute( template ) ); \
   }

/* Use the macro to transfer each Frame attribute in turn. */
   OVERLAY(Dut1);
   OVERLAY(Digits);
   OVERLAY(Domain);
   OVERLAY(Epoch);
   OVERLAY(Title);
   OVERLAY(ObsLat)
   OVERLAY(ObsLon)
   OVERLAY(ObsAlt)

/* Transfer the ActiveUnit flag. */
   astSetActiveUnit( result, astGetActiveUnit( template ) );

/* Only overlay the System and AlignSystem attribute if the values are
   valid for the result class. */
   if( astTestSystem( template ) ) {
      sys = astGetSystem( template );
      if( astValidateSystem( result, sys, "astOverlay" ) ) {
         astSetSystem( result, sys );
      }
   }

   if( astTestAlignSystem( template ) ) {
      sys = astGetAlignSystem( template );
      if( astValidateSystem( result, sys, "astOverlay" ) ) {
         astSetAlignSystem( result, sys );
      }
   }

/* Now transfer attributes associated with individual axes. Obtain the number
   of axes in the template and result Frames. */
   template_naxes = astGetNaxes( template );
   result_naxes = astGetNaxes( result );
   if ( astOK ) {

/* Loop through all the axes in the result Frame and determine to which
   template axis each one corresponds. Check that the resulting axis index is
   valid. If not, then the axis will not receive new attributes. */
      for ( result_axis = 0; result_axis < result_naxes; result_axis++ ) {
         template_axis = template_axes ? template_axes[ result_axis ] : result_axis;
         if ( ( template_axis >= 0 ) && ( template_axis < template_naxes ) ) {

/* Obtain pointers to the relevant Axis objects of each Frame and use the
   astAxisOverlay method of the template Axis to overlay attributes on to
   the result Axis. Annul the Axis pointers afterwards. */
            template_ax = astGetAxis( template, template_axis );
            result_ax = astGetAxis( result, result_axis );
            astAxisOverlay( template_ax, result_ax );
            template_ax = astAnnul( template_ax );
            result_ax = astAnnul( result_ax );

/* Quit looping if an error occurs. */
            if ( !astOK ) break;
         }
      }
   }

/* Undefine macros local to this function. */
#undef OVERLAY
}

static void PermAxes( AstFrame *this, const int perm[], int *status ) {
/*
*+
*  Name:
*     astPermAxes

*  Purpose:
*     Permute the order of a Frame's axes.

*  Type:
*     Protected virtual function.

*  Synopsis:
*     #include "frame.h"
*     void astPermAxes( AstFrame *this, const int perm[] )

*  Class Membership:
*     Frame method.

*  Description:
*     This function permutes the order in which a Frame's axes occur.

*  Parameters:
*     this
*        Pointer to the Frame.
*     perm
*        An array of int (with one element for each axis of the Frame)
*        which lists the axes in their new order. Each element of this
*        array should be a (zero-based) axis index identifying the
*        axes according to their old (un-permuted) order.

*  Notes:
*     - Only genuine permutations of the axis order are permitted, so
*     each axis must be referenced exactly once in the "perm" array.
*     - If more than one axis permutation is applied to a Frame, the
*     effects are cumulative.
*-

*  Implementation Notes:
*     - This function implements the basic astPermAxes method which is
*     available via the protected interface to the Frame class. A
*     public interface to this method is provided by the
*     astPermAxesId_ function.
*/

/* Local Variables: */
   int *old;                     /* Pointer to copy of old permutation array */
   int axis;                     /* Loop counter for Frame axes */
   int naxes;                    /* Number of Frame axes */

/* Check the global error status. */
   if ( !astOK ) return;

/* Validate the permutation array, to check that it describes a genuine
   permutation. */
   astCheckPerm( this, perm, "astPermAxes" );

/* Obtain the number of Frame axes. */
   naxes = astGetNaxes( this );

/* Allocate memory and use it to store a copy of the old permutation array for
   the Frame. */
   old = astStore( NULL, this->perm, sizeof( int ) * (size_t) naxes );

/* Apply the new axis permutation cumulatively to the old one and store the
   result in the Frame. */
   if ( astOK ) {
      for ( axis = 0; axis < naxes; axis++ ) {
         this->perm[ axis ] = old[ perm[ axis ] ];
      }
   }

/* Free the temporary copy of the old array. */
   old = astFree( old );
}

static AstFrame *PickAxes( AstFrame *this, int naxes, const int axes[],
                           AstMapping **map, int *status ) {
/*
*+
*  Name:
*     astPickAxes

*  Purpose:
*     Create a new Frame by picking axes from an existing one.

*  Type:
*     Protected virtual function.

*  Synopsis:
*     #include "frame.h"
*     AstFrame *PickAxes( AstFrame *this, int naxes, const int axes[],
*                         AstMapping **map )

*  Class Membership:
*     Frame method.

*  Description:
*     This function creates a new Frame whose axes are copies of axes
*     picked from an existing Frame. Other Frame attributes are also
*     copied to the new Frame. Zero or more of the original axes may
*     be picked in any order, but each can be used only
*     once. Additional axes (with default characteristics) may be
*     included in the new Frame if required.
*
*     Optionally, a Mapping that converts between the original Frame's
*     axes and those of the new Frame may also be returned.

*  Parameters:
*     this
*        Pointer to the original Frame.
*     naxes
*        The number of axes required in the new Frame.
*     axes
*        Pointer to an array of int with naxes elements. This should
*        contain (zero based) axis indices specifying the axes which
*        are to be included in the new Frame, in the order
*        required. Each axis index may occur only once.
*
*        If additional (default) axes are also to be included, the
*        corresponding elements of this array should be set to -1.
*     map
*        Address of a location to receive a pointer to a new
*        Mapping. This will be a PermMap (or a UnitMap as a special
*        case) that describes the axis permutation that has taken
*        place between the original and new Frames.  The forward
*        transformation will convert from the original Frame's axes to
*        the new one's, and vice versa.
*
*        If this Mapping is not required, a NULL value may be supplied
*        for this parameter.

*  Returned Value:
*     Pointer to the new Frame.

*  Notes:
*     - The class of object returned may differ from that of the
*     original Frame, depending on which axes are selected. For
*     example, if a single axis is picked from a SkyFrame (which
*     always has two axes), the resulting Frame cannot be a valid
*     SkyFrame, so will revert to the parent class (Frame) instead.
*     - The new Frame contains a deep copy of all the data selected
*     from the original Frame. Modifying the new Frame will therefore
*     not affect the original one.
*     - A NULL pointer will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*-

*  Implementation Notes:
*     - This function implements the basic astPickAxes method
*     available via the protected interface to the Frame class. The
*     public interface to this method is provided by the
*     astPickAxesId_ function.
*/

/* Local Variables: */
   AstFrame *frame;              /* Pointer to Frame to be returned */
   AstMapping *mapping;          /* Pointer to Mapping to be returned */

/* Check the global error status. */
   if ( !astOK ) return NULL;

/* Initialise the returned pointers. */
   frame = NULL;
   if ( map ) *map = NULL;

/* Check that a valid set of axes is being selected . */
   astValidateAxisSelection( this, naxes, axes, "astPickAxes" );

/* Create the required new Frame by selecting the axes. This also returns a
   Mapping which transforms between the original Frame and the new one. */
   astSubFrame( this, NULL, naxes, axes, NULL, &mapping, &frame );
   if ( astOK ) {

/* Return the Mapping pointer if required. */
      if ( map ) {
         *map = mapping;

/* Otherwise annul the Mapping. If an error occurs, also annul the Frame. */
      } else {
         mapping = astAnnul( mapping );
         if ( !astOK ) frame = astAnnul( frame );
      }
   }

/* Return the pointer to the new Frame. */
   return frame;
}

static void PrimaryFrame( AstFrame *this, int axis1,
                          AstFrame **frame, int *axis2, int *status ) {
/*
*+
*  Name:
*     astPrimaryFrame

*  Purpose:
*     Uniquely identify a primary Frame and one of its axes.

*  Type:
*     Protected virtual function.

*  Synopsis:
*     #include "frame.h"
*     void astPrimaryFrame( AstFrame *this, int axis1, AstFrame **frame,
*                           int *axis2 )

*  Class Membership:
*     Frame method.

*  Description:
*     This function returns information about the underlying (primary) Frame
*     corresponding to a specified axis, when given what may be a compound
*     Frame composed of more than one simpler one.

*  Parameters:
*     this
*        Pointer to the Frame.
*     axis1
*        An axis index (zero-based) identifying the Frame axis for which
*        information is required.
*     frame
*        Address of a location to receive a pointer to the underlying (primary)
*        frame to which the requested axis belongs (i.e. this will not be a
*        compound Frame).
*     axis2
*        Pointer to an int which is to receive the (zero-based) axis
*        index within "frame" which identifies the axis being referred
*        to, using the axis order that applied when the primary Frame
*        was originally constructed (i.e. this function undoes all
*        subsequent axis pemutations and the effects of combining
*        Frames, in order to reveal the original underlying axis
*        order).

*  Notes:
*     -  This protected method is provided so that class implementations can
*     distinguish the axes of frames from one another (e.g. can distinguish
*     a longitude axis as being different from a latitide axis) even after
*     their order has been permuted and they have been combined with axes from
*     other Frames.
*     -  The reference count of the primary Frame will be incremented by one to
*     reflect the new pointer returned.
*-
*/

/* Check the global error status. */
   if ( !astOK ) return;

/* Initialise the returned values. */
   *frame = NULL;
   *axis2 = 0;

/* Validate and permute the axis index supplied. */
   axis1 = astValidateAxis( this, axis1, "astPrimaryFrame" );

/* Since "this" is a primary Frame (i.e. is not compound), simply clone a
   pointer to it. */
   if ( astOK ) *frame = astClone( this );

/* Return the permuted axis index, which refers to the original axis order. */
   if ( astOK ) *axis2 = axis1;
}

double astReadDateTime_( const char *value, int *status ) {
/*
*+
*  Name:
*     astReadDateTime

*  Purpose:
*     Read a date/time string.

*  Type:
*     Private function.

*  Synopsis:
*     #include "frame.h"
*     double astReadDateTime( const char *value )

*  Class Membership:
*     Frame member function.

*  Description:
*     This function reads a date/time string in a variety of formats and
*     returns the resulting time as a Modified Julian Date. If the string
*     cannot be interpreted as a date/time or contains invalid values, an
*     error is reported.

*  Parameters:
*     value
*        Pointer to a null terminated string containing the value to be read.

*  Returned Value:
*     The time as a Modified Julian date.

*  Date/Time Formats:
*     The date/time formats supported by this function are listed below. These
*     are interpreted in a case-insensitive manner and the function is
*     generally flexible about the presence of additional white space and the
*     use of alternative field delimiters.
*
*     Besselian Epoch
*        Expressed in decimal years, with or without decimal places
*        ("B1950" or "B1976.13", for example).
*     Julian Epoch
*        Expressed in decimal years, with or without decimal places
*        ("J2000" or "J2100.9", for example).
*     Year
*        Decimal years, with or without decimal places ("1996.8" for example).
*        Such values are interpreted as a Besselian epoch (see above) if less
*        than 1984.0 and as a Julian epoch otherwise.
*     Julian Date
*        With or without decimal places ("JD 2454321.9" for example).
*     Modified Julian Date
*        With or without decimal places ("MJD 54321.4" for example).
*     Gregorian Calendar Date
*        With the month expressed as an integer or 3-character
*        abbreviation, and with optional decimal places to represent a
*        fraction of a day ("1996-10-2" or "1996-Oct-2.6" for
*        example). If no fractional part of a day is given, the time
*        refers to the start of the day (zero hours).
*     Gregorian Date and Time
*        Any calendar date (as above) but with a fraction of a day expressed
*        as hours, minutes and seconds ("1996-Oct-2 12:13:56.985" for example).
*        The date and time can be separated by a space or by a "T" (as used
*        by ISO8601).

*  Notes:
*     -  The date/time value is interpreted as a calendar date and time, not
*     linked to any particular time system. Thus, interpretation of hours,
*     minutes and seconds is done in the obvious manner assuming 86400 seconds
*     in a day. No allowance for is made, for instance, for leap seconds or for
*     the varying length of a second in some time systems.
*     -  A value of AST__BAD is returned if this function is invoked with the
*     global error status set or if it should fail for any reason.
*-
*/

/* Local Vaiables: */
   char cmonth[ 4 ];             /* Buffer for name of month */
   char sep1[ 2 ];               /* Year/month separator string */
   char sep2[ 2 ];               /* Month/day separator string */
   char sep3[ 2 ];               /* Hour/minute separator string */
   char sep4[ 2 ];               /* Minute/second separator string */
   char *cc;                     /* Pointer to copy of remaining text */
   const char *v;                /* Pointer into value string */
   const char *p;                /* Pointer to date/time separator */
   double day;                   /* Day number plus fraction of whole day */
   double epoch;                 /* Epoch stored as decimal years */
   double hms;                   /* Hours, min & sec as fraction of a day */
   double jd;                    /* Julian Date */
   double mjd;                   /* Modified Julian Date */
   double result;                /* Result to be returned */
   double sec;                   /* Seconds and fractions of a second */
   int hour;                     /* Number of hours */
   int iday;                     /* Number of whole days */
   int l;                        /* Length of string remaining */
   int len;                      /* Length of string */
   int match;                    /* Date/time string has correct form? */
   int minute;                   /* Number of minutes */
   int month;                    /* Number of months */
   int nc;                       /* Number of characters read from string */
   int stat;                     /* Status return from SLALIB functions */
   int year;                     /* Number of years */

/* Check the global error status. */
   if ( !astOK ) return AST__BAD;

/* Initialise. */
   result = AST__BAD;

/* Obtain the length of the input string. */
   len = (int) strlen( value );

/* Attempt to read the string using each recognised format in turn. */

/* Besselian epoch in decimal years (e.g. "B1950.0"). */
/* ================================================== */
   if ( nc = 0,
        ( 1 == astSscanf( value, " %*1[Bb] %lf %n", &epoch, &nc ) )
        && ( nc >= len ) ) {

/* Convert to Modified Julian Date. */
      result = palSlaEpb2d( epoch );

/* Julian epoch in decimal years (e.g. "J2000.0"). */
/* =============================================== */
   } else if ( nc = 0,
               ( 1 == astSscanf( value, " %*1[Jj] %lf %n", &epoch, &nc ) )
               && ( nc >= len ) ) {

/* Convert to Modified Julian Date. */
      result = palSlaEpj2d( epoch );

/* Decimal years (e.g. "1976.2"). */
/* ============================== */
   } else if ( nc = 0,
               ( 1 == astSscanf( value, " %lf %n", &epoch, &nc ) )
               && ( nc >= len ) ) {

/* Convert to Modified Julian Date, treating the epoch as Julian or Besselian
   depending on whether it is 1984.0 or later. */
      result = ( epoch < 1984.0 ) ? palSlaEpb2d( epoch ) : palSlaEpj2d( epoch );

/* Modified Julian Date (e.g. "MJD 54321.0"). */
/* ============================================ */
   } else if ( nc = 0,
               ( 1 == astSscanf( value, " %*1[Mm] %*1[Jj] %*1[Dd] %lf %n",
                              &mjd, &nc ) ) && ( nc >= len ) ) {

/* Use the result directly. */
      result = mjd;

/* Julian Date (e.g. "JD 2454321.5"). */
/* ==================================== */
   } else if ( nc = 0,
               ( 1 == astSscanf( value, " %*1[Jj] %*1[Dd] %lf %n",
                              &jd, &nc ) ) && ( nc >= len ) ) {

/* Convert to Modified Julian Date. */
      result = jd - 2400000.5;

/* Gregorian calendar date (e.g. "1996-10-2" or "1996-Oct-2"). */
/* =========================================================== */
/* This format also allows day fractions expressed as decimal days, e.g:

      "1996-Oct-2.5001"

   or as hours, minutes and seconds, e.g:

      "1996-Oct-2 12:14:30.52"

   Various alternative field delimiters are also allowed. */
   } else {

/* Note that the method used to parse this format relies heavily on
   conditional execution controlled by "&&" and "||" operators. Initialise
   the variables used. */
      v = value;
      l = len;
      *cmonth = '\0';
      year = month = iday = hour = minute = 0;
      day = sec = 0.0;

/* Identify the year and month. */
/* ---------------------------- */
/* Try to match an initial " 1996 - 10 -" or " 1996 10 " or similar. */
      match =
         ( nc = 0, ( 4 == astSscanf( v, " %d %1[:/-] %2d %1[:/-]%n",
                                  &year, sep1, &month, sep2, &nc ) ) );
      match = match ||
         ( nc = 0, ( 4 == astSscanf( v, " %d%1[ ] %2d%1[ ]%n",
                                  &year, sep1, &month, sep2, &nc ) ) );

/* If that failed, allow " 1996 - Oct -" or " 1996 Oct " or similar. */
      match = match ||
         ( nc = 0, ( 4 == astSscanf( v,
                                  " %d %1[:/-] %3[ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                                  "abcdefghijklmnopqrstuvwxyz] %1[:/-]%n",
                                  &year, sep1, cmonth, sep2, &nc ) ) );
      match = match ||
         ( nc = 0, ( 4 == astSscanf( v,
                                  " %d%1[ ] %3[ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                                  "abcdefghijklmnopqrstuvwxyz]%1[ ]%n",
                                  &year, sep1, cmonth, sep2, &nc ) ) );

/* Alternative field separators are permitted above, but ensure that
   they are both the same. */
      match = match && ( *sep1 == *sep2 );

/* Identify the day and fraction of day. */
/*-------------------------------------- */
/* If the above matched correctly, modify the string pointer "v" to
   the next character to be interpreted and decrement the remaining
   string length. */
      if ( match ) {
         v += nc;
         l -= nc;

/* ISO8601 format uses the litter T as a delimiter between the date and time.
   If there is a T in the remaining string, take a copy and change the T to
   a space. */
         p = strchr( v, 'T' );
         if( p ) {
            cc = astStore( NULL, v, l + 1 );
            cc[ p - v ] = ' ';
            v = cc;
         } else {
            cc = NULL;
         }

/* We now try to match the following characters but without reading
   any values.  This is done to ensure the string has the correct form
   (e.g. exclude "-" signs and exponents in numbers, which are
   otherwise hard to detect). */

/* Try to match " 12.3456 " or similar. */
         match =
            ( nc = 0, ( 0 == astSscanf( v, " %*2[0123456789].%*[0123456789] %n",
                                     &nc ) )
                      && ( nc == l ) );

/* If that failed, then try to match " 12. " or similar. */
         match = match ||
            ( nc = 0, ( 0 == astSscanf( v, " %*2[0123456789]. %n", &nc ) )
                      && ( nc == l ) );

/* If that also failed, then try to match just " 12 " or similar. */
         match = match ||
            ( nc = 0, ( 0 == astSscanf( v, " %*2[0123456789] %n", &nc ) )
                      && ( nc == l ) );

/* If any of the above patterns matched, now read the data (the day number)
   as a double value. */
         if ( match ) {
            match = ( nc = 0, ( 1 == astSscanf( v, " %lf %n", &day, &nc ) )
                              && ( nc == l ) );

/* If none of the above matched, then look to see if the day fraction has been
   given in hours, minutes and seconds by trying to match " 12 03 : 45 :" or
   " 12 13 45 " or similar. */
         } else {
            match =
               ( nc = 0, ( 5 == astSscanf( v,
                                        " %2d%*1[ ] %2d %1[:/-] %2d %1[:/-]%n",
                                        &iday, &hour, sep3, &minute, sep4,
                                        &nc ) ) );
            match = match ||
               ( nc = 0, ( 5 == astSscanf( v, " %2d%*1[ ] %2d%1[ ] %2d%1[ ]%n",
                                        &iday, &hour, sep3, &minute, sep4,
                                        &nc ) ) );

/* Alternative field separators are permitted above, but ensure that
   they are both the same. */
            match = match && ( *sep3 == *sep4 );

/* If the day number was read as an integer, convert it to double. */
            if ( match ) day = (double) iday;

/* If no match, see if we can get a match without a trailing seconds field. */
            if( !match ) {
               match =
                  ( nc = 0, ( 4 == astSscanf( v,
                                        " %2d%*1[ ] %2d %1[:/-] %2d %n",
                                        &iday, &hour, sep3, &minute, &nc ) &&
                                        ( nc == l ) ) );
               match = match ||
                  ( nc = 0, ( 4 == astSscanf( v, " %2d%*1[ ] %2d%1[ ] %2d %n",
                                        &iday, &hour, sep3, &minute, &nc ) &&
                                        ( nc == l ) ) );

/* If the day number was read as an integer, convert it to double. */
               if ( match ) day = (double) iday;

/* Otherwise, identify the seconds field. */
/* -------------------------------------- */
/* If hours and minutes fields have been matched, now look for the
   final seconds (and fractions of seconds) field. This is similar to
   the day/fraction field (see earlier) in that we first check that it
   has the correct form before reading its value. */

/* Adjust the string pointer and remaining string length. */
            } else {
               v += nc;
               l -= nc;

/* Try to match " 12.3456 " or similar. */
               match =
                  ( nc = 0, ( 0 == astSscanf( v,
                                          " %*2[0123456789].%*[0123456789] %n",
                                           &nc ) )
                            && ( nc == l ) );

/* If that failed, then try to match " 12. " or similar. */
               match = match ||
                  ( nc = 0, ( 0 == astSscanf( v, " %*2[0123456789]. %n", &nc ) )
                            && ( nc == l ) );

/* If that also failed, then try to match just " 12 " or similar. */
               match = match ||
                  ( nc = 0, ( 0 == astSscanf( v, " %*2[0123456789] %n", &nc ) )
                            && ( nc == l ) );

/* If any of the above patterns matched, now read the data (the number of
   seconds) as a double value. */
               if ( match ) {
                  match = ( nc = 0, ( 1 == astSscanf( v, " %lf %n", &sec, &nc ) )
                                    && ( nc == l ) );
               }
            }
         }

/* Free resources */
         if( cc ) cc = astFree( cc );

      }

/* Interpret the values that were read. */
/* ------------------------------------ */
/* We execute this if all of the above text matching was successful,
   transferred the required number of data values, and consumed the
   entire input string. */
      if ( match ) {

/* See if the month was given as a character string (e.g. "Oct") instead of
   a number. If so, define local variables for use in converting it. */
         if ( *cmonth ) {
            char lcmonth[ 4 ];      /* Lower case copy of month string */
            const char *ptr;        /* Pointer result from look up */
            const char *table =     /* Month look up table */
                       "jan feb mar apr may jun jul aug sep oct nov dec";
            int i;                  /* Loop counter for characters */

/* Convert the month string to lower case. */
            for ( i = 0; cmonth[ i ]; i++ ) {
               lcmonth[ i ] = tolower( cmonth[ i ] );
            }
            lcmonth[ i ] = '\0';

/* Look the month up in the table of months and generate the required month
   number. */
            if ( ( ptr = strstr( table, lcmonth ) ) ) {
               month = 1 + ( ptr - table ) / 4;

/* If the lookup failed, report an error. */
   	    } else {
               astError( AST__DTERR, "Month value \"%s\" is invalid.", status,
                         cmonth );
            }
         }

/* If OK, extract the integral day number and convert years, months and days
   to a Modified Julian Date. */
         if ( astOK ) {
            iday = (int) day;
            palSlaCaldj( year, month, iday, &mjd, &stat );

/* Examine the return status from the conversion and report an appropriate
   error if necessary. */
            switch ( stat ) {
            case 1:
               astError( AST__DTERR, "Year value (%d) is invalid.", status, year );
               break;
            case 2:
               astError( AST__DTERR, "Month value (%d) is invalid.", status, month );
               break;
            case 3:
               astError( AST__DTERR, "Day value (%.*g) is invalid.", status, DBL_DIG,
                         day );
               break;

/* If conversion to MJD was successful, add any fractional part of a day to the
   result. */
            default:
               mjd += ( day - (double) iday );

/* Convert hours, minutes and seconds to a fraction of a day (this will give
   zero if none of these quantities was supplied). */
               palSlaDtf2d( hour, minute, sec, &hms, &stat );

/* Examine the return status from the conversion and report an appropriate
   error if necessary. */
               switch ( stat ) {
               case 1:
                  astError( AST__DTERR, "Hour value (%d) is invalid.", status, hour );
                  break;
               case 2:
                  astError( AST__DTERR, "Minute value (%d) is invalid.", status,
                            minute );
                  break;
               case 3:
                  astError( AST__DTERR, "Seconds value (%.*g) is invalid.", status,
                            DBL_DIG, sec );
                  break;

/* Add the fraction of a day derived from hours, minutes and seconds fields to
   the result. */
               default:
                  mjd += hms;
                  break;
               }
               break;
            }

/* Return the result, if no error occurred. */
            if ( astOK ) result = mjd;
         }

/* If none of the supported date/time formats matched, then report an error. */
      } else {
         astError( AST__DTERR, "Date/time does not have the correct form." , status);
      }
   }

/* Return the result. */
   return result;
}

static void ReportPoints( AstMapping *this_mapping, int forward,
                          AstPointSet *in_points, AstPointSet *out_points, int *status ) {
/*
*  Name:
*     ReportPoints

*  Purpose:
*     Report the effect of transforming a set of points using a Frame.

*  Type:
*     Private function.

*  Synopsis:
*     #include "mapping.h"
*     void ReportPoints( AstMapping *this, int forward,
*                        AstPointSet *in_points, AstPointSet *out_points, int *status )

*  Class Membership:
*     Frame member function (over-rides the protected astReportPoints
*     method inherited from the Mapping class).

*  Description:
*     This function reports the coordinates of a set of points before
*     and after being transformed by a Frame, by writing them to
*     standard output.

*  Parameters:
*     this
*        Pointer to the Frame.
*     forward
*        A non-zero value indicates that the Frame's forward
*        coordinate transformation has been applied, while a zero
*        value indicates the inverse transformation.
*     in_points
*        Pointer to a PointSet which is associated with the
*        coordinates of a set of points before the Frame was applied.
*     out_points
*        Pointer to a PointSet which is associated with the
*        coordinates of the same set of points after the Frame has
*        been applied.
*     status
*        Pointer to the inherited status variable.
*/

/* Local Variables: */
   AstFrame *this;               /* Pointer to the Frame structure */
   double **ptr_in;              /* Pointer to array of input data pointers */
   double **ptr_out;             /* Pointer to array of output data pointers */
   int coord;                    /* Loop counter for coordinates */
   int ncoord_in;                /* Number of input coordinates per point */
   int ncoord_out;               /* Number of output coordinates per point */
   int npoint;                   /* Number of points to report */
   int npoint_in;                /* Number of input points */
   int npoint_out;               /* Number of output points */
   int point;                    /* Loop counter for points */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain a pointer to the Frame structure. */
   this = (AstFrame *) this_mapping;

/* Obtain the numbers of points and coordinates associated with each
   PointSet. */
   npoint_in = astGetNpoint( in_points );
   npoint_out = astGetNpoint( out_points );
   ncoord_in = astGetNcoord( in_points );
   ncoord_out = astGetNcoord( out_points );

/* Obtain the pointers that give access to the coordinate data
   associated with each PointSet. */
   ptr_in = astGetPoints( in_points );
   ptr_out = astGetPoints( out_points );

/* In the event that both PointSets don't contain equal numbers of
   points (this shouldn't actually happen), simply use the minimum
   number. */
   npoint = ( npoint_in < npoint_out ) ? npoint_in : npoint_out;

/* Loop to report the effect of the transformation on each point in
   turn. */
   for ( point = 0; point < npoint; point++ ) {

/* Report the input coordinates (in parentheses and separated by
   commas). Format each value for display using the Frame's astFormat
   method. */
      printf( "(" );
      for ( coord = 0; coord < ncoord_in; coord++ ) {
         printf( "%s%s", coord ? ", " : "",
                 astFormat( this, coord, ptr_in[ coord ][ point ] ) );
      }

/* Similarly report the output coordinates. */
      printf( ") --> (" );
      for ( coord = 0; coord < ncoord_out; coord++ ) {
         printf( "%s%s", coord ? ", " : "",
                 astFormat( this, coord, ptr_out[ coord ][ point ] ) );
      }
      printf( ")\n" );
   }
}

static void Resolve( AstFrame *this, const double point1[],
                       const double point2[], const double point3[],
                       double point4[], double *d1, double *d2, int *status ){
/*
*++
*  Name:
c     astResolve
f     AST_RESOLVE

*  Purpose:
*     Resolve a vector into two orthogonal components

*  Type:
*     Public virtual function.

*  Synopsis:
c     #include "frame.h"
c     void astResolve( AstFrame *this, const double point1[],
c                      const double point2[], const double point3[],
c                      double point4[], double *d1, double *d2 );
f     CALL AST_RESOLVE( THIS, POINT1, POINT2, POINT3, POINT4, D1, D2,
f                       STATUS )

*  Class Membership:
*     Frame method.

*  Description:
c     This function resolves a vector into two perpendicular components.
f     This routine resolves a vector into two perpendicular components.
*     The vector from point 1 to point 2 is used as the basis vector.
*     The vector from point 1 to point 3 is resolved into components
*     parallel and perpendicular to this basis vector. The lengths of the
*     two components are returned, together with the position of closest
*     aproach of the basis vector to point 3.

*  Parameters:
c     this
f     THIS = INTEGER (Given)
*        Pointer to the Frame.
c     point1
f     POINT1( * ) = DOUBLE PRECISION (Given)
c        An array of double, with one element for each Frame axis
f        An array with one element for each Frame axis
*        (Naxes attribute). This marks the start of the basis vector,
*        and of the vector to be resolved.
c     point2
f     POINT2( * ) = DOUBLE PRECISION (Given)
c        An array of double, with one element for each Frame axis
f        An array with one element for each Frame axis
*        (Naxes attribute). This marks the end of the basis vector.
c     point3
f     POINT3( * ) = DOUBLE PRECISION (Given)
c        An array of double, with one element for each Frame axis
f        An array with one element for each Frame axis
*        (Naxes attribute). This marks the end of the vector to be
*        resolved.
c     point4
f     POINT4( * ) = DOUBLE PRECISION (Returned)
c        An array of double, with one element for each Frame axis
f        An array with one element for each Frame axis
*        in which the coordinates of the point of closest approach of the
*        basis vector to point 3 will be returned.
c     d1
f     D1 = DOUBLE PRECISION (Returned)
c        The address of a location at which to return the distance from
f        The distance from
*        point 1 to point 4 (that is, the length of the component parallel
*        to the basis vector). Positive values are in the same sense as
*        movement from point 1 to point 2.
c     d2
f     D2 = DOUBLE PRECISION (Returned)
c        The address of a location at which to return the distance from
f        The distance from
*        point 4 to point 3 (that is, the length of the component
*        perpendicular to the basis vector). The value is always positive.
f     STATUS = INTEGER (Given and Returned)
f        The global status.

*  Notes:
c     - Each vector used in this function is the path of
f     - Each vector used in this routine is the path of
*     shortest distance between two points, as defined by the
c     astDistance function.
f     AST_DISTANCE function.
*     - This function will return "bad" coordinate values (AST__BAD)
*     if any of the input coordinates has this value, or if the required
*     output values are undefined.
*--
*/

/* Local Variables: */
   double bv;                    /* Length of basis vector */
   double c;                     /* Component length */
   double dp;                    /* Dot product */
   int axis;                     /* Loop counter for axes */
   int naxes;                    /* Number of Frame axes */
   int ok;                       /* OK to proceed? */

/* Check the global error status. */
   *d1 = AST__BAD;
   *d2 = AST__BAD;
   if ( !astOK ) return;

/* Determine the number of Frame axes. */
   naxes = astGetNaxes( this );

/* Initialize bad values, and check if the supplied vectors are good. */
   ok = 1;
   for( axis = 0; axis < naxes; axis++ ){
      point4[ axis ] = AST__BAD;
      if( point1[ axis ] == AST__BAD ||
          point2[ axis ] == AST__BAD ||
          point3[ axis ] == AST__BAD ) ok = 0;
   }

/* Check the supplied values. */
   if ( ok ) {

/* Find the dot product of the basis vector with the vector joining point 1
   and point 3. At the same time form the squared length of the basis
   vector. */
      dp = 0.0;
      bv = 0.0;
      for( axis = 0; axis < naxes; axis++ ){
         c = point2[ axis ] - point1[ axis ];
         dp += c * ( point3[ axis ] - point1[ axis ] );
         bv += c * c;
      }

/* Check the basis vector does not have zero length, and convert the
   squared length into a length. */
      if( bv > 0.0 ) {
         bv = sqrt( bv );

/* The dot product is the required distance d1 multiplied by the length
   of the basis vector. Form the distance d1. */
         *d1 = dp/bv;

/* Offset away from point 1 towards point 2 by a distance of d1. */
         for( axis = 0; axis < naxes; axis++ ){
            point4[ axis ] = point1[ axis ] +
                             (*d1/bv)*( point2[ axis ] - point1[ axis ] );
         }

/* Finally, form the required length d2. */
         *d2 = 0.0;
         for( axis = 0; axis < naxes; axis++ ){
            c = ( point3[ axis ] - point4[ axis ] );
            *d2 += c*c;
         }
         *d2 = sqrt( *d2 );

      }
   }

   return;

}

static AstPointSet *ResolvePoints( AstFrame *this, const double point1[],
                                   const double point2[], AstPointSet *in,
                                   AstPointSet *out, int *status ) {
/*
*+
*  Name:
*     astResolvePoints

*  Purpose:
*     Resolve a set of vectors into orthogonal components

*  Type:
*     Protected virtual function.

*  Synopsis:
*     #include "frame.h"
*     AstPointSet *astResolvePoints( AstFrame *this, const double point1[],
*                                    const double point2[], AstPointSet *in,
*                                    AstPointSet *out )

*  Class Membership:
*     Frame method.

*  Description:
*     This function takes a Frame and a set of vectors encapsulated
*     in a PointSet, and resolves each one into two orthogonal components,
*     returning these two components in another PointSet.
*
*     This is exactly the same as the public astResolve method, except
*     that this method allows many vectors to be processed in a single call,
*     thus reducing the computational cost of overheads of many
*     individual calls to astResolve.

*  Parameters:
*     this
*        Pointer to the Frame.
*     point1
*        An array of double, with one element for each Frame axis
*        (Naxes attribute). This marks the start of the basis vector,
*        and of the vectors to be resolved.
*     point2
*        An array of double, with one element for each Frame axis
*        (Naxes attribute). This marks the end of the basis vector.
*     in
*        Pointer to the PointSet holding the ends of the vectors to be
*        resolved.
*     out
*        Pointer to a PointSet which will hold the length of the two
*        resolved components. A NULL value may also be given, in which
*        case a new PointSet will be created by this function.

*  Returned Value:
*     Pointer to the output (possibly new) PointSet. The first axis will
*     hold the lengths of the vector components parallel to the basis vector.
*     These values will be signed (positive values are in the same sense as
*     movement from point 1 to point 2. The second axis will hold the lengths
*     of the vector components perpendicular to the basis vector. These
*     values will be signed only if the Frame is 2-dimensional, in which
*     case a positive value indicates that rotation from the basis vector
*     to the tested vector is in the same sense as rotation from the first
*     to the second axis of the Frame.

*  Notes:
*     - The number of coordinate values per point in the input
*     PointSet must match the number of axes in the supplied Frame.
*     - If an output PointSet is supplied, it must have space for
*     sufficient number of points and 2 coordinate values per point.
*     - A null pointer will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*     - We assume flat geometry throughout this function. Other classes,
*     (e.g. SkyFrame) will override this method using more appropriate
*     geometry.
*-
*/

/* Local Variables: */
   AstPointSet *result;          /* Pointer to output PointSet */
   double **ptr_in;              /* Pointers to input axis values */
   double **ptr_out;             /* Pointers to returned axis values */
   double *basisv;               /* Pointer to array holding basis vector */
   double *d1;                   /* Pointer to next parallel component value */
   double *d2;                   /* Pointer to next perpendicular component value */
   double *ip;                   /* Pointer to next input axis value */
   double bv;                    /* Length of basis vector */
   double c;                     /* Constant value */
   double d;                     /* Component length */
   double dp;                    /* Dot product */
   double x1;                    /* First axis of basis vector */
   double x2;                    /* First axis of test vector */
   double y1;                    /* Second axis of basis vector */
   double y2;                    /* Second axis of test vector */
   int axis;                     /* Loop counter for axes */
   int ipoint;                   /* Index of next point */
   int nax;                      /* Number of Frame axes */
   int ncoord_in;                /* Number of input PointSet coordinates */
   int ncoord_out;               /* Number of coordinates in output PointSet */
   int npoint;                   /* Number of points to transform */
   int npoint_out;               /* Number of points in output PointSet */
   int ok;                       /* OK to proceed? */

/* Initialise. */
   result = NULL;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Obtain the number of axes in the Frame. */
   nax = astGetNaxes( this );

/* Obtain the number of input vectors to resolve and the number of coordinate
   values per vector. */
   npoint = astGetNpoint( in );
   ncoord_in = astGetNcoord( in );

/* If OK, check that the number of input coordinates matches the number
   required by the Frame. Report an error if these numbers do not match. */
   if ( astOK && ( ncoord_in != nax ) ) {
      astError( AST__NCPIN, "astResolvePoints(%s): Bad number of coordinate "
                "values (%d) in input %s.", status, astGetClass( this ), ncoord_in,
                astGetClass( in ) );
      astError( AST__NCPIN, "The %s given requires %d coordinate value(s) for "
                "each input point.", status, astGetClass( this ), nax );
   }

/* If still OK, and a non-NULL pointer has been given for the output PointSet,
   then obtain the number of points and number of coordinates per point for
   this PointSet. */
   if ( astOK && out ) {
      npoint_out = astGetNpoint( out );
      ncoord_out = astGetNcoord( out );

/* Check that the dimensions of this PointSet are adequate to accommodate the
   output coordinate values and report an error if they are not. */
      if ( astOK ) {
         if ( npoint_out < npoint ) {
            astError( AST__NOPTS, "astResolvePoints(%s): Too few points (%d) in "
                      "output %s.", status, astGetClass( this ), npoint_out,
                      astGetClass( out ) );
            astError( AST__NOPTS, "The %s needs space to hold %d transformed "
                      "point(s).", status, astGetClass( this ), npoint );
         } else if ( ncoord_out < 2 ) {
            astError( AST__NOCTS, "astResolvePoints(%s): Too few coordinate "
                      "values per point (%d) in output %s.", status,
                      astGetClass( this ), ncoord_out, astGetClass( out ) );
            astError( AST__NOCTS, "The %s supplied needs space to store 2 "
                      "coordinate value(s) per transformed point.", status,
                      astGetClass( this ) );
         }
      }
   }

/* If all the validation stages are passed successfully, and a NULL output
   pointer was given, then create a new PointSet to encapsulate the output
   coordinate data. */
   if ( astOK ) {
      if ( !out ) {
         result = astPointSet( npoint, 2, "", status );

/* Otherwise, use the PointSet supplied. */
      } else {
         result = out;
      }
   }

/* Get pointers to the input and output axis values */
   ptr_in = astGetPoints( in );
   ptr_out = astGetPoints( result );

/* Store points to the first two axis arrays in the returned PointSet. */
   d1 = ptr_out[ 0 ];
   d2 = ptr_out[ 1 ];

/* Allocate work space. */
   basisv = astMalloc( sizeof( double )*(size_t) nax );

/* If the Frame has only one axis, then the supplied basic vector is
   irrelevant - the returned perpendicular distances are always zero and
   the returned parallel distances are just the distances from point1
   to each input point. */
   if( nax < 2 && basisv ) {
      ip = ptr_in[ 0 ];
      for( ipoint = 0; ipoint < npoint; ipoint++, d1++, d2++, ip++ ) {
         *d1 = astAxDistance( this, 1, point1[0], *ip );
         *d2 = 0.0;
      }

/* Now deal with Frames which have 2 or more axes */
   } else if( basisv ){

/* Check if the supplied positions defining the basis vector are good.
   Store the basis vector, and get its squared length. */
      ok = 1;
      bv = 0.0;
      for( axis = 0; axis < nax; axis++ ){
         if( point1[ axis ] == AST__BAD ||
             point2[ axis ] == AST__BAD ) {
            ok = 0;
            break;
         } else {
            basisv[ axis ] =  point2[ axis ] - point1[ axis ];
            bv += basisv[ axis ]*basisv[ axis ];
         }
      }

/* Check the basis vector does not have zero length, and convert the
   squared length into a length. */
      if( ok && bv > 0.0 ) {
         bv = sqrt( bv );
      } else {
         ok = 0;
      }

/* Store points to the first two axis arrays in the returned PointSet. */
      d1 = ptr_out[ 0 ];
      d2 = ptr_out[ 1 ];

/* Check supplied values can be used */
      if( ok ) {

/* Loop round each supplied vector. */
         for( ipoint = 0; ipoint < npoint; ipoint++, d1++, d2++ ) {

/* Find the dot product of the basis vector with the vector joining point 1
   and the end of the current vector. */
            ok = 1;
            dp = 0.0;
            for( axis = 0; axis < nax; axis++ ){
               d = ptr_in[ axis ][ ipoint ] - point1[ axis ];
               if( d != AST__BAD ) {
                  dp += basisv[ axis ] * d;
               } else {
                  ok = 0;
                  break;
               }
            }

/* If this input position is good... */
            if( ok ) {

/* The dot product is the required parallel component length multiplied by the
   length of the basis vector. Form the distance d1. */
               *d1 = dp/bv;

/* Offset away from point 1 towards point 2 by a distance of d1, and form the
   required length d2. */
               c = *d1/bv;
               if( nax > 2 ) {
                  *d2 = 0.0;
                  for( axis = 0; axis < nax; axis++ ){
                     d = ptr_in[ axis ][ ipoint ] -
                         ( point1[ axis ] + c*basisv[ axis ] );
                     *d2 += d*d;
                  }
                  *d2 = sqrt( *d2 );

/* If the Frame is 2 dimensional, we can give a sign the the perpendicular
   component. */
               } else {
                  x1 = c*basisv[ 0 ];
                  y1 = c*basisv[ 1 ];
                  x2 = ptr_in[ 0 ][ ipoint ] - ( point1[ 0 ] + x1 );
                  y2 = ptr_in[ 1 ][ ipoint ] - ( point1[ 1 ] + y1 );
                  *d2 = sqrt( x2*x2 + y2*y2 );
                  if( x1*y2 - x2*y1 < 0.0 ) *d2 = -(*d2);
               }

/* If this input vector is bad, put bad values in the output */
            } else {
               *d1 = AST__BAD;
               *d2 = AST__BAD;
            }
         }

/* If supplied values cannot be used, fill the returned PointSet with bad
   values */
      } else {
         for( ipoint = 0; ipoint < npoint; ipoint++, d1++, d2++ ) {
            *d1 = AST__BAD;
            *d2 = AST__BAD;
         }
      }
   }

/* Free resources */
   basisv = astFree( basisv );

/* Annul the returned PointSet if an error occurred. */
   if( !astOK ) result = astAnnul( result );

/* Return a pointer to the output PointSet. */
   return result;
}

static void SetActiveUnit( AstFrame *this, int value, int *status ){
/*
*++
*  Name:
c     astSetActiveUnit
f     AST_SETACTIVEUNIT

*  Purpose:
*     Specify how the Unit attribute should be used.

*  Type:
*     Public virtual function.

*  Synopsis:
c     #include "frame.h"
c     void astSetActiveUnit( AstFrame *this, int value )
f     CALL AST_SETACTIVEUNIT( THIS, VALUE, STATUS )

*  Class Membership:
*     Frame method.

*  Description:
c     This function
f     This routine
*     sets the current value of the ActiveUnit flag for a Frame, which
*     controls how the Frame behaves when it is used (by
c     astFindFrame or astConvert)
f     AST_FINDFRAME or AST_CONVERT)
*     to match another Frame. If the ActiveUnit flag is set in both
*     template and target Frames then the returned Mapping takes into account
*     any differences in axis units. The default value for simple Frames is
*     zero, which preserves the behaviour of versions of AST prior to
*     version 2.0.
*
*     If the ActiveUnit flag of either Frame is
c     zero,
f     .FALSE.,
*     then the Mapping will ignore any difference in the Unit attributes of
*     corresponding template and target axes. In this mode, the Unit
*     attributes are purely descriptive commentary for the benefit of
*     human readers and do not influence the Mappings between Frames.
*     This is the behaviour which all Frames had in older version of AST,
*     prior to the introduction of this attribute.
*
*     If the ActiveUnit flag of both Frames is
c     non-zero,
f     .TRUE.,
*     then the Mapping from template to target will take account of any
*     difference in the axis Unit attributes, where-ever possible. For
*     instance, if corresponding target and template axes have Unit strings of
*     "km" and "m", then the FrameSet class will use a ZoomMap to connect
*     them which introduces a scaling of 1000. If no Mapping can be found
*     between the corresponding units string, then an error is reported.
*     In this mode, it is assumed that values of the Unit attribute conform
*     to the syntax for units strings described in the FITS WCS Paper I
*     "Representations of world coordinates in FITS" (Greisen & Calabretta).
*     Particularly, any of the named unit symbols, functions, operators or
*     standard multiplier prefixes listed within that paper can be used within
*     a units string. A units string may contain symbols for unit which are
*     not listed in the FITS paper, but transformation to any other units
*     will then not be possible (except to units which depend only on the
*     same unknown units - thus "flops" can be transformed to "Mflops"
*     even though "flops" is not a standard FITS unit symbol).
*
*     A range of common non-standard variations of unit names and multiplier
*     prefixes are also allowed, such as adding an "s" to the end of Angstrom,
*     using a lower case "a" at the start of "angstrom", "micron" instead of
*     "um", "sec" instead of "s", etc.
*
c     If the ActiveUnit flag is non-zero, setting a new Unit value for an
f     If the ActiveUnit flag is .TRUE., setting a new Unit value for an
*     axis may also change its Label and Symbol attributes. For instance, if
*     an axis has Unit "Hz" and Label "frequency", then changing its Unit to
*     "log(Hz)" will change its Label to "log( frequency )". In addition,
*     the Axis Format attribute will be cleared when-ever a new value
*     is assigned to the Unit attribute.
*
c     Note, if a non-zero value is set for the ActiveUnit flag, then changing a
f     Note, if a .TRUE. value is set for the ActiveUnit flag, then changing a
*     Unit value for the current Frame within a FrameSet will result in the
*     Frame being re-mapped (that is, the Mappings which define the
*     relationships between Frames within the FrameSet will be modified to
*     take into account the change in Units).

*  Parameters:
c     this
f     THIS = INTEGER (Given)
*        Pointer to the Frame.
c     value
f     VALUE = LOGICAL (Given)
*        The new value to use.
f     STATUS = INTEGER (Given and Returned)
f        The global status.

*  Applicability:
*     SkyFrame
c        The ActiveUnit flag for a SkyFrame is always 0 (any value
c        supplied using this function is ignored).
f        The ActiveUnit flag for a SkyFrame is always .FALSE. (any value
f        supplied using this routine is ignored).
*     SpecFrame
c        The ActiveUnit flag for a SpecFrame is always 1 (any value
c        supplied using this function is ignored).
f        The ActiveUnit flag for a SpecFrame is always .TRUE. (any value
f        supplied using this routine is ignored).
*     FluxFrame
c        The ActiveUnit flag for a FluxFrame is always 1 (any value
c        supplied using this function is ignored).
f        The ActiveUnit flag for a FluxFrame is always .TRUE. (any value
f        supplied using this routine is ignored).
*     CmpFrame
c        The default ActiveUnit flag for a CmpFrame is 1 if both of the
c        component Frames are using active units, and zero otherwise. When
f        The default ActiveUnit flag for a CmpFrame is .TRUE. if both of the
f        component Frames are using active units, and .FALSE. otherwise. When
*        a new value is set for the ActiveUnit flag, the flag value
*        is propagated to the component Frames. This change will be
*        reflected through all references to the component Frames, not
*        just those encapsulated within the CmpFrame.
*     Region:
*        Regions always use active units if possible.

*  Notes:
*     - The ActiveUnit flag resembles a Frame attribute, except that it
*     cannot be tested or cleared, and it cannot be accessed using the
c     generic astGet<X> and astSet<X> functions.
f     generic AST_GET<X> and AST_SET<X> routines.
c     - The astGetActiveUnit function can be used to retrieve the current
f     - The AST_GETACTIVEUNIT routine can be used to retrieve the current
*     value of the ActiveUnit flag.

*--
*/

/* Check the global error status. */
   if ( !astOK ) return;

/* Store a value of 1 for the Frame component if the supplied value is
   non-zero. */
   this->active_unit = ( value ) ? 1 : 0;
}

static void SetAttrib( AstObject *this_object, const char *setting, int *status ) {
/*
*  Name:
*     SetAttrib

*  Purpose:
*     Set an attribute value for a Frame.

*  Type:
*     Private function.

*  Synopsis:
*     #include "frame.h"
*     void SetAttrib( AstObject *this, const char *setting, int *status )

*  Class Membership:
*     Frame member function (over-rides the astSetAttrib method inherited
*     from the Mapping class).

*  Description:
*     This function assigns an attribute value for a Frame, the
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
*        Pointer to the Frame.
*     setting
*        Pointer to a null terminated string specifying the new attribute
*        value.
*     status
*        Pointer to the inherited status variable.

*  Notes:
*     - This function uses one-based axis numbering so that it is
*     suitable for external (public) use.
*/

/* Local Vaiables: */
   AstAxis *ax;                  /* Pointer to Axis */
   AstFrame *pfrm;               /* Pointer to primary Frame containing axis */
   AstFrame *this;               /* Pointer to the Frame structure */
   AstSystemType system_code;    /* System code */
   char pfrm_attrib[ 100 ];      /* Primary Frame attribute */
   char *pfrm_setting;           /* Primary Frame attribute */
   char *axis_setting;           /* Pointer to axis attribute setting string */
   const char *equals;           /* Pointer to equals sign */
   const char *old_setting;      /* Pointer to supplied setting string */
   const char *op;               /* Pointer to opening parenthesis */
   double dval;                  /* Double attibute value */
   double mjd;                   /* Epoch as a Modified Julian Date */
   int axis;                     /* Index for the Frame axis */
   int axis_nc;                  /* No. characters in axis attribute name */
   int axis_value;               /* Offset of value to be assigned to axis */
   int digits;                   /* Number of digits of precision */
   int direction;                /* Axis direction flag */
   int domain;                   /* Offset of Domain string */
   int epoch;                    /* Offset of Epoch string */
   int format;                   /* Offset of axis Format string */
   int free_axis_setting;        /* Should axis_setting be freed? */
   int has_axis;                 /* Does setting include an axis specifier? */
   int ival;                     /* Integer attribute value */
   int label;                    /* Offset of axis Label string */
   int len;                      /* Length of setting string */
   int match_end;                /* Match final axes of target? */
   int max_axes;                 /* Maximum number of axes matched */
   int min_axes;                 /* Minimum number of axes matched */
   int nc;                       /* Number of characters read by astSscanf */
   int off2;                     /* Modified offset of attribute value */
   int off;                      /* Offset of attribute value */
   int oldrep;                   /* Original error reporting state */
   int paxis;                    /* Axis index within primary frame */
   int permute;                  /* Permute axes in order to match? */
   int preserve_axes;            /* Preserve matched target axes? */
   int sign;                     /* Sign of longitude value */
   int symbol;                   /* Offset of axis Symbol string */
   int system;                   /* Offset of System string */
   int title;                    /* Offset of Title string */
   int unit;                     /* Offset of axis Unit string */
   int used;                     /* Could the setting string be used? */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain a pointer to the Frame structure. */
   this = (AstFrame *) this_object;

/* Find the offset to the first equal sign in the setting string. */
   equals = strchr( setting, '=' );

/* Set a flag indicating if the attribute name includes an axis
   specifier. */
   op = strchr( setting, '(' );
   has_axis = ( !op || op > equals ) ? 0 : 1;

/* A flag indicating that we do not need to free the axis_setting memory. */
   free_axis_setting = 0;

/* Initialise things to avoid compiler warnings. */
   axis_setting = NULL;
   old_setting = NULL;

/* Jump back to here if we are trying the same attribute setting but with
   an explicit axis "(1)" added to the attribute name. */
L1:

/* Obtain the length of the setting string. */
   len = strlen( setting );

/* Test for each recognised attribute in turn, using "astSscanf" to parse the
   setting string and extract the attribute value (or an offset to it in the
   case of string values). In each case, use the value set in "nc" to check
   that the entire string was matched. Once a value has been obtained, use the
   appropriate method to set it. */

/* Digits. */
/* ------- */
   if ( nc = 0,
        ( 1 == astSscanf( setting, "digits= %d %n", &digits, &nc ) )
        && ( nc >= len ) ) {
      astSetDigits( this, digits );

/* Digits(axis). */
/* ------------- */
   } else if ( nc = 0,
               ( 2 == astSscanf( setting, "digits(%d)= %d %n",
                              &axis, &digits, &nc ) )
               && ( nc >= len ) ) {

/* There is no function to set the Digits attribute value for an axis
   directly, so obtain a pointer to the Axis and use this to set the
   attribute. */
      (void) astValidateAxis( this, axis - 1, "astSetDigits(axis)" );
      ax = astGetAxis( this, axis - 1 );
      astSetAxisDigits( ax, digits );
      ax = astAnnul( ax );

/* Direction(axis). */
/* ---------------- */
   } else if ( nc = 0,
               ( 2 == astSscanf( setting, "direction(%d)= %d %n",
                              &axis, &direction, &nc ) )
               && ( nc >= len ) ) {
      astSetDirection( this, axis - 1, direction );

/* Epoch. */
/* ------ */
   } else if ( nc = 0,
        ( 0 == astSscanf( setting, "epoch=%n%*[^\n]%n", &epoch, &nc ) )
        && ( nc >= len ) ) {

/* Convert the Epoch value to a Modified Julian Date before use. */
      mjd = astReadDateTime( setting + epoch );
      if ( astOK ) {
         astSetEpoch( this, mjd );

/* Report contextual information if the conversion failed. */
      } else {
         astError( AST__ATTIN, "astSetAttrib(%s): Invalid epoch value "
                   "\"%s\" given for coordinate system.", status,
                   astGetClass( this ), setting + epoch );
      }

/* Top(axis). */
/* ---------- */
   } else if ( nc = 0,
               ( 2 == astSscanf( setting, "top(%d)= %lg %n",
                              &axis, &dval, &nc ) )
               && ( nc >= len ) ) {
      astSetTop( this, axis - 1, dval );

/* Bottom(axis). */
/* ------------- */
   } else if ( nc = 0,
               ( 2 == astSscanf( setting, "bottom(%d)= %lg %n",
                              &axis, &dval, &nc ) )
               && ( nc >= len ) ) {
      astSetBottom( this, axis - 1, dval );

/* Domain. */
/* ------- */
   } else if ( nc = 0,
               ( 0 == astSscanf( setting, "domain=%n%*[^\n]%n", &domain, &nc ) )
               && ( nc >= len ) ) {
      astSetDomain( this, setting + domain );

/* Format(axis). */
/* ------------- */
   } else if ( nc = 0,
               ( 1 == astSscanf( setting, "format(%d)=%n%*[^\n]%n",
                              &axis, &format, &nc ) )
               && ( nc >= len ) ) {
      astSetFormat( this, axis - 1, setting + format );

/* Label(axis). */
/* ------------ */
   } else if ( nc = 0,
               ( 1 == astSscanf( setting, "label(%d)=%n%*[^\n]%n",
                              &axis, &label, &nc ) )
               && ( nc >= len ) ) {
      astSetLabel( this, axis - 1, setting + label );

/* MatchEnd. */
/* --------- */
   } else if ( nc = 0,
               ( 1 == astSscanf( setting, "matchend= %d %n", &match_end, &nc ) )
               && ( nc >= len ) ) {
      astSetMatchEnd( this, match_end );

/* MaxAxes. */
/* -------- */
   } else if ( nc = 0,
               ( 1 == astSscanf( setting, "maxaxes= %d %n", &max_axes, &nc ) )
               && ( nc >= len ) ) {
      astSetMaxAxes( this, max_axes );

/* MinAxes. */
/* -------- */
   } else if ( nc = 0,
               ( 1 == astSscanf( setting, "minaxes= %d %n", &min_axes, &nc ) )
               && ( nc >= len ) ) {
      astSetMinAxes( this, min_axes );

/* Permute. */
/* -------- */
   } else if ( nc = 0,
               ( 1 == astSscanf( setting, "permute= %d %n", &permute, &nc ) )
               && ( nc >= len ) ) {
      astSetPermute( this, permute );

/* PreserveAxes. */
/* ------------- */
   } else if ( nc = 0,
               ( 1 == astSscanf( setting, "preserveaxes= %d %n",
                              &preserve_axes, &nc ) )
               && ( nc >= len ) ) {
      astSetPreserveAxes( this, preserve_axes );

/* Symbol(axis). */
/* ------------- */
   } else if ( nc = 0,
               ( 1 == astSscanf( setting, "symbol(%d)=%n%*[^\n]%n",
                              &axis, &symbol, &nc ) )
               && ( nc >= len ) ) {
      astSetSymbol( this, axis - 1, setting + symbol );

/* AlignSystem. */
/* ------------ */
   } else if ( nc = 0,
               ( 0 == astSscanf( setting, "alignsystem= %n%*s %n", &system, &nc ) )
               && ( nc >= len ) ) {

/* Convert the string to a System code before use. */
      system_code = astSystemCode( this, system + setting );
      if ( system_code != AST__BADSYSTEM ) {
         astSetAlignSystem( this, system_code );

/* Report an error if the string value wasn't recognised. */
      } else {
         astError( AST__ATTIN,
                   "astSetAttrib(%s): Invalid AlignSystem description \"%s\".", status,
                   astGetClass( this ), system + setting );
      }

/* System. */
/* ------- */
   } else if ( nc = 0,
               ( 0 == astSscanf( setting, "system= %n%*s %n", &system, &nc ) )
               && ( nc >= len ) ) {

/* Convert the string to a System code before use. */
      system_code = astSystemCode( this, system + setting );
      if ( system_code != AST__BADSYSTEM ) {
         astSetSystem( this, system_code );

/* Report an error if the string value wasn't recognised. */
      } else {
         astError( AST__ATTIN,
                   "astSetAttrib(%s): Invalid System description \"%s\".", status,
                   astGetClass( this ), system + setting );
      }

/* Title. */
/* ------ */
   } else if ( nc = 0,
               ( 0 == astSscanf( setting, "title=%n%*[^\n]%n", &title, &nc ) )
               && ( nc >= len ) ) {
      astSetTitle( this, setting + title );

/* Unit(axis). */
/* ----------- */
   } else if ( nc = 0,
               ( 1 == astSscanf( setting, "unit(%d)=%n%*[^\n]%n",
                              &axis, &unit, &nc ) )
               & ( nc >= len ) ) {
      astSetUnit( this, axis - 1, setting + unit );

/* ObsLat. */
/* ------- */
   } else if ( nc = 0,
              ( 0 == astSscanf( setting, "obslat=%n%*s %n", &off, &nc ) )
              && ( nc >= 7 ) ) {

/* If the first character in the value string is "N" or "S", remember the
   sign of the value and skip over the sign character. Default is north
   (+ve). */
      off2 = off;
      if( setting[ off ] == 'N' || setting[ off ] == 'n' ) {
         off2++;
         sign = +1;
      } else if( setting[ off ] == 'S' || setting[ off ] == 's' ) {
         off2++;
         sign = -1;
      } else {
         sign = +1;
      }

/* If not already created, create an FK5 J2000 SkyFrame which will be used
   for formatting and unformatting ObsLon and ObsLat values. */
      if( !skyframe ) {
         astBeginPM;
         skyframe = astSkyFrame( "system=FK5,equinox=J2000,format(2)=dms.2", status );
         astEndPM;
      }

/* Convert the string to a radians value before use. */
      ival = astUnformat( skyframe, 1, setting + off2, &dval );
      if ( ival == astChrLen( setting ) - off2  ) {
         astSetObsLat( this, dval*sign );

/* Report an error if the string value wasn't recognised. */
      } else {
         astError( AST__ATTIN, "astSetAttrib(%s): Invalid value for "
                   "ObsLat (observers latitude) \"%s\".", status, astGetClass( this ),
                   setting + off );
      }

/* ObsLon. */
/* ------- */
   } else if ( nc = 0,
              ( 0 == astSscanf( setting, "obslon=%n%*s %n", &off, &nc ) )
              && ( nc >= 7 ) ) {

/* If the first character in the value string is "E" or "W", remember the
   sign of the value and skip over the sign character. Default is east
   (+ve). */
      off2 = off;
      if( setting[ off ] == 'E' || setting[ off ] == 'e' ) {
         off2++;
         sign = +1;
      } else if( setting[ off ] == 'W' || setting[ off ] == 'w' ) {
         off2++;
         sign = -1;
      } else {
         sign = +1;
      }

/* If not already created, create an FK5 J2000 SkyFrame which will be used
   for formatting and unformatting ObsLon and ObsLat values. */
      if( !skyframe ) {
         astBeginPM;
         skyframe = astSkyFrame( "system=FK5,equinox=J2000,format(2)=dms.2", status );
         astEndPM;
      }

/* Convert the string to a radians value before use. */
      ival = astUnformat( skyframe, 1, setting + off2, &dval );
      if ( ival == astChrLen( setting ) - off2  ) {
         astSetObsLon( this, dval*sign );

/* Report an error if the string value wasn't recognised. */
      } else {
         astError( AST__ATTIN, "astSetAttrib(%s): Invalid value for "
                   "ObsLon (observers longitude) \"%s\".", status, astGetClass( this ),
                   setting + off );
      }

/* ObsAlt. */
/* ------- */
   } else if ( nc = 0,
        ( 1 == astSscanf( setting, "obsalt= %lg %n", &dval, &nc ) )
        && ( nc >= len ) ) {
      astSetObsAlt( this, dval );

/* Dut1. */
/* ---- */
   } else if ( nc = 0,
        ( 1 == astSscanf( setting, "dut1= %lg %n", &dval, &nc ) )
        && ( nc >= len ) ) {
      astSetDut1( this, dval );


/* Read-only attributes. */
/* --------------------- */
/* Define a macro to see if the setting string matches any of the
   read-only attributes of this class. */
#define MATCH(attrib) \
        ( nc = 0, ( 0 == astSscanf( setting, attrib "=%*[^\n]%n", &nc ) ) && \
                  ( nc >= len ) )

/* Use this macro to report an error if a read-only attribute has been
   specified. */
   } else if ( MATCH( "naxes" ) ||
               !strncmp( setting, "normunit", 8 ) ) {
      astError( AST__NOWRT, "astSet: The setting \"%s\" is invalid for a %s.", status,
                setting, astGetClass( this ) );
      astError( AST__NOWRT, "This is a read-only attribute." , status);

/* Other axis attributes. */
/* ---------------------- */
/* If the attribute was not identified above, but appears to refer to
   a Frame axis, then it may refer to an Axis object of a derived type
   (which has additional attributes not recognised here). */
   } else if ( !free_axis_setting && ( nc = 0,
               ( 1 == astSscanf( setting, "%*[^()]%n(%d)%n=%*[^\n]%n",
                                       &axis_nc, &axis, &axis_value, &nc ) )
               && ( nc >= len ) ) ) {

/* Validate the axis index and copy the attribute setting string. */
      (void) astValidateAxis( this, axis - 1, "astSet" );
      axis_setting = astString( setting, len );
      if ( astOK ) {

/* Over-write the axis index in the copy with the value to be
   assigned. */
         (void) strcpy( axis_setting + axis_nc, setting + axis_value );

/* Obtain a pointer to the Axis object. */
         ax = astGetAxis( this, axis - 1 );
         if( astOK ) {

/* Assume that we will be able to use the setting. */
            used = 1;

/* Temporarily switch off error reporting so that if the following attempt
   to access the axis attribute fails, we can try to interpret the
   attribute name as an attribute of the primary Frame containing the
   specified axis. Any errors reported in this context will simply be
   ignored, in particularly they are not deferred for later delivery. */
            oldrep = astReporting( 0 );

/* Use the Axis astSetAttrib method
   to set the value. */
            astSetAttrib( ax, axis_setting );

/* If the above call failed with a status of AST__BADAT, indicating that
   the attribute name was not recognised, clear the status so that we can
   try to interpret the attribute name as an attribute of the primary Frame
   containing the specified axis. */
            if( astStatus == AST__BADAT ) {
               astClearStatus;

/* Find the primary Frame containing the specified axis. */
               astPrimaryFrame( this, axis - 1, &pfrm, &paxis );

/* Only attempt to use the primary Frame if it is not the same as "this"
   - otherwise we could end up in an infinite loop. */
               if( pfrm != this ) {

/* Modify the attribute name to refer to the axis numbering of the
   primary frame. */
                  sprintf( pfrm_attrib, "%.*s(%d)", axis_nc, setting, paxis + 1 );

/* Create a setting string in which the attribute name refers to the axis
   numbering of the primary frame. */
                  pfrm_setting = NULL;
                  nc = 0;
                  pfrm_setting = astAppendString( pfrm_setting, &nc, pfrm_attrib );
                  pfrm_setting = astAppendString( pfrm_setting, &nc, setting + axis_value );

/* Attempt to set the attribute within the primary Frame. */
                  astSetAttrib( pfrm, pfrm_setting );

/* Free the memory. */
                  pfrm_setting = astFree( pfrm_setting );

/* If this failed, clear the status and indicate that we have not managed to
   use the attribute setting. */
                  if( !astOK ) {
                     astClearStatus;
                     used = 0;
                  }

               } else {
                  used = 0;
               }

/* If not found attempt to set the attribute value in the Axis, omitting
   the axis index. */
               if( ! used ) {
                  astSetAttrib( pfrm, axis_setting );
                  if( !astOK ) {
                     astClearStatus;
                  } else {
                     used = 1;
                  }
               }

/* Free the setting string, and annul the primary Frame pointer. */
               pfrm = astAnnul( pfrm );
            }

/* Re-instate the original error reporting state. */
            astReporting( oldrep );

/* If we could not use the setting, attempt to set the axis attribute again,
   this time retaining the error report. This is done to ensure the user
   gets an appropriate error message. */
            if( !used ) astSetAttrib( ax, axis_setting );
         }

/* Annul the Axis pointer and free the memory holding the attribute
   setting. */
         ax = astAnnul( ax );
      }
      axis_setting = astFree( axis_setting );

/* Not recognised. */
/* --------------- */
/* If the attribute is still not recognised, and the Frame has only 1 axis,
   and the attribute name does not already include an axis specifier, try
   again after appending "(1)" to the end of the attribute name. */
   } else if( !has_axis && astGetNaxes( this ) == 1 && equals ) {

/* Take a copy of the supplied setting, allowing 3 extra characters for the
   axis specifier "(1)". */
      axis_setting = astMalloc( len + 4 );
      if( axis_setting ) memcpy( axis_setting, setting, len );

/* Indicate we should free the axis_setting memory. */
      free_axis_setting = 1;

/* Add in the axis specifier. */
      strcpy( axis_setting + ( equals - setting ), "(1)" );

/* Add in the equals sign and attribute value. */
      strcpy( axis_setting + ( equals - setting ) + 3, equals );

/* Use the new setting instead of the supplied setting. */
      old_setting = setting;
      setting = axis_setting;

/* Indicate the setting now has an axis specifier. */
      has_axis = 1;

/* Jump back to try interpreting the new setting string. */
      goto L1;

/* Not recognised. */
/* --------------- */
/* If the attribute is still not recognised, pass it on to the parent
   method for further interpretation. First re-instate the original setting
   string if it was changed above. */
   } else {
      if( free_axis_setting ) {
         setting = old_setting;
         axis_setting = astFree( axis_setting );
         free_axis_setting = 0;
      }
      (*parent_setattrib)( this_object, setting, status );
   }

   if( free_axis_setting ) axis_setting = astFree( axis_setting );

/* Undefine macros local to this function. */
#undef MATCH
}

static void SetAxis( AstFrame *this, int axis, AstAxis *newaxis, int *status ) {
/*
*+
*  Name:
*     astSetAxis

*  Purpose:
*     Set a new Axis for a Frame.

*  Type:
*     Protected virtual function.

*  Synopsis:
*     #include "frame.h"
*     void astSetAxis( AstFrame *this, int axis, AstAxis *newaxis )

*  Class Membership:
*     Frame method.

*  Description:
*     This function allows a new Axis object to be associated with one
*     of the axes of a Frame, replacing the previous one. Each Axis
*     object contains a description of the quantity represented along
*     one of the Frame's axes, so this function allows this
*     description to be exchanged for another one.

*  Parameters:
*     this
*        Pointer to the Frame.
*     axis
*        The index (zero-based) of the axis whose associated Axis object is to
*        be replaced.
*     newaxis
*        Pointer to the new Axis object.
*-
*/

/* Check the global error status. */
   if ( !astOK ) return;

/* Validate and permute the axis index supplied. */
   axis = astValidateAxis( this, axis, "astSetAxis" );

/* If OK, annul the Frame's pointer to the old Axis object and clone a pointer
   to the new one to replace it. */
   if ( astOK ) {
      this->axis[ axis ] = astAnnul( this->axis[ axis ] );
      this->axis[ axis ] = astClone( newaxis );
   }
}

static void SetFrameFlags( AstFrame *this, int flags, int *status ){
/*
*+
*  Name:
*     astSetFrameFlags

*  Purpose:
*     Store a new bit mask of flags in a Frame.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "frame.h"
*     void astSetFrameFlags( astFrame *this, int flags )

*  Class Membership:
*     Frame member function.

*  Description:
*     This function stores a new set of flags in a Frame. The flags can
*     be retrieved using astGetFrameFlags.

*  Parameters:
*     this
*        The Frame.
*     flags
*        A bit mask holding the flags. Currently, the following bits are
*        used:
*
*        0 - Used to indicate if the Frame is currently involved in an
*        attempt to restore the integrity of a FrameSet following
*        changes to the attribute values of the Frame.

*-
*/

/* Check the global error status. */
   if ( !astOK ) return;

/* Assign the new bit mask. */
   this->flags = flags;
}

static void SetUnit( AstFrame *this, int axis, const char *unit, int *status ) {
/*
*  Name:
*     SetUnit

*  Purpose:
*     Set a value for the Unit attribute of a Frame.

*  Type:
*     Protected virtual function.

*  Synopsis:
*     #include "frame.h"
*     void SetUnit( AstFrame *this, int axis, const char *unit, int *status )

*  Class Membership:
*     Frame method.

*  Description:
*     This function sets the Unit value for a Frame.

*  Parameters:
*     this
*        Pointer to the Frame.
*     axis
*        The number of the axis (zero-based) for which the Unit value is to
*        be set.
*     unit
*        The new value to be set.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     void.
*/

/* Local Variables: */
   AstAxis *ax;                  /* Pointer to Axis object */
   char *c;                      /* Copy of supplied string */
   const char *oldunit;          /* Pointer to old units string */
   int l;                        /* Used length of supplied string */

/* Check the global error status. */
   if ( !astOK ) return;

/* Get a copy of the supplied string which excludes trailing spaces. */
   l = astChrLen( unit );
   c = astStore( NULL, unit, (size_t) (l + 1) );
   if( astOK ) {
      c[ l ] = 0;

/* Validate the axis index and obtain a pointer to the required Axis. */
   (void) astValidateAxis( this, axis, "astSetUnit" );
      ax = astGetAxis( this, axis );

/* The new unit may require the Label and/or Symbol to be changed, but
   only if the Frames ActiveUnit flag is set. */
      if( astGetActiveUnit( this ) ) {

/* Get the existing Axis unit, using the astGetUnit method (rather than
   astGetAxisUnit) in order to get any default value in the case where
   the Unit attribute is not set. */
         oldunit = astGetUnit( this, axis );

/* Assign the new Unit value. This modifies labels and/or Symbols if
   necessary. */
         NewUnit( ax, oldunit, c, "astSetUnit", astGetClass( this ), status );
      }

/* Set the Axis Unit attribute value. */
      astSetAxisUnit( ax, c );

/* Annul the Axis pointer. */
      ax = astAnnul( ax );
   }

/* Free the string copy */
   c = astFree( c );

}

static int SubFrame( AstFrame *target, AstFrame *template,
                     int result_naxes, const int *target_axes,
                     const int *template_axes, AstMapping **map,
                     AstFrame **result, int *status ) {
/*
*+
*  Name:
*     astSubFrame

*  Purpose:
*     Select axes from a Frame and convert to the new coordinate system.

*  Type:
*     Protected virtual function.

*  Synopsis:
*     #include "frame.h"
*     int astSubFrame( AstFrame *target, AstFrame *template,
*                      int result_naxes, const int *target_axes,
*                      const int *template_axes, AstMapping **map,
*                      AstFrame **result )

*  Class Membership:
*     Frame method.

*  Description:
*     This function selects a requested sub-set (or super-set) of the axes from
*     a "target" Frame and creates a new Frame with copies of the selected
*     axes assembled in the requested order. It then optionally overlays the
*     attributes of a "template" Frame on to the result. It returns both the
*     resulting Frame and a Mapping that describes how to convert between the
*     coordinate systems described by the target and result Frames. If
*     necessary, this Mapping takes account of any differences in the Frames'
*     attributes due to the influence of the template.

*  Parameters:
*     target
*        Pointer to the target Frame, from which axes are to be selected.
*     template
*        Pointer to the template Frame, from which new attributes for the
*        result Frame are to be obtained. Optionally, this may be NULL, in
*        which case no overlaying of template attributes will be performed.
*     result_naxes
*        Number of axes to be selected from the target Frame. This number may
*        be greater than or less than the number of axes in this Frame (or
*        equal).
*     target_axes
*        Pointer to an array of int with result_naxes elements, giving a list
*        of the (zero-based) axis indices of the axes to be selected from the
*        target Frame. The order in which these are given determines the order
*        in which the axes appear in the result Frame. If any of the values in
*        this array is set to -1, the corresponding result axis will not be
*        derived from the target Frame, but will be assigned default attributes
*        instead.
*     template_axes
*        Pointer to an array of int with result_naxes elements. This should
*        contain a list of the template axes (given as zero-based axis indices)
*        with which the axes of the result Frame are to be associated. This
*        array determines which axes are used when overlaying axis-dependent
*        attributes of the template on to the result. If any element of this
*        array is set to -1, the corresponding result axis will not receive any
*        template attributes.
*
*        If the template argument is given as NULL, this array is not used and
*        a NULL pointer may also be supplied here.
*     map
*        Address of a location to receive a pointer to the returned Mapping.
*        The forward transformation of this Mapping will describe how to
*        convert coordinates from the coordinate system described by the target
*        Frame to that described by the result Frame. The inverse
*        transformation will convert in the opposite direction.
*     result
*        Address of a location to receive a pointer to the result Frame.

*  Returned Value:
*     A non-zero value is returned if coordinate conversion is
*     possible between the target and the result Frame. Otherwise zero
*     is returned and *map and *result are returned as NULL (but this
*     will not in itself result in an error condition). In general,
*     coordinate conversion should always be possible if no template
*     Frame is supplied but may not always be possible otherwise.

*  Notes:
*     - A value of zero will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.

*  Implementation Deficiencies:
*     - Any axis selection is currently permitted. Probably this
*     should be restricted so that each axis can only be selected
*     once. The astValidateAxisSelection method will do this but
*     currently there are bugs in the CmpFrame class that cause axis
*     selections which will not pass this test. Install the validation
*     when these are fixed.
*-

*  Implementation Notes:
*     - This implementation addresses the selection of axes from a
*     Frame class object. This simply results in another object of the
*     same class and a Mapping which describes an axis permutation (or
*     a unit Mapping as a special case). Changes of Frame attributes
*     have no significance for coordinate values in this class, so do
*     not affect the Mapping returned.
*/

/* Local Variables: */
   AstAxis *newaxis;             /* Pointer to new Axis object */
   AstFrame *tempframe;          /* Pointer to temporary Frame */
   AstMapping *aumap;            /* A units Mapping for a single axis */
   AstMapping *numap;            /* The new total units Mapping */
   AstMapping *umap;             /* The total units Mapping */
   int *inperm;                  /* Pointer to permutation array */
   int *outperm;                 /* Pointer to permutation array */
   int match;                    /* Coordinate conversion possible? */
   int result_axis;              /* Result Frame axis index */
   int target_axis;              /* Target Frame axis index */
   int target_naxes;             /* Number of target Frame axes */
   int unit;                     /* Unit Mapping appropriate? */
   int uunit;                    /* Is the "umap" Mapping a UnitMap? */

/* Initialise the returned values. */
   *map = NULL;
   *result = NULL;
   match = 0;

/* Check the global error status. */
   if ( !astOK ) return match;

/* Obtain the number of target Frame axes. */
   target_naxes = astGetNaxes( target );

/* Ensure we do not attempt to use a negative number of result axes. */
   if ( result_naxes < 0 ) result_naxes = 0;

/* Create a temporary new Frame with the required number of axes. This will
   have a default Axis object associated with each of its axes. We will
   replace these where necessary with copies of the actual Axis objects we
   require. */
   tempframe = astFrame( result_naxes, "", status );

/* Allocate memory to store two permutation arrays. These will be used to
   construct the Mapping that relates the target and result Frames. */
   inperm = astMalloc( sizeof( int ) * (size_t) target_naxes );
   outperm = astMalloc( sizeof( int ) * (size_t) result_naxes );
   if ( astOK ) {

/* Initialise the array that associates each target axis with the corresponding
   result axis (filling it with the value -1 initially signifies no
   associations). */
      for ( target_axis = 0; target_axis < target_naxes; target_axis++ ) {
         inperm[ target_axis ] = -1;
      }

/* Loop through each axis in the result Frame and obtain the index of the axis
   in the target Frame from which it is to be derived. */
      for ( result_axis = 0; result_axis < result_naxes; result_axis++ ) {
         target_axis = target_axes[ result_axis ];

/* Check if the resulting axis index is valid. If not, this result axis is not
   to be derived from any target axis, and it will therefore be left with its
   default attributes. Make an entry in the appropriate permutation array to
   indicate that this result axis is unassociated. */
         if ( ( target_axis < 0 ) || ( target_axis >= target_naxes ) ) {
            outperm[ result_axis ] = -1;

/* Otherwise, obtain a pointer to the target Axis object and modify the
   temporary Frame so that its axis is associated with the same Axis object.
   Annul the Axis pointer afterwards. */
         } else {
            newaxis = astGetAxis( target, target_axis );
            astSetAxis( tempframe, result_axis, newaxis );
            newaxis = astAnnul( newaxis );

/* Update both permutation arrays to record the association between the target
   and result axes. */
            outperm[ result_axis ] = target_axis;
            inperm[ target_axis ] = result_axis;
         }

/* Quit looping if an error occurs. */
         if ( !astOK ) break;
      }

/* So far, we have only modified pointers in the temporary Frame to refer to
   the target Frame's Axis objects. Since we will next modify these objects'
   attributes, we must make a deep copy of the entire temporary Frame so that
   we do not modify the target's axes. This copy now becomes our result Frame.
   Annul the temporary one. */
      if ( astOK ) {
         *result = astCopy( tempframe );
         tempframe = astAnnul( tempframe );

/* Invoke the target "astOverlay" method to overlay any remaining
   attributes from the target Frame which are not associated with
   individual axes (e.g.  the Frame's Title and Domain). */
         astOverlay( target, target_axes, *result );

/* If a template Frame was supplied, also invoke its astOverlay method to
   overlay its attributes on the result Frame. (Note that in this particular
   case this has no effect other than transferring attributes. In general,
   however, i.e. in derived classes, this process is vital to determining the
   mapping below, whose main purpose is to convert between the target and
   result Frames. These will have different attributes as a result of the
   influence that the template has here.) */
         if ( template ) astOverlay( template, template_axes, *result );

/* We will next generate the Mapping that relates the target and result
   Frames. If appropriate this should be a unit Mapping (UnitMap), so test if
   the number of axes in both Frames is equal. */
         unit = ( target_naxes == result_naxes );

/* If so, check the contents of one of the permutation arrays to see if all
   result axes are associated with the corresponding target axis (the converse
   then also follows). If not, note this fact and quit checking. */
         if ( unit ) {
            for ( result_axis = 0; result_axis < result_naxes;
                                   result_axis++ ) {
               if ( outperm[ result_axis ] != result_axis ) {
                  unit = 0;
                  break;
	       }
	    }
	 }

/* If a unit Mapping is appropriate, then construct it. */
         if ( unit ) {
            *map = (AstMapping *) astUnitMap( result_naxes, "", status );

/* Otherwise, construct a Mapping describing the axis permutation we have
   produced. */
         } else {
            *map = (AstMapping *) astPermMap( target_naxes, inperm,
                                              result_naxes, outperm, NULL,
                                              "", status );
         }

/* Note that coordinate conversion is possible. */
         match = 1;

/* If the ActiveUnit flag in both template and result Frame is non-zero, we
   now modify the Mapping to take account of any differences in the Units
   attributes of the target and results Frames. */
         if( template && astGetActiveUnit( template ) &&
                         astGetActiveUnit( *result ) ) {

/* Loop round the axes of the results Frame, accumulating a parallel CmpMap
   ("umap") in which each Mapping is the 1-D Mapping which transforms the
   Units of the corresponding target axis into the Units of the results
   axis. */
            umap = NULL;
            uunit = 1;
            for( result_axis = 0; result_axis < result_naxes; result_axis++ ) {

/* Find the index of the corresponding target axis. */
               if( unit ) {
                  target_axis = result_axis;
               } else {
                  target_axis = outperm[ result_axis ];
               }

/* Get the Unit string for both axes, and attempt to find a Mapping which
   transforms values in the target units into the corresponding value in the
   results units. If this results axis does not have a corresponding
   target axis, then indicate that no units mapping can be found. */
               if( target_axis > -1 ) {
                  aumap = astUnitMapper( astGetUnit( target, target_axis ),
                                         astGetUnit( *result, result_axis ),
                                         NULL, NULL );
               } else {
                  aumap = NULL;
               }

/* If no Mapping could be found, annull the Mapping and leave the loop.
   Otherwise, see if the Mapping is a UnitMap. If not, set a flag to indicate
   that we have at least one non-unit map. */
               if( !aumap ) {
                  if( umap ) umap = astAnnul( umap );
                  match = 0;
                  break;
               } else {
                  if( !astIsAUnitMap( aumap ) ) uunit = 0;
               }

/* Add this Mapping into the parallel CmpMap. */
               if( umap ) {
                  numap = (AstMapping *) astCmpMap( umap, aumap, 0, "", status );
                  umap = astAnnul( umap );
                  aumap = astAnnul( aumap );
                  umap = numap;
               } else {
                  umap = aumap;
               }
            }

/* If the resulting CmpMap is not just a UnitMap, add it in series with
   the current results mapping, and then simplify it. */
            if( !uunit && umap ) {
               numap = (AstMapping *) astCmpMap( *map, umap, 1, "", status );
               (void) astAnnul( *map );
               *map = numap;
            }

/* Annul the CmpMap containing the units Mappings. */
            if( umap ) umap = astAnnul( umap );

/* If the units could not bve matched annul the returned mapping. */
            if( !match && *map ) *map = astAnnul( *map );
         }
      }
   }

/* Free the memory used for the permutation arrays. */
   inperm = astFree( inperm );
   outperm = astFree( outperm );

/* If an error occurred, annul the returned objects and reset the returned
   value. */
   if ( !astOK ) {
      *map = astAnnul( *map );
      *result = astAnnul( *result );
      match = 0;
   }

/* Return the result. */
   return match;
}

static AstSystemType SystemCode( AstFrame *this, const char *system, int *status ) {
/*
*+
*  Name:
*     astSystemCode

*  Purpose:
*     Convert a string into a coordinate system type code.

*  Type:
*     Protected virtual function.

*  Synopsis:
*     #include "frame.h"
*     AstSystemType SystemCode( AstFrame *this, const char *system )

*  Class Membership:
*     Frame method.

*  Description:
*     This function converts a string used for the external description of
*     a coordinate system into a Frame coordinate system type code (System
*     attribute value). It is the inverse of the astSystemString function.

*  Parameters:
*     this
*        Pointer to the Frame.
*     system
*        Pointer to a constant null-terminated string containing the
*        external description of the coordinate system.

*  Returned Value:
*     The System type code.

*  Notes:
*     - A value of AST__BADSYSTEM is returned if the coordinate system
*     description was not recognised. This does not produce an error.
*     - A value of AST__BADSYSTEM is also returned if this function
*     is invoked with the global error status set or if it should fail
*     for any reason.
*-
*/

/* Local Variables: */
   AstSystemType result;      /* Result value to return */

/* Initialise. */
   result = AST__BADSYSTEM;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Match the "system" string against each possibility and assign the
   result. The basic Frame class only supports a single system
   "Cartesian". */
   if ( astChrMatch( "Cartesian", system ) ) {
      result = AST__CART;
   }

/* Return the result. */
   return result;
}

static const char *SystemString( AstFrame *this, AstSystemType system, int *status ) {
/*
*+
*  Name:
*     astSystemString

*  Purpose:
*     Convert a coordinate system type code into a string.

*  Type:
*     Protected virtual function.

*  Synopsis:
*     #include "frame.h"
*     const char *astSystemString( AstFrame *this, AstSystemType system )

*  Class Membership:
*     Frame method.

*  Description:
*     This function converts a Frame coordinate system type code
*     (System attribute value) into a string suitable for use as an
*     external representation of the coordinate system type.

*  Parameters:
*     this
*        Pointer to the Frame.
*     system
*        The coordinate system type code.

*  Returned Value:
*     Pointer to a constant null-terminated string containing the
*     textual equivalent of the type code supplied.

*  Notes:
*     - A NULL pointer value is returned if the coordinate system
*     code was not recognised. This does not produce an error.
*     - A NULL pointer value is also returned if this function is
*     invoked with the global error status set or if it should fail
*     for any reason.
*-
*/

/* Local Variables: */
   const char *result;        /* Pointer value to return */

/* Initialise. */
   result = NULL;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Match the "system" value against each possibility and convert to a
   string pointer. (Where possible, return the same string as would be
   used in the FITS WCS representation of the coordinate system). A basic
   Frame only allows a single System value, "Cartesian". */
   switch ( system ) {
   case AST__CART:
      result = "Cartesian";
      break;
   }

/* Return the result pointer. */
   return result;

}

static int TestActiveUnit( AstFrame *this, int *status ){
/*
*+
*  Name:
*     astTestActiveUnit

*  Purpose:
*     Determines if the ActiveUnit flag is set.

*  Type:
*     Protected virtual function.

*  Synopsis:
*     #include "frame.h"
*     int astTestActiveUnit( AstFrame *this )

*  Class Membership:
*     Frame method.

*  Description:
*     This function tests the current value of the ActiveUnit flag for a
*     Frame. See the description of the astSetActiveUnit function for a
*     description of the ActiveUnit flag.

*  Parameters:
*     this
*        Pointer to the Frame.

*  Returned Value:
*     Non-zero if the flag has been set. Zero otherwise.

*  Notes:
*     - A zero value will be returned if this function is
*     invoked with the AST error status set, or if it should fail for
*     any reason.
*--
*/

/* Local Variables: */
   int result;         /* The returned value */

/* Initialise. */
   result = 0;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Return the result. */
   return ( this->active_unit != -INT_MAX );
}

static int TestAttrib( AstObject *this_object, const char *attrib, int *status ) {
/*
*  Name:
*     TestAttrib

*  Purpose:
*     Test if a specified attribute value is set for a Frame.

*  Type:
*     Private function.

*  Synopsis:
*     #include "frame.h"
*     int TestAttrib( AstObject *this, const char *attrib, int *status )

*  Class Membership:
*     Frame member function (over-rides the astTestAttrib protected
*     method inherited from the Mapping class).

*  Description:
*     This function returns a boolean result (0 or 1) to indicate whether
*     a value has been set for one of a Frame's attributes.

*  Parameters:
*     this
*        Pointer to the Frame.
*     attrib
*        Pointer to a null terminated string specifying the attribute
*        name.  This should be in lower case with no surrounding white
*        space.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     One if a value has been set, otherwise zero.

*  Notes:
*     - This function uses one-based axis numbering so that it is
*     suitable for external (public) use.
*     - A value of zero will be returned if this function is invoked
*     with the global status set, or if it should fail for any reason.
*/

/* Local Variables: */
   AstAxis *ax;                  /* Pointer to Axis */
   AstFrame *pfrm;               /* Pointer to primary Frame containing axis */
   AstFrame *this;               /* Pointer to the Frame structure */
   char pfrm_attrib[ 100 ];      /* Primary Frame attribute */
   char *axis_attrib;            /* Pointer to axis attribute name */
   const char *old_attrib;       /* Pointer to supplied attribute name string */
   int axis;                     /* Frame axis number */
   int axis_nc;                  /* No. characters in axis attribute name */
   int free_axis_attrib;         /* Should axis_attrib be freed? */
   int has_axis;                 /* Does attrib name include axis specifier? */
   int len;                      /* Length of attrib string */
   int nc;                       /* No. characters read by astSscanf */
   int oldrep;                   /* Original error reporting state */
   int paxis;                    /* Axis index within primary frame */
   int result;                   /* Result value to return */
   int used;                     /* Could the setting string be used? */

/* Initialise. */
   result = 0;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Obtain a pointer to the Frame structure. */
   this = (AstFrame *) this_object;

/* Set a flag indicating if the attribute name includes an axis
   specifier. */
   has_axis = ( strchr( attrib, '(' ) != NULL );

/* A flag indicating that we do not need to free the axis_attrib memory. */
   free_axis_attrib = 0;

/* Initialise things to avoid compiler warnings. */
   axis_attrib = NULL;
   old_attrib = NULL;

/* Jump back to here if we are trying the same attribute but with an explicit
   axis "(1)" added to the end of the name. */
L1:

/* Obtain the length of the attrib string. */
   len = strlen( attrib );

/* Check the attribute name and test the appropriate attribute. */

/* Digits. */
/* ------- */
   if ( !strcmp( attrib, "digits" ) ) {
      result = astTestDigits( this );

/* Digits(axis). */
/* ------------- */
   } else if ( nc = 0,
               ( 1 == astSscanf( attrib, "digits(%d)%n", &axis, &nc ) )
               && ( nc >= len ) ) {

/* There is no function to test the Digits attribute for an axis
   directly, so obtain a pointer to the Axis and use this to test the
   attribute. */
      (void) astValidateAxis( this, axis - 1, "astTestDigits(axis)" );
      ax = astGetAxis( this, axis - 1 );
      result = astTestAxisDigits( ax );
      ax = astAnnul( ax );

/* Direction(axis). */
/* ---------------- */
   } else if ( nc = 0,
               ( 1 == astSscanf( attrib, "direction(%d)%n", &axis, &nc ) )
               && ( nc >= len ) ) {
      result = astTestDirection( this, axis - 1 );

/* Epoch. */
/* ------ */
   } else if ( !strcmp( attrib, "epoch" ) ) {
      result = astTestEpoch( this );

/* Bottom(axis). */
/* ------------- */
   } else if ( nc = 0,
               ( 1 == astSscanf( attrib, "bottom(%d)%n", &axis, &nc ) )
               && ( nc >= len ) ) {
      result = astTestBottom( this, axis - 1 );

/* Top(axis). */
/* ---------- */
   } else if ( nc = 0,
               ( 1 == astSscanf( attrib, "top(%d)%n", &axis, &nc ) )
               && ( nc >= len ) ) {
      result = astTestTop( this, axis - 1 );

/* Domain. */
/* ------- */
   } else if ( !strcmp( attrib, "domain" ) ) {
      result = astTestDomain( this );

/* Format(axis). */
/* ------------- */
   } else if ( nc = 0,
               ( 1 == astSscanf( attrib, "format(%d)%n", &axis, &nc ) )
               && ( nc >= len ) ) {
      result = astTestFormat( this, axis - 1 );

/* Label(axis). */
/* ------------ */
   } else if ( nc = 0,
               ( 1 == astSscanf( attrib, "label(%d)%n", &axis, &nc ) )
               && ( nc >= len ) ) {
      result = astTestLabel( this, axis - 1 );

/* MatchEnd. */
/* --------- */
   } else if ( !strcmp( attrib, "matchend" ) ) {
      result = astTestMatchEnd( this );

/* MaxAxes. */
/* -------- */
   } else if ( !strcmp( attrib, "maxaxes" ) ) {
      result = astTestMaxAxes( this );

/* MinAxes. */
/* -------- */
   } else if ( !strcmp( attrib, "minaxes" ) ) {
      result = astTestMinAxes( this );

/* Permute. */
/* -------- */
   } else if ( !strcmp( attrib, "permute" ) ) {
      result = astTestPermute( this );

/* PreserveAxes. */
/* ------------- */
   } else if ( !strcmp( attrib, "preserveaxes" ) ) {
      result = astTestPreserveAxes( this );

/* Symbol(axis). */
/* ------------- */
   } else if ( nc = 0,
               ( 1 == astSscanf( attrib, "symbol(%d)%n", &axis, &nc ) )
               && ( nc >= len ) ) {
      result = astTestSymbol( this, axis - 1 );

/* AlignSystem. */
/* ------------ */
   } else if ( !strcmp( attrib, "alignsystem" ) ) {
      result = astTestAlignSystem( this );

/* System. */
/* ------- */
   } else if ( !strcmp( attrib, "system" ) ) {
      result = astTestSystem( this );

/* Title. */
/* ------ */
   } else if ( !strcmp( attrib, "title" ) ) {
      result = astTestTitle( this );

/* Unit(axis). */
/* ----------- */
   } else if ( nc = 0,
               ( 1 == astSscanf( attrib, "unit(%d)%n", &axis, &nc ) )
               && ( nc >= len ) ) {
      result = astTestUnit( this, axis - 1 );

/* ObsLat. */
/* ------- */
   } else if ( !strcmp( attrib, "obslat" ) ) {
      result = astTestObsLat( this );

/* ObsLon. */
/* ------- */
   } else if ( !strcmp( attrib, "obslon" ) ) {
      result = astTestObsLon( this );

/* ObsAlt. */
/* ------- */
   } else if ( !strcmp( attrib, "obsalt" ) ) {
      result = astTestObsAlt( this );

/* Dut1. */
/* ---- */
   } else if ( !strcmp( attrib, "dut1" ) ) {
      result = astTestDut1( this );

/* Read-only attributes. */
/* --------------------- */
/* Test if the attribute name matches any of the read-only attributes
   of this class. If it does, then return zero. */
   } else if ( !strcmp( attrib, "naxes" ) ||
               !strncmp( attrib, "normunit", 8 ) ) {
      result = 0;

/* Other axis attributes. */
/* ---------------------- */
/* If the attribute was not identified above, but appears to refer to
   a Frame axis, then it may refer to an Axis object of a derived type
   (which has additional attributes not recognised here). */
   } else if ( !free_axis_attrib && ( nc = 0,
               ( 1 == astSscanf( attrib, "%*[^()]%n(%d)%n",
                                      &axis_nc, &axis, &nc ) )
               && ( nc >= len ) ) ) {

/* Validate the axis index and extract the attribute name. */
      (void) astValidateAxis( this, axis - 1, "astTest" );
      axis_attrib = astString( attrib, axis_nc );

/* Obtain a pointer to the Axis object. */
      ax = astGetAxis( this, axis - 1 );
      if( astOK ) {

/* Assume that we will be able to use the attribute name. */
         used = 1;

/* Temporarily switch off error reporting so that if the following attempt
   to access the axis attribute fails, we can try to interpret the
   attribute name as an attribute of the primary Frame containing the
   specified axis. Any errors reported in this context will simply be
   ignored, in particularly they are not deferred for later delivery. */
         oldrep = astReporting( 0 );

/* Use the Axis astTestAttrib method to test the attribute value. */
         result = astTestAttrib( ax, axis_attrib );

/* If the above call failed with a status of AST__BADAT, indicating that
   the attribute name was not recognised, clear the status so that we can
   try to interpret the attribute name as an attribute of the primary Frame
   containing the specified axis. */
         if( astStatus == AST__BADAT ) {
            astClearStatus;

/* Find the primary Frame containing the specified axis. */
            astPrimaryFrame( this, axis - 1, &pfrm, &paxis );

/* Only attempt to use the primary Frame if it is not the same as "this"
   - otherwise we could end up in an infinite loop. */
            if( pfrm != this ) {

/* Modify the attribute name to refer to the axis numbering of the
   primary frame. */
               sprintf( pfrm_attrib, "%s(%d)", axis_attrib, paxis + 1 );

/* Attempt to test the attribute as an attribute of the primary Frame. */
               result = astTestAttrib( pfrm, pfrm_attrib );

/* If this failed, clear the status and indicate that we have not managed to
   use the attribute name. */
               if( !astOK ) {
                  astClearStatus;
                  used = 0;
               }

            } else {
               used = 0;
            }

/* If not found attempt to test the attribute value in the Axis, omitting
   the axis index. */
            if( ! used ) {
               result = astTestAttrib( pfrm, axis_attrib );
               if( !astOK ) {
                  astClearStatus;
               } else {
                  used = 1;
               }
            }

/* Annul the primary Frame pointer. */
            pfrm = astAnnul( pfrm );
         }

/* Re-instate the original error reporting state. */
         astReporting( oldrep );

/* If we could not use the attribute name, attempt to test the axis
   attribute again, this time retaining the error report. This is done
   to ensure the user gets an appropriate error message. */
         if( !used ) result = astTestAttrib( ax, axis_attrib );
      }

/* Annul the Axis pointer and free the memory holding the attribute
   name. */
      ax = astAnnul( ax );
      axis_attrib = astFree( axis_attrib );

/* Not recognised. */
/* --------------- */
/* If the attribute is still not recognised, and the Frame has only 1 axis,
   and the attribute name does not already include an axis specifier, try
   again after appending "(1)" to the end of the attribute name. */
   } else if( !has_axis && astGetNaxes( this ) == 1 ) {

/* Take a copy of the supplied name, allowing 3 extra characters for the
   axis specifier "(1)". */
      axis_attrib = astMalloc( len + 4 );
      if( axis_attrib ) memcpy( axis_attrib, attrib, len );

/* Indicate we should free the axis_attrib memory. */
      free_axis_attrib = 1;

/* Add in the axis specifier. */
      strcpy( axis_attrib + len, "(1)" );

/* Use the new attribute name instead of the supplied name. */
      old_attrib = attrib;
      attrib = axis_attrib;

/* Indicate the attribute name now has an axis specifier. */
      has_axis = 1;

/* Jump back to try interpreting the new attribute name. */
      goto L1;

/* Not recognised. */
/* --------------- */
/* If the attribute name is still not recognised, pass it on to the parent
   method for further interpretation. First re-instate the original attrib
   name string if it was changed above. */
   } else {
      if( free_axis_attrib ) {
         attrib = old_attrib;
         axis_attrib = astFree( axis_attrib );
      }
      result = (*parent_testattrib)( this_object, attrib, status );
   }

/* Return the result, */
   return result;
}

static AstPointSet *Transform( AstMapping *this_mapping, AstPointSet *in,
                               int forward, AstPointSet *out, int *status ) {
/*
*  Name:
*     Transform

*  Purpose:
*     Use a Frame to transform a set of points.

*  Type:
*     Private function.

*  Synopsis:
*     #include "frame.h"
*     AstPointSet *Transform( AstMapping *this, AstPointSet *in,
*                             int forward, AstPointSet *out, int *status )

*  Class Membership:
*     Frame member function (over-rides the astTransform method inherited
*     from the Mapping class).

*  Description:
*     This function takes a Frame and a set of points encapsulated in a
*     PointSet and transforms the points so as to perform the identity
*     transformation (i.e. simply copies the coordinate values).

*  Parameters:
*     this
*        Pointer to the Frame.
*     in
*        Pointer to the PointSet holding the input coordinate data.
*     forward
*        A non-zero value indicates that the forward coordinate transformation
*        should be applied, while a zero value requests the inverse
*        transformation. In this case, both transformations are equivalent.
*     out
*        Pointer to a PointSet which will hold the transformed (output)
*        coordinate values. A NULL value may also be given, in which case a
*        new PointSet will be created by this function.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     Pointer to the output (possibly new) PointSet.

*  Notes:
*     -  A null pointer will be returned if this function is invoked with the
*     global error status set, or if it should fail for any reason.
*     -  The number of coordinate values per point in the input PointSet must
*     match the number of coordinates for the Frame being applied. This number
*     will be equal to the number of Frame axes.
*     -  If an output PointSet is supplied, it must have space for sufficient
*     number of points and coordinate values per point to accommodate the
*     result. Any excess space will be ignored.
*/

/* Local Variables: */
   AstFrame *this;               /* Pointer to the Frame structure */
   AstPointSet *result;          /* Pointer value to be returned */
   AstUnitMap *unitmap;          /* Pointer to temporary UnitMap */

/* Check the global error status. */
   if ( !astOK ) return NULL;

/* Obtain a pointer to the Frame structure. */
   this = (AstFrame *) this_mapping;

/* Create a unit Mapping with one coordinate for each Frame axis. */
   unitmap = astUnitMap( astGetNaxes( this ), "", status );

/* Use the Mapping to transform (i.e. copy) the coordinate values. */
   result = astTransform( unitmap, in, forward, out );

/* Annul the Mapping. */
   unitmap = astAnnul( unitmap );

/* If an error occurred and a new PointSet may have been created, then annul
   the result. In any case, ensure that a NULL pointer is returned. */
   if ( !astOK ) {
      if ( !out ) result = astAnnul( result );
      result = NULL;
   }

/* Return the result pointer. */
   return result;
}

static int Unformat( AstFrame *this, int axis, const char *string,
                     double *value, int *status ) {
/*
*+
*  Name:
*     astUnformat

*  Purpose:
*     Read a formatted coordinate value for a Frame axis.

*  Type:
*     Protected virtual function.

*  Synopsis:
*     #include "frame.h"
*     int astUnformat( AstFrame *this, int axis, const char *string,
*                      double *value )

*  Class Membership:
*     Frame method.

*  Description:
*     This function reads a formatted coordinate value for a Frame
*     axis (supplied as a string) and returns the equivalent numerical
*     value as a double. It also returns the number of characters read
*     from the string.

*  Parameters:
*     this
*        Pointer to the Frame.
*     axis
*        The number of the Frame axis for which the coordinate value
*        is to be read (axis numbering starts at zero for the first
*        axis).
*     string
*        Pointer to a constant null-terminated string containing the
*        formatted coordinate value.
*     value
*        Pointer to a double in which the coordinate value read will be
*        returned.

*  Returned Value:
*     The number of characters read from the string to obtain the
*     coordinate value.

*  Notes:
*     - Any white space at the beginning of the string will be
*     skipped, as also will any trailing white space following the
*     coordinate value read. The function's return value will reflect
*     this.
*     - A function value of zero (and no coordinate value) will be
*     returned, without error, if the string supplied does not contain
*     a suitably formatted value.
*     - The string "<bad>" is recognised as a special case and will
*     generate the value AST__BAD, without error. The test for this
*     string is case-insensitive and permits embedded white space.
*     - A function result of zero will be returned and no coordinate
*     value will be returned via the "value" pointer if this function
*     is invoked with the global error status set, or if it should
*     fail for any reason.
*-

*  Implementation Notes:
*     - This function implements the basic astUnformat method
*     available via the protected interface to the Frame class. The
*     public interface to this method is provided by the
*     astUnformatId_ function.
*/

/* Local Variables: */
   AstAxis *ax;                  /* Pointer to Axis object */
   const char *label;            /* Pointer to axis label string */
   double coord;                 /* Coordinate value read */
   int digits_set;               /* Axis Digits attribute set? */
   int nc;                       /* Number of characters read */
   int status_value;             /* AST error status */

/* Initialise. */
   nc = 0;

/* Check the global error status. */
   if ( !astOK ) return nc;

/* Validate the axis index and obtain a pointer to the required Axis. */
   (void) astValidateAxis( this, axis, "astUnformat" );
   ax = astGetAxis( this, axis );

/* Test if any Axis attributes which may affect the result are
   undefined (i.e. have not been explicitly set). If so, we over-ride
   them, giving them temporary values dictated by the Frame. Only the
   Digits attribute is potentially relevant here. */
   digits_set = astTestAxisDigits( ax );
   if ( !digits_set ) astSetAxisDigits( ax, astGetDigits( this ) );

/* Read the coordinate value. */
   if ( astOK ) {
      nc = astAxisUnformat( ax, string, &coord );

/* If an error occurred, save and temporarily clear the global error
   status while the axis Label string is obtained. Then restore the
   original error status value afterwards. */
      if ( !astOK ) {
         status_value = astStatus;
         astClearStatus;
         label = astGetLabel( this, axis );
         astSetStatus( status_value );

/* Report a contextual error message containing the axis label. */
         astError( status_value, "%s(%s): Unable to read \"%s\" value.", status,
                   "astUnformat", astGetClass( this ), label );
      }
   }

/* Clear any Axis attributes that were temporarily over-ridden. */
   if ( !digits_set ) astClearAxisDigits( ax );

/* Annul the Axis pointer. */
   ax = astAnnul( ax );

/* If an error occurred, clear the count of characters read. */
   if ( !astOK ) {
      nc = 0;

/* Otherwise, if characters were read, return the coordinate value. */
   } else if ( nc ) {
      *value = coord;
   }

/* Return the number of characters read. */
   return nc;
}

static int ValidateAxis( AstFrame *this, int axis, const char *method, int *status ) {
/*
*+
*  Name:
*     astValidateAxis

*  Purpose:
*     Validate and permute a Frame's axis index.

*  Type:
*     Protected virtual function.

*  Synopsis:
*     #include "frame.h"
*     int astValidateAxis( AstFrame *this, int axis, const char *method )

*  Class Membership:
*     Frame method.

*  Description:
*     This function checks the validity of an index (zero-based) which
*     is to be used to address one of the coordinate axes of a
*     Frame. If the index is valid, it is permuted using the axis
*     permutation array associated with the Frame and the (zero-based)
*     permuted axis index is returned.  This gives the location of the
*     required axis information within the Frame's internal arrays. If
*     the axis index supplied is not valid, an error is reported and
*     the global error status is set.

*  Parameters:
*     this
*        Pointer to the Frame.
*     axis
*        The axis index (zero-based) to be checked. To be valid, it
*        must lie between zero and (naxes-1) inclusive, where "naxes"
*        is the number of coordinate axes associated with the Frame.
*     method
*        Pointer to a constant null-terminated character string
*        containing the name of the method that invoked this function
*        to validate an axis index. This method name is used solely
*        for constructing error messages.

*  Returned Value:
*     The permuted axis index.

*  Notes:
*     - A value of zero will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*     - Error messages issued by this function refer to the external
*     (public) numbering system used for axes (which is one-based),
*     whereas zero-based axis indices are used internally.
*-
*/

/* Local Variables: */
   const int *perm;              /* Pointer to axis permutation array */
   int naxes;                    /* Number of Frame axes */
   int result;                   /* Permuted axis index */

/* Initialise. */
   result = 0;

/* Determine the number of Frame axes. */
   naxes = astGetNaxes( this );
   if ( astOK ) {

/* If the Frame has no axes, report an error (note we convert to
   one-based axis numbering in the error message). */
      if ( naxes == 0 ) {
         astError( AST__AXIIN, "%s(%s): Invalid attempt to use an axis index "
                   "(%d) for a %s which has no axes.", status, method,
                   astGetClass( this ), axis + 1, astGetClass( this ) );

/* Otherwise, check the axis index for validity and report an error if
   it is not valid (again, use one-based axis numbering). */
      } else if ( ( axis < 0 ) || ( axis >= naxes ) ) {
         astError( AST__AXIIN, "%s(%s): Axis index (%d) invalid - it should "
                   "be in the range 1 to %d.", status, method, astGetClass( this ),
                   axis + 1, naxes );

/* If the axis index was valid, obtain the axis permutation array and
   use this to generate the permuted axis value. */
      } else {
         perm = astGetPerm( this );
         if( perm ) result = perm[ axis ];
      }
   }

/* Return the result. */
   return result;
}

static void ValidateAxisSelection( AstFrame *this, int naxes, const int *axes,
                                   const char *method, int *status ) {
/*
*+
*  Name:
*     astValidateAxisSelection

*  Purpose:
*     Check that a set of axes selected from a Frame is valid.

*  Type:
*     Protected virtual function.

*  Synopsis:
*     #include "frame.h"
*     void astValidateAxisSelection( AstFrame *this, int naxes,
*                                    const int *axes, const char *method )

*  Class Membership:
*     Frame method.

*  Description:
*     This function checks the validity of an array of (zero-based)
*     axis indices that specify a set of axes to be selected from a
*     Frame. To be valid, no axis should be selected more than
*     once. In assessing this, any axis indices that do not refer to
*     valid Frame axes (e.g. are set to -1) are ignored.
*
*     If the axis selection is valid, this function returns without further
*     action. Otherwise, an error is reported and the global error status is
*     set.

*  Parameters:
*     this
*        Pointer to the Frame.
*     naxes
*        The number of axes to be selected (may be zero).
*     axes
*        Pointer to an array of int with naxes elements that contains the
*        (zero based) axis indices to be checked.
*     method
*        Pointer to a constant null-terminated character string
*        containing the name of the method that invoked this function
*        to validate an axis selection. This method name is used
*        solely for constructing error messages.
*-
*/

/* Local Variables: */
   int *count;                   /* Pointer to temporary array of counts */
   int axis;                     /* Loop counter for selected axes */
   int frame_axis;               /* Loop counter for Frame axes */
   int frame_naxes;              /* Number of Frame axes */
   int valid;                    /* Axis selection valid? */

/* Check the global error status. */
   if ( !astOK ) return;

/* Check to see if no axes have been selected. If so, there is nothing to
   do. */
   if ( naxes ) {

/* Initialise. */
      valid = 1;

/* Obtain the number of Frame axes and allocate an array of int with
   one element for each Frame axis. This will store a count of the
   number of times each axis is selected. */
      frame_naxes = astGetNaxes( this );
      count = astMalloc( sizeof( int ) * (size_t) frame_naxes );
      if ( astOK ) {

/* Initialise the array of counts to zero. */
         for ( frame_axis = 0; frame_axis < frame_naxes; frame_axis++ ) {
            count[ frame_axis ] = 0;
         }

/* Loop through each selected axis. */
         for ( axis = 0; axis < naxes; axis++ ) {
            frame_axis = axes[ axis ];

/* Check if the selected axis index is valid for the Frame. If so, increment
   the selection count for that Frame axis. */
            if ( ( frame_axis >= 0 ) && ( frame_axis < frame_naxes ) ) {
               count[ frame_axis ]++;
            }
         }

/* Loop through the count array and check that no Frame axis was selected
   more than once. If it was, clear the "valid" flag and quit checking. */
         for ( frame_axis = 0; frame_axis < frame_naxes; frame_axis++ ) {
            if ( count[ frame_axis ] > 1 ) {
               valid = 0;
               break;
            }
         }
      }

/* Free the temporary count array. */
      count = astFree( count );

/* If no error has occurred, but the axis selection is not valid, then report
   an error. */
      if ( astOK && !valid ) {
         astError( AST__SELIN, "%s(%s): Invalid axis selection - each axis "
                   "may be selected only once.", status, method, astGetClass( this ) );
      }
   }
}

static int ValidateSystem( AstFrame *this, AstSystemType system, const char *method, int *status ) {
/*
*+
*  Name:
*     astValidateSystem

*  Purpose:
*     Validate a value for a Frame's System attribute.

*  Type:
*     Protected virtual function.

*  Synopsis:
*     #include "frame.h"
*     int astValidateSystem( AstFrame *this, AstSystemType system,
*                            const char *method )

*  Class Membership:
*     Frame method.

*  Description:
*     This function checks the validity of the supplied system value.
*     If the value is valid, it is returned unchanged. Otherwise, an
*     error is reported and a value of AST__BADSYSTEM is returned.

*  Parameters:
*     this
*        Pointer to the Frame.
*     system
*        The system value to be checked.
*     method
*        Pointer to a constant null-terminated character string
*        containing the name of the method that invoked this function
*        to validate an axis index. This method name is used solely
*        for constructing error messages.

*  Returned Value:
*     The validated system value.

*  Notes:
*     - A value of AST_BADSYSTEM will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*-
*/

/* Local Variables: */
   AstSystemType result;              /* Validated system value */

/* Initialise. */
   result = AST__BADSYSTEM;

/* Check the global error status. */
   if ( !astOK ) return result;

/* If the value is out of bounds, report an error. */
   if ( system < FIRST_SYSTEM || system > LAST_SYSTEM ) {
         astError( AST__AXIIN, "%s(%s): Bad value (%d) given for the System "
                   "or AlignSystem attribute of a %s.", status, method,
                   astGetClass( this ), (int) system, astGetClass( this ) );

/* Otherwise, return the supplied value. */
   } else {
      result = system;
   }

/* Return the result. */
   return result;
}

/* Functions which access class attributes. */
/* ---------------------------------------- */
/* Implement member functions to access the attributes associated with
   the axes of a Frame using the private macros defined for this
   purpose at the start of this file. */

/*
*att++
*  Name:
*     Naxes

*  Purpose:
*     Number of Frame axes.

*  Type:
*     Public attribute.

*  Synopsis:
*     Integer, read-only.

*  Description:
*     This is a read-only attribute giving the number of axes in a
*     Frame (i.e. the number of dimensions of the coordinate space
*     which the Frame describes). This value is determined when the
*     Frame is created.

*  Applicability:
*     Frame
*        All Frames have this attribute.
*     FrameSet
*        The Naxes attribute of a FrameSet is the same as that of its
*        current Frame (as specified by the Current attribute).
*     CmpFrame
*        The Naxes attribute of a CmpFrame is equal to the sum of the
*        Naxes values of its two component Frames.
*att--
*/


/*
*att++
*  Name:
*     Direction(axis)

*  Purpose:
*     Display axis in conventional direction?

*  Type:
*     Public attribute.

*  Synopsis:
*     Integer (boolean).

*  Description:
*     This attribute is a boolean value which suggests how the axes of
*     a Frame should be displayed (e.g.) in graphical output. By
*     default, it has the value one, indicating that they should be
*     shown in the conventional sense (increasing left to right for an
*     abscissa, and bottom to top for an ordinate). If set to zero,
*     this attribute indicates that the direction should be reversed,
*     as would often be done for an astronomical magnitude or a right
*     ascension axis.

*  Applicability:
*     Frame
*        The default Direction value supplied by the Frame class is 1,
*        indicating that all axes should be displayed in the
*        conventional direction.
*     SkyFrame
*        The SkyFrame class re-defines the default Direction value to
*        suggest that certain axes (e.g. right ascension) should be
*        plotted in reverse when appropriate.
*     FrameSet
*        The Direction attribute of a FrameSet axis is the same as
*        that of its current Frame (as specified by the Current
*        attribute).
*     Plot
*        The Direction attribute of the base Frame in a Plot is set to
*        indicate the sense of the two graphics axes, as implied by the
*        graphics bounding box supplied when the Plot was created.

*  Notes:
*     - When specifying this attribute by name, it should be
*     subscripted with the number of the Frame axis to which it
*     applies.
*     - The Direction attribute does not directly affect the behaviour
*     of the AST library. Instead, it serves as a hint to applications
*     programs about the orientation in which they may wish to display
*     any data associated with the Frame. Applications are free to
*     ignore this hint if they wish.
*att--
*/
/* This simply provides an interface to the Axis methods for accessing
   the Direction flag. */
MAKE_CLEAR(Direction)
MAKE_GET(Direction,int,0,0,0)
MAKE_SET(Direction,int)
MAKE_TEST(Direction)

/*
*att++
*  Name:
*     Dut1

*  Purpose:
*     The UT1-UTC correction.

*  Type:
*     Public attribute.

*  Synopsis:
*     Floating point.

*  Description:
*     This attribute is used when calculating the Local Apparent Sidereal
*     Time corresponding to SkyFrame's Epoch value (used when converting
*     positions to or from the "AzEl" system). It should be set to the
*     difference, in seconds, between the UT1 and UTC timescales at the
*     moment in time represented by the SkyFrame's Epoch attribute. The
*     value to use is unpredictable and depends on changes in the earth's
*     rotation speed. Values for UT1-UTC can be obtained from the
*     International Earth Rotation and Reference Systems Service
*     (IERS) at http://www.iers.org/.
*
*     Currently, the correction is always less than 1 second. This is
*     ensured by the occasional introduction of leap seconds into the UTC
*     timescale. Therefore no great error will usually result if no value
*     is assigned to this attribute (in which case a default value of
*     zero is used). However, it is possible that a decision may be taken
*     at some time in the future to abandon the introduction of leap
*     seconds, in which case the DUT correction could grow to significant
*     sizes.

*  Applicability:
*     Frame
*        All Frames have this attribute.

*att--
*/
/* The UT1-UTC correction, in seconds. Has a value of AST__BAD when not set
   yielding a default value of 0.0. */
astMAKE_CLEAR(Frame,Dut1,dut1,AST__BAD)
astMAKE_GET(Frame,Dut1,double,0.0,(this->dut1 == AST__BAD ? 0.0 : this->dut1))
astMAKE_SET(Frame,Dut1,double,dut1,value)
astMAKE_TEST(Frame,Dut1,( this->dut1 != AST__BAD ))



/*
*att++
*  Name:
*     Epoch

*  Purpose:
*     Epoch of observation.

*  Type:
*     Public attribute.

*  Synopsis:
*     Floating point.

*  Description:
*     This attribute is used to qualify the coordinate systems described by
*     a Frame, by giving the moment in time when the coordinates are known
*     to be correct. Often, this will be the date of observation, and is
*     important in cases where coordinates systems move with respect to each
*     other over the course of time.
*
*     The Epoch attribute is stored as a Modified Julian Date, but
*     when setting its value it may be given in a variety of
*     formats. See the "Input Formats" section (below) for details.
*     Strictly, the Epoch value should be supplied in the TDB timescale,
*     but for some purposes (for instance, for converting sky positions
*     between different types of equatorial system) the timescale is not
*     significant, and UTC may be used.

*  Input Formats:
*     The formats accepted when setting an Epoch value are listed
*     below. They are all case-insensitive and are generally tolerant
*     of extra white space and alternative field delimiters:
*
*     - Besselian Epoch: Expressed in decimal years, with or without
*     decimal places ("B1950" or "B1976.13" for example).
*
*     - Julian Epoch: Expressed in decimal years, with or without
*     decimal places ("J2000" or "J2100.9" for example).
*
*     - Year: Decimal years, with or without decimal places ("1996.8"
*     for example).  Such values are interpreted as a Besselian epoch
*     (see above) if less than 1984.0 and as a Julian epoch otherwise.
*
*     - Julian Date: With or without decimal places ("JD 2454321.9" for
*     example).
*
*     - Modified Julian Date: With or without decimal places
*     ("MJD 54321.4" for example).
*
*     - Gregorian Calendar Date: With the month expressed either as an
*     integer or a 3-character abbreviation, and with optional decimal
*     places to represent a fraction of a day ("1996-10-2" or
*     "1996-Oct-2.6" for example). If no fractional part of a day is
*     given, the time refers to the start of the day (zero hours).
*
*     - Gregorian Date and Time: Any calendar date (as above) but with
*     a fraction of a day expressed as hours, minutes and seconds
*     ("1996-Oct-2 12:13:56.985" for example). The date and time can be
*     separated by a space or by a "T" (as used by ISO8601 format).

*  Output Format:
*     When enquiring Epoch values, the format used is the "Year"
*     format described under "Input Formats". This is a value in
*     decimal years which will be a Besselian epoch if less than
*     1984.0 and a Julian epoch otherwise.  By omitting any character
*     prefix, this format allows the Epoch value to be obtained as
*     either a character string or a floating point value.

*  Applicability:
*     Frame
*        All Frames have this attribute. The basic Frame class provides
*        a default of J2000.0 (Julian) but makes no use of the Epoch value.
*        This is because the Frame class does not distinguish between
*        different Cartesian coordinate systems (see the System attribute).
*     CmpFrame
*        The default Epoch value for a CmpFrame is selected as follows;
*        if the Epoch attribute has been set in the first component Frame
*        then the Epoch value from the first component Frame is used as
*        the default for the CmpFrame. Otherwise, if the Epoch attribute has
*        been set in the second component Frame then the Epoch value from the
*        second component Frame is used as the default for the CmpFrame.
*        Otherwise, the default Epoch value from the first component
*        Frame is used as the default for the CmpFrame. When the Epoch
*        attribute of a CmpFrame is set or cleared, it is also set or
*        cleared in the two component Frames.
*     FrameSet
*        The Epoch attribute of a FrameSet is the same as that of its current
*        Frame (as specified by the Current attribute).
*     SkyFrame
*        The coordinates of sources within a SkyFrame can changed with time
*        for various reasons, including: (i) changing aberration of light
*        caused by the observer's velocity (e.g. due to the Earth's motion
*        around the Sun), (ii) changing gravitational deflection by the Sun
*        due to changes in the observer's position with time, (iii) fictitious
*        motion due to rotation of non-inertial coordinate systems (e.g. the
*        old FK4 system), and (iv) proper motion of the source itself (although
*        this last effect is not handled by the SkyFrame class because it
*        affects individual sources rather than the coordinate system as
*        a whole).
*
*        The default Epoch value in a SkyFrame is B1950.0 (Besselian) for the
*        old FK4-based coordinate systems (see the System attribute) and
*        J2000.0 (Julian) for all others.
*
*        Care must be taken to distinguish the Epoch value, which relates to
*        motion (or apparent motion) of the source, from the superficially
*        similar Equinox value. The latter is used to qualify a coordinate
*        system which is itself in motion in a (notionally) predictable way
*        as a result of being referred to a slowly moving reference plane
*        (e.g. the equator).
*
*        See the description of the System attribute for details of which
*        qualifying attributes apply to each celestial coordinate system.
*     TimeFrame
*        A TimeFrame describes a general time axis and so cannot be completely
*        characterised by a single Epoch value. For this reason the TimeFrame
*        class makes no use of the Epoch attribute. However, user code can
*        still make use of the attribute if necessary to represent a "typical"
*        time spanned by the TimeFrame. The default Epoch value for a TimeFrame
*        will be the TDB equivalent of the current value of the TimeFrame's
*        TimeOrigin attribute. If no value has been set for TimeOrigin,
*        then the default Epoch value is J2000.0.


The coordinates of sources within a SkyFrame can changed with time
*att--
*/
/* Clear the Epoch value by setting it to AST__BAD. */
astMAKE_CLEAR(Frame,Epoch,epoch,AST__BAD)

/* Provide a default value of J2000.0 setting. */
astMAKE_GET(Frame,Epoch,double,AST__BAD,(
           ( this->epoch != AST__BAD ) ? this->epoch : palSlaEpj2d( 2000.0 )))

/* Allow any Epoch value to be set. */
astMAKE_SET(Frame,Epoch,double,epoch,value)

/* An Epoch value is set if it is not equal to AST__BAD. */
astMAKE_TEST(Frame,Epoch,( this->epoch != AST__BAD ))

/*
*att++
*  Name:
*     Top(axis)

*  Purpose:
*     Highest axis value to display

*  Type:
*     Public attribute.

*  Synopsis:
*     Floating point.

*  Description:
*     This attribute gives the highest axis value to be displayed (for
c     instance, by the astGrid method).
f     instance, by the AST_GRID method).

*  Applicability:
*     Frame
*        The default supplied by the Frame class is to display all axis
*        values, without any limit.
*     SkyFrame
*        The SkyFrame class re-defines the default Top value to +90 degrees
*        for latitude axes, and 180 degrees for co-latitude axes. The
*        default for longitude axes is to display all axis values.

*  Notes:
*     - When specifying this attribute by name, it should be
*     subscripted with the number of the Frame axis to which it
*     applies.
*att--
*/
/* This simply provides an interface to the Axis methods for accessing
   the Top value. */
MAKE_CLEAR(Top)
MAKE_GET(Top,double,DBL_MAX,0,DBL_MAX)
MAKE_SET(Top,double)
MAKE_TEST(Top)

/*
*att++
*  Name:
*     Bottom(axis)

*  Purpose:
*     Lowest axis value to display

*  Type:
*     Public attribute.

*  Synopsis:
*     Floating point.

*  Description:
*     This attribute gives the lowest axis value to be displayed (for
c     instance, by the astGrid method).
f     instance, by the AST_GRID method).

*  Applicability:
*     Frame
*        The default supplied by the Frame class is to display all axis
*        values, without any limit.
*     SkyFrame
*        The SkyFrame class re-defines the default Bottom value to -90 degrees
*        for latitude axes, and 0 degrees for co-latitude axes. The
*        default for longitude axes is to display all axis values.

*  Notes:
*     - When specifying this attribute by name, it should be
*     subscripted with the number of the Frame axis to which it
*     applies.
*att--
*/
/* This simply provides an interface to the Axis methods for accessing
   the Bottom value. */
MAKE_CLEAR(Bottom)
MAKE_GET(Bottom,double,-DBL_MAX,0,-DBL_MAX)
MAKE_SET(Bottom,double)
MAKE_TEST(Bottom)

/*
*att++
*  Name:
*     Format(axis)

*  Purpose:
*     Format specification for axis values.

*  Type:
*     Public attribute.

*  Synopsis:
*     String.

*  Description:
*     This attribute specifies the format to be used when displaying
*     coordinate values associated with a particular Frame axis
*     (i.e. to convert values from binary to character form). It is
c     interpreted by the astFormat function and determines the
f     interpreted by the AST_FORMAT function and determines the
*     formatting which it applies.
*
*     If no Format value is set for a Frame axis, a default value is
*     supplied instead. This is based on the value of the Digits, or
*     Digits(axis), attribute and is chosen so that it displays the
*     requested number of digits of precision.

*  Applicability:
*     Frame
*        The Frame class interprets this attribute as a format
*        specification string to be passed to the C "printf" function
*        (e.g. "%1.7G") in order to format a single coordinate value
*        (supplied as a double precision number).
c
c        When supplying a value for this attribute, beware that the
c        "%" character may be interpreted directly as a format
c        specification by some printf-like functions (such as
c        astSet). You may need to double it (i.e. use "%%") to avoid
c        this.
*     SkyFrame
*        The SkyFrame class re-defines the syntax and default value of
*        the Format string to allow the formatting of sexagesimal
*        values as appropriate for the particular celestial coordinate
*        system being represented. The syntax of SkyFrame Format
*        strings is described (below) in the "SkyFrame Formats"
*        section.
*     FrameSet
*        The Format attribute of a FrameSet axis is the same as that
*        of its current Frame (as specified by the Current
*        attribute). Note that the syntax of the Format string is also
*        determined by the current Frame.
*     TimeFrame
*        The TimeFrame class extends the syntax of the Format string to
*        allow the formatting of TimeFrame axis values as Gregorian calendar
*        dates and times. The syntax of TimeFrame Format strings is described
*        (below) in the "TimeFrame Formats" section.

*  SkyFrame Formats:
*     The Format string supplied for a SkyFrame should contain zero or
*     more of the following characters. These may occur in any order,
*     but the following is recommended for clarity:
*
*     - "+": Indicates that a plus sign should be prefixed to positive
*     values. By default, no plus sign is used.
*
*     - "z": Indicates that leading zeros should be prefixed to the
*     value so that the first field is of constant width, as would be
*     required in a fixed-width table (leading zeros are always
*     prefixed to any fields that follow). By default, no leading
*     zeros are added.
*
*     - "i": Use the standard ISO field separator (a colon) between
*     fields. This is the default behaviour.
*
*     - "b": Use a blank to separate fields.
*
*     - "l": Use a letter ("h"/"d", "m" or "s" as appropriate) to
*     separate fields.
*
*     - "g": Use a letter and symbols to separate fields ("h"/"d", "m" or "s",
*     etc, as appropriate), but include escape sequences in the formatted
*     value so that the Plot class will draw the separators as small
*     super-scripts.
c     The default escape sequences are optimised for the pgplot graphics
c     package, but new escape sequences may be specified using function
c     astSetSkyDelim.
*
*     - "d": Include a degrees field. Expressing the angle purely in
*     degrees is also the default if none of "h", "m", "s" or "t" are
*     given.
*
*     - "h": Express the angle as a time and include an hours field
*     (where 24 hours correspond to 360 degrees). Expressing the angle
*     purely in hours is also the default if "t" is given without
*     either "m" or "s".
*
*     - "m": Include a minutes field. By default this is not included.
*
*     - "s": Include a seconds field. By default this is not included.
*     This request is ignored if "d" or "h" is given, unless a minutes
*     field is also included.
*
*     - "t": Express the angle as a time (where 24 hours correspond to
*     360 degrees). This option is ignored if either "d" or "h" is
*     given and is intended for use where the value is to be expressed
*     purely in minutes and/or seconds of time (with no hours
*     field). If "t" is given without "d", "h", "m" or "s" being
*     present, then it is equivalent to "h".
*
*     - ".": Indicates that decimal places are to be given for the
*     final field in the formatted string (whichever field this
*     is). The "." should be followed immediately by an unsigned
*     integer which gives the number of decimal places required, or by an
*     asterisk. If an asterisk is supplied, a default number of decimal
*     places is used which is based on the value of the Digits
*     attribute.
*
*     All of the above format specifiers are case-insensitive. If
*     several characters make conflicting requests (e.g. if both "i"
*     and "b" appear), then the character occurring last takes
*     precedence, except that "d" and "h" always override "t".
*
*     If the format string starts with a percentage sign (%), then the
*     whole format string is assumed to conform to the syntax defined by
*     the Frame class, and the axis values is formated as a decimal
*     radians value.

*  TimeFrame Formats:
*     The Format string supplied for a TimeFrame should either use the
*     syntax defined by the base Frame class (i.e. a C "printf" format
*     string), or the extended "iso" syntax described below (the default
*     value is inherited from the Frame class):
*
*     - C "printf" syntax: If the Format string is a C "printf" format
*     description such as "%1.7G", the TimeFrame axis value will be
*     formatted without change as a floating point value using this format.
*     The formatted string will thus represent an offset from the zero point
*     specified by the TimeFrame's TimeOrigin attribute, measured in
*     units given by the TimeFrame's Unit attribute.
*
*     - "iso" syntax: This is used to format a TimeFrame axis value as a
*     Gregorian date followed by an optional time of day. If the Format
*     value commences with the string "iso" then the TimeFrame axis value
*     will be converted to an absolute MJD, including the addition of the
*     current TimeOrigin value, and then formatted as a Gregorian date
*     using the format "yyyy-mm-dd". Optionally, the Format value may
*     include an integer precision following the "iso" specification (e.g.
*     "iso.2"), in which case the time of day will be appended to the
*     formatted date (if no time of day is included, the date field is
*     rounded to the nearest day). The integer value in the Format string
*     indicates the number of decimal places to use in the seconds field. For
*     instance, a Format value of "iso.0" produces a time of day of the form
*     "hh:mm:ss", and a Format value of "iso.2" produces a time of day of the
*     form "hh:mm:ss.ss". The date and time fields will be separated by a
*     space unless 'T' is appended to the end of string, in which case
*     the letter T (upper case) will be used as the separator. The value of
*     the Digits attribute is ignored when using this "iso" format.

*  Notes:
*     - When specifying this attribute by name, it should be
*     subscripted with the number of the Frame axis to which it
*     applies.
*att--
*/
/* This simply provides an interface to the Axis methods for accessing
   the Format string. */
MAKE_CLEAR(Format)
MAKE_GET(Format,const char *,NULL,0,0)
MAKE_SET(Format,const char *)
MAKE_TEST(Format)

/*
*att++
*  Name:
*     Label(axis)

*  Purpose:
*     Axis label.

*  Type:
*     Public attribute.

*  Synopsis:
*     String.

*  Description:
*     This attribute specifies a label to be attached to each axis of
*     a Frame when it is represented (e.g.) in graphical output.
*
*     If a Label value has not been set for a Frame axis, then a
*     suitable default is supplied.

*  Applicability:
*     Frame
*        The default supplied by the Frame class is the string "Axis
*        <n>", where <n> is 1, 2, etc. for each successive axis.
*     SkyFrame
*        The SkyFrame class re-defines the default Label value
*        (e.g. to "Right ascension" or "Galactic latitude") as
*        appropriate for the particular celestial coordinate system
*        being represented.
*     TimeFrame
*        The TimeFrame class re-defines the default Label value as
*        appropriate for the particular time system being represented.
*     FrameSet
*        The Label attribute of a FrameSet axis is the same as that of
*        its current Frame (as specified by the Current attribute).

*  Notes:
*     - Axis labels are intended purely for interpretation by human
*     readers and not by software.
*     - When specifying this attribute by name, it should be
*     subscripted with the number of the Frame axis to which it
*     applies.
*att--
*/
/* This provides an interface to the Axis methods for accessing the
   Label string, but provides an alternative default Label based on
   the axis number.  This default string is written to the static
   "label_buff" buffer and a pointer to this is returned if
   required. */
MAKE_CLEAR(Label)
MAKE_GET(Label,const char *,NULL,1,GetDefaultLabel( axis, status ))
MAKE_SET(Label,const char *)
MAKE_TEST(Label)

/*
*att++
*  Name:
*     Symbol(axis)

*  Purpose:
*     Axis symbol.

*  Type:
*     Public attribute.

*  Synopsis:
*     String.

*  Description:
*     This attribute specifies a short-form symbol to be used to
*     represent coordinate values for a particular axis of a
*     Frame. This might be used (e.g.) in algebraic expressions where
*     a full description of the axis would be inappropriate. Examples
*     include "RA" and "Dec" (for Right Ascension and Declination).
*
*     If a Symbol value has not been set for a Frame axis, then a
*     suitable default is supplied.

*  Applicability:
*     Frame
*        The default Symbol value supplied by the Frame class is the
*        string "<Domain><n>", where <n> is 1, 2, etc. for successive
*        axes, and <Domain> is the value of the Frame's Domain
*        attribute (truncated if necessary so that the final string
*        does not exceed 15 characters). If no Domain value has been
*        set, "x" is used as the <Domain> value in constructing this
*        default string.
*     SkyFrame
*        The SkyFrame class re-defines the default Symbol value
*        (e.g. to "RA" or "Dec") as appropriate for the particular
*        celestial coordinate system being represented.
*     TimeFrame
*        The TimeFrame class re-defines the default Symbol value as
*        appropriate for the particular time system being represented.
*     FrameSet
*        The Symbol attribute of a FrameSet axis is the same as that
*        of its current Frame (as specified by the Current attribute).

*  Notes:
*     - When specifying this attribute by name, it should be
*     subscripted with the number of the Frame axis to which it
*     applies.
*att--
*/
/* This provides an interface to the Axis methods for accessing the
   Symbol string, but provides an alternative default Symbol based on
   the axis number and the Frame's Domain (if defined, otherwise "x"
   is used). This default string is written to the static
   "symbol_buff" buffer and a pointer to this is returned if
   required. */
MAKE_CLEAR(Symbol)
MAKE_GET(Symbol,const char *,NULL,1,GetDefaultSymbol( this, axis, status ) )
MAKE_SET(Symbol,const char *)
MAKE_TEST(Symbol)

/*
*att++
*  Name:
*     Unit(axis)

*  Purpose:
*     Axis physical units.

*  Type:
*     Public attribute.

*  Synopsis:
*     String.

*  Description:
*     This attribute contains a textual representation of the physical
*     units used to represent coordinate values on a particular axis
c     of a Frame. The astSetActiveUnit function controls how the Unit values
f     of a Frame. The AST_SETACTIVEUNIT routine controls how the Unit values
*     are used.

*  Applicability:
*     Frame
*        The default supplied by the Frame class is an empty string.
*     SkyFrame
*        The SkyFrame class re-defines the default Unit value (e.g. to
*        "hh:mm:ss.sss") to describe the character string returned by
c        the astFormat function when formatting coordinate values.
f        the AST_FORMAT function when formatting coordinate values.
*     SpecFrame
*        The SpecFrame class re-defines the default Unit value so that it
*        is appropriate for the current System value. See the System
*        attribute for details. An error will be reported if an attempt
*        is made to use an inappropriate Unit.
*     TimeFrame
*        The TimeFrame class re-defines the default Unit value so that it
*        is appropriate for the current System value. See the System
*        attribute for details. An error will be reported if an attempt
*        is made to use an inappropriate Unit (e.g. "km").
*     FrameSet
*        The Unit attribute of a FrameSet axis is the same as that of
*        its current Frame (as specified by the Current attribute).

*  Notes:
*     - When specifying this attribute by name, it should be
*     subscripted with the number of the Frame axis to which it
*     applies.
*att--
*/
/* This simply provides an interface to the Axis methods for accessing
   the Unit string. */
MAKE_GET(Unit,const char *,NULL,0,0)
MAKE_TEST(Unit)

/*
*att++
*  Name:
*     NormUnit(axis)

*  Purpose:
*     Normalised Axis physical units.

*  Type:
*     Public attribute.

*  Synopsis:
*     String, read-only.

*  Description:
*     The value of this read-only attribute is derived from the current
*     value of the Unit attribute. It will represent an equivalent system
*     of units to the Unit attribute, but will potentially be simplified.
*     For instance, if Unit is set to "s*(m/s)", the NormUnit value will
*     be "m". If no simplification can be performed, the value of the
*     NormUnit attribute will equal that of the Unit attribute.

*  Applicability:
*     Frame
*        All Frames have this attribute.

*  Notes:
*     - When specifying this attribute by name, it should be
*     subscripted with the number of the Frame axis to which it
*     applies.
*att--
*/
/* This simply provides an interface to the Axis methods for accessing
   the Unit string. */
MAKE_GET(NormUnit,const char *,NULL,0,0)

/* Implement member functions to access the attributes associated with
   the Frame as a whole using the macros defined for this purpose in
   the "object.h" file. */

/*
*att++
*  Name:
*     Digits/Digits(axis)

*  Purpose:
*     Number of digits of precision.

*  Type:
*     Public attribute.

*  Synopsis:
*     Integer.

*  Description:
*     This attribute specifies how many digits of precision are
*     required by default when a coordinate value is formatted for a
c     Frame axis (e.g. using astFormat). Its value may be set either
f     Frame axis (e.g. using AST_FORMAT). Its value may be set either
*     for a Frame as a whole, or (by subscripting the attribute name
*     with the number of an axis) for each axis individually. Any
*     value set for an individual axis will over-ride the value for
*     the Frame as a whole.
*
*     Note that the Digits value acts only as a means of determining a
*     default Format string. Its effects are over-ridden if a Format
*     string is set explicitly for an axis. However, if the Format
*     attribute specifies the precision using the string ".*", then
*     the Digits attribute is used to determine the number of decimal
*     places to produce.

*  Applicability:
*     Frame
*        The default Digits value supplied by the Frame class is 7. If
*        a value less than 1 is supplied, then 1 is used instead.
*     FrameSet
*        The Digits attribute of a FrameSet (or one of its axes) is
*        the same as that of its current Frame (as specified by the
*        Current attribute).
*     Plot
*        The default Digits value used by the Plot class when drawing
*        annotated axis labels is the smallest value which results in all
*        adjacent labels being distinct.
*     TimeFrame
*        The Digits attribute is ignored when a TimeFrame formats a value
*        as a date and time string (see the Format attribute).
*att--
*/
/* Clear the Digits value by setting it to -INT_MAX. */
astMAKE_CLEAR(Frame,Digits,digits,-INT_MAX)

/* Supply a default of 7 digits if no value has been set. */
astMAKE_GET(Frame,Digits,int,0,( ( this->digits != -INT_MAX ) ? this->digits :
                                                                7 ))

/* Constrain the Digits value being set to be at least 1. */
astMAKE_SET(Frame,Digits,int,digits,( value > 1 ? value : 1 ))

/* The Digits value is set if it is not -INT_MAX. */
astMAKE_TEST(Frame,Digits,( this->digits != -INT_MAX ))

/*
*att++
*  Name:
*     MatchEnd

*  Purpose:
*     Match trailing axes?

*  Type:
*     Public attribute.

*  Synopsis:
*     Integer (boolean).

*  Description:
*     This attribute is a boolean value which controls how a Frame
c     behaves when it is used (by astFindFrame) as a template to match
f     behaves when it is used (by AST_FINDFRAME) as a template to match
*     another (target) Frame. It applies only in the case where a
*     match occurs between template and target Frames with different
*     numbers of axes.
*
*     If the MatchEnd value of the template Frame is zero, then the
*     axes which occur first in the target Frame will be matched and
*     any trailing axes (in either the target or template) will be
*     disregarded. If it is non-zero, the final axes in each Frame
*     will be matched and any un-matched leading axes will be
*     disregarded instead.

*  Applicability:
*     Frame
*        The default MatchEnd value for a Frame is zero, so that
*        trailing axes are disregarded.
*     FrameSet
*        The MatchEnd attribute of a FrameSet is the same as that of
*        its current Frame (as specified by the Current attribute).
*att--
*/
/* Clear the MatchEnd value by setting it to -INT_MAX. */
astMAKE_CLEAR(Frame,MatchEnd,match_end,-INT_MAX)

/* Supply a default of 0 if no MatchEnd value has been set. */
astMAKE_GET(Frame,MatchEnd,int,0,( ( this->match_end != -INT_MAX ) ?
                                   this->match_end : 0 ))

/* Set a MatchEnd value of 1 if any non-zero value is supplied. */
astMAKE_SET(Frame,MatchEnd,int,match_end,( value != 0 ))

/* The MatchEnd value is set if it is not -INT_MAX. */
astMAKE_TEST(Frame,MatchEnd,( this->match_end != -INT_MAX ))

/*
*att++
*  Name:
*     MaxAxes

*  Purpose:
*     Maximum number of Frame axes to match.

*  Type:
*     Public attribute.

*  Synopsis:
*     Integer.

*  Description:
*     This attribute controls how a Frame behaves when it is used (by
c     astFindFrame) as a template to match another (target) Frame. It
f     AST_FINDFRAME) as a template to match another (target) Frame. It
*     specifies the maximum number of axes that the target Frame may
*     have in order to match the template.
*
*     Normally, this value will equal the number of Frame axes, so
*     that a template Frame will only match another Frame with the
*     same number of axes as itself. By setting a different value,
*     however, the matching process may be used to identify Frames
*     with specified numbers of axes.

*  Applicability:
*     Frame
*        The default MaxAxes value for a Frame is equal to the number
*        of Frame axes (Naxes attribute).
*     CmpFrame
*        The MaxAxes attribute of a CmpFrame defaults to a large number
*        (1000000) which is much larger than any likely number of axes in
*        a Frame. Combined with the MinAxes default of zero (for a
*        CmpFrame), this means that the default behaviour for a CmpFrame
*        is to match any target Frame that consists of a subset of the
*        axes in the template CmpFrame. To change this so that a CmpFrame
*        will only match Frames that have the same number of axes, you
*        should set the CmpFrame MaxAxes and MinAxes attributes to the
*        number of axes in the CmpFrame.
*     FrameSet
*        The MaxAxes attribute of a FrameSet is the same as that of
*        its current Frame (as specified by the Current attribute).

*  Notes:
*     - When setting a MaxAxes value, the value of the MinAxes
*     attribute may also be silently changed so that it remains
*     consistent with (i.e. does not exceed) the new value. The
*     default MaxAxes value may also be reduced to remain consistent
*     with the MinAxes value.
*     - If a template Frame is used to match a target with a different
*     number of axes, the MatchEnd attribute of the template is used
*     to determine how the individual axes of each Frame should match.
*att--
*/
/* Clear the MaxAxes value by setting it to -INT_MAX. */
astMAKE_CLEAR(Frame,MaxAxes,max_axes,-INT_MAX)

/* Use the DefaultMaxAxes and ConsistentMaxAxes functions (defined earlier) for
   the Get and Set operations to ensure that MinAxes and MaxAxes values remain
   consistent. */
astMAKE_GET(Frame,MaxAxes,int,0,DefaultMaxAxes( this, status ))
astMAKE_SET(Frame,MaxAxes,int,max_axes,ConsistentMaxAxes( this, value, status ))

/* The MaxAxes value is set if it is not -INT_MAX. */
astMAKE_TEST(Frame,MaxAxes,( this->max_axes != -INT_MAX ))

/*
*att++
*  Name:
*     MinAxes

*  Purpose:
*     Minimum number of Frame axes to match.

*  Type:
*     Public attribute.

*  Synopsis:
*     Integer.

*  Description:
*     This attribute controls how a Frame behaves when it is used (by
c     astFindFrame) as a template to match another (target) Frame. It
f     AST_FINDFRAME) as a template to match another (target) Frame. It
*     specifies the minimum number of axes that the target Frame may
*     have in order to match the template.
*
*     Normally, this value will equal the number of Frame axes, so
*     that a template Frame will only match another Frame with the
*     same number of axes as itself. By setting a different value,
*     however, the matching process may be used to identify Frames
*     with specified numbers of axes.

*  Applicability:
*     Frame
*        The default MinAxes value for a Frame is equal to the number
*        of Frame axes (Naxes attribute).
*     CmpFrame
*        The MinAxes attribute of a CmpFrame defaults to zero. Combined
*        with the MaxAxes default of 1000000 (for a CmpFrame), this means
*        that the default behaviour for a CmpFrame is to match any target
*        Frame that consists of a subset of the axes in the template
*        CmpFrame. To change this so that a CmpFrame will only match Frames
*        that have the same number of axes, you should set the CmpFrame
*        MinAxes and MaxAxes attributes to the number of axes in the CmpFrame.
*     FrameSet
*        The MinAxes attribute of a FrameSet is the same as that of
*        its current Frame (as specified by the Current attribute).

*  Notes:
*     - When setting a MinAxes value, the value of the MaxAxes
*     attribute may also be silently changed so that it remains
*     consistent with (i.e. is not less than) the new value. The
*     default MinAxes value may also be reduced to remain consistent
*     with the MaxAxes value.
*     - If a template Frame is used to match a target with a different
*     number of axes, the MatchEnd attribute of the template is used
*     to determine how the individual axes of each Frame should match.
*att--
*/
/* Clear the MinAxes value by setting it to -INT_MAX. */
astMAKE_CLEAR(Frame,MinAxes,min_axes,-INT_MAX)

/* Use the DefaultMinAxes and ConsistentMinAxes functions (defined earlier) for
   the Get and Set operations to ensure that MinAxes and MaxAxes values remain
   consistent. */
astMAKE_GET(Frame,MinAxes,int,0,DefaultMinAxes( this, status ))
astMAKE_SET(Frame,MinAxes,int,min_axes,ConsistentMinAxes( this, value, status ))

/* The MinAxes value is set if it is not -INT_MAX. */
astMAKE_TEST(Frame,MinAxes,( this->min_axes != -INT_MAX ))

/*
*att++
*  Name:
*     Domain

*  Purpose:
*     Coordinate system domain.

*  Type:
*     Public attribute.

*  Synopsis:
*     String.

*  Description:
*     This attribute contains a string which identifies the physical
*     domain of the coordinate system that a Frame describes.
*
*     The Domain attribute also controls how a Frame behaves when it is
c     used (by astFindFrame) as a template to match another (target)
f     used (by AST_FINDFRAME) as a template to match another (target)
*     Frame. It does this by specifying the Domain that the target
*     Frame should have in order to match the template. If the Domain
*     value in the template Frame is set, then only targets with the
*     same Domain value will be matched. If the template's Domain
*     value is not set, however, then the target's Domain will be
*     ignored.

*  Applicability:
*     Frame
*        The default Domain value supplied by the Frame class is an
*        empty string.
*     SkyFrame
*        The SkyFrame class re-defines the default Domain value to be
*        "SKY".
*     CmpFrame
*        The CmpFrame class re-defines the default Domain value to be
*        of the form "<dom1>-<dom2>", where <dom1> and <dom2> are the
*        Domains of the two component Frames. If both these Domains are
*        blank, then the string "CMP" is used as the default Domain name.
*     FrameSet
*        The Domain attribute of a FrameSet is the same as that of its
*        current Frame (as specified by the Current attribute).
*     SpecFrame
*        The SpecFrame class re-defines the default Domain value to be
*        "SPECTRUM".
*     DSBSpecFrame
*        The DSBSpecFrame class re-defines the default Domain value to be
*        "DSBSPECTRUM".
*     FluxFrame
*        The FluxFrame class re-defines the default Domain value to be
*        "FLUX".
*     SpecFluxFrame
*        The FluxFrame class re-defines the default Domain value to be
*        "SPECTRUM-FLUX".
*     TimeFrame
*        The TimeFrame class re-defines the default Domain value to be
*        "TIME".

*  Notes:
*     - All Domain values are converted to upper case and white space
*     is removed before use.
*att--
*/
/* Clear the Domain value by freeing the allocated memory and
   assigning a NULL pointer. */
astMAKE_CLEAR(Frame,Domain,domain,astFree( this->domain ))

/* If the Domain value is not set, supply a default in the form of a
   pointer to the constant string "". */
astMAKE_GET(Frame,Domain,const char *,NULL,( this->domain ? this->domain :
                                                            "" ))

/* Set a Domain value by freeing any previously allocated memory,
   allocating new memory, storing the string, removing white space,
   converting to upper case and saving the pointer to the cleaned
   copy. */
astMAKE_SET(Frame,Domain,const char *,domain,CleanDomain(
                                                astStore( this->domain,
                                    value, strlen( value ) + (size_t) 1 ), status ))

/* The Domain value is set if the pointer to it is not NULL. */
astMAKE_TEST(Frame,Domain,( this->domain != NULL ))

/*
*att++
*  Name:
*     Permute

*  Purpose:
*     Permute axis order?

*  Type:
*     Public attribute.

*  Synopsis:
*     String.

*  Description:
*     This attribute is a boolean value which controls how a Frame
c     behaves when it is used (by astFindFrame) as a template to match
f     behaves when it is used (by AST_FINDFRAME) as a template to match
*     another (target) Frame. It specifies whether the axis order of
*     the target Frame may be permuted in order to obtain a match.
*
*     If the template's Permute value is zero, it will match a target
*     only if it can do so without changing the order of its
*     axes. Otherwise, it will attempt to permute the target's axes as
*     necessary.
*
*     The default value is 1, so that axis permutation will be attempted.

*  Applicability:
*     Frame
*        All Frames have this attribute. However, the Frame class
*        effectively ignores this attribute and behaves as if it has
*        the value 1. This is because the axes of a basic Frame are
*        not distinguishable and will always match any other Frame
*        whatever their order.
*     SkyFrame
*        Unlike a basic Frame, the SkyFrame class makes use of this
*        attribute.
*     FrameSet
*        The Permute attribute of a FrameSet is the same as that of
*        its current Frame (as specified by the Current attribute).
*att--
*/
/* Clear the Permute value by setting it to -INT_MAX. */
astMAKE_CLEAR(Frame,Permute,permute,-INT_MAX)

/* Supply a default of 1 if no Permute value has been set. */
astMAKE_GET(Frame,Permute,int,0,( ( this->permute != -INT_MAX ) ?
                                  this->permute : 1 ))

/* Set a Permute value of 1 if any non-zero value is supplied. */
astMAKE_SET(Frame,Permute,int,permute,( value != 0 ))

/* The Permute value is set if it is not -INT_MAX. */
astMAKE_TEST(Frame,Permute,( this->permute != -INT_MAX ))

/*
*att++
*  Name:
*     PreserveAxes

*  Purpose:
*     Preserve axes?

*  Type:
*     Public attribute.

*  Synopsis:
*     Integer (boolean).

*  Description:
*     This attribute controls how a Frame behaves when it is used (by
c     astFindFrame) as a template to match another (target) Frame. It
f     AST_FINDFRAME) as a template to match another (target) Frame. It
*     determines which axes appear (and in what order) in the "result"
*     Frame produced.
*
*     If PreserveAxes is zero in the template Frame, then the result
*     Frame will have the same number (and order) of axes as the
*     template. If it is non-zero, however, the axes of the target
*     Frame will be preserved, so that the result Frame will have the
*     same number (and order) of axes as the target.
*
*     The default value is zero, so that target axes are not preserved
*     and the result Frame resembles the template.

*  Applicability:
*     Frame
*        All Frames have this attribute.
*     FrameSet
*        The PreserveAxes attribute of a FrameSet is the same as that
*        of its current Frame (as specified by the Current attribute).
*att--
*/
/* Clear the PreserveAxes value by setting it to -INT_MAX. */
astMAKE_CLEAR(Frame,PreserveAxes,preserve_axes,-INT_MAX)

/* Supply a default of 0 if no PreserveAxes value has been set. */
astMAKE_GET(Frame,PreserveAxes,int,0,( ( this->preserve_axes != -INT_MAX ) ?
                                       this->preserve_axes : 0 ))

/* Set a PreserveAxes value of 1 if any non-zero value is supplied. */
astMAKE_SET(Frame,PreserveAxes,int,preserve_axes,( value != 0 ))

/* The PreserveAxes value is set if it is not -INT_MAX. */
astMAKE_TEST(Frame,PreserveAxes,( this->preserve_axes != -INT_MAX ))

/*
*att++
*  Name:
*     AlignSystem

*  Purpose:
*     Coordinate system in which to align the Frame.

*  Type:
*     Public attribute.

*  Synopsis:
*     String.

*  Description:
*     This attribute controls how a Frame behaves when it is used (by
c     astFindFrame or astConvert) as a template to match another (target)
f     AST_FINDFRAME or AST_CONVERT) as a template to match another (target)
*     Frame. It identifies the coordinate system in which the two Frames
*     will be aligned by the match.
*
*     The values which may be assigned to this attribute, and its default
*     value, depend on the class of Frame and are described in the
*     "Applicability" section below. In general, the AlignSystem attribute
*     will accept any of the values which may be assigned to the System
*     attribute.
*
c     The Mapping returned by AST_FINDFRAME or AST_CONVERT will use the
f     The Mapping returned by astFindFrame or astConvert will use the
*     coordinate system specified by the AlignSystem attribute as an
*     intermediate coordinate system. The total returned Mapping will first
*     map positions from the first Frame into this intermediate coordinate
*     system, using the attributes of the first Frame. It will then map
*     these positions from the intermediate coordinate system into the
*     second Frame, using the attributes of the second Frame.

*  Applicability:
*     Frame
*        The AlignSystem attribute for a basic Frame always equals "Cartesian",
*        and may not be altered.
*     CmpFrame
*        The AlignSystem attribute for a CmpFrame always equals "Compound",
*        and may not be altered.
*     FrameSet
*        The AlignSystem attribute of a FrameSet is the same as that of its
*        current Frame (as specified by the Current attribute).
*     SkyFrame
*        The default AlignSystem attribute for a SkyFrame is "ICRS".
*     SpecFrame
*        The default AlignSystem attribute for a SpecFrame is "Wave"
*        (wavelength).
*     TimeFrame
*        The default AlignSystem attribute for a TimeFrame is "MJD".
*att--
*/
/* Clear the AlignSystem value by setting it to AST__BADSYSTEM. */
astMAKE_CLEAR(Frame,AlignSystem,alignsystem,AST__BADSYSTEM)

/* Provide a default AlignSystem of AST__CART. */
astMAKE_GET(Frame,AlignSystem,AstSystemType,AST__BADSYSTEM,(
            ( this->alignsystem == AST__BADSYSTEM ) ? AST__CART : this->alignsystem ) )

/* Validate the AlignSystem value being set and retain the original if the
   supplied value is not recognized. */
astMAKE_SET(Frame,AlignSystem,AstSystemType,alignsystem,(
           (astValidateSystem( this, value, "astSetAlignSystem" ) != AST__BADSYSTEM) ?
            value : this->alignsystem ))

/* The AlignSystem value is set if it is not AST__BADSYSTEM. */
astMAKE_TEST(Frame,AlignSystem,( this->alignsystem != AST__BADSYSTEM ))

/*
*att++
*  Name:
*     System

*  Purpose:
*     Coordinate system used to describe positions within the domain

*  Type:
*     Public attribute.

*  Synopsis:
*     String.

*  Description:
*     In general it is possible for positions within a given physical
*     domain to be described using one of several different coordinate
*     systems. For instance, the SkyFrame class can use galactic
*     coordinates, equatorial coordinates, etc, to describe positions on
*     the sky. As another example, the SpecFrame class can use frequency,
*     wavelength, velocity, etc, to describe a position within an
*     electromagnetic spectrum. The System attribute identifies the particular
*     coordinate system represented by a Frame. Each class of Frame
*     defines a set of acceptable values for this attribute, as listed
*     below (all are case insensitive). Where more than one alternative
*     System value is shown, the first of will be returned when an
*     enquiry is made.

*  Applicability:
*     Frame
*        The System attribute for a basic Frame always equals "Cartesian",
*        and may not be altered.
*     CmpFrame
*        The System attribute for a CmpFrame always equals "Compound",
*        and may not be altered. In addition, the CmpFrame class allows
*        the System attribute to be referenced for a component Frame by
*        including the index of an axis within the required component
*        Frame. For instance, "System(3)" refers to the System attribute
*        of the component Frame which includes axis 3 of the CmpFrame.
*     FrameSet
*        The System attribute of a FrameSet is the same as that of its
*        current Frame (as specified by the Current attribute).
*     SkyFrame
*        The SkyFrame class supports the following System values and
*        associated celestial coordinate systems:
*
*        - "AZEL": Horizon coordinates. The longitude axis is azimuth
*        such that geographic north has an azimuth of zero and geographic
*        east has an azimuth of +PI/2 radians. The zenith has elevation
*        +PI/2. When converting to and from other celestial coordinate
*        systems, no corrections are applied for atmospheric refraction
*        or polar motion (however, a correction for diurnal aberattion is
*        applied). Note, unlike most other
*        celestial coordinate systems, this system is right handed. Also,
*        unlike other SkyFrame systems, the AzEl system is sensitive to
*        the timescale in which the Epoch value is supplied. This is
*        because of the gross diurnal rotation which this system undergoes,
*        causing a small change in time to translate to a large rotation.
*        When converting to or from an AzEl system, the Epoch value for
*        both source and destination SkyFrames should be supplied in the
*        TDB timescale. The difference between TDB and TT is between 1
*        and 2 milliseconds, and so a TT value can usually be supplied in
*        place of a TDB value. The TT timescale is related to TAI via
*        TT = TAI + 32.184 seconds.
*
*        - "ECLIPTIC": Ecliptic coordinates (IAU 1980), referred to the
*        ecliptic and mean equinox specified by the qualifying Equinox
*        value.
*
*        - "FK4": The old FK4 (barycentric) equatorial coordinate system,
*        which should be qualified by an Equinox value. The underlying
*        model on which this is based is non-inertial and rotates slowly
*        with time, so for accurate work FK4 coordinate systems should
*        also be qualified by an Epoch value.
*
*        - "FK4-NO-E" or "FK4_NO_E": The old FK4 (barycentric) equatorial
*        system but without the "E-terms of aberration" (e.g. some radio
*        catalogues). This coordinate system should also be qualified by
*        both an Equinox and an Epoch value.
*
*        - "FK5" or "EQUATORIAL": The modern FK5 (barycentric) equatorial
*        coordinate system. This should be qualified by an Equinox value.
*
*        - "GALACTIC": Galactic coordinates (IAU 1958).
*
*        - "GAPPT", "GEOCENTRIC" or "APPARENT": The geocentric apparent
*        equatorial coordinate system, which gives the apparent positions
*        of sources relative to the true plane of the Earth's equator and
*        the equinox (the coordinate origin) at a time specified by the
*        qualifying Epoch value. (Note that no Equinox is needed to
*        qualify this coordinate system because no model "mean equinox"
*        is involved.)  These coordinates give the apparent right
*        ascension and declination of a source for a specified date of
*        observation, and therefore form an approximate basis for
*        pointing a telescope. Note, however, that they are applicable to
*        a fictitious observer at the Earth's centre, and therefore
*        ignore such effects as atmospheric refraction and the (normally
*        much smaller) aberration of light due to the rotational velocity
*        of the Earth's surface.  Geocentric apparent coordinates are
*        derived from the standard FK5 (J2000.0) barycentric coordinates
*        by taking account of the gravitational deflection of light by
*        the Sun (usually small), the aberration of light caused by the
*        motion of the Earth's centre with respect to the barycentre
*        (larger), and the precession and nutation of the Earth's spin
*        axis (normally larger still).
*
*        - "HELIOECLIPTIC": Ecliptic coordinates (IAU 1980), referred to the
*        ecliptic and mean equinox of J2000.0, in which an offset is added to
*        the longitude value which results in the centre of the sun being at
*        zero longitude at the date given by the Epoch attribute. Attempts to
*        set a value for the Equinox attribute will be ignored, since this
*        system is always referred to J2000.0.
*
*        - "ICRS": The Internation Celestial Reference System, realised
*        through the Hipparcos catalogue. Whilst not an equatorial system
*        by definition, the ICRS is very close to the FK5 (J2000) system
*        and is usually treated as an equatorial system. The distinction
*        between ICRS and FK5 (J2000) only becomes important when accuracies
*        of 50 milli-arcseconds or better are required. ICRS need not be
*        qualified by an Equinox value.
*
*        - "J2000": An equatorial coordinate system based on the mean
*        dynamical equator and equinox of the J2000 epoch. The dynamical
*        equator and equinox differ slightly from those used by the FK5
*        model, and so a "J2000" SkyFrame will differ slightly from an
*        "FK5(Equinox=J2000)" SkyFrame. The J2000 System need not be
*        qualified by an Equinox value
*
*        - "SUPERGALACTIC": De Vaucouleurs Supergalactic coordinates.
*
*        - "UNKNOWN": Any other general spherical coordinate system. No
*        Mapping can be created between a pair of SkyFrames if either of the
*        SkyFrames has System set to "Unknown".
*
*        Currently, the default System value is "ICRS". However, this
*        default may change in future as new astrometric standards
*        evolve. The intention is to track the most modern appropriate
*        standard. For this reason, you should use the default only if
*        this is what you intend (and can tolerate any associated slight
*        change in future). If you intend to use the ICRS system
*        indefinitely, then you should specify it explicitly.
*     SpecFrame
*        The SpecFrame class supports the following System values and
*        associated spectral coordinate systems (the default is "WAVE" -
*        wavelength). They are all defined in FITS-WCS paper III:
*
*        - "FREQ": Frequency (GHz)
*        - "ENER" or "ENERGY": Energy (J)
*        - "WAVN" or "WAVENUM": Wave-number (1/m)
*        - "WAVE" or "WAVELEN": Vacuum wave-length (Angstrom)
*        - "AWAV" or "AIRWAVE": Wave-length in air (Angstrom)
*        - "VRAD" or "VRADIO": Radio velocity (km/s)
*        - "VOPT" or "VOPTICAL": Optical velocity (km/s)
*        - "ZOPT" or "REDSHIFT": Redshift (dimensionless)
*        - "BETA": Beta factor (dimensionless)
*        - "VELO" or "VREL": Apparent radial ("relativistic") velocity (km/s)
*
*        The default value for the Unit attribute for each system is shown
*        in parentheses. Note that the default value for the ActiveUnit flag
c        is non-zero
f        is .TRUE.
*        for a SpecFrame, meaning that changes to the Unit attribute for
*        a SpecFrame will result in the SpecFrame being re-mapped within
*        its enclosing FrameSet in order to reflect the change in units
c        (see astSetActiveUnit function for further information).
f        (see AST_SETACTIVEUNIT routine for further information).
*     TimeFrame
*        The TimeFrame class supports the following System values and
*        associated coordinate systems (the default is "MJD"):
*
*        - "MJD": Modified Julian Date (d)
*        - "JD": Julian Date (d)
*        - "JEPOCH": Julian epoch (yr)
*        - "BEPOCH": Besselian (yr)
*
*        The default value for the Unit attribute for each system is shown
*        in parentheses. Strictly, these systems should not allow changes
*        to be made to the units. For instance, the usual definition of
*        "MJD" and "JD" include the statement that the values will be in
*        units of days. However, AST does allow the use of other units
*        with all the above supported systems (except BEPOCH), on the
*        understanding that conversion to the "correct" units involves
*        nothing more than a simple scaling (1 yr = 365.25 d, 1 d = 24 h,
*        1 h = 60 min, 1 min = 60 s). Besselian epoch values are defined
*        in terms of tropical years of 365.2422 days, rather than the
*        usual Julian year of 365.25 days. Therefore, to avoid any
*        confusion, the Unit attribute is automatically cleared to "yr" when
*        a System value of BEPOCH System is selected, and an error is
*        reported if any attempt is subsequently made to change the Unit
*        attribute.
*
*        Note that the default value for the ActiveUnit flag
c        is non-zero
f        is .TRUE.
*        for a TimeFrame, meaning that changes to the Unit attribute for
*        a TimeFrame will result in the TimeFrame being re-mapped within
*        its enclosing FrameSet in order to reflect the change in units
c        (see astSetActiveUnit function for further information).
f        (see AST_SETACTIVEUNIT routine for further information).
*     FluxFrame
*        The FluxFrame class supports the following System values and
*        associated systems for measuring observed value:
*
*        - "FLXDN": Flux per unit frequency (W/m^2/Hz)
*        - "FLXDNW": Flux per unit wavelength (W/m^2/Angstrom)
*        - "SFCBR": Surface brightness in frequency units (W/m^2/Hz/arcmin**2)
*        - "SFCBRW": Surface brightness in wavelength units (W/m^2/Angstrom/arcmin**2)
*
*        The above lists specified the default units for each System. If an
*        explicit value is set for the Unit attribute but no value is set
*        for System, then the default System value is determined by the Unit
*        string (if the units are not appropriate for describing any of the
*        supported Systems then an error will be reported when an attempt is
*        made to access the System value). If no value has been specified for
*        either Unit or System, then System=FLXDN and Unit=W/m^2/Hz are
*        used.
*att--
*/
/* Clear the System value by setting it to AST__BADSYSTEM. */
astMAKE_CLEAR(Frame,System,system,AST__BADSYSTEM)

/* Provide a default coordinate system of AST__CART. */
astMAKE_GET(Frame,System,AstSystemType,AST__BADSYSTEM,(
            ( this->system == AST__BADSYSTEM ) ? AST__CART : this->system ) )

/* Validate the System value being set and retain the original if the
   supplied value is not recognized. */
astMAKE_SET(Frame,System,AstSystemType,system,(
           (astValidateSystem( this, value, "astSetSystem" ) != AST__BADSYSTEM) ?
            value : this->system ))

/* The System value is set if it is not AST__BADSYSTEM. */
astMAKE_TEST(Frame,System,( this->system != AST__BADSYSTEM ))

/*
*att++
*  Name:
*     Title

*  Purpose:
*     Frame title.

*  Type:
*     Public attribute.

*  Synopsis:
*     String.

*  Description:
*     This attribute holds a string which is used as a title in (e.g.)
*     graphical output to describe the coordinate system which a Frame
*     represents. Examples might be "Detector Coordinates" or
*     "Galactic Coordinates".
*
*     If a Title value has not been set for a Frame, then a suitable
*     default is supplied, depending on the class of the Frame.

*  Applicability:
*     Frame
*        The default supplied by the Frame class is "<n>-d coordinate
*        system", where <n> is the number of Frame axes (Naxes
*        attribute).
*     CmpFrame
*        The CmpFrame class re-defines the default Title value to be
*        "<n>-d compound coordinate system", where <n> is the number
*        of CmpFrame axes (Naxes attribute).
*     FrameSet
*        The Title attribute of a FrameSet is the same as that of its
*        current Frame (as specified by the Current attribute).

*  Notes:
*     - A Frame's Title is intended purely for interpretation by human
*     readers and not by software.
*att--
*/
/* Clear the Title value by freeing the allocated memory and assigning
   a NULL pointer. */
astMAKE_CLEAR(Frame,Title,title,astFree( this->title ))

/* If the Title value is not set, write a default based on the number of Frame
   axes into the static "title_buff" buffer, and return a pointer to this
   buffer. */
astMAKE_GET(Frame,Title,const char *,NULL,( this->title ?
                                            this->title : GetDefaultTitle( this, status ) ))

/* Set a Title value by freeing any previously allocated memory, allocating
   new memory, storing the string and saving the pointer to the copy. */
astMAKE_SET(Frame,Title,const char *,title,astStore( this->title, value,
                                           strlen( value ) + (size_t) 1 ))

/* The Title value is set if the pointer to it is not NULL. */
astMAKE_TEST(Frame,Title,( this->title != NULL ))

/*
*att++
*  Name:
*     ObsLat

*  Purpose:
*     The geodetic latitude of the observer

*  Type:
*     Public attribute.

*  Synopsis:
*     String.

*  Description:
*     This attribute specifies the geodetic latitude of the observer, in
*     degrees, relative to the IAU 1976 reference ellipsoid. The basic Frame
*     class makes no use of this attribute, but specialised subclasses of
*     Frame may use it. For instance, the SpecFrame, SkyFrame and TimeFrame
*     classes use it. The default value is zero.
*
*     The value is stored internally in radians, but is converted to and
*     from a degrees string for access. Some example input formats are:
*     "22:19:23.2", "22 19 23.2", "22:19.387", "22.32311", "N22.32311",
*     "-45.6", "S45.6". As indicated, the sign of the latitude can
*     optionally be indicated using characters "N" and "S" in place of the
*     usual "+" and "-". When converting the stored value to a string, the
*     format "[s]dd:mm:ss.ss" is used, when "[s]" is "N" or "S".

*  Applicability:
*     Frame
*        All Frames have this attribute.
*     SpecFrame
*        Together with the ObsLon, Epoch, RefRA and RefDec attributes,
*        it defines the Doppler shift introduced by the observers diurnal
*        motion around the earths axis, which is needed when converting to
*        or from the topocentric standard of rest. The maximum velocity
*        error which can be caused by an incorrect value is 0.5 km/s. The
*        default value for the attribute is zero.
*     TimeFrame
*        Together with the ObsLon attribute, it is used when converting
*        between certain time scales (TDB, TCB, LMST, LAST)

*att--
*/
/* The geodetic latitude of the observer (radians). Clear the ObsLat value by
   setting it to AST__BAD, returning zero as the default value. Any value is
   acceptable. */
astMAKE_CLEAR(Frame,ObsLat,obslat,AST__BAD)
astMAKE_GET(Frame,ObsLat,double,0.0,((this->obslat!=AST__BAD)?this->obslat:0.0))
astMAKE_SET(Frame,ObsLat,double,obslat,value)
astMAKE_TEST(Frame,ObsLat,(this->obslat!=AST__BAD))


/*
*att++
*  Name:
*     ObsAlt

*  Purpose:
*     The geodetic altitude of the observer

*  Type:
*     Public attribute.

*  Synopsis:
*     String.

*  Description:
*     This attribute specifies the geodetic altitude of the observer, in
*     metres, relative to the IAU 1976 reference ellipsoid. The basic Frame
*     class makes no use of this attribute, but specialised subclasses of
*     Frame may use it. For instance, the SpecFrame, SkyFrame and TimeFrame
*     classes use it. The default value is zero.

*  Applicability:
*     Frame
*        All Frames have this attribute.
*     SpecFrame
*        Together with the ObsLon, Epoch, RefRA and RefDec attributes,
*        it defines the Doppler shift introduced by the observers diurnal
*        motion around the earths axis, which is needed when converting to
*        or from the topocentric standard of rest. The maximum velocity
*        error which can be caused by an incorrect value is 0.5 km/s. The
*        default value for the attribute is zero.
*     TimeFrame
*        Together with the ObsLon attribute, it is used when converting
*        between certain time scales (TDB, TCB, LMST, LAST)

*att--
*/
/* The geodetic altitude of the observer (metres). Clear the ObsAlt value by
   setting it to AST__BAD, returning zero as the default value. Any value is
   acceptable. */
astMAKE_CLEAR(Frame,ObsAlt,obsalt,AST__BAD)
astMAKE_GET(Frame,ObsAlt,double,0.0,((this->obsalt!=AST__BAD)?this->obsalt:0.0))
astMAKE_SET(Frame,ObsAlt,double,obsalt,value)
astMAKE_TEST(Frame,ObsAlt,(this->obsalt!=AST__BAD))


/*
*att++
*  Name:
*     ObsLon

*  Purpose:
*     The geodetic longitude of the observer

*  Type:
*     Public attribute.

*  Synopsis:
*     String.

*  Description:
*     This attribute specifies the geodetic (or equivalently, geocentric)
*     longitude of the observer, in degrees, measured positive eastwards.
*     See also attribute ObsLat. The basic Frame class makes no use of this
*     attribute, but specialised subclasses of Frame may use it. For instance,
*     the SpecFrame, SkyFrame and TimeFrame classes use it. The default value
*     is zero.
*
*     The value is stored internally in radians, but is converted to and
*     from a degrees string for access. Some example input formats are:
*     "155:19:23.2", "155 19 23.2", "155:19.387", "155.32311", "E155.32311",
*     "-204.67689", "W204.67689". As indicated, the sign of the longitude can
*     optionally be indicated using characters "E" and "W" in place of the
*     usual "+" and "-". When converting the stored value to a string, the
*     format "[s]ddd:mm:ss.ss" is used, when "[s]" is "E" or "W" and the
*     numerical value is chosen to be less than 180 degrees.

*  Applicability:
*     Frame
*        All Frames have this attribute.
*     SpecFrame
*        Together with the ObsLon, Epoch, RefRA and RefDec attributes,
*        it defines the Doppler shift introduced by the observers diurnal
*        motion around the earths axis, which is needed when converting to
*        or from the topocentric standard of rest. The maximum velocity
*        error which can be caused by an incorrect value is 0.5 km/s. The
*        default value for the attribute is zero.
*     TimeFrame
*        Together with the ObsLon attribute, it is used when converting
*        between certain time scales (TDB, TCB, LMST, LAST)

*att--
*/
/* The geodetic longitude of the observer (radians). Clear the ObsLon value by
   setting it to AST__BAD, returning zero as the default value. Any value is
   acceptable. */
astMAKE_CLEAR(Frame,ObsLon,obslon,AST__BAD)
astMAKE_GET(Frame,ObsLon,double,0.0,((this->obslon!=AST__BAD)?this->obslon:0.0))
astMAKE_SET(Frame,ObsLon,double,obslon,value)
astMAKE_TEST(Frame,ObsLon,(this->obslon!=AST__BAD))


/* Copy constructor. */
/* ----------------- */
static void Copy( const AstObject *objin, AstObject *objout, int *status ) {
/*
*  Name:
*     Copy

*  Purpose:
*     Copy constructor for Frame objects.

*  Type:
*     Private function.

*  Synopsis:
*     void Copy( const AstObject *objin, AstObject *objout, int *status )

*  Description:
*     This function implements the copy constructor for Frame objects.

*  Parameters:
*     objin
*        Pointer to the object to be copied.
*     objout
*        Pointer to the object being constructed.
*     status
*        Pointer to the inherited status variable.

*  Notes:
*     -  This constructor makes a deep copy.
*/

/* Local Variables: */
   AstFrame *in;                 /* Pointer to input Frame */
   AstFrame *out;                /* Pointer to output Frame */
   int axis;                     /* Loop counter for axes */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain pointers to the input and output Frames. */
   in = (AstFrame *) objin;
   out = (AstFrame *) objout;

/* For safety, first clear any references to the input memory from
   the output Frame. */
   out->axis = NULL;
   out->domain = NULL;
   out->perm = NULL;
   out->title = NULL;

/* If necessary, allocate memory in the output Frame and store a copy of the
   input Title and Domain strings. */
   if ( in->title ) out->title = astStore( NULL, in->title,
                                           strlen( in->title ) + (size_t) 1 );
   if ( in->domain ) out->domain = astStore( NULL, in->domain,
                                             strlen( in->domain ) +
                                             (size_t) 1 );

/* Allocate memory to hold the output Frame's Axis object pointers and its axis
   permutation array. */
   out->axis = astMalloc( sizeof( AstAxis * ) * (size_t) in->naxes );
   out->perm = astMalloc( sizeof( int ) * (size_t) in->naxes );

/* Make a copy of each of the input Frame's Axis objects, storing the pointer
   to each new Axis in the memory just allocated. Also copy the axis
   permutation array. */
   if ( astOK ) {
      for ( axis = 0; axis < in->naxes; axis++ ) {
         out->axis[ axis ] = astCopy( in->axis[ axis ] );
         out->perm[ axis ] = in->perm[ axis ];
      }

/* If an error occurred while copying the Axis objects, then loop through the
   resulting array of pointers and make sure that all of them are properly
   annulled. */
      if ( !astOK ) {
         for ( axis = 0; axis < in->naxes; axis++ ) {
            out->axis[ axis ] = astAnnul( out->axis[ axis ] );
         }
      }
   }

/* If an error occurred, free any allocated memory. */
   if ( !astOK ) {
      out->axis = astFree( out->axis );
      out->domain = astFree( out->domain );
      out->perm = astFree( out->perm );
      out->title = astFree( out->title );
   }
}

/* Destructor. */
/* ----------- */
static void Delete( AstObject *obj, int *status ) {
/*
*  Name:
*     Delete

*  Purpose:
*     Destructor for Frame objects.

*  Type:
*     Private function.

*  Synopsis:
*     void Delete( AstObject *obj, int *status )

*  Description:
*     This function implements the destructor for Frame objects.

*  Parameters:
*     obj
*        Pointer to the object to be deleted.
*     status
*        Pointer to the inherited status variable.

*  Notes:
*     This function attempts to execute even if the global error status is
*     set.
*/

/* Local Variables: */
   AstFrame *this;               /* Pointer to Frame */
   int axis;                     /* Loop counter for Frame axes */

/* Obtain a pointer to the Frame structure. */
   this = (AstFrame *) obj;

/* Free the memory used for the Title and Domain strings if necessary. */
   this->title = astFree( this->title );
   this->domain = astFree( this->domain );

/* If memory has been allocated to store pointers to the Frame's Axis objects,
   annul each of these pointers and then free the memory. */
   if ( this->axis ) {
      for ( axis = 0; axis < this->naxes; axis++ ) {
         this->axis[ axis ] = astAnnul( this->axis[ axis ] );
      }
      this->axis = astFree( this->axis );
   }

/* Free memory used for the axis permutation array if necessary. */
   this->perm = astFree( this->perm );
}

/* Dump function. */
/* -------------- */
static void Dump( AstObject *this_object, AstChannel *channel, int *status ) {
/*
*  Name:
*     Dump

*  Purpose:
*     Dump function for Frame objects.

*  Type:
*     Private function.

*  Synopsis:
*     void Dump( AstObject *this, AstChannel *channel, int *status )

*  Description:
*     This function implements the Dump function which writes out data
*     for the Frame class to an output Channel.

*  Parameters:
*     this
*        Pointer to the Frame whose data are being written.
*     channel
*        Pointer to the Channel to which the data are being written.
*     status
*        Pointer to the inherited status variable.
*/

/* Local Constants: */
#define COMMENT_LEN 150          /* Maximum length of a comment string */
#define KEY_LEN 50               /* Maximum length of a keyword */

/* Local Variables: */
   AstAxis *ax;                  /* Pointer to Axis */
   AstFrame *cfrm;               /* Pointer to FrameSet's current Frame */
   AstFrame *this;               /* Pointer to the Frame structure */
   AstSystemType system;         /* System code */
   char comment[ COMMENT_LEN + 1 ]; /* Buffer for comment strings */
   char key[ KEY_LEN + 1 ];      /* Buffer for keywords */
   const char *sval;             /* Pointer to string value */
   const char *lab;              /* Pointer to unit label */
   const int *perm;              /* Pointer to axis permutation array */
   double dval;                  /* Double attibute value */
   int *invperm;                 /* Pointer to inverse permutation array */
   int axis;                     /* Loop counter for Frame axes */
   int bessyr;                   /* Format as Besselian years (else Julian) */
   int digits_set;               /* Digits set explicitly for any axis? */
   int full;                     /* Full attribute value */
   int full_set;                 /* Full attribute set? */
   int helpful;                  /* Helpful to show value even if not set? */
   int isFrame;                  /* Is this a simple Frame? */
   int ival;                     /* Integer value */
   int naxes;                    /* Number of Frame axes */
   int set;                      /* Attribute value set? */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain a pointer to the Frame structure. */
   this = (AstFrame *) this_object;

/* Determine the number of Frame axes and a pointer to the Frame's
   axis permutation array (using methods, to allow for any over-ride
   by a derived class). */
   naxes = astGetNaxes( this );
   perm = astGetPerm( this );

/* Some default attribute values are not helpful for a simple Frame. Note
   if this is a simple Frame, or if it is a FrameSet with a simple Frame
   as its current Frame., or if it is a CmpFrame. */
   if( !strcmp( astGetClass( this ), "Frame" ) ) {
      isFrame = 1;
   } else if( astIsAFrameSet( this ) ) {
      cfrm = astGetFrame( (AstFrameSet *) this, AST__CURRENT );
      isFrame = !strcmp( astGetClass( cfrm ), "Frame" );
      cfrm = astAnnul( cfrm );
   } else if( astIsACmpFrame( this ) ) {
      isFrame = 1;
   } else {
      isFrame = 0;
   }

/* Allocate memory to hold an inverse axis permutation array and
   generate this array from the forward permutation values. This will
   be used to determine which axis should be enquired about (using
   possibly over-ridden methods) to obtain data to correspond with a
   particular internal value (i.e. instance variable) relating to an
   axis. This step is needed so that the effect of any axis
   permutation can be un-done before values are written out, as output
   values are written by this function in un-permuted order. */
   invperm = astMalloc( sizeof( int ) * (size_t) naxes );
   if ( astOK ) {
      for ( axis = 0; axis < naxes; axis++ ) invperm[ perm[ axis ] ] = axis;

/* Write out values representing the instance variables for the Frame
   class.  Accompany these with appropriate comment strings, possibly
   depending on the values being written.*/

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

/* Title. */
/* ------ */
      set = TestTitle( this, status );
      sval = set ? GetTitle( this, status ) : astGetTitle( this );
      astWriteString( channel, "Title", set, 1, sval,
                      "Title of coordinate system" );

/* Naxes. */
/* ------ */
      set = ( this->naxes != 0 );
      ival = set ? this->naxes : naxes;
      astWriteInt( channel, "Naxes", set, 1, ival,
                   "Number of coordinate axes" );

/* Domain. */
/* ------- */
      set = TestDomain( this, status );
      sval = set ? GetDomain( this, status ) : astGetDomain( this );

/* Don't show an un-set Domain value if it is blank. */
      helpful = ( sval && *sval );
      astWriteString( channel, "Domain", set, helpful, sval,
                      "Coordinate system domain" );

/* Epoch. */
/* ------ */
      set = TestEpoch( this, status );
      dval = set ? GetEpoch( this, status ) : astGetEpoch( this );

/* Convert MJD to Besselian or Julian years, depending on the value. */
      bessyr = ( dval < palSlaEpj2d( 1984.0 ) );
      dval = bessyr ? palSlaEpb( dval ) : palSlaEpj( dval );
      astWriteDouble( channel, "Epoch", set, !isFrame, dval,
                      bessyr ? "Besselian epoch of observation" :
                               "Julian epoch of observation" );

/* Label. */
/* ------ */
/* This, and some other, attributes are stored internally by the
   Frame's Axis objects, but are "re-packaged" by the Frame class to
   appear as Frame attributes. We treat them here like Frame
   attributes that are "un-set". There is a Label value for each Frame
   axis. */
      for ( axis = 0; axis < naxes; axis++ ) {

/* The inverse axis permutation array is used to obtain the axis index
   for astGetLabel. This reverses the effect of the Frame's axis
   permutation array and yields a default value appropriate to the
   axis with internal index "axis". */
         sval = astGetLabel( this, invperm[ axis ] );

/* Create keyword and comment strings appropriate to each axis
   (converting to 1-based axis numbering) and write out the Label
   values. */
         (void) sprintf( key, "Lbl%d", axis + 1 );
         (void) sprintf( comment, "Label for axis %d", axis + 1 );
         astWriteString( channel, key, 0, 1, sval, comment );
      }

/* Symbol. */
/* ------- */
/* There is a Symbol value for each Frame axis. These are handled in
   the same way as the Label values. */
      for ( axis = 0; axis < naxes; axis++ ) {
         sval = astGetSymbol( this, invperm[ axis ] );
         (void) sprintf( key, "Sym%d", axis + 1 );
         (void) sprintf( comment, "Symbol for axis %d", axis + 1 );
         astWriteString( channel, key, 0, 0, sval, comment );
      }

/* System. */
/* ------- */
      set = TestSystem( this, status );
      system = set ? GetSystem( this, status ) : astGetSystem( this );

/* If set, convert explicitly to a string for the external representation. */
      if ( set ) {
         if ( astOK ) {
            sval = astSystemString( this, system );

/* Report an error if the System value was not recognised. */
            if ( !sval ) {
               astError( AST__SCSIN,
                        "astWrite(%s): Corrupt %s contains invalid "
                        "System identification code (%d).", status,
                        astGetClass( channel ), astGetClass( this ),
                        (int) system );
            }
         }

/* If not set, use astGetAttrib which returns a string value using
   (possibly over-ridden) methods. */
      } else {
         sval = astGetAttrib( this_object, "system" );
      }

/* Write out the value. */
      astWriteString( channel, "System", set, !isFrame, sval,
                      "Coordinate system type" );

/* AlignSystem. */
/* ------------ */
      set = TestAlignSystem( this, status );
      system = set ? GetAlignSystem( this, status ) : astGetAlignSystem( this );

/* If set, convert explicitly to a string for the external representation. */
      if ( set ) {
         if ( astOK ) {
            sval = astSystemString( this, system );

/* Report an error if the AlignSystem value was not recognised. */
            if ( !sval ) {
               astError( AST__SCSIN,
                        "astWrite(%s): Corrupt %s contains invalid "
                        "AlignSystem identification code (%d).", status,
                        astGetClass( channel ), astGetClass( this ),
                        (int) system );
            }
         }

/* If not set, use astGetAttrib which returns a string value using
   (possibly over-ridden) methods. */
      } else {
         sval = astGetAttrib( this_object, "alignsystem" );
      }

/* Write out the value. */
      astWriteString( channel, "AlSys", set, 0, sval,
                      "Alignment coordinate system" );

/* Unit. */
/* ----- */
/* There is a Unit value for each axis. */
      for ( axis = 0; axis < naxes; axis++ ) {
         sval = astGetUnit( this, invperm[ axis ] );

/* Get any label associated with the unit string. */
         lab = astUnitLabel( sval );

/* Construct a comment including the above label (but only if it is not
   the same as the unit string) . */
         if( lab && strcmp( lab, sval ) ) {
            (void) sprintf( comment, "Units for axis %d (%s)", axis + 1, lab );
         } else {
            (void) sprintf( comment, "Units for axis %d", axis + 1 );
         }

/* Show the Unit value if it is not blank. */
         helpful = ( sval && *sval );
         (void) sprintf( key, "Uni%d", axis + 1 );
         astWriteString( channel, key, 0, helpful, sval, comment );
      }

/* Digits. */
/* ------- */
/* There is a Digits value for each axis... */
      digits_set = 0;
      for ( axis = 0; axis < naxes; axis++ ) {

/* Obtain the axis Digits value, using the Frame's Digits value as a
   default. */
         ax = astGetAxis( this, invperm[ axis ] );
         set = astTestAxisDigits( ax );
         ival = set ? astGetAxisDigits( ax ) : astGetDigits( this );
         ax = astAnnul( ax );

/* Show the value if it is set for the axis (i.e. if it differs from
   the default for the whole Frame) and note if any such value is
   set. */
         helpful = set;
         if ( set ) digits_set = 1;
         (void) sprintf( key, "Dig%d", axis + 1 );
         (void) sprintf( comment, "Individual precision for axis %d",
                         axis + 1 );
         astWriteInt( channel, key, 0, helpful, ival, comment );
      }

/* There is also a Digits value for the Frame as a whole... */
      set = TestDigits( this, status );

/* Show the value (even if not set) if an explicit Digits value has
   been set for any axis (above). */
      helpful = digits_set;
      ival = set ? GetDigits( this, status ) : astGetDigits( this );
      astWriteInt( channel, "Digits", set, helpful, ival,
                   "Default formatting precision" );

/* Format. */
/* ------- */
/* There is a Format value for each axis. */
      for ( axis = 0; axis < naxes; axis++ ) {
         sval = astGetFormat( this, invperm[ axis ] );

/* Show the Format value if the Digits value is set for an individual
   axis. */
         ax = astGetAxis( this, invperm[ axis ] );
         helpful = astTestAxisDigits( ax );
         ax = astAnnul( ax );
         (void) sprintf( key, "Fmt%d", axis + 1 );
         (void) sprintf( comment, "Format specifier for axis %d", axis + 1 );
         astWriteString( channel, key, 0, helpful, sval, comment );
      }

/* Direction. */
/* ---------- */
/* There is a Direction value for each axis. */
      for ( axis = 0; axis < naxes; axis++ ) {
         ival = astGetDirection( this, invperm[ axis ] );

/* Show the value if it is zero. */
         helpful = ( ival == 0 );
         (void) sprintf( key, "Dir%d", axis + 1 );
         (void) sprintf( comment,
                         ival ? "Plot axis %d in conventional direction" :
                                "Plot axis %d in reverse direction",
                         axis + 1 );
         astWriteInt( channel, key, 0, helpful, ival, comment );
      }

/* Bottom. */
/* ------- */
/* There is a Bottom value for each axis. */
      for ( axis = 0; axis < naxes; axis++ ) {
         dval = astGetBottom( this, invperm[ axis ] );

/* Show the value if it is zero. */
         helpful = ( dval != -DBL_MAX );
         (void) sprintf( key, "Bot%d", axis + 1 );
         astWriteDouble( channel, key, 0, helpful, dval, "Lowest legal axis value");
      }

/* Top. */
/* ------- */
/* There is a Top value for each axis. */
      for ( axis = 0; axis < naxes; axis++ ) {
         dval = astGetTop( this, invperm[ axis ] );

/* Show the value if it is zero. */
         helpful = ( dval != DBL_MAX );
         (void) sprintf( key, "Top%d", axis + 1 );
         astWriteDouble( channel, key, 0, helpful, dval, "Highest legal axis value");
      }

/* PreserveAxes. */
/* ------------- */
      set = TestPreserveAxes( this, status );
      ival = set ? GetPreserveAxes( this, status ) : astGetPreserveAxes( this );
      astWriteInt( channel, "Presrv", set, 0, ival,
                   ival ? "Preserve target axes" :
                          "Don't preserve target axes" );

/* Permute. */
/* -------- */
      set = TestPermute( this, status );
      ival = set ? GetPermute( this, status ) : astGetPermute( this );
      astWriteInt( channel, "Permut", set, 0, ival,
                   ival ? "Axes may be permuted to match" :
                          "Axes may not be permuted match" );

/* MinAxes. */
/* -------- */
      set = TestMinAxes( this, status );
      ival = set ? GetMinAxes( this, status ) : astGetMinAxes( this );
      astWriteInt( channel, "MinAx", set, 0, ival,
                   "Minimum number of axes to match" );

/* MaxAxes. */
/* -------- */
      set = TestMaxAxes( this, status );
      ival = set ? GetMaxAxes( this, status ) : astGetMaxAxes( this );
      astWriteInt( channel, "MaxAx", set, 0, ival,
                   "Maximum number of axes to match" );

/* MatchEnd. */
/* --------- */
      set = TestMatchEnd( this, status );
      ival = set ? GetMatchEnd( this, status ) : astGetMatchEnd( this );
      astWriteInt( channel, "MchEnd", set, 0, ival,
                   ival ? "Match final target axes" :
                          "Match initial target axes" );

/* ObsLat. */
/* ------- */
   set = TestObsLat( this, status );
   dval = set ? GetObsLat( this, status ) : astGetObsLat( this );
   astWriteDouble( channel, "ObsLat", set, 0, dval, "Observers geodetic latitude (rads)" );

/* ObsLon. */
/* ------- */
   set = TestObsLon( this, status );
   dval = set ? GetObsLon( this, status ) : astGetObsLon( this );
   astWriteDouble( channel, "ObsLon", set, 0, dval, "Observers geodetic longitude (rads)" );

/* ObsAlt. */
/* ------- */
   set = TestObsAlt( this, status );
   dval = set ? GetObsAlt( this, status ) : astGetObsAlt( this );
   astWriteDouble( channel, "ObsAlt", set, 0, dval, "Observers geodetic altitude (metres)" );

/* Dut1*/
/* ---- */
   set = TestDut1( this, status );
   dval = set ? GetDut1( this, status ) : astGetDut1( this );
   astWriteDouble( channel, "Dut1", set, 0, dval, "UT1-UTC in seconds" );


/* ActiveUnit. */
/* ----------- */
      if( astTestActiveUnit( this ) ) {
         ival = astGetActiveUnit( this );
         astWriteInt( channel, "ActUnt", 1, 0, ival,
                      ival ? "Unit strings affects alignment" :
                             "Unit strings do not affect alignment" );
      }

/* Axis permutation array. */
/* ----------------------- */
/* Write out the axis permutation array value for each axis,
   converting to 1-based axis numbering. */
      for ( axis = 0; axis < this->naxes; axis++ ) {
         set = ( this->perm[ axis ] != axis );
         ival = this->perm[ axis ] + 1;

/* Create a keyword and comment appropriate to the axis. */
         (void) sprintf( key, "Prm%d", axis + 1 );
         if ( set ) {
            (void) sprintf( comment,
                            "Axis %d permuted to use internal axis %d",
                            axis + 1, ival );
         } else {
            (void) sprintf( comment, "Axis %d not permuted", axis + 1 );
         }
         astWriteInt( channel, key, set, 0, ival, comment );
      }

/* Axis Objects. */
/* ------------- */
/* Temporarily set the Channel's Full attribute to -1 (unless it is +1
   to start with), remembering the original setting. This prevents any
   unnecessary "un-set" Axis values being output that would otherwise
   simply duplicate the Frame's attributes which have already been
   written. "Set" Axis values are still written, however (and all
   values are written if Full is set to 1). */
      full_set = astTestFull( channel );
      full = astGetFull( channel );
      if ( full <= 0 ) astSetFull( channel, -1 );

/* Handle each axis in turn. */
      for ( axis = 0; axis < this->naxes; axis++ ) {

/* Create a keyword and comment appropriate to the axis (converting to
   1-based axis numbering). */
         (void) sprintf( key, "Ax%d", axis + 1 );
         (void) sprintf( comment, "Axis number %d", axis + 1 );

/* Write out the axis Object description. */
         astWriteObject( channel, key, 1, 0, this->axis[ axis ], comment );
      }

/* Restore the Channel's original Full attribute setting. */
      if ( full_set ) {
         astSetFull( channel, full );
      } else {
         astClearFull( channel );
      }

/* Free the inverse axis permutation array. */
      invperm = astFree( invperm );
   }

/* Undefine macros local to this function. */
#undef COMMENT_LEN
#undef KEY_LEN
}

/* Standard class functions. */
/* ========================= */
/* Implement the astIsAFrame and astCheckFrame functions using the macros
   defined for this purpose in the "object.h" header file. */
astMAKE_ISA(Frame,Mapping)
astMAKE_CHECK(Frame)

AstFrame *astFrame_( int naxes, const char *options, int *status, ...) {
/*
*+
*  Name:
*     astFrame

*  Purpose:
*     Create a Frame.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "frame.h"
*     AstFrame *astFrame( int naxes, const char *options, int *status, ... )

*  Class Membership:
*     Frame constructor.

*  Description:
*     This function creates a new Frame and optionally initialises its
*     attributes.

*  Parameters:
*     naxes
*        The number of Frame axes.
*     options
*        Pointer to a null terminated string containing an optional
*        comma-separated list of attribute assignments to be used for
*        initialising the new Frame. The syntax used is the same as
*        for the astSet method and may include "printf" format
*        specifiers identified by "%" symbols in the normal way.
*     status
*        Pointer to the inherited status variable.
*     ...
*        If the "options" string contains "%" format specifiers, then
*        an optional list of arguments may follow it in order to
*        supply values to be substituted for these specifiers. The
*        rules for supplying these are identical to those for the
*        astSet method (and for the C "printf" function).

*  Returned Value:
*     A pointer to the new Frame.

*  Notes:
*     - A NULL pointer will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*-

*  Implementation Notes:
*     - This function implements the basic Frame constructor which is
*     available via the protected interface to the Frame class.  A
*     public interface is provided by the astFrameId_ function.
*/

/* Local Variables: */
   astDECLARE_GLOBALS            /* Pointer to thread-specific global data */
   AstFrame *new;                /* Pointer to new Frame */
   va_list args;                 /* Variable argument list */

/* Get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(NULL);

/* Check the global error status. */
   if ( !astOK ) return NULL;

/* Initialise the Frame, allocating memory and initialising the virtual
   function table as well if necessary. */
   new = astInitFrame( NULL, sizeof( AstFrame ), !class_init, &class_vtab,
                       "Frame", naxes );

/* If successful, note that the virtual function table has been initialised. */
   if ( astOK ) {
      class_init = 1;

/* Obtain the variable argument list and pass it along with the options string
   to the astVSet method to initialise the new Frame's attributes. */
      va_start( args, status );
      astVSet( new, options, NULL, args );
      va_end( args );

/* If an error occurred, clean up by deleting the new object. */
      if ( !astOK ) new = astDelete( new );
   }

/* Return a pointer to the new Frame. */
   return new;
}

AstFrame *astInitFrame_( void *mem, size_t size, int init,
                         AstFrameVtab *vtab, const char *name,
                         int naxes, int *status ) {
/*
*+
*  Name:
*     astInitFrame

*  Purpose:
*     Initialise a Frame.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "frame.h"
*     AstFrame *astInitFrame( void *mem, size_t size, int init,
*                             AstFrameVtab *vtab, const char *name,
*                             int naxes )

*  Class Membership:
*     Frame initialiser.

*  Description:
*     This function is provided for use by class implementations to initialise
*     a new Frame object. It allocates memory (if necessary) to accommodate
*     the Frame plus any additional data associated with the derived class.
*     It then initialises a Frame structure at the start of this memory. If
*     the "init" flag is set, it also initialises the contents of a virtual
*     function table for a Frame at the start of the memory passed via the
*     "vtab" parameter.

*  Parameters:
*     mem
*        A pointer to the memory in which the Frame is to be created. This
*        must be of sufficient size to accommodate the Frame data
*        (sizeof(Frame)) plus any data used by the derived class. If a value
*        of NULL is given, this function will allocate the memory itself using
*        the "size" parameter to determine its size.
*     size
*        The amount of memory used by the Frame (plus derived class data).
*        This will be used to allocate memory if a value of NULL is given for
*        the "mem" parameter. This value is also stored in the Frame
*        structure, so a valid value must be supplied even if not required for
*        allocating memory.
*     init
*        A logical flag indicating if the Frame's virtual function table is
*        to be initialised. If this value is non-zero, the virtual function
*        table will be initialised by this function.
*     vtab
*        Pointer to the start of the virtual function table to be associated
*        with the new Frame.
*     name
*        Pointer to a constant null-terminated character string which contains
*        the name of the class to which the new object belongs (it is this
*        pointer value that will subsequently be returned by the astGetClass
*        method).
*     naxes
*        The number of Frame axes.

*  Returned Value:
*     A pointer to the new Frame.

*  Notes:
*     -  A null pointer will be returned if this function is invoked with the
*     global error status set, or if it should fail for any reason.
*-
*/

/* Local Variables: */
   AstFrame *new;                /* Pointer to new Frame */
   int axis;                     /* Loop counter for Frame axes */

/* Check the global status. */
   if ( !astOK ) return NULL;

/* If necessary, initialise the virtual function table. */
   if ( init ) astInitFrameVtab( vtab, name );

/* Initialise. */
   new = NULL;

/* Check the number of axes for validity, reporting an error if necessary. */
   if ( naxes < 0 ) {
      astError( AST__NAXIN, "astInitFrame(%s): Number of axes (%d) is "
                "invalid - this number should not be negative.", status, name, naxes );

/* Initialise a Mapping structure (the parent class) as the first
   component within the Frame structure, allocating memory if
   necessary. Set the number of input/output coordinates to zero (the
   astGetNin and astGetNout methods are over-ridden by the Frame class
   to provide values for these that are equal to the number of Frame
   axes). */
   } else {
      new = (AstFrame *) astInitMapping( mem, size, 0,
                                         (AstMappingVtab *) vtab, name,
                                         0, 0, 1, 1 );

      if ( astOK ) {

/* Initialise the Frame data. */
/* ----------------------------- */
/* Set the number of Frame axes. */
         new->naxes = naxes;

/* Initialise all attributes to their "undefined" values. */
         new->digits = -INT_MAX;
         new->domain = NULL;
         new->epoch = AST__BAD;
         new->match_end = -INT_MAX;
         new->max_axes = -INT_MAX;
         new->min_axes = -INT_MAX;
         new->permute = -INT_MAX;
         new->preserve_axes = -INT_MAX;
         new->title = NULL;
         new->system = AST__BADSYSTEM;
         new->alignsystem = AST__BADSYSTEM;
         new->active_unit = -INT_MAX;
         new->obsalt = AST__BAD;
         new->obslat = AST__BAD;
         new->obslon = AST__BAD;
         new->dut1 = AST__BAD;
         new->flags = 0;

/* Allocate memory to store pointers to the Frame's Axis objects and to store
   its axis permutation array. */
         new->axis = astMalloc( sizeof( AstAxis * ) * (size_t) naxes );
         new->perm = astMalloc( sizeof( int ) * (size_t) naxes );

/* Create a new Axis object to describe each axis of the Frame and store the
   resulting pointers in the memory allocated above. Also initialise the
   axis permutation array so that the axes appear in their natural order. */
         if ( astOK ) {
            for ( axis = 0; axis < naxes; axis++ ) {
               new->axis[ axis ] = astAxis( "", status );
               new->perm[ axis ] = axis;
	    }

/* If an error occurred while creating the Axis objects, scan through the array
   of pointers to them again to ensure that they are all correctly annulled. */
            if ( !astOK ) {
               for ( axis = 0; axis < naxes; axis++ ) {
                  new->axis[ axis ] = astAnnul( new->axis[ axis ] );
   	       }
	    }
	 }

/* If an error occurred, clean up by deleting the new object. */
         if ( !astOK ) new = astDelete( new );
      }
   }

/* Return a pointer to the new object. */
   return new;
}

AstFrame *astLoadFrame_( void *mem, size_t size,
                         AstFrameVtab *vtab, const char *name,
                         AstChannel *channel, int *status ) {
/*
*+
*  Name:
*     astLoadFrame

*  Purpose:
*     Load a Frame.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "frame.h"
*     AstFrame *astLoadFrame( void *mem, size_t size,
*                              AstFrameVtab *vtab, const char *name,
*                              AstChannel *channel )

*  Class Membership:
*     Frame loader.

*  Description:
*     This function is provided to load a new Frame using data read
*     from a Channel. It first loads the data used by the parent class
*     (which allocates memory if necessary) and then initialises a
*     Frame structure in this memory, using data read from the input
*     Channel.
*
*     If the "init" flag is set, it also initialises the contents of a
*     virtual function table for a Frame at the start of the memory
*     passed via the "vtab" parameter.


*  Parameters:
*     mem
*        A pointer to the memory into which the Frame is to be loaded.
*        This must be of sufficient size to accommodate the Frame data
*        (sizeof(Frame)) plus any data used by derived classes. If a
*        value of NULL is given, this function will allocate the
*        memory itself using the "size" parameter to determine its
*        size.
*     size
*        The amount of memory used by the Frame (plus derived class
*        data).  This will be used to allocate memory if a value of
*        NULL is given for the "mem" parameter. This value is also
*        stored in the Frame structure, so a valid value must be
*        supplied even if not required for allocating memory.
*
*        If the "vtab" parameter is NULL, the "size" value is ignored
*        and sizeof(AstFrame) is used instead.
*     vtab
*        Pointer to the start of the virtual function table to be
*        associated with the new Frame. If this is NULL, a pointer to
*        the (static) virtual function table for the Frame class is
*        used instead.
*     name
*        Pointer to a constant null-terminated character string which
*        contains the name of the class to which the new object
*        belongs (it is this pointer value that will subsequently be
*        returned by the astGetClass method).
*
*        If the "vtab" parameter is NULL, the "name" value is ignored
*        and a pointer to the string "Frame" is used instead.

*  Returned Value:
*     A pointer to the new Frame.

*  Notes:
*     - A null pointer will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*-
*/

/* Local Constants: */
   astDECLARE_GLOBALS            /* Pointer to thread-specific global data */
#define KEY_LEN 50               /* Maximum length of a keyword */

/* Local Variables: */
   AstFrame *new;                /* Pointer to the new Frame */
   char *sval;                   /* Pointer to string value */
   char key[ KEY_LEN + 1 ];      /* Buffer for keywords */
   double dval;                  /* DOuble attribute value */
   int axis;                     /* Loop counter for axes */
   int ival;                     /* Integer value */

/* Get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(channel);

/* Initialise. */
   new = NULL;

/* Check the global error status. */
   if ( !astOK ) return new;

/* If a NULL virtual function table has been supplied, then this is
   the first loader to be invoked for this Frame. In this case the
   Frame belongs to this class, so supply appropriate values to be
   passed to the parent class loader (and its parent, etc.). */
   if ( !vtab ) {
      size = sizeof( AstFrame );
      vtab = &class_vtab;
      name = "Frame";

/* If required, initialise the virtual function table for this class. */
      if ( !class_init ) {
         astInitFrameVtab( vtab, name );
         class_init = 1;
      }
   }

/* Invoke the parent class loader to load data for all the ancestral
   classes of the current one, returning a pointer to the resulting
   partly-built Frame. */
   new = astLoadMapping( mem, size, (AstMappingVtab *) vtab, name,
                         channel );

   if ( astOK ) {

/* Assign values for transient components that are not included in the
   Frame dump */
   new->flags = 0;

/* Read input data. */
/* ================ */
/* Request the input Channel to read all the input data appropriate to
   this class into the internal "values list". */
      astReadClassData( channel, "Frame" );

/* Now read each individual data item from this list and use it to
   initialise the appropriate instance variable(s) for this class. */

/* In the case of attributes, we first read the "raw" input value,
   supplying the "unset" value as the default. If a "set" value is
   obtained, we then use the appropriate (private) Set... member
   function to validate and set the value properly. */

/* Naxes. */
/* ------ */
/* Obtain the number of Frame axes and allocate memory for the arrays
   which hold axis information. */
      new->naxes = astReadInt( channel, "naxes", 0 );
      if ( new->naxes < 0 ) new->naxes = 0;
      new->perm = astMalloc( sizeof( int ) * (size_t) new->naxes );
      new->axis = astMalloc( sizeof( AstAxis * ) * (size_t) new->naxes );

/* If an error occurred, ensure that any allocated memory is freed. */
      if ( !astOK ) {
         new->perm = astFree( new->perm );
         new->axis = astFree( new->axis );

/* Otherwise, initialise the array of Axis pointers. */
      } else {
         for ( axis = 0; axis < new->naxes; axis++ ) new->axis[ axis ] = NULL;

/* Now obtain those input values which are required for each axis... */
         for ( axis = 0; axis < new->naxes; axis++ ) {

/* Axis object. */
/* ------------ */
/* This must be read first, so that it can hold the other axis values
   obtained below. */

/* Create a keyword appropriate to this axis. */
            (void) sprintf( key, "ax%d", axis + 1 );

/* Read the Axis object. If none was read, provide a default Axis
   instead. */
            new->axis[ axis ] = astReadObject( channel, key, NULL );
            if ( !new->axis[ axis ] ) new->axis[ axis ] = astAxis( "", status );

/* Label. */
/* ------ */
/* Read the Label string for each axis. If a value is obtained, use
   it to set the Label attribute for the axis. Free the memory holding
   the string when no longer needed. */
            (void) sprintf( key, "lbl%d", axis + 1 );
            sval = astReadString( channel, key, NULL );
            if ( sval ) {
               astSetAxisLabel( new->axis[ axis ], sval );
               sval = astFree( sval );
            }

/* Symbol. */
/* ------- */
            (void) sprintf( key, "sym%d", axis + 1 );
            sval = astReadString( channel, key, NULL );
            if ( sval ) {
               astSetAxisSymbol( new->axis[ axis ], sval );
               sval = astFree( sval );
            }

/* Format. */
/* ------- */
            (void) sprintf( key, "fmt%d", axis + 1 );
            sval = astReadString( channel, key, NULL );
            if ( sval ) {
               astSetAxisFormat( new->axis[ axis ], sval );
               sval = astFree( sval );
            }

/* Unit. */
/* ----- */
            (void) sprintf( key, "uni%d", axis + 1 );
            sval = astReadString( channel, key, NULL );
            if ( sval ) {
               astSetAxisUnit( new->axis[ axis ], sval );
               sval = astFree( sval );
            }

/* Direction. */
/* ---------- */
            (void) sprintf( key, "dir%d", axis + 1 );
            ival = astReadInt( channel, key, -INT_MAX );
            if ( ival != -INT_MAX ) {
               astSetAxisDirection( new->axis[ axis ], ival );
            }

/* Top. */
/*----- */
            (void) sprintf( key, "top%d", axis + 1 );
            dval = astReadDouble( channel, key, AST__BAD );
            if ( dval != AST__BAD ) {
               astSetAxisTop( new->axis[ axis ], dval );
            }

/* Bottom. */
/*----- -- */
            (void) sprintf( key, "bot%d", axis + 1 );
            dval = astReadDouble( channel, key, AST__BAD );
            if ( dval != AST__BAD ) {
               astSetAxisBottom( new->axis[ axis ], dval );
            }

/* Digits. */
/* ------- */
            (void) sprintf( key, "dig%d", axis + 1 );
            ival = astReadInt( channel, key, -INT_MAX );
            if ( ival != -INT_MAX ) {
               astSetAxisDigits( new->axis[ axis ], ival );
            }

/* Axis permutation array. */
/* ----------------------- */
/* Convert from 1-based to zero-based axis numbering at this
   point. The default is the "un-permuted" value. */
            sprintf( key, "prm%d", axis + 1 );
            new->perm[ axis ] = astReadInt( channel, key, axis + 1 ) - 1;

/* Quit looping if an error occurs. */
            if ( !astOK ) break;
         }

/* The remaining values are not associated with particular axes... */

/* Title. */
/* ------ */
         new->title = astReadString( channel, "title", NULL );

/* Domain. */
/* ------- */
         new->domain = astReadString( channel, "domain", NULL );

/* Epoch. */
/* ------ */
/* Interpret this as Besselian or Julian depending on its value. */
         new->epoch = astReadDouble( channel, "epoch", AST__BAD );
         if ( TestEpoch( new, status ) ) {
            SetEpoch( new, ( new->epoch < 1984.0 ) ? palSlaEpb2d( new->epoch ) :
                                                     palSlaEpj2d( new->epoch ), status );
         }

/* Digits. */
/* ------- */
/* This is the value that applies to the Frame as a whole. */
         new->digits = astReadInt( channel, "digits", -INT_MAX );
         if ( TestDigits( new, status ) ) SetDigits( new, new->digits, status );

/* PreserveAxes. */
/* ------------- */
         new->preserve_axes = astReadInt( channel, "presrv", -INT_MAX );
         if ( TestPreserveAxes( new, status ) ) {
            SetPreserveAxes( new, new->preserve_axes, status );
         }

/* Permute. */
/* -------- */
         new->permute = astReadInt( channel, "permut", -INT_MAX );
         if ( TestPermute( new, status ) ) SetPermute( new, new->permute, status );

/* MinAxes. */
/* -------- */
         new->min_axes = astReadInt( channel, "minax", -INT_MAX );
         if ( TestMinAxes( new, status ) ) SetMinAxes( new, new->min_axes, status );

/* MaxAxes. */
/* -------- */
         new->max_axes = astReadInt( channel, "maxax", -INT_MAX );
         if ( TestMaxAxes( new, status ) ) SetMaxAxes( new, new->max_axes, status );

/* MatchEnd. */
/* --------- */
         new->match_end = astReadInt( channel, "mchend", -INT_MAX );
         if ( TestMatchEnd( new, status ) ) SetMatchEnd( new, new->match_end, status );

/* ObsLat. */
/* ------- */
         new->obslat = astReadDouble( channel, "obslat", AST__BAD );
         if ( TestObsLat( new, status ) ) SetObsLat( new, new->obslat, status );

/* ObsLon. */
/* ------- */
         new->obslon = astReadDouble( channel, "obslon", AST__BAD );
         if ( TestObsLon( new, status ) ) SetObsLon( new, new->obslon, status );

/* ObsAlt. */
/* ------- */
         new->obsalt = astReadDouble( channel, "obsalt", AST__BAD );
         if ( TestObsAlt( new, status ) ) SetObsAlt( new, new->obsalt, status );

/* Dut1. */
/* ---- */
         new->dut1 = astReadDouble( channel, "dut1", AST__BAD );
         if ( TestDut1( new, status ) ) SetDut1( new, new->dut1, status );

/* ActiveUnit. */
/* ----------- */
         new->active_unit = astReadInt( channel, "actunt", -INT_MAX );
         if ( TestActiveUnit( new, status ) ) SetActiveUnit( new, new->active_unit, status );

/* System. */
/* ------- */
/* Set the default and read the external representation as a string. */
         new->system = AST__BADSYSTEM;
         sval = astReadString( channel, "system", NULL );

/* If a value was read, convert from a string to a System code. */
         if ( sval ) {
            if ( astOK ) {
               new->system = astSystemCode( new, sval );

/* Report an error if the value wasn't recognised. */
               if ( new->system == AST__BADSYSTEM ) {
                  astError( AST__ATTIN,
                            "astRead(%s): Invalid System description "
                            "\"%s\".", status, astGetClass( channel ), sval );
               }
            }

/* Free the string value. */
            sval = astFree( sval );
         }

/* AlignSystem. */
/* ------------ */
/* Set the default and read the external representation as a string. */
         new->alignsystem = AST__BADSYSTEM;
         sval = astReadString( channel, "alsys", NULL );

/* If a value was read, convert from a string to a System code. */
         if ( sval ) {
            if ( astOK ) {
               new->alignsystem = astSystemCode( new, sval );

/* Report an error if the value wasn't recognised. */
               if ( new->alignsystem == AST__BADSYSTEM ) {
                  astError( AST__ATTIN,
                            "astRead(%s): Invalid AlignSystem description "
                            "\"%s\".", status, astGetClass( channel ), sval );
               }
            }

/* Free the string value. */
            sval = astFree( sval );
         }
      }

/* If an error occurred, clean up by deleting the new Frame. */
      if ( !astOK ) new = astDelete( new );
   }

/* Return the new Frame pointer. */
   return new;

/* Undefine macros local to this function. */
#undef KEY_LEN
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
const char *astAbbrev_( AstFrame *this, int axis, const char *fmt,
                        const char *str1, const char *str2, int *status ) {
   if ( !astOK ) return str2;
   return (**astMEMBER(this,Frame,Abbrev))( this, axis, fmt, str1, str2, status );
}
int astFields_( AstFrame *this, int axis, const char *fmt,
                const char *str, int maxfld, char **fields,
                int *nc, double *val, int *status ) {
   if ( !astOK ) return 0;
   return (**astMEMBER(this,Frame,Fields))( this, axis, fmt, str, maxfld, fields, nc, val, status );
}
void astCheckPerm_( AstFrame *this, const int *perm, const char *method, int *status ) {
   if ( !astOK ) return;
   (**astMEMBER(this,Frame,CheckPerm))( this, perm, method, status );
}

AstPointSet *astResolvePoints_( AstFrame *this, const double point1[],
                                const double point2[], AstPointSet *in,
                                AstPointSet *out, int *status ) {
   if ( !astOK ) return NULL;
   return (**astMEMBER(this,Frame,ResolvePoints))( this, point1, point2, in, out, status );
}
AstLineDef *astLineDef_( AstFrame *this, const double start[2],
                             const double end[2], int *status ) {
   if ( !astOK ) return NULL;
   return (**astMEMBER(this,Frame,LineDef))( this, start, end, status );
}
int astLineCrossing_( AstFrame *this, AstLineDef *l1, AstLineDef *l2,
                      double **cross, int *status ) {
   if ( !astOK ) return 0;
   return (**astMEMBER(this,Frame,LineCrossing))( this, l1, l2, cross, status );
}
void astLineOffset_( AstFrame *this, AstLineDef *line, double par, double prp,
                     double point[2], int *status ){
   if ( !astOK ) return;
   (**astMEMBER(this,Frame,LineOffset))( this, line, par, prp, point, status );
}
int astLineContains_( AstFrame *this, AstLineDef *l, int def, double *point, int *status ) {
   if ( !astOK ) return 0;
   return (**astMEMBER(this,Frame,LineContains))( this, l, def, point, status );
}
AstFrameSet *astConvert_( AstFrame *from, AstFrame *to,
                          const char *domainlist, int *status ) {
   if ( !astOK ) return NULL;
   return (**astMEMBER(from,Frame,Convert))( from, to, domainlist, status );
}
AstFrameSet *astConvertX_( AstFrame *to, AstFrame *from,
                           const char *domainlist, int *status ) {
   if ( !astOK ) return NULL;
   return (**astMEMBER(to,Frame,ConvertX))( to, from, domainlist, status );
}
double astAngle_( AstFrame *this, const double a[], const double b[],
                  const double c[], int *status ) {
   if ( !astOK ) return AST__BAD;
   return (**astMEMBER(this,Frame,Angle))( this, a, b, c, status );
}
int astGetActiveUnit_( AstFrame *this, int *status ) {
   if ( !astOK ) return 0;
   return (**astMEMBER(this,Frame,GetActiveUnit))( this, status );
}
int astTestActiveUnit_( AstFrame *this, int *status ) {
   if ( !astOK ) return 0;
   return (**astMEMBER(this,Frame,TestActiveUnit))( this, status );
}
void astSetActiveUnit_( AstFrame *this, int value, int *status ) {
   if ( !astOK ) return;
   (**astMEMBER(this,Frame,SetActiveUnit))( this, value, status );
}
double astDistance_( AstFrame *this,
                     const double point1[], const double point2[], int *status ) {
   if ( !astOK ) return AST__BAD;
   return (**astMEMBER(this,Frame,Distance))( this, point1, point2, status );
}
AstFrameSet *astFindFrame_( AstFrame *target, AstFrame *template,
                            const char *domainlist, int *status ) {
   if ( !astOK ) return NULL;
   return (**astMEMBER(target,Frame,FindFrame))( target, template, domainlist, status );
}
void astMatchAxes_( AstFrame *frm1, AstFrame *frm2, int *axes, int *status ) {
   if ( !astOK ) return;
   (**astMEMBER(frm1,Frame,MatchAxes))( frm1, frm2, axes, status );
}
void astMatchAxesX_( AstFrame *frm2, AstFrame *frm1, int *axes, int *status ) {
   if ( !astOK ) return;
   (**astMEMBER(frm2,Frame,MatchAxesX))( frm2, frm1, axes, status );
}
const char *astFormat_( AstFrame *this, int axis, double value, int *status ) {
   if ( !astOK ) return NULL;
   return (**astMEMBER(this,Frame,Format))( this, axis, value, status );
}
double astGap_( AstFrame *this, int axis, double gap, int *ntick, int *status ) {
   if ( !astOK ) return 0.0;
   return (**astMEMBER(this,Frame,Gap))( this, axis, gap, ntick, status );
}
AstAxis *astGetAxis_( AstFrame *this, int axis, int *status ) {
   if ( !astOK ) return NULL;
   return (**astMEMBER(this,Frame,GetAxis))( this, axis, status );
}
int astGetNaxes_( AstFrame *this, int *status ) {
   if ( !astOK ) return 0;
   return (**astMEMBER(this,Frame,GetNaxes))( this, status );
}
const int *astGetPerm_( AstFrame *this, int *status ) {
   if ( !astOK ) return NULL;
   return (**astMEMBER(this,Frame,GetPerm))( this, status );
}


int astMatch_( AstFrame *this, AstFrame *target, int matchsub,
               int **template_axes, int **target_axes,
               AstMapping **map, AstFrame **result, int *status ) {

   AstFrame *super_this;
   const char *dom;
   int match;

   if ( !astOK ) return 0;

   match = (**astMEMBER(this,Frame,Match))( this, target, matchsub,
                                            template_axes, target_axes,
                                            map, result, status );

/* If the template ("this") could not be used to probe the target, it may
   be because the template class is a more specialised form of the target
   class. E.g. a SkyFrame cannot directly be used to probe a Frame, but a
   Frame *can* be used to probe a SkyFrame. This means (for instance),
   that a basic Frame with Domain FRED cannot be aligned (using astConvert)
   with a CmpFrame with Domain FRED. This sort of alignment is often
   useful, so we try now to use the supplied template to probe a modified
   form of the target that has been cast into the same class as the
   template. This is only possible if the template class is a sub-class of
   the target class. Attempt to do the cast. */
   if( ! match && matchsub ) {
      super_this = (AstFrame *) astCast( this, target );

/* If the cast was  possible, fix the template Domain since the parent
   class may provide a different default Domain, and then invoke the Match
   method appropriate to the new template class (i.e. the target class). */
      if( super_this ) {
         if( astTestDomain( target ) ) {
            dom = astGetDomain( this );
            if( astChrLen( dom ) > 0 ) astSetDomain( super_this, dom );
         }
         match = (**astMEMBER(super_this,Frame,Match))( super_this, target,
                                                        matchsub, template_axes,
                                                        target_axes, map,
                                                        result, status );
         super_this = astAnnul( super_this );
      }
   }

   return match;
}


int astIsUnitFrame_( AstFrame *this, int *status ){
   if ( !astOK ) return 0;
   return (**astMEMBER(this,Frame,IsUnitFrame))( this, status );
}
void astNorm_( AstFrame *this, double value[], int *status ) {
   if ( !astOK ) return;
   (**astMEMBER(this,Frame,Norm))( this, value, status );
}
void astNormBox_( AstFrame *this, double lbnd[], double ubnd[], AstMapping *reg, int *status ) {
   if ( !astOK ) return;
   (**astMEMBER(this,Frame,NormBox))( this, lbnd, ubnd, reg, status );
}
double astAxDistance_( AstFrame *this, int axis, double v1, double v2, int *status ) {
   if ( !astOK ) return AST__BAD;
   return (**astMEMBER(this,Frame,AxDistance))( this, axis, v1, v2, status );
}
double astAxOffset_( AstFrame *this, int axis, double v1, double dist, int *status ) {
   if ( !astOK ) return AST__BAD;
   return (**astMEMBER(this,Frame,AxOffset))( this, axis, v1, dist, status );
}


AstPointSet *astFrameGrid_( AstFrame *this, int size, const double *lbnd,
                            const double *ubnd, int *status ){
   if ( !astOK ) return NULL;
   return (**astMEMBER(this,Frame,FrameGrid))( this, size, lbnd, ubnd, status );
}


void astOffset_( AstFrame *this, const double point1[], const double point2[],
                 double offset, double point3[], int *status ) {
   if ( !astOK ) return;
   (**astMEMBER(this,Frame,Offset))( this, point1, point2, offset, point3, status );
}
double astAxAngle_( AstFrame *this, const double a[2], const double b[2],
                    int axis, int *status ) {
   if ( !astOK ) return AST__BAD;
   return (**astMEMBER(this,Frame,AxAngle))( this, a, b, axis, status );
}
double astOffset2_( AstFrame *this, const double point1[2], double angle,
                 double offset, double point2[2], int *status ) {
   if ( !astOK ) return AST__BAD;
   return (**astMEMBER(this,Frame,Offset2))( this, point1, angle, offset, point2, status );
}
void astIntersect_( AstFrame *this, const double a1[2],
                    const double a2[2], const double b1[2],
                    const double b2[2], double cross[2],
                    int *status ) {
   if ( !astOK ) return;
   (**astMEMBER(this,Frame,Intersect))( this, a1, a2, b1, b2, cross, status );
}
void astOverlay_( AstFrame *template, const int *template_axes,
                  AstFrame *result, int *status ) {
   if ( !astOK ) return;
   (**astMEMBER(template,Frame,Overlay))( template, template_axes, result, status );
}
void astPermAxes_( AstFrame *this, const int perm[], int *status ) {
   if ( !astOK ) return;
   (**astMEMBER(this,Frame,PermAxes))( this, perm, status );
}
AstFrame *astPickAxes_( AstFrame *this, int naxes, const int axes[],
                        AstMapping **map, int *status ) {
   if ( !astOK ) return NULL;
   return (**astMEMBER(this,Frame,PickAxes))( this, naxes, axes, map, status );
}
void astPrimaryFrame_( AstFrame *this, int axis1,
                      AstFrame **frame, int *axis2, int *status ) {
   if ( !astOK ) return;
   (**astMEMBER(this,Frame,PrimaryFrame))( this, axis1, frame, axis2, status );
}
void astResolve_( AstFrame *this, const double point1[], const double point2[],
                 const double point3[], double point4[], double *d1,
                 double *d2, int *status ) {
   if ( !astOK ) return;
   (**astMEMBER(this,Frame,Resolve))( this, point1, point2, point3, point4, d1, d2, status );
}
void astSetAxis_( AstFrame *this, int axis, AstAxis *newaxis, int *status ) {
   if ( !astOK ) return;
   (**astMEMBER(this,Frame,SetAxis))( this, axis, newaxis, status );
}
void astSetUnit_( AstFrame *this, int axis, const char *value, int *status ) {
   if ( !astOK ) return;
   (**astMEMBER(this,Frame,SetUnit))( this, axis, value, status );
}
void astClearUnit_( AstFrame *this, int axis, int *status ) {
   if ( !astOK ) return;
   (**astMEMBER(this,Frame,ClearUnit))( this, axis, status );
}
int astSubFrame_( AstFrame *target, AstFrame *template, int result_naxes,
                  const int *target_axes, const int *template_axes,
                  AstMapping **map, AstFrame **result, int *status ) {
   if ( !astOK ) return 0;
   return (**astMEMBER(target,Frame,SubFrame))( target, template, result_naxes,
                                                target_axes, template_axes,
                                                map, result, status );
}
int astUnformat_( AstFrame *this, int axis, const char *string,
                  double *value, int *status ) {
   if ( !astOK ) return 0;
   return (**astMEMBER(this,Frame,Unformat))( this, axis, string, value, status );
}
int astValidateAxis_( AstFrame *this, int axis, const char *method, int *status ) {
   if ( !astOK ) return 0;
   return (**astMEMBER(this,Frame,ValidateAxis))( this, axis, method, status );
}
void astValidateAxisSelection_( AstFrame *this, int naxes, const int *axes,
                                const char *method, int *status ) {
   if ( !astOK ) return;
   (**astMEMBER(this,Frame,ValidateAxisSelection))( this, naxes, axes,
                                                    method, status );
}
AstSystemType astValidateSystem_( AstFrame *this, AstSystemType system, const char *method, int *status ) {
   if ( !astOK ) return AST__BADSYSTEM;
   return (**astMEMBER(this,Frame,ValidateSystem))( this, system, method, status );
}
AstSystemType astSystemCode_( AstFrame *this, const char *system, int *status ) {
   if ( !astOK ) return AST__BADSYSTEM;
   return (**astMEMBER(this,Frame,SystemCode))( this, system, status );
}
const char *astSystemString_( AstFrame *this, AstSystemType system, int *status ) {
   if ( !astOK ) return NULL;
   return (**astMEMBER(this,Frame,SystemString))( this, system, status );
}
int astAxIn_( AstFrame *this, int axis, double lo, double hi, double val,
              int closed, int *status ) {
   if ( !astOK ) return 0;
   return (**astMEMBER(this,Frame,AxIn))( this, axis, lo, hi, val, closed, status );
}
int astGetFrameFlags_( AstFrame *this, int *status ) {
   if ( !astOK ) return 0;
   return (**astMEMBER(this,Frame,GetFrameFlags))( this, status );
}
void astSetFrameFlags_( AstFrame *this, int value, int *status ) {
   if ( !astOK ) return;
   (**astMEMBER(this,Frame,SetFrameFlags))( this, value, status );
}


/* Special public interface functions. */
/* =================================== */
/* These provide the public interface to certain special functions
   whose public interface cannot be handled using macros (such as
   astINVOKE) alone. In general, they are named after the
   corresponding protected version of the function, but with "Id"
   appended to the name. */

/* Public Interface Function Prototypes. */
/* ------------------------------------- */
/* The following functions have public prototypes only (i.e. no
   protected prototypes), so we must provide local prototypes for use
   within this module. */
AstFrame *PickAxesId_( AstFrame *, int, const int[], AstMapping **, int * );
AstFrame *astFrameId_( int, const char *, ... );
const char *astFormatId_( AstFrame *, int, double, int * );
int astUnformatId_( AstFrame *, int, const char *, double *, int * );
void astPermAxesId_( AstFrame *, const int[], int * );

/* Special interface function implementations. */
/* ------------------------------------------- */
const char *astFormatId_( AstFrame *this, int axis, double value, int *status ) {
/*
*++
*  Name:
c     astFormat
f     AST_FORMAT

*  Purpose:
*     Format a coordinate value for a Frame axis.

*  Type:
*     Public virtual function.

*  Synopsis:
c     #include "frame.h"
c     const char *astFormat( AstFrame *this, int axis, double value )
f     RESULT = AST_FORMAT( THIS, AXIS, VALUE, STATUS )

*  Class Membership:
*     Frame method.

*  Description:
c     This function returns a pointer to a string containing the
f     This function returns a character string containing the
*     formatted (character) version of a coordinate value for a Frame
*     axis. The formatting applied is determined by the Frame's
*     attributes and, in particular, by any Format attribute string
*     that has been set for the axis. A suitable default format (based
*     on the Digits attribute value) will be applied if necessary.

*  Parameters:
c     this
f     THIS = INTEGER (given)
*        Pointer to the Frame.
c     axis
f     AXIS = INTEGER (Given)
*        The number of the Frame axis for which formatting is to be
*        performed (axis numbering starts at 1 for the first axis).
c     value
f     VALUE = DOUBLE PRECISION (Given)
*        The coordinate value to be formatted.
f     STATUS = INTEGER (Given and Returned)
f        The global status.

*  Returned Value:
c     astFormat()
c        A pointer to a null-terminated string containing the formatted
c        value.
f     AST_FORMAT = CHARACTER * ( AST__SZCHR )
f        The formatted value.

*  Notes:
c     - The returned pointer is guaranteed to remain valid and the
c     string to which it points will not be over-written for a total
c     of 50 successive invocations of this function. After this, the
c     memory containing the string may be re-used, so a copy of the
c     string should be made if it is needed for longer than this.
c     - A formatted value may be converted back into a numerical (double)
c     value using astUnformat.
f     - A formatted value may be converted back into a numerical
f     (double precision) value using AST_UNFORMAT.
c     - A NULL pointer will be returned if this function is invoked
c     with the AST error status set, or if it should fail for any
c     reason.
f     - A blank string will be returned if this function is invoked
f     with STATUS set to an error value, or if it should fail for any
f     reason.
*--

*  Implementation Notes:
*     This function implements the public interface for the astFormat
*     method. It is identical to astFormat_ except that:
*
*     - The axis index is decremented by 1 before use. This allows the
*     public interface to use 1-based axis numbers (whereas internally
*     axis numbers are zero-based).
*
*     - The returned string value is buffered in dynamically allocated
*     memory so that it will remain valid for a guaranteed number of
*     function invocations.
*/

/* Local Variables: */
   astDECLARE_GLOBALS            /* Thread-specific global data */
   const char *fvalue;           /* Pointer to formatted value */
   const char *result;           /* Pointer value to return */
   int i;                        /* Loop counter for initialisation */

/* Initialise. */
   result = NULL;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Get a pointer to Thread-specific global data. */
   astGET_GLOBALS(this);

/* If the "astformatid_strings" array has not been initialised, fill it with NULL
   pointers. */
   if ( !astformatid_init ) {
      astformatid_init = 1;
      for ( i = 0; i < ASTFORMATID_MAX_STRINGS; i++ ) astformatid_strings[ i ] = NULL;
   }

/* Invoke the normal astFormat_ function to obtain a pointer to the
   required formatted value, adjusting the axis index to become
   zero-based. */
   fvalue = astFormat( this, axis - 1, value );

/* If OK, store a copy of the resulting string in dynamically allocated memory,
   putting a pointer to the copy into the next element of the "astformatid_strings"
   array.  (This process also de-allocates any previously allocated memory pointed
   at by this "astformatid_strings" element, so the earlier string is effectively
   replaced by the new one.) */
   if ( astOK ) {
      astBeginPM;
      astformatid_strings[ astformatid_istr ] = astStore( astformatid_strings[ astformatid_istr ], fvalue,
                                  strlen( fvalue ) + (size_t) 1 );
      astEndPM;

/* If OK, return a pointer to the copy and increment "astformatid_istr" to use
   the next element of "astformatid_strings" on the next invocation. Recycle
   "astformatid_istr" to zero when all elements have been used. */
      if ( astOK ) {
         result = astformatid_strings[ astformatid_istr++ ];
         if ( astformatid_istr == ( ASTFORMATID_MAX_STRINGS - 1 ) ) astformatid_istr = 0;
      }
   }

/* Return the result. */
   return result;

}

AstFrame *astFrameId_( int naxes, const char *options, ... ) {
/*
*++
*  Name:
c     astFrame
f     AST_FRAME

*  Purpose:
*     Create a Frame.

*  Type:
*     Public function.

*  Synopsis:
c     #include "frame.h"
c     AstFrame *astFrame( int naxes, const char *options, ... )
f     RESULT = AST_FRAME( NAXES, OPTIONS, STATUS )

*  Class Membership:
*     Frame constructor.

*  Description:
*     This function creates a new Frame and optionally initialises its
*     attributes.
*
*     A Frame is used to represent a coordinate system. It does this
*     in rather the same way that a frame around a graph describes the
*     coordinate space in which data are plotted. Consequently, a
*     Frame has a Title (string) attribute, which describes the
*     coordinate space, and contains axes which in turn hold
*     information such as Label and Units strings which are used for
*     labelling (e.g.) graphical output. In general, however, the
*     number of axes is not restricted to two.
*
*     Functions are available for converting Frame coordinate values
*     into a form suitable for display, and also for calculating
*     distances and offsets between positions within the Frame.
*
*     Frames may also contain knowledge of how to transform to and
*     from related coordinate systems.

*  Parameters:
c     naxes
f     NAXES = INTEGER (Given)
*        The number of Frame axes (i.e. the number of dimensions of
*        the coordinate space which the Frame describes).
c     options
f     OPTIONS = CHARACTER * ( * ) (Given)
c        Pointer to a null-terminated string containing an optional
c        comma-separated list of attribute assignments to be used for
c        initialising the new Frame. The syntax used is identical to
c        that for the astSet function and may include "printf" format
c        specifiers identified by "%" symbols in the normal way.
c        If no initialisation is required, a zero-length string may be
c        supplied.
f        A character string containing an optional comma-separated
f        list of attribute assignments to be used for initialising the
f        new Frame. The syntax used is identical to that for the
f        AST_SET routine. If no initialisation is required, a blank
f        value may be supplied.
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
c     astFrame()
f     AST_FRAME = INTEGER
*        A pointer to the new Frame.

*  Examples:
c     frame = astFrame( 2, "Title=Energy Spectrum: Plot %d", n );
c        Creates a new 2-dimensional Frame and initialises its Title
c        attribute to the string "Energy Spectrum: Plot <n>", where
c        <n> takes the value of the int variable "n".
c     frame = astFrame( 2, "Label(1)=Energy, Label(2)=Response" );
c        Creates a new 2-dimensional Frame and initialises its axis
c        Label attributes to suitable string values.
f     FRAME = AST_FRAME( 2, 'Title=Energy Spectrum', STATUS );
f        Creates a new 2-dimensional Frame and initialises its Title
f        attribute to the string "Energy Spectrum".
f     FRAME = AST_FRAME( 2, 'Label(1)=Energy, Label(2)=Response', STATUS );
f        Creates a new 2-dimensional Frame and initialises its axis
f        Label attributes to suitable string values.

*  Notes:
*     - A null Object pointer (AST__NULL) will be returned if this
c     function is invoked with the AST error status set, or if it
f     function is invoked with STATUS set to an error value, or if it
*     should fail for any reason.
*--

*  Implementation Notes:
*     - This function implements the external (public) interface to
*     the astFrame constructor function. It returns an ID value
*     (instead of a true C pointer) to external users, and must be
*     provided because astFrame_ has a variable argument list which
*     cannot be encapsulated in a macro (where this conversion would
*     otherwise occur).
*     - The variable argument list also prevents this function from
*     invoking astFrame_ directly, so it must be a re-implementation
*     of it in all respects, except for the final conversion of the
*     result to an ID value.
*/

/* Local Variables: */
   astDECLARE_GLOBALS            /* Pointer to thread-specific global data */
   AstFrame *new;                /* Pointer to new Frame */
   va_list args;                 /* Variable argument list */

   int *status;                  /* Pointer to inherited status value */

/* Get a pointer to the inherited status value. */
   status = astGetStatusPtr;

/* Get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(NULL);

/* Check the global error status. */
   if ( !astOK ) return NULL;

/* Initialise the Frame, allocating memory and initialising the virtual
   function table as well if necessary. */
   new = astInitFrame( NULL, sizeof( AstFrame ), !class_init, &class_vtab,
                       "Frame", naxes );

/* If successful, note that the virtual function table has been initialised. */
   if ( astOK ) {
      class_init = 1;

/* Obtain the variable argument list and pass it along with the options string
   to the astVSet method to initialise the new Frame's attributes. */
      va_start( args, options );
      astVSet( new, options, NULL, args );
      va_end( args );

/* If an error occurred, clean up by deleting the new object. */
      if ( !astOK ) new = astDelete( new );
   }

/* Return an ID value for the new Frame. */
   return astMakeId( new );
}

void astPermAxesId_( AstFrame *this, const int perm[], int *status ) {
/*
*++
*  Name:
c     astPermAxes
f     AST_PERMAXES

*  Purpose:
*     Permute the axis order in a Frame.

*  Type:
*     Public virtual function.

*  Synopsis:
c     #include "frame.h"
c     void astPermAxes( AstFrame *this, const int perm[] )
f     CALL AST_PERMAXES( THIS, PERM, STATUS )

*  Class Membership:
*     Frame method.

*  Description:
c     This function permutes the order in which a Frame's axes occur.
f     This routine permutes the order in which a Frame's axes occur.

*  Parameters:
c     this
f     THIS = INTEGER (Given)
*        Pointer to the Frame.
c     perm
f     PERM( * ) = INTEGER (Given)
*        An array with one element for each axis of the Frame (Naxes
*        attribute). This should list the axes in their new order,
*        using the original axis numbering (which starts at 1 for the
*        first axis).
f     STATUS = INTEGER (Given and Returned)
f        The global status.

*  Notes:
*     - Only genuine permutations of the axis order are permitted, so
c     each axis must be referenced exactly once in the "perm" array.
f     each axis must be referenced exactly once in the PERM array.
*     - If successive axis permutations are applied to a Frame, then
*     the effects are cumulative.
*--

*  Implementation Notes:
*     This function implements the public interface for the
*     astPermAxes method. It is identical to astPermAxes_ except that
*     the axis numbers in the "perm" array are decremented by 1 before
*     use. This is to allow the public interface to use one-based axis
*     numbering (internally, zero-based axis numbering is used).
*/

/* Local Variables: */
   int *perm1;                   /* Pointer to modified perm array */
   int axis;                     /* Loop counter for Frame axes */
   int naxes;                    /* Number of Frame axes */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain the number of Frame axes. */
   naxes = astGetNaxes( this );

/* Allocate an array to hold a modified version of the "perm"
   array. */
   perm1 = astMalloc( sizeof( int ) * (size_t) naxes );
   if ( astOK ) {

/* Make a modified copy of the "perm" array by subtracting one from
   each element. This allows the public interface to use one-based
   axis numbering, whereas all internal code is zero-based. */
      for ( axis = 0; axis < naxes; axis++ ) perm1[ axis ] = perm[ axis ] - 1;

/* Invoke the normal astPermAxes_ function to permute the Frame's axes. */
      astPermAxes( this, perm1 );
   }

/* Free the temporary array. */
   perm1 = astFree( perm1 );
}

AstFrame *astPickAxesId_( AstFrame *this, int naxes, const int axes[],
                          AstMapping **map, int *status ) {
/*
*++
*  Name:
c     astPickAxes
f     AST_PICKAXES

*  Purpose:
*     Create a new Frame by picking axes from an existing one.

*  Type:
*     Public virtual function.

*  Synopsis:
c     #include "frame.h"
c     AstFrame *astPickAxes( AstFrame *this, int naxes, const int axes[],
c                            AstMapping **map )
f     RESULT = AST_PICKAXES( THIS, NAXES, AXES, MAP, STATUS )

*  Class Membership:
*     Frame method.

*  Description:
*     This function creates a new Frame whose axes are copied from an
*     existing Frame along with other Frame attributes, such as its
*     Title. Any number (zero or more) of the original Frame's axes
*     may be copied, in any order, and additional axes with default
*     attributes may also be included in the new Frame.
*
c     Optionally, a Mapping that converts between the coordinate
f     A Mapping that converts between the coordinate
*     systems described by the two Frames will also be returned.

*  Parameters:
c     this
f     THIS = INTEGER (Given)
*        Pointer to the original Frame.
c     naxes
f     NAXES = INTEGER (Given)
*        The number of axes required in the new Frame.
c     axes
f     AXES( NAXES ) = INTEGER (Given)
c        An array, with "naxes" elements, which lists the axes to be
f        An array which lists the axes to be
*        copied. These should be given in the order required in the
*        new Frame, using the axis numbering in the original Frame
*        (which starts at 1 for the first axis). Axes may be selected
*        in any order, but each may only be used once.  If additional
*        (default) axes are also to be included, the corresponding
*        elements of this array should be set to zero.
c     map
f     MAP = INTEGER (Returned)
c        Address of a location in which to return a pointer to a new
f        A pointer to a new
*        Mapping. This will be a PermMap (or a UnitMap as a special
*        case) that describes the axis permutation that has taken
*        place between the original and new Frames. The Mapping's
*        forward transformation will convert coordinates from the
*        original Frame into the new one, and vice versa.
c
c        If this Mapping is not required, a NULL value may be supplied
c        for this parameter.
f     STATUS = INTEGER (Given and Returned)
f        The global status.

*  Returned Value:
c     astPickAxes()
f     AST_PICKAXES = INTEGER
*        A pointer to the new Frame.

*  Applicability:
*     Frame
*        This function applies to all Frames. The class of Frame returned
*        may differ from that of the original Frame, depending on which
*        axes are selected. For example, if a single axis is picked from a
*        SkyFrame (which must always have two axes) then the resulting
*        Frame cannot be a valid SkyFrame, so will revert to the parent
*        class (Frame) instead.
*     FrameSet
*        Using this function on a FrameSet is identical to using it on
*        the current Frame in the FrameSet. The returned Frame will not
*        be a FrameSet.
*     Region
*        If this function is used on a Region, an attempt is made to
*        retain the bounds information on the selected axes. If
*        succesful, the returned Frame will be a Region of some class.
*        Otherwise, the returned Frame is obtained by calling this
*        function on the Frame represented by the supplied Region (the
*        returned Frame will then not be a Region). In order to be
*        succesful, the selected axes in the Region must be independent
*        of the others. For instance, a Box can be split in this way but
*        a Circle cannot. Another requirement for success is that no
*        default axes are added (that is, the
c        "axes"
f        AXES
*        array must not contain any zero values.

*  Notes:
c     - The new Frame will contain a "deep" copy (c.f. astCopy) of all
f     - The new Frame will contain a "deep" copy (c.f. AST_COPY) of all
*     the data selected from the original Frame. Modifying any aspect
*     of the new Frame will therefore not affect the original one.
*     - A null Object pointer (AST__NULL) will be returned if this
c     function is invoked with the AST error status set, or if it
f     function is invoked with STATUS set to an error value, or if it
*     should fail for any reason.
*--

*  Implementation Notes:
*     This function implements the public interface for the
*     astPickAxes method. It is identical to astPickAxes_ except for
*     the following:
*
*     - The axis numbers in the "axes" array are decremented by 1 before
*     use. This is to allow the public interface to use one-based axis
*     numbering (internally, zero-based axis numbering is used).
*
*     - An ID value is returned via the "map" parameter (if used)
*     instead of a true C pointer. This is required because this
*     conversion cannot be performed by the macro that invokes the
*     function.
*/

/* Local Variables: */
   AstFrame *result;             /* Pointer to result Frame */
   int *axes1;                   /* Pointer to modified axes array */
   int axis;                     /* Loop counter for axes */

/* Initialise. */
   result = NULL;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Allocate an array to hold a modified version of the "axes" array
   (check that "naxes" is valid first - if not, this error will be
   reported by astPickAxes_ below). */
   axes1 = ( naxes >= 0 ) ? astMalloc( sizeof( int ) * (size_t) naxes ) :
                            NULL;
   if ( astOK ) {

/* Make a modified copy of the "axes" array by subtracting one from
   each element. This allows the public interface to use one-based
   axis numbering, whereas all internal code is zero-based. */
      for ( axis = 0; axis < naxes; axis++ ) axes1[ axis ] = axes[ axis ] - 1;

/* Invoke the normal astPickAxes_ function to select the required axes. */
      result = astPickAxes( this, naxes, axes1, map );
   }

/* Free the temporary array. */
   axes1 = astFree( axes1 );

/* If required, return an ID value for the Mapping. */
   if ( map ) *map = astMakeId( *map );

/* Return the result. */
   return result;
}

int astUnformatId_( AstFrame *this, int axis, const char *string,
                    double *value, int *status ) {
/*
*++
*  Name:
c     astUnformat
f     AST_UNFORMAT

*  Purpose:
*     Read a formatted coordinate value for a Frame axis.

*  Type:
*     Public virtual function.

*  Synopsis:
c     #include "frame.h"
c     int astUnformat( AstFrame *this, int axis, const char *string,
c                      double *value )
f     RESULT = AST_UNFORMAT( THIS, AXIS, STRING, VALUE, STATUS )

*  Class Membership:
*     Frame method.

*  Description:
c     This function reads a formatted coordinate value (given as a
c     character string) for a Frame axis and returns the equivalent
c     numerical (double) value. It also returns the number of
c     characters read from the string.
f     This function reads a formatted coordinate value (given as a
f     character string) for a Frame axis and returns the equivalent
f     numerical (double precision) value. It also returns the number
f     of characters read from the string.
*
c     The principle use of this function is in decoding user-supplied
c     input which contains formatted coordinate values. Free-format
c     input is supported as far as possible. If input is ambiguous, it
c     is interpreted with reference to the Frame's attributes (in
c     particular, the Format string associated with the Frame's
c     axis). This function is, in essence, the inverse of astFormat.
f     The principle use of this function is in decoding user-supplied
f     input which contains formatted coordinate values. Free-format
f     input is supported as far as possible. If input is ambiguous, it
f     is interpreted with reference to the Frame's attributes (in
f     particular, the Format string associated with the Frame's
f     axis). This function is, in essence, the inverse of AST_FORMAT.

*  Parameters:
c     this
f     THIS = INTEGER (Given)
*        Pointer to the Frame.
c     axis
f     AXIS = INTEGER (Given)
*        The number of the Frame axis for which a coordinate value is to
*        be read (axis numbering starts at 1 for the first axis).
c     string
f     STRING = CHARACTER * ( * ) (Given)
c        Pointer to a null-terminated character string containing the
c        formatted coordinate value.
f        A character string containing the formatted coordinate value.
*        This string may contain additional information following the
*        value to be read, in which case reading stops at the first
*        character which cannot be interpreted as part of the value.
*        Any white space before or after the value is discarded.
c     value
f     VALUE = DOUBLE PRECISION (Returned)
c        Pointer to a double in which the coordinate value read will be
c        returned.
f        The coordinate value read.
f     STATUS = INTEGER (Given and Returned)
f        The global status.

*  Returned Value:
c     astUnformat()
f     AST_UNFORMAT = INTEGER
*        The number of characters read from the string in order to
*        obtain the coordinate value. This will include any white
*        space which occurs before or after the value.

*  Applicability:
*     Frame
*        This function applies to all Frames. See the "Frame Input
*        Format" section below for details of the input formats
*        accepted by a basic Frame.
*     SkyFrame
*        The SkyFrame class re-defines the input format to be suitable
*        for representing angles and times, with the resulting
*        coordinate value returned in radians.  See the "SkyFrame
*        Input Format" section below for details of the formats
*        accepted.
*     FrameSet
*        The input formats accepted by a FrameSet are determined by
*        its current Frame (as specified by the Current attribute).

*  Frame Input Format
*     The input format accepted for a basic Frame axis is as follows:
*     - An optional sign, followed by:
*     - A sequence of one or more digits possibly containing a decimal point,
*     followed by:
*     - An optional exponent field.
*     - The exponent field, if present, consists of "E" or "e"
*     followed by a possibly signed integer.
*
*     Examples of acceptable Frame input formats include:
*     - 99
*     - 1.25
*     - -1.6
*     - 1E8
*     - -.99e-17
*     - <bad>

*  SkyFrame Input Format
*     The input format accepted for a SkyFrame axis is as follows:
*     - An optional sign, followed by between one and three fields
*     representing either degrees, arc-minutes, arc-seconds or hours,
*     minutes, seconds (e.g. "-12 42 03").
*     - Each field should consist of a sequence of one or more digits,
*     which may include leading zeros. At most one field may contain a
*     decimal point, in which case it is taken to be the final field
*     (e.g. decimal degrees might be given as "124.707", while degrees
*     and decimal arc-minutes might be given as "-13 33.8").
*     - The first field given may take any value, allowing angles and
*     times outside the conventional ranges to be
*     represented. However, subsequent fields must have values of less
*     than 60 (e.g. "720 45 31" is valid, whereas "11 45 61" is not).
*     - Fields may be separated by white space or by ":" (colon), but
*     the choice of separator must be used consistently throughout the
*     value. Additional white space may be present around fields and
*     separators (e.g. "- 2: 04 : 7.1").
*     - The following field identification characters may be used as
*     separators to replace either of those above (or may be appended
*     to the final field), in order to identify the field to which
*     they are appended: "d"---degrees; "h"---hours; "m"---minutes of
*     arc or time; "s"---seconds of arc or time; "'" (single
*     quote)---minutes of arc; """ (double quote)---seconds of arc.
*     Either lower or upper case may be used.  Fields must be given in
*     order of decreasing significance (e.g. "-11D 3' 14.4"" or
*     "22h14m11.2s").
*     - The presence of any of the field identification characters
*     "d", "'" (single quote) or """ (double quote) indicates that the
*     value is to be interpreted as an angle. Conversely, the presence
*     of "h" indicates that it is to be interpreted as a time (with 24
*     hours corresponding to 360 degrees). Incompatible angle/time
*     identification characters may not be mixed (e.g. "10h14'3"" is
*     not valid).  The remaining field identification characters and
*     separators do not specify a preference for an angle or a time
*     and may be used with either.
c     - If no preference for an angle or a time is expressed anywhere
c     within the value, it is interpreted as an angle if the Format
c     attribute string associated with the SkyFrame axis generates an
c     angle and as a time otherwise. This ensures that values produced
c     by astFormat are correctly interpreted by astUnformat.
f     - If no preference for an angle or a time is expressed anywhere
f     within the value, it is interpreted as an angle if the Format
f     attribute string associated with the SkyFrame axis generates an
f     angle and as a time otherwise. This ensures that values produced
f     by AST_FORMAT are correctly interpreted by AST_UNFORMAT.
*     - Fields may be omitted, in which case they default to zero. The
*     remaining fields may be identified by using appropriate field
*     identification characters (see above) and/or by adding extra
*     colon separators (e.g. "-05m13s" is equivalent to "-:05:13"). If
*     a field is not identified explicitly, it is assumed that
*     adjacent fields have been given, after taking account of any
*     extra separator characters (e.g. "14:25.4s" specifies minutes
*     and seconds, while "14::25.4s" specifies degrees and seconds).
c     - If fields are omitted in such a way that the remaining ones
c     cannot be identified uniquely (e.g. "01:02"), then the first
c     field (either given explicitly or implied by an extra leading
c     colon separator) is taken to be the most significant field that
c     astFormat would produce when formatting a value (using the
c     Format attribute associated with the SkyFrame axis).  By
c     default, this means that the first field will normally be
c     interpreted as degrees or hours. However, if this does not
c     result in consistent field identification, then the last field
c     (either given explicitly or implied by an extra trailing colon
c     separator) is taken to to be the least significant field that
c     astFormat would produce.
f     - If fields are omitted in such a way that the remaining ones
f     cannot be identified uniquely (e.g. "01:02"), then the first
f     field (either given explicitly or implied by an extra leading
f     colon separator) is taken to be the most significant field that
f     AST_FORMAT would produce when formatting a value (using the
f     Format attribute associated with the SkyFrame axis).  By
f     default, this means that the first field will normally be
f     interpreted as degrees or hours. However, if this does not
f     result in consistent field identification, then the last field
f     (either given explicitly or implied by an extra trailing colon
f     separator) is taken to to be the least significant field that
f     AST_FORMAT would produce.
*
c     This final convention is intended to ensure that values formatted
c     by astFormat which contain less than three fields will be
c     correctly interpreted if read back using astUnformat, even if
c     they do not contain field identification characters.
f     This final convention is intended to ensure that values formatted
f     by AST_FORMAT which contain less than three fields will be
f     correctly interpreted if read back using AST_UNFORMAT, even if
f     they do not contain field identification characters.
*
*     Examples of acceptable SkyFrame input formats (with
*     interpretation in parentheses) include:
*     - -14d 13m 22.2s (-14d 13' 22.2")
*     - + 12:34:56.7 (12d 34' 56.7" or 12h 34m 56.7s)
*     - 001 : 02 : 03.4 (1d 02' 03.4" or 1h 02m 03.4s)
*     - 22h 30 (22h 30m 00s)
*     - 136::10" (136d 00' 10" or 136h 00m 10s)
*     - -14M 27S (-0d 14' 27" or -0h 14m 27s)
*     - -:14: (-0d 14' 00" or -0h 14m 00s)
*     - -::4.1 (-0d 00' 04.1" or -0h 00m 04.1s)
*     - .9" (0d 00' 00.9")
*     - d12m (0d 12' 00")
*     - H 12:22.3s (0h 12m 22.3s)
*     - <bad> (AST__BAD)
*
*     Where alternative interpretations are shown, the choice of angle or
*     time depends on the associated Format(axis) attribute.

*  Notes:
*     - A function value of zero (and no coordinate value) will be
*     returned, without error, if the string supplied does not contain
*     a suitably formatted value.
c     - Beware that it is possible for a formatting error part-way
c     through an input string to terminate input before it has been
c     completely read, but to yield a coordinate value that appears
c     valid. For example, if a user types "1.5r6" instead of "1.5e6",
c     the "r" will terminate input, giving an incorrect coordinate
c     value of 1.5. It is therefore most important to check the return
c     value of this function to ensure that the correct number of
c     characters have been read.
f     - Beware that it is possible for a formatting error part-way
f     through an input string to terminate input before it has been
f     completely read, but to yield a coordinate value that appears
f     valid. For example, if a user types "1.5R6" instead of "1.5E6",
f     the "R" will terminate input, giving an incorrect coordinate
f     value of 1.5. It is therefore most important to check the return
f     value of this function to ensure that the correct number of
f     characters have been read.
*     - An error will result if a value is read which appears to have
*     the correct format, but which cannot be converted into a valid
*     coordinate value (for instance, because the value of one or more
*     of its fields is invalid).
*     - The string "<bad>" is recognised as a special case and will
*     yield the coordinate value AST__BAD without error. The test for
*     this string is case-insensitive and also permits embedded white
*     space.
c     - A function result of zero will be returned and no coordinate
c     value will be returned via the "value" pointer if this function
c     is invoked with the AST error status set, or if it should fail
c     for any reason.
f     - A function result of zero will be returned and no coordinate
f     value will be returned via the VALUE argument if this function
f     is invoked with the AST error status set, or if it should fail
f     for any reason.
*--

*  Implementation Notes:
*     This function implements the public interface for the
*     astUnformat method. It is identical to astUnformat_ except that:
*
*     - The axis index is decremented by 1 before use. This allows the
*     public interface to use 1-based axis numbers (whereas internally
*     axis numbers are zero-based).
*/

/* Invoke the normal astUnformat_ function, adjusting the axis index
   to become zero-based. */
   return astUnformat( this, axis - 1, string, value );
}













