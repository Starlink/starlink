/*
*class++
*  Name:
*     FrameSet

*  Purpose:
*     Set of inter-related coordinate systems.

*  Constructor Function:
c     astFrameSet
f     AST_FRAMESET

*  Description:
*     A FrameSet consists of a set of one or more Frames (which
*     describe coordinate systems), connected together by Mappings
*     (which describe how the coordinate systems are inter-related). A
*     FrameSet makes it possible to obtain a Mapping between any pair
*     of these Frames (i.e. to convert between any of the coordinate
*     systems which it describes).  The individual Frames are
*     identified within the FrameSet by an integer index, with Frames
*     being numbered consecutively from one as they are added to the
*     FrameSet.
*
*     Every FrameSet has a "base" Frame and a "current" Frame (which
*     are allowed to be the same). Any of the Frames may be nominated
*     to hold these positions, and the choice is determined by the
*     values of the FrameSet's Base and Current attributes, which hold
*     the indices of the relevant Frames.  By default, the first Frame
*     added to a FrameSet is its base Frame, and the last one added is
*     its current Frame.
*
*     The base Frame describes the "native" coordinate system of
*     whatever the FrameSet is used to calibrate (e.g. the pixel
*     coordinates of an image) and the current Frame describes the
*     "apparent" coordinate system in which it should be viewed
*     (e.g. displayed, etc.). Any further Frames represent a library
*     of alternative coordinate systems, which may be selected by
*     making them current.
*
*     When a FrameSet is used in a context that requires a Frame,
*     (e.g. obtaining its Title value, or number of axes), the current
*     Frame is used. A FrameSet may therefore be used in place of its
*     current Frame in most situations.
*
*     When a FrameSet is used in a context that requires a Mapping,
*     the Mapping used is the one between its base Frame and its
*     current Frame. Thus, a FrameSet may be used to convert "native"
*     coordinates into "apparent" ones, and vice versa. Like any
c     Mapping, a FrameSet may also be inverted (see astInvert), which
f     Mapping, a FrameSet may also be inverted (see AST_INVERT), which
*     has the effect of interchanging its base and current Frames and
*     hence of reversing the Mapping between them.
*
*     Regions may be added into a FrameSet (since a Region is a type of
*     Frame), either explicitly or as components within CmpFrames. In this
*     case the Mapping between a pair of Frames within a FrameSet will
*     include the effects of the clipping produced by any Regions included
*     in the path between the Frames.

*  Inheritance:
*     The FrameSet class inherits from the Frame class.

*  Attributes:
*     In addition to those attributes common to all Frames, every
*     FrameSet also has the following attributes:
*
*     - Base: FrameSet base Frame index
*     - Current: FrameSet current Frame index
*     - Nframe: Number of Frames in a FrameSet
*
*     Every FrameSet also inherits any further attributes that belong
*     to its current Frame, regardless of that Frame's class. (For
*     example, the Equinox attribute, defined by the SkyFrame class, is
*     inherited by any FrameSet which has a SkyFrame as its current
*     Frame.) The set of attributes belonging to a FrameSet may therefore
*     change when a new current Frame is selected.

*  Functions:
c     In addition to those functions applicable to all Frames, the
c     following functions may also be applied to all FrameSets:
f     In addition to those routines applicable to all Frames, the
f     following routines may also be applied to all FrameSets:
*
c     - astAddFrame: Add a Frame to a FrameSet to define a new coordinate
c     system
c     - astGetFrame: Obtain a pointer to a specified Frame in a FrameSet
c     - astGetMapping: Obtain a Mapping between two Frames in a FrameSet
c     - astRemapFrame: Modify a Frame's relationship to the other Frames in a
c     FrameSet
c     - astRemoveFrame: Remove a Frame from a FrameSet
f     - AST_ADDFRAME: Add a Frame to a FrameSet to define a new coordinate
f     system
f     - AST_GETFRAME: Obtain a pointer to a specified Frame in a FrameSet
f     - AST_GETMAPPING: Obtain a Mapping between two Frames in a FrameSet
f     - AST_REMAPFRAME: Modify a Frame's relationship to the other Frames in a
f     FrameSet
f     - AST_REMOVEFRAME: Remove a Frame from a FrameSet

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

*  History:
*     16-FEB-1996 (RFWS):
*        Original version.
*     5-JUN-1996 (RFWS):
*        Tidied up, etc.
*     2-JUL-1996 (RFWS):
*        Fixed bug in astRemoveFrame which caused the base/current
*        Frame index to be wrong.
*     12-JUL-1996 (RFWS):
*        Over-ride the astReportPoints method to provide
*        Frame-specific formatting.
*     12-AUG-1996 (RFWS):
*        Upgraded to provide a public interface, plus improvements to
*        astAlign and the handling of nodes as Frames are
*        added/removed.
*     11-SEP-1996 (RFWS):
*        Added Gap.
*     25-SEP-1996 (RFWS):
*        Added I/O facilities.
*     30-MAY-1997 (RFWS):
*        Add special treatment for the ID attribute (which is not
*        derived from the current Frame).
*     10-JUN-1997 (RFWS):
*        Rationalised the astConvert implementation.
*     11-JUN-1997 (RFWS):
*        Added the FindFrame implementation.
*     27-JUN-1997 (RFWS):
*        Fixed bug which caused certain Mapping attributes to be
*        handled by the current Frame instead of by the member
*        functions defined by this class.
*     3-JUL-1997 (RFWS):
*        Fixed bug: failing to extend the invert array in
*        astRemapFrame.
*     10-JUL-1997 (RFWS):
*        Over-ride the astSimplify method.
*     14-NOV-1997 (RFWS):
*        Fixed error in loop implementing search over domains in
*        FindFrame.
*     20-NOV-1997 (RFWS):
*        Fixed bug in default Base and Current attribute values when a
*        FrameSet has been inverted.
*     20-NOV-1997 (RFWS):
*        Modified astConvert to use the current Frame of the "to"
*        FrameSet as the destination coordinate system (instead of the
*        base Frame) and to modify its Base attribute instead of its
*        Current attribute.
*     22-DEC-1997 (RFWS):
*        Further modified astConvert to convert from the Current Frame
*        of the "from" FrameSet and to modify its Base
*        attribute. Frame search order also reversed if the Invert
*        attribute is non-zero for either FrameSet.
*     19-JAN-1998 (RFWS):
*        Installed the TidyNodes function.
*     20-JAN-1998 (RFWS):
*        Implemented preservation of FrameSet integrity when attribute
*        values associated with the current Frame are modified.
*     24-FEB-1998 (RFWS):
*        Added the ForceCopy function to allow integrity to be preserved
*        when there are multiple references to the same Frame.
*     25-FEB-1998 (RFWS):
*        Over-ride the astUnformat method.
*     24-MAR-1998 (RFWS):
*        Fixed unterminated comment causing problems in CombineMaps.
*     6-APR-1998 (RFWS):
*        Fixed another unterminated comment in CombineMaps.
*     27-MAY-1998 (RFWS):
*        Fixed bug: failure to record new invert flag value after
*        simplifying a CmpMap in TidyNodes.
*     17-DEC-2002 (DSB):
*        Override accessors for Frame attributes Top, Bottom, Epoch,
*        System, AlignSystem and ActiveUnit.
*     8-JAN-2003 (DSB):
*        Changed private InitVtab method to protected astInitFrameSetVtab
*        method.
*     24-JAN-2004 (DSB):
*        o  Override the astFields method.
*        o  Add argument "fmt" to Abbrev.
*     23-MAR-2004 (DSB):
*        Modified astGetMapping and Span to include the clipping effect of
*        any Regions in the path between the two supplied Frames.
*     24-AUG-2004 (DSB):
*        - Override various methods inherited from Frame (astAngle,
*	astAxAngle, astAxDistance, astAxOffset, astCheckPerm, astOffset2,
*	astResolve, astSystemCode, astSystemString, astValidateSystem,
*	astValidateAxisSelection). These should have been overridden a
*       long time ago!
*     8-SEP-2004 (DSB):
*       Override astResolvePoints.
*     12-MAY-2005 (DSB):
*        Override astNormBox method.
*     12-AUG-2005 (DSB):
*        Override ObsLat and ObsLon accessor methods.
*     14-FEB-2006 (DSB):
*        Override astGetObjSize.
*     15-MAY-2006 (DSB):
*        Override astEqual.
*     30-JUN-2006 (DSB):
*        Allow astAbbrev to have a null "str1" value.
*     22-JUN-2007 (DSB):
*        Modify VSet to avoid using the args va_list twice since the
*        first use (by the parent VSet function) invalidates the va_list
*        causing a segvio to be generated by the second use (when
*        formatting an error message).
*     11-JAN-2008 (DSB):
*        Override the astRate method.
*     17-NOV-2008 (DSB):
*        Correct parent class in invocation of astMAKE_ISA.
*     14-JAN-2009 (DSB):
*        Override the astIntersect method.
*     18-JUN-2009 (DSB):
*        Override ObsAlt accessor methods.
*     30-OCT-2009 (DSB):
*        Make the Ident attribute relate to the FrameSet, not the current
*        Frame.
*     22-MAR-2011 (DSB):
*        Override astFrameGrid method.
*     29-APR-2011 (DSB):
*        Prevent astFindFrame from matching a subclass template against a
*        superclass target.
*     2-SEP-2011 (DSB):
*        Fix FrameSet implememntation of astEqual (mapping comparison
*        tests were logically inverted).
*class--
*/

/* Module Macros. */
/* ============== */
/* Set the name of the class we are implementing. This indicates to
   the header files that define class interfaces that they should make
   "protected" symbols available. */
#define astCLASS FrameSet

/*
*  Name:
*     MAKE_CLEAR

*  Purpose:
*     Define a function to clear an attribute value for a FrameSet.

*  Type:
*     Private macro.

*  Synopsis:
*     #include "frameset.h"
*     MAKE_CLEAR(attribute)

*  Class Membership:
*     Defined by the FrameSet class.

*  Description:
*     This macro expands to an implementation of a private member function
*     of the form:
*
*        static void Clear<Attribute>( AstFrame *this )
*
*     that clears the value of a specified attribute for the current Frame
*     of a FrameSet (this). This function is intended to over-ride the
*     astClear<Attribute> method inherited from the Frame class.

*  Parameters:
*     attribute
*        Name of the attribute, as it appears in the function name.
*/

/* Define the macro. */
#define MAKE_CLEAR(attribute) \
static void Clear##attribute( AstFrame *this_frame, int *status ) { \
\
/* Local Variables: */ \
   AstFrame *fr;                 /* Pointer to current Frame */ \
   AstFrameSet *this;            /* Pointer to the FrameSet structure */ \
\
/* Check the global error status. */ \
   if ( !astOK ) return; \
\
/* Obtain a pointer to the FrameSet structure. */ \
   this = (AstFrameSet *) this_frame; \
\
/* Obtain a pointer to the current Frame and invoke its astClear<Attribute> \
   method. Annul the Frame pointer afterwards. */ \
   fr = astGetFrame( this, AST__CURRENT ); \
   astClear##attribute( fr ); \
   fr = astAnnul( fr ); \
}

/*
*  Name:
*     MAKE_CLEAR_AXIS

*  Purpose:
*     Define a function to clear an attribute value for a FrameSet axis.

*  Type:
*     Private macro.

*  Synopsis:
*     #include "frameset.h"
*     MAKE_CLEAR_AXIS(attribute)

*  Class Membership:
*     Defined by the FrameSet class.

*  Description:
*     This macro expands to an implementation of a private member function
*     of the form:
*
*        static void Clear<Attribute>( AstFrame *this, int axis )
*
*     that clears the value of a specified attribute for an axis of
*     the current Frame of a FrameSet (this). This function is
*     intended to over-ride the astClear<Attribute> method inherited
*     from the Frame class.

*  Parameters:
*     attribute
*        Name of the attribute, as it appears in the function name.
*/

/* Define the macro. */
#define MAKE_CLEAR_AXIS(attribute) \
static void Clear##attribute( AstFrame *this_frame, int axis, int *status ) { \
\
/* Local Variables: */ \
   AstFrame *fr;                 /* Pointer to current Frame */ \
   AstFrameSet *this;            /* Pointer to the FrameSet structure */ \
\
/* Check the global error status. */ \
   if ( !astOK ) return; \
\
/* Obtain a pointer to the FrameSet structure. */ \
   this = (AstFrameSet *) this_frame; \
\
/* Validate the axis index supplied. */ \
   (void) astValidateAxis( this, axis, 1, "astClear" #attribute ); \
\
/* Obtain a pointer to the FrameSet's current Frame and invoke its \
   astClear<Attribute> method. Annul the Frame pointer afterwards. */ \
   fr = astGetFrame( this, AST__CURRENT ); \
   astClear##attribute( fr, axis ); \
   fr = astAnnul( fr ); \
}

/*
*  Name:
*     MAKE_GET

*  Purpose:
*     Define a function to get an attribute value for a FrameSet.

*  Type:
*     Private macro.

*  Synopsis:
*     #include "frameset.h"
*     MAKE_GET(attribute,type)

*  Class Membership:
*     Defined by the FrameSet class.

*  Description:
*     This macro expands to an implementation of a private member function
*     of the form:
*
*        static <type> Get<Attribute>( AstFrame *this )
*
*     that gets the value of a specified attribute for the current Frame
*     of a FrameSet (this). This function is intended to over-ride the
*     astGet<Attribute> method inherited from the Frame class.

*  Parameters:
*     attribute
*        Name of the attribute, as it appears in the function name.
*     type
*        The C type of the attribute.
*/

/* Define the macro. */
#define MAKE_GET(attribute,type) \
static type Get##attribute( AstFrame *this_frame, int *status ) { \
\
/* Local Variables: */ \
   AstFrame *fr;                 /* Pointer to current Frame */ \
   AstFrameSet *this;            /* Pointer to the FrameSet structure */ \
   type result;                  /* Value to return */ \
\
/* Check the global error status. */ \
   if ( !astOK ) return (type) 0; \
\
/* Obtain a pointer to the FrameSet structure. */ \
   this = (AstFrameSet *) this_frame; \
\
/* Obtain a pointer to the current Frame and invoke its \
   astGet<Attribute> method.  Annul the Frame pointer afterwards. */ \
   fr = astGetFrame( this, AST__CURRENT ); \
   result = astGet##attribute( fr ); \
   fr = astAnnul( fr ); \
\
/* If an error occurred, clear the result value. */ \
   if ( !astOK ) result = (type) 0; \
\
/* Return the result. */ \
   return result; \
}

/*
*  Name:
*     MAKE_GET_AXIS

*  Purpose:
*     Define a function to get an attribute value for a FrameSet axis.

*  Type:
*     Private macro.

*  Synopsis:
*     #include "frameset.h"
*     MAKE_GET_AXIS(attribute,type)

*  Class Membership:
*     Defined by the FrameSet class.

*  Description:
*     This macro expands to an implementation of a private member function
*     of the form:
*
*        static <type> Get<Attribute>( AstFrame *this, int axis )
*
*     that gets the value of a specified attribute for an axis of the
*     current Frame of a FrameSet (this). This function is intended to
*     over-ride the astGet<Attribute> method inherited from the Frame
*     class.

*  Parameters:
*     attribute
*        Name of the attribute, as it appears in the function name.
*     type
*        The C type of the attribute.
*/

/* Define the macro. */
#define MAKE_GET_AXIS(attribute,type) \
static type Get##attribute( AstFrame *this_frame, int axis, int *status ) { \
\
/* Local Variables: */ \
   AstFrame *fr;                 /* Pointer to current Frame */ \
   AstFrameSet *this;            /* Pointer to the FrameSet structure */ \
   type result;                  /* Value to return */ \
\
/* Check the global error status. */ \
   if ( !astOK ) return (type) 0; \
\
/* Obtain a pointer to the FrameSet structure. */ \
   this = (AstFrameSet *) this_frame; \
\
/* Validate the axis index supplied. */ \
   (void) astValidateAxis( this, axis, 1, "astGet" #attribute ); \
\
/* Obtain a pointer to the FrameSet's current Frame and invoke its \
   astGet<Attribute> method.  Annul the Frame pointer afterwards. */ \
   fr = astGetFrame( this, AST__CURRENT ); \
   result = astGet##attribute( fr, axis ); \
   fr = astAnnul( fr ); \
\
/* If an error occurred, clear the result value. */ \
   if ( !astOK ) result = (type) 0; \
\
/* Return the result. */ \
   return result; \
}

/*
*  Name:
*     MAKE_SET

*  Purpose:
*     Define a function to set an attribute value for a FrameSet.

*  Type:
*     Private macro.

*  Synopsis:
*     #include "frameset.h"
*     MAKE_SET(attribute,type)

*  Class Membership:
*     Defined by the FrameSet class.

*  Description:
*     This macro expands to an implementation of a private member function
*     of the form:
*
*        static void Set<Attribute>( AstFrame *this, <type> value )
*
*     that sets the value of a specified attribute for the current Frame
*     of a FrameSet (this). This function is intended to over-ride the
*     astSet<Attribute> method inherited from the Frame class.

*  Parameters:
*     attribute
*        Name of the attribute, as it appears in the function name.
*     type
*        The C type of the attribute.
*/

/* Define the macro. */
#define MAKE_SET(attribute,type) \
static void Set##attribute( AstFrame *this_frame, type value, int *status ) { \
\
/* Local Variables: */ \
   AstFrame *fr;                 /* Pointer to current Frame */ \
   AstFrameSet *this;            /* Pointer to the FrameSet structure */ \
\
/* Check the global error status. */ \
   if ( !astOK ) return; \
\
/* Obtain a pointer to the FrameSet structure. */ \
   this = (AstFrameSet *) this_frame; \
\
/* Obtain a pointer to the FrameSet's current Frame and invoke its \
   astSet<Attribute> method.  Annul the Frame pointer afterwards. */ \
   fr = astGetFrame( this, AST__CURRENT ); \
   astSet##attribute( fr, value ); \
   fr = astAnnul( fr ); \
}

/*
*  Name:
*     MAKE_SET_AXIS

*  Purpose:
*     Define a function to set an attribute value for a FrameSet axis.

*  Type:
*     Private macro.

*  Synopsis:
*     #include "frameset.h"
*     MAKE_SET_AXIS(attribute,type)

*  Class Membership:
*     Defined by the FrameSet class.

*  Description:
*     This macro expands to an implementation of a private member function
*     of the form:
*
*        static void Set<Attribute>( AstFrame *this, int axis, <type> value )
*
*     that sets the value of a specified attribute for an axis of the
*     current Frame of a FrameSet (this). This function is intended to
*     over-ride the astSet<Attribute> method inherited from the Frame
*     class.

*  Parameters:
*     attribute
*        Name of the attribute, as it appears in the function name.
*     type
*        The C type of the attribute.
*/

/* Define the macro. */
#define MAKE_SET_AXIS(attribute,type) \
static void Set##attribute( AstFrame *this_frame, int axis, type value, int *status ) { \
\
/* Local Variables: */ \
   AstFrame *fr;                 /* Pointer to current Frame */ \
   AstFrameSet *this;            /* Pointer to the FrameSet structure */ \
\
/* Check the global error status. */ \
   if ( !astOK ) return; \
\
/* Obtain a pointer to the FrameSet structure. */ \
   this = (AstFrameSet *) this_frame; \
\
/* Validate the axis index supplied. */ \
   (void) astValidateAxis( this, axis, 1, "astSet" #attribute ); \
\
/* Obtain a pointer to the FrameSet's current Frame and invoke its \
   astSet<Attribute> method.  Annul the Frame pointer afterwards. */ \
   fr = astGetFrame( this, AST__CURRENT ); \
   astSet##attribute( fr, axis, value ); \
   fr = astAnnul( fr ); \
}

/*
*  Name:
*     MAKE_TEST

*  Purpose:
*     Define a function to test if an attribute value is set for a FrameSet.

*  Type:
*     Private macro.

*  Synopsis:
*     #include "frameset.h"
*     MAKE_TEST(attribute)

*  Class Membership:
*     Defined by the FrameSet class.

*  Description:
*     This macro expands to an implementation of a private member function
*     of the form:
*
*        static int Test<Attribute>( AstFrame *this )
*
*     that returns a boolean result (0 or 1) to indicate if the value
*     of a specified attribute for the current Frame of a FrameSet
*     (this) is set. This function is intended to over-ride the
*     astTest<Attribute> method inherited from the Frame class.

*  Parameters:
*     attribute
*        Name of the attribute, as it appears in the function name.
*/

/* Define the macro. */
#define MAKE_TEST(attribute) \
static int Test##attribute( AstFrame *this_frame, int *status ) { \
\
/* Local Variables: */ \
   AstFrame *fr;                 /* Pointer to current Frame */ \
   AstFrameSet *this;            /* Pointer to FrameSet structure */ \
   int result;                   /* Result to return */ \
\
/* Check the global error status. */ \
   if ( !astOK ) return 0; \
\
/* Obtain a pointer to the FrameSet structure. */ \
   this = (AstFrameSet *) this_frame; \
\
/* Obtain a pointer to the FrameSet's current Frame and invoke its \
   astTest<Attribute> method.  Annul the Frame pointer afterwards. */ \
   fr = astGetFrame( this, AST__CURRENT ); \
   result = astTest##attribute( fr ); \
   fr = astAnnul( fr ); \
\
/* If an error occurred, clear the result value. */ \
   if ( !astOK ) result = 0; \
\
/* Return the result. */ \
   return result; \
}

/*
*  Name:
*     MAKE_TEST_AXIS

*  Purpose:
*     Define a function to test if an attribute value is set for a FrameSet
*     axis.

*  Type:
*     Private macro.

*  Synopsis:
*     #include "frameset.h"
*     MAKE_TEST_AXIS(attribute)

*  Class Membership:
*     Defined by the FrameSet class.

*  Description:
*     This macro expands to an implementation of a private member function
*     of the form:
*
*        static int Test<Attribute>( AstFrame *this, int axis )
*
*     that returns a boolean result (0 or 1) to indicate if the value
*     of a specified attribute for an axis of the current Frame of a
*     FrameSet (this) is set. This function is intended to over-ride
*     the astTest<Attribute> method inherited from the Frame class.

*  Parameters:
*     attribute
*        Name of the attribute, as it appears in the function name.
*/

/* Define the macro. */
#define MAKE_TEST_AXIS(attribute) \
static int Test##attribute( AstFrame *this_frame, int axis, int *status ) { \
\
/* Local Variables: */ \
   AstFrame *fr;                 /* Pointer to current Frame */ \
   AstFrameSet *this;            /* Pointer to the FrameSet structure */ \
   int result;                   /* Value to return */ \
\
/* Check the global error status. */ \
   if ( !astOK ) return 0; \
\
/* Obtain a pointer to the FrameSet structure. */ \
   this = (AstFrameSet *) this_frame; \
\
/* Validate the axis index supplied. */ \
   (void) astValidateAxis( this, axis, 1, "astTest" #attribute ); \
\
/* Obtain a pointer to the FrameSet's current Frame and invoke its \
   astTest<Attribute> method.  Annul the Frame pointer afterwards. */ \
   fr = astGetFrame( this, AST__CURRENT ); \
   result = astTest##attribute( fr, axis ); \
   fr = astAnnul( fr ); \
\
/* If an error occurred, clear the result value. */ \
   if ( !astOK ) result = 0; \
\
/* Return the result. */ \
   return result; \
}

/* Header files. */
/* ============= */
/* Interface definitions. */
/* ---------------------- */

#include "globals.h"             /* Thread-safe global data access */
#include "error.h"               /* Error reporting facilities */
#include "memory.h"              /* Memory allocation facilities */
#include "object.h"              /* Base Object class */
#include "mapping.h"             /* Coordinate Mappings */
#include "unitmap.h"             /* Unit Mappings */
#include "permmap.h"             /* Coordinate permutation Mappings */
#include "cmpmap.h"              /* Compound Mappings */
#include "frame.h"               /* Parent Frame class */
#include "frameset.h"            /* Interface definition for this class */
#include "cmpframe.h"            /* Compound coordinate frames */

/* Error code definitions. */
/* ----------------------- */
#include "ast_err.h"             /* AST error codes */

/* C header files. */
/* --------------- */
#include <ctype.h>
#include <limits.h>
#include <stdarg.h>
#include <stddef.h>
#include <stdio.h>
#include <string.h>

/* Module Variables. */
/* ================= */

/* Address of this static variable is used as a unique identifier for
   member of this class. */
static int class_check;

/* Pointers to parent class methods which are extended by this class. */
static int (* parent_getobjsize)( AstObject *, int * );
static void (* parent_clear)( AstObject *, const char *, int * );
static int (* parent_getusedefs)( AstObject *, int * );
static void (* parent_vset)( AstObject *, const char *, char **, va_list, int * );

#if defined(THREAD_SAFE)
static int (* parent_managelock)( AstObject *, int, int, AstObject **, int * );
#endif

/* Define macros for accessing each item of thread specific global data. */
#ifdef THREAD_SAFE

/* Define how to initialise thread-specific globals. */
#define GLOBAL_inits \
   globals->Class_Init = 0; \
   globals->GetAttrib_Buff[ 0 ] = 0; \
   globals->Integrity_Frame = NULL; \
   globals->Integrity_Method = ""; \
   globals->Integrity_Lost = 0; \

/* Create the function that initialises global data for this module. */
astMAKE_INITGLOBALS(FrameSet)

/* Define macros for accessing each item of thread specific global data. */
#define class_init astGLOBAL(FrameSet,Class_Init)
#define class_vtab astGLOBAL(FrameSet,Class_Vtab)
#define getattrib_buff astGLOBAL(FrameSet,GetAttrib_Buff)
#define integrity_frame astGLOBAL(FrameSet,Integrity_Frame)
#define integrity_method astGLOBAL(FrameSet,Integrity_Method)
#define integrity_lost astGLOBAL(FrameSet,Integrity_Lost)



/* If thread safety is not needed, declare and initialise globals at static
   variables. */
#else

/* Buffer returned by GetAttrib. */
static char getattrib_buff[ 51 ];

/* Variables associated with preserving FrameSet integrity. */
static AstFrame *integrity_frame = NULL; /* Pointer to copy of current Frame */
static const char *integrity_method = ""; /* Name of method being used */
static int integrity_lost = 0;   /* Current Frame modified? */


/* Define the class virtual function table and its initialisation flag
   as static variables. */
static AstFrameSetVtab class_vtab;   /* Virtual function table */
static int class_init = 0;       /* Virtual function table initialised? */

#endif


/* Prototypes for Private Member Functions. */
/* ======================================== */
static int GetObjSize( AstObject *, int * );
static AstAxis *GetAxis( AstFrame *, int, int * );
static AstFrame *GetFrame( AstFrameSet *, int, int * );
static AstFrame *PickAxes( AstFrame *, int, const int[], AstMapping **, int * );
static AstFrameSet *Convert( AstFrame *, AstFrame *, const char *, int * );
static AstFrameSet *ConvertX( AstFrame *, AstFrame *, const char *, int * );
static AstFrameSet *FindFrame( AstFrame *, AstFrame *, const char *, int * );
static AstMapping *CombineMaps( AstMapping *, int, AstMapping *, int, int, int * );
static AstMapping *GetMapping( AstFrameSet *, int, int, int * );
static AstMapping *RemoveRegions( AstMapping *, int * );
static AstMapping *Simplify( AstMapping *, int * );
static AstObject *Cast( AstObject *, AstObject *, int * );
static AstPointSet *FrameGrid( AstFrame *, int, const double *, const double *, int * );
static AstPointSet *ResolvePoints( AstFrame *, const double [], const double [], AstPointSet *, AstPointSet *, int * );
static AstPointSet *Transform( AstMapping *, AstPointSet *, int, AstPointSet *, int * );
static const char *Abbrev( AstFrame *, int, const char *, const char *, const char *, int * );
static const char *Format( AstFrame *, int, double, int * );
static const char *GetAttrib( AstObject *, const char *, int * );
static const char *GetDomain( AstFrame *, int * );
static const char *GetFormat( AstFrame *, int, int * );
static const char *GetLabel( AstFrame *, int, int * );
static const char *GetSymbol( AstFrame *, int, int * );
static const char *GetTitle( AstFrame *, int * );
static const char *GetUnit( AstFrame *, int, int * );
static const int *GetPerm( AstFrame *, int * );
static double Angle( AstFrame *, const double[], const double[], const double[], int * );
static double AxAngle( AstFrame *, const double[], const double[], int, int * );
static double AxDistance( AstFrame *, int, double, double, int * );
static double AxOffset( AstFrame *, int, double, double, int * );
static double Offset2( AstFrame *, const double[2], double, double, double[2], int * );
static double Rate( AstMapping *, double *, int, int, int * );
static AstSystemType ValidateSystem( AstFrame *, AstSystemType, const char *, int * );
static AstSystemType SystemCode( AstFrame *, const char *, int * );
static const char *SystemString( AstFrame *, AstSystemType, int * );
static void CheckPerm( AstFrame *, const int *, const char *, int * );
static void Resolve( AstFrame *, const double [], const double [], const double [], double [], double *, double *, int * );
static void ValidateAxisSelection( AstFrame *, int, const int *, const char *, int * );
static AstLineDef *LineDef( AstFrame *, const double[2], const double[2], int * );
static int Equal( AstObject *, AstObject *, int * );
static int LineCrossing( AstFrame *, AstLineDef *, AstLineDef *, double **, int * );
static int LineContains( AstFrame *, AstLineDef *, int, double *, int * );
static void LineOffset( AstFrame *, AstLineDef *, double, double, double[2], int * );

static double Distance( AstFrame *, const double[], const double[], int * );
static double Gap( AstFrame *, int, double, int *, int * );
static int *MapSplit( AstMapping *, int, const int *, AstMapping **, int * );
static int Fields( AstFrame *, int, const char *, const char *, int, char **, int *, double *, int * );
static int ForceCopy( AstFrameSet *, int, int * );
static int GetBase( AstFrameSet *, int * );
static int GetCurrent( AstFrameSet *, int * );
static int GetDigits( AstFrame *, int * );
static int GetDirection( AstFrame *, int, int * );
static int GetActiveUnit( AstFrame *, int * );
static int GetIsLinear( AstMapping *, int * );
static int GetMatchEnd( AstFrame *, int * );
static int GetMaxAxes( AstFrame *, int * );
static int GetMinAxes( AstFrame *, int * );
static int GetNaxes( AstFrame *, int * );
static int GetNframe( AstFrameSet *, int * );
static int GetNin( AstMapping *, int * );
static int GetNout( AstMapping *, int * );
static int GetPermute( AstFrame *, int * );
static int GetPreserveAxes( AstFrame *, int * );
static int GetTranForward( AstMapping *, int * );
static int GetTranInverse( AstMapping *, int * );
static int IsUnitFrame( AstFrame *, int * );
static int Match( AstFrame *, AstFrame *, int, int **, int **, AstMapping **, AstFrame **, int * );
static int Span( AstFrameSet *, AstFrame **, int, int, int, AstMapping **, int *, int * );
static int SubFrame( AstFrame *, AstFrame *, int, const int *, const int *, AstMapping **, AstFrame **, int * );
static int TestAttrib( AstObject *, const char *, int * );
static int TestBase( AstFrameSet *, int * );
static int TestCurrent( AstFrameSet *, int * );
static int TestDigits( AstFrame *, int * );
static int TestDirection( AstFrame *, int, int * );
static int TestDomain( AstFrame *, int * );
static int TestFormat( AstFrame *, int, int * );
static int TestLabel( AstFrame *, int, int * );
static int TestActiveUnit( AstFrame *, int * );
static int TestMatchEnd( AstFrame *, int * );
static int TestMaxAxes( AstFrame *, int * );
static int TestMinAxes( AstFrame *, int * );
static int TestPermute( AstFrame *, int * );
static int TestPreserveAxes( AstFrame *, int * );
static int TestSymbol( AstFrame *, int, int * );
static int TestTitle( AstFrame *, int * );
static int TestUnit( AstFrame *, int, int * );
static int Unformat( AstFrame *, int, const char *, double *, int * );
static int ValidateAxis( AstFrame *, int, int, const char *, int * );
static int ValidateFrameIndex( AstFrameSet *, int, const char *, int * );
static void AddFrame( AstFrameSet *, int, AstMapping *, AstFrame *, int * );
static void AppendAxes( AstFrameSet *, AstFrame *, int * );
static void Clear( AstObject *, const char *, int * );
static void ClearAttrib( AstObject *, const char *, int * );
static void ClearBase( AstFrameSet *, int * );
static void ClearCurrent( AstFrameSet *, int * );
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
static void MatchAxes( AstFrame *, AstFrame *, int *, int * );
static void MatchAxesX( AstFrame *, AstFrame *, int *, int * );
static void Norm( AstFrame *, double[], int * );
static void NormBox( AstFrame *, double[], double[], AstMapping *, int * );
static void Offset( AstFrame *, const double[], const double[], double, double[], int * );
static void Overlay( AstFrame *, const int *, AstFrame *, int * );
static void PermAxes( AstFrame *, const int[], int * );
static void PrimaryFrame( AstFrame *, int, AstFrame **, int *, int * );
static void RecordIntegrity( AstFrameSet *, int * );
static void RemapFrame( AstFrameSet *, int, AstMapping *, int * );
static void RemoveFrame( AstFrameSet *, int, int * );
static void ReportPoints( AstMapping *, int, AstPointSet *, AstPointSet *, int * );
static void RestoreIntegrity( AstFrameSet *, int * );
static void SetAttrib( AstObject *, const char *, int * );
static void SetAxis( AstFrame *, int, AstAxis *, int * );
static void SetBase( AstFrameSet *, int, int * );
static void SetCurrent( AstFrameSet *, int, int * );
static void SetDigits( AstFrame *, int, int * );
static void SetDirection( AstFrame *, int, int, int * );
static void SetDomain( AstFrame *, const char *, int * );
static void SetFormat( AstFrame *, int, const char *, int * );
static void SetLabel( AstFrame *, int, const char *, int * );
static void SetActiveUnit( AstFrame *, int, int * );
static void SetMatchEnd( AstFrame *, int, int * );
static void SetMaxAxes( AstFrame *, int, int * );
static void SetMinAxes( AstFrame *, int, int * );
static void SetPermute( AstFrame *, int, int * );
static void SetPreserveAxes( AstFrame *, int, int * );
static void SetSymbol( AstFrame *, int, const char *, int * );
static void SetTitle( AstFrame *, const char *, int * );
static void SetUnit( AstFrame *, int, const char *, int * );
static void TidyNodes( AstFrameSet *, int * );
static void VSet( AstObject *, const char *, char **, va_list, int * );

static double GetBottom( AstFrame *, int, int * );
static int TestBottom( AstFrame *, int, int * );
static void ClearBottom( AstFrame *, int, int * );
static void SetBottom( AstFrame *, int, double, int * );

static double GetTop( AstFrame *, int, int * );
static int TestTop( AstFrame *, int, int * );
static void ClearTop( AstFrame *, int, int * );
static void SetTop( AstFrame *, int, double, int * );

static double GetEpoch( AstFrame *, int * );
static int TestEpoch( AstFrame *, int * );
static void ClearEpoch( AstFrame *, int * );
static void SetEpoch( AstFrame *, double, int * );

static double GetObsAlt( AstFrame *, int * );
static int TestObsAlt( AstFrame *, int * );
static void ClearObsAlt( AstFrame *, int * );
static void SetObsAlt( AstFrame *, double, int * );

static double GetObsLat( AstFrame *, int * );
static int TestObsLat( AstFrame *, int * );
static void ClearObsLat( AstFrame *, int * );
static void SetObsLat( AstFrame *, double, int * );

static double GetObsLon( AstFrame *, int * );
static int TestObsLon( AstFrame *, int * );
static void ClearObsLon( AstFrame *, int * );
static void SetObsLon( AstFrame *, double, int * );

static int GetUseDefs( AstObject *, int * );

static AstSystemType GetSystem( AstFrame *, int * );
static int TestSystem( AstFrame *, int * );
static void ClearSystem( AstFrame *, int * );
static void SetSystem( AstFrame *, AstSystemType, int * );

static AstSystemType GetAlignSystem( AstFrame *, int * );
static int TestAlignSystem( AstFrame *, int * );
static void ClearAlignSystem( AstFrame *, int * );
static void SetAlignSystem( AstFrame *, AstSystemType, int * );

#if defined(THREAD_SAFE)
static int ManageLock( AstObject *, int, int, AstObject **, int * );
#endif

/* Member functions. */
/* ================= */
static const char *Abbrev( AstFrame *this_frame, int axis, const char *fmt,
                           const char *str1, const char *str2, int *status ) {
/*
*  Name:
*     Abbrev

*  Purpose:
*     Abbreviate a formatted FrameSet axis value by skipping leading fields.

*  Type:
*     Private function.

*  Synopsis:
*     #include "frameset.h"
*     const char *Abbrev( AstFrame *this, int axis, const char *fmt,
*                         const char *str1, const char *str2, int *status )

*  Class Membership:
*     FrameSet member function (over-rides the protected astAbbrev
*     method inherited from the Frame class).

*  Description:
*     This function compares two FrameSet axis values that have been
*     formatted (using astFormat) and determines if they have any
*     redundant leading fields (i.e. leading fields in common which
*     can be suppressed when tabulating the values or plotting them on
*     the axis of a graph).

*  Parameters:
*     this
*        Pointer to the FrameSet.
*     axis
*        The number of the FrameSet axis for which the values have
*        been formatted (axis numbering starts at zero for the first
*        axis).
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
*     status
*        Pointer to the inherited status variable.

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
*     apply to the same FrameSet axis.
*     - A pointer to the start of "str2" will be returned if this
*     function is invoked with the global error status set, or if it
*     should fail for any reason.
*/

/* Local Variables: */
   AstFrame *fr;                 /* Pointer to current Frame */
   AstFrameSet *this;            /* Pointer to the FrameSet structure */
   const char *result;           /* Pointer value to return */

/* Check the global error status. */
   if ( !astOK ) return str2;

/* Obtain a pointer to the FrameSet structure. */
   this = (AstFrameSet *) this_frame;

/* Validate the axis index. */
   (void) astValidateAxis( this, axis, 1, "astAbbrev" );

/* Obtain a pointer to the FrameSet's current Frame and invoke this
   Frame's astAbbrev method to perform the processing. Annul the Frame
   pointer afterwards. */
   fr = astGetFrame( this, AST__CURRENT );
   result = astAbbrev( fr, axis, fmt, str1, str2 );
   fr = astAnnul( fr );

/* If an error occurred, clear the result. */
   if ( !astOK ) result = str2;

/* Return the result. */
   return result;
}

static void AddFrame( AstFrameSet *this, int iframe, AstMapping *map,
                      AstFrame *frame, int *status ) {
/*
*++
*  Name:
c     astAddFrame
f     AST_ADDFRAME

*  Purpose:
*     Add a Frame to a FrameSet to define a new coordinate system.

*  Type:
*     Public virtual function.

*  Synopsis:
c     #include "frameset.h"
c     void astAddFrame( AstFrameSet *this, int iframe, AstMapping *map,
c                       AstFrame *frame )
f     CALL AST_ADDFRAME( THIS, IFRAME, MAP, FRAME, STATUS )

*  Class Membership:
*     FrameSet method.

*  Description:
c     This function adds a new Frame and an associated Mapping to a
f     This routine adds a new Frame and an associated Mapping to a
*     FrameSet so as to define a new coordinate system, derived from
*     one which already exists within the FrameSet. The new Frame then
*     becomes the FrameSet's current Frame.
*
c     This function
f     This routine
*     may also be used to merge two FrameSets, or to append extra axes
*     to every Frame in a FrameSet.

*  Parameters:
c     this
f     THIS = INTEGER (Given)
*        Pointer to the FrameSet.
c     iframe
f     IFRAME = INTEGER (Given)
*        The index of the Frame within the FrameSet which describes
*        the coordinate system upon which the new one is to be based.
*        This value should lie in the range from 1 to the number of
*        Frames already in the FrameSet (as given by its Nframe
*        attribute). As a special case, AST__ALLFRAMES may be supplied,
*        in which case the axes defined by the supplied Frame are appended
*        to every Frame in the FrameSet (see the Notes section for details).
c     map
f     MAP = INTEGER (Given)
*        Pointer to a Mapping which describes how to convert
*        coordinates from the old coordinate system (described by the
c        Frame with index "iframe") into coordinates in the new
f        Frame with index IFRAME) into coordinates in the new
*        system. The Mapping's forward transformation should perform
*        this conversion, and its inverse transformation should
*        convert in the opposite direction. The supplied Mapping is ignored
c        if parameter "iframe"is equal to AST__ALLFRAMES.
f        if parameter IFRAME is equal to AST__ALLFRAMES.
c     frame
f     FRAME = INTEGER (Given)
*        Pointer to a Frame that describes the new coordinate system.
*        Any class of Frame may be supplied (including Regions and
*        FrameSets).
*
c        This function may also be used to merge two FrameSets by
c        supplying a pointer to a second FrameSet for this parameter
f        This routine may also be used to merge two FrameSets by
f        supplying a pointer to a second FrameSet for this argument
*        (see the Notes section for details).
f     STATUS = INTEGER (Given and Returned)
f        The global status.

*  Notes:
*     - A value of AST__BASE or AST__CURRENT may be given for the
c     "iframe" parameter to specify the base Frame or the current
f     IFRAME argument to specify the base Frame or the current
*     Frame respectively.
c     - This function sets the value of the Current attribute for the
f     - This routine sets the value of the Current attribute for the
*     FrameSet so that the new Frame subsequently becomes the current
*     Frame.
*     - The number of input coordinate values accepted by the supplied
*     Mapping (its Nin attribute) must match the number of axes in the
c     Frame identified by the "iframe" parameter. Similarly, the
f     Frame identified by the IFRAME argument. Similarly, the
*     number of output coordinate values generated by this Mapping
*     (its Nout attribute) must match the number of axes in the new
*     Frame.
*     - As a special case, if a pointer to a FrameSet is given for the
c     "frame" parameter, this is treated as a request to merge a pair of
f     FRAME argument, this is treated as a request to merge a pair of
*     FrameSets.  This is done by appending all the new Frames (in the
c     "frame" FrameSet) to the original FrameSet, while preserving
f     FRAME FrameSet) to the original FrameSet, while preserving
*     their order and retaining all the inter-relationships
*     (i.e. Mappings) between them. The two sets of Frames are
*     inter-related within the merged FrameSet by using the Mapping
*     supplied. This should convert between the Frame identified by
c     the "iframe" parameter (in the original FrameSet) and the current
c     Frame of the "frame" FrameSet. This latter Frame becomes the
f     the IFRAME argument (in the original FrameSet) and the current
f     Frame of the FRAME FrameSet. This latter Frame becomes the
*     current Frame in the merged FrameSet.
*     - As another special case, if a value of AST__ALLFRAMES is supplied
*     for parameter
c     "iframe",
f     IFRAME,
*     then the supplied Mapping is ignored, and the axes defined by the
*     supplied Frame are appended to each Frame in the FrameSet. In detail,
*     each Frame in the FrameSet is replaced by a CmpFrame containing the
*     original Frame and the Frame specified by parameter
c     "frame".
f     FRAME.
*     In addition, each Mapping in the FrameSet is replaced by a CmpMap
*     containing the original Mapping and a UnitMap in parallel. The Nin and
*     Nout attributes of the UnitMap are set equal to the number of axes
*     in the supplied Frame. Each new CmpMap is simplified using
c     astSimplify
f     AST_SIMPLIFY
*     before being stored in the FrameSet.



*--
*/

/* Local Variables: */
   AstFrame *fr;                 /* Pointer to Frame identified by "iframe" */
   AstFrameSet *frameset;        /* Pointer to new FrameSet (if given) */
   AstMapping *inode_map;        /* Temporarily saved Mapping pointer */
   AstMapping *next_map;         /* Temporarily saved Mapping pointer */
   int current;                  /* Current Frame index in merged FrameSet */
   int current_node;             /* Node number for current Frame */
   int ifr;                      /* Loop counter for Frames */
   int inode;                    /* Loop counter for nodes */
   int inode_invert;             /* Temporarily saved invert flag value */
   int inode_link;               /* Temporarily saved link value */
   int naxes;                    /* Number of Frame axes */
   int ncoord;                   /* Number of Mapping coordinates per point */
   int next;                     /* Number of next node in path */
   int next_invert;              /* Temporarily saved invert flag value */
   int next_link;                /* Temporarily saved link value */
   int nframe;                   /* Number of Frames in merged FrameSet */
   int nnode;                    /* Number of nodes in merged FrameSet */
   int node_zero;                /* Location of "node zero" after merging */

/* Check the global error status. */
   if ( !astOK ) return;

/* First handle cases where we are appending axes to the existing
   Frames in a FrameSet. */
   if( iframe == AST__ALLFRAMES ) {
      AppendAxes( this, frame, status );
      return;
   }

/* Now handle cases where we are adding a new Frame into the FrameSet.
   Initialise variables to avoid "used of uninitialised variable"
   messages from dumb compilers. */
   inode_map = NULL;
   next_map = NULL;
   inode_invert = 0;
   next = 0;
   next_invert = 0;
   next_link = 0;

/* Validate and translate the Frame index supplied. */
   iframe = astValidateFrameIndex( this, iframe, "astAddFrame" );

/* Obtain a pointer to the Frame from which the new coordinate system
   will be derived and determine how many axes it has. Annul the Frame
   pointer afterwards. */
   if ( astOK ) {
      fr = astGetFrame( this, iframe );
      naxes = astGetNaxes( fr );
      fr = astAnnul( fr );

/* Obtain the number of input coordinate values per point for the
   Mapping supplied and check that this matches the number of axes
   obtained above. Report an error if it does not. */
      ncoord = astGetNin( map );
      if ( astOK && ( naxes != ncoord ) ) {
         astError( AST__NCPIN, "astAddFrame(%s): Bad number of %s input "
                   "coordinate values (%d).", status, astGetClass( this ),
                   astGetClass( map ), ncoord );
         astError( AST__NCPIN, "The %s given should accept %d coordinate "
                   "value%s for each input point.", status, astGetClass( map ), naxes,
                   ( naxes == 1 ) ? "" : "s" );
      }
   }

/* Similarly, obtain the number of output coordinate values per point
   for the Mapping and check that this equals the number of axes for
   the Frame supplied. Report an error if necessary. */
   if ( astOK ) {
      ncoord = astGetNout( map );
      naxes = astGetNaxes( frame );
      if ( astOK && ( ncoord != naxes ) ) {
         astError( AST__NCPIN, "astAddFrame(%s): Bad number of %s output "
                   "coordinate values (%d).", status, astGetClass( this ),
                   astGetClass( map ), ncoord );
         astError( AST__NCPIN, "The %s given should generate %d "
                   "coordinate value%s for each output point.", status,
                   astGetClass( map ), naxes, ( naxes == 1 ) ? "" : "s" );
      }
   }

/* Normal Frame supplied. */
/* ====================== */
/* Check that the Frame supplied is not a FrameSet (handling a
   FrameSet is a special case which is addressed later). */
   if ( !astIsAFrameSet( frame ) && astOK ) {

/* Increase the size of the FrameSet's arrays to accommodate one new
   Frame. */
      this->frame = astGrow( this->frame, this->nframe + 1,
                             sizeof( AstFrame * ) );
      this->node = astGrow( this->node, this->nframe + 1, sizeof( int ) );
      this->map = astGrow( this->map, this->nnode, sizeof( AstMapping * ) );
      this->link = astGrow( this->link, this->nnode, sizeof( int ) );
      this->invert = astGrow( this->invert, this->nnode, sizeof( int ) );
      if ( astOK ) {

/* Clone pointers to the Frame and Mapping supplied and store these pointers
   in the FrameSet arrays. */
         this->frame[ this->nframe ] = astClone( frame );
         this->map[ this->nnode - 1 ] = astClone( map );

/* Associate the Frame with the Mapping via the "node" array. */
         this->node[ this->nframe ] = this->nnode;

/* Add a "link" value which identifies the node from which the new
   node is derived and store the current value of the Invert attribute
   for the Mapping. */
         this->link[ this->nnode - 1 ] = this->node[ iframe - 1 ];
         this->invert[ this->nnode - 1 ] = astGetInvert( map );

/* If successful, increment the FrameSet's Frame and node counts and
   set the Current attribute so that the new Frame becomes the current
   Frame. */
         if ( astOK ) {
            this->nframe++;
            this->nnode++;
            astSetCurrent( this, this->nframe );

/* If an error occurred while filling the FrameSet's arrays, clear any values
   that may have been added, annulling any cloned pointers. */
         } else {
            this->frame[ this->nframe ] =
                  astAnnul( this->frame[ this->nframe ] );
            this->node[ this->nframe ] = -1;
            this->map[ this->nnode - 1 ] =
                  astAnnul( this->map[ this->nnode - 1 ] );
            this->link[ this->nnode - 1 ] = -1;
	 }
      }

/* FrameSet supplied. */
/* ================== */
/* If the Frame supplied is a FrameSet, we handle this as a special
   case by merging the two FrameSets (so that the final result
   contains references to all the Frames from both FrameSets). */
   } else if ( astOK ) {

/* Obtain a pointer to the FrameSet structure containing the new Frame
   references and calculate how many Frames and nodes the combined
   FrameSet will contain. */
      frameset = (AstFrameSet *) frame;
      nframe = this->nframe + frameset->nframe;
      nnode = this->nnode + frameset->nnode;

/* Extend the original FrameSet's arrays to accommodate the new Frames
   and nodes. */
      this->frame = astGrow( this->frame, nframe, sizeof( AstFrame * ) );
      this->node = astGrow( this->node, nframe, sizeof( int ) );
      this->map = astGrow( this->map, nnode - 1, sizeof( AstMapping * ) );
      this->link = astGrow( this->link, nnode - 1, sizeof( int ) );
      this->invert = astGrow( this->invert, nnode - 1, sizeof( int ) );

/* If OK, loop to transfer the new Frame data into the new array
   elements, cloning each Frame pointer. Increment each "node" value
   to allow for the new node numbering which will apply when the new
   node data is appended to the new arrays. */
      if ( astOK ) {
         for ( ifr = 1; ifr <= frameset->nframe; ifr++ ) {
            this->frame[ this->nframe + ifr - 1 ] =
               astClone( frameset->frame[ ifr - 1 ] );
            this->node[ this->nframe + ifr - 1 ] =
               frameset->node[ ifr - 1 ] + this->nnode;
         }

/* Similarly, transfer the new node data, cloning each Mapping
   pointer. Increment each "link" value to allow for the new node
   numbering. */
         for ( inode = 1; inode < frameset->nnode; inode++ ) {
            this->map[ this->nnode + inode - 1 ] =
               astClone( frameset->map[ inode - 1 ] );
            this->link[ this->nnode + inode - 1 ] =
               frameset->link[ inode - 1 ] + this->nnode;
            this->invert[ this->nnode + inode - 1 ] =
               frameset->invert[ inode - 1 ];
         }

/* In transferring the node data (above), we left an empty array
   element which will later be filled with data corresponding to node
   zero in the new FrameSet (there are no data to be copied for this
   node). Initialise the data for this element to null values. */
         this->map[ this->nnode - 1 ] = NULL;
         this->link[ this->nnode - 1 ] = -1;
         this->invert[ this->nnode - 1 ] = -1;

/* Determine which is the current Frame in the new FrameSet and
   convert this into the corresponding Frame number in the combined
   one. */
         current = astGetCurrent( frameset ) + this->nframe;

/* We must now form a new link between this Frame and Frame "iframe"
   (using the Mapping supplied) in order to inter-relate the Frames
   from the two FrameSets. However, this cannot be done immediately
   because in general the node corresponding to Frame "current" will
   already have a link pointing to another node. Moreover, the node
   which was originally node zero (in the new FrameSet) still has
   no data in our merged FrameSet.

   To overcome this, we must re-structure the links within the
   transferred data.  We do this by starting at the node corresponding
   to Frame "current" and working back through each link until the
   original node zero is reached. At each step along this path, we
   reverse the direction of the link. This involves shifting the
   associated data by one step along the path, so that it becomes
   associated with the next node. This results in the final
   (initialised-to-null) node acquiring some data, and the starting
   node being left free to receive our new link.

   We compensate for reversing the links by reversing the sense of the
   "invert" flag associated with each Mapping along the path, so that
   the overall structure of the FrameSet is unchanged. */

/* Identify the starting node (the one corresponding to Frame
   "current"). */
         if ( astOK ) {
            current_node = this->node[ current - 1 ];

/* Obtain the value which a "link" element will now have if it
   originally identified node zero in the new FrameSet. We will use
   this value to detect the end of the path. */
            node_zero = this->nnode;

/* If we are not already at "node zero", save the data for the current
   node. */
            if ( current_node != node_zero ) {
               inode_map = this->map[ current_node - 1 ];
               inode_link = this->link[ current_node - 1 ];
               inode_invert = this->invert[ current_node - 1 ];

/* Reset the node's data to null values (pending setting up the new
   link using the Mapping supplied). */
               this->map[ current_node - 1 ] = NULL;
               this->link[ current_node - 1 ] = -1;
               this->invert[ current_node - 1 ] = -1;

/* Identify the next node in the path. */
               next = inode_link;
            }

/* Follow the path until "node zero" is reached. */
            inode = current_node;
            while( inode != node_zero ) {

/* If the next node on the path is not "node zero", save its data
   (because we are about to write over it). */
               if ( next != node_zero ) {
                  next_map = this->map[ next - 1 ];
                  next_link = this->link[ next - 1 ];
                  next_invert = this->invert[ next - 1 ];
               }

/* Reverse the link from the current node to the "next" node. This
   involves transferring the "map" and "invert" values to the "next"
   node and inverting the sense of the latter to compensate. Make the
   "next" node point back to the current one. */
               this->map[ next - 1 ] = inode_map;
               this->link[ next - 1 ] = inode;
               this->invert[ next - 1 ] = !inode_invert;

/* Move on to consider the next node. */
               inode = next;

/* If we have not reached "node zero" yet, transfer the node data we
   saved above into the variables from which it will be transferred to
   the following node on the next pass through this loop. */
               if ( inode != node_zero ) {
                  inode_map = next_map;
                  inode_link = next_link;
                  inode_invert = next_invert;

/* Identify the node that follows the next one. */
                  next = inode_link;
               }
            }

/* Once the necessary links have been re-structured, establish the new
   link that inter-relates the Frames from the two FrameSets. */
            this->map[ current_node - 1 ] = astClone( map );
            this->link[ current_node - 1 ] = this->node[ iframe - 1 ];
            this->invert[ current_node - 1 ] = astGetInvert( map );
         }

/* If successful, update the Frame and node counts and make the
   appropriate Frame current. */
         if ( astOK ) {
            this->nframe = nframe;
            this->nnode = nnode;
            astSetCurrent( this, current );

/* If an error occurred, loop through all the new Frame and node array
   elements and clear them, ensuring that any remaining Object
   pointers are annulled. */
         } else {
            for ( ifr = 1; ifr <= frameset->nframe; ifr++ ) {
               this->frame[ this->nframe + ifr - 1 ] =
                  astAnnul( this->frame[ this->nframe + ifr - 1 ] );
               this->node[ this->nframe + ifr - 1 ] = -1;
            }
            for ( inode = 0; inode < frameset->nnode; inode++ ) {
               this->map[ this->nnode + inode - 1 ] =
                  astAnnul( this->map[ this->nnode + inode - 1 ] );
               this->link[ this->nnode + inode - 1 ] = -1;
               this->invert[ this->nnode + inode - 1 ] = -1;
            }
         }
      }
   }
}

static double Angle( AstFrame *this_frame, const double a[],
                     const double b[], const double c[], int *status ) {
/*
*  Name:
*     Angle

*  Purpose:
*     Calculate the angle subtended by two points at a third point.

*  Type:
*     Private function.

*  Synopsis:
*     #include "frameset.h"
*     double Angle( AstFrame *this, const double a[], const double b[],
*                   const double c[], int *status )

*  Class Membership:
*     FrameSet member function (over-rides the protected astAngle
*     method inherited from the Frame class).

*  Description:
*     This function finds the angle at point B between the line joining points
*     A and B, and the line joining points C and B. These lines will in fact be
*     geodesic curves appropriate to the Frame in use. For instance, in
*     SkyFrame, they will be great circles.

*  Parameters:
*     this
*        Pointer to the Frame.
*     a
*        An array of double, with one element for each Frame axis
*        (Naxes attribute) containing the coordinates of the first point.
*     b
*        An array of double, with one element for each Frame axis
*        (Naxes attribute) containing the coordinates of the second point.
*     c
*        An array of double, with one element for each Frame axis
*        (Naxes attribute) containing the coordinates of the third point.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     astAngle
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
*     invoked with the AST error status set, or if it should fail for
*     any reason.
*/

/* Local Variables: */
   AstFrame *fr;                 /* Pointer to current Frame */
   AstFrameSet *this;            /* Pointer to the FrameSet structure */
   double result;                /* Value to return */

/* Check the global error status. */
   if ( !astOK ) return AST__BAD;

/* Obtain a pointer to the FrameSet structure. */
   this = (AstFrameSet *) this_frame;

/* Obtain a pointer to the FrameSet's current Frame and invoke this
   Frame's astAngle method. Annul the Frame pointer afterwards. */
   fr = astGetFrame( this, AST__CURRENT );
   result = astAngle( fr, a, b, c );
   fr = astAnnul( fr );

/* If an error occurred, clear the result. */
   if ( !astOK ) result = AST__BAD;

/* Return the result. */
   return result;
}

static void AppendAxes( AstFrameSet *this, AstFrame *frame, int *status ) {
/*
*  Name:
*     AppendAxes

*  Purpose:
*     Append axes to every Frame in a FrameSet.

*  Type:
*     Private function.

*  Synopsis:
*     #include "frameset.h"
*     void AppendAxes( AstFrameSet *this, AstFrame *frame, int *status )

*  Class Membership:
*     FrameSet member function

*  Description:
*     This function replaces every Frame in the FrameSet with a CmpFrame
*     holding the original Frame and the supplied Frame. It also replaces
*     every Mapping in the FrameSet with a parallel CmpMap holding the
*     original Mapping and a UnitMap. The Nin and Nout attributes of every
*     UnitMap are equal to the number of axes in the supplied Frame. Each
*     CmpMap is simplified before being stored in the FrameSet.


*  Parameters:
*     this
*        Pointer to the Frame.
*     frame
*        Pointer to a Frame holding the new axes to add to every Frame in
*        the FrameSet.
*     status
*        Pointer to the inherited status variable.

*/

/* Local Variables: */
   AstCmpFrame *frm;             /* Pointer to new Frame */
   AstCmpMap *map;               /* UnitMap to new Mapping */
   AstUnitMap *umap;             /* UnitMap to feed the new axes */
   int iframe;                   /* Frame index */
   int imap;                     /* Mapping index */

/* Check the global error status. */
   if ( !astOK ) return;

/* Loop round every Frame in the FrameSet. */
   for ( iframe = 0; iframe < this->nframe; iframe++ ) {

/* Create a CmpFrame holding the original Frame and the new Frame. */
      frm = astCmpFrame( this->frame[ iframe ], frame, " ", status );

/* Annul the original Frame pointer and store the new CmpFrame pointer. */
      (void) astAnnul( this->frame[ iframe ] );
      this->frame[ iframe ] = (AstFrame *) frm;
   }

/* Create a UnitMap with the number of inputs and outputs equal to the
   number of axes in the supplied Frame. */
   umap = astUnitMap( astGetNaxes( frame ), " ", status );

/* Loop round every Mapping in the FrameSet. */
   for ( imap = 0; imap < this->nnode - 1; imap++ ) {

/*  Crate a parallel CmpMap holding the original Mapping and the UnitMap. */
      map = astCmpMap( this->map[ imap ], umap, 0, " ", status );

/* Annul the original Mapping pointer. */
      (void) astAnnul( this->map[ imap ] );

/* Simplify the new Mapping, and store it in the FrameSet. */
      this->map[ imap ] = astSimplify( map );

/* Annul the un-simplified Mapping pointer. */
      map = astAnnul( map );
   }

/* Annul the UnitMap pointer. */
   umap = astAnnul( umap );
}

static double AxAngle( AstFrame *this_frame, const double a[], const double b[], int axis, int *status ) {
/*
*  Name:
*     AxAngle

*  Purpose:
*     Returns the angle from an axis, to a line through two points.

*  Type:
*     Private function.

*  Synopsis:
*     #include "frameset.h"
*     double AxAngle( AstFrame *this, const double a[], const double b[], int axis, int *status )

*  Class Membership:
*     FrameSet member function (over-rides the protected astAxAngle
*     method inherited from the Frame class).

*  Description:
*     This function finds the angle, as seen from point A, between the positive
*     direction of a specified axis, and the geodesic curve joining point
*     A to point B.

*  Parameters:
*     this
*        Pointer to the Frame.
*     a
*        An array of double, with one element for each Frame axis
*        (Naxes attribute) containing the coordinates of the first point.
*     b
*        An array of double, with one element for each Frame axis
*        (Naxes attribute) containing the coordinates of the second point.
*     axis
*        The number of the Frame axis from which the angle is to be
*        measured (one-based).
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*        The angle in radians, from the positive direction of the
*        specified axis, to the line AB. If the Frame is 2-dimensional,
*        it will be in the range $\pm \pi$, and positive rotation is in
*        the same sense as rotation from the positive direction of axis 2
*        to the positive direction of axis 1. If the Frame has more than 2
*        axes, a positive value will always be returned in the range zero
*        to $\pi$.

*  Notes:
*     - The geodesic curve used by this function is the path of
*     shortest distance between two points, as defined by the
*     astDistance function.
*     - This function will return "bad" coordinate values (AST__BAD)
*     if any of the input coordinates has this value, or if the require
*     position angle is undefined.
*/

/* Local Variables: */
   AstFrame *fr;                 /* Pointer to current Frame */
   AstFrameSet *this;            /* Pointer to the FrameSet structure */
   double result;                /* Value to return */

/* Check the global error status. */
   if ( !astOK ) return AST__BAD;

/* Obtain a pointer to the FrameSet structure. */
   this = (AstFrameSet *) this_frame;

/* Validate the axis index. */
   (void) astValidateAxis( this, axis - 1, 1, "astAxAngle" );

/* Obtain a pointer to the FrameSet's current Frame and invoke the
   astAxAngle method for this Frame. Annul the Frame pointer
   afterwards. */
   fr = astGetFrame( this, AST__CURRENT );
   result = astAxAngle( fr, a, b, axis );
   fr = astAnnul( fr );

/* If an error occurred, clear the result value. */
   if ( !astOK ) result = AST__BAD;

/* Return the result. */
   return result;
}

static double AxDistance( AstFrame *this_frame, int axis, double v1, double v2, int *status ) {
/*
*  Name:
*     AxDistance

*  Purpose:
*     Find the distance between two axis values.

*  Type:
*     Private function.

*  Synopsis:
*     #include "frameset.h"
*     double AxDistance( AstFrame *this, int axis, double v1, double v2, int *status )

*  Class Membership:
*     FrameSet member function (over-rides the protected astAxDistance
*     method inherited from the Frame class).

*  Description:
*     This function returns a signed value representing the axis increment
*     from axis value v1 to axis value v2.
*
*     For a simple Frame, this is a trivial operation returning the
*     difference between the two axis values. But for other derived classes
*     of Frame (such as a SkyFrame) this is not the case.

*  Parameters:
*     this
*        Pointer to the Frame.
*     axis
*        The index of the axis to which the supplied values refer. The
*        first axis has index 1.
*     v1
*        The first axis value.
*     v2
*        The second axis value.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     The distance between the two axis values.

*  Notes:
*     - This function will return a "bad" result value (AST__BAD) if
*     any of the input vaues has this value.
*     - A "bad" value will also be returned if this function is
*     invoked with the AST error status set, or if it should fail for
*     any reason.
*/

/* Local Variables: */
   AstFrame *fr;                 /* Pointer to current Frame */
   AstFrameSet *this;            /* Pointer to the FrameSet structure */
   double result;                /* Value to return */

/* Check the global error status. */
   if ( !astOK ) return AST__BAD;

/* Obtain a pointer to the FrameSet structure. */
   this = (AstFrameSet *) this_frame;

/* Validate the axis index. */
   (void) astValidateAxis( this, axis - 1, 1, "astAxDistance" );

/* Obtain a pointer to the FrameSet's current Frame and invoke the
   astAxDistance method for this Frame. Annul the Frame pointer
   afterwards. */
   fr = astGetFrame( this, AST__CURRENT );
   result = astAxDistance( fr, axis, v1, v2 );
   fr = astAnnul( fr );

/* If an error occurred, clear the result value. */
   if ( !astOK ) result = AST__BAD;

/* Return the result. */
   return result;
}

static double AxOffset( AstFrame *this_frame, int axis, double v1, double dist, int *status ) {
/*
*  Name:
*     AxOffset

*  Purpose:
*     Add an increment onto a supplied axis value.

*  Type:
*     Private function.

*  Synopsis:
*     #include "frameset.h"
*     double AxOffset( AstFrame *this, int axis, double v1, double dist, int *status )

*  Class Membership:
*     FrameSet member function (over-rides the protected astAxOffset
*     method inherited from the Frame class).

*  Description:
*     This function returns an axis value formed by adding a signed axis
*     increment onto a supplied axis value.
*
*     For a simple Frame, this is a trivial operation returning the
*     sum of the two supplied values. But for other derived classes
*     of Frame (such as a SkyFrame) this is not the case.

*  Parameters:
*     this
*        Pointer to the Frame.
*     axis
*        The index of the axis to which the supplied values refer. The
*        first axis has index 1.
*     v1
*        The original axis value.
*     dist
*        The axis increment to add to the original axis value.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     The incremented axis value.

*  Notes:
*     - This function will return a "bad" result value (AST__BAD) if
*     any of the input vaues has this value.
*     - A "bad" value will also be returned if this function is
*     invoked with the AST error status set, or if it should fail for
*     any reason.
*/

/* Local Variables: */
   AstFrame *fr;                 /* Pointer to current Frame */
   AstFrameSet *this;            /* Pointer to the FrameSet structure */
   double result;                /* Value to return */

/* Check the global error status. */
   if ( !astOK ) return AST__BAD;

/* Obtain a pointer to the FrameSet structure. */
   this = (AstFrameSet *) this_frame;

/* Validate the axis index. */
   (void) astValidateAxis( this, axis - 1, 1, "astAxOffset" );

/* Obtain a pointer to the FrameSet's current Frame and invoke the
   astAxOffset method for this Frame. Annul the Frame pointer
   afterwards. */
   fr = astGetFrame( this, AST__CURRENT );
   result = astAxOffset( fr, axis, v1, dist );
   fr = astAnnul( fr );

/* If an error occurred, clear the result value. */
   if ( !astOK ) result = AST__BAD;

/* Return the result. */
   return result;
}

static AstObject *Cast( AstObject *this_object, AstObject *obj, int *status ) {
/*
*  Name:
*     Cast

*  Purpose:
*     Cast an Object into an instance of a sub-class.

*  Type:
*     Private function.

*  Synopsis:
*     #include "frameset.h"
*     AstObject *Cast( AstObject *this, AstObject *obj, int *status )

*  Class Membership:
*     FrameSet member function (over-rides the protected astCast
*     method inherited from the Frame class).

*  Description:
*     This function returns a deep copy of an ancestral component of the
*     supplied object. The required class of the ancestral component is
*     specified by another object. Specifically, if "this" and "new" are
*     of the same class, a copy of "this" is returned. If "this" is an
*     instance of a subclass of "obj", then a copy of the component
*     of "this" that matches the class of "obj" is returned. Otherwise,
*     a NULL pointer is returned without error.

*  Parameters:
*     this
*        Pointer to the Object to be cast.
*     obj
*        Pointer to an Object that defines the class of the returned Object.
*        The returned Object will be of the same class as "obj".

*  Returned Value:
*     A pointer to the new Object. NULL if "this" is not a sub-class of
*     "obj", or if an error occurs.

*  Notes:
*     - A NULL pointer will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*/

/* Local Variables; */
   AstFrame *cfrm;
   AstObject *new;
   astDECLARE_GLOBALS
   int generation_gap;

/* Initialise */
   new = NULL;

/* Check inherited status */
   if( !astOK ) return new;

/* Get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(NULL);

/* See how many steps up the class inheritance ladder it is from "obj"
   to this class (FrameSet). A positive value is returned if FrameSet
   is a sub-class of "obj". A negative value is returned if "obj" is
   a sub-class of FrameSet. Zero is returned if "obj" is a FrameSet.
   AST__COUSIN is returned if "obj" is not on the same line of descent
   as FrameSet. */
   generation_gap = astClassCompare( (AstObjectVtab *) &class_vtab,
                                     astVTAB( obj ) );

/* If "obj" is a FrameSet or a sub-class of FrameSet, we can cast by
   truncating the vtab for "this" so that it matches the vtab of "obJ",
   and then taking a deep copy of "this". */
   if( generation_gap <= 0 && generation_gap != AST__COUSIN ) {
      new = astCastCopy( this_object, obj );

/* If "obj" is not a FrameSet or a sub-class of FrameSet (e.g. a Frame or
   some sub-class of Frame), we attempt to cast the current Frame into
   the class indicated by "obj". */
   } else {
      cfrm = astGetFrame( (AstFrameSet *) this_object, AST__CURRENT );
      new = astCast( cfrm, obj );
      cfrm = astAnnul( cfrm );
   }

/* Return the new pointer. */
   return new;
}

static void CheckPerm( AstFrame *this_frame, const int *perm, const char *method, int *status ) {
/*
*  Name:
*     CheckPerm

*  Purpose:
*     Check that an array contains a valid permutation.

*  Type:
*     Private function.

*  Synopsis:
*     #include "frameset.h"
*     void CheckPerm( AstFrame *this, const int *perm, const char *method, int *status )

*  Class Membership:
*     FrameSet member function (over-rides the protected astCheckPerm
*     method inherited from the Frame class).

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
*     status
*        Pointer to the inherited status variable.

*  Notes:
*     - Error messages issued by this function refer to the external
*     (public) numbering system used for axes (which is one-based),
*     whereas zero-based axis indices are used internally.
*/

/* Local Variables: */
   AstFrame *fr;                 /* Pointer to current Frame */
   AstFrameSet *this;            /* Pointer to the FrameSet structure */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain a pointer to the FrameSet structure. */
   this = (AstFrameSet *) this_frame;

/* Obtain a pointer to the FrameSet's current Frame and invoke this
   Frame's astCheckPerm method. Annul the Frame pointer afterwards. */
   fr = astGetFrame( this, AST__CURRENT );
   astCheckPerm( fr, perm, method );
   fr = astAnnul( fr );

}

static void Clear( AstObject *this_object, const char *attrib, int *status ) {
/*
*  Name:
*     Clear

*  Purpose:
*     Clear attribute values for a FrameSet.

*  Type:
*     Private function.

*  Synopsis:
*     #include "frameset.h"
*     void Clear( AstObject *this, const char *attrib, int *status )

*  Class Membership:
*     FrameSet member function (over-rides the public astClear method
*     inherited from the Object class).

*  Description:
*     This function clears the values of a specified set of attributes
*     for a FrameSet. Clearing an attribute cancels any value that has
*     previously been explicitly set for it, so that the standard
*     default attribute value will subsequently be used instead. This
*     also causes the astTest function to return the value zero for
*     the attribute, indicating that no value has been set.

*  Parameters:
*     this
*        Pointer to the FrameSet.
*     attrib
*        Pointer to a null-terminated character string containing a
*        comma-separated list of the names of the attributes to be
*        cleared.
*     status
*        Pointer to the inherited status variable.

*  Notes:
*     - This function preserves the integrity of the FrameSet (if
*     possible) by appropriately remapping its current Frame to take
*     account of its changed attribute values.
*/

/* Local Variables: */
   astDECLARE_GLOBALS            /* Declare the thread specific global data */
   AstFrame *save_frame;         /* Saved pointer to integrity Frame */
   AstFrameSet *this;            /* Pointer to FrameSet structure */
   const char *save_method;      /* Saved pointer to method name */
   int ok;                       /* Status OK? */
   int save_lost;                /* Saved integrity modified flag */

/* Check the global error status. */
   if ( !astOK ) return;

/* Get a pointer to the structure holding thread-specific global data. */
   astGET_GLOBALS(this_object);

/* Obtain a pointer to the FrameSet structure. */
   this = (AstFrameSet *) this_object;

/* To allow this function to be invoked recursively, we first save any
   existing FrameSet integrity information in local variables. */
   save_frame = integrity_frame;
   save_lost= integrity_lost;
   save_method = integrity_method;

/* Set the name of the method being used (for use in error
   messages). */
   integrity_method = "astClear";

/* Record the initial integrity state of the FrameSet. */
   RecordIntegrity( this, status );

/* Invoke the parent astClear method to clear the FrameSet's attribute
   values and note if this succeeds. */
   (*parent_clear)( this_object, attrib, status );
   ok = astOK;

/* Restore the FrameSet's integrity. */
   RestoreIntegrity( this, status );

/* If integrity could not be restored, then add contextual error
   information. */
   if ( !astOK && ok ) {
      astError( astStatus, "Unable to accommodate clearing the \"%s\" "
                           "attribute(s).", status, attrib );
   }

/* Restore any saved FrameSet integrity information. */
   integrity_frame = save_frame;
   integrity_lost = save_lost;
   integrity_method = save_method;
}

static void ClearAttrib( AstObject *this_object, const char *attrib, int *status ) {
/*
*  Name:
*     ClearAttrib

*  Purpose:
*     Clear an attribute value for a FrameSet.

*  Type:
*     Private function.

*  Synopsis:
*     #include "frameset.h"
*     void ClearAttrib( AstObject *this, const char *attrib )

*  Class Membership:
*     FrameSet member function (over-rides the astClearAttrib protected
*     method inherited from the Frame class).

*  Description:
*     This function clears the value of a specified attribute for a
*     FrameSet, so that the default value will subsequently be used.

*  Parameters:
*     this
*        Pointer to the FrameSet.
*     attrib
*        Pointer to a null terminated string specifying the attribute
*        name.  This should be in lower case with no surrounding white
*        space.
*/

/* Local Variables: */
   astDECLARE_GLOBALS            /* Declare the thread specific global data */
   AstFrame *fr;                 /* Pointer to current Frame */
   AstFrameSet *this;            /* Pointer to the FrameSet structure */

/* Check the global error status. */
   if ( !astOK ) return;


/* Get a pointer to the structure holding thread-specific global data. */
   astGET_GLOBALS(this_object);
/* Obtain a pointer to the FrameSet structure. */
   this = (AstFrameSet *) this_object;

/* Check the attribute name and clear the appropriate attribute. */

/* We first handle attributes that apply to the FrameSet as a whole
   (rather than to the current Frame). */

/* Base. */
/* ----- */
   if ( !strcmp( attrib, "base" ) ) {
      astClearBase( this );

/* Current. */
/* -------- */
/* Since this determines the choice of current Frame, we must restore
   the integrity state of the FrameSet before changing this attribute
   and record the new integrity state afterwards. */
   } else if ( !strcmp( attrib, "current" ) ) {
      RestoreIntegrity( this, status );
      astClearCurrent( this );
      RecordIntegrity( this, status );

/* ID. */
/* --- */
   } else if ( !strcmp( attrib, "id" ) ) {
      astClearID( this );

/* Ident. */
/* ------ */
   } else if ( !strcmp( attrib, "ident" ) ) {
      astClearIdent( this );

/* Invert. */
/* ------- */
/* Since this affects the choice of current Frame, we must restore the
   integrity state of the FrameSet before changing this attribute and
   record the new integrity state afterwards. */
   } else if ( !strcmp( attrib, "invert" ) ) {
      RestoreIntegrity( this, status );
      astClearInvert( this );
      RecordIntegrity( this, status );

/* Report. */
/* ------- */
   } else if ( !strcmp( attrib, "report" ) ) {
      astClearReport( this );

/* If the name was not recognised, test if it matches any of the
   read-only attributes of this class. If it does, then report an
   error. */
   } else if ( !strcmp( attrib, "class" ) ||
               !strcmp( attrib, "nframe" ) ||
               !strcmp( attrib, "nin" ) ||
               !strcmp( attrib, "nobject" ) ||
               !strcmp( attrib, "nout" ) ||
               !strcmp( attrib, "refcount" ) ||
               !strcmp( attrib, "tranforward" ) ||
               !strcmp( attrib, "traninverse" ) ) {
      astError( AST__NOWRT, "astClear: Invalid attempt to clear the \"%s\" "
                "value for a %s.", status, attrib, astGetClass( this ) );
      astError( AST__NOWRT, "This is a read-only attribute." , status);

/* Pass unrecognised attributes on to the FrameSet's current Frame for
   further interpretation. */
   } else {

/* Force a copy to be made of the current Frame, if needed, to make it
   independent of other Frames within the FrameSet. */
      (void) ForceCopy( this, AST__CURRENT, status );

/* Obtain a pointer to the current Frame and invoke its astClearAttrib
   method. Annul the Frame pointer afterwards. */
      fr = astGetFrame( this, AST__CURRENT );
      astClearAttrib( fr, attrib );
      fr = astAnnul( fr );

/* Note that the current Frame has been modified. */
      integrity_lost = 1;
   }
}

static void ClearBase( AstFrameSet *this, int *status ) {
/*
*+
*  Name:
*     astClearBase

*  Purpose:
*     Clear the value of the Base attribute of a FrameSet.

*  Type:
*     Protected virtual function.

*  Synopsis:
*     #include "frameset.h"
*     void astClearBase( AstFrameSet *this )

*  Class Membership:
*     FrameSet method.

*  Description:
*     This function clears the value of the Base attribute of a
*     FrameSet. This value is an index that identifies the base Frame
*     for the FrameSet.

*  Parameters:
*     this
*        Pointer to the FrameSet.
*-
*/

/* Local Variables: */
   int invert;                    /* FrameSet is inverted? */

/* Check the global error status. */
   if ( !astOK ) return;

/* Determine if the FrameSet has been inverted. */
   invert = astGetInvert( this );

/* If it has not been inverted, clear the base Frame index, otherwise
   clear the current Frame index instead. */
   if ( astOK ) *( invert ? &this->current : &this->base ) = -INT_MAX;
}

static void ClearCurrent( AstFrameSet *this, int *status ) {
/*
*+
*  Name:
*     astClearCurrent

*  Purpose:
*     Clear the value of the Current attribute for a FrameSet.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "frameset.h"
*     int astClearCurrent( AstFrameSet *this )

*  Class Membership:
*     FrameSet method.

*  Description:
*     This function clears the value of the Current attribute for a
*     FrameSet. This attribute is an index that identifies the current
*     Frame for the FrameSet.

*  Parameters:
*     this
*        Pointer to the FrameSet.
*-
*/

/* Local Variables: */
   int invert;                   /* FrameSet is inverted? */

/* Check the global error status. */
   if ( !astOK ) return;

/* Determine if the FrameSet has been inverted. */
   invert = astGetInvert( this );

/* If it has not been inverted, clear the current frame index,
   otherwise clear the base Frame index instead. */
   if ( astOK ) *( invert ? &this->base : &this->current ) = -INT_MAX;
}

static AstMapping *CombineMaps( AstMapping *mapping1, int invert1,
                                AstMapping *mapping2, int invert2,
                                int series, int *status ) {
/*
*  Name:
*     CombineMaps

*  Purpose:
*     Combine two Mappings with specified Invert flags into a CmpMap.

*  Type:
*     Private function.

*  Synopsis:
*     #include "frameset.h"
*     AstMapping *CombineMaps( AstMapping *mapping1, int invert1,
*                              AstMapping *mapping2, int invert2,
*                              int series )

*  Class Membership:
*     FrameSet member function.

*  Description:
*     This function combines two Mappings into a CmpMap (compound
*     Mapping) as if their Invert flags were set to specified values
*     when the CmpMap is created. However, the individual Mappings are
*     returned with their Invert flag values unchanged from their
*     original state.

*  Parameters:
*     mapping1
*        Pointer to the first Mapping.
*     invert1
*        The (boolean) Invert flag value required for the first Mapping.
*     mapping2
*        Pointer to the second Mapping.
*     invert2
*        The (boolean) Invert flag value required for the second Mapping.
*     series
*        Whether the Mappings are to be combined in series (as opposed to
*        in parallel).

*  Returned Value:
*     A pointer to the resulting compound Mapping (a CmpMap).

*  Notes:
*     - This function is a wrap-up for the astCmpMap constructor and
*     temporarily assigns the required Invert flag values while
*     creating the required CmpMap. However, it also takes account of
*     the possibility that the two Mapping pointers supplied may point
*     at the same Mapping.
*     - A null Object pointer (AST__NULL) will be returned if this
*     function is invoked with the AST error status set, or if it
*     should fail for any reason.
*/

/* Local Variables: */
   AstMapping *map1;             /* First temporary Mapping pointer */
   AstMapping *map2;             /* Second temporary Mapping pointer */
   AstMapping *result;           /* Pointer to result Mapping */
   int copy;                     /* Copy needed? */
   int inv1;                     /* First original Invert flag value */
   int inv2;                     /* Second original Invert flag value */
   int set1;                     /* First Invert flag originally set? */
   int set2;                     /* Second Invert flag originally set? */

/* Initialise */
   result = NULL;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Limit incoming values to 0 or 1. */
   invert1 = ( invert1 != 0 );
   invert2 = ( invert2 != 0 );

/* Obtain the Invert flag values for each Mapping. */
   inv1 = astGetInvert( mapping1 );
   inv2 = astGetInvert( mapping2 );

/* Also determine if these values are explicitly set. */
   set1 = astTestInvert( mapping1 );
   set2 = astTestInvert( mapping2 );

/* If both Mappings are actually the same but we need different Invert
   flag values to be set, then this can only be achieved by making a
   copy. Note if this is necessary. */
   copy = ( ( mapping1 == mapping2 ) && ( invert1 != invert2 ) );

/* Clone the first Mapping pointer. Do likewise for the second but
   make a copy instead if necessary. */
   map1 = astClone( mapping1 );
   map2 = copy ? astCopy( mapping2 ) : astClone( mapping2 );

/* If the Invert value for the first Mapping needs changing, make the
   change. */
   if ( invert1 != inv1 ) {
      if ( invert1 ) {
         astSetInvert( map1, 1 );
      } else {
         astClearInvert( map1 );
      }
   }

/* Similarly, change the Invert flag for the second Mapping if
   necessary. */
   if ( invert2 != inv2 ) {
      if ( invert2 ) {
         astSetInvert( map2, 1 );
      } else {
         astClearInvert( map2 );
      }
   }

/* Combine the two Mappings into a CmpMap. */
   result = (AstMapping *) astCmpMap( map1, map2, series, "", status );

/* If the first Mapping's Invert value was changed, restore it to its
   original state. */
   if ( invert1 != inv1 ) {
      if ( set1 ) {
         astSetInvert( map1, inv1 );
      } else {
         astClearInvert( map1 );
      }
   }

/* Similarly, restore the second Mapping's Invert value if
   necessary. This step is not needed, however, if a copy was made. */
   if ( ( invert2 != inv2 ) && !copy ) {
      if ( set2 ) {
         astSetInvert( map2, inv2 );
      } else {
         astClearInvert( map2 );
      }
   }

/* Annul the temporary Mapping pointers. */
   map1 = astAnnul( map1 );
   map2 = astAnnul( map2 );

/* If an error occurred, then annul the result pointer. */
   if ( !astOK ) result = astAnnul( result );

/* Return the result. */
   return result;
}

static AstFrameSet *Convert( AstFrame *from, AstFrame *to,
                             const char *domainlist, int *status ) {
/*
*  Name:
*     Convert

*  Purpose:
*     Determine how to convert between two coordinate systems.

*  Type:
*     Private function.

*  Synopsis:
*     #include "frameset.h"
*     AstFrameSet *Convert( AstFrame *from, AstFrame *to,
*                           const char *domainlist, int *status )

*  Class Membership:
*     FrameSet member function (over-rides the public astConvert
*     method inherited fromm the Frame class).

*  Description:
*     This function compares two FrameSets and determines whether it
*     is possible to convert between the coordinate systems which
*     their current Frames represent. If conversion is possible, it
*     returns a FrameSet which describes the conversion and which may
*     be used (as a Mapping) to transform coordinate values in either
*     direction.
*
*     If conversion is possible, the Base attributes of both FrameSets
*     will be modified on exit to identify the Frames which were used
*     as the intermediate coordinate system.

*  Parameters:
*     from
*        Pointer to a FrameSet whose current Frame represents the
*        "source" coordinate system.  Note that the Base attribute of
*        the FrameSet may be modified by this function.
*     to
*        Pointer to a FrameSet whose current Frame represents the
*        "destination" coordinate system.  Note that the Base
*        attribute of the FrameSet may be modified by this function.
*     domainlist
*        Pointer to a null-terminated character string containing a
*        comma-separated list of Frame domains. This may be used to
*        define a priority order for the different intermediate
*        coordinate systems that might be used to perform the
*        conversion.
*
*        The function will first try to obtain a conversion by making
*        use only of intermediate Frames whose Domain attribute
*        matches the first domain in this list. If this fails, the
*        second domain in the list will be used, and so on, until
*        conversion is achieved. A blank domain (e.g. two consecutive
*        commas) indicates that all Frames should be considered,
*        regardless of their Domain attributes. The list is
*        case-insensitive and all white space is ignored.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*        If the requested coordinate conversion is possible, the
*        function returns a pointer to a FrameSet which describes the
*        conversion. Otherwise, a null Object pointer (AST__NULL) is
*        returned without error.
*
*        If a FrameSet is returned, it will contain two Frames. Frame
*        number 1 (its base Frame) will describe the source coordinate
*        system, corresponding to the "from" parameter. Frame number 2
*        (its current Frame) will describe the destination coordinate
*        system, corresponding to the "to" parameter. The Mapping
*        which inter-relates these Frames will perform the required
*        conversion between the two coordinate systems.

*  Notes:
*     - Either of the "from" and "to" pointers may identify a basic
*     Frame instead of a FrameSet, in which case the function behaves
*     as if it were a FrameSet containing only a single Frame.
*     - A NULL pointer will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.

* Implementation Notes:
*    - This function is simply a wrap-up for the ConvertX function
*    which performs the required processing but swaps the order of the
*    first two arguments. This is a trick to allow the astConvert
*    method to be over-ridden by derived classes on the basis of the
*    class of either of the first two arguments.
*/

/* Check the inherited status. */
   if ( !astOK ) return NULL;

/* Invoke the private "ConvertX" member function with the first two
   arguments swapped. */
   return ConvertX( to, from, domainlist, status );
}

static AstFrameSet *ConvertX( AstFrame *to, AstFrame *from,
                              const char *domainlist, int *status ) {
/*
*  Name:
*     ConvertX

*  Purpose:
*     Determine how to convert between two coordinate systems.

*  Type:
*     Private function.

*  Synopsis:
*     #include "frameset.h"
*     AstFrameSet *ConvertX( AstFrame *to, AstFrame *from,
*                            const char *domainlist )

*  Class Membership:
*     FrameSet member function (over-rides the protected "astConvertX"
*     method inherited from the Frame class).

*  Description:
*     This function performs the processing for the public astConvert
*     method (as inherited from the Frame class and over-ridden by the
*     FrameSet class) and has exactly the same interface except that
*     the order of the first two arguments is swapped. This is a trick
*     to allow the astConvert method to be over-ridden by derived
*     classes on the basis of the class of either of its first two
*     arguments.
*
*     See the astConvert method for details of the interface.
*/

/* Local Variables: */
   AstFrame *from_frame;         /* Pointer to "from" Frame */
   AstFrame *to_frame;           /* Pointer to "to" Frame */
   AstFrameSet *cvt;             /* Pointer to conversion FrameSet */
   AstFrameSet *result;          /* Pointer to FrameSet to be returned */
   AstMapping *from_map;         /* Pointer to "from" Mapping */
   AstMapping *map;              /* Pointer to conversion Mapping */
   AstMapping *result_map;       /* Pointer to result Mapping */
   AstMapping *tmp;              /* Temporary Mapping pointer */
   AstMapping *to_map;           /* Pointer to "to" Mapping */
   char *domain;                 /* Pointer to individual domain string */
   char *domain_end;             /* Pointer to final null of domain string */
   char *domainlist_copy;        /* Pointer to copy of domains list */
   int *from_order;              /* List of Frame indices in search order */
   int *to_order;                /* List of Frame indices in search order */
   int best_score;               /* Score from best match */
   int from_base;                /* Index of "from" base Frame */
   int from_current;             /* Index of "from" current Frame */
   int from_index;               /* Index of "from" Frame */
   int from_isframe;             /* "from" is a Frame (not a FrameSet)? */
   int from_nframe;              /* Number of "from" Frames */
   int from_number;              /* Loop counter for "from" Frames */
   int iframe_from;              /* Index of best "from" Frame */
   int iframe_to;                /* Index of best "to" Frame */
   int match;                    /* Possible match found? */
   int n;                        /* Count of Frames */
   int perfect;                  /* Perfect match found? */
   int score;                    /* Score from latest match */
   int to_base;                  /* Index of "to" base Frame */
   int to_current;               /* Index of "to" current Frame */
   int to_index;                 /* Index of "to" Frame */
   int to_isframe;               /* "to" is a Frame (not a FrameSet)? */
   int to_nframe;                /* Number of "to" Frames */
   int to_number;                /* Loop counter for "to" Frames */

/* Initialise. */
   result = NULL;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Initialise variables to avoid "used of uninitialised variable"
   messages from dumb compilers. */
   result_map = NULL;
   iframe_from = 0;
   iframe_to = 0;

/* Determine the number of Frames in "from" and the indices of its
   base and current Frames. Use values of 1 if "from" is a Frame and
   not a FrameSet. */
   from_isframe = !astIsAFrameSet( from );
   from_nframe = from_isframe ? 1 : astGetNframe( from );
   from_base = from_isframe ? 1 : astGetBase( from );
   from_current = from_isframe ? 1 : astGetCurrent( from );

/* Obtain similar values for "to". */
   to_isframe = !astIsAFrameSet( to );
   to_nframe = to_isframe ? 1 : astGetNframe( to );
   to_base = to_isframe ? 1 : astGetBase( to );
   to_current = to_isframe ? 1 : astGetCurrent( to );

/* Allocate memory for arrays which will hold the indices of "from"
   and "to" Frames. */
   from_order = astMalloc( sizeof( int ) * (size_t) from_nframe );
   to_order = astMalloc( sizeof( int ) * (size_t) to_nframe );

/* Make a temporary copy of the domains list. */
   domainlist_copy = astStore( NULL, domainlist,
                               strlen( domainlist ) + (size_t) 1 );
   if ( astOK ) {

/* Fill the "from_order" array with the indices of all the Frames in
   "from", in the order in which they will be used for searching. Use
   the base Frame first. */
      n = 0;
      from_order[ n++ ] = from_base;

/* Then add all the "from" Frames in the appropriate order, omitting
   the base and current Frames. */
      if ( !astGetInvert( from ) ) {
         for ( from_index = 1; from_index <= from_nframe; from_index++ ) {
            if ( ( from_index != from_base ) &&
                 ( from_index != from_current ) ) {
               from_order[ n++ ] = from_index;
            }
         }
      } else {
         for ( from_index = from_nframe; from_index >= 1; from_index-- ) {
            if ( ( from_index != from_base ) &&
                 ( from_index != from_current ) ) {
               from_order[ n++ ] = from_index;
            }
         }
      }

/* Finish with the current Frame, if different from the base Frame. */
      if ( from_current != from_base ) from_order[ n++ ] = from_current;

/* Repeat this process for the "to" Frame. */
      n = 0;
      to_order[ n++ ] = to_base;
      if ( !astGetInvert( to ) ) {
         for ( to_index = 1; to_index <= to_nframe; to_index++ ) {
            if ( ( to_index != to_base ) && ( to_index != to_current ) ) {
               to_order[ n++ ] = to_index;
            }
         }
      } else {
         for ( to_index = to_nframe; to_index >= 1; to_index-- ) {
            if ( ( to_index != to_base ) && ( to_index != to_current ) ) {
               to_order[ n++ ] = to_index;
            }
         }
      }
      if ( to_current != to_base ) to_order[ n++ ] = to_current;

/* Loop to inspect each comma-separated field in the domains list
   until an error occurs, all the domains are used up, or a match is
   found. */
      domain = domainlist_copy;
      match = 0;
      while ( astOK && domain && !match ) {

/* Change the comma at the end of each field to a null to terminate
   the domain. */
         if ( ( domain_end = strchr( domain, ',' ) ) ) *domain_end = '\0';

/* For any given domain, we will ignore imperfect matches in favour of
   better ones by assigning a score to each match. Initialise the best
   score value for the current domain. */
         best_score = -1;

/* Loop through each Frame in "to". Quit looping early if an error
   occurs or a perfect match is found. */
         perfect = 0;
         for ( to_number = 0;
               astOK && !perfect && ( to_number < to_nframe );
               to_number++ ) {

/* Permute the "to" Frame number into a Frame index to implement the
   required search order, and obtain a pointer to the required "to"
   Frame. */
            to_index = to_order[ to_number ];
            to_frame = to_isframe ? astClone( to ) :
                                    astGetFrame( to, to_index );

/* Loop through each Frame in "from". Quit looping early if an error
   occurs or a perfect match is found. */
            for ( from_number = 0;
                  astOK && !perfect && ( from_number < from_nframe );
                  from_number++ ) {

/* Permute the "from" Frame number into a Frame index to implement the
   required search order, and obtain a pointer to the required "from"
   Frame. */
               from_index = from_order[ from_number ];
               from_frame = from_isframe ? astClone( from ) :
                                           astGetFrame( from, from_index );

/* Attempt to obtain a FrameSet which describes the conversion between
   the selected "from" and "to" Frames and test if successful. If so,
   we have a potential route to construct the overall Mapping we
   want. */
               cvt = astConvert( from_frame, to_frame, domain );
               if ( astOK && cvt ) {

/* Extract the required Mapping from the returned FrameSet. */
                  map = astGetMapping( cvt, AST__BASE, AST__CURRENT );

/* If necessary, prefix the Mapping between the "from" current Frame
   and the individual "from" Frame we have selected. */
                  if ( from_index != from_current ) {
                     from_map = astGetMapping( from, AST__CURRENT,
                                               from_index );
                     tmp = (AstMapping *) astCmpMap( from_map, map, 1, "", status );
                     from_map = astAnnul( from_map );
                     map = astAnnul( map );
                     map = tmp;
                  }

/* Similarly, if necessary, append the Mapping between the selected
   "to" Frame and the "to" current Frame. */
                  if ( to_index != to_current ) {
                     to_map = astGetMapping( to, to_index, AST__CURRENT );
                     tmp = (AstMapping *) astCmpMap( map, to_map, 1, "", status );
                     to_map = astAnnul( to_map );
                     map = astAnnul( map );
                     map = tmp;
                  }

/* Simplify the resulting overall Mapping (this is done here because
   it may sometimes affect the attribute values used to assign a score
   below). */
                  tmp = astSimplify( map );
                  map = astAnnul( map );
                  map = tmp;

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

/* Note which "from" and "to" Frames were used. */
                     iframe_from = from_index;
                     iframe_to = to_index;
                  }

/* Annul pointers to the intermediate Objects. */
                  map = astAnnul( map );
                  cvt = astAnnul( cvt );
               }
               from_frame = astAnnul( from_frame );
            }
            to_frame = astAnnul( to_frame );
         }

/* Go on to consider the next field in the domains list. */
         domain = domain_end ? domain_end + 1 : NULL;
      }
   }

/* Free the memory allocated for temporary arrays. */
   domainlist_copy = astFree( domainlist_copy );
   from_order = astFree( from_order );
   to_order = astFree( to_order );

/* If a result is being returned, then obtain a pointer to the current
   "from" Frame and use it to start constructing the result
   FrameSet. */
   if ( result_map ) {
      from_frame = from_isframe ? astClone( from ) :
                                  astGetFrame( from, AST__CURRENT );
      result = astFrameSet( from_frame, "", status );
      from_frame = astAnnul( from_frame );

/* Similarly. obtain a pointer to the current "to" frame and add it to
   the result FrameSet (related to the base Frame by the result
   Mapping). */
      to_frame = to_isframe ? astClone( to ) :
                              astGetFrame( to, AST__CURRENT );
      astAddFrame( result, AST__BASE, result_map, to_frame );
      to_frame = astAnnul( to_frame );

/* Annul the result Mapping pointer. */
      result_map = astAnnul( result_map );
   }

/* If successful, and a FrameSet is being returned, then set the base
   Frames of "from" and "to" (if they are FrameSets) to indicate the
   route used to generate the result Mapping. */
   if ( astOK && result ) {
      if ( !from_isframe ) astSetBase( from, iframe_from );
      if ( !to_isframe ) astSetBase( to, iframe_to );
   }

/* If an error occurred, annul the returned FrameSet pointer. */
   if ( !astOK && result ) result = astAnnul( result );

/* Return the result. */
   return result;
}

static double Distance( AstFrame *this_frame,
                        const double point1[], const double point2[], int *status ) {
/*
*  Name:
*     Distance

*  Purpose:
*     Calculate the distance between two points.

*  Type:
*     Private function.

*  Synopsis:
*     #include "frameset.h"
*     double Distance( AstFrame *this,
*                      const double point1[], const double point2[], int *status )

*  Class Membership:
*     FrameSet member function (over-rides the protected astDistance
*     method inherited from the Frame class).

*  Description:
*     This function finds the distance between two points whose
*     FrameSet coordinates are given. The distance calculated is that
*     along the geodesic curve that joins the two points.

*  Parameters:
*     this
*        Pointer to the FrameSet.
*     point1
*        An array of double, with one element for each FrameSet axis
*        containing the coordinates of the first point.
*     point2
*        An array of double, with one element for each FrameSet axis
*        containing the coordinates of the second point.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     The distance between the two points.

*  Notes:
*     - This function will return a "bad" result value (AST__BAD) if
*     any of the input coordinates has this value.
*     - A "bad" value will also be returned if this function is
*     invoked with the AST error status set or if it should fail for
*     any reason.
*/

/* Local Variables: */
   AstFrame *fr;                 /* Pointer to current Frame */
   AstFrameSet *this;            /* Pointer to the FrameSet structure */
   double result;                /* Value to return */

/* Check the global error status. */
   if ( !astOK ) return AST__BAD;

/* Obtain a pointer to the FrameSet structure. */
   this = (AstFrameSet *) this_frame;

/* Obtain a pointer to the FrameSet's current Frame and invoke this
   Frame's astDistance method. Annul the Frame pointer afterwards. */
   fr = astGetFrame( this, AST__CURRENT );
   result = astDistance( fr, point1, point2 );
   fr = astAnnul( fr );

/* If an error occurred, clear the result. */
   if ( !astOK ) result = AST__BAD;

/* Return the result. */
   return result;
}

static int Equal( AstObject *this_object, AstObject *that_object, int *status ) {
/*
*  Name:
*     Equal

*  Purpose:
*     Test if two FrameSets are equivalent.

*  Type:
*     Private function.

*  Synopsis:
*     #include "frameset.h"
*     int Equal( AstObject *this, AstObject *that, int *status )

*  Class Membership:
*     FrameSet member function (over-rides the astEqual protected
*     method inherited from the Mapping class).

*  Description:
*     This function returns a boolean result (0 or 1) to indicate whether
*     two FrameSets are equivalent.

*  Parameters:
*     this
*        Pointer to the first FrameSet.
*     that
*        Pointer to the second FrameSet.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     One if the FrameSets are equivalent, zero otherwise.

*  Notes:
*     - The two FrameSets are considered equivalent if all the encapsulated
*     Frames are equal and all the encapsulated Mappings are equal.
*     - A value of zero will be returned if this function is invoked
*     with the global status set, or if it should fail for any reason.
*/

/* Local Variables: */
   AstFrameSet *that;            /* Pointer to the second FrameSet structure */
   AstFrameSet *this;            /* Pointer to the first FrameSet structure */
   int i;                        /* Loop index */
   int result;                   /* Result value to return */

/* Initialise. */
   result = 0;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Checks that the second object is of the same class as the first . */
   if( !strcmp( astGetClass( this_object ), astGetClass( that_object ) ) ){

/* Obtain pointers to the two FrameSet structures. */
      this = (AstFrameSet *) this_object;
      that = (AstFrameSet *) that_object;

/* Check the number of nodes and frames are equal. Also check the indices
   of the base and current Frames are equal */
      if( this->nframe == that->nframe &&
          this->nnode == that->nnode &&
          this->base == that->base &&
          this->current == that->current ) {

/* Check the Frames and nodes are equal. */
          result = 1;
          for ( i = 0; i < this->nframe; i++ ) {
             if( !astEqual( this->frame[ i ], that->frame[ i ] ) ||
                 this->node[ i ] != that->node[ i ] ){
                result = 0;
                break;
             }
          }

/* Check the Mappings, links and invert flags are equal. */
         if( result ) {
            for ( i = 0; i < this->nnode - 1; i++ ) {
               if( !astEqual( this->map[ i ], that->map[ i ] ) ||
                   this->link[ i ] != that->link[ i ] ||
                   this->invert[ i ] != that->invert[ i ] ) {
                  result = 0;
                  break;
               }
            }
         }
      }
   }

/* If an error occurred, clear the result value. */
   if ( !astOK ) result = 0;

/* Return the result, */
   return result;
}

static int Fields( AstFrame *this_frame, int axis, const char *fmt,
                   const char *str, int maxfld, char **fields,
                   int *nc, double *val, int *status ) {
/*
*+
*  Name:
*     astFields

*  Purpose:
*     Identify numerical fields within a formatted FrameSet axis value.

*  Type:
*     Protected virtual function.

*  Synopsis:
*     #include "frameset.h"
*     int astFields( AstFrame *this, int axis, const char *fmt,
*                    const char *str, int maxfld, char **fields,
*                    int *nc, double *val )

*  Class Membership:
*     FrameSet member function (over-rides the protected astFields
*     method inherited from the Frame class).

*  Description:
*     This function identifies the numerical fields within a FrameSet axis
*     value that has been formatted using astAxisFormat. It assumes that
*     the value was formatted using the supplied format string. It also
*     returns the equivalent floating point value.

*  Parameters:
*     this
*        Pointer to the FrameSet.
*     axis
*        The number of the FrameSet axis for which the values have been
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
   AstFrame *fr;                 /* Pointer to current Frame */
   AstFrameSet *this;            /* Pointer to the FrameSet structure */
   int result;                   /* Result field count to return */

/* Check the global error status. */
   if ( !astOK ) return 0;

/* Obtain a pointer to the FrameSet structure. */
   this = (AstFrameSet *) this_frame;

/* Validate the axis index. */
   (void) astValidateAxis( this, axis, 1, "astFields" );

/* Obtain a pointer to the FrameSet's current Frame and invoke this
   Frame's astFields method to perform the processing. Annul the Frame
   pointer afterwards. */
   fr = astGetFrame( this, AST__CURRENT );
   result = astFields( fr, axis, fmt, str, maxfld, fields, nc, val );
   fr = astAnnul( fr );

/* If an error occurred, clear the result. */
   if ( !astOK ) result = 0;

/* Return the result. */
   return result;
}

static AstFrameSet *FindFrame( AstFrame *target_frame, AstFrame *template,
                               const char *domainlist, int *status ) {
/*
*  Name:
*     FindFrame

*  Purpose:
*     Find a coordinate system with specified characteristics.

*  Type:
*     Private function.

*  Synopsis:
*     #include "frameset.h"
*     AstFrameSet *FindFrame( AstFrame *target, AstFrame *template,
*                             const char *domainlist, int *status )

*  Class Membership:
*     FrameSet member function (over-rides the astFindFrame method
*     inherited from the Frame class).

*  Description:
*     This function uses a "template" Frame to search a FrameSet to
*     identify a coordinate system which has a specified set of
*     characteristics. If a suitable coordinate system can be found,
*     the function returns a pointer to a FrameSet which describes the
*     required coordinate system and how to convert coordinates to and
*     from it.

*  Parameters:
*     target
*        Pointer to the target FrameSet.  Note that if a suitable
*        coordinate system is found, then the FrameSet's Current
*        attribute will be modified to indicate which Frame was used
*        to obtain attribute values which were not specified by the
*        template.
*     template
*        Pointer to the template Frame, which should be an instance of
*        the type of Frame you wish to find.
*     domainlist
*        Pointer to a null-terminated character string containing a
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
*        If you do not wish to restrict the domain in this way, you
*        should supply an empty string.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     If the search is successful, the function returns a pointer to a
*     FrameSet which contains the Frame found and a description of how
*     to convert to (and from) the coordinate system it
*     represents. Otherwise, a null Object pointer (AST__NULL) is
*     returned without error.
*
*     If a FrameSet is returned, it will contain two Frames. Frame
*     number 1 (its base Frame) represents the target coordinate
*     system and will be the same as the (base Frame of the)
*     target. Frame number 2 (its current Frame) will be a Frame
*     representing the coordinate system which the function found. The
*     Mapping which inter-relates these two Frames will describe how
*     to convert between their respective coordinate systems.

*  Notes:
*     - A null Object pointer (AST__NULL) will be returned if this
*     function is invoked with the AST error status set, or if it
*     should fail for any reason.
*/

/* Local Variables: */
   AstFrame *base_frame;         /* Pointer to target base Frame */
   AstFrame *frame;              /* Pointer to result Frame */
   AstFrame *selected_frame;     /* Pointer to selected target Frame */
   AstFrameSet *found;           /* FrameSet pointer (result of search) */
   AstFrameSet *result;          /* Pointer to result FrameSet */
   AstFrameSet *target;          /* Pointer to target FrameSet structure */
   AstMapping *map;              /* Pointer to result Mapping */
   AstMapping *prefix;           /* Pointer to prefix Mapping */
   AstMapping *tmp;              /* Temporary Mapping pointer */
   char *domain;                 /* Pointer to individual domain field */
   char *domain_end;             /* Pointer to null at end of domain */
   char *domainlist_copy;        /* Pointer to copy of domains list */
   int *target_order;            /* Array of indices defining search order */
   int match;                    /* Match obtained? */
   int n;                        /* Count of target_order elements */
   int target_base;              /* Index of target base Frame */
   int target_current;           /* Index of target current Frame */
   int target_index;             /* Index of selected target Frame */
   int target_nframe;            /* Number of Frames in target FrameSet */
   int target_number;            /* Loop index for search */

/* Initialise. */
   result = NULL;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Initialise variables to avoid "used of uninitialised variable"
   messages from dumb compilers. */
   target_index = 0;

/* Obtain a pointer to the target FrameSet structure. */
   target = (AstFrameSet *) target_frame;

/* Determine the number of Frames in the target FrameSet and the
   indices of the current and base Frames. */
   target_nframe = astGetNframe( target );
   target_current = astGetCurrent( target );
   target_base = astGetBase( target );

/* Allocate an array to hold a list of all the target Frame indices. */
   target_order = astMalloc( sizeof( int ) * (size_t) target_nframe );

/* Make a temporary copy of the domains list. */
   domainlist_copy = astStore( NULL, domainlist,
                               strlen( domainlist ) + (size_t) 1 );
   if ( astOK ) {

/* Form a list of the indices of all the Frames in the target in the
   order they will be searched for a match. Add the current Frame
   index first. */
      n = 0;
      target_order[ n++ ] = target_current;

/* Follow this by the base Frame index, if different. */
      if ( target_base != target_current ) target_order[ n++ ] = target_base;

/* Then add all the remaining target Frame indices. */
      for ( target_index = 1; target_index <= target_nframe; target_index++ ) {
         if ( ( target_index != target_current ) &&
              ( target_index != target_base ) ) {
            target_order[ n++ ] = target_index;
         }
      }

/* Loop to inspect each comma-separated field in the domains list
   until an error occurs, all the domains are used up, or a match is
   found. */
      domain = domainlist_copy;
      match = 0;
      while ( astOK && domain && !match ) {

/* Change the comma at the end of each field to a null to terminate
   the domain. */
         if ( ( domain_end = strchr( domain, ',' ) ) ) *domain_end = '\0';

/* Loop to try and match each target Frame in turn, in the order
   identified above. Quit the loop early if an error occurs or a match
   is found. */
         for ( target_number = 0;
               astOK && !match && ( target_number < target_nframe );
               target_number++ ) {

/* Permute the target Frame number into a Frame index to implement the
   required search order. Then obtain a pointer to the selected target
   Frame. */
            target_index = target_order[ target_number ];
            selected_frame = astGetFrame( target, target_index );

/* Search the target Frame using the template supplied, together with
   the current domain. */
            found = astFindFrame( selected_frame, template, domain );

/* Note if a match is found, and extract pointers to the conversion
   Mapping and the result Frame from the FrameSet produced. */
            if ( astOK && found ) {
               match = 1;
               map = astGetMapping( found, AST__BASE, AST__CURRENT );
               frame = astGetFrame( found, AST__CURRENT );

/* Obtain a pointer to the Mapping between the target base Frame and
   the selected target Frame, and prefix this Mapping to the one
   obtained above. */
               prefix = astGetMapping( target, AST__BASE, target_index );
               tmp = (AstMapping *) astCmpMap( prefix, map, 1, "", status );
               prefix = astAnnul( prefix );
               map = astAnnul( map );
               map = tmp;

/* Simplify the resulting Mapping. */
               tmp = astSimplify( map );
               map = astAnnul( map );
               map = tmp;

/* Obtain a pointer to the target base Frame, and use this to start
   building the result FrameSet. */
               base_frame = astGetFrame( target, AST__BASE );
               result = astFrameSet( base_frame, "", status );
               base_frame = astAnnul( base_frame );

/* Add the result Frame, which is related to the base Frame by the
   result Mapping. */
               astAddFrame( result, AST__BASE, map, frame );

/* Annul pointers to all intermediate Objects. */
               map = astAnnul( map );
               frame = astAnnul( frame );
               found = astAnnul( found );
            }
            selected_frame = astAnnul( selected_frame );
         }

/* Go on to consider the next field in the domains list. */
         domain = domain_end ? domain_end + 1 : NULL;
      }
   }

/* Free the temporary arrays. */
   target_order = astFree( target_order );
   domainlist_copy = astFree( domainlist_copy );

/* If a result is being returned, set the current Frame of the target
   to indicate where the result Frame was found. */
   if ( astOK && result ) astSetCurrent( target, target_index );

/* If an error occurred, annul any result FrameSet pointer. */
   if ( !astOK && result ) result = astAnnul( result );

/* Return the result. */
   return result;
}

static AstPointSet *FrameGrid( AstFrame *this_frame, int size, const double *lbnd,
                               const double *ubnd, int *status ){
/*
*  Name:
*     FrameGrid

*  Purpose:
*     Return a grid of points covering a rectangular area of a Frame.

*  Type:
*     Private function.

*  Synopsis:
*     #include "frameset.h"
*     AstPointSet *FrameGrid( AstFrame *this_frame, int size,
*                             const double *lbnd, const double *ubnd,
*                             int *status )

*  Class Membership:
*     FrameSet member function (over-rides the protected astFrameGrid
*     method inherited from the Frame class).

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
*/

/* Local Variables: */
   AstFrame *fr;                 /* Pointer to current Frame */
   AstFrameSet *this;            /* Pointer to the FrameSet structure */
   AstPointSet *result;          /* Value to return */

/* Check the global error status. */
   if ( !astOK ) return NULL;

/* Obtain a pointer to the FrameSet structure. */
   this = (AstFrameSet *) this_frame;

/* Obtain a pointer to the FrameSet's current Frame and invoke this
   Frame's astFrameGrid method. Annul the Frame pointer afterwards. */
   fr = astGetFrame( this, AST__CURRENT );
   result = astFrameGrid( fr, size, lbnd, ubnd );
   fr = astAnnul( fr );

/* If an error occurred, clear the result. */
   if ( !astOK ) result = astAnnul( result );

/* Return the result. */
   return result;
}

static int ForceCopy( AstFrameSet *this, int iframe, int *status ) {
/*
*  Name:
*     ForceCopy

*  Purpose:
*     Force a copy to be made of a Frame, if necessary.

*  Type:
*     Private function.

*  Synopsis:
*     #include "frameset.h"
*     int ForceCopy( AstFrameSet *this, int iframe, int *status )

*  Class Membership:
*     FrameSet member function.

*  Description:
*     This function examines a Frame within a FrameSet, identified by its
*     Frame index. If the same Frame is found to be referenced a second time
*     within the FrameSet, then the original reference is replaced with an
*     independent copy of the Frame.
*
*     This process supports the preservation of FrameSet integrity in cases
*     where the same Frame is referenced more than once. After using this
*     function, the nominated Frame's attributes may be modified without
*     danger of affecting other parts of the FrameSet.

*  Parameters:
*     this
*        Pointer to the FrameSet.
*     iframe
*        The index of the Frame to be examined.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     One if a copy of the nominated Frame was made, otherwise zero.

*  Notes:
*     - Using this function a second time on the same Frame will have no
*     effect, since the first usage will make the Frame independent of any
*     other Frames within the FrameSet.
*     - A value of zero will be returned if this function is invoked with
*     the global error status set, or if it should fail for any reason.
*/

/* Local Variables: */
   astDECLARE_GLOBALS            /* Declare the thread specific global data */
   AstFrame *frame;             /* Pointer to Frame */
   AstFrame *tmp;               /* Temporary Frame pointer */
   int ifr;                     /* Loop counter for Frames */
   int result;                  /* Value to return */

/* Initialise. */
   result = 0;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Get a pointer to the structure holding thread-specific global data. */
   astGET_GLOBALS(this);

/* Validate and translate the Frame index supplied. */
   iframe = astValidateFrameIndex( this, iframe, integrity_method );

/* If OK, obtain the corresponding Frame pointer (don't clone it). */
   if ( astOK ) {
      frame = this->frame[ iframe - 1 ];

/* Loop to inspect each Frame in the FrameSet. */
      for ( ifr = 1; ifr <= this->nframe; ifr++ ) {

/* If the same Frame is referenced anywhere else, then make a copy of it. */
         if ( ( ifr != iframe ) && ( this->frame[ ifr - 1 ] == frame ) ) {
            tmp = astCopy( frame );

/* If successful, replace the original reference to the Frame with a pointer
   to this copy and annul the original pointer. */
            if ( astOK ) {
               this->frame[ iframe - 1 ] = tmp;
               frame = astAnnul( frame );

/* Set the returned result. */
               if ( astOK ) result = 1;
            }

/* Quit looping once a copy has been made. */
            break;
         }
      }
   }

/* Return the result. */
   return result;
}

static const char *Format( AstFrame *this_frame, int axis, double value, int *status ) {
/*
*  Name:
*     Format

*  Purpose:
*     Format a coordinate value for a FrameSet axis.

*  Type:
*     Private function.

*  Synopsis:
*     #include "frameset.h"
*     const char *Format( AstFrame *this, int axis, double value, int *status )

*  Class Membership:
*     FrameSet member function (over-rides the astFormat method
*     inherited from the Frame class).

*  Description:
*     This function returns a pointer to a string containing the
*     formatted (character) version of a coordinate value for a
*     FrameSet axis. The formatting applied is that specified by a
*     previous invocation of the astSetFormat method. A suitable
*     default format is applied if necessary.

*  Parameters:
*     this
*        Pointer to the FrameSet.
*     axis
*        The number of the axis (zero-based) for which formatting is
*        to be performed.
*     value
*        The coordinate value to be formatted.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     A pointer to a null-terminated string containing the formatted
*     value.

*  Notes:
*     - The returned string pointer may point at memory allocated
*     within the FrameSet object, or at static memory. The contents of
*     the string may be over-written or the pointer may become invalid
*     following a further invocation of the same function or deletion
*     of the FrameSet. A copy of the string should therefore be made
*     if necessary.
*     - A NULL pointer will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*/

/* Local Variables: */
   AstFrame *fr;                 /* Pointer to current Frame */
   AstFrameSet *this;            /* Pointer to the FrameSet structure */
   const char *result;           /* Pointer value to return */

/* Check the global error status. */
   if ( !astOK ) return NULL;

/* Obtain a pointer to the FrameSet structure. */
   this = (AstFrameSet *) this_frame;

/* Validate the axis index. */
   (void) astValidateAxis( this, axis, 1, "astFormat" );

/* Obtain a pointer to the FrameSet's current Frame and invoke the
   astFormat method for this Frame. Annul the Frame pointer
   afterwards. */
   fr = astGetFrame( this, AST__CURRENT );
   result = astFormat( fr, axis, value );
   fr = astAnnul( fr );

/* If an error occurred, clear the result value. */
   if ( !astOK ) result = NULL;

/* Return the result. */
   return result;
}

static double Gap( AstFrame *this_frame, int axis, double gap, int *ntick, int *status ) {
/*
*  Name:
*     Gap

*  Purpose:
*     Find a "nice" gap for tabulating FrameSet axis values.

*  Type:
*     Private function.

*  Synopsis:
*     #include "frameset.h"
*     double Gap( AstFrame *this, int axis, double gap, int *ntick, int *status )

*  Class Membership:
*     FrameSet member function (over-rides the protected astGap method
*     inherited from the Frame class).

*  Description:
*     This function returns a gap size which produces a nicely spaced
*     series of formatted values for a FrameSet axis, the returned gap
*     size being as close as possible to the supplied target gap
*     size. It also returns a convenient number of divisions into
*     which the gap can be divided.

*  Parameters:
*     this
*        Pointer to the FrameSet.
*     axis
*        The number of the axis (zero-based) for which a gap is to be found.
*     gap
*        The target gap size.
*     ntick
*        Address of an int in which to return a convenient number of
*        divisions into which the gap can be divided.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     The nice gap size.

*  Notes:
*     - A value of zero is returned if the target gap size is zero.
*     - A negative gap size is returned if the supplied gap size is negative.
*     - A value of zero will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*/

/* Local Variables: */
   AstFrame *fr;                 /* Pointer to current Frame */
   AstFrameSet *this;            /* Pointer to the FrameSet structure */
   double result;                /* Gap value to return */

/* Check the global error status. */
   if ( !astOK ) return 0.0;

/* Obtain a pointer to the FrameSet structure. */
   this = (AstFrameSet *) this_frame;

/* Validate the axis index. */
   (void) astValidateAxis( this, axis, 1, "astGap" );

/* Obtain a pointer to the FrameSet's current Frame and invoke this
   Frame's astGap method to obtain the required gap value. Annul the
   Frame pointer afterwards. */
   fr = astGetFrame( this, AST__CURRENT );
   result = astGap( fr, axis, gap, ntick );
   fr = astAnnul( fr );

/* If an error occurred, clear the result. */
   if ( !astOK ) result = 0.0;

/* Return the result. */
   return result;
}

static int GetObjSize( AstObject *this_object, int *status ) {
/*
*  Name:
*     GetObjSize

*  Purpose:
*     Return the in-memory size of an Object.

*  Type:
*     Private function.

*  Synopsis:
*     #include "frameset.h"
*     int GetObjSize( AstObject *this, int *status )

*  Class Membership:
*     FrameSet member function (over-rides the astGetObjSize protected
*     method inherited from the parent class).

*  Description:
*     This function returns the in-memory size of the supplied FrameSet,
*     in bytes.

*  Parameters:
*     this
*        Pointer to the FrameSet.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     The Object size, in bytes.

*  Notes:
*     - A value of zero will be returned if this function is invoked
*     with the global status set, or if it should fail for any reason.
*/

/* Local Variables: */
   AstFrameSet *this;         /* Pointer to FrameSet structure */
   int result;                /* Result value to return */
   int iframe;                /* Loop counter for Frames */
   int inode;                 /* Loop counter for nodes */

/* Initialise. */
   result = 0;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Obtain a pointers to the FrameSet structure. */
   this = (AstFrameSet *) this_object;

/* Invoke the GetObjSize method inherited from the parent class, and then
   add on any components of the class structure defined by thsi class
   which are stored in dynamically allocated memory. */
   result = (*parent_getobjsize)( this_object, status );

   for ( iframe = 0; iframe < this->nframe; iframe++ ) {
      result += astGetObjSize( this->frame[ iframe ] );
   }

   for ( inode = 0; inode < this->nnode - 1; inode++ ) {
      result += astGetObjSize( this->map[ inode ] );
   }

   result += astTSizeOf( this->frame );
   result += astTSizeOf( this->node );
   result += astTSizeOf( this->map );
   result += astTSizeOf( this->link );
   result += astTSizeOf( this->invert );

/* If an error occurred, clear the result value. */
   if ( !astOK ) result = 0;

/* Return the result, */
   return result;
}

static const char *GetAttrib( AstObject *this_object, const char *attrib, int *status ) {
/*
*  Name:
*     GetAttrib

*  Purpose:
*     Get the value of a specified attribute for a FrameSet.

*  Type:
*     Private function.

*  Synopsis:
*     #include "frameset.h"
*     const char *GetAttrib( AstObject *this, const char *attrib, int *status )

*  Class Membership:
*     FrameSet member function (over-rides the protected astGetAttrib
*     method inherited from the Frame class).

*  Description:
*     This function returns a pointer to the value of a specified
*     attribute for a FrameSet, formatted as a character string.

*  Parameters:
*     this
*        Pointer to the FrameSet.
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
*     - The returned string pointer may point at memory allocated
*     within the FrameSet, or at static memory. The contents of the
*     string may be over-written or the pointer may become invalid
*     following a further invocation of the same function or any
*     modification of the FrameSet. A copy of the string should
*     therefore be made if necessary.
*     - A NULL pointer will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*/

/* Local Variables: */
   astDECLARE_GLOBALS            /* Declare the thread specific global data */
   AstFrame *fr;                 /* Pointer to current Frame */
   AstFrameSet *this;            /* Pointer to the FrameSet structure */
   const char *result;           /* Pointer value to return */
   int base;                     /* Base attribute value */
   int current;                  /* Current attribute value */
   int invert;                   /* Invert attribute value */
   int nframe;                   /* Nframe attribute value */
   int nin;                      /* Nin attribute value */
   int nobject;                  /* Nobject attribute value */
   int nout;                     /* Nout attribute value */
   int ref_count;                /* RefCount attribute value */
   int report;                   /* Report attribute value */
   int tranforward;              /* TranForward attribute value */
   int traninverse;              /* TranInverse attribute value */

/* Initialise. */
   result = NULL;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Get a pointer to the structure holding thread-specific global data. */
   astGET_GLOBALS(this_object);

/* Obtain a pointer to the FrameSet structure. */
   this = (AstFrameSet *) this_object;

/* Compare "attrib" with each recognised attribute name in turn,
   obtaining the value of the required attribute. If necessary, write
   the value into "getattrib_buff" as a null-terminated string in an appropriate
   format.  Set "result" to point at the result string. */

/* We first handle attributes that apply to the FrameSet as a whole
   (rather than to the current Frame). */

/* Base. */
/* ----- */
   if ( !strcmp( attrib, "base" ) ) {
      base = astGetBase( this );
      if ( astOK ) {
         (void) sprintf( getattrib_buff, "%d", base );
         result = getattrib_buff;
      }

/* Class. */
/* ------ */
   } else if ( !strcmp( attrib, "class" ) ) {
      result = astGetClass( this );

/* Current. */
/* -------- */
   } else if ( !strcmp( attrib, "current" ) ) {
      current = astGetCurrent( this );
      if ( astOK ) {
         (void) sprintf( getattrib_buff, "%d", current );
         result = getattrib_buff;
      }

/* ID. */
/* --- */
   } else if ( !strcmp( attrib, "id" ) ) {
      result = astGetID( this );

/* Ident. */
/* ------ */
   } else if ( !strcmp( attrib, "ident" ) ) {
      result = astGetIdent( this );

/* Invert. */
/* ------- */
   } else if ( !strcmp( attrib, "invert" ) ) {
      invert = astGetInvert( this );
      if ( astOK ) {
         (void) sprintf( getattrib_buff, "%d", invert );
         result = getattrib_buff;
      }

/* Nframe. */
/* ------- */
   } else if ( !strcmp( attrib, "nframe" ) ) {
      nframe = astGetNframe( this );
      if ( astOK ) {
         (void) sprintf( getattrib_buff, "%d", nframe );
         result = getattrib_buff;
      }

/* Nin. */
/* ---- */
   } else if ( !strcmp( attrib, "nin" ) ) {
      nin = astGetNin( this );
      if ( astOK ) {
         (void) sprintf( getattrib_buff, "%d", nin );
         result = getattrib_buff;
      }

/* Nobject. */
/* -------- */
   } else if ( !strcmp( attrib, "nobject" ) ) {
      nobject = astGetNobject( this );
      if ( astOK ) {
         (void) sprintf( getattrib_buff, "%d", nobject );
         result = getattrib_buff;
      }

/* Nout. */
/* ----- */
   } else if ( !strcmp( attrib, "nout" ) ) {
      nout = astGetNout( this );
      if ( astOK ) {
         (void) sprintf( getattrib_buff, "%d", nout );
         result = getattrib_buff;
      }

/* RefCount. */
/* --------- */
   } else if ( !strcmp( attrib, "refcount" ) ) {
      ref_count = astGetRefCount( this );
      if ( astOK ) {
         (void) sprintf( getattrib_buff, "%d", ref_count );
         result = getattrib_buff;
      }

/* Report. */
/* ------- */
   } else if ( !strcmp( attrib, "report" ) ) {
      report = astGetReport( this );
      if ( astOK ) {
         (void) sprintf( getattrib_buff, "%d", report );
         result = getattrib_buff;
      }

/* TranForward. */
/* ------------ */
   } else if ( !strcmp( attrib, "tranforward" ) ) {
      tranforward = astGetTranForward( this );
      if ( astOK ) {
         (void) sprintf( getattrib_buff, "%d", tranforward );
         result = getattrib_buff;
      }

/* TranInverse. */
/* ------------ */
   } else if ( !strcmp( attrib, "traninverse" ) ) {
      traninverse = astGetTranInverse( this );
      if ( astOK ) {
         (void) sprintf( getattrib_buff, "%d", traninverse );
         result = getattrib_buff;
      }

/* Pass unrecognised attributes on to the FrameSet's current Frame for
   further interpretation. */
   } else {

/* Obtain a pointer to the current Frame and invoke its astGetAttrib
   method. Annul the Frame pointer afterwards. */
      fr = astGetFrame( this, AST__CURRENT );
      result = astGetAttrib( fr, attrib );
      fr = astAnnul( fr );
   }

/* If an error occurred, clear the result value. */
   if ( !astOK ) result = NULL;

/* Return the result. */
   return result;
}

static AstAxis *GetAxis( AstFrame *this_frame, int axis, int *status ) {
/*
*  Name:
*     GetAxis

*  Purpose:
*     Obtain a pointer to a specified Axis from a FrameSet.

*  Type:
*     Private function.

*  Synopsis:
*     #include "frameset.h"
*     AstAxis *GetAxis( AstFrame *this, int axis, int *status )

*  Class Membership:
*     FrameSet member function (over-rides the astGetAxis method
*     inherited from the Frame class).

*  Description:
*     This function returns a pointer to the Axis object associated
*     with one of the axes of the current Frame of a FrameSet. This
*     object describes the quantity which is represented along that
*     axis.

*  Parameters:
*     this
*        Pointer to the FrameSet.
*     axis
*        The number of the axis (zero-based) for which an Axis pointer
*        is required.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     A pointer to the requested Axis object.

*  Notes:
*     - The reference count of the requested Axis object will be
*     incremented by one to reflect the additional pointer returned by
*     this function.
*     - A NULL pointer will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*/

/* Local Variables: */
   AstAxis *result;              /* Pointer to Axis */
   AstFrame *fr;                 /* Pointer to current Frame */
   AstFrameSet *this;            /* Pointer to the FrameSet structure */

/* Check the global error status. */
   if ( !astOK ) return NULL;

/* Obtain a pointer to the FrameSet structure. */
   this = (AstFrameSet *) this_frame;

/* Validate the axis index. */
   (void) astValidateAxis( this, axis, 1, "astGetAxis" );

/* Obtain a pointer to the FrameSet's current Frame and invoke this
   Frame's astGetAxis method to obtain the required Axis
   pointer. Annul the Frame pointer afterwards. */
   fr = astGetFrame( this, AST__CURRENT );
   result = astGetAxis( fr, axis );
   fr = astAnnul( fr );

/* If an error occurred, annul the result. */
   if ( !astOK ) result = astAnnul( result );

/* Return the result. */
   return result;
}

static int GetBase( AstFrameSet *this, int *status ) {
/*
*+
*  Name:
*     astGetBase

*  Purpose:
*     Obtain the value of the Base attribute for a FrameSet.

*  Type:
*     Protected virtual function.

*  Synopsis:
*     #include "frameset.h"
*     int astGetBase( AstFrameSet *this )

*  Class Membership:
*     FrameSet method.

*  Description:
*     This function returns the value of the Base attribute for a
*     FrameSet. This value is an index that identifies the base Frame
*     in the FrameSet.

*  Parameters:
*     this
*        Pointer to the FrameSet.

*  Returned Value:
*     The Base attribute value.

*  Notes:
*     - A value of zero will be returned if this function is invoked
*     with the global status set, or if it should fail for any reason.
*-
*/

/* Local Variables: */
   int invert;                   /* FrameSet is inverted? */
   int result;                   /* Value to return */

/* Initialise */
   result = 0;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Determine if the FrameSet has been inverted. */
   invert = astGetInvert( this );

/* If it has not been inverted, return the base Frame index, otherwise
   return the index of the current Frame instead. Provide defaults if
   necessary. */
   if ( astOK ) {
      if ( !invert ) {
         result = ( this->base != -INT_MAX ) ? this->base : 1;
      } else {
         result = ( this->current != -INT_MAX ) ? this->current :
                                                  astGetNframe( this );
      }
   }

/* If an error occurred, clear the result. */
   if ( !astOK ) result = 0;

/* Return the result. */
   return result;
}

static int GetCurrent( AstFrameSet *this, int *status ) {
/*
*+
*  Name:
*     astGetCurrent

*  Purpose:
*     Obtain the value of the Current attribute for a FrameSet.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "frameset.h"
*     int astGetCurrent( AstFrameSet *this )

*  Class Membership:
*     FrameSet method.

*  Description:
*     This function returns the value of the Current attribute for a
*     FrameSet.  This attribute is an index that identifies the
*     current Frame in a FrameSet.

*  Parameters:
*     this
*        Pointer to the FrameSet.

*  Returned Value:
*     Value of the Current attribute.

*  Notes:
*     - A value of zero will be returned if this function is invoked
*     with the global status set, or if it should fail for any reason.
*-
*/

/* Local Variables: */
   int invert;                   /* FrameSet is inverted? */
   int result;                   /* Value to return */

/* Initialise */
   result = 0;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Determine if the FrameSet has been inverted. */
   invert = astGetInvert( this );

/* If it has not been inverted, return the current Frame index,
   otherwise return the index of the base Frame instead. Provide
   defaults if necessary. */
   if ( astOK ) {
      if ( !invert ) {
         result = ( this->current != -INT_MAX ) ? this->current :
                                                  astGetNframe( this );
      } else {
         result = ( this->base != -INT_MAX ) ? this->base : 1;
      }
   }

/* If an error occurred, clear the result. */
   if ( !astOK ) result = 0;

/* Return the result. */
   return result;
}

static AstFrame *GetFrame( AstFrameSet *this, int iframe, int *status ) {
/*
*++
*  Name:
c     astGetFrame
f     AST_GETFRAME

*  Purpose:
*     Obtain a pointer to a specified Frame in a FrameSet.

*  Type:
*     Public virtual function.

*  Synopsis:
c     #include "frameset.h"
c     AstFrame *astGetFrame( AstFrameSet *this, int iframe )
f     RESULT = AST_GETFRAME( THIS, IFRAME, STATUS )

*  Class Membership:
*     FrameSet method.

*  Description:
*     This function returns a pointer to a specified Frame in a
*     FrameSet.

*  Parameters:
c     this
f     THIS = INTEGER (Given)
*        Pointer to the FrameSet.
c     iframe
f     IFRAME = INTEGER (Given)
*        The index of the required Frame within the FrameSet.  This
*        value should lie in the range from 1 to the number of Frames
*        in the FrameSet (as given by its Nframe attribute).
f     STATUS = INTEGER (Given and Returned)
f        The global status.

*  Returned Value:
c     astGetFrame()
f     AST_GETFRAME = INTEGER
*        A pointer to the requested Frame.

*  Notes:
*     - A value of AST__BASE or AST__CURRENT may be given for the
c     "iframe" parameter to specify the base Frame or the current
f     IFRAME argument to specify the base Frame or the current
*     Frame respectively.
*     - This function increments the RefCount attribute of the
*     selected Frame by one.
*     - A null Object pointer (AST__NULL) will be returned if this
c     function is invoked with the AST error status set, or if it
f     function is invoked with STATUS set to an error value, or if it
*     should fail for any reason.
*--
*/

/* Local Variables: */
   AstFrame *result;             /* Pointer value to return */

/* Initialise. */
   result = NULL;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Validate and translate the Frame index supplied. */
   iframe = astValidateFrameIndex( this, iframe, "astGetFrame" );

/* If OK, clone a pointer to the requested Frame. */
   if ( astOK ) result = astClone( this->frame[ iframe - 1 ] );

/* Return the result. */
   return result;
}

static int GetIsLinear( AstMapping *this_mapping, int *status ){
/*
*  Name:
*     GetIsLinear

*  Purpose:
*     Return the value of the IsLinear attribute for a FrameSet.

*  Type:
*     Private function.

*  Synopsis:
*     #include "mapping.h"
*     void GetIsLinear( AstMapping *this, int *status )

*  Class Membership:
*     FrameSet member function (over-rides the protected astGetIsLinear
*     method inherited from the Mapping class).

*  Description:
*     This function returns the value of the IsLinear attribute for a
*     FrameSet, which is the IsLinear value of he base->current Mapping.

*  Parameters:
*     this
*        Pointer to the Frame.
*     status
*        Pointer to the inherited status variable.
*/
/* Local Variables: */
   AstMapping *map;
   int result;

/* Check global status */
   if( !astOK ) return 0;

/* Get the Mapping. */
   map = astGetMapping( (AstFrameSet *) this_mapping, AST__BASE,
                        AST__CURRENT );

/* Get its IsLinear attribute value. */
   result = astGetIsLinear( map );

/* Free the Mapping. */
   map = astAnnul( map );

/* Return the result. */
   return result;
}

static AstMapping *GetMapping( AstFrameSet *this, int iframe1, int iframe2, int *status ) {
/*
*++
*  Name:
c     astGetMapping
f     AST_GETMAPPING

*  Purpose:
*     Obtain a Mapping that converts between two Frames in a FrameSet.

*  Type:
*     Public virtual function.

*  Synopsis:
c     #include "frameset.h"
c     AstMapping *astGetMapping( AstFrameSet *this, int iframe1, int iframe2 )
f     RESULT = AST_GETMAPPING( THIS, IFRAME1, IFRAME2, STATUS )

*  Class Membership:
*     FrameSet method.

*  Description:
*     This function returns a pointer to a Mapping that will convert
*     coordinates between the coordinate systems represented by two
*     Frames in a FrameSet.

*  Parameters:
c     this
f     THIS = INTEGER (Given)
*        Pointer to the FrameSet.
c     iframe1
f     IFRAME1 = INTEGER (Given)
*        The index of the first Frame in the FrameSet. This Frame describes
*        the coordinate system for the "input" end of the Mapping.
c     iframe2
f     IFRAME2 = INTEGER (Given)
*        The index of the second Frame in the FrameSet. This Frame
*        describes the coordinate system for the "output" end of the
*        Mapping.
f     STATUS = INTEGER (Given and Returned)
f        The global status.

*  Returned Value:
c     astGetMapping()
f     AST_GETMAPPING = INTEGER
*        Pointer to a Mapping whose forward transformation converts
*        coordinates from the first coordinate system to the second
*        one, and whose inverse transformation converts coordinates in
*        the opposite direction.

*  Notes:
*     - The returned Mapping will include the clipping effect of any
*     Regions which occur on the path between the two supplied Frames
*     (this includes the two supplied Frames themselves).
c     - The values given for the "iframe1" and "iframe2" parameters
f     - The values given for the IFRAME1 and IFRAME2 arguments
*     should lie in the range from 1 to the number of Frames in the
*     FrameSet (as given by its Nframe attribute). A value of
*     AST__BASE or AST__CURRENT may also be given to identify the
*     FrameSet's base Frame or current Frame respectively.  It is
c     permissible for both these parameters to have the same value, in
f     permissible for both these arguments to have the same value, in
*     which case a unit Mapping (UnitMap) is returned.
*     - It should always be possible to generate the Mapping
*     requested, but this does necessarily guarantee that it will be
*     able to perform the required coordinate conversion. If
*     necessary, the TranForward and TranInverse attributes of the
*     returned Mapping should be inspected to determine if the
*     required transformation is available.
*     - A null Object pointer (AST__NULL) will be returned if this
c     function is invoked with the AST error status set, or if it
f     function is invoked with STATUS set to an error value, or if it
*     should fail for any reason.
*--
*/

/* Local Variables: */
   AstFrame *fr;                 /* Temporary pointer to Frame */
   AstFrame **frames;            /* Pointer to array of Frames */
   AstMapping **path;            /* Pointer to array of conversion Mappings */
   AstMapping *copy;             /* Pointer to copy of Mapping */
   AstMapping *result;           /* Result pointer to be returned */
   AstMapping *tmp;              /* Temporary pointer for joining Mappings */
   int *forward;                 /* Pointer to array of Mapping directions */
   int ipath;                    /* Loop counter for conversion path steps */
   int iframe;                   /* Frame index */
   int inode;                    /* Node index */
   int npath;                    /* Number of steps in conversion path */

/* Initialise. */
   result = NULL;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Validate and translate the Frame indices supplied. */
   iframe1 = astValidateFrameIndex( this, iframe1, "astGetMapping" );
   iframe2 = astValidateFrameIndex( this, iframe2, "astGetMapping" );

/* Allocate memory to hold an array of Mapping pointers and associated
   direction flags - a maximum of one element for each Mapping and one
   for each Frame in the FrameSet. */
   path = astMalloc( sizeof( AstMapping * ) * (size_t) ( this->nnode - 1 +
                                                         this->nframe ) );
   forward = astMalloc( sizeof( int ) * (size_t) ( this->nnode - 1 +
                                                   this->nframe ) );

/* Allocate memory to hold a list of the Frame pointers (if any) associated
   with each node. */
   frames = astMalloc( sizeof( AstFrame * )  * (size_t) ( this->nnode ) );

/* If OK, set up an array of Frame pointers indexed by node index. If a
   node has no associated Frame store a NULL pointer. This is needed so
   that we can find Frame pointers quickly within the Span function. Note,
   we simply copy the pointers rather than cloning them, so they do not
   need to be annulled when finished with. */
   if ( astOK ) {
      for( inode = 0; inode < this->nnode; inode++ ) frames[ inode ] = NULL;
      for( iframe = 0; iframe < this->nframe; iframe++ ) {
         frames[ this->node[ iframe ] ] = this->frame[ iframe ];
      }

/* Obtain the Mapping pointers and direction flags needed to convert
   coordinates between the nodes associated with the two specified
   Frames. */
      npath = Span( this, frames, this->node[ iframe1 - 1 ],
                    this->node[ iframe2 - 1 ], -1, path, forward, status ) - 1;

/* If this failed, it indicates a corrupt FrameSet object, so report
   an error. */
      if ( npath < 0 ) {
         astError( AST__FRSIN, "astGetMapping(%s): Invalid or corrupt "
                   "%s - could not find conversion path between Frames "
                   "%d and %d.", status, astGetClass( this ), astGetClass( this ),
                   iframe1, iframe2 );

/* If the conversion path is of zero length (i.e. the two Frames are
   the same) then we will return a Mapping which is equivalent to the
   Frame. Most classes of Frame are equivalent to a UnitMap. However, we do
   not hard-wire this equivalence since some classes of Frame (e.g. Regions
   or CmpFrames containing Regions) do not correspond to a UnitMap. Instead
   we use the astIsUnitFrame method on the Frame to determine if the
   Frame is equivalent to a UnitMap.Is os, create a suitable UnitMap. If
   not, return the Frame itself (a form of Mapping). */
      } else if ( npath == 0 ) {
         fr = astGetFrame( this, iframe1 );
         if( astIsUnitFrame( fr ) ){
            result = (AstMapping *) astUnitMap( astGetNaxes( fr ), "", status );
         } else {
            result = (AstMapping *) astClone( fr );
         }
         fr = astAnnul( fr );

/* If the conversion path involves at least one non-trivial Mapping,
   make a copy of the first Mapping, inverting the copy if
   necessary. */
      } else {
         result = astCopy( path[ 0 ] );
         if ( !forward[ 0 ] ) astInvert( result );

/* Now loop to concatenate any further Mappings. First make a copy of
   each additional Mapping and invert the copy if necessary. */
         for ( ipath = 1; ipath < npath; ipath++ ) {
            copy = astCopy( path[ ipath ] );
            if ( !forward[ ipath ] ) astInvert( copy );

/* Concatenate the copy with the result so far, then annul the pointer
   to the copy and save the pointer to the new result. */
            tmp = (AstMapping *) astCmpMap( result, copy, 1, "", status );
            result = astAnnul( result );
            copy = astAnnul( copy );
            result = tmp;
         }
      }
   }

/* Free the memory allocated for the conversion path information. */
   path = astFree( path );
   forward = astFree( forward );
   frames = astFree( frames );

/* If an error occurred, annul the returned Mapping. */
   if ( !astOK ) result = astAnnul( result );

/* Return the result. */
   return result;
}

static int GetNaxes( AstFrame *this_frame, int *status ) {
/*
*  Name:
*     GetNaxes

*  Purpose:
*     Determine how many axes a FrameSet has.

*  Type:
*     Private function.

*  Synopsis:
*     #include "frameset.h"
*     int GetNaxes( AstFrame *this, int *status )

*  Class Membership:
*     FrameSet member function (over-rides the astGetNaxes method
*     inherited from the Frame class).

*  Description:
*     This function returns the number of axes for a FrameSet. This is equal
*     to the number of axes in its current Frame.

*  Parameters:
*     this
*        Pointer to the FrameSet.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     The number of FrameSet axes (zero or more).

*  Notes:
*     - A value of zero will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*/

/* Local Variables: */
   AstFrame *fr;                 /* Pointer to current Frame */
   AstFrameSet *this;            /* Pointer to the FrameSet structure */
   int result;                   /* Result to be returned */

/* Check the global error status. */
   if ( !astOK ) return 0;

/* Obtain a pointer to the FrameSet structure. */
   this = (AstFrameSet *) this_frame;

/* Obtain a pointer to the FrameSet's current Frame. */
   fr = astGetFrame( this, AST__CURRENT );

/* Obtain the number of axes in this Frame. */
   result = astGetNaxes( fr );

/* Annul the current Frame pointer. */
   fr = astAnnul( fr );

/* If an error occurred, clear the result value. */
   if ( !astOK ) result = 0;

/* Return the result. */
   return result;
}

static int GetNframe( AstFrameSet *this, int *status ) {
/*
*+
*  Name:
*     astGetNframe

*  Purpose:
*     Determine the number of Frames in a FrameSet.

*  Type:
*     Protected virtual function.

*  Synopsis:
*     #include "frameset.h"
*     int astGetNframe( AstFrameSet *this )

*  Class Membership:
*     FrameSet method.

*  Description:
*     This function returns the number of Frames in a FrameSet.

*  Parameters:
*     this
*        Pointer to the FrameSet.

*  Returned Value:
*     The number of Frames in the FrameSet (always 1 or more).

*  Notes:
*     - A value of zero will be returned if this function is invoked
*     with the global status set, or if it should fail for any reason.
*-
*/

/* Check the global error status. */
   if ( !astOK ) return 0;

/* Return the Frame count. */
   return this->nframe;
}

static int GetNin( AstMapping *this_mapping, int *status ) {
/*
*  Name:
*     GetNin

*  Purpose:
*     Get the number of input coordinates for a FrameSet.

*  Type:
*     Private function.

*  Synopsis:
*     #include "frameset.h"
*     int GetNin( AstMapping *this, int *status )

*  Class Membership:
*     FrameSet member function (over-rides the astGetNin method
*     inherited from the Frame class).

*  Description:
*     This function returns the number of input coordinate values
*     required per point by a FrameSet when used to transform a set of
*     points (i.e. the number of dimensions of the space in which the
*     input points reside).
*
*     The value returned is equal to the number of axes in the
*     FrameSet's base Frame.

*  Parameters:
*     this
*        Pointer to the FrameSet.
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
   AstFrame *fr;                 /* Pointer to base Frame */
   AstFrameSet *this;            /* Pointer to the FrameSet structure */
   int result;                   /* Result to be returned */

/* Check the global error status. */
   if ( !astOK ) return 0;

/* Obtain a pointer to the FrameSet structure. */
   this = (AstFrameSet *) this_mapping;

/* Obtain a pointer to the FrameSet's base Frame. */
   fr = astGetFrame( this, AST__BASE );

/* Obtain the number of axes in this Frame. */
   result = astGetNaxes( fr );

/* Annul the base Frame pointer. */
   fr = astAnnul( fr );

/* If an error occurred, clear the result value. */
   if ( !astOK ) result = 0;

/* Return the result. */
   return result;
}

static int GetNout( AstMapping *this_mapping, int *status ) {
/*
*  Name:
*     GetNout

*  Purpose:
*     Get the number of output coordinates for a FrameSet.

*  Type:
*     Private function.

*  Synopsis:
*     #include "frameset.h"
*     int GetNout( AstMapping *this, int *status )

*  Class Membership:
*     FrameSet member function (over-rides the astGetNout method
*     inherited from the Frame class).

*  Description:
*     This function returns the number of output coordinate values
*     generated per point by a FrameSet when used to transform a set
*     of points (i.e. the number of dimensions of the space in which
*     the output points reside).
*
*     The value returned is equal to the number of axes in the
*     FrameSet's current Frame.

*  Parameters:
*     this
*        Pointer to the FrameSet.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     Number of coordinate values generated.

*  Notes:
*     - A value of zero will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*/

/* Check the global error status. */
   if ( !astOK ) return 0;

/* Return the number of axes in the FrameSet's current Frame. */
   return GetNaxes( (AstFrame *) this_mapping, status );
}

static const int *GetPerm( AstFrame *this_frame, int *status ) {
/*
*  Name:
*     GetPerm

*  Purpose:
*     Access the axis permutation array for the current Frame of a FrameSet.

*  Type:
*     Private function.

*  Synopsis:
*     #include "frameset.h"
*     const int *GetPerm( AstFrame *this, int *status )

*  Class Membership:
*     FrameSet member function (over-rides the astGetPerm protected
*     method inherited from the Frame class).

*  Description:
*     This function returns a pointer to the axis permutation array
*     for the current Frame of a FrameSet. This array constitutes a
*     lookup-table that converts between an axis number supplied
*     externally and the corresponding index in the Frame's internal
*     axis arrays.

*  Parameters:
*     this
*        Pointer to the FrameSet.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     Pointer to the current Frame's axis permutation array (a
*     constant array of int).  Each element of this contains the
*     (zero-based) internal axis index to be used in place of the
*     external index which is used to address the permutation
*     array. If the current Frame has zero axes, this pointer will be
*     NULL.

*  Notes:
*     - The pointer returned by this function gives direct access to
*     data internal to the Frame object. It remains valid only so long
*     as the Frame exists. The permutation array contents may be
*     modified by other functions which operate on the Frame and this
*     may render the returned pointer invalid.
*     - A NULL pointer will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*/

/* Local Variables: */
   AstFrame *fr;                 /* Pointer to current Frame */
   AstFrameSet *this;            /* Pointer to FrameSet structure */
   const int *result;            /* Result pointer value */

/* Check the global error status. */
   if ( !astOK ) return NULL;

/* Obtain a pointer to the FrameSet structure. */
   this = (AstFrameSet *) this_frame;

/* Obtain a pointer to the FrameSet's current Frame and then obtain a
   pointer to its axis permutation array. Annul the Frame pointer
   afterwards. */
   fr = astGetFrame( this, AST__CURRENT );
   result = astGetPerm( fr );
   fr = astAnnul( fr );

/* If an error occurred, clear the result value. */
   if ( !astOK ) result = NULL;

/* Return the result. */
   return result;
}

static int GetTranForward( AstMapping *this_mapping, int *status ) {
/*
*  Name:
*     GetTranForward

*  Purpose:
*     Determine if a FrameSet defines a forward coordinate transformation.

*  Type:
*     Private function.

*  Synopsis:
*     #include "frameset.h"
*     int GetTranForward( AstMapping *this )

*  Class Membership:
*     Frameset member function (over-rides the astGetTranForward
*     protected method inherited from the Frame class).

*  Description:
*     This function returns a value indicating whether a FrameSet is
*     able to perform a coordinate transformation in the "forward"
*     direction.

*  Parameters:
*     this
*        Pointer to the FrameSet.

*  Returned Value:
*     Zero if the forward coordinate transformation is not defined, or
*     1 if it is.

*  Notes:
*     - A value of zero will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*/

/* Local Variables: */
   AstFrameSet *this;            /* Pointer to the FrameSet structure */
   AstMapping *map;              /* Pointer to base->current Mapping */
   int result;                   /* Result to be returned */

/* Check the global error status. */
   if ( !astOK ) return 0;

/* Obtain a pointer to the FrameSet structure. */
   this = (AstFrameSet *) this_mapping;

/* Obtain the Mapping between the base and current Frames in the
   FrameSet (note this takes account of whether the FrameSet has been
   inverted). */
   map = astGetMapping( this, AST__BASE, AST__CURRENT );

/* Determine whether the required transformation is defined. */
   result = astGetTranForward( map );

/* Annul the Mapping pointer. */
   map = astAnnul( map );

/* If an error occurred, clear the returned result. */
   if ( !astOK ) result = 0;

/* Return the result. */
   return result;
}

static int GetTranInverse( AstMapping *this_mapping, int *status ) {
/*
*  Name:
*     GetTranInverse

*  Purpose:
*     Determine if a FrameSet defines an inverse coordinate transformation.

*  Type:
*     Private function.

*  Synopsis:
*     #include "frameset.h"
*     int GetTranInverse( AstMapping *this )

*  Class Membership:
*     Frameset member function (over-rides the astGetTranInverse
*     protected method inherited from the Frame class).

*  Description:
*     This function returns a value indicating whether a FrameSet is
*     able to perform a coordinate transformation in the "inverse"
*     direction.

*  Parameters:
*     this
*        Pointer to the FrameSet.

*  Returned Value:
*     Zero if the inverse coordinate transformation is not defined, or
*     1 if it is.

*  Notes:
*     - A value of zero will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*/

/* Local Variables: */
   AstFrameSet *this;            /* Pointer to the FrameSet structure */
   AstMapping *map;              /* Pointer to base->current Mapping */
   int result;                   /* Result to be returned */

/* Check the global error status. */
   if ( !astOK ) return 0;

/* Obtain a pointer to the FrameSet structure. */
   this = (AstFrameSet *) this_mapping;

/* Obtain the Mapping between the base and current Frames in the
   FrameSet (note this takes account of whether the FrameSet has been
   inverted). */
   map = astGetMapping( this, AST__BASE, AST__CURRENT );

/* Determine whether the required transformation is defined. */
   result = astGetTranInverse( map );

/* Annul the Mapping pointer. */
   map = astAnnul( map );

/* If an error occurred, clear the returned result. */
   if ( !astOK ) result = 0;

/* Return the result. */
   return result;
}

static int GetUseDefs( AstObject *this_object, int *status ) {
/*
*  Name:
*     GetUseDefs

*  Purpose:
*     Get the value of the UseDefs attribute for a FrameSet.

*  Type:
*     Private function.

*  Synopsis:
*     #include "frameset.h"
*     int GetUseDefs( AstObject *this_object, int *status ) {

*  Class Membership:
*     FrameSet member function (over-rides the protected astGetUseDefs
*     method inherited from the Frame class).

*  Description:
*     This function returns the value of the UseDefs attribute for a FrameSet,
*     supplying a suitable default.

*  Parameters:
*     this
*        Pointer to the FrameSet.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     - The USeDefs value.
*/

/* Local Variables: */
   AstFrame *fr;                 /* Pointer to current Frame */
   AstFrameSet *this;            /* Pointer to the FrameSet structure */
   int result;                   /* Value to return */

/* Initialise. */
   result = 0;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Obtain a pointer to the FrameSet structure. */
   this = (AstFrameSet *) this_object;

/* If the UseDefs value for the FrameSet has been set explicitly, use the
   Get method inherited from the parent Frame class to get its value> */
   if( astTestUseDefs( this ) ) {
      result = (*parent_getusedefs)( this_object, status );

/* Otherwise, supply a default value equal to the UseDefs value of the
   current Frame. */
   } else {
      fr = astGetFrame( this, AST__CURRENT );
      result = astGetUseDefs( fr );
      fr = astAnnul( fr );
   }

/* Return the result. */
   return result;
}

void astInitFrameSetVtab_(  AstFrameSetVtab *vtab, const char *name, int *status ) {
/*
*+
*  Name:
*     astInitFrameSetVtab

*  Purpose:
*     Initialise a virtual function table for a FrameSet.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "frameset.h"
*     void astInitFrameSetVtab( AstFrameSetVtab *vtab, const char *name )

*  Class Membership:
*     FrameSet vtab initialiser.

*  Description:
*     This function initialises the component of a virtual function
*     table which is used by the FrameSet class.

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
   AstFrameVtab *frame;          /* Pointer to Frame component of Vtab */
   AstMappingVtab *mapping;      /* Pointer to Mapping component of Vtab */
   AstObjectVtab *object;        /* Pointer to Object component of Vtab */

/* Check the local error status. */
   if ( !astOK ) return;

/* Get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(NULL);

/* Initialize the component of the virtual function table used by the
   parent class. */
   astInitFrameVtab( (AstFrameVtab *) vtab, name );

/* Store a unique "magic" value in the virtual function table. This
   will be used (by astIsAFrameSet) to determine if an object belongs
   to this class.  We can conveniently use the address of the (static)
   class_check variable to generate this unique value. */
   vtab->id.check = &class_check;
   vtab->id.parent = &(((AstFrameVtab *) vtab)->id);

/* Initialise member function pointers. */
/* ------------------------------------ */
/* Store pointers to the member functions (implemented here) that
   provide virtual methods for this class. */
   vtab->AddFrame = AddFrame;
   vtab->ClearBase = ClearBase;
   vtab->ClearCurrent = ClearCurrent;
   vtab->GetBase = GetBase;
   vtab->GetCurrent = GetCurrent;
   vtab->GetFrame = GetFrame;
   vtab->GetMapping = GetMapping;
   vtab->GetNframe = GetNframe;
   vtab->RemapFrame = RemapFrame;
   vtab->RemoveFrame = RemoveFrame;
   vtab->SetBase = SetBase;
   vtab->SetCurrent = SetCurrent;
   vtab->TestBase = TestBase;
   vtab->TestCurrent = TestCurrent;
   vtab->ValidateFrameIndex = ValidateFrameIndex;

/* Save the inherited pointers to methods that will be extended, and
   replace them with pointers to the new member functions. */
   object = (AstObjectVtab *) vtab;

   parent_clear = object->Clear;
   parent_getobjsize = object->GetObjSize;
   object->GetObjSize = GetObjSize;
   object->Clear = Clear;

   parent_vset = object->VSet;
   object->VSet = VSet;

   parent_getusedefs = object->GetUseDefs;
   object->GetUseDefs = GetUseDefs;

#if defined(THREAD_SAFE)
   parent_managelock = object->ManageLock;
   object->ManageLock = ManageLock;
#endif

/* Store replacement pointers for methods which will be over-ridden by
   new member functions implemented here. */
   mapping = (AstMappingVtab *) vtab;
   frame = (AstFrameVtab *) vtab;

   object->ClearAttrib = ClearAttrib;
   object->GetAttrib = GetAttrib;
   object->SetAttrib = SetAttrib;
   object->TestAttrib = TestAttrib;

   object->GetUseDefs = GetUseDefs;
   object->Equal = Equal;
   object->Cast = Cast;

   mapping->GetIsLinear = GetIsLinear;
   mapping->GetNin = GetNin;
   mapping->GetNout = GetNout;
   mapping->GetTranForward = GetTranForward;
   mapping->GetTranInverse = GetTranInverse;
   mapping->Rate = Rate;
   mapping->ReportPoints = ReportPoints;
   mapping->RemoveRegions = RemoveRegions;
   mapping->Simplify = Simplify;
   mapping->Transform = Transform;
   mapping->MapSplit = MapSplit;

   frame->Abbrev = Abbrev;
   frame->Angle = Angle;
   frame->AxAngle = AxAngle;
   frame->AxDistance = AxDistance;
   frame->AxOffset = AxOffset;
   frame->CheckPerm = CheckPerm;
   frame->ClearDigits = ClearDigits;
   frame->ClearDirection = ClearDirection;
   frame->ClearDomain = ClearDomain;
   frame->ClearFormat = ClearFormat;
   frame->ClearLabel = ClearLabel;
   frame->ClearMatchEnd = ClearMatchEnd;
   frame->ClearMaxAxes = ClearMaxAxes;
   frame->ClearMinAxes = ClearMinAxes;
   frame->ClearPermute = ClearPermute;
   frame->ClearPreserveAxes = ClearPreserveAxes;
   frame->ClearSymbol = ClearSymbol;
   frame->ClearTitle = ClearTitle;
   frame->ClearUnit = ClearUnit;
   frame->Convert = Convert;
   frame->ConvertX = ConvertX;
   frame->Distance = Distance;
   frame->Fields = Fields;
   frame->FindFrame = FindFrame;
   frame->Format = Format;
   frame->FrameGrid = FrameGrid;
   frame->Gap = Gap;
   frame->GetAxis = GetAxis;
   frame->GetDigits = GetDigits;
   frame->GetDirection = GetDirection;
   frame->GetDomain = GetDomain;
   frame->GetFormat = GetFormat;
   frame->GetLabel = GetLabel;
   frame->GetMatchEnd = GetMatchEnd;
   frame->GetMaxAxes = GetMaxAxes;
   frame->GetMinAxes = GetMinAxes;
   frame->GetNaxes = GetNaxes;
   frame->GetPerm = GetPerm;
   frame->GetPermute = GetPermute;
   frame->GetPreserveAxes = GetPreserveAxes;
   frame->GetSymbol = GetSymbol;
   frame->GetTitle = GetTitle;
   frame->GetUnit = GetUnit;
   frame->Intersect = Intersect;
   frame->IsUnitFrame = IsUnitFrame;
   frame->LineContains = LineContains;
   frame->LineCrossing = LineCrossing;
   frame->LineDef = LineDef;
   frame->LineOffset = LineOffset;
   frame->Match = Match;
   frame->MatchAxes = MatchAxes;
   frame->MatchAxesX = MatchAxesX;
   frame->Norm = Norm;
   frame->NormBox = NormBox;
   frame->Offset = Offset;
   frame->Offset2 = Offset2;
   frame->Overlay = Overlay;
   frame->PermAxes = PermAxes;
   frame->PickAxes = PickAxes;
   frame->PrimaryFrame = PrimaryFrame;
   frame->Resolve = Resolve;
   frame->ResolvePoints = ResolvePoints;
   frame->SetAxis = SetAxis;
   frame->SetDigits = SetDigits;
   frame->SetDirection = SetDirection;
   frame->SetDomain = SetDomain;
   frame->SetFormat = SetFormat;
   frame->SetLabel = SetLabel;
   frame->SetMatchEnd = SetMatchEnd;
   frame->SetMaxAxes = SetMaxAxes;
   frame->SetMinAxes = SetMinAxes;
   frame->SetPermute = SetPermute;
   frame->SetPreserveAxes = SetPreserveAxes;
   frame->SetSymbol = SetSymbol;
   frame->SetTitle = SetTitle;
   frame->SetUnit = SetUnit;
   frame->SubFrame = SubFrame;
   frame->SystemCode = SystemCode;
   frame->SystemString = SystemString;
   frame->TestDigits = TestDigits;
   frame->TestDirection = TestDirection;
   frame->TestDomain = TestDomain;
   frame->TestFormat = TestFormat;
   frame->TestLabel = TestLabel;
   frame->TestMatchEnd = TestMatchEnd;
   frame->TestMaxAxes = TestMaxAxes;
   frame->TestMinAxes = TestMinAxes;
   frame->TestPermute = TestPermute;
   frame->TestPreserveAxes = TestPreserveAxes;
   frame->TestSymbol = TestSymbol;
   frame->TestTitle = TestTitle;
   frame->TestUnit = TestUnit;
   frame->Unformat = Unformat;
   frame->ValidateAxis = ValidateAxis;
   frame->ValidateAxisSelection = ValidateAxisSelection;
   frame->ValidateSystem = ValidateSystem;

   frame->GetActiveUnit = GetActiveUnit;
   frame->SetActiveUnit = SetActiveUnit;
   frame->TestActiveUnit = TestActiveUnit;

   frame->GetTop = GetTop;
   frame->SetTop = SetTop;
   frame->TestTop = TestTop;
   frame->ClearTop = ClearTop;

   frame->GetBottom = GetBottom;
   frame->SetBottom = SetBottom;
   frame->TestBottom = TestBottom;
   frame->ClearBottom = ClearBottom;

   frame->GetEpoch = GetEpoch;
   frame->SetEpoch = SetEpoch;
   frame->TestEpoch = TestEpoch;
   frame->ClearEpoch = ClearEpoch;

   frame->GetSystem = GetSystem;
   frame->SetSystem = SetSystem;
   frame->TestSystem = TestSystem;
   frame->ClearSystem = ClearSystem;

   frame->GetAlignSystem = GetAlignSystem;
   frame->SetAlignSystem = SetAlignSystem;
   frame->TestAlignSystem = TestAlignSystem;
   frame->ClearAlignSystem = ClearAlignSystem;

   frame->ClearObsLat = ClearObsLat;
   frame->TestObsLat = TestObsLat;
   frame->GetObsLat = GetObsLat;
   frame->SetObsLat = SetObsLat;

   frame->ClearObsAlt = ClearObsAlt;
   frame->TestObsAlt = TestObsAlt;
   frame->GetObsAlt = GetObsAlt;
   frame->SetObsAlt = SetObsAlt;

   frame->ClearObsLon = ClearObsLon;
   frame->TestObsLon = TestObsLon;
   frame->GetObsLon = GetObsLon;
   frame->SetObsLon = SetObsLon;

/* Declare the copy constructor, destructor and class dump
   functions. */
   astSetDelete( vtab, Delete );
   astSetCopy( vtab, Copy );
   astSetDump( vtab, Dump, "FrameSet",
               "Set of inter-related coordinate systems" );

/* If we have just initialised the vtab for the current class, indicate
   that the vtab is now initialised, and store a pointer to the class
   identifier in the base "object" level of the vtab. */
   if( vtab == &class_vtab ) {
      class_init = 1;
      astSetVtabClassIdentifier( vtab, &(vtab->id) );
   }
}

static void Intersect( AstFrame *this_frame, const double a1[2],
                       const double a2[2], const double b1[2],
                       const double b2[2], double cross[2],
                       int *status ) {
/*
*  Name:
*     Intersect

*  Purpose:
*     Find the point of intersection between two geodesic curves.

*  Type:
*     Private function.

*  Synopsis:
*     #include "frameset.h"
*     void Intersect( AstFrame *this_frame, const double a1[2],
*                      const double a2[2], const double b1[2],
*                      const double b2[2], double cross[2],
*                      int *status )

*  Class Membership:
*     FrameSet member function (over-rides the astIntersect method
*     inherited from the Frame class).

*  Description:
*     This function finds the coordinate values at the point of
*     intersection between two geodesic curves. Each curve is specified
*     by two points on the curve.

*  Parameters:
*     this
*        Pointer to the SkyFrame.
*     a1
*        An array of double, with one element for each Frame axis.
*        This should contain the coordinates of a point on the first
*        geodesic curve.
*     a2
*        An array of double, with one element for each Frame axis.
*        This should contain the coordinates of a second point on the
*        first geodesic curve.
*     b1
*        An array of double, with one element for each Frame axis.
*        This should contain the coordinates of a point on the second
*        geodesic curve.
*     b2
*        An array of double, with one element for each Frame axis.
*        This should contain the coordinates of a second point on
*        the second geodesic curve.
*     cross
*        An array of double, with one element for each Frame axis
*        in which the coordinates of the required intersection
*        point will be returned. These will be AST__BAD if the curves do
*        not intersect.
*     status
*        Pointer to the inherited status variable.

*  Notes:
*     - The geodesic curve used by this function is the path of
*     shortest distance between two points, as defined by the
*     astDistance function.
*     - This function will return "bad" coordinate values (AST__BAD)
*     if any of the input coordinates has this value.
*     - For SkyFrames each curve will be a great circle, and in general
*     each pair of curves will intersect at two diametrically opposite
*     points on the sky. The returned position is the one which is
*     closest to point "a1".
*/

/* Local Variables: */
   AstFrame *fr;                 /* Pointer to current Frame */
   AstFrameSet *this;            /* Pointer to the FrameSet structure */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain a pointer to the FrameSet structure. */
   this = (AstFrameSet *) this_frame;

/* Obtain a pointer to the FrameSet's current Frame and invoke the
   astIntersect method for this Frame. Annul the Frame pointer
   afterwards. */
   fr = astGetFrame( this, AST__CURRENT );
   astIntersect( fr, a1, a2, b1, b2, cross );
   fr = astAnnul( fr );

}

static int IsUnitFrame( AstFrame *this_frame, int *status ){
/*
*  Name:
*     IsUnitFrame

*  Purpose:
*     Is this Frame equivalent to a UnitMap?

*  Type:
*     Private function.

*  Synopsis:
*     #include "frameset.h"
*     int IsUnitFrame( AstFrame *this, int *status )

*  Class Membership:
*     FrameSet member function (over-rides the protected astIsUnitFrame
*     method inherited from the Frame class).

*  Description:
*     This function returns a flag indicating if the supplied Frame is
*     equivalent to a UnitMap when treated as a Mapping (note, the Frame
*     class inherits from Mapping and therefore every Frame is also a Mapping).

*  Parameters:
*     this
*        Pointer to the Frame.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     A non-zero value is returned if the supplied Frame is equivalent to
*     a UnitMap when treated as a Mapping.

*-
*/

/* Local Variables: */
   AstFrame *fr;                 /* Pointer to FrameSet's current Frame */
   AstFrameSet *this;            /* Pointer to the FrameSet structure */
   int result;                   /* Result to be returned */

/* Initialise the returned value. */
   result = 0;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Obtain a pointer to the FrameSet structure. */
   this = (AstFrameSet *) this_frame;

/* Obtain a pointer to the FrameSet's current Frame. */
   fr = astGetFrame( this, AST__CURRENT );

/* Invoke the astIsUnitFrame method for this Frame. */
   result = astIsUnitFrame( fr );

/* Annul the Frame pointer. */
   fr = astAnnul( fr );

/* If an error occurred, clean up by clearing the returned result. */
   if ( !astOK ) result = 0;

/* Return the result. */
   return result;
}

static int LineContains( AstFrame *this_frame, AstLineDef *l, int def, double *point, int *status ) {
/*
*  Name:
*     LineContains

*  Purpose:
*     Determine if a line contains a point.

*  Type:
*     Private function.

*  Synopsis:
*     #include "frameset.h"
*     int LineContains( AstFrame *this, AstLineDef *l, int def, double *point, int *status )

*  Class Membership:
*     FrameSet member function (over-rides the protected astLineContains
*     method inherited from the Frame class).

*  Description:
*     This function determines if the supplied point is on the supplied
*     line within the supplied Frame.

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
*     status
*        Pointer to the inherited status variable.

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
*/

/* Local Variables: */
   AstFrame *fr;                 /* Pointer to current Frame */
   int result;                   /* Returned value */

/* Initialise */
   result =0;

/* Obtain a pointer to the FrameSet's current Frame and then invoke the
   method. Annul the Frame pointer afterwards. */
   fr = astGetFrame( (AstFrameSet *) this_frame, AST__CURRENT );
   result = astLineContains( fr, l, def, point );
   fr = astAnnul( fr );

/* Return the result. */
   return result;
}

static int LineCrossing( AstFrame *this_frame, AstLineDef *l1, AstLineDef *l2,
                         double **cross, int *status ) {
/*
*  Name:
*     LineCrossing

*  Purpose:
*     Determine if two lines cross.

*  Type:
*     Private function.

*  Synopsis:
*     #include "frameset.h"
*     int LineCrossing( AstFrame *this, AstLineDef *l1, AstLineDef *l2,
*                       double **cross, int *status )

*  Class Membership:
*     FrameSet member function (over-rides the protected astLineCrossing
*     method inherited from the Frame class).

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
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     A non-zero value is returned if the lines cross at a point which is
*     within the [start,end) segment of both lines. If the crossing point
*     is outside this segment on either line, or if the lines are parallel,
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
*/

/* Local Variables: */
   AstFrame *fr;                 /* Pointer to current Frame */
   int result;                   /* Returned value */

/* Initialise */
   result =0;

/* Obtain a pointer to the FrameSet's current Frame and then invoke the
   method. Annul the Frame pointer afterwards. */
   fr = astGetFrame( (AstFrameSet *) this_frame, AST__CURRENT );
   result = astLineCrossing( fr, l1, l2, cross );
   fr = astAnnul( fr );

/* Return the result. */
   return result;
}

static AstLineDef *LineDef( AstFrame *this_frame, const double start[2],
                            const double end[2], int *status ) {
/*
*  Name:
*     LineDef

*  Purpose:
*     Creates a structure describing a line segment in a 2D Frame.

*  Type:
*     Private function.

*  Synopsis:
*     #include "frameset.h"
*     AstLineDef *LineDef( AstFrame *this, const double start[2],
*                             const double end[2], int *status )

*  Class Membership:
*     FrameSet member function (over-rides the protected astLineDef
*     method inherited from the Frame class).

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
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     Pointer to the memory structure containing the description of the
*     line. This structure should be freed using astFree when no longer
*     needed. A NULL pointer is returned (without error) if any of the
*     supplied axis values are AST__BAD.

*  Notes:
*     - A null pointer will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*/

/* Local Variables: */
   AstFrame *fr;                 /* Pointer to current Frame */
   AstLineDef *result;           /* Returned value */

/* Initialise */
   result = NULL;

/* Obtain a pointer to the FrameSet's current Frame and then invoke the
   method. Annul the Frame pointer afterwards. */
   fr = astGetFrame( (AstFrameSet *) this_frame, AST__CURRENT );
   result = astLineDef( fr, start, end );
   fr = astAnnul( fr );

/* Return the result. */
   return result;
}

static void LineOffset( AstFrame *this_frame, AstLineDef *line, double par,
                        double prp, double point[2], int *status ){
/*
*  Name:
*     LineOffset

*  Purpose:
*     Find a position close to a line.

*  Type:
*     Private function.

*  Synopsis:
*     #include "frame.h"
*     void LineOffset( AstFrame *this, AstLineDef *line, double par,
*                      double prp, double point[2], int *status )

*  Class Membership:
*     FrameSet member function (over-rides the protected astLineOffset
*     method inherited from the Frame class).

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
*     status
*        Pointer to the inherited status variable.

*  Notes:
*     - The pointer supplied for "line" should have been created using the
*     astLineDef method. This structure contains cached information about the
*     line which improves the efficiency of this method when many repeated
*     calls are made. An error will be reported if the structure does not
*     refer to the Frame specified by "this".
*/


/* Local Variables: */
   AstFrame *fr;                 /* Pointer to current Frame */

/* Obtain a pointer to the FrameSet's current Frame and then invoke the
   method. Annul the Frame pointer afterwards. */
   fr = astGetFrame( (AstFrameSet *) this_frame, AST__CURRENT );
   astLineOffset( fr, line, par, prp, point );
   fr = astAnnul( fr );
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
*     FrameSet member function (over-rides the astManageLock protected
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
   AstFrameSet *this;    /* Pointer to FrameSet structure */
   int i;                /* Loop count */
   int result;           /* Returned status value */

/* Initialise */
   result = 0;

/* Check the supplied pointer is not NULL. */
   if( !this_object ) return result;

/* Obtain a pointers to the FrameSet structure. */
   this = (AstFrameSet *) this_object;

/* Invoke the ManageLock method inherited from the parent class. */
   if( !result ) result = (*parent_managelock)( this_object, mode, extra,
                                                fail, status );

/* Invoke the astManageLock method on any Objects contained within
   the supplied Object. */
   for( i = 0; i < this->nframe; i++ ) {
      if( !result ) result = astManageLock( this->frame[ i ], mode,
                                            extra, fail );
   }

   for ( i = 0; i < this->nnode - 1; i++ ) {
      if( !result ) result = astManageLock( this->map[ i ], mode, extra,
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
*     FrameSet.

*  Type:
*     Private function.

*  Synopsis:
*     #include "frameset.h"
*     int *MapSplit( AstMapping *this, int nin, const int *in, AstMapping **map, int *status )

*  Class Membership:
*     FrameSet method (over-rides the protected astMapSplit method
*     inherited from the Mapping class).

*  Description:
*     This function creates a new Mapping by picking specified inputs from
*     an existing FrameSet. This is only possible if the specified inputs
*     correspond to some subset of the FrameSet outputs. That is, there
*     must exist a subset of the FrameSet outputs for which each output
*     depends only on the selected FrameSet inputs, and not on any of the
*     inputs which have not been selected. If this condition is not met
*     by the supplied FrameSet, then a NULL Mapping is returned.

*  Parameters:
*     this
*        Pointer to the FrameSet to be split (the FrameSet is not actually
*        modified by this function).
*     nin
*        The number of inputs to pick from "this".
*     in
*        Pointer to an array of indices (zero based) for the inputs which
*        are to be picked. This array should have "nin" elements. If "Nin"
*        is the number of inputs of the supplied FrameSet, then each element
*        should have a value in the range zero to Nin-1.
*     map
*        Address of a location at which to return a pointer to the new
*        Mapping. This Mapping will have "nin" inputs (the number of
*        outputs may be different to "nin"). A NULL pointer will be
*        returned if the supplied FrameSet has no subset of outputs which
*        depend only on the selected inputs.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     A pointer to a dynamically allocated array of ints. The number of
*     elements in this array will equal the number of outputs for the
*     returned Mapping. Each element will hold the index of the
*     corresponding output in the supplied FrameSet. The array should be
*     freed using astFree when no longer needed. A NULL pointer will
*     be returned if no output Mapping can be created.

*  Notes:
*     - If this function is invoked with the global error status set,
*     or if it should fail for any reason, then NULL values will be
*     returned as the function value and for the "map" pointer.
*/

/* Local Variables: */
   AstMapping *bcmap;   /* Base->current Mapping */
   int *result;         /* Returned pointer */

/* Initialise */
   result = NULL;
   *map = NULL;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Get the Mapping from base to current Frame and try to split it. */
   bcmap = astGetMapping( (AstFrameSet *) this_map, AST__BASE, AST__CURRENT );
   result = astMapSplit( bcmap, nin, in, map );
   bcmap = astAnnul( bcmap );

/* Free returned resources if an error has occurred. */
   if( !astOK ) {
      result = astFree( result );
      *map = astAnnul( *map );
   }

/* Return the list of output indices. */
   return result;
}

static int Match( AstFrame *this_frame, AstFrame *target, int matchsub,
                  int **template_axes, int **target_axes,
                  AstMapping **map, AstFrame **result, int *status ) {
/*
*  Name:
*     Match

*  Purpose:
*     Determine if conversion is possible between two coordinate systems.

*  Type:
*     Private function.

*  Synopsis:
*     #include "frameset.h"
*     int Match( AstFrame *template, AstFrame *target, int matchsub,
*                int **template_axes, int **target_axes,
*                AstMapping **map, AstFrame **result, int *status )

*  Class Membership:
*     FrameSet member function (over-rides the protected astMatch
*     method inherited from the Frame class).

*  Description:
*     This function matches the current Frame of a "template" FrameSet
*     to a "target" frame and determines whether it is possible to
*     convert coordinates between them.  If it is, a Mapping that
*     performs the transformation is returned along with a new Frame
*     that describes the coordinate system that results when this
*     Mapping is applied to the current Frame of the target
*     FrameSet. In addition, information is returned to allow the axes
*     in this "result" Frame to be associated with the corresponding
*     axes in the target and template Frames from which they are
*     derived.

*  Parameters:
*     template
*        Pointer to the template FrameSet, whose current Frame
*        describes the coordinate system (or set of possible
*        coordinate systems) into which we wish to convert our
*        coordinates.
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
*        of this array will return the index of the axis in the
*        template FrameSet's current Frame from which it is
*        derived. If it is not derived from any template FrameSet
*        axis, a value of -1 will be returned instead.
*     target_axes
*        Address of a location where a pointer to int will be returned
*        if the requested coordinate conversion is possible. This
*        pointer will point at a dynamically allocated array of
*        integers with one element for each axis of the "result" Frame
*        (see below). It must be freed by the caller (using astFree)
*        when no longer required.
*
*        For each axis in the result Frame, the corresponding element
*        of this array will return the index of the target Frame axis
*        from which it is derived. If it is not derived from any
*        target Frame axis, a value of -1 will be returned instead.
*     map
*        Address of a location where a pointer to a new Mapping will
*        be returned if the requested coordinate conversion is
*        possible. If returned, the forward transformation of this
*        Mapping may be used to convert coordinates between the target
*        Frame and the result Frame (see below) and the inverse
*        transformation will convert in the opposite direction.
*     result
*        Address of a location where a pointer to a new Frame will be
*        returned if the requested coordinate conversion is
*        possible. If returned, this Frame describes the coordinate
*        system that results from applying the returned Mapping
*        (above) to the "target" coordinate system. In general, this
*        Frame will combine attributes from (and will therefore be
*        more specific than) both the target Frame and the current
*        Frame of the template FrameSet. In particular, when the
*        template allows the possibility of transformaing to any one
*        of a set of alternative coordinate systems, the "result"
*        Frame will indicate which of the alternatives was used.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     A non-zero value is returned if the requested coordinate
*     conversion is possible. Otherwise zero is returned (this will
*     not in itself result in an error condition).

*  Notes:
*     - A value of zero will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*/

/* Local Variables: */
   AstFrame *fr;                 /* Pointer to FrameSet's current Frame */
   AstFrameSet *this;            /* Pointer to the FrameSet structure */
   int match;                    /* Result to be returned */

/* Initialise the returned values. */
   *template_axes = NULL;
   *target_axes = NULL;
   *map = NULL;
   *result = NULL;
   match = 0;

/* Check the global error status. */
   if ( !astOK ) return match;

/* Obtain a pointer to the FrameSet structure. */
   this = (AstFrameSet *) this_frame;

/* Obtain a pointer to the FrameSet's current Frame. */
   fr = astGetFrame( this, AST__CURRENT );

/* Invoke the astMatch method for this Frame. */
   match =astMatch( fr, target, matchsub, template_axes, target_axes,
                    map, result );

/* Annul the Frame pointer. */
   fr = astAnnul( fr );

/* If an error occurred, clean up by freeing any allocated memory,
   annulling returned objects and clearing the returned result. */
   if ( !astOK ) {
      *template_axes = astFree( *template_axes );
      *target_axes = astFree( *target_axes );
      *map = astAnnul( *map );
      *result = astAnnul( *result );
      match = 0;
   }

/* Return the result. */
   return match;
}

static void MatchAxes( AstFrame *frm1_frame, AstFrame *frm2, int *axes,
                       int *status ) {
/*
*  Name:
*     MatchAxes

*  Purpose:
*     Find any corresponding axes in two Frames.

*  Type:
*     Private function.

*  Synopsis:
*     #include "frameset.h"
*     void MatchAxes( AstFrame *frm1, AstFrame *frm2, int *axes )
*                     int *status )

*  Class Membership:
*     FrameSet member function (over-rides the protected astMatchAxes
*     method inherited from the Frame class).

*  Description:
*     This function looks for corresponding axes within two supplied
*     Frames. An array of integers is returned that contains an element
*     for each axis in the second supplied Frame. An element in this array
*     will be set to zero if the associated axis within the second Frame
*     has no corresponding axis within the first Frame. Otherwise, it
*     will be set to the index (a non-zero positive integer) of the
*     corresponding axis within the first supplied Frame.

*  Parameters:
*     frm1
*        Pointer to the first Frame.
*     frm2
*        Pointer to the second Frame.
*     axes
*        Pointer to an
*        integer array in which to return the indices of the axes (within
*        the second Frame) that correspond to each axis within the first
*        Frame. Axis indices start at 1. A value of zero will be stored
*        in the returned array for each axis in the first Frame that has
*        no corresponding axis in the second Frame.
*
*        The number of elements in this array must be greater than or
*        equal to the number of axes in the first Frame.
*     status
*        Pointer to inherited status value.

*  Notes:
*     -  Corresponding axes are identified by the fact that a Mapping
*     can be found between them using astFindFrame or astConvert. Thus,
*     "corresponding axes" are not necessarily identical. For instance,
*     SkyFrame axes in two Frames will match even if they describe
*     different celestial coordinate systems
*/

/* Local Variables: */
   AstFrame *frm1;

/* Check the global error status. */
   if ( !astOK ) return;

/* Get a pointer to the current Frame in the FrameSet. */
   frm1 = astGetFrame( (AstFrameSet *) frm1_frame, AST__CURRENT );

/* Invoke the astMatchAxesX on the second Frame. */
   astMatchAxesX( frm2, frm1, axes );

/* Free resources */
   frm1 = astAnnul( frm1 );
}

static void MatchAxesX( AstFrame *frm2_frame, AstFrame *frm1, int *axes,
                        int *status ) {
/*
*  Name:
*     MatchAxesX

*  Purpose:
*     Find any corresponding axes in two Frames.

*  Type:
*     Private function.

*  Synopsis:
*     #include "frameset.h"
*     void MatchAxesX( AstFrame *frm2, AstFrame *frm1, int *axes )
*                      int *status )

*  Class Membership:
*     FrameSet member function (over-rides the protected astMatchAxesX
*     method inherited from the Frame class).

*     This function looks for corresponding axes within two supplied
*     Frames. An array of integers is returned that contains an element
*     for each axis in the second supplied Frame. An element in this array
*     will be set to zero if the associated axis within the second Frame
*     has no corresponding axis within the first Frame. Otherwise, it
*     will be set to the index (a non-zero positive integer) of the
*     corresponding axis within the first supplied Frame.

*  Parameters:
*     frm2
*        Pointer to the second Frame.
*     frm1
*        Pointer to the first Frame.
*     axes
*        Pointer to an integer array in which to return the indices of
*        the axes (within the first Frame) that correspond to each axis
*        within the second Frame. Axis indices start at 1. A value of zero
*        will be stored in the returned array for each axis in the second
*        Frame that has no corresponding axis in the first Frame.
*
*        The number of elements in this array must be greater than or
*        equal to the number of axes in the second Frame.
*     status
*        Pointer to inherited status value.

*  Notes:
*     -  Corresponding axes are identified by the fact that a Mapping
*     can be found between them using astFindFrame or astConvert. Thus,
*     "corresponding axes" are not necessarily identical. For instance,
*     SkyFrame axes in two Frames will match even if they describe
*     different celestial coordinate systems
*/

/* Local Variables: */
   AstFrame *frm2;

/* Check the global error status. */
   if ( !astOK ) return;

/* Get a pointer to the current Frame in the FrameSet. */
   frm2 = astGetFrame( (AstFrameSet *) frm2_frame, AST__CURRENT );

/* Invoke the astMatchAxesX on the current Frame. */
   astMatchAxesX( frm2, frm1, axes );

/* Free resources */
   frm2 = astAnnul( frm2 );
}

static void Norm( AstFrame *this_frame, double value[], int *status ) {
/*
*  Name:
*     Norm

*  Purpose:
*     Normalise a set of FrameSet coordinates.

*  Type:
*     Private function.

*  Synopsis:
*     #include "frameset.h"
*     void Norm( AstAxis *this, double value[], int *status )

*  Class Membership:
*     FrameSet member function (over-rides the astNorm method
*     inherited from the Frame class).

*  Description:
*     This function converts a set of coordinate values for the
*     current Frame of a FrameSet, which might potentially be
*     unsuitable for display to a user (for instance, may lie outside
*     the expected range of values) into a set of acceptable
*     alternative values suitable for display.
*
*     Typically, for Frames whose axes represent cyclic values (such
*     as angles or positions on the sky), this function wraps an
*     arbitrary set of coordinates, so that they lie within the first
*     cycle (say zero to 2*pi or -pi/2 to +pi/2). For Frames with
*     ordinary linear axes, without constraints, this function will
*     typically return the original coordinate values unchanged.

*  Parameters:
*     this
*        Pointer to the FrameSet.
*     value
*        An array of double, with one element for each FrameSet axis.
*        This should contain the initial set of coordinate values,
*        which will be modified in place.
*     status
*        Pointer to the inherited status variable.
*/

/* Local Variables: */
   AstFrame *fr;                 /* Pointer to the current Frame */
   AstFrameSet *this;            /* Pointer to the FrameSet structure */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain a pointer to the FrameSet structure. */
   this = (AstFrameSet *) this_frame;

/* Obtain a pointer to the FrameSet's current Frame and invoke this
   Frame's astNorm method to obtain the new values. Annul the Frame
   pointer afterwards. */
   fr = astGetFrame( this, AST__CURRENT );
   astNorm( fr, value );
   fr = astAnnul( fr );
}

static void NormBox( AstFrame *this_frame, double lbnd[], double ubnd[],
                     AstMapping *reg, int *status ) {
/*
*  Name:
*     NormBox

*  Purpose:
*     Extend a box to include effect of any singularities in the Frame.

*  Type:
*     Private function.

*  Synopsis:
*     #include "frameset.h"
*     void astNormBox( AstFrame *this, double lbnd[], double ubnd[],
*                      AstMapping *reg, int *status )

*  Class Membership:
*     FrameSet member function (over-rides the astNormBox method inherited
*     from the Frame class).

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
*     status
*        Pointer to the inherited status variable.
*/

/* Local Variables: */
   AstFrame *fr;                 /* Pointer to the current Frame */
   AstFrameSet *this;            /* Pointer to the FrameSet structure */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain a pointer to the FrameSet structure. */
   this = (AstFrameSet *) this_frame;

/* Obtain a pointer to the FrameSet's current Frame and invoke this
   Frame's astNormBox method to obtain the new values. Annul the Frame
   pointer afterwards. */
   fr = astGetFrame( this, AST__CURRENT );
   astNormBox( fr, lbnd, ubnd, reg );
   fr = astAnnul( fr );
}

static void Offset( AstFrame *this_frame, const double point1[],
                    const double point2[], double offset, double point3[], int *status ) {
/*
*  Name:
*     Offset

*  Purpose:
*     Calculate an offset along a geodesic curve.

*  Type:
*     Public virtual function.

*  Synopsis:
*     #include "frameset.h"
*     void Offset( AstFrame *this,
*                  const double point1[], const double point2[],
*                  double offset, double point3[], int *status )

*  Class Membership:
*     FrameSet member function (over-rides the protected astOffset
*     method inherited from the Frame class).

*  Description:
*     This function finds the FrameSet coordinate values of a point
*     which is offset a specified distance along the geodesic curve
*     between two other points.

*  Parameters:
*     this
*        Pointer to the FrameSet.
*     point1
*        An array of double, with one element for each FrameSet axis.
*        This should contain the coordinates of the point marking the
*        start of the geodesic curve.
*     point2
*        An array of double, with one element for each FrameSet axis
*        This should contain the coordinates of the point marking the
*        end of the geodesic curve.
*     offset
*        The required offset from the first point along the geodesic
*        curve. If this is positive, it will be towards the second
*        point. If it is negative, it will be in the opposite
*        direction. This offset need not imply a position lying
*        between the two points given, as the curve will be
*        extrapolated if necessary.
*     point3
*        An array of double, with one element for each FrameSet axis
*        in which the coordinates of the required point will be
*        returned.
*     status
*        Pointer to the inherited status variable.

*  Notes:
*     - The geodesic curve used by this function is the path of
*     shortest distance between two points, as defined by the
*     astDistance function.
*     - This function will return "bad" coordinate values (AST__BAD)
*     if any of the input coordinates has this value.
*     - "Bad" coordinate values will also be returned if the two
*     points supplied are coincident (or otherwise fail to uniquely
*     specify a geodesic curve) but the requested offset is non-zero.
*/

/* Local Variables: */
   AstFrame *fr;                 /* Pointer to current Frame */
   AstFrameSet *this;            /* Pointer to the FrameSet structure */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain a pointer to the FrameSet structure. */
   this = (AstFrameSet *) this_frame;

/* Obtain a pointer to the FrameSet's current Frame and invoke this
   Frame's astOffset method. Annul the Frame pointer afterwards. */
   fr = astGetFrame( this, AST__CURRENT );
   astOffset( fr, point1, point2, offset, point3 );
   fr = astAnnul( fr );
}

static double Offset2( AstFrame *this_frame, const double point1[2],
                       double angle, double offset, double point2[2], int *status ){
/*
*  Name:
*     Offset2

*  Purpose:
*     Calculate an offset along a geodesic curve in a 2D Frame.

*  Type:
*     Private function.

*  Synopsis:
*     #include "frameset.h"
*     double Offset2( AstFrame *this, const double point1[2], double angle,
*                     double offset, double point2[2], int *status );

*  Class Membership:
*     FrameSet member function (over-rides the protected astOffset2
*     method inherited from the Frame class).

*  Description:
*     This function finds the Frame coordinate values of a point which
*     is offset a specified distance along the geodesic curve at a
*     given angle from a specified starting point. It can only be
*     used with 2-dimensional Frames.
*
*     For example, in a basic Frame, this offset will be along the
*     straight line joining two points. For a more specialised Frame
*     describing a sky coordinate system, however, it would be along
*     the great circle passing through two sky positions.

*  Parameters:
*     this
*        Pointer to the Frame.
*     point1
*        An array of double, with one element for each Frame axis
*        (Naxes attribute). This should contain the coordinates of the
*        point marking the start of the geodesic curve.
*     angle
*        The angle (in radians) from the positive direction of the second
*        axis, to the direction of the required position, as seen from
*        the starting position. Positive rotation is in the sense of
*        rotation from the positive direction of axis 2 to the positive
*        direction of axis 1.
*     offset
*        The required offset from the first point along the geodesic
*        curve. If this is positive, it will be in the direction of the
*        given angle. If it is negative, it will be in the opposite
*        direction.
*     point2
*        An array of double, with one element for each Frame axis
*        in which the coordinates of the required point will be returned.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     The direction of the geodesic curve at the end point. That is, the
*     angle (in radians) between the positive direction of the second
*     axis and the continuation of the geodesic curve at the requested
*     end point. Positive rotation is in the sense of rotation from
*     the positive direction of axis 2 to the positive direction of axis 1.

*  Notes:
*     - The geodesic curve used by this function is the path of
*     shortest distance between two points, as defined by the
*     astDistance function.
*     - An error will be reported if the Frame is not 2-dimensional.
*     - This function will return "bad" coordinate values (AST__BAD)
*     if any of the input coordinates has this value.
*/

/* Local Variables: */
   AstFrame *fr;                 /* Pointer to current Frame */
   AstFrameSet *this;            /* Pointer to the FrameSet structure */
   double result;                /* Value to return */

/* Check the global error status. */
   if ( !astOK ) return AST__BAD;

/* Obtain a pointer to the FrameSet structure. */
   this = (AstFrameSet *) this_frame;

/* Obtain a pointer to the FrameSet's current Frame and invoke the
   astOffset2 method for this Frame. Annul the Frame pointer afterwards. */
   fr = astGetFrame( this, AST__CURRENT );
   result = astOffset2( fr, point1, angle, offset, point2 );
   fr = astAnnul( fr );

/* If an error occurred, clear the result value. */
   if ( !astOK ) result = AST__BAD;

/* Return the result. */
   return result;
}

static void Overlay( AstFrame *template_frame, const int *template_axes,
                     AstFrame *result, int *status ) {
/*
*  Name:
*     Overlay

*  Purpose:
*     Overlay the attributes of a template FrameSet on to another Frame.

*  Type:
*     Private function.

*  Synopsis:
*     #include "frameset.h"
*     void Overlay( AstFrame *template, const int *template_axes,
*                   AstFrame *result, int *status )

*  Class Membership:
*     FrameSet member function (over-rides the protected astOverlay
*     method inherited from the Frame class).

*  Description:
*     This function overlays attributes from the current Frame of a
*     FrameSet on to another Frame, so as to over-ride selected
*     attributes of that second Frame. Normally only those attributes
*     which have been specifically set in the template will be
*     transferred. This implements a form of defaulting, in which a
*     Frame acquires attributes from the template, but retains its
*     original attributes (as the default) if new values have not
*     previously been explicitly set in the template.

*  Parameters:
*     template
*        Pointer to the template FrameSet, for whose current Frame
*        values should have been explicitly set for any attribute
*        which is to be transferred.
*     template_axes
*        Pointer to an array of int, with one element for each axis of
*        the "result" Frame (see below). For each axis in the result
*        frame, the corresponding element of this array should contain
*        the (zero-based) index of the axis in the current Frame of
*        the template FrameSet to which it corresponds. This array is
*        used to establish from which template Frame axis any
*        axis-dependent attributes should be obtained.
*
*        If any axis in the result Frame is not associated with a
*        template Frame axis, the corresponding element of this array
*        should be set to -1.
*
*        If a NULL pointer is supplied, the template and result axis
*        indicies are assumed to be identical.
*     result
*        Pointer to the Frame which is to receive the new attribute values.
*     status
*        Pointer to the inherited status variable.
*/

/* Local Variables: */
   AstFrame *fr;                 /* Pointer to current Frame */
   AstFrameSet *template;        /* Pointer to the FrameSet structure */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain a pointer to the FrameSet structure. */
   template = (AstFrameSet *) template_frame;

/* Obtain a pointer to the current Frame and invoke its astOverlay
   method to overlay its attributes. Annul the Frame pointer
   afterwards. */
   fr = astGetFrame( template, AST__CURRENT );
   astOverlay( fr, template_axes, result );
   fr = astAnnul( fr );
}

static void PermAxes( AstFrame *this_frame, const int perm[], int *status ) {
/*
*  Name:
*     PermAxes

*  Purpose:
*     Permute the order of a FrameSet's axes.

*  Type:
*     Private function.

*  Synopsis:
*     #include "frameset.h"
*     void PermAxes( AstFrame *this, const int perm[], int *status )

*  Class Membership:
*     FrameSet member function (over-rides the astPermAxes method
*     inherited from the Frame class).

*  Description:
*     This function permutes the order in which the axes in the
*     current Frame of a FrameSet occur.

*  Parameters:
*     this
*        Pointer to the FrameSet.
*     perm
*        An array of int (with one element for each axis of the
*        FrameSet's current Frame) which lists the axes in their new
*        order. Each element of this array should be a (zero-based)
*        axis index identifying the axes according to their old
*        (un-permuted) order.
*     status
*        Pointer to the inherited status variable.

*  Notes:
*     - Only genuine permutations of the axis order are permitted, so
*     each axis must be referenced exactly once in the "perm" array.
*     - If more than one axis permutation is applied to the same Frame
*     in a FrameSet, the effects are cumulative.
*/

/* Local Variables: */
   AstFrame *fr;                 /* Pointer to current Frame */
   AstFrameSet *this;            /* Pointer to the FrameSet structure */
   AstPermMap *map;              /* Pointer to axis permutation Mapping */
   int *invperm;                 /* Pointer to inverse permutation array */
   int axis;                     /* Loop counter for axes */
   int naxes;                    /* Number of FrameSet axes */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain a pointer to the FrameSet structure. */
   this = (AstFrameSet *) this_frame;

/* Validate the permutation array, to check that it describes a
   genuine permutation. */
   astCheckPerm( this, perm, "astPermAxes" );

/* Obtain a pointer to the FrameSet's current Frame and invoke this
   Frame's astPermAxes method to permute its axes. Annul the Frame
   pointer afterwards. */
   fr = astGetFrame( this, AST__CURRENT );
   astPermAxes( fr, perm );
   fr = astAnnul( fr );

/* Obtain the number of axes in the FrameSet's current Frame and allocate
   memory to hold an inverse permutation array. */
   naxes =  astGetNaxes( this );
   invperm = astMalloc( sizeof( int ) * (size_t) naxes );

/* Fill the inverse permutation array with values that will invert the
   axis permutation supplied. */
   if ( astOK ) {
      for ( axis = 0; axis < naxes; axis++ ) invperm[ perm[ axis ] ] = axis;

/* Create a PermMap that will permute coordinate values in the same way as
   the current Frame's axes have been permuted. */
      map = astPermMap( naxes, invperm, naxes, perm, NULL, "", status );

/* Modify the Frame's relationship to the rest of the Frames in the
   FrameSet so that the correct coordinate values remain associated
   with the permuted axes. */
      astRemapFrame( this, AST__CURRENT, map );

/* Annul the PermMap and free the inverse permutation array. */
      map = astAnnul( map );
   }
   invperm = astFree( invperm );
}

static AstFrame *PickAxes( AstFrame *this_frame, int naxes, const int axes[],
                           AstMapping **map, int *status ) {
/*
*  Name:
*     PickAxes

*  Purpose:
*     Create a new Frame by picking axes from a FrameSet.

*  Type:
*     Private function.

*  Synopsis:
*     #include "frameset.h"
*     AstFrame *PickAxes( AstFrame *this, int naxes, const int axes[],
*                         AstMapping **map, int *status )

*  Class Membership:
*     FrameSet member function (over-rides the astPickAxes protected
*     method inherited from the Frame class).

*  Description:
*     This function creates a new Frame whose axes are copies of axes
*     picked from the current Frame of an existing FrameSet. Other
*     Frame attributes are also copied from this current Frame to the
*     new Frame. Zero or more of the original axes may be picked in
*     any order, but each can be used only once. Additional axes (with
*     default characteristics) may be included in the new Frame if
*     required.
*
*     Optionally, a Mapping that converts between the original Frame's
*     axes and those of the new Frame may also be returned.

*  Parameters:
*     this
*        Pointer to the FrameSet.
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
*        place between the current Frame of the FrameSet and the new
*        Frame.  The forward transformation will convert from the
*        original FrameSet's axes to the new one's, and vice versa.
*
*        If this Mapping is not required, a NULL value may be supplied
*        for this parameter.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     Pointer to the new Frame.

*  Notes:
*     - The class of object returned may differ from that of the
*     original current Frame, depending on which axes are
*     selected. For example, if a single axis is picked from a
*     SkyFrame (which always has two axes), the resulting Frame cannot
*     be a valid SkyFrame, so will revert to the parent class (Frame)
*     instead.
*     - The new Frame contains a deep copy of all the data selected
*     from the original current Frame. Modifying the new Frame will
*     therefore not affect the FrameSet or the Frames it contains.
*     - A NULL pointer will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*/

/* Local Variables: */
   AstFrame *fr;                 /* Pointer to current Frame */
   AstFrame *frame;              /* Pointer to Frame to be returned */
   AstFrameSet *this;            /* Pointer to FrameSet structure */

/* Initialise the returned pointers. */
   if ( map ) *map = NULL;
   frame = NULL;

/* Check the global error status. */
   if ( !astOK ) return frame;

/* Obtain a pointer to the FrameSet structure. */
   this = (AstFrameSet *) this_frame;

/* Check that a valid set of axes is being selected . */
   astValidateAxisSelection( this, naxes, axes, "astPickAxes" );

/* Obtain a pointer to the FrameSet's current Frame and use its
   astPickAxes method to obtain the required new Frame and
   Mapping. Annul the current Frame pointer afterwards. */
   fr = astGetFrame( this, AST__CURRENT );
   frame = astPickAxes( fr, naxes, axes, map );
   fr = astAnnul( fr );

/* If an error occurred, annul the Mapping pointer (if requested) and
   the new Frame pointer. */
   if ( !astOK ) {
      if ( map ) *map = astAnnul( *map );
      frame = astAnnul( frame );
   }

/* Return the pointer to the new Frame. */
   return frame;
}

static void PrimaryFrame( AstFrame *this_frame, int axis1,
                          AstFrame **frame, int *axis2, int *status ) {
/*
*  Name:
*     PrimaryFrame

*  Purpose:
*     Uniquely identify a primary Frame and one of its axes.

*  Type:
*     Private function.

*  Synopsis:
*     #include "frameset.h"
*     void PrimaryFrame( AstFrame *this, int axis1, AstFrame **frame,
*                        int *axis2, int *status )

*  Class Membership:
*     FrameSet member function (over-rides the protected
*     astPrimaryFrame method inherited from the Frame class).

*  Description:
*     This function returns information about the underlying (primary)
*     Frame corresponding to a specified axis of the current Frame of
*     a FrameSet, when this current Frame may be a compound Frame
*     composed of more than one simpler one.

*  Parameters:
*     this
*        Pointer to the FrameSet.
*     axis1
*        An axis index (zero-based) identifying the axis of the
*        FrameSet's current Frame for which information is required.
*     frame
*        Address of a location to receive a pointer to the underlying
*        (primary) frame to which the requested axis belongs
*        (i.e. this will not be a compound Frame).
*     axis2
*        Pointer to an int which is to receive the axis index within
*        "frame" which identifies the axis being referred to, using
*        the axis order that applied when the primary Frame was
*        originally constructed (i.e. this function undoes all
*        subsequent axis pemutations and the effects of combining
*        Frames, in order to reveal the original underlying axis
*        order).
*     status
*        Pointer to the inherited status variable.

*  Notes:
*     - This protected method is provided so that class
*     implementations can distinguish the axes of Frames from one
*     another (e.g. can distinguish a longitude axis as being
*     different from a latitide axis) even after their order has been
*     permuted and they have been combined with axes from other
*     Frames.
*     - The reference count of the primary Frame will be incremented
*     by one to reflect the new pointer returned.
*/

/* Local Variables: */
   AstFrame *fr;                 /* Pointer to current Frame */
   AstFrameSet *this;            /* Pointer to the FrameSet structure */

/* Initialise the returned values. */
   *frame = NULL;
   *axis2 = 0;

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain a pointer to the FrameSet structure. */
   this = (AstFrameSet *) this_frame;

/* Validate the axis index supplied. */
   (void) astValidateAxis( this, axis1, 1, "astPrimaryFrame" );

/* Obtain a pointer to the FrameSet's current Frame and invoke its
   astPrimaryFrame method. Annul the Frame pointer afterwards. */
   fr = astGetFrame( this, AST__CURRENT );
   astPrimaryFrame( fr, axis1, frame, axis2 );
   fr = astAnnul( fr );

/* If an error occurred, annul the returned object and clear the
   returned axis value. */
   if ( !astOK ) {
      *frame = astAnnul( *frame );
      *axis2 = 0;
   }
}

static double Rate( AstMapping *this_mapping, double *at, int ax1, int ax2, int *status ){
/*
*  Name:
*     Rate

*  Purpose:
*     Calculate the rate of change of a Mapping output.

*  Type:
*     Private function.

*  Synopsis:
*     #include "frameset.h"
*     result = Rate( AstMapping *this, double *at, int ax1, int ax2, int *status )

*  Class Membership:
*     FrameSet member function (over-rides the astRate method
*     inherited from the Frame class).

*     This function evaluates the rate of change of a specified output of
*     the supplied Mapping with respect to a specified input, at a
*     specified input position.
*
*     The result is estimated by interpolating the function using a
*     fourth order polynomial in the neighbourhood of the specified
*     position. The size of the neighbourhood used is chosen to minimise
*     the RMS residual per unit length between the interpolating
*     polynomial and the supplied Mapping function.

*  Parameters:
*     this
*        Pointer to the Mapping to be applied.
*     at
*        The address of an array holding the axis values at the position
*        at which the rate of change is to be evaluated. The number of
*        elements in this array should equal the number of inputs to the
*        Mapping.
*     ax1
*        The index of the Mapping output for which the rate of change is to
*        be found (output numbering starts at 0 for the first output).
*     ax2
*        The index of the Mapping input which is to be varied in order to
*        find the rate of change (input numbering starts at 0 for the first
*        input).
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     astRate()
*        The rate of change of Mapping output "ax1" with respect to input
*        "ax2", evaluated at "at", or AST__BAD if the value cannot be
*        calculated.

*  Notes:
*     - A value of AST__BAD will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*-
*/

/* Local Variables: */
   AstFrameSet *this;            /* Pointer to the FrameSet structure */
   AstMapping *map;              /* Pointer to the base->current Mapping */
   double result;                /* Returned rate of change */

/* Check the global error status. */
   if ( !astOK ) return AST__BAD;

/* Obtain a pointer to the FrameSet structure. */
   this = (AstFrameSet *) this_mapping;

/* Obtain the Mapping between the base and current Frames in the
   FrameSet (note this takes account of whether the FrameSet has been
   inverted). */
   map = astGetMapping( this, AST__BASE, AST__CURRENT );

/* Invoke the astRate method on the Mapping. */
   result = astRate( map, at, ax1, ax2 );

/* Annul the Mapping pointer. */
   map = astAnnul( map );

/* Return a pointer to the output PointSet. */
   return result;
}

static void RecordIntegrity( AstFrameSet *this, int *status ) {
/*
*+
*  Name:
*     RecordIntegrity

*  Purpose:
*     Record the current integrity state of a FrameSet.

*  Type:
*     Private function.

*  Synopsis:
*     #include "frameset.h"
*     void RecordIntegrity( AstFrameSet *this, int *status )

*  Class Membership:
*     FrameSet member function.

*  Description:
*     This function makes a record of the current integrity state of a
*     FrameSet by taking a copy of its current Frame (it stores a
*     pointer to this copy in a static variable). If the current Frame
*     is subsequently modified, the RestoreIntegrity function can then
*     attempt to restore the FrameSet's integrity to this recorded
*     state by appropriately remapping its current Frame.

*  Parameters:
*     this
*        Pointer to the FrameSet.
*-
*     status
*        Pointer to the inherited status variable.
*/

/* Local Variables: */
   astDECLARE_GLOBALS            /* Declare the thread specific global data */
   AstFrame *current;            /* Pointer to current Frame */

/* Get a pointer to the structure holding thread-specific global data. */
   astGET_GLOBALS(this);

/* Initialise the record of the FrameSet's integrity. */
   integrity_frame = NULL;
   integrity_lost = 0;

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain a pointer to the FrameSet's current Frame. */
   current = astGetFrame( this, AST__CURRENT );

/* Make a copy of this Frame, storing its pointer. */
   integrity_frame = astCopy( current );

/* Annul the current Frame pointer. */
   current = astAnnul( current );
}

static void RemapFrame( AstFrameSet *this, int iframe, AstMapping *map, int *status ) {
/*
*++
*  Name:
c     astRemapFrame
f     AST_REMAPFRAME

*  Purpose:
*     Modify a Frame's relationship to other Frames in a FrameSet.

*  Type:
*     Public virtual function.

*  Synopsis:
c     #include "frameset.h"
c     void astRemapFrame( AstFrameSet *this, int iframe, AstMapping *map )
f     CALL AST_REMAPFRAME( THIS, IFRAME, MAP, STATUS )

*  Class Membership:
*     FrameSet method.

*  Description:
c     This function modifies the relationship (i.e. Mapping) between a
f     This routine modifies the relationship (i.e. Mapping) between a
*     specified Frame in a FrameSet and the other Frames in that
*     FrameSet.
*
*     Typically, this might be required if the FrameSet has been used
*     to calibrate (say) an image, and that image is re-binned. The
*     Frame describing the image will then have undergone a coordinate
*     transformation, and this should be communicated to the associated
c     FrameSet using this function.
f     FrameSet using this routine.

*  Parameters:
c     this
f     THIS = INTEGER (Given)
*        Pointer to the FrameSet.
c     iframe
f     IFRAME = INTEGER (Given)
*        The index within the FrameSet of the Frame to be modified.
*        This value should lie in the range from 1 to the number of
*        Frames in the FrameSet (as given by its Nframe attribute).
c     map
f     MAP = INTEGER (Given)
*        Pointer to a Mapping whose forward transformation converts
*        coordinate values from the original coordinate system
*        described by the Frame to the new one, and whose inverse
*        transformation converts in the opposite direction.
f     STATUS = INTEGER (Given and Returned)
f        The global status.

*  Notes:
*     - A value of AST__BASE or AST__CURRENT may be given for the
c     "iframe" parameter to specify the base Frame or the current
f     IFRAME argument to specify the base Frame or the current
*     Frame respectively.
*     - The relationship between the selected Frame and any other
c     Frame within the FrameSet will be modified by this function,
f     Frame within the FrameSet will be modified by this routine,
*     but the relationship between all other Frames in the FrameSet
*     remains unchanged.
*     - The number of input coordinate values accepted by the Mapping
*     (its Nin attribute) and the number of output coordinate values
*     generated (its Nout attribute) must be equal and must match the
*     number of axes in the Frame being modified.
*     - If a simple change of axis order is required, then the
c     astPermAxes function may provide a more straightforward method
f     AST_PERMAXES routine may provide a more straightforward method
*     of making the required changes to the FrameSet.
c     - This function cannot be used to change the number of Frame
f     - This routine cannot be used to change the number of Frame
*     axes. To achieve this, a new Frame must be added to the FrameSet
c     (astAddFrame) and the original one removed if necessary
c     (astRemoveFrame).
f     (AST_ADDFRAME) and the original one removed if necessary
f     (AST_REMOVEFRAME).
*--
*/

/* Local Variables: */
   AstFrame *fr;                 /* Pointer to Frame */
   int naxes;                    /* Number of Frame axes */
   int nin;                      /* Number of Mapping input coordinates */
   int nout;                     /* Number of Mapping output coordinates */

/* Check the global error status. */
   if ( !astOK ) return;

/* Validate and translate the Frame index supplied. */
   iframe = astValidateFrameIndex( this, iframe, "astRemapFrame" );

/* Obtain the number of input and output coordinates per point for the
   Mapping supplied. */
   nin = astGetNin( map );
   nout = astGetNout( map );

/* Obtain a pointer to the specified Frame and determine how many axes
   it has. Annul the Frame pointer afterwards. */
   fr = astGetFrame( this, iframe );
   naxes = astGetNaxes( fr );
   fr = astAnnul( fr );

/* Check that the number of input coordinates matches the number of
   Frame axes and report an error if necessary. */
   if ( astOK ) {
      if ( nin != naxes ) {
         astError( AST__NCPIN, "astRemapFrame(%s): Bad number of %s input "
                   "coordinate values (%d).", status, astGetClass( this ),
                   astGetClass( map ), nin );
         astError( AST__NCPIN, "The %s given should accept %d coordinate "
                   "value%s for each input point.", status, astGetClass( map ), naxes,
                   ( naxes == 1 ) ? "" : "s" );

/* Similarly, check that the number of output coordinates matches the
   number of Frame axes. */
      } else if ( nout != naxes ) {
         astError( AST__NCPIN, "astRemapFrame(%s): Bad number of %s output "
                   "coordinate values (%d).", status, astGetClass( this ),
                   astGetClass( map ), nout );
         astError( AST__NCPIN, "The %s given should generate %d "
                   "coordinate value%s for each output point.", status,
                   astGetClass( map ), naxes, ( naxes == 1 ) ? "" : "s" );
      }
   }

/* If there is more than one Frame present in the FrameSet, extend the
   FrameSet arrays to hold a new node. */
   if ( astOK && ( this->nframe > 1 ) ) {
      this->map = astGrow( this->map, this->nnode, sizeof( AstMapping * ) );
      this->link = astGrow( this->link, this->nnode, sizeof( int ) );
      this->invert = astGrow( this->invert, this->nnode, sizeof( int ) );

/* Clone and store a pointer to the Mapping. */
      if ( astOK ) {
         this->map[ this->nnode - 1 ] = astClone( map );

/* Add a new "link" element showing that the new node is derived from
   that of the old Frame and store the current value of the Invert
   attribute for the Mapping. */
         this->link[ this->nnode - 1 ] = this->node[ iframe - 1 ];
         this->invert[ this->nnode - 1 ] = astGetInvert( map );

/* Increment the node count and associate the modified Frame with the
   new node. */
         if ( astOK ) {
            this->nnode++;
            this->node[ iframe - 1 ] = this->nnode - 1;

/* Tidy the resulting set of nodes, because the node originally
   referenced by the Frame may no longer be needed. This also
   simplifies any compound Mapping which may result if this node is
   removed. */
            TidyNodes( this, status );
         }
      }
   }
}

static void RemoveFrame( AstFrameSet *this, int iframe, int *status ) {
/*
*++
*  Name:
c     astRemoveFrame
f     AST_REMOVEFRAME

*  Purpose:
*     Remove a Frame from a FrameSet.

*  Type:
*     Public virtual function.

*  Synopsis:
c     #include "frameset.h"
c     void astRemoveFrame( AstFrameSet *this, int iframe )
f     CALL AST_REMOVEFRAME( THIS, IFRAME, STATUS )

*  Class Membership:
*     FrameSet method.

*  Description:
c     This function removes a Frame from a FrameSet. All other Frames
f     This routine removes a Frame from a FrameSet. All other Frames
*     in the FrameSet have their indices re-numbered from one (if
*     necessary), but are otherwise unchanged.

*  Parameters:
c     this
f     THIS = INTEGER (Given)
*        Pointer to the FrameSet.
c     iframe
f     IFRAME = INTEGER (Given)
*        The index within the FrameSet of the Frame to be removed.
*        This value should lie in the range from 1 to the number of
*        Frames in the FrameSet (as given by its Nframe attribute).
f     STATUS = INTEGER (Given and Returned)
f        The global status.

*  Notes:
*     - Removing a Frame from a FrameSet does not affect the
*     relationship between other Frames in the FrameSet, even if they
*     originally depended on the Frame being removed.
*     - The number of Frames in a FrameSet cannot be reduced to zero.
*     An error will result if an attempt is made to remove the only
*     remaining Frame.
*     - A value of AST__BASE or AST__CURRENT may be given for the
c     "iframe" parameter to specify the base Frame or the current
f     IFRAME argument to specify the base Frame or the current
*     Frame respectively.
*     - If a FrameSet's base or current Frame is removed, the Base or
*     Current attribute (respectively) of the FrameSet will have its
*     value cleared, so that another Frame will then assume its role
*     by default.
*     - If any other Frame is removed, the base and current Frames
*     will remain the same. To ensure this, the Base and/or Current
*     attributes of the FrameSet will be changed, if necessary, to
*     reflect any change in the indices of these Frames.
*--
*/

/* Local Variables: */
   int ifr;                      /* Loop counter for Frames */
   int ii;                       /* Base/current Frame index */

/* Check the global error status. */
   if ( !astOK ) return;

/* Validate and translate the Frame index supplied. */
   iframe = astValidateFrameIndex( this, iframe, "astRemoveFrame" );
   if ( astOK ) {

/* Reject any attempt to remove the final Frame from the FrameSet. */
      if ( this->nframe == 1 ) {
         astError( AST__REMIN, "astRemoveFrame(%s): Invalid attempt to "
                   "remove the only Frame in a %s.", status, astGetClass( this ),
                   astGetClass( this ) );

/* If OK, annul the pointer to the selected Frame. */
      } else {
         this->frame[ iframe - 1 ] = astAnnul( this->frame[ iframe - 1 ] );

/* Loop to move all subsequent Frame pointers down in the FrameSet's
   "frame" array to close the resulting gap. Also move the associated
   "node" array contents in the same way. */
         for ( ifr = iframe; ifr < this->nframe; ifr++ ) {
            this->frame[ ifr - 1 ] = this->frame[ ifr ];
            this->node[ ifr - 1 ] = this->node[ ifr ];
         }
         this->frame[ this->nframe - 1 ] = NULL;
         this->node[ this->nframe - 1 ] = -1;

/* Decrement the Frame count. */
         this->nframe--;

/* Tidy the nodes in the FrameSet. */
         TidyNodes( this, status );

/* If the Base attribute is set and the removed Frame was the base
   Frame, then clear the attribute value so that a new base Frame will
   be selected by default. */
         if ( astTestBase( this ) ) {
            ii = astGetBase( this );
            if ( iframe == ii ) {
               astClearBase( this );

/* If the index of the removed Frame is smaller than the base Frame
   index, then decrement the Base attribute so that the same base
   Frame will be used in future. */
            } else if ( iframe < ii ) {
               astSetBase( this, ii - 1 );
            }
         }

/* Repeat the above procedure for the current Frame. */
         if ( astTestCurrent( this ) ) {
            ii = astGetCurrent( this );
            if ( iframe == ii ) {
               astClearCurrent( this );
            } else if ( iframe < ii ) {
               astSetCurrent( this, ii - 1 );
            }
         }
      }
   }
}

static AstMapping *RemoveRegions( AstMapping *this_mapping, int *status ) {
/*
*  Name:
*     RemoveRegions

*  Purpose:
*     Remove any Regions from a Mapping.

*  Type:
*     Private function.

*  Synopsis:
*     #include "frameset.h"
*     AstMapping *RemoveRegions( AstMapping *this, int *status )

*  Class Membership:
*     FrameSet method (over-rides the astRemoveRegions method inherited
*     from the Mapping class).

*  Description:
*     This function searches the supplied Mapping (which may be a
*     compound Mapping such as a FrameSet) for any component Mappings
*     that are instances of the AST Region class. It then creates a new
*     Mapping from which all Regions have been removed. If a Region
*     cannot simply be removed (for instance, if it is a component of a
*     parallel FrameSet), then it is replaced with an equivalent UnitMap
*     in the returned Mapping.
*
*     The implementation provided by the FrameSet class invokes the
*     astRemoveRegions method on all the component Frames and Mappings,
*     and joins the results together into a new FrameSet.

*  Parameters:
*     this
*        Pointer to the original Region.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     A pointer to the modified mapping.

*  Notes:
*     - A NULL pointer value will be returned if this function is
*     invoked with the AST error status set, or if it should fail for
*     any reason.
*/

/* Local Variables: */
   AstFrame **newfrms;           /* Array of new Frames */
   AstFrameSet *new;             /* Pointer to new FrameSet */
   AstFrameSet *this;            /* Pointer to FrameSet structure */
   AstMapping **newmaps;         /* Array of new Mappings */
   AstMapping *result;           /* Result pointer to return */
   int changed;                  /* Has any mapping been changed? */
   int i;                        /* Loop count */
   int nax;                      /* Number of Frame axes */

/* Initialise. */
   result = NULL;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Get a pointer to the FrameSet. */
   this = (AstFrameSet *) this_mapping;

/* Allocate arrays to hold the modified Mapping and Frame pointers. */
   newmaps = astMalloc( sizeof( AstMapping *)*( this->nnode - 1 ) );
   newfrms = astMalloc( sizeof( AstFrame *)*( this->nframe ) );
   if( astOK ) {

/* Invoke the astRemoveRegions method on all the component Mappings. */
      changed = 0;
      for( i = 0; i < this->nnode - 1; i++ ) {
         newmaps[ i ] = astRemoveRegions( this->map[ i ] );

/* Note if any Mapping was changed. */
         if( newmaps[ i ] != this->map[ i ] ) {
            changed = 1;

/* The implementation of the astRemoveRegions method provided by the
   Region class returns a Frame rather than a UnitMap. But we need
   Mappings here, not Frames. So if the new Mapping is a Frame, replace
   it with an equivalent UnitMap. */
            if( astIsAFrame( newmaps[ i ] ) ) {
               nax = astGetNin( newmaps[ i ] );
               (void) astAnnul( newmaps[ i ] );
               newmaps[ i ] = (AstMapping *) astUnitMap( nax, " ", status );
            }
         }
      }

/* Invoke the astRemoveRegions method on all the component Frames. */
      for( i = 0; i < this->nframe; i++ ) {
         newfrms[ i ] = astRemoveRegions( this->frame[ i ] );

/* Note if any Frame was changed. */
         if( newfrms[ i ] != this->frame[ i ] ) changed = 1;
      }

/* If no component was modified, just return a clone of the supplied
   pointer. */
      if( ! changed ) {
         result = astClone( this );

/* Otherwise, we need to create a new FrameSet to return. We take a deep
   copy of the supplied FrameSet and then modify the Mappings and Frames
   so that we retain any extra information in the supplied FrameSet. */
      } else {
         new = astCopy( this );

         for( i = 0; i < this->nnode - 1; i++ ) {
            (void) astAnnul( new->map[ i ] );
            new->map[ i ] = astClone( newmaps[ i ] );
         }

         for( i = 0; i < this->nframe; i++ ) {
            (void) astAnnul( new->frame[ i ] );
            new->frame[ i ] = astClone( newfrms[ i ] );
         }

         result = (AstMapping *) new;
      }

/* Free resources. */
      for( i = 0; i < this->nnode - 1; i++ ) {
         newmaps[ i ] = astAnnul( newmaps[ i ] );
      }

      for( i = 0; i < this->nframe; i++ ) {
         newfrms[ i ] = astAnnul( newfrms[ i ] );
      }

   }

   newfrms = astFree( newfrms );
   newmaps = astFree( newmaps );

/* Annul the returned Mapping if an error has occurred. */
   if( !astOK ) result = astAnnul( result );

/* Return the result. */
   return result;
}

static void ReportPoints( AstMapping *this_mapping, int forward,
                          AstPointSet *in_points, AstPointSet *out_points, int *status ) {
/*
*  Name:
*     ReportPoints

*  Purpose:
*     Report the effect of transforming a set of points using a FrameSet.

*  Type:
*     Private function.

*  Synopsis:
*     #include "mapping.h"
*     void ReportPoints( AstMapping *this, int forward,
*                        AstPointSet *in_points, AstPointSet *out_points, int *status )

*  Class Membership:
*     FrameSet member function (over-rides the protected astReportPoints
*     method inherited from the Frame class).

*  Description:
*     This function reports the coordinates of a set of points before
*     and after being transformed by a FrameSet, by writing them to
*     standard output.

*  Parameters:
*     this
*        Pointer to the FrameSet.
*     forward
*        A non-zero value indicates that the FrameSet's forward
*        coordinate transformation has been applied, while a zero
*        value indicates the inverse transformation.
*     in_points
*        Pointer to a PointSet which is associated with the
*        coordinates of a set of points before the FrameSet was
*        applied.
*     out_points
*        Pointer to a PointSet which is associated with the
*        coordinates of the same set of points after the FrameSet has
*        been applied.
*     status
*        Pointer to the inherited status variable.
*/

/* Local Variables: */
   AstFrame *base_frame;         /* Pointer to current Frame */
   AstFrame *current_frame;      /* Pointer to base Frame */
   AstFrameSet *this;            /* Pointer to the FrameSet structure */
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

/* Obtain a pointer to the FrameSet structure. */
   this = (AstFrameSet *) this_mapping;

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

/* Obtain pointers to the FrameSet's base and current Frames. */
   base_frame = astGetFrame( this, AST__BASE );
   current_frame = astGetFrame( this, AST__CURRENT );

/* Loop to report the effect of the transformation on each point in
   turn. */
   if ( astOK ) {
      for ( point = 0; point < npoint; point++ ) {

/* Report the input coordinates (in parentheses and separated by
   commas). Format each value for display using the appropriate
   Frame's astFormat method. */
         printf( "(" );
         for ( coord = 0; coord < ncoord_in; coord++ ) {
            printf( "%s%s", coord ? ", " : "",
                    astFormat( forward ? base_frame : current_frame,
                               coord, ptr_in[ coord ][ point ] ) );
         }

/* Similarly report the output coordinates, this time formatting
   values using the other Frame's astFormat method. */
         printf( ") --> (" );
         for ( coord = 0; coord < ncoord_out; coord++ ) {
            printf( "%s%s", coord ? ", " : "",
                    astFormat( forward ? current_frame : base_frame,
                               coord, ptr_out[ coord ][ point ] ) );
         }
         printf( ")\n" );
      }
   }

/* Annul the Frame pointers. */
   base_frame = astAnnul( base_frame );
   current_frame = astAnnul( current_frame );
}

static void Resolve( AstFrame *this_frame, const double point1[],
                     const double point2[], const double point3[],
                     double point4[], double *d1, double *d2, int *status ){
/*
*  Name:
*     Resolve

*  Purpose:
*     Resolve a vector into two orthogonal components

*  Type:
*     Private function.

*  Synopsis:
*     #include "frameset.h"
*     void Resolve( AstFrame *this, const double point1[],
*                   const double point2[], const double point3[],
*                   double point4[], double *d1, double *d2, int *status );

*  Class Membership:
*     FrameSet member function (over-rides the protected astResolve
*     method inherited from the Frame class).

*  Description:
*     This function resolves a vector into two perpendicular components.
*     The vector from point 1 to point 2 is used as the basis vector.
*     The vector from point 1 to point 3 is resolved into components
*     parallel and perpendicular to this basis vector. The lengths of the
*     two components are returned, together with the position of closest
*     aproach of the basis vector to point 3.

*  Parameters:
*     this
*        Pointer to the Frame.
*     point1
*        An array of double, with one element for each Frame axis
*        (Naxes attribute). This marks the start of the basis vector,
*        and of the vector to be resolved.
*     point2
*        An array of double, with one element for each Frame axis
*        (Naxes attribute). This marks the end of the basis vector.
*     point3
*        An array of double, with one element for each Frame axis
*        (Naxes attribute). This marks the end of the vector to be
*        resolved.
*     point4
*        An array of double, with one element for each Frame axis
*        in which the coordinates of the point of closest approach of the
*        basis vector to point 3 will be returned.
*     d1
*        The address of a location at which to return the distance from
*        point 1 to point 4 (that is, the length of the component parallel
*        to the basis vector). Positive values are in the same sense as
*        movement from point 1 to point 2.
*     d2
*        The address of a location at which to return the distance from
*        point 4 to point 3 (that is, the length of the component
*        perpendicular to the basis vector). The value is always positive.
*     status
*        Pointer to the inherited status variable.

*  Notes:
*     - Each vector used in this function is the path of
*     shortest distance between two points, as defined by the
*     astDistance function.
*     - This function will return "bad" coordinate values (AST__BAD)
*     if any of the input coordinates has this value, or if the required
*     output values are undefined.
*/

/* Local Variables: */
   AstFrame *fr;                 /* Pointer to current Frame */
   AstFrameSet *this;            /* Pointer to the FrameSet structure */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain a pointer to the FrameSet structure. */
   this = (AstFrameSet *) this_frame;

/* Obtain a pointer to the FrameSet's current Frame and invoke this
   Frame's astResolve method. Annul the Frame pointer afterwards. */
   fr = astGetFrame( this, AST__CURRENT );
   astResolve( fr, point1, point2, point3, point4, d1, d2 );
   fr = astAnnul( fr );

}

static AstPointSet *ResolvePoints( AstFrame *this_frame, const double point1[],
                                   const double point2[], AstPointSet *in,
                                   AstPointSet *out, int *status ) {
/*
*  Name:
*     ResolvePoints

*  Purpose:
*     Resolve a set of vectors into orthogonal components

*  Type:
*     Private function.

*  Synopsis:
*     #include "frameset.h"
*     AstPointSet *astResolvePoints( AstFrame *this, const double point1[],
*                                    const double point2[], AstPointSet *in,
*                                    AstPointSet *out )

*  Class Membership:
*     FrameSet member function (over-rides the astResolvePoints method
*     inherited from the Frame class).

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
*     values will always be positive.

*  Notes:
*     - The number of coordinate values per point in the input
*     PointSet must match the number of axes in the supplied Frame.
*     - If an output PointSet is supplied, it must have space for
*     sufficient number of points and 2 coordinate values per point.
*     - A null pointer will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*/

/* Local Variables: */
   AstPointSet *result;          /* Pointer to output PointSet */
   AstFrameSet *this;            /* Pointer to FrameSet structure */
   AstFrame *fr;                 /* Pointer to current Frame */

/* Initialise. */
   result = NULL;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Obtain a pointer to the FrameSet structure. */
   this = (AstFrameSet *) this_frame;

/* Obtain a pointer to the FrameSet's current Frame and invoke this
   Frame's astResolvePoints method. Annul the Frame pointer afterwards. */
   fr = astGetFrame( this, AST__CURRENT );
   result = astResolvePoints( this, point1, point2, in, out );
   fr = astAnnul( fr );

/* Return a pointer to the output PointSet. */
   return result;

}

static void RestoreIntegrity( AstFrameSet *this, int *status ) {
/*
*+
*  Name:
*     RestoreIntegrity

*  Purpose:
*     Restore a previous integrity state for a FrameSet.

*  Type:
*     Private function.

*  Synopsis:
*     #include "frameset.h"
*     void RestoreIntegrity( AstFrameSet *this )

*  Class Membership:
*     FrameSet member function.

*  Description:
*     This function restores a FrameSet to a previous integrity state,
*     as recorded (in static variables) by a previous invocation of
*     the RecordIntegrity function. It does this by appropriately
*     remapping the FrameSet's current Frame, if this appears
*     necessary.

*  Parameters:
*     this
*        Pointer to the FrameSet.

*  Notes:
*     - The previous record of the FrameSet's integrity state (as
*     recorded by RecordIntegrity) is deleted by this function, even
*     if it is invoked with the global error status set.
*     - An error will result if the previous integrity state cannot be
*     restored.
*-
*/

/* Local Variables: */
   astDECLARE_GLOBALS            /* Declare the thread specific global data */
   AstFrame *current;            /* Pointer to current Frame */
   AstFrameSet *cvt;             /* Pointer to conversion FrameSet */
   AstMapping *map;              /* Pointer to conversion Mapping */
   int flags;                    /* Flags associated with current frame */

/* Get a pointer to the structure holding thread-specific global data. */
   astGET_GLOBALS(this);

/* Check that a previous record of the FrameSet's integrity state has
   been made. Do not modify the FrameSet if it appears that the
   previous integrity state has not been lost (i.e. that the current
   Frame has not been modified), nor if there is only one Frame
   present. Check the global error status. */
   if ( integrity_frame && integrity_lost && ( astGetNframe( this ) > 1 ) &&
        astOK ) {

/* Obtain a pointer to the current Frame. */
      current = astGetFrame( this, AST__CURRENT );

/* Since we need to obtain a conversion between the recorded copy of
   this Frame and the current one, we must match their Domain
   attributes (otherwise conversion cannot be performed). Do this by
   changing the recorded copy as necessary. */
      if ( astTestDomain( current ) ) {
         astSetDomain( integrity_frame, astGetDomain( current ) );
      } else {
         astClearDomain( integrity_frame );
      }

/* Temporarily set both Frames AST__INTFLAG flag to indicate that the
   following call to astConvert is part of the process of restoring a
   FrameSet's integrity. Some classes of Frame (e.g. DSBSpecFrames) may
   choose to return a different Mapping in this case. */
      astSetFrameFlags( integrity_frame, astGetFrameFlags( integrity_frame )
                                         | AST__INTFLAG );
      flags = astGetFrameFlags( current );
      astSetFrameFlags( current, flags | AST__INTFLAG );

/* Obtain the required conversion FrameSet, restore the original frame
   flags and annul the current Frame pointer. */
      cvt = astConvert( integrity_frame, current, "" );
      astSetFrameFlags( current, flags );
      current = astAnnul( current );

/* If no conversion could be found, then the FrameSet's integrity
   state cannot be restored, so report an error. */
      if ( !cvt ) {
         if( astOK ) {
            astError( AST__ILOST, "%s(%s): Cannot maintain %s integrity.", status,
                      integrity_method, astGetClass( this ),
                      astGetClass( this ) );
         }

/* Otherwise, obtain a pointer to the conversion Mapping. */
      } else {
         map = astGetMapping( cvt, AST__BASE, AST__CURRENT );

/* If the Mapping is not a UnitMap (i.e. a null Mapping), then use it
   to remap the FrameSet's current Frame. */
         if ( strcmp( astGetClass( map ), "UnitMap" ) ) {
            astRemapFrame( this, AST__CURRENT, map );
         }

/* Annul the conversion Mapping and Frameset pointers. */
         map = astAnnul( map );
         cvt = astAnnul( cvt );
      }
   }

/* Delete the recorded integrity information by annulling the original
   copy of the current Frame (thus deleting it) and resetting the
   associated modification flag. */
   if ( integrity_frame ) integrity_frame = astAnnul( integrity_frame );
   integrity_lost = 0;
}

static void SetAttrib( AstObject *this_object, const char *setting, int *status ) {
/*
*  Name:
*     SetAttrib

*  Purpose:
*     Set an attribute value for a FrameSet.

*  Type:
*     Private function.

*  Synopsis:
*     #include "frameset.h"
*     void SetAttrib( AstObject *this, const char *setting, int *status )

*  Class Membership:
*     FrameSet member function (extends the astSetAttrib method
*     inherited from the Frame class).

*  Description:
*     This function assigns an attribute value for a FrameSet, the
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
*        Pointer to the FrameSet.
*     setting
*        Pointer to a null terminated string specifying the new
*        attribute value.
*     status
*        Pointer to the inherited status variable.

*  Attributes:
*     The set of attribute values is not fixed and is determined by
*     the current Frame. In addition, the FrameSet class defines the
*     following attributes:
*
*        Base (integer)
*        Current (integer)

*  Notes:
*     - This protected method is intended to be invoked by the Object
*     astSet method and makes additional attributes accessible to it.
*     - All attribute settings passed to this function are simply
*     passed on to the corresponding method for the FrameSet's current
*     Frame.
*/

/* Local Variables: */
   astDECLARE_GLOBALS            /* Declare the thread specific global data */
   AstFrame *fr;                 /* Pointer to current Frame */
   AstFrameSet *this;            /* Pointer to the FrameSet structure */
   int base;                     /* Base attribute value */
   int base_off;                 /* Offset of Base value string */
   int current;                  /* Current attribute value */
   int current_off;              /* Offset of Current value string */
   int id;                       /* Offset of ID string */
   int invert;                   /* Invert attribute value */
   int len;                      /* Length of setting string */
   int nc;                       /* Number of characters read by astSscanf */
   int report;                   /* Report attribute value */

/* Check the global error status. */
   if ( !astOK ) return;

/* Get a pointer to the structure holding thread-specific global data. */
   astGET_GLOBALS(this_object);

/* Obtain a pointer to the FrameSet structure. */
   this = (AstFrameSet *) this_object;

/* Obtain the length of the setting string. */
   len = strlen( setting );

/* Test for each recognised attribute in turn, using "astSscanf" to parse the
   setting string and extract the attribute value (or an offset to it in the
   case of string values). In each case, use the value set in "nc" to check
   that the entire string was matched. Once a value has been obtained, use the
   appropriate method to set it. */

/* We first handle attributes that apply to the FrameSet as a whole
   (rather than to the current Frame). */

/* Base. */
/* ----- */
/* Read as an integer. */
   if ( nc = 0,
        ( 1 == astSscanf( setting, "base= %d %n", &base, &nc ) )
          && ( nc >= len ) ) {
      astSetBase( this, base );

/* Also allow a string. */
   } else if ( nc = 0,
               ( 0 == astSscanf( setting, "base= %n%*s %n", &base_off, &nc ) )
                 && ( nc >= len ) ) {

/* Check for "AST__CURRENT" or "Current". */
      if ( astChrMatch( "AST__CURRENT", setting + base_off ) ||
           astChrMatch( "Current", setting + base_off ) ) {
         astSetBase( this, AST__CURRENT );

/* Check for "AST__BASE" or "Base" (possible, although not very
   useful). */
      } else if ( astChrMatch( "AST__BASE", setting + base_off ) ||
                  astChrMatch( "Base", setting + base_off ) ) {

/* Report an error if the value wasn't recognised. */
      } else {
         astError( AST__ATTIN, "astSetAttrib(%s): Invalid index value for "
                   "Base Frame \"%s\".", status,
                   astGetClass( this ),  setting + base_off );
      }

/* Current. */
/* -------- */
/* Since this determines the choice of current Frame, we must restore
   the integrity state of the FrameSet before changing this attribute
   and record the new integrity state afterwards. */

/* Read as an integer. */
   } else if ( nc = 0,
               ( 1 == astSscanf( setting, "current= %d %n", &current, &nc ) )
                 && ( nc >= len ) ) {
      RestoreIntegrity( this, status );
      astSetCurrent( this, current );
      RecordIntegrity( this, status );

/* Also allow a string. */
   } else if ( nc = 0,
               ( 0 == astSscanf( setting, "current= %n%*s %n",
                              &current_off, &nc ) )
                 && ( nc >= len ) ) {

/* Check for "AST__BASE" or "Base". */
      if ( astChrMatch( "AST__BASE", setting + current_off ) ||
           astChrMatch( "Base", setting + current_off ) ) {
         RestoreIntegrity( this, status );
         astSetCurrent( this, AST__BASE );
         RecordIntegrity( this, status );

/* Check for "AST__CURRENT" or "Current" (possible, although not very
   useful). */
      } else if ( astChrMatch( "AST__CURRENT", setting + current_off ) ||
                  astChrMatch( "Current", setting + current_off ) ) {

/* Report an error if the value wasn't recognised. */
      } else {
         astError( AST__ATTIN, "astSetAttrib(%s): Invalid index value for "
                   "Current Frame \"%s\".", status,
                   astGetClass( this ),  setting + current_off );
      }

/* ID. */
/* --- */
   } else if ( nc = 0, ( 0 == astSscanf( setting, "id=%n%*[^\n]%n", &id, &nc ) )
                       && ( nc >= len ) ) {
      astSetID( this, setting + id );

/* Ident. */
/* ------ */
   } else if ( nc = 0, ( 0 == astSscanf( setting, "ident=%n%*[^\n]%n", &id, &nc ) )
                       && ( nc >= len ) ) {
      astSetIdent( this, setting + id );

/* Invert. */
/* ------- */
/* Since this affects the choice of current Frame, we must restore the
   integrity state of the FrameSet before changing this attribute and
   record the new integrity state afterwards. */
   } else if ( nc = 0,
        ( 1 == astSscanf( setting, "invert= %d %n", &invert, &nc ) )
        && ( nc >= len ) ) {
      RestoreIntegrity( this, status );
      astSetInvert( this, invert );
      RecordIntegrity( this, status );

/* Report. */
/* ------- */
   } else if ( nc = 0,
        ( 1 == astSscanf( setting, "report= %d %n", &report, &nc ) )
        && ( nc >= len ) ) {
      astSetReport( this, report );

/* Define a macro to see if the setting string matches any of the
   read-only attributes of this class. */
#define MATCH(attrib) \
        ( nc = 0, ( 0 == astSscanf( setting, attrib "=%*[^\n]%n", &nc ) ) && \
                  ( nc >= len ) )

/* If the attribute was not recognised, use this macro to report an error
   if a read-only attribute has been specified. */
   } else if ( MATCH( "class" ) ||
               MATCH( "nframe" ) ||
               MATCH( "nin" ) ||
               MATCH( "nobject" ) ||
               MATCH( "nout" ) ||
               MATCH( "refcount" ) ||
               MATCH( "tranforward" ) ||
               MATCH( "traninverse" ) ) {
      astError( AST__NOWRT, "astSet: The setting \"%s\" is invalid for a %s.", status,
                setting, astGetClass( this ) );
      astError( AST__NOWRT, "This is a read-only attribute." , status);

/* Pass unrecognised settings on to the FrameSet's current Frame for
   further interpretation. */
   } else {

/* Force a copy to be made of the current Frame, if needed, to make it
   independent of other Frames within the FrameSet. */
      (void) ForceCopy( this, AST__CURRENT, status );

/* Obtain a pointer to the current Frame and invoke its astSetAttrib
   method. Annul the Frame pointer afterwards. */
      fr = astGetFrame( this, AST__CURRENT );
      astSetAttrib( fr, setting );
      fr = astAnnul( fr );

/* Note that the current Frame has been modified. */
      integrity_lost = 1;
   }

/* Undefine macros local to this function. */
#undef MATCH
}

static void SetAxis( AstFrame *this_frame, int axis, AstAxis *newaxis, int *status ) {
/*
*  Name:
*     SetAxis

*  Purpose:
*     Set a new Axis for a FrameSet.

*  Type:
*     Private function.

*  Synopsis:
*     #include "frameset.h"
*     void SetAxis( AstFrame *this, int axis, AstAxis *newaxis, int *status )

*  Class Membership:
*     FrameSet member function (over-rides the astSetAxis method
*     inherited from the Frame class).

*  Description:
*     This function allows a new Axis object to be associated with one
*     of the axes of the current Frame in a FrameSet, replacing the
*     previous one. Each Axis object contains a description of the
*     quantity represented along one of the Frame's axes, so this
*     function allows this description to be exchanged for another
*     one.

*  Parameters:
*     this
*        Pointer to the FrameSet.
*     axis
*        The index (zero-based) of the axis whose associated Axis
*        object is to be replaced.
*     newaxis
*        Pointer to the new Axis object.
*     status
*        Pointer to the inherited status variable.
*/

/* Local Variables: */
   AstFrame *fr;                 /* Pointer to current Frame */
   AstFrameSet *this;            /* Pointer to the FrameSet structure */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain a pointer to the FrameSet structure. */
   this = (AstFrameSet *) this_frame;

/* Validate the axis index supplied. */
   (void) astValidateAxis( this, axis, 1, "astSetAxis" );

/* Obtain a pointer to the FrameSet's current Frame and invoke this
   Frame's astSetAxis method to assign the new Axis object. Annul the
   Frame pointer afterwards. */
   fr = astGetFrame( this, AST__CURRENT );
   astSetAxis( fr, axis, newaxis );
   fr = astAnnul( fr );
}

static void SetBase( AstFrameSet *this, int iframe, int *status ) {
/*
*+
*  Name:
*     astSetBase

*  Purpose:
*     Set a value for the Base attribute of a FrameSet.

*  Type:
*     Protected virtual function.

*  Synopsis:
*     #include "frameset.h"
*     void astSetBase( AstFrameSet *this, int iframe )

*  Class Membership:
*     FrameSet method.

*  Description:
*     This function sets a value for the Base attribute of a FrameSet. This
*     value is an index that identifies the base Frame for the FrameSet.

*  Parameters:
*     this
*        Pointer to the FrameSet.
*     iframe
*        Value to be set for the Base attribute.

*  Notes:
*     - A value of AST__BASE or AST__CURRENT may be given for the
*     "iframe" parameter to identify the base Frame or the current
*     Frame respectively.
*-
*/

/* Local Variables: */
   int invert;                    /* FrameSet is inverted? */

/* Check the global error status. */
   if ( !astOK ) return;

/* Validate and translate the Frame index supplied. */
   iframe = astValidateFrameIndex( this, iframe, "astSetBase" );

/* Determine if the FrameSet has been inverted. */
   invert = astGetInvert( this );

/* If it has not been inverted, set the base Frame index, otherwise
   set the current Frame index instead. */
   if ( astOK ) *( invert ? &this->current : &this->base ) = iframe;
}

static void SetCurrent( AstFrameSet *this, int iframe, int *status ) {
/*
*+
*  Name:
*     astSetCurrent

*  Purpose:
*     Set a value for the Current attribute of a FrameSet.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "frameset.h"
*     int astSetCurrent( AstFrameSet *this, int iframe )

*  Class Membership:
*     FrameSet method.

*  Description:
*     This function sets a value for the Current attribute of a
*     FrameSet. This attribute is an index that identifies the current
*     Frame for the FrameSet.

*  Parameters:
*     this
*        Pointer to the FrameSet.
*     iframe
*        Value to be set for the Current attribute.

*  Notes:
*     - A value of AST__BASE or AST__CURRENT may be given for the
*     "iframe" parameter to identify the base Frame or the current
*     Frame respectively.
*-
*/

/* Local Variables: */
   int invert;                   /* FrameSet is inverted? */

/* Check the global error status. */
   if ( !astOK ) return;

/* Validate and translate the Frame index supplied. */
   iframe = astValidateFrameIndex( this, iframe, "astSetCurrent" );

/* Determine if the FrameSet has been inverted. */
   invert = astGetInvert( this );

/* If it has not been inverted, set the current frame index, otherwise
   set the base Frame index instead. */
   if ( astOK ) *( invert ? &this->base : &this->current ) = iframe;
}

static AstMapping *Simplify( AstMapping *this_mapping, int *status ) {
/*
*  Name:
*     Simplify

*  Purpose:
*     Simplify the Mappings in a FrameSet.

*  Type:
*     Private function.

*  Synopsis:
*     #include "frameset.h"
*     AstMapping *Simplify( AstMapping *this, int *status )

*  Class Membership:
*     FrameSet method (over-rides the astSimplify method inherited
*     from the Frame class).

*  Description:
*     This function simplifies the Mappings in a FrameSet to eliminate
*     redundant computational steps, or to merge separate steps which
*     can be performed more efficiently in a single operation.

*  Parameters:
*     this
*        Pointer to the original FrameSet.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     A new pointer to the (possibly simplified) FrameSet. If
*     simplification was not possible, this will be a cloned pointer
*     to the original FrameSet.

*  Notes:
*     - A NULL pointer value will be returned if this function is
*     invoked with the AST error status set, or if it should fail for
*     any reason.
*/

/* Local Variables: */
   AstFrameSet *new;             /* Pointer to new (simpler?) FrameSet */
   AstFrameSet *this;            /* Pointer to original FrameSet structure */
   AstMapping *map;              /* Pointer to Mapping */
   AstMapping *result;           /* Result pointer to return */
   AstMapping *tmp;              /* Temporary Mapping pointer */
   int inode;                    /* Loop counter for FrameSet nodes */
   int inv;                      /* Mapping Invert attribute value */
   int invert;                   /* Invert flag value */
   int set;                      /* Invert attribute set? */
   int simpler;                  /* Simplification achieved? */

/* Initialise. */
   result = NULL;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Obtain a pointer to the FrameSet structure. */
   this = (AstFrameSet *) this_mapping;

/* Make a copy of the FrameSet, since we may alter it (this is a deep
   copy, which is a minor limitation of the current implementation). */
   new = astCopy( this );

/* Loop to examine each of the Mappings between the Frames in the
   copy. */
   simpler = 0;
   for ( inode = 1; astOK && ( inode < new->nnode ); inode++ ) {

/* Obtain the Mapping pointer and associated invert flag. */
      map = new->map[ inode - 1 ];
      invert = new->invert[ inode - 1 ];

/* Determine if the Mapping's Invert attribute is set, and obtain its
   value. */
      set = astTestInvert( map );
      inv = astGetInvert( map );

/* If necessary, set the required value for the Invert attribute. */
      if ( inv != invert ) astSetInvert( map, invert );

/* Simplify the Mapping. */
      tmp = astSimplify( map );

/* If necessary, restore the original state of the Mapping's Invert
   attribute. */
      if ( inv != invert ) {
         if ( set ) {
            astSetInvert( map, inv );
         } else {
            astClearInvert( map );
         }
      }

/* Test if simplification was performed. */
      if ( astOK ) {
         if ( tmp != map ) {

/* If so, annul the original Mapping pointer and substitute the new
   one. Also set a new invert flag to accompany it. */
            (void) astAnnul( new->map[ inode - 1 ] );
            new->map[ inode - 1 ] = astClone( tmp );
            new->invert[ inode - 1 ] = astGetInvert( tmp );

/* Note if any Mapping within the FrameSet is simplified. */
            simpler = 1;
         }
      }

/* Annul the pointer to the simplified Mapping. */
      tmp = astAnnul( tmp );
   }

/* If simplification was possible, clone a pointer to the new
   FrameSet. Otherwise clone a pointer to the original one. */
   if ( astOK ) result = astClone( simpler ? new : this );

/* Annul the new FrameSet pointer. */
   new = astAnnul( new );

/* If an error occurred, annul the returned pointer. */
   if ( !astOK ) result = astAnnul( result );

/* Return the result. */
   return result;
}

static int Span( AstFrameSet *this, AstFrame **frames, int inode1, int inode2,
                 int avoid, AstMapping **map, int *forward, int *status ) {
/*
*  Name:
*     Span

*  Purpose:
*     Find a path between two nodes in a FrameSet.

*  Type:
*     Private function.

*  Synopsis:
*     #include "frameset.h"
*     int Span( AstFrameSet *this, AstFrame **frames, int inode1, int inode2,
*               int avoid, AstMapping **map, int *forward, int *status )

*  Class Membership:
*     FrameSet member function.

*  Description:
*     This function searches a FrameSet to identify a path between two
*     specified nodes. It returns an array of pointers to each Mapping
*     in the path, along with direction information, so that an
*     overall Mapping between the two nodes can be constructed.

*  Parameters:
*     this
*        Pointer to the FrameSet.
*     frames
*        Pointer to an array of Frame pointers, indexed by node index.
*        Nodes which have no associated Frame will have a NULL pointer
*        stored in this array.
*     inode1
*        Zero based index of the starting node.
*     inode2
*        Zero based index of the ending node.
*     avoid
*        Zero based index which identifies a node which is to be
*        avoided (i.e. the initial step in the path should not be via
*        this node). This value is required because the function
*        invokes itself recursively; it provides a mechanism to
*        prevent searches proceeding back down paths that have already
*        been searched. External callers should provide a value of -1,
*        which indicates that all possible paths should initially be
*        explored.
*     map
*        Pointer to the start of an array that will be filled with a
*        series of pointers to Mappings which must be applied in turn
*        in order to transform between the two Frames. External
*        callers should ensure that this array contains at least as many
*        elements as there are Mappings and Frames in the FrameSet (one less
*        than the number of nodes plus the number of Frames).
*
*        Note that the pointers are simply copies of addresses from
*        the FrameSet's "map" array. They are not cloned, so should
*        not be annulled by the caller.
*     forward
*        Pointer to the start of an array of int that will be filled
*        with boolean flags (0 or 1) to indicate whether the forward
*        (as opposed to the inverse) transformation should be used for
*        each Mapping returned in order to effect the transformation
*        between the starting and ending nodes. This array should be the
*        same size as the "map" array.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     The function returns one more than the number of Mappings
*     required to perform the transformation, or zero if it was not
*     possible to find a path between the two nodes.

*  Notes:
*     - If a node has an associated Frame, the Frame usually represents a
*     UnitMap and so can be ignored. The exception is if the Frame is
*     actually a Region (or a CmpFrame containing a Region), in which case
*     it represents a Mapping which returns bad values if the input position
*     is outside the region. This form of Mapping should not be ignored, and
*     so the returned list of Mappings includes the effect of any Frames
*     along the path which are not equivalent to a UnitMap. This
*     equivalence is determined by invoking the astSimplify method on the
*     Frame.
*     - A value of zero will be returned if this function is invoked
*     with the global status set, or if it should fail for any reason.
*     - On the assumption that the FrameSet has been consistently
*     constructed, there should be exactly one path between any pair
*     of its nodes.  It should not, therefore, ever fail to find a
*     path except when invoked recursively to explore a subset of the
*     FrameSet's nodes (this should not be visible to an external
*     caller).  Failure to find a path does not in itself result in an
*     error condition.
*/

/* Local Variables: */
   AstFrame *frame;          /* Pointer to Frame associated with inode1 */
   int fwd;                  /* Forward Mapping identified? */
   int inode;                /* Loop counter for nodes */
   int inv;                  /* Inverse Mapping identified? */
   int invert;               /* Original Mapping Invert value */
   int nextra;               /* No. of extra Mappings to add to path */
   int result;               /* Count of mappings (to be returned) */

/* Check the global error status. */
   if ( !astOK ) return 0;

/* See if the two nodes are the same. */
   result = ( inode1 == inode2 );

/* If so, we need to consider the Mapping represented by any Frame
   associated with the node. Most classes of Frames are equivalent to a
   UnitMap and so can be ignored. But some (e.g. the Region class) are not
   equivalent to a UnitMap and so needs to be included in the returned
   Mapping list. */
   if( result ) {
      result = 1;

/* If inode1 is associated with a Frame, which is not equivalent to a
   UnitMap, add the Frame as the first Mapping into the returned list. The
   "forward" value is irrelevant since the forward and inverse transformations
   of Frames are the same. */
      frame = frames[ inode1 ];
      if( frame ) {
         if( !astIsUnitFrame( frame ) ) {
            result++;
            *map = (AstMapping *) frame;
            *forward = 1;
         }
      }

/* If the nodes are different, we now attempt to find the next step in
   the path between them. Loop through all available nodes looking for
   the next one to transform to (i.e. one that is directly related by
   a Mapping to our starting node). */
   } else {
      for ( inode = 0; inode < this->nnode; inode++ ) {

/* Do not consider node "avoid". This prevents us re-tracing our steps
   backwards when this function is invoked recursively. */
         if ( inode != avoid ) {

/* Test if inode is derived from inode1 (if so, the Mapping associated
   with inode will convert from inode1 to inode when applied in the
   forward direction). */
            fwd = ( inode > 0 ) && ( this->link[ inode - 1 ] == inode1 );

/* Test if inode1 is derived from inode (if so, the Mapping associated
   with inode1 will convert from inode1 to inode when applied in the
   inverse direction). */
            inv = ( inode1 > 0 ) && ( this->link[ inode1 - 1 ] == inode );

/* If the nodes are directly related, we try to find a path from inode to
   inode2 without going back through inode1. */
            if ( fwd || inv ) {

/* If node1 is associated with a Frame, we need to include the Frame
   as a Mapping in the returned list unless the Frame is equivalent to a
   UnitMap. Note the number of slots to be reserved for node1 when we call
   Span recursively below. */
               nextra = 1;
               frame = frames[ inode1 ];
               if( frame && !astIsUnitFrame( frame ) ) nextra = 2;

/* Invoke this function recursively to try and find a path from inode
   to inode2 without going back through inode1. If this is possible, a
   non-zero result will be returned. Store the returned Mappings and
   direction information in the arrays supplied, but leave extra space to
   insert information about the Mapping between nodes inode1 and inode. */
               result = Span( this, frames, inode, inode2, inode1,
                              map + nextra, forward + nextra, status );

/* If a path was found, increment the Mapping count to account for the
   one that transforms between nodes inode1 and inode and insert
   information for this Mapping into the output arrays. */
               if ( result ) {
                  result++;
                  nextra--;
                  map[ nextra ] = this->map[ ( fwd ? inode : inode1 ) - 1 ];
                  forward[ nextra ] = fwd;

/* Obtain the original value of the Invert attribute for the Mapping
   between nodes inode1 and inode (recorded when the Mapping was first
   added to the FrameSet). Test if this value has now changed. If so,
   some external code has inverted the Mapping via another pointer, so
   invert the returned direction information to compensate for
   this. */
                  invert = this->invert[ ( fwd ? inode : inode1 ) - 1 ];
                  if ( invert != astGetInvert( map[ nextra ] ) ) {
                     forward[ nextra ] = !forward[ nextra ];
                  }

/* If inode1 is associated with a non-unit Frame Mapping, add the Frame
   Mapping in as the first Mapping in the returned list. The "forward" value
   is irrelevant since the forward and inverse transformations of Frames
   are the same. */
                  if( nextra ) {
                     result++;
                     *map = (AstMapping *) frame;
                     *forward = 1;
                  }

/* Quit searching once a path has been found. */
                  break;
	       }
	    }
         }
      }
   }

/* Return the result, which is one more than the number of mappings
   found (i.e. steps in the path), or zero if no path was found (this
   should only occur when invoked recursively to explore an
   unsuccessful sub-path). */
   return result;
}

static int SubFrame( AstFrame *this_frame, AstFrame *template,
                     int result_naxes,
                     const int *target_axes, const int *template_axes,
                     AstMapping **map, AstFrame **result, int *status ) {
/*
*  Name:
*     SubFrame

*  Purpose:
*     Select axes from a FrameSet and convert to the new coordinate system.

*  Type:
*     Private function.

*  Synopsis:
*     #include "frameset.h"
*     int SubFrame( AstFrame *target, AstFrame *template, int result_naxes,
*                   const int *target_axes, const int *template_axes,
*                   AstMapping **map, AstFrame **result, int *status )

*  Class Membership:
*     FrameSet member function (over-rides the protected astSubFrame
*     method inherited from the Frame class).

*  Description:
*     This function selects a requested sub-set (or super-set) of the
*     axes from the current Frame of a "target" FrameSet and creates a
*     new Frame with copies of the selected axes assembled in the
*     requested order. It then optionally overlays the attributes of a
*     "template" Frame on to the result. It returns both the resulting
*     Frame and a Mapping that describes how to convert between the
*     coordinate systems described by the current Frame of the target
*     FrameSet and the result Frame. If necessary, this Mapping takes
*     account of any differences in the Frames' attributes due to the
*     influence of the template.

*  Parameters:
*     target
*        Pointer to the target FrameSet, from whose current Frame the
*        axes are to be selected.
*     template
*        Pointer to the template Frame, from which new attributes for
*        the result Frame are to be obtained. Optionally, this may be
*        NULL, in which case no overlaying of template attributes will
*        be performed.
*     result_naxes
*        Number of axes to be selected from the target FrameSet. This
*        number may be greater than or less than the number of axes in
*        the FrameSet's current Frame (or equal).
*     target_axes
*        Pointer to an array of int with result_naxes elements, giving
*        a list of the (zero-based) axis indices of the axes to be
*        selected from the current Frame of the target FrameSet. The
*        order in which these are given determines the order in which
*        the axes appear in the result Frame. If any of the values in
*        this array is set to -1, the corresponding result axis will
*        not be derived from the target FrameSet, but will be assigned
*        default attributes instead.
*     template_axes
*        Pointer to an array of int with result_naxes elements. This
*        should contain a list of the template axes (given as
*        zero-based axis indices) with which the axes of the result
*        Frame are to be associated. This array determines which axes
*        are used when overlaying axis-dependent attributes of the
*        template on to the result. If any element of this array is
*        set to -1, the corresponding result axis will not receive any
*        template attributes.
*
*        If the template argument is given as NULL, this array is not
*        used and a NULL pointer may also be supplied here.
*     map
*        Address of a location to receive a pointer to the returned
*        Mapping.  The forward transformation of this Mapping will
*        describe how to convert coordinates from the coordinate
*        system described by the current Frame of the target FrameSet
*        to that described by the result Frame. The inverse
*        transformation will convert in the opposite direction.
*     result
*        Address of a location to receive a pointer to the result Frame.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     A non-zero value is returned if coordinate conversion is
*     possible between the current Frame of the target FrameSet and
*     the result Frame. Otherwise zero is returned and *map and
*     *result are returned as NULL (but this will not in itself result
*     in an error condition). In general, coordinate conversion should
*     always be possible if no template Frame is supplied but may not
*     always be possible otherwise.

*  Notes:
*     - A value of zero will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*/

/* Local Variables: */
   AstFrame *fr;                 /* Pointer to FrameSet's current Frame */
   AstFrameSet *this;            /* Pointer to the FrameSet structure */
   int match;                    /* Result to be returned */

/* Initialise. */
   *map = NULL;
   *result = NULL;
   match = 0;

/* Check the global error status. */
   if ( !astOK ) return match;

/* Obtain a pointer to the FrameSet structure. */
   this = (AstFrameSet *) this_frame;

/* Obtain a pointer to the FrameSet's current Frame. */
   fr = astGetFrame( this, AST__CURRENT );

/* Invoke the astSubFrame method for this Frame. */
   match = astSubFrame( fr, template, result_naxes, target_axes, template_axes,
                        map, result );

/* Annul the Frame pointer. */
   fr = astAnnul( fr );

/* If an error occurred, clean up by annulling any returned objects and clear
   the returned result. */
   if ( !astOK ) {
      *map = astAnnul( *map );
      *result = astAnnul( *result );
      match = 0;
   }

/* Return the result. */
   return match;
}

static AstSystemType SystemCode( AstFrame *this_frame, const char *system, int *status ) {
/*
*  Name:
*     SystemCode

*  Purpose:
*     Convert a string into a coordinate system type code.

*  Type:
*     Private function.

*  Synopsis:
*     #include "frameset.h"
*     AstSystemType SystemCode( AstFrame *this, const char *system, int *status )

*  Class Membership:
*     FrameSet member function (over-rides the protected astSystemCode
*     method inherited from the Frame class).

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
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     The System type code.

*  Notes:
*     - A value of AST__BADSYSTEM is returned if the coordinate system
*     description was not recognised. This does not produce an error.
*     - A value of AST__BADSYSTEM is also returned if this function
*     is invoked with the global error status set or if it should fail
*     for any reason.
*/

/* Local Variables: */
   AstSystemType result;      /* Result value to return */
   AstFrame *fr;              /* Pointer to FrameSet's current Frame */
   AstFrameSet *this;         /* Pointer to the FrameSet structure */

/* Initialise. */
   result = AST__BADSYSTEM;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Obtain a pointer to the FrameSet structure. */
   this = (AstFrameSet *) this_frame;

/* Obtain a pointer to the FrameSet's current Frame and invoke the
   astSystemCode method for this Frame. Annul the Frame pointer afterwards. */
   fr = astGetFrame( this, AST__CURRENT );
   result = astSystemCode( fr, system );
   fr = astAnnul( fr );

/* If an error occurred, clear the result value. */
   if ( !astOK ) result = AST__BADSYSTEM;

/* Return the result. */
   return result;
}

static const char *SystemString( AstFrame *this_frame, AstSystemType system, int *status ) {
/*
*  Name:
*     SystemString

*  Purpose:
*     Convert a coordinate system type code into a string.

*  Type:
*     Private function.

*  Synopsis:
*     #include "frameset.h"
*     const char *SystemString( AstFrame *this, AstSystemType system, int *status )

*  Class Membership:
*     FrameSet member function (over-rides the protected astSystemString
*     method inherited from the Frame class).

*  Description:
*     This function converts a Frame coordinate system type code
*     (System attribute value) into a string suitable for use as an
*     external representation of the coordinate system type.

*  Parameters:
*     this
*        Pointer to the Frame.
*     system
*        The coordinate system type code.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     Pointer to a constant null-terminated string containing the
*     textual equivalent of the type code supplied.

*  Notes:
*     - A NULL pointer value is returned if the coordinate system
*     code was not recognised. This does not produce an error.
*     - A NULL pointer value is also returned if this function is
*     invoked with the global error status set or if it should fail
*     for any reason.
*/

/* Local Variables: */
   AstFrame *fr;                 /* Pointer to FrameSet's current Frame */
   AstFrameSet *this;            /* Pointer to the FrameSet structure */
   const char *result;           /* Pointer value to return */

/* Initialise. */
   result = NULL;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Obtain a pointer to the FrameSet structure. */
   this = (AstFrameSet *) this_frame;

/* Obtain a pointer to the FrameSet's current Frame and invoke the
   astSystemString method for this Frame. Annul the Frame pointer
   afterwards. */
   fr = astGetFrame( this, AST__CURRENT );
   result = astSystemString( fr, system );
   fr = astAnnul( fr );

/* If an error occurred, clear the result value. */
   if ( !astOK ) result = NULL;

/* Return the result pointer. */
   return result;

}

static int TestAttrib( AstObject *this_object, const char *attrib, int *status ) {
/*
*  Name:
*     TestAttrib

*  Purpose:
*     Test if a specified attribute value is set for a FrameSet.

*  Type:
*     Private function.

*  Synopsis:
*     #include "frameset.h"
*     int TestAttrib( AstObject *this, const char *attrib, int *status )

*  Class Membership:
*     FrameSet member function (over-rides the astTestAttrib protected
*     method inherited from the Frame class).

*  Description:
*     This function returns a boolean result (0 or 1) to indicate whether
*     a value has been set for one of a FrameSet's attributes.

*  Parameters:
*     this
*        Pointer to the FrameSet.
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
   AstFrame *fr;                 /* Pointer to current Frame */
   AstFrameSet *this;            /* Pointer to the FrameSet structure */
   int result;                   /* Result value to return */

/* Initialise. */
   result = 0;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Obtain a pointer to the FrameSet structure. */
   this = (AstFrameSet *) this_object;

/* Check the attribute name and test the appropriate attribute. */

/* We first handle attributes that apply to the FrameSet as a whole
   (rather than to the current Frame). */

/* Base. */
/* ----- */
   if ( !strcmp( attrib, "base" ) ) {
      result = astTestBase( this );

/* Current. */
/* -------- */
   } else if ( !strcmp( attrib, "current" ) ) {
      result = astTestCurrent( this );

/* ID. */
/* --- */
   } else if ( !strcmp( attrib, "id" ) ) {
      result = astTestID( this );

/* Ident. */
/* ------ */
   } else if ( !strcmp( attrib, "ident" ) ) {
      result = astTestIdent( this );

/* Invert. */
/* ------- */
   } else if ( !strcmp( attrib, "invert" ) ) {
      result = astTestInvert( this );

/* Report. */
/* ------- */
   } else if ( !strcmp( attrib, "report" ) ) {
      result = astTestReport( this );

/* If the name is not recognised, test if it matches any of the
   read-only attributes of this class. If it does, then return
   zero. */
   } else if ( !strcmp( attrib, "class" ) ||
               !strcmp( attrib, "nframe" ) ||
               !strcmp( attrib, "nin" ) ||
               !strcmp( attrib, "nobject" ) ||
               !strcmp( attrib, "nout" ) ||
               !strcmp( attrib, "refcount" ) ||
               !strcmp( attrib, "tranforward" ) ||
               !strcmp( attrib, "traninverse" ) ) {
      result = 0;

/* Pass unrecognised attributes on to the FrameSet's current Frame for
   further interpretation. */
   } else {

/* Obtain a pointer to the current Frame and invoke its astTestAttrib
   method. Annul the Frame pointer afterwards. */
      fr = astGetFrame( this, AST__CURRENT );
      result = astTestAttrib( fr, attrib );
      fr = astAnnul( fr );
   }

/* If an error occurred, clear the result value. */
   if ( !astOK ) result = 0;

/* Return the result, */
   return result;
}

static int TestBase( AstFrameSet *this, int *status ) {
/*
*+
*  Name:
*     astTestBase

*  Purpose:
*     Determine if a value has been set for the Base attribute of a FrameSet.

*  Type:
*     Protected virtual function.

*  Synopsis:
*     #include "frameset.h"
*     int astTestBase( AstFrameSet *this )

*  Class Membership:
*     FrameSet method.

*  Description:
*     This function returns a boolean result to indicate if a value
*     has been set for the Base attribute of a FrameSet. This
*     attribute is an index that identifies the base Frame in the
*     FrameSet.

*  Parameters:
*     this
*        Pointer to the FrameSet.

*  Returned Value:
*     Zero or 1, depending on whether a value has been set.

*  Notes:
*     - A value of zero will be returned if this function is invoked
*     with the global status set, or if it should fail for any reason.
*-
*/

/* Local Variables: */
   int invert;                   /* FrameSet is inverted? */
   int result;                   /* Value to return */

/* Initialise. */
   result = 0;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Determine if the FrameSet has been inverted. */
   invert = astGetInvert( this );

/* If it has not been inverted, test the base Frame index, otherwise
   test the index of the current Frame instead. */
   if ( astOK ) {
      if ( !invert ) {
         result = ( this->base != -INT_MAX );
      } else {
         result = ( this->current != -INT_MAX );
      }
   }

/* Return the result. */
   return result;
}

static int TestCurrent( AstFrameSet *this, int *status ) {
/*
*+
*  Name:
*     astTestCurrent

*  Purpose:
*     Test if a value has been set for the Current attribute of a FrameSet.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "frameset.h"
*     int astTestCurrent( AstFrameSet *this )

*  Class Membership:
*     FrameSet method.

*  Description:
*     This function returns a boolean result to indicate whether a
*     value has been set for the Current attribute of a FrameSet.
*     This attribute is an index that identifies the current Frame in
*     a FrameSet.

*  Parameters:
*     this
*        Pointer to the FrameSet.

*  Returned Value:
*     Zero or 1, depending on whether a value has been set.

*  Notes:
*     - A value of zero will be returned if this function is invoked
*     with the global status set, or if it should fail for any reason.
*-
*/

/* Local Variables: */
   int invert;                   /* FrameSet is inverted? */
   int result;                   /* Value to return */

/* Initialise. */
   result = 0;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Determine if the FrameSet has been inverted. */
   invert = astGetInvert( this );

/* If it has not been inverted, test the current Frame index,
   otherwise test the index of the base Frame instead. */
   if ( astOK ) {
      if ( !invert ) {
         result = ( this->current != -INT_MAX );
      } else {
         result = ( this->base != -INT_MAX );
      }
   }

/* Return the result. */
   return result;
}

static void TidyNodes( AstFrameSet *this, int *status ) {
/*
*  Name:
*     TidyNodes

*  Purpose:
*     Tidy the nodes in a FrameSet.

*  Type:
*     Private function.

*  Synopsis:
*     #include "frameset.h"
*     void TidyNodes( AstFrameSet *this, int *status )

*  Class Membership:
*     FrameSet member function.

*  Description:
*     This function tidies the nodes in a FrameSet, removing any that
*     are unnecessary or represent dead-ends. It should be used after
*     any changes have been made to a FrameSet that may have reduced
*     the number of references to any of its nodes (either by Frames
*     or by other nodes).

*  Parameters:
*     this
*        Pointer to the FrameSet.
*     status
*        Pointer to the inherited status variable.
*/

/* Local Variables: */
   AstMapping *newmap;           /* Pointer to simplified Mapping */
   AstMapping *tmpmap;           /* Pointer to new compound Mapping */
   int ifr;                      /* Loop counter for Frames */
   int inode;                    /* Loop counter for nodes */
   int last_link[ 2 ];           /* Last nodes to reference via "link" array */
   int link_ref;                 /* Number of "link" array references */
   int needed;                   /* Node still required? */
   int next;                     /* Node which references the removed one */
   int remove;                   /* Node to be removed */
   int suspect;                  /* Loop counter for testing nodes */

/* Check the global error status. */
   if ( !astOK ) return;

/* Loop to search for unnecessary nodes until no more are found. */
   needed = 0;
   while ( !needed ) {

/* Inspect each node (including node zero, which does not actually
   have a Mapping associated with it) to see how many times it is
   referenced. */
      for ( suspect = 0; suspect < this->nnode; suspect++ ) {
         link_ref = 0;

/* Test for at least one reference from within the "node" array which
   associates Frames with particular nodes. */
         for ( ifr = 1; ifr <= this->nframe; ifr++ ) {
            if ( ( needed = ( this->node[ ifr - 1 ] == suspect ) ) ) break;
         }

/* If no references were found above, look for references in the
   "link" array that inter-connects all the nodes. */
         if ( !needed ) {
            for ( inode = 1; inode < this->nnode; inode ++ ) {
               if ( this->link[ inode - 1 ] == suspect ) {

/* Node zero must be retained if it has more than two links
   referencing it, while other nodes only require more than one. */
                  if ( ( needed = ( link_ref >= ( suspect ? 1 : 2 ) ) ) ) break;

/* Remember (up to) the first two nodes which reference the current one. */
                  last_link[ link_ref++ ] = inode;
               }
            }
         }

/* If there were insufficient references to retain this node, we must
   now decide why it should be removed. */
         if ( !needed ) {

/* If there is no Frame associated with a node and there are less than
   two links to it (for node zero), or less then one link (for other
   nodes), then the there is no route to anything else via this node.
   It is a dead-end. */
            if ( link_ref < ( suspect ? 1 : 2 ) ) {

/* To tidy up, we remove the affected node or, for node zero, the
   remaining one that references it. Annul the Mapping associated with
   the node being removed. */
               remove = suspect ? suspect : last_link[ 0 ];
               this->map[ remove - 1 ] = astAnnul( this->map[ remove - 1 ] );

/* If an unnecessary node is not a dead-end, then it is a redundant
   node which simply joins two Mappings. */
            } else {

/* To tidy up, we remove the affected node or, for node zero, the
   first one that references it. */
               remove = suspect ? suspect : last_link[ 0 ];

/* We then produce a compound Mapping which spans the gap by
   concatenating the Mappings associated with the node being removed
   and the remaining one which references it. For node zero, the first
   of these Mappings must be inverted because there are no out-going
   Mappings from node zero. */
               next = suspect ? last_link[ 0 ] : last_link[ 1 ];
               tmpmap = CombineMaps( this->map[ remove - 1 ],
                                     this->invert[ remove - 1 ] != !suspect,
                                     this->map[ next - 1 ],
                                     this->invert[ next - 1 ], 1, status );

/* Simplify this compound Mapping. */
               newmap = astSimplify( tmpmap );
               tmpmap = astAnnul( tmpmap );

/* Annul the individual Mapping pointers. */
               this->map[ remove - 1 ] = astAnnul( this->map[ remove - 1 ] );
               this->map[ next - 1 ] = astAnnul( this->map[ next - 1 ] );

/* Install the new compound Mapping and its Invert flag. */
               this->map[ next - 1 ] = newmap;
               this->invert[ next - 1 ] = astGetInvert( newmap );

/* Transfer the "link" value from the removed node to the one which
   takes its place. */
               this->link[ next - 1 ] = this->link[ remove - 1 ];
            }

/* Loop to move all subsequent node data down in the "map", "invert"
   and "link" arrays to close the gap where a node has been
   removed. */
            for ( inode = remove; inode < this->nnode - 1; inode ++ ) {
               this->map [ inode - 1 ] = this->map[ inode ];
               this->link [ inode - 1 ] = this->link[ inode ];
               this->invert[ inode - 1 ] = this->invert[ inode ];
            }
            this->map[ this->nnode - 2 ] = NULL;
            this->link[ this->nnode - 2 ] = -1;
            this->invert[ this->nnode - 2 ] = -1;

/* Decrement the node count. */
            this->nnode--;

/* Loop to adjust each entry in the "node" array for the change in
   node numbering, re-directing references to the removed node towards
   the new node zero. */
            for ( ifr = 1; ifr <= this->nframe; ifr++ ) {
               if ( this->node[ ifr - 1 ] > remove ) {
                  this->node[ ifr - 1 ]--;
               } else if ( this->node[ ifr - 1 ] == remove ) {
                  this->node[ ifr - 1 ] = 0;
               }
            }

/* Similarly adjust each entry in the "link" array. */
            for ( inode = 1; inode < this->nnode; inode++ ) {
               if ( this->link[ inode - 1 ] > remove ) {
                  this->link[ inode - 1 ]--;
               } else if ( this->link[ inode - 1 ] == remove ) {
                  this->link[ inode - 1 ] = 0;
               }
            }

/* Once a node has been removed, other nodes (perhaps already tested)
   may no longer be needed, so quit the testing loop and start testing
   again with node zero. The process terminates when no more
   unnecessary nodes can be found. */
            break;
         }
      }
   }
}

static AstPointSet *Transform( AstMapping *this_mapping, AstPointSet *in,
                               int forward, AstPointSet *out, int *status ) {
/*
*  Name:
*     Transform

*  Purpose:
*     Transform a set of points.

*  Type:
*     Private function.

*  Synopsis:
*     #include "frameset.h"
*     AstPointSet *Transform( AstMapping *this, AstPointSet *in,
*                             int forward, AstPointSet *out, int *status )

*  Class Membership:
*     FrameSet member function (over-rides the astTransform method
*     inherited from the Frame class).

*  Description:
*     This function takes a FrameSet and a set of points encapsulated
*     in a PointSet, and applies either the forward or inverse
*     coordinate transformation (if defined by the FrameSet) to the
*     points. The forward transformation converts between the
*     FrameSet's base Frame and its current Frame, while the inverse
*     transformation converts in the opposite direction.

*  Parameters:
*     this
*        Pointer to the FrameSet.
*     in
*        Pointer to the PointSet holding the input coordinate data.
*     forward
*        A non-zero value indicates that the forward coordinate transformation
*        should be applied, while a zero value requests the inverse
*        transformation.
*     out
*        Pointer to a PointSet which will hold the transformed (output)
*        coordinate values. A NULL value may also be given, in which case a
*        new PointSet will be created by this function.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     Pointer to the output (possibly new) PointSet.

*  Notes:
*     - An error will result if the FrameSet supplied does not define
*     the requested coordinate transformation (either forward or
*     inverse).
*     - The number of coordinate values per point in the input
*     PointSet must match the number of input coordinates for the
*     FrameSet being applied (or number of output coordinates if the
*     inverse transformation is requested).  This will be equal to the
*     number of axes in the FrameSet's base Frame (or the current
*     Frame for the inverse transformation).
*     - If an output PointSet is supplied, it must have space for
*     sufficient number of points and coordinate values per point to
*     accommodate the result (e.g. the number of FrameSet output
*     coordinates, or number of input coordinates if the inverse
*     transformation is requested). Any excess space will be ignored.
*     - A null pointer will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*/

/* Local Variables: */
   AstFrameSet *this;            /* Pointer to the FrameSet structure */
   AstMapping *map;              /* Pointer to the base->current Mapping */
   AstPointSet *result;          /* Pointer value to return */

/* Check the global error status. */
   if ( !astOK ) return NULL;

/* Obtain a pointer to the FrameSet structure. */
   this = (AstFrameSet *) this_mapping;

/* Obtain the Mapping between the base and current Frames in the
   FrameSet (note this takes account of whether the FrameSet has been
   inverted). */
   map = astGetMapping( this, AST__BASE, AST__CURRENT );

/* Apply the Mapping to the input PointSet. */
   result = astTransform( map, in, forward, out );

/* Annul the Mapping pointer. */
   map = astAnnul( map );

/* If an error has occurred and a new PointSet may have been created, then
   clean up by annulling it. In any case, ensure that a NULL result is
   returned.*/
   if ( !astOK ) {
      if ( !out ) result = astAnnul( result );
      result = NULL;
   }

/* Return a pointer to the output PointSet. */
   return result;
}

static int Unformat( AstFrame *this_frame, int axis, const char *string,
                     double *value, int *status ) {
/*
*  Name:
*     Unformat

*  Purpose:
*     Read a formatted coordinate value for a FrameSet axis.

*  Type:
*     Private function.

*  Synopsis:
*     #include "frameset.h"
*     int Unformat( AstFrame *this, int axis, const char *string,
*                   double *value, int *status )

*  Class Membership:
*     FrameSet member function (over-rides the public astUnformat
*     method inherited from the Frame class).

*  Description:
*     This function reads a formatted coordinate value for a FrameSet
*     axis (supplied as a string) and returns the equivalent numerical
*     value as a double. It also returns the number of characters read
*     from the string.

*  Parameters:
*     this
*        Pointer to the FrameSet.
*     axis
*        The number of the FrameSet axis for which the coordinate
*        value is to be read (axis numbering starts at zero for the
*        first axis).
*     string
*        Pointer to a constant null-terminated string containing the
*        formatted coordinate value.
*     value
*        Pointer to a double in which the coordinate value read will be
*        returned.
*     status
*        Pointer to the inherited status variable.

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
*/

/* Local Variables: */
   AstFrame *fr;                 /* Pointer to current Frame */
   AstFrameSet *this;            /* Pointer to the FrameSet structure */
   double coord;                 /* Coordinate value read */
   int nc;                       /* Number of characters read */

/* Initialise. */
   nc = 0;

/* Check the global error status. */
   if ( !astOK ) return nc;

/* Obtain a pointer to the FrameSet structure. */
   this = (AstFrameSet *) this_frame;

/* Validate the axis index. */
   (void) astValidateAxis( this, axis, 1, "astUnformat" );

/* Obtain a pointer to the FrameSet's current Frame and invoke the
   astUnformat method for this Frame. Annul the Frame pointer
   afterwards. */
   fr = astGetFrame( this, AST__CURRENT );
   nc = astUnformat( fr, axis, string, &coord );
   fr = astAnnul( fr );

/* If an error occurred, clear the number of characters read. */
   if ( !astOK ) {
      nc = 0;

/* Otherwise, if characters were read, return the coordinate value. */
   } else if ( nc ) {
      *value = coord;
   }

/* Return the number of characters read. */
   return nc;
}

static int ValidateAxis( AstFrame *this_frame, int axis, int fwd,
                         const char *method, int *status ) {
/*
*  Name:
*     ValidateAxis

*  Purpose:
*     Validate and permute a FrameSet's axis index.

*  Type:
*     Private function.

*  Synopsis:
*     #include "frameset.h"
*     int ValidateAxis( AstFrame *this, int axis, int fwd, const char *method,
*                       int *status )

*  Class Membership:
*     FrameSet member function (over-rides the protected
*     astValidateAxis method inherited from the Frame class).

*  Description:
*     This function checks the validity of an index (zero-based) which
*     is to be used to address one of the coordinate axes of the
*     current Frame in a FrameSet. If the index is valid, it is
*     permuted using the axis permutation array associated with the
*     FrameSet's current Frame and the (zero-based) permuted axis
*     index is returned.  This gives the index the axis had when the
*     Frame was first created. If the axis index supplied is not
*     valid, an error is reported and the global error status is set.

*  Parameters:
*     this
*        Pointer to the FrameSet.
*     axis
*        The axis index (zero-based) to be checked. To be valid, it
*        must lie between zero and (naxes-1) inclusive, where "naxes"
*        is the number of coordinate axes associated with the
*        FrameSet's current Frame.
*     fwd
*        If non-zero, the suppplied axis index is assumed to be an
*        "external" axis index, and the corresponding "internal" axis index
*        is returned as the function value. Otherwise, the suppplied axis
*        index is assumed to be an "internal" axis index, and the
*        corresponding "external" axis index is returned as the function
*        value.
*     method
*        Pointer to a constant null-terminated character string
*        containing the name of the method that invoked this function
*        to validate an axis index. This method name is used solely
*        for constructing error messages.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     The permuted axis index - either "internal" or "external" as
*     specified by "fwd".

*  Notes:
*     - A value of zero will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*/

/* Local Variables: */
   AstFrame *fr;                 /* Pointer to current Frame */
   AstFrameSet *this;            /* Pointer to the FrameSet structure */
   int naxes;                    /* Number of FrameSet axes */
   int result;                   /* Permuted axis index */

/* Initialise. */
   result = 0;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Obtain a pointer to the FrameSet structure. */
   this = (AstFrameSet *) this_frame;

/* Determine the number of FrameSet axes. */
   naxes = astGetNaxes( this );
   if ( astOK ) {

/* If the FrameSet has no axes, report an error (convert to 1-based
   axis numbering for the benefit of the public interface). */
      if ( naxes == 0 ) {
         astError( AST__AXIIN, "%s(%s): Invalid attempt to use an axis index "
                   "(%d) for a %s which has no axes.", status, method,
                   astGetClass( this ), axis + 1, astGetClass( this ) );

/* Otherwise, check the axis index for validity and report an error if
   it is not valid (again, convert to 1-based axis numbering). */
      } else if ( ( axis < 0 ) || ( axis >= naxes ) ) {
         astError( AST__AXIIN, "%s(%s): Axis index (%d) invalid - it should "
                   "be in the range 1 to %d.", status, method, astGetClass( this ),
                   axis + 1, naxes );

/* If the axis index was valid, obtain a pointer to the FrameSet's
   current Frame and invoke this Frame's astValidateAxis method to
   obtain the permuted axis index. Annul the Frame pointer
   afterwards. */
      } else {
         fr = astGetFrame( this, AST__CURRENT );
         result = astValidateAxis( fr, axis, fwd, "astValidateAxis" );
         fr = astAnnul( fr );
      }
   }

/* If an error occurred, clear the result value. */
   if ( !astOK ) result = 0;

/* Return the result. */
   return result;
}

static void ValidateAxisSelection( AstFrame *this_frame, int naxes,
                                   const int *axes, const char *method, int *status ) {
/*
*  Name:
*     ValidateAxisSelection

*  Purpose:
*     Check that a set of axes selected from a Frame is valid.

*  Type:
*     Private function.

*  Synopsis:
*     #include "frameset.h"
*     void ValidateAxisSelection( AstFrame *this, int naxes,
*                                 const int *axes, const char *method, int *status )

*  Class Membership:
*     FrameSet member function (over-rides the protected astValidateAxisSelection
*     method inherited from the Frame class).

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
*     status
*        Pointer to the inherited status variable.
*/

/* Local Variables: */
   AstFrame *fr;                 /* Pointer to current Frame */
   AstFrameSet *this;            /* Pointer to the FrameSet structure */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain a pointer to the FrameSet structure. */
   this = (AstFrameSet *) this_frame;

/* Obtain a pointer to the FrameSet's current Frame and invoke this
   Frame's astValidateAxisSelection method. Annul the Frame pointer
   afterwards. */
   fr = astGetFrame( this, AST__CURRENT );
   astValidateAxisSelection( fr, naxes, axes, method );
   fr = astAnnul( fr );

}

static int ValidateFrameIndex( AstFrameSet *this, int iframe,
                               const char *method, int *status ) {
/*
*+
*  Name:
*     astValidateFrameIndex

*  Purpose:
*     Validate a FrameSet Frame index number.

*  Type:
*     Protected virtual function.

*  Synopsis:
*     #include "frameset.h"
*     int astValidateFrameIndex( AstFrameSet *this, int iframe,
*                                const char *method )

*  Class Membership:
*     FrameSet method.

*  Description:
*     This function checks a (one-based) FrameSet Frame index for
*     validity. If it is not valid, an error is reported. Otherwise,
*     the function returns the Frame index value, having translated
*     the special values AST__CURRENT and AST__BASE into valid Frame
*     indices if necessary.

*  Parameters:
*     this
*        Pointer to the FrameSet.
*     iframe
*        The Frame index. To be valid this should lie in the range 1
*        to the number of Frames in the FrameSet.  In addition, the
*        values AST__CURRENT and AST__BASE may be given to indicate
*        the "current" and "base" Frames. These values will be
*        translated into the acceptable range.
*     method
*        Pointer to a constant null-terminated character string
*        containing the name of the method that invoked this function
*        to validate a Frame index. This method name is used solely
*        for constructing error messages.

*  Returned Value:
*     The validated (one-based) Frame index.

*  Notes:
*     - A value of zero will be returned if this function is invoked
*     with the global status set, or if it should fail for any reason
*     (e.g. if the Frame index is invalid).
*-
*/

/* Local Variables: */
   int nframe;                   /* Number of Frames */
   int result;                   /* Returned index value */

/* Initialise. */
   result = 0;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Check if the base or current Frame was specified and retrieve the
   required Frame index from the FrameSet. */
   if ( iframe == AST__BASE ) {
      result = astGetBase( this );
   } else if ( iframe == AST__CURRENT ) {
      result = astGetCurrent( this );

/* Otherwise, determine how many Frames there are in the FrameSet. */
   } else {
      nframe = astGetNframe( this );
      if ( astOK ) {

/* Check that the supplied index is within range and report an error
   if it is not. */
         if ( ( iframe < 1 ) || ( iframe > nframe ) ) {
            astError( AST__FRMIN, "%s(%s): Invalid Frame index (%d) given.", status,
                      method, astGetClass( this ), iframe );
            astError( AST__FRMIN, "This value should be in the range 1 to "
                      "%d (or AST__CURRENT or AST__BASE).", status, nframe );

/* If OK, return the validated index value. */
         } else {
            result = iframe;
         }
      }
   }

/* Return the result. */
   return result;
}

static int ValidateSystem( AstFrame *this_frame, AstSystemType system, const char *method, int *status ) {
/*
*  Name:
*     ValidateSystem

*  Purpose:
*     Validate a value for a Frame's System attribute.

*  Type:
*     Private function.

*  Synopsis:
*     #include "frameset.h"
*     int ValidateSystem( AstFrame *this, AstSystemType system,
*                         const char *method, int *status )

*  Class Membership:
*     FrameSet member function (over-rides the protected astValidateSystem
*     method inherited from the Frame class).

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
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     The validated system value.

*  Notes:
*     - A value of AST_BADSYSTEM will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*-
*/

/* Local Variables: */
   AstSystemType result;      /* Validated system value */
   AstFrame *fr;              /* Pointer to FrameSet's current Frame */
   AstFrameSet *this;         /* Pointer to the FrameSet structure */

/* Initialise. */
   result = AST__BADSYSTEM;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Obtain a pointer to the FrameSet structure. */
   this = (AstFrameSet *) this_frame;

/* Obtain a pointer to the FrameSet's current Frame and invoke the
   astValidateSystem method for this Frame. Annul the Frame pointer
   afterwards. */
   fr = astGetFrame( this, AST__CURRENT );
   result = astValidateSystem( this, system, method );
   fr = astAnnul( fr );

/* If an error occurred, clear the result value. */
   if ( !astOK ) result = AST__BADSYSTEM;

/* Return the result. */
   return result;
}

static void VSet( AstObject *this_object, const char *settings,
                  char **text, va_list args, int *status ) {
/*
*  Name:
*     VSet

*  Purpose:
*     Set values for a FrameSet's attributes.

*  Type:
*     Private function.

*  Synopsis:
*     #include "frameset.h"
*     void VSet( AstObject *this, const char *settings, char **text,
*                va_list args, int *status )

*  Class Membership:
*     FrameSet member function (over-rides the protected astVSet
*     method inherited from the Object class).

*  Description:
*     This function assigns a set of attribute values for a FrameSet,
*     the attributes and their values being specified by means of a
*     string containing a comma-separated list of the form:
*
*        "attribute1 = value1, attribute2 = value2, ... "
*
*     Here, "attribute" specifies an attribute name and the value to
*     the right of each "=" sign should be a suitable textual
*     representation of the value to be assigned to that
*     attribute. This will be interpreted according to the attribute's
*     data type.
*
*     The string supplied may also contain "printf"-style format
*     specifiers identified by a "%" sign in the usual way. If
*     present, these will be substituted by values supplied as
*     optional arguments (as a va_list variable argument list), using
*     the normal "printf" rules, before the string is used.

*  Parameters:
*     this
*        Pointer to the FrameSet.
*     settings
*        Pointer to a null-terminated string containing a
*        comma-separated list of attribute settings.
*     text
*        Pointer to a location at which to return a pointer to dynamic
*        memory holding a copy of the expanded setting string. This memory
*        should be freed using astFree when no longer needed. If a NULL
*        pointer is supplied, no string is created.
*     args
*        The variable argument list which contains values to be
*        substituted for any "printf"-style format specifiers that
*        appear in the "settings" string.
*     status
*        Pointer to the inherited status variable.

*  Notes:
*     - This function preserves the integrity of the FrameSet (if
*     possible) by appropriately remapping its current Frame to take
*     account of its changed attribute values.
*/

/* Local Variables: */
   astDECLARE_GLOBALS            /* Declare the thread specific global data */
   AstFrame *save_frame;         /* Saved pointer to integrity Frame */
   AstFrameSet *this;            /* Pointer to FrameSet structure */
   char *fulltext;               /* Pointer to expanded text string */
   const char *save_method;      /* Saved pointer to method name */
   int len;                      /* Length of settings string */
   int ok;                       /* Status OK? */
   int save_lost;                /* Saved integrity modified flag */

/* Initialise */
   if( text ) *text = NULL;

/* Check the global error status. */
   if ( !astOK ) return;

/* Get a pointer to the structure holding thread-specific global data. */
   astGET_GLOBALS(this_object);

/* Obtain the length of the "settings" string and test it is not
   zero. If it is, there is nothing more to do. */
   len = (int) strlen( settings );
   if ( len != 0 ) {

/* Obtain a pointer to the FrameSet structure. */
      this = (AstFrameSet *) this_object;

/* This function may be invoked recursively (because astConvert,
   below, constructs a FrameSet and may require that its attributes be
   set). To allow this, we first save any existing FrameSet integrity
   information in local variables. */
      save_frame = integrity_frame;
      save_lost = integrity_lost;
      save_method = integrity_method;

/* Set the name of the method being used (for use in error
   messages). */
      integrity_method = "astSet";

/* Record the initial integrity state of the FrameSet. */
      RecordIntegrity( this, status );

/* Invoke the parent astVSet method to set the FrameSet's attribute
   values and note if this succeeds. */
      (*parent_vset)( this_object, settings, &fulltext, args, status );
      ok = astOK;

/* Restore the FrameSet's integrity. */
      RestoreIntegrity( this, status );

/* If integrity could not be restored, then add contextual error
   information. */
      if ( !astOK && ok ) {

/* Display the message. */
         astError( astStatus, "Unable to accommodate the attribute setting "
                               "\"%s\".", status, fulltext );
      }

/* Restore any saved FrameSet integrity information. */
      integrity_frame = save_frame;
      integrity_lost = save_lost;
      integrity_method = save_method;

/* If the full text of the setting string is not needed, free it.
   Otherwise return it. */
      if( text ) {
         *text = fulltext;
      } else {
         fulltext = astFree( fulltext );
      }
   }
}

/* FrameSet Attributes. */
/* -------------------- */
/*
*att++
*  Name:
*     Base

*  Purpose:
*     FrameSet base Frame index.

*  Type:
*     Public attribute.

*  Synopsis:
*     Integer.

*  Description:
*     This attribute gives the index of the Frame which is to be
*     regarded as the "base" Frame within a FrameSet. The default is
*     the first Frame added to the FrameSet when it is created (this
*     Frame always has an index of 1).

*  Applicability:
*     FrameSet
*        All FrameSets have this attribute.

*  Notes:
*     - Inverting a FrameSet (inverting the boolean sense of its
c     Invert attribute, with the astInvert function for example) will
f     Invert attribute, with the AST_INVERT routine for example) will
*     interchange the values of its Base and Current attributes.
*att--
*/

/*
*att++
*  Name:
*     Current

*  Purpose:
*     FrameSet current Frame index.

*  Type:
*     Public attribute.

*  Synopsis:
*     Integer.

*  Description:
*     This attribute gives the index of the Frame which is to be
*     regarded as the "current" Frame within a FrameSet. The default
*     is the most recent Frame added to the FrameSet (this Frame
*     always has an index equal to the FrameSet's Nframe attribute).

*  Applicability:
*     FrameSet
*        All FrameSets have this attribute.

*  Notes:
*     - Inverting a FrameSet (inverting the boolean sense of its
c     Invert attribute, with the astInvert function for example) will
f     Invert attribute, with the AST_INVERT routine for example) will
*     interchange the values of its Base and Current attributes.
*att--
*/

/*
*att++
*  Name:
*     Nframe

*  Purpose:
*     Number of Frames in a FrameSet.

*  Type:
*     Public attribute.

*  Synopsis:
*     Integer, read-only.

*  Description:
*     This attrbute gives the number of Frames in a FrameSet. This
*     value will change as Frames are added or removed, but will
*     always be at least one.

*  Applicability:
*     FrameSet
*        All FrameSets have this attribute.
*att--
*/

/* Access to attributes of the current Frame. */
/* ------------------------------------------ */
/* Use the macros defined at the start of this file to implement
   private member functions that give access to the attributes of the
   current Frame of a FrameSet and its axes. These functions over-ride
   the attribute access methods inherited from the Frame class. */

/* Clear, Get, Set and Test axis-independent Frame attributes. */
MAKE_CLEAR(Digits)
MAKE_CLEAR(Domain)
MAKE_CLEAR(MatchEnd)
MAKE_CLEAR(MaxAxes)
MAKE_CLEAR(MinAxes)
MAKE_CLEAR(Permute)
MAKE_CLEAR(PreserveAxes)
MAKE_CLEAR(Title)

MAKE_GET(Digits,int)
MAKE_GET(Domain,const char *)
MAKE_GET(MatchEnd,int)
MAKE_GET(MaxAxes,int)
MAKE_GET(MinAxes,int)
MAKE_GET(Permute,int)
MAKE_GET(PreserveAxes,int)
MAKE_GET(Title,const char *)
MAKE_SET(Digits,int)
MAKE_SET(Domain,const char *)
MAKE_SET(MatchEnd,int)
MAKE_SET(MaxAxes,int)
MAKE_SET(MinAxes,int)
MAKE_SET(Permute,int)
MAKE_SET(PreserveAxes,int)
MAKE_SET(Title,const char *)
MAKE_TEST(Digits)
MAKE_TEST(Domain)
MAKE_TEST(MatchEnd)
MAKE_TEST(MaxAxes)
MAKE_TEST(MinAxes)
MAKE_TEST(Permute)
MAKE_TEST(PreserveAxes)
MAKE_TEST(Title)

MAKE_GET(ActiveUnit,int)
MAKE_SET(ActiveUnit,int)
MAKE_TEST(ActiveUnit)

MAKE_GET(System,AstSystemType)
MAKE_SET(System,AstSystemType)
MAKE_TEST(System)
MAKE_CLEAR(System)

MAKE_GET(AlignSystem,AstSystemType)
MAKE_SET(AlignSystem,AstSystemType)
MAKE_TEST(AlignSystem)
MAKE_CLEAR(AlignSystem)

MAKE_GET(Epoch,double)
MAKE_SET(Epoch,double)
MAKE_TEST(Epoch)
MAKE_CLEAR(Epoch)

MAKE_GET(ObsLon,double)
MAKE_SET(ObsLon,double)
MAKE_TEST(ObsLon)
MAKE_CLEAR(ObsLon)

MAKE_GET(ObsLat,double)
MAKE_SET(ObsLat,double)
MAKE_TEST(ObsLat)
MAKE_CLEAR(ObsLat)

MAKE_GET(ObsAlt,double)
MAKE_SET(ObsAlt,double)
MAKE_TEST(ObsAlt)
MAKE_CLEAR(ObsAlt)

/* Clear, Get, Set and Test axis-dependent Frame attributes. */
MAKE_CLEAR_AXIS(Direction)
MAKE_CLEAR_AXIS(Format)
MAKE_CLEAR_AXIS(Label)
MAKE_CLEAR_AXIS(Symbol)
MAKE_CLEAR_AXIS(Unit)
MAKE_GET_AXIS(Direction,int)
MAKE_GET_AXIS(Format,const char *)
MAKE_GET_AXIS(Label,const char *)
MAKE_GET_AXIS(Symbol,const char *)
MAKE_GET_AXIS(Unit,const char *)
MAKE_SET_AXIS(Direction,int)
MAKE_SET_AXIS(Format,const char *)
MAKE_SET_AXIS(Label,const char *)
MAKE_SET_AXIS(Symbol,const char *)
MAKE_SET_AXIS(Unit,const char *)
MAKE_TEST_AXIS(Direction)
MAKE_TEST_AXIS(Format)
MAKE_TEST_AXIS(Label)
MAKE_TEST_AXIS(Symbol)
MAKE_TEST_AXIS(Unit)

MAKE_GET_AXIS(Bottom,double)
MAKE_SET_AXIS(Bottom,double)
MAKE_TEST_AXIS(Bottom)
MAKE_CLEAR_AXIS(Bottom)

MAKE_GET_AXIS(Top,double)
MAKE_SET_AXIS(Top,double)
MAKE_TEST_AXIS(Top)
MAKE_CLEAR_AXIS(Top)

/* Copy constructor. */
/* ----------------- */
static void Copy( const AstObject *objin, AstObject *objout, int *status ) {
/*
*  Name:
*     Copy

*  Purpose:
*     Copy constructor for FrameSet objects.

*  Type:
*     Private function.

*  Synopsis:
*     void Copy( const AstObject *objin, AstObject *objout, int *status )

*  Description:
*     This function implements the copy constructor for FrameSet objects.

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
   AstFrameSet *in;              /* Pointer to input FrameSet */
   AstFrameSet *out;             /* Pointer to output FrameSet */
   int iframe;                   /* Loop counter for Frames */
   int inode;                    /* Loop counter for nodes */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain pointers to the input and output FrameSets. */
   in = (AstFrameSet *) objin;
   out = (AstFrameSet *) objout;

/* For safety, first clear any references to the input memory from
   the output FrameSet. */
   out->frame = NULL;
   out->node = NULL;
   out->map = NULL;
   out->link = NULL;
   out->invert = NULL;

/* Allocate memory in the output FrameSet to store the Frame and node
   information and copy scalar information across. */
   out->frame = astMalloc( sizeof( AstFrame * ) * (size_t) in->nframe );
   out->node = astStore( NULL, in->node, sizeof( int ) *
                                         (size_t) in->nframe );
   out->map = astMalloc( sizeof( AstMapping * ) * (size_t) ( in->nnode - 1 ) );
   out->link = astStore( NULL, in->link, sizeof( int ) *
                                         (size_t) ( in->nnode - 1 ) );
   out->invert = astStore( NULL, in->invert, sizeof( int ) *
                                         (size_t) ( in->nnode - 1 ) );

/* If OK, make copies of each input Frame and Mapping and store the
   resulting pointers in the output FrameSet. */
   if ( astOK ) {
      for ( iframe = 0; iframe < in->nframe; iframe++ ) {
         out->frame[ iframe ] = astCopy( in->frame[ iframe ] );
      }
      for ( inode = 0; inode < in->nnode - 1; inode++ ) {
         out->map[ inode ] = astCopy( in->map[ inode ] );
      }

/* If an error occurred while copying any of these objects, clean up
   by looping through the arrays of pointers again and annulling them
   all. */
      if ( !astOK ) {
         for ( iframe = 0; iframe < in->nframe; iframe++ ) {
            out->frame[ iframe ] = astAnnul( out->frame[ iframe ] );
         }
         for ( inode = 0; inode < in->nnode - 1; inode++ ) {
            out->map[ inode ] = astAnnul( out->map[ inode ] );
         }
      }
   }

/* If an error occurred, clean up by freeing all memory allocated above. */
   if ( !astOK ) {
      out->frame = astFree( out->frame );
      out->node = astFree( out->node );
      out->map = astFree( out->map );
      out->link = astFree( out->link );
      out->invert = astFree( out->invert );
   }
}

/* Destructor. */
/* ----------- */
static void Delete( AstObject *obj, int *status ) {
/*
*  Name:
*     Delete

*  Purpose:
*     Destructor for FrameSet objects.

*  Type:
*     Private function.

*  Synopsis:
*     void Delete( AstObject *obj, int *status )

*  Description:
*     This function implements the destructor for FrameSet objects.

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
   AstFrameSet *this;               /* Pointer to FrameSet */
   int iframe;                      /* Loop counter for Frames */
   int inode;                       /* Loop counter for nodes */

/* Obtain a pointer to the FrameSet structure. */
   this = (AstFrameSet *) obj;

/* Annul all Frame pointers and clear the node numbers associated with
   them. */
   for ( iframe = 0; iframe < this->nframe; iframe++ ) {
      this->frame[ iframe ] = astAnnul( this->frame[ iframe ] );
      this->node[ iframe ] = 0;
   }

/* Annul all Mapping pointers and clear the links between pairs of
   nodes and the associated Mapping Invert information. */
   for ( inode = 0; inode < this->nnode - 1; inode++ ) {
      this->map[ inode ] = astAnnul( this->map[ inode ] );
      this->link[ inode ] = 0;
      this->invert[ inode ] = 0;
   }

/* Free all allocated memory. */
   this->frame = astFree( this->frame );
   this->node = astFree( this->node );
   this->map = astFree( this->map );
   this->link = astFree( this->link );
   this->invert = astFree( this->invert );
}

/* Dump function. */
/* -------------- */
static void Dump( AstObject *this_object, AstChannel *channel, int *status ) {
/*
*  Name:
*     Dump

*  Purpose:
*     Dump function for FrameSet objects.

*  Type:
*     Private function.

*  Synopsis:
*     void Dump( AstObject *this, AstChannel *channel, int *status )

*  Description:
*     This function implements the Dump function which writes out data
*     for the FrameSet class to an output Channel.

*  Parameters:
*     this
*        Pointer to the FrameSet whose data are being written.
*     channel
*        Pointer to the Channel to which the data are being written.
*     status
*        Pointer to the inherited status variable.
*/

/* Local Constants: */
#define COMMENT_LEN 150          /* Maximum length of a comment string */
#define KEY_LEN 50               /* Maximum length of a keyword */

/* Local Variables: */
   AstFrameSet *this;            /* Pointer to the FrameSet structure */
   char comment[ COMMENT_LEN + 1 ]; /* Buffer for comment string */
   char key[ KEY_LEN + 1 ];      /* Buffer for keyword string */
   int ifr;                      /* Loop counter for Frames */
   int inode;                    /* Loop counter for nodes */
   int invert;                   /* Invert attribute value */
   int ival;                     /* Integer value */
   int set;                      /* Attribute value set? */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain a pointer to the FrameSet structure. */
   this = (AstFrameSet *) this_object;

/* Determine if the FrameSet is inverted. This is required so that the
   effects of inversion can be un-done to obtain information about the
   "true" Base and Current Frames. (The values written are "internal"
   values that are not affected by the Invert setting.) */
   invert = astGetInvert( this );

/* Write out values representing the instance variables for the
   FrameSet class.  Accompany these with appropriate comment strings,
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

/* Nframe. */
/* ------- */
   set = ( this->nframe != 0 );
   ival = set ? this->nframe : astGetNframe( this );
   astWriteInt( channel, "Nframe", set, 1, ival,
                "Number of Frames in FrameSet" );

/* Base. */
/* ----- */
   set = ( this->base != -INT_MAX );
   ival = set ? this->base : ( !invert ? astGetBase( this ) :
                                         astGetCurrent( this ) );
   astWriteInt( channel, "Base", set, 1, ival, "Index of base Frame" );

/* Current. */
/* -------- */
   set = ( this->current != -INT_MAX );
   ival = set ? this->current : ( !invert ? astGetCurrent( this ) :
                                            astGetBase( this ) );
   astWriteInt( channel, "Currnt", set, 1, ival, "Index of current Frame" );

/* Number of nodes. */
/* ---------------- */
   ival = this->nnode;
   set = ( ival != this->nframe );
   astWriteInt( channel, "Nnode", set, 0, ival,
                "Number of nodes in FrameSet" );

/* Node index for each Frame. */
/* -------------------------- */
/* There is a node value for each Frame in the FrameSet. */
   for ( ifr = 1; ifr <= this->nframe; ifr++ ) {

/* Convert node numbering to start at 1 for the external
   representation. Regard a node number as "set" if it differs from
   the Frame number. */
      ival = this->node[ ifr - 1 ] + 1;
      set = ( ival != ifr );

/* Create a suitable keyword and comment. */
      (void) sprintf( key, "Nod%d", ifr );
      (void) sprintf( comment,
                      "Frame %d is associated with node %d", ifr, ival );

/* Write out the value. */
      astWriteInt( channel, key, set, 0, ival, comment );
   }

/* Links between nodes. */
/* -------------------- */
/* Each node in the FrameSet (except the first) has a link to another
   node from which it is derived. */
   for ( inode = 1; inode < this->nnode; inode++ ) {

/* Convert node numbering to start at 1 (as above). */
      ival = this->link[ inode - 1 ] + 1;
      (void) sprintf( key, "Lnk%d", inode + 1 );
      (void) sprintf( comment,
                      "Node %d is derived from node %d", inode + 1, ival );
      astWriteInt( channel, key, 1, 0, ival, comment );

/* Inversion flags. */
/* ---------------- */
/* Each node with a link has a value which the Invert attribute of the
   associated Mapping should have when the transformation from the
   parent node to the node in question is required. */
      ival = this->invert[ inode - 1 ];

/* Regard the value as set only if the Mapping's inverse
   transformation is required. */
      set = ( ival != 0 );
      (void) sprintf( key, "Inv%d", inode + 1 );
      astWriteInt( channel, key, set, 0, ival,
                   ival ? "The inverse mapping is used" :
                          "The forward mapping is used" );
   }

/* Frame objects. */
/* -------------- */
/* Output an Object description for each Frame in the FrameSet. */
   for ( ifr = 1; ifr <= this->nframe; ifr++ ) {
      (void) sprintf( key, "Frm%d", ifr );
      (void) sprintf( comment, "Frame number %d", ifr );
      astWriteObject( channel, key, 1, 1, this->frame[ ifr - 1 ],
                      comment );
   }

/* Mapping objects. */
/* ---------------- */
/* Output an Object description for each Mapping in the FrameSet. */
   for ( inode = 1; inode < this->nnode; inode++ ) {
      (void) sprintf( key, "Map%d", inode + 1 );
      (void) sprintf( comment, "Mapping between nodes %d and %d",
                      this->link[ inode - 1 ] + 1, inode + 1 );
      astWriteObject( channel, key, 1, 1, this->map[ inode - 1 ], comment );
   }

/* Undefine macros local to this function. */
#undef COMMENT_LEN
#undef KEY_LEN
}

/* Standard class functions. */
/* ========================= */
/* Implement the astIsAFrameSet and astCheckFrameSet functions using
   the macros defined for this purpose in the "object.h" header
   file. */
astMAKE_ISA(FrameSet,Frame)
astMAKE_CHECK(FrameSet)

AstFrameSet *astFrameSet_( void *frame_void, const char *options, int *status, ...) {
/*
*+
*  Name:
*     astFrameSet

*  Purpose:
*     Create a FrameSet.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "frameset.h"
*     AstFrameSet *astFrameSet( AstFrame *frame, const char *options, int *status, ... )

*  Class Membership:
*     FrameSet constructor.

*  Description:
*     This function creates a new FrameSet and optionally initialises
*     its attributes.

*  Parameters:
*     frame
*        Pointer to the initial Frame. If this is not a FrameSet, the
*        new FrameSet will be initialised to contain this Frame alone.
*
*        If it is a FrameSet, the new FrameSet will be initialised to
*        contain the same Frames (and Mappings) and to have the same
*        attribute values as the one supplied. This is similar to
*        making a copy, except that the Frames (and Mappings)
*        contained in the original FrameSet are not themselves copied,
*        but are shared by both FrameSets.
*     options
*        Pointer to a null terminated string containing an optional
*        comma-separated list of attribute assignments to be used for
*        initialising the new FrameSet. The syntax used is the same as
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
*     A pointer to the new FrameSet.

*  Notes:
*     - A NULL pointer will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*-

*  Implementation Notes:
*     - This function implements the basic FrameSet constructor which
*     is available via the protected interface to the FrameSet class.
*     A public interface is provided by the astFrameSetId_ function.
*     - Because this function has a variable argument list, it is
*     invoked by a macro that evaluates to a function pointer (not a
*     function invocation) and no checking or casting of arguments is
*     performed before the function is invoked. Because of this, the
*     "frame" parameter is of type (void *) and is converted and
*     validated within the function itself.
*/

/* Local Variables: */
   astDECLARE_GLOBALS            /* Pointer to thread-specific global data */
   AstFrame *frame;              /* Pointer to Frame structure */
   AstFrameSet *new;             /* Pointer to new FrameSet */
   va_list args;                 /* Variable argument list */

/* Initialise. */
   new = NULL;

/* Get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(NULL);

/* Check the global status. */
   if ( !astOK ) return new;

/* Obtain and validate a pointer to the Frame structure provided. */
   frame = astCheckFrame( frame_void );
   if ( astOK ) {

/* Initialise the FrameSet, allocating memory and initialising the
   virtual function table as well if necessary. */
      new = astInitFrameSet( NULL, sizeof( AstFrameSet ), !class_init,
                             &class_vtab, "FrameSet", frame );

/* If successful, note that the virtual function table has been
   initialised. */
      if ( astOK ) {
         class_init = 1;

/* Obtain the variable argument list and pass it along with the
   options string to the astVSet method to initialise the new
   FrameSet's attributes. */
         va_start( args, status );
         astVSet( new, options, NULL, args );
         va_end( args );

/* If an error occurred, clean up by deleting the new object. */
         if ( !astOK ) new = astDelete( new );
      }
   }

/* Return a pointer to the new FrameSet. */
   return new;
}

AstFrameSet *astInitFrameSet_( void *mem, size_t size, int init,
                               AstFrameSetVtab *vtab, const char *name,
                               AstFrame *frame, int *status ) {
/*
*+
*  Name:
*     astInitFrameSet

*  Purpose:
*     Initialise a FrameSet.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "frameset.h"
*     AstFrameSet *astInitFrameSet( void *mem, size_t size, int init,
*                                   AstFrameSetVtab *vtab, const char *name,
*                                   AstFrame *frame )

*  Class Membership:
*     FrameSet initialiser.

*  Description:
*     This function is provided for use by class implementations to
*     initialise a new FrameSet object. It allocates memory (if
*     necessary) to accommodate the FrameSet plus any additional data
*     associated with the derived class.  It then initialises a
*     FrameSet structure at the start of this memory. If the "init"
*     flag is set, it also initialises the contents of a virtual
*     function table for a FrameSet at the start of the memory passed
*     via the "vtab" parameter.

*  Parameters:
*     mem
*        A pointer to the memory in which the FrameSet is to be
*        created. This must be of sufficient size to accommodate the
*        FrameSet data (sizeof(FrameSet)) plus any data used by the
*        derived class. If a value of NULL is given, this function
*        will allocate the memory itself using the "size" parameter to
*        determine its size.
*     size
*        The amount of memory used by the FrameSet (plus derived class
*        data).  This will be used to allocate memory if a value of
*        NULL is given for the "mem" parameter. This value is also
*        stored in the FrameSet structure, so a valid value must be
*        supplied even if not required for allocating memory.
*     init
*        A logical flag indicating if the FrameSet's virtual function
*        table is to be initialised. If this value is non-zero, the
*        virtual function table will be initialised by this function.
*     vtab
*        Pointer to the start of the virtual function table to be
*        associated with the new FrameSet.
*     name
*        Pointer to a constant null-terminated character string which
*        contains the name of the class to which the new object
*        belongs (it is this pointer value that will subsequently be
*        returned by the astGetClass method).
*     frame
*        Pointer to the initial Frame (or FrameSet).

*  Returned Value:
*     A pointer to the new FrameSet.

*  Notes:
*     - A null pointer will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*-
*/

/* Local Variables: */
   AstFrameSet *new;                /* Pointer to new FrameSet */
   AstFrameSet *old;                /* Pointer to original FrameSet */
   int iframe;                      /* Loop counter for Frames */
   int inode;                       /* Loop counter for nodes */

/* Check the global status. */
   if ( !astOK ) return NULL;

/* If necessary, initialise the virtual function table. */
   if ( init ) astInitFrameSetVtab( vtab, name );

/* Initialise a Frame structure (the parent class) as the first
   component within the FrameSet structure, allocating memory if
   necessary. Give this Frame zero axes, as all axis information for
   the FrameSet will be derived from the Frames it contains. */
   new = (AstFrameSet *) astInitFrame( mem, size, 0,
                                       (AstFrameVtab *) vtab, name, 0 );

   if ( astOK ) {

/* Initialise the FrameSet data. */
/* ----------------------------- */

/* Normal Frame supplied. */
/* ---------------------- */
/* Check that the Frame supplied is not a FrameSet (initialising using
   a FrameSet is a special case which is handled below).  If not, we
   initialise the new FrameSet to refer to a single Frame. */
      if ( !astIsAFrameSet( frame ) && astOK ) {

/* Allocate memory for the arrays of Frame information. */
         new->frame = astMalloc( sizeof( AstFrame * ) );
         new->node = astMalloc( sizeof( int ) );

/* The node arrays are not required until at least two Frames are
   present. */
         new->map = NULL;
         new->link = NULL;
         new->invert = NULL;

/* If OK, initialise these arrays, thus adding the Frame to the
   FrameSet. */
         if ( astOK ) {
            new->frame[ 0 ] = astClone( frame );
            new->node[ 0 ] = 0;
            new->nframe = 1;
            new->nnode = 1;

/* Initialise the FrameSet attributes to their undefined states. */
            new->base = -INT_MAX;
            new->current = -INT_MAX;
         }

/* FrameSet supplied. */
/* ------------------ */
/* If a FrameSet was supplied, we will initialise the new FrameSet to
   refer to the same Frame and Mapping information (this is similar to
   making a copy, except that we clone all the pointers, instead of
   copying the Objects they refer to). */
      } else if ( astOK ) {

/* Obtain a pointer to the original FrameSet structure. */
         old = (AstFrameSet *) frame;

/* Allocate memory in the new FrameSet to store the Frame and node
   information and copy any scalar information across. */
         new->frame = astMalloc( sizeof( AstFrame * ) * (size_t) old->nframe );
         new->node = astStore( NULL, old->node,
                               sizeof( int ) * (size_t) old->nframe );
         new->map = astMalloc( sizeof( AstMapping * ) *
                               (size_t) ( old->nnode - 1 ) );
         new->link = astStore( NULL, old->link,
                               sizeof( int ) * (size_t) ( old->nnode - 1 ) );
         new->invert = astStore( NULL, old->invert,
                                 sizeof( int ) * (size_t) ( old->nnode - 1 ) );

/* If OK, clone the pointer to each Frame and Mapping referenced by
   the original FrameSet and store the resulting pointers in the new
   FrameSet. */
         if ( astOK ) {
            for ( iframe = 0; iframe < old->nframe; iframe++ ) {
               new->frame[ iframe ] = astClone( old->frame[ iframe ] );
            }
            for ( inode = 0; inode < old->nnode - 1; inode++ ) {
               new->map[ inode ] = astClone( old->map[ inode ] );
            }

/* If an error occurred while cloning any of these pointers, clean up
   by looping through the arrays of cloned pointers again and
   annulling them all. */
            if ( !astOK ) {
               for ( iframe = 0; iframe < old->nframe; iframe++ ) {
                  new->frame[ iframe ] = astAnnul( new->frame[ iframe ] );
               }
               for ( inode = 0; inode < old->nnode - 1; inode++ ) {
                  new->map[ inode ] = astAnnul( new->map[ inode ] );
               }
            }
         }

/* If an error occurred, clean up by freeing all memory allocated
   above. */
         if ( !astOK ) {
            new->frame = astFree( new->frame );
            new->node = astFree( new->node );
            new->map = astFree( new->map );
            new->link = astFree( new->link );
            new->invert = astFree( new->invert );
         }

/* Copy the Frame and node counts across. */
         new->nframe = old->nframe;
         new->nnode = old->nnode;

/* Copy the other FrameSet attributes across. */
         new->base = old->base;
         new->current = old->current;

/* Transfer any other inherited attribute values that relate to the
   FrameSet itself (rather than the enclosed Frames). */
        if ( astTestInvert( old ) ) astSetInvert( new, astGetInvert( old ) );
      }

/* If an error occurred, clean up by deleting the new object. */
      if ( !astOK ) new = astDelete( new );
   }

/* Return a pointer to the new object. */
   return new;

/* Undefine macros local to this function. */
#undef TRANSFER
}

AstFrameSet *astLoadFrameSet_( void *mem, size_t size,
                               AstFrameSetVtab *vtab, const char *name,
                               AstChannel *channel, int *status ) {
/*
*+
*  Name:
*     astLoadFrameSet

*  Purpose:
*     Load a FrameSet.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "frameset.h"
*     AstFrameSet *astLoadFrameSet( void *mem, size_t size,
*                                   AstFrameSetVtab *vtab, const char *name,
*                                   AstChannel *channel )

*  Class Membership:
*     FrameSet loader.

*  Description:
*     This function is provided to load a new FrameSet using data read
*     from a Channel. It first loads the data used by the parent class
*     (which allocates memory if necessary) and then initialises a
*     FrameSet structure in this memory, using data read from the
*     input Channel.

*  Parameters:
*     mem
*        A pointer to the memory into which the FrameSet is to be
*        loaded.  This must be of sufficient size to accommodate the
*        FrameSet data (sizeof(FrameSet)) plus any data used by
*        derived classes. If a value of NULL is given, this function
*        will allocate the memory itself using the "size" parameter to
*        determine its size.
*     size
*        The amount of memory used by the FrameSet (plus derived class
*        data).  This will be used to allocate memory if a value of
*        NULL is given for the "mem" parameter. This value is also
*        stored in the FrameSet structure, so a valid value must be
*        supplied even if not required for allocating memory.
*
*        If the "vtab" parameter is NULL, the "size" value is ignored
*        and sizeof(AstFrameSet) is used instead.
*     vtab
*        Pointer to the start of the virtual function table to be
*        associated with the new FrameSet. If this is NULL, a pointer
*        to the (static) virtual function table for the FrameSet class
*        is used instead.
*     name
*        Pointer to a constant null-terminated character string which
*        contains the name of the class to which the new object
*        belongs (it is this pointer value that will subsequently be
*        returned by the astGetClass method).
*
*        If the "vtab" parameter is NULL, the "name" value is ignored
*        and a pointer to the string "FrameSet" is used instead.

*  Returned Value:
*     A pointer to the new FrameSet.

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
   AstFrameSet *new;             /* Pointer to the new FrameSet */
   char key[ KEY_LEN + 1 ];      /* Buffer for keyword string */
   int ifr;                      /* Get a pointer to the thread specific global data structure. */

/* Loop counter for Frames */
   int inode;                    /* Loop counter for nodes */

   astGET_GLOBALS(channel);

/* Initialise. */
   new = NULL;

/* Check the global error status. */
   if ( !astOK ) return new;

/* If a NULL virtual function table has been supplied, then this is
   the first loader to be invoked for this FrameSet. In this case the
   FrameSet belongs to this class, so supply appropriate values to be
   passed to the parent class loader (and its parent, etc.). */
   if ( !vtab ) {
      size = sizeof( AstFrameSet );
      vtab = &class_vtab;
      name = "FrameSet";

/* If required, initialise the virtual function table for this class. */
      if ( !class_init ) {
         astInitFrameSetVtab( vtab, name );
         class_init = 1;
      }
   }

/* Invoke the parent class loader to load data for all the ancestral
   classes of the current one, returning a pointer to the resulting
   partly-built FrameSet. */
   new = astLoadFrame( mem, size, (AstFrameVtab *) vtab, name,
                       channel );

   if ( astOK ) {

/* Read input data. */
/* ================ */
/* Request the input Channel to read all the input data appropriate to
   this class into the internal "values list". */
      astReadClassData( channel, "FrameSet" );

/* Now read each individual data item from this list and use it to
   initialise the appropriate instance variable(s) for this class. */

/* In the case of attributes, we first read the "raw" input value,
   supplying the "unset" value as the default. If a "set" value is
   obtained, we then use the appropriate (private) Set... member
   function to validate and set the value properly. */

/* Nframe. */
/* ------- */
   new->nframe = astReadInt( channel, "nframe", 1 );
   if ( new->nframe < 0 ) new->nframe = 1;

/* Number of nodes. */
/* ---------------- */
   new->nnode = astReadInt( channel, "nnode", new->nframe );
   if ( new->nnode < 1 ) new->nnode = 1;

/* Allocate memory to hold Frame and node information. */
   new->frame = astMalloc( sizeof( AstFrame *) * (size_t) new->nframe );
   new->node = astMalloc( sizeof( int ) * (size_t) new->nframe );
   new->link = astMalloc( sizeof( int ) * (size_t) ( new->nnode - 1 ) );
   new->invert = astMalloc( sizeof( int ) * (size_t) ( new->nnode - 1 ) );
   new->map = astMalloc( sizeof( AstMapping * ) *
                         (size_t) ( new->nnode - 1 ) );

/* If an error occurs, ensure that all allocated memory is freed. */
   if ( !astOK ) {
      new->frame = astFree( new->frame );
      new->node = astFree( new->node );
      new->link = astFree( new->link );
      new->invert = astFree( new->invert );
      new->map = astFree( new->map );

/* Otherwise, initialise the arrays which will hold Object pointers. */
   } else {
      for ( ifr = 1; ifr <= new->nframe; ifr++ ) {
         new->frame[ ifr - 1 ] = NULL;
      }
      for ( inode = 1; inode < new->nnode; inode++ ) {
         new->map[ inode - 1 ] = NULL;
      }

/* Read Frame data... */
      for ( ifr = 1; ifr <= new->nframe; ifr++ ) {

/* Frame objects. */
/* -------------- */
/* Create the required keyword and then read the Frame. */
         (void) sprintf( key, "frm%d", ifr );
         new->frame[ ifr - 1 ] = astReadObject( channel, key, NULL );

/* Node index for each Frame. */
/* -------------------------- */
         (void) sprintf( key, "nod%d", ifr );
         new->node[ ifr - 1 ] = astReadInt( channel, key, ifr ) - 1;
      }

/* Read node data... */
      for ( inode = 1; inode < new->nnode; inode++ ) {

/* Links between nodes. */
/* -------------------- */
         (void) sprintf( key, "lnk%d", inode + 1 );
         new->link[ inode - 1 ] = astReadInt( channel, key, 0 ) - 1;

/* Inversion flags. */
/* ---------------- */
         (void) sprintf( key, "inv%d", inode + 1 );
         new->invert[ inode - 1 ] = astReadInt( channel, key, 0 );

/* Mapping objects. */
/* ---------------- */
         (void) sprintf( key, "map%d", inode + 1 );
         new->map[ inode - 1 ] = astReadObject( channel, key, NULL );
      }

/* Read remaining data... */

/* Base. */
/* ----- */
      new->base = astReadInt( channel, "base", -INT_MAX );
      if ( new->base < 1 ) new->base = -INT_MAX;

/* Current. */
/* -------- */
      new->current = astReadInt( channel, "currnt", -INT_MAX );
      if ( new->base < 1 ) new->base = -INT_MAX;
   }

/* If an error occurred, clean up by deleting the new FrameSet. */
      if ( !astOK ) new = astDelete( new );
   }

/* Return the new FrameSet pointer. */
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
void astAddFrame_( AstFrameSet *this, int iframe, AstMapping *map,
                   AstFrame *frame, int *status ) {
   if ( !astOK ) return;
   (**astMEMBER(this,FrameSet,AddFrame))( this, iframe, map, frame, status );
}
void astClearBase_( AstFrameSet *this, int *status ) {
   if ( !astOK ) return;
   (**astMEMBER(this,FrameSet,ClearBase))( this, status );
}
void astClearCurrent_( AstFrameSet *this, int *status ) {
   if ( !astOK ) return;
   (**astMEMBER(this,FrameSet,ClearCurrent))( this, status );
}
int astGetBase_( AstFrameSet *this, int *status ) {
   if ( !astOK ) return 0;
   return (**astMEMBER(this,FrameSet,GetBase))( this, status );
}
int astGetCurrent_( AstFrameSet *this, int *status ) {
   if ( !astOK ) return 0;
   return (**astMEMBER(this,FrameSet,GetCurrent))( this, status );
}
AstFrame *astGetFrame_( AstFrameSet *this, int iframe, int *status ) {
   if ( !astOK ) return NULL;
   return (**astMEMBER(this,FrameSet,GetFrame))( this, iframe, status );
}
AstMapping *astGetMapping_( AstFrameSet *this, int iframe1, int iframe2, int *status ) {
   if ( !astOK ) return NULL;
   return (**astMEMBER(this,FrameSet,GetMapping))( this, iframe1, iframe2, status );
}
int astGetNframe_( AstFrameSet *this, int *status ) {
   if ( !astOK ) return 0;
   return (**astMEMBER(this,FrameSet,GetNframe))( this, status );
}
void astRemapFrame_( AstFrameSet *this, int iframe, AstMapping *map, int *status ) {
   if ( !astOK ) return;
   (**astMEMBER(this,FrameSet,RemapFrame))( this, iframe, map, status );
}
void astRemoveFrame_( AstFrameSet *this, int iframe, int *status ) {
   if ( !astOK ) return;
   (**astMEMBER(this,FrameSet,RemoveFrame))( this, iframe, status );
}
void astSetBase_( AstFrameSet *this, int ibase, int *status ) {
   if ( !astOK ) return;
   (**astMEMBER(this,FrameSet,SetBase))( this, ibase, status );
}
void astSetCurrent_( AstFrameSet *this, int icurrent, int *status ) {
   if ( !astOK ) return;
   (**astMEMBER(this,FrameSet,SetCurrent))( this, icurrent, status );
}
int astTestBase_( AstFrameSet *this, int *status ) {
   if ( !astOK ) return 0;
   return (**astMEMBER(this,FrameSet,TestBase))( this, status );
}
int astTestCurrent_( AstFrameSet *this, int *status ) {
   if ( !astOK ) return 0;
   return (**astMEMBER(this,FrameSet,TestCurrent))( this, status );
}
int astValidateFrameIndex_( AstFrameSet *this, int iframe,
                            const char *method, int *status ) {
   if ( !astOK ) return 0;
   return (**astMEMBER(this,FrameSet,ValidateFrameIndex))( this, iframe,
                                                           method, status );
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
AstFrameSet *astFrameSetId_( void *, const char *, ... );

/* Special interface function implementations. */
/* ------------------------------------------- */
AstFrameSet *astFrameSetId_( void *frame_void, const char *options, ... ) {
/*
*++
*  Name:
c     astFrameSet
f     AST_FRAMESET

*  Purpose:
*     Create a FrameSet.

*  Type:
*     Public function.

*  Synopsis:
c     #include "frameset.h"
c     AstFrameSet *astFrameSet( AstFrame *frame, const char *options, ... )
f     RESULT = AST_FRAMESET( FRAME, OPTIONS, STATUS )

*  Class Membership:
*     FrameSet constructor.

*  Description:
*     This function creates a new FrameSet and optionally initialises
*     its attributes.
*
*     A FrameSet consists of a set of one or more Frames (which
*     describe coordinate systems), connected together by Mappings
*     (which describe how the coordinate systems are inter-related). A
*     FrameSet makes it possible to obtain a Mapping between any pair
*     of these Frames (i.e. to convert between any of the coordinate
*     systems which it describes).  The individual Frames are
*     identified within the FrameSet by an integer index, with Frames
*     being numbered consecutively from one as they are added to the
*     FrameSet.
*
*     Every FrameSet has a "base" Frame and a "current" Frame (which
*     are allowed to be the same). Any of the Frames may be nominated
*     to hold these positions, and the choice is determined by the
*     values of the FrameSet's Base and Current attributes, which hold
*     the indices of the relevant Frames.  By default, the first Frame
*     added to a FrameSet is its base Frame, and the last one added is
*     its current Frame.
*
*     The base Frame describes the "native" coordinate system of
*     whatever the FrameSet is used to calibrate (e.g. the pixel
*     coordinates of an image) and the current Frame describes the
*     "apparent" coordinate system in which it should be viewed
*     (e.g. displayed, etc.). Any further Frames represent a library
*     of alternative coordinate systems, which may be selected by
*     making them current.
*
*     When a FrameSet is used in a context that requires a Frame,
*     (e.g. obtaining its Title value, or number of axes), the current
*     Frame is used. A FrameSet may therefore be used in place of its
*     current Frame in most situations.
*
*     When a FrameSet is used in a context that requires a Mapping,
*     the Mapping used is the one between its base Frame and its
*     current Frame. Thus, a FrameSet may be used to convert "native"
*     coordinates into "apparent" ones, and vice versa. Like any
c     Mapping, a FrameSet may also be inverted (see astInvert), which
f     Mapping, a FrameSet may also be inverted (see AST_INVERT), which
*     has the effect of interchanging its base and current Frames and
*     hence of reversing the Mapping between them.
*
*     Regions may be added into a FrameSet (since a Region is a type of
*     Frame), either explicitly or as components within CmpFrames. In this
*     case the Mapping between a pair of Frames within a FrameSet will
*     include the effects of the clipping produced by any Regions included
*     in the path between the Frames.

*  Parameters:
c     frame
f     FRAME = INTEGER (Given)
*        Pointer to the first Frame to be inserted into the
*        FrameSet. This initially becomes both the base and the
*        current Frame. (Further Frames may be added using the
c        astAddFrame function.)
f        AST_ADDFRAME routine.)
c     options
f     OPTIONS = CHARACTER * ( * ) (Given)
c        Pointer to a null-terminated string containing an optional
c        comma-separated list of attribute assignments to be used for
c        initialising the new FrameSet. The syntax used is identical to
c        that for the astSet function and may include "printf" format
c        specifiers identified by "%" symbols in the normal way.
c        If no initialisation is required, a zero-length string may be
c        supplied.
f        A character string containing an optional comma-separated
f        list of attribute assignments to be used for initialising the
f        new FrameSet. The syntax used is identical to that for the
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
c     astFrameSet()
f     AST_FRAMESET
*        A pointer to the new FrameSet.

*  Notes:
c     - If a pointer to an existing FrameSet is given for the "frame"
c     parameter, then the new FrameSet will (as a special case) be
f     - If a pointer to an existing FrameSet is given for the FRAME
f     argument, then the new FrameSet will (as a special case) be
*     initialised to contain the same Frames and Mappings, and to have
*     the same attribute values, as the one supplied. This process is
c     similar to making a copy of a FrameSet (see astCopy), except
f     similar to making a copy of a FrameSet (see AST_COPY), except
*     that the Frames and Mappings contained in the original are not
*     themselves copied, but are shared by both FrameSets.
*     - A null Object pointer (AST__NULL) will be returned if this
c     function is invoked with the AST error status set, or if it
f     function is invoked with STATUS set to an error value, or if it
*     should fail for any reason.
*--

*  Implementation Notes:
*     - This function implements the external (public) interface to
*     the astFrameSet constructor function. It returns an ID value
*     (instead of a true C pointer) to external users, and must be
*     provided because astFrameSet_ has a variable argument list which
*     cannot be encapsulated in a macro (where this conversion would
*     otherwise occur).
*     - Because no checking or casting of arguments is performed
*     before the function is invoked, the "frame" parameter is of type
*     (void *) and is converted from an ID value to a pointer and
*     validated within the function itself.
*     - The variable argument list also prevents this function from
*     invoking astFrameSet_ directly, so it must be a
*     re-implementation of it in all respects, except for the
*     conversions between IDs and pointers on input/output of Objects.
*/

/* Local Variables: */
   astDECLARE_GLOBALS            /* Pointer to thread-specific global data */
   AstFrame *frame;              /* Pointer to Frame structure */
   AstFrameSet *new;             /* Pointer to new FrameSet */
   va_list args;                 /* Variable argument list */

   int *status;

/* Initialise. */
   new = NULL;

/* Get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(NULL);

/* Pointer to inherited status value */

/* Get a pointer to the inherited status value. */
   status = astGetStatusPtr;

/* Check the global status. */
   if ( !astOK ) return new;

/* Obtain the Frame pointer from the ID supplied and validate the
   pointer to ensure it identifies a valid Frame. */
   frame = astVerifyFrame( astMakePointer( frame_void ) );
   if ( astOK ) {

/* Initialise the FrameSet, allocating memory and initialising the
   virtual function table as well if necessary. */
      new = astInitFrameSet( NULL, sizeof( AstFrameSet ), !class_init,
                             &class_vtab, "FrameSet", frame );

/* If successful, note that the virtual function table has been
   initialised. */
      if ( astOK ) {
         class_init = 1;

/* Obtain the variable argument list and pass it along with the
   options string to the astVSet method to initialise the new
   FrameSet's attributes. */
         va_start( args, options );
         astVSet( new, options, NULL, args );
         va_end( args );

/* If an error occurred, clean up by deleting the new object. */
         if ( !astOK ) new = astDelete( new );
      }
   }

/* Return an ID value for the new FrameSet. */
   return astMakeId( new );
}



