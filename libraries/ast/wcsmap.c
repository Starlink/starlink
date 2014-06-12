/*
*class++
*  Name:
*     WcsMap

*  Purpose:
*     Implement a FITS-WCS sky projection.

*  Constructor Function:
c     astWcsMap
f     AST_WCSMAP

*  Description:
*     This class is used to represent sky coordinate projections as
*     described in the FITS world coordinate system (FITS-WCS) paper II
*     "Representations of Celestial Coordinates in FITS" by M. Calabretta
*     and E.W. Griesen. This paper defines a set of functions, or sky
*     projections, which transform longitude-latitude pairs representing
*     spherical celestial coordinates into corresponding pairs of Cartesian
*     coordinates (and vice versa).
*
*     A WcsMap is a specialised form of Mapping which implements these
*     sky projections and applies them to a specified pair of coordinates.
*     All the projections in the FITS-WCS paper are supported, plus the now
*     deprecated "TAN with polynomial correction terms" projection which
*     is refered to here by the code "TPN". Using the FITS-WCS terminology,
*     the transformation is between "native spherical" and "projection
*     plane" coordinates (also called "intermediate world coordinates".
*     These coordinates may, optionally, be embedded in a space with more
*     than two dimensions, the remaining coordinates being copied unchanged.
*     Note, however, that for consistency with other AST facilities, a
*     WcsMap handles coordinates that represent angles in radians (rather
*     than the degrees used by FITS-WCS).
*
*     The type of FITS-WCS projection to be used and the coordinates
*     (axes) to which it applies are specified when a WcsMap is first
*     created. The projection type may subsequently be determined
*     using the WcsType attribute and the coordinates on which it acts
*     may be determined using the WcsAxis(lonlat) attribute.
*
*     Each WcsMap also allows up to 100 "projection parameters" to be
*     associated with each axis. These specify the precise form of the
*     projection, and are accessed using PVi_m attribute, where "i" is
*     the integer axis index (starting at 1), and m is an integer
*     "parameter index" in the range 0 to 99. The number of projection
*     parameters required by each projection, and their meanings, are
*     dependent upon the projection type (most projections either do not
*     use any projection parameters, or use parameters 1 and 2 associated
*     with the latitude axis). Before creating a WcsMap you should consult
*     the FITS-WCS paper for details of which projection parameters are
*     required, and which have defaults. When creating the WcsMap, you must
*     explicitly set values for all those required projection parameters
*     which do not have defaults defined in this paper.

*  Inheritance:
*     The WcsMap class inherits from the Mapping class.

*  Attributes:
*     In addition to those attributes common to all Mappings, every
*     WcsMap also has the following attributes:
*
*     - NatLat: Native latitude of the reference point of a FITS-WCS projection
*     - NatLon: Native longitude of the reference point of a FITS-WCS projection
*     - PVi_m: FITS-WCS projection parameters
*     - PVMax: Maximum number of FITS-WCS projection parameters
*     - WcsAxis(lonlat): FITS-WCS projection axes
*     - WcsType: FITS-WCS projection type

*  Functions:
c     The WcsMap class does not define any new functions beyond those
f     The WcsMap class does not define any new routines beyond those
*     which are applicable to all Mappings.

*  Copyright:
*     Copyright (C) 1997-2006 Council for the Central Laboratory of the
*     Research Councils

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
*     DSB: D.S. Berry (Starlink)
*     RFWS: R.F. Warren-Smith (Starlink)

*  History:
*     15-FEB-1996 (DSB):
*        Original version.
*     23-MAR-1996 (DSB):
*        Added support for PointSets with more than 2 axes.
*     14-NOV-1996 (DSB):
*        Added I/O facilities, external interface, attributes, etc.
*     16-JAN-1997 (DSB):
*        Allowed WCSBAD as a projection type in astWcsMap.
*     24-APR-1997 (RFWS):
*        Tidied prologues.
*     26-SEP-1997 (DSB):
*        o  Long descriptions of projections changed to include a textual
*        description as well as the three letter acronym.
*        o  Added protected function astPrjDesc.
*        o  String values now used instead of integers to represent
*        "choice" attributes externally (eg WcsType).
*     8-APR-1998 (DSB):
*        Modified MapMerge so that a WcsMap can merge with its own
*        inverse when astSimplify is used.
*     20-APR-1998 (DSB):
*        Modified MapMerge to avoid the possibility of returning an
*        empty mappings list.
*     4-SEP-1998 (DSB):
*        Changed MapMerge to allow WcsMaps to swap with PermMaps in
*        order to bring mergable WcsMaps closer together.
*     5-MAY-1999 (DSB):
*        More corrections to MapMerge: Cleared up errors in the use of the
*        supplied invert flags, and corrected logic for deciding which
*        neighbouring Mapping to swap with.
*     12-JUL-1999 (DSB):
*        -  Report an error if too many or two few projection parameters are
*        supplied in a WcsMap.
*        -  Corrected MapMerge to prevent unset projection parameters
*        being copied to any new WcsMaps.
*        -  Correct handling of invert flags in WcsPerm.
*     16-JUL-1999 (DSB):
*        Fixed memory leak in MapMerge.
*     11-FEB-2000 (DSB):
*        Added PVj_m attributes. Attributes ProjP(0) to ProjP(9) are now
*        aliases for PV(axlat)_0 to PV(axlat)_9. Renamed GLS projection
*        as SFL (GLS retained as alias for SFL).
*     10-AUG-2000 (DSB):
*        MapMerge no longer simplifies a CAR projection. Previously they
*        were replaced by a UnitMap, but this removed the cylic nature of
*        the mapping (i.e. 2.PI == 0 ).
*     6-OCT-2000 (DSB):
*        Ignore leading and trailing spaces in astWCsPrjType (some
*        CTYPE FITS keywords have appeared with trailing white space).
*     26-SEP-2001 (DSB):
*        Changed names of all functions and structure to avoid name clashes
*        with wcslib.
*     10-OCT-2002 (DSB):
*        Added astIsZenithal.
*     22-OCT-2002 (DSB):
*        - GetPV now returns the FITS default value instead of AST__BAD
*        if a defaultable latitude projection parameter has not been set.
*        - A number of changes needed to support WcsLib v2.9.
*        - Added AST__TPN projection.
*     8-JAN-2003 (DSB):
*        Changed private InitVtab method to protected astInitWcsMapVtab
*        method.
*     4-JUN-2003 (DSB):
*        - Added attribute "NatLon".
*        - Changed to allow a user-specified fiducial point to be stored
*        in projection parameter PVi_1 and PVi_2 for the longitude axis.
*        - Changed "PVj_m" to "PVi_m" for consistency with FITS-WCS paper II.
*     18-AUG-2003 (DSB):
*        In function Map, assign zero longitude to output positions which
*        are very close to a pole.
*     23-SEP-2003 (DSB):
*        - Changed so that the NatLat and NatLon attributes refer to the
*        fixed values for the projections defined in FITS-WCS paper II, rather
*        than the user-defined values stored in projection parameter PVi_1 and
*        PVi_2 for the longitude axis.
*     11-FEB-2004 (DSB):
*        Corrected axis numbering when reporting missing keywords in
*        Transform.
*     23-APR-2004 (DSB):
*        Changes to simplification algorithm.
*     1-SEP-2004 (DSB):
*        CopyPV rewritten to avoid assumption that the input and output
*        WcsMaps have the same number of axes and that the lon/lat axes have
*        the same indices.
*     7-DEC-2005 (DSB):
*        Free memory allocated by calls to astReadString.
*     14-FEB-2006 (DSB):
*        Override astGetObjSize.
*     15-FEB-2006 (DSB):
*        Use dynamic rather than static memory for the parameter arrays in
*        the AstPrjPrm structure.Override astGetObjSize. This is to
*        reduce the in-memory size of a WcsMap.
*     10-MAY-2006 (DSB):
*        Override astEqual.
*     10-AUG-2006 (DSB):
*        Correct astLoadWcsMap to take acount of the different number of
*        PVi_m values that can be associated with each axis.
*     4-JAN-2007 (DSB):
*        Correct astLoadWcsMap to load the projection parameter with
*        highest index correctly.
*     23-FEB-2007 (DSB):
*        Added HPX projection.
*     1-MAR-2011 (DSB):
*        In function Map, do not allow valid longitude range to include both
*        the high limit and the low limt (since they are both the same point on
*        the sky).
*     7-MAR-2011 (DSB):
*        In function Map, only do the longitude check if the projection
*        is not cyclic.
*     24-MAY-2011 (DSB):
*        Added protected FITSProj and TPNTan attributes (they should be
*        removed when the PolyMap class has an iterative inverse).
*     6-MAR-2014 (DSB):
*        Revert the change made on 18-AUG-2003 since setting the
*        longitude arbitrarily to zero for points close to the pole
*        causes significant round trip errors when doing pixel->sky->pixel
*        transformation for points very close to the pole, if the pixel
*        size is very small. The longitude at the pole is indeterminate,
*        but whatever random numerical value is returned by atan2 is
*        no less useful (and no more useful) than a fixed value of zero.
*     12-JUN-2014 (DSB):
*        Added XPH projection.
*class--
*/

/* Module Macros. */
/* ============== */
/* Set the name of the class we are implementing. This indicates to
   the header files that define class interfaces that they should make
   "protected" symbols available. */
#define astCLASS WcsMap

/* Macros which return the maximum and minimum of two values. */
#define MAX(aa,bb) ((aa)>(bb)?(aa):(bb))
#define MIN(aa,bb) ((aa)<(bb)?(aa):(bb))

/* Macros to check for equality of floating point values. We cannot
   compare bad values directory because of the danger of floating point
   exceptions, so bad values are dealt with explicitly. */
#define EQUAL(aa,bb) (((aa)==AST__BAD)?(((bb)==AST__BAD)?1:0):(((bb)==AST__BAD)?0:(fabs((aa)-(bb))<=1.0E5*MAX((fabs(aa)+fabs(bb))*DBL_EPSILON,DBL_MIN))))

/*
*
*  Name:
*     MAKE_CLEAR

*  Purpose:
*     Implement a method to clear a single value in a multi-valued attribute.

*  Type:
*     Private macro.

*  Synopsis:
*     #include "wcsmap.h"
*     MAKE_CLEAR(attr,component,assign,nval)

*  Class Membership:
*     Defined by the WcsMap class.

*  Description:
*     This macro expands to an implementation of a private member function of
*     the form:
*
*        static void Clear<Attribute>( AstWcsMap *this, int axis )
*
*     and an external interface function of the form:
*
*        void astClear<Attribute>_( AstWcsMap *this, int axis )
*
*     which implement a method for clearing a single value in a specified
*     multi-valued attribute for an axis of a WcsMap.

*  Parameters:
*     attr
*        The name of the attribute to be cleared, as it appears in the function
*        name (e.g. Label in "astClearLabelAt").
*     component
*        The name of the class structure component that holds the attribute
*        value.
*     assign
*        An expression that evaluates to the value to assign to the component
*        to clear its value. The variable "axis" can be used to refer to
*        the zero-based index of the attribute component being cleared.
*     nval
*        Specifies the number of values in the multi-valued attribute. The
*        "axis" values supplied to the created function should be in the
*        range zero to (nval - 1).

*  Notes:
*     -  To avoid problems with some compilers, you should not leave any white
*     space around the macro arguments.
*
*/

/* Define the macro. */
#define MAKE_CLEAR(attr,component,assign,nval) \
\
/* Private member function. */ \
/* ------------------------ */ \
static void Clear##attr( AstWcsMap *this, int axis, int *status ) { \
\
/* Check the global error status. */ \
   if ( !astOK ) return; \
\
/* Validate the axis index. */ \
   if( axis < 0 || axis >= nval ){ \
      astError( AST__AXIIN, "%s(%s): Index (%d) is invalid for attribute " \
                #attr " - it should be in the range 1 to %d.", status, \
                "astClear" #attr, astGetClass( this ), \
                axis + 1, nval ); \
\
/* Assign the "clear" value. */ \
   } else { \
      this->component[ axis ] = (assign); \
   } \
} \
\
/* External interface. */ \
/* ------------------- */ \
void astClear##attr##_( AstWcsMap *this, int axis, int *status ) { \
\
/* Check the global error status. */ \
   if ( !astOK ) return; \
\
/* Invoke the required method via the virtual function table. */ \
   (**astMEMBER(this,WcsMap,Clear##attr))( this, axis, status ); \
}


/*
*
*  Name:
*     MAKE_GET

*  Purpose:
*     Implement a method to get a single value in a multi-valued attribute.

*  Type:
*     Private macro.

*  Synopsis:
*     #include "wcsmap.h"
*     MAKE_GET(attr,type,bad_value,assign,nval)

*  Class Membership:
*     Defined by the WcsMap class.

*  Description:
*     This macro expands to an implementation of a private member function of
*     the form:
*
*        static <Type> Get<Attribute>( AstWcsMap *this, int axis )
*
*     and an external interface function of the form:
*
*        <Type> astGet<Attribute>_( AstWcsMap *this, int axis )
*
*     which implement a method for getting a single value from a specified
*     multi-valued attribute for an axis of a WcsMap.

*  Parameters:
*     attr
*        The name of the attribute whose value is to be obtained, as it
*        appears in the function name (e.g. Label in "astGetLabel").
*     type
*        The C type of the attribute.
*     bad_value
*        A constant value to return if the global error status is set, or if
*        the function fails.
*     assign
*        An expression that evaluates to the value to be returned. This can
*        use the string "axis" to represent the zero-based value index.
*     nval
*        Specifies the number of values in the multi-valued attribute. The
*        "axis" values supplied to the created function should be in the
*        range zero to (nval - 1).

*  Notes:
*     -  To avoid problems with some compilers, you should not leave any white
*     space around the macro arguments.
*
*/

/* Define the macro. */
#define MAKE_GET(attr,type,bad_value,assign,nval) \
\
/* Private member function. */ \
/* ------------------------ */ \
static type Get##attr( AstWcsMap *this, int axis, int *status ) { \
   type result;                  /* Result to be returned */ \
\
/* Initialise */ \
   result = (bad_value); \
\
/* Check the global error status. */ \
   if ( !astOK ) return result; \
\
/* Validate the axis index. */ \
   if( axis < 0 || axis >= nval ){ \
      astError( AST__AXIIN, "%s(%s): Index (%d) is invalid for attribute " \
                #attr " - it should be in the range 1 to %d.", status, \
                "astGet" #attr, astGetClass( this ), \
                axis + 1, nval ); \
\
/* Assign the result value. */ \
   } else { \
      result = (assign); \
   } \
\
/* Check for errors and clear the result if necessary. */ \
   if ( !astOK ) result = (bad_value); \
\
/* Return the result. */ \
   return result; \
} \
/* External interface. */ \
/* ------------------- */  \
type astGet##attr##_( AstWcsMap *this, int axis, int *status ) { \
\
/* Check the global error status. */ \
   if ( !astOK ) return (bad_value); \
\
/* Invoke the required method via the virtual function table. */ \
   return (**astMEMBER(this,WcsMap,Get##attr))( this, axis, status ); \
}

/*
*
*  Name:
*     MAKE_SET

*  Purpose:
*     Implement a method to set a single value in a multi-valued attribute
*     for a WcsMap.

*  Type:
*     Private macro.

*  Synopsis:
*     #include "wcsmap.h"
*     MAKE_SET(attr,type,component,assign,nval)

*  Class Membership:
*     Defined by the WcsMap class.

*  Description:
*     This macro expands to an implementation of a private member function of
*     the form:
*
*        static void Set<Attribute>( AstWcsMap *this, int axis, <Type> value )
*
*     and an external interface function of the form:
*
*        void astSet<Attribute>_( AstWcsMap *this, int axis, <Type> value )
*
*     which implement a method for setting a single value in a specified
*     multi-valued attribute for a WcsMap.

*  Parameters:
*      attr
*         The name of the attribute to be set, as it appears in the function
*         name (e.g. Label in "astSetLabelAt").
*      type
*         The C type of the attribute.
*      component
*         The name of the class structure component that holds the attribute
*         value.
*      assign
*         An expression that evaluates to the value to be assigned to the
*         component.
*      nval
*         Specifies the number of values in the multi-valued attribute. The
*         "axis" values supplied to the created function should be in the
*         range zero to (nval - 1).

*  Notes:
*     -  To avoid problems with some compilers, you should not leave any white
*     space around the macro arguments.
*-
*/

/* Define the macro. */
#define MAKE_SET(attr,type,component,assign,nval) \
\
/* Private member function. */ \
/* ------------------------ */ \
static void Set##attr( AstWcsMap *this, int axis, type value, int *status ) { \
\
/* Check the global error status. */ \
   if ( !astOK ) return; \
\
/* Validate the axis index. */ \
   if( axis < 0 || axis >= nval ){ \
      astError( AST__AXIIN, "%s(%s): Index (%d) is invalid for attribute " \
                #attr " - it should be in the range 1 to %d.", status, \
                "astSet" #attr, astGetClass( this ), \
                axis + 1, nval ); \
\
/* Store the new value in the structure component. */ \
   } else { \
      this->component[ axis ] = (assign); \
   } \
} \
\
/* External interface. */ \
/* ------------------- */ \
void astSet##attr##_( AstWcsMap *this, int axis, type value, int *status ) { \
\
/* Check the global error status. */ \
   if ( !astOK ) return; \
\
/* Invoke the required method via the virtual function table. */ \
   (**astMEMBER(this,WcsMap,Set##attr))( this, axis, value, status ); \
}

/*
*
*  Name:
*     MAKE_TEST

*  Purpose:
*     Implement a method to test if a single value has been set in a
*     multi-valued attribute for a class.

*  Type:
*     Private macro.

*  Synopsis:
*     #include "wcsmap.h"
*     MAKE_TEST(attr,assign,nval)

*  Class Membership:
*     Defined by the WcsMap class.

*  Description:
*     This macro expands to an implementation of a private member function of
*     the form:
*
*        static int Test<Attribute>( AstWcsMap *this, int axis )
*
*     and an external interface function of the form:
*
*        int astTest<Attribute>_( AstWcsMap *this, int axis )
*
*     which implement a method for testing if a single value in a specified
*     multi-valued attribute has been set for a class.

*  Parameters:
*      attr
*         The name of the attribute to be tested, as it appears in the function
*         name (e.g. Label in "astTestLabelAt").
*      assign
*         An expression that evaluates to 0 or 1, to be used as the returned
*         value. This can use the string "axis" to represent the zero-based
*         index of the value within the attribute.
*      nval
*         Specifies the number of values in the multi-valued attribute. The
*         "axis" values supplied to the created function should be in the
*         range zero to (nval - 1).

*  Notes:
*     -  To avoid problems with some compilers, you should not leave any white
*     space around the macro arguments.
*-
*/

/* Define the macro. */
#define MAKE_TEST(attr,assign,nval) \
\
/* Private member function. */ \
/* ------------------------ */ \
static int Test##attr( AstWcsMap *this, int axis, int *status ) { \
   int result;                   /* Value to return */ \
\
/* Initialise */ \
   result = 0; \
\
/* Check the global error status. */ \
   if ( !astOK ) return result; \
\
\
/* Validate the axis index. */ \
   if( axis < 0 || axis >= nval ){ \
      astError( AST__AXIIN, "%s(%s): Index (%d) is invalid for attribute " \
                #attr " - it should be in the range 1 to %d.", status, \
                "astTest" #attr, astGetClass( this ), \
                axis + 1, nval ); \
\
/* Assign the result value. */ \
   } else { \
      result = (assign); \
   } \
\
/* Check for errors and clear the result if necessary. */ \
   if ( !astOK ) result = 0; \
\
/* Return the result. */ \
   return result; \
} \
/* External interface. */ \
/* ------------------- */ \
int astTest##attr##_( AstWcsMap *this, int axis, int *status ) { \
\
/* Check the global error status. */ \
   if ( !astOK ) return 0; \
\
/* Invoke the required method via the virtual function table. */ \
   return (**astMEMBER(this,WcsMap,Test##attr))( this, axis, status ); \
}


/* Include files. */
/* ============== */
/* Interface definitions. */
/* ---------------------- */

#include "globals.h"             /* Thread-safe global data access */
#include "error.h"               /* Error reporting facilities */
#include "memory.h"              /* Memory allocation facilities */
#include "globals.h"             /* Thread-safe global data access */
#include "object.h"              /* Base Object class */
#include "pointset.h"            /* Sets of points/coordinates */
#include "mapping.h"             /* Coordinate mappings (parent class) */
#include "unitmap.h"             /* Unit mappings */
#include "permmap.h"             /* Axis permutation mappings */
#include "wcsmap.h"              /* Interface definition for this class */
#include "pal.h"                 /* SLALIB function prototypes */
#include "channel.h"             /* I/O channels */
#include "proj.h"                /* WCSLIB projections and WCSLIB_MXPAR */

/* Error code definitions. */
/* ----------------------- */
#include "ast_err.h"             /* AST error codes */

/* C header files. */
/* --------------- */
#include <ctype.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>

/* Local Type Definitions. */
/* ----------------------- */
/* This structure is used to hold information describing a WCSLIB
   projection. */
typedef struct PrjData {
   int prj;                     /* WCSLIB projection identifier value */
   int mxpar;                   /* Max index for a lat axis projection param */
   int mxpar2;                  /* Max index for a lon axis projection param */
   char desc[60];               /* Long projection description */
   char ctype[5];               /* FITS CTYPE identifying string */
   int (* WcsFwd)(double, double, struct AstPrjPrm *, double *, double *);
                                /* Pointer to forward projection function */
   int (* WcsRev)(double, double, struct AstPrjPrm *, double *, double *);
                                /* Pointer to reverse projection function */
   double theta0;               /* Default native latitude of fiducial point */
} PrjData;

/* Module Variables. */
/* ================= */

/* Address of this static variable is used as a unique identifier for
   member of this class. */
static int class_check;

/* Pointers to parent class methods which are extended by this class. */
static int (* parent_getobjsize)( AstObject *, int * );
static AstPointSet *(* parent_transform)( AstMapping *, AstPointSet *, int, AstPointSet *, int * );
static const char *(* parent_getattrib)( AstObject *, const char *, int * );
static int (* parent_testattrib)( AstObject *, const char *, int * );
static void (* parent_clearattrib)( AstObject *, const char *, int * );
static void (* parent_setattrib)( AstObject *, const char *, int * );
static int *(* parent_mapsplit)( AstMapping *, int, const int *, AstMapping **, int * );

/* The following array of PrjData structured describes each of the WCSLIB
   projections. The last entry in the list should be for the AST__WCSBAD
   projection. This marks the end of the list. */
static PrjData PrjInfo[] = {
   { AST__AZP,  2, 4, "zenithal perspective", "-AZP", astAZPfwd, astAZPrev, AST__DPIBY2 },
   { AST__SZP,  3, 4, "slant zenithal perspective", "-SZP", astSZPfwd, astSZPrev, AST__DPIBY2 },
   { AST__TAN,  0, 4, "gnomonic", "-TAN",  astTANfwd, astTANrev, AST__DPIBY2 },
   { AST__STG,  0, 4, "stereographic", "-STG",  astSTGfwd, astSTGrev, AST__DPIBY2 },
   { AST__SIN,  2, 4, "orthographic", "-SIN",  astSINfwd, astSINrev, AST__DPIBY2 },
   { AST__ARC,  0, 4, "zenithal equidistant", "-ARC",  astARCfwd, astARCrev, AST__DPIBY2 },
   { AST__ZPN,  WCSLIB_MXPAR, 4, "zenithal polynomial", "-ZPN",  astZPNfwd, astZPNrev, AST__DPIBY2 },
   { AST__ZEA,  0, 4, "zenithal equal area", "-ZEA",  astZEAfwd, astZEArev, AST__DPIBY2 },
   { AST__AIR,  1, 4, "Airy", "-AIR",  astAIRfwd, astAIRrev, AST__DPIBY2 },
   { AST__CYP,  2, 4, "cylindrical perspective", "-CYP",  astCYPfwd, astCYPrev, 0.0 },
   { AST__CEA,  1, 4, "cylindrical equal area", "-CEA",  astCEAfwd, astCEArev, 0.0 },
   { AST__CAR,  0, 4, "Cartesian", "-CAR",  astCARfwd, astCARrev, 0.0 },
   { AST__MER,  0, 4, "Mercator", "-MER",  astMERfwd, astMERrev, 0.0 },
   { AST__SFL,  0, 4, "Sanson-Flamsteed", "-SFL",  astSFLfwd, astSFLrev, 0.0 },
   { AST__PAR,  0, 4, "parabolic", "-PAR",  astPARfwd, astPARrev, 0.0 },
   { AST__MOL,  0, 4, "Mollweide", "-MOL",  astMOLfwd, astMOLrev, 0.0 },
   { AST__AIT,  0, 4, "Hammer-Aitoff", "-AIT",  astAITfwd, astAITrev, 0.0 },
   { AST__COP,  2, 4, "conical perspective", "-COP",  astCOPfwd, astCOPrev, AST__BAD },
   { AST__COE,  2, 4, "conical equal area", "-COE",  astCOEfwd, astCOErev, AST__BAD },
   { AST__COD,  2, 4, "conical equidistant", "-COD",  astCODfwd, astCODrev, AST__BAD },
   { AST__COO,  2, 4, "conical orthomorphic", "-COO",  astCOOfwd, astCOOrev, AST__BAD },
   { AST__BON,  1, 4, "Bonne's equal area", "-BON",  astBONfwd, astBONrev, 0.0 },
   { AST__PCO,  0, 4, "polyconic", "-PCO",  astPCOfwd, astPCOrev, 0.0 },
   { AST__TSC,  0, 4, "tangential spherical cube", "-TSC",  astTSCfwd, astTSCrev, 0.0 },
   { AST__CSC,  0, 4, "cobe quadrilateralized spherical cube", "-CSC", astCSCfwd, astCSCrev, 0.0 },
   { AST__QSC,  0, 4, "quadrilateralized spherical cube", "-QSC",  astQSCfwd, astQSCrev, 0.0 },
   { AST__NCP,  2, 4, "AIPS north celestial pole", "-NCP",  NULL,   NULL, 0.0 },
   { AST__GLS,  0, 4, "sinusoidal", "-GLS",  astSFLfwd, astSFLrev, 0.0 },
   { AST__HPX,  2, 4, "HEALPix", "-HPX",  astHPXfwd, astHPXrev, 0.0 },
   { AST__XPH,  0, 4, "polar HEALPix", "-XPH",  astXPHfwd, astXPHrev, AST__DPIBY2 },
   { AST__TPN,  WCSLIB_MXPAR, WCSLIB_MXPAR, "gnomonic polynomial", "-TPN",  astTPNfwd, astTPNrev, AST__DPIBY2 },
   { AST__WCSBAD, 0, 4, "<null>",   "    ",  NULL,   NULL, 0.0 } };

/* Define macros for accessing each item of thread specific global data. */
#ifdef THREAD_SAFE

/* Define how to initialise thread-specific globals. */
#define GLOBAL_inits \
   globals->Class_Init = 0; \
   globals->GetAttrib_Buff[ 0 ] = 0;

/* Create the function that initialises global data for this module. */
astMAKE_INITGLOBALS(WcsMap)

/* Define macros for accessing each item of thread specific global data. */
#define class_init astGLOBAL(WcsMap,Class_Init)
#define class_vtab astGLOBAL(WcsMap,Class_Vtab)
#define getattrib_buff astGLOBAL(WcsMap,GetAttrib_Buff)



/* If thread safety is not needed, declare and initialise globals at static
   variables. */
#else

static char getattrib_buff[ 101 ];


/* Define the class virtual function table and its initialisation flag
   as static variables. */
static AstWcsMapVtab class_vtab;   /* Virtual function table */
static int class_init = 0;       /* Virtual function table initialised? */

#endif

/* External Interface Function Prototypes. */
/* ======================================= */
/* The following functions have public prototypes only (i.e. no
   protected prototypes), so we must provide local prototypes for use
   within this module. */
AstWcsMap *astWcsMapId_( int, int, int, int, const char *options, ... );

/* Prototypes for Private Member Functions. */
/* ======================================== */
static int GetObjSize( AstObject *, int * );
static double GetPV( AstWcsMap *, int, int, int * );
static int TestPV( AstWcsMap *, int, int, int * );
static void ClearPV( AstWcsMap *, int, int, int * );
static void SetPV( AstWcsMap *, int, int, double, int * );

static int GetPVMax( AstWcsMap *, int, int * );
static int GetWcsType( AstWcsMap *, int * );
static double GetNatLat( AstWcsMap *, int * );
static double GetNatLon( AstWcsMap *, int * );
static int GetWcsAxis( AstWcsMap *, int, int * );

static int GetFITSProj( AstWcsMap *, int * );
static int TestFITSProj( AstWcsMap *, int * );
static void ClearFITSProj( AstWcsMap *, int * );
static void SetFITSProj( AstWcsMap *, int, int * );

static int GetTPNTan( AstWcsMap *, int * );
static int TestTPNTan( AstWcsMap *, int * );
static void ClearTPNTan( AstWcsMap *, int * );
static void SetTPNTan( AstWcsMap *, int, int * );

static AstPointSet *Transform( AstMapping *, AstPointSet *, int, AstPointSet *, int * );
static const PrjData *FindPrjData( int, int * );
static const char *GetAttrib( AstObject *, const char *, int * );
static int CanMerge( AstMapping *, int, AstMapping *, int, int * );
static int CanSwap( AstMapping *, AstMapping *, int, int, int *, int * );
static int Equal( AstObject *, AstObject *, int * );
static int GetNP( AstWcsMap *, int, int * );
static int IsZenithal( AstWcsMap *, int * );
static int LongRange( const PrjData *, struct AstPrjPrm *, double *, double *, int * );
static int Map( AstWcsMap *, int, int, double *, double *, double *, double *, int * );
static int MapMerge( AstMapping *, int, int, int *, AstMapping ***, int **, int * );
static int TestAttrib( AstObject *, const char *, int * );
static void ClearAttrib( AstObject *, const char *, int * );
static void Copy( const AstObject *, AstObject *, int * );
static void CopyPV( AstWcsMap *, AstWcsMap *, int * );
static void Delete( AstObject *obj, int * );
static void Dump( AstObject *, AstChannel *, int * );
static void FreePV( AstWcsMap *, int * );
static void InitPrjPrm( AstWcsMap *, int * );
static void PermGet( AstPermMap *, int **, int **, double **, int * );
static void SetAttrib( AstObject *, const char *, int * );
static void WcsPerm( AstMapping **, int *, int, int * );
static int *MapSplit( AstMapping *, int, const int *, AstMapping **, int * );

/* Member functions. */
/* ================= */
static int CanMerge( AstMapping *map1, int inv1, AstMapping *map2, int inv2, int *status ){
/*
*
*  Name:
*     CanMerge

*  Purpose:
*     Checks if two WcsMaps can be merged.

*  Type:
*     Private function.

*  Synopsis:
*     #include "wcsmap.h"
*     int CanMerge( AstMapping *map1, int inv1, AstMapping *map2, int inv2, int *status )

*  Class Membership:
*     WcsMap internal utility function.

*  Description:
*     This function checks the two supplied Mappings to see if they are
*     two WcsMaps which can be merged. This is only possible if they
*     form an inverse pair.

*  Parameters:
*     map1
*        A pointer to the first mapping.
*     map2
*        A pointer to the second mapping.
*     inv1
*        The invert flag to use with the first mapping.
*     inv2
*        The invert flag to use with the second mapping.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     1 if the WcsMaps can be merged, zero otherwise.

*/

/* Local Variables: */
   AstWcsMap *wcs1;               /* Pointer to first WcsMap */
   AstWcsMap *wcs2;               /* Pointer to second WcsMap */
   int m;                         /* Projection parameter index */
   int ret;                       /* Can the Mappings be merged? */
   int i;                         /* Axis index */

/* Initialise the returned value. */
   ret = 0;

/* Check the global error status. */
   if ( !astOK ) return ret;

/* Both Mappings must be WcsMaps to merge. */
   if( !strcmp( "WcsMap", astGetClass( map1 ) ) &&
       !strcmp( "WcsMap", astGetClass( map2 ) ) ) {

/* Get pointers to the WcsMaps. */
      wcs1 = (AstWcsMap *) map1;
      wcs2 = (AstWcsMap *) map2;

/* Check that the two WcsMaps performs the same sort of projection, and
   have the same number of axes. */
      if( astGetWcsType( wcs1 ) == astGetWcsType( wcs2 ) &&
          astGetNin( wcs1 ) == astGetNin( wcs2 ) ) {

/* Check that the Mappings are applied in opposite senses. */
         if( inv1 != inv2 ) {

/* Check that the latitude and longitude axes have the same indices in
   both WcsMaps. */
            if( astGetWcsAxis( wcs1, 0 ) == astGetWcsAxis( wcs2, 0 ) &&
                astGetWcsAxis( wcs1, 1 ) == astGetWcsAxis( wcs2, 1 ) ){

/* We nopw check the projection parameters are equal. Assume they are for
   the moment. */
               ret = 1;

/* Check the parameters for each axis in turn. */
               for( i = 0; i < astGetNin( wcs1 ); i++ ){

/* If the two WcsMaps have a different number of parameters for this axes,
   they cannot merge. */
                  if( GetNP( wcs1, i, status ) != GetNP( wcs1, i, status ) ){
                     ret = 0;
                     break;

/* Otherwise, check each parameter value in turn. If any are found which
   are not equal, the WcsMaps cannot merge. */
                  } else {
                     for( m = 0; m < GetNP( wcs1, i, status ); m++ ){
                        if( !EQUAL( astGetPV( wcs1, i, m ),
                                    astGetPV( wcs2, i, m ) ) ){
                           ret = 0;
                           break;
                        }
                     }
                     if( !ret ) break;
                  }
               }
            }
         }
      }
   }

/* Return the answer. */
   return ret;
}

static int CanSwap( AstMapping *map1, AstMapping *map2, int inv1, int inv2,
                    int *simpler, int *status ){
/*
*  Name:

*     CanSwap

*  Purpose:
*     Determine if two Mappings could be swapped.

*  Type:
*     Private function.

*  Synopsis:
*     #include "wcsmap.h"
*     int CanSwap( AstMapping *map1, AstMapping *map2, int inv1, int inv2,
*                  int *simpler )

*  Class Membership:
*     WcsMap member function

*  Description:
*     This function returns a flag indicating if the pair of supplied
*     Mappings could be replaced by an equivalent pair of Mappings from the
*     same classes as the supplied pair, but in reversed order. Each pair
*     of Mappings is considered to be compunded in series. The supplied
*     Mapings are not changed in any way.

*  Parameters:
*     map1
*        The Mapping to be applied first.
*     map2
*        The Mapping to be applied second.
*     inv1
*        The invert flag to use with map1. A value of zero causes the forward
*        mapping to be used, and a non-zero value causes the inverse
*        mapping to be used.
*     inv2
*        The invert flag to use with map2.
*     simpler
*        Addresss of a location at which to return a flag indicating if
*        the swapped Mappings would be intrinsically simpler than the
*        original Mappings.

*  Returned Value:
*     1 if the Mappings could be swapped, 0 otherwise.

*  Notes:
*     -  One of the supplied pair of Mappings must be a WcsMap.
*     -  A value of 0 is returned if the two Mappings could be merged into
*     a single Mapping.
*     -  A value of 0 is returned if an error has already occurred, or if
*     this function should fail for any reason.
*/

/* Local Variables: */
   AstMapping *nowcs;        /* Pointer to non-WcsMap Mapping */
   AstWcsMap  *wcs;          /* Pointer to WcsMap Mapping */
   const char *class1;       /* Pointer to map1 class string */
   const char *class2;       /* Pointer to map2 class string */
   const char *nowcs_class;  /* Pointer to non-WcsMap class string */
   double *consts;           /* Pointer to constants array */
   int *inperm;              /* Pointer to input axis permutation array */
   int *outperm;             /* Pointer to output axis permutation array */
   int i;                    /* Loop count */
   int invert[ 2 ];          /* Original invert flags */
   int latax;                /* Index of latitude axis in WcsMap */
   int lonax;                /* Index of longitude axis in WcsMap */
   int nin;                  /* No. of input coordinates for the PermMap */
   int nout;                 /* No. of output coordinates for the PermMap */
   int ret;                  /* Returned flag */

/* Check the global error status. */
   if ( !astOK ) return 0;

/* Initialise */
   ret = 0;
   *simpler = 0;

/* Temporarily set the Invert attributes of both Mappings to the supplied
   values. */
   invert[ 0 ] = astGetInvert( map1 );
   astSetInvert( map1, inv1 );

   invert[ 1 ] = astGetInvert( map2 );
   astSetInvert( map2, inv2 );

/* Get the classes of the two mappings. */
   class1 = astGetClass( map1 );
   class2 = astGetClass( map2 );
   if( astOK ){

/* Get a pointer to the non-WcsMap Mapping. */
      if( !strcmp( class1, "WcsMap" ) ){
         wcs = (AstWcsMap *) map1;
         nowcs = map2;
         nowcs_class = class2;
      } else {
         nowcs = map1;
         wcs = (AstWcsMap *) map2;
         nowcs_class = class1;
      }

/* If it is a PermMap, the Mappings can be swapped so long as:
   1) all links between input and output axes in the PermMap are
   bi-directional. This does not preclude the existence of unconnected axes,
   which do not have links (bi-directional or otherwise).
   2) The PermMap passesd though both the longitude and latitude axes of
   the WcsMap */
      if( !strcmp( nowcs_class, "PermMap" ) ){

/* Get the number of input and output coordinates. */
         nin = astGetNin( nowcs );
         nout = astGetNout( nowcs );

/* We need to know the axis permutation arrays and constants array for
   the PermMap. */
         PermGet( (AstPermMap *) nowcs, &outperm, &inperm, &consts, status );
         if( astOK ) {

/* Indicate we can swap with the PermMap. */
            ret = 1;

/* Check each output axis. If any links between axes are found which are
   not bi-directional, indicate that we cannot swap with the PermMap. */
            for( i = 0; i < nout; i++ ){
               if( outperm[ i ] >= 0 && outperm[ i ] < nin ) {
                  if( inperm[ outperm[ i ] ] != i ) {
                     ret = 0;
                     break;
                  }
               }
            }

/* Check each input axis. If any links between axes are found which are
   not bi-directional, indicate that we cannot swap with the PermMap. */
            for( i = 0; i < nin; i++ ){
               if( inperm[ i ] >= 0 && inperm[ i ] < nout ) {
                  if( outperm[ inperm[ i ] ] != i ) {
                     ret = 0;
                     break;
                  }
               }
            }

/* Check that the longitude and latitude axes both have bi-directional
   links in the PermMap, or are both unassigned. */
            if( ret ) {

/* Get the indices of the longitude and latitude axes in the WcsMap */
               lonax = astGetWcsAxis( wcs, 0 );
               latax = astGetWcsAxis( wcs, 1 );

/* If the WcsMap is applied first... */
               if( wcs == (AstWcsMap *) map1 ) {
                  if( inperm[ lonax] < 0 && inperm[ latax ] < 0 ) {
                     ret = 1;
                  } else if( inperm[ lonax ] < 0 || inperm[ lonax ] >= nout ||
                             inperm[ latax ] < 0 || inperm[ latax ] >= nout ) {
                     ret = 0;
                  }

/* If the WcsMap is applied second ... */
               } else {
                  if( outperm[ lonax ] < 0 && outperm[ latax ] < 0 ) {
                     ret = 1;
                  } else if( outperm[ lonax ] < 0 || outperm[ lonax ] >= nin ||
                             outperm[ latax ] < 0 || outperm[ latax ] >= nin ) {
                     ret = 0;
                  }
               }
            }

/* If we can swap with the PermMap, the swapped Mappings may be
   intrinsically simpler than the original mappings. */
            if( ret ) {

/* If the PermMap precedes the WcsMap, this will be the case if the PermMap
   has more outputs than inputs. If the WcsMap precedes the PermMap, this
   will be the case if the PermMap has more inputs than outputs. */
               *simpler = ( nowcs == map1 ) ? nout > nin : nin > nout;
            }

/* Free the axis permutation and constants arrays. */
            outperm = (int *) astFree( (void *) outperm );
            inperm = (int *) astFree( (void *) inperm );
            consts = (double *) astFree( (void *) consts );
         }
      }
   }

/* Re-instate the original settings of the Invert attributes for the
   supplied MatrixMaps. */
   astSetInvert( map1, invert[ 0 ] );
   astSetInvert( map2, invert[ 1 ] );

/* Return the answer. */
   return astOK ? ret : 0;
}

static void ClearAttrib( AstObject *this_object, const char *attrib, int *status ) {
/*
*  Name:
*     ClearAttrib

*  Purpose:
*     Clear an attribute value for a WcsMap.

*  Type:
*     Private function.

*  Synopsis:
*     #include "wcsmap.h"
*     void ClearAttrib( AstObject *this, const char *attrib, int *status )

*  Class Membership:
*     WcsMap member function (over-rides the astClearAttrib protected
*     method inherited from the Mapping class).

*  Description:
*     This function clears the value of a specified attribute for a
*     WcsMap, so that the default value will subsequently be used.

*  Parameters:
*     this
*        Pointer to the WcsMap.
*     attrib
*        Pointer to a null terminated string specifying the attribute
*        name.  This should be in lower case with no surrounding white
*        space.
*     status
*        Pointer to the inherited status variable.
*/

/* Local Variables: */
   AstWcsMap *this;             /* Pointer to the WcsMap structure */
   int i;                       /* Axis index */
   int len;                     /* Length of the attribute name */
   int m;                       /* Projection parameter index */
   int nc;                      /* No. of characters read by astSscanf */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain a pointer to the WcsMap structure. */
   this = (AstWcsMap *) this_object;

/* Obtain the length of the attrib string. */
   len = strlen( attrib );

/* Check the attribute name and clear the appropriate attribute. */

/* ProjP. */
/* ------ */
   if ( nc = 0, ( 1 == astSscanf( attrib, "prpjp(%d)%n", &m, &nc ) )
                  && ( nc >= len ) ) {
      astClearPV( this, astGetWcsAxis( this, 1 ), m );

/* PV. */
/* ------ */
   } else if ( nc = 0, ( 2 == astSscanf( attrib, "pv%d_%d%n", &i, &m, &nc ) )
                  && ( nc >= len ) ) {
      astClearPV( this, i - 1, m );

/* If the name was not recognised, test if it matches any of the
   read-only attributes of this class. If it does, then report an
   error. */
   } else if ( ( nc = 0, ( 1 == astSscanf( attrib, "wcsaxis(%d)%n", &i, &nc ) )
                           && ( nc >= len ) ) ||
        !strcmp( attrib, "wcstype" ) ||
        !strcmp( attrib, "natlat" ) ||
        !strcmp( attrib, "natlon" ) ){
      astError( AST__NOWRT, "astClear: Invalid attempt to clear the \"%s\" "
                "value for a %s.", status, attrib, astGetClass( this ) );
      astError( AST__NOWRT, "This is a read-only attribute." , status);

/* If the attribute is still not recognised, pass it on to the parent
   method for further interpretation. */
   } else {
      (*parent_clearattrib)( this_object, attrib, status );
   }
}

static void ClearPV( AstWcsMap *this, int i, int m, int *status ) {
/*
*+
*  Name:
*     astClearPV

*  Purpose:
*     Clear a PVi_m attribute.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "wcsmap.h"
*     void astClearPV( AstWcsMap *this, int i, int m )

*  Class Membership:
*     WcsMap protected function

*  Description:
*     This function clears a specified member of the PV attribute array, by
*     resetting its value to AST__BAD.

*  Parameters:
*     this
*        A pointer to the WcsMap.
*     i
*        Zero based axis index.
*     m
*        Zero based parameter index.

*-
*/
/* Local Variables; */
   int npar;
   int mxpar;

/* Check the global error status. */
   if ( !astOK ) return;

/* Validate the axis index. */
   if( i < 0 || i >= astGetNin( this ) ){
      astError( AST__AXIIN, "astClearPV(%s): Axis index (%d) is invalid in "
                "attribute PV%d_%d  - it should be in the range 1 to %d.",
                status, astGetClass( this ), i + 1, i + 1, m,
                astGetNin( this ) );

   } else {

/* Find the maximum number of parameters allowed for the axis. */
      mxpar = astGetPVMax( this, i );

/* Ignore unused parameters. */
      if( m < 0 || m > mxpar ){

/* See if the parameter is currently set. Is so, set its value to
   AST__BAD. */
      } else if( this->np && this->p ){
         npar = this->np[ i ];
         if( m < npar && this->p[ i ] ) this->p[ i ][ m ] = AST__BAD;
      }

/* Re-initialize the values stored in the "AstPrjPrm" structure. */
      InitPrjPrm( this, status );
   }
}

static void ClearTPNTan( AstWcsMap *this, int *status ) {
/*
*+
*  Name:
*     astClearTPNTan

*  Purpose:
*     Clear the TPNTan attribute.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "wcsmap.h"
*     void ClearTPNTan( AstWcsMap *this, int *status )

*  Class Membership:
*     WcsMap protected function

*  Description:
*     This function clears the TPNTan attribute, ensuring the projection
*     parameters used by WCSLIB are adjusted accordingly.

*  Parameters:
*     this
*        A pointer to the WcsMap.

*-
*/

/* Check the global error status. */
   if ( !astOK ) return;

/* Clear the value. */
   this->tpn_tan = -INT_MAX;

/* Re-initialize the values stored in the "AstPrjPrm" structure. */
   InitPrjPrm( this, status );
}

static void CopyPV( AstWcsMap *in, AstWcsMap *out, int *status ) {
/*
*  Name:
*     CopyPV

*  Purpose:
*     Copy projection parameter information from one WcsMap to another.

*  Type:
*     Private function.

*  Synopsis:
*     void CopyPV( AstWcsMap *in, AstWcsMap *out )

*  Description:
*     This function copies projection parameter information from one
*     WcsMap to another.

*  Parameters:
*     in
*        Pointer to the input WcsMap.
*     out
*        Pointer to the output WcsMap.

*/


/* Local Variables: */
   int i;                        /* Axis index */
   int latax_in;                 /* Index of input latitude axis */
   int latax_out;                /* Index of output latitude axis */
   int lonax_in;                 /* Index of input longitude axis */
   int lonax_out;                /* Index of output longitude axis */
   int nax_out;                  /* No. of axis in the output WcsMap */

/* Check the global error status. */
   if ( !astOK ) return;

/* Nullify the pointers stored in the output WcsMap since these may
   currently be pointing at good data. Otherwise, the good data could be
   freed by accident if the output object is deleted due to an error
   occuring in this function. */
   out->np = NULL;
   out->p = NULL;

/* Do nothing more if either of the input pointers are null (i.e. if there
   are no projection parameters. */
   if( in->np && in->p ){

/* Store the number of axes in the input and output WcsMaps */
      nax_out = astGetNin( out );

/* Allocate memory for the array holding the number of projection parameters
   associated with each axis. */
      out->np = (int *) astMalloc( sizeof( int )*nax_out );

/* Allocate memory for the array of pointers which identify the arrays
   holding the parameter values. */
      out->p = (double **) astMalloc( sizeof( double *)*nax_out );

/* Check pointers can be used */
      if( astOK ) {

/* Initialise the above arrays. */
         for( i = 0; i < nax_out; i++ ) {
            (out->np)[ i ] = 0;
            (out->p)[ i ] = NULL;
         }

/* Copy the longitude and latitude values from in to out (other axes do
   not have projection parameters). */
         lonax_in = astGetWcsAxis( in, 0 );
         latax_in = astGetWcsAxis( in, 1 );
         lonax_out = astGetWcsAxis( out, 0 );
         latax_out = astGetWcsAxis( out, 1 );

         (out->np)[ lonax_out ] = (in->np)[ lonax_in ];
         (out->p)[ lonax_out ] = (double *) astStore( NULL,
                                          (void *) (in->p)[ lonax_in ],
                                          sizeof(double)*(in->np)[ lonax_in ] );

         (out->np)[ latax_out ] = (in->np)[ latax_in ];
         (out->p)[ latax_out ] = (double *) astStore( NULL,
                                          (void *) (in->p)[ latax_in ],
                                          sizeof(double)*(in->np)[ latax_in ] );
      }

/* If an error has occurred, free the output arrays. */
      if( !astOK ) FreePV( out, status );

   }

/* Re-initialize the values stored in the "AstPrjPrm" structure. */
   InitPrjPrm( out, status );

}

static int Equal( AstObject *this_object, AstObject *that_object, int *status ) {
/*
*  Name:
*     Equal

*  Purpose:
*     Test if two WcsMaps are equivalent.

*  Type:
*     Private function.

*  Synopsis:
*     #include "wcsmap.h"
*     int Equal( AstObject *this, AstObject *that, int *status )

*  Class Membership:
*     WcsMap member function (over-rides the astEqual protected
*     method inherited from the astMapping class).

*  Description:
*     This function returns a boolean result (0 or 1) to indicate whether
*     two WcsMaps are equivalent.

*  Parameters:
*     this
*        Pointer to the first Object (a WcsMap).
*     that
*        Pointer to the second Object.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     One if the WcsMaps are equivalent, zero otherwise.

*  Notes:
*     - A value of zero will be returned if this function is invoked
*     with the global status set, or if it should fail for any reason.
*/

/* Local Variables: */
   AstWcsMap *that;
   AstWcsMap *this;
   int i, j;
   int nin;
   int nout;
   int result;

/* Initialise. */
   result = 0;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Obtain pointers to the two WcsMap structures. */
   this = (AstWcsMap *) this_object;
   that = (AstWcsMap *) that_object;

/* Check the second object is a WcsMap. We know the first is a
   WcsMap since we have arrived at this implementation of the virtual
   function. */
   if( astIsAWcsMap( that ) ) {

/* Get the number of inputs and outputs and check they are the same for both. */
      nin = astGetNin( this );
      nout = astGetNout( this );
      if( astGetNin( that ) == nin && astGetNout( that ) == nout ) {

/* If the Invert flags for the two WcsMaps differ, it may still be possible
   for them to be equivalent. First compare the WcsMaps if their Invert
   flags are the same. In this case all the attributes of the two WcsMaps
   must be identical. */
         if( astGetInvert( this ) == astGetInvert( that ) ) {

            if( this->type == that->type &&
                this->wcsaxis[ 0 ] == that->wcsaxis[ 0 ] &&
                this->wcsaxis[ 1 ] == that->wcsaxis[ 1 ] ) {

               result = 1;

               if( this->np && that->np ){

                  for( i = 0; i < nout && result; i++ ) {

                     if( (this->np)[ i ] != (that->np)[ i ] ) {
                        result = 0;

                     } else if( (this->p)[ i ] && !(this->p)[ i ] ) {
                        result = 0;

                     } else if( !(this->p)[ i ] && (this->p)[ i ] ) {
                        result = 0;

                     } else if( (this->p)[ i ] && (this->p)[ i ] ) {

                        for( j = 0; j < (this->np)[ i ]; j++ ) {
                           if( !astEQUAL( (this->p)[ i ][ j ],
                                          (that->p)[ i ][ j ] ) ) {
                              result = 0;
                              break;
                           }
                        }
                     }
                  }
               }

            } else if( this->np || that->np ){
               result = 0;
            }

/* If the Invert flags for the two WcsMaps differ, the attributes of the two
   WcsMaps must be inversely related to each other. */
         } else {

/* In the specific case of a WcsMap, Invert flags must be equal. */
            result = 0;

         }
      }
   }

/* If an error occurred, clear the result value. */
   if ( !astOK ) result = 0;

/* Return the result, */
   return result;
}

static const PrjData *FindPrjData( int type, int *status ){
/*
*+
*  Name:
*     FindPrjData

*  Purpose:
*     Get information about a WCSLIB projection given a projection type.

*  Type:
*     Private function.

*  Synopsis:
*     #include "wcsmap.h"
*     const PrjData *FindPrjData( int type, int *status )

*  Class Membership:
*     WcsMap member function

*  Description:
*     This function returns a pointer to an PrjData structure describing
*     the WCSLIB projection with the supplied type.

*  Parameters:
*     type
*        The projection type.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     A pointer to the "const" PrjData structure describing the projection.

*  Notes:
*     -  The returned pointer points to an element in a static array and
*     should not be freed.
*     -  This function attempts to execute even if an error has already
*     occurred. A description of a "null" projection will be returned if
*     this function subsequently fails (for instance if the projection is
*     not recognised).
*-
*/

   const PrjData *data;
   data = PrjInfo;
   while( data->prj != AST__WCSBAD && data->prj != type ) data++;
   return data;
}

static void FreePV( AstWcsMap *this, int *status ) {
/*
*
*  Name:
*     FreePV

*  Purpose:
*     Free memory used to hold projection parameters

*  Type:
*     Private function.

*  Synopsis:
*     #include "wcsmap.h"
*     FreePV( AstWcsMap *this, int *status )

*  Class Membership:
*     WcsMap private function

*  Description:
*     This function frees all the dynamic memory used to store projection
*     parameter information in the supplied WcsMap.

*  Parameters:
*     this
*        A pointer to the WcsMap.
*     status
*        Pointer to the inherited status variable.

*  Notes:
*     This function attempts to execute even if an error has already occurred.

*
*/
   int i;              /* Axis index */

   if( this->np ) this->np = (int *) astFree( (void *) this->np );
   if( this->p ){
      for( i = 0; i < astGetNin( this ); i++ ){
         this->p[ i ]  = (double *) astFree( (void *) this->p[ i ] );
      }
      this->p = (double **) astFree( (void *) this->p );
   }

/* Re-initialize the values stored in the "AstPrjPrm" structure. */
   InitPrjPrm( this, status );


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
*     #include "wcsmap.h"
*     int GetObjSize( AstObject *this, int *status )

*  Class Membership:
*     WcsMap member function (over-rides the astGetObjSize protected
*     method inherited from the parent class).

*  Description:
*     This function returns the in-memory size of the supplied WcsMap,
*     in bytes.

*  Parameters:
*     this
*        Pointer to the WcsMap.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     The Object size, in bytes.

*  Notes:
*     - A value of zero will be returned if this function is invoked
*     with the global status set, or if it should fail for any reason.
*/

/* Local Variables: */
   AstWcsMap *this;         /* Pointer to WcsMap structure */
   int result;              /* Result value to return */
   int i;                   /* Axis index */

/* Initialise. */
   result = 0;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Obtain a pointers to the WcsMap structure. */
   this = (AstWcsMap *) this_object;

/* Invoke the GetObjSize method inherited from the parent class, and then
   add on any components of the class structure defined by thsi class
   which are stored in dynamically allocated memory. */
   result = (*parent_getobjsize)( this_object, status );

   result += astTSizeOf( this->np );
   if( this->p ){
      for( i = 0; i < astGetNin( this ); i++ ){
         result += astTSizeOf( (void *) this->p[ i ] );
      }
      result += astTSizeOf( this->p );
   }

   result += astTSizeOf( this->params.p );
   result += astTSizeOf( this->params.p2 );

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
*     Get the value of a specified attribute for a WcsMap.

*  Type:
*     Private function.

*  Synopsis:
*     #include "wcsmap.h"
*     const char *GetAttrib( AstObject *this, const char *attrib, int *status )

*  Class Membership:
*     WcsMap member function (over-rides the protected astGetAttrib
*     method inherited from the Mapping class).

*  Description:
*     This function returns a pointer to the value of a specified
*     attribute for a WcsMap, formatted as a character string.

*  Parameters:
*     this
*        Pointer to the WcsMap.
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
*     within the WcsMap, or at static memory. The contents of the
*     string may be over-written or the pointer may become invalid
*     following a further invocation of the same function or any
*     modification of the WcsMap. A copy of the string should
*     therefore be made if necessary.
*     - A NULL pointer will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*/

/* Local Variables: */
   astDECLARE_GLOBALS           /* Pointer to thread-specific global data */
   AstWcsMap *this;             /* Pointer to the WcsMap structure */
   const char *result;          /* Pointer value to return */
   double dval;                 /* Floating point attribute value */
   int i;                       /* Axis index */
   int ival;                    /* Integer attribute value */
   int len;                     /* Length of attribute string */
   int m;                       /* Projection parameter index */
   int nc;                      /* No. of characters read by astSscanf */

/* Initialise. */
   result = NULL;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(this_object);

/* Obtain a pointer to the WcsMap structure. */
   this = (AstWcsMap *) this_object;

/* Obtain the length of the attrib string. */
   len = strlen( attrib );

/* Compare "attrib" with each recognised attribute name in turn,
   obtaining the value of the required attribute. If necessary, write
   the value into "getattrib_buff" as a null-terminated string in an appropriate
   format.  Set "result" to point at the result string. */

/* ProjP. */
/* ------ */
   if ( nc = 0, ( 1 == astSscanf( attrib, "projp(%d)%n", &m, &nc ) )
                  && ( nc >= len ) ) {
      dval = astGetPV( this, astGetWcsAxis( this, 1 ), m );
      if ( astOK ) {
         (void) sprintf( getattrib_buff, "%.*g", DBL_DIG, dval );
         result = getattrib_buff;
      }

/* PV. */
/* --- */
   } else if ( nc = 0, ( 2 == astSscanf( attrib, "pv%d_%d%n", &i, &m, &nc ) )
                  && ( nc >= len ) ) {
      dval = astGetPV( this, i - 1, m );
      if ( astOK ) {
         (void) sprintf( getattrib_buff, "%.*g", DBL_DIG, dval );
         result = getattrib_buff;
      }

/* WcsType */
/* ======= */
   } else if ( !strcmp( attrib, "wcstype" ) ) {
      ival = astGetWcsType( this );
      if ( astOK ) {
         (void) sprintf( getattrib_buff, "%d", ival );
         result = getattrib_buff;
      }

/* PVMax */
/* ===== */
   } else if ( nc = 0, ( 1 == astSscanf( attrib, "pvmax(%d)%n", &i, &nc ) )
                  && ( nc >= len ) ) {
      ival = astGetPVMax( this, i - 1 );
      if ( astOK ) {
         (void) sprintf( getattrib_buff, "%d", ival );
         result = getattrib_buff;
      }

/* NatLat */
/* ====== */
   } else if ( !strcmp( attrib, "natlat" ) ) {
      dval = astGetNatLat( this );
      if ( astOK ) {
         (void) sprintf( getattrib_buff, "%.*g", DBL_DIG, dval );
         result = getattrib_buff;
      }

/* NatLon */
/* ====== */
   } else if ( !strcmp( attrib, "natlon" ) ) {
      dval = astGetNatLon( this );
      if ( astOK ) {
         (void) sprintf( getattrib_buff, "%.*g", DBL_DIG, dval );
         result = getattrib_buff;
      }


/* WcsAxis */
/* ======= */
   } else if ( nc = 0, ( 1 == astSscanf( attrib, "wcsaxis(%d)%n", &i, &nc ) )
                         && ( nc >= len ) ) {
      ival = astGetWcsAxis( this, i - 1 ) + 1;
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

static double GetNatLat( AstWcsMap *this, int *status ) {
/*
*+
*  Name:
*     GetNatLat

*  Purpose:
*     Get the value of the NatLat attribute.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "wcsmap.h"
*     double GetNatLat( AstWcsMap *this, int *status )

*  Class Membership:
*     WcsMap protected function

*  Description:
*     This function returns the value of the NatLat attribute. This is
*     fixed value for most projection types , defined in the FITS-WCS paper
*     II. For instance, all zenithal projections have NatLat = PI/2 (90
*     degrees). For some prjections (e.g. conics), the value is defined
*     by a projection parameter.

*  Parameters:
*     this
*        A pointer to the WcsMap.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     The attribute value, in radians.

*-
*/
   double ret;     /* Returned value */

/* The native latitude of the reference point of the projection is
   constant for most projection, but for some (the conics) it is
   specified by projection one on the latitude axis. */
   ret = FindPrjData( this->type, status )->theta0;
   if( ret == AST__BAD ){
      ret = astGetPV( this, astGetWcsAxis( this, 1 ), 1 );
      if( ret != AST__BAD ) ret *= AST__DD2R;
   }

/* Return the result. */
   return ret;
}

static double GetNatLon( AstWcsMap *this, int *status ) {
/*
*+
*  Name:
*     GetNatLon

*  Purpose:
*     Get the value of the NatLon attribute.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "wcsmap.h"
*     double GetNatLon( AstWcsMap *this, int *status )

*  Class Membership:
*     WcsMap protected function

*  Description:
*     This function returns the value of the NatLon attribute. This is
*     fixed value of zero for all projection types.

*  Parameters:
*     this
*        A pointer to the WcsMap.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     The attribute value, in radians.

*-
*/
   return 0.0;
}

static int GetNP( AstWcsMap *this, int i, int *status ) {
/*
*+
*  Name:
*     GetNP

*  Purpose:
*     Get the number of projection parameters for a specified axis.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "wcsmap.h"
*     int GetNP( AstWcsMap *this, int i, int *status )

*  Class Membership:
*     WcsMap protected function

*  Description:
*     This function returns the current number of projection parameters
*     associated with the speified axis. Some of these may be unset (i.e.
*     equal to AST__BAD). The returned number is the size of the array
*     holding the projection parameters.

*  Parameters:
*     this
*        A pointer to the WcsMap.
*     i
*        Zero based axis index.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     The number of projection parameters for the specified axis.

*-
*/
   double ret;

/* Initialise */
   ret = 0;

/* Check the global error status. */
   if ( !astOK ) return ret;

/* Validate the axis index, and get the count. */
   if( i >= 0 && this->np && i < astGetNin( this ) ) ret = this->np[ i ];

   return ret;

}

static double GetPV( AstWcsMap *this, int i, int m, int *status ) {
/*
*+
*  Name:
*     astGetPV

*  Purpose:
*     Get the value of a PVi_m attribute.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "wcsmap.h"
*     double astGetPV( AstWcsMap *this, int i, int m )

*  Class Membership:
*     WcsMap protected function

*  Description:
*     This function returns the current value of a specified member of the
*     PV attribute array. A value of AST__BAD is returned if no value has
*     been set for the parameter.

*  Parameters:
*     this
*        A pointer to the WcsMap.
*     i
*        Zero based axis index.
*     m
*        Zero based parameter index.

*  Returned Value:
*     The value of the requested attribute, of AST__BAD if not set.

*-
*/

/* Local Variables: */
   double ret;
   int npar;
   int mxpar;

/* Initialise */
   ret = AST__BAD;

/* Check the global error status. */
   if ( !astOK ) return ret;

/* Validate the axis index. */
   if( i < 0 || i >= astGetNin( this ) ){
      astError( AST__AXIIN, "astGetPV(%s): Axis index (%d) is invalid in "
                "attribute PV%d_%d  - it should be in the range 1 to %d.",
                status, astGetClass( this ), i + 1, i + 1, m, astGetNin( this ) );

/* Find the maximum number of parameters allowed for the axis. */
   } else {
      mxpar = astGetPVMax( this, i );

/* Validate the parameter index. */
      if( m < 0 || m > mxpar ){
         astError( AST__AXIIN, "astGetPV(%s): Parameter index (%d) is invalid "
                   "in attribute PV%d_%d for a \"%s\" projection - it should be "
                   "in the range 0 to %d.", status, astGetClass( this ), m, i + 1, m,
                   FindPrjData( this->type, status )->ctype, mxpar );

/* For latitude parameters use the values in the "params" structure which will
   have been defaulted. */
      } else if( i == astGetWcsAxis( this, 1 ) ) {
         ret = (this->params).p[ m ];

/* For other axes, see if the arrays stored in the WcsMap structure extend as
   far as the requested parameter. If so, return the required attribute value.
   Otherwise the AST__BAD value initialised above is retained. */
      } else if( this->np && this->p ){
         npar = this->np[ i ];
         if( m < npar && this->p[ i ] ) ret = this->p[ i ][ m ];
      }

/* FITS-WCS paper II gives defaults for the first 3 longitude axis
   parameters. The AST-specific TPN projection does not use this
   convention since it needs all projection parameters to specify
   correction terms. */
      if( ret == AST__BAD && i == astGetWcsAxis( this, 0 ) &&
          astGetWcsType( this ) != AST__TPN ) {

/* Parameter zero has a default of zero. */
         if( m == 0 ) {
            ret = 0.0;

/* Parameter one has a default equal to the native longitude of the
   reference point of the projection, in degrees. */
         } else if( m == 1 ) {
            ret = astGetNatLon( this )*AST__DR2D;

/* Parameter two has a default equal to the native latitude of the
   reference point of the projection (in degrees). This is constant for
   most projection, but for some (the conics) it is specified by
   projection one on the latitude axis. */
         } else if( m == 2 ) {
            ret = astGetNatLat( this )*AST__DR2D;
         }
      }
   }

   return ret;

}

static int GetPVMax( AstWcsMap *this, int i, int *status ) {
/*
*+
*  Name:
*     astGetPVMax

*  Purpose:
*     Get the maximum projection parameter index for a WcsMap.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "wcsmap.h"
*     int astGetPVMax( AstWcsMap *this, int i )

*  Class Membership:
*     WcsMap protected function

*  Description:
*     This function returns the largest legal projection parameter index
*     for a specified axis of the given WcsMap (i.e. the largest value of
*     "m" in the attribute "PVi_m").

*  Parameters:
*     this
*        A pointer to the WcsMap.
*     i
*        Zero based axis index.

*  Returned Value:
*     The largest legal projection parameter index, or -1 if no
*     projection parameters are allowed on the specified axis.

*-
*/

/* Local Variables: */
   int mxpar;

/* Initialise */
   mxpar = 0;

/* Check the global error status. */
   if ( !astOK ) return -1;

/* Validate the axis index. */
   if( i < 0 || i >= astGetNin( this ) ){
      astError( AST__AXIIN, "astGetPVMax(%s): Axis index (%d) is invalid in "
                "attribute PVMax(%d)  - it should be in the range 1 to %d.",
                status, astGetClass( this ), i + 1, i + 1, astGetNin( this ) );

/* Find the maximum number of parameters allowed for the axis. */
   } else if( i == astGetWcsAxis( this, 0 ) ) {
      mxpar = astSizeOf( this->params.p2 )/sizeof( double );

   } else if( i == astGetWcsAxis( this, 1 ) ) {
      mxpar = astSizeOf( this->params.p )/sizeof( double );

   }

/* The mxpar variable holds the max number of parameters. Return the the
   largest legal parameter index (one less than the max number of
   parameters). */
   return mxpar - 1;
}

static void InitPrjPrm( AstWcsMap *this, int *status ) {
/*
*  Name:
*     InitPrjPrm

*  Purpose:
*     Initialise the WcsLib PrjPrm structure, assigning default values for
*     missing parameters.

*  Type:
*     Private function.

*  Synopsis:
*     void InitPrjPrm( AstWcsMap *this, int *status )

*  Description:
*     This function initializes the projection parameter information
*     stored within the WcsLib AstPrjPrm structure associated with the
*     supplied WcsMap. Default values are assigned to any unspecified
*     parameter values. AST__BAD values are assigned if any parameters
*     have not been supplied for which there is no default.

*  Parameters:
*     this
*        The WcsMap.
*     status
*        Pointer to the inherited status variable.

*/


/* Local Variables: */
   struct AstPrjPrm *params;     /* The AstPrjPrm structure from the WcsMap */
   int i;                        /* Loop index */
   int latax;                    /* Index of latitude axis */
   int lonax;                    /* Index of longitude axis */
   int npar;                     /* No. of parameters supplied */
   int plen;                     /* Length of params array */
   int plen2;                    /* Length of latitude params array */
   int type;                     /* Projection type */

/* Check the global error status. */
   if ( !astOK ) return;

/* Get a pointer to the AstPrjPrm structure*/
   params = &(this->params);

/* Tell the routines within the WcsLib "proj.c" module  to re-calculate
   intermediate values. */
   params->flag = 0;
   params->r0 = 0;

/* If this is a TPN projection, indicate whether or not the
   transformation should include the TAN projection or just the
   polynomial transformation. */
   if( this->type == AST__TPN ) params->n = astGetTPNTan( this );

/* Find the max number of projection parameters associated with each
   axis.*/
   plen2 = astSizeOf( params->p2 )/sizeof( double );
   plen = astSizeOf( params->p )/sizeof( double );

/* Initially set all parameter to AST__BAD. */
   for( i = 0; i < plen; i++ ) (params->p)[i] = AST__BAD;
   for( i = 0; i < plen2; i++ ) (params->p2)[i] = AST__BAD;

/* If the WcsMap contains any projection parameter values... */
   if( this->np && this->p ){

/* Get the index of the latitude axis. Currently, all projection
   parameters are associated with the latitude axis (except for
   the TPN projection, which is a hang-over from a earlier draft of the
   FITS-WCS paper). */
      latax = astGetWcsAxis( this, 1 );

/* Find the number of projection parameters in the WcsMap for the
   latitude axis. */
      npar = (this->np)[ latax ];
      if( npar > plen ) {
         astError( AST__INTER, "InitPrjPrm(WcsMap): Too many projection "
                   "parameters on the latitude axis (%d > %d) (internal "
                   "AST programming error).", status, npar, plen );
      }

/* Copy the parameters to the AstPrjPrm structure. Do not copy more than
   can be stored in the AstPrjPrm structure. */
      for( i = 0; i < npar && i < plen; i++ ) {
         (params->p)[ i ] = (this->p)[ latax ][ i ];
      }

/* Do the same for the longitude axis (for the benefit of the TPN projection). */
      lonax = astGetWcsAxis( this, 0 );
      npar = (this->np)[ lonax ];

      if( npar > plen2 ) {
         astError( AST__INTER, "InitPrjPrm(WcsMap): Too many projection "
                   "parameters on the longitude axis (%d > %d) (internal "
                   "AST programming error).", status, npar, plen2 );
      }

      for( i = 0; i < npar && i < plen2; i++ ) {
         (params->p2)[ i ] = (this->p)[ lonax ][ i ];
      }

   }

/* Get the projection type. */
   type = astGetWcsType( this );

/* First supply default values for any missing projection parameters which
   do not default to zero. */
   if( type == AST__SZP ){
      if( (params->p)[ 3 ] == AST__BAD ) (params->p)[ 3 ] = 90.0;

   } else if( type == AST__AIR ){
      if( (params->p)[ 1 ] == AST__BAD ) (params->p)[ 1 ] = 90.0;

   } else if( type == AST__CYP ){
      if( (params->p)[ 1 ] == AST__BAD ) (params->p)[ 1 ] = 1.0;
      if( (params->p)[ 2 ] == AST__BAD ) (params->p)[ 2 ] = 1.0;

   } else if( type == AST__CEA ){
      if( (params->p)[ 1 ] == AST__BAD ) (params->p)[ 1 ] = 1.0;

   } else if( type == AST__TPN ){
      if( (params->p)[ 1 ] == AST__BAD ) (params->p)[ 1 ] = 1.0;
      if( (params->p2)[ 1 ] == AST__BAD ) (params->p2)[ 1 ] = 1.0;

   } else if( type == AST__HPX ){
      if( (params->p)[ 1 ] == AST__BAD ) (params->p)[ 1 ] = 4.0;
      if( (params->p)[ 2 ] == AST__BAD ) (params->p)[ 2 ] = 3.0;

   }

/* Now use a default value of zero for any remaining unspecified values,
   except for un-defaultable projection parameters. */
   for( i = 0; i < plen; i++ ){

/* Retain any AST__BAD value for these undefaultable parameters. */
      if( i == 1 && ( type == AST__BON ||
                     type == AST__COP || type == AST__COE ||
                     type == AST__COD || type == AST__COO ) ){

/* Use a default of zero for all other parameters. */
      } else {
         if( (params->p)[ i ] == AST__BAD ) (params->p)[ i ] = 0.0;
      }
   }

/* Do the same for the latitude projection parameters (if any) */
   for( i = 0; i < plen2; i++ ){
      if( i == 1 && ( type == AST__BON ||
                      type == AST__COP || type == AST__COE ||
                      type == AST__COD || type == AST__COO ) ){
      } else {
         if( (params->p2)[ i ] == AST__BAD ) (params->p2)[ i ] = 0.0;
      }
   }
}

void astInitWcsMapVtab_(  AstWcsMapVtab *vtab, const char *name, int *status ) {
/*
*+
*  Name:
*     astInitWcsMapVtab

*  Purpose:
*     Initialise a virtual function table for a WcsMap.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "wcsmap.h"
*     void astInitWcsMapVtab( AstWcsMapVtab *vtab, const char *name )

*  Class Membership:
*     WcsMap vtab initialiser.

*  Description:
*     This function initialises the component of a virtual function
*     table which is used by the WcsMap class.

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
   will be used (by astIsAWcsMap) to determine if an object belongs
   to this class.  We can conveniently use the address of the (static)
   class_check variable to generate this unique value. */
   vtab->id.check = &class_check;
   vtab->id.parent = &(((AstMappingVtab *) vtab)->id);

/* Initialise member function pointers. */
/* ------------------------------------ */
/* Store pointers to the member functions (implemented here) that provide
   virtual methods for this class. */
   vtab->ClearPV = ClearPV;
   vtab->GetNatLat = GetNatLat;
   vtab->GetNatLon = GetNatLon;
   vtab->GetPV = GetPV;
   vtab->GetWcsAxis = GetWcsAxis;
   vtab->GetPVMax = GetPVMax;
   vtab->GetWcsType = GetWcsType;
   vtab->SetPV = SetPV;
   vtab->TestPV = TestPV;
   vtab->IsZenithal = IsZenithal;

   vtab->ClearFITSProj = ClearFITSProj;
   vtab->TestFITSProj = TestFITSProj;
   vtab->GetFITSProj = GetFITSProj;
   vtab->SetFITSProj = SetFITSProj;

   vtab->ClearTPNTan = ClearTPNTan;
   vtab->TestTPNTan = TestTPNTan;
   vtab->GetTPNTan = GetTPNTan;
   vtab->SetTPNTan = SetTPNTan;

/* Save the inherited pointers to methods that will be extended, and
   replace them with pointers to the new member functions. */
   object = (AstObjectVtab *) vtab;
   mapping = (AstMappingVtab *) vtab;
   parent_getobjsize = object->GetObjSize;
   object->GetObjSize = GetObjSize;

   parent_clearattrib = object->ClearAttrib;
   object->ClearAttrib = ClearAttrib;
   parent_getattrib = object->GetAttrib;
   object->GetAttrib = GetAttrib;
   parent_setattrib = object->SetAttrib;
   object->SetAttrib = SetAttrib;
   parent_testattrib = object->TestAttrib;
   object->TestAttrib = TestAttrib;

   parent_transform = mapping->Transform;
   mapping->Transform = Transform;

   parent_mapsplit = mapping->MapSplit;
   mapping->MapSplit = MapSplit;

/* Store replacement pointers for methods which will be over-ridden by
   new member functions implemented here. */
   object->Equal = Equal;
   mapping->MapMerge = MapMerge;

/* Declare the destructor and copy constructor. */
   astSetDelete( (AstObjectVtab *) vtab, Delete );
   astSetCopy( (AstObjectVtab *) vtab, Copy );

/* Declare the class dump function. */
   astSetDump( vtab, Dump, "WcsMap", "FITS-WCS sky projection" );

/* If we have just initialised the vtab for the current class, indicate
   that the vtab is now initialised, and store a pointer to the class
   identifier in the base "object" level of the vtab. */
   if( vtab == &class_vtab ) {
      class_init = 1;
      astSetVtabClassIdentifier( vtab, &(vtab->id) );
   }
}

static int IsZenithal( AstWcsMap *this, int *status ){
/*
*+
*  Name:
*     IsZenithal

*  Purpose:
*     Determine if this WcsMap represents a zenithal projection.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "wcsmap.h"
*     int IsZenithal( AstWcsMap *this, int *status )

*  Class Membership:
*     WcsMap protected function

*  Description:
*     This function returns a flag indicating if this WcsMap is a zenithal
*     projection. Some projections which are classed as zenithal in the
*     Calabretta and Greisen paper are only genuinely zenithal if the
*     projection parameters have certain values. These projections are
*     not considered to be zenithal unless the projection parameters have
*     appropriate values.

*  Parameters:
*     this
*        The WcsMap.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     A non-zero value if the WcsMap represents a zenithal projection.

*-
*/

/* Local Variables: */
   double p1;                    /* PVi_1 */
   double p2;                    /* PVi_2 */
   double p3;                    /* PVi_3 */
   int latax;                    /* Index of latitude axis */
   int ret;                      /* Returned flag */
   int type;                     /* Projection type */

/* Initialise the returned value. */
   ret = 0;

/* Check the global error status. */
   if ( !astOK ) return ret;

/* Get the projection type. */
   type = astGetWcsType( this );

/* Get the index of the latitude axis. */
   latax = astGetWcsAxis( this, 1 );

/* The following are always zenithal... */
   if( type == AST__TAN  || type == AST__STG  || type == AST__ARC  ||
       type == AST__ZPN  || type == AST__ZEA  || type == AST__AIR ||
       type == AST__TPN ) {
      ret = 1;

/* The following are sometimes zenithal... */
   } else if( type == AST__AZP ) {
      p2 = astGetPV( this, latax, 2 );
      if( p2 == AST__BAD || p2 == 0.0 ) ret = 1;

   } else if( type == AST__SIN ) {
      p1 = astGetPV( this, latax, 1 );
      p2 = astGetPV( this, latax, 2 );
      if( p1 == AST__BAD ) p1 = 0.0;
      if( p2 == AST__BAD ) p2 = 0.0;
      if( p1 == 0.0 && p2 == 0.0 ) ret = 1;

   } else if( type == AST__SZP ) {
      p3 = astGetPV( this, latax, 2 );
      if( p3 == AST__BAD ) p3 = 90.0;
      if( p3 == 90.0 || p3 == -90.0 ) ret = 1;

   }

   return ret;
}

static int LongRange( const PrjData *prjdata, struct AstPrjPrm *params,
                       double *high, double *low, int *status ){
/*
*
*  Name:
*     LongRange

*  Purpose:
*     See if primary range of longitude produced by a WCSLIB mapping is
*     [0,360] or [-180,+180].

*  Type:
*     Private function.

*  Synopsis:
*     #include "wcsmap.h"
*     void LongRange( const PrjData *prjdata, struct AstPrjPrm *params,
*                     double *high, double *low, int *status )

*  Class Membership:
*     WcsMap internal utility function.

*  Description:
*     This function uses the WCSLIB library to transform the supplied input
*     positions.

*  Parameters:
*     prjdata
*        A pointer to information about the mapping.
*     params
*        Pointer to a WCSLIB "AstPrjPrm" structure containing the projection
*        parameters, etc.
*     high
*        A pointer to a location at which is returned the upper bound of
*        the primary longitude range.
*     low
*        A pointer to a location at which is returned the lower bound of
*        the primary longitude range.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     A flag indicating if the sky->xy transformation is cyclic (i.e.
*     [a,0] gets mapped to the same (x,y) position as [a+2.PI,0]).
*/


/* Local Variables: */
   int point;                    /* Loop counter for points */
   static double xx[ 4 ] = { -1.0E-6,    0.0, 1.0E-6,     0.0 };
   static double yy[ 4 ] = {     0.0, 1.0E-6,    0.0, -1.0E-6 };
   double aa;
   double bb;
   double xxx[ 2 ];
   double yyy[ 2 ];
   int cyclic;

/* Initialise the returned values. */
   *high = 180.0;
   *low = -180.0;

/* Check the global error status. */
   if ( !astOK ) return 0;

/* Project each of the points. If any longitude value is found which is
   greater than 180 degrees, return [0,360] as the longitude range. */
   for( point = 0; point < 4; point++ ){
      if( !prjdata->WcsRev( xx[ point ], yy[ point ], params, &aa, &bb ) ){
         if( aa > 180.0 ){
            *high = 360.0;
            *low = 0.0;
            break;
         }
      }
   }

/* See if the projection is cyclic. Transform the sky positions [90,bb] and
   [450,bb] into cartesian positions and see if they are the same. */
   prjdata->WcsFwd( 90.0, bb, params, xxx, yyy );
   prjdata->WcsFwd( 450.0, bb, params, xxx + 1, yyy + 1 );
   cyclic = ( fabs( xxx[ 0 ] - xxx[ 1 ] ) < 1.0E-10 &&
              fabs( yyy[ 0 ] - yyy[ 1 ] ) < 1.0E-10 );

/* Return. */
   return cyclic;

}

static int Map( AstWcsMap *this, int forward, int npoint, double *in0,
                double *in1, double *out0, double *out1, int *status ){
/*
*
*  Name:
*     Map

*  Purpose:
*     Transform a set of points using a function from the WCSLIB library.

*  Type:
*     Private function.

*  Synopsis:
*     #include "wcsmap.h"
*     int Map( AstWcsMap *this, int forward, int npoint, double *in0,
*              double *in1, double *out0, double *out1 )

*  Class Membership:
*     WcsMap internal utility function.

*  Description:
*     This function uses the WCSLIB library to transform the supplied input
*     positions.

*  Parameters:
*     this
*        Pointer to the WcsMap.
*     forward
*        A non-zero value indicates that the forward projection from
*        (long,lat) to (x,y) is required, while a zero value requests the
*        reverse transformation.
*     npoint
*        The number of points to transform (i.e. the size of the
*        in0, in1, out0 and out1 arrays).
*     in0
*        A pointer to the input coordinate data for the 0th axis (i.e.
*        longitude or X depending on "forward").
*     in1
*        A pointer to the input coordinate data for the 1st axis (i.e.
*        latitude or Y depending on "forward").
*     out0
*        A pointer to the returned output coordinate data for the 0th axis
*        (i.e. X or longitude depending on "forward").
*     out1
*        A pointer to the returned output coordinate data for the 1st axis
*        (i.e. Y or latitude depending on "forward").

*  Returned Value:
*     The status value: 0 - Success
*                       1 - Unrecognised projection type
*                       2 - Invalid projection parameters values.
*                       4 - Error existed on entry
*                       100 - 399: Longitude axis projection parameter
*                           (status-100) not supplied.
*                       400 - 699: Latitude axis projection parameter
*                           (status-400) not supplied.


*  Notes:
*     -  This function does not report any errors. Reporting of suitable
*     error messages is the responsibility of the calling function.
*     -  The value 4 will be returned if this function is invoked with the
*     global error status set.
*
*/

/* Local Variables: */
   const PrjData *prjdata;       /* Information about the projection */
   double factor;                /* Factor that scales input into radians. */
   double latitude;              /* Latitude value in degrees */
   double longhi;                /* Upper longitude limit in degrees */
   double longitude;             /* Longitude value in degrees */
   double longlo;                /* Lower longitude limit in degrees */
   double x;                     /* X Cartesian coordinate in degrees */
   double y;                     /* Y Cartesian coordinate in degrees */
   int cyclic;                   /* Is sky->xy transformation cyclic? */
   int i;                        /* Loop count */
   int plen;                     /* Length of proj par array */
   int point;                    /* Loop counter for points */
   int type;                     /* Projection type */
   int wcs_status;               /* Status from WCSLIB functions */
   struct AstPrjPrm *params;     /* Pointer to structure holding WCSLIB info */

/* Check the global error status. */
   if ( !astOK ) return 4;

/* Initialise variables to avoid compiler warnings. */
   longlo = AST__BAD;
   longhi = AST__BAD;

/* Store the projection type. */
   type = astGetWcsType( this );

/* Get information about the projection. */
   prjdata = FindPrjData( type, status );

/* Return if there are no WcsLib mapping functons associated with the
   projection. */
   if( ( !prjdata->WcsFwd && forward ) ||
       ( !prjdata->WcsRev && !forward ) ) return 1;

/* Check that all necessary projection parameters have been supplied, or
   can be defaulted. */
   params = &(this->params);
   plen = astSizeOf( params->p )/sizeof( double );
   for( i = 0; i < plen; i++ ) {
      if( ( params->p)[ i ] == AST__BAD ) return 400+i;
   }

/* If we are doing a reverse mapping, get the acceptable range of longitude
   values. */
   cyclic = forward ? 0 : LongRange( prjdata, params, &longhi, &longlo,
                                     status );

/* The WcsMap input and output values are normally in radians, but if
   the TPNTan attribute has been reset then they are in degrees. The
   WCSLIB projection functions always expect and return degrees. Get
   the factor that scales the WcsMap input into radians. */
   factor = astGetTPNTan( this ) ? 1.0 : AST__DD2R;

/* Loop to apply the projection to each point in turn, checking for
   (and propagating) bad values in the process. */
   for ( point = 0; point < npoint; point++ ) {
       if ( in0[ point ] == AST__BAD ||
           in1[ point ] == AST__BAD ){
          out0[ point ] = AST__BAD;
          out1[ point ] = AST__BAD;
       } else {

/* First deal with forward projection calls */
         if ( forward ){

/* The input coordinates are assumed to be longitude and latitude, in
   radians or degrees (as specified by the TPNTan attribute). Convert them
   to degrees ensuring that the longitude value is in the range [-180,180]
   and the latitude is in the range [-90,90] (as required by the WCSLIB
   library). Any point with a latitude outside the range [-90,90] is
   converted to the equivalent point on the complementary meridian. */
            latitude = AST__DR2D*palDrange(  factor*in1[ point ] );
            if ( latitude > 90.0 ){
               latitude = 180.0 - latitude;
               longitude = AST__DR2D*palDrange( AST__DPI + factor*in0[ point ] );

            } else if ( latitude < -90.0 ){
               latitude = -180.0 - latitude;
               longitude = AST__DR2D*palDrange( AST__DPI + factor*in0[ point ] );

            } else {
               longitude = AST__DR2D*palDrange( factor*in0[ point ] );
            }

/* Call the relevant WCSLIB forward projection function. */
            wcs_status = prjdata->WcsFwd( longitude, latitude, params, &x, &y );

/* Store the returned Cartesian coordinates, converting them from degrees
   to radians. If the position could not be projected, use the value
   AST__BAD.  Abort for any other bad status. */
            if( wcs_status == 0 ){
               out0[ point ] = (AST__DD2R/factor)*x;
               out1[ point ] = (AST__DD2R/factor)*y;

            } else if( wcs_status == 1 ){
               return 2;

            } else if( wcs_status == 2 ){
               out0[ point ] = AST__BAD;
               out1[ point ] = AST__BAD;

            } else {
               return wcs_status;
            }

/* Now deal with reverse projection calls */
         } else {

/* Convert the supplied Cartesian coordinates from radians to degrees. */
            x = (AST__DR2D*factor)*in0[ point ];
            y = (AST__DR2D*factor)*in1[ point ];

/* Call the relevant WCSLIB reverse projection function. */
            wcs_status = prjdata->WcsRev( x, y, params, &longitude, &latitude );

/* Store the returned longitude and latitude, converting them from degrees
   to radians. Many projections (ARC, AIT, ZPN, etc) are not cyclic (i.e.
   [long,lat]=[0,0] does not get mapped to the same place as
   [long,lat]=[360,0] ). Only accept values in the primary longitude or
   latitude ranges. This avoids (x,y) points outside the physical domain
   of the mapping being assigned valid (long,lat) values. */
            if( wcs_status == 0 ){
               if( ( cyclic || ( longitude < longhi &&
                                 longitude >= longlo ) ) &&
                   fabs( latitude ) <= 90.0 ){

                  out0[ point ] = (AST__DD2R/factor)*longitude;
                  out1[ point ] = (AST__DD2R/factor)*latitude;

               } else {
                  out0[ point ] = AST__BAD;
                  out1[ point ] = AST__BAD;
               }

/* Abort if projection parameters were unusable. */
            } else if( wcs_status == 1 ){
               return 2;

/* If the position could not be projected, use the value AST__BAD. */
            } else if( wcs_status == 2 ){
               out0[ point ] = AST__BAD;
               out1[ point ] = AST__BAD;

/* Abort if projection parameters were not supplied. */
            } else {
               return wcs_status;
            }

         }

      }

   }

   return 0;
}

static int MapMerge( AstMapping *this, int where, int series, int *nmap,
                     AstMapping ***map_list, int **invert_list, int *status ) {
/*
*  Name:
*     MapMerge

*  Purpose:
*     Simplify a sequence of Mappings containing a WcsMap.

*  Type:
*     Private function.

*  Synopsis:
*     #include "wcsmap.h"
*     int MapMerge( AstMapping *this, int where, int series, int *nmap,
*                   AstMapping ***map_list, int **invert_list, int *status )

*  Class Membership:
*     WcsMap method (over-rides the protected astMapMerge method
*     inherited from the Mapping class).

*  Description:
*     This function attempts to simplify a sequence of Mappings by
*     merging a nominated WcsMap in the sequence with its neighbours,
*     so as to shorten the sequence if possible.
*
*     In many cases, simplification will not be possible and the
*     function will return -1 to indicate this, without further
*     action.
*
*     In most cases of interest, however, this function will either
*     attempt to replace the nominated WcsMap with a Mapping which it
*     considers simpler, or to merge it with the Mappings which
*     immediately precede it or follow it in the sequence (both will
*     normally be considered). This is sufficient to ensure the
*     eventual simplification of most Mapping sequences by repeated
*     application of this function.
*
*     In some cases, the function may attempt more elaborate
*     simplification, involving any number of other Mappings in the
*     sequence. It is not restricted in the type or scope of
*     simplification it may perform, but will normally only attempt
*     elaborate simplification in cases where a more straightforward
*     approach is not adequate.

*  Parameters:
*     this
*        Pointer to the nominated WcsMap which is to be merged with
*        its neighbours. This should be a cloned copy of the WcsMap
*        pointer contained in the array element "(*map_list)[where]"
*        (see below). This pointer will not be annulled, and the
*        WcsMap it identifies will not be modified by this function.
*     where
*        Index in the "*map_list" array (below) at which the pointer
*        to the nominated WcsMap resides.
*     series
*        A non-zero value indicates that the sequence of Mappings to
*        be simplified will be applied in series (i.e. one after the
*        other), whereas a zero value indicates that they will be
*        applied in parallel (i.e. on successive sub-sets of the
*        input/output coordinates).
*     nmap
*        Address of an int which counts the number of Mappings in the
*        sequence. On entry this should be set to the initial number
*        of Mappings. On exit it will be updated to record the number
*        of Mappings remaining after simplification.
*     map_list
*        Address of a pointer to a dynamically allocated array of
*        Mapping pointers (produced, for example, by the astMapList
*        method) which identifies the sequence of Mappings. On entry,
*        the initial sequence of Mappings to be simplified should be
*        supplied.
*
*        On exit, the contents of this array will be modified to
*        reflect any simplification carried out. Any form of
*        simplification may be performed. This may involve any of: (a)
*        removing Mappings by annulling any of the pointers supplied,
*        (b) replacing them with pointers to new Mappings, (c)
*        inserting additional Mappings and (d) changing their order.
*
*        The intention is to reduce the number of Mappings in the
*        sequence, if possible, and any reduction will be reflected in
*        the value of "*nmap" returned. However, simplifications which
*        do not reduce the length of the sequence (but improve its
*        execution time, for example) may also be performed, and the
*        sequence might conceivably increase in length (but normally
*        only in order to split up a Mapping into pieces that can be
*        more easily merged with their neighbours on subsequent
*        invocations of this function).
*
*        If Mappings are removed from the sequence, any gaps that
*        remain will be closed up, by moving subsequent Mapping
*        pointers along in the array, so that vacated elements occur
*        at the end. If the sequence increases in length, the array
*        will be extended (and its pointer updated) if necessary to
*        accommodate any new elements.
*
*        Note that any (or all) of the Mapping pointers supplied in
*        this array may be annulled by this function, but the Mappings
*        to which they refer are not modified in any way (although
*        they may, of course, be deleted if the annulled pointer is
*        the final one).
*     invert_list
*        Address of a pointer to a dynamically allocated array which,
*        on entry, should contain values to be assigned to the Invert
*        attributes of the Mappings identified in the "*map_list"
*        array before they are applied (this array might have been
*        produced, for example, by the astMapList method). These
*        values will be used by this function instead of the actual
*        Invert attributes of the Mappings supplied, which are
*        ignored.
*
*        On exit, the contents of this array will be updated to
*        correspond with the possibly modified contents of the
*        "*map_list" array.  If the Mapping sequence increases in
*        length, the "*invert_list" array will be extended (and its
*        pointer updated) if necessary to accommodate any new
*        elements.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     If simplification was possible, the function returns the index
*     in the "map_list" array of the first element which was
*     modified. Otherwise, it returns -1 (and makes no changes to the
*     arrays supplied).

*  Notes:
*     - A value of -1 will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*/

/* Local Variables: */
   AstMapping *mc[2];    /* Copies of supplied Mappings to swap */
   AstMapping *smc0;     /* Simplied Mapping */
   AstMapping *smc1;     /* Simplied Mapping */
   const char *nclass;   /* Pointer to neighbouring Mapping class */
   const char *class1;   /* Pointer to first Mapping class string */
   const char *class2;   /* Pointer to second Mapping class string */
   int do1;              /* Would a backward swap make a simplification? */
   int do2;              /* Would a forward swap make a simplification? */
   int i1;               /* Lower index of the two WcsMaps being merged */
   int i2;               /* Upper index of the two WcsMaps being merged */
   int i;                /* Mapping index */
   int ic[2];            /* Copies of supplied invert flags to swap */
   int merge;            /* Can WcsMap merge with a neighbour? */
   int nin;              /* Number of coordinates for WcsMap */
   int nstep1;           /* No. of Mappings backwards to next mergable Mapping */
   int nstep2;           /* No. of Mappings forward to next mergable Mapping */
   int result;           /* Result value to return */
   int swaphi;           /* Can WcsMap be swapped with higher neighbour? */
   int swaplo;           /* Can WcsMap be swapped with lower neighbour? */
   int type;             /* Projection type */

/* Initialise. */
   result = -1;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Initialise variables to avoid "used of uninitialised variable"
   messages from dumb compilers. */
   i1 = 0;
   i2 = 0;

/* Get the number of axes for the WcsMap. */
   nin = astGetNin( ( *map_list )[ where ] );

/* First of all, see if the WcsMap can be replaced by a simpler Mapping,
   without reference to the neighbouring Mappings in the list.           */
/* ======================================================================*/
/* WcsMaps with map type of AST__WCSBAD are equivalent to a UnitMap. */
   type = astGetWcsType( this );
   if( type == AST__WCSBAD ){

/* Annul the WcsMap pointer in the list and replace it with a UnitMap
   pointer, and indicate that the forward transformation of the returned
   UnitMap should be used. */
      (void) astAnnul( ( *map_list )[ where ] );
      ( *map_list )[ where ] = (AstMapping *) astUnitMap( nin, "", status );
      ( *invert_list )[ where ] = 0;

/* Return the index of the first modified element. */
      result = where;

/* If the WcsMap itself could not be simplified, see if it can be merged
   with the Mappings on either side of it in the list. This can only be
   done in series for a WcsMap. */
/* ===================================================================== */
   } else if( series && *nmap > 1 ) {

/* Store the classes of the neighbouring Mappings in the list. */
      class1 = ( where > 0 ) ? astGetClass( ( *map_list )[ where - 1 ] ) : NULL;
      class2 = ( where < *nmap - 1 ) ? astGetClass( ( *map_list )[ where + 1 ] ) : NULL;

/* A WcsMap can only combine with its own inverse. Set a flag indicating
   that we have not yet found a neighbour with which the WcsMap can be
   merged. */
      merge = 0;

/* First check the lower neighbour (if any). */
      if( where > 0 ) {
         i1 = where - 1;
         i2 = where;
         merge = CanMerge( ( *map_list )[ i1 ], (* invert_list)[ i1 ],
                           ( *map_list )[ i2 ], (* invert_list)[ i2 ], status );
      }

/* If the WcsMap can not be merged with its lower neighbour, check its
   upper neighbour (if any) in the same way. */
      if( !merge && where < *nmap - 1 ) {
         i1 = where;
         i2 = where + 1;
         merge = CanMerge( ( *map_list )[ i1 ], (* invert_list)[ i1 ],
                           ( *map_list )[ i2 ], (* invert_list)[ i2 ], status );
      }

/* If either neighbour has passed these checks, it is the inverse of the
   WcsMap being checked. The pair of WcsMaps can be replaced by a single
   UnitMap. */
      if( merge ) {

/* Annul the two WcsMaps. */
         (void) astAnnul( ( *map_list )[ i1 ] );
         (void) astAnnul( ( *map_list )[ i2 ] );

/* Create a UnitMap, and store a pointer for it in place of the first
   WcsMap. */
         ( *map_list )[ i1 ] = (AstMapping *) astUnitMap( nin, "", status );
         ( *invert_list )[ i1 ] = 0;

/* Shuffle down the remaining Mappings to fill the hole left by the
   second WcsMap. */
         for ( i = i2 + 1; i < *nmap; i++ ) {
            ( *map_list )[ i - 1 ] = ( *map_list )[ i ];
            ( *invert_list )[ i - 1 ] = ( *invert_list )[ i ];
         }

/* Clear the vacated element at the end. */
         ( *map_list )[ *nmap - 1 ] = NULL;
         ( *invert_list )[ *nmap - 1 ] = 0;

/* Decrement the Mapping count and return the index of the first
   modified element. */
         (*nmap)--;
         result = i1;

/* If the WcsMap could not merge directly with either of its neighbours,
   we consider whether it would be worthwhile to swap the WcsMap with
   either of its neighbours. This can only be done for certain PermMaps,
   and will usually require both Mappings to be modified (unless they are
   commutative). The advantage of swapping the order of the Mappings is that
   it may result in the WcsMap being adjacent to a Mapping with which it can
   merge directly on the next invocation of this function, thus reducing the
   number of Mappings in the list. */
      } else {

/* Set a flag if we could swap the WcsMap with its higher neighbour. "do2"
   is returned if swapping the Mappings would simplify either of the
   Mappings. */
         if( where + 1 < *nmap ){
            swaphi = CanSwap(  ( *map_list )[ where ],
                               ( *map_list )[ where + 1 ],
                               ( *invert_list )[ where ],
                               ( *invert_list )[ where + 1 ], &do2, status );
         } else {
            do2 = 0;
            swaphi = 0;
         }

/* If so, step through each of the Mappings which follow the WcsMap,
   looking for a Mapping with which the WcsMap could merge directly. Stop
   when such a Mapping is found, or if a Mapping is found with which the
   WcsMap could definitely not swap. Note the number of Mappings which
   separate the WcsMap from the Mapping with which it could merge (if
   any). */
         nstep2 = -1;
         if( swaphi ){
            for( i2 = where + 1; i2 < *nmap; i2++ ){

/* See if we can merge with this Mapping. If so, note the number of steps
   between the two Mappings and leave the loop. */
               if( CanMerge( ( *map_list )[ i2 ], ( *invert_list )[ i2 ],
                             ( *map_list )[ where ], ( *invert_list )[ where ], status ) ) {
                  nstep2 = i2 - where - 1;
                  break;
               }

/* If there is no chance that we can swap with this Mapping, leave the loop
   with -1 for the number of steps to indicate that no merging is possible.
   WcsMaps can swap only with some PermMaps. */
               nclass = astGetClass( ( *map_list )[ i2 ] );
               if( strcmp( nclass, "PermMap" ) ) {
                  break;
               }

            }

         }

/* Do the same working forward from the WcsMap towards the start of the map
   list. */
         if( where > 0 ){
            swaplo = CanSwap(  ( *map_list )[ where - 1 ],
                               ( *map_list )[ where ],
                               ( *invert_list )[ where - 1 ],
                               ( *invert_list )[ where ], &do1, status );
         } else {
            do1 = 0;
            swaplo = 0;
         }

         nstep1 = -1;
         if( swaplo ){
            for( i1 = where - 1; i1 >= 0; i1-- ){

               if( CanMerge( ( *map_list )[ i1 ], ( *invert_list )[ i1 ],
                             ( *map_list )[ where ], ( *invert_list )[ where ], status ) ) {
                  nstep1 = where - 1 - i1;
                  break;
               }

               nclass = astGetClass( ( *map_list )[ i1 ] );
               if( strcmp( nclass, "PermMap" ) ) {
                  break;
               }

            }

         }

/* Choose which neighbour to swap with so that the WcsMap moves towards the
   nearest Mapping with which it can merge. */
         if( do1 || (
             nstep1 != -1 && ( nstep2 == -1 || nstep2 > nstep1 ) ) ){
            nclass = class1;
            i1 = where - 1;
            i2 = where;
         } else if( do2 || nstep2 != -1 ){
            nclass = class2;
            i1 = where;
            i2 = where + 1;
         } else {
            nclass = NULL;
         }

/* If there is a target Mapping in the list with which the WcsMap could
   merge, replace the supplied Mappings with swapped Mappings to bring a
   WcsMap closer to the target Mapping. */
         if( nclass ){

            WcsPerm( (*map_list) + i1, (*invert_list) + i1, where - i1, status );

/* Store the index of the first modified Mapping. */
            result = i1;

/* If there is no Mapping available for merging, it may still be
   advantageous to swap with a neighbour because the swapped Mapping may
   be simpler than the original Mappings. */
         } else if( swaphi || swaplo ) {

/*  Choose a neightbour to swap with. If both are suitable for swapping,
    swap with the lower. */
            if( swaplo ){
               nclass = class1;
               i1 = where - 1;
               i2 = where;
            } else {
               nclass = class2;
               i1 = where;
               i2 = where + 1;
            }

/* Take copies of the Mapping and Invert flag arrays so we do not change
   the supplied values. */
            mc[ 0 ] = (AstMapping *) astCopy( ( (*map_list) + i1 )[0] );
            mc[ 1 ] = (AstMapping *) astCopy( ( (*map_list) + i1 )[1] );
            ic[ 0 ] = ( (*invert_list) + i1 )[0];
            ic[ 1 ] = ( (*invert_list) + i1 )[1];

/* Swap these Mappings. */
            WcsPerm( mc, ic, where - i1, status );

/* If neither of the swapped Mappings can be simplified further, then there
   is no point in swapping the Mappings, so just annul the map copies. */
            smc0 = astSimplify( mc[0] );
            smc1 = astSimplify( mc[1] );

            if( astGetClass( smc0 ) == astGetClass( mc[0] ) &&
                astGetClass( smc1 ) == astGetClass( mc[1] ) ) {

               mc[ 0 ] = (AstMapping *) astAnnul( mc[ 0 ] );
               mc[ 1 ] = (AstMapping *) astAnnul( mc[ 1 ] );

/* If one or both of the swapped Mappings could be simplified, then annul
   the supplied Mappings and return the swapped mappings, storing the index
   of the first modified Mapping. */
            } else {
               (void ) astAnnul( ( (*map_list) + i1 )[0] );
               (void ) astAnnul( ( (*map_list) + i1 )[1] );

               ( (*map_list) + i1 )[0] = mc[ 0 ];
               ( (*map_list) + i1 )[1] = mc[ 1 ];

               ( (*invert_list) + i1 )[0] = ic[ 0 ];
               ( (*invert_list) + i1 )[1] = ic[ 1 ];

               result = i1;

            }

/* Annul the simplied Mappings */
            smc0 = astAnnul( smc0 );
            smc1 = astAnnul( smc1 );

         }
      }
   }

/* Return the result. */
   return result;
}

static int *MapSplit( AstMapping *this_map, int nin, const int *in, AstMapping **map, int *status ){
/*
*  Name:
*     MapSplit

*  Purpose:
*     Create a Mapping representing a subset of the inputs of an existing
*     WcsMap.

*  Type:
*     Private function.

*  Synopsis:
*     #include "wcsmap.h"
*     int *MapSplit( AstMapping *this, int nin, const int *in, AstMapping **map, int *status )

*  Class Membership:
*     WcsMap method (over-rides the protected astMapSplit method
*     inherited from the Mapping class).

*  Description:
*     This function creates a new Mapping by picking specified inputs from
*     an existing WcsMap. This is only possible if the specified inputs
*     correspond to some subset of the WcsMap outputs. That is, there
*     must exist a subset of the WcsMap outputs for which each output
*     depends only on the selected WcsMap inputs, and not on any of the
*     inputs which have not been selected. If this condition is not met
*     by the supplied WcsMap, then a NULL Mapping is returned.

*  Parameters:
*     this
*        Pointer to the WcsMap to be split (the WcsMap is not actually
*        modified by this function).
*     nin
*        The number of inputs to pick from "this".
*     in
*        Pointer to an array of indices (zero based) for the inputs which
*        are to be picked. This array should have "nin" elements. If "Nin"
*        is the number of inputs of the supplied WcsMap, then each element
*        should have a value in the range zero to Nin-1.
*     map
*        Address of a location at which to return a pointer to the new
*        Mapping. This Mapping will have "nin" inputs (the number of
*        outputs may be different to "nin"). A NULL pointer will be
*        returned if the supplied WcsMap has no subset of outputs which
*        depend only on the selected inputs.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     A pointer to a dynamically allocated array of ints. The number of
*     elements in this array will equal the number of outputs for the
*     returned Mapping. Each element will hold the index of the
*     corresponding output in the supplied WcsMap. The array should be
*     freed using astFree when no longer needed. A NULL pointer will
*     be returned if no output Mapping can be created.

*  Notes:
*     - If this function is invoked with the global error status set,
*     or if it should fail for any reason, then NULL values will be
*     returned as the function value and for the "map" pointer.
*/

/* Local Variables: */
   AstWcsMap *newwcs;         /* Pointer to returned WcsMap */
   AstWcsMap *this;           /* Pointer to WcsMap structure */
   int *result;               /* Pointer to returned array */
   int *inperm;               /* Input axis permutation array */
   int *outperm;              /* Output axis permutation array */
   int i;                     /* Loop count */
   int iin;                   /* Mapping input index */
   int ilat;                  /* Index of latitude axis in new WcsMap */
   int ilatlon;               /* Index of last lat or lon axis */
   int ilon;                  /* Index of longitude axis in new WcsMap */
   int latax;                 /* Index of latitude axis in supplied WcsMap */
   int lonax;                 /* Index of longitude axis in supplied WcsMap */
   int mnin;                  /* No. of Mapping inputs */
   int ok;                    /* Are input indices OK? */

/* Initialise */
   result = NULL;
   *map = NULL;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Invoke the parent astMapSplit method to see if it can do the job. */
   result = (*parent_mapsplit)( this_map, nin, in, map, status );

/* If not, we provide a special implementation here. */
   if( !result ) {

/* Get a pointer to the WcsMap structure. */
      this = (AstWcsMap *) this_map;

/* Prevent compiler warnings. */
      ilatlon = -1;

/* Allocate memory for the returned array. */
      result = astMalloc( sizeof( int )*(size_t) nin );
      if( astOK ) {

/* Get the indices of the longitude and latitude axes in the WcsMap */
         lonax = astGetWcsAxis( this, 0 );
         latax = astGetWcsAxis( this, 1 );

/* See if the selected axes include the longitude and/or latitude axis.
   At the same time check the axis indices are ok, and set up the output
   axis array. */
         ilat = -1;
         ilon = -1;
         mnin = astGetNin( this );
         ok = 1;
         for( i = 0; i < nin; i++ ) {
            iin = in[ i ];
            if( iin < 0 || iin >= mnin ) {
               ok = 0;
               break;
            } else if( iin == lonax ) {
               ilon = i;
               ilatlon = i;
            } else if( iin == latax ) {
               ilat = i;
               ilatlon = i;
            }
            result[ i ] = iin;
         }

/* If any of the input indices were invalid, free the returned array. */
         if( !ok ) {
            result = astFree( result );

/* If both longitude and latitude axes are selected, then the returned Mapping
   is a WcsMap. Create one based on the supplied WcsMap. */
         } else if( ilat != -1 && ilon != -1 ) {
            newwcs = astWcsMap( nin, astGetWcsType( this ), ilon + 1, ilat + 1,
                                "", status );
            CopyPV( this, newwcs, status );
            astSetInvert( newwcs, astGetInvert( this ) );
            *map = (AstMapping *) newwcs;

/* If neither the longitude nor the latitude axis has been selected, then
   the returned Mapping is a UnitMap. */
         } else if( ilat == -1 && ilon == -1 ) {
            *map = (AstMapping *) astUnitMap( nin, "", status );

/* If only one of the latitude and longitude axes was selected we remove
   it from the returned Mapping (a PermMap) and list of outputs */
         } else if( nin > 1 ) {

            for( i = ilatlon; i < nin - 1; i++ ) {
               result[ i ] = result[ i + 1 ];
            }
            result[ i ] = -1;

            inperm = astMalloc( sizeof( int )*(size_t) nin );
            outperm = astMalloc( sizeof( int )*(size_t) ( nin - 1 ) );
            if( outperm ) {
               for( i = 0; i < ilatlon; i++ ) {
                  inperm[ i ] = i;
                  outperm[ i ] = i;
               }
               inperm[ ilatlon ] = INT_MAX;
               for( i = ilatlon + 1; i < nin; i++ ) {
                  inperm[ i ] = i - 1;
                  outperm[ i - 1 ] = i;
               }

               *map = (AstMapping *) astPermMap( nin, inperm, nin - 1, outperm, NULL, " ", status );

            }
            inperm = astFree( inperm );
            outperm = astFree( outperm );

         } else {
            result = astFree( result );
         }
      }
   }

/* Free returned resources if an error has occurred. */
   if( !astOK ) {
      result = astFree( result );
      *map = astAnnul( *map );
   }

/* Return the list of output indices. */
   return result;
}

static void PermGet( AstPermMap *map, int **outperm, int **inperm,
                     double **consts, int *status ){
/*
*  Name:
*     PermGet

*  Purpose:
*     Get the axis permutation and constants array for a PermMap.

*  Type:
*     Private function.

*  Synopsis:
*     #include "wcsmap.h"
*     void PermGet( AstPermMap *map, int **outperm, int **inperm,
*                   double **const, int *status )

*  Class Membership:
*     WcsMap member function

*  Description:
*     This function returns axis permutation and constants arrays which can
*     be used to create a PermMap which is equivalent to the supplied PermMap.

*  Parameters:
*     map
*        The PermMap.
*     outperm
*        An address at which to return a popinter to an array of ints
*        holding the output axis permutation array. The array should be
*        released using astFree when no longer needed.
*     inperm
*        An address at which to return a popinter to an array of ints
*        holding the input axis permutation array. The array should be
*        released using astFree when no longer needed.
*     consts
*        An address at which to return a popinter to an array of doubles
*        holding the constants array. The array should be released using
*        astFree when no longer needed.
*     status
*        Pointer to the inherited status variable.

*  Notes:
*     -  NULL pointers are returned if an error has already occurred, or if
*     this function should fail for any reason.
*/

/* Local Variables: */
   AstPointSet *pset1;       /* PointSet holding input positions for PermMap */
   AstPointSet *pset2;       /* PointSet holding output positions for PermMap */
   double **ptr1;            /* Pointer to pset1 data */
   double **ptr2;            /* Pointer to pset2 data */
   double *cnst;             /* Pointer to constants array */
   double cn;                /* Potential new constant value */
   double ip;                /* Potential output axis index */
   double op;                /* Potential input axis index */
   int *inprm;               /* Pointer to input axis permutation array */
   int *outprm;              /* Pointer to output axis permutation array */
   int i;                    /* Axis count */
   int nc;                   /* Number of constants stored so far */
   int nin;                  /* No. of input coordinates for the PermMap */
   int nout;                 /* No. of output coordinates for the PermMap */

/* Initialise. */
   if( outperm ) *outperm = NULL;
   if( inperm ) *inperm = NULL;
   if( consts ) *consts = NULL;

/* Check the global error status and the supplied pointers. */
   if ( !astOK || !outperm || !inperm || !consts ) return;

/* Initialise variables to avoid "used of uninitialised variable"
   messages from dumb compilers. */
   nc = 0;

/* Get the number of input and output axes for the supplied PermMap. */
   nin = astGetNin( map );
   nout = astGetNout( map );

/* Allocate the memory for the returned arrays. */
   outprm = (int *) astMalloc( sizeof( int )* (size_t) nout );
   inprm = (int *) astMalloc( sizeof( int )* (size_t) nin );
   cnst = (double *) astMalloc( sizeof( double )* (size_t) ( nout + nin ) );

/* Returned the pointers to these arrays.*/
   *outperm = outprm;
   *inperm = inprm;
   *consts = cnst;

/* Create two PointSets, each holding two points, which can be used for
   input and output positions with the PermMap. */
   pset1 = astPointSet( 2, nin, "", status );
   pset2 = astPointSet( 2, nout, "", status );

/* Set up the two input positions to be [0,1,2...] and [-1,-1,-1,...]. The
   first position is used to enumerate the axes, and the second is used to
   check for constant axis values. */
   ptr1 = astGetPoints( pset1 );
   if( astOK ){
      for( i = 0; i < nin; i++ ){
         ptr1[ i ][ 0 ] = ( double ) i;
         ptr1[ i ][ 1 ] = -1.0;
      }
   }

/* Use the PermMap to transform these positions in the forward direction. */
   (void) astTransform( map, pset1, 1, pset2 );

/* Look at the mapped positions to determine the output axis permutation
   array. */
   ptr2 = astGetPoints( pset2 );
   if( astOK ){

/* No constant axis valeus found yet. */
      nc = 0;

/* Do each output axis. */
      for( i = 0; i < nout; i++ ){

/* If the output axis value is copied from an input axis value, the index
   of the appropriate input axis will be in the mapped first position. */
         op = ptr2[ i ][ 0 ];

/* If the output axis value is assigned a constant value, the result of
   mapping the two different input axis values will be the same. */
         cn = ptr2[ i ][ 1 ];
         if( op == cn ) {

/* We have found another constant. Store it in the constants array, and
   store the index of the constant in the output axis permutation array. */
            cnst[ nc ] = cn;
            outprm[ i ] = -( nc + 1 );
            nc++;

/* If the output axis values are different, then the output axis value
   must be copied from the input axis value. */
         } else {
            outprm[ i ] = (int) ( op + 0.5 );
         }
      }
   }

/* Now do the same thing to determine the input permutation array. */
   if( astOK ){
      for( i = 0; i < nout; i++ ){
         ptr2[ i ][ 0 ] = ( double ) i;
         ptr2[ i ][ 1 ] = -1.0;
      }
   }

   (void) astTransform( map, pset2, 0, pset1 );

   if( astOK ){

      for( i = 0; i < nin; i++ ){

         ip = ptr1[ i ][ 0 ];
         cn = ptr1[ i ][ 1 ];
         if( ip == cn ) {

            cnst[ nc ] = cn;
            inprm[ i ] = -( nc + 1 );
            nc++;

         } else {
            inprm[ i ] = (int) ( ip + 0.5 );
         }
      }
   }

/* Annul the PointSets. */
   pset1 = astAnnul( pset1 );
   pset2 = astAnnul( pset2 );

/* If an error has occurred, attempt to free the returned arrays. */
   if( !astOK ) {
      *outperm = (int *) astFree( (void *) *outperm );
      *inperm = (int *) astFree( (void *) *inperm );
      *consts = (double *) astFree( (void *) *consts );
   }

/* Return. */
   return;
}

static void SetAttrib( AstObject *this_object, const char *setting, int *status ) {
/*
*  Name:
*     SetAttrib

*  Purpose:
*     Set an attribute value for a WcsMap.

*  Type:
*     Private function.

*  Synopsis:
*     #include "wcsmap.h"
*     void SetAttrib( AstObject *this, const char *setting )

*  Class Membership:
*     WcsMap member function (over-rides the astSetAttrib protected
*     method inherited from the Mapping class).

*  Description:
*     This function assigns an attribute value for a WcsMap, the
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
*        Pointer to the WcsMap.
*     setting
*        Pointer to a null-terminated string specifying the new attribute
*        value.
*/

/* Local Variables: */
   AstWcsMap *this;              /* Pointer to the WcsMap structure */
   double dval;                  /* Attribute value */
   int len;                      /* Length of setting string */
   int nc;                       /* Number of characters read by astSscanf */
   int i;                        /* Axis index */
   int m;                        /* Projection parameter number */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain a pointer to the WcsMap structure. */
   this = (AstWcsMap *) this_object;

/* Obtain the length of the setting string. */
   len = (int) strlen( setting );

/* Test for each recognised attribute in turn, using "astSscanf" to parse
   the setting string and extract the attribute value (or an offset to
   it in the case of string values). In each case, use the value set
   in "nc" to check that the entire string was matched. Once a value
   has been obtained, use the appropriate method to set it. */

/* ProjP(i). */
/* --------- */
   if ( nc = 0, ( 2 == astSscanf( setting, "projp(%d)= %lg %n", &m, &dval, &nc ) )
                  && ( nc >= len ) ) {
      astSetPV( this, astGetWcsAxis( this, 1 ), m, dval );

/* PV. */
/* --- */
   } else if ( nc = 0, ( 3 == astSscanf( setting, "pv%d_%d= %lg %n", &i, &m, &dval, &nc ) )
                  && ( nc >= len ) ) {
      astSetPV( this, i - 1, m, dval );

/* Define macros to see if the setting string matches any of the
   read-only attributes of this class. */
#define MATCH(attrib) \
        ( nc = 0, ( 0 == astSscanf( setting, attrib "=%*[^\n]%n", &nc ) ) && \
                  ( nc >= len ) )

#define MATCH2(attrib) \
        ( nc = 0, ( 1 == astSscanf( setting, attrib "(%d)=%*[^\n]%n", &i, &nc ) ) && \
                  ( nc >= len ) )

/* If the attribute was not recognised, use this macro to report an error
   if a read-only attribute has been specified. */
   } else if ( MATCH( "wcstype" ) ||
        MATCH( "natlat" ) ||
        MATCH( "natlon" ) ||
        MATCH2( "wcsaxis" ) ) {
      astError( AST__NOWRT, "astSet: The setting \"%s\" is invalid for a %s.", status,
                setting, astGetClass( this ) );
      astError( AST__NOWRT, "This is a read-only attribute." , status);

/* If the attribute is still not recognised, pass it on to the parent
   method for further interpretation. */
   } else {
      (*parent_setattrib)( this_object, setting, status );
   }

/* Undefine macros local to this function. */
#undef MATCH
#undef MATCH2
}

static void SetPV( AstWcsMap *this, int i, int m, double val, int *status ) {
/*
*+
*  Name:
*     astSetPV

*  Purpose:
*     Set the value of a PVi_m attribute.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "wcsmap.h"
*     void astSetPV( AstWcsMap *this, int i, int m, double val )

*  Class Membership:
*     WcsMap protected function

*  Description:
*     This function stores a value for the specified member of the PV
*     attribute array.

*  Parameters:
*     this
*        A pointer to the WcsMap.
*     i
*        Zero based axis index.
*     m
*        Zero based parameter index.

*-
*/
/* Local Variables: */
   int naxis;       /* No. of axes in WcsMap */
   int mm;          /* Loop count */
   int mxpar;       /* Max number of parameters allowed for the axis */

/* Check the global error status. */
   if ( !astOK ) return;

/* Find the number of axes in the WcsMap. */
   naxis = astGetNin( this );

/* Validate the axis index. */
   if( i < 0 || i >= naxis ){
      astError( AST__AXIIN, "astSetPV(%s): Axis index (%d) is invalid in "
                "attribute PV%d_%d  - it should be in the range 1 to %d.",
                status, astGetClass( this ), i + 1, i + 1, m, naxis );

/* Validate the parameter index. */
   } else {
      mxpar = astGetPVMax( this, i );
      if( m < 0 || m > mxpar ){
         astError( AST__AXIIN, "astSetPV(%s): Parameter index (%d) is invalid "
                   "in attribute PV%d_%d for a \"%s\" projection - it should be "
                   "in the range 0 to %d.", status, astGetClass( this ), m, i + 1, m,
                   FindPrjData( this->type, status )->ctype, mxpar );

/* If the dynamic arrays used to hold the parameters have not yet been
   created, create them now, and store pointers to them in the WcsMap
   structure. */
      } else {
         if( !this->np || !this->p ) {
            this->np = (int *) astMalloc( sizeof(int)*naxis );
            this->p = (double **) astMalloc( sizeof(double *)*naxis );
            if( astOK ) {
               for( mm = 0; mm < naxis; mm++ ) {
                  this->np[ mm ] = 0;
                  this->p[ mm ] = NULL;
               }
            }

/* Release the dynamic arrays if an error has occurred. */
            if( !astOK ) FreePV( this, status );

         }
      }

/* Check we can use the arrays. */
      if( astOK ) {

/* Ensure the dynamic array used to hold parameter values for the
   specified axis is big enough to hold the specified parameter. */
         this->p[ i ] = (double *) astGrow( (void *) this->p[ i ],
                                            m + 1, sizeof(double) );

/* Check we can use this array. */
         if( astOK ) {

/* Store the supplied value in the relevant element of this array. */
            this->p[ i ][ m ] = val;

/* If the array was extended to hold this parameter... */
            if( this->np[ i ] <= m ) {

/* Fill any other new elements in this array with AST__BAD */
               for( mm = this->np[ i ]; mm < m; mm++ ) this->p[ i ][ mm ] = AST__BAD;

/* Remember the new array size. */
               this->np[ i ] = m + 1;
            }
         }
      }
   }

/* Re-initialize the values stored in the "AstPrjPrm" structure. */
   InitPrjPrm( this, status );
}

static void SetTPNTan( AstWcsMap *this, int val, int *status ) {
/*
*+
*  Name:
*     astSetTPNTan

*  Purpose:
*     Set the value of a TPNTan attribute.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "wcsmap.h"
*     void astSetTPNTan( AstWcsMap *this, int val )

*  Class Membership:
*     WcsMap protected function

*  Description:
*     This function stores a value for the TPNTan attribute and updates
*     the projection parameters used by WCSLIB accordingly.

*  Parameters:
*     this
*        A pointer to the WcsMap.
*     val
*        New attribute value.

*-
*/

/* Check the global error status. */
   if ( !astOK ) return;

/* Store the new value. */
   this->tpn_tan = ( val != 0 );

/* Re-initialize the values stored in the "AstPrjPrm" structure. */
   InitPrjPrm( this, status );
}

static int TestAttrib( AstObject *this_object, const char *attrib, int *status ) {
/*
*  Name:
*     TestAttrib

*  Purpose:
*     Test if a specified attribute value is set for a WcsMap.

*  Type:
*     Private function.

*  Synopsis:
*     #include "wcsmap.h"
*     int TestAttrib( AstObject *this, const char *attrib, int *status )

*  Class Membership:
*     WcsMap member function (over-rides the astTestAttrib protected
*     method inherited from the Mapping class).

*  Description:
*     This function returns a boolean result (0 or 1) to indicate whether
*     a value has been set for one of a WcsMap's attributes.

*  Parameters:
*     this
*        Pointer to the WcsMap.
*     attrib
*        Pointer to a null-terminated string specifying the attribute
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
   AstWcsMap *this;             /* Pointer to the WcsMap structure */
   int i;                       /* Axis index */
   int m;                       /* Projection parameter index */
   int len;                     /* Length os supplied string */
   int nc;                      /* No. of characters read by astSscanf */
   int result;                  /* Result value to return */

/* Initialise. */
   result = 0;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Obtain a pointer to the WcsMap structure. */
   this = (AstWcsMap *) this_object;

/* Obtain the length of the attrib string. */
   len = strlen( attrib );

/* Check the attribute name and test the appropriate attribute. */


/* ProjP(i). */
/* --------- */
   if ( nc = 0, ( 1 == astSscanf( attrib, "projp(%d)%n", &m, &nc ) )
                  && ( nc >= len ) ) {
      result = astTestPV( this, astGetWcsAxis( this, 1 ), m );

/* PV. */
/* --- */
   } else if ( nc = 0, ( 2 == astSscanf( attrib, "pv%d_%d%n", &i, &m, &nc ) )
                  && ( nc >= len ) ) {
      result = astTestPV( this, i - 1, m );

/* If the name is not recognised, test if it matches any of the
   read-only attributes of this class. If it does, then return
   zero. */
   } else if ( !strcmp( attrib, "wcstype" ) ||
               !strcmp( attrib, "natlat" ) ||
               !strcmp( attrib, "natlon" ) ||
               ( nc = 0, ( 1 == astSscanf( attrib, "wcsaxis(%d)%n", &i, &nc ) )
                           && ( nc >= len ) ) ) {
      result = 0;

/* If the attribute is still not recognised, pass it on to the parent
   method for further interpretation. */
   } else {
      result = (*parent_testattrib)( this_object, attrib, status );
   }

/* Return the result, */
   return result;
}

static int TestPV( AstWcsMap *this, int i, int m, int *status ) {
/*
*+
*  Name:
*     astTestPV

*  Purpose:
*     Test a PVi_m attribute.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "wcsmap.h"
*     int astTestPV( AstWcsMap *this, int i, int m )

*  Class Membership:
*     WcsMap protected function

*  Description:
*     This function returns 1 if a specified member of the PV attribute
*     array is currently set, and 0 otherwise.

*  Parameters:
*     this
*        A pointer to the WcsMap.
*     i
*        Zero based axis index.
*     m
*        Zero based parameter index.

*  Returned Value:
*     1 if the attribute is set, 0 otherwise.

*-
*/
/* Local Variables: */
   int ret;
   int npar;
   int mxpar;

/* Initialise */
   ret = 0;

/* Check the global error status. */
   if ( !astOK ) return ret;

/* Validate the axis index. */
   if( i < 0 || i >= astGetNin( this ) ){
      astError( AST__AXIIN, "astTestPV(%s): Axis index (%d) is invalid in "
                "attribute PV%d_%d  - it should be in the range 1 to %d.",
                status, astGetClass( this ), i + 1, i + 1, m, astGetNin( this ) );

/* Find the maximum number of parameters allowed for the axis. */
   } else {
      mxpar = astGetPVMax( this, i );

/* Ignore unused parameters. */
      if( m < 0 || m > mxpar ){

/* See if the parameter is currently set. */
      } else if( this->np && this->p ){
         npar = this->np[ i ];
         if( m < npar && this->p[ i ] ){
            ret = ( this->p[ i ][ m ] != AST__BAD );
         }
      }
   }

   return ret;
}

static AstPointSet *Transform( AstMapping *this, AstPointSet *in,
                               int forward, AstPointSet *out, int *status ) {
/*
*+
*  Name:
*     Transform

*  Purpose:
*     Apply a WcsMap to a set of points.

*  Type:
*     Private function.

*  Synopsis:
*     #include "wcsmap.h"
*     AstPointSet *Transform( AstMapping *this, AstPointSet *in,
*                             int forward, AstPointSet *out, int *status )

*  Class Membership:
*     WcsMap member function (over-rides the astTransform method inherited
*     from the Mapping class).

*  Description:
*     This function takes a WcsMap and a set of points encapsulated in a
*     PointSet and transforms the points using the requested projection.

*  Parameters:
*     this
*        Pointer to the WcsMap.
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
*     -  Assuming the WcsMap has not been inverted, the forward mapping
*     transforms spherical (longitude,latitude) pairs into Cartesian (x,y)
*     pairs. Longitude and latitude values are given and returned in radians,
*     and no restrictions are imposed on their ranges. X and Y values are
*     also given and returned in radians, with no restrictions imposed on
*     their ranges.
*     -  A null pointer will be returned if this function is invoked with the
*     global error status set, or if it should fail for any reason.
*     -  The number of coordinate values per point in the input PointSet must
*     match the number of coordinates for the WcsMap being applied.
*     -  If an output PointSet is supplied, it must have space for sufficient
*     number of points and coordinate values per point to accommodate the
*     result. Any excess space will be ignored.
*-
*/

/* Local Variables: */
   AstPointSet *result;          /* Pointer to output PointSet */
   AstWcsMap *map;               /* Pointer to WcsMap to be applied */
   double **ptr_in;              /* Pointer to input coordinate data */
   double **ptr_out;             /* Pointer to output coordinate data */
   int i;                        /* Axis count */
   int latax;                    /* Latitude axis index */
   int lonax;                    /* Longitude axis index */
   int npoint;                   /* Number of points */
   int status_value;                   /* Status from Map function */

/* Check the global error status. */
   if ( !astOK ) return NULL;

/* Obtain a pointer to the WcsMap. */
   map = (AstWcsMap *) this;

/* Apply the parent mapping using the stored pointer to the Transform member
   function inherited from the parent Mapping class. This function validates
   all arguments and generates an output PointSet if necessary, but does not
   actually transform any coordinate values. */
   result = (*parent_transform)( this, in, forward, out, status );

/* We will now extend the parent astTransform method by performing the
   calculations needed to generate the output coordinate values. */

/* Determine the numbers of points from the input PointSet and obtain pointers
   for accessing the input and output coordinate values. */
   npoint = astGetNpoint( in );
   ptr_in = astGetPoints( in );
   ptr_out = astGetPoints( result );

/* If a genuine FITS-WCS projection is being performed... */
   if( astGetWcsType( map ) != AST__WCSBAD ){

/* Determine whether to apply the forward or inverse mapping, according to the
   direction specified and whether the mapping has been inverted. */
      if ( astGetInvert( map ) ) forward = !forward;

/* Do the coordinate transformation. */
      lonax = astGetWcsAxis( map, 0 );
      latax = astGetWcsAxis( map, 1 );
      status_value = Map( map, forward, npoint, ptr_in[ lonax ], ptr_in[ latax ],
                          ptr_out[ lonax ], ptr_out[ latax ], status );

/* Report an error if the projection type was unrecognised. */
      if ( status_value == 1 ) {
         astError( AST__WCSTY, "astTransform(%s): The %s specifies an "
                   "illegal projection type ('%s').", status, astClass( this ),
                   astClass( this ), FindPrjData( map->type, status )->desc  );

/* Report an error if the projection parameters were invalid. */
       } else if ( status_value == 2 ) {
          astError( AST__WCSPA, "astTransform(%s): The %s projection "
                    "parameter values in this %s are unusable.", status,
                    astClass( this ), FindPrjData( map->type, status )->desc,
                    astClass( this )  );

/* Report an error if required projection parameters were not supplied. */
       } else if ( status_value >= 400 ) {
          astError( AST__WCSPA, "astTransform(%s): Required projection "
                    "parameter PV%d_%d was not supplied for a %s "
                    "projection.", status, astClass( this ), latax+1, status_value - 400,
                    FindPrjData( map->type, status )->desc  );

       } else if ( status_value >= 100 ) {
          astError( AST__WCSPA, "astTransform(%s): Required projection "
                    "parameter PV%d_%d was not supplied for a %s "
                    "projection.", status, astClass( this ), lonax+1, status_value - 100,
                    FindPrjData( map->type, status )->desc  );
       }

/* Copy the remaining axes (i.e. all axes except the longitude and latitude
   axes) from the input to the output. */
      for( i = 0; i < astGetNcoord( in ); i++ ){
         if( ( i != lonax && i != latax ) ){
            (void) memcpy( ptr_out[ i ], ptr_in[ i ], sizeof( double )*
                           (size_t) npoint );
         }

      }

/* If there is no FITS-WCS projection, just copy all the axes from input to
   output. */
   } else {
      for( i = 0; i < astGetNcoord( in ); i++ ){
         (void) memcpy( ptr_out[ i ], ptr_in[ i ], sizeof( double )*
                        (size_t) npoint );
      }
   }

/* If an error has occurred, attempt to delete the results PointSet. */
   if ( !astOK ) result = astDelete( result );

/* Return a pointer to the output PointSet. */
   return result;

}

int astWcsPrjType_( const char *ctype, int *status ){
/*
*+
*  Name:
*     astWcsPrjType

*  Purpose:
*     Get the integer identifier for a WCSLIB projection given by a FITS
*     CTYPE keyword value.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "wcsmap.h"
*     int astWcsPrjType( const char *ctype )

*  Class Membership:
*     WcsMap protected function

*  Description:
*     This function returns the integer identifier for the WCSLIB projection
*     specified by the supplied FITS CTYPE keyword value. The returned
*     value can be passed to astWcsMap to create a WcsMap which implements
*     the corresponding projection.

*  Parameters:
*     ctype
*        A pointer to the last 4 characters of a FITS CTYPE1 (etc) keyword.

*  Returned Value:
*     The integer identifier associated with the projection.

*  Notes:
*     -  A value of AST__WCSBAD is returned if the projection type is
*     unknown, but no error is reported.
*-
*/

   PrjData *data;
   char buffer[81];
   const char *a;
   char *b;

/* Remove leading and trailing blanks from the supplied string. */
   a = ctype;
   b = buffer;
   while( *a && (b - buffer) < 80 ){
      if( !isspace( (int) *a ) ) {
         *(b++) = *a;
      }
      a++;
   }
   *b = 0;

/* Search for the projection in the list of available projectons. */
   data = PrjInfo;
   while( data->prj != AST__WCSBAD && strcmp( data->ctype, buffer ) ) data ++;

   return data->prj;
}

const char *astWcsPrjName_( int type, int *status ){
/*
*+
*  Name:
*     astWcsPrjName

*  Purpose:
*     Get the name of a projection given its integer identifier.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "wcsmap.h"
*     const char *astWcsPrjName( int type )

*  Class Membership:
*     WcsMap protected function

*  Description:
*     This function returns a string holding 4 characters which can be
*     used as the last 4 characters of a FITS CTYPE keyword value
*     describing the WCSLIB projection specified by the supplied type.

*  Parameters:
*     type
*        The projection type.

*  Returned Value:
*     A pointer to a null-terminated const character string holding the
*     last 4 CTYPE characters describing the projection.

*  Notes:
*     -  This function attempts to execute even if an error has already
*     occurred.
*-
*/

   PrjData *data;
   data = PrjInfo;
   while( data->prj != AST__WCSBAD && data->prj != type ) data ++;
   return data->ctype;
}

const char *astWcsPrjDesc_( int type, int *status ){
/*
*+
*  Name:
*     astWcsPrjDesc

*  Purpose:
*     Get a textual description of a projection given its integer
*     identifier.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "wcsmap.h"
*     const char *astWcsPrjDesc( int type )

*  Class Membership:
*     WcsMap protected function

*  Description:
*     This function returns a pointer to a string string holding a
*     textual description of the specified projection type.

*  Parameters:
*     type
*        The projection type.

*  Returned Value:
*     A pointer to a null-terminated const character string holding the
*     projection description.

*  Notes:
*     -  This function attempts to execute even if an error has already
*     occurred.
*-
*/

   PrjData *data;
   data = PrjInfo;
   while( data->prj != AST__WCSBAD && data->prj != type ) data ++;
   return data->desc;
}

static void WcsPerm( AstMapping **maps, int *inverts, int iwm, int *status ){
/*
*  Name:
*     WcsPerm

*  Purpose:
*     Swap a WcsMap and a PermMap.

*  Type:
*     Private function.

*  Synopsis:
*     #include "wcsmap.h"
*     void WcsPerm( AstMapping **maps, int *inverts, int iwm, int *status )

*  Class Membership:
*     WcsMap member function

*  Description:
*     A list of two Mappings is supplied containing a WcsMap and a
*     PermMap. These Mappings are annulled, and replaced with
*     another pair of Mappings consisting of a WcsMap and a PermMap
*     in the opposite order. These Mappings are chosen so that their
*     combined effect is the same as the original pair of Mappings.

*  Parameters:
*     maps
*        A pointer to an array of two Mapping pointers.
*     inverts
*        A pointer to an array of two invert flags.
*     iwm
*        The index within "maps" of the WcsMap.
*     status
*        Pointer to the inherited status variable.

*  Notes:
*     -  All links between input and output axes in the PermMap must
*     be bi-directional, but there can be unconnected axes, and there
*     need not be the same number of input and output axes. Both
*     longitude and latitude axes must be passed by the PermMap.

*/

/* Local Variables: */
   AstPermMap *pm;               /* Pointer to the supplied PermMap */
   AstPermMap *newpm;            /* Pointer to the returned PermMap */
   AstMapping *newwm;            /* Pointer to the returned WcsMap */
   AstWcsMap *wm;                /* Pointer to the supplied WcsMap */
   double *consts;               /* Pointer to constants array */
   int *inperm;                  /* Pointer to input axis permutation array */
   int *outperm;                 /* Pointer to output axis permutation array */
   int latax;                    /* Index of latitude axis */
   int lonax;                    /* Index of longitude axis */
   int npin;                     /* No. of input axes in supplied PermMap */
   int npout;                    /* No. of output axes in supplied PermMap */
   int old_pinv;                 /* Invert value for the supplied PermMap */
   int old_winv;                 /* Invert value for the supplied WcsMap */
   int type;                     /* Projection type */
   int done;                     /* Have Mappings been swapped? */
   int i;                        /* AXis index */
   double *p;                    /* Pointer to input position */
   double *q;                    /* Pointer to output position */

/* Check the global error status. */
   if ( !astOK ) return;

/* Initialise variables to avoid "used of uninitialised variable"
   messages from dumb compilers. */
   newpm = NULL;
   newwm = NULL;

/* Store pointers to the supplied WcsMap and the PermMap. */
   wm = (AstWcsMap *) maps[ iwm ];
   pm = (AstPermMap *) maps[ 1 - iwm ];

/* Temporarily set the Invert attribute of the supplied PermMap to the
   supplied values. */
   old_pinv = astGetInvert( pm );
   astSetInvert( pm, inverts[ 1 - iwm ] );
   old_winv = astGetInvert( wm );
   astSetInvert( wm, inverts[ iwm ] );

/* Get the projection type of the supplied WcsMap and the indices of the
   longitude and latitude axes. */
   type = astGetWcsType( wm );
   lonax = astGetWcsAxis( wm, 0 );
   latax = astGetWcsAxis( wm, 1 );

/* Get the axis permutation and constants arrays representing the
   PermMap. Note, no constants are used more than once in the returned
   arrays (i.e. duplicate constants are returned in "consts" if more than
   one axis uses a given constant). */
   PermGet( pm, &outperm, &inperm, &consts, status );

   if( astOK ) {

/* Get the number of input and output axes in the PermMap. */
      npin = astGetNin( pm );
      npout = astGetNout( pm );

/* If the lon and lat axes of the WcsMap are unassigned, we return a
   UnitMap instead of a WcsMap.
   ================================================================ */
      done = 0;

/* If the PermMap comes after the WcsMap... */
      if( iwm == 0 ) {
         if( inperm[ lonax ] < 0 && inperm[ latax ] < 0 ) {
            done = 1;

/* Transform the constant values, using AST__BAD for other axes. */
            p = (double *) astMalloc( sizeof( double )*(size_t) npin );
            q = (double *) astMalloc( sizeof( double )*(size_t) npin );
            if( astOK ) {
               for( i = 0; i < npin; i++ ) {
                  if( inperm[ i ] < 0 ) {
                     p[ i ] = consts[ -inperm[ i ] - 1 ];
                  } else {
                     p[ i ] = AST__BAD;
                  }
               }

/* Transform this position using the inverse WcsMap. */
               astTranN( wm, 1, npin, 1, p, 0, npin, 1, q );

/* The new PermMap has the same axis permutations as the original, but it
   has different constants. */
               for( i = 0; i < npin; i++ ) {
                  if( inperm[ i ] < 0 ) {
                     consts[ -inperm[ i ] - 1 ] = q[ i ];
                  }
               }

               newpm = astPermMap( npin, inperm, npout, outperm, consts, "", status );

/* Use a UnitMap instead of the WcsMap. */
               newwm = (AstMapping *) astUnitMap( npout, "", status );

            }

/* Free memory */
            p = astFree( p );
            q = astFree( q );

         }

/* If the WcsMap comes after the PermMap... */
      } else {
         if( outperm[ lonax ] < 0 && outperm[ latax ] < 0 ) {
            done = 1;

/* Transform the constant values, using AST__BAD for other axes. */
            p = (double *) astMalloc( sizeof( double )*(size_t) npout );
            q = (double *) astMalloc( sizeof( double )*(size_t) npout );
            if( astOK ) {
               for( i = 0; i < npout; i++ ) {
                  if( outperm[ i ] < 0 ) {
                     p[ i ] = consts[ -outperm[ i ] - 1 ];
                  } else {
                     p[ i ] = AST__BAD;
                  }
               }

/* Transform this position using the forward WcsMap. */
               astTranN( wm, 1, npout, 1, p, 1, npout, 1, q );

/* The new PermMap has the same axis permutations as the original, but it
   has different constants. */
               for( i = 0; i < npout; i++ ) {
                  if( outperm[ i ] < 0 ) {
                     consts[ -outperm[ i ] - 1 ] = q[ i ];
                  }
               }

               newpm = astPermMap( npin, inperm, npout, outperm, consts, "", status );

/* Use a UnitMap instead ofhte WcsMap. */
               newwm = (AstMapping *) astUnitMap( npin, "", status );

            }

/* Free memory */
            p = astFree( p );
            q = astFree( q );

         }
      }

/* If the lon and lat axes of the WcsMap are both assigned, we return a
   WcsMap.
   ================================================================ */
      if( !done ) {

/* Create the new WcsMap with permuted longitude and latitude axes. Note,
   the private interface to astWcsMap uses 1-based axis indices. */
         if( iwm == 0 ) {
            newwm = (AstMapping *) astWcsMap( npout, type, inperm[ lonax ] + 1,
                                              inperm[ latax ] + 1, "", status );
         } else {
            newwm = (AstMapping *) astWcsMap( npin, type, outperm[ lonax ] + 1,
                                              outperm[ latax ] + 1, "", status );
         }

/* Copy any projection parameters which have been set. */
         CopyPV( wm, (AstWcsMap *) newwm, status );

/* Set the invert flag. */
         astSetInvert( newwm, inverts[ iwm ] );

/* The returned PermMap is a clone of the supplied PermMap */
         newpm = astClone( pm );
      }

/* Free the axis permutation and constants arrays. */
      outperm = (int *) astFree( (void *) outperm );
      inperm = (int *) astFree( (void *) inperm );
      consts = (double *) astFree( (void *) consts );
   }

/* Re-instate the original value of the Invert attributes. */
   astSetInvert( pm, old_pinv );
   astSetInvert( wm, old_winv );

/* Annul the supplied pointers */
   pm = astAnnul( pm );
   wm = astAnnul( wm );

/* Store the returned Mappings. */
   maps[ iwm ] = (AstMapping *) newpm;
   inverts[ iwm ] = astGetInvert( newpm );
   maps[ 1 - iwm ] = newwm;
   inverts[ 1 - iwm ] = astGetInvert( newwm );

/* Return. */
   return;
}

/* Functions which access class attributes. */
/* ---------------------------------------- */
/* Implement member functions to access the attributes associated with
   this class using the macros defined for this purpose in the
   "object.h" file. For a description of each attribute, see the class
   interface (in the associated .h file). */


/*
*att+
*  Name:
*     FITSProj

*  Purpose:
*     Is this WcsMap used as a FITS-WCS projection?

*  Type:
*     Protected attribute.

*  Synopsis:
*     Integer (boolean).

*  Description:
*     This attribute controls how a WcsMap is used when creating a set
*     of FITS headers. If the WcsMap is contained within a FrameSet
*     that is to be  converted into a set of FITS headers using the
c     astWrite funtion,
f     AST_WRITE routine,
*     the WcsMap will be used to define the projection code appeneded to
*     the FITS "CTYPEi" keywords if, and only if, the FITSProj attribute
*     is set non-zero in the WcsMap. In order for the conversion to be
*     successful, the compoound Mapping connecting the base and current
*     Frames in the FrameSet must contained one (and only one) WcsMap
*     that has a non-zero value for its FITSProj attribute.
*
*     The default value is one.

*  Applicability:
*     WcsMap
*        All Frames have this attribute.
*att-
*/
astMAKE_CLEAR(WcsMap,FITSProj,fits_proj,-INT_MAX)
astMAKE_GET(WcsMap,FITSProj,int,1,( ( this->fits_proj != -INT_MAX ) ?
                                       this->fits_proj : 1 ))
astMAKE_SET(WcsMap,FITSProj,int,fits_proj,( value != 0 ))
astMAKE_TEST(WcsMap,FITSProj,( this->fits_proj != -INT_MAX ))

/*
*att+
*  Name:
*     TPNTan

*  Purpose:
*     Should the TPN projection include a TAN projection?

*  Type:
*     Protected attribute.

*  Synopsis:
*     Integer (boolean).

*  Description:
*     This attribute controls how a WcsMap with a AST__TPN projection
*     type behaves. If the attribute value is non-zero (the default),
*     the complete projection is performed as described in the draft
*     version of FITS-WCS paper II. If it is zero, then the TAN
*     projection from (psi,theta) to (xi,eta) is replaced by a unit
*     transformation, so that the WcsMap implements the polynomial
*     transformation only, without any preceding TAN projection.
*
*     In addition if the TPNTan value is zero, then the WcsMap
*     assumes that the input and output values are in degrees rather
*     than radians.

*  Applicability:
*     WcsMap
*        All Frames have this attribute.
*att-
*/
astMAKE_GET(WcsMap,TPNTan,int,1,( ( this->tpn_tan != -INT_MAX ) ?
                                       this->tpn_tan : 1 ))
astMAKE_TEST(WcsMap,TPNTan,( this->tpn_tan != -INT_MAX ))

/* ProjP. */
/* ------ */
/*
*att++
*  Name:
*     ProjP(m)

*  Purpose:
*     FITS-WCS projection parameters.

*  Type:
*     Public attribute.

*  Synopsis:
*     Floating point.

*  Description:
*     This attribute provides aliases for the PV attributes, which
*     specifies the projection parameter values to be used by a WcsMap
*     when implementing a FITS-WCS sky projection. ProjP is retained for
*     compatibility with previous versions of FITS-WCS and AST. New
*     applications should use the PV attibute instead.
*
*     Attributes ProjP(0) to ProjP(9) correspond to attributes PV<axlat>_0
*     to PV<axlat>_9, where <axlat> is replaced by the index of the
*     latitude axis (given by attribute WcsAxis(2)). See PV for further
*     details.

*  Applicability:
*     WcsMap
*        All WcsMaps have this attribute.

*att--
*/

/* PV. */
/* --- */
/*
*att++
*  Name:
*     PVi_m

*  Purpose:
*     FITS-WCS projection parameters.

*  Type:
*     Public attribute.

*  Synopsis:
*     Floating point.

*  Description:
*     This attribute specifies the projection parameter values to be
*     used by a WcsMap when implementing a FITS-WCS sky projection.
*     Each PV attribute name should include two integers, i and m,
*     separated by an underscore. The axis index is specified
*     by i, and should be in the range 1 to 99. The parameter number
*     is specified by m, and should be in the range 0 to 99. For
*     example, "PV2_1=45.0" would specify a value for projection
*     parameter 1 of axis 2 in a WcsMap.
*
*     These projection parameters correspond exactly to the values
*     stored using the FITS-WCS keywords "PV1_1", "PV1_2", etc. This
*     means that projection parameters which correspond to angles must
*     be given in degrees (despite the fact that the angular
*     coordinates and other attributes used by a WcsMap are in
*     radians).
*
*     The set of projection parameters used by a WcsMap depends on the
*     type of projection, which is determined by its WcsType
*     parameter.  Most projections either do not require projection
*     parameters, or use parameters 1 and 2 associated with the latitude
*     axis. You should consult the FITS-WCS paper for details.
*
*     Some projection parameters have default values (as defined in
*     the FITS-WCS paper) which apply if no explicit value is given.
*     You may omit setting a value for these "optional" parameters and the
*     default will apply. Some projection parameters, however, have no
*     default and a value must be explicitly supplied.  This is most
*     conveniently
c     done using the "options" argument of astWcsMap (q.v.) when a WcsMap
f     done using the OPTIONS argument of AST_WCSMAP (q.v.) when a WcsMap
*     is first created. An error will result when a WcsMap is used to
*     transform coordinates if any of its required projection
*     parameters has not been set and lacks a default value.

*     A "get" operation for a parameter which has not been assigned a value
*     will return the default value defined in the FITS-WCS paper, or
*     AST__BAD if the paper indicates that the parameter has no default.
*     A default value of zero is returned for parameters which are not
*     accessed by the projection.
*
*     Note, the FITS-WCS paper reserves parameters 1 and 2 on the longitude
*     axis to hold the native longitude and latitude of the fiducial
*     point of the projection, in degrees. The default values for these
*     parameters are determined by the projection type. The AST-specific
*     TPN projection does not use this convention - all projection
*     parameters for both axes are used to represent polynomical correction
*     terms, and the native longitude and latitude at the fiducial point may
*     not be changed from the default values of zero and 90 degrees.

*  Applicability:
*     WcsMap
*        All WcsMaps have this attribute.

*  Notes:
*     - If the projection parameter values given for a WcsMap do not
*     satisfy all the required constraints (as defined in the FITS-WCS
*     paper), then an error will result when the WcsMap is used to
*     transform coordinates.
*att--
*/

/* PVMax. */
/* ------ */
/*
*att++
*  Name:
*     PVMax(i)

*  Purpose:
*     Maximum number of FITS-WCS projection parameters.

*  Type:
*     Public attribute.

*  Synopsis:
*     Integer, read-only.

*  Description:
*     This attribute specifies the largest legal index for a PV projection
*     parameter attached to a specified axis of the WcsMap (i.e. the
*     largest legal value for "m" when accessing the "PVi_m" attribute).
*     The axis index is specified by i, and should be in the range 1 to 99.
*     The value for each axis is determined by the projection type specified
*     when the WcsMap
c     is first created using astWcsMap and cannot subsequently be
f     is first created using AST_WCSMAP and cannot subsequently be
*     changed.

*  Applicability:
*     WcsMap
*        All WcsMaps have this attribute.
*att--
*/

/* WcsType. */
/* -------- */
/*
*att++
*  Name:
*     WcsType

*  Purpose:
*     FITS-WCS projection type.

*  Type:
*     Public attribute.

*  Synopsis:
*     Integer, read-only.

*  Description:
*     This attribute specifies which type of FITS-WCS projection will
*     be performed by a WcsMap. The value is specified when a WcsMap
c     is first created using astWcsMap and cannot subsequently be
f     is first created using AST_WCSMAP and cannot subsequently be
*     changed.
*
c     The values used are represented by macros with names of
f     The values used are represented by symbolic constants with names of
*     the form "AST__XXX", where "XXX" is the (upper case) 3-character
*     code used by the FITS-WCS "CTYPEi" keyword to identify the
*     projection. For example, possible values are AST__TAN (for the
*     tangent plane or gnomonic projection) and AST__AIT (for the
*     Hammer-Aitoff projection). AST__TPN is an exception in that it
*     is not part of the FITS-WCS standard (it represents a TAN
*     projection with polynomial correction terms as defined in an early
*     draft of the FITS-WCS paper).

*  Applicability:
*     WcsMap
*        All WcsMaps have this attribute.

*  Notes:
*     - For a list of available projections, see the FITS-WCS paper.
*att--
*/

/* Type of FITS-WCS projection. Read only. */
astMAKE_GET(WcsMap,WcsType,int,AST__WCSBAD,this->type)

/* NatLat. */
/* ------- */
/*
*att++
*  Name:
*     NatLat

*  Purpose:
*     Native latitude of the reference point of a FITS-WCS projection.

*  Type:
*     Public attribute.

*  Synopsis:
*     Floating point, read-only.

*  Description:
*     This attribute gives the latitude of the reference point of the
*     FITS-WCS projection implemented by a WcsMap. The value is in
*     radians in the "native spherical" coordinate system. This value is
*     fixed for most projections, for instance it is PI/2 (90 degrees)
*     for all zenithal projections. For some projections (e.g. the conics)
*     the value is not fixed, but is specified by parameter one on the
*     latitude axis.
*
*     FITS-WCS paper II introduces the concept of a "fiducial point"
*     which is logical distinct from the projection reference point.
*     It is easy to confuse the use of these two points. The fiducial
*     point is the point which has celestial coordinates given by the
*     CRVAL FITS keywords. The native spherical coordinates for this point
*     default to the values of the NatLat and NatLon, but these defaults
*     mey be over-ridden by values stored in the PVi_j keywords. Put
*     another way, the CRVAL keywords will by default give the celestial
*     coordinates of the projection reference point, but may refer to
*     some other point if alternative native longitude and latitude values
*     are provided through the PVi_j keywords.
*
*     The NatLat attribute is read-only.

*  Applicability:
*     WcsMap
*        All WcsMaps have this attribute.

*  Notes:
*     - A default value of AST__BAD is used if no latitude value is available.
*att--
*/

/*
*att++
*  Name:
*     NatLon

*  Purpose:
*     Native longitude of the reference point of a FITS-WCS projection.

*  Type:
*     Public attribute.

*  Synopsis:
*     Floating point, read-only.

*  Description:
*     This attribute gives the longitude of the reference point of the
*     FITS-WCS projection implemented by a WcsMap. The value is in
*     radians in the "native spherical" coordinate system, and will
*     usually be zero. See the description of attribute NatLat for further
*     information.
*
*     The NatLon attribute is read-only.

*  Applicability:
*     WcsMap
*        All WcsMaps have this attribute.

*  Notes:
*att--
*/

/* WcsAxis. */
/* -------- */
/*
*att++
*  Name:
*     WcsAxis(lonlat)

*  Purpose:
*     FITS-WCS projection axes.

*  Type:
*     Public attribute.

*  Synopsis:
*     Integer, read-only.

*  Description:
*     This attribute gives the indices of the longitude and latitude
*     coordinates of the FITS-WCS projection within the coordinate
*     space used by a WcsMap. These indices are defined when the
c     WcsMap is first created using astWcsMap and cannot
f     WcsMap is first created using AST_WCSMAP and cannot
*     subsequently be altered.
*
*     If "lonlat" is 1, the index of the longitude axis is
*     returned. Otherwise, if it is 2, the index of the latitude axis
*     is returned.

*  Applicability:
*     WcsMap
*        All WcsMaps have this attribute.
*att--
*/

/* Index of the latitude or longitude axis. */
MAKE_GET(WcsAxis,int,0,this->wcsaxis[ axis ],2)

/* Copy constructor. */
/* ----------------- */
static void Copy( const AstObject *objin, AstObject *objout, int *status ) {
/*
*  Name:
*     Copy

*  Purpose:
*     Copy constructor for WcsMap objects.

*  Type:
*     Private function.

*  Synopsis:
*     void Copy( const AstObject *objin, AstObject *objout, int *status )

*  Description:
*     This function implements the copy constructor for WcsMap objects.

*  Parameters:
*     objin
*        Pointer to the object to be copied.
*     objout
*        Pointer to the object being constructed.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     void

*  Notes:
*     -  This constructor makes a deep copy, including a copy of the
*     projection parameter values associated with the input WcsMap.
*/


/* Local Variables: */
   AstWcsMap *in;                /* Pointer to input WcsMap */
   AstWcsMap *out;               /* Pointer to output WcsMap */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain pointers to the input and output WcsMaps. */
   in = (AstWcsMap *) objin;
   out = (AstWcsMap *) objout;

/* Allocate memory to hold the projection parameters within the AstPrjPrm
   structure used by WCSLIB. */
   (out->params).p = astMalloc( astSizeOf( (in->params).p ) );
   (out->params).p2 = astMalloc( astSizeOf( (in->params).p2 ) );

/* Copy the projection parameter information. */
   CopyPV( in, out, status );

   return;

}

/* Destructor. */
/* ----------- */
static void Delete( AstObject *obj, int *status ) {
/*
*  Name:
*     Delete

*  Purpose:
*     Destructor for WcsMap objects.

*  Type:
*     Private function.

*  Synopsis:
*     void Delete( AstObject *obj, int *status )

*  Description:
*     This function implements the destructor for WcsMap objects.

*  Parameters:
*     obj
*        Pointer to the object to be deleted.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     void

*  Notes:
*     This function attempts to execute even if the global error status is
*     set.
*/

/* Local Variables: */
   AstWcsMap *this;            /* Pointer to WcsMap */

/* Obtain a pointer to the WcsMap structure. */
   this = (AstWcsMap *) obj;

/* Free the arrays used to store projection parameters. */
   FreePV( this, status );

/* Free memory used to hold the projection parameters within the AstPrjPrm
   structure used by WCSLIB. */
   this->params.p = astFree( this->params.p );
   this->params.p2 = astFree( this->params.p2 );

}

/* Dump function. */
/* -------------- */
static void Dump( AstObject *this_object, AstChannel *channel, int *status ) {
/*
*  Name:
*     Dump

*  Purpose:
*     Dump function for WcsMap objects.

*  Type:
*     Private function.

*  Synopsis:
*     void Dump( AstObject *this, AstChannel *channel, int *status )

*  Description:
*     This function implements the Dump function which writes out data
*     for the WcsMap class to an output Channel.

*  Parameters:
*     this
*        Pointer to the WcsMap whose data are being written.
*     channel
*        Pointer to the Channel to which the data are being written.
*     status
*        Pointer to the inherited status variable.
*/

#define COMMENT_LEN 150          /* Maximum length of a keyword comment */
#define KEY_LEN 50               /* Maximum length of a keyword */

/* Local Variables: */
   AstWcsMap *this;              /* Pointer to the WcsMap structure */
   char *comment;                /* Pointer to comment string */
   char buff[ KEY_LEN + 1 ];     /* Buffer for keyword string */
   char comment_buff[ COMMENT_LEN + 1 ]; /* Buffer for keyword comment */
   const PrjData *prjdata;       /* Information about the projection */
   double dval;                  /* Double precision value */
   int axis;                     /* Zero based axis index */
   int i;                        /* Axis index */
   int m;                        /* Parameter index */
   int ival;                     /* Integer value */
   int set;                      /* Attribute value set? */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain a pointer to the WcsMap structure. */
   this = (AstWcsMap *) this_object;

/* Write out values representing the instance variables for the
   WcsMap class.  Accompany these with appropriate comment strings,
   possibly depending on the values being written.*/

/* In the case of attributes, we first use the appropriate (private)
   Test...  member function to see if they are set. If so, we then use
   the (private) Get... function to obtain the value to be written
   out. Note, all read-only attributes are considered to be set.

   For attributes which are not set, we use the astGet... method to
   obtain the value instead. This will supply a default value
   (possibly provided by a derived class which over-rides this method)
   which is more useful to a human reader as it corresponds to the
   actual default attribute value.  Since "set" will be zero, these
   values are for information only and will not be read back. */

/* WcsType. */
/* -------- */
   ival = GetWcsType( this, status );
   prjdata = FindPrjData( ival, status );
   (void) sprintf( comment_buff, "%s projection", prjdata->desc );
   comment_buff[ 0 ] = toupper( comment_buff[ 0 ] );
   astWriteString( channel, "Type", 1, 1, prjdata->ctype + 1, comment_buff );

/* FITSProj */
/* -------- */
   set = TestFITSProj( this, status );
   ival = set ? GetFITSProj( this, status ) : astGetFITSProj( this );
   astWriteInt( channel, "FitsPrj", set, 0, ival,
                ival ? "Defines the FITS-WCS projection" :
                       "Does not define the FITS-WCS projection" );

/* TPNTan */
/* ------ */
   set = TestTPNTan( this, status );
   ival = set ? GetTPNTan( this, status ) : astGetTPNTan( this );
   astWriteInt( channel, "TpnTan", set, 0, ival,
                ival ? "Include TAN projection in TPN mapping" :
                       "Exclude TAN projection from TPN mapping" );

/* PVi_m. */
/* ------ */
   for( i = 0; i < astGetNin( this ); i++ ){
      if( this->np ) {
         for( m = 0; m < this->np[ i ]; m++ ){
            set = TestPV( this, i, m, status );
            if( set ) {
               dval = set ? GetPV( this, i, m, status ) : astGetPV( this, i, m );
               (void) sprintf( buff, "PV%d_%d", i + 1, m );
               (void) sprintf( comment_buff, "Projection parameter %d for axis %d", m, i + 1 );
               astWriteDouble( channel, buff, set, 0, dval, comment_buff );
            }
         }
      }
   }

/* WcsAxis(axis). */
/* -------------- */
   for( axis = 0; axis < 2; axis++ ){
      ival =  GetWcsAxis( this, axis, status );
      (void) sprintf( buff, "WcsAx%d", axis + 1 );
      if( axis == 0 ) {
         comment = "Index of celestial longitude axis";
      } else {
         comment = "Index of celestial latitude axis";
      }
      astWriteInt( channel, buff, (axis!=ival), 0, ival + 1, comment );
   }

/* Note, the "params" component of the AstWcsMap structure is not written out
   because it can be re-generated from the other components. */

/* Return. */
   return;

/* Undefine macros local to this function. */
#undef COMMENT_LEN
#undef KEY_LEN
}

/* Standard class functions. */
/* ========================= */
/* Implement the astIsAWcsMap and astCheckWcsMap functions using the macros
   defined for this purpose in the "object.h" header file. */
astMAKE_ISA(WcsMap,Mapping)
astMAKE_CHECK(WcsMap)

AstWcsMap *astWcsMap_( int ncoord, int type, int lonax, int latax,
                       const char *options, int *status, ...){
/*
*++
*  Name:
c     astWcsMap
f     AST_WCSMAP

*  Purpose:
*     Create a WcsMap.

*  Type:
*     Public function.

*  Synopsis:
c     #include "wcsmap.h"
c     AstWcsMap *astWcsMap( int ncoord, int type, int lonax, int latax,
c                           const char *options, ... )
f     RESULT = AST_WCSMAP( NCOORD, TYPE, LONAX, LATAX, OPTIONS, STATUS )

*  Class Membership:
*     WcsMap constructor.

*  Description:
*     This function creates a new WcsMap and optionally initialises its
*     attributes.
*
*     A WcsMap is used to represent sky coordinate projections as
*     described in the (draft) FITS world coordinate system (FITS-WCS)
*     paper by E.W. Griesen and M. Calabretta (A & A, in preparation).
*     This paper defines a set of functions, or sky projections, which
*     transform longitude-latitude pairs representing spherical
*     celestial coordinates into corresponding pairs of Cartesian
*     coordinates (and vice versa).
*
*     A WcsMap is a specialised form of Mapping which implements these
*     sky projections and applies them to a specified pair of coordinates.
*     All the projections in the FITS-WCS paper are supported, plus the now
*     deprecated "TAN with polynomial correction terms" projection which
*     is refered to here by the code "TPN". Using the FITS-WCS terminology,
*     the transformation is between "native spherical" and "projection
*     plane" coordinates.  These coordinates may, optionally, be embedded in
*     a space with more than two dimensions, the remaining coordinates being
*     copied unchanged. Note, however, that for consistency with other AST
*     facilities, a WcsMap handles coordinates that represent angles
*     in radians (rather than the degrees used by FITS-WCS).
*
*     The type of FITS-WCS projection to be used and the coordinates
*     (axes) to which it applies are specified when a WcsMap is first
*     created. The projection type may subsequently be determined
*     using the WcsType attribute and the coordinates on which it acts
*     may be determined using the WcsAxis(lonlat) attribute.
*
*     Each WcsMap also allows up to 100 "projection parameters" to be
*     associated with each axis. These specify the precise form of the
*     projection, and are accessed using PVi_m attribute, where "i" is
*     the integer axis index (starting at 1), and m is an integer
*     "parameter index" in the range 0 to 99. The number of projection
*     parameters required by each projection, and their meanings, are
*     dependent upon the projection type (most projections either do not
*     use any projection parameters, or use parameters 1 and 2 associated
*     with the latitude axis). Before creating a WcsMap you should consult
*     the FITS-WCS paper for details of which projection parameters are
*     required, and which have defaults. When creating the WcsMap, you must
*     explicitly set values for all those required projection parameters
*     which do not have defaults defined in this paper.

*  Parameters:
c     ncoord
f     NCOORD = INTEGER (Given)
*        The number of coordinate values for each point to be
*        transformed (i.e. the number of dimensions of the space in
*        which the points will reside). This must be at least 2. The
*        same number is applicable to both input and output points.
c     type
f     TYPE = INTEGER (Given)
*        The type of FITS-WCS projection to apply. This should be
c        given using a macro value such as AST__TAN (for a tangent
f        given as a symbolic value such as AST__TAN (for a tangent
*        plane projection), where the characters following the double
*        underscore give the projection type code (in upper case) as
*        used in the FITS-WCS "CTYPEi" keyword. You should consult the
*        FITS-WCS paper for a list of the available projections. The
*        additional code of AST__TPN can be supplied which represents a
*        TAN projection with polynomial correction terms as defined in an
*        early draft of the FITS-WCS paper.
c     lonax
f     LONAX = INTEGER (Given)
*        The index of the longitude axis. This should lie in the range
c        1 to "ncoord".
f        1 to NCOORD.
c     latax
f     LATAX = INTEGER (Given)
*        The index of the latitude axis. This should lie in the range
c        1 to "ncoord" and be distinct from "lonax".
f        1 to NCOORD and be distinct from LONAX.
c     options
f     OPTIONS = CHARACTER * ( * ) (Given)
c        Pointer to a null-terminated string containing an optional
c        comma-separated list of attribute assignments to be used for
c        initialising the new WcsMap. The syntax used is identical to
c        that for the astSet function and may include "printf" format
c        specifiers identified by "%" symbols in the normal way.
f        A character string containing an optional comma-separated
f        list of attribute assignments to be used for initialising the
f        new WcsMap. The syntax used is identical to that for the
f        AST_SET routine.
*
*        If the sky projection to be implemented requires projection
*        parameter values to be set, then this should normally be done
*        here via the PVi_m attribute (see the "Examples"
*        section). Setting values for these parameters is mandatory if
*        they do not have default values (as defined in the FITS-WCS
*        paper).
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
c     astWcsMap()
f     AST_WCSMAP = INTEGER
*        A pointer to the new WcsMap.

*  Examples:
c     wcsmap = astWcsMap( 2, AST__MER, 1, 2, "" );
f     WCSMAP = AST_WCSMAP( 2, AST__MER, 1, 2, ' ', STATUS )
*        Creates a WcsMap that implements a FITS-WCS Mercator
*        projection on pairs of coordinates, with coordinates 1 and 2
*        representing the longitude and latitude respectively. Note
*        that the FITS-WCS Mercator projection does not require any
*        projection parameters.
c     wcsmap = astWcsMap( 3, AST__COE, 2, 3, "PV3_1=40.0" );
f     WCSMAP = AST_WCSMAP( 3, AST__COE, 2, 3, 'PV3_1=40.0', STATUS )
*        Creates a WcsMap that implements a FITS-WCS conical equal
*        area projection. The WcsMap acts on points in a 3-dimensional
*        space; coordinates 2 and 3 represent longitude and latitude
*        respectively, while the values of coordinate 1 are copied
*        unchanged.  Projection parameter 1 associatyed with the latitude
*        axis (corresponding to FITS keyword "PV3_1") is required and has
*        no default, so is set explicitly to 40.0 degrees. Projection
*        parameter 2 (corresponding to FITS keyword "PV3_2") is required
*        but has a default of zero, so need not be specified.

*  Notes:
*     - The forward transformation of a WcsMap converts between
*     FITS-WCS "native spherical" and "relative physical" coordinates,
*     while the inverse transformation converts in the opposite
*     direction. This arrangement may be reversed, if required, by
c     using astInvert or by setting the Invert attribute to a non-zero
f     using AST_INVERT or by setting the Invert attribute to a non-zero
*     value.
*     - If any set of coordinates cannot be transformed (for example,
*     many projections do not cover the entire celestial sphere), then
*     a WcsMap will yield coordinate values of AST__BAD.
*     - The validity of any projection parameters given via the PVi_m
c     parameter in the "options" string is not checked by this
f     parameter in the OPTIONS string is not checked by this
*     function. However, their validity is checked when the resulting
*     WcsMap is used to transform coordinates, and an error will
*     result if the projection parameters do not satisfy all the
*     required constraints (as defined in the FITS-WCS paper).
*     - A null Object pointer (AST__NULL) will be returned if this
c     function is invoked with the AST error status set, or if it
f     function is invoked with STATUS set to an error value, or if it
*     should fail for any reason.

*  Status Handling:
*     The protected interface to this function includes an extra
*     parameter at the end of the parameter list descirbed above. This
*     parameter is a pointer to the integer inherited status
*     variable: "int *status".

*--
*/

/* Local Variables: */
   astDECLARE_GLOBALS            /* Pointer to thread-specific global data */
   AstWcsMap *new;               /* Pointer to new WcsMap */
   va_list args;                 /* Variable argument list */

/* Check the global status. */
   if ( !astOK ) return NULL;

/* Get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(NULL);

/* Initialise the WcsMap, allocating memory and initialising the
   virtual function table as well if necessary. */
   new = astInitWcsMap( NULL, sizeof( AstWcsMap ), !class_init, &class_vtab,
                        "WcsMap", ncoord, type, lonax - 1, latax - 1 );

/* If successful, note that the virtual function table has been
   initialised. */
   if ( astOK ) {
      class_init = 1;

/* Obtain the variable argument list and pass it along with the options string
   to the astVSet method to initialise the new WcsMap's attributes. */
      va_start( args, status );
      astVSet( new, options, NULL, args );
      va_end( args );

/* If an error occurred, clean up by deleting the new object. */
      if ( !astOK ) new = astDelete( new );
   }

/* Return a pointer to the new WcsMap. */
   return new;
}

AstWcsMap *astWcsMapId_( int ncoord, int type, int lonax, int latax,
                         const char *options, ... ){
/*
*  Name:
*     astWcsMapId_

*  Purpose:
*     Create a WcsMap.

*  Type:
*     Private function.

*  Synopsis:
*     #include "wcsmap.h"
*     AstWcsMap *astWcsMap_( int ncoord, int type, int lonax, int latax,
*                            const char *options, ... )

*  Class Membership:
*     WcsMap constructor.

*  Description:
*     This function implements the external (public) interface to the
*     astWcsMap constructor function. It returns an ID value (instead
*     of a true C pointer) to external users, and must be provided
*     because astWcsMap_ has a variable argument list which cannot be
*     encapsulated in a macro (where this conversion would otherwise
*     occur).
*
*     The variable argument list also prevents this function from
*     invoking astWcsMap_ directly, so it must be a re-implementation
*     of it in all respects, except for the final conversion of the
*     result to an ID value.

*  Parameters:
*     As for astWcsMap_.

*  Returned Value:
*     The ID value associated with the new WcsMap.
*/

/* Local Variables: */
   astDECLARE_GLOBALS            /* Pointer to thread-specific global data */
   AstWcsMap *new;               /* Pointer to new WcsMap */
   va_list args;                 /* Variable argument list */
   int *status;                  /* Pointer to inherited status value */

/* Get a pointer to the inherited status value. */
   status = astGetStatusPtr;

/* Get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(NULL);

/* Check the global status. */
   if ( !astOK ) return NULL;

/* Initialise the WcsMap, allocating memory and initialising the
   virtual function table as well if necessary. */
   new = astInitWcsMap( NULL, sizeof( AstWcsMap ), !class_init, &class_vtab,
                        "WcsMap", ncoord, type, lonax - 1, latax - 1 );

/* If successful, note that the virtual function table has been
   initialised. */
   if ( astOK ) {
      class_init = 1;

/* Obtain the variable argument list and pass it along with the options string
   to the astVSet method to initialise the new WcsMap's attributes. */
      va_start( args, options );
      astVSet( new, options, NULL, args );
      va_end( args );

/* If an error occurred, clean up by deleting the new object. */
      if ( !astOK ) new = astDelete( new );
   }

/* Return an ID value for the new WcsMap. */
   return astMakeId( new );
}

AstWcsMap *astInitWcsMap_( void *mem, size_t size, int init,
                           AstWcsMapVtab *vtab, const char *name,
                           int ncin, int type, int lonax, int latax, int *status ) {
/*
*+
*  Name:
*     astInitWcsMap

*  Purpose:
*     Initialise a WcsMap.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "wcsmap.h"
*     AstWcsMap *astInitWcsMap( void *mem, size_t size, int init,
*                               AstWcsMapVtab *vtab, const char *name,
*                               int ncin, int type, int lonax, int latax )

*  Class Membership:
*     WcsMap initialiser.

*  Description:
*     This function is provided for use by class implementations to initialise
*     a new WcsMap object. It allocates memory (if necessary) to accommodate
*     the WcsMap plus any additional data associated with the derived class.
*     It then initialises a WcsMap structure at the start of this memory. If
*     the "init" flag is set, it also initialises the contents of a virtual
*     function table for a WcsMap at the start of the memory passed via the
*     "vtab" parameter.

*  Parameters:
*     mem
*        A pointer to the memory in which the WcsMap is to be initialised.
*        This must be of sufficient size to accommodate the WcsMap data
*        (sizeof(WcsMap)) plus any data used by the derived class. If a value
*        of NULL is given, this function will allocate the memory itself using
*        the "size" parameter to determine its size.
*     size
*        The amount of memory used by the WcsMap (plus derived class data).
*        This will be used to allocate memory if a value of NULL is given for
*        the "mem" parameter. This value is also stored in the WcsMap
*        structure, so a valid value must be supplied even if not required for
*        allocating memory.
*     init
*        A logical flag indicating if the WcsMap's virtual function table is
*        to be initialised. If this value is non-zero, the virtual function
*        table will be initialised by this function.
*     vtab
*        Pointer to the start of the virtual function table to be associated
*        with the new WcsMap.
*     name
*        Pointer to a constant null-terminated character string which contains
*        the name of the class to which the new object belongs (it is this
*        pointer value that will subsequently be returned by the astGetClass
*        method).
*     ncin
*        The number of coordinate values per point.
*     type
*        The type of projection to use, choosen from the list available
*        in the FITS celestial coordinates system. These are specified by
*        symbolic values such as AST__TAN (specifying a tangent plane
*        projection), where the characters following the double underscore
*        gives the FITS projection type (in upper case) as used in the FITS
*        "CTYPEn" keyword.
*     lonax
*        The index of the longitude axis. The first axis has index 0.
*     latax
*        The index of the latitude axis. The first axis has index 0.

*  Returned Value:
*     A pointer to the new WcsMap.

*  Notes:
*     -  A null pointer will be returned if this function is invoked with the
*     global error status set, or if it should fail for any reason.
*-
*/

/* Local Variables: */
   const PrjData *prjdata;      /* Information about the projection */
   AstWcsMap *new;              /* Pointer to new WcsMap */

/* Check the global status. */
   if ( !astOK ) return NULL;

/* If necessary, initialise the virtual function table. */
   if ( init ) astInitWcsMapVtab( vtab, name );

/* Initialise. */
   new = NULL;

/* If a genuine FITS-WCS projection has been specified, check the initialisation
   value(s) for validity, reporting an error if necessary. Prefix the error
   report with the name of the function and the class of object being
   processed. First check that at least two dimensions are to be mapped. */
   if( type != AST__WCSBAD ){
      if ( ncin < 2 ){
         astError( AST__WCSNC, "astInitWcsMap(%s): Too few axes (%d) "
                   "specified. Must be at least 2.", status, name, ncin );

/* Report an error if either the longitude or latitude axes are out of
   bounds. */
      } else if ( lonax < 0 || lonax >= ncin ){
         astError( AST__WCSAX, "astInitWcsMap(%s): Specified longitude axis (%d) "
                   "does not exist within a %d dimensional coordinate system. ", status,
                   name, lonax + 1, ncin );

      } else if ( latax < 0 || latax >= ncin ){
         astError( AST__WCSAX, "astInitWcsMap(%s): Specified latitude axis (%d) "
                   "does not exist within a %d dimensional coordinate system. ", status,
                   name, latax + 1, ncin );

/* Report an error if the longitude or latitude axes are the same. */
      } else if ( lonax == latax ){
         astError( AST__WCSAX, "astInitWcsMap(%s): The same axis (%d) has been "
                   "given for both the longitude and the latitude axis.", status, name,
                   lonax + 1 );

/* Report an error if projection type is unknown. */
      } else if ( type < 1 || type >= AST__WCSBAD ){
         astError( AST__WCSTY, "astInitWcsMap(%s): Projection type %d is "
                   "undefined. Projection types must be in the range 1 to %d.", status,
                   name, type, AST__WCSBAD - 1 );
      }
   }

/* Get a description of the requeste dprojection type. */
   prjdata = FindPrjData( type, status );

/* If all the above checks have been passed succesfully... */
   if( astOK ){

/* Initialise a Mapping structure (the parent class) as the first component
   within the WcsMap structure, allocating memory if necessary. Specify that
   the Mapping should be defined in both the forward and inverse directions. */
      new = (AstWcsMap *) astInitMapping( mem, size, 0,
                                          (AstMappingVtab *) vtab, name,
                                          ncin, ncin, 1, 1 );

      if ( astOK ) {

/* Initialise the WcsMap data. */
/* ---------------------------- */
/* Store the projection type. */
         new->type = type;

/* Store the "use as FITS-WCS projection" flag. */
         new->fits_proj = -INT_MAX;

/* Store the "include TAN component in TPN Mapping" flag. */
         new->tpn_tan = -INT_MAX;

/* Store the axes associated with longitude and latitude. */
         new->wcsaxis[0] = lonax;
         new->wcsaxis[1] = latax;

/* Store NULL pointers for the arrays holding projection parameters. */
         new->p = NULL;
         new->np = NULL;

/* Allocate memory of the right size to hold the maximum number of
   projection parameters needed by the projection. */
         new->params.p = astMalloc( sizeof( double ) * (prjdata->mxpar + 1) );
         new->params.p2 = astMalloc( sizeof( double ) * (prjdata->mxpar2 + 1) );

/* Initialise the "AstPrjPrm" structure (defined in proj.h). */
         InitPrjPrm( new, status );

/* If an error occurred, clean up by deleting the new WcsMap. */
         if ( !astOK ) new = astDelete( new );
      }
   }

/* Return a pointer to the new WcsMap. */
   return new;
}

AstWcsMap *astLoadWcsMap_( void *mem, size_t size,
                           AstWcsMapVtab *vtab, const char *name,
                           AstChannel *channel, int *status ) {
/*
*+
*  Name:
*     astLoadWcsMap

*  Purpose:
*     Load a WcsMap.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "wcsmap.h"
*     AstWcsMap *astLoadWcsMap( void *mem, size_t size,
*                               AstWcsMapVtab *vtab, const char *name,
*                               AstChannel *channel )

*  Class Membership:
*     WcsMap loader.

*  Description:
*     This function is provided to load a new WcsMap using data read
*     from a Channel. It first loads the data used by the parent class
*     (which allocates memory if necessary) and then initialises a
*     WcsMap structure in this memory, using data read from the input
*     Channel.
*
*     If the "init" flag is set, it also initialises the contents of a
*     virtual function table for a WcsMap at the start of the memory
*     passed via the "vtab" parameter.


*  Parameters:
*     mem
*        A pointer to the memory into which the WcsMap is to be
*        loaded.  This must be of sufficient size to accommodate the
*        WcsMap data (sizeof(WcsMap)) plus any data used by derived
*        classes. If a value of NULL is given, this function will
*        allocate the memory itself using the "size" parameter to
*        determine its size.
*     size
*        The amount of memory used by the WcsMap (plus derived class
*        data).  This will be used to allocate memory if a value of
*        NULL is given for the "mem" parameter. This value is also
*        stored in the WcsMap structure, so a valid value must be
*        supplied even if not required for allocating memory.
*
*        If the "vtab" parameter is NULL, the "size" value is ignored
*        and sizeof(AstWcsMap) is used instead.
*     vtab
*        Pointer to the start of the virtual function table to be
*        associated with the new WcsMap. If this is NULL, a pointer
*        to the (static) virtual function table for the WcsMap class
*        is used instead.
*     name
*        Pointer to a constant null-terminated character string which
*        contains the name of the class to which the new object
*        belongs (it is this pointer value that will subsequently be
*        returned by the astGetClass method).
*
*        If the "vtab" parameter is NULL, the "name" value is ignored
*        and a pointer to the string "WcsMap" is used instead.

*  Returned Value:
*     A pointer to the new WcsMap.

*  Notes:
*     - A null pointer will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*-
*/

#define KEY_LEN 50               /* Maximum length of a keyword */

   astDECLARE_GLOBALS            /* Pointer to thread-specific global data */
/* Local Variables: */
   const PrjData *prjdata;      /* Information about the projection */
   AstWcsMap *new;              /* Pointer to the new WcsMap */
   char *text;                  /* Textual form of an integer value */
   char buff[ KEY_LEN + 1 ];    /* Buffer for keyword string */
   double pv;                   /* Projection parameter */
   int axis;                    /* Axis index */
   int i;                       /* Axis index */
   int m;                       /* Parameter index */
   int mxpar;                   /* Maximum number of PVi_m values */

/* Get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(channel);

/* Initialise. */
   new = NULL;

/* Check the global error status. */
   if ( !astOK ) return new;

/* If a NULL virtual function table has been supplied, then this is
   the first loader to be invoked for this WcsMap. In this case the
   WcsMap belongs to this class, so supply appropriate values to be
   passed to the parent class loader (and its parent, etc.). */
   if ( !vtab ) {
      size = sizeof( AstWcsMap );
      vtab = &class_vtab;
      name = "WcsMap";

/* If required, initialise the virtual function table for this class. */
      if ( !class_init ) {
         astInitWcsMapVtab( vtab, name );
         class_init = 1;
      }
   }

/* Invoke the parent class loader to load data for all the ancestral
   classes of the current one, returning a pointer to the resulting
   partly-built WcsMap. */
   new = astLoadMapping( mem, size, (AstMappingVtab *) vtab, name,
                         channel );

   if ( astOK ) {

/* Read input data. */
/* ================ */
/* Request the input Channel to read all the input data appropriate to
   this class into the internal "values list". */
      astReadClassData( channel, "WcsMap" );

/* Now read each individual data item from this list and use it to
   initialise the appropriate instance variable(s) for this class. */

/* In the case of attributes, we first read the "raw" input value,
   supplying the "unset" value as the default. If a "set" value is
   obtained, we then use the appropriate (private) Set... member
   function to validate and set the value properly. Note, this is only
   done for read/write attributes, not read-only ones. */

/* FITSProj */
/* -------- */
      new->fits_proj = astReadInt( channel, "fitsprj", -INT_MAX );
      if ( TestFITSProj( new, status ) ) {
         SetFITSProj( new, new->fits_proj, status );
      }

/* TPNTan */
/* -------- */
      new->tpn_tan = astReadInt( channel, "tpntan", -INT_MAX );
      if ( TestTPNTan( new, status ) ) {
         SetTPNTan( new, new->tpn_tan, status );
      }

/* WcsType */
/* ------- */
      text = astReadString( channel, "type", " " );
      if( strcmp( text, " " ) ){
         char tmp[ 10 ];
         (void) sprintf( tmp, "-%.8s", text );
         new->type = astWcsPrjType( tmp );
      } else {
         new->type = AST__WCSBAD;
      }
      text = astFree( text );
      prjdata = FindPrjData( new->type, status );

/* WcsAxis(axis). */
/* -------------- */
      for( axis = 0; axis < 2; axis++ ){
         (void) sprintf( buff, "wcsax%d", axis + 1 );
         new->wcsaxis[ axis ] = astReadInt( channel, buff, axis + 1 ) - 1;
      }

/* Initialise the pointers to the projection parameter information. */
      new->p = NULL;
      new->np = NULL;
      new->params.p = astMalloc( sizeof( double ) * (prjdata->mxpar + 1) );
      new->params.p2 = astMalloc( sizeof( double ) * (prjdata->mxpar2 + 1) );

/* Initialise the structure used by WCSLIB to hold intermediate values,
   so that the values will be re-calculated on the first invocation of a
   mapping function. */
      InitPrjPrm( new, status );

/* ProjP(m). */
/* --------- */
      for( m = 0; m < AST__WCSMX; m++ ){
         (void) sprintf( buff, "projp%d", m );
         pv = astReadDouble( channel, buff, AST__BAD );
         if( pv != AST__BAD ) SetPV( new, new->wcsaxis[ 1 ], m, pv, status );
      }

/* PVi_m. */
/* -------*/
      for( i = 0; i < astGetNin( new ); i++ ){

         if( i == new->wcsaxis[ 0 ] ) {
            mxpar = prjdata->mxpar2;
         } else if( i == new->wcsaxis[ 1 ] ) {
            mxpar = prjdata->mxpar;
         } else {
            mxpar = 0;
         }

         for( m = 0; m <= mxpar; m++ ){
            (void) sprintf( buff, "pv%d_%d", i + 1, m );
            pv = astReadDouble( channel, buff, AST__BAD );
            if( pv != AST__BAD ) SetPV( new, i, m, pv, status );
         }
      }

/* If an error occurred, clean up by deleting the new WcsMap. */
      if ( !astOK ) new = astDelete( new );
   }

/* Return the new WcsMap pointer. */
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

void astClearPV_( AstWcsMap *this, int i, int m, int *status ) {
   if ( !astOK ) return;
   (**astMEMBER(this,WcsMap,ClearPV))( this, i, m, status );
}

void astClearTPNTan_( AstWcsMap *this, int *status ) {
   if ( !astOK ) return;
   (**astMEMBER(this,WcsMap,ClearTPNTan))( this, status );
}

double astGetPV_( AstWcsMap *this, int i, int m, int *status ) {
   if ( !astOK ) return AST__BAD;
   return (**astMEMBER(this,WcsMap,GetPV))( this, i, m, status );
}

void astSetPV_( AstWcsMap *this, int i, int m, double val, int *status ) {
   if ( !astOK ) return;
   (**astMEMBER(this,WcsMap,SetPV))( this, i, m, val, status );
}

void astSetTPNTan_( AstWcsMap *this, int val, int *status ) {
   if ( !astOK ) return;
   (**astMEMBER(this,WcsMap,SetTPNTan))( this, val, status );
}

int astTestPV_( AstWcsMap *this, int i, int m, int *status ) {
   if ( !astOK ) return 0;
   return (**astMEMBER(this,WcsMap,TestPV))( this, i, m, status );
}

int astIsZenithal_( AstWcsMap *this, int *status ) {
   if ( !astOK ) return 0;
   return (**astMEMBER(this,WcsMap,IsZenithal))( this, status );
}

double astGetNatLat_( AstWcsMap *this, int *status ) {
   if ( !astOK ) return AST__BAD;
   return (**astMEMBER(this,WcsMap,GetNatLat))( this, status );
}

double astGetNatLon_( AstWcsMap *this, int *status ) {
   if ( !astOK ) return AST__BAD;
   return (**astMEMBER(this,WcsMap,GetNatLon))( this, status );
}

int astGetPVMax_( AstWcsMap *this, int i, int *status ) {
   if ( !astOK ) return -1;
   return (**astMEMBER(this,WcsMap,GetPVMax))( this, i, status );
}







