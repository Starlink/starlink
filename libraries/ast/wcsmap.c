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
*     described in the (draft) FITS world coordinate system (FITS-WCS)
*     paper by E.W. Griesen and M. Calabretta (A & A, in preparation).
*     This paper defines a set of functions, or sky projections, which
*     transform longitude-latitude pairs representing spherical
*     celestial coordinates into corresponding pairs of Cartesian
*     coordinates (and vice versa).
*
*     A WcsMap is a specialised form of Mapping which implements these
*     sky projections (all of those in the FITS-WCS paper are
*     supported) and applies them to a specified pair of coordinates.
*     Using the FITS-WCS terminology, this transformation is between
*     "native spherical" and "relative physical" coordinates.  These
*     coordinates may, optionally, be embedded in a space with more
*     than two dimensions, the remaining coordinates being copied
*     unchanged. Note, however, that for consistency with other AST
*     facilities, a WcsMap handles coordinates that represent angles
*     in radians (rather than the degrees used by FITS-WCS).
*
*     The type of FITS-WCS projection to be used and the coordinates
*     (axes) to which it applies are specified when a WcsMap is first
*     created. The projection type may subsequently be determined
*     using the WcsType attribute and the coordinates on which it acts
*     may be determined using the WcsAxis(lonlat) attribute.
*
*     Each WcsMap also uses up to 10 "projection parameters" which
*     specify the precise form of the projection. These parameter
*     values are accessed using the ProjP(i) attribute, where "i" is
*     an integer in the range 0 to 9. The number of projection
*     parameters used, and their meanings, are dependent upon the
*     projection type (most projections either do not use any
*     projection parameters, or use parameters 1 and 2). Before
*     creating a WcsMap you should consult the FITS-WCS paper for
*     details of which projection parameters are required, and which
*     have defaults. When creating the WcsMap, you must explicitly set
*     values for all those required projection parameters which do not
*     have defaults defined in this paper.

*  Inheritance:
*     The WcsMap class inherits from the Mapping class.

*  Attributes:
*     In addition to those attributes common to all Mappings, every
*     WcsMap also has the following attributes:
*
*     - NatLat: Native latitude of the reference point of a FITS-WCS projection
*     - ProjP(i): FITS-WCS projection parameters
*     - WcsAxis(lonlat): FITS-WCS projection axes
*     - WcsType: FITS-WCS projection type

*  Functions:
c     The WcsMap class does not define any new functions beyond those
f     The WcsMap class does not define any new routines beyond those
*     which are applicable to all Mappings.

*  Copyright:
*     <COPYRIGHT_STATEMENT>

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
*class--
*/

/* Module Macros. */
/* ============== */
/* Set the name of the class we are implementing. This indicates to
   the header files that define class interfaces that they should make
   "protected" symbols available. */
#define astCLASS WcsMap

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
static void Clear##attr( AstWcsMap *this, int axis ) { \
\
/* Check the global error status. */ \
   if ( !astOK ) return; \
\
/* Validate the axis index. */ \
   if( axis < 0 || axis >= nval ){ \
      astError( AST__AXIIN, "%s(%s): Index (%d) is invalid for attribute " \
                #attr " - it should be in the range 1 to %d.", \
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
void astClear##attr##_( AstWcsMap *this, int axis ) { \
\
/* Check the global error status. */ \
   if ( !astOK ) return; \
\
/* Invoke the required method via the virtual function table. */ \
   (**astMEMBER(this,WcsMap,Clear##attr))( this, axis ); \
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
static type Get##attr( AstWcsMap *this, int axis ) { \
   type result;                  /* Result to be returned */ \
\
/* Check the global error status. */ \
   if ( !astOK ) return (bad_value); \
\
/* Validate the axis index. */ \
   if( axis < 0 || axis >= nval ){ \
      astError( AST__AXIIN, "%s(%s): Index (%d) is invalid for attribute " \
                #attr " - it should be in the range 1 to %d.", \
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
type astGet##attr##_( AstWcsMap *this, int axis ) { \
\
/* Check the global error status. */ \
   if ( !astOK ) return (bad_value); \
\
/* Invoke the required method via the virtual function table. */ \
   return (**astMEMBER(this,WcsMap,Get##attr))( this, axis ); \
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
static void Set##attr( AstWcsMap *this, int axis, type value ) { \
\
/* Check the global error status. */ \
   if ( !astOK ) return; \
\
/* Validate the axis index. */ \
   if( axis < 0 || axis >= nval ){ \
      astError( AST__AXIIN, "%s(%s): Index (%d) is invalid for attribute " \
                #attr " - it should be in the range 1 to %d.", \
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
void astSet##attr##_( AstWcsMap *this, int axis, type value ) { \
\
/* Check the global error status. */ \
   if ( !astOK ) return; \
\
/* Invoke the required method via the virtual function table. */ \
   (**astMEMBER(this,WcsMap,Set##attr))( this, axis, value ); \
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
static int Test##attr( AstWcsMap *this, int axis ) { \
   int result;                   /* Value to return */ \
\
/* Check the global error status. */ \
   if ( !astOK ) return 0; \
\
\
/* Validate the axis index. */ \
   if( axis < 0 || axis >= nval ){ \
      astError( AST__AXIIN, "%s(%s): Index (%d) is invalid for attribute " \
                #attr " - it should be in the range 1 to %d.", \
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
int astTest##attr##_( AstWcsMap *this, int axis ) { \
\
/* Check the global error status. */ \
   if ( !astOK ) return 0; \
\
/* Invoke the required method via the virtual function table. */ \
   return (**astMEMBER(this,WcsMap,Test##attr))( this, axis ); \
}


/* Include files. */
/* ============== */
/* Interface definitions. */
/* ---------------------- */
#include "error.h"               /* Error reporting facilities */
#include "memory.h"              /* Memory allocation facilities */
#include "object.h"              /* Base Object class */
#include "pointset.h"            /* Sets of points/coordinates */
#include "mapping.h"             /* Coordinate mappings (parent class) */
#include "unitmap.h"             /* Unit mappings */
#include "permmap.h"             /* Axis permutation mappings */
#include "wcsmap.h"              /* Interface definition for this class */
#include "slalib.h"              /* SLALIB function prototypes */
#include "channel.h"             /* I/O channels */
#include "proj.h"                /* WCSLIB projections */

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

/* Local Type Definitions. */
/* ----------------------- */
/* This structure is used to hold information describing a WCSLIB 
   projection. */
typedef struct PrjData {
   int prj;                     /* WCSLIB projection identifier value */
   char desc[60];               /* Long projection description */
   char ctype[5];               /* FITS CTYPE identifying string */
   int pbase;                   /* First significant paremeter index */
   int pmin;                    /* Lowest usable no. of parameters */
   int pmax;                    /* Highest usable no. of parameters */
   int (* WcsFwd)(double, double, struct prjprm *, double *, double *);
                                /* Pointer to forward projection function */
   int (* WcsRev)(double, double, struct prjprm *, double *, double *);
                                /* Pointer to reverse projection function */
   double theta0;               /* Native latitude of reference point */
} PrjData;

/* Module Variables. */
/* ================= */
/* Define the class virtual function table and its initialisation flag
   as static variables. */
static AstWcsMapVtab class_vtab; /* Virtual function table */
static int class_init = 0;       /* Virtual function table initialised? */

/* Pointers to parent class methods which are extended by this class. */
static AstPointSet *(* parent_transform)( AstMapping *, AstPointSet *, int, AstPointSet * );
static const char *(* parent_getattrib)( AstObject *, const char * );
static int (* parent_testattrib)( AstObject *, const char * );
static void (* parent_clearattrib)( AstObject *, const char * );
static void (* parent_setattrib)( AstObject *, const char * );

/* The following array of PrjData structured describes each of the WCSLIB
   projections. The last entry in the list should be for the AST__WCSBAD 
   projection. This marks the end of the list. */
static PrjData PrjInfo[] = {
   { AST__AZP,  "zenithal perspective", "-AZP",  1,  1,  1, azpfwd, azprev, AST__DPIBY2 },
   { AST__TAN,  "gnomonic", "-TAN",  0,  0,  0, tanfwd, tanrev, AST__DPIBY2 },
   { AST__SIN,  "orthographic", "-SIN",  1,  0,  2, sinfwd, sinrev, AST__DPIBY2 },
   { AST__STG,  "stereographic", "-STG",  0,  0,  0, stgfwd, stgrev, AST__DPIBY2 },
   { AST__ARC,  "zenithal equidistant", "-ARC",  0,  0,  0, arcfwd, arcrev, AST__DPIBY2 },
   { AST__ZPN,  "zenithal polynomial", "-ZPN",  0,  0, 10, zpnfwd, zpnrev, AST__DPIBY2 },
   { AST__ZEA,  "zenithal equal area", "-ZEA",  0,  0,  0, zeafwd, zearev, AST__DPIBY2 },
   { AST__AIR,  "Airy", "-AIR",  1,  1,  1, airfwd, airrev, AST__DPIBY2 },
   { AST__CYP,  "cylindrical perspective", "-CYP",  1,  2,  2, cypfwd, cyprev, 0.0 },
   { AST__CAR,  "Cartesian", "-CAR",  0,  0,  0, carfwd, carrev, 0.0 },
   { AST__MER,  "Mercator", "-MER",  0,  0,  0, merfwd, merrev, 0.0 },
   { AST__CEA,  "cylindrical equal area", "-CEA",  1,  1,  1, ceafwd, cearev, 0.0 },
   { AST__COP,  "conical perspective", "-COP",  1,  1,  2, copfwd, coprev, AST__BAD },
   { AST__COD,  "conical equidistant", "-COD",  1,  1,  2, codfwd, codrev, AST__BAD },
   { AST__COE,  "conical equal area", "-COE",  1,  1,  2, coefwd, coerev, AST__BAD },
   { AST__COO,  "conical orthomorphic", "-COO",  1,  1,  2, coofwd, coorev, AST__BAD },
   { AST__BON,  "Bonne's equal area", "-BON",  1,  1,  1, bonfwd, bonrev, 0.0 },
   { AST__PCO,  "polyconic", "-PCO",  0,  0,  0, pcofwd, pcorev, 0.0 },
   { AST__GLS,  "sinusoidal", "-GLS",  0,  0,  0, glsfwd, glsrev, 0.0 },
   { AST__PAR,  "parabolic", "-PAR",  0,  0,  0, parfwd, parrev, 0.0 },
   { AST__AIT,  "Hammer-Aitoff", "-AIT",  0,  0,  0, aitfwd, aitrev, 0.0 },
   { AST__MOL,  "Mollweide", "-MOL",  0,  0,  0, molfwd, molrev, 0.0 },
   { AST__CSC,  "cobe quadrilateralized spherical cube", "-CSC",  0,  0,  0, cscfwd, cscrev, 0.0 },
   { AST__QSC,  "quadrilateralized spherical cube", "-QSC",  0,  0,  0, qscfwd, qscrev, 0.0 },
   { AST__TSC,  "tangential spherical cube", "-TSC",  0,  0,  0, tscfwd, tscrev, 0.0 },
   { AST__NCP,  "AIPS north celestial pole", "-NCP",  0,  0,  0,   NULL,   NULL, 0.0 },
   { AST__WCSBAD, "<null>",   "    ",  0,  0,  0,   NULL,   NULL, 0.0 } };

/* External Interface Function Prototypes. */
/* ======================================= */
/* The following functions have public prototypes only (i.e. no
   protected prototypes), so we must provide local prototypes for use
   within this module. */
AstWcsMap *astWcsMapId_( int, int, int, int, const char *options, ... );

/* Prototypes for Private Member Functions. */
/* ======================================== */
static double GetProjP( AstWcsMap *, int );
static int TestProjP( AstWcsMap *, int );
static void ClearProjP( AstWcsMap *, int );
static void SetProjP( AstWcsMap *, int, double );

static int GetWcsType( AstWcsMap * );
static double GetNatLat( AstWcsMap * );
static int GetWcsAxis( AstWcsMap *, int );

static AstPointSet *Transform( AstMapping *, AstPointSet *, int, AstPointSet * );
static const PrjData *FindPrjData( int );
static const char *GetAttrib( AstObject *, const char * );
static int CanMerge( AstMapping *, int, AstMapping *, int );
static int CanSwap( AstMapping *, AstMapping *, int, int );
static int Map( AstWcsMap *, int, int, double *, double *, double *, double *);
static int MapMerge( AstMapping *, int, int, int *, AstMapping ***, int ** );
static int TestAttrib( AstObject *, const char * );
static void ClearAttrib( AstObject *, const char * );
static void Dump( AstObject *, AstChannel * );
static void InitVtab( AstWcsMapVtab * );
static void LongRange( const PrjData *, struct prjprm *, double *, double *);
static void PermGet( AstPermMap *, int **, int **, double ** );
static void SetAttrib( AstObject *, const char * );
static void WcsPerm( AstMapping **, int *, int );

/* Member functions. */
/* ================= */
static int CanMerge( AstMapping *map1, int inv1, AstMapping *map2, int inv2 ){
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
*     int CanMerge( AstMapping *map1, int inv1, AstMapping *map2, int inv2 )

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

*  Returned Value:
*     1 if the WcsMaps can be merged, zero otherwise.

*/

/* Local Variables: */
   AstWcsMap *wcs1;               /* Pointer to first WcsMap */
   AstWcsMap *wcs2;               /* Pointer to second WcsMap */
   int ip;                        /* Projection parameter index */
   int ret;                       /* Can the Mappings be merged? */

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

/* Check that the two WcsMaps performs the same sort of projection. */
       if( astGetWcsType( wcs1 ) == astGetWcsType( wcs2 ) ) {

/* Check that the Mappings are applied in opposite senses. */
          if( inv1 != inv2 ) {

/* Check that all projection parameters are equal. */
            ret = 1;
            for( ip = 0; ip < AST__WCSMX; ip++ ){
               if( astGetProjP( wcs1, ip ) != 
                   astGetProjP( wcs2, ip ) ) {
                  ret = 0;
                  break;
               }
            }
         }
      }
   }

/* Return the answer. */
   return ret;
}

static int CanSwap( AstMapping *map1, AstMapping *map2, int inv1, int inv2 ){
/*
*  Name:
*     CanSwap

*  Purpose:
*     Determine if two Mappings could be swapped.

*  Type:
*     Private function.

*  Synopsis:
*     #include "wcsmap.h"
*     int CanSwap( AstMapping *map1, AstMapping *map2, int inv1, int inv2 )

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
         PermGet( (AstPermMap *) nowcs, &outperm, &inperm, &consts );
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
   links in the PermMap. Get the indices of the longitude and latitude
   axes in the WcsMap */
            lonax = astGetWcsAxis( wcs, 0 );
            latax = astGetWcsAxis( wcs, 1 );

/* If the WcsMap is applied first... */
            if( wcs == (AstWcsMap *) map1 ) {
               if( inperm[ lonax ] < 0 || inperm[ lonax ] >= nout ||
                   inperm[ latax ] < 0 || inperm[ latax ] >= nout ) {
                  ret = 0;
               } 

/* If the WcsMap is applied second ... */
            } else {
               if( outperm[ lonax ] < 0 || outperm[ lonax ] >= nin ||
                   outperm[ latax ] < 0 || outperm[ latax ] >= nin ) {
                  ret = 0;
               } 
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

static void ClearAttrib( AstObject *this_object, const char *attrib ) {
/*
*  Name:
*     ClearAttrib

*  Purpose:
*     Clear an attribute value for a WcsMap.

*  Type:
*     Private function.

*  Synopsis:
*     #include "wcsmap.h"
*     void ClearAttrib( AstObject *this, const char *attrib )

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
*/

/* Local Variables: */
   AstWcsMap *this;             /* Pointer to the WcsMap structure */
   int i;                       /* Attribute index */
   int len;                     /* Length of the attribute name */
   int nc;                      /* No. of characters read by sscanf */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain a pointer to the WcsMap structure. */
   this = (AstWcsMap *) this_object;

/* Check the attribute name and clear the appropriate attribute. */

/* ProjP. */
/* ------ */
   if ( nc = 0, ( 1 == sscanf( attrib, "prpjp(%d)%n", &i, &nc ) )
                  && ( nc >= len ) ) {
      astClearProjP( this, i );

/* If the name was not recognised, test if it matches any of the
   read-only attributes of this class. If it does, then report an
   error. */
   } else if ( ( nc = 0, ( 1 == sscanf( attrib, "wcsaxis(%d)%n", &i, &nc ) )
                           && ( nc >= len ) ) ||
        !strcmp( attrib, "wcstype" ) ||
        !strcmp( attrib, "natlat" ) ){
      astError( AST__NOWRT, "astClear: Invalid attempt to clear the \"%s\" "
                "value for a %s.", attrib, astGetClass( this ) );
      astError( AST__NOWRT, "This is a read-only attribute." );

/* If the attribute is still not recognised, pass it on to the parent
   method for further interpretation. */
   } else {
      (*parent_clearattrib)( this_object, attrib );
   }
}

static const PrjData *FindPrjData( int type ){
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
*     const PrjData *FindPrjData( int type )

*  Class Membership:
*     WcsMap member function

*  Description:
*     This function returns a pointer to an PrjData structure describing 
*     the WCSLIB projection with the supplied type. 

*  Parameters:
*     type
*        The projection type.

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

static const char *GetAttrib( AstObject *this_object, const char *attrib ) {
/*
*  Name:
*     GetAttrib

*  Purpose:
*     Get the value of a specified attribute for a WcsMap.

*  Type:
*     Private function.

*  Synopsis:
*     #include "wcsmap.h"
*     const char *GetAttrib( AstObject *this, const char *attrib )

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

/* Local Constants: */
#define BUFF_LEN 50              /* Max. characters in result buffer */

/* Local Variables: */
   AstWcsMap *this;             /* Pointer to the WcsMap structure */
   const char *result;          /* Pointer value to return */
   double dval;                 /* Floating point attribute value */
   int i;                       /* Attribute index */
   int ival;                    /* Integer attribute value */
   int len;                     /* Length of attribute string */
   int nc;                      /* No. of characters read by sscanf */
   static char buff[ BUFF_LEN + 1 ]; /* Buffer for string result */

/* Initialise. */
   result = NULL;

/* Check the global error status. */   
   if ( !astOK ) return result;

/* Obtain a pointer to the WcsMap structure. */
   this = (AstWcsMap *) this_object;

/* Obtain the length of the attrib string. */
   len = strlen( attrib );

/* Compare "attrib" with each recognised attribute name in turn,
   obtaining the value of the required attribute. If necessary, write
   the value into "buff" as a null-terminated string in an appropriate
   format.  Set "result" to point at the result string. */

/* ProjP. */
/* ------ */
   if ( nc = 0, ( 1 == sscanf( attrib, "projp(%d)%n", &i, &nc ) )
                  && ( nc >= len ) ) {
      dval = astGetProjP( this, i );
      if ( astOK ) {
         (void) sprintf( buff, "%.*g", DBL_DIG, dval );
         result = buff;
      }

/* WcsType */
/* ======= */
   } else if ( !strcmp( attrib, "wcstype" ) ) {
      ival = astGetWcsType( this );
      if ( astOK ) {
         (void) sprintf( buff, "%d", ival );
         result = buff;
      }

/* NatLat */
/* ====== */
   } else if ( !strcmp( attrib, "natlat" ) ) {
      dval = astGetNatLat( this );
      if ( astOK ) {
         (void) sprintf( buff, "%.*g", DBL_DIG, dval );
         result = buff;
      }


/* WcsAxis */
/* ======= */
   } else if ( nc = 0, ( 1 == sscanf( attrib, "wcsaxis(%d)%n", &i, &nc ) )
                         && ( nc >= len ) ) {
      ival = astGetWcsAxis( this, i - 1 ) + 1;
      if ( astOK ) {
         (void) sprintf( buff, "%d", ival );
         result = buff;
      }

/* If the attribute name was not recognised, pass it on to the parent
   method for further interpretation. */
   } else {
      result = (*parent_getattrib)( this_object, attrib );
   }

/* Return the result. */
   return result;

/* Undefine macros local to this function. */
#undef BUFF_LEN
}

static void InitVtab( AstWcsMapVtab *vtab ) {
/*
*  Name:
*     InitVtab

*  Purpose:
*     Initialise a virtual function table for a WcsMap.

*  Type:
*     Private function.

*  Synopsis:
*     #include "wcsmap.h"
*     void InitVtab( AstWcsMapVtab *vtab )

*  Class Membership:
*     WcsMap member function.

*  Description:
*     This function initialises the component of a virtual function
*     table which is used by the WcsMap class.

*  Parameters:
*     vtab
*        Pointer to the virtual function table. The components used by
*        all ancestral classes should already have been initialised.
*/

/* Local Variables: */
   AstObjectVtab *object;        /* Pointer to Object component of Vtab */
   AstMappingVtab *mapping;      /* Pointer to Mapping component of Vtab */

/* Check the local error status. */
   if ( !astOK ) return;

/* Store a unique "magic" value in the virtual function table. This
   will be used (by astIsAWcsMap) to determine if an object belongs
   to this class.  We can conveniently use the address of the (static)
   class_init variable to generate this unique value. */
   vtab->check = &class_init;

/* Initialise member function pointers. */
/* ------------------------------------ */
/* Store pointers to the member functions (implemented here) that provide
   virtual methods for this class. */
   vtab->ClearProjP = ClearProjP;
   vtab->GetNatLat = GetNatLat;
   vtab->GetProjP = GetProjP;
   vtab->GetWcsAxis = GetWcsAxis;
   vtab->GetWcsType = GetWcsType;
   vtab->SetProjP = SetProjP;
   vtab->TestProjP = TestProjP;

/* Save the inherited pointers to methods that will be extended, and
   replace them with pointers to the new member functions. */
   object = (AstObjectVtab *) vtab;
   mapping = (AstMappingVtab *) vtab;

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

/* Store replacement pointers for methods which will be over-ridden by
   new member functions implemented here. */
   mapping->MapMerge = MapMerge;

/* Declare the class dump function. There is no copy constructor or
   destructor. */
   astSetDump( vtab, Dump, "WcsMap", "FITS-WCS sky projection" );
}

static void LongRange( const PrjData *prjdata, struct prjprm *params,
                       double *high, double *low ){
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
*     void LongRange( const PrjData *prjdata, struct prjprm *params,
*                     double *high, double *low )

*  Class Membership:
*     WcsMap internal utility function.

*  Description:
*     This function uses the WCSLIB library to transform the supplied input 
*     positions.

*  Parameters:
*     prjdata
*        A pointer to information about the mapping.
*     params
*        Pointer to a WCSLIB "prjprm" structure containing the projection 
*        parameters, etc.
*     high
*        A pointer to a location at which is returned the upper bound of
*        the primary longitude range.
*     low
*        A pointer to a location at which is returned the lower bound of
*        the primary longitude range.

*  Returned Value:
*     None.

*/


/* Local Variables: */
   int point;                    /* Loop counter for points */
   static double xx[ 4 ] = { -1.0E-6,    0.0, 1.0E-6,     0.0 };
   static double yy[ 4 ] = {     0.0, 1.0E-6,    0.0, -1.0E-6 };
   double aa;
   double bb;

/* Initialise the returned values. */
   *high = 180.0;
   *low = -180.0;

/* Check the global error status. */
   if ( !astOK ) return;

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

/* Return. */
   return;

}

static int Map( AstWcsMap *this, int forward, int npoint, double *in0, 
                double *in1, double *out0, double *out1 ){
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
*     static int Map( AstWcsMap *this, int forward, int npoint, double *in0, 
*                     double *in1, double *out0, double *out1 )

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
*                       3 - Incorrect number of projection parameters.
*                       4 - Error existed on entry

*  Notes:
*     -  This function does not report any errors. Reporting of suitable
*     error messages is the responsibility of the calling function. 
*     -  The value 4 will be returned if this function is invoked with the
*     global error status set. 
*
*/

/* Local Variables: */
   struct prjprm *params;        /* Pointer to structure holding WCSLIB info */
   const PrjData *prjdata;       /* Information about the projection */
   double latitude;              /* Latitude value in degrees */
   double longitude;             /* Longitude value in degrees */
   double longhi;                /* Upper longitude limit in degrees */
   double longlo;                /* Lower longitude limit in degrees */
   double x;                     /* X Cartesian coordinate in degrees */
   double y;                     /* Y Cartesian coordinate in degrees */
   int i;                        /* Parameter index */
   int point;                    /* Loop counter for points */
   int wcs_status;               /* Status from WCSLIB functions */
   int type;                     /* Projection type */

/* Check the global error status. */
   if ( !astOK ) return 4;

/* Store the projection type. */
   type = astGetWcsType( this );

/* Get information about the projection. */
   prjdata = FindPrjData( type );

/* Return if there are no WcsLib mapping functons associated with the
   projection. */
   if( ( !prjdata->WcsFwd && forward ) ||
       ( !prjdata->WcsRev && !forward ) ) return 1;

/* First verify that a usable number of projection parameters
   have been supplied. */
   for( i = prjdata->pbase; i < prjdata->pbase + prjdata->pmin - 1; i++ ){
      if( !astTestProjP( this, i ) ) return 3;
   }

/* Check that no extra projection parameters have been supplied which are
   are needed. */
   for( i = prjdata->pbase + prjdata->pmax; i < AST__WCSMX; i++ ){
      if( astTestProjP( this, i ) ) return 3;
   }

/* If we are doing a reverse mapping, get the acceptable range of longitude
   values. */
   params = &(this->params);
   if( !forward ) LongRange( prjdata, params, &longhi, &longlo );
      
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
   radians (as used by SLALIB). Convert them to degrees ensuring that 
   the longitude value is in the range [-180,180] and the latitude is 
   in the range [-90,90] (as required by the WCSLIB library). Any point with
   a latitude outside the range [-90,90] is converted to the equivalent
   point on the complementary meridian. */
            latitude = AST__DR2D*slaDrange(  in1[ point ] );  
            if ( latitude > 90.0 ){
               latitude = 180.0 - latitude;
               longitude = AST__DR2D*slaDrange( PI + in0[ point ] );

            } else if ( latitude < -90.0 ){
               latitude = -180.0 - latitude;
               longitude = AST__DR2D*slaDrange( PI + in0[ point ] );

            } else {
               longitude = AST__DR2D*slaDrange( in0[ point ] );
            }

/* Call the relevant WCSLIB forward projection function. */
            wcs_status = prjdata->WcsFwd( longitude, latitude, params, &x, &y );

/* Store the returned Cartesian coordinates, converting them from degrees 
   to radians. If the position could not be projected, use the value
   AST__BAD. */
            if( wcs_status == 0 ){
               out0[ point ] = AST__DD2R*x; 
               out1[ point ] = AST__DD2R*y; 

            } else {
               out0[ point ] = AST__BAD; 
               out1[ point ] = AST__BAD; 
            }

/* Now deal with reverse projection calls */
         } else {

/* Convert the supplied Cartesian coordinates from radians to degrees. */
            x = AST__DR2D*in0[ point ];
            y = AST__DR2D*in1[ point ];

/* Call the relevant WCSLIB reverse projection function. */
            wcs_status = prjdata->WcsRev( x, y, params, &longitude, &latitude ); 

/* Store the returned longitude and latitude, converting them from degrees 
   to radians. Many projections (ARC, AIT, ZPN, etc) are not cyclic (i.e.
   [long,lat]=[0,0] does not get mapped to the same place as 
   [long,lat]=[360,0] ). Only accept values in the primary longitude or 
   latitude ranges. This avoids (x,y) points outside the physical domain 
   of the mapping being assigned valid (long,lat) values. */
            if( wcs_status == 0 ){
               if( longitude <= longhi && longitude >= longlo &&
                   fabs( latitude ) <= 90.0 ){
                  out0[ point ] = AST__DD2R*longitude; 
                  out1[ point ] = AST__DD2R*latitude; 

               } else {
                  out0[ point ] = AST__BAD; 
                  out1[ point ] = AST__BAD; 
               }

/* If the position could not be projected, use the value AST__BAD. */
            } else {
               out0[ point ] = AST__BAD; 
               out1[ point ] = AST__BAD; 
            }

         }

/* If the projection parameters were bad, return with the status set to 2. */
         if ( wcs_status == 1 ) return 2;
      }
   }      

   return 0;
}

static int MapMerge( AstMapping *this, int where, int series, int *nmap,
                     AstMapping ***map_list, int **invert_list ) {
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
*                   AstMapping ***map_list, int **invert_list )

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
   AstMapping *neighbour; /* Pointer to neighbouring Mapping */
   const char *nclass;   /* Pointer to neighbouring Mapping class */
   const char *class1;   /* Pointer to first Mapping class string */
   const char *class2;   /* Pointer to second Mapping class string */
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

/* Get the number of axes for the WcsMap. */
   nin = astGetNin( ( *map_list )[ where ] );

/* First of all, see if the WcsMap can be replaced by a simpler Mapping,
   without reference to the neighbouring Mappings in the list.           */
/* ======================================================================*/
/* WcsMaps with map types of AST__WCSBAD or AST__CAR are equivalent to 
   UnitMap. */
   type = astGetWcsType( this );
   if( type == AST__WCSBAD || type == AST__CAR ){

/* Annul the WcsMap pointer in the list and replace it with a UnitMap
   pointer, and indicate that the forward transformation of the returned
   UnitMap should be used. */
      (void) astAnnul( ( *map_list )[ where ] );
      ( *map_list )[ where ] = (AstMapping *) astUnitMap( nin, "" );
      ( *invert_list )[ where ] = 0;

/* Return the index of the first modified element. */
      result = where;

/* If the WcsMap itself could not be simplified, see if it can be merged
   with the Mappings on either side of it in the list. This can only be
   done in series for a WcsMap. */
/* ===================================================================== */
   } else if( series ) {

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
         neighbour = ( *map_list )[ i1 ];
         merge = CanMerge( ( *map_list )[ i1 ], (* invert_list)[ i1 ],
                           ( *map_list )[ i2 ], (* invert_list)[ i2 ] );
      }

/* If the WcsMap can not be merged with its lower neighbour, check its
   upper neighbour (if any) in the same way. */
      if( !merge && where < *nmap - 1 ) {
         i1 = where;
         i2 = where + 1;
         neighbour = ( *map_list )[ i2 ];
         merge = CanMerge( ( *map_list )[ i1 ], (* invert_list)[ i1 ],
                           ( *map_list )[ i2 ], (* invert_list)[ i2 ] );
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
         ( *map_list )[ i1 ] = (AstMapping *) astUnitMap( nin, "" );
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

/* Set a flag if we could swap the WcsMap with its higher neighbour. */
         if( where + 1 < *nmap ){
            swaphi = CanSwap(  ( *map_list )[ where ], 
                               ( *map_list )[ where + 1 ],
                               ( *invert_list )[ where ], 
                               ( *invert_list )[ where + 1 ] );
         } else {
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
                             ( *map_list )[ where ], ( *invert_list )[ where ]  ) ) {
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
                               ( *invert_list )[ where ] );
         } else {
            swaplo = 0;
         }

         nstep1 = -1;
         if( swaplo ){
            for( i1 = where - 1; i1 >= 0; i1-- ){

               if( CanMerge( ( *map_list )[ i1 ], ( *invert_list )[ i1 ],
                             ( *map_list )[ where ], ( *invert_list )[ where ]  ) ) {
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
         if( nstep1 != -1 && ( nstep2 == -1 || nstep2 > nstep1 ) ){
            nclass = class1;
            i1 = where - 1;
            i2 = where;
         } else if( nstep2 != -1 ){
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

            WcsPerm( (*map_list) + i1, (*invert_list) + i1, where - i1 );

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
            WcsPerm( mc, ic, where - i1 );

/* If neither of the swapped Mappings can be simplified further, then there
   is no point in swapping the Mappings, so just annul the map copies. */
            if( astGetClass( astSimplify( mc[0] ) ) == 
                astGetClass( mc[0] ) &&
                astGetClass( astSimplify( mc[1] ) ) == 
                astGetClass( mc[1] ) ) {

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
         }
      }
   }

/* Return the result. */
   return result;
}

static void PermGet( AstPermMap *map, int **outperm, int **inperm, 
                     double **consts ){
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
*                   double **const )

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
   pset1 = astPointSet( 2, nin, "" );
   pset2 = astPointSet( 2, nout, "" );

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

static void SetAttrib( AstObject *this_object, const char *setting ) {
/*
*  Name:
*     astSetAttrib

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
   int nc;                       /* Number of characters read by sscanf */
   int i;                        /* Projection parameter number */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain a pointer to the WcsMap structure. */
   this = (AstWcsMap *) this_object;

/* Obtain the length of the setting string. */
   len = (int) strlen( setting );

/* Test for each recognised attribute in turn, using "sscanf" to parse
   the setting string and extract the attribute value (or an offset to
   it in the case of string values). In each case, use the value set
   in "nc" to check that the entire string was matched. Once a value
   has been obtained, use the appropriate method to set it. */

/* ProjP(i). */
/* --------- */
   if ( nc = 0, ( 2 == sscanf( setting, "projp(%d)= %lg %n", &i, &dval, &nc ) ) 
                  && ( nc >= len ) ) {
      astSetProjP( this, i, dval );

/* Define macros to see if the setting string matches any of the
   read-only attributes of this class. */
#define MATCH(attrib) \
        ( nc = 0, ( 0 == sscanf( setting, attrib "=%*[^\n]%n", &nc ) ) && \
                  ( nc >= len ) )

#define MATCH2(attrib) \
        ( nc = 0, ( 1 == sscanf( setting, attrib "(%d)=%*[^\n]%n", &i, &nc ) ) && \
                  ( nc >= len ) )

/* If the attribute was not recognised, use this macro to report an error
   if a read-only attribute has been specified. */
   } else if ( MATCH( "wcstype" ) ||
        MATCH( "natlat" ) ||
        MATCH2( "wcsaxis" ) ) {
      astError( AST__NOWRT, "astSet: The setting \"%s\" is invalid for a %s.",
                setting, astGetClass( this ) );
      astError( AST__NOWRT, "This is a read-only attribute." );

/* If the attribute is still not recognised, pass it on to the parent
   method for further interpretation. */
   } else {
      (*parent_setattrib)( this_object, setting );
   }

/* Undefine macros local to this function. */
#undef MATCH
#undef MATCH2
}

static int TestAttrib( AstObject *this_object, const char *attrib ) {
/*
*  Name:
*     TestAttrib

*  Purpose:
*     Test if a specified attribute value is set for a WcsMap.

*  Type:
*     Private function.

*  Synopsis:
*     #include "wcsmap.h"
*     int TestAttrib( AstObject *this, const char *attrib )

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

*  Returned Value:
*     One if a value has been set, otherwise zero.

*  Notes:
*     - A value of zero will be returned if this function is invoked
*     with the global status set, or if it should fail for any reason.
*/

/* Local Variables: */
   AstWcsMap *this;             /* Pointer to the WcsMap structure */
   int i;                       /* Projection parameter index */
   int len;                     /* Length os supplied string */
   int nc;                      /* No. of characters read by sscanf */
   int result;                  /* Result value to return */

/* Initialise. */
   result = 0;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Obtain a pointer to the WcsMap structure. */
   this = (AstWcsMap *) this_object;

/* Check the attribute name and test the appropriate attribute. */


/* ProjP(i). */
/* --------- */
   if ( nc = 0, ( 1 == sscanf( attrib, "projp(%d)%n", &i, &nc ) )
                  && ( nc >= len ) ) {
      result = astTestProjP( this, i );

/* If the name is not recognised, test if it matches any of the
   read-only attributes of this class. If it does, then return
   zero. */
   } else if ( !strcmp( attrib, "wcstype" ) ||
               !strcmp( attrib, "natlat" ) ||
               ( nc = 0, ( 1 == sscanf( attrib, "wcsaxis(%d)%n", &i, &nc ) )
                           && ( nc >= len ) ) ) {
      result = 0;

/* If the attribute is still not recognised, pass it on to the parent
   method for further interpretation. */
   } else {
      result = (*parent_testattrib)( this_object, attrib );
   }

/* Return the result, */
   return result;
}

static AstPointSet *Transform( AstMapping *this, AstPointSet *in,
                               int forward, AstPointSet *out ) {
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
*                             int forward, AstPointSet *out )

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
   int status;                   /* Status from Map function */

/* Check the global error status. */
   if ( !astOK ) return NULL;

/* Obtain a pointer to the WcsMap. */
   map = (AstWcsMap *) this;

/* Apply the parent mapping using the stored pointer to the Transform member
   function inherited from the parent Mapping class. This function validates
   all arguments and generates an output PointSet if necessary, but does not
   actually transform any coordinate values. */
   result = (*parent_transform)( this, in, forward, out );

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
      status = Map( map, forward, npoint, ptr_in[ lonax ], ptr_in[ latax ], 
                    ptr_out[ lonax ], ptr_out[ latax ] );

/* Report an error if the projection type was unrecognised. */
      if ( status == 1 ) {
         astError( AST__WCSTY, "astTransform(%s): The %s specifies an "
                   "illegal projection type ('%s').", astClass( this ), 
                   astClass( this ), FindPrjData( map->type )->desc  );

/* Report an error if the projection parameters were invalid. */
       } else if ( status == 2 ){
          astError( AST__WCSPA, "astTransform(%s): The %s has an unusable "
                    "parameter set for a %s projection.", astClass( this ),
                    astClass( this ), FindPrjData( map->type )->desc  );
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

int PrjType_( const char *ctype ){
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
   data = PrjInfo;
   while( data->prj != AST__WCSBAD && strcmp( data->ctype, ctype ) ) data ++;
   return data->prj;
}

const char *PrjName_( int type ){
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

const char *PrjDesc_( int type ){
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

static void WcsPerm( AstMapping **maps, int *inverts, int iwm  ){
/*
*  Name:
*     WcsPerm

*  Purpose:
*     Swap a WcsMap and a PermMap.

*  Type:
*     Private function.

*  Synopsis:
*     #include "wcsmap.h"
*     void WcsPerm( AstMapping **maps, int *inverts, int iwm )

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

*  Notes:
*     -  All links between input and output axes in the PermMap must 
*     be bi-directional, but there can be unconnected axes, and there
*     need not be the same number of input and output axes. Both
*     longitude and latitude axes must be passed by the PermMap.

*/

/* Local Variables: */
   AstPermMap *pm;               /* Pointer to the supplied PermMap */
   AstWcsMap *w1;                /* Pointer to the returned WcsMap */
   AstWcsMap *wm;                /* Pointer to the supplied WcsMap */
   double *consts;               /* Pointer to constants array */
   int *inperm;                  /* Pointer to input axis permutation array */
   int *outperm;                 /* Pointer to output axis permutation array */
   int ip;                       /* Index of projection parameter */
   int latax;                    /* Index of latitude axis */
   int lonax;                    /* Index of longitude axis */
   int npin;                     /* No. of input axes in supplied PermMap */
   int npout;                    /* No. of output axes in supplied PermMap */
   int old_pinv;                 /* Invert value for the supplied PermMap */
   int type;                     /* Projection type */

/* Check the global error status. */
   if ( !astOK ) return;

/* Store pointers to the supplied WcsMap and the PermMap. */
   wm = (AstWcsMap *) maps[ iwm ];
   pm = (AstPermMap *) maps[ 1 - iwm ];

/* Temporarily set the Invert attribute of the supplied PermMap to the
   supplied values. */
   old_pinv = astGetInvert( pm );
   astSetInvert( pm, inverts[ 1 - iwm ] );

/* Get the projection type of the supplied WcsMap and the indices of the
   longitude and latitude axes. */
   type = astGetWcsType( wm );
   lonax = astGetWcsAxis( wm, 0 );
   latax = astGetWcsAxis( wm, 1 );

/* Get the axis permutation and constants arrays representing the
   PermMap. Note, no constants are used more than once in the returned
   arrays (i.e. duplicate constants are returned in "consts" if more than
   one axis uses a given constant). */
   PermGet( pm, &outperm, &inperm, &consts );

   if( astOK ) {

/* Get the number of input and output axes in the PermMap. */
      npin = astGetNin( pm );
      npout = astGetNout( pm );

/* Create the new WcsMap with permuted longitude and latitude axes. Note,
   the private interface to astWcsMap uses 1-based axis indices. */
      if( iwm == 0 ) {
         w1 = astWcsMap( npout, type, inperm[ lonax ] + 1, 
                         inperm[ latax ] + 1, "" );
      } else {
         w1 = astWcsMap( npin, type, outperm[ lonax ] + 1, 
                         outperm[ latax ] + 1, "" );
      }

/* Copy the projection parameters. */
      for( ip = 0; ip < AST__WCSMX; ip++ ){
         astSetProjP( w1, ip, astGetProjP( wm, ip ) );
      }

/* Set the invert flag. */
      astSetInvert( w1, inverts[ iwm ] );

/* Free the axis permutation and constants arrays. */
      outperm = (int *) astFree( (void *) outperm );
      inperm = (int *) astFree( (void *) inperm );
      consts = (double *) astFree( (void *) consts );
   }

/* Re-instate the original value of the Invert attribute of the supplied 
   PermMap. */
   astSetInvert( pm, old_pinv );

/* Replace the supplied WcsMap with the one created above, swapping the
   order. */
   if( astOK ){
      (void) astAnnul( wm );
      maps[ 1 - iwm ] = (AstMapping *) w1;

      maps[ iwm ] = (AstMapping *) pm;
      inverts[ iwm ] = old_pinv;

   }

/* Return. */
   return;
}

/* Functions which access class attributes. */
/* ---------------------------------------- */
/* Implement member functions to access the attributes associated with
   this class using the macros defined for this purpose in the
   "object.h" file. For a description of each attribute, see the class
   interface (in the associated .h file). */


/* ProjP. */
/* ------ */
/*
*att++
*  Name:
*     ProjP(i)

*  Purpose:
*     FITS-WCS projection parameters.

*  Type:
*     Public attribute.

*  Synopsis:
*     Floating point.

*  Description:
*     This attribute specifies the projection parameter values to be
*     used by a WcsMap when implementing a FITS-WCS sky
*     projection. The ProjP attribute name should be subscripted by an
*     integer in the range 0 to 9 to identify which projection
*     parameter is required. For example, "ProjP(1)=45.0" would
*     specify a value for projection parameter 1 of a WcsMap.
*
*     These projection parameters correspond exactly to the values
*     stored using the FITS-WCS keywords "PROJP0" to "PROJP9". This
*     means that projection parameters which correspond to angles must
*     be given in degrees (despite the fact that the angular
*     coordinates and other attributes used by a WcsMap are in
*     radians).
*
*     The set of projection parameters used by a WcsMap depends on the
*     type of projection, which is determined by its WcsType
*     parameter.  Most projections either do not require projection
*     parameters, or use parameters 1 and 2. You should consult the
*     FITS-WCS paper for details.
*
*     Some projection parameters have default values (as defined in
*     the FITS-WCS paper) which apply if no explicit value is given.
*     Currently, these defaults are all zero. You may omit setting a
*     value for these "optional" parameters and the default will
*     apply. Some projection parameters, however, have no default and
*     a value must be explicitly supplied.  This is most conveniently
c     done using the "options" argument of astWcsMap (q.v.) when a WcsMap
f     done using the OPTIONS argument of AST_WCSMAP (q.v.) when a WcsMap
*     is first created. An error will result when a WcsMap is used to
*     transform coordinates if any of its required projection
*     parameters has not been set and lacks a default value.

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

/* This is an array of 10 double values with a value of AST__BAD when 
   undefined but yielding a default of 0.0. If a new parameter value is
   set, the contents of the WCSLIB "prjprm" structure named "params"
   needs to be updated to reflect the new parameter value, and its flag
   needs to be cleared to force new intermediate values to be calculated
   based on the new projection parameters. Also, the value of attribute
   "NatLat" may need to be updated if it is stored in projection
   parameter 1. */
MAKE_CLEAR(ProjP,projp,((this->params.p)[ axis ] = 0.0,AST__BAD),AST__WCSMX)
MAKE_GET(ProjP,double,0.0,( this->projp[axis] == AST__BAD ? 0.0 : this->projp[axis] ),AST__WCSMX)
MAKE_TEST(ProjP,( this->projp[axis] != AST__BAD ),AST__WCSMX)
MAKE_SET(ProjP,double,projp,( 
            this->params.p[ axis ] = value, 
            this->params.flag = 0, 
            this->natlat = ( FindPrjData( this->type )->theta0 != AST__BAD )? 
                             this->natlat : 
                             AST__DD2R*(this->params.p)[ 1 ], 
            value),AST__WCSMX)

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
*     Hammer-Aitoff projection).

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
*     radians in the "native spherical" coordinate system. In some
*     cases, this latitude value may be determined by one or more
*     projection parameter values supplied for the WcsMap using its
*     ProjP(i) attribute.

*  Applicability:
*     WcsMap
*        All WcsMaps have this attribute.

*  Notes:
*     - A value of AST__BAD is returned if no latitude value is
*     available.
*     - For a definition of the reference point for a projection, see
*     the FITS-WCS paper.
*att--
*/
/* Native latitude of the projection's reference point. Read only. */
astMAKE_GET(WcsMap,NatLat,double,AST__BAD,this->natlat)

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
/* No copy constructor is needed, as a byte-by-byte copy suffices. */

/* Destructor. */
/* ----------- */
/* No destructor is needed as no memory, etc. needs freeing. */

/* Dump function. */
/* -------------- */
static void Dump( AstObject *this_object, AstChannel *channel ) {
/*
*  Name:
*     Dump

*  Purpose:
*     Dump function for WcsMap objects.

*  Type:
*     Private function.

*  Synopsis:
*     void Dump( AstObject *this, AstChannel *channel )

*  Description:
*     This function implements the Dump function which writes out data
*     for the WcsMap class to an output Channel.

*  Parameters:
*     this
*        Pointer to the WcsMap whose data are being written.
*     channel
*        Pointer to the Channel to which the data are being written.
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
   int i;                        /* Parameter index */
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
   ival = GetWcsType( this );
   prjdata = FindPrjData( ival );
   (void) sprintf( comment_buff, "%s projection", prjdata->desc );
   comment_buff[ 0 ] = toupper( comment_buff[ 0 ] );
   astWriteString( channel, "Type", 1, 1, prjdata->ctype + 1, comment_buff );

/* ProjP(i). */
/* --------- */
   for( i = 0; i < AST__WCSMX; i++ ){
      set = TestProjP( this, i );
      dval = set ? GetProjP( this, i ) : astGetProjP( this, i );
      (void) sprintf( buff, "ProjP%d", i );
      (void) sprintf( comment_buff, "Projection parameter %d", i );
      astWriteDouble( channel, buff, set, 0, dval, comment_buff );
   }

/* WcsAxis(axis). */
/* -------------- */
   for( axis = 0; axis < 2; axis++ ){
      ival =  GetWcsAxis( this, axis );
      (void) sprintf( buff, "WcsAx%d", axis + 1 );
      if( axis == 0 ) {
         comment = "Index of celestial longitude axis";
      } else {
         comment = "Index of celestial latitude axis";
      }
      astWriteInt( channel, buff, (axis!=ival), 0, ival + 1, comment );
   }

/* Note, the "params" and "natlat" components of the AstWcsMap structure
   are not written out because they can be re-generated from the other 
   components. */

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
astMAKE_ISA(WcsMap,Mapping,check,&class_init)
astMAKE_CHECK(WcsMap)

AstWcsMap *astWcsMap_( int ncoord, int type, int lonax, int latax, 
                       const char *options, ... ){
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
*     sky projections (all of those in the FITS-WCS paper are
*     supported) and applies them to a specified pair of coordinates.
*     Using the FITS-WCS terminology, this transformation is between
*     "native spherical" and "relative physical" coordinates.  These
*     coordinates may, optionally, be embedded in a space with more
*     than two dimensions, the remaining coordinates being copied
*     unchanged. Note, however, that for consistency with other AST
*     facilities, a WcsMap handles coordinates that represent angles
*     in radians (rather than the degrees used by FITS-WCS).
*
*     The type of FITS-WCS projection to be used and the coordinates
*     (axes) to which it applies are specified when a WcsMap is first
*     created. The projection type may subsequently be determined
*     using the WcsType attribute and the coordinates on which it acts
*     may be determined using the WcsAxis(lonlat) attribute.
*
*     Each WcsMap also uses up to 10 "projection parameters" which
*     specify the precise form of the projection. These parameter
*     values are accessed using the ProjP(i) attribute, where "i" is
*     an integer in the range 0 to 9. The number of projection
*     parameters used, and their meanings, are dependent upon the
*     projection type (most projections either do not use any
*     projection parameters, or use parameters 1 and 2). Before
*     creating a WcsMap you should consult the FITS-WCS paper for
*     details of which projection parameters are required, and which
*     have defaults. When creating the WcsMap, you must explicitly set
*     values for all those required projection parameters which do not
*     have defaults defined in this paper.

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
*        FITS-WCS paper for a list of the available projections.
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
*        here via the ProjP(i) attribute (see the "Examples"
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
c     wcsmap = astWcsMap( AST__MER, 2, 1, 2, "" );
f     WCSMAP = AST_WCSMAP( AST__MER, 2, 1, 2, ' ', STATUS )
*        Creates a WcsMap that implements a FITS-WCS Mercator
*        projection on pairs of coordinates, with coordinates 1 and 2
*        representing the longitude and latitude respectively. Note
*        that the FITS-WCS Mercator projection does not require any
*        projection parameters.
c     wcsmap = astWcsMap( AST__COE, 3, 2, 3, "ProjP(1)=40.0" );
f     WCSMAP = AST_WCSMAP( AST__COE, 3, 2, 3, 'ProjP(1)=40.0', STATUS )
*        Creates a WcsMap that implements a FITS-WCS conical equal
*        area projection. The WcsMap acts on points in a 3-dimensional
*        space; coordinates 2 and 3 represent longitude and latitude
*        respectively, while the values of coordinate 1 are copied
*        unchanged.  Projection parameter 1 (corresponding to FITS
*        keyword "PROJP1") is required and has no default, so is set
*        explicitly to 40.0 degrees. Projection parameter 2
*        (corresponding to FITS keyword "PROJP2") is required but has
*        a default of zero, so need not be specified.

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
*     - The validity of any projection parameters given via the ProjP(i)
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
*--
*/

/* Local Variables: */
   AstWcsMap *new;               /* Pointer to new WcsMap */
   va_list args;                 /* Variable argument list */

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
      astVSet( new, options, args );
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
   AstWcsMap *new;              /* Pointer to new WcsMap */
   va_list args;                 /* Variable argument list */

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
      astVSet( new, options, args );
      va_end( args );

/* If an error occurred, clean up by deleting the new object. */
      if ( !astOK ) new = astDelete( new );
   }

/* Return an ID value for the new WcsMap. */
   return astMakeId( new );
}

AstWcsMap *astInitWcsMap_( void *mem, size_t size, int init,
                           AstWcsMapVtab *vtab, const char *name,
                           int ncin, int type, int lonax, int latax ) {
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
   AstWcsMap *new;              /* Pointer to new WcsMap */
   int i;                       /* parameter index */

/* Check the global status. */
   if ( !astOK ) return NULL;

/* Initialise. */
   new = NULL;

/* If a genuine FITS-WCS projection has been specified, check the initialisation 
   value(s) for validity, reporting an error if necessary. Prefix the error 
   report with the name of the function and the class of object being 
   processed. First check that at least two dimensions are to be mapped. */
   if( type != AST__WCSBAD ){
      if ( ncin < 2 ){
         astError( AST__WCSNC, "astInitWcsMap(%s): Too few axes (%d) "
                   "specified. Must be at least 2.", name, ncin );

/* Report an error if either the longitude or latitude axes are out of
   bounds. */
      } else if ( lonax < 0 || lonax >= ncin ){
         astError( AST__WCSAX, "astInitWcsMap(%s): Specified longitude axis (%d) "
                   "does not exist within a %d dimensional coordinate system. ",
                   name, lonax + 1, ncin );
   
      } else if ( latax < 0 || latax >= ncin ){
         astError( AST__WCSAX, "astInitWcsMap(%s): Specified latitude axis (%d) "
                   "does not exist within a %d dimensional coordinate system. ",
                   name, lonax + 1, ncin );

/* Report an error if the longitude or latitude axes are the same. */
      } else if ( lonax == latax ){
         astError( AST__WCSAX, "astInitWcsMap(%s): The same axis (%d) has been "
                   "given for both the longitude and the latitude axis.", name,
                   lonax + 1 );

/* Report an error if projection type is unknown. */
      } else if ( type < 1 || type >= AST__WCSBAD ){
         astError( AST__WCSTY, "astInitWcsMap(%s): Projection type %d is "
                   "undefined. Projection types must be in the range 1 to %d.",
                   name, type, AST__WCSBAD - 1 );
      }
   }

/* If all the above checks have been passed succesfully... */
   if( astOK ){

/* Initialise a Mapping structure (the parent class) as the first component
   within the WcsMap structure, allocating memory if necessary. Specify that
   the Mapping should be defined in both the forward and inverse directions. */
      new = (AstWcsMap *) astInitMapping( mem, size, init,
                                          (AstMappingVtab *) vtab, name,
                                          ncin, ncin, 1, 1 );

/* If necessary, initialise the virtual function table. */
/* ---------------------------------------------------- */
      if ( init ) InitVtab( vtab );
      if ( astOK ) {

/* Initialise the WcsMap data. */
/* ---------------------------- */
/* Store the projection type. */
         new->type = type;

/* Store the axes associated with longitude and latitude. */
         new->wcsaxis[0] = lonax;
         new->wcsaxis[1] = latax;

/* Store "undefined" projection parameters. */
         for( i = 0; i < AST__WCSMX; i++ ) new->projp[ i ] = AST__BAD;

/* Initialise the projection parameters, etc, in the FITS-WCS libraries "prjprm"
   structure (defined in proj.h). Note, BAD values in "projp" are mirrored
   by the safer value of zero in "params.p". */
         new->params.flag = 0;   /* Tells FITS-WCS to re-initialise the structure. */
         new->params.r0 = 0;     /* Tells FITS-WCS to use a unit sphere. */
         for( i = 0; i < AST__WCSMX; i++ ) new->params.p[ i ] = 0.0;

/* Get the native latitude of the reference point for the specified
   projection stored in the projection database in wcsmap.h. This will be
   set to AST__BAD if the required value is not constant but is given by
   projection parameter number 1. In this case, the value of the "natlat"
   component will be assigned the correct value when a value is assigned to
   projection parameter 1 using astSetProjP. */
         if( type != AST__WCSBAD ){
            new->natlat = FindPrjData( new->type )->theta0;
         } else {
            new->natlat = 0.0;
         }

/* If an error occurred, clean up by deleting the new WcsMap. */
         if ( !astOK ) new = astDelete( new );
      }
   }

/* Return a pointer to the new WcsMap. */
   return new;
}

AstWcsMap *astLoadWcsMap_( void *mem, size_t size, int init,
                           AstWcsMapVtab *vtab, const char *name,
                           AstChannel *channel ) {
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
*     AstWcsMap *astLoadWcsMap( void *mem, size_t size, int init,
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
*     init
*        A boolean flag indicating if the WcsMap's virtual function
*        table is to be initialised. If this value is non-zero, the
*        virtual function table will be initialised by this function.
*
*        If the "vtab" parameter is NULL, the "init" value is ignored
*        and the (static) virtual function table initialisation flag
*        for the WcsMap class is used instead.
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

/* Local Variables: */
   AstWcsMap *new;              /* Pointer to the new WcsMap */
   const char *text;            /* Textual form of an integer value */
   char buff[ KEY_LEN + 1 ];    /* Buffer for keyword string */
   double natlat;               /* New value for NatLat attribute */
   int axis;                    /* Axis index */
   int i;                       /* Parameter index */

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
      init = !class_init;
      vtab = &class_vtab;
      name = "WcsMap";
   }

/* Invoke the parent class loader to load data for all the ancestral
   classes of the current one, returning a pointer to the resulting
   partly-built WcsMap. */
   new = astLoadMapping( mem, size, init, (AstMappingVtab *) vtab, name,
                         channel );

/* If required, initialise the part of the virtual function table used
   by this class. */
   if ( init ) InitVtab( vtab );

/* Note if we have successfully initialised the (static) virtual
   function table owned by this class (so that this is done only
   once). */
   if ( astOK ) {
      if ( ( vtab == &class_vtab ) && init ) class_init = 1;

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

/* ProjP(i). */
/* --------- */
      for( i = 0; i < AST__WCSMX; i++ ){
         (void) sprintf( buff, "projp%d", i );
         new->projp[ i ] = astReadDouble( channel, buff, AST__BAD );
         if ( TestProjP( new, i ) ) SetProjP( new, i, new->projp[ i ]);
      }

/* WcsAxis(axis). */
/* -------------- */
      for( axis = 0; axis < 2; axis++ ){
         (void) sprintf( buff, "wcsax%d", axis + 1 );
         new->wcsaxis[ axis ] = astReadInt( channel, buff, axis + 1 ) - 1;
      }

/* Initialise the structure used by WCSLIB to hold intermediate values,
   so that the values will be re-calculated on the first invocation of a
   mapping function. */
      new->params.flag = 0;
      new->params.r0 = 0;  
      for( i = 0; i < AST__WCSMX; i++ ) {
         if( new->projp[ i ] != AST__BAD ){
            new->params.p[ i ] = new->projp[ i ];
         } else {
            new->params.p[ i ] = 0.0;
         }
      }

/* Get the native latitude of the reference point for the specified
   projection stored in the projection database. This will be
   set to AST__BAD if the required value is not constant but is given by
   projection parameter number 1. In this case, use of the radian
   equivalent of PROJP1 (which was set above). */
      natlat = FindPrjData( new->type )->theta0;
      if( natlat == AST__BAD ){
         natlat = AST__DD2R*(new->params.p)[ 1 ];
      }
      new->natlat = natlat;

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
