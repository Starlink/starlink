#if !defined( SKYAXIS_INCLUDED ) /* Include this file only once */
#define SKYAXIS_INCLUDED
/*
*+
*  Name:
*     skyaxis.h

*  Type:
*     C include file.

*  Purpose:
*     Define the interface to the SkyAxis class.

*  Invocation:
*     #include "skyaxis.h"

*  Description:
*     This include file defines the interface to the SkyAxis class and
*     provides the type definitions, function prototypes and macros,
*     etc.  needed to use this class.
*
*     The SkyAxis class extends the Axis class to represent angles on
*     the sky measured in radians. It provides alternative formatting
*     facilities for representing these coordinate values either as
*     angles (in degrees) or as time (in hours) using sexagesimal
*     notation. It also provides alternative defaults for certain
*     attributes and adds new attributes and methods of its own which
*     are needed to manipulate angular coordinates on the sky.

*  Inheritance:
*     The SkyAxis class inherits from the Axis class.

*  Attributes Over-Ridden:
*     Format (string)
*        The SkyAxis class defines a new syntax for this string.
*     Label (string)
*        The SkyAxis class defines new default values. These may
*        depend on other attribute settings.
*     Symbol (string)
*        The SkyAxis class defines new default values. These may
*        depend on other attribute settings.
*     Unit (string)
*        The SkyAxis class defines new default values. These may
*        depend on other attribute settings.

*  New Attributes Defined:
*     AsTime (integer)
*        A boolean value which indicates whether SkyAxis coordinate
*        values should be formatted for display as times (instead of
*        angles). It is used to determine the default format to use if
*        no explicit value has been set for the Format attribute.
*     IsLatitude (integer)
*        A boolean value which indicates whether a SkyAxis is a
*        latitude axis (as opposed to a longitude axis). It is used by
*        the astAxisNorm method to determine how to wrap angles into
*        an acceptable range for display. It also determines the
*        default value for the "AsTime" attribute (since longitudes on
*        the sky are usually expressed as times).

*  Methods Over-Ridden:
*     Public:
*        astAxisFormat
*           Format a coordinate value for a SkyAxis.
*        astAxisNorm
*           Normalise a SkyAxis coordinate value.
*        astAxisUnformat
*           Read a formatted coordinate value for a SkyAxis.

*     Protected:
*        astAxisAbbrev
*           Abbreviate a formatted SkyAxis value by skipping leading fields.
*        astAxisGap
*           Find a "nice" gap for tabulating SkyAxis values.
*        astClearAxisFormat
*           Clear the Format attribute for a SkyAxis.
*        astGetAxisDirection
*           Obtain the value of the Direction attribute for a SkyAxis.
*        astGetAxisFormat
*           Obtain a pointer to the Format attribute for a SkyAxis.
*        astGetAxisLabel
*           Obtain a pointer to the Label attribute for a SkyAxis.
*        astGetAxisSymbol
*           Obtain a pointer to the Symbol attribute for a SkyAxis.
*        astGetAxisUnit
*           Obtain a pointer to the Unit attribute for a SkyAxis.
*        astSetAxisFormat
*           Set a value for the Format attribute of a SkyAxis.
*        astTestAxisFormat
*           Test if a value has been set for the Format attribute of a SkyAxis.
*        astAxisOverlay
*           Overlay the attributes of a template SkyAxis on to another Axis.
*        astSetAttrib
*           Set an attribute value for a SkyAxis.

*  New Methods Defined:
*     Public:
*        None.

*     Protected:
*        astClearAxisAsTime
*           Clear the AsTime attribute for a SkyAxis.
*        astClearAxisIsLatitude
*           Clear the IsLatitude attribute for a SkyAxis.
*        astGetAxisAsTime
*           Obtain the value of the AsTime attribute for a SkyAxis.
*        astGetAxisIsLatitude
*           Obtain the value of the IsLatitude attribute for a SkyAxis.
*        astSetAxisAsTime
*           Set a value for the AsTime attribute of a SkyAxis.
*        astSetAxisIsLatitude
*           Set a value for the IsLatitude attribute of a SkyAxis.
*        astTestAxisAsTime
*           Test if a value has been set for the AsTime attribute of a SkyAxis.
*        astTestAxisIsLatitude
*           Test if a value has been set for the IsLatitude attribute of a
*           SkyAxis.

*  Other Class Functions:
*     Public:
*        astIsASkyAxis
*           Test class membership.
*        astSkyAxis
*           Create an SkyAxis.

*     Protected:
*        astCheckSkyAxis
*           Validate class membership.
*        astInitSkyAxis
*           Initialise an SkyAxis.

*  Macros:
*     None.

*  Type Definitions:
*     Public:
*        AstSkyAxis
*           SkyAxis object type.

*     Protected:
*        AstSkyAxisVtab
*           SkyAxis virtual function table type.

*  Feature Test Macros:
*     astCLASS
*        If the astCLASS macro is undefined, only public symbols are
*        made available, otherwise protected symbols (for use in other
*        class implementations) are defined. This macro also affects
*        the reporting of error context information, which is only
*        provided for external calls to the AST library.

*  Copyright:
*     <COPYRIGHT_STATEMENT>

*  Authors:
*     RFWS: R.F. Warren-Smith (Starlink)

*  History:
*     29-MAR-1996 (RFWS):
*        Original version.
*     25-APR-1996 (RFWS):
*        Made all attribute access functions protected.
*     13-MAY-1996 (RFWS):
*        Documented over-riding of the astGetAxisDirection method.
*     26-FEB-1998 (RFWS):
*        Over-ride the astAxisUnformat method.
*-
*/

/* Include files. */
/* ============== */
/* Interface definitions. */
/* ---------------------- */
#include "axis.h"                /* Coordinate axes (parent class) */

/* Type Definitions. */
/* ================= */
/* SkyAxis structure. */
/* ------------------ */
/* This structure contains all information that is unique to each
   object in the class (e.g. its instance variables). */
typedef struct AstSkyAxis {

/* Attributes inherited from the parent class. */
   AstAxis axis;                 /* Parent class structure */

/* Attributes specific to objects in this class. */
   char *skyformat;              /* Pointer to sky format string */
   int as_time;                  /* Format angles as time (hours)? */
   int is_latitude;              /* SkyAxis is a latitude axis? */
} AstSkyAxis;

/* Virtual function table. */
/* ----------------------- */
/* This table contains all information that is the same for all
   objects in the class (e.g. pointers to its virtual functions). */
#if defined(astCLASS)            /* Protected */
typedef struct AstSkyAxisVtab {

/* Properties (e.g. methods) inherited from the parent class. */
   AstAxisVtab axis_vtab;        /* Parent class virtual function table */

/* Unique flag value to determine class membership. */
   int *check;                   /* Check value */

/* Properties (e.g. methods) specific to this class. */
   int (* GetAxisAsTime)( AstSkyAxis * );
   int (* GetAxisIsLatitude)( AstSkyAxis * );
   int (* TestAxisAsTime)( AstSkyAxis * );
   int (* TestAxisIsLatitude)( AstSkyAxis * );
   void (* ClearAxisAsTime)( AstSkyAxis * );
   void (* ClearAxisIsLatitude)( AstSkyAxis * );
   void (* SetAxisAsTime)( AstSkyAxis *, int );
   void (* SetAxisIsLatitude)( AstSkyAxis *, int );
} AstSkyAxisVtab;
#endif

/* Function prototypes. */
/* ==================== */
/* Prototypes for standard class functions. */
/* ---------------------------------------- */
astPROTO_CHECK(SkyAxis)          /* Check class membership */
astPROTO_ISA(SkyAxis)            /* Test class membership */

/* Constructor. */
#if defined(astCLASS)            /* Protected. */
AstSkyAxis *astSkyAxis_( const char *, ... );
#else
AstSkyAxis *astSkyAxisId_( const char *, ... );
#endif

#if defined(astCLASS)            /* Protected */

/* Initialiser. */
AstSkyAxis *astInitSkyAxis_( void *, size_t, int, AstSkyAxisVtab *,
                             const char * );

/* Loader. */
AstSkyAxis *astLoadSkyAxis_( void *, size_t, int, AstSkyAxisVtab *,
                             const char *, AstChannel * );
#endif

/* Prototypes for member functions. */
/* -------------------------------- */
#if defined(astCLASS)            /* Protected */
int astGetAxisAsTime_( AstSkyAxis * );
int astGetAxisIsLatitude_( AstSkyAxis * );
int astTestAxisAsTime_( AstSkyAxis * );
int astTestAxisIsLatitude_( AstSkyAxis * );
void astClearAxisAsTime_( AstSkyAxis * );
void astClearAxisIsLatitude_( AstSkyAxis * );
void astSetAxisAsTime_( AstSkyAxis *, int );
void astSetAxisIsLatitude_( AstSkyAxis *, int );
#endif

/* Function interfaces. */
/* ==================== */
/* These macros are wrap-ups for the functions defined by this class
   to make them easier to invoke (e.g. to avoid type mis-matches when
   passing pointers to objects from derived classes). */

/* Interfaces to standard class functions. */
/* --------------------------------------- */
/* Some of these functions provide validation, so we cannot use them
   to validate their own arguments. We must use a cast when passing
   object pointers (so that they can accept objects from derived
   classes). */

/* Check class membership. */
#define astCheckSkyAxis(this) astINVOKE_CHECK(SkyAxis,this)

/* Test class membership. */
#define astIsASkyAxis(this) astINVOKE_ISA(SkyAxis,this)

/* Constructor. */
#if defined(astCLASS)            /* Protected. */
#define astSkyAxis astINVOKE(F,astSkyAxis_)
#else
#define astSkyAxis astINVOKE(F,astSkyAxisId_)
#endif

#if defined(astCLASS)            /* Protected */

/* Initialiser. */
#define astInitSkyAxis(mem,size,init,vtab,name) \
astINVOKE(O,astInitSkyAxis_(mem,size,init,vtab,name))

/* Loader. */
#define astLoadSkyAxis(mem,size,init,vtab,name,channel) \
astINVOKE(O,astLoadSkyAxis_(mem,size,init,vtab,name,astCheckChannel(channel)))
#endif

/* Interfaces to public member functions. */
/* -------------------------------------- */
/* Here we make use of astCheckSkyAxis to validate SkyAxis pointers
   before use. This provides a contextual error report if a pointer to
   the wrong sort of object is supplied. */
#if defined(astCLASS)            /* Protected */
#define astClearAxisAsTime(this) \
astINVOKE(V,astClearAxisAsTime_(astCheckSkyAxis(this)))
#define astClearAxisIsLatitude(this) \
astINVOKE(V,astClearAxisIsLatitude_(astCheckSkyAxis(this)))
#define astGetAxisAsTime(this) \
astINVOKE(V,astGetAxisAsTime_(astCheckSkyAxis(this)))
#define astGetAxisIsLatitude(this) \
astINVOKE(V,astGetAxisIsLatitude_(astCheckSkyAxis(this)))
#define astSetAxisAsTime(this,value) \
astINVOKE(V,astSetAxisAsTime_(astCheckSkyAxis(this),value))
#define astSetAxisIsLatitude(this,value) \
astINVOKE(V,astSetAxisIsLatitude_(astCheckSkyAxis(this),value))
#define astTestAxisAsTime(this) \
astINVOKE(V,astTestAxisAsTime_(astCheckSkyAxis(this)))
#define astTestAxisIsLatitude(this) \
astINVOKE(V,astTestAxisIsLatitude_(astCheckSkyAxis(this)))
#endif
#endif
