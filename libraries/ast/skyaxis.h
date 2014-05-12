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
*     CentreZero (integer)
*        A boolean value which indicates whether a SkyAxis value should
*        be normalised into the range [-PI,+PI] or [0,2.PI] when astNorm
*        is used.
*     IsLatitude (integer)
*        A boolean value which indicates whether a SkyAxis is a
*        latitude axis (as opposed to a longitude axis). It is used to
*        determine default axis labels and symbols. It also determines the
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
*        astAxisDistance
*           Find the distance between two SkyAxis values.
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
*        astAxisOffset
*           Add an increment onto a supplied SkyAxis value.
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
*        astClearAxisCentreZero
*           Clear the CentreZero attribute for a SkyAxis.
*        astClearAxisIsLatitude
*           Clear the IsLatitude attribute for a SkyAxis.
*        astGetAxisAsTime
*           Obtain the value of the AsTime attribute for a SkyAxis.
*        astGetAxisIsLatitude
*           Obtain the value of the IsLatitude attribute for a SkyAxis.
*        astGetAxisCentreZero
*           Obtain the value of the CentreZero attribute for a SkyAxis.
*        astSetAxisAsTime
*           Set a value for the AsTime attribute of a SkyAxis.
*        astSetAxisIsLatitude
*           Set a value for the IsLatitude attribute of a SkyAxis.
*        astSetAxisCentreZero
*           Set a value for the CentreZero attribute of a SkyAxis.
*        astTestAxisAsTime
*           Test if a value has been set for the AsTime attribute of a SkyAxis.
*        astTestAxisIsLatitude
*           Test if a value has been set for the IsLatitude attribute of a
*           SkyAxis.
*        astTestAxisCentreZero
*           Test if a value has been set for the CentreZero attribute of a
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
*     8-JAN-2003 (DSB):
*        Added protected astInitSkyAxisVtab method.
*-
*/

/* Include files. */
/* ============== */
/* Interface definitions. */
/* ---------------------- */
#include "axis.h"                /* Coordinate axes (parent class) */

/* Macros */
/* ====== */

/* Define constants used to size global arrays in this module. */
/* Define numerical constants for use in thie module. */
#define AST__SKYAXIS_GETAXISFORMAT_BUFF_LEN 50
#define AST__SKYAXIS_DHMSFORMAT_BUFF_LEN 70
#define AST__SKYAXIS_DHMSUNIT_BUFF_LEN 17
#define AST__SKYAXIS_GETATTRIB_BUFF_LEN 50

/* Define a dummy __attribute__ macro for use on non-GNU compilers. */
#ifndef __GNUC__
#  define  __attribute__(x)  /*NOTHING*/
#endif


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
   int centrezero;               /* Normalised range is zero-centred? */
} AstSkyAxis;

/* Virtual function table. */
/* ----------------------- */
/* This table contains all information that is the same for all
   objects in the class (e.g. pointers to its virtual functions). */
#if defined(astCLASS)            /* Protected */
typedef struct AstSkyAxisVtab {

/* Properties (e.g. methods) inherited from the parent class. */
   AstAxisVtab axis_vtab;        /* Parent class virtual function table */

/* A Unique identifier to determine class membership. */
   AstClassIdentifier id;

/* Properties (e.g. methods) specific to this class. */
   int (* GetAxisAsTime)( AstSkyAxis *, int * );
   int (* GetAxisIsLatitude)( AstSkyAxis *, int * );
   int (* GetAxisCentreZero)( AstSkyAxis *, int * );
   int (* TestAxisAsTime)( AstSkyAxis *, int * );
   int (* TestAxisIsLatitude)( AstSkyAxis *, int * );
   int (* TestAxisCentreZero)( AstSkyAxis *, int * );
   void (* ClearAxisAsTime)( AstSkyAxis *, int * );
   void (* ClearAxisIsLatitude)( AstSkyAxis *, int * );
   void (* ClearAxisCentreZero)( AstSkyAxis *, int * );
   void (* SetAxisAsTime)( AstSkyAxis *, int, int * );
   void (* SetAxisIsLatitude)( AstSkyAxis *, int, int * );
   void (* SetAxisCentreZero)( AstSkyAxis *, int, int * );
} AstSkyAxisVtab;

#if defined(THREAD_SAFE)

/* Define a structure holding all data items that are global within this
   class. */
typedef struct AstSkyAxisGlobals {
   AstSkyAxisVtab Class_Vtab;
   int Class_Init;
   char DHmsFormat_Buff[ AST__SKYAXIS_DHMSFORMAT_BUFF_LEN + 1 ];
   char DHmsUnit_Buff[ AST__SKYAXIS_DHMSUNIT_BUFF_LEN + 1 ];
   char GetAttrib_Buff[ AST__SKYAXIS_GETATTRIB_BUFF_LEN + 1 ];
   char GetAxisFormat_Buff[ AST__SKYAXIS_GETAXISFORMAT_BUFF_LEN + 1 ];
   char *GhDelim;
   char *GmDelim;
   char *GsDelim;
   char *GdDelim;
   char *GamDelim;
   char *GasDelim;
} AstSkyAxisGlobals;

#endif


#endif

/* Function prototypes. */
/* ==================== */
/* Prototypes for standard class functions. */
/* ---------------------------------------- */
astPROTO_CHECK(SkyAxis)          /* Check class membership */
astPROTO_ISA(SkyAxis)            /* Test class membership */

/* Constructor. */
#if defined(astCLASS)            /* Protected. */
AstSkyAxis *astSkyAxis_( const char *, int *, ...);
#else
AstSkyAxis *astSkyAxisId_( const char *, ... )__attribute__((format(printf,1,2)));
#endif

#if defined(astCLASS)            /* Protected */

/* Initialiser. */
AstSkyAxis *astInitSkyAxis_( void *, size_t, int, AstSkyAxisVtab *,
                             const char *, int * );

/* Vtab initialiser. */
void astInitSkyAxisVtab_( AstSkyAxisVtab *, const char *, int * );

/* Loader. */
AstSkyAxis *astLoadSkyAxis_( void *, size_t, AstSkyAxisVtab *,
                             const char *, AstChannel *, int * );

/* Thread-safe initialiser for all global data used by this module. */
#if defined(THREAD_SAFE)
void astInitSkyAxisGlobals_( AstSkyAxisGlobals * );
#endif

#endif

/* Prototypes for member functions. */
/* -------------------------------- */
#if defined(astCLASS)            /* Protected */
int astGetAxisAsTime_( AstSkyAxis *, int * );
int astGetAxisIsLatitude_( AstSkyAxis *, int * );
int astGetAxisCentreZero_( AstSkyAxis *, int * );
int astTestAxisAsTime_( AstSkyAxis *, int * );
int astTestAxisIsLatitude_( AstSkyAxis *, int * );
int astTestAxisCentreZero_( AstSkyAxis *, int * );
void astClearAxisAsTime_( AstSkyAxis *, int * );
void astClearAxisIsLatitude_( AstSkyAxis *, int * );
void astClearAxisCentreZero_( AstSkyAxis *, int * );
void astSetAxisAsTime_( AstSkyAxis *, int, int * );
void astSetAxisIsLatitude_( AstSkyAxis *, int, int * );
void astSetAxisCentreZero_( AstSkyAxis *, int, int * );

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
#define astCheckSkyAxis(this) astINVOKE_CHECK(SkyAxis,this,0)
#define astVerifySkyAxis(this) astINVOKE_CHECK(SkyAxis,this,1)

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
astINVOKE(O,astInitSkyAxis_(mem,size,init,vtab,name,STATUS_PTR))

/* Vtab Initialiser. */
#define astInitSkyAxisVtab(vtab,name) astINVOKE(V,astInitSkyAxisVtab_(vtab,name,STATUS_PTR))
/* Loader. */
#define astLoadSkyAxis(mem,size,vtab,name,channel) \
astINVOKE(O,astLoadSkyAxis_(mem,size,vtab,name,astCheckChannel(channel),STATUS_PTR))
#endif

/* Interfaces to public member functions. */
/* -------------------------------------- */

/* Here we make use of astCheckSkyAxis to validate SkyAxis pointers
   before use. This provides a contextual error report if a pointer to
   the wrong sort of object is supplied. */
#if defined(astCLASS)            /* Protected */

#define astClearAxisAsTime(this) \
astINVOKE(V,astClearAxisAsTime_(astCheckSkyAxis(this),STATUS_PTR))
#define astClearAxisIsLatitude(this) \
astINVOKE(V,astClearAxisIsLatitude_(astCheckSkyAxis(this),STATUS_PTR))
#define astGetAxisAsTime(this) \
astINVOKE(V,astGetAxisAsTime_(astCheckSkyAxis(this),STATUS_PTR))
#define astGetAxisIsLatitude(this) \
astINVOKE(V,astGetAxisIsLatitude_(astCheckSkyAxis(this),STATUS_PTR))
#define astSetAxisAsTime(this,value) \
astINVOKE(V,astSetAxisAsTime_(astCheckSkyAxis(this),value,STATUS_PTR))
#define astSetAxisIsLatitude(this,value) \
astINVOKE(V,astSetAxisIsLatitude_(astCheckSkyAxis(this),value,STATUS_PTR))
#define astTestAxisAsTime(this) \
astINVOKE(V,astTestAxisAsTime_(astCheckSkyAxis(this),STATUS_PTR))
#define astTestAxisIsLatitude(this) \
astINVOKE(V,astTestAxisIsLatitude_(astCheckSkyAxis(this),STATUS_PTR))

#define astClearAxisCentreZero(this) \
astINVOKE(V,astClearAxisCentreZero_(astCheckSkyAxis(this),STATUS_PTR))
#define astGetAxisCentreZero(this) \
astINVOKE(V,astGetAxisCentreZero_(astCheckSkyAxis(this),STATUS_PTR))
#define astSetAxisCentreZero(this,value) \
astINVOKE(V,astSetAxisCentreZero_(astCheckSkyAxis(this),value,STATUS_PTR))
#define astTestAxisCentreZero(this) \
astINVOKE(V,astTestAxisCentreZero_(astCheckSkyAxis(this),STATUS_PTR))

#endif
#endif





