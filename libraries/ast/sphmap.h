#if !defined( SPHMAP_INCLUDED ) /* Include this file only once */
#define SPHMAP_INCLUDED
/*
*+
*  Name:
*     sphmap.h

*  Type:
*     C include file.

*  Purpose:
*     Define the interface to the SphMap class.

*  Invocation:
*     #include "sphmap.h"

*  Description:
*     This include file defines the interface to the SphMap class and
*     provides the type definitions, function prototypes and macros,
*     etc.  needed to use this class.
*
*     The SphMap class implements Mappings which maps positions from
*     3-dimensional Cartesian coordinates into 2-dimensional spherical
*     coordinates (i.e. longitude and latitude on a unit sphere). The
*     inverse Mapping always produces vectors of unit length.
*
*     The spherical coordinates are longitude (positive anti-clockwise
*     looking from the positive latitude pole) and latitude. The
*     Cartesian coordinates are right-handed, with the x-axis (axis 1)
*     at zero longitude and latitude, and the z-axis (axis 3) at the
*     positive latitude pole.
*
*     At either pole, the longitude is set to the value of the PolarLong
*     attribute. If the Cartesian coordinates are all zero, then the
*     longitude and latitude values are set to AST__BAD.

*  Inheritance:
*     The SphMap class inherits from the Mapping class.

*  Attributes Over-Ridden:
*     None.

*  New Attributes Defined:
*     PolarLong (double)
*        This attribute holds the longitude value, in radians, to be
*        returned when a Cartesian position corresponding to either the north
*        or south pole is transformed into spherical coordinates. The
*        default value is zero.
*     UnitRadius (integer)
*        This is a boolean attribute which indicates whether the
*        3-dimensional vectors which are supplied as input to a SphMap
*        are known to always have unit length, so that they lie on a
*        unit sphere centred on the origin.
*
*        If this condition is true (indicated by setting UnitRadius
*        non-zero), it implies that a CmpMap which is composed of a
*        SphMap applied in the forward direction followed by a similar
*        SphMap applied in the inverse direction may be simplified
*        (e.g. by astSimplify) to become a UnitMap. This is because
*        the input and output vectors will both have unit length and
*        will therefore have the same coordinate values.
*
*        If UnitRadius is zero (the default), then although the output
*        vector produced by the CmpMap (above) will still have unit
*        length, the input vector may not have. This will, in general,
*        change the coordinate values, so it prevents the pair of
*        SphMaps being simplified.

*  Methods Over-Ridden:
*     Public:
*        None.
*
*     Protected:
*        astClearAttrib
*           Clear an attribute value for a SphMap.
*        astGetAttrib
*           Get an attribute value for a SphMap.
*        astMapMerge
*           Simplify a sequence of Mappings containing a SphMap.
*        astSetAttrib
*           Set an attribute value for a SphMap.
*        astTestAttrib
*           Test if an attribute value has been set for a SphMap.
*        astTransform
*           Apply a SphMap to transform a set of points.

*  New Methods Defined:
*     Public:
*        None.
*
*     Protected:
*        astClearUnitRadius
*           Clear the UnitRadius attribute value for a SphMap.
*        astGetUnitRadius
*           Get the UnitRadius attribute value for a SphMap.
*        astSetUnitRadius
*           Set the UnitRadius attribute value for a SphMap.
*        astTestUnitRadius
*           Test if a UnitRadius attribute value has been set for a SphMap.
*        astClearPolarLong
*           Clear the PolarLong attribute value for a SphMap.
*        astGetPolarLong
*           Get the PolarLong attribute value for a SphMap.
*        astSetPolarLong
*           Set the PolarLong attribute value for a SphMap.
*        astTestPolarLong
*           Test if a PolarLong attribute value has been set for a SphMap.

*  Other Class Functions:
*     Public:
*        astIsASphMap
*           Test class membership.
*        astSphMap
*           Create a SphMap.
*
*     Protected:
*        astCheckSphMap
*           Validate class membership.
*        astInitSphMap
*           Initialise a SphMap.
*        astInitSphMapVtab
*           Initialise the virtual function table for the SphMap class.
*        astLoadSphMap
*           Load a SphMap.

*  Macros:
*     None.

*  Type Definitions:
*     Public:
*        AstSphMap
*           SphMap object type.
*
*     Protected:
*        AstSphMapVtab
*           SphMap virtual function table type.

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
*     DSB: D.S. Berry (Starlink)

*  History:
*     25-OCT-1996 (DSB):
*        Original version.
*     24-MAR-1998 (RFWS):
*        Override the astMapMerge method.
*     4-SEP-1998 (DSB):
*        Added UnitRadius attribute.
*     8-JAN-2003 (DSB):
*        Added protected astInitSphMapVtab method.
*     11-JUN-2003 (DSB):
*        Added PolarLong attribute.
*-
*/

/* Include files. */
/* ============== */
/* Interface definitions. */
/* ---------------------- */
#include "mapping.h"             /* Coordinate mappings (parent class) */

#if defined(astCLASS)            /* Protected */
#include "pointset.h"            /* Sets of points/coordinates */
#include "channel.h"             /* I/O channels */
#endif

/* C header files. */
/* --------------- */
#if defined(astCLASS)            /* Protected */
#include <stddef.h>
#endif

/* Macros */
/* ====== */

/* Define a dummy __attribute__ macro for use on non-GNU compilers. */
#ifndef __GNUC__
#  define  __attribute__(x)  /*NOTHING*/
#endif

/* Type Definitions. */
/* ================= */
/* SphMap structure. */
/* ------------------ */
/* This structure contains all information that is unique to each object in
   the class (e.g. its instance variables). */
typedef struct AstSphMap {

/* Attributes inherited from the parent class. */
   AstMapping mapping;           /* Parent class structure */

/* Attributes specific to objects in this class. */
   double polarlong;             /* Longitude to assign to either pole */
   int unitradius;               /* Are input vectors always of unit length? */
} AstSphMap;

/* Virtual function table. */
/* ----------------------- */
/* This table contains all information that is the same for all
   objects in the class (e.g. pointers to its virtual functions). */
#if defined(astCLASS)            /* Protected */
typedef struct AstSphMapVtab {

/* Properties (e.g. methods) inherited from the parent class. */
   AstMappingVtab mapping_vtab;  /* Parent class virtual function table */

/* A Unique identifier to determine class membership. */
   AstClassIdentifier id;

/* Properties (e.g. methods) specific to this class. */
   int (* GetUnitRadius)( AstSphMap *, int * );
   int (* TestUnitRadius)( AstSphMap *, int * );
   void (* ClearUnitRadius)( AstSphMap *, int * );
   void (* SetUnitRadius)( AstSphMap *, int, int * );

   double (* GetPolarLong)( AstSphMap *, int * );
   int (* TestPolarLong)( AstSphMap *, int * );
   void (* ClearPolarLong)( AstSphMap *, int * );
   void (* SetPolarLong)( AstSphMap *, double, int * );
} AstSphMapVtab;

#if defined(THREAD_SAFE)

/* Define a structure holding all data items that are global within this
   class. */
typedef struct AstSphMapGlobals {
   AstSphMapVtab Class_Vtab;
   int Class_Init;
   char GetAttrib_Buff[ 51 ];
} AstSphMapGlobals;
#endif

#endif

/* Function prototypes. */
/* ==================== */
/* Prototypes for standard class functions. */
/* ---------------------------------------- */
astPROTO_CHECK(SphMap)          /* Check class membership */
astPROTO_ISA(SphMap)            /* Test class membership */

/* Constructor. */
#if defined(astCLASS)            /* Protected. */
AstSphMap *astSphMap_( const char *, int *, ...);
#else
AstSphMap *astSphMapId_( const char *, ...)__attribute__((format(printf,1,2)));
#endif

#if defined(astCLASS)            /* Protected */

/* Initialiser. */
AstSphMap *astInitSphMap_( void *, size_t, int, AstSphMapVtab *,
                           const char *, int * );

/* Vtab initialiser. */
void astInitSphMapVtab_( AstSphMapVtab *, const char *, int * );

/* Loader. */
AstSphMap *astLoadSphMap_( void *, size_t, AstSphMapVtab *,
                           const char *, AstChannel *, int * );

/* Thread-safe initialiser for all global data used by this module. */
#if defined(THREAD_SAFE)
void astInitSphMapGlobals_( AstSphMapGlobals * );
#endif

#endif

/* Prototypes for member functions. */
/* -------------------------------- */
# if defined(astCLASS)           /* Protected */
int astGetUnitRadius_( AstSphMap *, int * );
int astTestUnitRadius_( AstSphMap *, int * );
void astClearUnitRadius_( AstSphMap *, int * );
void astSetUnitRadius_( AstSphMap *, int, int * );

double astGetPolarLong_( AstSphMap *, int * );
int astTestPolarLong_( AstSphMap *, int * );
void astClearPolarLong_( AstSphMap *, int * );
void astSetPolarLong_( AstSphMap *, double, int * );
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
#define astCheckSphMap(this) astINVOKE_CHECK(SphMap,this,0)
#define astVerifySphMap(this) astINVOKE_CHECK(SphMap,this,1)

/* Test class membership. */
#define astIsASphMap(this) astINVOKE_ISA(SphMap,this)

/* Constructor. */
#if defined(astCLASS)            /* Protected. */
#define astSphMap astINVOKE(F,astSphMap_)
#else
#define astSphMap astINVOKE(F,astSphMapId_)
#endif

#if defined(astCLASS)            /* Protected */

/* Initialiser. */
#define \
astInitSphMap(mem,size,init,vtab,name) \
astINVOKE(O,astInitSphMap_(mem,size,init,vtab,name,STATUS_PTR))

/* Vtab Initialiser. */
#define astInitSphMapVtab(vtab,name) astINVOKE(V,astInitSphMapVtab_(vtab,name,STATUS_PTR))
/* Loader. */
#define astLoadSphMap(mem,size,vtab,name,channel) \
astINVOKE(O,astLoadSphMap_(mem,size,vtab,name,astCheckChannel(channel),STATUS_PTR))
#endif

/* Interfaces to public member functions. */
/* -------------------------------------- */
/* Here we make use of astCheckSphMap to validate SphMap pointers
   before use.  This provides a contextual error report if a pointer
   to the wrong sort of Object is supplied. */

#if defined(astCLASS)            /* Protected */
#define astClearUnitRadius(this)     astINVOKE(V,astClearUnitRadius_(astCheckSphMap(this),STATUS_PTR))
#define astGetUnitRadius(this)       astINVOKE(V,astGetUnitRadius_(astCheckSphMap(this),STATUS_PTR))
#define astSetUnitRadius(this,value) astINVOKE(V,astSetUnitRadius_(astCheckSphMap(this),value,STATUS_PTR))
#define astTestUnitRadius(this)      astINVOKE(V,astTestUnitRadius_(astCheckSphMap(this),STATUS_PTR))

#define astClearPolarLong(this)     astINVOKE(V,astClearPolarLong_(astCheckSphMap(this),STATUS_PTR))
#define astGetPolarLong(this)       astINVOKE(V,astGetPolarLong_(astCheckSphMap(this),STATUS_PTR))
#define astSetPolarLong(this,value) astINVOKE(V,astSetPolarLong_(astCheckSphMap(this),value,STATUS_PTR))
#define astTestPolarLong(this)      astINVOKE(V,astTestPolarLong_(astCheckSphMap(this),STATUS_PTR))
#endif

#endif





