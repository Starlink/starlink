#if !defined( UNITMAP_INCLUDED ) /* Include this file only once */
#define UNITMAP_INCLUDED
/*
*+
*  Name:
*     unitmap.h

*  Type:
*     C include file.

*  Purpose:
*     Define the interface to the UnitMap class.

*  Invocation:
*     #include "unitmap.h"

*  Description:
*     This include file defines the interface to the UnitMap class and
*     provides the type definitions, function prototypes and macros,
*     etc.  needed to use this class.
*
*     The UnitMap class implements Mappings that perform an identity
*     (unit) coordinate transformation, simply by copying each
*     coordinate value from input to output (and similarly for the
*     inverse transformation). UnitMaps are therefore of little use
*     for converting coordinates, but they serve a useful role as
*     "null" Mappings in cases where a Mapping is needed (e.g.  to
*     pass to a function), but no coordinate transformation is wanted.

*  Inheritance:
*     The UnitMap class inherits from the Mapping class.

*  Attributes Over-Ridden:
*     None.

*  New Attributes Defined:
*     None.

*  Methods Over-Ridden:
*     Public:
*        None.
*
*     Protected:
*        astMapMerge
*           Simplify a sequence of Mappings containing a UnitMap.
*        astTransform
*           Transform a set of points.

*  New Methods Defined:
*     Public:
*        None.
*
*     Protected:
*        None.

*  Other Class Functions:
*     Public:
*        astIsAUnitMap
*           Test class membership.
*        astUnitMap
*           Create a UnitMap.
*
*     Protected:
*        astCheckUnitMap
*           Validate class membership.
*        astInitUnitMap
*           Initialise a UnitMap.
*        astInitUnitMapVtab
*           Initialise the virtual function table for the UnitMap class.
*        astLoadUnitMap
*           Load a UnitMap.

*  Macros:
*     None.

*  Type Definitions:
*     Public:
*        AstUnitMap
*           UnitMap object type.
*
*     Protected:
*        AstUnitMapVtab
*           UnitMap virtual function table type.

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
*     DSB: David S. Berry (Starlink)

*  History:
*     7-FEB-1996 (RFWS):
*        Original version.
*     13-DEC-1996 (RFWS):
*        Over-ride the astMapMerge method.
*     8-JAN-2003 (DSB):
*        Added protected astInitUnitMapVtab method.
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
/* UnitMap structure. */
/* ------------------ */
/* This structure contains all information that is unique to each
   object in the class (e.g. its instance variables). */
typedef struct AstUnitMap {

/* Attributes inherited from the parent class. */
   AstMapping mapping;           /* Parent class structure */

/* Attributes specific to objects in this class. */
/* None. */
} AstUnitMap;

/* Virtual function table. */
/* ----------------------- */
/* This table contains all information that is the same for all
   objects in the class (e.g. pointers to its virtual functions). */
#if defined(astCLASS)            /* Protected */
typedef struct AstUnitMapVtab {

/* Properties (e.g. methods) inherited from the parent class. */
   AstMappingVtab mapping_vtab;  /* Parent class virtual function table */

/* A Unique identifier to determine class membership. */
   AstClassIdentifier id;

/* Properties (e.g. methods) specific to this class. */
/* None. */
} AstUnitMapVtab;

#if defined(THREAD_SAFE)

/* Define a structure holding all data items that are global within the
   object.c file. */

typedef struct AstUnitMapGlobals {
   AstUnitMapVtab Class_Vtab;
   int Class_Init;
} AstUnitMapGlobals;


/* Thread-safe initialiser for all global data used by this module. */
void astInitUnitMapGlobals_( AstUnitMapGlobals * );

#endif


#endif

/* Function prototypes. */
/* ==================== */
/* Prototypes for standard class functions. */
/* ---------------------------------------- */
/* Prototypes for standard class functions. */
/* ---------------------------------------- */
astPROTO_CHECK(UnitMap)          /* Check class membership */
astPROTO_ISA(UnitMap)            /* Test class membership */

/* Constructor. */
#if defined(astCLASS)            /* Protected. */
AstUnitMap *astUnitMap_( int, const char *, int *, ...);
#else
AstUnitMap *astUnitMapId_( int, const char *, ... )__attribute__((format(printf,2,3)));
#endif

#if defined(astCLASS)            /* Protected */

/* Initialiser. */
AstUnitMap *astInitUnitMap_( void *, size_t, int, AstUnitMapVtab *,
                             const char *, int, int * );

/* Vtab initialiser. */
void astInitUnitMapVtab_( AstUnitMapVtab *, const char *, int * );

/* Loader. */
AstUnitMap *astLoadUnitMap_( void *, size_t, AstUnitMapVtab *,
                             const char *, AstChannel *, int * );
#endif

/* Prototypes for member functions. */
/* -------------------------------- */
/* None. */

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
#define astCheckUnitMap(this) astINVOKE_CHECK(UnitMap,this,0)
#define astVerifyUnitMap(this) astINVOKE_CHECK(UnitMap,this,1)

/* Test class membership. */
#define astIsAUnitMap(this) astINVOKE_ISA(UnitMap,this)

/* Constructor. */
#if defined(astCLASS)            /* Protected. */
#define astUnitMap astINVOKE(F,astUnitMap_)
#else
#define astUnitMap astINVOKE(F,astUnitMapId_)
#endif

#if defined(astCLASS)            /* Protected */

/* Initialiser. */
#define astInitUnitMap(mem,size,init,vtab,name,ncoord) \
astINVOKE(O,astInitUnitMap_(mem,size,init,vtab,name,ncoord,STATUS_PTR))

/* Vtab Initialiser. */
#define astInitUnitMapVtab(vtab,name) astINVOKE(V,astInitUnitMapVtab_(vtab,name,STATUS_PTR))
/* Loader. */
#define astLoadUnitMap(mem,size,vtab,name,channel) \
astINVOKE(O,astLoadUnitMap_(mem,size,vtab,name,astCheckChannel(channel),STATUS_PTR))
#endif

/* Interfaces to public member functions. */
/* -------------------------------------- */
/* Here we make use of astCheckUnitMap to validate UnitMap pointers
   before use.  This provides a contextual error report if a pointer
   to the wrong sort of Object is supplied. */
/* None. */
#endif





