#if !defined( CMPMAP_INCLUDED )  /* Include this file only once */
#define CMPMAP_INCLUDED
/*
*+
*  Name:
*     cmpmap.h

*  Type:
*     C include file.

*  Purpose:
*     Define the interface to the CmpMap class.

*  Invocation:
*     #include "cmpmap.h"

*  Description:
*     This include file defines the interface to the CmpMap class and
*     provides the type definitions, function prototypes and macros,
*     etc. needed to use this class.
*
*     A CmpMap is a compound Mapping which allows two component
*     Mappings (of any class) to be connected together to form a more
*     complex Mapping. This connection may either be "in series"
*     (where the first Mapping is used to transform the coordinates of
*     each point and the second mapping is then applied to the
*     result), or "in parallel" (where one Mapping transforms the
*     earlier coordinates for each point and the second Mapping
*     simultaneously transforms the later coordinates).
*
*     Since a CmpMap is itself a Mapping, it can be used as a
*     component in forming further CmpMaps. Mappings of arbitrary
*     complexity may be built from simple individual Mappings in this
*     way.

*  Inheritance:
*     The CmpMap class inherits from the Mapping class.

*  Attributes Over-Ridden:
*     None.

*  New Attributes Defined:
*     None.

*  Methods Over-Ridden:
*     Public:
*        astSimplify
*           Simplify a CmpMap.
*
*     Protected:
*        astMapList
*           Decompose a CmpMap into a sequence of simpler Mappings.
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
*        astIsACmpMap
*           Test class membership.
*        astCmpMap
*           Create a CmpMap.
*
*     Protected:
*        astCheckCmpMap
*           Validate class membership.
*        astInitCmpMap
*           Initialise a CmpMap.
*        astInitCmpMapVtab
*           Initialise the virtual function table for the CmpMap class.
*        astLoadCmpMap
*           Load a CmpMap.

*  Macros:
*     None.

*  Type Definitions:
*     Public:
*        AstCmpMap
*           CmpMap object type.
*
*     Protected:
*        AstCmpMapVtab
*           CmpMap virtual function table type.

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
*     6-FEB-1996 (RFWS):
*        Original version.
*     25-SEP-1996 (RFWS):
*        Implemented external interface and I/O facilities.
*     13-DEC-1996 (RFWS):
*        Over-ride the astSimplify method.
*     8-JAN-2003 (DSB):
*        Added protected astInitCmpMapVtab method.
*-
*/

/* Include files. */
/* ============== */
/* Interface definitions. */
/* ---------------------- */
#include "mapping.h"             /* Coordinate Mappings (parent class) */

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
/* CmpMap structure. */
/* ----------------- */
/* This structure contains all information that is unique to each
   object in the class (e.g. its instance variables). */
typedef struct AstCmpMap {

/* Attributes inherited from the parent class. */
   AstMapping mapping;           /* Parent class structure */

/* Attributes specific to objects in this class. */
   AstMapping *map1;              /* Pointer to first Mapping */
   AstMapping *map2;              /* Pointer to second Mapping */
   char invert1;                  /* Inversion flag for first Mapping */
   char invert2;                  /* Inversion flag for second Mapping */
   char series;                   /* Connect in series (else in parallel)? */
} AstCmpMap;

/* Virtual function table. */
/* ----------------------- */
/* This table contains all information that is the same for all
   objects in the class (e.g. pointers to its virtual functions). */
#if defined(astCLASS)            /* Protected */
typedef struct AstCmpMapVtab {

/* Properties (e.g. methods) inherited from the parent class. */
   AstMappingVtab mapping_vtab;  /* Parent class virtual function table */

/* A Unique identifier to determine class membership. */
   AstClassIdentifier id;

/* Properties (e.g. methods) specific to this class. */
/* None. */
} AstCmpMapVtab;

#if defined(THREAD_SAFE)

/* Define a structure holding all data items that are global within this
   class. */
typedef struct AstCmpMapGlobals {
   AstCmpMapVtab Class_Vtab;
   int Class_Init;
   int Simplify_Depth;
   AstMapping **Simplify_Stackmaps;
} AstCmpMapGlobals;

#endif

#endif

/* Function prototypes. */
/* ==================== */
/* Prototypes for standard class functions. */
/* ---------------------------------------- */
astPROTO_CHECK(CmpMap)           /* Check class membership */
astPROTO_ISA(CmpMap)             /* Test class membership */

/* Constructor. */
#if defined(astCLASS)            /* Protected. */
AstCmpMap *astCmpMap_( void *, void *, int, const char *, int *, ...);
#else
AstCmpMap *astCmpMapId_( void *, void *, int, const char *, ... )__attribute__((format(printf,4,5)));
#endif

#if defined(astCLASS)            /* Protected */

/* Initialiser. */
AstCmpMap *astInitCmpMap_( void *, size_t, int, AstCmpMapVtab *,
                           const char *, AstMapping *, AstMapping *, int, int * );

/* Vtab initialiser. */
void astInitCmpMapVtab_( AstCmpMapVtab *, const char *, int * );

/* Loader. */
AstCmpMap *astLoadCmpMap_( void *, size_t, AstCmpMapVtab *,
                           const char *, AstChannel *, int * );

/* Thread-safe initialiser for all global data used by this module. */
#if defined(THREAD_SAFE)
void astInitCmpMapGlobals_( AstCmpMapGlobals * );
#endif

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
#define astCheckCmpMap(this) astINVOKE_CHECK(CmpMap,this,0)
#define astVerifyCmpMap(this) astINVOKE_CHECK(CmpMap,this,1)

/* Test class membership. */
#define astIsACmpMap(this) astINVOKE_ISA(CmpMap,this)

/* Constructor. */
#if defined(astCLASS)            /* Protected. */
#define astCmpMap astINVOKE(F,astCmpMap_)
#else
#define astCmpMap astINVOKE(F,astCmpMapId_)
#endif

#if defined(astCLASS)            /* Protected */

/* Initialiser. */
#define astInitCmpMap(mem,size,init,vtab,name,map1,map2,series) \
astINVOKE(O,astInitCmpMap_(mem,size,init,vtab,name,astCheckMapping(map1),astCheckMapping(map2),series,STATUS_PTR))

/* Vtab Initialiser. */
#define astInitCmpMapVtab(vtab,name) astINVOKE(V,astInitCmpMapVtab_(vtab,name,STATUS_PTR))
/* Loader. */
#define astLoadCmpMap(mem,size,vtab,name,channel) \
astINVOKE(O,astLoadCmpMap_(mem,size,vtab,name,astCheckChannel(channel),STATUS_PTR))
#endif

/* Interfaces to public member functions. */
/* -------------------------------------- */
/* Here we make use of astCheckCmpMap to validate CmpMap pointers
   before use.  This provides a contextual error report if a pointer
   to the wrong sort of Object is supplied. */
/* None. */
#endif





