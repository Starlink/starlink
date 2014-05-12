#if !defined( TRANMAP_INCLUDED )  /* Include this file only once */
#define TRANMAP_INCLUDED
/*
*+
*  Name:
*     tranmap.h

*  Type:
*     C include file.

*  Purpose:
*     Define the interface to the TranMap class.

*  Invocation:
*     #include "tranmap.h"

*  Description:
*     This include file defines the interface to the TranMap class and
*     provides the type definitions, function prototypes and macros,
*     etc. needed to use this class.

*  Inheritance:
*     The TranMap class inherits from the Mapping class.

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
*           Merge a TranMap within a sequence of Mappings.
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
*        astIsATranMap
*           Test class membership.
*        astTranMap
*           Create a TranMap.
*
*     Protected:
*        astCheckTranMap
*           Validate class membership.
*        astInitTranMap
*           Initialise a TranMap.
*        astInitTranMapVtab
*           Initialise the virtual function table for the TranMap class.
*        astLoadTranMap
*           Load a TranMap.

*  Macros:
*     None.

*  Type Definitions:
*     Public:
*        AstTranMap
*           TranMap object type.
*
*     Protected:
*        AstTranMapVtab
*           TranMap virtual function table type.

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
*     DSB: David S. Berry (Starlink)

*  History:
*     10-FEB-2004 (DSB):
*        Original version.
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
/* TranMap structure. */
/* ----------------- */
/* This structure contains all information that is unique to each
   object in the class (e.g. its instance variables). */
typedef struct AstTranMap {

/* Attributes inherited from the parent class. */
   AstMapping mapping;           /* Parent class structure */

/* Attributes specific to objects in this class. */
   AstMapping *map1;             /* Pointer to first Mapping */
   AstMapping *map2;             /* Pointer to second Mapping */
   int invert1;                  /* Inversion flag for first Mapping */
   int invert2;                  /* Inversion flag for second Mapping */
} AstTranMap;

/* Virtual function table. */
/* ----------------------- */
/* This table contains all information that is the same for all
   objects in the class (e.g. pointers to its virtual functions). */
#if defined(astCLASS)            /* Protected */
typedef struct AstTranMapVtab {

/* Properties (e.g. methods) inherited from the parent class. */
   AstMappingVtab mapping_vtab;  /* Parent class virtual function table */

/* A Unique identifier to determine class membership. */
   AstClassIdentifier id;

/* Properties (e.g. methods) specific to this class. */
/* None. */
} AstTranMapVtab;

#if defined(THREAD_SAFE)

/* Define a structure holding all data items that are global within the
   object.c file. */

typedef struct AstTranMapGlobals {
   AstTranMapVtab Class_Vtab;
   int Class_Init;
} AstTranMapGlobals;


/* Thread-safe initialiser for all global data used by this module. */
void astInitTranMapGlobals_( AstTranMapGlobals * );

#endif


#endif

/* Function prototypes. */
/* ==================== */
/* Prototypes for standard class functions. */
/* ---------------------------------------- */
astPROTO_CHECK(TranMap)           /* Check class membership */
astPROTO_ISA(TranMap)             /* Test class membership */

/* Constructor. */
#if defined(astCLASS)            /* Protected. */
AstTranMap *astTranMap_( void *, void *, const char *, int *, ...);
#else
AstTranMap *astTranMapId_( void *, void *, const char *, ... )__attribute__((format(printf,3,4)));
#endif

#if defined(astCLASS)            /* Protected */

/* Initialiser. */
AstTranMap *astInitTranMap_( void *, size_t, int, AstTranMapVtab *,
                           const char *, AstMapping *, AstMapping *, int * );

/* Vtab initialiser. */
void astInitTranMapVtab_( AstTranMapVtab *, const char *, int * );

/* Loader. */
AstTranMap *astLoadTranMap_( void *, size_t, AstTranMapVtab *,
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
#define astCheckTranMap(this) astINVOKE_CHECK(TranMap,this,0)
#define astVerifyTranMap(this) astINVOKE_CHECK(TranMap,this,1)

/* Test class membership. */
#define astIsATranMap(this) astINVOKE_ISA(TranMap,this)

/* Constructor. */
#if defined(astCLASS)            /* Protected. */
#define astTranMap astINVOKE(F,astTranMap_)
#else
#define astTranMap astINVOKE(F,astTranMapId_)
#endif

#if defined(astCLASS)            /* Protected */

/* Initialiser. */
#define astInitTranMap(mem,size,init,vtab,name,map1,map2) \
astINVOKE(O,astInitTranMap_(mem,size,init,vtab,name,astCheckMapping(map1),astCheckMapping(map2),STATUS_PTR))

/* Vtab Initialiser. */
#define astInitTranMapVtab(vtab,name) astINVOKE(V,astInitTranMapVtab_(vtab,name,STATUS_PTR))
/* Loader. */
#define astLoadTranMap(mem,size,vtab,name,channel) \
astINVOKE(O,astLoadTranMap_(mem,size,vtab,name,astCheckChannel(channel),STATUS_PTR))
#endif

/* Interfaces to public member functions. */
/* -------------------------------------- */
/* Here we make use of astCheckTranMap to validate TranMap pointers
   before use.  This provides a contextual error report if a pointer
   to the wrong sort of Object is supplied. */
/* None. */
#endif





