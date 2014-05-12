#if !defined( RATEMAP_INCLUDED )  /* Include this file only once */
#define RATEMAP_INCLUDED
/*
*+
*  Name:
*     ratemap.h

*  Type:
*     C include file.

*  Purpose:
*     Define the interface to the RateMap class.

*  Invocation:
*     #include "ratemap.h"

*  Description:
*     This include file defines the interface to the RateMap class and
*     provides the type definitions, function prototypes and macros,
*     etc. needed to use this class.

*  Inheritance:
*     The RateMap class inherits from the Mapping class.

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
*           Merge a RateMap within a sequence of Mappings.
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
*        astIsARateMap
*           Test class membership.
*        astRateMap
*           Create a RateMap.
*
*     Protected:
*        astCheckRateMap
*           Validate class membership.
*        astInitRateMap
*           Initialise a RateMap.
*        astInitRateMapVtab
*           Initialise the virtual function table for the RateMap class.
*        astLoadRateMap
*           Load a RateMap.

*  Macros:
*     None.

*  Type Definitions:
*     Public:
*        AstRateMap
*           RateMap object type.
*
*     Protected:
*        AstRateMapVtab
*           RateMap virtual function table type.

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
*     7-DEC-2004 (DSB):
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
/* RateMap structure. */
/* ----------------- */
/* This structure contains all information that is unique to each
   object in the class (e.g. its instance variables). */
typedef struct AstRateMap {

/* Attributes inherited from the parent class. */
   AstMapping mapping;           /* Parent class structure */

/* Attributes specific to objects in this class. */
   AstMapping *map;              /* Pointer to the Mapping */
   int invert;                   /* Inversion flag for Mapping */
   int iin;                      /* Index of Mapping input to vary */
   int iout;                     /* Index of Mapping output to measure */
} AstRateMap;

/* Virtual function table. */
/* ----------------------- */
/* This table contains all information that is the same for all
   objects in the class (e.g. pointers to its virtual functions). */
#if defined(astCLASS)            /* Protected */
typedef struct AstRateMapVtab {

/* Properties (e.g. methods) inherited from the parent class. */
   AstMappingVtab mapping_vtab;  /* Parent class virtual function table */

/* A Unique identifier to determine class membership. */
   AstClassIdentifier id;

/* Properties (e.g. methods) specific to this class. */
/* None. */
} AstRateMapVtab;

#if defined(THREAD_SAFE)

/* Define a structure holding all data items that are global within the
   object.c file. */

typedef struct AstRateMapGlobals {
   AstRateMapVtab Class_Vtab;
   int Class_Init;
} AstRateMapGlobals;


/* Thread-safe initialiser for all global data used by this module. */
void astInitRateMapGlobals_( AstRateMapGlobals * );

#endif


#endif

/* Function prototypes. */
/* ==================== */
/* Prototypes for standard class functions. */
/* ---------------------------------------- */
astPROTO_CHECK(RateMap)           /* Check class membership */
astPROTO_ISA(RateMap)             /* Test class membership */

/* Constructor. */
#if defined(astCLASS)            /* Protected. */
AstRateMap *astRateMap_( void *, int, int, const char *, int *, ...);
#else
AstRateMap *astRateMapId_( void *, int, int, const char *, ... )__attribute__((format(printf,4,5)));
#endif

#if defined(astCLASS)            /* Protected */

/* Initialiser. */
AstRateMap *astInitRateMap_( void *, size_t, int, AstRateMapVtab *,
                             const char *, AstMapping *, int, int, int * );

/* Vtab initialiser. */
void astInitRateMapVtab_( AstRateMapVtab *, const char *, int * );

/* Loader. */
AstRateMap *astLoadRateMap_( void *, size_t, AstRateMapVtab *,
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
#define astCheckRateMap(this) astINVOKE_CHECK(RateMap,this,0)
#define astVerifyRateMap(this) astINVOKE_CHECK(RateMap,this,1)

/* Test class membership. */
#define astIsARateMap(this) astINVOKE_ISA(RateMap,this)

/* Constructor. */
#if defined(astCLASS)            /* Protected. */
#define astRateMap astINVOKE(F,astRateMap_)
#else
#define astRateMap astINVOKE(F,astRateMapId_)
#endif

#if defined(astCLASS)            /* Protected */

/* Initialiser. */
#define astInitRateMap(mem,size,init,vtab,name,map,iin,iout) \
astINVOKE(O,astInitRateMap_(mem,size,init,vtab,name,astCheckMapping(map),iin,iout,STATUS_PTR))

/* Vtab Initialiser. */
#define astInitRateMapVtab(vtab,name) astINVOKE(V,astInitRateMapVtab_(vtab,name,STATUS_PTR))
/* Loader. */
#define astLoadRateMap(mem,size,vtab,name,channel) \
astINVOKE(O,astLoadRateMap_(mem,size,vtab,name,astCheckChannel(channel),STATUS_PTR))
#endif

/* Interfaces to public member functions. */
/* -------------------------------------- */
/* Here we make use of astCheckRateMap to validate RateMap pointers
   before use.  This provides a contextual error report if a pointer
   to the wrong sort of Object is supplied. */
/* None. */
#endif





