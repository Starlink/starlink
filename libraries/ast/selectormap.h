#if !defined( SELECTORMAP_INCLUDED )  /* Include this file only once */
#define SELECTORMAP_INCLUDED
/*
*+
*  Name:
*     selectormap.h

*  Type:
*     C include file.

*  Purpose:
*     Define the interface to the SelectorMap class.

*  Invocation:
*     #include "selectormap.h"

*  Description:
*     This include file defines the interface to the SelectorMap class and
*     provides the type definitions, function prototypes and macros,
*     etc. needed to use this class.

*  Inheritance:
*     The SelectorMap class inherits from the Mapping class.

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
*           Merge a SelectorMap within a sequence of Mappings.
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
*        astIsASelectorMap
*           Test class membership.
*        astSelectorMap
*           Create a SelectorMap.
*
*     Protected:
*        astCheckSelectorMap
*           Validate class membership.
*        astInitSelectorMap
*           Initialise a SelectorMap.
*        astInitSelectorMapVtab
*           Initialise the virtual function table for the SelectorMap class.
*        astLoadSelectorMap
*           Load a SelectorMap.

*  Macros:
*     None.

*  Type Definitions:
*     Public:
*        AstSelectorMap
*           SelectorMap object type.
*
*     Protected:
*        AstSelectorMapVtab
*           SelectorMap virtual function table type.

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
*     13-MAR-2006 (DSB):
*        Original version.
*-
*/

/* Include files. */
/* ============== */
/* Interface definitions. */
/* ---------------------- */
#include "mapping.h"             /* Coordinate Mappings (parent class) */
#include "region.h"              /* Coordinate Regions (parent class) */

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
/* SelectorMap structure. */
/* ----------------- */
/* This structure contains all information that is unique to each
   object in the class (e.g. its instance variables). */
typedef struct AstSelectorMap {

/* Attributes inherited from the parent class. */
   AstMapping mapping;       /* Parent class structure */

/* Attributes specific to objects in this class. */
   int nreg;                /* The number of Regions in the SelectorMap */
   AstRegion **reg;         /* Array of Region pointers */
   double badval;           /* Output value for positions with bad axis values */

} AstSelectorMap;

/* Virtual function table. */
/* ----------------------- */
/* This table contains all information that is the same for all
   objects in the class (e.g. pointers to its virtual functions). */
#if defined(astCLASS)            /* Protected */
typedef struct AstSelectorMapVtab {

/* Properties (e.g. methods) inherited from the parent class. */
   AstMappingVtab mapping_vtab;  /* Parent class virtual function table */

/* A Unique identifier to determine class membership. */
   AstClassIdentifier id;

/* Properties (e.g. methods) specific to this class. */
/* None. */
} AstSelectorMapVtab;

#if defined(THREAD_SAFE)

/* Define a structure holding all data items that are global within the
   object.c file. */

typedef struct AstSelectorMapGlobals {
   AstSelectorMapVtab Class_Vtab;
   int Class_Init;
} AstSelectorMapGlobals;


/* Thread-safe initialiser for all global data used by this module. */
void astInitSelectorMapGlobals_( AstSelectorMapGlobals * );

#endif


#endif

/* Function prototypes. */
/* ==================== */
/* Prototypes for standard class functions. */
/* ---------------------------------------- */
astPROTO_CHECK(SelectorMap)           /* Check class membership */
astPROTO_ISA(SelectorMap)             /* Test class membership */

/* Constructor. */
#if defined(astCLASS)            /* Protected. */
AstSelectorMap *astSelectorMap_( int, void **, double, const char *, int *, ...);
#else
AstSelectorMap *astSelectorMapId_( int, void **, double, const char *, ... )__attribute__((format(printf,4,5)));
#endif

#if defined(astCLASS)            /* Protected */

/* Initialiser. */
AstSelectorMap *astInitSelectorMap_( void *, size_t, int, AstSelectorMapVtab *,
                                     const char *, int, AstRegion **, double, int * );

/* Vtab initialiser. */
void astInitSelectorMapVtab_( AstSelectorMapVtab *, const char *, int * );

/* Loader. */
AstSelectorMap *astLoadSelectorMap_( void *, size_t, AstSelectorMapVtab *,
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
#define astCheckSelectorMap(this) astINVOKE_CHECK(SelectorMap,this,0)
#define astVerifySelectorMap(this) astINVOKE_CHECK(SelectorMap,this,1)

/* Test class membership. */
#define astIsASelectorMap(this) astINVOKE_ISA(SelectorMap,this)

/* Constructor. */
#if defined(astCLASS)            /* Protected. */
#define astSelectorMap astINVOKE(F,astSelectorMap_)
#else
#define astSelectorMap astINVOKE(F,astSelectorMapId_)
#endif

#if defined(astCLASS)            /* Protected */

/* Initialiser. */
#define astInitSelectorMap(mem,size,init,vtab,name,nreg,regs,badval) \
astINVOKE(O,astInitSelectorMap_(mem,size,init,vtab,name,nreg,regs,badval,STATUS_PTR))

/* Vtab Initialiser. */
#define astInitSelectorMapVtab(vtab,name) astINVOKE(V,astInitSelectorMapVtab_(vtab,name,STATUS_PTR))
/* Loader. */
#define astLoadSelectorMap(mem,size,vtab,name,channel) \
astINVOKE(O,astLoadSelectorMap_(mem,size,vtab,name,astCheckChannel(channel),STATUS_PTR))
#endif

/* Interfaces to public member functions. */
/* -------------------------------------- */
/* Here we make use of astCheckSelectorMap to validate SelectorMap pointers
   before use.  This provides a contextual error report if a pointer
   to the wrong sort of Object is supplied. */
/* None. */
#endif





