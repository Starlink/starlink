#if !defined( SWITCHMAP_INCLUDED )  /* Include this file only once */
#define SWITCHMAP_INCLUDED
/*
*+
*  Name:
*     switchmap.h

*  Type:
*     C include file.

*  Purpose:
*     Define the interface to the SwitchMap class.

*  Invocation:
*     #include "switchmap.h"

*  Description:
*     This include file defines the interface to the SwitchMap class and
*     provides the type definitions, function prototypes and macros,
*     etc. needed to use this class.

*  Inheritance:
*     The SwitchMap class inherits from the Mapping class.

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
*           Merge a SwitchMap within a sequence of Mappings.
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
*        astIsASwitchMap
*           Test class membership.
*        astSwitchMap
*           Create a SwitchMap.
*
*     Protected:
*        astCheckSwitchMap
*           Validate class membership.
*        astInitSwitchMap
*           Initialise a SwitchMap.
*        astInitSwitchMapVtab
*           Initialise the virtual function table for the SwitchMap class.
*        astLoadSwitchMap
*           Load a SwitchMap.

*  Macros:
*     None.

*  Type Definitions:
*     Public:
*        AstSwitchMap
*           SwitchMap object type.
*
*     Protected:
*        AstSwitchMapVtab
*           SwitchMap virtual function table type.

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
/* SwitchMap structure. */
/* ----------------- */
/* This structure contains all information that is unique to each
   object in the class (e.g. its instance variables). */
typedef struct AstSwitchMap {

/* Attributes inherited from the parent class. */
   AstMapping mapping;       /* Parent class structure */

/* Attributes specific to objects in this class. */
   AstMapping *fsmap;       /* Pointer to forward selector Mapping */
   AstMapping *ismap;       /* Pointer to inverse selector Mapping */
   int fsinv;               /* Inversion flag for forward selector Mapping */
   int isinv;               /* Inversion flag for inverse selector Mapping */
   int nroute;              /* The number of route Mappings in the SwitchMap */
   AstMapping **routemap;   /* Array of route Mapping pointers */
   int *routeinv;           /* Array of inversion flags for route Mappings */
} AstSwitchMap;

/* Virtual function table. */
/* ----------------------- */
/* This table contains all information that is the same for all
   objects in the class (e.g. pointers to its virtual functions). */
#if defined(astCLASS)            /* Protected */
typedef struct AstSwitchMapVtab {

/* Properties (e.g. methods) inherited from the parent class. */
   AstMappingVtab mapping_vtab;  /* Parent class virtual function table */

/* A Unique identifier to determine class membership. */
   AstClassIdentifier id;

/* Properties (e.g. methods) specific to this class. */
/* None. */
} AstSwitchMapVtab;

#if defined(THREAD_SAFE)

/* Define a structure holding all data items that are global within the
   object.c file. */

typedef struct AstSwitchMapGlobals {
   AstSwitchMapVtab Class_Vtab;
   int Class_Init;
} AstSwitchMapGlobals;


/* Thread-safe initialiser for all global data used by this module. */
void astInitSwitchMapGlobals_( AstSwitchMapGlobals * );

#endif


#endif

/* Function prototypes. */
/* ==================== */
/* Prototypes for standard class functions. */
/* ---------------------------------------- */
astPROTO_CHECK(SwitchMap)           /* Check class membership */
astPROTO_ISA(SwitchMap)             /* Test class membership */

/* Constructor. */
#if defined(astCLASS)            /* Protected. */
AstSwitchMap *astSwitchMap_( void *, void *, int, void **, const char *, int *, ...);
#else
AstSwitchMap *astSwitchMapId_( void *, void *, int, void **, const char *, ... )__attribute__((format(printf,5,6)));
#endif

#if defined(astCLASS)            /* Protected */

/* Initialiser. */
AstSwitchMap *astInitSwitchMap_( void *, size_t, int, AstSwitchMapVtab *,
                                 const char *, AstMapping *, AstMapping *,
                                 int, AstMapping **, int * );

/* Vtab initialiser. */
void astInitSwitchMapVtab_( AstSwitchMapVtab *, const char *, int * );

/* Loader. */
AstSwitchMap *astLoadSwitchMap_( void *, size_t, AstSwitchMapVtab *,
                                 const char *, AstChannel *, int * );
#endif

/* Prototypes for member functions. */
/* -------------------------------- */
#if defined(astCLASS)            /* Protected */

int astSwitchList_( AstSwitchMap *, int, int *, AstMapping ***, int **, int * );

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
#define astCheckSwitchMap(this) astINVOKE_CHECK(SwitchMap,this,0)
#define astVerifySwitchMap(this) astINVOKE_CHECK(SwitchMap,this,1)

/* Test class membership. */
#define astIsASwitchMap(this) astINVOKE_ISA(SwitchMap,this)

/* Constructor. */
#if defined(astCLASS)            /* Protected. */
#define astSwitchMap astINVOKE(F,astSwitchMap_)
#else
#define astSwitchMap astINVOKE(F,astSwitchMapId_)
#endif

#if defined(astCLASS)            /* Protected */

/* Initialiser. */
#define astInitSwitchMap(mem,size,init,vtab,name,fsmap,ismap,nroute,routemaps) \
astINVOKE(O,astInitSwitchMap_(mem,size,init,vtab,name,astCheckMapping(fsmap),\
          astCheckMapping(ismap),nroute,routemaps,STATUS_PTR))

/* Vtab Initialiser. */
#define astInitSwitchMapVtab(vtab,name) astINVOKE(V,astInitSwitchMapVtab_(vtab,name,STATUS_PTR))
/* Loader. */
#define astLoadSwitchMap(mem,size,vtab,name,channel) \
astINVOKE(O,astLoadSwitchMap_(mem,size,vtab,name,astCheckChannel(channel),STATUS_PTR))

#define astSwitchList astSwitchList_

#endif

/* Interfaces to public member functions. */
/* -------------------------------------- */
/* Here we make use of astCheckSwitchMap to validate SwitchMap pointers
   before use.  This provides a contextual error report if a pointer
   to the wrong sort of Object is supplied. */
/* None. */
#endif





