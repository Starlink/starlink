#if !defined( WINMAP_INCLUDED ) /* Include this file only once */
#define WINMAP_INCLUDED
/*
*+
*  Name:
*     winmap.h

*  Type:
*     C include file.

*  Purpose:
*     Define the interface to the WinMap class.

*  Invocation:
*     #include "winmap.h"

*  Description:
*     This include file defines the interface to the WinMap class and
*     provides the type definitions, function prototypes and macros,
*     etc.  needed to use this class.
*
*     The WinMap class implements Mappings which maps one window onto
*     another window by scaling and shifting the values on each axis.

*  Inheritance:
*     The WinMap class inherits from the Mapping class.

*  Attributes Over-Ridden:
*     None.

*  New Attributes Defined:
*     None.

*  Methods Over-Ridden:
*     Public:
*        None.
*
*     Protected:
*        ClearAttrib
*           Clear an attribute value for a WinMap.
*        GetAttrib
*           Get an attribute value for a WinMap.
*        SetAttrib
*           Set an attribute value for a WinMap.
*        TestAttrib
*           Test if an attribute value has been set for a WinMap.
*        astMapMerge
*           Simplify a sequence of Mappings containing a WinMap.
*        astTransform
*           Apply a WinMap to transform a set of points.

*  New Methods Defined:
*     Public:
*        None.
*
*     Protected:
*        astWinTerms
*           Obtain copies of the shift and scale terms used by a WinMap.

*  Other Class Functions:
*     Public:
*        astIsAWinMap
*           Test class membership.
*        astWinMap
*           Create a WinMap.
*
*     Protected:
*        astCheckWinMap
*           Validate class membership.
*        astInitWinMap
*           Initialise a WinMap.
*        astInitWinMapVtab
*           Initialise the virtual function table for the WinMap class.
*        astLoadWinMap
*           Load a WinMap.

*  Macros:
*     None.

*  Type Definitions:
*     Public:
*        AstWinMap
*           WinMap object type.
*
*     Protected:
*        AstWinMapVtab
*           WinMap virtual function table type.

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
*     23-OCT-1996 (DSB):
*        Original version.
*     8-JAN-2003 (DSB):
*        Changed private InitVtab method to protected astInitWinMapVtab
*        method.
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
/* WinMap structure. */
/* ------------------ */
/* This structure contains all information that is unique to each object in
   the class (e.g. its instance variables). */
typedef struct AstWinMap {

/* Attributes inherited from the parent class. */
   AstMapping mapping;           /* Parent class structure */

/* Attributes specific to objects in this class. */
   double *a;                   /* Pointer to array of shifts */
   double *b;                   /* Pointer to array of scale factors */

} AstWinMap;

/* Virtual function table. */
/* ----------------------- */
/* This table contains all information that is the same for all
   objects in the class (e.g. pointers to its virtual functions). */
#if defined(astCLASS)            /* Protected */
typedef struct AstWinMapVtab {

/* Properties (e.g. methods) inherited from the parent class. */
   AstMappingVtab mapping_vtab;  /* Parent class virtual function table */

/* A Unique identifier to determine class membership. */
   AstClassIdentifier id;

/* Properties (e.g. methods) specific to this class. */
   int (* WinTerms)( AstWinMap *, double **, double **, int * );

} AstWinMapVtab;

#if defined(THREAD_SAFE)

/* Define a structure holding all data items that are global within the
   object.c file. */

typedef struct AstWinMapGlobals {
   AstWinMapVtab Class_Vtab;
   int Class_Init;
} AstWinMapGlobals;


/* Thread-safe initialiser for all global data used by this module. */
void astInitWinMapGlobals_( AstWinMapGlobals * );

#endif


#endif

/* Function prototypes. */
/* ==================== */
/* Prototypes for standard class functions. */
/* ---------------------------------------- */
astPROTO_CHECK(WinMap)          /* Check class membership */
astPROTO_ISA(WinMap)            /* Test class membership */

/* Constructor. */
#if defined(astCLASS)            /* Protected. */
AstWinMap *astWinMap_( int, const double [], const double [], const double [], const double [], const char *, int *, ...);
#else
AstWinMap *astWinMapId_( int, const double [], const double [], const double [], const double [], const char *, ... )__attribute__((format(printf,6,7)));
#endif

#if defined(astCLASS)            /* Protected */

/* Initialiser. */
AstWinMap *astInitWinMap_( void *, size_t, int, AstWinMapVtab *,
                           const char *, int, const double *, const double *,
                           const double *, const double *, int * );

/* Vtab initialiser. */
void astInitWinMapVtab_( AstWinMapVtab *, const char *, int * );

/* Loader. */
AstWinMap *astLoadWinMap_( void *, size_t, AstWinMapVtab *,
                           const char *, AstChannel *, int * );
#endif

/* Prototypes for member functions. */
/* -------------------------------- */
# if defined(astCLASS)           /* Protected */
int astWinTerms_( AstWinMap *, double **, double **, int * );
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
#define astCheckWinMap(this) astINVOKE_CHECK(WinMap,this,0)
#define astVerifyWinMap(this) astINVOKE_CHECK(WinMap,this,1)

/* Test class membership. */
#define astIsAWinMap(this) astINVOKE_ISA(WinMap,this)

/* Constructor. */
#if defined(astCLASS)            /* Protected. */
#define astWinMap astINVOKE(F,astWinMap_)
#else
#define astWinMap astINVOKE(F,astWinMapId_)
#endif

#if defined(astCLASS)            /* Protected */

/* Initialiser. */
#define \
astInitWinMap(mem,size,init,vtab,name,ncoord,c1_in,c2_in,c1_out,c2_out) \
astINVOKE(O,astInitWinMap_(mem,size,init,vtab,name,ncoord,c1_in,c2_in,c1_out,c2_out,STATUS_PTR))

/* Vtab Initialiser. */
#define astInitWinMapVtab(vtab,name) astINVOKE(V,astInitWinMapVtab_(vtab,name,STATUS_PTR))
/* Loader. */
#define astLoadWinMap(mem,size,vtab,name,channel) \
astINVOKE(O,astLoadWinMap_(mem,size,vtab,name,astCheckChannel(channel),STATUS_PTR))
#endif

/* Interfaces to public member functions. */
/* -------------------------------------- */
/* Here we make use of astCheckWinMap to validate WinMap pointers
   before use.  This provides a contextual error report if a pointer
   to the wrong sort of Object is supplied. */

#if defined(astCLASS)            /* Protected */
#define astWinTerms(this,scale,shift) \
astINVOKE(V,astWinTerms_(astCheckWinMap(this),scale,shift,STATUS_PTR))
#endif

#endif





