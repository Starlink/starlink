#if !defined( TIMEMAP_INCLUDED )  /* Include this file only once */
#define TIMEMAP_INCLUDED
/*
*+
*  Name:
*     timemap.h

*  Type:
*     C include file.

*  Purpose:
*     Define the interface to the TimeMap class.

*  Invocation:
*     #include "timemap.h"

*  Description:
*     This include file defines the interface to the TimeMap class and
*     provides the type definitions, function prototypes and macros,
*     etc.  needed to use this class.
*
*     The TimeMap class encapsulates various time coordinate
*     conversions. Since, typically, a sequence of these conversions is
*     required, a TimeMap can be used to accumulate a series of conversions
*     which it then applies in sequence.

*  Inheritance:
*     The TimeMap class inherits from the Mapping class.

*  Attributes Over-Ridden:
*     None.

*  New Attributes Defined:
*     None.

*  Methods Over-Ridden:
*     Public:
*        astTransform
*           Use an TimeMap to transform a set of points.

*     Protected:
*        astMapMerge
*           Simplify a sequence of Mappings containing an TimeMap.

*  New Methods Defined:
*     Public:
*        astTimeAdd
*           Add a coordinate conversion step to an TimeMap.

*     Private:
*        None.

*  Other Class Functions:
*     Public:
*        astIsATimeMap
*           Test class membership.
*        astTimeMap
*           Create an TimeMap.

*     Protected:
*        astCheckTimeMap
*           Validate class membership.
*        astInitTimeMap
*           Initialise an TimeMap.
*        astLoadTimeMap
*           Load an TimeMap.

*  Macros:
*     None.

*  Type Definitions:
*     Public:
*        AstTimeMap
*           TimeMap object type.

*     Protected:
*        AstTimeMapVtab
*           TimeMap virtual function table type.

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
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public Licence as
*     published by the Free Software Foundation; either version 2 of
*     the Licence, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public Licence for more details.
*
*     You should have received a copy of the GNU General Public Licence
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     DSB: David S. Berry (Starlink)

*  History:
*     24-MAY-2005 (DSB):
*        Original version.
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

/* TimeMap structure. */
/* ----------------- */
/* This structure contains all information that is unique to each
   object in the class (e.g. its instance variables). */
typedef struct AstTimeMap {

/* Attributes inherited from the parent class. */
   AstMapping mapping;           /* Parent class structure */

/* Attributes specific to objects in this class. */
   int *cvttype;                 /* Pointer to array of conversion types */
   double **cvtargs;             /* Pointer to argument list pointer array */
   int ncvt;                     /* Number of conversions to perform */
} AstTimeMap;

/* Virtual function table. */
/* ----------------------- */
/* This table contains all information that is the same for all
   objects in the class (e.g. pointers to its virtual functions). */
#if defined(astCLASS)            /* Protected */
typedef struct AstTimeMapVtab {

/* Properties (e.g. methods) inherited from the parent class. */
   AstMappingVtab mapping_vtab;  /* Parent class virtual function table */

/* A Unique identifier to determine class membership. */
   AstClassIdentifier id;

/* Properties (e.g. methods) specific to this class. */
   void (* TimeAdd)( AstTimeMap *, const char *, const double[], int * );
} AstTimeMapVtab;

#if defined(THREAD_SAFE)

/* Define a structure holding all data items that are global within the
   object.c file. */

typedef struct AstTimeMapGlobals {
   AstTimeMapVtab Class_Vtab;
   int Class_Init;
} AstTimeMapGlobals;


/* Thread-safe initialiser for all global data used by this module. */
void astInitTimeMapGlobals_( AstTimeMapGlobals * );

#endif


#endif

/* Function prototypes. */
/* ==================== */
/* Prototypes for standard class functions. */
/* ---------------------------------------- */
astPROTO_CHECK(TimeMap)           /* Check class membership */
astPROTO_ISA(TimeMap)             /* Test class membership */

/* Constructor. */
#if defined(astCLASS)            /* Protected. */
AstTimeMap *astTimeMap_( int, const char *, int *, ...);
#else
AstTimeMap *astTimeMapId_( int, const char *, ... )__attribute__((format(printf,2,3)));
#endif

#if defined(astCLASS)            /* Protected */

/* Initialiser. */
AstTimeMap *astInitTimeMap_( void *, size_t, int, AstTimeMapVtab *,
                             const char *, int, int * );

/* Vtab initialiser. */
void astInitTimeMapVtab_( AstTimeMapVtab *, const char *, int * );

/* Loader. */
AstTimeMap *astLoadTimeMap_( void *, size_t, AstTimeMapVtab *,
                             const char *, AstChannel *, int * );
#endif

/* Prototypes for member functions. */
/* -------------------------------- */
void astTimeAdd_( AstTimeMap *, const char *, const double[], int * );

#if defined(astCLASS)            /* Protected. */
double astDat_( double, int, int * );
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
#define astCheckTimeMap(this) astINVOKE_CHECK(TimeMap,this,0)
#define astVerifyTimeMap(this) astINVOKE_CHECK(TimeMap,this,1)

/* Test class membership. */
#define astIsATimeMap(this) astINVOKE_ISA(TimeMap,this)

/* Constructor. */
#if defined(astCLASS)            /* Protected. */
#define astTimeMap astINVOKE(F,astTimeMap_)
#else
#define astTimeMap astINVOKE(F,astTimeMapId_)
#endif

#if defined(astCLASS)            /* Protected */

/* Initialiser. */
#define astInitTimeMap(mem,size,init,vtab,name,flags) \
astINVOKE(O,astInitTimeMap_(mem,size,init,vtab,name,flags,STATUS_PTR))

/* Vtab Initialiser. */
#define astInitTimeMapVtab(vtab,name) astINVOKE(V,astInitTimeMapVtab_(vtab,name,STATUS_PTR))
/* Loader. */
#define astLoadTimeMap(mem,size,vtab,name,channel) \
astINVOKE(O,astLoadTimeMap_(mem,size,vtab,name,astCheckChannel(channel),STATUS_PTR))
#endif

/* Interfaces to public member functions. */
/* -------------------------------------- */
/* Here we make use of astCheckTimeMap to validate TimeMap pointers
   before use.  This provides a contextual error report if a pointer
   to the wrong sort of Object is supplied. */
#define astTimeAdd(this,cvt,args) \
astINVOKE(V,astTimeAdd_(astCheckTimeMap(this),cvt,args,STATUS_PTR))

#if defined(astCLASS)            /* Protected */
#define astDat(in,forward) astDat_(in,forward,STATUS_PTR)
#endif
#endif





