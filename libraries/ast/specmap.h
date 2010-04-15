#if !defined( SPECMAP_INCLUDED )  /* Include this file only once */
#define SPECMAP_INCLUDED
/*
*+
*  Name:
*     specmap.h

*  Type:
*     C include file.

*  Purpose:
*     Define the interface to the SpecMap class.

*  Invocation:
*     #include "specmap.h"

*  Description:
*     This include file defines the interface to the SpecMap class and
*     provides the type definitions, function prototypes and macros,
*     etc.  needed to use this class.
*
*     The SpecMap class encapsulates various ecptral coordinate
*     conversions. Since, typically, a sequence of these conversions is
*     required, a SpecMap can be used to accumulate a series of conversions
*     which it then applies in sequence.

*  Inheritance:
*     The SpecMap class inherits from the Mapping class.

*  Attributes Over-Ridden:
*     None.

*  New Attributes Defined:
*     None.

*  Methods Over-Ridden:
*     Public:
*        astTransform
*           Use an SpecMap to transform a set of points.

*     Protected:
*        astMapMerge
*           Simplify a sequence of Mappings containing an SpecMap.

*  New Methods Defined:
*     Public:
*        astSpecAdd
*           Add a coordinate conversion step to an SpecMap.

*     Private:
*        None.

*  Other Class Functions:
*     Public:
*        astIsASpecMap
*           Test class membership.
*        astSpecMap
*           Create an SpecMap.

*     Protected:
*        astCheckSpecMap
*           Validate class membership.
*        astInitSpecMap
*           Initialise an SpecMap.
*        astLoadSpecMap
*           Load an SpecMap.

*  Macros:
*     None.

*  Type Definitions:
*     Public:
*        AstSpecMap
*           SpecMap object type.

*     Protected:
*        AstSpecMapVtab
*           SpecMap virtual function table type.

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
*     8-NOV-2002 (DSB):
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
/* ------ */
/* Physical constants taken from Chapter 15 of the "Explanatory Supplement
   to the Astronomical Ephemeris". */
#define AST__C 2.99792458E8      /* Speed of light (metres per second) */
#define AST__H 6.6260755E-34     /* Plank constant (Joule.seconds) */

/* Define a dummy __attribute__ macro for use on non-GNU compilers. */
#ifndef __GNUC__
#  define  __attribute__(x)  /*NOTHING*/
#endif

/* SpecMap structure. */
/* ----------------- */
/* This structure contains all information that is unique to each
   object in the class (e.g. its instance variables). */
typedef struct AstSpecMap {

/* Attributes inherited from the parent class. */
   AstMapping mapping;           /* Parent class structure */

/* Attributes specific to objects in this class. */
   int *cvttype;                 /* Pointer to array of conversion types */
   double **cvtargs;             /* Pointer to argument list pointer array */
   int ncvt;                     /* Number of conversions to perform */
} AstSpecMap;

/* Virtual function table. */
/* ----------------------- */
/* This table contains all information that is the same for all
   objects in the class (e.g. pointers to its virtual functions). */
#if defined(astCLASS)            /* Protected */
typedef struct AstSpecMapVtab {

/* Properties (e.g. methods) inherited from the parent class. */
   AstMappingVtab mapping_vtab;  /* Parent class virtual function table */

/* A Unique identifier to determine class membership. */
   AstClassIdentifier id;

/* Properties (e.g. methods) specific to this class. */
   void (* SpecAdd)( AstSpecMap *, const char *, const double[], int * );
} AstSpecMapVtab;

#if defined(THREAD_SAFE)

/* Define a structure holding all data items that are global within the
   object.c file. */

typedef struct AstSpecMapGlobals {
   AstSpecMapVtab Class_Vtab;
   int Class_Init;
} AstSpecMapGlobals;


/* Thread-safe initialiser for all global data used by this module. */
void astInitSpecMapGlobals_( AstSpecMapGlobals * );

#endif


#endif

/* Function prototypes. */
/* ==================== */
/* Prototypes for standard class functions. */
/* ---------------------------------------- */
astPROTO_CHECK(SpecMap)           /* Check class membership */
astPROTO_ISA(SpecMap)             /* Test class membership */

/* Constructor. */
#if defined(astCLASS)            /* Protected. */
AstSpecMap *astSpecMap_( int, int, const char *, int *, ...);
#else
AstSpecMap *astSpecMapId_( int, int, const char *, ... )__attribute__((format(printf,3,4)));
#endif

#if defined(astCLASS)            /* Protected */

/* Initialiser. */
AstSpecMap *astInitSpecMap_( void *, size_t, int, AstSpecMapVtab *,
                             const char *, int, int, int * );

/* Vtab initialiser. */
void astInitSpecMapVtab_( AstSpecMapVtab *, const char *, int * );

/* Loader. */
AstSpecMap *astLoadSpecMap_( void *, size_t, AstSpecMapVtab *,
                           const char *, AstChannel *, int * );
#endif

/* Prototypes for member functions. */
/* -------------------------------- */
void astSpecAdd_( AstSpecMap *, const char *, const double[], int * );

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
#define astCheckSpecMap(this) astINVOKE_CHECK(SpecMap,this,0)
#define astVerifySpecMap(this) astINVOKE_CHECK(SpecMap,this,1)

/* Test class membership. */
#define astIsASpecMap(this) astINVOKE_ISA(SpecMap,this)

/* Constructor. */
#if defined(astCLASS)            /* Protected. */
#define astSpecMap astINVOKE(F,astSpecMap_)
#else
#define astSpecMap astINVOKE(F,astSpecMapId_)
#endif

#if defined(astCLASS)            /* Protected */

/* Initialiser. */
#define astInitSpecMap(mem,size,init,vtab,name,nin,flags) \
astINVOKE(O,astInitSpecMap_(mem,size,init,vtab,name,nin,flags,STATUS_PTR))

/* Vtab Initialiser. */
#define astInitSpecMapVtab(vtab,name) astINVOKE(V,astInitSpecMapVtab_(vtab,name,STATUS_PTR))
/* Loader. */
#define astLoadSpecMap(mem,size,vtab,name,channel) \
astINVOKE(O,astLoadSpecMap_(mem,size,vtab,name,astCheckChannel(channel),STATUS_PTR))
#endif

/* Interfaces to public member functions. */
/* -------------------------------------- */
/* Here we make use of astCheckSpecMap to validate SpecMap pointers
   before use.  This provides a contextual error report if a pointer
   to the wrong sort of Object is supplied. */
#define astSpecAdd(this,cvt,args) \
astINVOKE(V,astSpecAdd_(astCheckSpecMap(this),cvt,args,STATUS_PTR))

#endif





