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
*     <COPYRIGHT_STATEMENT>

*  Authors:
*     RFWS: R.F. Warren-Smith (Starlink)

*  History:
*     6-FEB-1996 (RFWS):
*        Original version.
*     25-SEP-1996 (RFWS):
*        Implemented external interface and I/O facilities.
*     13-DEC-1996 (RFWS):
*        Over-ride the astSimplify method.
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
   AstMapping *map1;             /* Pointer to first Mapping */
   AstMapping *map2;             /* Pointer to second Mapping */
   int invert1;                  /* Inversion flag for first Mapping */
   int invert2;                  /* Inversion flag for second Mapping */
   int series;                   /* Connect in series (else in parallel)? */
} AstCmpMap;

/* Virtual function table. */
/* ----------------------- */
/* This table contains all information that is the same for all
   objects in the class (e.g. pointers to its virtual functions). */
#if defined(astCLASS)            /* Protected */
typedef struct AstCmpMapVtab {

/* Properties (e.g. methods) inherited from the parent class. */
   AstMappingVtab mapping_vtab;  /* Parent class virtual function table */

/* Unique flag value to determine class membership. */
   int *check;                   /* Check value */

/* Properties (e.g. methods) specific to this class. */
/* None. */
} AstCmpMapVtab;
#endif

/* Function prototypes. */
/* ==================== */
/* Prototypes for standard class functions. */
/* ---------------------------------------- */
astPROTO_CHECK(CmpMap)           /* Check class membership */
astPROTO_ISA(CmpMap)             /* Test class membership */

/* Constructor. */
#if defined(astCLASS)            /* Protected. */
AstCmpMap *astCmpMap_( void *, void *, int, const char *, ... );
#else
AstCmpMap *astCmpMapId_( void *, void *, int, const char *, ... );
#endif

#if defined(astCLASS)            /* Protected */

/* Initialiser. */
AstCmpMap *astInitCmpMap_( void *, size_t, int, AstCmpMapVtab *,
                           const char *, AstMapping *, AstMapping *, int );

/* Loader. */
AstCmpMap *astLoadCmpMap_( void *, size_t, int, AstCmpMapVtab *,
                           const char *, AstChannel * );
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
#define astCheckCmpMap(this) astINVOKE_CHECK(CmpMap,this)

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
astINVOKE(O,astInitCmpMap_(mem,size,init,vtab,name,astCheckMapping(map1),astCheckMapping(map2),series))

/* Loader. */
#define astLoadCmpMap(mem,size,init,vtab,name,channel) \
astINVOKE(O,astLoadCmpMap_(mem,size,init,vtab,name,astCheckChannel(channel)))
#endif

/* Interfaces to public member functions. */
/* -------------------------------------- */
/* Here we make use of astCheckCmpMap to validate CmpMap pointers
   before use.  This provides a contextual error report if a pointer
   to the wrong sort of Object is supplied. */
/* None. */
#endif
