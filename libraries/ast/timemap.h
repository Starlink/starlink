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
*     <COPYRIGHT_STATEMENT>

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

/* Unique flag value to determine class membership. */
   int *check;                   /* Check value */

/* Properties (e.g. methods) specific to this class. */
   void (* TimeAdd)( AstTimeMap *, const char *, const double[] );
} AstTimeMapVtab;
#endif

/* Function prototypes. */
/* ==================== */
/* Prototypes for standard class functions. */
/* ---------------------------------------- */
astPROTO_CHECK(TimeMap)           /* Check class membership */
astPROTO_ISA(TimeMap)             /* Test class membership */

/* Constructor. */
#if defined(astCLASS)            /* Protected. */
AstTimeMap *astTimeMap_( int, const char *, ... );
#else
AstTimeMap *astTimeMapId_( int, const char *, ... );
#endif

#if defined(astCLASS)            /* Protected */

/* Initialiser. */
AstTimeMap *astInitTimeMap_( void *, size_t, int, AstTimeMapVtab *,
                             const char *, int );

/* Vtab initialiser. */
void astInitTimeMapVtab_( AstTimeMapVtab *, const char * );

/* Loader. */
AstTimeMap *astLoadTimeMap_( void *, size_t, AstTimeMapVtab *,
                             const char *, AstChannel * );
#endif

/* Prototypes for member functions. */
/* -------------------------------- */
void astTimeAdd_( AstTimeMap *, const char *, const double[] );

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
#define astCheckTimeMap(this) astINVOKE_CHECK(TimeMap,this)

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
astINVOKE(O,astInitTimeMap_(mem,size,init,vtab,name,flags))

/* Vtab Initialiser. */
#define astInitTimeMapVtab(vtab,name) astINVOKE(V,astInitTimeMapVtab_(vtab,name))
/* Loader. */
#define astLoadTimeMap(mem,size,vtab,name,channel) \
astINVOKE(O,astLoadTimeMap_(mem,size,vtab,name,astCheckChannel(channel)))
#endif

/* Interfaces to public member functions. */
/* -------------------------------------- */
/* Here we make use of astCheckTimeMap to validate TimeMap pointers
   before use.  This provides a contextual error report if a pointer
   to the wrong sort of Object is supplied. */
#define astTimeAdd(this,cvt,args) \
astINVOKE(V,astTimeAdd_(astCheckTimeMap(this),cvt,args))

#endif
