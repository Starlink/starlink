#if !defined( PERMMAP_INCLUDED ) /* Include this file only once */
#define PERMMAP_INCLUDED
/*
*+
*  Name:
*     permmap.h

*  Type:
*     C include file.

*  Purpose:
*     Define the interface to the PermMap class.

*  Invocation:
*     #include "permmap.h"

*  Description:
*     This include file defines the interface to the PermMap class and
*     provides the type definitions, function prototypes and macros,
*     etc.  needed to use this class.
*
*     The PermMap class implements Mappings that perform permutation
*     of the order of coordinate values, possibly also accompanied by
*     changes in the number of coordinates (between input and output).
*
*     In addition to permuting the coordinate order, coordinates may
*     also be assigned constant values which are unrelated to other
*     coordinate values.  This facility is useful when the number of
*     coordinates is being increased, as it allows fixed values to be
*     assigned to the new coordinates.

*  Inheritance:
*     The PermMap class inherits from the Mapping class.

*  Attributes Over-Ridden:
*     None.

*  New Attributes Defined:
*     None.

*  Methods Over-Ridden:
*     Public:
*        None.
*
*     Protected:
*        astTransform
*           Transform a set of points.

*  New Methods Defined:
*     None.

*  Other Class Functions:
*     Public:
*        astIsAPermMap
*           Test class membership.
*        astPermMap
*           Create a PermMap.
*
*     Protected:
*        astCheckPermMap
*           Validate class membership.
*        astInitPermMap
*           Initialise a PermMap.
*        astLoadPermMap
*           Load a PermMap.

*  Macros:
*     None.

*  Type Definitions:
*     Public:
*        AstPermMap
*           PermMap object type.
*
*     Protected:
*        AstPermMapVtab
*           PermMap virtual function table type.

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
*     29-FEB-1996 (RFWS):
*        Original version.
*     26-SEP-1996 (RFWS):
*        Added external interface and I/O facilities.
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

/* Type Definitions. */
/* ================= */
/* PermMap structure. */
/* ------------------ */
/* This structure contains all information that is unique to each
   object in the class (e.g. its instance variables). */
typedef struct AstPermMap {

/* Attributes inherited from the parent class. */
   AstMapping mapping;           /* Parent class structure */

/* Attributes specific to objects in this class. */
   int *inperm;                  /* Pointer to input permutation array */
   int *outperm;                 /* Pointer to output permutation array */
   double *constant;             /* Pointer to array of constant values */
} AstPermMap;

/* Virtual function table. */
/* ----------------------- */
/* This table contains all information that is the same for all
   objects in the class (e.g. pointers to its virtual functions). */
#if defined(astCLASS)            /* Protected */
typedef struct AstPermMapVtab {

/* Properties (e.g. methods) inherited from the parent class. */
   AstMappingVtab mapping_vtab;  /* Parent class virtual function table */

/* Unique flag value to determine class membership. */
   int *check;                   /* Check value */

/* Properties (e.g. methods) specific to this class. */

/* None. */

} AstPermMapVtab;
#endif

/* Function prototypes. */
/* ==================== */
/* Prototypes for standard class functions. */
/* ---------------------------------------- */
astPROTO_CHECK(PermMap)          /* Check class membership */
astPROTO_ISA(PermMap)            /* Test class membership */

/* Constructor. */
#if defined(astCLASS)            /* Protected. */
AstPermMap *astPermMap_( int, const int [], int, const int [],
                         const double [], const char *, ... );
#else
AstPermMap *astPermMapId_( int, const int [], int, const int [],
                           const double [], const char *, ... );
#endif

#if defined(astCLASS)            /* Protected */

/* Initialiser. */
AstPermMap *astInitPermMap_( void *, size_t, int, AstPermMapVtab *,
                             const char *, int, const int [], int,
                             const int [], const double [] );

/* Loader. */
AstPermMap *astLoadPermMap_( void *, size_t, int, AstPermMapVtab *,
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
#define astCheckPermMap(this) astINVOKE_CHECK(PermMap,this)

/* Test class membership. */
#define astIsAPermMap(this) astINVOKE_ISA(PermMap,this)

/* Constructor. */
#if defined(astCLASS)            /* Protected. */
#define astPermMap astINVOKE(F,astPermMap_)
#else
#define astPermMap astINVOKE(F,astPermMapId_)
#endif

#if defined(astCLASS)            /* Protected */

/* Initialiser. */
#define astInitPermMap(mem,size,init,vtab,name,nin,inperm,nout,outperm,constant) \
astINVOKE(O,astInitPermMap_(mem,size,init,vtab,name,nin,inperm,nout,outperm,constant))

/* Loader. */
#define astLoadPermMap(mem,size,init,vtab,name,channel) \
astINVOKE(O,astLoadPermMap_(mem,size,init,vtab,name,astCheckChannel(channel)))
#endif

/* Interfaces to public member functions. */
/* -------------------------------------- */
/* Here we make use of astCheckPermMap to validate PermMap pointers
   before use.  This provides a contextual error report if a pointer
   to the wrong sort of Object is supplied. */

#endif
