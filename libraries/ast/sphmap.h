#if !defined( SPHMAP_INCLUDED ) /* Include this file only once */
#define SPHMAP_INCLUDED
/*
*+
*  Name:
*     sphmap.h

*  Type:
*     C include file.

*  Purpose:
*     Define the interface to the SphMap class.

*  Invocation:
*     #include "sphmap.h"

*  Description:
*     This include file defines the interface to the SphMap class and
*     provides the type definitions, function prototypes and macros,
*     etc.  needed to use this class.
*
*     The SphMap class implements Mappings which maps positions from 
*     3-dimensional Cartesian coordinates into 2-dimensional spherical 
*     coordinates (i.e. longitude and latitude on a unit sphere). The 
*     inverse Mapping always produces vectors of unit length.
*
*     The spherical coordinates are longitude (positive anti-clockwise 
*     looking from the positive latitude pole) and latitude. The
*     Cartesian coordinates are right-handed, with the x-axis (axis 1) 
*     at zero longitude and latitude, and the z-axis (axis 3) at the 
*     positive latitude pole.
*
*     At either pole, the longitude is set arbitrarly to zero. If the 
*     Cartesian coordinates are all zero, then the longitude and latitude
*     values are set to AST__BAD.

*  Inheritance:
*     The SphMap class inherits from the Mapping class.

*  Attributes Over-Ridden:
*     None.

*  New Attributes Defined:
*     None.

*  Methods Over-Ridden:
*     Public:
*        None.
*
*     Protected:
*        astClearAttrib
*           Clear an attribute value for a SphMap.
*        astGetAttrib
*           Get an attribute value for a SphMap.
*        astMapMerge
*           Simplify a sequence of Mappings containing a SphMap.
*        astSetAttrib
*           Set an attribute value for a SphMap.
*        astTestAttrib
*           Test if an attribute value has been set for a SphMap.
*        astTransform
*           Apply a SphMap to transform a set of points.

*  New Methods Defined:
*     Public:
*        None.
*
*     Protected:
*        None.

*  Other Class Functions:
*     Public:
*        astIsASphMap
*           Test class membership.
*        astSphMap
*           Create a SphMap.
*
*     Protected:
*        astCheckSphMap
*           Validate class membership.
*        astInitSphMap
*           Initialise a SphMap.
*        astLoadSphMap
*           Load a SphMap.

*  Macros:
*     None.

*  Type Definitions:
*     Public:
*        AstSphMap
*           SphMap object type.
*
*     Protected:
*        AstSphMapVtab
*           SphMap virtual function table type.

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
*     DSB: D.S. Berry (Starlink)

*  History:
*     25-OCT-1996 (DSB):
*        Original version.
*     24-MAR-1998 (RFWS):
*        Override the astMapMerge method.
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
/* SphMap structure. */
/* ------------------ */
/* This structure contains all information that is unique to each object in
   the class (e.g. its instance variables). */
typedef struct AstSphMap {

/* Attributes inherited from the parent class. */
   AstMapping mapping;           /* Parent class structure */

} AstSphMap;

/* Virtual function table. */
/* ----------------------- */
/* This table contains all information that is the same for all
   objects in the class (e.g. pointers to its virtual functions). */
#if defined(astCLASS)            /* Protected */
typedef struct AstSphMapVtab {

/* Properties (e.g. methods) inherited from the parent class. */
   AstMappingVtab mapping_vtab;  /* Parent class virtual function table */

/* Unique flag value to determine class membership. */
   int *check;                   /* Check value */

/* Properties (e.g. methods) specific to this class. */
} AstSphMapVtab;
#endif

/* Function prototypes. */
/* ==================== */
/* Prototypes for standard class functions. */
/* ---------------------------------------- */
astPROTO_CHECK(SphMap)          /* Check class membership */
astPROTO_ISA(SphMap)            /* Test class membership */

/* Constructor. */
#if defined(astCLASS)            /* Protected. */
AstSphMap *astSphMap_( const char *, ... );
#else
AstSphMap *astSphMapId_( const char *, ... );
#endif

#if defined(astCLASS)            /* Protected */

/* Initialiser. */
AstSphMap *astInitSphMap_( void *, size_t, int, AstSphMapVtab *,
                           const char * );

/* Loader. */
AstSphMap *astLoadSphMap_( void *, size_t, int, AstSphMapVtab *,
                           const char *, AstChannel * );
#endif

/* Prototypes for member functions. */
/* -------------------------------- */
# if defined(astCLASS)           /* Protected */
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
#define astCheckSphMap(this) astINVOKE_CHECK(SphMap,this)

/* Test class membership. */
#define astIsASphMap(this) astINVOKE_ISA(SphMap,this)

/* Constructor. */
#if defined(astCLASS)            /* Protected. */
#define astSphMap astINVOKE(F,astSphMap_)
#else
#define astSphMap astINVOKE(F,astSphMapId_)
#endif

#if defined(astCLASS)            /* Protected */

/* Initialiser. */
#define \
astInitSphMap(mem,size,init,vtab,name) \
astINVOKE(O,astInitSphMap_(mem,size,init,vtab,name))

/* Loader. */
#define astLoadSphMap(mem,size,init,vtab,name,channel) \
astINVOKE(O,astLoadSphMap_(mem,size,init,vtab,name,astCheckChannel(channel)))
#endif

/* Interfaces to public member functions. */
/* -------------------------------------- */
/* Here we make use of astCheckSphMap to validate SphMap pointers
   before use.  This provides a contextual error report if a pointer
   to the wrong sort of Object is supplied. */

#if defined(astCLASS)            /* Protected */
#endif

#endif
