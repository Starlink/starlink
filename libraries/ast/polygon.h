#if !defined( POLYGON_INCLUDED ) /* Include this file only once */
#define POLYGON_INCLUDED
/*
*+
*  Name:
*     polygon.h

*  Type:
*     C include file.

*  Purpose:
*     Define the interface to the Polygon class.

*  Invocation:
*     #include "polygon.h"

*  Description:
*     This include file defines the interface to the Polygon class and
*     provides the type definitions, function prototypes and macros,
*     etc.  needed to use this class.
*
*     The Polygon class implements a Region which represents a collection
*     of points in a Frame.

*  Inheritance:
*     The Polygon class inherits from the Region class.

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
*     26-OCT-2004 (DSB):
*        Original version.
*-
*/

/* Include files. */
/* ============== */
/* Interface definitions. */
/* ---------------------- */
#include "region.h"              /* Coordinate regions (parent class) */

#if defined(astCLASS)            /* Protected */
#include "channel.h"             /* I/O channels */
#endif

/* C header files. */
/* --------------- */
#if defined(astCLASS)            /* Protected */
#include <stddef.h>
#endif

/* Type Definitions. */
/* ================= */
/* Polygon structure. */
/* ------------------ */
/* This structure contains all information that is unique to each object in
   the class (e.g. its instance variables). */
typedef struct AstPolygon {

/* Attributes inherited from the parent class. */
   AstRegion region;          /* Parent class structure */

/* Attributes specific to objects in this class. */
   double lbnd[2];            /* Lower axis limits of bounding box */
   double ubnd[2];            /* Upper axis limits of bounding box */

} AstPolygon;

/* Virtual function table. */
/* ----------------------- */
/* This table contains all information that is the same for all
   objects in the class (e.g. pointers to its virtual functions). */
#if defined(astCLASS)            /* Protected */
typedef struct AstPolygonVtab {

/* Properties (e.g. methods) inherited from the parent class. */
   AstRegionVtab region_vtab;    /* Parent class virtual function table */

/* Unique flag value to determine class membership. */
   int *check;                   /* Check value */

/* Properties (e.g. methods) specific to this class. */
} AstPolygonVtab;
#endif

/* Function prototypes. */
/* ==================== */
/* Prototypes for standard class functions. */
/* ---------------------------------------- */
astPROTO_CHECK(Polygon)          /* Check class membership */
astPROTO_ISA(Polygon)            /* Test class membership */

/* Constructor. */
#if defined(astCLASS)            /* Protected. */
AstPolygon *astPolygon_( void *, int, int, const double *, AstRegion *, const char *, ... );
#else
AstPolygon *astPolygonId_( void *, int, int, const double *, AstRegion *, const char *, ... );
#endif

#if defined(astCLASS)            /* Protected */

/* Initialiser. */
AstPolygon *astInitPolygon_( void *, size_t, int, AstPolygonVtab *, const char *, AstFrame *, int, int, const double *, AstRegion * );

/* Vtab initialiser. */
void astInitPolygonVtab_( AstPolygonVtab *, const char * );

/* Loader. */
AstPolygon *astLoadPolygon_( void *, size_t, AstPolygonVtab *,
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
#define astCheckPolygon(this) astINVOKE_CHECK(Polygon,this)

/* Test class membership. */
#define astIsAPolygon(this) astINVOKE_ISA(Polygon,this)

/* Constructor. */
#if defined(astCLASS)            /* Protected. */
#define astPolygon astINVOKE(F,astPolygon_)
#else
#define astPolygon astINVOKE(F,astPolygonId_)
#endif

#if defined(astCLASS)            /* Protected */

/* Initialiser. */
#define astInitPolygon(mem,size,init,vtab,name,frame,npnt,indim,points,unc) \
astINVOKE(O,astInitPolygon_(mem,size,init,vtab,name,frame,npnt,indim,points,unc))

/* Vtab Initialiser. */
#define astInitPolygonVtab(vtab,name) astINVOKE(V,astInitPolygonVtab_(vtab,name))

/* Loader. */
#define astLoadPolygon(mem,size,vtab,name,channel) \
astINVOKE(O,astLoadPolygon_(mem,size,vtab,name,astCheckChannel(channel)))
#endif

/* Interfaces to public member functions. */
/* -------------------------------------- */
/* Here we make use of astCheckPolygon to validate Polygon pointers
   before use.  This provides a contextual error report if a pointer
   to the wrong sort of Object is supplied. */

#if defined(astCLASS)            /* Protected */
#endif
#endif
