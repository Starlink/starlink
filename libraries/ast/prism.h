#if !defined( PRISM_INCLUDED ) /* Include this file only once */
#define PRISM_INCLUDED
/*
*+
*  Name:
*     prism.h

*  Type:
*     C include file.

*  Purpose:
*     Define the interface to the Prism class.

*  Invocation:
*     #include "prism.h"

*  Description:
*     This include file defines the interface to the Prism class and
*     provides the type definitions, function prototypes and macros,
*     etc.  needed to use this class.
*
*     The Prism class implement a Region which represents an extrusion of
*     another Region into higher dimensions. For instance, a Prism can be
*     used to represent a cylinder, which is an extrusion of a circle into a
*     3rd dimension.

*  Inheritance:
*     The Prism class inherits from the Region class.

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
*     17-DEC-2004 (DSB):
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
#include "interval.h"            /* Axis intervals */
#endif

/* C header files. */
/* --------------- */
#if defined(astCLASS)            /* Protected */
#include <stddef.h>
#endif

/* Macros. */
/* ------- */

/* Type Definitions. */
/* ================= */
/* Prism structure. */
/* ------------------ */
/* This structure contains all information that is unique to each object in
   the class (e.g. its instance variables). */
typedef struct AstPrism {

/* Attributes inherited from the parent class. */
   AstRegion region;             /* Parent class structure */

/* Attributes specific to objects in this class. */
   AstRegion *region1;           /* First component Region */
   AstRegion *region2;           /* Second component Region */

} AstPrism;

/* Virtual function table. */
/* ----------------------- */
/* This table contains all information that is the same for all
   objects in the class (e.g. pointers to its virtual functions). */
#if defined(astCLASS)            /* Protected */
typedef struct AstPrismVtab {

/* Properties (e.g. methods) inherited from the parent class. */
   AstRegionVtab region_vtab;    /* Parent class virtual function table */

/* Unique flag value to determine class membership. */
   int *check;                   /* Check value */

/* Properties (e.g. methods) specific to this class. */
} AstPrismVtab;
#endif

/* Function prototypes. */
/* ==================== */
/* Prototypes for standard class functions. */
/* ---------------------------------------- */
astPROTO_CHECK(Prism)          /* Check class membership */
astPROTO_ISA(Prism)            /* Test class membership */

/* Constructor. */
#if defined(astCLASS)            /* Protected. */
AstPrism *astPrism_( void *, void *, const char *, ... );
#else
AstPrism *astPrismId_( void *, void *, const char *, ... );
#endif

#if defined(astCLASS)            /* Protected */

/* Initialiser. */
AstPrism *astInitPrism_( void *, size_t, int, AstPrismVtab *,
                     const char *, AstRegion *, AstInterval * );

/* Vtab initialiser. */
void astInitPrismVtab_( AstPrismVtab *, const char * );

/* Loader. */
AstPrism *astLoadPrism_( void *, size_t, AstPrismVtab *,
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
#define astCheckPrism(this) astINVOKE_CHECK(Prism,this)

/* Test class membership. */
#define astIsAPrism(this) astINVOKE_ISA(Prism,this)

/* Constructor. */
#if defined(astCLASS)            /* Protected. */
#define astPrism astINVOKE(F,astPrism_)
#else
#define astPrism astINVOKE(F,astPrismId_)
#endif

#if defined(astCLASS)            /* Protected */

/* Initialiser. */
#define astInitPrism(mem,size,init,vtab,name,reg1,reg2) \
astINVOKE(O,astInitPrism_(mem,size,init,vtab,name,reg1,reg2))

/* Vtab Initialiser. */
#define astInitPrismVtab(vtab,name) astINVOKE(V,astInitPrismVtab_(vtab,name))
/* Loader. */
#define astLoadPrism(mem,size,vtab,name,channel) \
astINVOKE(O,astLoadPrism_(mem,size,vtab,name,astCheckChannel(channel)))
#endif

/* Interfaces to public member functions. */
/* -------------------------------------- */
/* Here we make use of astCheckPrism to validate Prism pointers
   before use.  This provides a contextual error report if a pointer
   to the wrong sort of Object is supplied. */

#if defined(astCLASS)            /* Protected */
#endif
#endif
