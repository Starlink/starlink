#if !defined( BOX_INCLUDED ) /* Include this file only once */
#define BOX_INCLUDED
/*
*+
*  Name:
*     box.h

*  Type:
*     C include file.

*  Purpose:
*     Define the interface to the Box class.

*  Invocation:
*     #include "box.h"

*  Description:
*     This include file defines the interface to the Box class and
*     provides the type definitions, function prototypes and macros,
*     etc.  needed to use this class.
*
*     The Box class implement a Region which represents a simple interval
*     on each axis of the encapsulated Frame

*  Inheritance:
*     The Box class inherits from the Region class.

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
*     22-MAR-2003 (DSB):
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
/* Box structure. */
/* ------------------ */
/* This structure contains all information that is unique to each object in
   the class (e.g. its instance variables). */
typedef struct AstBox {

/* Attributes inherited from the parent class. */
   AstRegion region;             /* Parent class structure */

/* Attributes specific to objects in this class. */
   double *extent;               /* Axis half-widths */
   double *centre;               /* Box centre coords */

} AstBox;

/* Virtual function table. */
/* ----------------------- */
/* This table contains all information that is the same for all
   objects in the class (e.g. pointers to its virtual functions). */
#if defined(astCLASS)            /* Protected */
typedef struct AstBoxVtab {

/* Properties (e.g. methods) inherited from the parent class. */
   AstRegionVtab region_vtab;    /* Parent class virtual function table */

/* Unique flag value to determine class membership. */
   int *check;                   /* Check value */

/* Properties (e.g. methods) specific to this class. */
} AstBoxVtab;
#endif

/* Function prototypes. */
/* ==================== */
/* Prototypes for standard class functions. */
/* ---------------------------------------- */
astPROTO_CHECK(Box)          /* Check class membership */
astPROTO_ISA(Box)            /* Test class membership */

/* Constructor. */
#if defined(astCLASS)            /* Protected. */
AstBox *astBox_( void *, int, const double[], const double[], AstRegion *, const char *, ... );
#else
AstBox *astBoxId_( void *, int, const double[], const double[], AstRegion *, const char *, ... );
#endif

#if defined(astCLASS)            /* Protected */

/* Initialiser. */
AstBox *astInitBox_( void *, size_t, int, AstBoxVtab *,
                     const char *, AstFrame *, int, const double[],
                     const double[], AstRegion * );

/* Vtab initialiser. */
void astInitBoxVtab_( AstBoxVtab *, const char * );

/* Loader. */
AstBox *astLoadBox_( void *, size_t, AstBoxVtab *,
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
#define astCheckBox(this) astINVOKE_CHECK(Box,this)

/* Test class membership. */
#define astIsABox(this) astINVOKE_ISA(Box,this)

/* Constructor. */
#if defined(astCLASS)            /* Protected. */
#define astBox astINVOKE(F,astBox_)
#else
#define astBox astINVOKE(F,astBoxId_)
#endif

#if defined(astCLASS)            /* Protected */

/* Initialiser. */
#define astInitBox(mem,size,init,vtab,name,frame,form,p1,p2,unc) \
astINVOKE(O,astInitBox_(mem,size,init,vtab,name,frame,form,p1,p2,unc))

/* Vtab Initialiser. */
#define astInitBoxVtab(vtab,name) astINVOKE(V,astInitBoxVtab_(vtab,name))
/* Loader. */
#define astLoadBox(mem,size,vtab,name,channel) \
astINVOKE(O,astLoadBox_(mem,size,vtab,name,astCheckChannel(channel)))
#endif

/* Interfaces to public member functions. */
/* -------------------------------------- */
/* Here we make use of astCheckBox to validate Box pointers
   before use.  This provides a contextual error report if a pointer
   to the wrong sort of Object is supplied. */

#if defined(astCLASS)            /* Protected */
#endif
#endif
