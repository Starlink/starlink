#if !defined( INTRAMAP_INCLUDED ) /* Include this file only once */
#define INTRAMAP_INCLUDED
/*
*+
*  Name:
*     intramap.h

*  Type:
*     C include file.

*  Purpose:
*     Define the interface to the IntraMap class.

*  Invocation:
*     #include "intramap.h"

*  Description:
*     This include file defines the interface to the IntraMap class
*     and provides the type definitions, function prototypes and
*     macros, etc. needed to use this class.
*
*     The IntraMap class implements Mappings which transform
*     coordinates using a privately-defined transformation function
*     (e.g. written in C).

*  Inheritance:
*     The IntraMap class inherits from the Mapping class.

*  Attributes Over-Ridden:
*     None.

*  New Attributes Defined:
*     None.

*  Methods Over-Ridden:
*     Public:
*        None.
*
*     Protected:
*        astMapMerge
*           Simplify a sequence of Mappings containing an IntraMap.
*        astTransform
*           Transform a set of points using an IntraMap.

*  New Methods Defined:
*     Public:
*        None.
*
*     Protected:
*        None.

*  Other Class Functions:
*     Public:
*        astIntraMap
*           Create an IntraMap.
*        astIntraReg
*           Register a transformation function for use by an IntraMap.
*        astIsAIntraMap
*           Test class membership.
*
*     Protected:
*        astCheckIntraMap
*           Validate class membership.
*        astInitIntraMap
*           Initialise an IntraMap.
*        astLoadIntraMap
*           Load an IntraMap.

*  Macros:
*     None.

*  Type Definitions:
*     Public:
*        AstIntraMap
*           IntraMap object type.
*
*     Protected:
*        AstIntraMapVtab
*           IntraMap virtual function table type.

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
*     24-MAR-1998 (RFWS):
*        Original version.
*-
*/

/* Include files. */
/* ============== */
/* Interface definitions. */
/* ---------------------- */
#include "mapping.h"             /* Coordinate mappings (parent class) */

#if defined(astCLASS)            /* Protected */
#include "channel.h"             /* I/O channels */
#endif

/* C header files. */
/* --------------- */
#if defined(astCLASS)            /* Protected */
#include <stddef.h>
#endif

/* Macro Definitions. */
/* ================== */
#define AST__NOFWD (1U)          /* No forward transformation defined */
#define AST__NOINV (2U)          /* No inverse transformation defined */
#define AST__SIMPFI (4U)         /* Forward-inverse may be simplified */
#define AST__SIMPIF (8U)         /* Inverse-forward may be simplified */

#define AST__ANY (-66)           /* Allow any number of input/output coords */

/* Type Definitions. */
/* ================= */
/* IntraMap structure. */
/* ------------------ */
/* This structure contains all information that is unique to each object in
   the class (e.g. its instance variables). */
typedef struct AstIntraMap {

/* Attributes inherited from the parent class. */
   AstMapping mapping;           /* Parent class structure */

/* Attributes specific to objects in this class. */
   int ifun;                     /* Transformation function index */
} AstIntraMap;

/* Virtual function table. */
/* ----------------------- */
/* This table contains all information that is the same for all
   objects in the class (e.g. pointers to its virtual functions). */
#if defined(astCLASS)            /* Protected */
typedef struct AstIntraMapVtab {

/* Properties (e.g. methods) inherited from the parent class. */
   AstMappingVtab mapping_vtab;  /* Parent class virtual function table */

/* Unique flag value to determine class membership. */
   int *check;                   /* Check value */

/* Properties (e.g. methods) specific to this class. */
} AstIntraMapVtab;
#endif

/* Function prototypes. */
/* ==================== */
/* Prototypes for standard class functions. */
/* ---------------------------------------- */
astPROTO_CHECK(IntraMap)          /* Check class membership */
astPROTO_ISA(IntraMap)            /* Test class membership */

/* Constructor. */
#if defined(astCLASS)            /* Protected. */
AstIntraMap *astIntraMap_( const char *, int, int, const char *, ... );
#else
AstIntraMap *astIntraMapId_( const char *, int, int, const char *, ... );
#endif

#if defined(astCLASS)            /* Protected */

/* Initialiser. */
AstIntraMap *astInitIntraMap_( void *, size_t, int, AstIntraMapVtab *,
                               const char *, const char *, int, int );

/* Loader. */
AstIntraMap *astLoadIntraMap_( void *, size_t, int, AstIntraMapVtab *,
                               const char *, AstChannel * );
#endif

/* Prototypes for member functions. */
/* -------------------------------- */
void astIntraReg_( const char *, int, int, void (*)( int, int, const double *[], int, int, double *[] ), unsigned int, const char *, const char *, const char * );

# if defined(astCLASS)           /* Protected */
#else                            /* Public only */
void astIntraRegFor_( const char *, int, int, void (*)( int, int, const double *[], int, int, double *[] ), void (*)( void (*)( int, int, const double *[], int, int, double *[] ), int, int, const double *[], int, int, double *[] ), unsigned int, const char *, const char *, const char * );
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
#define astCheckIntraMap(this) astINVOKE_CHECK(IntraMap,this)

/* Test class membership. */
#define astIsAIntraMap(this) astINVOKE_ISA(IntraMap,this)

/* Constructor. */
#if defined(astCLASS)            /* Protected. */
#define astIntraMap astINVOKE(F,astIntraMap_)
#else
#define astIntraMap astINVOKE(F,astIntraMapId_)
#endif

#if defined(astCLASS)            /* Protected */

/* Initialiser. */
#define astInitIntraMap(mem,size,init,vtab,name,fname,nin,nout) \
astINVOKE(O,astInitIntraMap_(mem,size,init,vtab,name,fname,nin,nout))

/* Loader. */
#define astLoadIntraMap(mem,size,init,vtab,name,channel) \
astINVOKE(O,astLoadIntraMap_(mem,size,init,vtab,name,astCheckChannel(channel)))
#endif

/* Interfaces to public member functions. */
/* -------------------------------------- */
/* Here we make use of astCheckIntraMap to validate IntraMap pointers
   before use.  This provides a contextual error report if a pointer
   to the wrong sort of Object is supplied. */

#define astIntraReg(name,nin,nout,tran,flags,purpose,author,contact) \
astIntraReg_(name,nin,nout,tran,flags,purpose,author,contact)

#if defined(astCLASS)            /* Protected */
#else                            /* Public only */
#define astIntraRegFor(name,nin,nout,tran,tran_wrap,flags,purpose,author,contact) \
astIntraRegFor_(name,nin,nout,tran,tran_wrap,flags,purpose,author,contact)
#endif
#endif
