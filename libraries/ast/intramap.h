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
*     IntraFlag
*        IntraMap identification string.

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
*        astClearIntraFlag
*           Clear the IntraFlag attribute for an IntraMap.
*        astGetIntraFlag
*           Get the value of the IntraFlag attribute for an IntraMap.
*        astSetIntraFlag
*           Set the value of the IntraFlag attribute for an IntraMap.
*        astTestIntraFlag
*           Test whether a value has been set for the IntraFlag attribute of
*           an IntraMap.

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
*     Copyright (C) 1997-2006 Council for the Central Laboratory of the
*     Research Councils

*  Licence:
*     This program is free software: you can redistribute it and/or
*     modify it under the terms of the GNU Lesser General Public
*     License as published by the Free Software Foundation, either
*     version 3 of the License, or (at your option) any later
*     version.
*     
*     This program is distributed in the hope that it will be useful,
*     but WITHOUT ANY WARRANTY; without even the implied warranty of
*     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
*     GNU Lesser General Public License for more details.
*     
*     You should have received a copy of the GNU Lesser General
*     License along with this program.  If not, see
*     <http://www.gnu.org/licenses/>.

*  Authors:
*     RFWS: R.F. Warren-Smith (Starlink)

*  History:
*     24-MAR-1998 (RFWS):
*        Original version.
*     16-SEP-1999 (RFWS):
*        Added the IntraFlag attribute and added a Mapping pointer as a new
*        first argument to transformation functions.
*     8-JAN-2003 (DSB):
*        Added protected astInitIntraMapVtab method.
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

/* Define a dummy __attribute__ macro for use on non-GNU compilers. */
#ifndef __GNUC__
#  define  __attribute__(x)  /*NOTHING*/
#endif

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
   char *intraflag;              /* Pointer to identification string */
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

/* A Unique identifier to determine class membership. */
   AstClassIdentifier id;

/* Properties (e.g. methods) specific to this class. */
   const char *(* GetIntraFlag)( AstIntraMap *, int * );
   int (* TestIntraFlag)( AstIntraMap *, int * );
   void (* ClearIntraFlag)( AstIntraMap *, int * );
   void (* SetIntraFlag)( AstIntraMap *, const char *, int * );
} AstIntraMapVtab;


/* Structure to hold data for transformation functions. */
typedef struct AstIntraMapTranData {
   void (* tran)( AstMapping *, int, int, const double *[], int, int, double *[] );
                                 /* Pointer to transformation function */
   void (* tran_wrap)( void (*)( AstMapping *, int, int, const double *[], int, int, double *[] ), AstMapping *, int, int, const double *[], int, int, double *[], int * );
                                 /* Pointer to wrapper function */
   char *author;                 /* Author's name */
   char *contact;                /* Contact details (e.g. e-mail address) */
   char *name;                   /* Function name (assigned by caller) */
   char *purpose;                /* Comment string describing purpose */
   int nin;                      /* Number of input coordinates per point */
   int nout;                     /* Number of output coordinates per point */
   unsigned int flags;           /* Flags to describe function behaviour */
} AstIntraMapTranData;

#if defined(THREAD_SAFE)

/* Define a structure holding all data items that are global within this
   class. */
typedef struct AstIntraMapGlobals {
   AstIntraMapVtab Class_Vtab;
   int Class_Init;
} AstIntraMapGlobals;

#endif

#endif

/* Function prototypes. */
/* ==================== */
/* Prototypes for standard class functions. */
/* ---------------------------------------- */
astPROTO_CHECK(IntraMap)          /* Check class membership */
astPROTO_ISA(IntraMap)            /* Test class membership */

/* Constructor. */
#if defined(astCLASS)            /* Protected. */
AstIntraMap *astIntraMap_( const char *, int, int, const char *, int *, ...);
#else
AstIntraMap *astIntraMapId_( const char *, int, int, const char *, ... )__attribute__((format(printf,4,5)));
#endif

#if defined(astCLASS)            /* Protected */

/* Initialiser. */
AstIntraMap *astInitIntraMap_( void *, size_t, int, AstIntraMapVtab *,
                               const char *, const char *, int, int, int * );

/* Vtab initialiser. */
void astInitIntraMapVtab_( AstIntraMapVtab *, const char *, int * );

/* Loader. */
AstIntraMap *astLoadIntraMap_( void *, size_t, AstIntraMapVtab *,
                               const char *, AstChannel *, int * );

/* Thread-safe initialiser for all global data used by this module. */
#if defined(THREAD_SAFE)
void astInitIntraMapGlobals_( AstIntraMapGlobals * );
#endif

#endif

/* Prototypes for member functions. */
/* -------------------------------- */
void astIntraReg_( const char *, int, int, void (*)( AstMapping *, int, int, const double *[], int, int, double *[] ), unsigned int, const char *, const char *, const char *, int * );

#if defined(astCLASS)            /* Protected */
const char *astGetIntraFlag_( AstIntraMap *, int * );
int astTestIntraFlag_( AstIntraMap *, int * );
void astClearIntraFlag_( AstIntraMap *, int * );
void astSetIntraFlag_( AstIntraMap *, const char *, int * );

#else                            /* Public only */
void astIntraRegFor_( const char *, int, int, void (*)( AstMapping *, int, int, const double *[], int, int, double *[] ), void (*)( void (*)( AstMapping *, int, int, const double *[], int, int, double *[]), AstMapping *, int, int, const double *[], int, int, double *[], int * ), unsigned int, const char *, const char *, const char *, int * );

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
#define astCheckIntraMap(this) astINVOKE_CHECK(IntraMap,this,0)
#define astVerifyIntraMap(this) astINVOKE_CHECK(IntraMap,this,1)

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
astINVOKE(O,astInitIntraMap_(mem,size,init,vtab,name,fname,nin,nout,STATUS_PTR))

/* Vtab Initialiser. */
#define astInitIntraMapVtab(vtab,name) astINVOKE(V,astInitIntraMapVtab_(vtab,name,STATUS_PTR))
/* Loader. */
#define astLoadIntraMap(mem,size,vtab,name,channel) \
astINVOKE(O,astLoadIntraMap_(mem,size,vtab,name,astCheckChannel(channel),STATUS_PTR))
#endif

/* Interfaces to public member functions. */
/* -------------------------------------- */
/* Here we make use of astCheckIntraMap to validate IntraMap pointers
   before use.  This provides a contextual error report if a pointer
   to the wrong sort of Object is supplied. */

#define astIntraReg(name,nin,nout,tran,flags,purpose,author,contact) \
astIntraReg_(name,nin,nout,tran,flags,purpose,author,contact,STATUS_PTR)

#if defined(astCLASS)            /* Protected */
#define astClearIntraFlag(this) \
astINVOKE(V,astClearIntraFlag_(astCheckIntraMap(this),STATUS_PTR))
#define astGetIntraFlag(this) \
astINVOKE(V,astGetIntraFlag_(astCheckIntraMap(this),STATUS_PTR))
#define astSetIntraFlag(this,value) \
astINVOKE(V,astSetIntraFlag_(astCheckIntraMap(this),value,STATUS_PTR))
#define astTestIntraFlag(this) \
astINVOKE(V,astTestIntraFlag_(astCheckIntraMap(this),STATUS_PTR))

#else                            /* Public only */
#define astIntraRegFor(name,nin,nout,tran,tran_wrap,flags,purpose,author,contact) \
astIntraRegFor_(name,nin,nout,tran,tran_wrap,flags,purpose,author,contact,STATUS_PTR)
#endif
#endif





