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
*     Public:
*        None.
*
*     Protected:
*        astGetConstants
*           Obtain a copy of the constants array
*        astGetInPerm
*           Obtain a copy of the input permutation array
*        astGetOutPerm
*           Obtain a copy of the output permutation array

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
*        astInitPermMapVtab
*           Initialise the virtual function table for the PermMap class.
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
*     29-FEB-1996 (RFWS):
*        Original version.
*     26-SEP-1996 (RFWS):
*        Added external interface and I/O facilities.
*     8-JAN-2003 (DSB):
*        Changed private InitVtab method to protected astInitPermMapVtab
*        method.
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
/* ====== */

/* Define a dummy __attribute__ macro for use on non-GNU compilers. */
#ifndef __GNUC__
#  define  __attribute__(x)  /*NOTHING*/
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
   int permsplit;                /* Method to use within MapSplit */
} AstPermMap;

/* Virtual function table. */
/* ----------------------- */
/* This table contains all information that is the same for all
   objects in the class (e.g. pointers to its virtual functions). */
#if defined(astCLASS)            /* Protected */
typedef struct AstPermMapVtab {

/* Properties (e.g. methods) inherited from the parent class. */
   AstMappingVtab mapping_vtab;  /* Parent class virtual function table */

/* A Unique identifier to determine class membership. */
   AstClassIdentifier id;

/* Properties (e.g. methods) specific to this class. */
   double *(* GetConstants)( AstPermMap *, int * );
   int *(* GetInPerm)( AstPermMap *, int * );
   int *(* GetOutPerm)( AstPermMap *, int * );
   void (* SetPermSplit)( AstPermMap *, int, int * );
   void (* ClearPermSplit)( AstPermMap *, int * );
   int (* TestPermSplit)( AstPermMap *, int * );
   int (* GetPermSplit)( AstPermMap *, int * );

} AstPermMapVtab;

#if defined(THREAD_SAFE)

/* Define a structure holding all data items that are global within the
   object.c file. */

typedef struct AstPermMapGlobals {
   AstPermMapVtab Class_Vtab;
   int Class_Init;
} AstPermMapGlobals;


/* Thread-safe initialiser for all global data used by this module. */
void astInitPermMapGlobals_( AstPermMapGlobals * );

#endif


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
                         const double [], const char *, int *, ...);
#else
AstPermMap *astPermMapId_( int, const int [], int, const int [],
                           const double [], const char *, ... )__attribute__((format(printf,6,7)));
#endif

#if defined(astCLASS)            /* Protected */

/* Initialiser. */
AstPermMap *astInitPermMap_( void *, size_t, int, AstPermMapVtab *,
                             const char *, int, const int [], int,
                             const int [], const double [], int * );

/* Vtab initialiser. */
void astInitPermMapVtab_( AstPermMapVtab *, const char *, int * );

/* Loader. */
AstPermMap *astLoadPermMap_( void *, size_t, AstPermMapVtab *,
                             const char *, AstChannel *, int * );
#endif

/* Prototypes for member functions. */
/* -------------------------------- */
# if defined(astCLASS)           /* Protected */
double *astGetConstants_( AstPermMap *, int * );
int *astGetInPerm_( AstPermMap *, int * );
int *astGetOutPerm_( AstPermMap *, int * );
void astSetPermSplit_( AstPermMap *, int, int * );
void astClearPermSplit_( AstPermMap *, int * );
int astTestPermSplit_( AstPermMap *, int * );
int astGetPermSplit_( AstPermMap *, int * );
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
#define astCheckPermMap(this) astINVOKE_CHECK(PermMap,this,0)
#define astVerifyPermMap(this) astINVOKE_CHECK(PermMap,this,1)

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
astINVOKE(O,astInitPermMap_(mem,size,init,vtab,name,nin,inperm,nout,outperm,constant,STATUS_PTR))

/* Vtab Initialiser. */
#define astInitPermMapVtab(vtab,name) astINVOKE(V,astInitPermMapVtab_(vtab,name,STATUS_PTR))
/* Loader. */
#define astLoadPermMap(mem,size,vtab,name,channel) \
astINVOKE(O,astLoadPermMap_(mem,size,vtab,name,astCheckChannel(channel),STATUS_PTR))
#endif

/* Interfaces to public member functions. */
/* -------------------------------------- */
/* Here we make use of astCheckPermMap to validate PermMap pointers
   before use.  This provides a contextual error report if a pointer
   to the wrong sort of Object is supplied. */

#if defined(astCLASS)            /* Protected */
#define astGetConstants(this) astINVOKE(V,astGetConstants_(astCheckPermMap(this),STATUS_PTR))
#define astGetInPerm(this) astINVOKE(V,astGetInPerm_(astCheckPermMap(this),STATUS_PTR))
#define astGetOutPerm(this) astINVOKE(V,astGetOutPerm_(astCheckPermMap(this),STATUS_PTR))
#define astSetPermSplit(this,permsplit) astINVOKE(V,astSetPermSplit_(astCheckPermMap(this),permsplit,STATUS_PTR))
#define astClearPermSplit(this) astINVOKE(V,astClearPermSplit_(astCheckPermMap(this),STATUS_PTR))
#define astTestPermSplit(this) astINVOKE(V,astTestPermSplit_(astCheckPermMap(this),STATUS_PTR))
#define astGetPermSplit(this) astINVOKE(V,astGetPermSplit_(astCheckPermMap(this),STATUS_PTR))
#endif

#endif





