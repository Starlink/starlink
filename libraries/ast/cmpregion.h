#if !defined( CMPREGION_INCLUDED ) /* Include this file only once */
#define CMPREGION_INCLUDED
/*
*+
*  Name:
*     cmpregion.h

*  Type:
*     C include file.

*  Purpose:
*     Define the interface to the CmpRegion class.

*  Invocation:
*     #include "cmpregion.h"

*  Description:
*     This include file defines the interface to the CmpRegion class and
*     provides the type definitions, function prototypes and macros,
*     etc.  needed to use this class.
*
*     The CmpRegion class implement a Region which represents a simple interval
*     on each axis of the encapsulated Frame

*  Inheritance:
*     The CmpRegion class inherits from the Region class.

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
*     DSB: David S. Berry (Starlink)

*  History:
*     11-OCT-2004 (DSB):
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

/* Macros. */
/* ------- */
/* Boolean operators */

#if defined(astCLASS) || defined(astFORTRAN77)
#define STATUS_PTR status
#else
#define STATUS_PTR astGetStatusPtr
#endif
#define AST__AND 1
#define AST__OR 2
#define AST__XOR 3

/* Define a dummy __attribute__ macro for use on non-GNU compilers. */
#ifndef __GNUC__
#  define  __attribute__(x)  /*NOTHING*/
#endif

/* Type Definitions. */
/* ================= */
/* CmpRegion structure. */
/* ------------------ */
/* This structure contains all information that is unique to each object in
   the class (e.g. its instance variables). */
typedef struct AstCmpRegion {

/* Attributes inherited from the parent class. */
   AstRegion region;             /* Parent class structure */

/* Attributes specific to objects in this class. */
   AstRegion *region1;           /* First component Region */
   AstRegion *region2;           /* Second component Region */
   int oper;                     /* Boolean operator */
   double *rvals[ 2 ];           /* Used boundary length at each break */
   double *offs[ 2 ];            /* Jump at each break */
   int nbreak[ 2 ];              /* Number of breaks */
   double d0[ 2 ];               /* Total used boundary length */
   double dtot[ 2 ];             /* Total boundary length */
   AstRegion *xor1;              /* First XORed Region */
   AstRegion *xor2;              /* Second XORed Region */
   int bounded;                  /* Is this CmpRegion bounded? */
} AstCmpRegion;

/* Virtual function table. */
/* ----------------------- */
/* This table contains all information that is the same for all
   objects in the class (e.g. pointers to its virtual functions). */
#if defined(astCLASS)            /* Protected */
typedef struct AstCmpRegionVtab {

/* Properties (e.g. methods) inherited from the parent class. */
   AstRegionVtab region_vtab;    /* Parent class virtual function table */

/* A Unique identifier to determine class membership. */
   AstClassIdentifier id;

/* Properties (e.g. methods) specific to this class. */
   int (* CmpRegionList)(  AstCmpRegion *, int *, AstRegion ***, int * );

} AstCmpRegionVtab;

#if defined(THREAD_SAFE)

/* Define a structure holding all data items that are global within the
   object.c file. */

typedef struct AstCmpRegionGlobals {
   AstCmpRegionVtab Class_Vtab;
   int Class_Init;
} AstCmpRegionGlobals;


/* Thread-safe initialiser for all global data used by this module. */
void astInitCmpRegionGlobals_( AstCmpRegionGlobals * );

#endif


#endif

/* Function prototypes. */
/* ==================== */
/* Prototypes for standard class functions. */
/* ---------------------------------------- */
astPROTO_CHECK(CmpRegion)          /* Check class membership */
astPROTO_ISA(CmpRegion)            /* Test class membership */

/* Constructor. */
#if defined(astCLASS)            /* Protected. */
AstCmpRegion *astCmpRegion_( void *, void *, int, const char *, int *, ...);
#else
AstCmpRegion *astCmpRegionId_( void *, void *, int, const char *, ... )__attribute__((format(printf,4,5)));
#endif

#if defined(astCLASS)            /* Protected */

/* Initialiser. */
AstCmpRegion *astInitCmpRegion_( void *, size_t, int, AstCmpRegionVtab *,
                     const char *, AstRegion *, AstRegion *, int, int * );

/* Vtab initialiser. */
void astInitCmpRegionVtab_( AstCmpRegionVtab *, const char *, int * );

/* Loader. */
AstCmpRegion *astLoadCmpRegion_( void *, size_t, AstCmpRegionVtab *,
                                 const char *, AstChannel *, int * );

#endif

/* Prototypes for member functions. */
/* -------------------------------- */
# if defined(astCLASS)           /* Protected */
int astCmpRegionList_(  AstCmpRegion *, int *, AstRegion ***, int * );
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
#define astCheckCmpRegion(this) astINVOKE_CHECK(CmpRegion,this,0)
#define astVerifyCmpRegion(this) astINVOKE_CHECK(CmpRegion,this,1)

/* Test class membership. */
#define astIsACmpRegion(this) astINVOKE_ISA(CmpRegion,this)

/* Constructor. */
#if defined(astCLASS)            /* Protected. */
#define astCmpRegion astINVOKE(F,astCmpRegion_)
#else
#define astCmpRegion astINVOKE(F,astCmpRegionId_)
#endif

#if defined(astCLASS)            /* Protected */

/* Initialiser. */
#define astInitCmpRegion(mem,size,init,vtab,name,reg1,reg2,oper) \
astINVOKE(O,astInitCmpRegion_(mem,size,init,vtab,name,reg1,reg2,oper,STATUS_PTR))

/* Vtab Initialiser. */
#define astInitCmpRegionVtab(vtab,name) astINVOKE(V,astInitCmpRegionVtab_(vtab,name,STATUS_PTR))
/* Loader. */
#define astLoadCmpRegion(mem,size,vtab,name,channel) \
astINVOKE(O,astLoadCmpRegion_(mem,size,vtab,name,astCheckChannel(channel),STATUS_PTR))
#endif

/* Interfaces to public member functions. */
/* -------------------------------------- */
/* Here we make use of astCheckCmpRegion to validate CmpRegion pointers
   before use.  This provides a contextual error report if a pointer
   to the wrong sort of Object is supplied. */

#if defined(astCLASS)            /* Protected */
#define astCmpRegionList(this,nreg,reg_list) \
astINVOKE(V,astCmpRegionList_(this,nreg,reg_list,STATUS_PTR))
#endif
#endif





