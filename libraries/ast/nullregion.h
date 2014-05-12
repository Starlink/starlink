#if !defined( NULLREGION_INCLUDED ) /* Include this file only once */
#define NULLREGION_INCLUDED
/*
*+
*  Name:
*     nullregion.h

*  Type:
*     C include file.

*  Purpose:
*     Define the interface to the NullRegion class.

*  Invocation:
*     #include "nullregion.h"

*  Description:
*     This include file defines the interface to the NullRegion class and
*     provides the type definitions, function prototypes and macros,
*     etc.  needed to use this class.
*
*     The NullRegion class implement a Region with no boundaries.

*  Inheritance:
*     The NullRegion class inherits from the Region class.

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

/* Macros */
/* ====== */

/* Define a dummy __attribute__ macro for use on non-GNU compilers. */
#ifndef __GNUC__
#  define  __attribute__(x)  /*NOTHING*/
#endif

/* Type Definitions. */
/* ================= */
/* NullRegion structure. */
/* ------------------ */
/* This structure contains all information that is unique to each object in
   the class (e.g. its instance variables). */
typedef struct AstNullRegion {

/* Attributes inherited from the parent class. */
   AstRegion region;             /* Parent class structure */

} AstNullRegion;

/* Virtual function table. */
/* ----------------------- */
/* This table contains all information that is the same for all
   objects in the class (e.g. pointers to its virtual functions). */
#if defined(astCLASS)            /* Protected */
typedef struct AstNullRegionVtab {

/* Properties (e.g. methods) inherited from the parent class. */
   AstRegionVtab region_vtab;    /* Parent class virtual function table */

/* A Unique identifier to determine class membership. */
   AstClassIdentifier id;

/* Properties (e.g. methods) specific to this class. */
} AstNullRegionVtab;

#if defined(THREAD_SAFE)

/* Define a structure holding all data items that are global within the
   object.c file. */

typedef struct AstNullRegionGlobals {
   AstNullRegionVtab Class_Vtab;
   int Class_Init;
} AstNullRegionGlobals;


/* Thread-safe initialiser for all global data used by this module. */
void astInitNullRegionGlobals_( AstNullRegionGlobals * );

#endif


#endif

/* Function prototypes. */
/* ==================== */
/* Prototypes for standard class functions. */
/* ---------------------------------------- */
astPROTO_CHECK(NullRegion)          /* Check class membership */
astPROTO_ISA(NullRegion)            /* Test class membership */

/* Constructor. */
#if defined(astCLASS)            /* Protected. */
AstNullRegion *astNullRegion_( void *, AstRegion *, const char *, int *, ...);
#else
AstNullRegion *astNullRegionId_( void *, AstRegion *, const char *, ... )__attribute__((format(printf,3,4)));
#endif

#if defined(astCLASS)            /* Protected */

/* Initialiser. */
AstNullRegion *astInitNullRegion_( void *, size_t, int, AstNullRegionVtab *,
                                   const char *, AstFrame *, AstRegion *, int * );

/* Vtab initialiser. */
void astInitNullRegionVtab_( AstNullRegionVtab *, const char *, int * );

/* Loader. */
AstNullRegion *astLoadNullRegion_( void *, size_t, AstNullRegionVtab *,
                                   const char *, AstChannel *, int * );

#endif

/* Prototypes for member functions. */
/* -------------------------------- */

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
#define astCheckNullRegion(this) astINVOKE_CHECK(NullRegion,this,0)
#define astVerifyNullRegion(this) astINVOKE_CHECK(NullRegion,this,1)

/* Test class membership. */
#define astIsANullRegion(this) astINVOKE_ISA(NullRegion,this)

/* Constructor. */
#if defined(astCLASS)            /* Protected. */
#define astNullRegion astINVOKE(F,astNullRegion_)
#else
#define astNullRegion astINVOKE(F,astNullRegionId_)
#endif

#if defined(astCLASS)            /* Protected */

/* Initialiser. */
#define astInitNullRegion(mem,size,init,vtab,name,frame,unc) \
astINVOKE(O,astInitNullRegion_(mem,size,init,vtab,name,frame,unc,STATUS_PTR))

/* Vtab Initialiser. */
#define astInitNullRegionVtab(vtab,name) astINVOKE(V,astInitNullRegionVtab_(vtab,name,STATUS_PTR))
/* Loader. */
#define astLoadNullRegion(mem,size,vtab,name,channel) \
astINVOKE(O,astLoadNullRegion_(mem,size,vtab,name,astCheckChannel(channel),STATUS_PTR))
#endif

/* Interfaces to public member functions. */
/* -------------------------------------- */
/* Here we make use of astCheckNullRegion to validate NullRegion pointers
   before use.  This provides a contextual error report if a pointer
   to the wrong sort of Object is supplied. */

#if defined(astCLASS)            /* Protected */
#endif
#endif





