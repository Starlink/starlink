#if !defined( CIRCLE_INCLUDED ) /* Include this file only once */
#define CIRCLE_INCLUDED
/*
*+
*  Name:
*     circle.h

*  Type:
*     C include file.

*  Purpose:
*     Define the interface to the Circle class.

*  Invocation:
*     #include "circle.h"

*  Description:
*     This include file defines the interface to the Circle class and
*     provides the type definitions, function prototypes and macros,
*     etc.  needed to use this class.
*
*     The Circle class implement a Region which represents a simple interval
*     on each axis of the encapsulated Frame

*  Inheritance:
*     The Circle class inherits from the Region class.

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
*     31-AUG-2004 (DSB):
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
/* Circle structure. */
/* ------------------ */
/* This structure contains all information that is unique to each object in
   the class (e.g. its instance variables). */
typedef struct AstCircle {

/* Attributes inherited from the parent class. */
   AstRegion region;             /* Parent class structure */

/* Attributes specific to objects in this class. */
   double *centre;               /* Circle centre coords */
   double radius;                /* Circle radius */
   double *lb;                   /* Lower limits of mesh bounding box */
   double *ub;                   /* Upper limit of mesh bounding box */
   int stale;                    /* Is Cached information stale? */

} AstCircle;

/* Virtual function table. */
/* ----------------------- */
/* This table contains all information that is the same for all
   objects in the class (e.g. pointers to its virtual functions). */
#if defined(astCLASS)            /* Protected */
typedef struct AstCircleVtab {

/* Properties (e.g. methods) inherited from the parent class. */
   AstRegionVtab region_vtab;    /* Parent class virtual function table */

/* A Unique identifier to determine class membership. */
   AstClassIdentifier id;

/* Properties (e.g. methods) specific to this class. */
   void (* CirclePars)( AstCircle *, double *, double *, double *, int * );
} AstCircleVtab;

#if defined(THREAD_SAFE)

/* Define a structure holding all data items that are global within the
   object.c file. */

typedef struct AstCircleGlobals {
   AstCircleVtab Class_Vtab;
   int Class_Init;
} AstCircleGlobals;


/* Thread-safe initialiser for all global data used by this module. */
void astInitCircleGlobals_( AstCircleGlobals * );

#endif


#endif

/* Function prototypes. */
/* ==================== */
/* Prototypes for standard class functions. */
/* ---------------------------------------- */
astPROTO_CHECK(Circle)          /* Check class membership */
astPROTO_ISA(Circle)            /* Test class membership */

/* Constructor. */
#if defined(astCLASS)            /* Protected. */
AstCircle *astCircle_( void *, int, const double[], const double[], AstRegion *, const char *, int *, ...);
#else
AstCircle *astCircleId_( void *, int, const double[], const double[],
                         AstRegion *, const char *, ... )__attribute__((format(printf,6,7)));
#endif

#if defined(astCLASS)            /* Protected */

/* Initialiser. */
AstCircle *astInitCircle_( void *, size_t, int, AstCircleVtab *,
                           const char *, AstFrame *, int, const double[],
                           const double[], AstRegion *, int * );

/* Vtab initialiser. */
void astInitCircleVtab_( AstCircleVtab *, const char *, int * );

/* Loader. */
AstCircle *astLoadCircle_( void *, size_t, AstCircleVtab *,
                           const char *, AstChannel *, int * );

#endif

/* Prototypes for member functions. */
/* -------------------------------- */
void astCirclePars_( AstCircle *, double *, double *, double *, int * );

# if defined(astCLASS)           /* Protected */
AstRegion *astBestCircle_( AstPointSet *, double *, AstRegion *, int * );
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
#define astCheckCircle(this) astINVOKE_CHECK(Circle,this,0)
#define astVerifyCircle(this) astINVOKE_CHECK(Circle,this,1)

/* Test class membership. */
#define astIsACircle(this) astINVOKE_ISA(Circle,this)

/* Constructor. */
#if defined(astCLASS)            /* Protected. */
#define astCircle astINVOKE(F,astCircle_)
#else
#define astCircle astINVOKE(F,astCircleId_)
#endif

#if defined(astCLASS)            /* Protected */

/* Initialiser. */
#define astInitCircle(mem,size,init,vtab,name,frame,form,p1,p2,unc) \
astINVOKE(O,astInitCircle_(mem,size,init,vtab,name,frame,form,p1,p2,unc,STATUS_PTR))

/* Vtab Initialiser. */
#define astInitCircleVtab(vtab,name) astINVOKE(V,astInitCircleVtab_(vtab,name,STATUS_PTR))
/* Loader. */
#define astLoadCircle(mem,size,vtab,name,channel) \
astINVOKE(O,astLoadCircle_(mem,size,vtab,name,astCheckChannel(channel),STATUS_PTR))
#endif

/* Interfaces to public member functions. */
/* -------------------------------------- */
/* Here we make use of astCheckCircle to validate Circle pointers
   before use.  This provides a contextual error report if a pointer
   to the wrong sort of Object is supplied. */

#define astCirclePars(this,centre,radius,p1) \
astINVOKE(V,astCirclePars_(astCheckCircle(this),centre,radius,p1,STATUS_PTR))

#if defined(astCLASS)            /* Protected */
#define astBestCircle(pset,cen,unc) astBestCircle_(pset,cen,unc,STATUS_PTR)
#endif
#endif





