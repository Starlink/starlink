#if !defined( INTERVAL_INCLUDED ) /* Include this file only once */
#define INTERVAL_INCLUDED
/*
*+
*  Name:
*     interval.h

*  Type:
*     C include file.

*  Purpose:
*     Define the interface to the Interval class.

*  Invocation:
*     #include "interval.h"

*  Description:
*     This include file defines the interface to the Interval class and
*     provides the type definitions, function prototypes and macros,
*     etc.  needed to use this class.
*
*     The Interval class implement a Region which represents a simple interval
*     on each axis of the encapsulated Frame

*  Inheritance:
*     The Interval class inherits from the Region class.

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
*     1-NOV-2004 (DSB):
*        Original version.
*-
*/

/* Include files. */
/* ============== */
/* Interface definitions. */
/* ---------------------- */
#include "region.h"              /* Coordinate regions (parent class) */
#include "box.h"                 /* Closed box regions */

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
/* Interval structure. */
/* ------------------ */
/* This structure contains all information that is unique to each object in
   the class (e.g. its instance variables). */
typedef struct AstInterval {

/* Attributes inherited from the parent class. */
   AstRegion region;             /* Parent class structure */

/* Attributes specific to objects in this class. */
   double *lbnd;                 /* Lower limits */
   double *ubnd;                 /* Lower limits */
   AstBox *box;                  /* Equivalent Box */
   int stale;                    /* Is cached information stale? */

} AstInterval;

/* Virtual function table. */
/* ----------------------- */
/* This table contains all information that is the same for all
   objects in the class (e.g. pointers to its virtual functions). */
#if defined(astCLASS)            /* Protected */
typedef struct AstIntervalVtab {

/* Properties (e.g. methods) inherited from the parent class. */
   AstRegionVtab region_vtab;    /* Parent class virtual function table */

/* A Unique identifier to determine class membership. */
   AstClassIdentifier id;

/* Properties (e.g. methods) specific to this class. */
   void (* IntervalPoints)( AstInterval *, double *, double *, int *);

} AstIntervalVtab;

#if defined(THREAD_SAFE)

/* Define a structure holding all data items that are global within the
   object.c file. */

typedef struct AstIntervalGlobals {
   AstIntervalVtab Class_Vtab;
   int Class_Init;
} AstIntervalGlobals;


/* Thread-safe initialiser for all global data used by this module. */
void astInitIntervalGlobals_( AstIntervalGlobals * );

#endif


#endif

/* Function prototypes. */
/* ==================== */
/* Prototypes for standard class functions. */
/* ---------------------------------------- */
astPROTO_CHECK(Interval)          /* Check class membership */
astPROTO_ISA(Interval)            /* Test class membership */

/* Constructor. */
#if defined(astCLASS)            /* Protected. */
AstInterval *astInterval_( void *, const double[], const double[], AstRegion *, const char *, int *, ...);
#else
AstInterval *astIntervalId_( void *, const double[], const double[], AstRegion *, const char *, ... )__attribute__((format(printf,5,6)));
#endif

#if defined(astCLASS)            /* Protected */

/* Initialiser. */
AstInterval *astInitInterval_( void *, size_t, int, AstIntervalVtab *,
                               const char *, AstFrame *, const double[],
                               const double[], AstRegion *, int * );

/* Vtab initialiser. */
void astInitIntervalVtab_( AstIntervalVtab *, const char *, int * );

/* Loader. */
AstInterval *astLoadInterval_( void *, size_t, AstIntervalVtab *,
                               const char *, AstChannel *, int * );

#endif

/* Prototypes for member functions. */
/* -------------------------------- */
# if defined(astCLASS)           /* Protected */
void astIntervalPoints_( AstInterval *, double *, double *, int *);
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
#define astCheckInterval(this) astINVOKE_CHECK(Interval,this,0)
#define astVerifyInterval(this) astINVOKE_CHECK(Interval,this,1)

/* Test class membership. */
#define astIsAInterval(this) astINVOKE_ISA(Interval,this)

/* Constructor. */
#if defined(astCLASS)            /* Protected. */
#define astInterval astINVOKE(F,astInterval_)
#else
#define astInterval astINVOKE(F,astIntervalId_)
#endif

#if defined(astCLASS)            /* Protected */

/* Initialiser. */
#define astInitInterval(mem,size,init,vtab,name,frame,lbnd,ubnd,unc) \
astINVOKE(O,astInitInterval_(mem,size,init,vtab,name,frame,lbnd,ubnd,unc,STATUS_PTR))

/* Vtab Initialiser. */
#define astInitIntervalVtab(vtab,name) astINVOKE(V,astInitIntervalVtab_(vtab,name,STATUS_PTR))
/* Loader. */
#define astLoadInterval(mem,size,vtab,name,channel) \
astINVOKE(O,astLoadInterval_(mem,size,vtab,name,astCheckChannel(channel),STATUS_PTR))
#endif

/* Interfaces to public member functions. */
/* -------------------------------------- */
/* Here we make use of astCheckInterval to validate Interval pointers
   before use.  This provides a contextual error report if a pointer
   to the wrong sort of Object is supplied. */

#if defined(astCLASS)            /* Protected */
#define astIntervalPoints(this,lbnd,ubnd) astINVOKE(V,astIntervalPoints_(astCheckInterval(this),lbnd,ubnd,STATUS_PTR))
#endif
#endif





