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

/* Macros */
/* ====== */
/* Define a dummy __attribute__ macro for use on non-GNU compilers. */
#ifndef __GNUC__
#  define  __attribute__(x)  /*NOTHING*/
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
   double *extent;               /* Original axis half-widths */
   double *centre;               /* Box centre coords */
   double *lo;                   /* Low limits */
   double *hi;                   /* High limits */
   double *geolen;               /* Geodesic half-dimensions of box */
   int stale;                    /* Is other info out of date? */
} AstBox;

/* Virtual function table. */
/* ----------------------- */
/* This table contains all information that is the same for all
   objects in the class (e.g. pointers to its virtual functions). */
#if defined(astCLASS)            /* Protected */
typedef struct AstBoxVtab {

/* Properties (e.g. methods) inherited from the parent class. */
   AstRegionVtab region_vtab;    /* Parent class virtual function table */

/* A Unique identifier to determine class membership. */
   AstClassIdentifier id;

/* Properties (e.g. methods) specific to this class. */
   void (* BoxPoints)( AstBox *, double *, double *, int *);
} AstBoxVtab;

#if defined(THREAD_SAFE)

/* Define a structure holding all data items that are global within the
   object.c file. */

typedef struct AstBoxGlobals {
   AstBoxVtab Class_Vtab;
   int Class_Init;
} AstBoxGlobals;


/* Thread-safe initialiser for all global data used by this module. */
void astInitBoxGlobals_( AstBoxGlobals * );

#endif


#endif

/* Function prototypes. */
/* ==================== */
/* Prototypes for standard class functions. */
/* ---------------------------------------- */
astPROTO_CHECK(Box)          /* Check class membership */
astPROTO_ISA(Box)            /* Test class membership */

/* Constructor. */
#if defined(astCLASS)            /* Protected. */
AstBox *astBox_( void *, int, const double[], const double[], AstRegion *, const char *, int *, ...);
#else
AstBox *astBoxId_( void *, int, const double[], const double[], AstRegion *, const char *, ... )__attribute__((format(printf,6,7)));
#endif

#if defined(astCLASS)            /* Protected */

/* Initialiser. */
AstBox *astInitBox_( void *, size_t, int, AstBoxVtab *,
                     const char *, AstFrame *, int, const double[],
                     const double[], AstRegion *, int * );

/* Vtab initialiser. */
void astInitBoxVtab_( AstBoxVtab *, const char *, int * );

/* Loader. */
AstBox *astLoadBox_( void *, size_t, AstBoxVtab *,
                     const char *, AstChannel *, int * );

#endif

/* Prototypes for member functions. */
/* -------------------------------- */
# if defined(astCLASS)           /* Protected */
void astBoxPoints_( AstBox *, double *, double *, int *);
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
#define astCheckBox(this) astINVOKE_CHECK(Box,this,0)
#define astVerifyBox(this) astINVOKE_CHECK(Box,this,1)

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
astINVOKE(O,astInitBox_(mem,size,init,vtab,name,frame,form,p1,p2,unc,STATUS_PTR))

/* Vtab Initialiser. */
#define astInitBoxVtab(vtab,name) astINVOKE(V,astInitBoxVtab_(vtab,name,STATUS_PTR))
/* Loader. */
#define astLoadBox(mem,size,vtab,name,channel) \
astINVOKE(O,astLoadBox_(mem,size,vtab,name,astCheckChannel(channel),STATUS_PTR))
#endif

/* Interfaces to public member functions. */
/* -------------------------------------- */
/* Here we make use of astCheckBox to validate Box pointers
   before use.  This provides a contextual error report if a pointer
   to the wrong sort of Object is supplied. */

#if defined(astCLASS)            /* Protected */
#define astBoxPoints(this,centre,corner) astINVOKE(V,astBoxPoints_(astCheckBox(this),centre,corner,STATUS_PTR))
#endif
#endif





