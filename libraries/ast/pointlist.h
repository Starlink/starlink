#if !defined( POINTLIST_INCLUDED ) /* Include this file only once */
#define POINTLIST_INCLUDED
/*
*+
*  Name:
*     pointlist.h

*  Type:
*     C include file.

*  Purpose:
*     Define the interface to the PointList class.

*  Invocation:
*     #include "pointlist.h"

*  Description:
*     This include file defines the interface to the PointList class and
*     provides the type definitions, function prototypes and macros,
*     etc.  needed to use this class.
*
*     The PointList class implements a Region which represents a collection
*     of points in a Frame.

*  Inheritance:
*     The PointList class inherits from the Region class.

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
*     23-AUG-2004 (DSB):
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
/* PointList structure. */
/* ------------------ */
/* This structure contains all information that is unique to each object in
   the class (e.g. its instance variables). */
typedef struct AstPointList {

/* Attributes inherited from the parent class. */
   AstRegion region;          /* Parent class structure */

/* Attributes specific to objects in this class. */
   double *lbnd;              /* Lower axis limits of bounding box */
   double *ubnd;              /* Upper axis limits of bounding box */
} AstPointList;

/* Virtual function table. */
/* ----------------------- */
/* This table contains all information that is the same for all
   objects in the class (e.g. pointers to its virtual functions). */
#if defined(astCLASS)            /* Protected */
typedef struct AstPointListVtab {

/* Properties (e.g. methods) inherited from the parent class. */
   AstRegionVtab region_vtab;    /* Parent class virtual function table */

/* A Unique identifier to determine class membership. */
   AstClassIdentifier id;

/* Properties (e.g. methods) specific to this class. */
   int (* GetListSize)( AstPointList *, int * );
   void (* PointListPoints)( AstPointList *, AstPointSet **, int * );
} AstPointListVtab;

#if defined(THREAD_SAFE)

/* Define a structure holding all data items that are global within the
   object.c file. */

typedef struct AstPointListGlobals {
   AstPointListVtab Class_Vtab;
   int Class_Init;
   char GetAttrib_Buff[ 101 ];
} AstPointListGlobals;


/* Thread-safe initialiser for all global data used by this module. */
void astInitPointListGlobals_( AstPointListGlobals * );

#endif


#endif

/* Function prototypes. */
/* ==================== */
/* Prototypes for standard class functions. */
/* ---------------------------------------- */
astPROTO_CHECK(PointList)          /* Check class membership */
astPROTO_ISA(PointList)            /* Test class membership */

/* Constructor. */
#if defined(astCLASS)            /* Protected. */
AstPointList *astPointList_( void *, AstPointSet *, AstRegion *, const char *, int *, ...);
#else
AstPointList *astPointListId_( void *, int, int, int, const double *, AstRegion *, const char *, ... )__attribute__((format(printf,7,8)));
#endif

#if defined(astCLASS)            /* Protected */

/* Initialiser. */
AstPointList *astInitPointList_( void *, size_t, int, AstPointListVtab *,
                                 const char *, AstFrame *, AstPointSet *,
                                 AstRegion *, int * );

/* Vtab initialiser. */
void astInitPointListVtab_( AstPointListVtab *, const char *, int * );

/* Loader. */
AstPointList *astLoadPointList_( void *, size_t, AstPointListVtab *,
                                 const char *, AstChannel *, int * );

#endif

/* Prototypes for member functions. */
/* -------------------------------- */
# if defined(astCLASS)           /* Protected */
int astGetListSize_( AstPointList *, int * );
void astPointListPoints_( AstPointList *, AstPointSet **, int * );
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
#define astCheckPointList(this) astINVOKE_CHECK(PointList,this,0)
#define astVerifyPointList(this) astINVOKE_CHECK(PointList,this,1)

/* Test class membership. */
#define astIsAPointList(this) astINVOKE_ISA(PointList,this)

/* Constructor. */
#if defined(astCLASS)            /* Protected. */
#define astPointList astINVOKE(F,astPointList_)
#else
#define astPointList astINVOKE(F,astPointListId_)
#endif

#if defined(astCLASS)            /* Protected */

/* Initialiser. */
#define astInitPointList(mem,size,init,vtab,name,frame,points,unc) \
astINVOKE(O,astInitPointList_(mem,size,init,vtab,name,frame,points,unc,STATUS_PTR))

/* Vtab Initialiser. */
#define astInitPointListVtab(vtab,name) astINVOKE(V,astInitPointListVtab_(vtab,name,STATUS_PTR))

/* Loader. */
#define astLoadPointList(mem,size,vtab,name,channel) \
astINVOKE(O,astLoadPointList_(mem,size,vtab,name,astCheckChannel(channel),STATUS_PTR))
#endif

/* Interfaces to public member functions. */
/* -------------------------------------- */
/* Here we make use of astCheckPointList to validate PointList pointers
   before use.  This provides a contextual error report if a pointer
   to the wrong sort of Object is supplied. */

#if defined(astCLASS)            /* Protected */
#define astGetListSize(this) \
astINVOKE(V,astGetListSize_(astCheckPointList(this),STATUS_PTR))
#define astPointListPoints(this,pset) \
astINVOKE(V,astPointListPoints_(astCheckPointList(this),pset,STATUS_PTR))

#endif
#endif





