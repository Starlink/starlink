#if !defined( ZOOMMAP_INCLUDED ) /* Include this file only once */
#define ZOOMMAP_INCLUDED
/*
*+
*  Name:
*     zoommap.h

*  Type:
*     C include file.

*  Purpose:
*     Define the interface to the ZoomMap class.

*  Invocation:
*     #include "zoommap.h"

*  Description:
*     This include file defines the interface to the ZoomMap class and
*     provides the type definitions, function prototypes and macros,
*     etc.  needed to use this class.
*
*     The ZoomMap class implements Mappings which perform a "zoom"
*     transformation by multiplying all coordinate values by the same
*     scale factor (the inverse transformation is performed by
*     dividing by this scale factor).

*  Inheritance:
*     The ZoomMap class inherits from the Mapping class.

*  Attributes Over-Ridden:
*     None.

*  New Attributes Defined:
*     Zoom (double)
*        The ZoomMap scale factor, by which coordinate values are
*        multiplied (by the forward transformation) or divided (by the
*        inverse transformation). This factor is set when a ZoomMap is
*        created, but may later be modified. It may not be set to
*        zero. The default value (if cleared) is one.
*
*        Note that if the ZoomMap is inverted (by using astInvert or
*        setting a non-zero value for its Invert attribute), then the
*        reciprocal of this Zoom value will, in effect, be used.

*  Methods Over-Ridden:
*     Public:
*        None.
*
*     Protected:
*        astClearAttrib
*           Clear an attribute value for a ZoomMap.
*        astGetAttrib
*           Get an attribute value for a ZoomMap.
*        astSetAttrib
*           Set an attribute value for a ZoomMap.
*        astTestAttrib
*           Test if an attribute value has been set for a ZoomMap.
*        astTransform
*           Apply a ZoomMap to transform a set of points.

*  New Methods Defined:
*     Public:
*        None.
*
*     Protected:
*        astClearZoom
*           Clear the Zoom attribute value for a ZoomMap.
*        astGetZoom
*           Get the Zoom attribute value for a ZoomMap.
*        astSetZoom
*           Set the Zoom attribute value for a ZoomMap.
*        astTestZoom
*           Test if a Zoom attribute value has been set for a ZoomMap.

*  Other Class Functions:
*     Public:
*        astIsAZoomMap
*           Test class membership.
*        astZoomMap
*           Create a ZoomMap.
*
*     Protected:
*        astCheckZoomMap
*           Validate class membership.
*        astInitZoomMap
*           Initialise a ZoomMap.
*        astInitZoomMapVtab
*           Initialise the virtual function table for the ZoomMap class.
*        astLoadZoomMap
*           Load a ZoomMap.

*  Macros:
*     None.

*  Type Definitions:
*     Public:
*        AstZoomMap
*           ZoomMap object type.
*
*     Protected:
*        AstZoomMapVtab
*           ZoomMap virtual function table type.

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
*     30-JAN-1996 (RFWS):
*        Original version.
*     18-JUL-1996 (RFWS):
*        Updated to provide an external interface.
*     10-SEP-1996 (RFWS):
*        Added I/O facilities.
*     8-JAN-2003 (DSB):
*        Added protected astInitZoomMapVtab method.
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

/* Macros */
/* ====== */

/* Define a dummy __attribute__ macro for use on non-GNU compilers. */
#ifndef __GNUC__
#  define  __attribute__(x)  /*NOTHING*/
#endif

/* Type Definitions. */
/* ================= */
/* ZoomMap structure. */
/* ------------------ */
/* This structure contains all information that is unique to each object in
   the class (e.g. its instance variables). */
typedef struct AstZoomMap {

/* Attributes inherited from the parent class. */
   AstMapping mapping;           /* Parent class structure */

/* Attributes specific to objects in this class. */
   double zoom;                  /* Zoom factor */
} AstZoomMap;

/* Virtual function table. */
/* ----------------------- */
/* This table contains all information that is the same for all
   objects in the class (e.g. pointers to its virtual functions). */
#if defined(astCLASS)            /* Protected */
typedef struct AstZoomMapVtab {

/* Properties (e.g. methods) inherited from the parent class. */
   AstMappingVtab mapping_vtab;  /* Parent class virtual function table */

/* A Unique identifier to determine class membership. */
   AstClassIdentifier id;

/* Properties (e.g. methods) specific to this class. */
   double (*GetZoom)( AstZoomMap *, int * );
   int (* TestZoom)( AstZoomMap *, int * );
   void (* ClearZoom)( AstZoomMap *, int * );
   void (* SetZoom)( AstZoomMap *, double, int * );
} AstZoomMapVtab;


#if defined(THREAD_SAFE)

/* Define a structure holding all data items that are global within this
   class. */
typedef struct AstZoomMapGlobals {
   AstZoomMapVtab Class_Vtab;
   int Class_Init;
   char GetAttrib_Buff[ 101 ];
} AstZoomMapGlobals;

#endif

#endif

/* Function prototypes. */
/* ==================== */
/* Prototypes for standard class functions. */
/* ---------------------------------------- */
astPROTO_CHECK(ZoomMap)          /* Check class membership */
astPROTO_ISA(ZoomMap)            /* Test class membership */

/* Constructor. */
#if defined(astCLASS)            /* Protected. */
AstZoomMap *astZoomMap_( int, double, const char *, int *, ...);
#else
AstZoomMap *astZoomMapId_( int, double, const char *, ... )__attribute__((format(printf,3,4)));
#endif

#if defined(astCLASS)            /* Protected */

/* Initialiser. */
AstZoomMap *astInitZoomMap_( void *, size_t, int, AstZoomMapVtab *,
                             const char *, int, double, int * );

/* Vtab initialiser. */
void astInitZoomMapVtab_( AstZoomMapVtab *, const char *, int * );

/* Loader. */
AstZoomMap *astLoadZoomMap_( void *, size_t, AstZoomMapVtab *,
                             const char *, AstChannel *, int * );

/* Thread-safe initialiser for all global data used by this module. */
#if defined(THREAD_SAFE)
void astInitZoomMapGlobals_( AstZoomMapGlobals * );
#endif

#endif

/* Prototypes for member functions. */
/* -------------------------------- */
# if defined(astCLASS)           /* Protected */
double astGetZoom_( AstZoomMap *, int * );
int astTestZoom_( AstZoomMap *, int * );
void astClearZoom_( AstZoomMap *, int * );
void astSetZoom_( AstZoomMap *, double, int * );
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
#define astCheckZoomMap(this) astINVOKE_CHECK(ZoomMap,this,0)
#define astVerifyZoomMap(this) astINVOKE_CHECK(ZoomMap,this,1)

/* Test class membership. */
#define astIsAZoomMap(this) astINVOKE_ISA(ZoomMap,this)

/* Constructor. */
#if defined(astCLASS)            /* Protected. */
#define astZoomMap astINVOKE(F,astZoomMap_)
#else
#define astZoomMap astINVOKE(F,astZoomMapId_)
#endif

#if defined(astCLASS)            /* Protected */

/* Initialiser. */
#define astInitZoomMap(mem,size,init,vtab,name,ncoord,zoom) \
astINVOKE(O,astInitZoomMap_(mem,size,init,vtab,name,ncoord,zoom,STATUS_PTR))

/* Vtab Initialiser. */
#define astInitZoomMapVtab(vtab,name) astINVOKE(V,astInitZoomMapVtab_(vtab,name,STATUS_PTR))
/* Loader. */
#define astLoadZoomMap(mem,size,vtab,name,channel) \
astINVOKE(O,astLoadZoomMap_(mem,size,vtab,name,astCheckChannel(channel),STATUS_PTR))
#endif

/* Interfaces to public member functions. */
/* -------------------------------------- */
/* Here we make use of astCheckZoomMap to validate ZoomMap pointers
   before use.  This provides a contextual error report if a pointer
   to the wrong sort of Object is supplied. */

#if defined(astCLASS)            /* Protected */
#define astClearZoom(this) astINVOKE(V,astClearZoom_(astCheckZoomMap(this),STATUS_PTR))
#define astGetZoom(this) astINVOKE(V,astGetZoom_(astCheckZoomMap(this),STATUS_PTR))
#define astSetZoom(this,value) \
astINVOKE(V,astSetZoom_(astCheckZoomMap(this),value,STATUS_PTR))
#define astTestZoom(this) astINVOKE(V,astTestZoom_(astCheckZoomMap(this),STATUS_PTR))
#endif
#endif





