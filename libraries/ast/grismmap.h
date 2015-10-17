#if !defined( GRISMMAP_INCLUDED ) /* Include this file only once */
#define GRISMMAP_INCLUDED
/*
*+
*  Name:
*     grismmap.h

*  Type:
*     C include file.

*  Purpose:
*     Define the interface to the GrismMap class.

*  Invocation:
*     #include "grismmap.h"

*  Description:
*     This include file defines the interface to the GrismMap class and
*     provides the type definitions, function prototypes and macros,
*     etc.  needed to use this class.
*
*     The GrismMap class implements Mappings which perform a "zoom"
*     transformation by multiplying all coordinate values by the same
*     scale factor (the inverse transformation is performed by
*     dividing by this scale factor).

*  Inheritance:
*     The GrismMap class inherits from the Mapping class.

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
*     8-JUL-2003 (DSB):
*        Initial version.
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
/* GrismMap structure. */
/* ------------------ */
/* This structure contains all information that is unique to each object in
   the class (e.g. its instance variables). */
typedef struct AstGrismMap {

/* Attributes inherited from the parent class. */
   AstMapping mapping;           /* Parent class structure */

/* Attributes specific to objects in this class. */
   double nr;
   double nrp;
   double waver;
   double alpha;
   double g;
   int m;
   double eps;
   double theta;
   double k1;
   double k2;
   double k3;

} AstGrismMap;

/* Virtual function table. */
/* ----------------------- */
/* This table contains all information that is the same for all
   objects in the class (e.g. pointers to its virtual functions). */
#if defined(astCLASS)            /* Protected */
typedef struct AstGrismMapVtab {

/* Properties (e.g. methods) inherited from the parent class. */
   AstMappingVtab mapping_vtab;  /* Parent class virtual function table */

/* A Unique identifier to determine class membership. */
   AstClassIdentifier id;

/* Properties (e.g. methods) specific to this class. */
   double (* GetGrismNR)( AstGrismMap *, int * );
   int (* TestGrismNR)( AstGrismMap *, int * );
   void (* ClearGrismNR)( AstGrismMap *, int * );
   void (* SetGrismNR)( AstGrismMap *, double, int * );

   double (* GetGrismNRP)( AstGrismMap *, int * );
   int (* TestGrismNRP)( AstGrismMap *, int * );
   void (* ClearGrismNRP)( AstGrismMap *, int * );
   void (* SetGrismNRP)( AstGrismMap *, double, int * );

   double (* GetGrismWaveR)( AstGrismMap *, int * );
   int (* TestGrismWaveR)( AstGrismMap *, int * );
   void (* ClearGrismWaveR)( AstGrismMap *, int * );
   void (* SetGrismWaveR)( AstGrismMap *, double, int * );

   double (* GetGrismAlpha)( AstGrismMap *, int * );
   int (* TestGrismAlpha)( AstGrismMap *, int * );
   void (* ClearGrismAlpha)( AstGrismMap *, int * );
   void (* SetGrismAlpha)( AstGrismMap *, double, int * );

   double (* GetGrismG)( AstGrismMap *, int * );
   int (* TestGrismG)( AstGrismMap *, int * );
   void (* ClearGrismG)( AstGrismMap *, int * );
   void (* SetGrismG)( AstGrismMap *, double, int * );

   int (* GetGrismM)( AstGrismMap *, int * );
   int (* TestGrismM)( AstGrismMap *, int * );
   void (* ClearGrismM)( AstGrismMap *, int * );
   void (* SetGrismM)( AstGrismMap *, int, int * );

   double (* GetGrismEps)( AstGrismMap *, int * );
   int (* TestGrismEps)( AstGrismMap *, int * );
   void (* ClearGrismEps)( AstGrismMap *, int * );
   void (* SetGrismEps)( AstGrismMap *, double, int * );

   double (* GetGrismTheta)( AstGrismMap *, int * );
   int (* TestGrismTheta)( AstGrismMap *, int * );
   void (* ClearGrismTheta)( AstGrismMap *, int * );
   void (* SetGrismTheta)( AstGrismMap *, double, int * );

} AstGrismMapVtab;

#if defined(THREAD_SAFE)

/* Define a structure holding all data items that are global within this
   class. */
typedef struct AstGrismMapGlobals {
   AstGrismMapVtab Class_Vtab;
   int Class_Init;
   char GetAttrib_Buff[ 101 ];
} AstGrismMapGlobals;

#endif
#endif

/* Function prototypes. */
/* ==================== */
/* Prototypes for standard class functions. */
/* ---------------------------------------- */
astPROTO_CHECK(GrismMap)          /* Check class membership */
astPROTO_ISA(GrismMap)            /* Test class membership */

/* Constructor. */
#if defined(astCLASS)            /* Protected. */
AstGrismMap *astGrismMap_( const char *, int *, ...);
#else
AstGrismMap *astGrismMapId_( const char *, ... )__attribute__((format(printf,1,2)));
#endif

#if defined(astCLASS)            /* Protected */

/* Initialiser. */
AstGrismMap *astInitGrismMap_( void *, size_t, int, AstGrismMapVtab *,
                             const char *, int * );

/* Vtab initialiser. */
void astInitGrismMapVtab_( AstGrismMapVtab *, const char *, int * );

/* Loader. */
AstGrismMap *astLoadGrismMap_( void *, size_t, AstGrismMapVtab *,
                             const char *, AstChannel *, int * );

/* Thread-safe initialiser for all global data used by this module. */
#if defined(THREAD_SAFE)
void astInitGrismMapGlobals_( AstGrismMapGlobals * );
#endif

#endif

/* Prototypes for member functions. */
/* -------------------------------- */
# if defined(astCLASS)           /* Protected */

   double astGetGrismNR_( AstGrismMap *, int * );
   int astTestGrismNR_( AstGrismMap *, int * );
   void astClearGrismNR_( AstGrismMap *, int * );
   void astSetGrismNR_( AstGrismMap *, double, int * );

   double astGetGrismNRP_( AstGrismMap *, int * );
   int astTestGrismNRP_( AstGrismMap *, int * );
   void astClearGrismNRP_( AstGrismMap *, int * );
   void astSetGrismNRP_( AstGrismMap *, double, int * );

   double astGetGrismWaveR_( AstGrismMap *, int * );
   int astTestGrismWaveR_( AstGrismMap *, int * );
   void astClearGrismWaveR_( AstGrismMap *, int * );
   void astSetGrismWaveR_( AstGrismMap *, double, int * );

   double astGetGrismAlpha_( AstGrismMap *, int * );
   int astTestGrismAlpha_( AstGrismMap *, int * );
   void astClearGrismAlpha_( AstGrismMap *, int * );
   void astSetGrismAlpha_( AstGrismMap *, double, int * );

   double astGetGrismG_( AstGrismMap *, int * );
   int astTestGrismG_( AstGrismMap *, int * );
   void astClearGrismG_( AstGrismMap *, int * );
   void astSetGrismG_( AstGrismMap *, double, int * );

   int astGetGrismM_( AstGrismMap *, int * );
   int astTestGrismM_( AstGrismMap *, int * );
   void astClearGrismM_( AstGrismMap *, int * );
   void astSetGrismM_( AstGrismMap *, int, int * );

   double astGetGrismEps_( AstGrismMap *, int * );
   int astTestGrismEps_( AstGrismMap *, int * );
   void astClearGrismEps_( AstGrismMap *, int * );
   void astSetGrismEps_( AstGrismMap *, double, int * );

   double astGetGrismTheta_( AstGrismMap *, int * );
   int astTestGrismTheta_( AstGrismMap *, int * );
   void astClearGrismTheta_( AstGrismMap *, int * );
   void astSetGrismTheta_( AstGrismMap *, double, int * );

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
#define astCheckGrismMap(this) astINVOKE_CHECK(GrismMap,this,0)
#define astVerifyGrismMap(this) astINVOKE_CHECK(GrismMap,this,1)

/* Test class membership. */
#define astIsAGrismMap(this) astINVOKE_ISA(GrismMap,this)

/* Constructor. */
#if defined(astCLASS)            /* Protected. */
#define astGrismMap astINVOKE(F,astGrismMap_)
#else
#define astGrismMap astINVOKE(F,astGrismMapId_)
#endif

#if defined(astCLASS)            /* Protected */

/* Initialiser. */
#define astInitGrismMap(mem,size,init,vtab,name) \
astINVOKE(O,astInitGrismMap_(mem,size,init,vtab,name,STATUS_PTR))

/* Vtab Initialiser. */
#define astInitGrismMapVtab(vtab,name) astINVOKE(V,astInitGrismMapVtab_(vtab,name,STATUS_PTR))
/* Loader. */
#define astLoadGrismMap(mem,size,vtab,name,channel) \
astINVOKE(O,astLoadGrismMap_(mem,size,vtab,name,astCheckChannel(channel),STATUS_PTR))
#endif

/* Interfaces to public member functions. */
/* -------------------------------------- */
/* Here we make use of astCheckGrismMap to validate GrismMap pointers
   before use.  This provides a contextual error report if a pointer
   to the wrong sort of Object is supplied. */

#if defined(astCLASS)            /* Protected */

#define astGetGrismNR(this) astINVOKE(V,astGetGrismNR_(astCheckGrismMap(this),STATUS_PTR))
#define astTestGrismNR(this) astINVOKE(V,astTestGrismNR_(astCheckGrismMap(this),STATUS_PTR))
#define astClearGrismNR(this) astINVOKE(V,astClearGrismNR_(astCheckGrismMap(this),STATUS_PTR))
#define astSetGrismNR(this,value) astINVOKE(V,astSetGrismNR_(astCheckGrismMap(this),value,STATUS_PTR))

#define astGetGrismNRP(this) astINVOKE(V,astGetGrismNRP_(astCheckGrismMap(this),STATUS_PTR))
#define astTestGrismNRP(this) astINVOKE(V,astTestGrismNRP_(astCheckGrismMap(this),STATUS_PTR))
#define astClearGrismNRP(this) astINVOKE(V,astClearGrismNRP_(astCheckGrismMap(this),STATUS_PTR))
#define astSetGrismNRP(this,value) astINVOKE(V,astSetGrismNRP_(astCheckGrismMap(this),value,STATUS_PTR))

#define astGetGrismWaveR(this) astINVOKE(V,astGetGrismWaveR_(astCheckGrismMap(this),STATUS_PTR))
#define astTestGrismWaveR(this) astINVOKE(V,astTestGrismWaveR_(astCheckGrismMap(this),STATUS_PTR))
#define astClearGrismWaveR(this) astINVOKE(V,astClearGrismWaveR_(astCheckGrismMap(this),STATUS_PTR))
#define astSetGrismWaveR(this,value) astINVOKE(V,astSetGrismWaveR_(astCheckGrismMap(this),value,STATUS_PTR))

#define astGetGrismAlpha(this) astINVOKE(V,astGetGrismAlpha_(astCheckGrismMap(this),STATUS_PTR))
#define astTestGrismAlpha(this) astINVOKE(V,astTestGrismAlpha_(astCheckGrismMap(this),STATUS_PTR))
#define astClearGrismAlpha(this) astINVOKE(V,astClearGrismAlpha_(astCheckGrismMap(this),STATUS_PTR))
#define astSetGrismAlpha(this,value) astINVOKE(V,astSetGrismAlpha_(astCheckGrismMap(this),value,STATUS_PTR))

#define astGetGrismG(this) astINVOKE(V,astGetGrismG_(astCheckGrismMap(this),STATUS_PTR))
#define astTestGrismG(this) astINVOKE(V,astTestGrismG_(astCheckGrismMap(this),STATUS_PTR))
#define astClearGrismG(this) astINVOKE(V,astClearGrismG_(astCheckGrismMap(this),STATUS_PTR))
#define astSetGrismG(this,value) astINVOKE(V,astSetGrismG_(astCheckGrismMap(this),value,STATUS_PTR))

#define astGetGrismM(this) astINVOKE(V,astGetGrismM_(astCheckGrismMap(this),STATUS_PTR))
#define astTestGrismM(this) astINVOKE(V,astTestGrismM_(astCheckGrismMap(this),STATUS_PTR))
#define astClearGrismM(this) astINVOKE(V,astClearGrismM_(astCheckGrismMap(this),STATUS_PTR))
#define astSetGrismM(this,value) astINVOKE(V,astSetGrismM_(astCheckGrismMap(this),value,STATUS_PTR))

#define astGetGrismEps(this) astINVOKE(V,astGetGrismEps_(astCheckGrismMap(this),STATUS_PTR))
#define astTestGrismEps(this) astINVOKE(V,astTestGrismEps_(astCheckGrismMap(this),STATUS_PTR))
#define astClearGrismEps(this) astINVOKE(V,astClearGrismEps_(astCheckGrismMap(this),STATUS_PTR))
#define astSetGrismEps(this,value) astINVOKE(V,astSetGrismEps_(astCheckGrismMap(this),value,STATUS_PTR))

#define astGetGrismTheta(this) astINVOKE(V,astGetGrismTheta_(astCheckGrismMap(this),STATUS_PTR))
#define astTestGrismTheta(this) astINVOKE(V,astTestGrismTheta_(astCheckGrismMap(this),STATUS_PTR))
#define astClearGrismTheta(this) astINVOKE(V,astClearGrismTheta_(astCheckGrismMap(this),STATUS_PTR))
#define astSetGrismTheta(this,value) astINVOKE(V,astSetGrismTheta_(astCheckGrismMap(this),value,STATUS_PTR))


#endif
#endif





