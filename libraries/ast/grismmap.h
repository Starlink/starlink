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
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public Licence as
*     published by the Free Software Foundation; either version 2 of
*     the Licence, or (at your option) any later version.
*     
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public Licence for more details.
*     
*     You should have received a copy of the GNU General Public Licence
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

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
   double m;     
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

/* Unique flag value to determine class membership. */
   int *check;                   /* Check value */

/* Properties (e.g. methods) specific to this class. */
   double (* GetGrismNR)( AstGrismMap * );
   int (* TestGrismNR)( AstGrismMap * );
   void (* ClearGrismNR)( AstGrismMap * );
   void (* SetGrismNR)( AstGrismMap *, double );

   double (* GetGrismNRP)( AstGrismMap * );
   int (* TestGrismNRP)( AstGrismMap * );
   void (* ClearGrismNRP)( AstGrismMap * );
   void (* SetGrismNRP)( AstGrismMap *, double );

   double (* GetGrismWaveR)( AstGrismMap * );
   int (* TestGrismWaveR)( AstGrismMap * );
   void (* ClearGrismWaveR)( AstGrismMap * );
   void (* SetGrismWaveR)( AstGrismMap *, double );

   double (* GetGrismAlpha)( AstGrismMap * );
   int (* TestGrismAlpha)( AstGrismMap * );
   void (* ClearGrismAlpha)( AstGrismMap * );
   void (* SetGrismAlpha)( AstGrismMap *, double );

   double (* GetGrismG)( AstGrismMap * );
   int (* TestGrismG)( AstGrismMap * );
   void (* ClearGrismG)( AstGrismMap * );
   void (* SetGrismG)( AstGrismMap *, double );

   int (* GetGrismM)( AstGrismMap * );
   int (* TestGrismM)( AstGrismMap * );
   void (* ClearGrismM)( AstGrismMap * );
   void (* SetGrismM)( AstGrismMap *, int );

   double (* GetGrismEps)( AstGrismMap * );
   int (* TestGrismEps)( AstGrismMap * );
   void (* ClearGrismEps)( AstGrismMap * );
   void (* SetGrismEps)( AstGrismMap *, double );

   double (* GetGrismTheta)( AstGrismMap * );
   int (* TestGrismTheta)( AstGrismMap * );
   void (* ClearGrismTheta)( AstGrismMap * );
   void (* SetGrismTheta)( AstGrismMap *, double );

} AstGrismMapVtab;
#endif

/* Function prototypes. */
/* ==================== */
/* Prototypes for standard class functions. */
/* ---------------------------------------- */
astPROTO_CHECK(GrismMap)          /* Check class membership */
astPROTO_ISA(GrismMap)            /* Test class membership */

/* Constructor. */
#if defined(astCLASS)            /* Protected. */
AstGrismMap *astGrismMap_( const char *, ... );
#else
AstGrismMap *astGrismMapId_( const char *, ... );
#endif

#if defined(astCLASS)            /* Protected */

/* Initialiser. */
AstGrismMap *astInitGrismMap_( void *, size_t, int, AstGrismMapVtab *,
                             const char * );

/* Vtab initialiser. */
void astInitGrismMapVtab_( AstGrismMapVtab *, const char * );

/* Loader. */
AstGrismMap *astLoadGrismMap_( void *, size_t, AstGrismMapVtab *,
                             const char *, AstChannel * );
#endif

/* Prototypes for member functions. */
/* -------------------------------- */
# if defined(astCLASS)           /* Protected */

   double astGetGrismNR_( AstGrismMap * );
   int astTestGrismNR_( AstGrismMap * );
   void astClearGrismNR_( AstGrismMap * );
   void astSetGrismNR_( AstGrismMap *, double );

   double astGetGrismNRP_( AstGrismMap * );
   int astTestGrismNRP_( AstGrismMap * );
   void astClearGrismNRP_( AstGrismMap * );
   void astSetGrismNRP_( AstGrismMap *, double );

   double astGetGrismWaveR_( AstGrismMap * );
   int astTestGrismWaveR_( AstGrismMap * );
   void astClearGrismWaveR_( AstGrismMap * );
   void astSetGrismWaveR_( AstGrismMap *, double );

   double astGetGrismAlpha_( AstGrismMap * );
   int astTestGrismAlpha_( AstGrismMap * );
   void astClearGrismAlpha_( AstGrismMap * );
   void astSetGrismAlpha_( AstGrismMap *, double );

   double astGetGrismG_( AstGrismMap * );
   int astTestGrismG_( AstGrismMap * );
   void astClearGrismG_( AstGrismMap * );
   void astSetGrismG_( AstGrismMap *, double );

   int astGetGrismM_( AstGrismMap * );
   int astTestGrismM_( AstGrismMap * );
   void astClearGrismM_( AstGrismMap * );
   void astSetGrismM_( AstGrismMap *, int );

   double astGetGrismEps_( AstGrismMap * );
   int astTestGrismEps_( AstGrismMap * );
   void astClearGrismEps_( AstGrismMap * );
   void astSetGrismEps_( AstGrismMap *, double );

   double astGetGrismTheta_( AstGrismMap * );
   int astTestGrismTheta_( AstGrismMap * );
   void astClearGrismTheta_( AstGrismMap * );
   void astSetGrismTheta_( AstGrismMap *, double );

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
#define astCheckGrismMap(this) astINVOKE_CHECK(GrismMap,this)

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
astINVOKE(O,astInitGrismMap_(mem,size,init,vtab,name))

/* Vtab Initialiser. */
#define astInitGrismMapVtab(vtab,name) astINVOKE(V,astInitGrismMapVtab_(vtab,name))
/* Loader. */
#define astLoadGrismMap(mem,size,vtab,name,channel) \
astINVOKE(O,astLoadGrismMap_(mem,size,vtab,name,astCheckChannel(channel)))
#endif

/* Interfaces to public member functions. */
/* -------------------------------------- */
/* Here we make use of astCheckGrismMap to validate GrismMap pointers
   before use.  This provides a contextual error report if a pointer
   to the wrong sort of Object is supplied. */

#if defined(astCLASS)            /* Protected */

#define astGetGrismNR(this) astINVOKE(V,astGetGrismNR_(astCheckGrismMap(this)))
#define astTestGrismNR(this) astINVOKE(V,astTestGrismNR_(astCheckGrismMap(this)))
#define astClearGrismNR(this) astINVOKE(V,astClearGrismNR_(astCheckGrismMap(this)))
#define astSetGrismNR(this,value) astINVOKE(V,astSetGrismNR_(astCheckGrismMap(this),value))

#define astGetGrismNRP(this) astINVOKE(V,astGetGrismNRP_(astCheckGrismMap(this)))
#define astTestGrismNRP(this) astINVOKE(V,astTestGrismNRP_(astCheckGrismMap(this)))
#define astClearGrismNRP(this) astINVOKE(V,astClearGrismNRP_(astCheckGrismMap(this)))
#define astSetGrismNRP(this,value) astINVOKE(V,astSetGrismNRP_(astCheckGrismMap(this),value))

#define astGetGrismWaveR(this) astINVOKE(V,astGetGrismWaveR_(astCheckGrismMap(this)))
#define astTestGrismWaveR(this) astINVOKE(V,astTestGrismWaveR_(astCheckGrismMap(this)))
#define astClearGrismWaveR(this) astINVOKE(V,astClearGrismWaveR_(astCheckGrismMap(this)))
#define astSetGrismWaveR(this,value) astINVOKE(V,astSetGrismWaveR_(astCheckGrismMap(this),value))

#define astGetGrismAlpha(this) astINVOKE(V,astGetGrismAlpha_(astCheckGrismMap(this)))
#define astTestGrismAlpha(this) astINVOKE(V,astTestGrismAlpha_(astCheckGrismMap(this)))
#define astClearGrismAlpha(this) astINVOKE(V,astClearGrismAlpha_(astCheckGrismMap(this)))
#define astSetGrismAlpha(this,value) astINVOKE(V,astSetGrismAlpha_(astCheckGrismMap(this),value))

#define astGetGrismG(this) astINVOKE(V,astGetGrismG_(astCheckGrismMap(this)))
#define astTestGrismG(this) astINVOKE(V,astTestGrismG_(astCheckGrismMap(this)))
#define astClearGrismG(this) astINVOKE(V,astClearGrismG_(astCheckGrismMap(this)))
#define astSetGrismG(this,value) astINVOKE(V,astSetGrismG_(astCheckGrismMap(this),value))

#define astGetGrismM(this) astINVOKE(V,astGetGrismM_(astCheckGrismMap(this)))
#define astTestGrismM(this) astINVOKE(V,astTestGrismM_(astCheckGrismMap(this)))
#define astClearGrismM(this) astINVOKE(V,astClearGrismM_(astCheckGrismMap(this)))
#define astSetGrismM(this,value) astINVOKE(V,astSetGrismM_(astCheckGrismMap(this),value))

#define astGetGrismEps(this) astINVOKE(V,astGetGrismEps_(astCheckGrismMap(this)))
#define astTestGrismEps(this) astINVOKE(V,astTestGrismEps_(astCheckGrismMap(this)))
#define astClearGrismEps(this) astINVOKE(V,astClearGrismEps_(astCheckGrismMap(this)))
#define astSetGrismEps(this,value) astINVOKE(V,astSetGrismEps_(astCheckGrismMap(this),value))

#define astGetGrismTheta(this) astINVOKE(V,astGetGrismTheta_(astCheckGrismMap(this)))
#define astTestGrismTheta(this) astINVOKE(V,astTestGrismTheta_(astCheckGrismMap(this)))
#define astClearGrismTheta(this) astINVOKE(V,astClearGrismTheta_(astCheckGrismMap(this)))
#define astSetGrismTheta(this,value) astINVOKE(V,astSetGrismTheta_(astCheckGrismMap(this),value))


#endif
#endif
