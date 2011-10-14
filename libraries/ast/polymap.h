#if !defined( POLYMAP_INCLUDED ) /* Include this file only once */
#define POLYMAP_INCLUDED
/*
*+
*  Name:
*     polymap.h

*  Type:
*     C include file.

*  Purpose:
*     Define the interface to the PolyMap class.

*  Invocation:
*     #include "polymap.h"

*  Description:
*     This include file defines the interface to the PolyMap class and
*     provides the type definitions, function prototypes and macros,
*     etc.  needed to use this class.
*
*     A PolyMap is a form of Mapping which performs a general polynomial
*     transformation.  Each output coordinate is a polynomial function of
*     all the input coordinates. The coefficients are specified separately
*     for each output coordinate. The forward and inverse transformations
*     are defined independantly by separate sets of coefficients.

*  Inheritance:
*     The PolyMap class inherits from the Mapping class.

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
*           Apply a PolyMap to transform a set of points.

*  New Methods Defined:
*     Public:
*        None.
*
*     Protected:
*        None.

*  Other Class Functions:
*     Public:
*        astIsAPolyMap
*           Test class membership.
*        astPolyMap
*           Create a PolyMap.
*
*     Protected:
*        astCheckPolyMap
*           Validate class membership.
*        astInitPolyMap
*           Initialise a PolyMap.
*        astInitPolyMapVtab
*           Initialise the virtual function table for the PolyMap class.
*        astLoadPolyMap
*           Load a PolyMap.

*  Macros:
*     None.

*  Type Definitions:
*     Public:
*        AstPolyMap
*           PolyMap object type.
*
*     Protected:
*        AstPolyMapVtab
*           PolyMap virtual function table type.

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
*     DSB: D.S. Berry (Starlink)

*  History:
*     28-SEP-2003 (DSB):
*        Original version.
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
/* PolyMap structure. */
/* ------------------ */
/* This structure contains all information that is unique to each object in
   the class (e.g. its instance variables). */
typedef struct AstPolyMap {

/* Attributes inherited from the parent class. */
   AstMapping mapping;           /* Parent class structure */

/* Attributes specific to objects in this class. */
   int *ncoeff_f;             /* No. of coeffs for each forward polynomial */
   int *mxpow_f;              /* Max power of each i/p axis for each forward polynomial */
   int ***power_f;            /* Pointer to i/p powers for all forward coefficients */
   double **coeff_f;          /* Pointer to values of all forward coefficients */
   int *ncoeff_i;             /* No. of coeffs for each inverse polynomial */
   int *mxpow_i;              /* Max power of each i/p axis for each inverse polynomial */
   int ***power_i;            /* Pointer to i/p powers for all inverse coefficients */
   double **coeff_i;          /* Pointer to values of all inverse coefficients */
   int iterinverse;           /* Use an iterative inverse? */
   int niterinverse;          /* Max number of iterations for iterative inverse */
   double tolinverse;         /* Target relative error for iterative inverse */
   struct AstPolyMap **jacobian;/* PolyMaps defining Jacobian of forward transformation */
} AstPolyMap;

/* Virtual function table. */
/* ----------------------- */
/* This table contains all information that is the same for all
   objects in the class (e.g. pointers to its virtual functions). */
#if defined(astCLASS)            /* Protected */
typedef struct AstPolyMapVtab {

/* Properties (e.g. methods) inherited from the parent class. */
   AstMappingVtab mapping_vtab;  /* Parent class virtual function table */

/* A Unique identifier to determine class membership. */
   AstClassIdentifier id;

/* Properties (e.g. methods) specific to this class. */
   AstPolyMap *(* PolyTran)( AstPolyMap *, int, double, double, int, const double *, const double *, int * );
   int (*GetIterInverse)( AstPolyMap *, int * );
   int (* TestIterInverse)( AstPolyMap *, int * );
   void (* ClearIterInverse)( AstPolyMap *, int * );
   void (* SetIterInverse)( AstPolyMap *, int, int * );

   int (*GetNiterInverse)( AstPolyMap *, int * );
   int (* TestNiterInverse)( AstPolyMap *, int * );
   void (* ClearNiterInverse)( AstPolyMap *, int * );
   void (* SetNiterInverse)( AstPolyMap *, int, int * );

   double (*GetTolInverse)( AstPolyMap *, int * );
   int (* TestTolInverse)( AstPolyMap *, int * );
   void (* ClearTolInverse)( AstPolyMap *, int * );
   void (* SetTolInverse)( AstPolyMap *, double, int * );

} AstPolyMapVtab;

#if defined(THREAD_SAFE)

/* Define a structure holding all data items that are global within the
   object.c file. */

typedef struct AstPolyMapGlobals {
   AstPolyMapVtab Class_Vtab;
   int Class_Init;
   char GetAttrib_Buff[ AST__GETATTRIB_BUFF_LEN + 1 ];
} AstPolyMapGlobals;


/* Thread-safe initialiser for all global data used by this module. */
void astInitPolyMapGlobals_( AstPolyMapGlobals * );

#endif


#endif

/* Function prototypes. */
/* ==================== */
/* Prototypes for standard class functions. */
/* ---------------------------------------- */
astPROTO_CHECK(PolyMap)          /* Check class membership */
astPROTO_ISA(PolyMap)            /* Test class membership */

/* Constructor. */
#if defined(astCLASS)            /* Protected. */
AstPolyMap *astPolyMap_( int, int, int, const double[], int, const double[], const char *, int *, ...);
#else
AstPolyMap *astPolyMapId_( int, int, int, const double[], int, const double[], const char *, ... )__attribute__((format(printf,7,8)));
#endif

#if defined(astCLASS)            /* Protected */

/* Initialiser. */
AstPolyMap *astInitPolyMap_( void *, size_t, int, AstPolyMapVtab *, const char *, int, int, int, const double[], int, const double[], int * );

/* Vtab initialiser. */
void astInitPolyMapVtab_( AstPolyMapVtab *, const char *, int * );

/* Loader. */
AstPolyMap *astLoadPolyMap_( void *, size_t, AstPolyMapVtab *,
                                 const char *, AstChannel *, int * );
#endif

/* Prototypes for member functions. */
/* -------------------------------- */
AstPolyMap *astPolyTran_( AstPolyMap *, int, double, double, int, const double *, const double *, int * );

# if defined(astCLASS)           /* Protected */
   int astGetIterInverse_( AstPolyMap *, int * );
   int astTestIterInverse_( AstPolyMap *, int * );
   void astClearIterInverse_( AstPolyMap *, int * );
   void astSetIterInverse_( AstPolyMap *, int, int * );

   int astGetNiterInverse_( AstPolyMap *, int * );
   int astTestNiterInverse_( AstPolyMap *, int * );
   void astClearNiterInverse_( AstPolyMap *, int * );
   void astSetNiterInverse_( AstPolyMap *, int, int * );

   double astGetTolInverse_( AstPolyMap *, int * );
   int astTestTolInverse_( AstPolyMap *, int * );
   void astClearTolInverse_( AstPolyMap *, int * );
   void astSetTolInverse_( AstPolyMap *, double, int * );
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
#define astCheckPolyMap(this) astINVOKE_CHECK(PolyMap,this,0)
#define astVerifyPolyMap(this) astINVOKE_CHECK(PolyMap,this,1)

/* Test class membership. */
#define astIsAPolyMap(this) astINVOKE_ISA(PolyMap,this)

/* Constructor. */
#if defined(astCLASS)            /* Protected. */
#define astPolyMap astINVOKE(F,astPolyMap_)
#else
#define astPolyMap astINVOKE(F,astPolyMapId_)
#endif

#if defined(astCLASS)            /* Protected */

/* Initialiser. */
#define astInitPolyMap(mem,size,init,vtab,name,nin,nout,ncoeff_f,coeff_f,ncoeff_i,coeff_i) \
astINVOKE(O,astInitPolyMap_(mem,size,init,vtab,name,nin,nout,ncoeff_f,coeff_f,ncoeff_i,coeff_i,STATUS_PTR))

/* Vtab Initialiser. */
#define astInitPolyMapVtab(vtab,name) astINVOKE(V,astInitPolyMapVtab_(vtab,name,STATUS_PTR))
/* Loader. */
#define astLoadPolyMap(mem,size,vtab,name,channel) \
astINVOKE(O,astLoadPolyMap_(mem,size,vtab,name,astCheckChannel(channel),STATUS_PTR))
#endif

/* Interfaces to public member functions. */
/* -------------------------------------- */
/* Here we make use of astCheckPolyMap to validate PolyMap pointers
   before use.  This provides a contextual error report if a pointer
   to the wrong sort of Object is supplied. */

#define astPolyTran(this,forward,acc,maxacc,maxorder,lbnd,ubnd) \
astINVOKE(O,astPolyTran_(astCheckPolyMap(this),forward,acc,maxacc,maxorder,lbnd,ubnd,STATUS_PTR))

#if defined(astCLASS)            /* Protected */

#define astClearIterInverse(this) \
        astINVOKE(V,astClearIterInverse_(astCheckPolyMap(this),STATUS_PTR))
#define astGetIterInverse(this) \
        astINVOKE(V,astGetIterInverse_(astCheckPolyMap(this),STATUS_PTR))
#define astSetIterInverse(this,value) \
        astINVOKE(V,astSetIterInverse_(astCheckPolyMap(this),value,STATUS_PTR))
#define astTestIterInverse(this) \
        astINVOKE(V,astTestIterInverse_(astCheckPolyMap(this),STATUS_PTR))

#define astClearNiterInverse(this) \
        astINVOKE(V,astClearNiterInverse_(astCheckPolyMap(this),STATUS_PTR))
#define astGetNiterInverse(this) \
        astINVOKE(V,astGetNiterInverse_(astCheckPolyMap(this),STATUS_PTR))
#define astSetNiterInverse(this,value) \
        astINVOKE(V,astSetNiterInverse_(astCheckPolyMap(this),value,STATUS_PTR))
#define astTestNiterInverse(this) \
        astINVOKE(V,astTestNiterInverse_(astCheckPolyMap(this),STATUS_PTR))

#define astClearTolInverse(this) \
        astINVOKE(V,astClearTolInverse_(astCheckPolyMap(this),STATUS_PTR))
#define astGetTolInverse(this) \
        astINVOKE(V,astGetTolInverse_(astCheckPolyMap(this),STATUS_PTR))
#define astSetTolInverse(this,value) \
        astINVOKE(V,astSetTolInverse_(astCheckPolyMap(this),value,STATUS_PTR))
#define astTestTolInverse(this) \
        astINVOKE(V,astTestTolInverse_(astCheckPolyMap(this),STATUS_PTR))

#endif
#endif





