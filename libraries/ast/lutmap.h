#if !defined( LUTMAP_INCLUDED )  /* Include this file only once */
#define LUTMAP_INCLUDED
/*
*+
*  Name:
*     lutmap.h

*  Type:
*     C include file.

*  Purpose:
*     Define the interface to the LutMap class.

*  Invocation:
*     #include "lutmap.h"

*  Description:
*     This include file defines the interface to the LutMap class and
*     provides the type definitions, function prototypes and macros,
*     etc.  needed to use this class.
*
*     The LutMap class implements Mappings which transform
*     1-dimensional coordinates using linear interpolation in a lookup
*     table.

*  Inheritance:
*     The LutMap class inherits from the Mapping class.

*  Attributes Over-Ridden:
*     None.

*  New Attributes Defined:
*     None.

*  Methods Over-Ridden:
*     Public:
*        None.
*
*     Protected:
*        astMapMerge
*           Simplify a sequence of Mappings.
*        astTransform
*           Apply a LutMap to transform a set of points.

*  New Methods Defined:
*     Public:
*        None.
*
*     Protected:
*        None.

*  Other Class Functions:
*     Public:
*        astIsALutMap
*           Test class membership.
*        astLutMap
*           Create a LutMap.
*
*     Protected:
*        astCheckLutMap
*           Validate class membership.
*        astInitLutMap
*           Initialise a LutMap.
*        astInitLutMapVtab
*           Initialise the virtual function table for the LutMap class.
*        astLoadLutMap
*           Load a LutMap.

*  Macros:
*     None.

*  Type Definitions:
*     Public:
*        AstLutMap
*           LutMap object type.
*
*     Protected:
*        AstLutMapVtab
*           LutMap virtual function table type.

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
*     8-JUL-1997 (RFWS):
*        Original version.
*     8-JAN-2003 (DSB):
*        Added protected astInitLutMapVtab method.
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
/* LutMap structure. */
/* ------------------ */
/* This structure contains all information that is unique to each
   object in the class (e.g. its instance variables). */
typedef struct AstLutMap {

/* Attributes inherited from the parent class. */
   AstMapping mapping;           /* Parent class structure */

/* Attributes specific to objects in this class. */
   double *lut;                 /* Pointer to lookup table */
   double *luti;                /* Reduced lookup table for inverse trans. */
   double inc;                  /* Input increment between table entries */
   double last_fwd_in;          /* Last input value (forward transfm.) */
   double last_fwd_out;         /* Last output value (forward transfm.) */
   double last_inv_in;          /* Last input value (inverse transfm.) */
   double last_inv_out;         /* Last output value (inverse transfm.) */
   double start;                /* Input value for first table entry */
   int *flagsi;                 /* Flags indicating adjacent bad values */
   int *indexi;                 /* Translates reduced to original indices */
   double lutepsilon;           /* Relative error of table values */
   int lutinterp;               /* Interpolation method */
   int nlut;                    /* Number of table entries */
   int nluti;                   /* Reduced number of table entries */
} AstLutMap;

/* Virtual function table. */
/* ----------------------- */
/* This table contains all information that is the same for all
   objects in the class (e.g. pointers to its virtual functions). */
#if defined(astCLASS)            /* Protected */
typedef struct AstLutMapVtab {

/* Properties (e.g. methods) inherited from the parent class. */
   AstMappingVtab mapping_vtab;  /* Parent class virtual function table */

/* A Unique identifier to determine class membership. */
   AstClassIdentifier id;

/* Properties (e.g. methods) specific to this class. */
   int (*GetLutInterp)( AstLutMap *, int * );
   int (* TestLutInterp)( AstLutMap *, int * );
   void (* ClearLutInterp)( AstLutMap *, int * );
   void (* SetLutInterp)( AstLutMap *, int, int * );
   double (*GetLutEpsilon)( AstLutMap *, int * );
   int (* TestLutEpsilon)( AstLutMap *, int * );
   void (* ClearLutEpsilon)( AstLutMap *, int * );
   void (* SetLutEpsilon)( AstLutMap *, double, int * );
   double *(* GetLutMapInfo)( AstLutMap *, double *, double *, int *, int * );

} AstLutMapVtab;

#if defined(THREAD_SAFE)

/* Define a structure holding all data items that are global within this
   class. */
typedef struct AstLutMapGlobals {
   AstLutMapVtab Class_Vtab;
   int Class_Init;
   char GetAttrib_Buff[ 101 ];
} AstLutMapGlobals;

#endif

#endif

/* Function prototypes. */
/* ==================== */
/* Prototypes for standard class functions. */
/* ---------------------------------------- */
astPROTO_CHECK(LutMap)          /* Check class membership */
astPROTO_ISA(LutMap)            /* Test class membership */

/* Constructor. */
#if defined(astCLASS)            /* Protected. */
AstLutMap *astLutMap_( int, const double [], double, double, const char *, int *, ...);
#else
AstLutMap *astLutMapId_( int, const double [], double, double, const char *, ... )__attribute__((format(printf,5,6)));
#endif

#if defined(astCLASS)            /* Protected */

/* Initialiser. */
AstLutMap *astInitLutMap_( void *, size_t, int, AstLutMapVtab *, const char *, int, const double *, double, double, int * );

/* Vtab initialiser. */
void astInitLutMapVtab_( AstLutMapVtab *, const char *, int * );

/* Loader. */
AstLutMap *astLoadLutMap_( void *, size_t, AstLutMapVtab *, const char *, AstChannel *, int * );

/* Thread-safe initialiser for all global data used by this module. */
#if defined(THREAD_SAFE)
void astInitLutMapGlobals_( AstLutMapGlobals * );
#endif

#endif

/* Prototypes for member functions. */
/* -------------------------------- */
# if defined(astCLASS)           /* Protected */
   int astGetLutInterp_( AstLutMap *, int * );
   int astTestLutInterp_( AstLutMap *, int * );
   void astClearLutInterp_( AstLutMap *, int * );
   void astSetLutInterp_( AstLutMap *, int, int * );

   double astGetLutEpsilon_( AstLutMap *, int * );
   int astTestLutEpsilon_( AstLutMap *, int * );
   void astClearLutEpsilon_( AstLutMap *, int * );
   void astSetLutEpsilon_( AstLutMap *, double, int * );

   double *astGetLutMapInfo_( AstLutMap *, double *, double *, int *, int * );
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
#define astCheckLutMap(this) astINVOKE_CHECK(LutMap,this,0)
#define astVerifyLutMap(this) astINVOKE_CHECK(LutMap,this,1)

/* Test class membership. */
#define astIsALutMap(this) astINVOKE_ISA(LutMap,this)

/* Constructor. */
#if defined(astCLASS)            /* Protected. */
#define astLutMap astINVOKE(F,astLutMap_)
#else
#define astLutMap astINVOKE(F,astLutMapId_)
#endif

#if defined(astCLASS)            /* Protected */

/* Initialiser. */
#define \
astInitLutMap(mem,size,init,vtab,name,nlut,lut,start,inc) \
astINVOKE(O,astInitLutMap_(mem,size,init,vtab,name,nlut,lut,start,inc,STATUS_PTR))

/* Vtab Initialiser. */
#define astInitLutMapVtab(vtab,name) astINVOKE(V,astInitLutMapVtab_(vtab,name,STATUS_PTR))
/* Loader. */
#define astLoadLutMap(mem,size,vtab,name,channel) \
astINVOKE(O,astLoadLutMap_(mem,size,vtab,name,astCheckChannel(channel),STATUS_PTR))
#endif

/* Interfaces to public member functions. */
/* -------------------------------------- */
/* Here we make use of astCheckLutMap to validate LutMap pointers
   before use.  This provides a contextual error report if a pointer
   to the wrong sort of Object is supplied. */
#if defined(astCLASS)            /* Protected */

#define astClearLutInterp(this) \
        astINVOKE(V,astClearLutInterp_(astCheckLutMap(this),STATUS_PTR))
#define astGetLutInterp(this) \
        astINVOKE(V,astGetLutInterp_(astCheckLutMap(this),STATUS_PTR))
#define astSetLutInterp(this,value) \
        astINVOKE(V,astSetLutInterp_(astCheckLutMap(this),value,STATUS_PTR))
#define astTestLutInterp(this) \
        astINVOKE(V,astTestLutInterp_(astCheckLutMap(this),STATUS_PTR))
#define astGetLutMapInfo(this,start,inc,nlut) \
        astINVOKE(V,astGetLutMapInfo_(astCheckLutMap(this),start,inc,nlut,STATUS_PTR))
#define astClearLutEpsilon(this) \
        astINVOKE(V,astClearLutEpsilon_(astCheckLutMap(this),STATUS_PTR))
#define astGetLutEpsilon(this) \
        astINVOKE(V,astGetLutEpsilon_(astCheckLutMap(this),STATUS_PTR))
#define astSetLutEpsilon(this,value) \
        astINVOKE(V,astSetLutEpsilon_(astCheckLutMap(this),value,STATUS_PTR))
#define astTestLutEpsilon(this) \
        astINVOKE(V,astTestLutEpsilon_(astCheckLutMap(this),STATUS_PTR))

#endif

#endif





