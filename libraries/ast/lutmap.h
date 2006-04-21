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
   double start;                /* Input value for first table entry */
   double inc;                  /* Input increment between table entries */
   double last_fwd_in;          /* Last input value (forward transfm.) */
   double last_fwd_out;         /* Last output value (forward transfm.) */
   double last_inv_in;          /* Last input value (inverse transfm.) */
   double last_inv_out;         /* Last output value (inverse transfm.) */
   int nlut;                    /* Number of table entries */
   int lutinterp;               /* Interpolation method */
} AstLutMap;

/* Virtual function table. */
/* ----------------------- */
/* This table contains all information that is the same for all
   objects in the class (e.g. pointers to its virtual functions). */
#if defined(astCLASS)            /* Protected */
typedef struct AstLutMapVtab {

/* Properties (e.g. methods) inherited from the parent class. */
   AstMappingVtab mapping_vtab;  /* Parent class virtual function table */

/* Unique flag value to determine class membership. */
   int *check;                   /* Check value */

/* Properties (e.g. methods) specific to this class. */
   int (*GetLutInterp)( AstLutMap * );
   int (* TestLutInterp)( AstLutMap * );
   void (* ClearLutInterp)( AstLutMap * );
   void (* SetLutInterp)( AstLutMap *, int );

} AstLutMapVtab;
#endif

/* Function prototypes. */
/* ==================== */
/* Prototypes for standard class functions. */
/* ---------------------------------------- */
astPROTO_CHECK(LutMap)          /* Check class membership */
astPROTO_ISA(LutMap)            /* Test class membership */

/* Constructor. */
#if defined(astCLASS)            /* Protected. */
AstLutMap *astLutMap_( int, const double [], double, double, const char *, ... );
#else
AstLutMap *astLutMapId_( int, const double [], double, double, const char *, ... );
#endif

#if defined(astCLASS)            /* Protected */

/* Initialiser. */
AstLutMap *astInitLutMap_( void *, size_t, int, AstLutMapVtab *, const char *, int, const double *, double, double );

/* Vtab initialiser. */
void astInitLutMapVtab_( AstLutMapVtab *, const char * );

/* Loader. */
AstLutMap *astLoadLutMap_( void *, size_t, AstLutMapVtab *, const char *, AstChannel * );
#endif

/* Prototypes for member functions. */
/* -------------------------------- */
# if defined(astCLASS)           /* Protected */
   int astGetLutInterp_( AstLutMap * );
   int astTestLutInterp_( AstLutMap * );
   void astClearLutInterp_( AstLutMap * );
   void astSetLutInterp_( AstLutMap *, int );
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
#define astCheckLutMap(this) astINVOKE_CHECK(LutMap,this)

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
astINVOKE(O,astInitLutMap_(mem,size,init,vtab,name,nlut,lut,start,inc))

/* Vtab Initialiser. */
#define astInitLutMapVtab(vtab,name) astINVOKE(V,astInitLutMapVtab_(vtab,name))
/* Loader. */
#define astLoadLutMap(mem,size,vtab,name,channel) \
astINVOKE(O,astLoadLutMap_(mem,size,vtab,name,astCheckChannel(channel)))
#endif

/* Interfaces to public member functions. */
/* -------------------------------------- */
/* Here we make use of astCheckLutMap to validate LutMap pointers
   before use.  This provides a contextual error report if a pointer
   to the wrong sort of Object is supplied. */
#if defined(astCLASS)            /* Protected */
#define astClearLutInterp(this) \
        astINVOKE(V,astClearLutInterp_(astCheckLutMap(this)))
#define astGetLutInterp(this) \
        astINVOKE(V,astGetLutInterp_(astCheckLutMap(this)))
#define astSetLutInterp(this,value) \
        astINVOKE(V,astSetLutInterp_(astCheckLutMap(this),value))
#define astTestLutInterp(this) \
        astINVOKE(V,astTestLutInterp_(astCheckLutMap(this)))
#endif

#endif
