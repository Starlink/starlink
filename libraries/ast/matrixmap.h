#if !defined( MATRIXMAP_INCLUDED ) /* Include this file only once */
#define MATRIXMAP_INCLUDED
/*
*+
*  Name:
*     matrixmap.h

*  Type:
*     C include file.

*  Purpose:
*     Define the interface to the MatrixMap class.

*  Invocation:
*     #include "matrixmap.h"

*  Description:
*     This include file defines the interface to the MatrixMap class and
*     provides the type definitions, function prototypes and macros,
*     etc.  needed to use this class.
*
*     The MatrixMap class implements Mappings that transform a set of
*     coordinates by multiplying them by a matrix. The inverse transformation
*     can only be applied if the associated matrix is square and non-singular.

*  Inheritance:
*     The MatrixMap class inherits from the Mapping class.

*  Attributes Over-Ridden:
*     TranForward (integer)
*        A read-only boolean value (0 or 1) which indicates whether a
*        MatrixMap is able to transform coordinates in the "forward"
*        direction (i.e. converting input coordinates into output
*        coordinates).
*     TranInverse (integer)
*        A read-only boolean value (0 or 1) which indicates whether a
*        MatrixMap is able to transform coordinates in the "inverse"
*        direction (i.e. converting output coordinates back into input
*        coordinates).

*  New Attributes Defined:
*     None.

*  Methods Over-Ridden:
*     Public:
*        None.
*
*     Protected:
*        astTransform
*           Apply a MatrixMap to transform a set of points.
*        astGetTranForward
*           Determine if a MatrixMap can perform a "forward" coordinate
*           transformation.
*        astGetTranInverse
*           Determine if a MatrixMap can perform an "inverse" coordinate
*           transformation.

*  New Methods Defined:
*     Public:
*        None.
*
*     Protected:
*        astMtrMult
*           Multiply a MatrixMap by another MatrixMap.
*        astMtrRot
*           Rotate a MatrixMap.

*  Other Class Functions:
*     Public:
*        astIsAMatrixMap
*           Test class membership.
*        astMatrixMap
*           Create a MatrixMap.
*
*     Protected:
*        astCheckMatrixMap
*           Validate class membership.
*        astInitMatrixMap
*           Initialise a MatrixMap.
*        astInitMatrixMapVtab
*           Initialise the virtual function table for the MatrixMap class.
*        astLoadMatrixMap
*           Load a MatrixMap.

*  Macros:
*     None.

*  Type Definitions:
*     Public:
*        AstMatrixMap
*           MatrixMap object type.
*
*     Protected:
*        AstMatrixMapVtab
*           MatrixMap virtual function table type.

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
*     DSB: D.S. Berry (Starlink)

*  History:
*     12-Feb-1996 (DSB):
*        Original version.
*     22-Feb-1996 (DSB):
*        Method "astMatrixRotate" added.
*     5-Mar-1996 (DSB):
*        Method "astMatrixMult" added.
*     14-NOV-1996 (DSB):
*        External interface and I/O added. Public method names shortened.
*     3-JUN-1997 (DSB):
*        astMtrMult and astMtrRot made protected instead of public.
*     8-JAN-2003 (DSB):
*        Added protected astInitMatrixMapVtab method.
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
/* MatrixMap structure. */
/* ------------------ */
/* This structure contains all information that is unique to each object in
   the class (e.g. its instance variables). */
typedef struct AstMatrixMap {

/* Attributes inherited from the parent class. */
   AstMapping mapping;           /* Parent class structure */

/* Attributes specific to objects in this class. */
   double *f_matrix;             /* Pointer to forward matrix */
   double *i_matrix;             /* Pointer to inverse matrix */
   int form;                     /* Matrix storage form */

} AstMatrixMap;

/* Virtual function table. */
/* ----------------------- */
/* This table contains all information that is the same for all
   objects in the class (e.g. pointers to its virtual functions). */
#if defined(astCLASS)            /* Protected */
typedef struct AstMatrixMapVtab {

/* Properties (e.g. methods) inherited from the parent class. */
   AstMappingVtab mapping_vtab;  /* Parent class virtual function table */

/* A Unique identifier to determine class membership. */
   AstClassIdentifier id;

/* Properties (e.g. methods) specific to this class. */
   AstMatrixMap *(* MtrRot)( AstMatrixMap *, double, const double[], int * );
   AstMatrixMap *(* MtrMult)( AstMatrixMap *,  AstMatrixMap *, int * );

} AstMatrixMapVtab;

#if defined(THREAD_SAFE)

/* Define a structure holding all data items that are global within the
   object.c file. */

typedef struct AstMatrixMapGlobals {
   AstMatrixMapVtab Class_Vtab;
   int Class_Init;
} AstMatrixMapGlobals;


/* Thread-safe initialiser for all global data used by this module. */
void astInitMatrixMapGlobals_( AstMatrixMapGlobals * );

#endif


#endif

/* Function prototypes. */
/* ==================== */
/* Prototypes for standard class functions. */
/* ---------------------------------------- */
astPROTO_CHECK(MatrixMap)          /* Check class membership */
astPROTO_ISA(MatrixMap)            /* Test class membership */

/* Constructor. */
#if defined(astCLASS)            /* Protected. */
AstMatrixMap *astMatrixMap_( int, int, int, const double[], const char *, int *, ...);
#else
AstMatrixMap *astMatrixMapId_( int, int, int, const double[], const char *, ... )__attribute__((format(printf,5,6)));
#endif

#if defined(astCLASS)            /* Protected */

/* Initialiser. */
AstMatrixMap *astInitMatrixMap_( void *, size_t, int, AstMatrixMapVtab *,
                                 const char *, int, int, int, const double[], int * );

/* Vtab initialiser. */
void astInitMatrixMapVtab_( AstMatrixMapVtab *, const char *, int * );

/* Loader. */
AstMatrixMap *astLoadMatrixMap_( void *, size_t, AstMatrixMapVtab *,
                                 const char *, AstChannel *, int * );
#endif

/* Prototypes for member functions. */
/* -------------------------------- */
# if defined(astCLASS)           /* Protected */
AstMatrixMap *astMtrRot_( AstMatrixMap *, double, const double[], int * );
AstMatrixMap *astMtrMult_( AstMatrixMap *, AstMatrixMap *, int * );
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
#define astCheckMatrixMap(this) astINVOKE_CHECK(MatrixMap,this,0)
#define astVerifyMatrixMap(this) astINVOKE_CHECK(MatrixMap,this,1)

/* Test class membership. */
#define astIsAMatrixMap(this) astINVOKE_ISA(MatrixMap,this)

/* Constructor. */
#if defined(astCLASS)            /* Protected. */
#define astMatrixMap astINVOKE(F,astMatrixMap_)
#else
#define astMatrixMap astINVOKE(F,astMatrixMapId_)
#endif

#if defined(astCLASS)            /* Protected */

/* Initialiser. */
#define astInitMatrixMap(mem,size,init,vtab,name,nin,nout,form,matrix) \
astINVOKE(O,astInitMatrixMap_(mem,size,init,vtab,name,nin,nout,form,matrix,STATUS_PTR))

/* Vtab Initialiser. */
#define astInitMatrixMapVtab(vtab,name) astINVOKE(V,astInitMatrixMapVtab_(vtab,name,STATUS_PTR))
/* Loader. */
#define astLoadMatrixMap(mem,size,vtab,name,channel) \
astINVOKE(O,astLoadMatrixMap_(mem,size,vtab,name,astCheckChannel(channel),STATUS_PTR))
#endif

/* Interfaces to public member functions. */
/* -------------------------------------- */
/* Here we make use of astCheckMatrixMap to validate MatrixMap pointers
   before use.  This provides a contextual error report if a pointer
   to the wrong sort of Object is supplied. */

#if defined(astCLASS)            /* Protected */
#define astMtrRot(this,theta,axis) \
astINVOKE(O,astMtrRot_(astCheckMatrixMap(this),theta,axis,STATUS_PTR))

#define astMtrMult(this,a) \
astINVOKE(O,astMtrMult_(astCheckMatrixMap(this),astCheckMatrixMap(a),STATUS_PTR))
#endif
#endif





