#if !defined( PCDMAP_INCLUDED ) /* Include this file only once */
#define PCDMAP_INCLUDED
/*
*+
*  Name:
*     pcdmap.h

*  Type:
*     C include file.

*  Purpose:
*     Define the interface to the PcdMap class.

*  Invocation:
*     #include "pcdmap.h"

*  Description:
*     This include file defines the interface to the PcdMap class and
*     provides the type definitions, function prototypes and macros,
*     etc.  needed to use this class.
*
*     The PcdMap class implements Mappings which perform pincushion
*     distortion.

*  Inheritance:
*     The PcdMap class inherits from the Mapping class.

*  Attributes Over-Ridden:
*     None.

*  New Attributes Defined:
*     Disco (double)
*        This attribute holds the PcdMap distortion coefficient used by
*        the forward transformation. This coefficient is set when a
*        PcdMap is created, but may later be modified. The default value
*        is zero, which gives no distortion. For pincushion distortion,
*        the supplied value should be positive. For barrel distortion, it
*        should be negative.
*
*        Note that the forward transformation of a PcdMap applies the
*        distortion corresponding to this attribute, and the inverse
*        transformation removes this distortion. If a PcdMap is inverted
*        (e.g. by using astInvert), then the forward transformation will
*        remove the distortion and the inverse transformation will apply
*        it. The distortion itself will still be given by the same value of
*        Disco.
*     PcdCen(axis)
*        This attribute specifies the centre of a pincushion distortion.
*        It takes a separate value for each axis of the PcdMap so that, for
*        instance, the settings "PcdCen(1)=345.0,PcdCen(2)=-104.4" specify
*        that the pincushion distortion is centred at values of 345.0 and
*        -104.4 on axes 1 and 2 of the PcdMap. The default for both axes is
*        zero.

*  Methods Over-Ridden:
*     Public:
*        None.
*
*     Protected:
*        astClearAttrib
*           Clear an attribute value for a PcdMap.
*        astGetAttrib
*           Get an attribute value for a PcdMap.
*        astSetAttrib
*           Set an attribute value for a PcdMap.
*        astTestAttrib
*           Test if an attribute value has been set for a PcdMap.
*        astTransform
*           Apply a PcdMap to transform a set of points.

*  New Methods Defined:
*     Public:
*        None.
*
*     Protected:
*        astClearDisco
*           Clear the Disco attribute value for a PcdMap.
*        astGetDisco
*           Get the Disco attribute value for a PcdMap.
*        astSetDisco
*           Set the Disco attribute value for a PcdMap.
*        astTestDisco
*           Test if a Disco attribute value has been set for a PcdMap.
*        astClearPcdCen
*           Clear the PcdCen attribute value for a PcdMap.
*        astGetPcdCen
*           Get the PcdCen attribute value for a PcdMap.
*        astSetPcdCen
*           Set the PcdCen attribute value for a PcdMap.
*        astTestPcdCen
*           Test if a PcdCen attribute value has been set for a PcdMap.

*  Other Class Functions:
*     Public:
*        astIsAPcdMap
*           Test class membership.
*        astPcdMap
*           Create a PcdMap.
*
*     Protected:
*        astCheckPcdMap
*           Validate class membership.
*        astInitPcdMap
*           Initialise a PcdMap.
*        astInitPcdMapVtab
*           Initialise the virtual function table for the PcdMap class.
*        astLoadPcdMap
*           Load a PcdMap.

*  Macros:
*     None.

*  Type Definitions:
*     Public:
*        AstPcdMap
*           PcdMap object type.
*
*     Protected:
*        AstPcdMapVtab
*           PcdMap virtual function table type.

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
*     18-MAY-1999 (DSB):
*        Original version.
*     8-JAN-2003 (DSB):
*        Added protected astInitPcdMapVtab method.
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
/* PcdMap structure. */
/* ------------------ */
/* This structure contains all information that is unique to each object in
   the class (e.g. its instance variables). */
typedef struct AstPcdMap {

/* Attributes inherited from the parent class. */
   AstMapping mapping;           /* Parent class structure */

/* Attributes specific to objects in this class. */
   double disco;                 /* Distortion coefficient */
   double pcdcen[2];             /* Distortion centre */

} AstPcdMap;

/* Virtual function table. */
/* ----------------------- */
/* This table contains all information that is the same for all
   objects in the class (e.g. pointers to its virtual functions). */
#if defined(astCLASS)            /* Protected */
typedef struct AstPcdMapVtab {

/* Properties (e.g. methods) inherited from the parent class. */
   AstMappingVtab mapping_vtab;  /* Parent class virtual function table */

/* A Unique identifier to determine class membership. */
   AstClassIdentifier id;

/* Properties (e.g. methods) specific to this class. */
   double (*GetDisco)( AstPcdMap *, int * );
   int (* TestDisco)( AstPcdMap *, int * );
   void (* ClearDisco)( AstPcdMap *, int * );
   void (* SetDisco)( AstPcdMap *, double, int * );
   double (*GetPcdCen)( AstPcdMap *, int, int * );
   int (* TestPcdCen)( AstPcdMap *, int, int * );
   void (* ClearPcdCen)( AstPcdMap *, int, int * );
   void (* SetPcdCen)( AstPcdMap *, int, double, int * );
} AstPcdMapVtab;

#if defined(THREAD_SAFE)

/* Define a structure holding all data items that are global within this
   class. */
typedef struct AstPcdMapGlobals {

/* Define the thread-specific globals. */
   char GetAttrib_Buff[ 101 ];
   AstPcdMapVtab Class_Vtab;
   int Class_Init;
} AstPcdMapGlobals;

#endif

#endif

/* Function prototypes. */
/* ==================== */
/* Prototypes for standard class functions. */
/* ---------------------------------------- */
astPROTO_CHECK(PcdMap)          /* Check class membership */
astPROTO_ISA(PcdMap)            /* Test class membership */

/* Constructor. */
#if defined(astCLASS)            /* Protected. */
AstPcdMap *astPcdMap_( double, const double [2], const char *, int *, ...);
#else
AstPcdMap *astPcdMapId_( double, const double [2], const char *, ... )__attribute__((format(printf,3,4)));
#endif

#if defined(astCLASS)            /* Protected */

/* Initialiser. */
AstPcdMap *astInitPcdMap_( void *, size_t, int, AstPcdMapVtab *,
                           const char *, double, const double [2], int * );

/* Vtab initialiser. */
void astInitPcdMapVtab_( AstPcdMapVtab *, const char *, int * );

/* Loader. */
AstPcdMap *astLoadPcdMap_( void *, size_t, AstPcdMapVtab *,
                           const char *, AstChannel *, int * );

/* Thread-safe initialiser for all global data used by this module. */
#if defined(THREAD_SAFE)
void astInitPcdMapGlobals_( AstPcdMapGlobals * );
#endif

#endif

/* Prototypes for member functions. */
/* -------------------------------- */
# if defined(astCLASS)           /* Protected */
double astGetDisco_( AstPcdMap *, int * );
int astTestDisco_( AstPcdMap *, int * );
void astClearDisco_( AstPcdMap *, int * );
void astSetDisco_( AstPcdMap *, double, int * );
double astGetPcdCen_( AstPcdMap *, int, int * );
int astTestPcdCen_( AstPcdMap *, int, int * );
void astClearPcdCen_( AstPcdMap *, int, int * );
void astSetPcdCen_( AstPcdMap *, int, double, int * );
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
#define astCheckPcdMap(this) astINVOKE_CHECK(PcdMap,this,0)
#define astVerifyPcdMap(this) astINVOKE_CHECK(PcdMap,this,1)

/* Test class membership. */
#define astIsAPcdMap(this) astINVOKE_ISA(PcdMap,this)

/* Constructor. */
#if defined(astCLASS)            /* Protected. */
#define astPcdMap astINVOKE(F,astPcdMap_)
#else
#define astPcdMap astINVOKE(F,astPcdMapId_)
#endif

#if defined(astCLASS)            /* Protected */

/* Initialiser. */
#define astInitPcdMap(mem,size,init,vtab,name,disco,pcdcen) \
astINVOKE(O,astInitPcdMap_(mem,size,init,vtab,name,disco,pcdcen,STATUS_PTR))

/* Vtab Initialiser. */
#define astInitPcdMapVtab(vtab,name) astINVOKE(V,astInitPcdMapVtab_(vtab,name,STATUS_PTR))
/* Loader. */
#define astLoadPcdMap(mem,size,vtab,name,channel) \
astINVOKE(O,astLoadPcdMap_(mem,size,vtab,name,astCheckChannel(channel),STATUS_PTR))
#endif

/* Interfaces to public member functions. */
/* -------------------------------------- */
/* Here we make use of astCheckPcdMap to validate PcdMap pointers
   before use.  This provides a contextual error report if a pointer
   to the wrong sort of Object is supplied. */

#if defined(astCLASS)            /* Protected */
#define astClearDisco(this) astINVOKE(V,astClearDisco_(astCheckPcdMap(this),STATUS_PTR))
#define astGetDisco(this) astINVOKE(V,astGetDisco_(astCheckPcdMap(this),STATUS_PTR))
#define astSetDisco(this,value) \
astINVOKE(V,astSetDisco_(astCheckPcdMap(this),value,STATUS_PTR))
#define astTestDisco(this) astINVOKE(V,astTestDisco_(astCheckPcdMap(this),STATUS_PTR))

#define astClearPcdCen(this,axis) \
astINVOKE(V,astClearPcdCen_(astCheckPcdMap(this),axis,STATUS_PTR))
#define astGetPcdCen(this,axis) \
astINVOKE(V,astGetPcdCen_(astCheckPcdMap(this),axis,STATUS_PTR))
#define astSetPcdCen(this,axis,value) \
astINVOKE(V,astSetPcdCen_(astCheckPcdMap(this),axis,value,STATUS_PTR))
#define astTestPcdCen(this,axis) \
astINVOKE(V,astTestPcdCen_(astCheckPcdMap(this),axis,STATUS_PTR))

#endif
#endif





