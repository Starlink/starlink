#if !defined( SLAMAP_INCLUDED )  /* Include this file only once */
#define SLAMAP_INCLUDED
/*
*+
*  Name:
*     slamap.h

*  Type:
*     C include file.

*  Purpose:
*     Define the interface to the SlaMap class.

*  Invocation:
*     #include "slamap.h"

*  Description:
*     This include file defines the interface to the SlaMap class and
*     provides the type definitions, function prototypes and macros,
*     etc.  needed to use this class.
*
*     The SlaMap class encapsulates the conversions provided by the
*     SLALIB library (SUN/67) for converting between different sky
*     coordinate systems.  Since, typically, a sequence of these
*     SLALIB conversions is required, an SlaMap can be used to
*     accumulate a series of conversions which it then applies in
*     sequence.

*  Inheritance:
*     The SlaMap class inherits from the Mapping class.

*  Attributes Over-Ridden:
*     None.

*  New Attributes Defined:
*     None.

*  Methods Over-Ridden:
*     Public:
*        astTransform
*           Use an SlaMap to transform a set of points.

*     Protected:
*        astMapMerge
*           Simplify a sequence of Mappings containing an SlaMap.

*  New Methods Defined:
*     Public:
*        astSlaAdd
*           Add a coordinate conversion step to an SlaMap.

*     Private:
*        None.

*  Other Class Functions:
*     Public:
*        astIsASlaMap
*           Test class membership.
*        astSlaMap
*           Create an SlaMap.

*     Protected:
*        astCheckSlaMap
*           Validate class membership.
*        astInitSlaMap
*           Initialise an SlaMap.
*        astLoadSlaMap
*           Load an SlaMap.

*  Macros:
*     None.

*  Type Definitions:
*     Public:
*        AstSlaMap
*           SlaMap object type.

*     Protected:
*        AstSlaMapVtab
*           SlaMap virtual function table type.

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
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     RFWS: R.F. Warren-Smith (Starlink)
*     DSB: David S. Berry (Starlink)

*  History:
*     26-APR-1996 (RFWS):
*        Original version.
*     26-SEP-1996 (RFWS):
*        Added external interface and I/O facilities.
*     23-MAY-1997 (RFWS):
*        Over-ride the astMapMerge method.
*     15-OCT-2002 (DSB):
*        Added astSTPConv, astSTPConv1, and STP coordinate system macros.
*     8-JAN-2003 (DSB):
*        Added protected astInitSlaMapVtab method.
*     22-FEB-2006 (DSB):
*        Added cvtextra to the AstSlaMap structure.
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
#if defined(astCLASS)            /* Protected */
#define AST__NOSTP -1 /* An invalid value for an STP coordinate system */
#define AST__HAE  0   /* Heliocentric-aries-ecliptic spherical coordinates */
#define AST__HAEC 1   /* Heliocentric-aries-ecliptic cartesian coordinates */
#define AST__HAQ  2   /* Heliocentric-aries-equatorial spherical coordinates */
#define AST__HAQC 3   /* Heliocentric-aries-equatorial cartesian coordinates */
#define AST__HG	  4   /* Heliographic spherical coordinates */
#define AST__HGC  5   /* Heliographic cartesian coordinates */
#define AST__HPC  6   /* Helioprojective-cartesian spherical coordinates */
#define AST__HPCC 7   /* Helioprojective-cartesian cartesian coordinates */
#define AST__HPR  8   /* Helioprojective-radial spherical coordinates */
#define AST__HPRC 9   /* Helioprojective-radial cartesian coordinates */
#define AST__GSE  10  /* Geocentric-solar-ecliptic spherical coordinates */
#define AST__GSEC 11  /* Geocentric-solar-ecliptic cartesian coordinates */
#endif

/* One IAU astronomical unit, in metres. */
#define AST__AU 1.49597870E11

/* One solar radius (top of photosphere?), in metres (from "The Explanatory
   Supplement to the Astronomical Almanac"). */
#define AST__SOLRAD 6.96E8

/* Define a dummy __attribute__ macro for use on non-GNU compilers. */
#ifndef __GNUC__
#  define  __attribute__(x)  /*NOTHING*/
#endif

/* SlaMap structure. */
/* ----------------- */
/* This structure contains all information that is unique to each
   object in the class (e.g. its instance variables). */
typedef struct AstSlaMap {

/* Attributes inherited from the parent class. */
   AstMapping mapping;           /* Parent class structure */

/* Attributes specific to objects in this class. */
   int *cvttype;                 /* Pointer to array of conversion types */
   double **cvtargs;             /* Pointer to argument list pointer array */
   double **cvtextra;            /* Pointer to intermediate values pointer array */
   int ncvt;                     /* Number of conversions to perform */
} AstSlaMap;

/* Virtual function table. */
/* ----------------------- */
/* This table contains all information that is the same for all
   objects in the class (e.g. pointers to its virtual functions). */
#if defined(astCLASS)            /* Protected */
typedef struct AstSlaMapVtab {

/* Properties (e.g. methods) inherited from the parent class. */
   AstMappingVtab mapping_vtab;  /* Parent class virtual function table */

/* A Unique identifier to determine class membership. */
   AstClassIdentifier id;

/* Properties (e.g. methods) specific to this class. */
   void (* SlaAdd)( AstSlaMap *, const char *, const double[], int * );
} AstSlaMapVtab;

#if defined(THREAD_SAFE)

/* Define a structure holding all data items that are global within this
   class. */
typedef struct AstSlaMapGlobals {
   AstSlaMapVtab Class_Vtab;
   int Class_Init;
   double Eq_Cache;
   double Ep_Cache;
   double Amprms_Cache[ 21 ];
} AstSlaMapGlobals;

#endif

#endif

/* Function prototypes. */
/* ==================== */
/* Prototypes for standard class functions. */
/* ---------------------------------------- */
astPROTO_CHECK(SlaMap)           /* Check class membership */
astPROTO_ISA(SlaMap)             /* Test class membership */

/* Constructor. */
#if defined(astCLASS)            /* Protected. */
AstSlaMap *astSlaMap_( int, const char *, int *, ...);
#else
AstSlaMap *astSlaMapId_( int, const char *, ... )__attribute__((format(printf,2,3)));
#endif

#if defined(astCLASS)            /* Protected */

/* Initialiser. */
AstSlaMap *astInitSlaMap_( void *, size_t, int, AstSlaMapVtab *,
                           const char *, int, int * );

/* Vtab initialiser. */
void astInitSlaMapVtab_( AstSlaMapVtab *, const char *, int * );

/* Loader. */
AstSlaMap *astLoadSlaMap_( void *, size_t, AstSlaMapVtab *,
                           const char *, AstChannel *, int * );

/* Thread-safe initialiser for all global data used by this module. */
#if defined(THREAD_SAFE)
void astInitSlaMapGlobals_( AstSlaMapGlobals * );
#endif

/* Other functions. */
void astSTPConv1_( double, int, double[3], double[3], int, double[3], double[3], int * );
void astSTPConv_( double, int, int, double[3], double *[3], int, double[3], double *[3], int * );

#endif

/* Prototypes for member functions. */
/* -------------------------------- */
void astSlaAdd_( AstSlaMap *, const char *, const double[], int * );

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
#define astCheckSlaMap(this) astINVOKE_CHECK(SlaMap,this,0)
#define astVerifySlaMap(this) astINVOKE_CHECK(SlaMap,this,1)

/* Test class membership. */
#define astIsASlaMap(this) astINVOKE_ISA(SlaMap,this)

/* Constructor. */
#if defined(astCLASS)            /* Protected. */
#define astSlaMap astINVOKE(F,astSlaMap_)
#else
#define astSlaMap astINVOKE(F,astSlaMapId_)
#endif

#if defined(astCLASS)            /* Protected */

/* Initialiser. */
#define astInitSlaMap(mem,size,init,vtab,name,flags) \
astINVOKE(O,astInitSlaMap_(mem,size,init,vtab,name,flags,STATUS_PTR))

/* Vtab Initialiser. */
#define astInitSlaMapVtab(vtab,name) astINVOKE(V,astInitSlaMapVtab_(vtab,name,STATUS_PTR))
/* Loader. */
#define astLoadSlaMap(mem,size,vtab,name,channel) \
astINVOKE(O,astLoadSlaMap_(mem,size,vtab,name,astCheckChannel(channel),STATUS_PTR))
#endif

/* Interfaces to public member functions. */
/* -------------------------------------- */
/* Here we make use of astCheckSlaMap to validate SlaMap pointers
   before use.  This provides a contextual error report if a pointer
   to the wrong sort of Object is supplied. */
#define astSlaAdd(this,cvt,args) \
astINVOKE(V,astSlaAdd_(astCheckSlaMap(this),cvt,args,STATUS_PTR))

#if defined(astCLASS)            /* Protected */
#define astSTPConv astSTPConv_
#define astSTPConv1 astSTPConv1_
#endif

#endif





