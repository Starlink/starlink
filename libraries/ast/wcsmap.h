#if !defined( WCSMAP_INCLUDED ) /* Include this file only once */
#define WCSMAP_INCLUDED
/*
*+
*  Name:
*     wcsmap.h

*  Type:
*     C include file.

*  Purpose:
*     Define the interface to the WcsMap class.

*  Invocation:
*     #include "wcsmap.h"

*  Description:
*     This include file defines the interface to the WcsMap class and
*     provides the type definitions, function prototypes and macros,
*     etc.  needed to use this class.
*
*     The WcsMap class implements Mappings that transform a pair of
*     longitude/latitude values into a pair of projected Cartesian
*     coordinates. All the projections included in FITS WCS are included.
*     For more information about these projections, see the appropriate
*     FITS document.

*  Inheritance:
*     The WcsMap class inherits from the Mapping class.

*  Attributes Over-Ridden:
*     None.

*  New Attributes Defined:
*     NatLat (double)
*        This attribute gives the latitude of the reference point of
*        a FITS WCS projection, in the native coordinate system. The value
*        returned is in radians. A value of AST__BAD is returned if
*        no value is defined. This attribute is read only, and may change
*        if new values are assigned to the projection parameters.
*     NatLon (double)
*        This attribute gives the longitude of the reference point of
*        a FITS WCS projection, in the native coordinate system. The value
*        returned is in radians. A value of AST__BAD is returned if
*        no value is defined. This attribute is read only, and may change
*        if new values are assigned to the projection parameters.
*     ProjP(i) (double)
*        This attribute provides aliases for the PV attributes, which
*        specifies the projection parameter values to be used by a WcsMap
*        when implementing a FITS-WCS sky projection. ProjP is retained for
*        compatibility with previous versions of FITS-WCS and AST. New
*        applications should use the PV attibute instead.
*     PVj_m (double)
*        This attribute gives the parameter values used by a FITS WCS
*        projection. The index j is the axis index in the range 1 to 99, and
*        the index m is the parameter index in the range 0 to 99. They will
*        have the value AST__BAD if undefined. By default, no projection
*        parameters are defined. These should be assigned appropriate values
*        before using a WcsMap to transform points.
*     WcsAxis(lonlat) (int)
*        This attribute gives the indices of the longitude and latitude axes
*        of a FITS WCS projection within the coordinate system used by a
*        WcsMap. If "lonlat" is 1 then the index of the longitude axis is
*        returned. If it is 2 the index of the latitude axis is returned.
*        The first axis in the coordinate system is axis 1. This is a
*        read-only attribute.
*     WcsType (int)
*        This attribute gives the FITS WCS projection type implemented by a
*        WcsMap. Macros giving the integer value associated with supported
*        projections are defined. They have the general form "AST__xxx" where
*        "xxx" is the 3-character code used to represent the projection in the
*        FITS CTYPE keyword.

*  Methods Over-Ridden:
*     Public:
*        None.
*
*     Protected:
*        astClearAttrib
*           Clear an attribute value for a WcsMap.
*        astGetAttrib
*           Get an attribute value for a WcsMap.
*        astSetAttrib
*           Set an attribute value for a WcsMap.
*        astTestAttrib
*           Test if an attribute value has been set for a WcsMap.
*        astTransform
*           Apply a WcsMap to transform a set of points.

*  New Methods Defined:
*     Public:
*        None.
*
*     Protected:
*        astClearPV
*           Clear a PVi_j attribute value for a WcsMap.
*        astGetNatLat
*           Get the NatLat attribute value for a WcsMap.
*        astGetNatLon
*           Get the NatLon attribute value for a WcsMap.
*        astGetPV
*           Get a PVi_j attribute value for a WcsMap.
*        astGetWcsAxis
*           Get a WcsAxis attribute value for a WcsMap.
*        astGetWcsType
*           Get the WcsType attribute value for a WcsMap.
*        astIsZenithal
*           Is the projection zenithal?
*        astSetPV
*           Set a PVi_j attribute value for a WcsMap.
*        astTestPV
*           Test if a PVi_j attribute value has been set for a WcsMap.
*        astWcsPrjName
*           Return the FITS CTYPE keyword value for a given projection type.
*        astWcsPrjDesc
*           Return a textual description for a given projection type.
*        astWcsPrjType
*           Return the projection type given a FITS CTYPE keyword value.

*  Other Class Functions:
*     Public:
*        astIsAWcsMap
*           Test class membership.
*        astWcsMap
*           Create a WcsMap.
*
*     Protected:
*        astCheckWcsMap
*           Validate class membership.
*        astInitWcsMap
*           Initialise a WcsMap.
*        astInitWcsMapVtab
*           Initialise the virtual function table for the WcsMap class.
*        astLoadWcsMap
*           Load a WcsMap.

*  Macros:
*     Public:
*        AST__WCSMX
*           Maximum number of parameters associated with a projection.
*        AST__DPI
*           180 degrees in radians.
*        AST__DPIBY2
*           90 degrees in radians.
*        AST__DD2R
*           Factor for converting degrees to radians.
*        AST__DR2D
*           Factor for converting radians to degrees.
*        AST__AZP
*           An integer identifier for the FITS AZP projection.
*        AST__TAN
*           An integer identifier for the FITS TAN projection.
*        AST__SIN
*           An integer identifier for the FITS SIN projection.
*        AST__STG
*           An integer identifier for the FITS STG projection.
*        AST__ARC
*           An integer identifier for the FITS ARC projection.
*        AST__ZPN
*           An integer identifier for the FITS ZPN projection.
*        AST__ZEA
*           An integer identifier for the FITS ZEA projection.
*        AST__AIR
*           An integer identifier for the FITS AIR projection.
*        AST__CYP
*           An integer identifier for the FITS CYP projection.
*        AST__CAR
*           An integer identifier for the FITS CAR projection.
*        AST__MER
*           An integer identifier for the FITS MER projection.
*        AST__CEA
*           An integer identifier for the FITS CEA projection.
*        AST__COP
*           An integer identifier for the FITS COP projection.
*        AST__COD
*           An integer identifier for the FITS COD projection.
*        AST__COE
*           An integer identifier for the FITS COE projection.
*        AST__COO
*           An integer identifier for the FITS COO projection.
*        AST__BON
*           An integer identifier for the FITS BON projection.
*        AST__PCO
*           An integer identifier for the FITS PCO projection.
*        AST__GLS
*           A depracated integer identifier for the FITS SFL projection.
*        AST__SFL
*           An integer identifier for the FITS SFL projection.
*        AST__PAR
*           An integer identifier for the FITS PAR projection.
*        AST__AIT
*           An integer identifier for the FITS AIT projection.
*        AST__MOL
*           An integer identifier for the FITS MOL projection.
*        AST__CSC
*           An integer identifier for the FITS CSC projection.
*        AST__QSC
*           An integer identifier for the FITS QSC projection.
*        AST__TSC
*           An integer identifier for the FITS TSC projection
*        AST__HPX
*           An integer identifier for the FITS HPX projection.
*        AST__XPH
*           An integer identifier for the FITS XPH projection.
*        AST__TPN
*           An integer identifier for a "TAN with correction terms" projection.
*        AST__WCSBAD
*           An integer identifier for a "null" projection.
*
*     Protected:
*        None.

*  Type Definitions:
*     Public:
*        AstWcsMap
*           WcsMap object type.
*
*     Protected:
*        AstWcsMapVtab
*           WcsMap virtual function table type.

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
*     15-Feb-1996 (DSB):
*        Original version.
*     23-MAR-1996 (DSB):
*        Support for PointSets with more than 2 axes added.
*     18-NOV-1996 (DSB):
*        Updated to include attributes, etc.
*     26-SEP-1997 (DSB):
*        Included new protected function, astPrjDesc.
*     11-FEB-2000 (DSB):
*        Replaced wcsmap component projp by pointers p and np.
*     20-OCT-2002 (DSB):
*        Added astIsZenithal
*     8-JAN-2003 (DSB):
*        Added protected astInitWcsMapVtab method.
*-
*/

/* Include files. */
/* ============== */
/* Interface definitions. */
/* ---------------------- */
#include "mapping.h"             /* Coordinate mappings (parent class) */
#include "proj.h"                /* Mark Calabretta's WCSLIB library header
                                    file */

#if defined(astCLASS)            /* Protected */
#include "pointset.h"            /* Sets of points/coordinates */
#include "channel.h"             /* I/O channels */
#endif

/* C header files. */
/* --------------- */
#if defined(astCLASS)            /* Protected */
#include <stddef.h>
#endif

/* Macros. */
/* ------- */
/* Max. number of parameters for a WCS projection */

#if defined(astCLASS) || defined(astFORTRAN77)
#define STATUS_PTR status
#else
#define STATUS_PTR astGetStatusPtr
#endif
#define AST__WCSMX 10

/* pi:  180 degrees in radians - from SLALIB file slamac.h. */
#define AST__DPI 3.1415926535897932384626433832795028841971693993751

/* pi/2:  90 degrees in radians - from SLALIB file slamac.h. */
#define AST__DPIBY2 1.5707963267948966192313216916397514420985846996876

/* pi/180:  degrees to radians - from SLALIB file slamac.h. */
#define AST__DD2R 0.017453292519943295769236907684886127134428718885417

/* 180/pi:  radians to degrees - from SLALIB file slamac.h. */
#define AST__DR2D 57.295779513082320876798154814105170332405472466564

/* Projection Types: (note, WCSBAD must be the last in this list) */

#define AST__AZP 1
#define AST__SZP 2
#define AST__TAN 3
#define AST__STG 4
#define AST__SIN 5
#define AST__ARC 6
#define AST__ZPN 7
#define AST__ZEA 8
#define AST__AIR 9
#define AST__CYP 10
#define AST__CEA 11
#define AST__CAR 12
#define AST__MER 13
#define AST__SFL 14
#define AST__PAR 15
#define AST__MOL 16
#define AST__AIT 17
#define AST__COP 18
#define AST__COE 19
#define AST__COD 20
#define AST__COO 21
#define AST__BON 22
#define AST__PCO 23
#define AST__TSC 24
#define AST__CSC 25
#define AST__QSC 26
#define AST__NCP 27
#define AST__GLS 28
#define AST__TPN 29
#define AST__HPX 30
#define AST__XPH 31
#define AST__WCSBAD 32   /* A bad projection type */

/* Define a dummy __attribute__ macro for use on non-GNU compilers. */
#ifndef __GNUC__
#  define  __attribute__(x)  /*NOTHING*/
#endif

/* Type Definitions. */
/* ================= */
/* WcsMap structure. */
/* ------------------ */
/* This structure contains all information that is unique to each object in
   the class (e.g. its instance variables). */
typedef struct AstWcsMap {

/* Attributes inherited from the parent class. */
   AstMapping mapping;           /* Parent class structure */

/* Attributes specific to objects in this class. */
   int type;                     /* Projection type */
   int wcsaxis[2];               /* Indices of lon and lat. axes */
   double **p;                   /* Pointer to array of projection parameter arrays */
   int *np;                      /* Pointer to array of projection parameter counts */
   struct AstPrjPrm params;      /* WCS structure holding projection
                                    parameters, etc. Defined in proj.h */
   int fits_proj;                /* Use as FITS-WCS projection? */
   int tpn_tan;                  /* Include TAN projection in TPN transformation? */
} AstWcsMap;

/* Virtual function table. */
/* ----------------------- */
/* This table contains all information that is the same for all
   objects in the class (e.g. pointers to its virtual functions). */
#if defined(astCLASS)            /* Protected */
typedef struct AstWcsMapVtab {

/* Properties (e.g. methods) inherited from the parent class. */
   AstMappingVtab mapping_vtab;  /* Parent class virtual function table */

/* A Unique identifier to determine class membership. */
   AstClassIdentifier id;

/* Properties (e.g. methods) specific to this class. */
   double (* GetNatLat)( AstWcsMap *, int * );
   double (* GetNatLon)( AstWcsMap *, int * );
   double (* GetPV)( AstWcsMap *, int, int, int * );
   int (* GetWcsAxis)( AstWcsMap *, int, int * );
   int (* GetWcsType)( AstWcsMap *, int * );
   int (* GetPVMax)( AstWcsMap *, int, int * );
   int (* TestPV)( AstWcsMap *, int, int, int * );
   void (* ClearPV)( AstWcsMap *, int, int, int * );
   void (* SetPV)( AstWcsMap *, int, int, double, int * );
   int (* IsZenithal)( AstWcsMap *, int * );

   int (* GetFITSProj)( AstWcsMap *, int * );
   int (* TestFITSProj)( AstWcsMap *, int * );
   void (* ClearFITSProj)( AstWcsMap *, int * );
   void (* SetFITSProj)( AstWcsMap *, int, int * );

   int (* GetTPNTan)( AstWcsMap *, int * );
   int (* TestTPNTan)( AstWcsMap *, int * );
   void (* ClearTPNTan)( AstWcsMap *, int * );
   void (* SetTPNTan)( AstWcsMap *, int, int * );

} AstWcsMapVtab;

#if defined(THREAD_SAFE)

/* Define a structure holding all data items that are global within this
   class. */
typedef struct AstWcsMapGlobals {
   AstWcsMapVtab Class_Vtab;
   int Class_Init;
   char GetAttrib_Buff[ 101 ];
} AstWcsMapGlobals;

#endif
#endif

/* Function prototypes. */
/* ==================== */
/* Prototypes for standard class functions. */
/* ---------------------------------------- */
astPROTO_CHECK(WcsMap)          /* Check class membership */
astPROTO_ISA(WcsMap)            /* Test class membership */

/* Constructor. */
#if defined(astCLASS)            /* Protected. */
AstWcsMap *astWcsMap_( int, int, int, int, const char *, int *, ...);
#else
AstWcsMap *astWcsMapId_( int, int, int, int, const char *, ... )__attribute__((format(printf,5,6)));
#endif

#if defined(astCLASS)            /* Protected */

/* Initialiser. */
AstWcsMap *astInitWcsMap_( void *, size_t, int, AstWcsMapVtab *,
                           const char *, int, int, int, int, int * );

/* Vtab initialiser. */
void astInitWcsMapVtab_( AstWcsMapVtab *, const char *, int * );

/* Loader. */
AstWcsMap *astLoadWcsMap_( void *, size_t, AstWcsMapVtab *,
                           const char *, AstChannel *, int * );

/* Thread-safe initialiser for all global data used by this module. */
#if defined(THREAD_SAFE)
void astInitWcsMapGlobals_( AstWcsMapGlobals * );
#endif

#endif

/* Prototypes for member functions. */
/* -------------------------------- */
# if defined(astCLASS)           /* Protected */
   const char *astWcsPrjDesc_( int, int * );
   const char *astWcsPrjName_( int, int * );
   double astGetNatLat_( AstWcsMap *, int * );
   double astGetNatLon_( AstWcsMap *, int * );
   double astGetPV_( AstWcsMap *, int, int, int * );
   int astWcsPrjType_( const char *, int * );
   int astGetWcsAxis_( AstWcsMap *, int, int * );
   int astGetWcsType_( AstWcsMap *, int * );
   int astGetPVMax_( AstWcsMap *, int, int * );
   int astTestPV_( AstWcsMap *, int, int, int * );
   int astIsZenithal_( AstWcsMap *, int * );
   void astClearPV_( AstWcsMap *, int, int, int * );
   void astSetPV_( AstWcsMap *, int, int, double, int * );

   int astGetFITSProj_( AstWcsMap *, int * );
   int astTestFITSProj_( AstWcsMap *, int * );
   void astClearFITSProj_( AstWcsMap *, int * );
   void astSetFITSProj_( AstWcsMap *, int, int * );

   int astGetTPNTan_( AstWcsMap *, int * );
   int astTestTPNTan_( AstWcsMap *, int * );
   void astClearTPNTan_( AstWcsMap *, int * );
   void astSetTPNTan_( AstWcsMap *, int, int * );

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
#define astCheckWcsMap(this) astINVOKE_CHECK(WcsMap,this,0)
#define astVerifyWcsMap(this) astINVOKE_CHECK(WcsMap,this,1)

/* Test class membership. */
#define astIsAWcsMap(this) astINVOKE_ISA(WcsMap,this)

/* Constructor. */
#if defined(astCLASS)            /* Protected. */
#define astWcsMap astINVOKE(F,astWcsMap_)
#else
#define astWcsMap astINVOKE(F,astWcsMapId_)
#endif

#if defined(astCLASS)            /* Protected */

/* Initialiser. */
#define astInitWcsMap(mem,size,init,vtab,name,ncoord,type,lon,lat) \
astINVOKE(O,astInitWcsMap_(mem,size,init,vtab,name,ncoord,type,lon,lat,STATUS_PTR))

/* Vtab Initialiser. */
#define astInitWcsMapVtab(vtab,name) astINVOKE(V,astInitWcsMapVtab_(vtab,name,STATUS_PTR))
/* Loader. */
#define astLoadWcsMap(mem,size,vtab,name,channel) \
astINVOKE(O,astLoadWcsMap_(mem,size,vtab,name,astCheckChannel(channel),STATUS_PTR))
#endif

/* Interfaces to public member functions. */
/* -------------------------------------- */
/* Here we make use of astCheckWcsMap to validate WcsMap pointers
   before use.  This provides a contextual error report if a pointer
   to the wrong sort of Object is supplied. */

#if defined(astCLASS)            /* Protected */

#define astWcsPrjType(ctype) astWcsPrjType_(ctype,STATUS_PTR)
#define astWcsPrjName(type) astWcsPrjName_(type,STATUS_PTR)
#define astWcsPrjDesc(type) astWcsPrjDesc_(type,STATUS_PTR)

#define astClearPV(this,i,j) \
astINVOKE(V,astClearPV_(astCheckWcsMap(this),i,j,STATUS_PTR))
#define astGetPV(this,i,j) \
astINVOKE(V,astGetPV_(astCheckWcsMap(this),i,j,STATUS_PTR))
#define astSetPV(this,i,j,par) \
astINVOKE(V,astSetPV_(astCheckWcsMap(this),i,j,par,STATUS_PTR))
#define astTestPV(this,i,j) \
astINVOKE(V,astTestPV_(astCheckWcsMap(this),i,j,STATUS_PTR))

#define astClearFITSProj(this) \
astINVOKE(V,astClearFITSProj_(astCheckWcsMap(this),STATUS_PTR))
#define astGetFITSProj(this) \
astINVOKE(V,astGetFITSProj_(astCheckWcsMap(this),STATUS_PTR))
#define astSetFITSProj(this,value) \
astINVOKE(V,astSetFITSProj_(astCheckWcsMap(this),value,STATUS_PTR))
#define astTestFITSProj(this) \
astINVOKE(V,astTestFITSProj_(astCheckWcsMap(this),STATUS_PTR))

#define astClearTPNTan(this) \
astINVOKE(V,astClearTPNTan_(astCheckWcsMap(this),STATUS_PTR))
#define astGetTPNTan(this) \
astINVOKE(V,astGetTPNTan_(astCheckWcsMap(this),STATUS_PTR))
#define astSetTPNTan(this,value) \
astINVOKE(V,astSetTPNTan_(astCheckWcsMap(this),value,STATUS_PTR))
#define astTestTPNTan(this) \
astINVOKE(V,astTestTPNTan_(astCheckWcsMap(this),STATUS_PTR))

#define astGetWcsType(this) \
astINVOKE(V,astGetWcsType_(astCheckWcsMap(this),STATUS_PTR))

#define astGetPVMax(this,i) \
astINVOKE(V,astGetPVMax_(astCheckWcsMap(this),i,STATUS_PTR))

#define astGetNatLat(this) \
astINVOKE(V,astGetNatLat_(astCheckWcsMap(this),STATUS_PTR))

#define astGetNatLon(this) \
astINVOKE(V,astGetNatLon_(astCheckWcsMap(this),STATUS_PTR))

#define astGetWcsAxis(this,index) \
astINVOKE(V,astGetWcsAxis_(astCheckWcsMap(this),index,STATUS_PTR))

#define astIsZenithal(this) \
astINVOKE(V,astIsZenithal_(astCheckWcsMap(this),STATUS_PTR))

#endif
#endif





