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
*        if new values are assigned to the projection parameters given by 
*        attribute ProjP.
*     ProjP(i) (double)
*        This attribute gives the parameter values used by a FITS WCS 
*        projection. The index value included in the attribute name selects
*        a particular parameter, and should be an integer in the range 0 to 9. 
*        These values correspond to FITS keywords PROJP0 to PROJP9. They will 
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
*        astClearProjP
*           Clear a ProjP attribute value for a WcsMap.
*        astGetNatLat
*           Get the NatLat attribute value for a WcsMap.
*        astGetProjP
*           Get a ProjP attribute value for a WcsMap.
*        astGetWcsAxis
*           Get a WcsAxis attribute value for a WcsMap.
*        astGetWcsType
*           Get the WcsType attribute value for a WcsMap.
*        astSetProjP
*           Set a ProjP attribute value for a WcsMap.
*        astTestProjP
*           Test if a ProjP attribute value has been set for a WcsMap.
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
*           An integer identifier for the FITS GLS projection. 
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
*     <COPYRIGHT_STATEMENT>

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
#define   AST__AZP    1          /* The FITS AZP projection */ 
#define   AST__TAN    2          /* The FITS TAN projection */ 
#define   AST__SIN    3          /* The FITS SIN projection */ 
#define   AST__STG    4          /* The FITS STG projection */ 
#define   AST__ARC    5          /* The FITS ARC projection */ 
#define   AST__ZPN    6          /* The FITS ZPN projection */ 
#define   AST__ZEA    7          /* The FITS ZEA projection */ 
#define   AST__AIR    8          /* The FITS AIR projection */ 
#define   AST__CYP    9          /* The FITS CYP projection */ 
#define   AST__CAR    10         /* The FITS CAR projection */ 
#define   AST__MER    11         /* The FITS MER projection */ 
#define   AST__CEA    12         /* The FITS CEA projection */ 
#define   AST__COP    13         /* The FITS COP projection */ 
#define   AST__COD    14         /* The FITS COD projection */ 
#define   AST__COE    15         /* The FITS COE projection */ 
#define   AST__COO    16         /* The FITS COO projection */ 
#define   AST__BON    17         /* The FITS BON projection */ 
#define   AST__PCO    18         /* The FITS PCO projection */ 
#define   AST__GLS    19         /* The FITS GLS projection */ 
#define   AST__PAR    20         /* The FITS PAR projection */ 
#define   AST__AIT    21         /* The FITS AIT projection */ 
#define   AST__MOL    22         /* The FITS MOL projection */ 
#define   AST__CSC    23         /* The FITS CSC projection */ 
#define   AST__QSC    24         /* The FITS QSC projection */ 
#define   AST__NCP    25         /* The AIPS NCP projection */
#define   AST__TSC    26         /* The FITS TSC projection */
#define   AST__WCSBAD 27          /* A bad projection type */

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
   double natlat;                /* Native latitude of reference point */
   int type;                     /* Projection type */
   int wcsaxis[2];               /* Indices of lon and lat. axes */
   double projp[ AST__WCSMX ];   /* Projection parameters */
   struct prjprm params;         /* WCS structure holding projection
                                    parameters, etc. Defined in proj.h */

} AstWcsMap;

/* Virtual function table. */
/* ----------------------- */
/* This table contains all information that is the same for all
   objects in the class (e.g. pointers to its virtual functions). */
#if defined(astCLASS)            /* Protected */
typedef struct AstWcsMapVtab {

/* Properties (e.g. methods) inherited from the parent class. */
   AstMappingVtab mapping_vtab;  /* Parent class virtual function table */

/* Unique flag value to determine class membership. */
   int *check;                   /* Check value */

/* Properties (e.g. methods) specific to this class. */
   double (* GetNatLat)( AstWcsMap * );
   double (* GetProjP)( AstWcsMap *, int );
   int (* GetWcsAxis)( AstWcsMap *, int );
   int (* GetWcsType)( AstWcsMap * );
   int (* TestProjP)( AstWcsMap *, int );
   void (* ClearProjP)( AstWcsMap *, int );
   void (* SetProjP)( AstWcsMap *, int, double );

} AstWcsMapVtab;
#endif

/* Function prototypes. */
/* ==================== */
/* Prototypes for standard class functions. */
/* ---------------------------------------- */
astPROTO_CHECK(WcsMap)          /* Check class membership */
astPROTO_ISA(WcsMap)            /* Test class membership */

/* Constructor. */
#if defined(astCLASS)            /* Protected. */
AstWcsMap *astWcsMap_( int, int, int, int, const char *, ... );
#else
AstWcsMap *astWcsMapId_( int, int, int, int, const char *, ... );
#endif

#if defined(astCLASS)            /* Protected */

/* Initialiser. */
AstWcsMap *astInitWcsMap_( void *, size_t, int, AstWcsMapVtab *,
                           const char *, int, int, int, int );

/* Loader. */
AstWcsMap *astLoadWcsMap_( void *, size_t, int, AstWcsMapVtab *,
                           const char *, AstChannel * );
#endif

/* Prototypes for member functions. */
/* -------------------------------- */
# if defined(astCLASS)           /* Protected */
   const char *PrjDesc_( int );
   const char *PrjName_( int );
   double astGetNatLat_( AstWcsMap * );
   double astGetProjP_( AstWcsMap *, int );
   int PrjType_( const char * );
   int astGetWcsAxis_( AstWcsMap *, int );
   int astGetWcsType_( AstWcsMap * );
   int astTestProjP_( AstWcsMap *, int );
   void astClearProjP_( AstWcsMap *, int );
   void astSetProjP_( AstWcsMap *, int, double );
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
#define astCheckWcsMap(this) astINVOKE_CHECK(WcsMap,this)

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
astINVOKE(O,astInitWcsMap_(mem,size,init,vtab,name,ncoord,type,lon,lat))

/* Loader. */
#define astLoadWcsMap(mem,size,init,vtab,name,channel) \
astINVOKE(O,astLoadWcsMap_(mem,size,init,vtab,name,astCheckChannel(channel)))
#endif

/* Interfaces to public member functions. */
/* -------------------------------------- */
/* Here we make use of astCheckWcsMap to validate WcsMap pointers
   before use.  This provides a contextual error report if a pointer
   to the wrong sort of Object is supplied. */

#if defined(astCLASS)            /* Protected */

#define astWcsPrjType PrjType_
#define astWcsPrjName PrjName_
#define astWcsPrjDesc PrjDesc_

#define astClearProjP(this,index) \
astINVOKE(V,astClearProjP_(astCheckWcsMap(this),index))
#define astGetProjP(this,index) \
astINVOKE(V,astGetProjP_(astCheckWcsMap(this),index))
#define astSetProjP(this,index,par) \
astINVOKE(V,astSetProjP_(astCheckWcsMap(this),index,par))
#define astTestProjP(this,index) \
astINVOKE(V,astTestProjP_(astCheckWcsMap(this),index))

#define astGetWcsType(this) \
astINVOKE(V,astGetWcsType_(astCheckWcsMap(this)))

#define astGetNatLat(this) \
astINVOKE(V,astGetNatLat_(astCheckWcsMap(this)))

#define astGetWcsAxis(this,index) \
astINVOKE(V,astGetWcsAxis_(astCheckWcsMap(this),index))

#endif
#endif
