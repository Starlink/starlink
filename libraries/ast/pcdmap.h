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
*     Copyright (C) 1999 Central Laboratory of the Research Councils

*  Authors:
*     DSB: David S. Berry (Starlink)

*  History:
*     18-MAY-1999 (DSB):
*        Original version.
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

/* Unique flag value to determine class membership. */
   int *check;                   /* Check value */

/* Properties (e.g. methods) specific to this class. */
   double (*GetDisco)( AstPcdMap * );
   int (* TestDisco)( AstPcdMap * );
   void (* ClearDisco)( AstPcdMap * );
   void (* SetDisco)( AstPcdMap *, double );
   double (*GetPcdCen)( AstPcdMap *, int );
   int (* TestPcdCen)( AstPcdMap *, int );
   void (* ClearPcdCen)( AstPcdMap *, int );
   void (* SetPcdCen)( AstPcdMap *, int, double );
} AstPcdMapVtab;
#endif

/* Function prototypes. */
/* ==================== */
/* Prototypes for standard class functions. */
/* ---------------------------------------- */
astPROTO_CHECK(PcdMap)          /* Check class membership */
astPROTO_ISA(PcdMap)            /* Test class membership */

/* Constructor. */
#if defined(astCLASS)            /* Protected. */
AstPcdMap *astPcdMap_( double, const double [2], const char *, ... );
#else
AstPcdMap *astPcdMapId_( double, const double [2], const char *, ... );
#endif

#if defined(astCLASS)            /* Protected */

/* Initialiser. */
AstPcdMap *astInitPcdMap_( void *, size_t, int, AstPcdMapVtab *,
                           const char *, double, const double [2] );

/* Loader. */
AstPcdMap *astLoadPcdMap_( void *, size_t, int, AstPcdMapVtab *,
                           const char *, AstChannel * );
#endif

/* Prototypes for member functions. */
/* -------------------------------- */
# if defined(astCLASS)           /* Protected */
double astGetDisco_( AstPcdMap * );
int astTestDisco_( AstPcdMap * );
void astClearDisco_( AstPcdMap * );
void astSetDisco_( AstPcdMap *, double );
double astGetPcdCen_( AstPcdMap *, int );
int astTestPcdCen_( AstPcdMap *, int );
void astClearPcdCen_( AstPcdMap *, int );
void astSetPcdCen_( AstPcdMap *, int, double );
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
#define astCheckPcdMap(this) astINVOKE_CHECK(PcdMap,this)

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
astINVOKE(O,astInitPcdMap_(mem,size,init,vtab,name,disco,pcdcen))

/* Loader. */
#define astLoadPcdMap(mem,size,init,vtab,name,channel) \
astINVOKE(O,astLoadPcdMap_(mem,size,init,vtab,name,astCheckChannel(channel)))
#endif

/* Interfaces to public member functions. */
/* -------------------------------------- */
/* Here we make use of astCheckPcdMap to validate PcdMap pointers
   before use.  This provides a contextual error report if a pointer
   to the wrong sort of Object is supplied. */

#if defined(astCLASS)            /* Protected */
#define astClearDisco(this) astINVOKE(V,astClearDisco_(astCheckPcdMap(this)))
#define astGetDisco(this) astINVOKE(V,astGetDisco_(astCheckPcdMap(this)))
#define astSetDisco(this,value) \
astINVOKE(V,astSetDisco_(astCheckPcdMap(this),value))
#define astTestDisco(this) astINVOKE(V,astTestDisco_(astCheckPcdMap(this)))

#define astClearPcdCen(this,axis) \
astINVOKE(V,astClearPcdCen_(astCheckPcdMap(this),axis))
#define astGetPcdCen(this,axis) \
astINVOKE(V,astGetPcdCen_(astCheckPcdMap(this),axis))
#define astSetPcdCen(this,axis,value) \
astINVOKE(V,astSetPcdCen_(astCheckPcdMap(this),axis,value))
#define astTestPcdCen(this,axis) \
astINVOKE(V,astTestPcdCen_(astCheckPcdMap(this),axis))

#endif
#endif
