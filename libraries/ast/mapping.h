#if !defined( MAPPING_INCLUDED ) /* Include this file only once */
#define MAPPING_INCLUDED
/*
*++
*  Name:
*     mapping.h

*  Type:
*     C include file.

*  Purpose:
*     Define the interface to the Mapping class.

*  Invocation:
*     #include "mapping.h"

*  Description:
*     This include file defines the interface to the Mapping class and
*     provides the type definitions, function prototypes and macros, etc.
*     needed to use this class.
*
*     The Mapping class provides basic facilities for transforming a
*     set of points to give a new set of points. However, it does not
*     have a constructor function. This is because the class only
*     forms a template for deriving new classes which themselves
*     implement specific forms of coordinate transformation. They do
*     this by extending the protected astTransform method provided by
*     this class.

*  Inheritance:
*     The Mapping class inherits from the Object class.

*  Attributes Over-Ridden:
*     None.

*  New Attributes Defined:
*     Nin (integer)
*        A read-only attribute giving the number of input coordinate
*        values required per point by a Mapping (i.e. the number of
*        dimensions of the space in which input points reside).
*     Nout (integer)
*        A read-only attribute giving the number of output coordinate
*        values generated per point by a Mapping (i.e. the number of
*        dimensions of the space in which output points reside).
*     Invert (integer)
*        A boolean value (0 or 1) which controls which of a Mapping's
*        two possible coordinate transformations is considered the
*        "forward" transformation and which is the "inverse"
*        transformation. If this value is zero (the default), the
*        behaviour will be as defined when the Mapping was first
*        created.  If it is non-zero, the transformations will be
*        inter-changed, so that the Mapping displays the inverse of
*        its original behaviour.
*
*        Note that inverting the boolean sense of the Invert attribute
*        will cause the values of the Nin/Nout and
*        TranForward/TranInverse attributes to be interchanged.
*     Report (integer)
*        A boolean value (0 or 1) which controls whether to report
*        coordinate values when a Mapping is used to transform a set
*        of points. If this value is zero (the default), no report is
*        made. If it is non-zero, the coordinates of each point
*        (before and after transformation) are reported by writing
*        them to standard output.
*
*        This attribute is intended as an aid to debugging and to save
*        having to report values explicitly in simple programs.
*        Unlike other attributes, the value of the Report attribute is
*        not inherited when a Mapping is copied (its value is
*        initially undefined, and therefore defaults to zero, in any
*        copy).
*     TranForward (integer)
*        A read-only boolean value (0 or 1) which indicates whether a
*        Mapping is able to transform coordinates in the "forward"
*        direction (i.e. converting input coordinates into output
*        coordinates).
*     TranInverse (integer)
*        A read-only boolean value (0 or 1) which indicates whether a
*        Mapping is able to transform coordinates in the "inverse"
*        direction (i.e. converting output coordinates back into input
*        coordinates).

*  Methods Over-Ridden:
*     Public:
*        None.
*
*     Protected:
*        astClearAttrib
*           Clear an attribute value for a Mapping.
*        astGetAttrib
*           Get an attribute value for a Mapping.
*        astSetAttrib
*           Set an attribute value for a Mapping.
*        astTestAttrib
*           Test if an attribute value has been set for a Mapping.

*  New Methods Defined:
*     Public:
*        astInvert
*           Invert a Mapping.
*        astMapBox
*           Find a bounding box for a Mapping.
*        astSimplify
*           Simplify a Mapping.
*        astTran1
*           Transform 1-dimensional coordinates.
*        astTran2
*           Transform 2-dimensional coordinates.
*        astTranN
*           Transform N-dimensional coordinates.
*        astTranP (C only)
*           Transform N-dimensional coordinates held in separate arrays.
*
*     Protected:
*        astClearInvert
*           Clear the Invert attribute value for a Mapping.
*        astClearReport
*           Clear the Report attribute value for a Mapping.
*        astGetInvert
*           Get the Invert attribute value for a Mapping.
*        astGetNin
*           Get the number of input coordinates for a Mapping.
*        astGetNout
*           Get the number of output coordinates for a Mapping.
*        astGetReport
*           Get the Report attribute value for a Mapping.
*        astGetTranForward
*           Determine if a Mapping can perform a "forward" coordinate
*           transformation.
*        astGetTranInverse
*           Determine if a Mapping can perform an "inverse" coordinate
*           transformation.
*        astMapList
*           Decompose a Mapping into a sequence of simpler Mappings.
*        astMapMerge
*           Simplify a sequence of Mappings.
*        astReportPoints
*           Report the effect of transforming a set of points using a Mapping.
*        astSetInvert
*           Set the Invert attribute value for a Mapping.
*        astSetReport
*           Set the Report attribute value for a Mapping.
*        astTestInvert
*           Test if an Invert attribute value has been set for a Mapping.
*        astTestReport
*           Test if an Report attribute value has been set for a Mapping.
*        astTransform
*           Transform a set of points.

*  Other Class Functions:
*     Public:
*        astIsAMapping
*           Test class membership.
*
*     Protected:
*        astCheckMapping
*           Validate class membership.
*        astInitMapping
*           Initialise a Mapping.
*        astLoadMapping
*           Load a Mapping.

*  Macros:
*     None.

*  Type Definitions:
*     Public:
*        AstMapping
*           Mapping object type.
*
*     Protected:
*        AstMappingVtab
*           Mapping virtual function table type.

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
*     RFWS: R.F. Warren-Smith (Starlink)

*  History:
*     30-JAN-1996 (RFWS):
*        Original version.
*     12-JUL-1996 (RFWS):
*        Updated to support the external interface plus various other
*        additions.
*     12-DEC-1996 (RFWS):
*        Added the astMapList method.
*     13-DEC-1996 (RFWS):
*        Added the astMapMerge method.
*     13-DEC-1996 (RFWS):
*        Added the astSimplify method.
*     28-MAY-1998 (RFWS):
*        Added the astMapBox method.
*--
*/

/* Include files. */
/* ============== */
/* Interface definitions. */
/* ---------------------- */
#include "object.h"              /* Base Object class */
#include "pointset.h"            /* Sets of points/coordinates */
#include "channel.h"

/* C header files. */
/* --------------- */
#include <stddef.h>

/* Type Definitions. */
/* ================= */
/* Mapping structure. */
/* ------------------ */
/* This structure contains all information that is unique to each
   object in the class (e.g. its instance variables). */
typedef struct AstMapping {

/* Attributes inherited from the parent class. */
   AstObject object;             /* Parent class structure */

/* Attributes specific to objects in this class. */
   int invert;                   /* Mapping inverted? */
   int nin;                      /* Number of input coordinates */
   int nout;                     /* Number of output coordinates */
   int report;                   /* Report when converting coordinates? */
   int tran_forward;             /* Forward transformation defined? */
   int tran_inverse;             /* Inverse transformation defined? */
} AstMapping;

typedef int (* AstInterpolate)( int, const int *, const int *, const double *,
                                int, const int *, double *, int, double,
                                double * );
#define AST__NEAREST ( (AstInterpolate) 1 )
#define AST__LINEAR ( (AstInterpolate) NULL )

/* Virtual function table. */
/* ----------------------- */
/* This table contains all information that is the same for all
   objects in the class (e.g. pointers to its virtual functions). */
#if defined(astCLASS)            /* Protected */
typedef struct AstMappingVtab {

/* Properties (e.g. methods) inherited from the parent class. */
   AstObjectVtab object_vtab;    /* Parent class virtual function table */

/* Unique flag value to determine class membership. */
   int *check;                   /* Check value */

/* Properties (e.g. methods) specific to this class. */
   AstMapping *(* Simplify)( AstMapping * );
   AstPointSet *(* Transform)( AstMapping *, AstPointSet *, int, AstPointSet * );
   int (* GetInvert)( AstMapping * );
   int (* GetNin)( AstMapping * );
   int (* GetNout)( AstMapping * );
   int (* GetReport)( AstMapping * );
   int (* GetTranForward)( AstMapping * );
   int (* GetTranInverse)( AstMapping * );
   int (* MapMerge)( AstMapping *, int, int, int *, AstMapping ***, int ** );
   int (* Resample)( AstMapping *, int, const int *, const int *, const double *, AstInterpolate, double, int, double, int, const int *, const int *, const int *, const int *, double * );
   int (* TestInvert)( AstMapping * );
   int (* TestReport)( AstMapping * );
   void (* ClearInvert)( AstMapping * );
   void (* ClearReport)( AstMapping * );
   void (* Invert)( struct AstMapping * );
   void (* MapBox)( AstMapping *, const double [], const double [], int, int, double *, double *, double [], double [] );
   void (* MapList)( AstMapping *, int, int, int *, AstMapping ***, int ** );
   void (* ReportPoints)( AstMapping *, int, AstPointSet *, AstPointSet * );
   void (* SetInvert)( AstMapping *, int );
   void (* SetReport)( AstMapping *, int );
   void (* Tran1)( AstMapping *, int, const double [], int, double [] );
   void (* Tran2)( AstMapping *, int, const double [], const double [], int, double [], double [] );
   void (* TranN)( AstMapping *, int, int, int, const double (*)[], int, int, int, double (*)[] );
   void (* TranP)( AstMapping *, int, int, const double *[], int, int, double *[] );
} AstMappingVtab;
#endif

/* Function prototypes. */
/* ==================== */
/* Prototypes for standard class functions. */
/* ---------------------------------------- */
astPROTO_CHECK(Mapping)          /* Check class membership */
astPROTO_ISA(Mapping)            /* Test class membership */

/* NB. There is no constructor function for this class. */

#if defined(astCLASS)            /* Protected */

/* Initialiser. */
AstMapping *astInitMapping_( void *, size_t, int, AstMappingVtab *,
                             const char *, int, int, int, int );

/* Loader. */
AstMapping *astLoadMapping_( void *, size_t, int, AstMappingVtab *,
                             const char *, AstChannel *channel );
#endif

/* Prototypes for member functions. */
/* -------------------------------- */
AstMapping *astSimplify_( AstMapping * );
int astResample_( AstMapping *, int, const int *, const int *, const double *, AstInterpolate, double, int, double, int, const int *, const int *, const int *, const int *, double * );
void astInvert_( AstMapping * );
void astTran1_( AstMapping *, int, const double [], int, double [] );
void astTran2_( AstMapping *, int, const double [], const double [], int, double [], double [] );
void astTranN_( AstMapping *, int, int, int, const double (*)[], int, int, int,double (*)[] );
void astTranP_( AstMapping *, int, int, const double *[], int, int, double *[] );

#if defined(astCLASS)            /* Protected */
void astMapBox_( AstMapping *, const double [], const double [], int, int, double *, double *, double [], double [] );
#else
void astMapBoxId_( AstMapping *, const double [], const double [], int, int, double *, double *, double [], double [] );
#endif

#if defined(astCLASS)            /* Protected */
AstPointSet *astTransform_( AstMapping *, AstPointSet *, int, AstPointSet * );
int astGetInvert_( AstMapping * );
int astGetNin_( AstMapping * );
int astGetNout_( AstMapping * );
int astGetReport_( AstMapping * );
int astGetTranForward_( AstMapping * );
int astGetTranInverse_( AstMapping * );
int astMapMerge_( AstMapping *, int, int, int *, AstMapping ***, int ** );
int astTestInvert_( AstMapping * );
int astTestReport_( AstMapping * );
void astClearInvert_( AstMapping * );
void astClearReport_( AstMapping * );
void astMapList_( AstMapping *, int, int, int *, AstMapping ***, int ** );
void astReportPoints_( AstMapping *, int, AstPointSet *, AstPointSet * );
void astSetInvert_( AstMapping *, int );
void astSetReport_( AstMapping *, int );
#endif

/* Function interfaces. */
/* ==================== */
/* These macros are wrap-ups for the functions defined by this class to make
   them easier to invoke (e.g. to avoid type mis-matches when passing pointers
   to objects from derived classes). */

/* Interfaces to standard class functions. */
/* --------------------------------------- */
/* Some of these functions provide validation, so we cannot use them to
   validate their own arguments. We must use a cast when passing object
   pointers (so that they can accept objects from derived classes). */

/* Check class membership. */
#define astCheckMapping(this) astINVOKE_CHECK(Mapping,this)

/* Test class membership. */
#define astIsAMapping(this) astINVOKE_ISA(Mapping,this)

/* NB. There is no constructor function for this class. */

#if defined(astCLASS)            /* Protected */

/* Initialiser. */
#define astInitMapping(mem,size,init,vtab,name,nin,nout,tran_forward,tran_inverse) \
astINVOKE(O,astInitMapping_(mem,size,init,vtab,name,nin,nout,tran_forward,tran_inverse))

/* Loader. */
#define astLoadMapping(mem,size,init,vtab,name,channel) \
astINVOKE(O,astLoadMapping_(mem,size,init,vtab,name,astCheckChannel(channel)))
#endif

/* Interfaces to member functions. */
/* ------------------------------- */
/* Here we make use of astCheckMapping (et al.) to validate Mapping
   pointers before use. This provides a contextual error report if a
   pointer to the wrong sort of object is supplied. */
#define astInvert(this) \
astINVOKE(V,astInvert_(astCheckMapping(this)))
#define astResample(this,ndim_in,lbnd_in,ubnd_in,in,method,acc,usebad,badflag,ndim_out,lbnd_out,ubnd_out,lbnd,ubnd,out) \
astINVOKE(V,astResample_(astCheckMapping(this),ndim_in,lbnd_in,ubnd_in,in,method, acc,usebad,badflag,ndim_out,lbnd_out,ubnd_out,lbnd,ubnd,out))
#define astSimplify(this) astINVOKE(O,astSimplify_(astCheckMapping(this)))
#define astTran1(this,npoint,xin,forward,xout) \
astINVOKE(V,astTran1_(astCheckMapping(this),npoint,xin,forward,xout))
#define astTran2(this,npoint,xin,yin,forward,xout,yout) \
astINVOKE(V,astTran2_(astCheckMapping(this),npoint,xin,yin,forward,xout,yout))
#define astTranN(this,npoint,ncoord_in,indim,in,forward,ncoord_out,outdim,out) \
astINVOKE(V,astTranN_(astCheckMapping(this),npoint,ncoord_in,indim,in,forward,ncoord_out,outdim,out))
#define astTranP(this,npoint,ncoord_in,ptr_in,forward,ncoord_out,ptr_out) \
astINVOKE(V,astTranP_(astCheckMapping(this),npoint,ncoord_in,ptr_in,forward,ncoord_out,ptr_out))

#if defined(astCLASS)            /* Protected */
#define astMapBox(this,lbnd_in,ubnd_in,forward,coord_out,lbnd_out,ubnd_out,xl,xu) \
astINVOKE(V,astMapBox_(astCheckMapping(this),lbnd_in,ubnd_in,forward,coord_out,lbnd_out,ubnd_out,xl,xu))
#else
#define astMapBox(this,lbnd_in,ubnd_in,forward,coord_out,lbnd_out,ubnd_out,xl,xu) \
astINVOKE(V,astMapBoxId_(astCheckMapping(this),lbnd_in,ubnd_in,forward,coord_out,lbnd_out,ubnd_out,xl,xu))
#endif

#if defined(astCLASS)            /* Protected */
#define astClearInvert(this) \
astINVOKE(V,astClearInvert_(astCheckMapping(this)))
#define astClearReport(this) \
astINVOKE(V,astClearReport_(astCheckMapping(this)))
#define astGetInvert(this) \
astINVOKE(V,astGetInvert_(astCheckMapping(this)))
#define astGetNin(this) \
astINVOKE(V,astGetNin_(astCheckMapping(this)))
#define astGetNout(this) \
astINVOKE(V,astGetNout_(astCheckMapping(this)))
#define astGetReport(this) \
astINVOKE(V,astGetReport_(astCheckMapping(this)))
#define astGetTranForward(this) \
astINVOKE(V,astGetTranForward_(astCheckMapping(this)))
#define astGetTranInverse(this) \
astINVOKE(V,astGetTranInverse_(astCheckMapping(this)))
#define astMapList(this,series,invert,nmap,map_list,invert_list) \
astINVOKE(V,astMapList_(this,series,invert,nmap,map_list,invert_list))
#define astMapMerge(this,where,series,nmap,map_list,invert_list) \
astINVOKE(V,astMapMerge_(astCheckMapping(this),where,series,nmap,map_list,invert_list))
#define astReportPoints(this,forward,in_points,out_points) \
astINVOKE(V,astReportPoints_(astCheckMapping(this),forward,astCheckPointSet(in_points),astCheckPointSet(out_points)))
#define astSetInvert(this,value) \
astINVOKE(V,astSetInvert_(astCheckMapping(this),value))
#define astSetReport(this,value) \
astINVOKE(V,astSetReport_(astCheckMapping(this),value))
#define astTestInvert(this) \
astINVOKE(V,astTestInvert_(astCheckMapping(this)))
#define astTestReport(this) \
astINVOKE(V,astTestReport_(astCheckMapping(this)))

/* Since a NULL PointSet pointer is acceptable here, we must omit the argument
   checking in that case. (But unfortunately, "out" then gets evaluated
   twice - this is unlikely to matter, but is there a better way?) */
#define astTransform(this,in,forward,out) \
astINVOKE(O,astTransform_(astCheckMapping(this),astCheckPointSet(in),forward,(out)?astCheckPointSet(out):NULL))
#endif
#endif
