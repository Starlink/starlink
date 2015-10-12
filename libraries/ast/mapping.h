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
*     set of points to give a new set of points and for resampling
*     grids of data. However, it does not have a constructor
*     function. This is because the class only forms a template for
*     deriving new classes which themselves implement specific forms
*     of coordinate transformation. They do this by extending the
*     protected astTransform method provided by this class.

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
*     IsSimple (boolean)
*        A read-only attribute indicating if the Mapping has been
*        simpified.
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
*        astDecompose
*           Decompose a Mapping into two component Mappings.
*        astInvert
*           Invert a Mapping.
*        astLinearApprox
*           Form a linear approximation to a Mapping
*        astMapBox
*           Find a bounding box for a Mapping.
*        astQuadApprox
*           Form a quadratic approximation to a Mapping
*        astRate
*           Find rate of change of a Mapping output
*        astRebin<X>
*           Rebin a region of a data grid.
*        astRebinSeq<X>
*           Rebin a region of a sequence of data grids.
*        astResample<X>
*           Resample a region of a data grid.
*        astSimplify
*           Simplify a Mapping.
*        astTran1
*           Transform 1-dimensional coordinates.
*        astTran2
*           Transform 2-dimensional coordinates.
*        astTranGrid
*           Transform an N-dimensional regular grid of positions.
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
*        astGetIsSimple
*           Get the IsSimple attribute.
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
*        astMapSplit
*           Select a subset of Mapping inputs.
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
*        astInitMappingVtab
*           Initialise the virtual function table for the Mapping class.
*        astLoadMapping
*           Load a Mapping.

*  Macros:
*     Public:
*        AST__BLOCKAVE
*           Block averaging interpolation.
*        AST__GAUSS
*           Use exp(-k*x*x) spreading.
*        AST__LINEAR
*           Simple linear interpolation.
*        AST__NEAREST
*           Use nearest pixel centre.
*        AST__SINC
*           Use sinc(pi*x) interpolation.
*        AST__SINCCOS
*           Use sinc(pi*x)*cos(k*pi*x) interpolation.
*        AST__SINCGAUSS
*           Use sinc(pi*x)*exp(-k*x*x) interpolation.
*        AST__SINCSINC
*           Use sinc(pi*x)*sinc(k*pi*x) interpolation.
*        AST__SOMB
*           Use somb(pi*x) interpolation.
*        AST__SOMBCOS
*           Use somb(pi*x)*cos(k*pi*x) interpolation.
*        AST__UINTERP
*           Use general user-defined sub-pixel interpolation algorithm.
*        AST__UKERN1
*           Use user-defined 1-d interpolation kernel.
*        AST__URESAMP1, 2, 3 & 4
*           Flags reserved for user-defined purposes.
*        AST__USEBAD
*           Recognise bad pixels?
*        AST__CONSERVEFLUX
*           Conserve flux in astResample?
*        AST__REBININIT
*           Initialise a new sequnece of calls to astRebinSeq<X>
*        AST__REBINEND
*           End a sequnece of calls to astRebinSeq<X>
*        AST__NOBAD
*           Leave bad output pixels unchanged in calls to astResample<X>
*        AST__USEVAR
*           Use variance arrays?

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
*     MBT: Mark Taylor (Starlink)
*     DSB: David S. Berry (Starlink)

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
*     12-NOV-1998 (RFWS):
*        Added astResample<X> and associated code.
*     24-NOV-2000 (MBT):
*        Added AST__BLOCKAVE interpolation scheme.
*     9-JAN-2001 (DSB):
*        Changed in and out arguments for TranN from type "double (*)[]"
*        to "double *".
*     8-JAN-2003 (DSB):
*        Added protected astInitMappingVtab method.
*     10-JUL-2003 (DSB):
*        Added method astRate.
*     20-SEP-2004 (DSB):
*        Added method astLinearApprox.
*     30-JUN-2005 (DSB):
*        Added method astRebin
*     1-SEP-2005 (DSB):
*        Added method astRebinSeq
*     31-JAN-2006 (DSB):
*        Added IsSimple attribute.
*     2-MAR-2006 (DSB):
*        Use HAVE_LONG_DOUBLE in place of AST_LONG_DOUBLE
*     8-MAR-2006 (DSB):
*        Add astTranGrid.
*     5-MAY-2009 (DSB):
*        Add astRemoveRegions.
*     26-FEB-2010 (DSB):
*        Added method astQuadApprox.
*--
*/

/* Include files. */
/* ============== */

/* Configuration results */
/* --------------------- */
#if HAVE_CONFIG_H
#include <config.h>
#endif

/* Interface definitions. */
/* ---------------------- */
#include "object.h"              /* Base Object class */
#include "pointset.h"            /* Sets of points/coordinates */
#include "channel.h"             /* I/O channels */

/* C header files. */
/* --------------- */
#include <stddef.h>
#include <stdint.h>

/* Macros. */
/* ======= */

/* Sizes of global arrays */

#if defined(astCLASS) || defined(astFORTRAN77)
#define STATUS_PTR status
#else
#define STATUS_PTR astGetStatusPtr
#endif
#define AST__MAPPING_GETATTRIB_BUFF_LEN 50
#define AST__MAPPING_RATEFUN_MAX_CACHE  5

/* Resampling flags. */
/* ----------------- */
/* These macros define flag values which may be passed to
   astResample<X> (via the "flags" argument) to provide control over
   resampling operations. */
#define AST__URESAMP1 (1)        /* Flags reserved for user-defined purposes */
#define AST__URESAMP2 (2)
#define AST__URESAMP3 (4)
#define AST__URESAMP4 (8)
#define AST__USEVAR (16)         /* Use variance arrays? */
#define AST__USEBAD (32)         /* Recognise bad pixels? */
#define AST__CONSERVEFLUX (64)   /* Conserve flux? */
#define AST__REBININIT (128)     /* Initialise a new sequence of calls to astRebinSeq? */
#define AST__REBINEND (256)      /* End a sequence of calls to astRebinSeq? */
#define AST__GENVAR (512)        /* Generate output variances when rebinning? */
#define AST__VARWGT (1024)       /* Use input variances as weights? */
#define AST__NOBAD (2048)        /* Leave bad output values unchanged? */
#define AST__DISVAR (4096)       /* Generate distribution (not mean) variance? */
#define AST__NONORM (8192)       /* No normalisation required at end? */

/* These macros identify standard sub-pixel interpolation algorithms
   for use by astResample<X>. They are used by giving the macro's
   value for the "interp" argument. */
#define AST__UKERN1 (1)          /* User-supplied 1-d interpolation kernel */
#if 0                            /* Not yet implemented */
#define AST__UKERNN (2)          /* User-supplied n-d interpolation kernel */
#endif
#define AST__UINTERP (3)         /* User-supplied interpolation function */
#define AST__NEAREST (4)         /* Use pixel with nearest centre */
#define AST__LINEAR (5)          /* Simple linear interpolation */
#define AST__SINC (6)            /* sinc(pi*x) interpolation */
#define AST__SINCSINC (7)        /* sinc(pi*x)*sinc(k*pi*x) interpolation */
#define AST__SINCCOS (8)         /* sinc(pi*x)*cos(k*pi*x) interpolation */
#define AST__SINCGAUSS (9)       /* sinc(pi*x)*exp(-k*x*x) interpolation */
#define AST__BLOCKAVE (10)       /* Block averaging interpolation */
#define AST__GAUSS (11)          /* exp(-k*x*x) spreading */
#define AST__SOMB (12)           /* somp(pi*x) interpolation */
#define AST__SOMBCOS (13)        /* somp(pi*x)*cos(k*pi*x) interpolation */

/* 64 bit types */
#if HAVE_INT64_T && HAVE_UINT64_T
#include <stdint.h>
typedef int64_t INT_BIG;
typedef uint64_t UINT_BIG;

#elif SIZEOF_LONG == 8
typedef long int INT_BIG;
typedef unsigned long int UINT_BIG;

#elif SIZEOF_LONG_LONG == 8
typedef long long int INT_BIG;
typedef unsigned long long int UINT_BIG;

#else
#define INT_BIG  "no int64_t type available"
#define UINT_BIG "no uint64_t type available"
#endif

/* Flags defining the meaning of each bit in the "flags" field of the
   Mapping structure. */
#if defined(astCLASS)         /* Protected */
#define AST__ISSIMPLE_FLAG 1  /* Mapping has been simplified */
#define AST__FROZEN_FLAG 2    /* Mapping cannot be nominated for simplification */
#endif


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
   char invert;                   /* Mapping inverted? */
   char flags;                    /* Bit-wise flags describing the Mapping */
   int nin;                       /* Number of input coordinates */
   int nout;                      /* Number of output coordinates */
   char report;                   /* Report when converting coordinates? */
   char tran_forward;             /* Forward transformation defined? */
   char tran_inverse;             /* Inverse transformation defined? */
} AstMapping;

/* Virtual function table. */
/* ----------------------- */
/* This table contains all information that is the same for all
   objects in the class (e.g. pointers to its virtual functions). */
#if defined(astCLASS)            /* Protected */
typedef struct AstMappingVtab {

/* Properties (e.g. methods) inherited from the parent class. */
   AstObjectVtab object_vtab;    /* Parent class virtual function table */

/* A Unique identifier to determine class membership. */
   AstClassIdentifier id;

/* Properties (e.g. methods) specific to this class. */
   AstMapping *(* RemoveRegions)( AstMapping *, int * );
   AstMapping *(* Simplify)( AstMapping *, int * );
   AstPointSet *(* Transform)( AstMapping *, AstPointSet *, int, AstPointSet *, int * );
   double (* Rate)( AstMapping *, double *, int, int, int * );
   int (* DoNotSimplify)( AstMapping *, int * );
   int (* GetInvert)( AstMapping *, int * );
   int (* GetIsSimple)( AstMapping *, int * );
   int (* GetNin)( AstMapping *, int * );
   int (* GetNout)( AstMapping *, int * );
   int (* GetReport)( AstMapping *, int * );
   int (* GetTranForward)( AstMapping *, int * );
   int (* GetTranInverse)( AstMapping *, int * );
   int (* GetIsLinear)( AstMapping *, int * );
   int (* LinearApprox)( AstMapping *, const double *, const double *, double, double *, int * );
   int (* MapMerge)( AstMapping *, int, int, int *, AstMapping ***, int **, int * );
   int (* QuadApprox)( AstMapping *, const double[2], const double[2], int, int, double *, double *, int * );
   int (* TestInvert)( AstMapping *, int * );
   int (* TestReport)( AstMapping *, int * );
   void (* ClearInvert)( AstMapping *, int * );
   void (* ClearReport)( AstMapping *, int * );
   void (* Decompose)( AstMapping *, AstMapping **, AstMapping **, int *, int *, int *, int * );
   void (* Invert)( struct AstMapping *, int * );
   void (* MapBox)( AstMapping *, const double [], const double [], int, int, double *, double *, double [], double [], int * );
   int (* MapList)( AstMapping *, int, int, int *, AstMapping ***, int **, int * );
   int *(* MapSplit)( AstMapping *, int, const int *, AstMapping **, int * );
   void (* ReportPoints)( AstMapping *, int, AstPointSet *, AstPointSet *, int * );
   void (* SetInvert)( AstMapping *, int, int * );
   void (* SetReport)( AstMapping *, int, int * );
   void (* Tran1)( AstMapping *, int, const double [], int, double [], int * );
   void (* Tran2)( AstMapping *, int, const double [], const double [], int, double [], double [], int * );
   void (* TranGrid)( AstMapping *, int, const int[], const int[], double, int, int, int, int, double *, int * );
   void (* TranN)( AstMapping *, int, int, int, const double *, int, int, int, double *, int * );
   void (* TranP)( AstMapping *, int, int, const double *[], int, int, double *[], int * );

#define DECLARE_GENERIC_ALL(X,Xtype) \
   int (* Resample##X)( AstMapping *, int, const int [], const int [], \
                        const Xtype [], const Xtype [], int, \
                        void (*)( void ), const double [], int, double, int, \
                        Xtype, int, const int [], const int [], \
                        const int [], const int [], Xtype [], Xtype [], int * ); \

DECLARE_GENERIC_ALL(B,signed char)
DECLARE_GENERIC_ALL(D,double)
DECLARE_GENERIC_ALL(F,float)
DECLARE_GENERIC_ALL(I,int)
DECLARE_GENERIC_ALL(K,INT_BIG)
DECLARE_GENERIC_ALL(L,long int)
DECLARE_GENERIC_ALL(S,short int)
DECLARE_GENERIC_ALL(UB,unsigned char)
DECLARE_GENERIC_ALL(UI,unsigned int)
DECLARE_GENERIC_ALL(UK,UINT_BIG)
DECLARE_GENERIC_ALL(UL,unsigned long int)
DECLARE_GENERIC_ALL(US,unsigned short int)

#if HAVE_LONG_DOUBLE     /* Not normally implemented */
DECLARE_GENERIC_ALL(LD,long double)
#endif

#undef DECLARE_GENERIC_ALL

#define DECLARE_GENERIC_DFI(X,Xtype) \
   void (* Rebin##X)( AstMapping *, double, int, const int [], const int [], \
                      const Xtype [], const Xtype [], int, const double [], int, \
                      double, int, Xtype, int, const int [], const int [], \
                      const int [], const int [], Xtype [], Xtype [], int * ); \
   void (* RebinSeq##X)( AstMapping *, double, int, const int [], const int [], \
                         const Xtype [], const Xtype [], int, const double [], \
                         int, double, int, Xtype, int, const int [], \
                         const int [], const int [], const int [], Xtype [], \
                         Xtype [], double [], int64_t *, int * );

DECLARE_GENERIC_DFI(D,double)
DECLARE_GENERIC_DFI(F,float)
DECLARE_GENERIC_DFI(I,int)
DECLARE_GENERIC_DFI(B,signed char)
DECLARE_GENERIC_DFI(UB,unsigned char)

#if HAVE_LONG_DOUBLE     /* Not normally implemented */
DECLARE_GENERIC_DFI(LD,long double)
#endif

#undef DECLARE_GENERIC_DFI

} AstMappingVtab;


#if defined(THREAD_SAFE)

/* Define a structure holding all data items that are global within this
   class. */
typedef struct AstMappingGlobals {
   AstMappingVtab Class_Vtab;
   int Class_Init;
   char GetAttrib_Buff[ AST__MAPPING_GETATTRIB_BUFF_LEN + 1 ];
   AstMapping *Unsimplified_Mapping;
   int Rate_Disabled;
   AstPointSet *RateFun_Pset1_Cache[ AST__MAPPING_RATEFUN_MAX_CACHE ];
   AstPointSet *RateFun_Pset2_Cache[ AST__MAPPING_RATEFUN_MAX_CACHE ];
   int RateFun_Next_Slot;
   int RateFun_Pset_Size[ AST__MAPPING_RATEFUN_MAX_CACHE ];
} AstMappingGlobals;

#endif
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
                             const char *, int, int, int, int, int * );

/* Vtab initialiser. */
void astInitMappingVtab_( AstMappingVtab *, const char *, int * );

/* Loader. */
AstMapping *astLoadMapping_( void *, size_t, AstMappingVtab *,
                             const char *, AstChannel *channel, int * );

/* Thread-safe initialiser for all global data used by this module. */
#if defined(THREAD_SAFE)
void astInitMappingGlobals_( AstMappingGlobals * );
#endif
#endif

/* Prototypes for member functions. */
/* -------------------------------- */
#define PROTO_GENERIC_ALL(X,Xtype) \
   int astResample##X##_( AstMapping *, int, const int [], const int [], \
                        const Xtype [], const Xtype [], int, \
                        void (*)( void ), const double [], int, double, int, \
                        Xtype, int, const int [], const int [], \
                        const int [], const int [], Xtype [], Xtype [], int * ); \

PROTO_GENERIC_ALL(B,signed char)
PROTO_GENERIC_ALL(D,double)
PROTO_GENERIC_ALL(F,float)
PROTO_GENERIC_ALL(I,int)
PROTO_GENERIC_ALL(K,INT_BIG)
PROTO_GENERIC_ALL(L,long int)
PROTO_GENERIC_ALL(S,short int)
PROTO_GENERIC_ALL(UB,unsigned char)
PROTO_GENERIC_ALL(UI,unsigned int)
PROTO_GENERIC_ALL(UK,UINT_BIG)
PROTO_GENERIC_ALL(UL,unsigned long int)
PROTO_GENERIC_ALL(US,unsigned short int)

#if HAVE_LONG_DOUBLE     /* Not normally implemented */
PROTO_GENERIC_ALL(LD,long double)
#endif

#undef PROTO_GENERIC_ALL

#define PROTO_GENERIC_DFI(X,Xtype) \
   void astRebin##X##_( AstMapping *, double, int, const int [], const int [], \
                      const Xtype [], const Xtype [], int, const double [], int, \
                      double, int, Xtype, int, const int [], const int [], \
                      const int [], const int [], Xtype [], Xtype [], int * ); \
   void astRebinSeq##X##_( AstMapping *, double, int, const int [], const int [], \
                         const Xtype [], const Xtype [], int, const double [], \
                         int, double, int, Xtype, int, const int [], \
                         const int [], const int [], const int [], Xtype [], \
                         Xtype [], double [], int64_t *, int * );

PROTO_GENERIC_DFI(D,double)
PROTO_GENERIC_DFI(F,float)
PROTO_GENERIC_DFI(I,int)
PROTO_GENERIC_DFI(B,signed char)
PROTO_GENERIC_DFI(UB,unsigned char)

#if HAVE_LONG_DOUBLE     /* Not normally implemented */
PROTO_GENERIC_DFI(LD,long double)
#endif

#undef PROTO_GENERIC_DFI

AstMapping *astRemoveRegions_( AstMapping *, int * );
AstMapping *astSimplify_( AstMapping *, int * );
void astInvert_( AstMapping *, int * );
int astLinearApprox_( AstMapping *, const double *, const double *, double, double *, int * );
int astQuadApprox_( AstMapping *, const double[2], const double[2], int, int, double *, double *, int * );
void astTran1_( AstMapping *, int, const double [], int, double [], int * );
void astTran2_( AstMapping *, int, const double [], const double [], int, double [], double [], int * );
void astTranGrid_( AstMapping *, int, const int[], const int[], double, int, int, int, int, double *, int * );
void astTranN_( AstMapping *, int, int, int, const double *, int, int, int, double *, int * );
void astTranP_( AstMapping *, int, int, const double *[], int, int, double *[], int * );

#if defined(astCLASS)            /* Protected */
void astDecompose_( AstMapping *, AstMapping **, AstMapping **, int *, int *, int *, int * );
void astMapBox_( AstMapping *, const double [], const double [], int, int, double *, double *, double [], double [], int * );
double astRate_( AstMapping *, double *, int, int, int * );
int *astMapSplit_( AstMapping *, int, const int *, AstMapping **, int * );
#else
void astDecomposeId_( AstMapping *, AstMapping **, AstMapping **, int *, int *, int *, int * );
void astMapBoxId_( AstMapping *, const double [], const double [], int, int, double *, double *, double [], double [], int * );
double astRateId_( AstMapping *, double *, int, int, int * );
void astMapSplitId_( AstMapping *, int, const int *, int *, AstMapping **, int * );
#endif

#if defined(astCLASS)            /* Protected */
int astRateState_( int, int * );
AstPointSet *astTransform_( AstMapping *, AstPointSet *, int, AstPointSet *, int * );
int astGetInvert_( AstMapping *, int * );
int astGetIsSimple_( AstMapping *, int * );
int astGetNin_( AstMapping *, int * );
int astGetNout_( AstMapping *, int * );
int astGetReport_( AstMapping *, int * );
int astGetTranForward_( AstMapping *, int * );
int astGetTranInverse_( AstMapping *, int * );
int astGetIsLinear_( AstMapping *, int * );
int astDoNotSimplify_( AstMapping *, int * );
int astMapMerge_( AstMapping *, int, int, int *, AstMapping ***, int **, int * );
int astTestInvert_( AstMapping *, int * );
int astTestReport_( AstMapping *, int * );
void astClearInvert_( AstMapping *, int * );
void astClearReport_( AstMapping *, int * );
int astMapList_( AstMapping *, int, int, int *, AstMapping ***, int **, int * );
void astReportPoints_( AstMapping *, int, AstPointSet *, AstPointSet *, int * );
void astSetInvert_( AstMapping *, int, int * );
void astSetReport_( AstMapping *, int, int * );
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
#define astCheckMapping(this) astINVOKE_CHECK(Mapping,this,0)
#define astVerifyMapping(this) astINVOKE_CHECK(Mapping,this,1)

/* Test class membership. */
#define astIsAMapping(this) astINVOKE_ISA(Mapping,this)

/* NB. There is no constructor function for this class. */

#if defined(astCLASS)            /* Protected */

/* Initialiser. */
#define astInitMapping(mem,size,init,vtab,name,nin,nout,tran_forward,tran_inverse) \
astINVOKE(O,astInitMapping_(mem,size,init,vtab,name,nin,nout,tran_forward,tran_inverse,STATUS_PTR))

/* Vtab Initialiser. */
#define astInitMappingVtab(vtab,name) astINVOKE(V,astInitMappingVtab_(vtab,name,STATUS_PTR))
/* Loader. */
#define astLoadMapping(mem,size,vtab,name,channel) \
astINVOKE(O,astLoadMapping_(mem,size,vtab,name,astCheckChannel(channel),STATUS_PTR))
#endif

/* Interfaces to member functions. */
/* ------------------------------- */
/* Here we make use of astCheckMapping (et al.) to validate Mapping
   pointers before use. This provides a contextual error report if a
   pointer to the wrong sort of object is supplied. */
#if HAVE_LONG_DOUBLE     /* Not normally implemented */
#define astResampleLD(this,ndim_in,lbnd_in,ubnd_in,in,in_var,interp,finterp,params,flags,tol,maxpix,badval,ndim_out,lbnd_out,ubnd_out,lbnd,ubnd,out,out_var) \
astINVOKE(V,astResampleLD_(astCheckMapping(this),ndim_in,lbnd_in,ubnd_in,in,in_var,interp,finterp,params,flags,tol,maxpix,badval,ndim_out,lbnd_out,ubnd_out,lbnd,ubnd,out,out_var,STATUS_PTR))
#endif

#define astInvert(this) \
astINVOKE(V,astInvert_(astCheckMapping(this),STATUS_PTR))
#define astLinearApprox(this,lbnd,ubnd,tol,fit) \
astINVOKE(V,astLinearApprox_(astCheckMapping(this),lbnd,ubnd,tol,fit,STATUS_PTR))
#define astQuadApprox(this,lbnd,ubnd,nx,ny,fit,rms) \
astINVOKE(V,astQuadApprox_(astCheckMapping(this),lbnd,ubnd,nx,ny,fit,rms,STATUS_PTR))
#define astRebinD(this,wlim,ndim_in,lbnd_in,ubnd_in,in,in_var,interp,params,flags,tol,maxpix,badval,ndim_out,lbnd_out,ubnd_out,lbnd,ubnd,out,out_var) \
astINVOKE(V,astRebinD_(astCheckMapping(this),wlim,ndim_in,lbnd_in,ubnd_in,in,in_var,interp,params,flags,tol,maxpix,badval,ndim_out,lbnd_out,ubnd_out,lbnd,ubnd,out,out_var,STATUS_PTR))
#define astRebinF(this,wlim,ndim_in,lbnd_in,ubnd_in,in,in_var,interp,params,flags,tol,maxpix,badval,ndim_out,lbnd_out,ubnd_out,lbnd,ubnd,out,out_var) \
astINVOKE(V,astRebinF_(astCheckMapping(this),wlim,ndim_in,lbnd_in,ubnd_in,in,in_var,interp,params,flags,tol,maxpix,badval,ndim_out,lbnd_out,ubnd_out,lbnd,ubnd,out,out_var,STATUS_PTR))
#define astRebinI(this,wlim,ndim_in,lbnd_in,ubnd_in,in,in_var,interp,params,flags,tol,maxpix,badval,ndim_out,lbnd_out,ubnd_out,lbnd,ubnd,out,out_var) \
astINVOKE(V,astRebinI_(astCheckMapping(this),wlim,ndim_in,lbnd_in,ubnd_in,in,in_var,interp,params,flags,tol,maxpix,badval,ndim_out,lbnd_out,ubnd_out,lbnd,ubnd,out,out_var,STATUS_PTR))
#define astRebinB(this,wlim,ndim_in,lbnd_in,ubnd_in,in,in_var,interp,params,flags,tol,maxpix,badval,ndim_out,lbnd_out,ubnd_out,lbnd,ubnd,out,out_var) \
astINVOKE(V,astRebinB_(astCheckMapping(this),wlim,ndim_in,lbnd_in,ubnd_in,in,in_var,interp,params,flags,tol,maxpix,badval,ndim_out,lbnd_out,ubnd_out,lbnd,ubnd,out,out_var,STATUS_PTR))
#define astRebinUB(this,wlim,ndim_in,lbnd_in,ubnd_in,in,in_var,interp,params,flags,tol,maxpix,badval,ndim_out,lbnd_out,ubnd_out,lbnd,ubnd,out,out_var) \
astINVOKE(V,astRebinUB_(astCheckMapping(this),wlim,ndim_in,lbnd_in,ubnd_in,in,in_var,interp,params,flags,tol,maxpix,badval,ndim_out,lbnd_out,ubnd_out,lbnd,ubnd,out,out_var,STATUS_PTR))
#define astRebinSeqD(this,wlim,ndim_in,lbnd_in,ubnd_in,in,in_var,interp,params,flags,tol,maxpix,badval,ndim_out,lbnd_out,ubnd_out,lbnd,ubnd,out,out_var,weights,nused) \
astINVOKE(V,astRebinSeqD_(astCheckMapping(this),wlim,ndim_in,lbnd_in,ubnd_in,in,in_var,interp,params,flags,tol,maxpix,badval,ndim_out,lbnd_out,ubnd_out,lbnd,ubnd,out,out_var,weights,nused,STATUS_PTR))
#define astRebinSeqF(this,wlim,ndim_in,lbnd_in,ubnd_in,in,in_var,interp,params,flags,tol,maxpix,badval,ndim_out,lbnd_out,ubnd_out,lbnd,ubnd,out,out_var,weights,nused) \
astINVOKE(V,astRebinSeqF_(astCheckMapping(this),wlim,ndim_in,lbnd_in,ubnd_in,in,in_var,interp,params,flags,tol,maxpix,badval,ndim_out,lbnd_out,ubnd_out,lbnd,ubnd,out,out_var,weights,nused,STATUS_PTR))
#define astRebinSeqI(this,wlim,ndim_in,lbnd_in,ubnd_in,in,in_var,interp,params,flags,tol,maxpix,badval,ndim_out,lbnd_out,ubnd_out,lbnd,ubnd,out,out_var,weights,nused) \
astINVOKE(V,astRebinSeqI_(astCheckMapping(this),wlim,ndim_in,lbnd_in,ubnd_in,in,in_var,interp,params,flags,tol,maxpix,badval,ndim_out,lbnd_out,ubnd_out,lbnd,ubnd,out,out_var,weights,nused,STATUS_PTR))
#define astRebinSeqB(this,wlim,ndim_in,lbnd_in,ubnd_in,in,in_var,interp,params,flags,tol,maxpix,badval,ndim_out,lbnd_out,ubnd_out,lbnd,ubnd,out,out_var,weights,nused) \
astINVOKE(V,astRebinSeqB_(astCheckMapping(this),wlim,ndim_in,lbnd_in,ubnd_in,in,in_var,interp,params,flags,tol,maxpix,badval,ndim_out,lbnd_out,ubnd_out,lbnd,ubnd,out,out_var,weights,nused,STATUS_PTR))
#define astRebinSeqUB(this,wlim,ndim_in,lbnd_in,ubnd_in,in,in_var,interp,params,flags,tol,maxpix,badval,ndim_out,lbnd_out,ubnd_out,lbnd,ubnd,out,out_var,weights,nused) \
astINVOKE(V,astRebinSeqUB_(astCheckMapping(this),wlim,ndim_in,lbnd_in,ubnd_in,in,in_var,interp,params,flags,tol,maxpix,badval,ndim_out,lbnd_out,ubnd_out,lbnd,ubnd,out,out_var,weights,nused,STATUS_PTR))
#define astResampleD(this,ndim_in,lbnd_in,ubnd_in,in,in_var,interp,finterp,params,flags,tol,maxpix,badval,ndim_out,lbnd_out,ubnd_out,lbnd,ubnd,out,out_var) \
astINVOKE(V,astResampleD_(astCheckMapping(this),ndim_in,lbnd_in,ubnd_in,in,in_var,interp,finterp,params,flags,tol,maxpix,badval,ndim_out,lbnd_out,ubnd_out,lbnd,ubnd,out,out_var,STATUS_PTR))
#define astResampleF(this,ndim_in,lbnd_in,ubnd_in,in,in_var,interp,finterp,params,flags,tol,maxpix,badval,ndim_out,lbnd_out,ubnd_out,lbnd,ubnd,out,out_var) \
astINVOKE(V,astResampleF_(astCheckMapping(this),ndim_in,lbnd_in,ubnd_in,in,in_var,interp,finterp,params,flags,tol,maxpix,badval,ndim_out,lbnd_out,ubnd_out,lbnd,ubnd,out,out_var,STATUS_PTR))
#define astResampleL(this,ndim_in,lbnd_in,ubnd_in,in,in_var,interp,finterp,params,flags,tol,maxpix,badval,ndim_out,lbnd_out,ubnd_out,lbnd,ubnd,out,out_var) \
astINVOKE(V,astResampleL_(astCheckMapping(this),ndim_in,lbnd_in,ubnd_in,in,in_var,interp,finterp,params,flags,tol,maxpix,badval,ndim_out,lbnd_out,ubnd_out,lbnd,ubnd,out,out_var,STATUS_PTR))
#define astResampleUL(this,ndim_in,lbnd_in,ubnd_in,in,in_var,interp,finterp,params,flags,tol,maxpix,badval,ndim_out,lbnd_out,ubnd_out,lbnd,ubnd,out,out_var) \
astINVOKE(V,astResampleUL_(astCheckMapping(this),ndim_in,lbnd_in,ubnd_in,in,in_var,interp,finterp,params,flags,tol,maxpix,badval,ndim_out,lbnd_out,ubnd_out,lbnd,ubnd,out,out_var,STATUS_PTR))
#define astResampleI(this,ndim_in,lbnd_in,ubnd_in,in,in_var,interp,finterp,params,flags,tol,maxpix,badval,ndim_out,lbnd_out,ubnd_out,lbnd,ubnd,out,out_var) \
astINVOKE(V,astResampleI_(astCheckMapping(this),ndim_in,lbnd_in,ubnd_in,in,in_var,interp,finterp,params,flags,tol,maxpix,badval,ndim_out,lbnd_out,ubnd_out,lbnd,ubnd,out,out_var,STATUS_PTR))
#define astResampleUI(this,ndim_in,lbnd_in,ubnd_in,in,in_var,interp,finterp,params,flags,tol,maxpix,badval,ndim_out,lbnd_out,ubnd_out,lbnd,ubnd,out,out_var) \
astINVOKE(V,astResampleUI_(astCheckMapping(this),ndim_in,lbnd_in,ubnd_in,in,in_var,interp,finterp,params,flags,tol,maxpix,badval,ndim_out,lbnd_out,ubnd_out,lbnd,ubnd,out,out_var,STATUS_PTR))
#define astResampleK(this,ndim_in,lbnd_in,ubnd_in,in,in_var,interp,finterp,params,flags,tol,maxpix,badval,ndim_out,lbnd_out,ubnd_out,lbnd,ubnd,out,out_var) \
astINVOKE(V,astResampleK_(astCheckMapping(this),ndim_in,lbnd_in,ubnd_in,in,in_var,interp,finterp,params,flags,tol,maxpix,badval,ndim_out,lbnd_out,ubnd_out,lbnd,ubnd,out,out_var,STATUS_PTR))
#define astResampleUK(this,ndim_in,lbnd_in,ubnd_in,in,in_var,interp,finterp,params,flags,tol,maxpix,badval,ndim_out,lbnd_out,ubnd_out,lbnd,ubnd,out,out_var) \
astINVOKE(V,astResampleUK_(astCheckMapping(this),ndim_in,lbnd_in,ubnd_in,in,in_var,interp,finterp,params,flags,tol,maxpix,badval,ndim_out,lbnd_out,ubnd_out,lbnd,ubnd,out,out_var,STATUS_PTR))
#define astResampleS(this,ndim_in,lbnd_in,ubnd_in,in,in_var,interp,finterp,params,flags,tol,maxpix,badval,ndim_out,lbnd_out,ubnd_out,lbnd,ubnd,out,out_var) \
astINVOKE(V,astResampleS_(astCheckMapping(this),ndim_in,lbnd_in,ubnd_in,in,in_var,interp,finterp,params,flags,tol,maxpix,badval,ndim_out,lbnd_out,ubnd_out,lbnd,ubnd,out,out_var,STATUS_PTR))
#define astResampleUS(this,ndim_in,lbnd_in,ubnd_in,in,in_var,interp,finterp,params,flags,tol,maxpix,badval,ndim_out,lbnd_out,ubnd_out,lbnd,ubnd,out,out_var) \
astINVOKE(V,astResampleUS_(astCheckMapping(this),ndim_in,lbnd_in,ubnd_in,in,in_var,interp,finterp,params,flags,tol,maxpix,badval,ndim_out,lbnd_out,ubnd_out,lbnd,ubnd,out,out_var,STATUS_PTR))
#define astResampleB(this,ndim_in,lbnd_in,ubnd_in,in,in_var,interp,finterp,params,flags,tol,maxpix,badval,ndim_out,lbnd_out,ubnd_out,lbnd,ubnd,out,out_var) \
astINVOKE(V,astResampleB_(astCheckMapping(this),ndim_in,lbnd_in,ubnd_in,in,in_var,interp,finterp,params,flags,tol,maxpix,badval,ndim_out,lbnd_out,ubnd_out,lbnd,ubnd,out,out_var,STATUS_PTR))
#define astResampleUB(this,ndim_in,lbnd_in,ubnd_in,in,in_var,interp,finterp,params,flags,tol,maxpix,badval,ndim_out,lbnd_out,ubnd_out,lbnd,ubnd,out,out_var) \
astINVOKE(V,astResampleUB_(astCheckMapping(this),ndim_in,lbnd_in,ubnd_in,in,in_var,interp,finterp,params,flags,tol,maxpix,badval,ndim_out,lbnd_out,ubnd_out,lbnd,ubnd,out,out_var,STATUS_PTR))
#define astRemoveRegions(this) astINVOKE(O,astRemoveRegions_(astCheckMapping(this),STATUS_PTR))
#define astSimplify(this) astINVOKE(O,astSimplify_(astCheckMapping(this),STATUS_PTR))
#define astTran1(this,npoint,xin,forward,xout) \
astINVOKE(V,astTran1_(astCheckMapping(this),npoint,xin,forward,xout,STATUS_PTR))
#define astTran2(this,npoint,xin,yin,forward,xout,yout) \
astINVOKE(V,astTran2_(astCheckMapping(this),npoint,xin,yin,forward,xout,yout,STATUS_PTR))
#define astTranGrid(this,ncoord_in,lbnd,ubnd,tol,maxpix,forward,ncoord_out,outdim,out) \
astINVOKE(V,astTranGrid_(astCheckMapping(this),ncoord_in,lbnd,ubnd,tol,maxpix,forward,ncoord_out,outdim,out,STATUS_PTR))
#define astTranN(this,npoint,ncoord_in,indim,in,forward,ncoord_out,outdim,out) \
astINVOKE(V,astTranN_(astCheckMapping(this),npoint,ncoord_in,indim,in,forward,ncoord_out,outdim,out,STATUS_PTR))
#define astTranP(this,npoint,ncoord_in,ptr_in,forward,ncoord_out,ptr_out) \
astINVOKE(V,astTranP_(astCheckMapping(this),npoint,ncoord_in,ptr_in,forward,ncoord_out,ptr_out,STATUS_PTR))

#if defined(astCLASS)            /* Protected */
#define astDecompose(this,map1,map2,series,inv1,inv2) \
astINVOKE(V,astDecompose_(astCheckMapping(this),(AstMapping **)(map1),(AstMapping **)(map2),series,inv1,inv2,STATUS_PTR))
#define astMapBox(this,lbnd_in,ubnd_in,forward,coord_out,lbnd_out,ubnd_out,xl,xu) \
astINVOKE(V,astMapBox_(astCheckMapping(this),lbnd_in,ubnd_in,forward,coord_out,lbnd_out,ubnd_out,xl,xu,STATUS_PTR))
#define astRate(this,at,ax1,ax2) \
astINVOKE(V,astRate_(astCheckMapping(this),at,ax1,ax2,STATUS_PTR))
#define astMapSplit(this,nin,in,map) \
astINVOKE(V,astMapSplit_(this,nin,in,map,STATUS_PTR))
#else
#define astDecompose(this,map1,map2,series,inv1,inv2) \
astINVOKE(V,astDecomposeId_(astCheckMapping(this),(AstMapping **)(map1),(AstMapping **)(map2),series,inv1,inv2,STATUS_PTR))
#define astMapBox(this,lbnd_in,ubnd_in,forward,coord_out,lbnd_out,ubnd_out,xl,xu) \
astINVOKE(V,astMapBoxId_(astCheckMapping(this),lbnd_in,ubnd_in,forward,coord_out,lbnd_out,ubnd_out,xl,xu,STATUS_PTR))
#define astRate(this,at,ax1,ax2) \
astINVOKE(V,astRateId_(astCheckMapping(this),at,ax1,ax2,STATUS_PTR))
#define astMapSplit(this,nin,in,out,map) \
astINVOKE(V,astMapSplitId_(astCheckMapping(this),nin,in,out,map,STATUS_PTR))
#endif

#if defined(astCLASS)            /* Protected */
#define astRateState(disabled) astRateState_(disabled,STATUS_PTR)
#define astClearInvert(this) \
astINVOKE(V,astClearInvert_(astCheckMapping(this),STATUS_PTR))
#define astClearReport(this) \
astINVOKE(V,astClearReport_(astCheckMapping(this),STATUS_PTR))
#define astGetInvert(this) \
astINVOKE(V,astGetInvert_(astCheckMapping(this),STATUS_PTR))
#define astGetIsSimple(this) \
astINVOKE(V,astGetIsSimple_(astCheckMapping(this),STATUS_PTR))
#define astGetNin(this) \
astINVOKE(V,astGetNin_(astCheckMapping(this),STATUS_PTR))
#define astGetNout(this) \
astINVOKE(V,astGetNout_(astCheckMapping(this),STATUS_PTR))
#define astGetReport(this) \
astINVOKE(V,astGetReport_(astCheckMapping(this),STATUS_PTR))
#define astGetTranForward(this) \
astINVOKE(V,astGetTranForward_(astCheckMapping(this),STATUS_PTR))
#define astGetTranInverse(this) \
astINVOKE(V,astGetTranInverse_(astCheckMapping(this),STATUS_PTR))
#define astGetIsLinear(this) \
astINVOKE(V,astGetIsLinear_(astCheckMapping(this),STATUS_PTR))
#define astMapList(this,series,invert,nmap,map_list,invert_list) \
astINVOKE(V,astMapList_(this,series,invert,nmap,map_list,invert_list,STATUS_PTR))
#define astMapMerge(this,where,series,nmap,map_list,invert_list) \
astINVOKE(V,astMapMerge_(astCheckMapping(this),where,series,nmap,map_list,invert_list,STATUS_PTR))
#define astReportPoints(this,forward,in_points,out_points) \
astINVOKE(V,astReportPoints_(astCheckMapping(this),forward,astCheckPointSet(in_points),astCheckPointSet(out_points),STATUS_PTR))
#define astSetInvert(this,value) \
astINVOKE(V,astSetInvert_(astCheckMapping(this),value,STATUS_PTR))
#define astSetReport(this,value) \
astINVOKE(V,astSetReport_(astCheckMapping(this),value,STATUS_PTR))
#define astTestInvert(this) \
astINVOKE(V,astTestInvert_(astCheckMapping(this),STATUS_PTR))
#define astTestReport(this) \
astINVOKE(V,astTestReport_(astCheckMapping(this),STATUS_PTR))
#define astDoNotSimplify(this) \
astINVOKE(V,astDoNotSimplify_(astCheckMapping(this),STATUS_PTR))

/* Since a NULL PointSet pointer is acceptable here, we must omit the argument
   checking in that case. (But unfortunately, "out" then gets evaluated
   twice - this is unlikely to matter, but is there a better way?) */
#define astTransform(this,in,forward,out) \
astINVOKE(O,astTransform_(astCheckMapping(this),astCheckPointSet(in),forward,(out)?astCheckPointSet(out):NULL,STATUS_PTR))
#endif
#endif





