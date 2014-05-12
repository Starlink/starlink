#if !defined( REGION_INCLUDED ) /* Include this file only once */
#define REGION_INCLUDED
/*
*+
*  Name:
*     region.h

*  Type:
*     C include file.

*  Purpose:
*     Define the interface to the Region class.

*  Invocation:
*     #include "region.h"

*  Description:
*     This include file defines the interface to the Region class and
*     provides the type definitions, function prototypes and macros, etc.
*     needed to use this class.

*  Inheritance:
*     The Region class inherits from the Frame class.

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
*     5-DEC-2003 (DSB):
*        Original version.
*     2-MAR-2006 (DSB):
*        Changed AST_LONG_DOUBLE to HAVE_LONG_DOUBLE.
*-
*/

/* Include files. */
/* ============== */
/* Interface definitions. */
/* ---------------------- */
#include "frame.h"               /* Parent Frame class */

/* Macros. */
/* ======= */

/* Type Definitions. */
/* ================= */
/* Region structure. */
/* ------------------- */
/* This structure contains all information that is unique to each object in
   the class (e.g. its instance variables). */
#if defined(astCLASS) || defined(astFORTRAN77)
#define STATUS_PTR status
#else
#define STATUS_PTR astGetStatusPtr
#endif

typedef struct AstRegion {

/* Attributes inherited from the parent class. */
   AstFrame parent;           /* Parent class structure */

/* Attributes specific to objects in this class. */
   AstFrameSet *frameset;     /* FrameSet holding original and current Frames */
   AstPointSet *points;       /* Points defining region location and extent */
   struct AstRegion *unc;     /* Region specifying position uncertainties */
   double fillfactor;         /* Fill factor (0.0->1.0) */
   int regionfs;              /* Include FrameSet in dump? */
   int negated;               /* Has the Region been negated? */
   int closed;                /* Is the boundary part of the Region? */
   int meshsize;              /* No. of points on boundary mesh */
   struct AstRegion *defunc;  /* Default uncertainty Region */
   AstPointSet *basemesh;     /* Base frame mesh covering the boundary */
   AstPointSet *basegrid;     /* Base frame grid covering the boundary */
   int adaptive;              /* Does the Region adapt to coord sys changes? */
   int nomap;                 /* Ignore the Region's FrameSet? */
   struct AstRegion *negation;/* Negated copy of "this" */
} AstRegion;

/* Virtual function table. */
/* ----------------------- */
/* This table contains all information that is the same for all objects in the
   class (e.g. pointers to its virtual functions). */
#if defined(astCLASS)            /* Protected */
typedef struct AstRegionVtab {

/* Properties (e.g. methods) inherited from the parent class. */
   AstFrameVtab frame_vtab;      /* Parent class virtual function table */

/* A Unique identifier to determine class membership. */
   AstClassIdentifier id;

/* Properties (e.g. methods) specific to this class. */
   int (* Overlap)( AstRegion *, AstRegion *, int * );
   int (* OverlapX)( AstRegion *, AstRegion *, int * );
   AstRegion *(* MapRegion)( AstRegion *, AstMapping *, AstFrame *, int * );
   AstFrame *(* GetRegionFrame)( AstRegion *, int * );
   AstFrameSet *(* GetRegionFrameSet)( AstRegion *, int * );
   AstFrame *(* RegFrame)( AstRegion *, int * );
   AstFrameSet *(* GetRegFS)( AstRegion *, int * );
   AstPointSet *(* RegTransform)( AstRegion *, AstPointSet *, int, AstPointSet *, AstFrame **, int * );
   AstPointSet *(* BTransform)( AstRegion *, AstPointSet *, int, AstPointSet *, int * );
   void (* Negate)( AstRegion *, int * );
   void (* RegBaseBox)( AstRegion *, double *, double *, int * );
   void (* RegBaseBox2)( AstRegion *, double *, double *, int * );
   void (* RegSetAttrib)( AstRegion *, const char *, char **, int * );
   void (* RegClearAttrib)( AstRegion *, const char *, char **, int * );
   void (* GetRegionBounds)( AstRegion *, double *, double *, int * );
   void (* ShowMesh)( AstRegion *, int, const char *, int * );
   void (* GetRegionBounds2)( AstRegion *, double *, double *, int * );
   void (* ClearUnc)( AstRegion *, int * );
   void (* RegOverlay)( AstRegion *, AstRegion *, int, int * );
   void (* GetRegionMesh)( AstRegion *, int, int, int, int *, double *, int * );
   void (* GetRegionPoints)( AstRegion *, int, int, int *, double *, int * );
   int (* GetBounded)( AstRegion *, int * );
   int (* TestUnc)( AstRegion *, int * );
   int (* RegDummyFS)( AstRegion *, int * );
   int (* RegPins)( AstRegion *, AstPointSet *, AstRegion *, int **, int * );
   AstMapping *(* RegMapping)( AstRegion *, int * );
   AstPointSet *(* RegMesh)( AstRegion *, int * );
   AstPointSet *(* RegGrid)( AstRegion *, int * );
   AstPointSet *(* RegBaseMesh)( AstRegion *, int * );
   AstPointSet *(* RegBaseGrid)( AstRegion *, int * );
   AstRegion **(* RegSplit)( AstRegion *, int *, int * );
   AstPointSet *(* BndBaseMesh)( AstRegion *, double *, double *, int * );
   AstPointSet *(* BndMesh)( AstRegion *, double *, double *, int * );
   AstRegion *(* GetNegation)( AstRegion *, int * );
   AstRegion *(* GetUncFrm)( AstRegion *, int, int * );
   AstRegion *(* GetUnc)( AstRegion *, int, int * );
   AstRegion *(* GetDefUnc)( AstRegion *, int * );
   AstRegion *(* RegBasePick)( AstRegion *this, int, const int *, int * );
   void (* ResetCache)( AstRegion *, int * );
   int (* RegTrace)( AstRegion *, int, double *, double **, int * );
   void (* SetUnc)( AstRegion *, AstRegion *, int * );
   void (* SetRegFS)( AstRegion *, AstFrame *, int * );
   double *(* RegCentre)( AstRegion *, double *, double **, int, int, int * );

#if HAVE_LONG_DOUBLE     /* Not normally implemented */
   int (* MaskLD)( AstRegion *, AstMapping *, int, int, const int[], const int ubnd[], long double [], long double, int * );
#endif
   int (* MaskB)( AstRegion *, AstMapping *, int, int, const int[], const int[], signed char[], signed char, int * );
   int (* MaskD)( AstRegion *, AstMapping *, int, int, const int[], const int[], double[], double, int * );
   int (* MaskF)( AstRegion *, AstMapping *, int, int, const int[], const int[], float[], float, int * );
   int (* MaskI)( AstRegion *, AstMapping *, int, int, const int[], const int[], int[], int, int * );
   int (* MaskL)( AstRegion *, AstMapping *, int, int, const int[], const int[], long int[], long int, int * );
   int (* MaskS)( AstRegion *, AstMapping *, int, int, const int[], const int[], short int[], short int, int * );
   int (* MaskUB)( AstRegion *, AstMapping *, int, int, const int[], const int[], unsigned char[], unsigned char, int * );
   int (* MaskUI)( AstRegion *, AstMapping *, int, int, const int[], const int[], unsigned int[], unsigned int, int * );
   int (* MaskUL)( AstRegion *, AstMapping *, int, int, const int[], const int[], unsigned long int[], unsigned long int, int * );
   int (* MaskUS)( AstRegion *, AstMapping *, int, int, const int[], const int[], unsigned short int[], unsigned short int, int * );

   int (* GetNegated)( AstRegion *, int * );
   int (* TestNegated)( AstRegion *, int * );
   void (* ClearNegated)( AstRegion *, int * );
   void (* SetNegated)( AstRegion *, int, int * );

   int (* GetRegionFS)( AstRegion *, int * );
   int (* TestRegionFS)( AstRegion *, int * );
   void (* ClearRegionFS)( AstRegion *, int * );
   void (* SetRegionFS)( AstRegion *, int, int * );

   int (* GetClosed)( AstRegion *, int * );
   int (* TestClosed)( AstRegion *, int * );
   void (* ClearClosed)( AstRegion *, int * );
   void (* SetClosed)( AstRegion *, int, int * );

   int (* GetMeshSize)( AstRegion *, int * );
   int (* TestMeshSize)( AstRegion *, int * );
   void (* ClearMeshSize)( AstRegion *, int * );
   void (* SetMeshSize)( AstRegion *, int, int * );

   double (* GetFillFactor)( AstRegion *, int * );
   int (* TestFillFactor)( AstRegion *, int * );
   void (* ClearFillFactor)( AstRegion *, int * );
   void (* SetFillFactor)( AstRegion *, double, int * );

   int (* GetAdaptive)( AstRegion *, int * );
   int (* TestAdaptive)( AstRegion *, int * );
   void (* ClearAdaptive)( AstRegion *, int * );
   void (* SetAdaptive)( AstRegion *, int, int * );

} AstRegionVtab;
#if defined(THREAD_SAFE)

/* Define a structure holding all data items that are global within this
   class. */
typedef struct AstRegionGlobals {
   AstRegionVtab Class_Vtab;
   int Class_Init;
   char GetAttrib_Buff[ 101 ];
} AstRegionGlobals;

#endif
#endif

/* Function prototypes. */
/* ==================== */
/* Prototypes for standard class functions. */
/* ---------------------------------------- */
astPROTO_CHECK(Region)         /* Check class membership */
astPROTO_ISA(Region)           /* Test class membership */

#if defined(astCLASS)            /* Protected */

/* Initialiser. */
AstRegion *astInitRegion_( void *, size_t, int, AstRegionVtab *, const char *,
                           AstFrame *, AstPointSet *, AstRegion *, int * );

/* Vtab initialiser. */
void astInitRegionVtab_( AstRegionVtab *, const char *, int * );

/* Loader. */
AstRegion *astLoadRegion_( void *, size_t, AstRegionVtab *,
                           const char *, AstChannel *, int * );

/* Thread-safe initialiser for all global data used by this module. */
#if defined(THREAD_SAFE)
void astInitRegionGlobals_( AstRegionGlobals * );
#endif

#endif

/* Prototypes for member functions. */
/* -------------------------------- */

AstFrame *astGetRegionFrame_( AstRegion *, int * );
AstFrameSet *astGetRegionFrameSet_( AstRegion *, int * );
int astOverlap_( AstRegion *, AstRegion *, int * );
void astNegate_( AstRegion *, int * );

#if HAVE_LONG_DOUBLE     /* Not normally implemented */
int astMaskLD_( AstRegion *, AstMapping *, int, int, const int[], const int[], long double [], long double, int * );
#endif
int astMaskB_( AstRegion *, AstMapping *, int, int, const int[], const int[], signed char[], signed char, int * );
int astMaskD_( AstRegion *, AstMapping *, int, int, const int[], const int[], double[], double, int * );
int astMaskF_( AstRegion *, AstMapping *, int, int, const int[], const int[], float[], float, int * );
int astMaskI_( AstRegion *, AstMapping *, int, int, const int[], const int[], int[], int, int * );
int astMaskL_( AstRegion *, AstMapping *, int, int, const int[], const int[], long int[], long int, int * );
int astMaskS_( AstRegion *, AstMapping *, int, int, const int[], const int[], short int[], short int, int * );
int astMaskUB_( AstRegion *, AstMapping *, int, int, const int[], const int[], unsigned char[], unsigned char, int * );
int astMaskUI_( AstRegion *, AstMapping *, int, int, const int[], const int[], unsigned int[], unsigned int, int * );
int astMaskUL_( AstRegion *, AstMapping *, int, int, const int[], const int[], unsigned long int[], unsigned long int, int * );
int astMaskUS_( AstRegion *, AstMapping *, int, int, const int[], const int[], unsigned short int[], unsigned short int, int * );
void astSetUnc_( AstRegion *, AstRegion *, int * );
AstRegion *astGetNegation_( AstRegion *, int * );
AstRegion *astGetUnc_( AstRegion *, int, int * );
void astGetRegionBounds_( AstRegion *, double *, double *, int * );
void astShowMesh_( AstRegion *, int, const char *, int * );
void astGetRegionMesh_( AstRegion *, int, int, int, int *, double *, int * );
void astGetRegionPoints_( AstRegion *, int, int, int *, double *, int * );

#if defined(astCLASS)            /* Protected */
void astGetRegionBounds2_( AstRegion *, double *, double *, int * );
AstRegion *astMapRegion_( AstRegion *, AstMapping *, AstFrame *, int * );
AstFrame *astRegFrame_( AstRegion *, int * );
AstPointSet *astRegTransform_( AstRegion *, AstPointSet *, int, AstPointSet *, AstFrame **, int * );
AstPointSet *astBTransform_( AstRegion *, AstPointSet *, int, AstPointSet *, int * );
void astRegBaseBox_( AstRegion *, double *, double *, int * );
void astRegBaseBox2_( AstRegion *, double *, double *, int * );
void astRegSetAttrib_( AstRegion *, const char *, char **, int * );
void astRegClearAttrib_( AstRegion *, const char *, char **, int * );
void astClearUnc_( AstRegion *, int * );
void astRegOverlay_( AstRegion *, AstRegion *, int, int * );
int astGetBounded_( AstRegion *, int * );
int astTestUnc_( AstRegion *, int * );
int astRegDummyFS_( AstRegion *, int * );
int astRegPins_( AstRegion *, AstPointSet *, AstRegion *, int **, int * );
AstMapping *astRegMapping_( AstRegion *, int * );
AstPointSet *astRegMesh_( AstRegion *, int * );
AstPointSet *astRegGrid_( AstRegion *, int * );
AstPointSet *astRegBaseMesh_( AstRegion *, int * );
AstPointSet *astRegBaseGrid_( AstRegion *, int * );
AstPointSet *astBndBaseMesh_( AstRegion *, double *, double *, int * );
AstRegion **astRegSplit_( AstRegion *, int *, int * );
AstPointSet *astBndMesh_( AstRegion *, double *, double *, int * );
AstRegion *astGetUncFrm_( AstRegion *, int, int * );
AstRegion *astGetDefUnc_( AstRegion *, int * );
AstRegion *astRegBasePick_( AstRegion *this, int, const int *, int * );
int astOverlapX_( AstRegion *, AstRegion *, int * );
AstFrameSet *astGetRegFS_( AstRegion *, int * );
void astSetRegFS_( AstRegion *, AstFrame *, int * );
double *astRegCentre_( AstRegion *, double *, double **, int, int, int * );
double *astRegTranPoint_( AstRegion *, double *, int, int, int * );
void astResetCache_( AstRegion *, int * );
int astRegTrace_( AstRegion *, int, double *, double **, int * );

int astGetNegated_( AstRegion *, int * );
int astTestNegated_( AstRegion *, int * );
void astClearNegated_( AstRegion *, int * );
void astSetNegated_( AstRegion *, int, int * );

int astGetRegionFS_( AstRegion *, int * );
int astTestRegionFS_( AstRegion *, int * );
void astClearRegionFS_( AstRegion *, int * );
void astSetRegionFS_( AstRegion *, int, int * );

int astGetMeshSize_( AstRegion *, int * );
int astTestMeshSize_( AstRegion *, int * );
void astClearMeshSize_( AstRegion *, int * );
void astSetMeshSize_( AstRegion *, int, int * );

int astGetClosed_( AstRegion *, int * );
int astTestClosed_( AstRegion *, int * );
void astClearClosed_( AstRegion *, int * );
void astSetClosed_( AstRegion *, int, int * );

double astGetFillFactor_( AstRegion *, int * );
int astTestFillFactor_( AstRegion *, int * );
void astClearFillFactor_( AstRegion *, int * );
void astSetFillFactor_( AstRegion *, double, int * );

int astGetAdaptive_( AstRegion *, int * );
int astTestAdaptive_( AstRegion *, int * );
void astClearAdaptive_( AstRegion *, int * );
void astSetAdaptive_( AstRegion *, int, int * );

#else   /* Public only */
AstRegion *astMapRegionId_( AstRegion *, AstMapping *, AstFrame *, int * );

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
#define astCheckRegion(this) astINVOKE_CHECK(Region,this,0)
#define astVerifyRegion(this) astINVOKE_CHECK(Region,this,1)

/* Test class membership. */
#define astIsARegion(this) astINVOKE_ISA(Region,this)

#if defined(astCLASS)            /* Protected */

/* Initialiser. */
#define astInitRegion(mem,size,init,vtab,name,frame,pset,acc)\
astINVOKE(O,astInitRegion_(mem,size,init,vtab,name,astCheckFrame(frame),pset,acc,STATUS_PTR))

/* Vtab Initialiser. */
#define astInitRegionVtab(vtab,name) astINVOKE(V,astInitRegionVtab_(vtab,name,STATUS_PTR))
/* Loader. */
#define astLoadRegion(mem,size,vtab,name,channel) \
astINVOKE(O,astLoadRegion_(mem,size,vtab,name,astCheckChannel(channel),STATUS_PTR))
#endif

/* Interfaces to public member functions. */
/* -------------------------------------- */
/* Here we make use of astCheckRegion to validate Region pointers before
   use. This provides a contextual error report if a pointer to the wrong sort
   of object is supplied. */
#define astGetRegionFrame(this) \
astINVOKE(O,astGetRegionFrame_(astCheckRegion(this),STATUS_PTR))
#define astGetRegionFrameSet(this) \
astINVOKE(O,astGetRegionFrameSet_(astCheckRegion(this),STATUS_PTR))
#define astNegate(this) \
astINVOKE(V,astNegate_(astCheckRegion(this),STATUS_PTR))
#define astOverlap(this,that) \
astINVOKE(V,astOverlap_(astCheckRegion(this),astCheckRegion(that),STATUS_PTR))

#if HAVE_LONG_DOUBLE     /* Not normally implemented */
#define astMaskLD(this,map,inside,ndim,lbnd,ubnd,in,val) \
astINVOKE(V,astMaskLD_(astCheckRegion(this),(map?astCheckMapping(map):NULL),inside,ndim,lbnd,ubnd,in,val,STATUS_PTR))
#endif

#define astMaskB(this,map,inside,ndim,lbnd,ubnd,in,val) \
astINVOKE(V,astMaskB_(astCheckRegion(this),(map?astCheckMapping(map):NULL),inside,ndim,lbnd,ubnd,in,val,STATUS_PTR))
#define astMaskD(this,map,inside,ndim,lbnd,ubnd,in,val) \
astINVOKE(V,astMaskD_(astCheckRegion(this),(map?astCheckMapping(map):NULL),inside,ndim,lbnd,ubnd,in,val,STATUS_PTR))
#define astMaskF(this,map,inside,ndim,lbnd,ubnd,in,val) \
astINVOKE(V,astMaskF_(astCheckRegion(this),(map?astCheckMapping(map):NULL),inside,ndim,lbnd,ubnd,in,val,STATUS_PTR))
#define astMaskI(this,map,inside,ndim,lbnd,ubnd,in,val) \
astINVOKE(V,astMaskI_(astCheckRegion(this),(map?astCheckMapping(map):NULL),inside,ndim,lbnd,ubnd,in,val,STATUS_PTR))
#define astMaskL(this,map,inside,ndim,lbnd,ubnd,in,val) \
astINVOKE(V,astMaskL_(astCheckRegion(this),(map?astCheckMapping(map):NULL),inside,ndim,lbnd,ubnd,in,val,STATUS_PTR))
#define astMaskS(this,map,inside,ndim,lbnd,ubnd,in,val) \
astINVOKE(V,astMaskS_(astCheckRegion(this),(map?astCheckMapping(map):NULL),inside,ndim,lbnd,ubnd,in,val,STATUS_PTR))
#define astMaskUB(this,map,inside,ndim,lbnd,ubnd,in,val) \
astINVOKE(V,astMaskUB_(astCheckRegion(this),(map?astCheckMapping(map):NULL),inside,ndim,lbnd,ubnd,in,val,STATUS_PTR))
#define astMaskUI(this,map,inside,ndim,lbnd,ubnd,in,val) \
astINVOKE(V,astMaskUI_(astCheckRegion(this),(map?astCheckMapping(map):NULL),inside,ndim,lbnd,ubnd,in,val,STATUS_PTR))
#define astMaskUL(this,map,inside,ndim,lbnd,ubnd,in,val) \
astINVOKE(V,astMaskUL_(astCheckRegion(this),(map?astCheckMapping(map):NULL),inside,ndim,lbnd,ubnd,in,val,STATUS_PTR))
#define astMaskUS(this,map,inside,ndim,lbnd,ubnd,in,val) \
astINVOKE(V,astMaskUS_(astCheckRegion(this),(map?astCheckMapping(map):NULL),inside,ndim,lbnd,ubnd,in,val,STATUS_PTR))
#define astSetUnc(this,unc) astINVOKE(V,astSetUnc_(astCheckRegion(this),unc?astCheckRegion(unc):NULL,STATUS_PTR))
#define astGetUnc(this,def) astINVOKE(O,astGetUnc_(astCheckRegion(this),def,STATUS_PTR))
#define astGetRegionBounds(this,lbnd,ubnd) astINVOKE(V,astGetRegionBounds_(astCheckRegion(this),lbnd,ubnd,STATUS_PTR))
#define astShowMesh(this,format,ttl) astINVOKE(V,astShowMesh_(astCheckRegion(this),format,ttl,STATUS_PTR))
#define astGetRegionMesh(this,surface,maxpoint,maxcoord,npoint,points) \
astINVOKE(V,astGetRegionMesh_(astCheckRegion(this),surface,maxpoint,maxcoord,npoint,points,STATUS_PTR))
#define astGetRegionPoints(this,maxpoint,maxcoord,npoint,points) \
astINVOKE(V,astGetRegionPoints_(astCheckRegion(this),maxpoint,maxcoord,npoint,points,STATUS_PTR))

/* Interfaces to protected member functions. */
/* ----------------------------------------- */
#if defined(astCLASS)            /* Protected */

#define astGetNegation(this) astINVOKE(O,astGetNegation_(astCheckRegion(this),STATUS_PTR))
#define astGetRegionBounds2(this,lbnd,ubnd) astINVOKE(V,astGetRegionBounds2_(astCheckRegion(this),lbnd,ubnd,STATUS_PTR))
#define astClearUnc(this) astINVOKE(V,astClearUnc_(astCheckRegion(this),STATUS_PTR))
#define astGetBounded(this) astINVOKE(V,astGetBounded_(astCheckRegion(this),STATUS_PTR))
#define astGetUncFrm(this,ifrm) astINVOKE(O,astGetUncFrm_(astCheckRegion(this),ifrm,STATUS_PTR))
#define astGetDefUnc(this) astINVOKE(O,astGetDefUnc_(astCheckRegion(this),STATUS_PTR))
#define astMapRegion(this,map,frame) astINVOKE(O,astMapRegion_(astCheckRegion(this),astCheckMapping(map),astCheckFrame(frame),STATUS_PTR))
#define astOverlapX(that,this) astINVOKE(V,astOverlapX_(astCheckRegion(that),astCheckRegion(this),STATUS_PTR))
#define astRegBaseBox(this,lbnd,ubnd) astINVOKE(V,astRegBaseBox_(astCheckRegion(this),lbnd,ubnd,STATUS_PTR))
#define astRegBaseBox2(this,lbnd,ubnd) astINVOKE(V,astRegBaseBox2_(astCheckRegion(this),lbnd,ubnd,STATUS_PTR))
#define astRegSetAttrib(this,setting,bset) astINVOKE(V,astRegSetAttrib_(astCheckRegion(this),setting,bset,STATUS_PTR))
#define astRegClearAttrib(this,setting,batt) astINVOKE(V,astRegClearAttrib_(astCheckRegion(this),setting,batt,STATUS_PTR))
#define astRegBaseMesh(this) astINVOKE(O,astRegBaseMesh_(astCheckRegion(this),STATUS_PTR))
#define astRegBasePick(this,naxes,axes) astINVOKE(O,astRegBasePick_(astCheckRegion(this),naxes,axes,STATUS_PTR))
#define astRegBaseGrid(this) astINVOKE(O,astRegBaseGrid_(astCheckRegion(this),STATUS_PTR))
#define astRegSplit(this,nlist) astINVOKE(V,astRegSplit_(astCheckRegion(this),nlist,STATUS_PTR))
#define astBndBaseMesh(this,lbnd,ubnd) astINVOKE(O,astBndBaseMesh_(astCheckRegion(this),lbnd,ubnd,STATUS_PTR))
#define astBndMesh(this,lbnd,ubnd) astINVOKE(O,astBndMesh_(astCheckRegion(this),lbnd,ubnd,STATUS_PTR))
#define astRegCentre(this,cen,ptr,index,ifrm) astINVOKE(V,astRegCentre_(astCheckRegion(this),cen,ptr,index,ifrm,STATUS_PTR))
#define astRegFrame(this) astINVOKE(O,astRegFrame_(astCheckRegion(this),STATUS_PTR))
#define astRegGrid(this) astINVOKE(O,astRegGrid_(astCheckRegion(this),STATUS_PTR))
#define astRegMesh(this) astINVOKE(O,astRegMesh_(astCheckRegion(this),STATUS_PTR))
#define astRegOverlay(this,that,unc) astINVOKE(V,astRegOverlay_(astCheckRegion(this),astCheckRegion(that),unc,STATUS_PTR))
#define astRegDummyFS(this) astINVOKE(V,astRegDummyFS_(astCheckRegion(this),STATUS_PTR))
#define astRegMapping(this) astINVOKE(O,astRegMapping_(astCheckRegion(this),STATUS_PTR))
#define astRegPins(this,pset,unc,mask) astINVOKE(V,astRegPins_(astCheckRegion(this),astCheckPointSet(pset),unc?astCheckRegion(unc):unc,mask,STATUS_PTR))
#define astRegTranPoint(this,in,np,forward) astRegTranPoint_(this,in,np,forward,STATUS_PTR)
#define astGetRegFS(this) astINVOKE(O,astGetRegFS_(astCheckRegion(this),STATUS_PTR))
#define astSetRegFS(this,frm) astINVOKE(V,astSetRegFS_(astCheckRegion(this),astCheckFrame(frm),STATUS_PTR))
#define astTestUnc(this) astINVOKE(V,astTestUnc_(astCheckRegion(this),STATUS_PTR))
#define astResetCache(this) astINVOKE(V,astResetCache_(astCheckRegion(this),STATUS_PTR))
#define astRegTrace(this,n,dist,ptr) astINVOKE(V,astRegTrace_(astCheckRegion(this),n,dist,ptr,STATUS_PTR))

/* Since a NULL PointSet pointer is acceptable for "out", we must omit the
   argument checking in that case. (But unfortunately, "out" then gets
   evaluated twice - this is unlikely to matter, but is there a better way?) */

#define astRegTransform(this,in,forward,out,frm) \
astINVOKE(O,astRegTransform_(astCheckRegion(this),in?astCheckPointSet(in):NULL,forward,(out)?astCheckPointSet(out):NULL,frm,STATUS_PTR))

#define astBTransform(this,in,forward,out) \
astINVOKE(O,astBTransform_(astCheckRegion(this),in?astCheckPointSet(in):NULL,forward,(out)?astCheckPointSet(out):NULL,STATUS_PTR))

#define astClearNegated(this) astINVOKE(V,astClearNegated_(astCheckRegion(this),STATUS_PTR))
#define astGetNegated(this) astINVOKE(V,astGetNegated_(astCheckRegion(this),STATUS_PTR))
#define astSetNegated(this,negated) astINVOKE(V,astSetNegated_(astCheckRegion(this),negated,STATUS_PTR))
#define astTestNegated(this) astINVOKE(V,astTestNegated_(astCheckRegion(this),STATUS_PTR))

#define astClearAdaptive(this) astINVOKE(V,astClearAdaptive_(astCheckRegion(this),STATUS_PTR))
#define astGetAdaptive(this) astINVOKE(V,astGetAdaptive_(astCheckRegion(this),STATUS_PTR))
#define astSetAdaptive(this,adaptive) astINVOKE(V,astSetAdaptive_(astCheckRegion(this),adaptive,STATUS_PTR))
#define astTestAdaptive(this) astINVOKE(V,astTestAdaptive_(astCheckRegion(this),STATUS_PTR))

#define astClearRegionFS(this) astINVOKE(V,astClearRegionFS_(astCheckRegion(this),STATUS_PTR))
#define astGetRegionFS(this) astINVOKE(V,astGetRegionFS_(astCheckRegion(this),STATUS_PTR))
#define astSetRegionFS(this,fs) astINVOKE(V,astSetRegionFS_(astCheckRegion(this),fs,STATUS_PTR))
#define astTestRegionFS(this) astINVOKE(V,astTestRegionFS_(astCheckRegion(this),STATUS_PTR))

#define astClearMeshSize(this) astINVOKE(V,astClearMeshSize_(astCheckRegion(this),STATUS_PTR))
#define astGetMeshSize(this) astINVOKE(V,astGetMeshSize_(astCheckRegion(this),STATUS_PTR))
#define astSetMeshSize(this,meshsize) astINVOKE(V,astSetMeshSize_(astCheckRegion(this),meshsize,STATUS_PTR))
#define astTestMeshSize(this) astINVOKE(V,astTestMeshSize_(astCheckRegion(this),STATUS_PTR))

#define astClearClosed(this) astINVOKE(V,astClearClosed_(astCheckRegion(this),STATUS_PTR))
#define astGetClosed(this) astINVOKE(V,astGetClosed_(astCheckRegion(this),STATUS_PTR))
#define astSetClosed(this,closed) astINVOKE(V,astSetClosed_(astCheckRegion(this),closed,STATUS_PTR))
#define astTestClosed(this) astINVOKE(V,astTestClosed_(astCheckRegion(this),STATUS_PTR))

#define astClearFillFactor(this) astINVOKE(V,astClearFillFactor_(astCheckRegion(this),STATUS_PTR))
#define astGetFillFactor(this) astINVOKE(V,astGetFillFactor_(astCheckRegion(this),STATUS_PTR))
#define astSetFillFactor(this,ff) astINVOKE(V,astSetFillFactor_(astCheckRegion(this),ff,STATUS_PTR))
#define astTestFillFactor(this) astINVOKE(V,astTestFillFactor_(astCheckRegion(this),STATUS_PTR))

#else  /* Public only */
#define astMapRegion(this,map,frame) astINVOKE(O,astMapRegionId_(astCheckRegion(this),astCheckMapping(map),astCheckFrame(frame),STATUS_PTR))
#endif

#endif





