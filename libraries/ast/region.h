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
*     <COPYRIGHT_STATEMENT>

*  Authors:
*     DSB: David S. Berry (Starlink)

*  History:
*     5-DEC-2003 (DSB):
*        Original version.
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
   int defunc;                /* Is "unc" set to a default value? */
   AstPointSet *basemesh;     /* Base frame mesh covering the boundary */
   AstPointSet *basegrid;     /* Base frame grid covering the boundary */
} AstRegion;

/* Virtual function table. */
/* ----------------------- */
/* This table contains all information that is the same for all objects in the
   class (e.g. pointers to its virtual functions). */
#if defined(astCLASS)            /* Protected */
typedef struct AstRegionVtab {

/* Properties (e.g. methods) inherited from the parent class. */
   AstFrameVtab frame_vtab;      /* Parent class virtual function table */

/* Unique flag value to determine class membership. */
   int *check;                   /* Check value */

/* Properties (e.g. methods) specific to this class. */
   int (* Overlap)( AstRegion *, AstRegion * );
   int (* OverlapX)( AstRegion *, AstRegion * );
   AstRegion *(* MapRegion)( AstRegion *, AstMapping *, AstFrame * );
   AstFrame *(* GetRegionFrame)( AstRegion * );
   AstFrame *(* RegFrame)( AstRegion * );
   AstPointSet *(* RegTransform)( AstRegion *, AstPointSet *, int, AstPointSet *, AstFrame ** );
   void (* Negate)( AstRegion * );
   void (* RegBaseBox)( AstRegion *, double *, double * );
   void (* GetRegionBounds)( AstRegion *, double *, double * );
   void (* ClearUnc)( AstRegion * );
   void (* RegOverlay)( AstRegion *, AstRegion * );
   int (* DumpUnc)( AstRegion * );
   int (* GetBounded)( AstRegion * );
   int (* TestUnc)( AstRegion * );
   int (* RegDummyFS)( AstRegion * );
   int (* RegPins)( AstRegion *, AstPointSet *, AstRegion *, int **);
   AstPointSet *(* RegMesh)( AstRegion * );
   AstPointSet *(* RegGrid)( AstRegion * );
   AstPointSet *(* RegBaseMesh)( AstRegion * );
   AstPointSet *(* RegBaseGrid)( AstRegion * );
   AstPointSet *(* BndBaseMesh)( AstRegion *, double *, double * );
   AstRegion *(* GetUncFrm)( AstRegion *, int );
   AstRegion *(* GetUnc)( AstRegion *, int );
   AstRegion *(* GetDefUnc)( AstRegion * );
   void (* SetUnc)( AstRegion *, AstRegion * );
   void (* SetRegFS)( AstRegion *, AstFrame * );
   double *(* RegCentre)( AstRegion *, double *, double **, int, int );

#if defined(AST_LONG_DOUBLE)     /* Not normally implemented */
   int (* MaskLD)( AstRegion *, AstMapping *, int, int, const int[], const int ubnd[], long double [], long double );
#endif
   int (* MaskB)( AstRegion *, AstMapping *, int, int, const int[], const int[], signed char[], signed char );
   int (* MaskD)( AstRegion *, AstMapping *, int, int, const int[], const int[], double[], double );
   int (* MaskF)( AstRegion *, AstMapping *, int, int, const int[], const int[], float[], float );
   int (* MaskI)( AstRegion *, AstMapping *, int, int, const int[], const int[], int[], int );
   int (* MaskL)( AstRegion *, AstMapping *, int, int, const int[], const int[], long int[], long int );
   int (* MaskS)( AstRegion *, AstMapping *, int, int, const int[], const int[], short int[], short int );
   int (* MaskUB)( AstRegion *, AstMapping *, int, int, const int[], const int[], unsigned char[], unsigned char );
   int (* MaskUI)( AstRegion *, AstMapping *, int, int, const int[], const int[], unsigned int[], unsigned int );
   int (* MaskUL)( AstRegion *, AstMapping *, int, int, const int[], const int[], unsigned long int[], unsigned long int );
   int (* MaskUS)( AstRegion *, AstMapping *, int, int, const int[], const int[], unsigned short int[], unsigned short int );

   int (* GetNegated)( AstRegion * );
   int (* TestNegated)( AstRegion * );
   void (* ClearNegated)( AstRegion * );
   void (* SetNegated)( AstRegion *, int );

   int (* GetRegionFS)( AstRegion * );
   int (* TestRegionFS)( AstRegion * );
   void (* ClearRegionFS)( AstRegion * );
   void (* SetRegionFS)( AstRegion *, int );

   int (* GetClosed)( AstRegion * );
   int (* TestClosed)( AstRegion * );
   void (* ClearClosed)( AstRegion * );
   void (* SetClosed)( AstRegion *, int );

   int (* GetMeshSize)( AstRegion * );
   int (* TestMeshSize)( AstRegion * );
   void (* ClearMeshSize)( AstRegion * );
   void (* SetMeshSize)( AstRegion *, int );

   double (* GetFillFactor)( AstRegion * );
   int (* TestFillFactor)( AstRegion * );
   void (* ClearFillFactor)( AstRegion * );
   void (* SetFillFactor)( AstRegion *, double );

} AstRegionVtab;
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
                           AstFrame *, AstPointSet *, AstRegion * );

/* Vtab initialiser. */
void astInitRegionVtab_( AstRegionVtab *, const char * );

/* Loader. */
AstRegion *astLoadRegion_( void *, size_t, AstRegionVtab *,
                           const char *, AstChannel * );
#endif

/* Prototypes for member functions. */
/* -------------------------------- */

AstFrame *astGetRegionFrame_( AstRegion * );
int astOverlap_( AstRegion *, AstRegion * );
void astNegate_( AstRegion * );

#if defined(AST_LONG_DOUBLE)     /* Not normally implemented */
int astMaskLD_( AstRegion *, AstMapping *, int, int, const int[], const int[], long double [], long double );
#endif
int astMaskB_( AstRegion *, AstMapping *, int, int, const int[], const int[], signed char[], signed char );
int astMaskD_( AstRegion *, AstMapping *, int, int, const int[], const int[], double[], double );
int astMaskF_( AstRegion *, AstMapping *, int, int, const int[], const int[], float[], float );
int astMaskI_( AstRegion *, AstMapping *, int, int, const int[], const int[], int[], int );
int astMaskL_( AstRegion *, AstMapping *, int, int, const int[], const int[], long int[], long int );
int astMaskS_( AstRegion *, AstMapping *, int, int, const int[], const int[], short int[], short int );
int astMaskUB_( AstRegion *, AstMapping *, int, int, const int[], const int[], unsigned char[], unsigned char );
int astMaskUI_( AstRegion *, AstMapping *, int, int, const int[], const int[], unsigned int[], unsigned int );
int astMaskUL_( AstRegion *, AstMapping *, int, int, const int[], const int[], unsigned long int[], unsigned long int );
int astMaskUS_( AstRegion *, AstMapping *, int, int, const int[], const int[], unsigned short int[], unsigned short int );
void astSetUnc_( AstRegion *, AstRegion * );
AstRegion *astGetUnc_( AstRegion *, int );
void astGetRegionBounds_( AstRegion *, double *, double * );

#if defined(astCLASS)            /* Protected */
AstRegion *astMapRegion_( AstRegion *, AstMapping *, AstFrame * );
AstFrame *astRegFrame_( AstRegion * );
AstPointSet *astRegTransform_( AstRegion *, AstPointSet *, int, AstPointSet *, AstFrame ** );
void astRegBaseBox_( AstRegion *, double *, double * );
void astClearUnc_( AstRegion * );
void astRegOverlay_( AstRegion *, AstRegion * );
int astDumpUnc_( AstRegion * );
int astGetBounded_( AstRegion * );
int astTestUnc_( AstRegion * );
int astRegDummyFS_( AstRegion * );
int astRegPins_( AstRegion *, AstPointSet *, AstRegion *, int **);
AstPointSet *astRegMesh_( AstRegion * );
AstPointSet *astRegGrid_( AstRegion * );
AstPointSet *astRegBaseMesh_( AstRegion * );
AstPointSet *astRegBaseGrid_( AstRegion * );
AstPointSet *astBndBaseMesh_( AstRegion *, double *, double * );
AstRegion *astGetUncFrm_( AstRegion *, int );
AstRegion *astGetDefUnc_( AstRegion * );
int astOverlapX_( AstRegion *, AstRegion * );
void astSetRegFS_( AstRegion *, AstFrame * );
double *astRegCentre_( AstRegion *, double *, double **, int, int );
double *astRegTranPoint_( AstRegion *, double *, int, int );

int astGetNegated_( AstRegion * );
int astTestNegated_( AstRegion * );
void astClearNegated_( AstRegion * );
void astSetNegated_( AstRegion *, int );

int astGetRegionFS_( AstRegion * );
int astTestRegionFS_( AstRegion * );
void astClearRegionFS_( AstRegion * );
void astSetRegionFS_( AstRegion *, int );

int astGetMeshSize_( AstRegion * );
int astTestMeshSize_( AstRegion * );
void astClearMeshSize_( AstRegion * );
void astSetMeshSize_( AstRegion *, int );

int astGetClosed_( AstRegion * );
int astTestClosed_( AstRegion * );
void astClearClosed_( AstRegion * );
void astSetClosed_( AstRegion *, int );

double astGetFillFactor_( AstRegion * );
int astTestFillFactor_( AstRegion * );
void astClearFillFactor_( AstRegion * );
void astSetFillFactor_( AstRegion *, double );

#else   /* Public only */
AstRegion *astMapRegionId_( AstRegion *, AstMapping *, AstFrame * );

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
#define astCheckRegion(this) astINVOKE_CHECK(Region,this)

/* Test class membership. */
#define astIsARegion(this) astINVOKE_ISA(Region,this)

#if defined(astCLASS)            /* Protected */

/* Initialiser. */
#define astInitRegion(mem,size,init,vtab,name,frame,pset,acc)\
astINVOKE(O,astInitRegion_(mem,size,init,vtab,name,astCheckFrame(frame),pset,acc))

/* Vtab Initialiser. */
#define astInitRegionVtab(vtab,name) astINVOKE(V,astInitRegionVtab_(vtab,name))
/* Loader. */
#define astLoadRegion(mem,size,vtab,name,channel) \
astINVOKE(O,astLoadRegion_(mem,size,vtab,name,astCheckChannel(channel)))
#endif

/* Interfaces to public member functions. */
/* -------------------------------------- */
/* Here we make use of astCheckRegion to validate Region pointers before
   use. This provides a contextual error report if a pointer to the wrong sort
   of object is supplied. */
#define astGetRegionFrame(this) \
astINVOKE(O,astGetRegionFrame_(astCheckRegion(this)))
#define astNegate(this) \
astINVOKE(V,astNegate_(astCheckRegion(this)))
#define astOverlap(this,that) \
astINVOKE(V,astOverlap_(astCheckRegion(this),astCheckRegion(that)))

#if defined(AST_LONG_DOUBLE)     /* Not normally implemented */
#define astMaskLD(this,map,inside,ndim,lbnd,ubnd,in,val) \
astINVOKE(V,astMaskLD_(astCheckRegion(this),(map?astCheckMapping(map):NULL),inside,ndim,lbnd,ubnd,in,val))
#endif

#define astMaskB(this,map,inside,ndim,lbnd,ubnd,in,val) \
astINVOKE(V,astMaskB_(astCheckRegion(this),(map?astCheckMapping(map):NULL),inside,ndim,lbnd,ubnd,in,val))
#define astMaskD(this,map,inside,ndim,lbnd,ubnd,in,val) \
astINVOKE(V,astMaskD_(astCheckRegion(this),(map?astCheckMapping(map):NULL),inside,ndim,lbnd,ubnd,in,val))
#define astMaskF(this,map,inside,ndim,lbnd,ubnd,in,val) \
astINVOKE(V,astMaskF_(astCheckRegion(this),(map?astCheckMapping(map):NULL),inside,ndim,lbnd,ubnd,in,val))
#define astMaskI(this,map,inside,ndim,lbnd,ubnd,in,val) \
astINVOKE(V,astMaskI_(astCheckRegion(this),(map?astCheckMapping(map):NULL),inside,ndim,lbnd,ubnd,in,val))
#define astMaskL(this,map,inside,ndim,lbnd,ubnd,in,val) \
astINVOKE(V,astMaskL_(astCheckRegion(this),(map?astCheckMapping(map):NULL),inside,ndim,lbnd,ubnd,in,val))
#define astMaskS(this,map,inside,ndim,lbnd,ubnd,in,val) \
astINVOKE(V,astMaskS_(astCheckRegion(this),(map?astCheckMapping(map):NULL),inside,ndim,lbnd,ubnd,in,val))
#define astMaskUB(this,map,inside,ndim,lbnd,ubnd,in,val) \
astINVOKE(V,astMaskUB_(astCheckRegion(this),(map?astCheckMapping(map):NULL),inside,ndim,lbnd,ubnd,in,val))
#define astMaskUI(this,map,inside,ndim,lbnd,ubnd,in,val) \
astINVOKE(V,astMaskUI_(astCheckRegion(this),(map?astCheckMapping(map):NULL),inside,ndim,lbnd,ubnd,in,val))
#define astMaskUL(this,map,inside,ndim,lbnd,ubnd,in,val) \
astINVOKE(V,astMaskUL_(astCheckRegion(this),(map?astCheckMapping(map):NULL),inside,ndim,lbnd,ubnd,in,val))
#define astMaskUS(this,map,inside,ndim,lbnd,ubnd,in,val) \
astINVOKE(V,astMaskUS_(astCheckRegion(this),(map?astCheckMapping(map):NULL),inside,ndim,lbnd,ubnd,in,val))
#define astSetUnc(this,unc) astINVOKE(V,astSetUnc_(astCheckRegion(this),unc?astCheckRegion(unc):NULL))
#define astGetUnc(this,def) astINVOKE(O,astGetUnc_(astCheckRegion(this),def))
#define astGetRegionBounds(this,lbnd,ubnd) astINVOKE(V,astGetRegionBounds_(astCheckRegion(this),lbnd,ubnd))

/* Interfaces to protected member functions. */
/* ----------------------------------------- */
#if defined(astCLASS)            /* Protected */

#define astClearUnc(this) astINVOKE(V,astClearUnc_(astCheckRegion(this)))
#define astDumpUnc(this) astINVOKE(V,astDumpUnc_(astCheckRegion(this)))
#define astGetBounded(this) astINVOKE(V,astGetBounded_(astCheckRegion(this)))
#define astGetUncFrm(this,ifrm) astINVOKE(O,astGetUncFrm_(astCheckRegion(this),ifrm))
#define astGetDefUnc(this) astINVOKE(O,astGetDefUnc_(astCheckRegion(this)))
#define astMapRegion(this,map,frame) astINVOKE(O,astMapRegion_(astCheckRegion(this),astCheckMapping(map),astCheckFrame(frame)))
#define astOverlapX(that,this) astINVOKE(V,astOverlapX_(astCheckRegion(that),astCheckRegion(this)))
#define astRegBaseBox(this,lbnd,ubnd) astINVOKE(V,astRegBaseBox_(astCheckRegion(this),lbnd,ubnd))
#define astRegBaseMesh(this) astINVOKE(O,astRegBaseMesh_(astCheckRegion(this)))
#define astRegBaseGrid(this) astINVOKE(O,astRegBaseGrid_(astCheckRegion(this)))
#define astBndBaseMesh(this,lbnd,ubnd) astINVOKE(O,astBndBaseMesh_(astCheckRegion(this),lbnd,ubnd))
#define astRegCentre(this,cen,ptr,index,ifrm) astINVOKE(V,astRegCentre_(astCheckRegion(this),cen,ptr,index,ifrm))
#define astRegFrame(this) astINVOKE(O,astRegFrame_(astCheckRegion(this)))
#define astRegGrid(this) astINVOKE(O,astRegGrid_(astCheckRegion(this)))
#define astRegMesh(this) astINVOKE(O,astRegMesh_(astCheckRegion(this)))
#define astRegOverlay(this,that) astINVOKE(V,astRegOverlay_(astCheckRegion(this),astCheckRegion(that)))
#define astRegDummyFS(this) astINVOKE(V,astRegDummyFS_(astCheckRegion(this)))
#define astRegPins(this,pset,unc,mask) astINVOKE(V,astRegPins_(astCheckRegion(this),astCheckPointSet(pset),unc?astCheckRegion(unc):unc,mask))
#define astRegTranPoint astRegTranPoint_
#define astSetRegFS(this,frm) astINVOKE(V,astSetRegFS_(astCheckRegion(this),astCheckFrame(frm)))
#define astTestUnc(this) astINVOKE(V,astTestUnc_(astCheckRegion(this)))


/* Since a NULL PointSet pointer is acceptable for "out", we must omit the 
   argument checking in that case. (But unfortunately, "out" then gets 
   evaluated twice - this is unlikely to matter, but is there a better way?) */

#define astRegTransform(this,in,forward,out,frm) \
astINVOKE(O,astRegTransform_(astCheckRegion(this),astCheckPointSet(in),forward,(out)?astCheckPointSet(out):NULL,frm))

#define astClearNegated(this) astINVOKE(V,astClearNegated_(astCheckRegion(this)))
#define astGetNegated(this) astINVOKE(V,astGetNegated_(astCheckRegion(this)))
#define astSetNegated(this,negated) astINVOKE(V,astSetNegated_(astCheckRegion(this),negated))
#define astTestNegated(this) astINVOKE(V,astTestNegated_(astCheckRegion(this)))

#define astClearRegionFS(this) astINVOKE(V,astClearRegionFS_(astCheckRegion(this)))
#define astGetRegionFS(this) astINVOKE(V,astGetRegionFS_(astCheckRegion(this)))
#define astSetRegionFS(this,fs) astINVOKE(V,astSetRegionFS_(astCheckRegion(this),fs))
#define astTestRegionFS(this) astINVOKE(V,astTestRegionFS_(astCheckRegion(this)))

#define astClearMeshSize(this) astINVOKE(V,astClearMeshSize_(astCheckRegion(this)))
#define astGetMeshSize(this) astINVOKE(V,astGetMeshSize_(astCheckRegion(this)))
#define astSetMeshSize(this,meshsize) astINVOKE(V,astSetMeshSize_(astCheckRegion(this),meshsize))
#define astTestMeshSize(this) astINVOKE(V,astTestMeshSize_(astCheckRegion(this)))

#define astClearClosed(this) astINVOKE(V,astClearClosed_(astCheckRegion(this)))
#define astGetClosed(this) astINVOKE(V,astGetClosed_(astCheckRegion(this)))
#define astSetClosed(this,closed) astINVOKE(V,astSetClosed_(astCheckRegion(this),closed))
#define astTestClosed(this) astINVOKE(V,astTestClosed_(astCheckRegion(this)))

#define astClearFillFactor(this) astINVOKE(V,astClearFillFactor_(astCheckRegion(this)))
#define astGetFillFactor(this) astINVOKE(V,astGetFillFactor_(astCheckRegion(this)))
#define astSetFillFactor(this,ff) astINVOKE(V,astSetFillFactor_(astCheckRegion(this),ff))
#define astTestFillFactor(this) astINVOKE(V,astTestFillFactor_(astCheckRegion(this)))

#else  /* Public only */
#define astMapRegion(this,map,frame) astINVOKE(O,astMapRegionId_(astCheckRegion(this),astCheckMapping(map),astCheckFrame(frame)))
#endif

#endif
