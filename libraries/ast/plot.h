#if !defined( PLOT_INCLUDED ) /* Include this file only once */
#define PLOT_INCLUDED
/*
*+
*  Name:
*     plot.h

*  Type:
*     C include file.

*  Purpose:
*     Define the interface to the Plot class.

*  Invocation:
*     #include "plot.h"

*  Description:
*     This include file defines the interface to the Plot class and
*     provides the type definitions, function prototypes and macros, etc.
*     needed to use this class.
*
*     The Plot class provides facilities for producing graphical information
*     describing positions within coordinate systems. These include the
*     creation of annotated coordinate axes, the plotting of markers at given
*     physical positions, etc.

*  Inheritance:
*     The Plot class inherits from the FrameSet class.

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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     DSB: D.S. Berry (Starlink)

*  History:
*     18-SEP-1996 (DSB):
*        Original version.
*     28-OCT-1998 (DSB):
*        Added method astPolyCurve. 
*     12-OCT-1999 (DSB):
*        Allow tick marks to be specified separately for both axes.
*     9-JAN-2001 (DSB):
*        Change argument "in" for astMark and astPolyCurve from type
*        "const double (*)[]" to "const double *".
*     13-JUN-2001 (DSB):
*        Added methods astGenCurve, astGrfSet, astGrfPop, astGrfPush and 
*        attribute Grf.
*     8-JAN-2003 (DSB):
*        Added protected astInitPlotVtab method.
*     13-JAN-2004 (DSB):
*        Added bbox, logticks and logplot to the AstPlot structure. Added 
*        LogPlot and LogTicks accessor methods.
*     19-JAN-2004 (DSB):
*        Added loggap and loglabel to the AstPlot structure. Added 
*        LogGap and LogLabel accessor methods.
*     21-MAR-2005 (DSB):
*        - Added the Clip attribute.
*     24-OCT-2006 (DSB):
*        - Remove duplicated documentation from prologue.
*        - Add ForceExterior attribute.
*-
*/

/* Include files. */
/* ============== */
/* Interface definitions. */
/* ---------------------- */
#include "frameset.h"              /* Parent FrameSet class */

/* C header files. */
/* --------------- */
#include <stddef.h>

/* Macros. */
/* ======= */
#define AST__NPID      15   /* No. of different genuine plot object id's */

#define AST__GATTR	0   /* Identifiers for GRF functions */
#define AST__GFLUSH	1   /* Note, if any items are added or changed here, */
#define AST__GLINE	2   /* make sure that the astGrfFunID function is */
#define AST__GMARK	3   /* updated in plot.c */
#define AST__GTEXT	4
#define AST__GTXEXT	5
#define AST__GSCALES	6
#define AST__GQCH	7
#define AST__GCAP	8

#define AST__NGRFFUN    9   /* No. of Grf functions used by Plot */

#if defined(astCLASS)       /* Protected */
#define AST__MXBRK     100  /* Max. no. of breaks in a drawn curve */
#endif

/* Type Definitions */
/* ================ */

/* Pre-declare the AstPlot structure so that it can be used within the 
   GRF function typedefs. */
struct AstPlot;

/* Interfaces for GRF functions */
/* ---------------------------- */
/* A general interface into which actual Grf functions should be cast 
   before being passed as an argument to astGrfSet. */
typedef void (* AstGrfFun)( void );              

/* Interfaces for specific Grf funstions implemented in C (other languages
   may have different interfaces). */
typedef int (* AstGAttrFun)( AstObject *, int, double, double *, int );
typedef int (* AstGFlushFun)( AstObject * );
typedef int (* AstGLineFun)( AstObject *, int, const float *, const float * );
typedef int (* AstGMarkFun)( AstObject *, int, const float *, const float *, int );
typedef int (* AstGTextFun)( AstObject *, const char *, float, float, const char *, float, float );
typedef int (* AstGCapFun)( AstObject *, int, int );
typedef int (* AstGTxExtFun)( AstObject *, const char *, float, float, const char *, float, float, float *, float * );
typedef int (* AstGScalesFun)( AstObject *, float *, float * );
typedef int (* AstGQchFun)( AstObject *, float *, float * );

/* A general interface into which Grf Wrapper functions should be cast 
   before being passed as an argument to astGrfWrapper. */
typedef void (* AstGrfWrap)( void );              

/* Interfaces for the wrapper functions which wrap specific Grf funstions. */
typedef int (* AstGAttrWrap)( struct AstPlot *, int, double, double *, int );
typedef int (* AstGFlushWrap)( struct AstPlot * );
typedef int (* AstGLineWrap)( struct AstPlot *, int, const float *, const float * );
typedef int (* AstGMarkWrap)( struct AstPlot *, int, const float *, const float *, int );
typedef int (* AstGTextWrap)( struct AstPlot *, const char *, float, float, const char *, float, float );
typedef int (* AstGCapWrap)( struct AstPlot *, int, int );
typedef int (* AstGTxExtWrap)( struct AstPlot *, const char *, float, float, const char *, float, float, float *, float * );
typedef int (* AstGScalesWrap)( struct AstPlot *, float *, float * );
typedef int (* AstGQchWrap)( struct AstPlot *, float *, float * );

/* A structure in which a collection of Grf function pointers can be
   stored. */
typedef struct AstGrfPtrs {
   AstGrfFun grffun[AST__NGRFFUN];
   AstGAttrWrap GAttr;
   AstGFlushWrap GFlush;
   AstGLineWrap GLine;
   AstGMarkWrap GMark;
   AstGTextWrap GText;
   AstGCapWrap GCap;
   AstGTxExtWrap GTxExt;
   AstGScalesWrap GScales;
   AstGQchWrap GQch;
} AstGrfPtrs;

/* Structure holding current graphical attribute values for text. */
typedef struct AstGat {
   float rise;
   double size;
   double width;
   double col;
   double font;
   double style;
} AstGat;

/* Plot structure. */
/* ------------------- */
/* This structure contains all information that is unique to each object in
   the class (e.g. its instance variables). */
typedef struct AstPlot {

/* Attributes inherited from the parent class. */
   AstFrameSet parent;           /* Parent class structure */

/* Attributes specific to objects in this class. */
   double *clip_lbnd;
   double *clip_ubnd;
   double centre[ 2 ];
   double gap[ 2 ];
   double loggap[ 2 ];
   double labelat[ 2 ];
   double majticklen[ 2 ];
   double minticklen[ 2 ];
   double numlabgap[ 2 ];
   double size[ AST__NPID ];
   double textlabgap[ 2 ];
   double titlegap;
   double tol;
   double ucentre[ 2 ];
   double ugap[ 2 ];
   double uloggap[ 2 ];
   double ulblat[ 2 ];
   double umjtkln[ 2 ];
   double width[ AST__NPID ];
   double xhi;
   double xlo;
   double yhi;
   double ylo;
   double bbox[ 4 ];
   int border;
   int clip_axes;
   int clip_frame;
   int clip;
   int clipop;
   int colour[ AST__NPID ];
   int drawaxes[ 2 ];
   int abbrev[ 2 ];
   int escape;
   int drawtitle;
   int edge[ 2 ];
   int font[ AST__NPID ];
   int grf;
   int grid;
   int invisible;
   int labelling;
   int labelunits[ 2 ];
   int labelup[ 2 ];
   int mintick[ 2 ];
   int numlab[ 2 ];
   int style[ AST__NPID ];
   int textlab[ 2 ];
   int tickall;
   int forceexterior;
   int uborder;
   int uedge[ 2 ];
   int ugrid;
   int ulbling;
   int ulbunit[ 2 ];
   int ulgtk[ 2 ];
   int ulglb[ 2 ];
   int umintk[ 2 ];
   int utxtlb[ 2 ];
   int xrev;
   int yrev;      
   int ink;
   int logplot[ 2 ];
   int logticks[ 2 ];
   int loglabel[ 2 ];
   AstGrfFun grffun[AST__NGRFFUN];
   AstGAttrWrap GAttr;
   AstGFlushWrap GFlush;
   AstGLineWrap GLine;
   AstGMarkWrap GMark;
   AstGTextWrap GText;
   AstGCapWrap GCap;
   AstGTxExtWrap GTxExt;
   AstGScalesWrap GScales;
   AstGQchWrap GQch;
   AstGrfPtrs *grfstack;
   int grfnstack;
   AstGat **gat;
   int ngat;
   AstObject *grfcontextId;
   float hmarkx;
   float hmarky;

} AstPlot;

/* Virtual function table. */
/* ----------------------- */
/* This table contains all information that is the same for all
   objects in the class (e.g. pointers to its virtual functions). */
#if defined(astCLASS)            /* Protected */

typedef struct AstPlotVtab {

/* Properties (e.g. methods) inherited from the parent class. */
   AstFrameSetVtab FrameSet_vtab;/* Parent class virtual function table */

/* Unique flag value to determine class membership. */
   int *check;                   /* Check value */

/* Properties (e.g. methods) specific to this class. */
   int (* Border)( AstPlot * );
   void (* BoundingBox)( AstPlot *, float[2], float[2] );
   void (* SetGrfContext)( AstPlot *, AstObject * );
   void (* Clip)( AstPlot *, int, const double [], const double [] );
   int (* CvBrk)( AstPlot *, int, double *, double *, double * );
   void (* GridLine)( AstPlot *, int, const double [], double );
   void (* Curve)( AstPlot *, const double [], const double [] );
   void (* GenCurve)( AstPlot *, AstMapping * );
   void (* PolyCurve)( AstPlot *, int, int, int, const double * );
   void (* GrfSet)( AstPlot *, const char *, AstGrfFun );
   void (* GrfPush)( AstPlot * );
   void (* GrfPop)( AstPlot * );
   void (* GrfWrapper)( AstPlot *, const char *, AstGrfWrap );
   void (* Grid)( AstPlot * ); 
   void (* Mark)( AstPlot *, int, int, int, const double *, int  ); 
   void (* Text)( AstPlot *, const char *, const double [], const float [2], const char * );

   double (* GetTol)( AstPlot * );
   int (* TestTol)( AstPlot * );
   void (* SetTol)( AstPlot *, double );
   void (* ClearTol)( AstPlot * );

   int (* GetGrid)( AstPlot * );
   int (* TestGrid)( AstPlot * );
   void (* SetGrid)( AstPlot *, int );
   void (* ClearGrid)( AstPlot * );

   int (* GetTickAll)( AstPlot * );
   int (* TestTickAll)( AstPlot * );
   void (* SetTickAll)( AstPlot *, int );
   void (* ClearTickAll)( AstPlot * );

   int (* GetForceExterior)( AstPlot * );
   int (* TestForceExterior)( AstPlot * );
   void (* SetForceExterior)( AstPlot *, int );
   void (* ClearForceExterior)( AstPlot * );

   int (* GetInvisible)( AstPlot * );
   int (* TestInvisible)( AstPlot * );
   void (* SetInvisible)( AstPlot *, int );
   void (* ClearInvisible)( AstPlot * );

   int (* GetBorder)( AstPlot * );
   int (* TestBorder)( AstPlot * );
   void (* SetBorder)( AstPlot *, int );
   void (* ClearBorder)( AstPlot * );

   int (* GetClipOp)( AstPlot * );
   int (* TestClipOp)( AstPlot * );
   void (* SetClipOp)( AstPlot *, int );
   void (* ClearClipOp)( AstPlot * );

   int (* GetClip)( AstPlot * );
   int (* TestClip)( AstPlot * );
   void (* SetClip)( AstPlot *, int );
   void (* ClearClip)( AstPlot * );

   int (* GetGrf)( AstPlot * );
   int (* TestGrf)( AstPlot * );
   void (* SetGrf)( AstPlot *, int );
   void (* ClearGrf)( AstPlot * );

   int (* GetDrawTitle)( AstPlot * );
   int (* TestDrawTitle)( AstPlot * );
   void (* SetDrawTitle)( AstPlot *, int );
   void (* ClearDrawTitle)( AstPlot * );

   int (* GetLabelUp)( AstPlot *, int );
   int (* TestLabelUp)( AstPlot *, int );
   void (* SetLabelUp)( AstPlot *, int, int );
   void (* ClearLabelUp)( AstPlot *, int );

   int (* GetLogPlot)( AstPlot *, int );
   int (* TestLogPlot)( AstPlot *, int );
   void (* SetLogPlot)( AstPlot *, int, int );
   void (* ClearLogPlot)( AstPlot *, int );

   int (* GetLogTicks)( AstPlot *, int );
   int (* TestLogTicks)( AstPlot *, int );
   void (* SetLogTicks)( AstPlot *, int, int );
   void (* ClearLogTicks)( AstPlot *, int );

   int (* GetLogLabel)( AstPlot *, int );
   int (* TestLogLabel)( AstPlot *, int );
   void (* SetLogLabel)( AstPlot *, int, int );
   void (* ClearLogLabel)( AstPlot *, int );

   int (* GetDrawAxes)( AstPlot *, int );
   int (* TestDrawAxes)( AstPlot *, int );
   void (* SetDrawAxes)( AstPlot *, int, int );
   void (* ClearDrawAxes)( AstPlot *, int );

   int (* GetAbbrev)( AstPlot *, int );
   int (* TestAbbrev)( AstPlot *, int );
   void (* SetAbbrev)( AstPlot *, int, int );
   void (* ClearAbbrev)( AstPlot *, int );

   int (* GetEscape)( AstPlot * );
   int (* TestEscape)( AstPlot * );
   void (* SetEscape)( AstPlot *, int );
   void (* ClearEscape)( AstPlot * );

   int (* GetLabelling)( AstPlot * );
   int (* TestLabelling)( AstPlot * );
   void (* SetLabelling)( AstPlot *, int );
   void (* ClearLabelling)( AstPlot * );

   double (* GetMajTickLen)( AstPlot *, int );
   int (* TestMajTickLen)( AstPlot *, int );
   void (* SetMajTickLen)( AstPlot *, int, double );
   void (* ClearMajTickLen)( AstPlot *, int );

   double (* GetMinTickLen)( AstPlot *, int );
   int (* TestMinTickLen)( AstPlot *, int );
   void (* SetMinTickLen)( AstPlot *, int, double );
   void (* ClearMinTickLen)( AstPlot *, int );

   double (* GetNumLabGap)( AstPlot *, int );
   int (* TestNumLabGap)( AstPlot *, int );
   void (* SetNumLabGap)( AstPlot *, int, double );
   void (* ClearNumLabGap)( AstPlot *, int );

   double (* GetTextLabGap)( AstPlot *, int );
   int (* TestTextLabGap)( AstPlot *, int );
   void (* SetTextLabGap)( AstPlot *, int, double );
   void (* ClearTextLabGap)( AstPlot *, int );

   double (* GetTitleGap)( AstPlot * );
   int (* TestTitleGap)( AstPlot * );
   void (* SetTitleGap)( AstPlot *, double );
   void (* ClearTitleGap)( AstPlot * );

   double (* GetLabelAt)( AstPlot *, int  );
   int (* TestLabelAt)( AstPlot *, int  );
   void (* SetLabelAt)( AstPlot *, int, double );
   void (* ClearLabelAt)( AstPlot *, int );

   double (* GetGap)( AstPlot *, int  );
   int (* TestGap)( AstPlot *, int  );
   void (* SetGap)( AstPlot *, int, double );
   void (* ClearGap)( AstPlot *, int );

   double (* GetLogGap)( AstPlot *, int  );
   int (* TestLogGap)( AstPlot *, int  );
   void (* SetLogGap)( AstPlot *, int, double );
   void (* ClearLogGap)( AstPlot *, int );

   double (* GetCentre)( AstPlot *, int  );
   int (* TestCentre)( AstPlot *, int  );
   void (* SetCentre)( AstPlot *, int, double );
   void (* ClearCentre)( AstPlot *, int );

   int (* GetEdge)( AstPlot *, int );
   int (* TestEdge)( AstPlot *, int );
   void (* SetEdge)( AstPlot *, int, int );
   void (* ClearEdge)( AstPlot *, int );

   int (* GetNumLab)( AstPlot *, int );
   int (* TestNumLab)( AstPlot *, int );
   void (* SetNumLab)( AstPlot *, int, int );
   void (* ClearNumLab)( AstPlot *, int );

   int (* GetMinTick)( AstPlot *, int );
   int (* TestMinTick)( AstPlot *, int );
   void (* SetMinTick)( AstPlot *, int, int );
   void (* ClearMinTick)( AstPlot *, int );

   int (* GetTextLab)( AstPlot *, int );
   int (* TestTextLab)( AstPlot *, int );
   void (* SetTextLab)( AstPlot *, int, int );
   void (* ClearTextLab)( AstPlot *, int );

   int (* GetLabelUnits)( AstPlot *, int );
   int (* TestLabelUnits)( AstPlot *, int );
   void (* SetLabelUnits)( AstPlot *, int, int );
   void (* ClearLabelUnits)( AstPlot *, int );

   int (* GetStyle)( AstPlot *, int );
   int (* TestStyle)( AstPlot *, int );
   void (* SetStyle)( AstPlot *, int, int );
   void (* ClearStyle)( AstPlot *, int );

   int (* GetFont)( AstPlot *, int );
   int (* TestFont)( AstPlot *, int );
   void (* SetFont)( AstPlot *, int, int );
   void (* ClearFont)( AstPlot *, int );

   int (* GetColour)( AstPlot *, int );
   int (* TestColour)( AstPlot *, int );
   void (* SetColour)( AstPlot *, int, int );
   void (* ClearColour)( AstPlot *, int );

   double (* GetWidth)( AstPlot *, int );
   int (* TestWidth)( AstPlot *, int );
   void (* SetWidth)( AstPlot *, int, double );
   void (* ClearWidth)( AstPlot *, int );

   double (* GetSize)( AstPlot *, int );
   int (* TestSize)( AstPlot *, int );
   void (* SetSize)( AstPlot *, int, double );
   void (* ClearSize)( AstPlot *, int );

   int (* GetInk)( AstPlot * );
   int (* TestInk)( AstPlot * );
   void (* SetInk)( AstPlot *, int );
   void (* ClearInk)( AstPlot * );

} AstPlotVtab;
#endif

/* Function prototypes. */
/* ==================== */
/* Prototypes for standard class functions. */
/* ---------------------------------------- */
astPROTO_CHECK(Plot)          /* Check class membership */
astPROTO_ISA(Plot)            /* Test class membership */

/* Constructor. */
#if defined(astCLASS)            /* Protected. */
AstPlot *astPlot_( void *, const float *, const double *, const char *, ... );
#else
AstPlot *astPlotId_( void *, const float [], const double [], const char *, ... );
#endif

#if defined(astCLASS)            /* Protected */

/* Initialiser. */
AstPlot *astInitPlot_( void *, size_t, int, AstPlotVtab *, 
                       const char *, AstFrame *, const float *, const double * );

/* Vtab initialiser. */
void astInitPlotVtab_( AstPlotVtab *, const char * );

/* Loader. */
AstPlot *astLoadPlot_( void *, size_t, AstPlotVtab *,
                       const char *, AstChannel *channel );
#endif

/* Prototypes for member functions. */
/* -------------------------------- */
   const char *astStripEscapes_( const char * );
   int astFindEscape_( const char *, int *, int *, int * );
   int astBorder_( AstPlot * );
   void astSetGrfContext_( AstPlot *, AstObject * );
   void astBoundingBox_( AstPlot *, float[2], float[2] );
   void astClip_( AstPlot *, int, const double [], const double [] );
   void astGridLine_( AstPlot *, int, const double [], double );
   void astCurve_( AstPlot *, const double [], const double [] );
   void astGrid_( AstPlot * );
   void astMark_( AstPlot *, int, int, int, const double *, int  ); 
   void astGrfSet_( AstPlot *, const char *, AstGrfFun );
   void astGrfPush_( AstPlot * );
   void astGrfPop_( AstPlot * );
   void astGenCurve_( AstPlot *, AstMapping * );
   void astPolyCurve_( AstPlot *, int, int, int, const double * );
   void astText_( AstPlot *, const char *, const double [], const float [2], const char * );

   void astGrfWrapper_( AstPlot *, const char *, AstGrfWrap );
   int astGrfFunID_( const char *, const char *, const char *  );

#if defined(astCLASS)            /* Protected */
   int astCvBrk_( AstPlot *, int, double *, double *, double * );

   double astGetTol_( AstPlot * );
   int astTestTol_( AstPlot * );
   void astSetTol_( AstPlot *, double );
   void astClearTol_( AstPlot * );

   int astGetGrid_( AstPlot * );
   int astTestGrid_( AstPlot * );
   void astSetGrid_( AstPlot *, int );
   void astClearGrid_( AstPlot * );

   int astGetTickAll_( AstPlot * );
   int astTestTickAll_( AstPlot * );
   void astSetTickAll_( AstPlot *, int );
   void astClearTickAll_( AstPlot * );

   int astGetForceExterior_( AstPlot * );
   int astTestForceExterior_( AstPlot * );
   void astSetForceExterior_( AstPlot *, int );
   void astClearForceExterior_( AstPlot * );

   int astGetInvisible_( AstPlot * );
   int astTestInvisible_( AstPlot * );
   void astSetInvisible_( AstPlot *, int );
   void astClearInvisible_( AstPlot * );

   int astGetBorder_( AstPlot * );
   int astTestBorder_( AstPlot * );
   void astSetBorder_( AstPlot *, int );
   void astClearBorder_( AstPlot * );

   int astGetClip_( AstPlot * );
   int astTestClip_( AstPlot * );
   void astSetClip_( AstPlot *, int );
   void astClearClip_( AstPlot * );

   int astGetClipOp_( AstPlot * );
   int astTestClipOp_( AstPlot * );
   void astSetClipOp_( AstPlot *, int );
   void astClearClipOp_( AstPlot * );

   int astGetGrf_( AstPlot * );
   int astTestGrf_( AstPlot * );
   void astSetGrf_( AstPlot *, int );
   void astClearGrf_( AstPlot * );

   int astGetDrawTitle_( AstPlot * );
   int astTestDrawTitle_( AstPlot * );
   void astSetDrawTitle_( AstPlot *, int );
   void astClearDrawTitle_( AstPlot * );

   int astGetLabelUp_( AstPlot *, int );
   int astTestLabelUp_( AstPlot *, int );
   void astSetLabelUp_( AstPlot *, int, int );
   void astClearLabelUp_( AstPlot *, int );

   int astGetLogPlot_( AstPlot *, int );
   int astTestLogPlot_( AstPlot *, int );
   void astSetLogPlot_( AstPlot *, int, int );
   void astClearLogPlot_( AstPlot *, int );

   int astGetLogTicks_( AstPlot *, int );
   int astTestLogTicks_( AstPlot *, int );
   void astSetLogTicks_( AstPlot *, int, int );
   void astClearLogTicks_( AstPlot *, int );

   int astGetLogLabel_( AstPlot *, int );
   int astTestLogLabel_( AstPlot *, int );
   void astSetLogLabel_( AstPlot *, int, int );
   void astClearLogLabel_( AstPlot *, int );

   int astGetDrawAxes_( AstPlot *, int );
   int astTestDrawAxes_( AstPlot *, int );
   void astSetDrawAxes_( AstPlot *, int, int );
   void astClearDrawAxes_( AstPlot *, int );

   int astGetAbbrev_( AstPlot *, int );
   int astTestAbbrev_( AstPlot *, int );
   void astSetAbbrev_( AstPlot *, int, int );
   void astClearAbbrev_( AstPlot *, int );

   int astGetEscape_( AstPlot * );
   int astTestEscape_( AstPlot * );
   void astSetEscape_( AstPlot *, int );
   void astClearEscape_( AstPlot * );

   double astGetLabelAt_( AstPlot *, int  );
   int astTestLabelAt_( AstPlot *, int  );
   void astSetLabelAt_( AstPlot *, int, double );
   void astClearLabelAt_( AstPlot *, int );

   double astGetGap_( AstPlot *, int  );
   int astTestGap_( AstPlot *, int  );
   void astSetGap_( AstPlot *, int, double );
   void astClearGap_( AstPlot *, int );

   double astGetLogGap_( AstPlot *, int  );
   int astTestLogGap_( AstPlot *, int  );
   void astSetLogGap_( AstPlot *, int, double );
   void astClearLogGap_( AstPlot *, int );

   double astGetCentre_( AstPlot *, int  );
   int astTestCentre_( AstPlot *, int  );
   void astSetCentre_( AstPlot *, int, double );
   void astClearCentre_( AstPlot *, int );

   int astGetLabelling_( AstPlot * );
   int astTestLabelling_( AstPlot * );
   void astSetLabelling_( AstPlot *, int );
   void astClearLabelling_( AstPlot * );

   double astGetMajTickLen_( AstPlot *, int );
   int astTestMajTickLen_( AstPlot *, int );
   void astSetMajTickLen_( AstPlot *, int, double );
   void astClearMajTickLen_( AstPlot *, int );

   double astGetMinTickLen_( AstPlot *, int );
   int astTestMinTickLen_( AstPlot *, int );
   void astSetMinTickLen_( AstPlot *, int, double );
   void astClearMinTickLen_( AstPlot *, int );

   double astGetNumLabGap_( AstPlot *, int );
   int astTestNumLabGap_( AstPlot *, int );
   void astSetNumLabGap_( AstPlot *, int, double );
   void astClearNumLabGap_( AstPlot *, int );

   double astGetTextLabGap_( AstPlot *, int );
   int astTestTextLabGap_( AstPlot *, int );
   void astSetTextLabGap_( AstPlot *, int, double );
   void astClearTextLabGap_( AstPlot *, int );

   double astGetTitleGap_( AstPlot * );
   int astTestTitleGap_( AstPlot * );
   void astSetTitleGap_( AstPlot *, double );
   void astClearTitleGap_( AstPlot * );

   int astGetEdge_( AstPlot *, int );
   int astTestEdge_( AstPlot *, int );
   void astSetEdge_( AstPlot *, int, int );
   void astClearEdge_( AstPlot *, int );

   int astGetMinTick_( AstPlot *, int );
   int astTestMinTick_( AstPlot *, int );
   void astSetMinTick_( AstPlot *, int, int );
   void astClearMinTick_( AstPlot *, int );

   int astGetNumLab_( AstPlot *, int );
   int astTestNumLab_( AstPlot *, int );
   void astSetNumLab_( AstPlot *, int, int );
   void astClearNumLab_( AstPlot *, int );

   int astGetTextLab_( AstPlot *, int );
   int astTestTextLab_( AstPlot *, int );
   void astSetTextLab_( AstPlot *, int, int );
   void astClearTextLab_( AstPlot *, int );

   int astGetLabelUnits_( AstPlot *, int );
   int astTestLabelUnits_( AstPlot *, int );
   void astSetLabelUnits_( AstPlot *, int, int );
   void astClearLabelUnits_( AstPlot *, int );

   int astGetStyle_( AstPlot *, int );
   int astTestStyle_( AstPlot *, int );
   void astSetStyle_( AstPlot *, int, int );
   void astClearStyle_( AstPlot *, int );

   int astGetFont_( AstPlot *, int );
   int astTestFont_( AstPlot *, int );
   void astSetFont_( AstPlot *, int, int );
   void astClearFont_( AstPlot *, int );

   int astGetColour_( AstPlot *, int );
   int astTestColour_( AstPlot *, int );
   void astSetColour_( AstPlot *, int, int );
   void astClearColour_( AstPlot *, int );

   double astGetWidth_( AstPlot *, int );
   int astTestWidth_( AstPlot *, int );
   void astSetWidth_( AstPlot *, int, double );
   void astClearWidth_( AstPlot *, int );

   double astGetSize_( AstPlot *, int );
   int astTestSize_( AstPlot *, int );
   void astSetSize_( AstPlot *, int, double );
   void astClearSize_( AstPlot *, int );

   int astGetInk_( AstPlot * );
   int astTestInk_( AstPlot * );
   void astSetInk_( AstPlot *, int );
   void astClearInk_( AstPlot * );
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
#define astCheckPlot(this) astINVOKE_CHECK(Plot,this)

/* Test class membership. */
#define astIsAPlot(this) astINVOKE_ISA(Plot,this)

#if defined(astCLASS)            /* Protected */
#define astPlot astINVOKE(F,astPlot_)
#else
#define astPlot astINVOKE(F,astPlotId_)
#endif

#if defined(astCLASS)            /* Protected */

/* Initialiser. */
#define astInitPlot(mem,size,init,vtab,name,frame,graph,base) \
astINVOKE(O,astInitPlot_(mem,size,init,vtab,name,frame,graph,base))

/* Vtab Initialiser. */
#define astInitPlotVtab(vtab,name) astINVOKE(V,astInitPlotVtab_(vtab,name))
/* Loader. */
#define astLoadPlot(mem,size,vtab,name,channel) \
astINVOKE(O,astLoadPlot_(mem,size,vtab,name,astCheckChannel(channel)))
#endif

/* Interfaces to member functions. */
/* ------------------------------- */
/* Here we make use of astCheckPlot (et al.) to validate Plot
   pointers before use. This provides a contextual error report if a
   pointer to the wrong sort of object is supplied. */


#define astSetGrfContext(this,grfcon) \
astINVOKE(V,astSetGrfContext_(astCheckPlot(this),astCheckObject(grfcon)))

#define astBorder(this) \
astINVOKE(V,astBorder_(astCheckPlot(this)))

#define astBoundingBox(this,lbnd,ubnd) \
astINVOKE(V,astBoundingBox_(astCheckPlot(this),lbnd,ubnd))

#define astClip(this,iframe,lbnd,ubnd) \
astINVOKE(V,astClip_(astCheckPlot(this),iframe,lbnd,ubnd))

#define astMark(this,nmark,ncoord,indim,in,type) \
astINVOKE(V,astMark_(astCheckPlot(this),nmark,ncoord,indim,in,type))

#define astText(this,text,pos,up,just) \
astINVOKE(V,astText_(astCheckPlot(this),text,pos,up,just))

#define astGrid(this) \
astINVOKE(V,astGrid_(astCheckPlot(this)))

#define astGridLine(this,axis,start,length) \
astINVOKE(V,astGridLine_(astCheckPlot(this),axis,start,length))

#define astCurve(this,start,finish) \
astINVOKE(V,astCurve_(astCheckPlot(this),start,finish))

#define astGenCurve(this,map) \
astINVOKE(V,astGenCurve_(astCheckPlot(this),astCheckMapping(map)))

#define astPolyCurve(this,npoint,ncoord,dim,in) \
astINVOKE(V,astPolyCurve_(astCheckPlot(this),npoint,ncoord,dim,in))

#define astGrfSet(this,name,fun) \
astINVOKE(V,astGrfSet_(astCheckPlot(this),name,fun))

#define astGrfPush(this) \
astINVOKE(V,astGrfPush_(astCheckPlot(this)))

#define astGrfPop(this) \
astINVOKE(V,astGrfPop_(astCheckPlot(this)))


#define astGrfFunID astGrfFunID_
#define astFindEscape astFindEscape_
#define astStripEscapes(text) astStripEscapes_(text)

#define astGrfWrapper(this,name,wrapper) \
astINVOKE(V,astGrfWrapper_(astCheckPlot(this),name,wrapper))

#if defined(astCLASS)            /* Protected */

#define astCvBrk(this,ibrk,brk,vbrk,len) \
astINVOKE(V,astCvBrk_(astCheckPlot(this),ibrk,brk,vbrk,len))

#define astClearTol(this) \
astINVOKE(V,astClearTol_(astCheckPlot(this)))
#define astGetTol(this) \
astINVOKE(V,astGetTol_(astCheckPlot(this)))
#define astSetTol(this,tol) \
astINVOKE(V,astSetTol_(astCheckPlot(this),tol))
#define astTestTol(this) \
astINVOKE(V,astTestTol_(astCheckPlot(this)))

#define astClearGrid(this) \
astINVOKE(V,astClearGrid_(astCheckPlot(this)))
#define astGetGrid(this) \
astINVOKE(V,astGetGrid_(astCheckPlot(this)))
#define astSetGrid(this,grid) \
astINVOKE(V,astSetGrid_(astCheckPlot(this),grid))
#define astTestGrid(this) \
astINVOKE(V,astTestGrid_(astCheckPlot(this)))

#define astClearInk(this) \
astINVOKE(V,astClearInk_(astCheckPlot(this)))
#define astGetInk(this) \
astINVOKE(V,astGetInk_(astCheckPlot(this)))
#define astSetInk(this,ink) \
astINVOKE(V,astSetInk_(astCheckPlot(this),ink))
#define astTestInk(this) \
astINVOKE(V,astTestInk_(astCheckPlot(this)))

#define astClearTickAll(this) \
astINVOKE(V,astClearTickAll_(astCheckPlot(this)))
#define astGetTickAll(this) \
astINVOKE(V,astGetTickAll_(astCheckPlot(this)))
#define astSetTickAll(this,tickall) \
astINVOKE(V,astSetTickAll_(astCheckPlot(this),tickall))
#define astTestTickAll(this) \
astINVOKE(V,astTestTickAll_(astCheckPlot(this)))

#define astClearForceExterior(this) \
astINVOKE(V,astClearForceExterior_(astCheckPlot(this)))
#define astGetForceExterior(this) \
astINVOKE(V,astGetForceExterior_(astCheckPlot(this)))
#define astSetForceExterior(this,frcext) \
astINVOKE(V,astSetForceExterior_(astCheckPlot(this),frcext))
#define astTestForceExterior(this) \
astINVOKE(V,astTestForceExterior_(astCheckPlot(this)))

#define astClearBorder(this) \
astINVOKE(V,astClearBorder_(astCheckPlot(this)))
#define astGetBorder(this) \
astINVOKE(V,astGetBorder_(astCheckPlot(this)))
#define astSetBorder(this,border) \
astINVOKE(V,astSetBorder_(astCheckPlot(this),border))
#define astTestBorder(this) \
astINVOKE(V,astTestBorder_(astCheckPlot(this)))

#define astClearClip(this) \
astINVOKE(V,astClearClip_(astCheckPlot(this)))
#define astGetClip(this) \
astINVOKE(V,astGetClip_(astCheckPlot(this)))
#define astSetClip(this,clip) \
astINVOKE(V,astSetClip_(astCheckPlot(this),clip))
#define astTestClip(this) \
astINVOKE(V,astTestClip_(astCheckPlot(this)))

#define astClearClipOp(this) \
astINVOKE(V,astClearClipOp_(astCheckPlot(this)))
#define astGetClipOp(this) \
astINVOKE(V,astGetClipOp_(astCheckPlot(this)))
#define astSetClipOp(this,clipop) \
astINVOKE(V,astSetClipOp_(astCheckPlot(this),clipop))
#define astTestClipOp(this) \
astINVOKE(V,astTestClipOp_(astCheckPlot(this)))

#define astClearInvisible(this) \
astINVOKE(V,astClearInvisible_(astCheckPlot(this)))
#define astGetInvisible(this) \
astINVOKE(V,astGetInvisible_(astCheckPlot(this)))
#define astSetInvisible(this,invisible) \
astINVOKE(V,astSetInvisible_(astCheckPlot(this),invisible))
#define astTestInvisible(this) \
astINVOKE(V,astTestInvisible_(astCheckPlot(this)))

#define astClearGrf(this) \
astINVOKE(V,astClearGrf_(astCheckPlot(this)))
#define astGetGrf(this) \
astINVOKE(V,astGetGrf_(astCheckPlot(this)))
#define astSetGrf(this,grf) \
astINVOKE(V,astSetGrf_(astCheckPlot(this),grf))
#define astTestGrf(this) \
astINVOKE(V,astTestGrf_(astCheckPlot(this)))

#define astClearDrawTitle(this) \
astINVOKE(V,astClearDrawTitle_(astCheckPlot(this)))
#define astGetDrawTitle(this) \
astINVOKE(V,astGetDrawTitle_(astCheckPlot(this)))
#define astSetDrawTitle(this,drawtitle) \
astINVOKE(V,astSetDrawTitle_(astCheckPlot(this),drawtitle))
#define astTestDrawTitle(this) \
astINVOKE(V,astTestDrawTitle_(astCheckPlot(this)))

#define astClearDrawAxes(this,axis) \
astINVOKE(V,astClearDrawAxes_(astCheckPlot(this),axis))
#define astGetDrawAxes(this,axis) \
astINVOKE(V,astGetDrawAxes_(astCheckPlot(this),axis))
#define astSetDrawAxes(this,axis,drawaxes) \
astINVOKE(V,astSetDrawAxes_(astCheckPlot(this),axis,drawaxes))
#define astTestDrawAxes(this,axis) \
astINVOKE(V,astTestDrawAxes_(astCheckPlot(this),axis))

#define astClearAbbrev(this,axis) \
astINVOKE(V,astClearAbbrev_(astCheckPlot(this),axis))
#define astGetAbbrev(this,axis) \
astINVOKE(V,astGetAbbrev_(astCheckPlot(this),axis))
#define astSetAbbrev(this,axis,abbrev) \
astINVOKE(V,astSetAbbrev_(astCheckPlot(this),axis,abbrev))
#define astTestAbbrev(this,axis) \
astINVOKE(V,astTestAbbrev_(astCheckPlot(this),axis))

#define astClearEscape(this) \
astINVOKE(V,astClearEscape_(astCheckPlot(this)))
#define astGetEscape(this) \
astINVOKE(V,astGetEscape_(astCheckPlot(this)))
#define astSetEscape(this,escape) \
astINVOKE(V,astSetEscape_(astCheckPlot(this),escape))
#define astTestEscape(this) \
astINVOKE(V,astTestEscape_(astCheckPlot(this)))

#define astClearLabelAt(this,axis) \
astINVOKE(V,astClearLabelAt_(astCheckPlot(this),axis))
#define astGetLabelAt(this,axis) \
astINVOKE(V,astGetLabelAt_(astCheckPlot(this),axis))
#define astSetLabelAt(this,axis,labelat) \
astINVOKE(V,astSetLabelAt_(astCheckPlot(this),axis,labelat))
#define astTestLabelAt(this,axis) \
astINVOKE(V,astTestLabelAt_(astCheckPlot(this),axis))

#define astClearGap(this,axis) \
astINVOKE(V,astClearGap_(astCheckPlot(this),axis))
#define astGetGap(this,axis) \
astINVOKE(V,astGetGap_(astCheckPlot(this),axis))
#define astSetGap(this,axis,gap) \
astINVOKE(V,astSetGap_(astCheckPlot(this),axis,gap))
#define astTestGap(this,axis) \
astINVOKE(V,astTestGap_(astCheckPlot(this),axis))

#define astClearLogGap(this,axis) \
astINVOKE(V,astClearLogGap_(astCheckPlot(this),axis))
#define astGetLogGap(this,axis) \
astINVOKE(V,astGetLogGap_(astCheckPlot(this),axis))
#define astSetLogGap(this,axis,gap) \
astINVOKE(V,astSetLogGap_(astCheckPlot(this),axis,gap))
#define astTestLogGap(this,axis) \
astINVOKE(V,astTestLogGap_(astCheckPlot(this),axis))

#define astClearCentre(this,axis) \
astINVOKE(V,astClearCentre_(astCheckPlot(this),axis))
#define astGetCentre(this,axis) \
astINVOKE(V,astGetCentre_(astCheckPlot(this),axis))
#define astSetCentre(this,axis,centre) \
astINVOKE(V,astSetCentre_(astCheckPlot(this),axis,centre))
#define astTestCentre(this,axis) \
astINVOKE(V,astTestCentre_(astCheckPlot(this),axis))

#define astClearMajTickLen(this,axis) \
astINVOKE(V,astClearMajTickLen_(astCheckPlot(this),axis))
#define astGetMajTickLen(this,axis) \
astINVOKE(V,astGetMajTickLen_(astCheckPlot(this),axis))
#define astSetMajTickLen(this,axis,majticklen) \
astINVOKE(V,astSetMajTickLen_(astCheckPlot(this),axis,majticklen))
#define astTestMajTickLen(this,axis) \
astINVOKE(V,astTestMajTickLen_(astCheckPlot(this),axis))

#define astClearMinTickLen(this,axis) \
astINVOKE(V,astClearMinTickLen_(astCheckPlot(this),axis))
#define astGetMinTickLen(this,axis) \
astINVOKE(V,astGetMinTickLen_(astCheckPlot(this),axis))
#define astSetMinTickLen(this,axis,minticklen) \
astINVOKE(V,astSetMinTickLen_(astCheckPlot(this),axis,minticklen))
#define astTestMinTickLen(this,axis) \
astINVOKE(V,astTestMinTickLen_(astCheckPlot(this),axis))

#define astClearNumLabGap(this,axis) \
astINVOKE(V,astClearNumLabGap_(astCheckPlot(this),axis))
#define astGetNumLabGap(this,axis) \
astINVOKE(V,astGetNumLabGap_(astCheckPlot(this),axis))
#define astSetNumLabGap(this,axis,numlabgap) \
astINVOKE(V,astSetNumLabGap_(astCheckPlot(this),axis,numlabgap))
#define astTestNumLabGap(this,axis) \
astINVOKE(V,astTestNumLabGap_(astCheckPlot(this),axis))

#define astClearTextLabGap(this,axis) \
astINVOKE(V,astClearTextLabGap_(astCheckPlot(this),axis))
#define astGetTextLabGap(this,axis) \
astINVOKE(V,astGetTextLabGap_(astCheckPlot(this),axis))
#define astSetTextLabGap(this,axis,textlabgap) \
astINVOKE(V,astSetTextLabGap_(astCheckPlot(this),axis,textlabgap))
#define astTestTextLabGap(this,axis) \
astINVOKE(V,astTestTextLabGap_(astCheckPlot(this),axis))

#define astClearTitleGap(this) \
astINVOKE(V,astClearTitleGap_(astCheckPlot(this)))
#define astGetTitleGap(this) \
astINVOKE(V,astGetTitleGap_(astCheckPlot(this)))
#define astSetTitleGap(this,titlegap) \
astINVOKE(V,astSetTitleGap_(astCheckPlot(this),titlegap))
#define astTestTitleGap(this) \
astINVOKE(V,astTestTitleGap_(astCheckPlot(this)))

#define astClearLabelling(this) \
astINVOKE(V,astClearLabelling_(astCheckPlot(this)))
#define astGetLabelling(this) \
astINVOKE(V,astGetLabelling_(astCheckPlot(this)))
#define astSetLabelling(this,labelling) \
astINVOKE(V,astSetLabelling_(astCheckPlot(this),labelling))
#define astTestLabelling(this) \
astINVOKE(V,astTestLabelling_(astCheckPlot(this)))

#define astClearEdge(this,axis) \
astINVOKE(V,astClearEdge_(astCheckPlot(this),axis))
#define astGetEdge(this,axis) \
astINVOKE(V,astGetEdge_(astCheckPlot(this),axis))
#define astSetEdge(this,axis,edge) \
astINVOKE(V,astSetEdge_(astCheckPlot(this),axis,edge))
#define astTestEdge(this,axis) \
astINVOKE(V,astTestEdge_(astCheckPlot(this),axis))

#define astClearMinTick(this,axis) \
astINVOKE(V,astClearMinTick_(astCheckPlot(this),axis))
#define astGetMinTick(this,axis) \
astINVOKE(V,astGetMinTick_(astCheckPlot(this),axis))
#define astSetMinTick(this,axis,mintick) \
astINVOKE(V,astSetMinTick_(astCheckPlot(this),axis,mintick))
#define astTestMinTick(this,axis) \
astINVOKE(V,astTestMinTick_(astCheckPlot(this),axis))

#define astClearNumLab(this,axis) \
astINVOKE(V,astClearNumLab_(astCheckPlot(this),axis))
#define astGetNumLab(this,axis) \
astINVOKE(V,astGetNumLab_(astCheckPlot(this),axis))
#define astSetNumLab(this,axis,numlab) \
astINVOKE(V,astSetNumLab_(astCheckPlot(this),axis,numlab))
#define astTestNumLab(this,axis) \
astINVOKE(V,astTestNumLab_(astCheckPlot(this),axis))

#define astClearLabelUp(this,axis) \
astINVOKE(V,astClearLabelUp_(astCheckPlot(this),axis))
#define astGetLabelUp(this,axis) \
astINVOKE(V,astGetLabelUp_(astCheckPlot(this),axis))
#define astSetLabelUp(this,axis,labelup) \
astINVOKE(V,astSetLabelUp_(astCheckPlot(this),axis,labelup))
#define astTestLabelUp(this,axis) \
astINVOKE(V,astTestLabelUp_(astCheckPlot(this),axis))

#define astClearLogPlot(this,axis) \
astINVOKE(V,astClearLogPlot_(astCheckPlot(this),axis))
#define astGetLogPlot(this,axis) \
astINVOKE(V,astGetLogPlot_(astCheckPlot(this),axis))
#define astSetLogPlot(this,axis,logplot) \
astINVOKE(V,astSetLogPlot_(astCheckPlot(this),axis,logplot))
#define astTestLogPlot(this,axis) \
astINVOKE(V,astTestLogPlot_(astCheckPlot(this),axis))

#define astClearLogTicks(this,axis) \
astINVOKE(V,astClearLogTicks_(astCheckPlot(this),axis))
#define astGetLogTicks(this,axis) \
astINVOKE(V,astGetLogTicks_(astCheckPlot(this),axis))
#define astSetLogTicks(this,axis,logticks) \
astINVOKE(V,astSetLogTicks_(astCheckPlot(this),axis,logticks))
#define astTestLogTicks(this,axis) \
astINVOKE(V,astTestLogTicks_(astCheckPlot(this),axis))

#define astClearLogLabel(this,axis) \
astINVOKE(V,astClearLogLabel_(astCheckPlot(this),axis))
#define astGetLogLabel(this,axis) \
astINVOKE(V,astGetLogLabel_(astCheckPlot(this),axis))
#define astSetLogLabel(this,axis,loglabel) \
astINVOKE(V,astSetLogLabel_(astCheckPlot(this),axis,loglabel))
#define astTestLogLabel(this,axis) \
astINVOKE(V,astTestLogLabel_(astCheckPlot(this),axis))

#define astClearTextLab(this,axis) \
astINVOKE(V,astClearTextLab_(astCheckPlot(this),axis))
#define astGetTextLab(this,axis) \
astINVOKE(V,astGetTextLab_(astCheckPlot(this),axis))
#define astSetTextLab(this,axis,textlab) \
astINVOKE(V,astSetTextLab_(astCheckPlot(this),axis,textlab))
#define astTestTextLab(this,axis) \
astINVOKE(V,astTestTextLab_(astCheckPlot(this),axis))

#define astClearLabelUnits(this,axis) \
astINVOKE(V,astClearLabelUnits_(astCheckPlot(this),axis))
#define astGetLabelUnits(this,axis) \
astINVOKE(V,astGetLabelUnits_(astCheckPlot(this),axis))
#define astSetLabelUnits(this,axis,labelunits) \
astINVOKE(V,astSetLabelUnits_(astCheckPlot(this),axis,labelunits))
#define astTestLabelUnits(this,axis) \
astINVOKE(V,astTestLabelUnits_(astCheckPlot(this),axis))

#define astClearStyle(this,axis) \
astINVOKE(V,astClearStyle_(astCheckPlot(this),axis))
#define astGetStyle(this,axis) \
astINVOKE(V,astGetStyle_(astCheckPlot(this),axis))
#define astSetStyle(this,axis,style) \
astINVOKE(V,astSetStyle_(astCheckPlot(this),axis,style))
#define astTestStyle(this,axis) \
astINVOKE(V,astTestStyle_(astCheckPlot(this),axis))

#define astClearFont(this,axis) \
astINVOKE(V,astClearFont_(astCheckPlot(this),axis))
#define astGetFont(this,axis) \
astINVOKE(V,astGetFont_(astCheckPlot(this),axis))
#define astSetFont(this,axis,font) \
astINVOKE(V,astSetFont_(astCheckPlot(this),axis,font))
#define astTestFont(this,axis) \
astINVOKE(V,astTestFont_(astCheckPlot(this),axis))

#define astClearColour(this,axis) \
astINVOKE(V,astClearColour_(astCheckPlot(this),axis))
#define astGetColour(this,axis) \
astINVOKE(V,astGetColour_(astCheckPlot(this),axis))
#define astSetColour(this,axis,colour) \
astINVOKE(V,astSetColour_(astCheckPlot(this),axis,colour))
#define astTestColour(this,axis) \
astINVOKE(V,astTestColour_(astCheckPlot(this),axis))

#define astClearWidth(this,axis) \
astINVOKE(V,astClearWidth_(astCheckPlot(this),axis))
#define astGetWidth(this,axis) \
astINVOKE(V,astGetWidth_(astCheckPlot(this),axis))
#define astSetWidth(this,axis,width) \
astINVOKE(V,astSetWidth_(astCheckPlot(this),axis,width))
#define astTestWidth(this,axis) \
astINVOKE(V,astTestWidth_(astCheckPlot(this),axis))

#define astClearSize(this,axis) \
astINVOKE(V,astClearSize_(astCheckPlot(this),axis))
#define astGetSize(this,axis) \
astINVOKE(V,astGetSize_(astCheckPlot(this),axis))
#define astSetSize(this,axis,size) \
astINVOKE(V,astSetSize_(astCheckPlot(this),axis,size))
#define astTestSize(this,axis) \
astINVOKE(V,astTestSize_(astCheckPlot(this),axis))
#endif
#endif
