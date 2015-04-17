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
#include "keymap.h"
#include "region.h"

#if defined(astCLASS)       /* Protected */
#include "grf.h"
#endif

/* C header files. */
/* --------------- */
#include <stddef.h>

/* Macros. */
/* ======= */

#if defined(astCLASS) || defined(astFORTRAN77)
#define STATUS_PTR status
#else
#define STATUS_PTR astGetStatusPtr
#endif
#define AST__NPID      20   /* No. of different genuine plot object id's */

#define AST__GATTR	0   /* Identifiers for GRF functions */
#define AST__GFLUSH	1   /* Note, if any items are added or changed here, */
#define AST__GLINE	2   /* make sure that the astGrfFunID function is */
#define AST__GMARK	3   /* updated in plot.c */
#define AST__GTEXT	4
#define AST__GTXEXT	5
#define AST__GSCALES	6
#define AST__GQCH	7
#define AST__GCAP	8
#define AST__GBBUF	9
#define AST__GEBUF	10

#define AST__NGRFFUN    11   /* No. of Grf functions used by Plot */

#if defined(astCLASS)       /* Protected */
#define AST__MXBRK     100  /* Max. no. of breaks in a drawn curve */

/* Identifiers for the graphical elements of an annotated coord grid.
   "Pseudo-elements" (i.e. values used to indicate a group of other
   genuine elements) should come at the end of the list. The number of
   genuine elements should be stored in AST__NPID. */
#define AST__BORDER_ID       0 /* Id for astBorder curves */
#define AST__CURVE_ID        1 /* Id for astCurve, astGenCurve or astPolyCurve curves */
#define AST__TITLE_ID        2 /* Id for textual title */
#define AST__MARKS_ID        3 /* Id for marks drawn by astMark */
#define AST__TEXT_ID         4 /* Id for text strings drawn by astText */
#define AST__AXIS1_ID        5 /* Id for axis 1 through interior tick marks */
#define AST__AXIS2_ID        6 /* Id for axis 2 through interior tick marks */
#define AST__AXIS3_ID        7 /* Id for axis 2 through interior tick marks */
#define AST__NUMLAB1_ID      8 /* Id for numerical labels */
#define AST__NUMLAB2_ID      9 /* Id for numerical labels */
#define AST__NUMLAB3_ID     10 /* Id for numerical labels */
#define AST__TEXTLAB1_ID    11 /* Id for textual axis labels */
#define AST__TEXTLAB2_ID    12 /* Id for textual axis labels */
#define AST__TEXTLAB3_ID    13 /* Id for textual axis labels */
#define AST__TICKS1_ID      14 /* Id for major and minor tick marks */
#define AST__TICKS2_ID      15 /* Id for major and minor tick marks */
#define AST__TICKS3_ID      16 /* Id for major and minor tick marks */
#define AST__GRIDLINE1_ID   17 /* Id for axis 1 astGridLine AST__curves */
#define AST__GRIDLINE2_ID   18 /* Id for axis 2 astGridLine AST__curves */
#define AST__GRIDLINE3_ID   19 /* Id for axis 2 astGridLine AST__curves */
#define AST__AXES_ID        20 /* Id for axes through interior tick marks */
#define AST__NUMLABS_ID     21 /* Id for numerical labels */
#define AST__TEXTLABS_ID    22 /* Id for textual axis labels */
#define AST__GRIDLINE_ID    23 /* Id for astGridLine AST__curves */
#define AST__TICKS_ID       24 /* Id for major and minor tick marks */

/* Define constants used to size global arrays in this module. */
#define AST__PLOT_CRV_MXBRK 1000 /* Max. no. of breaks allowed in a plotted curve */
#define AST__PLOT_STRIPESCAPES_BUFF_LEN 50 /* Length of string returned by astStripEscapes */

#endif

/* Define a dummy __attribute__ macro for use on non-GNU compilers. */
#ifndef __GNUC__
#  define  __attribute__(x)  /*NOTHING*/
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
typedef int (* AstGAttrFun)( AstKeyMap *, int, double, double *, int );
typedef int (* AstGFlushFun)( AstKeyMap * );
typedef int (* AstGBBufFun)( AstKeyMap * );
typedef int (* AstGEBufFun)( AstKeyMap * );
typedef int (* AstGLineFun)( AstKeyMap *, int, const float *, const float * );
typedef int (* AstGMarkFun)( AstKeyMap *, int, const float *, const float *, int );
typedef int (* AstGTextFun)( AstKeyMap *, const char *, float, float, const char *, float, float );
typedef int (* AstGCapFun)( AstKeyMap *, int, int );
typedef int (* AstGTxExtFun)( AstKeyMap *, const char *, float, float, const char *, float, float, float *, float * );
typedef int (* AstGScalesFun)( AstKeyMap *, float *, float * );
typedef int (* AstGQchFun)( AstKeyMap *, float *, float * );

/* A general interface into which Grf Wrapper functions should be cast
   before being passed as an argument to astGrfWrapper. */
typedef void (* AstGrfWrap)( void );

/* Interfaces for the wrapper functions which wrap specific Grf funstions. */
typedef int (* AstGAttrWrap)( struct AstPlot *, int, double, double *, int, int * );
typedef int (* AstGBBufWrap)( struct AstPlot *, int * );
typedef int (* AstGEBufWrap)( struct AstPlot *, int * );
typedef int (* AstGFlushWrap)( struct AstPlot *, int * );
typedef int (* AstGLineWrap)( struct AstPlot *, int, const float *, const float *, int * );
typedef int (* AstGMarkWrap)( struct AstPlot *, int, const float *, const float *, int, int * );
typedef int (* AstGTextWrap)( struct AstPlot *, const char *, float, float, const char *, float, float, int * );
typedef int (* AstGCapWrap)( struct AstPlot *, int, int, int * );
typedef int (* AstGTxExtWrap)( struct AstPlot *, const char *, float, float, const char *, float, float, float *, float *, int * );
typedef int (* AstGScalesWrap)( struct AstPlot *, float *, float *, int * );
typedef int (* AstGQchWrap)( struct AstPlot *, float *, float *, int * );

/* A structure in which a collection of Grf function pointers can be
   stored. */
typedef struct AstGrfPtrs {
   AstGrfFun grffun[AST__NGRFFUN];
   AstGAttrWrap GAttr;
   AstGBBufWrap GBBuf;
   AstGEBufWrap GEBuf;
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
   double centre[ 3 ];
   double gap[ 3 ];
   double loggap[ 3 ];
   double labelat[ 3 ];
   double majticklen[ 3 ];
   double minticklen[ 3 ];
   double numlabgap[ 3 ];
   double size[ AST__NPID ];
   double textlabgap[ 3 ];
   double titlegap;
   double tol;
   double ucentre[ 3 ];
   double ugap[ 3 ];
   double uloggap[ 3 ];
   double ulblat[ 3 ];
   double umjtkln[ 3 ];
   double width[ AST__NPID ];
   double *majtickgx[ 3 ];
   double *majtickgy[ 3 ];
   double *mintickgx[ 3 ];
   double *mintickgy[ 3 ];
   int majtickcount[ 3 ];
   int mintickcount[ 3 ];
   int nmajtickval[ 3 ];
   double *majtickval[ 3 ];
   int nmintickval[ 3 ];
   double *mintickval[ 3 ];
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
   int drawaxes[ 3 ];
   int abbrev[ 3 ];
   int escape;
   int drawtitle;
   int edge[ 3 ];
   int font[ AST__NPID ];
   int grf;
   int grid;
   int invisible;
   int labelling;
   int labelunits[ 3 ];
   int labelup[ 3 ];
   int mintick[ 3 ];
   int numlab[ 3 ];
   int style[ AST__NPID ];
   int textlab[ 3 ];
   int tickall;
   int forceexterior;
   int uborder;
   int uedge[ 3 ];
   int ugrid;
   int ulbling;
   int ulbunit[ 3 ];
   int ulgtk[ 3 ];
   int ulglb[ 3 ];
   int umintk[ 3 ];
   int utxtlb[ 3 ];
   int xrev;
   int yrev;
   int ink;
   int logplot[ 3 ];
   int logticks[ 3 ];
   int loglabel[ 3 ];
   AstGrfFun grffun[AST__NGRFFUN];
   AstGAttrWrap GAttr;
   AstGBBufWrap GBBuf;
   AstGEBufWrap GEBuf;
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
   AstKeyMap *grfcontext;
   AstKeyMap *grfcontextID;
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

/* A Unique identifier to determine class membership. */
   AstClassIdentifier id;

/* Properties (e.g. methods) specific to this class. */
   AstKeyMap *(* GetGrfContext)( AstPlot *, int * );
   AstPointSet *(* GetDrawnTicks)( AstPlot *, int, int, int * );
   int (* Border)( AstPlot *, int * );
   int (* CvBrk)( AstPlot *, int, double *, double *, double *, int * );
   void (* BBuf)( AstPlot *, int * );
   void (* BoundingBox)( AstPlot *, float[2], float[2], int * );
   void (* Clip)( AstPlot *, int, const double [], const double [], int * );
   void (* CopyPlotDefaults)( AstPlot *, int, AstPlot *, int, int * );
   void (* Curve)( AstPlot *, const double [], const double [], int * );
   void (* DrawExtraTicks)( AstPlot *, int, AstPointSet *, int * );
   void (* EBuf)( AstPlot *, int * );
   void (* GenCurve)( AstPlot *, AstMapping *, int * );
   void (* GrfPop)( AstPlot *, int * );
   void (* GrfPush)( AstPlot *, int * );
   void (* GrfSet)( AstPlot *, const char *, AstGrfFun, int * );
   void (* GrfWrapper)( AstPlot *, const char *, AstGrfWrap, int * );
   void (* Grid)( AstPlot *, int * );
   void (* GridLine)( AstPlot *, int, const double [], double, int * );
   void (* Mark)( AstPlot *, int, int, int, const double *, int, int * );
   void (* Mirror)( AstPlot *, int, int * );
   void (* PolyCurve)( AstPlot *, int, int, int, const double *, int * );
   void (* RegionOutline)( AstPlot *, AstRegion *, int * );
   void (* SetTickValues)( AstPlot *, int, int, double *, int, double *, int * );
   void (* Text)( AstPlot *, const char *, const double [], const float [], const char *, int * );

   double (* GetTol)( AstPlot *, int * );
   int (* TestTol)( AstPlot *, int * );
   void (* SetTol)( AstPlot *, double, int * );
   void (* ClearTol)( AstPlot *, int * );

   int (* GetGrid)( AstPlot *, int * );
   int (* TestGrid)( AstPlot *, int * );
   void (* SetGrid)( AstPlot *, int, int * );
   void (* ClearGrid)( AstPlot *, int * );

   int (* GetTickAll)( AstPlot *, int * );
   int (* TestTickAll)( AstPlot *, int * );
   void (* SetTickAll)( AstPlot *, int, int * );
   void (* ClearTickAll)( AstPlot *, int * );

   int (* GetForceExterior)( AstPlot *, int * );
   int (* TestForceExterior)( AstPlot *, int * );
   void (* SetForceExterior)( AstPlot *, int, int * );
   void (* ClearForceExterior)( AstPlot *, int * );

   int (* GetInvisible)( AstPlot *, int * );
   int (* TestInvisible)( AstPlot *, int * );
   void (* SetInvisible)( AstPlot *, int, int * );
   void (* ClearInvisible)( AstPlot *, int * );

   int (* GetBorder)( AstPlot *, int * );
   int (* TestBorder)( AstPlot *, int * );
   void (* SetBorder)( AstPlot *, int, int * );
   void (* ClearBorder)( AstPlot *, int * );

   int (* GetClipOp)( AstPlot *, int * );
   int (* TestClipOp)( AstPlot *, int * );
   void (* SetClipOp)( AstPlot *, int, int * );
   void (* ClearClipOp)( AstPlot *, int * );

   int (* GetClip)( AstPlot *, int * );
   int (* TestClip)( AstPlot *, int * );
   void (* SetClip)( AstPlot *, int, int * );
   void (* ClearClip)( AstPlot *, int * );

   int (* GetGrf)( AstPlot *, int * );
   int (* TestGrf)( AstPlot *, int * );
   void (* SetGrf)( AstPlot *, int, int * );
   void (* ClearGrf)( AstPlot *, int * );

   int (* GetDrawTitle)( AstPlot *, int * );
   int (* TestDrawTitle)( AstPlot *, int * );
   void (* SetDrawTitle)( AstPlot *, int, int * );
   void (* ClearDrawTitle)( AstPlot *, int * );

   int (* GetLabelUp)( AstPlot *, int, int * );
   int (* TestLabelUp)( AstPlot *, int, int * );
   void (* SetLabelUp)( AstPlot *, int, int, int * );
   void (* ClearLabelUp)( AstPlot *, int, int * );

   int (* GetLogPlot)( AstPlot *, int, int * );
   int (* TestLogPlot)( AstPlot *, int, int * );
   void (* SetLogPlot)( AstPlot *, int, int, int * );
   void (* ClearLogPlot)( AstPlot *, int, int * );

   int (* GetLogTicks)( AstPlot *, int, int * );
   int (* TestLogTicks)( AstPlot *, int, int * );
   void (* SetLogTicks)( AstPlot *, int, int, int * );
   void (* ClearLogTicks)( AstPlot *, int, int * );

   int (* GetLogLabel)( AstPlot *, int, int * );
   int (* TestLogLabel)( AstPlot *, int, int * );
   void (* SetLogLabel)( AstPlot *, int, int, int * );
   void (* ClearLogLabel)( AstPlot *, int, int * );

   int (* GetDrawAxes)( AstPlot *, int, int * );
   int (* TestDrawAxes)( AstPlot *, int, int * );
   void (* SetDrawAxes)( AstPlot *, int, int, int * );
   void (* ClearDrawAxes)( AstPlot *, int, int * );

   int (* GetAbbrev)( AstPlot *, int, int * );
   int (* TestAbbrev)( AstPlot *, int, int * );
   void (* SetAbbrev)( AstPlot *, int, int, int * );
   void (* ClearAbbrev)( AstPlot *, int, int * );

   int (* GetEscape)( AstPlot *, int * );
   int (* TestEscape)( AstPlot *, int * );
   void (* SetEscape)( AstPlot *, int, int * );
   void (* ClearEscape)( AstPlot *, int * );

   int (* GetLabelling)( AstPlot *, int * );
   int (* TestLabelling)( AstPlot *, int * );
   void (* SetLabelling)( AstPlot *, int, int * );
   void (* ClearLabelling)( AstPlot *, int * );

   double (* GetMajTickLen)( AstPlot *, int, int * );
   int (* TestMajTickLen)( AstPlot *, int, int * );
   void (* SetMajTickLen)( AstPlot *, int, double, int * );
   void (* ClearMajTickLen)( AstPlot *, int, int * );

   double (* GetMinTickLen)( AstPlot *, int, int * );
   int (* TestMinTickLen)( AstPlot *, int, int * );
   void (* SetMinTickLen)( AstPlot *, int, double, int * );
   void (* ClearMinTickLen)( AstPlot *, int, int * );

   double (* GetNumLabGap)( AstPlot *, int, int * );
   int (* TestNumLabGap)( AstPlot *, int, int * );
   void (* SetNumLabGap)( AstPlot *, int, double, int * );
   void (* ClearNumLabGap)( AstPlot *, int, int * );

   double (* GetTextLabGap)( AstPlot *, int, int * );
   int (* TestTextLabGap)( AstPlot *, int, int * );
   void (* SetTextLabGap)( AstPlot *, int, double, int * );
   void (* ClearTextLabGap)( AstPlot *, int, int * );

   double (* GetTitleGap)( AstPlot *, int * );
   int (* TestTitleGap)( AstPlot *, int * );
   void (* SetTitleGap)( AstPlot *, double, int * );
   void (* ClearTitleGap)( AstPlot *, int * );

   double (* GetLabelAt)( AstPlot *, int, int * );
   int (* TestLabelAt)( AstPlot *, int, int * );
   void (* SetLabelAt)( AstPlot *, int, double, int * );
   void (* ClearLabelAt)( AstPlot *, int, int * );

   double (* GetGap)( AstPlot *, int, int * );
   int (* TestGap)( AstPlot *, int, int * );
   void (* SetGap)( AstPlot *, int, double, int * );
   void (* ClearGap)( AstPlot *, int, int * );

   double (* GetLogGap)( AstPlot *, int, int * );
   int (* TestLogGap)( AstPlot *, int, int * );
   void (* SetLogGap)( AstPlot *, int, double, int * );
   void (* ClearLogGap)( AstPlot *, int, int * );

   double (* GetCentre)( AstPlot *, int, int * );
   int (* TestCentre)( AstPlot *, int, int * );
   void (* SetCentre)( AstPlot *, int, double, int * );
   void (* ClearCentre)( AstPlot *, int, int * );

   int (* GetEdge)( AstPlot *, int, int * );
   int (* TestEdge)( AstPlot *, int, int * );
   void (* SetEdge)( AstPlot *, int, int, int * );
   void (* ClearEdge)( AstPlot *, int, int * );

   int (* GetNumLab)( AstPlot *, int, int * );
   int (* TestNumLab)( AstPlot *, int, int * );
   void (* SetNumLab)( AstPlot *, int, int, int * );
   void (* ClearNumLab)( AstPlot *, int, int * );

   int (* GetMinTick)( AstPlot *, int, int * );
   int (* TestMinTick)( AstPlot *, int, int * );
   void (* SetMinTick)( AstPlot *, int, int, int * );
   void (* ClearMinTick)( AstPlot *, int, int * );

   int (* GetTextLab)( AstPlot *, int, int * );
   int (* TestTextLab)( AstPlot *, int, int * );
   void (* SetTextLab)( AstPlot *, int, int, int * );
   void (* ClearTextLab)( AstPlot *, int, int * );

   int (* GetLabelUnits)( AstPlot *, int, int * );
   int (* TestLabelUnits)( AstPlot *, int, int * );
   void (* SetLabelUnits)( AstPlot *, int, int, int * );
   void (* ClearLabelUnits)( AstPlot *, int, int * );

   int (* GetStyle)( AstPlot *, int, int * );
   int (* TestStyle)( AstPlot *, int, int * );
   void (* SetStyle)( AstPlot *, int, int, int * );
   void (* ClearStyle)( AstPlot *, int, int * );

   int (* GetFont)( AstPlot *, int, int * );
   int (* TestFont)( AstPlot *, int, int * );
   void (* SetFont)( AstPlot *, int, int, int * );
   void (* ClearFont)( AstPlot *, int, int * );

   int (* GetColour)( AstPlot *, int, int * );
   int (* TestColour)( AstPlot *, int, int * );
   void (* SetColour)( AstPlot *, int, int, int * );
   void (* ClearColour)( AstPlot *, int, int * );

   double (* GetWidth)( AstPlot *, int, int * );
   int (* TestWidth)( AstPlot *, int, int * );
   void (* SetWidth)( AstPlot *, int, double, int * );
   void (* ClearWidth)( AstPlot *, int, int * );

   double (* GetSize)( AstPlot *, int, int * );
   int (* TestSize)( AstPlot *, int, int * );
   void (* SetSize)( AstPlot *, int, double, int * );
   void (* ClearSize)( AstPlot *, int, int * );

   int (* GetInk)( AstPlot *, int * );
   int (* TestInk)( AstPlot *, int * );
   void (* SetInk)( AstPlot *, int, int * );
   void (* ClearInk)( AstPlot *, int * );

} AstPlotVtab;

/* Structure holding information about curves drawn by astGridLine and
   astCurve. */
typedef struct AstPlotCurveData{
   int out;          /* Was the curve completely outside the clipping area? */
   int nbrk;         /* The number of breaks in the curve. */
   float xbrk[ AST__PLOT_CRV_MXBRK ];  /* Graphics X coordinate at each break. */
   float ybrk[ AST__PLOT_CRV_MXBRK ];  /* Graphics Y coordinate at each break. */
   float vxbrk[ AST__PLOT_CRV_MXBRK ]; /* X comp. of unit tangent vector */
   float vybrk[ AST__PLOT_CRV_MXBRK ]; /* Y comp. of unit tangent vector */
   float length;     /* Drawn length of the curve in graphics coordinates */
} AstPlotCurveData;

#if defined(THREAD_SAFE)

/* Define a structure holding all data items that are global within this
   class. */
typedef struct AstPlotGlobals {
   AstPlotVtab Class_Vtab;
   int Class_Init;
   double GrfAttrs_attrs_t[ GRF__NATTR ];
   int GrfAttrs_nesting_t;
   double Crv_limit_t;
   double Crv_scerr_t;
   double Crv_tol_t;
   double Crv_ux0_t;
   double Crv_uy0_t;
   double Crv_vxl_t;
   double Crv_vyl_t;
   double Crv_xhi_t;
   double Crv_xl_t;
   double Crv_xlo_t;
   double Crv_yhi_t;
   double Crv_yl_t;
   double Crv_ylo_t;
   float *Crv_vxbrk_t;
   float *Crv_vybrk_t;
   float *Crv_xbrk_t;
   float *Crv_ybrk_t;
   float Crv_len_t;
   int Crv_ink_t;
   int Crv_nbrk_t;
   int Crv_nent_t;
   int Crv_out_t;
   int Crv_clip_t;
   void (*Crv_map_t)( int, double *, double *, double *, const char *, const char *, int *, struct AstGlobals * );
   void *Crv_mapstatics_t;
   float Box_lbnd_t[ 2 ];
   float Box_ubnd_t[ 2 ];
   float Boxp_lbnd_t[ 2 ];
   float Boxp_ubnd_t[ 2 ];
   int Boxp_freeze_t;
   float 	*Poly_x_t;
   float 	*Poly_y_t;
   int   	 Poly_n_t;
   float       **Poly_xp_t;
   float       **Poly_yp_t;
   int          *Poly_np_t;
   int   	 Poly_npoly_t;
   int           Map1_ncoord_t;
   AstPlot      *Map1_plot_t;
   AstMapping   *Map1_map_t;
   AstFrame     *Map1_frame_t;
   const double *Map1_origin_t;
   double        Map1_length_t;
   int           Map1_axis_t;
   void         *Map1_statics_t;
   int           Map1_norm_t;
   int           Map1_log_t;
   int           Map2_ncoord_t;
   AstPlot      *Map2_plot_t;
   AstMapping   *Map2_map_t;
   double        Map2_x0_t;
   double        Map2_y0_t;
   double        Map2_deltax_t;
   double        Map2_deltay_t;
   void         *Map2_statics_t;
   int           Map3_ncoord_t;
   AstPlot      *Map3_plot_t;
   AstMapping   *Map3_map_t;
   AstFrame     *Map3_frame_t;
   const double *Map3_origin_t;
   const double *Map3_end_t;
   double        Map3_scale_t;
   void         *Map3_statics_t;
   int           Map4_ncoord_t;
   AstPlot      *Map4_plot_t;
   AstMapping   *Map4_map_t;
   AstMapping   *Map4_umap_t;
   void         *Map4_statics_t;
   int           Map5_ncoord_t;
   AstPlot      *Map5_plot_t;
   AstMapping   *Map5_map_t;
   AstRegion    *Map5_region_t;
   void         *Map5_statics_t;
   AstPlotCurveData Curve_data_t;
   char 	GetAttrib_Buff[ 200 ];
   char 	SplitValue_Buff[ 200 ];
   char 	StripEscapes_Buff[ AST__PLOT_STRIPESCAPES_BUFF_LEN + 1 ];
   double 	Grf_chv_t;
   double 	Grf_chh_t;
   float 	Grf_alpha_t;
   float 	Grf_beta_t;
} AstPlotGlobals;

#endif
#endif

/* Function prototypes. */
/* ==================== */
/* Prototypes for standard class functions. */
/* ---------------------------------------- */
astPROTO_CHECK(Plot)          /* Check class membership */
astPROTO_ISA(Plot)            /* Test class membership */

/* Constructor. */
#if defined(astCLASS)            /* Protected. */
AstPlot *astPlot_( void *, const float *, const double *, const char *, int *, ...);
#else
AstPlot *astPlotId_( void *, const float [], const double [], const char *, ... )__attribute__((format(printf,4,5)));
#endif

#if defined(astCLASS)            /* Protected */

/* Initialiser. */
AstPlot *astInitPlot_( void *, size_t, int, AstPlotVtab *,
                       const char *, AstFrame *, const float *, const double *, int * );

/* Vtab initialiser. */
void astInitPlotVtab_( AstPlotVtab *, const char *, int * );

/* Loader. */
AstPlot *astLoadPlot_( void *, size_t, AstPlotVtab *,
                       const char *, AstChannel *channel, int * );

/* Thread-safe initialiser for all global data used by this module. */
#if defined(THREAD_SAFE)
void astInitPlotGlobals_( AstPlotGlobals * );
#endif

#endif

/* Prototypes for member functions. */
/* -------------------------------- */
   AstKeyMap *astGetGrfContext_( AstPlot *, int * );
   AstKeyMap *astGrfConID_( AstPlot *, int * );
   const char *astStripEscapes_( const char *, int * );
   int astBorder_( AstPlot *, int * );
   int astFindEscape_( const char *, int *, int *, int *, int * );
   void astBBuf_( AstPlot *, int * );
   void astBoundingBox_( AstPlot *, float[2], float[2], int * );
   void astClip_( AstPlot *, int, const double [], const double [], int * );
   void astCurve_( AstPlot *, const double [], const double [], int * );
   void astEBuf_( AstPlot *, int * );
   void astGenCurve_( AstPlot *, AstMapping *, int * );
   void astGrfPop_( AstPlot *, int * );
   void astGrfPush_( AstPlot *, int * );
   void astGrfSet_( AstPlot *, const char *, AstGrfFun, int * );
   void astGridLine_( AstPlot *, int, const double [], double, int * );
   void astGrid_( AstPlot *, int * );
   void astMark_( AstPlot *, int, int, int, const double *, int, int * );
   void astPolyCurve_( AstPlot *, int, int, int, const double *, int * );
   void astRegionOutline_( AstPlot *, AstRegion *, int * );
   void astText_( AstPlot *, const char *, const double [], const float [], const char *, int * );

   void astGrfWrapper_( AstPlot *, const char *, AstGrfWrap, int * );
   int astGrfFunID_( const char *, const char *, const char *, int * );

#if defined(astCLASS)            /* Protected */
   int astCvBrk_( AstPlot *, int, double *, double *, double *, int * );
   void astCopyPlotDefaults_( AstPlot *, int, AstPlot *, int, int * );
   void astMirror_( AstPlot *, int, int * );
   AstPointSet *astGetDrawnTicks_( AstPlot *, int, int, int * );
   void astSetTickValues_( AstPlot *, int, int, double *, int, double *, int * );
   void astDrawExtraTicks_( AstPlot *, int, AstPointSet *, int * );
   void astGrfAttrs_( AstPlot *, int, int, int, const char *, const char *, int * );

   double astGetTol_( AstPlot *, int * );
   int astTestTol_( AstPlot *, int * );
   void astSetTol_( AstPlot *, double, int * );
   void astClearTol_( AstPlot *, int * );

   int astGetGrid_( AstPlot *, int * );
   int astTestGrid_( AstPlot *, int * );
   void astSetGrid_( AstPlot *, int, int * );
   void astClearGrid_( AstPlot *, int * );

   int astGetTickAll_( AstPlot *, int * );
   int astTestTickAll_( AstPlot *, int * );
   void astSetTickAll_( AstPlot *, int, int * );
   void astClearTickAll_( AstPlot *, int * );

   int astGetForceExterior_( AstPlot *, int * );
   int astTestForceExterior_( AstPlot *, int * );
   void astSetForceExterior_( AstPlot *, int, int * );
   void astClearForceExterior_( AstPlot *, int * );

   int astGetInvisible_( AstPlot *, int * );
   int astTestInvisible_( AstPlot *, int * );
   void astSetInvisible_( AstPlot *, int, int * );
   void astClearInvisible_( AstPlot *, int * );

   int astGetBorder_( AstPlot *, int * );
   int astTestBorder_( AstPlot *, int * );
   void astSetBorder_( AstPlot *, int, int * );
   void astClearBorder_( AstPlot *, int * );

   int astGetClip_( AstPlot *, int * );
   int astTestClip_( AstPlot *, int * );
   void astSetClip_( AstPlot *, int, int * );
   void astClearClip_( AstPlot *, int * );

   int astGetClipOp_( AstPlot *, int * );
   int astTestClipOp_( AstPlot *, int * );
   void astSetClipOp_( AstPlot *, int, int * );
   void astClearClipOp_( AstPlot *, int * );

   int astGetGrf_( AstPlot *, int * );
   int astTestGrf_( AstPlot *, int * );
   void astSetGrf_( AstPlot *, int, int * );
   void astClearGrf_( AstPlot *, int * );

   int astGetDrawTitle_( AstPlot *, int * );
   int astTestDrawTitle_( AstPlot *, int * );
   void astSetDrawTitle_( AstPlot *, int, int * );
   void astClearDrawTitle_( AstPlot *, int * );

   int astGetLabelUp_( AstPlot *, int, int * );
   int astTestLabelUp_( AstPlot *, int, int * );
   void astSetLabelUp_( AstPlot *, int, int, int * );
   void astClearLabelUp_( AstPlot *, int, int * );

   int astGetLogPlot_( AstPlot *, int, int * );
   int astTestLogPlot_( AstPlot *, int, int * );
   void astSetLogPlot_( AstPlot *, int, int, int * );
   void astClearLogPlot_( AstPlot *, int, int * );

   int astGetLogTicks_( AstPlot *, int, int * );
   int astTestLogTicks_( AstPlot *, int, int * );
   void astSetLogTicks_( AstPlot *, int, int, int * );
   void astClearLogTicks_( AstPlot *, int, int * );

   int astGetLogLabel_( AstPlot *, int, int * );
   int astTestLogLabel_( AstPlot *, int, int * );
   void astSetLogLabel_( AstPlot *, int, int, int * );
   void astClearLogLabel_( AstPlot *, int, int * );

   int astGetDrawAxes_( AstPlot *, int, int * );
   int astTestDrawAxes_( AstPlot *, int, int * );
   void astSetDrawAxes_( AstPlot *, int, int, int * );
   void astClearDrawAxes_( AstPlot *, int, int * );

   int astGetAbbrev_( AstPlot *, int, int * );
   int astTestAbbrev_( AstPlot *, int, int * );
   void astSetAbbrev_( AstPlot *, int, int, int * );
   void astClearAbbrev_( AstPlot *, int, int * );

   int astGetEscape_( AstPlot *, int * );
   int astTestEscape_( AstPlot *, int * );
   void astSetEscape_( AstPlot *, int, int * );
   void astClearEscape_( AstPlot *, int * );

   double astGetLabelAt_( AstPlot *, int, int * );
   int astTestLabelAt_( AstPlot *, int, int * );
   void astSetLabelAt_( AstPlot *, int, double, int * );
   void astClearLabelAt_( AstPlot *, int, int * );

   double astGetGap_( AstPlot *, int, int * );
   int astTestGap_( AstPlot *, int, int * );
   void astSetGap_( AstPlot *, int, double, int * );
   void astClearGap_( AstPlot *, int, int * );

   double astGetLogGap_( AstPlot *, int, int * );
   int astTestLogGap_( AstPlot *, int, int * );
   void astSetLogGap_( AstPlot *, int, double, int * );
   void astClearLogGap_( AstPlot *, int, int * );

   double astGetCentre_( AstPlot *, int, int * );
   int astTestCentre_( AstPlot *, int, int * );
   void astSetCentre_( AstPlot *, int, double, int * );
   void astClearCentre_( AstPlot *, int, int * );

   int astGetLabelling_( AstPlot *, int * );
   int astTestLabelling_( AstPlot *, int * );
   void astSetLabelling_( AstPlot *, int, int * );
   void astClearLabelling_( AstPlot *, int * );

   double astGetMajTickLen_( AstPlot *, int, int * );
   int astTestMajTickLen_( AstPlot *, int, int * );
   void astSetMajTickLen_( AstPlot *, int, double, int * );
   void astClearMajTickLen_( AstPlot *, int, int * );

   double astGetMinTickLen_( AstPlot *, int, int * );
   int astTestMinTickLen_( AstPlot *, int, int * );
   void astSetMinTickLen_( AstPlot *, int, double, int * );
   void astClearMinTickLen_( AstPlot *, int, int * );

   double astGetNumLabGap_( AstPlot *, int, int * );
   int astTestNumLabGap_( AstPlot *, int, int * );
   void astSetNumLabGap_( AstPlot *, int, double, int * );
   void astClearNumLabGap_( AstPlot *, int, int * );

   double astGetTextLabGap_( AstPlot *, int, int * );
   int astTestTextLabGap_( AstPlot *, int, int * );
   void astSetTextLabGap_( AstPlot *, int, double, int * );
   void astClearTextLabGap_( AstPlot *, int, int * );

   double astGetTitleGap_( AstPlot *, int * );
   int astTestTitleGap_( AstPlot *, int * );
   void astSetTitleGap_( AstPlot *, double, int * );
   void astClearTitleGap_( AstPlot *, int * );

   int astGetEdge_( AstPlot *, int, int * );
   int astTestEdge_( AstPlot *, int, int * );
   void astSetEdge_( AstPlot *, int, int, int * );
   void astClearEdge_( AstPlot *, int, int * );

   int astGetMinTick_( AstPlot *, int, int * );
   int astTestMinTick_( AstPlot *, int, int * );
   void astSetMinTick_( AstPlot *, int, int, int * );
   void astClearMinTick_( AstPlot *, int, int * );

   int astGetNumLab_( AstPlot *, int, int * );
   int astTestNumLab_( AstPlot *, int, int * );
   void astSetNumLab_( AstPlot *, int, int, int * );
   void astClearNumLab_( AstPlot *, int, int * );

   int astGetTextLab_( AstPlot *, int, int * );
   int astTestTextLab_( AstPlot *, int, int * );
   void astSetTextLab_( AstPlot *, int, int, int * );
   void astClearTextLab_( AstPlot *, int, int * );

   int astGetLabelUnits_( AstPlot *, int, int * );
   int astTestLabelUnits_( AstPlot *, int, int * );
   void astSetLabelUnits_( AstPlot *, int, int, int * );
   void astClearLabelUnits_( AstPlot *, int, int * );

   int astGetStyle_( AstPlot *, int, int * );
   int astTestStyle_( AstPlot *, int, int * );
   void astSetStyle_( AstPlot *, int, int, int * );
   void astClearStyle_( AstPlot *, int, int * );

   int astGetFont_( AstPlot *, int, int * );
   int astTestFont_( AstPlot *, int, int * );
   void astSetFont_( AstPlot *, int, int, int * );
   void astClearFont_( AstPlot *, int, int * );

   int astGetColour_( AstPlot *, int, int * );
   int astTestColour_( AstPlot *, int, int * );
   void astSetColour_( AstPlot *, int, int, int * );
   void astClearColour_( AstPlot *, int, int * );

   double astGetWidth_( AstPlot *, int, int * );
   int astTestWidth_( AstPlot *, int, int * );
   void astSetWidth_( AstPlot *, int, double, int * );
   void astClearWidth_( AstPlot *, int, int * );

   double astGetSize_( AstPlot *, int, int * );
   int astTestSize_( AstPlot *, int, int * );
   void astSetSize_( AstPlot *, int, double, int * );
   void astClearSize_( AstPlot *, int, int * );

   int astGetInk_( AstPlot *, int * );
   int astTestInk_( AstPlot *, int * );
   void astSetInk_( AstPlot *, int, int * );
   void astClearInk_( AstPlot *, int * );
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
#define astCheckPlot(this) astINVOKE_CHECK(Plot,this,0)
#define astVerifyPlot(this) astINVOKE_CHECK(Plot,this,1)

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
astINVOKE(O,astInitPlot_(mem,size,init,vtab,name,frame,graph,base,STATUS_PTR))

/* Vtab Initialiser. */
#define astInitPlotVtab(vtab,name) astINVOKE(V,astInitPlotVtab_(vtab,name,STATUS_PTR))
/* Loader. */
#define astLoadPlot(mem,size,vtab,name,channel) \
astINVOKE(O,astLoadPlot_(mem,size,vtab,name,astCheckChannel(channel),STATUS_PTR))
#endif

/* Interfaces to member functions. */
/* ------------------------------- */
/* Here we make use of astCheckPlot (et al.) to validate Plot
   pointers before use. This provides a contextual error report if a
   pointer to the wrong sort of object is supplied. */


#define astGetGrfContext(this) \
astINVOKE(O,astGetGrfContext_(astCheckPlot(this),STATUS_PTR))

#define astBorder(this) \
astINVOKE(V,astBorder_(astCheckPlot(this),STATUS_PTR))

#define astBoundingBox(this,lbnd,ubnd) \
astINVOKE(V,astBoundingBox_(astCheckPlot(this),lbnd,ubnd,STATUS_PTR))

#define astClip(this,iframe,lbnd,ubnd) \
astINVOKE(V,astClip_(astCheckPlot(this),iframe,lbnd,ubnd,STATUS_PTR))

#define astMark(this,nmark,ncoord,indim,in,type) \
astINVOKE(V,astMark_(astCheckPlot(this),nmark,ncoord,indim,in,type,STATUS_PTR))

#define astText(this,text,pos,up,just) \
astINVOKE(V,astText_(astCheckPlot(this),text,pos,up,just,STATUS_PTR))

#define astGrid(this) \
astINVOKE(V,astGrid_(astCheckPlot(this),STATUS_PTR))

#define astGridLine(this,axis,start,length) \
astINVOKE(V,astGridLine_(astCheckPlot(this),axis,start,length,STATUS_PTR))

#define astCurve(this,start,finish) \
astINVOKE(V,astCurve_(astCheckPlot(this),start,finish,STATUS_PTR))

#define astGenCurve(this,map) \
astINVOKE(V,astGenCurve_(astCheckPlot(this),astCheckMapping(map),STATUS_PTR))

#define astPolyCurve(this,npoint,ncoord,dim,in) \
astINVOKE(V,astPolyCurve_(astCheckPlot(this),npoint,ncoord,dim,in,STATUS_PTR))

#define astRegionOutline(this,region) \
astINVOKE(V,astRegionOutline_(astCheckPlot(this),astCheckRegion(region),STATUS_PTR))

#define astGrfSet(this,name,fun) \
astINVOKE(V,astGrfSet_(astCheckPlot(this),name,fun,STATUS_PTR))

#define astGrfPush(this) \
astINVOKE(V,astGrfPush_(astCheckPlot(this),STATUS_PTR))

#define astGrfPop(this) \
astINVOKE(V,astGrfPop_(astCheckPlot(this),STATUS_PTR))

#define astBBuf(this) \
astINVOKE(V,astBBuf_(astCheckPlot(this),STATUS_PTR))

#define astEBuf(this) \
astINVOKE(V,astEBuf_(astCheckPlot(this),STATUS_PTR))


#define astGrfFunID(name,method,class) astGrfFunID_(name,method,class,STATUS_PTR)
#define astFindEscape(text,type,value,nc) astFindEscape_(text,type,value,nc,STATUS_PTR)
#define astStripEscapes(text) astStripEscapes_(text,STATUS_PTR)
#define astGrfConID(this) astGrfConID_(this,STATUS_PTR)

#define astGrfWrapper(this,name,wrapper) \
astINVOKE(V,astGrfWrapper_(astCheckPlot(this),name,wrapper,STATUS_PTR))

#if defined(astCLASS)            /* Protected */

#define astGrfAttrs(this,id,set,prim,method,class) \
astGrfAttrs_(astCheckPlot(this),id,set,prim,method,class,STATUS_PTR)

#define astCvBrk(this,ibrk,brk,vbrk,len) \
astINVOKE(V,astCvBrk_(astCheckPlot(this),ibrk,brk,vbrk,len,STATUS_PTR))

#define astCopyPlotDefaults(this,axis,dplot,daxis) \
astINVOKE(V,astCopyPlotDefaults_(astCheckPlot(this),axis,astCheckPlot(dplot),daxis,STATUS_PTR))

#define astMirror(this,axis) \
astINVOKE(V,astMirror_(astCheckPlot(this),axis,STATUS_PTR))

#define astGetDrawnTicks(this,axis,major) \
astINVOKE(O,astGetDrawnTicks_(astCheckPlot(this),axis,major,STATUS_PTR))

#define astSetTickValues(this,axis,nmajor,major,nminor,minor) \
astINVOKE(V,astSetTickValues_(astCheckPlot(this),axis,nmajor,major,nminor,minor,STATUS_PTR))

#define astDrawExtraTicks(this,axis,pset) \
astINVOKE(V,astDrawExtraTicks_(astCheckPlot(this),axis,astCheckPointSet(pset),STATUS_PTR))

#define astClearTol(this) \
astINVOKE(V,astClearTol_(astCheckPlot(this),STATUS_PTR))
#define astGetTol(this) \
astINVOKE(V,astGetTol_(astCheckPlot(this),STATUS_PTR))
#define astSetTol(this,tol) \
astINVOKE(V,astSetTol_(astCheckPlot(this),tol,STATUS_PTR))
#define astTestTol(this) \
astINVOKE(V,astTestTol_(astCheckPlot(this),STATUS_PTR))

#define astClearGrid(this) \
astINVOKE(V,astClearGrid_(astCheckPlot(this),STATUS_PTR))
#define astGetGrid(this) \
astINVOKE(V,astGetGrid_(astCheckPlot(this),STATUS_PTR))
#define astSetGrid(this,grid) \
astINVOKE(V,astSetGrid_(astCheckPlot(this),grid,STATUS_PTR))
#define astTestGrid(this) \
astINVOKE(V,astTestGrid_(astCheckPlot(this),STATUS_PTR))

#define astClearInk(this) \
astINVOKE(V,astClearInk_(astCheckPlot(this),STATUS_PTR))
#define astGetInk(this) \
astINVOKE(V,astGetInk_(astCheckPlot(this),STATUS_PTR))
#define astSetInk(this,ink) \
astINVOKE(V,astSetInk_(astCheckPlot(this),ink,STATUS_PTR))
#define astTestInk(this) \
astINVOKE(V,astTestInk_(astCheckPlot(this),STATUS_PTR))

#define astClearTickAll(this) \
astINVOKE(V,astClearTickAll_(astCheckPlot(this),STATUS_PTR))
#define astGetTickAll(this) \
astINVOKE(V,astGetTickAll_(astCheckPlot(this),STATUS_PTR))
#define astSetTickAll(this,tickall) \
astINVOKE(V,astSetTickAll_(astCheckPlot(this),tickall,STATUS_PTR))
#define astTestTickAll(this) \
astINVOKE(V,astTestTickAll_(astCheckPlot(this),STATUS_PTR))

#define astClearForceExterior(this) \
astINVOKE(V,astClearForceExterior_(astCheckPlot(this),STATUS_PTR))
#define astGetForceExterior(this) \
astINVOKE(V,astGetForceExterior_(astCheckPlot(this),STATUS_PTR))
#define astSetForceExterior(this,frcext) \
astINVOKE(V,astSetForceExterior_(astCheckPlot(this),frcext,STATUS_PTR))
#define astTestForceExterior(this) \
astINVOKE(V,astTestForceExterior_(astCheckPlot(this),STATUS_PTR))

#define astClearBorder(this) \
astINVOKE(V,astClearBorder_(astCheckPlot(this),STATUS_PTR))
#define astGetBorder(this) \
astINVOKE(V,astGetBorder_(astCheckPlot(this),STATUS_PTR))
#define astSetBorder(this,border) \
astINVOKE(V,astSetBorder_(astCheckPlot(this),border,STATUS_PTR))
#define astTestBorder(this) \
astINVOKE(V,astTestBorder_(astCheckPlot(this),STATUS_PTR))

#define astClearClip(this) \
astINVOKE(V,astClearClip_(astCheckPlot(this),STATUS_PTR))
#define astGetClip(this) \
astINVOKE(V,astGetClip_(astCheckPlot(this),STATUS_PTR))
#define astSetClip(this,clip) \
astINVOKE(V,astSetClip_(astCheckPlot(this),clip,STATUS_PTR))
#define astTestClip(this) \
astINVOKE(V,astTestClip_(astCheckPlot(this),STATUS_PTR))

#define astClearClipOp(this) \
astINVOKE(V,astClearClipOp_(astCheckPlot(this),STATUS_PTR))
#define astGetClipOp(this) \
astINVOKE(V,astGetClipOp_(astCheckPlot(this),STATUS_PTR))
#define astSetClipOp(this,clipop) \
astINVOKE(V,astSetClipOp_(astCheckPlot(this),clipop,STATUS_PTR))
#define astTestClipOp(this) \
astINVOKE(V,astTestClipOp_(astCheckPlot(this),STATUS_PTR))

#define astClearInvisible(this) \
astINVOKE(V,astClearInvisible_(astCheckPlot(this),STATUS_PTR))
#define astGetInvisible(this) \
astINVOKE(V,astGetInvisible_(astCheckPlot(this),STATUS_PTR))
#define astSetInvisible(this,invisible) \
astINVOKE(V,astSetInvisible_(astCheckPlot(this),invisible,STATUS_PTR))
#define astTestInvisible(this) \
astINVOKE(V,astTestInvisible_(astCheckPlot(this),STATUS_PTR))

#define astClearGrf(this) \
astINVOKE(V,astClearGrf_(astCheckPlot(this),STATUS_PTR))
#define astGetGrf(this) \
astINVOKE(V,astGetGrf_(astCheckPlot(this),STATUS_PTR))
#define astSetGrf(this,grf) \
astINVOKE(V,astSetGrf_(astCheckPlot(this),grf,STATUS_PTR))
#define astTestGrf(this) \
astINVOKE(V,astTestGrf_(astCheckPlot(this),STATUS_PTR))

#define astClearDrawTitle(this) \
astINVOKE(V,astClearDrawTitle_(astCheckPlot(this),STATUS_PTR))
#define astGetDrawTitle(this) \
astINVOKE(V,astGetDrawTitle_(astCheckPlot(this),STATUS_PTR))
#define astSetDrawTitle(this,drawtitle) \
astINVOKE(V,astSetDrawTitle_(astCheckPlot(this),drawtitle,STATUS_PTR))
#define astTestDrawTitle(this) \
astINVOKE(V,astTestDrawTitle_(astCheckPlot(this),STATUS_PTR))

#define astClearDrawAxes(this,axis) \
astINVOKE(V,astClearDrawAxes_(astCheckPlot(this),axis,STATUS_PTR))
#define astGetDrawAxes(this,axis) \
astINVOKE(V,astGetDrawAxes_(astCheckPlot(this),axis,STATUS_PTR))
#define astSetDrawAxes(this,axis,drawaxes) \
astINVOKE(V,astSetDrawAxes_(astCheckPlot(this),axis,drawaxes,STATUS_PTR))
#define astTestDrawAxes(this,axis) \
astINVOKE(V,astTestDrawAxes_(astCheckPlot(this),axis,STATUS_PTR))

#define astClearAbbrev(this,axis) \
astINVOKE(V,astClearAbbrev_(astCheckPlot(this),axis,STATUS_PTR))
#define astGetAbbrev(this,axis) \
astINVOKE(V,astGetAbbrev_(astCheckPlot(this),axis,STATUS_PTR))
#define astSetAbbrev(this,axis,abbrev) \
astINVOKE(V,astSetAbbrev_(astCheckPlot(this),axis,abbrev,STATUS_PTR))
#define astTestAbbrev(this,axis) \
astINVOKE(V,astTestAbbrev_(astCheckPlot(this),axis,STATUS_PTR))

#define astClearEscape(this) \
astINVOKE(V,astClearEscape_(astCheckPlot(this),STATUS_PTR))
#define astGetEscape(this) \
astINVOKE(V,astGetEscape_(astCheckPlot(this),STATUS_PTR))
#define astSetEscape(this,escape) \
astINVOKE(V,astSetEscape_(astCheckPlot(this),escape,STATUS_PTR))
#define astTestEscape(this) \
astINVOKE(V,astTestEscape_(astCheckPlot(this),STATUS_PTR))

#define astClearLabelAt(this,axis) \
astINVOKE(V,astClearLabelAt_(astCheckPlot(this),axis,STATUS_PTR))
#define astGetLabelAt(this,axis) \
astINVOKE(V,astGetLabelAt_(astCheckPlot(this),axis,STATUS_PTR))
#define astSetLabelAt(this,axis,labelat) \
astINVOKE(V,astSetLabelAt_(astCheckPlot(this),axis,labelat,STATUS_PTR))
#define astTestLabelAt(this,axis) \
astINVOKE(V,astTestLabelAt_(astCheckPlot(this),axis,STATUS_PTR))

#define astClearGap(this,axis) \
astINVOKE(V,astClearGap_(astCheckPlot(this),axis,STATUS_PTR))
#define astGetGap(this,axis) \
astINVOKE(V,astGetGap_(astCheckPlot(this),axis,STATUS_PTR))
#define astSetGap(this,axis,gap) \
astINVOKE(V,astSetGap_(astCheckPlot(this),axis,gap,STATUS_PTR))
#define astTestGap(this,axis) \
astINVOKE(V,astTestGap_(astCheckPlot(this),axis,STATUS_PTR))

#define astClearLogGap(this,axis) \
astINVOKE(V,astClearLogGap_(astCheckPlot(this),axis,STATUS_PTR))
#define astGetLogGap(this,axis) \
astINVOKE(V,astGetLogGap_(astCheckPlot(this),axis,STATUS_PTR))
#define astSetLogGap(this,axis,gap) \
astINVOKE(V,astSetLogGap_(astCheckPlot(this),axis,gap,STATUS_PTR))
#define astTestLogGap(this,axis) \
astINVOKE(V,astTestLogGap_(astCheckPlot(this),axis,STATUS_PTR))

#define astClearCentre(this,axis) \
astINVOKE(V,astClearCentre_(astCheckPlot(this),axis,STATUS_PTR))
#define astGetCentre(this,axis) \
astINVOKE(V,astGetCentre_(astCheckPlot(this),axis,STATUS_PTR))
#define astSetCentre(this,axis,centre) \
astINVOKE(V,astSetCentre_(astCheckPlot(this),axis,centre,STATUS_PTR))
#define astTestCentre(this,axis) \
astINVOKE(V,astTestCentre_(astCheckPlot(this),axis,STATUS_PTR))

#define astClearMajTickLen(this,axis) \
astINVOKE(V,astClearMajTickLen_(astCheckPlot(this),axis,STATUS_PTR))
#define astGetMajTickLen(this,axis) \
astINVOKE(V,astGetMajTickLen_(astCheckPlot(this),axis,STATUS_PTR))
#define astSetMajTickLen(this,axis,majticklen) \
astINVOKE(V,astSetMajTickLen_(astCheckPlot(this),axis,majticklen,STATUS_PTR))
#define astTestMajTickLen(this,axis) \
astINVOKE(V,astTestMajTickLen_(astCheckPlot(this),axis,STATUS_PTR))

#define astClearMinTickLen(this,axis) \
astINVOKE(V,astClearMinTickLen_(astCheckPlot(this),axis,STATUS_PTR))
#define astGetMinTickLen(this,axis) \
astINVOKE(V,astGetMinTickLen_(astCheckPlot(this),axis,STATUS_PTR))
#define astSetMinTickLen(this,axis,minticklen) \
astINVOKE(V,astSetMinTickLen_(astCheckPlot(this),axis,minticklen,STATUS_PTR))
#define astTestMinTickLen(this,axis) \
astINVOKE(V,astTestMinTickLen_(astCheckPlot(this),axis,STATUS_PTR))

#define astClearNumLabGap(this,axis) \
astINVOKE(V,astClearNumLabGap_(astCheckPlot(this),axis,STATUS_PTR))
#define astGetNumLabGap(this,axis) \
astINVOKE(V,astGetNumLabGap_(astCheckPlot(this),axis,STATUS_PTR))
#define astSetNumLabGap(this,axis,numlabgap) \
astINVOKE(V,astSetNumLabGap_(astCheckPlot(this),axis,numlabgap,STATUS_PTR))
#define astTestNumLabGap(this,axis) \
astINVOKE(V,astTestNumLabGap_(astCheckPlot(this),axis,STATUS_PTR))

#define astClearTextLabGap(this,axis) \
astINVOKE(V,astClearTextLabGap_(astCheckPlot(this),axis,STATUS_PTR))
#define astGetTextLabGap(this,axis) \
astINVOKE(V,astGetTextLabGap_(astCheckPlot(this),axis,STATUS_PTR))
#define astSetTextLabGap(this,axis,textlabgap) \
astINVOKE(V,astSetTextLabGap_(astCheckPlot(this),axis,textlabgap,STATUS_PTR))
#define astTestTextLabGap(this,axis) \
astINVOKE(V,astTestTextLabGap_(astCheckPlot(this),axis,STATUS_PTR))

#define astClearTitleGap(this) \
astINVOKE(V,astClearTitleGap_(astCheckPlot(this),STATUS_PTR))
#define astGetTitleGap(this) \
astINVOKE(V,astGetTitleGap_(astCheckPlot(this),STATUS_PTR))
#define astSetTitleGap(this,titlegap) \
astINVOKE(V,astSetTitleGap_(astCheckPlot(this),titlegap,STATUS_PTR))
#define astTestTitleGap(this) \
astINVOKE(V,astTestTitleGap_(astCheckPlot(this),STATUS_PTR))

#define astClearLabelling(this) \
astINVOKE(V,astClearLabelling_(astCheckPlot(this),STATUS_PTR))
#define astGetLabelling(this) \
astINVOKE(V,astGetLabelling_(astCheckPlot(this),STATUS_PTR))
#define astSetLabelling(this,labelling) \
astINVOKE(V,astSetLabelling_(astCheckPlot(this),labelling,STATUS_PTR))
#define astTestLabelling(this) \
astINVOKE(V,astTestLabelling_(astCheckPlot(this),STATUS_PTR))

#define astClearEdge(this,axis) \
astINVOKE(V,astClearEdge_(astCheckPlot(this),axis,STATUS_PTR))
#define astGetEdge(this,axis) \
astINVOKE(V,astGetEdge_(astCheckPlot(this),axis,STATUS_PTR))
#define astSetEdge(this,axis,edge) \
astINVOKE(V,astSetEdge_(astCheckPlot(this),axis,edge,STATUS_PTR))
#define astTestEdge(this,axis) \
astINVOKE(V,astTestEdge_(astCheckPlot(this),axis,STATUS_PTR))

#define astClearMinTick(this,axis) \
astINVOKE(V,astClearMinTick_(astCheckPlot(this),axis,STATUS_PTR))
#define astGetMinTick(this,axis) \
astINVOKE(V,astGetMinTick_(astCheckPlot(this),axis,STATUS_PTR))
#define astSetMinTick(this,axis,mintick) \
astINVOKE(V,astSetMinTick_(astCheckPlot(this),axis,mintick,STATUS_PTR))
#define astTestMinTick(this,axis) \
astINVOKE(V,astTestMinTick_(astCheckPlot(this),axis,STATUS_PTR))

#define astClearNumLab(this,axis) \
astINVOKE(V,astClearNumLab_(astCheckPlot(this),axis,STATUS_PTR))
#define astGetNumLab(this,axis) \
astINVOKE(V,astGetNumLab_(astCheckPlot(this),axis,STATUS_PTR))
#define astSetNumLab(this,axis,numlab) \
astINVOKE(V,astSetNumLab_(astCheckPlot(this),axis,numlab,STATUS_PTR))
#define astTestNumLab(this,axis) \
astINVOKE(V,astTestNumLab_(astCheckPlot(this),axis,STATUS_PTR))

#define astClearLabelUp(this,axis) \
astINVOKE(V,astClearLabelUp_(astCheckPlot(this),axis,STATUS_PTR))
#define astGetLabelUp(this,axis) \
astINVOKE(V,astGetLabelUp_(astCheckPlot(this),axis,STATUS_PTR))
#define astSetLabelUp(this,axis,labelup) \
astINVOKE(V,astSetLabelUp_(astCheckPlot(this),axis,labelup,STATUS_PTR))
#define astTestLabelUp(this,axis) \
astINVOKE(V,astTestLabelUp_(astCheckPlot(this),axis,STATUS_PTR))

#define astClearLogPlot(this,axis) \
astINVOKE(V,astClearLogPlot_(astCheckPlot(this),axis,STATUS_PTR))
#define astGetLogPlot(this,axis) \
astINVOKE(V,astGetLogPlot_(astCheckPlot(this),axis,STATUS_PTR))
#define astSetLogPlot(this,axis,logplot) \
astINVOKE(V,astSetLogPlot_(astCheckPlot(this),axis,logplot,STATUS_PTR))
#define astTestLogPlot(this,axis) \
astINVOKE(V,astTestLogPlot_(astCheckPlot(this),axis,STATUS_PTR))

#define astClearLogTicks(this,axis) \
astINVOKE(V,astClearLogTicks_(astCheckPlot(this),axis,STATUS_PTR))
#define astGetLogTicks(this,axis) \
astINVOKE(V,astGetLogTicks_(astCheckPlot(this),axis,STATUS_PTR))
#define astSetLogTicks(this,axis,logticks) \
astINVOKE(V,astSetLogTicks_(astCheckPlot(this),axis,logticks,STATUS_PTR))
#define astTestLogTicks(this,axis) \
astINVOKE(V,astTestLogTicks_(astCheckPlot(this),axis,STATUS_PTR))

#define astClearLogLabel(this,axis) \
astINVOKE(V,astClearLogLabel_(astCheckPlot(this),axis,STATUS_PTR))
#define astGetLogLabel(this,axis) \
astINVOKE(V,astGetLogLabel_(astCheckPlot(this),axis,STATUS_PTR))
#define astSetLogLabel(this,axis,loglabel) \
astINVOKE(V,astSetLogLabel_(astCheckPlot(this),axis,loglabel,STATUS_PTR))
#define astTestLogLabel(this,axis) \
astINVOKE(V,astTestLogLabel_(astCheckPlot(this),axis,STATUS_PTR))

#define astClearTextLab(this,axis) \
astINVOKE(V,astClearTextLab_(astCheckPlot(this),axis,STATUS_PTR))
#define astGetTextLab(this,axis) \
astINVOKE(V,astGetTextLab_(astCheckPlot(this),axis,STATUS_PTR))
#define astSetTextLab(this,axis,textlab) \
astINVOKE(V,astSetTextLab_(astCheckPlot(this),axis,textlab,STATUS_PTR))
#define astTestTextLab(this,axis) \
astINVOKE(V,astTestTextLab_(astCheckPlot(this),axis,STATUS_PTR))

#define astClearLabelUnits(this,axis) \
astINVOKE(V,astClearLabelUnits_(astCheckPlot(this),axis,STATUS_PTR))
#define astGetLabelUnits(this,axis) \
astINVOKE(V,astGetLabelUnits_(astCheckPlot(this),axis,STATUS_PTR))
#define astSetLabelUnits(this,axis,labelunits) \
astINVOKE(V,astSetLabelUnits_(astCheckPlot(this),axis,labelunits,STATUS_PTR))
#define astTestLabelUnits(this,axis) \
astINVOKE(V,astTestLabelUnits_(astCheckPlot(this),axis,STATUS_PTR))

#define astClearStyle(this,axis) \
astINVOKE(V,astClearStyle_(astCheckPlot(this),axis,STATUS_PTR))
#define astGetStyle(this,axis) \
astINVOKE(V,astGetStyle_(astCheckPlot(this),axis,STATUS_PTR))
#define astSetStyle(this,axis,style) \
astINVOKE(V,astSetStyle_(astCheckPlot(this),axis,style,STATUS_PTR))
#define astTestStyle(this,axis) \
astINVOKE(V,astTestStyle_(astCheckPlot(this),axis,STATUS_PTR))

#define astClearFont(this,axis) \
astINVOKE(V,astClearFont_(astCheckPlot(this),axis,STATUS_PTR))
#define astGetFont(this,axis) \
astINVOKE(V,astGetFont_(astCheckPlot(this),axis,STATUS_PTR))
#define astSetFont(this,axis,font) \
astINVOKE(V,astSetFont_(astCheckPlot(this),axis,font,STATUS_PTR))
#define astTestFont(this,axis) \
astINVOKE(V,astTestFont_(astCheckPlot(this),axis,STATUS_PTR))

#define astClearColour(this,axis) \
astINVOKE(V,astClearColour_(astCheckPlot(this),axis,STATUS_PTR))
#define astGetColour(this,axis) \
astINVOKE(V,astGetColour_(astCheckPlot(this),axis,STATUS_PTR))
#define astSetColour(this,axis,colour) \
astINVOKE(V,astSetColour_(astCheckPlot(this),axis,colour,STATUS_PTR))
#define astTestColour(this,axis) \
astINVOKE(V,astTestColour_(astCheckPlot(this),axis,STATUS_PTR))

#define astClearWidth(this,axis) \
astINVOKE(V,astClearWidth_(astCheckPlot(this),axis,STATUS_PTR))
#define astGetWidth(this,axis) \
astINVOKE(V,astGetWidth_(astCheckPlot(this),axis,STATUS_PTR))
#define astSetWidth(this,axis,width) \
astINVOKE(V,astSetWidth_(astCheckPlot(this),axis,width,STATUS_PTR))
#define astTestWidth(this,axis) \
astINVOKE(V,astTestWidth_(astCheckPlot(this),axis,STATUS_PTR))

#define astClearSize(this,axis) \
astINVOKE(V,astClearSize_(astCheckPlot(this),axis,STATUS_PTR))
#define astGetSize(this,axis) \
astINVOKE(V,astGetSize_(astCheckPlot(this),axis,STATUS_PTR))
#define astSetSize(this,axis,size) \
astINVOKE(V,astSetSize_(astCheckPlot(this),axis,size,STATUS_PTR))
#define astTestSize(this,axis) \
astINVOKE(V,astTestSize_(astCheckPlot(this),axis,STATUS_PTR))
#endif
#endif





