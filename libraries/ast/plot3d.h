#if !defined( PLOT3D_INCLUDED ) /* Include this file only once */
#define PLOT3D_INCLUDED
/*
*+
*  Name:
*     plot3d.h

*  Type:
*     C include file.

*  Purpose:
*     Define the interface to the Plot3D class.

*  Invocation:
*     #include "plot3d.h"

*  Description:
*     This include file defines the interface to the Plot3D class
*     and provides the type definitions, function prototypes and
*     macros, etc. needed to use this class.

*  Copyright:
*     Copyright (C) 2007 Science & Technology Facilities Council.
*     All Rights Reserved.

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
*     6-JUN-2007 (DSB):
*        Original version.
*-
*/

/* Include files. */
/* ============== */
/* Interface definitions. */
/* ---------------------- */
#include "object.h"              /* Base Object class */
#include "plot.h"               /* Parent Plot class */

/* Macros. */
/* ======= */
#if defined(astCLASS) || defined(astFORTRAN77)
#define STATUS_PTR status
#else
#define STATUS_PTR astGetStatusPtr
#endif


#if defined(astCLASS)            /* Protected */

#endif

/* Type Definitions. */
/* ================= */

/* Plot3D structure. */
/* ------------------- */
/* This structure contains all information that is unique to each object in
   the class (e.g. its instance variables). */
typedef struct AstPlot3D {

/* Attributes inherited from the parent class. */
   AstPlot plot;               /* Parent class structure */

/* Attributes specific to objects in this class. */
   AstPlot *plotxy;            /* Plot describing the XY plane */
   AstPlot *plotxz;            /* Plot describing the XZ plane */
   AstPlot *plotyz;            /* Plot describing the YZ plane */
   double gbox[6];             /* Graphics box */
   int pix_frame;              /* Index of original base Frame */
   int rootcorner;             /* Corner at junction of the annotated axes */
   int baseplot;               /* The Plot that is used to label 2 3D axes */
   int axis_plot1[3];          /* The Plot used to label each 3D axis */
   int axis_index1[3];         /* The axis index within the axis_plot1 Plot */
   int axis_plot2[3];          /* The other Plot touching each 3D axis */
   int axis_index2[3];         /* The axis index within the axis_plot2 Plot */
   double norm[3];             /* Normal vector for text plane */
} AstPlot3D;

/* Virtual function table. */
/* ----------------------- */
/* This table contains all information that is the same for all objects in the
   class (e.g. pointers to its virtual functions). */
#if defined(astCLASS)            /* Protected */
typedef struct AstPlot3DVtab {

/* Properties (e.g. methods) inherited from the parent class. */
   AstPlotVtab plot_vtab;      /* Parent class virtual function table */

/* A Unique identifier to determine class membership. */
   AstClassIdentifier id;

   int (* GetRootCorner)( AstPlot3D *, int * );
   int (* TestRootCorner)( AstPlot3D *, int * );
   void (* SetRootCorner)( AstPlot3D *, int, int * );
   void (* ClearRootCorner)( AstPlot3D *, int * );

   double (* GetNorm)( AstPlot3D *, int, int * );
   int (* TestNorm)( AstPlot3D *, int, int * );
   void (* SetNorm)( AstPlot3D *, int, double, int * );
   void (* ClearNorm)( AstPlot3D *, int, int * );

} AstPlot3DVtab;

#if defined(THREAD_SAFE)

/* Define a structure holding all data items that are global within this
   class. */
typedef struct AstPlot3DGlobals {
   AstPlot3DVtab Class_Vtab;
   int Class_Init;
   char GetAttrib_Buff[ 101 ];
} AstPlot3DGlobals;

#endif

#endif

/* Function prototypes. */
/* ==================== */
/* Prototypes for standard class functions. */
/* ---------------------------------------- */
astPROTO_CHECK(Plot3D)         /* Check class membership */
astPROTO_ISA(Plot3D)           /* Test class membership */

/* Constructor. */
#if defined(astCLASS)            /* Protected */
AstPlot3D *astPlot3D_( void *, const float *, const double *, const char *, int *, ...);
#else
AstPlot3D *astPlot3DId_( void *, const float [], const double [], const char *, ... );
#endif

#if defined(astCLASS)            /* Protected */

/* Initialiser. */
AstPlot3D *astInitPlot3D_( void *, size_t, int, AstPlot3DVtab *,
                           const char *, AstFrame *, const float *,
                           const double *, int * );

/* Vtab initialiser. */
void astInitPlot3DVtab_( AstPlot3DVtab *, const char *, int * );

/* Loader. */
AstPlot3D *astLoadPlot3D_( void *, size_t,
                                         AstPlot3DVtab *,
                                         const char *, AstChannel *channel, int * );

/* Thread-safe initialiser for all global data used by this module. */
#if defined(THREAD_SAFE)
void astInitPlot3DGlobals_( AstPlot3DGlobals * );
#endif

#endif

/* Prototypes for member functions. */
/* -------------------------------- */

#if defined(astCLASS)            /* Protected */

   int astGetRootCorner_( AstPlot3D *, int * );
   int astTestRootCorner_( AstPlot3D *, int * );
   void astSetRootCorner_( AstPlot3D *, int, int * );
   void astClearRootCorner_( AstPlot3D *, int * );

   double astGetNorm_( AstPlot3D *, int, int * );
   int astTestNorm_( AstPlot3D *, int, int * );
   void astSetNorm_( AstPlot3D *, int, double, int * );
   void astClearNorm_( AstPlot3D *, int, int * );

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
#define astCheckPlot3D(this) astINVOKE_CHECK(Plot3D,this,0)
#define astVerifyPlot3D(this) astINVOKE_CHECK(Plot3D,this,1)

/* Test class membership. */
#define astIsAPlot3D(this) astINVOKE_ISA(Plot3D,this)

/* Constructor. */
#if defined(astCLASS)            /* Protected */
#define astPlot3D astINVOKE(F,astPlot3D_)
#else
#define astPlot3D astINVOKE(F,astPlot3DId_)
#endif

#if defined(astCLASS)            /* Protected */

/* Initialiser. */
#define astInitPlot3D(mem,size,init,vtab,name,frame,graph,base) \
astINVOKE(O,astInitPlot3D_(mem,size,init,vtab,name,frame,graph,base,STATUS_PTR))

/* Vtab Initialiser. */
#define astInitPlot3DVtab(vtab,name) astINVOKE(V,astInitPlot3DVtab_(vtab,name,STATUS_PTR))
/* Loader. */
#define astLoadPlot3D(mem,size,vtab,name,channel) \
astINVOKE(O,astLoadPlot3D_(mem,size,vtab,name,astCheckChannel(channel),STATUS_PTR))

#endif

/* Interfaces to public member functions. */
/* -------------------------------------- */

/* Interfaces to protected member functions. */
/* ----------------------------------------- */
/* Here we make use of astCheckPlot3D to validate Plot3D pointers
   before use. This provides a contextual error report if a pointer to
   the wrong sort of object is supplied. */

#if defined(astCLASS)            /* Protected */

#define astGetRootCorner(this) astINVOKE(V,astGetRootCorner_(astCheckPlot3D(this),STATUS_PTR))
#define astTestRootCorner(this) astINVOKE(V,astTestRootCorner_(astCheckPlot3D(this),STATUS_PTR))
#define astClearRootCorner(this) astINVOKE(V,astClearRootCorner_(astCheckPlot3D(this),STATUS_PTR))
#define astSetRootCorner(this,value) astINVOKE(V,astSetRootCorner_(astCheckPlot3D(this),value,STATUS_PTR))

#define astGetNorm(this,axis) astINVOKE(V,astGetNorm_(astCheckPlot3D(this),axis,STATUS_PTR))
#define astTestNorm(this,axis) astINVOKE(V,astTestNorm_(astCheckPlot3D(this),axis,STATUS_PTR))
#define astClearNorm(this,axis) astINVOKE(V,astClearNorm_(astCheckPlot3D(this),axis,STATUS_PTR))
#define astSetNorm(this,axis,value) astINVOKE(V,astSetNorm_(astCheckPlot3D(this),axis,value,STATUS_PTR))

#endif
#endif





