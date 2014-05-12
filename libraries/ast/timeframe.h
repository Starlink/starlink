#if !defined( TIMEFRAME_INCLUDED ) /* Include this file only once */
#define TIMEFRAME_INCLUDED
/*
*+
*  Name:
*     timeframe.h

*  Type:
*     C include file.

*  Purpose:
*     Define the interface to the TimeFrame class.

*  Invocation:
*     #include "timeframe.h"

*  Description:
*     This include file defines the interface to the TimeFrame class
*     and provides the type definitions, function prototypes and
*     macros, etc. needed to use this class.

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
*     20-MAY-2005 (DSB):
*        Original version.
*-
*/

/* Include files. */
/* ============== */
/* Interface definitions. */
/* ---------------------- */
#include "object.h"              /* Base Object class */
#include "frame.h"               /* Parent Frame class */
#include "skyframe.h"            /* Celestial coordinate systems */

/* Macros. */
/* ======= */
#if defined(astCLASS) || defined(astFORTRAN77)
#define STATUS_PTR status
#else
#define STATUS_PTR astGetStatusPtr
#endif

/* Define a dummy __attribute__ macro for use on non-GNU compilers. */
#ifndef __GNUC__
#  define  __attribute__(x)  /*NOTHING*/
#endif

#if defined(astCLASS)            /* Protected */

/* Values used to represent different System attribute values. */
#define AST__MJD    1
#define AST__JD     2
#define AST__JEPOCH 3
#define AST__BEPOCH 4

/* Values used to represent different TimeScale attribute values. */
#define AST__BADTS        0
#define AST__TAI          1
#define AST__UTC          2
#define AST__UT1          3
#define AST__GMST         4
#define AST__LAST         5
#define AST__LMST         6
#define AST__TT           7
#define AST__TDB          8
#define AST__TCB          9
#define AST__TCG         10
#define AST__LT          11

/* Define constants used to size global arrays in this module. */
#define AST__TIMEFRAME_FORMAT_BUFF_LEN 200
#define AST__TIMEFRAME_GETATTRIB_BUFF_LEN 50
#define AST__TIMEFRAME_GETLABEL_BUFF_LEN 200
#define AST__TIMEFRAME_GETSYMBOL_BUFF_LEN 20
#define AST__TIMEFRAME_GETTITLE_BUFF_LEN 200

#endif

/* Type Definitions. */
/* ================= */

/* Integer type used to store the TimeScale attribute. */
typedef int AstTimeScaleType;

/* TimeFrame structure. */
/* ------------------- */
/* This structure contains all information that is unique to each object in
   the class (e.g. its instance variables). */
typedef struct AstTimeFrame {

/* Attributes inherited from the parent class. */
   AstFrame frame;               /* Parent class structure */

/* Attributes specific to objects in this class. */
   double ltoffset;              /* Offset from UTC to Local Time */
   double timeorigin;            /* Zero point for time axis */
   AstTimeScaleType timescale;   /* Time scale */
   AstTimeScaleType aligntimescale; /* Alignment time scale */
} AstTimeFrame;

/* Virtual function table. */
/* ----------------------- */
/* This table contains all information that is the same for all objects in the
   class (e.g. pointers to its virtual functions). */
#if defined(astCLASS)            /* Protected */
typedef struct AstTimeFrameVtab {

/* Properties (e.g. methods) inherited from the parent class. */
   AstFrameVtab frame_vtab;      /* Parent class virtual function table */

/* A Unique identifier to determine class membership. */
   AstClassIdentifier id;

/* Properties (e.g. methods) specific to this class. */
   double (* CurrentTime)( AstTimeFrame *, int * );

   double (* GetLTOffset)( AstTimeFrame *, int * );
   int (* TestLTOffset)( AstTimeFrame *, int * );
   void (* ClearLTOffset)( AstTimeFrame *, int * );
   void (* SetLTOffset)( AstTimeFrame *, double, int * );

   double (* GetTimeOrigin)( AstTimeFrame *, int * );
   int (* TestTimeOrigin)( AstTimeFrame *, int * );
   void (* ClearTimeOrigin)( AstTimeFrame *, int * );
   void (* SetTimeOrigin)( AstTimeFrame *, double, int * );

   AstTimeScaleType (* GetTimeScale)( AstTimeFrame *, int * );
   int (* TestTimeScale)( AstTimeFrame *, int * );
   void (* ClearTimeScale)( AstTimeFrame *, int * );
   void (* SetTimeScale)( AstTimeFrame *, AstTimeScaleType, int * );

   AstTimeScaleType (* GetAlignTimeScale)( AstTimeFrame *, int * );
   int (* TestAlignTimeScale)( AstTimeFrame *, int * );
   void (* ClearAlignTimeScale)( AstTimeFrame *, int * );
   void (* SetAlignTimeScale)( AstTimeFrame *, AstTimeScaleType, int * );

} AstTimeFrameVtab;

#if defined(THREAD_SAFE)

/* Define a structure holding all data items that are global within this
   class. */
typedef struct AstTimeFrameGlobals {
   AstTimeFrameVtab Class_Vtab;
   int Class_Init;
   char Format_Buff[ AST__TIMEFRAME_FORMAT_BUFF_LEN + 1 ];
   char GetAttrib_Buff[ AST__TIMEFRAME_GETATTRIB_BUFF_LEN + 1 ];
   char GetLabel_Buff[ AST__TIMEFRAME_GETLABEL_BUFF_LEN + 1 ];
   char GetSymbol_Buff[ AST__TIMEFRAME_GETSYMBOL_BUFF_LEN + 1 ];
   char GetTitle_Buff[ AST__TIMEFRAME_GETTITLE_BUFF_LEN + 1 ];
} AstTimeFrameGlobals;

#endif

#endif

/* Function prototypes. */
/* ==================== */
/* Prototypes for standard class functions. */
/* ---------------------------------------- */
astPROTO_CHECK(TimeFrame)         /* Check class membership */
astPROTO_ISA(TimeFrame)           /* Test class membership */

/* Constructor. */
#if defined(astCLASS)            /* Protected */
AstTimeFrame *astTimeFrame_( const char *, int *, ...);
#else
AstTimeFrame *astTimeFrameId_( const char *, ... )__attribute__((format(printf,1,2)));
#endif

#if defined(astCLASS)            /* Protected */

/* Initialiser. */
AstTimeFrame *astInitTimeFrame_( void *, size_t, int, AstTimeFrameVtab *,
                                 const char *, int * );

/* Vtab initialiser. */
void astInitTimeFrameVtab_( AstTimeFrameVtab *, const char *, int * );

/* Loader. */
AstTimeFrame *astLoadTimeFrame_( void *, size_t, AstTimeFrameVtab *,
                                 const char *, AstChannel *channel, int * );

/* Thread-safe initialiser for all global data used by this module. */
#if defined(THREAD_SAFE)
void astInitTimeFrameGlobals_( AstTimeFrameGlobals * );
#endif

#endif

/* Prototypes for member functions. */
/* -------------------------------- */
double astCurrentTime_( AstTimeFrame *, int * );

#if defined(astCLASS)            /* Protected */

double astGetLTOffset_( AstTimeFrame *, int * );
int astTestLTOffset_( AstTimeFrame *, int * );
void astClearLTOffset_( AstTimeFrame *, int * );
void astSetLTOffset_( AstTimeFrame *, double, int * );

double astGetTimeOrigin_( AstTimeFrame *, int * );
int astTestTimeOrigin_( AstTimeFrame *, int * );
void astClearTimeOrigin_( AstTimeFrame *, int * );
void astSetTimeOrigin_( AstTimeFrame *, double, int * );

AstTimeScaleType astGetTimeScale_( AstTimeFrame *, int * );
int astTestTimeScale_( AstTimeFrame *, int * );
void astClearTimeScale_( AstTimeFrame *, int * );
void astSetTimeScale_( AstTimeFrame *, AstTimeScaleType, int * );

AstTimeScaleType astGetAlignTimeScale_( AstTimeFrame *, int * );
int astTestAlignTimeScale_( AstTimeFrame *, int * );
void astClearAlignTimeScale_( AstTimeFrame *, int * );
void astSetAlignTimeScale_( AstTimeFrame *, AstTimeScaleType, int * );
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
#define astCheckTimeFrame(this) astINVOKE_CHECK(TimeFrame,this,0)
#define astVerifyTimeFrame(this) astINVOKE_CHECK(TimeFrame,this,1)

/* Test class membership. */
#define astIsATimeFrame(this) astINVOKE_ISA(TimeFrame,this)

/* Constructor. */
#if defined(astCLASS)            /* Protected */
#define astTimeFrame astINVOKE(F,astTimeFrame_)
#else
#define astTimeFrame astINVOKE(F,astTimeFrameId_)
#endif

#if defined(astCLASS)            /* Protected */

/* Initialiser. */
#define astInitTimeFrame(mem,size,init,vtab,name) \
astINVOKE(O,astInitTimeFrame_(mem,size,init,vtab,name,STATUS_PTR))

/* Vtab Initialiser. */
#define astInitTimeFrameVtab(vtab,name) astINVOKE(V,astInitTimeFrameVtab_(vtab,name,STATUS_PTR))
/* Loader. */
#define astLoadTimeFrame(mem,size,vtab,name,channel) \
astINVOKE(O,astLoadTimeFrame_(mem,size,vtab,name,astCheckChannel(channel),STATUS_PTR))

#endif

/* Interfaces to public member functions. */
/* -------------------------------------- */

/* None. */

/* Interfaces to protected member functions. */
/* ----------------------------------------- */
/* Here we make use of astCheckTimeFrame to validate TimeFrame pointers
   before use. This provides a contextual error report if a pointer to
   the wrong sort of object is supplied. */

#define astCurrentTime(this) astINVOKE(V,astCurrentTime_(astCheckTimeFrame(this),STATUS_PTR))

#if defined(astCLASS)            /* Protected */

#define astGetTimeOrigin(this) astINVOKE(V,astGetTimeOrigin_(astCheckTimeFrame(this),STATUS_PTR))
#define astTestTimeOrigin(this) astINVOKE(V,astTestTimeOrigin_(astCheckTimeFrame(this),STATUS_PTR))
#define astClearTimeOrigin(this) astINVOKE(V,astClearTimeOrigin_(astCheckTimeFrame(this),STATUS_PTR))
#define astSetTimeOrigin(this,value) astINVOKE(V,astSetTimeOrigin_(astCheckTimeFrame(this),value,STATUS_PTR))

#define astGetLTOffset(this) astINVOKE(V,astGetLTOffset_(astCheckTimeFrame(this),STATUS_PTR))
#define astTestLTOffset(this) astINVOKE(V,astTestLTOffset_(astCheckTimeFrame(this),STATUS_PTR))
#define astClearLTOffset(this) astINVOKE(V,astClearLTOffset_(astCheckTimeFrame(this),STATUS_PTR))
#define astSetLTOffset(this,value) astINVOKE(V,astSetLTOffset_(astCheckTimeFrame(this),value,STATUS_PTR))

#define astGetTimeScale(this) astINVOKE(V,astGetTimeScale_(astCheckTimeFrame(this),STATUS_PTR))
#define astTestTimeScale(this) astINVOKE(V,astTestTimeScale_(astCheckTimeFrame(this),STATUS_PTR))
#define astClearTimeScale(this) astINVOKE(V,astClearTimeScale_(astCheckTimeFrame(this),STATUS_PTR))
#define astSetTimeScale(this,value) astINVOKE(V,astSetTimeScale_(astCheckTimeFrame(this),value,STATUS_PTR))

#define astGetAlignTimeScale(this) astINVOKE(V,astGetAlignTimeScale_(astCheckTimeFrame(this),STATUS_PTR))
#define astTestAlignTimeScale(this) astINVOKE(V,astTestAlignTimeScale_(astCheckTimeFrame(this),STATUS_PTR))
#define astClearAlignTimeScale(this) astINVOKE(V,astClearAlignTimeScale_(astCheckTimeFrame(this),STATUS_PTR))
#define astSetAlignTimeScale(this,value) astINVOKE(V,astSetAlignTimeScale_(astCheckTimeFrame(this),value,STATUS_PTR))

#endif
#endif





