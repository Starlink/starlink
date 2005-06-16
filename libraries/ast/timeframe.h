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
*     <COPYRIGHT_STATEMENT>

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
#define AST__TT           6
#define AST__TDB          7
#define AST__TCB          8
#define AST__TCG          9

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
   double clocklat;              /* Geodetic latitude of observer */
   double clocklon;              /* Geodetic longitude of observer */
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

/* Unique flag value to determine class membership. */
   int *check;                   /* Check value */

/* Properties (e.g. methods) specific to this class. */
   double (* CurrentTime)( AstTimeFrame * );

   double (* GetClockLon)( AstTimeFrame * );
   int (* TestClockLon)( AstTimeFrame * );
   void (* ClearClockLon)( AstTimeFrame * );
   void (* SetClockLon)( AstTimeFrame *, double );

   double (* GetClockLat)( AstTimeFrame * );
   int (* TestClockLat)( AstTimeFrame * );
   void (* ClearClockLat)( AstTimeFrame * );
   void (* SetClockLat)( AstTimeFrame *, double );

   double (* GetTimeOrigin)( AstTimeFrame * );
   int (* TestTimeOrigin)( AstTimeFrame * );
   void (* ClearTimeOrigin)( AstTimeFrame * );
   void (* SetTimeOrigin)( AstTimeFrame *, double );

   AstTimeScaleType (* GetTimeScale)( AstTimeFrame * );
   int (* TestTimeScale)( AstTimeFrame * );
   void (* ClearTimeScale)( AstTimeFrame * );
   void (* SetTimeScale)( AstTimeFrame *, AstTimeScaleType );

   AstTimeScaleType (* GetAlignTimeScale)( AstTimeFrame * );
   int (* TestAlignTimeScale)( AstTimeFrame * );
   void (* ClearAlignTimeScale)( AstTimeFrame * );
   void (* SetAlignTimeScale)( AstTimeFrame *, AstTimeScaleType );

} AstTimeFrameVtab;
#endif

/* Function prototypes. */
/* ==================== */
/* Prototypes for standard class functions. */
/* ---------------------------------------- */
astPROTO_CHECK(TimeFrame)         /* Check class membership */
astPROTO_ISA(TimeFrame)           /* Test class membership */

/* Constructor. */
#if defined(astCLASS)            /* Protected */
AstTimeFrame *astTimeFrame_( const char *, ... );
#else
AstTimeFrame *astTimeFrameId_( const char *, ... );
#endif

#if defined(astCLASS)            /* Protected */

/* Initialiser. */
AstTimeFrame *astInitTimeFrame_( void *, size_t, int, AstTimeFrameVtab *,
                                 const char * );

/* Vtab initialiser. */
void astInitTimeFrameVtab_( AstTimeFrameVtab *, const char * );

/* Loader. */
AstTimeFrame *astLoadTimeFrame_( void *, size_t, AstTimeFrameVtab *,
                                 const char *, AstChannel *channel );
#endif

/* Prototypes for member functions. */
/* -------------------------------- */
double astCurrentTime_( AstTimeFrame * );

#if defined(astCLASS)            /* Protected */

double astGetClockLon_( AstTimeFrame * );
int astTestClockLon_( AstTimeFrame * );
void astClearClockLon_( AstTimeFrame * );
void astSetClockLon_( AstTimeFrame *, double );

double astGetClockLat_( AstTimeFrame * );
int astTestClockLat_( AstTimeFrame * );
void astClearClockLat_( AstTimeFrame * );
void astSetClockLat_( AstTimeFrame *, double );

double astGetTimeOrigin_( AstTimeFrame * );
int astTestTimeOrigin_( AstTimeFrame * );
void astClearTimeOrigin_( AstTimeFrame * );
void astSetTimeOrigin_( AstTimeFrame *, double );

AstTimeScaleType astGetTimeScale_( AstTimeFrame * );
int astTestTimeScale_( AstTimeFrame * );
void astClearTimeScale_( AstTimeFrame * );
void astSetTimeScale_( AstTimeFrame *, AstTimeScaleType );

AstTimeScaleType astGetAlignTimeScale_( AstTimeFrame * );
int astTestAlignTimeScale_( AstTimeFrame * );
void astClearAlignTimeScale_( AstTimeFrame * );
void astSetAlignTimeScale_( AstTimeFrame *, AstTimeScaleType );
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
#define astCheckTimeFrame(this) astINVOKE_CHECK(TimeFrame,this)

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
astINVOKE(O,astInitTimeFrame_(mem,size,init,vtab,name))

/* Vtab Initialiser. */
#define astInitTimeFrameVtab(vtab,name) astINVOKE(V,astInitTimeFrameVtab_(vtab,name))
/* Loader. */
#define astLoadTimeFrame(mem,size,vtab,name,channel) \
astINVOKE(O,astLoadTimeFrame_(mem,size,vtab,name,astCheckChannel(channel)))

#endif

/* Interfaces to public member functions. */
/* -------------------------------------- */

/* None. */

/* Interfaces to protected member functions. */
/* ----------------------------------------- */
/* Here we make use of astCheckTimeFrame to validate TimeFrame pointers
   before use. This provides a contextual error report if a pointer to
   the wrong sort of object is supplied. */

#define astCurrentTime(this) astINVOKE(V,astCurrentTime_(astCheckTimeFrame(this)))

#if defined(astCLASS)            /* Protected */

#define astGetClockLon(this) astINVOKE(V,astGetClockLon_(astCheckTimeFrame(this)))
#define astTestClockLon(this) astINVOKE(V,astTestClockLon_(astCheckTimeFrame(this)))
#define astClearClockLon(this) astINVOKE(V,astClearClockLon_(astCheckTimeFrame(this)))
#define astSetClockLon(this,value) astINVOKE(V,astSetClockLon_(astCheckTimeFrame(this),value))

#define astGetClockLat(this) astINVOKE(V,astGetClockLat_(astCheckTimeFrame(this)))
#define astTestClockLat(this) astINVOKE(V,astTestClockLat_(astCheckTimeFrame(this)))
#define astClearClockLat(this) astINVOKE(V,astClearClockLat_(astCheckTimeFrame(this)))
#define astSetClockLat(this,value) astINVOKE(V,astSetClockLat_(astCheckTimeFrame(this),value))

#define astGetTimeOrigin(this) astINVOKE(V,astGetTimeOrigin_(astCheckTimeFrame(this)))
#define astTestTimeOrigin(this) astINVOKE(V,astTestTimeOrigin_(astCheckTimeFrame(this)))
#define astClearTimeOrigin(this) astINVOKE(V,astClearTimeOrigin_(astCheckTimeFrame(this)))
#define astSetTimeOrigin(this,value) astINVOKE(V,astSetTimeOrigin_(astCheckTimeFrame(this),value))

#define astGetTimeScale(this) astINVOKE(V,astGetTimeScale_(astCheckTimeFrame(this)))
#define astTestTimeScale(this) astINVOKE(V,astTestTimeScale_(astCheckTimeFrame(this)))
#define astClearTimeScale(this) astINVOKE(V,astClearTimeScale_(astCheckTimeFrame(this)))
#define astSetTimeScale(this,value) astINVOKE(V,astSetTimeScale_(astCheckTimeFrame(this),value))

#define astGetAlignTimeScale(this) astINVOKE(V,astGetAlignTimeScale_(astCheckTimeFrame(this)))
#define astTestAlignTimeScale(this) astINVOKE(V,astTestAlignTimeScale_(astCheckTimeFrame(this)))
#define astClearAlignTimeScale(this) astINVOKE(V,astClearAlignTimeScale_(astCheckTimeFrame(this)))
#define astSetAlignTimeScale(this,value) astINVOKE(V,astSetAlignTimeScale_(astCheckTimeFrame(this),value))

#endif
#endif
