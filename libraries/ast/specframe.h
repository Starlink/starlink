#if !defined( SPECFRAME_INCLUDED ) /* Include this file only once */
#define SPECFRAME_INCLUDED
/*
*+
*  Name:
*     specframe.h

*  Type:
*     C include file.

*  Purpose:
*     Define the interface to the SpecFrame class.

*  Invocation:
*     #include "specframe.h"

*  Description:
*     This include file defines the interface to the SpecFrame class
*     and provides the type definitions, function prototypes and
*     macros, etc. needed to use this class.

*  Copyright:
*     <COPYRIGHT_STATEMENT>

*  Authors:
*     DSB: David S. Berry (Starlink)

*  History:
*     12-NOV-2002 (DSB):
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
#define AST__FREQ          1
#define AST__ENERGY        2
#define AST__WAVENUM       3
#define AST__WAVELEN       4
#define AST__AIRWAVE       5
#define AST__VRADIO        6
#define AST__VOPTICAL      7
#define AST__REDSHIFT      8
#define AST__BETA          9
#define AST__VREL          10

/* Values used to represent different StdOfRest attribute values. */
#define AST__BADSOR        0
#define AST__TPSOR         1
#define AST__GESOR         2
#define AST__BYSOR         3
#define AST__HLSOR         4
#define AST__LDSOR         5
#define AST__LKSOR         6
#define AST__LGSOR         7
#define AST__GLSOR         8
#define AST__SCSOR         9
#endif

/* Type Definitions. */
/* ================= */

/* Integer type used to store the spectral StdOfRest attribute. */
typedef int AstStdOfRestType;

/* SpecFrame structure. */
/* ------------------- */
/* This structure contains all information that is unique to each object in
   the class (e.g. its instance variables). */
typedef struct AstSpecFrame {

/* Attributes inherited from the parent class. */
   AstFrame frame;               /* Parent class structure */

/* Attributes specific to objects in this class. */
   AstStdOfRestType alignstdofrest;/* Code identifying alignment StdOfRest */
   AstStdOfRestType stdofrest;   /* Standard of rest */
   double refdec;                /* Dec (FK5 J2000) of source */
   double refra;                 /* RA (FK5 J2000) of source */
   double restfreq;              /* Rest frequency (Hz)*/
   double sourcevel;             /* Source velocity (heliocentric, m/s) */
   AstStdOfRestType sourcevrf;   /* Code identifying source vel. StdOfRest */
   int nuunits;                  /* Size of usedunits array */
   char **usedunits;             /* Last used units for each system */
} AstSpecFrame;

/* Virtual function table. */
/* ----------------------- */
/* This table contains all information that is the same for all objects in the
   class (e.g. pointers to its virtual functions). */
#if defined(astCLASS)            /* Protected */
typedef struct AstSpecFrameVtab {

/* Properties (e.g. methods) inherited from the parent class. */
   AstFrameVtab frame_vtab;      /* Parent class virtual function table */

/* Unique flag value to determine class membership. */
   int *check;                   /* Check value */

/* Properties (e.g. methods) specific to this class. */
   void (* GetRefPos)( AstSpecFrame *, AstSkyFrame *, double *, double * );
   void (* SetRefPos)( AstSpecFrame *, AstSkyFrame *, double, double );

   AstStdOfRestType (* GetStdOfRest)( AstSpecFrame * );
   int (* TestStdOfRest)( AstSpecFrame * );
   void (* ClearStdOfRest)( AstSpecFrame * );
   void (* SetStdOfRest)( AstSpecFrame *, AstStdOfRestType );

   AstStdOfRestType (* GetAlignStdOfRest)( AstSpecFrame * );
   int (* TestAlignStdOfRest)( AstSpecFrame * );
   void (* ClearAlignStdOfRest)( AstSpecFrame * );
   void (* SetAlignStdOfRest)( AstSpecFrame *, AstStdOfRestType );

   AstStdOfRestType (* GetSourceVRF)( AstSpecFrame * );
   int (* TestSourceVRF)( AstSpecFrame * );
   void (* ClearSourceVRF)( AstSpecFrame * );
   void (* SetSourceVRF)( AstSpecFrame *, AstStdOfRestType );

   double (* GetRestFreq)( AstSpecFrame * );
   int (* TestRestFreq)( AstSpecFrame * );
   void (* ClearRestFreq)( AstSpecFrame * );
   void (* SetRestFreq)( AstSpecFrame *, double );

   double (* GetRefRA)( AstSpecFrame * );
   int (* TestRefRA)( AstSpecFrame * );
   void (* ClearRefRA)( AstSpecFrame * );
   void (* SetRefRA)( AstSpecFrame *, double );

   double (* GetRefDec)( AstSpecFrame * );
   int (* TestRefDec)( AstSpecFrame * );
   void (* ClearRefDec)( AstSpecFrame * );
   void (* SetRefDec)( AstSpecFrame *, double );

   double (* GetSourceVel)( AstSpecFrame * );
   int (* TestSourceVel)( AstSpecFrame * );
   void (* ClearSourceVel)( AstSpecFrame * );
   void (* SetSourceVel)( AstSpecFrame *, double );

} AstSpecFrameVtab;
#endif

/* Function prototypes. */
/* ==================== */
/* Prototypes for standard class functions. */
/* ---------------------------------------- */
astPROTO_CHECK(SpecFrame)         /* Check class membership */
astPROTO_ISA(SpecFrame)           /* Test class membership */

/* Constructor. */
#if defined(astCLASS)            /* Protected */
AstSpecFrame *astSpecFrame_( const char *, ... );
#else
AstSpecFrame *astSpecFrameId_( const char *, ... );
#endif

#if defined(astCLASS)            /* Protected */

/* Initialiser. */
AstSpecFrame *astInitSpecFrame_( void *, size_t, int, 
                                         AstSpecFrameVtab *,
                                         const char * );

/* Vtab initialiser. */
void astInitSpecFrameVtab_( AstSpecFrameVtab *, const char * );

/* Loader. */
AstSpecFrame *astLoadSpecFrame_( void *, size_t, 
                                         AstSpecFrameVtab *,
                                         const char *, AstChannel *channel );
#endif

/* Prototypes for member functions. */
/* -------------------------------- */
void astGetRefPos_( AstSpecFrame *, AstSkyFrame *, double *, double * );
void astSetRefPos_( AstSpecFrame *, AstSkyFrame *, double, double );

#if defined(astCLASS)            /* Protected */

AstStdOfRestType astGetStdOfRest_( AstSpecFrame * );
int astTestStdOfRest_( AstSpecFrame * );
void astClearStdOfRest_( AstSpecFrame * );
void astSetStdOfRest_( AstSpecFrame *, AstStdOfRestType );

AstStdOfRestType astGetAlignStdOfRest_( AstSpecFrame * );
int astTestAlignStdOfRest_( AstSpecFrame * );
void astClearAlignStdOfRest_( AstSpecFrame * );
void astSetAlignStdOfRest_( AstSpecFrame *, AstStdOfRestType );

AstStdOfRestType astGetSourceVRF_( AstSpecFrame * );
int astTestSourceVRF_( AstSpecFrame * );
void astClearSourceVRF_( AstSpecFrame * );
void astSetSourceVRF_( AstSpecFrame *, AstStdOfRestType );

double astGetRestFreq_( AstSpecFrame * );
int astTestRestFreq_( AstSpecFrame * );
void astClearRestFreq_( AstSpecFrame * );
void astSetRestFreq_( AstSpecFrame *, double );

double astGetRefRA_( AstSpecFrame * );
int astTestRefRA_( AstSpecFrame * );
void astClearRefRA_( AstSpecFrame * );
void astSetRefRA_( AstSpecFrame *, double );

double astGetRefDec_( AstSpecFrame * );
int astTestRefDec_( AstSpecFrame * );
void astClearRefDec_( AstSpecFrame * );
void astSetRefDec_( AstSpecFrame *, double );

double astGetSourceVel_( AstSpecFrame * );
int astTestSourceVel_( AstSpecFrame * );
void astClearSourceVel_( AstSpecFrame * );
void astSetSourceVel_( AstSpecFrame *, double );

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
#define astCheckSpecFrame(this) astINVOKE_CHECK(SpecFrame,this)

/* Test class membership. */
#define astIsASpecFrame(this) astINVOKE_ISA(SpecFrame,this)

/* Constructor. */
#if defined(astCLASS)            /* Protected */
#define astSpecFrame astINVOKE(F,astSpecFrame_)
#else
#define astSpecFrame astINVOKE(F,astSpecFrameId_)
#endif

#if defined(astCLASS)            /* Protected */

/* Initialiser. */
#define astInitSpecFrame(mem,size,init,vtab,name) \
astINVOKE(O,astInitSpecFrame_(mem,size,init,vtab,name))

/* Vtab Initialiser. */
#define astInitSpecFrameVtab(vtab,name) astINVOKE(V,astInitSpecFrameVtab_(vtab,name))
/* Loader. */
#define astLoadSpecFrame(mem,size,vtab,name,channel) \
astINVOKE(O,astLoadSpecFrame_(mem,size,vtab,name,astCheckChannel(channel)))

#endif

/* Interfaces to public member functions. */
/* -------------------------------------- */

/* None. */

/* Interfaces to protected member functions. */
/* ----------------------------------------- */
/* Here we make use of astCheckSpecFrame to validate SpecFrame pointers
   before use. This provides a contextual error report if a pointer to
   the wrong sort of object is supplied. */

#define astGetRefPos(this,frm,lon,lat) astINVOKE(V,astGetRefPos_(astCheckSpecFrame(this),(frm==NULL?NULL:astCheckSkyFrame(frm)),lon,lat))
#define astSetRefPos(this,frm,lon,lat) astINVOKE(V,astSetRefPos_(astCheckSpecFrame(this),(frm==NULL?NULL:astCheckSkyFrame(frm)),lon,lat))

#if defined(astCLASS)            /* Protected */

#define astGetStdOfRest(this) astINVOKE(V,astGetStdOfRest_(astCheckSpecFrame(this)))
#define astTestStdOfRest(this) astINVOKE(V,astTestStdOfRest_(astCheckSpecFrame(this)))
#define astClearStdOfRest(this) astINVOKE(V,astClearStdOfRest_(astCheckSpecFrame(this)))
#define astSetStdOfRest(this,value) astINVOKE(V,astSetStdOfRest_(astCheckSpecFrame(this),value))

#define astGetAlignStdOfRest(this) astINVOKE(V,astGetAlignStdOfRest_(astCheckSpecFrame(this)))
#define astTestAlignStdOfRest(this) astINVOKE(V,astTestAlignStdOfRest_(astCheckSpecFrame(this)))
#define astClearAlignStdOfRest(this) astINVOKE(V,astClearAlignStdOfRest_(astCheckSpecFrame(this)))
#define astSetAlignStdOfRest(this,value) astINVOKE(V,astSetAlignStdOfRest_(astCheckSpecFrame(this),value))

#define astGetSourceVRF(this) astINVOKE(V,astGetSourceVRF_(astCheckSpecFrame(this)))
#define astTestSourceVRF(this) astINVOKE(V,astTestSourceVRF_(astCheckSpecFrame(this)))
#define astClearSourceVRF(this) astINVOKE(V,astClearSourceVRF_(astCheckSpecFrame(this)))
#define astSetSourceVRF(this,value) astINVOKE(V,astSetSourceVRF_(astCheckSpecFrame(this),value))

#define astGetRestFreq(this) astINVOKE(V,astGetRestFreq_(astCheckSpecFrame(this)))
#define astTestRestFreq(this) astINVOKE(V,astTestRestFreq_(astCheckSpecFrame(this)))
#define astClearRestFreq(this) astINVOKE(V,astClearRestFreq_(astCheckSpecFrame(this)))
#define astSetRestFreq(this,value) astINVOKE(V,astSetRestFreq_(astCheckSpecFrame(this),value))

#define astGetRefRA(this) astINVOKE(V,astGetRefRA_(astCheckSpecFrame(this)))
#define astTestRefRA(this) astINVOKE(V,astTestRefRA_(astCheckSpecFrame(this)))
#define astClearRefRA(this) astINVOKE(V,astClearRefRA_(astCheckSpecFrame(this)))
#define astSetRefRA(this,value) astINVOKE(V,astSetRefRA_(astCheckSpecFrame(this),value))

#define astGetRefDec(this) astINVOKE(V,astGetRefDec_(astCheckSpecFrame(this)))
#define astTestRefDec(this) astINVOKE(V,astTestRefDec_(astCheckSpecFrame(this)))
#define astClearRefDec(this) astINVOKE(V,astClearRefDec_(astCheckSpecFrame(this)))
#define astSetRefDec(this,value) astINVOKE(V,astSetRefDec_(astCheckSpecFrame(this),value))

#define astGetSourceVel(this) astINVOKE(V,astGetSourceVel_(astCheckSpecFrame(this)))
#define astTestSourceVel(this) astINVOKE(V,astTestSourceVel_(astCheckSpecFrame(this)))
#define astClearSourceVel(this) astINVOKE(V,astClearSourceVel_(astCheckSpecFrame(this)))
#define astSetSourceVel(this,value) astINVOKE(V,astSetSourceVel_(astCheckSpecFrame(this),value))

#endif
#endif
