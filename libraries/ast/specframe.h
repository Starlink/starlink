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
*     12-NOV-2002 (DSB):
*        Original version.
*     18-OCT-2006 (DSB):
*        Added SpecOrigin.
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
   AstSystemType sourcesys;      /* Code identifying source vel. system */
   int nuunits;                  /* Size of usedunits array */
   char **usedunits;             /* Last used units for each system */
   double specorigin;            /* Origin for sectral values */
   int alignspecoffset;          /* Align SpecFrame in offset coords? */
} AstSpecFrame;

/* Virtual function table. */
/* ----------------------- */
/* This table contains all information that is the same for all objects in the
   class (e.g. pointers to its virtual functions). */
#if defined(astCLASS)            /* Protected */
typedef struct AstSpecFrameVtab {

/* Properties (e.g. methods) inherited from the parent class. */
   AstFrameVtab frame_vtab;      /* Parent class virtual function table */

/* A Unique identifier to determine class membership. */
   AstClassIdentifier id;

/* Properties (e.g. methods) specific to this class. */
   void (* GetRefPos)( AstSpecFrame *, AstSkyFrame *, double *, double *, int * );
   void (* SetRefPos)( AstSpecFrame *, AstSkyFrame *, double, double, int * );

   AstStdOfRestType (* GetStdOfRest)( AstSpecFrame *, int * );
   int (* TestStdOfRest)( AstSpecFrame *, int * );
   void (* ClearStdOfRest)( AstSpecFrame *, int * );
   void (* SetStdOfRest)( AstSpecFrame *, AstStdOfRestType, int * );

   AstStdOfRestType (* GetAlignStdOfRest)( AstSpecFrame *, int * );
   int (* TestAlignStdOfRest)( AstSpecFrame *, int * );
   void (* ClearAlignStdOfRest)( AstSpecFrame *, int * );
   void (* SetAlignStdOfRest)( AstSpecFrame *, AstStdOfRestType, int * );

   AstStdOfRestType (* GetSourceVRF)( AstSpecFrame *, int * );
   int (* TestSourceVRF)( AstSpecFrame *, int * );
   void (* ClearSourceVRF)( AstSpecFrame *, int * );
   void (* SetSourceVRF)( AstSpecFrame *, AstStdOfRestType, int * );

   AstSystemType (* GetSourceSys)( AstSpecFrame *, int * );
   int (* TestSourceSys)( AstSpecFrame *, int * );
   void (* ClearSourceSys)( AstSpecFrame *, int * );
   void (* SetSourceSys)( AstSpecFrame *, AstSystemType, int * );

   double (* GetRestFreq)( AstSpecFrame *, int * );
   int (* TestRestFreq)( AstSpecFrame *, int * );
   void (* ClearRestFreq)( AstSpecFrame *, int * );
   void (* SetRestFreq)( AstSpecFrame *, double, int * );

   double (* GetRefRA)( AstSpecFrame *, int * );
   int (* TestRefRA)( AstSpecFrame *, int * );
   void (* ClearRefRA)( AstSpecFrame *, int * );
   void (* SetRefRA)( AstSpecFrame *, double, int * );

   double (* GetRefDec)( AstSpecFrame *, int * );
   int (* TestRefDec)( AstSpecFrame *, int * );
   void (* ClearRefDec)( AstSpecFrame *, int * );
   void (* SetRefDec)( AstSpecFrame *, double, int * );

   double (* GetSourceVel)( AstSpecFrame *, int * );
   int (* TestSourceVel)( AstSpecFrame *, int * );
   void (* ClearSourceVel)( AstSpecFrame *, int * );
   void (* SetSourceVel)( AstSpecFrame *, double, int * );

   double (* GetSpecOrigin)( AstSpecFrame *, int * );
   int (* TestSpecOrigin)( AstSpecFrame *, int * );
   void (* ClearSpecOrigin)( AstSpecFrame *, int * );
   void (* SetSpecOrigin)( AstSpecFrame *, double, int * );

   int (* GetAlignSpecOffset)( AstSpecFrame *, int * );
   int (* TestAlignSpecOffset)( AstSpecFrame *, int * );
   void (* ClearAlignSpecOffset)( AstSpecFrame *, int * );
   void (* SetAlignSpecOffset)( AstSpecFrame *, int, int * );


} AstSpecFrameVtab;


#if defined(THREAD_SAFE)

/* Define a structure holding all data items that are global within this
   class. */
typedef struct AstSpecFrameGlobals {
   AstSpecFrameVtab Class_Vtab;
   int Class_Init;
   char GetAttrib_Buff[ 51 ];
   char GetLabel_Buff[ 201 ];
   char GetSymbol_Buff[ 21 ];
   char GetTitle_Buff[ 201 ];
} AstSpecFrameGlobals;

#endif

#endif

/* Function prototypes. */
/* ==================== */
/* Prototypes for standard class functions. */
/* ---------------------------------------- */
astPROTO_CHECK(SpecFrame)         /* Check class membership */
astPROTO_ISA(SpecFrame)           /* Test class membership */

/* Constructor. */
#if defined(astCLASS)            /* Protected */
AstSpecFrame *astSpecFrame_( const char *, int *, ...);
#else
AstSpecFrame *astSpecFrameId_( const char *, ... )__attribute__((format(printf,1,2)));
#endif

#if defined(astCLASS)            /* Protected */

/* Initialiser. */
AstSpecFrame *astInitSpecFrame_( void *, size_t, int,
                                         AstSpecFrameVtab *,
                                         const char *, int * );

/* Vtab initialiser. */
void astInitSpecFrameVtab_( AstSpecFrameVtab *, const char *, int * );

/* Loader. */
AstSpecFrame *astLoadSpecFrame_( void *, size_t,
                                         AstSpecFrameVtab *,
                                         const char *, AstChannel *channel, int * );

/* Thread-safe initialiser for all global data used by this module. */
#if defined(THREAD_SAFE)
void astInitSpecFrameGlobals_( AstSpecFrameGlobals * );
#endif

#endif

/* Prototypes for member functions. */
/* -------------------------------- */
void astGetRefPos_( AstSpecFrame *, AstSkyFrame *, double *, double *, int * );
void astSetRefPos_( AstSpecFrame *, AstSkyFrame *, double, double, int * );

#if defined(astCLASS)            /* Protected */

AstStdOfRestType astGetStdOfRest_( AstSpecFrame *, int * );
int astTestStdOfRest_( AstSpecFrame *, int * );
void astClearStdOfRest_( AstSpecFrame *, int * );
void astSetStdOfRest_( AstSpecFrame *, AstStdOfRestType, int * );

AstStdOfRestType astGetAlignStdOfRest_( AstSpecFrame *, int * );
int astTestAlignStdOfRest_( AstSpecFrame *, int * );
void astClearAlignStdOfRest_( AstSpecFrame *, int * );
void astSetAlignStdOfRest_( AstSpecFrame *, AstStdOfRestType, int * );

AstStdOfRestType astGetSourceVRF_( AstSpecFrame *, int * );
int astTestSourceVRF_( AstSpecFrame *, int * );
void astClearSourceVRF_( AstSpecFrame *, int * );
void astSetSourceVRF_( AstSpecFrame *, AstStdOfRestType, int * );

AstSystemType astGetSourceSys_( AstSpecFrame *, int * );
int astTestSourceSys_( AstSpecFrame *, int * );
void astClearSourceSys_( AstSpecFrame *, int * );
void astSetSourceSys_( AstSpecFrame *, AstSystemType, int * );

double astGetRestFreq_( AstSpecFrame *, int * );
int astTestRestFreq_( AstSpecFrame *, int * );
void astClearRestFreq_( AstSpecFrame *, int * );
void astSetRestFreq_( AstSpecFrame *, double, int * );

double astGetRefRA_( AstSpecFrame *, int * );
int astTestRefRA_( AstSpecFrame *, int * );
void astClearRefRA_( AstSpecFrame *, int * );
void astSetRefRA_( AstSpecFrame *, double, int * );

double astGetRefDec_( AstSpecFrame *, int * );
int astTestRefDec_( AstSpecFrame *, int * );
void astClearRefDec_( AstSpecFrame *, int * );
void astSetRefDec_( AstSpecFrame *, double, int * );

double astGetSourceVel_( AstSpecFrame *, int * );
int astTestSourceVel_( AstSpecFrame *, int * );
void astClearSourceVel_( AstSpecFrame *, int * );
void astSetSourceVel_( AstSpecFrame *, double, int * );

double astGetSpecOrigin_( AstSpecFrame *, int * );
int astTestSpecOrigin_( AstSpecFrame *, int * );
void astClearSpecOrigin_( AstSpecFrame *, int * );
void astSetSpecOrigin_( AstSpecFrame *, double, int * );

int astGetAlignSpecOffset_( AstSpecFrame *, int * );
int astTestAlignSpecOffset_( AstSpecFrame *, int * );
void astClearAlignSpecOffset_( AstSpecFrame *, int * );
void astSetAlignSpecOffset_( AstSpecFrame *, int, int * );

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
#define astCheckSpecFrame(this) astINVOKE_CHECK(SpecFrame,this,0)
#define astVerifySpecFrame(this) astINVOKE_CHECK(SpecFrame,this,1)

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
astINVOKE(O,astInitSpecFrame_(mem,size,init,vtab,name,STATUS_PTR))

/* Vtab Initialiser. */
#define astInitSpecFrameVtab(vtab,name) astINVOKE(V,astInitSpecFrameVtab_(vtab,name,STATUS_PTR))
/* Loader. */
#define astLoadSpecFrame(mem,size,vtab,name,channel) \
astINVOKE(O,astLoadSpecFrame_(mem,size,vtab,name,astCheckChannel(channel),STATUS_PTR))

#endif

/* Interfaces to public member functions. */
/* -------------------------------------- */

/* None. */

/* Interfaces to protected member functions. */
/* ----------------------------------------- */
/* Here we make use of astCheckSpecFrame to validate SpecFrame pointers
   before use. This provides a contextual error report if a pointer to
   the wrong sort of object is supplied. */

#define astGetRefPos(this,frm,lon,lat) astINVOKE(V,astGetRefPos_(astCheckSpecFrame(this),(frm==NULL?NULL:astCheckSkyFrame(frm)),lon,lat,STATUS_PTR))
#define astSetRefPos(this,frm,lon,lat) astINVOKE(V,astSetRefPos_(astCheckSpecFrame(this),(frm==NULL?NULL:astCheckSkyFrame(frm)),lon,lat,STATUS_PTR))

#if defined(astCLASS)            /* Protected */

#define astGetStdOfRest(this) astINVOKE(V,astGetStdOfRest_(astCheckSpecFrame(this),STATUS_PTR))
#define astTestStdOfRest(this) astINVOKE(V,astTestStdOfRest_(astCheckSpecFrame(this),STATUS_PTR))
#define astClearStdOfRest(this) astINVOKE(V,astClearStdOfRest_(astCheckSpecFrame(this),STATUS_PTR))
#define astSetStdOfRest(this,value) astINVOKE(V,astSetStdOfRest_(astCheckSpecFrame(this),value,STATUS_PTR))

#define astGetAlignStdOfRest(this) astINVOKE(V,astGetAlignStdOfRest_(astCheckSpecFrame(this),STATUS_PTR))
#define astTestAlignStdOfRest(this) astINVOKE(V,astTestAlignStdOfRest_(astCheckSpecFrame(this),STATUS_PTR))
#define astClearAlignStdOfRest(this) astINVOKE(V,astClearAlignStdOfRest_(astCheckSpecFrame(this),STATUS_PTR))
#define astSetAlignStdOfRest(this,value) astINVOKE(V,astSetAlignStdOfRest_(astCheckSpecFrame(this),value,STATUS_PTR))

#define astGetSourceVRF(this) astINVOKE(V,astGetSourceVRF_(astCheckSpecFrame(this),STATUS_PTR))
#define astTestSourceVRF(this) astINVOKE(V,astTestSourceVRF_(astCheckSpecFrame(this),STATUS_PTR))
#define astClearSourceVRF(this) astINVOKE(V,astClearSourceVRF_(astCheckSpecFrame(this),STATUS_PTR))
#define astSetSourceVRF(this,value) astINVOKE(V,astSetSourceVRF_(astCheckSpecFrame(this),value,STATUS_PTR))

#define astGetSourceSys(this) astINVOKE(V,astGetSourceSys_(astCheckSpecFrame(this),STATUS_PTR))
#define astTestSourceSys(this) astINVOKE(V,astTestSourceSys_(astCheckSpecFrame(this),STATUS_PTR))
#define astClearSourceSys(this) astINVOKE(V,astClearSourceSys_(astCheckSpecFrame(this),STATUS_PTR))
#define astSetSourceSys(this,value) astINVOKE(V,astSetSourceSys_(astCheckSpecFrame(this),value,STATUS_PTR))

#define astGetRestFreq(this) astINVOKE(V,astGetRestFreq_(astCheckSpecFrame(this),STATUS_PTR))
#define astTestRestFreq(this) astINVOKE(V,astTestRestFreq_(astCheckSpecFrame(this),STATUS_PTR))
#define astClearRestFreq(this) astINVOKE(V,astClearRestFreq_(astCheckSpecFrame(this),STATUS_PTR))
#define astSetRestFreq(this,value) astINVOKE(V,astSetRestFreq_(astCheckSpecFrame(this),value,STATUS_PTR))

#define astGetRefRA(this) astINVOKE(V,astGetRefRA_(astCheckSpecFrame(this),STATUS_PTR))
#define astTestRefRA(this) astINVOKE(V,astTestRefRA_(astCheckSpecFrame(this),STATUS_PTR))
#define astClearRefRA(this) astINVOKE(V,astClearRefRA_(astCheckSpecFrame(this),STATUS_PTR))
#define astSetRefRA(this,value) astINVOKE(V,astSetRefRA_(astCheckSpecFrame(this),value,STATUS_PTR))

#define astGetRefDec(this) astINVOKE(V,astGetRefDec_(astCheckSpecFrame(this),STATUS_PTR))
#define astTestRefDec(this) astINVOKE(V,astTestRefDec_(astCheckSpecFrame(this),STATUS_PTR))
#define astClearRefDec(this) astINVOKE(V,astClearRefDec_(astCheckSpecFrame(this),STATUS_PTR))
#define astSetRefDec(this,value) astINVOKE(V,astSetRefDec_(astCheckSpecFrame(this),value,STATUS_PTR))

#define astGetSourceVel(this) astINVOKE(V,astGetSourceVel_(astCheckSpecFrame(this),STATUS_PTR))
#define astTestSourceVel(this) astINVOKE(V,astTestSourceVel_(astCheckSpecFrame(this),STATUS_PTR))
#define astClearSourceVel(this) astINVOKE(V,astClearSourceVel_(astCheckSpecFrame(this),STATUS_PTR))
#define astSetSourceVel(this,value) astINVOKE(V,astSetSourceVel_(astCheckSpecFrame(this),value,STATUS_PTR))

#define astGetSpecOrigin(this) astINVOKE(V,astGetSpecOrigin_(astCheckSpecFrame(this),STATUS_PTR))
#define astTestSpecOrigin(this) astINVOKE(V,astTestSpecOrigin_(astCheckSpecFrame(this),STATUS_PTR))
#define astClearSpecOrigin(this) astINVOKE(V,astClearSpecOrigin_(astCheckSpecFrame(this),STATUS_PTR))
#define astSetSpecOrigin(this,value) astINVOKE(V,astSetSpecOrigin_(astCheckSpecFrame(this),value,STATUS_PTR))

#define astClearAlignSpecOffset(this) astINVOKE(V,astClearAlignSpecOffset_(astCheckSpecFrame(this),STATUS_PTR))
#define astGetAlignSpecOffset(this) astINVOKE(V,astGetAlignSpecOffset_(astCheckSpecFrame(this),STATUS_PTR))
#define astSetAlignSpecOffset(this,value) astINVOKE(V,astSetAlignSpecOffset_(astCheckSpecFrame(this),value,STATUS_PTR))
#define astTestAlignSpecOffset(this) astINVOKE(V,astTestAlignSpecOffset_(astCheckSpecFrame(this),STATUS_PTR))



#endif
#endif





