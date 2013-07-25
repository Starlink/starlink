#if !defined( SKYFRAME_INCLUDED ) /* Include this file only once */
#define SKYFRAME_INCLUDED
/*
*+
*  Name:
*     skyframe.h

*  Type:
*     C include file.

*  Purpose:
*     Define the interface to the SkyFrame class.

*  Invocation:
*     #include "skyframe.h"

*  Description:
*     This include file defines the interface to the SkyFrame class
*     and provides the type definitions, function prototypes and
*     macros, etc. needed to use this class.

*  Inheritance:
*     The SkyFrame class inherits from the Frame class.

*  Macros:
*     None.

*  Type Definitions:
*     Public:
*        AstSkyFrame
*           SkyFrame object type.

*     Protected:
*        AstSkyFrameVtab
*           SkyFrame virtual function table type.

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
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     RFWS: R.F. Warren-Smith (Starlink)
*     DSB: David S. Berry (Starlink)

*  History:
*     4-MAR-1996 (RFWS):
*        Original version.
*     24-MAY-1996 (RFWS):
*        Tidied up, etc.
*     24-SEP-1996 (RFWS):
*        Added I/O facilities.
*     16-JUL-1997 (RFWS):
*        Added Projection attribute.
*     26-FEB-1998 (RFWS):
*        Over-ride the astUnformat method.
*     3-APR-2001 (DSB):
*        Added "Unknown" option for the System attribute. Added read-only
*        attributes LatAxis and LonAxis.
*     10-OCT-2002 (DSB):
*        Moved definitions of macros for SkyFrame system values into
*        this file from skyframe.c.
*     15-NOV-2002 (DSB):
*        Move the System attribute from this class to the parent (Frame)
*        class.
*     8-JAN-2003 (DSB):
*        Added protected astInitSkyFrameVtab method.
*     19-APR-2004 (DSB):
*        Added SkyRef, SkyRefIs, SkyRefP and AlignOffset attributes.
*        Simplified prologue.
*     2-DEC-2004 (DSB):
*        Added System "J2000"
*     22-FEB-2006 (DSB):
*        Added Local Apparent Sidereal Time to the SkyFrame structure.
*     3-OCT-2006 (DSB):
*        Added Equation of Equinoxes to the SkyFrame structure.
*     6-OCT-2006 (DSB):
*        Removed Equation of Equinoxes from the SkyFrame structure.
*        Added dut1 to the SkyFrame structure.
*        Added Dut1 accessor methods.
*     14-OCT-2006 (DSB):
*        Moved dut1 to the Frame class.
*-
*/

/* Include files. */
/* ============== */
/* Interface definitions. */
/* ---------------------- */
#include "object.h"              /* Base Object class */
#include "frame.h"               /* Parent Frame class */

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

/* Define values for the different values of the SkyRefIs attribute. */
#define AST__BAD_REF 	 0
#define AST__POLE_REF    1
#define AST__ORIGIN_REF  2
#define AST__IGNORED_REF 3

/* Values used to represent different System attribute values. */
#define AST__FK4           1
#define AST__FK4_NO_E      2
#define AST__FK5           3
#define AST__GAPPT         4
#define AST__ECLIPTIC      5
#define AST__GALACTIC      6
#define AST__SUPERGALACTIC 7
#define AST__ICRS          8
#define AST__HELIOECLIPTIC 9
#define AST__J2000         10
#define AST__UNKNOWN       11
#define AST__AZEL          12

/* Define constants used to size global arrays in this module. */
/* Define other numerical constants for use in this module. */
#define AST__SKYFRAME_GETATTRIB_BUFF_LEN 200
#define AST__SKYFRAME_GETFORMAT_BUFF_LEN 50
#define AST__SKYFRAME_GETLABEL_BUFF_LEN 40
#define AST__SKYFRAME_GETSYMBOL_BUFF_LEN 20
#define AST__SKYFRAME_GETTITLE_BUFF_LEN 200

#endif

/* Type Definitions. */
/* ================= */

/* Cached LAST look-up table. */
/* -------------------------- */
/* Holds a list of epoch values and the corresponding Local Apparent
   Sidereal Time values. Also holds the observatory position and DUT1
   value used when calculating the LAST values. */
typedef struct AstSkyLastTable {
   double obslat;         /* ObsLat at which LAST values were calculated */
   double obslon;         /* ObsLon at which LAST values were calculated */
   double obsalt;         /* ObsAlt at which LAST values were calculated */
   double dut1;           /* Dut1 values at which LAST values were calculated */
   int nentry;            /* Number of entries in the epoch and last arrays */
   double *epoch;         /* Array of epoch values */
   double *last;          /* Array of LAST values */
} AstSkyLastTable;

/* SkyFrame structure. */
/* ------------------- */
/* This structure contains all information that is unique to each object in
   the class (e.g. its instance variables). */
typedef struct AstSkyFrame {

/* Attributes inherited from the parent class. */
   AstFrame frame;               /* Parent class structure */

/* Attributes specific to objects in this class. */
   char *projection;             /* Description of sky projection */
   double equinox;               /* Modified Julian Date of mean equinox */
   int neglon;                   /* Display negative longitude values? */
   int alignoffset;              /* Align SkyFrame in offset coords? */
   int skyrefis;                 /* Nature of offset coord system */
   double skyref[ 2 ];           /* Origin or pole of offset coord system */
   double skyrefp[ 2 ];          /* Point on primary meridian of offset coord system */
   double last;                  /* Local Apparent Sidereal Time */
   double eplast;                /* Epoch used to calculate "last" */
   double klast;                 /* Ratio of solar to sidereal time */
   double diurab;                /* Magnitude of diurnal aberration vector */
} AstSkyFrame;

/* Virtual function table. */
/* ----------------------- */
/* This table contains all information that is the same for all objects in the
   class (e.g. pointers to its virtual functions). */
#if defined(astCLASS)            /* Protected */
typedef struct AstSkyFrameVtab {

/* Properties (e.g. methods) inherited from the parent class. */
   AstFrameVtab frame_vtab;      /* Parent class virtual function table */

/* A Unique identifier to determine class membership. */
   AstClassIdentifier id;

/* Properties (e.g. methods) specific to this class. */
   AstMapping *(* SkyOffsetMap)( AstSkyFrame *, int * );
   const char *(* GetProjection)( AstSkyFrame *, int * );
   double (* GetEquinox)( AstSkyFrame *, int * );
   int (* GetNegLon)( AstSkyFrame *, int * );
   int (* GetAsTime)( AstSkyFrame *, int, int * );
   int (* GetIsLatAxis)( AstSkyFrame *, int, int * );
   int (* GetIsLonAxis)( AstSkyFrame *, int, int * );
   int (* GetLatAxis)( AstSkyFrame *, int * );
   int (* GetLonAxis)( AstSkyFrame *, int * );
   int (* TestAsTime)( AstSkyFrame *, int, int * );
   int (* TestEquinox)( AstSkyFrame *, int * );
   int (* TestNegLon)( AstSkyFrame *, int * );
   int (* TestProjection)( AstSkyFrame *, int * );
   void (* ClearAsTime)( AstSkyFrame *, int, int * );
   void (* ClearEquinox)( AstSkyFrame *, int * );
   void (* ClearNegLon)( AstSkyFrame *, int * );
   void (* ClearProjection)( AstSkyFrame *, int * );
   void (* SetAsTime)( AstSkyFrame *, int, int, int * );
   void (* SetEquinox)( AstSkyFrame *, double, int * );
   void (* SetNegLon)( AstSkyFrame *, int, int * );
   void (* SetProjection)( AstSkyFrame *, const char *, int * );

   int (* GetSkyRefIs)( AstSkyFrame *, int * );
   int (* TestSkyRefIs)( AstSkyFrame *, int * );
   void (* ClearSkyRefIs)( AstSkyFrame *, int * );
   void (* SetSkyRefIs)( AstSkyFrame *, int, int * );

   double (* GetSkyRef)( AstSkyFrame *, int, int * );
   int (* TestSkyRef)( AstSkyFrame *, int, int * );
   void (* ClearSkyRef)( AstSkyFrame *, int, int * );
   void (* SetSkyRef)( AstSkyFrame *, int, double, int * );

   double (* GetSkyRefP)( AstSkyFrame *, int, int * );
   int (* TestSkyRefP)( AstSkyFrame *, int, int * );
   void (* ClearSkyRefP)( AstSkyFrame *, int, int * );
   void (* SetSkyRefP)( AstSkyFrame *, int, double, int * );

   int (* GetAlignOffset)( AstSkyFrame *, int * );
   int (* TestAlignOffset)( AstSkyFrame *, int * );
   void (* ClearAlignOffset)( AstSkyFrame *, int * );
   void (* SetAlignOffset)( AstSkyFrame *, int, int * );

} AstSkyFrameVtab;

#if defined(THREAD_SAFE)

/* The AstSkyFrameGlobals structure makes a forward reference to the
   AstTimeFrame structure which is not defined here (since the
   timeframe.h file includes skyframe.h). Hence make a preliminary
   definition available now. */
struct AstTimeFrame;

/* Define a structure holding all data items that are global within this
   class. */
typedef struct AstSkyFrameGlobals {
   AstSkyFrameVtab Class_Vtab;
   int Class_Init;
   char GetAttrib_Buff[ AST__SKYFRAME_GETATTRIB_BUFF_LEN + 1 ];
   char GetFormat_Buff[ AST__SKYFRAME_GETFORMAT_BUFF_LEN + 1 ];
   char GetLabel_Buff[ AST__SKYFRAME_GETLABEL_BUFF_LEN + 1 ];
   char GetSymbol_Buff[ AST__SKYFRAME_GETSYMBOL_BUFF_LEN + 1 ];
   char GetTitle_Buff[ AST__SKYFRAME_GETTITLE_BUFF_LEN + 1 ];
   char GetTitle_Buff2[ AST__SKYFRAME_GETTITLE_BUFF_LEN + 1 ];
   struct AstTimeFrame *TDBFrame;
   struct AstTimeFrame *LASTFrame;
} AstSkyFrameGlobals;

#endif

#endif

/* Function prototypes. */
/* ==================== */
/* Prototypes for standard class functions. */
/* ---------------------------------------- */
astPROTO_CHECK(SkyFrame)         /* Check class membership */
astPROTO_ISA(SkyFrame)           /* Test class membership */

/* Constructor. */
#if defined(astCLASS)            /* Protected */
AstSkyFrame *astSkyFrame_( const char *, int *, ...);
#else
AstSkyFrame *astSkyFrameId_( const char *, ... )__attribute__((format(printf,1,2)));
#endif

#if defined(astCLASS)            /* Protected */

/* Initialiser. */
AstSkyFrame *astInitSkyFrame_( void *, size_t, int, AstSkyFrameVtab *,
                               const char *, int * );

/* Vtab initialiser. */
void astInitSkyFrameVtab_( AstSkyFrameVtab *, const char *, int * );

/* Loader. */
AstSkyFrame *astLoadSkyFrame_( void *, size_t, AstSkyFrameVtab *,
                               const char *, AstChannel *channel, int * );

/* Thread-safe initialiser for all global data used by this module. */
#if defined(THREAD_SAFE)
void astInitSkyFrameGlobals_( AstSkyFrameGlobals * );
#endif
#endif

/* Prototypes for member functions. */
/* -------------------------------- */
AstMapping *astSkyOffsetMap_( AstSkyFrame *, int * );

#if defined(astCLASS)            /* Protected */
const char *astGetProjection_( AstSkyFrame *, int * );
double astGetEquinox_( AstSkyFrame *, int * );
int astGetNegLon_( AstSkyFrame *, int * );
int astGetAsTime_( AstSkyFrame *, int, int * );
int astGetIsLatAxis_( AstSkyFrame *, int, int * );
int astGetIsLonAxis_( AstSkyFrame *, int, int * );
int astGetLatAxis_( AstSkyFrame *, int * );
int astGetLonAxis_( AstSkyFrame *, int * );
int astTestAsTime_( AstSkyFrame *, int, int * );
int astTestEquinox_( AstSkyFrame *, int * );
int astTestNegLon_( AstSkyFrame *, int * );
int astTestProjection_( AstSkyFrame *, int * );
void astClearAsTime_( AstSkyFrame *, int, int * );
void astClearEquinox_( AstSkyFrame *, int * );
void astClearNegLon_( AstSkyFrame *, int * );
void astClearProjection_( AstSkyFrame *, int * );
void astSetAsTime_( AstSkyFrame *, int, int, int * );
void astSetEquinox_( AstSkyFrame *, double, int * );
void astSetNegLon_( AstSkyFrame *, int, int * );
void astSetProjection_( AstSkyFrame *, const char *, int * );

int astGetAlignOffset_( AstSkyFrame *, int * );
int astTestAlignOffset_( AstSkyFrame *, int * );
void astClearAlignOffset_( AstSkyFrame *, int * );
void astSetAlignOffset_( AstSkyFrame *, int, int * );

int astGetSkyRefIs_( AstSkyFrame *, int * );
int astTestSkyRefIs_( AstSkyFrame *, int * );
void astClearSkyRefIs_( AstSkyFrame *, int * );
void astSetSkyRefIs_( AstSkyFrame *, int, int * );

double astGetSkyRef_( AstSkyFrame *, int, int * );
int astTestSkyRef_( AstSkyFrame *, int, int * );
void astClearSkyRef_( AstSkyFrame *, int, int * );
void astSetSkyRef_( AstSkyFrame *, int, double, int * );

double astGetSkyRefP_( AstSkyFrame *, int, int * );
int astTestSkyRefP_( AstSkyFrame *, int, int * );
void astClearSkyRefP_( AstSkyFrame *, int, int * );
void astSetSkyRefP_( AstSkyFrame *, int, double, int * );


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
#define astCheckSkyFrame(this) astINVOKE_CHECK(SkyFrame,this,0)
#define astVerifySkyFrame(this) astINVOKE_CHECK(SkyFrame,this,1)

/* Test class membership. */
#define astIsASkyFrame(this) astINVOKE_ISA(SkyFrame,this)

/* Constructor. */
#if defined(astCLASS)            /* Protected */
#define astSkyFrame astINVOKE(F,astSkyFrame_)
#else
#define astSkyFrame astINVOKE(F,astSkyFrameId_)
#endif

#if defined(astCLASS)            /* Protected */

/* Initialiser. */
#define astInitSkyFrame(mem,size,init,vtab,name) \
astINVOKE(O,astInitSkyFrame_(mem,size,init,vtab,name,STATUS_PTR))

/* Vtab Initialiser. */
#define astInitSkyFrameVtab(vtab,name) astINVOKE(V,astInitSkyFrameVtab_(vtab,name,STATUS_PTR))
/* Loader. */
#define astLoadSkyFrame(mem,size,vtab,name,channel) \
astINVOKE(O,astLoadSkyFrame_(mem,size,vtab,name,astCheckChannel(channel),STATUS_PTR))

#endif

/* Interfaces to public member functions. */
/* -------------------------------------- */

#define astSkyOffsetMap(this) \
astINVOKE(O,astSkyOffsetMap_(astCheckSkyFrame(this),STATUS_PTR))

/* Interfaces to protected member functions. */
/* ----------------------------------------- */
/* Here we make use of astCheckSkyFrame to validate SkyFrame pointers
   before use. This provides a contextual error report if a pointer to
   the wrong sort of object is supplied. */

#if defined(astCLASS)            /* Protected */
#define astClearAsTime(this,axis) \
astINVOKE(V,astClearAsTime_(astCheckSkyFrame(this),axis,STATUS_PTR))
#define astClearEquinox(this) \
astINVOKE(V,astClearEquinox_(astCheckSkyFrame(this),STATUS_PTR))
#define astClearNegLon(this) \
astINVOKE(V,astClearNegLon_(astCheckSkyFrame(this),STATUS_PTR))
#define astClearProjection(this) \
astINVOKE(V,astClearProjection_(astCheckSkyFrame(this),STATUS_PTR))
#define astGetAsTime(this,axis) \
astINVOKE(V,astGetAsTime_(astCheckSkyFrame(this),axis,STATUS_PTR))
#define astGetEquinox(this) \
astINVOKE(V,astGetEquinox_(astCheckSkyFrame(this),STATUS_PTR))
#define astGetNegLon(this) \
astINVOKE(V,astGetNegLon_(astCheckSkyFrame(this),STATUS_PTR))
#define astGetIsLatAxis(this,axis) \
astINVOKE(V,astGetIsLatAxis_(astCheckSkyFrame(this),axis,STATUS_PTR))
#define astGetIsLonAxis(this,axis) \
astINVOKE(V,astGetIsLonAxis_(astCheckSkyFrame(this),axis,STATUS_PTR))
#define astGetLatAxis(this) \
astINVOKE(V,astGetLatAxis_(astCheckSkyFrame(this),STATUS_PTR))
#define astGetLonAxis(this) \
astINVOKE(V,astGetLonAxis_(astCheckSkyFrame(this),STATUS_PTR))
#define astGetProjection(this) \
astINVOKE(V,astGetProjection_(astCheckSkyFrame(this),STATUS_PTR))
#define astSetAsTime(this,axis,value) \
astINVOKE(V,astSetAsTime_(astCheckSkyFrame(this),axis,value,STATUS_PTR))
#define astSetEquinox(this,value) \
astINVOKE(V,astSetEquinox_(astCheckSkyFrame(this),value,STATUS_PTR))
#define astSetNegLon(this,value) \
astINVOKE(V,astSetNegLon_(astCheckSkyFrame(this),value,STATUS_PTR))
#define astSetProjection(this,value) \
astINVOKE(V,astSetProjection_(astCheckSkyFrame(this),value,STATUS_PTR))
#define astTestAsTime(this,axis) \
astINVOKE(V,astTestAsTime_(astCheckSkyFrame(this),axis,STATUS_PTR))
#define astTestEquinox(this) \
astINVOKE(V,astTestEquinox_(astCheckSkyFrame(this),STATUS_PTR))
#define astTestNegLon(this) \
astINVOKE(V,astTestNegLon_(astCheckSkyFrame(this),STATUS_PTR))
#define astTestProjection(this) \
astINVOKE(V,astTestProjection_(astCheckSkyFrame(this),STATUS_PTR))

#define astClearAlignOffset(this) astINVOKE(V,astClearAlignOffset_(astCheckSkyFrame(this),STATUS_PTR))
#define astGetAlignOffset(this) astINVOKE(V,astGetAlignOffset_(astCheckSkyFrame(this),STATUS_PTR))
#define astSetAlignOffset(this,value) astINVOKE(V,astSetAlignOffset_(astCheckSkyFrame(this),value,STATUS_PTR))
#define astTestAlignOffset(this) astINVOKE(V,astTestAlignOffset_(astCheckSkyFrame(this),STATUS_PTR))

#define astClearSkyRefIs(this) astINVOKE(V,astClearSkyRefIs_(astCheckSkyFrame(this),STATUS_PTR))
#define astGetSkyRefIs(this) astINVOKE(V,astGetSkyRefIs_(astCheckSkyFrame(this),STATUS_PTR))
#define astSetSkyRefIs(this,value) astINVOKE(V,astSetSkyRefIs_(astCheckSkyFrame(this),value,STATUS_PTR))
#define astTestSkyRefIs(this) astINVOKE(V,astTestSkyRefIs_(astCheckSkyFrame(this),STATUS_PTR))

#define astClearSkyRef(this,axis) astINVOKE(V,astClearSkyRef_(astCheckSkyFrame(this),axis,STATUS_PTR))
#define astGetSkyRef(this,axis) astINVOKE(V,astGetSkyRef_(astCheckSkyFrame(this),axis,STATUS_PTR))
#define astSetSkyRef(this,axis,value) astINVOKE(V,astSetSkyRef_(astCheckSkyFrame(this),axis,value,STATUS_PTR))
#define astTestSkyRef(this,axis) astINVOKE(V,astTestSkyRef_(astCheckSkyFrame(this),axis,STATUS_PTR))

#define astClearSkyRefP(this,axis) astINVOKE(V,astClearSkyRefP_(astCheckSkyFrame(this),axis,STATUS_PTR))
#define astGetSkyRefP(this,axis) astINVOKE(V,astGetSkyRefP_(astCheckSkyFrame(this),axis,STATUS_PTR))
#define astSetSkyRefP(this,axis,value) astINVOKE(V,astSetSkyRefP_(astCheckSkyFrame(this),axis,value,STATUS_PTR))
#define astTestSkyRefP(this,axis) astINVOKE(V,astTestSkyRefP_(astCheckSkyFrame(this),axis,STATUS_PTR))

#endif
#endif





