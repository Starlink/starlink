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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

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

#if defined(astCLASS)            /* Protected */

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

#endif

/* Type Definitions. */
/* ================= */

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
} AstSkyFrame;

/* Virtual function table. */
/* ----------------------- */
/* This table contains all information that is the same for all objects in the
   class (e.g. pointers to its virtual functions). */
#if defined(astCLASS)            /* Protected */
typedef struct AstSkyFrameVtab {

/* Properties (e.g. methods) inherited from the parent class. */
   AstFrameVtab frame_vtab;      /* Parent class virtual function table */

/* Unique flag value to determine class membership. */
   int *check;                   /* Check value */

/* Properties (e.g. methods) specific to this class. */
   const char *(* GetProjection)( AstSkyFrame * );
   double (* GetEquinox)( AstSkyFrame * );
   int (* GetNegLon)( AstSkyFrame * );
   int (* GetAsTime)( AstSkyFrame *, int );
   int (* GetLatAxis)( AstSkyFrame * );
   int (* GetLonAxis)( AstSkyFrame * );
   int (* TestAsTime)( AstSkyFrame *, int );
   int (* TestEquinox)( AstSkyFrame * );
   int (* TestNegLon)( AstSkyFrame * );
   int (* TestProjection)( AstSkyFrame * );
   void (* ClearAsTime)( AstSkyFrame *, int );
   void (* ClearEquinox)( AstSkyFrame * );
   void (* ClearNegLon)( AstSkyFrame * );
   void (* ClearProjection)( AstSkyFrame * );
   void (* SetAsTime)( AstSkyFrame *, int, int );
   void (* SetEquinox)( AstSkyFrame *, double );
   void (* SetNegLon)( AstSkyFrame *, int );
   void (* SetProjection)( AstSkyFrame *, const char * );

   int (* GetSkyRefIs)( AstSkyFrame * );
   int (* TestSkyRefIs)( AstSkyFrame * );
   void (* ClearSkyRefIs)( AstSkyFrame * );
   void (* SetSkyRefIs)( AstSkyFrame *, int );

   double (* GetSkyRef)( AstSkyFrame *, int );
   int (* TestSkyRef)( AstSkyFrame *, int );
   void (* ClearSkyRef)( AstSkyFrame *, int );
   void (* SetSkyRef)( AstSkyFrame *, int, double );

   double (* GetSkyRefP)( AstSkyFrame *, int );
   int (* TestSkyRefP)( AstSkyFrame *, int );
   void (* ClearSkyRefP)( AstSkyFrame *, int );
   void (* SetSkyRefP)( AstSkyFrame *, int, double );

   int (* GetAlignOffset)( AstSkyFrame * );
   int (* TestAlignOffset)( AstSkyFrame * );
   void (* ClearAlignOffset)( AstSkyFrame * );
   void (* SetAlignOffset)( AstSkyFrame *, int );


} AstSkyFrameVtab;
#endif

/* Function prototypes. */
/* ==================== */
/* Prototypes for standard class functions. */
/* ---------------------------------------- */
astPROTO_CHECK(SkyFrame)         /* Check class membership */
astPROTO_ISA(SkyFrame)           /* Test class membership */

/* Constructor. */
#if defined(astCLASS)            /* Protected */
AstSkyFrame *astSkyFrame_( const char *, ... );
#else
AstSkyFrame *astSkyFrameId_( const char *, ... );
#endif

#if defined(astCLASS)            /* Protected */

/* Initialiser. */
AstSkyFrame *astInitSkyFrame_( void *, size_t, int, AstSkyFrameVtab *,
                               const char * );

/* Vtab initialiser. */
void astInitSkyFrameVtab_( AstSkyFrameVtab *, const char * );

/* Loader. */
AstSkyFrame *astLoadSkyFrame_( void *, size_t, AstSkyFrameVtab *,
                               const char *, AstChannel *channel );
#endif

/* Prototypes for member functions. */
/* -------------------------------- */
#if defined(astCLASS)            /* Protected */
const char *astGetProjection_( AstSkyFrame * );
double astGetEquinox_( AstSkyFrame * );
int astGetNegLon_( AstSkyFrame * );
int astGetAsTime_( AstSkyFrame *, int );
int astGetLatAxis_( AstSkyFrame * );
int astGetLonAxis_( AstSkyFrame * );
int astTestAsTime_( AstSkyFrame *, int );
int astTestEquinox_( AstSkyFrame * );
int astTestNegLon_( AstSkyFrame * );
int astTestProjection_( AstSkyFrame * );
void astClearAsTime_( AstSkyFrame *, int );
void astClearEquinox_( AstSkyFrame * );
void astClearNegLon_( AstSkyFrame * );
void astClearProjection_( AstSkyFrame * );
void astSetAsTime_( AstSkyFrame *, int, int );
void astSetEquinox_( AstSkyFrame *, double );
void astSetNegLon_( AstSkyFrame *, int );
void astSetProjection_( AstSkyFrame *, const char * );

int astGetAlignOffset_( AstSkyFrame * );
int astTestAlignOffset_( AstSkyFrame * );
void astClearAlignOffset_( AstSkyFrame * );
void astSetAlignOffset_( AstSkyFrame *, int );

int astGetSkyRefIs_( AstSkyFrame * );
int astTestSkyRefIs_( AstSkyFrame * );
void astClearSkyRefIs_( AstSkyFrame * );
void astSetSkyRefIs_( AstSkyFrame *, int );

double astGetSkyRef_( AstSkyFrame *, int );
int astTestSkyRef_( AstSkyFrame *, int );
void astClearSkyRef_( AstSkyFrame *, int );
void astSetSkyRef_( AstSkyFrame *, int, double );

double astGetSkyRefP_( AstSkyFrame *, int );
int astTestSkyRefP_( AstSkyFrame *, int );
void astClearSkyRefP_( AstSkyFrame *, int );
void astSetSkyRefP_( AstSkyFrame *, int, double );

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
#define astCheckSkyFrame(this) astINVOKE_CHECK(SkyFrame,this)

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
astINVOKE(O,astInitSkyFrame_(mem,size,init,vtab,name))

/* Vtab Initialiser. */
#define astInitSkyFrameVtab(vtab,name) astINVOKE(V,astInitSkyFrameVtab_(vtab,name))
/* Loader. */
#define astLoadSkyFrame(mem,size,vtab,name,channel) \
astINVOKE(O,astLoadSkyFrame_(mem,size,vtab,name,astCheckChannel(channel)))

#endif

/* Interfaces to public member functions. */
/* -------------------------------------- */

/* None. */

/* Interfaces to protected member functions. */
/* ----------------------------------------- */
/* Here we make use of astCheckSkyFrame to validate SkyFrame pointers
   before use. This provides a contextual error report if a pointer to
   the wrong sort of object is supplied. */

#if defined(astCLASS)            /* Protected */
#define astClearAsTime(this,axis) \
astINVOKE(V,astClearAsTime_(astCheckSkyFrame(this),axis))
#define astClearEquinox(this) \
astINVOKE(V,astClearEquinox_(astCheckSkyFrame(this)))
#define astClearNegLon(this) \
astINVOKE(V,astClearNegLon_(astCheckSkyFrame(this)))
#define astClearProjection(this) \
astINVOKE(V,astClearProjection_(astCheckSkyFrame(this)))
#define astGetAsTime(this,axis) \
astINVOKE(V,astGetAsTime_(astCheckSkyFrame(this),axis))
#define astGetEquinox(this) \
astINVOKE(V,astGetEquinox_(astCheckSkyFrame(this)))
#define astGetNegLon(this) \
astINVOKE(V,astGetNegLon_(astCheckSkyFrame(this)))
#define astGetLatAxis(this) \
astINVOKE(V,astGetLatAxis_(astCheckSkyFrame(this)))
#define astGetLonAxis(this) \
astINVOKE(V,astGetLonAxis_(astCheckSkyFrame(this)))
#define astGetProjection(this) \
astINVOKE(V,astGetProjection_(astCheckSkyFrame(this)))
#define astSetAsTime(this,axis,value) \
astINVOKE(V,astSetAsTime_(astCheckSkyFrame(this),axis,value))
#define astSetEquinox(this,value) \
astINVOKE(V,astSetEquinox_(astCheckSkyFrame(this),value))
#define astSetNegLon(this,value) \
astINVOKE(V,astSetNegLon_(astCheckSkyFrame(this),value))
#define astSetProjection(this,value) \
astINVOKE(V,astSetProjection_(astCheckSkyFrame(this),value))
#define astTestAsTime(this,axis) \
astINVOKE(V,astTestAsTime_(astCheckSkyFrame(this),axis))
#define astTestEquinox(this) \
astINVOKE(V,astTestEquinox_(astCheckSkyFrame(this)))
#define astTestNegLon(this) \
astINVOKE(V,astTestNegLon_(astCheckSkyFrame(this)))
#define astTestProjection(this) \
astINVOKE(V,astTestProjection_(astCheckSkyFrame(this)))

#define astClearAlignOffset(this) astINVOKE(V,astClearAlignOffset_(astCheckSkyFrame(this)))
#define astGetAlignOffset(this) astINVOKE(V,astGetAlignOffset_(astCheckSkyFrame(this)))
#define astSetAlignOffset(this,value) astINVOKE(V,astSetAlignOffset_(astCheckSkyFrame(this),value))
#define astTestAlignOffset(this) astINVOKE(V,astTestAlignOffset_(astCheckSkyFrame(this)))

#define astClearSkyRefIs(this) astINVOKE(V,astClearSkyRefIs_(astCheckSkyFrame(this)))
#define astGetSkyRefIs(this) astINVOKE(V,astGetSkyRefIs_(astCheckSkyFrame(this)))
#define astSetSkyRefIs(this,value) astINVOKE(V,astSetSkyRefIs_(astCheckSkyFrame(this),value))
#define astTestSkyRefIs(this) astINVOKE(V,astTestSkyRefIs_(astCheckSkyFrame(this)))

#define astClearSkyRef(this,axis) astINVOKE(V,astClearSkyRef_(astCheckSkyFrame(this),axis))
#define astGetSkyRef(this,axis) astINVOKE(V,astGetSkyRef_(astCheckSkyFrame(this),axis))
#define astSetSkyRef(this,axis,value) astINVOKE(V,astSetSkyRef_(astCheckSkyFrame(this),axis,value))
#define astTestSkyRef(this,axis) astINVOKE(V,astTestSkyRef_(astCheckSkyFrame(this),axis))

#define astClearSkyRefP(this,axis) astINVOKE(V,astClearSkyRefP_(astCheckSkyFrame(this),axis))
#define astGetSkyRefP(this,axis) astINVOKE(V,astGetSkyRefP_(astCheckSkyFrame(this),axis))
#define astSetSkyRefP(this,axis,value) astINVOKE(V,astSetSkyRefP_(astCheckSkyFrame(this),axis,value))
#define astTestSkyRefP(this,axis) astINVOKE(V,astTestSkyRefP_(astCheckSkyFrame(this),axis))

#endif
#endif
