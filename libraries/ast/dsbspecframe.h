#if !defined( DSBSPECFRAME_INCLUDED ) /* Include this file only once */
#define DSBSPECFRAME_INCLUDED
/*
*+
*  Name:
*     dsbspecframe.h

*  Type:
*     C include file.

*  Purpose:
*     Define the interface to the DSBSpecFrame class.

*  Invocation:
*     #include "dsbspecframe.h"

*  Description:
*     This include file defines the interface to the DSBSpecFrame class and
*     provides the type definitions, function prototypes and macros,
*     etc.  needed to use this class.

*  Inheritance:
*     The DSBSpecFrame class inherits from the SpecFrame class.

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
*     5-AUG-2004 (DSB):
*        Original version.
*     27-OCT-2006 (DSB):
*        Added AlignSideBand.
*-
*/

/* Include files. */
/* ============== */
/* Interface definitions. */
/* ---------------------- */
#include "specframe.h"           /* Spectral coord systems (parent class) */

/* C header files. */
/* --------------- */

/* Macros */
/* ====== */

/* Define a dummy __attribute__ macro for use on non-GNU compilers. */
#ifndef __GNUC__
#  define  __attribute__(x)  /*NOTHING*/
#endif

/* Type Definitions. */
/* ================= */
/* DSBSpecFrame structure. */
/* ------------------ */
/* This structure contains all information that is unique to each object in
   the class (e.g. its instance variables). */
typedef struct AstDSBSpecFrame {

/* Attributes inherited from the parent class. */
   AstSpecFrame specframe;      /* Parent class structure */

/* Attributes specific to objects in this class. */
   double dsbcentre;            /* Centre frequency */
   double ifr;                  /* Intermediate frequency */
   int sideband;                /* Current sideband */
   int alignsideband;           /* Aligns sidebands? */

} AstDSBSpecFrame;

/* Virtual function table. */
/* ----------------------- */
/* This table contains all information that is the same for all
   objects in the class (e.g. pointers to its virtual functions). */
#if defined(astCLASS)            /* Protected */
typedef struct AstDSBSpecFrameVtab {

/* Properties (e.g. methods) inherited from the parent class. */
   AstSpecFrameVtab specframe_vtab;  /* Parent class virtual function table */

/* A Unique identifier to determine class membership. */
   AstClassIdentifier id;

/* Properties (e.g. methods) specific to this class. */
   double (* GetDSBCentre)( AstDSBSpecFrame *, int * );
   int (* TestDSBCentre)( AstDSBSpecFrame *, int * );
   void (* ClearDSBCentre)( AstDSBSpecFrame *, int * );
   void (* SetDSBCentre)( AstDSBSpecFrame *, double, int * );

   double (* GetIF)( AstDSBSpecFrame *, int * );
   int (* TestIF)( AstDSBSpecFrame *, int * );
   void (* ClearIF)( AstDSBSpecFrame *, int * );
   void (* SetIF)( AstDSBSpecFrame *, double, int * );

   int (* GetSideBand)( AstDSBSpecFrame *, int * );
   int (* TestSideBand)( AstDSBSpecFrame *, int * );
   void (* ClearSideBand)( AstDSBSpecFrame *, int * );
   void (* SetSideBand)( AstDSBSpecFrame *, int, int * );

   int (* GetAlignSideBand)( AstDSBSpecFrame *, int * );
   int (* TestAlignSideBand)( AstDSBSpecFrame *, int * );
   void (* ClearAlignSideBand)( AstDSBSpecFrame *, int * );
   void (* SetAlignSideBand)( AstDSBSpecFrame *, int, int * );

   double (* GetImagFreq)( AstDSBSpecFrame *, int * );

} AstDSBSpecFrameVtab;

#if defined(THREAD_SAFE)

/* Define a structure holding all data items that are global within this
   class. */
typedef struct AstDSBSpecFrameGlobals {
   AstDSBSpecFrameVtab Class_Vtab;
   int Class_Init;
   char GetAttrib_Buff[ 101 ];
   char GetLabel_Buff[ 101 ];
} AstDSBSpecFrameGlobals;

#endif

#endif

/* Function prototypes. */
/* ==================== */
/* Prototypes for standard class functions. */
/* ---------------------------------------- */
astPROTO_CHECK(DSBSpecFrame)          /* Check class membership */
astPROTO_ISA(DSBSpecFrame)            /* Test class membership */

/* Constructor. */
#if defined(astCLASS)            /* Protected. */
AstDSBSpecFrame *astDSBSpecFrame_( const char *, int *, ...);
#else
AstDSBSpecFrame *astDSBSpecFrameId_( const char *, ... )__attribute__((format(printf,1,2)));
#endif

#if defined(astCLASS)            /* Protected */

/* Initialiser. */
AstDSBSpecFrame *astInitDSBSpecFrame_( void *, size_t, int, AstDSBSpecFrameVtab *,
                                       const char *, int * );

/* Vtab initialiser. */
void astInitDSBSpecFrameVtab_( AstDSBSpecFrameVtab *, const char *, int * );

/* Loader. */
AstDSBSpecFrame *astLoadDSBSpecFrame_( void *, size_t, AstDSBSpecFrameVtab *,
                                       const char *, AstChannel *, int * );

/* Thread-safe initialiser for all global data used by this module. */
#if defined(THREAD_SAFE)
void astInitDSBSpecFrameGlobals_( AstDSBSpecFrameGlobals * );
#endif

#endif

/* Prototypes for member functions. */
/* -------------------------------- */
# if defined(astCLASS)           /* Protected */
   double astGetDSBCentre_( AstDSBSpecFrame *, int * );
   int astTestDSBCentre_( AstDSBSpecFrame *, int * );
   void astClearDSBCentre_( AstDSBSpecFrame *, int * );
   void astSetDSBCentre_( AstDSBSpecFrame *, double, int * );

   double astGetIF_( AstDSBSpecFrame *, int * );
   int astTestIF_( AstDSBSpecFrame *, int * );
   void astClearIF_( AstDSBSpecFrame *, int * );
   void astSetIF_( AstDSBSpecFrame *, double, int * );

   int astGetSideBand_( AstDSBSpecFrame *, int * );
   int astTestSideBand_( AstDSBSpecFrame *, int * );
   void astClearSideBand_( AstDSBSpecFrame *, int * );
   void astSetSideBand_( AstDSBSpecFrame *, int, int * );

   int astGetAlignSideBand_( AstDSBSpecFrame *, int * );
   int astTestAlignSideBand_( AstDSBSpecFrame *, int * );
   void astClearAlignSideBand_( AstDSBSpecFrame *, int * );
   void astSetAlignSideBand_( AstDSBSpecFrame *, int, int * );

   double astGetImagFreq_( AstDSBSpecFrame *, int * );
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
#define astCheckDSBSpecFrame(this) astINVOKE_CHECK(DSBSpecFrame,this,0)
#define astVerifyDSBSpecFrame(this) astINVOKE_CHECK(DSBSpecFrame,this,1)

/* Test class membership. */
#define astIsADSBSpecFrame(this) astINVOKE_ISA(DSBSpecFrame,this)

/* Constructor. */
#if defined(astCLASS)            /* Protected. */
#define astDSBSpecFrame astINVOKE(F,astDSBSpecFrame_)
#else
#define astDSBSpecFrame astINVOKE(F,astDSBSpecFrameId_)
#endif

#if defined(astCLASS)            /* Protected */

/* Initialiser. */
#define \
astInitDSBSpecFrame(mem,size,init,vtab,name) \
astINVOKE(O,astInitDSBSpecFrame_(mem,size,init,vtab,name,STATUS_PTR))

/* Vtab Initialiser. */
#define astInitDSBSpecFrameVtab(vtab,name) astINVOKE(V,astInitDSBSpecFrameVtab_(vtab,name,STATUS_PTR))
/* Loader. */
#define astLoadDSBSpecFrame(mem,size,vtab,name,channel) \
astINVOKE(O,astLoadDSBSpecFrame_(mem,size,vtab,name,astCheckChannel(channel),STATUS_PTR))
#endif

/* Interfaces to public member functions. */
/* -------------------------------------- */
/* Here we make use of astCheckDSBSpecFrame to validate DSBSpecFrame pointers
   before use.  This provides a contextual error report if a pointer
   to the wrong sort of Object is supplied. */

#if defined(astCLASS)            /* Protected */

#define astGetDSBCentre(this) \
astINVOKE(V,astGetDSBCentre_(astCheckDSBSpecFrame(this),STATUS_PTR))
#define astTestDSBCentre(this) \
astINVOKE(V,astTestDSBCentre_(astCheckDSBSpecFrame(this),STATUS_PTR))
#define astClearDSBCentre(this) \
astINVOKE(V,astClearDSBCentre_(astCheckDSBSpecFrame(this),STATUS_PTR))
#define astSetDSBCentre(this,val) \
astINVOKE(V,astSetDSBCentre_(astCheckDSBSpecFrame(this),val,STATUS_PTR))

#define astGetIF(this) \
astINVOKE(V,astGetIF_(astCheckDSBSpecFrame(this),STATUS_PTR))
#define astTestIF(this) \
astINVOKE(V,astTestIF_(astCheckDSBSpecFrame(this),STATUS_PTR))
#define astClearIF(this) \
astINVOKE(V,astClearIF_(astCheckDSBSpecFrame(this),STATUS_PTR))
#define astSetIF(this,val) \
astINVOKE(V,astSetIF_(astCheckDSBSpecFrame(this),val,STATUS_PTR))

#define astGetSideBand(this) \
astINVOKE(V,astGetSideBand_(astCheckDSBSpecFrame(this),STATUS_PTR))
#define astTestSideBand(this) \
astINVOKE(V,astTestSideBand_(astCheckDSBSpecFrame(this),STATUS_PTR))
#define astClearSideBand(this) \
astINVOKE(V,astClearSideBand_(astCheckDSBSpecFrame(this),STATUS_PTR))
#define astSetSideBand(this,val) \
astINVOKE(V,astSetSideBand_(astCheckDSBSpecFrame(this),val,STATUS_PTR))

#define astGetAlignSideBand(this) \
astINVOKE(V,astGetAlignSideBand_(astCheckDSBSpecFrame(this),STATUS_PTR))
#define astTestAlignSideBand(this) \
astINVOKE(V,astTestAlignSideBand_(astCheckDSBSpecFrame(this),STATUS_PTR))
#define astClearAlignSideBand(this) \
astINVOKE(V,astClearAlignSideBand_(astCheckDSBSpecFrame(this),STATUS_PTR))
#define astSetAlignSideBand(this,val) \
astINVOKE(V,astSetAlignSideBand_(astCheckDSBSpecFrame(this),val,STATUS_PTR))

#define astGetImagFreq(this) \
astINVOKE(V,astGetImagFreq_(astCheckDSBSpecFrame(this),STATUS_PTR))
#endif
#endif





