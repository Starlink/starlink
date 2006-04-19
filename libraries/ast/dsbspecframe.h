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
*     <COPYRIGHT_STATEMENT>

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
*     5-AUG-2004 (DSB):
*        Original version.
*-
*/

/* Include files. */
/* ============== */
/* Interface definitions. */
/* ---------------------- */
#include "specframe.h"           /* Spectral coord systems (parent class) */

/* C header files. */
/* --------------- */

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

} AstDSBSpecFrame;

/* Virtual function table. */
/* ----------------------- */
/* This table contains all information that is the same for all
   objects in the class (e.g. pointers to its virtual functions). */
#if defined(astCLASS)            /* Protected */
typedef struct AstDSBSpecFrameVtab {

/* Properties (e.g. methods) inherited from the parent class. */
   AstSpecFrameVtab specframe_vtab;  /* Parent class virtual function table */

/* Unique flag value to determine class membership. */
   int *check;                   /* Check value */

/* Properties (e.g. methods) specific to this class. */
   double (* GetDSBCentre)( AstDSBSpecFrame * );
   int (* TestDSBCentre)( AstDSBSpecFrame * );
   void (* ClearDSBCentre)( AstDSBSpecFrame * );
   void (* SetDSBCentre)( AstDSBSpecFrame *, double );

   double (* GetIF)( AstDSBSpecFrame * );
   int (* TestIF)( AstDSBSpecFrame * );
   void (* ClearIF)( AstDSBSpecFrame * );
   void (* SetIF)( AstDSBSpecFrame *, double );

   int (* GetSideBand)( AstDSBSpecFrame * );
   int (* TestSideBand)( AstDSBSpecFrame * );
   void (* ClearSideBand)( AstDSBSpecFrame * );
   void (* SetSideBand)( AstDSBSpecFrame *, int );

   double (* GetImagFreq)( AstDSBSpecFrame * );

} AstDSBSpecFrameVtab;
#endif

/* Function prototypes. */
/* ==================== */
/* Prototypes for standard class functions. */
/* ---------------------------------------- */
astPROTO_CHECK(DSBSpecFrame)          /* Check class membership */
astPROTO_ISA(DSBSpecFrame)            /* Test class membership */

/* Constructor. */
#if defined(astCLASS)            /* Protected. */
AstDSBSpecFrame *astDSBSpecFrame_( const char *, ... );
#else
AstDSBSpecFrame *astDSBSpecFrameId_( const char *, ... );
#endif

#if defined(astCLASS)            /* Protected */

/* Initialiser. */
AstDSBSpecFrame *astInitDSBSpecFrame_( void *, size_t, int, AstDSBSpecFrameVtab *,
                                       const char * );

/* Vtab initialiser. */
void astInitDSBSpecFrameVtab_( AstDSBSpecFrameVtab *, const char * );

/* Loader. */
AstDSBSpecFrame *astLoadDSBSpecFrame_( void *, size_t, AstDSBSpecFrameVtab *,
                                       const char *, AstChannel * );
#endif

/* Prototypes for member functions. */
/* -------------------------------- */
# if defined(astCLASS)           /* Protected */
   double astGetDSBCentre_( AstDSBSpecFrame * );
   int astTestDSBCentre_( AstDSBSpecFrame * );
   void astClearDSBCentre_( AstDSBSpecFrame * );
   void astSetDSBCentre_( AstDSBSpecFrame *, double );

   double astGetIF_( AstDSBSpecFrame * );
   int astTestIF_( AstDSBSpecFrame * );
   void astClearIF_( AstDSBSpecFrame * );
   void astSetIF_( AstDSBSpecFrame *, double );

   int astGetSideBand_( AstDSBSpecFrame * );
   int astTestSideBand_( AstDSBSpecFrame * );
   void astClearSideBand_( AstDSBSpecFrame * );
   void astSetSideBand_( AstDSBSpecFrame *, int );

   double astGetImagFreq_( AstDSBSpecFrame * );
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
#define astCheckDSBSpecFrame(this) astINVOKE_CHECK(DSBSpecFrame,this)

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
astINVOKE(O,astInitDSBSpecFrame_(mem,size,init,vtab,name))

/* Vtab Initialiser. */
#define astInitDSBSpecFrameVtab(vtab,name) astINVOKE(V,astInitDSBSpecFrameVtab_(vtab,name))
/* Loader. */
#define astLoadDSBSpecFrame(mem,size,vtab,name,channel) \
astINVOKE(O,astLoadDSBSpecFrame_(mem,size,vtab,name,astCheckChannel(channel)))
#endif

/* Interfaces to public member functions. */
/* -------------------------------------- */
/* Here we make use of astCheckDSBSpecFrame to validate DSBSpecFrame pointers
   before use.  This provides a contextual error report if a pointer
   to the wrong sort of Object is supplied. */

#if defined(astCLASS)            /* Protected */

#define astGetDSBCentre(this) \
astINVOKE(V,astGetDSBCentre_(astCheckDSBSpecFrame(this)))
#define astTestDSBCentre(this) \
astINVOKE(V,astTestDSBCentre_(astCheckDSBSpecFrame(this)))
#define astClearDSBCentre(this) \
astINVOKE(V,astClearDSBCentre_(astCheckDSBSpecFrame(this)))
#define astSetDSBCentre(this,val) \
astINVOKE(V,astSetDSBCentre_(astCheckDSBSpecFrame(this),val))
				                                   
#define astGetIF(this) \
astINVOKE(V,astGetIF_(astCheckDSBSpecFrame(this)))
#define astTestIF(this) \
astINVOKE(V,astTestIF_(astCheckDSBSpecFrame(this)))
#define astClearIF(this) \
astINVOKE(V,astClearIF_(astCheckDSBSpecFrame(this)))
#define astSetIF(this,val) \
astINVOKE(V,astSetIF_(astCheckDSBSpecFrame(this),val))
				                                   
#define astGetSideBand(this) \
astINVOKE(V,astGetSideBand_(astCheckDSBSpecFrame(this)))
#define astTestSideBand(this) \
astINVOKE(V,astTestSideBand_(astCheckDSBSpecFrame(this)))
#define astClearSideBand(this) \
astINVOKE(V,astClearSideBand_(astCheckDSBSpecFrame(this)))
#define astSetSideBand(this,val) \
astINVOKE(V,astSetSideBand_(astCheckDSBSpecFrame(this),val))

#define astGetImagFreq(this) \
astINVOKE(V,astGetImagFreq_(astCheckDSBSpecFrame(this)))
#endif
#endif
