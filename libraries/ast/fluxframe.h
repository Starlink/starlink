#if !defined( FLUXFRAME_INCLUDED ) /* Include this file only once */
#define FLUXFRAME_INCLUDED
/*
*+
*  Name:
*     fluxframe.h

*  Type:
*     C include file.

*  Purpose:
*     Define the interface to the FluxFrame class.

*  Invocation:
*     #include "fluxframe.h"

*  Description:
*     This include file defines the interface to the FluxFrame class
*     and provides the type definitions, function prototypes and
*     macros, etc. needed to use this class.

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
*     DSB: David S. Berry (Starlink)

*  History:
*     1-DEC-2004 (DSB):
*        Original version.
*-
*/

/* Include files. */
/* ============== */
/* Interface definitions. */
/* ---------------------- */
#include "object.h"              /* Base Object class */
#include "frame.h"               /* Parent Frame class */
#include "specframe.h"           /* Spectral coordinate systems */

/* Macros. */
/* ======= */

#if defined(astCLASS)            /* Protected */

/* Values used to represent different System attribute values. */
#define AST__FLUXDEN       1
#define AST__FLUXDENW      2
#define AST__SBRIGHT       3
#define AST__SBRIGHTW      4

#endif

/* Type Definitions. */
/* ================= */

/* FluxFrame structure. */
/* ------------------- */
/* This structure contains all information that is unique to each object in
   the class (e.g. its instance variables). */
typedef struct AstFluxFrame {

/* Attributes inherited from the parent class. */
   AstFrame frame;               /* Parent class structure */

/* Attributes specific to objects in this class. */
   double specval;               /* Spectral position */
   double defspecval;            /* Default spectral position */
   AstSpecFrame *specframe;       /* SpecFrame describing specval & defspecval */
   int nuunits;                  /* Size of usedunits array */
   char **usedunits;             /* Last used units for each system */
} AstFluxFrame;

/* Virtual function table. */
/* ----------------------- */
/* This table contains all information that is the same for all objects in the
   class (e.g. pointers to its virtual functions). */
#if defined(astCLASS)            /* Protected */
typedef struct AstFluxFrameVtab {

/* Properties (e.g. methods) inherited from the parent class. */
   AstFrameVtab frame_vtab;      /* Parent class virtual function table */

/* Unique flag value to determine class membership. */
   int *check;                   /* Check value */

   AstSystemType (* GetDensitySystem)( AstFluxFrame * );
   const char *(* GetDensityUnit)( AstFluxFrame * );

   double (* GetSpecVal)( AstFluxFrame * );
   int (* TestSpecVal)( AstFluxFrame * );
   void (* ClearSpecVal)( AstFluxFrame * );
   void (* SetSpecVal)( AstFluxFrame *, double );

} AstFluxFrameVtab;
#endif

/* Function prototypes. */
/* ==================== */
/* Prototypes for standard class functions. */
/* ---------------------------------------- */
astPROTO_CHECK(FluxFrame)         /* Check class membership */
astPROTO_ISA(FluxFrame)           /* Test class membership */

/* Constructor. */
#if defined(astCLASS)            /* Protected */
AstFluxFrame *astFluxFrame_( double, void *, const char *, ... );
#else
AstFluxFrame *astFluxFrameId_( double, void *, const char *, ... );
#endif

#if defined(astCLASS)            /* Protected */

/* Initialiser. */
AstFluxFrame *astInitFluxFrame_( void *, size_t, int, 
                                 AstFluxFrameVtab *,
                                 const char *, double, AstSpecFrame * );

/* Vtab initialiser. */
void astInitFluxFrameVtab_( AstFluxFrameVtab *, const char * );

/* Loader. */
AstFluxFrame *astLoadFluxFrame_( void *, size_t, 
                                 AstFluxFrameVtab *,
                                 const char *, AstChannel *channel );
#endif

/* Prototypes for member functions. */
/* -------------------------------- */

#if defined(astCLASS)            /* Protected */

AstSystemType astGetDensitySystem_( AstFluxFrame * );
const char *astGetDensityUnit_( AstFluxFrame * );

double astGetSpecVal_( AstFluxFrame * );
int astTestSpecVal_( AstFluxFrame * );
void astClearSpecVal_( AstFluxFrame * );
void astSetSpecVal_( AstFluxFrame *, double );

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
#define astCheckFluxFrame(this) astINVOKE_CHECK(FluxFrame,this)

/* Test class membership. */
#define astIsAFluxFrame(this) astINVOKE_ISA(FluxFrame,this)

/* Constructor. */
#if defined(astCLASS)            /* Protected */
#define astFluxFrame astINVOKE(F,astFluxFrame_)
#else
#define astFluxFrame astINVOKE(F,astFluxFrameId_)
#endif

#if defined(astCLASS)            /* Protected */

/* Initialiser. */
#define astInitFluxFrame(mem,size,init,vtab,name,specval,specfrm) \
astINVOKE(O,astInitFluxFrame_(mem,size,init,vtab,name,specval,astCheckSpecFrame(specfrm)))

/* Vtab Initialiser. */
#define astInitFluxFrameVtab(vtab,name) astINVOKE(V,astInitFluxFrameVtab_(vtab,name))
/* Loader. */
#define astLoadFluxFrame(mem,size,vtab,name,channel) \
astINVOKE(O,astLoadFluxFrame_(mem,size,vtab,name,astCheckChannel(channel)))

#endif

/* Interfaces to public member functions. */
/* -------------------------------------- */

/* None. */

/* Interfaces to protected member functions. */
/* ----------------------------------------- */
/* Here we make use of astCheckFluxFrame to validate FluxFrame pointers
   before use. This provides a contextual error report if a pointer to
   the wrong sort of object is supplied. */

#if defined(astCLASS)            /* Protected */

#define astGetDensitySystem(this) astINVOKE(V,astGetDensitySystem_(astCheckFluxFrame(this)))
#define astGetDensityUnit(this) astINVOKE(V,astGetDensityUnit_(astCheckFluxFrame(this)))

#define astGetSpecVal(this) astINVOKE(V,astGetSpecVal_(astCheckFluxFrame(this)))
#define astTestSpecVal(this) astINVOKE(V,astTestSpecVal_(astCheckFluxFrame(this)))
#define astClearSpecVal(this) astINVOKE(V,astClearSpecVal_(astCheckFluxFrame(this)))
#define astSetSpecVal(this,value) astINVOKE(V,astSetSpecVal_(astCheckFluxFrame(this),value))

#endif
#endif
