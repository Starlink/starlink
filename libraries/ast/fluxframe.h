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
#define AST__FLUXDEN       1
#define AST__FLUXDENW      2
#define AST__SBRIGHT       3
#define AST__SBRIGHTW      4

/* Define constants used to size global arrays in this module. */
#define AST__FLUXFRAME_GETATTRIB_BUFF_LEN 50
#define AST__FLUXFRAME_GETLABEL_BUFF_LEN 200
#define AST__FLUXFRAME_GETSYMBOL_BUFF_LEN 20
#define AST__FLUXFRAME_GETTITLE_BUFF_LEN 200

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

/* A Unique identifier to determine class membership. */
   AstClassIdentifier id;

   AstSystemType (* GetDensitySystem)( AstFluxFrame *, int * );
   const char *(* GetDensityUnit)( AstFluxFrame *, int * );

   double (* GetSpecVal)( AstFluxFrame *, int * );
   int (* TestSpecVal)( AstFluxFrame *, int * );
   void (* ClearSpecVal)( AstFluxFrame *, int * );
   void (* SetSpecVal)( AstFluxFrame *, double, int * );

} AstFluxFrameVtab;

#if defined(THREAD_SAFE)

/* Define a structure holding all data items that are global within this
   class. */
typedef struct AstFluxFrameGlobals {
   AstFluxFrameVtab Class_Vtab;
   int Class_Init;
   char GetAttrib_Buff[ AST__FLUXFRAME_GETATTRIB_BUFF_LEN + 1 ];
   char GetLabel_Buff[ AST__FLUXFRAME_GETLABEL_BUFF_LEN + 1 ];
   char GetSymbol_Buff[ AST__FLUXFRAME_GETSYMBOL_BUFF_LEN + 1 ];
   char GetTitle_Buff[ AST__FLUXFRAME_GETTITLE_BUFF_LEN + 1 ];
} AstFluxFrameGlobals;

#endif

#endif

/* Function prototypes. */
/* ==================== */
/* Prototypes for standard class functions. */
/* ---------------------------------------- */
astPROTO_CHECK(FluxFrame)         /* Check class membership */
astPROTO_ISA(FluxFrame)           /* Test class membership */

/* Constructor. */
#if defined(astCLASS)            /* Protected */
AstFluxFrame *astFluxFrame_( double, void *, const char *, int *, ...);
#else
AstFluxFrame *astFluxFrameId_( double, void *, const char *, ... )__attribute__((format(printf,3,4)));
#endif

#if defined(astCLASS)            /* Protected */

/* Initialiser. */
AstFluxFrame *astInitFluxFrame_( void *, size_t, int,
                                 AstFluxFrameVtab *,
                                 const char *, double, AstSpecFrame *, int * );

/* Vtab initialiser. */
void astInitFluxFrameVtab_( AstFluxFrameVtab *, const char *, int * );

/* Loader. */
AstFluxFrame *astLoadFluxFrame_( void *, size_t,
                                 AstFluxFrameVtab *,
                                 const char *, AstChannel *channel, int * );

/* Thread-safe initialiser for all global data used by this module. */
#if defined(THREAD_SAFE)
void astInitFluxFrameGlobals_( AstFluxFrameGlobals * );
#endif

#endif

/* Prototypes for member functions. */
/* -------------------------------- */

#if defined(astCLASS)            /* Protected */

AstSystemType astGetDensitySystem_( AstFluxFrame *, int * );
const char *astGetDensityUnit_( AstFluxFrame *, int * );

double astGetSpecVal_( AstFluxFrame *, int * );
int astTestSpecVal_( AstFluxFrame *, int * );
void astClearSpecVal_( AstFluxFrame *, int * );
void astSetSpecVal_( AstFluxFrame *, double, int * );

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
#define astCheckFluxFrame(this) astINVOKE_CHECK(FluxFrame,this,0)
#define astVerifyFluxFrame(this) astINVOKE_CHECK(FluxFrame,this,1)

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
astINVOKE(O,astInitFluxFrame_(mem,size,init,vtab,name,specval,astCheckSpecFrame(specfrm),STATUS_PTR))

/* Vtab Initialiser. */
#define astInitFluxFrameVtab(vtab,name) astINVOKE(V,astInitFluxFrameVtab_(vtab,name,STATUS_PTR))
/* Loader. */
#define astLoadFluxFrame(mem,size,vtab,name,channel) \
astINVOKE(O,astLoadFluxFrame_(mem,size,vtab,name,astCheckChannel(channel),STATUS_PTR))

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

#define astGetDensitySystem(this) astINVOKE(V,astGetDensitySystem_(astCheckFluxFrame(this),STATUS_PTR))
#define astGetDensityUnit(this) astINVOKE(V,astGetDensityUnit_(astCheckFluxFrame(this),STATUS_PTR))

#define astGetSpecVal(this) astINVOKE(V,astGetSpecVal_(astCheckFluxFrame(this),STATUS_PTR))
#define astTestSpecVal(this) astINVOKE(V,astTestSpecVal_(astCheckFluxFrame(this),STATUS_PTR))
#define astClearSpecVal(this) astINVOKE(V,astClearSpecVal_(astCheckFluxFrame(this),STATUS_PTR))
#define astSetSpecVal(this,value) astINVOKE(V,astSetSpecVal_(astCheckFluxFrame(this),value,STATUS_PTR))

#endif
#endif





