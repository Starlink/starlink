#if !defined( SPECFLUXFRAME_INCLUDED ) /* Include this file only once */
#define SPECFLUXFRAME_INCLUDED
/*
*+
*  Name:
*     specfluxframe.h

*  Type:
*     C include file.

*  Purpose:
*     Define the interface to the SpecFluxFrame class.

*  Invocation:
*     #include "specfluxframe.h"

*  Description:
*     This include file defines the interface to the SpecFluxFrame class
*     and provides the type definitions, function prototypes and
*     macros, etc. needed to use this class.

*  Inheritance:
*     The SpecFluxFrame class inherits from the Frame class.

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
*     8-DEC-2004 (DSB):
*        Original version.
*-
*/

/* Include files. */
/* ============== */
/* Interface definitions. */
/* ---------------------- */
#include "object.h"              /* Base Object class */
#include "cmpframe.h"            /* Parent Frame class */

#if defined(astCLASS)            /* Protected */
#include "channel.h"             /* I/O channels */
#endif

/* C header files. */
/* --------------- */
#if defined(astCLASS)            /* Protected */
#include <stddef.h>
#endif

/* Macros. */
/* ------- */
#if defined(astCLASS) || defined(astFORTRAN77)
#define STATUS_PTR status
#else
#define STATUS_PTR astGetStatusPtr
#endif

/* Define a dummy __attribute__ macro for use on non-GNU compilers. */
#ifndef __GNUC__
#  define  __attribute__(x)  /*NOTHING*/
#endif

/* Type Definitions. */
/* ================= */
/* SpecFluxFrame structure. */
/* ------------------ */
/* This structure contains all information that is unique to each
   object in the class (e.g. its instance variables). */
typedef struct AstSpecFluxFrame {

/* Attributes inherited from the parent class. */
   AstCmpFrame cmpframe;         /* Parent class structure */

} AstSpecFluxFrame;

/* Virtual function table. */
/* ----------------------- */
/* This table contains all information that is the same for all
   objects in the class (e.g. pointers to its virtual functions). */
#if defined(astCLASS)            /* Protected */
typedef struct AstSpecFluxFrameVtab {

/* Properties (e.g. methods) inherited from the parent class. */
   AstCmpFrameVtab frame_vtab;   /* Parent class virtual function table */

/* A Unique identifier to determine class membership. */
   AstClassIdentifier id;

/* Properties (e.g. methods) specific to this class. */

} AstSpecFluxFrameVtab;

#if defined(THREAD_SAFE)

/* Define a structure holding all data items that are global within this
   class. */
typedef struct AstSpecFluxFrameGlobals {
   AstSpecFluxFrameVtab Class_Vtab;
   int Class_Init;
   char GetTitle_Buff[ 201 ];
} AstSpecFluxFrameGlobals;

#endif
#endif

/* Function prototypes. */
/* ==================== */
/* Prototypes for standard class functions. */
/* ---------------------------------------- */
astPROTO_CHECK(SpecFluxFrame)         /* Check class membership */
astPROTO_ISA(SpecFluxFrame)           /* Test class membership */

/* Constructor. */
#if defined(astCLASS)            /* Protected. */
AstSpecFluxFrame *astSpecFluxFrame_( void *, void *, const char *, int *, ...);
#else
AstSpecFluxFrame *astSpecFluxFrameId_( void *, void *, const char *, ... )__attribute__((format(printf,3,4)));
#endif

#if defined(astCLASS)            /* Protected */

/* Initialiser. */
AstSpecFluxFrame *astInitSpecFluxFrame_( void *, size_t, int, AstSpecFluxFrameVtab *,
                               const char *, AstSpecFrame *, AstFluxFrame *, int * );

/* Vtab initialiser. */
void astInitSpecFluxFrameVtab_( AstSpecFluxFrameVtab *, const char *, int * );

/* Loader. */
AstSpecFluxFrame *astLoadSpecFluxFrame_( void *, size_t, AstSpecFluxFrameVtab *,
                               const char *, AstChannel *, int * );

/* Thread-safe initialiser for all global data used by this module. */
#if defined(THREAD_SAFE)
void astInitSpecFluxFrameGlobals_( AstSpecFluxFrameGlobals * );
#endif

#endif

/* Prototypes for member functions. */
/* -------------------------------- */

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
#define astCheckSpecFluxFrame(this) astINVOKE_CHECK(SpecFluxFrame,this,0)
#define astVerifySpecFluxFrame(this) astINVOKE_CHECK(SpecFluxFrame,this,1)

/* Test class membership. */
#define astIsASpecFluxFrame(this) astINVOKE_ISA(SpecFluxFrame,this)

/* Constructor. */
#if defined(astCLASS)            /* Protected. */
#define astSpecFluxFrame astINVOKE(F,astSpecFluxFrame_)
#else
#define astSpecFluxFrame astINVOKE(F,astSpecFluxFrameId_)
#endif

#if defined(astCLASS)            /* Protected */

/* Initialiser. */
#define astInitSpecFluxFrame(mem,size,init,vtab,name,frame1,frame2) \
astINVOKE(O,astInitSpecFluxFrame_(mem,size,init,vtab,name,astCheckSpecFrame(frame1),astCheckFluxFrame(frame2),STATUS_PTR))

/* Vtab Initialiser. */
#define astInitSpecFluxFrameVtab(vtab,name) astINVOKE(V,astInitSpecFluxFrameVtab_(vtab,name,STATUS_PTR))
/* Loader. */
#define astLoadSpecFluxFrame(mem,size,vtab,name,channel) \
astINVOKE(O,astLoadSpecFluxFrame_(mem,size,vtab,name,astCheckChannel(channel),STATUS_PTR))
#endif

/* Interfaces to public member functions. */
/* -------------------------------------- */
/* Here we make use of astCheckSpecFluxFrame to validate SpecFluxFrame pointers
   before use.  This provides a contextual error report if a pointer
   to the wrong sort of Object is supplied. */

#endif





