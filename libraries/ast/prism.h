#if !defined( PRISM_INCLUDED ) /* Include this file only once */
#define PRISM_INCLUDED
/*
*+
*  Name:
*     prism.h

*  Type:
*     C include file.

*  Purpose:
*     Define the interface to the Prism class.

*  Invocation:
*     #include "prism.h"

*  Description:
*     This include file defines the interface to the Prism class and
*     provides the type definitions, function prototypes and macros,
*     etc.  needed to use this class.
*
*     The Prism class implement a Region which represents an extrusion of
*     another Region into higher dimensions. For instance, a Prism can be
*     used to represent a cylinder, which is an extrusion of a circle into a
*     3rd dimension.

*  Inheritance:
*     The Prism class inherits from the Region class.

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
*     17-DEC-2004 (DSB):
*        Original version.
*-
*/

/* Include files. */
/* ============== */
/* Interface definitions. */
/* ---------------------- */
#include "region.h"              /* Coordinate regions (parent class) */

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

/* Define a dummy __attribute__ macro for use on non-GNU compilers. */
#ifndef __GNUC__
#  define  __attribute__(x)  /*NOTHING*/
#endif

/* Type Definitions. */
/* ================= */
/* Prism structure. */
/* ------------------ */
/* This structure contains all information that is unique to each object in
   the class (e.g. its instance variables). */
#if defined(astCLASS) || defined(astFORTRAN77)
#define STATUS_PTR status
#else
#define STATUS_PTR astGetStatusPtr
#endif

typedef struct AstPrism {

/* Attributes inherited from the parent class. */
   AstRegion region;             /* Parent class structure */

/* Attributes specific to objects in this class. */
   AstRegion *region1;           /* First component Region */
   AstRegion *region2;           /* Second component Region */

} AstPrism;

/* Virtual function table. */
/* ----------------------- */
/* This table contains all information that is the same for all
   objects in the class (e.g. pointers to its virtual functions). */
#if defined(astCLASS)            /* Protected */
typedef struct AstPrismVtab {

/* Properties (e.g. methods) inherited from the parent class. */
   AstRegionVtab region_vtab;    /* Parent class virtual function table */

/* A Unique identifier to determine class membership. */
   AstClassIdentifier id;

/* Properties (e.g. methods) specific to this class. */
} AstPrismVtab;

#if defined(THREAD_SAFE)

/* Define a structure holding all data items that are global within the
   object.c file. */

typedef struct AstPrismGlobals {
   AstPrismVtab Class_Vtab;
   int Class_Init;
} AstPrismGlobals;


/* Thread-safe initialiser for all global data used by this module. */
void astInitPrismGlobals_( AstPrismGlobals * );

#endif


#endif

/* Function prototypes. */
/* ==================== */
/* Prototypes for standard class functions. */
/* ---------------------------------------- */
astPROTO_CHECK(Prism)          /* Check class membership */
astPROTO_ISA(Prism)            /* Test class membership */

/* Constructor. */
#if defined(astCLASS)            /* Protected. */
AstPrism *astPrism_( void *, void *, const char *, int *, ...);
#else
AstPrism *astPrismId_( void *, void *, const char *, ... )__attribute__((format(printf,3,4)));
#endif

#if defined(astCLASS)            /* Protected */

/* Initialiser. */
AstPrism *astInitPrism_( void *, size_t, int, AstPrismVtab *,
                     const char *, AstRegion *, AstRegion *, int * );

/* Vtab initialiser. */
void astInitPrismVtab_( AstPrismVtab *, const char *, int * );

/* Loader. */
AstPrism *astLoadPrism_( void *, size_t, AstPrismVtab *,
                         const char *, AstChannel *, int * );

#endif

/* Prototypes for member functions. */
/* -------------------------------- */
# if defined(astCLASS)           /* Protected */
AstRegion *astConvertToPrism_( AstRegion *, int * );
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
#define astCheckPrism(this) astINVOKE_CHECK(Prism,this,0)
#define astVerifyPrism(this) astINVOKE_CHECK(Prism,this,1)

/* Test class membership. */
#define astIsAPrism(this) astINVOKE_ISA(Prism,this)

/* Constructor. */
#if defined(astCLASS)            /* Protected. */
#define astPrism astINVOKE(F,astPrism_)
#else
#define astPrism astINVOKE(F,astPrismId_)
#endif

#if defined(astCLASS)            /* Protected */

/* Initialiser. */
#define astInitPrism(mem,size,init,vtab,name,reg1,reg2) \
astINVOKE(O,astInitPrism_(mem,size,init,vtab,name,reg1,reg2,STATUS_PTR))

/* Vtab Initialiser. */
#define astInitPrismVtab(vtab,name) astINVOKE(V,astInitPrismVtab_(vtab,name,STATUS_PTR))
/* Loader. */
#define astLoadPrism(mem,size,vtab,name,channel) \
astINVOKE(O,astLoadPrism_(mem,size,vtab,name,astCheckChannel(channel),STATUS_PTR))
#endif

/* Interfaces to public member functions. */
/* -------------------------------------- */
/* Here we make use of astCheckPrism to validate Prism pointers
   before use.  This provides a contextual error report if a pointer
   to the wrong sort of Object is supplied. */

#if defined(astCLASS)            /* Protected */
#define astConvertToPrism(this) astConvertToPrism_(this,STATUS_PTR)
#endif
#endif





