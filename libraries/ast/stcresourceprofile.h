#if !defined( STCRESOURCEPROFILE_INCLUDED ) /* Include this file only once */
#define STCRESOURCEPROFILE_INCLUDED
/*
*+
*  Name:
*     stcresourceprofile.h

*  Type:
*     C include file.

*  Purpose:
*     Define the interface to the StcResourceProfile class.

*  Invocation:
*     #include "stcresourceprofile.h"

*  Description:
*     This include file defines the interface to the StcResourceProfile class
*     and provides the type definitions, function prototypes and macros,
*     etc.  needed to use this class.
*
*     The StcResourceProfile class is a sub-class of Stc used to describe
*     the coverage of the datasets contained in some VO resource.
*
*     See http://hea-www.harvard.edu/~arots/nvometa/STC.html

*  Inheritance:
*     The StcResourceProfile class inherits from the Stc class.

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
*     26-NOV-2004 (DSB):
*        Original version.
*-
*/

/* Include files. */
/* ============== */
/* Interface definitions. */
/* ---------------------- */
#include "stc.h"                 /* Coordinate stcs (parent class) */

#if defined(astCLASS)            /* Protected */
#include "channel.h"             /* I/O channels */
#endif

/* C header files. */
/* --------------- */
#if defined(astCLASS)            /* Protected */
#include <stddef.h>
#endif

/* Macros */
/* ====== */

/* Define a dummy __attribute__ macro for use on non-GNU compilers. */
#ifndef __GNUC__
#  define  __attribute__(x)  /*NOTHING*/
#endif

/* Type Definitions. */
/* ================= */
/* StcResourceProfile structure. */
/* ----------------------------- */
/* This structure contains all information that is unique to each object in
   the class (e.g. its instance variables). */
typedef struct AstStcResourceProfile {

/* Attributes inherited from the parent class. */
   AstStc stc;             /* Parent class structure */

} AstStcResourceProfile;

/* Virtual function table. */
/* ----------------------- */
/* This table contains all information that is the same for all
   objects in the class (e.g. pointers to its virtual functions). */
#if defined(astCLASS)            /* Protected */
typedef struct AstStcResourceProfileVtab {

/* Properties (e.g. methods) inherited from the parent class. */
   AstStcVtab stc_vtab;    /* Parent class virtual function table */

/* A Unique identifier to determine class membership. */
   AstClassIdentifier id;

/* Properties (e.g. methods) specific to this class. */
} AstStcResourceProfileVtab;

#if defined(THREAD_SAFE)

/* Define a structure holding all data items that are global within the
   object.c file. */

typedef struct AstStcResourceProfileGlobals {
   AstStcResourceProfileVtab Class_Vtab;
   int Class_Init;
} AstStcResourceProfileGlobals;


/* Thread-safe initialiser for all global data used by this module. */
void astInitStcResourceProfileGlobals_( AstStcResourceProfileGlobals * );

#endif


#endif

/* Function prototypes. */
/* ==================== */
/* Prototypes for standard class functions. */
/* ---------------------------------------- */
astPROTO_CHECK(StcResourceProfile)          /* Check class membership */
astPROTO_ISA(StcResourceProfile)            /* Test class membership */

/* Constructor. */
#if defined(astCLASS)            /* Protected. */
AstStcResourceProfile *astStcResourceProfile_( void *, int, AstKeyMap **, const char *, int *, ...);
#else
AstStcResourceProfile *astStcResourceProfileId_( void *, int, AstKeyMap **, const char *, ... )__attribute__((format(printf,4,5)));
#endif

#if defined(astCLASS)            /* Protected */

/* Initialiser. */
AstStcResourceProfile *astInitStcResourceProfile_( void *, size_t, int, AstStcResourceProfileVtab *, const char *, AstRegion *, int, AstKeyMap **, int * );

/* Vtab initialiser. */
void astInitStcResourceProfileVtab_( AstStcResourceProfileVtab *, const char *, int * );

/* Loader. */
AstStcResourceProfile *astLoadStcResourceProfile_( void *, size_t, AstStcResourceProfileVtab *,
                                   const char *, AstChannel *, int * );

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
#define astCheckStcResourceProfile(this) astINVOKE_CHECK(StcResourceProfile,this,0)
#define astVerifyStcResourceProfile(this) astINVOKE_CHECK(StcResourceProfile,this,1)

/* Test class membership. */
#define astIsAStcResourceProfile(this) astINVOKE_ISA(StcResourceProfile,this)

/* Constructor. */
#if defined(astCLASS)            /* Protected. */
#define astStcResourceProfile astINVOKE(F,astStcResourceProfile_)
#else
#define astStcResourceProfile astINVOKE(F,astStcResourceProfileId_)
#endif

#if defined(astCLASS)            /* Protected */

/* Initialiser. */
#define astInitStcResourceProfile(mem,size,init,vtab,name,region,ncoords,coords) \
astINVOKE(O,astInitStcResourceProfile_(mem,size,init,vtab,name,region,ncoords,coords,STATUS_PTR))

/* Vtab Initialiser. */
#define astInitStcResourceProfileVtab(vtab,name) astINVOKE(V,astInitStcResourceProfileVtab_(vtab,name,STATUS_PTR))
/* Loader. */
#define astLoadStcResourceProfile(mem,size,vtab,name,channel) \
astINVOKE(O,astLoadStcResourceProfile_(mem,size,vtab,name,astCheckChannel(channel),STATUS_PTR))
#endif

/* Interfaces to public member functions. */
/* -------------------------------------- */
/* Here we make use of astCheckStcResourceProfile to validate StcResourceProfile pointers
   before use.  This provides a contextual error report if a pointer
   to the wrong sort of Object is supplied. */

#if defined(astCLASS)            /* Protected */
#endif
#endif





