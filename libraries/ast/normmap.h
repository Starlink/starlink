#if !defined( NORMMAP_INCLUDED ) /* Include this file only once */
#define NORMMAP_INCLUDED
/*
*+
*  Name:
*     normmap.h

*  Type:
*     C include file.

*  Purpose:
*     Define the interface to the NormMap class.

*  Invocation:
*     #include "normmap.h"

*  Description:
*     This include file defines the interface to the NormMap class and
*     provides the type definitions, function prototypes and macros,
*     etc.  needed to use this class.
*
*     The NormMap class implements Mappings which use a Frame to
*     normalise the input axis values.

*  Inheritance:
*     The NormMap class inherits from the Mapping class.

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
*     DSB: David S. Berry (Starlink)

*  History:
*     11-JUL-2005 (DSB):
*        Original version.
*-
*/

/* Include files. */
/* ============== */
/* Interface definitions. */
/* ---------------------- */
#include "mapping.h"             /* Coordinate mappings (parent class) */
#include "frame.h"               /* Coordinate Frames */

#if defined(astCLASS)            /* Protected */
#include "channel.h"             /* I/O channels */
#endif

/* C header files. */
/* --------------- */
#if defined(astCLASS)            /* Protected */
#include <stddef.h>
#endif

/* Type Definitions. */
/* ================= */
/* NormMap structure. */
/* ------------------ */
/* This structure contains all information that is unique to each object in
   the class (e.g. its instance variables). */
typedef struct AstNormMap {

/* Attributes inherited from the parent class. */
   AstMapping mapping;           /* Parent class structure */

/* Attributes specific to objects in this class. */
   AstFrame *frame;              /* Encapsulated Frame */
} AstNormMap;

/* Virtual function table. */
/* ----------------------- */
/* This table contains all information that is the same for all
   objects in the class (e.g. pointers to its virtual functions). */
#if defined(astCLASS)            /* Protected */
typedef struct AstNormMapVtab {

/* Properties (e.g. methods) inherited from the parent class. */
   AstMappingVtab mapping_vtab;  /* Parent class virtual function table */

/* Unique flag value to determine class membership. */
   int *check;                   /* Check value */

} AstNormMapVtab;
#endif

/* Function prototypes. */
/* ==================== */
/* Prototypes for standard class functions. */
/* ---------------------------------------- */
astPROTO_CHECK(NormMap)          /* Check class membership */
astPROTO_ISA(NormMap)            /* Test class membership */

/* Constructor. */
#if defined(astCLASS)            /* Protected. */
AstNormMap *astNormMap_( void *, const char *, ... );
#else
AstNormMap *astNormMapId_( void *, const char *, ... );
#endif

#if defined(astCLASS)            /* Protected */

/* Initialiser. */
AstNormMap *astInitNormMap_( void *, size_t, int, AstNormMapVtab *,
                             const char *, AstFrame * );

/* Vtab initialiser. */
void astInitNormMapVtab_( AstNormMapVtab *, const char * );

/* Loader. */
AstNormMap *astLoadNormMap_( void *, size_t, AstNormMapVtab *,
                             const char *, AstChannel * );
#endif

/* Prototypes for member functions. */
/* -------------------------------- */
# if defined(astCLASS)           /* Protected */
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
#define astCheckNormMap(this) astINVOKE_CHECK(NormMap,this)

/* Test class membership. */
#define astIsANormMap(this) astINVOKE_ISA(NormMap,this)

/* Constructor. */
#if defined(astCLASS)            /* Protected. */
#define astNormMap astINVOKE(F,astNormMap_)
#else
#define astNormMap astINVOKE(F,astNormMapId_)
#endif

#if defined(astCLASS)            /* Protected */

/* Initialiser. */
#define astInitNormMap(mem,size,init,vtab,name,frame) \
astINVOKE(O,astInitNormMap_(mem,size,init,vtab,name,frame))

/* Vtab Initialiser. */
#define astInitNormMapVtab(vtab,name) astINVOKE(V,astInitNormMapVtab_(vtab,name))
/* Loader. */
#define astLoadNormMap(mem,size,vtab,name,channel) \
astINVOKE(O,astLoadNormMap_(mem,size,vtab,name,astCheckChannel(channel)))
#endif

/* Interfaces to public member functions. */
/* -------------------------------------- */
/* Here we make use of astCheckNormMap to validate NormMap pointers
   before use.  This provides a contextual error report if a pointer
   to the wrong sort of Object is supplied. */

#if defined(astCLASS)            /* Protected */
#endif
#endif
