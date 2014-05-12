#if !defined( MATHMAP_INCLUDED ) /* Include this file only once */
#define MATHMAP_INCLUDED
/*
*+
*  Name:
*     mathmap.h

*  Type:
*     C include file.

*  Purpose:
*     Define the interface to the MathMap class.

*  Invocation:
*     #include "mathmap.h"

*  Description:
*     This include file defines the interface to the MathMap class and
*     provides the type definitions, function prototypes and macros,
*     etc.  needed to use this class.
*
*     The MathMap class implements Mappings that are specified by a series
*     of arithmetic expressions that relate output variables to input
*     variables (and vice versa).

*  Inheritance:
*     The MathMap class inherits from the Mapping class.

*  Attributes Over-Ridden:
*     None.

*  New Attributes Defined:
*     Seed
*        Random number seed.
*     SimpFI
*        Forward-inverse MathMap pairs simplify?
*     SimpIF
*        Inverse-forward MathMap pairs simplify?

*  Methods Over-Ridden:
*     Public:
*        None.
*
*     Protected:
*        astClearAttrib
*           Clear an attribute value for a Frame.
*        astGetAttrib
*           Get an attribute value for a Frame.
*        astMapMerge
*           Simplify a sequence of Mappings containing a MathMap.
*        astSetAttrib
*           Set an attribute value for a Frame.
*        astTestAttrib
*           Test if an attribute value has been set for a Frame.
*        astTransform
*           Transform a set of points.

*  New Methods Defined:
*     Public:
*        None.
*
*     Protected:
*        astClearSeed
*           Clear the Seed attribute for a MathMap.
*        astClearSimpFI
*           Clear the SimpFI attribute for a MathMap.
*        astClearSimpIF
*           Clear the SimpIF attribute for a MathMap.
*        astGetSeed
*           Get the value of the Seed attribute for a MathMap.
*        astGetSimpFI
*           Get the value of the SimpFI attribute for a MathMap.
*        astGetSimpIF
*           Get the value of the SimpIF attribute for a MathMap.
*        astSetSeed
*           Set the value of the Seed attribute for a MathMap.
*        astSetSimpFI
*           Set the value of the SimpFI attribute for a MathMap.
*        astSetSimpIF
*           Set the value of the SimpIF attribute for a MathMap.
*        astTestSeed
*           Test whether a value has been set for the Seed attribute of a
*           MathMap.
*        astTestSimpFI
*           Test whether a value has been set for the SimpFI attribute of a
*           MathMap.
*        astTestSimpIF
*           Test whether a value has been set for the SimpIF attribute of a
*           MathMap.

*  Other Class Functions:
*     Public:
*        astIsAMathMap
*           Test class membership.
*        astMathMap
*           Create a MathMap.
*
*     Protected:
*        astCheckMathMap
*           Validate class membership.
*        astInitMathMap
*           Initialise a MathMap.
*        astInitMathMapVtab
*           Initialise the virtual function table for the MathMap class.
*        astLoadMathMap
*           Load a MathMap.

*  Macros:
*     None.

*  Type Definitions:
*     Public:
*        AstMathMap
*           MathMap object type.
*
*     Protected:
*        AstMathMapVtab
*           MathMap virtual function table type.

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
*     RFWS: R.F. Warren-Smith (Starlink)

*  History:
*     3-SEP-1999 (RFWS):
*        Original version.
*     8-JAN-2003 (DSB):
*        Added protected astInitMathMapVtab method.
*-
*/

/* Include files. */
/* ============== */
/* Interface definitions. */
/* ---------------------- */
#include "mapping.h"             /* Coordinate mappings (parent class) */

#if defined(astCLASS)            /* Protected */
#include "channel.h"             /* I/O channels */
#include "pointset.h"            /* Sets of points/coordinates */
#endif

/* C header files. */
/* --------------- */
#if defined(astCLASS)            /* Protected */
#include <stddef.h>
#endif

/* Macros. */
/* ======= */
/* This value defines the size of an internal table in the AstMathMap
   data type. Since it will be publicly accessible (but of no external
   use), we give it an obscure name. */

#if defined(astCLASS) || defined(astFORTRAN77)
#define STATUS_PTR status
#else
#define STATUS_PTR astGetStatusPtr
#endif
#define AST_MATHMAP_RAND_CONTEXT_NTAB_ (32)

/* Define a dummy __attribute__ macro for use on non-GNU compilers. */
#ifndef __GNUC__
#  define  __attribute__(x)  /*NOTHING*/
#endif

/* Type Definitions. */
/* ================= */
/* Random number generator context. */
/* -------------------------------- */
/* This structure holds the context for the random number generator
   used by each MathMap. This ensures that the random number sequences
   used by different MathMaps are independent, and can be independently
   controlled by setting/clearing their Seed attributes. Random numbers
   are produced by combining the output of two internal generators. */
typedef struct AstMathMapRandContext_ {
   long int rand1;               /* State of first internal generator */
   long int rand2;               /* State of second internal generator */
   long int random_int;          /* Last random integer produced */
   long int table[ AST_MATHMAP_RAND_CONTEXT_NTAB_ ]; /* Shuffle table */
   int active;                   /* Generator has been initialised? */
   int seed;                     /* Seed to be used during initialisation */
   int seed_set;                 /* Seed value set via "Seed" attribute? */
} AstMathMapRandContext_;

/* MathMap structure. */
/* ------------------ */
/* This structure contains all information that is unique to each
   object in the class (e.g. its instance variables). */
typedef struct AstMathMap {

/* Attributes inherited from the parent class. */
   AstMapping mapping;           /* Parent class structure */

/* Attributes specific to objects in this class. */
   AstMathMapRandContext_ rcontext; /* Random number generator context */
   char **fwdfun;                /* Array of forward functions */
   char **invfun;                /* Array of inverse functions */
   double **fwdcon;              /* Array of constants for forward functions */
   double **invcon;              /* Array of constants for inverse functions */
   int **fwdcode;                /* Array of opcodes for forward functions */
   int **invcode;                /* Array of opcodes for inverse functions */
   int fwdstack;                 /* Stack size required by forward functions */
   int invstack;                 /* Stack size required by inverse functions */
   int nfwd;                     /* Number of forward functions */
   int ninv;                     /* Number of inverse functions */
   int simp_fi;                  /* Forward-inverse MathMap pairs simplify? */
   int simp_if;                  /* Inverse-forward MathMap pairs simplify? */
} AstMathMap;

/* Virtual function table. */
/* ----------------------- */
/* This table contains all information that is the same for all
   objects in the class (e.g. pointers to its virtual functions). */
#if defined(astCLASS)            /* Protected */
typedef struct AstMathMapVtab {

/* Properties (e.g. methods) inherited from the parent class. */
   AstMappingVtab mapping_vtab;  /* Parent class virtual function table */

/* A Unique identifier to determine class membership. */
   AstClassIdentifier id;

/* Properties (e.g. methods) specific to this class. */
   int (* GetSeed)( AstMathMap *, int * );
   int (* GetSimpFI)( AstMathMap *, int * );
   int (* GetSimpIF)( AstMathMap *, int * );
   int (* TestSeed)( AstMathMap *, int * );
   int (* TestSimpFI)( AstMathMap *, int * );
   int (* TestSimpIF)( AstMathMap *, int * );
   void (* ClearSeed)( AstMathMap *, int * );
   void (* ClearSimpFI)( AstMathMap *, int * );
   void (* ClearSimpIF)( AstMathMap *, int * );
   void (* SetSeed)( AstMathMap *, int, int * );
   void (* SetSimpFI)( AstMathMap *, int, int * );
   void (* SetSimpIF)( AstMathMap *, int, int * );
} AstMathMapVtab;

#if defined(THREAD_SAFE)

/* Define a structure holding all data items that are global within this
   class. */
typedef struct AstMathMapGlobals {
   AstMathMapVtab Class_Vtab;
   int Class_Init;
   char GetAttrib_Buff[ 51 ];
} AstMathMapGlobals;

#endif

#endif

/* Function prototypes. */
/* ==================== */
/* Prototypes for standard class functions. */
/* ---------------------------------------- */
astPROTO_CHECK(MathMap)          /* Check class membership */
astPROTO_ISA(MathMap)            /* Test class membership */

/* Constructor. */
#if defined(astCLASS)            /* Protected. */
AstMathMap *astMathMap_( int, int, int, const char *[], int, const char *[],
                         const char *, int *, ...);
#else
AstMathMap *astMathMapId_( int, int, int, const char *[], int, const char *[],
                           const char *, ... )__attribute__((format(printf,7,8)));
#endif

#if defined(astCLASS)            /* Protected */

/* Initialiser. */
AstMathMap *astInitMathMap_( void *, size_t, int, AstMathMapVtab *,
                             const char *, int, int,
                             int, const char *[], int, const char *[], int * );

/* Vtab initialiser. */
void astInitMathMapVtab_( AstMathMapVtab *, const char *, int * );

/* Loader. */
AstMathMap *astLoadMathMap_( void *, size_t, AstMathMapVtab *,
                             const char *, AstChannel *, int * );

/* Thread-safe initialiser for all global data used by this module. */
#if defined(THREAD_SAFE)
void astInitMathMapGlobals_( AstMathMapGlobals * );
#endif

#endif

/* Prototypes for member functions. */
/* -------------------------------- */
#if defined(astCLASS)            /* Protected */
int astGetSeed_( AstMathMap *, int * );
int astGetSimpFI_( AstMathMap *, int * );
int astGetSimpIF_( AstMathMap *, int * );
int astTestSeed_( AstMathMap *, int * );
int astTestSimpFI_( AstMathMap *, int * );
int astTestSimpIF_( AstMathMap *, int * );
void astClearSeed_( AstMathMap *, int * );
void astClearSimpFI_( AstMathMap *, int * );
void astClearSimpIF_( AstMathMap *, int * );
void astSetSeed_( AstMathMap *, int, int * );
void astSetSimpFI_( AstMathMap *, int, int * );
void astSetSimpIF_( AstMathMap *, int, int * );
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
#define astCheckMathMap(this) astINVOKE_CHECK(MathMap,this,0)
#define astVerifyMathMap(this) astINVOKE_CHECK(MathMap,this,1)

/* Test class membership. */
#define astIsAMathMap(this) astINVOKE_ISA(MathMap,this)

/* Constructor. */
#if defined(astCLASS)            /* Protected. */
#define astMathMap astINVOKE(F,astMathMap_)
#else
#define astMathMap astINVOKE(F,astMathMapId_)
#endif

#if defined(astCLASS)            /* Protected */

/* Initialiser. */
#define astInitMathMap(mem,size,init,vtab,name,nin,nout,nfwd,fwd,ninv,inv) \
astINVOKE(O,astInitMathMap_(mem,size,init,vtab,name,nin,nout,nfwd,fwd,ninv,inv,STATUS_PTR))

/* Vtab Initialiser. */
#define astInitMathMapVtab(vtab,name) astINVOKE(V,astInitMathMapVtab_(vtab,name,STATUS_PTR))
/* Loader. */
#define astLoadMathMap(mem,size,vtab,name,channel) \
astINVOKE(O,astLoadMathMap_(mem,size,vtab,name,astCheckChannel(channel),STATUS_PTR))
#endif

/* Interfaces to public member functions. */
/* -------------------------------------- */
/* Here we make use of astCheckMathMap to validate MathMap pointers
   before use. This provides a contextual error report if a pointer
   to the wrong sort of Object is supplied. */
#if defined(astCLASS)            /* Protected */
#define astClearSeed(this) \
astINVOKE(V,astClearSeed_(astCheckMathMap(this),STATUS_PTR))
#define astClearSimpFI(this) \
astINVOKE(V,astClearSimpFI_(astCheckMathMap(this),STATUS_PTR))
#define astClearSimpIF(this) \
astINVOKE(V,astClearSimpIF_(astCheckMathMap(this),STATUS_PTR))
#define astGetSeed(this) \
astINVOKE(V,astGetSeed_(astCheckMathMap(this),STATUS_PTR))
#define astGetSimpFI(this) \
astINVOKE(V,astGetSimpFI_(astCheckMathMap(this),STATUS_PTR))
#define astGetSimpIF(this) \
astINVOKE(V,astGetSimpIF_(astCheckMathMap(this),STATUS_PTR))
#define astSetSeed(this,value) \
astINVOKE(V,astSetSeed_(astCheckMathMap(this),value,STATUS_PTR))
#define astSetSimpFI(this,value) \
astINVOKE(V,astSetSimpFI_(astCheckMathMap(this),value,STATUS_PTR))
#define astSetSimpIF(this,value) \
astINVOKE(V,astSetSimpIF_(astCheckMathMap(this),value,STATUS_PTR))
#define astTestSeed(this) \
astINVOKE(V,astTestSeed_(astCheckMathMap(this),STATUS_PTR))
#define astTestSimpFI(this) \
astINVOKE(V,astTestSimpFI_(astCheckMathMap(this),STATUS_PTR))
#define astTestSimpIF(this) \
astINVOKE(V,astTestSimpIF_(astCheckMathMap(this),STATUS_PTR))
#endif
#endif





