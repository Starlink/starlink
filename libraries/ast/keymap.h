#if !defined( KEYMAP_INCLUDED ) /* Include this file only once */
#define KEYMAP_INCLUDED
/*
*+
*  Name:
*     keymap.h

*  Type:
*     C include file.

*  Purpose:
*     Define the interface to the KeyMap class.

*  Invocation:
*     #include "keymap.h"

*  Description:
*     This include file defines the interface to the KeyMap class and
*     provides the type definitions, function prototypes and macros,
*     etc.  needed to use this class.
*
*     The KeyMap class extends the Object class to represent a set of
*     key/value pairs. Keys are strings, and values can be integer,
*     floating point, string or Object - scalar of vector.

*  Inheritance:
*     The KeyMap class inherits from the Object class.

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
*     13-NOV-2004 (DSB):
*        Original version.
*     5-JUN-2006 (DSB):
*        Added support for single precision entries.
*     7-MAR-2008 (DSB):
*        Added support for pointer ("P") entries.
*-
*/

/* Include files. */
/* ============== */
/* Interface definitions. */
/* ---------------------- */
#include "object.h"              /* Coordinate objects (parent class) */

/* C header files. */
/* --------------- */

/* Macros */
/* ====== */
/* Data type constants: */
#define AST__BADTYPE 0
#define AST__INTTYPE 1
#define AST__DOUBLETYPE 2
#define AST__STRINGTYPE 3
#define AST__OBJECTTYPE 4
#define AST__FLOATTYPE 5
#define AST__POINTERTYPE 6

/* Define constants used to size global arrays in this module. */
#define AST__KEYMAP_GETATTRIB_BUFF_LEN 50       /* Max length of string returned by GetAttrib */
#define AST__KEYMAP_CONVERTVALUE_MAX_STRINGS 50 /* Number of string values to buffer in ConvertValue */
#define AST__KEYMAP_CONVERTVALUE_BUFF_LEN 50    /* Max. characters in result buffer for ConvertValue */
#define AST__KEYMAP_MAPKEY_MAX_STRINGS 50       /* Number of string values to buffer in MapKey */


/* Type Definitions. */
/* ================= */

/* This structure contains information describing a single generic entry in 
   a KeyMap. This structure is extended by other structures to hold data of
   specific data types. */

typedef struct AstMapEntry {
   struct AstMapEntry *next; /* Pointer to next structure in list. */
   const char *key;          /* The name used to identify the entry */
   unsigned long hash;       /* The full width hash value */
   int type;                 /* Data type. */
   int nel;                  /* 0 => scalar, >0 => array with "nel" elements */
   const char *comment;      /* Pointer to a comment for the entry */
} AstMapEntry;

/* KeyMap structure. */
/* ------------------ */
/* This structure contains all information that is unique to each object in
   the class (e.g. its instance variables). */
typedef struct AstKeyMap {

/* Attributes inherited from the parent class. */
   AstObject object;                   /* Parent class structure */

/* Attributes specific to objects in this class. */
   int sizeguess;                      /* Guess at KeyMap size */
   AstMapEntry **table;                /* Hash table containing pointers to 
                                          the KeyMap entries */
   int *nentry;                        /* No. of Entries in each table element */
   int mapsize;                        /* Length of table */
} AstKeyMap;

/* Virtual function table. */
/* ----------------------- */
/* This table contains all information that is the same for all
   objects in the class (e.g. pointers to its virtual functions). */
#if defined(astCLASS)            /* Protected */
typedef struct AstKeyMapVtab {

/* Properties (e.g. methods) inherited from the parent class. */
   AstObjectVtab object_vtab;  /* Parent class virtual function table */

/* Unique flag value to determine class membership. */
   int *check;                   /* Check value */

/* Properties (e.g. methods) specific to this class. */
   void (* MapPut0I)( AstKeyMap *, const char *, int, const char *, int * );
   void (* MapPut0D)( AstKeyMap *, const char *, double, const char *, int * );
   void (* MapPut0F)( AstKeyMap *, const char *, float, const char *, int * );
   void (* MapPut0C)( AstKeyMap *, const char *, const char *, const char *, int * );
   void (* MapPut0A)( AstKeyMap *, const char *, AstObject *, const char *, int * );
   void (* MapPut0P)( AstKeyMap *, const char *, void *, const char *, int * );
   void (* MapPut1I)( AstKeyMap *, const char *, int, int[], const char *, int * );
   void (* MapPut1D)( AstKeyMap *, const char *, int, double[], const char *, int * );
   void (* MapPut1F)( AstKeyMap *, const char *, int, float[], const char *, int * );
   void (* MapPut1C)( AstKeyMap *, const char *, int, const char *[], const char *, int * );
   void (* MapPut1A)( AstKeyMap *, const char *, int, AstObject *[], const char *, int * );
   void (* MapPut1P)( AstKeyMap *, const char *, int, void *[], const char *, int * );
   int (* MapGet0I)( AstKeyMap *, const char *, int *, int * );
   int (* MapGet0D)( AstKeyMap *, const char *, double *, int * );
   int (* MapGet0F)( AstKeyMap *, const char *, float *, int * );
   int (* MapGet0C)( AstKeyMap *, const char *, const char **, int * );
   int (* MapGet0A)( AstKeyMap *, const char *, AstObject **, int * );
   int (* MapGet0P)( AstKeyMap *, const char *, void **, int * );
   int (* MapGet1A)( AstKeyMap *, const char *, int, int *, AstObject **, int * );
   int (* MapGet1P)( AstKeyMap *, const char *, int, int *, void **, int * );
   int (* MapGet1C)( AstKeyMap *, const char *, int, int, int *, char *, int * );
   int (* MapGet1D)( AstKeyMap *, const char *, int, int *, double *, int * );
   int (* MapGet1F)( AstKeyMap *, const char *, int, int *, float *, int * );
   int (* MapGet1I)( AstKeyMap *, const char *, int, int *, int *, int * );
   void (* MapRemove)( AstKeyMap *, const char *, int * );
   int (* MapSize)( AstKeyMap *, int * );
   int (* MapLength)( AstKeyMap *, const char *, int * );
   int (* MapLenC)( AstKeyMap *, const char *, int * );
   int (* MapType)( AstKeyMap *, const char *, int * );
   int (* MapHasKey)( AstKeyMap *, const char *, int * );
   const char *(* MapKey)( AstKeyMap *, int, int * );

   int (* GetSizeGuess)( AstKeyMap *, int * );
   int (* TestSizeGuess)( AstKeyMap *, int * );
   void (* SetSizeGuess)( AstKeyMap *, int, int * );
   void (* ClearSizeGuess)( AstKeyMap *, int * );

} AstKeyMapVtab;

#if defined(THREAD_SAFE) 

/* Define a structure holding all data items that are global within this
   class. */
typedef struct AstKeyMapGlobals {
   AstKeyMapVtab Class_Vtab;
   int Class_Init;
   char GetAttrib_Buff[ AST__KEYMAP_GETATTRIB_BUFF_LEN + 1 ];
   char *ConvertValue_Strings[ AST__KEYMAP_CONVERTVALUE_MAX_STRINGS ];
   int ConvertValue_Istr;
   int ConvertValue_Init;
   char ConvertValue_Buff[ AST__KEYMAP_CONVERTVALUE_BUFF_LEN + 1 ];
   char *MapKey_Strings[ AST__KEYMAP_MAPKEY_MAX_STRINGS ];
   int MapKey_Istr;
   int MapKey_Init; 
} AstKeyMapGlobals;

#endif

#endif

/* Function prototypes. */
/* ==================== */
/* Prototypes for standard class functions. */
/* ---------------------------------------- */
astPROTO_CHECK(KeyMap)          /* Check class membership */
astPROTO_ISA(KeyMap)            /* Test class membership */

/* Constructor. */
#if defined(astCLASS)            /* Protected. */
AstKeyMap *astKeyMap_( const char *, int *, ...);
#else
AstKeyMap *astKeyMapId_( const char *, ... )
#ifdef __GNUC__ /* Check the variable argument list if using GNU compiler */
__attribute__((format(printf,1,2)))
#endif
;
#endif

#if defined(astCLASS)            /* Protected */

/* Initialiser. */
AstKeyMap *astInitKeyMap_( void *, size_t, int, AstKeyMapVtab *, const char *, int * );

/* Vtab initialiser. */
void astInitKeyMapVtab_( AstKeyMapVtab *, const char *, int * );

/* Loader. */
AstKeyMap *astLoadKeyMap_( void *, size_t, AstKeyMapVtab *, const char *, AstChannel *, int * );

/* Thread-safe initialiser for all global data used by this module. */
#if defined(THREAD_SAFE) 
void astInitKeyMapGlobals_( AstKeyMapGlobals * );
#endif

#endif

/* Prototypes for member functions. */
/* -------------------------------- */

#if defined(astCLASS)            /* Protected */
int astMapGet0A_( AstKeyMap *, const char *, AstObject **, int * );
int astMapGet1A_( AstKeyMap *, const char *, int, int *, AstObject **, int * );
void astMapPut1A_( AstKeyMap *, const char *, int, AstObject *[], const char *, int * );
#else
int astMapGet0AId_( AstKeyMap *, const char *, AstObject **, int * );
int astMapGet1AId_( AstKeyMap *, const char *, int, int *, AstObject **, int * );
void astMapPut1AId_( AstKeyMap *, const char *, int, AstObject *[], const char *, int * );
#endif

const char *astMapKey_( AstKeyMap *, int, int * );
int astMapGet0C_( AstKeyMap *, const char *, const char **, int * );
int astMapGet0D_( AstKeyMap *, const char *, double *, int * );
int astMapGet0F_( AstKeyMap *, const char *, float *, int * );
int astMapGet0I_( AstKeyMap *, const char *, int *, int * );
int astMapGet1C_( AstKeyMap *, const char *, int, int, int *, char *, int * );
int astMapGet1D_( AstKeyMap *, const char *, int, int *, double *, int * );
int astMapGet1F_( AstKeyMap *, const char *, int, int *, float *, int * );
int astMapGet1I_( AstKeyMap *, const char *, int, int *, int *, int * );
int astMapHasKey_( AstKeyMap *, const char *, int * );
int astMapLength_( AstKeyMap *, const char *, int * );
int astMapLenC_( AstKeyMap *, const char *, int * );
int astMapSize_( AstKeyMap *, int * );
int astMapType_( AstKeyMap *, const char *, int * );
void astMapPut0A_( AstKeyMap *, const char *, AstObject *, const char *, int * );
void astMapPut0C_( AstKeyMap *, const char *, const char *, const char *, int * );
void astMapPut0D_( AstKeyMap *, const char *, double, const char *, int * );
void astMapPut0F_( AstKeyMap *, const char *, float, const char *, int * );
void astMapPut0I_( AstKeyMap *, const char *, int, const char *, int * );
void astMapPut1C_( AstKeyMap *, const char *, int, const char *[], const char *, int * );
void astMapPut1D_( AstKeyMap *, const char *, int, double *, const char *, int * );
void astMapPut1F_( AstKeyMap *, const char *, int, float *, const char *, int * );
void astMapPut1I_( AstKeyMap *, const char *, int, int *, const char *, int * );
void astMapRemove_( AstKeyMap *, const char *, int * );
int astMapGet0P_( AstKeyMap *, const char *, void **, int * );
int astMapGet1P_( AstKeyMap *, const char *, int, int *, void **, int * );
void astMapPut0P_( AstKeyMap *, const char *, void *, const char *, int * );
void astMapPut1P_( AstKeyMap *, const char *, int, void *[], const char *, int * );

#if defined(astCLASS)            /* Protected */
int astGetSizeGuess_( AstKeyMap *, int * );
int astTestSizeGuess_( AstKeyMap *, int * );
void astSetSizeGuess_( AstKeyMap *, int, int * );
void astClearSizeGuess_( AstKeyMap *, int * );
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
#define astCheckKeyMap(this) astINVOKE_CHECK(KeyMap,this)

/* Test class membership. */
#define astIsAKeyMap(this) astINVOKE_ISA(KeyMap,this)

/* Constructor. */
#if defined(astCLASS)            /* Protected. */
#define astKeyMap astINVOKE(F,astKeyMap_)
#else
#define astKeyMap astINVOKE(F,astKeyMapId_)
#endif

#if defined(astCLASS)            /* Protected */

/* Initialiser. */
#define astInitKeyMap(mem,size,init,vtab,name) astINVOKE(O,astInitKeyMap_(mem,size,init,vtab,name,STATUS_PTR))

/* Vtab Initialiser. */
#define astInitKeyMapVtab(vtab,name) astINVOKE(V,astInitKeyMapVtab_(vtab,name,STATUS_PTR))
/* Loader. */
#define astLoadKeyMap(mem,size,vtab,name,channel) \
astINVOKE(O,astLoadKeyMap_(mem,size,vtab,name,astCheckChannel(channel),STATUS_PTR))
#endif

/* Interfaces to public member functions. */
/* -------------------------------------- */
/* Here we make use of astCheckKeyMap to validate KeyMap pointers
   before use.  This provides a contextual error report if a pointer
   to the wrong sort of Object is supplied. */
#define astMapPut0I(this,key,value,comment) astINVOKE(V,astMapPut0I_(astCheckKeyMap(this),key,value,comment,STATUS_PTR))
#define astMapPut0D(this,key,value,comment) astINVOKE(V,astMapPut0D_(astCheckKeyMap(this),key,value,comment,STATUS_PTR))
#define astMapPut0F(this,key,value,comment) astINVOKE(V,astMapPut0F_(astCheckKeyMap(this),key,value,comment,STATUS_PTR))
#define astMapPut0C(this,key,value,comment) astINVOKE(V,astMapPut0C_(astCheckKeyMap(this),key,value,comment,STATUS_PTR))
#define astMapPut0A(this,key,value,comment) astINVOKE(V,astMapPut0A_(astCheckKeyMap(this),key,astCheckObject(value),comment,STATUS_PTR))
#define astMapPut0P(this,key,value,comment) astINVOKE(V,astMapPut0P_(astCheckKeyMap(this),key,value,comment,STATUS_PTR))
#define astMapPut1I(this,key,size,value,comment) astINVOKE(V,astMapPut1I_(astCheckKeyMap(this),key,size,value,comment,STATUS_PTR))
#define astMapPut1D(this,key,size,value,comment) astINVOKE(V,astMapPut1D_(astCheckKeyMap(this),key,size,value,comment,STATUS_PTR))
#define astMapPut1F(this,key,size,value,comment) astINVOKE(V,astMapPut1F_(astCheckKeyMap(this),key,size,value,comment,STATUS_PTR))
#define astMapPut1C(this,key,size,value,comment) astINVOKE(V,astMapPut1C_(astCheckKeyMap(this),key,size,value,comment,STATUS_PTR))
#define astMapGet0I(this,key,value) astINVOKE(V,astMapGet0I_(astCheckKeyMap(this),key,value,STATUS_PTR))
#define astMapGet0D(this,key,value) astINVOKE(V,astMapGet0D_(astCheckKeyMap(this),key,value,STATUS_PTR))
#define astMapGet0F(this,key,value) astINVOKE(V,astMapGet0F_(astCheckKeyMap(this),key,value,STATUS_PTR))
#define astMapGet0C(this,key,value) astINVOKE(V,astMapGet0C_(astCheckKeyMap(this),key,value,STATUS_PTR))
#define astMapGet1I(this,key,mxval,nval,value) astINVOKE(V,astMapGet1I_(astCheckKeyMap(this),key,mxval,nval,value,STATUS_PTR))
#define astMapGet1D(this,key,mxval,nval,value) astINVOKE(V,astMapGet1D_(astCheckKeyMap(this),key,mxval,nval,value,STATUS_PTR))
#define astMapGet1F(this,key,mxval,nval,value) astINVOKE(V,astMapGet1F_(astCheckKeyMap(this),key,mxval,nval,value,STATUS_PTR))
#define astMapGet1C(this,key,l,mxval,nval,value) astINVOKE(V,astMapGet1C_(astCheckKeyMap(this),key,l,mxval,nval,value,STATUS_PTR))
#define astMapRemove(this,key) astINVOKE(V,astMapRemove_(astCheckKeyMap(this),key,STATUS_PTR))
#define astMapSize(this) astINVOKE(V,astMapSize_(astCheckKeyMap(this),STATUS_PTR))
#define astMapLength(this,key) astINVOKE(V,astMapLength_(astCheckKeyMap(this),key,STATUS_PTR))
#define astMapLenC(this,key) astINVOKE(V,astMapLenC_(astCheckKeyMap(this),key,STATUS_PTR))
#define astMapHasKey(this,key) astINVOKE(V,astMapHasKey_(astCheckKeyMap(this),key,STATUS_PTR))
#define astMapKey(this,index) astINVOKE(V,astMapKey_(astCheckKeyMap(this),index,STATUS_PTR))
#define astMapType(this,key) astINVOKE(V,astMapType_(astCheckKeyMap(this),key,STATUS_PTR))
#define astMapGet0P(this,key,value) astINVOKE(V,astMapGet0P_(astCheckKeyMap(this),key,value,STATUS_PTR))
#define astMapGet1P(this,key,mxval,nval,value) astINVOKE(V,astMapGet1P_(astCheckKeyMap(this),key,mxval,nval,value,STATUS_PTR))
#define astMapPut1P(this,key,size,value,comment) astINVOKE(V,astMapPut1P_(astCheckKeyMap(this),key,size,value,comment,STATUS_PTR))

#if defined(astCLASS)            /* Protected */
#define astMapGet0A(this,key,value) astINVOKE(V,astMapGet0A_(astCheckKeyMap(this),key,(AstObject **)(value),STATUS_PTR))
#define astMapGet1A(this,key,mxval,nval,value) astINVOKE(V,astMapGet1A_(astCheckKeyMap(this),key,mxval,nval,(AstObject **)(value),STATUS_PTR))
#define astMapPut1A(this,key,size,value,comment) astINVOKE(V,astMapPut1A_(astCheckKeyMap(this),key,size,value,comment,STATUS_PTR))

#define astClearSizeGuess(this) \
astINVOKE(V,astClearSizeGuess_(astCheckKeyMap(this),STATUS_PTR))
#define astGetSizeGuess(this) \
astINVOKE(V,astGetSizeGuess_(astCheckKeyMap(this),STATUS_PTR))
#define astSetSizeGuess(this,sizeguess) \
astINVOKE(V,astSetSizeGuess_(astCheckKeyMap(this),sizeguess,STATUS_PTR))
#define astTestSizeGuess(this) \
astINVOKE(V,astTestSizeGuess_(astCheckKeyMap(this),STATUS_PTR))

#else
#define astMapGet0A(this,key,value) astINVOKE(V,astMapGet0AId_(astCheckKeyMap(this),key,(AstObject **)(value),STATUS_PTR))
#define astMapGet1A(this,key,mxval,nval,value) astINVOKE(V,astMapGet1AId_(astCheckKeyMap(this),key,mxval,nval,(AstObject **)(value),STATUS_PTR))
#define astMapPut1A(this,key,size,value,comment) astINVOKE(V,astMapPut1AId_(astCheckKeyMap(this),key,size,value,comment,STATUS_PTR))
#endif

#endif





