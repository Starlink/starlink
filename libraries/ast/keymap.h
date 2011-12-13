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
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

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
#define AST__SINTTYPE 7
#define AST__UNDEFTYPE 8
#define AST__BYTETYPE 9

/* Define constants used to size global arrays in this module. */
#define AST__KEYMAP_GETATTRIB_BUFF_LEN 50       /* Max length of string returned by GetAttrib */
#define AST__KEYMAP_CONVERTVALUE_MAX_STRINGS 50 /* Number of string values to buffer in ConvertValue */
#define AST__KEYMAP_CONVERTVALUE_BUFF_LEN 50    /* Max. characters in result buffer for ConvertValue */
#define AST__KEYMAP_MAPKEY_MAX_STRINGS 50       /* Number of string values to buffer in MapKey */

/* Define a dummy __attribute__ macro for use on non-GNU compilers. */
#ifndef __GNUC__
#  define  __attribute__(x)  /*NOTHING*/
#endif

/* Maximum key length when using case insensitive keymaps */
#define AST__MXKEYLEN 200

/* Type Definitions. */
/* ================= */

/* This structure contains information describing a single generic entry in
   a KeyMap. This structure is extended by other structures to hold data of
   specific data types. */

typedef struct AstMapEntry {
   struct AstMapEntry *next; /* Pointer to next structure in unsorted list. */
   const char *key;          /* The name used to identify the entry */
   unsigned long hash;       /* The full width hash value */
   int type;                 /* Data type. */
   int nel;                  /* 0 => scalar, >0 => array with "nel" elements */
   const char *comment;      /* Pointer to a comment for the entry */
   int defined;              /* Non-zero if the entry value is defined */
   struct AstMapEntry *snext; /* Pointer to next structure in sorted list. */
   struct AstMapEntry *sprev; /* Pointer to previous structure in sorted list. */
   int member;               /* No. of entries added to KeyMap prior to this one  */
   int sortby;               /* Used for comunnication with qsort function */
} AstMapEntry;

/* KeyMap structure. */
/* ------------------ */
/* This structure contains all information that is unique to each object in
   the class (e.g. its instance variables). */
typedef struct AstKeyMap {

/* Attributes inherited from the parent class. */
   AstObject object;               /* Parent class structure */

/* Attributes specific to objects in this class. */
   int sizeguess;                  /* Guess at KeyMap size */
   AstMapEntry **table;            /* Hash table containing pointers to
                                      the KeyMap entries */
   int *nentry;                    /* No. of Entries in each table element */
   int mapsize;                    /* Length of table */
   int keycase;                    /* Are keys case sensitive? */
   int keyerror;                   /* Report error if no key? */
   int maplocked;                  /* Prevent addition of new entries? */
   int sortby;                     /* How the keys should be sorted */
   AstMapEntry *first;             /* Pointer to first structure in sorted list. */
   int nsorted;                    /* Length of sorted list */
   int member_count;               /* Total no. of entries ever added to keyMap */
   AstMapEntry *firstA;            /* Pointer to first "AST object"-type entry */
   int iter_itab;                  /* Next hash table entry to return */
   AstMapEntry *iter_entry;        /* Next entry to return */
} AstKeyMap;

/* Virtual function table. */
/* ----------------------- */
/* This table contains all information that is the same for all
   objects in the class (e.g. pointers to its virtual functions). */
#if defined(astCLASS)            /* Protected */
typedef struct AstKeyMapVtab {

/* Properties (e.g. methods) inherited from the parent class. */
   AstObjectVtab object_vtab;  /* Parent class virtual function table */

/* A Unique identifier to determine class membership. */
   AstClassIdentifier id;

/* Properties (e.g. methods) specific to this class. */
   void (* MapPut0I)( AstKeyMap *, const char *, int, const char *, int * );
   void (* MapPut0D)( AstKeyMap *, const char *, double, const char *, int * );
   void (* MapPut0B)( AstKeyMap *, const char *, unsigned char, const char *, int * );
   void (* MapPut0S)( AstKeyMap *, const char *, short int, const char *, int * );
   void (* MapPut0F)( AstKeyMap *, const char *, float, const char *, int * );
   void (* MapPut0C)( AstKeyMap *, const char *, const char *, const char *, int * );
   void (* MapPut0A)( AstKeyMap *, const char *, AstObject *, const char *, int * );
   void (* MapPut0P)( AstKeyMap *, const char *, void *, const char *, int * );
   void (* MapPut1I)( AstKeyMap *, const char *, int, const int[], const char *, int * );
   void (* MapPut1D)( AstKeyMap *, const char *, int, const double[], const char *, int * );
   void (* MapPut1B)( AstKeyMap *, const char *, int, const unsigned char[], const char *, int * );
   void (* MapPut1S)( AstKeyMap *, const char *, int, const short int[], const char *, int * );
   void (* MapPut1F)( AstKeyMap *, const char *, int, const float[], const char *, int * );
   void (* MapPut1C)( AstKeyMap *, const char *, int, const char *const [], const char *, int * );
   void (* MapPut1A)( AstKeyMap *, const char *, int, AstObject *const [], const char *, int * );
   void (* MapPut1P)( AstKeyMap *, const char *, int, void *const [], const char *, int * );
   void (* MapPutU)( AstKeyMap *, const char *, const char *, int * );
   int (* MapGet0I)( AstKeyMap *, const char *, int *, int * );
   int (* MapGet0D)( AstKeyMap *, const char *, double *, int * );
   int (* MapGet0B)( AstKeyMap *, const char *, unsigned char *, int * );
   int (* MapGet0S)( AstKeyMap *, const char *, short int *, int * );
   int (* MapGet0F)( AstKeyMap *, const char *, float *, int * );
   int (* MapGet0C)( AstKeyMap *, const char *, const char **, int * );
   int (* MapGet0A)( AstKeyMap *, const char *, AstObject **, int * );
   int (* MapGet0P)( AstKeyMap *, const char *, void **, int * );
   int (* MapGet1A)( AstKeyMap *, const char *, int, int *, AstObject **, int * );
   int (* MapGet1P)( AstKeyMap *, const char *, int, int *, void **, int * );
   int (* MapGet1C)( AstKeyMap *, const char *, int, int, int *, char *, int * );
   int (* MapGet1D)( AstKeyMap *, const char *, int, int *, double *, int * );
   int (* MapGet1B)( AstKeyMap *, const char *, int, int *, unsigned char *, int * );
   int (* MapGet1S)( AstKeyMap *, const char *, int, int *, short int *, int * );
   int (* MapGet1F)( AstKeyMap *, const char *, int, int *, float *, int * );
   int (* MapGet1I)( AstKeyMap *, const char *, int, int *, int *, int * );
   int (* MapGetElemA)( AstKeyMap *, const char *, int, AstObject **, int * );
   int (* MapGetElemP)( AstKeyMap *, const char *, int, void **, int * );
   int (* MapGetElemC)( AstKeyMap *, const char *, int, int, char *, int * );
   int (* MapGetElemD)( AstKeyMap *, const char *, int, double *, int * );
   int (* MapGetElemB)( AstKeyMap *, const char *, int, unsigned char *, int * );
   int (* MapGetElemS)( AstKeyMap *, const char *, int, short int *, int * );
   int (* MapGetElemF)( AstKeyMap *, const char *, int, float *, int * );
   int (* MapGetElemI)( AstKeyMap *, const char *, int, int *, int * );
   void (* MapPutElemA)( AstKeyMap *, const char *, int, AstObject *, int * );
   void (* MapPutElemP)( AstKeyMap *, const char *, int, void *, int * );
   void (* MapPutElemC)( AstKeyMap *, const char *, int, const char *, int * );
   void (* MapPutElemD)( AstKeyMap *, const char *, int, double, int * );
   void (* MapPutElemB)( AstKeyMap *, const char *, int, unsigned char, int * );
   void (* MapPutElemS)( AstKeyMap *, const char *, int, short int, int * );
   void (* MapPutElemF)( AstKeyMap *, const char *, int, float, int * );
   void (* MapPutElemI)( AstKeyMap *, const char *, int, int, int * );
   void (* MapRemove)( AstKeyMap *, const char *, int * );
   void (* MapRename)( AstKeyMap *, const char *, const char *, int * );
   void (* MapCopy)( AstKeyMap *, AstKeyMap *, int * );
   int (* MapSize)( AstKeyMap *, int * );
   int (* MapLength)( AstKeyMap *, const char *, int * );
   int (* MapLenC)( AstKeyMap *, const char *, int * );
   int (* MapType)( AstKeyMap *, const char *, int * );
   int (* MapHasKey)( AstKeyMap *, const char *, int * );
   const char *(* MapIterate)( AstKeyMap *, int, int * );
   const char *(* MapKey)( AstKeyMap *, int, int * );

   int (* GetSizeGuess)( AstKeyMap *, int * );
   int (* TestSizeGuess)( AstKeyMap *, int * );
   void (* SetSizeGuess)( AstKeyMap *, int, int * );
   void (* ClearSizeGuess)( AstKeyMap *, int * );

   int (* GetMapLocked)( AstKeyMap *, int * );
   int (* TestMapLocked)( AstKeyMap *, int * );
   void (* ClearMapLocked)( AstKeyMap *, int * );
   void (* SetMapLocked)( AstKeyMap *, int, int * );

   int (* GetKeyError)( AstKeyMap *, int * );
   int (* TestKeyError)( AstKeyMap *, int * );
   void (* ClearKeyError)( AstKeyMap *, int * );
   void (* SetKeyError)( AstKeyMap *, int, int * );

   int (* GetKeyCase)( AstKeyMap *, int * );
   int (* TestKeyCase)( AstKeyMap *, int * );
   void (* ClearKeyCase)( AstKeyMap *, int * );
   void (* SetKeyCase)( AstKeyMap *, int, int * );

   int (* GetSortBy)( AstKeyMap *, int * );
   int (* TestSortBy)( AstKeyMap *, int * );
   void (* ClearSortBy)( AstKeyMap *, int * );
   void (* SetSortBy)( AstKeyMap *, int, int * );

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
AstKeyMap *astKeyMapId_( const char *, ... )__attribute__((format(printf,1,2)));
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
void astMapPut1A_( AstKeyMap *, const char *, int, AstObject *const [], const char *, int * );
int astMapGetElemA_( AstKeyMap *, const char *, int, AstObject **, int * );
#else
int astMapGet0AId_( AstKeyMap *, const char *, AstObject **, int * );
int astMapGet1AId_( AstKeyMap *, const char *, int, int *, AstObject **, int * );
void astMapPut1AId_( AstKeyMap *, const char *, int, AstObject *const [], const char *, int * );
int astMapGetElemAId_( AstKeyMap *, const char *, int, AstObject **, int * );
#endif

const char *astMapKey_( AstKeyMap *, int, int * );


int astMapGet0B_( AstKeyMap *, const char *, unsigned char *, int * );
int astMapGet0C_( AstKeyMap *, const char *, const char **, int * );
int astMapGet0D_( AstKeyMap *, const char *, double *, int * );
int astMapGet0F_( AstKeyMap *, const char *, float *, int * );
int astMapGet0I_( AstKeyMap *, const char *, int *, int * );
int astMapGet0P_( AstKeyMap *, const char *, void **, int * );
int astMapGet0S_( AstKeyMap *, const char *, short int *, int * );
int astMapGet1B_( AstKeyMap *, const char *, int, int *, unsigned char *, int * );
int astMapGet1C_( AstKeyMap *, const char *, int, int, int *, char *, int * );
int astMapGet1D_( AstKeyMap *, const char *, int, int *, double *, int * );
int astMapGet1F_( AstKeyMap *, const char *, int, int *, float *, int * );
int astMapGet1I_( AstKeyMap *, const char *, int, int *, int *, int * );
int astMapGet1P_( AstKeyMap *, const char *, int, int *, void **, int * );
int astMapGet1S_( AstKeyMap *, const char *, int, int *, short int *, int * );
int astMapGetElemB_( AstKeyMap *, const char *, int, unsigned char *, int * );
int astMapGetElemC_( AstKeyMap *, const char *, int, int, char *, int * );
int astMapGetElemD_( AstKeyMap *, const char *, int, double *, int * );
int astMapGetElemF_( AstKeyMap *, const char *, int, float *, int * );
int astMapGetElemI_( AstKeyMap *, const char *, int, int *, int * );
int astMapGetElemP_( AstKeyMap *, const char *, int, void **, int * );
int astMapGetElemS_( AstKeyMap *, const char *, int, short int *, int * );
int astMapHasKey_( AstKeyMap *, const char *, int * );
int astMapLenC_( AstKeyMap *, const char *, int * );
int astMapLength_( AstKeyMap *, const char *, int * );
int astMapSize_( AstKeyMap *, int * );
int astMapType_( AstKeyMap *, const char *, int * );
void astMapCopy_( AstKeyMap *, AstKeyMap *, int * );
void astMapPut0A_( AstKeyMap *, const char *, AstObject *, const char *, int * );
void astMapPut0B_( AstKeyMap *, const char *, unsigned char, const char *, int * );
void astMapPut0C_( AstKeyMap *, const char *, const char *, const char *, int * );
void astMapPut0D_( AstKeyMap *, const char *, double, const char *, int * );
void astMapPut0F_( AstKeyMap *, const char *, float, const char *, int * );
void astMapPut0I_( AstKeyMap *, const char *, int, const char *, int * );
void astMapPut0P_( AstKeyMap *, const char *, void *, const char *, int * );
void astMapPut0S_( AstKeyMap *, const char *, short int, const char *, int * );
void astMapPut1B_( AstKeyMap *, const char *, int, const unsigned char[], const char *, int * );
void astMapPut1C_( AstKeyMap *, const char *, int, const char *const [], const char *, int * );
void astMapPut1D_( AstKeyMap *, const char *, int, const double *, const char *, int * );
void astMapPut1F_( AstKeyMap *, const char *, int, const float *, const char *, int * );
void astMapPut1I_( AstKeyMap *, const char *, int, const int *, const char *, int * );
void astMapPut1P_( AstKeyMap *, const char *, int, void *const [], const char *, int * );
void astMapPut1S_( AstKeyMap *, const char *, int, const short int *, const char *, int * );
void astMapPutElemA_( AstKeyMap *, const char *, int, AstObject *, int * );
void astMapPutElemB_( AstKeyMap *, const char *, int, unsigned char, int * );
void astMapPutElemC_( AstKeyMap *, const char *, int, const char *, int * );
void astMapPutElemD_( AstKeyMap *, const char *, int, double, int * );
void astMapPutElemF_( AstKeyMap *, const char *, int, float, int * );
void astMapPutElemI_( AstKeyMap *, const char *, int, int, int * );
void astMapPutElemP_( AstKeyMap *, const char *, int, void *, int * );
void astMapPutElemS_( AstKeyMap *, const char *, int, short int, int * );
void astMapPutU_( AstKeyMap *, const char *, const char *, int * );
void astMapRemove_( AstKeyMap *, const char *, int * );
void astMapRename_( AstKeyMap *, const char *, const char *, int * );

#if defined(astCLASS)            /* Protected */
const char *astMapIterate_( AstKeyMap *, int, int * );

int astGetSizeGuess_( AstKeyMap *, int * );
int astTestSizeGuess_( AstKeyMap *, int * );
void astSetSizeGuess_( AstKeyMap *, int, int * );
void astClearSizeGuess_( AstKeyMap *, int * );

int astGetKeyError_( AstKeyMap *, int * );
int astTestKeyError_( AstKeyMap *, int * );
void astSetKeyError_( AstKeyMap *, int, int * );
void astClearKeyError_( AstKeyMap *, int * );

int astGetKeyCase_( AstKeyMap *, int * );
int astTestKeyCase_( AstKeyMap *, int * );
void astSetKeyCase_( AstKeyMap *, int, int * );
void astClearKeyCase_( AstKeyMap *, int * );

int astGetSortBy_( AstKeyMap *, int * );
int astTestSortBy_( AstKeyMap *, int * );
void astSetSortBy_( AstKeyMap *, int, int * );
void astClearSortBy_( AstKeyMap *, int * );

int astGetMapLocked_( AstKeyMap *, int * );
int astTestMapLocked_( AstKeyMap *, int * );
void astSetMapLocked_( AstKeyMap *, int, int * );
void astClearMapLocked_( AstKeyMap *, int * );
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
#define astCheckKeyMap(this) astINVOKE_CHECK(KeyMap,this,0)
#define astVerifyKeyMap(this) astINVOKE_CHECK(KeyMap,this,1)

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
#define astMapPutU(this,key,comment) astINVOKE(V,astMapPutU_(astCheckKeyMap(this),key,comment,STATUS_PTR))
#define astMapPut0I(this,key,value,comment) astINVOKE(V,astMapPut0I_(astCheckKeyMap(this),key,value,comment,STATUS_PTR))
#define astMapPut0B(this,key,value,comment) astINVOKE(V,astMapPut0B_(astCheckKeyMap(this),key,value,comment,STATUS_PTR))
#define astMapPut0S(this,key,value,comment) astINVOKE(V,astMapPut0S_(astCheckKeyMap(this),key,value,comment,STATUS_PTR))
#define astMapPut0D(this,key,value,comment) astINVOKE(V,astMapPut0D_(astCheckKeyMap(this),key,value,comment,STATUS_PTR))
#define astMapPut0F(this,key,value,comment) astINVOKE(V,astMapPut0F_(astCheckKeyMap(this),key,value,comment,STATUS_PTR))
#define astMapPut0C(this,key,value,comment) astINVOKE(V,astMapPut0C_(astCheckKeyMap(this),key,value,comment,STATUS_PTR))
#define astMapPut0A(this,key,value,comment) astINVOKE(V,astMapPut0A_(astCheckKeyMap(this),key,astCheckObject(value),comment,STATUS_PTR))
#define astMapPut0P(this,key,value,comment) astINVOKE(V,astMapPut0P_(astCheckKeyMap(this),key,value,comment,STATUS_PTR))
#define astMapPut1I(this,key,size,value,comment) astINVOKE(V,astMapPut1I_(astCheckKeyMap(this),key,size,value,comment,STATUS_PTR))
#define astMapPut1B(this,key,size,value,comment) astINVOKE(V,astMapPut1B_(astCheckKeyMap(this),key,size,value,comment,STATUS_PTR))
#define astMapPut1S(this,key,size,value,comment) astINVOKE(V,astMapPut1S_(astCheckKeyMap(this),key,size,value,comment,STATUS_PTR))
#define astMapPut1D(this,key,size,value,comment) astINVOKE(V,astMapPut1D_(astCheckKeyMap(this),key,size,value,comment,STATUS_PTR))
#define astMapPut1F(this,key,size,value,comment) astINVOKE(V,astMapPut1F_(astCheckKeyMap(this),key,size,value,comment,STATUS_PTR))
#define astMapPut1C(this,key,size,value,comment) astINVOKE(V,astMapPut1C_(astCheckKeyMap(this),key,size,value,comment,STATUS_PTR))
#define astMapGet0I(this,key,value) astINVOKE(V,astMapGet0I_(astCheckKeyMap(this),key,value,STATUS_PTR))
#define astMapGet0B(this,key,value) astINVOKE(V,astMapGet0B_(astCheckKeyMap(this),key,value,STATUS_PTR))
#define astMapGet0S(this,key,value) astINVOKE(V,astMapGet0S_(astCheckKeyMap(this),key,value,STATUS_PTR))
#define astMapGet0D(this,key,value) astINVOKE(V,astMapGet0D_(astCheckKeyMap(this),key,value,STATUS_PTR))
#define astMapGet0F(this,key,value) astINVOKE(V,astMapGet0F_(astCheckKeyMap(this),key,value,STATUS_PTR))
#define astMapGet0C(this,key,value) astINVOKE(V,astMapGet0C_(astCheckKeyMap(this),key,value,STATUS_PTR))
#define astMapGet1I(this,key,mxval,nval,value) astINVOKE(V,astMapGet1I_(astCheckKeyMap(this),key,mxval,nval,value,STATUS_PTR))
#define astMapGet1B(this,key,mxval,nval,value) astINVOKE(V,astMapGet1B_(astCheckKeyMap(this),key,mxval,nval,value,STATUS_PTR))
#define astMapGet1S(this,key,mxval,nval,value) astINVOKE(V,astMapGet1S_(astCheckKeyMap(this),key,mxval,nval,value,STATUS_PTR))
#define astMapGet1D(this,key,mxval,nval,value) astINVOKE(V,astMapGet1D_(astCheckKeyMap(this),key,mxval,nval,value,STATUS_PTR))
#define astMapGet1F(this,key,mxval,nval,value) astINVOKE(V,astMapGet1F_(astCheckKeyMap(this),key,mxval,nval,value,STATUS_PTR))
#define astMapGet1C(this,key,l,mxval,nval,value) astINVOKE(V,astMapGet1C_(astCheckKeyMap(this),key,l,mxval,nval,value,STATUS_PTR))
#define astMapGetElemI(this,key,elem,value) astINVOKE(V,astMapGetElemI_(astCheckKeyMap(this),key,elem,value,STATUS_PTR))
#define astMapGetElemB(this,key,elem,value) astINVOKE(V,astMapGetElemB_(astCheckKeyMap(this),key,elem,value,STATUS_PTR))
#define astMapGetElemS(this,key,elem,value) astINVOKE(V,astMapGetElemS_(astCheckKeyMap(this),key,elem,value,STATUS_PTR))
#define astMapGetElemD(this,key,elem,value) astINVOKE(V,astMapGetElemD_(astCheckKeyMap(this),key,elem,value,STATUS_PTR))
#define astMapGetElemF(this,key,elem,value) astINVOKE(V,astMapGetElemF_(astCheckKeyMap(this),key,elem,value,STATUS_PTR))
#define astMapGetElemC(this,key,l,elem,value) astINVOKE(V,astMapGetElemC_(astCheckKeyMap(this),key,l,elem,value,STATUS_PTR))
#define astMapGetElemP(this,key,elem,value) astINVOKE(V,astMapGetElemP_(astCheckKeyMap(this),key,elem,value,STATUS_PTR))
#define astMapPutElemA(this,key,elem,value) astINVOKE(V,astMapPutElemA_(astCheckKeyMap(this),key,elem,astCheckObject(value),STATUS_PTR))
#define astMapPutElemI(this,key,elem,value) astINVOKE(V,astMapPutElemI_(astCheckKeyMap(this),key,elem,value,STATUS_PTR))
#define astMapPutElemB(this,key,elem,value) astINVOKE(V,astMapPutElemB_(astCheckKeyMap(this),key,elem,value,STATUS_PTR))
#define astMapPutElemS(this,key,elem,value) astINVOKE(V,astMapPutElemS_(astCheckKeyMap(this),key,elem,value,STATUS_PTR))
#define astMapPutElemD(this,key,elem,value) astINVOKE(V,astMapPutElemD_(astCheckKeyMap(this),key,elem,value,STATUS_PTR))
#define astMapPutElemF(this,key,elem,value) astINVOKE(V,astMapPutElemF_(astCheckKeyMap(this),key,elem,value,STATUS_PTR))
#define astMapPutElemC(this,key,elem,value) astINVOKE(V,astMapPutElemC_(astCheckKeyMap(this),key,elem,value,STATUS_PTR))
#define astMapPutElemP(this,key,elem,value) astINVOKE(V,astMapPutElemP_(astCheckKeyMap(this),key,elem,value,STATUS_PTR))
#define astMapRemove(this,key) astINVOKE(V,astMapRemove_(astCheckKeyMap(this),key,STATUS_PTR))
#define astMapRename(this,oldkey,newkey) astINVOKE(V,astMapRename_(astCheckKeyMap(this),oldkey,newkey,STATUS_PTR))
#define astMapCopy(this,that) astINVOKE(V,astMapCopy_(astCheckKeyMap(this),astCheckKeyMap(that),STATUS_PTR))
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
#define astMapGetElemA(this,key,elem,value) astINVOKE(V,astMapGetElemA_(astCheckKeyMap(this),key,elem,(AstObject **)(value),STATUS_PTR))
#define astMapIterate(this,reset) astINVOKE(V,astMapIterate_(astCheckKeyMap(this),reset,STATUS_PTR))

#define astClearSizeGuess(this) \
astINVOKE(V,astClearSizeGuess_(astCheckKeyMap(this),STATUS_PTR))
#define astGetSizeGuess(this) \
astINVOKE(V,astGetSizeGuess_(astCheckKeyMap(this),STATUS_PTR))
#define astSetSizeGuess(this,sizeguess) \
astINVOKE(V,astSetSizeGuess_(astCheckKeyMap(this),sizeguess,STATUS_PTR))
#define astTestSizeGuess(this) \
astINVOKE(V,astTestSizeGuess_(astCheckKeyMap(this),STATUS_PTR))

#define astClearKeyError(this) \
astINVOKE(V,astClearKeyError_(astCheckKeyMap(this),STATUS_PTR))
#define astGetKeyError(this) \
astINVOKE(V,astGetKeyError_(astCheckKeyMap(this),STATUS_PTR))
#define astSetKeyError(this,keyerror) \
astINVOKE(V,astSetKeyError_(astCheckKeyMap(this),keyerror,STATUS_PTR))
#define astTestKeyError(this) \
astINVOKE(V,astTestKeyError_(astCheckKeyMap(this),STATUS_PTR))

#define astClearKeyCase(this) \
astINVOKE(V,astClearKeyCase_(astCheckKeyMap(this),STATUS_PTR))
#define astGetKeyCase(this) \
astINVOKE(V,astGetKeyCase_(astCheckKeyMap(this),STATUS_PTR))
#define astSetKeyCase(this,keycase) \
astINVOKE(V,astSetKeyCase_(astCheckKeyMap(this),keycase,STATUS_PTR))
#define astTestKeyCase(this) \
astINVOKE(V,astTestKeyCase_(astCheckKeyMap(this),STATUS_PTR))

#define astClearSortBy(this) \
astINVOKE(V,astClearSortBy_(astCheckKeyMap(this),STATUS_PTR))
#define astGetSortBy(this) \
astINVOKE(V,astGetSortBy_(astCheckKeyMap(this),STATUS_PTR))
#define astSetSortBy(this,sortby) \
astINVOKE(V,astSetSortBy_(astCheckKeyMap(this),sortby,STATUS_PTR))
#define astTestSortBy(this) \
astINVOKE(V,astTestSortBy_(astCheckKeyMap(this),STATUS_PTR))

#define astClearMapLocked(this) \
astINVOKE(V,astClearMapLocked_(astCheckKeyMap(this),STATUS_PTR))
#define astGetMapLocked(this) \
astINVOKE(V,astGetMapLocked_(astCheckKeyMap(this),STATUS_PTR))
#define astSetMapLocked(this,maplocked) \
astINVOKE(V,astSetMapLocked_(astCheckKeyMap(this),maplocked,STATUS_PTR))
#define astTestMapLocked(this) \
astINVOKE(V,astTestMapLocked_(astCheckKeyMap(this),STATUS_PTR))


#else
#define astMapGet0A(this,key,value) astINVOKE(V,astMapGet0AId_(astCheckKeyMap(this),key,(AstObject **)(value),STATUS_PTR))
#define astMapGet1A(this,key,mxval,nval,value) astINVOKE(V,astMapGet1AId_(astCheckKeyMap(this),key,mxval,nval,(AstObject **)(value),STATUS_PTR))
#define astMapPut1A(this,key,size,value,comment) astINVOKE(V,astMapPut1AId_(astCheckKeyMap(this),key,size,value,comment,STATUS_PTR))
#define astMapGetElemA(this,key,elem,value) astINVOKE(V,astMapGetElemAId_(astCheckKeyMap(this),key,elem,(AstObject **)(value),STATUS_PTR))
#endif

#endif

