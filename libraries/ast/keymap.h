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
*     <COPYRIGHT_STATEMENT>

*  Authors:
*     DSB: David S. Berry (Starlink)

*  History:
*     13-NOV-2004 (DSB):
*        Original version.
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
/* The number of elements in the hash table stored in each KeyMap structure. */
#define AST__MAPSIZE 23

/* Data type constants: */
#define AST__BADTYPE 0
#define AST__INTTYPE 1
#define AST__DOUBLETYPE 2
#define AST__STRINGTYPE 3
#define AST__OBJECTTYPE 4

/* Type Definitions. */
/* ================= */

/* This structure contains information describing a single generic entry in 
   a KeyMap. This structure is extended by other structures to hold data of
   specific data types. */

typedef struct AstMapEntry {
   struct AstMapEntry *next; /* Pointer to next structure in list. */
   const char *key;          /* The name used to identify the entry */
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
   AstMapEntry *table[ AST__MAPSIZE ]; /* Hash table containing pointers to 
                                          the KeyMap entries */
   int nentry[ AST__MAPSIZE ];         /* No. of Entries in each table element */
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
   void (* MapPut0I)( AstKeyMap *, const char *, int, const char * );
   void (* MapPut0D)( AstKeyMap *, const char *, double, const char * );
   void (* MapPut0C)( AstKeyMap *, const char *, const char *, const char * );
   void (* MapPut0A)( AstKeyMap *, const char *, AstObject *, const char * );
   void (* MapPut1I)( AstKeyMap *, const char *, int, int[], const char * );
   void (* MapPut1D)( AstKeyMap *, const char *, int, double[], const char * );
   void (* MapPut1C)( AstKeyMap *, const char *, int, const char *[], const char * );
   void (* MapPut1A)( AstKeyMap *, const char *, int, AstObject *[], const char * );
   int (* MapGet0I)( AstKeyMap *, const char *, int * );
   int (* MapGet0D)( AstKeyMap *, const char *, double * );
   int (* MapGet0C)( AstKeyMap *, const char *, const char ** );
   int (* MapGet0A)( AstKeyMap *, const char *, AstObject ** );
   int (* MapGet1A)( AstKeyMap *, const char *, int, int *, AstObject ** );
   int (* MapGet1C)( AstKeyMap *, const char *, int, int, int *, char * );
   int (* MapGet1D)( AstKeyMap *, const char *, int, int *, double * );
   int (* MapGet1I)( AstKeyMap *, const char *, int, int *, int * );
   void (* MapRemove)( AstKeyMap *, const char * );
   int (* MapSize)( AstKeyMap * );
   int (* MapLength)( AstKeyMap *, const char * );
   int (* MapLenC)( AstKeyMap *, const char * );
   int (* MapType)( AstKeyMap *, const char * );
   int (* MapHasKey)( AstKeyMap *, const char * );
   const char *(* MapKey)( AstKeyMap *, int );

} AstKeyMapVtab;
#endif

/* Function prototypes. */
/* ==================== */
/* Prototypes for standard class functions. */
/* ---------------------------------------- */
astPROTO_CHECK(KeyMap)          /* Check class membership */
astPROTO_ISA(KeyMap)            /* Test class membership */

/* Constructor. */
#if defined(astCLASS)            /* Protected. */
AstKeyMap *astKeyMap_( const char *, ... );
#else
AstKeyMap *astKeyMapId_( const char *, ... );
#endif

#if defined(astCLASS)            /* Protected */

/* Initialiser. */
AstKeyMap *astInitKeyMap_( void *, size_t, int, AstKeyMapVtab *, const char * );

/* Vtab initialiser. */
void astInitKeyMapVtab_( AstKeyMapVtab *, const char * );

/* Loader. */
AstKeyMap *astLoadKeyMap_( void *, size_t, AstKeyMapVtab *, const char *, AstChannel * );
#endif

/* Prototypes for member functions. */
/* -------------------------------- */

#if defined(astCLASS)            /* Protected */
int astMapGet0A_( AstKeyMap *, const char *, AstObject ** );
int astMapGet1A_( AstKeyMap *, const char *, int, int *, AstObject ** );
void astMapPut1A_( AstKeyMap *, const char *, int, AstObject *[], const char * );
#else
int astMapGet0AId_( AstKeyMap *, const char *, AstObject ** );
int astMapGet1AId_( AstKeyMap *, const char *, int, int *, AstObject ** );
void astMapPut1AId_( AstKeyMap *, const char *, int, AstObject *[], const char * );
#endif

const char *astMapKey_( AstKeyMap *, int );
int astMapGet0C_( AstKeyMap *, const char *, const char ** );
int astMapGet0D_( AstKeyMap *, const char *, double * );
int astMapGet0I_( AstKeyMap *, const char *, int * );
int astMapGet1C_( AstKeyMap *, const char *, int, int, int *, char * );
int astMapGet1D_( AstKeyMap *, const char *, int, int *, double * );
int astMapGet1I_( AstKeyMap *, const char *, int, int *, int * );
int astMapHasKey_( AstKeyMap *, const char * );
int astMapLength_( AstKeyMap *, const char * );
int astMapLenC_( AstKeyMap *, const char * );
int astMapSize_( AstKeyMap * );
int astMapType_( AstKeyMap *, const char * );
int astMapType_( AstKeyMap *, const char * );
void astMapPut0A_( AstKeyMap *, const char *, AstObject *, const char * );
void astMapPut0C_( AstKeyMap *, const char *, const char *, const char * );
void astMapPut0D_( AstKeyMap *, const char *, double, const char * );
void astMapPut0I_( AstKeyMap *, const char *, int, const char * );
void astMapPut1C_( AstKeyMap *, const char *, int, const char *[], const char * );
void astMapPut1D_( AstKeyMap *, const char *, int, double *, const char * );
void astMapPut1I_( AstKeyMap *, const char *, int, int *, const char * );
void astMapRemove_( AstKeyMap *, const char * );


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
#define astInitKeyMap(mem,size,init,vtab,name) astINVOKE(O,astInitKeyMap_(mem,size,init,vtab,name))

/* Vtab Initialiser. */
#define astInitKeyMapVtab(vtab,name) astINVOKE(V,astInitKeyMapVtab_(vtab,name))
/* Loader. */
#define astLoadKeyMap(mem,size,vtab,name,channel) \
astINVOKE(O,astLoadKeyMap_(mem,size,vtab,name,astCheckChannel(channel)))
#endif

/* Interfaces to public member functions. */
/* -------------------------------------- */
/* Here we make use of astCheckKeyMap to validate KeyMap pointers
   before use.  This provides a contextual error report if a pointer
   to the wrong sort of Object is supplied. */
#define astMapPut0I(this,key,value,comment) astINVOKE(V,astMapPut0I_(astCheckKeyMap(this),key,value,comment))
#define astMapPut0D(this,key,value,comment) astINVOKE(V,astMapPut0D_(astCheckKeyMap(this),key,value,comment))
#define astMapPut0C(this,key,value,comment) astINVOKE(V,astMapPut0C_(astCheckKeyMap(this),key,value,comment))
#define astMapPut0A(this,key,value,comment) astINVOKE(V,astMapPut0A_(astCheckKeyMap(this),key,astCheckObject(value),comment))
#define astMapPut1I(this,key,size,value,comment) astINVOKE(V,astMapPut1I_(astCheckKeyMap(this),key,size,value,comment))
#define astMapPut1D(this,key,size,value,comment) astINVOKE(V,astMapPut1D_(astCheckKeyMap(this),key,size,value,comment))
#define astMapPut1C(this,key,size,value,comment) astINVOKE(V,astMapPut1C_(astCheckKeyMap(this),key,size,value,comment))
#define astMapGet0I(this,key,value) astINVOKE(V,astMapGet0I_(astCheckKeyMap(this),key,value))
#define astMapGet0D(this,key,value) astINVOKE(V,astMapGet0D_(astCheckKeyMap(this),key,value))
#define astMapGet0C(this,key,value) astINVOKE(V,astMapGet0C_(astCheckKeyMap(this),key,value))
#define astMapGet1I(this,key,mxval,nval,value) astINVOKE(V,astMapGet1I_(astCheckKeyMap(this),key,mxval,nval,value))
#define astMapGet1D(this,key,mxval,nval,value) astINVOKE(V,astMapGet1D_(astCheckKeyMap(this),key,mxval,nval,value))
#define astMapGet1C(this,key,l,mxval,nval,value) astINVOKE(V,astMapGet1C_(astCheckKeyMap(this),key,l,mxval,nval,value))
#define astMapRemove(this,key) astINVOKE(V,astMapRemove_(astCheckKeyMap(this),key))
#define astMapSize(this) astINVOKE(V,astMapSize_(astCheckKeyMap(this)))
#define astMapLength(this,key) astINVOKE(V,astMapLength_(astCheckKeyMap(this),key))
#define astMapLenC(this,key) astINVOKE(V,astMapLenC_(astCheckKeyMap(this),key))
#define astMapHasKey(this,key) astINVOKE(V,astMapHasKey_(astCheckKeyMap(this),key))
#define astMapKey(this,index) astINVOKE(V,astMapKey_(astCheckKeyMap(this),index))
#define astMapType(this,key) astINVOKE(V,astMapType_(astCheckKeyMap(this),key))

#if defined(astCLASS)            /* Protected */
#define astMapGet0A(this,key,value) astINVOKE(V,astMapGet0A_(astCheckKeyMap(this),key,(AstObject **)(value)))
#define astMapGet1A(this,key,mxval,nval,value) astINVOKE(V,astMapGet1A_(astCheckKeyMap(this),key,mxval,nval,(AstObject **)(value)))
#define astMapPut1A(this,key,size,value,comment) astINVOKE(V,astMapPut1A_(astCheckKeyMap(this),key,size,value,comment))
#else
#define astMapGet0A(this,key,value) astINVOKE(V,astMapGet0AId_(astCheckKeyMap(this),key,(AstObject **)(value)))
#define astMapGet1A(this,key,mxval,nval,value) astINVOKE(V,astMapGet1AId_(astCheckKeyMap(this),key,mxval,nval,(AstObject **)(value)))
#define astMapPut1A(this,key,size,value,comment) astINVOKE(V,astMapPut1AId_(astCheckKeyMap(this),key,size,value,comment))
#endif

#endif
