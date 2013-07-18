/*
*class++
*  Name:
*     KeyMap

*  Purpose:
*     Store a set of key/value pairs.

*  Constructor Function:
c     astKeyMap
f     AST_KEYMAP

*  Description:
*     The KeyMap class is used to store a set of values with associated keys
*     which identify the values. The keys are strings. These may be case
*     sensitive or insensitive as selected by the KeyCase attribute, and
*     trailing spaces are ignored. The value associated with a key can be
*     integer (signed 4 and 2 byte, or unsigned 1 byte), floating point
*     (single or double precision),
c     void pointer,
*     character string or AST Object pointer. Each
*     value can be a scalar or a one-dimensional vector. A KeyMap is
*     conceptually similar to a Mapping in that a KeyMap transforms an
*     input into an output - the input is the key, and the output is the
*     value associated with the key. However, this is only a conceptual
*     similarity, and it should be noted that the KeyMap class inherits from
*     the Object class rather than the Mapping class. The methods of the
*     Mapping class cannot be used with a KeyMap.

*  Inheritance:
*     The KeyMap class inherits from the Object class.

*  Attributes:
*     In addition to those attributes common to all Objects, every
*     KeyMap also has the following attributes:
*
*     - KeyCase: Sets the case in which keys are stored
*     - KeyError: Report an error if the requested key does not exist?
*     - SizeGuess: The expected size of the KeyMap.
*     - SortBy: Determines how keys are sorted in a KeyMap.
*     - MapLocked: Prevent new entries being added to the KeyMap?

*  Functions:
c     In addition to those functions applicable to all Objects, the
c     following functions may also be applied to all KeyMaps:
f     In addition to those routines applicable to all Objects, the
f     following routines may also be applied to all KeyMaps:
*
c     - astMapDefined: Does a KeyMap contain a defined value for a key?
c     - astMapGet0<X>: Get a named scalar entry from a KeyMap
c     - astMapGet1<X>: Get a named vector entry from a KeyMap
c     - astMapGetElem<X>: Get an element of a named vector entry from a KeyMap
c     - astMapHasKey: Does the KeyMap contain a named entry?
c     - astMapKey: Return the key name at a given index in the KeyMap
c     - astMapLenC: Get the length of a named character entry in a KeyMap
c     - astMapLength: Get the length of a named entry in a KeyMap
c     - astMapCopy: Copy entries from one KeyMap into another
c     - astMapPut0<X>: Add a new scalar entry to a KeyMap
c     - astMapPut1<X>: Add a new vector entry to a KeyMap
c     - astMapPutElem<X>: Puts a value into a vector entry in a KeyMap
c     - astMapPutU: Add a new entry to a KeyMap with an undefined value
c     - astMapRemove: Removed a named entry from a KeyMap
c     - astMapRename: Rename an existing entry in a KeyMap
c     - astMapSize: Get the number of entries in a KeyMap
c     - astMapType: Return the data type of a named entry in a map
f     - AST_MAPDEFINED: Does a KeyMap contain a defined value for a key?
f     - AST_MAPGET0<X>: Get a named scalar entry from a KeyMap
f     - AST_MAPGET1<X>: Get a named vector entry from a KeyMap
f     - AST_MAPGETELEM<X>: Get an element of a named vector entry from a KeyMap
f     - AST_MAPHASKEY: Does the KeyMap contain a named entry?
f     - AST_MAPKEY: Return the key name at a given index in the KeyMap
f     - AST_MAPLENC: Get the length of a named character entry in a KeyMap
f     - AST_MAPLENGTH: Get the length of a named entry in a KeyMap
f     - AST_MAPCOPY: Copy entries from one KeyMap into another
f     - AST_MAPPUT0<X>: Add a new scalar entry to a KeyMap
f     - AST_MAPPUT1<X>: Add a new vector entry to a KeyMap
f     - AST_MAPPUTELEM<X>: Puts a value into a vector entry in a KeyMap
f     - AST_MAPPUTU: Add a new entry to a KeyMap with an undefined value
f     - AST_MAPREMOVE: Removed a named entry from a KeyMap
f     - AST_MAPRENAME: Rename an existing entry in a KeyMap
f     - AST_MAPSIZE: Get the number of entries in a KeyMap
f     - AST_MAPTYPE: Return the data type of a named entry in a map

*  Copyright:
*     Copyright (C) 1997-2006 Council for the Central Laboratory of the
*     Research Councils
*     Copyright (C) 2008-2010 Science & Technology Facilities Council.
*     All Rights Reserved.

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
*     DSB: B.S. Berry (Starlink)

*  History:
*     12-NOV-2004 (DSB):
*        Original version.
*     5-JAN-2005 (DSB):
*        Added astMapLenC method.
*     17-JAN-2005 (DSB):
*        Remove "void *" arithmetic.
*     25-JAN-2005 (DSB):
*        Added more DEBUG blocks
*     30-SEP-2005 (DSB):
*        Allow an integer to be read from a formatted floating point value.
*     6-DEC-2005 (DSB):
*        Remove astMapGet0C stuff from description of astMapGet1C.
*     14-FEB-2006 (DSB):
*        Override astGetObjSize.
*     1-MAR-2006 (DSB):
*        Replace astSetPermMap within DEBUG blocks by astBeginPM/astEndPM.
*     5-JUN-2006 (DSB):
*        Added support for single precision entries.
*     30-NOV-2007 (DSB):
*        Added SizeGuess attribute.
*     4-DEC-2007 (DSB):
*        Allow size of hash table to grow dynamically as more entries are
*        added to the KeyMap.
*     5-DEC-2007 (DSB):
*        Ensure mapsize is always a power of 2.
*     6-DEC-2007 (DSB):
*        - Define the minium table size rather than the default SizeGuess
*        value, and derive the default SizeGuess value from the minimum
*        table size.
*        - Use "&" rather than "%" to get the hash table index from the
*        full width hash value (& may be faster than %).
*     7-MAR-2008 (DSB):
*         Added support for pointer ("P") entries.
*     31-MAR-2009 (DSB):
*         Remove rounding errors from formatted double values.
*     27-APR-2009 (DSB):
*         Added astMapGetElem<X>.
*     1-SEP-2009 (DSB):
*         Added KeyError attribute.
*     12-FEB-2010 (DSB):
*         When converting an entry value between double and string, treat
*         "<bad>" as the formatted version of AST__BAD.
*     3-MAR-2010 (DSB):
*         Added astMapPutElem<X>.
*     27-APR-2010 (DSB):
*         Added MapLocked attribute.
*     4-MAY-2010 (DSB):
*         - Propagate MapLocked and KeyError attributes to any encapsulated
*         KeyMaps.
*         - Added astMapCopy method.
*         - Added astMapPutU method and AST__UNDEFTYPE data type.
*     11-AUG-2010 (DSB):
*        Added SortBy attribute.
*     12-AUG-2010 (DSB):
*        Speed up access to large KeyMaps.
*     13-AUG-2010 (DSB):
*        - No need to sort all entries when doubling the table size since
*        changing the table size does not change the linked list of sorted
*        entries.
*        - Initialise the sortby attribute to the cleared value, rather
*        than the default value.
*     2-OCT-2010 (DSB):
*        Added support for short int valued entries.
*     24-NOV-2010 (DSB):
*        Fix memory leak in astMapPutElemC and astMapPutElemA.
*     26-NOV-2010 (DSB):
*        Added support for unsigned byte valued entries.
*     3-DEC-2010 (DSB):
*         Added KeyCase attribute.
*     14-JAN-2011 (DSB):
*         Fix bug that prevented zero length strings being stored in a
*         keymap.
*     17-SEP-2012 (DSB):
*         Fix bug that prevented UNDEF entries from being read back in
*         from a dump of a KeyMap.
*     18-MAR-2013 (DSB):
*         Added astMapDefined.
*     18-JUL-2013 (DSB):
*         Added SortBy options "KeyAgeUp" and "KeyAgeDown".
*class--
*/

/* Module Macros. */
/* ============== */
/* Set the name of the class we are implementing. This indicates to
   the header files that define class interfaces that they should make
   "protected" symbols available. */
#define astCLASS KeyMap

/* Minimum size for the hash table. */
#define MIN_TABLE_SIZE 16

/* The maximum number of entries per element of the hash table. If this
   value is exceeded the hash table will be doubled in size. */
#define MAX_ENTRIES_PER_TABLE_ENTRY 10

/* String used to represent the formatetd version of AST__BAD. */
#define BAD_STRING "<bad>"

/* Integer values to represent the different values of the SortBy attribute. */
#define SORTBY_NONE 0
#define SORTBY_AGEUP 1
#define SORTBY_AGEDOWN 2
#define SORTBY_KEYUP 3
#define SORTBY_KEYDOWN 4
#define SORTBY_KEYAGEUP 5
#define SORTBY_KEYAGEDOWN 6


/* Include files. */
/* ============== */
/* Interface definitions. */
/* ---------------------- */

#include "globals.h"             /* Thread-safe global data access */
#include "error.h"               /* Error reporting facilities */
#include "memory.h"              /* Memory management facilities */
#include "object.h"              /* Base Object class */
#include "pointset.h"            /* For AST__BAD */
#include "channel.h"             /* I/O channels */
#include "keymap.h"              /* Interface definition for this class */
#include "globals.h"             /* Thread-safe global data access */

/* Error code definitions. */
/* ----------------------- */
#include "ast_err.h"             /* AST error codes */

/* C header files. */
/* --------------- */
#include <limits.h>
#include <stdarg.h>
#include <stddef.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

/* Type Definitions */
/* ================ */

/* This structure is a AstMapEntry holding a scalar int */
typedef struct Entry0I {
   struct AstMapEntry entry; /* The parent Entry information */
   int value;                /* The integer value */
} Entry0I;

/* This structure is a AstMapEntry holding a scalar double */
typedef struct Entry0D {
   struct AstMapEntry entry; /* The parent Entry information */
   double value;             /* The floating point value */
} Entry0D;

/* This structure is a AstMapEntry holding a scalar short int */
typedef struct Entry0S {
   struct AstMapEntry entry; /* The parent Entry information */
   short int value;          /* The short int value */
} Entry0S;

/* This structure is a AstMapEntry holding a scalar unsigned byte
   (unsigned char) */
typedef struct Entry0B {
   struct AstMapEntry entry; /* The parent Entry information */
   unsigned char value;      /* The byte value */
} Entry0B;

/* This structure is a AstMapEntry holding a scalar float */
typedef struct Entry0F {
   struct AstMapEntry entry; /* The parent Entry information */
   float value;              /* The floating point value */
} Entry0F;

/* This structure is a AstMapEntry holding a scalar string */
typedef struct Entry0C {
   struct AstMapEntry entry; /* The parent Entry information */
   const char *value;        /* The string pointer */
} Entry0C;

/* This structure is a AstMapEntry holding a scalar AST Object */
typedef struct Entry0A {
   struct AstMapEntry entry; /* The parent Entry information */
   AstObject *value;         /* The Object pointer */
   struct AstMapEntry *next; /* Pointer to next AST Object entry */
   struct AstMapEntry *prev; /* Pointer to previous AST Object entry */
} Entry0A;

/* This structure is a AstMapEntry holding a scalar void pointer */
typedef struct Entry0P {
   struct AstMapEntry entry; /* The parent Entry information */
   void *value;              /* The pointer */
} Entry0P;

/* This structure is a AstMapEntry holding a 1D array of ints */
typedef struct Entry1I {
   struct AstMapEntry entry; /* The parent Entry information */
   int *value;               /* The integer values */
} Entry1I;

/* This structure is a AstMapEntry holding a 1D array of doubles */
typedef struct Entry1D {
   struct AstMapEntry entry; /* The parent Entry information */
   double *value;            /* The floating point values */
} Entry1D;

/* This structure is a AstMapEntry holding a 1D array of short ints */
typedef struct Entry1S {
   struct AstMapEntry entry; /* The parent Entry information */
   short int *value;         /* The short int values */
} Entry1S;

/* This structure is a AstMapEntry holding a 1D array of unsigned bytes */
typedef struct Entry1B {
   struct AstMapEntry entry; /* The parent Entry information */
   unsigned char *value;     /* The byte values */
} Entry1B;

/* This structure is a AstMapEntry holding a 1D array of floats */
typedef struct Entry1F {
   struct AstMapEntry entry; /* The parent Entry information */
   float *value;             /* The floating point values */
} Entry1F;

/* This structure is a AstMapEntry holding a 1D array of strings */
typedef struct Entry1C {
   struct AstMapEntry entry; /* The parent Entry information */
   const char **value;       /* The string pointers */
} Entry1C;

/* This structure is a AstMapEntry holding a 1D array of AST Objects */
typedef struct Entry1A {
   struct AstMapEntry entry; /* The parent Entry information */
   AstObject **value;        /* The Object pointers */
   struct AstMapEntry *next; /* Pointer to next AST Object entry */
   struct AstMapEntry *prev; /* Pointer to previous AST Object entry */
} Entry1A;

/* This structure is a AstMapEntry holding a 1D array of void pointers. */
typedef struct Entry1P {
   struct AstMapEntry entry; /* The parent Entry information */
   void **value;             /* The pointers */
} Entry1P;


/* Module Variables. */
/* ================= */

/* Address of this static variable is used as a unique identifier for
   member of this class. */
static int class_check;

/* Pointers to parent class methods which are extended by this class. */
static int (* parent_getobjsize)( AstObject *, int * );
static const char *(* parent_getattrib)( AstObject *, const char *, int * );
static int (* parent_testattrib)( AstObject *, const char *, int * );
static void (* parent_clearattrib)( AstObject *, const char *, int * );
static void (* parent_setattrib)( AstObject *, const char *, int * );

#if defined(THREAD_SAFE)
static int (* parent_managelock)( AstObject *, int, int, AstObject **, int * );
#endif

/* Define macros for accessing each item of thread specific global data. */
#ifdef THREAD_SAFE

/* Define how to initialise thread-specific globals. */
#define GLOBAL_inits \
   globals->Class_Init = 0; \
   globals->GetAttrib_Buff[ 0 ] = 0; \
   globals->ConvertValue_Init = 0; \
   globals->ConvertValue_Istr = 0; \
   globals->ConvertValue_Buff[ 0 ] = 0; \
   globals->MapKey_Init = 0; \
   globals->MapKey_Istr = 0;

/* Create the function that initialises global data for this module. */
astMAKE_INITGLOBALS(KeyMap)

/* Define macros for accessing each item of thread specific global data. */
#define class_init astGLOBAL(KeyMap,Class_Init)
#define class_vtab astGLOBAL(KeyMap,Class_Vtab)
#define getattrib_buff astGLOBAL(KeyMap,GetAttrib_Buff)
#define convertvalue_strings astGLOBAL(KeyMap,ConvertValue_Strings)
#define convertvalue_istr astGLOBAL(KeyMap,ConvertValue_Istr)
#define convertvalue_init astGLOBAL(KeyMap,ConvertValue_Init)
#define convertvalue_buff astGLOBAL(KeyMap,ConvertValue_Buff)
#define mapkey_strings astGLOBAL(KeyMap,MapKey_Strings)
#define mapkey_istr astGLOBAL(KeyMap,MapKey_Istr)
#define mapkey_init astGLOBAL(KeyMap,MapKey_Init)



/* If thread safety is not needed, declare and initialise globals at static
   variables. */
#else

/* Buffer returned by GetAttrib. */ \
static char getattrib_buff[ AST__KEYMAP_GETATTRIB_BUFF_LEN + 1 ];

/* Strings returned by ConvertValue */ \
static char *convertvalue_strings[ AST__KEYMAP_CONVERTVALUE_MAX_STRINGS ];

/* Offset of next string in "ConvertValue_Strings" */ \
static int convertvalue_istr;

/* "ConvertValue_Strings" array initialised? */ \
static int convertvalue_init;

/* ConvertValue string buffer */ \
static char convertvalue_buff[ AST__KEYMAP_CONVERTVALUE_BUFF_LEN + 1 ];

/* Strings returned by MapKey */ \
static char *mapkey_strings[ AST__KEYMAP_MAPKEY_MAX_STRINGS ];

/* Offset of next string in "MapKey_Strings" */ \
static int mapkey_istr;

/* "MapKey_Strings" array initialised? */ \
static int mapkey_init;


/* Define the class virtual function table and its initialisation flag
   as static variables. */
static AstKeyMapVtab class_vtab;   /* Virtual function table */
static int class_init = 0;         /* Virtual function table initialised? */

#endif

/* External Interface Function Prototypes. */
/* ======================================= */
/* The following functions have public prototypes only (i.e. no
   protected prototypes), so we must provide local prototypes for use
   within this module. */
AstKeyMap *astKeyMapId_( const char *, ... );

/* Prototypes for Private Member Functions. */
/* ======================================== */
static AstMapEntry *AddTableEntry( AstKeyMap *, int, AstMapEntry *, int, int * );
static AstMapEntry *CopyMapEntry( AstMapEntry *, int * );
static AstMapEntry *FreeMapEntry( AstMapEntry *, int * );
static AstMapEntry *RemoveTableEntry( AstKeyMap *, int, const char *, int * );
static AstMapEntry *SearchTableEntry( AstKeyMap *, int, const char *, int * );
static const char *ConvertKey( AstKeyMap *, const char *, char *, int, const char *, int * );
static const char *GetKey( AstKeyMap *, int index, int * );
static const char *MapIterate( AstKeyMap *, int, int * );
static const char *MapKey( AstKeyMap *, int index, int * );
static const char *SortByString( int, const char *, int * );
static int CompareEntries( const void *, const void * );
static int ConvertValue( void *, int, void *, int, int * );
static int GetObjSize( AstObject *, int * );
static int HashFun( const char *, int, unsigned long *, int * );
static int KeyCmp( const char *, const char * );
static int MapDefined( AstKeyMap *, const char *, int * );
static int MapGet0A( AstKeyMap *, const char *, AstObject **, int * );
static int MapGet0C( AstKeyMap *, const char *, const char **, int * );
static int MapGet0D( AstKeyMap *, const char *, double *, int * );
static int MapGet0S( AstKeyMap *, const char *, short int *, int * );
static int MapGet0B( AstKeyMap *, const char *, unsigned char *, int * );
static int MapGet0F( AstKeyMap *, const char *, float *, int * );
static int MapGet0I( AstKeyMap *, const char *, int *, int * );
static int MapGet0P( AstKeyMap *, const char *, void **, int * );
static int MapGet1A( AstKeyMap *, const char *, int, int *, AstObject **, int * );
static int MapGet1C( AstKeyMap *, const char *, int, int, int *, char *, int * );
static int MapGet1D( AstKeyMap *, const char *, int, int *, double *, int * );
static int MapGet1B( AstKeyMap *, const char *, int, int *, unsigned char *, int * );
static int MapGet1S( AstKeyMap *, const char *, int, int *, short int *, int * );
static int MapGet1F( AstKeyMap *, const char *, int, int *, float *, int * );
static int MapGet1I( AstKeyMap *, const char *, int, int *, int *, int * );
static int MapGet1P( AstKeyMap *, const char *, int, int *, void **, int * );
static int MapGetElemA( AstKeyMap *, const char *, int, AstObject **, int * );
static int MapGetElemC( AstKeyMap *, const char *, int, int, char *, int * );
static int MapGetElemD( AstKeyMap *, const char *, int, double *, int * );
static int MapGetElemB( AstKeyMap *, const char *, int, unsigned char *, int * );
static int MapGetElemS( AstKeyMap *, const char *, int, short int *, int * );
static int MapGetElemF( AstKeyMap *, const char *, int, float *, int * );
static int MapGetElemI( AstKeyMap *, const char *, int, int *, int * );
static int MapGetElemP( AstKeyMap *, const char *, int, void **, int * );
static int MapHasKey( AstKeyMap *, const char *, int * );
static int MapLenC( AstKeyMap *, const char *, int * );
static int MapLength( AstKeyMap *, const char *, int * );
static int MapSize( AstKeyMap *, int * );
static int MapType( AstKeyMap *, const char *, int * );
static int SortByInt( const char *, const char *, int * );
static size_t SizeOfEntry( AstMapEntry *, int * );
static void AddToObjectList( AstKeyMap *, AstMapEntry *, int * );
static void AddToSortedList( AstKeyMap *, AstMapEntry *, int * );
static void CheckCircle( AstKeyMap *, AstObject *, const char *, int * );
static void Copy( const AstObject *, AstObject *, int * );
static void CopyTableEntry( AstKeyMap *, AstKeyMap *, int, int * );
static void Delete( AstObject *, int * );
static void DoubleTableSize( AstKeyMap *, int * );
static void Dump( AstObject *, AstChannel *, int * );
static void DumpEntry( AstMapEntry *, AstChannel *, int, int * );
static void FreeTableEntry( AstKeyMap *, int itab, int * );
static void InitMapEntry( AstMapEntry *, int, int, int * );
static void MapCopy( AstKeyMap *, AstKeyMap *, int * );
static void MapPut0A( AstKeyMap *, const char *, AstObject *, const char *, int * );
static void MapPut0C( AstKeyMap *, const char *, const char *, const char *, int * );
static void MapPut0D( AstKeyMap *, const char *, double, const char *, int * );
static void MapPut0B( AstKeyMap *, const char *, unsigned char, const char *, int * );
static void MapPut0S( AstKeyMap *, const char *, short int, const char *, int * );
static void MapPut0F( AstKeyMap *, const char *, float, const char *, int * );
static void MapPut0I( AstKeyMap *, const char *, int, const char *, int * );
static void MapPut0P( AstKeyMap *, const char *, void *, const char *, int * );
static void MapPut1A( AstKeyMap *, const char *, int, AstObject *const [], const char *, int * );
static void MapPut1C( AstKeyMap *, const char *, int, const char *const [], const char *, int * );
static void MapPut1D( AstKeyMap *, const char *, int, const double *, const char *, int * );
static void MapPut1B( AstKeyMap *, const char *, int, const unsigned char *, const char *, int * );
static void MapPut1S( AstKeyMap *, const char *, int, const short int *, const char *, int * );
static void MapPut1F( AstKeyMap *, const char *, int, const float *, const char *, int * );
static void MapPut1I( AstKeyMap *, const char *, int, const int *, const char *, int * );
static void MapPut1P( AstKeyMap *, const char *, int, void *const [], const char *, int * );
static void MapPutElemA( AstKeyMap *, const char *, int, AstObject *, int * );
static void MapPutElemC( AstKeyMap *, const char *, int, const char *, int * );
static void MapPutElemD( AstKeyMap *, const char *, int, double, int * );
static void MapPutElemB( AstKeyMap *, const char *, int, unsigned char, int * );
static void MapPutElemS( AstKeyMap *, const char *, int, short int, int * );
static void MapPutElemF( AstKeyMap *, const char *, int, float, int * );
static void MapPutElemI( AstKeyMap *, const char *, int, int, int * );
static void MapPutElemP( AstKeyMap *, const char *, int, void *, int * );
static void MapPutU( AstKeyMap *, const char *, const char *, int * );
static void MapRemove( AstKeyMap *, const char *, int * );
static void MapRename( AstKeyMap *, const char *, const char *, int * );
static void NewTable( AstKeyMap *, int, int * );
static void RemoveFromSortedList( AstKeyMap *, AstMapEntry *, int * );
static void RemoveFromObjectList( AstKeyMap *, AstMapEntry *, int * );
static void SortEntries( AstKeyMap *, int * );

static const char *GetAttrib( AstObject *, const char *, int * );
static int TestAttrib( AstObject *, const char *, int * );
static void ClearAttrib( AstObject *, const char *, int * );
static void SetAttrib( AstObject *, const char *, int * );

static int GetSizeGuess( AstKeyMap *, int * );
static int TestSizeGuess( AstKeyMap *, int * );
static void ClearSizeGuess( AstKeyMap *, int * );
static void SetSizeGuess( AstKeyMap *, int, int * );

static int GetSortBy( AstKeyMap *, int * );
static int TestSortBy( AstKeyMap *, int * );
static void ClearSortBy( AstKeyMap *, int * );
static void SetSortBy( AstKeyMap *, int, int * );

static int GetKeyError( AstKeyMap *, int * );
static int TestKeyError( AstKeyMap *, int * );
static void ClearKeyError( AstKeyMap *, int * );
static void SetKeyError( AstKeyMap *, int, int * );

static int GetKeyCase( AstKeyMap *, int * );
static int TestKeyCase( AstKeyMap *, int * );
static void ClearKeyCase( AstKeyMap *, int * );
static void SetKeyCase( AstKeyMap *, int, int * );

static int GetMapLocked( AstKeyMap *, int * );
static int TestMapLocked( AstKeyMap *, int * );
static void ClearMapLocked( AstKeyMap *, int * );
static void SetMapLocked( AstKeyMap *, int, int * );

#if defined(THREAD_SAFE)
static int ManageLock( AstObject *, int, int, AstObject **, int * );
#endif

/* Member functions. */
/* ================= */
static AstMapEntry *AddTableEntry( AstKeyMap *this, int itab,
                                   AstMapEntry *entry, int keymember,
                                   int *status ){
/*
*  Name:
*     AddTableEntry

*  Purpose:
*     Add an new entry to a linked-list of KeyMap entries.

*  Type:
*     Private function.

*  Synopsis:
*     #include "keymap.h"
*     AstMapEntry *AddTableEntry( AstKeyMap *this, int itab,
*                                 AstMapEntry *entry, int keymember,
*                                 int *status ){

*  Class Membership:
*     KeyMap member function.

*  Description:
*     This function adds the supplied MapEntry to the head of the linked
*     list of MapEntries stored at the specified entry of the hash table.
*     If this results in the linked list having too many entries, then a
*     new larger hash table is allocated and the entries in the existing
*     table are moved into the new table.

*  Parameters:
*     this
*        Pointer to the KeyMap.
*     itab
*        Index of the hash table element to be searched.
*     entry
*        Pointer to the MapEntry to be added.
*     keymember
*        A unique integer identifier for the key that increases
*        monotonically with age of the key. If this is negative,
*        the next available identifier will be used automatically.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     A NULL pointer.

*/

/* Check the global error status. */
   if ( !astOK ) return NULL;

/* Put a pointer to the MapEntry which is currently at the head of the
   linked list in the "next" component of the supplied MapEntry. */
   entry->next = this->table[ itab ];

/* Store the supplied MapEntry pointer in the requested element of the
   hash table. */
   this->table[ itab ] = entry;

/* Increment the length of linked list. */
   this->nentry[ itab ]++;

/* Each new entry added to the KeyMap has a unique member index that is
   never re-used. */
   entry->member = (this->member_count)++;

/* Each key added to the  KeyMap also has a separate unique member index,
   but this index is re-used each time the same key is added into the
   KeyMap. So changing the value associated with a key does not cause the
   keymember value to change. */
   if( keymember >= 0 ) {
      entry->keymember = keymember;
   } else {
      entry->keymember = (this->member_count)++;
   }

/* Insert the supplied MapEntry into a list sorted by key. */
   AddToSortedList( this, entry, status );

/* If the entry is of type AST__OBJECTTYPE, add it to the head of the
   list of AST__OBJECTTYPE entries in the KeyMap. */
   AddToObjectList( this, entry, status );

/* If the population of this table entry is now too large, double the size
   of the table, moving the table entries to appropriate places in the
   new larger table. */
   if( this->nentry[ itab ] > MAX_ENTRIES_PER_TABLE_ENTRY ) {
      DoubleTableSize( this, status );
   }

/* Return a NULL pointer. */
   return NULL;
}

static void AddToObjectList( AstKeyMap *this, AstMapEntry *entry, int *status ){
/*
*  Name:
*     AddToObjectList

*  Purpose:
*     Add AST__OBJECTTYPE entries into a linked-list of such entries.

*  Type:
*     Private function.

*  Synopsis:
*     #include "keymap.h"
*     void AddToObjectList( AstKeyMap *this, AstMapEntry *entry, int *status )

*  Class Membership:
*     KeyMap member function.

*  Description:
*     If the supplied MapEntry holds one or more pointers to AST Objects,
*     then the entry is added to a linked list of such entries.

*  Parameters:
*     this
*        Pointer to the KeyMap.
*     entry
*        Pointer to the MapEntry to be added.
*     status
*        Pointer to the inherited status variable.

*/

/* Local Variables: */
   Entry0A *scalar;         /* Pointer to a scalar AST__OBJECTTYPE entry */
   Entry1A *vector;         /* Pointer to a vector AST__OBJECTTYPE entry */

/* Check the global error status. */
   if ( !astOK ) return;

/* Do nothing if the entry does not hold AST Object pointers. */
   if( entry->type == AST__OBJECTTYPE ) {

/* If the list is not currently empty, add the new entry into the list. */
      if( this->firstA ) {

/* Store a pointer to the new entry as the previous link in the current
   first entry in the list. */
         if( this->firstA->nel == 0 ) {
            scalar = (Entry0A *) this->firstA;
            scalar->prev = entry;
         } else {
            vector = (Entry1A *) this->firstA;
            vector->prev = entry;
         }

/* Store a pointer to the current first entry as the next link in the new
   entry, and nullify the previus link. */
         if( entry->nel == 0 ) {
            scalar = (Entry0A *) entry;
            scalar->next = this->firstA;
            scalar->prev = NULL;
         } else {
            vector = (Entry1A *) entry;
            vector->next = this->firstA;
            vector->prev = NULL;
         }

/* If the list is currently empty, nullify both links in the entry. */
      } else {
         if( entry->nel == 0 ) {
            scalar = (Entry0A *) entry;
            scalar->next = NULL;
            scalar->prev = NULL;
         } else {
            vector = (Entry1A *) entry;
            vector->next = NULL;
            vector->prev = NULL;
         }

      }

/* Store the new entry as the first entry. */
      this->firstA = entry;
   }
}

static void AddToSortedList( AstKeyMap *this, AstMapEntry *entry, int *status ){
/*
*  Name:
*     AddToSortedList

*  Purpose:
*     Add an entry into the linked-list of sorted KeyMap entries.

*  Type:
*     Private function.

*  Synopsis:
*     #include "keymap.h"
*     void AddToSortedList( AstKeyMap *this, AstMapEntry *entry, int *status )

*  Class Membership:
*     KeyMap member function.

*  Description:
*     This function adds the supplied MapEntry into the linked list of
*     sorted MapEntries at a position that maintains the sorted order.

*  Parameters:
*     this
*        Pointer to the KeyMap.
*     entry
*        Pointer to the MapEntry to be added.
*     status
*        Pointer to the inherited status variable.

*/

/* Local Variables: */
   AstMapEntry *hi;         /* MapEntry at high end of current range */
   AstMapEntry *lo;         /* MapEntry at low end of current range */
   AstMapEntry *mid;        /* MapEntry at middle of current range */
   int cmp;                 /* Result of comparing two entries */
   int istep;               /* Step counter */
   int nstep;               /* Number of entries in current range */
   int sortby;              /* How to sort the keys */

/* Check the global error status. */
   if ( !astOK ) return;

/* Get the SortBy value. */
   sortby = astGetSortBy( this );

/* Do nothing if no sorting is required. */
   if( sortby != SORTBY_NONE ) {

/* Get pointers to the entries at the start and end of the sorted list. */
      lo = this->first;
      hi = lo ? lo->sprev : NULL;

/* Store sortby value in the mapentry structures. */
      if( lo ) lo->sortby = sortby;
      if( hi ) hi->sortby = sortby;
      entry->sortby = sortby;

/* If the sorted list is empty, just store the supplied entry at the
   head, and set the links to point back to itself. */
      if( !lo ) {
         this->first = entry;
         entry->sprev = entry;
         entry->snext = entry;

/* If the new entry comes before the first entry or is equal to the first
   entry, record it as the new first entry, and insert it into the linked
   list before the original first entry. */
      } else if( CompareEntries( &entry, &lo ) <= 0 ) {
         this->first = entry;
         entry->snext = lo;
         entry->sprev = hi;
         lo->sprev = entry;
         hi->snext = entry;

/* If the new entry comes after the last entry or is equal to the last
   entry, insert it into the linked list after the last entry. */
      } else if( CompareEntries( &entry, &hi ) >= 0 ) {
         entry->snext = lo;
         entry->sprev = hi;
         lo->sprev = entry;
         hi->snext = entry;

/* If the list only contains two values, insert the new entry into the linked
   list between the existing two entries. */
      } else if( lo->snext == hi ) {
         entry->snext = hi;
         entry->sprev = lo;
         lo->snext = entry;
         hi->sprev = entry;

/* Otherwise we do a binary chop within the existing sorted list to find the
   correct position for the new entry. */
      } else {

/* Get a pointer to the entry mid way between the hi and lo entries. The
   mid entry will be on the upper side of half way if there are an even
   number of entries. */
         nstep = this->nsorted/2;
         mid = lo;
         for( istep = 0; istep < nstep; istep++ ) mid = mid->snext;

/* Loop until we have a pointer to the first entry which is equal to or
   higher than the new entry. */
         while( lo->snext != hi ) {

/* The next step will be half the length of the previous step. Do not
   allow the step size to fall to zero. */
            nstep = ( nstep > 1 ) ? nstep/2 : 1;

/* Compare the new entry with the current mid-way entry. */
            mid->sortby = sortby;
            cmp = CompareEntries( &entry, &mid );

/* If the new entry comes before the mid entry, use the mid entry as the
   next hi entry, and go down the list by the new step size to find the
   new mid-way entry. */
            if( cmp < 0 ) {
               hi = mid;
               for( istep = 0; istep < nstep; istep++ ) mid = mid->sprev;

/* If the new entry comes after the mid entry, use the mid entry as the
   next lo entry, and go up the list by the new step size to find the
   new mid-way entry. */
            } else if( cmp > 0 ) {
               lo = mid;
               for( istep = 0; istep < nstep; istep++ ) mid = mid->snext;

/* If the new entry is equal to the mid entry, use the mid entry as hi
   and set lo to the previous entry. This causes the loop to quit. */
            } else {
               hi = mid;
               lo = mid->sprev;
            }
         }

/* Insert the new entry into the list between lo and hi. */
         entry->sprev = lo;
         entry->snext = hi;
         lo->snext = entry;
         hi->sprev = entry;
      }

/* Increment the number of entries in the sorted list. */
      (this->nsorted)++;
   }
}

static void CheckCircle( AstKeyMap *this, AstObject *obj, const char *method, int *status ) {
/*
*  Name:
*     CheckCircle

*  Purpose:
*     Check for circular dependencies between KeyMaps.

*  Type:
*     Private function.

*  Synopsis:
*     #include "keymap.h"
*     void CheckCircle( AstKeyMap *this, AstObject *obj, const char *method, int *status )

*  Class Membership:
*     KeyMap member function.

*  Description:
*     This function checks that the given AstObject is not a KeyMap which
*     contains "this". If it is, an error is reported.

*  Parameters:
*     this
*        The KeyMap pointer.
*     obj
*        Pointer to the AstObject to be inserted into the KeyMap, or NULL.
*     method
*        Name of method to include in error messages.
*     status
*        Pointer to the inherited status variable.

*/

/* Local Variables: */
   AstKeyMap *keymap;       /* The KeyMap being added to "this" */
   AstObject **vec;         /* Pointer to list of AstObject pointers */
   const char *key;         /* The i'th key within second KeyMap */
   int i;                   /* Index of entry within second KeyMap */
   int j;                   /* Index within the vector of values */
   int len;                 /* No. of AST pointers stored in the entry */
   int nkey;                /* No. of entries in the second KeyMap */

/* Check the global error status. */
   if( !astOK ) return;

/* Return if the AstObject is not a KeyMap. */
   if( obj && astIsAKeyMap( obj ) ) {
      keymap = (AstKeyMap *) obj;

/* First check if the supplied Objects are the same. You cannot store a
   KeyMap as an entry within itself. */
      if( keymap == this ) {
         astError( AST__KYCIR, "%s(%s): Cannot add a %s into another "
                   "%s because they are same %s.", status, method,
                   astGetClass( this ), astGetClass( this ),
                   astGetClass( this ), astGetClass( this ) );

/* Otherwise, loop through all the entries in the KeyMap looking for AstObject
   entries. */
      } else {
         nkey = astMapSize( keymap );
         for( i = 0; i < nkey && astOK; i++ ) {
            key = astMapKey( keymap, i );
            if( astMapType( keymap, key ) == AST__OBJECTTYPE ) {

/* Find the number of AstObject pointers stored in this entry, and
   allocate memory to store a copy of the every pointer. */
               len = astMapLength( keymap, key );
               vec = astMalloc( sizeof( AstObject *) * len );
               if( vec ) {

/* Extract pointers to the AstObjects at this entry, and loop round them. */
                  astMapGet1A( keymap, key, len, &len, vec );
                  for( j = 0; j < len; j++ ) {

/* If this entry is a KeyMap, we need to check if is the same as "this"
   or contains "this". */
                     if( astIsAKeyMap( vec[ j ] ) ) {

/* If it is the same as "this", report an error. */
                        if( vec[ j ] == (AstObject *) this ) {
                           astError( AST__KYCIR, "%s(%s): Cannot add a KeyMap "
                                     "into another KeyMap because the first "
                                     "KeyMap contains the second KeyMap.", status,
                                     method, astGetClass( this ) );
                           break;

/* Otherwise, see if it contains "this". */
                        } else {
                           CheckCircle( this, vec[ j ], method, status );
                        }
                     }

/* Free resources. */
                     vec[ j ] = astAnnul( vec[ j ] );
                  }
                  vec = astFree( vec );
               }
            }
         }
      }
   }
}

static void ClearAttrib( AstObject *this_object, const char *attrib, int *status ) {
/*
*  Name:
*     ClearAttrib

*  Purpose:
*     Clear an attribute value for a KeyMap.

*  Type:
*     Private function.

*  Synopsis:
*     #include "keymap.h"
*     void ClearAttrib( AstObject *this, const char *attrib, int *status )

*  Class Membership:
*     KeyMap member function (over-rides the astClearAttrib protected
*     method inherited from the Mapping class).

*  Description:
*     This function clears the value of a specified attribute for a
*     KeyMap, so that the default value will subsequently be used.

*  Parameters:
*     this
*        Pointer to the KeyMap.
*     attrib
*        Pointer to a null-terminated string specifying the attribute
*        name.  This should be in lower case with no surrounding white
*        space.
*     status
*        Pointer to the inherited status variable.
*/

/* Local Variables: */
   AstKeyMap *this;             /* Pointer to the KeyMap structure */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain a pointer to the KeyMap structure. */
   this = (AstKeyMap *) this_object;

/* Check the attribute name and clear the appropriate attribute. */

/* SizeGuess. */
/* ---------- */
   if ( !strcmp( attrib, "sizeguess" ) ) {
      astClearSizeGuess( this );

/* KeyError. */
/* --------- */
   } else if ( !strcmp( attrib, "keyerror" ) ) {
      astClearKeyError( this );

/* KeyCase. */
/* --------- */
   } else if ( !strcmp( attrib, "keycase" ) ) {
      astClearKeyCase( this );

/* MapLocked. */
/* --------- */
   } else if ( !strcmp( attrib, "maplocked" ) ) {
      astClearMapLocked( this );

/* SortBy. */
/* ------- */
   } else if ( !strcmp( attrib, "sortby" ) ) {
      astClearSortBy( this );

/* If the attribute is still not recognised, pass it on to the parent
   method for further interpretation. */
   } else {
      (*parent_clearattrib)( this_object, attrib, status );
   }
}

static void ClearKeyCase( AstKeyMap *this, int *status ) {
/*
*+
*  Name:
*     astClearKeyCase

*  Purpose:
*     Clear the value of the KeyCase attribute for a KeyMap.

*  Type:
*     Protected virtual function.

*  Synopsis:
*     #include "keymap.h"
*     void astSetKeyCase( AstKeyMap *this )

*  Class Membership:
*     KeyMap method.

*  Description:
*     This function Clears the KeyCase attribute of a KeyMap. It reports
*     an error if the KeyMap contains any entries.

*  Parameters:
*     this
*        Pointer to the KeyMap.

*-
*/

/* Local Variables: */
   int defval;                /* Default KeyCase value */
   int itab;                  /* Index into hash table */
   int oldval;                /* Old KeyCase value */

/* Check the global error status. */
   if ( !astOK ) return;

/* Save the old value. */
   oldval = astGetKeyCase( this );

/* Clear it. */
   this->keycase = -1;

/* Get the default value. */
   defval = astGetKeyCase( this );

/* If the old value and the default value are not the same, we must check
   that the KeyMap is empty. If not, restore the old value and report an
   error. */
   if( defval != oldval ) {
      for( itab = 0; itab < this->mapsize; itab++ ) {
         if( this->nentry[ itab ] > 0 ) {
            this->keycase = oldval;
            astError( AST__NOWRT, "astClearAttrib(KeyMap): Illegal attempt to "
                      "clear the KeyCase attribute of a non-empty KeyMap.",
                      status);
            break;
         }
      }
   }
}

static void ClearKeyError( AstKeyMap *this, int *status ) {
/*
*+
*  Name:
*     astClearKeyError

*  Purpose:
*     Clear the value of the KeyError attribute for a KeyMap.

*  Type:
*     Protected virtual function.

*  Synopsis:
*     #include "keymap.h"
*     void astClearKeyError( AstKeyMap *this )

*  Class Membership:
*     KeyMap method.

*  Description:
*     This function clears the value of the KeyError attribute for a
*     KeyMap. It clears the attribute recursively in any KeyMaps
*     contained within the supplied KeyMap.

*  Parameters:
*     this
*        Pointer to the KeyMap.

*-
*/

/* Local Variables: */
   AstMapEntry *next;     /* Pointer to next Entry to copy */
   AstObject **obj_list;  /* List of pointers to AST Object entries */
   int i;                 /* Index into hash table */
   int iel;               /* Index of current vector element */
   int nel;               /* Number of elements in vector */

/* Check the global error status. */
   if ( !astOK ) return;

/* Clear the KeyError value in the supplied KeyMap. */
   this->keyerror = -INT_MAX;

/* Loop round each entry in the hash table. */
   for( i = 0; i < this->mapsize; i++ ) {

/* Get a pointer to the next KeyMap entry. */
      next = this->table[ i ];

/* Loop round all entries in this element of the hash table. */
      while( next && astOK ) {

/* If this entry has an Object data type, see if holds any KeyMaps. */
         if( next->type == AST__OBJECTTYPE ) {

/* Get the number of objects to check, and a pointer to the first. */
            nel = next->nel;
            if( nel == 0 ) {
               obj_list = &( ((Entry0A *)next)->value );
               nel = 1;
            } else {
               obj_list = ((Entry1A *)next)->value;
            }

/* Loop round checking all Objects. */
            for( iel = 0; iel < nel; iel++ ) {

/* If this Object is a KeyMap, clear its KeyError attribute. */
               if( astIsAKeyMap( obj_list[ iel ] ) ) {
                  astClearKeyError( (AstKeyMap *) obj_list[ iel ] );
               }
            }
         }

/* Get a pointer to the next entry. */
         next = next->next;
      }
   }
}

static void ClearMapLocked( AstKeyMap *this, int *status ) {
/*
*+
*  Name:
*     astClearMapLocked

*  Purpose:
*     Clear the value of the MapLocked attribute for a KeyMap.

*  Type:
*     Protected virtual function.

*  Synopsis:
*     #include "keymap.h"
*     void astClearMapLocked( AstKeyMap *this )

*  Class Membership:
*     KeyMap method.

*  Description:
*     This function clears the value of the MapLocked attribute for a
*     KeyMap. It clears the attribute recursively in any KeyMaps
*     contained within the supplied KeyMap.

*  Parameters:
*     this
*        Pointer to the KeyMap.

*-
*/

/* Local Variables: */
   AstMapEntry *next;     /* Pointer to next Entry to copy */
   AstObject **obj_list;  /* List of pointers to AST Object entries */
   int i;                 /* Index into hash table */
   int iel;               /* Index of current vector element */
   int nel;               /* Number of elements in vector */

/* Check the global error status. */
   if ( !astOK ) return;

/* Clear the MapLocked value in the supplied KeyMap. */
   this->maplocked = -INT_MAX;

/* Loop round each entry in the hash table. */
   for( i = 0; i < this->mapsize; i++ ) {

/* Get a pointer to the next KeyMap entry. */
      next = this->table[ i ];

/* Loop round all entries in this element of the hash table. */
      while( next && astOK ) {

/* If this entry has an Object data type, see if holds any KeyMaps. */
         if( next->type == AST__OBJECTTYPE ) {

/* Get the number of objects to check, and a pointer to the first. */
            nel = next->nel;
            if( nel == 0 ) {
               obj_list = &( ((Entry0A *)next)->value );
               nel = 1;
            } else {
               obj_list = ((Entry1A *)next)->value;
            }

/* Loop round checking all Objects. */
            for( iel = 0; iel < nel; iel++ ) {

/* If this Object is a KeyMap, clear its MapLocked attribute. */
               if( astIsAKeyMap( obj_list[ iel ] ) ) {
                  astClearMapLocked( (AstKeyMap *) obj_list[ iel ] );
               }
            }
         }

/* Get a pointer to the next entry. */
         next = next->next;
      }
   }
}

static void ClearSizeGuess( AstKeyMap *this, int *status ) {
/*
*+
*  Name:
*     astClearSizeGuess

*  Purpose:
*     Clear the value of the SizeGuess attribute for a KeyMap.

*  Type:
*     Protected virtual function.

*  Synopsis:
*     #include "keymap.h"
*     void astClearSizeGuess( AstKeyMap *this )

*  Class Membership:
*     KeyMap method.

*  Description:
*     This function clears the value of the SizeGuess attribute for a
*     KeyMap. It reports an error if the KeyMap contains any entries.

*  Parameters:
*     this
*        Pointer to the KeyMap.

*-
*/

/* Local Variables: */
   int empty;                 /* Is the KeyMap empty? */
   int itab;                  /* Index into hash table */

/* Check the global error status. */
   if ( !astOK ) return;

/* See if the KeyMap is empty. */
   empty = 1;
   for( itab = 0; itab < this->mapsize; itab++ ) {
      if( this->nentry[ itab ] > 0 ) {
         empty = 0;
         break;
      }
   }

/* If not report an error. */
   if( !empty ) {
      astError( AST__NOWRT, "astClearAttrib(KeyMap): Illegal attempt to "
                "clear the SizeGuess attribute of a non-empty KeyMap." , status);

/* Otherwise, store the "cleared" value and change the size of the hash
   table. */
   } else {
      this->sizeguess = INT_MAX;
      NewTable( this, MIN_TABLE_SIZE, status );
   }
}

static void ClearSortBy( AstKeyMap *this, int *status ) {
/*
*+
*  Name:
*     astClearSortBy

*  Purpose:
*     Clear the value of the SortBy attribute for a KeyMap.

*  Type:
*     Protected virtual function.

*  Synopsis:
*     #include "keymap.h"
*     void astClearSortBy( AstKeyMap *this )

*  Class Membership:
*     KeyMap method.

*  Description:
*     This function clears the value of the SortBy attribute for a
*     KeyMap.

*  Parameters:
*     this
*        Pointer to the KeyMap.

*-
*/

/* Local Variables: */
   int oldval;            /* The old sortby value */

/* Check the global error status. */
   if ( !astOK ) return;

/* Get the original SortBy value. */
   oldval = astGetSortBy( this );

/* Clear the SortBy value in the supplied KeyMap. */
   this->sortby = -INT_MAX;

/* If the value has changed, re-sort the keys. */
   if( oldval != astGetSortBy( this ) ) SortEntries( this, status );
}

static int CompareEntries( const void *first_void, const void *second_void ) {
/*
*  Name:
*     CompareEntries

*  Purpose:
*     Determine the sorting order of two mapEntries.

*  Type:
*     Private function.

*  Synopsis:
*     #include "keymap.h"
*     int CompareEntries( const void *first, const void *second )

*  Class Membership:
*     KeyMap member function.

*  Description:
*     This function returns a value indicating if the first MapEntry
*     is less than, equal to, or greater than the second MapEntry using
*     the indicated sorting method. It is designed for use with the
*     "qsort" function, and therefore must used "void *" pointers.

*  Parameters:
*     first
*        Pointer to the address of the first MapEntry.
*     second
*        Pointer to the address of the second MapEntry.

*  Returned Value:
*     -1 if "first" is less than "second". This implies that "first"
*     should come before "second" in the sorted list.
*
*     0 if "first" is equal to "second".
*
*     +1 if "first" is greater than "second". This implies that "first"
*     should come after "second" in the sorted list.

*/

/* Local Variables: */
   AstMapEntry *first;   /* Pointer to first MapEntry structure */
   AstMapEntry *second;  /* Pointer to second MapEntry structure */
   int result;           /* Returned value */
   int sortby;           /* Sorting method */

/* Initialise returned value */
   result = 0;

/* Get pointers to the MapEntry structures, and get the sorting method. */
   first = *( (AstMapEntry **) first_void );
   second = *( (AstMapEntry **) second_void );
   sortby = first->sortby;

/* First handle sorting by increasing age of the value */
   if( sortby == SORTBY_AGEUP ) {
      if( first->member < second->member ) {
         result = 1;
      } else if( first->member > second->member ) {
         result = -1;
      } else {
         result = 0;
      }

/* Next handle sorting by decreasing age of the value */
   } else if( sortby == SORTBY_AGEDOWN ) {
      if( first->member < second->member ) {
         result = -1;
      } else if( first->member > second->member ) {
         result = 1;
      } else {
         result = 0;
      }

/* Next handle sorting by increasing age of the key */
   } else if( sortby == SORTBY_KEYAGEUP ) {
      if( first->keymember < second->keymember ) {
         result = 1;
      } else if( first->keymember > second->keymember ) {
         result = -1;
      } else {
         result = 0;
      }

/* Next handle sorting by decreasing age of the key */
   } else if( sortby == SORTBY_KEYAGEDOWN ) {
      if( first->keymember < second->keymember ) {
         result = -1;
      } else if( first->keymember > second->keymember ) {
         result = 1;
      } else {
         result = 0;
      }

/* Next handle sorting by increasing alphabetical position. */
   } else if( sortby == SORTBY_KEYUP ) {
      result = KeyCmp( first->key, second->key );

/* Next handle sorting by decreasing alphabetical position. */
   } else if( sortby == SORTBY_KEYDOWN ) {
      result = KeyCmp( second->key, first->key );

   }

/* Return the result. */
   return result;

}

static const char *ConvertKey( AstKeyMap *this, const char *skey, char *keybuf,
                               int blen, const char *method, int *status ){
/*
*  Name:
*     ConvertValue

*  Purpose:
*     Convert the supplied key to upper case if required.

*  Type:
*     Private function.

*  Synopsis:
*     #include "keymap.h"
*     const char *ConvertKey( AstKeyMap *this, const char *skey, char *keybuf,
*                             int blen, const char *method, int *status )

*  Class Membership:
*     KeyMap member function.

*  Description:
*     This function converts the supplied key string to uppercase if the
*     KeyCase attribute it currently set to zero in the supplied KeyMap.

*  Parameters:
*     this
*        Pointer to the KeyMap.
*     skey
*        Pointer to the supplied key string.
*     keybuf
*        Pointer to a buffer in which to place the converted string. This
*        will only be used if the supplied key string needs to be
*        converted.
*     blen
*        The length of the "keybuf" buffer. This should include room for
*        a terminating null character.
*     method
*        Pointer to a string holding the name of the method to include in
*        any error message.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     If the KeyMap's KeyCase attribute is currently set to a non-zero
*     value, the returned value will be a copy of "skey". Otherwise it
*     will be copy of "keybuf" (the buffer holding the upper case version
*     of the supplied string).

*  Notes:
*     - The valeu of "skey" will be returned if this function is invoked
*     with the global error status set, or if it should fail for any reason.
*/

/* Local Variables: */
   const char *result;
   int len;

/* Initialise. */
   result = skey;

/* Check the global error status and the supplied pointers. */
   if( !astOK ) return result;

/* If the KeyCase attribute is non-zero, return "skey". Otherwise, convert
   the "skey" string to upper case and return "keybuf". Report an error if
   the key is too long. */
   if( !astGetKeyCase( this ) && astOK ) {
      len = astChrLen( skey );
      if( len >= blen ) {
         astError( AST__BIGKEY, "%s(%s): Supplied key '%s' is too long "
                   "(keys must be no more than %d characters long).",
                   status, method, astGetClass( this ), skey, blen - 1 );
      } else {
         astChrCase( skey, keybuf, 1, blen );
         result = keybuf;
      }
   }

/* Return the result. */
   return result;
}

static int ConvertValue( void *raw, int raw_type, void *out, int out_type, int *status ) {
/*
*  Name:
*     ConvertValue

*  Purpose:
*     Convert a value from one KeyMap data type to another.

*  Type:
*     Private function.

*  Synopsis:
*     #include "keymap.h"
*     int ConvertValue( void *raw, int raw_type, void *out, int out_type, int *status )

*  Class Membership:
*     KeyMap member function.

*  Description:
*     This function converts a supplied value from one KeyMap data type to
*     another, if possible.

*  Parameters:
*     raw
*        Pointer to input value.
*     raw_type
*        The data type of the input value.
*     out
*        Pointer to the location at which to store the output value. This
*        may be NULL, in which case the conversion is still performed if
*        possible, but the result of the conversion is thrown away.
*     out_type
*        The data type of the output value.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     Non-zero if the conversion was performed succesfully, otherwise zero.
*     In the case of the output type being AST__STRINGTYPE, the returned
*     non-zero value will be the length of the formatted string (including
*     the terminating null character). This value will be returned correctly
*     even if "out" is NULL.

*  Notes:
*     - Zero will be returned if this function is invoked with the global
*     error status set, or if it should fail for any reason.
*/

/* Local Variables: */
   AstObject *aval;              /* AstObject pointer value */
   astDECLARE_GLOBALS            /* Declare the thread specific global data */
   const char *cval;             /* Pointer to string value */
   const char *cvalue;           /* Pointer to output string value */
   double dval;                  /* Double precision value */
   float fval;                   /* Single precision value */
   int i;                        /* Loop count */
   int ival;                     /* Integer value */
   int n1;                       /* Number of characters at reduced precision */
   int n2;                       /* Number of characters at full precision */
   int nc;                       /* Number of characters read from string */
   int nval;                     /* Number of values read from string */
   int result;                   /* Returned flag */
   short int sval;               /* Short int value */
   unsigned char bval;           /* Byte value */

/* Initialise. */
   result = 0;

/* Check the global error status and the supplied pointers. */
   if( !astOK || !raw ) return result;

/* Get a pointer to the structure holding thread-specific global data. */
   astGET_GLOBALS(NULL);

/* If the "convertvalue_strings" array has not been initialised, fill it with
   NULL pointers. */
   if( !convertvalue_init ) {
      convertvalue_init = 1;
      for( i = 0; i < AST__KEYMAP_CONVERTVALUE_MAX_STRINGS; i++ ) convertvalue_strings[ i ] = NULL;
   }

/* Assume conversion is possible */
   result = 1;
   cvalue = NULL;

/* Do nothing if both data types are AST__UNDEFTYPE. */
    if( raw_type == AST__UNDEFTYPE && out_type == AST__UNDEFTYPE ) {

/* Indicate failure if one of the two types is AST__UNDEFTYPE and the
   other is not. */
    } else if( raw_type == AST__UNDEFTYPE || out_type == AST__UNDEFTYPE ) {
      result = 0;

/* Otherwise, consider conversion from "int". */
    } else if( raw_type == AST__INTTYPE ) {
      ival = *( (int *) raw );

/* Consider conversion to "int". */
      if( out_type == AST__INTTYPE ) {
         if( out ) *( (int *) out ) = ival;

/* Consider conversion to "short int". */
      } else if( out_type == AST__SINTTYPE ) {
         if( out ) *( (short int *) out ) = (short int) ival;

/* Consider conversion to "byte". */
      } else if( out_type == AST__BYTETYPE ) {
         if( out ) *( (unsigned char *) out ) = (unsigned char) ival;

/* Consider conversion to "float". */
      } else if( out_type == AST__FLOATTYPE ) {
         if( out ) *( (float *) out ) = (float) ival;

/* Consider conversion to "double". */
      } else if( out_type == AST__DOUBLETYPE ) {
         if( out ) *( (double *) out ) = (double) ival;

/* Consider conversion to "const char *". */
      } else if( out_type == AST__STRINGTYPE ) {
         (void) sprintf( convertvalue_buff, "%d", ival );
         cvalue = convertvalue_buff;

/* Consider conversion to "AstObject *". */
      } else if( out_type == AST__OBJECTTYPE ) {
         result = 0;

/* Consider conversion to "void *". */
      } else if( out_type == AST__POINTERTYPE ) {
         result = 0;

/* Report an error if the data type is unknown. */
      } else {
         result = 0;
         astError( AST__INTER, "ConvertValue(KeyMap): Illegal map entry data "
                   "type %d encountered (internal AST programming error).", status,
                   out_type );
      }

/* Otherwise, consider conversion from "short int". */
    } else if( raw_type == AST__SINTTYPE ) {
      sval = *( (short int *) raw );

/* Consider conversion to "int". */
      if( out_type == AST__INTTYPE ) {
         if( out ) *( (int *) out ) = sval;

/* Consider conversion to "short int". */
      } else if( out_type == AST__SINTTYPE ) {
         if( out ) *( (short int *) out ) = sval;

/* Consider conversion to "byte". */
      } else if( out_type == AST__BYTETYPE ) {
         if( out ) *( (unsigned char *) out ) = sval;

/* Consider conversion to "float". */
      } else if( out_type == AST__FLOATTYPE ) {
         if( out ) *( (float *) out ) = (float) sval;

/* Consider conversion to "double". */
      } else if( out_type == AST__DOUBLETYPE ) {
         if( out ) *( (double *) out ) = (double) sval;

/* Consider conversion to "const char *". */
      } else if( out_type == AST__STRINGTYPE ) {
         (void) sprintf( convertvalue_buff, "%d", (int) sval );
         cvalue = convertvalue_buff;

/* Consider conversion to "AstObject *". */
      } else if( out_type == AST__OBJECTTYPE ) {
         result = 0;

/* Consider conversion to "void *". */
      } else if( out_type == AST__POINTERTYPE ) {
         result = 0;

/* Report an error if the data type is unknown. */
      } else {
         result = 0;
         astError( AST__INTER, "ConvertValue(KeyMap): Illegal map entry data "
                   "type %d encountered (internal AST programming error).", status,
                   out_type );
      }

/* Otherwise, consider conversion from "byte". */
    } else if( raw_type == AST__BYTETYPE ) {
      bval = *( (unsigned char *) raw );

/* Consider conversion to "int". */
      if( out_type == AST__INTTYPE ) {
         if( out ) *( (int *) out ) = bval;

/* Consider conversion to "short int". */
      } else if( out_type == AST__SINTTYPE ) {
         if( out ) *( (short int *) out ) = bval;

/* Consider conversion to "byte". */
      } else if( out_type == AST__BYTETYPE ) {
         if( out ) *( (unsigned char *) out ) = bval;

/* Consider conversion to "float". */
      } else if( out_type == AST__FLOATTYPE ) {
         if( out ) *( (float *) out ) = (float) bval;

/* Consider conversion to "double". */
      } else if( out_type == AST__DOUBLETYPE ) {
         if( out ) *( (double *) out ) = (double) bval;

/* Consider conversion to "const char *". */
      } else if( out_type == AST__STRINGTYPE ) {
         (void) sprintf( convertvalue_buff, "%d", (int) bval );
         cvalue = convertvalue_buff;

/* Consider conversion to "AstObject *". */
      } else if( out_type == AST__OBJECTTYPE ) {
         result = 0;

/* Consider conversion to "void *". */
      } else if( out_type == AST__POINTERTYPE ) {
         result = 0;

/* Report an error if the data type is unknown. */
      } else {
         result = 0;
         astError( AST__INTER, "ConvertValue(KeyMap): Illegal map entry data "
                   "type %d encountered (internal AST programming error).", status,
                   out_type );
      }

/* Consider conversion from "double". */
   } else if( raw_type == AST__DOUBLETYPE ) {
      dval = *( (double *) raw );

/* Consider conversion to "int". */
      if( out_type == AST__INTTYPE ) {
         if( out ) *( (int *) out ) = (int)( dval + 0.5 );

/* Consider conversion to "short int". */
      } else if( out_type == AST__SINTTYPE ) {
         if( out ) *( (short int *) out ) = (int)( dval + 0.5 );

/* Consider conversion to "byte". */
      } else if( out_type == AST__BYTETYPE ) {
         if( out ) *( (unsigned char *) out ) = (int)( dval + 0.5 );

/* Consider conversion to "double". */
      } else if( out_type == AST__DOUBLETYPE ) {
         if( out ) *( (double *) out ) = dval;

/* Consider conversion to "float". */
      } else if( out_type == AST__FLOATTYPE ) {
         if( out ) *( (float *) out ) = (float) dval;

/* Consider conversion to "const char *". If reducing the number of
   decimal places by two produces a saving of 10 or more characters,
   assume the least significant two characters are rounding error. */
      } else if( out_type == AST__STRINGTYPE ) {
         if( dval != AST__BAD ) {
            n1 = sprintf( convertvalue_buff, "%.*g", DBL_DIG - 2, dval );
            n2 = sprintf( convertvalue_buff, "%.*g", DBL_DIG, dval );
            if( n2 - n1 > 9 ) {
               (void) sprintf( convertvalue_buff, "%.*g", DBL_DIG - 2, dval );
            }
            cvalue = convertvalue_buff;
         } else {
            cvalue = BAD_STRING;
         }

/* Consider conversion to "AstObject *". */
      } else if( out_type == AST__OBJECTTYPE ) {
         result = 0;

/* Consider conversion to "void *". */
      } else if( out_type == AST__POINTERTYPE ) {
         result = 0;

/* Report an error if the data type is unknown. */
      } else {
         result = 0;
         astError( AST__INTER, "ConvertValue(KeyMap): Illegal map entry data "
                   "type %d encountered (internal AST programming error).", status,
                   out_type );
      }

/* Consider conversion from "float". */
   } else if( raw_type == AST__FLOATTYPE ) {
      fval = *( (float *) raw );

/* Consider conversion to "int". */
      if( out_type == AST__INTTYPE ) {
         if( out ) *( (int *) out ) = (int)( fval + 0.5 );

/* Consider conversion to "short int". */
      } else if( out_type == AST__SINTTYPE ) {
         if( out ) *( (short int *) out ) = (int)( fval + 0.5 );

/* Consider conversion to "byte". */
      } else if( out_type == AST__BYTETYPE ) {
         if( out ) *( (unsigned char *) out ) = (int)( fval + 0.5 );

/* Consider conversion to "double". */
      } else if( out_type == AST__DOUBLETYPE ) {
         if( out ) *( (double *) out ) = (double) fval;

/* Consider conversion to "float". */
      } else if( out_type == AST__FLOATTYPE ) {
         if( out ) *( (float *) out ) = fval;

/* Consider conversion to "const char *". */
      } else if( out_type == AST__STRINGTYPE ) {
         (void) sprintf( convertvalue_buff, "%.*g", FLT_DIG, fval );
         cvalue = convertvalue_buff;

/* Consider conversion to "AstObject *". */
      } else if( out_type == AST__OBJECTTYPE ) {
         result = 0;

/* Consider conversion to "void *". */
      } else if( out_type == AST__POINTERTYPE ) {
         result = 0;

/* Report an error if the data type is unknown. */
      } else {
         result = 0;
         astError( AST__INTER, "ConvertValue(KeyMap): Illegal map entry data "
                   "type %d encountered (internal AST programming error).", status,
                   out_type );
      }

/* Consider conversion from "const char *". */
   } else if( raw_type == AST__STRINGTYPE ) {
      cval = *( (const char **) raw );

/* Consider conversion to "int". */
      if( out_type == AST__INTTYPE ) {
         nc = 0;
         nval = astSscanf( cval, " %d %n", &ival, &nc );
         if( ( nval == 1 ) && ( nc >= (int) strlen( cval ) ) ) {
            if( out ) *( (int *) out ) = ival;
         } else {
            nc = 0;
            nval = astSscanf( cval, " %lf %n", &dval, &nc );
            if( ( nval == 1 ) && ( nc >= (int) strlen( cval ) ) ) {
               if( out ) *( (int *) out ) = (int) ( dval + 0.5 );
            } else {
               result = 0;
            }
         }

/* Consider conversion to "short int". */
      } else if( out_type == AST__SINTTYPE ) {
         nc = 0;
         nval = astSscanf( cval, " %d %n", &ival, &nc );
         if( ( nval == 1 ) && ( nc >= (int) strlen( cval ) ) ) {
            if( out ) *( (short int *) out ) = ival;
         } else {
            nc = 0;
            nval = astSscanf( cval, " %lf %n", &dval, &nc );
            if( ( nval == 1 ) && ( nc >= (int) strlen( cval ) ) ) {
               if( out ) *( (short int *) out ) = (int) ( dval + 0.5 );
            } else {
               result = 0;
            }
         }

/* Consider conversion to "byte". */
      } else if( out_type == AST__BYTETYPE ) {
         nc = 0;
         nval = astSscanf( cval, " %d %n", &ival, &nc );
         if( ( nval == 1 ) && ( nc >= (int) strlen( cval ) ) ) {
            if( out ) *( (unsigned char *) out ) = ival;
         } else {
            nc = 0;
            nval = astSscanf( cval, " %lf %n", &dval, &nc );
            if( ( nval == 1 ) && ( nc >= (int) strlen( cval ) ) ) {
               if( out ) *( (unsigned char *) out ) = (int) ( dval + 0.5 );
            } else {
               result = 0;
            }
         }

/* Consider conversion to "double". */
      } else if( out_type == AST__DOUBLETYPE ) {
         nc = 0;
         nval = astSscanf( cval, " " BAD_STRING " %n", &nc );
         if( ( astSscanf( cval, " " BAD_STRING " %n", &nc ) == 0 ) &&
             ( nc >= (int) strlen( cval ) ) ) {
            if( out ) *( (double *) out ) = AST__BAD;

         } else if( ( astSscanf( cval, " %lf %n", &dval, &nc ) == 1 ) &&
                     ( nc >= (int) strlen( cval ) ) ) {
            if( out ) *( (double *) out ) = dval;

         } else {
            result = 0;
         }

/* Consider conversion to "float". */
      } else if( out_type == AST__FLOATTYPE ) {
         nc = 0;
         nval = astSscanf( cval, " %f %n", &fval, &nc );
         if( ( nval == 1 ) && ( nc >= (int) strlen( cval ) ) ) {
            if( out ) *( (float *) out ) = fval;
         } else {
            result = 0;
         }

/* Consider conversion to "const char *". */
      } else if( out_type == AST__STRINGTYPE ) {
         cvalue = cval;

/* Consider conversion to "AstObject *". */
      } else if( out_type == AST__OBJECTTYPE ) {
         result = 0;

/* Consider conversion to "void *". */
      } else if( out_type == AST__POINTERTYPE ) {
         result = 0;

/* Report an error if the data type is unknown. */
      } else {
         result = 0;
         astError( AST__INTER, "ConvertValue(KeyMap): Illegal map entry data "
                   "type %d encountered (internal AST programming error).", status,
                   out_type );
      }

/* Consider conversion from "AstObject *". */
   } else if( raw_type == AST__OBJECTTYPE ) {

/* Consider conversion to "int". */
      if( out_type == AST__INTTYPE ) {
         result = 0;

/* Consider conversion to "short int". */
      } else if( out_type == AST__SINTTYPE ) {
         result = 0;

/* Consider conversion to "byte". */
      } else if( out_type == AST__BYTETYPE ) {
         result = 0;

/* Consider conversion to "double". */
      } else if( out_type == AST__DOUBLETYPE ) {
         result = 0;

/* Consider conversion to "float". */
      } else if( out_type == AST__FLOATTYPE ) {
         result = 0;

/* Consider conversion to "const char *". */
      } else if( out_type == AST__STRINGTYPE ) {
         result = 0;

/* Consider conversion to "AstObject *". */
      } else if( out_type == AST__OBJECTTYPE ) {
         aval = *( (AstObject **) raw );
         if( out ) *( (AstObject **) out ) = aval ? astClone( aval ) : NULL;

/* Consider conversion to "void *". */
      } else if( out_type == AST__POINTERTYPE ) {
         result = 0;

/* Report an error if the data type is unknown. */
      } else {
         result = 0;
         astError( AST__INTER, "ConvertValue(KeyMap): Illegal map entry data "
                   "type %d encountered (internal AST programming error).", status,
                   out_type );
      }

/* Consider conversion from "void *". */
   } else if( raw_type == AST__POINTERTYPE ) {

/* Consider conversion to "int". */
      if( out_type == AST__INTTYPE ) {
         result = 0;

/* Consider conversion to "short int". */
      } else if( out_type == AST__SINTTYPE ) {
         result = 0;

/* Consider conversion to "byte". */
      } else if( out_type == AST__BYTETYPE ) {
         result = 0;

/* Consider conversion to "double". */
      } else if( out_type == AST__DOUBLETYPE ) {
         result = 0;

/* Consider conversion to "float". */
      } else if( out_type == AST__FLOATTYPE ) {
         result = 0;

/* Consider conversion to "const char *". */
      } else if( out_type == AST__STRINGTYPE ) {
         result = 0;

/* Consider conversion to "AstObject *". */
      } else if( out_type == AST__OBJECTTYPE ) {
         result = 0;

/* Consider conversion to "void *". */
      } else if( out_type == AST__POINTERTYPE ) {
         if( out ) *( (void **) out ) = *( (void **) raw );

/* Report an error if the data type is unknown. */
      } else {
         result = 0;
         astError( AST__INTER, "ConvertValue(KeyMap): Illegal map entry data "
                   "type %d encountered (internal AST programming error).", status,
                   out_type );
      }

/* Report an error if the data type is unknown. */
   } else {
      result = 0;
      astError( AST__INTER, "ConvertValue(KeyMap): Illegal map entry data "
                "type %d encountered (internal AST programming error).", status,
                raw_type );
   }

/* If the output is a string, store a copy of the resulting string in
   dynamically allocated memory, putting a pointer to the copy into the next
   element of the "convertvalue_strings" array.  (This process also de-allocates
   any previously allocated memory pointed at by this "convertvalue_strings"
   element, so the earlier string is effectively replaced by the new
   one.) */
   if( out_type == AST__STRINGTYPE && astOK && result && cvalue ) {
      result = strlen( cvalue ) + 1;

      astBeginPM;
      convertvalue_strings[ convertvalue_istr ] = astStore( convertvalue_strings[ convertvalue_istr ], cvalue,
                                  (size_t) result );
      astEndPM;

/* If OK, return a pointer to the copy and increment "convertvalue_istr" to use the
   next element of "convertvalue_strings" on the next invocation. Recycle "convertvalue_istr" to
   zero when all elements have been used. */
      if ( astOK ) {
         if( out ) *( (const char **) out ) = convertvalue_strings[ convertvalue_istr++ ];
         if( convertvalue_istr == ( AST__KEYMAP_CONVERTVALUE_MAX_STRINGS - 1 ) ) convertvalue_istr = 0;
      }
   }

/* If an error has occurred, return zero. */
   if( !astOK ) result = 0;

/* Return the result. */
   return result;
}


static AstMapEntry *CopyMapEntry( AstMapEntry *in, int *status ){
/*
*  Name:
*     CopyMapEntry

*  Purpose:
*     Produces a copy of the supplied KeyMap entry.

*  Type:
*     Private function.

*  Synopsis:
*     #include "keymap.h"
*     AstMapEntry *CopyMapEntry( AstMapEntry *in, int *status )

*  Class Membership:
*     KeyMap member function.

*  Description:
*     This function creates a deep copy of the supplied KeyMap entry.

*  Parameters:
*     in
*        Pointer to the MapEntry to be copied. NULL may be supplied in
*        which case NULL will be returned.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     Pointer to the new copy. The link to the next MapEntry in the
*     linked list is set NULL in the returned copy.

*  Notes:
*     - A NULL pointer will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*/

/* Local Variables: */
   AstMapEntry *result;   /* Returned pointer */
   AstObject **alist;     /* Pointer to list of AST object pointers */
   AstObject *obj;        /* Pointer to AstObject value */
   const char **slist;    /* Pointer to list of text pointers */
   const char *text;      /* Pointer to text string */
   int i;                 /* Loop count */
   int nel;               /* No. of values in entry vector (0 => scalar) */
   int type;              /* Entry data type */
   size_t size;           /* Size of Entry structure */

/* Initialise. */
   result = NULL;

/* Check the global error status and the supplied pointer. */
   if ( !astOK || !in ) return result;

/* Get the size, data type and length of the MapEntry. */
   size = SizeOfEntry( in, status );
   nel = in->nel;
   type = in->type;

/* Do a byte-for-byte copy of the supplied MapEntry. */
   result = astStore( NULL, in, size );

/* Copy or nullify pointers in the AstMapEntry structure. */
   result->next = NULL;
   result->snext = NULL;
   result->sprev = NULL;
   text = in->key;
   result->key = text ? astStore( NULL, text, strlen( text ) + 1 ) : NULL;
   text = in->comment;
   result->comment = text ? astStore( NULL, text, strlen( text ) + 1 ) : NULL;

/* Nothing further to do for undefined values. */
   if( type == AST__UNDEFTYPE ) {

/* Next deal with string entries. */
   } else if( type == AST__STRINGTYPE ) {

/* Scalar valued entries... */
      if( nel == 0 ) {

/* Take a copy of the single string in the input entry. */
         text = ( (Entry0C *) in )->value;
         ( (Entry0C *) result )->value = text ? astStore( NULL, text,
                                                strlen( text ) + 1 ) : NULL;
/* Vector valued entries... */
      } else {

/* Allocate an array to store the string pointers. */
         slist = astMalloc( sizeof(char *)*(size_t)nel );
         ( (Entry1C *) result )->value = slist;

/* Copy the strings. */
         if( slist ) {
            for( i = 0; i < nel; i++ ) {
               text = ( (Entry1C *) in )->value[ i ];
               slist[ i ] = text ? astStore( NULL, text, strlen( text ) + 1 ) : NULL;
            }
         }
      }

/* Similarly deal with AST Object entries. */
   } else if( type == AST__OBJECTTYPE ) {
      if( nel == 0 ) {
         obj = ( (Entry0A *) in )->value;
         ( (Entry0A *) result )->value = obj ? astCopy( obj ) : NULL;
         ( (Entry0A *) result )->next = NULL;
         ( (Entry0A *) result )->prev = NULL;
      } else {
         alist = astMalloc( sizeof(AstObject *)*(size_t)nel );
         ( (Entry1A *) result )->value = alist;
         if( alist ) {
            for( i = 0; i < nel; i++ ) {
               obj = ( (Entry1A *) in )->value[ i ];
               alist[ i ] = obj ? astCopy( obj ) : NULL;
            }
            ( (Entry1A *) result )->next = NULL;
            ( (Entry1A *) result )->prev = NULL;
         }
      }

/* Now deal with integer entries. Scalar entries do not need any further
   action. If this is a vector entry copy the values array. */
   } else if( type == AST__INTTYPE ) {
      if( nel > 0 ) {
         ( (Entry1I *) result )->value = astStore( NULL,
                                                  ( (Entry1I *) in )->value,
                                                  sizeof( int )*(size_t)nel );
      }

/* Now deal with short int entries. Scalar entries do not need any further
   action. If this is a vector entry copy the values array. */
   } else if( type == AST__SINTTYPE ) {
      if( nel > 0 ) {
         ( (Entry1S *) result )->value = astStore( NULL,
                                            ( (Entry1S *) in )->value,
                                            sizeof( short int )*(size_t)nel );
      }

/* Now deal with byte entries. Scalar entries do not need any further
   action. If this is a vector entry copy the values array. */
   } else if( type == AST__BYTETYPE ) {
      if( nel > 0 ) {
         ( (Entry1B *) result )->value = astStore( NULL,
                                            ( (Entry1B *) in )->value,
                                            sizeof( unsigned char )*(size_t)nel );
      }

/* Similarly deal with floating point entries. */
   } else if( type == AST__DOUBLETYPE ) {
      if( nel > 0 ) {
         ( (Entry1D *) result )->value = astStore( NULL,
                                                  ( (Entry1D *) in )->value,
                                                  sizeof( double )*(size_t)nel );
      }

   } else if( type == AST__FLOATTYPE ) {
      if( nel > 0 ) {
         ( (Entry1F *) result )->value = astStore( NULL,
                                                  ( (Entry1F *) in )->value,
                                                  sizeof( float )*(size_t)nel );
      }

/* Similarly deal with void pointer entries. */
   } else if( type == AST__POINTERTYPE ) {
      if( nel > 0 ) {
         ( (Entry1P *) result )->value = astStore( NULL,
                                                  ( (Entry1P *) in )->value,
                                                  sizeof( void * )*(size_t)nel );
      }

/* Report an error if the data type is unknown. */
   } else {
      astError( AST__INTER, "CopyMapEntry(KeyMap): Illegal map entry data "
                "type %d encountered (internal AST programming error).", status,
                type );
   }

/* If an error has occurred, attempt to delete the returned MapEntry. */
   if( !astOK ) result = FreeMapEntry( result, status );

/* Return the result. */
   return result;
}

static void CopyTableEntry( AstKeyMap *in, AstKeyMap *out, int itab, int *status ){
/*
*  Name:
*     CopyTableEntry

*  Purpose:
*     Produces a deep copy of a hash table element.

*  Type:
*     Private function.

*  Synopsis:
*     #include "keymap.h"
*     void CopyTableEntry( AstKeyMap *in, AstKeyMap *out, int itab, int *status )

*  Class Membership:
*     KeyMap member function.

*  Description:
*     This function creates a deep copy of the linked-list of KeyMap entries
*     stored in the specified element of the input KeyMaps hash table.

*  Parameters:
*     in
*        Pointer to the input KeyMap.
*     out
*        Pointer to the output KeyMap.
*     itab
*        Index of the hash table element to be copied.
*     status
*        Pointer to the inherited status variable.

*/

/* Local Variables: */
   AstMapEntry **link;    /* Address to store foward link */
   AstMapEntry *next;     /* Pointer to next Entry to copy */

/* Check the global error status. */
   if ( !astOK ) return;

/* The "link" variable holds the address of the location at which the
   pointer to the next copied MapEntry should be stored. Initialise this to
   be the address of the required element of the output hash table. */
   link = &( out->table[ itab ] );

/* The "next" variable holds the address of the next MapEntry to be
   copied. Initialise this to the MapEntry at the head of the linked list
   associated with the specified index of the input KeyMaps hash table. */
   next = in->table[ itab ];

/* If the hash table element is empty, store a null pointer and pass on. */
   if( !next ) {
      out->table[ itab ] = NULL;

/* Otherwise copy the liked list. */
   } else {

/* Loop round until we have copied all entries. */
      while( next && astOK ) {

/* Copy the next entry, storing the resulting pointer at the position
   indicated by "link". */
         *link = CopyMapEntry( next, status );

/* If the entry is of type AST__OBJECTTYPE, add it to the head of the
   list of AST__OBJECTTYPE entries in the output KeyMap. */
         AddToObjectList( out, *link, status );

/* Update "link" and "next" */
         next = next->next;
         link = &( (*link)->next );
      }
   }

/* Set the number of entries in the output to be the same as the input. */
   out->nentry[ itab ] = in->nentry[ itab ];

/* If an error has occurred, attempt to delete the returned MapEntry. */
   if( !astOK ) FreeTableEntry( out, itab, status );
}

static void DoubleTableSize( AstKeyMap *this, int *status ) {
/*
*  Name:
*     DoubleTableSize

*  Purpose:
*     Double the size of the hash table in a KeyMap

*  Type:
*     Private function.

*  Synopsis:
*     #include "keymap.h"
*     void DoubleTableSize( AstKeyMap *this, int *status )

*  Class Membership:
*     KeyMap member function.

*  Description:
*     This function creates a new hash table which has twice as many
*     elements as the current hash table, and moves all the entries out
*     of the old table into the new table (at their new positions).

*  Parameters:
*     this
*        The KeyMap pointer.
*     status
*        Pointer to the inherited status variable.

*/

/* Local Variables: */
   AstMapEntry **newtable;
   AstMapEntry *next;
   AstMapEntry *new_next;
   int *newnentry;
   int bitmask;
   int i;
   int newi;
   int newmapsize;

/* Check the global error status. */
   if( !astOK ) return;

/* Determine the new hash table size. Since mapsize starts out as a power
   of 2 (ensured by the NewTable function), the new mapsize will also be
   a power of 2. Also, create a bit mask that can be used to zero the
   upper bits in a full width hash value. */
   newmapsize = 2*this->mapsize;
   bitmask = newmapsize - 1;

/* Create the new arrays, leaving the old arrays intact for the moment. */
   newtable = astMalloc( newmapsize*sizeof( AstMapEntry * ) );
   newnentry = astMalloc( newmapsize*sizeof( int ) );
   if( astOK ) {

/* Initialise the new table. */
      for( i = 0; i < newmapsize; i++ ) {
         newtable[ i ] = NULL;
         newnentry[ i ] = 0;
      }

/* Loop round each of the existing table entries. */
      for( i = 0; i < this->mapsize; i++ ) {

/* The "next" variable holds the address of the next MapEntry to be
   moved. Initialise this to the MapEntry at the head of the linked list
   associated with the specified index of the input KeyMaps hash table. */
         next = this->table[ i ];

/* Loop round until we have moved all entries. */
         while( next && astOK ) {

/* Find the index within the new table at which to store this entry. */
            newi = ( next->hash & bitmask );

/* Save the pointer to the next entry following the current one in the
   linked list. */
            new_next = next->next;

/* Put a pointer to the MapEntry which is currently at the head of the
   linked list in the "next" component of the current MapEntry. */
            next->next = newtable[ newi ];

/* Store the supplied MapEntry pointer in the requested element of the
   hash table. */
            newtable[ newi ] = next;

/* Increment the length of linked list. */
            newnentry[ newi ]++;

/* Use the pointer to the next map entry to be moved. */
            next = new_next;
         }
      }
   }

/* If OK, delete the existing table and use the new table */
   if( astOK ) {
      this->mapsize = newmapsize;

      (void) astFree( this->table );
      this->table = newtable;

      (void) astFree( this->nentry );
      this->nentry = newnentry;

/* If not OK, delete the new table. */
   } else {
      newtable = astFree( newtable );
      newnentry = astFree( newnentry );
   }
}

static void DumpEntry( AstMapEntry *entry, AstChannel *channel, int nentry, int *status ) {
/*
*  Name:
*     DumpEntry

*  Purpose:
*     Dump a single AstMapEntry to a Channel.

*  Type:
*     Private function.

*  Synopsis:
*     void DumpEntry( AstMapEntry *entry, AstChannel *channel, int nentry )

*  Description:
*     This function dumps the supplied MapEntry to the supplied Channel.

*  Parameters:
*     entry
*        Pointer to the MapEntry whose data are being written.
*     channel
*        Pointer to the Channel to which the data are being written.
*     nentry
*        The integer index value to use when describing the MapEntry in
*        the dump.
*/

/* Local Variables: */
   char buff[20];                /* Buffer for item names */
   const char *com;              /* Pointer to comment string */
   int index;                    /* Index into vector valued entry */
   int nel;                      /* Number of elements in value */
   int type;                     /* Entry data type */

/* Check the global error status. */
   if ( !astOK ) return;

/* Dump the entry key. */
   (void) sprintf( buff, "Key%d", nentry );
   astWriteString( channel, buff, 1, 1, entry->key, "Item name" );

/* Dump the comment if not blank. */
   if( entry->comment && *(entry->comment) ) {
      (void) sprintf( buff, "Com%d", nentry );
      astWriteString( channel, buff, 1, 1, entry->comment, "Item comment" );
   }

/* Get the data type and the length of the Entry. */
   type = entry->type;
   nel = entry->nel;

/* Dump the entry data type. */
   if( type == AST__STRINGTYPE ) {
      com = "Item data type (string)";

   } else if( type == AST__OBJECTTYPE ) {
      com = "Item data type (AST Object)";

   } else if( type == AST__INTTYPE ) {
      com = "Item data type (int)";

   } else if( type == AST__SINTTYPE ) {
      com = "Item data type (short int)";

   } else if( type == AST__BYTETYPE ) {
      com = "Item data type (unsigned byte)";

   } else if( type == AST__DOUBLETYPE ) {
      com = "Item data type (double)";

   } else if( type == AST__FLOATTYPE ) {
      com = "Item data type (float)";

   } else if( type == AST__POINTERTYPE ) {
      com = "Item data type (pointer)";

   } else if( type == AST__UNDEFTYPE ) {
      com = "Item data type (undefined)";

   } else {
      com = "";
      astError( AST__INTER, "DumpEntry(KeyMap): Illegal map entry data "
                "type %d encountered (internal AST programming error).", status,
                type );
   }
   (void) sprintf( buff, "Typ%d", nentry );
   astWriteInt( channel, buff, 1, 1, entry->type, com );

/* Dump the vector length */
   if( entry->nel > 0 ) {
      (void) sprintf( buff, "Nel%d", nentry );
      astWriteInt( channel, buff, 1, 1, entry->nel, "Vector length" );
   }

/* First deal with integer entries. */
   if( type == AST__INTTYPE ) {
      if( entry->nel == 0 ) {
         (void) sprintf( buff, "Val%d", nentry );
         astWriteInt( channel, buff, 1, 1, ((Entry0I *)entry)->value, "Item value" );
      } else {
         com = "Item values";
         for( index = 0; index < nel; index++ ){
            (void) sprintf( buff, "V%d_%d", nentry, index + 1 );
            astWriteInt( channel, buff, 1, 1, ((Entry1I *)entry)->value[ index ], com );
            com = "";
         }
      }

/* Similarly, deal with short int entries. */
   } else if( type == AST__SINTTYPE ) {
      if( entry->nel == 0 ) {
         (void) sprintf( buff, "Val%d", nentry );
         astWriteInt( channel, buff, 1, 1, (int) ((Entry0S *)entry)->value, "Item value" );
      } else {
         com = "Item values";
         for( index = 0; index < nel; index++ ){
            (void) sprintf( buff, "V%d_%d", nentry, index + 1 );
            astWriteInt( channel, buff, 1, 1, (int) ((Entry1S *)entry)->value[ index ], com );
            com = "";
         }
      }

/* Similarly, deal with byte entries. */
   } else if( type == AST__BYTETYPE ) {
      if( entry->nel == 0 ) {
         (void) sprintf( buff, "Val%d", nentry );
         astWriteInt( channel, buff, 1, 1, (int) ((Entry0B *)entry)->value, "Item value" );
      } else {
         com = "Item values";
         for( index = 0; index < nel; index++ ){
            (void) sprintf( buff, "V%d_%d", nentry, index + 1 );
            astWriteInt( channel, buff, 1, 1, (int) ((Entry1B *)entry)->value[ index ], com );
            com = "";
         }
      }

/* Similarly deal with floating point entries. */
   } else if( type == AST__DOUBLETYPE ) {
      if( entry->nel == 0 ) {
         if( ((Entry0D *)entry)->value != AST__BAD ) {
            (void) sprintf( buff, "Val%d", nentry );
            astWriteDouble( channel, buff, 1, 1, ((Entry0D *)entry)->value, "Item value" );
         }
      } else {
         com = "Item values";
         for( index = 0; index < nel; index++ ){
            if( ((Entry1D *)entry)->value[ index ] != AST__BAD ) {
               (void) sprintf( buff, "V%d_%d", nentry, index + 1 );
               astWriteDouble( channel, buff, 1, 1, ((Entry1D *)entry)->value[ index ], com );
               com = "";
            }
         }
      }

/* Similarly deal with single precision floating point entries. */
   } else if( type == AST__FLOATTYPE ) {
      if( entry->nel == 0 ) {
         (void) sprintf( buff, "Val%d", nentry );
         astWriteDouble( channel, buff, 1, 1, (double) ((Entry0F *)entry)->value, "Item value" );
      } else {
         com = "Item values";
         for( index = 0; index < nel; index++ ){
            (void) sprintf( buff, "V%d_%d", nentry, index + 1 );
            astWriteDouble( channel, buff, 1, 1, (double) ((Entry1F *)entry)->value[ index ], com );
            com = "";
         }
      }

/* Do the same for string values. */
   } else if( type == AST__STRINGTYPE ) {
      if( entry->nel == 0 ) {
         (void) sprintf( buff, "Val%d", nentry );
         astWriteString( channel, buff, 1, 1, ((Entry0C *)entry)->value, "Item value" );
      } else {
         com = "Item values";
         for( index = 0; index < nel; index++ ){
            (void) sprintf( buff, "V%d_%d", nentry, index + 1 );
            astWriteString( channel, buff, 1, 1, ((Entry1C *)entry)->value[ index ], com );
            com = "";
         }
      }

/* Do the same for Object values. */
   } else if( type == AST__OBJECTTYPE ) {
      if( entry->nel == 0 ) {
         if( ((Entry0A *)entry)->value ) {
            (void) sprintf( buff, "Val%d", nentry );
            astWriteObject( channel, buff, 1, 1, ((Entry0A *)entry)->value, "Item value" );
         }
      } else {
         com = "Item values";
         for( index = 0; index < nel; index++ ){
            if( ((Entry1A *)entry)->value[ index ] ) {
               (void) sprintf( buff, "V%d_%d", nentry, index + 1 );
               astWriteObject( channel, buff, 1, 1, ((Entry1A *)entry)->value[ index ], com );
               com = "";
            }
         }
      }

/* Void pointer values cannot be dumped. */
   } else if( type == AST__POINTERTYPE ) {
      astError( AST__INTER, "DumpEntry(KeyMap): Cannot dump KeyMaps in "
                "which the values are arbitrary C pointers (possible "
                "programming error)." , status);

/* Nothing further to do for undefined values. */
   } else if( type == AST__UNDEFTYPE ) {

/* Report an error if the data type is unknown. */
   } else if( astOK ) {
      astError( AST__INTER, "DumpEntry(KeyMap): Illegal map entry data "
                "type %d encountered (internal AST programming error).", status,
                type );
   }
}

static AstMapEntry *FreeMapEntry( AstMapEntry *in, int *status ){
/*
*  Name:
*     FreeMapEntry

*  Purpose:
*     Frees the supplied KeyMap entry.

*  Type:
*     Private function.

*  Synopsis:
*     #include "keymap.h"
*     AstMapEntry *FreeMapEntry( AstMapEntry *in, int *status )

*  Class Membership:
*     KeyMap member function.

*  Description:
*     This function frees resources used by the supplied MapEntry, then
*     frees the MapEntry structure itself and returns a NULL pointer.

*  Parameters:
*     in
*        Pointer to the MapEntry to be freed. NULL may be supplied in
*        which the function returns without action.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     A NULL pointer.

*  Notes:
*     - It is the callers responsibility to ensure that any other MapEntry
*     which refers to the supplied MapEntry (e.g. through the "next" link
*     in the MapEntry structure) is modified to take account of the
*     freeing of the supplied MapEntry.
*     - This function attempts to execute even if it is invoked with the
*     global error status set.
*/

/* Local Variables: */
   AstObject **alist;     /* Pointer to list of AST object pointers */
   AstObject *obj;        /* Pointer to AST object */
   const char **slist;    /* Pointer to list of text pointers */
   int i;                 /* Loop count */
   int nel;               /* No. of values in entry vector (0 => scalar) */
   int type;              /* Entry data type */

/* Check the supplied pointer. */
   if( !in ) return NULL;

/* Get the data type and length of the MapEntry. */
   nel = in->nel;
   type = in->type;

/* First deal with string entries. */
   if( type == AST__STRINGTYPE ) {

/* For scalar valued entries, free the single string in the input entry. */
      if( nel == 0 ) {
         ( (Entry0C *) in )->value = (const char *) astFree( ( void *) ( (Entry0C *) in )->value );

/* For vector valued entries, free the strings, then free the array storing
   the string pointers. */
      } else {
         slist = ( (Entry1C *) in )->value;
         if( slist ) {
            for( i = 0; i < nel; i++ ) slist[ i ] = astFree( (void *) slist[ i ] );
            ( (Entry1C *) in )->value = astFree( (void *) slist );
         }
      }

/* Similarly deal with AST Object entries. */
   } else if( type == AST__OBJECTTYPE ) {
      if( nel == 0 ) {
         obj = ( (Entry0A *) in )->value;
         if( obj ) ( (Entry0A *) in )->value = astAnnul( obj );
         ( (Entry0A *) in )->next = NULL;
         ( (Entry0A *) in )->prev = NULL;
      } else {
         alist = ( (Entry1A *) in )->value;
         if( alist ) {
            for( i = 0; i < nel; i++ ) {
               if( alist[ i ] ) alist[ i ] = astAnnul( alist[ i ] );
            }
            ( (Entry1A *) in )->value = astFree( alist );
            ( (Entry1A *) in )->next = NULL;
            ( (Entry1A *) in )->prev = NULL;
         }
      }

/* Now deal with integer entries. Scalar entries do not need any further
   action. If this is a vector entry free the values array. */
   } else if( type == AST__INTTYPE ) {
      if( nel > 0 ) ( (Entry1I *) in )->value = astFree( ( (Entry1I *) in )->value );

/* Now deal with short int entries. Scalar entries do not need any further
   action. If this is a vector entry free the values array. */
   } else if( type == AST__SINTTYPE ) {
      if( nel > 0 ) ( (Entry1S *) in )->value = astFree( ( (Entry1S *) in )->value );

/* Now deal with byte entries. Scalar entries do not need any further
   action. If this is a vector entry free the values array. */
   } else if( type == AST__BYTETYPE ) {
      if( nel > 0 ) ( (Entry1B *) in )->value = astFree( ( (Entry1B *) in )->value );

/* Similarly deal with void * pointer entries. */
   } else if( type == AST__POINTERTYPE ) {
      if( nel > 0 ) ( (Entry1P *) in )->value = astFree( ( (Entry1P *) in )->value );

/* Similarly deal with floating point entries. */
   } else if( type == AST__DOUBLETYPE ) {
      if( nel > 0 ) ( (Entry1D *) in )->value = astFree( ( (Entry1D *) in )->value );

/* Similarly deal with single precision floating point entries. */
   } else if( type == AST__FLOATTYPE ) {
      if( nel > 0 ) ( (Entry1F *) in )->value = astFree( ( (Entry1F *) in )->value );

/* Nothing further to do for undefined values. */
   } else if( type == AST__UNDEFTYPE ) {

/* Report an error if the data type is unknown. */
   } else {
      astError( AST__INTER, "FreeMapEntry(KeyMap): Illegal map entry data "
                "type %d encountered (internal AST programming error).", status,
                type );
   }

/* Free or nullify pointers in the AstMapEntry structure. */
   in->next = NULL;
   in->snext = NULL;
   in->sprev = NULL;
   in->key = astFree( (void *) in->key );
   in->comment = astFree( (void *) in->comment );

/* Free the complete AstMapEntry structure. */
   astFree( in );

/* Return a NULL pointer. */
   return NULL;
}

static void FreeTableEntry( AstKeyMap *this, int itab, int *status ){
/*
*  Name:
*     FreeTableEntry

*  Purpose:
*     Frees the linked list of KeyMap entries stored in a given element of
*     the hash table.

*  Type:
*     Private function.

*  Synopsis:
*     #include "keymap.h"
*     void FreeTableEntry( AstKeyMap *this, int itab, int *status )

*  Class Membership:
*     KeyMap member function.

*  Description:
*     This function frees resources used by all the MapEntries in the
*     linked list associated with the specified element of the hash table
*     of the supplied KeyMap.

*  Parameters:
*     this
*        Pointer to the KeyMap
*     itab
*        Index of the hash table element to free.
*     status
*        Pointer to the inherited status variable.

*  Notes:
*     - This function attempts to execute even if it is invoked with the
*     global error status set.
*/

/* Local Variables: */
   AstMapEntry *link;     /* Pointer the next but one MapEntry to be freed */
   AstMapEntry *next;     /* Pointer the next MapEntry to be freed */

/* Check it is safe to proceed. */
   if( this && itab >= 0 && itab < this->mapsize ) {

/* Store a pointer to the MapEntry which is to be freed next. */
      next = this->table[ itab ];

/* Loop round freeing all MapEntries in the linked list. */
      while( next ) {

/* Store a pointer to the MapEntry which will be freed after this one. */
         link = next->next;

/* Free this MapEntry */
         (void) FreeMapEntry( next, status );

/* Set up the next MapEntry to be freed. */
         next = link;
      }

/* Store a NULL pointer in the table element. */
      this->table[ itab ] = NULL;

/* Sets the number of entries in this hash table element to zero. */
      this->nentry[ itab ] = 0;
   }
}

static const char *GetAttrib( AstObject *this_object, const char *attrib, int *status ) {
/*
*  Name:
*     GetAttrib

*  Purpose:
*     Get the value of a specified attribute for a KeyMap.

*  Type:
*     Private function.

*  Synopsis:
*     #include "keymap.h"
*     const char *GetAttrib( AstObject *this, const char *attrib, int *status )

*  Class Membership:
*     KeyMap member function (over-rides the protected astGetAttrib
*     method inherited from the Mapping class).

*  Description:
*     This function returns a pointer to the value of a specified
*     attribute for a KeyMap, formatted as a character string.

*  Parameters:
*     this
*        Pointer to the KeyMap.
*     attrib
*        Pointer to a null-terminated string containing the name of
*        the attribute whose value is required. This name should be in
*        lower case, with all white space removed.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     - Pointer to a null-terminated string containing the attribute
*     value.

*  Notes:
*     - The returned string pointer may point at memory allocated
*     within the KeyMap, or at static memory. The contents of the
*     string may be over-written or the pointer may become invalid
*     following a further invocation of the same function or any
*     modification of the KeyMap. A copy of the string should
*     therefore be made if necessary.
*     - A NULL pointer will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*/

/* Local Variables: */
   astDECLARE_GLOBALS            /* Declare the thread specific global data */
   AstKeyMap *this;              /* Pointer to the KeyMap structure */
   const char *result;           /* Pointer value to return */
   int ival;                     /* Attribute value */

/* Initialise. */
   result = NULL;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Get a pointer to the structure holding thread-specific global data. */
   astGET_GLOBALS(this_object);

/* Obtain a pointer to the KeyMap structure. */
   this = (AstKeyMap *) this_object;

/* Compare "attrib" with each recognised attribute name in turn,
   obtaining the value of the required attribute. If necessary, write
   the value into "getattrib_buff" as a null-terminated string in an appropriate
   format.  Set "result" to point at the result string. */

/* SizeGuess. */
/* ---------- */
   if ( !strcmp( attrib, "sizeguess" ) ) {
      ival = astGetSizeGuess( this );
      if ( astOK ) {
         (void) sprintf( getattrib_buff, "%d", ival );
         result = getattrib_buff;
      }

/* KeyCase. */
/* --------- */
   } else if ( !strcmp( attrib, "keycase" ) ) {
      ival = astGetKeyCase( this );
      if ( astOK ) {
         (void) sprintf( getattrib_buff, "%d", ival );
         result = getattrib_buff;
      }

/* KeyError. */
/* --------- */
   } else if ( !strcmp( attrib, "keyerror" ) ) {
      ival = astGetKeyError( this );
      if ( astOK ) {
         (void) sprintf( getattrib_buff, "%d", ival );
         result = getattrib_buff;
      }

/* MapLocked. */
/* --------- */
   } else if ( !strcmp( attrib, "maplocked" ) ) {
      ival = astGetMapLocked( this );
      if ( astOK ) {
         (void) sprintf( getattrib_buff, "%d", ival );
         result = getattrib_buff;
      }

/* SortBy. */
/* --------- */
   } else if ( !strcmp( attrib, "sortby" ) ) {
      ival = astGetSortBy( this );
      if ( astOK ) {
         result = SortByString( ival, "astGetAttrib", status );
      }

/* If the attribute name was not recognised, pass it on to the parent
   method for further interpretation. */
   } else {
      result = (*parent_getattrib)( this_object, attrib, status );
   }

/* Return the result. */
   return result;

}

static int GetObjSize( AstObject *this_object, int *status ) {
/*
*  Name:
*     GetObjSize

*  Purpose:
*     Return the in-memory size of an Object.

*  Type:
*     Private function.

*  Synopsis:
*     #include "keymap.h"
*     int GetObjSize( AstObject *this, int *status )

*  Class Membership:
*     KeyMap member function (over-rides the astGetObjSize protected
*     method inherited from the parent class).

*  Description:
*     This function returns the in-memory size of the supplied KeyMap,
*     in bytes.

*  Parameters:
*     this
*        Pointer to the KeyMap.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     The Object size, in bytes.

*  Notes:
*     - A value of zero will be returned if this function is invoked
*     with the global status set, or if it should fail for any reason.
*/

/* Local Variables: */
   AstKeyMap *this;       /* Pointer to KeyMap structure */
   AstMapEntry *next;     /* Pointer the next MapEntry */
   AstObject **alist;     /* Pointer to list of AST object pointers */
   AstObject *obj;        /* Pointer to AST object */
   const char **slist;    /* Pointer to list of text pointers */
   int i;                 /* Loop count */
   int itab;              /* Table entry index */
   int nel;               /* No. of values in entry vector (0 => scalar) */
   int result;            /* Result value to return */
   int type;              /* Entry data type */

/* Initialise. */
   result = 0;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Obtain a pointers to the KeyMap structure. */
   this = (AstKeyMap *) this_object;

/* Invoke the GetObjSize method inherited from the parent class, and then
   add on any components of the class structure defined by thsi class
   which are stored in dynamically allocated memory. */
   result = (*parent_getobjsize)( this_object, status );

   for( itab = 0; itab < this->mapsize; itab++ ) {
      next = this->table[ itab ];
      while( next ) {
         nel = next->nel;
         type = next->type;

         if( type == AST__STRINGTYPE ) {

            if( nel == 0 ) {
               result += astTSizeOf( ( void *) ( (Entry0C *) next )->value );

            } else {
               slist = ( (Entry1C *) next )->value;
               if( slist ) {
                  for( i = 0; i < nel; i++ ) result += astTSizeOf( (void *) slist[ i ] );
                  result += astTSizeOf( (void *) slist );
               }
            }

         } else if( type == AST__OBJECTTYPE ) {
            if( nel == 0 ) {
               obj = ( (Entry0A *) next )->value;
               result += astGetObjSize( obj );
            } else {
               alist = ( (Entry1A *) next )->value;
               if( alist ) {
                  for( i = 0; i < nel; i++ ) {
                     result += astGetObjSize( alist[ i ] );
                  }
                  result += astTSizeOf( alist );
               }
            }

         } else if( type == AST__POINTERTYPE ) {
            if( nel > 0 ) result += astTSizeOf( ( (Entry1P *) next )->value );

         } else if( type == AST__INTTYPE ) {
            if( nel > 0 ) result += astTSizeOf( ( (Entry1I *) next )->value );

         } else if( type == AST__SINTTYPE ) {
            if( nel > 0 ) result += astTSizeOf( ( (Entry1S *) next )->value );

         } else if( type == AST__BYTETYPE ) {
            if( nel > 0 ) result += astTSizeOf( ( (Entry1B *) next )->value );

         } else if( type == AST__DOUBLETYPE ) {
            if( nel > 0 ) result += astTSizeOf( ( (Entry1D *) next )->value );

         } else if( type == AST__FLOATTYPE ) {
            if( nel > 0 ) result += astTSizeOf( ( (Entry1F *) next )->value );

         } else if( type == AST__UNDEFTYPE ) {

         } else {
            astError( AST__INTER, "astGetObjSize(KeyMap): Illegal map entry data "
                      "type %d encountered (internal AST programming error).", status,
                      type );
         }

         result += astTSizeOf( (void *) next->key );
         result += astTSizeOf( (void *) next->comment );
         result += astTSizeOf( next );

         next = next->next;
      }
   }

/* If an error occurred, clear the result value. */
   if ( !astOK ) result = 0;

/* Return the result, */
   return result;
}

static const char *GetKey( AstKeyMap *this, int index, int *status ) {
/*
*  Name:
*     GetKey

*  Purpose:
*     Get the key at a given index within the KeyMap.

*  Type:
*     Private member function.

*  Synopsis:
*     #include "keymap.h"
*     const char *GetKey( AstKeyMap *this, int index, int *status )

*  Class Membership:
*     KeyMap method.

*  Description:
*     This function returns a string holding the key for the entry with
*     the given index within the KeyMap. The index associated with a
*     given key is determined by the current setting of the SortBy attribute.

*  Parameters:
*     this
*        Pointer to the KeyMap.
*     index
*        The index into the KeyMap. The first entry has index zero, and the last
*        has index "size-1", where "size" is the value returned by the
*        astMapSize function. An error is reported if the supplied index is
*        out of bounds.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*        A pointer to a null-terminated string containing the key.

*  Notes:
*     - A NULL pointer will be returned if this function is invoked
*     with the AST error status set, or if it should fail for any
*     reason.
*/

/* Local Variables: */
   AstMapEntry *entry;         /* Pointer to the entry */
   const char *result;         /* Pointer value to return */
   int ifirst;                 /* Index of first entry in this table element */
   int ilast;                  /* Index of last entry in this table element */
   int istep;                  /* Entry count */
   int itab;                   /* Index into hash table */
   int nstep;                  /* No. of entries to skip */
   int sortby;                 /* The value of the SortBy attribute */

/* Initialise. */
   result = NULL;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Get the SortBy value. */
   sortby = astGetSortBy( this );

/* First deal with unsorted keys. */
   if( sortby == SORTBY_NONE ) {

/* Loop round each entry in the hash table. */
      ilast = -1;
      for( itab = 0; itab < this->mapsize; itab++ ) {

/* Find the index of the first and the last Entry in the linked list associated
   with this element of the hash table. */
         ifirst = ilast + 1;
         ilast += this->nentry[ itab ];

/* Pass on if we have not yet reached the element containing the required
   key. */
         if( ilast >= index ) {

/* Find how many steps we need to proceed down the linked list to reach
   the required index. */
            nstep = index - ifirst;

/* Make this many steps down the linked list.*/
            entry = this->table[ itab ];
            for( istep = 0; entry && istep < nstep; istep++ ) entry = entry->next;

/* Return a pointer to the key string, and break out of the loop. */
            if( entry ) result = entry->key;
            break;

         }
      }

/* Now deal with sorted keys. */
   } else {

/* Get a pointer to the first entry in the sorted list. */
      entry = this->first;

/* Move up the sorted list by the required number of entries. */
      for( istep = 0; entry && istep < index; istep++ ) entry = entry->snext;

/* Return a pointer to the key string. */
      if( entry ) result = entry->key;
   }

/* Report an error if the element was not found. */
   if( !result && astOK ) {
      astError( AST__MPIND, "astMapKey(%s): Cannot find element "
                "%d (zero-based) of the %s.", status, astGetClass( this ),
                index, astGetClass( this ) );
   }

/* Return the result.*/
   return result;
}

static int GetSizeGuess( AstKeyMap *this, int *status ) {
/*
*+
*  Name:
*     astGetSizeGuess

*  Purpose:
*     Get the value of the SizeGuess attribute for a KeyMap.

*  Type:
*     Protected virtual function.

*  Synopsis:
*     #include "keymap.h"
*     int astGetSizeGuess( AstKeyMap *this )

*  Class Membership:
*     KeyMap method.

*  Description:
*     This function returns the value of the SizeGuess attribute for a
*     KeyMap.

*  Parameters:
*     this
*        Pointer to the KeyMap.

*  Returned Value:
*     The value of the SizeGuess attribute.

*  Notes:
*     - A value of zero is returned if this function is invoked with the
*     global error status set.

*-
*/

/* Local Variables: */
   int result;                /* Returned value */

/* Initialise */
   result = 0;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Return the attribute value using a default if not set. */
   return ( this->sizeguess == INT_MAX ) ?
           MIN_TABLE_SIZE*MAX_ENTRIES_PER_TABLE_ENTRY : this->sizeguess;
}

static int HashFun( const char *key, int bitmask, unsigned long *hash, int *status ){
/*
*  Name:
*     HashFun

*  Purpose:
*     Returns an integer hash code for a string

*  Type:
*     Private function.

*  Synopsis:
*     #include "keymap.h"
*     int HashFun( const char *key, int bitmask, int *hash, int *status )

*  Class Membership:
*     KeyMap member function.

*  Description:
*     This function returns an integer hash code for the supplied string,
*     This is the value that isused as the index into the hash table for
*     the specified key.

*  Parameters:
*     key
*        Pointer to the string. Trailing spaces are ignored.
*     bitmask
*        A bit mask that is used to zero the upper bits of a full width
*        hash value in order to produce the required array index. This
*        should be one less than the length of the hash table.
*     hash
*        Pointer to a location at which to put the full width hash value.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     An integer in the range zero to ( mapsize - 1 ).

*  Notes:
*     - A value of zero is returned if this function is invoked with the
*     global error status set.
*/

/* Local Variables: */
   int c;

/* Check the local error status. */
   if ( !astOK ) return 0;

/* djb2: This hash function was first reported by Dan Bernstein many years
   ago in comp.lang.c Each through the "hile" loop corresponds to
   "hash = hash*33 + c ". Ignore spaces so that trailing spaces used to
   pad F77 character variables will be ignored. */
   *hash = 5381;
   while( (c = *key++) ) {
      if( c != ' ' ) {
         *hash = ((*hash << 5) + *hash) + c;
      }
   }
   return ( *hash & bitmask );
}

void astInitKeyMapVtab_(  AstKeyMapVtab *vtab, const char *name, int *status ) {
/*
*+
*  Name:
*     astInitKeyMapVtab

*  Purpose:
*     Initialise a virtual function table for a KeyMap.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "keymap.h"
*     void astInitKeyMapVtab( AstKeyMapVtab *vtab, const char *name )

*  Class Membership:
*     KeyMap vtab initialiser.

*  Description:
*     This function initialises the component of a virtual function
*     table which is used by the KeyMap class.

*  Parameters:
*     vtab
*        Pointer to the virtual function table. The components used by
*        all ancestral classes will be initialised if they have not already
*        been initialised.
*     name
*        Pointer to a constant null-terminated character string which contains
*        the name of the class to which the virtual function table belongs (it
*        is this pointer value that will subsequently be returned by the Object
*        astClass function).
*-
*/

/* Local Variables: */
   astDECLARE_GLOBALS            /* Pointer to thread-specific global data */
   AstObjectVtab *object;        /* Pointer to Object component of Vtab */

/* Check the local error status. */
   if ( !astOK ) return;

/* Get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(NULL);

/* Initialize the component of the virtual function table used by the
   parent class. */
   astInitObjectVtab( (AstObjectVtab *) vtab, name );

/* Store a unique "magic" value in the virtual function table. This
   will be used (by astIsAKeyMap) to determine if an object belongs
   to this class.  We can conveniently use the address of the (static)
   class_check variable to generate this unique value. */
   vtab->id.check = &class_check;
   vtab->id.parent = &(((AstObjectVtab *) vtab)->id);

/* Initialise member function pointers. */
/* ------------------------------------ */
/* Store pointers to the member functions (implemented here) that provide
   virtual methods for this class. */

   vtab->MapGet0P = MapGet0P;
   vtab->MapGet0A = MapGet0A;
   vtab->MapGet0C = MapGet0C;
   vtab->MapGet0D = MapGet0D;
   vtab->MapGet0F = MapGet0F;
   vtab->MapGet0I = MapGet0I;
   vtab->MapGet0B = MapGet0B;
   vtab->MapGet0S = MapGet0S;
   vtab->MapGet1P = MapGet1P;
   vtab->MapGet1A = MapGet1A;
   vtab->MapGet1C = MapGet1C;
   vtab->MapGet1D = MapGet1D;
   vtab->MapGet1F = MapGet1F;
   vtab->MapGet1I = MapGet1I;
   vtab->MapGet1B = MapGet1B;
   vtab->MapGet1S = MapGet1S;
   vtab->MapGetElemP = MapGetElemP;
   vtab->MapGetElemA = MapGetElemA;
   vtab->MapGetElemC = MapGetElemC;
   vtab->MapGetElemD = MapGetElemD;
   vtab->MapGetElemF = MapGetElemF;
   vtab->MapGetElemI = MapGetElemI;
   vtab->MapGetElemS = MapGetElemS;
   vtab->MapGetElemB = MapGetElemB;
   vtab->MapPutElemP = MapPutElemP;
   vtab->MapPutElemA = MapPutElemA;
   vtab->MapPutElemC = MapPutElemC;
   vtab->MapPutElemD = MapPutElemD;
   vtab->MapPutElemF = MapPutElemF;
   vtab->MapPutElemI = MapPutElemI;
   vtab->MapPutElemS = MapPutElemS;
   vtab->MapPutElemB = MapPutElemB;
   vtab->MapPut0A = MapPut0A;
   vtab->MapPut0P = MapPut0P;
   vtab->MapPut0C = MapPut0C;
   vtab->MapPut0D = MapPut0D;
   vtab->MapPut0F = MapPut0F;
   vtab->MapPut0I = MapPut0I;
   vtab->MapPut0S = MapPut0S;
   vtab->MapPut0B = MapPut0B;
   vtab->MapPut1P = MapPut1P;
   vtab->MapPut1A = MapPut1A;
   vtab->MapPut1C = MapPut1C;
   vtab->MapPut1D = MapPut1D;
   vtab->MapPut1F = MapPut1F;
   vtab->MapPut1I = MapPut1I;
   vtab->MapPut1S = MapPut1S;
   vtab->MapPut1B = MapPut1B;
   vtab->MapPutU = MapPutU;
   vtab->MapRemove = MapRemove;
   vtab->MapRename = MapRename;
   vtab->MapCopy = MapCopy;
   vtab->MapDefined = MapDefined;
   vtab->MapSize = MapSize;
   vtab->MapLenC = MapLenC;
   vtab->MapLength = MapLength;
   vtab->MapType = MapType;
   vtab->MapHasKey = MapHasKey;
   vtab->MapKey = MapKey;
   vtab->MapIterate = MapIterate;

   vtab->ClearSizeGuess = ClearSizeGuess;
   vtab->SetSizeGuess = SetSizeGuess;
   vtab->GetSizeGuess = GetSizeGuess;
   vtab->TestSizeGuess = TestSizeGuess;

   vtab->ClearSortBy = ClearSortBy;
   vtab->SetSortBy = SetSortBy;
   vtab->GetSortBy = GetSortBy;
   vtab->TestSortBy = TestSortBy;

   vtab->ClearKeyError = ClearKeyError;
   vtab->SetKeyError = SetKeyError;
   vtab->GetKeyError = GetKeyError;
   vtab->TestKeyError = TestKeyError;

   vtab->ClearKeyCase = ClearKeyCase;
   vtab->SetKeyCase = SetKeyCase;
   vtab->GetKeyCase = GetKeyCase;
   vtab->TestKeyCase = TestKeyCase;

   vtab->ClearMapLocked = ClearMapLocked;
   vtab->SetMapLocked = SetMapLocked;
   vtab->GetMapLocked = GetMapLocked;
   vtab->TestMapLocked = TestMapLocked;

/* Save the inherited pointers to methods that will be extended, and
   replace them with pointers to the new member functions. */
   object = (AstObjectVtab *) vtab;

/* Store replacement pointers for methods which will be over-ridden by
   new member functions implemented here. */
   parent_getobjsize = object->GetObjSize;
   object->GetObjSize = GetObjSize;

   parent_clearattrib = object->ClearAttrib;
   object->ClearAttrib = ClearAttrib;
   parent_getattrib = object->GetAttrib;
   object->GetAttrib = GetAttrib;
   parent_setattrib = object->SetAttrib;
   object->SetAttrib = SetAttrib;
   parent_testattrib = object->TestAttrib;
   object->TestAttrib = TestAttrib;

#if defined(THREAD_SAFE)
   parent_managelock = object->ManageLock;
   object->ManageLock = ManageLock;
#endif

/* Declare the destructor, copy constructor and dump function. */
   astSetDelete( vtab, Delete );
   astSetCopy( vtab, Copy );
   astSetDump( vtab, Dump, "KeyMap", "Map of key/value pairs" );

/* If we have just initialised the vtab for the current class, indicate
   that the vtab is now initialised, and store a pointer to the class
   identifier in the base "object" level of the vtab. */
   if( vtab == &class_vtab ) {
      class_init = 1;
      astSetVtabClassIdentifier( vtab, &(vtab->id) );
   }
}

static void InitMapEntry( AstMapEntry *entry, int type, int nel, int *status ){
/*
*  Name:
*     InitMapEntry

*  Purpose:
*     initialise a MapEntry structure to null values.

*  Type:
*     Private function.

*  Synopsis:
*     #include "keymap.h"
*     void InitMapEntry( AstMapEntry *entry, int type, int nel, int *status )

*  Class Membership:
*     KeyMap member function.

*  Description:
*     This function initialises the contents of a MapEntry to null values.

*  Parameters:
*     this
*        Pointer to the MapEntry.
*     type
*        The type of the MapEntry.
*     nel
*        The number of elements in the entry: 0 = scalar, >0 = vector.
*     status
*        Pointer to the inherited status variable.

*/

/* Check the global error status. */
   if( !astOK ) return;

/* Initialise all elements with in the MapEntry structure. */
   entry->next = NULL;
   entry->key = NULL;
   entry->hash = 0;
   entry->type = type;
   entry->nel = nel;
   entry->comment = NULL;
   entry->defined = 0;
   entry->snext = NULL;
   entry->sprev = NULL;
   entry->member = 0;
   entry->keymember = 0;
   entry->sortby = SORTBY_NONE;

   if( type == AST__OBJECTTYPE ) {
      if( nel == 0 ) {
         ( (Entry0A *) entry )->next = NULL;
         ( (Entry0A *) entry )->prev = NULL;
      } else {
         ( (Entry1A *) entry )->next = NULL;
         ( (Entry1A *) entry )->prev = NULL;
      }
   }

}

static int KeyCmp( const char *key1, const char *key2 ) {
/*
*  Name:
*     KeyCmp

*  Purpose:
*     Compares keys for equality.

*  Type:
*     Private function.

*  Synopsis:
*     #include "keymap.h"
*     int KeyCmp( const char *key1, const char *key2 )

*  Class Membership:
*     KeyMap member function.

*  Description:
*     This function compares two strings. It is like strcmp except that it
*     ignores trailing spaces.

*  Parameters:
*     key1
*        Pointer to first string.
*     key2
*        Pointer to second string.

*  Returned Value:
*     One if the keys differ. Zero if they are identical (except for
*     trailing spaces).

*/

/* Local Variables: */
   const char *k1;               /* Pointer to next "key1" character */
   const char *k2;               /* Pointer to next "key2" character */
   int result;                   /* Returned flag */

/* Check the strings are deifned. */
   if ( !key1 || !key2 ) return 0;

/* Get pointers to the first characters to differ, or to the first null
   character, which ever comes first. */
   k1 = key1;
   k2 = key2;
   while( *k1 && ( *k1 == *k2 ) ) {
      k1++;
      k2++;
   }

/* If both characters are null, the strings are identical. If neither is null,
   the string definitely differ. If one is null, we need to check if the
   other one only has spaces to the end of the string. */
   if( *k1 ) {
      if( *k2 ) {
         result = ( *k1 > *k2 ) ? 1 : -1;
      } else {
         while( *k1 == ' ' ) k1++;
         result = ( *k1 == 0 ) ? 0 : 1;
      }
   } else {
      if( *k2 ) {
         while( *k2 == ' ' ) k2++;
         result = ( *k2 == 0 ) ? 0 : -1;
      } else {
         result = 0;
      }
   }

/* Return the result. */
   return result;
}

#if defined(THREAD_SAFE)
static int ManageLock( AstObject *this_object, int mode, int extra,
                       AstObject **fail, int *status ) {
/*
*  Name:
*     ManageLock

*  Purpose:
*     Manage the thread lock on an Object.

*  Type:
*     Private function.

*  Synopsis:
*     #include "object.h"
*     AstObject *ManageLock( AstObject *this, int mode, int extra,
*                            AstObject **fail, int *status )

*  Class Membership:
*     KeyMap member function (over-rides the astManageLock protected
*     method inherited from the parent class).

*  Description:
*     This function manages the thread lock on the supplied Object. The
*     lock can be locked, unlocked or checked by this function as
*     deteremined by parameter "mode". See astLock for details of the way
*     these locks are used.

*  Parameters:
*     this
*        Pointer to the Object.
*     mode
*        An integer flag indicating what the function should do:
*
*        AST__LOCK: Lock the Object for exclusive use by the calling
*        thread. The "extra" value indicates what should be done if the
*        Object is already locked (wait or report an error - see astLock).
*
*        AST__UNLOCK: Unlock the Object for use by other threads.
*
*        AST__CHECKLOCK: Check that the object is locked for use by the
*        calling thread (report an error if not).
*     extra
*        Extra mode-specific information.
*     fail
*        If a non-zero function value is returned, a pointer to the
*        Object that caused the failure is returned at "*fail". This may
*        be "this" or it may be an Object contained within "this". Note,
*        the Object's reference count is not incremented, and so the
*        returned pointer should not be annulled. A NULL pointer is
*        returned if this function returns a value of zero.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*    A local status value:
*        0 - Success
*        1 - Could not lock or unlock the object because it was already
*            locked by another thread.
*        2 - Failed to lock a POSIX mutex
*        3 - Failed to unlock a POSIX mutex
*        4 - Bad "mode" value supplied.

*  Notes:
*     - This function attempts to execute even if an error has already
*     occurred.
*/

/* Local Variables: */
   AstKeyMap *this;       /* Pointer to KeyMap structure */
   AstMapEntry *next;     /* Pointer the next MapEntry */
   AstObject **alist;     /* Pointer to list of AST object pointers */
   AstObject *obj;        /* Pointer to AST object */
   int i;                 /* Loop count */
   int nel;               /* No. of values in entry vector (0 => scalar) */
   int result;            /* Returned status value */

/* Initialise */
   result = 0;

/* Check the supplied pointer is not NULL. */
   if( !this_object ) return result;

/* Obtain a pointers to the KeyMap structure. */
   this = (AstKeyMap *) this_object;

/* Invoke the ManageLock method inherited from the parent class. */
   if( !result ) result = (*parent_managelock)( this_object, mode, extra,
                                                fail, status );

/* Invoke the astManageLock method on any Objects contained within
   the supplied Object. */

   next = this->firstA;
   while( next ) {
      nel = next->nel;
      if( nel == 0 ) {
         obj = ( (Entry0A *) next )->value;
         if( !result ) result = astManageLock( obj, mode, extra, fail );
         next = ( (Entry0A *) next)->next;
      } else {
         alist = ( (Entry1A *) next )->value;
         if( alist ) {
            for( i = 0; i < nel; i++ ) {
               if( !result ) result = astManageLock( alist[ i ], mode,
                                                     extra, fail );
            }
         }
         next = ( (Entry1A *) next)->next;
      }
   }

   return result;

}
#endif

static void MapCopy( AstKeyMap *this, AstKeyMap *that, int *status ) {
/*
*++
*  Name:
c     astMapCopy
f     AST_MAPCOPY

*  Purpose:
*     Copy entries from one KeyMap into another.

*  Type:
*     Public virtual function.

*  Synopsis:
c     #include "keymap.h"
c     void astMapCopy( AstKeyMap *this, AstKeyMap *that )
f     CALL AST_MAPCOPY( THIS, THAT, STATUS )

*  Class Membership:
*     KeyMap method.

*  Description:
c     This function
f     This routine
*     copies all entries from one KeyMap into another.

*  Parameters:
c     this
f     THIS = INTEGER (Given)
*        Pointer to the destination KeyMap.
c     that
f     THAT = INTEGER (Given)
*        Pointer to the source KeyMap.
f     STATUS = INTEGER (Given and Returned)
f        The global status.

*  Notes:
*     - Entries from the source KeyMap will replace any existing entries in
*     the destination KeyMap that have the same key.
*     - The one exception to the above rule is that if a source entry
*     contains a scalar KeyMap entry, and the destination contains a
*     scalar KeyMap entry with the same key, then the source KeyMap entry
*     will be copied into the destination KeyMap entry using this function,
*     rather than simply replacing the destination KeyMap entry.
*     - If the destination entry has a non-zero value for its MapLocked
*     attribute, then an error will be reported if the source KeyMap
*     contains any keys that do not already exist within the destination
*     KeyMap.

*--
*/

/* Local Variables: */
   AstMapEntry *in_entry; /* Pointer to next source entry to copy */
   AstMapEntry *out_entry;/* Pointer to existing destination entry */
   AstObject *in_obj;     /* Pointer for source Object entry */
   AstObject *out_obj;    /* Pointer for destination Object entry */
   const char *key;       /* Key for current entry */
   int i;                 /* Index into source hash table */
   int itab;              /* Index of destination hash table element */
   int keymember;         /* Identifier for key */
   int merged;            /* Were source and destination KeyMaps merged? */
   unsigned long hash;    /* Full width hash value */

/* Check the global error status. */
   if ( !astOK ) return;

/* Loop round all entries in the source hash table. */
   for( i = 0; i < that->mapsize; i++ ) {

/* Get a pointer to the next source KeyMap entry. */
      in_entry = that->table[ i ];

/* Loop round all entries in this element of the source hash table. */
      while( in_entry && astOK ) {

/* Get its key. */
         key = in_entry->key;

/* Search for a destination entry with the same key. */
         itab = HashFun( key, this->mapsize - 1, &hash, status );
         out_entry = SearchTableEntry( this, itab, key, status );

/* If the destination KeyMap does not contain an entry with the current
   key, store a copy of the entry in the destination, or report an error
   if the destination's MapLocked attribute is set. */
         if( !out_entry ) {
            if( astGetMapLocked( this ) ) {
               astError( AST__BADKEY, "astMapCopy(%s): Failed to copy "
                         "item \"%s\": \"%s\" is not a known item.", status,
                         astGetClass( this ), key, key );
            } else {
               out_entry = CopyMapEntry( in_entry, status );
               out_entry = AddTableEntry( this, itab, out_entry, -1, status );
            }

/* If the destination KeyMap contains an entry with the current key... */
         } else {

/* The usual thing is to just replace the existing entry in the
   destination with a copy of the source entry. The one case where this is
   not done is if both entries are scalar KeyMaps. In this case the source
   KeyMap is merged into the destination KeyMap using this function. First
   see if we have this situation, and if so, copy the entries from the
   source KeyMap to the destination KeyMap. */
            merged = 0;
            if( in_entry->nel == 0 || in_entry->nel == 1 ) {
               if( out_entry->nel == 0 || out_entry->nel == 1 ) {
                  if( in_entry->type == AST__OBJECTTYPE &&
                      out_entry->type == AST__OBJECTTYPE ) {

                     if( in_entry->nel == 0 ) {
                        in_obj = ((Entry0A *)in_entry)->value;
                     } else {
                        in_obj = (((Entry1A *)in_entry)->value)[ 0 ];
                     }

                     if( out_entry->nel == 0 ) {
                        out_obj = ((Entry0A *)out_entry)->value;
                     } else {
                        out_obj = (((Entry1A *)out_entry)->value)[ 0 ];
                     }

                     if( astIsAKeyMap( in_obj ) &&
                         astIsAKeyMap( out_obj ) ) {
                        astMapCopy( (AstKeyMap *) out_obj,
                                    (AstKeyMap *) in_obj );
                        merged = 1;
                     }
                  }
               }
            }

/* If the source and desination entries are not KeyMaps, then just remove
   the entry in the desination KeyMap and add a copy of the source entry.
   But retain the original keymember value since we are just changing the
   value of an existing key. */
            if( ! merged ) {
               out_entry = RemoveTableEntry( this, itab, key, status );
               keymember = out_entry->keymember;
               (void) FreeMapEntry( out_entry, status );
               out_entry = CopyMapEntry( in_entry, status );
               out_entry = AddTableEntry( this, itab, out_entry, keymember, status );
            }
         }

/* Update the address of the next MapEntry in the source. */
         in_entry = in_entry->next;
      }
   }
}

static const char *MapKey( AstKeyMap *this, int index, int *status ) {
/*
*++
*  Name:
c     astMapKey
f     AST_MAPKEY

*  Purpose:
*     Get the key at a given index within the KeyMap.

*  Type:
*     Public virtual function.

*  Synopsis:
c     #include "keymap.h"
c     const char *astMapKey( AstKeyMap *this, int index )
f     RESULT = AST_MAPKEY( THIS, INDEX, STATUS )

*  Class Membership:
*     KeyMap method.

*  Description:
*     This function returns a string holding the key for the entry with
*     the given index within the KeyMap.
*
*     This function is intended primarily as a means of iterating round all
*     the elements in a KeyMap. For this purpose, the number of entries in
*     the KeyMap should first be found using
c     astMapSize
f     AST_MAPSIZE
*     and this function should then be called in a loop, with the index
*     value going from
c     zero to one less than the size of the KeyMap.
f     one to the size of the KeyMap.
*     The index associated with a given entry is determined by the SortBy
*     attribute.

*  Parameters:
c     this
f     THIS = INTEGER (Given)
*        Pointer to the KeyMap.
c     index
f     INDEX = INTEGER (Given)
*        The index into the KeyMap. The first entry has index
c        zero, and the last has index "size-1", where "size" is the value
c        returned by the astMapSize function.
f        one, and the last has index SIZE, the value returned by the
f        AST_MAPSIZE function.
f     STATUS = INTEGER (Given and Returned)
f        The global status.

*  Returned Value:
c     astMapKey()
c        A pointer to a null-terminated string containing the key.
f     AST_MAPKEY = CHARACTER * ( AST__SZCHR )
f        The key value.

*  Notes:
c     - The returned pointer is guaranteed to remain valid and the
c     string to which it points will not be over-written for a total
c     of 50 successive invocations of this function. After this, the
c     memory containing the string may be re-used, so a copy of the
c     string should be made if it is needed for longer than this.
c     - A NULL pointer will be returned if this function is invoked
c     with the AST error status set, or if it should fail for any
c     reason.
f     - A blank string will be returned if this function is invoked
f     with STATUS set to an error value, or if it should fail for any
f     reason.
*--
*/

/* Local Variables: */
   astDECLARE_GLOBALS            /* Declare the thread specific global data */
   const char *result;           /* Pointer value to return */
   const char *value;            /* Pointer to key value */
   int i;                        /* Loop counter for initialisation */

/* Initialise. */
   result = NULL;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Get a pointer to the structure holding thread-specific global data. */
   astGET_GLOBALS(this);

/* If the "mapkey_strings" array has not been initialised, fill it with
   NULL pointers. */
   if ( !mapkey_init ) {
      mapkey_init = 1;
      for ( i = 0; i < AST__KEYMAP_MAPKEY_MAX_STRINGS; i++ ) mapkey_strings[ i ] = NULL;
   }

/* Obtain a pointer to the required key value. */
   value = GetKey( this, index, status );

/* If OK, store a copy of the resulting string in dynamically
   allocated memory, putting a pointer to the copy into the next
   element of the "mapkey_strings" array.  (This process also de-allocates
   any previously allocated memory pointed at by this "mapkey_strings"
   element, so the earlier string is effectively replaced by the new
   one.) */
   if ( astOK ) {
      astBeginPM;
      mapkey_strings[ mapkey_istr ] = astStore( mapkey_strings[ mapkey_istr ], value,
                                  strlen( value ) + (size_t) 1 );
      astEndPM;

/* If OK, return a pointer to the copy and increment "mapkey_istr" to use the
   next element of "mapkey_strings" on the next invocation. Recycle "mapkey_istr" to
   zero when all elements have been used. */
      if ( astOK ) {
         result = mapkey_strings[ mapkey_istr++ ];
         if ( mapkey_istr == ( AST__KEYMAP_MAPKEY_MAX_STRINGS - 1 ) ) mapkey_istr = 0;
      }
   }

/* Return the result. */
   return result;

}

/*
*++
*  Name:
c     astMapPut0<X>
f     AST_MAPPUT0<X>

*  Purpose:
*     Add a scalar value to a KeyMap.

*  Type:
*     Public virtual function.

*  Synopsis:
c     #include "ast.h"
c     void astMapPut0<X>( AstKeyMap *this, const char *key, <X>type value,
c                         const char *comment );
f     CALL AST_MAPPUT0<X>( THIS, KEY, VALUE, COMMENT, STATUS )

*  Class Membership:
*     KeyMap method.

*  Description:
c     This is a set of functions
f     This is a set of routine
*     for adding scalar values to a KeyMap. You should use a
c     function
f     routine
*     which matches the data type of the data you wish to add to the KeyMap
*     by replacing <X> in the generic
c     function name astMapPut0<X>
f     routine name AST_MAPPUT0<X>
*     by an appropriate 1-character type code (see the "Data Type Codes"
*     section below for the code appropriate to each supported data type).

*  Parameters:
c     this
f     THIS = INTEGER (Given)
*        Pointer to the KeyMap in which to store the supplied value.
c     key
f     KEY = CHARACTER * ( * ) (Given)
*        A character string to be stored with the value, which can later
*        be used to identify the value. Trailing spaces are ignored.
*        The supplied string is converted to upper case before use if the
*        KeyCase attribute is currently set to zero.
c     value
f     VALUE = <X>type (Given)
*        The value to be stored. The data type of this value should match the
*        1-character type code appended to the
c        function name (e.g. if you are using astMapPut0A, the type of this
c        value should be "pointer to AstObject").
f        routine name (e.g. if you are using AST_MAPPUT0A, the type of this
f        value should be "integer pointer for an AstObject").
c     comment
f     COMMENT = CHARACTER * ( * ) (Given)
f        A comment string to be stored with the value.
c        A pointer to a null-terminated comment string to be stored with the
c        value. A NULL pointer may be supplied, in which case no comment is
c        stored.
f     STATUS = INTEGER (Given and Returned)
f        The global status.

*  Notes:
*     - If the supplied key is already in use in the KeyMap, the new value
*     will replace the old value.
*     - If the stored value is an AST Object pointer, the Object's reference
*     count is incremented by this call. Any subsequent changes made to
*     the Object using the returned pointer will be reflected in any
*     any other active pointers for the Object, including any obtained
*     later using
c     astMapget0A.
f     AST_MAPGET0A.
*     The reference count for the Object will be decremented when the
*     KeyMap is destroyed, or the entry is removed or over-written with a
*     different pointer.

*  Data Type Codes:
*     To select the appropriate
c     function, you should replace <X> in the generic function name astMapPut0<X>
f     routine, you should replace <X> in the generic routine name AST_MAPPUT0<X>
*     with a 1-character data type code, so as to match the data type <X>type
*     of the data you are processing, as follows:
c     - D: double
c     - F: float
c     - I: int
c     - C: "const" pointer to null terminated character string
c     - A: Pointer to AstObject
c     - P: Generic "void *" pointer
c     - S: short int
c     - B: unsigned byte (i.e. unsigned char)
f     - D: DOUBLE PRECISION
f     - R: REAL
f     - I: INTEGER
f     - C: CHARACTER
f     - A: INTEGER used to identify an AstObject
f     - S: INTEGER*2 (short integer)
f     - B: Unsigned byte
*
c     For example, astMapPut0D would be used to store a "double" value,
c     while astMapPut0I would be used to store an "int", etc.
f     For example, AST_MAPPUT0D would be used to store a DOUBLE PRECISION value,
f     while AST_MAPPUT0I would be used to store an INTEGER, etc.
c
c     Note that KeyMaps containing generic "void *" pointers cannot be
c     written out using astShow or astWrite. An error will be reported if
c     this is attempted.
*--
*/
/* Define a macro to implement the function for a specific data type. */
#define MAKE_MAPPUT0(X,Xtype,Itype,ValExp) \
static void MapPut0##X( AstKeyMap *this, const char *skey, Xtype value, \
                        const char *comment, int *status ) { \
\
/* Local Variables: */ \
   AstMapEntry *mapentry;  /* Pointer to parent MapEntry structure */ \
   AstMapEntry *oldent;    /* Pointer to existing MapEntry */ \
   Entry0##X *entry;       /* Structure holding the data for the new Entry */ \
   const char *key;        /* Pointer to key string to use */ \
   char *p;                /* Pointer to next key character */ \
   char keybuf[ AST__MXKEYLEN + 1 ]; /* Buffer for upper cas key */ \
   int itab;               /* Index of hash table element to use */ \
   int keylen;             /* Length of supplied key string */ \
   int keymember;          /* Identifier for existing key */ \
   int there;              /* Did the entry already exist in the KeyMap? */ \
\
/* Check the global error status. */ \
   if ( !astOK ) return; \
\
/* Perform any necessary checks on the supplied value to be stored. */ \
   CHECK_##X \
\
/* Convert the supplied key to upper case if required. */ \
   key = ConvertKey( this, skey, keybuf, AST__MXKEYLEN + 1, "astMapPut0" #X, \
                     status ); \
\
/* Allocate memory for the new MapEntry. */ \
   entry = astMalloc( sizeof( Entry0##X ) ); \
   if( astOK ) { \
\
/* Initialise the new structure.*/ \
      mapentry = (AstMapEntry *) entry; \
      InitMapEntry( mapentry, Itype, 0, status ); \
\
/* Now store the new values. */ \
      keylen = strlen( key ); \
      mapentry->key = astStore( NULL, key, keylen + 1 ); \
      if( comment ) mapentry->comment = astStore( NULL, comment, strlen( comment ) + 1 ); \
      mapentry->defined = 1; \
      entry->value = ValExp; \
\
/* Terminate the key string to exclude any trailing spaces. */ \
      if( astOK ) { \
         p = (char *) mapentry->key + keylen; \
         while( --p >= mapentry->key ) { \
            if( *p == ' ' ) { \
               *p = 0; \
            } else { \
               break; \
            } \
         } \
      } \
\
/* Use the hash function to determine the element of the hash table in \
   which to store the new entry. */ \
      itab = HashFun( mapentry->key, this->mapsize - 1, &(mapentry->hash), status ); \
\
/* Remove any existing entry with the given key from the table element. \
   First save the key identifier. */ \
      oldent = RemoveTableEntry( this, itab, mapentry->key, status ); \
      if( oldent ) { \
         keymember = oldent->keymember; \
         oldent = FreeMapEntry( oldent, status ); \
         there = 1; \
      } else { \
         keymember = -1; \
         there = 0; \
      } \
\
/* If the KeyMap is locked we report an error if an attempt is made to add a value for \
   a new key. */ \
      if( !there && astGetMapLocked( this ) ) { \
         astError( AST__BADKEY, "astMapPut0" #X "(%s): Failed to add item \"%s\" to a KeyMap: " \
                   "\"%s\" is not a known item.", status, astGetClass( this ), key, key ); \
      } \
\
/* If all has gone OK, store the new entry at the head of the linked list \
   associated with the selected table entry. */ \
      if( astOK ) { \
         mapentry = AddTableEntry( this, itab, mapentry, keymember, status ); \
\
/* If anything went wrong, try to delete the new entry. */ \
      } else { \
         mapentry = FreeMapEntry( mapentry, status ); \
      } \
   } \
}

/* Define macros which perform any necessary checks on the supplied value
   to be stored. For Object entries, check that we are not adding a KeyMap
   which already contains "this". This avoids circular dependencies.
   Other types do not need any checks. */
#define CHECK_A CheckCircle( this, value, "astMapPut0A", status );
#define CHECK_I
#define CHECK_D
#define CHECK_F
#define CHECK_C
#define CHECK_P
#define CHECK_S
#define CHECK_B

/* Expand the above macro to generate a function for each required
   data type. */
MAKE_MAPPUT0(I,int,AST__INTTYPE,value)
MAKE_MAPPUT0(D,double,AST__DOUBLETYPE,value)
MAKE_MAPPUT0(F,float,AST__FLOATTYPE,value)
MAKE_MAPPUT0(C,const char *,AST__STRINGTYPE,astStore(NULL,value,strlen(value)+1))
MAKE_MAPPUT0(A,AstObject *,AST__OBJECTTYPE,(value?astClone(value):NULL))
MAKE_MAPPUT0(P,void *,AST__POINTERTYPE,value)
MAKE_MAPPUT0(S,short int,AST__SINTTYPE,value)
MAKE_MAPPUT0(B,unsigned char,AST__BYTETYPE,value)

/* Undefine the macro. */
#undef MAKE_MAPPUT0
#undef CHECK_A
#undef CHECK_I
#undef CHECK_S
#undef CHECK_D
#undef CHECK_F
#undef CHECK_C
#undef CHECK_P
#undef CHECK_B

/*
*++
*  Name:
c     astMapPut1<X>
f     AST_MAPPUT1<X>

*  Purpose:
*     Add a vector value to a KeyMap.

*  Type:
*     Public virtual function.

*  Synopsis:
c     #include "ast.h"
c     void astMapPut1<X>( AstKeyMap *this, const char *key, int size,
c                         const <X>type value[], const char *comment );
f     CALL AST_MAPPUT1<X>( THIS, KEY, SIZE, VALUE, COMMENT, STATUS )

*  Class Membership:
*     KeyMap method.

*  Description:
c     This is a set of functions
f     This is a set of routine
*     for adding vector values to a KeyMap. You should use a
c     function
f     routine
*     which matches the data type of the data you wish to add to the KeyMap
*     by replacing <X> in the generic
c     function name astMapPut1<X>
f     routine name AST_MAPPUT1<X>
*     by an appropriate 1-character type code (see the "Data Type Codes"
*     section below for the code appropriate to each supported data type).

*  Parameters:
c     this
f     THIS = INTEGER (Given)
*        Pointer to the KeyMap in which to store the supplied values.
c     key
f     KEY = CHARACTER * ( * ) (Given)
*        A character string to be stored with the values, which can later
*        be used to identify the values. Trailing spaces are ignored.
*        The supplied string is converted to upper case before use if the
*        KeyCase attribute is currently set to zero.
c     size
f     SIZE = INTEGER (Given)
*        The number of elements in the supplied array of values.
c     value
f     VALUE( * ) = <X>type (Given)
*        The array of values to be stored. The data type of this value should
*        match the 1-character type code appended to the
c        function name (e.g. if you are using astMapPut1A, the type of this
c        value should be "array of pointers to AstObject").
f        routine name (e.g. if you are using AST_MAPPUT1A, the type of this
f        value should be "integer pointer for an AstObject)".
c     comment
f     COMMENT = CHARACTER * ( * ) (Given)
f        A comment string to be stored with the values.
c        A pointer to a null-terminated comment string to be stored with the
c        values. A NULL pointer may be supplied, in which case no comment is
c        stored.
f     STATUS = INTEGER (Given and Returned)
f        The global status.

*  Notes:
*     - If the supplied key is already in use in the KeyMap, the new values
*     will replace the old values.

*  Data Type Codes:
*     To select the appropriate
c     function, you should replace <X> in the generic function name astMapPut1<X>
f     routine, you should replace <X> in the generic routine name AST_MAPPUT1<X>
*     with a 1-character data type code, so as to match the data type <X>type
*     of the data you are processing, as follows:
c     - D: double
c     - F: float
c     - I: int
c     - C: "const" pointer to null terminated character string
c     - A: Pointer to AstObject
c     - P: Generic "void *" pointer
c     - S: short int
c     - B: Unsigned byte (i.e. char)
f     - D: DOUBLE PRECISION
f     - R: REAL
f     - I: INTEGER
f     - C: CHARACTER
f     - A: INTEGER used to identify an AstObject
f     - S: INTEGER*2 (short integer)
f     - B: Unsigned byte
*
c     For example, astMapPut1D would be used to store "double" values,
c     while astMapPut1I would be used to store "int", etc.
f     For example, AST_MAPPUT1D would be used to store DOUBLE PRECISION values,
f     while AST_MAPPUT1I would be used to store INTEGER, etc.
c
c     Note that KeyMaps containing generic "void *" pointers cannot be
c     written out using astShow or astWrite. An error will be reported if
c     this is attempted.
*--
*/
/* Define a macro to implement the function for a specific data type. */
#define MAKE_MAPPUT1(X,Xtype,Itype,ValExp) \
static void MapPut1##X( AstKeyMap *this, const char *skey, int size, \
                        Xtype value[], const char *comment, \
                        int *status ) { \
\
/* Local Variables: */ \
   AstMapEntry *mapentry;  /* Pointer to parent MapEntry structure */ \
   AstMapEntry *oldent;    /* Pointer to existing MapEntry */ \
   Entry1##X *entry;       /* Structure holding the data for the new Entry */ \
   char keybuf[ AST__MXKEYLEN + 1 ]; /* Buffer for upper cas key */ \
   const char *key;        /* Pointer to key string to use */ \
   char *p;                /* Pointer to next key character */ \
   int itab;               /* Index of hash table element to use */ \
   int i;                  /* Loop count */ \
   int keylen;             /* Length of supplied key string */ \
   int keymember;          /* Identifier for existing key */ \
   int there;              /* Did the entry already exist in the KeyMap? */ \
\
/* Check the global error status. */ \
   if ( !astOK ) return; \
\
/* Perform any necessary checks on the supplied value to be stored. */ \
   CHECK_##X \
\
/* Convert the supplied key to upper case if required. */ \
   key = ConvertKey( this, skey, keybuf, AST__MXKEYLEN + 1, "astMapPut1" #X, \
                     status ); \
\
/* Allocate memory for the new MapEntry. */ \
   entry = astMalloc( sizeof( Entry1##X ) ); \
   if( astOK ) { \
\
/* Initialise the new structure.*/ \
      mapentry = (AstMapEntry *) entry; \
      InitMapEntry( mapentry, Itype, size, status ); \
\
/* Now store the new values. */ \
      keylen = strlen( key ); \
      mapentry->key = astStore( NULL, key, keylen + 1 ); \
      if( comment ) mapentry->comment = astStore( NULL, comment, strlen( comment ) + 1 ); \
      mapentry->defined = 1; \
      entry->value = astMalloc( sizeof( Xtype )*(size_t)size ); \
\
      if( astOK ) { \
         for( i = 0; i < size; i++ ) { \
            entry->value[ i ] = ValExp; \
         } \
\
/* Terminate the key string to exclude any trailing spaces. */ \
         p = (char *) mapentry->key + keylen; \
         while( --p >= mapentry->key ) { \
            if( *p == ' ' ) { \
               *p = 0; \
            } else { \
               break; \
            } \
         } \
      } \
\
/* Use the hash function to determine the element of the hash table in \
   which to store the new entry. */ \
      itab = HashFun( mapentry->key, this->mapsize - 1, &(mapentry->hash), status ); \
\
/* Remove any existing entry with the given key from the table element. \
   First save the key identifier. */ \
      oldent = RemoveTableEntry( this, itab, mapentry->key, status ); \
      if( oldent ) { \
         keymember = oldent->keymember; \
         oldent = FreeMapEntry( oldent, status ); \
         there = 1; \
      } else { \
         keymember = -1; \
         there = 0; \
      } \
\
/* If the KeyMap is locked we report an error if an attempt is made to add a value for \
   a new key. */ \
      if( !there && astGetMapLocked( this ) ) { \
         astError( AST__BADKEY, "astMapPut1" #X "(%s): Failed to add item \"%s\" to a KeyMap: " \
                   "\"%s\" is not a known item.", status, astGetClass( this ), key, key ); \
      } \
\
/* If all has gone OK, store the new entry at the head of the linked list \
   associated with the selected table entry. */ \
      if( astOK ) { \
         mapentry = AddTableEntry( this, itab, mapentry, keymember, status ); \
\
/* If anything went wrong, try to delete the new entry. */ \
      } else { \
         mapentry = FreeMapEntry( mapentry, status ); \
      } \
   } \
}

/* Define macros which perform any necessary checks on the supplied values
   to be stored. For Object entries, check that we are not adding a KeyMap
   which already contains "this". This avoids circular dependencies.
   Other types do not need any checks. */
#define CHECK_A \
for( i = 0; i < size; i++ ) { \
   CheckCircle( this, value[ i ], "astMapPut1A", status ); \
}
#define CHECK_I
#define CHECK_S
#define CHECK_B
#define CHECK_D
#define CHECK_F
#define CHECK_C
#define CHECK_P

/* Expand the above macro to generate a function for each required
   data type. */
MAKE_MAPPUT1(D,const double,AST__DOUBLETYPE,value[i])
MAKE_MAPPUT1(F,const float,AST__FLOATTYPE,value[i])
MAKE_MAPPUT1(I,const int,AST__INTTYPE,value[i])
MAKE_MAPPUT1(C,const char *const,AST__STRINGTYPE,astStore(NULL,value[i],strlen(value[i])+1))
MAKE_MAPPUT1(A,AstObject *const,AST__OBJECTTYPE,(value[i]?astClone(value[i]):NULL))
MAKE_MAPPUT1(P,void *const,AST__POINTERTYPE,value[i])
MAKE_MAPPUT1(S,const short int,AST__SINTTYPE,value[i])
MAKE_MAPPUT1(B,const unsigned char,AST__BYTETYPE,value[i])

/* Undefine the macro. */
#undef MAKE_MAPPUT1
#undef CHECK_A
#undef CHECK_I
#undef CHECK_B
#undef CHECK_S
#undef CHECK_D
#undef CHECK_F
#undef CHECK_C
#undef CHECK_P

void astMapPut1AId_( AstKeyMap *this, const char *skey, int size,
                     AstObject *const value[], const char *comment,
                     int *status ) {
/*
*  Name:
*     astMapPut1AId_

*  Purpose:
*     Add a vector of AstObject pointers to a KeyMap.

*  Type:
*     Private function.

*  Synopsis:
*     #include "ast.h"
*     void astMapPut1A( AstKeyMap *this, const char *key, int size,
*                       AstObject *const value[], const char *comment )

*  Class Membership:
*     KeyMap method.

*  Description:
*     This is the public implementation of the astMapPut1A function
*     It is identical to astMapPut1A_ except that ID values are supplied
*     via the "value" parameter instead of a true C pointers.

*  Parameters:
*     (see astMapPut1<X>)

*/

/* Local Variables: */
   AstMapEntry *mapentry;  /* Pointer to parent MapEntry structure */
   AstMapEntry *oldent;    /* Pointer to existing MapEntry */
   AstObject *op;          /* Object pointer */
   Entry1A *entry;         /* Structure holding the data for the new Entry */
   char *p;                /* Pointer to next key character */
   char keybuf[ AST__MXKEYLEN + 1 ]; /* Buffer for upper cas key */ \
   const char *key;        /* Pointer to key string to use */ \
   int i;                  /* Loop count */
   int itab;               /* Index of hash table element to use */
   int keylen;             /* Length of supplied key string */
   int keymember;          /* Identifier for existing key */
   int there;              /* Did the entry already exist in the KeyMap? */

/* Check the global error status. */
   if ( !astOK ) return;

/* Convert the supplied key to upper case if required. */
   key = ConvertKey( this, skey, keybuf, AST__MXKEYLEN + 1, "astMapPut1A",
                     status );

/* Allocate memory for the new MapEntry. */
   entry = astMalloc( sizeof( Entry1A ) );
   if( astOK ) {

/* Initialise the new structure.*/
      mapentry = (AstMapEntry *) entry;
      InitMapEntry( mapentry, AST__OBJECTTYPE, size, status );

/* Now store the new values. */
      keylen = strlen( key );
      mapentry->key = astStore( NULL, key, keylen + 1 );
      if( comment ) mapentry->comment = astStore( NULL, comment, strlen( comment ) + 1 );
      mapentry->defined = 1;
      entry->value = astMalloc( sizeof( AstObject * )*(size_t)size );

      if( astOK ) {
         for( i = 0; i < size; i++ ) {
            op = value[ i ] ? astMakePointer( value[ i ] ) : NULL;
            entry->value[ i ] = op ? astClone( op ) : NULL;
         }

/* Terminate the key string to exclude any trailing spaces. */ \
         p = (char *) mapentry->key + keylen;
         while( --p >= mapentry->key ) {
            if( *p == ' ' ) {
               *p = 0;
            } else {
               break;
            }
         }
      }

/* Use the hash function to determine the element of the hash table in
   which to store the new entry. */
      itab = HashFun( mapentry->key, this->mapsize - 1, &(mapentry->hash), status );

/* Remove any existing entry with the given key from the table element. */
      oldent = RemoveTableEntry( this, itab, mapentry->key, status );
      if( oldent ) {
         keymember = oldent->keymember;
         oldent = FreeMapEntry( oldent, status );
         there = 1;
      } else {
         there = 0;
         keymember = -1;
      }

/* If the KeyMap is locked we report an error if an attempt is made to add a value for
   a new key. */
      if( !there && astGetMapLocked( this ) ) {
         astError( AST__BADKEY, "astMapPut1A(%s): Failed to add item \"%s\" to a KeyMap: "
                   "\"%s\" is not a known item.", status, astGetClass( this ), key, key );
      }

/* If all has gone OK, store the new entry at the head of the linked list
   associated with the selected table entry. */
      if( astOK ) {
         mapentry = AddTableEntry( this, itab, mapentry, keymember, status );

/* If anything went wrong, try to delete the new entry. */
      } else {
         mapentry = FreeMapEntry( mapentry, status );
      }
   }
}

static void MapPutU( AstKeyMap *this, const char *skey, const char *comment,
                     int *status ) {
/*
*++
*  Name:
c     astMapPutU
f     AST_MAPPUTU

*  Purpose:
*     Add an entry to a KeyMap with an undefined value.

*  Type:
*     Public virtual function.

*  Synopsis:
c     #include "ast.h"
c     void astMapPutU( AstKeyMap *this, const char *key, const char *comment );
f     CALL AST_MAPPUTU( THIS, KEY, COMMENT, STATUS )

*  Class Membership:
*     KeyMap method.

*  Description:
c     This function
f     This routine
*     adds a new entry to a KeyMap, but no value is stored with the
*     entry. The entry therefore has a special data type represented by
*     symbolic constant AST__UNDEFTYPE.
*
*     An example use is to add entries with undefined values to a KeyMap
*     prior to locking them with the MapLocked attribute. Such entries
*     can act as placeholders for values that can be added to the KeyMap
*     later.

*  Parameters:
c     this
f     THIS = INTEGER (Given)
*        Pointer to the KeyMap in which to store the supplied value.
c     key
f     KEY = CHARACTER * ( * ) (Given)
*        A character string to be stored with the value, which can later
*        be used to identify the value. Trailing spaces are ignored.
*        The supplied string is converted to upper case before use if the
*        KeyCase attribute is currently set to zero.
c     comment
f     COMMENT = CHARACTER * ( * ) (Given)
f        A comment string to be stored with the value.
c        A pointer to a null-terminated comment string to be stored with the
c        value. A NULL pointer may be supplied, in which case no comment is
c        stored.
f     STATUS = INTEGER (Given and Returned)
f        The global status.

*  Notes:
*     - If the supplied key is already in use in the KeyMap, the value
*     associated with the key will be removed.

*--
*/

/* Local Variables: */
   AstMapEntry *mapentry;  /* Pointer to parent MapEntry structure */
   AstMapEntry *oldent;    /* Pointer to existing MapEntry */
   char keybuf[ AST__MXKEYLEN + 1 ]; /* Buffer for upper cas key */
   const char *key;        /* Pointer to key string to use */
   char *p;                /* Pointer to next key character */
   int itab;               /* Index of hash table element to use */
   int keylen;             /* Length of supplied key string */
   int keymember;          /* Identifier for existing key */
   int there;              /* Did the entry already exist in the KeyMap? */

/* Check the global error status. */
   if ( !astOK ) return;

/* Convert the supplied key to upper case if required. */
   key = ConvertKey( this, skey, keybuf, AST__MXKEYLEN + 1, "astMapPutU",
                     status );

/* Allocate memory for the new MapEntry. */
   mapentry = astMalloc( sizeof( AstMapEntry ) );
   if( astOK ) {

/* Initialise the new structure.*/
      InitMapEntry( mapentry, AST__UNDEFTYPE, 0, status );

/* Now store the new values. */
      keylen = strlen( key );
      mapentry->key = astStore( NULL, key, keylen + 1 );
      if( comment ) mapentry->comment = astStore( NULL, comment, strlen( comment ) + 1 );
      mapentry->defined = 0;

/* Terminate the key string to exclude any trailing spaces. */
      if( astOK ) {
         p = (char *) mapentry->key + keylen;
         while( --p >= mapentry->key ) {
            if( *p == ' ' ) {
               *p = 0;
            } else {
               break;
            }
         }
      }

/* Use the hash function to determine the element of the hash table in
   which to store the new entry. */
      itab = HashFun( mapentry->key, this->mapsize - 1, &(mapentry->hash), status );

/* Remove any existing entry with the given key from the table element. */
      oldent = RemoveTableEntry( this, itab, mapentry->key, status );
      if( oldent ) {
         keymember = oldent->keymember;
         oldent = FreeMapEntry( oldent, status );
         there = 1;
      } else {
         keymember = -1;
         there = 0;
      }

/* If the KeyMap is locked we report an error if an attempt is made to add a value for
   a new key. */
      if( !there && astGetMapLocked( this ) ) {
         astError( AST__BADKEY, "astMapPutU(%s): Failed to add item \"%s\" to a KeyMap: "
                   "\"%s\" is not a known item.", status, astGetClass( this ), key, key );
      }

/* If all has gone OK, store the new entry at the head of the linked list
   associated with the selected table entry. */
      if( astOK ) {
         mapentry = AddTableEntry( this, itab, mapentry, keymember, status );

/* If anything went wrong, try to delete the new entry. */
      } else {
         mapentry = FreeMapEntry( mapentry, status );
      }
   }
}

/*
*++
*  Name:
c     astMapGet0<X>
f     AST_MAPGET0<X>

*  Purpose:
*     Get a scalar value from a KeyMap.

*  Type:
*     Public virtual function.

*  Synopsis:
c     #include "ast.h"
c     int astMapGet0<X>( AstKeyMap *this, const char *key, <X>type *value );
f     RESULT = AST_MAPGET0<X>( THIS, KEY, VALUE, STATUS )

f     RESULT = AST_MAPGET0C( THIS, KEY, VALUE, L, STATUS )

*  Class Membership:
*     KeyMap method.

*  Description:
*     This is a set of functions for retrieving a scalar value from a KeyMap.
*     You should replace <X> in the generic function name
c     astMapGet0<X>
f     AST_MAPGET0<X>
*     by an appropriate 1-character type code (see the "Data Type Codes"
*     section below for the code appropriate to each supported data type).
*     The stored value is converted to the data type indiced by <X>
*     before being returned (an error is reported if it is not possible to
*     convert the stored value to the requested data type).
f     Note, the version of this function which returns character strings,
f     AST_MAPGET0C, has an extra parameter in which is returned the number
f     of characters written into the supplied CHARACTER variable.

*  Parameters:
c     this
f     THIS = INTEGER (Given)
*        Pointer to the KeyMap.
c     key
f     KEY = CHARACTER * ( * ) (Given)
*        The character string identifying the value to be retrieved. Trailing
*        spaces are ignored. The supplied string is converted to upper
*        case before use if the KeyCase attribute is currently set to zero.
c     value
f     VALUE = <X>type (Returned)
c        A pointer to a buffer in which to return the requested value.
f        The requested value.
*        If the requested key is not found, or if it is found but has an
*        undefined value (see
c        astMapPutU),
f        AST_MAPPUTU),
*        then the contents of the
*        buffer on entry to this function will be unchanged on exit.
c        For pointer types ("A" and "C"), the buffer should be a suitable
c        pointer, and the address of this pointer should be supplied as the
c        "value" parameter.
f     L = INTEGER (Returned)
f        This parameter is only present in the interface for the AST_MAPGET0C
f        function. It is returned holding the number of characters
f        written into the CHARACTER variable supplied for parameter VALUE.
f     STATUS = INTEGER (Given and Returned)
f        The global status.

*  Returned Value:
c     astMapGet0<X>()
f     AST_MAPGET0<X> = LOGICAL
c        A non-zero value
f        .TRUE.
*        is returned if the requested key name was found, and does not have
*        an undefined value (see
c        astMapPutU). Zero
f        AST_MAPPUTU). .FALSE.
*        is returned otherwise.

*  Notes:
*     - No error is reported if the requested key cannot be found in the
*     given KeyMap, but a
c     zero
f     .FALSE.
*     value will be returned as the function value. The supplied buffer
*     will be returned unchanged.
*     - If the stored value is a vector value, then the first value in
*     the vector will be returned.
c     - A string pointer returned by astMapGet0C is guaranteed to remain valid
c     and the string to which it points will not be over-written for a
c     total of 50 successive invocations of this function. After this,
c     the memory containing the string may be re-used, so a copy of
c     the string should be made if it is needed for longer than this.
*     - If the returned value is an AST Object pointer, the Object's reference
*     count is incremented by this call. Any subsequent changes made to
*     the Object using the returned pointer will be reflected in any
*     any other active pointers for the Object. The returned pointer
*     should be annulled using
c     astAnnul
f     AST_ANNUL
*     when it is no longer needed.

*  Data Type Codes:
*     To select the appropriate
c     function, you should replace <X> in the generic function name astMapGet0<X>
f     routine, you should replace <X> in the generic routine name AST_MAPGET0<X>
*     with a 1-character data type code, so as to match the data type <X>type
*     of the data you are processing, as follows:
c     - F: float
c     - D: double
c     - I: int
c     - C: "const" pointer to null terminated character string
c     - A: Pointer to AstObject
c     - P: Generic "void *" pointer
c     - S: short int
c     - B: Unsigned byte (i.e. word)
f     - D: DOUBLE PRECISION
f     - R: REAL
f     - I: INTEGER
f     - C: CHARACTER
f     - A: INTEGER used to identify an AstObject
f     - S: INTEGER*2 (short integer)
f     - B: Unsigned byte
*
c     For example, astMapGet0D would be used to get a "double" value,
c     while astMapGet0I would be used to get an "int", etc.
f     For example, AST_MAPGET0D would be used to get a DOUBLE PRECISION value,
f     while AST_MAPGET0I would be used to get an INTEGER, etc.
*--
*/
/* Define a macro to implement the function for a specific data type. */
#define MAKE_MAPGET0(X,Xtype,Itype) \
static int MapGet0##X( AstKeyMap *this, const char *skey, Xtype *value, int *status ) { \
\
/* Local Variables: */ \
   AstMapEntry *mapentry;  /* Pointer to parent MapEntry structure */ \
   const char *key;        /* Pointer to key string to use */ \
   char keybuf[ AST__MXKEYLEN + 1 ]; /* Buffer for upper cas key */ \
   int itab;               /* Index of hash table element to use */ \
   int raw_type;           /* Data type of stored value */ \
   int result;             /* Returned flag */ \
   unsigned long hash;     /* Full width hash value */ \
   void *raw;              /* Pointer to stored value */ \
\
/* Initialise */ \
   result = 0; \
\
/* Check the global error status. */ \
   if ( !astOK ) return result; \
\
/* Convert the supplied key to upper case if required. */ \
   key = ConvertKey( this, skey, keybuf, AST__MXKEYLEN + 1, "astMapGet0" #X, \
                     status ); \
\
/* Use the hash function to determine the element of the hash table in \
   which the key will be stored. */ \
   itab = HashFun( key, this->mapsize - 1, &hash, status ); \
\
/* Search the relevent table entry for the required MapEntry. */ \
   mapentry = SearchTableEntry( this, itab, key, status ); \
\
/* Skip rest if the key was not found. */ \
   if( mapentry ) { \
\
/* Get the address of the raw value, and its data type. */ \
      raw_type = mapentry->type; \
      if( raw_type == AST__INTTYPE ){ \
         if( mapentry->nel == 0 ) { \
            raw = &( ((Entry0I *)mapentry)->value ); \
         } else { \
            raw = ((Entry1I *)mapentry)->value; \
         } \
\
      } else if( raw_type == AST__SINTTYPE ){ \
         if( mapentry->nel == 0 ) { \
            raw = &( ((Entry0S *)mapentry)->value ); \
         } else { \
            raw = ((Entry1S *)mapentry)->value; \
         } \
\
      } else if( raw_type == AST__BYTETYPE ){ \
         if( mapentry->nel == 0 ) { \
            raw = &( ((Entry0B *)mapentry)->value ); \
         } else { \
            raw = ((Entry1B *)mapentry)->value; \
         } \
\
      } else if( raw_type == AST__DOUBLETYPE ){ \
         if( mapentry->nel == 0 ) { \
            raw = &( ((Entry0D *)mapentry)->value ); \
         } else { \
            raw = ((Entry1D *)mapentry)->value; \
         } \
\
      } else if( raw_type == AST__FLOATTYPE ){ \
         if( mapentry->nel == 0 ) { \
            raw = &( ((Entry0F *)mapentry)->value ); \
         } else { \
            raw = ((Entry1F *)mapentry)->value; \
         } \
\
      } else if( raw_type == AST__POINTERTYPE ){ \
         if( mapentry->nel == 0 ) { \
            raw = &( ((Entry0P *)mapentry)->value ); \
         } else { \
            raw = ((Entry1P *)mapentry)->value; \
         } \
\
      } else if( raw_type == AST__STRINGTYPE ){ \
         if( mapentry->nel == 0 ) { \
            raw = &( ((Entry0C *)mapentry)->value ); \
         } else { \
            raw = ((Entry1C *)mapentry)->value; \
         } \
\
      } else if( raw_type == AST__OBJECTTYPE ){ \
         if( mapentry->nel == 0 ) { \
            raw = &( ((Entry0A *)mapentry)->value ); \
         } else { \
            raw = ((Entry1A *)mapentry)->value; \
         } \
\
      } else if( raw_type == AST__UNDEFTYPE ){ \
         raw = NULL; \
\
      } else { \
         raw = NULL; \
         astError( AST__INTER, "astMapGet0<X>(KeyMap): Illegal map entry data " \
                   "type %d encountered (internal AST programming error).", status, \
                   raw_type ); \
      } \
\
/* Convert the value, storing the result the supplied buffer. Report an \
   error if conversion is not possible. */ \
      if( !raw ) { \
         result = 0; \
\
      } else if( !ConvertValue( raw, raw_type, value, Itype, status ) && astOK ){ \
         astError( AST__MPGER, "astMapGet0" #X "(%s): The value of KeyMap key " \
                   "\"%s\" cannot be read using the requested data " \
                   "type.", status,astGetClass( this ), key ); \
\
      } else { \
         result = 1; \
      } \
\
/* If the KeyError attribute is non-zero, report an error if the key is not \
   found */ \
   } else if( astGetKeyError( this ) && astOK ) { \
      astError( AST__MPKER, "astMapGet0" #X "(%s): No value was found for " \
                "%s in the supplied KeyMap.", status, astGetClass( this ), \
                key ); \
   } \
\
/* If an error occurred, return zero. */ \
   if( !astOK ) result = 0; \
\
/* Return the result.*/ \
   return result; \
}

/* Expand the above macro to generate a function for each required
   data type. */
MAKE_MAPGET0(I,int,AST__INTTYPE)
MAKE_MAPGET0(D,double,AST__DOUBLETYPE)
MAKE_MAPGET0(F,float,AST__FLOATTYPE)
MAKE_MAPGET0(C,const char *,AST__STRINGTYPE)
MAKE_MAPGET0(A,AstObject *,AST__OBJECTTYPE)
MAKE_MAPGET0(P,void *,AST__POINTERTYPE)
MAKE_MAPGET0(S,short int,AST__SINTTYPE)
MAKE_MAPGET0(B,unsigned char,AST__BYTETYPE)

/* Undefine the macro. */
#undef MAKE_MAPGET0

int astMapGet0AId_( AstKeyMap *this, const char *skey, AstObject **value, int *status ) {
/*
*  Name:
*     astMapGet0AId_

*  Purpose:
*     Get a scalar AstObject pointer from a KeyMap.

*  Type:
*     Private function.

*  Synopsis:
*     #include "ast.h"
*     int astMapGet0A( AstKeyMap *this, const char *key, AstObject **value )

*  Class Membership:
*     KeyMap method.

*  Description:
*     This is the public implementation of the astMapGet0A function
*     It is identical to astMapGet0A_ except that an ID value is returned
*     via the "value" parameter instead of a true C pointer. This is required
*     because this conversion cannot be performed by the macro that invokes
*     the function.

*  Parameters:
*     (see astMapGet0<X>)

*/

/* Local Variables: */
   AstMapEntry *mapentry;  /* Pointer to parent MapEntry structure */
   char keybuf[ AST__MXKEYLEN + 1 ]; /* Buffer for upper cas key */ \
   const char *key;        /* Pointer to key string to use */ \
   int itab;               /* Index of hash table element to use */
   int raw_type;           /* Data type of stored value */
   int result;             /* Returned flag */
   unsigned long hash;     /* Full width hash value */
   void *raw;              /* Pointer to stored value */

/* Initialise */
   result = 0;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Convert the supplied key to upper case if required. */
   key = ConvertKey( this, skey, keybuf, AST__MXKEYLEN + 1, "astMapGet0A",
                     status );

/* Use the hash function to determine the element of the hash table in
   which the key will be stored. */
   itab = HashFun( key, this->mapsize - 1, &hash, status );

/* Search the relevent table entry for the required MapEntry. */
   mapentry = SearchTableEntry( this, itab, key, status );

/* Skip rest if the key was not found. */
   if( mapentry ) {

/* Get the address of the raw value, and its data type. */
      raw_type = mapentry->type;
      if( raw_type == AST__INTTYPE ){
         if( mapentry->nel == 0 ) {
            raw = &( ((Entry0I *)mapentry)->value );
         } else {
            raw = ((Entry1I *)mapentry)->value;
         }

      } else if( raw_type == AST__POINTERTYPE ){
         if( mapentry->nel == 0 ) {
            raw = &( ((Entry0P *)mapentry)->value );
         } else {
            raw = ((Entry1P *)mapentry)->value;
         }

      } else if( raw_type == AST__SINTTYPE ){
         if( mapentry->nel == 0 ) {
            raw = &( ((Entry0S *)mapentry)->value );
         } else {
            raw = ((Entry1S *)mapentry)->value;
         }

      } else if( raw_type == AST__BYTETYPE ){
         if( mapentry->nel == 0 ) {
            raw = &( ((Entry0B *)mapentry)->value );
         } else {
            raw = ((Entry1B *)mapentry)->value;
         }

      } else if( raw_type == AST__DOUBLETYPE ){
         if( mapentry->nel == 0 ) {
            raw = &( ((Entry0D *)mapentry)->value );
         } else {
            raw = ((Entry1D *)mapentry)->value;
         }

      } else if( raw_type == AST__FLOATTYPE ){
         if( mapentry->nel == 0 ) {
            raw = &( ((Entry0F *)mapentry)->value );
         } else {
            raw = ((Entry1F *)mapentry)->value;
         }

      } else if( raw_type == AST__STRINGTYPE ){
         if( mapentry->nel == 0 ) {
            raw = &( ((Entry0C *)mapentry)->value );
         } else {
            raw = ((Entry1C *)mapentry)->value;
         }

      } else if( raw_type == AST__OBJECTTYPE ){
         if( mapentry->nel == 0 ) {
            raw = &( ((Entry0A *)mapentry)->value );
         } else {
            raw = ((Entry1A *)mapentry)->value;
         }

      } else if( raw_type == AST__UNDEFTYPE ){
         raw = NULL;

      } else {
         raw = NULL;
         astError( AST__INTER, "astMapGet0<X>(KeyMap): Illegal map entry data "
                   "type %d encountered (internal AST programming error).", status,
                   raw_type );
      }

/* Convert the value, storing the result the supplied buffer. Report an
   error if conversion is not possible. */
      if( !raw ) {
         result = 0;

      } else if( !ConvertValue( raw, raw_type, value, AST__OBJECTTYPE, status ) && astOK ){
         astError( AST__MPGER, "astMapGet0A(%s): The value of KeyMap key "
                   "\"%s\" cannot be read using the requested data "
                   "type.", status, astGetClass( this ), key );

      } else {
         result = 1;
      }

/* If the KeyError attribute is non-zero, report an error if the key is not
   found */
   } else if( astGetKeyError( this ) && astOK ) {
      astError( AST__MPKER, "astMapGet0A(%s): No value was found for "
                "%s in the supplied KeyMap.", status, astGetClass( this ),
                key );
   }

/* If required, return an ID value for the Object. */
   if( result && *value ) *value = astMakeId( *value );

/* Return the result.*/
   return result;
}

/*
*++
*  Name:
c     astMapGet1<X>
f     AST_MAPGET1<X>

*  Purpose:
*     Get a vector value from a KeyMap.

*  Type:
*     Public virtual function.

*  Synopsis:
c     #include "ast.h"
c     int astMapGet1<X>( AstKeyMap *this, const char *key, int mxval,
c                        int *nval, <X>type *value )
c     int astMapGet1C( AstKeyMap *this, const char *key, int l, int mxval,
c                      int *nval, const char *value )
f     RESULT = AST_MAPGET1<X>( THIS, KEY, MXVAL, NVAL, VALUE, STATUS )

*  Class Membership:
*     KeyMap method.

*  Description:
*     This is a set of functions for retrieving a vector value from a KeyMap.
*     You should replace <X> in the generic function name
c     astMapGet1<X>
f     AST_MAPGET1<X>
*     by an appropriate 1-character type code (see the "Data Type Codes"
*     section below for the code appropriate to each supported data type).
*     The stored value is converted to the data type indiced by <X>
*     before being returned (an error is reported if it is not possible to
*     convert the stored value to the requested data type).
c     Note, the astMapGet1C function has an extra parameter "l" which
c     specifies the maximum length of each string to be stored in the
c     "value" buffer (see the "astMapGet1C" section below).

*  Parameters:
c     this
f     THIS = INTEGER (Given)
*        Pointer to the KeyMap.
c     key
f     KEY = CHARACTER * ( * ) (Given)
*        The character string identifying the value to be retrieved. Trailing
*        spaces are ignored.
*        The supplied string is converted to upper case before use if the
*        KeyCase attribute is currently set to zero.
c     mxval
f     MXVAL = INTEGER (Given)
*        The number of elements in the
c        "value" array.
f        VALUE array.
c     nval
f     NVAL = INTEGER (Returned)
c        The address of an integer in which to put the
f        The
*        number of elements stored in the
c        "value" array.
*        Any unused elements of the array are left unchanged.
c     value
f     VALUE( MXVAL ) = <X>type (Returned)
c        A pointer to an array in which to return the requested values.
f        The requested values.
*        If the requested key is not found, or if it is found but has an
*        undefined value (see
c        astMapPutU),
f        AST_MAPPUTU),
*        then the contents of the
*        buffer on entry to this function will be unchanged on exit.
f     STATUS = INTEGER (Given and Returned)
f        The global status.

*  Returned Value:
c     astMapGet1<X>()
f     AST_MAPGET1<X> = LOGICAL
c        A non-zero value
f        .TRUE.
*        is returned if the requested key name was found, and does not have
*        an undefined value (see
c        astMapPutU). Zero
f        AST_MAPPUTU). .FALSE.
*        is returned otherwise.

*  Notes:
*     - No error is reported if the requested key cannot be found in the
*     given KeyMap, but a
c     zero
f     .FALSE.
*     value will be returned as the function value. The supplied array
*     will be returned unchanged.
*     - If the stored value is a scalar value, then the value will be
*     returned in the first element of the supplied array, and
c     "nval"
f     NVAL
*     will be returned set to 1.

c  astMapGet1C:
c     The "value" buffer supplied to the astMapGet1C function should be a
c     pointer to a character array with "mxval*l" elements, where "l" is
c     the maximum length of a string to be returned. The value of "l"
c     should be supplied as an extra parameter following "key" when
c     invoking astMapGet1C, and should include space for a terminating
c     null character.

*  Data Type Codes:
*     To select the appropriate
c     function, you should replace <X> in the generic function name astMapGet1<X>
f     routine, you should replace <X> in the generic routine name AST_MAPGET1<X>
*     with a 1-character data type code, so as to match the data type <X>type
*     of the data you are processing, as follows:
c     - D: double
c     - F: float
c     - I: int
c     - C: "const" pointer to null terminated character string
c     - A: Pointer to AstObject
c     - P: Generic "void *" pointer
c     - S: short int
c     - B: Unsigned byte (i.e. char)
f     - D: DOUBLE PRECISION
f     - R: REAL
f     - I: INTEGER
f     - C: CHARACTER
f     - A: INTEGER used to identify an AstObject
f     - S: INTEGER*2 (short integer)
f     - B: Unsigned byte
*
c     For example, astMapGet1D would be used to get "double" values, while
c     astMapGet1I would be used to get "int" values, etc. For D or I, the
c     supplied "value" parameter should be a pointer to an array of doubles
c     or ints, with "mxval" elements. For C, the supplied "value" parameter
c     should be a pointer to a character string with "mxval*l" elements.
c     For A, the supplied "value" parameter should be a pointer to an
c     array of AstObject pointers.
f     For example, AST_MAPGET1D would be used to get DOUBLE PRECISION values,
f     while AST_MAPGET1I would be used to get INTEGER values, etc.

*--
*/
/* Define a macro to implement the function for a specific data type
(excluding "C" since that needs an extra parameter). */
#define MAKE_MAPGET1(X,Xtype,Itype) \
static int MapGet1##X( AstKeyMap *this, const char *skey, int mxval, int *nval, Xtype *value, int *status ) { \
\
/* Local Variables: */ \
   AstMapEntry *mapentry;  /* Pointer to parent MapEntry structure */ \
   const char *key;        /* Pointer to key string to use */ \
   char keybuf[ AST__MXKEYLEN + 1 ]; /* Buffer for upper cas key */ \
   int i;                  /* Element index */ \
   int itab;               /* Index of hash table element to use */ \
   int nel;                /* Number of elements in raw vector */ \
   int raw_type;           /* Data type of stored value */ \
   int result;             /* Returned flag */ \
   size_t raw_size;        /* Size of a single raw value */ \
   unsigned long hash;     /* Full width hash value */ \
   void *raw;              /* Pointer to stored value */ \
\
/* Initialise */ \
   result = 0; \
   *nval = 0; \
\
/* Check the global error status. */ \
   if ( !astOK ) return result; \
\
/* Convert the supplied key to upper case if required. */ \
   key = ConvertKey( this, skey, keybuf, AST__MXKEYLEN + 1, "astMapGet1" #X, \
                     status ); \
\
/* Use the hash function to determine the element of the hash table in \
   which the key will be stored. */ \
   itab = HashFun( key, this->mapsize - 1, &hash, status ); \
\
/* Search the relevent table entry for the required MapEntry. */ \
   mapentry = SearchTableEntry( this, itab, key, status ); \
\
/* Skip rest if the key was not found. */ \
   if( mapentry ) { \
      result = 1; \
\
/* Get the address of the first raw value, and its data type. Also get \
   the size of each element of the vector. */ \
      nel = mapentry->nel; \
      raw_type = mapentry->type; \
      if( raw_type == AST__INTTYPE ){ \
         raw_size = sizeof( int ); \
         if( nel == 0 ) { \
            raw = &( ((Entry0I *)mapentry)->value ); \
         } else { \
            raw = ((Entry1I *)mapentry)->value; \
         } \
\
      } else if( raw_type == AST__DOUBLETYPE ){ \
         raw_size = sizeof( double ); \
         if( nel == 0 ) { \
            raw = &( ((Entry0D *)mapentry)->value ); \
         } else { \
            raw = ((Entry1D *)mapentry)->value; \
         } \
\
      } else if( raw_type == AST__SINTTYPE ){ \
         raw_size = sizeof( short int ); \
         if( nel == 0 ) { \
            raw = &( ((Entry0S *)mapentry)->value ); \
         } else { \
            raw = ((Entry1S *)mapentry)->value; \
         } \
\
      } else if( raw_type == AST__BYTETYPE ){ \
         raw_size = sizeof( unsigned char ); \
         if( nel == 0 ) { \
            raw = &( ((Entry0B *)mapentry)->value ); \
         } else { \
            raw = ((Entry1B *)mapentry)->value; \
         } \
\
      } else if( raw_type == AST__POINTERTYPE ){ \
         raw_size = sizeof( void * ); \
         if( nel == 0 ) { \
            raw = &( ((Entry0P *)mapentry)->value ); \
         } else { \
            raw = ((Entry1P *)mapentry)->value; \
         } \
\
      } else if( raw_type == AST__FLOATTYPE ){ \
         raw_size = sizeof( float ); \
         if( nel == 0 ) { \
            raw = &( ((Entry0F *)mapentry)->value ); \
         } else { \
            raw = ((Entry1F *)mapentry)->value; \
         } \
\
      } else if( raw_type == AST__STRINGTYPE ){ \
         raw_size = sizeof( const char * ); \
         if( nel == 0 ) { \
            raw = &( ((Entry0C *)mapentry)->value ); \
         } else { \
            raw = ((Entry1C *)mapentry)->value; \
         } \
\
      } else if( raw_type == AST__OBJECTTYPE ){ \
         raw_size = sizeof( AstObject * ); \
         if( nel == 0 ) { \
            raw = &( ((Entry0A *)mapentry)->value ); \
         } else { \
            raw = ((Entry1A *)mapentry)->value; \
         } \
\
      } else if( raw_type == AST__UNDEFTYPE ){ \
         raw_size = 0; \
         raw = NULL; \
\
      } else { \
         raw_size = 0; \
         raw = NULL; \
         astError( AST__INTER, "astMapGet1<X>(KeyMap): Illegal map entry data " \
                   "type %d encountered (internal AST programming error).", status, \
                   raw_type ); \
      } \
\
/* Treat scalars as single-value vectors. */ \
      if( nel == 0 ) nel = 1; \
\
/* Ensure no more than "mxval" values are returned. */ \
      if( nel > mxval ) nel = mxval; \
\
/* Return the number of values stored in the buffer. */ \
      *nval = nel; \
\
/* Loop round all values in the vector. */ \
      for( i = 0; i < nel && astOK; i++ ) { \
\
/* Convert the value, storing the result in the supplied buffer. Report an \
   error if conversion is not possible. */ \
         if( !raw ) { \
            result = 0; \
\
         } else if( !ConvertValue( raw, raw_type, value + i, Itype, status ) && astOK ){ \
            astError( AST__MPGER, "astMapGet1" #X "(%s): The value of " \
                      "element %d of KeyMap key \"%s\" cannot be read using " \
                      "the requested data type.", status,astGetClass( this ), i + 1, key ); \
         } \
\
/* Increment the pointers to the next raw value. */ \
         raw = (char *) raw + raw_size; \
      } \
\
/* If the KeyError attribute is non-zero, report an error if the key is not \
   found */ \
   } else if( astGetKeyError( this ) && astOK ) { \
      astError( AST__MPKER, "astMapGet1" #X "(%s): No value was found for " \
                "%s in the supplied KeyMap.", status, astGetClass( this ), \
                key ); \
   } \
\
/* If an error occurred,return zero. */ \
   if( !astOK ) result = 0; \
\
/* Return the result.*/ \
   return result; \
}

/* Expand the above macro to generate a function for each required
   data type (except C which is done differently). */
MAKE_MAPGET1(I,int,AST__INTTYPE)
MAKE_MAPGET1(D,double,AST__DOUBLETYPE)
MAKE_MAPGET1(F,float,AST__FLOATTYPE)
MAKE_MAPGET1(A,AstObject *,AST__OBJECTTYPE)
MAKE_MAPGET1(P,void *,AST__POINTERTYPE)
MAKE_MAPGET1(S,short int,AST__SINTTYPE)
MAKE_MAPGET1(B,unsigned char,AST__BYTETYPE)

/* Undefine the macro. */
#undef MAKE_MAPGET1


static int MapGet1C( AstKeyMap *this, const char *skey, int l, int mxval,
                     int *nval, char *value, int *status ) {
/*
*  Name:
*     MapGet1C

*  Purpose:
*     Get a vector value from a KeyMap.

*  Type:
*     Private member function.

*  Synopsis:
*     #include "ast.h"
*     int MapGet1C( AstKeyMap *this, const char *key, int l, int mxval,
*                   int *nval, char *value, int *status )

*  Class Membership:
*     KeyMap method.

*  Description:
*     This is the implementation of astMapGet1<X> for <X> = "C". We
*     cannot use the MAKE_MAPGET1 macro for this because the string
*     version of this function has an extra parameter giving the maximum
*     length of each string which can be stored in the supplied buffer.

*  Parameters:
*     (see astMapGet1<X>)
*/

/* Local Variables: */
   AstMapEntry *mapentry;  /* Pointer to parent MapEntry structure */
   char *val;              /* Pointer to next buffer element */
   char keybuf[ AST__MXKEYLEN + 1 ]; /* Buffer for upper cas key */ \
   const char *cvalue;     /* Pointer to converted string */
   const char *key;        /* Pointer to key string to use */ \
   int i;                  /* Element index */
   int itab;               /* Index of hash table element to use */
   int nel;                /* Number of elements in raw vector */
   int raw_type;           /* Data type of stored value */
   int result;             /* Returned flag */
   size_t raw_size;        /* Size of a single raw value */
   unsigned long hash;     /* Full width hash value */
   void *raw;              /* Pointer to stored value */

/* Initialise */
   result = 0;
   *nval = 0;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Convert the supplied key to upper case if required. */
   key = ConvertKey( this, skey, keybuf, AST__MXKEYLEN + 1, "astMapGet1C",
                     status );

/* Use the hash function to determine the element of the hash table in
   which the key will be stored. */
   itab = HashFun( key, this->mapsize - 1, &hash, status );

/* Search the relevent table entry for the required MapEntry. */
   mapentry = SearchTableEntry( this, itab, key, status );

/* Skip rest if the key was not found. */
   if( mapentry ) {
      result = 1;

/* Get the address of the first raw value, and its data type. Also get
   the size of each element of the vector. */
      nel = mapentry->nel;
      raw_type = mapentry->type;
      if( raw_type == AST__INTTYPE ){
         raw_size = sizeof( int );
         if( nel == 0 ) {
            raw = &( ((Entry0I *)mapentry)->value );
         } else {
            raw = ((Entry1I *)mapentry)->value;
         }

      } else if( raw_type == AST__POINTERTYPE ){
         raw_size = sizeof( void * );
         if( nel == 0 ) {
            raw = &( ((Entry0P *)mapentry)->value );
         } else {
            raw = ((Entry1P *)mapentry)->value;
         }

      } else if( raw_type == AST__DOUBLETYPE ){
         raw_size = sizeof( double );
         if( nel == 0 ) {
            raw = &( ((Entry0D *)mapentry)->value );
         } else {
            raw = ((Entry1D *)mapentry)->value;
         }

      } else if( raw_type == AST__SINTTYPE ){
         raw_size = sizeof( short int );
         if( nel == 0 ) {
            raw = &( ((Entry0S *)mapentry)->value );
         } else {
            raw = ((Entry1S *)mapentry)->value;
         }

      } else if( raw_type == AST__BYTETYPE ){
         raw_size = sizeof( unsigned char );
         if( nel == 0 ) {
            raw = &( ((Entry0B *)mapentry)->value );
         } else {
            raw = ((Entry1B *)mapentry)->value;
         }

      } else if( raw_type == AST__FLOATTYPE ){
         raw_size = sizeof( float );
         if( nel == 0 ) {
            raw = &( ((Entry0F *)mapentry)->value );
         } else {
            raw = ((Entry1F *)mapentry)->value;
         }

      } else if( raw_type == AST__STRINGTYPE ){
         raw_size = sizeof( const char * );
         if( nel == 0 ) {
            raw = &( ((Entry0C *)mapentry)->value );
         } else {
            raw = ((Entry1C *)mapentry)->value;
         }

      } else if( raw_type == AST__OBJECTTYPE ){
         raw_size = sizeof( AstObject * );
         if( nel == 0 ) {
            raw = &( ((Entry0A *)mapentry)->value );
         } else {
            raw = ((Entry1A *)mapentry)->value;
         }

      } else if( raw_type == AST__UNDEFTYPE ){
         raw_size = 0;
         raw = NULL;

      } else {
         raw_size = 0;
         raw = NULL;
         astError( AST__INTER, "astMapGet1C(KeyMap): Illegal map entry data "
                   "type %d encountered (internal AST programming error).", status,
                   raw_type );
      }

/* Treat scalars as single-value vectors. */
      if( nel == 0 ) nel = 1;

/* Ensure no more than "mxval" values are returned. */
      if( nel > mxval ) nel = mxval;

/* Return the number of values stored in the buffer. */
      *nval = nel;

/* Loop round all values in the vector. */
      val = value;
      for( i = 0; i < nel && astOK; i++ ) {

/* Convert the value, storing the result in the supplied buffer. Report an
   error if conversion is not possible. */
         if( !raw ) {
            result = 0;

         } else if( !ConvertValue( raw, raw_type, &cvalue, AST__STRINGTYPE, status ) && astOK ){
            astError( AST__MPGER, "astMapGet1C(%s): The value of "
                      "element %d of KeyMap key \"%s\" cannot be read using "
                      "the requested data type.", status,astGetClass( this ), i + 1, key );

/* If succesful, copy the string into the supplied buffer, or as much of
   it as will fit. Leave room for a trailing null character. */
         } else {
            strncpy( val, cvalue, l - 1 );
            val[ l - 1 ] = 0;
         }

/* Increment the pointers to the next raw value and the next buffer
   location. */
         raw = (char *) raw + raw_size;
         val += l;
      }

/* If the KeyError attribute is non-zero, report an error if the key is not
   found */
   } else if( astGetKeyError( this ) && astOK ) {
      astError( AST__MPKER, "astMapGet1C(%s): No value was found for "
                "%s in the supplied KeyMap.", status, astGetClass( this ),
                key );
   }

/* If an error occurred,return zero. */
   if( !astOK ) result = 0;

/* Return the result.*/
   return result;
}

int astMapGet1AId_( AstKeyMap *this, const char *skey, int mxval, int *nval,
                    AstObject **value, int *status ) {
/*
*  Name:
*     astMapGet1AId_

*  Purpose:
*     Get a vector of AstObject pointers from a KeyMap.

*  Type:
*     Private function.

*  Synopsis:
*     #include "ast.h"
*     int astMapGet1A( AstKeyMap *this, const char *key, int mxval, int *nval,
*                      AstObject **value )

*  Class Membership:
*     KeyMap method.

*  Description:
*     This is the public implementation of the astMapGet1A function
*     It is identical to astMapGet1A_ except that ID values are returned
*     via the "value" parameter instead of a true C pointers. This is required
*     because this conversion cannot be performed by the macro that invokes
*     the function.

*  Parameters:
*     (see astMapGet1<X>)

*/

/* Local Variables: */
   AstMapEntry *mapentry;  /* Pointer to parent MapEntry structure */
   AstObject *avalue;      /* Pointer to AstObject */
   char keybuf[ AST__MXKEYLEN + 1 ]; /* Buffer for upper cas key */ \
   const char *key;        /* Pointer to key string to use */ \
   int i;                  /* Element index */
   int itab;               /* Index of hash table element to use */
   int nel;                /* Number of elements in raw vector */
   int raw_type;           /* Data type of stored value */
   int result;             /* Returned flag */
   size_t raw_size;        /* Size of a single raw value */
   unsigned long hash;     /* Full width hash value */
   void *raw;              /* Pointer to stored value */

/* Initialise */
   result = 0;
   *nval = 0;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Convert the supplied key to upper case if required. */
   key = ConvertKey( this, skey, keybuf, AST__MXKEYLEN + 1, "astMapGet1A",
                     status );

/* Use the hash function to determine the element of the hash table in
   which the key will be stored. */
   itab = HashFun( key, this->mapsize - 1, &hash, status );

/* Search the relevent table entry for the required MapEntry. */
   mapentry = SearchTableEntry( this, itab, key, status );

/* Skip rest if the key was not found. */
   if( mapentry ) {
      result = 1;

/* Get the address of the first raw value, and its data type. Also get
   the size of each element of the vector. */
      nel = mapentry->nel;
      raw_type = mapentry->type;
      if( raw_type == AST__INTTYPE ){
         raw_size = sizeof( int );
         if( nel == 0 ) {
            raw = &( ((Entry0I *)mapentry)->value );
         } else {
            raw = ((Entry1I *)mapentry)->value;
         }

      } else if( raw_type == AST__DOUBLETYPE ){
         raw_size = sizeof( double );
         if( nel == 0 ) {
            raw = &( ((Entry0D *)mapentry)->value );
         } else {
            raw = ((Entry1D *)mapentry)->value;
         }

      } else if( raw_type == AST__SINTTYPE ){
         raw_size = sizeof( short int );
         if( nel == 0 ) {
            raw = &( ((Entry0S *)mapentry)->value );
         } else {
            raw = ((Entry1S *)mapentry)->value;
         }

      } else if( raw_type == AST__BYTETYPE ){
         raw_size = sizeof( unsigned char );
         if( nel == 0 ) {
            raw = &( ((Entry0B *)mapentry)->value );
         } else {
            raw = ((Entry1B *)mapentry)->value;
         }

      } else if( raw_type == AST__POINTERTYPE ){
         raw_size = sizeof( void * );
         if( nel == 0 ) {
            raw = &( ((Entry0P *)mapentry)->value );
         } else {
            raw = ((Entry1P *)mapentry)->value;
         }

      } else if( raw_type == AST__FLOATTYPE ){
         raw_size = sizeof( float );
         if( nel == 0 ) {
            raw = &( ((Entry0F *)mapentry)->value );
         } else {
            raw = ((Entry1F *)mapentry)->value;
         }

      } else if( raw_type == AST__STRINGTYPE ){
         raw_size = sizeof( const char * );
         if( nel == 0 ) {
            raw = &( ((Entry0C *)mapentry)->value );
         } else {
            raw = ((Entry1C *)mapentry)->value;
         }

      } else if( raw_type == AST__OBJECTTYPE ){
         raw_size = sizeof( AstObject * );
         if( nel == 0 ) {
            raw = &( ((Entry0A *)mapentry)->value );
         } else {
            raw = ((Entry1A *)mapentry)->value;
         }

      } else if( raw_type == AST__UNDEFTYPE ){
         raw_size = 0;
         raw = NULL;

      } else {
         raw_size = 0;
         raw = NULL;
         astError( AST__INTER, "astMapGet1A(KeyMap): Illegal map entry data "
                   "type %d encountered (internal AST programming error).",
                   status, raw_type );
      }

/* Treat scalars as single-value vectors. */
      if( nel == 0 ) nel = 1;

/* Ensure no more than "mxval" values are returned. */
      if( nel > mxval ) nel = mxval;

/* Return the number of values stored in the buffer. */
      *nval = nel;

/* Loop round all values in the vector. */
      for( i = 0; i < nel && astOK; i++ ) {

/* Convert the value, storing the result in the supplied buffer. Report an
   error if conversion is not possible. */
         if( !raw ) {
            result = 0;

         } else if( !ConvertValue( raw, raw_type, &avalue, AST__OBJECTTYPE, status ) && astOK ){
            astError( AST__MPGER, "astMapGet1A(%s): The value of "
                      "element %d of KeyMap key \"%s\" cannot be read using "
                      "the requested data type.", status, astGetClass( this ),
                      i + 1, key );

/* If succesful, return an ID value for the Object. */
         } else {
           value[ i ] = avalue ? astMakeId( avalue ) : NULL;
         }

/* Increment the pointers to the next raw value. */
         raw = (char *) raw + raw_size;
      }

/* If the KeyError attribute is non-zero, report an error if the key is not
   found */
   } else if( astGetKeyError( this ) && astOK ) {
      astError( AST__MPKER, "astMapGet1A(%s): No value was found for "
                "%s in the supplied KeyMap.", status, astGetClass( this ),
                key );
   }

/* If an error occurred,return zero. */
   if( !astOK ) result = 0;

/* Return the result.*/
   return result;
}

/*
*++
*  Name:
c     astMapGetElem<X>
f     AST_MAPGETELEM<X>

*  Purpose:
*     Get a single element of a vector value from a KeyMap.

*  Type:
*     Public virtual function.

*  Synopsis:
c     #include "ast.h"
c     int astMapGetElem<X>( AstKeyMap *this, const char *key, int elem,
c                           <X>type *value )
c     int astMapGetElemC( AstKeyMap *this, const char *key, int l, int elem,
c                         char *value )
f     RESULT = AST_MAPGETELEM<X>( THIS, KEY, ELEM, VALUE, STATUS )

*  Class Membership:
*     KeyMap method.

*  Description:
*     This is a set of functions for retrieving a single element of a vector
*     value from a KeyMap. You should replace <X> in the generic function name
c     astMapGetElem<X>
f     AST_MAPGETELEM<X>
*     by an appropriate 1-character type code (see the "Data Type Codes"
*     section below for the code appropriate to each supported data type).
*     The stored value is converted to the data type indiced by <X>
*     before being returned (an error is reported if it is not possible to
*     convert the stored value to the requested data type).
c     Note, the astMapGetElemC function has an extra parameter "l" which
c     specifies the maximum length of the string to be stored in the
c     "value" buffer (see the "astMapGetElemC" section below).

*  Parameters:
c     this
f     THIS = INTEGER (Given)
*        Pointer to the KeyMap.
c     key
f     KEY = CHARACTER * ( * ) (Given)
*        The character string identifying the value to be retrieved. Trailing
*        spaces are ignored.
*        The supplied string is converted to upper case before use if the
*        KeyCase attribute is currently set to zero.
c     elem
f     ELEM = INTEGER (Given)
*        The index of the required vector element, starting at
c        zero.
f        one.
*        An error will be reported if the value is outside the range of
*        the vector.
c     value
f     VALUE = <X>type (Returned)
c        A pointer to a buffer in which to return the requested value.
f        The requested value.
*        If the requested key is not found, or if it is found but has an
*        undefined value (see
c        astMapPutU),
f        AST_MAPPUTU),
*        then the contents of the
*        buffer on entry to this function will be unchanged on exit.
f     STATUS = INTEGER (Given and Returned)
f        The global status.

*  Returned Value:
c     astMapGetElem<X>()
f     AST_MAPGETELEM<X> = LOGICAL
c        A non-zero value
f        .TRUE.
*        is returned if the requested key name was found, and does not have
*        an undefined value (see
c        astMapPutU). Zero
f        AST_MAPPUTU). .FALSE.
*        is returned otherwise.

*  Notes:
*     - No error is reported if the requested key cannot be found in the
*     given KeyMap, or if it has an undefined value, but a
c     zero
f     .FALSE.
*     value will be returned as the function value.

c  astMapGetElemC:
c     The "value" buffer supplied to the astMapGetElemC function should be a
c     pointer to a character array with "l" elements, where "l" is the
c     maximum length of the string to be returned. The value of "l"
c     should be supplied as an extra parameter following "key" when
c     invoking astMapGetElemC, and should include space for a terminating
c     null character.

*  Data Type Codes:
*     To select the appropriate
c     function, you should replace <X> in the generic function name
c     astMapGetElem<X>
f     routine, you should replace <X> in the generic routine name
f     AST_MAPGETELEM<X>
*     with a 1-character data type code, so as to match the data type <X>type
*     of the data you are processing, as follows:
c     - D: double
c     - F: float
c     - I: int
c     - C: "const" pointer to null terminated character string
c     - A: Pointer to AstObject
c     - P: Generic "void *" pointer
c     - S: short int
c     - B: Unsigned byte (i.e. char)
f     - D: DOUBLE PRECISION
f     - R: REAL
f     - I: INTEGER
f     - C: CHARACTER
f     - A: INTEGER used to identify an AstObject
f     - S: INTEGER*2 (short integer)
f     - B: Unsigned byte
*
c     For example, astMapGetElemD would be used to get a "double" value, while
c     astMapGetElemI would be used to get an "int" value, etc. For D or I, the
c     supplied "value" parameter should be a pointer to a double or int. For
c     C, the supplied "value" parameter should be a pointer to a character
c     string with "l" elements. For A, the supplied "value" parameter should
c     be a pointer to an AstObject pointer.
f     For example, AST_MAPGETELEMD would be used to get a DOUBLE PRECISION
f     value, while AST_MAPGETELEMI would be used to get an INTEGER value, etc.

*--
*/
/* Define a macro to implement the function for a specific data type
(excluding "C" since that needs an extra parameter). */
#define MAKE_MAPGETELEM(X,Xtype,Itype) \
static int MapGetElem##X( AstKeyMap *this, const char *skey, int elem, \
                          Xtype *value, int *status ) { \
\
/* Local Variables: */ \
   AstMapEntry *mapentry;  /* Pointer to parent MapEntry structure */ \
   const char *key;        /* Pointer to key string to use */ \
   char keybuf[ AST__MXKEYLEN + 1 ]; /* Buffer for upper cas key */ \
   int itab;               /* Index of hash table element to use */ \
   int nel;                /* Number of elements in raw vector */ \
   int raw_type;           /* Data type of stored value */ \
   int result;             /* Returned flag */ \
   size_t raw_size;        /* Size of a single raw value */ \
   unsigned long hash;     /* Full width hash value */ \
   void *raw;              /* Pointer to stored value */ \
\
/* Initialise */ \
   result = 0; \
\
/* Check the global error status. */ \
   if ( !astOK ) return result; \
\
/* Convert the supplied key to upper case if required. */ \
   key = ConvertKey( this, skey, keybuf, AST__MXKEYLEN + 1, "astMapGetElem" #X, \
                     status ); \
\
/* Use the hash function to determine the element of the hash table in \
   which the key will be stored. */ \
   itab = HashFun( key, this->mapsize - 1, &hash, status ); \
\
/* Search the relevent table entry for the required MapEntry. */ \
   mapentry = SearchTableEntry( this, itab, key, status ); \
\
/* Skip rest if the key was not found. */ \
   if( mapentry ) { \
      result = 1; \
\
/* Get the address of the first raw value, and its data type. Also get \
   the size of each element of the vector. */ \
      nel = mapentry->nel; \
      raw_type = mapentry->type; \
      if( raw_type == AST__INTTYPE ){ \
         raw_size = sizeof( int ); \
         if( nel == 0 ) { \
            raw = &( ((Entry0I *)mapentry)->value ); \
         } else { \
            raw = ((Entry1I *)mapentry)->value; \
         } \
\
      } else if( raw_type == AST__DOUBLETYPE ){ \
         raw_size = sizeof( double ); \
         if( nel == 0 ) { \
            raw = &( ((Entry0D *)mapentry)->value ); \
         } else { \
            raw = ((Entry1D *)mapentry)->value; \
         } \
\
      } else if( raw_type == AST__SINTTYPE ){ \
         raw_size = sizeof( short int ); \
         if( nel == 0 ) { \
            raw = &( ((Entry0S *)mapentry)->value ); \
         } else { \
            raw = ((Entry1S *)mapentry)->value; \
         } \
\
      } else if( raw_type == AST__BYTETYPE ){ \
         raw_size = sizeof( unsigned char ); \
         if( nel == 0 ) { \
            raw = &( ((Entry0B *)mapentry)->value ); \
         } else { \
            raw = ((Entry1B *)mapentry)->value; \
         } \
\
      } else if( raw_type == AST__POINTERTYPE ){ \
         raw_size = sizeof( void * ); \
         if( nel == 0 ) { \
            raw = &( ((Entry0P *)mapentry)->value ); \
         } else { \
            raw = ((Entry1P *)mapentry)->value; \
         } \
\
      } else if( raw_type == AST__FLOATTYPE ){ \
         raw_size = sizeof( float ); \
         if( nel == 0 ) { \
            raw = &( ((Entry0F *)mapentry)->value ); \
         } else { \
            raw = ((Entry1F *)mapentry)->value; \
         } \
\
      } else if( raw_type == AST__STRINGTYPE ){ \
         raw_size = sizeof( const char * ); \
         if( nel == 0 ) { \
            raw = &( ((Entry0C *)mapentry)->value ); \
         } else { \
            raw = ((Entry1C *)mapentry)->value; \
         } \
\
      } else if( raw_type == AST__OBJECTTYPE ){ \
         raw_size = sizeof( AstObject * ); \
         if( nel == 0 ) { \
            raw = &( ((Entry0A *)mapentry)->value ); \
         } else { \
            raw = ((Entry1A *)mapentry)->value; \
         } \
\
      } else if( raw_type == AST__UNDEFTYPE ){ \
         raw = NULL; \
\
      } else { \
         raw_size = 0; \
         raw = NULL; \
         astError( AST__INTER, "astMapGetElem<X>(KeyMap): Illegal map entry " \
                   "data type %d encountered (internal AST programming " \
                   "error).", status, raw_type ); \
      } \
\
/* Treat scalars as single-value vectors. */ \
      if( nel == 0 ) nel = 1; \
\
/* Ensure the requested element is within the bounds of the vector */ \
      if( elem >= nel || elem < 0 ) { \
         if( astOK ) { \
            astError( AST__MPVIN, "astMapGetElem<X>(KeyMap): Illegal " \
                      "zero-based vector index %d supplied for KeyMap " \
                      "entry '%s' - the vector has %d elements.", status, \
                      elem, key, nel ); \
         } \
\
/* Get a pointer to the requested raw value. */ \
      } else if( raw ) { \
         raw = (char *) raw + elem*raw_size; \
\
/* Convert the requested value, storing the result in the supplied buffer. \
   Report an error if conversion is not possible. */ \
         if( !ConvertValue( raw, raw_type, value, Itype, status ) && astOK ){ \
            astError( AST__MPGER, "astMapGetElem" #X "(%s): The value of " \
                      "element %d of KeyMap key \"%s\" cannot be read using " \
                      "the requested data type.", status, astGetClass( this ), \
                      elem + 1, key ); \
         } \
      } \
\
/* If the KeyError attribute is non-zero, report an error if the key is not \
   found */ \
   } else if( astGetKeyError( this ) && astOK ) { \
      astError( AST__MPKER, "astMapGetElem" #X "(%s): No value was found for " \
                "%s in the supplied KeyMap.", status, astGetClass( this ), \
                key ); \
   } \
\
/* If an error occurred,return zero. */ \
   if( !astOK ) result = 0; \
\
/* Return the result.*/ \
   return result; \
}

/* Expand the above macro to generate a function for each required
   data type (except C which is done differently). */
MAKE_MAPGETELEM(I,int,AST__INTTYPE)
MAKE_MAPGETELEM(D,double,AST__DOUBLETYPE)
MAKE_MAPGETELEM(F,float,AST__FLOATTYPE)
MAKE_MAPGETELEM(A,AstObject *,AST__OBJECTTYPE)
MAKE_MAPGETELEM(P,void *,AST__POINTERTYPE)
MAKE_MAPGETELEM(S,short int,AST__SINTTYPE)
MAKE_MAPGETELEM(B,unsigned char,AST__BYTETYPE)

/* Undefine the macro. */
#undef MAKE_MAPGETELEM


static int MapGetElemC( AstKeyMap *this, const char *skey, int l, int elem,
                        char *value, int *status ) {
/*
*  Name:
*     MapGetElemC

*  Purpose:
*     Get a single element of a vector value from a KeyMap.

*  Type:
*     Private member function.

*  Synopsis:
*     #include "ast.h"
*     int MapGetElemC( AstKeyMap *this, const char *key, int l, int elem,
*                      char *value, int *status )

*  Class Membership:
*     KeyMap method.

*  Description:
*     This is the implementation of astMapGetElem<X> for <X> = "C". We
*     cannot use the MAKE_MAPGETELEM macro for this because the string
*     version of this function has an extra parameter giving the maximum
*     length of each string which can be stored in the supplied buffer.

*  Parameters:
*     (see astMapGetElem<X>)
*/

/* Local Variables: */
   AstMapEntry *mapentry;  /* Pointer to parent MapEntry structure */
   const char *key;        /* Pointer to key string to use */
   char keybuf[ AST__MXKEYLEN + 1 ]; /* Buffer for upper cas key */
   const char *cvalue;     /* Pointer to converted string */
   int itab;               /* Index of hash table element to use */
   int nel;                /* Number of elements in raw vector */
   int raw_type;           /* Data type of stored value */
   int result;             /* Returned flag */
   size_t raw_size;        /* Size of a single raw value */
   unsigned long hash;     /* Full width hash value */
   void *raw;              /* Pointer to stored value */

/* Initialise */
   result = 0;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Convert the supplied key to upper case if required. */
   key = ConvertKey( this, skey, keybuf, AST__MXKEYLEN + 1, "astMapGetElemC",
                     status );

/* Use the hash function to determine the element of the hash table in
   which the key will be stored. */
   itab = HashFun( key, this->mapsize - 1, &hash, status );

/* Search the relevent table entry for the required MapEntry. */
   mapentry = SearchTableEntry( this, itab, key, status );

/* Skip rest if the key was not found. */
   if( mapentry ) {
      result = 1;

/* Get the address of the first raw value, and its data type. Also get
   the size of each element of the vector. */
      nel = mapentry->nel;
      raw_type = mapentry->type;
      if( raw_type == AST__INTTYPE ){
         raw_size = sizeof( int );
         if( nel == 0 ) {
            raw = &( ((Entry0I *)mapentry)->value );
         } else {
            raw = ((Entry1I *)mapentry)->value;
         }

      } else if( raw_type == AST__POINTERTYPE ){
         raw_size = sizeof( void * );
         if( nel == 0 ) {
            raw = &( ((Entry0P *)mapentry)->value );
         } else {
            raw = ((Entry1P *)mapentry)->value;
         }

      } else if( raw_type == AST__DOUBLETYPE ){
         raw_size = sizeof( double );
         if( nel == 0 ) {
            raw = &( ((Entry0D *)mapentry)->value );
         } else {
            raw = ((Entry1D *)mapentry)->value;
         }

      } else if( raw_type == AST__SINTTYPE ){
         raw_size = sizeof( short int );
         if( nel == 0 ) {
            raw = &( ((Entry0S *)mapentry)->value );
         } else {
            raw = ((Entry1S *)mapentry)->value;
         }

      } else if( raw_type == AST__BYTETYPE ){
         raw_size = sizeof( unsigned char );
         if( nel == 0 ) {
            raw = &( ((Entry0B *)mapentry)->value );
         } else {
            raw = ((Entry1B *)mapentry)->value;
         }

      } else if( raw_type == AST__FLOATTYPE ){
         raw_size = sizeof( float );
         if( nel == 0 ) {
            raw = &( ((Entry0F *)mapentry)->value );
         } else {
            raw = ((Entry1F *)mapentry)->value;
         }

      } else if( raw_type == AST__STRINGTYPE ){
         raw_size = sizeof( const char * );
         if( nel == 0 ) {
            raw = &( ((Entry0C *)mapentry)->value );
         } else {
            raw = ((Entry1C *)mapentry)->value;
         }

      } else if( raw_type == AST__OBJECTTYPE ){
         raw_size = sizeof( AstObject * );
         if( nel == 0 ) {
            raw = &( ((Entry0A *)mapentry)->value );
         } else {
            raw = ((Entry1A *)mapentry)->value;
         }

      } else if( raw_type == AST__UNDEFTYPE ){
         raw = NULL;

      } else {
         raw_size = 0;
         raw = NULL;
         astError( AST__INTER, "astMapGetElemC(KeyMap): Illegal map entry data "
                   "type %d encountered (internal AST programming error).", status,
                   raw_type );
      }

/* Treat scalars as single-value vectors. */
      if( nel == 0 ) nel = 1;

/* Ensure the requested element is within the bounds of the vector */
      if( elem >= nel || elem < 0 ) {
         if( astOK ) {
            astError( AST__MPVIN, "astMapGetElemC(KeyMap): Illegal vector "
                      "index %d supplied for KeyMap entry '%s' - should be "
                      "in the range 1 to %d.", status, elem + 1, key, nel + 1 );
         }

/* Get a pointer to the requested raw value. */
      } else if( raw ){
         raw = (char *) raw + elem*raw_size;

/* Convert the value, storing the result in the supplied buffer. Report an
   error if conversion is not possible. */
         if( !ConvertValue( raw, raw_type, &cvalue, AST__STRINGTYPE, status ) && astOK ){
            astError( AST__MPGER, "astMapGetElemC(%s): The value of "
                      "element %d of KeyMap key \"%s\" cannot be read using "
                      "the requested data type.", status,astGetClass( this ),
                      elem + 1, key );

/* If succesful, copy the string into the supplied buffer, or as much of
   it as will fit. Leave room for a trailing null character. */
         } else {
            strncpy( value, cvalue, l - 1 );
            value[ l - 1 ] = 0;
         }
      }

/* If the KeyError attribute is non-zero, report an error if the key is not
   found */
   } else if( astGetKeyError( this ) && astOK ) {
      astError( AST__MPKER, "astMapGetElemC(%s): No value was found for "
                "%s in the supplied KeyMap.", status, astGetClass( this ),
                key );
   }

/* If an error occurred,return zero. */
   if( !astOK ) result = 0;

/* Return the result.*/
   return result;
}

int astMapGetElemAId_( AstKeyMap *this, const char *skey, int elem,
                       AstObject **value, int *status ) {
/*
*  Name:
*     astMapGetElemAId_

*  Purpose:
*     Get a single element of a vector of AstObject pointers from a KeyMap.

*  Type:
*     Private function.

*  Synopsis:
*     #include "ast.h"
*     int astMapGetElemA( AstKeyMap *this, const char *key, int elem,
*                         AstObject **value )

*  Class Membership:
*     KeyMap method.

*  Description:
*     This is the public implementation of the astMapGetElemA function
*     It is identical to astMapGetElemA_ except that an ID value is returned
*     via the "value" parameter instead of a true C pointer. This is required
*     because this conversion cannot be performed by the macro that invokes
*     the function.

*  Parameters:
*     (see astMapGet1<X>)

*/

/* Local Variables: */
   AstMapEntry *mapentry;  /* Pointer to parent MapEntry structure */
   AstObject *avalue;      /* Pointer to AstObject */
   const char *key;        /* Pointer to key string to use */
   char keybuf[ AST__MXKEYLEN + 1 ]; /* Buffer for upper cas key */
   int itab;               /* Index of hash table element to use */
   int nel;                /* Number of elements in raw vector */
   int raw_type;           /* Data type of stored value */
   int result;             /* Returned flag */
   size_t raw_size;        /* Size of a single raw value */
   unsigned long hash;     /* Full width hash value */
   void *raw;              /* Pointer to stored value */

/* Initialise */
   result = 0;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Convert the supplied key to upper case if required. */
   key = ConvertKey( this, skey, keybuf, AST__MXKEYLEN + 1, "astMapGetElemA",
                     status );

/* Use the hash function to determine the element of the hash table in
   which the key will be stored. */
   itab = HashFun( key, this->mapsize - 1, &hash, status );

/* Search the relevent table entry for the required MapEntry. */
   mapentry = SearchTableEntry( this, itab, key, status );

/* Skip rest if the key was not found. */
   if( mapentry ) {
      result = 1;

/* Get the address of the first raw value, and its data type. Also get
   the size of each element of the vector. */
      nel = mapentry->nel;
      raw_type = mapentry->type;
      if( raw_type == AST__INTTYPE ){
         raw_size = sizeof( int );
         if( nel == 0 ) {
            raw = &( ((Entry0I *)mapentry)->value );
         } else {
            raw = ((Entry1I *)mapentry)->value;
         }

      } else if( raw_type == AST__SINTTYPE ){
         raw_size = sizeof( short int );
         if( nel == 0 ) {
            raw = &( ((Entry0S *)mapentry)->value );
         } else {
            raw = ((Entry1S *)mapentry)->value;
         }

      } else if( raw_type == AST__BYTETYPE ){
         raw_size = sizeof( unsigned char );
         if( nel == 0 ) {
            raw = &( ((Entry0B *)mapentry)->value );
         } else {
            raw = ((Entry1B *)mapentry)->value;
         }

      } else if( raw_type == AST__DOUBLETYPE ){
         raw_size = sizeof( double );
         if( nel == 0 ) {
            raw = &( ((Entry0D *)mapentry)->value );
         } else {
            raw = ((Entry1D *)mapentry)->value;
         }

      } else if( raw_type == AST__POINTERTYPE ){
         raw_size = sizeof( void * );
         if( nel == 0 ) {
            raw = &( ((Entry0P *)mapentry)->value );
         } else {
            raw = ((Entry1P *)mapentry)->value;
         }

      } else if( raw_type == AST__FLOATTYPE ){
         raw_size = sizeof( float );
         if( nel == 0 ) {
            raw = &( ((Entry0F *)mapentry)->value );
         } else {
            raw = ((Entry1F *)mapentry)->value;
         }

      } else if( raw_type == AST__STRINGTYPE ){
         raw_size = sizeof( const char * );
         if( nel == 0 ) {
            raw = &( ((Entry0C *)mapentry)->value );
         } else {
            raw = ((Entry1C *)mapentry)->value;
         }

      } else if( raw_type == AST__OBJECTTYPE ){
         raw_size = sizeof( AstObject * );
         if( nel == 0 ) {
            raw = &( ((Entry0A *)mapentry)->value );
         } else {
            raw = ((Entry1A *)mapentry)->value;
         }

      } else if( raw_type == AST__UNDEFTYPE ){
         raw = NULL;

      } else {
         raw_size = 0;
         raw = NULL;
         astError( AST__INTER, "astMapGetElemA(KeyMap): Illegal map entry data "
                   "type %d encountered (internal AST programming error).", status,
                   raw_type );
      }

/* Treat scalars as single-value vectors. */
      if( nel == 0 ) nel = 1;

/* Ensure the requested element is within the bounds of the vector */
      if( elem >= nel || elem < 0 ) {
         if( astOK ) {
            astError( AST__MPVIN, "astMapGetElemA(KeyMap): Illegal vector "
                      "index %d supplied for KeyMap entry '%s' - should be "
                      "in the range 1 to %d.", status, elem + 1, key, nel + 1 );
         }

/* Get a pointer to the requested raw value. */
      } else if( raw ){
         raw = (char *) raw + elem*raw_size;

/* Convert the value, storing the result in the supplied buffer. Report an
   error if conversion is not possible. */
         if( !ConvertValue( raw, raw_type, &avalue, AST__OBJECTTYPE, status ) && astOK ){
            astError( AST__MPGER, "astMapGetElemA(%s): The value of "
                      "element %d of KeyMap key \"%s\" cannot be read using "
                      "the requested data type.", status,astGetClass( this ),
                      elem + 1, key );

/* If succesful, return an ID value for the Object. */
         } else {
            *value = avalue ? astMakeId( avalue ) : NULL;
         }
      }

/* If the KeyError attribute is non-zero, report an error if the key is not
   found */
   } else if( astGetKeyError( this ) && astOK ) {
      astError( AST__MPKER, "astMapGetElemA(%s): No value was found for "
                "%s in the supplied KeyMap.", status, astGetClass( this ),
                key );
   }

/* If an error occurred,return zero. */
   if( !astOK ) result = 0;

/* Return the result.*/
   return result;
}

static int MapDefined( AstKeyMap *this, const char *skey, int *status ) {
/*
*++
*  Name:
c     astMapDefined<X>
f     AST_MAPDEFINED<X>

*  Purpose:
*     Check if a KeyMap contains a defined value for a key.

*  Type:
*     Public virtual function.

*  Synopsis:
c     #include "ast.h"
c     int astMapDefined( AstKeyMap *this, const char *key );
f     RESULT = AST_MAPDEFINED( THIS, KEY, STATUS )

*  Class Membership:
*     KeyMap method.

*  Description:
*     This function checks to see if a KeyMap contains a defined value for
*     a given key. If the key is present in the KeyMap but has an
*     undefined value it returns
c     zero (unlike astMapHasKey which would return non-zero).
f     .FALSE. (unlike AST_MAPHASKEY which would return .TRUE.).

*  Parameters:
c     this
f     THIS = INTEGER (Given)
*        Pointer to the KeyMap.
c     key
f     KEY = CHARACTER * ( * ) (Given)
*        The character string identifying the value to be retrieved. Trailing
*        spaces are ignored. The supplied string is converted to upper
*        case before use if the KeyCase attribute is currently set to zero.
f     STATUS = INTEGER (Given and Returned)
f        The global status.

*  Returned Value:
c     astMapDefined()
f     AST_MAPDEFINED = LOGICAL
c        A non-zero value
f        .TRUE.
*        is returned if the requested key name is present in the KeyMap
*        and has a defined value.

*--
*/

/* Local Variables: */
   AstMapEntry *mapentry;  /* Pointer to parent MapEntry structure */
   const char *key;        /* Pointer to key string to use */
   char keybuf[ AST__MXKEYLEN + 1 ]; /* Buffer for upper cas key */
   int itab;               /* Index of hash table element to use */
   int result;             /* Returned flag */
   unsigned long hash;     /* Full width hash value */

/* Initialise */
   result = 0;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Convert the supplied key to upper case if required. */
   key = ConvertKey( this, skey, keybuf, AST__MXKEYLEN + 1, "astMapDefined",
                     status );

/* Use the hash function to determine the element of the hash table in
   which the key will be stored. */
   itab = HashFun( key, this->mapsize - 1, &hash, status );

/* Search the relevent table entry for the required MapEntry. */
   mapentry = SearchTableEntry( this, itab, key, status );

/* Skip rest if the key was not found. */
   if( mapentry ) {

/* Set the result depending on the entry data type. */
      if( mapentry->type == AST__UNDEFTYPE ){
         result = 0;
      } else {
         result = 1;
      }

/* If the KeyError attribute is non-zero, report an error if the key is not
   found */
   } else if( astGetKeyError( this ) && astOK ) {
      astError( AST__MPKER, "astMapDefined(%s): No value was found for "
                "%s in the supplied KeyMap.", status, astGetClass( this ),
                key );
   }

/* If an error occurred, return zero. */
   if( !astOK ) result = 0;

/* Return the result.*/
   return result;
}

static int MapHasKey( AstKeyMap *this, const char *skey, int *status ) {
/*
*++
*  Name:
c     astMapHasKey
f     AST_MAPHASKEY

*  Purpose:
*     Check if an entry with a given key exists in a KeyMap.

*  Type:
*     Public virtual function.

*  Synopsis:
c     #include "keymap.h"
c     int astMapHasKey( AstKeyMap *this, const char *key )
f     RESULT = AST_MAPHASKEY( THIS, KEY, STATUS )

*  Class Membership:
*     KeyMap method.

*  Description:
*     This function returns a flag indicating if the KeyMap contains an
*     entry with the given key.

*  Parameters:
c     this
f     THIS = INTEGER (Given)
*        Pointer to the KeyMap.
c     key
f     KEY = CHARACTER * ( * ) (Given)
*        The character string identifying the KeyMap entry. Trailing spaces are
*        ignored.
*        The supplied string is converted to upper case before use if the
*        KeyCase attribute is currently set to zero.
f     STATUS = INTEGER (Given and Returned)
f        The global status.

*  Returned Value:
c     astMapHasKey()
f     AST_MAPHASKEY = LOGICAL
c        Non-zero if the key was found, and zero otherwise.
f        .TRUE. if the key was found, and .FALSE. otherwise.

*  Notes:
c     - A non-zero function value
f     - .TRUE.
*     is returned if the key exists but has an undefined value (that is,
*     the returned value does not depend on whether the entry has a
*     defined value or not). See also
c     astMapDefined, which returns zero in such a case.
f     AST_MAPDEFINED, which returns zero in such a case.
*     - A function value of
c     zero
f     .FALSE.
*     will be returned if an error has already occurred, or if this
*     function should fail for any reason.

*--
*/

/* Local Variables: */
   AstMapEntry *mapentry;  /* Pointer to entry in linked list */
   const char *key;        /* Pointer to key string to use */
   char keybuf[ AST__MXKEYLEN + 1 ]; /* Buffer for upper cas key */
   int itab;               /* Index of hash table element to use */
   int result;             /* Returned value */
   unsigned long hash;     /* Full width hash value */

/* Initialise */
   result = 0;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Convert the supplied key to upper case if required. */
   key = ConvertKey( this, skey, keybuf, AST__MXKEYLEN + 1, "astMapHasKey",
                     status );

/* Use the hash function to determine the element of the hash table in
   which the key will be stored. */
   itab = HashFun( key, this->mapsize - 1, &hash, status );

/* Search the relevent table entry for the required MapEntry. */
   mapentry = SearchTableEntry( this, itab, key, status );

/* Set a non-zero return value if the key was found. */
   if( mapentry ) result = 1;

/* If an error has occurred, return zero. */
   if( !astOK ) result = 0;

/* Return the result. */
   return result;

}

static void MapRemove( AstKeyMap *this, const char *skey, int *status ) {
/*
*++
*  Name:
c     astMapRemove
f     AST_MAPREMOVE

*  Purpose:
*     Removed a named entry from a KeyMap.

*  Type:
*     Public virtual function.

*  Synopsis:
c     #include "keymap.h"
c     void astMapRemove( AstKeyMap *this, const char *key )
f     CALL AST_MAPREMOVE( THIS, KEY, STATUS )

*  Class Membership:
*     KeyMap method.

*  Description:
c     This function
f     This routine
*     removes a named entry from a KeyMap. It returns without action if the
*     KeyMap does not contain the specified key.

*  Parameters:
c     this
f     THIS = INTEGER (Given)
*        Pointer to the KeyMap.
c     key
f     KEY = CHARACTER * ( * ) (Given)
*        The character string identifying the value to be retrieved. Trailing
*        spaces are ignored.
*        The supplied string is converted to upper case before use if the
*        KeyCase attribute is currently set to zero.
f     STATUS = INTEGER (Given and Returned)
f        The global status.
*--
*/

/* Local Variables: */
   const char *key;        /* Pointer to key string to use */
   char keybuf[ AST__MXKEYLEN + 1 ]; /* Buffer for upper cas key */
   int itab;               /* Index of hash table element to use */
   unsigned long hash;     /* Full width hash value */

/* Check the global error status. */
   if ( !astOK ) return;

/* Convert the supplied key to upper case if required. */
   key = ConvertKey( this, skey, keybuf, AST__MXKEYLEN + 1, "astMapRemove",
                     status );

/* Use the hash function to determine the element of the hash table in
   which the key will be stored. */
   itab = HashFun( key, this->mapsize - 1, &hash, status );

/* Search the relevent table entry for the required MapEntry and remove it. */
   (void) FreeMapEntry( RemoveTableEntry( this, itab, key, status ), status );
}

static void MapRename( AstKeyMap *this, const char *soldkey, const char *snewkey,
                       int *status ) {
/*
*++
*  Name:
c     astMapRename
f     AST_MAPRENAME

*  Purpose:
*     Rename an existing KeyMap entry.

*  Type:
*     Public virtual function.

*  Synopsis:
c     #include "keymap.h"
c     void astMapRename( AstKeyMap *this, const char *oldkey, const char *newkey )
f     CALL AST_MAPRENAME( THIS, OLDKEY, NEWKEY, STATUS )

*  Class Membership:
*     KeyMap method.

*  Description:
c     This function
f     This routine
*     associated a new key with an existing entry in a KeyMap. It returns
*     without action if the oldkey does not exist in the KeyMap.

*  Parameters:
c     this
f     THIS = INTEGER (Given)
*        Pointer to the KeyMap.
c     oldkey
f     OLDKEY = CHARACTER * ( * ) (Given)
*        The character string identifying the entry to be renamed. Trailing
*        spaces are ignored.
*        The supplied string is converted to upper case before use if the
*        KeyCase attribute is currently set to zero.
c     newkey
f     NEKEY = CHARACTER * ( * ) (Given)
*        The new character string to associated with the renamed entry.
*        Trailing spaces are ignored.
*        The supplied string is converted to upper case before use if the
*        KeyCase attribute is currently set to zero.
f     STATUS = INTEGER (Given and Returned)
f        The global status.
*--
*/

/* Local Variables: */
   AstMapEntry *entry;     /* Pointer to the entry being renamed */
   AstMapEntry *oldent;    /* Pointer to old entry with new name */
   const char *oldkey;     /* Pointer to key string to use */
   char oldkeybuf[ AST__MXKEYLEN + 1 ]; /* Buffer for upper cas key */
   const char *newkey;     /* Pointer to key string to use */
   char newkeybuf[ AST__MXKEYLEN + 1 ]; /* Buffer for upper cas key */
   char *p;                /* Pointer to next key character */
   int itab;               /* Index of hash table element to use */
   int keylen;             /* Length of supplied key string */
   int keymember;          /* Identifier for new key */
   int there;              /* Did the entry already exist in the KeyMap? */
   unsigned long hash;     /* Full width hash value */

/* Check the global error status. */
   if ( !astOK ) return;

/* Convert the supplied keys to upper case if required. */
   oldkey = ConvertKey( this, soldkey, oldkeybuf, AST__MXKEYLEN + 1,
                        "astMapRename", status );
   newkey = ConvertKey( this, snewkey, newkeybuf, AST__MXKEYLEN + 1,
                        "astMapRename", status );

/* Do nothing if the keys are the same. */
   if( strcmp( oldkey, newkey ) ){

/* Use the hash function to determine the element of the hash table in
   which the old key will be stored. */
      itab = HashFun( oldkey, this->mapsize - 1, &hash, status );

/* Search the relevent table entry for the required MapEntry. Remove it
   from the list, but do not free it. */
      entry = RemoveTableEntry( this, itab, oldkey, status );

/* Skip rest if the key was not found. */
      if( entry ) {

/* Store the new key string, and terminate it to exclude any trailing
   spaces. */
         keylen = strlen( newkey );
         entry->key = astStore( (void *) entry->key, newkey, keylen + 1 );
         if( astOK ) {
            p = (char *) entry->key + keylen;
            while( --p >= entry->key ) {
               if( *p == ' ' ) {
                  *p = 0;
               } else {
                  break;
               }
            }
         }

/* Use the hash function to determine the element of the hash table in
   which to store the entry with its new key. */
         itab = HashFun( entry->key, this->mapsize - 1, &(entry->hash), status );

/* Remove and free any existing entry with the given key from the table
   element. */
         oldent = RemoveTableEntry( this, itab, entry->key, status );
         if( oldent ) {
            keymember = oldent->keymember;
            oldent = FreeMapEntry( oldent, status );
            there = 1;
         } else {
            keymember = -1;
            there = 0;
         }

/* If the KeyMap is locked we report an error if an attempt is made to
   introduce a new key. */
         if( !there && astGetMapLocked( this ) ) {
            astError( AST__BADKEY, "astMapRename(%s): Failed to rename item "
                      "\"%s\" in a KeyMap to \"%s\": \"%s\" is not a known "
                      "item.", status, astGetClass( this ), oldkey, newkey,
                      newkey );
         }

/* If all has gone OK, store the renamed entry at the head of the linked list
   associated with the selected table entry. */
         if( astOK ) {
            entry = AddTableEntry( this, itab, entry, keymember, status );

/* If anything went wrong, try to delete the renamed entry. */
         } else {
            entry = FreeMapEntry( entry, status );
         }
      }
   }
}

static int MapSize( AstKeyMap *this, int *status ) {
/*
*++
*  Name:
c     astMapSize
f     AST_MAPSIZE

*  Purpose:
*     Get the number of entries in a KeyMap.

*  Type:
*     Public virtual function.

*  Synopsis:
c     #include "keymap.h"
c     int astMapSize( AstKeyMap *this )
f     RESULT = AST_MAPSIZE( THIS, STATUS )

*  Class Membership:
*     KeyMap method.

*  Description:
*     This function returns the number of entries in a KeyMap.

*  Parameters:
c     this
f     THIS = INTEGER (Given)
*        Pointer to the KeyMap.
f     STATUS = INTEGER (Given and Returned)
f        The global status.

*  Returned Value:
c     astMapSize()
f     AST_MAPSIZE = INTEGER
*        The number of entries in the KeyMap.

*  Notes:
*     - A function value of zero will be returned if an error has already
*     occurred, or if this function should fail for any reason.

*--
*/

/* Local Variables: */
   int itab;               /* Index of hash table element to use */
   int result;             /* Returned value */

/* Initialise */
   result = 0;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Add up the number of entries in all elements of the hash table. */
   for( itab = 0; itab < this->mapsize; itab++ ) result += this->nentry[ itab ];

/* Return the result. */
   return result;

}

static int MapLenC( AstKeyMap *this, const char *skey, int *status ) {
/*
*++
*  Name:
c     astMapLenC
f     AST_MAPLENC

*  Purpose:
*     Get the number of characters in a character entry in a KeyMap.

*  Type:
*     Public virtual function.

*  Synopsis:
c     #include "keymap.h"
c     int astMapLenC( AstKeyMap *this, const char *key )
f     RESULT = AST_MAPLENC( THIS, KEY, STATUS )

*  Class Membership:
*     KeyMap method.

*  Description:
*     This function returns the minimum length which a character variable
*     which must have in order to be able to store a specified entry in
*     the supplied KeyMap. If the named entry is a vector entry, then the
*     returned value is the length of the longest element of the vector
*     value.

*  Parameters:
c     this
f     THIS = INTEGER (Given)
*        Pointer to the KeyMap.
c     key
f     KEY = CHARACTER * ( * ) (Given)
*        The character string identifying the KeyMap entry. Trailing
*        spaces are ignored.
*        The supplied string is converted to upper case before use if the
*        KeyCase attribute is currently set to zero.
f     STATUS = INTEGER (Given and Returned)
f        The global status.

*  Returned Value:
c     astMapLenC()
f     AST_MAPLENC = INTEGER
*        The length (i.e. number of characters) of the longest formatted
*        value associated with the named entry.
c        This does not include the trailing null character.

*  Notes:
*     - A function value of zero will be returned without error if the
*     named entry cannot be formatted as a character string.
*     - A function value of zero will be returned if an error has already
*     occurred, or if this function should fail for any reason.

*--
*/

/* Local Variables: */
   AstMapEntry *mapentry;  /* Pointer to parent MapEntry structure */
   const char *key;        /* Pointer to key string to use */
   char keybuf[ AST__MXKEYLEN + 1 ]; /* Buffer for upper cas key */
   int i;                  /* Element index */
   int itab;               /* Index of hash table element to use */
   int l;                  /* Length of formatted vector element */
   int nel;                /* Number of elements in raw vector */
   int raw_type;           /* Data type of stored value */
   int result;             /* Returned value */
   size_t raw_size;        /* Size of a single raw value */
   unsigned long hash;     /* Full width hash value */
   void *raw;              /* Pointer to stored value */

/* Initialise */
   result = 0;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Convert the supplied key to upper case if required. */
   key = ConvertKey( this, skey, keybuf, AST__MXKEYLEN + 1, "astMapLenC",
                     status );

/* Use the hash function to determine the element of the hash table in
   which the key will be stored. */
   itab = HashFun( key, this->mapsize - 1, &hash, status );

/* Search the relevent table entry for the required MapEntry. */
   mapentry = SearchTableEntry( this, itab, key, status );

/* Skip rest if the key was not found. */
   if( mapentry ) {

/* Get the address of the first raw value, and its data type. Also get
   the size of each element of the vector. */
      nel = mapentry->nel;
      raw_type = mapentry->type;
      if( raw_type == AST__INTTYPE ){
         raw_size = sizeof( int );
         if( nel == 0 ) {
            raw = &( ((Entry0I *)mapentry)->value );
         } else {
            raw = ((Entry1I *)mapentry)->value;
         }

      } else if( raw_type == AST__POINTERTYPE ){
         raw_size = sizeof( void * );
         if( nel == 0 ) {
            raw = &( ((Entry0P *)mapentry)->value );
         } else {
            raw = ((Entry1P *)mapentry)->value;
         }

      } else if( raw_type == AST__DOUBLETYPE ){
         raw_size = sizeof( double );
         if( nel == 0 ) {
            raw = &( ((Entry0D *)mapentry)->value );
         } else {
            raw = ((Entry1D *)mapentry)->value;
         }

      } else if( raw_type == AST__SINTTYPE ){
         raw_size = sizeof( short int );
         if( nel == 0 ) {
            raw = &( ((Entry0S *)mapentry)->value );
         } else {
            raw = ((Entry1S *)mapentry)->value;
         }

      } else if( raw_type == AST__BYTETYPE ){
         raw_size = sizeof( unsigned char );
         if( nel == 0 ) {
            raw = &( ((Entry0B *)mapentry)->value );
         } else {
            raw = ((Entry1B *)mapentry)->value;
         }

      } else if( raw_type == AST__FLOATTYPE ){
         raw_size = sizeof( float );
         if( nel == 0 ) {
            raw = &( ((Entry0F *)mapentry)->value );
         } else {
            raw = ((Entry1F *)mapentry)->value;
         }

      } else if( raw_type == AST__STRINGTYPE ){
         raw_size = sizeof( const char * );
         if( nel == 0 ) {
            raw = &( ((Entry0C *)mapentry)->value );
         } else {
            raw = ((Entry1C *)mapentry)->value;
         }

      } else if( raw_type == AST__OBJECTTYPE ){
         raw_size = sizeof( AstObject * );
         if( nel == 0 ) {
            raw = &( ((Entry0A *)mapentry)->value );
         } else {
            raw = ((Entry1A *)mapentry)->value;
         }

      } else if( raw_type == AST__UNDEFTYPE ){
         raw_size = 0;
         raw = NULL;

      } else {
         raw_size = 0;
         raw = NULL;
         astError( AST__INTER, "astMapLenC(KeyMap): Illegal map entry data "
                   "type %d encountered (internal AST programming error).", status,
                   raw_type );
      }

/* Treat scalars as single-value vectors. */
      if( nel == 0 ) nel = 1;

/* Skip undefined values. */
      if( raw ) {

/* Initialise the maximum length of any formatted value in the entry. */
         result= 0;

/* Loop round all values in the vector. */
         for( i = 0; i < nel && astOK; i++ ) {

/* Go through the motions of formatting the value. We do not actually
   need the formatted string (just its length) so we provide a NULL pointer
   for the output buffer. The entry is ignored if it cannot be formatted.
   Note, the length returned by ConvertValue includes the terminating null,
   so decrement it first. */
            l = ConvertValue( raw, raw_type, NULL, AST__STRINGTYPE, status );
            if( --l > result ) result = l;

/* Increment the pointer to the next raw value. */
            raw = (char *) raw + raw_size;
         }
      }
   }

/* If an error has occurred, return zero. */
   if( !astOK ) result = 0;

/* Return the result. */
   return result;

}

static int MapLength( AstKeyMap *this, const char *skey, int *status ) {
/*
*++
*  Name:
c     astMapLength
f     AST_MAPLENGTH

*  Purpose:
*     Get the vector length of an entry in a KeyMap.

*  Type:
*     Public virtual function.

*  Synopsis:
c     #include "keymap.h"
c     int astMapLength( AstKeyMap *this, const char *key )
f     RESULT = AST_MAPLENGTH( THIS, KEY, STATUS )

*  Class Membership:
*     KeyMap method.

*  Description:
*     This function returns the vector length of a named entry in a KeyMap,
*     (that is, how many values are associated with the entry).

*  Parameters:
c     this
f     THIS = INTEGER (Given)
*        Pointer to the KeyMap.
c     key
f     KEY = CHARACTER * ( * ) (Given)
*        The character string identifying the KeyMap entry. Trailing
*        spaces are ignored.
*        The supplied string is converted to upper case before use if the
*        KeyCase attribute is currently set to zero.
f     STATUS = INTEGER (Given and Returned)
f        The global status.

*  Returned Value:
c     astMapLength()
f     AST_MAPLENGTH = INTEGER
*        The length of the entry. One for a scalar, greater than one for
*        a vector. A value of zero is returned if the KeyMap does not
*        contain the named entry.

*  Notes:
*     - A function value of zero will be returned if an error has already
*     occurred, or if this function should fail for any reason.

*--
*/

/* Local Variables: */
   AstMapEntry *mapentry;  /* Pointer to entry in linked list */
   const char *key;        /* Pointer to key string to use */
   char keybuf[ AST__MXKEYLEN + 1 ]; /* Buffer for upper cas key */
   int itab;               /* Index of hash table element to use */
   int result;             /* Returned value */
   unsigned long hash;     /* Full width hash value */

/* Initialise */
   result = 0;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Convert the supplied key to upper case if required. */
   key = ConvertKey( this, skey, keybuf, AST__MXKEYLEN + 1, "astMapLength",
                     status );

/* Use the hash function to determine the element of the hash table in
   which the key will be stored. */
   itab = HashFun( key, this->mapsize - 1, &hash, status );

/* Search the relevent table entry for the required MapEntry. */
   mapentry = SearchTableEntry( this, itab, key, status );

/* Skip rest if the key was not found. */
   if( mapentry ) {

/* Store the netry length */
      result = mapentry->nel;

/* Return 1 for a scalar. */
      if( result == 0 ) result = 1;

   }

/* If an error has occurred, return zero. */
   if( !astOK ) result = 0;

/* Return the result. */
   return result;

}

/*
*++
*  Name:
c     astMapPutElem<X>
f     AST_MAPPUTELEM<X>

*  Purpose:
*     Put a value into an element of a vector value in a KeyMap.

*  Type:
*     Public virtual function.

*  Synopsis:
c     #include "ast.h"
c     void astMapPutElem<X>( AstKeyMap *this, const char *key, int elem,
c                            <X>type *value )
f     CALL AST_MAPPUTELEM<X>( THIS, KEY, ELEM, VALUE, STATUS )

*  Class Membership:
*     KeyMap method.

*  Description:
*     This is a set of functions for storing a value in a single element of
*     a vector value in a KeyMap. You should replace <X> in the generic
*     function name
c     astMapPutElem<X>
f     AST_MAPPUTELEM<X>
*     by an appropriate 1-character type code (see the "Data Type Codes"
*     section below for the code appropriate to each supported data type).
*     The supplied value is converted from the data type indicated by <X>
*     to the data type of the KeyMap entry before being stored (an error
*     is reported if it is not possible to convert the value to the
*     required data type).

*  Parameters:
c     this
f     THIS = INTEGER (Given)
*        Pointer to the KeyMap.
c     key
f     KEY = CHARACTER * ( * ) (Given)
*        The character string identifying the value to be retrieved. Trailing
*        spaces are ignored.
*        The supplied string is converted to upper case before use if the
*        KeyCase attribute is currently set to zero.
c     elem
f     ELEM = INTEGER (Given)
*        The index of the vector element to modify, starting at
c        zero.
f        one.
c     value
f     VALUE = <X>type (Given)
*        The value to store.
f     STATUS = INTEGER (Given and Returned)
f        The global status.

*  Applicability:
*     KeyMap
*        If the
c        "elem"
f        ELEM
*        index is outside the range of the vector, the length of
*        the vector will be increased by one element and the supplied
*        value will be stored at the end of the vector in the new element.
*     Table
*        If the
c        "elem"
f        ELEM
*        index is outside the range of the vector, an error will be
*        reported. The number of elements in each cell of a column is
*        specified when the column is created using
c        astAddColumn.
f        AST_ADDCOLUMN.

*  Notes:
*     - If the entry originally holds a scalar value, it will be treated
*     like a vector entry of length 1.
*     - If the specified key cannot be found in the given KeyMap, or is
*     found but has an undefined value, a new
*     vector entry with the given name, and data type implied by <X>, is
*     created and the supplied value is stored in its first entry.

*  Data Type Codes:
*     To select the appropriate
c     function, you should replace <X> in the generic function name
c     astMapPutElem<X>
f     routine, you should replace <X> in the generic routine name
f     AST_MAPPUTELEM<X>
*     with a 1-character data type code, so as to match the data type <X>type
*     of the data you are processing, as follows:
c     - D: double
c     - F: float
c     - I: int
c     - C: "const" pointer to null terminated character string
c     - A: Pointer to AstObject
c     - P: Generic "void *" pointer
c     - S: short int
c     - B: Unsigned byte (i.e. char)
f     - D: DOUBLE PRECISION
f     - R: REAL
f     - I: INTEGER
f     - C: CHARACTER
f     - A: INTEGER used to identify an AstObject
f     - S: INTEGER*2 (short integer)
f     - B: BYTE (unsigned)
*
c     For example, astMapPutElemD would be used to put a "double" value, while
c     astMapPutElemI would be used to put an "int" value, etc. For D or I, the
c     supplied "value" parameter should be a double or int. For
c     C, the supplied "value" parameter should be a pointer to a character
c     string. For A, the supplied "value" parameter should be an AstObject
c     pointer.
f     For example, AST_MAPPUTELEMD would be used to put a DOUBLE PRECISION
f     value, while AST_MAPPUTELEMI would be used to put an INTEGER value, etc.

*--
*/
/* Define a macro to implement the function for a specific data type
(excluding "C" since that needs an extra parameter). */
#define MAKE_MAPPUTELEM(X,Xtype,Itype) \
static void MapPutElem##X( AstKeyMap *this, const char *skey, int elem, \
                           Xtype value, int *status ) { \
\
/* Local Variables: */ \
   AstMapEntry *mapentry;  /* Pointer to parent MapEntry structure */ \
   const char *key;        /* Pointer to key string to use */ \
   char keybuf[ AST__MXKEYLEN + 1 ]; /* Buffer for upper cas key */ \
   int itab;               /* Index of hash table element to use */ \
   int nel;                /* Number of elements in raw vector */ \
   int new;                /* Was a new uninitialised element created? */ \
   int raw_type;           /* Data type of stored value */ \
   size_t raw_size;        /* Size of a single raw value */ \
   unsigned long hash;     /* Full width hash value */ \
   void *raw;              /* Pointer to stored value */ \
\
/* Check the global error status. */ \
   if ( !astOK ) return; \
\
/* Perform any necessary checks on the supplied value to be stored. */ \
   CHECK_##X \
\
/* Convert the supplied key to upper case if required. */ \
   key = ConvertKey( this, skey, keybuf, AST__MXKEYLEN + 1, "astMapPutElem" #X, \
                     status ); \
\
/* Use the hash function to determine the element of the hash table in \
   which the key will be stored. */ \
   itab = HashFun( key, this->mapsize - 1, &hash, status ); \
\
/* Search the relevent table entry for the required MapEntry. */ \
   mapentry = SearchTableEntry( this, itab, key, status ); \
\
/* If the key was not found, or was found but has an undefined value, create \
   a new one with a single element, \
   and store the supplied value in it. */ \
   if( !mapentry || mapentry->type == AST__UNDEFTYPE ) { \
      astMapPut1##X( this, key, 1, &value, NULL ); \
\
/* If the key was found.... */ \
   } else { \
\
/* Get the current length of the vector (0=>scalar), and the data type. */ \
      nel = mapentry->nel; \
      raw_type = mapentry->type; \
\
/* Do each data type in turn. */ \
      if( raw_type == AST__INTTYPE ){ \
\
/* If the existing entry is scalar, create a new vector entry with the \
   same name, value, data type and comment. Then get a pointer to the new \
   entry, and indicate that we now have a vector entry of length 1. */ \
         if( nel == 0 ) { \
            astMapPut1I( this, key, 1, &( ((Entry0I *)mapentry)->value ), \
                         mapentry->comment ); \
            mapentry = SearchTableEntry( this, itab, key, status ); \
            nel = 1; \
         } \
\
/* Get the address of the first raw value in the vector. Also get \
   the size of each element of the vector. */ \
         raw = ((Entry1I *)mapentry)->value; \
         raw_size = sizeof( int ); \
\
/* Handle other data type in the same way. */ \
      } else if( raw_type == AST__SINTTYPE ){ \
         if( nel == 0 ) { \
            astMapPut1S( this, key, 1, &( ((Entry0S *)mapentry)->value ), \
                         mapentry->comment ); \
            mapentry = SearchTableEntry( this, itab, key, status ); \
            nel = 1; \
         } \
         raw = ((Entry1S *)mapentry)->value; \
         raw_size = sizeof( short int ); \
\
      } else if( raw_type == AST__BYTETYPE ){ \
         if( nel == 0 ) { \
            astMapPut1B( this, key, 1, &( ((Entry0B *)mapentry)->value ), \
                         mapentry->comment ); \
            mapentry = SearchTableEntry( this, itab, key, status ); \
            nel = 1; \
         } \
         raw = ((Entry1B *)mapentry)->value; \
         raw_size = sizeof( unsigned char ); \
\
      } else if( raw_type == AST__DOUBLETYPE ){ \
         if( nel == 0 ) { \
            astMapPut1D( this, key, 1, &( ((Entry0D *)mapentry)->value ), \
                         mapentry->comment ); \
            mapentry = SearchTableEntry( this, itab, key, status ); \
            nel = 1; \
         } \
         raw = ((Entry1D *)mapentry)->value; \
         raw_size = sizeof( double ); \
\
      } else if( raw_type == AST__POINTERTYPE ){ \
         if( nel == 0 ) { \
            astMapPut1P( this, key, 1, &( ((Entry0P *)mapentry)->value ), \
                         mapentry->comment ); \
            mapentry = SearchTableEntry( this, itab, key, status ); \
            nel = 1; \
         } \
         raw = ((Entry1P *)mapentry)->value; \
         raw_size = sizeof( void * ); \
\
      } else if( raw_type == AST__FLOATTYPE ){ \
         if( nel == 0 ) { \
            astMapPut1F( this, key, 1, &( ((Entry0F *)mapentry)->value ), \
                         mapentry->comment ); \
            mapentry = SearchTableEntry( this, itab, key, status ); \
            nel = 1; \
         } \
         raw = ((Entry1F *)mapentry)->value; \
         raw_size = sizeof( float ); \
\
      } else if( raw_type == AST__STRINGTYPE ){ \
         if( nel == 0 ) { \
            astMapPut1C( this, key, 1, &( ((Entry0C *)mapentry)->value ), \
                         mapentry->comment ); \
            mapentry = SearchTableEntry( this, itab, key, status ); \
            nel = 1; \
         } \
         raw = ((Entry1C *)mapentry)->value; \
         raw_size = sizeof( const char * ); \
\
      } else if( raw_type == AST__OBJECTTYPE ){ \
         if( nel == 0 ) { \
            astMapPut1A( this, key, 1, &( ((Entry0A *)mapentry)->value ), \
                         mapentry->comment ); \
            mapentry = SearchTableEntry( this, itab, key, status ); \
            nel = 1; \
         } \
         raw = ((Entry1A *)mapentry)->value; \
         raw_size = sizeof( AstObject * ); \
\
      } else { \
         raw_size = 0; \
         raw = NULL; \
         astError( AST__INTER, "astMapPutElem<X>(KeyMap): Illegal map entry " \
                   "data type %d encountered (internal AST programming " \
                   "error).", status, raw_type ); \
      } \
\
/* If the requested element is outside the bounds of the vector, extend \
   the vector by one element. */ \
      new = ( elem >= nel || elem < 0 ); \
      if( new ) { \
         elem = nel++; \
         raw = astGrow( raw, nel, raw_size ); \
         if( astOK ) { \
            mapentry->nel = nel; \
            if( raw_type == AST__INTTYPE ){ \
               ((Entry1I *)mapentry)->value = (int *) raw; \
            } else if( raw_type == AST__SINTTYPE ){ \
               ((Entry1S *)mapentry)->value = (short int *) raw; \
            } else if( raw_type == AST__BYTETYPE ){ \
               ((Entry1B *)mapentry)->value = (unsigned char *) raw; \
            } else if( raw_type == AST__DOUBLETYPE ){ \
               ((Entry1D *)mapentry)->value = (double *) raw; \
            } else if( raw_type == AST__POINTERTYPE ){ \
               ((Entry1P *)mapentry)->value = (void *) raw; \
            } else if( raw_type == AST__FLOATTYPE ){ \
               ((Entry1F *)mapentry)->value = (float *) raw; \
            } else if( raw_type == AST__STRINGTYPE ){ \
               ((Entry1C *)mapentry)->value = (const char **) raw; \
            } else if( raw_type == AST__OBJECTTYPE ){ \
               ((Entry1A *)mapentry)->value = (AstObject **) raw; \
            } \
         } \
      } \
\
/* Get a pointer to the requested element. */ \
      if( astOK ) { \
         raw = (char *) raw + elem*raw_size; \
\
/* Free any memory used by the value already in the requested element. */ \
         if( ! new ) { \
            if( raw_type == AST__STRINGTYPE ){ \
               char **cp = (char **) raw; \
               *cp = astFree( *cp ); \
            } else if( raw_type == AST__OBJECTTYPE ){ \
               AstObject **op = (AstObject **) raw; \
               if( *op ) *op = astAnnul( *op ); \
            } \
         } \
\
/* Convert the supplied value, storing the result in the requested element. \
   Report an error if conversion is not possible. */ \
         if( !ConvertValue( &value, Itype, raw, raw_type, status ) && astOK ){ \
            astError( AST__MPPER, "astMapPutElem" #X "(%s): The supplied " \
                      "value cannot be converted to the data type of " \
                      "KeyMap key \"%s\".", status, astGetClass( this ), \
                      key ); \
         } \
      } \
   } \
}

/* Define macros which perform any necessary checks on the supplied value
   to be stored. For Object entries, check that we are not adding a KeyMap
   which already contains "this". This avoids circular dependencies.
   Other types do not need any checks. */
#define CHECK_A CheckCircle( this, value, "astMapPutElemA", status );
#define CHECK_I
#define CHECK_B
#define CHECK_S
#define CHECK_D
#define CHECK_F
#define CHECK_C
#define CHECK_P

/* Expand the above macro to generate a function for each required
   data type. */
MAKE_MAPPUTELEM(I,int,AST__INTTYPE)
MAKE_MAPPUTELEM(D,double,AST__DOUBLETYPE)
MAKE_MAPPUTELEM(F,float,AST__FLOATTYPE)
MAKE_MAPPUTELEM(A,AstObject *,AST__OBJECTTYPE)
MAKE_MAPPUTELEM(P,void *,AST__POINTERTYPE)
MAKE_MAPPUTELEM(C,const char *,AST__STRINGTYPE)
MAKE_MAPPUTELEM(S,short int,AST__SINTTYPE)
MAKE_MAPPUTELEM(B,unsigned char,AST__BYTETYPE)

/* Undefine the macro. */
#undef MAKE_MAPPUTELEM
#undef CHECK_A
#undef CHECK_I
#undef CHECK_B
#undef CHECK_S
#undef CHECK_D
#undef CHECK_F
#undef CHECK_C
#undef CHECK_P


static int MapType( AstKeyMap *this, const char *skey, int *status ) {
/*
*++
*  Name:
c     astMapType
f     AST_MAPTYPE

*  Purpose:
*     Get the data type of an entry in a KeyMap.

*  Type:
*     Public virtual function.

*  Synopsis:
c     #include "keymap.h"
c     int astMapType( AstKeyMap *this, const char *key )
f     RESULT = AST_MAPTYPE( THIS, KEY, STATUS )

*  Class Membership:
*     KeyMap method.

*  Description:
*     This function returns a value indicating the data type of a
*     named entry in a KeyMap. This is the data type which was used when the
*     entry was added to the KeyMap.

*  Parameters:
c     this
f     THIS = INTEGER (Given)
*        Pointer to the KeyMap.
c     key
f     KEY = CHARACTER * ( * ) (Given)
*        The character string identifying the KeyMap entry. Trailing
*        spaces are ignored.
*        The supplied string is converted to upper case before use if the
*        KeyCase attribute is currently set to zero.
f     STATUS = INTEGER (Given and Returned)
f        The global status.

*  Returned Value:
c     astMapType()
f     AST_MAPTYPE = INTEGER
*        One of AST__INTTYPE (for integer), AST__SINTTYPE (for
c        short int),
f        INTEGER*2),
*        AST__BYTETYPE (for unsigned bytes
c        - i.e. unsigned chars
*        ) AST__DOUBLETYPE (for double
*        precision floating point), AST__FLOATTYPE (for single
*        precision floating point), AST__STRINGTYPE (for character string),
*        AST__OBJECTTYPE (for AST Object pointer), AST__POINTERTYPE (for
*        arbitrary C pointer) or AST__UNDEFTYPE (for undefined values
*        created by
c        astMapPutU).
f        AST_MAPPUTU).
*        AST__BADTYPE is returned if the supplied key is not found in the KeyMap.

*  Notes:
*     - A function value of AST__BADTYPE will be returned if an error has
*     already occurred, or if this function should fail for any reason.

*--
*/

/* Local Variables: */
   AstMapEntry *mapentry;  /* Pointer to entry in linked list */
   const char *key;        /* Pointer to key string to use */
   char keybuf[ AST__MXKEYLEN + 1 ]; /* Buffer for upper cas key */
   int itab;               /* Index of hash table element to use */
   int result;             /* Returned value */
   unsigned long hash;     /* Full width hash value */

/* Initialise */
   result = AST__BADTYPE;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Convert the supplied key to upper case if required. */
   key = ConvertKey( this, skey, keybuf, AST__MXKEYLEN + 1, "astMapType",
                     status );

/* Use the hash function to determine the element of the hash table in
   which the key will be stored. */
   itab = HashFun( key, this->mapsize - 1, &hash, status );

/* Search the relevent table entry for the required MapEntry. */
   mapentry = SearchTableEntry( this, itab, key, status );

/* Store the type if found. */
   if( mapentry ) result = mapentry->type;

/* If an error has occurred, return zero. */
   if( !astOK ) result = AST__BADTYPE;

/* Return the result. */
   return result;

}

static const char *MapIterate( AstKeyMap *this, int reset, int *status ) {
/*
*+
*  Name:
*     astMapIterate

*  Purpose:
*     Iterate through the keys in a KeyMap.

*  Type:
*     Protected virtual function.

*  Synopsis:
*     #include "keymap.h"
*     const char *astMapIterate( AstKeyMap *this, int reset, int *status )

*  Class Membership:
*     KeyMap method.

*  Description:
*     If "reset" is non-zero, this function returns a pointer to a string
*     holding the first key in the KeyMap. On subsequent invocation (if
*     reset is zero) it returns a pointer to the next key in the KeyMap. The
*     context is stored within the KeyMap structure, so calls on different
*     KeyMaps can be mixed.
*
*     The order in which keys are returned is determined by the KeyMap
*     SortBy attribute.

*  Parameters:
*     this
*        Pointer to the KeyMap.
*     reset
*        If non-zero, return the first key in the KeyMap. Otherwise,
*        returns the key following the one returned by the previous
*        invocation of this function.

*  Returned Value:
*        A pointer to the null-terminated string holding the next key,
*        or NULL if there are no more keys in the KeyMap. The returned
*        string should NOT be freed or modified.

*  Notes:
*     - A NULL pointer will be returned if this function is invoked
*     with the AST error status set, or if it should fail for any
*     reason.
*/

/* Local Variables: */
   AstMapEntry *entry;         /* Pointer to the entry */
   const char *key;            /* Pointer value to return */
   int itab;                   /* Index into hash table */
   int sortby;                 /* The value of the SortBy attribute */

/* Initialise. */
   key = NULL;

/* Check the global error status. */
   if ( !astOK ) return key;

/* Get the SortBy value. */
   sortby = astGetSortBy( this );

/* First deal with unsorted keys. */
   if( sortby == SORTBY_NONE ) {

/* Get the index of the hash table to check first. Also get a pointer to
   the entry within the hash table to check next. */
      if( reset ){
         itab = 0;
         entry = this->table[ 0 ];
      } else {
         itab = this->iter_itab;
         entry = this->iter_entry;
      }

/* Move through elements of the hash table until we have a non-null entry. */
      while( !entry && ++itab < this->mapsize ) {
         entry = this->table[ itab ];
      }

/* Return a pointer to the key. */
      if( entry ) {
         key = entry->key;

/* Move on to the next entry in the unsorted linked list, saving the context
   in the KeyMap structure. */
         this->iter_itab = itab;
         this->iter_entry = entry->next;
      }

/* Now deal with sorted keys. */
   } else {

/* If starting from the beginning, use the "first" entry. Otherwise, use
   the nxt entry. */
      if( reset ) {
         entry = this->first;
      } else {
         entry = this->iter_entry;
      }

/* If we have an entry, return a pointer to its key, and then update the
   context to point to the next entry in the *sorted* list. */
      if( entry ) {
         key = entry->key;
         this->iter_entry = entry->snext;
      }
   }

/* If no more entries were found, reset the context in the KeyMap
   structure. */
   if( ! key ) {
      this->iter_itab = 0;
      this->iter_entry = NULL;
   }

/* Return the result.*/
   return key;
}

static void NewTable( AstKeyMap *this, int size, int *status ){
/*
*  Name:
*     NewTable

*  Purpose:
*     Create a new hash table.

*  Type:
*     Private function.

*  Synopsis:
*     #include "keymap.h"
*     void NewTable( AstKeyMap *this, int size, int *status )

*  Class Membership:
*     KeyMap member function.

*  Description:
*     This function removes any existing hash table and allocates memory
*     for a new one of the specified size (except that the supplied size
*     is modified to be the next higher power of 2). The table is
*     initialised to indicate that it is empty.

*  Parameters:
*     this
*        Pointer to the KeyMap.
*     size
*        The reuqired size of the hash table.
*     status
*        Pointer to the inherited status variable.

*/

/* Local Variables: */
   int i;

/* Check the global error status. */
   if ( !astOK ) return;

/* Ensure the table size is at least MIN_TABLE_SIZE and is a power of 2. */
   if( size <= MIN_TABLE_SIZE ) {
      size = MIN_TABLE_SIZE;
   } else {
      size = (int) ( 0.5 + pow( 2.0, ceil( log( size )/log( 2.0 ) ) ) );
   }

/* Remove any existing entries. */
   for( i = 0; i < this->mapsize; i++ ) FreeTableEntry( this, i, status );

/* Do nothing more if the table size is not changing. */
   if( size != this->mapsize ) {

/* Modify the size of the existing table. */
      this->mapsize = size;
      this->table = astGrow( this->table, size, sizeof( AstMapEntry * ) );
      this->nentry = astGrow( this->nentry, size, sizeof( int ) );

/* Initialise the new table. */
      if( astOK ) {
         for( i = 0; i < size; i++ ) {
            this->table[ i ] = NULL;
            this->nentry[ i ] = 0;
         }
      }
   }
}

static void RemoveFromObjectList( AstKeyMap *this, AstMapEntry *entry,
                                  int *status ){
/*
*  Name:
*     RemoveFromObjectList

*  Purpose:
*     Remove an entry from the linked-list of AST__OBJECTTYPE entries.

*  Type:
*     Private function.

*  Synopsis:
*     #include "keymap.h"
*     void RemoveFromObjectList( AstKeyMap *this, AstMapEntry *entry,
*                                int *status )

*  Class Membership:
*     KeyMap member function.

*  Description:
*     This function removes the supplied MapEntry from the linked list of
*     AST__OBJECTTYPE entries.

*  Parameters:
*     this
*        Pointer to the KeyMap.
*     entry
*        Pointer to the MapEntry to be removed.
*     status
*        Pointer to the inherited status variable.

*/

/* Local Variables: */
   AstMapEntry *a;          /* Previous entry */
   AstMapEntry *b;          /* Next entry */
   Entry0A *scalar;         /* Pointer to a scalar AST__OBJECTTYPE entry */
   Entry1A *vector;         /* Pointer to a vector AST__OBJECTTYPE entry */

/* Check the global error status. */
   if ( !astOK ) return;

/* Do nothing if the entry does not hold AST Object pointers. */
   if( entry->type == AST__OBJECTTYPE ) {

/* Get pointers to the MapEntries before and after the entry being
   removed. At the same time, nullify both pointers in the entry itself. */
      if( entry->nel == 0 ) {
         scalar = (Entry0A *) entry;
         a = scalar->prev;
         b = scalar->next;
         scalar->prev = NULL;
         scalar->next = NULL;
      } else {
         vector = (Entry1A *) entry;
         a = vector->prev;
         b = vector->next;
         vector->prev = NULL;
         vector->next = NULL;
      }

/* Set the forward link in the previous entry. */
      if( a ) {
         if( a->nel == 0 ) {
            scalar = (Entry0A *) a;
            scalar->next = b;
         } else {
            vector = (Entry1A *) a;
            vector->next = b;
         }

/* If we are removing the list head, store the following entry as the new head. */
      } else {
         this->firstA = b;
      }

/* Set the backward link in the next entry. */
      if( b ) {
         if( b->nel == 0 ) {
            scalar = (Entry0A *) b;
            scalar->prev = a;
         } else {
            vector = (Entry1A *) b;
            vector->prev = a;
         }
      }
   }
}

static void RemoveFromSortedList( AstKeyMap *this, AstMapEntry *entry,
                                  int *status ){
/*
*  Name:
*     RemoveFromSortedList

*  Purpose:
*     Remove an entry from the linked-list of sorted KeyMap entries.

*  Type:
*     Private function.

*  Synopsis:
*     #include "keymap.h"
*     void RemoveFromSortedList( AstKeyMap *this, AstMapEntry *entry,
*                                int *status )

*  Class Membership:
*     KeyMap member function.

*  Description:
*     This function removes the supplied MapEntry from the linked list of
*     sorted MapEntries.

*  Parameters:
*     this
*        Pointer to the KeyMap.
*     entry
*        Pointer to the MapEntry to be removed.
*     status
*        Pointer to the inherited status variable.

*/

/* Local Variables: */
   AstMapEntry *next;       /* Next higher MapEntry */
   AstMapEntry *prev;       /* Next lower MapEntry */

/* Check the global error status. */
   if ( !astOK ) return;

/* Get pointers to the entries on either side of the entry to be removed. */
   next = entry->snext;
   prev = entry->sprev;

/* If the entry is not in the sorted list, abort. */
   if( next && prev ) {

/* Connect the previous to the next, bypassing the entry being removed. */
      next->sprev = prev;
      prev->snext = next;

/* NULLify the next and previous entries stored in the entry being
   removed. */
      entry->snext = NULL;
      entry->sprev = NULL;

/* Decrement the number of entries in the sorted list. */
      (this->nsorted)--;

/* If the entry being removed is the first entry, store a pointer to the new
   first entry. */
      if( this->nsorted == 0 ) {
         this->first = NULL;
      } else if( entry == this->first ) {
         this->first = next;
      }
   }
}

static AstMapEntry *RemoveTableEntry( AstKeyMap *this, int itab,
                                      const char *key, int *status ){
/*
*  Name:
*     RemoveTableEntry

*  Purpose:
*     Remove an entry from a linked-list of KeyMap entries.

*  Type:
*     Private function.

*  Synopsis:
*     #include "keymap.h"
*     AstMapEntry *RemoveTableEntry( AstKeyMap *this, int itab,
*                                    const char *key, int *status )

*  Class Membership:
*     KeyMap member function.

*  Description:
*     This function removes any entries with the specified key from the
*     linked-list of entries stored at the specified entry of the hash
*     table. If the supplied key is found in the list, a pointer to the
*     first removed entry is returned. Otherwise, a NULL pointer is returned.

*  Parameters:
*     this
*        Pointer to the KeyMap.
*     itab
*        Index of the hash table element to be searched.
*     key
*        The key string to be searched for. Trailing spaces are ignored.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     Pointer to the removed Entry, or NULL if no matching entry found.

*/

/* Local Variables: */
   AstMapEntry **link;    /* Address to store foward link */
   AstMapEntry *next;     /* Pointer to next Entry to copy */
   AstMapEntry *result;

/* Initialise */
   result = NULL;

/* Check the global error status. */
   if ( !astOK ) return result;

/* The "next" variable holds the address of the next MapEntry to be
   checked. Initialise this to the MapEntry at the head of the linked
   list associated with the supplied element of the hash table. */
   next = this->table[ itab ];

/* The "link" variable holds the address of the location at which the
   pointer to the MapEntry following the removed MapEntry should be stored.
   Initialise this to be the address of the hash table element. */
   link = &( this->table[ itab ] );

/* Loop round until we have checked all entries. */
   while( next && astOK ) {

/* If the key for the current entry macthes the supplied key... */
      if( !KeyCmp( next->key, key ) ) {

/* Remove the MapEntry from the list sorted by key. */
         RemoveFromSortedList( this, next, status );

/* If the entry is of type AST__OBJECTTYPE, remove it from the
   list of AST__OBJECTTYPE entries. */
         RemoveFromObjectList( this, next, status );

/* Store a pointer to the next MapEntry in the list, replacing the
   original pointer to the MapEntry which is being deleted. */
         *link = next->next;

/* Return a pointer to the first matching MapEntry. Free any subsequent
   matching MapEntries.  */
         if( result ) {
            FreeMapEntry( next, status );
         } else {
            result = next;
         }

/* Decrement the number of entries in the linked list. */
         this->nentry[ itab ]--;

/* Set up the next MapEntry to be freed. */
         next = *link;

/* If the key for the current entry does not match the supplied key... */
      } else {

/* Update the address at which to store the pointer to the next MapEntry
   in the list. */
         link = &(next->next);

/* Update the address of the next MapEntry in the list. */
         next = next->next;
      }
   }

/* Return the result */
   return result;
}

static AstMapEntry *SearchTableEntry( AstKeyMap *this, int itab, const char *key, int *status ){
/*
*  Name:
*     SearchTableEntry

*  Purpose:
*     Search an element of a has table for a given key.

*  Type:
*     Private function.

*  Synopsis:
*     #include "keymap.h"
*     AstMapEntry *SearchTableEntry( AstKeyMap *this, int itab, const char *key, int *status )

*  Class Membership:
*     KeyMap member function.

*  Description:
*     This function searches the specified element of the KeyMaps hash table
*     until an element is found which has a key matching the supplied key.
*     The address of this entry is returned. If no suitable entry is found,
*     then NULL is returned.

*  Parameters:
*     this
*        Pointer to the KeyMap.
*     itab
*        The index of the hash table to be searched.
*     key
*        The key string to be searched for. Trailing spaces are ignored.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     The address of the first MapEntry in the linked list which refers
*     to the given key, or NULL if the key is not found.

*/

/* Local Variables: */
   AstMapEntry *next;     /* Pointer to next Entry to check */
   AstMapEntry *result;   /* Returned pointer */

/* Initialise */
   result = NULL;

/* Check the global error status. */
   if ( !astOK ) return result;

/* The "next" variable holds the address of the next MapEntry to be
   checked. Initialise this to the supplied MapEntry. */
   next = this->table[ itab ];

/* Loop round until we have checked all entries. */
   while( next ) {

/* If the key for the current entry matches the supplied key, store the
   MapEntry pointer and break. */
      if( !KeyCmp( next->key, key ) ) {
         result = next;
         break;
      }

/* Update the address of the next MapEntry in the list. */
      next = next->next;

   }

/* Return the result. */
   return result;
}

static void SetAttrib( AstObject *this_object, const char *setting, int *status ) {
/*
*  Name:
*     SetAttrib

*  Purpose:
*     Set an attribute value for a KeyMap.

*  Type:
*     Private function.

*  Synopsis:
*     #include "keymap.h"
*     void SetAttrib( AstObject *this, const char *setting )

*  Class Membership:
*     KeyMap member function (over-rides the astSetAttrib protected
*     method inherited from the Mapping class).

*  Description:
*     This function assigns an attribute value for a KeyMap, the
*     attribute and its value being specified by means of a string of
*     the form:
*
*        "attribute= value "
*
*     Here, "attribute" specifies the attribute name and should be in
*     lower case with no white space present. The value to the right
*     of the "=" should be a suitable textual representation of the
*     value to be assigned and this will be interpreted according to
*     the attribute's data type.  White space surrounding the value is
*     only significant for string attributes.

*  Parameters:
*     this
*        Pointer to the KeyMap.
*     setting
*        Pointer to a null-terminated string specifying the new attribute
*        value.
*/

/* Local Variables: */
   AstKeyMap *this;             /* Pointer to the KeyMap structure */
   int ival;                    /* Attribute value */
   int len;                     /* Length of setting string */
   int nc;                      /* Number of characters read by astSscanf */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain a pointer to the KeyMap structure. */
   this = (AstKeyMap *) this_object;

/* Obtain the length of the setting string. */
   len = (int) strlen( setting );

/* Test for each recognised attribute in turn, using "astSscanf" to parse
   the setting string and extract the attribute value (or an offset to
   it in the case of string values). In each case, use the value set
   in "nc" to check that the entire string was matched. Once a value
   has been obtained, use the appropriate method to set it. */

/* SizeGuess. */
/* ---------- */
   if ( nc = 0,
        ( 1 == astSscanf( setting, "sizeguess= %d %n", &ival, &nc ) )
        && ( nc >= len ) ) {
      astSetSizeGuess( this, ival );

/* KeyCase. */
/* --------- */
   } else if ( nc = 0,
        ( 1 == astSscanf( setting, "keycase= %d %n", &ival, &nc ) )
        && ( nc >= len ) ) {
      astSetKeyCase( this, ival );

/* KeyError. */
/* --------- */
   } else if ( nc = 0,
        ( 1 == astSscanf( setting, "keyerror= %d %n", &ival, &nc ) )
        && ( nc >= len ) ) {
      astSetKeyError( this, ival );

/* MapLocked. */
/* --------- */
   } else if ( nc = 0,
        ( 1 == astSscanf( setting, "maplocked= %d %n", &ival, &nc ) )
        && ( nc >= len ) ) {
      astSetMapLocked( this, ival );

/* SortBy. */
/* ------- */
   } else if ( nc = 0,
        ( 0 == astSscanf( setting, "sortby= %n%*s %n", &ival, &nc ) )
        && ( nc >= len ) ) {
      astSetSortBy( this, SortByInt( setting + ival, "astSetAttrib", status ) );

/* If the attribute is still not recognised, pass it on to the parent
   method for further interpretation. */
   } else {
      (*parent_setattrib)( this_object, setting, status );
   }
}

static void SetKeyCase( AstKeyMap *this, int keycase, int *status ) {
/*
*+
*  Name:
*     astSetKeyCase

*  Purpose:
*     Set the value of the KeyCase attribute for a KeyMap.

*  Type:
*     Protected virtual function.

*  Synopsis:
*     #include "keymap.h"
*     void astSetKeyCase( AstKeyMap *this, int keycase )

*  Class Membership:
*     KeyMap method.

*  Description:
*     This function sets a new value for the KeyCase attribute of a
*     KeyMap. It reports an error if the KeyMap contains any entries.

*  Parameters:
*     this
*        Pointer to the KeyMap.
*     keycase
*        The new attribute value.

*-
*/

/* Local Variables: */
   int ok;                    /* Can the KeyCase value be changed? */
   int itab;                  /* Index into hash table */
   int newval;                /* New KeyCase value */

/* Check the global error status. */
   if ( !astOK ) return;

/* Normalise the new value */
   newval = keycase ? 1 : 0;

/* If the KeyCase value is to be changed, see if the KeyMap is empty. */
   ok = 1;
   if( astGetKeyCase( this ) != newval ) {
      for( itab = 0; itab < this->mapsize; itab++ ) {
         if( this->nentry[ itab ] > 0 ) {
            ok = 0;
            break;
         }
      }
   }

/* If not report an error. */
   if( !ok ) {
      astError( AST__NOWRT, "astSetAttrib(KeyMap): Illegal attempt to "
                "change the KeyCase attribute of a non-empty KeyMap." , status);

/* Otherwise, store the new value. */
   } else {
      this->keycase = newval;
   }
}

static void SetKeyError( AstKeyMap *this, int keyerror, int *status ) {
/*
*+
*  Name:
*     astSetKeyError

*  Purpose:
*     Set the value of the KeyError attribute for a KeyMap.

*  Type:
*     Protected virtual function.

*  Synopsis:
*     #include "keymap.h"
*     void astSetKeyError( AstKeyMap *this, int keyerror )

*  Class Membership:
*     KeyMap method.

*  Description:
*     This function sets the value of the KeyError attribute for a
*     KeyMap. It also sets the attribute recursively in any KeyMaps
*     contained within the supplied KeyMap.

*  Parameters:
*     this
*        Pointer to the KeyMap.
*     keyerror
*        The new value for the attribute.
*-
*/

/* Local Variables: */
   AstMapEntry *next;     /* Pointer to next Entry to copy */
   AstObject **obj_list;  /* List of pointers to AST Object entries */
   int i;                 /* Index into hash table */
   int iel;               /* Index of current vector element */
   int nel;               /* Number of elements in vector */

/* Check the global error status. */
   if ( !astOK ) return;

/* Set the KeyError value in the supplied KeyMap. */
   this->keyerror = keyerror ? 1 : 0;

/* Loop round each entry in the hash table. */
   for( i = 0; i < this->mapsize; i++ ) {

/* Get a pointer to the next KeyMap entry. */
      next = this->table[ i ];

/* Loop round all entries in this element of the hash table. */
      while( next && astOK ) {

/* If this entry has an Object data type, see if holds any KeyMaps. */
         if( next->type == AST__OBJECTTYPE ) {

/* Get the number of objects to check, and a pointer to the first. */
            nel = next->nel;
            if( nel == 0 ) {
               obj_list = &( ((Entry0A *)next)->value );
               nel = 1;
            } else {
               obj_list = ((Entry1A *)next)->value;
            }

/* Loop round checking all Objects. */
            for( iel = 0; iel < nel; iel++ ) {

/* If this Object is a KeyMap, set its KeyError attribute. */
               if( astIsAKeyMap( obj_list[ iel ] ) ) {
                  astSetKeyError( (AstKeyMap *) obj_list[ iel ], keyerror );
               }
            }
         }

/* Get a pointer to the next entry. */
         next = next->next;
      }
   }
}

static void SetMapLocked( AstKeyMap *this, int maplocked, int *status ) {
/*
*+
*  Name:
*     astSetMapLocked

*  Purpose:
*     Set the value of the MapLocked attribute for a KeyMap.

*  Type:
*     Protected virtual function.

*  Synopsis:
*     #include "keymap.h"
*     void astSetMapLocked( AstKeyMap *this, int maplocked )

*  Class Membership:
*     KeyMap method.

*  Description:
*     This function sets the value of the MapLocked attribute for a
*     KeyMap. It also sets the attribute recursively in any KeyMaps
*     contained within the supplied KeyMap.

*  Parameters:
*     this
*        Pointer to the KeyMap.
*     maplocked
*        The new value for the attribute.
*-
*/

/* Local Variables: */
   AstMapEntry *next;     /* Pointer to next Entry to copy */
   AstObject **obj_list;  /* List of pointers to AST Object entries */
   int i;                 /* Index into hash table */
   int iel;               /* Index of current vector element */
   int nel;               /* Number of elements in vector */

/* Check the global error status. */
   if ( !astOK ) return;

/* Set the MapLocked value in the supplied KeyMap. */
   this->maplocked = maplocked ? 1 : 0;

/* Loop round each entry in the hash table. */
   for( i = 0; i < this->mapsize; i++ ) {

/* Get a pointer to the next KeyMap entry. */
      next = this->table[ i ];

/* Loop round all entries in this element of the hash table. */
      while( next && astOK ) {

/* If this entry has an Object data type, see if holds any KeyMaps. */
         if( next->type == AST__OBJECTTYPE ) {

/* Get the number of objects to check, and a pointer to the first. */
            nel = next->nel;
            if( nel == 0 ) {
               obj_list = &( ((Entry0A *)next)->value );
               nel = 1;
            } else {
               obj_list = ((Entry1A *)next)->value;
            }

/* Loop round checking all Objects. */
            for( iel = 0; iel < nel; iel++ ) {

/* If this Object is a KeyMap, set its MapLocked attribute. */
               if( astIsAKeyMap( obj_list[ iel ] ) ) {
                  astSetMapLocked( (AstKeyMap *) obj_list[ iel ], maplocked );
               }
            }
         }

/* Get a pointer to the next entry. */
         next = next->next;
      }
   }
}

static void SetSizeGuess( AstKeyMap *this, int sizeguess, int *status ) {
/*
*+
*  Name:
*     astSetSizeGuess

*  Purpose:
*     Set the value of the SizeGuess attribute for a KeyMap.

*  Type:
*     Protected virtual function.

*  Synopsis:
*     #include "keymap.h"
*     void astSetSizeGuess( AstKeyMap *this, int sizeguess )

*  Class Membership:
*     KeyMap method.

*  Description:
*     This function sets a new value for the SizeGuess attribute of a
*     KeyMap. It reports an error if the KeyMap contains any entries.

*  Parameters:
*     this
*        Pointer to the KeyMap.
*     sizeguess
*        The new attribute value.

*-
*/

/* Local Variables: */
   int empty;                 /* Is the KeyMap empty? */
   int itab;                  /* Index into hash table */

/* Check the global error status. */
   if ( !astOK ) return;

/* See if the KeyMap is empty. */
   empty = 1;
   for( itab = 0; itab < this->mapsize; itab++ ) {
      if( this->nentry[ itab ] > 0 ) {
         empty = 0;
         break;
      }
   }

/* If not report an error. */
   if( !empty ) {
      astError( AST__NOWRT, "astSetAttrib(KeyMap): Illegal attempt to "
                "change the SizeGuess attribute of a non-empty KeyMap." , status);

/* Otherwise, store the new value and change the size of the hash
   table. */
   } else {
      this->sizeguess = sizeguess;
      NewTable( this, sizeguess/MAX_ENTRIES_PER_TABLE_ENTRY, status );
   }
}

static void SetSortBy( AstKeyMap *this, int sortby, int *status ) {
/*
*+
*  Name:
*     astSetSortBy

*  Purpose:
*     Set the value of the SortBy attribute for a KeyMap.

*  Type:
*     Protected virtual function.

*  Synopsis:
*     #include "keymap.h"
*     void astSetSortBy( AstKeyMap *this, int sortby )

*  Class Membership:
*     KeyMap method.

*  Description:
*     This function sets the value of the SortBy attribute for a
*     KeyMap.

*  Parameters:
*     this
*        Pointer to the KeyMap.
*     sortby
*        The new value for the attribute.
*-
*/

/* Local Variables: */
   int oldval;            /* The old sortby value */

/* Check the global error status. */
   if ( !astOK ) return;

/* Get the old SortBy value. */
   oldval = astGetSortBy( this );

/* Set the new SortBy value. */
   this->sortby = sortby;

/* If the value has changed, re-sort the keys. */
   if( oldval != sortby ) SortEntries( this, status );

}

static size_t SizeOfEntry( AstMapEntry *entry, int *status ){
/*
*  Name:
*     SizeOfEntry

*  Purpose:
*     Return the size of the supplied MapEntry structure.

*  Type:
*     Private function.

*  Synopsis:
*     #include "keymap.h"
*     size_t SizeOfEntry( AstMapEntry *entry, int *status )

*  Class Membership:
*     KeyMap member function.

*  Description:
*     This function returns the size of the supplied MapEntry structure.

*  Parameters:
*     entry
*        Pointer to the MapEntry.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     The size of the MapEntry structure. This does not include the size
*     of any data for which pointers are stored in the MapEntry structure.

*  Notes:
*     - A value of zero will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*/

/* Local Variables: */
   size_t result;         /* Returned value */
   int nel;               /* Entry length */
   int type;              /* Data type */

/* Initialise. */
   result = 0;

/* Check the global error status and the supplied pointer. */
   if ( !astOK || !entry ) return result;

/* Get the data type and length of the MapEntry. */
   type = entry->type;
   nel = entry->nel;

/* Deal with each type. */
   if( type == AST__STRINGTYPE ) {
      result = ( nel == 0 ) ? sizeof( Entry0C ) : sizeof( Entry1C );

   } else if( type == AST__OBJECTTYPE ) {
      result = ( nel == 0 ) ? sizeof( Entry0A ) : sizeof( Entry1A );

   } else if( type == AST__INTTYPE ) {
      result = ( nel == 0 ) ? sizeof( Entry0I ) : sizeof( Entry1I );

   } else if( type == AST__POINTERTYPE ) {
      result = ( nel == 0 ) ? sizeof( Entry0P ) : sizeof( Entry1P );

   } else if( type == AST__SINTTYPE ) {
      result = ( nel == 0 ) ? sizeof( Entry0S ) : sizeof( Entry1S );

   } else if( type == AST__BYTETYPE ) {
      result = ( nel == 0 ) ? sizeof( Entry0B ) : sizeof( Entry1B );

   } else if( type == AST__DOUBLETYPE ) {
      result = ( nel == 0 ) ? sizeof( Entry0D ) : sizeof( Entry1D );

   } else if( type == AST__FLOATTYPE ) {
      result = ( nel == 0 ) ? sizeof( Entry0F ) : sizeof( Entry1F );

   } else if( type == AST__UNDEFTYPE ) {
      result = sizeof( AstMapEntry );

/* Report an error if the data type is unknown. */
   } else {
      astError( AST__INTER, "SizeOfEntry(KeyMap): Illegal map entry data "
                "type %d encountered (internal AST programming error).", status,
                type );
   }

/* Return the result. */
   return result;
}

static int SortByInt( const char *sortby, const char *method, int *status ){
/*
*  Name:
*     SortByInt

*  Purpose:
*     Get the integer associated with a string SortBy value.

*  Type:
*     Private function.

*  Synopsis:
*     #include "keymap.h"
*     int SortByInt( const char *sortby, const char *method, int *status )

*  Class Membership:
*     KeyMap member function.

*  Description:
*     This function returns the integer associated with the supplied
*     string SortBy value.

*  Parameters:
*     sortby
*        Pointer to the string SortBy value (case insensitive).
*     method
*        Pointer to a string holding the name of the calling method for
*        inclusion in error messages.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     The associated SortBy integer.

*/

/* Local Variables: */
   int result;             /* The returned integer */

/* Initialise. */
   result = SORTBY_NONE;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Check each known value. */
   if( astChrMatch( sortby, "None" ) ) {
      result = SORTBY_NONE;

   } else if( astChrMatch( sortby, "AgeUp" ) ) {
      result = SORTBY_AGEUP;

   } else if( astChrMatch( sortby, "AgeDown" ) ) {
      result = SORTBY_AGEDOWN;

   } else if( astChrMatch( sortby, "KeyAgeUp" ) ) {
      result = SORTBY_KEYAGEUP;

   } else if( astChrMatch( sortby, "KeyAgeDown" ) ) {
      result = SORTBY_KEYAGEDOWN;

   } else if( astChrMatch( sortby, "KeyUp" ) ) {
      result = SORTBY_KEYUP;

   } else if( astChrMatch( sortby, "KeyDown" ) ) {
      result = SORTBY_KEYDOWN;

   } else {
      astError( AST__INTER, "%s(KeyMap): Illegal SortBy value %s "
                "encountered.", status, method, sortby );
   }

/* Return the result. */
   return result;
}

static const char *SortByString( int sortby, const char *method, int *status ){
/*
*  Name:
*     SortByString

*  Purpose:
*     Get the string associated with an integer SortBy value.

*  Type:
*     Private function.

*  Synopsis:
*     #include "keymap.h"
*     const char *SortByString( int sortby, const char *method, int *status )

*  Class Membership:
*     KeyMap member function.

*  Description:
*     This function returns the string associated with the supplied
*     integer SortBy value.

*  Parameters:
*     sortby
*        The integer SortBy value.
*     method
*        Pointer to a string holding the name of the calling method for
*        inclusion in error messages.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     Pointer to the associated SortBy string.

*/

/* Local Variables: */
   const char *result;    /* The returned string */

/* Initialise. */
   result = NULL;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Check each value. */
   if( sortby == SORTBY_NONE ) {
      result = "None";

   } else if( sortby == SORTBY_AGEUP ) {
      result = "AgeUp";

   } else if( sortby == SORTBY_AGEDOWN ) {
      result = "AgeDown";

   } else if( sortby == SORTBY_KEYAGEUP ) {
      result = "KeyAgeUp";

   } else if( sortby == SORTBY_KEYAGEDOWN ) {
      result = "KeyAgeDown";

   } else if( sortby == SORTBY_KEYUP ) {
      result = "KeyUp";

   } else if( sortby == SORTBY_KEYDOWN ) {
      result = "KeyDown";

   } else {
      astError( AST__INTER, "%s(KeyMap): Illegal integer SortBy value %d "
                "encountered (internal AST programming error).", status,
                method, sortby );
   }

/* Return the result. */
   return result;
}

static void SortEntries( AstKeyMap *this, int *status ){
/*
*  Name:
*     SortEntries

*  Purpose:
*     Ensure the entries in a KeyMap are sorted correctly.

*  Type:
*     Private function.

*  Synopsis:
*     #include "keymap.h"
*     void SortEntries( AstKeyMap *this, int *status )

*  Class Membership:
*     KeyMap member function.

*  Description:
*     This function sorts all the entries in the supplied KeyMap in
*     the manner indicated by the SortBy attribute value in the KeyMap.
*     A double linked list is maintained indicating the ordering, with
*     the first entry in the sorted list being pointed to by "this->first".
*     Each entry contains "snext" and "sprev" pointers that point to the
*     next and previous entries in the sorted list. The number of entries
*     in the sorted list (which should usually equal the total number of
*     entries currently in the KeyMap), is stored in "this->nsorted".

*  Parameters:
*     this
*        Pointer to the KeyMap.
*     status
*        Pointer to the inherited status variable.

*/

/* Local Variables: */
   AstMapEntry **ents;
   AstMapEntry **pent;
   AstMapEntry **a;
   AstMapEntry **b;
   AstMapEntry *entry;
   int i;
   int nent;
   int sortby;

/* Check the global error status. */
   if ( !astOK ) return;

/* Empty the sorted list. */
   this->nsorted = 0;
   this->first = NULL;

/* Get the SortBy value. */
   sortby = astGetSortBy( this );

/* Do nothing more if no sorting is required. */
   if( sortby != SORTBY_NONE ) {

/* Get the number of entries in the keyMap. */
      nent = astMapSize( this );

/* Only sort if the KeyMap is not empty. */
      if( nent > 0 ) {

/* Allocate an array with one element for each entry. Each element is a
   pointer to a MapEntry structure. */
         ents = astMalloc( sizeof( *ents )*nent );
         if( astOK ) {

/* Loop round all entries in the hash table. */
            pent = ents;
            for( i = 0; i < this->mapsize; i++ ) {

/* Get a pointer to the next KeyMap entry. */
               entry = this->table[ i ];

/* Loop round all entries in this element of the hash table. */
               while( entry ) {

/* Store the sorting method in the MapEntry. */
                  entry->sortby = sortby;

/* Put a pointer to the MapEntry into the array. */
                  *(pent++) = entry;

/* Update the address of the next MapEntry in the source. */
                  entry = entry->next;
               }
            }

/* No need for sorting if there is only one entry. */
            if( nent == 1 ) {
               ents[ 0 ]->snext = ents[ 0 ];
               ents[ 0 ]->sprev = ents[ 0 ];

/* Sort the array of pointers if there is more than one entry... */
            } else {
               qsort( ents, nent, sizeof( *ents ), CompareEntries );

/* Establish the double linked list. */
               a = ents;
               b = ents + 1;
               for( i = 1; i < nent; i++ ) {
                  (*b)->sprev = *a;
                  (*a)->snext = *b;
                  a = b++;
               }

               b = ents;
               (*b)->sprev = *a;
               (*a)->snext = *b;

            }

/* Store a pointer to the first entry in the sorted list. */
            this->first = ents[ 0 ];

/* Store the number of entrie sin the sorted list. */
            this->nsorted = nent;
         }

/* Free resources. */
         ents = astFree( ents );
      }
   }
}

static int TestAttrib( AstObject *this_object, const char *attrib, int *status ) {
/*
*  Name:
*     TestAttrib

*  Purpose:
*     Test if a specified attribute value is set for a KeyMap.

*  Type:
*     Private function.

*  Synopsis:
*     #include "keymap.h"
*     int TestAttrib( AstObject *this, const char *attrib, int *status )

*  Class Membership:
*     KeyMap member function (over-rides the astTestAttrib protected
*     method inherited from the Mapping class).

*  Description:
*     This function returns a boolean result (0 or 1) to indicate whether
*     a value has been set for one of a KeyMap's attributes.

*  Parameters:
*     this
*        Pointer to the KeyMap.
*     attrib
*        Pointer to a null-terminated string specifying the attribute
*        name.  This should be in lower case with no surrounding white
*        space.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     One if a value has been set, otherwise zero.

*  Notes:
*     - A value of zero will be returned if this function is invoked
*     with the global status set, or if it should fail for any reason.
*/

/* Local Variables: */
   AstKeyMap *this;             /* Pointer to the KeyMap structure */
   int result;                   /* Result value to return */

/* Initialise. */
   result = 0;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Obtain a pointer to the KeyMap structure. */
   this = (AstKeyMap *) this_object;

/* Check the attribute name and test the appropriate attribute. */

/* SizeGuess. */
/* ---------- */
   if ( !strcmp( attrib, "sizeguess" ) ) {
      result = astTestSizeGuess( this );

/* KeyCase. */
/* --------- */
   } else if ( !strcmp( attrib, "keycase" ) ) {
      result = astTestKeyCase( this );

/* KeyError. */
/* --------- */
   } else if ( !strcmp( attrib, "keyerror" ) ) {
      result = astTestKeyError( this );

/* MapLocked. */
/* --------- */
   } else if ( !strcmp( attrib, "maplocked" ) ) {
      result = astTestMapLocked( this );

/* SortBy. */
/* ------- */
   } else if ( !strcmp( attrib, "sortby" ) ) {
      result = astTestSortBy( this );

/* If the attribute is still not recognised, pass it on to the parent
   method for further interpretation. */
   } else {
      result = (*parent_testattrib)( this_object, attrib, status );
   }

/* Return the result, */
   return result;
}

static int TestSizeGuess( AstKeyMap *this, int *status ) {
/*
*+
*  Name:
*     astTestSizeGuess

*  Purpose:
*     Test the value of the SizeGuess attribute for a KeyMap.

*  Type:
*     Protected virtual function.

*  Synopsis:
*     #include "keymap.h"
*     int astTestSizeGuess( AstKeyMap *this )

*  Class Membership:
*     KeyMap method.

*  Description:
*     This function returns a non-zero value if the SizeGuess attribute
*     has been set in a KeyMap.

*  Parameters:
*     this
*        Pointer to the KeyMap.

*  Returned Value:
*     Non-zero if the SizeGuess attribute is set.

*  Notes:
*     - A value of zero is returned if this function is invoked with the
*     global error status set.

*-
*/

/* Local Variables: */
   int result;                /* Returned value */

/* Initialise */
   result = 0;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Return non-zero if the attribute is still set to its "not set" value. */
   return ( this->sizeguess != INT_MAX );
}

/* Functions which access class attributes. */
/* ---------------------------------------- */

/*
*att++
*  Name:
*     SizeGuess

*  Purpose:
*     The expected size of the KeyMap.

*  Type:
*     Public attribute.

*  Synopsis:
*     Integer.

*  Description:
*     This is attribute gives an estimate of the number of entries that
*     will be stored in the KeyMap. It is used to tune the internal
*     properties of the KeyMap for speed and efficiency. A larger value
*     will result in faster access at the expense of increased memory
*     requirements. However if the SizeGuess value is much larger than
*     the actual size of the KeyMap, then there will be little, if any,
*     speed gained by making the SizeGuess even larger. The default value
*     is 300.
*
*     The value of this attribute can only be changed if the KeyMap is
*     empty. Its value can be set conveniently when creating the KeyMap.
*     An error will be reported if an attempt is made to set or clear the
*     attribute when the KeyMap contains any entries.

*  Applicability:
*     KeyMap
*        All KeyMaps have this attribute.
*att--
*/

/*
*att++
*  Name:
*     KeyCase

*  Purpose:
*     Are keys case sensitive?

*  Type:
*     Public attribute.

*  Synopsis:
*     Integer (boolean).

*  Description:
*     This attribute is a boolean value which controls how keys are
*     used. If KeyCase is zero, then key strings supplied to any method
*     are automatically converted to upper case before being used. If
*     KeyCase is non-zero (the default), then supplied key strings are
*     used without modification.
*
*     The value of this attribute can only be changed if the KeyMap is
*     empty. Its value can be set conveniently when creating the KeyMap.
*     An error will be reported if an attempt is made to change the
*     attribute value when the KeyMap contains any entries.

*  Applicability:
*     KeyMap
*        All KeyMaps have this attribute.
*     Table
*        The Table class over-rides this attribute by forcing it to zero.
*        That is, keys within a Table are always case insensitive.
*att--
*/
astMAKE_GET(KeyMap,KeyCase,int,1,(this->keycase == -1 ? 1 : this->keycase))
astMAKE_TEST(KeyMap,KeyCase,( this->keycase != -1 ))

/*
*att++
*  Name:
*     KeyError

*  Purpose:
*     Report an error when getting the value of a non-existant KeyMap entry?

*  Type:
*     Public attribute.

*  Synopsis:
*     Integer (boolean).

*  Description:
*     This attribute is a boolean value which controls how the
c     astMapGet...
f     AST_MAPGET...
*     functions behave if the requested key is not found in the KeyMap.
*     If KeyError is zero (the default), then these functions will return
c     zero
f     .FALSE.
*     but no error will be reported. If KeyError is non-zero, then the
*     same values are returned but an error is also reported.

*  Notes:
*     - When setting a new value for KeyError, the supplied value is
*     propagated to any KeyMaps contained within the supplied KeyMap.
*     - When clearing the KeyError attribute, the attribute is also
*     cleared in any KeyMaps contained within the supplied KeyMap.

*  Applicability:
*     KeyMap
*        All KeyMaps have this attribute.
*att--
*/
astMAKE_GET(KeyMap,KeyError,int,0,( ( this->keyerror != -INT_MAX ) ?
                                   this->keyerror : 0 ))
astMAKE_TEST(KeyMap,KeyError,( this->keyerror != -INT_MAX ))

/*
*att++
*  Name:
*     MapLocked

*  Purpose:
*     Prevent new entries being added to a KeyMap?

*  Type:
*     Public attribute.

*  Synopsis:
*     Integer (boolean).

*  Description:
*     If this boolean attribute is set to
c     a non-zero value,
f     .TRUE.,
*     an error will be reported if an attempt is made to add a new entry
*     to the KeyMap. Note, the value associated with any existing entries
*     can still be changed, but no new entries can be stored in the KeyMap.
*     The default value
c     (zero)
f     (.FALSE.)
*     allows new entries to be added to the KeyMap.

*  Notes:
*     - When setting a new value for MapLocked, the supplied value is
*     propagated to any KeyMaps contained within the supplied KeyMap.
*     - When clearing the MapLocked attribute, the attribute is also
*     cleared in any KeyMaps contained within the supplied KeyMap.

*  Applicability:
*     KeyMap
*        All KeyMaps have this attribute.
*att--
*/
astMAKE_GET(KeyMap,MapLocked,int,0,( ( this->maplocked != -INT_MAX ) ?
                                   this->maplocked : 0 ))
astMAKE_TEST(KeyMap,MapLocked,( this->maplocked != -INT_MAX ))

/*
*att++
*  Name:
*     SortBy

*  Purpose:
*     Determines how keys are sorted in a KeyMap.

*  Type:
*     Public attribute.

*  Synopsis:
*     String.

*  Description:
*     This attribute determines the order in which keys are returned by the
c     astMapKey
f     AST_MAPKEY
*     function. It may take the following values (the default is "None"):
*
*     - "None": The keys are returned in an arbitrary order. This is the
*     fastest method as it avoids the need for a sorted list of keys to
*     be maintained and used.
*
*     - "AgeDown": The keys are returned in the order in which values were
*     stored in the KeyMap, with the key for the most recent value being
*     returned last. If the value of an existing entry is changed, it goes
*     to the end of the list.
*
*     - "AgeUp": The keys are returned in the order in which values were
*     stored in the KeyMap, with the key for the most recent value being
*     returned first. If the value of an existing entry is changed, it goes
*     to the top of the list.
*
*     - "KeyAgeDown": The keys are returned in the order in which they
*     were originally stored in the KeyMap, with the most recent key being
*     returned last. If the value of an existing entry is changed, its
*     position in the list does not change.
*
*     - "KeyAgeUp": The keys are returned in the order in which they
*     were originally stored in the KeyMap, with the most recent key being
*     returned first. If the value of an existing entry is changed, its
*     position in the list does not change.
*
*     - "KeyDown": The keys are returned in alphabetical order, with "A..."
*     being returned last.
*
*     - "KeyUp": The keys are returned in alphabetical order, with "A..."
*     being returned first.

*  Notes:
*     - If a new value is assigned to SortBy (or if SortBy is cleared),
*     all entries currently in the KeyMap are re-sorted according to the
*     new SortBy value.

*  Applicability:
*     KeyMap
*        All KeyMaps have this attribute.
*att--
*/
astMAKE_GET(KeyMap,SortBy,int,SORTBY_NONE,( ( this->sortby != -INT_MAX ) ?
                                           this->sortby : SORTBY_NONE ))
astMAKE_TEST(KeyMap,SortBy,( this->sortby != -INT_MAX ))

/* Copy constructor. */
/* ----------------- */
static void Copy( const AstObject *objin, AstObject *objout, int *status ) {
/*
*  Name:
*     Copy

*  Purpose:
*     Copy constructor for KeyMap objects.

*  Type:
*     Private function.

*  Synopsis:
*     void Copy( const AstObject *objin, AstObject *objout, int *status )

*  Description:
*     This function implements the copy constructor for KeyMap objects.

*  Parameters:
*     objin
*        Pointer to the object to be copied.
*     objout
*        Pointer to the object being constructed.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     void

*  Notes:
*     -  This constructor makes a deep copy.
*/

/* Local Variables: */
   AstKeyMap *in;                  /* Pointer to input KeyMap */
   AstKeyMap *out;                 /* Pointer to output KeyMap */
   int i;                       /* Index into hash table */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain pointers to the input and output KeyMap structures. */
   in = (AstKeyMap *) objin;
   out = (AstKeyMap *) objout;

/* For safety, first clear any references to the input memory from
   the output KeyMap. */
   out->table = NULL;
   out->nentry = NULL;
   out->first = NULL;
   out->firstA = NULL;

/* Make copies of the table entries. */
   out->table = astMalloc( sizeof( AstMapEntry * )*( out->mapsize ) );
   out->nentry = astMalloc( sizeof( int )*( out->mapsize ) );

   for( i = 0; i < out->mapsize; i++ ) CopyTableEntry( in, out, i, status );

/* Create the required sorted key list in the new KeyMap. */
   SortEntries( out, status );

/* If an error occurred, clean up by freeing all memory allocated above. */
   if ( !astOK ) {
      for( i = 0; i < out->mapsize; i++ ) FreeTableEntry( out, i, status );
      out->table = astFree( out->table );
      out->nentry = astFree( out->nentry );
   }
}

/* Destructor. */
/* ----------- */
static void Delete( AstObject *obj, int *status ) {
/*
*  Name:
*     Delete

*  Purpose:
*     Destructor for KeyMap objects.

*  Type:
*     Private function.

*  Synopsis:
*     void Delete( AstObject *obj, int *status )

*  Description:
*     This function implements the destructor for KeyMap objects.

*  Parameters:
*     obj
*        Pointer to the object to be deleted.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     void

*  Notes:
*     This function attempts to execute even if the global error status is
*     set.
*/

/* Local Variables: */
   AstKeyMap *this;             /* Pointer to the KeyMap structure */
   int i;                    /* Loop count */

/* Obtain a pointer to the KeyMap structure. */
   this = (AstKeyMap *) obj;

/* Free all allocated memory. */
   for( i = 0; i < this->mapsize; i++ ) FreeTableEntry( this, i, status );

/* Free memory used to hold tables. */
   this->table = astFree( this->table );
   this->nentry = astFree( this->nentry );

/* Nullify other pointers. */
   this->first = NULL;
   this->firstA = NULL;
}

/* Dump function. */
/* -------------- */
static void Dump( AstObject *this_object, AstChannel *channel, int *status ) {
/*
*  Name:
*     Dump

*  Purpose:
*     Dump function for KeyMap objects.

*  Type:
*     Private function.

*  Synopsis:
*     void Dump( AstObject *this, AstChannel *channel, int *status )

*  Description:
*     This function implements the Dump function which writes out data
*     for the KeyMap class to an output Channel.

*  Parameters:
*     this
*        Pointer to the KeyMap whose data are being written.
*     channel
*        Pointer to the Channel to which the data are being written.
*     status
*        Pointer to the inherited status variable.
*/

/* Local Variables: */
   AstKeyMap *this;              /* Pointer to the KeyMap structure */
   AstMapEntry *next;            /* Pointer to the next AstMapEntry to dump */
   int i;                        /* Index into hash table */
   int nentry;                   /* Number of entries dumped so far */
   int set;                      /* Is attribute set? */
   int ival;                     /* Attribute value */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain a pointer to the KeyMap structure. */
   this = (AstKeyMap *) this_object;

/* Initialise the number of KeyMap entries dumped so far. */
   nentry = 0;

/* SizeGuess. */
/* ---------- */
   set = TestSizeGuess( this, status );
   ival = set ? GetSizeGuess( this, status ) : astGetSizeGuess( this );
   astWriteInt( channel, "SzGss", set, 0, ival, "Guess at KeyMap size" );

/* SortBy. */
/* ------- */
   set = TestSortBy( this, status );
   ival = set ? GetSortBy( this, status ) : astGetSortBy( this );
   astWriteString( channel, "SortBy", set, 0, SortByString( ival, "astDump",
                                                            status ),
                   "Sorting scheme for keys" );

/* KeyCase. */
/* --------- */
   set = TestKeyCase( this, status );
   ival = set ? GetKeyCase( this, status ) : astGetKeyCase( this );
   astWriteInt( channel, "KyCas", set, 0, ival, "Are keys case sensitive?" );

/* KeyError. */
/* --------- */
   set = TestKeyError( this, status );
   ival = set ? GetKeyError( this, status ) : astGetKeyError( this );
   astWriteInt( channel, "KyErr", set, 0, ival, "Report non-existant keys?" );

/* MapLocked. */
/* --------- */
   set = TestMapLocked( this, status );
   ival = set ? GetMapLocked( this, status ) : astGetMapLocked( this );
   astWriteInt( channel, "MpLck", set, 0, ival, "Prevent addition of new entries?" );

/* MapSize. */
/* -------- */
   astWriteInt( channel, "MapSz", 1, 1, this->mapsize, "Size of hash table" );

/* member count. */
   astWriteInt( channel, "MemCnt", 1, 1, this->member_count, "Total member count" );

/* Loop round each entry in the hash table. */
   for( i = 0; i < this->mapsize; i++ ) {

/* Get a pointer to the next KeyMap entry to dump. */
      next = this->table[ i ];

/* Loop round dumping all KeyMap entries in this element of the hash table. */
      while( next && astOK ) {
         DumpEntry( next, channel, ++nentry, status );

/* Get a pointer to the next entry to dump. */
         next = next->next;

      }
   }
}

/* Standard class functions. */
/* ========================= */
/* Implement the astIsAKeyMap and astCheckKeyMap functions using the macros
   defined for this purpose in the "object.h" header file. */
astMAKE_ISA(KeyMap,Object)
astMAKE_CHECK(KeyMap)

AstKeyMap *astKeyMap_( const char *options, int *status, ...) {
/*
*++
*  Name:
c     astKeyMap
f     AST_KEYMAP

*  Purpose:
*     Create a KeyMap.

*  Type:
*     Public function.

*  Synopsis:
c     #include "keymap.h"
c     AstKeyMap *astKeyMap( const char *options, ... )
f     RESULT = AST_KEYMAP( OPTIONS, STATUS )

*  Class Membership:
*     KeyMap constructor.

*  Description:
*     This function creates a new empty KeyMap and optionally initialises its
*     attributes. Entries can then be added to the KeyMap using the
c     astMapPut0<X> and astMapPut1<X> functions.
f     AST_MAPPUT0<X> and AST_MAPPUT1<X> functions.
*
*     The KeyMap class is used to store a set of values with associated keys
*     which identify the values. The keys are strings. These may be case
*     sensitive or insensitive as selected by the KeyCase attribute, and
*     trailing spaces are ignored. The value associated with a key can be
*     integer (signed 4 and 2 byte, or unsigned 1 byte), floating point
*     (single or double precision),
c     void pointer,
*     character string or AST Object pointer. Each
*     value can be a scalar or a one-dimensional vector. A KeyMap is
*     conceptually similar to a Mapping in that a KeyMap transforms an
*     input into an output - the input is the key, and the output is the
*     value associated with the key. However, this is only a conceptual
*     similarity, and it should be noted that the KeyMap class inherits from
*     the Object class rather than the Mapping class. The methods of the
*     Mapping class cannot be used with a KeyMap.

*  Parameters:
c     options
f     OPTIONS = CHARACTER * ( * ) (Given)
c        Pointer to a null-terminated string containing an optional
c        comma-separated list of attribute assignments to be used for
c        initialising the new KeyMap. The syntax used is identical to
c        that for the astSet function and may include "printf" format
c        specifiers identified by "%" symbols in the normal way.
f        A character string containing an optional comma-separated
f        list of attribute assignments to be used for initialising the
f        new KeyMap. The syntax used is identical to that for the
f        AST_SET routine.
c     ...
c        If the "options" string contains "%" format specifiers, then
c        an optional list of additional arguments may follow it in
c        order to supply values to be substituted for these
c        specifiers. The rules for supplying these are identical to
c        those for the astSet function (and for the C "printf"
c        function).
f     STATUS = INTEGER (Given and Returned)
f        The global status.

*  Returned Value:
c     astKeyMap()
f     AST_MAP = INTEGER
*        A pointer to the new KeyMap.

*  Notes:
*     - A null Object pointer (AST__NULL) will be returned if this
c     function is invoked with the AST error status set, or if it
f     function is invoked with STATUS set to an error value, or if it
*     should fail for any reason.

*  Status Handling:
*     The protected interface to this function includes an extra
*     parameter at the end of the parameter list descirbed above. This
*     parameter is a pointer to the integer inherited status
*     variable: "int *status".

*--
*/

/* Local Variables: */
   astDECLARE_GLOBALS            /* Pointer to thread-specific global data */
   AstKeyMap *new;                 /* Pointer to new KeyMap */
   va_list args;                /* Variable argument list */

/* Get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(NULL);

/* Check the global status. */
   if ( !astOK ) return NULL;

/* Initialise the KeyMap, allocating memory and initialising the
   virtual function table as well if necessary. */
   new = astInitKeyMap( NULL, sizeof( AstKeyMap ), !class_init, &class_vtab, "KeyMap" );

/* If successful, note that the virtual function table has been
   initialised. */
   if ( astOK ) {
      class_init = 1;

/* Obtain the variable argument list and pass it along with the options string
   to the astVSet method to initialise the new KeyMap's attributes. */
      va_start( args, status );
      astVSet( new, options, NULL, args );
      va_end( args );

/* If an error occurred, clean up by deleting the new object. */
      if ( !astOK ) new = astDelete( new );
   }

/* Return a pointer to the new KeyMap. */
   return new;
}

AstKeyMap *astKeyMapId_( const char *options, ... ) {
/*
*  Name:
*     astKeyMapId_

*  Purpose:
*     Create a KeyMap.

*  Type:
*     Private function.

*  Synopsis:
*     #include "keymap.h"
*     AstKeyMap *astKeyMapId_( const char *options, ... )

*  Class Membership:
*     KeyMap constructor.

*  Description:
*     This function implements the external (public) interface to the
*     astKeyMap constructor function. It returns an ID value (instead
*     of a true C pointer) to external users, and must be provided
*     because astKeyMap_ has a variable argument list which cannot be
*     encapsulated in a macro (where this conversion would otherwise
*     occur).
*
*     The variable argument list also prevents this function from
*     invoking astKeyMap_ directly, so it must be a re-implementation
*     of it in all respects, except for the final conversion of the
*     result to an ID value.

*  Parameters:
*     As for astKeyMap_.

*  Returned Value:
*     The ID value associated with the new KeyMap.
*/

/* Local Variables: */
   astDECLARE_GLOBALS            /* Pointer to thread-specific global data */
   AstKeyMap *new;                 /* Pointer to new KeyMap */
   va_list args;                /* Variable argument list */

   int *status;                  /* Pointer to inherited status value */

/* Get a pointer to the inherited status value. */
   status = astGetStatusPtr;

/* Get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(NULL);

/* Check the global status. */
   if ( !astOK ) return NULL;

/* Initialise the KeyMap, allocating memory and initialising the
   virtual function table as well if necessary. */
   new = astInitKeyMap( NULL, sizeof( AstKeyMap ), !class_init, &class_vtab, "KeyMap" );

/* If successful, note that the virtual function table has been
   initialised. */
   if ( astOK ) {
      class_init = 1;

/* Obtain the variable argument list and pass it along with the options string
   to the astVSet method to initialise the new KeyMap's attributes. */
      va_start( args, options );
      astVSet( new, options, NULL, args );
      va_end( args );

/* If an error occurred, clean up by deleting the new object. */
      if ( !astOK ) new = astDelete( new );
   }

/* Return an ID value for the new KeyMap. */
   return astMakeId( new );
}

AstKeyMap *astInitKeyMap_( void *mem, size_t size, int init, AstKeyMapVtab *vtab,
                           const char *name, int *status ) {
/*
*+
*  Name:
*     astInitKeyMap

*  Purpose:
*     Initialise a KeyMap.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "keymap.h"
*     AstKeyMap *astInitKeyMap( void *mem, size_t size, int init, AstKeyMapVtab *vtab,
*                               const char *name )

*  Class Membership:
*     KeyMap initialiser.

*  Description:
*     This function is provided for use by class implementations to initialise
*     a new KeyMap object. It allocates memory (if necessary) to accommodate
*     the KeyMap plus any additional data associated with the derived class.
*     It then initialises a KeyMap structure at the start of this memory. If
*     the "init" flag is set, it also initialises the contents of a virtual
*     function table for a KeyMap at the start of the memory passed via the
*     "vtab" parameter.

*  Parameters:
*     mem
*        A pointer to the memory in which the KeyMap is to be created. This
*        must be of sufficient size to accommodate the KeyMap data
*        (sizeof(KeyMap)) plus any data used by the derived class. If a value
*        of NULL is given, this function will allocate the memory itself using
*        the "size" parameter to determine its size.
*     size
*        The amount of memory used by the KeyMap (plus derived class data).
*        This will be used to allocate memory if a value of NULL is given for
*        the "mem" parameter. This value is also stored in the KeyMap
*        structure, so a valid value must be supplied even if not required for
*        allocating memory.
*     init
*        A logical flag indicating if the KeyMap's virtual function table is
*        to be initialised. If this value is non-zero, the virtual function
*        table will be initialised by this function.
*     vtab
*        Pointer to the start of the virtual function table to be associated
*        with the new KeyMap.
*     name
*        Pointer to a constant null-terminated character string which contains
*        the name of the class to which the new object belongs (it is this
*        pointer value that will subsequently be returned by the astClass
*        method).

*  Returned Value:
*     A pointer to the new KeyMap.

*  Notes:
*     -  A NULL pointer will be returned if this function is invoked with the
*     global error status set, or if it should fail for any reason.
*-
*/

/* Local Variables: */
   AstKeyMap *new;              /* Pointer to the new KeyMap */

/* Check the global error status. */
   if ( !astOK ) return NULL;

/* If necessary, initialise the virtual function table. */
   if ( init ) astInitKeyMapVtab( vtab, name );

/* Initialise an Object structure (the parent class) as the first component
   within the KeyMap structure, allocating memory if necessary. */
   new = (AstKeyMap *) astInitObject( mem, size, 0, (AstObjectVtab *) vtab,
                                      name );

   if ( astOK ) {

/* Initialise the KeyMap data. */
/* ---------------------------- */
/* Initialise all attributes to their "undefined" values. */
      new->sizeguess = INT_MAX;
      new->mapsize = 0;
      new->table = NULL;
      new->nentry = NULL;
      new->keycase = -1;
      new->keyerror = -INT_MAX;
      new->maplocked = -INT_MAX;
      new->sortby = -INT_MAX;
      new->first = NULL;
      new->nsorted = 0;
      new->member_count = 0;
      new->firstA = NULL;
      new->iter_itab = 0;
      new->iter_entry = NULL;

      NewTable( new, MIN_TABLE_SIZE, status );

/* If an error occurred, clean up by deleting the new KeyMap. */
      if ( !astOK ) new = astDelete( new );
   }

/* Return a pointer to the new KeyMap. */
   return new;
}

AstKeyMap *astLoadKeyMap_( void *mem, size_t size, AstKeyMapVtab *vtab,
                           const char *name, AstChannel *channel, int *status ) {
/*
*+
*  Name:
*     astLoadKeyMap

*  Purpose:
*     Load a KeyMap.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "keymap.h"
*     AstKeyMap *astLoadKeyMap( void *mem, size_t size, AstKeyMapVtab *vtab,
*                               const char *name, AstChannel *channel )

*  Class Membership:
*     KeyMap loader.

*  Description:
*     This function is provided to load a new KeyMap using data read
*     from a Channel. It first loads the data used by the parent class
*     (which allocates memory if necessary) and then initialises a
*     KeyMap structure in this memory, using data read from the input
*     Channel.
*
*     If the "init" flag is set, it also initialises the contents of a
*     virtual function table for a KeyMap at the start of the memory
*     passed via the "vtab" parameter.


*  Parameters:
*     mem
*        A pointer to the memory into which the KeyMap is to be
*        loaded.  This must be of sufficient size to accommodate the
*        KeyMap data (sizeof(KeyMap)) plus any data used by derived
*        classes. If a value of NULL is given, this function will
*        allocate the memory itself using the "size" parameter to
*        determine its size.
*     size
*        The amount of memory used by the KeyMap (plus derived class
*        data).  This will be used to allocate memory if a value of
*        NULL is given for the "mem" parameter. This value is also
*        stored in the KeyMap structure, so a valid value must be
*        supplied even if not required for allocating memory.
*
*        If the "vtab" parameter is NULL, the "size" value is ignored
*        and sizeof(AstKeyMap) is used instead.
*     vtab
*        Pointer to the start of the virtual function table to be
*        associated with the new KeyMap. If this is NULL, a pointer
*        to the (static) virtual function table for the KeyMap class
*        is used instead.
*     name
*        Pointer to a constant null-terminated character string which
*        contains the name of the class to which the new object
*        belongs (it is this pointer value that will subsequently be
*        returned by the astGetClass method).
*
*        If the "vtab" parameter is NULL, the "name" value is ignored
*        and a pointer to the string "KeyMap" is used instead.

*  Returned Value:
*     A pointer to the new KeyMap.

*  Notes:
*     - A null pointer will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*-
*/

/* Local Variables: */
   AstKeyMap *new;           /* Pointer to the new KeyMap */
   AstObject **alist;        /* Pointer to vector of entry values */
   AstObject *aval;          /* AST Object value for an entry */
   astDECLARE_GLOBALS        /* Pointer to thread-specific global data */
   char *com;                /* Pointer to comment string for an entry */
   char *key;                /* Pointer to key string for an entry */
   char *sval;               /* String value for an entry */
   char buff[ 30 ];          /* Buffer for key names */
   const char **slist;       /* Pointer to vector of entry values */
   double *dlist;            /* Pointer to vector of entry values */
   double dval;              /* Floating point value for an entry */
   float *flist;             /* Pointer to vector of entry values */
   int *ilist;               /* Pointer to vector of entry values */
   int index;                /* Index of next array element in a vector entry */
   int ival;                 /* Integer value for an entry */
   int mapsize;              /* Size for new hash table */
   int nel;                  /* Vector length */
   int nentry;               /* Number of KeyMap entries read so far */
   int type;                 /* Data type for an entry */
   short int *wlist;         /* Pointer to vector of entry values */
   short int wval;           /* Short int value for an entry */
   unsigned char *blist;     /* Pointer to vector of entry values */
   unsigned char bval;       /* Byte value for an entry */

/* Get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(channel);

/* Initialise. */
   new = NULL;

/* Check the global error status. */
   if ( !astOK ) return new;

/* If a NULL virtual function table has been supplied, then this is
   the first loader to be invoked for this KeyMap. In this case the
   KeyMap belongs to this class, so supply appropriate values to be
   passed to the parent class loader (and its parent, etc.). */
   if ( !vtab ) {
      size = sizeof( AstKeyMap );
      vtab = &class_vtab;
      name = "KeyMap";

/* If required, initialise the virtual function table for this class. */
      if ( !class_init ) {
         astInitKeyMapVtab( vtab, name );
         class_init = 1;
      }
   }

/* Invoke the parent class loader to load data for all the ancestral
   classes of the current one, returning a pointer to the resulting
   partly-built KeyMap. */
   new = astLoadObject( mem, size, (AstObjectVtab *) vtab, name, channel );

   if ( astOK ) {

/* Inidicate the KeyMap is empty. */
      new->mapsize = 0;
      new->table = NULL;
      new->nentry = NULL;
      new->firstA = NULL;
      new->iter_itab = 0;
      new->iter_entry = NULL;

/* Read input data. */
/* ================ */
/* Request the input Channel to read all the input data appropriate to
   this class into the internal "values list". */
      astReadClassData( channel, "KeyMap" );

/* Now read each individual data item from this list and use it to
   initialise the appropriate instance variable(s) for this class. */

/* SizeGuess. */
/* ---------- */
      new->sizeguess = astReadInt( channel, "szgss", INT_MAX );
      if ( TestSizeGuess( new, status ) ) SetSizeGuess( new, new->sizeguess, status );

/* KeyCase. */
/* --------- */
      new->keycase = astReadInt( channel, "kycas", -INT_MAX );
      if ( TestKeyCase( new, status ) ) SetKeyCase( new, new->keycase, status );

/* KeyError. */
/* --------- */
      new->keyerror = astReadInt( channel, "kyerr", -INT_MAX );
      if ( TestKeyError( new, status ) ) SetKeyError( new, new->keyerror, status );

/* MapLocked. */
/* --------- */
      new->maplocked = astReadInt( channel, "mplck", -INT_MAX );
      if ( TestMapLocked( new, status ) ) SetMapLocked( new, new->maplocked, status );

/* SortBy. */
/* ------- */
      sval = astReadString( channel, "sortby", " " );
      new->sortby = -INT_MAX;
      if( astOK && strcmp( sval, " " ) ) {
         new->sortby = SortByInt( sval, "astRead", status );
      }
      if( TestSortBy( new, status ) ) SetSortBy( new, new->sortby, status );
      sval = astFree( sval );

/* MapSize. */
/* -------- */
      mapsize = astReadInt( channel, "mapsz", MIN_TABLE_SIZE );
      NewTable( new, mapsize, status );

/* Entries... */
/* ---------- */

/* Initialise the index of the next AstMapEntry to be read. */
      nentry = 0;

/* Read Entries until no more are found */
      while( astOK ) {
         nentry++;

/* Get the entry key. */
         (void) sprintf( buff, "key%d", nentry );
         key = astReadString( channel, buff, NULL );

/* We have finished reading entries if no key was found. */
         if( !key ) break;

/* Get the entry comment. */
         (void) sprintf( buff, "com%d", nentry );
         com = astReadString( channel, buff, NULL );

/* Get the entry data type. */
         (void) sprintf( buff, "typ%d", nentry );
         type = astReadInt( channel, buff, AST__BADTYPE );

         if( type == AST__BADTYPE && astOK ) {
            astError( AST__BDFTS, "astLoadKeyMap(%s): No data type code found "
                      "whilst reading a %s.", status, name, name );

         }

/* Get the vector length. */
         (void) sprintf( buff, "nel%d", nentry );
         nel = astReadInt( channel, buff, 0 );

/* Get the entry member number. Set the KeyMap member count to this value
   so that the next entry added to the KeyMap will get this value as its
   member index. */
         (void) sprintf( buff, "mem%d", nentry );
         new->member_count = astReadInt( channel, buff, 0 );

/* First deal with integer entries. */
         if( type == AST__INTTYPE ) {

/* For scalar entries, use "val<nentry>" to get the value then create a new
   entry and add it to the KeyMap. */
            if( nel == 0 ) {
               (void) sprintf( buff, "val%d", nentry );
               ival = astReadInt( channel, buff, 0 );
               MapPut0I( new, key, ival, com, status );

/* If we must have an array of values... */
            } else {

/* Create an array to hold the values. */
               ilist = astMalloc( sizeof(int)*nel );

/* Loop round reading values. */
               for( index = 0; astOK && index < nel; index++ ) {
                  (void) sprintf( buff, "v%d_%d", nentry, index + 1 );
                  ilist[ index ] = astReadInt( channel, buff, 0 );
               }

/* Create the KeyMap entry. */
               MapPut1I( new, key, nel, ilist, com, status );

/* Free resources. */
               ilist = astFree( ilist );
            }

/* Do the same for short int values. */
         } else if( type == AST__SINTTYPE ) {
            if( nel == 0 ) {
               (void) sprintf( buff, "val%d", nentry );
               wval = (short int) astReadInt( channel, buff, 0 );
               MapPut0S( new, key, wval, com, status );
            } else {
               wlist = astMalloc( sizeof(short int)*nel );
               for( index = 0; astOK && index < nel; index++ ) {
                  (void) sprintf( buff, "v%d_%d", nentry, index + 1 );
                  wlist[ index ] = (short int) astReadInt( channel, buff, 0 );
               }
               MapPut1S( new, key, nel, wlist, com, status );
               wlist = astFree( wlist );
            }

/* Do the same for byte values. */
         } else if( type == AST__BYTETYPE ) {
            if( nel == 0 ) {
               (void) sprintf( buff, "val%d", nentry );
               bval = (unsigned char) astReadInt( channel, buff, 0 );
               MapPut0B( new, key, bval, com, status );
            } else {
               blist = astMalloc( sizeof(unsigned char)*nel );
               for( index = 0; astOK && index < nel; index++ ) {
                  (void) sprintf( buff, "v%d_%d", nentry, index + 1 );
                  blist[ index ] = (unsigned char) astReadInt( channel, buff, 0 );
               }
               MapPut1B( new, key, nel, blist, com, status );
               blist = astFree( blist );
            }

/* Do the same for double values. */
         } else if( type == AST__DOUBLETYPE ) {
            if( nel == 0 ) {
               (void) sprintf( buff, "val%d", nentry );
               dval = astReadDouble( channel, buff, AST__BAD );
               MapPut0D( new, key, dval, com, status );
            } else {
               dlist = astMalloc( sizeof(double)*nel );
               for( index = 0; astOK && index < nel; index++ ) {
                  (void) sprintf( buff, "v%d_%d", nentry, index + 1 );
                  dlist[ index ] = astReadDouble( channel, buff, AST__BAD );
               }
               MapPut1D( new, key, nel, dlist, com, status );
               dlist = astFree( dlist );
            }

/* Do the same for float values. */
         } else if( type == AST__FLOATTYPE ) {
            if( nel == 0 ) {
               (void) sprintf( buff, "val%d", nentry );
               dval = astReadDouble( channel, buff, 0.0 );
               MapPut0F( new, key, (float) dval, com, status );
            } else {
               flist = astMalloc( sizeof(float)*nel );
               for( index = 0; astOK && index < nel; index++ ) {
                  (void) sprintf( buff, "v%d_%d", nentry, index + 1 );
                  flist[ index ] = (float) astReadDouble( channel, buff, 0.0 );
               }
               MapPut1F( new, key, nel, flist, com, status );
               flist = astFree( flist );
            }

/* Do the same for string values. */
         } else if( type == AST__STRINGTYPE ) {
            if( nel == 0 ) {
               (void) sprintf( buff, "val%d", nentry );
               sval = astReadString( channel, buff, "" );
               MapPut0C( new, key, sval, com, status );
               sval = astFree( sval );
            } else {
               slist = astMalloc( sizeof(const char *)*nel );
               for( index = 0; astOK && index < nel; index++ ) {
                  (void) sprintf( buff, "v%d_%d", nentry, index + 1 );
                  slist[ index ] = astReadString( channel, buff, "" );
               }
               MapPut1C( new, key, nel, slist, com, status );
               for( index = 0; astOK && index < nel; index++ ) {
                  slist[ index ] = astFree( (void *) slist[ index ] );
               }
               slist = astFree( slist );
            }

/* Do the same for object values. */
         } else if( type == AST__OBJECTTYPE ) {
            if( nel == 0 ) {
               (void) sprintf( buff, "val%d", nentry );
               aval = astReadObject( channel, buff, NULL );
               MapPut0A( new, key, aval, com, status );
               if( aval ) aval = astAnnul( aval );
            } else {
               alist = astMalloc( sizeof(AstObject *)*nel );
               for( index = 0; astOK && index < nel; index++ ) {
                  (void) sprintf( buff, "v%d_%d", nentry, index + 1 );
                  alist[ index ] = astReadObject( channel, buff, NULL );
               }
               MapPut1A( new, key, nel, alist, com, status );
               for( index = 0; astOK && index < nel; index++ ) {
                  if( alist[ index ] ) alist[ index ] = astAnnul( alist[ index ] );
               }
               alist = astFree( alist );
            }

/* Undef values have no value. */
         } else if( type == AST__UNDEFTYPE ) {
            MapPutU( new, key, com, status );

/* Report an error if the data type is unknown. */
         } else if( astOK ) {
            astError( AST__BDFTS, "astLoadKeyMap(%s): Unknown data type code "
                      "(%d) encountered whilst reading a %s.", status, name, type,
                      name );
         }
/* Free resources. */
         key = astFree( key );
         if( com ) com = astFree( com );

      }

/* Set the final member count for the KeyMap. */
      new->member_count = astReadInt( channel, "memcnt", 0 );

/* If an error occurred, clean up by deleting the new KeyMap. */
      if ( !astOK ) new = astDelete( new );
   }

/* Return the new KeyMap pointer. */
   return new;
}

/* Virtual function interfaces. */
/* ============================ */
/* These provide the external interface to the virtual functions defined by
   this class. Each simply checks the global error status and then locates and
   executes the appropriate member function, using the function pointer stored
   in the object's virtual function table (this pointer is located using the
   astMEMBER macro defined in "object.h").

   Note that the member function may not be the one defined here, as it may
   have been over-ridden by a derived class. However, it should still have the
   same interface. */

#define MAKE_MAPPUT0_(X,Xtype) \
void astMapPut0##X##_( AstKeyMap *this, const char *key, Xtype value, \
                      const char *comment, int *status ){ \
   if ( !astOK ) return; \
   (**astMEMBER(this,KeyMap,MapPut0##X))(this,key,value,comment, status ); \
}
MAKE_MAPPUT0_(D,double)
MAKE_MAPPUT0_(F,float)
MAKE_MAPPUT0_(I,int)
MAKE_MAPPUT0_(C,const char *)
MAKE_MAPPUT0_(A,AstObject *)
MAKE_MAPPUT0_(P,void *)
MAKE_MAPPUT0_(S,short int)
MAKE_MAPPUT0_(B,unsigned char)
#undef MAKE_MAPPUT0_


#define MAKE_MAPPUT1_(X,Xtype) \
void astMapPut1##X##_( AstKeyMap *this, const char *key, int size, \
                       Xtype value[], const char *comment, \
                       int *status ){ \
   if ( !astOK ) return; \
   (**astMEMBER(this,KeyMap,MapPut1##X))(this,key,size,value,comment, status ); \
}
MAKE_MAPPUT1_(S,const short int)
MAKE_MAPPUT1_(B,const unsigned char)
MAKE_MAPPUT1_(D,const double)
MAKE_MAPPUT1_(F,const float)
MAKE_MAPPUT1_(I,const int)
MAKE_MAPPUT1_(C,const char *const)
MAKE_MAPPUT1_(A,AstObject *const)
MAKE_MAPPUT1_(P,void *const)
#undef MAKE_MAPPUT1_

#define MAKE_MAPGET0_(X,Xtype) \
int astMapGet0##X##_( AstKeyMap *this, const char *key, Xtype *value, int *status ){ \
   if ( !astOK ) return 0; \
   return (**astMEMBER(this,KeyMap,MapGet0##X))(this,key,value, status ); \
}
MAKE_MAPGET0_(D,double)
MAKE_MAPGET0_(S,short int)
MAKE_MAPGET0_(B,unsigned char)
MAKE_MAPGET0_(F,float)
MAKE_MAPGET0_(I,int)
MAKE_MAPGET0_(C,const char *)
MAKE_MAPGET0_(A,AstObject *)
MAKE_MAPGET0_(P,void *)
#undef MAKE_MAPGET0_


#define MAKE_MAPGET1_(X,Xtype) \
int astMapGet1##X##_( AstKeyMap *this, const char *key, int mxval, int *nval, \
                      Xtype *value, int *status ){ \
   if ( !astOK ) return 0; \
   return (**astMEMBER(this,KeyMap,MapGet1##X))(this,key,mxval,nval,value,status); \
}
MAKE_MAPGET1_(B,unsigned char)
MAKE_MAPGET1_(S,short int)
MAKE_MAPGET1_(D,double)
MAKE_MAPGET1_(F,float)
MAKE_MAPGET1_(I,int)
MAKE_MAPGET1_(A,AstObject *)
MAKE_MAPGET1_(P,void *)
#undef MAKE_MAPGET1_

#define MAKE_MAPGETELEM_(X,Xtype) \
int astMapGetElem##X##_( AstKeyMap *this, const char *key, int elem, \
                         Xtype *value, int *status ){ \
   if ( !astOK ) return 0; \
   return (**astMEMBER(this,KeyMap,MapGetElem##X))(this,key,elem,value,status); \
}
MAKE_MAPGETELEM_(B,unsigned char)
MAKE_MAPGETELEM_(S,short int)
MAKE_MAPGETELEM_(D,double)
MAKE_MAPGETELEM_(F,float)
MAKE_MAPGETELEM_(I,int)
MAKE_MAPGETELEM_(A,AstObject *)
MAKE_MAPGETELEM_(P,void *)
#undef MAKE_MAPGETELEM_

int astMapGet1C_( AstKeyMap *this, const char *key, int l, int mxval, int *nval,
                  char *value, int *status ){
   if ( !astOK ) return 0;
   return (**astMEMBER(this,KeyMap,MapGet1C))(this,key,l,mxval,nval,value,status);
}

int astMapGetElemC_( AstKeyMap *this, const char *key, int l, int elem,
                     char *value, int *status ){
   if ( !astOK ) return 0;
   return (**astMEMBER(this,KeyMap,MapGetElemC))(this,key,l,elem,value,status);
}

#define MAKE_MAPPUTELEM_(X,Xtype) \
void astMapPutElem##X##_( AstKeyMap *this, const char *key, int elem, \
                         Xtype value, int *status ){ \
   if ( !astOK ) return; \
   (**astMEMBER(this,KeyMap,MapPutElem##X))(this,key,elem,value,status); \
}
MAKE_MAPPUTELEM_(B,unsigned char)
MAKE_MAPPUTELEM_(S,short int)
MAKE_MAPPUTELEM_(D,double)
MAKE_MAPPUTELEM_(F,float)
MAKE_MAPPUTELEM_(I,int)
MAKE_MAPPUTELEM_(A,AstObject *)
MAKE_MAPPUTELEM_(C,const char *)
MAKE_MAPPUTELEM_(P,void *)
#undef MAKE_MAPPUTELEM_

void astMapPutU_( AstKeyMap *this, const char *key, const char *comment, int *status ){
   if ( !astOK ) return;
   (**astMEMBER(this,KeyMap,MapPutU))(this,key,comment,status);
}

void astMapRemove_( AstKeyMap *this, const char *key, int *status ){
   if ( !astOK ) return;
   (**astMEMBER(this,KeyMap,MapRemove))(this,key,status);
}
void astMapRename_( AstKeyMap *this, const char *oldkey, const char *newkey,
                    int *status ){
   if ( !astOK ) return;
   (**astMEMBER(this,KeyMap,MapRename))(this,oldkey,newkey,status);
}
void astMapCopy_( AstKeyMap *this, AstKeyMap *that, int *status ){
   if ( !astOK ) return;
   (**astMEMBER(this,KeyMap,MapCopy))(this,that,status);
}
int astMapDefined_( AstKeyMap *this, const char *key, int *status ){
   if ( !astOK ) return 0;
   return (**astMEMBER(this,KeyMap,MapDefined))(this,key,status);
}
int astMapSize_( AstKeyMap *this, int *status ){
   if ( !astOK ) return 0;
   return (**astMEMBER(this,KeyMap,MapSize))(this,status);
}
int astMapLenC_( AstKeyMap *this, const char *key, int *status ){
   if ( !astOK ) return 0;
   return (**astMEMBER(this,KeyMap,MapLenC))(this,key,status);
}
int astMapLength_( AstKeyMap *this, const char *key, int *status ){
   if ( !astOK ) return 0;
   return (**astMEMBER(this,KeyMap,MapLength))(this,key,status);
}
int astMapType_( AstKeyMap *this, const char *key, int *status ){
   if ( !astOK ) return 0;
   return (**astMEMBER(this,KeyMap,MapType))(this,key,status);
}
int astMapHasKey_( AstKeyMap *this, const char *key, int *status ){
   if ( !astOK ) return 0;
   return (**astMEMBER(this,KeyMap,MapHasKey))(this,key,status);
}
const char *astMapKey_( AstKeyMap *this, int index, int *status ){
   if ( !astOK ) return NULL;
   return (**astMEMBER(this,KeyMap,MapKey))(this,index,status);
}
const char *astMapIterate_( AstKeyMap *this, int reset, int *status ){
   if ( !astOK ) return NULL;
   return (**astMEMBER(this,KeyMap,MapIterate))(this,reset,status);
}
int astGetSizeGuess_( AstKeyMap *this, int *status ){
   if( !astOK ) return 0;
   return (**astMEMBER(this,KeyMap,GetSizeGuess))(this,status);
}
int astTestSizeGuess_( AstKeyMap *this, int *status ){
   if( !astOK ) return 0;
   return (**astMEMBER(this,KeyMap,TestSizeGuess))(this,status);
}
void astClearSizeGuess_( AstKeyMap *this, int *status ){
   if( !astOK ) return;
   (**astMEMBER(this,KeyMap,ClearSizeGuess))(this,status);
}
void astSetSizeGuess_( AstKeyMap *this, int sizeguess, int *status ){
   if( !astOK ) return;
   (**astMEMBER(this,KeyMap,SetSizeGuess))(this,sizeguess,status);
}

void astClearMapLocked_( AstKeyMap *this, int *status ){
   if( !astOK ) return;
   (**astMEMBER(this,KeyMap,ClearMapLocked))(this,status);
}
void astSetMapLocked_( AstKeyMap *this, int maplocked, int *status ){
   if( !astOK ) return;
   (**astMEMBER(this,KeyMap,SetMapLocked))(this,maplocked,status);
}

void astClearKeyError_( AstKeyMap *this, int *status ){
   if( !astOK ) return;
   (**astMEMBER(this,KeyMap,ClearKeyError))(this,status);
}
void astSetKeyError_( AstKeyMap *this, int keyerror, int *status ){
   if( !astOK ) return;
   (**astMEMBER(this,KeyMap,SetKeyError))(this,keyerror,status);
}

void astClearSortBy_( AstKeyMap *this, int *status ){
   if( !astOK ) return;
   (**astMEMBER(this,KeyMap,ClearSortBy))(this,status);
}
void astSetSortBy_( AstKeyMap *this, int sortby, int *status ){
   if( !astOK ) return;
   (**astMEMBER(this,KeyMap,SetSortBy))(this,sortby,status);
}

void astClearKeyCase_( AstKeyMap *this, int *status ){
   if( !astOK ) return;
   (**astMEMBER(this,KeyMap,ClearKeyCase))(this,status);
}
void astSetKeyCase_( AstKeyMap *this, int keycase, int *status ){
   if( !astOK ) return;
   (**astMEMBER(this,KeyMap,SetKeyCase))(this,keycase,status);
}

