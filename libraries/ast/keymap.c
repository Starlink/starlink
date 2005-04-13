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
*     which identify the values. The keys are strings (case-sensitive,
*     trailing spaces are ignored), and the data type of the values can be 
*     integer, floating point, character string or AST Object pointer. Each 
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
*     The KeyMap class does not define any new attributes beyond those
*     which are applicable to all Objects.

*  Functions:
c     In addition to those functions applicable to all Objects, the
c     following functions may also be applied to all KeyMaps:
f     In addition to those routines applicable to all Objects, the
f     following routines may also be applied to all KeyMaps:
*
c     - astMapGet0<X>: Get a named scalar entry from a KeyMap
c     - astMapGet1<X>: Get a named vector entry from a KeyMap
c     - astMapHasKey: Does the KeyMap contain a named entry?
c     - astMapKey: Return the key name at a given index in the KeyMap
c     - astMapLenC: Get the length of a named character entry in a KeyMap
c     - astMapLength: Get the length of a named entry in a KeyMap
c     - astMapPut0<X>: Add a new scalar entry to a KeyMap
c     - astMapPut1<X>: Add a new vector entry to a KeyMap
c     - astMapRemove: Removed a named entry from a KeyMap
c     - astMapSize: Get the number of entries in a KeyMap
c     - astMapType: Return the data type of a named entry in a map.
f     - AST_MAPGET0<X>: Get a named scalar entry from a KeyMap
f     - AST_MAPGET1<X>: Get a named vector entry from a KeyMap
f     - AST_MAPHASKEY: Does the KeyMap contain a named entry?
f     - AST_MAPKEY: Return the key name at a given index in the KeyMap
f     - AST_MAPLENC: Get the length of a named character entry in a KeyMap
f     - AST_MAPLENGTH: Get the length of a named entry in a KeyMap
f     - AST_MAPPUT0<X>: Add a new scalar entry to a KeyMap
f     - AST_MAPPUT1<X>: Add a new vector entry to a KeyMap
f     - AST_MAPREMOVE: Removed a named entry from a KeyMap
f     - AST_MAPSIZE: Get the number of entries in a KeyMap
f     - AST_MAPTYPE: Return the data type of a named entry in a map.

*  Copyright:
*     <COPYRIGHT_STATEMENT>

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
*class--
*/

/* Module Macros. */
/* ============== */
/* Set the name of the class we are implementing. This indicates to
   the header files that define class interfaces that they should make
   "protected" symbols available. */
#define astCLASS KeyMap

/* Include files. */
/* ============== */
/* Interface definitions. */
/* ---------------------- */
#include "error.h"               /* Error reporting facilities */
#include "memory.h"              /* Memory management facilities */
#include "object.h"              /* Base Object class */
#include "pointset.h"            /* For AST__BAD */
#include "channel.h"             /* I/O channels */
#include "keymap.h"              /* Interface definition for this class */

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

/* This structure is a AstMapEntry holding a scalar string */
typedef struct Entry0C {
   struct AstMapEntry entry; /* The parent Entry information */
   const char *value;        /* The string pointer */
} Entry0C;

/* This structure is a AstMapEntry holding a scalar AST Object */
typedef struct Entry0A {
   struct AstMapEntry entry; /* The parent Entry information */
   AstObject *value;         /* The Object pointer */
} Entry0A;

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

/* This structure is a AstMapEntry holding a 1D array of strings */
typedef struct Entry1C {
   struct AstMapEntry entry; /* The parent Entry information */
   const char **value;       /* The string pointers */
} Entry1C;

/* This structure is a AstMapEntry holding a 1D array of AST Objects */
typedef struct Entry1A {
   struct AstMapEntry entry; /* The parent Entry information */
   AstObject **value;        /* The Object pointers */
} Entry1A;


/* Module Variables. */
/* ================= */
/* Define the class virtual function table and its initialisation flag
   as static variables. */
static AstKeyMapVtab class_vtab; /* Virtual function table */
static int class_init = 0;       /* Virtual function table initialised? */

/* Pointers to parent class methods which are extended by this class. */

/* External Interface Function Prototypes. */
/* ======================================= */
/* The following functions have public prototypes only (i.e. no
   protected prototypes), so we must provide local prototypes for use
   within this module. */
AstKeyMap *astKeyMapId_( const char *, ... );

/* Prototypes for Private Member Functions. */
/* ======================================== */
static AstMapEntry *AddTableEntry( AstKeyMap *, int, AstMapEntry * );
static AstMapEntry *CopyMapEntry( AstMapEntry * );
static AstMapEntry *FreeMapEntry( AstMapEntry * );
static AstMapEntry *SearchTableEntry( AstKeyMap *, int, const char * );
static const char *GetKey( AstKeyMap *, int index );
static const char *MapKey( AstKeyMap *, int index );
static int ConvertValue( void *, int, void *, int );
static int HashFun( const char * );
static int KeyCmp( const char *, const char * );
static int MapGet0A( AstKeyMap *, const char *, AstObject ** );
static int MapGet0C( AstKeyMap *, const char *, const char ** );
static int MapGet0D( AstKeyMap *, const char *, double * );
static int MapGet0I( AstKeyMap *, const char *, int * );
static int MapGet1A( AstKeyMap *, const char *, int, int *, AstObject ** );
static int MapGet1C( AstKeyMap *, const char *, int, int, int *, char * );
static int MapGet1D( AstKeyMap *, const char *, int, int *, double * );
static int MapGet1I( AstKeyMap *, const char *, int, int *, int * );
static int MapHasKey( AstKeyMap *, const char *);
static int MapLenC( AstKeyMap *, const char *);
static int MapLength( AstKeyMap *, const char *);
static int MapSize( AstKeyMap *);
static int MapType( AstKeyMap *, const char *);
static size_t SizeOfEntry( AstMapEntry * );
static void Copy( const AstObject *, AstObject * );
static void CopyTableEntry( AstKeyMap *, AstKeyMap *, int );
static void Delete( AstObject * );
static void Dump( AstObject *, AstChannel * );
static void DumpEntry( AstMapEntry *, AstChannel *, int );
static void FreeTableEntry( AstKeyMap *, int itab );
static void MapPut0A( AstKeyMap *, const char *, AstObject *, const char * );
static void MapPut0C( AstKeyMap *, const char *, const char *, const char * );
static void MapPut0D( AstKeyMap *, const char *, double, const char * );
static void MapPut0I( AstKeyMap *, const char *, int, const char * );
static void MapPut1A( AstKeyMap *, const char *, int, AstObject *[], const char * );
static void MapPut1C( AstKeyMap *, const char *, int, const char *[], const char * );
static void MapPut1D( AstKeyMap *, const char *, int, double *, const char * );
static void MapPut1I( AstKeyMap *, const char *, int, int *, const char * );
static void MapRemove( AstKeyMap *, const char *);
static void RemoveTableEntry( AstKeyMap *, int, const char * );


/* Member functions. */
/* ================= */
static AstMapEntry *AddTableEntry( AstKeyMap *this, int itab, AstMapEntry *entry ){
/*
*  Name:
*     AddTableEntry

*  Purpose:
*     Add an new entry to a linked-list of KeyMap entries.

*  Type:
*     Private function.

*  Synopsis:
*     #include "keymap.h"
*     AstMapEntry *AddTableEntry( AstKeyMap *this, int itab, AstMapEntry *entry )

*  Class Membership:
*     KeyMap member function.

*  Description:
*     This function adds the supplied MapEntry to the head of the linked
*     list of MapEntries stored at the specified entry of the hash table. 

*  Parameters:
*     this
*        Pointer to the KeyMap.
*     itab
*        Index of the hash table element to be searched.
*     entry 
*        Pointer to the MapEntry to be added.

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

/* Increment the number of entriesin the hash table element. */
   this->nentry[ itab ]++;

/* Return a NULL pointer. */
   return NULL;
}

static int ConvertValue( void *raw, int raw_type, void *out, int out_type ) {
/*
*  Name:
*     ConvertValue

*  Purpose:
*     Convert a value from one KeyMap data type to another.

*  Type:
*     Private function.

*  Synopsis:
*     #include "keymap.h"
*     int ConvertValue( void *raw, int raw_type, void *out, int out_type )

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

*  Returned Value:
*     Non-zero if the conversion was performed succesfully, otherwise zero.
*     In the case of the output type being AST__STRINGTYPE, the returned
*     non-zero value will be the length of the formatted string (not including 
*     the terminating null character). This value will be returned correctly 
*     even if "out" is NULL.

*  Notes:
*     - Zero will be returned if this function is invoked with the global 
*     error status set, or if it should fail for any reason.
*/

/* Local Constants: */
#define MAX_STRINGS 50           /* Number of string values to buffer */
#define BUFF_LEN 50              /* Max. characters in result buffer */

/* Local Variables: */
   AstObject *aval;              /* AstObject pointer value */
   const char *cval;             /* Pointer to string value */ 
   const char *cvalue;           /* Pointer to output string value */ 
   double dval;                  /* Integer value */ 
   int ival;                     /* Integer value */ 
   int i;                        /* Loop count */
   int nc;                       /* Number of characters read from string */ 
   int nval;                     /* Number of values read from string */ 
   int result;                   /* Returned flag */
   static char *strings[ MAX_STRINGS ]; /* Pointers to string buffers */
   static char buff[ BUFF_LEN + 1 ]; /* Buffer for string result */
   static int init = 0;          /* "strings" array initialised? */
   static int istr = 0;          /* Offset of next string in "strings" */

#ifdef DEBUG
   int pm;     /* See astSetPermMem in memory.c */
#endif


/* Initialise. */
   result = 0;

/* Check the global error status and the supplied pointers. */
   if( !astOK || !raw ) return result;

/* If the "strings" array has not been initialised, fill it with
   NULL pointers. */
   if( !init ) {
      init = 1;
      for( i = 0; i < MAX_STRINGS; i++ ) strings[ i ] = NULL;
   }

/* Assume conversion is possible */
   result = 1;
   cvalue = NULL;

/* Consider conversion from "int". */
   if( raw_type == AST__INTTYPE ) {
      ival = *( (int *) raw );

/* Consider conversion to "int". */
      if( out_type == AST__INTTYPE ) {
         if( out ) *( (int *) out ) = ival;
         
/* Consider conversion to "double". */
      } else if( out_type == AST__DOUBLETYPE ) {
         if( out ) *( (double *) out ) = (double) ival;

/* Consider conversion to "const char *". */
      } else if( out_type == AST__STRINGTYPE ) {
         (void) sprintf( buff, "%d", ival );
         cvalue = buff;

/* Consider conversion to "AstObject *". */
      } else if( out_type == AST__OBJECTTYPE ) {
         result = 0;

/* Report an error if the data type is unknown. */
      } else {
         result = 0;
         astError( AST__INTER, "ConvertValue(KeyMap): Illegal map entry data "
                   "type %d encountered (internal AST programming error).",
                   out_type );
      }

/* Consider conversion from "double". */
   } else if( raw_type == AST__DOUBLETYPE ) {
      dval = *( (double *) raw );

/* Consider conversion to "int". */
      if( out_type == AST__INTTYPE ) {
         if( out ) *( (int *) out ) = (int)( dval + 0.5 );
         
/* Consider conversion to "double". */
      } else if( out_type == AST__DOUBLETYPE ) {
         if( out ) *( (double *) out ) = dval;

/* Consider conversion to "const char *". */
      } else if( out_type == AST__STRINGTYPE ) {
         (void) sprintf( buff, "%.*g", DBL_DIG, dval );
         cvalue = buff;

/* Consider conversion to "AstObject *". */
      } else if( out_type == AST__OBJECTTYPE ) {
         result = 0;

/* Report an error if the data type is unknown. */
      } else {
         result = 0;
         astError( AST__INTER, "ConvertValue(KeyMap): Illegal map entry data "
                   "type %d encountered (internal AST programming error).",
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
            result = 0;
         }

/* Consider conversion to "double". */
      } else if( out_type == AST__DOUBLETYPE ) {
         nc = 0;
         nval = astSscanf( cval, " %lf %n", &dval, &nc );
         if( ( nval == 1 ) && ( nc >= (int) strlen( cval ) ) ) {
            if( out ) *( (double *) out ) = dval;
         } else {
            result = 0;
         }

/* Consider conversion to "const char *". */
      } else if( out_type == AST__STRINGTYPE ) {
         cvalue = cval;

/* Consider conversion to "AstObject *". */
      } else if( out_type == AST__OBJECTTYPE ) {
         result = 0;

/* Report an error if the data type is unknown. */
      } else {
         result = 0;
         astError( AST__INTER, "ConvertValue(KeyMap): Illegal map entry data "
                   "type %d encountered (internal AST programming error).",
                   out_type );
      }

/* Consider conversion from "AstObject *". */
   } else if( raw_type == AST__OBJECTTYPE ) {

/* Consider conversion to "int". */
      if( out_type == AST__INTTYPE ) {
         result = 0;

/* Consider conversion to "double". */
      } else if( out_type == AST__DOUBLETYPE ) {
         result = 0;

/* Consider conversion to "const char *". */
      } else if( out_type == AST__STRINGTYPE ) {
         result = 0;

/* Consider conversion to "AstObject *". */
      } else if( out_type == AST__OBJECTTYPE ) {
         aval = *( (AstObject **) raw );
         if( out ) *( (AstObject **) out ) = aval ? astClone( aval ) : NULL;

/* Report an error if the data type is unknown. */
      } else {
         result = 0;
         astError( AST__INTER, "ConvertValue(KeyMap): Illegal map entry data "
                   "type %d encountered (internal AST programming error).",
                   out_type );
      }

/* Report an error if the data type is unknown. */
   } else {
      result = 0;
      astError( AST__INTER, "ConvertValue(KeyMap): Illegal map entry data "
                "type %d encountered (internal AST programming error).",
                raw_type );
   }

/* If the output is a string, store a copy of the resulting string in 
   dynamically allocated memory, putting a pointer to the copy into the next
   element of the "strings" array.  (This process also de-allocates
   any previously allocated memory pointed at by this "strings"
   element, so the earlier string is effectively replaced by the new
   one.) */
   if( out_type == AST__STRINGTYPE && astOK && result && cvalue ) {
      result = strlen( cvalue );

#ifdef DEBUG
   pm = astSetPermMem( 1 );
#endif

      strings[ istr ] = astStore( strings[ istr ], cvalue, 
                                  (size_t) ( result + 1 ) );

#ifdef DEBUG
   astSetPermMem( pm );
#endif

/* If OK, return a pointer to the copy and increment "istr" to use the
   next element of "strings" on the next invocation. Recycle "istr" to
   zero when all elements have been used. */
      if ( astOK ) {
         if( out ) *( (const char **) out ) = strings[ istr++ ];
         if( istr == ( MAX_STRINGS - 1 ) ) istr = 0;
      }
   }

/* If an error has occurred, return zero. */
   if( !astOK ) result = 0;

/* Return the result. */
   return result;
}

/* Undefine macros local to this function. */
#undef MAX_STRINGS

static AstMapEntry *CopyMapEntry( AstMapEntry *in ){
/*
*  Name:
*     CopyMapEntry

*  Purpose:
*     Produces a copy of the supplied KeyMap entry.

*  Type:
*     Private function.

*  Synopsis:
*     #include "keymap.h"
*     AstMapEntry *CopyMapEntry( AstMapEntry *in )

*  Class Membership:
*     KeyMap member function.

*  Description:
*     This function creates a deep copy of the supplied KeyMap entry.

*  Parameters:
*     in
*        Pointer to the MapEntry to be copied. NULL may be supplied in
*        which case NULL will be returned.

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
   size = SizeOfEntry( in );
   nel = in->nel;
   type = in->type;

/* Do a byte-for-byte copy of the supplied MapEntry. */
   result = astStore( NULL, in, size );

/* Copy or nullify pointers in the AstMapEntry structure. */
   result->next = NULL;
   text = in->key;
   result->key = text ? astStore( NULL, text, strlen( text ) + 1 ) : NULL;
   text = in->comment;
   result->comment = text ? astStore( NULL, text, strlen( text ) + 1 ) : NULL;

/* First deal with string entries. */
   if( type == AST__STRINGTYPE ) {

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
      } else {
         alist = astMalloc( sizeof(AstObject *)*(size_t)nel );
         ( (Entry1A *) result )->value = alist;
         if( alist ) {
            for( i = 0; i < nel; i++ ) {
               obj = ( (Entry1A *) in )->value[ i ];
               alist[ i ] = obj ? astCopy( obj ) : NULL;
            }
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

/* Similarly deal with floating point entries. */
   } else if( type == AST__DOUBLETYPE ) {
      if( nel > 0 ) {
         ( (Entry1D *) result )->value = astStore( NULL, 
                                                  ( (Entry1D *) in )->value,
                                                  sizeof( double )*(size_t)nel );
      }

/* Report an error if the data type is unknown. */
   } else {
      astError( AST__INTER, "CopyMapEntry(KeyMap): Illegal map entry data "
                "type %d encountered (internal AST programming error).",
                type );
   }

/* If an error has occurred, attempt to delete the returned MapEntry. */
   if( !astOK ) result = FreeMapEntry( result );

/* Return the result. */
   return result;
}

static void CopyTableEntry( AstKeyMap *in, AstKeyMap *out, int itab ){
/*
*  Name:
*     CopyTableEntry

*  Purpose:
*     Produces a deep copy of a hash table element.

*  Type:
*     Private function.

*  Synopsis:
*     #include "keymap.h"
*     void CopyTableEntry( AstKeyMap *in, AstKeyMap *out, int itab )

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

/* Loop round until we have copied all entries. */
   while( next ) {

/* Copy the next entry, storing the resulting pointer at the position
   indicated by "link". */
      *link = CopyMapEntry( next );

/* Update "link" and "next" */
      next = next->next;
      link = &( (*link)->next );
   }

/* Set the number of entries in the output to be the same as the input. */
   out->nentry[ itab ] = in->nentry[ itab ];

/* If an error has occurred, attempt to delete the returned MapEntry. */
   if( !astOK ) FreeTableEntry( out, itab );
}

static void DumpEntry( AstMapEntry *entry, AstChannel *channel, int nentry ) {
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

   } else if( type == AST__DOUBLETYPE ) {
      com = "Item data type (double)";

   } else {
      com = "";
      astError( AST__INTER, "DumpEntry(KeyMap): Illegal map entry data "
                "type %d encountered (internal AST programming error).",
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

/* Report an error if the data type is unknown. */
   } else if( astOK ) {
      astError( AST__INTER, "DumpEntry(KeyMap): Illegal map entry data "
                "type %d encountered (internal AST programming error).",
                type );
   }
}

static AstMapEntry *FreeMapEntry( AstMapEntry *in ){
/*
*  Name:
*     FreeMapEntry

*  Purpose:
*     Frees the supplied KeyMap entry.

*  Type:
*     Private function.

*  Synopsis:
*     #include "keymap.h"
*     AstMapEntry *FreeMapEntry( AstMapEntry *in )

*  Class Membership:
*     KeyMap member function.

*  Description:
*     This function frees resources used by the supplied MapEntry, then
*     frees the MapEntry structure itself and returns a NULL pointer.

*  Parameters:
*     in
*        Pointer to the MapEntry to be freed. NULL may be supplied in
*        which the function returns without action.

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
      } else {
         alist = ( (Entry1A *) in )->value;
         if( alist ) {
            for( i = 0; i < nel; i++ ) {
               if( alist[ i ] ) alist[ i ] = astAnnul( alist[ i ] );
            }
            ( (Entry1A *) in )->value = astFree( alist );
         }
      }

/* Now deal with integer entries. Scalar entries do not need any further 
   action. If this is a vector entry free the values array. */
   } else if( type == AST__INTTYPE ) {
      if( nel > 0 ) ( (Entry1I *) in )->value = astFree( ( (Entry1I *) in )->value );

/* Similarly deal with floating point entries. */
   } else if( type == AST__DOUBLETYPE ) {
      if( nel > 0 ) ( (Entry1D *) in )->value = astFree( ( (Entry1D *) in )->value );

/* Report an error if the data type is unknown. */
   } else {
      astError( AST__INTER, "FreeMapEntry(KeyMap): Illegal map entry data "
                "type %d encountered (internal AST programming error).",
                type );
   }

/* Free or nullify pointers in the AstMapEntry structure. */
   in->next = NULL;
   in->key = astFree( (void *) in->key );
   in->comment = astFree( (void *) in->comment );

/* Free the complete AstMapEntry structure. */
   astFree( in );

/* Return a NULL pointer. */
   return NULL;
}

static void FreeTableEntry( AstKeyMap *this, int itab ){
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
*     void FreeTableEntry( AstKeyMap *this, int itab )

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

*  Notes:
*     - This function attempts to execute even if it is invoked with the 
*     global error status set.
*/

/* Local Variables: */
   AstMapEntry *link;     /* Pointer the next but one MapEntry to be freed */
   AstMapEntry *next;     /* Pointer the next MapEntry to be freed */

/* Check it is safe to proceed. */
   if( this && itab >= 0 && itab < AST__MAPSIZE ) {

/* Store a pointer to the MapEntry which is to be freed next. */
      next = this->table[ itab ];

/* Loop round freeing all MapEntries in the linked list. */
      while( next ) {

/* Store a pointer to the MapEntry which will be freed after this one. */
         link = next->next;

/* Free this MapEntry */
         FreeMapEntry( next );

/* Set up the next MapEntry to be freed. */
         next = link;
      }

/* Store a NULL pointer in the table element. */
      this->table[ itab ] = NULL;

/* Sets the number of entries in this hash table element to zero. */
      this->nentry[ itab ] = 0;
   }
}

static const char *GetKey( AstKeyMap *this, int index ) {
/*
*  Name:
*     GetKey

*  Purpose:
*     Get the key at a given index within the KeyMap.

*  Type:
*     Private member function.

*  Synopsis:
*     #include "keymap.h"
*     const char *GetKey( AstKeyMap *this, int index )

*  Class Membership:
*     KeyMap method.

*  Description:
*     This function returns a string holding the key for the entry with
*     the given index within the KeyMap.

*  Parameters:
*     this
*        Pointer to the KeyMap.
*     index
*        The index into the KeyMap. The first entry has index zero, and the last 
*        has index "size-1", where "size" is the value returned by the 
*        astMapSize function. An error is reported if the supplied index is
*        out of bounds.

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

/* Initialise. */
   result = NULL;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Loop round each entry in the hash table. */
   ilast = -1;
   for( itab = 0; itab < AST__MAPSIZE; itab++ ) {

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

/* Report an error if the element was not found. */
   if( !result && astOK ) {
      astError( AST__MPIND, "astMapKey(%s): Cannot find element "
                "%d (zero-based) of the %s.", astGetClass( this ), 
                index, astGetClass( this ) );
   } 

/* Return the result.*/
   return result;
}

static int HashFun( const char *key ){
/*
*  Name:
*     HashFun

*  Purpose:
*     Returns an integer hash code for a string

*  Type:
*     Private function.

*  Synopsis:
*     #include "keymap.h"
*     int HashFun( const char *key )

*  Class Membership:
*     KeyMap member function.

*  Description:
*     This function returns an integer hash code for the supplied string,

*  Parameters:
*     key
*        Pointer to the string. Trailing spaces are ignored.

*  Returned Value:
*     An integer in the range zero to ( AST__MAPSIZE - 1 ).

*  Notes:
*     - A value of zero is returned if this function is invoked with the 
*     global error status set.
*/

/* Local Variables: */
   unsigned long hash;  
   int c;            

/* Check the local error status. */
   if ( !astOK ) return 0;

/* djb2: This hash function was first reported by Dan Bernstein many years 
   ago in comp.lang.c Each through the "hile" loop corresponds to 
   "hash = hash*33 + c ". Ignore spaces so that trailing spaces used to
   pad F77 character variables will be ignored. */   
   hash = 5381;
   while( (c = *key++) ) {
      if( c != ' ' ) {     
         hash = ((hash << 5) + hash) + c;
      }
   }
   return ( hash % AST__MAPSIZE );
}

void astInitKeyMapVtab_(  AstKeyMapVtab *vtab, const char *name ) {
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

/* Check the local error status. */
   if ( !astOK ) return;

/* Initialize the component of the virtual function table used by the
   parent class. */
   astInitObjectVtab( (AstObjectVtab *) vtab, name );

/* Store a unique "magic" value in the virtual function table. This
   will be used (by astIsAKeyMap) to determine if an object belongs
   to this class.  We can conveniently use the address of the (static)
   class_init variable to generate this unique value. */
   vtab->check = &class_init;

/* Initialise member function pointers. */
/* ------------------------------------ */
/* Store pointers to the member functions (implemented here) that provide
   virtual methods for this class. */
   vtab->MapPut0A = MapPut0A;
   vtab->MapPut0C = MapPut0C;
   vtab->MapPut0D = MapPut0D;
   vtab->MapPut0I = MapPut0I;
   vtab->MapPut1A = MapPut1A;
   vtab->MapPut1C = MapPut1C;
   vtab->MapPut1D = MapPut1D;
   vtab->MapPut1I = MapPut1I;
   vtab->MapGet0A = MapGet0A;
   vtab->MapGet0C = MapGet0C;
   vtab->MapGet0D = MapGet0D;
   vtab->MapGet0I = MapGet0I;
   vtab->MapGet1A = MapGet1A;
   vtab->MapGet1C = MapGet1C;
   vtab->MapGet1D = MapGet1D;
   vtab->MapGet1I = MapGet1I;
   vtab->MapRemove = MapRemove;
   vtab->MapSize = MapSize;
   vtab->MapLenC = MapLenC;
   vtab->MapLength = MapLength;
   vtab->MapType = MapType;
   vtab->MapHasKey = MapHasKey;
   vtab->MapKey = MapKey;

/* Save the inherited pointers to methods that will be extended, and
   replace them with pointers to the new member functions. */

/* Store replacement pointers for methods which will be over-ridden by
   new member functions implemented here. */

/* Declare the destructor, copy constructor and dump function. */
   astSetDelete( vtab, Delete );
   astSetCopy( vtab, Copy );
   astSetDump( vtab, Dump, "KeyMap", "Map of key/value pairs" );
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

*  Notes:
*     - Zero will be returned if this function is invoked with the global 
*     error status set, or if it should fail for any reason.
*/

/* Local Variables: */
   const char *k1;               /* Pointer to next "key1" character */ 
   const char *k2;               /* Pointer to next "key2" character */ 
   int result;                   /* Returned flag */

/* Check the global error status. */
   if ( !astOK ) return 0;

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
         result = 1;
      } else {
         while( *k1 == ' ' ) k1++;
         result = ( *k1 == 0 ) ? 0 : 1;
      }
   } else {
      if( *k2 ) {
         while( *k2 == ' ' ) k2++;
         result = ( *k2 == 0 ) ? 0 : 1;
      } else {
         result = 0;
      }
   }

/* Return the result. */
   return result;
}

static const char *MapKey( AstKeyMap *this, int index ) {
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
*     The index associated with a given entry is not, in general, related to 
*     the order in which the entries are added to the KeyMap, and may change
*     if other entries are added to or removed from the KeyMap. 

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

/* Local Constants: */
#define MAX_STRINGS 50           /* Number of string values to buffer */

/* Local Variables: */
   const char *result;           /* Pointer value to return */
   const char *value;            /* Pointer to key value */
   int i;                        /* Loop counter for initialisation */
   static char *strings[ MAX_STRINGS ]; /* Pointers to string buffers */
   static int init = 0;          /* "strings" array initialised? */
   static int istr = 0;          /* Offset of next string in "strings" */

#ifdef DEBUG
   int pm;     /* See astSetPermMem in memory.c */
#endif

/* Initialise. */
   result = NULL;

/* Check the global error status. */
   if ( !astOK ) return result;

/* If the "strings" array has not been initialised, fill it with
   NULL pointers. */
   if ( !init ) {
      init = 1;
      for ( i = 0; i < MAX_STRINGS; i++ ) strings[ i ] = NULL;
   }

/* Obtain a pointer to the required key value. */
   value = GetKey( this, index );

/* If OK, store a copy of the resulting string in dynamically
   allocated memory, putting a pointer to the copy into the next
   element of the "strings" array.  (This process also de-allocates
   any previously allocated memory pointed at by this "strings"
   element, so the earlier string is effectively replaced by the new
   one.) */
   if ( astOK ) {

#ifdef DEBUG
   pm = astSetPermMem( 1 );
#endif
      strings[ istr ] = astStore( strings[ istr ], value,
                                  strlen( value ) + (size_t) 1 );
#ifdef DEBUG
   astSetPermMem( pm );
#endif

/* If OK, return a pointer to the copy and increment "istr" to use the
   next element of "strings" on the next invocation. Recycle "istr" to
   zero when all elements have been used. */
      if ( astOK ) {
         result = strings[ istr++ ];
         if ( istr == ( MAX_STRINGS - 1 ) ) istr = 0;
      }
   }

/* Return the result. */
   return result;

/* Undefine macros local to this function. */
#undef MAX_STRINGS
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
*     - Key names are case sensitive, and white space is considered
*     significant.
*     - If the supplied key is already in use in the KeyMap, the new value
*     will replace the old value.

*  Data Type Codes:
*     To select the appropriate 
c     function, you should replace <X> in the generic function name astMapPut0<X>
f     routine, you should replace <X> in the generic routine name AST_MAPPUT0<X>
*     with a 1-character data type code, so as to match the data type <X>type
*     of the data you are processing, as follows:
c     - D: double
c     - I: int
c     - C: "const" pointer to null terminated character string
c     - A: Pointer to AstObject
f     - D: DOUBLE PRECISION
f     - I: INTEGER
f     - C: CHARACTER
f     - A: INTEGER used to identify an AstObject
*
c     For example, astMapPut0D would be used to store a "double" value,
c     while astMapPut0I would be used to store an "int", etc.
f     For example, AST_MAPPUT0D would be used to store a DOUBLE PRECISION value,
f     while AST_MAPPUT0I would be used to store an INTEGER, etc.
*--
*/
/* Define a macro to implement the function for a specific data type. */
#define MAKE_MAPPUT0(X,Xtype,Itype,ValExp) \
static void MapPut0##X( AstKeyMap *this, const char *key, Xtype value, \
                        const char *comment ) { \
\
/* Local Variables: */ \
   AstMapEntry *mapentry;  /* Pointer to parent MapEntry structure */ \
   Entry0##X *entry;       /* Structure holding the data for the new Entry */ \
   char *p;                /* Pointer to next key character */ \
   int itab;               /* Index of hash table element to use */ \
   int keylen;             /* Length of supplied key string */ \
\
/* Check the global error status. */ \
   if ( !astOK ) return; \
\
/* Allocate memory for the new MapEntry. */ \
   entry = astMalloc( sizeof( Entry0##X ) ); \
   if( astOK ) { \
\
/* Initialise pointers in the new structure.*/ \
      mapentry = (AstMapEntry *) entry; \
      mapentry->next = NULL; \
      mapentry->key = NULL; \
      mapentry->comment = NULL; \
\
/* Now store the new values. */ \
      keylen = strlen( key ); \
      mapentry->key = astStore( NULL, key, keylen + 1 ); \
      if( comment ) mapentry->comment = astStore( NULL, comment, strlen( comment ) + 1 ); \
      mapentry->type = Itype; \
      mapentry->nel = 0; \
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
      itab = HashFun( mapentry->key ); \
\
/* Remove any existing entry with the given key from the table element. */ \
      RemoveTableEntry( this, itab, mapentry->key ); \
\
/* If all has gone OK, store the new entry at the head of the linked list \
   associated with the selected table entry. */ \
      if( astOK ) { \
         mapentry = AddTableEntry( this, itab, mapentry ); \
\
/* If anything went wrong, try to delete the new entry. */ \
      } else { \
         mapentry = FreeMapEntry( mapentry ); \
      } \
   } \
} 

/* Expand the above macro to generate a function for each required
   data type. */
MAKE_MAPPUT0(I,int,AST__INTTYPE,value)
MAKE_MAPPUT0(D,double,AST__DOUBLETYPE,value)
MAKE_MAPPUT0(C,const char *,AST__STRINGTYPE,astStore(NULL,value,strlen(value)+1))
MAKE_MAPPUT0(A,AstObject *,AST__OBJECTTYPE,(value?astClone(value):NULL))

/* Undefine the macro. */
#undef MAKE_MAPPUT0

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
c     void astMapPut1<X>( AstKeyMap *this, const char *key, int size, <X>type value[], 
c                         const char *comment );
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
*     - Key names are case sensitive, and white space is considered
*     significant.

*  Data Type Codes:
*     To select the appropriate 
c     function, you should replace <X> in the generic function name astMapPut1<X>
f     routine, you should replace <X> in the generic routine name AST_MAPPUT1<X>
*     with a 1-character data type code, so as to match the data type <X>type 
*     of the data you are processing, as follows:
c     - D: double
c     - I: int
c     - C: "const" pointer to null terminated character string
c     - A: Pointer to AstObject
f     - D: DOUBLE PRECISION
f     - I: INTEGER
f     - C: CHARACTER
f     - A: INTEGER used to identify an AstObject
*
c     For example, astMapPut1D would be used to store "double" values,
c     while astMapPut1I would be used to store "int", etc.
f     For example, AST_MAPPUT1D would be used to store DOUBLE PRECISION values,
f     while AST_MAPPUT1I would be used to store INTEGER, etc.
*--
*/
/* Define a macro to implement the function for a specific data type. */
#define MAKE_MAPPUT1(X,Xtype,Itype,ValExp) \
static void MapPut1##X( AstKeyMap *this, const char *key, int size, Xtype value[], \
                        const char *comment ) { \
\
/* Local Variables: */ \
   AstMapEntry *mapentry;  /* Pointer to parent MapEntry structure */ \
   Entry1##X *entry;       /* Structure holding the data for the new Entry */ \
   char *p;                /* Pointer to next key character */ \
   int itab;               /* Index of hash table element to use */ \
   int i;                  /* Loop count */ \
   int keylen;             /* Length of supplied key string */ \
\
/* Check the global error status. */ \
   if ( !astOK ) return; \
\
/* Allocate memory for the new MapEntry. */ \
   entry = astMalloc( sizeof( Entry1##X ) ); \
   if( astOK ) { \
\
/* Initialise pointers in the new structure.*/ \
      mapentry = (AstMapEntry *) entry; \
      mapentry->next = NULL; \
      mapentry->key = NULL; \
      mapentry->comment = NULL; \
\
/* Now store the new values. */ \
      keylen = strlen( key ); \
      mapentry->key = astStore( NULL, key, keylen + 1 ); \
      if( comment ) mapentry->comment = astStore( NULL, comment, strlen( comment ) + 1 ); \
      mapentry->type = Itype; \
      mapentry->nel = size; \
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
      itab = HashFun( mapentry->key ); \
\
/* Remove any existing entry with the given key from the table element. */ \
      RemoveTableEntry( this, itab, mapentry->key ); \
\
/* If all has gone OK, store the new entry at the head of the linked list \
   associated with the selected table entry. */ \
      if( astOK ) { \
         mapentry = AddTableEntry( this, itab, mapentry ); \
\
/* If anything went wrong, try to delete the new entry. */ \
      } else { \
         mapentry = FreeMapEntry( mapentry ); \
      } \
   } \
} 

/* Expand the above macro to generate a function for each required
   data type. */
MAKE_MAPPUT1(D,double,AST__DOUBLETYPE,value[i])
MAKE_MAPPUT1(I,int,AST__INTTYPE,value[i])
MAKE_MAPPUT1(C,const char *,AST__STRINGTYPE,astStore(NULL,value[i],strlen(value[i])+1))
MAKE_MAPPUT1(A,AstObject *,AST__OBJECTTYPE,(value[i]?astClone(value[i]):NULL))

/* Undefine the macro. */
#undef MAKE_MAPPUT1

void astMapPut1AId_( AstKeyMap *this, const char *key, int size, AstObject *value[],
                     const char *comment ) {
/*
*  Name:
*     astMapPut1AId_

*  Purpose:
*     Add a vector of AstObject pointers to a KeyMap.

*  Type:
*     Private function.

*  Synopsis:
*     #include "ast.h"
*     void astMapPut1A( AstKeyMap *this, const char *key, int size, AstObject *value[], 
*                       const char *comment )

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
   AstObject *op;          /* Object pointer */
   AstMapEntry *mapentry;  /* Pointer to parent MapEntry structure */
   Entry1A *entry;         /* Structure holding the data for the new Entry */
   char *p;                /* Pointer to next key character */
   int itab;               /* Index of hash table element to use */
   int i;                  /* Loop count */
   int keylen;             /* Length of supplied key string */

/* Check the global error status. */
   if ( !astOK ) return;

/* Allocate memory for the new MapEntry. */
   entry = astMalloc( sizeof( Entry1A ) );
   if( astOK ) {

/* Initialise pointers in the new structure.*/
      mapentry = (AstMapEntry *) entry;
      mapentry->next = NULL;
      mapentry->key = NULL;
      mapentry->comment = NULL;

/* Now store the new values. */
      keylen = strlen( key );
      mapentry->key = astStore( NULL, key, keylen + 1 );
      if( comment ) mapentry->comment = astStore( NULL, comment, strlen( comment ) + 1 );
      mapentry->type = AST__OBJECTTYPE;
      mapentry->nel = size;
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
      itab = HashFun( mapentry->key );

/* Remove any existing entry with the given key from the table element. */
      RemoveTableEntry( this, itab, mapentry->key );

/* If all has gone OK, store the new entry at the head of the linked list
   associated with the selected table entry. */
      if( astOK ) {
         mapentry = AddTableEntry( this, itab, mapentry );

/* If anything went wrong, try to delete the new entry. */
      } else {
         mapentry = FreeMapEntry( mapentry );
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
*        spaces are ignored.
c     value
f     VALUE = <X>type (Returned)
c        A pointer to a buffer in which to return the requested value. 
f        The requested value. 
*        If the requested key is not found, then the contents of the
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
f        A flag which is set to
c        a non-zero value
f        .TRUE.
*        if the requested key name was found, and is set to
c        zero
f        .FALSE.
*        otherwise.

*  Notes:
*     - No error is reported if the requested key cannot be found in the
*     given KeyMap, but a
c     zero
f     .FALSE.
*     value will be returned as the function value. The supplied buffer
*     will be returned unchanged.
*     - Key names are case sensitive, and white space is considered
*     significant.
*     - If the stored value is a vector value, then the first value in
*     the vector will be returned.
c     - A string pointer returned by astMapGet0C is guaranteed to remain valid
c     and the string to which it points will not be over-written for a
c     total of 50 successive invocations of this function. After this,
c     the memory containing the string may be re-used, so a copy of
c     the string should be made if it is needed for longer than this.

*  Data Type Codes:
*     To select the appropriate 
c     function, you should replace <X> in the generic function name astMapGet0<X>
f     routine, you should replace <X> in the generic routine name AST_MAPGET0<X>
*     with a 1-character data type code, so as to match the data type <X>type 
*     of the data you are processing, as follows:
c     - D: double
c     - I: int
c     - C: "const" pointer to null terminated character string
c     - A: Pointer to AstObject
f     - D: DOUBLE PRECISION
f     - I: INTEGER
f     - C: CHARACTER
f     - A: INTEGER used to identify an AstObject
*
c     For example, astMapGet0D would be used to get a "double" value,
c     while astMapGet0I would be used to get an "int", etc.
f     For example, AST_MAPGET0D would be used to get a DOUBLE PRECISION value,
f     while AST_MAPGET0I would be used to get an INTEGER, etc.
*--
*/
/* Define a macro to implement the function for a specific data type. */
#define MAKE_MAPGET0(X,Xtype,Itype) \
static int MapGet0##X( AstKeyMap *this, const char *key, Xtype *value ) { \
\
/* Local Variables: */ \
   AstMapEntry *mapentry;  /* Pointer to parent MapEntry structure */ \
   int itab;               /* Index of hash table element to use */ \
   int result;             /* Returned flag */ \
   int raw_type;           /* Data type of stored value */ \
   void *raw;              /* Pointer to stored value */ \
\
/* Initialise */ \
   result = 0; \
\
/* Check the global error status. */ \
   if ( !astOK ) return result; \
\
/* Use the hash function to determine the element of the hash table in \
   which the key will be stored. */ \
   itab = HashFun( key ); \
\
/* Search the relevent table entry for the required MapEntry. */ \
   mapentry = SearchTableEntry( this, itab, key ); \
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
      } else if( raw_type == AST__DOUBLETYPE ){ \
         if( mapentry->nel == 0 ) { \
            raw = &( ((Entry0D *)mapentry)->value ); \
         } else { \
            raw = ((Entry1D *)mapentry)->value; \
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
      } else { \
         raw = NULL; \
         astError( AST__INTER, "astMapGet0<X>(KeyMap): Illegal map entry data " \
                   "type %d encountered (internal AST programming error).", \
                   raw_type ); \
      } \
\
/* Convert the value, storing the result the supplied buffer. Report an \
   error if conversion is not possible. */ \
      if( !ConvertValue( raw, raw_type, value, Itype ) && astOK ){ \
         astError( AST__MPGER, "astMapGet0" #X "(%s): The value of KeyMap key " \
                   "\"%s\" cannot be read using the requested data " \
                   "type.",astGetClass( this ), key ); \
      } else { \
         result = 1; \
      } \
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
MAKE_MAPGET0(C,const char *,AST__STRINGTYPE)
MAKE_MAPGET0(A,AstObject *,AST__OBJECTTYPE)

/* Undefine the macro. */
#undef MAKE_MAPGET0

int astMapGet0AId_( AstKeyMap *this, const char *key, AstObject **value ) {
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
   int itab;               /* Index of hash table element to use */
   int result;             /* Returned flag */
   int raw_type;           /* Data type of stored value */
   void *raw;              /* Pointer to stored value */

/* Initialise */
   result = 0;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Use the hash function to determine the element of the hash table in
   which the key will be stored. */
   itab = HashFun( key );

/* Search the relevent table entry for the required MapEntry. */
   mapentry = SearchTableEntry( this, itab, key );

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

      } else if( raw_type == AST__DOUBLETYPE ){
         if( mapentry->nel == 0 ) {
            raw = &( ((Entry0D *)mapentry)->value );
         } else {
            raw = ((Entry1D *)mapentry)->value;
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

      } else {
         raw = NULL;
         astError( AST__INTER, "astMapGet0<X>(KeyMap): Illegal map entry data "
                   "type %d encountered (internal AST programming error).",
                   raw_type );
      }

/* Convert the value, storing the result the supplied buffer. Report an
   error if conversion is not possible. */
      if( !ConvertValue( raw, raw_type, value, AST__OBJECTTYPE ) && astOK ){
         astError( AST__MPGER, "astMapGet0A(%s): The value of KeyMap key "
                   "\"%s\" cannot be read using the requested data "
                   "type.", astGetClass( this ), key );
      } else {
         result = 1;
      }
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
*        If the requested key is not found, then the contents of the
*        buffer on entry to this function will be unchanged on exit.
f     STATUS = INTEGER (Given and Returned)
f        The global status.

*  Returned Value:
c     astMapGet1<X>()
f     AST_MAPGET1<X> = LOGICAL
f        A flag which is set to
c        a non-zero value
f        .TRUE.
*        if the requested key name was found, and is set to
c        zero
f        .FALSE.
*        otherwise.

*  Notes:
*     - No error is reported if the requested key cannot be found in the
*     given KeyMap, but a
c     zero
f     .FALSE.
*     value will be returned as the function value. The supplied array
*     will be returned unchanged.
*     - Key names are case sensitive, and white space is considered
*     significant.
*     - If the stored value is a scalar value, then the value will be
*     returned in the first element of the supplied array, and 
c     "nval"
f     NVAL
*     will be returned set to 1.
c     - A string pointer returned by astMapGet0C is guaranteed to remain valid
c     and the string to which it points will not be over-written for a
c     total of 50 successive invocations of this function. After this,
c     the memory containing the string may be re-used, so a copy of
c     the string should be made if it is needed for longer than this.

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
c     - I: int
c     - C: "const" pointer to null terminated character string
c     - A: Pointer to AstObject
f     - D: DOUBLE PRECISION
f     - I: INTEGER
f     - C: CHARACTER
f     - A: INTEGER used to identify an AstObject
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
static int MapGet1##X( AstKeyMap *this, const char *key, int mxval, int *nval, Xtype *value ) { \
\
/* Local Variables: */ \
   AstMapEntry *mapentry;  /* Pointer to parent MapEntry structure */ \
   int i;                  /* Element index */ \
   int itab;               /* Index of hash table element to use */ \
   int nel;                /* Number of elements in raw vector */ \
   int result;             /* Returned flag */ \
   int raw_type;           /* Data type of stored value */ \
   size_t raw_size;        /* Size of a single raw value */ \
   void *raw;              /* Pointer to stored value */ \
\
/* Initialise */ \
   result = 0; \
   *nval = 0; \
\
/* Check the global error status. */ \
   if ( !astOK ) return result; \
\
/* Use the hash function to determine the element of the hash table in \
   which the key will be stored. */ \
   itab = HashFun( key ); \
\
/* Search the relevent table entry for the required MapEntry. */ \
   mapentry = SearchTableEntry( this, itab, key ); \
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
      } else { \
         raw_size = 0; \
         raw = NULL; \
         astError( AST__INTER, "astMapGet1<X>(KeyMap): Illegal map entry data " \
                   "type %d encountered (internal AST programming error).", \
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
         if( !ConvertValue( raw, raw_type, value + i, Itype ) && astOK ){ \
            astError( AST__MPGER, "astMapGet0" #X "(%s): The value of " \
                      "element %d of KeyMap key \"%s\" cannot be read using " \
                      "the requested data type.",astGetClass( this ), i + 1, key ); \
         } \
\
/* Increment the pointers to the next raw value. */ \
         raw = (char *) raw + raw_size; \
      } \
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
MAKE_MAPGET1(A,AstObject *,AST__OBJECTTYPE)

/* Undefine the macro. */
#undef MAKE_MAPGET1


static int MapGet1C( AstKeyMap *this, const char *key, int l, int mxval, 
                     int *nval, char *value ) { 
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
*                   int *nval, char *value )

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
   const char *cvalue;     /* Pointer to converted string */ 
   int i;                  /* Element index */
   int itab;               /* Index of hash table element to use */
   int nel;                /* Number of elements in raw vector */ 
   int raw_type;           /* Data type of stored value */
   int result;             /* Returned flag */
   size_t raw_size;        /* Size of a single raw value */
   void *raw;              /* Pointer to stored value */ 

/* Initialise */ 
   result = 0; 
   *nval = 0; 

/* Check the global error status. */ 
   if ( !astOK ) return result; 

/* Use the hash function to determine the element of the hash table in 
   which the key will be stored. */ 
   itab = HashFun( key ); 

/* Search the relevent table entry for the required MapEntry. */ 
   mapentry = SearchTableEntry( this, itab, key ); 

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

      } else { 
         raw_size = 0; 
         raw = NULL; 
         astError( AST__INTER, "astMapGet1C(KeyMap): Illegal map entry data " 
                   "type %d encountered (internal AST programming error).", 
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
         if( !ConvertValue( raw, raw_type, &cvalue, AST__STRINGTYPE ) && astOK ){ 
            astError( AST__MPGER, "astMapGet0C(%s): The value of " 
                      "element %d of KeyMap key \"%s\" cannot be read using " 
                      "the requested data type.",astGetClass( this ), i + 1, key ); 

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
   } 

/* If an error occurred,return zero. */ 
   if( !astOK ) result = 0; 

/* Return the result.*/ 
   return result; 
}

int astMapGet1AId_( AstKeyMap *this, const char *key, int mxval, int *nval, 
                    AstObject **value ) { 
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
   int i;                  /* Element index */
   int itab;               /* Index of hash table element to use */
   int nel;                /* Number of elements in raw vector */ 
   int raw_type;           /* Data type of stored value */
   int result;             /* Returned flag */
   size_t raw_size;        /* Size of a single raw value */
   void *raw;              /* Pointer to stored value */ 

/* Initialise */ 
   result = 0; 
   *nval = 0; 

/* Check the global error status. */ 
   if ( !astOK ) return result; 

/* Use the hash function to determine the element of the hash table in 
   which the key will be stored. */ 
   itab = HashFun( key ); 

/* Search the relevent table entry for the required MapEntry. */ 
   mapentry = SearchTableEntry( this, itab, key ); 

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

      } else { 
         raw_size = 0; 
         raw = NULL; 
         astError( AST__INTER, "astMapGet1<X>(KeyMap): Illegal map entry data " 
                   "type %d encountered (internal AST programming error).", 
                   raw_type ); 
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
         if( !ConvertValue( raw, raw_type, &avalue, AST__OBJECTTYPE ) && astOK ){ 
            astError( AST__MPGER, "astMapGet0A(%s): The value of " 
                      "element %d of KeyMap key \"%s\" cannot be read using " 
                      "the requested data type.",astGetClass( this ), i + 1, key ); 

/* If succesful, return an ID value for the Object. */
         } else {
           value[ i ] = avalue ? astMakeId( avalue ) : NULL;
         }

/* Increment the pointers to the next raw value. */
         raw = (char *) raw + raw_size;
      } 
   } 

/* If an error occurred,return zero. */ 
   if( !astOK ) result = 0; 

/* Return the result.*/ 
   return result; 
}

static int MapHasKey( AstKeyMap *this, const char *key ) {
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
f     STATUS = INTEGER (Given and Returned)
f        The global status.

*  Returned Value:
c     astMapHasKey()
f     AST_MAPHASKEY = LOGICAL
c        Non-zero if the key was found, and zero otherwise.
f        .TRUE. if the key was found, and .FALSE. otherwise.

*  Notes:
*     - A function value of 
c     zero 
f     .FALSE.
*     will be returned if an error has already occurred, or if this 
*     function should fail for any reason.

*--
*/

/* Local Variables: */
   AstMapEntry *mapentry;  /* Pointer to entry in linked list */
   int itab;               /* Index of hash table element to use */
   int result;             /* Returned value */

/* Initialise */
   result = 0;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Use the hash function to determine the element of the hash table in
   which the key will be stored. */
   itab = HashFun( key );

/* Search the relevent table entry for the required MapEntry. */
   mapentry = SearchTableEntry( this, itab, key );

/* Set a non-zero return value if the key was found. */
   if( mapentry ) result = 1;

/* If an error has occurred, return zero. */
   if( !astOK ) result = 0;

/* Return the result. */
   return result;

}

static void MapRemove( AstKeyMap *this, const char *key ) {
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
f     STATUS = INTEGER (Given and Returned)
f        The global status.
*--
*/

/* Local Variables: */
   int itab;               /* Index of hash table element to use */

/* Check the global error status. */
   if ( !astOK ) return;

/* Use the hash function to determine the element of the hash table in 
   which the key will be stored. */
   itab = HashFun( key );

/* Search the relevent table entry for the required MapEntry and remove it. */
   RemoveTableEntry( this, itab, key );

}

static int MapSize( AstKeyMap *this ) {
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
   for( itab = 0; itab < AST__MAPSIZE; itab++ ) result += this->nentry[ itab ];

/* Return the result. */
   return result;

}

static int MapLenC( AstKeyMap *this, const char *key ) {
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
   int i;                  /* Element index */
   int itab;               /* Index of hash table element to use */
   int l;                  /* Length of formatted vector element */
   int nel;                /* Number of elements in raw vector */ 
   int raw_type;           /* Data type of stored value */
   int result;             /* Returned value */
   size_t raw_size;        /* Size of a single raw value */
   void *raw;              /* Pointer to stored value */ 

/* Initialise */
   result = 0;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Use the hash function to determine the element of the hash table in
   which the key will be stored. */
   itab = HashFun( key );

/* Search the relevent table entry for the required MapEntry. */
   mapentry = SearchTableEntry( this, itab, key );

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

      } else if( raw_type == AST__DOUBLETYPE ){ 
         raw_size = sizeof( double ); 
         if( nel == 0 ) { 
            raw = &( ((Entry0D *)mapentry)->value ); 
         } else { 
            raw = ((Entry1D *)mapentry)->value; 
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

      } else { 
         raw_size = 0; 
         raw = NULL; 
         astError( AST__INTER, "astMapLenC(KeyMap): Illegal map entry data " 
                   "type %d encountered (internal AST programming error).", 
                   raw_type ); 
      } 

/* Treat scalars as single-value vectors. */ 
      if( nel == 0 ) nel = 1; 

/* Initialise the maximum length of any formatted value in the entry. */
      result= 0;

/* Loop round all values in the vector. */ 
      for( i = 0; i < nel && astOK; i++ ) { 

/* Go through the motions of formatting the value. We do not actually
   need the formatted string (just its length) so we provide a NULL pointer
   for the output buffer. The entry is ignored if it cannot be formatted. */
         l = ConvertValue( raw, raw_type, NULL, AST__STRINGTYPE );
         if( l > result ) result = l;

/* Increment the pointer to the next raw value. */
         raw = (char *) raw + raw_size;
      } 
   }

/* If an error has occurred, return zero. */
   if( !astOK ) result = 0;

/* Return the result. */
   return result;

}

static int MapLength( AstKeyMap *this, const char *key ) {
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
   int itab;               /* Index of hash table element to use */
   int result;             /* Returned value */

/* Initialise */
   result = 0;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Use the hash function to determine the element of the hash table in
   which the key will be stored. */
   itab = HashFun( key );

/* Search the relevent table entry for the required MapEntry. */
   mapentry = SearchTableEntry( this, itab, key );

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

static int MapType( AstKeyMap *this, const char *key ) {
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
f     STATUS = INTEGER (Given and Returned)
f        The global status.

*  Returned Value:
c     astMapType()
f     AST_MAPTYPE = INTEGER
*        One of AST__INTTYPE (for integer), AST__DOUBLETYPE (for double
*        precision floating point), AST__STRINGTYPE (for character string)
*        or AST__OBJECTTYPE (for AST Object pointer). AST__BADTYPE is
*        returned if the supplied key is not found in the KeyMap.

*  Notes:
*     - A function value of AST__BADTYPE will be returned if an error has 
*     already occurred, or if this function should fail for any reason.

*--
*/

/* Local Variables: */
   AstMapEntry *mapentry;  /* Pointer to entry in linked list */
   int itab;               /* Index of hash table element to use */
   int result;             /* Returned value */

/* Initialise */
   result = AST__BADTYPE;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Use the hash function to determine the element of the hash table in
   which the key will be stored. */
   itab = HashFun( key );

/* Search the relevent table entry for the required MapEntry. */
   mapentry = SearchTableEntry( this, itab, key );

/* Store the type if found. */
   if( mapentry ) result = mapentry->type;

/* If an error has occurred, return zero. */
   if( !astOK ) result = AST__BADTYPE;

/* Return the result. */
   return result;

}

static void RemoveTableEntry( AstKeyMap *this, int itab, const char *key ){
/*
*  Name:
*     RemoveTableEntry

*  Purpose:
*     Remove an entry from a linked-list of KeyMap entries.

*  Type:
*     Private function.

*  Synopsis:
*     #include "keymap.h"
*     void RemoveTableEntry( AstKeyMap *this, int itab, const char *key )

*  Class Membership:
*     KeyMap member function.

*  Description:
*     This function removes any entries with the specified key from the
*     linked-list of entries stored at the specified entry of the hash
*     table. If the supplied key is not found in the list, the function 
*     returns without action.

*  Parameters:
*     this
*        Pointer to the KeyMap.
*     itab
*        Index of the hash table element to be searched.
*     key
*        The key string to be searched for. Trailing spaces are ignored.

*/

/* Local Variables: */
   AstMapEntry **link;    /* Address to store foward link */
   AstMapEntry *next;     /* Pointer to next Entry to copy */

/* Check the global error status. */
   if ( !astOK ) return;

/* The "next" variable holds the address of the next MapEntry to be
   checked. Initialise this to the MapEntry at the head of the linked
   list associated with the supplied element of he hash table. */
   next = this->table[ itab ];

/* The "link" variable holds the address of the location at which the
   pointer to the MapEntry following the removed MapEntry should be stored. 
   Initialise this to be the adress of the hash table element. */
   link = &( this->table[ itab ] );

/* Loop round until we have checked all entries. */
   while( next && astOK ) {

/* If the key for the current entry macthes the supplied key... */
      if( !KeyCmp( next->key, key ) ) {
      
/* Store a pointer to the next MapEntry in the list, replacing the
   original pointer to the MapEntry which is being deleted. */
         *link = next->next;

/* Free this MapEntry */
         FreeMapEntry( next );

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
}

static AstMapEntry *SearchTableEntry( AstKeyMap *this, int itab, const char *key ){
/*
*  Name:
*     SearchTableEntry

*  Purpose:
*     Search an element of a has table for a given key.

*  Type:
*     Private function.

*  Synopsis:
*     #include "keymap.h"
*     AstMapEntry *SearchTableEntry( AstKeyMap *this, int itab, const char *key )

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

static size_t SizeOfEntry( AstMapEntry *entry ){
/*
*  Name:
*     SizeOfEntry

*  Purpose:
*     Return the size of the supplied MapEntry structure.

*  Type:
*     Private function.

*  Synopsis:
*     #include "keymap.h"
*     size_t SizeOfEntry( AstMapEntry *entry )

*  Class Membership:
*     KeyMap member function.

*  Description:
*     This function returns the size of the supplied MapEntry structure.

*  Parameters:
*     entry
*        Pointer to the MapEntry.

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

   } else if( type == AST__DOUBLETYPE ) {
      result = ( nel == 0 ) ? sizeof( Entry0D ) : sizeof( Entry1D );

/* Report an error if the data type is unknown. */
   } else {
      astError( AST__INTER, "SizeOfEntry(KeyMap): Illegal map entry data "
                "type %d encountered (internal AST programming error).",
                type );
   }

/* Return the result. */
   return result;
}

/* Functions which access class attributes. */
/* ---------------------------------------- */
/* Implement member functions to access the attributes associated with
   this class using the macros defined for this purpose in the
   "object.h" file. For a description of each attribute, see the class
   interface (in the associated .h file). */

/* Copy constructor. */
/* ----------------- */
static void Copy( const AstObject *objin, AstObject *objout ) {
/*
*  Name:
*     Copy

*  Purpose:
*     Copy constructor for KeyMap objects.

*  Type:
*     Private function.

*  Synopsis:
*     void Copy( const AstObject *objin, AstObject *objout )

*  Description:
*     This function implements the copy constructor for KeyMap objects.

*  Parameters:
*     objin
*        Pointer to the object to be copied.
*     objout
*        Pointer to the object being constructed.

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
   for( i = 0; i < AST__MAPSIZE; i++ ) {
      out->table[ i ] = NULL;
      out->nentry[ i ] = 0;
   }

/* Make copies of the table entries. */
   for( i = 0; i < AST__MAPSIZE; i++ ) CopyTableEntry( in, out, i );

/* If an error occurred, clean up by freeing all memory allocated above. */
   if ( !astOK ) {
      for( i = 0; i < AST__MAPSIZE; i++ ) FreeTableEntry( out, i );
   }
}

/* Destructor. */
/* ----------- */
static void Delete( AstObject *obj ) {
/*
*  Name:
*     Delete

*  Purpose:
*     Destructor for KeyMap objects.

*  Type:
*     Private function.

*  Synopsis:
*     void Delete( AstObject *obj )

*  Description:
*     This function implements the destructor for KeyMap objects.

*  Parameters:
*     obj
*        Pointer to the object to be deleted.

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
   for( i = 0; i < AST__MAPSIZE; i++ ) FreeTableEntry( this, i );

}

/* Dump function. */
/* -------------- */
static void Dump( AstObject *this_object, AstChannel *channel ) {
/*
*  Name:
*     Dump

*  Purpose:
*     Dump function for KeyMap objects.

*  Type:
*     Private function.

*  Synopsis:
*     void Dump( AstObject *this, AstChannel *channel )

*  Description:
*     This function implements the Dump function which writes out data
*     for the KeyMap class to an output Channel.

*  Parameters:
*     this
*        Pointer to the KeyMap whose data are being written.
*     channel
*        Pointer to the Channel to which the data are being written.
*/

/* Local Variables: */
   AstKeyMap *this;                 /* Pointer to the KeyMap structure */
   AstMapEntry *next;               /* Pointer to the next AstMapEntry to dump */
   int i;                        /* Index into hash table */
   int nentry;                   /* Number of entries dumped so far */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain a pointer to the KeyMap structure. */
   this = (AstKeyMap *) this_object;

/* Initialise the number of KeyMap entries dumped so far. */
   nentry = 0;

/* Loop round each entry in the hash table. */
   for( i = 0; i < AST__MAPSIZE; i++ ) {

/* Get a pointer to the next KeyMap entry to dump. */
      next = this->table[ i ];

/* Loop round dumping all KeyMap entries in this element of the hash table. */
      while( next && astOK ) {
         DumpEntry( next, channel, ++nentry  );

/* Get a pointer to the next entry to dump. */
         next = next->next;

      }
   }
}

/* Standard class functions. */
/* ========================= */
/* Implement the astIsAKeyMap and astCheckKeyMap functions using the macros
   defined for this purpose in the "object.h" header file. */
astMAKE_ISA(KeyMap,Object,check,&class_init)
astMAKE_CHECK(KeyMap)

AstKeyMap *astKeyMap_( const char *options, ... ) {
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
*     which identify the values. The keys are strings (case-sensitive,
*     trailing spaces are ignored), and the data type of the values can be 
*     integer, floating point, character string or AST Object pointer. Each 
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
*--
*/

/* Local Variables: */
   AstKeyMap *new;                 /* Pointer to new KeyMap */
   va_list args;                /* Variable argument list */

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
      astVSet( new, options, args );
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
   AstKeyMap *new;                 /* Pointer to new KeyMap */
   va_list args;                /* Variable argument list */

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
      astVSet( new, options, args );
      va_end( args );

/* If an error occurred, clean up by deleting the new object. */
      if ( !astOK ) new = astDelete( new );
   }

/* Return an ID value for the new KeyMap. */
   return astMakeId( new );
}

AstKeyMap *astInitKeyMap_( void *mem, size_t size, int init, AstKeyMapVtab *vtab, 
                     const char *name ) {
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
*                         const char *name )

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
   int i;                    /* Loop count */

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
      for( i = 0; i < AST__MAPSIZE; i++ ) {
         new->table[ i ] = NULL;
         new->nentry[ i ] = 0;
      }

/* If an error occurred, clean up by deleting the new KeyMap. */
      if ( !astOK ) new = astDelete( new );
   }

/* Return a pointer to the new KeyMap. */
   return new;
}

AstKeyMap *astLoadKeyMap_( void *mem, size_t size, AstKeyMapVtab *vtab, 
                     const char *name, AstChannel *channel ) {
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
*                         const char *name, AstChannel *channel )

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
   AstKeyMap *new;              /* Pointer to the new KeyMap */
   AstObject **alist;        /* Pointer to vector of entry values */
   AstObject *aval;          /* AST Object value for an entry */
   char buff[ 30 ];          /* Buffer for key names */
   char *key;                /* Pointer to key string for an entry */
   char *com;                /* Pointer to comment string for an entry */
   const char **slist;       /* Pointer to vector of entry values */
   char *sval;               /* String value for an entry */
   double *dlist;            /* Pointer to vector of entry values */
   double dval;              /* Floating point value for an entry */
   int *ilist;               /* Pointer to vector of entry values */
   int index;                /* Index of next array element in a vector entry */
   int ival;                 /* Integer value for an entry */
   int nel;                  /* Vector length */
   int nentry;               /* Number of KeyMap entries read so far */
   int type;                 /* Data type for an entry */

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

/* Read input data. */
/* ================ */
/* Request the input Channel to read all the input data appropriate to
   this class into the internal "values list". */
      astReadClassData( channel, "KeyMap" );

/* Now read each individual data item from this list and use it to
   initialise the appropriate instance variable(s) for this class. */

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
                      "whilst reading a %s.", name, name );

         }

/* Get the vector length. */
         (void) sprintf( buff, "nel%d", nentry );
         nel = astReadInt( channel, buff, 0 );

/* First deal with integer entries. */
         if( type == AST__INTTYPE ) {

/* For scalar entries, use "val<nentry>" to get the value then create a new
   entry and add it to the KeyMap. */
            if( nel == 0 ) {
               (void) sprintf( buff, "val%d", nentry );
               ival = astReadInt( channel, buff, 0 );
               astMapPut0I( new, key, ival, com );

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
               astMapPut1I( new, key, nel, ilist, com );

/* Free resources. */
               ilist = astFree( ilist );
            }

/* Do the same for double values. */
         } else if( type == AST__DOUBLETYPE ) {
            if( nel == 0 ) {
               (void) sprintf( buff, "val%d", nentry );
               dval = astReadDouble( channel, buff, AST__BAD );
               astMapPut0D( new, key, dval, com );
            } else {
               dlist = astMalloc( sizeof(double)*nel );
               for( index = 0; astOK && index < nel; index++ ) {
                  (void) sprintf( buff, "v%d_%d", nentry, index + 1 );
                  dlist[ index ] = astReadDouble( channel, buff, AST__BAD );
               }
               astMapPut1D( new, key, nel, dlist, com );
               dlist = astFree( dlist );
            }

/* Do the same for string values. */
         } else if( type == AST__STRINGTYPE ) {
            if( nel == 0 ) {
               (void) sprintf( buff, "val%d", nentry );
               sval = astReadString( channel, buff, "" );
               astMapPut0C( new, key, sval, com );
               sval = astFree( sval );
            } else {
               slist = astMalloc( sizeof(const char *)*nel );
               for( index = 0; astOK && index < nel; index++ ) {
                  (void) sprintf( buff, "v%d_%d", nentry, index + 1 );
                  slist[ index ] = astReadString( channel, buff, "" );
               }
               astMapPut1C( new, key, nel, slist, com );
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
               astMapPut0A( new, key, aval, com );
               if( aval ) aval = astAnnul( aval );
            } else {
               alist = astMalloc( sizeof(AstObject *)*nel );
               for( index = 0; astOK && index < nel; index++ ) {
                  (void) sprintf( buff, "v%d_%d", nentry, index + 1 );
                  alist[ index ] = astReadObject( channel, buff, NULL );
               }
               astMapPut1A( new, key, nel, alist, com );
               for( index = 0; astOK && index < nel; index++ ) {
                  if( alist[ index ] ) alist[ index ] = astAnnul( alist[ index ] );
               }
               alist = astFree( alist );
            }

/* Report an error if the data type is unknown. */
         } else if( astOK ) {
            astError( AST__BDFTS, "astLoadKeyMap(%s): Unknown data type code "
                      "(%d) encountered whilst reading a %s.", name, type, 
                      name );
         }

/* Free resources. */
         key = astFree( key );
         if( com ) com = astFree( com );

      }

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
                      const char *comment ){ \
   if ( !astOK ) return; \
   (**astMEMBER(this,KeyMap,MapPut0##X))(this,key,value,comment ); \
}
MAKE_MAPPUT0_(D,double)
MAKE_MAPPUT0_(I,int)
MAKE_MAPPUT0_(C,const char *)
MAKE_MAPPUT0_(A,AstObject *)
#undef MAKE_MAPPUT0_

#define MAKE_MAPPUT1_(X,Xtype) \
void astMapPut1##X##_( AstKeyMap *this, const char *key, int size, Xtype value[], \
                      const char *comment ){ \
   if ( !astOK ) return; \
   (**astMEMBER(this,KeyMap,MapPut1##X))(this,key,size,value,comment ); \
}
MAKE_MAPPUT1_(D,double)
MAKE_MAPPUT1_(I,int)
MAKE_MAPPUT1_(C,const char *)
MAKE_MAPPUT1_(A,AstObject *)
#undef MAKE_MAPPUT1_

#define MAKE_MAPGET0_(X,Xtype) \
int astMapGet0##X##_( AstKeyMap *this, const char *key, Xtype *value ){ \
   if ( !astOK ) return 0; \
   return (**astMEMBER(this,KeyMap,MapGet0##X))(this,key,value); \
}
MAKE_MAPGET0_(D,double)
MAKE_MAPGET0_(I,int)
MAKE_MAPGET0_(C,const char *)
MAKE_MAPGET0_(A,AstObject *)
#undef MAKE_MAPGET0_


#define MAKE_MAPGET1_(X,Xtype) \
int astMapGet1##X##_( AstKeyMap *this, const char *key, int mxval, int *nval, \
                      Xtype *value ){ \
   if ( !astOK ) return 0; \
   return (**astMEMBER(this,KeyMap,MapGet1##X))(this,key,mxval,nval,value); \
}
MAKE_MAPGET1_(D,double)
MAKE_MAPGET1_(I,int)
MAKE_MAPGET1_(A,AstObject *)
#undef MAKE_MAPGET1_

int astMapGet1C_( AstKeyMap *this, const char *key, int l, int mxval, int *nval,
                  char *value ){ 
   if ( !astOK ) return 0;
   return (**astMEMBER(this,KeyMap,MapGet1C))(this,key,l,mxval,nval,value);
}

void astMapRemove_( AstKeyMap *this, const char *key ){ 
   if ( !astOK ) return;
   (**astMEMBER(this,KeyMap,MapRemove))(this,key); 
}
int astMapSize_( AstKeyMap *this ){ 
   if ( !astOK ) return 0;
   return (**astMEMBER(this,KeyMap,MapSize))(this); 
}
int astMapLenC_( AstKeyMap *this, const char *key ){ 
   if ( !astOK ) return 0;
   return (**astMEMBER(this,KeyMap,MapLenC))(this,key); 
}
int astMapLength_( AstKeyMap *this, const char *key ){ 
   if ( !astOK ) return 0;
   return (**astMEMBER(this,KeyMap,MapLength))(this,key); 
}
int astMapType_( AstKeyMap *this, const char *key ){ 
   if ( !astOK ) return 0;
   return (**astMEMBER(this,KeyMap,MapType))(this,key); 
}
int astMapHasKey_( AstKeyMap *this, const char *key ){ 
   if ( !astOK ) return 0;
   return (**astMEMBER(this,KeyMap,MapHasKey))(this,key); 
}
const char *astMapKey_( AstKeyMap *this, int index ){ 
   if ( !astOK ) return NULL;
   return (**astMEMBER(this,KeyMap,MapKey))(this,index); 
}
















