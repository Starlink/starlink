/*
*  Name:
*     memory.c

*  Purpose:
*     Implement memory allocation/deallocation functions.

*  Description:
*     This file implements the Memory module which is used for
*     allocating and freeing memory in the AST library.  For a
*     description of the module and its interface, see the .h file of
*     the same name.

*  Copyright:
*     <COPYRIGHT_STATEMENT>

*  Authors:
*     RFWS: R.F. Warren-Smith (Starlink)
*     DSB: D.S. Berry (Starlink)

*  History:
*     2-JAN-1996 (RFWS):
*        Original version.
*     26-JAN-1996 (RFWS):
*        Removed trailing underscores from static functions and
*        changed to use new error function interfaces.
*     20-JUN-1996 (RFWS):
*        Added astString.
*     15-JUL-1996 (RFWS):
*        Make IsDynamic execute under error conditions to avoid memory
*        leaks in such situations.
*     11-SEP-1996 (RFWS):
*        Added astStringArray (original written by DSB).
*     18-MAR-1998 (RFWS):
*        Added notes about these functions being available for writing
*        foreign language and graphics interfaces, etc.
*/

/* Module Macros. */
/* ============== */
/* Define the astCLASS macro (even although this is not a class
   implementation) to obtain access to the protected error handling
   functions. */
#define astCLASS

/* Include files. */
/* ============== */
/* Interface definitions. */
/* ---------------------- */
#include "error.h"               /* Error reporting facilities */
#include "memory.h"              /* Interface to this module */

/* Error code definitions. */
/* ----------------------- */
#include "ast_err.h"             /* AST error codes */

/* C header files. */
/* --------------- */
#include <errno.h>
#include <string.h>
#include <stdlib.h>

/* Module Type Definitions. */
/* ======================== */
/* Header for allocated memory. */
/* ---------------------------- */
/* This stores a "magic" value so that dynamically allocated memory
   can be recognised, together with the allocated size. It also
   ensures correct alignment. */
typedef struct Memory {
   unsigned long magic;
   size_t size;
} Memory;

/* Prototypes for Private Functions. */
/* ================================= */
static int IsDynamic( void *ptr );
static unsigned long Magic( void *ptr, size_t size );

/* Function implementations. */
/* ========================= */
void *astFree_( void *ptr ) {
/*
*+
*  Name:
*     astFree

*  Purpose:
*     Free previously allocated memory.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "memory.h"
*     void *astFree( void *ptr )

*  Description:
*     This function frees memory that has previouly been dynamically
*     allocated.

*  Parameters:
*     ptr
*        Pointer to previously allocated memory. An error will result
*        if the memory has not previously been allocated by another
*        function in this module. However, a NULL pointer value is
*        accepted (without error) as indicating that no memory has yet
*        been allocated, so that no action is required.

*  Returned Value:
*     Always NULL.

*  Notes:
*     - This function is documented as protected because it should not
*     be invoked by external code. However, it is available via the
*     external C interface so that it may be used when writing (e.g.)
*     foreign language or graphics interfaces.
*-
*/

/* Local Variables: */
   Memory *mem;                  /* Pointer to memory header */

/* If the incoming pointer is NULL, do nothing. Otherwise, check if it
   points at dynamically allocated memory (IsDynamic sets the global
   error status if it does not). */
   if ( ptr && IsDynamic( ptr ) ) {

/* If OK, obtain a pointer to the memory header and clear the "magic
   number" and size values it contains. This helps prevent accidental
   re-use of the memory. */
      mem = ( (Memory *) ptr ) - 1;
      mem->magic = (unsigned long) 0;
      mem->size = (size_t) 0;

/* Free the allocated memory. */
      free( mem );
   }

/* Always return a NULL pointer. */
   return NULL;
}

void *astGrow_( void *ptr, int n, size_t size ) {
/*
*+
*  Name:
*     astGrow

*  Purpose:
*     Allocate memory for an adjustable array.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "memory.h"
*     void *astGrow( void *ptr, int n, size_t size )

*  Description:
*     This function allocates memory in which to store an array of
*     data whose eventual size is unknown. It should be invoked
*     whenever a new array size is determined and will appropriately
*     increase the amount of memory allocated when necessary. In
*     general, it will over-allocate in anticipation of future growth
*     so that the amount of memory does not need adjusting on every
*     invocation.

*  Parameters:
*     ptr
*        Pointer to previously allocated memory (or NULL if none has
*        yet been allocated).
*     n
*        Number of array elements to be stored (may be zero).
*     size
*        The size of each array element.

*  Returned Value:
*     If the memory was allocated successfully, a pointer to the start
*     of the possibly new memory region is returned (this may be the
*     same as the original pointer).

*  Notes:
*     - When new memory is allocated, the existing contents are preserved.
*     - This function does not free memory once it is allocated, so
*     the size allocated grows to accommodate the maximum size of the
*     array (or "high water mark"). Other memory handling routines may
*     be used to free the memory (or alter its size) if necessary.
*     - If this function is invoked with the global error status set,
*     or if it fails for any reason, the original pointer value is
*     returned and the memory contents are unchanged.
*     - This function is documented as protected because it should not
*     be invoked by external code. However, it is available via the
*     external C interface so that it may be used when writing (e.g.)
*     foreign language or graphics interfaces.
*-
*/

/* Local Variables: */
   Memory *mem;                  /* Pointer to memory header */
   size_t newsize;               /* New size to allocate */
   void *new;                    /* Result pointer */

/* Check the global error status. */
   if ( !astOK ) return ptr;

/* Initialise. */
   new = ptr;

/* Calculate the total size of memory needed. */
   size *= (size_t) n;

/* If no memory has yet been allocated, allocate exactly the amount
   required. */
   if ( !ptr ) {
      new = astMalloc( size );

/* Otherwise, check that the incoming pointer identifies previously
   allocated memory. */
   } else if ( IsDynamic( ptr ) ) {

/* Obtain a pointer to the memory header and check if the new size
   exceeds that already allocated. */
      mem = ( (Memory *) ptr ) - 1;
      if ( mem->size < size ) {

/* If so, calculate a possible new size by doubling the old
   size. Increase this further if necessary. */
         newsize = mem->size * ( (size_t) 2 );
         if ( size > newsize ) newsize = size;

/* Re-allocate the memory. */
         new = astRealloc( ptr, newsize );
      }
   }

/* Return the result. */
   return new;
}

static int IsDynamic( void *ptr ) {
/*
*  Name:
*     IsDynamic

*  Purpose:
*     Test whether a memory region has been dynamically allocated.

*  Type:
*     Private function.

*  Synopsis:
*     #include "memory.h"
*     int IsDynamic( void *ptr )

*  Description:
*     This function takes a pointer to a region of memory and tests if
*     the memory has previously been dynamically allocated using other
*     functions from this module. It does this by checking for the
*     presence of a "magic" number in the header which precedes the
*     allocated memory. If the magic number is not present (or the
*     pointer is invalid for any other reason), an error is reported
*     and the global error status is set.

*  Parameters:
*     ptr
*        Pointer to the start (as known to the external user) of the
*        dynamically allocated memory.

*  Returned Value:
*     If the memory was allocated dynamically, a value of 1 is
*     returned.  Otherwise, zero is returned and an error results.

*  Notes:
*     - A NULL pointer value produces an error report from this
*     function, although other functions may wish to regard a NULL
*     pointer as valid.
*     - This function attempts to execute even if the global error
*     status is set, although no further error report will be made if
*     the memory is not dynamically allocated under these
*     circumstances.
*     - The test performed by this function is not 100% secure as the
*     "magic" value could occur by accident (although this is
*     unlikely). It is mainly intended to provide security against
*     programming errors, including accidental corruption of the
*     memory header and attempts to allocate the same region of memory
*     more than once.
*/

/* Local Variables: */
   Memory *mem;                  /* Pointer to memory header */
   int dynamic;                  /* Dynamically allocated? */

/* Initialise. */
   dynamic = 0;

/* Check that a NULL pointer has not been supplied and report an error
   if it has (but not if the global status is already set). */
   if ( !ptr ) {
      if ( astOK ) {
         astError( AST__PTRIN, "Invalid NULL pointer (address %p).", ptr );
      }

/* If OK, derive a pointer to the memory header that precedes the
   allocated region of memory. */
   } else {
      mem = ( (Memory *) ptr ) - 1;

/* Check if the "magic number" in the header is valid and report an
   error if it is not (but not if the global status is already
   set). */
      if ( mem->magic != Magic( mem, mem->size ) ) {
         if ( astOK ) {
            astError( AST__PTRIN,
                      "Invalid pointer or corrupted memory at address %p.",
                      ptr );
         }

/* Note if the magic number is OK. */
      } else {
         dynamic = 1;
      }
   }

/* Return the result. */
   return dynamic;
}

static unsigned long Magic( void *ptr, size_t size ) {
/*
*  Name:
*     Magic

*  Purpose:
*     Generate a "magic number".

*  Type:
*     Private function.

*  Synopsis:
*     #include "memory.h"
*     unsigned long Magic( void *ptr, size_t size )

*  Description:
*     This function generates a "magic number" which is a function of
*     a memory address and an object size. This number may be stored
*     in a region of dynamically allocated memory to allow it to be
*     recognised as dynamically allocated by other routines, and also
*     to provide security against memory leaks, etc.

*  Parameters:
*     ptr
*        The memory pointer.
*     size
*        The object size.

*  Returned Value:
*     The function returns the magic number.

*  Notes:
*     This function does not perform error checking.
*/

/* Form the bit-wise exclusive OR between the memory address and the
   object size, then add 1 and invert the bits. Return the result as
   an unsigned long integer. */
   return ~( ( ( (unsigned long) ptr ) ^ ( (unsigned long) size ) ) +
             ( (unsigned long) 1 ) );
}

void *astMalloc_( size_t size ) {
/*
*+
*  Name:
*     astMalloc

*  Purpose:
*     Allocate memory.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "memory.h"
*     void *astMalloc( size_t size )

*  Description:
*     This function allocates memory in a similar manner to the
*     standard C "malloc" function, but with improved security
*     (against memory leaks, etc.) and with error reporting. It also
*     allows zero-sized memory allocation (without error), resulting
*     in a NULL returned pointer value.

*  Parameters:
*     size
*        The size of the memory region required (may be zero).

*  Returned Value:
*     If successful, the function returns a pointer to the start of
*     the allocated memory region. If the size allocated is zero, this
*     will be a NULL pointer.

*  Notes:
*     - A pointer value of NULL is returned if this function is
*     invoked with the global error status set or if it fails for any
*     reason.
*     - This function is documented as protected because it should not
*     be invoked by external code. However, it is available via the
*     external C interface so that it may be used when writing (e.g.)
*     foreign language or graphics interfaces.
*-
*/

/* Local Variables: */
   Memory *mem;                  /* Pointer to space allocated by malloc */

/* Check the global error status. */
   if ( !astOK ) return NULL;

/* Initialise. */
   mem = NULL;

/* Check that the size requested is not negative and report an error
   if it is. */
   if ( size < (size_t) 0 ) {
      astError( AST__MEMIN,
                "Invalid attempt to allocate %ld bytes of memory.",
                (long) size );

/* Otherwise, if the size is greater than zero, attempt to use malloc
   to allocate the memory, including space for the header
   structure. */
   } else if ( size != (size_t ) 0 ) {
      mem = malloc( sizeof( Memory ) + size );

/* Report an error if malloc failed. */
      if ( !mem ) {
         astError( AST__NOMEM, "malloc: %s", strerror( errno ) );
         astError( AST__NOMEM, "Failed to allocate %ld bytes of memory.",
                   (long) size );

/* If successful, set the "magic number" in the header and also store
   the size. */
      } else {
         mem->magic = Magic( mem, size );
         mem->size = size;

/* Increment the memory pointer to the start of the region of
   allocated memory to be used by the caller.*/
         mem++;
      }
   }

/* Return the result. */
   return mem;
}

void *astRealloc_( void *ptr, size_t size ) {
/*
*+
*  Name:
*     astRealloc

*  Purpose:
*     Change the size of a dynamically allocated region of memory.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "memory.h"
*     void *astRealloc( void *ptr, size_t size )

*  Description:
*     This function changes the size of a dynamically allocated region
*     of memory, preserving its contents up to the minimum of the old
*     and new sizes. This may involve copying the contents to a new
*     location, so a new pointer is returned (and the old memory freed
*     if necessary).
*
*     This function is similar to the standard C "realloc" function
*     except that it provides better security against programming
*     errors and also supports the allocation of zero-size memory
*     regions (indicated by a NULL pointer).

*  Parameters:
*     ptr
*        Pointer to previously allocated memory (or NULL if the
*        previous size of the allocated memory was zero).
*     size
*        New size required for the memory region. This may be zero, in
*        which case a NULL pointer is returned (no error results). It
*        should not be negative.

*  Returned Value:
*     If the memory was reallocated successfully, a pointer to the
*     start of the new memory region is returned (this may be the same
*     as the original pointer). If size was given as zero, a NULL
*     pointer is returned.

*  Notes:
*     - If this function is invoked with the error status set, or if
*     it fails for any reason, the original pointer value is returned
*     and the memory contents are unchanged. Note that this behaviour
*     differs from that of the standard C "realloc" function which
*     returns NULL if it fails.
*     - This function is documented as protected because it should not
*     be invoked by external code. However, it is available via the
*     external C interface so that it may be used when writing (e.g.)
*     foreign language or graphics interfaces.
*-
*/

/* Local Variables: */
   Memory *mem;                  /* Pointer to memory header */

/* Check the global error status. */
   if ( !astOK ) return ptr;

/* Initialise. */
   mem = (Memory *) ptr;

/* If a NULL pointer was supplied, use astMalloc to allocate some new
   memory. */
   if ( !ptr ) {
      mem = astMalloc( size );

/* Otherwise, check that the pointer supplied points at memory
   allocated by a function in this module (IsDynamic sets the global
   error status if it does not). */
   } else if ( IsDynamic( ptr ) ) {

/* Check that a negative size has not been given and report an error
   if necessary. */
      if ( size < (size_t) 0 ) {
         astError( AST__MEMIN,
            "Invalid attempt to reallocate a block of memory to %ld bytes.",
                   (long) size );

/* If OK, obtain a pointer to the memory header. */
      } else {
         mem = ( (Memory *) ptr ) - 1;

/* If the new size is zero, free the old memory and set a NULL return
   pointer value. */
         if ( size == (size_t) 0 ) {
            astFree( ptr );
            mem = NULL;

/* Otherwise, reallocate the memory. */
         } else {
            mem = realloc( mem, sizeof( Memory ) + size );

/* If this failed, report an error and restore the original pointer
   value. */
            if ( !mem ) {
               astError( AST__NOMEM, "realloc: %s", strerror( errno ) );
               astError( AST__NOMEM,
                  "Failed to reallocate a block of memory to %ld bytes.",
                         (long) size );
               mem = (Memory *) ptr;

/* If successful, set the new "magic" value and size in the memory
   header and obtain a pointer to the start of the region of memory to
   be used by the caller. */
            } else {
               mem->magic = Magic( mem, size );
               mem->size = size;
               mem++;
            }
         }
      }
   }

/* Return the result. */
   return mem;   
}

size_t astSizeOf_( void *ptr ) {
/*
*+
*  Name:
*     astSizeOf

*  Purpose:
*     Determine the size of a dynamically allocated region of memory.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "memory.h"
*     size_t astSizeOf( void *ptr )

*  Description:
*     This function returns the size of a region of dynamically
*     allocated memory.

*  Parameters:
*     ptr
*        Pointer to dynamically allocated memory (or NULL if the size
*        of the allocated memory was zero).

*  Returned Value:
*     The allocated size. This will be zero if a NULL pointer was
*     supplied (no error will result).

*  Notes:
*     - A value of zero is returned if this function is invoked with
*     the global error status set, or if it fails for any reason.
*     - This function is documented as protected because it should not
*     be invoked by external code. However, it is available via the
*     external C interface so that it may be used when writing (e.g.)
*     foreign language or graphics interfaces.
*-
*/

/* Local Variables: */
   size_t size;                  /* Memory size */

/* Check the global error status. */
   if ( !astOK ) return (size_t) 0;

/* Initialise. */
   size = (size_t) 0;

/* Check if a non-NULL valid pointer has been given. If so, extract
   the memory size from the header which precedes it. */
   if ( ptr && IsDynamic( ptr ) ) {
      size = ( ( (Memory *) ptr ) - 1 )->size;
   }

/* Return the result. */
   return size;
}

void *astStore_( void *ptr, const void *data, size_t size ) {
/*
*+
*  Name:
*     astStore

*  Purpose:
*     Store data in dynamically allocated memory.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "memory.h"
*     void *astStore( void *ptr, const void *data, size_t size )

*  Description:
*     This function stores data in dynamically allocated memory,
*     allocating the memory (or adjusting the size of previously
*     allocated memory) to match the amount of data to be stored.

*  Parameters:
*     ptr
*        Pointer to previously allocated memory (or NULL if none has
*        yet been allocated).
*     data
*        Pointer to the start of the data to be stored. This may be
*        given as NULL if there are no data, in which case it will be
*        ignored and this function behaves like astRealloc, preserving
*        the existing memory contents.
*     size
*        The total size of the data to be stored and/or the size of
*        memory to be allocated. This may be zero, in which case the
*        data parameter is ignored, any previously-allocated memory is
*        freed and a NULL pointer is returned.

*  Returned Value:
*     If the data were stored successfully, a pointer to the start of
*     the possibly new memory region is returned (this may be the same
*     as the original pointer). If size was given as zero, a NULL
*     pointer is returned.

*  Notes:
*     - This is a convenience function for use when storing data of
*     arbitrary size in memory which is to be allocated
*     dynamically. It is appropriate when the size of the data will
*     not change frequently because the size of the memory region will
*     be adjusted to fit the data on every invocation.
*     - If this function is invoked with the error status set, or if
*     it fails for any reason, the original pointer value is returned
*     and the memory contents are unchanged.
*     - This function is documented as protected because it should not
*     be invoked by external code. However, it is available via the
*     external C interface so that it may be used when writing (e.g.)
*     foreign language or graphics interfaces.
*-
*/

/* Local Variables: */
   void *new;                    /* Pointer to returned memory */

/* Check the global error status. */
   if ( !astOK ) return ptr;

/* Initialise. */
   new = ptr;

/* If the new size is zero, use astRealloc to free any previously
   allocated memory. Also re-allocate the memory if the data pointer
   is NULL (in which case we want to preserve its contents). */
   if ( ( size == (size_t) 0 ) || !data ) {
      new = astRealloc( ptr, size );

/* In other cases, we do not want to preserve any memory
   contents. Check if the incoming memory pointer is valid (IsDynamic
   sets the global error status if it is not). */
   } else if ( !ptr || ( IsDynamic( ptr ) ) ) {

/* Allocate the new memory. If successful, free the old memory (if
   necessary) and copy the data into it. */
      new = astMalloc( size );
      if ( astOK ) {
         if ( ptr ) ptr = astFree( ptr );
         (void) memcpy( new, data, size );

/* If memory allocation failed, do not free the old memory but return
   a pointer to it. */
      } else {
         new = ptr;
      }
   }

/* Return the result. */
   return new;
}

char *astString_( const char *chars, int nchars ) {
/*
*+
*  Name:
*     astString

*  Purpose:
*     Create a C string from an array of characters.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "memory.h"
*     char *astString( const char *chars, int nchars )

*  Description:
*     This function allocates memory to hold a C string and fills the
*     string with the sequence of characters supplied. It then
*     terminates the string with a null character and returns a
*     pointer to its start. The memory used for the string may later
*     be de-allocated using astFree.
*
*     This function is intended for constructing null terminated C
*     strings from arrays of characters which are not null terminated,
*     such as when importing a character argument from a Fortran 77
*     program.

*  Parameters:
*     chars
*        Pointer to the array of characters to be used to fill the string.
*     nchars
*        The number of characters in the array (zero or more).

*  Returned Value:
*     If successful, the function returns a pointer to the start of
*     the allocated string. If the number of characters is zero, a
*     zero-length string is still allocated and a pointer to it is
*     returned.

*  Notes:
*     - A pointer value of NULL is returned if this function is
*     invoked with the global error status set or if it fails for any
*     reason.
*     - This function is documented as protected because it should not
*     be invoked by external code. However, it is available via the
*     external C interface so that it may be used when writing (e.g.)
*     foreign language or graphics interfaces.
*-
*/

/* Local Variables: */
   char *result;                 /* Pointer value to return */

/* Initialise. */
   result = NULL;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Check that the number of characters in the string is valid and
   report an error if it is not. */
   if ( nchars < 0 ) {
      astError( AST__NCHIN, "astString: Invalid attempt to allocate a string "
                "with %d characters.", nchars);

/* Allocate memory to hold the string. */
   } else {
      result = (char *) astMalloc( (size_t) ( nchars + 1 ) );

/* If successful, copy the characters into the string. */
      if ( astOK && result ) {
         (void) memcpy( result, chars, (size_t) nchars );

/* Terminate the string. */
         result[ nchars ] = '\0';
      }
   }

/* Return the result. */
   return result;
}

char **astStringArray_( const char *chars, int nel, int len ) {
/*
*+
*  Name:
*     astStringArray

*  Purpose:
*     Create an array of C strings from an array of characters.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "memory.h"
*     char **astStringArray( const char *chars, int nel, int len ) 

*  Description:
*     This function turns an array of fixed-length character data into
*     a dynamicllay allocated array of null-terminated C strings with
*     an index array that may be used to access them.
*
*     The array of character data supplied is assumed to hold "nel"
*     adjacent fixed-length strings (without terminating nulls), each
*     of length "len" characters. This function allocates memory and
*     creates a null-terminated copy of each of these strings. It also
*     creates an array of "nel" pointers which point at the start of
*     each of these new strings. A pointer to this index array is
*     returned.
*
*     The memory used is allocated in a single block and should later
*     be de-allocated using astFree.
s
*  Parameters:
*     chars
*        Pointer to the array of input characters. The number of characters
*        in this array should be at least equal to (nel * len).
*     nel
*        The number of fixed-length strings in the input character
*        array. This may be zero but should not be negative.
*     len
*        The number of characters in each fixed-length input
*        string. This may be zero but should not be negative.

*  Returned Value:
*     A pointer to the start of the index array, which contains "nel"
*     pointers pointing at the start of each null-terminated output
*     string.
*
*     The returned pointer should be passed to astFree to de-allocate
*     the memory used when it is no longer required. This will free
*     both the index array and the memory used by the strings it
*     points at.

*  Notes:
*     - A NULL pointer will also be returned if the value of "nel" is
*     zero, in which case no memory is allocated.
*     - A pointer value of NULL will also be returned if this function
*     is invoked with the global error status set or if it fails for
*     any reason.
*     - This function is documented as protected because it should not
*     be invoked by external code. However, it is available via the
*     external C interface so that it may be used when writing (e.g.)
*     foreign language or graphics interfaces.
*-
*/

/* Local Variables: */
   char **result;                 /* Result pointer to return */
   char *out_str;                 /* Pointer to start of next output string */
   const char *in_str;            /* Pointer to start of next input string */
   int i;                         /* Loop counter for array elements */

/* Initialise. */
   result = NULL;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Check that the array size is valid and report an error if it is
   not. */
   if ( nel < 0 ) {
      astError( AST__NELIN,
                "astStringArray: Invalid attempt to allocate an array of "
                "%d strings.", nel );

/* If the string length will be used, check that it is valid and
   report an error if it is not. */
   } else if ( ( nel > 0 ) && ( len < 0 ) ) {
      astError( AST__NCHIN,
                "astStringArray: Invalid attempt to allocate an "
                "array of strings with %d characters in each.", len );

/* Allocate memory to hold the array of string pointers plus the
   string data (with terminating nulls). */
   } else {
      result = astMalloc( sizeof( char * ) * (size_t) nel +
                          (size_t) ( nel * ( len + 1 ) ) );

/* If successful, initialise pointers to the start of the current
   input and output strings. */
      if( astOK ){
         in_str = chars;
         out_str = (char *) ( result + nel );
 
/* Loop to copy each string. */
         for ( i = 0; i < nel; i++ ) {
            (void) memcpy( out_str, in_str, (size_t) len );

/* Terminate the output string. */
            out_str[ len ] = '\0';

/* Store a pointer to the start of the output string in the array of
   character pointers. */
            result[ i ] = out_str;

/* Increment the pointers to the start of the next string. */
            out_str += len + 1;
            in_str += len;
         }
      }
   }

/* Return the result. */
   return result;
}
