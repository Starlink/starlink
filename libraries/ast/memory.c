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
*     29-JAN-2002 (DSB):
*        Added astChrLen and astSscanf.
*     15-FEB-2002 (DSB):
*        Removed use of non-ANSI vsscanf from astSscanf.
*     15-NOV-2002 (DSB):
*        Moved ChrMatch from SkyFrame (etc) to here. Included stdio.h and
*        ctype.h.
*     10-FEB-2003 (DSB):
*        Added facilities for detecting and tracing memory leaks. These
*        are only included if AST is compiled with the -DDEBUG flag. 
*     3-MAR-2004 (DSB):
*        Modified astSscanf to avoid use of uninitialised values
*        corresponding to "%n" fields in the format string.
*/

/* Module Macros. */
/* ============== */
/* Define the astCLASS macro (even although this is not a class
   implementation) to obtain access to the protected error handling
   functions. */
#define astCLASS

/* The maximum number of fields within a format string allowed by astSscanf. */
#define VMAXFLD 20

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
#include <ctype.h>
#include <errno.h>
#include <string.h>
#include <stdlib.h>
#include <stdarg.h>
#include <stdio.h>

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

#ifdef DEBUG
   int id;      /* A unique identifier for every allocated memory chunk */
   int perm;    /* Is this chunk part of an acceptable once-off "memory leak"? */
#endif

} Memory;

/* Module Variables. */
/* ================= */
#ifdef DEBUG
static Memory **issued = NULL;
static int nissued = 0;
static int siz_issued = 0;
static int next_id = 0;
static int watch_id = 0;
static int perm_mem = 0;
static int memcheckid = -1;
static int domemcheck = 1;
static void (*memcheckfun)( void * );
#endif

/* Prototypes for Private Functions. */
/* ================================= */
static int IsDynamic( void *ptr );
static unsigned long Magic( void *ptr, size_t size );

#ifdef DEBUG
static void Issue( Memory *new );
static void DeIssue( Memory *old );
#endif

/* Function implementations. */
/* ========================= */
char *astAppendString_( char *str1, int *nc, const char *str2 ) {
/*
*  Name:
*     astAppendString

*  Purpose:
*     Append a string to another string which grows dynamically.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "memory.h"
*     char *astAppendString( char *str1, int *nc, const char *str2 )

*  Description:
*     This function appends one string to another dynamically
*     allocated string, extending the dynamic string as necessary to
*     accommodate the new characters (plus the final null).

*  Parameters:
*     str1
*        Pointer to the null-terminated dynamic string, whose memory
*        has been allocated using the AST memory allocation functions
*        defined in "memory.h". If no space has yet been allocated for
*        this string, a NULL pointer may be given and fresh space will
*        be allocated by this function.
*     nc
*        Pointer to an integer containing the number of characters in
*        the dynamic string (excluding the final null). This is used
*        to save repeated searching of this string to determine its
*        length and it defines the point where the new string will be
*        appended. Its value is updated by this function to include
*        the extra characters appended.
*
*        If "str1" is NULL, the initial value supplied for "*nc" will
*        be ignored and zero will be used.
*     str2
*        Pointer to a constant null-terminated string, a copy of which
*        is to be appended to "str1".

*  Returned Value:
*     A possibly new pointer to the dynamic string with the new string
*     appended (its location in memory may have to change if it has to
*     be extended, in which case the original memory is automatically
*     freed by this function). When the string is no longer required,
*     its memory should be freed using astFree.

*  Notes:
*     - If this function is invoked with the global error status set
*     or if it should fail for any reason, then the returned pointer
*     will be equal to "str1" and the dynamic string contents will be
*     unchanged.
*/

/* Local Variables: */
   char *result;                 /* Pointer value to return */
   int len;                      /* Length of new string */

/* Initialise. */
   result = str1;

/* If the first string pointer is NULL, also initialise the character
   count to zero. */
   if ( !str1 ) *nc = 0;

/* Check the global error status. */
   if ( !astOK || !str2 ) return result;

/* Calculate the total string length once the two strings have been
   concatenated. */
   len = *nc + (int) strlen( str2 );

/* Extend the first (dynamic) string to the required length, including
   a final null. Save the resulting pointer, which will be
   returned. */
   result = astGrow( str1, len + 1, sizeof( char ) );

/* If OK, append the second string and update the total character
   count. */
   if ( astOK ) {
      (void) strcpy( result + *nc, str2 );
      *nc = len;
   }

/* Return the result pointer. */
   return result;
}

int astChrMatch_( const char *str1, const char *str2 ) {
/*
*+
*  Name:
*     astChrMatch

*  Purpose:
*     Case insensitive string comparison.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "memory.h"
*     int astChrMatch( const char *str1, const char *str2 )

*  Description:
*     This function compares two null terminated strings for equality,
*     discounting differences in case and any trailing white space in either
*     string.

*  Parameters:
*     str1
*        Pointer to the first string.
*     str2
*        Pointer to the second string.

*  Returned Value:
*     Non-zero if the two strings match, otherwise zero.

*  Notes:
*     -  A value of zero is returned if this function is invoked with the
*     global error status set or if it should fail for any reason.
*-
*/

/* Local Variables: */
   int match;                    /* Strings match? */

/* Check the global error status. */
   if ( !astOK ) return 0;

/* Initialise. */
   match = 1;

/* Loop to compare characters in the two strings until a mis-match occurs or
   we reach the end of the longer string. */
   while ( match && ( *str1 || *str2 ) ) {

/* Two characters match if (a) we are at the end of one string and the other
   string contains white space or (b) both strings contain the same character
   when converted to lower case. */
      match = ( !*str1 && isspace( *str2 ) ) ||
              ( !*str2 && isspace( *str1 ) ) ||
              ( tolower( *str1 ) == tolower( *str2 ) );

/* Step through each string a character at a time until its end is reached. */
      if ( *str1 ) str1++;
      if ( *str2 ) str2++;
   }

/* Return the result. */
   return match;
}

int astChrMatchN_( const char *str1, const char *str2, size_t n ) {
/*
*+
*  Name:
*     astChrMatchN

*  Purpose:
*     Case insensitive string comparison of at most N characters

*  Type:
*     Protected function.

*  Synopsis:
*     #include "memory.h"
*     int astChrMatchN( const char *str1, const char *str2, size_t n )

*  Description:
*     This function compares two null terminated strings for equality,
*     discounting differences in case and any trailing white space in either
*     string. No more than "n" characters are compared.

*  Parameters:
*     str1
*        Pointer to the first string.
*     str2
*        Pointer to the second string.
*     n
*        Maximum number of characters to compare.

*  Returned Value:
*     Non-zero if the two strings match, otherwise zero.

*  Notes:
*     -  A value of zero is returned if this function is invoked with the
*     global error status set or if it should fail for any reason.
*-
*/

/* Local Variables: */
   int match;                    /* Strings match? */
   int nc;                       /* Number of characters compared so far */

/* Check the global error status. */
   if ( !astOK ) return 0;

/* Initialise. */
   match = 1;

/* So far we have compared zero characters */
   nc = 0;

/* Loop to compare characters in the two strings until a mis-match occurs or
   we reach the end of the longer string, or we reach the specified
   maximum number of characters. */
   while ( match && ( *str1 || *str2 ) && nc++ < n ) {

/* Two characters match if (a) we are at the end of one string and the other
   string contains white space or (b) both strings contain the same character
   when converted to lower case. */
      match = ( !*str1 && isspace( *str2 ) ) ||
              ( !*str2 && isspace( *str1 ) ) ||
              ( tolower( *str1 ) == tolower( *str2 ) );

/* Step through each string a character at a time until its end is reached. */
      if ( *str1 ) str1++;
      if ( *str2 ) str2++;
   }

/* Return the result. */
   return match;
}

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

#ifdef DEBUG
      mem->id = 0;
      mem->perm = perm_mem;
      DeIssue( mem );
#endif

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

#ifdef DEBUG
   if( domemcheck && memcheckfun && memcheckid != -1 ) {
      void *memcheckptr;
      domemcheck = 0;
      memcheckptr = astFindIdPtr( memcheckid );
      if( memcheckptr ) (*memcheckfun)( memcheckptr );
      domemcheck = 1;
   }
#endif

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

#ifdef DEBUG
         mem->id = ++next_id;
         mem->perm = perm_mem;
         Issue( mem );
#endif

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
   Memory *oldmem;               /* Original pointer to memory header */

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
            oldmem = mem;
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

#ifdef DEBUG
               if( oldmem != mem ) {
                  mem->id = ++next_id;
                  DeIssue( oldmem );
                  Issue( mem );
               }
#endif

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

size_t astChrLen_( const char *string ) {
/*
*+
*  Name:
*     astChrLen

*  Purpose:
*     Determine the used length of a string.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "memory.h"
*     size_t astChrLen( const char *string )

*  Description:
*     This function returns the used length of a string. This excludes any
*     trailing white space or non-printable characters (such as the
*     trailing null character).

*  Parameters:
*     string
*        Pointer to the string.

*  Returned Value:
*     The number of characters in the supplied string, not including the 
*     trailing newline, and any trailing white-spaces or non-printable 
*     characters.

*-
*/

/* Local Variables: */
   const char *c;           /* Pointer to the next character to check */
   size_t ret;              /* The returned string length */

/* Initialise the returned string length. */
   ret = 0;

/* Check a string has been supplied. */
   if( string ){

/* Check each character in turn, starting with the last one. */
      ret = strlen( string );
      c = string + ret - 1;
      while( ret ){
         if( isprint( (int) *c ) && !isspace( (int) *c ) ) break;
         c--;
         ret--;
      }
   }

/* Return the answer. */
   return ret;

}

int astSscanf_( const char *str, const char *fmt, ...) {
/*
*+
*  Name:
*     astSscanf

*  Purpose:
*     A wrapper for the ANSI sscanf function.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "memory.h"
*     int astSscanf( const char *str, const char *fmt, ...)

*  Description:
*     This function is a direct plug-in replacement for sscanf. It ensures ANSI
*     behaviour is available on all platforms, including those (such as
*     MacOS) on which have the native sscanf function exhibits non-ANSI
*     behaviour.

*  Parameters:
*     str
*        Pointer to the string to be scanned.
*     fmt
*        Pointer to the format string which defines the fields to be
*        looked for within "str".
*     ...
*        Pointers to locations at which to return the value of each
*        succesfuly converted field, in the order specified in "fmt".

*  Returned Value:
*     The number of fields which were succesfully read from "str".
*-
*/

/* Local Variables: */
   char *c;                 /* Pointer to the next character to check */
   char *newfor;            /* Pointer to modified format string */
   const char *d;           /* Pointer to the next character to check */
   int iptr;                /* Index into ptr array */
   int lfor;                /* No. of characters in format string */
   int lstr;                /* No. of characters in scanned string */
   int nc;                  /* No. of characters read from str */
   int nfld;                /* No. of counted field specifiers found so far */
   int nptr;                /* Np. of pointers stored */
   int ret;                 /* The returned number of conversions */
   va_list args;            /* Variable argument list pointer */
   void *fptr;              /* The next supplied pointer */ 
   void *ptr[ VMAXFLD ];    /* Array of supplied pointers */ 

/* Initialise the variable argument list pointer. */
   va_start( args, fmt );

/* Initialise the returned string length. */
   ret = 0;

/* Check a string and format have been supplied. */
   if( str && fmt ){

/* Go through the format string, counting the number of field specifiers which
   will return a value, and storing the corresponding points in the ptr
   array. */
      nptr = 0;
      c = (char *) fmt;
      while( *c ) {

/* Field specifiers are marked by a % sign. */
         if( *c == '%' ) {

/* Look at the character following the % sign. Quit if the end of the string 
   has been reached. */
            c++;
            if( *c ) {

/* If the % sign is followed by a "*" or another "%", then there will be no
   corresponding pointer in the variable argument list "args". Ignore such
   field specifiers. */ 
               if( *c != '*' && *c != '%' ) {

/* If possible store the corresponding pointer from the variable argument
   list supplied to this function. Report an error if there are too many. */
                  if ( nptr < VMAXFLD ) {
                     ptr[ nptr++ ] = va_arg( args, void *);

/* If the current field specifier is "%n" the corresponding pointer
   should be a pointer to an integer. We initialise the integer to zero.
   We need to do this because sscanf does not include "%n" values in the
   returned count of succesful conversions, and so there is no sure way
   of knowing whether a value has been stored for a "%n" field, and so
   whether it is safe to use it as an index into the supplied. */
                     if( *c == 'n' ) *( (int *) ptr[ nptr - 1 ] ) = 0;

	          } else {
                     astError( AST__INTER, "astSscanf: Format string " 
                               "'%s' contains more than %d fields "
                               "(AST internal programming error).", 
                               fmt, VMAXFLD );
                     break;
                  }
               }

/* Move on the first character following the field specifier. */
               c++;
            }

/* If this is not the start of a field specifier, pass on. */
         } else {
            c++;
         }
      }

/* Fill any unused pointers with NULL. */
      for( iptr = nptr; iptr < VMAXFLD; iptr++ ) ptr[iptr] = NULL;

/* Get the length of the string to be scanned. */
      lstr = strlen( str );

/* Get the length of the format string excluding any trailing white space. */
      lfor = astChrLen( fmt );

/* Bill Joye reports that MacOS sscanf fails to return the correct number of
   characters read (using a %n conversion) if there is a space before the
   %n. So check for this. Does the format string contain " %n"? */
      c = strstr( fmt, " %n" );
      if( c && astOK ) {

/* Take a copy of the supplied format string (excluding any trailing spaces). */
         newfor = (char *) astStore( NULL, (void *) fmt, (size_t) lfor + 1 ); 
         if( newfor ) {

/* Ensure the string is terminated (in case the supplied format string
   has any trailing spaces). */
            newfor[ lfor ] = 0;

/* Remove all spaces from before any %n. */
            c = strstr( (const char *) newfor, " %n" );
            while( c ) {
               while( *(c++) ) *( c - 1 ) = *c;
               c = strstr( newfor, " %n" );
            }

/* Use the native sscanf with the modified format string. Note, we cannot
   use vsscanf because it is not ANSI C. Instead, we list the pointers
   explicitly. */
            ret = sscanf( str, newfor, ptr[0], ptr[1], ptr[2], ptr[3],
                          ptr[4], ptr[5], ptr[6], ptr[7], ptr[8], ptr[9], 
                          ptr[10], ptr[11], ptr[12], ptr[13], ptr[14], 
                          ptr[15], ptr[16], ptr[17], ptr[18], ptr[19] );

/* Now look through the original format string for conversions specifiers.
   If any %n conversions are found which are preceeded by a space, then
   correct the returned character counts to include any spaces following the
   corresponding point in the scanned string. */
            nfld = 0;
            iptr = 0;
            c = (char *) fmt;
	    while( *c ) {

/* Field specifiers are marked by a % sign. */
               if( *c == '%' ) {

/* Look at the character following the % sign. Quit if the end of the string 
   has been reached. */
                  c++;
                  if( *c ) {

/* If the % sign is followed by a "*" or another "%", then there will be no
   corresponding pointer in the variable argument list "args". Ignore such
   field specifiers. */ 
                     if( *c != '*' && *c != '%' ) {

/* Get the supplied pointer corresponding to this field specifier. */
                        fptr = ptr[ iptr++ ];

/* Increment the number of matched fields required. "%n" specifiers are not 
   included in the value returned by sscanf so skip over them. */
                        if( *c != 'n' ) {
			   nfld++; 

/* If the % sign is followed by a "n", and was preceeded by a space, we
   may need to correct the returned character count. */
                        } else if( c > fmt + 1 && *(c-2) == ' ' ) {

/* Do not correct the returned value if sscanf did not get as far as this
   field specifier before an error occurred. */
                           if( ret >= nfld ) {

/* Get the original character count produced by sscanf. */
                              nc = *( (int *) fptr );

/* For each space in "str" which follows, increment the returned count by
   one (so long as the original count is not zero or more than the length
   of the string - this is not foolproof, but I can't think of a better
   check - all uses of %n in AST initialize the supplied count to zero 
   before calling sscanf so a value fo zero is a safe (ish) bet that the
   supplied string doesn't match the supplied format). */
                              if( nc > 0 && nc < lstr ) {
                                 d = str + nc;
                                 while( *(d++) == ' ' ) nc++;
                                 *( (int *) fptr ) = nc;
                              }
                           }
                        }
                     }

/* Move on the first character following the field specifier. */
                     c++;
                  }

/* If this is not the start of a field specifier, pass on. */
               } else {
                  c++;
               }
            }

/* Release the temporary copy of the format string. */
            newfor = (char *) astFree( (void *) newfor );
         }

/* If the format string should not trigger any known problems, use sscanf
   directly. */
      } else if( astOK ) {
         ret = sscanf( str, fmt, ptr[0], ptr[1], ptr[2], ptr[3],
                       ptr[4], ptr[5], ptr[6], ptr[7], ptr[8], ptr[9], 
                       ptr[10], ptr[11], ptr[12], ptr[13], ptr[14], 
                       ptr[15], ptr[16], ptr[17], ptr[18], ptr[19] );
      }      
   }

/* Tidy up the argument pointer. */
   va_end( args );

/* Return the answer. */
   return ret;

}

#ifdef DEBUG

static void Issue( Memory *new ) {
/*
*  Name:
*     Issue

*  Purpose:
*     Note that a dynamic memory address has been issued.

*  Type:
*     Private function.

*  Synopsis:
*     #include "memory.h"
*     void Issue( Memory *new )

*  Description:
*     This function stores a copy of the supplied pointer in a static 
*     array, so that checks on memory leakage can be performed.

*  Parameters:
*     new
*        The memory pointer.
*/

/* Local Variables: */
   int i;
   size_t old_size;
   size_t new_size;
   Memory **new_issued;

/* Check inherited status */
   if( !astOK ) return;

/* Report an error if this is the memory chunk which is being watched
   for. By setting a debugger breakpoint on astError_, you can identify
   the moment when a chunk known to be part of a leak is first issued. */
   if( new->id == watch_id ) {
      astError( AST__INTER, "Issue(memory): The id %d has been issued to "
                "memory address %p (internal AST programming error).", 
                watch_id, new );
      return;
   }       

/* Check the supplied object is not already on the list of issued
   addresses. */
   for( i = 0; i < nissued; i++ ) {
      if( new == issued[ i ] ){
         astError( AST__INTER, "Issue(memory): The memory address %p "
                   "has already been issued (internal AST programming "
                   "error).", new );
         break;
      }
   }

/* If OK, extend the list and add in the new address. */
   if( astOK ) {
      if( siz_issued - nissued < 5 ) {
         old_size = siz_issued * sizeof( Memory * );
         siz_issued += 100;
         new_size = siz_issued * sizeof( Memory * );
         new_issued = malloc( new_size );
         if( new_issued ) {
            if( issued ) {
               memcpy( new_issued, issued, old_size );
               free( issued );
            }
            issued = new_issued;
         } else {
            astError( AST__INTER, "Issue(memory): Failed to allocat %d "
                      "bytes of memory.", new_size );
         }
      }
      if( astOK ) issued[ nissued++ ] = new;
   }
}

static void DeIssue( Memory *old ) {
/*
*  Name:
*     Issue

*  Purpose:
*     Note that a dynamic memory address has been freed.

*  Type:
*     Private function.

*  Synopsis:
*     #include "memory.h"
*     void DeIssue( Memory *old )

*  Description:
*     This function removes the supplied pointer from a static array,
*     so that checks on memory leakage can be performed. AN error is
*     reported if the supplied pointer has not previously been issued.

*  Parameters:
*     old
*        The memory pointer.

*  Notes:
*     - This function attempts to execute even if an error has occurred.

*/

/* Local Variables: */
   int i;
   int ok;

/* Check a pointer was supplied. */
   if( !old ) return;

/* Search for the supplied pointer in the list of issued addresses. */
   ok = 0;
   for( i = 0; i < nissued; i++ ) {
      if( old == issued[ i ] ){

/* When found, set it NULL. */
         issued[ i ] = NULL;
         ok =1;
         break;
      }
   }

/* If not found, report an error. */
   if( !ok && astOK ) {
      astError( AST__INTER, "DeIssue(memory): The memory address %p "
                "was not issued by AST (internal AST programming "
                "error).", old );
   }
}


void astListIssued_( const char *label ) {
/*
*+
*  Name:
*     astListIssued

*  Purpose:
*     Display a list of the currently issued memory structures.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "memory.h"
*     astListIssued( const char *label )

*  Description:
*     This function displays a list of the identifiers for all currently
*     issued AST memory chunks. The list is written to standard output.

*  Parameters:
*     label
*        A textual label to display before the memody id values (may be
*        NULL).

*  Notes:
*     - This function attempts to execute even if an error has occurred.
*-
*/

   int i;
   int first;
   Memory *list;

   list = ( (Memory *) issued ) - 1;

   if( label ) printf("%s: ", label );
   first = 1;

   for( i = 0; i < nissued; i++ ) {
      if( issued[ i ] && !issued[ i ]->perm ) {
         if( first ) {
            printf("Currently issued AST memory chunks:\n");
            first = 0;
         }
         printf( "%d ", issued[ i ]->id );
      }
   }      

   if( first ) {
      printf("There are currently no issued AST memory chunks.");
   }
   printf("\n");

}

void astSetWatchId_( int id ) {
/*
*+
*  Name:
*     astSetWatchId

*  Purpose:
*     Indicate an error is to be reported when a specified chunk is issued.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "memory.h"
*     astSetWatchId( int id )

*  Description:
*     This function forces an error to be reported (via astError) when a
*     specified memory chunk is issued. By setting a debugger breakpoint 
*     on astError, the moment at which the specified memory chunk is
*     issued can be trapped, and the cause of the memory allocation 
*     determined by examining the call stack.

*  Parameters:
*     id
*        The identifier of the chunk which is to be weatched for.

*  Notes:
*     - This function attempts to execute even if an error has occurred.
*-
*/
   watch_id = id;
}

int astSetPermMem_( int perm ){
/*
*+
*  Name:
*     astSetPermMem

*  Purpose:
*     Indicate subsequent chunks are part of an acceptable memory leak.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "memory.h"
*     int astSetPermMem( int perm )

*  Description:
*     This function returns the current setting of the "perm" flag, and
*     then stores the supplied value as the new value.
*
*     If the "perm" flag is set non-zero, then subsequently allocated 
*     memory chunks are not added to the list of allocated memory chunks
*     and so do not appear when astListIssued is called. The "perm" flag
*     shoul dbe set non-zero before making any memory allocation which
*     forms part of an acceptable once-off "memory leak". For instance,
*     the memory used to hold pointers to destructors, dump functions and 
*     copy constructors within the Object virtual function table is never
*     released and forms part of an acceptable once-off memory leak.

*  Parameters:
*     perm
*        The new value for the "perm" flag.

*  Returned Value:
*     The original value of the "perm" flag.

*  Notes:
*     - This function attempts to execute even if an error has occurred.
*-
*/
   int old_perm;
   old_perm = perm_mem;
   perm_mem = perm;
   return old_perm;
}

int astGetMemId_( void *ptr ){
/*
*+
*  Name:
*     astGetMemId

*  Purpose:
*     Return the unique identifier associated with a given memory block.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "memory.h"
*     int astGetMemId( void *ptr )

*  Description:
*     This function returns the integer identiier associated with the
*     given memory block, which should have been allocated using one of
*     the AST memory management function defined in this module.

*  Parameters:
*     ptr
*        Pointer to the memory block.

*  Returned Value:
*     The integer identifier for the supplied memory block.

*  Notes:
*     - This function attempts to execute even if an error has occurred.
*     - An invalid identifier of -1 is returned if an error occurs.
*-
*/
/* Local Variables: */
   Memory *mem;                  /* Pointer to memory header */
   int result;                   /* The returned identifier value */

/* Initialise */
   result = -1;

/* If the incoming pointer is NULL, do nothing. Otherwise, check if it
   points at dynamically allocated memory (IsDynamic sets the global
   error status if it does not). */
   if ( ptr && IsDynamic( ptr ) ) {

/* If OK, obtain a pointer to the memory header and return the identifier. */
      mem = ( (Memory *) ptr ) - 1;
      result = mem->id;
   }

/* Return the result. */
   return result;
}

void astMemCheckId_( int id, void (*fun)( void *ptr ) ){
/*
*+
*  Name:
*     astMemCheckId

*  Purpose:
*     Cause a function to be executed at frequent intervals.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "memory.h"
*     void astMemCheckId( int id, void (*fun)( void * ) ){

*  Description:
*     This function causes the supplied "fun" function to be invoked when
*     ever a AST memory management function is invoked (actually when the 
*     Magic function is invoked), so long as the given memory block idenifier
*     is current active. Each time the function is invoked, it is passed a 
*     pointer to the memory block with the supplied identifier.
*
*     Note, if the memory pointer passed to the supplied function is a
*     pointer to an AST object, the object may be in an incomplete state
*     of construction and consequently may not be usable.

*  Parameters:
*     id
*        The identifier for the memory block to be passed to the function.
*     fun
*        The function to call.

*-
*/

   memcheckfun = fun;
   memcheckid = id;
}

void *astFindIdPtr_( int id ){
/*
*+
*  Name:
*     astFindIdPtr

*  Purpose:
*     Return a pointer to the memory block with a given identifier.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "memory.h"
*     void *astFindIdPtr( int id )

*  Description:
*     This function returns a pointer to the memory block with a given
*     identifier. NULL is returned if the given identifier is not active.

*  Parameters:
*     id
*        The identifier.

*  Returned Value:
*     The pointer to the memory block.

*-
*/
/* Local Variables: */
   Memory *list;
   int i;
   void *result;                 

/* Initialise */
   result = NULL;

/* Loop round all Memory structures which are on the list of issued
   blocks. */
   list = ( (Memory *) issued ) - 1;
   for( i = 0; i < nissued; i++ ) {

/* Set the returned pointer and leave the loop when one is found with the 
   supplied identifier. */
      if( issued[ i ] && issued[ i ]->id == id ) {
         result = issued[ i ] + 1;
         break;
      } 
   }

/* Return the result. */
   return result;
}



#endif

