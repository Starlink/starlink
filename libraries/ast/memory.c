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

*     Note, it is assumed that malloc, free and realloc are thread-safe.

*  Copyright:
*     Copyright (C) 1997-2006 Council for the Central Laboratory of the
*     Research Councils
*     Copyright (C) 2009-2010 Science & Technology Facilities Council.
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
*     26-JAN-2004 (DSB):
*        Modified astRealloc to clarify the nature of the returned pointer
*        (which is not a "Memory *"). Also correct issuing and deissuing
*        of pointers in DEBUG code within astRealloc.
*     16-FEB-2006 (DSB):
*        Convert Magic from a function to a macro for extra speed.
*     21-FEB-2006 (DSB):
*        Convert IsDynamic from a function to a macro for extra speed.
*     23-FEB-2006 (DSB):
*        Added the caching system for allocated but unused memory blocks,
*        controlled by AST tuning parameter MemoryCaching.
*     2-MAR-2006 (DSB):
*        Added astFlushMemory, and renamed the memory debugging functions.
*        These are now conditionally compiled if the MEM_DEBUG macros is
*        defined (set by configuring AST with the --with-memdebug option).
*        Also modified them to take into account MemoryCaching.
*     24-MAY-2006 (DSB):
*        Ensure that pointers to memory returned by this module are all
*        aligned on 8 byte boundaries. This fixes problems with ualigned
*        memory access that could cause bus errors on Solaris.
*     26-MAY-2006 (DSB):
*        Cast (void *) pointers to (char *) before doing arithmetic on
*        them (changes supplied by Micah Johnson).
*     4-DEC-2006 (DSB):
*        Fix bug in astMalloc that caused a non-null pointer to be
*        returned on error.
*     4-JAN-2007 (DSB):
*        Move definition of astCLASS macro so that it comes before the
*        inclusion of the AST include files (which test for astCLASS).
*     27-JUN-2007 (DSB):
*        Added astIsDynamic.
*     24-OCT-2007 (DSB):
*        Zero the size of memory blocks stored in the Cache so that an
*        error will be reported if an attempt is made to free a memory
*        block that has already been freed.
*     25-OCT-2007 (DSB):
*        Added astRemoveLeadingBlanks.
*     28-FEB-2008 (DSB):
*        Added astChrSub.
*     17-MAR-2008 (DSB):
*        Added "{nnn}" quantifier to astChrSub.
*     27-MAR-2008 (DSB):
*        Added astChrSplitRE, and re-structured regexp functions.
*     18-NOV-2008 (DSB):
*        In astFlushMemory, do not release permanent memory blocks as
*        they may still be needed.
*     9-FEB-2009 (DSB):
*        Added astChr2Double.
*     25-JUN-2009 (DSB):
*        Fix handling of escape characters in astSplitC.
*     19-MAY-2010 (DSB):
*        - Added astStringCase.
*        - Changed access from protected to public for commonly used
*        functions.
*     26-MAY-2010 (DSB):
*        Added astCalloc.
*     18-AUG-2010 (DSB):
*        Added astMemoryStats
*     19-AUG-2010 (DSB):
*        Added astMemoryWarning
*     8-OCT-2010 (DSB):
*        Modify memory allocation to use "calloc" directly, rather than
*        using "malloc+memset".
*     12-APR-2011 (DSB):
*        Fix regular expression problem where a ".*" template field failed to
*        match a null string if it occurred before a closing parenthesis at
*        the end of the template.
*     26-MAY-2011 (DSB):
*        - Changed API for astCalloc to match RTL (i.e. remove "init").
*        - Changed astChr2Double to check for strigs like "2.", which
*        some sscanfs fail to read as a floating point value.
*     27-MAY-2011 (DSB):
*        Added astFreeDouble to free a dynamically allocated array of
*        pointers to other dynamically allocated arrays.
*     21-JUN-2011 (DSB):
*        Added astCheckMemory - an alternative to astFlushMemory that does
*        not free any memory.
*     21-NOV-2011 (DSB):
*        Correct matchend value returned by astChrSplitRE.
*     6-JAN-2014 (DSB):
*        Optimise access to cache to avoid valgrind warnings.
*/

/* Configuration results. */
/* ---------------------- */
#if HAVE_CONFIG_H
#include <config.h>
#endif

/* Module Macros. */
/* ============== */
/* Define the astCLASS macro (even although this is not a class
   implementation) to obtain access to the protected error handling
   functions. */
#define astCLASS memory

/* The maximum number of fields within a format string allowed by astSscanf. */
#define VMAXFLD 20

/* The maximum number of nested astBeginPM/astEndPM contexts. */
#define PM_STACK_MAXSIZE 20

/* Select the appropriate memory management functions. These will be the
   system's malloc, calloc, free and realloc unless AST was configured with
   the "--with-starmem" option, in which case they will be the starmem
   malloc, calloc, free and realloc. */
#ifdef HAVE_STAR_MEM_H
#  include <star/mem.h>
#  define MALLOC starMalloc
#  define CALLOC starCalloc
#  define FREE starFree
#  define REALLOC starRealloc
#else
#  define MALLOC malloc
#  define CALLOC calloc
#  define FREE free
#  define REALLOC realloc
#endif


#ifdef MEM_DEBUG
#define ISSUED "issued"
#define FREED "freed"
#endif

/* Include files. */
/* ============== */
/* Interface definitions. */
/* ---------------------- */
#include "error.h"               /* Error reporting facilities */
#include "globals.h"             /* Thread-specific global data */
#include "memory.h"              /* Interface to this module */
#include "pointset.h"            /* For AST__BAD */

#ifdef MEM_DEBUG
#include "object.h"              /* For astMakePointer */
#endif

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
#include <limits.h>

#ifdef THREAD_SAFE
#include <pthread.h>
#endif

#ifdef MEM_PROFILE
#include <sys/times.h>
#endif

/* Function Macros. */
/* =============== */
/* These are defined as macros rather than functions to avoid the
   overhead of a function call since they are called extremely frequently. */

/*
*  Name:
*     IS_DYNAMIC

*  Purpose:
*     Test whether a memory region has been dynamically allocated.

*  Type:
*     Private macro

*  Synopsis:
*     #include "memory.h"
*     IS_DYNAMIC( ptr, dynamic )

*  Description:
*     This macro takes a pointer to a region of memory and tests if
*     the memory has previously been dynamically allocated using other
*     functions from this module. It does this by checking for the
*     presence of a "magic" number in the header which precedes the
*     allocated memory. If the magic number is not present (or the
*     pointer is invalid for any other reason), an error is reported
*     and the global error status is set.
*
*     The result of the test is written to the variable specified by "res".

*  Parameters:
*     ptr
*        Pointer to the start (as known to the external user) of the
*        dynamically allocated memory.
*     dynamic
*        Name of an "int" variable to recieve the result of the test.
*        If the memory was allocated dynamically, a value of 1 is
*        stored in this variable.  Otherwise, zero is stored and an error
*        results.

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

#define IS_DYNAMIC(ptr,dynamic) \
\
/* Initialise. */ \
   dynamic = 0; \
\
/* Check that a NULL pointer has not been supplied and report an error \
   if it has (but not if the global status is already set). */ \
   if ( !ptr ) { \
      if ( astOK ) { \
         astError( AST__PTRIN, "Invalid NULL pointer (address %p).", status, ptr ); \
      } \
\
/* If OK, derive a pointer to the memory header that precedes the \
   allocated region of memory. */ \
   } else { \
      Memory *isdynmem;                /* Pointer to memory header */ \
      isdynmem = (Memory *) ( (char *) ptr - SIZEOF_MEMORY ); \
\
/* Check if the "magic number" in the header is valid and report an \
   error if it is not (but not if the global status is already \
   set). */ \
      if ( isdynmem->magic != MAGIC( isdynmem, isdynmem->size ) ) { \
         if ( astOK ) { \
            astError( AST__PTRIN, \
                      "Invalid pointer or corrupted memory at address %p.", status, \
                      ptr ); \
         } \
\
/* Note if the magic number is OK. */ \
      } else { \
         dynamic = 1; \
      } \
   }



/*
*  Name:
*     MAGIC

*  Purpose:
*     Generate a "magic number".

*  Type:
*     Private macro.

*  Synopsis:
*     #include "memory.h"
*     unsigned long MAGIC( void *ptr, size_t size )

*  Description:
*     This macro generates a "magic number" which is a function of
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
#define MAGIC(ptr,size) \
   ( ~( ( ( (unsigned long) ptr ) ^ ( (unsigned long) size ) ) + \
             ( (unsigned long) 1 ) ) )

/* A macro that returns the size of the a Memory structure padded to a
   multiple of 8 bytes. */
#define SIZEOF_MEMORY \
   ( ( sizeof_memory != 0 ) ? sizeof_memory : SizeOfMemory( status ) )


/* Type Definitions. */
/* ================= */

#ifdef MEM_PROFILE

/* Structure used to record the time spent between matching calls to
   astStartTimer and astStopTimer. */
typedef struct AstTimer {
   int id;                    /* Unique integer identifier for timer */
   clock_t e0;                /* Absolute elapsed time at timer start */
   clock_t u0;                /* Absolute user time at timer start */
   clock_t s0;                /* Absolute system time at timer start */
   clock_t et;                /* Cumulative elapsed time within timer */
   clock_t ut;                /* Cumulative user time within timer */
   clock_t st;                /* Cumulative system time within timer */
   int nentry;                /* Number of entries into the timer */
   const char *name;          /* An identifying label for the timer */
   const char *file;          /* Name of source file where timer was started */
   int line;                  /* Source file line no. where timer was started */
   struct AstTimer *parent;   /* The parent enclosing timer */
   int nchild;                /* Number of child timers */
   struct AstTimer **children;/* Timers that count time within this timer */
} AstTimer;

#endif

/* Module Variables. */
/* ================= */

/* Extra stuff for profiling (can only be used in single threaded
   environments). */
#ifdef MEM_PROFILE
static AstTimer *Current_Timer = NULL;
static int Enable_Timers = 0;
static int Timer_Count = 0;
#endif

/* Extra stuff for debugging of memory management (tracking of leaks
   etc). */
#ifdef MEM_DEBUG

/* The identifier for the memory block which is to be tracked. */
static int Watched_ID = -1;

/* The next integer to use to identify an active memory block pointer. */
static int Next_ID = -1;

/* Indicates if future memory allocations are permanent (i.e. will not
   usually be freed explicitly by AST). */
static int Perm_Mem = 0;

/* A "first in, last out" stack of Perm_Mem values used by nested
   astBeginPM/astEndPM contexts. */
static int PM_Stack[ PM_STACK_MAXSIZE ];

/* The number of values currently in the PM_Stack array. */
static int PM_Stack_Size = 0;

/* A pointer to a double linked list holding pointers to currently active
   memory blocks (i.e. memory blocks for which a pointer has been issued
   but not yet freed). This does not include the memory blocks in the
   Cache array (these are not considered to be active). */
static Memory *Active_List = NULL;

/* Should a new ID be issued each time a cached memory block is returned
   by astMalloc? Otherwise, the same ID value is used throughout the
   life of a memory block. */
static int Keep_ID = 0;

/* Suppress all memory use reports except for issuing and freeing? */
static int Quiet_Use = 0;

/* Report the ID of every cached block when the cache is emptied? */
static int List_Cache = 0;

/* Memory allocation at which to issue a warning. */
static size_t Warn_Usage = 0;

/* Current memory allocated by AST. */
static size_t Current_Usage = 0;

/* Peak memory allocated by AST. */
static size_t Peak_Usage = 0;

#ifdef THREAD_SAFE
static pthread_mutex_t mutex2 = PTHREAD_MUTEX_INITIALIZER;
#define LOCK_DEBUG_MUTEX pthread_mutex_lock( &mutex2 );
#define UNLOCK_DEBUG_MUTEX pthread_mutex_unlock( &mutex2 );
#else
#define LOCK_DEBUG_MUTEX
#define UNLOCK_DEBUG_MUTEX
#endif

#endif

/* Define macros for accessing all items of thread-safe global data
   used by this module. */
#ifdef THREAD_SAFE

#define sizeof_memory astGLOBAL(Memory,Sizeof_Memory)
#define cache astGLOBAL(Memory,Cache)
#define cache_init astGLOBAL(Memory,Cache_Init)
#define use_cache astGLOBAL(Memory,Use_Cache)

/* Define the initial values for the global data for this module. */
#define GLOBAL_inits \
   globals->Sizeof_Memory = 0; \
   globals->Cache_Init = 0; \
   globals->Use_Cache = 0; \

/* Create the global initialisation function. */
astMAKE_INITGLOBALS(Memory)

/* If thread safety is not needed, declare globals at static variables. */
/* -------------------------------------------------------------------- */
#else

/* The size of a Memory header structure, padded to a multiple of 8
   bytes. This value is initialised by the SizeOfMemory function, and
   should be accessed using the SIZEOF_MEMORY macro. */
static size_t sizeof_memory = 0;

/* A cache of allocated but currently unused memory block. This cache is
   maintained in order to avoid the overhead of continual calls to malloc to
   allocate small blocks of memory. The vast majority of memory blocks
   allocated by AST are under 200 bytes in size. Each element in this array
   stores a pointer to the header for a free (i.e. allocated but currently
   unused) memory block. The size of the memory block (not including the
   Memory header) will equal the index at which the pointer is stored within
   "cache". Each free memory block contains (in its Memory header) a pointer
   to the header for another free memory block of the same size (or a NULL
   pointer if there are no other free memory blocks of the same size). */
static Memory *cache[ MXCSIZE + 1 ];

/* Has the "cache" array been initialised? */
static int cache_init = 0;

/* Should the cache be used? */
static int use_cache = 0;

#endif

/* Prototypes for Private Functions. */
/* ================================= */
static size_t SizeOfMemory( int * );
static char *CheckTempStart( const char *, const char *, const char *, char *, int *, int *, int *, int *, int *, int *, int *, int * );
static char *ChrMatcher( const char *, const char *, const char *, const char *, const char *[], int, int, int, char ***, int *, const char **, int * );
static char *ChrSuber( const char *, const char *, const char *[], int, int, char ***, int *, const char **, int * );

#ifdef MEM_DEBUG
static void Issue( Memory *, int * );
static void DeIssue( Memory *, int * );
#endif

#ifdef MEM_PROFILE
static AstTimer *ReportTimer( AstTimer *, int, AstTimer **, int *, int * );
static int CompareTimers( const void *, const void * );
static int CompareTimers2( const void *, const void * );
#endif

/* Function implementations. */
/* ========================= */
char *astAppendString_( char *str1, int *nc, const char *str2, int *status ) {
/*
*++
*  Name:
*     astAppendString

*  Purpose:
*     Append a string to another string which grows dynamically.

*  Type:
*     Public function.

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
*        has been allocated using an AST memory allocation function.
*        If no space has yet been allocated for this string, a NULL
*        pointer may be given and fresh space will be allocated by this
*        function.
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
*     astAppendString()
*        A possibly new pointer to the dynamic string with the new string
*        appended (its location in memory may have to change if it has to
*        be extended, in which case the original memory is automatically
*        freed by this function). When the string is no longer required,
*        its memory should be freed using astFree.

*  Notes:
*     - If this function is invoked with the global error status set
*     or if it should fail for any reason, then the returned pointer
*     will be equal to "str1" and the dynamic string contents will be
*     unchanged.
*--
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

void *astCalloc_( size_t nmemb, size_t size, int *status ) {
/*
*++
*  Name:
*     astCalloc

*  Purpose:
*     Allocate and initialise memory.

*  Type:
*     Public function.

*  Synopsis:
*     #include "memory.h"
*     void *astCalloc( size_t nmemb, size_t size )

*  Description:
*     This function allocates memory in a similar manner to the
*     standard C "calloc" function, but with improved security
*     (against memory leaks, etc.) and with error reporting. It also
*     fills the allocated memory with zeros.
*
*     Like astMalloc, it allows zero-sized memory allocation
*     (without error), resulting in a NULL returned pointer value.

*  Parameters:
*     nmemb
*        The number of array elements for which memory is to be allocated.
*     size
*        The size of each array element, in bytes.

*  Returned Value:
*     astCalloc()
*        If successful, the function returns a pointer to the start of
*        the allocated memory region. If the size allocated is zero, this
*        will be a NULL pointer.

*  Notes:
*     - A pointer value of NULL is returned if this function is
*     invoked with the global error status set or if it fails for any
*     reason.
*--
*/
/* Local Variables: */
   void *result;    /* Returned pointer */

/* Initialise. */
   result = NULL;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Attempt to allocate and initialise the required amount of memory. */
   result = astMallocInit( nmemb*size );

/* If the above call failed due to failure of the system malloc function,
   issue an extra error giving the number of elements and element size. */
   if( astStatus == AST__NOMEM ) {
      astError( AST__NOMEM, "(%lu elements, each of %lu bytes).", status,
                (unsigned long) nmemb, (unsigned long) size );
   }

/* Return the result. */
   return result;
}

static char *CheckTempStart( const char *template, const char *temp,
                             const char *pattern,
                             char *allowed, int *ntemp, int *allow,
                             int *min_nc, int *max_nc, int *start_sub,
                             int *end_sub, int *greedy, int *status ){
/*
*  Name:
*     CheckTempStart

*  Purpose:
*     Examine the leading field in an astChrSub template.

*  Type:
*     Private function.

*  Synopsis:
*     char *CheckTempStart( const char *template, const char *temp,
*                           const char *pattern,
*                           char *allowed, int *ntemp, int *allow,
*                           int *min_nc, int *max_nc, int *start_sub,
*                           int *end_sub, int *greedy, int *status )

*  Description:
*     This function returns inforation about the leading field in a
*     template string supplied to astChrSub.

*  Parameters:
*     template
*        The full template string (used for error messages).
*     temp
*        Pointer to the next character to read from the template string.
*     pattern
*        Pointer to the user supplied pattern string (only used in error
*        messages).
*     allowed
*        Pointer to a buffer in which to store a string of characters
*        that the leading temeplate field will match. A NULL pointer may
*        be supplied in which case new memory will be allocated. The
*        supplied memory is expanded as necessary, and a pointer to it is
*        returned as the function value.
*     ntemp
*        Address of an int in which to return the number of characters
*        consumed from the start of "temp".
*     allow
*        Address of an int in which to return a flag which is non-zero if
*        the returned string contains characters that are allowed in the
*        test field, or zero if the returned string contains characters that
*        are disallowed in the test field.
*     min_nc
*        Address of an int in which to return the minimum number of
*        test characters that must belong to the returned set of
*        allowed characters.
*     max_nc
*        Address of an int in which to return the maximum number of
*        test characters that must belong to the returned set of
*        allowed characters.
*     start_sub
*        Address of an int in which to return a flag which is non-zero if
*        the leading template field indicates the start of a field to be
*        substituted. In this case the supplied "allowed" pointer is
*        returned without change as the function value, "Min_nc" is
*        returned as zero, and max_nc is returned as zero.
*     end_sub
*        Address of an int in which to return a flag which is non-zero if
*        the leading template field indicates the end of a field to be
*        substituted. In this case the supplied "allowed" pointer is
*        returned without change as the function value, "Min_nc" is
*        returned as zero, and limit is returned as zero.
*     greedy
*        Address of an int in which to return a flag which is non-zero if
*        the template starts with a greedy quantifier.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     Pointer to a (possibly newly allocated) memory area holding a
*     string of characters that the leading temeplate field will match.
*     This string should be released using astFree when no longer needed.
*     If a NULL pointyer is returned, then all characters are allowed
*     (or disallowed if "*allow" is zero).

*  Notes:
*     - The returned value is also stored in the module variable
*     sizeof_memory.
*/

/* Local Variables: */
   char *result;
   const char *start;
   const char *end;

/* Initialise. */
   result = allowed;
   *ntemp = 0;
   *allow = 1;
   *min_nc = 0;
   *max_nc = 0;
   *start_sub = 0;
   *end_sub = 0;
   *greedy = 1;

/* Check global status */
   if( !astOK ) return result;

/* If the next character is an opening parenthesis, this marks the start
   of a substitution field. */
   if( *temp == '(' ) {
      *start_sub = 1;
      *ntemp = 1;

/* If the next character is an closing parenthesis, this marks the end
   of a substitution field. */
   } else if( *temp == ')' ) {
      *end_sub = 1;
      *ntemp = 1;

/* If the next character is an opening bracket, this marks the start of a
   field of allowed or disallowed characters. */
   } else {
      if( *temp == '[' ) {

/* If the first character in the brackets is "^" this is a field of
   disallowed characters, otherwise they are allowed. */
         if( temp[ 1 ] == '^' ) {
            *allow = 0;
            start = temp + 2;
         } else {
            start = temp + 1;
         }

/* Get a pointer to the closing bracket. */
         end = strchr( temp, ']' );

/* Copy the intervening string into the returned string. */
         if( end ) {
            result = astStore( allowed, start, end - start + 1 );
            if( result ) result[ end - start  ] = 0;

/* Report an error if no closing bracket was found. */
         } else {
            astError( AST__BADSUB, "Invalid pattern matching template \"%s\": "
                      "missing ']'.", status, pattern );
         }

/* Indicate how many template characters have been used. */
         *ntemp = end - temp + 1;

/* A single dot matches any character. */
      } else if( *temp == '.' ) {
         result = astFree( result );
         *ntemp = 1;

/* Now deal with escape sequences. */
      } else if( *temp == '\\' ) {

/* Digits... */
         if( temp[ 1 ] == 'd' || temp[ 1 ] == 'D' ) {
            result = astStore( allowed, "0123456789", 11 );
            result[ 10 ] = 0;
            if( temp[ 1 ] == 'D' ) *allow = 0;

/* White space... */
         } else if( temp[ 1 ] == 's' || temp[ 1 ] == 'S' ) {
            result = astStore( allowed, " 	\n\r", 5 );
            result[ 4 ] = 0;
            if( temp[ 1 ] == 'S' ) *allow = 0;

/* Word characters... */
         } else if( temp[ 1 ] == 'w' || temp[ 1 ] == 'W' ) {
            result = astStore( allowed, "abcdefghijklmnopqrstuvwxyz"
                               "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_",
                               64 );
            result[ 63 ] = 0;
            if( temp[ 1 ] == 'W' ) *allow = 0;

/* Any other character is treated literally. */
         } else {
            result = astStore( allowed, temp + 1, 2 );
            result[ 1 ] = 0;
         }

/* Set number of template characters consumed. */
         *ntemp = 2;

/* Everything else must be matched literally. */
      } else {

         if( *temp == '*' || *temp == '?' || *temp == '+' ){
            astError( AST__BADSUB, "Invalid pattern matching template \"%s\": "
                      "field starts with '%c'.", status, pattern, temp[ *ntemp ] );
         } else {
            result = astStore( allowed, temp, 2 );
            result[ 1 ] = 0;
            *ntemp = 1;
         }

      }

/* Now see if there is any quantifier. */
      if( temp[ *ntemp ] == '*' ) {
         *min_nc = 0;
         *max_nc = INT_MAX;
         (*ntemp)++;
         if( temp[ *ntemp ] == '?' ){
            *greedy = 0;
            (*ntemp)++;
         }

      } else if( temp[ *ntemp ] == '+' ) {
         *min_nc = 1;
         *max_nc = INT_MAX;
         (*ntemp)++;
         if( temp[ *ntemp ] == '?' ){
            *greedy = 0;
            (*ntemp)++;
         }

      } else if( temp[ *ntemp ] == '?' ) {
         *min_nc = 0;
         *max_nc = 1;
         (*ntemp)++;

      } else {

/* See if the remaining string starts with "{nnn}". If so, extract the
   "nnn" and use it as the minimum and maximum field length. */
         if( temp[ *ntemp ] == '{' ) {

            start = temp + *ntemp + 1;
            while( isdigit( (int) *start ) ) {
               *min_nc = 10*( *min_nc ) + (int )( ( *start ) - '0' );
               start++;
            }

            if( *start == '}' ) {
               *max_nc = *min_nc;
               *ntemp = start - temp + 1;
            } else {
               start = NULL;
            }

         } else {
            start = NULL;
         }

/* If the remaining string does not start with "{nnn}", use a minimum and
   maximum field length of 1. */
         if( !start ) {
            *min_nc = 1;
            *max_nc = 1;
         }
      }
   }

/* Return the string of allowed characters. */
   return result;
}

double astChr2Double_( const char *str, int *status ) {
/*
*++
*  Name:
*     astChr2Double

*  Purpose:
*     read a double value from a string.

*  Type:
*     Public function.

*  Synopsis:
*     #include "memory.h"
*     double astChr2Double( const char *str )

*  Description:
*     This function reads a double from the supplied null-terminated string,
*     ignoring leading and trailing white space. AST__BAD is ereturned
*     without error if the string is not a numerical value.

*  Parameters:
*     str
*        Pointer to the string.

*  Returned Value:
*     astChr2Double()
*       The double value, or AST__BAD.

*  Notes:
*     -  A value of AST__BAD is returned if this function is invoked with
*     the global error status set or if it should fail for any reason.
*--
*/

/* Local Variables: */
   double result;     /* The returned value */
   int ival;          /* Integer value read from string */
   int len;           /* Length of supplied string */
   int nc;            /* Number of characters read from the string */

/* Check the global error status and supplied pointer. */
   if ( !astOK || !str ) return AST__BAD;

/* Save the length of the supplied string. */
   len = strlen( str );

/* Use scanf to read the floating point value. This fails if either 1) the
   string does not begin with a numerical value (in which case astSscanf
   returns zero), or 2) there are non-white characters following the
   numerical value (in which case "nc" - the number of characters read from
   the string - is less than the length of the string). */
   if ( nc = 0,
        ( 1 != astSscanf( str, " %lg %n", &result, &nc ) ) || ( nc < len ) ) {
      result = AST__BAD;
   }

/* If the above failed, try again allowing the string to be an integer
   followed by a dot (e.g. "1."). Some implementations of sscanf do not
   consider this to be a floating point value. */
   if( 1 || result == AST__BAD ) {
      if ( nc = 0,
           ( 1 == astSscanf( str, " %d. %n", &ival, &nc ) ) && ( nc >= len ) ) {
         result = ival;
      }
   }

/* Return the result. */
   return result;
}

void astChrCase_( const char *in, char *out, int upper, int blen, int *status ) {
/*
*++
*  Name:
*     astChrCase

*  Purpose:
*     Convert a string to upper or lower case.

*  Type:
*     Public function.

*  Synopsis:
*     #include "memory.h"
*     void astChrCase( const char *in, char *out, int upper, int blen, int *status )

*  Description:
*     This function converts a supplied string to upper or lower case,
*     storing the result in a supplied buffer. The astStringCase function
*     is similar, but stores the result in a dynamically allocated buffer.

*  Parameters:
*     in
*        Pointer to the null terminated string to be converted. If this
*        is NULL, the supplied contents of the "out" string are used as
*        the input string.
*     out
*        Pointer to the buffer to receive the converted string. The
*        length of this buffer is given by "blen". If NULL is supplied
*        for "in", then the supplied contents of "out" are converted and
*        written back into "out" over-writing the supplied contents.
*     upper
*        If non-zero, the string is converted to upper case. Otherwise it
*        is converted to lower case.
*     blen
*        The length of the output buffer. Ignored if "in" is NULL. No
*        more than "blen - 1" characters will be copied from "in" to
*        "out", and a terminating null character will then be added.

*--
*/

/* Local Variables: */
   const char *pin;
   char *pout;
   int i;

/* Check the global error status. */
   if ( !astOK ) return;

/* The simple case of over-writing the supplied string. */
   if( ! in ) {
      pout = out - 1;
      while( *(++pout) ) *pout = toupper( (int) *pout );

/* If a separate output buffer has been supplied... */
   } else {

/* Initialise pointers to the input and output buffers. */
      pin = in;
      pout = out;

/* Copy the string character by character, converting the case in the
   process. Start counting from 1, not 0, in order to ensure that we are
   left with room for a terminating null. */
      for( i = 1; i < blen && *pin; i++ ) {
         *(pout++) = toupper( (int) *(pin++) );
      }

/* Terminate the returned string. */
      *pout = 0;
   }
}

int astChrMatch_( const char *str1, const char *str2, int *status ) {
/*
*++
*  Name:
*     astChrMatch

*  Purpose:
*     Case insensitive string comparison.

*  Type:
*     Public function.

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
*     astChrMatch()
*        Non-zero if the two strings match, otherwise zero.

*  Notes:
*     -  A value of zero is returned if this function is invoked with the
*     global error status set or if it should fail for any reason.
*--
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

int astChrMatchN_( const char *str1, const char *str2, size_t n, int *status ) {
/*
*++
*  Name:
*     astChrMatchN

*  Purpose:
*     Case insensitive string comparison of at most N characters

*  Type:
*     Public function.

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
*     astChrMatchN()
*        Non-zero if the two strings match, otherwise zero.

*  Notes:
*     -  A value of zero is returned if this function is invoked with the
*     global error status set or if it should fail for any reason.
*--
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

char **astChrSplit_( const char *str, int *n, int *status ) {
/*
*++
*  Name:
*     astChrSplit

*  Purpose:
*     Extract words from a supplied string.

*  Type:
*     Public function.

*  Synopsis:
*     #include "memory.h"
*     char **astChrSplit_( const char *str, int *n )

*  Description:
*     This function extracts all space-separated words form the supplied
*     string and returns them in an array of dynamically allocated strings.

*  Parameters:
*     str
*        Pointer to the string to be split.
*     n
*        Address of an int in which to return the number of words returned.

*  Returned Value:
*     astChrSplit()
*        A pointer to a dynamically allocated array containing "*n" elements.
*        Each element is a pointer to a dynamically allocated character
*        string containing a word extracted from the supplied string. Each
*        of these words will have no leading or trailing white space.

*  Notes:
*     -  A NULL pointer is returned if this function is invoked with the
*     global error status set or if it should fail for any reason, or if
*     the supplied string contains no words.
*--
*/

/* Local Variables: */
   char **result;
   char *w;
   const char *p;
   const char *ws;
   int first;
   int state;
   int wl;

/* Check the global error status. */
   if ( !astOK ) return NULL;

/* Initialise. */
   result = NULL;
   ws = NULL;
   *n = 0;

/* State 0 is "looking for the next non-white character which marks the
   start of the next word". State 1 is "looking for the next white character
   which marks the end of the current word". */
   state = 0;

/* Loop through all characters in the supplied string, including the
   terminating null. */
   p = str - 1;
   first = 1;
   while( *(p++) || first ) {
      first = 0;

/* If this is the terminating null or a space, and we are currently looking
   for the end of a word, allocate memory for the new word, copy the text
   in, terminate it, extend the returned array by one element, and store
   the new word in it. */
      if( !*p || isspace( *p ) ) {
         if( state == 1 ) {
            wl = p - ws;
            w = astMalloc( wl + 1 );
            if( w ) {
               strncpy( w, ws, wl );
               w[ wl ] = 0;
               result = astGrow( result, *n + 1, sizeof( char * ) );
               if( result ) result[ (*n)++ ] = w;
            }
            state = 0;
         }

/* If this is non-blank character, and we are currently looking for the
   start of a word, note the address of the start of the word, and
   indicate that we are now looking for the end of a word. */
      } else {
         if( state == 0 ) {
            state = 1;
            ws = p;
         }
      }
   }

/* Return the result. */
   return result;
}

char **astChrSplitC_( const char *str, char c, int *n, int *status ) {
/*
*++
*  Name:
*     astChrSplitC

*  Purpose:
*     Split a string using a specified character delimiter.

*  Type:
*     Public function.

*  Synopsis:
*     #include "memory.h"
*     char **astChrSplitC( const char *str, char c, int *n )

*  Description:
*     This function extracts all sub-strings separated by a given
*     character from the supplied string and returns them in an array
*     of dynamically allocated strings. The delimiter character itself
*     is not included in the returned strings.
*
*     Delimiter characters that are preceded by "\" are not used as
*     delimiters but are included in the returned word instead (without
*     the "\").

*  Parameters:
*     str
*        Pointer to the string to be split.
*     c
*        The delimiter character.
*     n
*        Address of an int in which to return the number of words returned.

*  Returned Value:
*     astChrSplitC()
*        A pointer to a dynamically allocated array containing "*n" elements.
*        Each element is a pointer to a dynamically allocated character
*        string containing a word extracted from the supplied string.

*  Notes:
*     -  A NULL pointer is returned if this function is invoked with the
*     global error status set or if it should fail for any reason, or if
*     the supplied string contains no words.
*--
*/

/* Local Variables: */
   char **result;
   char *word;
   const char *p;
   int escaped;
   int wordlen;

/* Initialise returned values. */
   *n = 0;
   result = NULL;

/* Check the global error status. */
   if ( !astOK ) return result;

/* More initialisations. */
   word = NULL;
   wordlen = 0;
   escaped = 0;

/* Loop through all characters in the supplied string, including the
   terminating null. */
   p = str;
   while( *p ) {

/* Is this a delimiter character? */
      if( *p == c ) {

/* If it is escaped, it does not mark the end of a word. Put it into the
   current output buffer instead, overwriting the escape character that
   preceded it. */
         if( escaped ) {
            word[ wordlen - 1 ] = c;

/* The next character is not escaped. */
            escaped = 0;

/* If the delimiter is not escaped, terminate the current word and store
   a pointer to it in the returned array. */
         } else {
            result = astGrow( result, *n + 1, sizeof( char * ) );
            word = astGrow( word, wordlen + 1, 1 );
            if( result && word ) {
               word[ wordlen ] = 0;
               result[ (*n)++ ] = word;
               wordlen = 0;
               word = NULL;
            }
         }

/* If this is not a delimitier character, store it in the returned word. */
      } else {
         word = astGrow( word, wordlen + 1, 1 );
         if( word ) word[ wordlen++ ] = *p;

/* If the current character was escaped, indicate that the next character
   is not escaped. */
         if( escaped ) {
            escaped = 0;

/* If this character is a unescaped backslash, set a flag indicating that the
   next character is escaped. */
         } else if( *p == '\\' ){
            escaped = 1;
         }
      }

/* Move on to the next character. */
      p++;
   }

/* Store the text following the final delimitier. */
   result = astGrow( result, *n + 1, sizeof( char * ) );
   word = astGrow( word, wordlen + 1, 1 );
   if( result && word ) {
      word[ wordlen ] = 0;
      result[ (*n)++ ] = word;
   }

/* Return the result. */
   return result;
}

char **astChrSplitRE_( const char *str, const char *regexp, int *n,
                       const char **matchend, int *status ) {
/*
*++
*  Name:
*     astChrSplitRE

*  Purpose:
*     Extract sub-strings matching a specified regular expression.

*  Type:
*     Public function.

*  Synopsis:
*     #include "memory.h"
*     char **astChrSplitRE( const char *str, const char *regexp, int *n,
*                           const char **matchend )

*  Description:
*     This function compares the supplied string with the supplied
*     regular expression. If they match, each section of the test string
*     that corresponds to a parenthesised sub-string in the regular
*     expression is copied and stored in the returned array.

*  Parameters:
*     str
*        Pointer to the string to be split.
*     regexp
*        The regular expression. See "Template Syntax:" in the astChrSub
*        prologue. Note, this function differs from astChrSub in that any
*        equals signs (=) in the regular expression are treated literally.
*     n
*        Address of an int in which to return the number of sub-strings
*        returned.
*     matchend
*        A pointer to a location at which to return a pointer to the
*        character that follows the last character within the supplied test
*        string that matched any parenthesises sub-section of "regexp". A
*        NULL pointer is returned if no matches were found. A NULL pointer
*        may be supplied if the location of the last matching character is
*        not needed.

*  Returned Value:
*     astChrSplitRE()
*        A pointer to a dynamically allocated array containing "*n" elements.
*        Each element is a pointer to a dynamically allocated character
*        string containing a sub-string extracted from the supplied string.
*        The array itself, and the strings within it, should all be freed
*        using astFree when no longer needed.

*  Notes:
*     - If a parenthesised sub-string in the regular expression is matched
*     by more than one sub-string within the test string, then only the
*     first is returned. To return multiple matches, the regular
*     expression should include multiple copies of the parenthesised
*     sub-string (for instance, separated by ".+?" if the intervening
*     string is immaterial).
*     -  A NULL pointer is returned if this function is invoked with the
*     global error status set or if it should fail for any reason, or if
*     the supplied string contains no words.
*--
*/

/* Local Variables: */
   char **result;
   char *temp;
   int i;

/* Initialise returned values. */
   *n = 0;
   result = NULL;

/* Check global status */
   if( !astOK ) return result;

/* Call ChrSuber to do the work, saving the matching parts of the test
   string. */
   temp = ChrSuber( str, regexp, NULL, 0, 1, &result, n, matchend, status );
   if( temp ) {
      temp = astFree( temp );

/* Free all results if no match was found. */
   } else if( result ) {
      for( i = 0; i < *n; i++ ) result[ i ] = astFree( result[ i ] );
      result = astFree( result );
      *n = 0;
   }

/* Return the result */
   return result;
}

char *ChrSuber( const char *test, const char *pattern, const char *subs[],
                int nsub, int ignore_equals, char ***parts, int *npart,
                const char **matchend, int *status ){
/*
*  Name:
*     ChrSuber

*  Purpose:
*     Performs substitutions on a supplied string.

*  Type:
*     Private function.

*  Synopsis:
*     #include "memory.h"
*     char *ChrSuber( const char *test, const char *pattern,
*                     const char *subs[], int nsub, int ignore_equals,
*                     char ***parts, int *npart, const char **matchend,
*                     int *status )

*  Description:
*     This function performs the work for astChrSub and astChrSplitRE.

*  Parameters:
*     test
*        The string to be tested.
*     pattern
*        The template string. See "Template Syntax:" in the astChrSub
         prologue.
*     subs
*        An array of strings that are to replace the sections of the test
*        string that match each parenthesised sub-string in "pattern". The
*        first element of "subs" replaces the part of the test string that
*        matches the first parenthesised sub-string in the template, etc.
*
*        If "nsub" is zero, then the "subs" pointer is ignored. In this
*        case, and if parameter "ignore_equals" is zero, substitution strings
*        may be specified by appended them to the end of the "pattern" string,
*        separated by "=" characters
*     nsub
*        The number of substitution strings supplied in array "subs".
*     ignore_equals
*        If non-zero, any equals signs in the supplied pattern are
*        treated literally, rather than being used to split the template
*        from any substitution strigs.
*     parts
*        Address of a location at which to return a pointer to an array
*        of character string pointers. The strings are the sub-sections
*        of "test" that matched the parenthesised sub-sections of
*        "template". The array will have "*npart" elements. Ignored if NULL.
*     npart
*        Address of a location at which to return the length of the
*        "parts" array. Ignored if "parts" is NULL.
*     matchend
*        A pointer to a location at which to return a pointer to the
*        character that follows the last character within the supplied test
*        string that matched any parenthesises sub-section of "regexp". A
*        NULL pointer is returned if no matches were found. A NULL pointer
*        may be supplied if the location of the last matching character is
*        not needed.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     A pointer to a dynamically allocated string holding the result
*     of the substitutions, or NULL if the test string does not match
*     the template string. This string should be freed using astFree
*     when no longer needed. If no substituions are specified then a
*     copy of the test string is returned if it matches the template.

*  Notes:
*     -  A NULL pointer is returned if this function is invoked with the
*     global error status set or if it should fail for any reason, or if
*     the supplied test string does not match the template.

*/

/* Local Variables: */
   char **sections;
   char **temps;
   char *cptr;
   char *result;
   char *temp;
   char *template;
   int i;
   int nsec;
   int ntemp;
   size_t tlen;

/* Initialise */
   result = NULL;
   if( parts ) *npart = 0;

/* Check global status */
   if( !astOK ) return result;

/* If required, split the total "pattern" string into sections, using
   (unescaped) "=" characters as the delimiter. The first section is the
   actual template, and each subsequent section (if any) holds a
   substitution string. */
   if( ! ignore_equals ) {
      sections = astChrSplitC( pattern, '=', &nsec );

/* If equals signs are being treated literally, just take a copy of the
   supplied pattern. */
   } else {
      cptr = astStore( NULL, pattern, strlen( pattern ) + 1 );
      sections = &cptr;
      nsec = 1;
   }

   if( sections ) {

/* If the caller did not provide any substitution strings, use the ones
   appended to the end of the pattern string (if any). */
      if( nsub == 0 ) {
         subs = (void *) ( sections + 1 );
         nsub = nsec - 1;
      }

/* Split the template section into sub-sections, using (unescaped) "|"
   characters as the delimiter. Each sub-section is an alternate pattern
   matching template. */
      temps = astChrSplitC( sections[ 0 ], '|', &ntemp );

   } else {
      temps = 0;
      ntemp = 0;
   }

/* Loop round each template until all templates have been checked or a
   match occurs.. */
   for( i = 0; i < ntemp && !result; i++ ) {
      temp = temps[ i ];
      tlen = strlen( temp );

/* If the template starts with "^" or "(^", remove the "^" character.
   Otherwise insert ".*?" at the start. Allocate three extra characters
   in case we later need to add ".*?" to the end of the string. */
      if( temp[ 0 ] == '^' ) {
         template = astMalloc( tlen + 3 );
         if( template ) {
            strcpy( template, temp + 1 );
            tlen--;
         }

      } else if( temp[ 0 ] == '(' && temp[ 1 ] == '^') {
         template = astMalloc( tlen + 3 );
         if( template ) {
            template[ 0 ] = '(';
            strcpy( template + 1, temp + 2 );
            tlen--;
         }

      } else {
         template = astMalloc( tlen + 7 );
         if( template ) {
            template[ 0 ] = '.';
            template[ 1 ] = '*';
            template[ 2 ] = '?';
            strcpy( template + 3, temp );
            tlen += 3;
         }
      }

/* If the pattern ends with "$" or "$)", remove the "$" character. Otherwise
   insert ".*?" at the end. */
      if( template[ tlen - 1 ] == '$' ) {
         tlen--;

      } else if( template[ tlen - 2 ] == '$' && template[ tlen - 1 ] == ')' ) {
         template[ tlen - 2 ] = ')';
         tlen--;

      } else {
         template[ tlen ] = '.';
         template[ tlen + 1 ] = '*';
         template[ tlen + 2 ] = '?';
         tlen += 3;
      }

/* Ensure the string is terminated */
      template[ tlen ] = 0;

/* See if the test string matches the current template. */
      result = ChrMatcher( test, test + strlen( test ), template, pattern,
                           subs, nsub, 0, 1, parts, npart, matchend, status );

/* Free resources. */
      template = astFree( template );
   }

   if( temps ) {
      for( i = 0; i < ntemp; i++ ) temps[ i ] = astFree( temps[ i ] );
      temps = astFree( temps );
   }

   if( sections ) {
      for( i = 0; i < nsec; i++ ) sections[ i ] = astFree( sections[ i ] );
      if( ! ignore_equals ) sections = astFree( sections );
   }

/* Return a NULL pointer if an error has occurred. */
   if( !astOK ) result = astFree( result );

/* Return the result */
   return result;
}

char *astChrSub_( const char *test, const char *pattern, const char *subs[],
                  int nsub, int *status ){
/*
*++
*  Name:
c     astChrSub
f     AST_CHRSUB

*  Purpose:
*     Performs substitutions on a supplied string.

*  Type:
*     Public function.

*  Synopsis:
c     #include "memory.h"
c     char *astChrSub( const char *test, const char *pattern,
c                      const char *subs[], int nsub )
f     MATCH = AST_CHRSUB( TEST, PATTERN, RESULT, STATUS )

*  Description:
*     This function checks a supplied test string to see if it matches a
*     supplied template. If it does, specified sub-sections of the test
*     string may optionally be replaced by supplied substitution strings.
*     The resulting string is returned.

*  Parameters:
c     test
f     TEST = CHARACTER * ( * ) (Given)
*        The string to be tested.
*     pattern
f     PATTERN = CHARACTER * ( * ) (Given)
*        The template string. See "Template Syntax:" below.
*     subs
*        An array of strings that are to replace the sections of the test
*        string that match each parenthesised sub-string in "pattern". The
*        first element of "subs" replaces the part of the test string that
*        matches the first parenthesised sub-string in the template, etc.
*
*        If "nsub" is zero, then the "subs" pointer is ignored. In this
*        case, substitution strings may be specified by appended them to
*        the end of the "pattern" string, separated by "=" characters.
*        Note, if you need to include a literal "=" character in the
*        pattern, precede it by an escape "\" character.
*     nsub
*        The number of substitution strings supplied in array "subs".
f     RESULT = CHARACTER * ( * ) (Returned)
f        Returned holding the result of the substitutions. If the test
f        string does not match the template, then a blank string is
f        returned.

*  Returned Value:
c     astChrSub()
c        A pointer to a dynamically allocated string holding the result
c        of the substitutions, or NULL if the test string does not match
c        the template string. This string should be freed using astFree
c        when no longer needed. If no substituions are specified then a
c        copy of the test string is returned if it matches the template.
f     AST_CHRSUB = LOGICAL
f        .TRUE. if the test string matched the supplied template, and
f        .FALSE. otherwise.

*  Template Syntax:
*     The template syntax is a minimal form of regular expression, The
*     quantifiers allowed are "*", "?", "+", "{n}", "*?" and "+?" (the
*     last two are non-greedy - they match the minimum length possible
*     that still gives an overall match to the template). The only
*     constraints allowed are "^" and "$". The following atoms are allowed:
*
*     - [chars]: Matches any of the specified characters.
*
*     - [^chars]: Matches anything but the specified characters.
*
*     - .: Matches any single character.
*
*     - x: Matches the character x so long as x has no other significance.
*
*     - \x: Always matches the character x (except for [dDsSwW]).
*
*     - \d: Matches a single digit.
*
*     - \D: Matches anything but a single digit.
*
*     - \w: Matches any alphanumeric character, and "_".
*
*     - \W: Matches anything but alphanumeric characters, and "_".
*
*     - \s: Matches white space.
*
*     - \S: Matches anything but white space.
*
*     Note, minus signs ("-") within brackets have no special significance,
*     so ranges of characters must be specified explicitly.
*
*     Multiple template strings can be concatenated, using the "|"
*     character to separate them. The test string is compared against
*     each one in turn until a match is found.
*
c     Parentheses are used within each template to identify sub-strings
c     that are to be replaced by the strings supplied in "sub".
c
c     If "nsub" is supplied as zero, then substitution strings may be
c     specified by appended them to the end of the "pattern" string,
c     separated by "=" characters. If "nsub" is not zero, then any
c     substitution strings appended to the end of "pattern" are ignored.
f
f     Parentheses are used within each template to identify sub-strings
f     that are to be replaced by new strings. The new strings are
f     specified by appended them to the end of the "pattern" string,
f     separated by "=" characters.
*
c     Each element of "subs"
f     Each new string
*     may contain a reference to a token of the
*     form "$1", "$2", etc. The "$1" token will be replaced by the part
*     of the test string that matched the first parenthesised sub-string
*     in "pattern". The "$2" token will be replaced by the part of the
*     test string that matched the second parenthesised sub-string in
*     "pattern", etc.
*

c  Notes:
c     -  A NULL pointer is returned if this function is invoked with the
c     global error status set or if it should fail for any reason, or if
c     the supplied test string does not match the template.

*--
*/

/* Call ChrSuber to do the work, without saving the matching parts of the
   test string. */
   return ChrSuber( test, pattern, subs, nsub, 0, NULL, NULL, NULL, status );
}

void *astFree_( void *ptr, int *status ) {
/*
*++
*  Name:
*     astFree

*  Purpose:
*     Free previously allocated memory.

*  Type:
*     Public function.

*  Synopsis:
*     #include "memory.h"
*     void *astFree( void *ptr )

*  Description:
*     This function frees memory that has previouly been dynamically
*     allocated using one of the AST memory function.

*  Parameters:
*     ptr
*        Pointer to previously allocated memory. An error will result
*        if the memory has not previously been allocated by another
*        function in this module. However, a NULL pointer value is
*        accepted (without error) as indicating that no memory has yet
*        been allocated, so that no action is required.

*  Returned Value:
*     astFree()
*        Always returns a NULL pointer.

*--
*/

/* Local Variables: */
   astDECLARE_GLOBALS            /* Pointer to thread-specific global data */
   Memory *mem;                  /* Pointer to memory header */
   int isdynamic;                /* Is the memory dynamically allocated? */
   size_t size;                  /* The usable size of the memory block */

/* If needed, get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(NULL);

/* If the incoming pointer is NULL, do nothing. Otherwise, check if it
   points at dynamically allocated memory (IsDynamic sets the global
   error status if it does not). */
   if( ptr ) {
      IS_DYNAMIC( ptr, isdynamic );
   } else {
      isdynamic = 0;
   }
   if ( isdynamic ) {

/* If OK, obtain a pointer to the memory header. */
      mem = (Memory *) ( (char *) ptr - SIZEOF_MEMORY );

#ifdef MEM_DEBUG
      DeIssue( mem, status );
#endif

/* If the memory block is small enough, and the cache is being used, put it
   into the cache rather than freeing it, so that it can be reused. */
      size = mem->size;
      if( use_cache && size <= MXCSIZE ) {
         mem->next = cache[ size ];
         cache[ size ] = mem;

/* Set the size to zero to indicate that the memory block has been freed.
   The size of the block is implied by the Cache element it is stored in. */
         mem->size = (size_t) 0;

/* Simply free other memory blocks, clearing the "magic number" and size
   values it contains. This helps prevent accidental re-use of the memory. */
      } else {
         mem->magic = (unsigned long) 0;
         mem->size = (size_t) 0;

/* Free the allocated memory. */
         FREE( mem );
      }
   }

/* Always return a NULL pointer. */
   return NULL;

}

void *astFreeDouble_( void *ptr, int *status ) {
/*
*++
*  Name:
*     astFreeDouble

*  Purpose:
*     Free previously double allocated memory.

*  Type:
*     Public function.

*  Synopsis:
*     #include "memory.h"
*     void *astFreeDouble( void *ptr )

*  Description:
*     This function frees memory that has previouly been dynamically
*     allocated using one of the AST memory function. It assumes that
*     the supplied pointer is a pointer to an array of pointers. Each
*     of these pointers is first freed, and then the supplied pointer
*     is freed.

*  Parameters:
*     ptr
*        Pointer to previously allocated memory. An error will result
*        if the memory has not previously been allocated by another
*        function in this module. However, a NULL pointer value is
*        accepted (without error) as indicating that no memory has yet
*        been allocated, so that no action is required.

*  Returned Value:
*     astFreeDouble()
*        Always returns a NULL pointer.

*--
*/

/* Local Variables: */
   int iptr;                     /* Index of sub-pointer */
   int nptr;                     /* Number of sub-pointers */
   size_t size;                  /* The usable size of the memory block */
   void **ptrs;                  /* Pointer to array of pointers */

/* Check a pointer was supplied. */
   if( ! ptr ) return NULL;

/* Get the size of the memory area. */
   size = astSizeOf( ptr );

/* Get the number of points this amount of memory could hold. */
   nptr = size/sizeof( void * );

/* Report an error if the size is not an integer multiple of an address
   size. */
   if( nptr*sizeof( void * ) != size ) {
      astError( AST__MEMIN, "Invalid attempt to free double allocated "
                "memory: the supplied memory size (%lu bytes) is not "
                "an integer multiple of %lu.", status, size,
                sizeof( void * ) );

   } else {

/* Free each sub-pointer. */
      ptrs = (void **) ptr;
      for( iptr = 0; iptr < nptr; iptr++ ) {
         ptrs[ iptr ] = astFree(  ptrs[ iptr ] );
      }

/* Free the supplied pointer. */
      ptr = astFree( ptr );
   }

/* Always return a NULL pointer. */
   return NULL;
}

void *astGrow_( void *ptr, int n, size_t size, int *status ) {
/*
*++
*  Name:
*     astGrow

*  Purpose:
*     Allocate memory for an adjustable array.

*  Type:
*     Public function.

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
*     astGrow()
*        If the memory was allocated successfully, a pointer to the start
*        of the possibly new memory region is returned (this may be the
*        same as the original pointer).

*  Notes:
*     - When new memory is allocated, the existing contents are preserved.
*     - This function does not free memory once it is allocated, so
*     the size allocated grows to accommodate the maximum size of the
*     array (or "high water mark"). Other memory handling routines may
*     be used to free the memory (or alter its size) if necessary.
*     - If this function is invoked with the global error status set,
*     or if it fails for any reason, the original pointer value is
*     returned and the memory contents are unchanged.
*--
*/

/* Local Variables: */
   astDECLARE_GLOBALS            /* Pointer to thread-specific global data */
   int isdynamic;                /* Is the memory dynamically allocated? */
   Memory *mem;                  /* Pointer to memory header */
   size_t newsize;               /* New size to allocate */
   void *new;                    /* Result pointer */

/* Check the global error status. */
   if ( !astOK ) return ptr;

/* If needed, get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(NULL);

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
   } else {
      IS_DYNAMIC( ptr, isdynamic );
      if ( isdynamic ) {

/* Obtain a pointer to the memory header and check if the new size
   exceeds that already allocated. */
         mem = (Memory *) ( (char *) ptr - SIZEOF_MEMORY );
         if ( mem->size < size ) {

/* If so, calculate a possible new size by doubling the old
   size. Increase this further if necessary. */
            newsize = mem->size * ( (size_t) 2 );
            if ( size > newsize ) newsize = size;

/* Re-allocate the memory. */
            new = astRealloc( ptr, newsize );
         }
      }
   }

/* Return the result. */
   return new;
}

int astIsDynamic_( const void *ptr, int *status ) {
/*
*++
*  Name:
*     astIsDynamic

*  Purpose:
*     Returns a flag indicating if memory was allocated dynamically.

*  Type:
*     Public function.

*  Synopsis:
*     #include "memory.h"
*     int astIsDynamic_( const void *ptr )

*  Description:
*     This function takes a pointer to a region of memory and tests if
*     the memory has previously been dynamically allocated using other
*     functions from this module. It does this by checking for the
*     presence of a "magic" number in the header which precedes the
*     allocated memory. If the magic number is not present (or the
*     pointer is invalid for any other reason), zero is returned.
*     Otherwise 1 is returned.

*  Parameters:
*     ptr
*        Pointer to test.

*  Returned Value:
*     astIsDynamic()
*        Non-zero if the memory was allocated dynamically. Zero is returned
*        if the supplied pointer is NULL.

*  Notes:
*     - A value of zero is returned if this function is invoked with
*     the global error status set, or if it fails for any reason.
*--
*/

/* Local Variables: */
   astDECLARE_GLOBALS            /* Pointer to thread-specific global data */
   Memory *isdynmem;               /* Pointer to memory header */ \

/* Check the global error status and the supplied pointer. */
   if ( !astOK || ! ptr ) return 0;

/* If needed, get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(NULL);

/* Derive a pointer to the memory header that precedes the
   supplied region of memory. */
   isdynmem = (Memory *) ( (char *) ptr - SIZEOF_MEMORY );

/* Check if the "magic number" in the header is valid, returning non-zero
   if it is. */
   return ( isdynmem->magic == MAGIC( isdynmem, isdynmem->size ) );
}

void *astMalloc_( size_t size, int init, int *status ) {
/*
*++
*  Name:
*     astMalloc

*  Purpose:
*     Allocate memory.

*  Type:
*     Public function.

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
*     astMalloc()
*        If successful, the function returns a pointer to the start of
*        the allocated memory region. If the size allocated is zero, this
*        will be a NULL pointer.

*  Notes:
*     - A pointer value of NULL is returned if this function is
*     invoked with the global error status set or if it fails for any
*     reason.
*--

*  astMallocInit:
*     - This function can be invoked using either the public astMalloc
*     macro documented above, or the private astMallocInit macro.
*     astMallocInit has the same interface as astMalloc, but calls calloc
*     rather than malloc so that the allocated memory is filled with zeros.
*     Ideally, we should use an extra layer in the calling heirarchy to
*     remove the hidden "init" argument in the astMalloc_ interface, but
*     astMalloc is time-critical in many situations and so it is included
*     as a "hidden" argument.

*/

/* Local Constants: */
#define ERRBUF_LEN 80

/* Local Variables: */
   astDECLARE_GLOBALS            /* Pointer to thread-specific global data */
   char errbuf[ ERRBUF_LEN ];    /* Buffer for system error message */
   char *errstat;                /* Pointer to system error message */
   Memory *mem;                  /* Pointer to space allocated by malloc */
   void *result;                 /* Returned pointer */

/* Initialise. */
   result = NULL;

/* Check the global error status. */
   if ( !astOK ) return result;

/* If needed, get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(NULL);

/* Check that the size requested is not negative and report an error
   if it is. */
   if ( size < (size_t) 0 ) {
      astError( AST__MEMIN,
                "Invalid attempt to allocate %lu bytes of memory.", status,
                (unsigned long) size );

/* Otherwise, if the size is greater than zero, either get a previously
   allocated memory block from the cache, or attempt to use malloc
   to allocate the memory, including space for the header structure. */
   } else if ( size != (size_t ) 0 ) {

/* If the cache is being used and a cached memory block of the required size
   is available, remove it from the cache array and use it. */
      mem = ( use_cache && size <= MXCSIZE ) ? cache[ size ] : NULL;
      if( mem ) {
         cache[ size ] = mem->next;
         mem->next = NULL;
         mem->size = (size_t) size;

/* Initialise the memory (but not the header) if required. */
         if( init ) (void) memset( (char *) mem + SIZEOF_MEMORY, 0, size );

/* Otherwise, allocate a new memory block using "malloc" or "calloc". */
      } else {
         if( init ) {
            mem = CALLOC( 1, SIZEOF_MEMORY + size );
         } else {
            mem = MALLOC( SIZEOF_MEMORY + size );
         }

/* Report an error if malloc failed. */
         if ( !mem ) {

#if HAVE_STRERROR_R
            strerror_r( errno, errbuf, ERRBUF_LEN );
            errstat = errbuf;
#else
            errstat = strerror( errno );
#endif
            astError( AST__NOMEM, "malloc: %s", status, errstat );
            astError( AST__NOMEM, "Failed to allocate %lu bytes of memory.", status,
                      (unsigned long) size );

/* If successful, set the "magic number" in the header and also store
   the size. */
         } else {
            mem->magic = MAGIC( mem, size );
            mem->size = size;
            mem->next = NULL;

#ifdef MEM_DEBUG
            mem->id = -1;
            mem->prev = NULL;
#endif

         }
      }

/* Do nothing more if no memory is being returned. */
      if( mem ) {

#ifdef MEM_DEBUG
      Issue( mem, status );
#endif

/* Increment the memory pointer to the start of the region of
   allocated memory to be used by the caller.*/
         result = mem;
         result = (char *) result + SIZEOF_MEMORY;
      }
   }

/* Return the result. */
   return result;
}
#undef ERRBUF_LEN

static char *ChrMatcher( const char *test, const char *end, const char *template,
                         const char *pattern, const char *subs[], int nsub,
                         int ignore, int expdoll, char ***mres, int *mlen,
                         const char **matchend, int *status ){
/*
*  Name:
*     ChrMatcher

*  Purpose:
*     Performs substitutions on a supplied string.

*  Type:
*     Private function.

*  Synopsis:
*     #include "memory.h"
*     char *ChrMatcher( const char *test, const char *end, const char *template,
*                       const char *pattern, const char *subs[], int nsub,
*                       int ignore, int expdoll, char ***mres, int *mlen,
*                       const char **matchend, int *status )

*  Description:
*     This function is performs most of the work for astChrSub.

*  Parameters:
*     test
*        The string to be tested.
*     end
*        Pointer to the terminating null character at the end of "test".
*     template
*        The template string. See astChrSub for details.
*     pattern
*        The user supplied "pattern" string (only used for error messages).
*     subs
*        An array of strings holding the values that are to be substituted
*        into each parenthesised substring in "test".
*     nsub
*        The length of the subs arrays.
*     ignore
*        If non-zero, then no substitutions are performed, and any
*        inbalance in parentheses is ignored.
*     expdoll
*        If non-zero, then any "$1", "$2", etc, tokens in the
*        substitution fields will be repalced by the corresponding fields
*        in the test string.
*     mres
*        Address of a location at which to return a pointer to an array
*        of character string pointers. The strings are the sub-sections
*        of "test" that matched the parenthesised sub-sections of
*        "template". The array will have "*m" elements. Ignored if NULL.
*     mlen
*        Address of a location at which to return the length of the
*        returned "mres" array. Ignored if "mres" is NULL.
*     matchend
*        A pointer to a location at which to return a pointer to the
*        character that follows the last character within the supplied test
*        string that matched any parenthesises sub-section of "regexp". A
*        NULL pointer is returned if no matches were found. A NULL pointer
*        may be supplied if the location of the last matching character is
*        not needed.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     A pointer to a dynamically allocated string holding the result of the
*     substitutions, or NULL if the test string does not match the template
*     string. This string should be freed using astFree when no longer
*     needed.

*  Notes:
*     -  A NULL pointer is returned if this function is invoked with the
*     global error status set or if it should fail for any reason, or if
*     the supplied test string does not match the template.
*/

/* Local Variables: */
   char **matches;
   char **newsubs;
   char **parts;
   char *allowed;
   char *r;
   char *result;
   char *sres;
   char *stest;
   char stemp[10];
   const char *aaa;
   const char *aa;
   const char *a;
   const char *b;
   int allow;
   int dollar;
   int end_sub;
   int greedy;
   int i;
   int in_sub;
   int ipart;
   int match;
   int matchlen;
   int max_na;
   int min_na;
   int na;
   int nb;
   int nmatch;
   int npart;
   int partlen;
   int reslen;
   int start_sub;
   size_t stl;

/* Initialisation. */
   if( mres ) *mlen = 0;
   aaa = NULL;
   if( matchend ) *matchend = NULL;

/* Check the global error status. */
   if( !astOK ) return NULL;

/* more initialisation. */
   result = NULL;
   allowed = NULL;

/* Get memory for a set of pointers to copies of the test sub-strings that
   fall between the sub-strings being replaced. */
   parts = astMalloc( sizeof( char *)*(size_t) ( nsub + 1 ) );

/* Get memory for a set of pointers to copies of the test sub-strings that
   match the parenthesised sub-strings in the template. */
   matches = astMalloc( sizeof( char *)*(size_t) nsub );

/* Initialise pointers to the next test and template characters to read. */
   a = test;
   b = template;

/* Initialise the pointer to the start of the previous test character */
   aa = test;

/* Assume the test string matches the template. */
   match = 1;

/* The template pointer is not currently in a substitution field. */
   in_sub = 0;

/* Initialise the number of substitution fields found so far. */
   npart = 0;
   nmatch = 0;

/* Loop until we have reached the end of either the test or template
   string. We break out of the loop early if we find that the test string
   does not match the template string. */
   while( match && *a && *b ) {

/* Examine the string at the start of the template string. This returns a
   string of allowed (or disallowed) characters that the next test character
   can match, the number of template characters consumed, the minimum number
   of test characters that must match the allowed character set, and a flag
   indicating if the number of matching test characters can exceed the
   minimum number or must be exactly equal to the minimum number.  */
      allowed = CheckTempStart( template, b, pattern, allowed, &nb, &allow,
                                &min_na, &max_na, &start_sub, &end_sub,
                                &greedy, status );
      if( !astOK ) break;

/* Increment the the pointer to the next template character. */
      b += nb;

/* If the leading field in the template indicates the start of a
   substitution field, record the test string up to the current point. */
      if( start_sub ){

/* Do nothing if we are ignoring substitutions. */
         if( ! ignore ){

/* Store a pointer to the first test character that matches the
   substitution template. */
            aaa = a;

/* Report an error and abort if we are already inside a substitution
   field */
            if( in_sub ) {
               astError( AST__BADSUB, "Invalid pattern matching template \"%s\": "
                         "missing ')'.", status, pattern );
               break;
            }

/* Indicate that we are now in a substitution field. */
            in_sub = 1;

/* If possible, store a copy of the test string that started at the end
   of the previous substitution field and ends at the current point.
   First ensure the "parts" array is large enough since the string may
   contain more than "nsub" parenthesised sub-strings. */
            parts = astGrow( parts, npart + 1, sizeof( char * ) );
            if( parts ) {
               partlen = ( a - aa );
               parts[ npart ] = astStore( NULL, aa, partlen + 1 );
               if( parts[ npart ] ) {
                  parts[ npart ][ partlen ] = 0;
                  npart++;
               }
            }
         }

/* If the leading field in the template indicates the end of a
   substitution field, initialise the start of the next part of the test
   string. */
      } else if( end_sub ){

/* Do nothing if we are ignoring substitutions. */
         if( ! ignore ){

/* Report an error and abort if we are not currently in a substitution
   field. */
            if( ! in_sub ) {
               astError( AST__BADSUB, "Invalid pattern matching template \"%s\": "
                         "missing '('.", status, pattern );
               break;
            }

/* We are no longer in a substitution field. */
            in_sub = 0;

/* If possible, store a copy of the test string that matched the
   substitution template. */
            matches = astGrow( matches, nmatch + 1, sizeof( char * ) );
            if( matches ) {
               matchlen = ( a - aaa );
               matches[ nmatch ] = astStore( NULL, aaa, matchlen + 1 );
               if( matches[ nmatch ] ) {
                  matches[ nmatch ][ matchlen ] = 0;
                  nmatch++;
                  if( matchend ) *matchend = a;
               }
            }

/* Record the start of the next test string part. */
            aa = a;
         }

/* Otherwise, find how many characters at the front of the test string
   match the leading field in the template. Find the number of leading
   characters in the test string that are contained in the set of
   characters allowed by the leading field in the template. */
      } else {
         if( !allowed ) {
            na = strlen( a );

         } else if( allow ) {
            na = strspn( a, allowed );

         } else {
            na = strcspn( a, allowed );
         }

/* Check that the minmum number of matching characters is available. */
         if( na < min_na ){
            match = 0;
            break;
         }

/* Dont match more characters than are needed. */
         if( na > max_na ) na = max_na;

/* We cannot match more characters than are available. */
         if( na < max_na ) max_na = na;

/* If we have exhausted the template, match the maximum number of
   characters. */
         if( ! *b ) {
            na = max_na;

/* If we still have a match, we may choose to use fewer than the max
   allowed number of test characters in order to allow the next template
   field to be matched. Don't need to do this if we have reached the end
   of the template. */
         } else if( max_na > min_na ) {
            match = 0;

/* If a greedy quantifier was used, try using a decreasing number of test
   characters, starting at the maximum allowed and decreasing down to the
   minimum, until a number is found which allows the rest of the string
   to be matched. */
            if( greedy ) {
               for( na = max_na; na >= min_na; na-- ) {
                  r = ChrMatcher( a + na, end, b, pattern, NULL, 0, 1, 0,
                                  NULL, NULL, NULL, status );
                  if( r ) {
                     match = 1;
                     r = astFree( r );
                     break;
                  }
               }

/* If a non-greedy quantifier was used, try using an increasing number of
   test characters, starting at the minimum allowed and increasing up to
   the maximum, until a number is found which allows the rest of the string
   to be matched. */
            } else {
               for( na = min_na; na <= max_na; na++ ) {
                  r = ChrMatcher( a + na, end, b, pattern, NULL, 0, 1, 0,
                                  NULL, NULL, NULL, status );
                  if( r ) {
                     match = 1;
                     r = astFree( r );
                     break;
                  }
               }
            }
         }

/* Increment the the pointer to the next test character. */
         a += na;
         if( a > end ) a = end;
      }
   }

/* If the test string is finished but the template string is not, see if
   the next part of the template string will match a null test string.
   But ignore the ends of substitution fields. */
   if( !*a && *b && match ) {
      while( *b && *b != ')' ) {
         allowed = CheckTempStart( template, b, pattern, allowed, &nb, &allow,
                                   &min_na, &max_na, &start_sub, &end_sub,
                                   &greedy, status );
         b += nb;
         allowed = astFree( allowed );

         if( min_na > 0 ) {
            match = 0;
            break;
         }
      }
   }

/* If the next character in the template is a closing parenthesis, then
   we are finishing a substitution field. */
   if( match && *b == ')' ) {

/*Check we are not ignoring substitutions. */
      if( ! ignore ){

/* Report an error and abort if we are not currently in a substitution
   field. */
         if( ! in_sub ) {
            astError( AST__BADSUB, "Invalid pattern matching template \"%s\": "
                      "missing '('.", status, pattern );
         }

/* We are no longer in a substitution field. */
         in_sub = 0;

/* If possible, store a copy of the test string that matched the
   substitution field. */
         matches = astGrow( matches, nmatch + 1, sizeof( char * ) );
         if( matches ) {
            matchlen = ( a - aaa );
            matches[ nmatch ] = astStore( NULL, aaa, matchlen + 1 );
            if( matches[ nmatch ] ) {
               matches[ nmatch ][ matchlen ] = 0;
               nmatch++;
               if( matchend ) *matchend = a;
            }
         }

         aa = a;
      }
      b++;
   }

/* If the test string is finished but the template string is not, see if
   the rest of the template string will match a null test string. */
   if( !*a && *b && match ) {

      while( *b ) {
         allowed = CheckTempStart( template, b, pattern, allowed, &nb, &allow,
                                   &min_na, &max_na, &start_sub, &end_sub,
                                   &greedy, status );
         b += nb;
         allowed = astFree( allowed );

         if( min_na > 0 ) {
            match = 0;
            break;
         }
      }

   }

/* No match if either string was not used completely. */
   if( *a || *b ) match = 0;

/* Report an error if we are still inside a substitution field */
   if( match && in_sub && !ignore ) {
      astError( AST__BADSUB, "Invalid pattern matching template \"%s\": "
                "missing ')'.", status, pattern );
      match = 0;
   }

/* If we have a match, construct the returned string. */
   if( match && parts ) {

/* Store the test string following the final substitution field. */
      parts = astGrow( parts, npart + 1, sizeof( char * ) );
      if( parts ) {
         partlen = ( a - aa );
         parts[ npart ] = astStore( NULL, aa, partlen + 1 );
         if( parts[ npart ] ) {
            parts[ npart ][ partlen ] = 0;
            npart++;
         }
      }

/* If required, expand  $1, $2, etc within the replacement strings. */
      if( expdoll) {
         newsubs = astMalloc( sizeof( char * )*nsub );
         if( newsubs ) {
            for( i = 0; i < nsub; i++ ) {
               stl = strlen( subs[ i ] );
               stest = astStore( NULL, subs[ i ], stl + 1 );
               for( dollar = 1; dollar <= nsub; dollar++ ) {
                  sprintf( stemp, ".*($%d).*", dollar );
                  sres = ChrMatcher( stest, stest + stl, stemp, stemp,
                                     (void *) ( matches + dollar - 1 ),
                                     1, 0, 0, NULL, NULL, NULL, status );
                  if( sres ) {
                     (void) astFree( stest );
                     stest = sres;
                  }
               }
               newsubs[ i ] = stest;
            }
         }

      } else {
         newsubs = (char **) subs;
      }

/* Concatenate the sub-strings to form the final string. */
      reslen = 0;
      for( ipart = 0; ipart < npart - 1; ipart++ ) {
         result = astAppendString( result, &reslen, parts[ ipart ] );
         if( ipart < nsub ) {
            result = astAppendString( result, &reslen, newsubs[ ipart ] );
         } else {
            result = astAppendString( result, &reslen, matches[ ipart ] );
         }
      }
      result = astAppendString( result, &reslen, parts[ ipart ] );

/* Free resources. */
      if( newsubs && newsubs != (char **) subs ) {
         for( i = 0; i < nsub; i++ ) {
            newsubs[ i ] = astFree( newsubs[ i ] );
         }
         newsubs = astFree( newsubs );
      }
   }

   allowed = astFree( allowed );
   for( ipart = 0; ipart < npart; ipart++ ) {
      parts[ ipart ] = astFree( parts[ ipart ] );
   }
   parts = astFree( parts );

/* If required, return the array holding the test sub-strings that
   matched the parenthesised template sub-strings, together with
   the length of the array. Otherwise, free the memory holding these
   strings. */
   if( mres ) {
      *mres = matches;
      *mlen = nmatch;

   } else if( matches ) {
      for( i = 0; i < nmatch; i++ ) {
         matches[ i ] = astFree( matches[ i ] );
      }
      matches = astFree( matches );

   }

/* Return the result. */
   return result;
}

int astMemCaching_( int newval, int *status ){
/*
*++
*  Name:
*     astMemCaching

*  Purpose:
*     Controls whether allocated but unused memory is cached in this module.

*  Type:
*     Public function.

*  Synopsis:
*     #include "memory.h"
*     int astMemCaching( int newval )

*  Description:
*     This function sets a flag indicating if allocated but unused memory
*     should be cached or not. It also returns the original value of the
*     flag.
*
*     If caching is switched on or off as a result of this call, then the
*     current contents of the cache are discarded.
*
*     Note, each thread has a separate cache. Calling this function
*     affects only the currently executing thread.

*  Parameters:
*     newval
*        The new value for the MemoryCaching tuning parameter (see
*        astTune in objectc.c). If AST__TUNULL is supplied, the current
*        value is left unchanged.

*  Returned Value:
*     astMemCaching()
*        The original value of the MemoryCaching tuning parameter.

*--
*/

/* Local Variables: */
   astDECLARE_GLOBALS
   int i;
   int result;
   Memory *mem;

#ifdef MEM_DEBUG
   int id_list_size;
   int *id_list;
#endif

/* Check the global error status. */
   if ( !astOK ) return 0;

/* If needed, get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(NULL);

/* Store the original value of the tuning parameter. */
   result = use_cache;

/* If a new value is to be set. */
   if( newval != AST__TUNULL ) {

/* If the cache has been initialised, empty it. */
      if( cache_init ) {

/* If we are listing the ID of every memory block in the cache, count the
   number of blocks in the cache and then allocate an array to store the ID
   values in. This is done so that we can sort them before displaying them. */
#ifdef MEM_DEBUG
         if( List_Cache ) {
            Memory *next;

            id_list_size = 0;
            for( i = 0; i <= MXCSIZE; i++ ) {
               next = cache[ i ];
               while( next ) {
                  id_list_size++;
                  next = next->next;
               }
            }

            id_list = MALLOC( sizeof(int)*id_list_size );
            if( !id_list ) {
               astError( AST__INTER, "astMemCaching: Cannot allocate %lu "
                         "bytes of memory", status, (unsigned long)(sizeof(int)*id_list_size) );
            }

            id_list_size = 0;

         } else {
            id_list = NULL;
         }
#endif

         for( i = 0; i <= MXCSIZE; i++ ) {
            while( cache[ i ] ) {
               mem = cache[ i ];
               cache[ i ] = mem->next;
               mem->size = (size_t) i;

#ifdef MEM_DEBUG
               if( id_list ) {
                  id_list[ id_list_size++ ] = mem->id;
               }
#endif

               FREE( mem );
            }
         }

/* If we are displaying the IDs of memory blocks still in the cache, sort
   them using a bubblesort algorithm, then display them. */
#ifdef MEM_DEBUG
         if( id_list ) {

            if( id_list_size == 0 ) {
               printf( "Emptying the AST memory cache - (the cache is "
                       "already empty)\n" );

            } else {
               int sorted, j, t;

               sorted = 0;
               for( j = id_list_size - 2; !sorted && j >= 0; j-- ) {
                  sorted = 1;
                  for( i = 0; i <= j; i++ ) {
                     if( id_list[ i ] > id_list[ i + 1 ] ) {
                        sorted = 0;
                        t = id_list[ i ];
                        id_list[ i ] = id_list[ i + 1 ];
                        id_list[ i + 1 ] = t;
                     }
                  }
               }

               printf( "Emptying the AST memory cache - freeing the "
                       "following memory blocks: ");
               for( i = 0; i < id_list_size; i++ ) printf( "%d ", id_list[ i ] );
               printf( "\n" );

            }
         }
#endif

/* Otherwise, initialise the cache array to hold a NULL pointer at every
   element. */
      } else {
         for( i = 0; i <= MXCSIZE; i++ ) cache[ i ] = NULL;
         cache_init = 1;
      }

/* Store the new value. */
      use_cache = newval;

   }

/* Return the original value. */
   return result;
}

void *astRealloc_( void *ptr, size_t size, int *status ) {
/*
*++
*  Name:
*     astRealloc

*  Purpose:
*     Change the size of a dynamically allocated region of memory.

*  Type:
*     Public function.

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
*     astRealloc()
*        If the memory was reallocated successfully, a pointer to the
*        start of the new memory region is returned (this may be the same
*        as the original pointer). If size was given as zero, a NULL
*        pointer is returned.

*  Notes:
*     - If this function is invoked with the error status set, or if
*     it fails for any reason, the original pointer value is returned
*     and the memory contents are unchanged. Note that this behaviour
*     differs from that of the standard C "realloc" function which
*     returns NULL if it fails.
*--
*/

/* Local Constants: */
#define ERRBUF_LEN 80

/* Local Variables: */
   astDECLARE_GLOBALS
   char errbuf[ ERRBUF_LEN ];    /* Buffer for system error message */
   char *errstat;                /* Pointer to system error message */
   int isdynamic;                /* Was memory allocated dynamically? */
   void *result;                 /* Returned pointer */
   Memory *mem;                  /* Pointer to memory header */

/* Check the global error status. */
   if ( !astOK ) return ptr;

/* Initialise. */
   result = ptr;

/* If needed, get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(NULL);

/* If a NULL pointer was supplied, use astMalloc to allocate some new
   memory. */
   if ( !ptr ) {
      result = astMalloc( size );

/* Otherwise, check that the pointer supplied points at memory
   allocated by a function in this module (IsDynamic sets the global
   error status if it does not). */
   } else {
      IS_DYNAMIC( ptr, isdynamic );
      if ( isdynamic ) {

/* Check that a negative size has not been given and report an error
   if necessary. */
         if ( size < (size_t) 0 ) {
            astError( AST__MEMIN,
               "Invalid attempt to reallocate a block of memory to %ld bytes.", status,
                      (long) size );

/* If OK, obtain a pointer to the memory header. */
         } else {
            mem = (Memory *) ( (char *) ptr - SIZEOF_MEMORY );

/* If the new size is zero, free the old memory and set a NULL return
   pointer value. */
            if ( size == (size_t) 0 ) {
               astFree( ptr );
               result = NULL;

/* Otherwise, reallocate the memory. */
            } else {

/* If the cache is being used, for small memory blocks, do the equivalent of
               mem = REALLOC( mem, SIZEOF_MEMORY + size );

   using astMalloc, astFree and memcpy explicitly in order to ensure
   that the memory blocks are cached. */
               if( use_cache && ( mem->size <= MXCSIZE || size <= MXCSIZE ) ) {
                  result = astMalloc( size );
                  if( result ) {
                     if( mem->size < size ) {
                        memcpy( result, ptr, mem->size );
                     } else {
                        memcpy( result, ptr, size );
                     }
                     astFree( ptr );

                  } else {
                     result = ptr;
                  }

/* For other memory blocks simply use realloc. */
               } else {

#ifdef MEM_DEBUG
                  DeIssue( mem, status );
#endif

                  mem = REALLOC( mem, SIZEOF_MEMORY + size );

/* If this failed, report an error and return the original pointer
   value. */
                  if ( !mem ) {
#if HAVE_STRERROR_R
                     strerror_r( errno, errbuf, ERRBUF_LEN );
                     errstat = errbuf;
#else
                     errstat = strerror( errno );
#endif
                     astError( AST__NOMEM, "realloc: %s", status, errstat );
                     astError( AST__NOMEM, "Failed to reallocate a block of "
                               "memory to %ld bytes.", status, (long) size );

/* If successful, set the new "magic" value and size in the memory
   header and obtain a pointer to the start of the region of memory to
   be used by the caller. */
                  } else {
                     mem->magic = MAGIC( mem, size );
                     mem->size = size;
                     mem->next = NULL;
#ifdef MEM_DEBUG
                     mem->id = -1;
                     mem->prev = NULL;
                     Issue( mem, status );
#endif
                     result = mem;
                     result = (char *) result + SIZEOF_MEMORY;
                  }
               }
            }
         }
      }
   }

/* Return the result. */
   return result;
}
#undef ERRBUF_LEN

void astRemoveLeadingBlanks_( char *string, int *status ) {
/*
*++
*  Name:
*     astRemoveLeadingBlanks

*  Purpose:
*     Remove any leading white space from a string.

*  Type:
*     Public function.

*  Synopsis:
*     #include "memory.h"
*     void astRemoveLeadingBlanks( char *string )

*  Description:
*     This function moves characters in the supplied string to the left
*     in order to remove any leading white space.

*  Parameters:
*     string
*        Pointer to the string.

*--
*/

/* Local Variables: */
   char *c, *d;

/* Check a string has been supplied. */
   if( string ){

/* Get a pointer to the first non-white character in the string. */
      c = string;
      while( *c && isspace( *c ) ) c++;

/* Do nothing more if there are no leading spaces. */
      if( c > string ) {

/* Copy all characters (excluding the trailing null) to the left to
   over-write the leading spaces. */
         d = string;
         while( *c ) *(d++) = *(c++);

/* Terminate the returned string. */
         *d = 0;
      }
   }
}

size_t astSizeOf_( const void *ptr, int *status ) {
/*
*++
*  Name:
*     astSizeOf

*  Purpose:
*     Determine the size of a dynamically allocated region of memory.

*  Type:
*     Public function.

*  Synopsis:
*     #include "memory.h"
*     size_t astSizeOf( const void *ptr )

*  Description:
*     This function returns the size of a region of dynamically
*     allocated memory.

*  Parameters:
*     ptr
*        Pointer to dynamically allocated memory (or NULL if the size
*        of the allocated memory was zero).

*  Returned Value:
*     astSizeOf()
*        The allocated size. This will be zero if a NULL pointer was
*        supplied (no error will result).

*  Notes:
*     - A value of zero is returned if this function is invoked with
*     the global error status set, or if it fails for any reason.
*--
*/

/* Local Variables: */
   astDECLARE_GLOBALS            /* Pointer to thread-specific global data */
   int isdynamic;                /* Was the memory allocated dynamically? */
   size_t size;                  /* Memory size */

/* Check the global error status. */
   if ( !astOK ) return (size_t) 0;

/* Initialise. */
   size = (size_t) 0;

/* If needed, get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(NULL);

/* Check if a non-NULL valid pointer has been given. If so, extract
   the memory size from the header which precedes it. */
   if ( ptr ){
      IS_DYNAMIC( ptr, isdynamic );
      if( isdynamic ) size = ( (Memory *) ( (char *) ptr - SIZEOF_MEMORY ) )->size;
   }

/* Return the result. */
   return size;
}

static size_t SizeOfMemory( int *status ){
/*
*  Name:
*     SizeOfMemory

*  Purpose:
*     Returns the size of a Memory structure, padded to an 8 byte
*     boundary.

*  Type:
*     Private function.

*  Synopsis:
*     size_t SizeOfMemory( int *status )

*  Description:
*     This function returns the size of a Memory structure used to
*     store header information about any block of memory allocated by this
*     module. The returned value may be larger than the actual size of
*     the Memory structure in order to ensure that the pointer returned by
*     astMalloc etc points to an 8 byte boundary. Failure to do this can
*     result in some operating systems having problems. E.g Solaris
*     requires this alignment if the returned pointer is going to be used to
*     store doubles.

*  Parameters:
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     The size to use for a Memory structure.

*  Notes:
*     - The returned value is also stored in the module variable
*     sizeof_memory.
*/

/* Local Variables: */
   astDECLARE_GLOBALS            /* Pointer to thread-specific global data */

/* If needed, get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(NULL);

/* Get the basic size of a Memory structure. */
   sizeof_memory = sizeof( Memory );

/* Now increase the returned value to ensure it is a multiple of 8. Mask
   off all but the last 3 bits, xor with 0x7 to get the remainder, add 1
   to make it a multiple of 8 bytes. */
   sizeof_memory += ((sizeof_memory & 0x7) ? ((sizeof_memory & 0x7) ^ 0x7) + 1 : 0);

/* Return the value */
   return sizeof_memory;

}

size_t astTSizeOf_( const void *ptr, int *status ) {
/*
*+
*  Name:
*     astTSizeOf

*  Purpose:
*     Determine the total size of a dynamically allocated region of memory.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "memory.h"
*     size_t astTSizeOf( const void *ptr )

*  Description:
*     This function returns the size of a region of dynamically
*     allocated memory, including the extra memory used to store
*     the header information for the memory block (size and magic number).

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
   astDECLARE_GLOBALS            /* Pointer to thread-specific global data */
   int isdynamic;                /* Was the memory allocated dynamically? */
   size_t size;                  /* Memory size */

/* Check the global error status. */
   if ( !astOK ) return (size_t) 0;

/* If needed, get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(NULL);

/* Initialise. */
   size = (size_t) 0;

/* Check if a non-NULL valid pointer has been given. If so, extract
   the memory size from the header which precedes it. */
   if ( ptr ){
      IS_DYNAMIC( ptr, isdynamic );
      if( isdynamic ) size = SIZEOF_MEMORY +
                             ( (Memory *) ( (char *) ptr - SIZEOF_MEMORY ) )->size;
   }

/* Return the result. */
   return size;
}

void *astStore_( void *ptr, const void *data, size_t size, int *status ) {
/*
*++
*  Name:
*     astStore

*  Purpose:
*     Store data in dynamically allocated memory.

*  Type:
*     Public function.

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
*     astStore()
*        If the data were stored successfully, a pointer to the start of
*        the possibly new memory region is returned (this may be the same
*        as the original pointer). If size was given as zero, a NULL
*        pointer is returned.

*  Notes:
*     - This is a convenience function for use when storing data of
*     arbitrary size in memory which is to be allocated
*     dynamically. It is appropriate when the size of the data will
*     not change frequently because the size of the memory region will
*     be adjusted to fit the data on every invocation.
*     - If this function is invoked with the error status set, or if
*     it fails for any reason, the original pointer value is returned
*     and the memory contents are unchanged.
*--
*/

/* Local Variables: */
   astDECLARE_GLOBALS            /* Pointer to thread-specific global data */
   int valid;                    /* Is the memory pointer usable? */
   void *new;                    /* Pointer to returned memory */

/* Check the global error status. */
   if ( !astOK ) return ptr;

/* If needed, get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(NULL);

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
   } else {
      if ( !ptr ){
         valid = 1;
      } else {
         IS_DYNAMIC( ptr, valid );
      }
      if( valid ) {

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
   }

/* Return the result. */
   return new;
}

char *astString_( const char *chars, int nchars, int *status ) {
/*
*++
*  Name:
*     astString

*  Purpose:
*     Create a C string from an array of characters.

*  Type:
*     Public function.

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
*     astString()
*        If successful, the function returns a pointer to the start of
*        the allocated string. If the number of characters is zero, a
*        zero-length string is still allocated and a pointer to it is
*        returned.

*  Notes:
*     - A pointer value of NULL is returned if this function is
*     invoked with the global error status set or if it fails for any
*     reason.
*--
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
                "with %d characters.", status, nchars);

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

char **astStringArray_( const char *chars, int nel, int len, int *status ) {
/*
*++
*  Name:
*     astStringArray

*  Purpose:
*     Create an array of C strings from an array of characters.

*  Type:
*     Public function.

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
*     astStringArray()
*        A pointer to the start of the index array, which contains "nel"
*        pointers pointing at the start of each null-terminated output
*        string.
*
*        The returned pointer should be passed to astFree to de-allocate
*        the memory used when it is no longer required. This will free
*        both the index array and the memory used by the strings it
*        points at.

*  Notes:
*     - A NULL pointer will also be returned if the value of "nel" is
*     zero, in which case no memory is allocated.
*     - A pointer value of NULL will also be returned if this function
*     is invoked with the global error status set or if it fails for
*     any reason.
*--
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
                "%d strings.", status, nel );

/* If the string length will be used, check that it is valid and
   report an error if it is not. */
   } else if ( ( nel > 0 ) && ( len < 0 ) ) {
      astError( AST__NCHIN,
                "astStringArray: Invalid attempt to allocate an "
                "array of strings with %d characters in each.", status, len );

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

char *astStringCase_( const char *string, int upper, int *status ) {
/*
*++
*  Name:
*     astStringCase

*  Purpose:
*     Convert a string to upper or lower case.

*  Type:
*     Public function.

*  Synopsis:
*     #include "memory.h"
*     char *astStringCase( const char string, int upper )

*  Description:
*     This function converts a supplied string to upper or lower case,
*     storing the result in dynamically allocated memory. The astChrCase
*     function is similar, but stores the result in a supplied buffer.

*  Parameters:
*     string
*        Pointer to the null terminated string to be converted.
*     upper
*        If non-zero, the string is converted to upper case. Otherwise it
*        is converted to lower case.

*  Returned Value:
*     astStringCase()
*        If successful, the function returns a pointer to the start of
*        the allocated string. The returned memory should be freed using
*        astFree when no longer needed.

*  Notes:
*     - A pointer value of NULL is returned if this function is
*     invoked with the global error status set or if it fails for any
*     reason.
*--
*/

/* Local Variables: */
   char *pout;                   /* Pointer to next output character */
   char *result;                 /* Pointer value to return */
   const char *pin;              /* Pointer to next input character */
   int i;                        /* Character index */
   int len;                      /* String length */

/* Initialise. */
   result = NULL;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Get the length of the supplied string, excluding the trailing null. */
   len = strlen( string );

/* Allocate memory to hold the converted string, plus terminating null. */
   result = (char *) astMalloc( (size_t) ( len + 1 ) );

/* If successful, copy the characters into the string, converting each
   one to the requested case. */
   if ( result ) {
      pin = string;
      pout = result;

      if( upper ) {
         for( i = 0; i < len; i++ ) {
            *(pout++) = toupper( (int) *(pin++) );
         }

      } else {

         for( i = 0; i < len; i++ ) {
            *(pout++) = tolower( (int) *(pin++) );
         }
      }

/* Terminate the string. */
      *pout = '\0';
   }

/* Return the result. */
   return result;
}

size_t astChrLen_( const char *string, int *status ) {
/*
*++
*  Name:
*     astChrLen

*  Purpose:
*     Determine the used length of a string.

*  Type:
*     Public function.

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
*     astChrLen()
*        The number of characters in the supplied string, not including the
*        trailing newline, and any trailing white-spaces or non-printable
*        characters.

*--
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
   int *status;             /* Pointer to inherited status value */
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

/* Get a pointer to the integer holding the inherited status value. */
   status = astGetStatusPtr;

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
                               "(AST internal programming error).", status,
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
   If any %n conversions are found which are preceded by a space, then
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

/* If the % sign is followed by a "n", and was preceded by a space, we
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


/* The next functions are used only when memory debugging is
   switched on via the MEM_DEBUG macro. They can be used for locating
   memory leaks, etc. */
#ifdef MEM_DEBUG

void astActiveMemory_( const char *label ) {
/*
*+
*  Name:
*     astActiveMemory

*  Purpose:
*     Display a list of any currently active AST memory pointers.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "memory.h"
*     astActiveMemory( const char *label )

*  Description:
*     This function displays a list of the identifiers for all currently
*     active AST memory chunks. The list is written to standard output
*     using "printf", preceded by the supplied text.

*  Parameters:
*     label
*        A textual label to display before the memody id values (may be
*        NULL).

*  Notes:
*     - This function attempts to execute even if an error has occurred.
*     - Memory blocks which are not usually freed are not reported. Such
*     blocks are typically used by AST to hold internal state information.
*-
*/

   Memory *next;

   if( label ) printf("%s: ", label );
   next = Active_List;
   if( next ) {
      while( next ) {
         if( !next->perm ) {
            printf( "%d(%s:%d) ", next->id, next->file, next->line );
         }
         next = next->next;
      }
   } else {
      printf("There are currently no active AST memory blocks.");
   }
   printf("\n");

}

void astWatchMemory_( int id ) {
/*
*+
*  Name:
*     astWatchMemory

*  Purpose:
*     Indicate uses of the memory block with the specified identifier
*     should be reported.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "memory.h"
*     astWatchMemory( int id )

*  Description:
*     This function forces astMemoryAlarm to be invoked when key
*     operations are performed on a specified memory block. These key
*     operations include; allocation, freeing, copying and cloning of
*     Objects, etc.
*
*     astMemoryAlarm reports a message when called identifying the memory
*     block and the action performed on it. When using a debugger, these
*     events can be trapped and investigated by setting a debugger
*     breakpoint in astMemoryAlarm_.

*  Parameters:
*     id
*        The identifier of the memory block which is to be watched.

*  Notes:
*     - This function attempts to execute even if an error has occurred.
*-
*/
   Watched_ID = id;
}

int astMemoryId_( const void *ptr, int *status ){
/*
*+
*  Name:
*     astMemoryId

*  Purpose:
*     Return the integer identifier for a memory block.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "memory.h"
*     int astMemoryId( const void *ptr )

*  Description:
*     This function returns the integer identifier associated with a
*     memory block allocated by function sin this module.

*  Parameters:
*     ptr
*        The pointer (a genuine C pointer, not an encoded object
*        identifier).

*  Returned Value:
*     The integer identifier. A value of -1 is returned if "ptr" is NULL.

*-
*/
   astDECLARE_GLOBALS
   astGET_GLOBALS(NULL);
   return ptr ? ((Memory *)(ptr-SIZEOF_MEMORY))->id : -1;
}

void *astMemoryPtr_( int id ){
/*
*+
*  Name:
*     astMemoryPtr

*  Purpose:
*     Return a pointer to the memory block with a given identifier.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "memory.h"
*     void *astMemoryPtr( int id )

*  Description:
*     This function returns a pointer to the memory block with a given
*     identifier. NULL is returned if the given identifier is not active.

*  Parameters:
*     id
*        The identifier for an active memory block.

*  Returned Value:
*     The pointer to the memory block. NULL is returned if no active memory
*     with the given ID can be found. Note, this is always a genuine C
*     pointer (even for public Object pointers).

*-
*/
   Memory *next;
   void *ret;

   ret = NULL;
   next = Active_List;
   while( next ) {
      if( next->id == id ) {
         ret = next + 1;
         break;
      }
   }

   return ret;
}

void astMemoryAlarm_( const char *verb ){
/*
*+
*  Name:
*     astMemoryAlarm

*  Purpose:
*     Called when a watched memory ID is used.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "memory.h"
*     void astMemoryAlarm( const char *verb )

*  Description:
*     This function is called when a watched memory ID is used. See
*     astWatchMemory.

*  Parameters:
*     verb
*        Text to include in message.
*-
*/

   printf( "astMemoryAlarm: Memory id %d has been %s.\n", Watched_ID, verb );
}

void astMemoryStats_( int reset, size_t *peak, size_t *current, int *status ) {
/*
*+
*  Name:
*     astMemoryStats

*  Purpose:
*     Return the current and peak AST memory usage.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "memory.h"
*     astMemoryStats( int reset, size_t *peak, size_t *current )

*  Description:
*     This function returns the current amount of memory allocated
*     using AST memory management functions, and the peak amount of
*     simultaneously allocated memory since the last time the peak value
*     was reset.

*  Parameters:
*     reset
*        If non-zero, the peak value is reset to the current usage
*        upon return.
*     peak
*        Address at which to return the peak memory usage since the last
*        reset, in bytes.
*     current
*        Address at which to return the current memory usage, in bytes.

*-
*/

   LOCK_DEBUG_MUTEX;

   if( peak ) *peak = Peak_Usage;
   if( current ) *current = Current_Usage;
   if( reset ) Peak_Usage = Current_Usage;

   UNLOCK_DEBUG_MUTEX;
}

void astMemoryWarning_( size_t threshold, int *status ) {
/*
*+
*  Name:
*     astMemoryWarning

*  Purpose:
*     Issues a warning memory goes over a specified threshold.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "memory.h"
*     astMemoryWarning( size_t threshold )

*  Description:
*     This function prints a warning message to standard output if the
*     AST memory usage exceeds the specified threshold.

*  Parameters:
*     threshold
*        The memory allocation, in bytes, at which a warning should be issued,
*        Supply zero to suppress warnings.

*  Notes:
*     - This function is used to reset the threshold to zero when the first
*     warning is issued in order to prevent a flood of warnings appearing.
*     Therefore, setting a debugger breakpoint in this function
*     ("astMemoryWarning_" - do not forget the trailing underscore)
*     allows you to locate the point at which memory allocation first
*     exceeds the threshold.

*-
*/

   LOCK_DEBUG_MUTEX;

   Warn_Usage = threshold;

   UNLOCK_DEBUG_MUTEX;
}

void astMemoryUse_( const void *ptr, const char *verb, int *status ){
/*
*+
*  Name:
*     astMemoryUse

*  Purpose:
*     Called to report the use of a memory block pointer.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "memory.h"
*     void astMemoryUse( void *ptr, const char *verb )

*  Description:
*     If the supplied memory block is being watched, astMemoryAlarm is
*     called to report the use of the pointer. The reported text includes
*     the supplied "verb". A memory block can be watched by calling
*     astWatchMemory.

*  Parameters:
*     ptr
*        A pointer to the memory block being used. The pointer must have
*        been returned by one of the AST memory management functions (e.g.
*        astMalloc, astRealloc, etc).
*     verb
*        A verb indicating what is being done to the pointer.
*-
*/

   astDECLARE_GLOBALS
   astGET_GLOBALS(NULL);

   if( ptr && astMemoryId( ptr ) == Watched_ID ) {
      if( !Quiet_Use || !strcmp( verb, ISSUED ) ||
                        !strcmp( verb, FREED ) ) {
         astMemoryAlarm( verb );
      }
   }
}

int astMemoryTune_( const char *name, int value, int *status ){
/*
*+
*  Name:
*     astMemoryTune

*  Purpose:
*     Set a tuning parameter for the memory debugging functions.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "memory.h"
*     int astMemoryTune( const char *name, int value )

*  Description:
*     There are a few tuning parameters which control the behaviour of
*     the memory debugging functions. This function allows these tuning
*     parameters to be queried or set.

*  Parameters:
*     name
*        The name of the tuning parameter to query or set. Valid names are:
*
*        "Keep_ID": A boolean flag indicating if a new ID should be issued
*        for a cached memory block each time it is returned by astMalloc?
*        Otherwise, the same ID value is used throughtout the life of a
*        memory block. Default is zero (false).
*
*        "List_Cache": A boolean flag which if non-zero (true) causes the
*        ID of every memory block in the cache to be reported when the
*        cache is emptied by astFlushMemory.
*
*        "Quiet_Use": A boolean flag controlling the number of reports issued
*        when a memory block is being watched (see astWatchMemory). If
*        non-zero (true), then the only events which are reported are the
*        issuing of a memory block pointer by astMalloc or astRealloc,and
*        the freeing (or caching) of a memory block by astFree. If Quiet_Use
*        is zero (the default), then additional reports are made for
*        memory blocks used to hold AST Objects whenever the Object is
*        copied, cloned, or checked.
*     value
*        The new value for the tuning parameter. If AST__TUNULL is
*        supplied, the original value is left unchanged.

*  Returned Value:
*     The original value of the tuning parameter.

*-
*/

   int result = AST__TUNULL;

   if( name ) {

      if( astChrMatch( name, "Keep_ID" ) ) {
         result = Keep_ID;
         if( value != AST__TUNULL ) Keep_ID = value;

      } else if( astChrMatch( name, "Quiet_Use" ) ) {
         result = Quiet_Use;
         if( value != AST__TUNULL ) Quiet_Use = value;

      } else if( astChrMatch( name, "List_Cache" ) ) {
         result = List_Cache;
         if( value != AST__TUNULL ) List_Cache = value;

      } else if( astOK ) {
         astError( AST__TUNAM, "astMemoryTune: Unknown AST memory tuning "
                   "parameter specified \"%s\".", status, name );
      }
   }

   return result;
}

void astBeginPM_( int *status ) {
/*
*+
*  Name:
*     astBeginPM

*  Purpose:
*     Start a block of permanent memory allocations.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "memory.h"
*     astBeginPM

*  Description:
*     This function indicates that all memory allocations made by calls
*     to other functions in this module (e.g. astMalloc), up to the
*     astEndPM call which matches the astBeginPM call,  will not usually
*     be freed explicitly. Matching astBeginPM/astEndPM calls should be
*     used to enclose all code which allocates memory which is never
*     freed explitly by AST. Such memory allocations may be freed if
*     required, using the astFlushMemory function (but note this should
*     only be done once all use of AST by an application has finished).
*
*     Matching pairs of astBeginPM/astEndPM calls can be nested up to a
*     maximum depth of 20.

*-
*/

   LOCK_DEBUG_MUTEX;

/* The global Perm_Mem flag indicates whether or not subsequent memory
   management functions in this module should store pointers to allocated
   blocks in the PM_List array. Push the current value of this flag
   onto a stack, and set the value to 1. */
   if( PM_Stack_Size >= PM_STACK_MAXSIZE ){
      if( astOK ) {
         astError( AST__INTER, "astBeginPM: Maximum stack size has been "
                   "exceeded (internal AST programming error)." , status);
      }

   } else {
      PM_Stack[ PM_Stack_Size++ ] = Perm_Mem;
      Perm_Mem = 1;
   }
   UNLOCK_DEBUG_MUTEX;
}

void astEndPM_( int *status ) {
/*
*+
*  Name:
*     astEndPM

*  Purpose:
*     End a block of permanent memory allocations.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "memory.h"
*     astEndPM

*  Description:
*     This function indicates the end of the block of permanent memory
*     allocations started by the matching call to astBeginPM. See
*     astBeginPM for further details.

*-
*/

   LOCK_DEBUG_MUTEX;

/* The global Perm_Mem flag indicates whether or not subsequent memory
   management functions in this module should store pointers to allocated
   blocks in the PM_List array. Pop the value from the top of this stack. */
   if( PM_Stack_Size == 0 ){
      if( astOK ) {
         astError( AST__INTER, "astEndPM: astEndPM called without "
                   "matching astBeginPM (internal AST programming error)." , status);
      }

   } else {
      Perm_Mem = PM_Stack[ --PM_Stack_Size ];
   }

   UNLOCK_DEBUG_MUTEX;
}

void astFlushMemory_( int leak, int *status ) {
/*
*+
*  Name:
*     astFlushMemory

*  Purpose:
*     Free all permanent and cached memory blocks.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "memory.h"
*     astFlushMemory( int leak );

*  Description:
*     This function should only be called once all use of AST by an
*     application has finished. It frees any allocated but currently
*     unused memory stored in an internal cache of unused memory
*     pointers. (Note, it does not free any memory used permanently to
*     store internal AST state information).
*
*     It is not normally necessary to call this function since the memory
*     will be freed anyway by the operating system when the application
*     terminates. However, it can be called if required in order to
*     stop memory management tools such as valgrind from reporting that
*     the memory has not been freed at the end of an application.
*
*     In addition, if "leak" is non-zero this function will also report
*     an error if any active AST memory pointers remain which have not
*     been freed (other than pointers for the cached and permanent
*     memory described above). Leakage of active memory blocks can be
*     investigated using astActiveMemory and astWatchMemory.

*  Parameters:
*     leak
*        Should an error be reported if any non-permanent memory blocks
*        are found to be active?

*-
*/

/* Local Variables: */
   Memory *next;
   int nact;
   int istat;

/* Empty the cache. */
   astMemCaching( astMemCaching( AST__TUNULL ) );

/* Free and count all non-permanent memory blocks. */
   nact = 0;
   next = Active_List;
   while( Active_List ) {
      next = Active_List->next;
      if( !Active_List->perm ) {
         nact++;
         FREE( Active_List );
      }
      Active_List = next;
   }

/* Report an error if any active pointers remained. if an error has
   already occurred, use the existing status value. */
   if( nact && leak ){

      if( astOK ) {
         istat = AST__INTER;
      } else {
         istat = astStatus;
      }
      astError( istat, "astFlushMemory: %d AST memory blocks have not "
                "been released (programming error).", status, nact );

   } else {
      printf("astFlushMemory: All AST memory blocks were released correctly.\n" );
   }
}

void astCheckMemory_( int *status ) {
/*
*+
*  Name:
*     astCheckMemory

*  Purpose:
*     Check that all AST memory blocks have been released.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "memory.h"
*     astCheckMemory

*  Description:
*     This macro reports an error if any active AST memory pointers
*     remain which have not been freed (other than pointers for cached
*     and "permanently allocated" memory). Leakage of active memory blocks
*     can be investigated using astActiveMemory and astWatchMemory.
*-
*/

/* Local Variables: */
   Memory *next;
   int nact;
   int istat;

/* Empty the cache. */
   astMemCaching( astMemCaching( AST__TUNULL ) );

/* Count all non-permanent memory blocks. */
   nact = 0;
   next = Active_List;
   while( Active_List ) {
      next = Active_List->next;
      if( !Active_List->perm ) nact++;
      Active_List = next;
   }

/* Report an error if any active pointers remained. If an error has
   already occurred, use the existing status value. */
   if( nact ){

      if( astOK ) {
         istat = AST__INTER;
      } else {
         istat = astStatus;
      }
      astError( istat, "astCheckMemory: %d AST memory blocks have not "
                "been released (programming error).", status, nact );

   } else {
      printf("astCheckMemory: All AST memory blocks were released correctly.\n" );
   }
}

static void Issue( Memory *mem, int *status ) {
/*
*  Name:
*     Issue

*  Purpose:
*     Indicate that a pointer to a memory block has been issued.

*  Type:
*     Private function.

*  Synopsis:
*     #include "memory.h"
*     void Issue( Memory *mem, int *status );

*  Description:
*     Initialises the extra debug items in the Memory header, and adds the
*     Memory structure to the list of active memory blocks.

*  Parameters:
*     mem
*        Pointer to the Memory structure.
*     status
*        Pointer to the inherited status value.
*/

/* Local Variables: */
   astDECLARE_GLOBALS

/* Return if no pointer was supplied. */
   if( !mem ) return;

   LOCK_DEBUG_MUTEX;
   astGET_GLOBALS(NULL);

/* Store a unique identifier for this pointer. Unless global Keep_ID is
   non-zero, a new identifier is used each time the pointer becomes active
   (i.e. each time it is remove from the cache or malloced). */
   if( !Keep_ID || mem->id < 0 ) mem->id = ++Next_ID;

/* Record the file name and line number where it was issued. */
   if( AST__GLOBALS && AST__GLOBALS->Error.Current_File ) {
      strncpy( mem->file, AST__GLOBALS->Error.Current_File, sizeof(mem->file) );
      mem->file[ sizeof(mem->file) - 1 ] = 0;
      mem->line = AST__GLOBALS->Error.Current_Line;
   } else {
      mem->file[ 0 ] = 0;
      mem->line = 0;
   }

/* Indicate if this is a permanent memory block (i.e. it will usually not
   be freed by AST). */
   mem->perm = Perm_Mem;

/* Add it to the double linked list of active pointers. */
   mem->next = Active_List;
   mem->prev = NULL;
   if( Active_List ) Active_List->prev = mem;
   Active_List = mem;

/* Report that the pointer is being issued. */
   astMemoryUse( (void *) mem + SIZEOF_MEMORY, ISSUED );

/* Update the current and peak memory usage. */
   Current_Usage += mem->size + SIZEOF_MEMORY;
   if( Current_Usage > Peak_Usage ) Peak_Usage = Current_Usage;

/* If the current allocation is above the threshold set using
   astMemoryWarning, issue a warning message, and then reset the threshold
   to zero to prevent further warnings being issued, and to allow a
   debugger breakpoint to be set. */
   if( Current_Usage > Warn_Usage &&
       Warn_Usage > 0 ) {
      printf( "Warning - AST memory allocation has exceeded %ld bytes\n",
              Warn_Usage );
      astMemoryWarning( 0 );
   }

   UNLOCK_DEBUG_MUTEX;
}

static void DeIssue( Memory *mem, int *status ) {
/*
*  Name:
*     DeIssue

*  Purpose:
*     Indicate that a pointer to a memory block has been freed.

*  Type:
*     Private function.

*  Synopsis:
*     #include "memory.h"
*     void DeIssue( Memeory *mem, int *status );

*  Description:
*     Initialises the extra debug items in the Memory header, and adds the
*     Memory structure to the list of active memory blocks.

*  Parameters:
*     mem
*        Pointer to the Memory structure.
*     status
*        Pointer to the inherited status value.
*/

/* Local Variables: */
   astDECLARE_GLOBALS
   Memory *next;
   Memory *prev;

/* Return if no pointer was supplied. */
   if( !mem ) return;

   LOCK_DEBUG_MUTEX;
   astGET_GLOBALS(NULL);

/* Report that the pointer is being freed. */
   astMemoryUse( (void *) mem + SIZEOF_MEMORY, FREED );

/* Remove the block from the double linked list of active pointers. */
   next = mem->next;
   prev = mem->prev;
   if( prev ) prev->next = next;
   if( next ) next->prev = prev;
   if( mem == Active_List ) Active_List = next;
   mem->next = NULL;
   mem->prev = NULL;

/* Update the current memory usage. */
   Current_Usage -= mem->size + SIZEOF_MEMORY;

   UNLOCK_DEBUG_MUTEX;
}


#endif






/* The next functions are used only when profiling AST application. */
#ifdef MEM_PROFILE


void astStartTimer_( const char *file, int line, const char *name, int *status ) {
/*
*+
*  Name:
*     astStartTimer

*  Purpose:
*     Measure the time spent until the corresponding call to astStopTimer.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "memory.h"
*     void astStartTimer( const char *name );

*  Description:
*     This function looks for a timer with the specified name within the
*     current parent timer. If no timer with the given name is found, a
*     new timer is created and initialised to zero. The current absolute
*     time (elapsed, user and system) is recorded in the timer. The new
*     timer then becomes the current timer.

*  Parameters:
*     name
*        A label for the timer. This should be unique within the
*        enclosing parent timer.

*  Notes:
*     - This function should only be used in a single-threaded environment.
*     - This function returns without action if timers are currently
*     disabled (see astEnableTimers).

*-
*/

/* Local Variables: */
   int n, found, i;
   AstTimer *t;
   struct tms buf;

/* Check inherited status. Also return if timers are currently disabled. */
   if( !Enable_Timers || *status != 0 ) return;

/* See if a timer with the given name exists in the list of child timers
   within the current timer. */
   found = 0;
   if( Current_Timer ) {
      for( i = 0; i < Current_Timer->nchild; i++ ) {
         t = Current_Timer->children[ i ];
         if( !strcmp( t->name, name ) ) {
            found = 1;
            break;
         }
      }
   }

/* If not, create and initialise one now, and add it into the list of
   children within the current timer. */
   if( !found ) {
      t = astMalloc( sizeof( AstTimer ) );
      t->id = Timer_Count++;
      t->et = 0;
      t->ut = 0;
      t->st = 0;
      t->nentry = 0;
      t->name = name;
      t->file = file;
      t->line = line;
      t->parent = Current_Timer;
      t->nchild = 0;
      t->children = NULL;

      if( Current_Timer ) {
         n = (Current_Timer->nchild)++;
         Current_Timer->children = astGrow( Current_Timer->children,
                                            sizeof( AstTimer *),
                                            Current_Timer->nchild );
         Current_Timer->children[ n ] = t;
      }
   }

/* Record the current absolute times (elapsed, user and system) within
   the new timer. */
   t->e0 = times(&buf);
   t->u0 = buf.tms_utime;
   t->s0 = buf.tms_stime;

/* Increment the number of entries into the timer. */
   (t->nentry)++;

/* Use the new timer as the current timer until the corresponding call to
   astStopTimer. */
   Current_Timer = t;
}

void astEnableTimers_( int enable, int *status ) {
/*
*+
*  Name:
*     astEnableTimers

*  Purpose:
*     Set a global flag indicating if the use of AST timers is enabled.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "memory.h"
*     void astStartTimer( int enable );

*  Description:
*     This function sets a global flag that enables otr disables the user
*     of AST Timers. If timers are disabled, the astStartTimer and
*     astStopTimer functions will return without action.

*  Parameters:
*     enable
*        If non-zero, timers will be used.

*  Notes:
*     - This function should only be used in a single-threaded environment.

*-
*/
   Enable_Timers = enable;
}

void astStopTimer_( int *status ) {
/*
*+
*  Name:
*     astStopTimer

*  Purpose:
*     Record the time spent since the corresponding call to astStartTimer.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "memory.h"
*     void astStopTimer;

*  Description:
*     This function obtains the time increments since the corresponding
*     call to astStartTimer, and adds these increments onto the total
*     times stored in the current timer. It then changes the current
*     timer to be the parent timer associated the current timer on entry.
*
*     If the current timer on entry has no parent (i.e. is a top level
*     timer), the times spent in the top-level timer, and all its
*     descendent timers, are displayed.

*  Notes:
*     - This function should only be used in a single-threaded environment.
*     - This function returns without action if timers are currently
*     disabled (see astEnableTimers).

*-
*/

/* Local Variables: */
   AstTimer *flat;
   AstTimer *t;
   int i;
   int nflat;
   struct tms buf;

/* Check inherited status. Also return if timers are currently disabled. */
   if( !Enable_Timers || !Current_Timer || *status != 0 ) return;

/* Get the current absolute times, and thus find the elapsed times since the
   corresponding call to astStartTimer. Use these elapsed times to increment
   the total times spent in the timer. */
   Current_Timer->et += ( times(&buf) - Current_Timer->e0 );
   Current_Timer->st += ( buf.tms_stime - Current_Timer->s0 );
   Current_Timer->ut += ( buf.tms_utime - Current_Timer->u0 );

/* If this is a top level timer, display the times spent in the current
   timer, and in all its descendent timers. This also frees the memory
   used by the timers. */
   if( !Current_Timer->parent ) {
      flat = NULL;
      nflat = 0;
      Current_Timer = ReportTimer( Current_Timer, 0, &flat, &nflat, status );

/* Sort and display the flat list of timers, then free the memory used by
   the flat list. */
      qsort( flat, nflat, sizeof( AstTimer), CompareTimers2 );
      printf("\n\n");
      t = flat;
      for( i = 0; i < nflat; i++,t++ ) {
         printf( "%s (%s:%d): ", t->name, t->file, t->line );
         printf( "elapsed=%ld ", (long int) t->et );
/*
         printf( "system=%ld ", (long int) t->st );
         printf( "user=%ld ", (long int) t->ut );
*/
         printf( "calls=%d ", t->nentry );
         printf("\n");
      }
      flat = astFree( flat );

/* If this is not a top level timer, restore the parent timer as the
   curent timer. */
   } else {
      Current_Timer = Current_Timer->parent;
   }
}

static AstTimer *ReportTimer( AstTimer *t, int ind, AstTimer **flat,
                              int *nflat, int *status ) {
/*
*  Name:
*     ReportTimer

*  Purpose:
*     Free and report the times spent in a given timer, and all descendents.

*  Type:
*     Private function.

*  Synopsis:
*     #include "memory.h"
*     AstTimer *ReportTimer( AstTimer *t, int ind, AstTimer **flat,
*                            int *nflat, int *status )

*  Description:
*     This routines reports to standard output the times spent in the
*     supplied timer. It then calls itself recursively to report the times
*     spent in each of the child timers of the supplied timer.
*
*     It also frees the memory used to hold the supplied timer.

*  Parameters:
*     t
*        Pointer to the AstTimer structure.
*     ind
*        The number of spaces of indentation to display before the timer
*        details.
*     flat
*        Address of a pointer to the start of an array of AstTimers. The
*        number of elements in this array is given by "*nflat". Each
*        Timer in this array holds the accumulated total for all entries
*        into a given timer, from all parent contexts.
*     nflat
*        Address of an int holding the current length of the "*flat" array.
*     status
*        Pointer to the inherited status value.
*/

/* Local Variables: */
   int found;
   int i;
   AstTimer *ft;
   AstTimer *parent;

/* Check inherited status */
   if( *status != 0 ) return NULL;

/* Display a single line of text containing the times stored in the supplied
   timer, preceded by the requested number of spaces. */
   for( i = 0; i < ind; i++ ) printf(" ");
   printf( "%s (%s:%d): ", t->name, t->file, t->line );

   printf( "id=%d ", t->id );
   printf( "elapsed=%ld ", (long int) t->et );
/*
   printf( "system=%ld ", (long int) t->st );
   printf( "user=%ld ", (long int) t->ut );
*/
   printf( "calls=%d ", t->nentry );

/* If there are any children, end the line with an opening bvrace. */
   if( t->nchild ) printf("{");
   printf("\n");

/* If there is more than one child, sort them into descending order of
   elapsed time usage. */
   if( t->nchild > 1 ) qsort( t->children, t->nchild, sizeof( AstTimer * ),
                              CompareTimers );

/* Increment the indentation and call this function recursively to
   display and free each child timer. */
   ind += 3;
   for( i = 0; i < t->nchild; i++ ) {
      (t->children)[ i ] = ReportTimer( (t->children)[ i ], ind, flat,
                                        nflat, status );
   }

/* Delimit the children by displaying a closing brace. */
   if( t->nchild ) {
      for( i = 0; i < ind - 3; i++ ) printf(" ");
      printf("}\n");
   }

/* See if this timer is contained within itself at a higher level. */
   parent = t->parent;
   while( parent && ( parent->line != t->line ||
                      strcmp( parent->file, t->file ) ) ) {
      parent = parent->parent;
   }

/* If not, search for a timer in the "flat" array of timers that has the same
   source file and line number. */
   if( !parent ) {
      found = 0;
      ft = *flat;
      for( i = 0; i < *nflat; i++, ft++ ) {
         if( ft->line == t->line &&
             !strcmp( ft->file, t->file ) ) {
            found = 1;
            break;
         }
      }

/* If not found, add a new timer to the end of the "flat" array and
   initialise it. */
      if( !found ) {
         i = (*nflat)++;
         *flat = astGrow( *flat, *nflat, sizeof( AstTimer ) );
         ft = (*flat) + i;
         ft->id = 0;
         ft->et = t->et;
         ft->ut = t->ut;
         ft->st = t->st;
         ft->nentry = t->nentry;
         ft->name = t->name;
         ft->file = t->file;
         ft->line = t->line;
         ft->parent = NULL;
         ft->nchild = 0;
         ft->children = NULL;


/* If found, increment the properites to include the supplied timer. */
      } else {
         ft->et += t->et;
         ft->ut += t->ut;
         ft->st += t->st;
         ft->nentry += t->nentry;
      }
   }

/* Free the memory used by the supplied timer. */
   t->children = astFree( t->children );
   return astFree( t );
}


static int CompareTimers( const void *a, const void *b ){
   return ((*((AstTimer **) b ))->et) - ((*((AstTimer **) a ))->et);
}

static int CompareTimers2( const void *a, const void *b ){
   return (((AstTimer *) b )->et) - (((AstTimer *) a )->et);
}

#endif
