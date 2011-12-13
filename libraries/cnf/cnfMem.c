/*
*  Name:
*     cnfMem.c

*  Purpose:
*     Manage the passing of pointers between C and Fortran.

*  Description:
*     This module implements functions which manage the passing of
*     dynamically allocated memory pointers between C and Fortran.
*
*     It operates by maintaining a list of "registered" C pointers
*     which may be converted into Fortran pointers (stored as
*     INTEGERs) by applying a bit-mask if necessary (i.e. if the
*     Fortran INTEGER type is shorter than a C pointer). The reverse
*     conversion (Fortran to C) is performed by ensuring that all
*     registered pointers are unique in the lower bits which are
*     passed to Fortran. Conversion may then be performed by matching
*     the Fortran pointer to the lower bits of the registered C
*     pointer.
*
*     This requirement for uniqueness in the lower bits means that not
*     all C pointers can be registered. Therefore, memory allocation
*     functions are also implemented here which ensure that all
*     allocated memory intended for use from both C and Fortran is
*     referenced only by pointers which have the required property.

*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils.
*     Copyright (C) 2008 Science and Technology Facilties Council.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     <{enter_new_authors_here}>

*  History:
*      5-FEB-1999 (RFWS):
*        Original version.
*      1-MAR-1999 (AJC):
*        Fix for warning on too large a shift
*     <{enter_changes_here}>
*/

#if HAVE_CONFIG_H
# include <config.h>
#endif

/* Header files. */
/* ============= */
/* C run-time library header files. */
#include <limits.h>              /* Implementation-defined limits */
#include <stdio.h>               /* for debugging */
#include <stdlib.h>
#include <string.h>

/* User header files. */
#include "star/mem.h"            /* Starlink malloc wrappers */
#include "f77.h"                 /* F77 <--> C interface macros */

/* Static variables for this module. */
/* ================================= */
/* Mask for extracting the lower bits from C pointers so that they fit
   into the Fortran POINTER type.
   The shift is split into two to avoid a compiler warning if the shift is
   = the number of bits in unsigned long. */
static const unsigned long int mask =
   ~( ~0UL << ( sizeof( F77_POINTER_TYPE ) * CHAR_BIT - 1) << 1 );

/* Number of entries for which space is allocated in pointer list. */
static unsigned int pointer_max = 0;

/* Number of entries so far used in pointer list. */
static unsigned int pointer_count = 0;

/* Number of registered pointers. */
static unsigned int registered_pointers = 0;

/* Pointer to list of C pointers. */
static void **pointer_list = NULL;

/* Pointer to list of associated offsets. */
static size_t *offset_list = NULL;

/* Function prototypes. */
/* ==================== */

static int Register( void * );
static size_t Unregister( void * );
static void *Malloc( size_t, int );
void *F77_EXTERNAL_NAME(cnf_pval)( POINTER(FPTR) );
TRAIL_TYPE F77_EXTERNAL_NAME(cnf_cval)( INTEGER(FINT) );
F77_POINTER_FUNCTION(cnf_preg)( void **(cptr), LOGICAL(isnew) );
F77_SUBROUTINE(cnf_unregp)( POINTER(FPTR) );

/* Define how to convert a C pointer to a fortran INTEGER. This does
   not have to be reversible */
/* If pointers are not the same size as 32bit fortran integers
   we attempt to mask */
#if SIZEOF_VOIDP != SIZEOF_UINT32_T
#define CTOFORTRAN(p)  ( mask & (unsigned long int) p )
#else
/* If pointer and integer are the same size just do an assignment
   without attempting to mask. Should be more efficient. */
#define CTOFORTRAN(p)  ( (F77_POINTER_TYPE)p )
#endif


/* Function definitions. */
/* ===================== */
void *cnfCalloc( size_t nobj, size_t size ) {
/*
*+
*  Name:
*     cnfCalloc

*  Purpose:
*     Allocate space that may be accessed from C and Fortran.

*  Invocation:
*     cpointer = cnfCalloc( nobj, size );

*  Description:
*     This function allocates space in the same way as the standard C
*     calloc() function, except that the pointer to the space
*     allocated is automatically registered (using cnfRegp) for use
*     from both C and Fortran. This means that the returned pointer
*     may subsequently be converted into a Fortran pointer of type
*     F77_POINTER_TYPE (using cnfFptr) and back into a C pointer
*     (using cnfCptr). The contents of the space may therefore be
*     accessed from both languages.

*  Arguments:
*     size_t nobj (Given)
*        The number of objects for which space is required.
*     size_t size (Given)
*        The size of each object.

*  Returned Value:
*     void *cnfCalloc
*        A registered pointer to the allocated space, or NULL if the
*        space could not be allocated.

*  Notes:
*     - As with calloc(), the allocated space is initialised to zero
*     bytes.
*     - The space should be freed using cnfFree when no longer
*     required.
*-
*/

/* Allocate the required memory with initialisation to zero. */
   return Malloc( nobj * size, 1 );
}

void *cnfCptr( F77_POINTER_TYPE fpointer ) {
/*
*+
*  Name:
*     cnfCptr

*  Purpose:
*     Convert a Fortran pointer to a C pointer.

*  Invocation:
*     cpointer = cnfCptr( fpointer )

*  Description:
*     Given a Fortran pointer, stored in a variable of type
*     F77_POINTER_TYPE, this function returns the equivalent C
*     pointer. Note that this conversion is only performed if the C
*     pointer has originally been registered (using cnfRegp) for use
*     from both C and Fortran. All pointers to space allocated by
*     cnfCalloc and cnfMalloc are automatically registered in this
*     way.

*  Arguments:
*     F77_POINTER_TYPE fpointer (Given)
*        The Fortran pointer value.

*  Returned Value:
*     void *cnfCptr
*        The equivalent C pointer.

*  Notes:
*     - A NULL value will be returned if the C pointer has not
*     previously been registered for use from both C and Fortran, or
*     if the Fortran pointer value supplied is zero.
*-
*/

/* Local Variables: */
   unsigned int i;               /* Loop counter for searching pointer list */
   void *result = NULL;          /* Result value to return */

/* Search the pointer list for an entry whose Fortran pointer matches
   the one supplied. */
   for ( i = 0; i < pointer_count; i++ ) {
      if ( pointer_list[ i ] &&
           ( fpointer == (F77_POINTER_TYPE) CTOFORTRAN( pointer_list[ i ] ))) {

/* If found, return the C version of the pointer. */
         result = pointer_list[ i ];
         break;
      }
   }

/* Return the result. */
   return result;
}

F77_POINTER_TYPE cnfFptr( void *cpointer ) {
/*
*+
*  Name:
*     cnfFptr

*  Purpose:
*     Convert a C pointer to a Fortran pointer.

*  Invocation:
*     fpointer = cnfFptr( cpointer )

*  Description:
*     Given a C pointer, this function returns the equivalent Fortran
*     pointer of type F77_POINTER_TYPE. Note that this conversion is
*     only performed if the C pointer has originally been registered
*     (using cnfRegp) for use from both C and Fortran. All pointers
*     to space allocated by cnfCalloc and cnfMalloc are
*     automatically registered in this way.

*  Arguments:
*     void *cpointer (Given)
*        The C pointer.

*  Returned Value:
*     F77_POINTER_TYPE cnfCptr
*        The equivalent Fortran pointer value.

*  Notes:
*     - A value of zero will be returned if the C pointer has not
*     previously been registered for use from both C and Fortran, or
*     if a NULL pointer is supplied.
*-
*/

/* Local Variables: */
   unsigned int i;                /* Loop counter for searching pointer list */
   F77_POINTER_TYPE result = (F77_POINTER_TYPE) 0; /* Result to return */

/* Do not bother translating if we have a null pointer */
   if ( cpointer == NULL ) return result;

/* Search the pointer list for an entry whose C pointer matches the
   one supplied. */
   for ( i = 0; i < pointer_count; i++ ) {
      if ( pointer_list[ i ] && ( cpointer == pointer_list[ i ] ) ) {

/* If found, return the Fortran version of the pointer. */
         result = (F77_POINTER_TYPE) CTOFORTRAN( pointer_list[ i ] );
         break;
      }
   }

/* Return the result. */
   return result;
}

void cnfFree( void *pointer ) {
/*
*+
*  Name:
*     cnfFree

*  Purpose:
*     Free allocated space.

*  Invocation:
*     cnfFree( pointer )

*  Description:
*     Free space that was previously allocated by a call to
*     cnfCalloc, cnfCreat, cnfCreib, cnfCreim or cnfMalloc.

*  Arguments:
*     void *pointer (Given)
*        A pointer to the space to be freed.

*  Notes:
*     - This function is not simply equivalent to the C free()
*     function, since if the pointer has been registered (using
*     cnfRegp) for use by both C and Fortran, then it will be
*     unregistered before the space is freed. All pointers to space
*     allocated by cnfCalloc and cnfMalloc are automatically
*     registered in this way, so cnfFree should always be used to
*     free them.
*     - It is also safe to free unregistered pointers with this
*     function.
*-
*/

/* Unregister the pointer and decrement it by the offset applied when
   it was issued (this will be zero if the pointer was not
   registered). */
   pointer = (void *) ( (char *) pointer - Unregister( pointer ) );

/* Free the allocated memory. */
   starFree( pointer );
}

static void *Malloc( size_t size, int zero ) {
/*
*  Name:
*     Malloc

*  Purpose:
*     Allocate memory that may be accessed from C and Fortran.

*  Invocation:
*     ptr = Malloc( size, zero );

*  Description:
*     This function allocates memory in the same way as the standard C
*     calloc() and malloc() functions, except that the pointer to the
*     allocated memory is automatically registered (using cnfRegp)
*     for use from both C and Fortran. This means that the returned
*     pointer may subsequently be converted into a Fortran pointer of
*     type F77_POINTER_TYPE (using cnfFptr) and back into a C pointer
*     (using cnfCptr). The contents of the memory may therefore be
*     accessed from both languages.

*  Arguments:
*     size_t size (Given)
*        The size of the required memory region.
*     int zero (Given)
*        If this is zero, the memory is not initialised (like
*        malloc()). If it is non-zero, the memory is initialised to
*        zero (like calloc()).

*  Returned Value:
*     void *Malloc
*        A registered pointer to the allocated memory, or NULL if the
*        memory could not be allocated.

*  Notes:
*     - The allocated memory should be freed using cnfFree when no
*     longer required.
*/

/* Local Constants: */
   const size_t offset_step = sizeof( long int ); /* Offset for OK alignment */

/* Local Variables: */
   int slot;                     /* Location of pointer in pointer list */
   size_t offset = (size_t) 0;   /* Offset from start of allocated memory */
   void *mem;                    /* Pointer to allocated memory */
   void *result;                 /* Pointer value to return */

/* Loop until an acceptable memory pointer has been found or an error
   occurs. */
   while ( 1 ) {

/* Attempt to allocate the required amount of memory, plus an extra
   amount to permit the returned pointer to be offset from the start
   of the allocated memory if necessary. */
      mem = zero ? starCalloc( (size_t) 1, size + offset ) :
                   starMalloc( size + offset );

/* Quit if the memory could not be allocated. */
      if ( !mem ) {
         result = NULL;
         break;

/* Otherwise, calculate the returned pointer value by offsetting from
   the start of the allocated memory. */
      } else {
         result = (void *) ( (char *) mem + offset );

/* See if this pointer can be registered (i.e. is unique when
   converted into a Fortran pointer). */
         if ( !( slot = Register( result ) ) ) {

/* If it cannot, then free the allocated memory and increment the
   offset, so that a new pointer value will be produced next time. */
            starFree( mem );
            offset += offset_step;

/* If the pointer was successfully registered, then store the offset
   value associated with it. */
         } else if ( slot > 0 ) {
            offset_list[ slot - 1 ] = offset;
            break;

/* If an error occurred during registration, then free the allocated
   memory and quit. */
         } else {
            starFree( mem );
            result = NULL;
            break;
         }
      }
   }

/* Return the result. */
   return result;
}

void *cnfMalloc( size_t size ) {
/*
*+
*  Name:
*     cnfMalloc

*  Purpose:
*     Allocate space that may be accessed from C and Fortran.

*  Invocation:
*     cpointer = cnfMalloc( size );

*  Description:
*     This function allocates space in the same way as the standard C
*     malloc() function, except that the pointer to the space
*     allocated is automatically registered (using cnfRegp) for use
*     from both C and Fortran. This means that the returned pointer
*     may subsequently be converted into a Fortran pointer of type
*     F77_POINTER_TYPE (using cnfFptr), and back into a C pointer
*     (using cnfCptr). The contents of the space may therefore be
*     accessed from both languages.

*  Arguments:
*     size_t size (Given)
*        The size of the required space.

*  Returned Value:
*     void *cnfMalloc
*        A registered pointer to the allocated space, or NULL if the
*        space could not be allocated.

*  Notes:
*     - The allocated space should be freed using cnfFree when no
*     longer required.
*-
*/

/* Allocate the required memory without initialisation. */
   return Malloc( size, 0 );
}

void *cnfRealloc( void * pntr, size_t size ) {
/*
*+
*  Name:
*     cnfMalloc

*  Purpose:
*     Re-Allocate space that may be accessed from C and Fortran.

*  Invocation:
*     cpointer = cnfMalloc( size );

*  Description:
*     This function allocates space in the same way as the standard C
*     remalloc() function, except that the pointer to the space
*     reallocated is automatically registered (using cnfRegp) for use
*     from both C and Fortran. This means that the returned pointer
*     may subsequently be converted into a Fortran pointer of type
*     F77_POINTER_TYPE (using cnfFptr), and back into a C pointer
*     (using cnfCptr). The contents of the space may therefore be
*     accessed from both languages.

*  Arguments:
*     void * pntr (Given)
*        Pointer to be re-allocated. Must have been malloced by cnfMalloc.
*        If new memory is allocated by this routine, this pointer will no
*        longer be valid. If the resize fails this pointer will still be valid.
*        This conforms to the standard ANSI C behaviour of realloc.
*     size_t size (Given)
*        The size of the required space.

*  Returned Value:
*     void *cnfRealloc
*        A registered pointer to the re-allocated space, or NULL if the
*        space could not be allocated. If NULL, "pntr" is not changed or freed.

*  Notes:
*     - The re-allocated space should be freed using cnfFree when no
*     longer required.
*-
*/

  int    reg;  /* Error status from pointer registration */
  void * p;    /* Temp pointer */
  void * temp; /* Local copy of pointer from realloc */

  /* Try to resize */
  temp = starRealloc( pntr, size );

/* If a pointer to new memory was returned, then un-register the old        */
/* pointer (if not NULL).                                                   */

   if ( ( temp != pntr ) && ( pntr != NULL ) ) cnfUregp( pntr );

/* If a pointer to new memory was returned, attempt to register the new     */
/* pointer (if not NULL).                                                   */

   if ( ( temp != pntr ) && ( temp != NULL ) )
   {
      reg = cnfRegp( temp );

/* If it could not be registered, then attempt to allocate some new memory  */
/* with a registered pointer associated with it.                            */

      if ( !reg )
      {
         p = cnfMalloc( size );

/* If successful, transfer the data to the new (registered) memory and free */
/* the memory which could not be registered.                                */

         if ( p )
         {
            memcpy( p, temp, size );
            starFree( temp );
            temp = p;
         }
         else

/* If no registered memory was available, free the unregistered memory and  */
/* set the returned pointer to NULL.                                        */
         {
            starFree( temp );
            temp = NULL;
         }
      }

/* If an error occurred during pointer registration, free the unregistered  */
/* memory and set the returned pointer to NULL.                             */

      else if ( reg < 0 )
      {
         starFree( temp );
         temp = NULL;
      }
   }

   return temp;
}

void *F77_EXTERNAL_NAME(cnf_pval)( POINTER(FPTR) ) {
/*
*+
*  Name:
*     CNF_PVAL

*  Purpose:
*     Expand a Fortran pointer to its full value.

*  Invocation:
*     CALL DOIT( ..., %VAL( CNF_PVAL( FPTR ) ), ... )

*  Description:
*     Given a Fortran pointer, stored in an INTEGER variable, this
*     function returns the full value of the pointer (on some
*     platforms, this may be longer than an INTEGER). Typically, this
*     is only required when the pointer is used to pass dynamically
*     allocated memory to another routine using the "%VAL" facility.

*  Arguments:
*     FPTR = INTEGER (Given)
*        The Fortran pointer value.

*  Returned Value:
*     CNF_PVAL
*        The full pointer value.

*  Notes:
*     - The data type of this function will depend on the platform in
*     use and is declared in the include file CNF_PAR.
*-

*  Implementation Notes:
*     - This routine is designed to be called from Fortran.
*/

/* Local Variables: */
   GENPTR_POINTER(FPTR)

/* Obtain the C pointer value and return it. */
   return cnfCptr( *FPTR );
}

TRAIL_TYPE F77_EXTERNAL_NAME(cnf_cval)( INTEGER(FINT) ) {
/*
*+
*  Name:
*     CNF_CVAL

*  Purpose:
*     Convert a Fortran INTEGER into the same type as used in the TRAIL macro.

*  Invocation:
*     CALL DOIT( %VAL( CNF_PVAL( FPTR ) ),..., %VAL( CNF_CVAL( FINT ) ) )

*  Description:
*     When passing dynamically allocated character strings to Fortran
*     or C routines the character string length is passed as a hidden
*     argument after the visible ones (see TRAIL). With some compilers this
*     length is a 64bit long (INTEGER*8), whereas for others it is more
*     typically a 32bit int (INTEGER*4). Using this function avoids the need
*     to know which size is used for the configured compiler.

*  Arguments:
*     FINT = INTEGER (Given)
         The FORTRAN integer value giving the expected length of the strings.

*  Returned Value:
*     CNF_CVAL
*        The string length in the correct type for the configured compiler.
*        Fortran equivalent of the type used by the TRAIL macro.

*  Notes:
*     - The data type of this function will depend on the platform in
*     use and is declared in the include file CNF_PAR.
*     - When mixing calls that pass locally declared character strings
*     and dynamically allocated ones, all the declared strings
*     must preceed all the dynamic ones in the argument list so that
*     the order of the TRAIL arguments is known.
*-

*  Implementation Notes:
*     - This routine is designed to be called from Fortran.
*/

/* Local Variables: */
   GENPTR_INTEGER(FINT)

   /* Cast to the TRAIL_TYPE */
   return (TRAIL_TYPE) *FINT;
}

/* Note that fortran will pass us a pointer to the %LOC value
   which in C is a pointer to a pointer */
F77_POINTER_FUNCTION(cnf_preg)( void ** locptr, LOGICAL(isnew) ) {
/*
*+
*  Name:
*     CNF_PREG

*  Purpose:
*     Register a full Fortran pointer from %LOC for later CNF_PVAL usage

*  Invocation:
*     IPNTR = CNF_PREG( %LOC( SOME_VAR ), ISNEW )

*  Description:
*     Given a Fortran pointer AS GENERATED BY %LOC (assuming %LOC
*     returns something the same size as a C pointer), register it with
*     CNF and return a key in a Fortran INTEGER variable which can be
*     used later to retrieve the original pointer using CNF_PVAL.

*  Arguments:
*     LOCPTR = INTEGER*X (Given)
*        The output from %LOC. If called using an INTEGER the
*        size of INTEGER must match the size of integer returned
*        from %LOC.
*     ISNEW = LOGICAL (Returned)
*        TRUE if the pointer was registered successfully.
*        FALSE if the pointer was already registered. The result is
*        undefined if the function itself returned 0.

*  Returned Value:
*     CNF_PREG = INTEGER
*        The pointer squeezed into an INTEGER*4. Will contain zero
*        if the pointer could not be registered.

*  Notes:
*     - The function is declared in CNF_PAR
*     - Do not use this in a function call. The 0 return value on failure
*       will result in a SEGV. Check the return value before use.
*     - If the pointer can not be registered usually nothing can be done
*       about it without changing variables.
*     - Use the second argument to determine whether you should
*       be responsible for unregistering the pointer.
*     - The pointer should be unregistered using CNF_UNREGP
*       when it is no longer required.
*     - If the routine is to be called using an INTEGER argument
*       make sure that the size of the INTEGER matches the size
*       returned by %LOC (ie a C pointer). Using an INTEGER*8
*       on a 32-bit system may lead to failures on big endian
*       systems (such as PowerPC).

*  Implementation Notes:
*     - This routine is designed to be called from Fortran.

*-
*/

/* Local Variables: */
  GENPTR_LOGICAL(isnew)
  int status;
  F77_POINTER_TYPE f77_ptr;

  *isnew = F77_FALSE;

  /* First see if the pointer has already been registered */
  f77_ptr = cnfFptr( *locptr );

  /* If we have non-zero, we just return it and leave isnew at FALSE */
  if (f77_ptr) return f77_ptr;

  /* Register the pointer */
  /* Note that to simplify the interface we do not ask for this pointer
     to be passed in as %VAL( %LOC( VAR ) ), only  %LOC( VAR ).
     This means we need to deref to get the pointer itself */
  status = cnfRegp( *locptr );

  /* Could not register, return a "null" pointer */
  if (status != 1) return (F77_POINTER_TYPE)0;

  /* Obtain the Fortran pointer value and return it. */
   *isnew = F77_TRUE;
   return cnfFptr( *locptr );
}

/* Do not call this cnf_uregp (to match C interface) because the
   backwards compatible name mangling converts it to cnfUregp_ */
F77_SUBROUTINE(cnf_unregp)( POINTER(FPTR) ) {
/*
*+
*  Name:
*     CNF_UNREGP

*  Purpose:
*     Unregister a CNF fortran pointer

*  Invocation:
*     CALL CNF_UNREGP( FPNTR )

*  Description:
*     This is a Fortran interface to the C cnfUregp function.
*     It is used to unregister pointers from CNF such that CNF_PVAL
*     will no longer be able to translate the Fortran "pointer" to
*     a true C pointer. Generally should only be used in conjunction
*     with CNF_PREG.

*  Arguments:
*     FPNTR = INTEGER (Given & Returned)
*        Fortran version of C pointer, as returned by CNF_PREG.
*        If the pointer has a value of 0 no action is taken.
*        The integer is set to 0 on exit if it translated to a valid
*        C pointer.

*  Implementation Notes:
*     - This routine is designed to be called from Fortran.
*/

/* Local Variables: */
   GENPTR_POINTER(FPTR)
   void * cptr;

/* Translate the fortran pointer to the C pointer */
   cptr = cnfCptr( *FPTR );
   if ( !cptr ) return;

/* Unregister the pointer and clear it */
   cnfUregp( cptr );
   *FPTR = 0;
}

static int Register( void *ptr ) {
/*
*  Name:
*     Register

*  Purpose:
*     Register a pointer for use from both C and Fortran.

*  Invocation:
*     result = Register( ptr )

*  Description:
*     This function attempts to register a C pointer so that it may be
*     used from both C and Fortran. If successful, registration
*     subsequently allows the pointer to be converted into a Fortran
*     pointer of type F77_POINTER_TYPE, and then back into a C
*     pointer, even if the Fortran pointer is stored in a smaller data
*     type than the C pointer.
*
*     Not all C pointers may be registered, and registration may fail
*     if the Fortran version of the pointer is indistinguishable from
*     that of a pointer which has already been registered. In such a
*     case, a new C pointer must be obtained (e.g. by allocating a
*     different region of memory).

*  Arguments:
*     void *ptr (Given)
*        The pointer to be registered.

*  Returned Value:
*     int Register
*        If registration was successful, the function returns one more
*        than the index in the "pointer_list" array where the
*        registered pointer information has been stored. If
*        registration was unsuccessful, zero is returned.
*
*  Notes:
*     - If an internal error occurs, the function returns -1.
*/

/* Local Constants: */
   const size_t init_size = (size_t) 64; /* Initial pointer list size */

/* Local Variables: */
   unsigned int i;               /* Loop counter for searching list */
   int result = 0;               /* Result value to return */
   int unique = 1;               /* Fortran pointer is unique? */
   int vacant = 0;               /* Location of first vacant list element */
   unsigned long int f77_ptr;    /* Fortran version of pointer */
   void *tmp;                    /* Temprary pointer to re-allocated memory */
/* Extract the lower bits from the C pointer to form the equivalent
   Fortran pointer. We can only register the pointer if the Fortran
   version is not zero, since it will otherwise conflict with the NULL
   pointer. */
   if ( ( f77_ptr = CTOFORTRAN(ptr) ) ) {

/* Search the list of registerd pointers to see if the new Fortran one
   is unique. */
      for ( i = 0; i < pointer_count; i++ ) {
         if ( pointer_list[ i ] ) {
            unique = f77_ptr != CTOFORTRAN( pointer_list[ i ] );
            if ( !unique ) break;

/* On the same pass, also search for the first list element (if any)
   which does not have a pointer entry. */
         } else {
            if ( !vacant ) vacant = i + 1;
         }
      }

/* If the new Fortran pointer is unique, we can register it. */
      if ( unique ) {

/* If a vacant list element was found, then re-use it to store the C
   version of the pointer, and initialise its offset. Note which
   element was used. */
         if ( vacant ) {
            pointer_list[ vacant - 1 ] = ptr;
            offset_list[ vacant - 1 ] = (size_t) 0;
            result = vacant;

/* Otherwise, check if the space allocated for the list is sufficient
   to allow a new element at the end. */
         } else {
            if ( pointer_count == pointer_max ) {

/* If not, then calculate a new list size, doubling it each time an
   extension becomes necessary. */
               pointer_max = pointer_max ? 2 * pointer_max : init_size;

/* Re-allocate the space for the pointer list, checking for memory
   allocation errors. */
               if ( ( tmp = starRealloc( pointer_list, sizeof( void * ) *
                                                   (size_t) pointer_max ) ) ) {
                  pointer_list = tmp;
               } else {
                  result = -1;
               }

/* Similarly, re-allocate memory for the list of pointer offsets. */
               if ( ( tmp = starRealloc( offset_list, sizeof( size_t ) *
                                                  (size_t) pointer_max ) ) ) {
                  offset_list = tmp;
               } else {
                  result = -1;
               }
            }

/* If OK, store the C pointer and initialise its offset. Increment the
   list length, noting which element was used. */
            if ( !result ) {
               pointer_list[ pointer_count ] = ptr;
               offset_list[ pointer_count ] = (size_t) 0;
               result = ++pointer_count;
            }
         }
      }
   }

/* If registration was successful, increment the count of registered
   pointers. */
   if ( result > 0 ) registered_pointers++;

/* Return the result. */
   return result;
}

int cnfRegp( void *cpointer ) {
/*
*+
*  Name:
*     cnfRegp

*  Purpose:
*     Register a pointer for use from both C and Fortran.

*  Invocation:
*     result = cnfRegp( cpointer )

*  Description:
*     This is a low-level function which will normally only be
*     required if you are implementing your own memory allocation
*     facilities (all memory allocated by cnfCalloc and cnfMalloc is
*     automatically registered using this function).
*
*     The function attempts to register a C pointer so that it may be
*     used from both C and Fortran. If successful, registration
*     subsequently allows the pointer to be converted into a Fortran
*     pointer of type F77_POINTER_TYPE (using cnfFptr), and then back
*     into a C pointer (using cnfCptr).  These conversions are
*     possible even if the Fortran pointer is stored in a shorter data
*     type than the C pointer.
*
*     Not all C pointers may be registered, and registration may fail
*     if the Fortran version of the pointer is indistinguishable from
*     that of a pointer which has already been registered. In such a
*     case, a new C pointer must be obtained (e.g. by allocating a
*     different region of memory).

*  Arguments:
*     void *cpointer (Given)
*        The C pointer to be registered.

*  Returned Value:
*     int cnfRegp
*        If registration was successful, the function returns 1. If
*        registration was unsuccessful, it returns zero.

*  Notes:
*     - If an internal error occurs (e.g. if insufficient memory is
*     available), the function returns -1.
*-
*/

/* Local Variables: */
   int result;                   /* Result value to return. */
/* Attempt to register the pointer. */
   result = Register( cpointer );
/* If successful, return 1. */
   if ( result > 0 ) result = 1;

/* Return the result. */
   return result;
}

static size_t Unregister( void *ptr ) {
/*
*  Name:
*     Unregister

*  Purpose:
*     Unregister a pointer previously registered using Register.

*  Invocation:
*     offset = Unregister( ptr )

*  Description:
*     This function accepts a C pointer which has previously been
*     registered for use from both C and Fortran (using Register) and
*     removes its registration. Subsequently, conversion between the C
*     pointer and its Fortran equivalent (and vice versa) will no
*     longer be supported.

*  Arguments:
*     void *ptr (Given)
*        The C pointer to be unregistered.

*  Returned Value:
*     size_t offset
*        The offset between the pointer and the start of the allocated
*        region of memory, as applied when the pointer was first
*        issued (this only applies to pointers issued from within this
*        module, otherwise the offset is always zero).

*  Notes:
*     - No action occurs (and no error results) if the C pointer has
*     not previously been registered for use from both C and Fortran.
*/

/* Local Variables: */
   unsigned int i;               /* Loop counter for searching pointer list */
   size_t result = (size_t) 0;   /* Result value to return */

/* Search the pointer list for an entry whose C pointer matches the
   one supplied. */
   for ( i = 0; i < pointer_count; i++ ) {
      if ( pointer_list[ i ] && ( ptr == pointer_list[ i ] ) ) {

/* If found, extract the pointer offset and clear the list entry. */
         result = offset_list[ i ];
         pointer_list[ i ] = NULL;
         offset_list[ i ] = (size_t) 0;

/* Decrement the count of registered pointers. If this reaches zero,
   then reset the pointer list counts and free the associated
   memory. */
         if ( !--registered_pointers ) {
            pointer_max = 0;
            pointer_count = 0;
            starFree( pointer_list ); pointer_list = NULL;
            starFree( offset_list ); offset_list = NULL;
         }
         break;
      }
   }

/* Return the result. */
   return result;
}

void cnfUregp( void *cpointer ) {
/*
*+
*  Name:
*     cnfUregp

*  Purpose:
*     Unregister a pointer previously registered using cnfRegp.

*  Invocation:
*     cnfUregp( cpointer )

*  Description:
*     This is a low-level function which will normally only be
*     required if you are implementing your own memory allocation
*     facilities.
*
*     The function accepts a C pointer which has previously been
*     registered for use from both C and Fortran (using cnfRegp) and
*     removes its registration. Subsequently, conversion between the C
*     pointer and its Fortran equivalent (and vice versa) will no
*     longer be performed by cnfFptr and cnfCptr.

*  Arguments:
*     void *cpointer (Given)
*        The C pointer to be unregistered.

*  Notes:
*     - No action occurs (and no error results) if the C pointer has
*     not previously been registered for use from both C and Fortran.
*-
*/

/* Unregister the pointer. */
   (void) Unregister( cpointer );
}
