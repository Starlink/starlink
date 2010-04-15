#if HAVE_CONFIG_H
#  include <config.h>
#endif

#include <stddef.h>
#include "hds1.h"                /* Global definitions for HDS              */
#include "dat_err.h"             /* DAT__ error codes                       */
#include "rec.h"                 /* Public rec_ definitions                 */
#include "rec1.h"                /* Internal rec_ definitions               */

/* Module-Level Data Structures:                                            */
   struct addrng{
      unsigned char *start;
      unsigned char *end;
   };

/* Module-Level Variables:                                                  */
   static int mxrng = 0;         /* Allocated size of address range array   */
   static int nrng = 0;          /* Number of address range entries         */
   static struct addrng *free_addr = NULL; /* Pointer to address ranges     */

   int rec1_get_addr( size_t size, unsigned char **start, unsigned char **end )
   {
/*+                                                                         */
/* Name:                                                                    */
/*    rec1_get_addr                                                         */

/* Purpose:                                                                 */
/*    Allocate a range of contiguous unused addresses.                      */

/* Invocation:                                                              */
/*    rec1_get_addr( size, start, end )                                     */

/* Description:                                                             */
/*    This function searches an internal list of free addresses to find a   */
/*    contiguous range of addresses of a specified size. If such a range is */
/*    found, it removes these addresses from the list and returns pointers  */
/*    to the start and end of the allocated range. New ranges of free       */
/*    addresses can be added to the internal list at any time using         */
/*    rec1_put_addr, which should also be used to return addresses          */
/*    allocated by this function when they are no longer required.          */

/* Parameters:                                                              */
/*    size_t size                                                           */
/*       Size of the address range required (in unsigned chars).            */
/*    unsigned char **start                                                 */
/*       Pointer to an unsigned char pointer in which the start of the      */
/*       allocated range will be returned (if found).                       */
/*    unsigned char **end                                                   */
/*       Pointer to an unsigned char pointer in which the end of the        */
/*       allocated range will be returned (if found).                       */

/* Returned Value:                                                          */
/*    int rec1_get_addr                                                     */
/*       Returns 1 if a suitable range of addresses could be found,         */
/*       otherwise it returns zero.                                         */

/* Notes:                                                                   */
/*    -  Null pointer values are returned via the start and end parameters  */
/*    if no suitable address range can be found.                            */
/*    -  The addresses handled by this function are not referenced, thus    */
/*    they need not necessarily be accessible.                              */

/* Authors:                                                                 */
/*    RFWS: R.F. Warren-Smith (STARLINK)                                    */
/*    {@enter_new_authors_here@}                                            */

/* History:                                                                 */
/*    19-DEC-1991 (RFWS):                                                   */
/*       Original version.                                                  */
/*    {@enter_changes_here@}                                                */

/* Bugs:                                                                    */
/*    {@note_any_bugs_here@}                                                */

/*-                                                                         */

/* Local Variables:                                                         */
      int found;                 /* Suitable address range found?           */
      int i;                     /* Loop counter for address ranges         */
      int ii=0;                  /* Index of address range found            */
      size_t best=0;             /* Size of best-fit address range          */
      size_t s;                  /* Size of free address range              */

/*.                                                                         */

/* Initialise.                                                              */
      found = 0;
      *start = NULL;
      *end = NULL;

/* Loop to search for a range of free addresses of sufficient size.         */
      for ( i = 0; i < nrng; i++ )
      {

/* Determine the size of the current range. Quit searching if it matches    */
/* the required size exactly.                                               */
         s = ( free_addr[ i ].end - free_addr[ i ].start ) + 1;
         if ( s == size )
         {
            ii = i;
            found = 1;
            break;
         }

/* Otherwise, only consider ranges which are larger than the required size. */
         else if ( s > size )
         {

/* Note when the first if these is found, storing its size as the "best     */
/* fit" so far.                                                             */
            if ( !found )
            {
               ii = i;
               best = s;
               found = 1;
            }

/* After the first possible range has been found, only consider new ranges  */
/* which fit better than the best fit so far.                               */
            else if ( s < best )
            {
               ii = i;
               best = s;
            }
         }
      }

/* If a suitable range has been found, set pointers to the start and end of */
/* the required part of it.                                                 */
      if ( found )
      {
         *start = free_addr[ ii ].start;
         *end = *start + ( size - 1 );

/* Update the start of the range to allow for the bit we've used.           */
         free_addr[ ii ].start += size;

/* If the range is now empty, eliminate it by shifting the remaining ranges */
/* down to fill the gap.                                                    */
         if ( free_addr[ ii ].start > free_addr[ ii ].end )
         {
            for ( i = ii; i < ( nrng - 1 ); i++ )
            {
               free_addr[ i ] = free_addr[ i + 1 ];
            }

/* Decrement the number of free address ranges available.                   */
            nrng--;
         }
      }

/* Return whether a suitable region was found.                              */
      return found;
   }

   void rec1_put_addr( unsigned char *start, unsigned char *end, int *status )
   {
/*+                                                                         */
/* Name:                                                                    */
/*    rec1_put_addr                                                         */

/* Purpose:                                                                 */
/*    Register a range of free addresses for subsequent re-use.             */

/* Invocation:                                                              */
/*    rec1_put_addr( start, end, status )                                   */

/* Description:                                                             */
/*    This function registers (or re-registers) a range of free addresses   */
/*    as available for subsequent re-allocation by rec1_get_addr.           */

/* Parameters:                                                              */
/*    unsigned char *start                                                  */
/*       Pointer to the start of the free address range.                    */
/*    unsigned char *end                                                    */
/*       Pointer to the end of the free address range.                      */
/*    int *status                                                           */
/*       Inherited global status.                                           */

/* Returned Value:                                                          */
/*    void                                                                  */

/* Notes:                                                                   */
/*    -  The addresses handled by this function are not referenced, thus    */
/*    they need not necessarily be accessible.                              */

/* Authors:                                                                 */
/*    RFWS: R.F. Warren-Smith (STARLINK)                                    */
/*    {@enter_new_authors_here@}                                            */

/* History:                                                                 */
/*    19-DEC-1991 (RFWS):                                                   */
/*       Original version.                                                  */
/*    {@enter_changes_here@}                                                */

/* Bugs:                                                                    */
/*    {@note_any_bugs_here@}                                                */

/*-                                                                         */

/* Local Variables:                                                         */
      int i;                     /* Loop counter for address ranges         */
      int ii;                    /* Loop counter for address ranges         */
      int new;                   /* New size for address range array        */

/*.                                                                         */

/* Check inherited global status.                                           */
      if ( !_ok( *status ) ) return;

/* Loop to find an existing free address range whose start address is       */
/* larger than the start of the address range being stored. This determines */
/* where in the list of address ranges the new one should be added.         */
      for ( i = 0; i < nrng; i++ )
      {
         if ( start < free_addr[ i ].start ) break;
      }

/* Check to see if the new range lies between two existing ranges and       */
/* completely fills the missing addresses between them.                     */
      if ( ( ( i > 0 ) && ( i < nrng ) ) &&
           ( start == ( free_addr[ i - 1 ].end + 1 ) ) &&
           ( end == ( free_addr[ i ].start - 1 ) ) )
      {

/* If so, merge all three ranges into one and loop to shift the remaining   */
/* ranges down, so as to turn the original pair of ranges into a single     */
/* range.                                                                   */
         free_addr[ i - 1 ].end = free_addr[ i ].end;
         for ( ii = i; ii < ( nrng - 1 ); ii++ )
         {
            free_addr[ ii ] = free_addr[ ii + 1 ];
         }

/* Decrement the number of free ranges.                                     */
         nrng--;
      }

/* Otherwise, see if the new range is contiguous with the range below. If   */
/* so, then merge them.                                                     */
      else if ( ( i > 0 ) && ( start == ( free_addr[ i - 1 ].end + 1 ) ) )
      {
         free_addr[ i - 1 ].end = end;
      }

/* Otherwise, see if the new range is contiguous with the range above. If   */
/* so, then merge them.                                                     */
      else if ( ( i < nrng ) && ( end == ( free_addr[ i ].start - 1 ) ) )
      {
         free_addr[ i ].start = start;
      }

/* Otherwise, we must add a new address range to the array. See if the      */
/* allocated array size is sufficient.                                      */
      else
      {
         if ( mxrng <= nrng )
         {

/* If not, allocate some space, or extend the space already allocated if    */
/* necessary.                                                               */
            if ( mxrng == 0 )
            {
               new = 2;
               rec_alloc_mem( new * sizeof( struct addrng ),
                              (void **) &free_addr );
            }
            else
            {
               new = 2 * mxrng;
               rec_reall_mem( new * sizeof( struct addrng ),
                              (void **) &free_addr );
            }

/* If successful, note the new allocated size.                              */
            *status = hds_gl_status;
            if ( _ok( *status ) )
            {
               mxrng = new;
            }
         }

/* Shift remaining address ranges up to make room for the new one.          */
         if ( _ok( *status ) )
         {
            for ( ii = nrng; ii > i; ii-- )
            {
               free_addr[ ii ] = free_addr[ ii - 1 ];
            }

/* Increment the number of address ranges stored and add the new one.       */
            nrng++;
            free_addr[ i ].start = start;
            free_addr[ i ].end = end;
         }
      }

/* Exit the routine.                                                        */
      return;
   }
