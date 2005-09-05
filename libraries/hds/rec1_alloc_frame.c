#if HAVE_CONFIG_H
#  include <config.h>
#endif

#include "ems.h"                 /* EMS error reporting routines            */
#include "hds1.h"                /* Global definitions for HDS              */
#include "rec1.h"                /* Internal rec_ definitions               */
#include "dat_err.h"             /* DAT__ error code definitions            */

   int rec1_alloc_frame( int slot, INT_BIG size, INT_BIG *bloc )
   {
/*+                                                                         */
/* Name:                                                                    */
/*    rec1_alloc_frame                                                      */

/* Purpose:                                                                 */
/*    Allocate a frame of blocks in a container file.                       */

/* Invocation:                                                              */
/*    rec1_alloc_frame( slot, size, bloc )                                  */

/* Description:                                                             */
/*    This function obtains a sequence of free blocks in a container file   */
/*    and allocates them for use, extending the file if necessary. The free */
/*    space stack is updated to reflect the fact that the new blocks are in */
/*    use.                                                                  */

/* Parameters:                                                              */
/*    int slot                                                              */
/*       Slot number of the container file in the File Control Vector.      */
/*    INT_BIG size                                                          */
/*       Number of contiguous blocks required in the frame.                 */
/*    INT_BIG *bloc                                                 */
/*       Pointer to an integer in which the block number of the first block */
/*       allocated will be returned (the first block in the file is no. 1). */

/* Returned Value:                                                          */
/*    int rec1_alloc_frame                                                  */
/*       The global status value current on exit.                           */

/* Authors:                                                                 */
/*    RFWS: R.F. Warren-Smith (STARLINK)                                    */
/*    BKM:  B.K. McIlwrath    (STARLINK)                                    */
/*    {@enter_new_authors_here@}                                            */

/* History:                                                                 */
/*    25-MAR-1991 (RFWS):                                                   */
/*       Prologue added and code tidied.                                    */
/*    22-MAY-1991 (RFWS):                                                   */
/*       Pass absolute file sizes to rec1_extend_file.                      */
/*    12-JUN-1991 (RFWS):                                                   */
/*       Fixed bug introduced in previous change - wrong sign for new       */
/*       number of spare blocks entered into free block stack.              */
/*    28-JUN-1991 (RFWS):                                                   */
/*       Eliminated use of a return statement in the middle of the routine. */
/*    5-AUG-1993 (RFWS):                                                    */
/*       Improved the commenting to make the method of stack usage clearer. */
/*    6-AUG-1993 (RFWS):                                                    */
/*       Extend the file when allocating blocks at the end to overcome      */
/*       problems with file truncation on VMS.                              */
/*    22-JUN-2000 (BKM):                                                    */
/*       Revise for extended format (64-bit) HDS files.                     */
/*    {@enter_further_changes_here@}                                        */

/* Bugs:                                                                    */
/*    {@note_any_bugs_here@}                                                */

/*-                                                                         */

/* Local Variables:                                                         */
      INT_BIG actsize;           /* Actual size of extended file            */
      INT_BIG newsize;           /* New size to extend file to              */
      int done;                  /* Search for free blocks succeeded?       */
      int empty;                 /* Position of empty stack entry           */
      int entno;                 /* Free block stack entry number           */
      int full;                  /* Free block stack full?                  */
      int i;                     /* Loop counter for free block stack       */
      struct HCB *hcb;           /* Pointer to Header Control Block         */
      struct STK *stk;           /* Pointer to free space stack             */

/*.                                                                         */

/* Check the inherited global status.                                       */
      if ( !_ok( hds_gl_status ) ) return hds_gl_status;

/* Initialise.                                                              */
      done = 0;

/* Locate the container file's Header Control Block and its free space      */
/* stack.                                                                   */
      rec1_locate_hcb( slot, 'U', &hcb );
      if ( _ok( hds_gl_status ) )
      {
         stk = hcb->stk;

/* Search the free block stack (which starts at the top) for a sequence of  */
/* contiguous blocks of the required size.                                  */
         for ( i = REC__MXSTK - 1; ; i-- )
         {

/* Quit searching when the end of the free block stack is reached, as       */
/* marked by the first unused entry (with .bloc still set to -1).           */
            if ( stk[ i ].bloc == -1 )
            {
               break;
            }

/* If a sequence of free blocks of sufficient size is found in the stack,   */
/* then note the block number at which the allocated frame should start.    */
            else if ( stk[ i ].spare >= size )
            {
               *bloc = stk[ i ].bloc;

/* Problems may arise here because previous versions of HDS did not cope    */
/* adequately with the effects of file truncation on VMS systems.           */
/*                                                                          */
/* Typically, an HDS file copied to a new VMS disk with a larger cluster    */
/* size than the original could have extra blocks added to it beyond the    */
/* (VMS) end of file mark. If new blocks were then required by HDS at the   */
/* end of the file, these extra blocks could be returned by VMS without     */
/* extending the file further. These would be entered into the free block   */
/* stack (and reflected in the HDS end of file mark) but because the file   */
/* did not need extending the (VMS) end of file mark was not updated (this  */
/* was the original HDS bug, now corrected). If the file was then truncated */
/* (say by moving it to another disk with a smaller cluster size), HDS      */
/* could now believe that free blocks existed beyond the physical end of    */
/* the file.                                                                */
/*                                                                          */
/* The original problem has been fixed, but old files with this problem may */
/* still be encountered. To handle them, we test to see if the range of     */
/* free blocks being allocated extends to the (HDS) end of file mark. If it */
/* does, we extend the file to ensure it is really large enough and update  */
/* the (HDS) end of file mark and the free block stack entry (in case more  */
/* space than we requested was actually allocated).                         */
               if ( stk[ i ].bloc + stk[ i ].spare > hcb->eof )
               {
                  rec1_extend_file( slot, hcb->eof, &actsize );
                  if ( _ok ( hds_gl_status ) )
                  {
                     hcb->eof = actsize;
                     stk[ i ].spare = hcb->eof - *bloc + 1;
                  }
               }

/* Allocate the required blocks by updating the free block stack entry and  */
/* quit searching.                                                          */
               stk[ i ].spare -= size;
               stk[ i ].bloc  += size;
               done = 1;
               break;
            }
         }

/* If the search for free blocks failed, then the file must be extended.    */
         if ( !done )
         {

/* Search the stack again to determine which entry should record the newly  */
/* allocated space.                                                         */
            *bloc = hcb->eof + 1;
            empty = 0;
            for ( i = REC__MXSTK - 1; ; i-- )
            {

/* If the end of the stack is reached, then use the next entry, unless an   */
/* empty entry (one with zero blocks) has been found first. Quit searching. */
               if ( stk[ i ].bloc == -1 )
               {
                  entno = _max( i, empty );
                  break;
               }

/* If any stack entry says there are spare blocks up to the end of the      */
/* file, then use that entry (the newly allocated blocks will be added to   */
/* them).                                                                   */
               else if ( stk[ i ].bloc + stk[ i ].spare > hcb->eof )
               {
                  entno = i;
                  *bloc = stk[ i ].bloc;
                  break;
               }

/* Note if any existing stack entries are empty. These are re-used in       */
/* preference to extending the stack.                                       */
               else if ( stk[ i ].spare == 0 )
               {
                  empty = i;
               }
            }

/* Extend the file sufficiently to accommodate the frame and record the new */
/* actual size of the file in the HCB end-of-file pointer.                  */
            newsize = *bloc + size - 1;
            rec1_extend_file( slot, newsize, &actsize );
            if ( _ok( hds_gl_status ) )
            {
               hcb->eof = actsize;

/* See if the HCB stack is full. This will be so if the free block stack    */
/* entry to be used next is unused but is adjacent to the top of the free   */
/* chip stack (which starts at the bottom).                                 */
               full = ( ( stk[ entno ].bloc == -1 ) &&
                        ( stk[ entno - 1 ].bloc != -1 ) );

/* Fill the free space stack entry (if one is available) with any space     */
/* left over from the file extension (if the free space stack is full, any  */
/* extra space is wasted and cannot be re-used).                            */
               if ( !full )
               {
                  stk[ entno ].spare = actsize - newsize;
                  stk[ entno ].bloc = *bloc + size;
               }
            }
         }
      }

/* Return the current global status value.                                  */
      return hds_gl_status;
   }
