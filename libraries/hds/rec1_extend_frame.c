#include "hds1_feature.h"	 /* Define feature-test macros, etc.	    */
#include "ems.h"		 /* EMS error reporting routines	    */
#include "hds1.h"		 /* Global definitions for HDS		    */
#include "rec1.h"		 /* Internal rec_ definitions		    */
#include "dat_err.h"		 /* DAT__ error code definitions	    */

   int rec1_extend_frame( int slot, int size, int extra, int *bloc )
   {
/*+									    */
/* Name:								    */
/*    rec1_extend_frame							    */

/* Purpose:								    */
/*    Extend an existing frame of blocks in a container file.		    */

/* Invocation:								    */
/*    rec1_extend_frame( slot, size, extra, bloc )			    */

/* Description:								    */
/*    This routine extends an existing frame of blocks in a container file. */
/*    The actual operations performed are dependent on which one of the	    */
/*    following conditions is true:					    */

/*    -  (1) If there is sufficient free space after the current end of the */
/*    frame, then this is allocated and the free space stack is up-dated.   */

/*    -  (2) Otherwise, if the last block in the frame coincides with the   */
/*    last allocated block in the file, then the file is extended	    */
/*    appropriately and the free space stack is updated.		    */

/*    -  (3) Otherwise, (or if the free space stack is full), then a new    */
/*    frame of the required size is allocated and the data in the existing  */
/*    frame moved. The old frame is then deallocated.			    */

/* Parameters:								    */
/*    int slot								    */
/*       Slot number of the container file in the File Control Vector.	    */
/*    int size								    */
/*       Current size of the frame in blocks.				    */
/*    int extra								    */
/*       Number of extra blocks required in the frame.			    */
/*    int * bloc							    */
/*	 Pointer to the starting block number of the frame (the first block */
/*	 in the file is no. 1).  This value may be changed on exit if the   */
/*	 frame has to be moved						    */

/* Returned Value:							    */
/*    int rec1_extend_frame						    */
/*       The global status value current on exit.			    */

/* Authors:								    */
/*    RFWS: R.F. Warren-Smith (STARLINK)				    */
/*    {@enter_new_authors_here@}					    */

/* History:								    */
/*    25-JUL-1990 (RFWS):						    */
/*       Added prologue.						    */
/*    8-MAY-1991 (RFWS):						    */
/*       Moved return statements to the end.				    */
/*    22-MAY-1991 (RFWS):						    */
/*       Pass absolute file sizes to rec1_extend_file.			    */
/*    6-AUG-1993 (RFWS):						    */
/*	 Extend the file when allocating blocks at the end to overcome	    */
/*	 problems with file truncation on VMS.				    */
/*    10-DEC-1993 (RFWS):						    */
/*	 Fixed bug introduced in last change: extra allocated space was not */
/*	 being entered properly into the free block stack.		    */
/*    {@enter_further_changes_here@}					    */

/* Bugs:								    */
/*    {@note_any_bugs_here@}						    */

/*-									    */

/* Local Variables:							    */
      int actsize;		 /* Actual size of extended file	    */
      int done;			 /* Extension operation complete?	    */
      int empty;		 /* Empty free space stack entry no.	    */
      int entno;		 /* Free space stack entry number	    */
      int full;			 /* Free space stack full?		    */
      int i;			 /* Loop counter for free space stack	    */
      int newbloc;		 /* First block no. of new frame	    */
      int newsize;		 /* New size to extend file to		    */
      int oldbloc;		 /* First block no. of old frame	    */
      struct HCB *hcb;		 /* Pointer to Header Control Block	    */
      struct STK *stk;		 /* Pointer to free space stack		    */
      unsigned char *newpdb;	 /* Pointer to start of new frame	    */
      unsigned char *oldpdb;	 /* Pointer to start of old frame	    */

/*.									    */

/* Check the inherited global status.					    */
      if ( !_ok( hds_gl_status ) ) return hds_gl_status;

/* Locate the Header Control Block and its free space stack.		    */
      rec1_locate_hcb( slot, 'U', &hcb );
      if ( _ok( hds_gl_status ) )
      {
         stk = hcb->stk;

/* Search the free block stack (which starts at the top) and determine if   */
/* there is sufficient space immediately after the frame. If not, then	    */
/* allocate the highest empty stack entry (if available).		    */
         empty = 0;
         for ( i = REC__MXSTK - 1; ; i-- )
         {
            if ( stk[ i ].bloc == -1 )
	    {
	       entno = _max( i, empty );
	       break;
	    }
	    else if ( stk[ i ].bloc == *bloc + size )
	    {
	       entno = i;
	       break;
	    }
	    else if ( stk[ i ].spare == 0 )
	    {
	       empty = i;
            }
         }

/* If a 'null' entry was allocated, then see if the stack is full.	    */
         full = ( ( stk[ entno ].bloc == -1 ) &&
                  ( stk[ entno - 1 ].bloc != -1 ) );

/* If not, then first determine if there are free blocks available at the   */
/* end of the frame.							    */
	 done = 0;
         if ( !full )
         {
            if ( stk[ entno ].spare >= extra )
	    {

/* Problems may arise here because previous versions of HDS did not cope    */
/* adequately with the effects of file truncation on VMS systems (see	    */
/* rec1_alloc_frame for details). To cope with this, we test to see if the  */
/* range of new blocks being allocated extends to the (HDS) end of file	    */
/* mark. If it does, we extend the file to ensure it is really large enough */
/* and update the (HDS) end of file mark and the free block stack entry (in */
/* case more space than we requested was actually allocated).		    */
	       if ( stk[ entno ].bloc + stk[ entno ].spare > hcb->eof )
               {
		  rec1_extend_file( slot, hcb->eof, &actsize );
		  if ( _ok ( hds_gl_status ) )
		  {
		     hcb->eof = actsize;
                     stk[ entno ].spare = hcb->eof - stk[ entno ].bloc + 1;
		  }
	       }

/* Allocate the extra free blocks by updating the free block stack.	    */
	       if ( _ok ( hds_gl_status ) )
	       {
	          stk[ entno ].spare -= extra;
	          stk[ entno ].bloc += extra;
	       }
	       done = 1;
            }

/*  If there are no free blocks, but the frame is at the end of the file,   */
/*  then extend the file.						    */
            else if ( stk[ entno ].spare + ( *bloc ) + size > hcb->eof )
            {
               newsize = hcb->eof + extra - stk[ entno ].spare;
	       rec1_extend_file( slot, newsize, &actsize );
	       if ( _ok ( hds_gl_status ) )
	       {
	          hcb->eof = actsize;
	          stk[ entno ].spare = actsize - newsize;
	          stk[ entno ].bloc = *bloc + size + extra;
	       }
	       done = 1;
            }
         }

/* If the free space stack is full, or there is insufficient space after    */
/* the current end of the frame, then allocate a new frame of the	    */
/* appropriate size.							    */
	 if ( !done )
	 {
            oldbloc = *bloc;
            rec1_alloc_frame( slot, size + extra, &newbloc );
	    if ( _ok( hds_gl_status ) )
	    {

/* Map both old and new frames.						    */
               rec1_map_frame( slot, oldbloc, size * REC__SZBLK, 0, 'R',
	                       &oldpdb );
               rec1_map_frame( slot, newbloc, size * REC__SZBLK, 0, 'W',
			       &newpdb );

/* If both frames cannot be concurrently mapped, then de-allocate the new   */
/* frame.								    */
               if ( !_ok( hds_gl_status ) )
               {
	          rec1_unmap_frame( slot, oldbloc, size * REC__SZBLK, 0, 'R',
			            &oldpdb );
	          rec1_unmap_frame( slot, newbloc, size * REC__SZBLK, 0, 'W',
			            &newpdb );
                  rec1_deall_frame( slot, size, newbloc );
               }

/* Otherwise, copy the existing frame data, de-allocate the old frame and   */
/* modify the start-of-frame block number.				    */
               else
               {
                  _chmove( size * REC__SZBLK, oldpdb, newpdb );
	          rec1_unmap_frame( slot, oldbloc, size * REC__SZBLK, 0, 'R',
			            &oldpdb );
	          rec1_unmap_frame( slot, newbloc, size * REC__SZBLK, 0, 'W',
			            &newpdb );
	          rec1_deall_frame( slot, size, oldbloc );
	          *bloc = newbloc;
	       }
	    }
         }
      }

/* Return the current global status value.				    */
      return hds_gl_status;
   }
