#include "hds1_feature.h"	 /* Define feature-test macros, etc.	    */
#include "ems.h"		 /* EMS error reporting routines	    */
#include "hds1.h"		 /* Global definitions for HDS		    */
#include "rec1.h"		 /* Internal rec_ definitions		    */
#include "dat_err.h"		 /* DAT__ error code definitions	    */

   int rec1_deall_frame( int slot, int size, int bloc )
   {
/*+									    */
/* Name:								    */
/*    rec1_deall_frame							    */

/* Purpose:								    */
/*    Deallocate a frame of blocks in a container file.			    */

/* Invocation:								    */
/*    rec1_deall_frame( slot, size, bloc )				    */

/* Description:								    */
/*    This function deallocates a sequence of blocks in a container file    */
/*    which have previously been allocated using rec1_alloc_frame, and	    */
/*    updates the free space stack to indicate that the blocks are	    */
/*    available for re-use.						    */

/* Parameters:								    */
/*    int slot								    */
/*	 Slot number of the container file in the File Control Vector.	    */
/*    int size								    */
/*	 Number of blocks to be deallocated.				    */
/*    int bloc								    */
/*	 Block number of the first block to be deallocated (the first block */
/*	 in the file is no. 1).						    */

/* Returned Value:							    */
/*    int rec1_deall_frame						    */
/*	 The global status value current on exit.			    */

/* Notes:								    */
/*    This function will attempt to execute even if the HDS global status   */
/*    is set on entry, although no further error report will be made if it  */
/*    subsequently fails under these circumstances.			    */

/* Authors:								    */
/*    RFWS: R.F. Warren-Smith (STARLINK, RAL)				    */
/*    {@enter_new_authors_here@}					    */

/* History:								    */
/*    25-MAR-1991 (RFWS):						    */
/*	 Added prologue and tidied code. Added error reporting and calls to */
/*	 ems_begin and ems_end. Made the function attempt to execute under  */
/*	 error conditions.						    */
/*    5-APR-1991 (RFWS):						    */
/*       Removed the _invoke macro, which subverts the ems_ nesting.	    */
/*    {@enter_further_changes_here@}					    */

/* Bugs:								    */
/*    {@note_any_bugs_here@}						    */

/*-									    */

/* Local Variables:							    */
      int empty;		 /* Empty free space stack entry no.	    */
      int entno;		 /* Free space stack entry number	    */
      int full;			 /* Free space stack full?		    */
      int i;			 /* Loop counter for free space stack	    */
      struct HCB *hcb;		 /* Pointer to Header Control Block	    */
      struct STK *stk;		 /* Pointer to free space stack		    */

/*.									    */

/* Begin a new error reporting context.					    */
      ems_begin_c( &hds_gl_status );

/* Report an error if the frame size is invalid.			    */
      if ( size <= 0 )
      {
         hds_gl_status = DAT__FATAL;
         ems_seti_c( "SIZE", size );
	 ems_rep_c( "REC1_DEALL_FRAME_1",
	            "Routine rec1_deall_frame called with an invalid size \
argument of ^SIZE (internal programming error).",
                    &hds_gl_status );
      }

/* Report an error if the block count is invalid.			    */
      else if ( bloc <= 0 )
      {
         hds_gl_status = DAT__FATAL;
         ems_seti_c( "BLOC", bloc );
	 ems_rep_c( "REC1_DEALL_FRAME_2",
	            "Routine rec1_deall_frame called with an invalid bloc \
argument of ^BLOC (internal programming error).",
                    &hds_gl_status );
      }

/* Otherwise, locate the Header Control Block and its free space stack.	    */
      else
      {
         rec1_locate_hcb( slot, 'U', &hcb );
         if ( _ok( hds_gl_status ) )
         {
            stk = hcb->stk;

/* Search the free space stack to determine if an adjacent frame is also    */
/* free. If not, then allocate the highest empty stack element (if	    */
/* available).								    */
            empty = 0;
            for ( i = REC__MXSTK - 1; ; i-- )
            {
	       if ( stk[ i ].bloc == -1 )
	       {
	          entno = _max( i, empty );
	          break;
	       }               
	       else if ( stk[ i ].spare == 0 )
	       {
	          empty = i;
               }
	       else if ( stk[ i ].bloc == bloc + size )
	       {
	          size = stk[ i ].spare + size;
	          entno = i;
	          break;
	       }
	       else if ( bloc == stk[ i ].bloc + stk[ i ].spare )
	       {
	          bloc = stk[ i ].bloc;
	          size = stk[ i ].spare + size;
	          entno = i;
	          break;
	       }
            }

/* If an entry is available, search the free space stack again to see	    */
/* whether there are now two adjacent frames that can be combined.	    */
            full = ( stk[ entno ].bloc == -1 && stk[ entno - 1 ].bloc != -1 );
            if ( !full )
	    {
	       for ( i = REC__MXSTK - 1; ; i-- )
	       {
	          if ( stk[ i ].bloc == -1 )
	          {
                     break;
                  }
	          else if ( i == entno )
                  {
	             continue;
	          }
	          else if ( stk[ i ].bloc == bloc + size )
	          {
	             size += stk[ i ].spare;
                     stk[ i ].spare = 0;
		     break;
	          }
	          else if ( bloc == stk[ i ].bloc + stk[ i ].spare )
	          {
		     stk[ i ].spare += size;
		     size = 0;
		     break;
	          }
               }

/* Fill the entry.							    */
	       stk[ entno ].bloc = bloc;
	       stk[ entno ].spare = size;
	    }
         }
      }

/* End the error reporting environment and return the current global status */
/* value.								    */
      ems_end_c( &hds_gl_status );
      return hds_gl_status;
   }
