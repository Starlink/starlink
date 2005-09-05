#if HAVE_CONFIG_H
#  include <config.h>
#endif

#include "ems.h"                 /* EMS error reporting routines            */
#include "ems_par.h"             /* EMS__ public constants                  */
#include "hds1.h"                /* Global definitions for HDS              */
#include "rec.h"                 /* Public rec_ definitions                 */
#include "rec1.h"                /* Private rec_ definitions                */
#include "dat_err.h"             /* DAT__ error code definitions            */

   void rec_end_wild( struct WLD **context )
   {
/*+                                                                         */
/* Name:                                                                    */
/*    rec_end_wild                                                          */

/* Purpose:                                                                 */
/*    End a wild-card file search begun with rec_wild_file.                 */

/* Invocation:                                                              */
/*    rec_end_wild( context )                                               */

/* Description:                                                             */
/*    This routine ends a wild-card file search begun by rec_wild_file and  */
/*    deallocates the associated context structure and its contents.        */

/* Parameters:                                                              */
/*    struct WLD **context                                                  */
/*       Pointer to a pointer identifying the wild-card search context      */
/*       which is to be ended. A null context pointer will be returned in   */
/*       *context.                                                          */

/* Returned Value:                                                          */
/*    void                                                                  */

/* Notes:                                                                   */
/*    This routine attempts to execute even if the global status is set on  */
/*    entry, but no further error report will be made if it subsequently    */
/*    fails under these circumstances.                                      */

/* Copyright:                                                               */
/*    Copyright (C) 1992 Science & Engineering Research Council             */

/* Authors:                                                                 */
/*    RFWS: R.F. Warren-Smith (STARLINK, RAL)                               */
/*    {@enter_new_authors_here@}                                            */

/* History:                                                                 */
/*    26-OCT-1992 (RFWS):                                                   */
/*       Original version.                                                  */
/*    {@enter_changes_here@}                                                */

/*-                                                                         */

/* Local Variables:                                                         */
      INT i;                     /* Loop counter for file names             */
      INT nc;                    /* Number of characters in file name       */
      char *mem;                 /* Pointer to allocated memory             */
      int again;                 /* Loop again?                             */
      int valid;                 /* Wild-card context ID valid?             */
      struct WLD *next;          /* Next wild-card context in queue         */
      struct WLD *qpntr;         /* Pointer into wild-card context queue    */

/*.                                                                         */

/* Begin a new error reporting environment.                                 */
      ems_begin_c( &hds_gl_status );

/* Loop to validate the wild-card search context by comparing it with each  */
/* context currently on the wild-card context queue. Omit this stage if the */
/* context pointer is null, as this is never valid.                         */
      valid = 0;
      if ( *context != NULL )
      {
         again = 1;
         for( qpntr = rec_gl_wldque; again; qpntr = next )
         {

/* Quit searching if a match is found.                                      */
            if ( qpntr == *context )
            {
               valid = 1;
               break;
            }

/* Otherwise return to test against the next queue element, so long as we   */
/* have not yet returned to the head of the queue.                          */
            next = qpntr->flink;
            again = ( next != rec_gl_wldque );
         }
      }

/* If no match was found, then report an error.                             */
      if ( !valid )
      {
         hds_gl_status = DAT__WLDIN;
         ems_seti_c( "IWLD", (INT) *context );
         ems_rep_c( "REC_END_WILD_1",
                    "Wild-card search context identifier is invalid; value \
is ^IWLD (possible programming error).",
                    &hds_gl_status );
      }

/* If the context is valid, then remove it from the queue.                  */
      else
      {
         _remque( *context, rec_gl_wldque )

/* Loop to deallocate the memory associated with each file name stored      */
/* within the context.                                                      */
         for ( i = 0; i < (*context)->nfile; i++ )
         {
            mem = (*context)->list[ i ].name;
            nc = (*context)->list[ i ].len;
            rec_deall_mem( nc + 1, (void **) &mem );
         }

/* Deallocate the memory used for storing the list of file name structures. */
         rec_deall_mem( (*context)->mxlist * sizeof( struct WLD_FILE ),
                        (void **) &(*context)->list );

/* Deallocate the context structure itself.                                 */
         rec_deall_mem( sizeof( struct WLD ), (void **) context );
      }

/* Return a null context pointer.                                           */
      *context = NULL;

/* End the error reporting environment and exit the routine.                */
      ems_end_c( &hds_gl_status );
      return;
   }
