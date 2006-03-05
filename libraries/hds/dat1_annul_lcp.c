#if HAVE_CONFIG_H
#  include <config.h>
#endif

#include "ems.h"                 /* ems_ error message service routines     */
#include "hds1.h"                /* Global definitions for HDS              */
#include "rec1.h"                /* Internal rec_ definitions               */
#include "dat1.h"                /* Internal dat_ definitions               */
#include "dat_err.h"             /* DAT__ error code definitions            */

   void dat1_annul_lcp( struct LCP **lcp )
   {
/*+                                                                         */
/* Name:                                                                    */
/*    dat1_annul_lcp                                                        */

/* Purpose:                                                                 */
/*    Annul a Locator Control Packet.                                       */

/* Invocation:                                                              */
/*    dat1_annul_lcp( lcp )                                                 */

/* Description:                                                             */
/*    This function annuls a Locator Control Packet (LCP), causing the      */
/*    associated locator to become invalid. Any mapped data associated with */
/*    the LCP are flushed and the LCP is returned to the free locator       */
/*    queue. A null pointer is returned. If a primary LCP is supplied, the  */
/*    reference count for the associated container file is also             */
/*    decremented. If this count falls to zero, all other LCPs associated   */
/*    with the same file are also annulled, and the file is closed (and     */
/*    deleted if it was been marked for deletion).                          */

/* Parameters:                                                              */
/*    struct LCP **lcp                                                      */
/*       Pointer to a pointer to the LCP structure to be annulled; a null   */
/*       pointer value will be returned in *lcp. An error will result if    */
/*       *lcp is null on entry.                                             */

/* Returned Value:                                                          */
/*    void                                                                  */

/* Notes:                                                                   */
/*    This routine attempts to execute even if the HDS global ststus is set */
/*    on entry, although no further error report will be made if it         */
/*    subsequently fails under these circumstances.                         */

/* Copyright:                                                               */
/*    Copyright (C) 1992 Science & Engineering Research Council             */

/* Authors:                                                                 */
/*    RFWS: R.F. Warren-Smith (STARLINK)                                    */
/*    {@enter_new_authors_here@}                                            */

/* History:                                                                 */
/*    11-SEP-1992 (RFWS):                                                   */
/*       Original version.                                                  */
/*    {@enter_changes_here@}                                                */

/* Bugs:                                                                    */
/*    {@note_any_bugs_here@}                                                */

/*-                                                                         */

/* Local Variables:                                                         */
      int again;                 /* Process another queue element?          */
      int primary;               /* Primary Locator Control Packet?         */
      int refcnt;                /* Container file reference count          */
      struct HAN han;            /* Handle to container file record         */
      struct LCP *next;          /* Pointer to next queue element           */
      struct LCP *qpntr;         /* Pointer to current queue element        */

/*.                                                                         */

/* Begin a new error reporting context.                                     */
      emsBegin( &hds_gl_status );

/* Check that the LCP pointer supplied is not null. Report an error if it   */
/* is.                                                                      */
      if ( *lcp == NULL )
      {
         hds_gl_status = DAT__FATAL;
         emsRep( "DAT1_ANNUL_LCP_1",
                    "Routine DAT1_ANNUL_LCP called with an invalid null LCP \
pointer (internal programming error).",
                    &hds_gl_status );
      }

/* If the LCP is valid, then determine if it is a primary LCP.              */
      else
      {
         primary = (*lcp)->data.valid ? (*lcp)->primary : 0;

/* If it is not primary, then simply defuse it.                             */
         if ( !primary )
         {
            dau_defuse_lcp( lcp );
         }

/* Otherwise, obtain a handle to the associated container file record and   */
/* decrement the file's reference count.                                    */
         else
         {
            han = (*lcp)->data.han;
            rec_refcnt( &han, -1, &refcnt, &hds_gl_status );
            if ( _ok( hds_gl_status ) )
            {

/* If the reference count is still positive, then simply defuse the LCP.    */
               if ( refcnt > 0 )
               {
                  dau_defuse_lcp( lcp );
               }

/* Otherwise, obtain a pointer to the head of the Working Locator Queue and */
/* loop to identify all Locator Control Packets associated with this        */
/* container file.                                                          */
               else
               {
                  again = 1;
                  for( qpntr = dat_ga_wlq; again; qpntr = next )
                  {

/* Obtain a pointer to the next element in the queue (while the current one */
/* still exists) and note whether we have reached the end of the queue.     */
                     next = qpntr->flink;
                     again = ( next != dat_ga_wlq );

/* Defuse all LCPs associated with the container file in question (causing  */
/* the associated locators to become invalid).                              */
                     if ( rec_same_file( &han, &qpntr->data.han ) )
                     {
                        dau_defuse_lcp( &qpntr );
                     }
                  }

/* After defusing all LCPs associated with the container file, close the    */
/* file.                                                                    */
                  rec_close_file( &han );
               }
            }      
         }
      }

/* Return a null LCP pointer.                                               */
      *lcp = NULL;

/* End the error reporting context and exit the routine.                    */
      emsEnd( &hds_gl_status );
      return;
   }
