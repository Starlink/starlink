#if HAVE_CONFIG_H
#  include <config.h>
#endif

#include "hds1.h"                /* Global definitions for HDS              */
#include "rec.h"                 /* Public rec_ definitions                 */
#include "rec1.h"                /* Internal rec_ definitions               */
#include "dat_err.h"             /* DAT__ error code definitions            */

   void rec_mark_delete( const struct HAN *han, int *status )
   {
/*+                                                                         */
/* Name:                                                                    */
/*    rec_mark_delete                                                       */

/* Purpose:                                                                 */
/*    Mark an HDS container file for deletion.                              */

/* Invocation:                                                              */
/*    rec_mark_delete( han, status )                                        */

/* Description:                                                             */
/*    This function marks an HDS container file so that it will be deleted  */
/*    when it is closed (but note that this function does not itslf close   */
/*    the file). A single invocation is sufficient to ensure that the file  */
/*    will eventually be deleted; subsequent invocations have no further    */
/*    effect.                                                               */

/* Parameters:                                                              */
/*    const struct HAN *han                                                 */
/*       Pointer to a HAN structure containing a handle to any record in    */
/*       the file.                                                          */
/*    int *status                                                           */
/*       Pointer to the inherited global status.                            */

/* Returned Value:                                                          */
/*    void                                                                  */

/* Copyright:                                                               */
/*    Copyright (C) 1992 Science & Engineering Research Council             */

/* Authors:                                                                 */
/*    RFWS: R.F. Warren-Smith (STARLINK)                                    */
/*    {@enter_new_authors_here@}                                            */

/* History:                                                                 */
/*    23-SEP-1992 (RFWS):                                                   */
/*       Original version.                                                  */
/*    {@enter_further_changes_here@}                                        */

/* Bugs:                                                                    */
/*    {@note_any_bugs_here@}                                                */

/*-                                                                         */

/*.                                                                         */

/* Check the inherited global status.                                       */
      if ( !_ok( *status ) ) return;

/* Set the deletion flag in the file control vector.                        */
      rec_ga_fcv[ han->slot ].dele = 1;

/* Exit the routine.                                                        */
      return;
   }
