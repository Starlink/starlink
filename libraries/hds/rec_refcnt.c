#if HAVE_CONFIG_H
#  include <config.h>
#endif

#include "hds1.h"                /* Global definitions for HDS              */
#include "rec.h"                 /* Public rec_ definitions                 */
#include "rec1.h"                /* Internal rec_ definitions               */
#include "dat_err.h"             /* DAT__ error code definitions            */

   void rec_refcnt( const struct HAN *han, int inc, int *refcnt, int *status )
   {
/*+                                                                         */
/* Name:                                                                    */
/*    rec_refcnt                                                            */

/* Purpose:                                                                 */
/*    Increment and/or obtain a reference count for a container file.       */

/* Invocation:                                                              */
/*    rec_refcnt( han, inc, refcnt, status )                                */

/* Description:                                                             */
/*    This function increments the count of the number of current           */
/*    references to a container file and returns the new result.            */

/* Parameters:                                                              */
/*    const struct HAN *han                                                 */
/*       Pointer to a HAN structure containing a handle for any record in   */
/*       the container file.                                                */
/*    int inc                                                               */
/*       The increment to be applied (may be positive or negative).         */
/*    int *refcnt                                                           */
/*       Pointer to an integer to receive the new reference count value.    */
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
/*    2-OCT-1991 (RFWS):                                                    */
/*       Original version.                                                  */
/*    3-OCT-1991 (RFWS):                                                    */
/*       Pass global status as a parameter.                                 */
/*    11-SEP-1992 (RFWS)                                                    */
/*       Added the inc argument.                                            */
/*    {@enter_further_changes_here@}                                        */

/* Bugs:                                                                    */
/*    {@note_any_bugs_here@}                                                */

/*-                                                                         */

/*.                                                                         */

/* Check the inherited global status.                                       */
      if ( !_ok( *status ) ) return;

/* Increment and return the reference count.                                */
      *refcnt = ( rec_ga_fcv[ han->slot ].count += inc );
      return;
   }
