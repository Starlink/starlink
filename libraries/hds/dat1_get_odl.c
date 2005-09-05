#if HAVE_CONFIG_H
#  include <config.h>
#endif

#include <stddef.h>
#include "hds1.h"                /* Global definitions for HDS              */
#include "rec.h"                 /* Public rec_ definitions                 */
#include "rec1.h"                /* Internal rec_ definitions               */
#include "dat1.h"                /* Internal dat_ definitions               */
#include "dat_err.h"             /* DAT__ error code definitions            */

   int dat1_get_odl( const struct HAN *han, struct ODL *odl )
   {
/*+                                                                         */
/* Name:                                                                    */
/*    rec1_get_odl                                                          */

/* Purpose:                                                                 */
/*    Read Object Descriptor Label information from an Object Record.       */

/* Invocation:                                                              */
/*    rec1_get_odl( han, odl )                                              */
/*                                                                          */
/* Description:                                                             */
/*    This function reads the Object Descriptor Label information from the  */
/*    Static Domain of an Object Record, returning it in an ODL structure.  */

/* Parameters:                                                              */
/*    const struct HAN *han                                                 */
/*       Pointer to a HAN structure containing a handle for the Object      */
/*       Record.                                                            */
/*    struct ODL *odl                                                       */
/*       Pointer to an ODL structure to receive the information.            */

/* Returned Value:                                                          */
/*    int dat1_get_odl                                                      */
/*       The global status value current on exit.                           */

/* Authors:                                                                 */
/*    RFWS: R.F. Warren-Smith (STARLINK)                                    */
/*    BKM:  B.K. McIlwrath    (STARLINK)                                    */
/*    {@enter_new_authors_here@}                                            */

/* History:                                                                 */
/*    11-APR-1991 (RFWS):                                                   */
/*       Original version.                                                  */
/*    19-APR-2004 (BKM):                                                    */
/*       Revise for 64-bit HDS files.                                       */
/*    {@enter_changes_here@}                                                */

/* Bugs:                                                                    */
/*    {@note_any_bugs_here@}                                                */

/*-                                                                         */

/* Local Variables:                                                         */
      unsigned char *lrb;        /* Pointer to Logical Record Block         */
      unsigned char *sdom;       /* Pointer to Static Domain                */
/*.                                                                         */

/* Check the inherited global status.                                       */
      if ( !_ok( hds_gl_status ) ) return hds_gl_status;

/* Locate the Logical Record Block which contains the record.               */
      rec_locate_block( han->slot, han->rid.bloc, 'R', &lrb );

/* Obtain a pointer to the Static Domain.                                   */
      sdom = lrb + REC__SZCBM + ( han->rid.chip * REC__SZCHIP ) + SZRCL;

/* Unpack the Object Descriptor Label.                                      */
      dat1_unpack_odl( sdom, odl );

/* Release the Logical Record Block.                                        */
      if ( lrb != NULL ) rec_release_block( han->slot, han->rid.bloc );

/* Return the current global status value.                                  */
      return hds_gl_status;
   }
