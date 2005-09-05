#if HAVE_CONFIG_H
#  include <config.h>
#endif

#include <stddef.h>
#include "hds1.h"                /* Global definitions for HDS              */
#include "rec.h"                 /* Public rec_ definitions                 */
#include "rec1.h"                /* Internal rec_ definitions               */
#include "dat1.h"                /* Internal dat_ definitions               */
#include "dat_err.h"             /* DAT__ error code definitions            */

   int dat1_put_odl( const struct HAN *han, struct ODL *odl )
   {
/*+                                                                         */
/* Name:                                                                    */
/*    rec1_put_odl                                                          */

/* Purpose:                                                                 */
/*    Write Object Descriptor Label information to an Object Record.        */

/* Invocation:                                                              */
/*    rec1_put_odl( han, odl )                                              */
/*                                                                          */
/* Description:                                                             */
/*    This function writes the Object Descriptor Label information to the   */
/*    Static Domain of an Object Record.                                    */

/* Parameters:                                                              */
/*    const struct HAN *han                                                 */
/*       Pointer to a HAN structure containing a handle for the Object      */
/*       Record.                                                            */
/*    struct ODL *odl                                                       */
/*       Pointer to an ODL structure containing the information to be       */
/*       written.                                                           */

/* Returned Value:                                                          */
/*    int dat1_put_odl                                                      */
/*       The global status value current on exit.                           */

/* Authors:                                                                 */
/*    RFWS: R.F. Warren-Smith (STARLINK)                                    */
/*    BKM:  B.K. McIlwrath    (STARLINK)                                    */
/*    {@enter_new_authors_here@}                                            */

/* History:                                                                 */
/*    11-APR-1991 (RFWS):                                                   */
/*       Original version.                                                  */
/*    14-JUNE-1991 (RFWS):                                                  */
/*       Changed access mode to 'U' in rec_locate_block.                    */
/*    19-APR-2004 (BKM):                                                    */
/*       Revise for 64-bit HDS files.                                       */
/*    {@enter_further_changes_here@}                                        */

/* Bugs:                                                                    */
/*    {@note_any_bugs_here@}                                                */

/*-                                                                         */

/* Local Variables:                                                         */
      unsigned char *lrb;        /* Pointer to logical Record Block         */
      unsigned char *sdom;       /* Pointer to Static Domain                */
/*.                                                                         */

/* Check the inherited global status.                                       */
      if ( !_ok( hds_gl_status ) ) return hds_gl_status;

/* Locate the Logical Record Block which contains the record.               */
      rec_locate_block( han->slot, han->rid.bloc, 'U', &lrb );

/* Obtain a pointer to the Static Domain.                                   */
      sdom = lrb + REC__SZCBM + ( han->rid.chip * REC__SZCHIP ) + SZRCL;

/* Pack the Object Descriptor Label.                                        */
      dat1_pack_odl( odl, sdom );

/* Release the Logical Record Block.                                        */
      if ( lrb != NULL ) rec_release_block( han->slot, han->rid.bloc );

/* Return the current global status value.                                  */
      return hds_gl_status;
   }
