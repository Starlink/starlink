#if HAVE_CONFIG_H
#  include <config.h>
#endif

#include "dat_err.h"             /* DAT__ error code definitions            */

#include "hds1.h"                /* Global definitions for HDS              */
#include "rec1.h"

   int rec1_pack_ncomp( int ncomp, unsigned char pncomp[ 4 ] )
   {
/*+                                                                         */
/* Name:                                                                    */
/*    rec1_pack_ncomp                                                       */

/* Purpose:                                                                 */
/*    Pack the number of components information for a Component Record.     */

/* Invocation:                                                              */
/*    rec1_pack_ncomp( ncomp, pncomp )                                      */
/*                                                                          */
/* Description:                                                             */
/*    This function packs the "number of components" information stored in  */
/*    the Static Domain of a Structure Record. This is done so that the     */
/*    Static Domain format need not depend on the details of the way that   */
/*    this information is stored in memory.                                 */

/* Parameters:                                                              */
/*    int ncomp                                                             */
/*       Number of structure components (information to be packed).         */
/*    unsigned char pncomp[ 4 ]                                             */
/*       Pointer to an array of 4 Static Domain bytes into which the        */
/*       information is to be packed.                                       */

/* Returned Value:                                                          */
/*    int rec1_pack_ncomp                                                   */
/*       The global status value current on exit.                           */

/* Authors:                                                                 */
/*    RFWS: R.F. Warren-Smith (STARLINK)                                    */
/*    {@enter_new_authors_here@}                                            */

/* History:                                                                 */
/*    10-APR-1991 (RFWS):                                                   */
/*       Original version.                                                  */
/*    {@enter_changes_here@}                                                */

/* Bugs:                                                                    */
/*    {@note_any_bugs_here@}                                                */

/*-                                                                         */

/*.                                                                         */

/* Check the inherited global status.                                       */
      if ( !_ok( hds_gl_status ) ) return hds_gl_status;

/* Pack the number of components into the 4 bytes of the Static Domain.     */
      pncomp[ 0 ] = ncomp & 0xff;
      pncomp[ 1 ] = ( ncomp >> 8 ) & 0xff;
      pncomp[ 2 ] = ( ncomp >> 16 ) & 0xff;
      pncomp[ 3 ] = ( ncomp >> 24 ) & 0xff;

/* Return the current global status value.                                  */
      return hds_gl_status;
   }
