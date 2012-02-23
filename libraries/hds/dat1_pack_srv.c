#if HAVE_CONFIG_H
#  include <config.h>
#endif

#include "hds1.h"                /* Global definitions for HDS              */
#include "rec.h"                 /* Public rec_ definitions                 */
#include "dat_err.h"             /* DAT__ error code definitions            */
#include "dat1.h"

   int dat1_pack_srv( const struct RID *rid, unsigned char psrv[] )
   {
/*+                                                                         */
/* Name:                                                                    */
/*    dat1_pack_srv                                                         */

/* Purpose:                                                                 */
/*    Pack a Structure Record Vector element.                               */

/* Invocation:                                                              */
/*    dat1_pack_srv( rid, psrv )                                            */
/*                                                                          */
/* Description:                                                             */
/*    This function packs information from a RID structrure (which          */
/*    identifies a structure element's Component Record) into an element of */
/*    a Structure Record Vector.  This is done so that the Structure Record */
/*    Vector format need not depend on the details of the way that a RID    */
/*    structure is stored in memory.                                        */

/* Parameters:                                                              */
/*    const struct RID *rid                                                 */
/*       Pointer to a RID structure containing the information to be        */
/*       packed.                                                            */
/*    unsigned char psrv[ ]                                                 */
/*       Pointer to an array of 4 or 8 (depending in the type of Structure  */
/*       Record Vecto) bytes into which the information is to be packed.    */

/* Returned Value:                                                          */
/*    int dat1_pack_srv                                                     */
/*       The global status value current on exit.                           */

/* Authors:                                                                 */
/*    RFWS: R.F. Warren-Smith (STARLINK)                                    */
/*    BKM:  B.K. McIlwrath    (STARLINK)                                    */
/*    DSB:  David S Berry     (JAC, UCLan)                                  */
/*    {@enter_new_authors_here@}                                            */

/* History:                                                                 */
/*    16-APR-1991 (RFWS):                                                   */
/*       Original version.                                                  */
/*    19-AUG-2004 (BKM):                                                    */
/*       Revise for 64-bit HDS files.                                       */
/*    19-JUN-2008 (DSB):                                                    */
/*       64-bit algorithm made more efficient.                              */
/*    {@enter_changes_here@}                                                */

/* Bugs:                                                                    */
/*    {@note_any_bugs_here@}                                                */

/*-                                                                         */

/*.                                                                         */

/* Local Variables : */
   INT_BIG temp;

/* Check the inherited global status.                                       */
      if ( !_ok( hds_gl_status ) ) return hds_gl_status;

      if ( ! hds_gl_64bit )
      {
/* Pack the RID .bloc field into the first 20 bits, and the RID .chip field */
/* into the following 4 bits.                                               */
         psrv[ 0 ] = rid->bloc & 0xff;
         psrv[ 1 ] = ( rid->bloc >> 8 ) & 0xff;
         psrv[ 2 ] = ( ( rid->bloc >> 16 ) & 0xf ) |
                     ( rid->chip << 4 );

/* The following byte is not used, so set it to zero.                       */
         psrv[ 3 ] = 0;
      }
      else
      {
         temp = rid->bloc;
         psrv[ 0 ] = temp & 0xff;
         psrv[ 1 ] = ( temp >>= 8 ) & 0xff;
         psrv[ 2 ] = ( temp >>= 8 ) & 0xff;
         psrv[ 3 ] = ( temp >>= 8 ) & 0xff;
         psrv[ 4 ] = ( temp >>= 8 ) & 0xff;
         psrv[ 5 ] = ( temp >>= 8 ) & 0xff;
         psrv[ 6 ] = ( temp >>= 8 ) & 0xff;
         psrv[ 7 ] = rid->chip;
      }

/* Return the current global status value.                                  */
      return hds_gl_status;
   }
