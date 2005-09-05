#if HAVE_CONFIG_H
#  include <config.h>
#endif

#include "hds1.h"                /* Global definitions for HDS              */
#include "rec.h"                 /* Public rec_ definitions                 */
#include "dat1.h"                /* Internal dat_ definitions               */
#include "dat_err.h"             /* DAT__ error code definitions            */

   int dat1_pack_crv( const struct RID *rid, int i, unsigned char *pcrv )
   {
/*+                                                                         */
/* Name:                                                                    */
/*    dat1_pack_crv                                                         */

/* Purpose:                                                                 */
/*    Pack record ID information into a Component Record Vector element.    */

/* Invocation:                                                              */
/*    dat1_pack_crv( rid, extended, i, pcrv )                                       */

/* Description:                                                             */
/*    This function packs record ID information from a RID structrure which */
/*    identifies a component's Object Record into an element of a Component */
/*    Record Vector.  This is done so that the Component Record Vector      */
/*    format need not depend on the details of the way that a RID structure */
/*    is stored in memory.                                                  */

/* Parameters:                                                              */
/*    const struct RID *rid                                                 */
/*       Pointer to a RID structure containing the information to be        */
/*       packed.                                                            */
/*    int i                                                                 */
/*       Number of the Component Record Vector element into which the       */
/*       information is to be packed (zero-based).                          */
/*    unsigned char *pcrv                                                   */
/*       Pointer to the start of a Component Record Vector into which the   */
/*       information is to be packed.                                       */

/* Returned Value:                                                          */
/*    int dat1_pack_crv                                                     */
/*       The global status value current on exit.                           */

/* Authors:                                                                 */
/*    RFWS: R.F. Warren-Smith (STARLINK)                                    */
/*    BKM:  B.K. McIlwrath    (STARLINK)                                    */
/*    {@enter_new_authors_here@}                                            */

/* History:                                                                 */
/*    17-APR-1991 (RFWS):                                                   */
/*       Original version.                                                  */
/*    19-APR-2004 (BKM):                                                    */
/*       Revised for 64-bit HDS files.                                      */
/*    {@enter_changes_here@}                                                */

/* Bugs:                                                                    */
/*    {@note_any_bugs_here@}                                                */

/*-                                                                         */

/* Local Variables:                                                         */
      unsigned char *element;    /* Pointer to the required CRV element     */

/*.                                                                         */

/* Check the inherited global status.                                       */
      if ( !_ok( hds_gl_status ) ) return hds_gl_status;

/* Obtain a pointer to the required Component Record Vector element.        */
      element = pcrv + ( i * SZCRV );

/* The first 15 bytes hold the component name, which this routine does not  */
/* touch. The following byte is unused, so set it to zero.                  */
      element[ 15 ] = 0;

      if ( !hds_gl_64bit )
      {

/* Pack the RID .bloc field into the next 20 bits, and the RID .chip field  */
/* into the following 4 bits.                                               */
         element[ 16 ] = rid->bloc & 0xff;
         element[ 17 ] = ( rid->bloc >> 8 ) & 0xff;
         element[ 18 ] = ( ( rid->bloc >> 16 ) & 0xf ) |
                         ( rid->chip << 4 );

/* The following byte is not used, so set it to zero.                       */
         element[ 19 ] = 0;
      }
      else
      {

/* Pack the RID .bloc field into the next 56 bits, and the RID .chip field  */
/* into the following 4 bits.                                               */
         element[ 16 ] = rid->bloc & 0xff;
         element[ 17 ] = ( rid->bloc >> 8 ) & 0xff;
         element[ 18 ] = ( rid->bloc >> 16 ) & 0xff;
         element[ 19 ] = ( rid->bloc >> 24 ) & 0xff;
         element[ 20 ] = ( rid->bloc >> 32 ) & 0xff;
         element[ 21 ] = ( rid->bloc >> 40 ) & 0xff;
         element[ 22 ] = ( rid->bloc >> 48 ) & 0xff;
         element[ 23 ] = rid->chip;
      }

/* Return the current global status value.                                  */
      return hds_gl_status;
   }
