#if HAVE_CONFIG_H
#  include <config.h>
#endif

#include "hds1.h"                /* Global definitions for HDS              */
#include "rec.h"                 /* Public rec_ definitions                 */
#include "dat1.h"                /* Internal dat_ definitions               */
#include "dat_err.h"             /* DAT__ error code definitions            */

   int dat1_unpack_crv( const unsigned char *pcrv, int i, struct RID *rid )
   {
/*+                                                                         */
/* Name:                                                                    */
/*    dat1_unpack_crv                                                       */

/* Purpose:                                                                 */
/*    Unpack record ID information from a Component Record Vector element.  */

/* Invocation:                                                              */
/*    dat1_unpack_crv( pcrv, i, rid )                                     */
/*                                                                          */
/* Description:                                                             */
/*    This function unpacks the record ID information from an element of a  */
/*    Component Record Vector, putting it into a RID structure which then   */
/*    identifies the Component's Object Record.  This is done so that the   */
/*    Component Record Vector format need not depend on the details of the  */
/*    way that a RID structure is stored in memory.                         */

/* Parameters:                                                              */
/*    const unsigned char *pcrv                                             */
/*       Pointer to the start of a packed Component Record Vector.          */
/*    int i                                                                 */
/*       Number of the Component Record Vector element to unpack            */
/*       (zero-based).                                                      */
/*    struct RID *rid                                                       */
/*       Pointer to a RID structure to receive the unpacked information.    */

/* Returned Value:                                                          */
/*    int dat1_unpack_crv                                                   */
/*       The global status value current on exit.                           */

/* Authors:                                                                 */
/*    RFWS: R.F. Warren-Smith (STARLINK)                                    */
/*    BKM:  B.K. McIlwrath    (STARLINK)                                    */
/*    {@enter_new_authors_here@}                                            */

/* History:                                                                 */
/*    17-APR-1991 (RFWS):                                                   */
/*       Original version.                                                  */
/*    5-JUN-1991 (RFWS):                                                    */
/*       Fixed bug in unpacking of chip field.                              */
/*    19-APR-2004 (BKM):                                                    */
/*       Revised for 64-bit HDS files.                                      */
/*    {@enter_further_changes_here@}                                        */

/* Bugs:                                                                    */
/*    {@note_any_bugs_here@}                                                */

/*-                                                                         */

/* Local Variables:                                                         */
      const unsigned char *element; /* Pointer to start of required element */
      INT_BIG temp;                 /* Temporary variable                   */
      int shift;                    /* Bit-shift count                      */
      int j;                        /* Loop counter                         */

/*.                                                                         */

/* Check the inherited global status.                                       */
      if ( !_ok( hds_gl_status ) ) return hds_gl_status;

/* Obtain a pointer to the start of the required Component Record Vector    */
/* element.                                                                 */
      element = pcrv + ( i * SZCRV );

      if( !hds_gl_64bit )    /* 32-bit HDS files (V3)                       */
      {
/* The first 15 chars in the element contain the component name, which is   */
/* not required. The following char is not used.  Unpack the RID bloc field */
/* from the next 20 bits, and the RID chip field from the following 4 bits. */
/* The final char is also not used.                                         */
         rid->bloc = ( ( ( ( element[ 18 ] & 0xf ) << 8 ) |
                             element[ 17 ] ) << 8 ) |
                             element[ 16 ];
         rid->chip =  ( element[ 18 ] >> 4 ) & 0xf;
      }
      else   /* 64-bit HDS files (V4)                                       */
      {
/* The first 15 chars in the element contain the component name, which is   */
/* not required. The following char is not used.  Unpack the RID bloc field */
/* from the next 56 bits, and the RID chip field from the following 4 bits. */
         shift = 48;
         rid->bloc = 0;
         for( j = 22; j >15; j-- )
         {
            temp = element[ j ] << shift;
            rid->bloc |= temp;
            shift -= 8;
         }
         rid->chip =  element[ 23 ];
      }

/* Return the current global status value.                                  */
      return hds_gl_status;
   }
