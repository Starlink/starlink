#if HAVE_CONFIG_H
#  include <config.h>
#endif

#include "hds1.h"                /* Global definitions for HDS              */
#include "rec1.h"                /* Internal rec_ definitions               */
#include "dat_err.h"             /* DAT__ error code definitions            */

/* Helper function to pack a stack entry into a byte string.                */
      static int pack_rec(struct STK entry, unsigned char *prec);


   int rec1_pack_hcb( const struct HCB *hcb,
                      unsigned char phcb[ REC__SZBLK ] )
   {
/*+                                                                         */
/* Name:                                                                    */
/*    rec1_pack_hcb                                                         */

/* Purpose:                                                                 */
/*    Pack Header Control Block information.                                */

/* Invocation:                                                              */
/*    rec1_pack_hcb( hcb, phcb )                                            */

/* Description:                                                             */
/*    This function packs the information in an HCB structure and puts it   */
/*    into an array of characters suitable for writing to the Header        */
/*    Control Block of an HDS container file. This is done so that the      */
/*    Header Control Block format need not depend on the details of the way */
/*    that an HCB structure is stored in memory.                            */

/* Parameters:                                                              */
/*    const struct HCB *hcb                                                 */
/*       Pointer to an HCB structure containing the information to be       */
/*       packed.                                                            */
/*    unsigned char phcb[ REC__SZBLK ]                                      */
/*       Pointer to an array of REC__SZBLK Header Control Block characters  */
/*       into which the information is to be packed.                        */

/* Returned Value:                                                          */
/*    int rec1_pack_hcb                                                     */
/*       The global status value current on exit.                           */

/* Authors:                                                                 */
/*    RFWS: R.F. Warren-Smith (STARLINK)                                    */
/*    BKM:  B.K. McIlwrath    (STARLINK)                                    */
/*    {@enter_new_authors_here@}                                            */

/* History:                                                                 */
/*    6-FEB-1991 (RFWS):                                                    */
/*       Original version.                                                  */
/*    21-MAY-1991 (RFWS):                                                   */
/*       Removed use of HCB "reserve" field: no longer present.             */
/*    23-MAY-1991 (RFWS):                                                   */
/*       Added checks for flag values of -1 in the STK .bloc and .spare     */
/*       fields. This is now needed because these fields are stored         */
/*       internally as normal integers rather than as 20 bit fields.        */
/*    23-MAY-2000: (BKM):                                                   */
/*       Revise STK storage algorithm to accept (and efficiently pack)      */
/*       64-bit pointers                                                    */
/*    14-JUN-2002: (BKM)                                                    */
/*       Choice of HDS V3 (32-bit) or HDS-V4 (64-bit) HCB                   */
/*    {@enter_further_changes_here@}                                        */

/* Bugs:                                                                    */
/*    {@note_any_bugs_here@}                                                */

/*-                                                                         */

/* Local Variables:                                                         */
      INT_BIG bloc;              /* STK .bloc field value                   */
      INT_BIG spare;             /* STK .spare field value                  */
      int i;                     /* Loop counter for packing arrays         */
      int j;                     /* Position of start of STK data           */
      int ix_lrb, ix_pdb;
      int byte=0;
      int nb;
      unsigned char prec[15];
/*.                                                                         */

/* Check the inherited global status.                                       */
   if ( !_ok( hds_gl_status ) )
      return hds_gl_status;

/* Pack the stamp field into the first 3 chars.                             */
   phcb[ 0 ] = hcb->stamp & 0xff;
   phcb[ 1 ] = ( hcb->stamp >> 8 ) & 0xff;
   phcb[ 2 ] = ( hcb->stamp >> 16 ) & 0xff;

/* Insert the version number into the 4th char.                             */
   phcb[ 3 ] = hcb->version;

/* Pack the end-of-file block number into the following 8 chars.            */
   phcb[ 4 ]  = hcb->eof & 0xff;
   phcb[ 5 ]  = ( hcb->eof >> 8  ) & 0xff;
   phcb[ 6 ]  = ( hcb->eof >> 16 ) & 0xff;
   phcb[ 7 ]  = ( hcb->eof >> 24 ) & 0xff;
   if( hds_gl_64bit ) {
      phcb[ 8 ]  = ( hcb->eof >> 32 ) & 0xff;
      phcb[ 9 ]  = ( hcb->eof >> 40 ) & 0xff;
      phcb[ 10 ] = ( hcb->eof >> 48 ) & 0xff;
      phcb[ 11 ] = ( hcb->eof >> 56 ) & 0xff;

/* The next 17 chars are not used, so set them to zero.                     */
      for ( i = 0; i < 17; i++ )
      {
         phcb[ i + 12 ] = 0;
      }

/* Reserve the next two bytes to record the number of LRB and PDB stack     */
/* entries (respectively) and fill in when we have calculated the number in */
/* use.                                                                     */

      phcb [ 29 ] = 0;
      phcb [ 30 ] = 0;
      byte = 31;
   } else {

/* The next 24 chars are not used, so set them to zero.                     */
      for ( i = 0; i < 24; i++ )
      {
         phcb[ i + 8 ] = 0;
      }
   }

/* Pack as many (normally all) of the STK entries in the remaining 480      */
/* bytes. As entries are two 64-bit integers from HDS version 4 these are   */
/* stored in packed format with unused leading bytes eliminated.            */
/* As previously, in the unlikely event that not all STK entries can be     */
/* accommodated in the space allocated some will be disgarded. This causes  */
/* no problems except that this space cannot then be reused.                */
/* The LRB and PDB entries are stored alternately.                          */

   if (hds_gl_64bit)
   {
      ix_lrb = 0;
      ix_pdb = REC__MXSTK - 1;
      do {
         if ( ix_lrb >= 0 ) {
            nb = pack_rec ( hcb->stk[ ix_lrb ], prec ); 
            if ( (hcb -> stk[ ix_lrb ].bloc < 0) |
                 ( byte + nb > 511 ) )
             {
                phcb[ 29 ] = ix_lrb;
                ix_lrb = -1;
             } else {
                memcpy( &phcb[ byte ], prec, nb );
                byte += nb;
                ix_lrb++;
             }
         }
         if ( ix_pdb >= 0 ) {
            nb = pack_rec ( hcb->stk[ ix_pdb ], prec ); 
            if ( (hcb -> stk[ ix_pdb ].bloc < 0) |
                 ( byte + nb > 511 ) )
             {
                phcb[ 30 ] = REC__MXSTK - ix_pdb - 1;
                ix_pdb= -1;
             } else {
                memcpy( &phcb[ byte ], prec, nb );
                byte += nb;
                ix_pdb--;
             }
         }
      } while( (ix_lrb >= 0) | (ix_pdb >= 0) );

/* Set unused bytes in the HCB to zero.                                     */
      for ( ; byte<512; byte++ )
         phcb [ byte ] = 0;
   } else {                                     /* 32-bit HDS */
   /* Loop to pack all the STK elements, each of which occupies a further 5    */
/* chars.                                                                   */
      for ( i = 0, j = 32; i < REC__MXSTK; i++, j += 5 )
      {

/* Obtain the values of the STK .bloc and .spare fields, checking for any   */
/* flag values of -1 and translating them into all 20 bits of the packed    */
/* value being set.                                                         */
         bloc = ( hcb->stk[ i ].bloc == -1 ) ? 0xfffff : hcb->stk[ i ].bloc;
         spare = ( hcb->stk[ i ].spare == -1 ) ? 0xfffff : hcb->stk[ i ].spare;

/* Pack the STK .bloc and .spare fields into each group of 5 chars, 20 bits */
/* per value.                                                               */
         phcb[ j ] = bloc & 0xff;
         phcb[ j + 1 ] = ( bloc >> 8 ) & 0xff;

/* The middle char contains contributions from both values...               */
         phcb[ j + 2 ] = ( ( bloc >> 16 ) & 0xf ) | ( ( spare << 4 ) & 0xf0 );
         phcb[ j + 3 ] = ( spare >> 4 ) & 0xff;
         phcb[ j + 4 ] = ( spare >> 12 ) & 0xff;
      }
   }

/* Return the current global status value.                                  */
      return hds_gl_status;
}

/* Static helper function for rec1_pack_hcb.                                */
/* This takes a stack entry any efficiently packs it into a byte string by  */
/* eliminating null leading bytes in the two integers and adding a byte     */
/* count.                                                                   */
static int
pack_rec(struct STK entry, unsigned char *prec) {
   INT_BIG mask;
   int byte=1;
   int i,j,k;
   if (entry.bloc <= 0) {
       prec[0] = (entry.bloc ? 0x80 : 0);
       i = 0;
   } else {
       mask = 0xff;
       mask <<= 56;
       for(i=8; i; i--) {
          if(entry.bloc & mask)
             break;
          mask >>= 8;
       }
       prec[0] = i << 4;
       for(k=i; k; k--) {
           prec[byte++] = entry.bloc & 0xff;
           entry.bloc >>= 8;
       }
   }
   if (entry.spare <= 0) {
      prec[0] |= (entry.spare ? 0x08: 0);
      j = 0;
   } else {
       mask = 0xff;
       mask <<= 56;
       for(j=8; j; j--) {
          if(entry.spare & mask)
             break;
          mask >>= 8;
       }
       prec[0] |= j;
       for(k=j; k; k--) {
           prec[byte++] = entry.spare & 0xff;
           entry.spare >>= 8;
       }
   }
   return(i+j+1);
}
