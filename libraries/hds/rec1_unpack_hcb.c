#if HAVE_CONFIG_H
#  include <config.h>
#endif

#include "ems.h"
#include "hds1.h"                /* Global definitions for HDS              */
#include "rec1.h"                /* Internal rec_ definitions               */
#include "dat_err.h"             /* DAT__ error code definitions            */

/* Static helper function for this routine                                  */

static int unpack_value( const unsigned char *ptr, struct STK *stk);


   int rec1_unpack_hcb( const unsigned char phcb[ REC__SZBLK ],
                        struct HCB *hcb )
   {
/*+                                                                         */
/* Name:                                                                    */
/*    rec1_unpack_hcb                                                       */

/* Purpose:                                                                 */
/*    Unpack Header Control Block information.                              */

/* Invocation:                                                              */
/*    rec1_unpack_hcb( phcb, hcb )                                          */
/*                                                                          */
/* Description:                                                             */
/*    This function unpacks the information in the Header Control Block of  */
/*    an HDS container file and puts it into an HCB structure. This is done */
/*    so that the Header Control Block format need not depend on the        */
/*    details of the way that an HCB structure is stored in memory.         */

/* Parameters:                                                              */
/*    const unsigned char phcb[ REC__SZBLK ]                                */
/*       Pointer to an array of REC__SZBLK Header Control Block characters  */
/*       to be unpacked.                                                    */
/*    struct HCB *hcb                                                       */
/*       Pointer to an HCB structure to receive the unpacked information.   */

/* Returned Value:                                                          */
/*    int rec1_unpack_hcb                                                   */
/*       The global status value current on exit.                           */

/* Copyright:                                                               */
/*    Copyright (C) 1990-1991 Science & Engineering Research Council        */
/*    Copyright (C) 2000,2004 Particle Physics and Astronomy Research       */
/*    Council. Copyright (C) 2007 Science and Technology Facilities Council.*/
/*    All Rights Reserved.                                                  */

/*  Licence:                                                                */
/*     This program is free software; you can redistribute it and/or        */
/*     modify it under the terms of the GNU General Public License as       */
/*     published by the Free Software Foundation; either version 2 of       */
/*     the License, or (at your option) any later version.                  */

/*     This program is distributed in the hope that it will be              */
/*     useful, but WITHOUT ANY WARRANTY; without even the implied           */
/*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR              */
/*     PURPOSE. See the GNU General Public License for more details.        */

/*     You should have received a copy of the GNU General Public            */
/*     License along with this program; if not, write to the Free           */
/*     Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,       */
/*     MA 02111-1307, USA                                                   */


/* Authors:                                                                 */
/*    RFWS: R.F. Warren-Smith (STARLINK)                                    */
/*    BKM:  B.K. McIlwrath    (STARLINK)                                    */
/*    TIMJ: Tim Jenness       (JAC, Hawaii)                                 */
/*    {@enter_new_authors_here@}                                            */

/* History:                                                                 */
/*    25-JUL-1990 (RFWS):                                                   */
/*       Original version.                                                  */
/*    25-MAR-1991 (RFWS):                                                   */
/*       Added unpacking of the reserve field and fixed bug in STK          */
/*       structure unpacking.                                               */
/*    21-MAY-1991 (RFWS):                                                   */
/*       Removed reserve field: no longer present.                          */
/*    23-MAY-1991 (RFWS):                                                   */
/*       Added test for all bits set in STK bloc and spare fields. This is  */
/*       now needed because these HCB components are stored internally as   */
/*       normal integers, not as 20-bit fields as they are in their packed  */
/*       form.                                                              */
/*    24-MAY-2000 (BKM):                                                    */
/*       Revise logic for new packing strategy for STK entries implemented  */
/*       in HDS version 4.                                                  */
/*    26-JUN-2000 (BKM):                                                    */
/*       Modify to accept both HDS V3 and HDS V4 packed formats.            */
/*    20-MAY-2004 (BKM):                                                    */
/*       Do not attempt to unpack HCB for invalid HDS files                 */
/*    23-NOV-2007 (TIMJ):                                                   */
/*       Use emsRep when status is set to bad so that people can know the   */
/*       reason for failure.                                                */
/*    {@enter_further_changes_here@}                                        */

/* Bugs:                                                                    */
/*    {@note_any_bugs_here@}                                                */

/*-                                                                         */

/* Local Variables:                                                         */
/*  Variables used in unpacking HDS V3 files.                               */
      int bloc;                  /* Value of STK bloc field                 */
      int i;                     /* Loop counter for STK elements           */
      int spare;                 /* Value of STK spare field                */
/*  Variables used in unpacking HDS V4 files.                               */
      int nlrb;                  /* Number of STK LRB values                */
      int npdb;                  /* Number of STK PDB values                */
      int ix_lrb;                /* Index of current LRB                    */
      int ix_pdb;                /* Index of current PDB                    */
/*  Variables used for both HDS file formats.                               */
      int byte;                  /* Current byte in HCB                     */

/*.                                                                         */

/* Check the inherited global status.                                       */
      if ( !_ok( hds_gl_status ) ) return hds_gl_status;

/* Extract the stamp field from the first 3 chars.                          */
      hcb->stamp = ( ( ( ( phcb[ 2 ] ) << 8 ) |
                           phcb[ 1 ] ) << 8 ) |
                           phcb[ 0 ];

/* Extract the version number from the 4th char.                            */
      hcb->version = phcb[ 3 ];

/* Initialise all the stack exlements to null.                              */
      for( i = 0; i < REC__MXSTK; i++ )
      {
         hcb->stk[ i ].bloc  = -1;
         hcb->stk[ i ].spare = -1;
      }

/* Return if not a valid HDS file                                           */
      if( hcb->stamp != REC__STAMP ||
          hcb->version > REC__VERSION4 )
      {
         hds_gl_status = DAT__FILIN;
         emsRep( "REC1_UNPACK_HCB_1",
                 "HDS file is invalid. Header Control Block is corrupt.",
                 &hds_gl_status);
         return hds_gl_status;
      }
/* Test for older format HCB (from HDS Version 3)                           */
      if ( hcb->version < REC__VERSION4 )
      {
/* Extract the end-of-file block number from the following 4 chars.         */
         hcb->eof = ( ( ( ( ( ( phcb[ 7 ] ) << 8 ) |
                                phcb[ 6 ] ) << 8 ) |
                                phcb[ 5 ] ) << 8 ) |
                                phcb[ 4 ];

/* Loop to extract all the STK elements, each of which occupies 5 chars.    */
/* Note that the HDS V3 stack size if smaller than that used with HDS V4.   */
         ix_lrb = ix_pdb  = 0;          /* Stack indices                    */
         for ( byte = 32; byte < REC__SZBLK; byte += 5 )
         {

/* Extract the STK bloc field from the first 20 bits of each element. If    */
/* all 20 bits are set, then use a flag value of -1 instead.                */
            bloc = ( ( ( ( phcb[ byte + 2 ] & 0xf ) << 8 ) |
                           phcb[ byte + 1 ] ) << 8 ) |
                           phcb[ byte ];
            if ( bloc != 0xfffff )
            {
                if ( ix_pdb == 0 )
                {
                   i = ix_lrb++;
                }
                else
                {
                   i = ix_pdb++;
                }
            }
            else
            {
               if ( ix_pdb == 0 )
                  ix_pdb = REC__MXSTK - 95 + i;
               ix_pdb++;
               continue;          /* Jump to next loop iteration            */
            }
            spare = ( ( ( phcb[ byte + 4 ] << 8 ) |
                          phcb[ byte + 3 ] ) << 4 ) |
                      ( ( phcb[ byte + 2 ] >> 4 ) & 0xf );
            hcb->stk[ i ].bloc = bloc;
            hcb->stk[ i ].spare = spare;
         }
     }
     else                  /* HDS Version 4 HCB                             */
     {


/* Extract the end-of-file block number from the next 8 chars.              */
         hcb->eof = ( ( ( ( ( ( ( ( ( ( ( ( ( (
                         phcb[ 11 ] ) << 8 ) |
                         phcb[ 10 ] ) << 8 ) |
                         phcb[ 9 ] )  << 8 ) |
                         phcb[ 8 ] )  << 8 ) |
                         phcb[ 7 ] )  << 8 ) |
                         phcb[ 6 ] )  << 8 ) |
                         phcb[ 5 ] )  << 8 ) |
                         phcb[ 4 ];

/* Extract the number of stored LRBs and PDBs                               */
         nlrb = phcb[ 29 ];
         npdb = phcb[ 30 ];

/* Point to start of packed information.                                    */
         byte = 31;

/* Extract packed information.                                              */

         ix_lrb = 0;
         ix_pdb = REC__MXSTK - 1;
         do
         {
            if ( nlrb ) {
               byte += unpack_value( &phcb[ byte], &hcb->stk[ ix_lrb++ ] );
               nlrb--;
            }
            if ( npdb )
            {
               byte += unpack_value( &phcb[ byte], &hcb->stk[ ix_pdb-- ] );
               npdb--;
            }
         } while ( nlrb | npdb );
      }

/* Return the current global status value.                                  */
      return hds_gl_status;
   }

static int
unpack_value( const unsigned char *ptr, struct STK *stk)
{
/* This is helper function for rec1_unpack_hcb.                             */
/* On entry the variable 'ptr' points to the first byte of a packed STK     */
/* structure.                                                               */
/*                                                                          */
/* On exit the unpacked STK structure is returned and the value of the      */
/* function is the number of bytes processed.                               */

      INT_BIG bloc;              /* Value of STK bloc field                 */
      INT_BIG spare;             /* Value of STK spare field                */
      INT_BIG temp;              /* Temporary variable                      */
      int nbb;                   /* Number of bytes in packed BLOC value    */
      int nbs;                   /* Number of bytes in packed SPARE value   */
      int shift;                 /* Bit shift left value                    */
      int retval;                /* Return value - number of bytes          */

      nbb = *ptr >> 4;
      nbs = *ptr++ & 0x0f;
      retval = nbb + nbs + 1;

      if ( nbb == 0 )
         bloc = 0;
      else if ( nbb & 0x08 )
         bloc = -1;
      else
      {
         bloc = 0;
         shift = 0;
         do
         {
            temp = ( 0 | *ptr++ );
            bloc |= ( temp << shift );
            shift += 8;
         } while ( --nbb != 0 );
      }
      if ( nbs == 0 )
         spare = 0;
      else if ( nbs & 0x08 )
         spare = -1;
      else
      {
         spare = 0;
         shift = 0;
         do
         {
            temp = ( 0 | *ptr++ );
            spare |= ( temp << shift );
            shift += 8;
          } while ( --nbs != 0 );
       }
       stk->bloc   = bloc;
       stk->spare = spare;

       return retval;
}
