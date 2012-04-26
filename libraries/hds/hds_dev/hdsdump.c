
#if HAVE_CONFIG_H
#include <config.h>
#endif

#include "hds1.h"
#include "rec1.h"
#include "dat1.h"

#include <stdlib.h>
#include <stdio.h>

/*
*+
*  Name:
*     hdsdump

*  Language:
*     Starlink C

*  Description:
*     This is a anaysis tool for HDS files - it will dump the internal record
*     contents to stdout.

*  Copyright:
*     Copyright (C) 2004 Central Laboratory of the Research Councils.
*     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
*     Copyright (C) 2007 Science and Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Author:
*     Brian McIlwrath, Starlink, RAL.
*     Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     01-MAY-2004 (BKM):
*      Original version for HDS version 3 files
*     24-JAN-2006 (BKM):
*      Extended to dump BOTH HDS V3 and V4 files
*     13-APR-2006 (BKM):
*      Correct a bug in dumping arrays of structures.
*     19-JUL-2006 (TIMJ):
*      Fix compiler warnings. Use fseeko. Privatise some globals.
*      Fix buffer overruns in unterminated strings.
*     26-JUL-2006 (TIMJ):
*      chip needs to be global (do not know why) to allow
*      one of my test HDS files to dump without segv.
*      Factor out fseek code.
*     19-JUL-2007 (BKM):
*      Logic went wrong when a chained structure or component used more
*      than 1 HDS block - corrected by malloc()ing the correct buffer size.
*     25-JUL-2007 (BKM):
*      Logic again wrong when LRB contents referred to items in preceeding
*      blocks.
*     23-NOV-2007 (TIMJ):
*      Abort the dump if the HCB is corrupt.
*     25-APR-2012 (TIMJ):
*      Add _INT64 support
*     {enter_further_changes_here}

*-
 */

/* Internal prototypes */
void decode_type( int type, char **string);
void decode_pdd( struct PDD *pdd );
void add_block(INT_BIG bloc);
void read_block ( FILE * fp, INT_BIG bloc, size_t nbytes, void * ptr );

/* Global variables */
INT_BIG nxtblk[50];
INT_BIG blkcnt;

/* for some as yet undetermined reason these two need to be globals to prevent
   core dump on some large test files. Even though they are not used outside of main */

   int  chip;						   /* Current chip  */



int
main(int argc, char **argv)
{
   /* Local variables */
   INT_BIG cur_block;					   /* Current block */
   FILE *fp = NULL;
   unsigned char block[REC__SZBLK];                         /* Block buffer  */
   struct HCB hcb;					   /* HCB data      */
   struct RCL rcl;                                          /* RCL data      */
   struct RID rid;			      		   /* Record id     */
   struct ODL odl;		      			   /* ODL           */
   char name[DAT__SZNAM+1];
   unsigned char *ptr;
   char *type;
   char axtype[DAT__SZTYP+1];
   unsigned int cbm;					   /* Chip Bit Mask */

   int i, j;
   int status;
   INT_BIG cblk;
   HDS_PTYPE axsz;
   UINT_BIG size, tpdb, tlrb;
   unsigned char *tblk;			                  /* Temporary block buffer */

/* Program entry point */

/* Check argument */
   if (argc != 2) {
      fprintf(stderr, "HDS file name must be first and only parameter!\n");
      exit(1);
   }
/* Open HDS file  and read packed HCB into buffer */
   if( (fp = fopen(argv[1], "rb")) == NULL ) {
      perror("hds file open error");
      exit(1);
   } else
      printf("\nHDS dump - BKM/TIMJ 20070725 - file %s\n\n", argv[1] );
   if( fread( block, 1, REC__SZBLK, fp) != REC__SZBLK ) {
      perror("HCB block read error");
      fclose( fp );
      exit(1);
   }

/*
 * Decode and print HCB contents
 */
   status = rec1_unpack_hcb( block, &hcb );
   if (status != DAT__OK) {
     printf("Unable to unpack HCB. Aborting dump.\n");
     fclose( fp );
     exit(1);
   }
   printf("HCB information:\n HDS version %d, eof block=%" HDS_INT_BIG_S "\n",
          hcb.version, hcb.eof);
   printf(" Stack information (LRB)\n");
   for( i=0; i<REC__MXSTK; i++ ) {
       if( hcb.stk[i].bloc < 0 )
       {
          printf(" LRB stack ends at entry %d\n", i);
          break;
       }
       printf(" Block %"HDS_INT_BIG_S", spare %"
	      HDS_INT_BIG_S"\n", hcb.stk[i].bloc, hcb.stk[i].spare );
   }
   printf("\n Stack information (PDB)\n");
   for( i=REC__MXSTK-1; i > 0; i-- ) {
       if( hcb.stk[i].bloc < 0 )
       {
          printf(" PDB stack ends at entry %d\n", i);
          break;
       }
       printf(" Block %" HDS_INT_BIG_S
	      ", spare %" HDS_INT_BIG_S "\n", hcb.stk[i].bloc, hcb.stk[i].spare );
   }

/* Set 64-bit flag - used internally by various decode routines */
   hds_gl_64bit = hcb.version > REC__VERSION3;

/* Zero the array of next LRB block numbers */
   for(i=0; i<50; i++)
      nxtblk[i] = 0;

/* Read and decode LRB entries  starting at (2,0) container record */
   nxtblk[0] = 2;
   blkcnt = 1;
   tlrb = 0;
   tpdb = 0;

   do {
      tlrb++;
      cur_block = nxtblk[0];
      read_block( fp, cur_block, REC__SZBLK, block );
      printf("\n\n**** LRB block %d", cur_block);
      cbm = (block[1] << 8 | block[0]);
      if( cbm == 0x7fff)
	 printf("  (All chips used)\n");
      else {
         printf("  (Free chips ");
         for(i =0; i < REC__MXCHIP; i++)
            if(!(cbm & (1<<i)))
               printf("%d, ", i);
         printf(")\n");
      }

      for( chip=0; chip < REC__MXCHIP ; chip+=rcl.size ) {
         ptr = block + REC__SZCBM + (chip * REC__SZCHIP );
         rec1_unpack_rcl( ptr, &rcl );
         if ( !rcl.size || rcl.active > 1 || rcl.active <= 0 ||
              rcl.class <= 0 || rcl.class > 4 || rcl.dlen <0 ) {
             rcl.size = 1;
             continue;
         }
         decode_type( rcl.class, &type);
         printf("\n%s record (%"HDS_INT_BIG_S",%d):\n", type, cur_block, chip);
         printf(" Parent(b=%" HDS_INT_BIG_S ",c=%d),size=%d,chained=%d,active=%d,"
                "slen=%d,dlen=%"HDS_INT_BIG_U"\n",
                 rcl.parent.bloc, rcl.parent.chip, rcl.size, rcl.chain,
                 rcl.active,rcl.slen, rcl.dlen);
         ptr += (rcl.extended ? REC__SZRCL : REC__SZORCL);
         if( rcl.chain ) {
             rec1_unpack_chain( ptr+rcl.slen, rcl.extended, &cblk);
             size = (rcl.dlen + REC__SZBLK-1)/REC__SZBLK;
             printf(" Chained data starts block = %" HDS_INT_BIG_S
		    " size(blocks)=%" HDS_INT_BIG_U "\n",
                      cblk, size);
             tpdb += size;
         }

         switch (rcl.class) {
            struct PDD pdd;
            case DAT__CONTAINER:
               _chmove( DAT__SZNAM, ptr, name );
	       name[DAT__SZNAM] = '\0';
               dat1_unpack_crv( ptr, 0, &rid );
               printf(" Name = %s Next record = (%" HDS_INT_BIG_S",%d)\n",
                        name, rid.bloc, rid.chip );
               if( rid.bloc > cur_block)
                  add_block( rid.bloc );
               break;
            case DAT__STRUCTURE:
               dat1_unpack_odl( ptr, &odl );
	       _chmove( DAT__SZTYP, odl.type, axtype );
	       axtype[DAT__SZTYP] = '\0';
               printf(" ODL name= %s, naxes=%d ", axtype, odl.naxes );
               for( i=0; i<odl.naxes; i++) {
                  if( i == 0 )
                     printf("(");
                  printf("%" HDS_DIM_FORMAT " ", odl.axis[i] );
               }
               if( odl.naxes != 0 )
                  printf(")");
               printf("\n Next record(s):\n");
               ptr += rcl.slen;
               if( rcl.chain )
               {
                  printf("  From chained block:\n");
                  tblk = malloc(rcl.dlen);
                  read_block( fp, cblk, rcl.dlen, tblk );
                  ptr = tblk;
               }
               if(odl.naxes == 0) {
                  odl.naxes = 1;
                  odl.axis[0] = 1;
               }
               for(i=0; i<odl.naxes; i++) {
                  for(axsz=0; axsz<odl.axis[i]; axsz++) {
		     dat1_unpack_srv( ptr, &rid );
                     printf("  (%" HDS_INT_BIG_S",%d)\n",rid.bloc, rid.chip);
                     if( rid.bloc > cur_block )
                        add_block( rid.bloc);
                     ptr += (rcl.extended ? 8: 4);
                  }
               }
               if( rcl.chain )
                  free(tblk);
               break;
            case DAT__COMPONENT:
               i = (int) *ptr;
               printf(" # components = %d\n", i);
               ptr += rcl.slen;
               if( rcl.chain )
               {
                  printf("  From chained block:\n");
                  tblk = malloc(rcl.dlen);
                  read_block( fp, cblk, rcl.dlen, tblk );
                  ptr = tblk;
               }
               for(j =0; j<i; j++ ) {
		  _chmove( DAT__SZNAM, ptr, name );
		  name[DAT__SZNAM] = '\0';
		  dat1_unpack_crv( ptr, 0, &rid );
                  printf("  Name = %s rid=(%" HDS_INT_BIG_S",%d)\n", name, rid.bloc,
                         rid.chip);
                  if( rid.bloc > cur_block )
                     add_block( rid.bloc );
                  ptr += SZCRV;
               }
               if( rcl.chain)
                  free(tblk);
               break;
            case DAT__PRIMITIVE:
               dat1_unpack_odl( ptr, &odl );
               dat1_unpack_type( odl.type, &pdd );

               if( ptr[1] == '_' ) {
                  printf(" Packed ");
                  decode_pdd( &pdd );
               }
               else
                  printf(" Name =%s", odl.type );
               printf(", naxes=%d ", odl.naxes );
               for( i=0; i<odl.naxes; i++) {
                  if( i == 0 )
                     printf("(");
                  printf("%" HDS_DIM_FORMAT" ", odl.axis[i] );
               }
               if( odl.naxes != 0 )
                  printf(")\n");
               else
                  printf("\n");
         }
      }
      for(i=1; i<= blkcnt; i++)
         nxtblk[i-1] = nxtblk[i];
      blkcnt--;
   } while ( nxtblk[0] != 0);
   printf("\nTotal LRB blocks = %" HDS_INT_BIG_U
	  " Total PDB blocks = %" HDS_INT_BIG_U "\n", tlrb, tpdb);
   fclose(fp);
   exit(0);

}

void
add_block(INT_BIG bloc)
{
   INT_BIG i,j;
   if(bloc < 2)
      return;
   for(i=0; i<blkcnt; i++) {
      if( nxtblk[i] == bloc )
         return;
      if( nxtblk[i] > bloc ) {
         for( j=blkcnt; j>=i; j-- )
            nxtblk[j+1] = nxtblk[j];
            nxtblk[i] = bloc;
            blkcnt++;
            return;
       }
    }
    nxtblk[blkcnt++] = bloc;
}


void
decode_type( int type, char **string)
{
   switch (type) {
      case 1:
         *string = "Container";
         break;
      case 2:
         *string = "Structure";
         break;
      case 3:
         *string = "Component";
         break;
      case 4:
         *string = "Primitive";
   }
}

void
decode_pdd( struct PDD *pdd )
{
   const char *string;

   switch (pdd->dtype) {
      case DAT__B:
          string = "_DOUBLE";
          break;
      case DAT__C:
          string = "_CHAR";
          break;
      case DAT__D:
          string = "_DOUBLE";
          break;
      case DAT__I:
          string = "_INTEGER";
          break;
      case DAT__L:
          string = "_LOGICAL";
          break;
      case DAT__R:
          string = "_REAL";
          break;
      case DAT__UB:
          string = "_UBYTE";
          break;
      case DAT__UW:
          string = "_UWORD";
          break;
      case DAT__W:
          string = "_WORD";
          break;
      case DAT__K:
          string = "_INT64";
          break;
      default:
          string = "Unknown!!";
   }

   printf("dtype = %s length=%d", string, (int)pdd->length);
}

void read_block ( FILE * fp, INT_BIG bloc, size_t nbytes, void * ptr ) {
   int seek_stat;
   int readok = 1;

#if HAVE_FSEEKO
   seek_stat = fseeko(fp, (bloc - 1) * REC__SZBLK, SEEK_SET);
#else
   seek_stat = fseek(fp, (bloc - 1) * REC__SZBLK, SEEK_SET);
#endif

   if (seek_stat == 0) {
     fread( ptr, 1, nbytes, fp );
     if (ferror(fp)) {
       readok = 0;
       clearerr(fp);
     }
   } else {
     readok = 0;
   }

   if (!readok) {
     memset( ptr, 0, nbytes );
     printf("Had trouble reading from file for some unknown reason. Not good.\n");
     exit(1);
   }
}
