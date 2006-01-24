#include "hds1.h"
#include "rec1.h"
#include "dat1.h"

/*
 * This is a anaysis tool for HDS files - it will dump the internal record
 * contents to stdout.
 *
 * Author:
 *   Brian McIlwrath, Starlink, RAL.
 *
 * History:
 *   May-2004: Original version for HDS version 3 files
 *   Jan-2006: Extended to dump BOTH HDS V3 and V4 files
 */ 

FILE *fp;

/* Global variables */
char block[REC__SZBLK];                                    /* Block buffer  */
int  cur_block;						   /* Current block */
int nxtblk[50];
int blkcnt;
int  chip;						   /* Current chip  */
struct HCB hcb;						   /* HCB data      */
struct RCL rcl;                                            /* RCL data      */
struct RID rid;						   /* Record id     */
struct ODL odl;						   /* ODL           */
char name[DAT__SZNAM];
char *ptr;
char *type;

int
main(int argc, char **argv)
{
/* Local variables */
   int i, j;
   long long int cblk;
   int tlrb, tpdb;
   char tblk[REC__SZBLK];			   /* Temporary block buffer */

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
      printf("\nHDS dump - BKM 20060124 - file %s\n\n", argv[1] );
   if( fread( block, 1, REC__SZBLK, fp) != REC__SZBLK ) {
      perror("HCB block read error");
      exit(1);
   }   
      
/*
 * Decode and print HCB contents
 */      
   rec1_unpack_hcb( block, &hcb );
   printf("HCB information:\n HDS version %d, eof block=%d\n", 
          hcb.version, hcb.eof);
   printf(" Stack information (LRB)\n");
   for( i=0; i<REC__MXSTK; i++ ) {
       if( hcb.stk[i].bloc < 0 )
       {
          printf(" LRB stack ends at entry %d\n", i);
          break;
       }
       printf(" Block %lld, spare %lld\n", hcb.stk[i].bloc, hcb.stk[i].spare );
   }    
   printf("\n Stack information (PDB)\n");
   for( i=REC__MXSTK-1; i > 0; i-- ) {
       if( hcb.stk[i].bloc < 0 )
       {
          printf(" PDB stack ends at entry %d\n", i);
          break;
       }
       printf(" Block %lld, spare %lld\n", hcb.stk[i].bloc, hcb.stk[i].spare );
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
      fseek(fp, (cur_block-1) * REC__SZBLK, SEEK_SET);
      fread( block, 1, REC__SZBLK, fp );

      for( chip=0; chip < REC__MXCHIP ; chip+=rcl.size ) {
         ptr = block + REC__SZCBM + (chip * REC__SZCHIP ); 
         rec1_unpack_rcl( ptr, &rcl );
         if ( !rcl.size ) {
             rcl.size = 1;
             continue;
         }    
         decode_type( rcl.class, &type);
         printf("\n%s record (%d,%d):\n", type, cur_block, chip);
         printf(" Parent(b=%lld,c=%d),size=%d,chained=%d,active=%d,"
                "slen=%d,dlen=%lld\n",
                 rcl.parent.bloc, rcl.parent.chip, rcl.size, rcl.chain,
                 rcl.active,rcl.slen, rcl.dlen);
         ptr += (rcl.extended ? REC__SZRCL : REC__SZORCL);
         if( rcl.chain ) {
             rec1_unpack_chain( ptr+rcl.slen, rcl.extended, &cblk);
             i = (rcl.dlen + REC__SZBLK-1)/REC__SZBLK;
             printf(" Chained data starts block = %lld size(blocks)=%d\n",
                      cblk, i);
             tpdb += i;
         }

         switch (rcl.class) {
            struct PDD pdd;
            case DAT__CONTAINER:
               _chmove( DAT__SZNAM, ptr, name );
               dat1_unpack_crv( ptr, 0, &rid );
               printf(" Name = %s Next record = (%lld,%d)\n",
                        name, rid.bloc, rid.chip );
               if( (int) rid.bloc != cur_block)
                  add_block( (int) rid.bloc );
               break;
            case DAT__STRUCTURE:
               dat1_unpack_odl( ptr, &odl );
               printf(" ODL name= %s, naxes=%d ", odl.type, odl.naxes );
               for( i=0; i<odl.naxes; i++) {
                  if( i == 0 )
                     printf("(");
                  printf("%d ", odl.axis[i] );
               }
               if( odl.naxes != 0 )   
                  printf(")");
               printf("\n Next record(s):\n");
               ptr += rcl.slen;
               if( rcl.chain )
               {
                  printf("  From chained block:\n");
                  fseek(fp, (cblk - 1) * REC__SZBLK, SEEK_SET);
                  fread( tblk, 1, REC__SZBLK, fp );
                  ptr = tblk;
               }
               else
                  odl.axis[0] = 1;
               for(i=0; i<odl.axis[0]; i++){
                  dat1_unpack_srv( ptr, &rid );
                  printf("  (%lld,%d)\n",rid.bloc, rid.chip );
                  if( (int) rid.bloc != cur_block )
                     add_block( (int) rid.bloc);
                  ptr += (rcl.extended ? 8: 4);
               }
               break;
            case DAT__COMPONENT:
               i = (int) *ptr;
               printf(" # components = %d\n", i);
               ptr += rcl.slen;
               if( rcl.chain )
               {
                  printf("  From chained block:\n");
                  fseek(fp, (cblk - 1) * REC__SZBLK, SEEK_SET);
                  fread( tblk, 1, REC__SZBLK, fp );
                  ptr = tblk;
               }
               for(j =0; j<i; j++ ) {
                  _chmove( DAT__SZNAM, ptr, name );
                  dat1_unpack_crv( ptr, 0, &rid ),
                  printf("  Name = %s rid=(%lld,%d)\n", name, rid.bloc, 
                         rid.chip);
                  if( (int) rid.bloc > cur_block )
                     add_block( (int) rid.bloc );
                  ptr += SZCRV;
               }
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
                  printf("%d ", odl.axis[i] );
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
   printf("\nTotal LRB blocks = %d Total PDB blocks = %d\n", tlrb, tpdb);
   exit(0);

}

int
add_block(int bloc)
{
   int i,j;
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
 

int
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

int
decode_pdd( struct PDD *pdd )
{
   char *string;
   
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
      default:
          string = "Unknown!!";
   }
          
   printf("dype = %s length=%d", string, pdd->length);
}
