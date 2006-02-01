#if !defined( REC1_INCLUDED )	 /* rec1.h already included?		    */
#define REC1_INCLUDED 1

#include "rec.h"		 /* Public rec definitions		    */

/* VMS version include files:						    */
/* =========================						    */
#if defined( vms )

/* POSIX version include files:						    */
/* ===========================						    */
#else
#include <stddef.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#endif

/* Constants:								    */
/* =========								    */
#define REC__BIGMEM 5121	 /* Bytes for a "big" memory request	    */
#define REC__MXCHIP 15		 /* No. chips in a block		    */
#define REC__MXSTK 96		 /* Max size in free space stack	    */
#define REC__SZBLK 512		 /* Size of a container file block	    */
#define REC__SZCBM 2		 /* Size of a packed Chip Bitmap	    */
#define REC__SZCHAIN 8		 /* Size of a packed chained block number   */
#define REC__SZOCHAIN 4		 /* Size of a packed V3 chain block number  */
#define REC__SZCHIP 34		 /* Size of a chip			    */

#define REC__STAMP 5456979	 /* HDS file identification stamp	    */
#define REC__VERSION4 4		 /* Current file format version #	    */
#define REC__VERSION3 3		 /* Compatibility file format version #	    */

#if defined( vms )
#define REC__NOIOCHAN 0		 /* Null value for an I/O channel	    */
#else
#define REC__NOIOCHAN NULL	 /* Null value for a file stream	    */
#endif

/* Macros:								    */
/* ======								    */

/* Number of chips required for a record.				    */
#define _nchips( len )\
		( ( ( hds_gl_64bit ? REC__SZRCL : REC__SZORCL ) + len + \
                      REC__SZCHIP - 1 ) / REC__SZCHIP )

/* Number of blocks required for a frame.				    */
#define _nblocs( len )\
		( ( len + REC__SZBLK - 1 ) / REC__SZBLK )

/* Number of bytes required for a chained block address                     */
#define SZCHAIN(extended)\
                ( extended ? REC__SZCHAIN : REC__SZOCHAIN)

/* Data Structures:							    */
/* ===============							    */

/* STK - Stack fields.							    */
      struct STK
      {
         INT_BIG bloc;		/* Block number			            */
         INT_BIG spare;	        /* Number of spare blocks or chips	    */
      };

/* HCB - Header Control Block.						    */
      struct HCB
      {
         struct STK stk[ REC__MXSTK ]; /* Free space stack		    */
         INT_BIG eof;		 /* End-of-file block number		    */
         unsigned int stamp;	 /* Container file stamp		    */
         int version;		 /* Data-system version number		    */
      };

/* FID - File ID.							    */

#if defined( vms )		 /* VMS-specific file ID		    */
      struct FID
      {
         char dev[ 16 ];	 /* Device				    */
         short int fil[ 3 ];	 /* File				    */
         short int dir[ 3 ];	 /* Directory				    */
      };
#else				 /* Portable (POSIX) file ID		    */
      struct FID
      {
         dev_t st_dev;		 /* ID of device containing file (POSIX)    */
         ino_t st_ino;		 /* File serial number (POSIX)		    */

#if defined __MINGW32__ || __CYGWIN__
         dev_t st_rdev;		 /* Extra part of win32 file information    */
#endif

      };
#endif

/* FCV - File Control Vector.						    */
      struct FCV
      {
         char *name;		 /* Pointer to file name string		    */
         struct FID *fid;	 /* Pointer to file-ID			    */
         struct HCB *hcb;	 /* Pointer to HCB information		    */
#if defined( vms )
         unsigned int lid;	 /* Lock-ID				    */
         int read;		 /* Read-only I/O channel		    */
	 int write;		 /* Write I/O channel			    */
#else
         FILE *read;		 /* Read-only I/O channel		    */
	 FILE *write;		 /* Write I/O channel			    */
#endif
         int count;		 /* Reference count			    */
	 int dele;		 /* Marked for deletion?		    */
         int open;		 /* Slot open?				    */
         int locked;		 /* Locked?				    */
         int hcbmodify;		 /* HCB modified?			    */
         int hds_version;	 /* HDS file version 32bit=3 64bit=4        */
      };

/* BID - Block ID.							    */
      struct BID
      {
         int slot;		 /* Slot number				    */
         INT_BIG bloc;		 /* Block number			    */
      };

/* BCP - Block Control Packet.						    */
      struct BCP
      {
         struct BCP *flink;	 /* Forward link to next BCP		    */
         struct BCP *blink;	 /* Backward link to last BCP		    */
         struct BID bid;	 /* Block ID				    */
         int count;		 /* Reference count			    */
         unsigned char *bloc;	 /* Pointer to block's cached data	    */
         int modify;		 /* Block modified?			    */
      };

      extern int rec_gl_active;	 /* rec_ facility active?		    */
      extern int rec_gl_endslot; /* Next FCV slot # to use		    */
      extern int rec_gl_mxslot;  /* Number of FCV slots allocated	    */
      extern int rec_gl_wplsize; /* Current Working Page List size	    */
      extern struct BCP *rec_ga_fpl; /*	Free Page List			    */
      extern struct BCP *rec_ga_lastbcp; /* Address of last used BCP	    */
      extern struct BCP *rec_ga_wpl; /*	Working Page List		    */
      extern struct FCV *rec_ga_fcv; /*	File control vector		    */
      extern struct WLD *rec_gl_wldque; /* Wild-card search context queue   */

extern struct BCP *rec_ga_fpl_malloced; /* memory address of malloced FPL */

/* Function Prototypes:							    */
/* ===================							    */
      int rec1_alloc_frame( int slot, INT_BIG size, INT_BIG *bloc );
      void rec1_clear_cbm( unsigned char cbm[ 2 ], int nchip, int pos );
      int rec1_close_file( int slot, char mode );
      int rec1_close_slot( int slot );
      void rec1_create_file( int expand, const char *file, INT file_len,
			     INT size, INT *slot, INT *alq );
      int rec1_deall_frame( int slot, INT_BIG size, INT_BIG bloc );
      INT rec1_extend_file( INT slot, INT_BIG size, INT_BIG *actsize );
      int rec1_extend_frame( int slot, INT_BIG size, INT_BIG extra,
                             INT_BIG *bloc );
#if defined( vms )
      void rec1_find_file( void );
#else
      void rec1_find_file( const char *fspec, INT fspec_len, pid_t *pid,
			   FILE **stream );
#endif
      int rec1_flush_block( struct BCP *bcp );
      void rec1_fmsg( const char *token, int slot );
      int rec1_get_addr( size_t size, unsigned char **start,
                         unsigned char **end );
#if defined( vms )		 /* These routines not used on VMS:	    */
      void rec1_get_fid( void );
      void rec1_get_path( void );
      void rec1_getcwd( void );
#else
      void rec1_get_fid( const char *fns, struct FID *fid );
      void rec1_get_path( const char *fname, INT fname_len, char **path,
			  INT *path_len );
      void rec1_getcwd( char **cwd, INT *lcwd );
#endif
      int rec1_locate_hcb( int slot, char mode, struct HCB **hcb );
      int rec1_lock_slot( int slot );
      int rec1_map_frame( int slot, INT_BIG bloc, INT_BIG length,
                          INT_BIG offset, char mode, unsigned char **pntr );
      void rec1_open_file( int expand, const char *file, INT file_len,
			   char mode, INT *slot, int *newslot );
      int rec1_pack_chain( INT_BIG chain, unsigned char pchain[ 8 ] );
      int rec1_pack_hcb( const struct HCB *hcb,
                         unsigned char phcb[ REC__SZBLK ] );
      int rec1_pack_ncomp( int ncomp, unsigned char pncomp[ 4 ] );
      int rec1_pack_rcl( const struct RCL *rcl, unsigned char prcl[ 10 ] );
      void rec1_put_addr( unsigned char *start, unsigned char *end,
                          int *status );
      int rec1_read_file( int slot, INT_BIG bloc, int size, 
                          unsigned char *buffer );
      int rec1_scan_cbm( const unsigned char cbm[ 2 ], int nchip, int *pos );
      void rec1_set_cbm( unsigned char cbm[ 2 ], int nchip, int pos );
#if defined( vms )
      void rec1_shell( void );
#else
      void rec1_shell( pid_t *pid, FILE *stream[ 2 ] );
#endif
      int rec1_test_cbm( const unsigned char cbm[ 2 ], int start, int nchip );
      int rec1_unlock_slot( int slot );
      int rec1_unmap_frame( int slot, INT_BIG bloc, INT_BIG length, 
                            INT_BIG offset, char mode, unsigned char **pntr );
      int rec1_unpack_chain( const unsigned char pchain[], int extended,
                             INT_BIG *chain );
      int rec1_unpack_hcb( const unsigned char phcb[ REC__SZBLK ],
                           struct HCB *hcb );
      int rec1_unpack_ncomp( const unsigned char pncomp[ 4 ], int *ncomp );
      int rec1_unpack_rcl( const unsigned char prcl[ 19 ], struct RCL *rcl );
      int rec1_update_free( int slot, INT_BIG bloc, 
                            const unsigned char cbm[ 2 ] );
      int rec1_write_file( int slot, int size, const unsigned char *buffer,
                           INT_BIG bloc );

#endif
