#if !defined( REC_INCLUDED )	 /* rec.h already included?		    */
#define REC_INCLUDED 1

#define REC__SZRCL  19		 /* Length of packed Record Control Label   */
#define REC__SZORCL 10           /* Length of RCL for pre-HDS V4 records    */

#define SZRCL (hds_gl_64bit ? REC__SZRCL : REC__SZORCL)

/* RID - Record ID.							    */
      struct RID
      {
         INT_BIG bloc;		 /* Block number			    */
         int chip;		 /* Chip offset				    */
      };

extern const struct RID rec_gl_ridzero;	/* Null record ID		    */

/* RCL - Record Control Label.						    */
      struct RCL
      {
         struct RID parent;	 /* Parent Record ID			    */
         int class;		 /* Record class			    */
         int size;		 /* Record size (chips)			    */
         int slen;		 /* Static domain length (chars)	    */
         UINT_BIG dlen;          /* Dynamic domain length (chars)	    */
         int active;		 /* Dynamic domain active?		    */
         int chain;		 /* Dynamic domain chained?		    */
         int modify;		 /* Dynamic domain modified?		    */
         int zero;		 /* Dynamic domain zero on create?	    */
         int extended;           /* 64-bit HDS file support                 */
      };

/* HAN - Record Handle.							    */
      struct HAN
      {
         struct RID rid;	 /* Record ID				    */
         int slot;		 /* FCV Slot number			    */
         int read;		 /* Read-only flag			    */
      };

/* WLD_FILE - Wild-card file name.					    */
      struct WLD_FILE
      {
	 char *name;		 /* Pointer to file name string		    */
	 INT len;		 /* File name length			    */
      };

/* WLD - Wild-card search context.					    */
      struct WLD
      {
         struct WLD *blink;	 /* Backward link to previous context	    */
	 struct WLD *flink;	 /* Forward link to next context	    */
	 struct WLD_FILE *list;	 /* Pointer to list of file names	    */
	 INT mxlist;		 /* Allocated size of file name list	    */
	 INT nfile;		 /* Number of file names in list	    */
	 INT current;		 /* Number of current file name (1-based)   */
      };

/* Function Prototypes:							    */
/* ===================							    */
      int rec_adopt_record( const struct HAN *han, const struct HAN *par );
      int rec_alloc_mem( size_t size, void **pntr );
      int rec_alloc_xmem( size_t size, void **pntr );
      void rec_attach_file( int expand, const char *file, INT file_len,
			    char state, char mode, struct RCL *rcl,
			    struct HAN *han );
      void rec_close_file( const struct HAN *han );
      int rec_count_files( int * count );
      int rec_create_record( const struct HAN *par, struct RCL *rcl,
	   		     struct HAN *han );
      int rec_deall_mem( size_t size, void **pntr );
      int rec_deall_xmem( size_t size, void **pntr );
      int rec_delete_record( const struct HAN *han );
      void rec_end_wild( struct WLD **context );
      int rec_extend_record( const struct HAN *han, INT_BIG extent );
      int rec_fcopy( const struct HAN *src, const struct HAN *des );
      int rec_get_handle( const struct RID *rid, const struct HAN *kin,
	   	          struct HAN *han );
      int rec_get_rcl( const struct HAN *han, struct RCL *rcl );
      int rec_get_rid( const struct HAN *han, struct RID *rid );
      int rec_list_files( void );
      int rec_locate_block( int slot, INT_BIG bloc, char mode,
                            unsigned char **lrb );
      int rec_locate_data( const struct HAN *han, INT_BIG length,
                           INT_BIG offset, char mode, unsigned char **pntr );
      int rec_locate_fns( const struct HAN *han, const char **fns );
      int rec_lock( const struct HAN *han );
      void rec_mark_delete( const struct HAN *han, int *status );
      int rec_reall_mem( size_t size, void **pntr );
      void rec_refcnt( const struct HAN *han, int inc, int *refcnt,
		       int *status );
      int rec_release_block ( int slot, INT_BIG bloc );
      int rec_release_data( const struct HAN *han, INT_BIG length,
                            INT_BIG offset, char mode, unsigned char **pntr );
      int rec_reset_record( const struct HAN *han );
      int rec_same_file( const struct HAN *han1, const struct HAN *han2 );
      int rec_shrink_record( const struct HAN *han, INT_BIG extent );
      void rec_start( void );
      void rec_stop( void );
      int rec_unlock( const struct HAN *han );
      int rec_where( const struct HAN *han, INT_BIG length, INT_BIG offset,
                    INT_BIG *bloc, INT_BIG *bytoff );
      void rec_wild_file( const char *fspec, INT fspec_len,
		          struct WLD **context, int *alldone, char **fname,
			  INT *fname_len );
#endif
