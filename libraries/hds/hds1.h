#if !defined( HDS1_INCLUDED )	 /* hds1.h already included?		    */
#define HDS1_INCLUDED 1

/* C include files:							    */
/* ===============							    */
#include <limits.h>
#include <string.h>
#if HAVE_CONFIG_H               /* configure definitions                    */
#include <config.h>
#endif

/* VMS include files:							    */
/* =================							    */
#if defined( vms )
#include <descrip.h>		 /* Data descriptor definitions (VMS)	    */
#endif

/* Global HDS success status.                                               */
/* =========================                                                */
#define DAT__OK 0

/* Description of file mapping implementation.				    */
/* ==========================================				    */
/* This will only need to be extended if new implementations of file	    */
/* mapping are added (which will normally require coding changes in the	    */
/* file access routines). The default for new machines is to assume no file */
/* mapping unless the _mmap macro is defined or the configure property	    */
/* HAVE_MMAP is true                                                        */
#if defined( vms )		 /* VMS implementation			    */
#define HDS__CANMAP 1		 /* File mapping supported?		    */
#define HDS__MAPSEQ 0		 /* Mapping best for sequential access?	    */
#define HDS__MAPSPARSE 1	 /* Mapping best for sparse access?	    */
#define HDS__MAPMEM 1		 /* Mapping best to reduce memory usage?    */

#elif defined( _mmap) || HAVE_MMAP
                                 /* UNIX implementation using mmap:	    */
#define HDS__CANMAP 1		 /* File mapping supported?		    */
#define HDS__MAPSEQ 1		 /* Mapping best for sequential access?	    */
#define HDS__MAPSPARSE 1	 /* Mapping best for sparse access?	    */
#define HDS__MAPMEM 1		 /* Mapping best to reduce memory usage?    */

#else				 /* Everything else: assume no file mapping */
#define HDS__CANMAP 0		 /* File mapping supported?		    */
#define HDS__MAPSEQ 1		 /* Mapping best for sequential access?	    */
#define HDS__MAPSPARSE 1	 /* Mapping best for sparse access?	    */
#define HDS__MAPMEM 1		 /* Mapping best to reduce memory usage?    */
#endif

/* UNIX shells:								    */
/* ===========								    */
/* Note: these numbers correspond to documented tuning parameter values, so */
/* should not be changed.						    */
#define HDS__NOSHELL -1		 /* Don't use a shell to expand file names  */
#define HDS__SHSHELL 0		 /* Use the "sh" shell			    */
#define HDS__CSHSHELL 1		 /* Use the "csh" shell			    */
#define HDS__TCSHSHELL 2	 /* Use the "tcsh" shell		    */

#define HDS__MXSHELL 2		 /* Max. UNIX shell number supported	    */

/* Define default command names for activating each UNIX shell (may be	    */
/* over-ridden by external definitions appropriate to a particular system). */
#if !defined( _sh )
#define _sh "sh"
#endif

#if !defined( _csh )
#define _csh "csh"
#endif

#if !defined( _tcsh )
#define _tcsh "tcsh"
#endif

/* Forking function:							    */
/* ================							    */
/* Use vfork for more efficient child process creation if supported,	    */
/* otherwise use the POSIX.1 fork function.				    */
#if defined( _vfork ) || HAVE_WORKING_VFORK
#define _fork vfork
#else
#define _fork fork
#endif

/* Defaults for tuning parameters:					    */
/* ==============================					    */
#define HDS__INALQ 2		 /* Initial file allocation quantity	    */
#define HDS__MAP 1		 /* Use file mapping if available?	    */
#define HDS__MAXWPL 32		 /* Maximum size of the "working page list" */
#define HDS__NBLOCKS 32		 /* Size of the internal "transfer buffer"  */
#define HDS__NCOMP 6		 /* Optimum number of structure components  */
#define HDS__SHELL HDS__SHSHELL	 /* UNIX "sh" shell used for file expansion */
#define HDS__SYSLCK 0		 /* System wide lock flag		    */
#define HDS__WAIT 0		 /* Wait for locked files?		    */
#define HDS__64BIT 0		 /* Create new files in 64-bit HDS format?  */

/* Arithmetic data types:						    */
/* =====================						    */

/* Define a "normal integer". Typically use the "int" type, but ensure it   */
/* has at least 4 bytes of precision.					    */
#if ( ( INT_MIN <= -2147483647L ) && ( INT_MAX >= 2147483647L ) )
#define INT int
#else
#define INT long int
#endif

/* Definitions for 64-bits integers                                        */ 
/* Use 'long' if it is 8 bytes, else use 'long long'                       */
#if SIZEOF_LONG == 8
typedef long int INT_BIG;
typedef unsigned long int UINT_BIG;
#define INT_BIG_S "ld"
#define INT_BIG_U "lu"
#elif SIZEOF_LONG_LONG == 8
typedef long long int INT_BIG;
typedef unsigned long long int UINT_BIG;
#define INT_BIG_S "lld"
#define INT_BIG_U "llu"
#else
error unable to find an 8 byte integer type
#endif

/* The Fortran interface can be compiled as either 32 or 64 bits depending */
/* on a switch definition                                                  */
#if !defined(HDS_F_64)
#define FORTRAN_INDEX_TYPE F77_INTEGER_TYPE
#else
#define FORTRAN_INDEX_TYPE INT_BIG
#endif

/* The internal size of array dimensions within HDS can be either 32 or 64  */
/* bits. Note that 64 bits is untested and WILL change the file format!!!   */
#if !defined(HDS_64)
typedef int HDS_PTYPE;
#define HDS_PTYPE_FORMAT "d"
#else
typedef INT_BIG HDS_PTYPE;
#define HDS_PTYPE_FORMAT INT_BIG_S
#endif

/* Global Structure Definitions:					    */
/* ============================						    */
/* DSC - Fixed length character string descriptor.			    */
      struct DSC
      {
         unsigned short int length; /* Object length			    */
         unsigned char dtype;	 /* Object type				    */
         unsigned char class;	 /* Object class			    */
         unsigned char *body;	 /* Pointer to object			    */
      };

/* Macros:								    */
/* ======								    */
/* Minimum and maximum.							    */
#define _min( i, j ) ( ( i ) < ( j ) ? ( i ) : ( j ) )
#define _max( i, j ) ( ( i ) > ( j ) ? ( i ) : ( j ) )

/* Call procedure, exiting if get bad status.				    */
#define _invoke(proc)\
		{\
		proc;\
		if (!_ok(hds_gl_status)) return hds_gl_status;\
		}

/* Test whether status is OK.						    */
#define _ok(status)\
		(status == DAT__OK)

/* Insert an entry at the head of a queue. (Here, a queue is a circular	    */
/* list of structures with forward and backward links given by components   */
/* flink and blink.) The variable queue should initially contain a pointer  */
/* to the element designated as the head-of-queue (or NULL if the queue is  */
/* empty) it is updated to point at the new element.			    */
#define _insque( entry, queue )\
        {\
           if ( (queue) == NULL )\
	   {\
              (entry)->flink = (entry);\
	      (entry)->blink = (entry);\
	   }\
	   else\
	   {\
	      (entry)->flink = (queue);\
	      (entry)->blink = (queue)->blink;\
	      ((queue)->blink)->flink = (entry);\
	      (queue)->blink = (entry);\
	   }\
	   (queue) = (entry);\
	}

/* Remove an entry from a queue.  The variable queue should initially	    */
/* contain a pointer to the element designated as the head-of-queue - it    */
/* will be updated if necessary, and will be set to NULL if the queue	    */
/* becomes empty.							    */
#define _remque( entry, queue )\
        {\
	   if ( (entry)->flink == (entry) )\
	   {\
	      (queue) = NULL;\
	   }\
	   else\
	   {\
	      ((entry)->blink)->flink = (entry)->flink;\
	      ((entry)->flink)->blink = (entry)->blink;\
	      if ( (entry) == (queue) ) (queue) = (entry)->flink;\
	   }\
	}

/* Move chars.								    */
#define _chmove( n, sptr, dptr )\
	        ( (void) memcpy( (void *) ( dptr ), (const void *) ( sptr ),\
		                 (size_t) ( n ) ) )

/* Move chars with fill.						    */
#define _chcopy( slen, sptr, fill, dlen, dptr )\
	        ( memcpy( (void *) ( dptr ), (const void *) ( sptr ),\
                          ( slen ) < ( dlen ) ? ( slen ) : ( dlen ) ),\
	          ( slen ) < ( dlen ) ?\
	          memset( (void *) ( dptr + slen ), ( fill ),\
		          ( dlen ) - ( slen ) ) : 0 )

/* Test char arrays for equality.					    */
#define _cheql( n, sptr, dptr )\
    	       ( !memcmp( (const void *) ( sptr ), (const void *) ( dptr ),\
	                  (size_t) ( n ) ) )

/* Initialise a character string data descriptor.			    */
#if defined( vms )
#define _dscinit(_name,_length,_body)\
	((_name)->length = _length,\
	 (_name)->dtype = DSC$K_DTYPE_T,\
	 (_name)->class = DSC$K_CLASS_S,\
	 (_name)->body = (void *)_body)
#else
#define _dscinit(_name,_length,_body)\
	((_name)->length = _length,\
	 (_name)->dtype = 0,\
	 (_name)->class = 0,\
	 (_name)->body = (void *)_body)
#endif

/* Global Variables:							    */
/* ================							    */
      extern int hds_gl_active;	 /* HDS active?				    */
      extern int hds_gl_ntemp;	 /* Counter for temporary names		    */
      extern int hds_gl_status;	 /* Global status			    */
      extern unsigned int hds_gl_locseq; /* Locator sequence number	    */
      extern int hds_gl_64bit;   /* V4 (64-bit) type records                */

/* Global Tuning Variables:						    */
/* =======================						    */
      extern int hds_gl_inalq0;	 /* Default initial file allocation quantity*/
      extern int hds_gl_inalq;	 /* Initial file allocation quantity	    */
      extern int hds_gl_map;	 /* Use file mapping if available?	    */
      extern int hds_gl_maxwpl;	 /* Maximum size of the "working page list" */
      extern int hds_gl_nblocks; /* Size of the internal "transfer buffer"  */
      extern int hds_gl_ncomp0;	 /* Default optimum number of components    */
      extern int hds_gl_ncomp;	 /* Optimum number of structure components  */
      extern int hds_gl_shell;	 /* UNIX shell used for file name expansion */
      extern int hds_gl_syslck;	 /* System-wide lock flag		    */
      extern int hds_gl_wait;	 /* Wait for locked files?		    */
      extern int hds_gl_c64bit;  /* Create 64-bit (HDS V4) new files?       */
      extern int hds_gl_longints;/* Default all INTEGERs to 64-bits?        */

/* Fixups for various machine deficiencies:				    */
/* =======================================				    */

#if !HAVE_STRERROR
      char *strerror( int );
#endif
#if !HAVE_ATEXIT
      int atexit( void (*fcn)( void ) );
#endif
#if !HAVE_MEMMOVE
      void *memmove( void *, const void *, size_t );
#endif

/*.									    */
#endif
