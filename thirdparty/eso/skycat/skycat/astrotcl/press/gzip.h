/*+
************************************************************************
****  C A N A D I A N   A S T R O N O M Y   D A T A   C E N T R E  *****
*
*   Include File Name:	press/h/gzip.h
*
*   Purpose:
*	Common declarations for all gzip modules
*	Copyright (C) 1992-1993 Jean-loup Gailly.
*	This is free software; you can redistribute it and/or modify it 
*	under the terms of the GNU General Public License, see the file 
*	COPYING.
*
*   Date		: June 22, 1993
*
*   SCCS data		: @(#)
*	Module Name	: gzip.h
*	Version Number	: 1.2
*	Release Number	: 1
*	Last Updated	: 07/04/97
*
*   Programmer		: Norman Hill
*
*   Modification History:
*	97/07/02 SEC  :	Added function prototype for flush_window().
*	98/10/16 ESO  :	Allan Brighton: changed global names to avoid clash 
*                       with cfitsio and tclpro, which also use gzip sources.
*                       Changed "window" to "swindow", "prev" to "sprev".
*
****  C A N A D I A N   A S T R O N O M Y   D A T A   C E N T R E  *****
************************************************************************
-*/

#ifdef __STDC__
   typedef void *voidp;
#else
   typedef char *voidp;
#endif


#  define memzero(s, n)     memset ((voidp)(s), 0, (n))

/* 
 * Compression methods (see algorithm.doc) 
 */
#define DEFLATED    8
#define MAX_METHODS 9

#define DIST_BUFSIZE 0x8000 	/* buffer for distances, see trees.c	*/

unsigned short	*dbuf;		/* buffer for distances, see trees.c	*/
unsigned char	*swindow;	/* Sliding window and suffix table (unlzw) */
#define tab_suffix swindow
#define tab_prefix sprev	/* hash link (see deflate.c) 		*/
#define head (sprev+WSIZE)	/* hash head (see deflate.c) 		*/
unsigned short	*tab_prefix;	/* prefix code (see unlzw.c)		*/

extern unsigned insize;		/* valid bytes in inbuf */
extern unsigned inptr;  /* index of next byte to be processed in inbuf */
extern unsigned outcnt; /* bytes in output buffer */

extern long bytes_in;   /* number of input bytes */
extern long bytes_out;  /* number of output bytes */
extern long header_bytes;/* number of bytes in gzip header */

#define isize bytes_in
/* for compatibility with old zip sources (to be cleaned) */

extern long time_stamp; /* original time stamp (modification time) */
extern long ifile_size; /* input file size, -1 for devices (debug only) */

typedef int file_t;     /* Do not use stdio */
#define NO_FILE  (-1)   /* in memory compression */


#define	GZIP_MAGIC     "\037\213" /* Magic header for gzip files, 1F 8B */


/* 
 * gzip flag byte 
 */

#define ASCII_FLAG   0x01 /* bit 0 set: file probably ascii text */
#define CONTINUATION 0x02 /* bit 1 set: continuation of multi-part gzip file */
#define EXTRA_FIELD  0x04 /* bit 2 set: extra field present */
#define ORIG_NAME    0x08 /* bit 3 set: original file name present */
#define COMMENT      0x10 /* bit 4 set: file comment present */
#define ENCRYPTED    0x20 /* bit 5 set: file is encrypted */
#define RESERVED     0xC0 /* bit 6,7:   reserved */

/* internal file attribute */
#define UNKNOWN 0xffff
#define BINARY  0
#define ASCII   1

#ifndef WSIZE
#  define WSIZE 0x8000     /* window size--must be a power of two, and */
#endif                     /*  at least 32K for zip's deflate method */

#define MIN_MATCH  3
#define MAX_MATCH  258
/* The minimum and maximum match lengths */

#define MIN_LOOKAHEAD (MAX_MATCH+MIN_MATCH+1)
/* Minimum amount of lookahead, except at the end of the input file.
 * See deflate.c for comments about the MIN_MATCH+1.
 */

#define MAX_DIST  (WSIZE-MIN_LOOKAHEAD)
/* In order to simplify the code, particularly on 16 bit machines, match
 * distances are limited to MAX_DIST instead of WSIZE.
 */

extern int decrypt;        /* flag to turn on decryption */
extern int exit_code;      /* program exit code */
extern int verbose;        /* be verbose (-v) */
extern int quiet;          /* be quiet (-q) */
extern int level;          /* compression level */
extern int test;           /* check .z file integrity */
extern int to_stdout;      /* output to stdout (-c) */
extern int save_orig_name; /* set if original name must be saved */

extern	pfi	char_in;	/* Function to read bytes.		*/
extern	pfi	char_out;	/* function to write bytes.		*/


/* put_byte is used for the compressed output, put_ubyte for the
 * uncompressed output. However unlzw() uses window for its
 * suffix table instead of its output buffer, so it does not use put_ubyte
 * (to be cleaned up).
 */


/* Output a 16 bit value, lsb first */

#define seekable()    0  /* force sequential output */
#define translate_eol 0  /* no option -a yet */

#define tolow(c)  (isupper(c) ? (c)-'A'+'a' : (c))    /* force to lower case */

/* Macros for getting two-byte and four-byte header values */
#define SH(p) ((unsigned short)(byte)((p)[0]) | \
		((unsigned short)(byte)((p)[1]) << 8))
#define LG(p) ((unsigned long)(SH(p)) | ((unsigned long)(SH((p)+2)) << 16))

#ifdef XXXDEBUG
#  define Tracevv(x) {if (verbose>1) fprintf x ;}
#  define Tracecv(c,x) {if (verbose>1 && (c)) fprintf x ;}
#else
#  define Tracevv(x)
#  define Tracecv(c,x)
#endif


/*
 *  Function prototypes. 
 */ 

extern	int	flush_window( void );


