/* compress.h -- definitions for the decompression routines used in CFITSIO */

/* Blatantly copied and modified from the original gzip-1.2.4 source code.  */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#if defined(VAXC) || defined(VMS)
#  define RECORD_IO 1
#else
#  define RECORD_IO 0
#endif

#define get_char() get_byte()

/* gzip.h -- common declarations for all gzip modules  */

#define OF(args)  args
typedef void *voidp;

#define memzero(s, n)     memset ((voidp)(s), 0, (n))

typedef unsigned char  uch;
typedef unsigned short ush;
typedef unsigned long  ulg;

/* Return codes from gzip */
#define OK      0
#define ERROR   1
#define WARNING 2

/* Compression methods (see algorithm.doc) */
#define STORED      0
#define COMPRESSED  1
#define PACKED      2
#define LZHED       3
/* methods 4 to 7 reserved */
#define DEFLATED    8
#define MAX_METHODS 9

#define INBUFSIZ  0x8000    /* input buffer size */
#define INBUF_EXTRA  64     /* required by unlzw() */
#define OUTBUFSIZ  16384    /* output buffer size */
#define OUTBUF_EXTRA 2048   /* required by unlzw() */
#define DIST_BUFSIZE 0x8000 /* buffer for distances, see trees.c */
#define WSIZE 0x8000        /* window size--must be a power of two, and */

#define DECLARE(type, array, size)  type array[size]

#define tab_suffix window
#define tab_prefix prev    /* hash link (see deflate.c) */
#define head (prev+WSIZE)  /* hash head (see deflate.c) */

#define	PACK_MAGIC     "\037\036" /* Magic header for packed files */
#define	GZIP_MAGIC     "\037\213" /* Magic header for gzip files, 1F 8B */
#define	OLD_GZIP_MAGIC "\037\236" /* Magic header for gzip 0.5 = freeze 1.x */
#define	LZH_MAGIC      "\037\240" /* Magic header for SCO LZH Compress files*/
#define	LZW_MAGIC      "\037\235" /* Magic header for lzw files, 1F 9D */
#define PKZIP_MAGIC    "\120\113\003\004" /* Magic header for pkzip files */

/* gzip flag byte */
#define ASCII_FLAG   0x01 /* bit 0 set: file probably ascii text */
#define CONTINUATION 0x02 /* bit 1 set: continuation of multi-part gzip file */
#define EXTRA_FIELD  0x04 /* bit 2 set: extra field present */
#define ORIG_NAME    0x08 /* bit 3 set: original file name present */
#define COMMENT      0x10 /* bit 4 set: file comment present */
#define ENCRYPTED    0x20 /* bit 5 set: file is encrypted */
#define RESERVED     0xC0 /* bit 6,7:   reserved */

#define get_byte()  (inptr < insize ? inbuf[inptr++] : fill_inbuf(0))
#define try_byte()  (inptr < insize ? inbuf[inptr++] : fill_inbuf(1))
#define put_ubyte(c) {window[outcnt++]=(uch)(c); if (outcnt==WSIZE)\
   flush_window();}

/* Macros for getting two-byte and four-byte header values */
#define SH(p) ((ush)(uch)((p)[0]) | ((ush)(uch)((p)[1]) << 8))
#define LG(p) ((ulg)(SH(p)) | ((ulg)(SH((p)+2)) << 16))

/* Diagnostic functions */
#  define Assert(cond,msg)
#  define Trace(x)
#  define Tracev(x)
#  define Tracevv(x)
#  define Tracec(c,x)
#  define Tracecv(c,x)

/* lzw.h -- define the lzw functions. */

#ifndef BITS
#  define BITS 16
#endif
#define INIT_BITS 9              /* Initial number of bits per code */
#define BIT_MASK    0x1f /* Mask for 'number of compression bits' */
#define BLOCK_MODE  0x80
#define LZW_RESERVED 0x60 /* reserved bits */
#define	CLEAR  256       /* flush the dictionary */
#define FIRST  (CLEAR+1) /* first free entry */

/* prototypes */

#define local static
void ffpmsg(const char *err_message);

local int  get_method   OF((FILE *in));

local ulg  updcrc        OF((uch *s, unsigned n));
local int  fill_inbuf    OF((int eof_ok));
local void flush_outbuf  OF((void));
local void flush_window  OF((void));
local void write_buf     OF((voidp buf, unsigned cnt));
local void error         OF((char *m));

/* io.c */
local void fillbuf      OF((int n));
local unsigned getbits  OF((int n));
local void init_getbits OF((void));

/* maketbl.c */
local void make_table OF((int nchar, uch bitlen[],
			  int tablebits, ush table[]));

/* huf.c */
local void read_pt_len      OF((int nn, int nbit, int i_special));
local void read_c_len       OF((void));
local unsigned decode_c     OF((void));
local unsigned decode_p     OF((void));
local void huf_decode_start OF((void));

/* decode.c */
local void decode_start OF((void));
local unsigned  decode  OF((unsigned count, uch buffer[]));

local int unlzh      OF((FILE *in, FILE *out));
local int unlzw  OF((FILE *in, FILE *out));

local void read_tree  OF((void));
local void build_tree OF((void));

local int unpack     OF((FILE *in, FILE *out));
local int check_zipfile OF((FILE *in));
local int unzip      OF((FILE *in, FILE *out));

int (*work) OF((FILE *infile, FILE *outfile)) = unzip; /* function to call */

/* inflate.c */
struct huft {
  uch e;                /* number of extra bits or operation */
  uch b;                /* number of bits in this code or subcode */
  union {
    ush n;              /* literal, length base, or distance base */
    struct huft *t;     /* pointer to next level of table */
  } v;
};

local int huft_build OF((unsigned *, unsigned, unsigned, ush *, ush *,
                   struct huft **, int *));
local int huft_free OF((struct huft *));
local int inflate_codes OF((struct huft *, struct huft *, int, int));
local int inflate_stored OF((void));
local int inflate_fixed OF((void));
local int inflate_dynamic OF((void));
local int inflate_block OF((int *));
local int inflate OF((void));

/*  end of compress.h include file */
