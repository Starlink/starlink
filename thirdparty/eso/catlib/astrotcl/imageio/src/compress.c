#include "compress.h"
local void error         OF((char *m));

/* Blatantly copied and modified from the original gzip-1.2.4 source code.  */

/* gzip (GNU zip) -- compress files with zip algorithm and 'compress' interface
 * Copyright (C) 1992-1993 Jean-loup Gailly
 * The unzip code was written and put in the public domain by Mark Adler.
 * Portions of the lzw code are derived from the public domain 'compress'
 * written by Spencer Thomas, Joe Orost, James Woods, Jim McKie, Steve Davies,
 * Ken Turkowski, Dave Mack and Peter Jannesen.
 *
 * See the license_msg below and the file COPYING for the software license.
 * See the file algorithm.doc for the compression algorithms and file formats.
 */

/* unlzh.c -- decompress files in SCO compress -H (LZH) format.
 * The code in this file is directly derived from the public domain 'ar002'
 * written by Haruhiko Okumura.
 */

		/* global buffers */

DECLARE(uch, inbuf,  INBUFSIZ +INBUF_EXTRA);
DECLARE(uch, outbuf, OUTBUFSIZ+OUTBUF_EXTRA);
DECLARE(ush, d_buf,  DIST_BUFSIZE);
DECLARE(uch, window, 2L*WSIZE);

#ifndef MAXSEG_64K
    DECLARE(ush, tab_prefix, 1L<<BITS);
#else
    DECLARE(ush, tab_prefix0, 1L<<(BITS-1));
    DECLARE(ush, tab_prefix1, 1L<<(BITS-1));
#endif

static long header_bytes;   /* number of bytes in gzip header */

		/* local variables */

/* 11/25/98: added 'static' to local variable definitions, to avoid */
/* conflict with external source files */

static int force = 0;        /* don't ask questions, compress links (-f) */
static int maxbits = BITS;   /* max bits per code for LZW */
static int method = DEFLATED;/* compression method */
/* static int level = 6;  */      /* compression level */
static int exit_code = OK;   /* program exit code */
static int last_member;      /* set for .zip and .Z files */
static int part_nb;          /* number of parts in .gz file */

static long bytes_in;             /* number of input bytes */
static long bytes_out;            /* number of output bytes */
static char ifname[128];          /* input file name */

static FILE *ifd;               /* input file descriptor */
static FILE *ofd;               /* output file descriptor */

static void **memptr;          /* memory location for uncompressed file */
static size_t *memsize;        /* size (bytes) of memory allocated for file */
void *(*realloc_fn)(void *p, size_t newsize);  /* reallocation function */

static unsigned insize;     /* valid bytes in inbuf */
static unsigned inptr;      /* index of next byte to be processed in inbuf */
static unsigned outcnt;     /* bytes in output buffer */

/* prototype for the following functions */
int uncompress2mem(char *filename, 
             FILE *diskfile, 
             char **buffptr, 
             size_t *buffsize, 
             void *(*mem_realloc)(void *p, size_t newsize),
             size_t *filesize,
             int *status);

int uncompress2file(char *filename, 
             FILE *indiskfile, 
             FILE *outdiskfile, 
             int *status);

/*--------------------------------------------------------------------------*/
int uncompress2mem(char *filename,  /* name of input file                 */
             FILE *diskfile,     /* I - file pointer                        */
             char **buffptr,   /* IO - memory pointer                     */
             size_t *buffsize,   /* IO - size of buffer, in bytes           */
             void *(*mem_realloc)(void *p, size_t newsize), /* function     */
             size_t *filesize,   /* O - size of file, in bytes              */
             int *status)        /* IO - error status                       */

/*
  Uncompress the file into memory.  Fill whatever amount of memory has
  already been allocated, then realloc more memory, using the supplied
  input function, if necessary.
*/
{
    if (*status > 0)
        return(*status);

    /*  save input parameters into global variables */
    strcpy(ifname, filename);
    ifd = diskfile;
    memptr = (void **) buffptr;
    memsize = buffsize;
    realloc_fn = mem_realloc;

    /* clear input and output buffers */

    outcnt = 0;
    insize = inptr = 0;
    bytes_in = bytes_out = 0L;

    part_nb = 0;

    method = get_method(ifd); 
    if (method < 0)
    {
	return(*status = 1);       /* error message already emitted */
    }

    /* Actually do the compression/decompression. Loop over zipped members.
     */
    for (;;) {
	if ((*work)(ifd, ofd) != OK) {
	    method = -1; /* force cleanup */
	    break;
	}
	if (last_member || inptr == insize) break;
	/* end of file */

	method = get_method(ifd);
	if (method < 0) break;    /* error message already emitted */
	bytes_out = 0;            /* required for length check */
    }

    *buffptr = *memptr;
    *buffsize = *memsize;
    *filesize = bytes_out;

    return(*status);
}
/*--------------------------------------------------------------------------*/
int uncompress2file(char *filename,  /* name of input file                  */
             FILE *indiskfile,     /* I - input file pointer                */
             FILE *outdiskfile,    /* I - output file pointer               */
             int *status)        /* IO - error status                       */

/*
  Uncompress the file into file. 
*/
{
    if (*status > 0)
        return(*status);

    /*  save input parameters into global variables */
    strcpy(ifname, filename);
    ifd = indiskfile;
    ofd = outdiskfile;
    realloc_fn = NULL; /* a null reallocation fn signals that the file is */
                       /* to be uncompressed to a file on disk, not memory */

    /* clear input and output buffers */
    outcnt = 0;
    insize = inptr = 0;
    bytes_in = bytes_out = 0L;

    part_nb = 0;

    method = get_method(ifd); 
    if (method < 0)
    {
	return(*status = 1);       /* error message already emitted */
    }

    /* Actually do the compression/decompression. Loop over zipped members.
     */
    for (;;) {
	if ((*work)(ifd, ofd) != OK) {
	    method = -1; /* force cleanup */
	    break;
	}
	if (last_member || inptr == insize) break;
	/* end of file */

	method = get_method(ifd);
	if (method < 0) break;    /* error message already emitted */
	bytes_out = 0;            /* required for length check */
    }

    return(*status);
}

/* ========================================================================
 * Check the magic number of the input file and update ofname if an
 * original name was given and to_stdout is not set.
 * Return the compression method, -1 for error, -2 for warning.
 * Set inptr to the offset of the next byte to be processed.
 * Updates time_stamp if there is one and --no-time is not used.
 * This function may be called repeatedly for an input file consisting
 * of several contiguous gzip'ed members.
 * IN assertions: there is at least one remaining compressed member.
 *   If the member is a zip file, it must be the only one.
 */
local int get_method(in)
    FILE *in;      /* input file descriptor */
{
    uch flags;     /* compression flags */
    char magic[2]; /* magic header */

    magic[0] = (char)get_byte();
    magic[1] = (char)get_byte();

    method = -1;                 /* unknown yet */
    part_nb++;                   /* number of parts in gzip file */
    header_bytes = 0;
    last_member = RECORD_IO;
    /* assume multiple members in gzip file except for record oriented I/O */

    if (memcmp(magic, GZIP_MAGIC, 2) == 0
        || memcmp(magic, OLD_GZIP_MAGIC, 2) == 0) {


	method = (int)get_byte();
	if (method != DEFLATED) {
	    error("unknown compression method -- get newer version of gzip");
	    exit_code = ERROR;
	    return -1;
	}
	work = unzip;
	flags  = (uch)get_byte();

	if ((flags & ENCRYPTED) != 0) {
	    error("input file is encrypted -- get newer version of gzip");
	    exit_code = ERROR;
	    return -1;
	}
	if ((flags & CONTINUATION) != 0) {
	    error(
	   "file is a a multi-part gzip file -- get newer version of gzip");
	    exit_code = ERROR;
	    if (force <= 1) return -1;
	}
	if ((flags & RESERVED) != 0) {
	    error("file has flags 0x%x -- get newer version of gzip");
	    exit_code = ERROR;
	    if (force <= 1) return -1;
	}
	(void)get_byte();  /* Ignore time stamp */
	(void)get_byte();
	(void)get_byte();
	(void)get_byte();

	(void)get_byte();  /* Ignore extra flags for the moment */
	(void)get_byte();  /* Ignore OS type for the moment */

	if ((flags & CONTINUATION) != 0) {
	    (void)get_byte();
	    (void)get_byte();
	}

	if ((flags & EXTRA_FIELD) != 0) {
	    unsigned len = (unsigned)get_byte();
	    len |= ((unsigned)get_byte())<<8;
	    while (len--) (void)get_byte();
	}

	/* Get original file name if it was truncated */
	if ((flags & ORIG_NAME) != 0) {
		/* Discard the old name */
		char c; /* dummy used for NeXTstep 3.0 cc optimizer bug */
		do {c=get_byte();} while (c != 0);
	} /* ORIG_NAME */

	/* Discard file comment if any */
	if ((flags & COMMENT) != 0) {
	    while (get_char() != 0) /* null */ ;
	}
	if (part_nb == 1) {
	    header_bytes = inptr + 2*sizeof(long); /* include crc and size */
	}

    } else if (memcmp(magic, PKZIP_MAGIC, 2) == 0 && inptr == 2
	    && memcmp((char*)inbuf, PKZIP_MAGIC, 4) == 0) {
	/* To simplify the code, we support a zip file when alone only.
         * We are thus guaranteed that the entire local header fits in inbuf.
         */
        inptr = 0;
	work = unzip;
	if (check_zipfile(in) != OK) return -1;
	/* check_zipfile may get ofname from the local header */
	last_member = 1;

    } else if (memcmp(magic, PACK_MAGIC, 2) == 0) {
	work = unpack;
	method = PACKED;

    } else if (memcmp(magic, LZW_MAGIC, 2) == 0) {
	work = unlzw;
	method = COMPRESSED;
	last_member = 1;

    } else if (memcmp(magic, LZH_MAGIC, 2) == 0) {
	work = unlzh;
	method = LZHED;
	last_member = 1;

    }
    if (method >= 0) return method;

    if (part_nb == 1) {
	error("file not in gzip format:");
	exit_code = ERROR;
	return -1;
    } else {
	/* decompression OK, trailing garbage ignored  */
	return -2;
    }
}

/* ========================================================================*/
/* this marks the start of the code that was originally in the  */
/* gzip source file called 'util.c'                             */

/* util.c -- utility functions for gzip support
 * Copyright (C) 1992-1993 Jean-loup Gailly
 * This is free software; you can redistribute it and/or modify it under the
 * terms of the GNU General Public License, see the file COPYING.
 */

extern ulg crc_32_tab[];   /* crc table, defined below */

/* ===========================================================================
 * Run a set of bytes through the crc shift register.  If s is a NULL
 * pointer, then initialize the crc shift register contents instead.
 * Return the current crc in either case.
 */
local ulg updcrc(s, n)
    uch *s;                 /* pointer to bytes to pump through */
    unsigned n;             /* number of bytes in s[] */
{
    register ulg c;         /* temporary variable */

    static ulg crc = (ulg)0xffffffffL; /* shift register contents */

    if (s == NULL) {
	c = 0xffffffffUL;
    } else {
	c = crc;
        if (n) do {
            c = crc_32_tab[((int)c ^ (*s++)) & 0xff] ^ (c >> 8);
        } while (--n);
    }
    crc = c;
    return c ^ 0xffffffffUL;       /* (instead of ~c for 64-bit machines) */
}

/* ===========================================================================
 * Fill the input buffer. This is called only when the buffer is empty.
 */
local int fill_inbuf(eof_ok)
    int eof_ok;          /* set if EOF acceptable as a result */
{
    int len;

    /* Read as much as possible */
    insize = 0;
    do {

/*	len = read(ifd, (char*)inbuf+insize, INBUFSIZ-insize); */
        len = fread((char*)inbuf+insize, 1, INBUFSIZ-insize, ifd);
        if (len == 0 || len == EOF) break;
	insize += len;
    } while (insize < INBUFSIZ);

    if (insize == 0) {
	if (eof_ok) return EOF;
	error("unexpected end of file");
        exit_code = ERROR;
        return ERROR;
    }
    bytes_in += (ulg)insize;
    inptr = 1;
    return inbuf[0];
}

/* ===========================================================================
 * Write the output buffer outbuf[0..outcnt-1] and update bytes_out.
 * (used for the compressed data only)
 */
local void flush_outbuf()
{
    if (outcnt == 0) return;

    write_buf((char *)outbuf, outcnt);
    bytes_out += (ulg)outcnt;
    outcnt = 0;
}

/* ===========================================================================
 * Write the output window window[0..outcnt-1] and update crc and bytes_out.
 * (Used for the decompressed data only.)
 */
local void flush_window()
{
    if (exit_code != OK) return;

    if (outcnt == 0) return;
    updcrc(window, outcnt);

    write_buf((char *)window, outcnt);

    bytes_out += (ulg)outcnt;
    outcnt = 0;
}

/* ===========================================================================
 * copy buffer into memory; allocate more memory if required
 */
local void write_buf(buf, cnt)
    voidp     buf;
    unsigned  cnt;
{
    if (!realloc_fn)
    {
      /* append buffer to file */
      /* added 'unsigned' to get rid of compiler warning (WDP 1/1/99) */
      if ((unsigned long) fwrite(buf, 1, cnt, ofd) != cnt)
      {
          error
          ("failed to write buffer to uncompressed output file (write_buf)");
          exit_code = ERROR;
          return;
      }
    }
    else
    {
      /* copy/append buffer into memory */

      /* get more memory if current buffer is too small */
      if (bytes_out + cnt > *memsize)
      {
        *memptr = realloc_fn(*memptr, bytes_out + cnt);
        *memsize = bytes_out + cnt;  /* new memory buffer size */
      }

      if (!(*memptr))
      {
            error("malloc failed while uncompressing (write_buf)");
            exit_code = ERROR;
            return;
      }

      /* copy  into memory buffer */
      memcpy((char *) *memptr + bytes_out, (char *) buf, cnt);
    }
}

/* ========================================================================
 * Error handlers.
 */

local void error(m)
    char *m;
{
    ffpmsg(ifname);
    ffpmsg(m);
}
/* ========================================================================
 * Table of CRC-32's of all single-byte values (made by makecrc.c)
 */
ulg crc_32_tab[] = {
  0x00000000L, 0x77073096L, 0xee0e612cL, 0x990951baL, 0x076dc419L,
  0x706af48fL, 0xe963a535L, 0x9e6495a3L, 0x0edb8832L, 0x79dcb8a4L,
  0xe0d5e91eL, 0x97d2d988L, 0x09b64c2bL, 0x7eb17cbdL, 0xe7b82d07L,
  0x90bf1d91L, 0x1db71064L, 0x6ab020f2L, 0xf3b97148L, 0x84be41deL,
  0x1adad47dL, 0x6ddde4ebL, 0xf4d4b551L, 0x83d385c7L, 0x136c9856L,
  0x646ba8c0L, 0xfd62f97aL, 0x8a65c9ecL, 0x14015c4fL, 0x63066cd9L,
  0xfa0f3d63L, 0x8d080df5L, 0x3b6e20c8L, 0x4c69105eL, 0xd56041e4L,
  0xa2677172L, 0x3c03e4d1L, 0x4b04d447L, 0xd20d85fdL, 0xa50ab56bL,
  0x35b5a8faL, 0x42b2986cL, 0xdbbbc9d6L, 0xacbcf940L, 0x32d86ce3L,
  0x45df5c75L, 0xdcd60dcfL, 0xabd13d59L, 0x26d930acL, 0x51de003aL,
  0xc8d75180L, 0xbfd06116L, 0x21b4f4b5L, 0x56b3c423L, 0xcfba9599L,
  0xb8bda50fL, 0x2802b89eL, 0x5f058808L, 0xc60cd9b2L, 0xb10be924L,
  0x2f6f7c87L, 0x58684c11L, 0xc1611dabL, 0xb6662d3dL, 0x76dc4190L,
  0x01db7106L, 0x98d220bcL, 0xefd5102aL, 0x71b18589L, 0x06b6b51fL,
  0x9fbfe4a5L, 0xe8b8d433L, 0x7807c9a2L, 0x0f00f934L, 0x9609a88eL,
  0xe10e9818L, 0x7f6a0dbbL, 0x086d3d2dL, 0x91646c97L, 0xe6635c01L,
  0x6b6b51f4L, 0x1c6c6162L, 0x856530d8L, 0xf262004eL, 0x6c0695edL,
  0x1b01a57bL, 0x8208f4c1L, 0xf50fc457L, 0x65b0d9c6L, 0x12b7e950L,
  0x8bbeb8eaL, 0xfcb9887cL, 0x62dd1ddfL, 0x15da2d49L, 0x8cd37cf3L,
  0xfbd44c65L, 0x4db26158L, 0x3ab551ceL, 0xa3bc0074L, 0xd4bb30e2L,
  0x4adfa541L, 0x3dd895d7L, 0xa4d1c46dL, 0xd3d6f4fbL, 0x4369e96aL,
  0x346ed9fcL, 0xad678846L, 0xda60b8d0L, 0x44042d73L, 0x33031de5L,
  0xaa0a4c5fL, 0xdd0d7cc9L, 0x5005713cL, 0x270241aaL, 0xbe0b1010L,
  0xc90c2086L, 0x5768b525L, 0x206f85b3L, 0xb966d409L, 0xce61e49fL,
  0x5edef90eL, 0x29d9c998L, 0xb0d09822L, 0xc7d7a8b4L, 0x59b33d17L,
  0x2eb40d81L, 0xb7bd5c3bL, 0xc0ba6cadL, 0xedb88320L, 0x9abfb3b6L,
  0x03b6e20cL, 0x74b1d29aL, 0xead54739L, 0x9dd277afL, 0x04db2615L,
  0x73dc1683L, 0xe3630b12L, 0x94643b84L, 0x0d6d6a3eL, 0x7a6a5aa8L,
  0xe40ecf0bL, 0x9309ff9dL, 0x0a00ae27L, 0x7d079eb1L, 0xf00f9344L,
  0x8708a3d2L, 0x1e01f268L, 0x6906c2feL, 0xf762575dL, 0x806567cbL,
  0x196c3671L, 0x6e6b06e7L, 0xfed41b76L, 0x89d32be0L, 0x10da7a5aL,
  0x67dd4accL, 0xf9b9df6fL, 0x8ebeeff9L, 0x17b7be43L, 0x60b08ed5L,
  0xd6d6a3e8L, 0xa1d1937eL, 0x38d8c2c4L, 0x4fdff252L, 0xd1bb67f1L,
  0xa6bc5767L, 0x3fb506ddL, 0x48b2364bL, 0xd80d2bdaL, 0xaf0a1b4cL,
  0x36034af6L, 0x41047a60L, 0xdf60efc3L, 0xa867df55L, 0x316e8eefL,
  0x4669be79L, 0xcb61b38cL, 0xbc66831aL, 0x256fd2a0L, 0x5268e236L,
  0xcc0c7795L, 0xbb0b4703L, 0x220216b9L, 0x5505262fL, 0xc5ba3bbeL,
  0xb2bd0b28L, 0x2bb45a92L, 0x5cb36a04L, 0xc2d7ffa7L, 0xb5d0cf31L,
  0x2cd99e8bL, 0x5bdeae1dL, 0x9b64c2b0L, 0xec63f226L, 0x756aa39cL,
  0x026d930aL, 0x9c0906a9L, 0xeb0e363fL, 0x72076785L, 0x05005713L,
  0x95bf4a82L, 0xe2b87a14L, 0x7bb12baeL, 0x0cb61b38L, 0x92d28e9bL,
  0xe5d5be0dL, 0x7cdcefb7L, 0x0bdbdf21L, 0x86d3d2d4L, 0xf1d4e242L,
  0x68ddb3f8L, 0x1fda836eL, 0x81be16cdL, 0xf6b9265bL, 0x6fb077e1L,
  0x18b74777L, 0x88085ae6L, 0xff0f6a70L, 0x66063bcaL, 0x11010b5cL,
  0x8f659effL, 0xf862ae69L, 0x616bffd3L, 0x166ccf45L, 0xa00ae278L,
  0xd70dd2eeL, 0x4e048354L, 0x3903b3c2L, 0xa7672661L, 0xd06016f7L,
  0x4969474dL, 0x3e6e77dbL, 0xaed16a4aL, 0xd9d65adcL, 0x40df0b66L,
  0x37d83bf0L, 0xa9bcae53L, 0xdebb9ec5L, 0x47b2cf7fL, 0x30b5ffe9L,
  0xbdbdf21cL, 0xcabac28aL, 0x53b39330L, 0x24b4a3a6L, 0xbad03605L,
  0xcdd70693L, 0x54de5729L, 0x23d967bfL, 0xb3667a2eL, 0xc4614ab8L,
  0x5d681b02L, 0x2a6f2b94L, 0xb40bbe37L, 0xc30c8ea1L, 0x5a05df1bL,
  0x2d02ef8dL
};

#define DICBIT    13    /* 12(-lh4-) or 13(-lh5-) */
#define DICSIZ ((unsigned) 1 << DICBIT)

#ifndef CHAR_BIT
#  define CHAR_BIT 8
#endif

#ifndef UCHAR_MAX
#  define UCHAR_MAX 255
#endif

#define BITBUFSIZ (CHAR_BIT * 2 * sizeof(char))
/* Do not use CHAR_BIT * sizeof(bitbuf), does not work on machines
 * for which short is not on 16 bits (Cray).
 */

/* encode.c and decode.c */

#define MAXMATCH 256    /* formerly F (not more than UCHAR_MAX + 1) */
#define THRESHOLD  3    /* choose optimal value */

/* huf.c */

#define NC (UCHAR_MAX + MAXMATCH + 2 - THRESHOLD)
	/* alphabet = {0, 1, 2, ..., NC - 1} */
#define CBIT 9  /* $\lfloor \log_2 NC \rfloor + 1$ */
#define CODE_BIT  16  /* codeword length */

#define NP (DICBIT + 1)
#define NT (CODE_BIT + 3)
#define PBIT 4  /* smallest integer such that (1U << PBIT) > NP */
#define TBIT 5  /* smallest integer such that (1U << TBIT) > NT */
#if NT > NP
# define NPT NT
#else
# define NPT NP
#endif

/* local ush left[2 * NC - 1]; */
/* local ush right[2 * NC - 1]; */
#define left  prev
#define right head
#if NC > (1<<(BITS-2))
    error cannot overlay left+right and prev
#endif

/* local uch c_len[NC]; */
#define c_len outbuf
#if NC > OUTBUFSIZ
    error cannot overlay c_len and outbuf
#endif

local uch pt_len[NPT];
local unsigned blocksize;
local ush pt_table[256];

/* local ush c_table[4096]; */
#define c_table d_buf
#if (DIST_BUFSIZE-1) < 4095
    error cannot overlay c_table and d_buf
#endif

/***********************************************************
        io.c -- input/output
***********************************************************/

local ush       bitbuf;
local unsigned  subbitbuf;
local int       bitcount;

local void fillbuf(n)  /* Shift bitbuf n bits left, read n bits */
    int n;
{
    bitbuf <<= n;
    while (n > bitcount) {
	bitbuf |= subbitbuf << (n -= bitcount);
	subbitbuf = (unsigned)try_byte();
	if ((int)subbitbuf == EOF) subbitbuf = 0;
	bitcount = CHAR_BIT;
    }
    bitbuf |= subbitbuf >> (bitcount -= n);
}

local unsigned getbits(n)
    int n;
{
    unsigned x;

    x = bitbuf >> (BITBUFSIZ - n);  fillbuf(n);
    return x;
}

local void init_getbits()
{
    bitbuf = 0;  subbitbuf = 0;  bitcount = 0;
    fillbuf(BITBUFSIZ);
}

/***********************************************************
	maketbl.c -- make table for decoding
***********************************************************/

local void make_table(nchar, bitlen, tablebits, table)
    int nchar;
    uch bitlen[];
    int tablebits;
    ush table[];
{
    ush count[17], weight[17], start[18], *p;
    unsigned i, k, len, ch, jutbits, avail, nextcode, mask;

    for (i = 1; i <= 16; i++) count[i] = 0;
    for (i = 0; i < (unsigned)nchar; i++) count[bitlen[i]]++;

    start[1] = 0;
    for (i = 1; i <= 16; i++)
	start[i + 1] = start[i] + (count[i] << (16 - i));
    if ((start[17] & 0xffff) != 0)
    {
	error("Bad table\n");
        exit_code = ERROR;
        return;
    }

    jutbits = 16 - tablebits;
    for (i = 1; i <= (unsigned)tablebits; i++) {
	start[i] >>= jutbits;
	weight[i] = (unsigned) 1 << (tablebits - i);
    }
    while (i <= 16) {
	weight[i] = (unsigned) 1 << (16 - i);
	i++;
    }

    i = start[tablebits + 1] >> jutbits;
    if (i != 0) {
	k = 1 << tablebits;
	while (i != k) table[i++] = 0;
    }

    avail = nchar;
    mask = (unsigned) 1 << (15 - tablebits);
    for (ch = 0; ch < (unsigned)nchar; ch++) {
	if ((len = bitlen[ch]) == 0) continue;
	nextcode = start[len] + weight[len];
	if (len <= (unsigned)tablebits) {
	    for (i = start[len]; i < nextcode; i++) table[i] = ch;
	} else {
	    k = start[len];
	    p = &table[k >> jutbits];
	    i = len - tablebits;
	    while (i != 0) {
		if (*p == 0) {
		    right[avail] = left[avail] = 0;
		    *p = avail++;
		}
		if (k & mask) p = &right[*p];
		else          p = &left[*p];
		k <<= 1;  i--;
	    }
	    *p = ch;
	}
	start[len] = nextcode;
    }
}

/***********************************************************
        huf.c -- static Huffman
***********************************************************/

local void read_pt_len(nn, nbit, i_special)
    int nn;
    int nbit;
    int i_special;
{
    int i, c, n;
    unsigned mask;

    n = getbits(nbit);
    if (n == 0) {
	c = getbits(nbit);
	for (i = 0; i < nn; i++) pt_len[i] = 0;
	for (i = 0; i < 256; i++) pt_table[i] = c;
    } else {
	i = 0;
	while (i < n) {
	    c = bitbuf >> (BITBUFSIZ - 3);
	    if (c == 7) {
		mask = (unsigned) 1 << (BITBUFSIZ - 1 - 3);
		while (mask & bitbuf) {  mask >>= 1;  c++;  }
	    }
	    fillbuf((c < 7) ? 3 : c - 3);
	    pt_len[i++] = c;
	    if (i == i_special) {
		c = getbits(2);
		while (--c >= 0) pt_len[i++] = 0;
	    }
	}
	while (i < nn) pt_len[i++] = 0;
	make_table(nn, pt_len, 8, pt_table);
    }
}

local void read_c_len()
{
    int i, c, n;
    unsigned mask;

    n = getbits(CBIT);
    if (n == 0) {
	c = getbits(CBIT);
	for (i = 0; i < NC; i++) c_len[i] = 0;
	for (i = 0; i < 4096; i++) c_table[i] = c;
    } else {
	i = 0;
	while (i < n) {
	    c = pt_table[bitbuf >> (BITBUFSIZ - 8)];
	    if (c >= NT) {
		mask = (unsigned) 1 << (BITBUFSIZ - 1 - 8);
		do {
		    if (bitbuf & mask) c = right[c];
		    else               c = left [c];
		    mask >>= 1;
		} while (c >= NT);
	    }
	    fillbuf((int) pt_len[c]);
	    if (c <= 2) {
		if      (c == 0) c = 1;
		else if (c == 1) c = getbits(4) + 3;
		else             c = getbits(CBIT) + 20;
		while (--c >= 0) c_len[i++] = 0;
	    } else c_len[i++] = c - 2;
	}
	while (i < NC) c_len[i++] = 0;
	make_table(NC, c_len, 12, c_table);
    }
}

local unsigned decode_c()
{
    unsigned j = 0, mask;

    if (blocksize == 0) {
	blocksize = getbits(16);
	if (blocksize == 0) {
	    return NC; /* end of file */
	}
	read_pt_len(NT, TBIT, 3);
        if (exit_code != OK)
           return j;

	read_c_len();
        if (exit_code != OK)
           return j;

	read_pt_len(NP, PBIT, -1);
        if (exit_code != OK)
           return j;

    }
    blocksize--;
    j = c_table[bitbuf >> (BITBUFSIZ - 12)];
    if (j >= NC) {
	mask = (unsigned) 1 << (BITBUFSIZ - 1 - 12);
	do {
	    if (bitbuf & mask) j = right[j];
	    else               j = left [j];
	    mask >>= 1;
	} while (j >= NC);
    }
    fillbuf((int) c_len[j]);
    return j;
}

local unsigned decode_p()
{
    unsigned j, mask;

    j = pt_table[bitbuf >> (BITBUFSIZ - 8)];
    if (j >= NP) {
	mask = (unsigned) 1 << (BITBUFSIZ - 1 - 8);
	do {
	    if (bitbuf & mask) j = right[j];
	    else               j = left [j];
	    mask >>= 1;
	} while (j >= NP);
    }
    fillbuf((int) pt_len[j]);
    if (j != 0) j = ((unsigned) 1 << (j - 1)) + getbits((int) (j - 1));
    return j;
}

local void huf_decode_start()
{
    init_getbits();  blocksize = 0;
}

/***********************************************************
        decode.c
***********************************************************/
/* changed 'j' to 'jj1' to avoid conflicts  - WDP 1/1/99) */

local int jj1;    /* remaining bytes to copy */
local int done; /* set at end of input */

local void decode_start()
{
    huf_decode_start();
    jj1 = 0;
    done = 0;
}

/* Decode the input and return the number of decoded bytes put in buffer
 */
local unsigned decode(count, buffer)
    unsigned count;
    uch buffer[];
    /* The calling function must keep the number of
       bytes to be processed.  This function decodes
       either 'count' bytes or 'DICSIZ' bytes, whichever
       is smaller, into the array 'buffer[]' of size
       'DICSIZ' or more.
       Call decode_start() once for each new file
       before calling this function.
     */
{
    local unsigned i;
    unsigned r, c;

    r = 0;
    while (--jj1 >= 0) {
	buffer[r] = buffer[i];
	i = (i + 1) & (DICSIZ - 1);
	if (++r == count) return r;
    }
    for ( ; ; ) {
	c = decode_c();
        if (exit_code != OK)
           return r;

	if (c == NC) {
	    done = 1;
	    return r;
	}
	if (c <= UCHAR_MAX) {
	    buffer[r] = c;
	    if (++r == count) return r;
	} else {
	    jj1 = c - (UCHAR_MAX + 1 - THRESHOLD);
	    i = (r - decode_p() - 1) & (DICSIZ - 1);
	    while (--jj1 >= 0) {
		buffer[r] = buffer[i];
		i = (i + 1) & (DICSIZ - 1);
		if (++r == count) return r;
	    }
	}
    }
}


/* ===========================================================================
 * Unlzh in to out. Return OK or ERROR.
 */
local int unlzh(in, out)
    FILE *in;
    FILE *out;
{
    unsigned n;
    ifd = in;
    ofd = out;

    decode_start();
    while (!done) {
	n = decode((unsigned) DICSIZ, window);
        if (exit_code != OK)
            return ERROR;

	if (n > 0) {
	    write_buf((char*)window, n);
	}
    }
    return OK;
}

/*=========================================================================*/
/* this marks the begining of the original file 'unlzw.c'                  */
/*=========================================================================*/

/* unlzw.c -- decompress files in LZW format.
 * The code in this file is directly derived from the public domain 'compress'
 * written by Spencer Thomas, Joe Orost, James Woods, Jim McKie, Steve Davies,
 * Ken Turkowski, Dave Mack and Peter Jannesen.
 *
 * This is a temporary version which will be rewritten in some future version
 * to accommodate in-memory decompression.
 */


typedef	unsigned char	char_type;
typedef          long   code_int;
typedef unsigned long 	count_int;
typedef unsigned short	count_short;
typedef unsigned long 	cmp_code_int;

#define MAXCODE(n)	(1L << (n))
    
#ifndef	REGISTERS
#	define	REGISTERS	2
#endif
#define	REG1	
#define	REG2	
#define	REG3	
#define	REG4	
#define	REG5	
#define	REG6	
#define	REG7	
#define	REG8	
#define	REG9	
#define	REG10
#define	REG11	
#define	REG12	
#define	REG13
#define	REG14
#define	REG15
#define	REG16
#if REGISTERS >= 1
#	undef	REG1
#	define	REG1	register
#endif
#if REGISTERS >= 2
#	undef	REG2
#	define	REG2	register
#endif
#if REGISTERS >= 3
#	undef	REG3
#	define	REG3	register
#endif
#if REGISTERS >= 4
#	undef	REG4
#	define	REG4	register
#endif
#if REGISTERS >= 5
#	undef	REG5
#	define	REG5	register
#endif
#if REGISTERS >= 6
#	undef	REG6
#	define	REG6	register
#endif
#if REGISTERS >= 7
#	undef	REG7
#	define	REG7	register
#endif
#if REGISTERS >= 8
#	undef	REG8
#	define	REG8	register
#endif
#if REGISTERS >= 9
#	undef	REG9
#	define	REG9	register
#endif
#if REGISTERS >= 10
#	undef	REG10
#	define	REG10	register
#endif
#if REGISTERS >= 11
#	undef	REG11
#	define	REG11	register
#endif
#if REGISTERS >= 12
#	undef	REG12
#	define	REG12	register
#endif
#if REGISTERS >= 13
#	undef	REG13
#	define	REG13	register
#endif
#if REGISTERS >= 14
#	undef	REG14
#	define	REG14	register
#endif
#if REGISTERS >= 15
#	undef	REG15
#	define	REG15	register
#endif
#if REGISTERS >= 16
#	undef	REG16
#	define	REG16	register
#endif
    
#ifndef	BYTEORDER
#	define	BYTEORDER	0000
#endif
	
#ifndef	NOALLIGN
#	define	NOALLIGN	0
#endif


union	bytes {
    long  word;
    struct {
#if BYTEORDER == 4321
	char_type	b1;
	char_type	b2;
	char_type	b3;
	char_type	b4;
#else
#if BYTEORDER == 1234
	char_type	b4;
	char_type	b3;
	char_type	b2;
	char_type	b1;
#else
#	undef	BYTEORDER
	int  dummy;
#endif
#endif
    } bytes;
};

#if BYTEORDER == 4321 && NOALLIGN == 1
#  define input(b,o,c,n,m){ \
     (c) = (*(long *)(&(b)[(o)>>3])>>((o)&0x7))&(m); \
     (o) += (n); \
   }
#else
#  define input(b,o,c,n,m){ \
     REG1 char_type *p = &(b)[(o)>>3]; \
     (c) = ((((long)(p[0]))|((long)(p[1])<<8)| \
     ((long)(p[2])<<16))>>((o)&0x7))&(m); \
     (o) += (n); \
   }
#endif

#ifndef MAXSEG_64K
   /* DECLARE(ush, tab_prefix, (1<<BITS)); -- prefix code */
#  define tab_prefixof(i) tab_prefix[i]
#  define clear_tab_prefixof()	memzero(tab_prefix, 256);
#else
   /* DECLARE(ush, tab_prefix0, (1<<(BITS-1)); -- prefix for even codes */
   /* DECLARE(ush, tab_prefix1, (1<<(BITS-1)); -- prefix for odd  codes */
   ush *tab_prefix[2];
#  define tab_prefixof(i) tab_prefix[(i)&1][(i)>>1]
#  define clear_tab_prefixof()	\
      memzero(tab_prefix0, 128), \
      memzero(tab_prefix1, 128);
#endif
#define de_stack        ((char_type *)(&d_buf[DIST_BUFSIZE-1]))
#define tab_suffixof(i) tab_suffix[i]

int block_mode = BLOCK_MODE; /* block compress mode -C compatible with 2.0 */

/* ============================================================================
 * Decompress in to out.  This routine adapts to the codes in the
 * file building the "string" table on-the-fly; requiring no table to
 * be stored in the compressed file.
 * IN assertions: the buffer inbuf contains already the beginning of
 *   the compressed data, from offsets iptr to insize-1 included.
 *   The magic header has already been checked and skipped.
 *   bytes_in and bytes_out have been initialized.
 */
local int unlzw(in, out) 
    FILE *in, *out;    /* input and output file descriptors */
{
    REG2   char_type  *stackp;
    REG3   code_int   code;
    REG4   int        finchar;
    REG5   code_int   oldcode;
    REG6   code_int   incode;
    REG7   long       inbits;
    REG8   long       posbits;
    REG9   int        outpos;
/*  REG10  int        insize; (global) */
    REG11  unsigned   bitmask;
    REG12  code_int   free_ent;
    REG13  code_int   maxcode;
    REG14  code_int   maxmaxcode;
    REG15  int        n_bits;
    REG16  int        rsize;
    
    ofd = out;

#ifdef MAXSEG_64K
    tab_prefix[0] = tab_prefix0;
    tab_prefix[1] = tab_prefix1;
#endif
    maxbits = get_byte();
    block_mode = maxbits & BLOCK_MODE;
    if ((maxbits & LZW_RESERVED) != 0) {
	error( "warning, unknown flags in unlzw decompression");
    }
    maxbits &= BIT_MASK;
    maxmaxcode = MAXCODE(maxbits);
    
    if (maxbits > BITS) {
	error("compressed with too many bits; cannot handle file");
	exit_code = ERROR;
	return ERROR;
    }
    rsize = insize;
    maxcode = MAXCODE(n_bits = INIT_BITS)-1;
    bitmask = (1<<n_bits)-1;
    oldcode = -1;
    finchar = 0;
    outpos = 0;
    posbits = inptr<<3;

    free_ent = ((block_mode) ? FIRST : 256);
    
    clear_tab_prefixof(); /* Initialize the first 256 entries in the table. */
    
    for (code = 255 ; code >= 0 ; --code) {
	tab_suffixof(code) = (char_type)code;
    }
    do {
	REG1 int i;
	int  e;
	int  o;
	
    resetbuf:
	e = insize-(o = (posbits>>3));
	
	for (i = 0 ; i < e ; ++i) {
	    inbuf[i] = inbuf[i+o];
	}
	insize = e;
	posbits = 0;
	
	if (insize < INBUF_EXTRA) {
/*  modified to use fread instead of read - WDP 10/22/97  */
/*	    if ((rsize = read(in, (char*)inbuf+insize, INBUFSIZ)) == EOF) { */

	    if ((rsize = fread((char*)inbuf+insize, 1, INBUFSIZ, in)) == EOF) {
		error("unexpected end of file");
	        exit_code = ERROR;
                return ERROR;
	    }
	    insize += rsize;
	    bytes_in += (ulg)rsize;
	}
	inbits = ((rsize != 0) ? ((long)insize - insize%n_bits)<<3 : 
		  ((long)insize<<3)-(n_bits-1));
	
	while (inbits > posbits) {
	    if (free_ent > maxcode) {
		posbits = ((posbits-1) +
			   ((n_bits<<3)-(posbits-1+(n_bits<<3))%(n_bits<<3)));
		++n_bits;
		if (n_bits == maxbits) {
		    maxcode = maxmaxcode;
		} else {
		    maxcode = MAXCODE(n_bits)-1;
		}
		bitmask = (1<<n_bits)-1;
		goto resetbuf;
	    }
	    input(inbuf,posbits,code,n_bits,bitmask);
	    Tracev((stderr, "%d ", code));

	    if (oldcode == -1) {
		if (code >= 256) {
                    error("corrupt input.");
	            exit_code = ERROR;
                    return ERROR;
                }

		outbuf[outpos++] = (char_type)(finchar = (int)(oldcode=code));
		continue;
	    }
	    if (code == CLEAR && block_mode) {
		clear_tab_prefixof();
		free_ent = FIRST - 1;
		posbits = ((posbits-1) +
			   ((n_bits<<3)-(posbits-1+(n_bits<<3))%(n_bits<<3)));
		maxcode = MAXCODE(n_bits = INIT_BITS)-1;
		bitmask = (1<<n_bits)-1;
		goto resetbuf;
	    }
	    incode = code;
	    stackp = de_stack;
	    
	    if (code >= free_ent) { /* Special case for KwKwK string. */
		if (code > free_ent) {
		    if (outpos > 0) {
			write_buf((char*)outbuf, outpos);
			bytes_out += (ulg)outpos;
		    }
		    error("corrupt input.");
	            exit_code = ERROR;
                    return ERROR;

		}
		*--stackp = (char_type)finchar;
		code = oldcode;
	    }

	    while ((cmp_code_int)code >= (cmp_code_int)256) {
		/* Generate output characters in reverse order */
		*--stackp = tab_suffixof(code);
		code = tab_prefixof(code);
	    }
	    *--stackp =	(char_type)(finchar = tab_suffixof(code));
	    
	    /* And put them out in forward order */
	    {
	/*	REG1 int	i;   already defined above (WDP) */
	    
		if (outpos+(i = (de_stack-stackp)) >= OUTBUFSIZ) {
		    do {
			if (i > OUTBUFSIZ-outpos) i = OUTBUFSIZ-outpos;

			if (i > 0) {
			    memcpy(outbuf+outpos, stackp, i);
			    outpos += i;
			}
			if (outpos >= OUTBUFSIZ) {
			    write_buf((char*)outbuf, outpos);
			    bytes_out += (ulg)outpos;
			    outpos = 0;
			}
			stackp+= i;
		    } while ((i = (de_stack-stackp)) > 0);
		} else {
		    memcpy(outbuf+outpos, stackp, i);
		    outpos += i;
		}
	    }

	    if ((code = free_ent) < maxmaxcode) { /* Generate the new entry. */

		tab_prefixof(code) = (unsigned short)oldcode;
		tab_suffixof(code) = (char_type)finchar;
		free_ent = code+1;
	    } 
	    oldcode = incode;	/* Remember previous code.	*/
	}
    } while (rsize != 0);
    
    if (outpos > 0) {
	write_buf((char*)outbuf, outpos);
	bytes_out += (ulg)outpos;
    }
    return OK;
}

/*=========================================================================*/
/*  This marks the beginning of the original file 'unpack.c'               */
/*=========================================================================*/

/* unpack.c -- decompress files in pack format.
 * Copyright (C) 1992-1993 Jean-loup Gailly
 * This is free software; you can redistribute it and/or modify it under the
 * terms of the GNU General Public License, see the file COPYING.
 */

#define MIN(a,b) ((a) <= (b) ? (a) : (b))
/* The arguments must not have side effects. */

#define MAX_BITLEN 25
/* Maximum length of Huffman codes. (Minor modifications to the code
 * would be needed to support 32 bits codes, but pack never generates
 * more than 24 bits anyway.)
 */

#define LITERALS 256
/* Number of literals, excluding the End of Block (EOB) code */

#define MAX_PEEK 12
/* Maximum number of 'peek' bits used to optimize traversal of the
 * Huffman tree.
 */

local ulg orig_len;       /* original uncompressed length */
local int max_len;        /* maximum bit length of Huffman codes */

local uch literal[LITERALS];
/* The literal bytes present in the Huffman tree. The EOB code is not
 * represented.
 */

local int lit_base[MAX_BITLEN+1];
/* All literals of a given bit length are contiguous in literal[] and
 * have contiguous codes. literal[code+lit_base[len]] is the literal
 * for a code of len bits.
 */

local int leaves [MAX_BITLEN+1]; /* Number of leaves for each bit length */
local int parents[MAX_BITLEN+1]; /* Number of parents for each bit length */

local int peek_bits; /* Number of peek bits currently used */

/* local uch prefix_len[1 << MAX_PEEK]; */
#define prefix_len outbuf
/* For each bit pattern b of peek_bits bits, prefix_len[b] is the length
 * of the Huffman code starting with a prefix of b (upper bits), or 0
 * if all codes of prefix b have more than peek_bits bits. It is not
 * necessary to have a huge table (large MAX_PEEK) because most of the
 * codes encountered in the input stream are short codes (by construction).
 * So for most codes a single lookup will be necessary.
 */
#if (1<<MAX_PEEK) > OUTBUFSIZ
    error cannot overlay prefix_len and outbuf
#endif

local ulg bitbufulg;

/* Bits are added on the low part of bitbufulg and read from the high part. */

local int valid;                  /* number of valid bits in bitbufulg */
/* all bits above the last valid bit are always zero */

/* Set code to the next 'bits' input bits without skipping them. code
 * must be the name of a simple variable and bits must not have side effects.
 * IN assertions: bits <= 25 (so that we still have room for an extra byte
 * when valid is only 24), and mask = (1<<bits)-1.
 */
#define look_bits(code,bits,mask) \
{ \
  while (valid < (bits)) bitbufulg = (bitbufulg<<8) | (ulg)get_byte(), valid += 8; \
  code = (bitbufulg >> (valid-(bits))) & (mask); \
}

/* Skip the given number of bits (after having peeked at them): */
#define skip_bits(bits)  (valid -= (bits))

#define clear_bitbuf() (valid = 0, bitbufulg = 0)

/* ===========================================================================
 * Read the Huffman tree.
 */
local void read_tree()
{
    int len;  /* bit length */
    int base; /* base offset for a sequence of leaves */
    int n;

    /* Read the original input size, MSB first */
    orig_len = 0;
    for (n = 1; n <= 4; n++) orig_len = (orig_len << 8) | (ulg)get_byte();

    max_len = (int)get_byte(); /* maximum bit length of Huffman codes */
    if (max_len > MAX_BITLEN) {
	error("invalid compressed data -- Huffman code > 32 bits");
    }

    /* Get the number of leaves at each bit length */
    n = 0;
    for (len = 1; len <= max_len; len++) {
	leaves[len] = (int)get_byte();
	n += leaves[len];
    }
    if (n > LITERALS) {
	error("too many leaves in Huffman tree");
    }
    Trace((stderr, "orig_len %ld, max_len %d, leaves %d\n",
	   orig_len, max_len, n));
    /* There are at least 2 and at most 256 leaves of length max_len.
     * (Pack arbitrarily rejects empty files and files consisting of
     * a single byte even repeated.) To fit the last leaf count in a
     * byte, it is offset by 2. However, the last literal is the EOB
     * code, and is not transmitted explicitly in the tree, so we must
     * adjust here by one only.
     */
    leaves[max_len]++;

    /* Now read the leaves themselves */
    base = 0;
    for (len = 1; len <= max_len; len++) {
	/* Remember where the literals of this length start in literal[] : */
	lit_base[len] = base;
	/* And read the literals: */
	for (n = leaves[len]; n > 0; n--) {
	    literal[base++] = (uch)get_byte();
	}
    }
    leaves[max_len]++; /* Now include the EOB code in the Huffman tree */
}

/* ===========================================================================
 * Build the Huffman tree and the prefix table.
 */
local void build_tree()
{
    int nodes = 0; /* number of nodes (parents+leaves) at current bit length */
    int len;       /* current bit length */
    uch *prefixp;  /* pointer in prefix_len */

    for (len = max_len; len >= 1; len--) {
	/* The number of parent nodes at this level is half the total
	 * number of nodes at parent level:
	 */
	nodes >>= 1;
	parents[len] = nodes;
	/* Update lit_base by the appropriate bias to skip the parent nodes
	 * (which are not represented in the literal array):
	 */
	lit_base[len] -= nodes;
	/* Restore nodes to be parents+leaves: */
	nodes += leaves[len];
    }
    /* Construct the prefix table, from shortest leaves to longest ones.
     * The shortest code is all ones, so we start at the end of the table.
     */
    peek_bits = MIN(max_len, MAX_PEEK);
    prefixp = &prefix_len[1<<peek_bits];
    for (len = 1; len <= peek_bits; len++) {
	int prefixes = leaves[len] << (peek_bits-len); /* may be 0 */
	while (prefixes--) *--prefixp = (uch)len;
    }
    /* The length of all other codes is unknown: */
    while (prefixp > prefix_len) *--prefixp = 0;
}

/* ===========================================================================
 * Unpack in to out.  This routine does not support the old pack format
 * with magic header \037\037.
 *
 * IN assertions: the buffer inbuf contains already the beginning of
 *   the compressed data, from offsets inptr to insize-1 included.
 *   The magic header has already been checked. The output buffer is cleared.
 */
local int unpack(in, out)
    FILE *in, *out;            /* input and output file descriptors */
{
    int len;                /* Bit length of current code */
    unsigned eob;           /* End Of Block code */
    register unsigned peek; /* lookahead bits */
    unsigned peek_mask;     /* Mask for peek_bits bits */

    ifd = in;
    ofd = out;

    read_tree();     /* Read the Huffman tree */
    build_tree();    /* Build the prefix table */
    clear_bitbuf();  /* Initialize bit input */
    peek_mask = (1<<peek_bits)-1;

    /* The eob code is the largest code among all leaves of maximal length: */
    eob = leaves[max_len]-1;
    Trace((stderr, "eob %d %x\n", max_len, eob));

    /* Decode the input data: */
    for (;;) {
	/* Since eob is the longest code and not shorter than max_len,
         * we can peek at max_len bits without having the risk of reading
         * beyond the end of file.
	 */
	look_bits(peek, peek_bits, peek_mask);
	len = prefix_len[peek];
	if (len > 0) {
	    peek >>= peek_bits - len; /* discard the extra bits */
	} else {
	    /* Code of more than peek_bits bits, we must traverse the tree */
	    ulg mask = peek_mask;
	    len = peek_bits;
	    do {
                len++, mask = (mask<<1)+1;
		look_bits(peek, len, mask);
	    } while (peek < (unsigned)parents[len]);
	    /* loop as long as peek is a parent node */
	}
	/* At this point, peek is the next complete code, of len bits */
	if (peek == eob && len == max_len) break; /* end of file? */
	put_ubyte(literal[peek+lit_base[len]]);
	Tracev((stderr,"%02d %04x %c\n", len, peek,
		literal[peek+lit_base[len]]));
	skip_bits(len);
    } /* for (;;) */

    flush_window();
    Trace((stderr, "bytes_out %ld\n", bytes_out));
    if (orig_len != (ulg)bytes_out) {
	error("invalid compressed data--length error");
        return ERROR;
    }
    return OK;
}
/*=======================================================================*/
/*  This marks the beginning of the original file 'unzip.c'              */
/*=======================================================================*/

/* unzip.c -- decompress files in gzip or pkzip format.
 * Copyright (C) 1992-1993 Jean-loup Gailly
 * This is free software; you can redistribute it and/or modify it under the
 * terms of the GNU General Public License, see the file COPYING.
 *
 * The code in this file is derived from the file funzip.c written
 * and put in the public domain by Mark Adler.
 */

/*
   This version can extract files in gzip or pkzip format.
   For the latter, only the first entry is extracted, and it has to be
   either deflated or stored.
 */

/* PKZIP header definitions */
#define LOCSIG 0x04034b50L      /* four-byte lead-in (lsb first) */
#define LOCFLG 6                /* offset of bit flag */
#define  CRPFLG 1               /*  bit for encrypted entry */
#define  EXTFLG 8               /*  bit for extended local header */
#define LOCHOW 8                /* offset of compression method */
#define LOCTIM 10               /* file mod time (for decryption) */
#define LOCCRC 14               /* offset of crc */
#define LOCSIZ 18               /* offset of compressed size */
#define LOCLEN 22               /* offset of uncompressed length */
#define LOCFIL 26               /* offset of file name field length */
#define LOCEXT 28               /* offset of extra field length */
#define LOCHDR 30               /* size of local header, including sig */
#define EXTHDR 16               /* size of extended local header, inc sig */


/* Globals */
/* added 'static' to the following 4 lines - WDP (1/1/99) */

static int decrypt;        /* flag to turn on decryption */
/* static char *key;  */        /* not used--needed to link crypt.c */
static int pkzip = 0;      /* set for a pkzip file */
static int ext_header = 0; /* set if extended local header */

/* ===========================================================================
 * Check zip file and advance inptr to the start of the compressed data.
 * Get ofname from the local header if necessary.
 */
local int check_zipfile(in)
    FILE *in;   /* input file descriptors */
{
    uch *h = inbuf + inptr; /* first local header */

    ifd = in;

    /* Check validity of local header, and skip name and extra fields */
    inptr += LOCHDR + SH(h + LOCFIL) + SH(h + LOCEXT);

    if (inptr > insize || LG(h) != LOCSIG) {
	error("not a valid zip file");
	exit_code = ERROR;
	return ERROR;
    }
    method = h[LOCHOW];
    if (method != STORED && method != DEFLATED) {
	error("first entry not deflated or stored -- use unzip");
	exit_code = ERROR;
	return ERROR;
    }

    /* If entry encrypted, decrypt and validate encryption header */
    if ((decrypt = h[LOCFLG] & CRPFLG) != 0) {
	error("encrypted file -- use unzip");
	exit_code = ERROR;
	return ERROR;
    }

    /* Save flags for unzip() */
    ext_header = (h[LOCFLG] & EXTFLG) != 0;
    pkzip = 1;

    /* Get ofname and time stamp from local header (to be done) */
    return OK;
}

/* ===========================================================================
 * Unzip in to out.  This routine works on both gzip and pkzip files.
 *
 * IN assertions: the buffer inbuf contains already the beginning of
 *   the compressed data, from offsets inptr to insize-1 included.
 *   The magic header has already been checked. The output buffer is cleared.
 */
local int unzip(in, out)
    FILE *in, *out;   /* input and output file descriptors */
{
    ulg orig_crc = 0;       /* original crc */

/* orig_len has already been defined statically before */
/*    ulg orig_len = 0;  */     /* original uncompressed length */

    int n;
    uch buf[EXTHDR];        /* extended local header */

    ifd = in;
    ofd = out;

    updcrc(NULL, 0);           /* initialize crc */

    if (pkzip && !ext_header) {  /* crc and length at the end otherwise */
	orig_crc = LG(inbuf + LOCCRC);
	orig_len = LG(inbuf + LOCLEN);
    }

    /* Decompress */
    if (method == DEFLATED)  {

	int res = inflate();

	if (res == 3) {
	    error("out of memory");
            return ERROR;
	} else if (res != 0) {
	    error("invalid compressed data--format violated");
            return ERROR;
	}

    } else if (pkzip && method == STORED) {

   /* 'nn' here was originally declared 'n' which conflicts with the */
   /*  previous local declaration of 'n'.  It was changed to 'nn' on 1/4/99 */
	register ulg nn = LG(inbuf + LOCLEN);

	if (nn != LG(inbuf + LOCSIZ) - (decrypt ? 12 : 0)) {
	    error("invalid compressed data--length mismatch");
            return ERROR;
	}
	while (nn--) {
	    uch c = (uch)get_byte();
#ifdef CRYPT
	    if (decrypt) zdecode(c);
#endif
	    put_ubyte(c);
	}
	flush_window();
    } else {
	error("internal error, invalid method");
        return ERROR;
    }

    /* Get the crc and original length */
    if (!pkzip) {
        /* crc32  (see algorithm.doc)
	 * uncompressed input size modulo 2^32
         */
	for (n = 0; n < 8; n++) {
	    buf[n] = (uch)get_byte(); /* may cause an error if EOF */
	}
	orig_crc = LG(buf);
	orig_len = LG(buf+4);

    } else if (ext_header) {  /* If extended header, check it */
	/* signature - 4bytes: 0x50 0x4b 0x07 0x08
	 * CRC-32 value
         * compressed size 4-bytes
         * uncompressed size 4-bytes
	 */
	for (n = 0; n < EXTHDR; n++) {
	    buf[n] = (uch)get_byte(); /* may cause an error if EOF */
	}
	orig_crc = LG(buf+4);
	orig_len = LG(buf+12);
    }

    /* Validate decompression */
    if (orig_crc != updcrc(outbuf, 0)) {
	error("invalid compressed data--crc error");
        return ERROR;
    }
    if (orig_len != (ulg)bytes_out) {
	error("invalid compressed data--length error");
        return ERROR;
    }

    /* Check if there are more entries in a pkzip file */
    if (pkzip && inptr + 4 < insize && LG(inbuf+inptr) == LOCSIG) {
	    /* Don't destroy the input zip file */
	    error("file has more than one entry -- unchanged");
	    exit_code = ERROR;
	    ext_header = pkzip = 0;
	    return ERROR;
    }
    ext_header = pkzip = 0; /* for next file */
    return OK;
}

/*======================================================================*/
/* This marks the start of the code originally in the file 'inflate.c'  */
/*======================================================================*/

/* inflate.c -- Not copyrighted 1992 by Mark Adler
   version c10p1, 10 January 1993 */

/* You can do whatever you like with this source file, though I would
   prefer that if you modify it and redistribute it that you include
   comments to that effect with your name and the date.  Thank you.
   [The history has been moved to the file ChangeLog.]
 */

/*
   Inflate deflated (PKZIP's method 8 compressed) data.  The compression
   method searches for as much of the current string of bytes (up to a
   length of 258) in the previous 32K bytes.  If it doesn't find any
   matches (of at least length 3), it codes the next byte.  Otherwise, it
   codes the length of the matched string and its distance backwards from
   the current position.  There is a single Huffman code that codes both
   single bytes (called "literals") and match lengths.  A second Huffman
   code codes the distance information, which follows a length code.  Each
   length or distance code actually represents a base value and a number
   of "extra" (sometimes zero) bits to get to add to the base value.  At
   the end of each deflated block is a special end-of-block (EOB) literal/
   length code.  The decoding process is basically: get a literal/length
   code; if EOB then done; if a literal, emit the decoded byte; if a
   length then get the distance and emit the referred-to bytes from the
   sliding window of previously emitted data.

   There are (currently) three kinds of inflate blocks: stored, fixed, and
   dynamic.  The compressor deals with some chunk of data at a time, and
   decides which method to use on a chunk-by-chunk basis.  A chunk might
   typically be 32K or 64K.  If the chunk is uncompressible, then the
   "stored" method is used.  In this case, the bytes are simply stored as
   is, eight bits per byte, with none of the above coding.  The bytes are
   preceded by a count, since there is no longer an EOB code.

   If the data is compressible, then either the fixed or dynamic methods
   are used.  In the dynamic method, the compressed data is preceded by
   an encoding of the literal/length and distance Huffman codes that are
   to be used to decode this block.  The representation is itself Huffman
   coded, and so is preceded by a description of that code.  These code
   descriptions take up a little space, and so for small blocks, there is
   a predefined set of codes, called the fixed codes.  The fixed method is
   used if the block codes up smaller that way (usually for quite small
   chunks), otherwise the dynamic method is used.  In the latter case, the
   codes are customized to the probabilities in the current block, and so
   can code it much better than the pre-determined fixed codes.
 
   The Huffman codes themselves are decoded using a mutli-level table
   lookup, in order to maximize the speed of decoding plus the speed of
   building the decoding tables.  See the comments below that precede the
   lbits and dbits tuning parameters.
 */

#define slide window

/* Huffman code lookup table entry--this entry is four bytes for machines
   that have 16-bit pointers (e.g. PC's in the small or medium model).
   Valid extra bits are 0..13.  e == 15 is EOB (end of block), e == 16
   means that v is a literal, 16 < e < 32 means that v is a pointer to
   the next table, which codes e - 16 bits, and lastly e == 99 indicates
   an unused code.  If a code with e == 99 is looked up, this implies an
   error in the data. */

/* The inflate algorithm uses a sliding 32K byte window on the uncompressed
   stream to find repeated byte strings.  This is implemented here as a
   circular buffer.  The index is updated simply by incrementing and then
   and'ing with 0x7fff (32K-1). */
/* It is left to other modules to supply the 32K area.  It is assumed
   to be usable as if it were declared "uch slide[32768];" or as just
   "uch *slide;" and then malloc'ed in the latter case.  The definition
   must be in unzip.h, included above. */
/* unsigned wp;             current position in slide */
#define wp outcnt
#define flush_output(w) (wp=(w),flush_window())

/* Tables for deflate from PKZIP's appnote.txt. */
static unsigned border[] = {    /* Order of the bit length code lengths */
        16, 17, 18, 0, 8, 7, 9, 6, 10, 5, 11, 4, 12, 3, 13, 2, 14, 1, 15};
static ush cplens[] = {         /* Copy lengths for literal codes 257..285 */
        3, 4, 5, 6, 7, 8, 9, 10, 11, 13, 15, 17, 19, 23, 27, 31,
        35, 43, 51, 59, 67, 83, 99, 115, 131, 163, 195, 227, 258, 0, 0};
        /* note: see note #13 above about the 258 in this list. */
static ush cplext[] = {         /* Extra bits for literal codes 257..285 */
        0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 2, 2,
        3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 0, 99, 99}; /* 99==invalid */
static ush cpdist[] = {         /* Copy offsets for distance codes 0..29 */
        1, 2, 3, 4, 5, 7, 9, 13, 17, 25, 33, 49, 65, 97, 129, 193,
        257, 385, 513, 769, 1025, 1537, 2049, 3073, 4097, 6145,
        8193, 12289, 16385, 24577};
static ush cpdext[] = {         /* Extra bits for distance codes */
        0, 0, 0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6,
        7, 7, 8, 8, 9, 9, 10, 10, 11, 11,
        12, 12, 13, 13};

/* Macros for inflate() bit peeking and grabbing.
   The usage is:
   
        NEEDBITS(j)
        x = b & mask_bits[j];
        DUMPBITS(j)

   where NEEDBITS makes sure that b has at least j bits in it, and
   DUMPBITS removes the bits from b.  The macros use the variable k
   for the number of bits in b.  Normally, b and k are register
   variables for speed, and are initialized at the beginning of a
   routine that uses these macros from a global bit buffer and count.

   If we assume that EOB will be the longest code, then we will never
   ask for bits with NEEDBITS that are beyond the end of the stream.
   So, NEEDBITS should not read any more bytes than are needed to
   meet the request.  Then no bytes need to be "returned" to the buffer
   at the end of the last block.

   However, this assumption is not true for fixed blocks--the EOB code
   is 7 bits, but the other literal/length codes can be 8 or 9 bits.
   (The EOB code is shorter than other codes because fixed blocks are
   generally short.  So, while a block always has an EOB, many other
   literal/length codes have a significantly lower probability of
   showing up at all.)  However, by making the first table have a
   lookup of seven bits, the EOB code will be found in that first
   lookup, and so will not require that too many bits be pulled from
   the stream.
 */

static ulg bb;                         /* bit buffer (allan: added static) */
static unsigned bk;                    /* bits in bit buffer (allan: added static) */

static ush mask_bits[] = {  /* allan: added static to avoid conflict */
    0x0000,
    0x0001, 0x0003, 0x0007, 0x000f, 0x001f, 0x003f, 0x007f, 0x00ff,
    0x01ff, 0x03ff, 0x07ff, 0x0fff, 0x1fff, 0x3fff, 0x7fff, 0xffff
};

#ifdef CRYPT
  uch cc;
#  define NEXTBYTE() \
     (decrypt ? (cc = get_byte(), zdecode(cc), cc) : get_byte())
#else
#  define NEXTBYTE()  (uch)get_byte()
#endif
#define NEEDBITS(n) {while(k<(n)){b|=((ulg)NEXTBYTE())<<k;k+=8;}}
#define DUMPBITS(n) {b>>=(n);k-=(n);}

/*
   Huffman code decoding is performed using a multi-level table lookup.
   The fastest way to decode is to simply build a lookup table whose
   size is determined by the longest code.  However, the time it takes
   to build this table can also be a factor if the data being decoded
   is not very long.  The most common codes are necessarily the
   shortest codes, so those codes dominate the decoding time, and hence
   the speed.  The idea is you can have a shorter table that decodes the
   shorter, more probable codes, and then point to subsidiary tables for
   the longer codes.  The time it costs to decode the longer codes is
   then traded against the time it takes to make longer tables.

   This results of this trade are in the variables lbits and dbits
   below.  lbits is the number of bits the first level table for literal/
   length codes can decode in one step, and dbits is the same thing for
   the distance codes.  Subsequent tables are also less than or equal to
   those sizes.  These values may be adjusted either when all of the
   codes are shorter than that, in which case the longest code length in
   bits is used, or when the shortest code is *longer* than the requested
   table size, in which case the length of the shortest code in bits is
   used.

   There are two different values for the two tables, since they code a
   different number of possibilities each.  The literal/length table
   codes 286 possible values, or in a flat code, a little over eight
   bits.  The distance table codes 30 possible values, or a little less
   than five bits, flat.  The optimum values for speed end up being
   about one bit more than those, so lbits is 8+1 and dbits is 5+1.
   The optimum values may differ though from machine to machine, and
   possibly even between compilers.  Your mileage may vary.
 */

static int lbits = 9;          /* bits in base literal/length lookup table (allan: added static) */
static int dbits = 6;          /* bits in base distance lookup table (allan: added static) */


/* If BMAX needs to be larger than 16, then h and x[] should be ulg. */
#define BMAX 16         /* maximum bit length of any code (16 for explode) */
#define N_MAX 288       /* maximum number of codes in any set */

static unsigned hufts;         /* track memory usage (allan: added static) */

local int huft_build(b, n, s, d, e, t, m)
unsigned *b;            /* code lengths in bits (all assumed <= BMAX) */
unsigned n;             /* number of codes (assumed <= N_MAX) */
unsigned s;             /* number of simple-valued codes (0..s-1) */
ush *d;                 /* list of base values for non-simple codes */
ush *e;                 /* list of extra bits for non-simple codes */
struct huft **t;        /* result: starting table */
int *m;                 /* maximum lookup bits, returns actual */
/* Given a list of code lengths and a maximum table size, make a set of
   tables to decode that set of codes.  Return zero on success, one if
   the given code set is incomplete (the tables are still built in this
   case), two if the input is invalid (all zero length codes or an
   oversubscribed set of lengths), and three if not enough memory. */
{
  unsigned a;                   /* counter for codes of length k */
  unsigned c[BMAX+1];           /* bit length count table */
  unsigned f;                   /* i repeats in table every f entries */
  int g;                        /* maximum code length */
  int h;                        /* table level */
  register unsigned i;          /* counter, current code */
  register unsigned j;          /* counter */
  register int k;               /* number of bits in current code */
  int l;                        /* bits per table (returned in m) */
  register unsigned *p;         /* pointer into c[], b[], or v[] */
  register struct huft *q;      /* points to current table */
  struct huft r;                /* table entry for structure assignment */
  struct huft *u[BMAX];         /* table stack */
  unsigned v[N_MAX];            /* values in order of bit length */
  register int w;               /* bits before this table == (l * h) */
  unsigned x[BMAX+1];           /* bit offsets, then code stack */
  unsigned *xp;                 /* pointer into x */
  int y;                        /* number of dummy codes added */
  unsigned z;                   /* number of entries in current table */


  /* Generate counts for each bit length */
  memzero(c, sizeof(c));
  p = b;  i = n;
  do {
    Tracecv(*p, (stderr, (n-i >= ' ' && n-i <= '~' ? "%c %d\n" : "0x%x %d\n"), 
	    n-i, *p));
    c[*p]++;                    /* assume all entries <= BMAX */
    p++;                      /* Can't combine with above line (Solaris bug) */
  } while (--i);
  if (c[0] == n)                /* null input--all zero length codes */
  {
    *t = (struct huft *)NULL;
    *m = 0;
    return 0;
  }


  /* Find minimum and maximum length, bound *m by those */
  l = *m;
  for (j = 1; j <= BMAX; j++)
    if (c[j])
      break;
  k = j;                        /* minimum code length */
  if ((unsigned)l < j)
    l = j;
  for (i = BMAX; i; i--)
    if (c[i])
      break;
  g = i;                        /* maximum code length */
  if ((unsigned)l > i)
    l = i;
  *m = l;


  /* Adjust last length count to fill out codes, if needed */
  for (y = 1 << j; j < i; j++, y <<= 1)
    if ((y -= c[j]) < 0)
      return 2;                 /* bad input: more codes than bits */
  if ((y -= c[i]) < 0)
    return 2;
  c[i] += y;


  /* Generate starting offsets into the value table for each length */
  x[1] = j = 0;
  p = c + 1;  xp = x + 2;
  while (--i) {                 /* note that i == g from above */
    *xp++ = (j += *p++);
  }


  /* Make a table of values in order of bit lengths */
  p = b;  i = 0;
  do {
    if ((j = *p++) != 0)
      v[x[j]++] = i;
  } while (++i < n);


  /* Generate the Huffman codes and for each, make the table entries */
  x[0] = i = 0;                 /* first Huffman code is zero */
  p = v;                        /* grab values in bit order */
  h = -1;                       /* no tables yet--level -1 */
  w = -l;                       /* bits decoded == (l * h) */
  u[0] = (struct huft *)NULL;   /* just to keep compilers happy */
  q = (struct huft *)NULL;      /* ditto */
  z = 0;                        /* ditto */

  /* go through the bit lengths (k already is bits in shortest code) */
  for (; k <= g; k++)
  {
    a = c[k];
    while (a--)
    {
      /* here i is the Huffman code of length k bits for value *p */
      /* make tables up to required level */
      while (k > w + l)
      {
        h++;
        w += l;                 /* previous table always l bits */

        /* compute minimum size table less than or equal to l bits */
        z = (z = g - w) > (unsigned)l ? l : z;  /* upper limit on table size */
        if ((f = 1 << (j = k - w)) > a + 1)     /* try a k-w bit table */
        {                       /* too few codes for k-w bit table */
          f -= a + 1;           /* deduct codes from patterns left */
          xp = c + k;
          while (++j < z)       /* try smaller tables up to z bits */
          {
            if ((f <<= 1) <= *++xp)
              break;            /* enough codes to use up j bits */
            f -= *xp;           /* else deduct codes from patterns */
          }
        }
        z = 1 << j;             /* table entries for j-bit table */

        /* allocate and link in new table */
        if ((q = (struct huft *)malloc((z + 1)*sizeof(struct huft))) ==
            (struct huft *)NULL)
        {
          if (h)
            huft_free(u[0]);
          return 3;             /* not enough memory */
        }
        hufts += z + 1;         /* track memory usage */
        *t = q + 1;             /* link to list for huft_free() */
        *(t = &(q->v.t)) = (struct huft *)NULL;
        u[h] = ++q;             /* table starts after link */

        /* connect to last table, if there is one */
        if (h)
        {
          x[h] = i;             /* save pattern for backing up */
          r.b = (uch)l;         /* bits to dump before this table */
          r.e = (uch)(16 + j);  /* bits in this table */
          r.v.t = q;            /* pointer to this table */
          j = i >> (w - l);     /* (get around Turbo C bug) */
          u[h-1][j] = r;        /* connect to last table */
        }
      }

      /* set up table entry in r */
      r.b = (uch)(k - w);
      if (p >= v + n)
        r.e = 99;               /* out of values--invalid code */
      else if (*p < s)
      {
        r.e = (uch)(*p < 256 ? 16 : 15);    /* 256 is end-of-block code */
        r.v.n = (ush)(*p);             /* simple code is just the value */
	p++;                           /* one compiler does not like *p++ */
      }
      else
      {
        r.e = (uch)e[*p - s];   /* non-simple--look up in lists */
        r.v.n = d[*p++ - s];
      }

      /* fill code-like entries with r */
      f = 1 << (k - w);
      for (j = i >> w; j < z; j += f)
        q[j] = r;

      /* backwards increment the k-bit code i */
      for (j = 1 << (k - 1); i & j; j >>= 1)
        i ^= j;
      i ^= j;

      /* backup over finished tables */
      while ((i & ((1 << w) - 1)) != x[h])
      {
        h--;                    /* don't need to update q */
        w -= l;
      }
    }
  }


  /* Return true (1) if we were given an incomplete table */
  return y != 0 && g != 1;
}

local int huft_free(t)
struct huft *t;         /* table to free */
/* Free the malloc'ed tables built by huft_build(), which makes a linked
   list of the tables it made, with the links in a dummy first entry of
   each table. */
{
  register struct huft *p, *q;


  /* Go through linked list, freeing from the malloced (t[-1]) address. */
  p = t;
  while (p != (struct huft *)NULL)
  {
    q = (--p)->v.t;
    free((char*)p);
    p = q;
  } 
  return 0;
}


local int inflate_codes(tl, td, bl, bd)
struct huft *tl, *td;   /* literal/length and distance decoder tables */
int bl, bd;             /* number of bits decoded by tl[] and td[] */
/* inflate (decompress) the codes in a deflated (compressed) block.
   Return an error code or zero if it all goes ok. */
{
  register unsigned e;  /* table entry flag/number of extra bits */
  unsigned n, d;        /* length and index for copy */
  unsigned w;           /* current window position */
  struct huft *t;       /* pointer to table entry */
  unsigned ml, md;      /* masks for bl and bd bits */
  register ulg b;       /* bit buffer */
  register unsigned k;  /* number of bits in bit buffer */


  /* make local copies of globals */
  b = bb;                       /* initialize bit buffer */
  k = bk;
  w = wp;                       /* initialize window position */

  /* inflate the coded data */
  ml = mask_bits[bl];           /* precompute masks for speed */
  md = mask_bits[bd];
  for (;;)                      /* do until end of block */
  {
    NEEDBITS((unsigned)bl)
    if ((e = (t = tl + ((unsigned)b & ml))->e) > 16)
      do {
        if (e == 99)
          return 1;
        DUMPBITS(t->b)
        e -= 16;
        NEEDBITS(e)
      } while ((e = (t = t->v.t + ((unsigned)b & mask_bits[e]))->e) > 16);
    DUMPBITS(t->b)
    if (e == 16)                /* then it's a literal */
    {
      slide[w++] = (uch)t->v.n;
      Tracevv((stderr, "%c", slide[w-1]));
      if (w == WSIZE)
      {
        flush_output(w);
        w = 0;
      }
    }
    else                        /* it's an EOB or a length */
    {
      /* exit if end of block */
      if (e == 15)
        break;

      /* get length of block to copy */
      NEEDBITS(e)
      n = t->v.n + ((unsigned)b & mask_bits[e]);
      DUMPBITS(e);

      /* decode distance of block to copy */
      NEEDBITS((unsigned)bd)
      if ((e = (t = td + ((unsigned)b & md))->e) > 16)
        do {
          if (e == 99)
            return 1;
          DUMPBITS(t->b)
          e -= 16;
          NEEDBITS(e)
        } while ((e = (t = t->v.t + ((unsigned)b & mask_bits[e]))->e) > 16);
      DUMPBITS(t->b)
      NEEDBITS(e)
      d = w - t->v.n - ((unsigned)b & mask_bits[e]);
      DUMPBITS(e)
      Tracevv((stderr,"\\[%d,%d]", w-d, n));

      /* do the copy */
      do {
        n -= (e = (e = WSIZE - ((d &= WSIZE-1) > w ? d : w)) > n ? n : e);
#if !defined(NOMEMCPY) && !defined(DEBUG)
        if (w - d >= e)         /* (this test assumes unsigned comparison) */
        {
          memcpy(slide + w, slide + d, e);
          w += e;
          d += e;
        }
        else                      /* do it slow to avoid memcpy() overlap */
#endif /* !NOMEMCPY */
          do {
            slide[w++] = slide[d++];
	    Tracevv((stderr, "%c", slide[w-1]));
          } while (--e);
        if (w == WSIZE)
        {
          flush_output(w);
          w = 0;
        }
      } while (n);
    }
  }

  /* restore the globals from the locals */
  wp = w;                       /* restore global window pointer */
  bb = b;                       /* restore global bit buffer */
  bk = k;

  /* done */
  return 0;
}

local int inflate_stored()
/* "decompress" an inflated type 0 (stored) block. */
{
  unsigned n;           /* number of bytes in block */
  unsigned w;           /* current window position */
  register ulg b;       /* bit buffer */
  register unsigned k;  /* number of bits in bit buffer */


  /* make local copies of globals */
  b = bb;                       /* initialize bit buffer */
  k = bk;
  w = wp;                       /* initialize window position */


  /* go to byte boundary */
  n = k & 7;
  DUMPBITS(n);


  /* get the length and its complement */
  NEEDBITS(16)
  n = ((unsigned)b & 0xffff);
  DUMPBITS(16)
  NEEDBITS(16)
  if (n != (unsigned)((~b) & 0xffff))
    return 1;                   /* error in compressed data */
  DUMPBITS(16)


  /* read and output the compressed data */
  while (n--)
  {
    NEEDBITS(8)
    slide[w++] = (uch)b;
    if (w == WSIZE)
    {
      flush_output(w);
      w = 0;
    }
    DUMPBITS(8)
  }

  /* restore the globals from the locals */
  wp = w;                       /* restore global window pointer */
  bb = b;                       /* restore global bit buffer */
  bk = k;
  return 0;
}


local int inflate_fixed()
/* decompress an inflated type 1 (fixed Huffman codes) block.  We should
   either replace this with a custom decoder, or at least precompute the
   Huffman tables. */
{
  int i;                /* temporary variable */
  struct huft *tl;      /* literal/length code table */
  struct huft *td;      /* distance code table */
  int bl;               /* lookup bits for tl */
  int bd;               /* lookup bits for td */
  unsigned l[288];      /* length list for huft_build */


  /* set up literal table */
  for (i = 0; i < 144; i++)
    l[i] = 8;
  for (; i < 256; i++)
    l[i] = 9;
  for (; i < 280; i++)
    l[i] = 7;
  for (; i < 288; i++)          /* make a complete, but wrong code set */
    l[i] = 8;
  bl = 7;
  if ((i = huft_build(l, 288, 257, cplens, cplext, &tl, &bl)) != 0)
    return i;


  /* set up distance table */
  for (i = 0; i < 30; i++)      /* make an incomplete code set */
    l[i] = 5;
  bd = 5;
  if ((i = huft_build(l, 30, 0, cpdist, cpdext, &td, &bd)) > 1)
  {
    huft_free(tl);
    return i;
  }


  /* decompress until an end-of-block code */
  if (inflate_codes(tl, td, bl, bd))
    return 1;


  /* free the decoding tables, return */
  huft_free(tl);
  huft_free(td);
  return 0;
}

local int inflate_dynamic()
/* decompress an inflated type 2 (dynamic Huffman codes) block. */
{
  int i;                /* temporary variables */
  unsigned j;
  unsigned l;           /* last length */
  unsigned m;           /* mask for bit lengths table */
  unsigned n;           /* number of lengths to get */
  struct huft *tl;      /* literal/length code table */
  struct huft *td;      /* distance code table */
  int bl;               /* lookup bits for tl */
  int bd;               /* lookup bits for td */
  unsigned nb;          /* number of bit length codes */
  unsigned nl;          /* number of literal/length codes */
  unsigned nd;          /* number of distance codes */
#ifdef PKZIP_BUG_WORKAROUND
  unsigned ll[288+32];  /* literal/length and distance code lengths */
#else
  unsigned ll[286+30];  /* literal/length and distance code lengths */
#endif
  register ulg b;       /* bit buffer */
  register unsigned k;  /* number of bits in bit buffer */

  /* make local bit buffer */
  b = bb;
  k = bk;

  /* read in table lengths */
  NEEDBITS(5)
  nl = 257 + ((unsigned)b & 0x1f);      /* number of literal/length codes */
  DUMPBITS(5)
  NEEDBITS(5)
  nd = 1 + ((unsigned)b & 0x1f);        /* number of distance codes */
  DUMPBITS(5)
  NEEDBITS(4)
  nb = 4 + ((unsigned)b & 0xf);         /* number of bit length codes */
  DUMPBITS(4)
#ifdef PKZIP_BUG_WORKAROUND
  if (nl > 288 || nd > 32)
#else
  if (nl > 286 || nd > 30)
#endif
    return 1;                   /* bad lengths */


  /* read in bit-length-code lengths */
  for (j = 0; j < nb; j++)
  {
    NEEDBITS(3)
    ll[border[j]] = (unsigned)b & 7;
    DUMPBITS(3)
  }
  for (; j < 19; j++)
    ll[border[j]] = 0;


  /* build decoding table for trees--single level, 7 bit lookup */
  bl = 7;
  if ((i = huft_build(ll, 19, 19, NULL, NULL, &tl, &bl)) != 0)
  {
    if (i == 1)
      huft_free(tl);
    return i;                   /* incomplete code set */
  }


  /* read in literal and distance code lengths */
  n = nl + nd;
  m = mask_bits[bl];
  i = l = 0;
  while ((unsigned)i < n)
  {
    NEEDBITS((unsigned)bl)
    j = (td = tl + ((unsigned)b & m))->b;
    DUMPBITS(j)
    j = td->v.n;
    if (j < 16)                 /* length of code in bits (0..15) */
      ll[i++] = l = j;          /* save last length in l */
    else if (j == 16)           /* repeat last length 3 to 6 times */
    {
      NEEDBITS(2)
      j = 3 + ((unsigned)b & 3);
      DUMPBITS(2)
      if ((unsigned)i + j > n)
        return 1;
      while (j--)
        ll[i++] = l;
    }
    else if (j == 17)           /* 3 to 10 zero length codes */
    {
      NEEDBITS(3)
      j = 3 + ((unsigned)b & 7);
      DUMPBITS(3)
      if ((unsigned)i + j > n)
        return 1;
      while (j--)
        ll[i++] = 0;
      l = 0;
    }
    else                        /* j == 18: 11 to 138 zero length codes */
    {
      NEEDBITS(7)
      j = 11 + ((unsigned)b & 0x7f);
      DUMPBITS(7)
      if ((unsigned)i + j > n)
        return 1;
      while (j--)
        ll[i++] = 0;
      l = 0;
    }
  }

  /* free decoding table for trees */
  huft_free(tl);


  /* restore the global bit buffer */
  bb = b;
  bk = k;


  /* build the decoding tables for literal/length and distance codes */
  bl = lbits;
  if ((i = huft_build(ll, nl, 257, cplens, cplext, &tl, &bl)) != 0)
  {
    if (i == 1) {
      error(" incomplete literal tree in inflate_dynamic");
      huft_free(tl);
    }
    return i;                   /* incomplete code set */
  }
  bd = dbits;
  if ((i = huft_build(ll + nl, nd, 0, cpdist, cpdext, &td, &bd)) != 0)
  {
    if (i == 1) {
      error(" incomplete distance tree in inflate_dynamic");
#ifdef PKZIP_BUG_WORKAROUND
      i = 0;
    }
#else
      huft_free(td);
    }
    huft_free(tl);
    return i;                   /* incomplete code set */
#endif
  }

  /* decompress until an end-of-block code */
  if (inflate_codes(tl, td, bl, bd))
    return 1;


  /* free the decoding tables, return */
  huft_free(tl);
  huft_free(td);
  return 0;
}

local int inflate_block(e)
int *e;                 /* last block flag */
/* decompress an inflated block */
{
  unsigned t;           /* block type */
  register ulg b;       /* bit buffer */
  register unsigned k;  /* number of bits in bit buffer */

  /* make local bit buffer */
  b = bb;
  k = bk;

  /* read in last block bit */
  NEEDBITS(1)
  *e = (int)b & 1;
  DUMPBITS(1)

  /* read in block type */
  NEEDBITS(2)
  t = (unsigned)b & 3;
  DUMPBITS(2)

  /* restore the global bit buffer */
  bb = b;
  bk = k;

  /* inflate that block type */
  if (t == 2)
    return inflate_dynamic();
  if (t == 0)
    return inflate_stored();
  if (t == 1)
    return inflate_fixed();

  /* bad block type */
  return 2;
}

local int inflate()
/* decompress an inflated entry */
{
  int e;                /* last block flag */
  int r;                /* result code */
  unsigned h;           /* maximum struct huft's malloc'ed */

  /* initialize window, bit buffer */
  wp = 0;
  bk = 0;
  bb = 0;

  /* decompress until the last block */
  h = 0;
  do {
    hufts = 0;
    if ((r = inflate_block(&e)) != 0)
      return r;
    if (hufts > h)
      h = hufts;
  } while (!e);

  /* Undo too much lookahead. The next read will be byte aligned so we
   * can discard unused bits in the last meaningful byte.
   */
  while (bk >= 8) {
    bk -= 8;
    inptr--;
  }

  /* flush out slide */
  flush_output(wp);

  return 0;
}
