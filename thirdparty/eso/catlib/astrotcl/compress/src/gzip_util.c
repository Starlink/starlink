/*+
************************************************************************
****  C A N A D I A N   A S T R O N O M Y   D A T A   C E N T R E  *****
*
*   Module Name:	press/src/gzip_util.c
*
*   Purpose:
*	Contains utility routines for gzip.
*
*   Routines:
*	type	routine		: Brief description.
*	type	routine		: Brief description.
*	type	routine		: Brief description.
*
*   Date		: mmm dd, 1993
*
*   SCCS data		: @(#)
*	Module Name	: gzip_util.c
*	Version Number	: 1.2
*	Release Number	: 1
*	Last Updated	: 07/04/97
*
*   Programmer		: your name
*
*   Modification History:
*	97/07/02 SEC  :	Bring up to ANSI standard. 
*
****  C A N A D I A N   A S T R O N O M Y   D A T A   C E N T R E  *****
************************************************************************
-*/

#include "gen_types.h"

#include "press.h"
#include "local_press.h"
#include "gzip.h"


/* ========================================================================
 * Table of CRC-32's of all single-byte values (made by makecrc.c)
 */
static unsigned long crc_32_tab[] = {
    0x00000000UL, 0x77073096UL, 0xee0e612cUL, 0x990951baUL, 0x076dc419UL,
    0x706af48fUL, 0xe963a535UL, 0x9e6495a3UL, 0x0edb8832UL, 0x79dcb8a4UL,
    0xe0d5e91eUL, 0x97d2d988UL, 0x09b64c2bUL, 0x7eb17cbdUL, 0xe7b82d07UL,
    0x90bf1d91UL, 0x1db71064UL, 0x6ab020f2UL, 0xf3b97148UL, 0x84be41deUL,
    0x1adad47dUL, 0x6ddde4ebUL, 0xf4d4b551UL, 0x83d385c7UL, 0x136c9856UL,
    0x646ba8c0UL, 0xfd62f97aUL, 0x8a65c9ecUL, 0x14015c4fUL, 0x63066cd9UL,
    0xfa0f3d63UL, 0x8d080df5UL, 0x3b6e20c8UL, 0x4c69105eUL, 0xd56041e4UL,
    0xa2677172UL, 0x3c03e4d1UL, 0x4b04d447UL, 0xd20d85fdUL, 0xa50ab56bUL,
    0x35b5a8faUL, 0x42b2986cUL, 0xdbbbc9d6UL, 0xacbcf940UL, 0x32d86ce3UL,
    0x45df5c75UL, 0xdcd60dcfUL, 0xabd13d59UL, 0x26d930acUL, 0x51de003aUL,
    0xc8d75180UL, 0xbfd06116UL, 0x21b4f4b5UL, 0x56b3c423UL, 0xcfba9599UL,
    0xb8bda50fUL, 0x2802b89eUL, 0x5f058808UL, 0xc60cd9b2UL, 0xb10be924UL,
    0x2f6f7c87UL, 0x58684c11UL, 0xc1611dabUL, 0xb6662d3dUL, 0x76dc4190UL,
    0x01db7106UL, 0x98d220bcUL, 0xefd5102aUL, 0x71b18589UL, 0x06b6b51fUL,
    0x9fbfe4a5UL, 0xe8b8d433UL, 0x7807c9a2UL, 0x0f00f934UL, 0x9609a88eUL,
    0xe10e9818UL, 0x7f6a0dbbUL, 0x086d3d2dUL, 0x91646c97UL, 0xe6635c01UL,
    0x6b6b51f4UL, 0x1c6c6162UL, 0x856530d8UL, 0xf262004eUL, 0x6c0695edUL,
    0x1b01a57bUL, 0x8208f4c1UL, 0xf50fc457UL, 0x65b0d9c6UL, 0x12b7e950UL,
    0x8bbeb8eaUL, 0xfcb9887cUL, 0x62dd1ddfUL, 0x15da2d49UL, 0x8cd37cf3UL,
    0xfbd44c65UL, 0x4db26158UL, 0x3ab551ceUL, 0xa3bc0074UL, 0xd4bb30e2UL,
    0x4adfa541UL, 0x3dd895d7UL, 0xa4d1c46dUL, 0xd3d6f4fbUL, 0x4369e96aUL,
    0x346ed9fcUL, 0xad678846UL, 0xda60b8d0UL, 0x44042d73UL, 0x33031de5UL,
    0xaa0a4c5fUL, 0xdd0d7cc9UL, 0x5005713cUL, 0x270241aaUL, 0xbe0b1010UL,
    0xc90c2086UL, 0x5768b525UL, 0x206f85b3UL, 0xb966d409UL, 0xce61e49fUL,
    0x5edef90eUL, 0x29d9c998UL, 0xb0d09822UL, 0xc7d7a8b4UL, 0x59b33d17UL,
    0x2eb40d81UL, 0xb7bd5c3bUL, 0xc0ba6cadUL, 0xedb88320UL, 0x9abfb3b6UL,
    0x03b6e20cUL, 0x74b1d29aUL, 0xead54739UL, 0x9dd277afUL, 0x04db2615UL,
    0x73dc1683UL, 0xe3630b12UL, 0x94643b84UL, 0x0d6d6a3eUL, 0x7a6a5aa8UL,
    0xe40ecf0bUL, 0x9309ff9dUL, 0x0a00ae27UL, 0x7d079eb1UL, 0xf00f9344UL,
    0x8708a3d2UL, 0x1e01f268UL, 0x6906c2feUL, 0xf762575dUL, 0x806567cbUL,
    0x196c3671UL, 0x6e6b06e7UL, 0xfed41b76UL, 0x89d32be0UL, 0x10da7a5aUL,
    0x67dd4accUL, 0xf9b9df6fUL, 0x8ebeeff9UL, 0x17b7be43UL, 0x60b08ed5UL,
    0xd6d6a3e8UL, 0xa1d1937eUL, 0x38d8c2c4UL, 0x4fdff252UL, 0xd1bb67f1UL,
    0xa6bc5767UL, 0x3fb506ddUL, 0x48b2364bUL, 0xd80d2bdaUL, 0xaf0a1b4cUL,
    0x36034af6UL, 0x41047a60UL, 0xdf60efc3UL, 0xa867df55UL, 0x316e8eefUL,
    0x4669be79UL, 0xcb61b38cUL, 0xbc66831aUL, 0x256fd2a0UL, 0x5268e236UL,
    0xcc0c7795UL, 0xbb0b4703UL, 0x220216b9UL, 0x5505262fUL, 0xc5ba3bbeUL,
    0xb2bd0b28UL, 0x2bb45a92UL, 0x5cb36a04UL, 0xc2d7ffa7UL, 0xb5d0cf31UL,
    0x2cd99e8bUL, 0x5bdeae1dUL, 0x9b64c2b0UL, 0xec63f226UL, 0x756aa39cUL,
    0x026d930aUL, 0x9c0906a9UL, 0xeb0e363fUL, 0x72076785UL, 0x05005713UL,
    0x95bf4a82UL, 0xe2b87a14UL, 0x7bb12baeUL, 0x0cb61b38UL, 0x92d28e9bUL,
    0xe5d5be0dUL, 0x7cdcefb7UL, 0x0bdbdf21UL, 0x86d3d2d4UL, 0xf1d4e242UL,
    0x68ddb3f8UL, 0x1fda836eUL, 0x81be16cdUL, 0xf6b9265bUL, 0x6fb077e1UL,
    0x18b74777UL, 0x88085ae6UL, 0xff0f6a70UL, 0x66063bcaUL, 0x11010b5cUL,
    0x8f659effUL, 0xf862ae69UL, 0x616bffd3UL, 0x166ccf45UL, 0xa00ae278UL,
    0xd70dd2eeUL, 0x4e048354UL, 0x3903b3c2UL, 0xa7672661UL, 0xd06016f7UL,
    0x4969474dUL, 0x3e6e77dbUL, 0xaed16a4aUL, 0xd9d65adcUL, 0x40df0b66UL,
    0x37d83bf0UL, 0xa9bcae53UL, 0xdebb9ec5UL, 0x47b2cf7fUL, 0x30b5ffe9UL,
    0xbdbdf21cUL, 0xcabac28aUL, 0x53b39330UL, 0x24b4a3a6UL, 0xbad03605UL,
    0xcdd70693UL, 0x54de5729UL, 0x23d967bfUL, 0xb3667a2eUL, 0xc4614ab8UL,
    0x5d681b02UL, 0x2a6f2b94UL, 0xb40bbe37UL, 0xc30c8ea1UL, 0x5a05df1bUL,
    0x2d02ef8dUL
};



int		get_byte
(
    void
)
{
    byte	c;
    int		s;


    s = char_in( &c, 1 );

    return( (int) c );
}



#ifdef omit
/* util.c -- utility functions for gzip support
 * Copyright (C) 1992-1993 Jean-loup Gailly
 * This is free software; you can redistribute it and/or modify it under the
 * terms of the GNU General Public License, see the file COPYING.
 */

#include <ctype.h>
#include <errno.h>
#include <sys/types.h>

x#include "tailor.h"

#include <fcntl.h>

#include <stdlib.h>

x#include "gzip.h"
x#include "crypt.h"



/* ===========================================================================
 * Copy input to output unchanged: zcat == cat with --force.
 * IN assertion: insize bytes have already been read in inbuf.
 */

int		copy
(
    int 	in, 		/* Input file descriptor.	 	*/
    int		out		/* Output file descriptor. 		*/
)
{
    errno = 0;

    while (insize != 0 && (int)insize != EOF) {
	write_buf(out, (char*)inbuf, insize);
	bytes_out += insize;
	insize = read(in, (char*)inbuf, INBUFSIZ);
    }

    if ((int)insize == EOF && errno != 0) {
	read_error();
    }

    bytes_in = bytes_out;


    return OK;
}
#endif /* omit */


/* ===========================================================================
 * Run a set of bytes through the crc shift register.  If s is a NULL
 * pointer, then initialize the crc shift register contents instead.
 * Return the current crc in either case.
 */

unsigned long	updcrc
(
    byte 	*s,		/* pointer to bytes to pump through. 	*/
    unsigned 	n 		/* number of bytes in s[].		*/
)
{
    unsigned long 	c;	/* temporary variable.			*/
    static unsigned long 
			crc = 0xffffffffUL; 
				/* shift register contents. 		*/

    if (s == NULL) 
    {
	c = 0xffffffffUL;
    } 
    else 
    {
	c = crc;
	for ( ; n > (unsigned) 0 ; n-- )
	{
            c = crc_32_tab[((int)c ^ (*s++)) & 0xff] ^ (c >> 8);
        }
    }

    crc = c;

    return ( c ^ 0xffffffffUL );      /* (instead of ~c for 64-bit machines) */
}

#ifdef omit

/* ===========================================================================
 * Clear input and output buffers
 */

void 		clear_bufs
(
    void
)
{
    outcnt = 0;
    insize = inptr = 0;
    bytes_in = bytes_out = 0L;
}


/* ===========================================================================
 * Fill the input buffer. This is called only when the buffer is empty.
 */

int fill_inbuf
(
    int 	eof_ok		/* set if EOF acceptable as a result. 	*/
)
{
    /* void	pr_format_message(); */
    int 	len;


    /* Read as much as possible */

    insize = 0;
    errno = 0;

    do {
	len = char_in((char*)inbuf+insize, INBUFSIZ-insize);
        if (len == 0 || len == PR_E_EOI ) break;
	insize += len;
    } while (insize < INBUFSIZ);

    if (insize == 0) {
	if (eof_ok) return(PR_E_EOI);
	pr_format_message( PR_E_EOI );
    }

    bytes_in += (unsigned long)insize;
    inptr = 1;

    return inbuf[0];
}


/* ===========================================================================
 * Write the output buffer outbuf[0..outcnt-1] and update bytes_out.
 * (used for the compressed data only)
 */

void 		flush_outbuf
(
    void
)
{
    if (outcnt == 0) return;

    write_buf(ofd, (char *)outbuf, outcnt);
    bytes_out += (ulg)outcnt;
    outcnt = 0;
}

#endif /* omit */


/*+
************************************************************************
*
*   Synopsis:
*	int	flush_window()
*
*   Purpose:
*	Write the output window window[0..outcnt-1] and update crc and 
*	bytes_out.  (Used for the decompressed data only.)
*
*   Parameters:
*	type	arg1		: (in)  What it is
*	type	arg2		: (mod) What it is
*	type	argn		: (out) What it is
*
*   Values Returned:
*	int	PR_SUCCESS	: Normal completion.
*	int	PR_E_IO		: Io error.
*
************************************************************************
-*/

int		flush_window
(
    void
)
{
    if (outcnt == 0) 
    {
	return( PR_SUCCESS );
    }

    updcrc(swindow, outcnt);

    PR_CHECK( char_out( swindow, outcnt ) );

    bytes_out += (unsigned long) outcnt;
    outcnt = 0;

    return( PR_SUCCESS );
}
#ifdef omit

/* ========================================================================
 * Put string s in lower case, return s.
 */

char 		*strlwr
(
    char 	*s		/* (mod) string to mod to lower case.	*/
)
{
    char 	*t;		/* pointer into string.			*/

    for (t = s; *t; t++) *t = tolow(*t);

    return s;
}

/* ========================================================================
 * Return the base name of a file (remove any directory prefix and
 * any version suffix). For systems with file names that are not
 * case sensitive, force the base name to lower case.
 */

char 		*basename
(
    char 	*fname		/* (mod) Filename to get base name of.	*/
)
{
    char 	*p;		/* pointer into filename string.	*/

    if ((p = strrchr(fname, PATH_SEP))  != NULL) fname = p+1;
#ifdef PATH_SEP2
    if ((p = strrchr(fname, PATH_SEP2)) != NULL) fname = p+1;
#endif
#ifdef PATH_SEP3
    if ((p = strrchr(fname, PATH_SEP3)) != NULL) fname = p+1;
#endif
#ifdef SUFFIX_SEP
    if ((p = strrchr(fname, SUFFIX_SEP)) != NULL) *p = '\0';
#endif
    if (casemap('A') == 'a') strlwr(fname);

    return fname;
}

/* ========================================================================
 * Make a file name legal for file systems not allowing file names with
 * multiple dots or starting with a dot (such as MSDOS), by changing
 * all dots except the last one into underlines.  A target dependent
 * function can be used instead of this simple function by defining the macro
 * MAKE_LEGAL_NAME in tailor.h and providing the function in a target
 * dependent module.
 */

void 		make_simple_name
(
    char 	*name		/* (mod) filename to make legal.	*/
)
{
    char *p = strrchr(name, '.');
				/* pointer into filename string.	*/


    if (p == NULL) return;
    if (p == name) p++;
    do {
        if (*--p == '.') *p = '_';
    } while (p != name);
}


#if defined(NO_STRING_H) && !defined(STDC_HEADERS)

/* Provide missing strspn and strcspn functions. */

#  ifndef __STDC__
#    define const
#  endif

int strspn  OF((const char *s, const char *accept));
int strcspn OF((const char *s, const char *reject));

/* ========================================================================
 * Return the length of the maximum initial segment
 * of s which contains only characters in accept.
 */

int strspn
(
    const char 	*s,		/* (in)  String to search for chars in.	*/
    const char 	*accept		/* (in)  String of chars to search for.	*/
)
{
    register const char 
    		*p;		/* Pointer into string s.		*/
    register const char 
    		*a;		/* Pointer into string a.		*/
    register int 
    		count = 0;	/* Counter. 				*/


    for (p = s; *p != '\0'; ++p) {
	for (a = accept; *a != '\0'; ++a) {
	    if (*p == *a) break;
	}
	if (*a == '\0') return count;
	++count;
    }

    return count;
}

/* ========================================================================
 * Return the length of the maximum inital segment of s
 * which contains no characters from reject.
 */

int strcspn
(
    const char	*s,		/* (in)  String to search for chars in.	*/
    const char	*reject		/* (in)  String of chars to search for.	*/
)
{
    register int 
		count = 0;	/* Counter. 				*/


    while (*s != '\0') {
	if (strchr(reject, *s++) != NULL) return count;
	++count;
    }

    return count;
}

#endif /* NO_STRING_H */

/* ========================================================================
 * Add an environment variable (if any) before argv, and update argc.
 * Return the expanded environment variable to be freed later, or NULL 
 * if no options were added to argv.
 */

#define SEPARATOR	" \t"	/* separators in env variable 		*/

char 		*add_envopt
(
    int 	*argcp,		/* pointer to argc		 	*/
    char 	***argvp,	/* pointer to argv 			*/
    char 	*env		/* name of environment variable 	*/
)
{
    char 	*p;		/* running pointer through env variable	*/
    char 	**oargv;	/* runs through old argv array		*/
    char 	**nargv;	/* runs through new argv array		*/
    int	 	oargc = *argcp;	/* old argc				*/
    int  	nargc = 0;	/* number of arguments in env variable	*/


    env = (char*)getenv(env);
    if (env == NULL) return NULL;

    p = (char*)xmalloc(strlen(env)+1);
    env = strcpy(p, env);                    /* keep env variable intact */

    for (p = env; *p; nargc++ ) {            /* move through env */
	p += strspn(p, SEPARATOR);	     /* skip leading separators */
	if (*p == '\0') break;

	p += strcspn(p, SEPARATOR);	     /* find end of word */
	if (*p) *p++ = '\0';		     /* mark it */
    }
    if (nargc == 0) {
	free(env); env = NULL;
	return NULL;
    }
    *argcp += nargc;
    /* Allocate the new argv array, with an extra element just in case
     * the original arg list did not end with a NULL.
     */
    nargv = (char**)calloc(*argcp+1, sizeof(char *));
    if (nargv == NULL) error("out of memory");
    oargv  = *argvp;
    *argvp = nargv;

    /* Copy the program name first */
    if (oargc-- < 0) error("argc<=0");
    *(nargv++) = *(oargv++);

    /* Then copy the environment args */
    for (p = env; nargc > 0; nargc--) {
	p += strspn(p, SEPARATOR);	     /* skip separators */
	*(nargv++) = p;			     /* store start */
	while (*p++) ;			     /* skip over word */
    }

    /* Finally copy the old args and add a NULL (usual convention) */
    while (oargc--) *(nargv++) = *(oargv++);
    *nargv = NULL;

    return env;
}

/* ========================================================================
 * Error handlers.
 */

void 		error
(
    char 	*m		/* (in)  Message to print.		*/
)
{
    fprintf(stderr, "\n%s: %s: %s\n", progname, ifname, m);
    abort_gzip();
}

void 		warn
(
    char 	*a, 		/* message string juxtaposed in output	*/
    char 	*b		/* message string juxtaposed in output	*/
)
{
    WARN((stderr, "%s: %s: warning: %s%s\n", progname, ifname, a, b));
}

void 		read_error
(
    void
)
{
    fprintf(stderr, "\n%s: ", progname);
    if (errno != 0) {
	perror(ifname);
    } else {
	fprintf(stderr, "%s: unexpected end of file\n", ifname);
    }
    abort_gzip();
}

void 		write_error
(
    void
)
{
    fprintf(stderr, "\n%s: ", progname);
    perror(ofname);
    abort_gzip();
}

/* ========================================================================
 * Display compression ratio on the given stream on 6 characters.
 */

void 		display_ratio
(
    long 	num;
    long 	den;
    FILE 	*file;
)
{
    long 	ratio;  	/* 1000 times the compression ratio.	*/

    if (den == 0) {
	ratio = 0; /* no compression */
    } else if (den < 2147483L) { /* (2**31 -1)/1000 */
	ratio = 1000L*num/den;
    } else {
	ratio = num/(den/1000L);
    }
    if (ratio < 0) {
	putc('-', file);
	ratio = -ratio;
    } else {
	putc(' ', file);
    }
    fprintf(file, "%2ld.%1ld%%", ratio / 10L, ratio % 10L);
}


/* ========================================================================
 * Semi-safe malloc -- never returns NULL.
 */
 
voidp i		xmalloc
(
    unsigned	size;		/* (in)  Size to malloc.		*/
)
{
    voidp 	cp = (voidp)malloc (size);
				/* pointer to malloced memory.		*/

    if (cp == NULL) error("out of memory");
    return cp;
}
#endif
