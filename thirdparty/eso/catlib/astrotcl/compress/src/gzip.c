/*+
************************************************************************
****  C A N A D I A N   A S T R O N O M Y   D A T A   C E N T R E  *****
*
*   Module Name:	press/src/gzip.c
*
*   Purpose:
* 	decompress files in gzip format.
*	Copyright (C) 1992-1993 Jean-loup Gailly
*	This is free software; you can redistribute it and/or modify it under 
*	the terms of the GNU General Public License, see the file COPYING.
*	This version can extract files in gzip format.
*
*	The code in this file is derived from the file funzip.c written
*	and put in the public domain by Mark Adler.
*
*   Routines:
*	int	gzip_comp	: Uncompresses gzipped data.
*	int	unzip		: Compress data into gzip format. 
*
*   Date		: June 21, 1993
*
*   SCCS data		: @(#)
*	Module Name	: gzip.c
*	Version Number	: 1.8
*	Release Number	: 1
*	Last Updated	: 07/16/97
*
*   Programmer		: Norman Hill
*
*   Modification History:
*	97/07/02 SEC  :	Bring up to ANSI C. 
*
****  C A N A D I A N   A S T R O N O M Y   D A T A   C E N T R E  *****
************************************************************************
-*/

/* allan: 8.8.96, for configure */
#include "config.h"

#include <stdio.h>

#ifdef HAVE_SYS_FILIO_H
#include <sys/filio.h>
#endif

#include <string.h>
#include <fcntl.h>
#include <memory.h>

#if ! defined( _SOL ) && ! defined( lint )
#include <unistd.h>
#endif

/* allan: 2.8.96, changed #ifdef for configure */
#ifdef HAVE_VFORK_H
#include <vfork.h>
#endif

#include "gen_types.h"
#include "gen_msg.h"

#include "press.h"
#include "local_press.h"
#include "gzip.h"

/* allan: 15-03-99: moved globals to here from gzip.h to avoid linker warning */
unsigned short	*dbuf;		/* buffer for distances, see trees.c	*/
unsigned char	*swindow;	/* Sliding window and suffix table (unlzw) */
unsigned short	*tab_prefix;	/* prefix code (see unlzw.c)		*/


unsigned	inptr;		/* Index of next byte to be processed 	*/
				/* in inbuf.				*/
unsigned	insize;		/* valid bytes in inbuf 		*/
pfi		char_in;	/* Function to read bytes.		*/
pfi		char_out;	/* Function to write bytes.		*/

long	bytes_out;		/* The number of output bytes.		*/

#define	EXTHDR	16		/* Size of extend local header.		*/

/*+
************************************************************************
*
*   Synopsis:
*	int	gzip_comp( char_in, char_out )
*
*   Purpose:
*	Statement of purpose.
*
*   Parameters:
*	int	(*char_in)()	: (in)	Function to get data from input.
*	int	(*char_out)()	: (in)	Function to write data to output.
*
*   Values Returned:
*	int	PR_SUCCESS	: Normal completion.
*	int	PR_E_IO		: Error during io.
*
************************************************************************
-*/

int	gzip_comp
( 
    pfi		char_in,	/* (in)  Func. to get data from input.	*/
    pfi		char_out	/* (in)  Func. to put data to output.	*/
)
{
    /* void	pr_format_message(); */

    byte	out_buffer[4096];
    byte	in_buffer[4096];
    byte	*buf_ptr;
    int		in_pipe[2];
    int		bytes_left;
    int		bytes_read;
    int		bytes_written;
    int		out_pipe[2];
    int		tmp_stdin;
    int		tmp_stdout;
    int		status;


    tmp_stdin =  dup( 0 );
    tmp_stdout =  dup( 1 );
    PR_CHECK_IO( pipe( in_pipe ), "in pipe" );
    PR_CHECK_IO( pipe( out_pipe ), "out pipe" );
    (void) dup2( in_pipe[1], 1 );
    (void) dup2( out_pipe[0], 0 );

    if ( ( status = vfork() ) == 0 )
    {
	(void) close( in_pipe[0] );
	(void) close( out_pipe[1] );
	(void) execlp( "gzip", "gzip", "-c", NULL );
	pr_format_message( MSG_ERRNO, "gzip" );
	_exit( 999 );
    }
    PR_CHECK_IO( status, "vfork" );

    (void) close( in_pipe[1] );
    (void) close( out_pipe[0] );

    (void) dup2( tmp_stdin, 0 );
    (void) dup2( tmp_stdout, 1 );
    (void) close( tmp_stdin );
    (void) close( tmp_stdout );
    PR_CHECK_IO( fcntl( out_pipe[1], F_SETFL, O_NDELAY | O_WRONLY ), "fcntl" );
    PR_CHECK_IO( fcntl( in_pipe[0], F_SETFL, O_NDELAY ), "fcntl" );

    while ( ( bytes_left = char_in( out_buffer, 4096 ) ) != PR_E_EOI )
    {
	buf_ptr = out_buffer;
	do  
	{
	    bytes_written = 
		    write( out_pipe[1], buf_ptr, bytes_left );
	    if ( bytes_written < 0 )
	    {
		bytes_written = 0;
	    }
	    do
	    {
		bytes_read = read( in_pipe[0], in_buffer, 4096 );
		if ( bytes_read > 0 )
		{
		    PR_CHECK( char_out( in_buffer, bytes_read ) );
		}
	    } while ( bytes_read > 0 );
	    if ( bytes_read <= 0 && bytes_written <= 0 )
	    {
		/* sleep( 1 ); allan: 22.3.98: commented out: very bad for performance... */
	    }
	    buf_ptr += bytes_written;
	    bytes_left -= bytes_written;
	} while ( bytes_left > 0 );
    }
 
    close( out_pipe[1] );
    PR_CHECK_IO( fcntl( in_pipe[0], F_SETFL, O_RDONLY ), "fcntl" );
    do
    {
        bytes_read = read( in_pipe[0], in_buffer, 4096 );
        if ( bytes_read > 0 )
        {
            PR_CHECK( char_out( in_buffer, bytes_read ) );
        }
    } while ( bytes_read > 0 );
    close( in_pipe[0] );

    return( PR_SUCCESS );
}

/*+
************************************************************************
*
*   Synopsis:
*	int	unzip( p_char_in, p_char_out )
*
*   Purpose:
*	Function to uncompress gzip files.
*
*   Parameters:
*	int	(*p_char_in)()	: (in)	Function to get the next input 
*					character.
*	int	(*p_char_out)()	: (in)	Function to send data to the output.
*
*   Values Returned:
*	int	PR_SUCCESS	: Normal completion.
*	int	PR_E_EOI	: End of input detected.
*	int	PR_E_MAGIC	: Invalid magic number.
*	int	PR_E_METHOD	: Unknown compression method.
*
************************************************************************
-*/

int gzip_uncomp
( 
    pfi		p_char_in,	/* (in)  Func. to get char from input.	*/
    pfi		p_char_out	/* (in)  Func. to put char to output.	*/
)
{
    /* void	pr_format_message(); */

    unsigned char
    		buff[EXTHDR];	/* extended local header 	*/
    char	c;
    byte	dummy[6];
    byte	flags;		/* Compression flags.		*/
    byte	magic[2];	/* The magic number.		*/
    byte	method;		/* The compress			*/
    unsigned long
    		orig_crc;	/* original crc			*/
    unsigned long
    		orig_len;   	/* original uncompressed length */


    char_in = p_char_in;
    char_out = p_char_out;


    orig_crc = 0;
    orig_len = 0;


    /*
     *  Check the magic number and compression type.
     */

    PR_CHECK( char_in( magic, 2 ) );
    if ( memcmp( magic, GZIP_MAGIC, 2 ) != 0 )
    {
	pr_format_message( PR_E_MAGIC );
	return( PR_E_MAGIC );
    }


    PR_CHECK( char_in( &method, 1 ) );
    if ( method != DEFLATED )
    {
	pr_format_message( PR_E_METHOD, method );
	return( PR_E_METHOD );
    }

    PR_CHECK( char_in( &flags, 1 ) );
    if ( flags & ENCRYPTED || flags & CONTINUATION || flags & RESERVED )
    {
	pr_format_message( PR_E_UNSUPPORT, "" );
	return( PR_E_UNSUPPORT );
    }


    /*
     *  Skip over time stamp, extra flags, and os.
     */

    PR_CHECK( char_in( dummy, 6 ) );


    if ( ( flags & EXTRA_FIELD ) != 0 )
    {
	/*
	 *  Skip the file length.
	 */

	PR_CHECK( char_in( dummy, 2 ) );
    }


    /*
     *  Skip over the original file name.
     */

    if ( ( flags & ORIG_NAME ) != 0 )
    {
	do
	{
	    PR_CHECK( char_in( &c, 1 ) );
	} while ( c != '\0' );
    }


    /*
     *  skip over the comment.
     */

    if ( ( flags & COMMENT ) != 0 )
    {
	do
	{
	    PR_CHECK( char_in( &c, 1 ) );
	} while ( c != '\0' );
    }


    /* 
     * initialize crc 
     */

    (void) updcrc(NULL, 0);

    /* 
     * Decompress 
     */

    PR_CHECK( gzip_inflate() );


    /* 
     *  Get the crc and original length 
     */

    /* 
     * crc32  (see algorithm.doc)
     * uncompressed input size modulo 2^32
     */

    PR_CHECK( char_in( buff, 8 ) );
    orig_crc = LG(buff);
    orig_len = LG(buff+4);


    /* 
     * Validate decompression 
     */

#ifdef OMIT
    if ( orig_crc != updcrc(buff, 0)) 
    {
	pr_format_message( PR_E_CRC );
    }
#endif
    if (orig_len != bytes_out) 
    {
	pr_format_message( PR_E_SIZE );
    }

    return( PR_SUCCESS );
}
