/*+
************************************************************************
****  C A N A D I A N   A S T R O N O M Y   D A T A   C E N T R E  *****
*
*   Module Name:	press/src/ux_press.c
*
*   Purpose:
*	Contains routines to do compression and decompression using the 
*	unix compress algorithm.  These routines are taken from 
*	version 4.1 of the compress program.
*
*   Routines:
*	int	ux_comp		: Compresses using teh unix compress
*				  algorithm.
*	int	ux_uncomp	: Uncompress using the unix compress 
*				  algorithm.
*
*   Date		: Feb 26, 1993
*
*   SCCS data		: @(#)
*	Module Name	: ux_press.c
*	Version Number	: 1.4
*	Release Number	: 1
*	Last Updated	: 07/04/97
*
*   Programmer		: Norman Hill 
*			Spencer W. Thomas (decvax!harpo!utah-cs!utah-gr!thomas)
*			Jim McKie	  (decvax!mcvax!jim)
*			Steve Davies	  (decvax!vax135!petsd!peora!srd)
*			Ken Turkowski	  (decvax!decwrl!turtlevax!ken)
*			James A. Woods	  (decvax!ihnp4!ames!jaw)
*			Joe Orost	  (decvax!vax135!petsd!joe)
*			Dave Mack 	  (csu@alembic.acs.com)
*
*   Modification History:
*	97/07/02 SEC  :	Bring up to ANSI standard. 
*
****  C A N A D I A N   A S T R O N O M Y   D A T A   C E N T R E  *****
************************************************************************
-*/
#include <memory.h>

#include "gen_types.h"

#include "press.h"
#include "local_press.h"
#include "ux_press.h"

static long		bytes_out;
static int		(*g_char_in)();
static int		(*g_char_out)();
static count_int	checkpoint;
static int 		clear_flg;
static unsigned short 	codetab [HSIZE];
static code_int		free_ent;	/* First unused entry.		*/
static count_int 	htab [HSIZE];
static long		in_count;
static byte		lmask[9] = {0xff, 0xfe, 0xfc, 0xf8, 0xf0, 
				0xe0, 0xc0, 0x80, 0x00};
static byte    		magic_header[] = { "\037\235" };      /* 1F 9D */
static int		maxbits;
static code_int		maxcode;
static code_int		maxmaxcode;
static int		n_bits;
static int		offset;
static long		out_count;
static int 		r_off;
static int		ratio;
static byte		rmask[9] = {0x00, 0x01, 0x03, 0x07, 0x0f, 
				    0x1f, 0x3f, 0x7f, 0xff};

/*
 *  Static function prototypes.
 */

static	int	cl_block( void );
static	int	cl_hash( count_int );
static code_int	getcode( void );
static 	int	output( code_int );




/*+
************************************************************************
*
*   Synopsis:
*	static int	cl_block( void )
*
*   Purpose:
*
*   Parameters:
*	void
*
*   Values Returned:
*	int	PR_SUCCESS	: Normal completion.
*
************************************************************************
-*/

static int	cl_block
(
    void
)
{
    long	rat;

    checkpoint = in_count + CHECK_GAP;

    if(in_count > 0x007fffff) 
    { 
	/* 
	 * shift will overflow 
	 */

        rat = bytes_out >> 8;
        if(rat == 0) 
	{ 
	    /* 
	     *  Don't divide by zero 
	     */

            rat = 0x7fffffff;
        } 
	else 
	{
            rat = in_count / rat;
        }
    } 
    else 
    {
        rat = (in_count << 8) / bytes_out;      /* 8 fractional bits */
    }
    if ( rat > ratio ) 
    {
        ratio = rat;
    } 
    else 
    {
        ratio = 0;

	cl_hash( (count_int) HSIZE );
	free_ent = FIRST;
	clear_flg = 1;
	PR_CHECK( output( (code_int) CLEAR ) );
    }

    return( PR_SUCCESS );
}


/*+
************************************************************************
*
*   Synopsis:
*	static int	cl_hash( hsize )
*
*   Purpose:
*	Clears the hash table.
*
*   Parameters:
*	count_int hsize		: (in)	The size of the hash table.
*
*   Values Returned:
*	int	PR_SUCCESS	: Normal completion.
*
************************************************************************
-*/

static int	cl_hash
(
    count_int	hsize
)
{
    memset( htab, (byte) 0xff, hsize * sizeof( count_int ) );

    return( PR_SUCCESS );
}


/*+
************************************************************************
*
*   Synopsis:
*	static code_int	getcode()
*
*   Purpose:
*	Gets the next code from the input stream.
*
*   Parameters:
*	int	(*char_in)()	: (in)	Function to get the next character
*					from the input stream.
*
*   Values Returned:
*	int	code		: Next code in the input stream.
*	int	PR_E_EOI	: End of input detected.
*
************************************************************************
-*/

static code_int	getcode
(
    void
)
{
    byte 	*bp;
    int		bits;
    static byte	buf[BITS];
    code_int	code;
    static int	offset = 0;
    static int	size = 0;

    bp = buf;

    if ( clear_flg > 0 || offset >= size || free_ent > maxcode ) 
    {
	/*
	 * If the next entry will be too big for the current code
	 * size, then we must increase the size.  This implies reading
	 * a new buffer full, too.
	 */
	if ( free_ent > maxcode ) 
	{
	    n_bits++;
	    if ( n_bits == maxbits )
	    {
		maxcode = maxmaxcode;	/* won't get any bigger now */
	    }
	    else
	    {
		maxcode = MAXCODE(n_bits);
	    }
	}
	if ( clear_flg > 0) 
	{
    	    maxcode = MAXCODE (n_bits = INIT_BITS);
	    clear_flg = 0;
	}
	offset = 0;
	if ( ( size = g_char_in( buf, n_bits ) ) <= 0 )
	{
	    size = 0;
	    return( PR_E_EOI );
	}
	/* Round size down to integral number of codes */
	size = (size << 3) - (n_bits - 1);
    }

    r_off = offset;
    bits = n_bits;


    /*
     * Get to the first byte.
     */

    bp += (r_off >> 3);
    r_off &= 7;


    /* 
     * Get first part (low order bits) 
     */

    code = (*bp++ >> r_off);
    bits -= (8 - r_off);
    r_off = 8 - r_off;		/* now, offset into code word */

    /* 
     * Get any 8 bit parts in the middle (<=1 for up to 16 bits). 
     */

    if ( bits >= 8 ) 
    {
	code |= *bp++ << r_off;
	r_off += 8;
	bits -= 8;
    }


    /* 
     * high order bits. 
     */

    code |= (*bp & rmask[bits]) << r_off;
    offset += n_bits;

    return( code );
}

/*+
************************************************************************
*
*   Synopsis:
*	int	output( code )
*
*   Purpose:
*	outputs a code.
*
*   Parameters:
*	code_int code		: (in)	Code to output.
*
*   Values Returned:
*	int	PR_SUCCESS	: Normal completion.
*
************************************************************************
-*/

int		output
(
    code_int	code
)
{
    int		bits;
    byte	*bp;
    static byte	buf[BITS];
    int		r_off;

    bp = buf;
    bits = n_bits;
    r_off = offset;

    if ( code >= 0 )
    {
	/*
	 * Get to the first byte.
	 */

	bp += (r_off >> 3);
	r_off &= 7;


	/*
	 * Since code is always >= 8 bits, only need to mask the first
	 * hunk on the left.
	 */

	*bp = ( *bp & rmask[ r_off ] ) | ( code << r_off ) & lmask[ r_off ];
	bp++;
	bits -= ( 8 - r_off );
	code >>= 8 - r_off;


	/* 
	 * Get any 8 bit parts in the middle (<=1 for up to 16 bits). 
	 */

	if ( bits >= 8 ) 
	{
	    *bp++ = code;
	    code >>= 8;
	    bits -= 8;
	}


	/* 
	 * Last bits. 
	 */

	if(bits)
	{
	    *bp = code;
	}

	offset += n_bits;
	if ( offset == ( n_bits << 3 ) ) 
	{
	    bp = buf;
	    bits = n_bits;
	    bytes_out += bits;
	    PR_CHECK( g_char_out( bp, bits ) );
	    bits = 0;
	    offset = 0;
	}


	/*
	 * If the next entry is going to be too big for the code size,
	 * then increase it, if possible.
	 */

	if ( free_ent > maxcode || ( clear_flg > 0 ) )
	{
	    /*
	     * Write the whole buffer, because the input side won't
	     * discover the size increase until after it has read it.
	     */

	    if ( offset > 0 ) 
	    {
		PR_CHECK( g_char_out( buf, n_bits ) );
	    }
	    offset = 0;

	    if ( clear_flg ) 
	    {
    	        maxcode = MAXCODE( n_bits = INIT_BITS );
	        clear_flg = 0;
	    }
	    else 
	    {
	    	n_bits++;
	    	if ( n_bits == maxbits )
		{
		    maxcode = maxmaxcode;
		}
	    	else
		{
		    maxcode = MAXCODE( n_bits );
		}
	    }
	}
    } 
    else 
    {
	/*
	 * At EOF, write the rest of the buffer.
	 */

	if ( offset > 0 )
	{
	    PR_CHECK( g_char_out( buf, (offset + 7) / 8 ) );
	}
	bytes_out += (offset + 7) / 8;
	offset = 0;
    }
    return( PR_SUCCESS );
}

/*+
************************************************************************
*
*   Synopsis:
*	int	ux_comp( char_in, char_out )
*
*   Purpose:
*	Compresses data using the unix compress algorithm.
* 	Algorithm:  use open addressing double hashing (no chaining) on the
*	prefix code / next character combination.  We do a variant of Knuth's
*	algorithm D (vol. 3, sec. 6.4) along with G. Knott's relatively-prime
*	secondary probe.  Here, the modular division first probe is gives way
*	to a faster exclusive-or manipulation.  Also do block compression with
*	an adaptive reset, whereby the code table is cleared when the compression
*	ratio decreases, but after the table fills.  The variable-length output
*	codes are re-sized at this point, and a special CLEAR code is generated
*	for the decompressor.  Late addition:  construct the table according to
*	file size for noticeable speed improvement on small files.  Please direct
*	questions about this implementation to ames!jaw.
*
*   Parameters:
*	int	(*char_in)()	: (in)	Function to get the next input
*					character.
*	int	(*char_out)()	: (in)	Function to write the next output
*					character.
*
*   Values Returned:
*	int	PR_SUCCESS	: Normal completion.
*
************************************************************************
-*/

int		ux_comp
(
    pfi		char_in,
    pfi		char_out
)
{
    int		block_compress;
    int 	c;
    byte	chr;
    int		disp;
    code_int	ent;
    long	fcode;
    int		hshift;
    code_int	i = 0;

    
    g_char_in = char_in;
    g_char_out = char_out;

    /*
     *  write the magic number.
     */

    PR_CHECK( char_out( magic_header, 2 ) ); 
    block_compress = BLOCK_MASK;
    chr = (byte) ( BITS | BLOCK_MASK );
    PR_CHECK( char_out( &chr, 1 ) );

    offset = 0;
    bytes_out = 3;		/* includes 3-byte header mojo */
    out_count = 0;
    clear_flg = 0;
    ratio = 0;
    in_count = 1;
    maxbits = BITS;
    maxmaxcode = 1 << maxbits;
    checkpoint = CHECK_GAP;
    maxcode = MAXCODE( n_bits = INIT_BITS );
    free_ent = ( ( block_compress ) ? FIRST : 256 );

    PR_CHECK( char_in( &chr, 1 ) );
    ent = (code_int) chr;

    hshift = 0;
    for ( fcode = (long) HSIZE;  fcode < 65536L; fcode *= 2L )
    {
    	hshift++;
    }
	

    /* 
     *  set hash code range bound 
     */

    hshift = 8 - hshift;		

    
    /*
     *  clear hash table.
     */

    cl_hash( (count_int) HSIZE);

    while ( char_in( &chr, 1 ) == 1 ) 
    {
	c = (int) chr;
	in_count++;
	fcode = (long) ( ( (long) c << maxbits) + ent);
 	i = ( ( c << hshift ) ^ ent );	/* xor hashing */

	if ( htabof( i ) == fcode ) 
	{
	    ent = codetabof( i );
	    continue;
	} 
	else if ( (long)htabof( i ) < 0 )	/* empty slot */
	{
	    goto nomatch;
	}
 	disp = HSIZE - i;		/* secondary hash (after G. Knott) */
	if ( i == 0 )
	{
	    disp = 1;
	}
probe:
	if ( (i -= disp) < 0 )
	{
	    i += HSIZE;
	}

	if ( htabof( i ) == fcode ) 
	{
	    ent = codetabof( i );
	    continue;
	}
	if ( (long)htabof( i ) > 0 ) 
	{
	    goto probe;
	}
nomatch:
	PR_CHECK( output( ent ) );
	out_count++;
 	ent = c;

	if ( free_ent < maxmaxcode ) 
	{
 	    codetabof( i ) = free_ent++;	/* code -> hashtable */
	    htabof( i ) = fcode;
	}
	else if ( (count_int)in_count >= checkpoint && block_compress )
	{
	    cl_block();
	}
    }


    /*
     * Put out the final code.
     */

    PR_CHECK( output( ent ) );
    out_count++;
    PR_CHECK( output( -1 ) );


    return( PR_SUCCESS );
}

/*+
************************************************************************
*
*   Synopsis:
*	int	ux_uncomp( char_in, char_out )
*
*   Purpose:
*	Decompresses a memory buffer into a second buffer.
*	Statement of purpose.
*
*   Parameters:
*	int	(*char_in)()	: (in)	Function to get a compressed char.
*	int	(*char_out)()	: (in)	Function to put an uncompressed char.
*
*   Values Returned:
*	int	PR_SUCCESS	: Normal completion.
*	int	PR_E_BITS	: More bits than can be handled.
*	int	PR_E_MAGIC	: Incorrect magic number.
*
*   References:
*	Compress compresses files using a heavily modified version of the
*	LZW algorithm as described in IEEE Computer, June 1984.
*
************************************************************************
-*/

int		ux_uncomp
(
    pfi		char_in,
    pfi		char_out
)
{
    int		block_compress;
    byte	buffer[2];
    byte	chr;
    code_int	code;
    int		finchar;
    code_int	incode;
    code_int	oldcode;		/* The previous code.		*/
    byte	*stackp;

    g_char_in = char_in;
    g_char_out = char_out;

    free_ent = 0;
    
    /*
     *  Check to see if the magic number is correct.
     */

    if ( char_in( buffer, 2 ) != 2 || memcmp( magic_header, buffer, 2 ) != 0 )
    {
	pr_format_message( PR_E_MAGIC );
	return( PR_E_MAGIC );
    }

    PR_CHECK( char_in( buffer, 1 ) );
    maxbits = (int) buffer[0];
    block_compress = maxbits & BLOCK_MASK;
    maxbits &= BIT_MASK;
    maxmaxcode = 1 << maxbits;

    if ( maxbits > BITS )
    {
	pr_format_message( PR_E_BITS, BITS );
	return( PR_E_BITS );
    }


    /*
     * As above, initialize the first 256 entries in the table.
     */

    maxcode = MAXCODE(n_bits = INIT_BITS);
    for ( code = 255; code >= 0; code-- ) 
    {
	tab_prefixof(code) = 0;
	tab_suffixof(code) = (byte)code;
    }

    free_ent = ((block_compress) ? FIRST : 256 );

    finchar = oldcode = getcode();


    /*
     *  Check for EOF.
     */

    if( oldcode == PR_E_EOI )
    {
	return( PR_SUCCESS );
    }


    /*
     *  Write the first character.
     */

    chr = (byte) finchar;
    PR_CHECK( char_out( &chr, 1 ) );

    stackp = de_stack;

    while ( ( code = getcode() ) > -1 ) 
    {
	if ( ( code == CLEAR ) && block_compress ) 
	{
	    for ( code = 255; code >= 0; code-- )
	    {
		tab_prefixof( code ) = 0;
	    }
	    clear_flg = 1;
	    free_ent = FIRST - 1;
	    if ( ( code = getcode () ) == PR_E_EOI )	
	    {
		/* 
		 * O, untimely death! 
		 */

		break;
	    }
	}
	incode = code;


	/*
	 * Special case for KwKwK string.
	 */

	if ( code >= free_ent ) 
	{
            *stackp++ = finchar;
	    code = oldcode;
	}


	/*
	 * Generate output characters in reverse order
	 */

	while ( code >= 256 ) 
	{
	    *stackp++ = tab_suffixof(code);
	    code = tab_prefixof(code);
	}
	*stackp++ = finchar = tab_suffixof(code);


	/*
	 * And put them out in forward order
	 */

	do
	{
	    PR_CHECK( char_out ( --stackp, 1 ) );
	}
	while ( stackp > de_stack );


	/*
	 * Generate the new entry.
	 */

	if ( ( code=free_ent ) < maxmaxcode ) 
	{
	    tab_prefixof( code ) = (unsigned short) oldcode;
	    tab_suffixof( code)  = finchar;
	    free_ent = code+1;
	} 


	/*
	 * Remember previous code.
	 */

	oldcode = incode;
    }
    return( PR_SUCCESS );
}
