/*+
************************************************************************
****  C A N A D I A N   A S T R O N O M Y   D A T A   C E N T R E  *****
*
*   Module Name:	press/src/h_uncomp.c
*
*   Purpose:
*	Countains routines to decompress hcompressed data.
*
*   Routines:
*	static int  decode		: Decode hcompressed data.
*	static int  dodecode		: Do the decode.
*	static int  fitspass		: Pass fits header from input to output.
*	int	    h_uncomp		: Uncompresses hcompressed data.
*	static int  input_bit		: Gets a single bit from the input.
*	static int  input_huffman	: Huffman decoding for fixed codes.
*	static int  input_nbits		: Get a specified number of bits from
*					  the input.
*	static int  makefits		: Write a simple fits header for a 
*					  2-d image.
*	static void qtree_bitins	:
*	static void qtree_copy		: 
*	static int  qtree_decode	: Turn a stream of bits into a 
*					  bitplanes.
*	static int  qtree_expand	: Do one quadtree expansion step.
*	static int  read_bdirect	: Reads an image packed 4 bits per 
*					  byte.
*	static void start_inputing_bits	: Initializes the bit input functions.
*
*   Date		: Mar 15, 1993
*
*   SCCS data		: @(#)
*	Module Name	: h_decomp.c
*	Version Number	: 1.7
*	Release Number	: 1
*	Last Updated	: 07/04/97
*
*   Programmer		: Norman Hill
*
*   Modification History:
*	97/07/02 SEC  :	Bring up to ANSI standard.
*
****  C A N A D I A N   A S T R O N O M Y   D A T A   C E N T R E  *****
************************************************************************
-*/

#include <math.h>
#include <stdio.h>
#include <sys/types.h>
#include <netinet/in.h>

#include "gen_types.h"
#include "gen_str.h"
#include "gen_msg.h"

#include "press.h"
#include "local_press.h"
#include "h_comp.h"

#define input_nybble(char_in)	input_nbits(char_in,4)

/* THE BIT BUFFER */
static int  buffer;		/* Bits waiting to be input		 */
static int  bits_to_go;		/* Number of bits still in buffer	 */

/* 
 *  Static function prototypes.
 */

static	int	decode( );
static	int	dodecode( pfi, int *, int, int, byte * ); 
static	int	fitspass( pfi, boolean, pfi );
static	int	makefits( pfi, int, int, int, char * );
static	int	input_bit( pfi	);
static 	int	input_nbits( pfi, int );
static 	int	qtree_decode( pfi, int *, int, int, int, int );
static 	void	start_inputing_bits( void );
static 	int	input_huffman( pfi );
static	void	qtree_bitins( byte *, int, int, int *, int, int );
static	void	qtree_copy( byte *, int, int, byte *, int );
static int	qtree_expand ( pfi, byte *, int, int, byte * );
static int	read_bdirect ( pfi, int *, int, int, int, byte *, int );


/*
 * allan: 17.12.97: replaced a non-portable #define with this routine
 */
int read_int(pfi char_in, int* i)
{
    int status = char_in((byte *)i, sizeof(int));
    *i = ntohl(*i);
    return status;
}



/*+
************************************************************************
*
*   Synopsis:
*	static int decode( char_in, char_out, a, nx, ny, scale, format )
*
*   Purpose:
*	Reads codes from an hcompressed file and creates an array.
*
*   Parameters:
*	int	(*char_in)()	: (in)	Function to read data.
*	int	(*char_out)()	: (in)	Function to write data.
*	int	**a		: (out)	Address of output array.
*	int	*nx		: (out)	Size on x axis.
*	int	*ny		: (out)	Size on y axis.
*	int	*scale		: (out)	Scale factor of digitization.
*	char	**format	: (mod)	A string indicating the output type.
*
*   Values Returned:
*	int	PR_SUCCESS	: Normal completion.
*	int	PR_E_FORMAT	: Data format was incorrect.
*	int	PR_E_IO		: Error during io.
*	int	PR_E_MEMORY	: Memory allocation failure.
*
************************************************************************
-*/

static int	decode
(
    pfi		char_in,	/* (in)  Function to read data. 	*/
    pfi		char_out,	/* (in)  Function to write data. 	*/
    int		**a,		/* (out) Address of output array. 	*/
    int		*nx,		/* (out) Size on x axis. 		*/
    int		*ny,		/* (out) Size on y axis. 		*/
    int		*scale,		/* (out) Scale factor of digitization. 	*/
    char	**format 	/* (mod) String of the output type. 	*/
)
{
    /* int		dodecode(); */
    /* static int	fitspass(); */
    /* static int	makefits(); */
    /* void	pr_format_message(); */

    int		nel;
    int		sumall;
    int		newfits = 0;
    byte	nbitplanes[3];
    char	tmagic[2];
    char	line[81];


    /*
     *  File starts either with special 2-byte magic code or with
     *  FITS keyword "SIMPLE  ="
     */

    PR_CHECK( char_in( tmagic, sizeof( tmagic ) ) );


    /*
     * Check for FITS
     */

    if ( strncmp( tmagic, "SI", 2 ) == 0 ) 
    {
	/*
	 * read rest of line and make sure the whole keyword is correct
	 */

	(void) strncpy( line, "SI", 2 );
	if ( char_in( line + 2, 78 ) != 78 )
	{
	    pr_format_message( PR_E_FORMAT );
	    return( PR_E_FORMAT );
	}

	line[80] = '\0';

	if ( strncmp( line, "SIMPLE  =", 9) != 0 )  
	{
	    pr_format_message( PR_E_FORMAT );
	    return( PR_E_FORMAT );
	}


	/*
	 * set output format to default "fits" if it is empty
	 */

	if ( ( *format )[0] == '\0' ) 
	{
	    *format = "fits";
	}


	/*
	 * if fits output format and outfile != NULL, write this line to
	 * outfile and then copy rest of FITS header; else just read past
	 * FITS header.
	 */

	if ( strcmp( *format, "fits" ) == 0 ) 
	{
	    char_out( line, 80 );
	    PR_CHECK( fitspass( char_in, TRUE, char_out ) );
	} 
	else 
	{
	    PR_CHECK( fitspass( char_in, FALSE, char_out) );
	}


	/*
	 * now read the next two characters -- this time they
	 * MUST be the magic code!
	 */

	PR_CHECK( char_in( tmagic, sizeof( tmagic ) ) );
    } 
    else 
    {
	/*
	 * set default format to raw if it is not specified
	 */

	if( ( *format )[0] == '\0' )
	{
	    *format = "raw";
	}


	/*
	 * if input format is not FITS but output format is FITS, set
	 * a flag so we generate a FITS header once we know how big
	 * the image must be.
	 */

	if ( strcmp( *format, "fits" ) == 0) 
	{
	    newfits = 1;
	}
    }


    /*
     * check for correct magic code value
     */

    if ( memcmp( tmagic, code_magic, sizeof( code_magic ) ) != 0) 
    {
	    pr_format_message( PR_E_FORMAT );
	    return( PR_E_FORMAT );
    }


    /*
     *  Read the size of the image and the scale factor for digitization.
     */

    PR_CHECK( read_int( char_in, nx ) );
    PR_CHECK( read_int( char_in, ny ) );
    PR_CHECK( read_int( char_in, scale ) );


    /*
     * write the new fits header to outfile if needed
     */

    if ( newfits )
    {
	PR_CHECK( makefits( char_out, *nx, *ny, 16, "INTEGER*2" ) );
    }


    /*
     * allocate memory for array
     */

    nel = (*nx) * (*ny);
    PR_CHECK_NULL( *a = int_alloc( nel ) );


    /* 
     *  Read the sum of all pixels	
     */

    PR_CHECK( read_int( char_in, &sumall ) );


    /* 
     *  Read the number of bits in quadrants.
     */

    PR_CHECK( char_in( nbitplanes, sizeof( nbitplanes ) ) );

    PR_CHECK( dodecode( char_in, *a, *nx, *ny, nbitplanes ) );


    /*
     * put sum of all pixels back into pixel 0
     */

    (*a)[0] = sumall;

    return( PR_SUCCESS );
}

/*+
************************************************************************
*
*   Synopsis:
*	static int	dodecode( char_in, a, nx, ny, nbitplanes )
*
*   Purpose:
*	Decode stream of characters and return array
*	This version encodes the different quadrants separately
*
*   Parameters:
*	int	(*char_in)()	: (in)	Function to get the next data.
*	int	a[]		: (in)  Array to be created.
*	int	nx		: (in)  X axis dimension.
*	int	ny		: (in)  Y axis dimension.
*	int	nbitplanes	: (in)	Number of bitplanes in quadrants.
*
*   Values Returned:
*	int	PR_SUCCESS	: Normal completion.
*	int	PR_E_BITPLANE: Bad bit plane. 
*	int	PR_E_CODE	: Bad format code.
*	int	PR_E_IO		: Error during io.
*	int	PR_E_MEMORY	: Memory allocation failure.
*
*   References:
*	Copied from function dodecode from the hcompress program.
*	Programmer: R. White		Date: 9 May 1991
*
************************************************************************
-*/

static int	dodecode
(
    pfi		char_in,
    int		a[],
    int		nx,
    int		ny,
    byte	nbitplanes[3]
)
{
    /* static int	input_bit(); */
    /* static int	input_nbits(); */
    /* static int	qtree_decode(); */
    /* static void start_inputing_bits(); */

    byte	bit;		/* The value of a bit.			*/
    int		i;
    int		nel;
    int		nx2;
    int		ny2;


    nel = nx*ny;
    nx2 = (nx+1)/2;
    ny2 = (ny+1)/2;


    /*
     * initialize a to zero
     */

    for ( i = 0; i < nel; i++ ) 
    {
	a[i] = 0;
    }


    /*
     * Initialize bit input
     */

    start_inputing_bits();


    /*
     * read bit planes for each quadrant
     */

    PR_CHECK( qtree_decode( char_in, &a[0],          ny, nx2,  ny2,  
	    nbitplanes[0] ) );
    PR_CHECK( qtree_decode( char_in, &a[ny2],        ny, nx2,  ny/2, 
	    nbitplanes[1] ) );
    PR_CHECK( qtree_decode( char_in, &a[ny*nx2],     ny, nx/2, ny2,  
	    nbitplanes[1] ) );
    PR_CHECK( qtree_decode( char_in, &a[ny*nx2+ny2], ny, nx/2, ny/2, 
	    nbitplanes[2] ) );


    /*
     * make sure there is an EOF symbol (nybble=0) at end
     */

    if ( input_nybble( char_in ) != 0 ) 
    {
	pr_format_message( PR_E_BITPLANE );
	return( PR_E_BITPLANE );
    }


    /*
     * now get the sign bits
     * Re-initialize bit input
     */

    start_inputing_bits();
    for ( i = 0; i < nel; i++)
    {
	if ( a[i] != 0 )
	{
	    PR_CHECK( bit = input_bit( char_in ) ); 
	    if ( bit != 0 ) 
	    {
		a[i] = -a[i];
	    }
	}
    }

    return( PR_SUCCESS );
}

/*+
************************************************************************
*
*   Synopsis:
*	static int	fitspass( char_in, passthru, char_out )
*
*   Purpose:
*	Skip over the fits header in the input, and optionaly copy it to
*	the output.
*
*   Parameters:
*	int	(*char_in)()	: (in)	Function to get data from the input.
*	boolean	passthru	: (in)	True if the header should be written
*					to the output.
*	int	(*char_out)()	: (in)	Function to write data to the output.
*
*   Values Returned:
*	int	PR_SUCCESS	: Normal completion.
*	int	PR_E_IO		: Error during io.
*	int	PR_E_EOI	: End of input.
*
*   References:
*	Copied from function dodecode from the hcompress program.
*	Programmer: R. White		Date: 16 April 1992
*
************************************************************************
-*/

static int	fitspass
(
    pfi		char_in,
    boolean	passthru,
    pfi		char_out 
)
{
    char	line[81];
    int		i;
    int		j;


    /*
     * Note that the SIMPLE line has already been stripped off and
     * written to outfile for FITS files, so we start at i=1
     */

    for ( i = 1;; i++ )
    {
	PR_CHECK( char_in( line, 80 ) );

	if ( passthru )
	{
	    PR_CHECK( char_out( line, 80 ) );
	}

	if ( strncmp(line, "END ", 4) == 0)
	{
	    break;
	}
    }


    /*
     * write blank lines to make a multiple of 36 lines in header
     * number of lines written so far is i+1
     */

    if ( passthru )
    {
	for ( j = 0; j < 80; j++ )
	{
	    line[j] = ' ';
	}
	line[80] = '\0';
	for ( i = 35 - ( i % 36 ); i > 0; i-- )
	{
	    PR_CHECK( char_out( line, 80 ) );
	}
    }

    return( PR_SUCCESS );
}

/*+
************************************************************************
*
*   Synopsis:
*	int	h_uncomp( char_in, char_out )
*
*   Purpose:
*	Uncompresses h compressed data.
*
*   Parameters:
*	byte	(*char_in)()	: (in)	Function to get data from input.
*	byte	(*char_out)()	: (in)	Function to write data to output.
*
*   Values Returned:
*	int	PR_SUCCESS	: Normal completion.
*	int	PR_E_FORMAT	: Data format was incorrect.
*	int	PR_E_IO		: Error during io.
*	int	PR_E_MEMORY	: Memory allocation failure.
*
************************************************************************
-*/

int		h_uncomp
(
    pfi		char_in,
    pfi		char_out 
)
{
    /* static int	decode(); */
    int		h_put_data();
    void	hinv();
    /* void	pr_format_message(); */
    void	undigitize();

    int		*a;		/* Pointer to the image array.		*/
    int		nx;		/* Size in x axis.			*/
    int		ny;		/* Size in y axis.			*/
    int		scale;		/* Scale factor of compression.		*/
    int		status;		/* Function return status.		*/

    PR_CHECK( decode( char_in, char_out, &a, &nx, &ny, &scale, 
	    &( local_press.lp_format ) ) );

    undigitize( a, nx, ny, scale );


    /*
     *  Inverse H-tranform
     */

    hinv( a, nx, ny, local_press.lp_smooth, scale );


    /* 
     *  Write the data.
     */

    status = h_put_data( char_out, a, nx, ny, local_press.lp_format );

    free( a );

    PR_CHECK( status );

    if ( local_press.lp_verbose )
    {
	pr_format_message( PR_HDECOMP_INFO, local_press.lp_smooth, nx, 
		ny, scale, local_press.lp_format );
    }

    return( PR_SUCCESS );
}

/*+
************************************************************************
*
*   Synopsis:
*	static int	input_bit( char_in )
*
*   Purpose:
*	input a bit.
*
*   Parameters:
*	int	(*char_in)()	: (in)	Gets data from input.
*
*   Values Returned:
*	int	value		: if value > 0, the bit value.
*	int	PR_E_IO		: Error during io.
*	int	PR_E_EOI	: End of input.
*
************************************************************************
-*/

static int	input_bit
(
    pfi		char_in
)
{
    byte	b;

    if ( bits_to_go == 0 )
    {				
	/* 
	 *  Read the next byte if no	 
	 */

	PR_CHECK( char_in( &b, 1 ) );
	buffer = b;
	bits_to_go = 8;
    }


    /*
     * Return the next bit
     */

    bits_to_go -= 1;
    return ( ( buffer >> bits_to_go ) & 1 );
}

/*+
************************************************************************
*
*   Synopsis:
*	static int	input_huffman( char_in )
*
*   Purpose:
*	 Huffman decoding for fixed codes
*	
*	 Coded values range from 0-15
*	
*	 Huffman code values ( hex ):
*	
*		3e, 00, 01, 08, 02, 09, 1a, 1b,
*		03, 1c, 0a, 1d, 0b, 1e, 3f, 0c
*	
*	 and number of bits in each code:
*	
*		6,  3,  3,  4,  3,  4,  5,  5,
*		3,  5,  4,  5,  4,  5,  6,  4
*		Statement of purpose.
*	
*   Parameters:
*	int	(*char_in)()	: (in)	Function to read data from input.
*
*   Values Returned:
*	int	code		: if value > 0.
*	int	PR_E_IO		: Error during io.
*
*   References:
*	Copied from the hcompress function input_huffman.
*	Programmer: R. White		Date: 7 May 1991
*
************************************************************************
-*/

static int	input_huffman
(
    pfi		char_in
)
{
    /* static int	input_bit(); */

    int		b;
    int		c;


    /*
     * get first 3 bits to start
     */

    PR_CHECK( c = input_nbits( char_in, 3 ) );
    if ( c < 4 )
    {
	/*
	 * this is all we need return 1,2,4,8 for c=0,1,2,3
	 */

	return ( 1 << c );
    }


    /*
     * get the next bit
     */

    PR_CHECK( b = input_bit( char_in ) );
    c = b | ( c << 1 );
    if ( c < 13 )
    {
	/*
	 * OK, 4 bits is enough
	 */

	switch ( c )
	{
	  case 8:
	    return ( 3 );
	  case 9:
	    return ( 5 );
	  case 10:
	    return ( 10 );
	  case 11:
	    return ( 12 );
	  case 12:
	    return ( 15 );
	}
    }


    /*
     * get yet another bit
     */

    PR_CHECK( b = input_bit( char_in ) );
    c = b | ( c << 1 );
    if ( c < 31 )
    {
	/*
	 * OK, 5 bits is enough
	 */

	switch ( c )
	{
	  case 26:
	    return ( 6 );
	  case 27:
	    return ( 7 );
	  case 28:
	    return ( 9 );
	  case 29:
	    return ( 11 );
	  case 30:
	    return ( 13 );
	}
    }


    /*
     * need the 6th bit
     */

    PR_CHECK( b = input_bit( char_in ) );
    c = b | ( c << 1 );
    if ( c == 62 )
    {
	return ( 0 );
    }
    else
    {
	return ( 14 );
    }
}

/*+
************************************************************************
*
*   Synopsis:
*	static int	input_nbits( char_in, n )
*
*   Purpose:
*	Input n bits, n <= 8.
*
*   Parameters:
*	int	(*char_in)()	: (in)	Function to read data from input.
*	int	n		: (in)	Number of bytes to read.
*
*   Values Returned:
*	int	value		: if > 0, value of the bits 
*	int	PR_E_EOI	: End of input detected.
*	int	PR_E_IO		: Error during io.
*
************************************************************************
-*/

static int	input_nbits
(
    pfi		char_in,
    int		n
)
{
    byte	c;


    if ( bits_to_go < n )
    {
	/*
	 * need another byte's worth of bits
	 */

	buffer <<= 8;
	PR_CHECK( char_in( &c, 1 ) );
	buffer |= c;
	bits_to_go += 8;
    }


    /*
     * now pick off the first n bits
     */

    bits_to_go -= n;
    return ( ( buffer >> bits_to_go ) & ( ( 1 << n ) - 1 ) );
}

/*+
************************************************************************
*
*   Synopsis:
*	static int	makefits( char_out, nx, ny, bitpix, datatype )
*
*   Purpose:
*	Writes a simple fits header for a 2-d image.
*	example of header:
*	lines must be exactly 80 characters, with no newline at the end
*
0123456789 123456789 123456789 123456789
SIMPLE  =                    T /Standard FITS format
BITPIX  =                   16 /
NAXIS   =                    2 /Number of axes
NAXIS1  =                  256 /
NAXIS2  =                  256 /
DATATYPE= 'INTEGER*2'          /
END
*
*   Parameters:
*	type	arg1		: (in)  What it is
*	type	arg2		: (mod) What it is
*	type	argn		: (out) What it is
*
*   Values Returned:
*	int	PR_SUCCESS	: Normal completion.
*	int	PR_E_IO		: IO error.
*
*   References:
*	Copied from function makefits from the hcompress program.
*	Programmer: R. White		Date: 24 April 1992
*
************************************************************************
-*/

static int	makefits
(
    pfi		char_out,
    int		nx,
    int		ny,
    int		bitpix,
    char	*datatype
)
{
    char line[81];
    int i;

    (void) sprintf( line, "%-80.80s",
	    "SIMPLE  =                    T /Standard FITS format" );
    PR_CHECK( char_out( line, 80 ) );

    (void) sprintf( line, "%10.10s%10d%-60.60s", "BITPIX  =", 
	    bitpix, "            /Bits per pixel." );
    PR_CHECK( char_out( line, 80 ) );

    (void) sprintf( line, "%-80.80s",
	    "NAXIS   =                    2 /Number of axes");
    PR_CHECK( char_out( line, 80 ) );

    (void) sprintf( line, "%10.10s%10d%-60.60s", "NAXIS1  =", 
	    ny, "            /" );
    PR_CHECK( char_out( line, 80 ) );

    (void) sprintf( line, "%10.10s%10d%-60.60s", "NAXIS2  =", 
	    nx, "            /" );
    PR_CHECK( char_out( line, 80 ) );

    (void) sprintf( line, "%10.10s'%*.*s'%*s%48s", "DATATYPE=", 
	    strlen( datatype), strlen( datatype ), datatype, 
	    22-strlen( datatype), "/", "" );
    PR_CHECK( char_out( line, 80 ) );

    (void) sprintf( line, "%-80.80s", "END" );
    PR_CHECK( char_out( line, 80 ) );


    /*
     * pad with blank lines to get multiple of 36 (2880 bytes)
     */
    for ( i = 0; i < 80; i++ ) 
    {
	line[i] = ' ';
    }
    line[80] = '\0';

    for ( i = 7; i < 36; i++) 
    {
	PR_CHECK( char_out( line, 80 ) );
    }

    return( PR_SUCCESS );
}

/*+
************************************************************************
*
*   Synopsis:
*	static void	qtree_bitins( a, nx, ny, b, n, bit )
*
*   Purpose:
*	Copy 4-bit values from a[( nx+1 )/2,( ny+1 )/2] to b[nx,ny], expanding
*	each value to 2x2 pixels and inserting into bitplane BIT of B.
*	A,B may NOT be same array ( it wouldn't make sense to be inserting
*	bits into the same array anyway. )
*
*   Parameters:
*	byte	a[]		: (in)	Input array.
*	int	nx		: (in)	X dimension.
*	int	ny		: (in)	Y dimension.
*	int	b[]		: (out)	Output array.
*	int	n		: (in)	Declare y dimension of b.
*	int	bit		: (in)
*
*   Values Returned:
*	void
*
*   References:
*	Copied from the hcompress function qtree_bitins.
*	Programmer: R. White		Date: 7 May 1991
*
************************************************************************
-*/

static void	qtree_bitins
(
    byte	a[],
    int		nx,
    int		ny,
    int		b[],
    int		n,
    int		bit
)
{
    int		i;
    int		j;
    int		*ptr_b00;
    int		*ptr_b10;
    byte	*ptr_k;
    int		tmp;
    int		mask;


    mask = 1 << bit;

    /*
     * expand each 2x2 block
     */

    ptr_k = a; 			/* ptr_k   is index of a[i/2,j/2]	 */
    for ( i = 0; i < nx - 1; i += 2 )
    {
	for ( ptr_b00 = b + n * i, ptr_b10 = ptr_b00 + n;
		ptr_b00 < b + n * i + ny - 1  ; ptr_b00 += 2, ptr_b10 += 2 )
	{
	    tmp = *ptr_k << bit;
	    *( ptr_b10 + 1 ) |= tmp & mask;
	    *ptr_b10 |= ( tmp >> 1 ) & mask;
	    *( ptr_b00 + 1 ) |= ( tmp >> 2 ) & mask;
	    *ptr_b00 |= ( tmp >> 3 ) & mask;
	    ptr_k ++;
	}
	if ( ptr_b00 < b + n * i + ny )
	/* if ( j < ny )*/
	{
	    /*
	     * row size is odd, do last element in row s00+1, s10+1 are
	     * off edge
	     */

	    *ptr_b10 |= ( ( *ptr_k >> 1 ) & 1 ) << bit;
	    *ptr_b00 |= ( ( *ptr_k >> 3 ) & 1 ) << bit;
	    ptr_k ++;
	}
    }
    if ( i < nx )
    {
	/*
	 * column size is odd, do last row s10, s10+1 are off edge
	 */

	ptr_b00 = b + n * i;
	for ( j = 0; j < ny - 1; j += 2 )
	{
	    *( ptr_b00 + 1 ) |= ( ( *ptr_k >> 2 ) & 1 ) << bit;
	    * ptr_b00 |= ( ( *ptr_k >> 3 ) & 1 ) << bit;
	    ptr_b00 += 2;
	    ptr_k ++;
	}
	if ( j < ny )
	{
	    /*
	     * both row and column size are odd, do corner element
	     * s00+1, s10, s10+1 are off edge
	     */

	    *ptr_b00 |= ( ( *ptr_k >> 3 ) & 1 ) << bit;
	    *ptr_k ++;
	}
    }
}

/*+
************************************************************************
*
*   Synopsis:
*	static void	qtree_copy( a, nx, ny, b, n )
*
*   Purpose:
*	copy 4-bit values from a[( nx+1 )/2,( ny+1 )/2] to b[nx,ny], expanding
*	each value to 2x2 pixels
*	a,b may be same array
*
*   Parameters:
*	byte	a[]		: (in)	Source array
*	int	nx		: (in)	X dimension of destination.
*	int	ny		: (in)	Y dimension of destination.
*	byte	b[]		: (out)	Destination array.
*	int	n		: (in)	Declared y dimension of b.
*
*   Values Returned:
*	void
*
*   References:
*	Copied from the hcompress function qtree_copy.
*	Programmer: R. White		Date: 7 May 1991
*
************************************************************************
-*/

static void	qtree_copy
(
    byte	a[],
    int		nx,
    int		ny,
    byte	b[],
    int		n
)
{
    int		i;
    int		j;
    int		k;
    int		nx2;
    int		ny2;
    int		s00;
    int		s10;


    /*
     * first copy 4-bit values to b start at end in case a,b are same
     * array
     */

    nx2 = ( nx + 1 ) / 2;
    ny2 = ( ny + 1 ) / 2;
    k = ny2 * ( nx2 - 1 ) + ny2 - 1;	/* k   is index of a[i,j]	*/
    for ( i = nx2 - 1; i >= 0; i-- )
    {
	s00 = 2 * ( n * i + ny2 - 1 );	/* s00 is index of b[2*i,2*j]	*/
	for ( j = ny2 - 1; j >= 0; j-- )
	{
	    b[s00] = a[k];
	    k -= 1;
	    s00 -= 2;
	}
    }


    /*
     * now expand each 2x2 block
     */

    for ( i = 0; i < nx - 1; i += 2 )
    {
	s00 = n * i;		/* s00 is index of b[i,j]	 */
	s10 = s00 + n;		/* s10 is index of b[i+1,j]	 */
	for ( j = 0; j < ny - 1; j += 2 )
	{
	    b[s10 + 1] = b[s00] & (byte) 1;
	    b[s10] = ( b[s00] >> 1 ) & 1;
	    b[s00 + 1] = ( b[s00] >> 2 ) & 1;
	    b[s00] = ( b[s00] >> 3 ) & 1;
	    s00 += 2;
	    s10 += 2;
	}
	if ( j < ny )
	{
	    /*
	     * row size is odd, do last element in row s00+1, s10+1 are
	     * off edge
	     */

	    b[s10] = ( b[s00] >> 1 ) & 1;
	    b[s00] = ( b[s00] >> 3 ) & 1;
	}
    }
    if ( i < nx )
    {
	/*
	 * column size is odd, do last row s10, s10+1 are off edge
	 */

	s00 = n * i;
	for ( j = 0; j < ny - 1; j += 2 )
	{
	    b[s00 + 1] = ( b[s00] >> 2 ) & 1;
	    b[s00] = ( b[s00] >> 3 ) & 1;
	    s00 += 2;
	}
	if ( j < ny )
	{
	    /*
	     * both row and column size are odd, do corner element
	     * s00+1, s10, s10+1 are off edge
	     */

	    b[s00] = ( b[s00] >> 3 ) & 1;
	}
    }
}

/*+
************************************************************************
*
*   Synopsis:
*	static int	qtree_decode( char_in, a, n, nqx, nqy, nbitplanes )
*
*   Purpose:
*	Read stream of codes from char_in and construct bit planes
*	in quadrant of 2-D array using binary quadtree coding
*
*   Parameters:
*	int	(*char_in)()	: (in)	Function to get the next byte.
*	int	a[]		: (out)	Array to fill.
*	int	n		: (in)  Declared y dimension of a.
*	int	nqx		: (in)	Partial length of row to decode.
*	int	nqy		: (in)	Partial length of column to decode.
*	int	nbitplanes	: (in)	Number of bitplanes to decode.
*
*   Values Returned:
*	int	PR_SUCCESS	: Normal completion.
*	int	PR_E_CODE	: Bad format code.
*	int	PR_E_IO		: Error during io.
*	int	PR_E_MEMORY	: Memory allocation failure.
*
*   References:
*	Copied from the hcompress function qtree_decode.
*	Programmer: R. White		Date: 7 May 1991
*
************************************************************************
-*/

static int	qtree_decode
(
    pfi		char_in,
    int		a[],
    int		n,
    int		nqx,
    int		nqy,
    int		nbitplanes
)
{
    /* static int  input_bit(); */
    /* static int  input_huffman(); */
    /* static int  input_nbits(); */
    /* void	pr_format_message(); */
    /* static int	qtree_expand(); */
    /* static void qtree_bitins(); */
    /* static int	read_bdirect(); */

    int		log2n;
    int		k;
    int		bit;
    int		b;
    int		nqmax;
    int		nx;
    int		ny;
    int		nfx;
    int		nfy;
    int		c;
    int		nqx2;
    int		nqy2;
    byte	*scratch;


    /*
     * log2n is log2 of max(nqx,nqy) rounded up to next power of 2
     */

    nqmax = ( nqx > nqy ) ? nqx : nqy;
    log2n = log( (float) nqmax ) / log( 2.0 ) + 0.5;
    if ( nqmax > ( 1 << log2n ) )
    {
	log2n += 1;
    }


    /*
     * allocate scratch array for working space
     */

    nqx2 = ( nqx + 1 ) / 2;
    nqy2 = ( nqy + 1 ) / 2;
    PR_CHECK_NULL( scratch = byte_alloc( nqx2 * nqy2 ) );


    /*
     * now decode each bit plane, starting at the top A is assumed to
     * be initialized to zero
     */

    for ( bit = nbitplanes - 1; bit >= 0; bit-- )
    {
	/*
	 * Was bitplane was quadtree-coded or written directly?
	 */

	b = input_nybble( char_in );
	if ( b == 0 )
	{
	    /*
	     * bit map was written directly
	     */

	    PR_CHECK( read_bdirect( char_in, a, n, nqx, nqy, scratch, bit ) );
	}
	else if ( b != 0xf )
	{
	    pr_format_message( PR_E_CODE, b );
	    return ( PR_E_CODE );
	}
	else
	{
	    /*
	     * bitmap was quadtree-coded, do log2n expansions
	     * 
	     * read first code
	     */

	    PR_CHECK( scratch[0] = input_huffman( char_in ) );


	    /*
	     * now do log2n expansions, reading codes from file as
	     * necessary
	     */

	    nx = 1;
	    ny = 1;
	    nfx = nqx;
	    nfy = nqy;
	    c = 1 << log2n;
	    for ( k = 1; k < log2n; k++ )
	    {
		/*
		 * this somewhat cryptic code generates the sequence
		 * n[k-1] = ( n[k]+1 )/2 where n[log2n]=nqx or nqy
		 */

		c = c >> 1;
		nx = nx << 1;
		ny = ny << 1;
		if ( nfx <= c )
		{
		    nx -= 1;
		}
		else
		{
		    nfx -= c;
		}
		if ( nfy <= c )
		{
		    ny -= 1;
		}
		else
		{
		    nfy -= c;
		}
		PR_CHECK( qtree_expand( char_in, scratch, nx, 
			ny, scratch ) );
	    }


	    /*
	     * now copy last set of 4-bit codes to bitplane bit of
	     * array a
	     */

	    qtree_bitins( scratch, nqx, nqy, a, n, bit );
	}
    }
    free( scratch );

    return( PR_SUCCESS );
}

/*+
************************************************************************
*
*   Synopsis:
*	static int	qtree_expand( char_in, a, nx, ny, b )
*
*   Purpose:
*	do one quadtree expansion step on array a[( nqx+1 )/2,( nqy+1 )/2]
*	results put into b[nqx,nqy] ( which may be the same as a )
*
*   Parameters:
*	int	(*char_in)()	: (in)	Function to read data from input
*	byte	a[]		: (in)	Array of data to expand.
*	int	nx		: (in)	X dimension of a.
*	int	ny		: (in)	Y dimension of a.
*	byte	b[]		: (out) Expanded data.
*
*   Values Returned:
*	int	PR_SUCCESS	: Normal completion.
*	int	PR_E_IO		: Error during io.
*
*   References:
*	Copied from the hcompress function qtree_expand.
*	Programmer: R. White		Date: 7 May 1991
*
************************************************************************
-*/

static int	qtree_expand
(
    pfi		char_in,
    byte	a[],
    int		nx,
    int		ny,
    byte	b[]
)
{
    /* static int  input_huffman(); */
    /* static void qtree_copy(); */

    int         i;


    /*
     * first copy a to b, expanding each 4-bit value
     */

    qtree_copy( a, nx, ny, b, ny );


    /*
     * now read new 4-bit values into b for each non-zero element
     */

    for ( i = nx * ny - 1; i >= 0; i-- )
    {
	if ( b[i] != 0 )
	{
	    PR_CHECK( b[i] = input_huffman( char_in ) );
	}
    }

    return( PR_SUCCESS );
}

/*+
************************************************************************
*
*   Synopsis:
*	static int	read_bdirect( char_in, a, n, nqx, nqy, scratch, bit )
*
*   Purpose:
*	Reads an image packed 4 bits per byte.
*
*   Parameters:
*	int	(*char_in)()	: (in)	Function to read data.
*	int	a[]		: (out)
*	int	n		: (in)  Declared y dimension of a.
*	int	nqx		: (in)	X dimension of a.
*	int	nqy		: (in)	Y dimension of a.
*	byte	scratch[]	: (in)	
*	int	bit		: (in)
*
*   Values Returned:
*	int	PR_SUCCESS	: Normal completion.
*	int	PR_E_IO		: Error during io.
*
*   References:
*	Copied from the hcompress function qtree_bdirect.
*	Programmer: R. White		Date: 7 May 1991
*
************************************************************************
-*/

static int	read_bdirect
(
    pfi		char_in,
    int		a[],
    int		n,
    int		nqx,
    int		nqy,
    byte	scratch[],
    int		bit
)
{
    /* static void qtree_bitins(); */

    int		i;


    /*
     * read bit image packed 4 pixels/nybble
     */

    for ( i = 0; i < ( ( nqx + 1 ) / 2 ) * ( ( nqy + 1 ) / 2 ); i++ )
    {
	PR_CHECK( scratch[i] = input_nybble( char_in ) );
    }


    /*
     * insert in bitplane BIT of image A
     */

    qtree_bitins( scratch, nqx, nqy, a, n, bit );

    return( PR_SUCCESS );
}

/*+
************************************************************************
*
*   Synopsis:
*	static void	start_inputing_bits()
*
*   Purpose:
*	Initialize bit input.
*
*   Parameters:
*	void
*
*   Values Returned:
*	void
*
************************************************************************
-*/

static void	start_inputing_bits
(
    void
)
{

    /*
     * Buffer starts out with no bits in it
     */

    bits_to_go = 0;
}
