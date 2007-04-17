/*+
************************************************************************
****  C A N A D I A N   A S T R O N O M Y   D A T A   C E N T R E  *****
*
*   Module Name:	press/src/hpress.c
*
*   Purpose:
*	Funciton to hcompress image data.
*
*   Routines:
*	static int	bufcopy		: Copy non-zero code from array to 
*					  buffer.
*	static int	doencode	: Encode 2-D array and write stream
*					  of characters to outfile.
*	static int	done_outputing_bits: Flush out the last bits in the
*					  bit buffer.
*	static int	encode		: Encode H-transform and write to 
*					  output.
*	static int	fitsread	: Reads fits header and optionally
*					  passes it to the output.
*	static int	get_data	: Gets data from input.
*	static int	get_fits	: Gets fits data from input.
*	static int	get_raw		: Gets raw data from input.
*	int		h_comp		: Does hcompress.
*	static int	output_nbits	: Writes a specified number of bits to
*					  the output.
*	static int	qtree_encode	: Encode the image using quadtree.
*	static void	qtree_onebit	: Do first quadtree reduction step.
*	static void	qtree_reduce	: Do one quadtree reduction step.
*	static void	start_outputing_bits: Initializes the bit output
*					  functions.
*	static int	write_bdirect	: 
*
*   Date		: Mar 01, 1993
*
*   SCCS data		: @(#)
*	Module Name	: h_press.c
*	Version Number	: 1.6
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
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <sys/types.h>
#include <netinet/in.h>

#include "gen_types.h"
#include "gen_msg.h"

#include "press.h"
#include "h_comp.h"
#include "local_press.h"

static int	bitcount;
static int	bo_buffer;		/* The bit buffer.		*/
static int	bo_bits_to_go;
static int	qt_bitbuffer;		/* Buffer for qtree_encode.	*/
static int	qt_bits_to_go;


/*
 * Huffman code values and number of bits in each code
 */

static int  code[16] =
{
    0x3e, 0x00, 0x01, 0x08, 0x02, 0x09, 0x1a, 0x1b,
    0x03, 0x1c, 0x0a, 0x1d, 0x0b, 0x1e, 0x3f, 0x0c
};
static int  ncode[16] =
{
    6, 3, 3, 4, 3, 4, 5, 5,
    3, 5, 4, 5, 4, 5, 6, 4
};

#define output_nybble(char_out,c)	output_nbits(char_out,c,4)
#define output_huffman(char_out,c)	output_nbits(char_out,code[c],ncode[c])


/*
 *  Static function prototypes.
 */

static	int	bufcopy( byte[], int, byte[], int *, int );
static	int	doencode( pfi, int[], int, int, byte[3] );
static	int	done_outputing_bits( pfi );
static	int	encode( pfi, int[], int, int, int );
static	int	fitsread( pfi, pfi, int *, int *, int, int );
static	int	get_data( pfi, pfi, int **, int *, int *, char * );
static	int	get_fits( pfi, pfi, int **, int *, int * );
static	int	get_raw( pfi, int **, int, int, int );
static	int	output_nbits( pfi, int, int );
static	int	qtree_encode( pfi, int[], int, int, int, int );
static	void	qtree_onebit( int[], int, int, int, byte[], int );
static	void	qtree_reduce( byte[], int, int, int, byte[] );
static	void	start_outputing_bits( void );
static	int	write_bdirect( pfi, int[], int, int, int, byte[], int );


/*
 * allan: 17.12.97: replaced a non-portable #define with this routine
 */
int write_int(pfi char_out, int* i)
{
    int n = htonl(*i);
    return char_out((byte *)&n, sizeof(int));
}



/*+
************************************************************************
*
*   Synopsis:
*	static int	bufcopy( a, n, buffer, b, bmax )
*
*   Purpose:
*	copy non-zero codes from array to buffer
*
*   Parameters:
*	byte	a[]		: (in)	array to copy from.
*	int	n		: (in)	size of array a.
*	byte	buffer		: (in)	Output buffer.
*	int	b		: (out)	Num. bytes in buffer.
*	int	bmax		: (in)	Size of buffer.
*
*   Values Returned:
*	int	1		: Buffer is full.
*	int	0		: Buffer is not full.
*
*   References:
*	Copied from hcompress fitsread function.
*	Programmer: R. White         Date: 15 May 1991
*
************************************************************************
-*/

static int	bufcopy
(
    byte	a[],
    int		n,
    byte	buffer[],
    int		*b,
    int		bmax
)
{
    int         i;

    for (i = 0; i < n; i++)
    {
	if (a[i] != 0)
	{

	    /*
	     * add Huffman code for a[i] to buffer
	     */
	    qt_bitbuffer |= code[a[i]] << qt_bits_to_go;
	    qt_bits_to_go += ncode[a[i]];
	    if (qt_bits_to_go >= 8)
	    {
		buffer[*b] = qt_bitbuffer & 0xFF;
		*b += 1;

		/*
		 * return warning code if we fill buffer
		 */
		if (*b >= bmax)
		    return (1);
		qt_bitbuffer >>= 8;
		qt_bits_to_go -= 8;
	    }
	}
    }
    return (0);
}

/*+
************************************************************************
*
*   Synopsis:
*	static int	doencode( char_out, a, nx, ny, nbitplanes )
*
*   Purpose:
*	Encode 2-D array and write stream of characters to outfile.
*
*   Parameters:
*	int	(*char_out)()	: (in)	Function to write data to output.
*	int	a[]		: (in)	Image to encode.
*	int	nx		: (in)	X dimension of the image.
*	int	ny		: (in)	Y dimension of the image.
*	byte	nbitplanes	: (in)	Number of bit planes in quadrants.
*
*   Values Returned:
*	int	PR_SUCCESS	: Normal completion.
*	int	PR_E_IO		: Error durring io.
*	int	PR_E_MEMORY	: Memory allocation failure.
*
*   References:
*	Copied from hcompress fitsread function.
*	Programmer: R. White         Date: 24 April 1992
*
************************************************************************
-*/

static int	doencode
(
    pfi		char_out,
    int		a[],
    int		nx,
    int		ny,
    byte	nbitplanes[3]
)
{
    /* static int	qtree_encode(); */
    /* static void	start_outputing_bits(); */
    /* static int	done_outputing_bits(); */
    /* static int	output_nbits(); */

    int		nx2;
    int		ny2;


    nx2 = (nx+1)/2;
    ny2 = (ny+1)/2;


    /*
     * Initialize bit output
     */

    start_outputing_bits();


    /*
     * write out the bit planes for each quadrant
     */

    PR_CHECK( qtree_encode( char_out, &a[0],          ny, nx2,  ny2,  
	    nbitplanes[0] ) );
    PR_CHECK( qtree_encode( char_out, &a[ny2],        ny, nx2,  ny/2, 
	    nbitplanes[1] ) );
    PR_CHECK( qtree_encode( char_out, &a[ny*nx2],     ny, nx/2, ny2,  
	    nbitplanes[1] ) );
    PR_CHECK( qtree_encode( char_out, &a[ny*nx2+ny2], ny, nx/2, ny/2, 
	    nbitplanes[2] ) );


    /*
     * Add zero as an EOF symbol
     */

    PR_CHECK( output_nybble( char_out, 0 ) );
    done_outputing_bits( char_out );

    return( PR_SUCCESS );
}

/*+
************************************************************************
*
*   Synopsis:
*	static int	done_outputing_bits( char_out )
*
*   Purpose:
*	Flush the last bits in the bit buffer.
*
*   Parameters:
*	int	(*char_out)()	: (in)	Function to write data to the output.
*
*   Values Returned:
*	int	PR_SUCCESS	: Normal completion
*	int	PR_E_IO		: Error during io.
*	int	PR_E_MEMORY	: Memory allocation failure.
*
*   References:
*	Copied from hcompress done_outputing_bits function.
*	Programmer: R. White 
*
************************************************************************
-*/

static int	done_outputing_bits
(
    pfi		char_out
)
{
    byte	b;


    if ( bo_bits_to_go < 8 )
    {
	b = bo_buffer<<bo_bits_to_go;
	PR_CHECK( char_out( &b, 1 ) );
	/* count the garbage bits too */
	bitcount += bo_bits_to_go;
    }

    return( PR_SUCCESS );
}

/*+
************************************************************************
*
*   Synopsis:
*	static int	encode( char_out, a, nx, ny, scale )
*
*   Purpose:
*	Encode H-transform and write it to the output.
*
*   Parameters:
*	int	(*char_out)()	: (in)	Function to write data to output.
*	int	a[]		: (in)	Data array to write.
*	int	nx		: (in)	X dimension of the array.
*	int	ny		: (in)	Y dimension of the array.
*	int	scale		: (in)	Scale factor for digitization.
*
*   Values Returned:
*	int	PR_SUCCESS	: Normal completion.
*	int	PR_E_IO		: Error during io.
*	int	PR_E_MEMORY	: Memory allocation failure.
*
*   References:
*	Copied from hcompress fitsread function.
*	Programmer: R. White         Date: 24 April 1992
*
************************************************************************
-*/

static int	encode
(
    pfi		char_out,
    int		a[],
    int		nx,
    int		ny,
    int		scale
)
{
    /* int		doencode(); */
    /* void	pr_format_message(); */

    int         nel;
    int		nx2;
    int		ny2;
    int		i;
    int		j;
    int		k;
    int		q;
    int		vmax[3];
    int		nsign;
    int		bits_to_go;
    byte	nbitplanes[3];
    byte	*signbits;

    nel = nx * ny;


    /*
     * write magic value
     */

    PR_CHECK( char_out( code_magic, sizeof( code_magic ) ) );


    /*
     *  Write the size of the image and scale factor.
     */

    PR_CHECK( write_int( char_out, &nx ) );
    PR_CHECK( write_int( char_out, &ny ) );
    PR_CHECK( write_int( char_out, &scale ) );


    /*
     * write first value of A (sum of all pixels -- the only value
     * which does not compress well)
     */

    PR_CHECK( write_int( char_out, &(a[0]) ) );
    a[0] = 0;


    /*
     * allocate array for sign bits and save values, 8 per byte
     */

    PR_CHECK_NULL( signbits = (unsigned char *) malloc((nel + 7) / 8) );
    nsign = 0;
    bits_to_go = 8;
    signbits[0] = 0;
    for (i = 0; i < nel; i++)
    {
	if (a[i] > 0)
	{
	    /*
	     * positive element, put zero at end of buffer
	     */

	    signbits[nsign] <<= 1;
	    bits_to_go -= 1;
	}
	else if (a[i] < 0)
	{
	    /*
	     * negative element, shift in a one
	     */

	    signbits[nsign] <<= 1;
	    signbits[nsign] |= 1;
	    bits_to_go -= 1;


	    /*
	     * replace a by absolute value
	     */

	    a[i] = -a[i];
	}
	if (bits_to_go == 0)
	{
	    /*
	     * filled up this byte, go to the next one
	     */

	    bits_to_go = 8;
	    nsign += 1;
	    signbits[nsign] = 0;
	}
    }
    if (bits_to_go != 8)
    {

	/*
	 * some bits in last element move bits in last byte to bottom
	 * and increment nsign
	 */
	signbits[nsign] <<= bits_to_go;
	nsign += 1;
    }


    /*
     * calculate number of bit planes for 3 quadrants
     * 
     * quadrant 0=bottom left, 1=bottom right or top left, 2=top right,
     */

    for (q = 0; q < 3; q++)
    {
	vmax[q] = 0;
    }


    /*
     * get maximum absolute value in each quadrant
     */

    nx2 = (nx + 1) / 2;
    ny2 = (ny + 1) / 2;
    j = 0;			/* column counter	 */
    k = 0;			/* row counter		 */
    for (i = 0; i < nel; i++)
    {
	q = (j >= ny2) + (k >= nx2);
	if (vmax[q] < a[i])
	    vmax[q] = a[i];
	if (++j >= ny)
	{
	    j = 0;
	    k += 1;
	}
    }


    /*
     * now calculate number of bits for each quadrant
     */

    for (q = 0; q < 3; q++)
    {
	nbitplanes[q] = log((float) (vmax[q] + 1)) / log(2.0) + 0.5;
	if ((vmax[q] + 1) > (1 << nbitplanes[q]))
	{
	    nbitplanes[q] += 1;
	}
    }


    /*
     * write nbitplanes
     */

    PR_CHECK( char_out( nbitplanes, sizeof( nbitplanes ) ) );


    /*
     * write coded array
     */

    PR_CHECK( doencode( char_out, a, nx, ny, nbitplanes ) );


    /*
     * write sign bits
     */

    if (nsign > 0)
    {
	PR_CHECK( char_out( signbits, nsign ) );
    }
    if ( local_press.lp_verbose )
    {
	/*
	 * total number of bits written to file
	 */

	i = bitcount +
		8 * (nsign + sizeof(code_magic) + sizeof(nbitplanes) + 
		4 * sizeof(int));
	pr_format_message( PR_HCOMP_INFO, ( (float) i) / nel,
			16.0 * nel / ( (float) i ) );
    }
    free(signbits);

    return( PR_SUCCESS );
}


/*+
************************************************************************
*
*   Synopsis:
*	static int	fitsread( char_in, char_out, nx, ny, passthru, 
*				padded )
*
*   Purpose:
*	Read FITS header from char_in and optionally pass it to char_out.
*	nx and ny are found in the header.
*
*   Parameters:
*	int	(*char_in)()	: (in)	Function to get data from input.
*	int	(*char_out)()	: (in)	Function to write data to the output.
*	int	*nx		: (out)	X dimension of the image.
*	int	*ny		: (out)	Y dimension of the image.
*	int	passthru	: (in)	Non-zero if copy header to char_out.
*	int	padded		: (in)	Non-zero if header is padded to 2880.
*
*   Values Returned:
*	int	PR_SUCCESS	: Normal completion.
*	int	PR_E_FITS	: Fits header read error.
*	int	PR_E_IO		: Error during io.
*	int	PR_E_MEMORY	: Memory allocation failure.
*
*   References:
*	Copied from hcompress fitsread function.
*	Programmer: R. White         Date: 16 April 1992
*
************************************************************************
-*/

static int	fitsread
(
    pfi		char_in,
    pfi		char_out,
    int		*nx,
    int		*ny,	
    int		passthru,
    int		padded
)
{
    /* void	pr_format_message(); */

    int         i;
    int         j;
    char        line[82];
    int         naxis1;
    int         naxis2;
    int         nline;
    int         noend;
    int		status;		/* Function return status.		*/
    int         val;


    /*
     * make sure we find NAXIS1 and NAXIS2
     */

    naxis1 = 0;
    naxis2 = 0;
    noend = 1;
    nline = 81;

    while (noend)
    {
	/*
	 * continue looping until we've read END statement or until
	 * we've read a multiple of 36 lines past END (if padded != 0)
	 */

	for (i = 0; (i < 36) && (noend || padded); i++)
	{
	    if ( ( status = char_in( line, nline-1 ) ) == PR_E_EOI )
	    {
		pr_format_message( PR_E_FITS_INC );
		return( PR_E_FITS );
	    }
	    PR_CHECK( status );
	    line[80] = '\0';
	    /* Replace line of nulls with line of blanks. */
	    if ((int)strlen(line) != 80)
	    {
		for (j = strlen(line); j < 80; j++)
		    line[j] = ' ';
	    }


	    /*
	     * copy to outfile if passthru != 0 and we have not reached
	     * END
	     */

	    if ( passthru && noend )
	    {
		PR_CHECK( char_out( line, 80 ) );
	    }

	    if (strncmp(line, "END ", 4) == 0)
	    {
		noend = 0;
	    }
	    else if (strncmp(line, "NAXIS1  =", 9) == 0)
	    {
		if (sscanf(&line[10], " %d", &naxis1) != 1)
		{
		    pr_format_message( PR_E_FITS, line );
		    return( PR_E_FITS );
		}
	    }
	    else if (strncmp(line, "NAXIS2  =", 9) == 0)
	    {
		if (sscanf(&line[10], " %d", &naxis2) != 1)
		{
		    pr_format_message( PR_E_FITS, line );
		    return( PR_E_FITS );
		}
	    }
	    else if (strncmp(line, "NAXIS   =", 9) == 0)
	    {
		if (sscanf(&line[10], " %d", &val) != 1)
		{
		    pr_format_message( PR_E_FITS, line );
		    return( PR_E_FITS );
		}
		if (val != 2)
		{
		    pr_format_message( PR_E_FITS_DIM );
		    return( PR_E_FITS );
		}
	    }
	    else if (strncmp(line, "DATATYPE=", 9) == 0)
	    {
		if (strncmp(&line[11], "INTEGER*2", 9) != 0)
		{
		    pr_format_message( PR_E_FITS_TYPE, "INTEGER*2" );
		    return( PR_E_FITS );
		}
	    }
	    else if (strncmp(line, "PSIZE   =", 9) == 0)
	    {
		/*
		 * check PSIZE == 0
		 */

		if (sscanf(&line[10], " %d", &val) != 1)
		{
		    pr_format_message( PR_E_FITS, line );
		    return( PR_E_FITS );
		}
		if (val != 0)
		{
		    pr_format_message( PR_E_FITS_GRP );
		    return( PR_E_FITS );
		}
	    }
	    else if (strncmp(line, "GCOUNT  =", 9) == 0)
	    {
		/*
		 * check for GCOUNT > 1
		 */

		if (sscanf(&line[10], " %d", &val) != 1)
		{
		    pr_format_message( PR_E_FITS, line );
		    return( PR_E_FITS );
		}
		if (val > 1)
		{
		    pr_format_message( PR_E_FITS_GRP );
		    return( PR_E_FITS );
		}
	    }
	}
    }

    if (naxis1 <= 0 || naxis2 <= 0)
    {
	pr_format_message( PR_E_NAXIS );
	return( PR_E_FITS );
    }

    *ny = naxis1;
    *nx = naxis2;

    return( PR_SUCCESS );
}


/*+
************************************************************************
*
*   Synopsis:
*	static int	get_data( char_in, char_out, a, nx, ny, format )
*
*   Purpose:
*	Reads data from the input into an array.
*
*   Parameters:
*	int	(*char_in)()	: (in)	Function to read data from input.
*	int	(*char_out)()	: (in)	Function to write data to output.
*	int	**a		: (out)	Image array.
*	int	*nx		: (out) X dimension of the image.
*	int	*ny		: (out) X dimension of the image.
*	char	*format		: (in)	Format of input file.
*
*   Values Returned:
*	int	PR_SUCCESS	: Normal completion.
*	int	PR_E_EOI	: Unexpected end of input.
*	int	PR_E_FITS	: Fits header read error.
*	int	PR_E_IO		: Error during io.
*	int	PR_E_MEMORY	: Memory allocation failure.
*
*   References:
*	Copied from hcompress get_data function.
*	Programmer: R. White         Date: 17 April 1992
*
************************************************************************
-*/

static int	get_data
(
    pfi		char_in,
    pfi		char_out,
    int		**a,
    int		*nx,
    int		*ny,
    char	*format
)
{
    /* int		get_raw(); */
    /* int		get_fits(); */
    /* void	pr_format_message(); */

    if (strcmp(format, "raw") == 0)
    {
	PR_CHECK( get_raw(char_in, a, *nx, *ny, 0) );
    }
    else if (strcmp(format, "net") == 0)
    {
	PR_CHECK( get_raw(char_in, a, *nx, *ny, 1) );
    }
    else if (strcmp(format, "fits") == 0)
    {
	PR_CHECK( get_fits(char_in, char_out, a, nx, ny) );
    }
    else
    {
	pr_format_message( PR_E_FORMAT );
	return( PR_E_FORMAT );
    }

    return( PR_SUCCESS );
}

/*+
************************************************************************
*
*   Synopsis:
*	static int	get_fits( char_in, char_out, a, nx, ny )
*
*   Purpose:
*	Reads a fits file into a buffer.
*
*   Parameters:
*	int	(*char_in)()	: (in)	Function to read data from input.
*	int	(*char_out)()	: (in)	Function to write data to output.
*	int	**a		: (out) Pointer to allocated image array.
*	int	*nx		: (out) X dimension of image.
*	int	*ny		: (out)	Y dimension of image.
*
*   Values Returned:
*	int	PR_SUCCESS	: Normal completion.
*	int	PR_E_EOI	: Unexpected end of input.
*	int	PR_E_FITS	: Fits header read error.
*	int	PR_E_IO		: Error during io.
*	int	PR_E_MEMORY	: Memory allocation failure.
*
*   References:
*	Copied from hcompress get_fits function.
*	Programmer: R. White         Date: 17 April 1992
*
************************************************************************
-*/

static int	get_fits
(
    pfi		char_in,
    pfi		char_out,
    int		**a,
    int		*nx,
    int		*ny
)
{
    /* int		fitsread(); */
    /* int		get_raw(); */


    /*
     * 1: pass lines through to outfile 1: header is multiple of
     * 2880 bytes 0: lines are not terminated by newline
     */

    PR_CHECK( fitsread( char_in, char_out, nx, ny, 1, 1 ) );


    /*
     * read raw pixel data with byte swapping
     */

    PR_CHECK( get_raw( char_in, a, *nx, *ny, 1 ) );

    return( PR_SUCCESS );
}

/*+
************************************************************************
*
*   Synopsis:
*	static int	get_raw( char_in, a, nx, ny, swap )
*
*   Purpose:
*	Reads raw data from the input.
*
*   Parameters:
*	int	(*char_in)()	: (in)	Function to read data from input.
*	int	(*char_out)()	: (in)	Function to write data to output.
*	int	**a		: (out)	Image array.
*	int	nx		: (in)	X dimension of the image.
*	int	ny		: (in)	X dimension of the image.
*	int	swap		: (in)	Should bytes be swapped?
*
*   Values Returned:
*	int	PR_SUCCESS	: Normal completion.
*	int	PR_E_EOI	: Unexpected end of input.
*	int	PR_E_IO		: Error during io.
*	int	PR_E_MEMORY	: Memory allocation failure.
*
*   References:
*	Copied from hcompress get_raw function.
*	Programmer: R. White         Date: 17 April 1992
*
************************************************************************
-*/

static int	get_raw
(
    pfi		char_in,
    int       	**a,
    int		nx,
    int		ny,
    int		swap
)
{
    boolean	test_swap();
    void	h_swap_bytes();
    /* void	pr_format_message(); */

    int         i;
    int         j;
    int         k;
    short       *sa;
    int         tswap;

    PR_CHECK_NULL( *a = (int *) malloc(nx * ny * sizeof(int)) );


    /*
     * read a row at a time to minimize page faulting problems
     */

    PR_CHECK_NULL( sa = (short *) malloc(ny * sizeof(short)) );


    /*
     * see if byte swapping will be needed
     */

    if (swap)
    {
	tswap = test_swap();
    }
    else
    {
	tswap = 0;
    }


    /*
     * read rows
     */

    for (i = 0; i < nx; i++)
    {
	if ( char_in( sa, ny * sizeof( short ) ) != ny * sizeof( short ) )
	{
	    pr_format_message( PR_E_EOI );
	    return( PR_E_EOI );
	}


	/*
	 * swap if necessary
	 */

	if (tswap)
	{
	    h_swap_bytes(sa, ny * sizeof(short));
	}


	/*
	 * copy to array A, converting to int
	 */

	k = i * ny;
	for (j = 0; j < ny; j++)
	{
	    (*a)[k++] = sa[j];
	}
    }
    free(sa);

    return( PR_SUCCESS );
}


/*+
************************************************************************
*
*   Synopsis:
*	int	h_comp( char_in, char_out )
*
*   Purpose:
*	Executes the hcomp program.
*
*   Parameters:
*	int	(*char_in)()	: (in)	Function to get the next character
*					from the input.
*	int	(*char_out)()	: (in)	Function to get the next character
*					from the output.
*
*   Values Returned:
*	int	PR_SUCCESS	: Normal completion.
*	int	PR_E_IO		: Error during io.
*
************************************************************************
-*/

int		h_comp
(
    pfi		char_in,
    pfi		char_out
)
{
    void	digitize();
    /* int		encode(); */
    /* int		get_data(); */
    void	htrans();
    /* void	pr_format_message(); */

    int		*a;			/* Image array.			*/
    int		nx;			/* X dimension of image.	*/
    int		ny;			/* Y dimension of image.	*/


    nx = local_press.lp_nx;
    ny = local_press.lp_ny;

    
    /*
     *  Read the data.
     */

    PR_CHECK( get_data( char_in, char_out, &a, &nx, &ny, 
	    local_press.lp_format ) );


    /*
     *  H-transform the data.
     */

    htrans( a, nx, ny );


    /*
     * Digitize the data.
     */

    digitize( a, nx, ny, local_press.lp_scale );


    /*
     *  Encode and write to stdout.
     */

    PR_CHECK( encode( char_out, a, nx, ny, local_press.lp_scale ) );

    free( a );

    return( PR_SUCCESS );
}

/*+
************************************************************************
*
*   Synopsis:
*	static int	output_nbites( char_out, bits, n )
*
*   Purpose:
*	Writes n bits to the output.
*
*   Parameters:
*	int	(*char_out)()	: (in)	Function to write bytes to output.
*	int	bits		: (in)	Int containing bits to write.
*	int	n		: (in)	Number of bits to write.
*
*   Values Returned:
*	int	PR_SUCCESS	: Normal completion.
*	int	PR_E_IO		: Error durring io.
*	int	PR_E_MEMORY	: Memory allocation failure.
*
*   References:
*	Copied from hcompress output_nbites function.
*	Programmer: R. White 
*
************************************************************************
-*/

static int	output_nbits
(
    pfi		char_out,
    int		bits,
    int		n
)
{
    byte	b;

    /*
     *  Insert bits at the end of the buffer.
     */

    bo_buffer <<= n;
    bo_buffer |= ( bits & ( ( 1 << n ) - 1 ) );
    bo_bits_to_go -= n;
    if ( bo_bits_to_go <= 0 )
    {
	/*
	 *  The buffer is full, write the top 8 bits.
	 */

	b = (bo_buffer >> (-bo_bits_to_go)) & 0xff;
	PR_CHECK( char_out( &b, 1 ) );
	bo_bits_to_go += 8;
    }
    bitcount += n;

    return( PR_SUCCESS );
}

/*+
************************************************************************
*
*   Synopsis:
*	static int	qtree_encode( char_out, a, n, nqx, nqy, nbitplanes )
*
*   Purpose:
*	Encode values in quadrant of 2-D array using binary quadtree coding 
*	for each bit plane.  Assumes array is positive.
*
*   Parameters:
*	int	(*char_out)()	: (in)	Function to write data to output.
*	int	a[]		: (in)	Array to output.
*	int	n		: (in)	Plysical dimension of rows in a.
*	int	nqx		: (in)	Length of row.
*	int	nqy		: (in)	Length of column.
*	int	nbitplanes	: (in)	Number of bit planes to output.
*
*   Values Returned:
*	int	PR_SUCCESS	: Normal completion.
*	int	PR_E_IO		: Error during io.
*	int	PR_E_MEMORY	: Memory allocation failure.
*
*   References:
*	Copied from hcompress get_raw function.
*	Programmer: R. White         Date: 15 May 1991
*
************************************************************************
-*/

static int	qtree_encode
(
    pfi		char_out,
    int		a[],
    int		n,
    int		nqx,
    int		nqy,
    int		nbitplanes
)
{
    /* static int	bufcopy(); */
    /* static int	output_nbits(); */
    /* void	pr_format_message(); */
    /* static void qtree_onebit(); */
    /* static void qtree_reduce(); */
    /* static int	write_bdirect(); */

    int		log2n;
    int		i;
    int		k;
    int		bit;
    int		b;
    int		bmax;
    int		nqmax;
    int		nqx2;
    int		nqy2;
    int		nx;
    int		ny;
    byte	*scratch;
    byte	*buffer;


    /*
     * log2n is log2 of max(nqx,nqy) rounded up to next power of 2
     */

    nqmax = (nqx > nqy) ? nqx : nqy;
    log2n = log((float) nqmax) / log(2.0) + 0.5;
    if (nqmax > (1 << log2n))
    {
	log2n += 1;
    }


    /*
     * initialize buffer point, max buffer size
     */

    nqx2 = (nqx + 1) / 2;
    nqy2 = (nqy + 1) / 2;
    bmax = (nqx2 * nqy2 + 1) / 2;


    /*
     * We're indexing A as a 2-D array with dimensions (nqx,nqy).
     * Scratch is 2-D with dimensions (nqx/2,nqy/2) rounded up. Buffer
     * is used to store string of codes for output.
     */

    PR_CHECK_NULL( scratch = (byte *) malloc(2 * bmax) );
    PR_CHECK_NULL( buffer = (byte *) malloc(bmax) );


    /*
     * now encode each bit plane, starting with the top
     */

    for (bit = nbitplanes - 1; bit >= 0; bit--)
    {
	/*
	 * initial bit buffer
	 */

	b = 0;
	qt_bitbuffer = 0;
	qt_bits_to_go = 0;


	/*
	 * on first pass copy A to scratch array
	 */

	qtree_onebit(a, n, nqx, nqy, scratch, bit);
	nx = (nqx + 1) >> 1;
	ny = (nqy + 1) >> 1;


	/*
	 * copy non-zero values to output buffer, which will be written
	 * in reverse order
	 */

	if (bufcopy(scratch, nx * ny, buffer, &b, bmax))
	{
	    /*
	     * quadtree is expanding data, change warning code and just
	     * fill buffer with bit-map
	     */

	    PR_CHECK( write_bdirect( char_out, a, n, nqx, nqy, scratch, bit ) );
	    goto bitplane_done;
	}


	/*
	 * do log2n reductions
	 */

	for (k = 1; k < log2n; k++)
	{
	    qtree_reduce(scratch, ny, nx, ny, scratch);
	    nx = (nx + 1) >> 1;
	    ny = (ny + 1) >> 1;
	    if (bufcopy(scratch, nx * ny, buffer, &b, bmax))
	    {
		PR_CHECK( write_bdirect( char_out, a, n, nqx, nqy, 
			scratch, bit ) );
		goto bitplane_done;
	    }
	}


	/*
	 * OK, we've got the code in buffer Write quadtree warning
	 * code, then write buffer in reverse order
	 */

	PR_CHECK( output_nybble(char_out, 0xF) );
	if (b == 0)
	{
	    if (qt_bits_to_go > 0)
	    {

		/*
		 * put out the last few bits
		 */
		PR_CHECK( output_nbits( char_out, 
			qt_bitbuffer & ((1 << qt_bits_to_go) - 1), 
			qt_bits_to_go) );
	    }
	    else
	    {

		/*
		 * have to write a zero nybble if there are no 1's in
		 * array
		 */
		PR_CHECK( output_huffman(char_out, 0) );
	    }
	}
	else
	{
	    if (qt_bits_to_go > 0)
	    {

		/*
		 * put out the last few bits
		 */
		PR_CHECK( output_nbits(char_out, 
			qt_bitbuffer & ((1 << qt_bits_to_go) - 1), 
			qt_bits_to_go ) );
	    }
	    for (i = b - 1; i >= 0; i--)
	    {
		PR_CHECK( output_nbits( char_out, buffer[i], 8 ) );
	    }
	}
bitplane_done:;
    }
    free(buffer);
    free(scratch);

    return( PR_SUCCESS );
}

/*+
************************************************************************
*
*   Synopsis:
*	static void	qtree_onebit( a, n, nx, ny, b, bit )
*
*   Purpose:
*	Do first quadtree reduction step on bit BIT of array A.
*	Results put into b.
*
*   Parameters:
*	int	a[]		:
*	int	n		:
*	int	nx		:
*	int	ny		:
*	byte	b		:
*	int	bit		:
*
*   Values Returned:
*	void
*
*   References:
*	Copied from hcompress write_bdirect function.
*	Programmer: R. White         Date: 15 May 1991
*
************************************************************************
-*/

static void	qtree_onebit
(
    int		a[],
    int		n,
    int		nx,
    int		ny,
    byte	b[],
    int		bit
)
{
    int         i;
    int		j;
    int		k;
    int         b0;
    int		b1;
    int		b2;
    int		b3;
    int         s10;
    int		s00;


    /*
     * use selected bit to get amount to shift
     */
    b0 = 1 << bit;
    b1 = b0 << 1;
    b2 = b0 << 2;
    b3 = b0 << 3;
    k = 0;			/* k is index of b[i/2,j/2]	 */
    for (i = 0; i < nx - 1; i += 2)
    {
	s00 = n * i;		/* s00 is index of a[i,j]	 */
	s10 = s00 + n;		/* s10 is index of a[i+1,j]	 */
	for (j = 0; j < ny - 1; j += 2)
	{
	    b[k] = ((a[s10 + 1] & b0)
		    | ((a[s10] << 1) & b1)
		    | ((a[s00 + 1] << 2) & b2)
		    | ((a[s00] << 3) & b3)) >> bit;
	    k += 1;
	    s00 += 2;
	    s10 += 2;
	}
	if (j < ny)
	{

	    /*
	     * row size is odd, do last element in row s00+1,s10+1 are
	     * off edge
	     */
	    b[k] = (((a[s10] << 1) & b1)
		    | ((a[s00] << 3) & b3)) >> bit;
	    k += 1;
	}
    }
    if (i < nx)
    {

	/*
	 * column size is odd, do last row s10,s10+1 are off edge
	 */
	s00 = n * i;
	for (j = 0; j < ny - 1; j += 2)
	{
	    b[k] = (((a[s00 + 1] << 2) & b2)
		    | ((a[s00] << 3) & b3)) >> bit;
	    k += 1;
	    s00 += 2;
	}
	if (j < ny)
	{

	    /*
	     * both row and column size are odd, do corner element
	     * s00+1, s10, s10+1 are off edge
	     */
	    b[k] = (((a[s00] << 3) & b3)) >> bit;
	    k += 1;
	}
    }
}

/*+
************************************************************************
*
*   Synopsis:
*	static void	qtree_reduce( a, n, nx, ny, b )
*
*   Purpose:
*	Do one quadtree reduction step on array a results put into b 
*	(which may be the same as a)
*
*   Parameters:
*	byte	a[]		:
*	int	n		:
*	int	nx		:
*	int	ny		:
*
*   Values Returned:
*	void
*
*   References:
*	Copied from hcompress write_bdirect function.
*	Programmer: R. White         Date: 15 May 1991
*
************************************************************************
-*/

static void	qtree_reduce
(
    byte	a[],
    int		n,
    int		nx,
    int		ny,
    byte	b[]
)
{
    int         i;
    int		j;
    int		k;
    int         s10;
    int		s00;

    k = 0;			/* k is index of b[i/2,j/2]	 */
    for (i = 0; i < nx - 1; i += 2)
    {
	s00 = n * i;		/* s00 is index of a[i,j]	 */
	s10 = s00 + n;		/* s10 is index of a[i+1,j]	 */
	for (j = 0; j < ny - 1; j += 2)
	{
	    b[k] = (a[s10 + 1] != 0)
		    | ((a[s10] != 0) << 1)
		    | ((a[s00 + 1] != 0) << 2)
		    | ((a[s00] != 0) << 3);
	    k += 1;
	    s00 += 2;
	    s10 += 2;
	}
	if (j < ny)
	{

	    /*
	     * row size is odd, do last element in row s00+1,s10+1 are
	     * off edge
	     */
	    b[k] = ((a[s10] != 0) << 1)
		    | ((a[s00] != 0) << 3);
	    k += 1;
	}
    }
    if (i < nx)
    {

	/*
	 * column size is odd, do last row s10,s10+1 are off edge
	 */
	s00 = n * i;
	for (j = 0; j < ny - 1; j += 2)
	{
	    b[k] = ((a[s00 + 1] != 0) << 2)
		    | ((a[s00] != 0) << 3);
	    k += 1;
	    s00 += 2;
	}
	if (j < ny)
	{

	    /*
	     * both row and column size are odd, do corner element
	     * s00+1, s10, s10+1 are off edge
	     */
	    b[k] = ((a[s00] != 0) << 3);
	    k += 1;
	}
    }
}

/*+
************************************************************************
*
*   Synopsis:
*	static void	start_outputing_bits()
*
*   Purpose:
*	Initializes the bit output functions.
*
*   Parameters:
*	void
*
*   Values Returned:
*	void
*
*   References:
*	Copied from hcompress start_outputing_bits function.
*	Programmer: R. White 
*
************************************************************************
-*/

static void	start_outputing_bits
(
    void
)
{
    bo_buffer = 0;
    bo_bits_to_go = 8;
    bitcount = 0;
}

/*+
************************************************************************
*
*   Synopsis:
*	static int	write_bdirect( char_out, a, n, nqx, nqy, scratch, bit )
*
*   Purpose:
*
*   Parameters:
*	int	(*char_out)()	: (in)	Function to output data.
*	int	a[]		: (in)
*	int	n		: (in)
*	int	nqx		: (in)
*	int	nqy		: (in)
*	byte	scratch		: (in)
*	int	bit		: (in)
*
*   Values Returned:
*	int	PR_SUCCESS	: Normal completion.
*	int	PR_E_IO		: Error during io.
*	int	PR_E_MEMORY	: Memory allocation failure.
*
*   References:
*	Copied from hcompress write_bdirect function.
*	Programmer: R. White         Date: 15 May 1991
*
************************************************************************
-*/

static int	write_bdirect
(
    pfi		char_out,
    int		a[],
    int		n,
    int		nqx,
    int		nqy,
    byte	scratch[],
    int		bit
)
{
    int         i;


    /*
     * Write the direct bitmap warning code
     */

    PR_CHECK( output_nybble( char_out, 0x0 ) );


    /*
     * Copy A to scratch array (again!), packing 4 bits/nybble
     */

    qtree_onebit(a, n, nqx, nqy, scratch, bit);


    /*
     * write to outfile
     */

    for (i = 0; i < ((nqx + 1) / 2) * ((nqy + 1) / 2); i++)
    {
	PR_CHECK( output_nybble( char_out, scratch[i] ) );
    }

    return( PR_SUCCESS );
}
