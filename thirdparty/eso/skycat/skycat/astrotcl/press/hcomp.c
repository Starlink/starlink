/*+
************************************************************************
****  C A N A D I A N   A S T R O N O M Y   D A T A   C E N T R E  *****
*
*   Module Name:	press/src/hcomp.c
*
*   Purpose:
*	Contains the hcompress routines shared by both hcompress and 
*	hdecompress.
*
*   Routines:
*	int		h_put_data	: Writes data to output.
*	static int	put_raw		: Writes raw data to output.
*	static int	put_fits	: Writes fits data to output.
*	void		h_swap_bytes	: Swaps bytes in an array.
*	boolean		test_swap	: Check to see if swaping is necessary.
*
*   Date		: Mar 15, 1993
*
*   SCCS data		: @(#)
*	Module Name	: hcomp.c
*	Version Number	: 1.7
*	Release Number	: 1
*	Last Updated	: 03/20/98
*
*   Programmer		: Norman Hill
*
*   Modification History:
*	97/07/02 SEC  : Bring up to ANSI standard. 
*
****  C A N A D I A N   A S T R O N O M Y   D A T A   C E N T R E  *****
************************************************************************
-*/

#include "gen_types.h"
#include "gen_str.h"

#include "press.h"
#include "local_press.h"
#include "h_comp.h"

/*
 *  Static function prototypes.
 */

static	int	put_raw( pfi, int[], int, int, boolean );
static	int	put_fits( pfi, int[], int, int );





/*+
************************************************************************
*
*   Synopsis:
*	int	h_put_data( char_out, a, nx, ny, format )
*
*   Purpose:
*	Write the data image swaping bytes if required.
*
*   Parameters:
*	int	(*char_out)()	: (in)	Function to write characters.
*	int	a[]		: (in)	Data array to write.
*	int	nx		: (in)	X dimension of the array.
*	int	ny		: (in)	Y dimension of the array.
*	char	*format		: (in)	Output format to use.
*
*   Values Returned:
*	int	PR_SUCCESS	: Normal completion.
*	int	PR_E_FORMAT	: Output format is unknown.
*	int	PR_E_IO		: Error during io.
*	int	PR_E_MEMORY	: Memory allocation failure.
*
*   References:
*	Copied from the hcompress put_data routine.
*	Programmer: R. White		Date: 17 April 1992
*
************************************************************************
-*/

int	h_put_data
(
    pfi		char_out,
    int		a[],
    int		nx,
    int		ny,
    char 	*format 
)
{
    /* void	pr_format_message(); */
    /* static int	put_raw(); */
    /* static int	put_fits(); */


    if ( streq( format, "raw" ) || streq( format, "hhh" ) ) 
    {
	PR_CHECK( put_raw( char_out, a, nx, ny, FALSE ) );
    } 
    else if ( streq( format, "net") ) 
    {
	PR_CHECK( put_raw( char_out, a, nx, ny, TRUE ) );
    } 
    else if (streq( format, "fits" ) ) 
    {
	PR_CHECK( put_fits( char_out, a, nx, ny ) );
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
*	static int	put_raw( char_out, a, nx, ny, swap )
*
*   Purpose:
*	Statement of purpose.
*
*   Parameters:
*	int	(*char_out)()	: (in)	Function to write bytes.
*	int	a[]		: (in)	Array to write.
*	int	nx		: (in)  Size of x axis.
*	int	ny		: (in)	Size of y axis.
*	boolean	swap		: (in)	If true, bytes will be swaped if
*					necesary.
*
*   Values Returned:
*	int	PR_SUCCESS	: Normal completion.
*	int	PR_E_IO		: Error during io.
*	int	PR_E_MEMORY	: Memory allocation failure.
*
*   References:
*	Copied from the hcompress put_raw routine.
*	Programmer: R. White		Date: 17 April 1992
*
************************************************************************
-*/

static int	put_raw
(
    pfi		char_out,
    int		a[],
    int		nx,
    int		ny,
    boolean	swap 
)
{
    void	h_swap_bytes();
    boolean	test_swap();

    int		i;
    int		j;
    int		k;
    short	*sa;		/* Working array for swaped data.	*/
    boolean	tswap;		/* is byte swaping required?		*/
    int		v;


    /*
     * see if byte-swapping will be needed
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
     * write a row at a time to minimize page faulting problems
     */

    PR_CHECK_NULL( sa = short_alloc( ny ) );

    for ( i = 0; i < nx ; i++ ) 
    {
	k = i * ny;

	for ( j = 0; j < ny; j++) 
	{
	    /*
	     * force value into 16-bit integer range
	     */

	    v = a[k++];
	    if (v < -32768) 
	    {
		sa[j] = -32768;
	    } 
	    else if (v > 32767) 
	    {
		sa[j] = 32767;
	    } 
	    else 
	    {
		sa[j] = v;
	    }
	}

	if ( tswap ) 
	{
	    h_swap_bytes( sa, ny * sizeof( short ) );
	}
	PR_CHECK( char_out( sa, ny * sizeof( short ) ) );
    }
    gen_free(sa);

    return( PR_SUCCESS );
}

/*+
************************************************************************
*
*   Synopsis:
*	static int	PUT_FITS( char_out, a, nx, ny )
*
*   Purpose:
*	Writes fits data to the output.
*
*   Parameters:
*	int	(*char_out)()	: (in)	Function to write data.
*	int	a[]		: (in)	Array to be written.
*	int	nx		: (in)	X dimension.
*	int	ny		: (in)	Y dimension.
*
*   Values Returned:
*	int	PR_SUCCESS	: Normal completion.
*	int	PR_E_IO		: Error during io.
*	int	PR_E_MEMORY	: Memory allocation failure.
*
*   References:
*	Copied from the hcompress put_fits routine.
*	Programmer: R. White		Date: 17 April 1992
*
************************************************************************
-*/

static int	put_fits
(
    pfi		char_out,
    int		a[],
    int		nx,
    int		ny 
)
{
    void	pr_foramt_message();

    int		n;
    short	*sa;
    int		status;
 

    /*
     * write data with swapping
     */

    PR_CHECK( put_raw( char_out, a, nx, ny, TRUE) );


    /*
     * add zeroes to get up to multiple of 2880 bytes
     * number of padding short ints to write is between 0 and 1439
     */

    n = 1439 - ( ( nx * ny - 1 ) % 1440 );
    status = PR_SUCCESS;
    if ( n > 0 )
    {
	/*
	 * allocate a block of zeros
	 */

	PR_CHECK_NULL( sa = (short *) calloc( n , sizeof( short ) ) );
	status = char_out( sa, n * sizeof( short ) );
	gen_free( sa );
    }
    return( status );
}

/*+
************************************************************************
*
*   Synopsis:
*	void	h_swap_bytes( a, n )
*
*   Purpose:
*	Swaps the bytes in a.
*
*   Parameters:
*	byte	a[]		: (in)	An array of bytes to swap.
*	int	n		: (in)	The size of the array.
*
*   Values Returned:
*	void
*
*   References:
*	Copied from the hcompress h_swap_bytes routine.
*	Programmer: R. White		Date: 17 April 1992
*
************************************************************************
-*/
void	h_swap_bytes
(
    byte	a[],
    int		n
)
{
    int		i;
    byte	tmp;

    for (i = 0; i < n - 1; i += 2) 
    {
	tmp = a[i];
	a[i] = a[i+1];
	a[i+1] = tmp;
    }
}

/*+
************************************************************************
*
*   Synopsis:
*	boolean	test_swap()
*
*   Purpose:
*	Test to see if byte swapping is necessary on this machine.
*
*   Parameters:
*	void
*
*   Values Returned:
*	boolean	TRUE		: Byte swapping is necesary.
*	boolean	FALSE		: Byte swapping is not necessary.
*
*   References:
*	Copied from the hcompress test_swap routine.
*	Programmer: R. White		Date: 17 April 1992
*
************************************************************************
-*/

boolean	test_swap
(
    void
)
{
    union 
    {
	short s;
	char c[2];
    } u;

    u.c[0] = 0;
    u.c[1] = 1;

    return (u.s != 1);
}
