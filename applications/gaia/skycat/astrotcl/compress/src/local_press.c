/*+
************************************************************************
****  C A N A D I A N   A S T R O N O M Y   D A T A   C E N T R E  *****
*
*   Module Name:	press/src/local_press.c
*
*   Purpose:
*	Contains the functions local to the press library.
*
*   Routines:
*	int	press		: Generic compression function.
*	int	press_buffer_in	: Reads characters from a buffer.
*	int	press_buffer_out: Writes characters to a buffer.
*	int	press_file_in	: Reads characters from a file.
*	int	press_file_out	: Writes characters to a file.
*	int	unpress		: Generic uncompression function.
*
*   Date		: June 22, 1993
*
*   SCCS data		: @(#)
*	Module Name	: local_press.c
*	Version Number	: 1.4
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

#include <sys/types.h>
#include <sys/uio.h>
#include <unistd.h>
#include <memory.h>

#include "gen_types.h"
#include "gen_str.h"
#include "gen_msg.h"

#include "press.h"
#include "local_press.h"

/*+
************************************************************************
*
*   Function: 	press
*
*   Purpose:
*	Generic compression function.
*
*   Values Returned:
*	int	PR_SUCCESS	: Normal completion.
*	int	PR_E_UNSUPPORT	: Unsuported compression type.
*
************************************************************************
-*/

int	press
(
    pfi		char_in,	/* (in)  Function to get input data.	*/
    pfi		char_out,	/* (in)  Function to write output data.	*/
    char	*type 		/* (in)  Compresion type.		*/
)
{
    if ( streq( type, PR_UNIX ) )
    {
	PR_CHECK( ux_comp( char_in, char_out ) );
    }
    else if ( streq( type, PR_HCOMP ) )
    {
	PR_CHECK( h_comp( char_in, char_out ) );
    }
    else if ( streq( type, PR_GZIP ) )
    {
	PR_CHECK( gzip_comp( char_in, char_out ) );
    }
    else if ( streq( type, PR_NONE ) )
    {
	PR_CHECK( none_comp( char_in, char_out ) );
    }
    else
    {
	pr_format_message( PR_E_UNSUPPORT, type );
	return( PR_E_UNSUPPORT );
    }
    return( PR_SUCCESS );
}

/*+
************************************************************************
*
*   Function:	press_buffer_in
*
*   Purpose:
*	Copies a string of characters from in input buffer.
*
*   Values Returned:
*	int	len		: The number of bytes transfered.
*	int	PR_E_EOI	: End of file detected.
*
************************************************************************
-*/

int		press_buffer_in
(
    byte	*buffer,	/* (in)  Place to put the characters.	*/
    int		length		/* (in)  The size of buffer.		*/
)
{
    int		len;


    if ( local_press.lp_in_buf_pos >= local_press.lp_in_buf_size )
    {
	return( PR_E_EOI );
    }
    else
    {
	len = MIN( length, 
		local_press.lp_in_buf_size - local_press.lp_in_buf_pos );
	(void) memcpy( buffer, 
		local_press.lp_in_buf + local_press.lp_in_buf_pos, len );
	local_press.lp_in_buf_pos += len;
	return( len );
    }
}

/*+
************************************************************************
*
*   Function:	press_buffer_out
*
*   Purpose:
*	Copies a string of characters to the output buffer.
*
*   Values Returned:
*	int	PR_SUCCESS	: Normal completion.
*	int	PR_E_MEMORY	: Memory allocation failure.
*
************************************************************************
-*/

int		press_buffer_out
(
    byte	*buffer,	/* (in)  Buffer to write.		*/
    int		length		/* (in)  Number of characters to copy.	*/
)
{
    if ( local_press.lp_out_buf_pos + length > local_press.lp_out_buf_size )
    {
	PR_CHECK_NULL( local_press.lp_out_buf = 
		(byte *) gen_realloc( local_press.lp_out_buf, 
		MAX( local_press.lp_out_buf_size + local_press.lp_out_buf_inc,
		local_press.lp_out_buf_pos + length ) ) );
	local_press.lp_out_buf_size += local_press.lp_out_buf_inc;
    }

    (void) memcpy( local_press.lp_out_buf + local_press.lp_out_buf_pos, 
	    buffer, length );
    local_press.lp_out_buf_pos += length;

    return( PR_SUCCESS );
}

/*+
************************************************************************
*
*   Function:	press_file_in
*
*   Purpose:
*	Gets a string of characters from an input file.
*
*   Values Returned:
*	int	length		: The number of characters read.
*	int	PR_E_EOI	: End of file detected.
*
************************************************************************
-*/

int		press_file_in
(
    byte	*buffer,	/* (in)  The buffer to fill.		*/
    int		length		/* (in)  Number of characters to get.	*/
)
{
    int		len;


    len = read( local_press.lp_infile, buffer, length );

    if ( len <= 0 )
    {
	return( PR_E_EOI );
    }
    else
    {
	return( len );
    }
}

/*+
************************************************************************
*
*   Function:	press_file_out
*
*   Purpose:
*	Writes a character to the output file.
*
*   Values Returned:
*	int	PR_SUCCESS	: Normal completion.
*	int	PR_E_IO		: Io error detected.
*
************************************************************************
-*/

int		press_file_out
(
    byte	*buffer,	/* (in)  Buffer to write.		*/
    int		length		/* (in)  Number of bytes to write.	*/
)
{
    PR_CHECK_IO( write( local_press.lp_outfile, buffer, length ), 
	    "output file" );

    return( PR_SUCCESS );
}

/*+
************************************************************************
*
*   Function:	unpress
*
*   Purpose:
*	Generic decompression function.
*
*   Values Returned:
*	int	PR_SUCCESS	: Normal completion.
*	int	PR_E_UNSUPPORT	: Unsupported compression type.
*
************************************************************************
-*/

int		unpress
(
    pfi		char_in,	/* (in)  Function to read characters.	*/
    pfi		char_out,	/* (in)  Function to write characters.	*/
    char	*type		/* (in)  Compression type.		*/
)
{
    if ( streq( type, PR_UNIX ) )
    {
	PR_CHECK( ux_uncomp( char_in, char_out ) );
    }
    else if ( streq( type, PR_HCOMP ) )
    {
	PR_CHECK( h_uncomp( char_in, char_out ) )
    }
    else if ( streq( type, PR_ULDA ) )
    {
	PR_CHECK( ulda_uncomp( char_in, char_out ) )
    }
    else if ( streq( type, PR_GZIP ) )
    {
	PR_CHECK( gzip_uncomp( char_in, char_out ) )
    }
    else if ( streq( type, PR_NONE ) )
    {
	PR_CHECK( none_uncomp( char_in, char_out ) )
    }
    else
    {
	pr_format_message( PR_E_UNSUPPORT, type );
	return( PR_E_UNSUPPORT );
    }

    return( PR_SUCCESS );
}
