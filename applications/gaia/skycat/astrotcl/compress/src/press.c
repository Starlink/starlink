/*+
************************************************************************
****  C A N A D I A N   A S T R O N O M Y   D A T A   C E N T R E  *****
*
*   Module Name:	press/src/press.c
*
*   Purpose:
*	Contains the user interface routines to the press library.
*
*   Routines:
*	int	press_f2f	: Compress file to file.
*	int	press_f2m	: Compress file to memory.
*	int	press_m2f	: Compress memory to file.
*	int	press_m2m	: Compress memory to memory.
*	int	press_setopt	: Set compression/decompression options.
*	int	unpress_f2f	: Uncompress file to file.
*	int	unpress_f2m	: Uncompress file to memory.
*	int	unpress_fsize	: Returns uncompressed file size.
*	int	unpress_m2f	: Uncompress memory to file.
*	int	unpress_m2m	: Uncompress memory to memory.
*	int	unpress_msize	: Returns uncompressed memory size.
*
*   Date		: Feb 24, 1993
*
*   SCCS data		: @(#)
*	Module Name	: press.c
*	Version Number	: 1.12
*	Release Number	: 1
*	Last Updated	: 07/04/97
*
*   Programmer		: Norman Hill
*
*   Modification History:
*	97/07/02 SEC  :	Bring up to ANSI standard, added new functions
*			unpress_fsize and unpress_msize.
*	98/03/22 Allan : changed varargs.h to stdarg.h and fixed
*                        va_start call.
*
****  C A N A D I A N   A S T R O N O M Y   D A T A   C E N T R E  *****
************************************************************************
-*/

#include <memory.h>

/* allan: 22.03.98: was varargs.h */
#include <stdarg.h>

#include <sys/types.h>
#include <unistd.h>

#include "gen_types.h"
#include "gen_msg.h"
#include "gen_str.h"

#include "press.h"
#include "local_press.h"

#define SH(p) ((unsigned short)(byte)((p)[0]) | 			\
		((unsigned short)(byte)((p)[1]) << 8))
#define LG(p) ((unsigned long)(SH(p)) | ((unsigned long)(SH((p)+2)) << 16))


/* allan: changed from "FITS" to "fits" */
LOCAL_PRESS	local_press = { "fits", 512, 512, 0, FALSE, 10};


/*
 *  An array of functions to call for each compression type.
 */


/*+
************************************************************************
*
*   Function:	press_f2f
*
*   Purpose:
*	Compresses an input file opened on fd_in to an output file opened 
*	on fd_out.
*
*   Values Returned:
*	int	PR_SUCCESS	: Normal completion.
*	int	PR_E_UNSUPPORT	: Unsupported compression type.
*
************************************************************************
-*/

int		press_f2f
(
    int		fd_in,		/* (in)  Input file descriptor.		*/
    int		fd_out,		/* (in)  Output file descriptor.	*/
    char	*type 		/* (in)  Compression type of the file.	*/
)
{
    local_press.lp_infile = fd_in;
    local_press.lp_outfile = fd_out;

    PR_CHECK( press( press_file_in, press_file_out, type ) );

    return( PR_SUCCESS );
}

/*+
************************************************************************
*
*   Function:	press_f2m
*
*   Purpose:
*	Compresses an input file opened on fd_in to memory buffer
*	buffer_out.
*
*   Values Returned:
*	int	PR_SUCCESS	: Normal completion.
*	int	PR_E_MEMORY	: Memory allocation failure.
*	int	PR_E_UNSUPPORT	: Unsupported compression type.
*
************************************************************************
-*/

int		press_f2m
(
    int		fd_in,		/* (in)  Input file descriptor.		*/
    byte	**o_buf,	/* (out) Output memory buffer.		*/
    int		*o_buf_size,	/* (mod) Estimated output buf size on input, 
    					 actual buffer size on output.	*/
    char	*type		/* (in)  Compression type of the file.	*/
)
{
    local_press.lp_infile = fd_in;
    local_press.lp_out_buf_size = MAX( *o_buf_size, 1024 );
    PR_CHECK_NULL( local_press.lp_out_buf = 
	    byte_alloc( local_press.lp_out_buf_size ) );
    local_press.lp_out_buf_pos = 0;
    local_press.lp_out_buf_inc = local_press.lp_out_buf_size;

    PR_CHECK( press( press_file_in, press_buffer_out, type ) );


    /*
     *  Set the returned buffer pointer and length.
     */

    *o_buf = local_press.lp_out_buf;
    *o_buf_size = local_press.lp_out_buf_pos;

    return( PR_SUCCESS );
}
 
/*+
************************************************************************
*
*   Function:	press_m2f
*
*   Purpose:
*	Compress the data in ibuf and write the data to the file opened
*	on file descriptor fd_out.
*
*   Values Returned:
*	int	PR_SUCCESS	: Normal completion.
*	int	PR_E_UNSUPPORT	: Unsupported compression type.
*
************************************************************************
-*/

int		press_m2f
(
    byte	*i_buf,		/* (in)  Input memory buffer.		*/
    int		i_buf_size,	/* (in)  Input buffer size.		*/
    int		fd_out,		/* (in)  Output file descriptor.	*/
    char	*type 		/* (in)  Compression type of the file.	*/
)
{
    local_press.lp_in_buf = i_buf;
    local_press.lp_in_buf_size = i_buf_size;
    local_press.lp_in_buf_pos = 0;
    local_press.lp_outfile = fd_out;

    PR_CHECK( press( press_buffer_in, press_file_out, type ) );

    return( PR_SUCCESS );
}
 
/*+
************************************************************************
*
*   Function:	press_m2m
*
*   Purpose:
*	Compress the data in ibuf and return the result in a buffer
*	pointed to by obuf.
*
*   Values Returned:
*	int	PR_SUCCESS	: Normal completion.
*	int	PR_E_MEMORY	: Memory allocation failure.
*	int	PR_E_UNSUPPORT	: Unsupported compression type.
*
************************************************************************
-*/

int		press_m2m
(
    byte	*i_buf,		/* (in)  Input memory buffer.		*/
    int		i_buf_size,	/* (in)  Input buffer size.		*/
    byte	**o_buf,	/* (out) Output memory buffer.		*/
    int		*o_buf_size,	/* (mod) Estimated output buf size on input,
					 actual buffer size on output.	*/
    char	*type 		/* (in)  Compression type of the file.	*/
)
{
    local_press.lp_out_buf_size = MAX( *o_buf_size, 1024 );
    PR_CHECK_NULL( local_press.lp_out_buf = 
	    byte_alloc( local_press.lp_out_buf_size ) );
    local_press.lp_out_buf_pos = 0;
    local_press.lp_out_buf_inc = local_press.lp_out_buf_size;
    local_press.lp_in_buf = i_buf;
    local_press.lp_in_buf_size = i_buf_size;
    local_press.lp_in_buf_pos = 0;


    PR_CHECK( press( press_buffer_in, press_buffer_out, type ) );

    /*
     *  Set the returned buffer pointer and length.
     */

    *o_buf = local_press.lp_out_buf;
    *o_buf_size = local_press.lp_out_buf_pos;

    return( PR_SUCCESS );
}

/*+
************************************************************************
*
*   Function:	press_setopt
*
*   Purpose:
*	This function set the compression options.  The options depend
*	on the compression type.  
*
*
*   For unix compression: 
*	int	press_setopt( PR_UNIX, verbose )
*
*   Parameters:
*	char	*type		: (in)  The type of compression: PR_UNIX
*	boolean	verbose		: (in)	Should run in verbose mode?
*
*   For gzip:
*	int	press_setopt( PR_GZIP, verbose, scale );
*
*   Parameter:
*	char	*type		: (in)	The type of compression: PR_GZIP
*	boolean	verbose		: (in)	Should run in verboase mode?
*	int	scale		: (in)	Scale factor for scaling: ( 1 - 9 )
*
*
*   For hcompress:
*	int	press_setopt( PR_HCOMP, verbose, smoothing, scale, format, 
*				nx, ny )
*
*   Parameters:
*	char	*type		: (in)  The type of compression: PR_HCOMP
*	boolean	verbose		: (in)	Should run in verbose mode?
*	boolean	smoothing	: (in)	Should hdecompress use smoothing
*	int	scale		: (in)	Scale factor to use for hcompress.
*	char	*format		: (in)	Format to use (fits | raw | net )
*	int	nx		: (in)  X dimension of the image.
*	int	ny		: (in)	Y dimension of teh image.
*
*	Note that nx and ny are required for raw and net files, but ignored for
*	fits files.
*
*   Values Returned:
*	int	PR_SUCCESS	: Normal completion.
*	int	PR_E_UNSUPPORTED: Unsupported compression type.
*
************************************************************************
-*/

int		press_setopt
(
    char	*type,
    ...
)
{
    va_list	args;


    va_start( args, type ); /* allan: 22.3.98: added type arg */

    if ( streq( type, PR_UNIX ) )
    {
	local_press.lp_verbose = va_arg( args, boolean );
	va_end( args );
    }
    else if ( streq( type, PR_HCOMP ) )
    {
	local_press.lp_verbose = (boolean) va_arg( args, int );
	local_press.lp_smooth = va_arg( args, int );
	local_press.lp_scale = va_arg( args, int );
	local_press.lp_format = va_arg( args, char *);
	local_press.lp_nx = va_arg( args, int );
	local_press.lp_ny = va_arg( args, int );
	va_end( args );
    }
    else if ( streq( type, PR_GZIP ) )
    {
	local_press.lp_verbose = (boolean) va_arg( args, int );
	local_press.lp_scale = va_arg( args, int );
	va_end( args );
    }
    else if ( streq( type, PR_NONE ) )
    {
	local_press.lp_verbose = va_arg( args, boolean );
	va_end( args );
    }
    else
    {
	va_end( args );
	pr_format_message( PR_E_UNSUPPORT, type );
	return( PR_E_UNSUPPORT );
    }
    return( PR_SUCCESS );
}

/*+
************************************************************************
*
*   Function: 	unpress_f2f
*
*   Purpose:
*	Uncompresses an input file opened on fd_in to an output file opened 
*	on fd_out.
*
*   Values Returned:
*	int	PR_SUCCESS	: Normal completion.
*	int	PR_E_UNSUPPORT	: Unsupported compression type.
*
************************************************************************
-*/

int		unpress_f2f
(
    int		fd_in,		/* (in)  Input file descriptor.		*/
    int		fd_out,		/* (in)  Output file descriptor.	*/
    char	*type 		/* (in)  Compression type of the file.	*/
)
{
    local_press.lp_infile = fd_in;
    local_press.lp_outfile = fd_out;

    PR_CHECK( unpress( press_file_in, press_file_out, type ) );

    return( PR_SUCCESS );
}

/*+
************************************************************************
*
*   Function:	unpress_f2m
*
*   Purpose:
*	Uncompresses an input file opened on fd_in to memory buffer
*	buffer_out.
*
*   Values Returned:
*	int	PR_SUCCESS	: Normal completion.
*	int	PR_E_MEMORY	: Memory allocation failure.
*	int	PR_E_UNSUPPORT	: Unsupported compression type.
*
************************************************************************
-*/

int		unpress_f2m
(
    int		fd_in,		/* (in)  Input file descriptor.		*/
    byte	**o_buf,	/* (out) Output memory buffer.		*/
    int		*o_buf_size,	/* (mod) Estimated output buf size on input,
    					 actual buffer size on output.	*/
    char	*type 		/* (in)  Compression type of the file.	*/
)
{
    local_press.lp_infile = fd_in;
    local_press.lp_out_buf_size = MAX( *o_buf_size, 1024 );
    PR_CHECK_NULL( local_press.lp_out_buf = 
	    byte_alloc( local_press.lp_out_buf_size ) );
    local_press.lp_out_buf_pos = 0;
    local_press.lp_out_buf_inc = local_press.lp_out_buf_size;

    PR_CHECK( unpress( press_file_in, press_buffer_out, type ) );


    /*
     *  Set the returned buffer pointer and length.
     */

    *o_buf = local_press.lp_out_buf;
    *o_buf_size = local_press.lp_out_buf_pos;

    return( PR_SUCCESS );
}

/*+
************************************************************************
*
*   Function:	unpress_fsize
*
*   Purpose:
*	Get the uncompressed size of a compressed file. 
*
*   Values Returned:
*	int	PR_SUCCESS	: Normal completion.
*	int	PR_E_IO		: Error during io.
*	int	PR_E_UNSUPPORT	: Unsupported compression type.
*
************************************************************************
-*/

int		unpress_fsize
(
    int		fd_in,		/* (in)  Input file descriptor.		*/
    int		*osize,		/* (mod) The uncompressed size. 	*/
    char	*type		/* (in)  Compression type of the file.	*/
)
{
    char        buf[4];


    if ( streq( type, PR_GZIP ) )
    {
	PR_CHECK_IO( lseek( fd_in, -4, SEEK_END ), "lseek" );
	PR_CHECK_IO( read( fd_in, (char *) buf, sizeof(buf) ), "read" );
	*osize = LG( buf );
    }
    else
    {
	local_press.lp_infile = fd_in;
	local_press.lp_out_buf_size = MAX( *osize, 1024 );
	PR_CHECK_NULL( local_press.lp_out_buf =
		byte_alloc( local_press.lp_out_buf_size ) );
	local_press.lp_out_buf_pos = 0;
	local_press.lp_out_buf_inc = local_press.lp_out_buf_size;

	PR_CHECK( unpress( press_file_in, press_buffer_out, type ) );

	gen_free( local_press.lp_out_buf );
	*osize = local_press.lp_out_buf_pos;
    }

    return( PR_SUCCESS );
}
 
/*+
************************************************************************
*
*   Function:	unpress_m2f
*
*   Purpose:
*	Uncompress the data in ibuf and write the result to the file 
*	descriptor fd_out.
*
*   Values Returned:
*	int	PR_SUCCESS	: Normal completion.
*	int	PR_E_UNSUPPORT	: Unsupported compression type.
*
************************************************************************
-*/

int		unpress_m2f
(
    byte	*i_buf,		/* (in)  Input memory buffer.		*/
    int		i_buf_size,	/* (in)  Input buffer size.		*/
    int		fd_out,		/* (in)  Output file descriptor.	*/
    char	*type 		/* (in)  Compression type of the file.	*/
)
{
    local_press.lp_in_buf = i_buf;
    local_press.lp_in_buf_size = i_buf_size;
    local_press.lp_in_buf_pos = 0;
    local_press.lp_outfile = fd_out;

    PR_CHECK( unpress( press_buffer_in, press_file_out, type ) );

    return( PR_SUCCESS );
}
 
/*+
************************************************************************
*
*   Function:	unpress_m2m
*
*   Purpose:
*	Uncompress the data in ibuf and return the result in a buffer
*	pointed to by obuf.
*
*   Values Returned:
*	int	PR_SUCCESS	: Normal completion.
*	int	PR_E_MEMORY	: Memory allocation failure.
*	int	PR_E_UNSUPPORT	: Unsupported compression type.
*
************************************************************************
-*/

int		unpress_m2m
(
    byte	*i_buf,		/* (in)  Input memory buffer.		*/
    int		i_buf_size,	/* (in)  Input buffer size.		*/
    byte	**o_buf,	/* (out) Output memory buffer.		*/
    int		*o_buf_size,	/* (mod) Estimated output buf size on input,
					 actual buffer size on output.	*/
    char	*type 		/* (in)  Compression type of the file.	*/
)
{
    local_press.lp_out_buf_size = MAX( *o_buf_size, i_buf_size );
    local_press.lp_out_buf_size = MAX( local_press.lp_out_buf_size, 1024 );
    PR_CHECK_NULL( local_press.lp_out_buf = 
	    byte_alloc( local_press.lp_out_buf_size ) );
    local_press.lp_out_buf_pos = 0;
    local_press.lp_out_buf_inc = local_press.lp_out_buf_size;
    local_press.lp_in_buf = i_buf;
    local_press.lp_in_buf_size = i_buf_size;
    local_press.lp_in_buf_pos = 0;

    PR_CHECK( unpress( press_buffer_in, press_buffer_out, type ) );


    /*
     *  Set the returned buffer pointer and length.
     */

    *o_buf = local_press.lp_out_buf;
    *o_buf_size = local_press.lp_out_buf_pos;

    return( PR_SUCCESS );
}

/*+
************************************************************************
*
*   Function:	unpress_msize
*
*   Purpose:
*	Get the uncompressed size of a compressed file in a memory buffer. 
*
*   Values Returned:
*	int	PR_SUCCESS	: Normal completion.
*	int	PR_E_IO		: Error during io.
*	int	PR_E_UNSUPPORT	: Unsupported compression type.
*
************************************************************************
-*/

int		unpress_msize
(
    byte        *i_buf,		/* (in)  Input memory buffer.		*/
    int         i_buf_size,	/* (in)  The buffer length.		*/
    int         *osize,         /* (mod) The uncompressed size.		*/
    char        *type           /* (in)  Compression type of the file.  */
)
{
    byte	*ptr;


    if ( streq( type, PR_GZIP ) )
    {
	ptr = &( i_buf[i_buf_size-4] );
        *osize = LG( ptr );
    }
    else
    {
	local_press.lp_out_buf_size = MAX( *osize, i_buf_size );
	local_press.lp_out_buf_size = MAX( local_press.lp_out_buf_size, 1024 );
	PR_CHECK_NULL( local_press.lp_out_buf =
		byte_alloc( local_press.lp_out_buf_size ) );
	local_press.lp_out_buf_pos = 0;
	local_press.lp_out_buf_inc = local_press.lp_out_buf_size;
	local_press.lp_in_buf = i_buf;
	local_press.lp_in_buf_size = i_buf_size;
	local_press.lp_in_buf_pos = 0;
 
        PR_CHECK( unpress( press_buffer_in, press_buffer_out, type ) );

	gen_free( local_press.lp_out_buf );
	*osize = local_press.lp_out_buf_pos;
    }

    return( PR_SUCCESS );
}
