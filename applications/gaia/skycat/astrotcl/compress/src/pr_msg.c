/*+
************************************************************************
****  C A N A D I A N   A S T R O N O M Y   D A T A   C E N T R E  *****
*
*   Module Name:	press/src/pr_msg.c
*
*   Purpose:
*	Formats messge for the press library.
*
*   Routines:
*	void	pr_format_message :  Formats a message.
*
*   Date		: Feb 23, 1993
*
*   SCCS data		: @(#)
*	Module Name	: pr_msg.c
*	Version Number	: 1.8
*	Release Number	: 1
*	Last Updated	: 07/04/97
*
*   Programmer		: Norman Hill
*
*   Modification History:
*	97/07/02 SEC  : Bring up to ANSI standard.
*
****  C A N A D I A N   A S T R O N O M Y   D A T A   C E N T R E  *****
************************************************************************
-*/

#include <stdarg.h>

#include "gen_types.h"
#include "gen_str.h"
#include "gen_msg.h"
#include "press.h"

#define	PR_PREFIX	"pr"

char	pr_msg[MSG_MAX_LEN];

MSG	pr_msgs[] = {
    { PR_HCOMP_INFO,
	"%6.4f bits/pixel, compression factor %5.1f." },
    { PR_HDECOMP_INFO,
	" Smoothing: %d, image size: %dx%d,  scale: %d, output format: %s." },
    { PR_E_BITPLANE,
	" bit plane in hdecompress." },
    { PR_E_BITS,
	"Only codes up to '%d' bits are supported." },
    { PR_E_BLOCK,
	" block type '%d'." },
    { PR_E_CODE,
	" code %d in hdecompress qtree decode." },
    { PR_E_CRC,
	"The CRC stored in the file does not match the data." },
    { PR_E_DATA,
	"Data could not be decompressed." },
    { PR_E_EOI,
	"Unexpected end of input detected." },
    { PR_E_FITS,
	"Error reading fits header line:\n%s" },
    { PR_E_FITS_DIM,
	"The image is not 2 dimensional." },
    { PR_E_FITS_GRP,
	"The fits file contains groups, and cannot be compressed." },
    { PR_E_FITS_INC,
	"The fits header is incomplete." },
    { PR_E_FITS_TYPE,
	"The type of the image data is not '%s'." },
    { PR_E_FORMAT,
	"Hcompress output format is not supported." },
    { PR_E_INC_LIT,
	"Incomplete literal tree." },
    { PR_E_MAGIC,
	"The magic number of the compressed data is wrong." },
    { PR_E_MEMORY,
	"Memory allocation failure." },
    { PR_E_METHOD,
	"Compression method '%s' is not supported." },
    { PR_E_NAXIS,
	"hcompress only supports 2 axis fits files." },
    { PR_E_SIZE,
	"The size of the data is not the expected size." },
    { PR_E_UNSUPPORT,
	"Compression type '%s' is not supported." },
};

int	pr_num_msgs = sizeof( pr_msgs ) / sizeof( MSG );


/*+
************************************************************************
*
*   Synopsis:
*	void	pr_format_message( va_alist )
*
*   Purpose:
*	Formats a message from the contents of the va_alist.
*
*   Parameters:
*	va_list			: (in)  Variable length arguments.
*
*   Values Returned:
*	None.
*
************************************************************************
-*/

void	pr_format_message
( 
    int		status,
    ...
)
{
    va_list	args;

    va_start( args, status );
    msg_format( pr_msg, PR_PREFIX, pr_num_msgs, pr_msgs, status, args );
    va_end( args );
}
