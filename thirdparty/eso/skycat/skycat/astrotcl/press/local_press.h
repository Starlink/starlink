/*+
************************************************************************
****  C A N A D I A N   A S T R O N O M Y   D A T A   C E N T R E  *****
*
*   Include File Name:	press/h/local_press.h
*
*   Purpose:
*	Contains the local symbol definitions for the press library.
*
*   Date		: Feb 24, 1993
*
*   SCCS data		: @(#)
*	Module Name	: local_press.h
*	Version Number	: 1.6
*	Release Number	: 1
*	Last Updated	: 07/04/97
*
*   Programmer		: Norman Hill
*
*   Modification History:
*	97/07/02 SEC  :	Added function prototypes. 
*
****  C A N A D I A N   A S T R O N O M Y   D A T A   C E N T R E  *****
************************************************************************
-*/

#define	PR_CHECK( s )		{ int estatus; \
				  if ( ( estatus = ( s ) ) < 0 ) { \
				    return( estatus ); } }

#define PR_CHECK_IO( s, name )	{ if ( ( s ) < 0 ) {\
				  pr_format_message( MSG_ERRNO, (name) );\
				  return( PR_E_IO ); } }

#define PR_CHECK_NULL( s )	{ if ( ( s ) == NULL ) {\
				  pr_format_message( PR_E_MEMORY );\
				  return( PR_E_MEMORY ); } }

typedef struct local_press 
{
    char	*lp_format;	/* Format of hcompress data: raw | fits */
				/* | net | hhh				*/
    int		lp_nx;		/* X dimension of the image.		*/
    int		lp_ny;		/* Y dimension of the image.		*/
    int		lp_smooth;	/* Do smoothing after hdecompress?	*/
    boolean	lp_verbose;	/* Run in verbose mode?			*/
    int		lp_scale;	/* Scale factor for hcompress and gzip.	*/
    int		lp_infile;	/* Input file descriptor.		*/
    int		lp_outfile;	/* Output file descriptor.		*/
    byte	*lp_in_buf;	/* Pointer to input buffer.		*/
    int		lp_in_buf_size;	/* Size of the input buffer.		*/
    int		lp_in_buf_pos;	/* Position in the input buffer.	*/
    byte	*lp_out_buf;	/* Pointer to output buffer.		*/
    int		lp_out_buf_size;/* Size of the output buffer.		*/
    int		lp_out_buf_pos;	/* Position in the ouput buffer.	*/
    int		lp_out_buf_inc;	/* Ouput buffer size increment.		*/
} LOCAL_PRESS;
    

/*
 * Huffman code lookup table entry--this entry is four bytes for machines
 * that have 16-bit pointers (e.g. PC's in the small or medium model).
 * Valid extra bits are 0..13.  e == 15 is EOB (end of block), e == 16
 * means that v is a literal, 16 < e < 32 means that v is a pointer to
 * the next table, which codes e - 16 bits, and lastly e == 99 indicates
 * an unused code.  If a code with e == 99 is looked up, this implies an
 * error in the data.
 */

typedef struct huft_struct
{
    byte         e;		/* number of extra bits or operation    */
    byte         b;		/* number of bits in this code or subcode */
    union
    {
	unsigned short	n;	/* literal, length base, or distance base */
	struct huft_struct *t;	/* pointer to next level of table       */
    } v;
} HUFT;


/*
 *  Globals variable declarations.
 */

extern	LOCAL_PRESS	local_press;


/*
 *  ANSI C Function Prototypes for local functions. 
 */

extern	int	get_byte( void );

#if 0 /* allan */
extern	int	huft_build( unsigned *, unsigned, unsigned, unsigned short *,  
		unsigned short *, HUFT **, int * );  
extern	int	huft_free( HUFT * );
#endif

extern	int	gzip_inflate( void ); /* allan: changed name from "inflate()" to avoid name clash with tclpro */
extern	unsigned long   
		updcrc( byte *, unsigned );

extern	int	h_comp( pfi, pfi );
extern	int	h_uncomp( pfi, pfi );

extern	int	gzip_comp( pfi, pfi );
extern	int	gzip_uncomp( pfi, pfi );

extern	int	none_comp( pfi, pfi );
extern	int	none_uncomp( pfi, pfi );

extern	void	pr_format_message( int, ... );

extern	int	press( pfi, pfi, char *);
extern	int	unpress( pfi, pfi, char * );

extern	int	press_buffer_in( byte *, int );
extern	int	press_buffer_out( byte *, int );
extern	int	press_file_in( byte *, int );
extern	int	press_file_out( byte *, int );

extern	int	ulda_uncomp( pfi, pfi );

extern	int	ux_comp( pfi, pfi );
extern	int	ux_uncomp( pfi, pfi );
