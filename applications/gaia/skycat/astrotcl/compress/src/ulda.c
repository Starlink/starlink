/*+
************************************************************************
****  C A N A D I A N   A S T R O N O M Y   D A T A   C E N T R E  *****
*
*   Module Name:	press/src/ulda.c
*
*   Purpose:
*	Function to uncompress ulda data into a fits file.
*
*   Routines:
*	int	ulda_uncomp	: Function to uncompress ulda data.
*
*   Date		: Apr 05, 1993
*
*   SCCS data		: @(#)
*	Module Name	: ulda.c
*	Version Number	: 1.3
*	Release Number	: 1
*	Last Updated	: 07/04/97
*
*   Programmer		: Norman Hill
*
*   Modification History:
*	97/07/02 SEC  : Mod'ed for clean compile. 
*
****  C A N A D I A N   A S T R O N O M Y   D A T A   C E N T R E  *****
************************************************************************
-*/

#include <memory.h>

#include "gen_types.h"

#include "press.h"
#include "local_press.h"

typedef struct ulda_header 
{
    int		uh_recno;
    double	uh_lamzer;	/*  Starting wavelength.		*/
    double      uh_lamstp;      /*  Wavelength step.                    */
    double      uh_bscale;      /*  Scale factor.                       */
    double      uh_bzero;       /*  Scale offset.                       */
    double      uh_exp_time;    /*  Exposure time in seconds.           */
    long        uh_ra;          /*  Right asscension.                   */
    long        uh_dec;         /*  Declination.                        */
    long        uh_numlam;      /*  Number of data points.              */
    long        uh_num_epsilon; /*  Number of encoded epsilon values.   */
    char        uh_dubious[81]; /*  Dubious comments.                   */
    char        uh_id[17];      /*  Homogeneous object id.              */
    char        uh_obs_date[10];/*  Observation date.                   */
    char        uh_imno[6];     /*  Image number.                       */
    char        uh_cam[4];      /*  Camera id.                          */
    char        uh_exp_code[4]; /*  Exposure code.                      */
    char        uh_ap;          /*  Aperture id.                        */
    char        uh_usage;       /*  Set to 'U' if the data has been used*/
    byte        uh_obj_class;   /*  Object class.                       */
} ULDA_HEADER;

typedef	char	ULDA_DATA[5];


/*+
************************************************************************
*
*   Synopsis:
*	int	ulda_uncomp( char_in, char_out )
*
*   Purpose:
*	Uncompresses a ulda spectrum into a fits file.
*
*   Parameters:
*	int	(*char_in)()	: (in)	Function to read data.
*	int	(*char_out)()	: (in)	Function to write data.
*
*   Values Returned:
*	int	PR_SUCCESS	: Normal completion.
*	int	PR_E_IO		: Error during io.
*	int	PR_E_MEMORY	: Memory allocaiton failure.
*
************************************************************************
-*/

int		ulda_uncomp
(
    pfi		char_in,
    pfi		char_out
)
{
    ULDA_HEADER	header;
    ULDA_DATA	*data;
    byte	line[81];
    int		line_count;
    static char	*fmt1 = "%-8.8s= %-30.30s/ %-38.38s";
    static char	*fmt2 = "%-8.8s= %-10d                    / %-38.38s";
    static char	*fmt3 = "%-8.8s= %-13.10E              / %-38.38s";
    static char	*fmt4 = "%-8.8s= '%-28.28s'/ %-38.38s";
    static char	*fmt5 = "%-8.8s= '%c       '                    / %-38.38s";
    int		i;
    int		j;
    byte	val;
    int		run_length;
    byte	e_value;
    int		band;
    short	tmp;


    /*
     * Read the header part of the data.
     */

    PR_CHECK( char_in( (byte *) &header, 189 ) );


    /*
     *  Write the fits header.
     */

    line_count = 0;

    (void) sprintf( (char *) line, fmt1, "SIMPLE", "T", 
	    "Standard FITS format" );
    PR_CHECK( char_out( line, 80 ) );
    line_count ++;

    (void) sprintf( (char *) line, fmt2, "BITPIX", 8, "8 bits per pixel." );
    PR_CHECK( char_out( line, 80 ) );
    line_count ++;

    (void) sprintf( (char *) line, fmt2, "NAXIS", 0, "" );
    PR_CHECK( char_out( line, 80 ) );
    line_count ++;

    (void) sprintf( (char *) line, fmt1, "EXTEND", "T", 
	    "There is a binary table extension." );
    PR_CHECK( char_out( line, 80 ) );
    line_count ++;

    (void) sprintf( (char *) line, fmt2, "RA", header.uh_ra, "" );
    PR_CHECK( char_out( line, 80 ) );
    line_count ++;

    (void) sprintf( (char *) line, fmt2, "DEC", header.uh_dec, "" );
    PR_CHECK( char_out( line, 80 ) );
    line_count ++;

    (void) sprintf( (char *) line, fmt4, "CAMERA", header.uh_cam, "Camera idenifier" );
    PR_CHECK( char_out( line, 80 ) );
    line_count ++;

    (void) sprintf( (char *) line, fmt1, "IMAGE", header.uh_imno, "Image number" );
    PR_CHECK( char_out( line, 80 ) );
    line_count ++;

    (void) sprintf( (char *) line, fmt5, "APERTURE", header.uh_ap, "Aperture id" );
    PR_CHECK( char_out( line, 80 ) );
    line_count ++;

    (void) sprintf( (char *) line, fmt4, "DISPERSN", "LOW", "Camera idenifier" );
    PR_CHECK( char_out( line, 80 ) );
    line_count ++;

    (void) sprintf( (char *) line, fmt4, "DATE", header.uh_obs_date, 
	    "Observation date" );
    PR_CHECK( char_out( line, 80 ) );
    line_count ++;

    (void) sprintf( (char *) line, fmt3, "EXPOSURE", header.uh_exp_time, 
	    "Exposure time (sec)" );
    PR_CHECK( char_out( line, 80 ) );
    line_count ++;

    (void) sprintf( (char *) line, fmt4, "EXPCODE", header.uh_exp_code, 
	    "Exposure code" );
    PR_CHECK( char_out( line, 80 ) );
    line_count ++;

    (void) sprintf( (char *) line, fmt4, "OBJECT", header.uh_id, "Object id" );
    PR_CHECK( char_out( line, 80 ) );
    line_count ++;

    (void) sprintf( (char *) line, fmt5, "CLASS", header.uh_obj_class, "Aperture id" );
    PR_CHECK( char_out( line, 80 ) );
    line_count ++;

    header.uh_dubious[72] = '\0';
    (void) sprintf( (char *) line, "COMMENT %-72.72s", header.uh_dubious );
    PR_CHECK( char_out( line, 80 ) );
    line_count ++;

    (void) sprintf( (char *) line, "%-80.80s", "END" );
    PR_CHECK( char_out( line, 80 ) );
    line_count ++;


    /*
     *  Fill in the rest of the header block with spaces.
     */

    (void) memset( line, ' ', 80 );
    for ( i = ( line_count - 1 ) % 36 + 1; i < 36; i ++ )
    {
	PR_CHECK( char_out( line, 80 ) );
    }


    /*
     *  Write the extension header.
     */

    line_count = 0;
    
    (void) sprintf( (char *) line, fmt1, "XTENSION", "'BINTABLE'", 
	    "Binary table extension" );
    PR_CHECK( char_out( line, 80 ) );
    line_count ++;

    (void) sprintf( (char *) line, fmt2, "BITPIX", 8, "8 bits per pixel" );
    PR_CHECK( char_out( line, 80 ) );
    line_count ++;

    (void) sprintf( (char *) line, fmt2, "NAXIS", 2, "" );
    PR_CHECK( char_out( line, 80 ) );
    line_count ++;

    (void) sprintf( (char *) line, fmt2, "NAXIS1", 5, "" );
    PR_CHECK( char_out( line, 80 ) );
    line_count ++;

    (void) sprintf( (char *) line, fmt2, "NAXIS2", header.uh_numlam, 
	    "Number of spectral bands" );
    PR_CHECK( char_out( line, 80 ) );
    line_count ++;

    (void) sprintf( (char *) line, fmt2, "PCOUNT", 0, "" );
    PR_CHECK( char_out( line, 80 ) );
    line_count ++;

    (void) sprintf( (char *) line, fmt2, "GCOUNT", 1, "" );
    PR_CHECK( char_out( line, 80 ) );
    line_count ++;

    (void) sprintf( (char *) line, fmt2, "TFIELDS", 3, "" );
    PR_CHECK( char_out( line, 80 ) );
    line_count ++;

    (void) sprintf( (char *) line, fmt1, "TFORM1", "'I       '", "Wavelength" );
    PR_CHECK( char_out( line, 80 ) );
    line_count ++;

    (void) sprintf( (char *) line, fmt4, "TTYPE1", "Wavelength", "Wavelength" );
    PR_CHECK( char_out( line, 80 ) );
    line_count ++;

    (void) sprintf( (char *) line, fmt4, "TUNIT1", "Angstroms", "Wavelength" );
    PR_CHECK( char_out( line, 80 ) );
    line_count ++;

    (void) sprintf( (char *) line, fmt3, "TZERO1", header.uh_lamzer, 
	    "Starting Wavelength" );
    PR_CHECK( char_out( line, 80 ) );
    line_count ++;

    (void) sprintf( (char *) line, fmt3, "TSCAL1", header.uh_lamstp, 
	    "Wavelength increment" );
    PR_CHECK( char_out( line, 80 ) );
    line_count ++;

    (void) sprintf( (char *) line, fmt1, "TFORM2", "'I       '", "Flux" );
    PR_CHECK( char_out( line, 80 ) );
    line_count ++;

    (void) sprintf( (char *) line, fmt4, "TTYPE2", "Flux", "Flux" );
    PR_CHECK( char_out( line, 80 ) );
    line_count ++;

    (void) sprintf( (char *) line, fmt3, "TZERO2", header.uh_bzero, 
	    "Flux offset" );
    PR_CHECK( char_out( line, 80 ) );
    line_count ++;

    (void) sprintf( (char *) line, fmt3, "TSCAL2", header.uh_bscale, 
	    "Flux increment" );
    PR_CHECK( char_out( line, 80 ) );
    line_count ++;

    (void) sprintf( (char *) line, fmt1, "TFORM3", "'B       '", "Epsilon" );
    PR_CHECK( char_out( line, 80 ) );
    line_count ++;

    (void) sprintf( (char *) line, fmt4, "TTYPE3", "Epsilon", "Epsilon" );
    PR_CHECK( char_out( line, 80 ) );
    line_count ++;

    (void) sprintf( (char *) line, "%-80.80s", "END" );
    PR_CHECK( char_out( line, 80 ) );
    line_count ++;


    /*
     *  Fill in the rest of the header block with spaces.
     */

    (void) memset( line, ' ', 80 );
    for ( i = ( line_count - 1 ) % 36 + 1; i < 36; i ++ )
    {
	PR_CHECK( char_out( line, 80 ) );
    }


    PR_CHECK_NULL( data = (ULDA_DATA *) gen_alloc( header.uh_numlam * 5 ) );


    /*
     *  Write the data to the output.
     */

    for ( tmp = 0; tmp < header.uh_numlam; tmp++ )
    {
	memcpy( data[tmp], &tmp, 2 );
	PR_CHECK( char_in( (byte *) data[tmp] + 2, 2 ) );
    }


    /*
     *  Decode the epsilon values.
     */

    for( band = 0, i = 0; i < header.uh_num_epsilon; i++ )
    {
	PR_CHECK( char_in( &val, 1 ) );
	run_length = val >> 3;
	e_value = val & 0x07;
	for ( j = 0; j < run_length && band < header.uh_numlam
		; j ++, band ++ )
	{
	    data[band][4] = e_value;
	}
    }

    
    /*
     *  write the data to the output
     */

    PR_CHECK( char_out( (byte *) data, header.uh_numlam * 5 ) );

    gen_free( data );


    /*
     *  Fill in the remainder of the last 2880 block with nulls.
     */

    for ( i = ( ( ( header.uh_numlam * 5 ) - 1 ) % 2880 )  + 1 ; 
	    i < 2880; i++ )
    {
	PR_CHECK( char_out( "\0", 1 ) );
    }

    return( PR_SUCCESS );
}
