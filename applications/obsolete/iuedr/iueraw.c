/*+
 *  Name:
 *     iueraw

 *  Purpose:
 *     Folds record length information into a raw binary file
 *     for unix IUEDR to read.

 *  Language:
 *     C

 *  Usage:

 *  Description:

 *  Authors:
 *     MJC: Martin Clayton (Starlink)
 *     {enter_new_authors_here}

 *  History:
 *     18-AUG-1994 (MJC):
 *       Original Version.
 *     01-MAY-1995 (MJC):
 *       Tidied up a little.
 *     09-JUN-1995 (MJC):
 *       Modified to support 2000 byte records in MELO files.
 *     11-MAR-1996 (MJC):
 *       Fixed problem with file size errors not being handled.
 *       Added support for NEWSIPS#1 MEHIs (361x1204 byte records).
 *     {enter_further_changes_here}

 *  Problems:
 *     At the moment the number of lines in MELO and MEHI files is based
 *     on experimentation rather than any rule.
 *     {note_further_problems_here}

 *  Bugs:
 *     {note_any_bugs_here}

 *-
 */

/*Standard Header Files:
 */
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <sys/types.h>


/*Local Macros:
 */
#define HEADER_LINE_LEN   (  72 )  /* Characters in a VICAR header line.      */
#define N_IMAGE_LINES     ( 768 )  /* Lines in an IUE image.                  */
#define IMAGE_LINE_LEN    ( 768 )  /* Samples per line in an IUE image.       */
#define N_SPEC_LINES      (   7 )  /* Lines in a MELO file.                   */
#define N_MEHISPEC_LINES  ( 421 )  /* Lines in a NEWSIPS #2 MEHI file.        */
#define N_MEHINS1_LINES   ( 361 )  /* Lines in a NEWSIPS #1 MEHI file.        */

#define UNK_IM            ( 0x00 ) /* Internal code for unknown image type.   */
#define RAW_IM            ( 0x01 ) /* Internal code for a RAW image.          */
#define PHOT_IM           ( 0x02 ) /* Internal code for a PHOT/GPHOT image.   */
#define MELO_IM           ( 0x03 ) /* Internal code for a MELO spectrum.      */
#define MEHI_IM           ( 0x04 ) /* Internal code for a MEHI spectrum.      */
#define MELO2_IM          ( 0x05 ) /* Code for a MELO spectrum, 2k record.    */
#define MEHI2_IM          ( 0x06 ) /* Code for a MEHI spectrum, 2k record.    */
#define MEHI3_IM          ( 0x07 ) /* MEHI spectrum, NEWSIPS #1.              */
#define STAT_FAIL         ( 0x08 ) /* Could not stat( ) the file.             */
#define SIZ_FAIL          ( 0x09 ) /* The file size doesn't make sense.       */

#define RAW_EXT           "raw"    /* File extension of a RAW image.          */
#define PHOT_EXT          "pi"     /* File extension of a PHOT/GPHOT image.   */
#define MELO_EXT          "melo"   /* File extension of a MELO spectrum.      */
#define MEHI_EXT          "mehi"   /* File extension of a MEHI spectrum.      */

/*Function Prototypes:
 */
int get_header_size( char *f );    /* Returns VICAR header size in lines.     */
int ident_file( char *f );         /* Identifies IUE data type.               */
void stop_convert( int s );        /* Closes files before exit.               */

/*Global Variables:
 */
int infile;                        /* Input file descriptor.                  */
int outfile;                       /* Output file descriptor.                 */
int image_lines;                   /* Number of image records.                */
int image_line_len;                /* Number of bytes per image record.       */
int n_excess_bytes;                /* Number of padding bytes per record.     */

/*.
 */
int main(

int argc,                          /* Command argument count.                 */
char *argv[]                       /* Command argument vectors.               */

)
{

/*Local Variables:
 */
char header_line[HEADER_LINE_LEN]; /* Buffer for VICAR header line.           */
char record_length_word[2];        /* Buffer for record-length word.          */
char *image_line;                  /* Pointer to image data buffer.           */
int i;                             /* Loop index.                             */
int ftype;                         /* Type of IUE data file.                  */
int header_lines;                  /* Expected header size in lines.          */
int status;                        /* File access status.                     */

    n_excess_bytes = 0;
    header_lines = 0;

/*Print limited help when program usage appears to be in error.
 */
    if ( argc != 3 && argc != 4 ) {
        printf( "! Usage: iueraw <rawfile> <outfile> |<n_excess_bytes>|\n" );
        exit( EXIT_FAILURE );

/*Don't allow the user to overwrite their source data file.
 */
    } else if ( !strcmp( argv[1], argv[2] ) ) {
        printf( "! Source and Destination file names must be different.\n" );
        exit ( EXIT_FAILURE );
    }

/*Extract the record byte padding value if one is given, otherwise assume
 *no padding.
 */
    if ( argc == 4 ) {
        if ( sscanf( argv[3], "%d", &n_excess_bytes ) != 1 ) {
            printf( "! Could not get byte count from \\%s\\.\n", argv[3] );
            exit( EXIT_FAILURE );

        } else {
#ifdef 0
            if ( n_excess_bytes > 32 ) {
                printf( "! Assumimg header of %d lines.\n",
                        n_excess_bytes );
                header_lines = n_excess_bytes;
                n_excess_bytes = 0;

            } else
#endif
            {
                printf( "! Assumimg records padded with %d bytes.\n",
                        n_excess_bytes );
            }
        }
    }

/*Identify the type of input data based on the input file name.
 */
    if ( ( ftype = ident_file( argv[1] ) ) == UNK_IM ) {
        printf( "! Could not identify input file type.\n" );
        exit ( EXIT_FAILURE );

    } else if ( ftype == PHOT_IM ) {
        printf( "! Assumed to be a PHOT or GPHOT image.\n" );
        image_line_len = 2 * IMAGE_LINE_LEN;
        image_lines = N_IMAGE_LINES;

    } else if ( ftype == RAW_IM ) {
        printf( "! Assumed to be a RAW image.\n" );
        image_line_len = IMAGE_LINE_LEN;
        image_lines = N_IMAGE_LINES;

    } else if ( ftype == MELO_IM ) {
        printf( "! Assumed to be a MELO spectrum.\n" );
        image_line_len = 2048;
        image_lines = N_SPEC_LINES;

    } else if ( ftype == MELO2_IM ) {
        printf( "! Assumed to be a MELO spectrum, 2000 byte records.\n" );
        image_line_len = 2000;
        image_lines = N_SPEC_LINES;

    } else if ( ftype == MEHI_IM ) {
        image_line_len = 2048;
        image_lines = N_MEHISPEC_LINES;
        printf( "! Assumed to be a MEHI spectrum.\n" );

    } else if ( ftype == MEHI2_IM ) {
        printf( "! Assumed to be a MEHI spectrum, 2000 byte records.\n" );
        image_line_len = 2000;
        image_lines = N_MEHISPEC_LINES;

    } else if ( ftype == MEHI3_IM ) {
        printf( "! Assumed to be a MEHI spectrum, 1204 byte records.\n" );
        image_line_len = 1204;
        image_lines = N_MEHINS1_LINES;

    } else if ( ftype == STAT_FAIL ) {
        printf( "! Could not stat file.\n" );
        exit ( EXIT_FAILURE );

    } else if ( ftype == SIZ_FAIL ) {
        printf( "! The size of the file appears to be in error.\n" );
        exit ( EXIT_FAILURE );

    } else {
        printf( "! Unknown file type.\n" );
        exit ( EXIT_FAILURE );
    }

/*Open input file for read.
 */
    infile = open( argv[1], O_RDONLY );
    if ( infile == -1 ) {
        printf( "! Could not open input file \\%s\\.\n", argv[1] );
        exit ( EXIT_FAILURE );
    }

/*Create and/or open output file.
 */
    outfile = open( argv[2], O_CREAT | O_RDWR );
    if ( outfile == -1 ) {
        printf( "! Could not open output file \\%s\\.\n", argv[2] );
        close ( infile );
        exit ( EXIT_FAILURE );
    }

/*Fix the output file permission codes.
 */
    chmod( argv[2], S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH );

/*Set the record length byte pair to carry 360 decimal, this is 5 VICAR
 *header lines.
 */
    record_length_word[0] = 0x68;
    record_length_word[1] = 0x01;

/*Make a guess at the size of the file header based on the size of the file.
 */
    if ( header_lines == 0 ) {
        header_lines = get_header_size( argv[1] );
    }
    if ( header_lines <= 0 ) {
        printf( "! File stat of \\%s\\ failed.\n", argv[1] );
        stop_convert( EXIT_FAILURE );

    } else {
        printf( "! Expecting header %d lines long.\n", header_lines );
    }

/*Read the header and add in record length words to the output.
 */
    for ( i = 0; i < header_lines; ) {

/*    Ignore any record-padding bytes.
 */
        if ( !( i % 5 ) && i ) {
            if ( n_excess_bytes ) {
                read( infile, header_line, n_excess_bytes );
            }
        }

/*    Read header line.
 */
        status = read( infile, header_line, HEADER_LINE_LEN );
        if ( status != HEADER_LINE_LEN ) {
            printf( "! Read %s header failed.\n", argv[1] );
            stop_convert( EXIT_FAILURE );
        }

/*    Fold in record length word every fifth header line.
 */
        if ( !( i % 5 ) ) {
            status = write( outfile, record_length_word, 2 );
            if ( status != 2 ) {
                printf( "! Write %s header failed.\n", argv[2] );
                stop_convert( EXIT_FAILURE );
            }
        }

/*    Write out the header line.
 */
        status = write( outfile, header_line, HEADER_LINE_LEN );
        if ( status != HEADER_LINE_LEN ) {
            printf( "! Write %s header failed.\n", argv[2] );
            stop_convert( EXIT_FAILURE );
        }
        i++;

/*    Look for "L" character in EBCDIC whihc marks the last header line.
 */
        if ( header_line[HEADER_LINE_LEN - 1] == 0x40 ) {

/*        Read the rest of the last VICAR header record.
 */
            while ( ( i % 5 ) ) {
                status = read( infile, header_line, HEADER_LINE_LEN );
                if ( status != HEADER_LINE_LEN ) {
                    printf( "! Read %s header filling failed.\n", argv[1] );
                    stop_convert( EXIT_FAILURE );
                }
                status = write( outfile, header_line, HEADER_LINE_LEN );
                if ( status != HEADER_LINE_LEN ) {
                    printf( "! Write %s header filling failed.\n", argv[2] );
                    stop_convert( EXIT_FAILURE );
                }
                i++;
            }
            break;
        }
    }

/*Check that header was of expected size.
 */
    printf( "! %i lines in VICAR header.\n", i );
    if ( i != header_lines ) {
        printf( "! %i lines error in header.\n", i - header_lines );
        stop_convert( EXIT_FAILURE );
    }

/*Ignore any record-padding bytes.
 */
    if ( n_excess_bytes ) {
        read( infile, header_line, n_excess_bytes );
    }

/*Set record length word on the basis of the type of file being processed.
 */
    if ( ftype == RAW_IM ) {
        record_length_word[0] = 0x00;
        record_length_word[1] = 0x03;

    } else if ( ftype == PHOT_IM ) {
        record_length_word[0] = 0x00;
        record_length_word[1] = 0x06;

    } else if ( ftype == MELO2_IM || ftype == MEHI2_IM ) {
        record_length_word[0] = 0xD0;
        record_length_word[1] = 0x07;

    } else if ( ftype == MEHI3_IM ) {
        record_length_word[0] = 0xB4;
        record_length_word[1] = 0x04;

    } else {
        record_length_word[0] = 0x00;
        record_length_word[1] = 0x08;
    }
    image_line = (char *)( malloc( image_line_len ) );

/*Process the rest of the image.
 */
    for ( i = 0; i < image_lines; i++ ) {
        status = read( infile, image_line, image_line_len );
        if ( n_excess_bytes ) {
            read( infile, header_line, n_excess_bytes );
        }
        if ( status != image_line_len ) {
            printf( "! Read %s failed at line %d.\n", argv[1], i );
            break;
        }
        status = write( outfile, record_length_word, 2 );
        if ( status != 2 ) {
            printf( "! Write %s failed at line %d.\n", argv[2], i );
            break;
        }
        status = write( outfile, image_line, image_line_len );
        if ( status != image_line_len ) {
            printf( "! Write %s reclen failed at line %d.\n", argv[2], i );
            break;
        }
    }

/*Complete.
 */
    (void)(free( (void *)( image_line ) ) );
    stop_convert( EXIT_SUCCESS );

return ( 0 );
}


/*.............................................................................
 */
void stop_convert(

int exit_status                    /* Status to return to caller.             */

)
{
    close( infile );
    close( outfile );
    exit( exit_status );

return;
}


/*.............................................................................
 */
int get_header_size(
/*+
 *  Name:
 *     get_header_size

 *  Purpose:
 *     Returns size of VICAR header in specified file.

 *  Language:
 *     C

 *  Description:
 *     This function uses the global variables describing the image size
 *     to work out how many bytes are present in the image.  This is subtracted
 *     from the image size as returned by a stat( ) call, (theoretically)
 *     leaving the size of the VICAR header in bytes.  This is divided by
 *     the header record size to return the number of header lines.

 *  Arguments:
 *     char *fname (Given)
 *        Pointer to name of the file to be evaluated.

 *  Return Value:
 *     int
 *        Either the number of VICAR header lines expected or -1 to indicate
 *        a failure.

 *  Authors:
 *     MJC: Martin Clayton (Starlink)
 *     {enter_new_authors_here}

 *  History:
 *     18-AUG-1994 (MJC):
 *       Original Version.
 *     {enter_further_changes_here}

 *  Problems:
 *     {note_further_problems_here}

 *  Bugs:
 *     {note_any_bugs_here}

 *-
 */

char *fname                        /* Name of the file to check.              */

)
{

/*Local Variables:
 */
struct stat fsbuf;                 /* Buffer for file information.            */

/*Check supplied arguments.
 */
    if ( ! fname || ! *fname ) {
       return ( -1 );
    }

    if ( stat( fname, &fsbuf ) == -1 ) {
       return ( -1 );

    } else {
        return ( 5 * ( (int)( fsbuf.st_size ) -
             ( image_lines * ( image_line_len + n_excess_bytes ) ) ) /
             ( 5 * HEADER_LINE_LEN + n_excess_bytes ) );
    }
}


/*.............................................................................
 */
int ident_file(
/*+
 *  Name:
 *     ident_file

 *  Purpose:
 *     Returns type identifier for file name supplied.

 *  Language:
 *     C

 *  Arguments:
 *     char *fname (Given)
 *        Pointer to name of the file to be evaluated.

 *  Return Value:
 *     int
 *        Code to indicate the type of the named file.

 *  Authors:
 *     MJC: Martin Clayton (Starlink)
 *     {enter_new_authors_here}

 *  History:
 *     18-AUG-1994 (MJC):
 *       Original Version.
 *     01-MAY-1995 (MJC):
 *       Reversed search though file name.
 *     {enter_further_changes_here}

 *  Problems:
 *     {note_further_problems_here}

 *  Bugs:
 *     {note_any_bugs_here}

 *-
 */

char *fname                        /* Name of file.                           */

)
{

/*Local Variables:
 */
struct stat fsbuf;                 /* Buffer for file information.            */
char *froot;                       /* Pointer to the start of the file name.  */
int efound = 0;                    /* Whether a matching file type is found.  */
int etype;                         /* Type code for the file name given.      */


/*Check supplied arguments.
 */
    if ( ! fname || ! *fname ) {
       return ( UNK_IM );
    }

/*Get a pointer to the file extension (remainder of name after the last ".").
 */
    froot = fname;
    while ( *fname ) {
        fname++;
    }
    while ( fname >= froot ) {
        if ( *fname == '.' ) {
            fname++;
            efound = 1;
            break;

        } else {
            fname--;
        }
    }

/*stat the file to get its size.
 */
    if ( stat( froot, &fsbuf ) == -1 ) {
       printf( "errno %d\n", errno );
       return ( STAT_FAIL );
    }

/*Try to identify the file type.
 */
    if ( ! efound ) {
        etype = UNK_IM;

    } else {
        if ( ! strncmp( fname, RAW_EXT, 2 ) ) {
           etype = RAW_IM;

        } else if ( ! strncmp( fname, PHOT_EXT, 2 ) ) {
           etype = PHOT_IM;

        } else if ( ! strncmp( fname, MELO_EXT, 3 ) ) {
           if ( ( 5 * ( (int)( fsbuf.st_size ) -
                N_SPEC_LINES * ( 2048 + n_excess_bytes ) ) ) %
                ( 5 * HEADER_LINE_LEN + n_excess_bytes ) == 0 ) {
               etype = MELO_IM;

           } else if ( ( 5 * ( (int)( fsbuf.st_size ) -
                       N_SPEC_LINES * ( 2000 + n_excess_bytes ) ) ) %
                       ( 5 * HEADER_LINE_LEN + n_excess_bytes ) == 0 ) {
               etype = MELO2_IM;

           } else {
               etype = SIZ_FAIL;
           }

        } else if ( ! strncmp( fname, MEHI_EXT, 3 ) ) {
           if ( ( 5 * ( (int)( fsbuf.st_size ) -
                N_MEHISPEC_LINES * ( 2048 + n_excess_bytes ) ) ) %
                ( 5 * HEADER_LINE_LEN + n_excess_bytes ) == 0 ) {
               etype = MEHI_IM;

           } else if ( ( 5 * ( (int)( fsbuf.st_size ) -
                       N_MEHISPEC_LINES * ( 2000 + n_excess_bytes ) ) ) %
                       ( 5 * HEADER_LINE_LEN + n_excess_bytes ) == 0 ) {
               etype = MEHI2_IM;

           } else if ( ( 5 * ( (int)( fsbuf.st_size ) -
                       N_MEHINS1_LINES * ( 1204 + n_excess_bytes ) ) ) %
                       ( 5 * HEADER_LINE_LEN + n_excess_bytes ) == 0 ) {
               etype = MEHI3_IM;

           } else {
               etype = SIZ_FAIL;
           }

        } else {
           etype = UNK_IM;
        }
    }

return ( etype );
}

/*End-of-file.
 */
