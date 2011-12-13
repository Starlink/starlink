/*
*+
*  Name:
*     psmerge

*  Purpose:
*     This program merges one or more Encapsulated Postscript (EPS) files
*     into a single postscript file that can be sent to a printer.
*     Arbritrary origin shifts scaling and rotations can be applied to each
*     EPS file independently.

*  Language:
*     Starlink ANSI C

*  Description:
*     The command parameters are a list of EPS files each optionally
*     preceeded by -s -t and -r to specify a scale factor, translation (in
*     points) and rotation (in degrees anti-clockwise). The transformations
*     are applied in the order given and are cumulative (ie. -r45 -r45 will
*     rotation the picture by 90 degrees) but the transformation is  reset
*     to the identity transformation after each EPS file.
*
*     The x and y values for -s and -t are separated by a single character
*     (eg. -s0.5x1.0) which can be any character other than a digit.
*
*     The merged file is sent to the standard output.

*  Copyright:
*     Copyright (C) 1992-1993 Science & Engineering Research Council.
*     Copyright (C) 2002 Central Laboratory of the Research Councils.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     DLT: D. L. Terrett (Starlink)
*     RTP: Roy Platon (Starlink)
*     {enter_new_authors_here}

*  History:
*     01-JUN-1992 (DLT):
*        Original version.
*     22-APR-1993 (DLT):
*        Change BoundingBox comment to integers.
*     12-FEB-2002 (RTP):
*        Changed scanf method to accept '0xn'
*     {enter_further_changes_here}

*-
 */

/*
   If it wasn't necessary to handle the %%BoundingBox and %%DocumentFont
   comments the process would be relatively simple: The output file
   needs some preamble comments, some magic to get the coordinate origin
   to the bottom left corner of the printable area of the page (unless
   we are writing an EPS file), then for each file to be merged, a
   BEGINEPSFILE command, an appropriate scale, translate or rotate
   command as each qualifier is encountered, the contents of the file
   followed by an ENDEPSFFILE command, and finally a showpage command
   and some trailing comments.

   For a file going directly to a printer the %%BoundingBox comments can
   be thrown away (since we don't know where the printable area of the
   page is positioned it is in fact impossible to write a correct
   bounding box). If we are writing an EPS file then we must read the
   bounding box for each file, apply any transformation that is
   specified on the command line and "add" the individual bounding boxes
   to construct an overall bounding box for the whole output file.
*/

/*
 *  This is a posix conforming program
 */
#   define POSIX_SOURCE 1

/*
 *  Standard include files
 */
#   include <stdlib.h>
#   include <stdio.h>
#   include <time.h>
#   include <math.h>
#   include <string.h>

#   define TRUE 1
#   define FALSE 0

/*
 *  For machines that aren't POSIX compliant
 */
#   if !(defined(EXIT_SUCCESS))
#      define EXIT_SUCCESS 0
#   endif
#   if !(defined(EXIT_FAILURE))
#      define EXIT_FAILURE 1
#   endif

/*
 *  Global variables
 */
    int epsf;

/*
 *  structure definitions
 */
    struct bb_tag
    {
        int defined;
        double llx, lly;
        double trx, try;
    };

/*
 *  Local functions
 */
    void ProcessFile( FILE *file, struct bb_tag *bb );

/*
 *  Main program
 */

int main( int argc, char *argv[] )
{
    time_t ticks;
    struct tm *timeptr;
    char *date;
    int i, nfields;
    FILE *file;
    double t[6], xscale, yscale, xtrans, ytrans, rot, a, b, c, d, tx, ty,
        rcos, rsin, x, y;
    struct bb_tag totalbb, localbb;

/*
 *  handle the case of no arguments at all.
 */
    if ( argc < 2 )
    {
        fprintf( stderr, "psmerge: Error: no arguments\n" );
        exit( EXIT_FAILURE );
    }

/*
 *  initialize bounding box structure
 */
    totalbb.defined = FALSE;

/*
 *  See if we have been asked to write an EPS file
 */
    epsf = FALSE;
    for ( i = 1; i < argc; i++ )
    {
        if ( strcmp( argv[i], "-e" ) == 0 )
        {
            epsf = TRUE;
            break;
        }
    }

/*
 *  Write the opening comments to the postscript file
 */
    printf( "%%!PS-Adobe-1.0\n" );
    printf( "%%%%Creator: psmerge V1.0\n" );
    ticks = time( 0 );
    timeptr = localtime( &ticks );
    date = asctime( timeptr );
    printf( "%%%%Creation date: %s", date );
    printf( "%%%%Pages: 1\n" );
    printf( "%%%%DocumentFonts: (atend)\n" );
    if ( epsf ) printf( "%%%%BoundingBox: (atend)\n" );
    printf( "%%%%EndComments\n" );
    if (!epsf) printf( "%%%%Page: ? 1\n" );

/*
 *  Set up the postscript transformation so that 1,1 is at the bottom
 *  left corner of the printable area of the page
 */
    if ( !epsf ) printf( "clippath pathbbox pop pop translate" );

/*
 *  Write definitions of BEGINEPSFILE and ENDEPSFILE. These are taken from
 *  the ENCAPSULATED Postscript FILES document V2.0 with the addition of
 *  the re-definition of defaultmatrix which the GKS postscript driver uses.
 *  (this may be a bug in GKS).
 */
    printf( "/BEGINEPSFILE{\n" );
    printf( "    /EPSFsave save def\n");
    printf( "    0 setgray 0 setlinecap 1 setlinewidth 0 setlinejoin\n");
    printf( "        10 setmiterlimit [] 0 setdash\n" );
    printf( "    newpath\n" );
    printf( "    /showpage {} def\n" );
    printf( "    /defaultmatrix {currentmatrix} def\n" );
    printf( "} bind def\n" );
    printf( "/ENDEPSFILE{\n" );
    printf( "    EPSFsave restore\n" );
    printf( "} bind def\n" );

/*
 *  initialise the transformation
 */
    t[0] = 1.0;
    t[1] = 0.0;
    t[2] = 0.0;
    t[3] = 1.0;
    t[4] = 0.0;
    t[5] = 0.0;

/*
 *  process all the command qualifiers
 */
    for ( i = 1; argv[i]; i++ )
    {
        int pos;
        char  *str, *xstr;

        if ( *argv[i] == '-' )		/* a qualifier */
        {
            switch( *(argv[i] + 1) )
            {

            case 's':			/* scale */
                xscale = yscale = 0.0;
                str = argv[i] + 2;
                pos = strcspn( str, "Xx" );
                xstr = malloc( strlen(str)+1 );
                xstr = strncpy( xstr, str, pos );
                nfields = sscanf( xstr, "%lf", &xscale );
                free( xstr);
                nfields += sscanf( str + pos + 1, "%lf", &yscale );
                if ( nfields != 2 ) {
                    fprintf( stderr,
                        "psmerge: Warning: %s not understood\n", argv[i] );
                    fprintf( stderr, " missing values replaced by zero\n" );
                }
                t[0] = t[0] * xscale;
                t[1] = t[1] * yscale;
                t[2] = t[2] * xscale;
                t[3] = t[3] * yscale;
                t[4] = t[4] * xscale;
                t[5] = t[5] * yscale;
                break;

            case 't':			/* translate */
                xtrans = ytrans = 0.0;
                str = argv[i] + 2;
                pos = strcspn( str, "Xx" );
                xstr = malloc( strlen(str)+1 );
                xstr = strncpy( xstr, str, pos );
                nfields = sscanf( xstr, "%lf", &xtrans );
                nfields += sscanf( str + pos + 1, "%lf", &ytrans );
                free( xstr);
                if ( nfields != 2 ) {
                    fprintf( stderr,
                        "psmerge: Warning: %s not understood\n", argv[i] );
                    fprintf( stderr, " missing values replaced by zero\n" );
                }
                t[4] = t[4] + xtrans;
                t[5] = t[5] + ytrans;
                break;

            case 'r':			/* rotate */
                rot = 0.0;
                nfields = sscanf( argv[i] + 2, "%lf", &rot );
                if ( nfields != 1 )
                {
                    fprintf( stderr,
                        "psmerge: Warning: %s not understood\n", argv[i] );
                    fprintf( stderr, " missing values replaced by zero\n" );
                }
                rcos = cos( rot * 3.14159/180.0 );
                rsin = sin( rot * 3.14159/180.0 );
                a = t[0] * rcos - t[1] * rsin;
                b = t[0] * rsin + t[1] * rcos;
                c = t[2] * rcos - t[3] * rsin;
                d = t[2] * rsin + t[3] * rcos;
                tx = t[4] * rcos - t[5] * rsin;
                ty = t[4] * rsin + t[5] * rcos;
                t[0] = a; t[1] = b; t[2] = c; t[3] = d; t[4] = tx; t[5] = ty;
                break;

            case 'e':			/* EPSF - no action required */
                break;

	    case '\0':			/* replace argument with null */
		*(argv[i]) = '\0';
		break;

            default:
                fprintf( stderr,
                    "psmerge: Warning: unrecognised qualifier %s ignored\n",
                    argv[i] );
                break;
            }
        }
        if ( *argv[i] != '-' )		/* a file */
        {

        /*
         *  Open the file
         */
	    if (*(argv[i]))
            	file = fopen( argv[i], "r" );
	    else
		file = stdin;
            if ( file == 0 )
            {
                fprintf( stderr, "psmerge: Error: Unable to open %s\n",
                     argv[i] );
            }
            else
            {

            /*
             *  Write the BEGINEPSF command and the accumulated
             *  transformation
             */
                printf( "BEGINEPSFILE\n" );
                printf( "[ %lf %lf %lf %lf %lf %lf ] concat\n", t[0], t[1],
                    t[2], t[3], t[4], t[5] );
                printf( "%%%%BeginFile: %s", argv[i] );

                ProcessFile( file, &localbb );

                printf( "%%%%EndFile\n" );
                printf( "ENDEPSFILE\n" );

            /*
             *  Transform the bounding box read from the file and update
             *  the overall bounding box if any of the resulting points
             *  lie outside it.
             */
		if (epsf)
		{
                    if ( localbb.defined )
                    {
                        x = localbb.llx * t[0] + localbb.lly * t[2] + t[4];
                        y = localbb.llx * t[1] + localbb.lly * t[3] + t[5];
                        if ( totalbb.defined )
                        {
                            if ( x < totalbb.llx ) totalbb.llx = x;
                            if ( x > totalbb.trx ) totalbb.trx = x;
                            if ( y < totalbb.lly ) totalbb.lly = y;
                            if ( y > totalbb.try ) totalbb.try = y;
                        }
                        else
                        {
                            totalbb.llx = x;
                            totalbb.trx = x;
                            totalbb.lly = y;
                            totalbb.try = y;
                            totalbb.defined = TRUE;
                        }

                        x = localbb.llx * t[0] + localbb.try * t[2] + t[4];
                        y = localbb.llx * t[1] + localbb.try * t[3] + t[5];
                        if ( x < totalbb.llx ) totalbb.llx = x;
                        if ( x > totalbb.trx ) totalbb.trx = x;
                        if ( y < totalbb.lly ) totalbb.lly = y;
                        if ( y > totalbb.try ) totalbb.try = y;

                        x = localbb.trx * t[0] + localbb.try * t[2] + t[4];
                        y = localbb.trx * t[1] + localbb.try * t[3] + t[5];
                        if ( x < totalbb.llx ) totalbb.llx = x;
                        if ( x > totalbb.trx ) totalbb.trx = x;
                        if ( y < totalbb.lly ) totalbb.lly = y;
                        if ( y > totalbb.try ) totalbb.try = y;

                        x = localbb.trx * t[0] + localbb.lly * t[2] + t[4];
                        y = localbb.trx * t[1] + localbb.lly * t[3] + t[5];
                        if ( x < totalbb.llx ) totalbb.llx = x;
                        if ( x > totalbb.trx ) totalbb.trx = x;
                        if ( y < totalbb.lly ) totalbb.lly = y;
                        if ( y > totalbb.try ) totalbb.try = y;
                    }
		    else
		    /*
		     *  warn the user that there was no bounding box
		     */
		    {
                        fprintf( stderr,
			    "psmerge: Error: %s has no bounding box\n",
                	    argv[i] );
		    }
                }
	    }

        /*
         *  Reset the transformation.
         */
            t[0] = 1.0; t[1] = 0.0; t[2] = 0.0; t[3] = 1.0; t[4] = 0.0;
            t[5] = 0.0;
        }
    }

/*
 *  Write the showpage command and trailing comments
 */
    printf( "showpage\n" );
    printf( "%%%%Trailer\n");
    if ( epsf & totalbb.defined )
        printf( "%%%%BoundingBox: %d %d %d %d\n", (int)totalbb.llx,
	    (int) totalbb.lly, (int)totalbb.trx, (int)totalbb.try );

    exit( EXIT_SUCCESS );
}

void ProcessFile( FILE *file, struct bb_tag *bb )
{
/*
 *  Copy from file to the standard output looking for "structured" comments
 *  (a line starting with %% as we go.
 */
    char comment[257];
    int ch, atend;

/*
 *  Initialize the last character read to \n as we are at the beginning
 *  of a line.
 */
    ch = '\n';

/*
 *  Initialise the bounding box structure
 */
    bb->defined = FALSE;
    atend = FALSE;

    for ( ;; )
    {
        if ( ch == '\n' )		/* we are at the beginning of a line */
        {
            ch = getc( file );
            if ( ch == '%' )		/* the next character is % */
            {
                ch = getc( file );
                if ( ch == '%' )	/* and so is the next */
                {

                /*
                 *  This is a comment so read it
                 */
                    fgets( comment, 256, file );

                /*
                 *  remove the trailing newline
                 */
                    comment[strlen( comment ) - 1] = '\0';

                    if ( strncmp( "Pages:", comment, 6 ) == 0 ||
			 strncmp( "Page:", comment, 5 ) == 0 )
                    {
                        /* Pages and Page comments are thrown away */
                    }
                    else if ( strncmp( "BoundingBox:", comment,
                        12 ) == 0 )
                    {
                        if ( epsf && ( atend || (!bb->defined) ) )
                        {
                            if ( strstr( &comment[12], "(atend)" ) != 0)
                            {
                                atend = TRUE;
                            }
                            else
                            {
                                if ( sscanf( &comment[12], "%lf %lf %lf %lf",
                                    &(bb->llx), &(bb->lly), &(bb->trx),
                                    &(bb->try) ) == 4 )
                                {
                                     bb->defined = TRUE;
                                }
                                else
                                {
                                    fprintf( stderr,
                        "psmerge: Warning: invalid bounding box comment: %s\n",
                                       comment );
                                }
                            }
			}
                    }
                    else if ( strncmp( "DocumentFonts:", comment,
                        14 ) == 0 )
                    {
                    }
                    else	/* write the comment to the output */
                    {
                        printf( "\n%%%%%s ", comment);
                    }
                /*
                 *  we are now at the start of the next line
                 */
                    ch = '\n';
                }
                else
		/*
		 *  The second character wasn't a % so this must be an
		 *  ordinary comment. Write the newline, the % to start
		 *  the comment and then copy the character we did find
		 */
                {
                    putchar( '\n' );
                    putchar( '%' );
            	    if ( ch == EOF ) break;
                    putchar( ch );
                    ch = getc( file );
                }
            }
            else
	    /*
	     *  The line didn't start with a % so write the newline and
	     *  copy the character we did find
	     */
            {
                putchar( '\n' );
            	if ( ch == EOF ) break;
                putchar( ch );
                ch = getc( file );
            }
        }
        else
	/*
	 *  Just an ordinary character so copy it to the output file
	 */
        {
            if ( ch == EOF ) break;
            putchar( ch );
            ch = getc( file );
        }
    }
}
