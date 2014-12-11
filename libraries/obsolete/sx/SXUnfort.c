/*
 *+
 *  Name:
 *    SXUnfort
 *
 *  Purpose:
 *    Remove record control bytes from a Fortran unfortmatted file
 *    created on an OSF system. Output goes to standard output or a file
 *
 *  Invocation:
 *    SXUnfort <input file>  [<output file>]

 *  Licence:
 *     This program is free software; you can redistribute it and/or
 *     modify it under the terms of the GNU General Public License as
 *     published by the Free Software Foundation; either version 2 of
 *     the License, or (at your option) any later version.
 *
 *     This program is distributed in the hope that it will be
 *     useful,but WITHOUT ANY WARRANTY; without even the implied
 *     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
 *     PURPOSE. See the GNU General Public License for more details.
 *
 *     You should have received a copy of the GNU General Public License
 *     along with this program; if not, write to the Free Software
 *     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
 *     02110-1301, USA

*  Copyright:
*     Copyright (C) 1995 Central Laboratory of the Research Councils.
*     All Rights Reserved.

 *  Authors:
 *    DSB: David Berry

 *  History:
 *    13-OCT-1995 (DSB):
 *       Original version.

 *-
 */

#include <stdlib.h>
#include <stdio.h>
#include <math.h>

FILE *fd1;
FILE *fd2;

int main( int argc, char *argv[] ){
      int n, nb, i, j;
      char c;

/*  Check that some parameters were given on the command line. */

      if( argc < 2 ){
         printf( "SXUnfort: Usage is 'SXUnfort <input file>  [<output file>]'\n");
         printf("\n");
         exit(0);
      }


/*  Open the input and output binary files. */

      if( ( fd1 = fopen( argv[1], "r" ) ) == 0 ){
         perror("SXUnfort");
         printf("SXUnfort: Unable to open input file '%s'.\n", argv[1] );
         printf("\n");
         exit(0);
      }


      if( argc > 2 ) {
         if( ( fd2 = fopen( argv[2], "w" ) ) == 0 ){
            perror("SXUnfort");
            printf("SXUnfort: Unable to open output file '%s'.\n", argv[2] );
            printf("\n");
            exit(0);
         }

      } else {
         fd2 = stdout;

      }


/*  Read through the input file copying the data bytes to the output file.
 *  Each record starts and ends with a longword containing the number of
 *  bytes in the record. */

      while( fread( &nb, 4, 1, fd1 ) == 1 ){
         for( i=0; i<nb; i++ ){
            fread( &c, 1, 1, fd1 );
            fwrite( &c, 1, 1, fd2 );
         }

         fread( &n, 4, 1, fd1 );
         if( n != nb ){
            printf("ERROR: Inconsistent record lengths: nb=%d n=%d\n",n);
            exit(0);
         }

      }


/*  Close the files. */

      fclose( fd1 );
      fclose( fd2 );

      return EXIT_SUCCESS;
}
