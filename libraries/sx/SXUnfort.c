/*
 *  Name:
 *    SXUnfort
 *
 *  Purpose:
 *    Remove record control bytes from a Fortran unfortmatted file
 *    created on an OSF system. Output goes to standard output or a file
 *
 *  Invocation:
 *    SXUnfort <input file>  [<output file>]
 *
 *  Author:
 *    DSB: David Berry
 *
 *  History:
 *    13-OCT-95 (DSB):
 *       Original version.
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
