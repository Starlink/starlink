#include <stdio.h>

int main( int argc, char **argv )
  {
  if ( argc < 2 ) 
    {
    printf( "Usage : logscrn filename\n\n" );
    }
  else
    {
    FILE *fp = fopen( argv[1], "w" );	/* Try to open file */

    if ( fp )
      {
      int more = 1;

      do
        {
        char c = fgetc( stdin );

        if ( ! feof(stdin) )
          {
          fputc( c, fp );
          putchar( c );
          }
        else
          more = 0;        
        }
      while ( more );

      fclose( fp );
      }
    else
      printf( "Unable to open file : %s\n", argv[1] );
    }
  }
