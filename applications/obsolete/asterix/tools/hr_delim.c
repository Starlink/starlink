#include <stdio.h>

/*
 * Echo the characters lying between the string <arg1> and </arg1> in the 
 * standard input to the standard output.
 */

int main( int argc, char **argv ) {
  char	c;
  int	elen, slen;			/* Lengths of tokens */
  char	etok[40],stok[40];		/* Start and end tokens */
  int	found = 0;			/* Find start yet? */
  int	ip,op=0;
  char	obuf[32768];

/* Make the start and end tokens */
  sprintf( stok, "<%s>", argv[1] );
  sprintf( etok, "</%s>", argv[1] );
  slen = strlen( stok );
  elen = strlen( etok );

/* Find the start of the string */
  while ( ! found && ! feof( stdin ) ) {

/*   Find the N'th character */
    ip = 0;
    while ( ip < slen) {
      c = getchar();
      if ( c == stok[ip] ) 
        ip++;
      else
        break;
      }
    found = (ip == slen);
    }

/* Found it? */
  if ( found ) {

/* Find the N'th character */
    ip = 0;
    while ( ip < elen) {
      c = getchar();
      obuf[op++] = c;
      if ( c == etok[ip] ) 
        ip++;
      else 
        ip = 0;
      }
    }

/* Write output */
  for( ip = 0; ip< (op - elen); ip ++ )
    putchar( obuf[ip] );

  return 0;
  }
