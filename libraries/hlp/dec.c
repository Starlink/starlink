#include <string.h>
#include <ctype.h>
long hlpDec ( char *string, int *iptr )
/*
**  - - - - - - -
**   h l p D e c
**  - - - - - - -
**
**  Decode a long integer from a decimal-encoded character string.
**
**  Given:
**     *string    char   string containing decimal digits
**     *iptr      int    string index for start of decode
**
**  Returned (argument):
**     *iptr      int    advanced one character past last decimal
**
**  Returned (function value):
**                long   decoded number, or -1 if none found
**
**  Notes:
**
**  1)  Leading zeroes or spaces are both acceptable.
**
**  2)  Decoding ends with the first non-decimal.
**
**  3)  No sign is permitted.
**
**  4)  If the decode is ended by end-of-string, iptr is returned with
**      a value one greater than the length of the string.
**
**  5)  No decode is attempted if iptr is initially outside the
**      string.
**
**  P.T.Wallace   Starlink   29 August 1995
*/
{
/* Decode state:  0 = accepting leading spaces
                  1 = accepting digits
                  2 = number terminated        */
   int istate;

   int l, i, c;
   long n, idigit;


/* Length of string */
   l = (int) strlen ( string );

/* State = waiting for first digit. */
   istate = 0;

/* Default result. */
   n = -1l;

/* Initialize local copy of pointer. */
   i = *iptr;

/* Loop until end-of-string or end-of-number. */
   while ( i >= 0 && i < l && istate < 2 ) {

   /* Next character. */
      c = (int) string [ i++ ];

   /* Blank? */
      if ( isspace ( c ) ) {

      /* Yes: if we have seen numbers, correct pointer and terminate. */
         if ( istate > 0 ) {
            i -= 1;
            istate = 2;
         }

   /*  Digit? */
      } else if ( isdigit ( c ) ) {

      /* Yes: convert to integer. */
         idigit = (long) ( c - '0' );

      /* First digit to be seen? */
         if ( istate == 0 ) {

         /* Yes: start the number and set the state. */
            n = idigit;
            istate = 1;
         } else {

         /* Not the first: include it. */
            n = idigit + n * 10l;
         }
      } else {

      /* Unrecognized character: correct pointer and terminate. */
         i -= 1;
         istate = 2;
      }

   /* Next character. */
   }

/* Results. */
   *iptr = i;
   return n;
}
