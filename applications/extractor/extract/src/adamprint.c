#include <stdio.h>
#include <stdarg.h>
#include <string.h>
#include "sae_par.h"
#include "merswrap.h"

/* adamprint - convert a C-style print arguments to ADAM MSG output
*/
void adamprint( FILE *file, char *fmt, ... )
{
   va_list ap;  /* points to each unnamed arg in turn */

   char *p, *pstart, *sval, *fmtcpy;
   char pfmt[10];
   char string[80]="";
   char tok[80];
   char cval;
   int ival;
   char *i, *j;
   double dval;
   int status = SAI__OK;

   fmtcpy = (char *)malloc( (size_t)strlen( fmt ) + 1 );
   strcpy( fmtcpy, fmt );

   va_start(ap, fmt); /* ap points to first unnamed arg */
   for (p=pstart=fmtcpy; *p; p++) {
      if (*p != '%') {
         continue;
      }
/* End of  a sequence of text */
/* Set in token if non-null   */
      *p++ = '\0';
      if (p-pstart-1) {
/* Remove any escape sequences and write to token */
/* N.B. ASSUMED 4 CHARACTER SEQUENCE */
         for ( i=pstart,j=tok; *i; *j++=*i++ )
            if ( *i == '\33' ) i = i+4;
         *j = '\0';
/*         msgSetc( "TOK", tok );*/
         strcat( string, tok );
      }          
/* Look for field conversion format */       
      pstart = p;
      for ( ; *p; p++ ) {
         if (strchr("0123456789.-",*p)==NULL) break;
      }         

/* Construct format string */
      strcpy( pfmt, "%" );
      strncat( pfmt, pstart, p-pstart+1 );
      
      switch (*p) {
      case 'c':
         cval = va_arg( ap, int );
         sprintf( tok, pfmt, cval );
         break;

      case 'd':
         ival = va_arg( ap, int );
         sprintf( tok, pfmt, ival );
         break;

      case 'f':
         dval = va_arg( ap, double );
         sprintf( tok, pfmt, dval );
         break;

      case 'g':
         dval = va_arg( ap, double );
         sprintf( tok, pfmt, dval );
         break;

      case 's':
         sval = va_arg( ap, char* );
         sprintf( tok, pfmt, sval );
/*     Remove any escape sequences */
/*     N.B. ASSUMED 4 CHARACTER SEQUENCE */
         for ( i=j=tok; *i; i++,j++) {
            if ( *i == '\33' ) i = i+4;
            if ( i != j ) *j = *i;
         }
         *j = '\0';         
         break;
      }

/*      msgSetc( "TOK", tok );*/
      strcat( string, tok );
      pstart = p + 1;

   }

/* Check for any remainder of format */
   if ( p != pstart ) {
/* Remove any escape sequences and write to token */
/* N.B. ASSUMED 4 CHARACTER SEQUENCE */
      for ( i=pstart,j=tok; *i; *j++=*i++ )
         if ( *i == '\33' ) i = i+4;
      *j = '\0';
/*      msgSetc( "TOK", tok );*/
      strcat( string, tok );
   }

/* Now output the message */
   msgOut( " ", string, &status );
/* and clean up */
   free( fmtcpy );
   va_end(ap);
}
