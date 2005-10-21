#include <stdio.h>
#include <stdarg.h>
#include <string.h>
#include <stdlib.h>

#include "sae_par.h"
#include "merswrap.h"
#include "msg_par.h"

/*
 * adamprint - convert a C-style print arguments to ADAM MSG output
 *
 * Uses va_list arguments as expected by the format statement.
 *
 * PWD: created this new version couldn't seem to find bug in older
 * one below...
 */
void adamprint( FILE *file, char *fmt, ... )
{
    char string[MSG__SZMSG];
    va_list args;
    int status = SAI__OK;
    char *fmtcpy;
    int len;
    int i;
    int j;

    /* Strip out any 4 character escape sequences from format string */
    len = strlen( fmt );
    fmtcpy = (char *) malloc( (size_t) len + 1 );
    for ( i = 0, j = 0; i < len; i++, j++ ) {
        if ( fmt[i] == '\33' ) i += 4;
        fmtcpy[j] = fmt[i];
    }
    fmtcpy[j]= '\0';

    /* And create the formatted string using the va_list arguments */
    va_start( args, fmt );
    vsprintf( string, fmtcpy, args );
    va_end( args );
    free( fmtcpy );

    /* Write out the string via the message system */
    msgOut( " ", string, &status );
}

#if 0
static void old_adamprint( FILE *file, char *fmt, ... )
{
    char *fmtcpy;
    char *i;
    char *j;
    char *p;
    char *pstart;
    char *sval;
    char cval;
    char pfmt[20];
    char string[MSG__SZMSG] = "";
    char tok[MSG__SZMSG];
    double dval;
    int ival;
    int status = SAI__OK;
    va_list ap;

    /* Copy format so we can safely modify it
     */
    fmtcpy = strdup( fmt );

    /* Get ap pointing to first unnamed arg after fmt.
     */
    va_start( ap, fmt );

    /* Loop over each character until at end of format string.
     */
    for ( p = pstart = fmtcpy; *p; p++ ) {

        /* Skip until we get to start of a format specifier */
        if ( *p != '%' ) {
            continue;
        }

        /* New format specifier now starting. Replace '%' with
         * null to terminate string up to this position. Previous
         * string now extends from pstart.
         */
        *p++ = '\0';

        /* Copy previous string to the token buffer, removing any
         * escape sequences (these are four characters). Skip null
         * sequences.
         */
        if ( p - pstart - 1 ) {
            for ( i = pstart, j = tok; *i; *j++ = *i++ ) {
                if ( *i == '\33' ) i = i + 4;
            }
            *j = '\0';
            strcat( string, tok );
        }

        /* Now process the trailing format statement. Extract it by
         * skipping over string until a non-numeric character is
         * located, or we get to the end of the format string.
         */
        pstart = p;
        for ( ; *p; p++ ) {
            if ( strchr( "0123456789.-", *p ) == NULL ) break;
        }

        /* Re-construct the format string
         */
        strcpy( pfmt, "%" );
        strncat( pfmt, pstart, p - pstart + 1 );

        /* Format the next va_list argument using this format
         * statemen. Result is place in tok.
         */
        switch ( *p ) {
            case 'c':
                cval = (char) va_arg( ap, int );
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

                /* Remove 4 character escape sequences from strings.
                 */
                for ( i = j = tok; *i; i++, j++ ) {
                    if ( *i == '\33' ) i = i + 4;
                    if ( i != j ) *j = *i;
                }
                *j = '\0';
                break;
        }

        /* Add the format value to the destination string
         */
        strcat( string, tok );

        /* Start again from end of current format specifier
         */
        pstart = p + 1;
    }

    /* Check for any remainder of format
     */
    if ( p != pstart ) {

        /* Remove any 4 character escape sequences and write to token
         * PWD: there is an occasional overrun here, when the above
         * test fails and we don't seem to see the end of the string.
         */
        for ( i = pstart, j = tok; *i; *j++ = *i++ ) {
            if ( *i == '\33' ) i = i + 4;
        }
        *j = '\0';
        strcat( string, tok );
    }

    /* Now output the message
     */
    msgOut( " ", string, &status );

    /* and clean up
     */
    free( fmtcpy );
    va_end( ap );
}
#endif
