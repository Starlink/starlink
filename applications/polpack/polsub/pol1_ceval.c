/* Module Macros. */
/* ============== */
#define AST__NOTYPE       -1
#define AST__COMMENT       0
#define AST__INT           1
#define AST__FLOAT         2
#define AST__STRING        3
#define AST__COMPLEXF      4
#define AST__COMPLEXI      5
#define AST__LOGICAL       6

#define FITSNAMLEN         8
#define FITSSTCOL          20
#define FITSRLCOL          30
#define FITSIMCOL          50
#define FITSCOMCOL         32
#define FITSCARDLEN        80
#define MXLIT              80

/* A macro which tests a character to see if it can be used within a FITS
   keyword name. We include lower case letters here, but they are considered
   as equivalent to upper case letter. */
#define isFits(a) ( islower(a) || isupper(a) || isdigit(a) || (a)=='-' || (a)=='_' )

/* Constants: */
#define DELIM 249
#define OPEN_P 250
#define CLOSE_P 251
#define EQUALS 252
#define CONCAT 253
#define OPEN_Q 254
#define CLOSE_Q 255

/* Include files. */
/* ============== */
#include "sae_par.h"
#include "f77.h"
#include "cnf.h"
#include "mers.h"
#include "ast.h"
#include "star/grp.h"
#include "star/one.h"
#include <ctype.h>
#include <float.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* Type Definitions */
/* ================ */

/* Module Variables. */
/* ================= */

/* Prototypes for Functions. */
/* ========================= */

int pol1Split( const char *, char **, char **, char **, int *);
int pol1Ustrcmp( const char *, const char * );
int pol1Ustrncmp( const char *, const char *, size_t );
int pol1FullForm( const char *, const char *, int * );

unsigned char *pol1Fchr( unsigned char *, unsigned char *,
                         unsigned char, int * );

int pol1Teval( unsigned char *, unsigned char *, AstFitsChan *,
                Grp *, Grp *, char **, char *, int * );

int pol1Ceval( unsigned char *, unsigned char *, AstFitsChan *,
               Grp *, Grp *, char **, char *, int * );

unsigned char *pol1Seval( unsigned char *, unsigned char *,
                          AstFitsChan *, int, Grp *,
                          Grp *, char **, char *, int *,
                          int * );

int pol1Cmpr( char *, char *, char *, char * );

/* Functions Definitions. */
/* ====================== */
F77_SUBROUTINE(pol1_ceval)( CHARACTER(EXPR), INTEGER(FCHAN), INTEGER(IGRP1),
                            INTEGER(IGRP2), CHARACTER(VALUE), LOGICAL(OK),
                            INTEGER(STATUS) TRAIL(EXPR) TRAIL(VALUE) ){
/*
*  Name:
*     POL1_CEVAL

*  Purpose:
*     Evaluate an import control table expression as a character function.

*  Language:
*     Starlink C (callable from Fortran 77)

*  Invocation:
*     CALL POL1_CEVAL( EXPR, FCHAN, IGRP1, IGRP2, VALUE, OK, STATUS )

*  Description:
*   This routine evaluates an import control table expression as a
*   character function.

*  Arguments:
*     EXPR = CHARACTER * ( * ) (Given)
*        The expression.
*     FCHAN = INTEGER (Given)
*        An AST FitsChan containing the FITS header cards to be used when
*        resolving references to FITS keywords contained in EXPR.
*     IGRP1 = INTEGER (Given)
*        A GRP identifier for a group holding HDS data types. Ignored
*        if either IGRP1 or IGRP2 is GRP__NOID.
*     IGRP2 = INTEGER (Given)
*        A GRP identifier for a group holding FITS keyword names. Ignored
*        if either IGRP1 or IGRP2 is GRP__NOID.
*     VALUE = CHARACTER * ( * ) (Returned)
*        The expression value. Set blank if OK is .FALSE.
*     OK = LOGICAL (Returned)
*        Was the supplied expression a character function? If not, it
*        must be a numerical function. This flag is returned .TRUE. even
*        if there were references to undefined FITS keywords within the
*        expression. It is returned .FALSE. only if there were no
*        character operators within the string ("//", "=", quotes,
*        backslashes), and the expression contained one or more characters
*        which are not legal within a FITS keyword name.
*     STATUS = INTEGER (Given and Returned)
*        The global status. A value of SAI__WARN is returned iff any
*        undefined FITS keywords are referenced within the expression.

*  Notes:
*     -  To be a character function the supplied expression must have the
*     following form:
*
*        term [ // term ] [// term ] ...
*
*     The terms in this expression are concatenated together. A "term" has
*     the following form:
*
*        value [ literal = literal ] [ literal = literal ] ...
*
*     If the value equals any of the left hand literals, then the value
*     is replaced by the corresponding right hand literal. Note, commas
*     may be used as delimiters in place of spaces. A value has the
*     following form:
*
*        string | name
*
*     and a literal has the following form:
*
*        string | word
*
*     A string is any text enclosed in matching single or double quotes.
*     quotes within these strings can be escaped by a backslash character
*     "\".
*
*     A name is the name of a known FITS keywords (i.e. a KEYWORD which
*     has a defined value in the supplied FItsChan). An error is reported
*     if the FITS keyword is not of type _CHAR (either because it has been
*     explicitly declared as _CHAR in IGRP1 and IGRP2, or because its
*     natural FITS type is _CHAR). The only exception to this is that no
*     error is reported if the expression consists of just the keyword
*     name. In this case a blank VALUE is returned with OK = .FALSE., but
*     no error is reported.
*
*     A word is any text which does not include any of the following:
*       - spaces
*       - equal signs
*       - single quotes
*       - double quotes
*       - adjacent slashes (/)

*  Copyright:
*     Copyright (C) 1998 Central Laboratory of the Research Councils

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     23-APR-1999 (DSB):
*        Original version.
*     8-AUG-2006 (DSB):
*        Change to use GRP C interface.
*        Fix bug that caused mis-handling of spaces.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*/


/* Arguments: */
      GENPTR_CHARACTER(EXPR)
      GENPTR_INTEGER(FCHAN)
      GENPTR_INTEGER(IGRP1)
      GENPTR_INTEGER(IGRP2)
      GENPTR_CHARACTER(VALUE)
      GENPTR_LOGICAL(OK)
      GENPTR_INTEGER(STATUS)

/* Local Variables: */
      Grp *grp1;              /* Pointer to IGRP1 group */
      Grp *grp2;              /* Pointer to IGRP2 group */
      char *c;                /* Pointer to next input character */
      char quote;             /* The opening quote character */
      char *v;                /* Pointer to start of returned value string */
      char *vl;               /* Pointer to end of returned value string */
      int esc;                /* Is current character escaped? */
      int i;                  /* Loop count */
      int iend;               /* Length of supplied expression */
      int ok;                 /* Could the term be evaluated? */
      int pair;               /* Was previous char an un-escaped "|"? */
      unsigned char *d;       /* Pointer to next output character */
      unsigned char *expr;    /* Tokenized copy of supplied expression */

/* Initialise */
      for( i = 0; i < VALUE_length; i++ ) VALUE[ i ] = ' ';
      *OK = F77_FALSE;

/* Check the STATUS value. */
      if( *STATUS != SAI__OK ) return;

/* Find the used length of the supplied EXPR string. */
      iend = cnf_lenf( EXPR, EXPR_length );

/* Allocate memory to hold a null-terminated copy of this string. */
      expr = malloc( ( iend + 1 )*sizeof( char ) );
      if( !expr ) {
         *STATUS = SAI__ERROR;
         errRep( " ", "POL1_CEVAL: Unable to allocate memory.",
                  STATUS );
         return;
      }

/* Replace un-escaped control characters in the expression with special
   integer token values. */
      esc = 0;
      d = expr;
      quote = ' ';
      pair = 0;
      ok = 0;

      for( c = EXPR; c < EXPR + iend; c++ ){

/* If this character was escaped, copy it to the output string, and
   indicate that the next character is not escaped. Escape characters can
   only appear within character functions. So if this is an escape character
   we must have a character function, so set "ok" non-zero. */
         if( esc ) {
            *(d++) = *c;
            esc = 0;
            ok= 1;

/* If this character has not been escaped, see if it is an escape
   character. If so do not copy it but indicdate that the next charater is
   escaped. */
         } else if( *c == '\\' ){
            esc = 1;

/* Now compare this (un-escaped) character with each of the special
   control characters. If a match is found, store a special integer token
   in the output string in place of the input character. Matches are only
   allowed outside quoted strings (except for closing quotes). Set "ok"
   non-zero if any characters are found which can only appear within
   character functions. Also remove any uncessarary delimiters before
   or after an equals sign or a concatentation operator (//). */
         } else if( ( *c == ' ' || *c == ',' ) && quote == ' ' ){
            if( *( d - 1 ) != DELIM &&
                *( d - 1 ) != EQUALS &&
                *( d - 1 ) != CONCAT ) *(d++) = DELIM;

         } else if( *c == '(' && quote == ' ' ){
            *(d++) = OPEN_P;

         } else if( *c == ')' && quote == ' ' ){
            *(d++) = CLOSE_P;

         } else if( *c == '=' && quote == ' ' ){
            if( *( d - 1 ) == DELIM ) d--;
            *(d++) = EQUALS;
            ok = 1;

         } else if( *c == '/' && pair && quote == ' ' ){
            if( *( d - 2 ) == DELIM ) d--;
            *( d - 1 ) = CONCAT;
            pair = 0;
            ok = 1;

         } else if( ( *c == '\'' || *c == '\"' ) && quote == ' ' ){
            quote = *c;
            *(d++) = OPEN_Q;
            ok = 1;

         } else if( *c == quote && quote != ' ' ){
            quote = ' ';
            *(d++) = CLOSE_Q;
            ok = 1;

/* If the current character is not a control character, copy it as it is. */
         } else {
            *(d++) = *c;
            pair = ( quote == ' ' && *c == '/' );
         }

      }

/* Terminate the string. */
      *d = 0;

/* Convert the supplied F77 GRP identifiers into C GRP pointers. */
      grp1 = grpF2C( *IGRP1, STATUS );
      grp2 = grpF2C( *IGRP2, STATUS );

/* Process the whole expression. */
      v = VALUE;
      vl = VALUE + VALUE_length - 1;
      ok = pol1Ceval( expr, d, astI2P( *FCHAN ), grp1, grp2, &v,
                      vl, STATUS ) || ok;

/* Free the Grp structurs used to hold the F77 GRP identifiers. */
      grp1 = grpFree( grp1, STATUS );
      grp2 = grpFree( grp2, STATUS );

/* Pad the returned string with spaces. */
      for( ; v < vl; v++ ) *v = ' ';

/* Return the flag indicating if the expression was a character function. */
      if( ok ) {
         *OK = F77_TRUE;
      } else {
         *OK = F77_FALSE;
      }

/* Free memory. */
      if( expr ) free( expr );

/* Return. */
      return;

}

int pol1Ceval( unsigned char *e0, unsigned char *e1, AstFitsChan *fchan,
               Grp *grp1, Grp *grp2, char **v, char *vl, int *status ){
/*
*  Name:
*     pol1Ceval

*  Purpose:
*     Evaluate an import control table expression as a character function.

*  Language:
*     Starlink C

*  Synopsis:
*     int pol1Ceval( unsigned char *e0, unsigned char *e1,
*                    AstFitsChan *fchan, Grp *grp1, Grp *grp2, char **v,
*                    char *vl, int *status )

*  Description:
*   This routine evaluates an entire expression as a character function.
*   Special characters in the expression should have been replaced by the
*   appropriate control codes before calling this function.
*
*   The string to which the expression evaluates is stored in the memory
*   pointed to by *v.

*  Arguments:
*     e0
*        Pointer to the first character of the expression.
*     e1
*        Pointer to the first character beyond the end of the expression.
*     fchan
*        A pointer to an AST FitsChan containing the FITS header cards to
*        be used when resolving references to FITS keywords contained in
*        the expression.
*     grp1
*        Pointer to the GRP group holding HDS data types. Ignored
*        if either grp1 or grp2 is NULL.
*     grp2
*        Pointer to the GRP group holding FITS keyword names. Ignored
*        if either grp1 or grp2 is NULL.
*     v
*        Address of a pointer to the start of the string to receive the string
*        corresponding to the expression. The pointer is updated on exit
*        to hold the address of the next character following the end of
*        the string.
*     vl
*        Pointer to the end of the string to receive the string
*        corresponding to the expression. The string should not be null
*        terminated.
*     status
*        The global status.

*  Returned Value:
*        Was the first value in every term made up exclusively of
*        characters which are legal in the context of a FITS keyword name?

*  Copyright:
*     Copyright (C) 1998 Central Laboratory of the Research Councils

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     26-APR-1999 (DSB):
*        Original version.
*     8-AUG-2006 (DSB):
*        Change to use GRP C interface.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*/

/* Local Variables: */
      int ok;                 /* Was expression evaluated succesfully? */
      unsigned char *d;       /* Pointer to next output character */
      unsigned char *e;       /* Pointer to first character in the next term */

/* Check the inherited status. */
      if( *status != SAI__OK ) return 1;

/* Process each term in the expression. Terms are delimited by CONCAT
   tokens at zero level of parenthesis nesting. */
      ok = 1;
      d = e0;
      while( d < e1 && ok && *status == SAI__OK ){

/* Find the first CONCAT token following the current character which is
   not contained within parentheses. The returned value equals e1 if no
   CONCAT character is found. */
         e = pol1Fchr( d, e1, CONCAT, status );

/* Process this term. */
         ok = ok && pol1Teval( d, e, fchan, grp1, grp2, v, vl, status );

/* Make d point to the first character in the next term. */
         d = e + 1;

      }

/* Return. */
      return ok;

}

unsigned char *pol1Fchr( unsigned char *e0, unsigned char *e1,
                         unsigned char chr, int *status ){
/*
*  Name:
*     pol1Fchr

*  Purpose:
*     Find the next occurence of a given character which is not enclosed
*     within parentheses.

*  Language:
*     Starlink C

*  Synopsis:
*      unsigned char *pol1Fchr( unsigned char *e0, unsigned char *e1,
*                               unsigned char chr, int *status )

*  Description:
*   This routine returns a pointer to the first occurence of a given
*   character within a string which is not enclosed within parentheses.

*  Arguments:
*     e0
*        Pointer to the first character of the expression.
*     e1
*        Pointer to the first character beyond the end of the expression.
*     chr
*        The character to be searched for.
*     status
*        The global status.

*  Returned Value:
*     A pointer to the found character. Returned equal to e1 if no character
*     was found.

*  Copyright:
*     Copyright (C) 1998 Central Laboratory of the Research Councils

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     26-APR-1999 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*/

/* Local Variables: */
      int nest;               /* Level of parenthesis nesting */
      unsigned char *e;       /* Pointer to next character */

/* Check the inherited status. */
      if( *status != SAI__OK ) return e1;

      nest = 0;
      e = e0;
      while( e < e1 ){
         if( *e == chr && nest == 0 ) {
            break;
         } else if( *e == OPEN_P ){
            nest++;
         } else if( *e == CLOSE_P ){
            nest--;
         }
         e++;
      }

      return e;
}

int pol1Split( const char *card, char **name, char **value, char **comment,
           int *status ){
/*
*  Name:
*     pol1Split

*  Purpose:
*     Extract the keyword value from a FITS header card.

*  Synopsis:
*     int pol1Split( const char *card, char **name, char **value, char **comment,
*                char **value, int *status )

*  Description:
*     The name, value and comment are extracted from the supplied FITS
*     header card and returned.

*  Parameters:
*     card
*        Pointer to a string holding the FITS header card.
*     name
*        Pointer to a location at which to return the pointer to a string
*        holding the keyword name.
*     value
*        Pointer to a location at which to return the pointer to a string
*        holding the keyword value.
*     comment
*        Pointer to a location at which to return the pointer to a string
*        holding the keyword comment.
*     status
*        Inherited status value.

*  Returned value:
*     -  An integer identifying the data type of the keyword value. This
*     will be one of the values AST__COMMENT, AST__INT, AST__STRING,
*     AST__FLOAT, AST__COMPLEXI or AST__COMPLEXF.

*  Notes:
*     -  If the keyword value is a string, then the returned value does not
*     include the delimiting quotes, and pairs of adjacent quotes within the
*     string are replaced by single quotes.
*     -  A maximum of 80 characters are read from the supplied card, so the
*     string does not need to be null terminated unless less than 80
*     characters are to be read.
*     -  The memory holding the string "value" should be released when no
*     longer needed using free.
*     -  A NULL pointer and a data type of AST__COMMENT are returned if an
*     error has already occurred, or if this function fails for any reason.
*/

/* Local Variables: */
   char *c;                   /* Pointer to returned comment string */
   char *dd;                  /* Pointer to intermediate character */
   char *slash;               /* Pointer to comment character */
   char *v;                   /* Pointer to returned value string */
   const char *d;             /* Pointer to first comment character */
   const char *v0;            /* Pointer to first non-blank value character */
   double fi, fr;             /* Values read from value string */
   int blank_name;            /* Is keyword name blank? */
   int i;                     /* Character index */
   int ii, ir;                /* Values read from value string */
   int iopt;                  /* Index of option within list */
   int lq;                    /* Was previous character an escaping quote? */
   int len;                   /* Used length of value string */
   int nch;                   /* No. of characters used */
   int type;                  /* Keyword data type */
   size_t nc;                 /* Number of character in the supplied card */
   size_t ncc;                /* No. of characters in the comment string */
   size_t ncv;                /* No. of characters in the value string */

/* Initialise the returned pointers. */
   *value = NULL;
   type = AST__COMMENT;

/* Check the global status. */
   if( *status != SAI__OK ) return type;

/* Store the number of characters to be read from the supplied card. This
   is not allowed to be more than the length of a FITS header card.
   Trailing white space and non-printing characters such as new-line are
   ignored. */
   nc = cnf_lenc( card );
   if( nc > FITSCARDLEN ) nc = FITSCARDLEN;

/* Allocate memory for a copy of the keyword name plus a terminating
   null character. */
   *name = (char *) malloc( ( 1 + FITSNAMLEN )*sizeof(char) );

/* Check the pointer can be used. */
   if( *name ){

/* Initialise the name string by filling it with spaces, and terminating it. */
      for( i = 0; i < FITSNAMLEN; i++ ) (*name)[ i ] = ' ';
      (*name)[ FITSNAMLEN ] = 0;

/* Copy the the keyword name, ensuring that no more than FITSNAMLEN (8)
   characters are copied. */
      strncpy( *name, card, ( nc > FITSNAMLEN ) ? FITSNAMLEN : nc );

/* If there is no keyword name, flag that we have a blank name which will
   be treated as a comment card. */
      if( strspn( *name, " " ) == strlen( *name ) ){
         blank_name = 1;

/* If the card contains a keyword name, replace any trailing blanks with
   nulls. */
      } else {
         blank_name = 0;
         dd = *name + strlen( *name ) - 1;
         while( *dd == ' ' ) *(dd--) = 0;
      }

/* Allocate memory to hold the keyword value and comment strings. */
      *value = (char *) malloc( sizeof(char)*( 2 + nc ) );
      *comment = (char *) malloc( sizeof(char)*( 1 + nc ) );

/* Check the pointers can be used. */
      if( *status == SAI__OK && *value && *comment ){

/* If column 9 does not contain an equals sign, or if the keyword is
   "HISTORY", "COMMENT" or blank, then columns 9 to the end are
   comment characters, and the value string is null. */
         if( nc <= FITSNAMLEN || card[ FITSNAMLEN ] != '='
                              || !pol1Ustrcmp( *name, "HISTORY" )
                              || !pol1Ustrcmp( *name, "COMMENT" )
                              || blank_name ){
            (*value)[ 0 ] = 0;
            if( nc > FITSNAMLEN ){
               (void) strncpy( *comment, card + FITSNAMLEN,
                               nc - FITSNAMLEN );
               (*comment)[ nc - FITSNAMLEN ] = 0;
            } else {
               (*comment)[ 0 ] = 0;
            }

/* Otherwise there is a value field. */
         } else {

/* Find the first non-blank character in the value string. */
            v0 = card + FITSNAMLEN + 1;
            while( (size_t)(v0 - card) < nc &&
                   isspace( (int) *v0 ) ) v0++;

/* Store pointers to the start of the returned value and comment strings. */
            v = *value;
            c = *comment;

/* If the first character in the value string is a single quote, the value is
   a string. In this case the value ends at the first non-escaped single
   quote. */
            if( *v0 == '\''){
               type = AST__STRING;

/* We want to copy the string value, without the delimiting quotes, to the
   returned value string. Single quotes within the string are represented
   by two adjacent quotes, so we also need to check for these and replace
   them by one quote in the returned string. First initialise a pointer
   to the first character after the opening quote, and set a flag
   indicating that (for the purposes of identifying pairs of adjacent
   quotes within the string) the previous character was not a quote. */
               d = v0 + 1;
               lq = 0;

/* Loop round each remaining character in the supplied card. */
               while( (size_t)(d - card) < nc ){

/* If the current character is a single quote... */
                  if( *d == '\'' ){

/* If the previous character was also a single quote then the quote does
   not mark the end of the string, but is a quote to be included literally
   in the value. Copy the quote to the returned string and clear the flag
   to indicate that the pair of adjacent quotes is now complete. */
                    if( lq ){
                       *(v++) = '\'';
                       lq = 0;

/* If the last character was not a quote, then set the flag for the next
   pass through the loop, but do not copy the quote to the returned string
   since it will either be a quote escaping a following adjacent quote, or
   a quote to mark the end of the string. */
                    } else {
                       lq = 1;
                    }

/* If the current character is not a quote... */
                  } else {

/* If the previous character was a quote, then we have found a single
   isolated quote which therefore marks the end of the string value.
   The pointer "d" is left pointing to the first character
   after the terminating quote. */
                     if( lq ){
                        if( (size_t)( d - card ) < FITSSTCOL - 1 ) break;

/* If the last character was not a quote, copy it to the returned string. */
                     } else {
                        *(v++) = *d;
                     }
                  }
                  d++;
               }

/* Terminate the returned value string. */
               *v = 0;

/* Now deal with logical and numerical values. */
            } else {

/* The end of the value field is marked by the first "/". Find the number
   of characters in the value field. Pointer "d" is left pointing to the
   first character in the comment (if any). */
               d = strchr( card, '/' );
               if( !d ){
                  ncv = nc - FITSNAMLEN - 1;
               } else {
                  ncv = (size_t)( d - card ) - FITSNAMLEN - 1;
               }

/* Copy the value string to the returned string. */
               if( ncv == 0 ){
                  *v = 0;
               } else {
                  strncpy( v, card + FITSNAMLEN + 1, ncv );
                  v[ ncv ] = ' ';
                  v[ ncv + 1 ] = 0;
               }

/* Find the first non-blank character in the value string. */
               v0 = v;
               while( *v0 && isspace( (int) *v0 ) ) v0++;

/* See if the value string is one of the following strings (optionally
   abbreviated and case insensitive): YES, NO, TRUE, FALSE. */
               iopt = pol1FullForm( "YES NO TRUE FALSE", v0, status );

/* Return the single character "T" or "F" at the start of the value string
   if the value matches one of the above strings. */
               if( iopt == 0 || iopt == 2 ) {
                  type = AST__LOGICAL;
                  strcpy ( v, "T" );

               } else if( iopt == 1 || iopt == 3 ) {
                  type = AST__LOGICAL;
                  strcpy ( v, "F" );

/* If it does not match, see if the value is numerical. */
               } else {

/* Save the length of the value string excluding trailing blanks. */
                  len = cnf_lenc( v );

/* If there are no dots (decimal points) in the value... */
                  if( !strchr( v, '.' ) ){

/* First attempt to read two integers from the string (separated by white
   space). */
                     if( nch = 0,
                         ( 2 == sscanf( v, " %d %d%n", &ir, &ii, &nch ) ) &&
                         ( nch >= len ) ) {
                        type = AST__COMPLEXI;

/* If that failed, attempt to read a single integer from the string. */
                     } else if( nch = 0,
                         ( 1 == sscanf( v, " %d%n", &ir, &nch ) ) &&
                         ( nch >= len ) ) {
                        type = AST__INT;
                     }

/* If there are dots (decimal points) in the value... */
                  } else {

/* First attempt to read two doubles from the string (separated by white
   space). */
                     if( nch = 0,
                         ( 2 == sscanf( v, " %lf %lf%n", &fr, &fi, &nch ) ) &&
                         ( nch >= len ) ) {
                        type = AST__COMPLEXF;

/* If that failed, attempt to read a single double from the string. */
                     } else if( nch = 0,
                         ( 1 == sscanf( v, " %lf%n", &fr, &nch ) ) &&
                         ( nch >= len ) ) {
                        type = AST__FLOAT;
                     }

/* If both the above failed, it could be because the string contains a
   "D" exponent (which is probably valid FITS) instead of an "E" exponent.
   Replace any "D" in the string with "e" and try again. */
                     if( type == AST__COMMENT ) {

/* Replace "d" and "D" by "e" (if this doesn't produce a readable floating
   point value then the value string will not be used, so it is safe to
   do the replacement in situ). */
                        for( i = 0; i < len; i++ ) {
                           if( v[ i ] == 'd' || v[ i ] == 'D' ) v[ i ] = 'e';
                        }

/* Attempt to read two doubles from the edited string (separated by white
   space). */
                        if( nch = 0,
                          ( 2 == sscanf( v, " %lf %lf%n", &fr, &fi, &nch ) ) &&
                          ( nch >= len ) ) {
                           type = AST__COMPLEXF;

/* If that failed, attempt to read a single double from the edited string. */
                        } else if( nch = 0,
                            ( 1 == sscanf( v, " %lf%n", &fr, &nch ) ) &&
                            ( nch >= len ) ) {
                           type = AST__FLOAT;
                        }
                     }
                  }
               }

/* If the value type could not be determined report an error. */
               if( type == AST__COMMENT ) {
                  *status = SAI__ERROR;
                  errRep( " ", "Illegal keyword value supplied.",
                          status );
               }
            }

/* Find the number of characters in the comment. Pointer "d" should point to
   the first character following the value string. */
            if( d ){
               ncc = nc - (size_t)( d - card );
            } else {
               ncc = 0;
            }

/* Copy the remainder of the card to the returned comment string. */
            if( *status == SAI__OK && ncc > 0 ){
               strncpy( c, d, ncc );
               c[ ncc ] = 0;

/* Find the start of the comment (indicated by the first "/" after the
   value string). */
               slash = strchr( c, '/' );

/* Temporarily terminate the string at the slash. */
               if( slash ) *slash = 0;

/* Shuffle the characters following the slash down to the
   start of the returned string. */
               if( slash ){
                  ncc -= (size_t)( slash - c ) + 1;
                  d = slash + 1;
                  for( i = 0; i < 1 + (int) ncc; i++ ) *(c++) = *(d++);
               }

/* If there is no comment string, return a null string. */
            } else {
               *c = 0;
            }
         }
      }
   }

/* If an error occurred, free the returned strings. */
   if( *status != SAI__OK ){
      free( (void *) *name );
      free( (void *) *value );
      free( (void *) *comment );
      *name = NULL;
      *value = NULL;
      *comment = NULL;
      type = AST__COMMENT;
   }

/* Return the data type. */
   return type;

}

int pol1FullForm( const char *list, const char *test, int *status ){
/*
*  Name:
*     pol1FullForm

*  Purpose:
*     Identify the full form of an option string.

*  Synopsis:
*     int pol1FullForm( const char *list, const char *test, int *status )

*  Description:
*     This function identifies a supplied test option within a supplied
*     list of valid options, and returns the index of the option within
*     the list. The test option may be abbreviated, and case is
*     insignificant.

*  Parameters:
*     list
*        A list of space separated option strings.
*     test
*        A candidate option string.
*     status
*        Inherited status.

*  Returned Value:
*     The index of the identified option within the supplied list, starting
*     at zero. -1 is returned if the option is not recognised, and -2 if
*     the option is ambiguous (no errors are reported in these cases).

*  Notes:
*     -  A value of -1 is returned if an error has already occurred, or
*     if this function should fail for any reason.

*/

/* Local Variables: */
   char *llist;            /* Pointer to a local copy of the options list */
   char *option;           /* Pointer to the start of the next option */
   int i;                  /* Current option index */
   int len;                /* Length of supplied option */
   int nmatch;             /* Number of matching options */
   int ret;                /* The returned index */

/* Initialise the answer to indicate that the option has not been
   identified. */
   ret = -1;

/* Check global status. */
   if( *status != SAI__OK ) return ret;

/* Take a local copy of the supplied options list. This is necessary since
   "strtok" modified the string by inserting null characters. */
   llist = (char *) malloc( ( strlen(list) + 1 )*sizeof( char ) );
   if( llist ){
      strcpy( llist, list );

/* Save the number of characters in the supplied test option (excluding
   trailing spaces). */
      len = cnf_lenc( test );

/* Compare the supplied test option against each of the known options in
   turn. Count the number of matches. */
      nmatch = 0;
      option = strtok( llist, " " );
      i = 0;
      while( option ){

/* If every character in the supplied label matches the corresponding
   character in the current test label we have a match. Increment the
   number of matches and save the current item index. */
         if( !pol1Ustrncmp( test, option, len ) ) {
            nmatch++;
            ret = i;
         }

/* Get a pointer to the next option. */
         option = strtok( NULL, " " );
         i++;
      }

/* Return -1 if no match was found. */
      if( !nmatch ){
         ret = -1;

/* Return -2 if the option was ambiguous. */
      } else if( nmatch > 1 ){
         ret = -2;
      }

/* Free the local copy of the options list. */
      free( (void *) llist );
   }

/* Return the answer. */
   return ret;
}

int pol1Ustrcmp( const char *a, const char *b ){
/*
*  Name:
*     pol1Ustrcmp

*  Purpose:
*     A case blind version of strcmp.

*  Synopsis:
*     int pol1Ustrcmp( const char *a, const char *b )

*  Description:
*     Returns 0 if there are no differences between the two strings, and 1
*     otherwise. Comparisons are case blind.

*  Parameters:
*     a
*        Pointer to first string.
*     b
*        Pointer to second string.

*  Returned Value:
*     Zero if the strings match, otherwise one.

*  Notes:
*     -  This function does not consider the sign of the difference between
*     the two strings, whereas "strcmp" does.
*     -  This function attempts to execute even if an error has occurred.

*/

/* Local Variables: */
   const char *aa;         /* Pointer to next "a" character */
   const char *bb;         /* Pointer to next "b" character */
   int ret;                /* Returned value */

/* Initialise the returned value to indicate that the strings match. */
   ret = 0;

/* Initialise pointers to the start of each string. */
   aa = a;
   bb = b;

/* Loop round each character. */
   while( 1 ){

/* We leave the loop if either of the strings has been exhausted. */
      if( !(*aa ) || !(*bb) ){

/* If one of the strings has not been exhausted, indicate that the
   strings are different. */
         if( *aa || *bb ) ret = 1;

/* Break out of the loop. */
         break;

/* If neither string has been exhausted, convert the next characters to
   upper case and compare them, incrementing the pointers to the next
   characters at the same time. If they are different, break out of the
   loop. */
      } else {

         if( toupper( (int) *(aa++) ) != toupper( (int) *(bb++) ) ){
            ret = 1;
            break;
         }

      }

   }

/* Return the result. */
   return ret;

}

int pol1Ustrncmp( const char *a, const char *b, size_t n ){
/*
*  Name:
*     pol1Ustrncmp

*  Purpose:
*     A case blind version of strncmp.

*  Synopsis:
*     int pol1Ustrncmp( const char *a, const char *b, size_t n )

*  Description:
*     Returns 0 if there are no differences between the first "n"
*     characters of the two strings, and 1 otherwise. Comparisons are
*     case blind.

*  Parameters:
*     a
*        Pointer to first string.
*     b
*        Pointer to second string.
*     n
*        The maximum number of characters to compare.

*  Returned Value:
*     Zero if the strings match, otherwise one.

*  Notes:
*     -  This function does not consider the sign of the difference between
*     the two strings, whereas "strncmp" does.
*     -  This function attempts to execute even if an error has occurred.

*/

/* Local Variables: */
   const char *aa;         /* Pointer to next "a" character */
   const char *bb;         /* Pointer to next "b" character */
   int i;                  /* Character index */
   int ret;                /* Returned value */

/* Initialise the returned value to indicate that the strings match. */
   ret = 0;

/* Initialise pointers to the start of each string. */
   aa = a;
   bb = b;

/* Compare up to "n" characters. */
   for( i = 0; i < (int) n; i++ ){

/* We leave the loop if either of the strings has been exhausted. */
      if( !(*aa ) || !(*bb) ){

/* If one of the strings has not been exhausted, indicate that the
   strings are different. */
         if( *aa || *bb ) ret = 1;

/* Break out of the loop. */
         break;

/* If neither string has been exhausted, convert the next characters to
   upper case and compare them, incrementing the pointers to the next
   characters at the same time. If they are different, break out of the
   loop. */
      } else {

         if( toupper( (int) *(aa++) ) != toupper( (int) *(bb++) ) ){
            ret = 1;
            break;
         }

      }

   }

/* Return the result. */
   return ret;

}

int pol1Teval( unsigned char *t0, unsigned char *t1, AstFitsChan *fchan,
               Grp *grp1, Grp *grp2, char **v, char *vl, int *status ){
/*
*  Name:
*     pol1Teval

*  Purpose:
*     Evaluate a term of an import control table character expression.

*  Language:
*     Starlink C

*  Synopsis:
*     int pol1Teval( unsigned char *t0, unsigned char *t1, AstFitsChan *fchan,
*                    Grp *grp1, Grp *grp2, char **v, char *vl, int *status )

*  Description:
*   This routine evaluates a single term of an expression. A term has the
*   form:
*           value [literal1=literal2] [literal1=literal2] ...
*
*   where "value" can be:
*      - A quoted string.
*      - A FITS keyword name
*      - An entire expression in parentheses.
*
*   and the literals can be:
*      - A quoted string
*      - A single literal word with no spaces, or other control characters
*      - An entire expression in parentheses.
*
*   The string to which "value" evaluates is compared with each of the
*   "literal1" strings (case insensitive). If a match is found, the
*   corresponding literal2 value is stored at *v. If no match is found,
*   the original value is returned at *v.

*  Arguments:
*     e0
*        Pointer to the first character of the expression.
*     e1
*        Pointer to the first character beyond the end of the expression.
*     fchan
*        A pointer to an AST FitsChan containing the FITS header cards to
*        be used when resolving references to FITS keywords contained in
*        the expression.
*     grp1
*        Pointer to the GRP group holding HDS data types. Ignored
*        if either grp1 or grp2 is NULL.
*     grp2
*        Pointer to the GRP group holding FITS keyword names. Ignored
*        if either grp1 or grp2 is NULL.
*     v
*        Address of a pointer to the start of the string to receive the string
*        corresponding to the expression. The pointer is updated on exit
*        to hold the address of the next character following the end of
*        the string.
*     vl
*        Pointer to the end of the string to receive the string
*        corresponding to the expression. The string should not be null
*        terminated.
*     status
*        The global status.

*  Returned Value:
*        Was the first value in the term made up exclusively of
*        characters which are legal in the context of a FITS keyword name?

*  Copyright:
*     Copyright (C) 1998 Central Laboratory of the Research Councils

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     26-APR-1999 (DSB):
*        Original version.
*     8-AUG-2006 (DSB):
*        Change to use GRP C interface.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*/

/* Local Variables: */
      unsigned char *d;   /* Pointer to next delimiter */
      unsigned char *e;   /* Pointer to next unsigned character*/
      unsigned char *f;   /* Pointer to equals sign */
      char *lhs0;         /* Pointer to start of left string */
      char *rhs0;         /* Pointer to start of right string */
      char *lhs1;         /* Pointer to end of left string */
      char *rhs1;         /* Pointer to end of right string */
      char lhs[ MXLIT + 1];/* Left string */
      char rhs[ MXLIT + 1];/* Right string */
      char *v0;           /* Pointer to returned string */
      int dummy;          /* Dummy argument */
      int ok;             /* Expression ok? */
      int test_done;      /* Have any comparisons been performed? */
      int match_found;    /* Have any matches been found? */

/* Initialise */
      ok = 1;

/* Check the inherited status. */
      if( *status != SAI__OK ) return ok;

/* Report an error if the term is null */
      if( t1 == t0 ) {
         *status = SAI__ERROR;
         errRep( " ", "Null character term given.",
                 status );
         return ok;
      }

/* Note the location of the first character to be added to the returned
   string. */
      v0 = *v;

/* The first component in the term gives a value which may be modified by
   later words in the term. Store the corresponding string at v. */
      d = pol1Seval( t0, t1, fchan, 1, grp1, grp2, v, vl, &ok, status );
      if( *status != SAI__OK ) return ok;

/* The character following the first component must be a delimiter or the
   end of the term. */
      if( d < t1 && *d != DELIM ) {
         *status = SAI__ERROR;
         errRep( " ", "Invalid character expression.", status );
         return ok;
      }

/* If its a delimiter, we need to look for any replacement specifications
   following the first component. */
      match_found = 0;
      test_done = 0;
      while( d < t1 ){

/* Find the next component value. Do not allow references to FITS
   keywords here. */
         lhs0 = lhs;
         lhs1 = lhs;
         e = pol1Seval( d, t1, fchan, 0, grp1, grp2, &lhs1,
                        lhs0 + MXLIT - 1, &dummy, status );
         if( *status != SAI__OK ) return ok;

/* The next character must be an equals sign. */
         if( *e != EQUALS ) {
            *status = SAI__ERROR;
            errRep( " ", "Possible missing equals sign.", status );
            return ok;
         }

/* The next character must not be a delimter. */
         e++;
         if( *e == DELIM ) {
            *status = SAI__ERROR;
            errRep( " ", "Extra spaces after an equal sign.", status );
            return ok;
         }

/* Find the component to the right of the equals sign. Do not allow
   references to FITS keywords here. */
         rhs0 = rhs;
         rhs1 = rhs;
         f = pol1Seval( e, t1, fchan, 0, grp1, grp2, &rhs1, rhs0 + MXLIT - 1,
                        &dummy, status );
         if( *status != SAI__OK ) {
            *status = SAI__ERROR;
            errRep( " ", "Possible missing delimiter.", status );
            return ok;
         }

/* Compare the string to the left of the equals with the original value.
   If they match, store the right hand string and leave the loop. */
         test_done = 1;
         if( pol1Cmpr( *v, v0, lhs1, lhs0 ) ) {
            *v = v0;
            for( ; rhs0 < rhs1 && *v < vl; rhs0++ ) *((*v)++) = *rhs0;
            match_found = 1;
            break;
         }

/* Arive here only if the current replacement spec does not natch the
   value. Move on to the next replacement spec. */
         d = f;

      }

/* Report an error if none of the suppleid replacement values was
   matched. */
      if( test_done && !match_found && *status == SAI__OK ){
         *status = SAI__ERROR;
         *(*v) = 0;
         msgSetc( "VAL", v0 );
         errRep( " ", "Keyword value (\'^VAL\') does not match any "
                 "of the supplied test strings.", status );
      }

      return ok;

}


unsigned char *pol1Seval( unsigned char *t0, unsigned char *t1,
                          AstFitsChan *fchan, int fitsok, Grp *grp1,
                          Grp *grp2, char **v, char *vl, int *ok,
                          int *status ){
/*
*  Name:
*     pol1Seval

*  Purpose:
*     Evaluate the first component of the supplied string.

*  Language:
*     Starlink C

*  Synopsis:
*     unsigned char *polSeval( unsigned char *t0, unsigned char *t1,
*                              AstFitsChan *fchan, int fitsok, Grp *grp1,
*                              Grp *grp2, char **v, char *vl, int *ok,
*                              int *status )

*  Description:
*     This routine returns the string corresponding to the first
*     component of the suppleid string:
*
*     - If the first non-delimiter is a quote, the string betwen the first
*     opening and closing quote is stored at *v.
*
*     - If the first non-delimiter is an opening parenthesis, the string
*     between the first opening and closing parentheses is evaluated as
*     an expression and stored at *v.
*
*     - Otherwise, the first component ends at the first control token.
*     If fitsok is non-zero, this is taken to be the name of a FITS
*     keyword, and the value of the keyword is stored at v (an error is
*     reported and STATUS is set to SAI__WARN if the keyword is not defined
*     in the FitsChan). Otherwise, the component is stored unchanged at *v.

*  Arguments:
*     t0
*        Pointer to the first character
*     t1
*        Pointer to the first character beyond the end
*     fchan
*        A pointer to an AST FitsChan containing the FITS header cards to
*        be used when resolving references to FITS keywords contained in
*        the expression.
*     fitsok
*        If non-zero, then the first component can be a reference to a
*        FITS keyword, in which case the stored string is the keyword value.
*     grp1
*        Pointer to a GRP group holding HDS data types. Ignored if either
*        grp1 or grp2 is NULL.
*     grp1
*        Pointer to a GRP group holding FITS keyword names. Ignored if either
*        grp1 or grp2 is NULL.
*     v
*        Address of a pointer to the start of the string to receive the string
*        corresponding to the expression.
*     vl
*        Pointer to the end of the string to receive the string
*        corresponding to the expression. The string should not be null
*        terminated.
*     ok
*        Returned equal to 1 if the first component is a quoted string,
*        or if it is a literal string containing only characters which are
*        valid in the context of a FITS keyword name.
*     status
*        The global status.

*  Returned Value:
*        Pointer to the first character following the first component.

*  Copyright:
*     Copyright (C) 1998 Central Laboratory of the Research Councils

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     26-APR-1999 (DSB):
*        Original version.
*     8-AUG-2006 (DSB):
*        Change to use GRP C interface.
*        Fix bug that caused mis-interpretation of quoted strings.
*     15-JUL-2008 (TIMJ):
*        Tweak to GRP C API.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*/

/* Local Variables: */
      char *c;            /* Pointer to next signed character */
      char *comment;      /* Pointer to start of FITS keyword comment */
      char *name;         /* Pointer to start of FITS keyword name */
      char *text;         /* Pointer to declared data type text string */
      char *value;        /* Pointer to start of FITS keyword value */
      char buffer[256];   /* Buffer for GRP element */
      char card[81];      /* Entire FITS header card for found keyword */
      char fname[20];     /* Null-terminated FITS keyword name string */
      int dtype;          /* Declared data type */
      int idec;           /* Index of FITS name within GRP2 */
      int type;           /* AST data type */
      unsigned char *d;   /* Pointer to next delimiter */
      unsigned char *e;   /* Pointer to next unsigned character*/
      unsigned char *ret; /* Returned pointer */

/* Initialise */
      *ok = 1;
      ret = t1;

/* Check the inherited status. */
      if( *status != SAI__OK ) return ret;

/* Find the first non-delimiter in the supplied string. */
      d = t0;
      while( *d == DELIM ) d++;

/* Check there is something left. */
      if( d >= t1 ) return ret;

/* If the first character is an opening parenthesis, then find the
   corresponding closing parenthesis. */
      if( *d == OPEN_P ) {
         e = pol1Fchr( d + 1, t1, CLOSE_P, status );

/* Report an error if there is no corresponding closing parenthesis. */
         if( e == t1 ) {
            *status = SAI__ERROR;
            errRep( " ", "Missing closing parenthesis ')'.",
                    status );
            return ret;
         }

/*  Process the string within the parentheses as a new expression. */
         *ok = pol1Ceval( d + 1, e, fchan, grp1, grp2, v, vl, status );
         if( *status == SAI__OK ) ret = e + 1;

/* If the first character is a quoted string. */
      } else if( *d == OPEN_Q ) {
         e = pol1Fchr( d + 1, t1, CLOSE_Q, status );

/* Report an error if there is no corresponding closing quote. */
         if( e == t1 ) {
            *status = SAI__ERROR;
            errRep( " ", "Missing closing quotes.",
                    status );
            return ret;
         }

/* Store the text between the quotes in the returned string. */
         d++;
         for( ; d < e && *v < vl ; d++ ) *((*v)++) = (char) *d;
         ret = e + 1;

/* If the first word is not a parenthesised expression, or a quoted
   string, the first component ends at the first token. */
      } else {
         ret = d;
         while( ret < t1 && *ret < 200 ) ret++;

/* If allowed, assume it is a FITS keyword name. Get the value. */
         if( fitsok ) {

/* We now need to check that the string looks like a FITS keyword. We
   assume it is a keyword name if it contains only characters which are
   legal within FITS keyword names, and if it starts with an alphabetical
   character. Check the first character. */
            if( !isalpha( (int) *d ) ) {
               *ok = 0;
               *status = SAI__ERROR;
               errRep( " ", "Syntax error in supplied expression.",
                       status );
               return ret;
            }

/* Copy the FITS keyword name into a null teminated string, checking each
   character to see if it is allowed within a FITS keyword name. */
            name = fname;
            for( ;  d < ret; d++ ) {
               if( isFits( (int) *d ) ) {
                  *(name++) = (char) *d;
               } else {
                  *ok = 0;
                  *status = SAI__ERROR;
                  errRep( " ", "Syntax error in supplied expression.",
                          status );
                  return ret;
               }
            }
            *name = 0;

/* Search the entire FitsChan for this keyword. If found, extract the
   keyword name, value and comment, and get its natural data type. */
            astClear( fchan, "card" );
            if( astFindFits( fchan, fname, card, 0 ) ){
               type = pol1Split( card, &name, &value, &comment, status );

/* Remove any leading spaces from its value if its natural type is not
   AST__STRING */
               if( type != AST__STRING ) {
                  text = value;
                  c = NULL;
                  while( *text ){
                    if( c || *text != ' ' ) {
                       if( !c ) c = value;
                       *(c++) = *text;
                    }
                    text++;
                  }
                  *(c--) = 0;

/* If the natural data type is AST__STRING, get a pointer to the last
   character before the terminating null. */
               } else {
                  c = value + strlen( value ) - 1;
               }

/* Remove any trailing spaces. */
               while( c >= value && *c == ' ' ) *(c--) = 0;

/* See if it is explicitly declared in the control table. The declared
   type over-rides the natural type. */
               dtype = AST__NOTYPE;
               if( grp1 != NULL && grp2 != NULL ) {
                  idec = grpIndex( name, grp2, 1, status );
                  if( idec > 0 ) {

/* Get it using grpInfoc, which converts the name to upper case. */
                     grpInfoc( grp1, idec, "NAME", buffer, 256, status );
                     text = ( *status == SAI__OK ) ? buffer : NULL;

                     if( !pol1Ustrcmp( text, "_DOUBLE") ||
                         !pol1Ustrcmp( text, "_REAL") ){
                        dtype = AST__FLOAT;

                     } else if( !pol1Ustrcmp( text, "_INTEGER") ||
                                !pol1Ustrcmp( text, "_WORD") ||
                                !pol1Ustrcmp( text, "_BYTE") ){
                        dtype = AST__INT;

                     } else if( !pol1Ustrcmp( text, "_CHAR") ){
                        dtype = AST__STRING;

                     } else if( *status == SAI__OK ) {
                        *status = SAI__ERROR;
                        msgSetc( "TYPE", text );
                        errRep( " ", "Unsupported or unrecognised data type \"^TYPE\".",
                                status );

                        free( (void *) name );
                        free( (void *) value );
                        free( (void *) comment );

                        return ret;
                     }
                  }
               }

/* If it was declared, use its declared type instead of its natural type. */
               if( dtype != AST__NOTYPE ) type = dtype;

/* Return the character equivalent of the keyword value, so long as the
   keyword was not explicitly declared as non-character, in which
   case report an error. */
               if( dtype != AST__NOTYPE && dtype != AST__STRING &&
                   *status == SAI__OK ){

                  *status = SAI__ERROR;
                  msgSetc( "name", name );
                  msgSetc( "type", text );
                  errRep( " ", "FITS keyword ^NAME was declared to be "
                          "of type ^TYPE but must be _CHAR.", status );

                  free( (void *) name );
                  free( (void *) value );
                  free( (void *) comment );

                  return ret;
               }

               *ok = 1;

/* If the keyword was not found, report an error. */
            } else {
               if( *status == SAI__OK ) {
                  *status = SAI__WARN;
                  msgSetc( "FNAME", fname );
                  errRep( " ", "No value available for FITS keyword \"^FNAME\".",
                          status );
               }
               return ret;
            }

/* Copy the keyword value to the returned string. */
            if( *ok ) {
               c = value;
               while( *c && *v < vl ) *((*v)++) = *(c++);
            }

/* Release the memory storing the strings returned by pol1Split. */
            free( (void *) name );
            free( (void *) value );
            free( (void *) comment );

/* If it cannot be a FITS keyword, return the first component as a literal
   string. */
         } else {
            for( ; d < ret && *v < vl; d++ ) *((*v)++) = *d;
            *ok = 0;
         }
      }

      return ret;

}

F77_SUBROUTINE(pol1_gtfit)( INTEGER(FCHAN), CHARACTER(NAME), CHARACTER(VALUE),
                            CHARACTER(TYPE), LOGICAL(THERE), INTEGER(STATUS)
                            TRAIL(NAME) TRAIL(VALUE) TRAIL(TYPE) ){
/*
*  Name:
*     POL1_GTFIT

*  Purpose:
*     Gets a FITS keyword value from a FitsChan.

*  Language:
*     Starlink C (callable from Fortran 77)

*  Invocation:
*     CALL POL1_GTFIT( FCHAN, NAME, VALUE, TYPE, THERE, STATUS )

*  Description:
*     This routine searches for a given FITS keyword in a FitsCHan, and
*     returns its formatted value, and its natural data type (i.e. the
*     data type implied by its formatting in the original FITS header card).

*  Arguments:
*     FCHAN = INTEGER (Given)
*        The FitsChan.
*     NAME = CHARACTER * ( * ) (Given)
*        The FITS keyword name.
*     VALUE = CHARACTER * ( * ) (Returned)
*        The formatted keyword value. Set blank if the keyword was not found.
*     TYPE = CHARACTER * ( * ) (Returned)
*        The natural data type of the keyword. "_REAL", "_DOUBLE", etc.
*     THERE = LOGICAL (Returned)
*        Was the keyword found? No error is reported if it was not found.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1998 Central Laboratory of the Research Councils

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     23-APR-1999 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*/

/* Argyments: */
      GENPTR_INTEGER(FCHAN)
      GENPTR_CHARACTER(NAME)
      GENPTR_CHARACTER(VALUE)
      GENPTR_CHARACTER(TYPE)
      GENPTR_LOGICAL(THERE)
      GENPTR_INTEGER(STATUS)

/* Local Variables: */
      AstFitsChan *fchan;
      char *a;
      char *comment;
      char *fname;
      char *name;
      char *value;
      char card[ FITSCARDLEN + 1 ];
      int i;
      int there;
      int type;

/* Initialise */
      for( i = 0; i < VALUE_length; i++ ) VALUE[ i ] = ' ';
      for( i = 0; i < TYPE_length; i++ ) TYPE[ i ] = ' ';
      *THERE = F77_FALSE;

/* Check the STATUS value. */
      if( *STATUS != SAI__OK ) return;

/* Create a null terminated copy of the keyword name. */
      fname = (char *) malloc( ( NAME_length + 1 )*sizeof( char ) );
      if( !fname ){
         *STATUS = SAI__ERROR;
         errRep( " ", "Cannot allocate memory in POL1_GTFIT.", STATUS );
         return;
      }
      a = fname;
      for( i = 0; i < NAME_length; i++ ) *(a++) = NAME[ i ];
      *a = 0;

/* Remove any trailing spaces. */
      a--;
      while( a >= fname && *a == ' ' ) *(a--) = 0;

/* Get a C pointer to the FitsChan. */
      fchan = astI2P( *FCHAN );

/* Rewind the FitsChan so that its entire contents are searched. */
      astClear( fchan, "card" );

/* Search the FitsChan for the keyword. */
      there = astFindFits( fchan, fname, card, 0 );

/* If found, extract the keyword name, value and comment, and get its
   natural data type. */
      if( there ){
         *THERE = F77_TRUE;
         type = pol1Split( card, &name, &value, &comment, STATUS );

/* Get a pointer to the first character to be copied to the returned
   value string. If the natural type of the keyword is not AST__STRING,
   ignore leading spaces. */
         a = value;
         if( type != AST__STRING ) {
            while( *a == ' ' ) a++;
         }

/* Copy the value to the returned string. */
         i = 0;
         while( *a && i < VALUE_length ) VALUE[ i++ ] = *(a++);

/* Return the appropriate HDS data type string. */
         if( type == AST__INT ){
            strncpy( TYPE, "_INTEGER", 8 );

         } else if( type == AST__FLOAT ){
            strncpy( TYPE, "_DOUBLE", 7 );

         } else if( type == AST__STRING ){
            strncpy( TYPE, "_CHAR", 5 );

         } else if( type == AST__LOGICAL ){
            strncpy( TYPE, "_LOGICAL", 8 );

/* Ignore keywords which have unusable data types (complex, comments, etc). */
         } else {
            for( i = 0; i < VALUE_length; i++ ) VALUE[ i ] = ' ';
            for( i = 0; i < TYPE_length; i++ ) TYPE[ i ] = ' ';
            *THERE = F77_FALSE;
         }

/* Free memory */
         free( name );
         free( value );
         free( comment );

      }

/* Free memory */
      free( fname );

      return;
}

int pol1Cmpr( char *a1, char *a0, char *b1, char *b0 ){
/*
*  Name:
*     pol1Cmpr

*  Purpose:
*     Compare two strings for equality.

*  Description:
*     This function compares two strings for equality. If both strings
*     are numerical they care converted to double precision before doing
*     the comparison so that for instance the strings "45.0", "45", "45.0E0",
*     "45.0D0", "45.", etc, will all be considered equal. If either string
*     is not numerical the strings must have the same length and must be
*     identical, except for case in order to match.

*  Parameters:
*     a1
*        A pointer to the first character beyond the end of the A string.
*     a0
*        A pointer to the first character of the A string.
*     b1
*        A pointer to the first character beyond the end of the B string.
*     b0
*        A pointer to the first character of the B string.

*  Returned Value:
*     1 if the A and B strings are equal, and zero otherwise.
*/
      int istat;               /* Local status */
      int ret;                 /* The returned flag. */
      int a0_length;           /* Length of A string */
      int b0_length;           /* Length of B string */
      char * acopy = NULL;     /* Local copy of A */
      char * bcopy = NULL;     /* Local copy of B */
      double dvala;            /* Double value from A string */
      double dvalb;            /* Double value from B string */

/* Initialise. */
      ret = 0;
      istat = SAI__OK;

      errMark();

/* Save the lengths of the two strings. */
      a0_length = a1 - a0;
      b0_length = b1 - b0;

/* Need proper C strings - copy specific number of characters out */
      acopy = malloc( a0_length + 1 );
      strncpy( acopy, a0, a0_length );
      acopy[a0_length] = '\0';

/* See if the first string is numerical. */
      dvala = one_strtod( acopy, &istat );

/* If so see if the second string is also numerical. */
      if( istat == SAI__OK ) {
         bcopy = malloc( b0_length + 1 );
         strncpy( bcopy, b0, b0_length );
         bcopy[b0_length] = '\0';
         dvalb = one_strtod( bcopy, &istat );
         if (bcopy) free(bcopy);

/* If so, compare the numerical values. */
         if( istat == SAI__OK ) ret = ( dvala == dvalb );
      }
      if (acopy) free(acopy);

/* If either of the two strings was not numerical, we compare the strings
   as text strings. */
      if( istat != SAI__OK ) {

/* Check that the strings are the same length. */
         if( a0_length == b0_length ) {

/* Check if the strings are equal apart from case. */
            ret = !pol1Ustrncmp( a0, b0, b0_length );

         }
      }

      if (istat != SAI__OK) errAnnul(&istat);
      errRlse();

/* Return the answer. */
      return ret;
}
