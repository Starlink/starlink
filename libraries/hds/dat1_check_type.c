#if HAVE_CONFIG_H
#  include <config.h>
#endif

#include <ctype.h>
#include <stdio.h>
#include "ems.h"                 /* EMS error reporting routines            */
#include "hds1.h"                /* Global definitions for HDS              */
#include "rec.h"                 /* Public rec_ definitions                 */
#include "dat1.h"                /* Internal dat_ definitions               */
#include "dat_err.h"             /* DAT__ error code definitions            */

   int dat1_check_type( const struct DSC *type, char ptype[ DAT__SZTYP ] )
   {
/*+                                                                         */
/* Name:                                                                    */
/*    dat1_check_type                                                       */

/* Purpose:                                                                 */
/*    Check and pack a user-supplied type specification.                    */

/* Invocation:                                                              */
/*    dat1_check_type( type, ptype )                                        */

/* Description:                                                             */
/*    This function checks the information in a user-supplied HDS data type */
/*    string for validity. If it is a valid structure type, then an         */
/*    upper-case, left justified version is returned with white space       */
/*    removed. If it is a valid primitive type, then a packed description   */
/*    of the host machine's native representation of that type is returned  */
/*    (which can be unpacked into a PDD structure by dat1_unpack_type).  If */
/*    the type specification is not valid, then an error results.           */

/* Parameters:                                                              */
/*    const struct DSC *type                                                */
/*       Pointer to a DSC descriptor for the user-supplied character string */
/*       containing the type specification.                                 */
/*    char ptype[ DAT__SZTYP ]                                              */
/*       Array of DAT__SZTYP characters in which the packed type            */
/*       information will be returned.                                      */

/* Returned Value:                                                          */
/*    int dat1_check_type                                                   */
/*       The global status value current on exit.                           */

/* Copyright:                                                               */
/*    Copyright (C) 1992 Science & Engineering Research Council             */

/* Authors:                                                                 */
/*    RFWS: R.F. Warren-Smith (STARLINK)                                    */
/*    TIMJ: Tim Jenness (JAC, Hawaii)                                       */
/*    {@enter_new_authors_here@}                                            */

/* History:                                                                 */
/*    17-JUL-1991 (RFWS):                                                   */
/*       Original version.                                                  */
/*    10-SEP-1992 (RFWS):                                                   */
/*       Added writing of format string for use by sscanf.                  */
/*    14-SEP-2005 (TIMJ):                                                   */
/*       Initialise ptype on entry to stop it accessing uninitialised       */
/*       memory.                                                            */
/*    20-APR-2012 (TIMJ):                                                   */
/*       Add _INT64 support.                                                */
/*    {@enter_further_changes_here@}                                        */

/* Bugs:                                                                    */
/*    {@note_any_bugs_here@}                                                */

/*-                                                                         */

/* Local Variables:                                                         */
      char *txt;                 /* Char pointer to input string            */
      char fmt[ 31 ];            /* Format string for sccanf                */
      int clen;                  /* Length of character data type           */
      int i;                     /* Loop counter for input characters       */
      int length;                /* Data element length in chars            */
      int lenok;                 /* Character length valid?                 */
      int n;                     /* Counter for output characters           */
      unsigned char *utype;      /* Unsigned char pointer to output buffer  */
      unsigned char dtype = 0;   /* Primitive data type code                */

/*.                                                                         */

/* Check the inherited global status.                                       */
      if ( !_ok( hds_gl_status ) ) return hds_gl_status;

/* Initialise typbuf to prevent valgrind warnings */
      memset( ptype, 0, DAT__SZTYP);

/* Obtain a char pointer to the input string and scan through it checking   */
/* the syntax and converting lower case characters to upper case.           */
      txt = (char *) type->body;
      n = 0;
      for ( i = 0; i < (int) type->length; i++ )
      {

/* Ignore white space.                                                      */
         if ( !isspace( txt[ i ] ) )
         {

/* Report an error if the maximum type string length is exceeded.           */
            if ( n >= DAT__SZTYP )
            {
               hds_gl_status = DAT__TYPIN;
               emsSetnc( "TYPE", txt, type->length );
               emsSeti( "SZTYP", DAT__SZTYP );
               emsRep( "DAU_CHECK_TYPE_1",
                          "Invalid type string \'^TYPE\' specified; more than \
^SZTYP characters long (possible programming error).",
                          &hds_gl_status );
               break;
            }

/* Report an error if a non-printable character is encountered.             */
            else if ( !isprint( txt[ i ] ) )
            {
               hds_gl_status = DAT__TYPIN;
               emsSetnc( "TYPE", txt, type->length );
               emsSeti( "CODE", (int) txt[ i ] );
               emsSeti( "POSN", i + 1 );
               emsRep( "DAU_CHECK_TYPE_2",
                          "Invalid type string \'^TYPE\' specified; contains \
illegal character (code=^CODE decimal) at position ^POSN (possible \
programming error).",
                          &hds_gl_status );
               break;
            }

/* Report an error if a '*' is encountered and the type is not primitive    */
/* (i.e. the type specification does not start with an underscore).         */
            else if ( ( txt[ i ] == '*' ) &&
                      ( ( n == 0 ) || ( ptype[ 0 ] != '_' ) ) )
            {
               hds_gl_status = DAT__TYPIN;
               emsSetnc( "TYPE", txt, type->length );
               emsRep( "DAU_CHECK_TYPE_3",
                          "Invalid type string \'^TYPE\' specified; the \'*\' \
character is not permitted in user-defined HDS types (possible programming \
error).",
                          &hds_gl_status );
               break;
            }

/* Otherwise, accumulate characters in the output buffer, converting them   */
/* to upper case.                                                           */
            else
            {
               ptype[ n++ ] = toupper( txt[ i ] );
            }
         }
      }

/* If a valid structure type was specified (i.e. no leading underscore),    */
/* then space-fill the end of the output buffer.                            */
      if ( _ok( hds_gl_status ) )
      {
         if ( ( n == 0 ) || ( ptype[ 0 ] != '_' ) )
         {
            for ( ; n < DAT__SZTYP; n++ )
            {
               ptype[ n ] = ' ';
            }
         }

/* If the type is primitive (i.e. has a leading underscore), then check for */
/* each of the recognised primitive types in turn and obtain the            */
/* appropriate data type code.                                              */
         else
         {

/* _DOUBLE                                                                  */
            if ( ( n == 7 ) &&
                 ( strncmp( ptype, "_DOUBLE", 7 ) == 0 ) )
            {
               dtype = DAT__D;
            }

/* _REAL                                                                    */
            else if ( ( n == 5 ) &&
                      ( strncmp( ptype, "_REAL", 5 ) == 0 ) )
            {
               dtype = DAT__R;
            }

/* _INTEGER                                                                 */
            else if ( ( n == 8 ) &&
                      ( strncmp( ptype, "_INTEGER", 8 ) == 0 ) )
            {
               dtype = DAT__I;
            }

/* _WORD                                                                    */
            else if ( ( n == 5 ) &&
                      ( strncmp( ptype, "_WORD", 5 ) == 0 ) )
            {
               dtype = DAT__W;
            }

/* _UWORD                                                                   */
            else if ( ( n == 6 ) &&
                      ( strncmp( ptype, "_UWORD", 6 ) == 0 ) )
            {
               dtype = DAT__UW;
            }

/* _BYTE                                                                    */
            else if ( ( n == 5 ) &&
                      ( strncmp( ptype, "_BYTE", 5 ) == 0 ) )
            {
               dtype = DAT__B;
            }

/* _UBYTE                                                                   */
            else if ( ( n == 6 ) &&
                      ( strncmp( ptype, "_UBYTE", 6 ) == 0 ) )
            {
               dtype = DAT__UB;
            }

/* _LOGICAL                                                                 */
            else if ( ( n == 8 ) &&
                      ( strncmp( ptype, "_LOGICAL", 8 ) == 0 ) )
            {
               dtype = DAT__L;
            }

/* _INT64                                                                   */
            else if ( ( n == 6 ) &&
                      ( strncmp( ptype, "_INT64", 6 ) == 0 ) )
            {
               dtype = DAT__K;
            }

/* _CHAR                                                                    */
            else if ( ( n >= 5 ) &&
                      ( strncmp( ptype, "_CHAR", 5 ) == 0 ) )
            {
               dtype = DAT__C;

/* If a character type specification contains no length expression, then    */
/* its length defaults to 1.                                                */
               if ( n == 5 )
               {
                  clen = 1;
               }

/* If it is followed by anything except '*', then report an error.          */
               else if ( ptype[ 5 ] != '*' )
               {
                  hds_gl_status = DAT__TYPIN;
                  emsSetnc( "TYPE", txt, type->length );
                  emsRep( "DAT1_CHECK_TYPE_5",
                             "Invalid length encountered in the character \
type specification \'^TYPE\'; should be \'_CHAR*n\' (possible programming \
error).",
                             &hds_gl_status );
               }

/* If a character-type specification is followed by a length (i.e. number   */
/* of characters specification starting with a '*'), then write a format    */
/* string (to specify the field width), read this length and check its      */
/* validity.                                                                */
               else
               {
                  (void) sprintf( fmt, "%%%dd", n - 6 );
                  lenok = ( sscanf( ptype + 6, fmt, &clen ) == 1 );
                  if ( lenok ) lenok = ( ( clen >= 1 ) &&
                                         ( clen <= DAT__MXCHR ) );
                  if ( !lenok )
                  {
                     hds_gl_status = DAT__TYPIN;
                     emsSetnc( "TYPE", txt, type->length );
                     emsSeti( "MXCHR", DAT__MXCHR );
                     emsRep( "DAT1_CHECK_TYPE_5",
                                "Invalid length encountered in the character \
type specification \'^TYPE\'; should be in the range 1 to ^MXCHR (possible \
programming error).",
                                &hds_gl_status );
                  }
               }
            }

/* If the candidate primitive type specification was not recognised, then   */
/* report an error.                                                         */
            else
            {
               hds_gl_status = DAT__TYPIN;
               emsSetnc( "TYPE", txt, type->length );
               emsRep( "DAT1_CHECK_TYPE_6",
                          "Invalid primitive data type \'^TYPE\' specified \
(possible programming error).",
                          &hds_gl_status );
            }

/* If OK, then pack the description of the host machine's representation of */
/* the primitive type into the output buffer.  A packed primitive type      */
/* description is indicated by two leading underscores.                     */
            if ( _ok( hds_gl_status ) )
            {
               ptype[ 0 ] = '_';
               ptype[ 1 ] = '_';

/* The remainder consists of unsigned chars...                              */
               utype = (unsigned char *) ptype;

/* The first two of these encode the data element length (either the        */
/* explicitly specified number of characters times the character size or    */
/* the intrinsic host machine length of the data type for other types).     */
               length = dat_gl_ndr[ dtype ].length * ( ( dtype == DAT__C ) ?
                                                         clen : 1 );
               utype[ 2 ] = length & 0xff;
               utype[ 3 ] = ( length >> 8 ) & 0xff;

/* The next 3 chars hold codes for the primitive data type, the number      */
/* format and the (byte) storage order.                                     */
               utype[ 4 ] = dtype;
               utype[ 5 ] = dat_gl_ndr[ dtype ].format;
               utype[ 6 ] = dat_gl_ndr[ dtype ].order;

/* Remaining chars are not used, so fill them with zeros.                   */
               for ( n = 7; n < DAT__SZTYP; n++ )
               {
                  utype[ n ] = 0;
               }
            }
         }
      }

/* Return the current global status value.                                  */
      return hds_gl_status;
   }
