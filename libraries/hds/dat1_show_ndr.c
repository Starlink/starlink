#if HAVE_CONFIG_H
#  include <config.h>
#endif

#include <stdio.h>

#include "hds1.h"                /* Global definitions for HDS              */
#include "rec1.h"                /* Internal rec_ definitions               */
#include "dat1.h"                /* Internal dat_ definitions               */
#include "dat_err.h"             /* DAT__ error code definitions            */

   void dat1_show_ndr( int *status )
   {
/*+                                                                         */
/* Name:                                                                    */
/*    dat1_show_ndr                                                         */

/* Purpose:                                                                 */
/*    Display native data representation information for the host machine.  */

/* Invocation:                                                              */
/*    dat1_show_ndr( status )                                               */

/* Description:                                                             */
/*    This function formats and displays the contents of the global native  */
/*    data representation structure which holds information about the way   */
/*    primitive values are stored on the current host machine. The results  */
/*    are written to the standard output. This routine is provided as an    */
/*    implementation aid only; the output format is liable to change        */
/*    without notice.                                                       */

/* Parameters:                                                              */
/*    int *status                                                           */
/*       The inherited global status.                                       */

/* Returned Value:                                                          */
/*    void                                                                  */

/* Notes:                                                                   */
/*    This routine makes much use of macros because it must apply similar   */
/*    processing to a range of arithmetic data types whose characteristics  */
/*    are not known directly (typically they are themselves parameterised   */
/*    via the HDS primitive type macros). The macros used here appear, and  */
/*    are commented, as if they formed part of the normal executable code.  */

/* Copyright:                                                               */
/*    Copyright (C) 1992 Science & Engineering Research Council             */

/* Authors:                                                                 */
/*    RFWS: R.F. Warren-Smith (STARLINK)                                    */
/*    TIMJ: Tim Jenness (JAC, Hawaii)                                       */
/*    {@enter_new_authors_here@}                                            */

/* History:                                                                 */
/*    4-SEP-1992 (RFWS):                                                    */
/*       Original version.                                                  */
/*    5-OCT-1992 (RFWS):                                                    */
/*       Changed to display and use decimal digits of precision value.      */
/*    20-APR-2012 (TIMJ):                                                   */
/*       Add _INT64 support.                                                */
/*    {@enter_further_changes_here@}                                        */

/* Bugs:                                                                    */
/*    {@note_any_bugs_here@}                                                */

/*-                                                                         */

/* Local Variables:                                                         */
      char hexbuf[ 101 ];        /* Buffer for formatting hex values        */
      const char *txt;           /* Pointer to text for output              */
      int i;                     /* Loop counter for data types             */

/*.                                                                         */

/* Check the inherited global status.                                       */
      if ( !_ok( *status ) ) return;

/* Define macros.                                                           */
/* =============                                                            */
/* Note that these macros execute in the context of the main display loop   */
/* (below). Any variables not declared within the macro itself take their   */
/* values from variables used within that loop.                             */

/* Define a macro to determine if an integer data type is signed or not,    */
/* returning 0 or 1.                                                        */
#define _signed( dtype ) ( ( (dtype) -1 ) < ( (dtype) 0 ) )

/* Define a macro to format a value (given by the value argument) in        */
/* hexadecimal format, where the bytes are stored in a specified order      */
/* (given by the order argument with value DAT__LSB or DAT__MSB). The       */
/* null-terminated result string is written to the char buffer hexbuf.      */
#define _hexfmt( value, order )\
      {\
         int j;                  /* Byte counter for formatting hex values  */\
         int k;                  /* Character count for hex values          */\
         unsigned char *ptr;     /* Pointer to byte sequence for formatting */\
\
/* Obtain a pointer to the byte sequence to be formatted. Loop through      */\
/* these bytes looking at the most significant first.                       */\
         ptr = (unsigned char *) &(value);\
         if ( (order) == DAT__MSB )\
         {\
            for ( j = 0, k = 0; j < sizeof( value ); j++, k += 2 )\
            {\
\
/* Write the hexadecimal equivalent of each byte into hexbuf (we assume     */\
/* 8-bit bytes here).                                                       */\
               (void) sprintf( hexbuf + k, "%02X", (unsigned int) ptr[ j ] );\
            }\
         }\
\
/* Handle the reverse byte order if necessary.                              */\
         else if ( (order) == DAT__LSB )\
         {\
            for ( j = sizeof( value ) - 1, k = 0; j >= 0; j--, k += 2 )\
            {\
               (void) sprintf( hexbuf + k, "%02X", (unsigned int) ptr[ j ] );\
            }\
         }\
      }

/* Define a macro for displaying values for the _CHAR data type. This is a  */
/* special case; display the bad data value only using character format     */
/* (and hexadecimal).                                                       */
#define _showc( dtype, t )\
      {\
         _hexfmt( dat_gl_ndr[ i ].bad.t, dat_gl_ndr[ i ].order )\
         (void) printf( "      Bad data value: \'%c\' (hex %s)\n",\
                        (int) dat_gl_ndr[ i ].bad.t, hexbuf );\
      }

/* Define a macro for displaying _LOGICAL values. This is also a special    */
/* case; display the bad data value only, but use integer format taking     */
/* account of whether it is a signed data type or not.                      */
#define _showl( dtype, t )\
      {\
         _hexfmt( dat_gl_ndr[ i ].bad.t, dat_gl_ndr[ i ].order )\
\
/* If the data type is signed, then cast to long int for display.           */\
         if ( _signed( dtype ) )\
         {\
            (void) printf( "      Bad data value: %-ld (hex %s)\n",\
                           (long int) dat_gl_ndr[ i ].bad.t, hexbuf );\
         }\
\
/* If it is unsigned, then cast to unsigned long int.                       */\
         else\
         {\
            (void) printf( "      Bad data value: %-lu (hex %s)\n",\
                           (unsigned long int) dat_gl_ndr[ i ].bad.t, hexbuf );\
         }\
      }

/* Define a macro for displaying integer values. Display the minimum,       */
/* maximum and bad data values, taking account of whether the data type is  */
/* signed or not.                                                           */
#define _showi( dtype, t )\
      {\
\
/* Display each value right justified within a field width given by its     */\
/* txtsize value. Follow it by its value in hexadecimal. Cast values to     */\
/* 64-bit int type for display if they are signed.                          */\
         if ( _signed( dtype ) )\
         {\
            _hexfmt( dat_gl_ndr[ i ].min.t, dat_gl_ndr[ i ].order )\
            (void) printf( "      Minimum value:  %*"INT_BIG_S" (hex %s)\n",\
                           (int) dat_gl_ndr[ i ].txtsize,\
                           (INT_BIG) dat_gl_ndr[ i ].min.t, hexbuf );\
            _hexfmt( dat_gl_ndr[ i ].max.t, dat_gl_ndr[ i ].order )\
            (void) printf( "      Maximum value:  %*"INT_BIG_S" (hex %s)\n",\
                           (int) dat_gl_ndr[ i ].txtsize,\
                           (INT_BIG) dat_gl_ndr[ i ].max.t, hexbuf );\
            _hexfmt( dat_gl_ndr[ i ].bad.t, dat_gl_ndr[ i ].order )\
            (void) printf( "      Bad data value: %*"INT_BIG_S" (hex %s)\n",\
                           (int) dat_gl_ndr[ i ].txtsize,\
                           (INT_BIG) dat_gl_ndr[ i ].bad.t, hexbuf );\
         }\
         else\
         {\
\
/* Cast unsigned values to unsigned 64-bit int for display.                 */\
            _hexfmt( dat_gl_ndr[ i ].min.t, dat_gl_ndr[ i ].order )\
            (void) printf( "      Minimum value:  %*"INT_BIG_U" (hex %s)\n",\
                           (int) dat_gl_ndr[ i ].txtsize,\
                           (UINT_BIG) dat_gl_ndr[ i ].min.t,\
                            hexbuf );\
            _hexfmt( dat_gl_ndr[ i ].max.t, dat_gl_ndr[ i ].order )\
            (void) printf( "      Maximum value:  %*"INT_BIG_U" (hex %s)\n",\
                           (int) dat_gl_ndr[ i ].txtsize,\
                           (UINT_BIG) dat_gl_ndr[ i ].max.t,\
                            hexbuf );\
            _hexfmt( dat_gl_ndr[ i ].bad.t, dat_gl_ndr[ i ].order )\
            (void) printf( "      Bad data value: %*"INT_BIG_U" (hex %s)\n",\
                           (int) dat_gl_ndr[ i ].txtsize,\
                           (UINT_BIG) dat_gl_ndr[ i ].bad.t,\
                            hexbuf );\
         }\
      }

/* Define a macro for displaying floating-point values. Display the         */
/* precision and the minimum, maximum and bad data values. There is no      */
/* clear way of deciding how to format the hexadecimal representation here  */
/* so that it will be meaningful (it depends on the byte order), so use the */
/* _INTEGER byte order as the best guess.                                   */
#if ( __STDC__ == 1 ) && defined( _longd )
#define _showf( dtype, t )\
      {\
\
/* Display the precision.                                                   */\
         (void) printf( "      Decimal digits of precision:  %d\n",\
                        (int) dat_gl_ndr[ i ].digits );\
\
/* Cast values to long double for display.                                  */\
         _hexfmt( dat_gl_ndr[ i ].min.t, dat_gl_ndr[ DAT__I ].order )\
         (void) printf( "      Minimum value:  %*.*LE (hex %s)\n",\
                        (int) dat_gl_ndr[ i ].txtsize,\
                        (int) ( dat_gl_ndr[ i ].digits - 1 ),\
                        (long double) dat_gl_ndr[ i ].min.t, hexbuf );\
         _hexfmt( dat_gl_ndr[ i ].max.t, dat_gl_ndr[ DAT__I ].order )\
         (void) printf( "      Maximum value:  %*.*LE (hex %s)\n",\
                        (int) dat_gl_ndr[ i ].txtsize,\
                        (int) ( dat_gl_ndr[ i ].digits - 1 ),\
                        (long double) dat_gl_ndr[ i ].max.t, hexbuf );\
         _hexfmt( dat_gl_ndr[ i ].bad.t, dat_gl_ndr[ DAT__I ].order )\
         (void) printf( "      Bad data value: %*.*LE (hex %s)\n",\
                        (int) dat_gl_ndr[ i ].txtsize,\
                        (int) ( dat_gl_ndr[ i ].digits - 1 ),\
                        (long double) dat_gl_ndr[ i ].bad.t, hexbuf );\
      }
#else
/* Don't use long double if it is not supported by the compiler. Use double */
/* instead.                                                                 */
#define _showf( dtype, t )\
      {\
         (void) printf( "      Decimal digits of precision:  %d\n",\
                        (int) dat_gl_ndr[ i ].digits );\
         _hexfmt( dat_gl_ndr[ i ].min.t, dat_gl_ndr[ DAT__I ].order )\
         (void) printf( "      Minimum value:  %*.*E (hex %s)\n",\
                        (int) dat_gl_ndr[ i ].txtsize,\
                        (int) ( dat_gl_ndr[ i ].digits - 1 ),\
                        (double) dat_gl_ndr[ i ].min.t, hexbuf );\
         _hexfmt( dat_gl_ndr[ i ].max.t, dat_gl_ndr[ DAT__I ].order )\
         (void) printf( "      Maximum value:  %*.*E (hex %s)\n",\
                        (int) dat_gl_ndr[ i ].txtsize,\
                        (int) ( dat_gl_ndr[ i ].digits - 1 ),\
                        (double) dat_gl_ndr[ i ].max.t, hexbuf );\
         _hexfmt( dat_gl_ndr[ i ].bad.t, dat_gl_ndr[ DAT__I ].order )\
         (void) printf( "      Bad data value: %*.*E (hex %s)\n",\
                        (int) dat_gl_ndr[ i ].txtsize,\
                        (int) ( dat_gl_ndr[ i ].digits - 1 ),\
                        (double) dat_gl_ndr[ i ].bad.t, hexbuf );\
      }
#endif

/* Display information.                                                     */
/* ===================                                                      */
/* Loop to display information about each primitive type in turn.           */
      for ( i = 0; i < DAT__MXPRM; i++ )
      {

/* Display a heading with the data type name.                               */
         (void) printf( "\n" );
         (void) printf( "   %s data type:\n", dat_gl_ndr[ i ].name );

/* Show the data element size and the number of characters required for     */
/* formatting.                                                              */
         (void) printf( "      Unformatted size in bytes:    %d\n",
                        (int) dat_gl_ndr[ i ].length );
         (void) printf( "      Formatted size in characters: %d\n",
                        (int) dat_gl_ndr[ i ].txtsize );

/* Invoke the appropriate macro to display value information.               */
         switch ( i )
         {
            case DAT__C:  _showc( _CHAR,    C )  break;
            case DAT__L:  _showl( _LOGICAL, L )  break;
            case DAT__B:  _showi( _BYTE,    B )  break;
            case DAT__UB: _showi( _UBYTE,   UB ) break;
            case DAT__W:  _showi( _WORD,    W )  break;
            case DAT__UW: _showi( _UWORD,   UW ) break;
            case DAT__I:  _showi( _INTEGER, I )  break;
            case DAT__K:  _showi( _INT64, K )  break;
            case DAT__R:  _showf( _REAL,    R )  break;
            case DAT__D:  _showf( _DOUBLE,  D )  break;
         }

/* Obtain a textual representation of the data format code.                 */
         switch ( dat_gl_ndr[ i ].format )
         {
            case DAT__ASCII:
            {
               txt = "ASCII characters (\'ASCII\')";
               break;
            }
            case DAT__BIT0:
            {
               txt = "Bit zero; 0 ==> FALSE, 1 ==> TRUE (\'BIT0\')";
               break;
            }
            case DAT__NZ:
            {
               txt = "Zero ==> FALSE, non-zero ==> TRUE (\'NZ\')";
               break;
            }
            case DAT__BINARY:
            {
               txt = "Binary integer (\'BINARY\')";
               break;
            }
            case DAT__2COMP:
            {
               txt = "2\'s complement integer (\'2COMP\')";
               break;
            }
            case DAT__VAXF:
            {
               txt = "VAX F-floating (\'VAXF\')";
               break;
            }
            case DAT__VAXD:
            {
               txt = "VAX D-floating (\'VAXD\')";
               break;
            }
            case DAT__IEEE_S:
            {
               txt = "IEEE single precision (\'IEEE_S\')";
               break;
            }
            case DAT__IEEE_D:
            {
               txt = "IEEE double precision (\'IEEE_D\')";
               break;
            }
            default:
            {
               txt = "?";
               break;
            }
         }

/* Display the data format.                                                 */
         (void) printf( "      Data format:    %s\n", txt );

/* Obtain a textual representation of the storage order.                    */
         switch ( dat_gl_ndr[ i ].order )
         {
            case DAT__MSB:
            {
               txt = "Most significant byte first (\'MSB\')";
               break;
            }
            case DAT__LSB:
            {
               txt = "Least significant byte first (\'LSB\')";
               break;
            }
            default:
            {
               txt = "?";
               break;
            }
         }

/* Display the storage order.                                               */
         (void) printf( "      Storage order:  %s\n", txt );
      }

/* Add a final blank output line.                                           */
      (void) printf( "\n" );

/* Exit the routine.                                                        */
      return;
   }
