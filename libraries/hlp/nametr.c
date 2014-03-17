#include <string.h>
#include "hlpsys.h"
int hlpNametr ( int kmd, char *strin, int lstrout, char *strout )
/*
**  - - - - - - - - - -
**   h l p N a m e t r
**  - - - - - - - - - -
**
**  Translate a HELP library name into an actual file name for use
**  in by the fopen function.
**
**  This is a SPECIFIC IMPLEMENTATION of a routine supplied by the
**  calling package.  It sandwiches the given name between a prefix
**  and a suffix.  As well as providing the mandatory "translate"
**  function (K=0), this implementation also provides for the
**  setting up and enquiring of the prefix and suffix strings (k=1-4).
**
**  Given:
**     kmd        int     command:  0 = translate
**                                  1 = specify prefix
**                                  2 = specify suffix
**                                  3 = enquire prefix
**                                  4 = enquire suffix
**     *strin     char    input string: for kmd=0, HELP library name
**                                          kmd=1, prefix
**                                          kmd=2, suffix
**                                          kmd=3, not used
**                                          kmd=4, not used
**      lstrout   int     length of output string
**
**  Returned (argument):
**      *strout   char    output string: for kmd=0, filename for fopen
**                                       for kmd=1, not used
**                                       for kmd=2, not used
**                                       for kmd=3, prefix
**                                       for kmd=4, suffix
**  Returned (function value):
**                int     status:   0 = OK
**                hlp_STRING_OVERFLOW = destination string too small
**                hlp_TRANSLATE_ERROR = illegal kmd, or no translation
**                                      possible
**
**  Defined in #include file:
**      error codes
**
**  Notes:
**
**  1)  See the declarations below for the maximum allowed prefix and
**      suffix sizes LPMAX, LSMAX.
**
**  2)  No checks are made for spaces, whether leading, embedded or
**      trailing.  All the strings are null-terminated.
**
**  3)  Other implementations, which exploit environment variables,
**      handle uppercase/lowercase, use lookup tables, and so on,
**      may be developed as the need arises.
**
**  Last revision:   22 February 1996
**
**  Copyright 1996 P.T.Wallace.  All rights reserved.
*/

#define OK 0

/* Maximum lengths of prefix and suffix, including the '\0'. */
#define LPMAX 100
#define LSMAX 20

{
/* Prefix and suffix, and lengths (saved between invocations) */
   static char prefix [ LPMAX ] = "\0", suffix [ LSMAX ] = "\0";
   static int lp = 0, ls = 0;


/*
** Switch according to command value
** ---------------------------------
*/

   switch ( kmd ) {

   case 0:

   /*
   ** kmd=0: Translate library name to filename
   ** -----------------------------------------
   */

   /* Translate the string (if enough room in output string). */
      if ( ( lp + (int) strlen ( strin ) + ls ) >= lstrout )
         return hlp_STRING_OVERFLOW;
      strout = strcat ( strcat ( strcpy ( strout, prefix ),
                                                  strin ),
                                                  suffix );
      return OK;

   case 1:

   /*
   ** kmd=1: Specify prefix
   ** ---------------------
   */

   /* Determine and validate length of prefix. */
      if ( ( lp = (int) strlen ( strin ) ) >= LPMAX)
         return hlp_STRING_OVERFLOW;

   /* Store the prefix. */
      strcpy ( prefix, strin );
      return OK;

   case 2:

   /*
   ** kmd=2: Specify suffix
   ** ---------------------
   */

   /* Determine and validate length of suffix. */
      if ( ( ls = (int) strlen ( strin ) ) >= LSMAX)
         return hlp_STRING_OVERFLOW;

   /* Store the suffix. */
      strcpy ( suffix, strin );
      return OK;

   case 3:

   /*
   ** kmd=3: Enquire prefix
   ** ---------------------
   */

   /* Copy the prefix and report if truncated. */
      strncpy ( strout, prefix, lstrout );
      return ( ( lp <= lstrout ) ? OK : hlp_STRING_OVERFLOW );

   case 4:

   /*
   ** kmd=4: Enquire suffix
   ** ---------------------
   */

   /* Copy the suffix and report if truncated. */
      strncpy ( strout, suffix, lstrout );
      return ( ( ls <= lstrout ) ? OK : hlp_STRING_OVERFLOW );

   default:

   /*
   ** General error
   ** -------------
   */

      return hlp_TRANSLATE_ERROR;
   }
}
