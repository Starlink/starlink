#include <string.h>
#include "hlpsys.h"
void hlpHchkl ( char *record, int *level, char *name )
/*
**  - - - - - - - - -
**   h l p H c h k l
**  - - - - - - - - -
**
**  Check if the line held in record is the start of a new help level
**  and if so get the level number and the keyword.
**
**  Given (in global data):
**     levoff    int      logical level for the current HELP library
**
**  Given (argument):
**     *record   char     the string to be examined
**
**  Returned:
**     *level    int      level number (-1 if not a keyword record)
**     *name     char     level name (only if level >= 0)
**
**  The line in record is provisionally assumed to be the start of a new
**  HELP level if the first space-separated field begins at the start of
**  the line and consists wholly of decimal digits.
**
**  Called:  hlpSplit, hlpDec
**
**  Last revision:   27 August 1995
**
**  Copyright 1996 P.T.Wallace.  All rights reserved.
*/
{
   int ips, ipf, l, i;

/* Preset level */
   *level = -1;

/* Locate the first field in record */
   hlpSplit ( record, 0, &ips, &ipf );

/* Does it start at the beginning of the line? */
   if ( ips == 0) {

   /* Yes: attempt to decode a decimal integer. */
      l = (int) hlpDec ( record, &ips );

   /* Was it entirely decimal digits? */
      i = ipf + 1;
      if ( ips == i ) {

      /* Yes: return the logical level number. */
         *level = l + levoff;

      /* Obtain the level name if any. */
         hlpSplit ( record, i, &ips, &ipf );
         if ( ips > 0 ) {
            for ( i = 0; i <= ipf - ips; i++ ) name [ i ] = record [ i + ips ];
            name [i] = '\0';
         }
      }
   }
}
