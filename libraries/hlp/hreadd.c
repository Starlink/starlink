#include "hlpsys.h"
int hlpHreadd ( int lstring, char *string, int *nc )
/*
**  - - - - - - - - - -
**   h l p H r e a d d
**  - - - - - - - - - -
**
**  Sequential-access read from the data region of the HELP library.
**
**  Given (global data):
**     nextd     long    address for this sequential access
**
**  Returned (global data):
**     nextd     long    address for next sequential access
**
**  Returned (arguments)
**     lstring   int     length of string
**     *string   char     input record (not including end-of-string)
**     *nc       int      length of record (0 or more)
**
**  Returned (function value):
**               int      status: +1 = OK, but quasi-end-of-file
**                                 0 = OK
**                 hlp_ILLEGAL_STATE = HELP system in wrong state
**                    hlp_READ_ERROR = read error
**                     hlp_READ_WIDE = attempt to read outside file
**               hlp_RECORD_OVERSIZE = record overflows STRING (see note)
**
**  The above error codes are defined in #include file hlpsys.h.
**
**  Notes:
**
**  1)  If the record overflows string, the first lstring-1
**      characters are stored in string, *nc is set to lstring-1
**      and an error status is returned.
**
**  2)  See the source for hlpHdread for side-effects involving
**      locations in global data.
**
**  3)  The condition "quasi-end-of-file" means that a zero-length record
**      has been read.
**
**  4)  See also the routine hlpHreadx, which allows rapid reading of
**      only those records with entries in the index portion of the HELP
**      library.
**
**  Called:  hlpHdread
**
**  Last revision:   13 September 1995
**
**  Copyright 1996 P.T.Wallace.  All rights reserved.
*/
{
   int j;

/* Perform direct-access read. */
   j = hlpHdread ( lstring, &nextd, string, nc );

/* Detect quasi-end-of-file. */
   return ( !j && !*nc ) ? 1 : j;
}
