#include "hlpsys.h"
int hlpHdread ( int lstring, long *iadr, char *string, int *nc )
/*
**  - - - - - - - - - -
**   h l p H d r e a d
**  - - - - - - - - - -
**
**  Direct-access read from the HELP library.
**
**  Given (in global data):
**     jhelp     int        state of HELP system: 1=open/write
**     *fphl     FILE       file pointer for HELP library file
**     nchh      long       number of characters in HELP library file
**
**  Given (argument):
**     lstring   int        length of string
**     *iadr     long       character address within HELP file (1st = 0)
**
**  Returned (argument)
**     *iadr     long       points to next character in sequence
**     *string   char       input record
**     *nc       int        length of record (0 or more)
**
**  Returned (function value):
**               int        status:  0 = OK
**                   hlp_ILLEGAL_STATE = HELP system in wrong state
**                      hlp_READ_ERROR = read error
**                       hlp_READ_WIDE = attempt to read outside file
**                 hlp_RECORD_OVERSIZE = record overflows STRING (note 2)
**
**  The above error codes are defined in #include file hlpsys.h.
**
**  Notes:
**
**  1)  No check is made that *iadr points to the start of a record.
**
**  2)  If the record overflows string, the first lstring-1 characters
**      are stored in string followed by '\0', nc is set to lstring-1
**      and an error status is returned.
**
**  Last revision:   13 September 1995
**
**  Copyright 1996 P.T.Wallace.  All rights reserved.
*/

#define EOS '\0'

{
   int j, c;


/* Abort if the HELP system isn't in the right state. */
   if ( jhelp != 2 ) return hlp_ILLEGAL_STATE;

/* Abort if the output string isn't big enough at least for the EOS. */
   if ( lstring < 1 ) return hlp_RECORD_OVERSIZE;

/* Abort if the specified address lies outside the file. */
   if ( *iadr < 0 || *iadr >= nchh ) return hlp_READ_WIDE;

/* Locate the right place in the file. */
   if ( fseek ( fphl, *iadr, 0 ) ) return hlp_READ_ERROR;

/* Initialize the character count (which is also the string index). */
   *nc = 0;

/* Preset status to OK. */
   j = 0;

/* Read characters until end-of-string or an error condition. */
   c = 'A';
   while ( c != EOS ) {

   /* Read the next character. */
      if ( ( c = fgetc ( fphl ) ) == EOF ) {

      /* Error: set status and prepare to terminate string. */
         j = hlp_READ_ERROR;
         c = EOS;
      } else {

      /* Store the character and increment the string index. */
         string [ ( *nc ) ++ ] = (char) c;

      /* Was that the EOS? */
         if ( c != EOS ) {

         /* No: is string prematurely full? */
            if ( *nc >= lstring ) {

            /* Yes: replace the last character with EOS and set status. */
               c = EOS;
               string [ *nc - 1 ] = (char) c;
               j = hlp_RECORD_OVERSIZE;

          /* Or are we about to drop off the end of the file? */
            } else if ( ( *iadr ) ++ >= nchh ) {

            /* Yes: replace the last character with EOS and set status. */
               c = EOS;
               string [ *nc - 1 ] = (char) c;
               j = hlp_READ_WIDE;
            }
         }
      }
   }

/* Adjust the character count to exclude the EOS character. */
   ( *nc ) --;

/* Push the address to the start of the next record. */
   ( *iadr ) ++;

/* Return the final status. */
   return j;
}
