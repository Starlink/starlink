#include <string.h>
#include "hlpsys.h"
int hlpHreadx ( int ( * nametr ) ( int, char*, int, char* ),
                int navig, int lstring, char *string, int *nc )
/*
**  - - - - - - - - - -
**   h l p H r e a d x
**  - - - - - - - - - -
**
**  Read the HELP library index, with end-of-file detection, leaving
**  the sequential-access addresses pointing to the indexed record.
**
**  Given (in global data):
**     *fphl      FILE   file pointer for HELP library file
**     *hlopen    char   name of open HELP library
**     *hlnext    char   name of next HELP library
**     nextx      long   index address for this sequential access
**     levoff     int    logical level for the current HELP library
**
**  Given (arguments):
**     nametr     func   user-supplied name translation routine (note 1)
**     navig      int    navigation option ('D' or 'A' - note 2)
**     lstring    int    length of string
**
**  Returned (in global data):
**     *hlnext    char   HELP library name for next access
**     nextx      long   index address for next sequential access
**     nextd      long   data address for next sequential access
**     levoff     int    logical level for the current HELP library
**     loffnu     int    logical level for the next HELP library
**
**  Returned (arguments):
**     *string    char   input record (not including the address of data
**                       record and the terminating '\0')
**     *nc        int    length of record (0 or more)
**
**  Returned (function value):
**                int    status: +1 = quasi-end-of-file (see note 3)
**                                0 = OK
**                hlp_ILLEGAL_STATE = HELP system in wrong state
**                   hlp_READ_ERROR = read error
**                  hlp_CLOSE_ERROR = close error
**                    hlp_READ_WIDE = attempt to read outside file
**              hlp_RECORD_OVERSIZE = record overflows STRING (see note 4)
**                    hlp_BAD_INDEX = illegal index record
**                     hlp_SELF_REF = attempted switch to current file
**
**  Defined in #include file hlpsys.h:
**      error codes
**      POINTR
**
**  Notes:
**
**  1)  The user-supplied name translation function nametr has
**      arguments command, string1, lstring2, string2 and returns
**      a status:
**
**      * command (given) is an integer which the HELP system always
**        sets to zero;  the application may use other values of
**        command, for example to perform initializations or enquiries;
**
**      * string1 (given) is, for command=0, the HELP library name to
**        be translated;  this name can be is the value of the file
**        argument to the present routine (identifying the root library)
**        or the name following the '@' symbol in a HELP source file
**        (identifying another library grafted onto the current
**        library);
**
**      * lstring2 (given) is, for command=0, the length of string2
**        (defined next);
**
**      * string2 (returned) is, for command=0, the translation of
**        string2 into a filename for use in fopen calls;
**
**      * the nametr function return is a status, zero indicating
**        success.
**
**  2)  The argument navig controls what index record will be input on
**      the next call to the present routine.  A navig value of
**      'D' or 'd' means that the next entry to be read will be one
**      level further down in the same subtree if such a record exists.
**      Any other navig value ('A' is recommended) means that the next
**      entry to be read will be the next member of the current subtree
**      at the same level as the current record, if such a record exists.
**      In either case, if the entry just described does not exist, the
**      file is positioned at the next entry in the order of the data
**      region of the HELP file, which is in the same order as the
**      original HELP source.  (This behaviour is controlled by address
**      values which are embedded in the HELP index records themselves,
**      and is thus determined by the HELP library creation process
**      rather than the present routine.)
**
**  3)  The condition "quasi-end-of-file" means that the read was
**      OK but that the record length was zero.  HELP files have
**      such zero-length records at the end of the index and the end
**      of the HELP records.  If the condition arises, the file is
**      is "backspaced" - repositioned at the zero-length record - so
**      that any subsequent call will also report quasi-end-of-file.
**
**  4)  If the record overflows string, the first lstring-1 characters
**      are stored in string, *nc is set to lstring-1 and an error
**      status is returned.
**
**  5)  Following a call to this routine, a sequential read will
**      input the data record which the index record points to.
**
**  6)  See the source for hlpHdread and hlpHclose for side-effects
**      involving locations in global data.
**
**  7)  This routine is responsible for opening HELP libraries, when it
**      detects that the currently-open file is not the next one to be
**      requested.  When it opens a new library and it is not the first
**      one, it skips the top-level record, noting en passant the level
**      for use elsewhere (see hlpHchkl).
**
**  8)  This routine handles pointers to other HELP libraries, signalled
**      by an extra field, beginning with the character '@', after the
**      three addresses and before the level number and topic name.
**
**  Called:  hlpHopenr, hlpHdread, hlpHopenr, hlpDec, hlpCopyn.
**
**  Last revision:   2 January 2004
**
**  Copyright 2004 P.T.Wallace.  All rights reserved.
*/

/* Character signifying a pointer to another HELP library */
#define POINTR '@'

{
   int old, jstat, iptr, l, ifrom;
   long iadrx, iadrd, idown, ialong;


/* Abort if the HELP system isn't in the right state. */
   if ( jhelp != 2 ) return hlp_ILLEGAL_STATE;

/* Is a switch to another library pending? */
   if ( strcmp ( hlopen, hlnext ) ) {

   /* Yes: save the address. */
      iadrx = nextx;

   /* Close any open HELP library file. */
      if ( ( old = *hlopen ) )
         if ( ( fclose ( fphl ) ) ) return hlp_CLOSE_ERROR;

   /* Open the new HELP library file and set its logical level number. */
      if ( ( jstat = hlpHopenr ( nametr ) ) ) return jstat;
      levoff = loffnu;

   /* Restore the address. */
      nextx = iadrx;

   /* Are we switching to the start of a new HELP library? */
      if ( old && nextx <= 0 ) {

      /* Yes: it will have contained an entry for the current */
      /* level and keyword, so read and ignore the top-level  */
      /* entry in the new library.                            */
         if ( ( jstat = hlpHdread ( lstring, &nextx, string, nc ) ) )
            return jstat;
      }
   } else if ( nextx <= 0 ) {

   /* The right library is already open, but we are trying */
   /* to read the first record: reopen.                    */
      if ( ( jstat = hlpHopenr ( nametr ) ) ) return jstat;
   }

/* Remember the current positions. */
   iadrx = nextx;
   iadrd = nextd;

/* Read the next index entry. */
   if ( ( jstat = hlpHdread ( lstring, &nextx, string, nc ) ) )
      return jstat;

/* Quasi-end-of-file? */
   if ( ! *nc ) {

   /* Yes: backspace and set the status. */
      nextx = iadrx;
      nextd = iadrd;
      jstat = 1;
   } else {

   /* No: decode the "data", "down" and "along" addresses. */
      iptr = 0;
      nextd = hlpDec ( string, &iptr );
      idown = hlpDec ( string, &iptr );
      ialong = hlpDec ( string, &iptr );

   /* Make sure we've landed on a space. */
      if ( string [ iptr ] != (char) ' ' ) return hlp_BAD_INDEX;

   /* Set iptr and ifrom to the start of the next field. */
      ifrom = ++iptr;

   /* The next field is usually a level number but might be a pointer  */
   /* to another library.  If it is a pointer, find out where it ends. */
      if ( string [ ifrom ] == POINTR ) {
         iptr = hlpIndex ( string + ifrom, " " ) + ifrom;
         if ( iptr < ifrom || iptr >= *nc - 1 ) return hlp_BAD_INDEX;
         iptr++;
      }

   /* ifrom points to the level number or to the special character */
   /* preceding a pointer to another HELP library.  iptr points to */
   /* the level number.                                            */

   /* Look at navig to decide where to leave the file positioned. */
      if ( navig != 'D' && navig != 'd' && ialong >= 0 ) {

      /* "Along" case: we have the address ready. */
         nextx = ialong;
      } else {

      /* "Down" case: is the current entry a pointer to another library? */
         if ( string [ ifrom ] != (char) POINTR ) {

         /* No: we have the address ready. */
            nextx = idown;
         } else {

         /* Yes: make sure we aren't trying to switch to the */
         /* current library.                                 */
            l = iptr - ifrom - 2;
            if ( ! strncmp ( string + ifrom + 1, hlopen, l ) )
               return hlp_SELF_REF;

         /* Store the library name, causing library switch on next */
         /* read, and set the address to indicate the first entry. */
            hlpCopyn (hlnext, string + ifrom + 1, l );
            nextx = 0l;

         /* Decode the level number to determine the logical level */
         /* of the new library.                                    */
            loffnu = (int) hlpDec ( string, &iptr );
            if ( loffnu < 0 ) return hlp_BAD_INDEX;
            loffnu = loffnu + levoff;

         /* Point iptr back at the level number. */
            iptr--;
         }
      }

   /* Squeeze out the part of the record preceding the level */
   /* number and keyword.                                    */
      strcpy ( string, string + iptr );
      *nc = (int) strlen ( string );
   }
   return jstat;
 }
