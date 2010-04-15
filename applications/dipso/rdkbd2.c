/*+
*  Name:
*     RDKBD2

*  Purpose:
*     Obtain a string from the controlling terminal, pre-loading the
*     keyboard buffer with a supplied string, and allowing editing of
*     the keyboard buffer using arrow keys and control keys.

*  Language:
*     ANSI C (callable from fortran)

*  Invocation:
*     RDKBD2( STRING, PBUF, LPRM, TERM, LENOUT )

*  Description:
*     The prompt supplied in argument PBUF is written to the screen,
*     followed by any string supplied in argument STRING. The text cursor
*     is placed at the end of this string. The user may then enter
*     text using the following editing facilities:
*
*        control-A   - Jump to the start of the input buffer.
*        control-E   - Jump to the current end of the input buffer.
*        control-U   - Clear the input buffer.
*        control-N   - Toggle insert/overstrike mode.
*        control-D   - End input.
*        up-arrow    - End input.
*        down-arrow  - End input.
*        left-arrow  - Move text cursor one character to the left.
*        right-arrow - Move text cursor one character to the right.
*        delete/backspace - Delete the character to left of the cursor
*                      and move remaining text one character to the left.
*        return      - End input.
*
*     Movement of the text cursor is limited by the current end of the
*     input buffer and the end of the prompt string.
*
*     The contents of the buffer are returned in argument STRING when any
*     of the "End input" keys listed above is pressed (a code identifying
*     the specific key pressed is returned in argument TERM).
*
*     The controlling terminal is first opened as device /dev/tty. The
*     "termios" facilities are then used to disable canonical processing
*     of key presses by the C IO system. This means that each call
*     to the read function is satisfied when a single key stroke is
*     made by the user, the keycode(s) for the pressed key being returned
*     in the read buffer. ECHO mode is also disabled so that key presses
*     are not automatically echoed to the screen. The echoing of keys
*     is performed internally by this function using expliciy calls to
*     the write function. An ANSI escape sequence is written to the
*     terminal to try and ensure that the keypad returns normal keycodes,
*     rather than application keycodes. On completion, the original terminal
*     characteristics are re-instated.

*  Arguments:
*     STRING = CHARACTER * ( * ) (Given and Returned)
*        The string supplied by the user. If the string is not blank
*        on entry, then the supplied string is pre-loaded into the
*        keyboard buffer, and the text cursor is placed at the end of
*        supplied text.
*     PBUF = CHARACTER * ( * ) (Given)
*        The prompt text.
*     LPRM = INTEGER (Given)
*        The number of characters from pbuf which are to be displayed
*        as the prompt.
*     TERM = INTEGER (Returned)
*        A code indicating how the keyboard entry was terminated by the
*        user:
*           1 - by pressing the RETURN key.
*           2 - by pressing the up-arrow key.
*           3 - by pressing the down-arrow key.
*           4 - by pressing control-D.
*     LENOUT = INTEGER (Given and Returned)
*        The number of characters in argument STRING.

*  Returned Function Value:
*     RDKBD2 = INTEGER
*        A status code (zero for success).

*  Notes:
*     -  Only works on UNIX!

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     20-SEP-1994 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     -  Does not use TERMIOS or TERMCAP databases, so may not work on
*     non-ANSI terminals. The escape sequences used are hard-wired into
*     the following code.
*     {note_any_bugs_here}

*-
*/


#include <fcntl.h>
#include <sys/termios.h>
#include <string.h>
#include "f77.h"

F77_INTEGER_FUNCTION(rdkbd2)( CHARACTER(string), CHARACTER(pbuf),
                              INTEGER(lprm), INTEGER(term), INTEGER(lenout)
                              TRAIL(string) TRAIL(pbuf) ){

GENPTR_CHARACTER(string)
GENPTR_CHARACTER(pbuf)
GENPTR_INTEGER(lprm)
GENPTR_INTEGER(term)
GENPTR_INTEGER(lenout)

      int fd,i,j,inov;
      char buf[3];

      char ret[2]={10,13};
      char del[4]={8,27,'[','P'};
      char ins[3]={27,'[','@'};
      char left[3]={27,'[','D'};
      char right[3]={27,'[','C'};
      char clear[3]={27,'[','K'};

      struct termios *terms, *oldterms;

/*  Open the controlling terminal. */
      fd = open("/dev/tty", O_RDWR );
      if( fd >=0 ){

/*  Set up the terminal. */
         terms = (struct termios *) malloc( sizeof( struct termios ) );
         if( terms == 0 ) return(-1);

         oldterms = (struct termios *) malloc( sizeof( struct termios ) );
         if( oldterms == 0 ) return(-1);

         if( tcgetattr( fd, terms ) != 0 ) return(-2);
         memcpy( oldterms, terms, sizeof( struct termios ) );

         terms->c_lflag = ( terms->c_lflag ) & (~ICANON ) & (~ECHO );
         terms->c_cc[VMIN]=1;
         if( tcsetattr( fd, TCSADRAIN, terms ) != 0 ) return(-3);
         free( terms );

         buf[0]= 27;
         buf[1]= 62;
         if( write( fd, buf, 2 ) != 2 ) return(-4);

/*  Display the prompt, and any initial string to put in the buffer. */
         if( *lprm > 0 ) {
            if( write( fd, pbuf, *lprm ) != *lprm ) return(-5);
         }
         if( *lenout > 0 ) {
            if( write( fd, string, *lenout ) != *lenout ) return(-20);
         }

/*  Indicate that no terminator has yet been given. */
         *term = 0;

/*  Initialise the index within string at which the next character will
 *  be written. */
         i = *lenout;

/*  Start of in insert mode. */
         inov = 1;

/*  Loop round until a terminator is given. */
         while( *term == 0 ) {

/*  Read a single byte from the keyboard. */
            read( fd, buf, 1);

/*  If an escape character has been obtained, read the next byte. */
            if( buf[ 0 ] == 27 ) {
               read( fd, &buf[1], 1);

/*  If the escape sequence is an arrow key, get the next byte which
 *  specifies the particular arrow key. */
               if( buf[1] == 91 ){
                  read( fd, &buf[2], 1);

/*  Store the terminator if up or down arrow was pressed. */
/*  Cursor up */
                  if( buf[2] == 65 ) {
                     *term = 2;

/*  Cursor down */
                  } else if( buf[2] == 66 ) {
                     *term = 3;

/*  Cursor right */
                  } else if( buf[2] == 67 ) {
                     if( i < *lenout ) {
                        if( write( fd, right, 3 ) != 3 ) return(-22);
                        i++;
                     }

/*  Cursor left */
                  } else if( buf[2] == 68 ) {
                     if( i > 0 ) {
                        if( write( fd, left, 3 ) != 3 ) return(-22);
                        i--;
                     }
                  }

               }

/*  Now deal with non-escape characters. */
            } else {

/*  <RETURN> */
               if( buf[0] == 10 ) {
                  write( fd, ret, 2 );
                  *term = 1;

/*  Delete or backspace */
               } else if( buf[0] == 127 || buf[0] == 8 ){
                  if( i > 0 ) {
                     if( write( fd, del, 4 ) != 4 ) return(-22);
                     for(j=i;j<string_length;j++) string[j-1] = string[j];
/*                     string[string_length-1]=0;*/
                     (*lenout)--;
                     i--;
                  }

/* ^D - End of input */
               } else if( buf[0] == 4 ){
                  *term = 4;

/* ^N - Toggle insert overstrike mode */
               } else if( buf[0] == 14 ){
                  inov = 1 - inov;

/* ^A - jump to start */
               } else if( buf[0] == 1 ){
                  while(i>0){
                     i--;
                     if( write( fd, left, 3 ) != 3 ) return(-22);
                  }

/* ^E - jump to end */
               } else if( buf[0] == 5 ){
                  while(i<*lenout){
                     i++;
                     if( write( fd, right, 3 ) != 3 ) return(-22);
                  }

/* ^U - Empty buffer */
               } else if( buf[0] == 21 ){
                  *lenout = 0;
                  string[0]=0;
                  while(i>0){
                     i--;
                     if( write( fd, left, 3 ) != 3 ) return(-22);
                  }
                  if( write( fd, clear, 3 ) != 3 ) return(-22);

/*  Any other character. */
               } else {
                  if( inov == 1 ) {
                     if( write( fd, ins, 3 ) != 3 ) return(-21);
                     if( write( fd, buf, 1 ) != 1 ) return(-21);
                     for(j=*lenout-1;j>=i;j--) string[j+1]=string[j];
                     string[i] = buf[0];
                     i++;
                     (*lenout)++;
                  } else {
                     if( write( fd, buf, 1 ) != 1 ) return(-21);
                     string[i]=buf[0];
                     i++;
                     if( i > *lenout ) *lenout = i;
                  }
               }
            }
         }


         if( tcsetattr( fd, TCSADRAIN, oldterms ) != 0 ) return(-3);
         free( oldterms );


         close(fd);
/*         string[*lenout] = 0;*/

      } else {
         return(-8);
      }

      return(0);
}
