*+  TRACON - Output "contents of" message for an array of structures.
      SUBROUTINE TRACON( NAME, NDIM, DIMS, INDENT, STATUS )
*    Description :
*     Outputs the "Contents of " message for an element of an array of
*     structures which is being traced.
*    Invocation :
*     CALL TRACON( NAME, NDIM, DIMS, INDENT, STATUS )
*    Parameters :
*     NAME = CHARACTER*( DAT__SZNAM )( READ )
*           Name of the array of structures being traced through.
*     NDIM = INTEGER( READ )
*           Dimensionality of the array of structures being traced through.
*     DIMS( DAT__MXDIM ) = INTEGER( READ )
*           Dimension indices for current element of array of structures.
*     INDENT = INTEGER( READ )
*           Indentation level for message output.
*     STATUS = INTEGER( UPDATE )
*           This is the global status, if an error occurs during the execution
*           of this routine then STATUS will be set to the appropriate value.
*    Method :
*     Write out blank line for spacing.
*     Set message line to all blanks.
*     Initialise start position in the line to the indent level.
*     Place 'Contents of ' in line at this position.
*     Find out the length of the NAME ignoring trailing blanks.
*     Append the name onto the message line.
*     Append the dimensions.
*     Get the line length ignoring trailing blanks.
*     Set up the message line as a single message token.
*     Push the message out.
*    Authors :
*     Dave Baines (ROE::ASOC5)
*    History :
*     15/03/1984 : Original version              (ROE::ASOC5)
*     07/04/1984 : Revised version to use PUTDIM (ROE::ASOC5)
*    Type Definitions :
      IMPLICIT NONE     ! switch off the default typing
*    Global constants :
      INCLUDE 'SAE_PAR' ! enviroment constants
      INCLUDE 'DAT_PAR'
*    Import :
      CHARACTER*( DAT__SZNAM )
     :  NAME ! name of the array
      INTEGER
     :  NDIM, ! dimensionality of array
     :  DIMS( DAT__MXDIM ), ! dimensions of array
     :  INDENT ! indentation level
*    Status :
      INTEGER STATUS ! the global status
*    External references :
      INTEGER CHR_LEN ! returns string length ignoring trailing blanks
*    Local constants :
      INTEGER LNSIZE
      PARAMETER( LNSIZE = 78 ) ! message line length
*    Local variables :
      CHARACTER*( LNSIZE )
     :  LINE ! character string line for the message
      INTEGER
     :  LENG, ! index inot character string LINE
     :  LEN   ! length of character strings ignoring trailing blanks
*-

*    write out blank line for spacing
      CALL MSG_OUT( 'BLANK', ' ', STATUS )

*    set LINE to all blanks
      LINE = ' '

*    initialise start position in the line to the indent level
      LENG = INDENT

*    place 'Contents of ' in line at this position
      CALL CHR_PUTC( 'Contents of ', LINE, LENG )

*    find out the length of the NAME ignoring trailing blanks
      LEN = CHR_LEN( NAME )

*    push the NAME out into the LINE string
      CALL CHR_PUTC( NAME(1:LEN), LINE, LENG )

*    append the dimensions
      CALL PUTDIM( NDIM, DIMS, LINE, LENG )

*    get the LINE length ignoring trailing blanks
      LEN = CHR_LEN( LINE )

*    set up the message line, LINE, as a single message token, MESSAGE
      CALL MSG_SETC( 'MESSAGE', LINE )

*    push message out
      CALL MSG_OUT( 'TRARR_CON', '^MESSAGE', STATUS )

      END
