*+  ARRSTR - Output the name and dimensions of an array of structures.
      SUBROUTINE ARRSTR( NAME, NDIM, DIMS, INDENT, STATUS )
*    Description :
*     The given name, NAME, and dimensions, DIMS( NDIM ) are formatted
*     into a message, indented by INDENT spaces. The string
*     <array of structures> is then appended and the message output.
*    Invocation :
*     CALL ARRSTR( NAME, NDIM, DIMS, INDENT, STATUS )
*    Parameters :
*     NAME = CHARACTER*(DAT__SZNAM)( READ )
*           Name of the array of structures.
*     NDIM = INTEGER( READ )
*           Dimensionality of the array of structures.
*     DIMS( DAT__MXDIM ) = INTEGER( READ )
*           Array of dimensions for the array of structures.
*     INDENT = INTEGER( READ )
*           Indentation level for the output line of information.
*     STATUS = INTEGER( UPDATE )
*           This is the global status, if it has an error value on entry then
*           an immediate return will occur. If an error occurs during the
*           execution of the routine then STATUS will be returned containing
*           the appropriate error number.
*    Method :
*     If no error on entry then
*        Initialise line with indentation.
*        Append the name.
*        Append the dimensions.
*        Find the last blank character in line and move 5 characters past it.
*        Append "<array of structures>"
*        Output the line as the only token in a message.
*     Endif
*    Authors :
*     Dave Baines (ROE::ASOC5)
*    History :
*     14/03/1984 : Original version                   (ROE::ASOC5)
*     07/04/1984 : Documentation revised, PUTDIM used (ROE::ASOC5)
*    Type Definitions :
      IMPLICIT NONE     ! switch off the default typing
*    Global constants :
      INCLUDE 'SAE_PAR' ! enviroment constants
      INCLUDE 'DAT_PAR'
*    Import :
      CHARACTER*(DAT__SZNAM)
     :  NAME ! name of the array of structures
      INTEGER
     :  NDIM, ! dimensionality of the array of structures
     :  DIMS( DAT__MXDIM ), ! array of dimensions for the array
     :  INDENT ! indentation for output
*    Status :
      INTEGER STATUS ! the global status
*    External references :
      INTEGER CHR_LEN   ! string length
*    Local Constants :
      INTEGER LNSIZE
      PARAMETER ( LNSIZE = 72 ) ! line string length
*    Local variables :
      CHARACTER*( LNSIZE )
     :  LINE ! line string
      INTEGER
     :  LENG, ! current position in line string
     :  I     ! character index
*-

*    check for error
      IF( STATUS .EQ. SAI__OK ) THEN

*       initialise line ( with indentation )
         LINE = ' '
         LENG = INDENT

*       append name
         I = CHR_LEN( NAME )
         CALL CHR_PUTC( NAME(1:I), LINE, LENG )

*       append dimensions
         CALL PUTDIM( NDIM, DIMS, LINE, LENG )

*       find last blank character in line and move 5 characters past it
         LENG = CHR_LEN( LINE ) + 5

*       append "<array of structures>"
         CALL CHR_PUTC( '<array of sructures>', LINE, LENG )

*       output the line as the only token in a message
         CALL MSG_SETC( 'LINE', LINE(1:LENG) )
         CALL MSG_OUT( 'TRACE_ARRS', '^LINE', STATUS )
      ENDIF

      END
