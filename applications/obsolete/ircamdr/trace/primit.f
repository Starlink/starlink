*+  PRIMIT - Output an informational message for a primitive object.
      SUBROUTINE PRIMIT( PRIMLO, NAME, TYPE, SIZE, NDIM, DIMS, INDENT,
     :  STATUS )
*    Description :
*     Puts out information and values for the primitive object with locator
*     PRIMLO via the message system.
*    Invocation :
*     CALL PRIMIT( PRIMLO, NAME, TYPE, SIZE, NDIM, DIMS, INDENT, STATUS )
*    Parameters :
*     PRIMLO = CHARACTER*(DAT__SZLOC)( READ )
*           Locator to the primitive object.
*     NAME = CHARACTER*(*)( READ )
*           Name of the primitive object.
*     TYPE = CHARACTER*(*)( READ )
*           Type of the primitive object.
*     SIZE = INTEGER( READ )
*           Number of elements for the primitive object if it is treated
*           as a vector.
*     NDIM = INTEGER( READ )
*           Dimensionality of the primitive object.
*     DIMS( DAT__MXDIM ) = INTEGER( READ )
*           Array of dimensions of the primitive object.
*     INDENT = INTEGER( READ )
*           Indentation level for the output information.
*     STATUS = INTEGER( UPDATE )
*           This is the global status, if it has an error value on entry
*           then an immediate return will occur. If an error occurs during
*           the execution of the routine then STATUS will be returned
*           containing the appropriate error message.
*    Method :
*     If no error on entry then
*        Initialise message line, with indentation.
*        Append the primitive component name.
*        Append the primitive component dimensions.
*        Find the last blank character in the line and move 2 characters
*        past it.
*        Append the values.
*        Output the line as the only token in a message.
*     Endif
*    Authors :
*     Dave Baines (ROE::ASOC5)
*    History :
*     14/03/1984 : Original version                 (ROE::ASOC5)
*     07/04/1984 : Revised to use PUTDIM and PUTVAL (ROE::ASOC5)
*    Type Definitions :
      IMPLICIT NONE     ! switch off the default typing
*    Global constants :
      INCLUDE 'SAE_PAR' ! enviroment constants
      INCLUDE 'DAT_PAR'
*    Import :
      CHARACTER*(DAT__SZLOC)
     :  PRIMLO ! locator pointing at primitive object to be examined
      CHARACTER*(DAT__SZNAM)
     :  NAME ! object name
      CHARACTER*(DAT__SZTYP)
     :  TYPE ! type of the primitive object
      INTEGER
     :  SIZE, ! size of object if vectorized
     :  NDIM, ! dimensionality of the object
     :  DIMS( DAT__MXDIM ), ! array of object dimensions
     :  INDENT ! indentation for output
*    Status :
      INTEGER STATUS ! the global status
*    External references :
      INTEGER CHR_LEN   ! string length ignoring trailing blanks
*    Local Constants :
      INTEGER LNSIZE
      PARAMETER ( LNSIZE = 78 ) ! line string length
*    Local variables :
      CHARACTER*( LNSIZE )
     :  LINE ! line string
      INTEGER
     :  LENG, ! line length
     :  I     ! character index
*-

*    check for error
      IF( STATUS .EQ. SAI__OK ) THEN

*       initialise line (with indentation)
         LINE = ' '
         LENG = INDENT

*       append name
         I = CHR_LEN( NAME )
         CALL CHR_PUTC( NAME(1:I), LINE, LENG )

*       append dimensions
         IF ( NDIM .GT. 0 ) THEN
            CALL PUTDIM( NDIM, DIMS, LINE, LENG )
         ENDIF

*       find last blank character in line and move 2 characters past it
         LENG = CHR_LEN( LINE ) + 2

*       append values
         CALL PUTVAL( PRIMLO, TYPE, NDIM, DIMS, SIZE, LINE, LENG )

*       output line as the only token in a message
         CALL MSG_SETC( 'LINE', LINE(1:LENG) )
         CALL MSG_OUT( 'PRIMIT_INFO', '^LINE', STATUS )
      ENDIF

      END
