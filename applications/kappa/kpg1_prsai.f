      SUBROUTINE KPG1_PRSAI( BUFFER, MAXVAL, VALUES, NVAL, STATUS )
*+
*  Name:
*     KPG1_PRSAx
 
*  Purpose:
*     Extracts a list of numerical values from a string.
 
*  Language:
*     Starlink Fortran 77
 
*  Invocation:
*     CALL KPG1_PRSAI( BUFFER, MAXVAL, VALUES, NVAL, STATUS )
 
*  Description:
*     A supplied string is search for words (separated by spaces, tabs
*     or commas), which are extracted and converted to the required
*     numerical data type.  The numerical values and their number are
*     returned.
 
*  Arguments:
*     BUFFER = CHARACTER * ( * ) (Given)
*        The string containing a list of numerical values.
*     MAXVAL = INTEGER (Given)
*        The maximum number of values that can be read from the buffer
*        and stored.
*     VALUES( MAXVAL ) = ? (Returned)
*        The numeric values extracted from the string.
*     NVAL = INTEGER (Returned)
*        The actual number of values extracted from the string.
*     STATUS = INTEGER (Given and Returned)
*        The global status.
 
*  Notes:
*     -  There is a routine for each numeric data type: replace "x"
*        in the routine name by D, R, I, W, UW, B or UB as appropriate.
 
*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}
 
*  History:
*     1990 Jun 12 (MJC):
*        Original version.
*     {enter_changes_here}
 
*  Bugs:
*     {note_any_bugs_here}
 
*-
 
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing
 
*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'CHR_ERR'          ! CHR error codes
 
*  Arguments Given:
      INTEGER
     :  MAXVAL
 
      CHARACTER * ( * )
     :  BUFFER
 
*  Arguments Returned:
      INTEGER
     :  VALUES( MAXVAL )
      INTEGER
     :  NVAL
 
*  Status:
      INTEGER STATUS             ! Global status
 
*  External References:
      INTEGER
     :  CHR_LEN                  ! Length of the string less trailing
                                 ! blanks
 
*  Local Variables:
      INTEGER
     :  INDEXE,                  ! Location within the string of the end
                                 ! of the current word
     :  INDEXS,                  ! Location within the string of the
                                 ! start of the current word
     :  NCOM                     ! Length of the input string less
                                 ! trailing blanks
 
*.
 
*    Check inherited global status.
 
      IF ( STATUS .NE. SAI__OK ) RETURN
 
*    Start a new error context.
 
      CALL ERR_MARK
 
*    Get the length of the string.
 
      NCOM = CHR_LEN( BUFFER )
      NVAL = 0
      INDEXE = -1
 
*    Loop until the end of the buffer.
 
      DO WHILE ( INDEXE .LT. NCOM .AND. NVAL .LE. MAXVAL .AND.
     :           STATUS .EQ. SAI__OK )
 
*       Shift the search to the next value.
 
         INDEXS = INDEXE + 1
 
*       Find the start and end indices of the value.
 
         CALL CHR_FIWS( BUFFER, INDEXS, STATUS )
         INDEXE = INDEXS
         CALL CHR_FIWE( BUFFER, INDEXE, STATUS )
 
*       Watch for the case where the word terminates the line.
*       Since this is quite normal the error should be annulled.
 
         IF ( STATUS .EQ. CHR__ENDOFSENT ) CALL ERR_ANNUL( STATUS )
 
*       Extract the value required and convert to INTEGER type.
 
         NVAL = NVAL + 1
         CALL CHR_CTOI( BUFFER( INDEXS:INDEXE ), VALUES( NVAL ),
     :                    STATUS )
      END DO
 
*    Should an error have occurred in extracting the data values, it
*    either came from finding the value or converting it from
*    characters.  If it was the latter then number of values is correct,
*    otherwise if the word was not found and therefore the number
*    of values must be decremented since it is not an abnormal way to
*    exit the loop.
 
      IF ( STATUS .NE. SAI__OK ) THEN
         IF ( STATUS .EQ. CHR__WRDNOTFND )  THEN
            NVAL = NVAL - 1
            CALL ERR_ANNUL( STATUS )
         ELSE
            CALL MSG_SETI( 'N', NVAL )
            CALL MSG_SETC( 'S', BUFFER( INDEXS:INDEXE ) )
            CALL ERR_REP( 'KPG1_PRSAI_EXTR',
     :        'KPG1_PRSAI: Error extracting value ^N from string ^S.',
     :        STATUS )
         END IF
      END IF
 
*    Release the error context.
 
      CALL ERR_RLSE
 
      END
