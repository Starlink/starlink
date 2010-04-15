      SUBROUTINE RDPIIN( IFS, BUF, MAXENT, INDEX, NENT, STATUS )
*+
*  Name:
*     {routine_name}

*  Purpose:
*     To read in the object indexes from the file IFS.

*  Language:
*     Starlink Fortran 77

*  Description:
*     Reads in the file a line at a time using FIO_READ. Decodes
*     the first word as an integer. Stores these values in array INDEX.
*     Passes the number of entries ( not exceeding MAXENT ) back to
*     calling module. Quits at MAXENT values with warning but no STATUS.

*  Arguments:
*     {argument_name}[dimensions] = {data_type} ({argument_access_mode})
*        {argument_description}
*     [argument_spec]...
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  [optional_subroutine_items]...
*  Authors:
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     30-JAN-1991 (PDRAPER):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'FIO_ERR'          ! FIO system parameters

*  Arguments Given:
      INTEGER IFS
      INTEGER MAXENT
      CHARACTER BUF * ( * )

*  Arguments Returned:
      INTEGER INDEX( MAXENT )
      INTEGER NENT

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER MXWRD              ! maximum words returned from file line
                                 ! buffer
      PARAMETER ( MXWRD = 1 )
      INTEGER WRDLEN             ! maximum length of words returned
      PARAMETER ( WRDLEN = 14 )

*  Local Variables:
      INTEGER NREAD              ! number of characters read from file
      INTEGER NRET               ! actual number of words in buf
      INTEGER START( MXWRD )     ! starting positions of words located
      INTEGER STOP( MXWRD )      ! end positions of words located
      CHARACTER*( WRDLEN ) WORDS( MXWRD ) ! the first mxwrds if they
                                 ! exist
      INTEGER LSTAT              ! local status return

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Read in the line from file.
      NENT = 0
1     CONTINUE
         CALL FIO_READ( IFS, BUF, NREAD, STATUS )
         IF ( STATUS .EQ. SAI__OK ) THEN

*  Read line - attempt to extract first word.
            CALL CHR_DCWRD( BUF, MXWRD, NRET, START, STOP, WORDS,
     :                      LSTAT )
            IF ( NRET .EQ. MXWRD ) THEN
               NENT = NENT + 1

*  Check that we're not exceeding the maximum buffer space
               IF( NENT .GT. MAXENT ) THEN
                  CALL MSG_OUT( 'MAXENT', 'Exceeded maximum number of'
     :                         //' input entries - truncated ', STATUS )
                  GO TO 2
               ELSE

*  Have index field convert into value.
                  CALL CHR_CTOI( WORDS( 1 ), INDEX( NENT ), STATUS )
               END IF
            ELSE

*  Bad return - probably a blank line skip it.
               GO TO 1
            END IF

         ELSE
*  End of file - exit the routine.
            IF ( STATUS .EQ. FIO__EOF ) THEN
               CALL ERR_ANNUL( STATUS )
            ENDIF
            GO TO 2
         END IF

*  Read in the next line.
         GO TO 1
 2    CONTINUE
      END
* $Id$
