      SUBROUTINE SPD_WZLA( FU, NLINES, LINES, STATUS )
*+
*  Name:
*     SPD_WZLA

*  Purpose:
*     Read one number per line from a file.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPD_WZLA( FU, NLINES, LINES, STATUS )

*  Description:
*     This routine tries for all lines in a file to read one REAL
*     number. If this succeeds, the number is stored in the returned
*     array. If a read fails, the line is just ignored. If the array is
*     too short, an error is reported and non-OK status returned. If the
*     array is too long, the unused elements are set to zero.

*  Arguments:
*     FU = INTEGER (Given)
*        The Fortran unit number on which the file has been opened. This
*        routine does neither open, nor rewind the unit.
*     NLINES = INTEGER (Given)
*        The length of the LINES array.
*     LINES( NLINES ) = REAL (Returned)
*        The returned array of numbers read from the file.
*     STATUS = INTEGER (Given and Returned)
*        The global status. This is set if more than NLINES values could
*        have been read from the file.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     28 May 1993 (hme):
*        Original version.
*     25 Nov 1994 (hme):
*        Renamed from SPADM.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER FU
      INTEGER NLINES

*  Arguments Returned:
      REAL LINES( NLINES )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I, J               ! Loop variables
      INTEGER RDSTAT             ! Status returned from COLRD
      REAL VALI                  ! Value read from file
      CHARACTER * ( 80 ) COMM    ! Comment read from file

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  This is an endless loop broken when the EOF was found.
      I = 0
 1    CONTINUE
         CALL SPD_UAAK( FU, 1, 1, 0., 1, VALI, COMM, RDSTAT )

*     If a value could be read from the line, store it.
*     Else if EOF found, previous line was the last; break out of loop.
*     Else (comment line, line with too few columns, other error),
*     ignore the line.
         IF ( RDSTAT .EQ. 0 ) THEN
            I = I + 1
            IF ( I .LE. NLINES ) THEN
               LINES(I) = VALI
            ELSE
               STATUS = SAI__ERROR
               CALL ERR_REP( 'SPD_WZLA_E01', 'SPD_WZLA: Error rea' //
     :            'ding values from file: Storage array is too short.',
     :            STATUS )
               GO TO 500
            END IF
         ELSE IF ( RDSTAT .EQ. 1 ) THEN
            GO TO 2
         END IF
      GO TO 1
 2    CONTINUE

*  Fill the rest of the array with zeros.
      DO 3 J = I+1, NLINES
         LINES(J) = 0.
 3    CONTINUE

*  Return.
 500  CONTINUE
      END
