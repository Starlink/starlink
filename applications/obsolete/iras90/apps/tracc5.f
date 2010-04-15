      SUBROUTINE TRACC5( PQEXP, INDF, EL, IN, OUT, QEXP, STATUS )
*+
*  Name:
*     TRACC5

*  Purpose:
*     Mask samples which don't satify a supplied quality expression.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL TRACC5( PQEXP, INDF, EL, IN, OUT, QEXP, STATUS )

*  Description:
*     A quality expression is obtained from the environment using the
*     parameter specified by PQEXP. A copy of the input NDF DATA array
*     is produced in which all samples which do not satisfy the quality
*     expression are set bad. If any of the quality names included in
*     the supplied quality expression are not defined in the input NDF,
*     then an error is reported and the user is asked to supply a new
*     quality expression. The parameter value is cancelled before
*     returning.

*  Arguments:
*     PQEXP = CHARACTER * ( * ) (Given)
*        The parameter to use to obtain the quality expression.
*     INDF = INTEGER (Given)
*        An NDF identifier for the input CRDD file.
*     EL = INTEGER (Given)
*        The numner of elements in the NDF.
*     IN( EL ) = REAL (Returned)
*        The input DATA array.
*     OUT( EL ) = REAL (Returned)
*        A copy of the input DATA array in which all un-required samples
*        have been set bad.
*     QEXP = CHARACTER * ( * ) (Returned)
*        The current quality expression.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     18-NOV-1992 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PAR_ERR'          ! PAR_ error constants.
      INCLUDE 'DAT_PAR'          ! DAT_ constants.
      INCLUDE 'IRQ_PAR'          ! IRQ_ constants.

*  Arguments Given:
      CHARACTER PQEXP*(*)
      INTEGER INDF
      INTEGER EL
      REAL IN( EL )

*  Arguments Returned:
      REAL OUT( EL )
      CHARACTER QEXP*(*)

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      LOGICAL CHR_SIMLR          ! True if two strings are equal apart
                                 ! from case.

*  Local Variables:
      CHARACTER LOCS(5)*(DAT__SZLOC)! Locators to the quality name
                                 ! information.
      CHARACTER UNDEF(IRQ__QNREF)*(IRQ__SZQNM)! Undefined quality names.
      CHARACTER XNAME*(DAT__SZNAM)! NDF extension containing quality
                                  ! name information.


      INTEGER ERRPNT             ! Position of first error in quality
                                 ! expression.
      INTEGER I                  ! Loop count.
      INTEGER IDQ                ! Identifier for compiled expression.
      INTEGER NBAD               ! No. of re-prompts performed.
      INTEGER NUNDEF             ! Number of undefined quality names.


      LOGICAL ALLBAD             ! True if no good samples remain.
      LOGICAL MORE               ! True if a good quality expression has
                                 ! not yet been obtained.
      LOGICAL NOBAD              ! True if no bad samples found.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the returned array to hold a copy of the input array.
      DO I = 1, EL
         OUT( I ) = IN( I )
      END DO

*  Get a quality expression giving the quality of input CRDD samples
*  which are to be included in the display.
      CALL IRM_GETQX( PQEXP, QEXP, STATUS )

*  If the quality expression is "ANY", return with the output data equal
*  to the input data.
      IF( CHR_SIMLR( QEXP, 'ANY' ) ) GO TO 999

*  Attempt to find quality name information within the supplied NDF.
      CALL IRQ_FIND( INDF, LOCS, XNAME, STATUS )

*  Now loop round until a good quality expression is obtained.
      NBAD = 0
      MORE = .TRUE.
      DO WHILE( MORE .AND. STATUS .EQ. SAI__OK )

*  Attempt to compile the quality expression.
         CALL IRQ_COMP( LOCS, IRQ__QNREF, .TRUE., QEXP, UNDEF, NUNDEF,
     :                  ERRPNT, IDQ, STATUS )

*  Set the appropriate pixels bad in the returned array.
         CALL IRQ_SBAD( IDQ, .FALSE., EL, OUT, ALLBAD, NOBAD, STATUS )

*  Annul the compiled quality expression.
         CALL IRQ_ANNUL( IDQ, STATUS )

*  If all data has been rejected, report an error.
         IF( ALLBAD .AND. STATUS .EQ. SAI__OK ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'TRACC5_ERR1',
     :                'TRACC5: No data matches the quality expression.',
     :                    STATUS )
         END IF

*  If any errors have been reported add a context message and then flush
*  them. Limit the number of re-prompts to 4.
         IF( STATUS .NE. SAI__OK .AND. NBAD .LE. 4 ) THEN

            CALL ERR_REP( 'TRACC5_ERR2',
     :          'TRACC5: Please give an alternative quality expression',
     :                    STATUS )
            CALL ERR_FLUSH( STATUS )

*  Increment the number of re-prompts.
            NBAD = NBAD + 1

*  Set the returned array to hold a copy of the input array.
            DO I = 1, EL
               OUT( I ) = IN( I )
            END DO

*  Get a new quality expression.
            CALL PAR_CANCL( PQEXP, STATUS )
            CALL IRM_GETQX( PQEXP, QEXP, STATUS )

*  If the quality expression is "ANY", return with the output data equal
*  to the input data.
            IF( CHR_SIMLR( QEXP, 'ANY' ) ) MORE = .FALSE.

*  Otherwise, return.
         ELSE
            MORE = .FALSE.
         END IF

      END DO

*  Release the locators to the quality name information.
 998  CONTINUE
      CALL IRQ_RLSE( LOCS, STATUS )

*  If an error has occurred (Other than PAR__ABORT), return with the
*  output data equal to the input data and return a quality expression
*  of "ANY". Add a context message, and then flush the current error
*  context.
 999  CONTINUE
      IF( STATUS .NE. SAI__OK .AND. STATUS .NE. PAR__ABORT ) THEN
         QEXP = 'ANY'

         DO I = 1, EL
            OUT( I ) = IN( I )
         END DO

         CALL ERR_REP( 'TRACC5_ERR3',
     :               'TRACC5:Displaying all data regardless of quality',
     :                 STATUS )

         CALL ERR_FLUSH( STATUS )

      END IF

      END
