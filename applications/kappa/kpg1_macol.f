      SUBROUTINE KPG1_MACOL( PNCOL, LP, UP, COLIND, STATUS )
*+
*  Name:
*     KPG1_MACOL

*  Purpose:
*     Obtain a marker colour.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_MACOL( PNCOL, LP, UP, COLIND, STATUS )

*  Description:
*     This routine obtains a colour index to be used to mark features
*     in an image display.  A string is obtained from the parameter
*     system.  The interpretation of this string provides a number of
*     ways to specify the colour index requested.  The options are:
*
*       'MAX'          - The maximum (non-reserved) colour index, i.e.
*                        the highest colour index used for the display
*                        of an image.
*       'MIN'          - The minimum non-reserved colour index, i.e. the
*                        lowest colour index used for the display of an
*                        image.
*       An integer     - The actual colour index. It is constrained
*                        between 0 and the highest colour index.
*       A named colour - Uses the named colour from the palette, and if
*                        it is not present, the nearest colour from the
*                        palette is selected.

*  Arguments:
*     PNCOL = CHARACTER * ( * ) (Given)
*        The name of the ADAM parameter to obtain the marker colour.
*        It should have type LITERAL.
*     LP = INTEGER (Given)
*        The lowest non-reserved colour index.
*     UP = INTEGER (Given)
*        The highest non-reserved colour index.
*     COLIND = INTEGER (Returned)
*        The colour index of the selected colour.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Prior Requirements:
*     - A GKS image-display workstation must be open and active.

*  [optional_subroutine_items]...
*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1991 July 23 (MJC):
*        Original version.
*     1996 October 1 (MJC):
*        Allow for change to the parameter system, where the literal
*        values "MIN" and "MAX" cannot be supplied.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      CHARACTER * ( * ) PNCOL
      INTEGER LP
      INTEGER UP

*  Arguments Returned:
      INTEGER COLIND

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER COLOUR * ( 18 )  ! The colour obtained from the
                                 ! parameter system
      LOGICAL LOOP               ! Retry for a colour?

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Validate the colour-index bounds.
      IF ( LP .GT. UP ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETI( 'LP', LP )
         CALL MSG_SETI( 'UP', UP )
         CALL ERR_REP( 'KPG1_MACOL_CIBOUND',
     :     'Upper colour-index bound (^UP) is less than lower bound '/
     :     /'(^LP).', STATUS )
         GOTO 999
      END IF

*  Define the range of acceptable "colours".  This uses the ASCII
*  collating sequence.  The range includes all numbers and upper and
*  lowercase letters, but also includes the next adjacent characters at
*  either end to indicate the MIN and MAX values.
      CALL PAR_MINC( PNCOL, '/', STATUS )
      CALL PAR_MAXC( PNCOL, '{', STATUS )

*  Loop until a permitted value has been obtained, or until an error
*  occurred.
      LOOP = .TRUE.
      DO WHILE ( STATUS .EQ. SAI__OK .AND. LOOP )

*  Obtain the colour.
*  ==================

*  Obtain the string from the parameter system.
         CALL PAR_GET0C( PNCOL, COLOUR, STATUS )

*  Check that a value was obtained.
         IF ( STATUS .EQ. SAI__OK ) THEN

*  Attempt to convert the string into a colour index. 
            CALL KPG1_GTCOL( COLOUR, LP, UP, COLIND, STATUS )

*  Try again if the colour was not recognised.
            IF ( STATUS .NE. SAI__OK ) THEN
               CALL ERR_FLUSH( STATUS )
               CALL PAR_CANCL( PNCOL, STATUS )

*  Exit the loop.
            ELSE
               LOOP = .FALSE.
            END IF

         END IF

      END DO

  999 CONTINUE

      END
