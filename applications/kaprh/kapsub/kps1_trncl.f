      SUBROUTINE KPS1_TRNCL( CLASS, LIST, NCHAR, STATUS )
*+
*  Name:
*     KPS1_TRNCL

*  Purpose:
*     Forms a comma-separated list of the TRANSFORM classes.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_TRNCL( CLASS, LIST, NCHAR, STATUS )

*  Description:
*     This routine takes the logical array of flags defining a TRANSFORM
*     structure's classification, and creates a string listing the
*     associated classifications separated by commas.

*  Arguments:
*     CLASS( TRN__MXCLS ) = LOGICAL (Given)
*        The classification flags.  Each element corresponds to a
*        specific classification.
*     LIST = CHARACTER * ( * ) (Returned)
*        The comma separated list of the TRANSFORM classes.
*     NCHAR = INTEGER (Returned)
*        The effective length of LIST inb characters.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  In order the classifications and meanings are:
*        - LINEAR       --- Linear and preserves straight lines.
*        - INDEPENDENT  --- Preserves the independence of the axes.
*        - DIAGONAL     --- Preserves the axes themselves.
*        - ISOTROPIC    --- Preserves angles and shapes.
*        - POSITIVE_DET --- A component of reflection is absent.
*        - NEGATIVE_DET --- A component of reflection is present.
*        - CONSTANT_DET --- The scale factor is constant.
*        - UNIT_DET     --- Areas (or volumes etc.) are preserved.

*  [optional_subroutine_items]...
*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1995 February 22 (MJC):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'TRN_PAR'          ! TRANSFORM constants

*  Arguments Given:
      LOGICAL CLASS( TRN__MXCLS )

*  Arguments Returned:
      CHARACTER * ( * ) LIST
      INTEGER NCHAR

*  Status:
      INTEGER STATUS             ! Global status

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the text for the list.
      LIST = ' '
      NCHAR = 0

*  Is the transformation linear?
      IF ( CLASS( TRN__LIN ) ) THEN
         CALL CHR_APPND( 'LINEAR,' , LIST, NCHAR )
         NCHAR = NCHAR + 1
      END IF

*  Is the transformation independent?
      IF ( CLASS( TRN__INDEP ) ) THEN
         CALL CHR_APPND( 'INDEPENDENT,' , LIST, NCHAR )
         NCHAR = NCHAR + 1
      END IF

*  Is the transformation diagonal?
      IF ( CLASS( TRN__DIAG ) ) THEN
         CALL CHR_APPND( 'DIAGONAL,' , LIST, NCHAR )
         NCHAR = NCHAR + 1
      END IF

*  Is the transformation isotropic?
      IF ( CLASS( TRN__ISOT ) ) THEN
         CALL CHR_APPND( 'ISOTROPIC,' , LIST, NCHAR )
         NCHAR = NCHAR + 1
      END IF

*  Does the transformation have a positive determinant?
      IF ( CLASS( TRN__POSDT ) ) THEN
         CALL CHR_APPND( 'POSITIVE_DET,' , LIST, NCHAR )
         NCHAR = NCHAR + 1
      END IF

*  Does the transformation have a negative determinant?
      IF ( CLASS( TRN__NEGDT ) ) THEN
         CALL CHR_APPND( 'NEGATIVE_DET,' , LIST, NCHAR )
         NCHAR = NCHAR + 1
      END IF

*  Does the transformation have a constant determinant?
      IF ( CLASS( TRN__CONDT ) ) THEN
         CALL CHR_APPND( 'CONSTANT_DET,' , LIST, NCHAR )
         NCHAR = NCHAR + 1
      END IF

*  Does the transformation have a unit determinant?
      IF ( CLASS( TRN__UNIDT ) ) THEN
         CALL CHR_APPND( 'UNIT_DET,' , LIST, NCHAR )
         NCHAR = NCHAR + 1
      END IF

*  Take off two characters for the last space and comma.
      NCHAR = NCHAR - 2

      END
