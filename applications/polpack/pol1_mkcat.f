      SUBROUTINE POL1_MKCAT( PARAM, CIRC, UNITS, VAR, CI, STATUS )
*+
*  Name:
*     POL1_MKCAT

*  Purpose:
*     Create a CAT catalogue to hold polarisation parameters.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL POL1_MKCAT( PARAM, CIRC, UNITS, VAR, CI, STATUS )

*  Description:
*     This routine creates a new CAT catalogue. Columns are created for the 
*     pixel coordinates (X and Y), Stokes parameters (I, Q and U), percentage 
*     polarisation, polarised intensity, and polarisation angle (in
*     degrees). Columns for the standard deviation associated with each 
*     (except pixel coordinates) are also produced. For circular polarisation, 
*     the Q and U columns are replaced by a single column for V. 

*  Arguments:
*     PARAM = CHARACTER * ( * ) (Given)
*        The parameter through which the name of the new catalogue should
*        be obtained.
*     CIRC = LOGICAL (Given)
*        Set this to .TRUE. if circular polarisation is being measured.
*     UNITS = CHARACTER * ( * ) (Given)
*        The units used to measure intensities and Stokes parameters. May
*        be blank.
*     VAR = LOGICAL (Given)
*        Are variance values available? If not, the columns containing
*        standard deviations are not created.
*     CI = INTEGER (Returned)
*        A CAT identifier for the created catalogue.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     26-JAN-1998 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST constants and function declarations
      INCLUDE 'CAT_PAR'          ! CAT constants

*  Arguments Given:
      CHARACTER PARAM*(*)
      LOGICAL CIRC
      CHARACTER UNITS*(*)
      LOGICAL VAR

*  Arguments Returned:
      INTEGER CI

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER II                 ! CAT identifier for most recent part
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Create the catalogue.
      CALL CAT_CREAT( PARAM, CI, STATUS )

*  Create the columns...

*  Pixel X coordinate.
      CALL CAT_CNEWS( CI, 'X', CAT__TYPER, 0, 'pixels', 'F7.1', 
     :                'X pixel coordinate', II, STATUS )

*  Pixel Y coordinate.
      CALL CAT_CNEWS( CI, 'Y', CAT__TYPER, 0, 'pixels', 'F7.1', 
     :                'Y pixel coordinate', II, STATUS )

*  Total intensity.
      CALL CAT_CNEWS( CI, 'I', CAT__TYPER, 0, UNITS, 'G13.6', 
     :                'Total intensity', II, STATUS )

*  Total intensity standard deviation (if variances are available).
      IF( VAR ) THEN 
         CALL CAT_CNEWS( CI, 'DI', CAT__TYPER, 0, UNITS, 'G13.6', 
     :                   'Standard deviation on total intensity', II, 
     :                   STATUS )
         CALL CAT_TATTL( II, 'PRFDSP', .FALSE., STATUS )
      END IF

*  Q and U (if not circular).
      IF( .NOT. CIRC ) THEN
         CALL CAT_CNEWS( CI, 'Q', CAT__TYPER, 0, UNITS, 'G13.6', 
     :                   'Stokes parameter Q', II, STATUS )
         CALL CAT_TATTL( II, 'PRFDSP', .FALSE., STATUS )

         IF( VAR ) THEN
            CALL CAT_CNEWS( CI, 'DQ', CAT__TYPER, 0, UNITS, 'G13.6', 
     :                     'Standard deviation on Q', II, STATUS )
            CALL CAT_TATTL( II, 'PRFDSP', .FALSE., STATUS )
         END IF

         CALL CAT_CNEWS( CI, 'U', CAT__TYPER, 0, UNITS, 'G13.6', 
     :                   'Stokes parameter U', II, STATUS )
         CALL CAT_TATTL( II, 'PRFDSP', .FALSE., STATUS )

         IF( VAR ) THEN
            CALL CAT_CNEWS( CI, 'DU', CAT__TYPER, 0, UNITS, 'G13.6', 
     :                     'Standard deviation on U', II, STATUS )
            CALL CAT_TATTL( II, 'PRFDSP', .FALSE., STATUS )
         END IF

*  V (if circular).
      ELSE
         CALL CAT_CNEWS( CI, 'V', CAT__TYPER, 0, UNITS, 'G13.6', 
     :                   'Stokes parameter V', II, STATUS )
         CALL CAT_TATTL( II, 'PRFDSP', .FALSE., STATUS )

         IF( VAR ) THEN
            CALL CAT_CNEWS( CI, 'DV', CAT__TYPER, 0, UNITS, 'G13.6', 
     :                     'Standard deviation on V', II, STATUS )
            CALL CAT_TATTL( II, 'PRFDSP', .FALSE., STATUS )
         END IF
      END IF

*  Percentage polarisation.
      CALL CAT_CNEWS( CI, 'P', CAT__TYPER, 0, '%', 'F6.2', 
     :                'Percentage polarisation', II, STATUS )

      IF( VAR ) THEN
         CALL CAT_CNEWS( CI, 'DP', CAT__TYPER, 0, '%', 'F6.3', 
     :                  'Standard deviation on percentage polarisation',
     :                   II, STATUS )
         CALL CAT_TATTL( II, 'PRFDSP', .FALSE., STATUS )
      END IF

*  Polarisation angle (degrees).
      CALL CAT_CNEWS( CI, 'THETA', CAT__TYPER, 0, 'Degrees', 'F6.2', 
     :                'Polarisation angle', II, STATUS )

      IF( VAR ) THEN
         CALL CAT_CNEWS( CI, 'DTHETA', CAT__TYPER, 0, 'Degrees', 'F6.3', 
     :                   'Standard deviation on polarisation angle',
     :                   II, STATUS )
         CALL CAT_TATTL( II, 'PRFDSP', .FALSE., STATUS )
      END IF

*  Polarised intensity.
      CALL CAT_CNEWS( CI, 'PI', CAT__TYPER, 0, UNITS, 'G13.6', 
     :                'Polarised intensity', II, STATUS )

      IF( VAR ) THEN
         CALL CAT_CNEWS( CI, 'DPI', CAT__TYPER, 0, UNITS, 'G13.6',
     :                   'Standard deviation on polarised intensity',
     :                   II, STATUS )
         CALL CAT_TATTL( II, 'PRFDSP', .FALSE., STATUS )
      END IF

      END
