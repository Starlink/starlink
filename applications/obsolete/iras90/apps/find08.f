      SUBROUTINE FIND08( STATUS )
*+
*  Name:
*     FIND08

*  Purpose:
*     Cleans plate common and associated source pointers

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FIND08( STATUS )

*  Description:
*     Cleans plate common and associated source pointers

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  External Routines Used:
*     None

*  Authors:
*     DCP: Diana Parsons (IPMAF/RAL)
*     {enter_new_authors_here}

*  History:
*     25-FEB-1992 (DCP):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'I90_PAR'          ! IRAS 90 General constants
      INCLUDE 'IRA_PAR'          ! IRAS Astrometry constants
      INCLUDE 'IRA_ERR'          ! IRAS Astrometry errors

*  Global Variables:
      INCLUDE 'FICOMN' ! Common blocks for FINDCRDD

*  Local Variables:
      INTEGER PLPOS              ! Plate do loop variable
      INTEGER SOPOS              ! Source do loop variable

*  Status:
      INTEGER STATUS             ! Global status
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check if any plate common positions have been used
      IF ( NOPLAT .GT. 0 ) THEN
         DO 200 PLPOS = 1, NOPLAT

*  Zeroise each plate
            PLNUM( PLPOS )  = 0
            PLID( PLPOS )   = '      '
            PLPOTA( PLPOS ) = 0
            PLPOLE( PLPOS ) = ' '
            PLLORA( PLPOS ) = 0.0
            PLHIRA( PLPOS ) = 0.0
            PLLODE( PLPOS ) = 0.0
            PLHIDE( PLPOS ) = 0.0
            PLFSCP( PLPOS ) = 0

*  For each source associated with that plate
            DO 100 SOPOS = 1, PLNOSO( PLPOS )

*  Clear the pointer from the plate to the source
               PLSOI( PLPOS,SOPOS) = 0

 100        CONTINUE

*  Zeroise the number of sources associated with the plate
            PLNOSO( PLPOS ) = 0

 200     CONTINUE

*  Zeroise the number of plates used in plate common
         NOPLAT = 0

*  For each source in source common
         DO 300 SOPOS = 1, NOFSO

*  Clear the pointer from the source to the plate
            SOBPLI( SOPOS ) = 0

 300     CONTINUE
      END IF

      END
