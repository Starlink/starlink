      SUBROUTINE FIND09( STATUS )
*+
*  Name:
*     FIND09

*  Purpose:
*     Cleans scan common and associated source pointers

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FIND09( STATUS )

*  Description:
*     Cleans scan common and associated source pointers

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  External Routines Used:
*     None

*  Authors:
*     DCP: Diana Parsons (IPMAF/RAL)
*     {enter_new_authors_here}

*  History:
*     21-APR-1992 (DCP):
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
      INTEGER SCPOS              ! Scan do loop variable
      INTEGER SOPOS              ! Source do loop variable

*  Status:
      INTEGER STATUS             ! Global status
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check if any scan common positions have been used
      IF ( NOSCAN .GT. 0 ) THEN
         DO 100 SCPOS = 1, NOSCAN

*  Zeroise each scan
            SCSOI( SCPOS )  = 0
            SCSOLI( SCPOS ) = 0
            SCXSC( SCPOS )  = 0.0
            SCSOP( SCPOS )  = 0
            SCOBS( SCPOS )  = 0
            SCSTUT( SCPOS ) = 0.0
            SCENUT( SCPOS ) = 0.0
            SCOVFL( SCPOS ) = .FALSE.
            SCRQFL( SCPOS ) = .TRUE.
            SCTH( SCPOS )   = 0.0
            SCSTPS( SCPOS ) = 0.0
            SCENPS( SCPOS ) = 0.0
            SCNPPS( SCPOS ) = 0.0
            SCSTSL( SCPOS ) = 0.0
            SCENSL( SCPOS ) = 0.0
            SCNPSL( SCPOS ) = 0.0
            SCSTST( SCPOS ) = 0.0
            SCENST( SCPOS ) = 0.0
            SCNPST( SCPOS ) = 0.0
            SCBLSZ( SCPOS ) = 0
            SCNSCP( SCPOS ) = 0

 100     CONTINUE

*  Zeroise the number of scans used in scan common
         NOSCAN= 0

*  For each source in source common
         DO 300 SOPOS = 1, NOFSO

*  For each scan associated with that source
            DO 200 SCPOS = 1, SONOSC( SOPOS )

*  Clear the pointer from the source to the scan
               SOSCI( SOPOS, SCPOS ) = 0

 200        CONTINUE

*  Clear the number of scans associated with the source
            SONOSC( SOPOS ) = 0

 300     CONTINUE
      END IF

      END
