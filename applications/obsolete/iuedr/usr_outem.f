      SUBROUTINE USR_OUTEM( STATUS )
*+
*  Name:
*     SUBROUTINE USR_OUTEM

*  Purpose:
*     Output template data to a file.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL USR_OUTEM( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     The current set of template data stored in CMTEM is output to a file
*     so that it can be used for other images.
*     For HIRES the default TEMFILE parameter is composed of
*     CAMERA/RESOLUTION/APERTURE; while for LORES, CAMERA/RESOLUTION
*     are used.

*  Authors:
*     JRG: Jack Giddings (UCL)
*     PCTR: Paul Rees (UCL)
*     MJC: Martin Clayton (UCL)
*     {enter_new_authors_here}

*  History:
*     01-AUG-82 (JRG):
*       IUEDR Vn. 1.0
*     05-NOV-88 (PCTR):
*       IUEDR Vn. 2.0
*     03-OCT-94 (MJC):
*       IUEDR Vn. 3.1-6
*     19-JAN-95 (MJC):
*       IUEDR Vn. 3.2
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'

*  Status:
      INTEGER STATUS     ! Global status.

*  External References:
      LOGICAL STR_SIMLR  ! Caseless string equality.

*  Global Variables:
      INCLUDE 'CMHEAD'
      INCLUDE 'CMTEM'

*  Local Variables:
      BYTE FILE( 81 )    ! File name.

      INTEGER ACTVAL     ! Parameter value count.
      INTEGER FD         ! File I/O unit.
      INTEGER FIOSTAT
*.

*   Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*   Get Calibration.
      CALL DASSOC( '\\', '\\', STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: could not access dataset\\', STATUS )
         GO TO 999

      ELSE IF ( NOTEM ) THEN
         CALL ERROUT( 'Error: no template data\\', STATUS )
         GO TO 999
      END IF

*   TEMFILE.
      CALL STR_MOVE( CAMERA, 81, FILE )
      IF ( STR_SIMLR( RESOL, 'HIRES\\' ) ) THEN
         CALL STR_APPND( 'HI\\', 81, FILE )
         CALL STR_APPND( APER, 81, FILE )

      ELSE
         CALL STR_APPND( 'LO\\', 81, FILE )
      END IF

      CALL STR_APPND( '.TEM\\', 81, FILE )
      CALL RDPARC( 'TEMFILE\\', .TRUE., 81, FILE, ACTVAL, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL PARFER( 'TEMFILE\\', STATUS )
         GO TO 999

      ELSE
         CALL CNPAR( 'TEMFILE\\', STATUS )
         IF ( STATUS .NE. SAI__OK) THEN
            CALL PCANER( 'TEMFILE\\', STATUS )
            GO TO 999
         END IF
      END IF

*   Create SPECTRUM file.
      CALL FIO_GUNIT( FD, STATUS )
      CALL WRTEM( FD, FILE, STATUS )
      FIOSTAT = SAI__OK
      CALL FIO_PUNIT( FD, FIOSTAT )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Could not create template data file\\', STATUS )
      END IF

 999  CONTINUE

      END
