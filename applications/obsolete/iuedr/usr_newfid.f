      SUBROUTINE USR_NEWFID( STATUS )
*+
*  Name:
*     SUBROUTINE USR_NEWFID

*  Description:
*     The contents of CMFID are read from a specified file, and the
*     Calibration marked as needing update.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL USR_NEWFID( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     JRG: Jack Giddings (UCL)
*     PCTR: Paul Rees (UCL)
*     MJC: Martin Clayton (UCL)
*     {enter_new_authors_here}

*  History:
*     01-MAY-82 (JRG):
*       IUEDR Vn. 1.0
*     04-NOV-88 (PCTR):
*       IUEDR Vn. 2.0
*     17-OCT-94 (MJC):
*       IUEDR Vn. 3.1-7
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
      INTEGER STATUS   ! Global status.

*  Global Variables:
      INCLUDE 'CMHEAD'
      INCLUDE 'CMDATA'
      INCLUDE 'CMFIDS'
      INCLUDE 'CMFIDT'
      INCLUDE 'CMGEOM'

*  Local Variables:
      INTEGER ACTVAL   ! Parameter value count.
      INTEGER DATA_VM  ! VM address for DATA array.
      INTEGER NAXIS1   ! Size of axis 1.
      INTEGER NAXIS2   ! Size of axis 2.
      INTEGER NGEOM    ! Polynomial order for geometric correction.
      INTEGER QUAL_VM  ! VM address for QUAL array.

      BYTE FILE( 81 )  ! File name.
*.

*   Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*   Get Calibration and Image.
      CALL DASSOC( 'I\\', 'T\\', STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: could not access dataset\\', STATUS )
         GO TO 999
      END IF

*   Map image.
      CALL MRDATA( NAXIS1, NAXIS2, DATA_VM, QUAL_VM, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: unable to access data\\', STATUS )
         GO TO 999
      END IF

*   FIDFILE.
      CALL RDPARC( 'FIDFILE\\', .FALSE., 81, FILE, ACTVAL, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL PARFER( 'FIDFILE\\', STATUS )
         GO TO 999

      ELSE
         CALL CNPAR( 'FIDFILE\\', STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL PCANER( 'FIDFILE\\', STATUS )
            GO TO 999
         END IF
      END IF

*   NGEOM.
      IF ( .NOT. GEOM ) THEN
         CALL RDPARI( 'NGEOM\\', .FALSE., 1, NGEOM, ACTVAL, STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL PARFER( 'NGEOM\\', STATUS )
            GO TO 999

         ELSE IF ( NGEOM.LT.2 .OR. NGEOM.GT.6 ) THEN
            CALL ERRPAR( 'NGEOM\\' )
            CALL ERROUT( ': has bad value\\', STATUS )
            GO TO 999
         END IF
      END IF

*   Whatever happens, the calibration file will require update.
      CALL MODCAL

*   Read file.
      NOFIDS = .TRUE.
      NOFIDT = .TRUE.
      NOGEOM = .TRUE.

      CALL RFFID( FILE, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: replacing fiducial data\\', STATUS )
         GO TO 999
      END IF

*   Move fiducials to account for THDA variations.
      CALL MVFIDT( 1, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: moving fiducials with THDA\\', STATUS )
         GO TO 999
      END IF

*   Mark pixels affected by fiducials.
      CALL MKFIDS( 1, .FALSE., NS, NL, %VAL( DATA_VM ), %VAL( QUAL_VM ),
     :             STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: marking fiducials\\', STATUS )
         GO TO 999
      END IF

*   Re-compute geometry.
      NGTERM( 1 ) = NGEOM
      NGTERM( 2 ) = NGEOM
      CALL GEOMF( 1, STATUS )

 999  CONTINUE

      END
