      SUBROUTINE RDTEM( FD, IRES, STATUS )
*+
*  Name:
*     SUBROUTINE RDTEM

*  Description:
*     The CMTEM contents are read.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL RDTEM( FD, IRES, STATUS )

*  Arguments:
*     FD = INTEGER (Given)
*        File descriptor of open template file.
*     IRES = INTEGER (Given)
*        Resolution mode required from file.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     JRG: Jack Giddings (UCL)
*     PCTR: Paul Rees (UCL)
*     MJC: Martin Clayton (UCL)
*     {enter_new_authors_here}

*  History:
*     01-MAY-82 (JRG):
*       AT4 version
*     06-OCT-88 (PCTR):
*       IUEDR Vn. 2.0
*     08-OCT-94 (MJC):
*       IUEDR Vn. 3.1-6
*     25-JAN-95 (MJC):
*       IUEDR Vn. 3.2
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'

*  Local Constants:
      INTEGER HIRES         ! HIRES index.
      INTEGER LORES         ! LORES index.
      INTEGER MAXNAME       ! Maximum name length.
      INTEGER MAXORD        ! Maximum number of orders.
      INTEGER MAXTEM        ! Maximum number of template points.
      PARAMETER ( HIRES = 2, LORES = 1, MAXNAME = 16,
     :            MAXORD = 100, MAXTEM = 100 )

*  Arguments Given:
      INTEGER FD            ! File descriptor.
      INTEGER IRES          ! Required resolution mode.

*  Status:
      INTEGER STATUS        ! Global status.

*  External References:
      LOGICAL STR_SIMLR     ! Caseless string equality.

*  Global Variables:
      INCLUDE 'CMTEM'

*  Local Variables:
      BYTE APR( MAXNAME )   ! Aperture string.
      BYTE TEMP( MAXNAME )  ! Template type.

      CHARACTER*( MAXNAME ) CAPR  ! Aperture character string.
      CHARACTER*( MAXNAME ) CTEMP ! CHARACTER template type.

      INTEGER I             ! Loop index.
      INTEGER J             ! Loop index.
      INTEGER NCHAR         ! Character count.
*.

*   Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      NOTEM = .TRUE.

*   Get resolution type from file and check.
      READ ( FD, *, IOSTAT = STATUS ) CTEMP
      CALL GEN_CTOS( CTEMP, MAXNAME, TEMP, NCHAR )
      CALL STR_RMBLK( TEMP )
      IF ( STATUS .NE. SAI__OK ) THEN
         GO TO 998

      ELSE IF ( IRES.EQ.HIRES .AND.
     :          .NOT.STR_SIMLR( 'HIRES\\', TEMP ) ) THEN
         CALL ERROUT( 'Error: template resolution wrong\\', STATUS )
         GO TO 998

      ELSE IF ( IRES.EQ.LORES .AND.
     :          .NOT.STR_SIMLR( 'LORES\\', TEMP ) ) THEN
         CALL ERROUT( 'Error: template resolution wrong\\', STATUS )
         GO TO 998
      END IF

*   Get number of orders contained in file.
      READ ( FD, *, IOSTAT = STATUS ) NTEMO
      IF ( STATUS .NE. SAI__OK ) THEN
         GO TO 998

      ELSE IF ( NTEMO .GT. MAXORD ) THEN
         CALL ERROUT( 'Error: too many templates\\', STATUS )
         GO TO 998

      ELSE IF ( NTEMO .GT. 0 ) THEN
         DO I = 1, NTEMO
            IF ( IRES .EQ. LORES ) THEN
               READ ( FD, *, IOSTAT = STATUS )
     :                CAPR, NTEMS( I ), TEMW0( I ), TEMDW( I )

            ELSE IF ( IRES .EQ. HIRES ) THEN
               READ ( FD, *, IOSTAT = STATUS )
     :                TEMORD( I ), NTEMS( I ), TEMW0( I ), TEMDW( I )
            END IF
            IF ( STATUS .NE. SAI__OK ) THEN
               GO TO 998

            ELSE IF ( NTEMS( I ).LT.0 .OR. NTEMS( I ).GT.MAXTEM ) THEN
               CALL ERROUT( 'Error: template has illegal size\\',
     :                      STATUS )
               GO TO 998
            END IF

            IF ( IRES .EQ. LORES ) THEN
               CALL GEN_CTOS( CAPR, MAXNAME, APR, NCHAR )
               CALL STR_RMBLK( APR )
               CALL FNAPER( APR, TEMORD( I ) )
            END IF

            READ ( FD, *, IOSTAT = STATUS )
     :             ( TEMCEN( J, I ), J = 1, NTEMS( I ) )
            IF ( STATUS .NE. SAI__OK ) THEN
               GO TO 998
            END IF
         END DO
         NOTEM = .FALSE.
      END IF

      GO TO 999

  998 CONTINUE

      STATUS = SAI__OK
      CALL ERROUT( 'Error: reading template file\\', STATUS )

  999 CONTINUE

      END
