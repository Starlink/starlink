      SUBROUTINE GRF_OPIDEV( STATUS )
*+
*  Name:
*     SUBROUTINE GRF_OPIDEV

*  Purpose:
*     Open image display device.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL GRF_OPIDEV( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     JRG: Jack Giddings (UCL)
*     PCTR: Paul Rees (UCL)
*     MJC: Martin Clayton (UCL)
*     {enter_new_authors_here}

*  History:
*     01-AUG-82 (JRG):
*       AT4 version.
*     28-NOV-88 (PCTR):
*       IUEDR Vn. 2.0
*       Conversion to FORTRAN.
*       Conversion to GKS 7.2 graphics.
*     09-MAY-89 (PCTR):
*       IUEDR Vn. 2.1
*       Some restructuring and final conversion to SGP/16 style.
*     26-JUL-94 (MJC):
*       IUEDR Vn. 3.1-2
*       Fixed support for available device listing under ADAM.
*       Don't miss-use STATUS parameter in RDPAR* calls.
*     30-JAN-95 (MJC):
*       IUEDR Vn. 3.2
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'PAR_ERR'

*  Global Variables:
      INCLUDE 'CMGRAF'

*  Local Constants:
      INTEGER ERR             ! Error status code.
      INTEGER MAXCOLI         ! Maximum number of preset colours.
      INTEGER MAXNAME         ! Maximum length of name string.
      INTEGER STDCMD          ! Command logical unit.
      PARAMETER ( ERR = -3, MAXCOLI = 256, MAXNAME = 16, STDCMD = 6 )

*  Status:
      INTEGER STATUS          ! Global status.

*  External References:
      LOGICAL STR_SIMLR	      ! Caseless string equality.

*  Local Variables:
      BYTE IDVNAM( MAXNAME )  ! Image display device name.

      CHARACTER DEVICE*( MAXNAME ) ! SGS device name.

      INTEGER ACTVAL          ! Parameter value count.
      INTEGER COLAV           ! GKS colour availability.
      INTEGER GFSTAT          ! GKS status.
      INTEGER ICONID          ! GKS connection id.
      INTEGER ISTAT           ! Subroutine internal status.
      INTEGER NCHAR           ! Character count.
      INTEGER NCOLI           ! GKS number of colours available.
      INTEGER NPCOLI          ! GKS number of preset colours available.
      INTEGER WTYPE           ! GKS workstation type.
*.

*   Get display DEVICE.
      ISTAT = PAR__NULL
      DO WHILE ( ISTAT .EQ. PAR__NULL )
         ISTAT = SAI__OK
         CALL RDPARC( 'DEVICE\\', .FALSE., MAXNAME, IDVNAM, ACTVAL,
     :                ISTAT )
         IF ( ISTAT .EQ. PAR__NULL ) THEN
            CALL ERR_FLUSH( ISTAT )
            ISTAT = PAR__NULL
            CALL LINE_WCONT( '%pAvailable SGS devices are:\\' )
            CALL PRTBUF( STATUS )
            CALL SGS_WLIST( STDCMD )
            CALL CNPAR( 'DEVICE\\', STATUS )
            IF ( STATUS .NE. SAI__OK ) THEN
               CALL PCANER( 'DEVICE\\', STATUS )
               GO TO 999
            END IF
         END IF
      END DO

      IF ( ISTAT .NE. SAI__OK ) THEN
         CALL PARFER( 'DEVICE\\', STATUS )
         GO TO 999
      END IF

*   Load character array DEVICE.
      CALL GEN_STOC( IDVNAM, MAXNAME, DEVICE, NCHAR )

*   Check that device exists.
      CALL SGS_WIDEN( DEVICE, WTYPE, ICONID, GFSTAT )
      IF ( GFSTAT .NE. SAI__OK ) THEN
         CALL CNPAR( 'DEVICE\\', STATUS )
         CALL ERR_FLUSH( GFSTAT )
         CALL ERRPAR( 'DEVICE\\' )
         CALL ERROUT( ': unavailable\\', STATUS )
         CALL STR_TERM( 0, MAXNAME, IDVNAM )
         GO TO 999

*   Check that device is an image display.
      ELSE
         CALL GQCF( WTYPE, GFSTAT, NCOLI, COLAV, NPCOLI )
         IF ( COLAV .NE. 1 ) THEN
            CALL ERRPAR( 'DEVICE\\' )
            CALL ERROUT( ': not an image display device\\', STATUS )
            GO TO 999
         END IF
      END IF

*   Check that it is not the current device
      IF ( .NOT. STR_SIMLR( IDVNAM, DVNAME ) ) THEN

*      Initialise new device.
         CALL GRF_ZINIT( DEVICE, DEV, ZBASE, STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL CNPAR( 'DEVICE\\', STATUS )
            CALL ERRPAR( 'DEVICE\\' )
            CALL ERROUT( ': unavailable\\', STATUS )
            CALL STR_TERM( 0, MAXNAME, IDVNAM )
         ELSE

*         Successful initialisation - load DVNAME.
            CALL STR_MOVE( IDVNAM, MAXNAME, DVNAME )
         END IF
      END IF

*   Reset colour table if required.
      IF ( NEWLUT ) THEN
         CALL GRF_RSCOLR
         NEWLUT = .FALSE.
      END IF

*   Reset plotting flags.
      ZCLEAR = .TRUE.
      ERASED = .FALSE.
      DRAWN = .FALSE.

 999  CONTINUE

      END
