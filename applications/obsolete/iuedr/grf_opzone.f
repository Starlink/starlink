      SUBROUTINE GRF_OPZONE( STATUS )
*+
*  Name:
*     SUBROUTINE GRF_OPZONE

*  Purpose:
*     Open display zone for plotting.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL GRF_OPZONE( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Method:
*     The display device parameter is read. If it differs from the
*     current one, then a new device is initialised. The zone is also
*     reset to 0.

*  Authors:
*     JRG: Jack Giddings (UCL)
*     PCTR: Paul Rees (UCL)
*     MJC: Martin Clayton (UCL)
*     {enter_new_authors_here}

*  History:
*     05-MAY-82 (JRG):
*       AT4 version.
*     06-JAN-88 (PCTR):
*       IUEDR Vn. 2.0
*       Conversion to FORTRAN.
*       Conversion to GKS 7.2 graphics.
*     08-MAY-89 (PCTR):
*       IUEDR Vn. 2.1
*       Some restructuring and final conversion to SGP/16 style.
*     27-JUL-94 (MJC):
*       IUEDR Vn. 3.1-2
*       SAEised.
*     06-JAN-95 (MJC):
*       IUEDR Vn. 3.2
*       Added check for ZONE in range.
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
      INTEGER ERR         ! Error status.
      INTEGER MAXNAME     ! Maximum length of name string.
      INTEGER STDCMD      ! Command logical unit.
      PARAMETER ( ERR = -3, MAXNAME = 16, STDCMD = 6 )

*  Status:
      INTEGER STATUS      ! Global status.

*  External References:
      LOGICAL STR_SIMLR   ! Caseless string equality.

*  Local Variables:
      BYTE DVNEW( MAXNAME )        ! Device parameter string.

      CHARACTER DEVICE*( MAXNAME ) ! Device character array.

      INTEGER ZNEW                 ! Zone parameter.
      INTEGER ACTVAL               ! Parameter value count.
      INTEGER ISTAT                ! Internal status.
      INTEGER NCHAR                ! Character count.
*.

*   Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*   DEVICE.
      DO WHILE ( .TRUE. )
          ISTAT = PAR__NULL
         DO WHILE ( ISTAT .EQ. PAR__NULL )
            ISTAT = SAI__OK

*         Read DEVICE name.
            CALL RDPARC( 'DEVICE\\', .FALSE., MAXNAME, DVNEW, ACTVAL,
     :                   ISTAT )
            IF ( ISTAT .EQ. PAR__NULL ) THEN
               CALL ERR_FLUSH( ISTAT )
               ISTAT = PAR__NULL
               CALL line_WCONT( '%pAvailable SGS devices are:\\' )
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
            CALL PARFER( 'DEVICE\\', STATUS)
            GO TO 999
         END IF

*      Read ZONE number.
         DO WHILE ( .TRUE. )
            CALL RDPARI( 'ZONE\\', .FALSE., 1, ZNEW, ACTVAL, STATUS )
            IF ( STATUS .EQ. PAR__NULL ) THEN
               CALL ERR_FLUSH( STATUS )
               CALL LINE_WRITS( '%p!  ZONE will take the value 0\\' )
               CALL PRTBUF( STATUS )
               ZNEW = 0

               CALL WRPARI( 'ZONE\\', 1, ZNEW, STATUS )
               IF ( STATUS .NE. SAI__OK ) THEN
                  CALL ERRPAR( 'ZONE\\' )
                  CALL ERROUT( ': parameter write error\\', STATUS )
                  GO TO 999
               END IF
               GO TO 100

            ELSE IF ( STATUS .NE. SAI__OK ) THEN
               CALL PARFER( 'ZONE\\', STATUS )
               GO TO 999

            ELSE IF ( ZNEW.LT.0 .OR. ZNEW.GT.8 ) THEN
               CALL ERRPAR( 'ZONE\\' )
               CALL ERROUT( ': out of range\\', STATUS )

            ELSE
               GO TO 100
            END IF

            CALL CNPAR( 'ZONE\\', STATUS )
            IF ( STATUS .NE. SAI__OK ) THEN
               CALL PCANER( 'ZONE\\', STATUS )
               GO TO 999
            END IF
         END DO
 100     CONTINUE

*      If parameters have been read in OK, then proceed.
*      Check for same device name
         IF ( .NOT. STR_SIMLR( DVNEW, DVNAME ) ) THEN

*         New DEVICE - load character array DEVICE
            CALL GEN_STOC( DVNEW, MAXNAME, DEVICE, NCHAR )

*         Initialise the new device
            CALL GRF_ZINIT( DEVICE, DEV, ZBASE, STATUS )
            IF ( STATUS .NE. SAI__OK ) THEN
               CALL CNPAR( 'DEVICE\\', STATUS )
               CALL ERR_FLUSH( STATUS )
               CALL ERRPAR( 'DEVICE\\' )
               CALL ERROUT( ': unavailable\\', STATUS )
               CALL STR_TERM( 0, MAXNAME, DVNAME )
               GO TO 999

            ELSE

*            Device exists and all is well
               CALL str_MOVE( DVNEW, MAXNAME, DVNAME )

*            Open ZONE
               CALL grf_TZONE( ZBASE, ZNEW, ZCLEAR, STATUS )
               IF ( STATUS .NE. SAI__OK ) THEN
                  CALL SGS_CLSWK( ZBASE, STATUS )
                  CALL CNPAR( 'ZONE\\', STATUS )
                  CALL ERRPAR( 'ZONE\\' )
                  CALL ERROUT( ': initialisation error', STATUS )
                  GO TO 999

               ELSE

*               Reset colour table.
                  CALL grf_RSCOLR
                  NEWLUT = .FALSE.

*               Reset plot flags.
                  ERASED = .FALSE.
                  DRAWN = .FALSE.

*               Mark new zone number.
                  ZONE = ZNEW
                  GO TO 999
               END IF
            END IF

*      Requested workstation is current.
         ELSE

*         Open ZONE.
            CALL GRF_TZONE( ZBASE, ZNEW, ZCLEAR, STATUS )
            IF ( STATUS .NE. SAI__OK ) THEN
               CALL SGS_CLSWK( ZBASE, STATUS )
               CALL CNPAR( 'ZONE\\', STATUS )
               CALL ERRPAR( 'ZONE\\' )
               CALL ERROUT( ': initialisation error', STATUS )
               GO TO 999

            ELSE

*            Reset plot flags
               ERASED = .FALSE.
               IF ( ZONE .NE. ZNEW ) THEN
                  DRAWN = .FALSE.
               END IF

*            Mark new zone number.
               ZONE = ZNEW
               GO TO 999
            END IF
         END IF
      END DO

 999  CONTINUE

      END
