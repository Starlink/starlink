      SUBROUTINE USR_SETD( STATUS )
*+
*  Name:
*     SUBROUTINE USR_SETD

*  Purpose:
*     Set values that are global the the dataset calibration.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL USR_SETD( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Method:
*     Read parameters, providing dataset values as dynamic defaults.
*     Since the resolution path is (CURRENT,DYNAMIC), any values
*     set by the used will provide "different" values from the
*     dynamic defaults.
*     The old and new parameters are compared to see if they have
*     really changed, and whether the dataset calibration needs
*     update in any way.
*     Retain swicthes for file update, and current spectrum calibration
*     update.
*
*     When some additional data (beyond UEC file) is needed, call DMORE
*     to satisfy need.

*  Authors:
*     JRG: Jack Giddings (UCL)
*     PCTR: Paul Rees (UCL)
*     MJC: Martin Clayton (UCL)
*     {enter_new_authors_here}

*  History:
*     01-MAY-82 (JRG):
*       IUEDR Vn. 1.0
*     07-NOV-88 (PCTR):
*       IUEDR Vn. 2.0
*     27-JUL-94 (MJC):
*       IUEDR Vn. 3.1-2
*     18-JAN-95 (MJC):
*       IUEDR Vn. 3.2
*     25-APR-95 (MJC):
*       Modified OBJECT part so that a change-of-case leads to a UEC update.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'

*  Status:
      INTEGER STATUS      ! Global status.

*  External References:
      LOGICAL STR_EQUAL   ! String equality.
      LOGICAL STR_SIMLR   ! Caseless string equality.

*  Global Variables:
      INCLUDE 'CMHEAD'
      INCLUDE 'CMDATA'
      INCLUDE 'CMABS'
      INCLUDE 'CMFIDS'
      INCLUDE 'CMFIDT'
      INCLUDE 'CMGEOM'
      INCLUDE 'CMITFC'

*  Local Constants:
      INTEGER MAXOBJECT   ! Maximum length of OBJECT string.
      PARAMETER ( MAXOBJECT = 40 )

*  Local Variables:
      REAL*8 FVAL( 32 )   ! Float temporary.

      LOGICAL LVAL( 32 )  ! Logical temporary.
      LOGICAL NEWCAL      ! Whether current spectrum calibration changes.
      LOGICAL NEWFID      ! Whether fiducials have changed.
      LOGICAL NEWFIL      ! Whether calibration file requires update.
      LOGICAL NEWGEO      ! Whether geometry needs change.

      BYTE SVAL( MAXOBJECT ) ! String temporary.

      INTEGER ACTVAL      ! Parameter value count.
      INTEGER DATA_VM     ! Data array VM.
      INTEGER ISTAT       ! Status.
      INTEGER IVAL( 32 )  ! Integer temporary.
      INTEGER NAXIS1      ! Axis1 size.
      INTEGER NAXIS2      ! Axis2 size.
      INTEGER QUAL_VM     ! Image quality data pointer.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get Calibration.
      CALL DASSOC( '\\', '\\', STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: could not access dataset\\', STATUS )
         GO TO 999
      END IF

*  Set switches.
      NEWFIL = .FALSE.
      NEWCAL = .FALSE.
      NEWFID = .FALSE.
      NEWGEO = .FALSE.

*  OBJECT - object title.
      DO WHILE ( .TRUE. )
         CALL STR_MOVE( TITLE, MAXOBJECT, SVAL )
         CALL RDPARC( 'OBJECT\\', .TRUE., MAXOBJECT, SVAL, ACTVAL,
     :                STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL PARFER( 'OBJECT\\', STATUS )
            GO TO 999

         ELSE IF ( .NOT. STR_EQUAL( SVAL, TITLE ) ) THEN
            CALL STR_MOVE( SVAL, MAXOBJECT, TITLE )
            NEWFIL = .TRUE.
         END IF

         CALL CNPAR( 'OBJECT\\', ISTAT )
         IF ( ISTAT .NE. SAI__OK ) THEN
            CALL PCANER( 'OBJECT\\', STATUS )
            GO TO 999

         ELSE IF ( STATUS .EQ. SAI__OK ) THEN
            GO TO 100
         END IF
      END DO
 100  CONTINUE

*  THDA - camera temperature.
      DO WHILE ( .TRUE. )
         FVAL( 1 ) = THDA
         CALL RDPARF( 'THDA\\', .TRUE., 1, FVAL, ACTVAL, STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL PARFER( 'THDA\\', STATUS )
            GO TO 999

         ELSE IF ( FVAL( 1 ) .NE. THDA ) THEN
            THDA = FVAL( 1 )
            NEWFIL = .TRUE.
            IF ( .NOT. NOABS ) THEN
               IF ( TSEN( 2 ) .NE. 0.0 ) NEWCAL = .TRUE.
            END IF
            IF ( .NOT. NOFIDT ) THEN
               NEWFID = .TRUE.
               NEWGEO = .TRUE.
            END IF
         END IF

         CALL CNPAR( 'THDA\\', ISTAT )
         IF ( ISTAT .NE. SAI__OK ) THEN
            CALL PCANER( 'THDA\\', STATUS )
            GO TO 999

         ELSE IF ( STATUS .EQ. SAI__OK ) THEN
            GO TO 200
         END IF
      END DO
 200  CONTINUE

*  FIDSIZE - fiducial half width.
      DO WHILE ( .NOT. NOFIDS )
         FVAL( 1 ) = FIDHW
         CALL RDPARF( 'FIDSIZE\\', .TRUE., 1, FVAL, ACTVAL, STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL PARFER( 'FIDSIZE\\', STATUS )
            GO TO 999

         ELSE IF ( FVAL( 1 ) .NE. FIDHW ) THEN
            FIDHW = FVAL( 1 )
            NEWFIL = .TRUE.
            NEWFID = .TRUE.
         END IF

         CALL CNPAR( 'FIDSIZE\\', ISTAT )
         IF ( ISTAT .NE. SAI__OK ) THEN
            CALL PCANER( 'FIDSIZE\\', STATUS )
            GO TO 999

         ELSE IF ( STATUS .EQ. SAI__OK ) THEN
            GO TO 300
         END IF
      END DO
 300  CONTINUE

      DO WHILE ( PHOT .AND. STR_SIMLR( 'LORES\\', RESOL ) )
         LVAL( 1 ) = ( .NOT. NOITFC )
         CALL RDPARL( 'BADITF\\', .TRUE., 1, LVAL, ACTVAL, STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL PARFER( 'BADITF\\', STATUS )
            GO TO 999

         ELSE IF ( LVAL( 1 ) .NEQV. .NOT.NOITFC ) THEN
            NOITFC = ( .NOT. LVAL( 1 ) )
            NEWFIL = .TRUE.
         END IF

         CALL CNPAR( 'BADITF\\', ISTAT )
         IF ( ISTAT .NE. SAI__OK ) THEN
            CALL PCANER( 'BADITF\\', STATUS )
            GO TO 999

         ELSE IF ( STATUS .EQ. SAI__OK ) THEN
            GO TO 400
         END IF
      END DO
 400  CONTINUE

*  NGEOM - camera temperature.
      DO WHILE ( .NOT. GEOM )
         IVAL( 1 ) = NGTERM( 1 )
         CALL RDPARI( 'NGEOM\\', .TRUE., 1, IVAL, ACTVAL, STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL PARFER( 'NGEOM\\', STATUS )
            GO TO 999

         ELSE IF ( IVAL( 1 ) .NE. NGTERM( 1 ) ) THEN
            NGTERM( 1 ) = IVAL( 1 )
            NGTERM( 2 ) = IVAL( 1 )
            NEWFIL = .TRUE.
            NEWGEO = .TRUE.
         END IF

         CALL CNPAR( 'NGEOM\\', ISTAT )
         IF ( ISTAT .NE. SAI__OK ) THEN
            CALL PCANER( 'NGEOM\\', STATUS )
            GO TO 999

         ELSE IF ( STATUS .EQ. SAI__OK ) THEN
            GO TO 500
         END IF
      END DO
 500  CONTINUE

*  HIRES.
      IF ( STR_SIMLR( 'HIRES\\', RESOL ) ) THEN

*     RIPPLE.
         CALL SETDR( NEWFIL, NEWCAL, STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            GO TO 999
         END IF

*     HALATION.
         CALL SETDH( NEWFIL, NEWCAL, STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            GO TO 999
         END IF
      END IF

*  Fiducial change?
      IF ( NEWFID ) THEN
         CALL DMORE( 'I\\', 'T\\', STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERROUT( 'Error: data not available\\', STATUS )
            GO TO 999

         ELSE IF ( NODATA ) THEN
            CALL ERROUT( 'Error: no data\\', STATUS )
            GO TO 999
         END IF

         CALL MVFIDT( 1, STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERROUT( 'Error: moving fiducials with THDA\\',
     :                   STATUS )
            GO TO 999
         END IF

*     Map data.
         CALL MRDATA( NAXIS1, NAXIS2, DATA_VM, QUAL_VM, STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERROUT( 'Error: accessing dataset\\', STATUS )
            GO TO 999
         END IF

         CALL MKFIDS( 1, .FALSE., NS, NL, %VAL( DATA_VM ),
     :                %VAL( QUAL_VM ), STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERROUT( 'Error: marking fiducial pixels\\', STATUS )
            GO TO 999
         END IF
      END IF

*  Geometry change?
      IF ( NEWGEO ) THEN
         CALL GEOMF( 1, STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERROUT( 'Error: with geometry representation\\',
     :                   STATUS )
            GO TO 999
         END IF
      END IF

*  Only calibrate if new.
      IF ( NEWCAL ) THEN
         CALL MODCAL
         CALL RECAL( STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERROUT( 'Error: recalibrating spectrum\\', STATUS )
            GO TO 999
         END IF

      ELSE IF ( NEWFIL ) THEN
         CALL MODCAL
      END IF

 999  CONTINUE
      END
