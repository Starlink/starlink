      SUBROUTINE RDABS( FD, STATUS )
*+
*  Name:
*     SUBROUTINE RDABS

*  Purpose:
*     Read contents of CMABS common blocks.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL RDABS( FD, STATUS )

*  Arguments:
*     FD = INTEGER (Given)
*        I/O unit of open file for read.
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
*       IUEDR Vn. 1.4
*       First pass.
*     07-DEC-88 (PCTR):
*       IUEDR Vn. 1.4
*       Final modifications.
*     31-MAR-89 (PCTR):
*       IUEDR Vn. 2.0
*       Bohlin and Grillmair (1988)
*     03-OCT-94 (MJC):
*       IUEDR Vn. 3.1-6
*     30-JAN-95 (MJC):
*       IUEDR Vn. 3.2
*       IOSTAT variable added to prevent spurious errors.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'

*  Global Variables:
      INCLUDE 'CMABS'
      INCLUDE 'CMHEAD'
      INCLUDE 'CMTEMP'

*  Local Constants:
      INTEGER BOHLIN     ! Non-linear date sensitivity correction.
      INTEGER LAPCOR     ! LAP date correction index.
      INTEGER LINEAR     ! Linear date sensitivity correction.
      INTEGER MAXABS     ! Maximum absolute calibration length.
      INTEGER MAXCASE    ! Maximum number of cases for date sensitivitity.
      INTEGER MAXDATE    ! Maximum number of dates for date sensitivity.
      INTEGER MAXLABEL   ! Maximum label length.
      INTEGER MAXNAME    ! Maximum name length.
      INTEGER MAXPOINT   ! Maximum number of points in spectrum.
      INTEGER NONE       ! No date sensitivity data.
      INTEGER OK         ! OK status.
      INTEGER SAPCOR     ! SAP date correction index.
      INTEGER TRACOR     ! TRAILED date correction index.
      PARAMETER ( BOHLIN = 2, LINEAR = 1, LAPCOR = 1,
     :            MAXABS = 1200, MAXCASE = 3, MAXDATE = 12,
     :            MAXLABEL = 40, MAXNAME = 16, MAXPOINT = 27800,
     :            NONE = 0, OK = 0, SAPCOR = 2, TRACOR = 3 )

*  Arguments Given:
      INTEGER FD                 ! File descriptor.

*  Status:
      INTEGER STATUS             ! Global status.

*  External References:
      LOGICAL STR_SIMLR          ! Case independent string match.

*  Local Variables:
      REAL*8 DATCOR              ! Linear interpolation factor.
      REAL*8 DATES( MAXCASE, MAXDATE ) ! Bohlin date fiducials.
      REAL*8 DATE1               ! Earlier date for interpolation.
      REAL*8 DATE2               ! Later date for interpolation.
      REAL*8 DSTABL( MAXCASE, MAXDATE, MAXABS)
                                 ! Bohlin date sensitivity table.
      REAL*8 DSWAV( MAXCASE, MAXABS) ! Bohlin date wavelengths.
      REAL*8 DYEARS              ! Julian years since 1978.8.
      REAL*8 FAC                 ! Calibration scaling factor.
      REAL*8 FY( MAXPOINT )      ! Interpolation array.
      REAL*8 YEARS               ! Decimal years A.D.

      CHARACTER CTYPE*( MAXNAME ) ! F77 type string.
      CHARACTER CID*( MAXLABEL ) ! F77 id string.

      INTEGER DSMOD              ! Date sensitivity mode (data format).
      INTEGER I                  ! Loop index.
      INTEGER IDATE              ! Date index.
      INTEGER J                  ! Loop index.
      INTEGER NCASE              ! Case (aperture of trailed) for dates.
      INTEGER NCHAR              ! Character count.
      INTEGER NDATE( MAXCASE )   ! Number of dates in date sensitivity.
      INTEGER NWDATE( MAXCASE )  ! Number of wavelengths in date sensitivity.
      INTEGER IOSTAT             ! File access status.
*.

*   Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      IOSTAT = SAI__OK

*   Initialise NOABS, NABS, NAPER, NTRAIL, NDSEN.
      NOABS = .TRUE.
      NABS = 0
      NAPSEN = 0
      NTRSEN = 0
      NDSEN = 0

*   Years A.D. and elapsed since 1978.8 (assumed Julian Years).
      IF ( DATE .GE. 0.0 ) THEN
         YEARS = 1978.0 + DATE / 365.25
         DYEARS = ( DATE / 365.25 ) - 0.8

      ELSE
         YEARS = 1978.8
         DYEARS = 0.0
      END IF

*   TYPE, LABEL.
      READ ( FD, *, IOSTAT = IOSTAT ) CTYPE, CID
      IF ( IOSTAT .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: reading absolute calibration data\\',
     :                STATUS )
         GO TO 999
      END IF

      CALL GEN_CTOS( CTYPE, MAXNAME, ABSTP, NCHAR )
      CALL STR_RMBLK( ABSTP )
      CALL GEN_CTOS( CID, MAXLABEL, ABSID, NCHAR )
      CALL STR_RMBLK( ABSID )

*   SCALE, TSEN.
      READ ( FD, *, IOSTAT = IOSTAT ) FAC, TSEN( 1 ), TSEN( 2 )
      IF ( IOSTAT .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: reading absolute calibration\\',
     :                STATUS )
         GO TO 999

      ELSE IF ( FAC .LE. 0.0 ) THEN
         CALL ERROUT( 'Error: absolute scale factor invalid\\',
     :                STATUS )
         GO TO 999
      END IF

*   NABS, NAPSEN, NTRSEN, DSMOD.
      READ ( FD, *, IOSTAT = IOSTAT) NABS, NAPSEN, NTRSEN, DSMOD
      IF ( IOSTAT .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: reading absolute calibration\\',
     :                STATUS )
         GO TO 999

      ELSE IF ( NABS.GT.MAXABS .OR. NABS.LT.2 ) THEN
         CALL ERROUT( 'Error: absolute calibration invalid size\\',
     :                STATUS )
         GO TO 999

      ELSE IF ( NAPSEN.GT.MAXABS .OR. NAPSEN.LT.0 .OR.
     :                                     NAPSEN.EQ.1 ) THEN
         CALL ERROUT( 'Error: aperture correction invalid size\\',
     :                STATUS )
         GO TO 999

      ELSE IF ( NTRSEN.GT.MAXABS .OR. NTRSEN.LT.0 .OR.
     :                                     NTRSEN.EQ.1 ) THEN
         CALL ERROUT(
     :           'Error: trailed spectrum correction invalid size\\',
     :           STATUS )
         GO TO 999

      ELSE IF ( DSMOD.NE.LINEAR .AND. DSMOD.NE.BOHLIN .AND.
     :                                        DSMOD.NE.NONE ) THEN
         CALL ERROUT(
     :           'Error: date sensitivity correction invalid mode\\',
     :           STATUS )
         GO TO 999
      END IF

*   XABS, YABS.
      READ ( FD, *, IOSTAT = IOSTAT )
     :       ( XABS( I ), YABS( I ), I = 1, NABS )
      IF ( IOSTAT .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: reading absolute calibration data\\',
     :                STATUS )
         GO TO 999
      END IF

*   TMPWRK1, TMPWRK2.
      READ ( FD, *, IOSTAT = IOSTAT)
     :       ( TMPWRK1( I ), TMPWRK2( I ), I = 1, NAPSEN )
      IF ( IOSTAT .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: reading aperture correction data\\',
     :                STATUS )
         GO TO 999
      END IF

*   XTRSEN, YTRSEN.
      READ ( FD, *, IOSTAT = IOSTAT )
     :       ( XTRSEN( I ), YTRSEN( I ), I = 1, NTRSEN )
      IF ( IOSTAT .NE. SAI__OK ) THEN
         CALL ERROUT(
     :           'Error: reading trailed spectrum correction data\\',
     :           STATUS )
         GO TO 999
      END IF

*  DSEN - dummy date sensitivity to maintain the structure of ".UEC" file.
      DSEN( 1 ) = 0.0
      DSEN( 2 ) = 0.0

*   XDSEN, YDSEN - branch on DSMOD.
      IF ( DSMOD .EQ. BOHLIN ) THEN

*      Date sensitivity correction is a non-linear one (i.e. Bohlin and
*      Grillmair, 1988).
         READ ( FD, *, IOSTAT = IOSTAT )
     :          NWDATE( LAPCOR ), NDATE( LAPCOR )
         READ ( FD, *, IOSTAT = IOSTAT )
     :          ( DATES( LAPCOR, I ), I = 1, NDATE( LAPCOR ) )
         READ ( FD, *, IOSTAT = IOSTAT )
     :          ( DSWAV( LAPCOR, J ), ( DSTABL( LAPCOR, I, J ),
     :          I = 1, NDATE( LAPCOR ) ), J = 1, NWDATE( LAPCOR ) )
*      Intermediate I/O status check.
         IF ( IOSTAT .NE. SAI__OK ) THEN
            CALL ERROUT(
     :           'Error: reading date sensitivity correction data\\',
     :           STATUS )
            GO TO 999
         END IF

         READ ( FD, *, IOSTAT = IOSTAT )
     :          NWDATE( SAPCOR ), NDATE( SAPCOR )
         READ ( FD, *, IOSTAT = IOSTAT )
     :          ( DATES( SAPCOR, I ), I = 1, NDATE( SAPCOR ) )
         READ ( FD, *, IOSTAT = IOSTAT )
     :          ( DSWAV( SAPCOR, J ), ( DSTABL( SAPCOR, I, J ),
     :          I = 1, NDATE( SAPCOR ) ), J = 1, NWDATE( SAPCOR ) )
*      Intermediate I/O status check.
         IF ( IOSTAT .NE. SAI__OK ) THEN
            CALL ERROUT(
     :           'Error: reading date sensitivity correction data\\',
     :           STATUS )
            GO TO 999
         END IF

         READ ( FD, *, IOSTAT = IOSTAT )
     :      NWDATE( TRACOR ), NDATE( TRACOR )
         READ ( FD, *, IOSTAT = IOSTAT )
     :          ( DATES( TRACOR, I ), I = 1, NDATE( TRACOR ) )
         READ ( FD, *, IOSTAT = IOSTAT )
     :          ( DSWAV( TRACOR, J ), ( DSTABL( TRACOR, I, J ),
     :          I = 1, NDATE( TRACOR ) ), J = 1, NWDATE( TRACOR ) )
*      Intermediate I/O status check.
         IF ( IOSTAT .NE. SAI__OK ) THEN
            CALL ERROUT(
     :           'Error: reading date sensitivity correction data\\',
     :           STATUS )
            GO TO 999
         END IF

*      Determine XDSEN, YDSEN, NDSEN by interpolating DSTABL
*      Check APER (LAP, SAP, BAP, TRAIL) - note that TRAIL is not yet
*      an option.
         IF ( STR_SIMLR( APER, 'LAP\\' ) ) THEN
            NCASE = LAPCOR

         ELSE IF ( STR_SIMLR( APER, 'SAP\\' ) ) THEN
            NCASE = SAPCOR

         ELSE IF ( STR_SIMLR( APER, 'BAP\\' ) ) THEN
            NCASE = LAPCOR
         END IF

*      Find wavelengths which bracket the DATE.
         DO IDATE = 1, NDATE( NCASE )
            IF ( DATES( NCASE, IDATE ) .GT. YEARS ) THEN
               GO TO 10
            END IF
         END DO
         IDATE = IDATE - 1
 10      CONTINUE

*      DATE is too early - probably before 1978.89.
         IF ( IDATE .EQ. 1 ) THEN
            CALL ERROUT(
     :      'Error: date out of range for date sensitivity data\\',
     :      STATUS )
            GO TO 999
         ELSE

*         Establish linear interpolation factor, DATCOR.
            DATE1 = DATES( NCASE, IDATE - 1 )
            DATE2 = DATES( NCASE, IDATE )
            DATCOR = ( YEARS - DATE1 ) / ( DATE2 - DATE1 )
         END IF

*      Interpolate DSTABL.
         NDSEN = NWDATE( NCASE )

         DO I = 1, NDSEN
            XDSEN( I ) = DSWAV( NCASE, I )
            YDSEN( I ) = DSTABL( NCASE, IDATE - 1, I ) +
     :                   ( DSTABL( NCASE, IDATE, I ) -
     :                     DSTABL( NCASE, IDATE - 1, I ) ) * DATCOR
         END DO

      ELSE IF ( DSMOD .EQ. LINEAR ) THEN

*      Date sensitivity correction is a linear one.
         READ ( FD, *, IOSTAT = IOSTAT ) NDSEN
         READ ( FD, *, IOSTAT = IOSTAT )
     :          ( XDSEN( I ), YDSEN( I ), I = 1, NDSEN )
      END IF
      IF ( IOSTAT .NE. SAI__OK ) THEN
          CALL ERROUT(
     :           'Error: reading date sensitivity correction data\\',
     :           STATUS )
         GO TO 999
      END IF

      IF ( NDSEN .GT. NABS ) THEN

*      Modify NABS, XABS, YABS to correspond to NDSEN, XDSEN by
*      interpolating XABS, YABS.
         CALL MSC_MAP1D4( NABS, XABS, YABS, NDSEN, XDSEN, FY, STATUS )
         IF ( STATUS .EQ. SAI__OK ) THEN

*         Loop to update XABS, YABS.
            NABS = NDSEN

            DO I = 1, NABS
               XABS( I ) = XDSEN( I )
               YABS( I ) = FY( I )
            END DO

         ELSE
            CALL ERROUT( 'Error: mapping absolute calibration data\\',
     :                   STATUS )
            GO TO 999
         END IF
      END IF

*   Scale YABS.
      DO I = 1, NABS
         YABS( I ) = YABS( I ) * FAC
      END DO

*   Load Ycorr.
      DO I = 1, NABS
         YCORR( I ) = YABS( I )
      END DO

*   Apply aperture sensitivity correction if necessary.
      IF ( STR_SIMLR( APER, 'SAP\\' ) .AND. NAPSEN.GT.0 ) THEN
         CALL MSC_MAP1D4( NAPSEN, TMPWRK1, TMPWRK2, NABS,
     :                    XABS, FY, STATUS )
         IF ( STATUS .EQ. SAI__OK ) THEN
            DO I = 1, NABS
               YCORR( I ) = YCORR( I ) / FY( I )
            END DO

            CALL LINE_WCONT(
     :     '%p Small Aperture Assumed for Absolute Calibration.\\' )
            CALL PRTBUF( STATUS )

         ELSE
            CALL ERROUT(
     :              'Error: mapping small aperture flux calibration\\',
     :              STATUS )
            GO TO 999
         END IF

      ELSE IF ( STR_SIMLR( APER, 'BAP\\' ) .AND. NAPSEN.GT.0 ) THEN
         CALL LINE_WCONT(
     :        '%p Large Aperture Assumed for Absolute Calibration.\\' )
         CALL PRTBUF( STATUS )

      ELSE
         CALL LINE_WCONT(
     :        '%p Large Aperture Assumed for Absolute Calibration.\\' )
         CALL PRTBUF( STATUS )
      END IF

*   Apply date sensitivity correction if necessary.
      IF ( NDSEN.GT.0 .AND. YEARS.GT.0.0 ) THEN
         CALL LINE_WCONT( '%p Applying Date Sensitivity\\' )
         CALL LINE_WCONT( ' Correction to Absolute Calibration.\\' )
         CALL PRTBUF( STATUS )

*      Branch on DSMOD.
         IF ( DSMOD .EQ. BOHLIN ) THEN

*         Date sensitivity correction is non-linear,
*         i.e. Bohlin and Grillmair, 1988.
            DO I = 1, NABS
               YCORR( I ) = YCORR( I ) / YDSEN( I )
            END DO

         ELSE IF ( DSMOD .EQ. LINEAR ) THEN

*         Date sensitivity correction is linear.
            CALL MSC_MAP1D4( NDSEN, XDSEN, YDSEN, NABS, XABS,
     :                       FY, STATUS )
            IF ( STATUS .EQ. SAI__OK ) THEN
               DO I = 1, NABS
                  YCORR( I ) = YCORR( I ) /
     :                         ( 1.0 - ( FY( I ) * DYEARS / 100.0 ) )
               END DO

            ELSE
               CALL ERROUT(
     :         'Error: mapping date sensitivity in flux calibration.\\',
     :         STATUS )
               GO TO 999
            END IF
         END IF
      END IF

      NOABS = .FALSE.

 999  CONTINUE

      END
