      SUBROUTINE IRC1_SBFIT( IDC, BPOSNS, MJD, UTC, SOLONG, PSI, WORK,
     :                       STATUS )
*+
*  Name:
*     IRC1_SBFIT

*  Purpose:
*     Fit straight lines to solar longitude and clock angles.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRC1_SBFIT( IDC, BPOSNS, MJD, UTC, SOLONG, PSI, WORK,
*                      STATUS )

*  Description:
*     Single precision NAG routines are called to fit least squares
*     straight lines through the solar longitude and clock angle values.
*     The independent variable is given by the supplied UTC values. The
*     given solar longitude values are converted from B1950 to mean of
*     date before being fitted. A solar latitude of zero is assumed
*     throughout. The gradients and intercepts are stored in common.

*  Arguments:
*     IDC = INTEGER (Given)
*        IRC identifier for the CRDD file from which the input values
*        come.
*     BPOSNS = INTEGER (Given)
*        No. of boresight positions in the input arrays.
*     MJD = DOUBLE PRECISION (Given)
*        Modified Julian Date at sample number 1 in the main DATA array
*        of the CRDD file.
*     UTC( BPOSNS ) = REAL (Given)
*        The difference in UTC between the moment each boresight
*        position was determined, and the moment sample number 1 of the
*        main CRDD file DATA array was taken.
*     SOLONG( BPOSNS ) = REAL (Given)
*        The solar longitude in radians, referred to the mean ecliptic
*        and equinox of Besselian epoch 1950.0.
*     PSI( BPOSNS ) = REAL (Given)
*        The clock angle in radians. Clock angle PSI decreases with
*        time. Thus the gradient of the corresponding linear fit is
*        negative.
*     WORK( BPOSNS, 3 ) = DOUBLE PRECISION (Given and Returned)
*        Work space.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     20-MAR-1991 (DSB):
*        Original version.
*     18-SEP-1991 (DSB):
*        Check for bad boresight values included.
*     14-MAY-1992 (DSB):
*        Modified to use double precision NAG library.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'DAT_PAR'          ! DAT__ constants
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! Starlink data constants
      INCLUDE 'I90_DAT'          ! IRAS90 parameter values.
      INCLUDE 'IRC_PAR'          ! IRC error values.
      INCLUDE 'IRC_ERR'          ! IRC error values.
      INCLUDE 'IRA_PAR'          ! IRA parameter values.

*  Global Variables:
      INCLUDE 'IRC_COM'          ! IRC common blocks.
*        CCM_CAGRD( IRC__MAX ) = REAL (Write)
*           The rate of change of clock angle (PSI) with time, in
*           radians per second (assumed constant).
*        CCM_CAZER( IRC__MAX ) = REAL (Write)
*           The clock angle (PSI) at sample number 1, in radians.
*        CCM_SLGRD( IRC__MAX ) = REAL (Write)
*           The rate of change of solar longitude with time, in radians
*           per second (assumed constant).
*        CCM_SLZER( IRC__MAX ) = REAL (Write)
*           The solar longitude at sample number 1, in radians.

*  Arguments Given:
      INTEGER IDC
      INTEGER BPOSNS
      DOUBLE PRECISION MJD
      REAL    UTC(BPOSNS)
      REAL    SOLONG(BPOSNS)
      REAL    PSI(BPOSNS)

*  Arguments Given and Returned:
      DOUBLE PRECISION WORK(BPOSNS,3)

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      DOUBLE PRECISION DDEC      ! DEC (J2000 FK5) of the sun.
      DOUBLE PRECISION DRA       ! RA (J2000 FK5) of the sun.
      DOUBLE PRECISION DSOLAT    ! Solar latitude (mean of date).
      DOUBLE PRECISION DSOLON    ! Solar longitude (mean of date).
      INTEGER          I         ! Loop count.
      INTEGER          IFAIL     ! NAG status value.
      INTEGER          J         ! No. of good boresight positions.
      INTEGER          JMIN      ! Min. number of good boresight samples
                                 ! required.
      DOUBLE PRECISION RESULT(20)! Values calculated by G02CAF.
      DOUBLE PRECISION SLA_EPB2D ! SLALIB function for converting
                                 ! Modified Julian Dates to Besselian
                                 ! Epochs.
      REAL             UTCMAX    ! Max. good UTC value.
      REAL             UTCMIN    ! Min. good UTC value.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Find the boresight samples at which SOLONG, PSI and UTS are all
*  valid. Also find the range of UTC values in the data.
      J = 0
      UTCMAX = VAL__MINR
      UTCMIN = VAL__MAXR

      DO I = 1, BPOSNS

         IF( UTC( I ) .NE. VAL__BADR ) THEN

            UTCMAX = MAX( UTCMAX, UTC( I ) )
            UTCMIN = MIN( UTCMIN, UTC( I ) )

            IF( SOLONG( I ) .NE. VAL__BADR .AND.
     :          PSI( I ) .NE. VAL__BADR ) THEN

               J = J + 1
               WORK( J , 1 ) = DBLE( SOLONG( I ) )
               WORK( J , 2 ) = DBLE( PSI( I ) )
               WORK( J , 3 ) = DBLE( UTC( I ) )

            END IF

         END IF

      END DO

*  Check that there is a reasonable amount of good boresight data (i.e.
*  at least one boresight sample per 10 seconds ).

      JMIN = MAX( 2, INT( 0.1*( UTCMAX - UTCMIN ) ) )
      IF( J .LT. JMIN ) THEN
         STATUS = IRC__NOBSD
         CALL ERR_REP( 'IRC1_SBFIT_ERR1',
     :            'IRC1_SBFIT: Insufficient good boresight data found',
     :                  STATUS )
         GO TO 999
      END IF

*  Convert the solar longitude values from 1950 values to mean of date.
*  Also remove any jumps of 2.PI in the stored values. NB, the solar
*  latitude in the 1950 system is assumed to be zero. The corresponding
*  solar latitude in the mean of date system is not used (a constant
*  value of zero is assumed).
      DO I = 1, J

         CALL SLA_ECLEQ( WORK( I, 1 ), 0.0D0, SLA_EPB2D( 1950.0D0 ),
     :                   DRA, DDEC )

         CALL SLA_EQECL( DRA, DDEC, MJD, DSOLON, DSOLAT )

         IF( I .GE. 2 ) THEN
            IF( WORK( I - 1, 1 ) - DSOLON .GT. IRA__PI ) THEN
               WORK( I, 1 ) = DSOLON + 2.0*IRA__PI
            ELSE
               WORK( I, 1 ) = DSOLON
            END IF

         ELSE
            WORK( 1, 1 ) = DSOLON

         END IF

      END DO

*  Fit a least squares straight line through the solar longitude values
*  which gives the solar longitude (mean of date) as a function of the
*  UTC since sample number 1.
      IFAIL = 1
      CALL G02CAF( J, WORK( 1, 3 ), WORK( 1, 1 ) , RESULT, IFAIL )

*  Check the IFAIL value.
      IF( IFAIL .NE. 0 ) THEN
         STATUS = IRC__NAGER
         CALL MSG_SETI( 'IF', IFAIL )
         CALL ERR_REP( 'IRC1_SBFIT_ERR2',
     :  'IRC1_SBFIT: G02CAF returned IFAIL = ^IF when fitting '//
     :  'solar longitude', STATUS )
         GO TO 999
      END IF

*  Store the gradient and intercept values in common.
      CCM_SLGRD( IDC ) = REAL( RESULT( 6 ) )
      CCM_SLZER( IDC ) = REAL( RESULT( 7 ) )

*  Remove any jumps of 2.PI in the stored values of clock angle, PSI.
      DO I = 2, J
         IF( WORK( I, 2 ) - WORK( I - 1, 2 ) .GT. IRA__PI )
     :                        WORK( I, 2 ) = WORK( I, 2 ) - 2.0*IRA__PI
      END DO

*  Fit a least squares straight line through the clock angle values
*  which gives the clock angle as a function of the UTC since sample
*  number 1.
      IFAIL = 1
      CALL G02CAF( J, WORK( 1, 3 ), WORK( 1, 2 ), RESULT, IFAIL )

*  Check the IFAIL value.
      IF( IFAIL .NE. 0 ) THEN
         STATUS = IRC__NAGER
         CALL MSG_SETI( 'IF', IFAIL )
         CALL ERR_REP( 'IRC1_SBFIT_ERR3',
     :   'IRC1_SBFIT: G02CAF returned IFAIL = ^IF when fitting'//
     :   ' clock angle', STATUS )
         GO TO 999
      END IF

*  Store the gradient and intercept values in common.
      CCM_CAGRD( IDC ) = REAL( RESULT( 6 ) )
      CCM_CAZER( IDC ) = REAL( RESULT( 7 ) )

 999  CONTINUE

      END
