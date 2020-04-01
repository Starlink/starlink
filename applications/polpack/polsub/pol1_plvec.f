      SUBROUTINE POL1_PLVEC( TR, EQMAP, NPIX, NROW, NZ, NSTOKE, NEL,
     :                       STOKE, VSTOKE, STKID, DEBIAS, VAR, ANGROT,
     :                       ANGRT, NDIMO, MAKEI, MAKEP, MAKET,
     :                       MAKEIP, MAKEQ, MAKEU, MAKEV, MAKECT, CI,
     :                       AI, AP, AT, AIP, AQ, AU, AV, AIV, APV, ATV,
     :                       AIPV, AQV, AUV, AVV, W, STATUS )

*+
*  Name:
*     POL1_PLVEC

*  Purpose:
*     Calculates polarisation parameters.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL POL1_PLVEC( TR, EQMAP, NPIX, NROW, NZ, NSTOKE, NEL, STOKE, VSTOKE,
*                      STKID, DEBIAS, VAR, ANGROT, ANGRT, NDIMO, MAKEI,
*                      MAKEP, MAKET, MAKEIP, MAKEQ, MAKEU, MAKEV, MAKECT,
*                      CI, AI, AP, AT, AIP, AQ, AU, AV, AIV, APV,
*                      ATV, AIPV, AQV, AUV, AVV, W, STATUS )

*  Description:
*     This routine calculates the polarisation parameters and (if
*     requested) variances for each pixel.

*  Arguments:
*     TR( 6 ) = _REAL (Given)
*        The coefficients of the transformation which converts cell indices
*        into (X,Y,Z) values to be stored in the catalogue (if required).
*           X = TR( 1 ) + TR( 2 )*REAL( IPIX )  ( IPIX = 1, NPIX )
*           Y = TR( 3 ) + TR( 4 )*REAL( IROW )  ( IROW = 1, NROW )
*           Z = TR( 5 ) + TR( 6 )*REAL( IZ )  ( IZ = 1, NZ )
*     EQMAP = INTEGER (Given)
*        If this is not AST__NULL, then the catalogue contains RA and DEC
*        columns, and EQMAP gives the AST Mapping from (X,Y(,Z)) to (RA,DEC).
*     NPIX = INTEGER (Given)
*        The number of pixels per row in STOKE and VSTOKE.
*     NROW = INTEGER (Given)
*        The number of rows in each Z plane of STOKE and VSTOKE.
*     NZ = INTEGER (Given)
*        The number of Z planes in each Stokes-cube of STOKE and VSTOKE.
*     NSTOKE = INTEGER (Given)
*        The number of planes in STOKE and VSTOKE.
*     NEL = INTEGER (Given)
*        Number of pixels in a single Z plane (=NROW*NPIX).
*     STOKE( NPIX, NROW, NZ, NSTOKE ) = REAL (Given)
*        The input Stokes parameters, as described by STKID.
*     VSTOKE( NPIX, NROW, NZ, NSTOKE ) = REAL (Given)
*        The variance on the Stokes parameters. It is ignored if VAR is
*        .FALSE..
*     STKID = CHARACTER * ( * ) (Given)
*        A string of characters identifying each Stokes-cube of the input
*        arrays. The first character applies to cube 1, the second to cube 2,
*        etc. Each character should be one of I, Q, U or V.
*     DEBIAS = INTEGER (Given)
*        If 0, no de-biasing is performed. If 1, de-biasing is performed
*        using the "asymptotic estimator". Any other non-zero value causes
*        de-biasing to be performed using the "modified asymptotic
*        estimator". See sections 2.3 and 2.5 of Montier et al "Polarization
*        measurements analysis II. Best estimators of polarization fraction
*        and angle" (A&A 2018). The returned variances are unaffected by
*        this value.
*     VAR = LOGICAL (Given)
*        It is .TRUE. if output variance values are to be returned.
*     ANGROT = REAL (Given)
*        ACW angle in degrees from pixel X axis to reference direction in
*        input cube.
*     ANGRT = REAL (Given)
*        ACW angle in degrees from pixel X axis to reference direction in
*        output NDFs and catalogue.
*     NDIMO = LOGICAL (Given)
*        The number of pixel axes (2 or 3) in output NDFs and catalogue.
*     MAKEI = LOGICAL (Given)
*        It is .TRUE. if a total intensity output array is required.
*     MAKEP = LOGICAL (Given)
*        It is .TRUE. if a percentage polarisation output array is
*        required.
*     MAKET = LOGICAL (Given)
*        It is .TRUE. if a polarisation angle output array is required.
*     MAKEIP = LOGICAL (Given)
*        It is .TRUE. if a polarised intensity output array is required.
*     MAKEQ = LOGICAL (Given)
*        It is .TRUE. if a Q output array is required. Ignored in
*        circular mode.
*     MAKEU = LOGICAL (Given)
*        It is .TRUE. if a U output array is required. Ignored in
*        circular mode.
*     MAKEV = LOGICAL (Given)
*        It is .TRUE. if a V output array is required. Ignored in
*        linear mode.
*     MAKECT = LOGICAL (Given)
*        It is .TRUE. if a catalogue containing everything is required.
*     CI = INTEGER (Given)
*        A CAT catalogue identifier. Only used if MAKECT is .TRUE.
*     AI( NPIX, NROW, NZ ) = REAL (Returned)
*        An array holding total intensity values.
*     AP( NPIX, NROW, NZ ) = REAL (Returned)
*        An array holding percentage polarisation values.
*     AT( NPIX, NROW, NZ ) = REAL (Returned)
*        An array holding polarisation angles (ACW from X axis to the
*        plane of polarization - in degrees)
*     AIP( NPIX, NROW, NZ ) = REAL (Returned)
*        An array holding polarised intensity values.
*     AQ( NPIX, NROW, NZ ) = REAL (Returned)
*        An array holding Q values.
*     AU( NPIX, NROW, NZ ) = REAL (Returned)
*        An array holding Q values.
*     AV( NPIX, NROW, NZ ) = REAL (Returned)
*        An array holding Q values.
*     AIV( NPIX, NROW, NZ ) = REAL (Returned)
*        An array holding the variance of AI.
*     APV( NPIX, NROW, NZ ) = REAL (Returned)
*        An array holding the variance of AP.
*     ATV( NPIX, NROW, NZ ) = REAL (Returned)
*        An array holding the variance of AT.
*     AIPV( NPIX, NROW, NZ ) = REAL (Returned)
*        An array holding the variance of AIP.
*     AQV( NPIX, NROW, NZ ) = REAL (Returned)
*        An array holding the variance of AQ.
*     AUV( NPIX, NROW, NZ ) = REAL (Returned)
*        An array holding the variance of AU.
*     AVV( NPIX, NROW, NZ ) = REAL (Returned)
*        An array holding the variance of AV.
*     W( NEL, NDIMO ) = DOUBLE PRECISION (Returned)
*        A work array. Only accessed if EQMAP is not AST__NULL.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1998 Central Laboratory of the Research Councils
*     Copyright (C) 2016 East Asian Observatory.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     13-JAN-1998 (DSB):
*        Original version.
*     22-JUN-1998 (DSB):
*        Corrected structuring of IF blocks which caused BAD values
*        to be used within calculations, resulting in NaN and Inf values
*        being stored in output images.
*     24-JUN-1998 (DSB):
*        Added Q,U and V outputs.
*     6-APR-1999 (DSB):
*        Added rotation of reference direction (ANGRT).
*     17-MAY-2000 (DSB):
*        Added EQMAP argument.
*     2-FEB-2001 (DSB):
*        Added support for 4D Stokes cubes.
*     28-SEP-2012 (DSB):
*        Use the absolute value of I to normalise Q and U. This avoids a
*        negative I value causing the vector to rotate by 90 degs.
*     14-JAN-2013 (DSB):
*        Store bad output values if the total intensity is negative.
*     16-OCT-2016 (DSB):
*        Change calculation of output values and errors. They no longer
*        start off by calculating normalised Q and U, and then deriving
*        everything else from them. The corresponding error propagation
*        formulae were complicated by the correlation between the
*        normalised values. But this complication was ignored. and so
*        the error values were wrong. Now base values on supplied Q and U
*        without normalisation.
*     23-MAR-2020 (DSB):
*        Store negative I values in the output catalogue but continue to
*        store bad values for P at such points. We need negative I values
*        to be stored so that the noise in vbackground regions looks right.
*     1-APR-2020 (DSB):
*        Provide option to do de-biasing using the "modified asymptotic
*        estimator".
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! VAL_ constants
      INCLUDE 'AST_PAR'          ! AST_ constants
      INCLUDE 'CAT_PAR'          ! CAT_ constants

*  Arguments Given:
      REAL TR( 6 )
      INTEGER EQMAP
      INTEGER NPIX
      INTEGER NROW
      INTEGER NZ
      INTEGER NSTOKE
      INTEGER NEL
      REAL STOKE( NPIX, NROW, NZ, NSTOKE )
      REAL VSTOKE( NPIX, NROW, NZ, NSTOKE )
      CHARACTER STKID*(*)
      INTEGER DEBIAS
      LOGICAL VAR
      REAL ANGROT
      REAL ANGRT
      INTEGER NDIMO
      LOGICAL MAKEI
      LOGICAL MAKEP
      LOGICAL MAKET
      LOGICAL MAKEIP
      LOGICAL MAKEQ
      LOGICAL MAKEU
      LOGICAL MAKEV
      LOGICAL MAKECT
      INTEGER CI

*  Arguments Returned:
      REAL AI( NPIX, NROW, NZ )
      REAL AP( NPIX, NROW, NZ )
      REAL AT( NPIX, NROW, NZ )
      REAL AIP( NPIX, NROW, NZ )
      REAL AQ( NPIX, NROW, NZ )
      REAL AU( NPIX, NROW, NZ )
      REAL AV( NPIX, NROW, NZ )
      REAL AIV( NPIX, NROW, NZ )
      REAL APV( NPIX, NROW, NZ )
      REAL ATV( NPIX, NROW, NZ )
      REAL AIPV( NPIX, NROW, NZ )
      REAL AQV( NPIX, NROW, NZ )
      REAL AUV( NPIX, NROW, NZ )
      REAL AVV( NPIX, NROW, NZ )
      DOUBLE PRECISION W( NEL, NDIMO )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER IC                 ! Index into STKID
      INTEGER II                 ! Loop variable
      INTEGER JI                 ! Index of I plane in input arrays
      INTEGER JQ                 ! Index of Q plane in input arrays
      INTEGER JU                 ! Index of U plane in input arrays
      INTEGER JV                 ! Index of V plane in input arrays
      INTEGER K                  ! Pixel index within Z plane
      INTEGER NVEC               ! No. of catalogue rows written
      INTEGER PIX                ! Array pixel index
      INTEGER ROW                ! Array row index
      INTEGER Z                  ! Z plane index
      LOGICAL CIRC               ! Measure circular polarisation?
      REAL COS2D                 ! Cos( 2* change in ref direction )
      REAL I                     ! Total intensity from all inputs
      REAL IIN                   ! Input I value
      REAL IP                    ! Polarised intensity
      REAL IP2                   ! Polarised intensity squared
      REAL P                     ! Percentage polarisation
      REAL Q2                    ! Normalised Q Stokes parameter squared
      REAL QIN                   ! Input Q value
      REAL QN                    ! Q w.r.t. new ref direction
      REAL RTOD                  ! Conversion factor; radians to degrees
      REAL SIN2D                 ! Cos( 2* change in ref direction )
      REAL T                     ! Polarisation angle
      REAL U2                    ! Normalised U Stokes parameter squared
      REAL UIN                   ! Input U value
      REAL UN                    ! U w.r.t. new ref direction
      REAL V                     ! Normalised V Stokes parameter
      REAL VIIN                  ! Input I variance
      REAL VIN                   ! Input V value
      REAL VIP                   ! Variance on polarised intensity
      REAL VP                    ! Variance on percentage polarisation
      REAL VQIN                  ! Input Q variance
      REAL VT                    ! Variance on Polarisation angle
      REAL VUIN                  ! Input U variance
      REAL VVIN                  ! Input V variance
      REAL VQN                   ! Rotated Q variance value
      REAL VUN                   ! Rotated U variance value
      REAL XR                    ! X value
      REAL YR                    ! Y value
      REAL ZR                    ! Z value

*  CAT identifiers for catalogue columns.
      INTEGER XCAT, YCAT, ZCAT, ICAT, PCAT, ANCAT, PICAT, VCAT, QCAT,
     :        UCAT, DICAT, DPCAT, DANCAT, DPICAT, DVCAT, DQCAT, DUCAT,
     :        RACAT, DECCAT

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Set up the conversion factor from radians to degrees.
      RTOD = 180.0 / ACOS( -1.0 )

*  Find the indices of the "planes" containing each Stokes paremeter.
      JI = 0
      JQ = 0
      JU = 0
      JV = 0

      DO IC = 1, MIN( LEN( STKID ), NSTOKE )
         IF( STKID( IC : IC ) .EQ. 'I' ) THEN
            JI = IC
         ELSE IF( STKID( IC : IC ) .EQ. 'Q' ) THEN
            JQ = IC
         ELSE IF( STKID( IC : IC ) .EQ. 'U' ) THEN
            JU = IC
         ELSE IF( STKID( IC : IC ) .EQ. 'V' ) THEN
            JV = IC
         END IF
      END DO

*  Set a flag indicating if we are measurring circular or plane
*  polarisation.
      CIRC = ( JV .NE. 0 )

*  Check that we have a complete set of Stokes parameters.
      IF( JI .EQ. 0 .OR. ( ( JQ .EQ. 0 .OR. JU .EQ. 0 )
     :                     .AND. JV .EQ. 0 ) ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'An incomplete set of Stokes parameters '//
     :                 'has been supplied.', STATUS )
         GO TO 999
      END IF

*  If a catalogue is being produced, get CAT identifiers for the required
*  columns.
      IF( MAKECT ) THEN
         NVEC = 0

         CALL POL1_GTCOL( CI, 'X', .TRUE., XCAT, STATUS )
         CALL POL1_GTCOL( CI, 'Y', .TRUE., YCAT, STATUS )
         IF( NDIMO .EQ. 3 ) THEN
            CALL POL1_GTCOL( CI, 'Z', .TRUE., ZCAT, STATUS )
         END IF

         IF( EQMAP .NE. AST__NULL ) THEN
            CALL POL1_GTCOL( CI, 'RA', .TRUE., RACAT, STATUS )
            CALL POL1_GTCOL( CI, 'DEC', .TRUE., DECCAT, STATUS )
         END IF

         CALL POL1_GTCOL( CI, 'I', .TRUE., ICAT, STATUS )
         CALL POL1_GTCOL( CI, 'P', .FALSE., PCAT, STATUS )
         CALL POL1_GTCOL( CI, 'ANG', .FALSE., ANCAT, STATUS )
         CALL POL1_GTCOL( CI, 'PI', .FALSE., PICAT, STATUS )

         IF( CIRC ) THEN
            CALL POL1_GTCOL( CI, 'V', .TRUE., VCAT, STATUS )
         ELSE
            CALL POL1_GTCOL( CI, 'Q', .TRUE., QCAT, STATUS )
            CALL POL1_GTCOL( CI, 'U', .TRUE., UCAT, STATUS )
         END IF

         IF( VAR ) THEN
            CALL POL1_GTCOL( CI, 'DI', .TRUE., DICAT, STATUS )
            CALL POL1_GTCOL( CI, 'DP', (PCAT .NE. CAT__NOID), DPCAT,
     :                       STATUS )
            CALL POL1_GTCOL( CI, 'DANG', (ANCAT .NE. CAT__NOID), DANCAT,
     :                       STATUS )
            CALL POL1_GTCOL( CI, 'DPI', (PICAT .NE. CAT__NOID), DPICAT,
     :                       STATUS )

            IF( CIRC ) THEN
               CALL POL1_GTCOL( CI, 'DV', .TRUE., DVCAT, STATUS )
            ELSE
               CALL POL1_GTCOL( CI, 'DQ', .TRUE., DQCAT, STATUS )
               CALL POL1_GTCOL( CI, 'DU', .TRUE., DUCAT, STATUS )
            END IF

         END IF

*  Calling AST_TRANN to map the pixel co-ords into RA/DEC has a high
*  overhead on each call. Therefore we need to do all points in a single
*  call to AST_TRAN2. So, if requried we calculate the RA and DECs now,
*  storing them in the supplied work arrays.
         IF( EQMAP .NE. AST__NULL ) THEN

*  Store the pixel co-ords for each vector.
            II = 0
            DO ROW = 1, NROW
               DO PIX = 1, NPIX
                  II = II + 1
                  W( II, 1 ) = DBLE( TR( 1 ) + TR( 2 )*REAL( PIX ) )
                  W( II, 2 ) = DBLE( TR( 3 ) + TR( 4 )*REAL( ROW ) )
               END DO
            END DO

            IF( NDIMO .EQ. 3 ) THEN
               DO II = 1, NEL
                  W( II, 3 ) = 1.0D0
               END DO
            END IF

*  Transform the pixel positions into RA and DECs.
            CALL AST_TRANN( EQMAP, NEL, NDIMO, NEL, W, .TRUE., 2, NEL,
     :                      W, STATUS )
         END IF
      END IF

*  First deal with plane polarisation cases...
      IF( .NOT. CIRC ) THEN

*  Loop round each Z plane.
         DO Z = 1, NZ

*  Loop round each element of a Z plane.
            K = 0
            DO ROW = 1, NROW
               DO PIX = 1, NPIX
                  K = K + 1

*  Initialise bad values for all inputs.
                  IIN = VAL__BADR
                  QIN = VAL__BADR
                  UIN = VAL__BADR
                  VIIN = VAL__BADR
                  VQIN = VAL__BADR
                  VUIN = VAL__BADR

*  Get the supplied stokes parameters and variances.
                  IIN = STOKE( PIX, ROW, Z, JI )
                  IF( VAR ) VIIN = VSTOKE( PIX, ROW, Z, JI )

                  QIN = STOKE( PIX, ROW, Z, JQ )
                  IF( VAR ) VQIN = VSTOKE( PIX, ROW, Z, JQ )

                  UIN = STOKE( PIX, ROW, Z, JU )
                  IF( VAR ) VUIN = VSTOKE( PIX, ROW, Z, JU )

*  If any of the intensities are bad, store bad results.
                  IF ( IIN .EQ. VAL__BADR .OR. QIN .EQ. VAL__BADR .OR.
     :                 UIN .EQ. VAL__BADR ) THEN

                     IP = VAL__BADR
                     I = VAL__BADR
                     P = VAL__BADR
                     T = VAL__BADR

                     IF ( VAR ) THEN
                        VIP = VAL__BADR
                        VP = VAL__BADR
                        VT = VAL__BADR
                     END IF

*  Otherwise, calculate everything.
                  ELSE

*  Copy the total intensity from input to output.
                     I = IIN

*  If required, rotate the Q and U values to the new reference direction.
                     IF( ANGROT .NE. ANGRT ) THEN
                        COS2D = COS( 2*( ANGRT - ANGROT )/RTOD )
                        SIN2D = SIN( 2*( ANGRT - ANGROT )/RTOD )
                        QN = QIN*COS2D + UIN*SIN2D
                        UN = UIN*COS2D - QIN*SIN2D
                        QIN = QN
                        UIN = UN
                     END IF

*  Get the squared Q and U values.
                     Q2 = QIN * QIN
                     U2 = UIN * UIN

*  Polarised intensity (without any de-biasing).
                     IP2 = Q2 + U2
                     IP = SQRT( MAX( 0.0, IP2 ) )

*  Polarisation angle.
                     IF( UIN .NE. 0.0 .OR. QIN .NE. 0.0 ) THEN
                        T = RTOD * 0.5 * ATAN2( UIN, QIN )
                     ELSE
                        T = VAL__BADR
                     END IF

*  Percentage polarisation (without any de-biasing).
                     IF( I .GT. 0.0 ) THEN
                        P = 100 * IP / I
                     ELSE
                        P = VAL__BADR
                     END IF

*  Now produced variances if required.
                     IF ( VAR ) THEN

*  If any of the input variances are bad, or if the percentage polarisation
*  is zero, store bad output variances. In this case we can't debias and
*  so store bad IP and P values.
                        IF( VIIN .EQ. VAL__BADR .OR. VQIN .EQ. VAL__BADR
     :                      .OR. VUIN .EQ. VAL__BADR .OR. I .LE. 0.0
     :                      .OR. IP2 .EQ. 0.0 ) THEN

                           VIP = VAL__BADR
                           VP = VAL__BADR
                           VT = VAL__BADR
                           IF ( DEBIAS .NE. 0 ) THEN
                              IP = VAL__BADR
                              P = VAL__BADR
                           END IF

*  Otherwise, calculate the variances.
                        ELSE

*  If required, rotate the Q and U variances to the new reference direction.
                           IF( ANGROT .NE. ANGRT ) THEN
                              VQN = VQIN*COS2D*COS2D + VUIN*SIN2D*SIN2D
                              VUN = VUIN*COS2D*COS2D + VQIN*SIN2D*SIN2D
                              VQIN = VQN
                              VUIN = VUN
                           END IF

*  Polarised intensity
                           VIP = ( Q2 * VQIN + U2 * VUIN )/IP2

*  Percentage polarisation
                           VP = 10000.0*( VIP/(I**2) + VIIN*IP2/(I**4) )

*  Polarisation angle (degs).
                           VT = RTOD*RTOD*( Q2*VUIN + U2*VQIN )/
     :                                    ( 4.0*IP2*IP2 )

*  If any of the variances are negative store bad results.
                           IF ( VIP .LT. 0.0 .OR. VP .LT. 0.0 .OR.
     :                          VT .LT. 0.0 ) THEN
                              VIP = VAL__BADR
                              VP = VAL__BADR
                              VT = VAL__BADR

*  If required, make an estimate of the percentage polarisation and
*  polarised intensity excluding the bias introduced because of the
*  distribution of P being non-symmetric. First deal with the traditional
*  asymptotic estimator.
                           ELSE IF ( DEBIAS .EQ. 1 ) THEN
                              IP = SQRT( MAX( 0.0, IP2 - VIP ) )
                              P = 100 * IP / I

*  Now deal with the modified asymptotic estimator described in section
*  2.5 of Montier et al.
                           ELSE IF ( DEBIAS .NE. 0 ) THEN
                              IF( IP .GT. 0.0 ) THEN
                                 IP = IP - 0.5*VIP*( 1.0 -
     :                                              EXP( -IP2/VIP ) )/IP
                              ELSE
                                 IP = 0.0
                              END IF
                           END IF

                        END IF

                     END IF

                  END IF

*  Store the required values in the output arrays.
                  IF ( MAKEI ) AI( PIX, ROW, Z ) = I
                  IF ( MAKEP ) AP( PIX, ROW, Z ) = P

                  IF ( MAKET ) THEN
                     IF( T .NE. VAL__BADR ) THEN
                        AT( PIX, ROW, Z ) = T
                     ELSE
                        AT( PIX, ROW, Z ) = VAL__BADR
                     END IF
                  END IF

                  IF ( MAKEIP ) AIP( PIX, ROW, Z ) = IP
                  IF ( MAKEQ ) AQ( PIX, ROW, Z ) = QIN
                  IF ( MAKEU ) AU( PIX, ROW, Z ) = UIN

                  IF ( VAR ) THEN
                     IF ( MAKEI ) AIV( PIX, ROW, Z ) = VIIN
                     IF ( MAKEP ) APV( PIX, ROW, Z ) = VP
                     IF ( MAKET ) ATV( PIX, ROW, Z ) = VT
                     IF ( MAKEIP ) AIPV( PIX, ROW, Z ) = VIP
                     IF ( MAKEQ ) AQV( PIX, ROW, Z ) = VQIN
                     IF ( MAKEU ) AUV( PIX, ROW, Z ) = VUIN
                  END IF

*  Append a row to the catalogue if required, and if some of the values
*  are not bad.
                  IF( MAKECT .AND. ( I .NE. VAL__BADR .OR.
     :                               QIN .NE. VAL__BADR .OR.
     :                               UIN .NE. VAL__BADR .OR.
     :                               P .NE. VAL__BADR .OR.
     :                               T .NE. VAL__BADR .OR.
     :                               IP .NE. VAL__BADR ) ) THEN

*  Increment the number of rows in the catalogue.
                     NVEC = NVEC + 1

*  Store values for all the catalogue columns in the current row buffer.
                     XR = TR( 1 ) + TR( 2 )*REAL( PIX )
                     YR = TR( 3 ) + TR( 4 )*REAL( ROW )
                     ZR = TR( 5 ) + TR( 6 )*REAL( Z )
                     CALL POL1_PUT0R( XCAT, XR, .FALSE., STATUS )
                     CALL POL1_PUT0R( YCAT, YR, .FALSE., STATUS )
                     IF( NDIMO .EQ. 3 ) THEN
                        CALL POL1_PUT0R( ZCAT, ZR, .FALSE., STATUS )
                     END IF

                     IF( EQMAP .NE. AST__NULL ) THEN
                        CALL POL1_PUT0D( RACAT, W( K, 1 ), .FALSE.,
     :                                   STATUS )
                        CALL POL1_PUT0D( DECCAT, W( K, 2 ), .FALSE.,
     :                                   STATUS )
                     END IF

                     CALL POL1_PUT0R( ICAT,   I, ( I .EQ. VAL__BADR ),
     :                               STATUS )
                     CALL POL1_PUT0R( QCAT, QIN, ( QIN .EQ. VAL__BADR ),
     :                               STATUS )
                     CALL POL1_PUT0R( UCAT, UIN, ( UIN .EQ. VAL__BADR ),
     :                               STATUS )
                     CALL POL1_PUT0R( PCAT,   P, ( P .EQ. VAL__BADR ),
     :                               STATUS )
                     CALL POL1_PUT0R( PICAT, IP, ( IP .EQ. VAL__BADR ),
     :                               STATUS )

                     IF ( T .NE. VAL__BADR ) THEN
                        CALL POL1_PUT0R( ANCAT,  T, .FALSE., STATUS )
                     ELSE
                        CALL POL1_PUT0R( ANCAT,  VAL__BADR, .TRUE.,
     :                                   STATUS )
                     END IF

                     IF( VAR ) THEN
                        IF( VIIN .NE. VAL__BADR ) THEN
                          CALL POL1_PUT0R( DICAT, SQRT( MAX(0.0,VIIN) ),
     :                                     .FALSE., STATUS )
                        ELSE
                          CALL POL1_PUT0R( DICAT, VAL__BADR, .TRUE.,
     :                                     STATUS )
                        END IF

                        IF( VQIN .NE. VAL__BADR ) THEN
                          CALL POL1_PUT0R( DQCAT, SQRT( MAX(0.0,VQIN) ),
     :                                     .FALSE., STATUS )
                        ELSE
                          CALL POL1_PUT0R( DQCAT, VAL__BADR, .TRUE.,
     :                                     STATUS )
                        END IF

                        IF( VUIN .NE. VAL__BADR ) THEN
                          CALL POL1_PUT0R( DUCAT, SQRT( MAX(0.0,VUIN) ),
     :                                     .FALSE., STATUS )
                        ELSE
                          CALL POL1_PUT0R( DUCAT, VAL__BADR, .TRUE.,
     :                                     STATUS )
                        END IF

                        IF( VP .NE. VAL__BADR ) THEN
                          CALL POL1_PUT0R( DPCAT, SQRT( MAX(0.0,VP) ),
     :                                .FALSE., STATUS )
                        ELSE
                          CALL POL1_PUT0R( DPCAT, VAL__BADR, .TRUE.,
     :                                     STATUS )
                        END IF

                        IF( VT .NE. VAL__BADR ) THEN
                          CALL POL1_PUT0R( DANCAT, SQRT( MAX(0.0,VT) ),
     :                                     .FALSE., STATUS )
                        ELSE
                          CALL POL1_PUT0R( DANCAT, VAL__BADR, .TRUE.,
     :                                     STATUS )
                        END IF

                        IF( VIP .NE. VAL__BADR ) THEN
                          CALL POL1_PUT0R( DPICAT, SQRT( MAX(0.0,VIP) ),
     :                                     .FALSE., STATUS )
                        ELSE
                          CALL POL1_PUT0R( DPICAT, VAL__BADR, .TRUE.,
     :                                     STATUS )
                        END IF

                     END IF

*  Append the current row buffer to the catalogue.
                     CALL CAT_RAPND( CI, STATUS )

                  END IF
               END DO
            END DO
         END DO

*  Now deal with circular polarisation
      ELSE

*  Loop round each Z plane.
         DO Z = 1, NZ

*  Loop round each element of a Z plane.
            K = 0
            DO ROW = 1, NROW
               DO PIX = 1, NPIX
                  K = K + 1

*  Initialise bad values for all inputs.
                  IIN = VAL__BADR
                  VIN = VAL__BADR
                  VIIN = VAL__BADR
                  VVIN = VAL__BADR

*  Get the supplied stokes parameters and variances.
                  IIN = STOKE( PIX, ROW, Z, JI )
                  IF( VAR ) VIIN = VSTOKE( PIX, ROW, Z, JI )

                  VIN = STOKE( PIX, ROW, Z, JV )
                  IF( VAR ) VVIN = VSTOKE( PIX, ROW, Z, JV )

*  If any of the two intensities are bad, store bad results.
                  IF ( IIN .EQ. VAL__BADR .OR. VIN .EQ. VAL__BADR ) THEN

                     IP = VAL__BADR
                     I = VAL__BADR
                     P = VAL__BADR
                     T = VAL__BADR

                     IF ( VAR ) THEN
                        VIP = VAL__BADR
                        VP = VAL__BADR
                        VT = VAL__BADR
                     END IF

*  If the total intensity is zero, store bad values for P and T, and zero
*  for I and IP.
                  ELSE IF( IIN .EQ. 0.0 ) THEN
                     IP = 0.0
                     I = 0.0
                     P = VAL__BADR
                     T = VAL__BADR

                     IF ( VAR ) THEN
                        VIP = VAL__BADR
                        VP = VAL__BADR
                        VT = VAL__BADR
                     END IF

*  Otherwise, calculate everything.
                  ELSE

*  Copy the total intensity from input to output.
                     I = IIN

*  Normalised Stokes parameter
                     V = VIN / ABS( IIN )

*  Percentage polarisation.
                     P = 100.0 * ABS( V )

*  Polarisation angle.
                     IF( V .GT. 0.0 ) THEN
                        T = 0.0
                     ELSE
                        T = 90.0
                     END IF

*  Polarised intensity.
                     IP = 0.01 * I * P

*  Now produced variances if required.
                     IF ( VAR ) THEN

*  If any of the input variances are bad, store bad output variances.
                        IF( VIIN .EQ. VAL__BADR .OR.
     :                      VVIN .EQ. VAL__BADR ) THEN

                           VIP = VAL__BADR
                           VP = VAL__BADR
                           VT = VAL__BADR

*  Otherwise, calculate the variances.
                        ELSE

*  Percentage polarisation.
                           VP = 10000.0 * ( V * V * VIIN + VVIN )/
     :                          ( I**2 )

*  Polarisation angle.
                           VT = VAL__BADR

*  Polarised intensity.
                           VIP = 0.0001 * ( P*P*VIIN + I*I*VP )

*  If any of the variances are negative store bad results.
                           IF ( VIP .LT. 0.0 .OR. VIIN .LT. 0.0 .OR.
     :                          VP .LT. 0.0 ) THEN
                              VIP = VAL__BADR
                              VIIN = VAL__BADR
                              VP = VAL__BADR
                           END IF

                        END IF

                     END IF

                  END IF

*  Store the required values in the output arrays.
                  IF ( MAKEI ) AI( PIX, ROW, Z ) = I
                  IF ( MAKEP ) AP( PIX, ROW, Z ) = P
                  IF ( MAKET ) AT( PIX, ROW, Z ) = T
                  IF ( MAKEIP ) AIP( PIX, ROW, Z ) = IP
                  IF ( MAKEV ) AV( PIX, ROW, Z ) = VIN

                  IF ( VAR ) THEN
                     IF ( MAKEI ) AIV( PIX, ROW, Z ) = VIIN
                     IF ( MAKEP ) APV( PIX, ROW, Z ) = VP
                     IF ( MAKET ) ATV( PIX, ROW, Z ) = VT
                     IF ( MAKEIP ) AIPV( PIX, ROW, Z ) = VIP
                     IF ( MAKEV ) AVV( PIX, ROW, Z ) = VVIN
                  END IF

*  Append a row to the catalogue if required, and if some of the values
*  are not bad.
                  IF( MAKECT .AND. ( I .NE. VAL__BADR .OR.
     :                               VIN .NE. VAL__BADR .OR.
     :                               P .NE. VAL__BADR .OR.
     :                               T .NE. VAL__BADR .OR.
     :                               IP .NE. VAL__BADR ) ) THEN

*  Increment the number of rows in the catalogue.
                     NVEC = NVEC + 1

*  Store values for all the catalogue columns in the current row buffer.
                     XR = TR( 1 ) + TR( 2 )*REAL( PIX )
                     YR = TR( 3 ) + TR( 4 )*REAL( ROW )
                     ZR = TR( 5 ) + TR( 6 )*REAL( Z )
                     CALL POL1_PUT0R( XCAT, XR, .FALSE., STATUS )
                     CALL POL1_PUT0R( YCAT, YR, .FALSE., STATUS )
                     IF( NDIMO .EQ. 3 ) THEN
                        CALL POL1_PUT0R( ZCAT, ZR, .FALSE., STATUS )
                     END IF

                     IF( EQMAP .NE. AST__NULL ) THEN
                        CALL POL1_PUT0D( RACAT, W( K, 1 ), .FALSE.,
     :                                  STATUS )
                        CALL POL1_PUT0D( DECCAT, W( K, 2 ), .FALSE.,
     :                                  STATUS )
                     END IF

                     CALL POL1_PUT0R( ICAT,   I, ( I .EQ. VAL__BADR ),
     :                               STATUS )
                     CALL POL1_PUT0R( VCAT, VIN, ( VIN .EQ. VAL__BADR ),
     :                               STATUS )
                     CALL POL1_PUT0R( PCAT,   P, ( P .EQ. VAL__BADR ),
     :                               STATUS )
                     CALL POL1_PUT0R( ANCAT,  T, ( T .EQ. VAL__BADR ),
     :                               STATUS )
                     CALL POL1_PUT0R( PICAT, IP, ( IP .EQ. VAL__BADR ),
     :                               STATUS )

                     IF( VAR ) THEN
                        IF( VIIN .NE. VAL__BADR ) THEN
                          CALL POL1_PUT0R( DICAT, SQRT( MAX(0.0,VIIN) ),
     :                                     .FALSE., STATUS )
                        ELSE
                          CALL POL1_PUT0R( DICAT, VAL__BADR, .TRUE.,
     :                                     STATUS )
                        END IF

                        IF( VVIN .NE. VAL__BADR ) THEN
                          CALL POL1_PUT0R( DVCAT, SQRT( MAX(0.0,VVIN) ),
     :                                     .FALSE., STATUS )
                        ELSE
                          CALL POL1_PUT0R( DVCAT, VAL__BADR, .TRUE.,
     :                                     STATUS )
                        END IF

                        IF( VP .NE. VAL__BADR ) THEN
                          CALL POL1_PUT0R( DPCAT, SQRT( MAX(0.0,VP) ),
     :                                     .FALSE., STATUS )
                        ELSE
                          CALL POL1_PUT0R( DPCAT, VAL__BADR, .TRUE.,
     :                                     STATUS )
                        END IF

                        IF( VT .NE. VAL__BADR ) THEN
                          CALL POL1_PUT0R( DANCAT, SQRT( MAX(0.0,VT) ),
     :                                     .FALSE., STATUS )
                        ELSE
                          CALL POL1_PUT0R( DANCAT, VAL__BADR, .TRUE.,
     :                                     STATUS )
                        END IF

                        IF( VIP .NE. VAL__BADR ) THEN
                          CALL POL1_PUT0R( DPICAT, SQRT( MAX(0.0,VIP) ),
     :                                     .FALSE., STATUS )
                        ELSE
                          CALL POL1_PUT0R( DPICAT, VAL__BADR, .TRUE.,
     :                                     STATUS )
                        END IF

                     END IF

*  Append the current row buffer to the catalogue.
                     CALL CAT_RAPND( CI, STATUS )

                  END IF

               END DO
            END DO
         END DO
      END IF

*  Display the number of rows written to the catalogue.
      IF( MAKECT ) THEN
         CALL MSG_BLANK( STATUS )
         IF( NVEC .GT. 1 ) THEN
            CALL MSG_SETI( 'N', NVEC )
            CALL MSG_OUT( 'POL1_PLVEC_1', '   ^N vectors written to '//
     :                    'the output catalogue.', STATUS )
         ELSE IF( NVEC .EQ. 1 ) THEN
            CALL MSG_OUT( 'POL1_PLVEC_2', '   One vector written to '//
     :                    'the output catalogue.', STATUS )
         ELSE
            CALL MSG_OUT( 'POL1_PLVEC_2', '   No vectors written to '//
     :                    'the output catalogue.', STATUS )
         END IF
         CALL MSG_BLANK( STATUS )
      END IF

 999  CONTINUE

      END
