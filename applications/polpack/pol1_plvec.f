      SUBROUTINE POL1_PLVEC( TR, NPIX, NROW, NPLANE, STOKE, VSTOKE, 
     :                       STKID, DEBIAS, VAR, MAKEI, MAKEP, MAKET, 
     :                       MAKEIP, MAKECT, CI, AI, AP, AT, AIP, AIV, 
     :                       APV, ATV, AIPV, STATUS )
*+
*  Name:
*     POL1_PLVEC

*  Purpose:
*     Calculates polarisation parameters.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*      CALL POL1_PLVEC( TR, NPIX, NROW, NPLANE, STOKE, VSTOKE, STKID, 
*                       DEBIAS, VAR, MAKEI, MAKEP, MAKET, MAKEIP, MAKECT, 
*                       CI, AI, AP, AT, AIP, AIV, APV, ATV, AIPV, STATUS )

*  Description:
*     This routine calculates the polarisation parameters and (if
*     requested) variances for each pixel.

*  Arguments:
*     TR( 4 ) = _REAL (Given)
*        The coefficients of the transformation which converts cell indices
*        into (X,Y) values to be stored in the catalogue (if required). 
*           X = TR( 1 ) + TR( 2 )*REAL( IPIX )  ( IPIX = 1, NPIX )
*           Y = TR( 3 ) + TR( 4 )*REAL( IROW )  ( IROW = 1, NROW )
*     NPIX = INTEGER (Given)
*        The number of pixels per row in STOKE and VSTOKE.
*     NROW = INTEGER (Given)
*        The number of rows in each plane of STOKE and VSTOKE.
*     NPLANE = INTEGER (Given)
*        The number of planes in STOKE and VSTOKE.
*     STOKE( NPIX, NROW, NPLANE ) = REAL (Given)
*        The input Stokes parameters, as described by STKID.
*     VSTOKE( NPIX, NROW, NPLANE ) = REAL (Given)
*        The variance on the Stokes parameters. It is ignored if VAR is 
*        .FALSE..
*     STKID = CHARACTER * ( * ) (Given)
*        A string of characters identifying each plane of the input arrays.
*        The first character applies to plane 1, the second to plane 2, 
*        etc. Each character should be one of I, Q, U or V.
*     DEBIAS = LOGICAL (Given)
*        It is .TRUE. if the effects of biassing caused by the
*        asymmetric distribution of percentage polarisation is to be
*        removed.  The returned variance values are unaffected by this
*        value.
*     VAR = LOGICAL (Given)
*        It is .TRUE. if output variance values are to be returned.
*     MAKEI = LOGICAL (Given)
*        It is .TRUE. if a total intensity output array is required.
*     MAKEP = LOGICAL (Given)
*        It is .TRUE. if a percentage polarisation output array is
*        required.
*     MAKET = LOGICAL (Given)
*        It is .TRUE. if a polarisation angle output array is required.
*     MAKEIP = LOGICAL (Given)
*        It is .TRUE. if a polarised intensity output array is required.
*     MAKECT = LOGICAL (Given)
*        It is .TRUE. if a catalogue containing everything is required.
*     CI = INTEGER (Given)
*        A CAT catalogue identifier. Only used if MAKECT is .TRUE.
*     AI( NPIX, NROW ) = REAL (Returned)
*        An array holding total intensity values.
*     AP( NPIX, NROW ) = REAL (Returned)
*        An array holding percentage polarisation values.
*     AT( NPIX, NROW ) = REAL (Returned)
*        An array holding polarisation angles (in degrees).
*     AIP( NPIX, NROW ) = REAL (Returned)
*        An array holding polarised intensity values.
*     AIV( NPIX, NROW ) = REAL (Returned)
*        An array holding the variance of AI.
*     APV( NPIX, NROW ) = REAL (Returned)
*        An array holding the variance of AP.
*     ATV( NPIX, NROW ) = REAL (Returned)
*        An array holding the variance of AT.
*     AIPV( NPIX, NROW ) = REAL (Returned)
*        An array holding the variance of AIP.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     13-JAN-1998 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! VAL_ constants

*  Arguments Given:
      REAL TR( 4 )
      INTEGER NPIX
      INTEGER NROW 
      INTEGER NPLANE
      REAL STOKE( NPIX, NROW, NPLANE )
      REAL VSTOKE( NPIX, NROW, NPLANE )
      CHARACTER STKID*(*)
      LOGICAL DEBIAS
      LOGICAL VAR
      LOGICAL MAKEI
      LOGICAL MAKEP
      LOGICAL MAKET
      LOGICAL MAKEIP
      LOGICAL MAKECT
      INTEGER CI

*  Arguments Returned:
      REAL AI( NPIX, NROW )
      REAL AP( NPIX, NROW )
      REAL AT( NPIX, NROW )
      REAL AIP( NPIX, NROW )
      REAL AIV( NPIX, NROW )
      REAL APV( NPIX, NROW )
      REAL ATV( NPIX, NROW )
      REAL AIPV( NPIX, NROW )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER IC                 ! Index into STKID
      INTEGER JI                 ! Index of I plane in input arrays
      INTEGER JQ                 ! Index of Q plane in input arrays
      INTEGER JU                 ! Index of U plane in input arrays
      INTEGER JV                 ! Index of V plane in input arrays
      INTEGER NVEC               ! No. of catalogue rows written
      INTEGER PIX                ! Pixel index
      INTEGER ROW                ! Row index
      LOGICAL CIRC               ! Measure circular polarisation?
      REAL EPS2                  ! Mean variance on normalised Q and U
      REAL FACT                  ! Commonly needed factor
      REAL I                     ! Total intensity from all inputs
      REAL IIN                   ! Input I value
      REAL IP                    ! Polarised intensity 
      REAL P                     ! Percentage polarisation
      REAL P2                    ! Fractional polarisation squared
      REAL Q                     ! Normalised Q Stokes parameter
      REAL Q2                    ! Normalised Q Stokes parameter squared
      REAL QIN                   ! Input Q value
      REAL RTOD                  ! Conversion factor; radians to degrees
      REAL T                     ! Polarisation angle
      REAL U                     ! Normalised U Stokes parameter
      REAL U2                    ! Normalised U Stokes parameter squared
      REAL UIN                   ! Input U value
      REAL V                     ! Normalised V Stokes parameter
      REAL V2                    ! Normalised V Stokes parameter squared
      REAL VI                    ! Variance on tot. int. from all inputs
      REAL VIIN                  ! Input I variance
      REAL VIN                   ! Input V value
      REAL VIP                   ! Variance on polarised intensity 
      REAL VP                    ! Variance on percentage polarisation
      REAL VQ                    ! Variance on normalised Q Stokes par.
      REAL VQIN                  ! Input Q variance
      REAL VT                    ! Variance on Polarisation angle
      REAL VU                    ! Variance on normalised U Stokes par.
      REAL VUIN                  ! Input U variance
      REAL VV                    ! Variance on normalised V Stokes par.
      REAL VVIN                  ! Input V variance

*  CAT identifiers for catalogue columns.
      INTEGER XCAT, YCAT, ICAT, PCAT, THCAT, PICAT, VCAT, QCAT, UCAT,
     :        DICAT, DPCAT, DTHCAT, DPICAT, DVCAT, DQCAT, DUCAT


*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Set up the conversion factor from radians to degrees.
      RTOD = 180.0 / ACOS( -1.0 )

*  Find the indices of the planes containing each Stokes paremeter.
      JI = 0
      JQ = 0
      JU = 0
      JV = 0

      DO IC = 1, MIN( LEN( STKID ), NPLANE )
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

         CALL CAT_TIDNT( CI, 'X', XCAT, STATUS )
         CALL CAT_TIDNT( CI, 'Y', YCAT, STATUS )
         CALL CAT_TIDNT( CI, 'I', ICAT, STATUS )
         CALL CAT_TIDNT( CI, 'P', PCAT, STATUS )
         CALL CAT_TIDNT( CI, 'THETA', THCAT, STATUS )
         CALL CAT_TIDNT( CI, 'PI', PICAT, STATUS )

         IF( CIRC ) THEN
            CALL CAT_TIDNT( CI, 'V', VCAT, STATUS )
         ELSE
            CALL CAT_TIDNT( CI, 'Q', QCAT, STATUS )
            CALL CAT_TIDNT( CI, 'U', UCAT, STATUS )
         END IF

         IF( VAR ) THEN
            CALL CAT_TIDNT( CI, 'DI', DICAT, STATUS )
            CALL CAT_TIDNT( CI, 'DP', DPCAT, STATUS )
            CALL CAT_TIDNT( CI, 'DTHETA', DTHCAT, STATUS )
            CALL CAT_TIDNT( CI, 'DPI', DPICAT, STATUS )

            IF( CIRC ) THEN
               CALL CAT_TIDNT( CI, 'DV', DVCAT, STATUS )
            ELSE
               CALL CAT_TIDNT( CI, 'DQ', DQCAT, STATUS )
               CALL CAT_TIDNT( CI, 'DU', DUCAT, STATUS )
            END IF

         END IF

      END IF

*  First deal with plane polarisation cases...
      IF( .NOT. CIRC ) THEN

*  Loop round each element of the arrays.
         DO ROW = 1, NROW
            DO PIX = 1, NPIX

*  Initialise bad values for all inputs.
               IIN = VAL__BADR  
               QIN = VAL__BADR  
               UIN = VAL__BADR  
               VIIN = VAL__BADR  
               VQIN = VAL__BADR  
               VUIN = VAL__BADR  

*  Get the supplied stokes parameters and variances.
               IIN = STOKE( PIX, ROW, JI )
               IF( VAR ) VIIN = VSTOKE( PIX, ROW, JI )
   
               QIN = STOKE( PIX, ROW, JQ )
               IF( VAR ) VQIN = VSTOKE( PIX, ROW, JQ )

               UIN = STOKE( PIX, ROW, JU )
               IF( VAR ) VUIN = VSTOKE( PIX, ROW, JU )

*  If any of the four intensities are bad, store bad results.
               IF ( IIN .EQ. VAL__BADR .OR. QIN .EQ. VAL__BADR .OR.
     :              UIN .EQ. VAL__BADR ) THEN
   
                  IP = VAL__BADR
                  I = VAL__BADR
                  P = VAL__BADR
                  T = VAL__BADR
      
                  IF ( VAR ) THEN
                     VIP = VAL__BADR
                     VI = VAL__BADR
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
                     VI = VIIN
                     VP = VAL__BADR
                     VT = VAL__BADR
                  END IF

*  Otherwise, calculate everything.
               ELSE

*  Copy the total intensity from input to output.
                  I = IIN

*  Normalised Stokes parameters.
                  Q = QIN / IIN
                  Q2 = Q * Q
   
                  U = UIN / IIN
                  U2 = U * U
   
*  Percentage polarisation.
                  P2 = Q2 + U2 
                  P = 100.0 * SQRT( MAX( 0.0, P2 ) )

*  Polarisation angle.
                  IF( U .NE. 0.0 .OR. Q .NE. 0.0 ) THEN
                     T = RTOD * 0.5 * ATAN2( U, Q )
                  ELSE
                     T = VAL__BADR
                  END IF

               END IF

*  Polarised intensity.
               IP = 0.01 * P * I

*  Now produced variances if required.
               IF ( VAR ) THEN

*  Total intensity.
                  VI = VIIN

*  If any of the input variances are bad, or if the percentage polarisation 
*  is zero, store bad output variances.
                  IF( VIIN .EQ. VAL__BADR .OR. VQIN .EQ. VAL__BADR .OR.
     :                VUIN .EQ. VAL__BADR .OR. P2 .EQ. 0.0 ) THEN

                     VIP = VAL__BADR
                     VP = VAL__BADR
                     VT = VAL__BADR

*  Otherwise, calculate the variances.
                  ELSE

*  Normalised Stokes parameter, Q and U.
                     VQ = ( Q2 * VIIN + VQIN )/( I**2 )
                     VU = ( U2 * VIIN + VUIN )/( I**2 )

*  Find the average variance of the normalised Q and U.
                     EPS2 = 0.5*( VQ + VU )

*  Percentage polarisation.
                     VP = 10000.0 * EPS2

*  Polarisation angle (degs).
                     VT = RTOD * RTOD * 0.25 * EPS2 / P2

*  Polarised intensity.
                     VIP = P2*VI + I*I*EPS2

*  If any of the variances are negative store bad results.
                     IF ( VIP .LT. 0.0 .OR. VI .LT. 0.0 .OR.
     :                    VP .LT. 0.0 .OR. VT .LT. 0.0 ) THEN
                        VIP = VAL__BADR
                        VI = VAL__BADR
                        VP = VAL__BADR
                        VT = VAL__BADR

*  If required, make an estimate of the percentage polarisation and
*  polarised intensity excluding the bias introduced because of the
*  distribution of P being non-symmetric.
                     ELSE
                        IF ( DEBIAS ) THEN
                           P = 100.0*SQRT( MAX( 0.0, P2 - EPS2 ) )
                           IP = 0.01 * I * P
                        END IF

                     END IF

                  END IF

               END IF

*  Store the required values in the output arrays.
               IF ( MAKEI ) AI( PIX, ROW ) = I
               IF ( MAKEP ) AP( PIX, ROW ) = P
               IF ( MAKET ) AT( PIX, ROW ) = T
               IF ( MAKEIP ) AIP( PIX, ROW ) = IP
      
               IF ( VAR ) THEN
                  IF ( MAKEI ) AIV( PIX, ROW ) = VI
                  IF ( MAKEP ) APV( PIX, ROW ) = VP
                  IF ( MAKET ) ATV( PIX, ROW ) = VT
                  IF ( MAKEIP ) AIPV( PIX, ROW ) = VIP
               END IF

*  Append a row to the catalogue if required, and if some of the values
*  are not bad.
               IF( MAKECT .AND. ( I .NE. VAL__BADR .OR. 
     :                            QIN .NE. VAL__BADR .OR. 
     :                            UIN .NE. VAL__BADR .OR. 
     :                            P .NE. VAL__BADR .OR. 
     :                            T .NE. VAL__BADR .OR. 
     :                            IP .NE. VAL__BADR ) ) THEN

*  Store values for all the catalogue columns in the current row buffer.
                  CALL CAT_PUT0R( XCAT, TR( 1 ) + TR( 2 )*REAL( PIX ),
     :                            .FALSE., STATUS )
                  CALL CAT_PUT0R( YCAT, TR( 3 ) + TR( 4 )*REAL( ROW ),
     :                            .FALSE., STATUS )
                  CALL CAT_PUT0R( ICAT,   I, ( I .EQ. VAL__BADR ), 
     :                            STATUS )
                  CALL CAT_PUT0R( QCAT, QIN, ( QIN .EQ. VAL__BADR ),
     :                            STATUS )
                  CALL CAT_PUT0R( UCAT, UIN, ( UIN .EQ. VAL__BADR ),
     :                            STATUS )
                  CALL CAT_PUT0R( PCAT,   P, ( P .EQ. VAL__BADR ), 
     :                            STATUS )
                  CALL CAT_PUT0R( THCAT,  T, ( T .EQ. VAL__BADR ), 
     :                            STATUS )
                  CALL CAT_PUT0R( PICAT, IP, ( IP .EQ. VAL__BADR ),
     :                            STATUS )

                  IF( VAR ) THEN
                     IF( VI .NE. VAL__BADR ) THEN
                        CALL CAT_PUT0R( DICAT, SQRT( MAX( 0.0, VI ) ), 
     :                                  .FALSE., STATUS )
                     ELSE
                        CALL CAT_PUT0R( DICAT, VAL__BADR, .TRUE., 
     :                                  STATUS )
                     END IF

                     IF( VQIN .NE. VAL__BADR ) THEN
                        CALL CAT_PUT0R( DQCAT, SQRT( MAX( 0.0, VQIN ) ), 
     :                                  .FALSE., STATUS )
                     ELSE
                        CALL CAT_PUT0R( DQCAT, VAL__BADR, .TRUE., 
     :                                  STATUS )
                     END IF

                     IF( VUIN .NE. VAL__BADR ) THEN
                        CALL CAT_PUT0R( DUCAT, SQRT( MAX( 0.0, VUIN ) ), 
     :                                  .FALSE., STATUS )
                     ELSE
                        CALL CAT_PUT0R( DUCAT, VAL__BADR, .TRUE., 
     :                                  STATUS )
                     END IF


                     IF( VP .NE. VAL__BADR ) THEN
                        CALL CAT_PUT0R( DPCAT, SQRT( MAX( 0.0, VP ) ), 
     :                             .FALSE., STATUS )
                     ELSE
                        CALL CAT_PUT0R( DPCAT, VAL__BADR, .TRUE., 
     :                                  STATUS )
                     END IF

                     IF( VT .NE. VAL__BADR ) THEN
                        CALL CAT_PUT0R( DTHCAT, SQRT( MAX( 0.0, VT ) ), 
     :                                  .FALSE., STATUS )
                     ELSE
                        CALL CAT_PUT0R( DTHCAT, VAL__BADR, .TRUE., 
     :                                  STATUS )
                     END IF

                     IF( VIP .NE. VAL__BADR ) THEN
                        CALL CAT_PUT0R( DPICAT, SQRT( MAX( 0.0, VIP ) ), 
     :                                  .FALSE., STATUS )
                     ELSE
                        CALL CAT_PUT0R( DPICAT, VAL__BADR, .TRUE., 
     :                                  STATUS )
                     END IF

                  END IF

*  Append the current row buffer to the catalogue.
                  CALL CAT_RAPND( CI, STATUS )
                  NVEC = NVEC + 1

               END IF
            END DO
         END DO

*  Now deal with circular polarisation
      ELSE

*  Loop round each element of the arrays.
         DO ROW = 1, NROW
            DO PIX = 1, NPIX

*  Initialise bad values for all inputs.
               IIN = VAL__BADR  
               VIN = VAL__BADR  
               VIIN = VAL__BADR  
               VVIN = VAL__BADR  

*  Get the supplied stokes parameters and variances.
               IIN = STOKE( PIX, ROW, JI )
               IF( VAR ) VIIN = VSTOKE( PIX, ROW, JI )
   
               VIN = STOKE( PIX, ROW, JV )
               IF( VAR ) VVIN = VSTOKE( PIX, ROW, JV )

*  If any of the two intensities are bad, store bad results.
               IF ( IIN .EQ. VAL__BADR .OR. VIN .EQ. VAL__BADR ) THEN
   
                  IP = VAL__BADR
                  I = VAL__BADR
                  P = VAL__BADR
                  T = VAL__BADR
      
                  IF ( VAR ) THEN
                     VIP = VAL__BADR
                     VI = VAL__BADR
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
                     VI = VIIN
                     VP = VAL__BADR
                     VT = VAL__BADR
                  END IF

*  Otherwise, calculate everything.
               ELSE

*  Copy the total intensity from input to output.
                  I = IIN

*  Normalised Stokes parameter
                  V = VIN / IIN

*  Percentage polarisation.
                  P = 100.0 * ABS( V )

*  Polarisation angle.
                  IF( V .GT. 0.0 ) THEN
                     T = 0.0
                  ELSE
                     T = 90.0
                  END IF

               END IF

*  Polarised intensity.
               IP = 0.01 * I * P

*  Now produced variances if required.
               IF ( VAR ) THEN

*  Total intensity.
                  VI = VIIN

*  If any of the input variances are bad, store bad output variances.
                  IF( VIIN .EQ. VAL__BADR .OR. 
     :                VVIN .EQ. VAL__BADR ) THEN

                     VIP = VAL__BADR
                     VP = VAL__BADR
                     VT = VAL__BADR

*  Otherwise, calculate the variances.
                  ELSE

*  Percentage polarisation.
                     VP = 10000.0 * ( V * V * VIIN + VVIN )/( I**2 )

*  Polarisation angle.
                     VT = VAL__BADR

*  Polarised intensity.
                     VIP = 0.0001 * ( P*P*VI + I*I*VP )

*  If any of the variances are negative store bad results.
                     IF ( VIP .LT. 0.0 .OR. VI .LT. 0.0 .OR.
     :                    VP .LT. 0.0 ) THEN
                        VIP = VAL__BADR
                        VI = VAL__BADR
                        VP = VAL__BADR
                     END IF

                  END IF

               END IF

*  Store the required values in the output arrays.
               IF ( MAKEI ) AI( PIX, ROW ) = I
               IF ( MAKEP ) AP( PIX, ROW ) = P
               IF ( MAKET ) AT( PIX, ROW ) = T
               IF ( MAKEIP ) AIP( PIX, ROW ) = IP
      
               IF ( VAR ) THEN
                  IF ( MAKEI ) AIV( PIX, ROW ) = VI
                  IF ( MAKEP ) APV( PIX, ROW ) = VP
                  IF ( MAKET ) ATV( PIX, ROW ) = VT
                  IF ( MAKEIP ) AIPV( PIX, ROW ) = VIP
               END IF
   
*  Append a row to the catalogue if required, and if some of the values
*  are not bad.
               IF( MAKECT .AND. ( I .NE. VAL__BADR .OR. 
     :                            VIN .NE. VAL__BADR .OR. 
     :                            P .NE. VAL__BADR .OR. 
     :                            T .NE. VAL__BADR .OR. 
     :                            IP .NE. VAL__BADR ) ) THEN

*  Store values for all the catalogue columns in the current row buffer.
                  CALL CAT_PUT0R( XCAT, TR( 1 ) + TR( 2 )*REAL( PIX ),
     :                            .FALSE., STATUS )
                  CALL CAT_PUT0R( YCAT, TR( 3 ) + TR( 4 )*REAL( ROW ),
     :                            .FALSE., STATUS )
                  CALL CAT_PUT0R( ICAT,   I, ( I .EQ. VAL__BADR ), 
     :                            STATUS )
                  CALL CAT_PUT0R( VCAT, VIN, ( VIN .EQ. VAL__BADR ), 
     :                            STATUS )
                  CALL CAT_PUT0R( PCAT,   P, ( P .EQ. VAL__BADR ), 
     :                            STATUS )
                  CALL CAT_PUT0R( THCAT,  T, ( T .EQ. VAL__BADR ), 
     :                            STATUS )
                  CALL CAT_PUT0R( PICAT, IP, ( IP .EQ. VAL__BADR ), 
     :                            STATUS )

                  IF( VAR ) THEN
                     IF( VI .NE. VAL__BADR ) THEN
                        CALL CAT_PUT0R( DICAT, SQRT( MAX( 0.0, VI ) ), 
     :                                  .FALSE., STATUS )
                     ELSE
                        CALL CAT_PUT0R( DICAT, VAL__BADR, .TRUE., 
     :                                  STATUS )
                     END IF

                     IF( VVIN .NE. VAL__BADR ) THEN
                        CALL CAT_PUT0R( DVCAT, SQRT( MAX( 0.0, VVIN ) ), 
     :                                  .FALSE., STATUS )
                     ELSE
                        CALL CAT_PUT0R( DVCAT, VAL__BADR, .TRUE., 
     :                                  STATUS )
                     END IF

                     IF( VP .NE. VAL__BADR ) THEN
                        CALL CAT_PUT0R( DPCAT, SQRT( MAX( 0.0, VP ) ), 
     :                                  .FALSE., STATUS )
                     ELSE
                        CALL CAT_PUT0R( DPCAT, VAL__BADR, .TRUE., 
     :                                  STATUS )
                     END IF

                     IF( VT .NE. VAL__BADR ) THEN
                        CALL CAT_PUT0R( DTHCAT, SQRT( MAX( 0.0, VT ) ), 
     :                                  .FALSE., STATUS )
                     ELSE
                        CALL CAT_PUT0R( DTHCAT, VAL__BADR, .TRUE., 
     :                                  STATUS )
                     END IF

                     IF( VIP .NE. VAL__BADR ) THEN
                        CALL CAT_PUT0R( DPICAT, SQRT( MAX( 0.0, VIP ) ), 
     :                                  .FALSE., STATUS )
                     ELSE
                        CALL CAT_PUT0R( DPICAT, VAL__BADR, .TRUE., 
     :                                  STATUS )
                     END IF

                  END IF

*  Append the current row buffer to the catalogue.
                  CALL CAT_RAPND( CI, STATUS )
                  NVEC = NVEC + 1
   
               END IF

            END DO
         END DO

      END IF

*  Display the number of rows written to the catalogue.
      IF( MAKECT ) THEN
         CALL MSG_BLANK( STATUS )
         CALL MSG_SETI( 'N', NVEC )
         CALL MSG_OUT( 'POL1_PLVEC_1', '   ^N vectors written to '//
     :                 'the output catalogue.', STATUS )
         CALL MSG_BLANK( STATUS )
      END IF

 999  CONTINUE

      END
