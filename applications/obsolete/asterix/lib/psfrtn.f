*+  PSF_ANAL - User defined PSF handler
      SUBROUTINE PSF_ANAL( PSID, X0, Y0, QX, QY, DX, DY, INTEG, NX,
     :                                          NY, ARRAY, STATUS )
*
*    Description :
*
*     Returns 2D array of PSF values centered on X0,Y0. There are NX by NY
*     pixels of size DX by DY.
*
*     Copes with   a) GAUSSIAN - 2D Gaussian
*                  b) TOPHAT   - Circular tophat
*                  c) TRIANGLE - Pyramid
*                  d) FLAT_TRI - ramped response with flat centre
*                  e) KING     - King profile
*
*    Method :
*
*     Must be normalised to that if the user requested the given spacing
*     over all the PSF, the total would be unity.
*
*    Deficiencies :
*    Bugs :
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     10 Jul 1989 (DJA):
*        Original version
*     19 Jul 1990 (DJA):
*        Copes with inequal spatial scaling. Some improvements to normalisation
*      9 Mar 1991 (DJA):
*        Gaussian handled by MATH_INTGAU2D
*      2 Aug 1993 (DJA):
*        Added King profile option
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'PSF_PAR'
      INCLUDE 'MATH_PAR'
*
*    Import :
*
      REAL                     DX, DY, X0, Y0,QX,QY
      INTEGER                  NX,NY,PSID
      LOGICAL                  INTEG
*
*    Export :
*
      REAL                     ARRAY(NX,NY)            ! Probability data
*
*    Status :
*
      INTEGER                  STATUS                  ! Run-time error
*
*    Local constants :
*
      INTEGER                  OVERSAMPLE
        PARAMETER              ( OVERSAMPLE = 5 )

      REAL                     ROOT2
        PARAMETER              ( ROOT2 = 1.41421356 )
*
*    Local variables :
*
      CHARACTER*3		KIND			! Form of psf

      REAL                      SIGX, SIGY              ! GAUSSIAN variables
      REAL                      HHYPOT, PROBP, PROBSP   ! TOPHAT variables
      REAL                      H, HR                   ! TRAINGLE variables
      REAL                      FWHM, FWMV, FWZR        ! FLAT_TRI variables

      REAL                      DELX, DELY              ! Offsets from X0,Y0
      REAL                      LHS, BOT                ! Left side and bottom
                                                       ! of ARRAY in radians
      REAL                      R                       ! Distance pixel to X0,Y0
      REAL                     	THETA                   ! PA of pixel wrt X0,Y0
      REAL                      W                       ! Mask width
      REAL			W1, W2			! Stored widths
      REAL                      XP, YP                  ! Pixel centres
      REAL                      XPP, YPP                ! Sub-pixel centres

      INTEGER                   I, J                    ! Loops over output data
      INTEGER                   II, JJ                  ! Subsample loops
*-

*  Check inherited global status
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Decide on form of PSF
      CALL PSF0_GETID0C( PSID, 'Form', KIND, STATUS )

*  Find LHS and BOT in radians
      LHS = X0 + QX - DX * REAL(NX) / 2.0
      BOT = Y0 + QY - DY * REAL(NY) / 2.0

*  Get widths
      CALL PSF0_GETID0R( PSID, 'Width1', W1, STATUS )
      CALL PSF0_GETID0R( PSID, 'Width2', W2, STATUS )

*  Gaussian
      IF ( KIND .EQ. 'GAU' ) THEN

*    Get the full-widths in each dimension, and convert to sigmas
        SIGX = W1 / ( 2.0 * SQRT(2.0*ALOG(2.0)) )
        SIGY = W2 / ( 2.0 * SQRT(2.0*ALOG(2.0)) )

*    Fill in values
        CALL MATH_INTGAU2D( SIGX, SIGY, 0.0,0.0,0.0, QX, QY,
     :                       DX, DY, NX, NY, ARRAY, STATUS )

*  King profile
      ELSE IF ( KIND .EQ. 'KIN' ) THEN

*    Fill in values
        CALL MATH_INTKING2D( W1, W2, 0.0, 0.0, QX, QY,
     :                       DX, DY, NX, NY, ARRAY, STATUS )


*  Lorentz profile
      ELSE IF ( KIND .EQ. 'LOR' ) THEN

*    Fill in values
        CALL MATH_INTLOR2D( W1, W2, 0.0, 0.0, 0.0, QX, QY,
     :                      DX, DY, NX, NY, ARRAY, STATUS )

*  Top hat
      ELSE IF ( KIND .EQ. 'TOP' ) THEN

*    Get radius of tophat
        W = W1 / 2.0

*    Probability per pixel completely inside tophat
        PROBP = ABS(DX*DY) / (MATH__PI*W*W)

*    Process each pixel grid
        DO J = 1, NY

*      Y position of pixel centres in this row
          YP = BOT + (REAL(J)-0.5)*DY

          DO I = 1, NX

*        X position of pixel centre
            XP = LHS + (REAL(I)-0.5)*DX

*        Find distance from psf centre
            R = SQRT( (XP-X0)**2 + (YP-Y0)**2 )

*        Find position angle of pixel
            IF ( (XP-X0) .EQ. 0.0 ) THEN
              THETA = 0.0
            ELSE
              THETA = ATAN2(YP-Y0,XP-X0)
            END IF

*        HHYPOT is the radius from the psf inside which any ARRAY pixel
*        centre is guaranteed to mean the whole pixel being inside the
*        tophat width. This clearly depends on the P.A. of the pixel
*        centre w.r.t. the psf centre. The relation below gives a value
*        of DX/2 on the principal axes rising to a maximum of (DX*root2)/2
*        at 45 degrees to the principal axes.
            HHYPOT = (1.0 + SIN(2.0*THETA)*(ROOT2-1.0))*ABS(DX)/2.0

*        Pixel well inside tophat radius
            IF ( R .LT. (W-HHYPOT) ) THEN

              ARRAY(I,J) = PROBP

*        Well outside?
            ELSE IF ( R .GT. (W+HHYPOT) ) THEN

              ARRAY(I,J) = 0.0

*       Sub-sample
            ELSE

*          Probability per sub-pixel
              PROBSP = PROBP / REAL(OVERSAMPLE**2)

*          Sub-sample the ARRAY pixel accumulating probability from those
*          subpixels inside the tophat.
              ARRAY(I,J) = 0.0
              DO JJ = 1, OVERSAMPLE
                YPP = YP - 0.5*DY + (REAL(JJ-1)+0.5)*DY/OVERSAMPLE
                DO II = 1, OVERSAMPLE
                  XPP = XP - 0.5*DX + (REAL(II-1)+0.5)*DX/OVERSAMPLE

*              Radius of subpixel
                  R = SQRT( (XPP-X0)**2 + (YPP-Y0)**2 )

*              Inside
                  IF ( R .LT. (W-HHYPOT/OVERSAMPLE) ) THEN

                    ARRAY(I,J) = ARRAY(I,J) + PROBSP

*              Some part of pixel inside top-hat radius?
                  ELSE IF ( R .LT. (W+HHYPOT/OVERSAMPLE) ) THEN

*                The permissible range of (R-W) is from -1.0 to 1.0 in units
*                of HHYPOT/OVERSAMPLE, corresponding to the radial extremity
*                of a subpixel being just inside W to just outside W. Map
*                the above range onto 1.0 to 0.0
                    ARRAY(I,J) = ARRAY(I,J) + PROBSP *
     :                    (0.5 - (R-W)/(HHYPOT/OVERSAMPLE)/2.0 )

                  END IF

                END DO
              END DO

            END IF

          END DO
        END DO

      ELSE IF ( KIND .EQ. 'TRI' ) THEN

*      Get width of triangle
        W = W1

*      Get height of pyramid and multiply by sub-pixel area.
        H = (3.0 / W**2) * ABS(DX*DY) / OVERSAMPLE**2

*      Make W the half-width now
        W = W / 2.0

*      Process each pixel grid
        DO J = 1, NY

*        Y position of pixel centres in this row
          YP = BOT + (REAL(J)-0.5)*DY

          DO I = 1, NX

*          X position of pixel centre
            XP = LHS + (REAL(I)-0.5)*DX

*          Sub-sample the ARRAY pixel accumulating probability from those
*          subpixels inside the triangle
            ARRAY(I,J) = 0.0
            DO JJ = 1, OVERSAMPLE
              YPP = YP - 0.5*DY + (REAL(JJ-1)+0.5)*DY/OVERSAMPLE
              DELY = ABS(YPP - Y0)

*            Row inside perimeter?
              IF ( DELY .LE. W ) THEN

                DO II = 1, OVERSAMPLE
                  XPP = XP - 0.5*DX + (REAL(II-1)+0.5)*DX/OVERSAMPLE

*                Distance from centre of pyramid to centre of pixel
                  DELX = ABS(XPP - X0)

*                Inside perimeter?
                  IF ( DELX .LE. W ) THEN

*                  Decide which of DELX or DELY to use for height estimate
                    IF ( DELX .GE. DELY) THEN
                      HR = H * (1.0-DELX/W)
                    ELSE
                      HR = H * (1.0-DELY/W)
                    END IF

*                  Add in probability
                    ARRAY(I,J) = ARRAY(I,J) + HR

                  END IF

                END DO

              END IF

            END DO

          END DO

        END DO

      ELSE IF ( KIND .EQ. 'FTR' ) THEN

*      Get various widths
        FWZR = W1
        FWHM = W2
        FWMV = 2.0*FWHM - FWZR

*      Height of pyramid without top cut off
        H = 3.0 / (FWZR**2-FWMV**3/FWZR)

*      Height of plateau
        HR = H*(FWZR-FWMV)/FWZR

*      Convert height to integrated probabilities
        HR = HR * ABS(DX*DY) / OVERSAMPLE**2

*      Make W the half-width of the triangle
        W = FWZR / 2.0

*      Set R to width of slopey bit round the edge
        R = (FWZR - FWMV) / 2.0

*      Process each pixel grid
        DO J = 1, NY

*        Y position of pixel centres in this row
          YP = BOT + (REAL(J)-0.5)*DY

          DO I = 1, NX

*          X position of pixel centre
            XP = LHS + (REAL(I)-0.5)*DX

*          Sub-sample the ARRAY pixel accumulating probability from those
*          subpixels inside the triangle
            ARRAY(I,J) = 0.0
            DO JJ = 1, OVERSAMPLE
              YPP = YP - 0.5*DY + (REAL(JJ-1)+0.5)*DY/OVERSAMPLE
              DELY = ABS(YPP - Y0)

*            Row inside perimeter?
              IF ( DELY .LE. W ) THEN

                DO II = 1, OVERSAMPLE
                  XPP = XP - 0.5*DX + (REAL(II-1)+0.5)*DX/OVERSAMPLE

*                Distance from centre of pyramid to centre of pixel
                  DELX = ABS(XPP - X0)

*                Inside perimeter?
                  IF ( DELX .LE. W ) THEN

*                  Inside plateau
                    IF ( MAX(DELX,DELY) .LE. (FWMV/2.0) ) THEN

                      ARRAY(I,J) = ARRAY(I,J) + HR

                    ELSE

                      ARRAY(I,J) = ARRAY(I,J) +
     :                    HR * (1.0-(MAX(DELX,DELY)-FWMV/2.0)/R)

                    END IF

                  END IF

                END DO

              END IF

            END DO

          END DO

        END DO

      END IF

      END

*+  PSF_ANAL_HINT - Analytic defined PSF initialisation
      SUBROUTINE PSF_ANAL_HINT( PSID, HINT, DATA, STATUS )
*
*    Description :
*
*     Return hints about the analytic psfs. These are particularly simple as
*      they are not functions of parameter of any detector.
*
*    Method :
*    Deficiencies :
*    Authors :
*
*     David J. Allan (ROSAT, University of Birmingham)
*
*    History :
*
*     23 Dec 93 : Original (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'PSF_PAR'
*
*    Import :
*
      INTEGER                 	PSID
      CHARACTER*(*)           	HINT		 	! Hint name
*
*    Export :
*
      BYTE			DATA(*)			! Hint data
*
*    Status :
*
      INTEGER STATUS
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Radial symmetry?
      IF ( HINT .EQ. PSF_H_RADSYM ) THEN

*      Analytic psfs are not functions of detector position
        CALL ARR_COP1L( 1, .TRUE., DATA, STATUS )

*    Energy dependent
      ELSE IF ( HINT .EQ. PSF_H_ENDEP ) THEN

*      Analytic psfs are not functions of energy
        CALL ARR_COP1L( 1, .FALSE., DATA, STATUS )

      ELSE
        STATUS = SAI__ERROR
        CALL MSG_SETC( 'H', HINT )
        CALL ERR_REP( ' ', 'Unknown psf hint /^H/', STATUS )

      END IF

      END

*+  PSF_ANAL_INIT - Analytic defined PSF initialisation
      SUBROUTINE PSF_ANAL_INIT( PSID, STATUS )
*
*    Description :
*
*     Gets a user defined PSF and associates it with a psf object
*
*    Environment parameters :
*
*     MASK = CHAR(R)
*     AUX = ...
*
*    Method :
*    Deficiencies :
*    Authors :
*
*     David J. Allan (ROSAT, University of Birmingham)
*
*    History :
*
*     03 Nov 89 : Original (DJA)
*     19 Jul 90 : Establish correct spatial scale if axes different (DJA)
*      2 Aug 93 : Added King profile (DJA)
*     15 Dec 93 : Removed reference back to PSF system. Generalised to
*                 work on arbitrary axis numbers. (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
*
*    Import :
*
      INTEGER			PSID
*
*    Status :
*
      INTEGER STATUS
*
*    Functions :
*
      INTEGER                 CHR_LEN
      REAL                    PSF0_GETAXDR, PSF0_GETAXTOR
      LOGICAL                 STR_ABBREV, PSF0_GETAXOK
      EXTERNAL			PSF_ANAL
      EXTERNAL			PSF_ANAL_HINT, PSF_ANAL_PFL
*
*    Local variables :
*
      CHARACTER*3		CH3			! 3 chars of CHOICE
      CHARACTER*30            CHOICE            ! User's choice for PSF
      CHARACTER*30            FTPAR             ! FLAT_TRI parameters
      CHARACTER*80            PROMPT            !
      CHARACTER*40            LABEL, UNITS

      REAL                    GWIDTH            ! Width of a mask
      REAL                    X_DR, Y_DR        ! Axis scales
      REAL                    X_TOR, Y_TOR      ! Axis unit conversions
      REAL			W1, W2			! Widths

      INTEGER                 	BEG, IC           ! Character pointers
      INTEGER                 	X_AX,Y_AX,E_AX,T_AX	! Axis numbers

      LOGICAL                 	X_OK, Y_OK        	! Axes ok?
*-

*  Check inherited global status
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise
      X_OK = .FALSE.
      Y_OK = .FALSE.

*  Tell user about user-definitions
      CALL MSG_PRNT( 'PSF definitions available')
      CALL MSG_PRNT( ' ')
      CALL MSG_PRNT( 'GAUSSIAN     - Gaussian response        '/
     :              /'  TRIANGLE     - Triangular response')
      CALL MSG_PRNT( 'TOPHAT       - Circular tophat function '/
     :              /'  FLAT_TRI     - Triangle with flat top')
      CALL MSG_PRNT( 'LORENTZ      - Lorentzian response      '/
     :              /'  KING         - King profile ')
      CALL MSG_PRNT( ' ')

*  Define the default mask
      CALL USI_DEF0C( 'MASK', 'GAUSSIAN', STATUS )

*  Get user's choice
      CALL USI_GET0C( 'MASK', CHOICE, STATUS )
      CALL USI_CANCL( 'MASK', STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*  Identify spatial axes
      CALL PSF_QAXES( PSID, X_AX, Y_AX, E_AX, T_AX, STATUS )

*  Tell the user about the pixel size if we have a dataset.
      X_OK = PSF0_GETAXOK( PSID, X_AX, STATUS )
      Y_OK = PSF0_GETAXOK( PSID, Y_AX, STATUS )
      X_DR = PSF0_GETAXDR( PSID, X_AX, STATUS )
      Y_DR = PSF0_GETAXDR( PSID, Y_AX, STATUS )
      X_TOR = PSF0_GETAXTOR( PSID, X_AX, STATUS )
      Y_TOR = PSF0_GETAXTOR( PSID, Y_AX, STATUS )
      CALL PSF0_GETAXTXT( PSID, X_AX, LABEL, UNITS, STATUS )

*  Are dataset axes ok?
      IF ( X_OK .AND. Y_OK .AND. .NOT.
     :                           STR_ABBREV(UNITS,'PIXELS') ) THEN
        CALL MSG_SETR( 'XP', ABS(X_DR/X_TOR) )
        CALL MSG_SETC( 'UNITS', UNITS )
        IF ( ABS(1.0-ABS(X_DR/Y_DR)) .LT. 0.01 ) THEN
          CALL MSG_PRNT( 'Dataset pixels are ^XP ^UNITS square' )
        ELSE
          CALL MSG_SETR( 'YP', ABS(Y_DR/Y_TOR) )
          CALL MSG_PRNT( 'Dataset pixels are ^XP by ^YP ^UNITS' )
        END IF
      END IF

*    Check on each possibility
      IF ( STR_ABBREV(CHOICE,'GAUSSIAN') ) THEN

        CH3 = 'GAU'

*      We need a width for the gaussian
        PROMPT = 'Gaussian FWHM in '//UNITS(:CHR_LEN(UNITS))
        CALL USI_PROMT( 'AUX', PROMPT(:CHR_LEN(PROMPT)), STATUS )
        CALL USI_DEF0R( 'AUX', ABS(X_DR/X_TOR), STATUS )
        CALL USI_GET0R( 'AUX', GWIDTH, STATUS )
        CALL USI_CANCL( 'AUX', STATUS )
        IF ( STATUS .NE. SAI__OK ) GOTO 99

*      Convert it to radians
        W1 = GWIDTH*X_TOR
        W2 = GWIDTH*Y_TOR

*  King profile
      ELSE IF ( STR_ABBREV(CHOICE,'KING') ) THEN

        CH3 = 'KIN'

        CALL USI_PROMT( 'AUX', 'King core radius (in '/
     :           /UNITS(:CHR_LEN(UNITS))//') and index', STATUS )


 39     CALL USI_GET0C( 'AUX', FTPAR, STATUS )
        CALL USI_CANCL( 'AUX', STATUS )
        IF ( STATUS .NE. SAI__OK ) THEN
          GOTO 99
        ELSE
          IC = 1
          CALL CHR_FIWS( FTPAR, IC, STATUS )
          BEG = IC
          CALL CHR_FIWE( FTPAR, IC, STATUS )
          CALL CHR_CTOR( FTPAR(BEG:IC), W1, STATUS )
          IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERR_FLUSH( STATUS )
            CALL MSG_PRNT( 'Error parsing core radius' )
            GOTO 39
          END IF

          IC = IC + 1
          CALL CHR_FIWS( FTPAR, IC, STATUS )
          BEG = IC
          CALL CHR_FIWE( FTPAR, IC, STATUS )
          CALL CHR_CTOR( FTPAR(BEG:IC), W2, STATUS )
          IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERR_FLUSH( STATUS )
            CALL MSG_PRNT( 'Error parsing profile index' )
            GOTO 39
          END IF

        END IF

*      Convert core radius to radians
        W1 = W1 * X_TOR

*  Lorentzian profile
      ELSE IF ( STR_ABBREV(CHOICE,'LORENTZ') ) THEN

        CH3 = 'LOR'

*    We need a width for the Lorentzian
        PROMPT = 'Lorentzian HWHM in '//UNITS(:CHR_LEN(UNITS))
        CALL USI_PROMT( 'AUX', PROMPT(:CHR_LEN(PROMPT)), STATUS )
        CALL USI_DEF0R( 'AUX', ABS(X_DR/X_TOR), STATUS )
        CALL USI_GET0R( 'AUX', GWIDTH, STATUS )
        CALL USI_CANCL( 'AUX', STATUS )
        IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Convert it to radians
        W1 = GWIDTH*X_TOR
        W2 = GWIDTH*Y_TOR

      ELSE IF ( STR_ABBREV(CHOICE,'TOPHAT') ) THEN

        CH3 = 'TOP'

*      Get width of tophat
        PROMPT = 'Tophat full width in '//UNITS(:CHR_LEN(UNITS))
        CALL USI_PROMT('AUX', PROMPT(:CHR_LEN(PROMPT)), STATUS )
        CALL USI_GET0R( 'AUX', GWIDTH, STATUS )
        CALL USI_CANCL( 'AUX', STATUS )

*      Convert it to radians
        W1 = GWIDTH*X_TOR

      ELSE IF ( STR_ABBREV(CHOICE,'TRIANGLE') ) THEN

        CH3 = 'TRI'

*      Get width of tophat
        PROMPT = 'Triangle zero-point full width in '//UNITS
     :                                        (:CHR_LEN(UNITS))
        CALL USI_PROMT('AUX', PROMPT(:CHR_LEN(PROMPT)), STATUS )
        CALL USI_GET0R( 'AUX', GWIDTH, STATUS )
        CALL USI_CANCL( 'AUX', STATUS )

*      Convert it to radians
        W1 = GWIDTH*X_TOR
        W2 = GWIDTH*Y_TOR

      ELSE IF ( STR_ABBREV(CHOICE,'FLAT_TRI') ) THEN

        CH3 = 'FTR'

        CALL MSG_PRNT( 'Please supply full-width at zero response'/
     :                  /' (FWZR) and full-width half-max (FWHM)' )

*      Get zero response and full-width of flat-top
        PROMPT = 'FWZR and FWHM in '//UNITS(:CHR_LEN(UNITS))//' eg. 6,4'
        CALL USI_PROMT('AUX', PROMPT(:CHR_LEN(PROMPT)), STATUS )
 49     CALL USI_GET0C( 'AUX', FTPAR, STATUS )
        CALL USI_CANCL( 'AUX', STATUS )
        IF ( STATUS .NE. SAI__OK ) THEN
          GOTO 99
        ELSE
          IC = 1
          CALL CHR_FIWS( FTPAR, IC, STATUS )
          BEG = IC
          CALL CHR_FIWE( FTPAR, IC, STATUS )
          CALL CHR_CTOR( FTPAR(BEG:IC), W1, STATUS )
          IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERR_FLUSH( STATUS )
            CALL MSG_PRNT( 'Error parsing FWZR' )
            GOTO 49
          END IF

          CALL CHR_FIWS( FTPAR, IC, STATUS )
          BEG = IC
          CALL CHR_FIWE( FTPAR, IC, STATUS )
          CALL CHR_CTOR( FTPAR(BEG:IC), W2, STATUS )
          IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERR_FLUSH( STATUS )
            CALL MSG_PRNT( 'Error parsing FWHM' )
            GOTO 49
          END IF

        END IF

        IF ( W1 .LT. W2 ) THEN
          CALL MSG_PRNT( 'The zero point full width can''t be LESS'/
     :                                            /' than the FWHM!' )
          GOTO 49
        END IF

*      Convert them both to radians
        W1 = W1*X_TOR
        W2 = W2*X_TOR

      ELSE
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'Unrecognised PSF specifier', STATUS )

      END IF

*  Store instance data
      CALL PSF0_SETID0C( PSID, 'Form', CH3, STATUS )
      CALL PSF0_SETID0R( PSID, 'Width1', W1, STATUS )
      CALL PSF0_SETID0R( PSID, 'Width2', W2, STATUS )

*  Set methods
      CALL PSF0_SETRTN( PSID, 'Data', PSF_ANAL, STATUS )
      CALL PSF0_SETRTN( PSID, 'Eprofile', PSF_ANAL_PFL, STATUS )
      CALL PSF0_SETRTN( PSID, 'Hint', PSF_ANAL_HINT, STATUS )

*  Tidy up
 99   IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'PSF_ANAL_INIT', STATUS )
      END IF

      END


*+  PSF_ANAL_PFL - Analytic psf energy profiling
      SUBROUTINE PSF_ANAL_PFL( PSID, X0, Y0, NFRAC, FRAC, RADII,
     :                         STATUS )
*
*    Description :
*
*     Energy profiles analytic psfs. Note that inthe TRIANGLE and
*     FLAT_TRI options, it is the half width of a box which found
*     and not the radius of a circle.
*
*    Method :
*
*     The methods used for each analytic profile are given below,
*
*       GAUSSIAN	- Returns bad status which forces the PSF
*                         system routine to do the job.
*       TOPHAT          - Enclosed energy is E = (r/R)^2 where R
*                         is the radius of the tophat.
*       TRIANGLE        - The box enclosing a fraction E of the
*                         energy is given by E = 3x^2-2x^3 where
*                         x is the normalised radius varying from
*                         zero to one.
*       FLAT_TRI        - The central plateau is handled like the
*                         tophat function. The extremity is changed
*                         into an equivalent part of an untruncated
*                         pyramid.
*
*    Accuracy :
*
*     The TOPHAT algorithm is exact. The accuracy on the TRIANGLE
*     and FLAT_TRI is better than 1 part in 10^4.
*
*    Deficiencies :
*
*     Would be nice to do the GAUSSIAN one, but the integration was
*     too difficult at the time.
*
*    Bugs :
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     14 Dec 91 : Original (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'PSF_PAR'
*
*    Import :
*
      INTEGER                  PSID,NFRAC
      REAL                     FRAC(*), X0, Y0
*
*    Export :
*
      REAL                     RADII(*)
*
*    Status :
*
      INTEGER                  STATUS                  ! Run-time error
*
*    Local variables :
*
      CHARACTER*3		KIND			! Form of psf

      REAL                     FP, FWHM, FWMV, FWZR    ! FLAT_TRI variables
      REAL                     FBP
      REAL                     R,OLDR                  !
      REAL                     W                       ! A mask width

      INTEGER                  I                       ! Loop over radii
*-

*  Check inherited global status
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Decide on form of PSF
      CALL PSF0_GETID0C( PSID, 'Form', KIND, STATUS )

*  Gaussian?
      IF ( KIND .EQ. 'GAU' ) THEN

        STATUS = SAI__ERROR

*  Tophat?
      ELSE IF ( KIND .EQ. 'TOP' ) THEN

*      Get half-width of mask in radians
        CALL PSF0_GETID0R( PSID, 'Width1', W, STATUS )
        W = W / 2.0

*      For each energy fraction
        DO I = 1, NFRAC
          RADII(I) = W * SQRT(FRAC(I))
        END DO

*  Pyramid
      ELSE IF ( KIND .EQ. 'TRI' ) THEN

*    Get width of mask in radians
        CALL PSF0_GETID0R( PSID, 'Width1', W, STATUS )

*    For each energy fraction
        DO I = 1, NFRAC

*      First guess at radius, scaled from 0.0 to 1.0
          R = SQRT(FRAC(I))

*      Iterate until convergence
          OLDR = -999.0
          DO WHILE ( ABS(R-OLDR)/R .GT. 0.0001 )
            OLDR = R
            R = R - (3.0*R**2-2.0*R**3-FRAC(I)) / (6.0*R-6.0*R**2)
          END DO

*      Convert to real distance
          RADII(I) = R*W/2.0

        END DO

*  Flat pyramid
      ELSE IF ( KIND .EQ. 'FTR' ) THEN

*    Get various widths
        CALL PSF0_GETID0R( PSID, 'Width1', FWZR, STATUS )
        CALL PSF0_GETID0R( PSID, 'Width2', FWHM, STATUS )
        FWMV = 2.0*FWHM - FWZR

*    Fraction of energy inside plateau
        FP = 3.0*(FWZR-FWMV)*FWMV**2/(FWZR**3-FWMV**3)

*    For each energy fraction
        DO I = 1, NFRAC

*      Requested fraction inside plateau
          IF ( FRAC(I) .LE. FP ) THEN

*        Just a tophat normalised to FP
            RADII(I) = (FWMV/2.0) * SQRT(FRAC(I)/FP)

          ELSE

*        An energy fraction in excess of FP can be translated into a
*        real excess volume in an untruncated pyramid of height H.

*        Find the fraction of energy enclosed in the untruncated pyramid
*        at the radius corresponding to the edge of the plateau, FWMV.
            R = (FWMV/FWZR)
            FBP = 3.0*R**2-2.0*R**3

*        Scale our (FRAC(I) between FP and 1) into the range (FBP to 1.0)
            FBP = FBP + (1.0-FBP)*(FRAC(I)-FP)/(1.0-FP)

*        Now use iteration scheme to get normalised radius
c            R = R + SQRT((FRAC(I)-FP)/(1.0-FP))
            OLDR = -999.0
            DO WHILE ( ABS(R-OLDR)/R .GT. 0.0001 )
              OLDR = R
              R = R - (3.0*R**2-2.0*R**3-FBP) / (6.0*R-6.0*R**2)
            END DO

*        And convert to real units
            RADII(I) = R * FWZR/2.0

          END IF

        END DO

*  Report error
      ELSE
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'Unknown analytic psf code', STATUS )

      END IF

      END

*+  PSF_RADIAL - 1D user supplied psf handler
      SUBROUTINE PSF_RADIAL( PSID, X0, Y0, QX, QY, DX, DY, INTEG, NX,
     :                                             NY, ARRAY, STATUS )
*
*    Description :
*
*     Returns 2D array of PSF values centered on X0,Y0. There are NX by NY
*     pixels of size DX by DY. The data is found by interpolation of a 1-d
*     radial surface brightness profile the characteristics of which are
*     stored in the PSF_RADD common block.
*
*    Method :
*
*     Must be normalised to that if the user requested the given spacing
*     over all the PSF, the total would be unity.
*
*    Deficiencies :
*    Bugs :
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*      2 Feb 93 : Original (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'PSF_PAR'
      INCLUDE 'MATH_PAR'
*
*    Import :
*
      REAL                     DX, DY, X0, Y0,QX,QY
      INTEGER                  NX,NY,PSID
      LOGICAL                  INTEG
*
*    Export :
*
      REAL                     ARRAY(NX,NY)            ! Psf data
*
*    Status :
*
      INTEGER                  STATUS                  ! Run-time error
*
*    Functions :
*
      REAL                     PSF_RADIAL_INT
*
*    Local constants :
*
      REAL                     RTOS                    ! Radians -> arcsec
        PARAMETER              ( RTOS = MATH__RTOD*3600.0 )
*
*    Local variables :
*
      REAL                     LNORM                   ! Normalisation constant
      REAL                     NORM                    ! Normalisation constant
      REAL                     RPS                     ! Radius of sub-pix ^ 2
      REAL                     SDX, SDY                ! Sub-pixel bin sizes
      REAL                     SUM                     ! Cumulative value
      REAL                     XP0, YP0                ! Major pixel centre
      REAL                     XPS, YPS                ! Sub-pixel pixel centre
      REAL                     YPS2                    ! Sub-pixel distance

      INTEGER                  I, J                    ! Major pixel loops
      INTEGER                  II, JJ                  ! Sub-pixel loops
      INTEGER                  MNX, MNY                ! Local calc bounds
      INTEGER                  XSUB, YSUB              ! Sub-pixel factors
      INTEGER			NELM			! Length of profile
      INTEGER			DPTR, APTR, WPTR	! Profile data?

      LOGICAL                  SYMMETRIC               ! Symmetric about centre?
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Fix sub-pixelling
      XSUB = 3
      YSUB = 3
      NORM = 1.0

*    Base coordinates
      XP0 = ( - REAL(NX)/2.0 ) * DX + X0 + QX
      YP0 = ( - REAL(NY)/2.0 ) * DY + Y0 + QY

*    Symmetric?
      SYMMETRIC = ( ( X0 .EQ. 0.0 ) .AND. ( Y0 .EQ. 0.0 )
     :        .AND. ( QX .EQ. 0.0 ) .AND. ( QY .EQ. 0.0 ) )

*    Bounds for calculation
      IF ( SYMMETRIC ) THEN

*      The "lower left corner" of the array. The +1 guarantees that the
*      centre pixel gets done for odd values of NX/Y
        MNX = (NX+1)/2
        MNY = (NY+1)/2

      ELSE

*      The whole array
        MNX = NX
        MNY = NY

      END IF

*  Extract instance data
      CALL PSF0_GETID0I( PSID, 'Dim', NELM, STATUS )
      CALL PSF0_GETID0I( PSID, 'Dptr', DPTR, STATUS )
      CALL PSF0_GETID0I( PSID, 'Aptr', APTR, STATUS )
      CALL PSF0_GETID0I( PSID, 'Wptr', WPTR, STATUS )

*    For each point requiring calculation
      DO J = 1, MNY

*      Find Y sub-pixelling
C        YSUB = SPIX( YP0 + DY*REAL(J-1), DY )
        SDY = DY / YSUB

        DO I = 1, MNX

*        Zero
          SUM = 0.0

*        Find X sub-pixelling
C          XSUB = SPIX( XP0 + DX*REAL(I-1), DX )
          SDX = DX / XSUB

*        Correct normalisation for sub-pixel and pixel size
          LNORM = ABS(SDX*SDY)/NORM

*        Y position of first sub-pixel centre
          YPS = YP0 + DY*(J-1) + 0.5*SDY

*        For each sub-pixel row
          DO JJ = 0, YSUB-1

*          Y distance from psf centre
            YPS2 = (YPS-Y0)**2

*          X position of first sub-pixel centre
            XPS = XP0 + DX*(I-1) + 0.5*SDX

*          For each sub-pixel
            DO II = 0, XSUB-1

*            Radius of sub-pixel squared
              RPS = (XPS-X0)**2 + YPS2

*            Value of gaussian
              SUM = SUM + PSF_RADIAL_INT( SQRT(RPS), NELM,
     :                %VAL(DPTR), %VAL(APTR), %VAL(WPTR), STATUS )

*            Next sub-pixel
              XPS = XPS + SDX

            END DO

*          Next row of sub-pixels
            YPS = YPS + SDY

          END DO

*        Set ARRAY value
          ARRAY(I,J) = SUM*LNORM

        END DO

      END DO

*    Copy array around if symmetrical
      IF ( SYMMETRIC ) THEN

*      Transfer data to other 3 quadrants
        JJ = NY
        DO J = 1, MNY
          II = NX
          DO I = 1, MNX
            ARRAY(II,J) = ARRAY(I,J)
            ARRAY(II,JJ) = ARRAY(I,J)
            ARRAY(I,JJ) = ARRAY(I,J)
            II = II - 1
          END DO
          JJ = JJ - 1
        END DO

      END IF

      END



*+  PSF_RADIAL_INT - 2D user supplied psf handler
      REAL FUNCTION PSF_RADIAL_INT( R, N, DAT, AX, WID, STATUS )
*
*    Description :
*
*     Returns 2D array of PSF values centered on X0,Y0. There are NX by NY
*     pixels of size DX by DY.
*
*    Method :
*
*     Must be normalised to that if the user requested the given spacing
*     over all the PSF, the total would be unity.
*
*    Deficiencies :
*    Bugs :
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     26 Jun 90 : Original (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
*
*    Status :
*
      INTEGER                  STATUS                  ! Run-time error
*
*    Import :
*
      REAL                     R 		       ! Test radius in radians
      INTEGER                  N                       ! No. points in profile
      REAL                     DAT(*),AX(*),WID(*)     ! Profile data
*
*    Local variables :
*
      REAL                     PSFV                    ! Return value
      REAL                     XIN(5), YIN(5)          ! Interpolation cooords

      INTEGER                  CP                      ! Profile pixel of R
      INTEGER                  I, J
      INTEGER                  JL, JM, JU              ! Used in binary search
*-

*  Find radial bin
      JL = 1
      JU = N
      DO WHILE ( (JU-JL) .GT. 1 )
        JM = (JL+JU)/2
        IF ( R .GT. (AX(JM)+WID(JM)/2.0) ) THEN
          JL = JM
        ELSE
          JU = JM
        END IF
      END DO
      CP = JL

*    Interpolate about pixel CP
      IF ( CP .LE. N ) THEN
        DO I = 1, 5
          J = CP + I - 3
          IF ( J .GT. 0 ) THEN
            XIN(I) = AX(J)
            YIN(I) = DAT(J)
          ELSE
            XIN(I) = -AX(-J+1)
            YIN(I) = DAT(-J+1)
          END IF
        END DO

        CALL MATH_INTERP( 5, XIN, YIN, 1, R, 2, PSFV, STATUS )

      ELSE
        PSFV = 0.0
      END IF

*    Set return value
      PSF_RADIAL_INT = PSFV

      END

*+  PSF_RADIAL_INIT - Radially defined PSF initialisation
      SUBROUTINE PSF_RADIAL_INIT( PSID, STATUS )
*
*    Description :
*
*     Gets a 1-D radial PSF and associates it with a psf object
*
*    Environment parameters :
*
*     MASK = CHAR(R)
*     AUX = ...
*
*    Method :
*    Deficiencies :
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*      2 Feb 93 : Original (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'ADI_PAR'
*
*    Import :
*
      INTEGER			PSID
*
*    Status :
*
      INTEGER STATUS
*
*    Function declarations :
*
      INTEGER                 CHR_LEN
*
*    Local variables :
*
      CHARACTER*80            TNAME             ! Table file name
      CHARACTER*40            UNITS             ! Axis units

      REAL                    	TOR               	! Conversion to radians

      INTEGER                 	DIMS(1)  		! Size of data array
      INTEGER                 	DPTR              	! Ptr to data
      INTEGER                 	NVAL,NDIM              	! Dimensionality
      INTEGER			TFID			! Tabular dataset
      INTEGER                 	TDPTR, TAPTR, TWPTR     ! Dynamic data

      LOGICAL                 	OK                	! General validity check
      LOGICAL                 	VALID             	! Have we a valid dataset?
*-

*  Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get user's choice of table
      CALL USI_PROMT( 'MASK', 'Dataset containing radial profile',
     :                                                    STATUS )
      CALL USI_GET0C( 'MASK', TNAME, STATUS )
      CALL USI_CANCL( 'MASK', STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*  Try to open file
      CALL ADI_FOPEN( TNAME(:CHR_LEN(TNAME)), 'BinDS', 'READ', TFID,
     :                STATUS )

*  Check the data
      CALL BDI_CHK( TFID, 'Data', VALID, STATUS )
      CALL BDI_GETSHP( TFID, 1, DIMS, NDIM, STATUS )
      IF ( VALID ) THEN

*    Map if ok
        CALL BDI_MAPR( TFID, 'Data', 'READ', DPTR, STATUS )

*    If ok then make a copy of this data and store
        IF ( STATUS .EQ. SAI__OK ) THEN

*      Store size
          CALL PSF0_SETID0I( PSID, 'Dim', DIMS(1), STATUS )

*      Copy profile data
          CALL DYN_MAPR( 1, DIMS(1), TDPTR, STATUS )
          CALL PSF0_SETID0I( PSID, 'Dptr', TDPTR, STATUS )
          CALL ARR_COP1R( DIMS(1), %VAL(DPTR), %VAL(TDPTR), STATUS )

*      Get axis info
          CALL BDI_AXCHK( TFID, 1, 'Data', OK, STATUS )

*      Copy axis data
          CALL DYN_MAPR( 1, DIMS(1), TAPTR, STATUS )
          CALL BDI_AXGET1R( TFID, 1, 'Data', DIMS(1),
     :                      %VAL(TAPTR), NVAL, STATUS )
          CALL PSF0_SETID0I( PSID, 'Aptr', TAPTR, STATUS )

*      Copy axis widths
          CALL DYN_MAPR( 1, DIMS(1), TWPTR, STATUS )
          CALL BDI_AXGET1R( TFID, 1, 'Width', DIMS(1),
     :                      %VAL(TWPTR), NVAL, STATUS )
          CALL PSF0_SETID0I( PSID, 'Wptr', TWPTR, STATUS )

*      Get axis units
          CALL BDI_AXGET0C( TFID, 1, 'Units', UNITS, STATUS )

*      Convert axis units
          CALL CONV_UNIT2R( UNITS, TOR, STATUS )

*      Normalise the profile
          CALL PSF_RADIAL_NORM( DIMS(1), %VAL(TDPTR),
     :              %VAL(TAPTR), %VAL(TWPTR), TOR, STATUS )

        END IF

*    Inform user
        IF ( STATUS .EQ. SAI__OK ) THEN
          CALL MSG_SETC( 'FILE', TNAME )
          CALL MSG_PRNT( 'PSF read in from file ^FILE' )
        END IF

      END IF

*  Energy radii not present
      CALL PSF0_SETID0I( PSID, 'Nlev', 0, STATUS )

*  Release from BDI
      IF ( ( STATUS .EQ. SAI__OK ) .AND. VALID ) THEN
        CALL ADI_FCLOSE( TFID, STATUS )
      END IF

*  Tidy up
 99   IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'PSF_RADIAL_INIT', STATUS )
      END IF

      END



*+  PSF_RADIAL_NORM - Normalise a radial profile
      SUBROUTINE PSF_RADIAL_NORM( N, DAT, AX, WID, TOR, STATUS )
*
*    Description :
*
*
*    Method :
*     <description of how the subroutine works - for programmer info>
*    Deficiencies :
*    Bugs :
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*      3 Feb 93 : Original (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'MATH_PAR'
*
*    Import :
*
      INTEGER                 N			! No. of profile bins
      REAL                    AX(*)             ! Profile axis values
      REAL                    WID(*)            ! Profile axis widths
      REAL                    TOR               ! Conversion to radians
*
*    Import-Export :
*
      REAL                    DAT(*)            ! Profile data
*
*    Status :
*
      INTEGER STATUS
*
*    Local variables :
*
      REAL                    NORM              ! Profile normalisation

      INTEGER                 I    		! Loop over profile
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Find normalisation constant after converting to radians
      NORM = 0.0
      DO I = 1, N
        AX(I) = AX(I)*TOR
        WID(I) = WID(I)*TOR
        NORM = NORM + DAT(I)*MATH__PI*2.0*AX(I)*WID(I)
      END DO

*    Normalise
      DO I = 1, N
        DAT(I) = DAT(I) / NORM
      END DO

      END

*+  PSF_RESPFILE - Extract psf data from response and give to user
      SUBROUTINE PSF_RESPFILE( PSID, X0, Y0, QX, QY, DX, DY, INTEG, NX,
     :                                              NY, ARRAY, STATUS )
*
*    Description :
*
*     Returns 2D array of PSF values centered on X0,Y0. There are NX by NY
*     pixels of size DX by DY.
*
*    Method :
*
*     Must be normalised to that if the user requested the given spacing
*     over all the PSF, the total would be unity.
*
*    Deficiencies :
*    Bugs :
*    Authors :
*
*     David J. Allan (ROSAT, University of Birmingham)
*
*    History :
*
*     26 Jun 90 : Original (DJA)
*      3 Mar 94 : Added resampling option for non-zero QX,QY (DJA)
*     17 Aug 94 : Fixed bug in _INT routine where the first psf was
*                 returned if the input array matched the expanded psf
*                 array, regardless of the psf requested (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'PRM_PAR'
      INCLUDE 'PSF_PAR'
*
*    Import :
*
      REAL                     DX, DY, X0, Y0,QX,QY
      INTEGER                  PSID, NX, NY
      LOGICAL                  INTEG
*
*    Export :
*
      REAL                     ARRAY(NX,NY)		! Psf data
*
*    Status :
*
      INTEGER                  STATUS                  	! Run-time error
*
*    Local variables :
*
      REAL			RBASE(5),RSCALE(5)	! Axis attributes
      REAL                      R			! Off-axis angle

      INTEGER			DIMS(5),NDIM		! Response dimensions
      INTEGER                   IPSF			! Sequential psf number
      INTEGER                   IPTR			! Pointer into index
      INTEGER                   IE, IR, IX, IY		! Response axis bins
      INTEGER			PHALO, PHAHI		! PHA band
      INTEGER			RDPTR, RIPTR, RRPTR	! Response data/index
      INTEGER			RNXY			! Resample size

      LOGICAL			PHADEF			! PHA band defined?
      LOGICAL			RADIAL			! Psf is radial?
*-

*  Check inherited global status
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Find PHA bounds
      CALL ADI_CGET0L( PSID, 'PhaDef', PHADEF, STATUS )
      CALL ADI_CGET0I( PSID, 'PhaLo', PHALO, STATUS )
      CALL ADI_CGET0I( PSID, 'PhaHi', PHAHI, STATUS )

*  Response data and index
      CALL PSF0_GETID0I( PSID, 'Dptr', RDPTR, STATUS )
      CALL PSF0_GETID0I( PSID, 'Iptr', RIPTR, STATUS )

*  Details of resampling space
      CALL PSF0_GETID0I( PSID, 'Rptr', RRPTR, STATUS )
      CALL PSF0_GETID0I( PSID, 'Rnxy', RNXY, STATUS )

*  Get dimensions of response
      CALL PSF0_GETID1I( PSID, 'Dims', 5, DIMS, NDIM, STATUS )

*  Get axis attributes
      CALL PSF0_GETID1R( PSID, 'Base', 5, RBASE, NDIM, STATUS )
      CALL PSF0_GETID1R( PSID, 'Scale', 5, RSCALE, NDIM, STATUS )

*  Radial response?
      CALL PSF0_GETID0L( PSID, 'Radial', RADIAL, STATUS )
      IF ( RADIAL ) THEN

*      Choose radial bin
        R = SQRT( X0**2 + Y0**2 )
        IR = INT(0.5+(R-RBASE(3)) / RSCALE(3)) + 1
        IR = MIN( IR, DIMS(3) )

*      Choose energy bin
        IF ( PHADEF .AND. (NDIM.GT.3) ) THEN
          IE = INT((PHALO-RBASE(4)) / RSCALE(4)) + 1
        ELSE
          IE = 1
        END IF

*      This selects the psf
        IPSF = (IE-1)*DIMS(3) + IR

      ELSE

*      Choose spatial bins
        IX = INT(0.5+(X0-RBASE(3)) / RSCALE(3)) + 1
        IY = INT(0.5+(Y0-RBASE(4)) / RSCALE(4)) + 1
        IF ( (IX.LT.1) .OR. (IX.GT.DIMS(3)) .OR.
     :       (IY.LT.1) .OR. (IY.GT.DIMS(4)) ) THEN
          STATUS = SAI__ERROR
          CALL MSG_SETI( 'XR', IX )
          CALL MSG_SETI( 'YR', IY )
          CALL MSG_SETI( 'NX', DIMS(3) )
          CALL MSG_SETI( 'NY', DIMS(4) )
          CALL ERR_REP( ' ', 'Psf requested at response grid '/
     :                  /'(^XR,^YR) when the bounds are (^NX,^NY). '/
     :                  /'Will continue but there may an SPRESP '/
     :                  /'problem', STATUS )
          CALL ERR_FLUSH( STATUS )

*      Force IX,IY into range
          IX = MAX(1,IX)
          IY = MAX(1,IY)
          IX = MIN(IX,DIMS(3))
          IY = MIN(IY,DIMS(4))

        END IF

*      Choose energy bin
        IF ( PHADEF .AND. (NDIM.GT.4) ) THEN
          IE = INT((PHALO-RBASE(5)) / RSCALE(5)) + 1
        ELSE
          IE = 1
        END IF

*      This selects the psf
        IPSF = (IE-1)*DIMS(3)*DIMS(4) + (IY-1)*DIMS(3) + IX

      END IF

*    The psf index allows us to index the INDEX array
      IPTR = RIPTR + 3*VAL__NBI*(IPSF-1)

*    Use that index to unpack the response data
      CALL PSF_RESPFILE_INT( %VAL(IPTR), %VAL(RDPTR),
     :                       NX, NY, ARRAY, STATUS )

*    Non-zero offset?
      IF ( (QX.NE.0.0) .AND. (QY.NE.0.0) ) THEN

*      Reallocate resampling workspace if not enough
        IF ( (RRPTR .GT. 0) .AND. (NX*NY.LT.RNXY) ) THEN
          CALL DYN_UNMAP( RRPTR, STATUS )
          RRPTR = 0
        END IF
        IF ( RRPTR .EQ. 0 ) THEN
          CALL DYN_MAPR( 1, NX*NY, RRPTR, STATUS )
          CALL PSF0_SETID0I( PSID, 'Rptr', RRPTR, STATUS )
          CALL PSF0_SETID0I( PSID, 'Rnxy', NX*NY, STATUS )
        END IF

*    Make copy of psf
        CALL ARR_COP1R( NX*NY, ARRAY, %VAL(RRPTR), STATUS )

*    Resample it
        CALL PSF_RESAMPLE( NX, NY, %VAL(RRPTR), QX/DX, QY/DY,
     :                                     0, ARRAY, STATUS )

      END IF

*  Abort point
 99   CONTINUE

      END



*+  PSF_RESPFILE_INT - Extract psf data from response and give to user
      SUBROUTINE PSF_RESPFILE_INT( INDEX, DATA, NX, NY, ARRAY, STATUS )
*
*    Description :
*
*     Returns 2D array of PSF values centered on X0,Y0. There are NX by NY
*     pixels of size DX by DY.
*
*    Method :
*
*     Must be normalised to that if the user requested the given spacing
*     over all the PSF, the total would be unity.
*
*    Deficiencies :
*    Bugs :
*    Authors :
*
*     David J. Allan (ROSAT, University of Birmingham)
*
*    History :
*
*      5 Jan 94 : Original (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
*
*    Import :
*
      INTEGER			INDEX(3)		!
      INTEGER                   NX, NY			! Dimensions of output
      REAL                      DATA(*)			! Response data
*
*    Export :
*
      REAL                     	ARRAY(-NX/2:NX/2,       ! Psf data
     :                                -NY/2:NY/2)
*
*    Status :
*
      INTEGER                  	STATUS                  ! Run-time error
*
*    Local variables :
*
      INTEGER		        DJ, DIJ			! Loops over DATA
      INTEGER                   DNX, DNY		! Packed data size
      INTEGER                   I, J 			! Loops over ARRAY
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    The full widths of the packed array
      DNX = INDEX(2)*2 + 1
      DNY = INDEX(3)*2 + 1

*    Exact match?
      IF ( (DNX.EQ.NX) .AND. (DNY.EQ.NY) ) THEN

        CALL ARR_COP1R( NX*NY, DATA(INDEX(1)), ARRAY, STATUS )

      ELSE

*      Zero the output unless the output is smaller than the DATA in both
*      dimensions.
        IF ( (NX.GT.DNX) .OR. (NY.GT.DNY) ) THEN
          DO J = -NY/2, NY/2
            DO I = -NX/2, NX/2
              ARRAY(I,J) = 0.0
            END DO
          END DO
        END IF

*      Loop over output values filling in pixels
        DJ = INDEX(1) + MAX(0,DNY/2-NY/2) * DNX
        DO J = -MIN(DNY/2,NY/2), MIN(DNY/2,NY/2)
          DIJ = DJ + MAX(0,DNX/2-NX/2)
          DO I = -MIN(DNX/2,NX/2), MIN(DNX/2,NX/2)
            ARRAY(I,J) = DATA(DIJ)
            DIJ = DIJ + 1
          END DO
          DJ = DJ + DNX
        END DO

      END IF

      END


*+  PSF_RESPFILE_HINT - Spatial response hint handler
      SUBROUTINE PSF_RESPFILE_HINT( PSID, HINT, DATA, STATUS )
*
*    Description :
*
*     Return hints about the psfs in spatial responses.
*
*    Method :
*    Deficiencies :
*    Authors :
*
*     David J. Allan (ROSAT, University of Birmingham)
*
*    History :
*
*     23 Dec 1993 (DJA):
*        Original version
*     13 Jun 1995 (DJA):
*        Return absolute value of min(dx,dy) for granularity in
*        rectangular case
*     21 Dec 1995 (DJA):
*        Energy granularity
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'PSF_PAR'
*
*    Import :
*
      INTEGER                 	PSID			!
      CHARACTER*(*)           	HINT		 	! Hint name
*
*    Export :
*
      BYTE			DATA(*)			! Hint data
*
*    Status :
*
      INTEGER STATUS
*
*    Local variables:
*
      REAL			RSCALE(5)		! Axis attributes

      INTEGER			DIMS(5), NDIM		! Response dimensions

      LOGICAL			RADIAL			! Radial psf?
*-

*  Check inherited global status
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Psf is radial?
      CALL PSF0_GETID0L( PSID, 'Radial', RADIAL, STATUS )

*  Radial symmetry?
      IF ( HINT .EQ. PSF_H_RADSYM ) THEN

*    Radially symmetric flag from common block
        CALL ARR_COP1L( 1, RADIAL, DATA, STATUS )

*    Energy dependent
      ELSE IF ( HINT .EQ. PSF_H_ENDEP ) THEN

*    Get dimensions of response
        CALL PSF0_GETID1I( PSID, 'Dims', 5, DIMS, NDIM, STATUS )

*    Energy dependent if last dimension has dimension other than one
        CALL ARR_COP1L( 1, (DIMS(NDIM).NE.1), DATA, STATUS )

*    Maximum radius
      ELSE IF ( HINT .EQ. PSF_H_MAXRAD ) THEN

*    Get dimensions of response
        CALL PSF0_GETID1I( PSID, 'Dims', 5, DIMS, NDIM, STATUS )

*    Can't be more than radius of a response psf
        CALL ARR_COP1I( 1, DIMS(1)/2, DATA, STATUS )

*  Spatial granularity
      ELSE IF ( HINT .EQ. PSF_H_SPATGRAN ) THEN

*    Get psf axis attributes
        CALL PSF0_GETID1R( PSID, 'Scale', 5, RSCALE, NDIM, STATUS )

*      Radial psf?
        IF ( RADIAL ) THEN
          CALL ARR_COP1R( 1, RSCALE(3), DATA, STATUS )
        ELSE
          CALL ARR_COP1R( 1, ABS(MIN(RSCALE(3),RSCALE(4))),
     :                    DATA, STATUS )
        END IF

*  Energy granularity
      ELSE IF ( HINT .EQ. PSF_H_EGRAN ) THEN

*    Get psf axis attributes
        CALL PSF0_GETID1I( PSID, 'Dims', 5, DIMS, NDIM, STATUS )
        CALL PSF0_GETID1R( PSID, 'Scale', 5, RSCALE, NDIM, STATUS )

*    Radial psf?
        CALL ARR_COP1R( 1, RSCALE(NDIM), DATA, STATUS )

      ELSE
        STATUS = SAI__ERROR
        CALL MSG_SETC( 'H', HINT )
        CALL ERR_REP( ' ', 'Unknown psf hint /^H/', STATUS )

      END IF

      END



*+  PSF_RESPFILE_CLOSE - Delete a psf handle
      SUBROUTINE PSF_RESPFILE_CLOSE( PSID, STATUS )
*
*    Description :
*
*    Environment parameters :
*
*    Method :
*    Deficiencies :
*    Authors :
*
*     David J. Allan (ROSAT, University of Birmingham)
*
*    History :
*
*     10 Jan 96 : Original (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
*
*    Import :
*
      INTEGER			PSID
*
*    Status :
*
      INTEGER STATUS
*
*    Local variables:
*
      INTEGER			PTR
*-

*  Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Release dynamic data
      CALL PSF0_GETID0I( PSID, 'Iptr', PTR, STATUS )
      CALL DYN_UNMAP( PTR, STATUS )
      CALL PSF0_GETID0I( PSID, 'Dptr', PTR, STATUS )
      CALL DYN_UNMAP( PTR, STATUS )

      END


*+  PSF_RESPFILE_INIT - Response defined PSF initialisation
      SUBROUTINE PSF_RESPFILE_INIT( PSID, STATUS )
*
*    Description :
*
*     Gets a PSF from a spatial response and associates it with a psf object
*
*    Environment parameters :
*
*     MASK = CHAR(R)
*     AUX = ...
*
*    Method :
*    Deficiencies :
*    Authors :
*
*     David J. Allan (ROSAT, University of Birmingham)
*
*    History :
*
*     16 Dec 93 : Original (DJA)
*      2 Mar 94 : Asks for mean photon energy if not defined externally
*                 and the response has a non-unity energy dimension. Now
*                 checks that binning of response matches that of the
*                 dataset. (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*
*    Import :
*
      INTEGER			PSID
*
*    Status :
*
      INTEGER STATUS
*
*    Functions :
*
      REAL 			PSF0_GETAXDR
      REAL 			PSF0_GETAXTOR
      EXTERNAL			PSF_RESPFILE
      EXTERNAL			PSF_RESPFILE_CLOSE
      EXTERNAL			PSF_RESPFILE_HINT
*
*    Local variables :
*
      CHARACTER*1		DAXC			! Axis character
      CHARACTER*40		EUNITS			! Energy axis units
      CHARACTER*(DAT__SZLOC)  	LOC              	! Dataset locator
      CHARACTER*(DAT__SZLOC)  	SLOC              	! Response locator
      CHARACTER*80		TEXT			! AUX prompt
      CHARACTER*30		VERSION			! SPRESP version

      REAL			BASE(5), SCALE(5)	! Axis attributes
      REAL			ENERGY			! Mean photon energy
      REAL			R_BASE, R_SCALE		! Reponse axis attrs
      REAL			D_SCALE			! Dataset scale
      REAL			MAJOR			! Creator version id
      REAL			SPARR(2)
      REAL			TOR			! Dataset radian factor

      INTEGER			DAX			! Dataset axis no.
      INTEGER		      	DIM			! Size of response axis
      INTEGER			DIMS(5), NDIM		! Response full size
      INTEGER			DPTR, IPTR		! Response data
      INTEGER			DX, DY			!
      INTEGER			FID			! Dataset handle
      INTEGER			FSTAT			! i/o status code
      INTEGER                 	IAX			! Loop over axes
      INTEGER                 	NELM 			! Number of elements mapped
      INTEGER			REVISION		! Creator revision no.
      INTEGER			RLIM			! Radial limit
      INTEGER			SID			! Response identifier
      INTEGER			TLEN			! Prompt length
      INTEGER			X_AX, Y_AX, E_AX, T_AX	! Axis identifiers

      LOGICAL                 	IN_DATASET        	! Response found in dataset
      LOGICAL			PHADEF			! PHA band defined?
      LOGICAL			RADIAL			! Radial response?
*-

*  Check inherited status
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Extract locator
      CALL ADI_CGET0I( PSID, 'FileID', FID, STATUS )
      CALL ADI1_GETLOC( FID, LOC, STATUS )

*  Does the dataset have an attached spatial response?
      CALL HDX_FIND( LOC, 'MORE.ASTERIX.SPATIAL_RESP', SLOC, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN
        CALL MSG_PRNT( 'Found spatial response in dataset...' )
        IN_DATASET = .TRUE.
        CALL ADI1_MKFILE( SLOC, 'READ', SID, STATUS )

      ELSE

*    Annul error
        CALL ERR_ANNUL( STATUS )

*    Get response name by prompting
        CALL USI_PROMT( 'MASK', 'Name of Asterix spatial response file',
     :                  STATUS )
        CALL USI_ASSOC( 'MASK', 'BinDS', 'READ', SID, STATUS )
        IF ( STATUS .NE. SAI__OK ) GOTO 99
        CALL USI_CANCL( 'MASK', STATUS )
        CALL ADI1_GETLOC( SID, SLOC, STATUS )
        IN_DATASET = .FALSE.

      END IF

*  Get expanded response dimensions
      CALL CMP_GET1I( SLOC, 'DIMS', 5, DIMS, NDIM, STATUS )
      CALL PSF0_SETID1I( PSID, 'Dims', NDIM, DIMS, STATUS )
      CALL PSF0_SETID0I( PSID, 'Rnxy', 0, STATUS )

*  Get radial limit so we can use BDI to access the axis structures
      CALL CMP_GET0I( SLOC, 'RLIMIT', RLIM, STATUS )
      DX = DIMS(1)
      DY = DIMS(2)
      DIMS(1) = RLIM*2 + 1
      DIMS(2) = RLIM*2 + 1

*  Link the response object to a BinDS so we can access its axis info
      CALL BDI_LINK( 'BinDS', NDIM, DIMS, 'REAL', SID, STATUS )
      DIMS(1) = DX
      DIMS(2) = DY

*  Load index into memory
      CALL CMP_SIZE( SLOC, 'INDEX', NELM, STATUS )
      CALL DYN_MAPI( 1, NELM, IPTR, STATUS )
      CALL CMP_GETVI( SLOC, 'INDEX', NELM, %VAL(IPTR), NELM, STATUS )
      CALL PSF0_SETID0I( PSID, 'Iptr', IPTR, STATUS )

*  Load the response into memory
      CALL CMP_SIZE( SLOC, 'DATA_ARRAY', NELM, STATUS )
      CALL DYN_MAPR( 1, NELM, DPTR, STATUS )
      CALL PSF0_SETID0I( PSID, 'Dptr', DPTR, STATUS )
      CALL CMP_GETVR( SLOC, 'DATA_ARRAY', NELM, %VAL(DPTR),
     :                NELM, STATUS )

*  Is it a radial response?
      CALL CMP_GET0L( SLOC, 'RADIAL', RADIAL, STATUS )
      CALL PSF0_SETID0L( PSID, 'Radial', RADIAL, STATUS )

*  Get creator version id
      CALL CMP_GET0C( SLOC, 'VERSION', VERSION, STATUS )

*  Extract version number
      READ( VERSION, '(15X,F3.1,1X,I1)', IOSTAT=FSTAT ) MAJOR, REVISION
      MAJOR = MAJOR + REVISION / 100.0

*  Axis attributes only written correctly from 1.7-3 onwards
      IF ( MAJOR .GE. 1.73 ) THEN

*    Identify spatial axes
        CALL PSF_QAXES( PSID, X_AX, Y_AX, E_AX, T_AX, STATUS )

*    Get X and Y axis parameters, and compare with dataset
        DO IAX = 1, 2

*      Choose dataset axis
          IF ( IAX .EQ. 1 ) THEN
            DAX = X_AX
            DAXC = 'X'
          ELSE
            DAX = Y_AX
            DAXC = 'Y'
          END IF

*      Get response axis attributes
          CALL BDI_AXGET1R( SID, IAX, 'SpacedData', 2, SPARR, DIM,
     :                      STATUS )
          R_BASE = SPARR(1)
          R_SCALE = SPARR(2)

*      Get dataset axis attributes
          D_SCALE = PSF0_GETAXDR( PSID, X_AX, STATUS )
          TOR = PSF0_GETAXTOR( PSID, X_AX, STATUS )

*        Compare bin sizes
          IF ( ABS((ABS(R_SCALE)-ABS(D_SCALE/TOR))/R_SCALE)
     :         .GT. 1.0E-4 ) THEN
            CALL MSG_SETC( 'AX', DAXC )
            STATUS = SAI__ERROR
            CALL ERR_REP( ' ', 'Response ^AX axis binning is incomp'/
     :                    /'atible with the dataset. Re-attach the '/
     :                    /'response using SPRESP', STATUS )
            GOTO 99
          END IF

        END DO

*  End of switch on SPRESP version
      END IF

*  Get axis info
      DO IAX = 3, NDIM
        CALL BDI_AXGET1R( SID, IAX, 'SpacedData', 2, SPARR, DIM,
     :                      STATUS )
        BASE(IAX) = SPARR(1)
        SCALE(IAX) = SPARR(2)
      END DO
      CALL PSF0_SETID1R( PSID, 'Base', NDIM, BASE, STATUS )
      CALL PSF0_SETID1R( PSID, 'Scale', NDIM, SCALE, STATUS )

*    If the PHA band is not defined, and the response has a significant
*    energy dimension, then we need a mean photon energy to index that
*    energy dimension.
      CALL ADI_CGET0L( PSID, 'PhaDef', PHADEF, STATUS )
      IF ( (DIMS(NDIM).GT.1) .AND. .NOT. PHADEF ) THEN

*    Construct prompt
        CALL BDI_AXGET0C( SID, NDIM, 'Units', EUNITS, STATUS )
        CALL MSG_SETC( 'UNITS', EUNITS )
        CALL MSG_MAKE( 'Mean photon energy in ^UNITS', TEXT, TLEN )
        CALL USI_PROMT( 'AUX', TEXT(:TLEN), STATUS )

*    Get user response
        CALL USI_GET0R( 'AUX', ENERGY, STATUS )
        IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Set PHA band
        CALL ADI_CPUT0R( PSID, 'PhaLo', ENERGY, STATUS )
        CALL ADI_CPUT0R( PSID, 'PhaHi', ENERGY, STATUS )
        CALL ADI_CPUT0L( PSID, 'PhaDef', .TRUE., STATUS )

      END IF

*  Annul response
      IF ( IN_DATASET ) THEN
        CALL DAT_ANNUL( SLOC, STATUS )
      ELSE
        CALL USI_ANNUL( 'MASK', STATUS )
      END IF

*  Reset workspace
      CALL PSF0_SETID0I( PSID, 'Rptr', 0, STATUS )

*  Set methods
      CALL PSF0_SETRTN( PSID, 'Data', PSF_RESPFILE, STATUS )
      CALL PSF0_SETRTN( PSID, 'Hint', PSF_RESPFILE_HINT, STATUS )
      CALL PSF0_SETRTN( PSID, 'Close', PSF_RESPFILE_CLOSE, STATUS )

*  Abort point
 99   CONTINUE

      END


*+  PSF_TABULAR - 2D user supplied psf handler
      SUBROUTINE PSF_TABULAR( PSID, X0, Y0, QX, QY, DX, DY, INTEG, NX,
     :                                             NY, ARRAY, STATUS )
*
*    Description :
*
*     Returns 2D array of PSF values centered on X0,Y0. There are NX by NY
*     pixels of size DX by DY.
*
*    Method :
*
*     Must be normalised to that if the user requested the given spacing
*     over all the PSF, the total would be unity.
*
*    Deficiencies :
*    Bugs :
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     26 Jun 90 : Original (DJA)
*      2 Feb 93 : Added resampling option for non-zero QX,QY (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
*
*    Import :
*
      REAL                     DX, DY, X0, Y0,QX,QY
      INTEGER                  NX,NY,PSID
      LOGICAL                  INTEG
*
*    Export :
*
      REAL                     ARRAY(-NX/2:NX/2,-NY/2:NY/2)
*
*    Status :
*
      INTEGER                  STATUS                  ! Run-time error
*
*    Local variables :
*
      INTEGER                  	TPTR                    ! Temp array for resample
      INTEGER			DIMS(2), NDIM, DPTR
*-

*  Check inherited global status
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Extract tabular control data
      CALL PSF0_GETID1I( PSID, 'Dims', 2, DIMS, NDIM, STATUS )
      CALL PSF0_GETID0I( PSID, 'Dptr', DPTR, STATUS )

*  Call internal routine
      CALL PSF_TABULAR_INT( DIMS(1), DIMS(2),
     :                      %VAL(DPTR), NX, NY, ARRAY, STATUS )

*  Non-zero offset?
      IF ( (QX.NE.0.0) .AND. (QY.NE.0.0) ) THEN
        CALL DYN_MAPR( 1, NX*NY, TPTR, STATUS )
        CALL ARR_COP1R( NX*NY, ARRAY, %VAL(TPTR), STATUS )
        CALL PSF_RESAMPLE( NX, NY, %VAL(TPTR), QX/DX, QY/DY, 0, ARRAY,
     :                                                        STATUS )
        CALL DYN_UNMAP( TPTR, STATUS )
      END IF

      END



*+  PSF_TABULAR_INT - 2D user supplied psf handler
      SUBROUTINE PSF_TABULAR_INT( INX, INY, INDATA, NX,
     :                              NY, ARRAY, STATUS )
*
*    Description :
*
*     Returns 2D array of PSF values centered on X0,Y0. There are NX by NY
*     pixels of size DX by DY.
*
*    Method :
*
*     Must be normalised to that if the user requested the given spacing
*     over all the PSF, the total would be unity.
*
*    Deficiencies :
*    Bugs :
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     26 Jun 90 : Original (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
*
*    Import :
*
      INTEGER                  INX,INY,NX,NY
      REAL                     INDATA(-INX/2:INX/2,-INY/2:INY/2)
*
*    Export :
*
      REAL                     ARRAY(-NX/2:NX/2,-NY/2:NY/2)
*
*    Status :
*
      INTEGER                  STATUS                  ! Run-time error
*
*    Local variables :
*
      INTEGER                  I, J
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Copy data
      DO J = -NY/2, NY/2
         DO I = -NX/2, NX/2
            ARRAY(I,J) = INDATA(I,J)
         END DO
      END DO

      END

*+  PSF_TABULAR_INIT - Tabular defined PSF initialisation
      SUBROUTINE PSF_TABULAR_INIT( PSID, STATUS )
*
*    Description :
*
*     Gets a 2-d tabular image and associates it with a psf object
*
*    Environment parameters :
*
*     MASK = CHAR(R)
*     AUX = ...
*
*    Method :
*    Deficiencies :
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     26 Jun 90 : Original (DJA)
*     27 Oct 90 : Looks for energy levels and radii components (DJA)
*     24 Nov 91 : Use internal BDA stuff (DJA)
*      4 Aug 92 : Don't capitalise file name (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'ADI_PAR'
      INCLUDE 'DAT_PAR'
*
*    Import :
*
      INTEGER			PSID
*
*    Status :
*
      INTEGER STATUS
*
*    Function declarations :
*
      INTEGER                 CHR_LEN
      EXTERNAL			PSF_TABULAR
*
*    Local variables :
*
      CHARACTER*(DAT__SZLOC)  	PLOC              	! PSF structure locator
      CHARACTER*80            	TNAME             	! Table file name
      CHARACTER*40            	UNITS             	! Spatial units

      REAL			ELEVS(10), RADII(10)
      REAL			SPARR(2)		! Axis definition
      REAL			TOR			! Radian conversion

      INTEGER			IDUM			! Dummy argument
      INTEGER                 	NDIM, DIMS(ADI__MXDIM)  ! Size of data array
      INTEGER			NLEV			! Number of levels
      INTEGER                 	DPTR              	! Ptr to data
      INTEGER                 	TDPTR              	! Dyn ptr to data
      INTEGER			TFID			! Table identifier

      LOGICAL                 ELEVS_OK          ! Energy levels structure ok?
      LOGICAL                 RADII_OK          ! Energy radii structure ok?
      LOGICAL                 VALID             ! Have we a valid dataset?
*-

*  Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get user's choice of table
      CALL USI_PROMT( 'MASK', 'Name of a 2D dataset containing psf',
     :                                                      STATUS )
      CALL USI_GET0C( 'MASK', TNAME, STATUS )
      CALL USI_CANCL( 'MASK', STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*  Try to open file
      CALL ADI_FOPEN( TNAME(:CHR_LEN(TNAME)), 'BinDS', 'READ', TFID,
     :                STATUS )

*  Check the data
      CALL BDI_CHK( TFID, 'Data', VALID, STATUS )
      CALL BDI_GETSHP( TFID, 2, DIMS, NDIM, STATUS )
      IF ( VALID ) THEN

        IF ( NDIM .NE. 2 ) THEN
          CALL MSG_PRNT( 'This isn''t a 2D dataset!' )
          STATUS = SAI__ERROR
        END IF

*    Map if ok
        CALL BDI_MAPR( TFID, 'Data', 'READ', DPTR, STATUS )

*    Get axis units
        CALL BDI_AXGET0C( TFID, 1, 'Units', UNITS, STATUS )
        CALL CONV_UNIT2R( UNITS, TOR, STATUS )
        IF ( STATUS .NE. SAI__OK ) THEN
          CALL ERR_ANNUL( STATUS )
          TOR = 1.0
        END IF

*      Get axis values
        CALL BDI_AXGET1R( TFID, 1, 'SpacedData', 2, SPARR, IDUM,
     :                    STATUS )
        TOR = ABS(TOR*SPARR(2))
        CALL PSF0_SETID0R( PSID, 'Tor', TOR, STATUS )

*      If ok then make a copy of this data and store
        IF ( STATUS .EQ. SAI__OK ) THEN
          CALL PSF0_SETID1I( PSID, 'Dims', 2, DIMS, STATUS )
          CALL DYN_MAPR( 2, DIMS, TDPTR, STATUS )
          CALL PSF0_SETID0I( PSID, 'Dptr', TDPTR, STATUS )
          CALL ARR_COP1R( DIMS(1)*DIMS(2), %VAL(DPTR),
     :                    %VAL(TDPTR), STATUS )
        END IF

*      Inform user
        IF ( STATUS .EQ. SAI__OK ) THEN
          CALL MSG_SETC( 'FILE', TNAME )
          CALL MSG_PRNT( 'PSF read in from file ^FILE' )
        END IF

      END IF

*  Look for energy radii
      NLEV = 0
      CALL ADI1_LOCPSF( TFID, .FALSE., PLOC, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN
        CALL DAT_THERE( PLOC, 'ELEVS', ELEVS_OK, STATUS )
        CALL DAT_THERE( PLOC, 'RADII', RADII_OK, STATUS )
        IF ( ELEVS_OK .AND. RADII_OK ) THEN
          CALL CMP_GET1R( PLOC, 'ELEVS', 10, ELEVS, NLEV, STATUS )
          CALL CMP_GET1R( PLOC, 'RADII', 10, RADII, NLEV, STATUS )
        END IF
      ELSE
        CALL ERR_ANNUL( STATUS )
      END IF
      CALL PSF0_SETID0I( PSID, 'Nlev', NLEV, STATUS )

*  Release from BDI
      IF ( ( STATUS .EQ. SAI__OK ) .AND. VALID ) THEN
        CALL ADI_FCLOSE( TFID, STATUS )
      END IF

*  Set methods
      CALL PSF0_SETRTN( PSID, 'Data', PSF_TABULAR, STATUS )

*  Tidy up
 99   IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'PSF_TABULAR_INIT', STATUS )
      END IF

      END

*+  PSF_TABULAR_PFL - Tabular energy profiling
      SUBROUTINE PSF_TABULAR_PFL( PSID, X0, Y0, NFRAC, FRAC, RADII,
     :         STATUS )
*
*    Description :
*
*     Returns radii at which the tabulated psf encloses the specified
*     energy fractions.
*
*    Method :
*
*    Deficiencies :
*    Bugs :
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     14 Dec 91 : Original (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
*
*    Import :
*
      INTEGER                  PSID,NFRAC
      REAL                     FRAC(*), X0, Y0
*
*    Export :
*
      REAL                     RADII(*)
*
*    Status :
*
      INTEGER                  STATUS                  ! Run-time error
*
*    Local variables:
*
      REAL			TOR
      INTEGER			DIMS(2), NDIM, DPTR
*-

*  Check inherited global status
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Extract tabular control data
      CALL PSF0_GETID0R( PSID, 'Tor', TOR, STATUS )
      CALL PSF0_GETID1I( PSID, 'Dims', 2, DIMS, NDIM, STATUS )
      CALL PSF0_GETID0I( PSID, 'Dptr', DPTR, STATUS )

*  Call internal routine
      CALL PSF_TABULAR_PFL_INT( PSID, DIMS(1), DIMS(2), %VAL(DPTR),
     :                          TOR, NFRAC, FRAC, RADII, STATUS )

      END



*+  PSF_TABULAR_PFL_INT -
      SUBROUTINE PSF_TABULAR_PFL_INT( PSID, NX, NY, DATA, TOR,
     :                                NFRAC, FRAC, RADII, STATUS )
*
*    Description :
*
*     Extracts energy radii in pixels from the array DATA(NX,NY). Both NX
*     and NY are assumed to be odd.
*
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     14 Dec 91 : Original (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
*
*    Import :
*
      INTEGER                  PSID,NFRAC
      REAL                     FRAC(*), TOR
      INTEGER                  NX,NY
      REAL                     DATA(-NX/2:NX/2,-NY/2:NY/2)
*
*    Export :
*
      REAL                     RADII(*)
*
*    Status :
*
      INTEGER                  STATUS                  ! Run-time error
*
*    Local constants :
*
      INTEGER                  MAXR                ! Maximum likely value NX/2
        PARAMETER              ( MAXR = 300 )
      INTEGER                  OVERSAMPLE          ! Pixel oversampling
        PARAMETER              ( OVERSAMPLE = 3 )
*
*    Local variables :
*
      REAL                     PROX(0:MAXR),PROY(0:MAXR)
      REAL                     XP,YP,TOT
      INTEGER                  AMAXR,I,J,R,II,JJ
*-

*    Initialise
      TOT = 0.0
      AMAXR = 0
      DO I = 0, MAXR
        PROX(I) = REAL(I) / OVERSAMPLE
        PROY(I) = 0.0
      END DO

*    Accumulate profile
      DO J = -NY/2, NY/2
        DO I = -NX/2, NX/2
          TOT = TOT + DATA(I,J)

*        Oversample
          DO JJ = 1, OVERSAMPLE
            YP = REAL(J) - 0.5 + (REAL(JJ)-0.5)/OVERSAMPLE
            DO II = 1, OVERSAMPLE
              XP = REAL(I) - 0.5 + (REAL(II)-0.5)/OVERSAMPLE
              R = NINT(SQRT(XP*XP+YP*YP)*REAL(OVERSAMPLE))
              PROY(R) = PROY(R) + DATA(I,J)/(OVERSAMPLE**2)
            END DO
          END DO
          AMAXR = MAX( R, AMAXR )

        END DO
      END DO

*    Convert to enclosed energy
      DO I = 1, AMAXR
        PROY(I) = PROY(I) + PROY(I-1)
      END DO

*    For each requested fraction
      DO I = 1, NFRAC

*      Inside first pixel?
        IF ( FRAC(I) .LT. PROY(0) ) THEN
          RADII(I) = 0.5 * SQRT(FRAC(I)/PROY(0)) / OVERSAMPLE

*      Outside last pixel - guess from gradient over last 2 points
        ELSE IF ( FRAC(I) .GT. TOT ) THEN

          RADII(I) = PROX(AMAXR) + (PROX(AMAXR)-PROX(AMAXR-2))*
     :               (FRAC(I)-PROY(AMAXR))/(PROY(AMAXR)-PROY(AMAXR-2))

*      Perform a fit
        ELSE

          J = 1
          DO WHILE ( FRAC(I) .GT. PROY(J) )
            J = J + 1
          END DO
          RADII(I) = PROX(J-1) + (FRAC(I)-PROY(J-1))/
     :                  (PROY(J)-PROY(J-1)) / REAL(OVERSAMPLE)

        END IF

*      Convert to radians from fractional pixels
        RADII(I) = RADII(I) * TOR

      END DO

      END

*+  PSF_WFC_CALIN - Initialise the WFC cal library
      SUBROUTINE PSF_WFC_CALIN( STATUS )
*
*    Description :
*
*    Method :
*
*    Deficiencies :
*    Bugs :
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     14 Mar 1996 (DJA):
*        Original version
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
*
*    Status :
*
      INTEGER                  STATUS                  ! Run-time error
*-

*  Check inherited global status
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise CAL
      CALL CAL_INIT( STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL ERR_REP( ' ', 'Unable to open WFC calibration '/
     :         /'database. Check assignment of CAL_WFC_MASTER '/
     :         /'environment variable.', STATUS )
      END IF

      END


*+  PSF_WFC_LOADD - Load detector info from file and store in PSID
      SUBROUTINE PSF_WFC_LOADD( FID, ISSUR, PSID, STATUS )
*
*    Description :
*
*    Method :
*
*    Deficiencies :
*    Bugs :
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     10 Jul 89 : Original (DJA)
*     23 Apr 90 : Checks detector and filter info from dataset (DJA)
*     19 May 90 : Bit more robust if no sort data present (DJA)
*     23 May 90 : Changed over to new CAL system. This routine now controls
*                 the survey psf. See PWFC for pointed phase psf. (DJA)
*     28 Jun 90 : Prompts for filter if not found (DJA)
*      6 Jul 90 : Suppressed error if BASE_MJD not found (DJA)
*     17 Jul 90 : Filter translation table added (DJA)
*     19 Jul 90 : Filter translation done by WFC_FILT_CONV (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'MATH_PAR'
      INCLUDE 'PSF_PAR'
*
*    Status :
*
      INTEGER                  STATUS                  ! Run-time error
*
*    Import :
*
      INTEGER			PSID,FID
      LOGICAL			ISSUR
*
*    Functions :
*
      INTEGER                   CAL_FILT_S2N
      INTEGER                   CHR_LEN
*
*    Local variables :
*
      CHARACTER*(DAT__SZLOC)    LOC                     ! Locator from FID
      CHARACTER*(DAT__SZLOC)    SLOC                    ! SORT locator
      CHARACTER*80              CID                     ! Filter description

      DOUBLE PRECISION		MJD			! MJD of observation

      REAL			ENER			! Energy for filter
      REAL                      IRIS                    ! Sort iris value

      INTEGER                   CALFN                   ! CAL filter id
      INTEGER                   IFILT                   ! Dataset filter id
      INTEGER			MCP			! Detector number
      INTEGER			TIMID			! Timing info

      LOGICAL                  DATASET,FILTER_OK, IRIS_OK      ! Sort components there?
*-

*  Check inherited global status
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Set the SURVEY flag
      CALL PSF0_SETID0L( PSID, 'Survey', ISSUR, STATUS )
      IRIS_OK = .FALSE.
      FILTER_OK = .FALSE.

*  Extract locator
      CALL ADI1_GETLOC( FID, LOC, STATUS )

*    Is locator valid
      CALL DAT_VALID( LOC, DATASET, STATUS )
      IF ( DATASET ) THEN

*    Try and get MJD from dataset
        CALL TCI_GETID( FID, TIMID, STATUS )
        CALL ADI_CGET0D( TIMID, 'MJDObs', MJD, STATUS )
        IF ( STATUS .NE. SAI__OK ) THEN
          CALL ERR_ANNUL( STATUS )
	  MJD = 48000.0D0
        END IF
        CALL PSF0_SETID0D( PSID, 'MJD', MJD, STATUS )

*    Get the detector id from CAL
        CALL CIN_SET_DET( MJD, MCP, STATUS )
        IF ( STATUS .NE. SAI__OK ) THEN
          CALL MSG_PRNT( 'No detector id present - '/
     :        /'assuming detector 2 for psf access' )
          STATUS = SAI__OK
          MCP = 2
        END IF
        CALL PSF0_SETID0I( PSID, 'MCP', MCP, STATUS )

*    Get filter id
        CALL ADI1_LOCSORT( FID, .FALSE., SLOC, STATUS )
        IF ( STATUS .EQ. SAI__OK ) THEN

*      Look for filter id
          CALL CMP_GET0I( SLOC, 'FILTER', IFILT, STATUS )
          IF ( (IFILT.LT.1) .OR. (IFILT.GT.8) .OR.
     :                    (STATUS .NE. SAI__OK) ) THEN
            IF ( STATUS .EQ. SAI__OK ) THEN
              CALL MSG_SETI( 'N', IFILT )
              CALL MSG_PRNT( 'Invalid filter id code ^N' )
            ELSE
              CALL ERR_ANNUL( STATUS )
            END IF
          ELSE
            FILTER_OK = .TRUE.
            CALL WFC_FILT_CONV( IFILT, CALFN, STATUS )
          END IF

*        Get iris value
          CALL DAT_THERE( SLOC, 'IRIS', IRIS_OK, STATUS )
          IF ( IRIS_OK ) THEN
            CALL CMP_GET0R( SLOC, 'IRIS', IRIS, STATUS )
            IF ( STATUS .NE. SAI__OK ) THEN
             CALL ERR_ANNUL( STATUS )
              IRIS_OK = .FALSE.
            ELSE IF ( IRIS .LT. 0.001 ) THEN
              CALL MSG_PRNT( 'WARNING : bad IRIS value,'/
     :                                /' check dataset' )
              IRIS_OK = .FALSE.
            END IF
          END IF

*        Tidy up
          CALL DAT_ANNUL( SLOC, STATUS )

        ELSE
          CALL ERR_ANNUL( STATUS )

        END IF

*      Get filter from user if none supplied
        CALL USI_PROMT( 'AUX', 'Enter filter id (P1,P2,UV,OP'/
     :                             /'Q,S1A/B,S2A/B)', STATUS )
        DO WHILE ( .NOT. FILTER_OK )
 20       CALL USI_GET0C( 'AUX', CID, STATUS )
          CALL USI_CANCL( 'AUX', STATUS )
          IF ( STATUS .NE. SAI__OK ) GOTO 69
          CALFN = CAL_FILT_S2N( CID(:CHR_LEN(CID)) )
          IF ( ( CALFN .GE. 1 ) .AND. ( CALFN .LE. 8) ) THEN
            FILTER_OK = .TRUE.
          ELSE
            CALL MSG_SETC( 'NAM', CID )
            CALL MSG_PRNT( 'Invalid filter name /^NAM/' )
          END IF
        END DO

*    Warn user if no filter
        IF ( .NOT. FILTER_OK ) THEN
          IF ( ISSUR ) THEN
            CALL MSG_SETC( 'PD', 'S1a' )
            CALFN = CAL_FILT_S2N( 'S1a' )
          ELSE
            CALL MSG_SETC( 'PD', 'P1' )
            CALFN = CAL_FILT_S2N( 'P1' )
          END IF
          CALL MSG_PRNT( 'Unable to get filter id from dataset -'/
     :                                     /' defaulting to ^PD' )
        END IF
        CALL PSF0_SETID0I( PSID, 'Filter', CALFN, STATUS )

*      Use filter to get stuff - inform user and store energy
        CALL CAL_FILT_INFO( CALFN, CID, ENER, STATUS )
        CALL PSF0_SETID0R( PSID, 'Energy', ENER, STATUS )
        CALL MSG_SETC( 'ID', CID )
        CALL MSG_PRNT( '   Filter ^ID')

        IF ( .NOT. IRIS_OK ) THEN
          IRIS = 2.5
          CALL MSG_PRNT( 'Unable to get IRIS value from dataset'/
     :                          /' - defaulting to 2.5 degrees' )
        END IF

*    Convert IRIS value to radians and store
        CALL PSF0_SETID0R( PSID, 'Iris', IRIS * MATH__DTOR, STATUS )

 69     CONTINUE

      END IF

      END


*+  PSF_XRT_PSPC - ROSAT XRT PSPC PSF
      SUBROUTINE PSF_XRT_PSPC( PSID, X0, Y0, QX, QY, DX, DY, INTEG, NX,
     :                                              NY, ARRAY, STATUS )
*
*    Description :
*
*     On axis, 3 component :
*
*     The PSPC on-axis PSF is a combination of three, physically well
*     understood terms:
*
*     1. A gaussian for the intrinsic PSPC resolution due to the inherent
*        statistics of the primary electron generation. Theoretically
*        the gaussian Sigma is proportional to 1/SQRT(Energy)
*
*     2. An exponential function due to the finite penetration depth of
*        the X-rays in the counter gas combined with the 8.5 degree cone
*        angle. The PSPC is focussed for 1 keV; the 'chromatic aberration'
*        is largest for large energies
*
*     3. A Lorentzian function for the mirror scattering which breaks into
*        a different power law slope at larger energies. Theoretically the
*        scattering fraction should increase like the square of the
*        energy, if the grazing angle remains constant. Due to the
*        diffraction laws, the shape parameters should be proporional to
*        1/Energy.
*
*     In principle these three components should be folded with each
*     other, however, their angular domains are reasonably well separated
*     that a simple addition is accurate enough. The detailed PSF
*     parameters and their energy dependence have been determined using
*     the PANTER telescope calibration data of both PSPC-A and PSPC-C at
*     the monochromatic energies 0.28, 0.93, 1.49 and 1.70 keV. At lower
*     pulseheights than channel 15 (0.15 keV) additional 'ghost images'
*     appear in the PSPC for which no analytical fit is possible. These
*     events should be avoided as far as possible in PSF modelling.
*
*     On axis, 2 component :
*
*       psf = a1*exp(-0.5*(r/s1)**2) + a2*exp(-r/s2)
*
*    Method :
*
*     Must be normalised to that if the user requested the given spacing
*     over all the PSF, the total would be unity.
*
*    Deficiencies :
*    Bugs :
*    Authors :
*
*     Gunther Hassinger (MPE::GRH)
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     11 Jan 91 : Original (DJA)
*     10 Sep 92 : 3 options added (DJA)
*     15 Oct 92 : Variable profile from cube added (DJA)
*     29 Oct 92 : Handles energy banding (DJA)
*      3 Sep 93 : Handles psf cubes with arbitrary energy binning (DJA)
*      1 Mar 94 : Use MATH_EXPR rather than D.P. intrinsic function (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'PRM_PAR'
      INCLUDE 'PSF_PAR'
      INCLUDE 'MATH_PAR'
      INCLUDE 'PSF_XRT_PSPC_CMN'
*
*    Import :
*
      REAL                      DX, DY, X0, Y0,QX,QY
      INTEGER                   NX,NY,PSID
      LOGICAL                   INTEG
*
*    Export :
*
      REAL                      ARRAY(NX,NY)
*
*    Status :
*
      INTEGER                   STATUS                  ! Run-time error
*
*    Functionss :
*
      REAL			MATH_EXPR
      REAL                      PSF_XRT_PSPC_EXC
      INTEGER                   PSF_XRT_PSPC_EBIN
*
*    Local constants :
*
      REAL                     FW2SIG
        PARAMETER              ( FW2SIG = 2.354820 )
      REAL                     O2_A1, O2_A2            ! Relative contributions
        PARAMETER              ( O2_A1 = 4.670,           ! of 2 components
     :                           O2_A2 = 2.187 )
      REAL                     O2_S1, O2_S2            ! Radial scale of comps
        PARAMETER              ( O2_S1 = 0.2137*60.,      ! in arcsec
     :	                         O2_S2 = 0.33329*60. )
      REAL                     RTOS                    ! Radian to arcsec
        PARAMETER              ( RTOS = MATH__RTOD*3600.0 )
      REAL                     RTOM                    ! Radian to arcmin
        PARAMETER              ( RTOM = MATH__RTOD*60.0 )
      REAL                     O3_NORM
        PARAMETER              ( O3_NORM = 20.034*RTOS )
      REAL                     NORM                    ! Normalisation
        PARAMETER              ( NORM = 2.005*5098.80 )
*
*    Local variables :
*
      CHARACTER*3		OPT			! Profile option

      REAL                     ENERGY                  ! Mean photon energy
      REAL                     FWHM, SIG               ! Gaussian attrs
      REAL                     LNORM                   ! Normalisation constant
      REAL                     O2_S1_2                 ! Onaxis_2 variables
      REAL                     O3_A1, O3_A2, O3_A3     ! Onaxis_3 variables
      REAL                     O3_SIGMA, O3_RC
      REAL                     O3_BREAK1, O3_BREAK2
      REAL                     O3_N1, O3_N2, O3_N3
      REAL                     O3_ALPHA2
      REAL                     OFFAXIS                 ! Off-axis angle (arcmin)
      REAL                     AUX,ARG1,ARG2
      REAL                     P_SCALE                 ! Scale size of psf
      REAL                     ROFF                    ! Off-axis angle
      REAL                     RPS                     ! Radius of sub-pix ^ 2
      REAL                     RSCALE                  ! Cube radial bin size
      REAL                     SDX, SDY                ! Sub-pixel bin sizes
      REAL                     SUM                     ! Cumulative value
      REAL                     XP0, YP0                ! Major pixel centre
      REAL                     XPS, YPS                ! Sub-pixel pixel centre
      REAL                     YPS2                    ! Sub-pixel distance

      INTEGER                  EBIN, OBIN              ! Energy/off-axis bins
      INTEGER                  I, J                    ! Major pixel loops
      INTEGER                  II, JJ                  ! Sub-pixel loops
      INTEGER                  MNX, MNY                ! Local calc bounds
      INTEGER			PHALO			! PHA lower bound
      INTEGER                  XSUB, YSUB              ! Sub-pixel factors

      LOGICAL			PHADEF			! PHA band defined?
      LOGICAL                   SYMMETRIC               ! Symmetric about centre?
*
*    Inline functions :
*
      REAL                     DEL,PIX
      INTEGER                  SPIX
      DOUBLE PRECISION         ONAXIS_2
      SPIX(DEL,PIX) = MAX(1,NINT(abs(10.0*PIX)/P_SCALE/MAX(1.0,
     :                                SQRT(ABS(DEL/P_SCALE)))))
      ONAXIS_2(DEL) = O2_A1*MATH_EXPR(DEL*O2_S1_2)+
     :                O2_A2*MATH_EXPR(-SQRT(DEL/O2_S2))
*-

*  Check inherited global status
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Defined energy band?
      CALL ADI_CGET0L( PSID, 'PhaDef', PHADEF, STATUS )
      IF ( PHADEF ) THEN
        CALL ADI_CGET0I( PSID, 'PhaLo', PHALO, STATUS )
        ENERGY = REAL(PHALO)/100.0
      ELSE
        CALL PSF0_GETID0R( PSID, 'Energy', ENERGY, STATUS )
      END IF

*  Get PSPC option
      CALL PSF0_GETID0C( PSID, 'Form', OPT, STATUS )

*  The variable gaussian option
      IF ( OPT .EQ. 'VGS' ) THEN

*      Get off-axis angle in arcminutes
        ROFF = SQRT( X0**2 + Y0**2 ) * MATH__RTOD * 60.0

*      Get gaussian FWHM in arcseconds
        CALL PSF_XRT_FWHM( ROFF, ENERGY, FWHM )

*      Convolve
        FWHM = SQRT( FWHM**2 )

*      Convert FWHM to sigmas in radians
        SIG = ( (FWHM/3600.0) * MATH__DTOR ) / FW2SIG

*      Evaluate gaussian
        CALL MATH_INTGAU2D( SIG, SIG, 0.0, 0.0, 0.0, QX, QY,
     :                       DX, DY, NX, NY, ARRAY, STATUS )

      ELSE

*      A few variables to speed things up
        IF ( OPT .EQ. 'ON3' ) THEN

*        Relative fractions of 3 components
          O3_A2 = 10**(-1.618+0.507*ENERGY+0.148*ENERGY*ENERGY) ! exponential
          O3_A3 = 0.059*ENERGY**1.43				! scattering
          O3_A1 = 1.0-O3_A2-O3_A3				! gaussian

*        Gaussian sigma
          O3_SIGMA=SQRT(108.7*ENERGY**(-0.888)+1.121*ENERGY**6)

*        Exponential e-folding angle
          O3_RC = SQRT(50.61*ENERGY**(-1.472)+6.80*ENERGY**5.62)

*        Scattering Lorentzian break angles
          O3_BREAK1 = 39.95/ENERGY
          O3_BREAK2 = 861.9/ENERGY

*        Scattering Lorentzian slope
          O3_ALPHA2 = 2.119+0.212*ENERGY

*        Normalization by integrals 0-infinity
          O3_N1 = O3_A1*O3_NORM/(2.0*MATH__PI*O3_SIGMA*O3_SIGMA)
          O3_N2 = O3_A2*O3_NORM/(2.0*MATH__PI*O3_RC*O3_RC)
          AUX = 1.0+O3_BREAK2*O3_BREAK2/O3_BREAK1/O3_BREAK1
          O3_N3 = O3_A3*O3_NORM/(MATH__PI*(LOG(AUX)+
     :                                  2./(AUX*(O3_ALPHA2-2.))))

*        Scale of psf
          P_SCALE = SQRT(O3_SIGMA**2+O3_RC**2)/RTOS

*      2-component on axis
        ELSE IF ( OPT .EQ. 'ON2' ) THEN
          O2_S1_2 = -0.5 / O2_S1**2
          P_SCALE = O2_S1/RTOS

*      Cube handling
        ELSE IF ( OPT .EQ. 'VRP' ) THEN

*        Select energy bin
          EBIN = PSF_XRT_PSPC_EBIN( RX_CB_NEBIN, %VAL(RX_CB_EAPTR),
     :                              (RX_CB_EWPTR.NE.0),
     :                              %VAL(RX_CB_EWPTR), ENERGY )

*        Off-axis angle in arcmin
          OFFAXIS = SQRT(X0*X0+Y0*Y0)*MATH__RTOD*60.0

*        Get offaxis bin
          OBIN = NINT(( OFFAXIS-RX_CB_OBASE ) / RX_CB_OSCALE ) + 1
          OBIN = MAX(1,OBIN)
          OBIN = MIN(OBIN,RX_CB_NOBIN)

*        Use OBIN,EBIN to look profile bin size
          CALL ARR_COP1R( 1, %VAL(RX_CB_RPTR+
     :        ((EBIN-1)*RX_CB_NOBIN+OBIN-1)*VAL__NBR),
     :         RSCALE, STATUS )

*        Set psf scale
          P_SCALE = O2_S1/RTOS

        END IF

*      Base coordinates
        XP0 = ( - REAL(NX)/2.0 ) * DX + X0 + QX
        YP0 = ( - REAL(NY)/2.0 ) * DY + Y0 + QY

*      Symmetric?
        SYMMETRIC = ( ( X0 .EQ. 0.0 ) .AND. ( Y0 .EQ. 0.0 )
     :        .AND. ( QX .EQ. 0.0 ) .AND. ( QY .EQ. 0.0 ) )

*      Bounds for calculation
        IF ( SYMMETRIC ) THEN

*        The "lower left corner" of the array. The +1 guarantees that the
*        centre pixel gets done for odd values of NX/Y
          MNX = (NX+1)/2
          MNY = (NY+1)/2

        ELSE

*        The whole array
          MNX = NX
          MNY = NY

        END IF

*      For each point requiring calculation
        DO J = 1, MNY

*        Find Y sub-pixelling
          YSUB = SPIX( YP0 + DY*REAL(J-1), DY )
          SDY = DY / YSUB

          DO I = 1, MNX

*          Zero
            SUM = 0.0

*          Find X sub-pixelling
            XSUB = SPIX( XP0 + DX*REAL(I-1), DX )
            SDX = DX / XSUB

*          Correct normalisation for sub-pixel and pixel size
            IF ( OPT .EQ. 'VRP' ) THEN
              LNORM = ABS(SDX*SDY*RTOM*RTOM)
            ELSE IF ( OPT .EQ. 'ON3' ) THEN
              LNORM = ABS(SDX*SDY*RTOS*RTOS)/NORM/408.7386
            ELSE
              LNORM = ABS(SDX*SDY*RTOS*RTOS)/NORM
            END IF

*          Y position of first sub-pixel centre
            YPS = YP0 + DY*(J-1) + 0.5*SDY

*          For each sub-pixel row
            DO JJ = 0, YSUB-1

*            Y distance from psf centre
              YPS2 = (YPS-Y0)**2

*            X position of first sub-pixel centre
              XPS = XP0 + DX*(I-1) + 0.5*SDX

*            For each sub-pixel
              IF ( OPT .EQ. 'VRP' ) THEN

                DO II = 0, XSUB-1

*                Radius of sub-pixel in arcmin
                  ROFF = SQRT((XPS-X0)**2 + YPS2)*MATH__RTOD*60.0

*                Value of function
                  SUM = SUM + PSF_XRT_PSPC_EXC( ROFF, RSCALE, OBIN,
     :                  EBIN, RX_CB_NRBIN, RX_CB_NOBIN,
     :                  RX_CB_NEBIN, %VAL(RX_CB_PPTR), STATUS )

*                Next sub-pixel
                  XPS = XPS + SDX

                END DO

              ELSE IF ( OPT .EQ. 'ON3' ) THEN

                DO II = 0, XSUB-1

*                Radius of sub-pixel in arcsec
                  RPS = ((XPS-X0)**2 + YPS2)*RTOS*RTOS
                  ROFF = SQRT(RPS)

*                Calculate function
                  ARG1 = 0.5*RPS/(O3_SIGMA**2)
                  IF ( ARG1 .GE. 75.0 ) ARG1=75.
                  ARG2 = ROFF/O3_RC
                  IF ( ARG2 .GE. 75.0 ) ARG2=75.
                  IF ( ROFF .LE. O3_BREAK2 ) THEN
                    SUM = SUM + O3_N1*MATH_EXPR(-ARG1)+
     :                          O3_N2*MATH_EXPR(-ARG2)+
     :                          O3_N3/(O3_BREAK1*O3_BREAK1+RPS)
                  ELSE
                    SUM = SUM + O3_N1*MATH_EXPR(-ARG1)+
     :                          O3_N2*MATH_EXPR(-ARG2)+
     :                          O3_N3/(O3_BREAK1*O3_BREAK1+
     :                                 O3_BREAK2*O3_BREAK2)*
     :                             (ROFF/O3_BREAK2)**(-O3_ALPHA2)
                  END IF

*                Next sub-pixel
                  XPS = XPS + SDX

                END DO

              ELSE

                DO II = 0, XSUB-1

*                Radius of sub-pixel squared
                  RPS = (XPS-X0)**2 + YPS2

*                Value of function
                  SUM = SUM + ONAXIS_2( RPS*RTOS*RTOS )

*                Next sub-pixel
                  XPS = XPS + SDX

                END DO

              END IF

*            Next row of sub-pixels
              YPS = YPS + SDY

            END DO

*          Set ARRAY value
            ARRAY(I,J) = SUM*LNORM

          END DO

        END DO

*      Copy array around if symmetrical
        IF ( SYMMETRIC ) THEN

*        Transfer data to other 3 quadrants
          JJ = NY
          DO J = 1, MNY
            II = NX
            DO I = 1, MNX
              ARRAY(II,J) = ARRAY(I,J)
              ARRAY(II,JJ) = ARRAY(I,J)
              ARRAY(I,JJ) = ARRAY(I,J)
              II = II - 1
            END DO
            JJ = JJ - 1
          END DO

        END IF

      END IF

      END


*+  PSF_XRT_FWHM  calculates the full width half max of the XRT point spread fn.
      SUBROUTINE PSF_XRT_FWHM(FOVR, ENERGY, FWHM)
*    Description :
*      Calculates the fwhm of the point spread function.
*
*   calculates the preliminary gaussian blur-circle as a function of
*   the off-axis distance which was calibrated with simulations.
*   point-spread function is based on a simple gaussian fit, where in
*   in the outer regions of the field of view the hole in the point
*   spread function has been filtered out by a gradient filter.
*   the fwhm of the detector was assumed to be
*   fwhm(d) = 23.3 / sqrt(e) [arcsec]       (e is energy in kev)
*   !!!!  NEW after PSF simulations  !!!
*   fwhm(d) = sqrt(409.65/e + 69.28*e**2.88 + 66.29)
*   the fwhm of the telescope was determined to
*   fwhm(t) = 0.29 * (eps**1.74) [arcsec]   (eps is off-axis angle ')
*
*    History :
*      author : GRH               date: 1987     original
*      update : RDS               date: 10-Dec-1990   Asterix version
*    Type definitions :
      IMPLICIT NONE
*    Import :
      REAL FOVR                ! Off axis angle in arcmins
      REAL ENERGY              ! Energy in keV
*    Import-Export :
*    Export :
      REAL FWHM                ! Full width half maximum of gaussian
*                              ! point spread in arcsecs
*    Local constants :
*    Local variables :
*-
      REAL DETECT              ! fwhm of the detector
      REAL TELESC              ! fwhm of the telescope
*
      REAL TELES
*    Local data :
      DATA TELES/0.29/
*
*  Calculate the telescope contribution
      TELESC = TELES * TELES * (FOVR)**3.48
*
*  Calculate the detector contribution
      DETECT = 409.65/ENERGY + 69.28*ENERGY**2.88 + 66.29
*
*  Calculate the total FWHM of the gaussian approximation to the point
*  spread function
      FWHM   = SQRT(DETECT+TELESC)
*
      END



*+  PSF_XRT_PSPC_EXC - Extract psf surface brightness from profile in cube
      REAL FUNCTION PSF_XRT_PSPC_EXC( R, RBIN, IO, IE, NR,
     :                              NO, NE, CUBE, STATUS )
*
*    Description :
*
*     Returns surface brightness of the psf in units of integrated probability
*     per square arcminute for given distance R from the psf centre.
*
*    Deficiencies :
*     <description of any deficiencies>
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*
*     David J. Allan (BHVAD)
*
*    History :
*
*     14 Oct 92 : Original (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Import :
*
      REAL                     R                       ! Radius from psf centre
                                                       ! in arcmin
      REAL                     RBIN                    ! Bin width in arcmin
      INTEGER                  IO, IE                  ! Off-axis and energy
                                                       ! cube profile numbers
      INTEGER                  NR, NO, NE              ! Cube dimensions
      REAL                     CUBE(NR,NO,NE)          ! The cube
*
*    Status :
*
      INTEGER                  STATUS                  ! Run-time error
*
*    Local variables :
*
      REAL                     INR(5)                  ! Interp radii
      REAL                     INP(5)                  ! Interp psf values
      REAL                     RVAL                    ! Function return value

      INTEGER                  I, IR                   ! Loop variables
      INTEGER                  NIN                     ! # interpolates
*-

*    Find central radial bin
      IR = INT(R/RBIN) + 1

*    Too big?
      IF ( IR .GT. NR ) THEN
        RVAL = 0.0
        GOTO 99
      END IF

*    Set up for interpolation
      NIN = 0
      DO I = MAX(IR-2,1), MIN(NR,IR+2)
        NIN = NIN + 1
        INR(NIN) = REAL(I-1)*RBIN
        INP(NIN) = CUBE(I,IO,IE)
      END DO

*    Interpolate
      CALL MATH_INTERP( NIN, INR, INP, 1, R, 2, RVAL, STATUS )

*    Check positive
      IF ( RVAL .LT. 0.0 ) THEN
        CALL MATH_INTERP( NIN, INR, INP, 1, R, 1, RVAL, STATUS )
        RVAL = MAX(0.0,RVAL)
      END IF

*    Set return value
 99   PSF_XRT_PSPC_EXC = RVAL

      END



*+  PSF_XRT_PSPC_EBIN - Return cube bin number of a given energy
      INTEGER FUNCTION PSF_XRT_PSPC_EBIN( NEB, EBIN, WOK, EWID, ENERGY )
*    Description :
*     <description of what the function does - for user info>
*    Deficiencies :
*     <description of any deficiencies>
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*      3 Sep 93 : Original (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Import :
*
      INTEGER                  NEB                     ! Number of energy bins
      REAL                     EBIN(*)                 ! Energy bin centres
      LOGICAL                  WOK                     ! Bin widths present?
      REAL                     EWID(*)                 ! Energy bin widths
      REAL                     ENERGY                  ! Mean photon energy
*
*    Local variables :
*
      INTEGER                  I                       ! Loop over energy bins
*-

      IF ( ENERGY .LT. EBIN(1) ) THEN
        PSF_XRT_PSPC_EBIN = 1

      ELSE IF ( ENERGY .GT. EBIN(NEB) ) THEN
        PSF_XRT_PSPC_EBIN = NEB

      ELSE IF ( WOK ) THEN
        DO I = 1, NEB
          IF ( I .LT. NEB ) THEN
            IF ( ENERGY .LT. (EBIN(I) + EWID(I)/2.0) ) THEN
              PSF_XRT_PSPC_EBIN = I
              RETURN
            END IF
          ELSE
            PSF_XRT_PSPC_EBIN = I
          END IF
        END DO

      ELSE
        DO I = 1, NEB
          IF ( I .LT. NEB ) THEN
            IF ( ENERGY .LT. (EBIN(I) + (EBIN(I+1)-EBIN(I))/2.0) ) THEN
              PSF_XRT_PSPC_EBIN = I
              RETURN
            END IF
          ELSE
            PSF_XRT_PSPC_EBIN = I
          END IF
        END DO

      END IF

      END



*+  PSF_XRT_PSPC_HINT - XRT PSPC psf hint handler
      SUBROUTINE PSF_XRT_PSPC_HINT( PSID, HINT, DATA, STATUS )
*
*    Description :
*
*     Return hints about the XRT PSPC psf.
*
*    Method :
*    Deficiencies :
*    Authors :
*
*     David J. Allan (ROSAT, University of Birmingham)
*
*    History :
*
*     23 Dec 93 : Original (DJA)
*      3 Mar 94 : POSDEP hint added (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'PSF_PAR'
      INCLUDE 'MATH_PAR'
*
*    Import :
*
      INTEGER                 	PSID
      CHARACTER*(*)           	HINT		 	! Hint name
*
*    Export :
*
      BYTE			DATA(*)			! Hint data
*
*    Status :
*
      INTEGER                   STATUS
*
*   Local variables:
*
      CHARACTER*3		OPT			! Psf form
*-

*  Check inherited global status
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Radial symmetry?
      IF ( HINT .EQ. PSF_H_RADSYM ) THEN

*    All our models are radially symmetric about on-axis direction
        CALL ARR_COP1L( 1, .TRUE., DATA, STATUS )

*  Position dependent?
      ELSE IF ( HINT .EQ. PSF_H_POSDEP ) THEN

*    Get the psf form
        CALL PSF0_GETID0C( PSID, 'Form', OPT, STATUS )

*    The ONAXIS3 psf isn't position dependent
        CALL ARR_COP1L( 1, (OPT.NE.'ON3'), DATA, STATUS )

*  Energy dependent?
      ELSE IF ( HINT .EQ. PSF_H_ENDEP ) THEN

*    They all vary with energy
        CALL ARR_COP1L( 1, .TRUE., DATA, STATUS )

*  Field size?
      ELSE IF ( HINT .EQ. PSF_H_FLDSIZ ) THEN

*    Write value
        CALL ARR_COP1R( 1, 1.06*MATH__DTOR, DATA, STATUS )

      ELSE
        STATUS = SAI__ERROR
        CALL MSG_SETC( 'H', HINT )
        CALL ERR_REP( ' ', 'Unknown psf hint /^H/', STATUS )

      END IF

      END

*+  PSF_XRT_PSPC_INIT - XRT PSPC psf initialisation
      SUBROUTINE PSF_XRT_PSPC_INIT( PSID, STATUS )
*
*    Description :
*
*     Associate a slot with an XRT PSPC psf.
*
*    Environment parameters :
*
*     MASK = CHAR(R)
*     AUX = ...
*
*    Method :
*    Deficiencies :
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     17 Jan 91 : Original (DJA)
*     28 Oct 92 : Don't ask for mean photon energy if already defined (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PSF_PAR'
      INCLUDE 'PSF_XRT_PSPC_CMN'
*
*    Import :
*
      INTEGER			PSID
*
*    Status :
*
      INTEGER STATUS
*
*    Function declarations :
*
      LOGICAL                  STR_ABBREV
      LOGICAL                  CHR_SIMLR
      EXTERNAL			PSF_XRT_PSPC
      EXTERNAL			PSF_XRT_PSPC_HINT
*
*    Local variables :
*
      CHARACTER*(DAT__SZLOC)   CBLOC			! Cube locator
      CHARACTER*3	       OPT			! Profile option

      REAL		       ENERGY			! Psf energy
      REAL		       SPARR(2)

      CHARACTER*132            FNAME             	! File name of cube
      CHARACTER*20             MASK              	! Mask name

      INTEGER                  IDUM,CDIMS(3)          	! XRT pf cube dimensions

      LOGICAL                  OK		      	!
      LOGICAL                  PHADEF		      	! PHA band defined?
*
*  Local Data:
*
      LOGICAL			FIRST
	SAVE			FIRST
      DATA			FIRST/.TRUE./
*-

*  Check inherited global status
      IF ( STATUS .NE. SAI__OK ) RETURN

*  First time?
      IF ( FIRST ) THEN
        RX_CB_OPEN = .FALSE.
        FIRST = .FALSE.
      END IF

*  Get mask name
      CALL USI_PROMT( 'MASK',
     :                'PSPC psf option (LIST for descriptions)',
     :                STATUS )
      CALL USI_DEF0C( 'MASK', 'VARPROFILE', STATUS )
 10   CALL USI_GET0C( 'MASK', MASK, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Validate choice
      IF ( CHR_SIMLR(MASK,'LIST') ) THEN
        CALL MSG_PRNT( ' ' )
        CALL MSG_PRNT( '  VARGAUSS      - Gaussian, fn(r,E)' )
        CALL MSG_PRNT( '  VARPROFILE    - Taken from RORD '/
     :                                   /'tables, fn(r,E)' )
        CALL MSG_PRNT( '  ONAXIS_3      - 3 component on-axis'/
     :                                         /' fit, fn(E)' )
        CALL MSG_PRNT( '  ONAXIS_2      - 2 component on-axis fit,'/
     :                                               /' not fn(E)' )
        CALL MSG_PRNT( ' ' )
        CALL USI_CANCL( 'MASK', STATUS )
        GOTO 10

      ELSE IF ( STR_ABBREV(MASK,'VARGAUSS') ) THEN

        OPT = 'VGS'

      ELSE IF ( STR_ABBREV(MASK,'VARPROFILE') ) THEN

*      Set option
        OPT = 'VRP'

*      Open the cube
        IF ( .NOT. RX_CB_OPEN ) THEN

*        Translate environment symbol
          CALL PSX_GETENV( 'AST_XRT_PSF_CUBE', FNAME, STATUS )

*        Open the file
          CALL ADI_FOPEN( FNAME, 'BinDS', 'READ', RX_CB_ID, STATUS )
          CALL ADI1_GETLOC( RX_CB_ID, CBLOC, STATUS )

*        Abort if failed
          IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERR_REP( ' ', 'Unable to open XRT psf cube', STATUS )
            GOTO 99
          END IF

*        Map the 2 components
          CALL CMP_MAPN( CBLOC, 'DATA_ARRAY', '_REAL', 'READ',
     :                   3, RX_CB_PPTR, CDIMS, STATUS )
          RX_CB_NRBIN = CDIMS(1)
          RX_CB_NOBIN = CDIMS(2)
          RX_CB_NEBIN = CDIMS(3)
          CALL CMP_MAPN( CBLOC, 'RBINSIZE', '_REAL', 'READ',
     :                   2, RX_CB_RPTR, CDIMS, STATUS )

*      Get off-axis angle axis attributes
          CALL BDI_AXGET1R( RX_CB_ID, 2, 'SpacedData', 2, SPARR,
     :                      IDUM, STATUS )
          RX_CB_OBASE = SPARR(1)
          RX_CB_OSCALE = SPARR(2)

*      Map energy axis data and widths if present
          CALL BDI_AXMAPR( RX_CB_ID, 3, 'Data', 'READ', RX_CB_EAPTR,
     :                     STATUS )
          CALL BDI_AXCHK( RX_CB_ID, 3, 'Width', OK, STATUS )
          IF ( OK ) THEN
            CALL BDI_AXMAPR( RX_CB_ID, 3, 'Width', 'READ', RX_CB_EWPTR,
     :                     STATUS )
          ELSE
            RX_CB_EWPTR = 0
          END IF

*        Report success
          IF ( STATUS .EQ. SAI__OK ) THEN
            RX_CB_OPEN = .TRUE.
            CALL MSG_SETI( 'NE', RX_CB_NEBIN )
            CALL MSG_PRNT( 'Loaded XRT psf cube with ^NE'/
     :                               /' energy channels' )
          ELSE
            CALL ERR_REP( ' ', 'Unable map components in XRT psf cube',
     :                                                         STATUS )
            GOTO 99
          END IF

        END IF

      ELSE IF ( STR_ABBREV(MASK,'ONAXIS_3') ) THEN

        OPT = 'ON3'

      ELSE IF ( STR_ABBREV(MASK,'ONAXIS_2') ) THEN

        OPT = 'ON2'

      ELSE
        STATUS = SAI__ERROR
        CALL MSG_SETC( 'MASK', MASK )
        CALL ERR_REP( ' ', 'Invalid PSPC psf option /^MASK/', STATUS )

      END IF

*  Get a mean photon energy unless PHA band is defined
      CALL ADI_CGET0L( PSID, 'PhaDef', PHADEF, STATUS )
      IF ( (OPT .NE. 'ON2') .AND. .NOT. PHADEF ) THEN
        CALL USI_PROMT( 'AUX', 'Mean photon energy in KeV', STATUS )
        CALL USI_GET0R( 'AUX', ENERGY, STATUS )
        CALL PSF0_SETID0R( PSID, 'Energy', ENERGY, STATUS )
      END IF

*  Store option
      CALL PSF0_SETID0C( PSID, 'Form', OPT, STATUS )

*  Store methods
      CALL PSF0_SETRTN( PSID, 'Data', PSF_XRT_PSPC, STATUS )
      CALL PSF0_SETRTN( PSID, 'Hint', PSF_XRT_PSPC_HINT, STATUS )

*  Tidy up
 99   IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'PSF_XRT_PSPC_INIT', STATUS )
      END IF

      END
