*+  PSF_ANAL - User defined PSF handler
      SUBROUTINE PSF_ANAL( SLOT, X0, Y0, QX, QY, DX, DY, INTEG, NX,
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
*     10 Jul 89 : Original (DJA)
*     19 Jul 90 : Copes with inequal spatial scaling. Some improvements to
*                 normalisation (DJA)
*      9 Mar 91 : Gaussian handled by MATH_INTGAU2D (DJA)
*      2 Aug 93 : Added King profile option (DJA)
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
      INCLUDE 'MATH_PAR'
      INCLUDE 'PSF_ANAL_CMN'
*
*    Import :
*
      REAL                     DX, DY, X0, Y0,QX,QY
      INTEGER                  NX,NY,SLOT
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
      REAL                     SIGX, SIGY              ! GAUSSIAN variables
      REAL                     HHYPOT, PROBP, PROBSP   ! TOPHAT variables
      REAL                     H, HR                   ! TRAINGLE variables
      REAL                     FWHM, FWMV, FWZR        ! FLAT_TRI variables

      REAL                     DELX, DELY              ! Offsets from X0,Y0
      REAL                     LHS, BOT                ! Left side and bottom
                                                       ! of ARRAY in radians
      REAL                     R                       ! Distance pixel to X0,Y0
      REAL                     RC, ALPHA               ! King profile params
      REAL                     THETA                   ! PA of pixel wrt X0,Y0
      REAL                     W                       ! Mask width
      REAL                     XP, YP                  ! Pixel centres
      REAL                     XPP, YPP                ! Sub-pixel centres

      INTEGER                  I, J                    ! Loops over output data
      INTEGER                  II, JJ                  ! Subsample loops
      INTEGER                  KIND                    ! Variety of user PSF
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Decide on form of PSF
      KIND = AN_KIND(SLOT)

*    Find LHS and BOT in radians
      LHS = X0 + QX - DX * REAL(NX) / 2.0
      BOT = Y0 + QY - DY * REAL(NY) / 2.0

*    Gaussian
      IF ( KIND .EQ. ANAL_GAUSS ) THEN

*      Get the full-widths in each dimension, and convert to sigmas
        SIGX = AN_PW(SLOT,1) /
     :                       ( 2.0 * SQRT(2.0*ALOG(2.0)) )
        SIGY = AN_PW(SLOT,2) /
     :                       ( 2.0 * SQRT(2.0*ALOG(2.0)) )

*      Fill in values
        CALL MATH_INTGAU2D( SIGX, SIGY, 0.0,0.0,0.0, QX, QY,
     :                       DX, DY, NX, NY, ARRAY, STATUS )

*    King profile
      ELSE IF ( KIND .EQ. ANAL_KING ) THEN

*      Get the full-widths in each dimension, and convert to sigmas
        RC = AN_PW(SLOT,1)
        ALPHA = AN_PW(SLOT,2)

*      Fill in values
        CALL MATH_INTKING2D( RC, ALPHA, 0.0, 0.0, QX, QY,
     :                       DX, DY, NX, NY, ARRAY, STATUS )


*    Lorentz profile
      ELSE IF ( KIND .EQ. ANAL_LORENTZ ) THEN

*      Fill in values
        CALL MATH_INTLOR2D( AN_PW(SLOT,1), AN_PW(SLOT,2), 0.0, 0.0,
     :                      0.0, QX, QY,
     :                      DX, DY, NX, NY, ARRAY, STATUS )

*    Top hat
      ELSE IF ( KIND .EQ. ANAL_TOPHAT ) THEN

*      Get radius of tophat
        W = AN_PW(SLOT,1) / 2.0

*      Probability per pixel completely inside tophat
        PROBP = ABS(DX*DY) / (MATH__PI*W*W)

*      Process each pixel grid
        DO J = 1, NY

*        Y position of pixel centres in this row
          YP = BOT + (REAL(J)-0.5)*DY

          DO I = 1, NX

*          X position of pixel centre
            XP = LHS + (REAL(I)-0.5)*DX

*          Find distance from psf centre
            R = SQRT( (XP-X0)**2 + (YP-Y0)**2 )

*          Find position angle of pixel
            IF ( (XP-X0) .EQ. 0.0 ) THEN
              THETA = 0.0
            ELSE
              THETA = ATAN2(YP-Y0,XP-X0)
            END IF

*          HHYPOT is the radius from the psf inside which any ARRAY pixel
*          centre is guaranteed to mean the whole pixel being inside the
*          tophat width. This clearly depends on the P.A. of the pixel
*          centre w.r.t. the psf centre. The relation below gives a value
*          of DX/2 on the principal axes rising to a maximum of (DX*root2)/2
*          at 45 degrees to the principal axes.
            HHYPOT = (1.0 + SIN(2.0*THETA)*(ROOT2-1.0))*ABS(DX)/2.0

*          Pixel well inside tophat radius
            IF ( R .LT. (W-HHYPOT) ) THEN

              ARRAY(I,J) = PROBP

*          Well outside?
            ELSE IF ( R .GT. (W+HHYPOT) ) THEN

              ARRAY(I,J) = 0.0

*          Sub-sample
            ELSE

*            Probability per sub-pixel
              PROBSP = PROBP / REAL(OVERSAMPLE**2)

*            Sub-sample the ARRAY pixel accumulating probability from those
*            subpixels inside the tophat.
              ARRAY(I,J) = 0.0
              DO JJ = 1, OVERSAMPLE
                YPP = YP - 0.5*DY + (REAL(JJ-1)+0.5)*DY/OVERSAMPLE
                DO II = 1, OVERSAMPLE
                  XPP = XP - 0.5*DX + (REAL(II-1)+0.5)*DX/OVERSAMPLE

*                Radius of subpixel
                  R = SQRT( (XPP-X0)**2 + (YPP-Y0)**2 )

*                Inside
                  IF ( R .LT. (W-HHYPOT/OVERSAMPLE) ) THEN

                    ARRAY(I,J) = ARRAY(I,J) + PROBSP

*                Some part of pixel inside top-hat radius?
                  ELSE IF ( R .LT. (W+HHYPOT/OVERSAMPLE) ) THEN

*                  The permissible range of (R-W) is from -1.0 to 1.0 in units
*                  of HHYPOT/OVERSAMPLE, corresponding to the radial extremity
*                  of a subpixel being just inside W to just outside W. Map
*                  the above range onto 1.0 to 0.0
                    ARRAY(I,J) = ARRAY(I,J) + PROBSP *
     :                    (0.5 - (R-W)/(HHYPOT/OVERSAMPLE)/2.0 )

                  END IF

                END DO
              END DO

            END IF

          END DO
        END DO

      ELSE IF ( KIND .EQ. ANAL_TRIANGLE ) THEN

*      Get width of triangle
        W = AN_PW(SLOT,1)

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

      ELSE IF ( KIND .EQ. ANAL_FLAT_TRI ) THEN

*      Get various widths
        FWZR = AN_PW(SLOT,1)
        FWHM = AN_PW(SLOT,2)
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
      SUBROUTINE PSF_ANAL_HINT( SLOT, HINT, DATA, STATUS )
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
      INCLUDE 'DAT_PAR'
      INCLUDE 'PSF_PAR'
*
*    Import :
*
      INTEGER                 	SLOT            	! PSF handle
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
        CALL ERR_REP( ' ', 'Unknown psf hint /'//HINT//'/', STATUS )

      END IF

      END

*+  PSF_ANAL_INIT - Analytic defined PSF initialisation
      SUBROUTINE PSF_ANAL_INIT( SLOT, LOC, STATUS )
*
*    Description :
*
*     Gets a user defined PSF and associates it with a PSF slot.
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
      INCLUDE 'DAT_PAR'
      INCLUDE 'PSF_PAR'
      INCLUDE 'PSF_ANAL_CMN'
*
*    Import :
*
      INTEGER                 SLOT(2)            ! PSF handle
      CHARACTER*(DAT__SZLOC)  LOC                ! Dataset locator
*
*    Status :
*
      INTEGER STATUS
*
*    Functions :
*
      INTEGER                 CHR_LEN
      REAL                    PSF1_GETAXDR, PSF1_GETAXTOR
      LOGICAL                 STR_ABBREV, PSF1_GETAXOK
*
*    Local variables :
*
      CHARACTER*30            CHOICE            ! User's choice for PSF
      CHARACTER*30            FTPAR             ! FLAT_TRI parameters
      CHARACTER*80            PROMPT            !
      CHARACTER*40            LABEL, UNITS

      REAL                    GWIDTH            ! Width of a mask
      REAL                    X_DR, Y_DR        ! Axis scales
      REAL                    X_TOR, Y_TOR      ! Axis unit conversions

      INTEGER                 BEG, IC           ! Character pointers
      INTEGER                 X_AX,Y_AX,E_AX,T_AX

      LOGICAL                 X_OK, Y_OK        ! Axes ok?
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Initialise
      X_OK = .FALSE.
      Y_OK = .FALSE.

*    Tell user about user-definitions
      CALL MSG_PRNT( 'PSF definitions available')
      CALL MSG_PRNT( ' ')
      CALL MSG_PRNT( 'GAUSSIAN     - Gaussian response        '/
     :              /'  TRIANGLE     - Triangular response')
      CALL MSG_PRNT( 'TOPHAT       - Circular tophat function '/
     :              /'  FLAT_TRI     - Triangle with flat top')
      CALL MSG_PRNT( 'KING         - King profile ')
      CALL MSG_PRNT( ' ')

*    Define the default mask
      CALL USI_DEF0C( 'MASK', 'GAUSSIAN', STATUS )

*    Get user's choice
      CALL USI_GET0C( 'MASK', CHOICE, STATUS )
      CALL USI_CANCL( 'MASK', STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Identify spatial axes
      CALL PSF1_GETAXID( SLOT(2), X_AX, Y_AX, E_AX, T_AX, STATUS )

*    Tell the user about the pixel size if we have a dataset.
      X_OK = PSF1_GETAXOK( SLOT(2), X_AX, STATUS )
      Y_OK = PSF1_GETAXOK( SLOT(2), Y_AX, STATUS )
      X_DR = PSF1_GETAXDR( SLOT(2), X_AX, STATUS )
      Y_DR = PSF1_GETAXDR( SLOT(2), Y_AX, STATUS )
      X_TOR = PSF1_GETAXTOR( SLOT(2), X_AX, STATUS )
      Y_TOR = PSF1_GETAXTOR( SLOT(2), Y_AX, STATUS )
      CALL PSF1_GETAXTXT( SLOT(2), X_AX, LABEL, UNITS, STATUS )

*    Are dataset axes ok?
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

        AN_KIND(SLOT(1)) = ANAL_GAUSS

*      We need a width for the gaussian
        PROMPT = 'Gaussian FWHM in '//UNITS(:CHR_LEN(UNITS))
        CALL USI_PROMT( 'AUX', PROMPT(:CHR_LEN(PROMPT)), STATUS )
        CALL USI_DEF0R( 'AUX', ABS(X_DR/X_TOR), STATUS )
        CALL USI_GET0R( 'AUX', GWIDTH, STATUS )
        CALL USI_CANCL( 'AUX', STATUS )
        IF ( STATUS .NE. SAI__OK ) GOTO 99

*      Convert it to radians
        AN_PW(SLOT(1),1) = GWIDTH*X_TOR
        AN_PW(SLOT(1),2) = GWIDTH*Y_TOR

      ELSE IF ( STR_ABBREV(CHOICE,'KING') ) THEN

        AN_KIND(SLOT(1)) = ANAL_KING

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
          CALL CHR_CTOR( FTPAR(BEG:IC), AN_PW(SLOT(1),1), STATUS )
          IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERR_FLUSH( STATUS )
            CALL MSG_PRNT( 'Error parsing core radius' )
            GOTO 39
          END IF

          IC = IC + 1
          CALL CHR_FIWS( FTPAR, IC, STATUS )
          BEG = IC
          CALL CHR_FIWE( FTPAR, IC, STATUS )
          CALL CHR_CTOR( FTPAR(BEG:IC), AN_PW(SLOT(1),2), STATUS )
          IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERR_FLUSH( STATUS )
            CALL MSG_PRNT( 'Error parsing profile index' )
            GOTO 39
          END IF

        END IF

*      Convert core radius to radians
        AN_PW(SLOT(1),1) = AN_PW(SLOT(1),1) * X_TOR

      ELSE IF ( STR_ABBREV(CHOICE,'LORENTZ') ) THEN

        AN_KIND(SLOT(1)) = ANAL_LORENTZ

*      We need a width for the gaussian
        PROMPT = 'Lorentzian HWHM in '//UNITS(:CHR_LEN(UNITS))
        CALL USI_PROMT( 'AUX', PROMPT(:CHR_LEN(PROMPT)), STATUS )
        CALL USI_DEF0R( 'AUX', ABS(X_DR/X_TOR), STATUS )
        CALL USI_GET0R( 'AUX', GWIDTH, STATUS )
        CALL USI_CANCL( 'AUX', STATUS )
        IF ( STATUS .NE. SAI__OK ) GOTO 99

*      Convert it to radians
        AN_PW(SLOT(1),1) = GWIDTH*X_TOR
        AN_PW(SLOT(1),2) = GWIDTH*Y_TOR

      ELSE IF ( STR_ABBREV(CHOICE,'TOPHAT') ) THEN

        AN_KIND(SLOT(1)) = ANAL_TOPHAT

*      Get width of tophat
        PROMPT = 'Tophat full width in '//UNITS(:CHR_LEN(UNITS))
        CALL USI_PROMT('AUX', PROMPT(:CHR_LEN(PROMPT)), STATUS )
        CALL USI_GET0R( 'AUX', GWIDTH, STATUS )
        CALL USI_CANCL( 'AUX', STATUS )

*      Convert it to radians
        AN_PW(SLOT(1),1) = GWIDTH*X_TOR

      ELSE IF ( STR_ABBREV(CHOICE,'TRIANGLE') ) THEN

        AN_KIND(SLOT(1)) = ANAL_TRIANGLE

*      Get width of tophat
        PROMPT = 'Triangle zero-point full width in '//UNITS
     :                                        (:CHR_LEN(UNITS))
        CALL USI_PROMT('AUX', PROMPT(:CHR_LEN(PROMPT)), STATUS )
        CALL USI_GET0R( 'AUX', GWIDTH, STATUS )
        CALL USI_CANCL( 'AUX', STATUS )

*      Convert it to radians
        AN_PW(SLOT(1),1) = GWIDTH*X_TOR
        AN_PW(SLOT(1),2) = GWIDTH*Y_TOR

      ELSE IF ( STR_ABBREV(CHOICE,'FLAT_TRI') ) THEN

        AN_KIND(SLOT(1)) = ANAL_FLAT_TRI

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
          CALL CHR_CTOR( FTPAR(BEG:IC), AN_PW(SLOT(1),1), STATUS )
          IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERR_FLUSH( STATUS )
            CALL MSG_PRNT( 'Error parsing FWZR' )
            GOTO 49
          END IF

          CALL CHR_FIWS( FTPAR, IC, STATUS )
          BEG = IC
          CALL CHR_FIWE( FTPAR, IC, STATUS )
          CALL CHR_CTOR( FTPAR(BEG:IC), AN_PW(SLOT(1),2), STATUS )
          IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERR_FLUSH( STATUS )
            CALL MSG_PRNT( 'Error parsing FWHM' )
            GOTO 49
          END IF

        END IF

        IF ( AN_PW(SLOT(1),1) .LT. AN_PW(SLOT(1),2) ) THEN
          CALL MSG_PRNT( 'The zero point full width can''t be LESS'/
     :                                            /' than the FWHM!' )
          GOTO 49
        END IF

*      Convert them both to radians
        AN_PW(SLOT(1),1) = AN_PW(SLOT(1),1)*X_TOR
        AN_PW(SLOT(1),2) = AN_PW(SLOT(1),2)*Y_TOR

      ELSE
        CALL ERR_REP( ' ', 'Unrecognised PSF specifier', STATUS )
        STATUS = SAI__ERROR
      END IF

*    Tidy up
 99   IF ( STATUS .NE. SAI__OK ) THEN
        CALL ERR_REP( ' ', '...from PSF_ANAL_INIT', STATUS )
      END IF

      END


*+  PSF_ANAL_PFL - Analytic psf energy profiling
      SUBROUTINE PSF_ANAL_PFL( SLOT, NFRAC, FRAC, RADII, STATUS )
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
      INCLUDE 'DAT_PAR'
      INCLUDE 'PSF_PAR'
      INCLUDE 'PSF_ANAL_CMN'
*
*    Import :
*
      INTEGER                  SLOT,NFRAC
      REAL                     FRAC(*)
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
      REAL                     FP, FWHM, FWMV, FWZR    ! FLAT_TRI variables
      REAL                     FBP
      REAL                     R,OLDR                  !
      REAL                     W                       ! A mask width

      INTEGER                  I                       ! Loop over radii
      INTEGER                  KIND                    ! Variety of user PSF
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Get analytic form
      KIND = AN_KIND(SLOT)

*    Gaussian?
      IF ( KIND .EQ. ANAL_GAUSS ) THEN

        STATUS = SAI__ERROR

*    Tophat?
      ELSE IF ( KIND .EQ. ANAL_TOPHAT ) THEN

*      Get half-width of mask in radians
        W = AN_PW(SLOT,1) / 2.0

*      For each energy fraction
        DO I = 1, NFRAC
          RADII(I) = W * SQRT(FRAC(I))
        END DO

*    Pyramid
      ELSE IF ( KIND .EQ. ANAL_TRIANGLE ) THEN

*      Get width of mask in radians
        W = AN_PW(SLOT,1)

*      For each energy fraction
        DO I = 1, NFRAC

*        First guess at radius, scaled from 0.0 to 1.0
          R = SQRT(FRAC(I))

*        Iterate until convergence
          OLDR = -999.0
          DO WHILE ( ABS(R-OLDR)/R .GT. 0.0001 )
            OLDR = R
            R = R - (3.0*R**2-2.0*R**3-FRAC(I)) / (6.0*R-6.0*R**2)
          END DO

*        Convert to real distance
          RADII(I) = R*W/2.0

        END DO

*    Flat pyramid
      ELSE IF ( KIND .EQ. ANAL_FLAT_TRI ) THEN

*      Get various widths
        FWZR = AN_PW(SLOT,1)
        FWHM = AN_PW(SLOT,2)
        FWMV = 2.0*FWHM - FWZR

*      Fraction of energy inside plateau
        FP = 3.0*(FWZR-FWMV)*FWMV**2/(FWZR**3-FWMV**3)

*      For each energy fraction
        DO I = 1, NFRAC

*        Requested fraction inside plateau
          IF ( FRAC(I) .LE. FP ) THEN

*          Just a tophat normalised to FP
            RADII(I) = (FWMV/2.0) * SQRT(FRAC(I)/FP)
          ELSE

*          An energy fraction in excess of FP can be translated into a
*          real excess volume in an untruncated pyramid of height H.

*          Find the fraction of energy enclosed in the untruncated pyramid
*          at the radius corresponding to the edge of the plateau, FWMV.
            R = (FWMV/FWZR)
            FBP = 3.0*R**2-2.0*R**3

*          Scale our (FRAC(I) between FP and 1) into the range (FBP to 1.0)
            FBP = FBP + (1.0-FBP)*(FRAC(I)-FP)/(1.0-FP)

*          Now use iteration scheme to get normalised radius
c            R = R + SQRT((FRAC(I)-FP)/(1.0-FP))
            OLDR = -999.0
            DO WHILE ( ABS(R-OLDR)/R .GT. 0.0001 )
              OLDR = R
              R = R - (3.0*R**2-2.0*R**3-FBP) / (6.0*R-6.0*R**2)
            END DO

*          And convert to real units
            RADII(I) = R * FWZR/2.0

          END IF

        END DO

*    Report error
      ELSE
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'Unknown analytic psf code', STATUS )

      END IF

      END

*+  PSF_ASCA - ASCA GIS and SIS psfs
      SUBROUTINE PSF_ASCA( SLOT, X0, Y0, QX, QY, DX, DY, INTEG, NX,
     :                                              NY, ARRAY, STATUS )
*
*    Description :
*
*     The ASCA SIS psf is taken from a manual tracing of Figure 5.3c in the
*     European AO. The GIS psf is that same psf convolved with a Gaussian
*     of FWHM = 0.5 * sqrt(5.9/E) arcmin.
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
*     12 Jan 94 : Original (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PRM_PAR'
      INCLUDE 'PSF_PAR'
      INCLUDE 'MATH_PAR'
      INCLUDE 'PSF_ASCA_CMN'
*
*    Import :
*
      REAL                     DX, DY, X0, Y0,QX,QY
      INTEGER                  NX,NY,SLOT
      LOGICAL                  INTEG
*
*    Export :
*
      REAL                     ARRAY(NX,NY)
*
*    Status :
*
      INTEGER                  STATUS                  ! Run-time error
*
*    Functionss :
*
      REAL                     PSF_ASCA_GIS
      REAL                     PSF_ASCA_SIS
*
*    Local constants :
*
      REAL                     FW2SIG
        PARAMETER              ( FW2SIG = 2.354820 )
      REAL                     RTOM                    ! Radian to arcmin
        PARAMETER              ( RTOM = MATH__RTOD*60.0 )
*
*    Local variables :
*
      REAL			BIT1, BIT2
      REAL                     ENERGY                  ! Mean photon energy
      REAL                     FWHM, SIG               ! Gaussian attrs
      REAL                     LNORM                   ! Normalisation constant
      REAL                     OFFAXIS                 ! Off-axis angle (arcmin)
      REAL                     P_SCALE                 ! Scale size of psf
      REAL                     ROFF                    ! Off-axis angle
      REAL                     RPS                     ! Radius of sub-pix ^ 2
      REAL                     RSCALE                  ! Cube radial bin size
      REAL                     SDX, SDY                ! Sub-pixel bin sizes
      REAL                     SUM                     ! Cumulative value
      REAL                     XP0, YP0                ! Major pixel centre
      REAL                     XPS, YPS                ! Sub-pixel pixel centre
      REAL                     YPS2                    ! Sub-pixel distance

      INTEGER			EGIS(2)			!
      INTEGER                  I, J                    ! Major pixel loops
      INTEGER                  II, JJ                  ! Sub-pixel loops
      INTEGER                  MNX, MNY                ! Local calc bounds
      INTEGER			NGEVAL
      INTEGER                  XSUB, YSUB              ! Sub-pixel factors

      LOGICAL			SIS			! SIS detector?
      LOGICAL                  	SYMMETRIC               ! Symmetric about X0,Y0?
*
*    Inline functions :
*
      REAL                     DEL,PIX
      INTEGER                  SPIX
      SPIX(DEL,PIX) = MAX(1,NINT(abs(10.0*PIX)/P_SCALE/MAX(1.0,
     :                                SQRT(ABS(DEL/P_SCALE)))))
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Base coordinates
      XP0 = ( - REAL(NX)/2.0 ) * DX + X0 + QX
      YP0 = ( - REAL(NY)/2.0 ) * DY + Y0 + QY

*    Symmetric?
      SYMMETRIC = ( ( X0 .EQ. 0.0 ) .AND. ( Y0 .EQ. 0.0 )
     :        .AND. ( QX .EQ. 0.0 ) .AND. ( QY .EQ. 0.0 ) )

*    SIS detector?
      IF ( AS_INSTR(SLOT) .EQ. 'SIS' ) THEN

        SIS = .TRUE.

      ELSE

        SIS = .FALSE.

*      Defined energy band?
        IF ( AS_PHA_DEF(SLOT) ) THEN
          ENERGY = REAL(AS_PHALO(SLOT))
        ELSE
          ENERGY = AS_ENERGY(SLOT)
        END IF

*      Use energy value to choose number of GIS evaluations.
        IF ( ENERGY .LE. 2.0 ) THEN
          NGEVAL = 1
          EGIS(1) = 1
        ELSE IF ( ENERGY .GE. 9.0 ) THEN
          NGEVAL = 1
          EGIS(1) = 3
        ELSE
          NGEVAL = 2
          IF ( ENERGY .LT. 5.9 ) THEN
            EGIS(1) = 1
            EGIS(1) = 2
          ELSE
            EGIS(1) = 2
            EGIS(1) = 3
          END IF
        END IF

      END IF

*    Scale size of psf
      P_SCALE = (0.3/60.0) * MATH__DTOR

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

*    Off-axis angle for centre of psf in arcmin
      OFFAXIS = SQRT(X0**2+Y0**2)*MATH__RTOD*60.0

*    For each point requiring calculation
      DO J = 1, MNY

*      Find Y sub-pixelling
        YSUB = SPIX( YP0 + DY*REAL(J-1), DY )
        SDY = DY / YSUB

        DO I = 1, MNX

*        Zero
          SUM = 0.0

*        Find X sub-pixelling
          XSUB = SPIX( XP0 + DX*REAL(I-1), DX )
          SDX = DX / XSUB

*        Correct normalisation for sub-pixel and pixel size
          LNORM = ABS(SDX*SDY*RTOM*RTOM)

*        Y position of first sub-pixel centre
          YPS = YP0 + DY*(J-1) + 0.5*SDY

*        For each sub-pixel row
          DO JJ = 0, YSUB-1

*          Y distance from psf centre
            YPS2 = (YPS-Y0)**2

*          X position of first sub-pixel centre
            XPS = XP0 + DX*(I-1) + 0.5*SDX

*          For each sub-pixel
            IF ( SIS ) THEN

              DO II = 0, XSUB-1

*              Radius of sub-pixel in arcmin
                ROFF = SQRT((XPS-X0)**2 + YPS2)*MATH__RTOD*60.0

*              Value of function
                SUM = SUM + PSF_ASCA_SIS( OFFAXIS, ROFF, STATUS )

*              Next sub-pixel
                XPS = XPS + SDX

              END DO

            ELSE

              DO II = 0, XSUB-1

*              Radius of sub-pixel in arcmin
                ROFF = SQRT((XPS-X0)**2 + YPS2)*MATH__RTOD*60.0

*              Value of function
                IF ( NGEVAL .EQ. 1 ) THEN
                  SUM = SUM + PSF_ASCA_GIS( EGIS(1), OFFAXIS, ROFF,
     :                                      STATUS )
                ELSE
                  BIT1 = PSF_ASCA_GIS( EGIS(1), OFFAXIS, ROFF, STATUS )
                  BIT2 = PSF_ASCA_GIS( EGIS(2), OFFAXIS, ROFF, STATUS )
                  IF ( EGIS(1) .EQ. 1 ) THEN
                    SUM = SUM + BIT1+(ENERGY-2.0)*(BIT2-BIT1)/(5.9-2.0)
                  ELSE
                    SUM = SUM + BIT1+(ENERGY-5.9)*(BIT2-BIT1)/(9.0-5.9)
                  END IF
                END IF

*              Next sub-pixel
                XPS = XPS + SDX

              END DO

            END IF

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


*+  PSF_ASCA_SIS - Get SIS surface brightness as function of off-axis angle
      REAL FUNCTION PSF_ASCA_SIS( OFFAX, R, STATUS )
*
*    Description :
*
*     Returns surface brightness of the psf in units of integrated probability
*     per square arcminute for given distance R from the psf centre.
*
*    Method :
*
*     Inside OFFAX < 1' the on-axis psf is used. Outside 19' the 20 arcmin
*     psf is used. Inbetween we interpolate. These are MODEs 1,2,3.
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
      REAL			OFFAX			! Offaxis angle (arcmin)
      REAL                      R                       ! Radius from psf centre
                                                        ! in arcmin
*
*    Status :
*
      INTEGER                  	STATUS                  ! Run-time error
*
*    Local constants :
*
      INTEGER			N_0, N_20		! Points in profiles
        PARAMETER		( N_0 = 20, N_20 = 30 )
      REAL			FUDGE_0			! On-axis fudge factor
        PARAMETER		( FUDGE_0 = 0.73021520 )
      REAL			FUDGE_20		! 20' fudge factor
        PARAMETER		( FUDGE_20 = 0.70984818 )
*
*    Function definitions :
*
      REAL			PSF_ASCA_INTERP
*
*    Local variables :
*
      REAL                     	RVAL                    ! Function return value
      REAL                     	RVAL_0, RVAL_20         ! Function return value

      INTEGER			MODE			! Operation mode
*
*    Local data :
*
      REAL                     	SIS_XP_0(2,N_0)
      DATA                     	SIS_XP_0/
     :                          0.00000, 0.63230, 0.10182, 0.59353,
     :                          0.12364, 0.56123, 0.14546, 0.51692,
     :                          0.16727, 0.48000, 0.19636, 0.42738,
     :                          0.21091, 0.36554, 0.23273, 0.32031,
     :                          0.26182, 0.26769, 0.30546, 0.22431,
     :                          0.37091, 0.17077, 0.47273, 0.12277,
     :                          0.69091, 0.07108, 0.96000, 0.04431,
     :                          1.27273, 0.02585, 1.60000, 0.01385,
     :                          1.92727, 0.00831, 2.32727, 0.00462,
     :                          3.63636, 0.00231, 7.27273, 0.00115/
      REAL                      SIS_XP_20(2,N_20)
      DATA                      SIS_XP_20/
     :                          0.00000, 0.55000, 0.04363, 0.54272,
     :                          0.05818, 0.53000, 0.08000, 0.51636,
     :                          0.10181, 0.49090, 0.12363, 0.46636,
     :                          0.14545, 0.44272, 0.16000, 0.41545,
     :                          0.17454, 0.37818, 0.18909, 0.35454,
     :                          0.19636, 0.32363, 0.20363, 0.30000,
     :                          0.21818, 0.27272, 0.23272, 0.24727,
     :                          0.24727, 0.22090, 0.27636, 0.19363,
     :                          0.31272, 0.16818, 0.34181, 0.14363,
     :                          0.40727, 0.12000, 0.49455, 0.09636,
     :                          0.61091, 0.07455, 0.74909, 0.05636,
     :                          0.91636, 0.04364, 1.09091, 0.03455,
     :                          1.28727, 0.02545, 1.56364, 0.01818,
     :                          1.96364, 0.01364, 2.40000, 0.00909,
     :                          3.27273, 0.00455, 4.36364, 0.00000/
*-

*    Which mode?
      IF ( OFFAX .LT. 1.0 ) THEN
        MODE = 1
      ELSE IF ( OFFAX .GT. 19.0 ) THEN
        MODE = 2
      ELSE
        MODE = 3
      END IF

*    Quick test for large radii
      IF ( R .GT. 10.0 ) THEN
        RVAL = 0.0
        GOTO 99
      END IF

*    Use on axis psf?
      IF ( (MODE.EQ.1) .OR. (MODE.EQ.3) ) THEN
        RVAL_0 = PSF_ASCA_INTERP( N_0, SIS_XP_0, R, STATUS ) / FUDGE_0
      ELSE
        RVAL_0 = 0.0
      END IF

*    Use 20 arcmin offaxis psf?
      IF ( (MODE.EQ.2) .OR. (MODE.EQ.3) ) THEN
        RVAL_20 = PSF_ASCA_INTERP( N_20, SIS_XP_20, R, STATUS )/FUDGE_20
      ELSE
        RVAL_20 = 0.0
      END IF

*    Set return value
      IF ( MODE .EQ. 1 ) THEN
        RVAL = RVAL_0

      ELSE IF ( MODE .EQ. 2 ) THEN
        RVAL = RVAL_20

      ELSE
        RVAL = RVAL_0 + (OFFAX/20.0)*(RVAL_20-RVAL_0)

      END IF

*    Set return value
 99   PSF_ASCA_SIS = RVAL

      END



*+  PSF_ASCA_GIS - Get GIS surface brightness as function of off-axis angle
      REAL FUNCTION PSF_ASCA_GIS( IENERGY, OFFAX, R, STATUS )
*
*    Description :
*
*     Returns surface brightness of the psf in units of integrated probability
*     per square arcminute for given distance R from the psf centre.
*
*    Method :
*
*     Inside OFFAX < 1' the on-axis psf is used. Outside 19' the 20 arcmin
*     psf is used. Inbetween we interpolate. These are MODEs 1,2,3.
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
      INTEGER			IENERGY			! Energy band to use
      REAL			OFFAX			! Offaxis angle (arcmin)
      REAL                      R                       ! Radius from psf centre
                                                        ! in arcmin
*
*    Status :
*
      INTEGER                  	STATUS                  ! Run-time error
*
*    Local constants :
*
      INTEGER			NSP			! Number of spots
        PARAMETER		( NSP = 15 )
      REAL			FUDGE_LO_0		! Low energy
        PARAMETER		( FUDGE_LO_0 = 0.6701705 )
      REAL			FUDGE_ME_0		! Medium energy
        PARAMETER		( FUDGE_ME_0 = 0.6503103 )
      REAL			FUDGE_HI_0		! High energy
        PARAMETER		( FUDGE_HI_0 = 0.6458237 )
      REAL			FUDGE_LO_20		! Low energy
        PARAMETER		( FUDGE_LO_20 = 0.7753197 )
      REAL			FUDGE_ME_20		! Medium energy
        PARAMETER		( FUDGE_ME_20 = 0.7090034 )
      REAL			FUDGE_HI_20		! High energy
        PARAMETER		( FUDGE_HI_20 = 0.7032154 )
*
*    Function definitions :
*
      REAL			PSF_ASCA_INTERP
*
*    Local variables :
*
      REAL                     	RVAL                    ! Function return value
      REAL			RVAL_0, RVAL_20
*
*    Local data :
*
      REAL                     	GIS_LO_0(2,NSP)
      DATA                     	GIS_LO_0/
     :                          0.00000, 0.20000, 0.14545, 0.19600,
     :                          0.25455, 0.18000, 0.40000, 0.15000,
     :                          0.50909, 0.13000, 0.59636, 0.10500,
     :                          0.83636, 0.07500, 0.94545, 0.06000,
     :                          1.01818, 0.05000, 1.16364, 0.03300,
     :                          1.38182, 0.03000, 1.56363, 0.02000,
     :                          1.81818, 0.01000, 2.61818, 0.00500,
     :                          4.00000, 0.00000/
      REAL                     	GIS_ME_0(2,NSP)
      DATA                     	GIS_ME_0/
     :                          0.00000, 0.31500, 0.08727, 0.30434,
     :                          0.14545, 0.28786, 0.18182, 0.26848,
     :                          0.21818, 0.24812, 0.29091, 0.23068,
     :                          0.36364, 0.18900, 0.55273, 0.11437,
     :                          0.72727, 0.07366, 0.94545, 0.04362,
     :                          1.09091, 0.03586, 1.45454, 0.01939,
     :                          1.81818, 0.01000, 2.61818, 0.00500,
     :                          4.00000, 0.00000/
      REAL                     	GIS_HI_0(2,NSP)
      DATA                     	GIS_HI_0/
     :                          0.00000, 0.37000, 0.07273, 0.35558,
     :                          0.10909, 0.33636, 0.18182, 0.30273,
     :                          0.25455, 0.24026, 0.29091, 0.21816,
     :                          0.36364, 0.17779, 0.47273, 0.13935,
     :                          0.62545, 0.08938, 0.82182, 0.06055,
     :                          1.04000, 0.03652, 1.30909, 0.02114,
     :                          1.81818, 0.01000, 2.61818, 0.00500,
     :                          4.00000, 0.00000/
      REAL                     	GIS_LO_20(2,NSP)
      DATA                     	GIS_LO_20/
     :                          0.00000, 0.18000, 0.18182, 0.17500,
     :                          0.29091, 0.16500, 0.36364, 0.15500,
     :                          0.43636, 0.14000, 0.50909, 0.13000,
     :                          0.58182, 0.11000, 0.72727, 0.08500,
     :                          0.98182, 0.06000, 1.16364, 0.04000,
     :                          1.45454, 0.02500, 1.81818, 0.01500,
     :                          2.54545, 0.00700, 3.27272, 0.00500,
     :                          4.00000, 0.00000/
      REAL                     	GIS_ME_20(2,NSP)
      DATA                     	GIS_ME_20/
     :                          0.00000, 0.29500, 0.14545, 0.28000,
     :                          0.23273, 0.25000, 0.26909, 0.23000,
     :                          0.32727, 0.20600, 0.43636, 0.17700,
     :                          0.50909, 0.14000, 0.58182, 0.11500,
     :                          0.65455, 0.09000, 0.81455, 0.06200,
     :                          1.01818, 0.04000, 1.30909, 0.02500,
     :                          1.81818, 0.01000, 2.54545, 0.00700,
     :                          4.00000, 0.00000/
      REAL                     	GIS_HI_20(2,NSP)
      DATA                     	GIS_HI_20/
     :                          0.00000, 0.34500, 0.12364, 0.33000,
     :                          0.16000, 0.32000, 0.25455, 0.26500,
     :                          0.36364, 0.20000, 0.43636, 0.15500,
     :                          0.58182, 0.10500, 0.76364, 0.07800,
     :                          0.90909, 0.05500, 0.98182, 0.04500,
     :                          1.41818, 0.02000, 1.96363, 0.01000,
     :                          2.25454, 0.00700, 2.90909, 0.00500,
     :                          4.00000, 0.00000/
*-

*    Low energy
      IF ( IENERGY .EQ. 1 ) THEN

*      Interpolate
        RVAL_0 = PSF_ASCA_INTERP( NSP, GIS_LO_0, R, STATUS )
     :                          / FUDGE_LO_0
        RVAL_20 = PSF_ASCA_INTERP( NSP, GIS_LO_20, R, STATUS )
     :                          / FUDGE_LO_20

*    Medium energy
      ELSE IF ( IENERGY .EQ. 2 ) THEN

*      Interpolate
        RVAL_0 = PSF_ASCA_INTERP( NSP, GIS_ME_0, R, STATUS )
     :                          / FUDGE_ME_0
        RVAL_20 = PSF_ASCA_INTERP( NSP, GIS_ME_20, R, STATUS )
     :                          / FUDGE_ME_20

*    High energy
      ELSE IF ( IENERGY .EQ. 3 ) THEN

*      Interpolate
        RVAL_0 = PSF_ASCA_INTERP( NSP, GIS_HI_0, R, STATUS )
     :                          / FUDGE_HI_0
        RVAL_20 = PSF_ASCA_INTERP( NSP, GIS_HI_20, R, STATUS )
     :                          / FUDGE_HI_20

      END IF

*    Set return value
 99   PSF_ASCA_GIS = RVAL_0 + OFFAX/20.0*(RVAL_20-RVAL_0)

      END



*+  PSF_ASCA_INTERP - Psf interpolation
      REAL FUNCTION PSF_ASCA_INTERP( NSPOT, SPOTS, R, STATUS )
*
*    Description :
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
*    Global constants :
*
      INCLUDE 'SAE_PAR'
*
*    Import :
*
      INTEGER			NSPOT			! Number of spots
      REAL			SPOTS(2,NSPOT)		! Spot values
      REAL                      R                       ! Radius from psf centre
                                                        ! in arcmin
*
*    Status :
*
      INTEGER                  	STATUS                  ! Run-time error
*
*    Local variables :
*
      REAL                     	INR(5)                  ! Interp radii
      REAL                     	INP(5)                  ! Interp psf values
      REAL                     	RVAL                    ! Function return value

      INTEGER                  	I, II, IR               ! Loop variables
      INTEGER                  	NIN                     ! # interpolates
*-

*    Check status
      IF ( (STATUS .NE. SAI__OK) .OR. (R.GT.10.0) ) THEN
        RVAL = 0.0
        GOTO 99
      END IF

*    Find bin of SPOTS containing our radius
      IF ( R .GT. SPOTS(1,NSPOT) ) THEN
        IR = NSPOT
      ELSE
        IR = 1
        DO WHILE ( (IR.LE.NSPOT) .AND. (R.GT.SPOTS(1,IR)) )
          IR = IR + 1
        END DO
        IF ( IR .GT. NSPOT ) IR = NSPOT
      END IF

*    Set up for interpolation
      NIN = 0
      DO I = IR-2, MIN(NSPOT,IR+2)
        NIN = NIN + 1
        IF ( I .LT. 1 ) THEN
          II = 1 - I
        ELSE
          II = I
        END IF
        INR(NIN) = SPOTS(1,II)
        INP(NIN) = SPOTS(2,II)
      END DO

*    Interpolate
      CALL MATH_INTERP( NIN, INR, INP, 1, R, 2, RVAL, STATUS )

*    Check positive
      IF ( RVAL .LT. 0.0 ) THEN
        CALL MATH_INTERP( NIN, INR, INP, 1, R, 1, RVAL, STATUS )
        RVAL = MAX(0.0,RVAL)
      END IF

*    Set return value
 99   PSF_ASCA_INTERP = RVAL

      END

*+  PSF_ASCA_DEF - ASCA PSF time/energy definition
      SUBROUTINE PSF_ASCA_DEF( SLOT, TLO, THI, ELO, EHI, UIN, UOUT,
     :                                                     STATUS )
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
*     28 Oct 92 : Original (DJA)
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
      INCLUDE 'PSF_ASCA_CMN'
*
*    Import :
*
      INTEGER                  SLOT                    ! Psf slot id
      DOUBLE PRECISION         TLO, THI                ! Time bounds
      INTEGER                  ELO, EHI                ! Energy channel bounds
      BYTE                     UIN(*), UOUT(*)         ! User in/out
*
*    Status :
*
      INTEGER                  STATUS                  ! Run-time error
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Define only energy bounds
      AS_PHA_DEF(SLOT) = .TRUE.
      AS_PHALO(SLOT) = ELO
      AS_PHAHI(SLOT) = EHI

      END

*+  PSF_ASCA_HINT - ASCA psf hint handler
      SUBROUTINE PSF_ASCA_HINT( SLOT, HINT, DATA, STATUS )
*
*    Description :
*
*     Return hints about the ASCA psf.
*
*    Method :
*    Deficiencies :
*    Authors :
*
*     David J. Allan (ROSAT, University of Birmingham)
*
*    History :
*
*     12 Jan 93 : Original (DJA)
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
*
*    Global variables :
*
      INCLUDE 'PSF_ASCA_CMN'
*
*    Import :
*
      INTEGER                 	SLOT            	! PSF handle
      CHARACTER*(*)           	HINT		 	! Hint name
*
*    Export :
*
      BYTE			DATA(*)			! Hint data
*
*    Status :
*
      INTEGER                   STATUS
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Radial symmetry?
      IF ( HINT .EQ. PSF_H_RADSYM ) THEN

*      All our models are radially symmetric about on-axis direction
        CALL ARR_COP1L( 1, .TRUE., DATA, STATUS )

*    Vary with detector position?
      ELSE IF ( HINT .EQ. PSF_H_POSDEP ) THEN

*      All our models vary with off-axis angle
        CALL ARR_COP1L( 1, .TRUE., DATA, STATUS )

*    Energy dependent
      ELSE IF ( HINT .EQ. PSF_H_ENDEP ) THEN

*      Only the GIS psf is energy dependent
        CALL ARR_COP1L( 1, (AS_INSTR(SLOT).EQ.'GIS'), DATA, STATUS )

      ELSE
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'Unknown psf hint /'//HINT//'/', STATUS )

      END IF

      END

*+  PSF_ASCA_INIT - ASCA psf initialisation
      SUBROUTINE PSF_ASCA_INIT( PSLOT, LOC, STATUS )
*
*    Description :
*
*     Associate a slot with an ASCA psf.
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
*     12 Jan 94 : Original (DJA)
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
      INCLUDE 'PSF_ASCA_CMN'
*
*    Import :
*
      INTEGER                 PSLOT(2)           ! PSF handle
      CHARACTER*(DAT__SZLOC)  LOC
*
*    Status :
*
      INTEGER STATUS
*
*    Function declarations :
*
      LOGICAL                 CHR_SIMLR
*
*    Local variables :
*
      CHARACTER*20            MASK              ! Mask name

      INTEGER                 SLOT              !
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Set up as user defined
      SLOT = PSLOT(1)

*    Get mask name
      CALL USI_PROMT( 'MASK', 'ASCA detector (GIS or SIS)', STATUS )
      CALL USI_DEF0C( 'MASK', 'SIS', STATUS )
 10   CALL USI_GET0C( 'MASK', MASK, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Validate choice
      IF ( CHR_SIMLR(MASK,'GIS') ) THEN
        AS_INSTR(SLOT) = 'GIS'

      ELSE IF ( CHR_SIMLR(MASK,'SIS') ) THEN

        AS_INSTR(SLOT) = 'SIS'

      ELSE
        STATUS = SAI__ERROR
        CALL MSG_SETC( 'MASK', MASK )
        CALL ERR_REP( ' ', 'Invalid ASCA instrument name'/
     :                /' /^MASK/', STATUS )

      END IF
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Get a mean photon energy
      IF ( (AS_INSTR(SLOT).EQ.'GIS') .AND. .NOT. AS_PHA_DEF(SLOT) ) THEN
        CALL USI_PROMT( 'AUX', 'Mean photon energy in KeV', STATUS )
        CALL USI_GET0R( 'AUX', AS_ENERGY(SLOT), STATUS )
      END IF

*    Tidy up
 99   IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'PSF_ASCA_INIT', STATUS )
      END IF

      END

*+  PSF_EXOLE - A fit to empirical LE PSF data
      SUBROUTINE PSF_EXOLE( DUM,X0, Y0, QX, QY, DX, DY, INTEG, NX, NY,
     :                                                 ARRAY, STATUS )
*
*    Description :
*
*     Returns array of values for a PSF centred at (X0,Y0) where the central
*     bin is at a position (QX,QY) from the PSF centre. There are NX by NY
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
*     10 Jul 89 : Original (DJA)
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
*
*    Import :
*
      REAL                     DX, DY, X0, Y0
      REAL                     QX,QY
      INTEGER                  NX,NY,DUM
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
*    Local constants :
*
      REAL                     RTOS2                   ! radians to arcsec^2
         PARAMETER             ( RTOS2 = MATH__RTOD * MATH__RTOD
     :                                       * 3600.0 * 3600.0 )
      REAL                     GN1, GW1                ! 1st gaussian
      REAL                     GN2, GW2                ! 2nd gaussian
      REAL                     KIN, KS0, KRC           ! King model for wings
         PARAMETER             ( GN1 = 1.3707E-2, GN2 = 1.5394E-2,
     :                           GW1 = 1.278*4.0, GW2 = 2.472*4.0,
     :                           KRC = 3.472*4.0, KIN = 1.187,
     :                                           KS0 = 2.0838E-3 )
*
*    Local variables :
*
      REAL                     NORM, C1, C2, C3, AVAL
      REAL                     R2,PX,PY
      INTEGER                  I, J
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Normalisation must take size of pixels into account so that, if the
*    whole PSF was asked for, the sum would be unity regardless of the
*    density of pixels. Must use absolute value to take account of axes
*    where DX or DY are negative.
      NORM = ABS(DX*DY*RTOS2)/
     :              (0.5969 + GN1*GW1*GW1 + GN2*GW2*GW2)/MATH__PI/2.0

      C1 = -0.5*KRC*KRC
      C2 = 1.0/GW1/GW1
      C3 = 1.0/GW2/GW2

*    Check to see if PSF offset from centre of array is so insignificant
*    that we can take a shortcut. We compare the magnitude of the offset
*    (QX,QY) to a PSF pixel diagonal. If the former is smaller by a factor
*    of > 1000 then we forget the offset completely. This means we need
*    only evaluate 1/4 of the PSF
      IF ( SQRT((QX*QX+QY*QY)/(DX*DX+DY*DY)) .LT. 0.001 ) THEN
         DO J = 0, NY/2
            PY = J*J*DY*DY
            DO I = 0, NX/2
               R2 = ( PY + I*I*DX*DX ) * RTOS2 * -0.5
               AVAL = ( GN1*DEXP(DBLE(R2*C2)) +
     :                        GN2*DEXP(DBLE(R2*C3)) +
     :                        KS0*(1.0+R2/C1)**(-KIN) )*NORM
               ARRAY(I,J) = AVAL
               ARRAY(I,-J) = AVAL
               ARRAY(-I,J) = AVAL
               ARRAY(-I,-J) = AVAL
            END DO
         END DO

      ELSE
         PY = QY + (-NY/2)*DY
         DO J = -NY/2, NY/2
            PX = QX + (-NX/2)*DX
            DO I = -NX/2, NX/2
               R2 = ( PX*PX + PY*PY ) * RTOS2 * -0.5
               ARRAY(I,J) = ( GN1*DEXP(DBLE(R2*C2)) +
     :                        GN2*DEXP(DBLE(R2*C3)) +
     :                        KS0*(1.0+R2/C1)**(-KIN) )*NORM
               PX = PX + DX
            END DO
            PY = PY + DY
         END DO

      END IF

      END

*+  PSF_PWFC - Interrogate the calibration database
      SUBROUTINE PSF_PWFC( SLOT, X0, Y0, QX, QY, DX, DY, INTEG, NX, NY,
     :                                                  ARRAY, STATUS )
*
*    Description :
*
*     Returns array of values for a PSF centred at (X0,Y0) where the central
*     bin is at a position (QX,QY) from the PSF centre. There are NX by NY
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
*     10 Jul 89 : Original ( DJA )
*     23 Apr 90 : Supplies correct energy derived from filter ID (DJA)
*     23 May 90 : Uses new CAL system (DJA)
*      2 Feb 93 : Sign of QX corrected (DJA)
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
      INCLUDE 'PSF_WFC_CMN'
*
*    Import :
*
      INTEGER                  SLOT
      REAL                     DX, DY, X0, Y0
      REAL                     QX,QY
      INTEGER                  NX,NY
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
*    Local variables :
*
      REAL                     LDX, LQX                ! Local copies for CAL
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    CAL doesn't handle coordinates as you'd expect!
      LDX = ABS(DX)
      LQX = ABS(QX)

      IF ( INTEG ) THEN
        CALL CAL_PSFT2D( WF_MJD(SLOT), WF_FID(SLOT), WF_ENERGY(SLOT),
     :                   -X0, LDX, NX, Y0, ABS(DY), NY, QX*(DX/LDX),
     :                   QY,ARRAY,STATUS )
      ELSE
        CALL CAL_PSF2D( WF_MJD(SLOT), WF_FID(SLOT), WF_ENERGY(SLOT),
     :                  -X0, ABS(DX), NX, Y0,
     :                  ABS(DY), NY, QX*(DX/LDX), QY, ARRAY,STATUS )
      END IF

      END

*+  PSF_PWFC_CLOSE - Shutdown the WFC psf system
      SUBROUTINE PSF_PWFC_CLOSE( STATUS )
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
*     23 May 89 : Original (DJA)
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
      INCLUDE 'PSF_WFC_CMN'
*
*    Status :
*
      INTEGER                  STATUS                  ! Run-time error
*
*    Local variable :
*
      INTEGER                  IGNORE
*-

*    Shut down WFC calibration database if open
      IGNORE = SAI__OK
      IF ( WF_CALOK ) THEN
C         CALL CAL_CLOSE( IGNORE )
        WF_CALOK= .FALSE.
        WF_INIT = .FALSE.

        IF ( IGNORE .NE. SAI__OK ) THEN
          CALL MSG_PRNT( 'WARNING : An error occurred releasing'/
     :                         /' the WFC calibration database.' )
        END IF

      END IF

      END

*+  PSF_PWFC_INIT - Initialise the WFC pointed psf system
      SUBROUTINE PSF_PWFC_INIT( SLOT, LOC, STATUS )
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
      INCLUDE 'PSF_WFC_CMN'
*
*    Status :
*
      INTEGER                  STATUS                  ! Run-time error
*
*    Import :
*
      INTEGER                  SLOT                    ! PSF slot id
      CHARACTER*(DAT__SZLOC)   LOC                     ! Dataset locator
*
*    Functions :
*
      INTEGER                  CAL_FILT_S2N
      INTEGER                  CHR_LEN
*
*    Local variables :
*
      CHARACTER*(DAT__SZLOC)   HLOC                    ! HEADER locator
      CHARACTER*(DAT__SZLOC)   ILOC                    ! INSTRUMENT locator
      CHARACTER*(DAT__SZLOC)   SLOC                    ! SORT locator
      CHARACTER*80             CID                     ! Filter description

      REAL                     IRIS                    ! Sort iris value

      INTEGER                  BDA                     ! BDA identifier
      INTEGER                  CALFN                   ! CAL filter id
      INTEGER                  IFILT                   ! Dataset filter id

      LOGICAL                  FILTER_OK, IRIS_OK      ! Sort components there?
      LOGICAL                  THERE
      LOGICAL                  I_THERE, S_THERE, B_THERE
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    System already initialised?
      IF ( .NOT. WF_CALOK ) THEN
        CALL CAL_INIT( STATUS )
        IF ( STATUS .NE. SAI__OK ) THEN
          CALL MSG_PRNT( 'Unable to open WFC calibration database.' )
          CALL MSG_PRNT( 'Check assignment of CAL_WFC_MASTER '/
     :                                       /'logical name.' )
        ELSE
          WF_CALOK = .TRUE.
        END IF
      END IF

*    Set the SURVEY flag
      WF_SURVEY(SLOT) = .FALSE.
      IRIS_OK = .FALSE.
      FILTER_OK = .FALSE.

*    Is locator valid
      CALL DAT_VALID( LOC, WF_DATASET(SLOT), STATUS )
      IF ( WF_DATASET(SLOT) ) THEN

*      Get BDA identifier
        CALL BDA_FIND( LOC, BDA, STATUS )

*      Store locator
        WF_LOC(SLOT) = LOC
        WF_MCP(SLOT) = 2

*      Try and get MJD from dataset
        CALL BDA_CHKHEAD_INT( BDA, THERE, STATUS )
        B_THERE = .FALSE.
        IF ( THERE ) THEN
          CALL BDA_LOCHEAD_INT( BDA, HLOC, STATUS )
          CALL DAT_THERE(HLOC,'BASE_MJD',B_THERE,STATUS)
          IF ( B_THERE ) THEN
            CALL CMP_GET0D( HLOC, 'BASE_MJD', WF_MJD(SLOT), STATUS )
            IF ( STATUS .NE. SAI__OK ) THEN
              CALL ERR_ANNUL(STATUS)
              B_THERE = .FALSE.
            END IF
          END IF
        END IF
        IF ( .NOT. B_THERE ) WF_MJD(SLOT) = 48000.0D0

*      Get the detector id from CAL
        CALL CIN_SET_DET( WF_MJD(SLOT), WF_MCP(SLOT), STATUS )
        IF ( STATUS .NE. SAI__OK ) THEN
          CALL MSG_PRNT( 'No detector id present - '/
     :        /'assuming detector 2 for psf access' )
          STATUS = SAI__OK
          WF_MCP(SLOT) = 2
        END IF

*      Locate components in dataset
        CALL BDA_CHKINSTR_INT( BDA, I_THERE, STATUS )
        IF ( I_THERE ) THEN
          CALL BDA_LOCINSTR_INT( BDA, ILOC, STATUS )
          CALL DAT_THERE( ILOC, 'SORT', S_THERE, STATUS )
        END IF
        IF ( I_THERE .AND. S_THERE ) THEN
          CALL DAT_FIND( ILOC, 'SORT', SLOC, STATUS )

*        Look for filter id
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
        WF_FID(SLOT) = CALFN

*      Warn user if no filter or iris
        IF ( .NOT. FILTER_OK ) THEN
          WF_FID(SLOT) = CAL_FILT_S2N( 'P1' )
          CALL MSG_PRNT( 'Unable to get filter id from dataset -'/
     :                                     /' defaulting to ...' )
        END IF

*      Use filter to get stuff - inform user and store energy
        CALL CAL_FILT_INFO( WF_FID(SLOT), CID, WF_ENERGY(SLOT), STATUS )
        CALL MSG_SETC( 'ID', CID )
        CALL MSG_PRNT( '   Filter ^ID')

        IF ( .NOT. IRIS_OK ) THEN
          IRIS = 2.5
          CALL MSG_PRNT( 'Unable to get IRIS value from dataset'/
     :                          /' - defaulting to 2.5 degrees' )
        END IF

*      Convert IRIS value to radians
        WF_IRIS(SLOT) = IRIS * MATH__DTOR

 69     CONTINUE

      END IF

      END

*+  PSF_RADIAL - 1D user supplied psf handler
      SUBROUTINE PSF_RADIAL( SLOT, X0, Y0, QX, QY, DX, DY, INTEG, NX,
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
      INCLUDE 'DAT_PAR'
      INCLUDE 'PSF_PAR'
      INCLUDE 'MATH_PAR'
      INCLUDE 'PSF_RADIAL_CMN'
*
*    Import :
*
      REAL                     DX, DY, X0, Y0,QX,QY
      INTEGER                  NX,NY,SLOT
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
              SUM = SUM + PSF_RADIAL_INT( SQRT(RPS), RD_DIM(SLOT),
     :                RD_REG(SLOT), %VAL(RD_DPTR(SLOT)),
     :                %VAL(RD_APTR(SLOT)), %VAL(RD_WPTR(SLOT)), STATUS )

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
      REAL FUNCTION PSF_RADIAL_INT( R, N, REG, DAT, AX, WID, STATUS )
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
      LOGICAL                  REG                     ! Profile is regular?
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

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Regular values
      IF ( REG ) THEN
        CP = INT((R-AX(1)) / WID(1) + 0.5)+1
      ELSE
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
      END IF

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
      SUBROUTINE PSF_RADIAL_INIT( SLOT, LOC, STATUS )
*
*    Description :
*
*     Gets a user defined PSF and associates it with a PSF slot.
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
      INCLUDE 'DAT_PAR'
      INCLUDE 'PSF_PAR'
      INCLUDE 'PSF_RADIAL_CMN'
*
*    Import :
*
      INTEGER                 SLOT              ! PSF handle
      CHARACTER*(DAT__SZLOC)  LOC
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
      CHARACTER               TLOC*(DAT__SZLOC) ! Table file ptr
      CHARACTER*40            UNITS             ! Axis units

      REAL                    BASE, SCALE       ! Axis attributes
      REAL                    TOR               ! Conversion to radians

      INTEGER                 APTR              ! Ptr to axis data
      INTEGER                 BDA               ! BDA identifier
      INTEGER                 DIMS(DAT__MXDIM)  ! Size of data array
      INTEGER                 DPTR              ! Ptr to data
      INTEGER                 NDIM              ! Dimensionality
      INTEGER                 WPTR              ! Ptr to axis width data

      LOGICAL                 OK                ! General validity check
      LOGICAL                 VALID             ! Have we a valid dataset?
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Get user's choice of table
      CALL USI_PROMT( 'MASK', 'Dataset containing radial profile',
     :                                                    STATUS )
      CALL USI_GET0C( 'MASK', TNAME, STATUS )
      CALL USI_CANCL( 'MASK', STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Try to open file
      CALL HDS_OPEN( TNAME(:CHR_LEN(TNAME)), 'READ', TLOC, STATUS )

*    Check the data
      CALL BDA_FIND( TLOC, BDA, STATUS )
      CALL BDA_CHKDATA_INT( BDA, VALID, NDIM, DIMS, STATUS )
      IF ( VALID ) THEN

        IF ( NDIM .NE. 1 ) THEN
          STATUS = SAI__ERROR
          CALL ERR_REP( ' ', 'This isn''t a 1-dimensional dataset!',
     :                                                      STATUS )
        END IF

*      Map if ok
        CALL BDA_MAPDATA_INT( BDA, 'READ', DPTR, STATUS )

*      If ok then make a copy of this data and store
        IF ( STATUS .EQ. SAI__OK ) THEN
          RD_LOC(SLOT) = TLOC
          RD_DIM(SLOT) = DIMS(1)
          CALL DYN_MAPR( 1, DIMS(1), RD_DPTR(SLOT), STATUS )
          CALL ARR_COP1R( DIMS(1), %VAL(DPTR),
     :                    %VAL(RD_DPTR(SLOT)), STATUS )

*        Get axis info
          CALL BDA_CHKAXVAL_INT( BDA, 1, OK, RD_REG(SLOT),
     :                           DIMS(1), STATUS )

*        Profile bin positions and heights
          CALL DYN_MAPR( 1, DIMS(1), RD_APTR(SLOT), STATUS )
          CALL DYN_MAPR( 1, DIMS(1), RD_WPTR(SLOT), STATUS )

*        Regular axis?
          IF ( RD_REG(SLOT) ) THEN
            CALL BDA_GETAXVAL_INT( BDA, 1, BASE, SCALE, DIMS(1),
     :                                                  STATUS )

*          Fill axis values array
            CALL ARR_REG1R( BASE, SCALE, DIMS(1),
     :                      %VAL(RD_APTR(SLOT)), STATUS )

*          Fill widths array
            CALL ARR_INIT1R( SCALE, DIMS(1),
     :                       %VAL(RD_WPTR(SLOT)), STATUS )

*        Irregular axis
          ELSE

*          Map axis data
            CALL BDA_MAPAXVAL_INT( BDA, 1, APTR, STATUS )
            CALL ARR_COP1R( DIMS(1), %VAL(APTR), %VAL(RD_APTR(SLOT)),
     :                      STATUS )

*          And the widths
            CALL BDA_MAPAXWID_INT( BDA, 1, WPTR, STATUS )
            CALL ARR_COP1R( DIMS(1), %VAL(WPTR),
     :                      %VAL(RD_WPTR(SLOT)), STATUS )

          END IF

*        Get axis units
          CALL BDA_GETAXUNITS_INT( BDA, 1, UNITS, STATUS )

*        Convert axis units
          CALL CONV_UNIT2R( UNITS, TOR, STATUS )

*        Normalise the profile
          CALL PSF_RADIAL_NORM( DIMS(1), %VAL(RD_DPTR(SLOT)),
     :              %VAL(RD_APTR(SLOT)), %VAL(RD_WPTR(SLOT)),
     :              TOR, STATUS )

        END IF

*      Inform user
        IF ( STATUS .EQ. SAI__OK ) THEN
          CALL MSG_SETC( 'FILE', TNAME )
          CALL MSG_PRNT( 'PSF read in from file ^FILE' )
        END IF

      END IF

*    Energy radii not present
      RD_NLEV(SLOT) = 0

*    Release from BDA
      IF ( ( STATUS .EQ. SAI__OK ) .AND. VALID ) THEN
        CALL BDA_RELEASE_INT( BDA, STATUS )
        CALL HDS_CLOSE( TLOC, STATUS )
      END IF

*    Tidy up
 99   IF ( STATUS .NE. SAI__OK ) THEN
        CALL ERR_REP( ' ', '...from PSF_RADIAL_INIT', STATUS )
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
      SUBROUTINE PSF_RESPFILE( SLOT, X0, Y0, QX, QY, DX, DY, INTEG, NX,
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
      INCLUDE 'DAT_PAR'
      INCLUDE 'PRM_PAR'
      INCLUDE 'PSF_PAR'
      INCLUDE 'PSF_RESPFILE_CMN'
*
*    Import :
*
      REAL                     DX, DY, X0, Y0,QX,QY
      INTEGER                  NX,NY,SLOT
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
      REAL                     R			! Off-axis angle

      INTEGER                  IE			! Response energy bin
      INTEGER                  IPSF			! Sequential psf number
      INTEGER                  IPTR			! Pointer into index
      INTEGER                  IR			! Response radial bin
      INTEGER                  IX			! Response X axis bin
      INTEGER                  IY			! Response Y axis bin
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Radial response?
      IF ( RF_RADIAL(SLOT) ) THEN

*      Choose radial bin
        R = SQRT( X0**2 + Y0**2 )
        IR = INT(0.5+(R-RF_BASE(3,SLOT)) / RF_SCALE(3,SLOT)) + 1
        IR = MIN( IR, RF_DIMS(3,SLOT) )

*      Choose energy bin
        IF ( RF_PHA_DEF(SLOT) .AND. (RF_NDIM(SLOT).GT.3) ) THEN
          IE = INT((RF_PHALO(SLOT)-RF_BASE(4,SLOT)) /
     :                       RF_SCALE(4,SLOT)) + 1
        ELSE
          IE = 1
        END IF

*      This selects the psf
        IPSF = (IE-1)*RF_DIMS(3,SLOT) + IR

      ELSE

*      Choose spatial bins
        IX = INT(0.5+(X0-RF_BASE(3,SLOT)) / RF_SCALE(3,SLOT)) + 1
        IY = INT(0.5+(X0-RF_BASE(4,SLOT)) / RF_SCALE(4,SLOT)) + 1

*      Choose energy bin
        IF ( RF_PHA_DEF(SLOT) .AND. (RF_NDIM(SLOT).GT.4) ) THEN
          IE = INT((RF_PHALO(SLOT)-RF_BASE(5,SLOT)) /
     :                       RF_SCALE(5,SLOT)) + 1
        ELSE
          IE = 1
        END IF

*      This selects the psf
        IPSF = (IE-1)*RF_DIMS(3,SLOT)*RF_DIMS(4,SLOT) +
     :         (IY-1)*RF_DIMS(3,SLOT) + (IX-1)

      END IF

*    The psf index allows us to index the INDEX array
      IPTR = RF_IPTR(SLOT) + 3*VAL__NBI*(IPSF-1)

*    Use that index to unpack the response data
      CALL PSF_RESPFILE_INT( %VAL(IPTR), %VAL(RF_DPTR(SLOT)),
     :                       NX, NY, ARRAY, STATUS )

*    Non-zero offset?
      IF ( (QX.NE.0.0) .AND. (QY.NE.0.0) ) THEN

*      Reallocate resampling workspace if not enough
        IF ( (RF_RESPTR(SLOT) .GT. 0) .AND.
     :       (NX*NY.LT.RF_RESNXY(SLOT)) ) THEN
          CALL DYN_UNMAP( RF_RESPTR(SLOT), STATUS )
          RF_RESPTR(SLOT) = 0
        END IF
        IF ( RF_RESPTR(SLOT) .EQ. 0 ) THEN
          CALL DYN_MAPR( 1, NX*NY, RF_RESPTR(SLOT), STATUS )
          RF_RESNXY(SLOT) = NX*NY
        END IF

*      Make copy of psf
        CALL ARR_COP1R( NX*NY, ARRAY, %VAL(RF_RESPTR(SLOT)), STATUS )

*      Resample it
        CALL PSF_RESAMPLE( NX, NY, %VAL(RF_RESPTR(SLOT)), QX/DX, QY/DY,
     :                                               0, ARRAY, STATUS )

      END IF

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

*+  PSF_RESPFILE_DEF - RESPFILE PSF time/energy definition
      SUBROUTINE PSF_RESPFILE_DEF( SLOT, TLO, THI, ELO, EHI, UIN, UOUT,
     :                                                         STATUS )
*
*    Description :
*
*    Method :
*
*    Deficiencies :
*    Bugs :
*    Authors :
*
*     David J. Allan (ROSAT,University of Birmingham)
*
*    History :
*
*     21 Dec 93 : Original (DJA)
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
      INCLUDE 'PSF_RESPFILE_CMN'
*
*    Import :
*
      INTEGER                  SLOT                    ! Psf slot id
      DOUBLE PRECISION         TLO, THI                ! Time bounds
      INTEGER                  ELO, EHI                ! Energy channel bounds
      BYTE                     UIN(*), UOUT(*)         ! User in/out
*
*    Status :
*
      INTEGER                  STATUS                  ! Run-time error
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Define only energy bounds
      RF_PHA_DEF(SLOT) = .TRUE.
      RF_PHALO(SLOT) = ELO
      RF_PHAHI(SLOT) = EHI

      END

*+  PSF_RESPFILE_HINT - Spatial response hint handler
      SUBROUTINE PSF_RESPFILE_HINT( SLOT, HINT, DATA, STATUS )
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
*     23 Dec 93 : Original (DJA)
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
*
*    Global variables :
*
      INCLUDE 'PSF_RESPFILE_CMN'
*
*    Import :
*
      INTEGER                 	SLOT            	! PSF handle
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

*      Radially symmetric flag from common block
        CALL ARR_COP1L( 1, RF_RADIAL(SLOT), DATA, STATUS )

*    Energy dependent
      ELSE IF ( HINT .EQ. PSF_H_ENDEP ) THEN

*      Energy dependent if last dimension has dimension other than one
        CALL ARR_COP1L( 1, (RF_DIMS(RF_NDIM(SLOT),SLOT).NE.1),
     :                  DATA, STATUS )

*    Maximum radius
      ELSE IF ( HINT .EQ. PSF_H_MAXRAD ) THEN

*      Can't be more than radius of a response psf
        CALL ARR_COP1I( 1, RF_DIMS(1,SLOT)/2, DATA, STATUS )

*    Spatial granularity
      ELSE IF ( HINT .EQ. PSF_H_SPATGRAN ) THEN

*      Radial psf?
        IF ( RF_RADIAL(SLOT) ) THEN
          CALL ARR_COP1R( 1, RF_SCALE(3,SLOT), DATA, STATUS )
        ELSE
          CALL ARR_COP1R( 1, MIN(RF_SCALE(3,SLOT),RF_SCALE(4,SLOT)),
     :                    DATA, STATUS )
        END IF

      ELSE
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'Unknown psf hint /'//HINT//'/', STATUS )

      END IF

      END

*+  PSF_RESPFILE_INIT - Response defined PSF initialisation
      SUBROUTINE PSF_RESPFILE_INIT( SLOT, LOC, STATUS )
*
*    Description :
*
*     Gets a user defined PSF and associates it with a PSF slot.
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
      INCLUDE 'PSF_PAR'
      INCLUDE 'PSF_RESPFILE_CMN'
*
*    Import :
*
      INTEGER            	SLOT(2)              	! PSF handle
      CHARACTER*(DAT__SZLOC)  	LOC			! Dataset locator
*
*    Status :
*
      INTEGER STATUS
*
*    Functions :
*
      REAL 			PSF1_GETAXDR
      REAL 			PSF1_GETAXTOR
*
*    Local variables :
*
      CHARACTER*1		DAXC			! Axis character
      CHARACTER*40		EUNITS			! Energy axis units
      CHARACTER*(DAT__SZLOC)  	SLOC              	! Response locator
      CHARACTER*80		TEXT			! AUX prompt
      CHARACTER*30		VERSION			! SPRESP version

      REAL			ENERGY			! Mean photon energy
      REAL			R_BASE, R_SCALE		! Reponse axis attrs
      REAL			D_SCALE			! Dataset scale
      REAL			MAJOR			! Creator version id
      REAL			TOR			! Dataset radian factor

      INTEGER			DAX			! Dataset axis no.
      INTEGER		      	DIM			! Size of response axis
      INTEGER			FSTAT			! i/o status code
      INTEGER                 	IAX			! Loop over axes
      INTEGER                 	NELM 			! Number of elements mapped
      INTEGER			PSLOT			! First SLOT element
      INTEGER			REVISION		! Creator revision no.
      INTEGER			TLEN			! Prompt length
      INTEGER			X_AX, Y_AX, E_AX, T_AX	! Axis identifiers

      LOGICAL                 	IN_DATASET        	! Response found in dataset
      LOGICAL                 	INPRIM        		! USI input primitive?
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Extract psf number
      PSLOT = SLOT(1)

*    Does the dataset have an attached spatial response?
      CALL HDX_FIND( LOC, 'MORE.ASTERIX.SPATIAL_RESP', SLOC, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN
        CALL MSG_PRNT( 'Found spatial response in dataset...' )
        IN_DATASET = .TRUE.

      ELSE

*      Annul error
        CALL ERR_ANNUL( STATUS )

*      Get response name by prompting
        CALL USI_PROMT( 'MASK', 'Name of Asterix spatial response file',
     :                  STATUS )
        CALL USI_ASSOCI( 'MASK', 'READ', SLOC, INPRIM, STATUS )
        IF ( STATUS .NE. SAI__OK ) GOTO 99
        CALL USI_CANCL( 'MASK', STATUS )
        IN_DATASET = .FALSE.

      END IF

*    Get expanded response dimensions
      CALL CMP_GET1I( SLOC, 'DIMS', 5, RF_DIMS(1,PSLOT), RF_NDIM(PSLOT),
     :                STATUS )

*    Map the index
      CALL CMP_MAPV( SLOC, 'INDEX', '_INTEGER', 'READ',
     :               RF_IPTR(PSLOT), NELM, STATUS )

*    Map the data
      CALL CMP_MAPV( SLOC, 'DATA_ARRAY', '_REAL', 'READ',
     :               RF_DPTR(PSLOT), NELM, STATUS )

*    Is it compressed?
      CALL CMP_GET0L( SLOC, 'COMPRESSED', RF_PIXCENT(PSLOT), STATUS )

*    Is it pixel centred?
      CALL CMP_GET0L( SLOC, 'PIXCENT', RF_PIXCENT(PSLOT), STATUS )

*    Is it a radial response?
      CALL CMP_GET0L( SLOC, 'RADIAL', RF_RADIAL(PSLOT), STATUS )

*    Get creator version id
      CALL CMP_GET0C( SLOC, 'VERSION', VERSION, STATUS )

*    Extract version number
      READ( VERSION, '(15X,F3.1,1X,I1)', IOSTAT=FSTAT ) MAJOR, REVISION
      MAJOR = MAJOR + REVISION / 100.0

*    Axis attributes only written correctly from 1.7-3 onwards
      IF ( MAJOR .GE. 1.73 ) THEN

*      Identify spatial axes
        CALL PSF1_GETAXID( SLOT(2), X_AX, Y_AX, E_AX, T_AX, STATUS )

*      Get X and Y axis parameters, and compare with dataset
        DO IAX = 1, 2

*        Choose dataset axis
          IF ( IAX .EQ. 1 ) THEN
            DAX = X_AX
            DAXC = 'X'
          ELSE
            DAX = Y_AX
            DAXC = 'Y'
          END IF

*        Get response axis attributes
          CALL BDA_GETAXVAL( SLOC, IAX, R_BASE, R_SCALE, DIM, STATUS )

*        Get dataset axis attributes
          D_SCALE = PSF1_GETAXDR( SLOT(2), X_AX, STATUS )
          TOR = PSF1_GETAXTOR( SLOT(2), X_AX, STATUS )

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

*    End of switch on SPRESP version
      END IF

*    Get axis info
      IF ( RF_RADIAL(PSLOT) ) THEN
        DO IAX = 3, 4
          CALL BDA_GETAXVAL( SLOC, IAX, RF_BASE(IAX,PSLOT),
     :                       RF_SCALE(IAX,PSLOT), DIM, STATUS )
        END DO
      ELSE
        DO IAX = 3, 5
          CALL BDA_GETAXVAL( SLOC, IAX, RF_BASE(IAX,PSLOT),
     :                       RF_SCALE(IAX,PSLOT), DIM, STATUS )
        END DO
      END IF

*    If the PHA band is not defined, and the response has a significant
*    energy dimension, then we need a mean photon energy to index that
*    energy dimension.
      IF ( (RF_DIMS(RF_NDIM(PSLOT),PSLOT).GT.1) .AND.
     :                     .NOT. RF_PHA_DEF(PSLOT) ) THEN

*      Construct prompt
        CALL BDA_GETAXUNITS( SLOC, RF_NDIM(PSLOT), EUNITS, STATUS )
        CALL MSG_SETC( 'UNITS', EUNITS )
        CALL MSG_MAKE( 'Mean photon energy in ^UNITS', TEXT, TLEN )
        CALL USI_PROMT( 'AUX', TEXT(:TLEN), STATUS )

*      Get user respomse
        CALL USI_GET0R( 'AUX', ENERGY, STATUS )
        IF ( STATUS .NE. SAI__OK ) GOTO 99

*      Validate

*      Set PHA band
        RF_PHALO(PSLOT) = ENERGY
        RF_PHAHI(PSLOT) = ENERGY
        RF_PHA_DEF(PSLOT) = .TRUE.

      END IF

*    Annul response
      IF ( IN_DATASET ) THEN
        CALL DAT_ANNUL( SLOC, STATUS )
      ELSE
        CALL USI_ANNUL( SLOC, STATUS )
      END IF

*    Reset workspace
      RF_RESPTR(PSLOT) = 0

*    Abort point
 99   CONTINUE

      END

*+  PSF_SHARE_CLOSE - Close down Asterix in this image
      SUBROUTINE PSF_SHARE_CLOSE( )
*
*    Description :
*
*     Free any resources allocated to the PSFLIB image.
*
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     07 Nov 89 : Original (DJA)
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
*    Status :
*
      INTEGER                  STATUS                  ! Run-time error
*-

*    New error context
      CALL ERR_MARK( STATUS )

*    Shut down WFC system
      CALL PSF_WFC_CLOSE( STATUS )

*    Shutdown ASTERIX
      CALL DYN_CLOSE()
      CALL BDA_CLOSE()

*    Back to old error context
      CALL ERR_RLSE( STATUS )

      END

*+  PSF_SHARE_INIT - Start up Asterix in this image
      SUBROUTINE PSF_SHARE_INIT( NMOD, MODULES, STATUS )
*
*    Description :
*
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     07 Nov 89 : Original (DJA)
*     16 Dec 93 : Added RESPFILE psf (DJA)
*     12 Jan 94 : Added ASCA psf (DJA)
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
      INCLUDE 'PSF_WFC_CMN'
      INCLUDE 'PSF_ANAL_CMN'
      INCLUDE 'PSF_ASCA_CMN'
      INCLUDE 'PSF_RADIAL_CMN'
      INCLUDE 'PSF_RESPFILE_CMN'
      INCLUDE 'PSF_TABULAR_CMN'
      INCLUDE 'PSF_XRT_PSPC_CMN'
*
*    Status :
*
      INTEGER                  STATUS                  ! Run-time error
*
*    Export :
*
      INTEGER                  NMOD                    ! # modules
      CHARACTER*(*)            MODULES(*)              ! Module names
*
*    Local constants :
*
      INTEGER                  PSFLIB_NMOD             !
        PARAMETER              ( PSFLIB_NMOD = 10 )
*
*    Local variables :
*
      INTEGER                  I                       ! Loop over models
*
*    Local data :
*
      CHARACTER*20             MODS(PSFLIB_NMOD)
      DATA                     MODS/'ANAL',
     :                              'ASCA',
     :                              'EXOLE',
     :                              'PWFC',
     :                              'RADIAL',
     :                              'RESPFILE',
     :                              'TABULAR',
     :                              'WFC',
     :                              'XRT_HRI',
     :                              'XRT_PSPC'/
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Initialise BDA and DYN packages
      CALL BDA_INIT()
      CALL DYN_INIT()

*    Reset the SYSTEM_INIT flag in each common block
      AN_INIT = .TRUE.
      AS_INIT = .TRUE.
      TB_INIT = .TRUE.
      RF_INIT = .TRUE.
      RD_INIT = .TRUE.
      WF_INIT = .TRUE.
      WF_CALOK = .FALSE.
      RX_INIT = .TRUE.
      RX_CB_OPEN = .FALSE.

*    Clear channel bounds set flag in XRT_PSPC psfs
      DO I = 1, PSF_NMAX
        RF_PHA_DEF(I) = .FALSE.
        RX_PHA_DEF(I) = .FALSE.
        AS_PHA_DEF(I) = .FALSE.
      END DO

*    Return models available
      IF ( PSFLIB_NMOD .LE. PSF_NMAX ) THEN
        NMOD = PSFLIB_NMOD
        DO I = 1, PSFLIB_NMOD
          MODULES(I) = MODS(I)
        END DO
      END IF

*    Tidy up
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'PSF_SHARE_INIT', STATUS )
      END IF

      END

*+  PSF_TABULAR - 2D user supplied psf handler
      SUBROUTINE PSF_TABULAR( SLOT, X0, Y0, QX, QY, DX, DY, INTEG, NX,
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
      INCLUDE 'DAT_PAR'
      INCLUDE 'PSF_PAR'
      INCLUDE 'PSF_TABULAR_CMN'
*
*    Import :
*
      REAL                     DX, DY, X0, Y0,QX,QY
      INTEGER                  NX,NY,SLOT
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
      INTEGER                  TPTR                    ! Temp array for resample
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Call internal routine
      CALL PSF_TABULAR_INT( TB_DIMS(SLOT,1), TB_DIMS(SLOT,2),
     :                 %VAL(TB_DPTR(SLOT)), NX, NY, ARRAY, STATUS )

*    Non-zero offset?
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
      SUBROUTINE PSF_TABULAR_INIT( SLOT, LOC, STATUS )
*
*    Description :
*
*     Gets a user defined PSF and associates it with a PSF slot.
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
      INCLUDE 'DAT_PAR'
      INCLUDE 'PSF_PAR'
      INCLUDE 'PSF_TABULAR_CMN'
*
*    Import :
*
      INTEGER                 SLOT              ! PSF handle
      CHARACTER*(DAT__SZLOC)  LOC
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
      CHARACTER*(DAT__SZLOC)  PLOC              ! PSF structure locator
      CHARACTER*80            TNAME             ! Table file name
      CHARACTER               TLOC*(DAT__SZLOC) ! Table file ptr
      CHARACTER*40            UNITS             ! Spatial units

      REAL                    BASE, SCALE       ! Axis quantities

      INTEGER                 BDA               ! BDA identifier
      INTEGER                 DIMS(DAT__MXDIM)  ! Size of data array
      INTEGER                 DPTR              ! Ptr to data
      INTEGER                 NDIM              ! Dimensionality

      LOGICAL                 ELEVS_OK          ! Energy levels structure ok?
      LOGICAL                 PSF_OK            ! PSF structure there?
      LOGICAL                 RADII_OK          ! Energy radii structure ok?
      LOGICAL                 VALID             ! Have we a valid dataset?
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Get user's choice of table
      CALL USI_PROMT( 'MASK', 'Name of a 2D dataset containing psf',
     :                                                      STATUS )
      CALL USI_GET0C( 'MASK', TNAME, STATUS )
      CALL USI_CANCL( 'MASK', STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Try to open file
      CALL HDS_OPEN( TNAME(:CHR_LEN(TNAME)), 'READ', TLOC, STATUS )

*    Check the data
      CALL BDA_FIND( TLOC, BDA, STATUS )
      CALL BDA_CHKDATA_INT( BDA, VALID, NDIM, DIMS, STATUS )
      IF ( VALID ) THEN

        IF ( NDIM .NE. 2 ) THEN
          CALL MSG_PRNT( 'This isn''t a 2D dataset!' )
          STATUS = SAI__ERROR
        END IF

*      Map if ok
        CALL BDA_MAPDATA_INT( BDA, 'READ', DPTR, STATUS )

*      Get axis units
        CALL BDA_GETAXUNITS_INT( BDA, 1, UNITS, STATUS )
        CALL CONV_UNIT2R( UNITS, TB_TOR(SLOT), STATUS )
        IF ( STATUS .NE. SAI__OK ) THEN
          CALL ERR_ANNUL( STATUS )
          TB_TOR(SLOT) = 1.0
        END IF

*      Get axis values
        CALL BDA_GETAXVAL_INT( BDA, 1, BASE, SCALE, DIMS(1), STATUS )
        TB_TOR(SLOT) = ABS(TB_TOR(SLOT)*SCALE)

*      If ok then make a copy of this data and store
        IF ( STATUS .EQ. SAI__OK ) THEN
          TB_LOC(SLOT) = TLOC
          TB_DIMS(SLOT,1) = DIMS(1)
          TB_DIMS(SLOT,2) = DIMS(2)
          CALL DYN_MAPR( 2, DIMS, TB_DPTR(SLOT), STATUS )
          CALL ARR_COP1R( DIMS(1)*DIMS(2), %VAL(DPTR),
     :                   %VAL(TB_DPTR(SLOT)), STATUS )
        END IF

*      Inform user
        IF ( STATUS .EQ. SAI__OK ) THEN
          CALL MSG_SETC( 'FILE', TNAME )
          CALL MSG_PRNT( 'PSF read in from file ^FILE' )
        END IF

      END IF

*    Look for energy radii
      IF ( STATUS .EQ. SAI__OK ) THEN
        CALL BDA_CHKPSF_INT( BDA, PSF_OK, STATUS )
        IF ( PSF_OK ) THEN
          CALL BDA_LOCPSF_INT( BDA, PLOC, STATUS )
          CALL DAT_THERE( PLOC, 'ELEVS', ELEVS_OK, STATUS )
          CALL DAT_THERE( PLOC, 'RADII', RADII_OK, STATUS )
          IF ( ELEVS_OK .AND. RADII_OK ) THEN
            CALL CMP_GET1R( PLOC, 'ELEVS', 10, TB_ELEVS(SLOT,1),
     :                                   TB_NLEV(SLOT), STATUS )
            CALL CMP_GET1R( PLOC, 'RADII', 10, TB_RADII(SLOT,1),
     :                                   TB_NLEV(SLOT), STATUS )
          END IF
        ELSE
          TB_NLEV(SLOT) = 0
        END IF
      END IF

*    Release from BDA
      IF ( ( STATUS .EQ. SAI__OK ) .AND. VALID ) THEN
        CALL BDA_RELEASE_INT( BDA, STATUS )
        CALL HDS_CLOSE( TLOC, STATUS )
      END IF

*    Tidy up
 99   IF ( STATUS .NE. SAI__OK ) THEN
        CALL ERR_REP( ' ', '...from PSF_TABULAR_INIT', STATUS )
      END IF

      END

*+  PSF_TABULAR_PFL - Tabular energy profiling
      SUBROUTINE PSF_TABULAR_PFL( SLOT, NFRAC, FRAC, RADII, STATUS )
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
      INCLUDE 'DAT_PAR'
      INCLUDE 'PSF_PAR'
      INCLUDE 'PSF_TABULAR_CMN'
*
*    Import :
*
      INTEGER                  SLOT,NFRAC
      REAL                     FRAC(*)
*
*    Export :
*
      REAL                     RADII(*)
*
*    Status :
*
      INTEGER                  STATUS                  ! Run-time error
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Call internal routine
      CALL PSF_TABULAR_PFL_INT( SLOT, TB_DIMS(SLOT,1), TB_DIMS(SLOT,2),
     :                          %VAL(TB_DPTR(SLOT)), NFRAC, FRAC,
     :                          RADII, STATUS )

      END



*+  PSF_TABULAR_PFL_INT -
      SUBROUTINE PSF_TABULAR_PFL_INT( SLOT, NX, NY, DATA, NFRAC, FRAC,
     :                                                 RADII, STATUS )
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
      INCLUDE 'DAT_PAR'
      INCLUDE 'PSF_PAR'
      INCLUDE 'PSF_TABULAR_CMN'
*
*    Import :
*
      INTEGER                  SLOT,NFRAC
      REAL                     FRAC(*)
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
        RADII(I) = RADII(I) * TB_TOR(SLOT)

      END DO

      END

*+  PSF_WFC - Interrogate the calibration database for survey psf
      SUBROUTINE PSF_WFC( SLOT, X0, Y0, QX, QY, DX, DY, INTEG, NX, NY,
     :                                                 ARRAY, STATUS )
*
*    Description :
*
*     Returns array of values for a PSF centred at (X0,Y0) where the central
*     bin is at a position (QX,QY) from the PSF centre. There are NX by NY
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
*     10 Jul 89 : Original (DJA)
*     23 Apr 90 : Supplies correct energy derived from filter ID (DJA)
*     23 May 90 : Uses new CAL system (DJA)
*      2 Feb 93 : Sign of QX corrected (DJA)
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
      INCLUDE 'PSF_WFC_CMN'
*
*    Import :
*
      INTEGER                  SLOT
      REAL                     DX, DY, X0, Y0
      REAL                     QX,QY
      INTEGER                  NX,NY
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
      REAL                     LDX, LQX                ! Local copies for CAL
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    CAL doesn't handle coordinates as you'd expect!
      LDX = ABS(DX)
      LQX = ABS(QX)

      IF ( INTEG ) THEN
        CALL CAL_PSFT2D_SUR( WF_MJD(SLOT), WF_FID(SLOT),WF_ENERGY(SLOT),
     :                       .FALSE., 0.0, WF_IRIS(SLOT), LDX, NX,
     :                     ABS(DY), NY, QX*(DX/LDX), QY,ARRAY,STATUS )
      ELSE
        CALL CAL_PSFT2D_SUR( WF_MJD(SLOT), WF_FID(SLOT),WF_ENERGY(SLOT),
     :                       .FALSE., 0.0, WF_IRIS(SLOT), LDX, NX,
     :                     ABS(DY), NY, QX*(DX/LDX),QY,ARRAY,STATUS )
      END IF

      END

*+  PSF_WFC_CLOSE - Shutdown the WFC psf system
      SUBROUTINE PSF_WFC_CLOSE( STATUS )
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
*     10 Jul 89 : Original ( DJA )
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
*    Status :
*
      INTEGER                  STATUS                  ! Run-time error
*-

*    Just call the WFC routine
      CALL PSF_PWFC_CLOSE( STATUS )

      END

*+  PSF_WFC_INIT - Initialise the WFC survey psf system
      SUBROUTINE PSF_WFC_INIT( SLOT, LOC, STATUS )
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
*     10 Jul 89 : Original ( DJA )
*     23 Apr 90 : Checks detector and filter info from dataset (DJA)
*     19 May 90 : Bit more robust if no sort data present (DJA)
*     23 May 90 : Changed over to new CAL system. This routine now controls
*                 the survey psf. See PWFC for pointed phase psf. (DJA)
*     26 Jun 90 : Asks user for filter if none found (DJA)
*     28 Jun 90 : Issues warning if filter selected is not a survey
*                 filter (DJA)
*      7 Jul 90 : Suppress warning if no BASE_MJD (DJA)
*     17 Jul 90 : Filter translation table added (DJA)
*     19 Jul 90 : Filter translation using WFC_FILT_CONV (DJA)
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
      INCLUDE 'PSF_WFC_CMN'
*
*    Status :
*
      INTEGER                  STATUS                  ! Run-time error
*
*    Import :
*
      INTEGER                  SLOT                    ! PSF slot id
      CHARACTER*(DAT__SZLOC)   LOC                     ! Dataset locator
*
*    Functions :
*
      INTEGER                  CAL_FILT_S2N
      INTEGER                  CHR_LEN
*
*    Local variables :
*
      CHARACTER*(DAT__SZLOC)   HLOC                    ! HEADER locator
      CHARACTER*(DAT__SZLOC)   ILOC                    ! INSTRUMENT locator
      CHARACTER*(DAT__SZLOC)   SLOC                    ! SORT locator
      CHARACTER*80             CID                     ! Filter description

      REAL                     IRIS                    ! Sort iris value

      INTEGER                  BDA                     ! BDA identifier
      INTEGER                  CALFN                   ! CAL filter no.
      INTEGER                  IFILT                   ! Dataset filter code

      LOGICAL                  FILTER_OK, IRIS_OK      !
      LOGICAL                  THERE
      LOGICAL                  I_THERE, S_THERE, B_THERE
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    System already initialised?
      IF ( .NOT. WF_CALOK ) THEN
         CALL CAL_INIT( STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL MSG_PRNT( 'Unable to open WFC calibration database.' )
            CALL MSG_PRNT( 'Check assignment of CAL_WFC_MASTER '/
     :                                         /'logical name.' )
         ELSE
            WF_CALOK = .TRUE.
         END IF

      END IF

*    Set the SURVEY flag
      WF_SURVEY(SLOT) = .TRUE.
      IRIS_OK = .FALSE.
      FILTER_OK = .FALSE.

*    Is locator valid
      CALL DAT_VALID( LOC, WF_DATASET(SLOT), STATUS )
      IF ( WF_DATASET(SLOT) ) THEN

*       Get BDA identifier
         CALL BDA_FIND( LOC, BDA, STATUS )

*       Store locator
         WF_LOC(SLOT) = LOC
         WF_MCP(SLOT) = 2

*       Try and get MJD from dataset
         CALL BDA_CHKHEAD_INT( BDA, THERE, STATUS )
         B_THERE = .FALSE.
         IF ( THERE ) THEN
            CALL BDA_LOCHEAD_INT( BDA, HLOC, STATUS )
            CALL DAT_THERE(HLOC,'BASE_MJD',B_THERE,STATUS )
            IF ( B_THERE ) THEN
               CALL CMP_GET0D( HLOC, 'BASE_MJD', WF_MJD(SLOT), STATUS )
               IF ( STATUS .NE. SAI__OK ) THEN
                  B_THERE = .FALSE.
                  CALL ERR_ANNUL(STATUS)
               END IF
            END IF
         END IF
         IF ( .NOT. B_THERE ) WF_MJD(SLOT) = 48000.0D0

*       Get the detector id from CAL
         CALL CIN_SET_DET( WF_MJD(SLOT), WF_MCP(SLOT), STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL MSG_PRNT( 'No detector id present - '/
     :            /'assuming detector 2 for psf access' )
            STATUS = SAI__OK
            WF_MCP(SLOT) = 2
         END IF

*       Locate components in dataset
         CALL BDA_CHKINSTR_INT( BDA, I_THERE, STATUS )
         IF ( I_THERE ) THEN
            CALL BDA_LOCINSTR_INT( BDA, ILOC, STATUS )
            CALL DAT_THERE( ILOC, 'SORT', S_THERE, STATUS )
         END IF
         IF ( I_THERE .AND. S_THERE ) THEN
            CALL DAT_FIND( ILOC, 'SORT', SLOC, STATUS )

*          Look for filter id
            CALL CMP_GET0I( SLOC, 'FILTER', IFILT, STATUS )
            IF ( ( IFILT .LT. 1 ) .OR. ( IFILT .GT. 8 ) .OR.
     :                              ( STATUS .NE. SAI__OK) ) THEN
               CALL ERR_ANNUL(STATUS)
               CALL MSG_SETI( 'N', IFILT )
               CALL MSG_PRNT( 'Invalid filter id code ^N' )
            ELSE
               CALL WFC_FILT_CONV( IFILT, CALFN, STATUS )
               FILTER_OK = .TRUE.
            END IF

*          Get iris value
            CALL DAT_THERE( SLOC, 'IRIS', IRIS_OK, STATUS )
            IF ( IRIS_OK ) THEN
               CALL CMP_GET0R( SLOC, 'IRIS', IRIS, STATUS )
               IF ( STATUS .NE. SAI__OK ) THEN
                  CALL ERR_ANNUL(STATUS)
                  IRIS_OK = .FALSE.
               ELSE IF ( IRIS .LT. 0.001 ) THEN
                  CALL MSG_PRNT( 'WARNING : bad IRIS value,'/
     :                                    /' check dataset' )
                  IRIS_OK = .FALSE.
               END IF
            END IF

*          Tidy up
            CALL DAT_ANNUL( SLOC, STATUS )

         END IF

*       Get filter from user if none supplied
         CALL USI_PROMT( 'AUX', 'Enter filter id (S1A/B,S2A/B)',STATUS )
         DO WHILE ( .NOT. FILTER_OK )
 20         CALL USI_GET0C( 'AUX', CID, STATUS )
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
         WF_FID(SLOT) = CALFN

*       Use filter to get stuff - inform user and store energy
         CALL CAL_FILT_INFO( WF_FID(SLOT), CID,
     :                       WF_ENERGY(SLOT), STATUS )
         CALL MSG_SETC( 'ID', CID )
         CALL MSG_PRNT( '   Filter ^ID')

         IF ( .NOT. IRIS_OK ) THEN
            IRIS = 2.5
            CALL MSG_PRNT( 'Unable to get IRIS value from dataset'/
     :                            /' - defaulting to 2.5 degrees' )
         END IF

*       Convert IRIS value to radians
         WF_IRIS(SLOT) = IRIS * MATH__DTOR

 69      CONTINUE

      END IF

      END

*+  PSF_XRT_HRI - ROSAT XRT HRI PSF
      SUBROUTINE PSF_XRT_HRI( SLOT, X0, Y0, QX, QY, DX, DY, INTEG, NX,
     :                                              NY, ARRAY, STATUS )
*
*    Description :
*
*     This model of the XRT HRI detector spatial response was constructed
*     by MPE/GSFC/SAO. The function form of the radial surface brightness
*     is
*
*       psf = a1*exp(-0.5*(r/s1)**2) + a2*exp(-0.5*(r/s2)**2) + a3*exp(-r/s3)
*
*     This is good fit out to 100 arcsec from the centre of the psf.
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
*     11 Mar 91 : Original (DJA)
*      3 Sep 92 : Updated response to 3-component fit (DJA)
*      1 Mar 94 : Use MATH_EXPR rather than D.P. intrinsic (DJA)
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
*
*    Import :
*
      REAL                     DX, DY, X0, Y0,QX,QY
      INTEGER                  NX,NY,SLOT
      LOGICAL                  INTEG
*
*    Export :
*
      REAL                     ARRAY(NX,NY)
*
*    Status :
*
      INTEGER                  STATUS                  ! Run-time error
*
*    Functions :
*
      REAL 			MATH_EXPR
*
*    Local constants :
*
      REAL                     A1, A2, A3              ! Relative contributions
        PARAMETER              ( A1 = 0.9638,          ! of 3 components
     :                           A2 = 0.1798,
     :                           A3 = 0.001168 )
      REAL                     S1, S2, S3              ! Radial scale of comps
        PARAMETER              ( S1 = 2.1858,          ! in arcsec
     :                           S2 = 4.0419,
     :	                         S3 = 31.69 )
      REAL                     NORM                    ! Normalisation
        PARAMETER              ( NORM = 54.848581 )
      REAL                     RTOS                    ! Radian to arcsec
        PARAMETER              ( RTOS = MATH__RTOD*3600.0 )
*
*    Local variables :
*
      REAL                     LNORM                   ! Normalisation constant
      REAL                     P_SCALE                 ! Scale size of psf
      REAL                     RPS                     ! Radius of sub-pix ^ 2
      REAL                     S1_2, S2_2              !
      REAL                     SDX, SDY                ! Sub-pixel bin sizes
      REAL                     SUM                     ! Cumulative value
      REAL                     XP0, YP0                ! Major pixel centre
      REAL                     XPS, YPS                ! Sub-pixel pixel centre
      REAL                     YPS2                    ! Sub-pixel distance

      INTEGER                  I, J                    ! Major pixel loops
      INTEGER                  II, JJ                  ! Sub-pixel loops
      INTEGER                  MNX, MNY                ! Local calc bounds
      INTEGER                  XSUB, YSUB              ! Sub-pixel factors

      LOGICAL                  SYMMETRIC               ! Symmetric about centre?
*
*    Inline functions :
*
      REAL                     DEL,PIX
      INTEGER                  SPIX
      DOUBLE PRECISION         HFUNC
      SPIX(DEL,PIX) = MAX(1,NINT(abs(10.0*PIX)/P_SCALE/MAX(1.0,
     :                                SQRT(ABS(DEL/P_SCALE)))))
      HFUNC(DEL) = A1*MATH_EXPR(DEL*S1_2)+
     :             A2*MATH_EXPR(DEL*S2_2)+
     :             A3*MATH_EXPR(-SQRT(DEL)/S3)
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    A few variables to speed things up
      S1_2 = -0.5 / S1**2
      S2_2 = -0.5 / S2**2
      P_SCALE = SQRT(S1**2+S2**2)/RTOS

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

*    For each point requiring calculation
      DO J = 1, MNY

*      Find Y sub-pixelling
        YSUB = SPIX( YP0 + DY*REAL(J-1), DY )
        SDY = DY / YSUB

        DO I = 1, MNX

*        Zero
          SUM = 0.0

*        Find X sub-pixelling
          XSUB = SPIX( XP0 + DX*REAL(I-1), DX )
          SDX = DX / XSUB

*        Correct normalisation for sub-pixel and pixel size
          LNORM = ABS(SDX*SDY*RTOS*RTOS)/NORM

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
              SUM = SUM + HFUNC( RPS*RTOS*RTOS )

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

*+  PSF_XRT_PSPC - ROSAT XRT PSPC PSF
      SUBROUTINE PSF_XRT_PSPC( SLOT, X0, Y0, QX, QY, DX, DY, INTEG, NX,
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
      INCLUDE 'DAT_PAR'
      INCLUDE 'PRM_PAR'
      INCLUDE 'PSF_PAR'
      INCLUDE 'MATH_PAR'
      INCLUDE 'PSF_XRT_PSPC_CMN'
*
*    Import :
*
      REAL                      DX, DY, X0, Y0,QX,QY
      INTEGER                   NX,NY,SLOT
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
      INTEGER                  OPT                     ! Psf option #
      INTEGER                  XSUB, YSUB              ! Sub-pixel factors

      LOGICAL                  SYMMETRIC               ! Symmetric about centre?
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

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Defined energy band?
      IF ( RX_PHA_DEF(SLOT) ) THEN
        ENERGY = REAL(RX_PHALO(SLOT))/100.0
      ELSE
        ENERGY = RX_ENERGY(SLOT)
      END IF

*    Get PSPC option and energy
      OPT = RX_OPTION(SLOT)

*    The variable gaussian option
      IF ( OPT .EQ. PSF_PSPC__VARG ) THEN

*      Get off-axis angle in arcminutes
        ROFF = SQRT( X0**2 + Y0**2 ) * MATH__RTOD * 60.0

*      Get gaussian FWHM in arcseconds
        CALL XRT_FWHM( ROFF, ENERGY, FWHM )

*      Convolve
        FWHM = SQRT( FWHM**2 )

*      Convert FWHM to sigmas in radians
        SIG = ( (FWHM/3600.0) * MATH__DTOR ) / FW2SIG

*      Evaluate gaussian
        CALL MATH_INTGAU2D( SIG, SIG, 0.0, 0.0, 0.0, QX, QY,
     :                       DX, DY, NX, NY, ARRAY, STATUS )

      ELSE

*      A few variables to speed things up
        IF ( OPT .EQ. PSF_PSPC__ONAX3 ) THEN

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
        ELSE IF ( OPT .EQ. PSF_PSPC__ONAX2 ) THEN
          O2_S1_2 = -0.5 / O2_S1**2
          P_SCALE = O2_S1/RTOS

*      Cube handling
        ELSE IF ( OPT .EQ. PSF_PSPC__VARP ) THEN

*        Select energy bin
          EBIN = PSF_XRT_PSPC_EBIN( RX_CB_NEBIN,
     :                              %VAL(RX_CB_EAPTR),
     :                              (RX_CB_EWPTR.NE.0),
     :                              %VAL(RX_CB_EWPTR),
     :                              ENERGY )

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
            IF ( OPT .EQ. PSF_PSPC__VARP ) THEN
              LNORM = ABS(SDX*SDY*RTOM*RTOM)
            ELSE IF ( OPT .EQ. PSF_PSPC__ONAX3 ) THEN
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
              IF ( OPT .EQ. PSF_PSPC__VARP ) THEN

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

              ELSE IF ( OPT .EQ. PSF_PSPC__ONAX3 ) THEN

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

*+  PSF_XRT_PSPC_DEF - ROSAT XRT PSPC PSF time/energy definition
      SUBROUTINE PSF_XRT_PSPC_DEF( SLOT, TLO, THI, ELO, EHI, UIN, UOUT,
     :                                                         STATUS )
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
*     28 Oct 92 : Original (DJA)
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
      INCLUDE 'MATH_PAR'
      INCLUDE 'PSF_XRT_PSPC_CMN'
*
*    Import :
*
      INTEGER                  SLOT                    ! Psf slot id
      DOUBLE PRECISION         TLO, THI                ! Time bounds
      INTEGER                  ELO, EHI                ! Energy channel bounds
      BYTE                     UIN(*), UOUT(*)         ! User in/out
*
*    Status :
*
      INTEGER                  STATUS                  ! Run-time error
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Define only energy bounds
      RX_PHA_DEF(SLOT) = .TRUE.
      RX_PHALO(SLOT) = ELO
      RX_PHAHI(SLOT) = EHI

      END

*+  PSF_XRT_PSPC_HINT - XRT PSPC psf hint handler
      SUBROUTINE PSF_XRT_PSPC_HINT( SLOT, HINT, DATA, STATUS )
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
      INCLUDE 'DAT_PAR'
      INCLUDE 'PSF_PAR'
*
*    Global variables :
*
      INCLUDE 'PSF_XRT_PSPC_CMN'
*
*    Import :
*
      INTEGER                 	SLOT            	! PSF handle
      CHARACTER*(*)           	HINT		 	! Hint name
*
*    Export :
*
      BYTE			DATA(*)			! Hint data
*
*    Status :
*
      INTEGER                   STATUS
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Radial symmetry?
      IF ( HINT .EQ. PSF_H_RADSYM ) THEN

*      All our models are radially symmetric about on-axis direction
        CALL ARR_COP1L( 1, .TRUE., DATA, STATUS )

*    Position dependent
      ELSE IF ( HINT .EQ. PSF_H_ENDEP ) THEN

*      The ONAXIS psf isn't position dependent
        CALL ARR_COP1L( 1, (RX_OPTION(SLOT).NE.PSF_PSPC__ONAX3),
     :                                            DATA, STATUS )

*    Energy dependent
      ELSE IF ( HINT .EQ. PSF_H_ENDEP ) THEN

*      They all vary with energy
        CALL ARR_COP1L( 1, .TRUE., DATA, STATUS )

      ELSE
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'Unknown psf hint /'//HINT//'/', STATUS )

      END IF

      END

*+  PSF_XRT_PSPC_INIT - XRT PSPC psf initialisation
      SUBROUTINE PSF_XRT_PSPC_INIT( PSLOT, LOC, STATUS )
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
      INTEGER                 PSLOT(2)           ! PSF handle
      CHARACTER*(DAT__SZLOC)  LOC
*
*    Status :
*
      INTEGER STATUS
*
*    Function declarations :
*
      LOGICAL                 STR_ABBREV
      LOGICAL                 CHR_SIMLR
*
*    Local variables :
*
      CHARACTER*132           FNAME             ! File name of cube
      CHARACTER*20            MASK              ! Mask name

      INTEGER                 CDIMS(3)          ! XRT pf cube dimensions
      INTEGER                 SLOT              !

      LOGICAL                 OK, UNIF          !
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Set up as user defined
      SLOT = PSLOT(1)

*    Get mask name
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

        RX_OPTION(SLOT) = PSF_PSPC__VARG

      ELSE IF ( STR_ABBREV(MASK,'VARPROFILE') ) THEN

*      Set option
        RX_OPTION(SLOT) = PSF_PSPC__VARP

*      Open the cube
        IF ( .NOT. RX_CB_OPEN ) THEN

*        Translate environment symbol
          CALL PSX_GETENV( 'AST_XRT_PSF_CUBE', FNAME, STATUS )

*        Open the file
          CALL HDS_OPEN( FNAME, 'READ', RX_CB_LOC, STATUS )

*        Abort if failed
          IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERR_REP( ' ', 'Unable to open XRT psf cube', STATUS )
            GOTO 99
          END IF

*        Map the 2 components
          CALL CMP_MAPN( RX_CB_LOC, 'DATA_ARRAY', '_REAL', 'READ',
     :                   3, RX_CB_PPTR, CDIMS, STATUS )
          RX_CB_NRBIN = CDIMS(1)
          RX_CB_NOBIN = CDIMS(2)
          RX_CB_NEBIN = CDIMS(3)
          CALL CMP_MAPN( RX_CB_LOC, 'RBINSIZE', '_REAL', 'READ',
     :                   2, RX_CB_RPTR, CDIMS, STATUS )

*        Get off-axis angle axis attributes
          CALL BDA_GETAXVAL( RX_CB_LOC, 2, RX_CB_OBASE,
     :                       RX_CB_OSCALE, CDIMS(2), STATUS )

*        Map energy axis data and widths if present
          CALL BDA_MAPAXVAL( RX_CB_LOC, 'READ', 3, RX_CB_EAPTR,STATUS )
          CALL BDA_CHKAXWID( RX_CB_LOC, 3, OK, UNIF, CDIMS(3), STATUS )
          IF ( OK ) THEN
            CALL BDA_MAPAXWID( RX_CB_LOC, 'READ', 3,
     :                         RX_CB_EWPTR, STATUS )
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

        RX_OPTION(SLOT) = PSF_PSPC__ONAX3

      ELSE IF ( STR_ABBREV(MASK,'ONAXIS_2') ) THEN

        RX_OPTION(SLOT) = PSF_PSPC__ONAX2

      ELSE
        STATUS = SAI__ERROR
        CALL MSG_SETC( 'MASK', MASK )
        CALL ERR_REP( ' ', 'Invalid PSPC psf option /^MASK/', STATUS )

      END IF

*    Get a mean photon energy
      IF ( (RX_OPTION(SLOT) .NE. PSF_PSPC__ONAX2)
     :      .AND. .NOT. RX_PHA_DEF(SLOT) ) THEN
        CALL USI_PROMT( 'AUX', 'Mean photon energy in KeV', STATUS )
        CALL USI_GET0R( 'AUX', RX_ENERGY(SLOT), STATUS )
      END IF

*    Tidy up
 99   IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'PSF_XRT_PSPC_INIT', STATUS )
      END IF

      END
