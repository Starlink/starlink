*+  PSF_ENERGY_PFL - Return radii at which energy level is enclosed
      SUBROUTINE PSF_ENERGY_PFL( PSID, NFRAC, FRAC, X0, Y0,
     :                                      RADII, STATUS )
*
*    Description :
*
*     Returns the radii in radians at which the specified fractions of the
*     integrated psf are enclosed, at image position (X0,Y0).
*
*    Method :
*
*     Evaluates the PSF in a strip in the X direction from (X0,Y0) of
*     the field.
*
*    Deficiencies :
*
*     Assumes radial symmetry
*
*    Authors :
*
*     David J. Allan (ROSAT, University of Birmingham)
*
*    History :
*
*     29 Apr 1990 (DJA):
*        Original version
*      9 Aug 1990 (DJA):
*        New iterative technique to counter coarse binning problems
*     27 Oct 1990 (DJA):
*        Tries to invoke special library routine first
*     18 Mar 1991 (DJA):
*        Varies over field
*     15 Dec 1993 (DJA):
*        Use internal routine for axis info
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
      INTEGER                  PSID                    ! The PSF id
      INTEGER                  NFRAC                   ! Number of fractions
      REAL                     FRAC(NFRAC)             ! Requested fractions
      REAL                     X0, Y0                  ! Image position
*
*    Export :
*
      REAL                     RADII(NFRAC)            ! Energy enclosed radii
*
*    Status :
*
      INTEGER                  STATUS
*
*    Functions :
*
      REAL                     PSF0_GETAXDR
*
*    Local constants :
*
      INTEGER                  MAXPR                   ! PSF trial box half wid
         PARAMETER             ( MAXPR = 100 )
      REAL                     MAXRAD                  ! max radius in degrees
         PARAMETER             ( MAXRAD = 0.5 )
*
*    Local variables :
*
      REAL                     CDX                     ! Coarse bin size
      REAL                     FDX                     ! Fine bin size
      REAL                     IR50                    ! Initial guess at 50%
      REAL                     COARSE(0:MAXPR)         ! Trial 1D PSF array
      REAL                     FINE(0:MAXPR)           ! Trial 1D PSF array
      REAL                     OLDCVAL                 !

      REAL                     TOT                     ! Integrated psf
      REAL                     XDIR                    ! Sign of axis increase
      REAL                     ZOOM                    ! Coarse to fine zoom

      INTEGER                  I, ILEV
      INTEGER                  NCBIN                   !
      INTEGER                  NDONE                   !
      INTEGER			RTNPTR			! Specialist routine
      INTEGER                  X_AX, Y_AX, E_AX, T_AX  !

      LOGICAL			THERE			! Specialist exists?
*-

*  Check inherited global status
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Invoke specialist routine if it exists
      CALL PSF0_FNDRTN( PSID, 'Eprofile', THERE, RTNPTR, STATUS )
      IF ( THERE ) THEN
        CALL PSF_ENERGY_PFL_SPEC( %VAL(RTNPTR), PSID, X0, Y0,
     :                            NFRAC, FRAC, RADII, STATUS )
        IF ( STATUS .EQ. SAI__OK ) THEN
          GOTO 99
        ELSE IF ( STATUS .NE. SAI__OK ) THEN
          CALL ERR_ANNUL( STATUS )
        END IF
      END IF

*  Axis identifiers
      CALL PSF_QAXES( PSID, X_AX, Y_AX, E_AX, T_AX, STATUS )

*  Direction of increase of X axis
      IF ( PSF1_GETAXDR( PSID, X_AX, STATUS ) .GT. 0.0 ) THEN
        XDIR = 1.0
      ELSE
        XDIR = -1.0
      END IF

*    STAGE 1
      CDX = MAXRAD * MATH__DTOR / REAL( MAXPR ) * XDIR
      CALL PSF_ENERGY_PFL_INT( PSID, X0, Y0, CDX, MAXPR, COARSE,
     :                                             TOT, STATUS )
      CDX = ABS(CDX)

*    Initial guess at 50% radius
      CALL PSF_ENERGY_PFL_ARAD( MAXPR, COARSE, TOT/2.0, IR50, STATUS )

*    Re-run to define psf in centre. Go out to twice the estimated 50% radius
*    Ensure that the fine bin spacing sub-divides the coarse binning exactly
      FDX = 2.0 * IR50 * CDX / REAL( MAXPR )
      ZOOM = CDX / FDX
      ZOOM = REAL( INT(ZOOM) + 1 )
      FDX = XDIR * CDX / ZOOM
      CALL PSF_ENERGY_PFL_INT( PSID, X0, Y0, FDX, MAXPR, FINE, TOT,
     :                                                     STATUS )

*    Try to do those radii inside the fine strip
      ILEV = 1
      NDONE = 0
      DO WHILE ( (ILEV.LE.NFRAC) .AND. (FRAC(ILEV).LT.FINE(MAXPR)) )

*      Interpolate to get radius
        CALL PSF_ENERGY_PFL_ARAD( MAXPR, FINE, TOT*FRAC(ILEV),
     :                                   RADII(ILEV), STATUS )

*      Convert radius to binning selected
        RADII(ILEV) = RADII(ILEV) * ABS(FDX)

*      Set as done
        NDONE = NDONE + 1
        ILEV = ILEV + 1

      END DO

*    More radii to do?
      IF ( NDONE .LT. NFRAC ) THEN

*      Correct the coarse array for excess due to poor accumulation in centre.
*      Find difference between value in coarse array and fine array, at the
*      maximum extent of the latter which includes an entire bin of the former
        NCBIN = INT( MAXPR*ABS(FDX) / CDX )
        OLDCVAL = COARSE( NCBIN - 1 )
        DO I = 0, NCBIN - 1
          COARSE(I) = FINE( (I+1)*INT(ZOOM) - 1 )
        END DO

*      Subtract difference from rest of coarse values to do normalisation
        DO I = NCBIN, MAXPR
          COARSE(I) = COARSE(I) - ( OLDCVAL - COARSE(NCBIN-1) )
        END DO

*      Perform interpolation for undone radii
        DO ILEV = NDONE + 1, NFRAC
          CALL PSF_ENERGY_PFL_ARAD( MAXPR, COARSE, FRAC(ILEV),
     :                                   RADII(ILEV), STATUS )
          RADII(ILEV) = RADII(ILEV) * CDX
        END DO

      END IF

*    Tidy up
  99  IF ( STATUS .NE. SAI__OK ) THEN
         CALL AST_REXIT( 'PSF_ENERGY_PFL', STATUS )
      END IF

      END



*+  PSF_ENERGY_PFL_SPEC - Invoke library routine for profiling
      SUBROUTINE PSF_ENERGY_PFL_SPEC( ROUTINE, PSID, X0, Y0, NFRAC,
     :                                        FRAC, RADII, STATUS )
*
*    Description :
*
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     27 Oct 90 : Original (DJA)
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
      EXTERNAL                  ROUTINE                 ! LIB routine to call
      REAL			X0, Y0			! Detector position
      INTEGER                  	PSID                    ! The PSF id
      INTEGER                   NFRAC                   ! Number of fractions
      REAL                      FRAC(NFRAC)             ! Requested fractions
*
*    Export :
*
      REAL                     RADII(NFRAC)            ! Energy enclosed radii
*
*    Status :
*
      INTEGER                  STATUS
*-

*  Check inherited global status
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Invoke routine
      CALL ROUTINE( PSID, X0, Y0, NFRAC, FRAC, RADII, STATUS )

      END



*+  PSF_ENERGY_PFL_INT - Internal psf access routine
      SUBROUTINE PSF_ENERGY_PFL_INT( PSID, X0, Y0, DX, NX, ARRAY,
     :                                              TOT, STATUS )
*    Description :
*    Method :
*    Deficiencies :
*    Bugs :
*
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*      9 Aug 90 : Original (DJA)
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
      INTEGER                  PSID           ! Psf handle
      REAL                     X0, Y0         ! Image position
      INTEGER                  NX             ! Size of psf data array
      REAL                     DX             ! Bin size to use
*
*    Export :
*
      REAL                     ARRAY(0:NX)    ! The energy function
      REAL                     TOT            ! The intergated value from centre
*
*    Status :
*
      INTEGER STATUS
*
*    Local constants :
*
      REAL                     YFAC           ! Y sub-division factor
         PARAMETER             ( YFAC = 10.0 )
*
*    Local variables :
*
      INTEGER                  I              ! Loop over psf values
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Get the psf data
 10   CALL PSF_2D_DATA( PSID, X0, Y0, (NX+1)*DX/2.0, 0.0, DX,
     :      ABS(DX/YFAC), .TRUE., NX+1, 1, ARRAY(0), STATUS )

*    Abort if bad status
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Correct psf data using annular weights
      TOT = 0.0
      DO I = 0, NX
        ARRAY(I) = ARRAY(I) * ( REAL(I+1)**2.0 - REAL(I)**2.0 ) *
     :                                              MATH__PI * YFAC
        TOT = TOT + ARRAY(I)
      END DO

*    Too large a pixel size is manifested by a TOT value greater than 1.0.
*    If this is the case, use a smaller pixel size
      IF ( TOT .GT. 1.05 ) THEN
        DX = DX / 2.0
        GOTO 10
      ELSE IF ( TOT .LT. 0.1 ) THEN
        DX = DX * 2.0
        GOTO 10
      END IF

*    Turn ARRAY array into enclosed energy array
      DO I = 1, NX
        ARRAY(I) = ARRAY(I) + ARRAY(I-1)
      END DO

 99   CONTINUE

      END



*+  PSF_ENERGY_PFL_ARAD - Get radius for a given energy
      SUBROUTINE PSF_ENERGY_PFL_ARAD( NX, ARRAY, ENERGY, RADIUS,
     :                                                  STATUS )
*    Description :
*    Method :
*    Deficiencies :
*    Bugs :
*
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*      9 Aug 90 : Original (DJA)
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
      INTEGER                  NX             ! Size of psf data array
      REAL                     ARRAY(0:NX)    ! The energy function
      REAL                     ENERGY         ! Interpolation energy
*
*    Export :
*
      REAL                     RADIUS         ! Interpolated value
*
*    Status :
*
      INTEGER STATUS
*
*    Local variables :
*
      INTEGER                  I              ! Loop over psf values
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Locate where profile exceeds requested fraction.
      I = 0
      DO WHILE ( (ARRAY(I) .LT. ENERGY) .AND. (I.LT.(NX-1)) )
        I = I + 1
      END DO

*    Interpolate
      IF ( I .EQ. 0 ) THEN
        RADIUS = SQRT( ENERGY/ARRAY(I) )
      ELSE
        RADIUS = REAL(I) + SQRT(
     :                 (ENERGY-ARRAY(I-1))/(ARRAY(I)-ARRAY(I-1)) )
      END IF

      END
