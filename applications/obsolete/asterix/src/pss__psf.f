*+  PSS_PSF_CLOSE - Terminates PSS use of PSF system
      SUBROUTINE PSS_PSF_CLOSE( STATUS )
*
*    Description :
*
*     All psf resources are released within a new error context to try
*     and guarantee a graceful exit, even in ICL, without residual file
*     locking prolems.
*
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     16 Jul 92 : Original (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'MATH_PAR'
      INCLUDE 'PSS_PAR'
*
*    Global variables :
*
      INCLUDE 'PSS_CMN'
*
*    Status :
*
      INTEGER                  STATUS               ! Run-time error
*-

*    New error context
      CALL ERR_BEGIN( STATUS )

*    Free memory for psf arrays
      IF ( PSF_DATA .NE. 0 ) CALL DYN_UNMAP( PSF_DATA, STATUS )
      IF ( PSF_STORE .NE. 0 ) CALL DYN_UNMAP( PSF_STORE, STATUS )

*    Free the psf handle
      CALL PSF_RELEASE( PSF_HAN, STATUS )

*    Restore external error context
      CALL ERR_END( STATUS )

*    Shutdown PSF system
      CALL PSF_CLOSE( STATUS )

      END
*+  PSS_PSF_CONVOLVE - Convolve psf with gaussian of given FWHM
      SUBROUTINE PSS_PSF_CONVOLVE( NX, NY, IN, GAU, FWHM, OUT, STATUS )
*
*    Description :
*
*    History :
*
*     12 Nov 90 : Original (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'MATH_PAR'
      INCLUDE 'PSS_PAR'
*
*    Global variables :
*
      INCLUDE 'PSS_CMN'
*
*    Import :
*
      INTEGER                  NX,NY                       ! Size of psf array
      REAL                     IN(-NX/2:NX/2,-NY/2:NY/2)   ! Instrument psf
      REAL                     FWHM                        ! Width in arcmin
*
*    Workspace :
*
      REAL                     GAU(-NX:NX,-NY:NY)          ! Blurring function
*
*    Export :
*
      REAL                     OUT(-NX/2:NX/2,-NY/2:NY/2)  ! Blurred psf
*
*    Status :
*
      INTEGER                  STATUS
*
*    Local variables :
*
      REAL                     GSIG                        ! Sigma in radians
      REAL                     LAST_FWHM                   ! FWHM store

      INTEGER                  I,J                         !
      INTEGER                  II,JJ                       !
      INTEGER                  WID                         ! Width of gaussian
                                                           ! to use
*
*    Preserve :
*
      SAVE                     LAST_FWHM
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Trap the obvious error
      IF ( FWHM .EQ. 0.0 ) THEN

        DO J = -NY/2, NY/2
          DO I = -NX/2, NX/2
            OUT(I,J) = IN(I,J)
          END DO
        END DO

*    Only do it if different from last time
      ELSE IF ( FWHM .NE. LAST_FWHM ) THEN

*      Find sigma from FWHM
        GSIG = (FWHM * MATH__DTOR / 60.0)/(2.0*SQRT(2.0*ALOG(2.0)) )

*      Fill gaussian array
        CALL MATH_INTGAU2D( GSIG, GSIG, 0.0, 0.0, 0.0, 0.0, 0.0,
     :       AX_DR(1), AX_DR(2), NX*2+1, NY*2+1, GAU, STATUS )

*      Initialise output
        DO J = -NY/2, NY/2
          DO I = -NX/2, NX/2
            OUT(I,J) = 0.0
          END DO
        END DO

*      Find where gaussian drops to negligiblity
        WID = 1
        DO WHILE ( (GAU(WID,0).GT.1.0E-15) .AND. (WID.LT.NX) )
          WID = WID + 1
        END DO

*      Perform convolution
        DO J = -NY/2, NY/2
          CALL PSS_SETRNG( 2, J, -NY/2, NY/2, WID )
          DO I = -NX/2, NX/2
            CALL PSS_SETRNG( 1, I, -NX/2, NX/2, WID )
            DO JJ = GR_RNG_LO(2), GR_RNG_HI(2)
              DO II = GR_RNG_LO(1), GR_RNG_HI(1)
                OUT(II,JJ) = OUT(II,JJ)+IN(I,J)*GAU(II-I,JJ-J)
              END DO
            END DO
          END DO
        END DO

*      Preserve current width
        LAST_FWHM = FWHM

      END IF

      END
*+  PSS_PSF_INIT - Find a psf and map required workspace
      SUBROUTINE PSS_PSF_INIT( STATUS )
*
*    Description :
*
*     The input dataset is associated with a psf, and a radial energy profile
*     obtained. In EXPERT mode, this profile is displayed and a choice is
*     made by the user for PSFPIX. In non-EXPERT mode, the 68% enclosed energy
*     radius is chosen automatically.
*
*     After adding a border for interpolation purposes, dynamic memory is
*     mapped for the pixel centred psf, and more space for resampling and
*     convolution if required.
*
*    Environment parameters :
*
*     PSFPIX                = INTEGER(R)
*           Size of PSF mask to cross-correlate
*     PSFCON                = LOGICAL(R)
*           Psf constant across image?
*
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     05 Feb 91 : Original (DJA)
*     19 Feb 92 : Tells user whether psf varies in idiot mode (DJA)
*     11 Sep 92 : Writes extrema of accessed image area (DJA)
*     10 Jul 93 : No longer uses inline functions (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'MATH_PAR'
      INCLUDE 'PSF_PAR'
      INCLUDE 'PSS_PAR'
*
*    Global variables :
*
      INCLUDE 'PSS_CMN'
*
*    Status :
*
      INTEGER                  STATUS               ! Run-time error
*
*    Local variables :
*
      CHARACTER*80     		TEXT                    ! Output data

      REAL             		MAXOFF                  ! Maximum off-axis angle
      REAL             		X0, Y0                  ! Image position

      INTEGER          		FSTAT                   ! WRITE status
      INTEGER          		IAX                     ! Loop over axes
      INTEGER          		IPOS                    ! Loop over image positions
      INTEGER          		ILEV                    ! Loop over levels
      INTEGER          		MAXW                    ! Maximum psf size
      INTEGER          		NIPOS                   ! Number of psf image posn's
      INTEGER          		NUR                     ! # user psfpix values
      INTEGER          		R68(3)                  !

      LOGICAL          		FIR_TRUNC               ! First psf truncated?
      LOGICAL          		GOT_RADII               ! Got valid radii?
      LOGICAL			HOK			! Psf hint ok?
      LOGICAL          		MODEL                   ! Using a model psf?
      LOGICAL          		POSVAR                  ! Psf is position dep.
*
*    Local data :
*
      REAL             		ELEVS(NPSFLEV)
      DATA             		ELEVS/0.5,0.68,0.9,0.95/
      DATA             		FIR_TRUNC/.TRUE./
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Using a psf model
      CALL PSF_QMODEL( PSF_HAN, MODEL, STATUS )

*    If not a model, see if the POSDEP hint is set true
      IF ( .NOT. MODEL ) THEN
        CALL PSF_QHINT( PSF_HAN, PSF_H_POSDEP, HOK, POSVAR, STATUS )
        POSVAR = (HOK.AND.POSVAR)
      ELSE
        POSVAR = MODEL
      END IF

*    PSF constant
      IF ( CP_EXPERT ) THEN

*      Set default
        CALL USI_DEF0L( 'PSF_CON', (.NOT. POSVAR), STATUS )
        CALL USI_GET0L( 'PSF_CON', PSF_CONSTANT, STATUS )

      ELSE
        PSF_CONSTANT = (.NOT. POSVAR)
      END IF
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Number of trial positions for energy profiling
      IF ( PSF_CONSTANT ) THEN
        NIPOS = 1
      ELSE
        NIPOS = 3
        MAXOFF = 1.0*MATH__DTOR
      END IF

*    Use centre of f.o.v
      X0 = 0.0
      Y0 = 0.0

*    Perform energy profiling
      DO IPOS = 1, NIPOS

*      Choose image position
        X0 = MAXOFF*FLOAT(IPOS-1)*0.5
        PSF_PPR(IPOS) = SQRT(X0*X0+Y0*Y0)

*      Get profile
        CALL PSF_ENERGY_PFL( PSF_HAN, NPSFLEV, ELEVS, X0, Y0,
     :                             PSF_PIXL(1,IPOS), STATUS )

*      Divide to get radii in pixels
        DO ILEV = 1, NPSFLEV
          PSF_PIXL(ILEV,IPOS) = PSF_PIXL(ILEV,IPOS)/ABS(AX_DR(1))
        END DO

      END DO
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Set default at 68% radius
      DO IPOS = 1, 3
        R68(IPOS) = MAX(1,NINT(PSF_PIXL(2,IPOS)))
      END DO

*    Display table in expert mode
      IF ( CP_EXPERT ) THEN

        CALL MSG_PRNT( ' ' )
        IF ( PSF_CONSTANT ) THEN
          CALL MSG_PRNT( 'Energy fraction    Radius' )
        ELSE
          CALL MSG_PRNT( 'Energy fraction    Off-axis angle' )
        END IF
        IF ( .NOT. PSF_CONSTANT ) THEN
  2       FORMAT( 18X, <NIPOS>(I3,3X), 'arcmin' )
          WRITE( TEXT, 2, IOSTAT=FSTAT ) (NINT(PSF_PPR(IPOS)*
     :                         MATH__RTOD*60.0) ,IPOS=1,NIPOS)
          CALL MSG_PRNT( TEXT )
        END IF
        CALL MSG_PRNT( ' ' )

        DO ILEV = 1, NPSFLEV

*        Check radius isn't an lower bound
  5       FORMAT( 4X,I4,'%',8X,<NIPOS>(F4.1,2X),' pixels' )
 15       FORMAT( 4X,I4,'%',8X,'> ',<NIPOS>(F5.1,2X),' pixels' )
          IF ( PSF_PIXL(ILEV,NIPOS) .GT. 0 ) THEN
            WRITE( TEXT, 5, IOSTAT=FSTAT ) NINT(ELEVS(ILEV)*100.0),
     :                     (ABS(PSF_PIXL(ILEV,IPOS)),IPOS=1,NIPOS)
          ELSE
            WRITE( TEXT,15, IOSTAT=FSTAT ) NINT(ELEVS(ILEV)*100.0),
     :                     (ABS(PSF_PIXL(ILEV,IPOS)),IPOS=1,NIPOS)
          END IF

          CALL MSG_PRNT( TEXT )

        END DO
        CALL MSG_PRNT( ' ' )

*      Set default to 68% energy
        CALL USI_DEF1I( 'PSFPIX', NIPOS, R68, STATUS )
        IF ( STATUS .NE. SAI__OK ) GOTO 99

*      Loop to get box spec
        GOT_RADII = .FALSE.
        DO WHILE ( (STATUS.EQ.SAI__OK) .AND. .NOT. GOT_RADII )

*        Get list of radii
          CALL USI_GET1I( 'PSFPIX', NIPOS, PSF_PPS, NUR, STATUS )
          CALL USI_CANCL( 'PSFPIX', STATUS )
          IF ( STATUS .NE. SAI__OK ) GOTO 99

*        Right number?
          IF ( NUR .NE. NIPOS ) THEN
            IF ( ( NUR .EQ. 1 ) .AND. ( NIPOS .GT. 1 ) ) THEN
              DO IPOS = 2, NIPOS
                PSF_PPS(IPOS) = PSF_PPS(1)
              END DO
            ELSE
              CALL MSG_PRNT( '! Incorrect number of radii' )
              GOTO 69
            END IF
          END IF

*        In multiple case, check that radii increase
          IF ( NIPOS .GT. 1 ) THEN
            DO IPOS = 2, NIPOS
              IF ( PSF_PPS(IPOS) .LT. PSF_PPS(IPOS-1) ) THEN
                CALL MSG_PRNT( '! Radii must increase with radius' )
                GOTO 69
              END IF
            END DO
          END IF

*        Accept repsonse?
          GOT_RADII = ( STATUS .EQ. SAI__OK )

 69       CONTINUE

        END DO

      ELSE
        CALL MSG_SETI( 'PP', R68(1) )
        IF ( NIPOS .EQ. 1 ) THEN
          CALL MSG_PRNT( 'Using a psf box of radius ^PP pixels' )
        ELSE
          CALL MSG_PRNT( 'Using a variable size psf box' )
        END IF
        DO IPOS = 1, NIPOS
          PSF_PPS(IPOS) = R68(IPOS)
        END DO
        IF ( PSF_CONSTANT ) THEN
          CALL MSG_SETC( 'TYPE', 'constant' )
        ELSE
          CALL MSG_SETC( 'TYPE', 'variable' )
        END IF
        CALL MSG_PRNT( 'Using a ^TYPE psf across the field' )
      END IF

*    Enforce maximum box size
      PSF_BORDER = 1
      MAXW = (PSS__MXHWID-PSF_BORDER)
      DO IPOS = 1, NIPOS
        IF ( PSF_PPS(IPOS) .GT. MAXW ) THEN
          IF ( FIR_TRUNC ) THEN
            CALL MSG_SETI( 'WID', MAXW )
            CALL MSG_PRNT( 'Psf box size truncated at ^WID pixels' )
            FIR_TRUNC = .FALSE.
          END IF
          PSF_PPS(IPOS) = MAXW
        END IF
      END DO

*    Define psf square big enough for largest psf
      PSF_NIPOS = NIPOS
      DO IAX = 1, 2

*      Dimension of psf arrays, big enough for max size + border
        PSF_DIMS(IAX) = PSF_PPS(NIPOS)*2 + 1 + 2*PSF_BORDER

      END DO

*    Map memory for grid of psfs, and space for one shifted psf
      CALL DYN_MAPR( 2, PSF_DIMS, PSF_DATA, STATUS )
      CALL DYN_MAPR( 2, PSF_DIMS, PSF_STORE, STATUS )

*    Tidy up
 99   IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'PSS_PSF_INIT', STATUS )
      END IF

      END
*+  PSS_PSF_RCRIT - Find critical radius for source separation
      REAL FUNCTION PSS_PSF_RCRIT( POS )
*    Description :
*    Deficiencies :
*    Bugs :
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     20 Jun 91 : Original (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'PSS_PAR'
*
*    Global variables :
*
      INCLUDE 'PSS_CMN'
*
*    Import :
*
      REAL                          POS(2)           ! Image position
*
*    Local variables :
*
      REAL                          R                ! Radius of (X,Y)
      REAL                          P50              ! 50% enclosed radius

      INTEGER                       LR               ! Lower psf radius
*-

*    Is the psf constant across the field?
      IF ( PSF_CONSTANT ) THEN

*      Use 50% enclosed energy value
        P50 = PSF_PIXL(1,1)

      ELSE

*      Find radius of point
        R = SQRT( POS(1)*POS(1)+POS(2)*POS(2) )

*      Interpolate to find 50% enclosed energy
        IF ( R .LT. PSF_PPR(1) ) THEN
          P50 = PSF_PIXL(1,1)
        ELSE IF ( R .GT. PSF_PPR(3) ) THEN
          P50 = PSF_PIXL(1,3)
        ELSE

*        Which radius is below R
          IF ( R .GT. PSF_PPR(2) ) THEN
            LR = 2
          ELSE
            LR = 1
          END IF

*        Interpolate. This finds the linear interpolation of 50% enclosed
*        energy PIXL(1,n) using by scaling R between the radii of psf
*        evaluation PPR(n) on either side.
          P50 = PSF_PIXL(1,LR) + ( PSF_PIXL(1,LR+1) -
     :        PSF_PIXL(1,LR) )*(R-PSF_PPR(LR))/
     :                          (PSF_PPR(LR+1)-PSF_PPR(LR))

        END IF

      END IF

*    Find radius in radians
      PSS_PSF_RCRIT = ABS( P50 * AX_DR(1) )

      END
*+  PSS_PSF_SUBSET - Access psf for smaller than maxium area
      SUBROUTINE PSS_PSF_SUBSET( POS, OFF, PSFV, STATUS )
*
*    Description :
*
*     Access the psf for a box half-width of CP_UPNR, but store data in
*     the full size array.
*
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     19 Jul 91 : Original (DJA)
*     10 Jul 93 : Source position vectorised (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'PSS_PAR'
*
*    Global variables :
*
      INCLUDE 'PSS_CMN'
*
*    Import :
*
      REAL                     POS(2)                  ! Image position
      REAL                     OFF(2)                  ! Pix offset to centre
*
*    Export :
*
      REAL                     PSFV(*)                 ! Psf data
*
*    Status :
*
      INTEGER                  STATUS                  ! Run-time error
*
*    Functions :
*
      INTEGER                  PSS_PPIX
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Decide bounds of psf
      PSF_UPNR = PSS_PPIX( SQRT(POS(1)*POS(1)+POS(2)*POS(2)) )

*    Access the data
      CALL PSF_2D_DATA( PSF_HAN, POS(1), POS(2), OFF(1)*AX_DR(1),
     :                  OFF(2)*AX_DR(2), AX_DR(1),
     :                  AX_DR(2), .TRUE., PSF_UDIMS(1),
     :                  PSF_UDIMS(2), PSFV, STATUS )

*    Move data to correct place in array. The psf routine has stuffed the
*    data into the first (PSF_UPNR*2+1)**2 memory locations of PSFV. We
*    transfer the data in reverse order to avoid overwrite corruption.
      IF ( PSF_UDIMS(1) .LT. PSF_DIMS(1) ) THEN
        CALL PSS_PSF_SUBSET_INT( PSF_UDIMS(1), PSFV, PSF_DIMS(1),
     :                                                       PSFV )
      END IF

      END
*+  PSS_PSF_SUBSET_INT - Copy a section of psf array
      SUBROUTINE PSS_PSF_SUBSET_INT( W, IN, OW, OUT )
*
*    Description :
*
*     Copy a 2D array to output with a change of indices. This routine is
*     safe if the two arrays are the same.
*
*    History :
*
*     19 Jul 91 : Original (BHVAD::DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Import :
*
      INTEGER    W,OW                         ! Array dimensions
      REAL       IN(-W/2:W/2,-W/2:W/2)        ! Input psf array
*
*    Export :
*
      REAL       OUT(-OW/2:OW/2,-OW/2:OW/2)   ! Output array
*
*    Local variables :
*
      INTEGER    I,J                          ! Loop over psf data
*-

      DO J = W/2, -W/2, -1
        DO I = W/2, -W/2, -1
          OUT(I,J) = IN(I,J)
        END DO
      END DO

      END
