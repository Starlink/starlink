      SUBROUTINE PSF1_ASCA_DAT( PSID, X0, Y0, QX, QY, DX, DY,
     :                          INTEG, NX, NY, ARRAY, STATUS )
*+
*  Name:
*     PSF1_ASCA_DAT

*  Purpose:
*     Return 2-D probability array for the ASCA GIS

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL PSF1_ASCA_DAT( PSID, X0, Y0, QX, QY, DX, DY, INTEG, NX,
*                         NY, ARRAY, STATUS )

*  Description:
*     Returns integrated psf probability for a NX * NY 2-D detector patch
*     whose position is specified by X0,Y0,QX,QY due to a source at
*     position X0,Y0.

*  Arguments:
*     PSID = INTEGER (given)
*        ADI identifier of psf storage object
*     X0 = REAL (given)
*        X detector position (radians)
*     Y0 = REAL (given)
*        Y detector position (radians)
*     QX = REAL (given)
*        X offset from psf centre to centre of ARRAY (radians)
*     QY = REAL (given)
*        Y offset from psf centre to centre of ARRAY (radians)
*     DX = REAL (given)
*        Size of ARRAY pixels in X axis (radians)
*     DY = REAL (given)
*        Size of ARRAY pixels in Y axis (radians)
*     INTEG = LOGICAL (given)
*        Return integrated probability (ie. normalised to unity if ARRAY
*        was sufficiently large)
*     NX = INTEGER (given)
*        X dimension of ARRAY
*     NY = INTEGER (given)
*        Y dimension of ARRAY
*     ARRAY[NX,NY] = REAL (returned)
*        Integrated psf probability
*     STATUS = INTEGER (given and returned)
*        The global status.

*  Examples:
*     {routine_example_text}
*        {routine_example_description}

*  Pitfalls:
*     {pitfall_description}...

*  Notes:
*     {routine_notes}...

*  Prior Requirements:
*     {routine_prior_requirements}...

*  Side Effects:
*     {routine_side_effects}...

*  Algorithm:
*     {algorithm_description}...

*  Accuracy:
*     {routine_accuracy}

*  Timing:
*     {routine_timing}

*  External Routines Used:
*     {name_of_facility_or_package}:
*        {routine_used}...

*  Implementation Deficiencies:
*     {routine_deficiencies}...

*  References:
*     PSF Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/psf.html

*  Keywords:
*     package:psf, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1996

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     12 Jan 1994 (DJA)
*        Original version.
*      7 May 1996 (DJA):
*        New header (DJA)
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'
      INCLUDE 'PSF_PAR'
      INCLUDE 'MATH_PAR'

*  Global Variables:
      INCLUDE 'PSF_ASCA_CMN'

*  Arguments Given:
      REAL                      DX, DY, X0, Y0, QX, QY
      INTEGER                   PSID, NX, NY
      LOGICAL                   INTEG

*  Arguments Returned:
      REAL                      ARRAY(NX,NY)

*  Status:
      INTEGER 			STATUS             	! Global status

*  External References:
      EXTERNAL			PSF1_ASCA_CINT
        REAL                    PSF1_ASCA_CINT
      EXTERNAL			PSF1_ASCA_E
        REAL                    PSF1_ASCA_E

*  Local Constants:
      REAL                      RTOM                    ! Radian to arcmin
        PARAMETER               ( RTOM = MATH__RTOD*60.0 )
      INTEGER			NRAD			!
        PARAMETER		( NRAD = 6 )

*  Local Variables:
      CHARACTER*132		FNAME

      REAL                     	NORM                    ! Normalisation constant
      REAL                      P_SCALE                 ! Scale size of psf
      REAL                      ROFF                    ! Off-axis angle
      REAL			ROTA			! Rotation angle
      REAL                      SUM                     ! Cumulative value
      REAL                      XP0, YP0                ! Major pixel centre
      REAL                      XPS, YPS                ! Sub-pixel pixel centre

      INTEGER                   I, J                    ! Major pixel loops
      INTEGER			IENER, IRAD		! Indexing values

*  Local Data:
      REAL			RADS(NRAD)		! Radial bnds (arcmin)
      INTEGER			AZIM(NRAD)
        DATA                	RADS/2.0,4.0,6.0,8.0,13.0,17.0/
        DATA			AZIM/5,10,9,35,0,0/
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Base coordinates
      XP0 = ( - REAL(NX)/2.0 ) * DX + X0 + QX
      YP0 = ( - REAL(NY)/2.0 ) * DY + Y0 + QY

*  Find energy bin
      IENER = INT( PSF1_ASCA_E( PSID, STATUS ) )
      IENER = MAX( IENER, 1 )
      IENER = MIN( IENER, AS_NE )

*  Find radial bin number
      ROFF = SQRT( X0*X0 + Y0*Y0 ) * MATH__RTOD * 60.0
      IRAD = 1
      DO WHILE ( (IRAD.LE.NRAD) .AND. (ROFF.GT.RADS(IRAD)) )
        IRAD = IRAD + 1
      END DO
      IF ( IRAD .GT. NRAD ) IRAD = NRAD

*  Rotation of requested psf position from calibration position
      IF ( X0 .EQ. 0.0 ) THEN
        IF ( Y0 .GE. 0.0 ) THEN
          ROTA = 0.0
        ELSE
          ROTA = 180.0
        END IF
      ELSE
        ROTA = ATAN2(Y0,-X0)*MATH__RTOD
      END IF
      IF ( ROTA .LT. 0.0 ) ROTA = ROTA + 360.0
      ROTA = (ROTA - REAL(AZIM(IRAD))+180.0) * MATH__DTOR

*  Load the telescope and detector psfs
 10   FORMAT( A, I2.2, '_', I2.2, '.fits' )
      IF ( AS_GPTR(IENER,IRAD) .EQ. 0 ) THEN
        WRITE( FNAME, 10 ) 'psf_', NINT(RADS(IRAD)), AZIM(IRAD)
        CALL PSF1_ASCA_FLOAD( 'gis', FNAME, AS_GPTR(1,IRAD),
     :                         AS_GPIX, STATUS )
      END IF

*  Scale size of psf
      P_SCALE = (0.3/60.0) * MATH__DTOR

*  Normalisation factor to account for difference in input pixel size
*  to calibration pixel size
      NORM = ABS((DX*DY*RTOM*RTOM) / AS_GPIX**2)

*  For each point requiring calculation
      DO J = 1, NY

*    Y coordinate of this pixel in arcmin
        YPS = (YP0 + (REAL(J)-0.5)*DY) * RTOM

        DO I = 1, NX

*      X position of this pixel in arcmin
          XPS = (XP0 + (REAL(I)-0.5)*DX) * RTOM

*      Enquire value of calibration array at this point
          SUM = PSF1_ASCA_CINT( AS_NXY, %VAL(AS_GPTR(IENER,IRAD)),
     :                           ROTA, (XPS-X0*RTOM)/AS_GPIX,
     :                           (YPS-Y0*RTOM)/AS_GPIX )

*      Correct for differing pixel sizes
          ARRAY(I,J) = SUM * NORM

        END DO

      END DO

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'PSF1_ASCA_DAT', STATUS )
      END IF

      END


*+  PSF1_ASCA_FLOAD - Load a file of psfs at different energies
      SUBROUTINE PSF1_ASCA_FLOAD( FORM, FILE, PARRAY, PIX, STATUS )
*
*    Description :
*
*    Method :
*
*     Loads AS_NE psfs stored in a FITS file and normalises those psfs
*     to unity.
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
      INCLUDE 'PRM_PAR'
      INCLUDE 'PSF_PAR'
      INCLUDE 'MATH_PAR'
      INCLUDE 'PSF_ASCA_CMN'
*
*    Import :
*
      CHARACTER*(*)		FORM			! xrt,sis or gis
      CHARACTER*(*)		FILE			! File to load
*
*    Export :
*
      INTEGER			PARRAY(*)		! Pointer array
      REAL			PIX			! Pixel size (arcmin)
*
*    Status :
*
      INTEGER                   STATUS                  ! Run-time error
*
*    Functionss :
*
      INTEGER			CHR_LEN
*
*    Local variables :
*
      CHARACTER*132             PFILE                   ! Psf file name

      REAL			SUM			! Normalisation

      INTEGER			BSIZE			! FITS block factor
      INTEGER			CPTR			! Cursor over data
      INTEGER			FSTAT			! FITSIO status
      INTEGER                   HDUTYPE                 ! HDU type from FITSIO
      INTEGER			IE			! Loop over energy
      INTEGER			LUN			! Logical unit

      LOGICAL			ANYNULL			! Any nulls read?
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Get logical unit from system
      CALL FIO_GUNIT( LUN, STATUS )

*    Open file
      FSTAT = 0
      PFILE = AS_CALDB(:AS_CALDBL)//'/data/asca/'//FORM/
     :              /'/bcf/psf/'//FILE(:CHR_LEN(FILE))

      CALL FTOPEN( LUN, PFILE, 0, BSIZE, FSTAT )
      CALL MSG_SETC( 'FILE', PFILE )
      IF ( FSTAT .NE. 0 ) THEN
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'Unable to open calibration file ^FILE',
     :                STATUS )
        GOTO 99
      ELSE
        CALL MSG_PRNT( 'Loading psfs from ^FILE' )
      END IF

*    Map sufficient memory for 10 energy bands at 63x63 resolution
      CALL DYN_MAPR( 1, AS_NXY*AS_NXY*AS_NE*VAL__NBR, CPTR, STATUS )

*    Read in different energies
      DO IE = 1, AS_NE

*      Store pointer
        PARRAY(IE) = CPTR

*      Load data from this image
        CALL FTGPVE( LUN, 1, 1, AS_NXY*AS_NXY, -999.0, %VAL(CPTR),
     :               ANYNULL, FSTAT )

*      Normalise
        CALL ARR_SUM1R( AS_NXY*AS_NXY, %VAL(CPTR), SUM, STATUS )
        CALL ARR_MULTR( 1.0/SUM, AS_NXY*AS_NXY, %VAL(CPTR), STATUS )

*      Move to next HDU
        IF ( IE .LT. 10 ) THEN
          CALL FTMRHD( LUN, 1, HDUTYPE, FSTAT )
          CPTR = CPTR + AS_NXY*AS_NXY*VAL__NBR
        END IF

      END DO

*    Close file
      CALL FTCLOS( LUN, FSTAT )

*    Pixel size
      PIX = 4.0 * 0.2456

*    Return unit to system
 99   CALL FIO_PUNIT( LUN, STATUS )

      END


*+  PSF1_ASCA_CINT - Get interpolated psf value
      REAL FUNCTION PSF1_ASCA_CINT( NXY, PVALS, ROTA, XP, YP )
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
      INTEGER			NXY			! Size of array
      REAL			PVALS(-NXY/2:NXY/2,	! Psf values
     :                                -NXY/2:NXY/2)
      REAL			ROTA			! Rotation required
      REAL			XP, YP			! Position of interest
*
*    Local variables :
*
      REAL			RXP, RYP, XOFF, YOFF, T, U

      INTEGER			IX,IY			! Pixel numbers
      INTEGER			J, K			! Base for interp
*-

*    Apply rotation to XP,YP
      CALL MATH_R2DR( -XP, YP, -ROTA, RXP, RYP )
      RXP = - RXP

*  Offsets from edge of grid
      XOFF = REAL(NXY)/2.0 + RXP
      YOFF = REAL(NXY)/2.0 + RYP

*  Pixel number in PVALS
      IX = -NXY/2 + INT(XOFF)
      IY = -NXY/2 + INT(YOFF)

      IF ( (ABS(IX) .GT. NXY) .OR. (ABS(IY) .GT. NXY) ) THEN
        PSF1_ASCA_CINT = 0.0
      ELSE IF ( (ABS(IX) .EQ. NXY) ) THEN
        PSF1_ASCA_CINT = PVALS(IX,IY)
      ELSE IF ( (ABS(IY) .EQ. NXY) ) THEN
        PSF1_ASCA_CINT = PVALS(IX,IY)
      ELSE
        J = INT( XOFF - 0.5 ) + 1
        K = INT( YOFF - 0.5 ) + 1
        T = XOFF - REAL(J) + 0.5
        U = YOFF - REAL(K) + 0.5
        J = -NXY/2 + J - 1
        K = -NXY/2 + K - 1
        PSF1_ASCA_CINT = (1.0-T)*(1.0-U)*PVALS(J,K) +
     :                   T * (1.0-U) * PVALS(J+1,K) +
     :                   T * U * PVALS(J+1,K+1) +
     :                   (1.0-T) * U * PVALS(J,K+1)
      END IF

      END
