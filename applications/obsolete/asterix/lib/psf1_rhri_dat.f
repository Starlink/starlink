      SUBROUTINE PSF1_RHRI_DAT( PSID, X0, Y0, QX, QY, DX, DY,
     :                          INTEG, NX, NY, ARRAY, STATUS )
*+
*  Name:
*     PSF1_RHRI_DAT

*  Purpose:
*     Return ROSAT HRI psf

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL PSF1_RHRI_DAT( PSID, X0, Y0, QX, QY, DX, DY, INTEG, NX,
*                          NY, ARRAY, STATUS )

*  Description:
*     Returns integrated psf probability for a NX * NY 2-D detector patch
*     whose position is specified by X0,Y0,QX,QY due to a source at
*     position X0,Y0.
*
*     This model of the XRT HRI detector spatial response was constructed
*     by MPE/GSFC/SAO. The function form of the radial surface brightness
*     is
*
*       psf = a1*exp(-0.5*(r/s1)**2) + a2*exp(-0.5*(r/s2)**2) + a3*exp(-r/s3)
*
*     This is good fit out to 100 arcsec from the centre of the psf.
*
*     The S2 parameter varies off-axis as follows,
*
*      s2 = 3.3 + 0.019 r - 0.016 r^2 + 0.0044 r^3
*
*     See "The ROSAT HRI", Feb 95, David et al.

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
*     11 Mar 1991 (DJA):
*        Original version
*      3 Sep 1992 (DJA):
*        Updated response to 3-component fit
*      1 Mar 1994 (DJA):
*        Use MATH_EXPR rather than D.P. intrinsic
*      4 Apr 1996 (DJA):
*        Now varies off-axis parameterisation for S2
*      2 May 1996 (DJA):
*        New header
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'MATH_PAR'

*  Arguments Given:
      REAL                     DX, DY, X0, Y0, QX, QY
      INTEGER                  PSID, NX, NY
      LOGICAL                  INTEG

*  Arguments Returned:
      REAL                     ARRAY(NX,NY)

*  Status:
      INTEGER 			STATUS             	! Global status

*  External References:
      EXTERNAL			MATH_EXPD
        DOUBLE PRECISION	MATH_EXPD
      EXTERNAL			MATH_EXPR
        DOUBLE PRECISION	MATH_EXPR

*  Local Constants:
      REAL                     	A1, A2, A3              ! Relative contributions
        PARAMETER               ( A1 = 0.9638,          ! of 3 components
     :                            A2 = 0.1798,
     :                            A3 = 0.00090 )
      REAL                      S1, S3              	! Radial scale of comps
        PARAMETER               ( S1 = 2.1858,          ! in arcsec
     :	                          S3 = 31.69 )
      REAL                      RTOS                    ! Radian to arcsec
        PARAMETER               ( RTOS = MATH__RTOD*3600.0 )

*  Local Variables:
      REAL                      LNORM                   ! Normalisation constant
      REAL			NORM,N1
      REAL                      P_SCALE                 ! Scale size of psf
      REAL			ROFF 			! Off axis angle
      REAL                      RPS_R,RPS               ! Radius of sub-pix ^ 2
      REAL                      S1_2, S2_2              !
      REAL			S2			!
      REAL                      SDX, SDY                ! Sub-pixel bin sizes
      REAL                      SUM                     ! Cumulative value
      REAL                      XP0, YP0                ! Major pixel centre
      REAL                      XPS, YPS                ! Sub-pixel pixel centre
      REAL                      YPS2                    ! Sub-pixel distance

      INTEGER                   I, J                    ! Major pixel loops
      INTEGER                   II, JJ                  ! Sub-pixel loops
      INTEGER                   MNX, MNY                ! Local calc bounds
      INTEGER                   XSUB, YSUB              ! Sub-pixel factors

      LOGICAL                   SYMMETRIC               ! Symmetric about centre?

*  Internal References:
      REAL                     DEL,PIX
      INTEGER                  SPIX
      DOUBLE PRECISION         HFUNC
      SPIX(DEL,PIX) = MAX(1,NINT(abs(10.0*PIX)/P_SCALE/MAX(1.0,
     :                                SQRT(ABS(DEL/P_SCALE)))))
      HFUNC(DEL) = A1*MATH_EXPR(DEL*S1_2)+
     :             A2*MATH_EXPR(DEL*S2_2)+
     :             A3*MATH_EXPR(-SQRT(DEL)/S3)
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Off-axis angle in arcmin
      ROFF = SQRT(X0*X0+Y0*Y0)*MATH__RTOD*60.0

*  Compute S2
      S2 = 3.3 + 0.019 * ROFF - 0.016 * ROFF**2 + 0.0044 * ROFF**3

*  Compute the normalisation. This was found by integrating the
*  above formulae out to a distance of 100 arcsec
      N1 = 750.0 + 4.31818 * ROFF - 3.63636*ROFF**2 + ROFF**3
      NORM = 10.6971 * MATH__PI +
     :       6.96186E-6 * MATH__PI * N1*N1 -
     :       6.96186E-6 * MATH__PI * N1*N1 *
     :       MATH_EXPD(DBLE(-5000.0/(S2*S2)))

*  A few variables to speed things up
      S1_2 = -0.5 / S1**2
      S2_2 = -0.5 / S2**2
      P_SCALE = SQRT(S1**2+S2**2)/RTOS

*  Base coordinates
      XP0 = ( - REAL(NX)/2.0 ) * DX + X0 + QX
      YP0 = ( - REAL(NY)/2.0 ) * DY + Y0 + QY

*  Symmetric?
      SYMMETRIC = ( ( X0 .EQ. 0.0 ) .AND. ( Y0 .EQ. 0.0 )
     :        .AND. ( QX .EQ. 0.0 ) .AND. ( QY .EQ. 0.0 ) )

*  Bounds for calculation
      IF ( SYMMETRIC ) THEN

*    The "lower left corner" of the array. The +1 guarantees that the
*    centre pixel gets done for odd values of NX/Y
        MNX = (NX+1)/2
        MNY = (NY+1)/2

      ELSE

*    The whole array
        MNX = NX
        MNY = NY

      END IF

*  For each point requiring calculation
      DO J = 1, MNY

*    Find Y sub-pixelling
        YSUB = SPIX( YP0 + DY*REAL(J-1), DY )
        SDY = DY / YSUB

        DO I = 1, MNX

*      Zero
          SUM = 0.0

*      Find X sub-pixelling
          XSUB = SPIX( XP0 + DX*REAL(I-1), DX )
          SDX = DX / XSUB

*      Correct normalisation for sub-pixel and pixel size
          LNORM = ABS(SDX*SDY*RTOS*RTOS)/NORM

*      Y position of first sub-pixel centre
          YPS = YP0 + DY*(J-1) + 0.5*SDY

*      For each sub-pixel row
          DO JJ = 0, YSUB-1

*        Y distance from psf centre
            YPS2 = (YPS-Y0)**2

*        X position of first sub-pixel centre
            XPS = XP0 + DX*(I-1) + 0.5*SDX

*        For each sub-pixel
            DO II = 0, XSUB-1

*          Radius of sub-pixel squared
              RPS = (XPS-X0)**2 + YPS2
              RPS_R = SQRT(RPS)*RTOS

*          Value of gaussian
              IF ( RPS_R .LE. 100.0) SUM=SUM+HFUNC( RPS*RTOS*RTOS )

*          Next sub-pixel
              XPS = XPS + SDX

            END DO

*        Next row of sub-pixels
            YPS = YPS + SDY

          END DO

*      Set ARRAY value
          ARRAY(I,J) = SUM*LNORM

        END DO

      END DO

*  Copy array around if symmetrical
      IF ( SYMMETRIC ) THEN

*    Transfer data to other 3 quadrants
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

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'PSF1_RHRI_DAT', STATUS )
      END IF

      END
