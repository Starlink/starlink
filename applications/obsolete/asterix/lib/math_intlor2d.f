*+  MATH_INTLOR2D - Return array of integrated 2D Lorentz distribution
      SUBROUTINE MATH_INTLOR2D( XWID, YWID, ROT, X0, Y0, QX, QY,
     :                          DX, DY, NX, NY, ARRAY, STATUS )
*
*    Description :
*
*     The value of the integrated unit normalised Lorentzian distribution
*     described by XWID,YWID ( the widths in both axes ) and ROT (the
*     angle of rotation wrt increasing X) is found for each element of ARRAY.
*     Units of ROT are radians.
*
*     The Lorentzian is centred at 0,0. The centre of ARRAY is X0,Y0.
*
*    Method :
*
*     Each pixel of ARRAY is subdivided in X and Y depending on the
*     distance in sigma in each axis at that point. The formula used
*     coded in the function SPIX. The sub-pixelling is given by 10
*     times the binsize / sigma, modified by SQRT(offset/sigma) to
*     tend to unity for large offsets. This combines high accuracy
*     where the gradient is highest, with fast operation in large
*     areas at large sigma values.
*
*     The spot value of the gaussian is found in each sub-pixel, the
*     sum of the sub-pixels contributing to each major (ARRAY) pixel.
*
*     The case where the array centre coincides with the centre of
*     the gaussian is handled separately as there is 4-fold symmetry
*     about X and Y axes.
*
*    Deficiencies :
*
*     ROT angle not yet implemented
*
*    Bugs :
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     14 Dec 94 : Original (DJA)
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
      REAL                      XWID, YWID, ROT         ! The Lorentz params
      REAL                      DX, DY, X0, Y0,QX,QY
      INTEGER                   NX,NY
*
*    Export :
*
      REAL                      ARRAY(NX,NY)            ! Gaussian int surface
*
*    Status :
*
      INTEGER                   STATUS                  ! Run-time error
*
*    Local constants :
*
      INTEGER                  SPIX
        PARAMETER              ( SPIX = 5 )
*
*    Local variables :
*
      REAL                      NORM               	! Normalisation constant
      REAL                      SDX, SDY                ! Sub-pixel bin sizes
      REAL                      SUM                     ! Cumulative value
      REAL			WID2
      REAL                      XNOR, YNOR              ! X,Y bits to NORM
      REAL                      XP0, YP0                ! Major pixel centre
      REAL                      XPS, YPS                ! Sub-pixel pixel centre
      REAL                      YPS2                    ! Sub-pixel distance

      INTEGER                   I, J                    ! Major pixel loops
      INTEGER                   II, JJ                  ! Sub-pixel loops
      INTEGER                   MNX, MNY                ! Local calc bounds
      INTEGER                   XSUB, YSUB              ! Sub-pixel factors

      LOGICAL                   SYMMETRIC               ! Symmetric about centre?
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Width squared
      WID2 = XWID*YWID

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
        YSUB = SPIX
        SDY = DY / YSUB

*      Y contribution to normalisation
        YNOR = YWID/SDY

        DO I = 1, MNX

*        Zero
          SUM = 0.0

*        Find X sub-pixelling
          XSUB = SPIX
          SDX = DX / XSUB

*        X contribution to normalisation - hence total normalisation
          XNOR = XWID/SDX
          NORM = ABS( XNOR*YNOR/MATH__PI )

*        Y position of first sub-pixel centre
          YPS = YP0 + DY*(J-1) + 0.5*SDY

*        For each sub-pixel row
          DO JJ = 0, YSUB-1

*          Y centre of sub-pixel in sigma^2
            YPS2 = (YPS/YWID)**2

*          X position of first sub-pixel centre
            XPS = XP0 + DX*(I-1) + 0.5*SDX

*          For each sub-pixel
            DO II = 0, XSUB-1

*            Value of Lorentzian
              SUM = SUM + 1.0 / ((XPS/XWID)**2 + YPS2 + WID2)

*            Next sub-pixel
              XPS = XPS + SDX

            END DO

*          Next row of sub-pixels
            YPS = YPS + SDY

          END DO

*        Set ARRAY value
          ARRAY(I,J) = SUM*NORM

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
