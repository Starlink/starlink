      SUBROUTINE PSF0_USEGRID( DIMS, NDIM, GDX, XYSAM, RPTR, EPTR,
     :                         DPTR, FLAGS, PSID, ENERGY, X0, Y0,
     :                         QX, QY, DX, DY,
     :                         NX, NY, ARRAY, STATUS )
*+
*  Name:
*     PSF0_USEGRID

*  Purpose:
*     Use psf grid to get 2D probability array

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL PSF0_USEGRID( DIMS, NDIM, GDX, XYSAM, RPTR, EPTR, DPTR, FLAGS,
*                        FLAGS, PSID, ENERGY, X0, Y0, QX, QY, DX, DY,
*                        NX, NY, ARRAY, STATUS )

*  Description:
*     Returns integrated psf probability for a NX * NY 2-D detector patch
*     whose position is specified by X0,Y0,QX,QY due to a source at
*     position X0,Y0, at energy ENERGY if energy is significant.
*
*     Psf grid dimensions are organised as follows,
*
*      1,2 - the X,Y offset dimensions from the psf position in the FOV
*       3  - in radial mode the off-axis dimension from the FOV
*      3,4 - in rectaungular mode, the X and Y offsets in the FOV
*      4/5 - the last dimension, if present, is always energy

*  Arguments:
*     DIMS[5] = INTEGER (given)
*        Grid dimensions. Only NDIM values are set
*     NDIM = INTEGER (given)
*        Number of grid dimensions. Can take values 3..5 depending on
*        presence of energy axis and polar/rectangular mode
*     GDX = REAL (given)
*        Size in radians of gridded psf pixels
*     XYSAM[2,2] = REAL (given)
*        Origin and spacing of rectangular samples if non-polar. Units
*        are radians
*     RPTR = INTEGER (given)
*        Pointer to radial samples, if radial mode, otherwise zero. Units
*        are radians
*     EPTR = INTEGER (given)
*        Pointer to energy bin values, zero if none
*     DPTR = INTEGER (given)
*        Pointer to grid data
*     FLAGS = INTEGER (given)
*        Flags denoting grid state.
*     PSID = INTEGER (given)
*        ADI identifier of psf storage object
*     ENERGY = REAL (given)
*        Energy at which is to be evaluated if energy is being used
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
*     Should allow for compression of grid because of azimuthal symmetry

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
*     15 May 1996 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER			DIMS(5), NDIM, RPTR, EPTR, DPTR, FLAGS
      REAL			GDX, XYSAM(2,2), ENERGY
      REAL                      DX, DY, X0, Y0, QX, QY
      INTEGER                   PSID, NX, NY

*  Arguments Returned:
      REAL                      ARRAY(NX,NY)

*  Status:
      INTEGER 			STATUS             	! Global status

*  External References:
      EXTERNAL			PSF0_GRID_D
        REAL			PSF0_GRID_D

*  Local Constants:
      INTEGER			SAMPLE			! Over sampling
        PARAMETER		( SAMPLE = 3 )

*  Local Variables:
      REAL			AFAC			! Normalisation factor
      REAL			E1, E2			! Sample energies
      REAL			F1, F2			! Interpolate fractions
      REAL			LHS, BOT		!
      REAL			P1, P2			! Psf values
      REAL			R1, R2			! Sample radii
      REAL			ROFF			! Off axis angle
      REAL			WIDX, WIDY		! Psf image size
      REAL			XP, YP, XPP, YPP	! Pixel positions

      INTEGER			EBIN			! Energy bin number
      INTEGER			I, J, II, JJ		! Pixel & subpix loops
      INTEGER			IP1, IP2		! Psf numbers
      INTEGER			RBIN			! Radial bin number
      INTEGER			XROW, YROW		! Psf image indices

      LOGICAL			INTERP			! Interpolate?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Scale factor for difference in pixel areas, and oversampling
      AFAC = (ABS(DX*DY) / GDX**2) / SAMPLE**2

*  Find LHS and BOT in radians
      LHS = X0 + QX - DX * REAL(NX) / 2.0
      BOT = Y0 + QY - DY * REAL(NY) / 2.0

*  Semi-widths of psf images in radians
      WIDX = GDX * REAL(DIMS(1))/2.0
      WIDY = GDX * REAL(DIMS(2))/2.0

*  Identify the the energy bin number, if any
      IF ( EPTR .EQ. 0 ) THEN
        EBIN = 1
      ELSE

*    Binary search to find energy bin centre below our energy
        CALL ARR_BSRCHR( DIMS(NDIM), %VAL(EPTR), ENERGY, EBIN,
     :                   STATUS )

*    See if the next energy is actually closer
        IF ( EBIN .LT. DIMS(NDIM) ) THEN
          CALL ARR_ELEM1R( EPTR, DIMS(NDIM), EBIN-1, E1, STATUS )
          CALL ARR_ELEM1R( EPTR, DIMS(NDIM), EBIN, E2, STATUS )
          IF ( (E2-ENERGY) .LT. (ENERGY-E1) ) EBIN = EBIN + 1
        END IF

      END IF

*  Polar mode?
      IF ( RPTR .NE. 0 ) THEN

*    Off axis angle of psf request
        ROFF = SQRT( X0**2 + Y0**2 )

*    Binary search to find radius
        CALL ARR_BSRCHR( DIMS(3), %VAL(RPTR), ROFF, RBIN, STATUS )

*    Off the bottom of the radial range?
        IF ( RBIN .EQ. 1 ) THEN
          CALL ARR_ELEM1R( RPTR, DIMS(3), 1, R1, STATUS )
          INTERP = ( ROFF .GT. R1 )

*    Off the top?
        ELSE IF ( RBIN .EQ. DIMS(3) ) THEN
          CALL ARR_ELEM1R( RPTR, DIMS(3), DIMS(3), R2, STATUS )
          INTERP = ( ROFF .LT. R2 )

*    Otherwise interpolate
        ELSE
          INTERP = .TRUE.

        END IF
        IF ( INTERP ) THEN
          CALL ARR_ELEM1R( RPTR, DIMS(3), RBIN, R1, STATUS )
          CALL ARR_ELEM1R( RPTR, DIMS(3), RBIN+1, R2, STATUS )
          F1 = (ROFF-R1)/(R2-R1)
          F2 = 1.0 - F1

        END IF

*    Convert these bin numbers into psf numbers
        IF ( EPTR .EQ. 0 ) THEN
          IP1 = RBIN
          IP2 = RBIN + 1
        ELSE
          IP1 = RBIN + (EBIN-1)*DIMS(NDIM)
          IP2 = RBIN + 1 + (EBIN-1)*DIMS(NDIM)
        END IF

*    Loop over output pixel values Y axis
        DO J = 1, NY

*      Y position of pixel centres in this row
          YP = BOT + (REAL(J)-0.5)*DY

*      Loop over output pixel values X axis
          DO I = 1, NX

*        X position of pixel centre
            XP = LHS + (REAL(I)-0.5)*DX

*        Initialise output
            ARRAY(I,J) = 0.0

*        Y axis subsampling
            DO JJ = 1, SAMPLE

*          Y centre of sub-pixel
              YPP = YP - 0.5*DY + (REAL(JJ-1)+0.5)*DY/SAMPLE

*          Sub-pixel is on the psf image?
              IF ( ABS(YPP) .GT. WIDY ) THEN

*            The Y psf row in which the sub-pixel lies
                YROW = NINT(YPP/GDX)

*            X axis subsampling
                DO II = 1, SAMPLE

*              X centre of sub-pixel
                  XPP = XP - 0.5*DX + (REAL(II-1)+0.5)*DX/SAMPLE

*              Sub-pixel is on the psf image?
                  IF ( ABS(XPP) .GT. WIDX ) THEN

*                The X psf row in which the sub-pixel lies
                    XROW = NINT(XPP/GDX)

*                We now have a request for the psf value at a position
*                (XPP,YPP) in radians wrt the centre of our 2D psf image(s).
*                Now switch on whether interpolation is required
                    IF ( INTERP ) THEN
                      P1 = PSF0_GRID_D( DIMS(1), DIMS(2), %VAL(DPTR),
     :                                  IP1, XROW, YROW )
                      P2 = PSF0_GRID_D( DIMS(1), DIMS(2), %VAL(DPTR),
     :                                  IP2, XROW, YROW )
                      ARRAY(I,J) = ARRAY(I,J) + (P1*F1+P2*F2) * AFAC

                    ELSE
                      P1 = PSF0_GRID_D( DIMS(1), DIMS(2), %VAL(DPTR),
     :                                  IP1, XROW, YROW )
                      ARRAY(I,J) = ARRAY(I,J) + P1 * AFAC

                    END IF

                  END IF

                END DO

*          End of test of sub-pixel on psf image
              END IF

*        End of Y sub-pixelling
            END DO

*      End loop over X axis
          END DO

*    End loop over Y axis
        END DO

*  Rectangular mode
      ELSE
      END IF

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'PSF0_USEGRID', STATUS )
      END IF

      END



      REAL FUNCTION PSF0_GRID_D( PNX, PNY, PSF, IP, IX, IY )
*+
*  Name:
*     PSF0_GRID_D

*  Purpose:
*     Return psf value

*  Language:
*     Starlink Fortran

*  Invocation:
*     RESULT = PSF0_GRID_D( PNX, PNY, PSF, IP, IX, IY )

*  Description:
*     {routine_description}

*  Arguments:
*     PNX = INTEGER (given)
*        X dimension of psf array
*     PNY = INTEGER (given)
*        Y dimension of psf array
*     PSF[] = REAL (given)
*        Psf data
*     IP = INTEGER (given)
*        Psf index
*     IX = INTEGER (given)
*        X index into psf
*     IY = INTEGER (given)
*        Y index into psf

*  Returned Value:
*     psf0_grid_d = {data_type}
*        {returned_value_description}

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
*     {facility_or_package}...

*  Implementation Deficiencies:
*     {routine_deficiencies}...

*  References:
*     PSF Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/psf.html

*  Keywords:
*     package:psf, usage:private

*  Copyright:
*     {routine_copyright}

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     16 May 1996 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given:
      INTEGER			PNX, PNY, IX, IY, IP
      REAL			PSF(-PNX/2:PNX/2,-PNY/2:PNY/2,*)
*.

*  Check spatial indices are in range
      IF ( (IX.LE.PNX/2) .AND. (IY.LE.PNY/2) ) THEN
        PSF0_GRID_D = PSF(IX,IY,IP)
      ELSE
        PSF0_GRID_D = 0.0
      END IF

      END
