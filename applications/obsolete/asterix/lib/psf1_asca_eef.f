      REAL FUNCTION PSF1_ASCA_EEF( E, R, OFF, AZI )
*+
*  Name:
*     PSF1_ASCA_EEF

*  Purpose:
*     Find enclosed energy fraction at specified radius from detector position

*  Language:
*     Starlink Fortran

*  Invocation:
*     RESULT = PSF1_ASCA_EEF( E, R, OFF, AZI )

*  Description:
*
*                f(r,r1) - c*f(r,r2)
*         eef = ----------------------
*                f(12,r1)- c*f(12,r2)
*
*     where f(r,R) = 1-(r/R)*exp(-r/R)-exp(-r/R)

*  Arguments:
*     E = REAL (given)
*        Energy in keV
*     R = REAL (given)
*        Diameter (mm) (0.9823 arc min per mm)
*     OFF = REAL (given)
*        Off axis angle (arcmin)
*     AZI = REAL (gven)
*        Azimuthal angle (radian). This parameter is insensitive for the EEF.

*  Returned Value:
*     PSF1_ASCA_EEF = REAL
*        Fraction of psf enclosed at this radius, normalised at 12mm
*        (11.8 arcmin)

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
*     package:psf, usage:public

*  Copyright:
*     {routine_copyright}

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*      1 May 1996 (DJA):
*        Adapted from original XANADU version by H.Awaki
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given:
      REAL			E, R, OFF, AZI

*  Local Variables:
      REAL 			EXR1 , EXR2 , EXO1 , EXO2
      REAL			RR1 , CC , RR2
      REAL			RVAL			! Return value

*  Local Data:
      REAL 			R1(4) , R2(4) , R0 , C(4)
      DATA r1/0.37273 , 0.013695 , 0.0 , 0.0023062/
      DATA c/6.1508 , -0.047136 , -0.062403 , -0.0069433/
      DATA r2/2.2771 , 0.012846 , 0.02 , 0.002/
      DATA r0/12.0/
*.

      rr1 = (r1(1)+r1(2)*Off) + (r1(3)+r1(4)*Off)*E
      cc = (c(1)+c(2)*Off) + (c(3)+c(4)*Off)*E
      rr2 = (r2(1)+r2(2)*Off) + (r2(3)+r2(4)*Off)*E
      exr1 = EXP(-R/rr1)
      exr2 = EXP(-R/rr2)
      exo1 = EXP(-r0/rr1)
      exo2 = EXP(-r0/rr2)
      IF ( R.GT.0.0 ) THEN
         RVAL = (1.0-R/rr1*exr1-exr1) + cc*(1.0-R/rr2*exr2-exr2)
         RVAL = RVAL/((1.0-r0/rr1*exo1-exo1)
     &              +cc*(1.0-r0/rr2*exo2-exo2))
      ELSE
         RVAL = 0.0
      ENDIF

      PSF1_ASCA_EEF = RVAL

      END
