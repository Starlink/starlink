*+  MATH_GAUSS - Returns Gaussian distributed value with given mean & std deviation
	REAL FUNCTION MATH_GAUSS(XMEAN,SIGMA)
*    Description :
*     Returns Gaussian distributed variable with mean XMEAN and Std devn SIGMA
*    Method :
*     Uses NAG routines
*    Deficiencies :
*    Bugs :
*    Authors :
*
*     Doug Bertram (BHVAD::DB)
*     David J. Allan (JET-X, University of Birmingham)
*
*    History :
*
*     10 Aug 90 : Original (adapted from MATH_POISS) (DB)
*     10 Jun 94 : Converted internals to double precision (DJA)
*
*    Type Definitions :
*
      IMPLICIT NONE
*
*    Import :
*
      REAL			XMEAN			! Mean value
      REAL 			SIGMA
*
*    External references :
*
      DOUBLE PRECISION 		G05DDF			! NAG gaussian random number
        EXTERNAL		G05DDF
*
*    Local variables :
*
      LOGICAL			INITIALISE		! True if first call
        SAVE			INITIALISE
*
*    Local data :
*
      DATA			INITIALISE /.TRUE./
*-

*    Initialise random number generator if necessary
      IF ( INITIALISE ) THEN
	CALL G05CCF
	INITIALISE=.FALSE.
      END IF

      MATH_GAUSS = G05DDF ( DBLE(XMEAN), DBLE(SIGMA) )

      END
