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
*     Richard Beard (ROSAT, University of Birminggam)
*
*    History :
*
*     10 Aug 90 : Original (adapted from MATH_POISS) (DB)
*     10 Jun 94 : Converted internals to double precision (DJA)
*      6 Jun 97 : Convert to PDA (RB)
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
      REAL			PDA_RNNOR
        EXTERNAL		  PDA_RNNOR
*
*    Local variables :
*
      LOGICAL			INITIALISE		! True if first call
        SAVE			INITIALISE

      INTEGER			SEED, TICKS, STATUS
*
*    Local data :
*
      DATA			INITIALISE /.TRUE./
*-

*    Initialise random number generator if necessary
      IF ( INITIALISE ) THEN
        STATUS = 0
        CALL PSX_TIME( TICKS, STATUS )
        SEED = ( TICKS / 4 ) * 4 + 1
        CALL PDA_RNSED( SEED )
	INITIALISE=.FALSE.
      END IF

      MATH_GAUSS = PDA_RNNOR ( XMEAN, SIGMA )

      END
