*+BINEXPS - Find the exposure of light curve time bins.
      SUBROUTINE BINEXPS(RC, CC, CV, CQ, BEXPS, BVAR, BQUAL, NB,STATUS)
      IMPLICIT NONE
* Input
      INTEGER	NB				! # bins.
      REAL      RC(NB)				! Raw counts
      REAL      CC(NB)				! Corrected counts
      REAL      CV(NB)				! Variance of corrected counts
      INTEGER   CQ(NB)				! Quality of corrected counts

* Output:
      REAL      BEXPS(NB)			! Time bin exposure.
      REAL      BVAR(NB)			! Time bin variance of exposure
      INTEGER   BQUAL(NB)			! Time bin quality.
      INTEGER   STATUS

* P. McGale - Aug 94.
*-

*    Local variables :
      INTEGER	I

      IF (STATUS .NE. 0) RETURN

*   Loop over the data
      DO I = 1, NB
	IF (CQ(I) .EQ. 0) THEN
	  BQUAL(I) = 0
	  BVAR(I)  = 0.0
	  IF (CC(I) .GT. 0.0) THEN
	    BEXPS(I) = RC(I) / CC(I)
	  ELSE
	    BEXPS(I) = SQRT(1./CV(I))
	  ENDIF
        ELSE
	  BEXPS(I) = 0.0
	  BQUAL(I) = 1
	  BVAR(I)  = 0.0
        ENDIF
      ENDDO

      END
