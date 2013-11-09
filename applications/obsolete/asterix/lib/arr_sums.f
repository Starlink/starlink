*+  ARR_SUMS - Arithmetic operations on REAL arrays
      SUBROUTINE ARR_SUMS(OPER,NA,A,NAV,AV,NB,B,NBV,BV,NC,C,NCV,CV)
*    Description :
*     Performs one of 4 arithmetic operations (+,-,*,/) specified in OPER
*     on the REAL arrays:
*           For I=1 to NC, C(I)=A(I) <OPER> B(I)
*     Note that NA and NB need not be equal but if they are not, then one
*     of them must be unity (case of operating on an array with a scalar).
*     Variances are handled correctly, if present.
*     Error functions used are:
*        + or - :    CV=(AV + BV)
*             * :    CV=(AV*B**2) + (BV*A**2)
*             / :    CV=(AV/B**2) + (BV*(A/B**2)**2)
*     Note: RETURNS without action if OPER is incorrectly specified.
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*     Jim Peden (BHVAD::JCMP)
*    History :
*     28 Jan 86:  original (BHVAD::JCMP)
*     27 Feb 90:  variance handling corrected (RJV)
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Import :
	CHARACTER OPER		! required operation
	INTEGER NA		! # of A elements
	REAL A(NA)		! input 1
	INTEGER NAV		! # of A variance elements
	REAL AV(*)		! may be of zero length
	INTEGER NB		! # of B elements
	REAL B(NB)		! input 2
	INTEGER NBV		! # of B variance elements
	REAL BV(*)		! may be of zero length
	INTEGER NC		! # C elements
	REAL C(NC)		! output
	INTEGER NCV		! # of C variance elements
*    Import-Export :
*    Export :
	REAL CV(*)		! may be of zero length
*    Status :
*    External references :
*    Global variables :
*    Local Constants :
*    Local variables :
	INTEGER I
*    Internal References :
*    Local data :
*-

* Select operation
	IF(OPER.EQ.'+') THEN
	   IF(NA.EQ.1.AND.NB.NE.1) THEN
	      DO I=1,NC
	         C(I)=A(1)+B(I)
	      ENDDO
	   ELSEIF(NA.NE.1.AND.NB.EQ.1) THEN
	      DO I=1,NC
	         C(I)=A(I)+B(1)
	      ENDDO
	   ELSE
	      DO I=1,NC
	         C(I)=A(I)+B(I)
	      ENDDO
	   ENDIF

	ELSEIF(OPER.EQ.'-') THEN
	   IF(NA.EQ.1.AND.NB.NE.1) THEN
	      DO I=1,NC
	         C(I)=A(1)-B(I)
	      ENDDO
	   ELSEIF(NA.NE.1.AND.NB.EQ.1) THEN
	      DO I=1,NC
	         C(I)=A(I)-B(1)
	      ENDDO
	   ELSE
	      DO I=1,NC
	         C(I)=A(I)-B(I)
	      ENDDO
	   ENDIF

	ELSEIF(OPER.EQ.'*') THEN
	   IF(NA.EQ.1.AND.NB.NE.1) THEN
	      DO I=1,NC
	         C(I)=A(1)*B(I)
	      ENDDO
	   ELSEIF(NA.NE.1.AND.NB.EQ.1) THEN
	      DO I=1,NC
	         C(I)=A(I)*B(1)
	      ENDDO
	   ELSE
	      DO I=1,NC
	         C(I)=A(I)*B(I)
	      ENDDO
	   ENDIF

	ELSEIF(OPER.EQ.'/') THEN
	   IF(NA.EQ.1.AND.NB.NE.1) THEN
	      DO I=1,NC
	         IF(B(I).NE.0.) THEN
	            C(I)=A(1)/B(I)
                 ELSE
                    C(I)=0.0
	         ENDIF
	      ENDDO
	   ELSEIF(NA.NE.1.AND.NB.EQ.1) THEN
	      IF(B(1).NE.0.) THEN
	         DO I=1,NC
	            C(I)=A(I)/B(1)
	         ENDDO
              ELSE
                 DO I=1,NC
                   C(I)=0.0
                 ENDDO
	      ENDIF
	   ELSE
	      DO I=1,NC
	         IF(B(I).NE.0.) THEN
	            C(I)=A(I)/B(I)
                 ELSE
                    C(I)=0.0
	         ENDIF
	      ENDDO
	   ENDIF

	ENDIF



* Deal with variances

	IF(NCV.NE.0) THEN

	   IF(OPER.EQ.'+'.OR.OPER.EQ.'-') THEN

*  value array  +-*/  value array  (value means has variance)
             IF     (NA.GT.1.AND.NAV.GT.1.AND.NB.GT.1.AND.NBV.GT.1) THEN

               DO I=1,NA
                 CV(I)=AV(I)+BV(I)
               ENDDO

*  constant array +-*/ value array
             ELSEIF (NA.GT.1.AND.NAV.EQ.0.AND.NB.GT.1.AND.NBV.GT.1) THEN

               DO I=1,NA
                 CV(I)=BV(I)
               ENDDO

*  value array +-*/ constant array
             ELSEIF (NA.GT.1.AND.NAV.GT.1.AND.NB.GT.1.AND.NBV.EQ.0) THEN

               DO I=1,NA
                 CV(I)=AV(I)
               ENDDO

*  scalar value +-*/ scalar value
             ELSEIF (NA.EQ.1.AND.NAV.EQ.1.AND.NB.EQ.1.AND.NBV.EQ.1) THEN

               CV(1)=AV(1)+BV(1)

*  scalar constant +-*/ scalar value
             ELSEIF (NA.EQ.1.AND.NAV.EQ.0.AND.NB.EQ.1.AND.NBV.EQ.1) THEN

               CV(1)=BV(1)

*  scalar value +-*/ scalar constant
             ELSEIF (NA.EQ.1.AND.NAV.EQ.1.AND.NB.EQ.1.AND.NBV.EQ.0) THEN

               CV(1)=AV(1)

*  scalar value +-*/ value array
             ELSEIF (NA.EQ.1.AND.NAV.EQ.1.AND.NB.GT.1.AND.NBV.GT.1) THEN

               DO I=1,NB
                 CV(I)=AV(1)+BV(I)
               ENDDO

*  value array +-*/ scalar value
             ELSEIF (NA.GT.1.AND.NAV.GT.1.AND.NB.EQ.1.AND.NBV.EQ.1) THEN

               DO I=1,NA
                 CV(I)=AV(I)+BV(1)
               ENDDO

*  scalar constant +-*/ value array
             ELSEIF (NA.EQ.1.AND.NAV.EQ.0.AND.NB.GT.1.AND.NBV.GT.1) THEN

               DO I=1,NB
                 CV(I)=BV(I)
               ENDDO

*  value array +-*/ scalar constant
             ELSEIF (NA.GT.1.AND.NAV.GT.1.AND.NB.EQ.1.AND.NBV.EQ.0) THEN

               DO I=1,NA
                 CV(I)=AV(I)
               ENDDO

*  scalar value +-*/ constant array
             ELSEIF (NA.EQ.1.AND.NAV.EQ.1.AND.NB.GT.1.AND.NBV.EQ.0) THEN

               DO I=1,NB
                 CV(I)=AV(1)
               ENDDO

*  constant array +-*/ scalar value
             ELSEIF (NA.GT.1.AND.NAV.EQ.0.AND.NB.EQ.1.AND.NBV.EQ.1) THEN

               DO I=1,NA
                 CV(I)=BV(1)
               ENDDO

             ENDIF


	   ELSEIF(OPER.EQ.'*') THEN

*  value array  +-*/  value array  (value means has variance)
             IF     (NA.GT.1.AND.NAV.GT.1.AND.NB.GT.1.AND.NBV.GT.1) THEN

               DO I=1,NA
                 CV(I)=AV(I)*B(I)**2 + BV(I)*A(I)**2
               ENDDO

*  constant array +-*/ value array
             ELSEIF (NA.GT.1.AND.NAV.EQ.0.AND.NB.GT.1.AND.NBV.GT.1) THEN

               DO I=1,NA
                 CV(I)=BV(I)*A(I)**2
               ENDDO

*  value array +-*/ constant array
             ELSEIF (NA.GT.1.AND.NAV.GT.1.AND.NB.GT.1.AND.NBV.EQ.0) THEN

               DO I=1,NA
                 CV(I)=AV(I)*B(I)**2
               ENDDO

*  scalar value +-*/ scalar value
             ELSEIF (NA.EQ.1.AND.NAV.EQ.1.AND.NB.EQ.1.AND.NBV.EQ.1) THEN

               CV(1)=AV(1)*B(1)**2 + BV(1)*A(1)**2

*  scalar constant +-*/ scalar value
             ELSEIF (NA.EQ.1.AND.NAV.EQ.0.AND.NB.EQ.1.AND.NBV.EQ.1) THEN

               CV(1)=BV(1)*A(1)**2

*  scalar value +-*/ scalar constant
             ELSEIF (NA.EQ.1.AND.NAV.EQ.1.AND.NB.EQ.1.AND.NBV.EQ.0) THEN

               CV(1)=AV(1)*B(1)**2

*  scalar value +-*/ value array
             ELSEIF (NA.EQ.1.AND.NAV.EQ.1.AND.NB.GT.1.AND.NBV.GT.1) THEN

               DO I=1,NB
                 CV(I)=AV(1)*B(I)**2 + BV(I)*A(1)**2
               ENDDO

*  value array +-*/ scalar value
             ELSEIF (NA.GT.1.AND.NAV.GT.1.AND.NB.EQ.1.AND.NBV.EQ.1) THEN

               DO I=1,NA
                 CV(I)=AV(I)*B(1)**2 + BV(1)*A(I)**2
               ENDDO

*  scalar constant +-*/ value array
             ELSEIF (NA.EQ.1.AND.NAV.EQ.0.AND.NB.GT.1.AND.NBV.GT.1) THEN

               DO I=1,NB
                 CV(I)=BV(I)*A(1)**2
               ENDDO

*  value array +-*/ scalar constant
             ELSEIF (NA.GT.1.AND.NAV.GT.1.AND.NB.EQ.1.AND.NBV.EQ.0) THEN

               DO I=1,NA
                 CV(I)=AV(I)*B(1)**2
               ENDDO

*  scalar value +-*/ constant array
             ELSEIF (NA.EQ.1.AND.NAV.EQ.1.AND.NB.GT.1.AND.NBV.EQ.0) THEN

               DO I=1,NB
                 CV(I)=AV(1)*B(I)**2
               ENDDO

*  constant array +-*/ scalar value
             ELSEIF (NA.GT.1.AND.NAV.EQ.0.AND.NB.EQ.1.AND.NBV.EQ.1) THEN

               DO I=1,NA
                 CV(I)=BV(1)*A(I)**2
               ENDDO

             ENDIF


	   ELSEIF(OPER.EQ.'/') THEN

*  value array  +-*/  value array  (value means has variance)
             IF     (NA.GT.1.AND.NAV.GT.1.AND.NB.GT.1.AND.NBV.GT.1) THEN

               DO I=1,NA
                 IF (B(I).NE.0.0) THEN
                   CV(I)=AV(I)/B(I)**2 + BV(I)*A(I)**2/B(I)**4
                 ELSE
                   CV(I)=0.0
                 ENDIF
               ENDDO

*  constant array +-*/ value array
             ELSEIF (NA.GT.1.AND.NAV.EQ.0.AND.NB.GT.1.AND.NBV.GT.1) THEN

               DO I=1,NA
                 IF (B(I).NE.0.0) THEN
                   CV(I)=BV(I)*A(I)**2/B(I)**4
                 ELSE
                   CV(I)=0.0
                 ENDIF
               ENDDO

*  value array +-*/ constant array
             ELSEIF (NA.GT.1.AND.NAV.GT.1.AND.NB.GT.1.AND.NBV.EQ.0) THEN

               DO I=1,NA
                 IF (B(I).NE.0.0) THEN
                   CV(I)=AV(I)/B(I)**2
                 ELSE
                   CV(I)=0.0
                 ENDIF
               ENDDO

*  scalar value +-*/ scalar value
             ELSEIF (NA.EQ.1.AND.NAV.EQ.1.AND.NB.EQ.1.AND.NBV.EQ.1) THEN

               IF (B(1).NE.0.0) THEN
                 CV(1)=AV(1)/B(1)**2 + BV(1)*A(1)**2/B(1)**4
               ELSE
                 CV(1)=0.0
               ENDIF

*  scalar constant +-*/ scalar value
             ELSEIF (NA.EQ.1.AND.NAV.EQ.0.AND.NB.EQ.1.AND.NBV.EQ.1) THEN

               IF (B(1).NE.0.0) THEN
                 CV(1)=BV(1)*A(1)**2/B(1)**4
               ELSE
                 CV(1)=0.0
               ENDIF

*  scalar value +-*/ scalar constant
             ELSEIF (NA.EQ.1.AND.NAV.EQ.1.AND.NB.EQ.1.AND.NBV.EQ.0) THEN

               IF (B(1).NE.0.0) THEN
                 CV(1)=AV(1)/B(1)**2
               ELSE
                 CV(1)=0.0
               ENDIF

*  scalar value +-*/ value array
             ELSEIF (NA.EQ.1.AND.NAV.EQ.1.AND.NB.GT.1.AND.NBV.GT.1) THEN

               DO I=1,NB
                 IF (B(I).NE.0.0) THEN
                   CV(I)=AV(1)/B(I)**2 + BV(I)*A(1)**2/B(I)**4
                 ELSE
                   CV(I)=0.0
                 ENDIF
               ENDDO

*  value array +-*/ scalar value
             ELSEIF (NA.GT.1.AND.NAV.GT.1.AND.NB.EQ.1.AND.NBV.EQ.1) THEN

               IF (B(1).NE.0.0) THEN
                 DO I=1,NA
                   CV(I)=AV(I)/B(1)**2 + BV(1)*A(I)**2/B(1)**4
                 ENDDO
               ELSE
                 DO I=1,NA
                   CV(I)=0.0
                 ENDDO
               ENDIF

*  scalar constant +-*/ value array
             ELSEIF (NA.EQ.1.AND.NAV.EQ.0.AND.NB.GT.1.AND.NBV.GT.1) THEN

               DO I=1,NB
                 IF (B(I).NE.0.0) THEN
                   CV(I)=BV(I)*A(1)**2/B(I)**4
                 ELSE
                   CV(I)=0.0
                 ENDIF
               ENDDO

*  value array +-*/ scalar constant
             ELSEIF (NA.GT.1.AND.NAV.GT.1.AND.NB.EQ.1.AND.NBV.EQ.0) THEN

               IF (B(1).NE.0.0) THEN
                 DO I=1,NA
                   CV(I)=AV(I)/B(1)**2
                 ENDDO
               ELSE
                 DO I=1,NA
                   CV(I)=0.0
                 ENDDO
               ENDIF

*  scalar value +-*/ constant array
             ELSEIF (NA.EQ.1.AND.NAV.EQ.1.AND.NB.GT.1.AND.NBV.EQ.0) THEN

               DO I=1,NB
                 IF (B(I).NE.0.0) THEN
                   CV(I)=AV(1)/B(I)**2
                 ELSE
                   CV(I)=0.0
                 ENDIF
               ENDDO

*  constant array +-*/ scalar value
             ELSEIF (NA.GT.1.AND.NAV.EQ.0.AND.NB.EQ.1.AND.NBV.EQ.1) THEN

               IF (B(1).NE.0.0) THEN
                 DO I=1,NA
                   CV(I)=BV(1)*A(I)**2/B(1)**4
                 ENDDO
               ELSE
                 DO I=1,NA
                   CV(I)=0.0
                 ENDDO
               ENDIF

             ENDIF


	   ENDIF

	ENDIF

	END
