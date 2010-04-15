	SUBROUTINE HOTSHOTSUB( NXI, NYI, IMAGE_IN, WHOLE, IXCEN, IYCEN, IXSZ,
     :	                       IYSZ, BINX, BINY, SIGMA, THRESH, SETWHAT,
     :	                       BADVAL, IMAGE_OUT, MAXNUMHOT, STATUS)

*       HISTORY
*       14-JUL-1994 Changed TYPE statements to MSG_OUT (SKL@JACH)

	IMPLICIT NONE

	INTEGER IXCEN
	INTEGER IYCEN
	INTEGER IXSZ
	INTEGER IYSZ
	INTEGER IXST
	INTEGER IYST
	INTEGER IXEN
	INTEGER IYEN
	INTEGER BINX
	INTEGER BINY
	INTEGER IEN1
	INTEGER IEN2
	INTEGER IST1
	INTEGER IST2
	INTEGER I
	INTEGER J
	INTEGER K
	INTEGER L
	INTEGER NXI
	INTEGER NYI
	INTEGER NUMGOOD
	INTEGER NUMHOT
	INTEGER MAXNUMHOT
	INTEGER STATUS
	INTEGER NITER
        INTEGER OLDHOT
	INTEGER TOLL

	REAL IMAGE_IN( NXI, NYI)
	REAL IMAGE_OUT( NXI, NYI)
	REAL SIGMA, THRESH, BADVAL

	CHARACTER*(*) SETWHAT

	LOGICAL WHOLE
	LOGICAL MORE

	REAL*8 SUMUP
	REAL*8 SUMUP2
	REAL*8 STD, MEANY

*      set the output image to zero
	DO J = 1, NYI
	  DO K = 1, NXI
	    IMAGE_OUT( K, J) = IMAGE_IN( K, J)
	  END DO
	END DO

*      define start of blocking
	IF( WHOLE ) THEN
	  IXST = 1
	  IYST = 1
	  IXEN = NXI
	  IYEN = NYI
	  IXSZ = NXI
	  IYSZ = NYI
	ELSE
	  IXST = IXCEN-INT(IXSZ/2.0)
	  IYST = IYCEN-INT(IYSZ/2.0)
	  IXEN = IXST+IXSZ
	  IYEN = IYST+IYSZ
	END IF
	NUMHOT = 0
	NITER = 0

*      loop for y scan of output image
!	type *, 'ixst,iyst,ixen,iyen = ', ixst, iyst, ixen, iyen
!	type *, 'nxi, nyi = ', nxi, nyi
!	type *, 'binx,biny = ', binx, biny
!	type *, 'thresh = ', thresh
!	type *, 'scan area y = ', IYST, (IYST+(BINY*INT(REAL(IYSZ)/REAL(BINY)))-1)
!	type *, 'scan area x = ', IXST, (IXST+(BINX*INT(REAL(IXSZ)/REAL(BINX)))-1)
!	type *, 'Setwhat, badval = ', setwhat( 1:1), badval
	MORE = .TRUE.
        OLDHOT = 1E5
	DO WHILE ( MORE)
	  IST2 = IYST
	  IEN2 = IST2+BINY-1
	  NITER = NITER + 1
	  DO I = IYST, (IYST+(BINY*INT(REAL(IYSZ)/REAL(BINY)))-1)
	    IST1 = IXST
	    IEN1 = IST1+BINX-1

*          loop for x scan of output image
	    DO J = IXST, (IXST+(BINX*INT(REAL(IXSZ)/REAL(BINX)))-1)
	      SUMUP = 0.0D0
	      SUMUP2 = 0.0D0

*            loops to scan through the input image pixels/output pixel
!	if( ist1 .eq. 1) type *, 'Row = ', i, ist1, ien1, ist2, ien2
	      NUMGOOD = 0
	      DO K = IST2, MIN( IEN2, NYI)
	        DO L = IST1, MIN( IEN1, NXI)
	          IF( SETWHAT( 1:1) .EQ. 'B') THEN
	            IF( ( IMAGE_OUT( L, K) .NE. BADVAL)) THEN
	              IF( L .EQ. J .AND. K .EQ. I) THEN
	              ELSE
	                NUMGOOD = NUMGOOD + 1
	                SUMUP = SUMUP+DBLE( IMAGE_OUT( L, K))
	                SUMUP2 = SUMUP2 + DBLE( IMAGE_OUT( L, K)**2)
	              END IF
	            END IF
	          ELSE
	            IF( L .EQ. J .AND. K .EQ. I) THEN
	            ELSE
	              NUMGOOD = NUMGOOD + 1
	              SUMUP = SUMUP+DBLE( IMAGE_OUT( L, K))
	              SUMUP2 = SUMUP2 + DBLE( IMAGE_OUT( L, K))**2
	            END IF
	          END IF
	        END DO
	      END DO
	      IF( NUMGOOD .GT. 0) THEN
	        MEANY = SUMUP/NUMGOOD
	      ELSE
	        MEANY = 1.0E10
	      END IF
	      STD = SUMUP2 - (NUMGOOD)*MEANY**2
	      IF( STD .GE. 0.0D0 .AND. (NUMGOOD-1) .GT. 0) THEN
	        STD = SQRT( SNGL(STD)/REAL(NUMGOOD-1))
	      ELSE
!	        type *, 'BAD STD, negative value or number good pixels = 1'
!	        type *, j, i, sngl(meany), sngl(std), numgood
	        STD = 1.0D35
	      END IF

	      IF ( SNGL( STD) .LE. THRESH) THEN
	        IF( IMAGE_OUT( J, I) .GT. SNGL(MEANY+SIGMA*STD) .OR.
     :	            IMAGE_OUT( J, I) .LT. SNGL(MEANY-SIGMA*STD)) THEN
	          NUMHOT = NUMHOT + 1
	          IF( SETWHAT( 1:1) .EQ. 'B') THEN
	            IMAGE_OUT( J, I) = BADVAL
	          ELSE
	            IMAGE_OUT( J, I) = SNGL(MEANY)
	          END IF
	        END IF
	      END IF

*            increment the X input pixel scanning variables
	      IST1 = IST1 + 1
	      IEN1 = IEN1 + 1
	    END DO

*          increment the Y input pixel scanning variables
	    IST2 = IST2 + 1
	    IEN2 = IEN2 + 1
	  END DO
          CALL MSG_SETI( 'NITER', NITER )
          CALL MSG_SETI( 'NUMHOT', NUMHOT )
          CALL MSG_OUT( 'MSG',
     :        'Iteration no. ^NITER, NUMHOT = ^NUMHOT', STATUS )
	  IF( NITER .EQ. 1) THEN
	    TOLL = IFIX( NUMHOT/10.0)
            CALL MSG_SETI( 'TOLL', TOLL )
            CALL MSG_OUT( 'MSG',
     :	  'terminate when NUMHOT<^TOLL, when = 0, or if diverge',
     :                    STATUS )
	    MAXNUMHOT = NUMHOT
	  END IF
	  IF( OLDHOT .LT. NUMHOT) THEN
	    CALL MSG_OUT( 'MSG', 'Interations diverging, terminating',
     :	      STATUS)
	    MORE = .FALSE.
	  ELSE
	    OLDHOT = NUMHOT
	  END IF
	  IF( NUMHOT .LT. TOLL .OR. NUMHOT .EQ. 0) MORE = .FALSE.
	  IF( NITER .GT. 50) MORE = .FALSE.
	  NUMHOT = 0
	END DO

	END
