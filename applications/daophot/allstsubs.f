      SUBROUTINE STRIP (ID, X, Y, MAG, SKY, SKIP, MAXSTR,
     .     NSTAR, NDISAP, RADIUS, INDEX, HOLD)
      REAL X(MAXSTR), Y(MAXSTR), MAG(MAXSTR), SKY(MAXSTR)
      REAL HOLD(MAXSTR)
      INTEGER ID(MAXSTR), INDEX(MAXSTR)
      LOGICAL SKIP(MAXSTR)
C
      NDISAP = 0
      RADSQ = RADIUS**2
      IF (NSTAR .LE. 1) RETURN
      DO I=1,NSTAR
         SKIP(I) = .FALSE.
      END DO
C
      CALL QUICK (Y, NSTAR, INDEX)
      CALL RECTFY (ID, NSTAR, INDEX, HOLD)
      CALL RECTFY (X, NSTAR, INDEX, HOLD)
      CALL RECTFY (MAG, NSTAR, INDEX, HOLD)
      CALL RECTFY (SKY, NSTAR, INDEX, HOLD)
C
      DO 1200 I=1,NSTAR-1
         IF (SKIP(I)) GO TO 1200
         DO 1100 J=I+1,NSTAR
            IF (SKIP(J)) GO TO 1100
            DY = Y(J)-Y(I)
            IF (DY .GT. RADIUS) GO TO 1200
            DX = X(J)-X(I)
            IF (ABS(DX) .GT. RADIUS) GO TO 1100
            IF (DX**2+DY**2 .GT. RADSQ) GO TO 1100
            IF (MAG(J) .LE. MAG(I)) THEN
               SKIP(J) = .TRUE.
               GO TO 1100
            ELSE
               SKIP(I) = .TRUE.
               GO TO 1200
            END IF
 1100    CONTINUE
 1200 CONTINUE
C
      ISTAR = 0
 2000 CONTINUE
      IF (SKIP(NSTAR)) THEN
         NSTAR = NSTAR-1
         NDISAP = NDISAP+1
         GO TO 2000
      END IF
 2100 ISTAR = ISTAR+1
      IF (ISTAR .GE. NSTAR) RETURN
      IF (SKIP(ISTAR)) THEN
         ID(ISTAR) = ID(NSTAR)
         X(ISTAR) = X(NSTAR)
         Y(ISTAR) = Y(NSTAR)
         MAG(ISTAR) = MAG(NSTAR)
         SKY(ISTAR) = SKY(NSTAR)
         SKIP(ISTAR) = .FALSE.
         NSTAR = NSTAR-1
         NDISAP = NDISAP+1
         GO TO 2000
      ELSE
         GO TO 2100
      END IF
      END
C
C#######################################################################
C
      SUBROUTINE  REGRP (ID, X, Y, MAG, SKY, CHI, DXOLD, DYOLD,
     .     XCLAMP, YCLAMP, MAXSTR, NSTAR, FITRAD, LAST, INDEX, HOLD)
C
C=======================================================================
C
C This subroutine accepts a list of stellar coordinates, and
C associates the stars into natural groups based on a critical
C separation:  stars within one critical separation of each other are
C put into the same group; no star is within one critical separation of
C any star outside its group.
C
C             OFFICIAL DAO VERSION:  1985 August 15
C
C======================================================================
C
      REAL X(MAXSTR), Y(MAXSTR), MAG(MAXSTR), SKY(MAXSTR)
      REAL CHI(MAXSTR), DXOLD(MAXSTR), DYOLD(MAXSTR)
      REAL XCLAMP(MAXSTR), YCLAMP(MAXSTR), HOLD(MAXSTR)
      INTEGER ID(MAXSTR), INDEX(MAXSTR)
      LOGICAL LAST(MAXSTR)
C
C-----------------------------------------------------------------------
C
C SECTION 1
C
C Get set up.
C
      IF (NSTAR .LE. 1) RETURN
      CRIT=2.*FITRAD
      CRITSQ=CRIT**2
C
C Check that the stars are sorted by y-coordinate on input.
C
      INDEX(1) = 1
      DO I=2,NSTAR
         IF (Y(I) .GE. Y(I-1)) THEN
            INDEX(I) = I
         ELSE
            CALL QUICK (Y, NSTAR, INDEX)
            CALL RECTFY (X, NSTAR, INDEX, HOLD)
         END IF
      END DO
C
      ITEST=0
      ITOP=2
C
C The stars are currently in a stack NSTAR stars long.  The variable
C ITEST will point to the position in the stack occupied by the star
C which is currently the center of a circle of the critical radius,
C within which we are looking for other stars; this also starts out
C with a value of 1.  ITOP points to the top position in the stack of
C the stars which have not yet been assigned to groups; this starts
C out with the value 2.  Each time through, the
C program goes down through the stack from ITOP and looks for stars
C within the critical distance from the star at stack position
C ITEST.  When such a star is found, it changes places in the stack
C with the star at ITOP and ITOP is incremented by one.  When the
C search reaches a star J such that Y(J)-Y(ITEST) > CRIT it is known
C that no further stars will be found within the critical distance, the
C pointer ITEST is incremented by one, and the search proceeds again
C from the new value of ITOP.  If the pointer ITEST catches up
C with the pointer ITOP, that means that the group currently being
C built up is complete.  Then a new group is started beginning with
C the star at the current position ITEST, ( = the instantaneous
C value of ITOP), ITOP is incremented by 1, and the next group is built
C up as before.
C
 2100 ITEST=ITEST+1
      LAST(ITEST)=.FALSE.
      IF (ITEST .EQ. ITOP) THEN
C
C ITEST has reached ITOP; no other unassigned stars are within a
C critical separation of any member of the current group.  The group is
C therefore complete.  Signify this by setting LAST(ITEST-1)=.TRUE.
C (ITEST = the current value of ITOP), and then increment the value of
C ITOP by one.
C
         J=ITEST-1
         IF (J .GT. 0) LAST(J)=.TRUE.
         ITOP=ITOP+1                              ! Increment ITOP
C
C If ITOP is greater than NSTAR at this point, then we are finished
C (the last group contains one star).  Otherwise, on with the search.
C
         IF (ITOP .GT. NSTAR) THEN
            LAST(ITEST)=.TRUE.
            GO TO 3000
         END IF
      END IF
C
C Now go through the list of unassigned stars, occupying positions ITOP
C through NSTAR in the stack, to look for stars within the critical
C distance of the star at position ITEST in the stack.  If one is found,
C move it up to stack position ITOP and increment ITOP by one.
C
      XTEST=X(ITEST)
      YTEST=Y(ITEST)
      J=ITOP
      DO 2120 I=J,NSTAR
         DY=Y(I)-YTEST
         IF (DY .GT. CRIT) GO TO 2100
         DX=X(I)-XTEST
         IF (ABS(DX) .GT. CRIT) GO TO 2120
         IF (DX**2+DY**2 .GT. CRITSQ) GO TO 2120
C
C This star is within the critical distance of the star at stack
C position ITEST.  Therefore it should be added to the current group by
C moving it up to position ITOP in the stack, where the pointer ITEST
C may eventually reach it.
C
         CALL ASWAP (MAXSTR, ITOP, I, X, Y, INDEX)
C
C Now increment ITOP by 1 to point at the topmost unassigned star in the
C stack.
C
         ITOP=ITOP+1
C
C If ITOP is greater than NSTAR, then all stars have been assigned to
C groups, and we are finished.
C
         IF (ITOP .GT. NSTAR) THEN
            DO K=ITEST,NSTAR-1
               LAST(K)=.FALSE.
            END DO
            LAST(NSTAR)=.TRUE.
            GO TO 3000
         END IF
 2120 CONTINUE
      GO TO 2100
C
 3000 CONTINUE
C
C Rectify the remaining quantities.
C
      CALL RECTFY (ID, NSTAR, INDEX, HOLD)
      CALL RECTFY (MAG, NSTAR, INDEX, HOLD)
      CALL RECTFY (SKY, NSTAR, INDEX, HOLD)
      CALL RECTFY (CHI, NSTAR, INDEX, HOLD)
      CALL RECTFY (DXOLD, NSTAR, INDEX, HOLD)
      CALL RECTFY (DYOLD, NSTAR, INDEX, HOLD)
      CALL RECTFY (XCLAMP, NSTAR, INDEX, HOLD)
      CALL RECTFY (YCLAMP, NSTAR, INDEX, HOLD)
C
      RETURN
      END
C
C#######################################################################
C
      SUBROUTINE  ASWAP (MAXSTR, I, J, X, Y, INDEX)
C
C=======================================================================
C
C Make the I-th and J-th stars in the stack change places (J > I),
C without otherwise altering the order of the stars.  The other
C arguments are self-evident.
C
C=======================================================================
C
      IMPLICIT NONE
      INTEGER MAXSTR
C
      REAL X(MAXSTR), Y(MAXSTR)
      INTEGER INDEX(MAXSTR)
C
      REAL XHOLD, YHOLD
      INTEGER I, J, K, L, IHOLD
C
C-----------------------------------------------------------------------
C
c     call ovrwrt ('ASWAP', 2)
      XHOLD=X(J)
      YHOLD=Y(J)
      IHOLD=INDEX(J)
      DO K=J,I+1,-1
         L=K-1
         X(K)=X(L)
         Y(K)=Y(L)
         INDEX(K)=INDEX(L)
      END DO
      X(I)=XHOLD
      Y(I)=YHOLD
      INDEX(I)=IHOLD
      RETURN
      END
