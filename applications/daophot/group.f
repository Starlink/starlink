      SUBROUTINE  GROUP (PAR, MAXPAR, PSF, MAXPSF, MAXEXP,
     .     ID, X, Y, MAG, SKY, ISIZE, NUMBER, INDEX, MAXSTR,
     .     FITRAD, PSFRAD)
C
C=======================================================================
C
C This subroutine accepts a file containing stellar coordinates, and
C associates the stars into natural groups based on the magnitude level
C at which they overlap:  if two stars are within a distance equal to
C one PSF radius plus one fitting radius plus one pixel of each other,
C the PSF of the brighter is evaluated at a point one fitting radius
C plus one pixel away from the fainter.  If this value is larger than
C some user-defined fraction of the anticipated noise per pixel, the
C two stars are put into the same group.
C
C Groups are written out into the disk file in order of increasing
C size.  (This is done to minimize the length of time the array
C processor must be attached to the program in NSTAR).
C
C             OFFICIAL DAO VERSION:  1991 April 18
C
C======================================================================
C
      IMPLICIT NONE

*  History:
*     17-Mar-1995 (GJP)
*     Replaced very negative numbers (-1E38) with VAL__MINR.

*  Global Constants:
      INCLUDE 'PRM_PAR'               ! PRIMDAT primitive data constants

      INTEGER MAXSTR, MAXPSF, MAXBOX, MAXPAR, MAXEXP
      PARAMETER  (MAXBOX=69)
C
C Parameter
C
C MAXSTR is the largest number of stars that may be contained in a data
C        file.
C
      CHARACTER*30 COOFIL, MAGFIL, PSFFIL, PROFIL, GRPFIL, SWITCH
      CHARACTER EXTEND*30, CASE*4
      REAL X(MAXSTR), Y(MAXSTR), MAG(MAXSTR), SKY(MAXSTR)
      REAL PSF(MAXPSF,MAXPSF,MAXEXP), DATA(MAXBOX,MAXBOX), PAR(MAXPAR)
      INTEGER ID(MAXSTR), INDEX(MAXSTR), RDPSF
      INTEGER ISIZE(MAXSTR), NUMBER(MAXSTR)
C
      REAL SQRT, USEPSF, ALOG10
C
      CHARACTER*4 PLSTR, PLGRP
      REAL HIBAD, THRESH, AP1, PHPADU, READNS, FRAD, PSFMAG, BRIGHT
      REAL XPSF, YPSF, CRIT, BRTMAG, DENOM, DX, DY, RSQ, FRSQ, VAL
      REAL DELTAX, DELTAY, DVDXC, DVDYC, FITRAD, PSFRAD, WT, FR
      REAL CRITSQ, CRITSEP, DXTEST, DYTEST, DXI, DYI, RATIO, ERR
      REAL LOBAD, MTEST, NUMER, XTEST, YTEST, STEST
      INTEGER I, J, II, IPSTYP, NPSF, NPAR, NEXP, NFRAC, LX, LY
      INTEGER NCOL, NROW, NL, JTEST, NGRP, NX, NY, IST, K, ISTAT
      INTEGER NTOT, ITEST, ITOP, N, IFIRST, MAXGRP, JTOP, LENGRP
C
      COMMON /FILNAM/ COOFIL, MAGFIL, PSFFIL, PROFIL, GRPFIL
C
C-----------------------------------------------------------------------
C
C SECTION 1
C
C Get set up.
C
C Open input photometry file.
C
      CALL TBLANK                                   ! Type a blank line
  950 CALL GETNAM ('File with photometry:', MAGFIL)
      IF ((MAGFIL .EQ. 'END OF FILE') .OR.
     .     (MAGFIL .EQ. 'GIVE UP')) THEN
         MAGFIL = ' '
         RETURN
      END IF
      CALL INFILE (2, MAGFIL, ISTAT)
      IF (ISTAT .NE. 0) THEN
         CALL STUPID ('Error opening input file '//MAGFIL)
         MAGFIL = 'GIVE UP'
         GO TO 950
      END IF
      CALL RDHEAD (2, NL, NCOL, NROW, LOBAD, HIBAD, THRESH, AP1,
     .     PHPADU, READNS, FRAD)
      IF (NL .LE. 0) NL=1
C
C Read the point-spread function into memory.
C
  960 CALL GETNAM ('File with the PSF:', PSFFIL)
      IF ((PSFFIL .EQ. 'END OF FILE') .OR.
     .     (PSFFIL .EQ. 'GIVE UP')) THEN
         CALL CLFILE (2)
         PSFFIL = ' '
         RETURN
      END IF
      ISTAT = RDPSF (PSFFIL, IPSTYP, PAR, MAXPAR, NPAR,
     .     PSF, MAXPSF, MAXEXP, NPSF, NEXP, NFRAC,
     .     PSFMAG, BRIGHT, XPSF, YPSF)
      IF (ISTAT .NE. 0) THEN
         PSFFIL = 'GIVE UP'
         GO TO 960
      END IF
C
C Obtain the critical overlap.
C
      CALL GETDAT ('Critical overlap:', CRIT, 1)
      IF (CRIT .LE. VAL__MINR) GO TO 9090
      CRIT=CRIT**2
C
C Open output group file.
C
      GRPFIL=SWITCH(MAGFIL, CASE('.grp'))
  970 CALL GETNAM ('File for stellar groups:', GRPFIL)
      IF ((GRPFIL .EQ. 'END OF FILE') .OR.
     .     (GRPFIL .EQ. 'GIVE UP')) THEN
         CALL CLFILE (2)
         RETURN
      END IF
      GRPFIL = EXTEND(GRPFIL, CASE('grp'))
      CALL OUTFIL (3, GRPFIL, ISTAT)
      IF (ISTAT .NE. 0) THEN
         CALL STUPID ('Error opening output file '//GRPFIL)
         GRPFIL = 'GIVE UP'
         GO TO 970
      END IF
      CALL WRHEAD (3, 3, NCOL, NROW, 6, LOBAD, HIBAD, THRESH, AP1,
     .     PHPADU, READNS, FRAD)
      READNS=READNS**2
C
C-----------------------------------------------------------------------
C
C SECTION 2
C
C Associate stars into natural groupings.
C
C Read in and count the stars.
C
      I=0
      BRTMAG=VAL__MAXR
      FRSQ=FITRAD**2
 2010 I=I+1
      IF (I .GT. MAXSTR) THEN
         CALL STUPID ('Too many stars in input file.')
         WRITE (6,6) MAXSTR
    6    FORMAT (I10, ' is the most you are currently allowed.')
         CALL STUPID ('Increase the MS parameter and try again')
         CALL CLFILE (2)
         CALL CLFILE (3)
         RETURN
      END IF
 2020 CALL RDSTAR (2, NL, ID(I), X(I), Y(I), MAG(I), SKY(I))
      IF (ID(I) .LT. 0) GO TO 2030             ! End-of-file encountered
      IF (ID(I) .EQ. 0) GO TO 2020             ! Blank line encountered
      ISIZE(I)=0                               ! Initialize ISIZE
      NUMBER(I)=0                              ! Initialize NUMBER
      IF (MAG(I) .GT. 90.) THEN                ! Null magnitude
         LX=INT(X(I)-FITRAD)+1
         LY=INT(Y(I)-FITRAD)+1
         NX=INT(X(I)+FITRAD)-LX+1
         NY=INT(Y(I)+FITRAD)-LY+1
         IF ((NX .LT. 1) .OR. (NY .LT. 1)) GO TO 2010
         CALL RDARAY ('DATA', LX, LY, NX, NY, MAXBOX, DATA, IST)
         IF ((NX .LT. 1) .OR. (NY .LT. 1)) GO TO 2010
         DELTAX=(X(I)-1.)/XPSF-1.
         DELTAY=(Y(I)-1.)/YPSF-1.
         DENOM=0.
         NUMER=0.
         DO K=1,NY
            DO J=1,NX
               DX=REAL(LX+J-1)-X(I)
               DY=REAL(LY+K-1)-Y(I)
               RSQ=DX**2+DY**2
               IF ((RSQ .LT. FRSQ) .AND. (DATA(J,K) .LE. HIBAD) .AND.
     .              (DATA(J,K) .GE. LOBAD)) THEN
                  VAL=USEPSF(IPSTYP, DX, DY, BRIGHT, PAR, PSF, NPSF,
     .                 NPAR, NEXP, NFRAC, DELTAX, DELTAY, DVDXC, DVDYC)
                  RSQ=RSQ/FRSQ
                  WT=5./(5.+RSQ/(1.-RSQ))
                  NUMER=NUMER+WT*VAL*(DATA(J,K)-SKY(I))
                  DENOM=DENOM+WT*VAL**2
               END IF
            END DO
         END DO
         IF ((DENOM .LE. 0.) .OR. (NUMER .LE. 0.)) GO TO 2010
         MAG(I)=PSFMAG-2.5*ALOG10(NUMER/DENOM)
      ELSE
         IF (MAG(I) .LT. BRTMAG) BRTMAG=MAG(I)
      END IF
      GO TO 2010
C
 2030 NTOT=I-1
      IF (NTOT .LE. 0) GO TO 9080
C
C All the stars have now been read in.
C NTOT is the number of stars in the file, which is also the
C theoretical maximum number of groups that can be found.
C
      CALL QUICK (Y, NTOT, INDEX)
C
C The vector of stellar y-values is now in increasing order.  ID, X,
C MAG, and SKY are still in the order in which they were read in.
C The array INDEX tells how they are to be cross-matched:  The i-th
C star in increasing y-order has ID(INDEX(i)), X(INDEX(i)), Y(i),
C MAG(INDEX(i)), and SKY(INDEX(i)).
C
      IF (BRTMAG .LT. 90.) THEN
         BRTMAG=10.**(0.4*(PSFMAG-BRTMAG))
      ELSE
         BRTMAG=1.
      END IF
C
C BRTMAG is the apparent magnitude of the brightest star in the
C input file.  (If all stars have apparent magnitudes greater than
C 90.0-- i.e. the aperture photometry blew up on all stars, BRTMAG
C is equal to the apparent magnitude of the point-spread function.)
C
      IF (NEXP+NFRAC .GE. 1)
     .     CALL SMTHPS (PSF, MAXPSF, MAXEXP, NPSF, NEXP+NFRAC)
C
C Compute the effective radius of the point-spread function, and
C add one fitting radius plus one pixel.
C
      FR=FITRAD+1.
      FRSQ=FR**2
      CRITSEP=AMIN1(PSFRAD, (REAL(NPSF-1)/2.-1.)/2. ) + FR
      CRITSQ=CRITSEP**2
C
C Now search the star list for stars lying within a critical distance of
C each other.  Stars are considered in order of increasing y-coordinate.
C
C Initialize the counters.  The variable N will count the
C number of stars in the current group.
C
      ITEST=0
      ITOP=2
      N=1
      IFIRST=1
      MAXGRP=0
C
C The stars are currently in a stack NTOT stars long.  The variable
C IFIRST points to the first star in the current group; this starts out,
C of course, with the value 1.  ITEST will point to the position in the
C stack occupied by the star which is currently the center of a circle
C of the critical radius, within which we are looking for other stars;
C this also starts out with a value of 1.  ITOP points to the top
C position in the stack of the stars which have not yet been assigned
C to groups; this starts out with the value 2.  Each time through, the
C program goes down through the stack from ITOP to NTOT and looks for
C stars within the critical distance from the star at stack position
C ITEST.  When such a star is found, it changes places in the stack
C with the star at ITOP and ITOP is incremented by one.  When the
C search has gotten to the last position in the stack (NTOT), the
C pointer ITEST is incremented by one, and the search proceeds again
C from the new value of ITOP to NTOT.  If the pointer ITEST catches up
C with the pointer ITOP, that means that the group currently being
C built up is complete.  The number of stars in the newly-created
C group (the first star of which is at stack position IFIRST) is stored
C in array element ISIZE(IFIRST).  Then a new group is started
C beginning with the star at the current position ITEST, ( = ITOP for
C the moment), ITOP is incremented by 1, and the next group is built
C up as before.
C
 2100 ITEST=ITEST+1
      IF (ITEST .LT. ITOP) GO TO 2110
C
C ITEST has reached ITOP; no other unassigned stars are within a
C critical separation of any member of the current group.  The group is
C therefore complete. Store N in ISIZE(IFIRST). Then start a new group
C with the star currently at ITEST ( = the old value of ITOP), and then
C increment the value of ITOP by one.
C
      ISIZE(IFIRST)=N
      IF (N .GT. MAXGRP) MAXGRP=N
      NUMBER(N)=NUMBER(N)+1
C
C The array NUMBER(i) builds up a histogram giving the number of
C groups of size i as a function of i.
C
      N=1                                   ! Re-initialize star counter
      IFIRST=ITEST
C
C If ITEST equals NTOT at this point, then we are finished (the last
C group contains one star).  Otherwise, set ITOP to the first star
C below ITEST and on with the search.
C
      IF (ITEST .GE. NTOT) GO TO 4000
      ITOP=ITOP+1                              ! Increment ITOP
      JTOP=INDEX(ITOP)
C
 2110 JTEST=INDEX(ITEST)
      XTEST=X(JTEST)
      YTEST=Y(ITEST)
      DXTEST=XTEST-XPSF
      DYTEST=YTEST-YPSF
C
      IF (MAG(JTEST) .GT. 90.) THEN
         MTEST=BRTMAG
      ELSE
         MTEST=10.**(0.4*(PSFMAG-MAG(JTEST)))
      END IF
C
      STEST=SKY(JTEST)
C
C Now go through the list of unassigned stars, occupying positions ITOP
C through NTOT in the stack, to look for stars within one critical
C distance of the star at position ITEST in the stack.  If one is found,
C compute the point-spread function of the brighter at a point one
C fitting radius plus one pixel from the fainter one along the line
C connecting them.  If this is larger than the input critical fraction
C of the total noise per pixel, move the new star up to stack position
C ITOP and increment ITOP by one.  Note:  if any star has an apparent
C magnitude greater than 90.0 (which means that the aperture
C photometry bombed), the brightest reasonable magnitude (= BRTMAG)
C will be assumed.
C
      II=ITOP
      DO 2120 I=II,NTOT
      DY=Y(I)-YTEST
      IF (DY .GT. CRITSEP) GO TO 2130
      J=INDEX(I)
      DX=X(J)-XTEST
      IF (ABS(DX) .GT. CRITSEP) GO TO 2120
      RSQ=DX**2+DY**2
      IF (RSQ .GT. CRITSQ) GO TO 2120
      DXI=X(J)-XPSF
      DYI=Y(I)-YPSF
C
C This star is within one critical separation of the star at stack
C position ITEST.  If their separation is less than one fitting radius
C plus one pixel put them in the same group no matter what.  Likewise,
C if the user entered a critical overlap less than or equal to zero,
C they belong in the same group no matter what.
C
      IF ((RSQ .LE. FRSQ) .OR. (CRIT .LE. 1.E-10)) GO TO 2115
C
C It is necessary to compute the PSF of the brighter star to determine
C whether the stars overlap by more than the critical amount.
C The two stars are already known to be separated by more than
C FR = (one fitting radius plus one pixel), and by less than one
C CRITSEP = (one PSF radius plus one fitting radius plus one pixel).
C Evaluate the point-spread function of the brighter star at a point
C FR pixels from the fainter, on the line segment connecting them.
C If this value is greater than CRIT times the standard error per
C pixel, then put them in the same group.
C
      IF (MAG(J) .GT. 90.) THEN
         VAL=BRTMAG
      ELSE
         VAL=10.**(0.4*(PSFMAG-MAG(J)))
      END IF
C
C For the time being, VAL is the brightness of the star currently
C being compared with the star at ITEST.
C
      RSQ=SQRT(RSQ)
      RATIO=(RSQ-FR)/RSQ
      DX=RATIO*DX
      DY=RATIO*DY
C
C Which is brighter?  What is the value of its PSF at a position DX, DY
C from the center of the fainter?
C
      IF (MTEST .GT. VAL) THEN
         VAL=MTEST*USEPSF(IPSTYP, DX, DY, BRIGHT, PAR, PSF, NPSF,
     .                 NPAR, NEXP, NFRAC, DELTAX, DELTAY, DVDXC, DVDYC)
      ELSE
         VAL=VAL*USEPSF(IPSTYP, DX, DY, BRIGHT, PAR, PSF, NPSF,
     .                 NPAR, NEXP, NFRAC, DELTAX, DELTAY, DVDXC, DVDYC)
      END IF
C
C Standard error per pixel.
C
      ERR=READNS+0.5*(STEST+SKY(J))/PHPADU
C
C If the point-spread function of the brighter star is less than
C fraction CRIT (input above) of the standard error per pixel, the
C two stars are considered NOT to overlap.
C
      IF (VAL**2 .LT. CRIT*ERR) GO TO 2120
C
C The two stars overlap.  Increment the group-size counter.
C
 2115 N=N+1
C
C Now move this star up to position ITOP in the stack, where the pointer
C ITEST may eventually reach it.
C
      CALL SWAP2 (I, ITOP, INDEX, Y)
C
C Now increment ITOP by 1 to point at the topmost unassigned star in the
C stack.
C
      ITOP=ITOP+1
      JTOP=INDEX(ITOP)
CD     TYPE *, 'ITEST=', ITEST, ' ITOP=', ITOP
CD     DO III=ITOP,ITOP+10
CD       TYPE *, III, ID(INDEX(III)), X(INDEX(III)), Y(III)
CD     END DO
CD     ACCEPT *
 2120 CONTINUE
C
C If ITOP is greater than NTOT, then all stars have been assigned to
C groups, and we are finished.  Otherwise, increment ITEST by 1 and
C keep going.
C
 2130 IF (ITOP .LE. NTOT) GO TO 2100
C
C-----------------------------------------------------------------------
C
C Normal completion.
C
C ITOP has exceeded NTOT.  That means that all the stars in the input
C file have been assigned to groups and have been written into the
C output file.  Now we may write out the groups, in order of
C increasing size, and close up shop.
C
 4000 ISIZE(IFIRST)=N
      NUMBER(N)=NUMBER(N)+1
      IF (N .GT. MAXGRP) MAXGRP=N
C
C Beginning of loop over group size.
C
      WRITE (6,640)
  640 FORMAT (/' Size of   Number of'/
     .         '  group     groups'/)
      NGRP=0
      DO 4040 I=1,MAXGRP
      IF (NUMBER(I) .LE. 0) GO TO 4040
C
C Now loop over actual groups, writing out any group whose size is
C equal to the current value of I.
C
      IFIRST=1
 4010 IF (ISIZE(IFIRST) .NE. I) GO TO 4030
      DO 4020 J=IFIRST,IFIRST+I-1
      K=INDEX(J)
 4020 WRITE (3,340) ID(K), X(K), Y(J), MAG(K), SKY(K)
  340 FORMAT (1X, I5, 4F9.3)
      NGRP=NGRP+1
      WRITE (3,340)                                 ! Write a blank line
 4030 IFIRST=IFIRST+ISIZE(IFIRST)
C
C End of loop over actual groups.
C
      IF (IFIRST .LE. NTOT) GO TO 4010
      WRITE (6,641) I, NUMBER(I)
  641 FORMAT (I6, I10)
C
C End of loop over group size.
C
 4040 CONTINUE
C
C Type out the number of stars and the number of groups NEATLY.
C
      PLSTR='s in'
      IF (NTOT .EQ. 1) PLSTR=' in '
      PLGRP='s.  '
      IF (NGRP .EQ. 1) PLGRP='.   '
      LENGRP=INT(ALOG10(NGRP+0.5))+2
      IF (NTOT .EQ. 1) LENGRP=LENGRP-1
      WRITE (6,643) NTOT, PLSTR, NGRP, PLGRP
  643 FORMAT (/I6, ' star', A4, I5, ' group', A4/)
 9080 CALL CLFILE (3)
 9090 CALL CLFILE (2)
      RETURN
      END!
C
C#######################################################################
C
      SUBROUTINE  SWAP2 (I, ITOP, INDEX, Y)
C
C=======================================================================
C
C Replace the ITOP-th star in the stack with the I-th star, then shift
C the intervening stars down the stack by one place.
C
C=======================================================================
C
      IMPLICIT NONE
      REAL Y(*)
      INTEGER INDEX(*)
C
      REAL YHOLD
      INTEGER I, J, K, L, ITOP, IHOLD
C
C-----------------------------------------------------------------------
C
      YHOLD=Y(I)
      IHOLD=INDEX(I)
      L=I+ITOP
      DO J=ITOP,I-1
         K=L-J
         Y(K)=Y(K-1)
         INDEX(K)=INDEX(K-1)
      END DO
      Y(ITOP)=YHOLD
      INDEX(ITOP)=IHOLD
      RETURN
      END!
C
C#######################################################################
C
      SUBROUTINE  SMTHPS (PSF, MAXPSF, MAXEXP, NPSF, NEXPND)
C
C Smooth the PSF azimuthally, so that noise will not influence whether
C or not stars belong in the same group.  Averages will be computed for
C four quadrants.
C
      IMPLICIT NONE

*  History:
*     17-Mar-1995 (GJP)
*     Replaced very negative numbers (-1E38) with VAL__MINR.

*  Global Constants:
      INCLUDE 'PRM_PAR'               ! PRIMDAT primitive data constants

      INTEGER MAXPSF, MAXEXP, MAXR, NSEC, IRMIN
      PARAMETER (MAXR=145, NSEC=4, IRMIN=4)
C
      REAL PSF(MAXPSF,MAXPSF,MAXEXP)
      REAL SUM(NSEC,MAXR), HIGH(NSEC,MAXR), LOW(NSEC,MAXR)
      INTEGER N(NSEC,MAXR)
C
      REAL REAL, SQRT
      INTEGER INT, ISCTR
C
      REAL RMAX, R, DYSQ
      INTEGER I, J, K, ICENTR, NPSF, IR, IS, NEXPND, IDX, IDY
C
      ICENTR=(NPSF+1)/2
      RMAX=0.7071068*REAL(NPSF-1)
C
      DO 3900 K=1,NEXPND
      DO IR=1,INT(RMAX+1.E-5)
         DO IS=1,NSEC
            SUM(IS,IR)=0.0
            HIGH(IS,IR)=VAL__MINR
            LOW(IS,IR)= VAL__MAXR
            N(IS,IR)=0
         END DO
      END DO
C
      DO 1900 J=1,NPSF
         IDY=J-ICENTR
         DYSQ=REAL(IDY**2)
         DO 1900 I=1,NPSF
            IDX=I-ICENTR
            R=SQRT(REAL(IDX**2)+DYSQ)
            IF (R .GT. RMAX) GO TO 1900
            IR=INT(R+1.E-5)
            IF (IR .LT. IRMIN) GO TO 1900
            IS=ISCTR(IDX,IDY)
            SUM(IS,IR)=SUM(IS,IR)+PSF(I,J,K)
            IF (PSF(I,J,K) .GT. HIGH(IS,IR)) HIGH(IS,IR)=PSF(I,J,K)
            IF (PSF(I,J,K) .LT. LOW(IS,IR)) LOW(IS,IR)=PSF(I,J,K)
            N(IS,IR)=N(IS,IR)+1
 1900 CONTINUE
C
      DO IR=IRMIN,INT(RMAX+1.E-5)
         DO IS=1,NSEC
            IF (N(IS,IR) .GT. 2)
     .           SUM(IS,IR)=(SUM(IS,IR)-HIGH(IS,IR)-LOW(IS,IR))
     .                         /REAL(N(IS,IR)-2)
         END DO
      END DO
C
      DO 2900 J=1,NPSF
         IDY=J-ICENTR
         DYSQ=REAL(IDY**2)
         DO 2900 I=1,NPSF
            IDX=I-ICENTR
            R=SQRT(REAL(IDX**2)+DYSQ)
            IF (R .GT. RMAX) GO TO 2900
            IR=INT(R+1.E-5)
            IF (IR .LT. IRMIN) GO TO 2900
            IS=ISCTR(IDX,IDY)
            PSF(I,J,K)=SUM(IS,IR)
 2900 CONTINUE
 3900 CONTINUE
C
      RETURN
      END!
C
C######################################################################
C
C Convert an x,y coordinate pair into a numbered sector from 1 to 4.
C
      FUNCTION ISCTR (I,J)
C
      IMPLICIT NONE
      INTEGER ISCTR, I, J
      IF (I .GT. 0) THEN
         ISCTR=1
      ELSE IF (I .LT. 0) THEN
         ISCTR=3
      ELSE
         IF (J .LE. 0) THEN
            ISCTR=1
         ELSE
            ISCTR=3
         END IF
      END IF
      IF (J .GT. 0) THEN
         ISCTR=ISCTR+1
      ELSE IF (J .EQ. 0) THEN
         IF (I .GT. 0) ISCTR=2
      END IF
      RETURN
      END!
