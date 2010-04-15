      SUBROUTINE SEARCH(SMARGE,NS,KK,XO,SX,JX,LX,ICROWD)
*+
*   SEARCH
*
*   acquire a star from a marginal distribution.
*
*   Given      (arguments)
*   SMARGE   RA marginal vector.
*   NS       I  dimension of vector.
*   KK       I  number of points to be ignored at each end
*               of marginal vector (greater than or equal to 2).
*
*   Returned   (arguments)
*   XO       R  output estimate of image center.
*   SX       R  output estimate of image width.
*   JX       I  low-side local minimum (stepped in by one).
*   LX       I  high-side local minimum (stepped in by one).
*   ICROWD   I  0 if no other star detected.
*               -1 if brighter star found on low side.
*               +1 if brighter star found on high side.
*
*   B.D.Kelly/ROE/1981
*-
      INTEGER NS,KK,JX,LX,ICROWD
      REAL SMARGE(NS),DSMARGE(512),DRMAX,DRMIN,S

      IMAX = 0
      IMIN = 0

      ICROWD=0
      IBEG=KK+1
      IEND=NS-KK
      N=NS-2
      DRMAX=0.0
      DRMIN=0.0
*
*   DSMARGE=numerical derivative
*
      DO I=IBEG,IEND
        DSMARGE(I)=SMARGE(I+2)-SMARGE(I-2)+2.0*(SMARGE(I+1)-SMARGE(I-1))
        IF(DSMARGE(I).GE.DRMAX) THEN
          DRMAX=DSMARGE(I)
          IMAX=I
        ENDIF
        IF(DSMARGE(I).LE.DRMIN) THEN
          DRMIN=DSMARGE(I)
          IMIN=I
        ENDIF
      ENDDO

*
*   crowded?
*
      IF(IMIN.LE.IMAX) THEN
*
*   which extremum is nearer the edge of the array?
*
        IF((NS-IMAX).GE.(IMIN-1)) THEN
*
*   find new minimum derivative to right of maximum derivative.
*
          ICROWD=-1
          DRMIN=DRMAX
          J=IMAX+1
          DO I=J,IEND
            IF(DSMARGE(I).LE.DRMIN) THEN
              DRMIN=DSMARGE(I)
              IMIN=I
            ENDIF
          ENDDO
        ELSE
*
*   find new maximum derivative to left of minimum derivative.
*
          ICROWD=1
          DRMAX=DRMIN
          J=IMIN-1
          DO I=IBEG,J
            IF(DSMARGE(I).GE.DRMAX) THEN
              DRMAX=DSMARGE(I)
              IMAX=I
            ENDIF
          ENDDO
        ENDIF
      ENDIF
*
*   compute estimates of image centre and width
*
      S=0.0
      SX=IMIN-IMAX
      DO I=IMAX,IMIN
        S=S+DSMARGE(I)
      ENDDO
      XO=0.5*(IMAX+IMIN)+S*SX/((SX+1.0)*(DRMAX-DRMIN))
      SX=0.5*SX
      J=NINT(XO)
*
*   find low- (left-) side local minimum.
*
      L=J-2
      JX=1
 311  L=L-1
      GO TO (402,312,313,314,315),L
 315  IF(SMARGE(L).GT.SMARGE(L-4))GO TO 311
 314  IF(SMARGE(L).GT.SMARGE(L-3))GO TO 311
 313  IF(SMARGE(L).GT.SMARGE(L-2))GO TO 311
 312  IF(SMARGE(L).GT.SMARGE(L-1))GO TO 311
      JX=L+1
*
*   find high- (right-) side local minimum.
*
 402  L=J+2
      LX=NS
      M=NS+1
 321  L=L+1
      GO TO (403,322,323,324,325),M-L
 325  IF(SMARGE(L).GT.SMARGE(L+4))GO TO 321
 324  IF(SMARGE(L).GT.SMARGE(L+3))GO TO 321
 323  IF(SMARGE(L).GT.SMARGE(L+2))GO TO 321
 322  IF(SMARGE(L).GT.SMARGE(L+1))GO TO 321
      LX=L-1
 403  RETURN
      END
