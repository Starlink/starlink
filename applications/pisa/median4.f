      SUBROUTINE MEDIAN4(XBUF,NPT,NFILT)
C     *** MEDIAN  performs median filtering on array xbuf

C  Changes:
C     6-NOV-1990 : P.W.Draper
C        Changed call to qsort to pqsort.
C     17-JUN-1995: P.W.Draper
C        Code transformed by TOOLPACK. INTEGER*2 references changed to
C        INTEGER*4.
C     .. Scalar Arguments ..
      INTEGER NFILT,NPT
C     ..
C     .. Array Arguments ..
      REAL XBUF(NPT)
C     ..
C     .. Local Scalars ..
      REAL XMNF,XMNS
      INTEGER I,IL,ILOW,J,JH,JL,L,NFO2P1
C     ..
C     .. Local Arrays ..
      REAL ARRAY(512),YBUF(8704)
      INTEGER*4 POINT(512)
C     ..
      IF (NFILT.GT.511) THEN
         STOP ' too large a filter'

      ELSEIF (NPT.GT.8192) THEN
         STOP ' too many points in data array'

      ELSE
         IF ((NFILT/2)*2.EQ.NFILT) NFILT = NFILT + 1
         NFO2P1 = NFILT/2 + 1
C     *** set first and last edges equal
         IL = NFILT/2
         ILOW = MAX0(3,NFILT/4)
         ILOW = (ILOW/2)*2 + 1
         DO 10 I = 1,ILOW
            ARRAY(I) = XBUF(I)
 10      CONTINUE
         CALL SORTM4(ARRAY,POINT,ILOW)
         XMNS = ARRAY(ILOW/2+1)
         DO 20 I = 1,ILOW
            ARRAY(I) = XBUF(NPT+1-I)
 20      CONTINUE
         CALL SORTM4(ARRAY,POINT,ILOW)
         XMNF = ARRAY(ILOW/2+1)
C     *** reflect edges before filtering
         DO 30 I = 1,IL
            YBUF(I) = 2.0*XMNS - XBUF(IL+ILOW+1-I)
            YBUF(NPT+I+IL) = 2.0*XMNF - XBUF(NPT-I-ILOW+1)
 30      CONTINUE
         DO 40 I = 1,NPT
            YBUF(I+IL) = XBUF(I)
 40      CONTINUE
C     *** do median filtering on rest
         DO 50 I = 1,NFILT
            ARRAY(I) = YBUF(I)
            POINT(I) = I
 50      CONTINUE
         CALL SORTM4(ARRAY,POINT,NFILT)
         XBUF(1) = ARRAY(NFO2P1)
         JL = NFILT + 1
         JH = NFILT + NPT - 1
         DO 60 J = JL,JH
            DO 70 I = 1,NFILT
               IF (POINT(I).EQ.1) THEN

                  POINT(I) = NFILT
                  ARRAY(I) = YBUF(J)
                  L = I

               ELSE
                  POINT(I) = POINT(I) - 1
               ENDIF

 70         CONTINUE
            CALL PQSORT4(ARRAY,POINT,L,NFILT)
            XBUF(J-JL+2) = ARRAY(NFO2P1)
 60      CONTINUE
      ENDIF

      END

