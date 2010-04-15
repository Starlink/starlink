      SUBROUTINE EXTRACT(IMAGE,NPIX,NLINES,XO,YO,SKYRAD,CLEANRAD,
     >                   SKYSIZE,CLEANSIZE,SKYARRAY,CLEANARRAY)

*     ARGUMENTS

*     IMAGE (input)                   The image array
*     INTEGER*2 (NPIX,NLINES)

*     NPIX,NLINES (input)             The dimensions of the image
*     INTEGER

*     XO,YO (input)                   The accurate star position in integers
*     REAL

*     SKYRAD,CLEANRAD (input)         The halfsizes of the regions to be
*     INTEGER                         extracted

*     SKYSIZE,CLEANSIZE (input)       The full side lengths of these regions
*     INTEGER

*     SKYARRAY (output)               The extracted sky sub array
*     REAL (45,45)

*     CLEANARRAY (output)             The extracted sub array to be cleaned
*     REAL (45,45)

C     Dimension arrays

      REAL IMAGE(NPIX,NLINES)

      REAL*4    SKYARRAY(45,45),
     >          CLEANARRAY(45,45)

C     Declare other stuff

      INTEGER NPIX,NLINES,SKYRAD,CLEANRAD,SKYSIZE,CLEANSIZE,
     >        XLOW,XHIGH,YLOW,YHIGH,XCHECK1,XCHECK2,YCHECK1,
     >        YCHECK2,I,J,K,L
      REAL XO,YO

C     Now set up the sky square

      XLOW=XO-SKYRAD
      XHIGH=XO+SKYRAD
      YLOW=YO-SKYRAD
      YHIGH=YO+SKYRAD

C     Now check none of these values is out of bounds - if it
C     is, then slide the square along retaining the total area

      XCHECK1=NPIX-XHIGH
      XCHECK2=0+XLOW
      YCHECK1=NLINES-YHIGH
      YCHECK2=0+YLOW
      IF (XCHECK1.LT.0) THEN
         XLOW=XLOW-XCHECK1
         XHIGH=XHIGH-XCHECK1
         WRITE(6,20)XCHECK1
 20      FORMAT(' SKY SQUARE HAS BEEN SHIFTED LEFT BY ',I3)
      ENDIF
      IF (XCHECK2.LT.0) THEN
         XLOW=XLOW+XCHECK2
         XHIGH=XHIGH+XCHECK2
         WRITE(6,21)XCHECK2
 21      FORMAT(' SKY SQUARE HAS BEEN SHIFTED RIGHT BY ',I3)
      ENDIF
      IF (YCHECK1.LT.0) THEN
         YLOW=YLOW-YCHECK1
         YHIGH=YHIGH-YCHECK1
         WRITE(6,22)YCHECK1
 22      FORMAT(' SKY SQUARE HAS BEEN SHIFTED DOWN BY ',I3)
      ENDIF
      IF (YCHECK2.LT.0) THEN
         YLOW=YLOW+YCHECK2
         YHIGH=YHIGH+YCHECK2
         WRITE(6,23)YCHECK2
 23      FORMAT(' SKY SQUARE HAS BEEN SHIFTED UP BY ',I3)
      ENDIF

C     Having checked and if necessary shifted the square we now
C     extract it into the small array SKYARRAY.

      DO J=1,NLINES
         DO I=1,NPIX
            K=I-XLOW+1
            L=J-YLOW+1
            IF (I.GE.XLOW) THEN
            IF (I.LE.XHIGH) THEN
            IF (J.GE.YLOW) THEN
            IF (J.LE.YHIGH) THEN
               SKYARRAY(K,L)=IMAGE(I,J)
            ENDIF
            ENDIF
            ENDIF
            ENDIF
         ENDDO
      ENDDO
      WRITE(6,'(A)')' SKY SQUARE HAS BEEN EXTRACTED '
      WRITE(6,'(A)')'      '

C     Now set up square to be cleaned

      XLOW=XO-CLEANRAD
      XHIGH=XO+CLEANRAD
      YLOW=YO-CLEANRAD
      YHIGH=YO+CLEANRAD

C     Now check none of these values is out of bounds - if it
C     is, then slide the square along retaining the total area

      XCHECK1=NPIX-XHIGH
      XCHECK2=0+XLOW
      YCHECK1=NLINES-YHIGH
      YCHECK2=0+YLOW
      IF (XCHECK1.LT.0) THEN
         XLOW=XLOW-XCHECK1
         XHIGH=XHIGH-XCHECK1
         WRITE(6,30)XCHECK1
 30      FORMAT(' CLEAN SQUARE HAS BEEN SHIFTED LEFT BY ',I3)
      ENDIF
      IF (XCHECK2.LT.0) THEN
         XLOW=XLOW+XCHECK2
         XHIGH=XHIGH+XCHECK2
         WRITE(6,31)XCHECK2
 31      FORMAT(' CLEAN SQUARE HAS BEEN SHIFTED RIGHT BY ',I3)
      ENDIF
      IF (YCHECK1.LT.0) THEN
         YLOW=YLOW-YCHECK1
         YHIGH=YHIGH-YCHECK1
         WRITE(6,32)YCHECK1
 32      FORMAT(' CLEAN SQUARE HAS BEEN SHIFTED DOWN BY ',I3)
      ENDIF
      IF (YCHECK2.LT.0) THEN
         YLOW=YLOW+YCHECK2
         YHIGH=YHIGH+YCHECK2
         WRITE(6,33)YCHECK2
 33      FORMAT(' CLEAN SQUARE HAS BEEN SHIFTED UP BY ',I3)
      ENDIF

C     Having checked and if necessary shifted the square we now
C     extract it into the small array CLEANARRAY.

      DO J=1,NLINES
         DO I=1,NPIX
            K=I-XLOW+1
            L=J-YLOW+1
            IF (I.GE.XLOW) THEN
            IF (I.LE.XHIGH) THEN
            IF (J.GE.YLOW) THEN
            IF (J.LE.YHIGH) THEN
               CLEANARRAY(K,L)=IMAGE(I,J)
            ENDIF
            ENDIF
            ENDIF
            ENDIF
         ENDDO
      ENDDO
      WRITE(6,'(A)')' CLEAN SQUARE HAS BEEN EXTRACTED '
      WRITE(6,'(A)')'         '


      RETURN
      END
