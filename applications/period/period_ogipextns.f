
      SUBROUTINE PERIOD_OGIPEXTNS(XI, YI, HDT, OKAY, HDC, HDCC,
     :                            HDC2, EXTN, HEAD1, EXTENS, UNIT)

C=============================================================================
C Obtain certain property values for useer-specified FITS file extension.
C
C Written by Kevin P Duffey @RAL, October 2001
C
C Converted to Double Precision (KPD), August 2001
C Modified to incorporate dynamic memory allocation for major
C  data/work array(s) and/or use of such arrays (KPD), October 2001
C=============================================================================

      IMPLICIT NONE

      INTEGER EXTENS, UNIT
      INTEGER XI(EXTENS), YI(EXTENS)
      INTEGER HDT(EXTENS), OKAY(EXTENS)
      DOUBLE PRECISION DVAL
      CHARACTER*40 HDC(EXTENS), HDCC(EXTENS)
      CHARACTER*40 HDC2(EXTENS), EXTN(EXTENS)
      CHARACTER*20 HEAD1(3)
      INTEGER FAIL, SOFAR, I
      INTEGER STATUS, IVAL, HDUTYPE, EXTENSO
      CHARACTER*40 STRING2
      CHARACTER*72 JUNK
      CHARACTER*80 COMMENT, STRING


      EXTENSO = 0

*      Look through/for extensions.
      DO 10 I = 1, EXTENS

*      Set default values
         XI(I)=0
         YI(I)=0
         HDCC(I)=' Unknown'
         EXTN(I)=' Unknown'
         HDC(I)= ' Unknown'
         HDC2(I)=' Unknown'
         HDT(I)=3
         OKAY(I)=1

*         Set default values
         FAIL=1

*         Jump to the required extension.
*         Store the extension type
         STATUS=0
         HDUTYPE=3
         CALL FTMAHD(UNIT,I,HDUTYPE,STATUS)
         IF(STATUS.NE.0) FAIL=1
         IF(HDUTYPE.EQ.0) HDUTYPE=3
         IF(HDUTYPE.EQ.3) FAIL=1
         HDT(I)=HDUTYPE

*         Continue if the extension was found.
         IF(STATUS.EQ.0) THEN

*         Set failure flag.
            FAIL=0

*         Determine the size of the file in X records.
            STATUS=0
            IVAL=0
            CALL FTGKYT(UNIT,'TFIELDS',IVAL,DVAL,COMMENT,STATUS)
            XI(I)=IVAL
            IF(STATUS.NE.0) FAIL=1
            IF(IVAL.EQ.0) FAIL=1

*         Determine the size of the file in Y records.
            STATUS=0
            IVAL=0
            CALL FTGKYT(UNIT,'NAXIS2',IVAL,DVAL,COMMENT,STATUS)
            YI(I)=IVAL
            IF(STATUS.NE.0) FAIL=1
            IF(IVAL.EQ.0) FAIL=1

*         Keep the HDUCLAS1 value to show the user.
            STATUS=0
            CALL FTGKEY(UNIT,'HDUCLAS1',STRING,COMMENT,STATUS)
            IF(STATUS.EQ.0) THEN
               CALL PERIOD_STRIP(STRING,STRING2)
               HDC(I)=STRING2
            END IF

*         Keep the HDUCLAS2 value to show the user.
            STATUS=0
            CALL FTGKEY(UNIT,'HDUCLAS2',STRING,COMMENT,STATUS)
            IF(STATUS.EQ.0) THEN
               CALL PERIOD_STRIP(STRING,STRING2)
               HDC2(I)=STRING2
            END IF

*         Keep the extension name to show the user.
            STATUS=0
            CALL FTGKEY(UNIT,'EXTNAME',STRING,COMMENT,STATUS)
            IF(STATUS.EQ.0) THEN
               CALL PERIOD_STRIP(STRING,STRING2)
               EXTN(I)=STRING2
            END IF

*         Keep the HDUCLASS value to show the user.
            STATUS=0
            CALL FTGKEY(UNIT,'HDUCLASS',STRING,COMMENT,STATUS)
            IF(STATUS.EQ.0) THEN
               CALL PERIOD_STRIP(STRING,STRING2)
               HDCC(I)=STRING2
               IF(FAIL.EQ.0) EXTENSO=EXTENSO+1
            END IF

*         Make a note that the current extension seems to have been okay.
            OKAY(I)=FAIL
            STATUS=0

         END IF

  10  CONTINUE

*   Warn the user of the lack of suitable OGIP extensions.
      IF(EXTENSO.EQ.0)THEN
         CALL PERIOD_WRITEBELL()
         WRITE(*,*) ' '
         WRITE(*,*)
     :    '** WARNING: No extensions of HDUCLASS OGIP found!'
         WRITE(*,*) ' '
      END IF

*   Show what was found.
      WRITE(*,*) 'Total number of file extensions found:   ',EXTENS
      WRITE(*,*) 'Number of OGIP usable extensions found:  ',EXTENSO
      WRITE(*,*) ' '

      SOFAR=0

      DO 20 I=1,EXTENS

*      Display the extension information.
         IF(OKAY(I).EQ.0) THEN
            WRITE(*,*) 'Extension ',I
         ELSE
            WRITE(*,*) 'Extension ',I,'   ------  NOT USABLE!'
         END IF
         WRITE(*,*) HEAD1(HDT(I))
         WRITE(*,*) 'Size ',xi(i),' by ',yi(i)
         WRITE(*,*) 'EXTNNAME is: '//EXTN(I)
         WRITE(*,*) 'HDUCLASS is: '//HDCC(I)
         WRITE(*,*) 'HDUCLAS1 is: '//HDC(I)
         WRITE(*,*) 'HDUCLAS2 is: '//HDC2(I)
         WRITE(*,*) ' '

*      Avoid a fast screen scroll.
         SOFAR=SOFAR+1
         IF(SOFAR.EQ.3) THEN
            WRITE(*,'(X,A,$)') 'Press enter to continue '
            READ (*,'(A)') JUNK
            SOFAR=0
            WRITE (*,*) ' '
         END IF

  20  CONTINUE
      RETURN
      END
