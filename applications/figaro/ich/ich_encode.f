C+
C                           I C H _ E N C O D E
C
C     Routine name:
C           ICH_ENCODE
C
C     Function:
C           Formats a REAL number into a character string
C
C     Decription:
C           This routine formats a real variable into a character string
C           in a nice way; that is, if the number is an integer (within
C           the integer 16 bit range), it will be printed as such, and
C           'E' format will only be used if the number is particularly
C           large or small.  Generally, an ordinary floating point format
C           will be used.
C
C     Language:
C           FORTRAN:
C
C     Call:
C           STATUS=ICH_ENCODE(FIELD,VALUE,IPTR,IDECC,NPTR)
C
C     Parameters:      (">" input, "<" output)
C
C          (>) FIELD   (Fixed string, descr) The character string into
C                      which VALUE is to be encoded.
C          (>) VALUE   (Real, ref) The number to be encoded
C          (>) IPTR    (Integer, ref) quantity giving the number of
C                      the first byte in FIELD (starting at 1) into
C                      which the ASCII encoded value is to be written.
C          (>) IDDEC   (Integer, ref) One less than the number of
C                      significant figures to be aimed at if a floating
C                      point output format is used.  This is the number of
C                      decimal places used in an E format (although the
C                      routine tries to avoid E format, and will print eg
C                      1234.5678 as 1234.6 for any value of IDECC less
C                      than 5).
C          (<) NPTR    (Integer, ref) Variable in which ENCODE
C                      returns the number of the next available character
C                      in FIELD.
C
C     Returns:         (if called as a function)
C
C          (<) STATUS  (Integer, function value) Return code.
C                      0 => OK,
C                      1 => Not enough character positions left in FIELD.
C
C     External variables used: None
C
C     External routines used: None
C
C     Authors: K. Shortridge,
C              H. Meyerdierks.
C
C     Date: 26th Aug 1992
C
C     Note:
C           This routine is not intended to give precise control over
C           formats.  The intention is that it will always produce a
C           sensibly formatted result, using IDECC as a guide to the
C           precision required.  Setting IDECC=6 gives a result at the
C           maximum precision available in single precision on the VAX.
C
C     Internal declarations:
C           INTEGER FUNCTION ICH_ENCODE(FIELD,VALUE,IPTR,IDECC,NPTR)
C           CHARACTER*(*) FIELD
C           INTEGER IPTR,NPTR,IDECC
C           REAL VALUE
C
C     Keywords:
C           Strings, free-format, encode
C-
C     Modifications:
C           19th July 1983.  Original VAX version.  KS / UCL, based on
C                            an Interdata program by Tony Hewer, UCL.
C           24th July 1986.  Complete re-write to use Fortran internal
C                            I/O.  Use of IDECC changed slightly - it
C                            used to be the number of decimal places,
C                            meaning that with IDECC=3 a number such as
C                            0.02345 would output as 0.023, whereas it
C                            is now output as 0.02345, and 0.002345 used
C                            to give 0.002 and now gives 2.345E-3
C           26th Aug  1992.  Use two work spaces to avoid dangerous
C                            substring copying.
C+
      INTEGER FUNCTION ICH_ENCODE(FIELD,VALUE,IPTR,IDECC,NPTR)
C
      IMPLICIT NONE
C
      CHARACTER*(*) FIELD
      INTEGER IPTR,NPTR,IDECC
      REAL VALUE
C
C     Local variables
C
      INTEGER   I                         ! Loop variable
      INTEGER   IGNORE                    ! Dummy status value for write
      INTEGER   IST                       ! First non-blank character
      INTEGER   ISTDIG                    ! First digit of fractional part
      INTEGER   LASTCH                    ! Last non-blank character
      INTEGER   LSTDIG                    ! Last digit of fractional part
      INTEGER   LSTSIG                    ! Last significant fract. digit
      INTEGER   NDEC                      ! Formatted # of decimal places
      INTEGER   NLOG                      ! Integer log of value
      CHARACTER WORK*13                   ! Work string
      CHARACTER WORK2*13                  ! Work string
      LOGICAL   USE_FLOAT                 ! Floating point format needed
      REAL      VLOG                      ! Log of value
C
C     See if we can get away with using an integer format.
C
      LASTCH=LEN(WORK)
      USE_FLOAT=.TRUE.
      IF ((VALUE.LE.65535.0).AND.(VALUE.GE.-65536.0)) THEN
         IF (FLOAT(INT(VALUE)).EQ.VALUE) THEN
            WRITE (WORK,'(I6)',IOSTAT=IGNORE) INT(VALUE)
            USE_FLOAT=.FALSE.
            LASTCH=6
         END IF
      END IF
      IF (USE_FLOAT) THEN
C
C        We need a floating point format.  See if we will need to
C        use an 'E' type format.
C
         IF (VALUE.EQ.0.0) THEN
            VLOG=0.0
         ELSE
            VLOG=LOG10(ABS(VALUE))
         END IF
         IF (VLOG.LT.0.0) THEN
            NLOG=INT(VLOG)-1
         ELSE
            NLOG=INT(VLOG)
         END IF
         IF ((NLOG.GT.5).OR.(NLOG.LT.-2)) THEN
C
C           Yes, we do need an 'E' format. The number of decimal
C           places is IDECC.
C
            NDEC=MAX(1,MIN(IDECC,6))
            WRITE (WORK,'(1PE13.'//CHAR(NDEC+ICHAR('0'))//')',
     :                                     IOSTAT=IGNORE) VALUE
            LSTDIG=9
            ISTDIG=10-NDEC
            IF (WORK(12:12).EQ.'0') THEN
               WORK2=WORK
               WORK(12:)=WORK2(13:)
               LASTCH=12
            END IF
         ELSE
C
C           Not an 'E' format.  The number of decimal places depends
C           on the significance required (IDECC) and the value itself.
C
            NDEC=MAX(1,MIN(IDECC-NLOG,7-NLOG))
            WRITE (WORK,'(F13.'//CHAR(NDEC+ICHAR('0'))//')',
     :                                     IOSTAT=IGNORE) VALUE

            LSTDIG=13
            ISTDIG=14-NDEC
         END IF
C
C        See if any of the trailing decimal figures are zero
C
         LSTSIG=LSTDIG
         DO I=ISTDIG+1,LSTDIG
            IF (WORK(I:I).NE.'0') LSTSIG=I
         END DO
         IF (LSTSIG.LT.LSTDIG) THEN
            IF (LSTDIG.EQ.LEN(WORK)) THEN
               WORK(LSTSIG+1:)=' '
            ELSE
               WORK2=WORK
               WORK(LSTSIG+1:)=WORK2(LSTDIG+1:)
            END IF
            LASTCH=LASTCH-(LSTDIG-LSTSIG)
         END IF
      END IF
C
C     Now find the first non-blank character.
C
      DO I=1,LASTCH
         IF (WORK(I:I).NE.' ') THEN
            IST=I
            GO TO 320
         END IF
      END DO
      IST=LASTCH
  320 CONTINUE
C
C     Now copy the result into the return string, if it will fit
C
      NPTR=IPTR+LASTCH-IST+1
      IF (NPTR.LE.LEN(FIELD)) THEN
         ICH_ENCODE=0
         FIELD(IPTR:)=WORK(IST:LASTCH)
      ELSE
         ICH_ENCODE=1
         NPTR=IPTR
      END IF
C
      END
