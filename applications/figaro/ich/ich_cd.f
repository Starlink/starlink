C+
      CHARACTER*(*) FUNCTION ICH_CD (VALUE)
C
C                                I C H _ C D
C
C  Routine: ICH_CD
C
C  Function:
C     Given a double precision value, returns a character string.
C
C  Description:
C     ICH_CD converts a double precision floating point number into
C     a character string.  ICH_CD returns the number formatted in as few
C     characters as possible without sacrificing precision.
C
C  Language:
C     FORTRAN
C
C  Call:
C     STRING = ICH_CD (VALUE)
C
C  Parameters:      (">" input, "<" output)
C     (>) VALUE     (Real, ref) The number to be formatted.
C
C  Returns:
C     (<) STRING    (Fixed string, function value) The number formatted
C                   into a character string.  If the string is too short,
C                   (ie, if ICH_CD has been declared too short), a string of
C                   asterisks will be returned.  ICH_CD should be declared
C                   as CHARACTER*22 or larger.
C
C  External variables used: None
C
C  External routines used:  None.
C
C  Author:  K. Shortridge, AAO
C
C  Date: 14th July 1986
C
C  History:
C     14 Jul 1986 (ks):  Original version.
C     20 Jul 1993 (hme): Try (again) to use a second work string so that
C        one does not copy within the single work string, which does not
C        work on all machines.
C+
      IMPLICIT NONE
C
C     Parameters
C
      DOUBLE PRECISION VALUE
C
C     Local variables
C
      INTEGER   I                         ! Loop variable
      INTEGER   IEXP                      ! Absolute value of exponent
      INTEGER   IGNORE                    ! Dummy status value for write
      INTEGER   IST                       ! First non-blank character
      INTEGER   LASTCH                    ! Last non-blank character
      INTEGER   LNZCH                     ! Last non-zero character
      INTEGER   LSTDIG                    ! Last digit in fraction
      CHARACTER WORK*22                   ! Work string
      CHARACTER WORK2*22                  ! Work string
      LOGICAL   USE_FLOAT                 ! Floating point format needed
C
C     See if we can get away with using an integer format.  If so,
C     use one, otherwise use a general floating point format.
C     Format the number into the work string.
C
      USE_FLOAT=.TRUE.
      IF ((VALUE.LE.65535.0).AND.(VALUE.GE.-65536.0)) THEN
         IF (FLOAT(INT(VALUE)).EQ.VALUE) THEN
            WRITE (WORK,'(I6)',IOSTAT=IGNORE) INT(VALUE)
            USE_FLOAT=.FALSE.
            LASTCH=6
         END IF
      END IF
      IF (USE_FLOAT) THEN
         WRITE (WORK,'(1PD22.15)',IOSTAT=IGNORE) VALUE
         WORK(19:19)='E'
         LASTCH=22
      END IF
C
C     Now try to tidy it up a bit.  First, find the first non-blank
C     character.
C
      DO I=1,LEN(WORK)
         IF (WORK(I:I).NE.' ') THEN
            IST=I
            GO TO 320
         END IF
      END DO
      IST=LEN(WORK)
  320 CONTINUE
C
C     A floating point number can be tidied a little more.  What
C     follows is based on the assumption that it was formatted with
C     1PE22.15 format.
C
      IF (USE_FLOAT) THEN
C
C        We can tidy up some exponent ranges.  We convert anything
C        in the range E-02 to E+05 into a value without an exponent.
C
         LSTDIG=18
         IF (WORK(21:21).EQ.'0') THEN
            IEXP=ICHAR(WORK(22:22))-ICHAR('0')
            IF (WORK(20:20).EQ.'-') THEN
               IF (IEXP.LE.2) THEN
                  WORK(19:)=' '
C
C                 WORK(IEXP+4:)=WORK(4:)
C                 WORK(IEXP+3:IEXP+3)=WORK(2:2)
                  WORK2=WORK
                  WORK2(IEXP+4:)=WORK(4:)
                  WORK2(IEXP+3:IEXP+3)=WORK(2:2)
                  WORK=WORK2
C
                  WORK(2:IEXP+2)='0.0'
                  LASTCH=IEXP+18
                  LSTDIG=LSTDIG+IEXP
               END IF
            ELSE
               IF (IEXP.LE.5) THEN
                  WORK(19:)=' '
                  LASTCH=18
                  IF (IEXP.GT.0) THEN
C
C                    WORK(3:IEXP+2)=WORK(4:IEXP+3)
                     WORK2=WORK
                     WORK2(3:IEXP+2)=WORK(4:IEXP+3)
                     WORK=WORK2
C
                     WORK(IEXP+3:IEXP+3)='.'
                  END IF
               END IF
            END IF
         END IF
C
C        See if we need all the decimal places in a floating
C        point number.
C
         LNZCH=4
         DO I=4,LSTDIG
            IF (WORK(I:I).NE.'0') LNZCH=I
         END DO
C
C        WORK(LNZCH+1:)=WORK(LSTDIG+1:)
         WORK2=WORK
         WORK2(LNZCH+1:)=WORK(LSTDIG+1:)
         WORK=WORK2
C
         LASTCH=LASTCH-(LSTDIG-LNZCH)
      END IF
C
C     Now copy the result into the return function value,
C     assuming it will fit.
C
      IF (LEN(ICH_CD).LT.(LASTCH-IST+1)) THEN
         ICH_CD='**********************'
      ELSE
         ICH_CD=WORK(IST:LASTCH)
      END IF
C
      END
