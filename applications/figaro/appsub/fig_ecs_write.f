C+
C                        F I G _ E C S _ W R I T E
C
C  Routine name:
C     FIG_ECS_WRITE
C
C  Function:
C     Writes an echelle order selection file for ECHSELECT
C
C  Description:
C     This routine writes the list of order selections produced
C     by ECHSELECT out to a file.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL FIG_ECS_WRITE (FILENAME,STRUCTURE,ORDERS,NELM,CODE)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) FILENAME    (Fixed string,descr) The name of the file
C                     to be read.  This can be as full a description
C                     as is necessary.  Note that no search paths
C                     are used automatically by this routine.
C     (>) STRUCTURE   (Fixed string, descr) The name of the structure
C                     used to produce this data.
C     (>) ORDERS      (Integer array,ref) Indicates for each cross-section
C                     whether it is unselected (=0), object for order M
C                     (=M) or sky for order M (= -M).
C     (>) NELM        (Integer,ref) The number of elements in ORDERS.
C     (<) CODE        (Integer, ref) Error code. 0=> OK, 1=> Information,
C                     2=> Warning, 3=> Error, 4=> Fatal.
C
C  External variables used:  None.
C
C  External subroutines / functions used:
C     GEN_TIME, ICH_LEN, PAR_WRUSER
C
C  Prior requirements:
C     This is an internal routine of ECHSELECT.
C
C  Support: Keith Shortridge, AAO
C
C  Version date: 22nd Feb 1989.
C-
C  Subroutine / function details:
C     PAR_WRUSER    Output string to user
C     ICH_LEN       Position of last non-blank char in string
C     GEN_TIME      Formats today's date in a friendly way
C
C  History:
C     22nd Feb 1989.   Original version.  KS / AAO.
C     25th Jul 1996.   Catenations for Linux.  MJC / Starlink, UCL.
C     26th Sep 2001.   Fixed spurious line breaks in the header information
C                      at the top of the file.  Also removed unused
C                      external function ICH_CI.  ACD / Starlink, UoE.
C
C+
      SUBROUTINE FIG_ECS_WRITE (FILENAME,STRUCTURE,ORDERS,NELM,CODE)
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NELM, ORDERS(NELM), CODE
      CHARACTER*(*) FILENAME, STRUCTURE
C
C     Functions used
C
      INTEGER   ICH_LEN
C
C     Local variables
C
      CHARACTER DATE*20          ! Today's date
      CHARACTER DAY*9            ! Day of the week
      LOGICAL   FOPEN            ! True if file opened OK
      INTEGER   FSTAT            ! Fortran I/O status
      CHARACTER HOUR*12          ! Time of day
      INTEGER   I                ! Loop index through ORDERS
      INTEGER   IGNORE           ! Used for don't care status codes
      INTEGER   LDATE            ! Significant characters in DATE
      INTEGER   LDAY             ! Significant characters in DAY
      INTEGER   LHOUR            ! Significant characters in HOUR
      INTEGER   LU               ! Logical unit number to use for file
      INTEGER   STATUS           ! DSA inherited status value
      CHARACTER STRING*80        ! String use to format messages
C
C     Error codes
C
      INTEGER OK, INFORM, WARNING, ERROR, FATAL
      PARAMETER (OK=0, INFORM=1, WARNING=2, ERROR=3, FATAL=4)
C
C     Initial values
C
      CODE=OK
      STATUS=0
      FSTAT=0
      FOPEN=.FALSE.
C
C     Get logical unit to use
C
      CALL DSA_GET_LU (LU,STATUS)
      IF (STATUS.NE.0) THEN
         CODE=ERROR
         GO TO 500
      END IF
C
C     Open file.  This may fail if the file exists.  So in case of an
C     error try to delete the existing file and try to open a new one
C     again.  Deletion is tried by open and close with dispose keyword.
C
      OPEN (UNIT=LU,FILE=FILENAME,STATUS='NEW',IOSTAT=FSTAT)
      IF (FSTAT.NE.0) THEN
         OPEN (UNIT=LU,FILE=FILENAME,STATUS='OLD',IOSTAT=FSTAT)
         IF (FSTAT.NE.0) GO TO 500
         CLOSE (UNIT=LU,STATUS='DELETE',IOSTAT=FSTAT)
         IF (FSTAT.NE.0) THEN
            CLOSE (UNIT=LU)
            GO TO 500
         END IF
         OPEN (UNIT=LU,FILE=FILENAME,STATUS='NEW',IOSTAT=FSTAT)
      END IF
      IF (FSTAT.NE.0) GO TO 500
      FOPEN=.TRUE.
C
C     Write header lines
C
      CALL GEN_TIME (6,DAY,LDAY,DATE,LDATE,HOUR,LHOUR)
      WRITE (LU, 2000, IOSTAT=FSTAT)
     :  STRUCTURE(1 : ICH_LEN(STRUCTURE)),
     :  DAY(1 : LDAY),  DATE(1 : LDATE),  HOUR(1 : LHOUR)
 2000 FORMAT(
     :   '*'  /
     :   '*    Order selections from image: ', A  /
     :   '*'  /
     :   '*    Created by ECHSELECT, ', A, ', ', A, ' ', A  /
     :   '*')
      IF (FSTAT.NE.0) GO TO 500
C
C     Write any non-zero elements of ORDERS to the file
C
      DO I=1,NELM
         IF (ORDERS(I).NE.0) THEN
            WRITE (LU,'(2I10)',IOSTAT=FSTAT) I,ORDERS(I)
            IF (FSTAT.NE.0) GO TO 500
         END IF
      END DO
C
C     On the way out, close down anything opened.  If FSTAT is non-zero,
C     there has been a Fortran I/O error.
C
  500 CONTINUE
      IF (FSTAT.NE.0) THEN
         IF (FOPEN) THEN
            STRING='I/O error creating file '//FILENAME
         ELSE
            STRING='I/O error writing to file '//FILENAME
         END IF
         CALL PAR_WRUSER(STRING(:ICH_LEN(STRING)),IGNORE)
         CALL GEN_FORTERR(FSTAT,.FALSE.,STRING)
         CALL PAR_WRUSER(STRING(:ICH_LEN(STRING)),IGNORE)
         CODE=MAX(CODE,ERROR)
      END IF
      IF (FOPEN) CLOSE(LU,IOSTAT=IGNORE)
      STATUS=0
      CALL DSA_FREE_LU (LU,STATUS)
C
      END
