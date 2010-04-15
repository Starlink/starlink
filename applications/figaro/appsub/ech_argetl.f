C+
      SUBROUTINE ECH_ARGETL(IOUT,ARFILE,NLMAX,PREV,ORDER,CHANS,
     :   WAVES,WEIGHTS,CLASS,FSIGMA,FORDER,NLID,ISTAT)
C
C     E C H _ A R G E T L
C
C     ECHARC utility. Either opens an existing arc line file and
C     reads in the channel numbers and wavelengths it contains,
C     and then opens a new arc line file, 'ARLINES.ECH', or else
C     just opens a new file.
C
C     Parameters -  (">" input, "<" output)
C
C     (>) IOUT     (Integer) Fortran unit number to be used.
C     (>) ARFILE   (Character) Name of previous line list.
C     (>) NLMAX    (Integer) Maximum possible number of lines.
C     (>) PREV     (Logical) If true, routine is to read previous
C                  line list.
C     (<) ORDER    (Integer array ORDER(NLMAX)) Order in which
C                  identified lines were found.
C     (<) CHANS    (Real array CHANS(NLMAX)) Channel numbers of
C                  identified lines.
C     (<) WAVES    (Real array WAVES(NLMAX)) Wavelengths of
C                  identified lines.
C     (<) WEIGHTS  (Real array WEIGHTS(NLMAX)) The weights for the
C                  identified arc lines.
C     (<) CLASS    (Integer array CLASS(NLMAX)) The class codes for
C                  the identified arc lines.
C     (<) FSIGMA   (Real) The sigma read from the file
C     (<) FORDER   (Integer) The order read from the file (the actual
C                  order value in the file, not that plus 1)
C     (<) NLID     (Integer) Number of identified lines.
C     (<) ISTAT    (Integer) 1 if arlines.ech could not be opened for
C                  output, 0 otherwise.
C
C     Subroutines / functions used -
C
C     ICH_LEN      (ICH_ package) Position of last non-blank char in
C                  string
C     PAR_WRUSER   (PAR_   "    ) Send message to user
C
C                                              KS / CIT 14th June 1984
C     Modified:
C
C     5th Sept 1985   KS / AAO  ARFILE, WEIGHTS and CLASS parameters
C                     added. Now looks for auto flag in output.  FSIGMA
C                     and FORDER added.
C     19th Jul 1995   HME / UoE, Starlink. Add the ISTAT argument to
C                     enable the caller to abort if the output file
C                     could not be opened.
C     12th Dec 1997   ACD / UoE, Starlink. Changed the STATUS for
C                     creating a new file from 'NEW' to 'UNKNOWN' so
C                     that any existing file is overwritten.  Also
C                     slightly improved the error reporting if the file
C                     fails to open.
C+
      IMPLICIT NONE
C
C     Parameters
C
      LOGICAL PREV
      INTEGER IOUT,NLMAX,NLID,ORDER(NLMAX),CLASS(NLMAX),FORDER,ISTAT
      REAL CHANS(NLMAX),WAVES(NLMAX),WEIGHTS(NLMAX),FSIGMA
      CHARACTER*(*) ARFILE
C
C     Local variables
C
      INTEGER NFILE
      INTEGER STATUS, OSTAT, LSTAT
      CHARACTER AUTO*4, FILE*64, BUFFER*75
C
C     See if we are to read the old file
C
      NLID=0
      IF (PREV) THEN
         FILE=ARFILE
         OPEN (UNIT=IOUT,FILE=FILE,STATUS='OLD',IOSTAT=STATUS)
         IF (STATUS.NE.0) THEN
            CALL PAR_WRUSER('Unable to open old line list file',STATUS)
         ELSE
            READ (IOUT,'(I5//)',IOSTAT=STATUS) NLID
            DO NFILE=1,NLID
               READ (IOUT,'(6X,I3,2F13.4,7X,A4)',IOSTAT=STATUS)
     :                    ORDER(NFILE),CHANS(NFILE),WAVES(NFILE),AUTO
               IF (STATUS.NE.0) THEN
                  CALL PAR_WRUSER('I/O error reading arcline file',
     :                                                         STATUS)
                  GO TO 320
               END IF
               IF (AUTO.EQ.'    ') THEN
                  CLASS(NFILE)=0
               ELSE IF (AUTO.EQ.' (A)') THEN
                  CLASS(NFILE)=1
               ELSE IF (AUTO.EQ.' (E)') THEN
                  CLASS(NFILE)=2
               ELSE
                  CLASS(NFILE)=3
               END IF
               WEIGHTS(NFILE)=1.0
            END DO
  320       CONTINUE
            IF (STATUS.EQ.0) THEN
               READ (IOUT,'(/41X,F5.2)',IOSTAT=STATUS) FSIGMA
               IF (STATUS.EQ.0) THEN
                  READ (IOUT,'(/15X,I3/)',IOSTAT=STATUS) FORDER
               END IF
               IF (STATUS.NE.0) THEN
                  CALL PAR_WRUSER(
     :                'I/O error reading SIGMA and NCOEFF from file',
     :                                                        STATUS)
               END IF
            END IF
            CLOSE (IOUT,IOSTAT=STATUS)
         END IF
      END IF
C
C     Open new file
C
      OPEN (UNIT=IOUT,FILE='arlines.ech',STATUS='UNKNOWN',IOSTAT=OSTAT)
      IF (OSTAT.NE.0) THEN
         BUFFER=' '
         WRITE(BUFFER, 2000, IOSTAT=LSTAT) OSTAT
 2000    FORMAT('Unable to open new arc line file (Fortran I/O ',
     :     'status: ', I4, ')' )
         CALL PAR_WRUSER(BUFFER,STATUS)
         ISTAT=1
      ELSE
         ISTAT=0
      END IF
C
      RETURN
      END
