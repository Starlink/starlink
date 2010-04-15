C+
      SUBROUTINE LHATPOL(STATUS)
C
C            L H A T P O L
C
C     Command name:
C        LHATPOL
C
C     Function:
C        List Hatfield Polarimeter Infrared Data
C
C     Description:
C        LHATPOL lists the IR data files in Figaro format as produced
C        by the Hatfield Polarimeter systems on UKIRT. Its principal
C        use is to detect spikes for subsequent removal using TSETBAD.
C
C        LHATPOL works on the original Figaro format file produced by the
C        data acquisition system. However, the spikes must be removed from
C        the data by using TSETBAD on the TSP file obtained by inporting the
C        data using RHATPOL.
C
C     Parameters:
C    (1) FIGARO     (Char)     The IRPS Figaro file to read.
C    (2) FILE      (File)      Name of listing file.
C
C     Support:
C         Jeremy Bailey, AAO
C
C     Version date:
C         1/4/1990
C
C-
C
C  History:
C    1/4/1990   Original Version.   JAB/JAC
C



      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'USER_ERR'

*  Status argument
      INTEGER STATUS

*  Data pointers
      INTEGER IPTR
      INTEGER OIPTR
      INTEGER XPTR

*  Number of elements
      INTEGER NELM

*  Figaro status
      INTEGER FSTATUS

*  Array size
      INTEGER NDIM, DIMS(7)
      INTEGER EL,SL

*  Length of file name
      INTEGER LENNAME

*  Run parameters
      INTEGER NPOINTS,NCYCLES,NPTS,N2
      INTEGER I
      INTEGER NSTRT
      INTEGER J
      CHARACTER*80 FNAME,LABEL,UNITS,ERRMES,UTSTART,UTEND,UTDATE
      LOGICAL OK
      INTEGER FD

      INTEGER CHR_LEN

*  Access the Figaro frame

      CALL PAR_GET0C('FIGARO',FNAME,STATUS)
      LENNAME = CHR_LEN(FNAME)
      CALL DSA_OPEN(STATUS)
      CALL DSA_NAMED_INPUT('INPUT',FNAME(:LENNAME),STATUS)

*  Get the data array

      IF (STATUS .EQ. SAI__OK) THEN
         CALL DSA_DATA_SIZE('INPUT',7,NDIM,DIMS,EL,STATUS)

*   Check dimensions are valid (must be 3D)
         IF (NDIM .NE. 3) THEN
            CALL MSG_OUT('MSG','Dimensions of Input File Invalid',
     :          STATUS)
            STATUS = USER__001
         ELSE
            NPOINTS = DIMS(1)
            N2 = DIMS(2)
            NCYCLES = DIMS(3)

*  Map the data
            CALL DSA_MAP_DATA('INPUT','READ','FLOAT',IPTR,SL,STATUS)
            IF (STATUS .NE. SAI__OK) THEN
               CALL MSG_OUT('MSG','Error Mapping Input Data',STATUS)
               GOTO 100
            ENDIF

*  Create the output file
            CALL FIO_ASSOC('FILE','WRITE','LIST',0,FD,STATUS)

*  List data to output file
            IF (STATUS .EQ. SAI__OK) THEN
                CALL TSP_LHATPOL(NCYCLES,N2,NPOINTS,%VAL(IPTR),
     :                  FD,STATUS)
            ENDIF

*  Close the output file
            CALL FIO_CANCL('FILE',STATUS)
100         CONTINUE
         ENDIF
      ENDIF

*  Close DSA
      CALL DSA_CLOSE(STATUS)
      END




      SUBROUTINE TSP_LHATPOL(NCYCLES,N2,NPOINTS,IN,FD,STATUS)
*+
*
*   T S P _ L H A T P O L
*
*   List data from a Hatfield polarimeter data file to a listing file.
*   Only the IR data (channel 1) is listed - the aim being to detect spikes
*   which are only present in the IR.
*
*
*    (>)  NCYCLES   (Integer)         Number of cycles
*    (>)  N2        (Integer)         Second dimension of data array
*    (>)  NPOINTS   (Integer)         Number of points
*    (>)  IN        (Real array(NCYCLES,N2,NPOINTS))   Data array
*    (>)  FD        (Integer)         File descriptor of listing file
*    (!)  STATUS    (Integer)         Status value
*
*   Jeremy Bailey    1/4/1990
*
*+

      IMPLICIT NONE

*  Parameters
      INTEGER NCYCLES,NPOINTS,N2
      REAL IN(NPOINTS,N2,NCYCLES)
      INTEGER FD
      INTEGER STATUS

*  Local variables
      CHARACTER*80 BUF
      INTEGER I,J,K
      J = 1

*  Loop over cycles
      DO I=1,NCYCLES

*  List first star and sky pair
         WRITE(BUF,'(I5,8F9.0)') J,(IN(K,1,I),K=1,48,6)
         CALL FIO_WRITE(FD,BUF,STATUS)
         WRITE(BUF,'(I5,8F9.0)') J,(IN(K,1,I),K=49,96,6)
         CALL FIO_WRITE(FD,BUF,STATUS)
         WRITE(BUF,'(I5,8F9.0)') J,(IN(K,2,I),K=1,48,6)
         CALL FIO_WRITE(FD,BUF,STATUS)
         WRITE(BUF,'(I5,8F9.0)') J,(IN(K,2,I),K=49,96,6)
         CALL FIO_WRITE(FD,BUF,STATUS)
         J=J+1

*  List second sky, star pair
         WRITE(BUF,'(I5,8F9.0)') J,(IN(K,3,I),K=1,48,6)
         CALL FIO_WRITE(FD,BUF,STATUS)
         WRITE(BUF,'(I5,8F9.0)') J,(IN(K,3,I),K=49,96,6)
         CALL FIO_WRITE(FD,BUF,STATUS)
         WRITE(BUF,'(I5,8F9.0)') J,(IN(K,4,I),K=1,48,6)
         CALL FIO_WRITE(FD,BUF,STATUS)
         WRITE(BUF,'(I5,8F9.0)') J,(IN(K,4,I),K=49,96,6)
         CALL FIO_WRITE(FD,BUF,STATUS)
         J=J+1
      ENDDO
      END
