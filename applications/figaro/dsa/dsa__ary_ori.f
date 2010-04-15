C+
C                        D S A _ _ A R R A Y _ O R I G I N
C
C  Routine name:
C     DSA__ARRAY_ORIGIN
C
C  Function:
C     Gets the origin values for a structured array.
C
C  Description:
C     This routine is passed the name of a structure that might be
C     a structured array.  If it is, it may have an ORIGIN array,
C     and this routine reads that array, should it exist, and returns
C     the values in it.  If no such array exists, the origin values returned
C     are all set to 1 (the default value).  This routine outputs no error
C     messages and indeed, performs no error tests at all - if it fails
C     to read the origin array, it just assumes that's because it
C     didn't exist.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA__ARRAY_ORIGIN (NAME,LENGTH,TYPE,MAXDIM,ORIGIN,
C                                                   NDIM,STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) NAME        (Fixed string,descr) The name of the structure
C                     that is supposed to be an SGP38 array type.
C     (>) LENGTH      (Integer,ref) The number of significant characters
C                     in NAME.
C     (>) TYPE        (Fixed string,descr) The type of the structure.
C                     Must be in upper case.
C     (>) MAXDIM      (Integer,ref) The number of elements in ORIGIN -
C                     ie the maximum number of elements to be read.
C     (<) ORIGIN      (Integer array,ref) The origin values for the
C                     axes of the structured array.  If not specified,
C                     set to 1.
C     (<) NDIM        (Integer,ref) The number of values actually read
C                     from the ORIGIN array.
C     (!) STATUS      (Integer,ref) Status code.  If non-zero status is
C                     passed to it, this routine returns immediately.
C
C  External variables used:  None.
C
C  External subroutines / functions used:
C     DTA_SZVAR, DTA_RDVARI
C
C  Prior requirements:
C     The structure in question should have been opened, and the structure
C     specified in NAME is assumed to exist.
C
C  Support: Keith Shortridge, AAO
C
C  Version date: 29th August 1992
C-
C  Subroutine / function details:
C     DTA_SZVAR     Get the size of a named data object.
C     DTA_RDVARI    Read the value of an integer data object.
C
C  History:
C     15th Apr 1990.   Original version.  KS / AAO.
C     21st Aug 1992    Automatic portability modifications
C                      ("INCLUDE" syntax etc) made. KS/AAO
C     29th Aug 1992    "INCLUDE" filenames now upper case. KS/AAO
C     25th Jul 1996    Catenations for Linux port.   MJCL/Starlink, UCL.
C+
      SUBROUTINE DSA__ARRAY_ORIGIN (NAME,LENGTH,TYPE,MAXDIM,ORIGIN,
     :                                                   NDIM,STATUS)
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER LENGTH, MAXDIM, ORIGIN(MAXDIM), NDIM, STATUS
      CHARACTER*(*) NAME, TYPE
C
C     Local variables
C
      INTEGER DTA_STATUS            ! Status returned by call to DTA routine
      INTEGER IDIM                  ! Loop index through dimensions.
      INTEGER ODIMS                 ! Number of dimensions of ORIGIN array.
      CHARACTER TSTRING*64          ! Local string storage
C
C     Return immediately on bad status
C
      IF (STATUS.NE.0) RETURN
C
C     See if an ORIGIN array is present, and if so read it.
C
      TSTRING = NAME(:LENGTH)//'.ORIGIN'
      CALL DTA_SZVAR (TSTRING,1,ODIMS,NDIM,DTA_STATUS)
      IF (DTA_STATUS.NE.0) THEN
         NDIM=0
      ELSE
         IF (NDIM.GT.MAXDIM) NDIM=MAXDIM
         CALL DTA_RDVARI (TSTRING,NDIM,ORIGIN,DTA_STATUS)
         IF (DTA_STATUS.NE.0) NDIM=0
      END IF
C
C     Set any origin values not specified to 1.
C
      DO IDIM=NDIM+1,MAXDIM
         ORIGIN(IDIM)=1
      END DO
C
      END
