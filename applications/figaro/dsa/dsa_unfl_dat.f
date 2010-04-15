C+
C                       D S A _ U N F L A G _ D A T A
C
C  Routine name:
C     DSA_UNFLAG_DATA
C
C  Function:
C     Removes `flagged' data values from a data array
C
C  Description:
C     This routine takes a data array of specified type that may
C     contain `flagged' data values.  These flagged values are replaced
C     by harmless values - since flag values tend to be extreme values
C     and liable to produce arithmetic errors if processed - and the
C     locations of the flagged data elements are remembered in a
C     quality array.  The number of flagged values found in the original
C     array is returned. Optionally, the flagged values can be left
C     unchanged, in which case all that happens is that their locations
C     are used to set elements of the quality array.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA_UNFLAG_DATA (NELM,LEAVE,TYPE,DATA,FLAGS,NFLAGGED,STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) NELM      (Integer,ref) The number of data elements in the array.
C     (>) LEAVE     (Logical,ref) True if the flags are to be left in the data.
C     (>) TYPE      (Fixed string,descr) The data type - this should
C                   be one of the primitive data types recognised by the
C                   DSA system.
C     (!) DATA      (Array, ref) The data array to be processed.  Note that
C                   even a character array is passed by reference.
C     (<) FLAGS     (Byte array, ref) The quality array produced. Elements
C                   corresponding to flagged elements are set to 1, all
C                   others are set to zero.
C     (<) NFLAGGED  (Integer,ref) The number of flagged values found.
C     (!) STATUS    (Integer,ref) Status value.  If bad status is passed to
C                   it, this routine returns immediately.
C
C  External variables used:  None.
C
C  External subroutines / functions used:
C     DSA_UNFLAG_x
C
C  Prior requirements: None.
C
C  Support: Keith Shortridge, AAO
C
C  Version date: 6th February 1995
C-
C  Subroutine / function details:
C     DSA_UNFLAG_x  (x=F,I,D,S,C,U,B) Unflag data of the specified type.
C
C  History:
C     20th July 1988  Original version.  KS / AAO.
C     25th Apr  1989  Support for USHORT type added. KS / AAO.
C     3rd  May  1990  NFLAGGED parameter added. KS/AAO.
C     21st Aug 1992   Automatic portability modifications
C                     ("INCLUDE" syntax etc) made. KS/AAO
C     29th Aug 1992   "INCLUDE" filenames now upper case. KS/AAO
C     6th  Feb  1995  Added 'LEAVE' argument. KS/AAO.
C+
      SUBROUTINE DSA_UNFLAG_DATA (NELM,LEAVE,TYPE,DATA,FLAGS,
     :                                            NFLAGGED,STATUS)
C
      IMPLICIT NONE
C
C     Parameters
C
      LOGICAL LEAVE
      INTEGER NELM, NFLAGGED, STATUS
      BYTE DATA, FLAGS(NELM)
      CHARACTER*(*) TYPE
C
C     Return immediately on bad status
C
      IF (STATUS.NE.0) RETURN
C
C     Call the appropriate routine to do the processing.
C
      IF (TYPE.EQ.'FLOAT') THEN
         CALL DSA_UNFLAG_F(NELM,LEAVE,DATA,NFLAGGED,FLAGS)
      ELSE IF (TYPE.EQ.'INT') THEN
         CALL DSA_UNFLAG_I(NELM,LEAVE,DATA,NFLAGGED,FLAGS)
      ELSE IF (TYPE.EQ.'DOUBLE') THEN
         CALL DSA_UNFLAG_D(NELM,LEAVE,DATA,NFLAGGED,FLAGS)
      ELSE IF (TYPE.EQ.'SHORT') THEN
         CALL DSA_UNFLAG_S(NELM,LEAVE,DATA,NFLAGGED,FLAGS)
      ELSE IF (TYPE.EQ.'USHORT') THEN
         CALL DSA_UNFLAG_U(NELM,LEAVE,DATA,NFLAGGED,FLAGS)
      ELSE IF (TYPE.EQ.'CHAR') THEN
         CALL DSA_UNFLAG_C(NELM,LEAVE,DATA,NFLAGGED,FLAGS)
      ELSE IF (TYPE.EQ.'BYTE') THEN
         CALL DSA_UNFLAG_B(NELM,LEAVE,DATA,NFLAGGED,FLAGS)
      END IF
C
      END
