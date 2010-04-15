C+
C                       D S A _ R E F L A G _ D A T A
C
C  Routine name:
C     DSA_REFLAG_DATA
C
C  Function:
C     Reinstates `flagged' data values into a data array
C
C  Description:
C     This routine reverses the operation of DSA_UNFLAG_DATA, taking
C     the flag value position array filled by that routine and setting
C     the indicated elements of a data array to the appropriate flag
C     values.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA_REFLAG_DATA (NELM,TYPE,DATA,FLAGS,STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) NELM      (Integer,ref) The number of data elements in the array.
C     (>) TYPE      (Fixed string,descr) The data type - this should
C                   be one of the primitive data types recognised by the
C                   DSA system.
C     (!) DATA      (Array, ref) The data array to be processed.  Note that
C                   even a character array is passed by reference.
C     (>) FLAGS     (Byte array, ref) The quality array produced by
C                   DSA_UNFLAG_ARRAY. Elements corresponding to flagged
C                   elements are set to 1, all others are set to zero.
C     (!) STATUS    (Integer,ref) Status value.  If bad status is passed to
C                   it, this routine returns immediately.
C
C  External variables used:  None.
C
C  External subroutines / functions used:
C     DSA_REFLAG_x
C
C  Prior requirements: None.
C
C  Support: Keith Shortridge, AAO
C
C  Version date: 29th August 1992
C-
C  Subroutine / function details:
C     DSA_REFLAG_x  (x=F,I,D,S,C,B,U) Reflag data of the specified type.
C
C  History:
C     20th July 1988  Original version.  KS / AAO.
C     25th Apr  1989  Support for USHORT type added.  KS / AAO.
C     21st Aug 1992   Automatic portability modifications
C                     ("INCLUDE" syntax etc) made. KS/AAO
C     29th Aug 1992   "INCLUDE" filenames now upper case. KS/AAO
C+
      SUBROUTINE DSA_REFLAG_DATA (NELM,TYPE,DATA,FLAGS,STATUS)
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NELM, STATUS
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
         CALL DSA_REFLAG_F(NELM,DATA,FLAGS)
      ELSE IF (TYPE.EQ.'INT') THEN
         CALL DSA_REFLAG_I(NELM,DATA,FLAGS)
      ELSE IF (TYPE.EQ.'DOUBLE') THEN
         CALL DSA_REFLAG_D(NELM,DATA,FLAGS)
      ELSE IF (TYPE.EQ.'SHORT') THEN
         CALL DSA_REFLAG_S(NELM,DATA,FLAGS)
      ELSE IF (TYPE.EQ.'USHORT') THEN
         CALL DSA_REFLAG_U(NELM,DATA,FLAGS)
      ELSE IF (TYPE.EQ.'CHAR') THEN
         CALL DSA_REFLAG_C(NELM,DATA,FLAGS)
      ELSE IF (TYPE.EQ.'BYTE') THEN
         CALL DSA_REFLAG_B(NELM,DATA,FLAGS)
      END IF
C
      END
