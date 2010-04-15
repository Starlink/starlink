C+
C                     D S A _ Z F I L L _ A R R A Y
C
C  Routine name:
C     DSA_ZFILL_ARRAY
C
C  Function:
C     Fills an array of the specified type with zeros.
C
C  Description:
C     Given the address of an array of specified type, this routine
C     fills the array with zeros.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA_ZFILL_ARRAY (NELM,ADDRESS,TYPE,STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) NELM        (Integer,ref) The number of elements in the array.
C     (>) ADDRESS     (Integer,ref) The address of the array.
C     (>) TYPE        (Fixed string,descr) The type of the data array.
C                     This must be one of 'FLOAT','INT',SHORT','REAL'
C                     'BYTE', 'USHORT', or 'DOUBLE'.  Anything else is
C                     ignored.
C     (>) STATUS      (Integer,ref) Status code.  If bad status is passed,
C                     this routine returns immediately.
C
C  External variables used:  None.
C
C  External subroutines / functions used:
C     CNF_PVAL, DSA_WRUSER, GEN_FILL
C
C  Prior requirements:  None.
C
C  Support: Keith Shortridge, AAO
C
C  Version date: 29th August 1992
C-
C  Subroutine / function details:
C     CNF_PVAL           Full pointer to dynamically allocated memory
C     GEN_FILL           Fill an array with a specified byte value.
C     DSA_WRUSER         Output message to user.
C
C  History:
C     8th July 1987  Original version.  KS / AAO.
C     25th Apr 1989  Support for USHORT type added.  KS / AAO.
C     21st Aug 1992  Automatic portability modifications
C                    ("INCLUDE" syntax etc) made. KS/AAO
C     29th Aug 1992  "INCLUDE" filenames now upper case. KS/AAO
C     2005 June 3    Replace DYNAMIC_MEMORY with %VAL(CNF_PVAL(ADDRESS))
C                    contruct for 64-bit addressing.  MJC / Starlink
C+
      SUBROUTINE DSA_ZFILL_ARRAY (NELM,ADDRESS,TYPE,STATUS)
C
      IMPLICIT NONE

      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
C
C     Parameters
C
      INTEGER NELM, ADDRESS, STATUS
      CHARACTER*(*) TYPE
C
C     Local variables
C
      INTEGER   ELEMENT                ! Dynamic array element number
      INTEGER   ITYPE                  ! Type code for data.
C
C     Sizes for elements of various types.  Note the order is determined
C     solely by the 'FDISBU' string used to calculate ITYPE.
C
      INTEGER SIZES(6)
      DATA SIZES/4,8,4,2,1,2/
C
C     Return immediately on bad status
C
      IF (STATUS.NE.0) RETURN
C
C     Work out number of bytes, given data type, then zero the array.
C     (Note the implied assumption that zero for all data types is just
C     a sequence of zero bytes.)
C
      ITYPE=INDEX('FDISBU',TYPE(1:1))
      IF (ITYPE.GT.0) THEN
         CALL GEN_FILL(NELM*SIZES(ITYPE),0,%VAL(CNF_PVAL(ADDRESS)))
      END IF
C
      END
