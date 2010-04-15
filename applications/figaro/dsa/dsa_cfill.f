C+
C                     D S A _ C F I L L _ A R R A Y
C
C  Routine name:
C     DSA_CFILL_ARRAY
C
C  Function:
C     Fills an array of the specified type with a constant value
C
C  Description:
C     Given the memory address for an array of specified type, this
C     routine fills the array with a constant value.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA_CFILL_ARRAY (NELM,ADDRESS,VALUE,TYPE,STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) NELM        (Integer,ref) The number of elements in the array.
C     (>) ADDRESS     (Integer,ref) The address of the data array.
C     (>) VALUE       (Double,ref) The constant value to be used.
C     (>) TYPE        (Fixed string,descr) The type of the data array.
C                     This must be one of 'FLOAT','INT',SHORT','REAL'
C                     'BYTE', 'USHORT' or 'DOUBLE'.  Anything else is
C                     ignored.
C     (>) STATUS      (Integer,ref) Status code.  If bad status is
C                     passed, this routine returns immediately.
C
C  External variables used:  None.
C
C  External subroutines / functions used:
C     DSA_WRUSER, DSA_CFILLx, CNF_PVAL
C
C  Prior requirements:  None.
C
C  Version date: 24th Feb 1993.
C
C  Authors: Keith Shortridge, AAO
C           Horst Meyerdierks, UoE, Starlink
C-
C  Subroutine / function details:
C     DSA_CFILLx    Fill array of type x with a constant.
C     DSA_WRUSER    Output message to user.
C     DYN_ELEMENT   Dynamic memory element corresponding to address
C
C  History:
C     24th Aug 1988.   Original version.  KS / AAO.
C     24th Apr 1989.   Support for USHORT type added.  KS/AAO.
C     31st Aug 1992.   No longer rely on CNV_.  HME / UoE, Starlink.
C      1st Sep 1992.   Usused variable declarations removed. KS/AAO.
C     24th Feb 1993.   No longer needs to include DSA_TYPES.INC KS/AAO.
C     2005 June 3      Replace DYNAMIC_MEMORY with
C                      %VAL(CNF_PVAL(ADDRESS)) contruct for 64-bit
C                      addressing.  MJC / Starlink
C+
      SUBROUTINE DSA_CFILL_ARRAY (NELM,ADDRESS,VALUE,TYPE,STATUS)
C
      IMPLICIT NONE

      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
C
C     Parameters
C
      INTEGER NELM, ADDRESS, STATUS
      DOUBLE PRECISION VALUE
      CHARACTER*(*) TYPE
C
C     Functions
C
      INTEGER ICH_LEN
      CHARACTER*24 ICH_CD
C
C     Local variables
C
      BYTE      BVALUE           ! Byte constant
      CHARACTER CHR*1            ! First character of TYPE
      REAL      FVALUE           ! Single precision constant
      INTEGER   IVALUE           ! Integer constant
      CHARACTER NUMBER*24        ! Used for formatting numbers
      INTEGER*2 SVALUE           ! Short constant (serves for both
                                 ! signed & unsigned types)
C
C     DSA_ system error codes
C
      INCLUDE 'DSA_ERRORS'
C
C     Return immediately on bad status
C
      IF (STATUS.NE.0) RETURN
C
C     Call appropriate routine for data type
C
      CHR=TYPE(1:1)
      IF (CHR.EQ.'F') THEN
         FVALUE=VALUE
         CALL DSA_CFILLF (NELM,FVALUE,%VAL(CNF_PVAL(ADDRESS)))
      ELSE IF (CHR.EQ.'D') THEN
         CALL DSA_CFILLD (NELM,VALUE,%VAL(CNF_PVAL(ADDRESS)))
      ELSE IF (CHR.EQ.'I') THEN
         IVALUE=VALUE
         CALL DSA_CFILLI (NELM,IVALUE,%VAL(CNF_PVAL(ADDRESS)))
      ELSE IF (CHR.EQ.'S') THEN
         IF ((VALUE.GT.32767.0).OR.(VALUE.LT.-32768.0)) THEN
            NUMBER=ICH_CD(VALUE)
            CALL DSA_WRUSER(
     :        'Cannot fill an array of short integers with the value ')
            CALL DSA_WRUSER(NUMBER(:ICH_LEN(NUMBER)))
            STATUS=DSA__INVTYP
         ELSE
            SVALUE=VALUE
            CALL DSA_CFILLS (NELM,SVALUE,%VAL(CNF_PVAL(ADDRESS)))
         END IF
      ELSE IF (CHR.EQ.'U') THEN
         IF ((VALUE.GT.65535.0).OR.(VALUE.LT.0.0)) THEN
            NUMBER=ICH_CD(VALUE)
            CALL DSA_WRUSER('Cannot fill an array of unsigned '//
     :                                'short integers with the value ')
            CALL DSA_WRUSER(NUMBER(:ICH_LEN(NUMBER)))
            STATUS=DSA__INVTYP
         ELSE
            IVALUE = VALUE
            IF ( IVALUE .GT. 32767 ) IVALUE = IVALUE - 65536
            SVALUE = IVALUE
            CALL DSA_CFILLU (NELM,SVALUE,%VAL(CNF_PVAL(ADDRESS)))
         END IF
      ELSE IF (CHR.EQ.'B') THEN
         IF ((VALUE.GT.127.0).OR.(VALUE.LT.-128.0)) THEN
            NUMBER=ICH_CD(VALUE)
            CALL DSA_WRUSER('Cannot fill a byte array with the value ')
            CALL DSA_WRUSER(NUMBER(:ICH_LEN(NUMBER)))
            STATUS=DSA__INVTYP
         ELSE
            BVALUE=VALUE
            CALL DSA_CFILLB (NELM,BVALUE,%VAL(CNF_PVAL(ADDRESS)))
         END IF
      END IF
C
      END
