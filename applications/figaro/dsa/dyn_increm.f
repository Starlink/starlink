C+
C                          D Y N _ I N C R E M E N T
C
C  Routine name:
C     DYN_INCREMENT
C
C  Function:
C     Increments a dynamic memory element by a number of array elements.
C
C  Description:
C     If a given element of the conceptual array DYNAMIC_MEM may be used
C     to address a specific element of a dynamic memory array, then this
C     routine returns the element of DYNAMIC_MEM that may be used to
C     address a higher element in the dynamic array.  Using this routine
C     relieves the calling routine of the need to worry about the details
C     of the implementation - ie 'what type has DYNAMIC_MEM been declared
C     as?', 'how many bytes are there to an element of a DOUBLE array?'.
C     Note that this routine does not output error messages or return
C     any error status - if it does not recognise the type it does nothing.
C
C  Language:
C     FORTRAN
C
C  Call:
C     ELEMENT = DYN_INCREMENT (OLD_ELEMENT,TYPE,INCREMENT)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) OLD_ELEMENT  (Integer,ref) The element of DYNAMIC_MEM that
C                      corresponds to a specific element of a dynamically
C                      allocated memory array.
C     (>) TYPE         (Fixed string,descr) The type of the dynamically
C                      allocated memory array.  This should be one of
C                      'BYTE','CHAR','INT','SHORT',DOUBLE, or 'FLOAT'.
C                      Case is not significant.  If TYPE is none of these,
C                      ELEMENT will quietly be set equal to OLD_ELEMENT.
C     (>) INCREMENT    (Integer,ref) The number of elements of the
C                      dynamically allocated array by which the current
C                      element number is to be incremented.
C
C  Returns:
C
C     (<) ELEMENT     (Integer,function value) The element of DYNAMIC_MEM
C                     corresponding to the required element of the
C                     dynamically allocated array.
C
C  External subroutines / functions used:
C     ICH_FOLD
C
C  Prior requirements:  None.
C
C  Support: Horst Meyerdierks, UoE, Starlink.
C-
C  Subroutine details:
C     ICH_FOLD   Convert string to upper case.
C
C  History:
C     21st Jul 1987.    Original version.  KS / AAO.
C     8th  May 1990.    USHORT added (rather belatedly).  KS / AAO.
C     27th Jun 1992.    Port to Unix. HME / UoE, Starlink.
C     10th Aug 1993.    Replace CHR_UCASE with ICH_FOLD.  HME / UoE, Starlink.
C
C  Note:
C     This routine is platform specific insofar as data types may have
C     different lengths in bytes on different machines.
C+
      INTEGER FUNCTION DYN_INCREMENT (OLD_ELEMENT,TYPE,INCREMENT)
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER OLD_ELEMENT,INCREMENT
      CHARACTER*(*) TYPE
C
C     Functions and local variables
C
      INTEGER ICH_FOLD
      INTEGER I                              ! Loop index
      CHARACTER UPTYPE*6                     ! Upper case version of TYPE
C
C     Types recognised and their sizes in bytes.
C
      INTEGER MAX_TYPES
      PARAMETER (MAX_TYPES=7)
      CHARACTER TYPES(MAX_TYPES)*6
      INTEGER   SIZE(MAX_TYPES)
C
C     The size array is valid for VAX, SunSparc, DECstation.
C
      DATA TYPES/'BYTE  ', 'CHAR  ','FLOAT ','DOUBLE','INT   ',
     :           'SHORT ', 'USHORT'/
      DATA SIZE / 1,        1,       4,       8,       4,
     :            2,        2/
C
C     Convert TYPE to upper case
C
      UPTYPE = TYPE
      I = ICH_FOLD(UPTYPE)
C
C     Look up TYPE and apply the appropriate increment - this assumes
C     that DYNAMIC_MEM is a byte array.
C
      DO I=1,MAX_TYPES
         IF (UPTYPE.EQ.TYPES(I)) THEN
            DYN_INCREMENT=OLD_ELEMENT+INCREMENT*SIZE(I)
            GO TO 320
         END IF
      END DO
      DYN_INCREMENT=OLD_ELEMENT
  320 CONTINUE
C
      END
