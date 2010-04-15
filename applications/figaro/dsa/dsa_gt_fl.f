C+
C                      D S A _ G E T _ F L A G  _ V A L U E
C
C  Routine name:
C     DSA_GET_FLAG_VALUE
C
C  Function:
C     Gets the `flag' value for a specified type.
C
C  Description:
C     This routine returns the the value being used as the `flag'
C     value (`bad' value, `magic number', or whatever) for a specified
C     primitive data type.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA_GET_FLAG_VALUE (TYPE,VALUE,STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) TYPE         (Fixed string,descr) Specifies the type in
C                      question.  This can be any of the primitive types
C                      recognised by DSA (eg. 'Float', 'Int', etc).
C     (<) VALUE        (See description) A variable of the type specified
C                      passed in the default way for VAX Fortran - that
C                      is (any numeric value, ref), or (character,descr).
C                      This receives the `flag' value.
C     (!) STATUS       (Integer,ref) Status value.  If bad status is passed
C                      to it, this routine returns immediately.
C
C  External variables used:  None.
C
C  External subroutines / functions used:
C     DSA_WRUSER
C
C  Prior requirements:
C     DSA_OPEN must have been called to initialise the system.
C
C  Authors: Keith Shortridge, AAO
C           Horst Meyerdierks, UoE, Starlink
C-
C  Subroutine / function details:
C     DSA_WRUSER          Output string to user.
C     DSA_WRFLUSH         Flush output message buffer to user.
C
C  History:
C     13th July 1988.  Original version.  KS / AAO.
C     25th Aug  1992.  Return the appropriate PRIMDAT constant.
C                      HME / UoE, Starlink.
C     31st Aug  1992.  Replaced use of '/N' with call to DSA_WRFLUSH.
C                      KS / AAO.
C     15th Jun  1993.  Fold given type before comparing with upper case
C                      constants. HME / UoE, Starlink.
C+
      SUBROUTINE DSA_GET_FLAG_VALUE (TYPE,VALUE,STATUS)
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER STATUS
      BYTE VALUE                 ! at this level, <TYPE> above and below
      CHARACTER*(*) TYPE
C
C     DSA system error codes
C
      INCLUDE 'DSA_ERRORS'
C
C     Local variables
C
      CHARACTER*(8) TYPE_UC      ! folded type string
      INTEGER IGNORE
C
C     Functions
C
      INTEGER ICH_LEN,ICH_FOLD
C
C     If bad status passed, return immediately.
C
      IF (STATUS.NE.0) RETURN
C
C     Depending on type return the PRIMDAT constant.
C
      TYPE_UC=TYPE
      IGNORE=ICH_FOLD(TYPE_UC)
      IF (TYPE_UC.EQ.'BYTE') THEN
         CALL DSA_FLAG_B( VALUE )
      ELSE IF (TYPE_UC.EQ.'WORD'.OR.TYPE_UC.EQ.'SHORT') THEN
         CALL DSA_FLAG_S( VALUE )
      ELSE IF (TYPE_UC.EQ.'INT') THEN
         CALL DSA_FLAG_I( VALUE )
      ELSE IF (TYPE_UC.EQ.'REAL'.OR.TYPE_UC.EQ.'FLOAT') THEN
         CALL DSA_FLAG_F( VALUE )
      ELSE IF (TYPE_UC.EQ.'DOUBLE') THEN
         CALL DSA_FLAG_D( VALUE )
      ELSE IF (TYPE_UC.EQ.'UWORD'.OR.TYPE_UC.EQ.'USHORT') THEN
         CALL DSA_FLAG_U( VALUE )
      ELSE
         CALL DSA_WRUSER('Unable to get `flag'' value for type "')
         CALL DSA_WRUSER(TYPE(:ICH_LEN(TYPE)))
         CALL DSA_WRUSER(
     :              '".  Invalid type, probable programming error.')
         CALL DSA_WRFLUSH
         STATUS=DSA__INVTYP
      END IF
C
      END
