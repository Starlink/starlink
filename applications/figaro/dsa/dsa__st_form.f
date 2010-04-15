C+
C                      D S A _ _ S E T _ F O R M A T
C
C  Routine name:
C     DSA__SET_FORMAT
C
C  Function:
C     Sets the common variables that control the file formats DSA handles.
C
C  Description:
C     DSA__SET_FORMAT looks at the value of the logical name (or
C     environment variable, depending on the system in use) FIGARO_FORMATS
C     and uses this to control the formats that the DSA routines will
C     recognise.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA__SET_FORMAT (STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (!) STATUS       (Integer,ref) Status code.  If a bad status value
C                      is passed, this routine returns immediately.
C
C  External variables used:
C     Common variables used only by the DSA_ package.
C
C  External subroutines / functions used:
C     DSA_WRUSER, ICH_FOLD, PSX_GETENV, EMS_BEGIN, EMS_ANNUL, EMS_END
C
C  Logical names:
C     FIGARO_FORMATS   Should be set to one of: `DSA', `DSA,NDF',
C                      `NDF,DSA', `NDF'.  These are the only format
C                      combinations accepted at present.  If not
C                      defined, `DSA' is assumed.
C
C  Prior requirements:
C     This is a DSA internal routine intended to be called as part of
C     DSA_OPEN.  It may generate DSA_WRUSER calls, so the common variables
C     controlling the DSA_WRUSER buffer must have been set.
C
C  Support: Keith Shortridge, AAO
C
C  Version date: 29th August 1992
C-
C  Common variable details:
C     (<) DISK_FORMAT  (Integer) Code controlling which formats are supported.
C     (>) DST_ONLY     (Integer parameter) Only DST format is supported.
C     (>) DST_THEN_NDF (Integer parameter) DST and NDF supported, DST default.
C     (>) NDF_THEN_DST (Integer parameter) DST and NDF supported, NDF default.
C     (>) NDF_ONLY     (Integer parameter) Only NDF format is supported.
C
C  Subroutine / function details:
C     DSA_WRUSER       Output message to user.
C     EMS_ANNUL        Clear any EMS errors
C     EMS_BEGIN        Start a new EMS environment
C     EMS_END          End an EMS environment
C     PSX_GETENV       Translate an environment variable or logical name
C     ICH_FOLD         Convert a string to upper case.
C
C  History:
C     18th Jan  1990   Original version.  KS / AAO.
C     21st Aug  1992   Automatic portability modifications
C                      ("INCLUDE" syntax etc) made. KS/AAO
C     29th Aug  1992   Changed from GEN_TRNLNM to PSX_GETENV - more portable
C                      and doesn't have the problem of VMS-style error codes
C                      to worry about. KS/AAO.
C     29th Aug 1992    "INCLUDE" filenames now upper case. KS/AAO
C+
      SUBROUTINE DSA__SET_FORMAT (STATUS)
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER STATUS
C
C     Functions used
C
      INTEGER ICH_FOLD
C
C     DSA_ system common data
C
      INCLUDE 'DSA_COMMON'
C
C     Local variables
C
      INTEGER   EMSTAT              ! Status used in EMS calls
      INTEGER   LENGTH              ! Significant characters in STRING
      INTEGER   PSX_STATUS          ! Status of PSX call
      CHARACTER STRING*32           ! Value of FIGARO_FORMATS logical name
C
C     Check for non-zero STATUS value.
C
      IF (STATUS.NE.0) RETURN
C
C     By default, assume DST files only.
C
      DISK_FORMAT=DST_ONLY
C
C     Attempt to decode the logical name FIGARO_FORMATS.
C
      EMSTAT=0
      CALL EMS_BEGIN(EMSTAT)
      PSX_STATUS=0
      CALL PSX_GETENV('FIGARO_FORMATS',STRING,PSX_STATUS)
      EMSTAT=0
      CALL EMS_ANNUL(EMSTAT)
      CALL EMS_END(EMSTAT)
      IF (PSX_STATUS.EQ.0) THEN
         LENGTH=ICH_FOLD(STRING)
         IF (STRING(:LENGTH).EQ.'DST') THEN
            DISK_FORMAT=DST_ONLY
         ELSE IF (STRING(:LENGTH).EQ.'DST,NDF') THEN
            DISK_FORMAT=DST_THEN_NDF
         ELSE IF (STRING(:LENGTH).EQ.'NDF,DST') THEN
            DISK_FORMAT=NDF_THEN_DST
         ELSE IF (STRING(:LENGTH).EQ.'NDF') THEN
            DISK_FORMAT=NDF_ONLY
         ELSE
            CALL DSA_WRUSER('Warning: the logical name '//
     :                           '"FIGARO_FORMATS" has the value: "')
            CALL DSA_WRUSER(STRING(:LENGTH))
            CALL DSA_WRUSER('". It needs to be one of "DST", "DST,NDF"'
     :              //', "NDF,DST" or "NDF".  "DST" has been assumed.')
         END IF
      END IF
C
      END
