C+
C                              F I T S E T
C
C  Function:
C     Figaro routine to set (or modify) a FITS keyword in a file.
C
C  Description:
C     This routine allows a FITS keyword in a file to be set or, if it
C     already exists, to be modified. This routine is needed mainly because
C     of the difficulty of changing items in a FITS header when the file
C     in question is in NDF format. (A .DST file has the FITS information
C     in separate structure items which can easily be modified using LET,
C     but an NDF format file has all the FITS keywords in a single
C     character array which is not amenable to such changes.)
C
C  Invocation:
C     FITSET file keyword value comment [logical] [string]
C
C  Parameters:
C     file     (Filename) The name of the Figaro format file in which the
C              keyword is to be set.
C     keyword  (Character string) The name of the FITS keyword that is to be
C              set.
C     value    (Character string) The new value of the FITS keyword. If
C              this can be interpreted as a numeric value it will be set as
C              such. Otherwise it will be kept as a character string.
C     comment  (Character string) The comment to be associated with the
C              keyword.
C     logical  (Keyword) If set, forces the value to be treated as a logical
C              value, in which case it must be one of 'T' or 'F' and will
C              be set as such in the output file.
C     string   (Keyword) If set, forces the value to be treated as a literal
C              string; in this case it will not be treated as a number even if
C              it can be.
C
C  Authors:
C     Keith Shortridge, AAO.
C     Horst Meyerdierks, UoE, Starlink.
C
C  History:
C     29 Jun 1994 (ks):
C        Original version.
C     13 Mar 1996 (hme):
C        Adapt to the FDA library.
C        Input read-only. Use DSA_INPUT_UPDATE.
C
C+
      SUBROUTINE FITSET
C
      IMPLICIT NONE
C
C     Local variables
C
      CHARACTER ACCESS*1          ! Access code for keyword type
      CHARACTER COMMENT*80        ! Comment associated with keyword
      CHARACTER CSTRING*80        ! Value of keyword as a string
      DOUBLE PRECISION DVALUE     ! Keyword value as a double precision value
      LOGICAL   EXIST             ! True if keyword already exists
      REAL      FVALUE            ! Keyword value as a single precision value
      INTEGER   IGNORE            ! Status value we don't care about
      INTEGER   INVOKE            ! Used to invoke a function
      INTEGER   ITEM              ! Loop index through keyword values
      INTEGER   IVALUE            ! Keyword value as an integer value
      CHARACTER KEYWORD*16        ! Name of FITS keyword to be set
      INTEGER   LENSTR            ! Length of character keyword value
      LOGICAL   LOGIC             ! Value of 'LOGICAL' keyword
      LOGICAL   LOGIT             ! True if keyword value to be output
      LOGICAL   LVALUE            ! Keyword value as a logical value
      INTEGER   NELM              ! Number of elements in keyword
      INTEGER   NEXT              ! Next character in string
      INTEGER   NSFIG             ! Number of significant figures specified
      INTEGER   NSTAT             ! Status of decode attempt
      INTEGER   STATUS            ! Inherited status used for DSA
      LOGICAL   STRING            ! Value of 'STRING' keyword
      CHARACTER VALUE*80          ! Value of keyword as input by user
      CHARACTER VSTRING*80        ! Value of keyword as a string
C
C     Functions used
C
      CHARACTER*32 ICH_CD
      INTEGER ICH_NUMBD, ICH_LEN, ICH_FOLD
      LOGICAL PAR_ABORT
C
      STATUS = 0
      CALL DSA_OPEN(STATUS)
C
C     Open the data file
C
      CALL DSA_INPUT_UPDATE ('FITSFILE','FILE',STATUS)
      IF (STATUS.NE.0) GO TO 500     ! Error exit
C
C     Get the name of the keyword. See if it already exists in the
C     file. If so, log its value(s).
C
      CALL PAR_RDCHAR ('KEYWORD',' ',KEYWORD)
      IF (PAR_ABORT()) GO TO 500     ! User requested abort
      CALL DSA_SEEK_FITS ('FITSFILE',KEYWORD,EXIST,ACCESS,NELM,LENSTR,
     :                                                          STATUS)
      IF (STATUS.NE.0) GO TO 500     ! Error exit
      VSTRING = ' '
      COMMENT = ' '
      IF (EXIST) THEN
C
C        The keyword already exists in the file, so we list it's current
C        value. We treat all numeric types as double precision, and handle
C        logical, character and 'unaccessable' types separately. Only
C        things like COMMENT and HISTORY and 'blank' should be multivalued,
C        but we don't test for that.
C
         CALL PAR_WRUSER(' ',IGNORE)
         DO ITEM=1,NELM
            LOGIT=.TRUE.
            IF (ACCESS.EQ.'L') THEN
               CALL DSA_GET_FITS_L ('FITSFILE',KEYWORD,ITEM,LVALUE,
     :                                               COMMENT,STATUS)
               IF (LVALUE) THEN
                  VSTRING='T'
               ELSE
                  VSTRING='F'
               END IF
            ELSE IF (ACCESS.EQ.'C') THEN
               CALL DSA_GET_FITS_C ('FITSFILE',KEYWORD,ITEM,CSTRING,
     :                                               COMMENT,STATUS)
               VSTRING='"'//CSTRING(:ICH_LEN(CSTRING))//'"'
            ELSE IF ((ACCESS.EQ.'I').OR.(ACCESS.EQ.'D').OR.
     :                    (ACCESS.EQ.'F').OR.(ACCESS.EQ.'S')) THEN
               CALL DSA_GET_FITS_D ('FITSFILE',KEYWORD,ITEM,DVALUE,
     :                                               COMMENT,STATUS)
               VSTRING=ICH_CD(DVALUE)
            ELSE IF (ACCESS.EQ.' ') THEN
               IF (ITEM.EQ.1) THEN
                  CALL PAR_WRUSER ('Keyword '//
     :                 KEYWORD(:ICH_LEN(KEYWORD))//
     :                 ' is not being held in a standard FITS manner',
     :                                                          IGNORE)
               END IF
               LOGIT=.FALSE.
            ELSE
               IF (ITEM.EQ.1) THEN
                  CALL PAR_WRUSER ('Keyword '//
     :             KEYWORD(:ICH_LEN(KEYWORD))//
     :             ' has an invalid access type "'//ACCESS//'"',IGNORE)
               END IF
               LOGIT=.FALSE.
            END IF
            IF (LOGIT) THEN
               VALUE=KEYWORD(:ICH_LEN(KEYWORD))//' = '//
     :             VSTRING(:ICH_LEN(VSTRING))//'  '//
     :               COMMENT(:ICH_LEN(COMMENT))
               CALL PAR_WRUSER(VALUE,IGNORE)
            END IF
         END DO
         CALL PAR_WRUSER(' ',IGNORE)
      END IF
C
C     Now get the new value for the keyword and comment
C
      CALL PAR_RDCHAR('VALUE',VSTRING,VALUE)
      VSTRING=COMMENT
      CALL PAR_RDCHAR('COMMENT',VSTRING,COMMENT)
C
C     See if the value is to be treated as a string or a logical value.
C
      CALL PAR_RDKEY('LOGICAL',.FALSE.,LOGIC)
      CALL PAR_RDKEY('STRING',.FALSE.,STRING)
C
C     Now see how we will treat the value, depending on the settings of
C     LOGICAL and STRING and the actual value supplied.
C
      IF (STRING) THEN
         ACCESS='C'
      ELSE IF (LOGIC) THEN
         ACCESS='L'
         INVOKE=ICH_FOLD(VALUE)
         IF (VALUE.EQ.'T') THEN
            LVALUE=.TRUE.
         ELSE IF (VALUE.EQ.'F') THEN
            LVALUE=.FALSE.
         ELSE
            CALL PAR_WRUSER('A logical value MUST be "T" or "F" ',
     :                                                       IGNORE)
            CALL DSA_WRUSER('and "'//VALUE(:ICH_LEN(VALUE))//
     :                              '" is neither of these.',IGNORE)
            GO TO 500     ! Error exit
         END IF
      ELSE
C
C        Here, we have to make up our own minds about how to treat the
C        supplied value. Can we interpret it as a number? (We have to
C        check for anything after the number as well, or "1.25 arcsec"
C        might look like a number, since ICH_NUMBD will return good
C        status). If it is a number, we treate it as an integer if we
C        can (this test is crude - we should really test over the
C        full integer range, but I can't remember it off-hand!). We
C        treat a floating point number as single precision depending on
C        the number of significant figures supplied.
C
         NSTAT=ICH_NUMBD(VALUE,1,' ',DVALUE,NSFIG,NEXT)
         IF (NEXT.GT.0) THEN
            IF (VALUE(NEXT:).NE.' ') NSTAT=1
         END IF
         IF (NSTAT.NE.0) THEN
            ACCESS='C'
         ELSE
            IF (NSFIG.GT.7) THEN
               ACCESS='D'
            ELSE
               ACCESS='F'
               FVALUE=DVALUE
            END IF
            IF ((DVALUE.GE.-32768.0).AND.(DVALUE.LE.32767.0)) THEN
               IF (DBLE(INT(DVALUE)).EQ.DVALUE) THEN
                  ACCESS='I'
                  IVALUE=DVALUE
               END IF
            END IF
         END IF
      END IF
C
C     Now we know how to treat the value, set it in the file.
C
      IF (ACCESS.EQ.'C') THEN
         CALL DSA_PUT_FITS_C ('FITSFILE',KEYWORD,VALUE,COMMENT,STATUS)
      ELSE IF (ACCESS.EQ.'L') THEN
         CALL DSA_PUT_FITS_L ('FITSFILE',KEYWORD,LVALUE,COMMENT,STATUS)
      ELSE IF (ACCESS.EQ.'I') THEN
         CALL DSA_PUT_FITS_I ('FITSFILE',KEYWORD,IVALUE,COMMENT,STATUS)
      ELSE IF (ACCESS.EQ.'F') THEN
         CALL DSA_PUT_FITS_F ('FITSFILE',KEYWORD,FVALUE,COMMENT,STATUS)
      ELSE IF (ACCESS.EQ.'D') THEN
         CALL DSA_PUT_FITS_D ('FITSFILE',KEYWORD,DVALUE,COMMENT,STATUS)
      END IF
C
C     Close everything down
C
  500 CONTINUE
      CALL DSA_CLOSE(STATUS)
C
      END
