*+  CAR_CHOI1 - Output option codes to the environment.
      SUBROUTINE CAR_CHOI1 (NCHOIC, DESCR, RESPS, ABBR, STATUS)
*    Description :
*     Subroutine to send a series of option codes, with descriptions
*     to the environment.
*    Invocation :
*     CALL CAR_CHOI1 (NCHOIC, DESCR, RESPS, ABBR; STATUS)
*    Parameters :
*     NCHOIC  =  INTEGER (READ)
*           Number of choices to be output.
*     DESCR(NCHOIC)  =  CHARACTER*(*) (READ)
*           Description for each choice.
*     RESPS(NCHOIC)  =  CHARACTER*(*) (READ)
*           Response to select each each choice.
*     ABBR  =  LOGICAL (READ)
*           Flag; are abbreviations permitted in the responses?
*     STATUS  =  INTEGER (UPDATE)
*           Running status.
*    Method :
*     <description of how the subroutine works>
*    Bugs :
*     None known.
*    Authors :
*     A C Davenhall.      (ROE::ACD, LEI::ACD)
*    History :
*     30/4/86: Original version (based on CHOICE1 which       (ROE::ACD)
*              interfaced to the interim environment 21/7/83).
*     24/9/93: Converted to StarBase.                         (LEI::ACD)
*     31/1/94: Added message about column names being case-   (LEI::ACD)
*              sensitive.
*     9/2/94:  Removed message about column names being case- (LEI::ACD)
*              sensitive.
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Import :
      INTEGER
     :  NCHOIC             ! Number of choices to be output.
      CHARACTER
     :  DESCR(NCHOIC)*(*), ! Description for each choice.
     :  RESPS(NCHOIC)*(*)  ! Response to select each each choice.
      LOGICAL
     :  ABBR      ! Flag; are abbreviations permitted in the responses?
*    Status :
      INTEGER
     :  STATUS    ! Running status.
*    External references :
      INTEGER
     :  CHR_LEN
*    Local Constants :
      CHARACTER
     :  WRKBUF*7  ! Buffer used in assembling the row of dots.
      PARAMETER
     : (WRKBUF='  ...  ')
*    Local variables :
      CHARACTER
     :  OUTBUF*75 ! Output buffer.
      INTEGER
     :  LOOP,     ! Loop index.
     :  WORK,     ! Position in output buffer.
     :  LENGTH,   ! Length of description of current choice.
     :  CHOICE    ! No. of current choice.
*-

      IF (STATUS .EQ. SAI__OK) THEN
         DO CHOICE = 1, NCHOIC
            OUTBUF = ' '

            DO LOOP = 1, 10
               WORK = ((LOOP-1) * 6) + 1
               OUTBUF(WORK : WORK+6) = WRKBUF
            END DO

            IF (CHOICE .EQ. 1) OUTBUF(51 : 59) = '(default)'

            LENGTH = CHR_LEN (DESCR(CHOICE))
            OUTBUF(1 : LENGTH) = DESCR(CHOICE)(1 : LENGTH)

            LENGTH = CHR_LEN (RESPS(CHOICE))
            OUTBUF(61 : 61+LENGTH-1) = RESPS(CHOICE)(1 : LENGTH)

            CALL MSG_OUT (' ', OUTBUF, STATUS)
         END DO

         IF (ABBR) THEN
            CALL MSG_OUT (' ', '(Some abbreviations permitted).',
     :        STATUS)
         END IF

      END IF

      END
