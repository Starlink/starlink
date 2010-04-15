      SUBROUTINE str_PWF(FVALUE, RIGHT, FIELD, FIXED, PREC, EDIT, MAXC,
     :                   VALUE)

*+
*
*   Name:
*      SUBROUTINE str_PWF
*
*   Description:
*      Encode floating point value into parameter string using the
*      edit parameters.
*
*   Authors:
*      Jack Giddings
*
*   History:
*      Jack Giddings      03-JAN-82
*         AT4 version.
*      Paul Rees          21-OCT-88     IUEDR Vn. 2.0
*         Conversion to FORTRAN.
*      Paul Rees          20-MAY-89     IUEDR Vn. 2.1
*         Conversion to SGP/16 style.
*
*   Method:
*      FVALUE is planted in the parameter string as specified by the
*      edit parameters.
*      The allowed formats are I, F, E, G and B.
*      The EDIT parameter defaults to B-format.
*
*-

*   Implicit:
      IMPLICIT NONE

*   Global constants:
      BYTE LETB             ! ASCII "b"
      BYTE LETE             ! ASCII "e"
      BYTE LETF             ! ASCII "f"
      BYTE LETG             ! ASCII "g"
      BYTE LETI             ! ASCII "i"
      BYTE PERIOD           ! ASCII "."

      PARAMETER (LETB=98, LETE=101, LETF=102, LETG=103, LETI=105,
     :           PERIOD=46)

      INTEGER MAXTOK        ! maximum length of token string

      PARAMETER (MAXTOK=256)

*   Import:
      REAL*8 FVALUE           ! float to be coded

      LOGICAL RIGHT         ! whether right justified

      INTEGER FIELD         ! field size

      LOGICAL FIXED         ! whether fixed point

      INTEGER PREC          ! precision

      BYTE EDIT             ! edit character

      INTEGER MAXC          ! maximum size of VALUE

*   Export:
      BYTE VALUE(MAXC)      ! resulting value string

*   External references:
      INTEGER str_INDEX     ! index of character in string

*   Local variables:
      BYTE LEDIT            ! local copy of EDIT
      BYTE LOCAL(MAXTOK)    ! local copy of STR

      INTEGER CSIGN         ! number of sign characters
      INTEGER EXPON         ! exponent
      INTEGER EFIELD        ! field size for E-format
      INTEGER EPREC         ! precision for E-format
      INTEGER I             ! string index
      INTEGER FFIELD        ! field size for F-format
      INTEGER FPREC         ! precision for F-format
      INTEGER SIGDEC        ! number of significant decimals

*   Default edit to B-format
      IF (str_INDEX('ifegb\\', EDIT).EQ.0) THEN
         LEDIT = LETB
      ELSE
         LEDIT = EDIT
      END IF

*   Select a format
      IF (LEDIT.EQ.LETI) THEN
         CALL gen_FTOS(FVALUE, 80, 40, MAXTOK, LOCAL)
         I = str_INDEX(LOCAL, PERIOD)
         CALL str_TERM(I-1, MAXTOK, LOCAL)
         CALL str_RMBLK(LOCAL)
      ELSE

*      Get precision requirements
         CALL str_DESF(FVALUE, CSIGN, SIGDEC, EXPON)

*      Bandwidth and precision for E-format
         IF (FIXED) THEN

            IF (PREC.LT.0) THEN
               EFIELD = CSIGN + 2 + SIGDEC + 4
               EPREC = MIN(30, SIGDEC)
            ELSE
               EFIELD = CSIGN + 2 + PREC + 4
               EPREC = MIN(30, PREC)
            END IF

         ELSE

            IF (PREC.LT.0) THEN
               EFIELD = CSIGN + 2 + SIGDEC + 4
               EPREC = MIN(30, SIGDEC)
            ELSE
               EFIELD = CSIGN + 2 + MAX(PREC-1, 0) + 4
               EPREC = MIN(30, MAX(PREC-1, 0))
            END IF
         END IF

*      Field size and precision for F-format
         IF (FIXED) THEN

            IF (PREC.LT.0) THEN
               FPREC = MIN(30, MAX(0, SIGDEC-EXPON))
            ELSE
               FPREC = MIN(30, PREC)
            END IF

            FFIELD = CSIGN + 2 + MAX(EXPON, 0) + FPREC
         ELSE

            IF (PREC.LT.0) THEN
               FPREC = MIN(30, MAX(0, SIGDEC-EXPON))
            ELSE
               FPREC = MIN(30, MAX(0, PREC-1-EXPON))
            END IF

            FFIELD = CSIGN + 2 + MAX(EXPON, 0) + FPREC
         END IF

         IF (EXPON.LT.0) FFIELD = FFIELD + 1

*      Encode number
         IF (LEDIT.EQ.LETF) THEN
            CALL gen_FTOS(FVALUE, FFIELD, FPREC, MAXTOK, LOCAL)
         ELSE IF (LEDIT.EQ.LETE) THEN
            CALL gen_FTOE(FVALUE, EFIELD, EPREC, MAXTOK, LOCAL)
         ELSE IF (LEDIT.EQ.LETG .OR. LEDIT.EQ.LETB) THEN

            IF (FFIELD.GT.EFIELD) THEN
               CALL gen_FTOE(FVALUE, EFIELD, EPREC, MAXTOK, LOCAL)
            ELSE
               CALL gen_FTOS(FVALUE, FFIELD, FPREC, MAXTOK, LOCAL)
            END IF
         END IF

         IF (LEDIT.EQ.LETB) CALL str_RMBLK(LOCAL)

      END IF

      CALL str_JUST(LOCAL, RIGHT, FIELD, MAXC, VALUE)

      END
