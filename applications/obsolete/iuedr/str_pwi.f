      SUBROUTINE str_PWI(IVALUE, RIGHT, FIELD, EDIT, MAXC, VALUE)

*+
*
*   Name:
*      SUBROUTINE str_PWI
*
*   Description:
*      Encode integer value into parameter string using edit parameters.
*
*   Authors:
*      Jack Giddings
*
*   History:
*      Jack Giddings      03-JAN-82
*         AT4 version.
*      Paul Rees          23-OCT-88     IUEDR Vn. 2.0
*         Conversion to FORTRAN.
*      Paul Rees          22-MAY-89     IUEDR Vn. 2.1
*         Conversion to SGP/16 style.
*
*   Method:
*      IVALUE is planted in the parameter string as specified by the
*      edit parameters.
*      The allowed formats are I, H and O.
*      The EDIT parameter defaults to I-format.
*
*-

*   Implicit:
      IMPLICIT NONE

*   Import:
      INTEGER IVALUE        ! integer to be coded

      LOGICAL RIGHT         ! whether right justified

      INTEGER FIELD         ! field size

      BYTE EDIT             ! edit character

      INTEGER MAXC          ! maximum size of VALUE

*   Export:
      BYTE VALUE(MAXC)      ! resulting value string

*   External references:
      INTEGER str_INDEX     ! index of character in string

*   Local variables:
      BYTE LEDIT            ! local copy of EDIT
      BYTE LOCAL(256)       ! local copy of STR

      INTEGER IFIELD        ! concise field size

*   Default edit to B-format
      IF (str_INDEX('ioh\\', EDIT).EQ.0) THEN

         LEDIT = 105

      ELSE

         LEDIT = EDIT

      END IF

*   Choose format
      IF (LEDIT.EQ.105) THEN

         CALL str_DESI(IVALUE, IFIELD)
         CALL gen_ITOS(IVALUE, IFIELD, 256, LOCAL)

      ELSE IF (LEDIT.EQ.111) THEN

         CALL str_DESO(IVALUE, IFIELD)
         CALL gen_ITOO(IVALUE, IFIELD, 256, LOCAL)

      ELSE IF (LEDIT.EQ.104) THEN

         CALL str_DESH(IVALUE, IFIELD)
         CALL gen_ITOH(IVALUE, IFIELD, 256, LOCAL)

      END IF

      CALL str_JUST(LOCAL, RIGHT, FIELD, MAXC, VALUE)

      END
