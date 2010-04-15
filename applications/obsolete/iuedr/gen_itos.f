      SUBROUTINE gen_ITOS(IVALUE, FIELD, MAXC, SVALUE)

*+
*
*   Name:
*      SUBROUTINE gen_ITOS
*
*   Description:
*      Encode integer value into string.
*
*   Authors:
*      Jack Giddings
*
*   History:
*      Jack Giddings      03-JAN-82
*         AT4 version.
*      Paul Rees          25-OCT-88     IUEDR Vn. 2.0
*         Conversion to FORTRAN.
*      Paul Rees          23-MAY-89     IUEDR Vn. 2.1
*         Conversion to SGP/16 style.
*
*   Method:
*      The integer is written using I-format into the string parameter.
*
*-

*   Implicit:
      IMPLICIT NONE

*   Global constants:
      INTEGER OK                  ! OK status

      PARAMETER (OK=0)

*   Local constants:
      INTEGER MAXBUF              ! maximum size of FORTRAN text buffer
      INTEGER MAXFMT              ! maximum size of format string

      PARAMETER (MAXBUF=132, MAXFMT=80)

*   Import:
      INTEGER IVALUE              ! value to be coded
      INTEGER FIELD               ! field width
      INTEGER MAXC                ! maximum size of string

*   Export:
      BYTE SVALUE(MAXC)           ! string to be filled

*   Local variables:
      CHARACTER CVALUE*(MAXBUF)   ! Fortran 77 version of value string
      CHARACTER FORMAT*(MAXFMT)   ! Fortran 77 format string

      INTEGER NCHAR               ! character count
      INTEGER CODE                ! local status

      WRITE (FORMAT, '(''(i'',I3,'')'')', IOSTAT=CODE) FIELD

      IF (CODE.EQ.OK) THEN
         WRITE (CVALUE(1:FIELD), FORMAT, IOSTAT=CODE) IVALUE

         IF (CODE.EQ.OK) THEN
            CALL gen_CTOS(CVALUE(1:FIELD), MAXC, SVALUE, NCHAR)
         ELSE
            CALL str_TERM(0, MAXC, SVALUE)
         END IF
      END IF

      END
