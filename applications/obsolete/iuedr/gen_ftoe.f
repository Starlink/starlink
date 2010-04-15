      SUBROUTINE gen_FTOE(FVALUE, FIELD, PREC, MAXC, SVALUE)

*+
*
*   Name:
*      SUBROUTINE gen_FTOE
*
*   Description:
*      Encode float value into string using E-format.
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
*         Converion to SGP/16 style.
*
*   Method:
*     The float is written using F-format into the string parameter.
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
      REAL*8 FVALUE                 ! decoded value

      INTEGER FIELD               ! field width
      INTEGER PREC                ! precision
      INTEGER MAXC                ! maximum size of string

*   Export:
      BYTE SVALUE(MAXC)           ! string to be filled

*   Local variables:
      CHARACTER CVALUE*(MAXBUF)   ! Fortran 77 version of value string
      CHARACTER FORMAT*(MAXFMT)   ! Fortran 77 format string

      INTEGER NCHAR               ! character count
      INTEGER CODE                ! local status

      WRITE (FORMAT, '(''(1pe'',I3,''.'',I3'')'')', IOSTAT=CODE)
     :       FIELD, PREC

      IF (CODE.EQ.OK) THEN
         WRITE (CVALUE(1:FIELD), FORMAT, IOSTAT=CODE) FVALUE

         IF (CODE.EQ.OK) THEN
            CALL gen_CTOS(CVALUE(1:FIELD), MAXC, SVALUE, NCHAR)
         ELSE
            CALL str_TERM(0, MAXC, SVALUE)
         END IF
      END IF

      END
