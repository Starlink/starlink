      SUBROUTINE gen_OTOI(SVALUE, IVALUE, STATUS)

*+
*
*   Name:
*      SUBROUTINE gen_OTOI
*
*   Description:
*      Decode integer from octal string.
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
*      The string is read using O-format into the integer parameter.
*
*-

*   Implicit:
      IMPLICIT NONE

*   Global constants:
      INTEGER ARB                 ! arbitrary string length
      INTEGER OK                  ! OK status

      PARAMETER (ARB=100, OK=0)

*   Local constants:
      INTEGER MAXBUF              ! maximum size of FORTRAN text buffer

      PARAMETER (MAXBUF=132)

*   Import:
      BYTE SVALUE(ARB)            ! string to be scanned

*   Export:
      INTEGER IVALUE              ! decoded value
      INTEGER STATUS              ! status return

*   Local variables:
      CHARACTER CVALUE*(MAXBUF)   ! Fortran 77 version of value string
      CHARACTER FORMAT*(6)        ! Fortran 77 format string

      INTEGER NCHAR               ! character count

      CALL gen_STOC(SVALUE, LEN(CVALUE), CVALUE, NCHAR)
      WRITE (FORMAT, '(I3)', IOSTAT=STATUS) NCHAR

      IF (STATUS.EQ.OK) THEN
         FORMAT = '(o'//FORMAT(1:3)//')'
         READ (CVALUE(1:NCHAR), FORMAT, IOSTAT=STATUS) IVALUE
      END IF

      END
