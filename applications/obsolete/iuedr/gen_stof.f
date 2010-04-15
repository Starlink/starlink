      SUBROUTINE gen_STOF(SVALUE, FVALUE, STATUS)

*+
*
*   Name:
*      SUBROUTINE gen_STOF
*
*   Description:
*      Decode float value from string using Fortran Character File.
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
*      The string is read using G-format into the float parameter.
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
      REAL*8 FVALUE                 ! decoded value

      INTEGER STATUS              ! status return

*   Local variables:
      CHARACTER CVALUE*(MAXBUF)   ! Fortran 77 version of value string
      CHARACTER FORMAT*(8)        ! Fortran 77 format string

      INTEGER NCHAR               ! character count

      CALL gen_STOC(SVALUE, LEN(CVALUE), CVALUE, NCHAR)
      WRITE (FORMAT, '(I3)', IOSTAT=STATUS) NCHAR

      IF (STATUS.EQ.OK) THEN
         FORMAT = '(g'//FORMAT(1:3)//'.0)'
         READ (CVALUE(1:NCHAR), FORMAT, IOSTAT=STATUS) FVALUE
      END IF

      END
