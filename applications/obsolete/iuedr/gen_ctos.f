      SUBROUTINE gen_CTOS( CHR, MAXC, STR, NCHAR )
*+
*   Name:
*      SUBROUTINE gen_CTOS
*
*   Description:
*      Convert character string to SWT string.
*
*   Authors:
*      Sid Wright
*
*   History:
*      Sid Wright      07-AUG-80
*         AT4 version.
*      Paul Rees       25-OCT-88     IUEDR Vn. 2.0
*         Conversion to FORTRAN.
*      Paul Rees       23-MAY-89     IUEDR Vn. 2.1
*         Conversion to SGP/16 style.
*
*   Method:
*      The supplied character string is converted into SWT form.
*      The string ends either at the size limit of the character
*      variable, or at a terminator, or when the output string
*      is filled (terminated).
*
*      The terminator is dealt with here explicitly.
*
*-

*   Implicit:
      IMPLICIT NONE

*   Global constants:
      BYTE EOS              ! string terminator

      PARAMETER (EOS=0)

*   Import:
      CHARACTER CHR*(*)     ! Fortran 77 character string

      INTEGER MAXC          ! maximum size of terminated string

*   Export:
      BYTE STR(MAXC)        ! SWT string

      INTEGER NCHAR         ! number of characters before terminator

      NCHAR = 0

      DO WHILE (NCHAR.LT.LEN(CHR))

         IF (NCHAR+1.EQ.MAXC) THEN
            GO TO 200
         ELSE IF (CHR(NCHAR+1:NCHAR+1).EQ.CHAR(EOS)) THEN
            GO TO 200
         ELSE
            NCHAR = NCHAR + 1
            STR(NCHAR) = ICHAR(CHR(NCHAR:NCHAR))
         END IF
      END DO

 200  CONTINUE
      STR(NCHAR+1) = EOS

      END
