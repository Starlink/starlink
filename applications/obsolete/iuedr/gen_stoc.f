      SUBROUTINE gen_STOC( STR, MAXC, CHR, NCHAR )

*+
*
*   Name:
*      SUBROUTINE gen_STOC
*
*   Description:
*      Convert SWT string to character string.
*
*   Authors:
*      Sid Wright
*
*   History:
*      Sid Wright      07-AUG-80
*         AT4 version.
*      Paul Rees       26-OCT-88     IUEDR Vn. 2.0
*         Conversion to FORTRAN.
*      Paul Rees       23-MAY-89     IUEDR Vn. 2.1
*         Conversion to SGP/16 style.
*
*   Method:
*      The string is assumed terminated. It is converted into
*      character form and CHR is filled until its maximum size
*      reached (including terminator).
*
*-

*   Implict:
      IMPLICIT NONE

*   Global constants:
      BYTE EOS              ! string terminator
      BYTE STRESC           ! escape character

      PARAMETER (EOS=0, STRESC=92)

*   Import:
      INTEGER MAXC          ! maximum size of terminated string

      BYTE STR(MAXC)        ! SWT string

*   Export:
      CHARACTER CHR*(*)     ! Fortran 77 character string

      INTEGER NCHAR         ! number of characters before terminator

*.

      CHR = ' '
      NCHAR = 0

      DO WHILE (NCHAR.LT.MAXC)

         IF ( NCHAR+1 .GE. LEN( CHR ) ) THEN
            GO TO 200

         ELSE IF ( STR( NCHAR+1 ) .EQ. EOS ) THEN
            GO TO 200

         ELSE IF ( STR( NCHAR+1 ) .EQ. STRESC ) THEN
            GO TO 200

         ELSE
            NCHAR = NCHAR + 1
            CHR(NCHAR:NCHAR) = CHAR(STR(NCHAR))
         END IF
      END DO

 200  CONTINUE

      END
