      SUBROUTINE str_TERM( CPOS, SIZE, STR )
*+
*   Name:
*      SUBROUTINE str_TERM
*
*   Description:
*      Plant terminator after specified position in string.
*
*   Authors:
*      Jack Giddings
*
*   History:
*      Jack Giddings      25-AUG-81
*         AT4 version.
*      Paul Rees          31-OCT-88     IUEDR Vn. 2.0
*         Conversion to FORTRAN.
*      Paul Rees          14-MAY-89     IUEDR Vn. 2.1
*         Final conversion to SGP/16 style.
*
*   Method:
*      The contents of the string are not examined, so that terminators
*      earlier on in the string than the specified position are
*      quite acceptible.
*
*-

*   Implicit:
      IMPLICIT NONE

*   Global constants:
      BYTE EOS           ! end of string marker
      PARAMETER (EOS=0)

*   Import:
      INTEGER CPOS       ! number of characters before terminator
      INTEGER SIZE       ! maximum number of characters

*   Import/Export:
      BYTE STR(SIZE)     ! string to be terminated
*.

      IF ( CPOS .LT. 0 ) THEN
         STR( 1 ) = EOS

      ELSE IF ( CPOS .LT. SIZE ) THEN
         STR( CPOS + 1 ) = EOS

      ELSE
         STR( SIZE ) = EOS
      END IF

      END
