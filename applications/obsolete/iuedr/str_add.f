      SUBROUTINE str_ADD(VALUE, MAXC, LINE, POS)

*+
*
*   Name:
*      SUBROUTINE str_ADD
*
*   Description:
*      Put a string into the line parameter at specified position.
*
*   Authors:
*      Jack Giddings
*
*   History:
*      Jack Giddings      03-JAN-82
*         AT4 version.
*      Paul Rees          21-OCT-88     IUEDR Vn. 2.0
*         Conversion to FORTRAN.
*      Paul Rees          22-MAY-89     IUEDR Vn. 2.1
*         Conversion to SGP/16 style.
*
*   Method:
*      The VALUE string is copied into the supplied line at the
*      specfied character position. The line is terminated
*      if this value increases its length.
*
*-

*   Implicit:
      IMPLICIT NONE

*   Import:
      BYTE VALUE(256)     ! value string

      INTEGER MAXC        ! maximum size of line string

*   Import/Export:
      BYTE LINE(MAXC)     ! line to be modified

      INTEGER POS         ! character position

*   External references:
      INTEGER str_LEN     ! string length

      IF (POS.LT.1) THEN

         POS = 1
         CALL str_TERM(0, MAXC, LINE)

      END IF

      CALL str_PLANT(VALUE, POS, MAXC, LINE)
      POS = MIN(POS + str_LEN(VALUE), str_LEN(LINE) + 1)

      END
