      SUBROUTINE sgs_ATXR (R, NFI, NDP)
*+
*   - - - - -
*    A T X R
*   - - - - -
*
*   Format a real number and append to the text buffer.
*
*   The field width is limited in size - see coding.
*
*   Given:
*      R      r     real number to be formatted
*      NFI    i     format indicator:
*                     either  number of leading spaces (NFI.GE.0)
*                         or  minus the field width (NFI.LT.0)
*      NDP    i     number of decimal places
*                     if NDP.LT.0, only the integer part appears
*                     if NDP.EQ.0, the decimal point appears
*                     if NDP.GT.0, NDP digits appear after the point
*
*   Externals:
*      sgs_ATXB, sgs_ATEXT
*
*   P.T.Wallace, D.L.Terrett   Starlink   7 September 1991
*-

      IMPLICIT NONE

      REAL R
      INTEGER NFI,NDP

      INTEGER NF,NW,ILAST
      PARAMETER (NW=100)
      CHARACTER FMT*20,STRING*(NW)



*  Build format specification
      NF=MAX(0,NDP)
      WRITE (FMT,'('' (F'',I3,''.'',I3,'')'')') NW,NF

*  Format the number
      WRITE (STRING,FMT) R

*  Point to last character to be appended
      IF (NDP.GE.0) THEN
         ILAST=NW
      ELSE
         ILAST=NW-1
      END IF

*  Append the field in the appropriate manner
      IF (NFI.GE.0) THEN
         CALL sgs_ATXB(STRING(:ILAST),NFI)
      ELSE
         CALL sgs_ATEXT(STRING(MAX(ILAST+NFI+1,1):ILAST))
      END IF

      END
