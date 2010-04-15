*+ IRUSER - ROE User Commands for DIPSO
      SUBROUTINE IRUSER
     :(CMD, PARAMS, MAXPT, MAXBRK, X, Y, NPT,
     :  BRKS, NBRK, TITLE, OK)
*    Type Definitions :
      IMPLICIT NONE
*    Import :
      CHARACTER*(*) CMD     ! Command name
      CHARACTER*(*) PARAMS  ! Parameter value string
      INTEGER MAXPT         ! Maximum number of spectrum points
      INTEGER MAXBRK        ! Maximum number of break points
*    Import-Export :
      REAL X(*)
      REAL Y(*)
      INTEGER NPT
      INTEGER BRKS(*)
      INTEGER NBRK
      CHARACTER*(*) TITLE   ! Associated character string

      LOGICAL OK

      WRITE (*,
     : '(''   '',A,'':  unavailable in this implementation'' //
     : '' of DIPSO'')') CMD

      OK = .FALSE.

      END
