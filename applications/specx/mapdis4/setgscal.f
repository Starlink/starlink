*-----------------------------------------------------------------------

      SUBROUTINE SETGSCAL (ZMIN, ZMAX, GRLIMITS)

*  Routine to set greyscale limits -- may be either auto-scaled according
*  to max and min on map, or set directly from limits in FLAGCOMM

      IMPLICIT NONE

*     Formal parameters

      REAL    ZMIN, ZMAX
      REAL    GRLIMITS(2)

*     Include file

      INCLUDE 'FLAGCOMM'

*  Ok, go...

      IF (AUTOGREY) THEN
        GRLIMITS(1) = ZMIN
        GRLIMITS(2) = ZMAX
      ELSE
        GRLIMITS(1) = GREYLIM(1)
        GRLIMITS(2) = GREYLIM(2)
      END IF

*     Set the symbol that informs people of the range that was used last
      MAPLIMITS(1) = GRLIMITS(1)
      MAPLIMITS(2) = GRLIMITS(2)


D     TYPE *, ' -- setgscal --'
D     TYPE *, '    Autogrey flag = ',       AUTOGREY
D     TYPE *, '    Array min and max:    ', ZMIN, ZMAX
D     TYPE *, '    Manual limits:        ', GREYLIM
D     TYPE *, '    Greyscale limits set: ', GRLIMITS

      RETURN
      END

*-----------------------------------------------------------------------
