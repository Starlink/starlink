      SUBROUTINE FTESTARG( ANSWER )

      CHARACTER*(*) ANSWER
      INTRINSIC IARGC
      INTEGER IARGC
      INTEGER I

      I = IARGC()

      print *,' Got ',I, ' arguments'
      CALL GETARG(1, ANSWER)

      END

