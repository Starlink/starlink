
C*GRTTER -- test whether device is user's terminal (Sun/Convex-UNIX)
C+
      SUBROUTINE GRTTER(STRING, SAME)
      CHARACTER*(*) STRING
      LOGICAL SAME
C
C Return a logical flag indicating whether the supplied device
C name is a name for the user's controlling terminal or not.
C (Some PGPLOT programs wish to take special action if they are
C plotting on the user's terminal.)
C
C Arguments:
C  STRING : (input) the device name to be tested.
C  SAME   : (output) .TRUE. is STRING contains a valid name for the
C           user's terminal; .FALSE. otherwise.
C--
C 18-Feb-1988
C-----------------------------------------------------------------------
      CHARACTER*64 T
      INTEGER L
C
      CALL GRTRML(T, L)
      SAME = (STRING.EQ.T(:L))
      END
