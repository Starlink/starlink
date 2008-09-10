#if HAVE_CONFIG_H
#  include "config.h"
#endif

* The ISHIFT intrinsic is assumed not to clash
* with this function name. This function should probably 
* be renamed so it can not clash with the intrinsic function.

* This function assumes ISHFT is available but does not use
* ISHFTC for the left shift case.

* This function seems to assume 32 bit INTEGER

      INTEGER FUNCTION ISHIFT(IWORD,N)
      IMPLICIT NONE
      INTEGER IWORD
      INTEGER N
C IF N.GT.0, THEN SHIFT LEFT CIRCULAR
C IF N.LT.0, THEN SHIFT RIGHT ARITHMATIC

C
      INTEGER MASK
#if HAVE_BOZ
      DATA MASK/Z'FFFFFFFF'/
#elif HAVE_OLD_TYPELESS_BOZ
      DATA MASK/'FFFFFFFF'X/
#else
  Cannot form data mask, no BOZ constants are supported
#endif
C
      IF (N.GT.0) THEN
        ISHIFT = IOR(ISHFT(IWORD,N),ISHFT(IWORD,N-32))
C
      ELSE IF (N.EQ.0) THEN
        ISHIFT = IWORD
C
      ELSE
        ISHIFT = ISHFT(IWORD,N)
        IF(IWORD.LT.0) ISHIFT = IOR(ISHIFT,ISHFT(MASK,N+32))
C
      END IF
C
      RETURN
      END
