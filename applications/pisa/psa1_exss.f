      SUBROUTINE PSA1_EXSS( MAP, IQL, IQH, IYL, IYH, NYOUT, SUBSID,
     :                      XPEAK, XBUF, STATUS )

      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
* map the appropriate part of map array into our box and subtract the
* sky value
*
* CODE FRAGMENT FROM PISAFIT - REMOVED TO MAKE INPUT ARRAY DYNAMIC
      INTEGER * 2 MAP( * )
      INTEGER IQL
      INTEGER IQH
      INTEGER NYOUT
      INTEGER SUBSID
      INTEGER IYL
      INTEGER IYH
      REAL XPEAK
      REAL XBUF( * )
      REAL XX
      INTEGER I
      INTEGER J
      INTEGER II
      INTEGER JJ
      INTEGER STATUS
      IF( STATUS .NE. SAI__OK ) RETURN

      DO 1 I = IQL, IQH
         II = ( I - 1 ) * NYOUT
         JJ = ( I - IQL ) * SUBSID
         DO 2 J = IYL, IYH
            XX = MAP( II + J ) - XPEAK
            XBUF( JJ + J - IYL + 1 ) = XX
 2       CONTINUE
 1    CONTINUE
      END
* $Id$
