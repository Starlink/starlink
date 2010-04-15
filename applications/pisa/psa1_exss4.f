      SUBROUTINE PSA1_EXSS4( MAP, IQL, IQH, IYL, IYH, NYOUT, SUBSID,
     :                       XPEAK, XBUF, STATUS )

      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'PRM_PAR'

*     CODE FRAGMENT FROM PISAFIT - REMOVED TO MAKE INPUT ARRAY DYNAMIC
*     Map the appropriate part of map array into our box and subtract the
*     sky value. "Replace bad pixels by zero convert to use INTEGER*4
*     (P.W.Draper: 14-July 1995)."

      INTEGER*4 MAP( * )
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
            IF ( MAP( II + J ) .EQ. VAL__BADI ) THEN
               XBUF( JJ + J - IYL + 1 ) = 0
            ELSE
               XX = MAP( II + J ) - XPEAK
               XBUF( JJ + J - IYL + 1 ) = XX
            END IF
 2       CONTINUE
 1    CONTINUE
      END
*     $Id$
