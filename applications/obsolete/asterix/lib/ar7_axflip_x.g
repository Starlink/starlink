*+  AR7_AXFLIP_<T> - Flip a 7-D <TYPE> array about specified axes
      SUBROUTINE AR7_AXFLIP_<T>( DIMS, IN, FLIP, OUT, STATUS )
*    Description :
*     <description of what the subroutine does - for user info>
*    Method :
*     <description of how the subroutine works - for programmer info>
*
*    Authors :
*
*     David J. Allan ( BHVAD::DJA )
*
*    History :
*
*     13 Dec 89 : Original (DJA)
*      3 Jun 94 : Source changed to generic form (DJA)
*
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Import :
      INTEGER              DIMS(DAT__MXDIM)
      <TYPE>               IN(*)
      LOGICAL              FLIP(DAT__MXDIM)
*    Export :
      <TYPE>               OUT(*)
*    Status :
      INTEGER STATUS
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Call internal routine
      CALL AR7_AXFLIP_<T>_INT( DIMS, DIMS(1), DIMS(2), DIMS(3), DIMS(4),
     :                        DIMS(5), DIMS(6), DIMS(7), IN, FLIP, OUT )

      END



*+  AR7_AXFLIP_<T>_INT - Flip <TYPE> array about specified axes
      SUBROUTINE AR7_AXFLIP_<T>_INT( DIMS, L1, L2, L3, L4, L5, L6, L7,
     :                                                 IN, FLIP, OUT )
*    Description :
*     <description of what the subroutine does - for user info>
*    Method :
*     <description of how the subroutine works - for programmer info>
*    Authors :
*
*     David J. Allan ( BHVAD::DJA )
*
*    History :
*
*     13 Dec 89 : Original (DJA)
*
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Import :
      INTEGER              DIMS(DAT__MXDIM)
      INTEGER              L1,L2,L3,L4,L5,L6,L7
      LOGICAL              FLIP(DAT__MXDIM)
      <TYPE>               IN(L1,L2,L3,L4,L5,L6,L7)
*    Export :
      <TYPE>               OUT(L1,L2,L3,L4,L5,L6,L7)
*    Local variables :
*
      INTEGER              AI(DAT__MXDIM)
      INTEGER              II(DAT__MXDIM)
      INTEGER              SI(DAT__MXDIM)
      INTEGER              A,B,C,D,E,F,G,I
*-

*    Set up indice counters and direction vectors
      DO I = 1, DAT__MXDIM
         IF ( FLIP(I) ) THEN
            SI(I) = DIMS(I)
            AI(I) = -1
         ELSE
            SI(I) = 1
            AI(I) = 1
         END IF
      END DO

*    Perform data transfer
      II(7) = SI(7)
      DO G = 1, L7
       II(6) = SI(6)
       DO F = 1, L6
        II(5) = SI(5)
        DO E = 1, L5
         II(4) = SI(4)
         DO D = 1, L4
          II(3) = SI(3)
          DO C = 1, L3
           II(2) = SI(2)
           DO B = 1, L2
            II(1) = SI(1)
            DO A = 1, L1
             OUT(II(1),II(2),II(3),II(4),II(5),II(6),II(7))
     :                                    = IN(A,B,C,D,E,F,G)
             II(1) = II(1) + AI(1)
            END DO
            II(2) = II(2) + AI(2)
           END DO
           II(3) = II(3) + AI(3)
          END DO
          II(4) = II(4) + AI(4)
         END DO
         II(5) = II(5) + AI(5)
        END DO
        II(6) = II(6) + AI(6)
       END DO
       II(7) = II(7) + AI(7)
      END DO

      END
