*+  AR7_AXSWAP_<T> - Swap <TYPE> array about specified axes
      SUBROUTINE AR7_AXSWAP_<T>( DIMS, IN, SAX, ODIMS, OUT, STATUS )
*    Description :
*    Method :
*
*    Authors :
*
*     David J. Allan ( BHVAD::DJA )
*
*    History :
*
*     13 Dec 89 : Original (DJA)
*      6 Jun 94 : Code made generic (DJA)
*
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*
*    Import :
*
      INTEGER              DIMS(DAT__MXDIM)
      <TYPE>               IN(*)
      INTEGER              SAX(DAT__MXDIM)
      INTEGER              ODIMS(DAT__MXDIM)

*    Export :
      <TYPE>               OUT(*)
*    Status :
      INTEGER STATUS
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Call internal routine
      CALL AR7_AXSWAP_<T>_INT( DIMS, DIMS(1), DIMS(2), DIMS(3), DIMS(4),
     :                         DIMS(5), DIMS(6), DIMS(7), IN, SAX,
     :                         ODIMS(1), ODIMS(2), ODIMS(3), ODIMS(4),
     :                         ODIMS(5), ODIMS(6), ODIMS(7), OUT )

      END



*+  AR7_AXSWAP_<T>_INT - Swap <TYPE> array about specified axes
      SUBROUTINE AR7_AXSWAP_<T>_INT( DIMS, L1, L2, L3, L4, L5, L6, L7, IN,
     :                            SAX, O1, O2, O3, O4, O5, O6, O7, OUT )
*    Description :
*    Method :
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
*
*    Import :
*
      INTEGER              DIMS(DAT__MXDIM)
      INTEGER              L1,L2,L3,L4,L5,L6,L7
      INTEGER              SAX(DAT__MXDIM)
      <TYPE>               IN(L1,L2,L3,L4,L5,L6,L7)
      INTEGER              O1,O2,O3,O4,O5,O6,O7
*
*    Export :
*
      <TYPE>               OUT(O1,O2,O3,O4,O5,O6,O7)
*
*    Local variables :
*
      INTEGER              II(DAT__MXDIM)
      INTEGER              SI(DAT__MXDIM)
      INTEGER              A,B,C,D,E,F,G
*-

*    Store position of old axis in new object
      DO A = 1, DAT__MXDIM
         DO B = 1, DAT__MXDIM
            IF ( SAX(B) .EQ. A ) SI(A) = B
         END DO
      END DO

*    Perform data transfer
      II(SI(7)) = 1
      DO G = 1, L7
       II(SI(6)) = 1
       DO F = 1, L6
        II(SI(5)) = 1
        DO E = 1, L5
         II(SI(4)) = 1
         DO D = 1, L4
          II(SI(3)) = 1
          DO C = 1, L3
           II(SI(2)) = 1
           DO B = 1, L2
            II(SI(1)) = 1
            DO A = 1, L1
             OUT(II(1),II(2),II(3),II(4),II(5),II(6),II(7))
     :                                    = IN(A,B,C,D,E,F,G)
             II(SI(1)) = II(SI(1)) + 1
            END DO
            II(SI(2)) = II(SI(2)) + 1
           END DO
           II(SI(3)) = II(SI(3)) + 1
          END DO
          II(SI(4)) = II(SI(4)) + 1
         END DO
         II(SI(5)) = II(SI(5)) + 1
        END DO
        II(SI(6)) = II(SI(6)) + 1
       END DO
       II(SI(7)) = II(SI(7)) + 1
      END DO

      END
