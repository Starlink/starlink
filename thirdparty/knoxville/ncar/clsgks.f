      SUBROUTINE CLSGKS
C
C IU(6), in IUTLCM, is the current metacode unit number.
C
      COMMON /IUTLCM/ IU(100)
C
C Deactivate the metacode workstation, close the workstation, and
C close GKS.
C
      CALL GDAWK (IU(6))
      CALL GCLWK (IU(6))
      CALL GCLKS
C
      RETURN
C
      END
