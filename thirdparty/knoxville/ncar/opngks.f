      SUBROUTINE OPNGKS
C
C 10-DEC-1991 P.C.T. Rees (STARLINK):
C    Changed argument list to reflect GKS 7.4 change to GOPKS.
C    Changed error output unit number.
      INTEGER KERRFL
      PARAMETER ( KERRFL = 0 )
C
C IU(6), in IUTLCM, is the current metacode unit number.
C
      COMMON /IUTLCM/ IU(100)
C
C Force all required BLOCKDATA's to load.
C
      EXTERNAL UERRBD,UTILBD
C
C Open GKS, define a workstation, and activate the workstation.
C
      CALL GOPKS (KERRFL,-1)
      CALL GOPWK (IU(6),2,1)
      CALL GACWK (IU(6))
C
      RETURN
C
      END
