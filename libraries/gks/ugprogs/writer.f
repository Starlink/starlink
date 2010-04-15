      PROGRAM WRITER
*                      GKS Example Program 7.1

      INTEGER MOUT
      PARAMETER (MOUT = 12)
*                      Declare metafile output workstation values
      CHARACTER*80 MFDR(1)
*                      Data for Ship's Outline
      INTEGER NSHIP
      PARAMETER (NSHIP = 18)
      REAL XSHIP(NSHIP), YSHIP(NSHIP)
      DATA XSHIP/0.20,0.10,0.40,0.42,0.50,0.52,0.58,0.56,0.64,
     :           0.66,0.72,0.70,0.78,0.78,0.92,0.92,0.90,0.20/
     :     YSHIP/0.12,0.22,0.20,0.26,0.26,0.32,0.32,0.26,0.26,
     :           0.32,0.32,0.26,0.26,0.20,0.20,0.14,0.12,0.12/

*                      Open GKS, open and activate workstation.

      WRITE(*,1000)
 1000 FORMAT(' Connection identifier?')
      READ(*,'(I2)') ICONID
      WRITE(*,1010)
 1010 FORMAT(' Workstation type?')
      READ(*,'(I4)') IWTYPE

      CALL GOPKS (0, -1)
      CALL GOPWK (1 , ICONID , IWTYPE)
      CALL GACWK (1)
*                      End of standard opening sequence
*---------------------------------------------------------------------

*                      Open a file to receive the metafile.
      OPEN (UNIT = MOUT, FILE = 'myfile.meta', STATUS = 'NEW')
*                      OPEN and ACTIVATE a metafile output workstation
*                      as workstation 2.
      CALL GOPWK (2 , MOUT, 50)
      CALL GACWK (2)

*                      Send user data
      WRITE(MFDR(1),'(A9)') 'SHIP DATA'
      CALL GWITM (2, 150, 9, 1, MFDR)
*                      Send graphical data
      CALL GPL (NSHIP, XSHIP, YSHIP)
*                      Closedown
      CALL GDAWK (2)
      CALL GCLWK (2)

*---------------------------------------------------------------------
*                      Update workstation and await operator action
*                      before finishing
      CALL GUWK(1, 1)
      PAUSE
*                      Deactivate and close workstation, close GKS
      CALL GDAWK (1)
      CALL GCLWK (1)
      CALL GCLKS
      END
