      PROGRAM INQTXT
*                      GKS example program 8.5

*                      CPX, CPY     Concat point
*                      TXEXPX, TXEXPY  Text extent
      REAL CPX, CPY, TXEXPX(1:4), TXEXPY(1:4)

*                      Open GKS, open and activate workstation. The
*                      parameter for open GKS is system-dependent;
*                      a typical value has been given here.

      WRITE(6,1000)
 1000 FORMAT(' Connection identifier?')
      READ(5,'(I2)') ICONID
      WRITE(6,1010)
 1010 FORMAT(' Workstation type?')
      READ(5,'(I4)') IWTYPE

      CALL GOPKS (0, -1)
      CALL GOPWK (1 , ICONID , IWTYPE)
      CALL GACWK (1)
*                      End of standard opening sequence
*---------------------------------------------------------------------

      CALL GSCHH(0.03)
      CALL GTX(0.05, 0.5, 'A Change of Text Bundle ')
      CALL GQTXX(1, 0.05, 0.5, 'A Change of Text Bundle ',KERROR,
     :              CPX, CPY, TXEXPX, TXEXPY)
      CALL GSTXI(2)
      CALL GTX(CPX, CPY, 'is required')

*---------------------------------------------------------------------
*                      Deactivate and close workstation, close GKS
      CALL GDAWK (1)
      CALL GCLWK (1)
      CALL GCLKS
      END
