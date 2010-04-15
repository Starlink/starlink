      PROGRAM QDEM
*                      GKS Example Program 2.7


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

      CALL GSCHH  (0.2)
      CALL GTX (0.1, 0.5, 'Qq')
      CALL GSCHH  (0.018)
      CALL GTX (0.1, 0.28,'Q is a beautiful but unnecessary')
      CALL GTX (0.1, 0.24,'letter in English, because')
      CALL GTX (0.1, 0.20,'it can be replaced by CW or KW.')
      CALL GSCHH (0.01)
      CALL GTX (0.1, 0.10,
     :'The Romans tried to abolish it, but their W was also a U.')
      CALL GTX (0.25, 0.85,'Output of Program Q')

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

