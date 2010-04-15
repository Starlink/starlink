      PROGRAM CONCOL
*                      GKS Example Program 2.13


*                      Open GKS, open and activate workstation.

      WRITE(*,1000)
 1000 FORMAT(' Connection identifier?')
      READ(*,'(I2)') ICONID
      WRITE(*,1010)
 1010 FORMAT(' Workstation type for colour device?')
      READ(*,'(I4)') IWTYPE

      CALL GOPKS (0, -1)
      CALL GOPWK (1 , ICONID , IWTYPE)
      CALL GACWK (1)
*                      End of standard opening sequence
*---------------------------------------------------------------------

*                      Set character height
      CALL GSCHH (0.1)
*                      Output three lines of text, each in a
*                      different colour by changing bundle index
      CALL GSTXI (1)
      CALL GTX   (0.1, 0.7, 'Pretty')
      CALL GSTXI (2)
      CALL GTX   (0.1, 0.5, 'Coloured')
      CALL GSTXI (3)
      CALL GTX   (0.1, 0.3, 'Text')

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
