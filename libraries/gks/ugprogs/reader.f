      PROGRAM READER
*                      GKS Example Program 7.2

      INTEGER  MINP
      PARAMETER (MINP = 13)
*                      Declare metafile input workstation values
      INTEGER  NCMFDR, MFTYPE
      CHARACTER*80 MFDR(200)

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

*                      Connect the metafile to the program
      OPEN (UNIT = MINP, FILE = 'myfile.meta', STATUS = 'OLD')
*                      OPEN and ACTIVATE a metafile input workstation
*                      as workstation 2.
      CALL GOPWK (2, MINP, 10)
*                      Scan through input, calling INTERPRET
*                      ITEM to drive output workstations
   10 CALL GGTITM (2, MFTYPE, NCMFDR)
*                      Test to see if item too long
      IF (NCMFDR.GT.16000) THEN
        WRITE(*,2000) NCMFDR
 2000   FORMAT(' ITEM LENGTH = ',I3,' >16000')
*                      Skip item
        CALL GRDITM (2, 0, 200, MFDR)
      ELSE
        CALL GRDITM (2, NCMFDR, 200, MFDR)
        IF (MFTYPE.GT.100) THEN
          WRITE (*,2010) MFTYPE
 2010     FORMAT (' USER ITEM ',I3,' NOT INTERPRETED BY THIS EXAMPLE')
        ELSE
          CALL GIITM (MFTYPE, NCMFDR, 200,  MFDR)
        ENDIF
      ENDIF
      IF (MFTYPE.NE.0) GO TO 10
*                      At end of run, close MI workstation
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
