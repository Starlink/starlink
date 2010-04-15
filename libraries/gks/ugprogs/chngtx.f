      PROGRAM CHNGTX
*                      GKS Example Program 8.5

*                      The following variable(s) are defined in the
*                      included file
*                      GSTRKP
      INCLUDE 'GKS_PAR'
      REAL CPX, CPY, CTX, CTY, BOXX(4), BOXY(4)

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

*                      Set text bundles
      CALL GSTXR (1,4,-104,GSTRKP,1.0,0.0,1)
      CALL GSTXR (1,5,-105,GSTRKP,1.0,0.0,1)
      CALL GSTXR (1,6,-106,GSTRKP,1.0,0.0,1)
*                      Set text index and character height
      CALL GSTXI(4)
      CALL GSCHH  (0.018)
      CALL GTX (0.1, 0.7, 'You can use routine')
*                      Inquire text extent, change text index (font)
*                      and continue from the concatenation point
      CALL GQTXX (1,0.1, 0.7, 'You can use routine', IER,
     :             CPX,CPY , BOXX,BOXY)
      IF (IER.NE.0) GO TO 1
      CALL GSTXI (6)
      CALL GTX (CPX,CPY, ' GQTXX ')
*                      Inquire text extent, return to original text index
*                      and continue from the concatenation point
      CALL GQTXX (1,CPX,CPY,' GQTXX ', IER, CTX,CTY, BOXX,BOXY)
      IF (IER.NE.0) GO TO 1
      CALL GSTXI (4)
      CALL GTX (CTX,CTY,'to')
      CALL GTX (0.1, 0.6, 'change the ')
*                      Inquire text extent, set a new text index (font)
*                      and continue from the concatenation point
      CALL GQTXX (1,0.1, 0.6, 'change the ',IER, CPX,CPY, BOXX,BOXY)
      IF (IER.NE.0) GO TO 1
      CALL GSTXI (5)
      CALL GTX (CPX,CPY, 'font')
*                      Inquire text extent, return to original text index
*                      and continue from the concatenation point
      CALL GQTXX (1,CPX,CPY,'font',IER, CTX,CTY, BOXX,BOXY)
      IF (IER.NE.0) GO TO 1
      CALL GSTXI (4)
      CALL GTX (CTX,CTY,' or some other')
*                      new line
      CALL GTX (0.1, 0.5,'text')
*                      Inquire text extent, change character height
*                      and continue from the concatenation point
      CALL GQTXX (1,0.1, 0.5,'text',IER, CPX,CPY, BOXX,BOXY)
      CALL GSCHH (0.027)
      CALL GTX (CPX,CPY,' attribute')
*                      Inquire text extent, return to original character he
*                      and continue from the concatenation point
      CALL GQTXX (1,CPX,CPY,' attribute ',IER, CTX,CTY, BOXX,BOXY)
      CALL GSCHH (0.018)
      CALL GTX (CTX,CTY,'within a line.')
    1 CONTINUE

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

