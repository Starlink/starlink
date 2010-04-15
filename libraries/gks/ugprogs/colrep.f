      PROGRAM COLREP
*                      GKS Example Program 2.14

*                      The following variable(s) are defined in the
*                      included file
*                      GCOLOR, GSET
      INCLUDE 'GKS_PAR'

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

*                      Check that the workstation supports colour
      CALL GQCF (IWTYPE, KERROR, NCOL, JCOLAV, NPCI)
      IF (JCOLAV .EQ. GCOLOR) THEN
*                      Device supports colour: check number of colours
         IF (NCOL .GE. 3) THEN
*                      Set representation for colour indices 1,2, 3
            CALL GSCR (1, 1, 1.0, 0.0, 0.0)
            CALL GSCR (1, 2, 0.0, 1.0, 0.0)
            CALL GSCR (1, 3, 0.0, 0.0, 1.0)
*                      Inquire text representations and reset colour
*                      for text indices 1,2,3 to 1,2,3
            DO 10 KTXI=1,3
               CALL GQTXR (1, KTXI, JTYPE, KERROR, KFONT,
     :                        JPREC, CHXP, CHSP, KTXCOL)
               IF (KTXCOL .NE. KTXI) CALL GSTXR (1, KTXI,
     :                     KFONT, JPREC, CHXP, CHSP, KTXCOL)
   10       CONTINUE
*                      Now produce text output as before
            CALL GSCHH (0.1)
            CALL GSTXI (1)
            CALL GTX (0.1, 0.7, 'Pretty')
            CALL GSTXI (2)
            CALL GTX (0.1, 0.5, 'Coloured')
            CALL GSTXI (3)
            CALL GTX (0.1, 0.3, 'Text')
         ENDIF
      ELSE
*                      Device doesn't support colour
         WRITE (0,*) 'Example COLREP: workstation does not',
     :   ' support colour'
      ENDIF

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
