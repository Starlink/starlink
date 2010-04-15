      SUBROUTINE GRSCR(IC,R,G,B)
*+
*    - - - - - - -
*      G R S C R
*    - - - - - - -
*
*   Sets the colour of index IC if the current device has a dynamically
*   alterable colour table.
*
*   Given
*      IC      i      Colour index
*      R       r      R
*      G       r      G
*      B       r      B
*
*   Read from COMMON
*      GRCIDE  i      Current device id
*      GRWKID  i()    Workstation id
*      GRTYP   i()    Workstation type
*
*   Constants fropm GKS_PAR
*      GIMM    i      Immediate
*      GMONOC  i      Monochrome
*      GEMPTY  i      Display surface empty
*
*   D.L.Terrett  Starlink  Aug 1987
*
*   1 Jun 1988  DLT  Correct colour transformation for monochrome devices
*+
      IMPLICIT NONE
      INCLUDE 'grecom.inc'

      INCLUDE 'PGP_ERR'

      INCLUDE 'GKS_PAR'


      REAL R,G,B
      INTEGER IC

      REAL RT,GT,BT
      INTEGER IERR,NCOLI,ICOLA,NPCI,NDEF
      INTEGER IPLBUN,IPMBUN,ITXBUN,IFABUN,IPAREP,ICOLRP,IWKTR
      INTEGER MPLBTE,MPMBTE,MTXBTE,MFABTE,MPAI,MCOLI
      INTEGER IDEFMO,IREGMO,IEMPTY,NFRAME
      LOGICAL INDOK

      IF (GRCIDE.EQ.0) THEN
         CALL ERR_REP('GRNODO', 'GRSCR - No PGPLOT device open',
     :   GRNODO)
      ELSE

*   Check that the colour index is valid, ie. it already exists or the
*   number of colour indices defined is less than the maximum number
*   allowed.
         INDOK = .FALSE.
         CALL GQCR(GRWKID(GRCIDE),IC,GSET,IERR,RT,GT,BT)
         IF (IERR.EQ.0) THEN
            INDOK = .TRUE.
         ELSE
            CALL GQECI(GRWKID(GRCIDE),1,IERR,NDEF,ICOLA)
            IF (IERR.NE.0) THEN
               CALL GRQREP('GRSCR', 'GQECI', IERR)
               GO TO 9999
            END IF
            CALL GQLWK(GRTYP(GRCIDE),IERR,MPLBTE,MPMBTE,MTXBTE,MFABTE,
     :                 MPAI,MCOLI)
            IF (IERR.NE.0) THEN
               CALL GRQREP('GRSCR', 'GQLWK', IERR)
               GO TO 9999
            END IF
            IF (NDEF.LT.MCOLI) INDOK = .TRUE.
         END IF

         CALL GRTERM
         CALL GQWKDU(GRWKID(GRCIDE),IERR,IDEFMO,IREGMO,IEMPTY,NFRAME)
         IF (IERR.NE.0) THEN
            CALL GRQREP('GRSCR', 'GQWKDU', IERR)
            GO TO 9999
         END IF

         CALL GQDWKA(GRTYP(GRCIDE),IERR,IPLBUN,IPMBUN,ITXBUN,IFABUN,
     :               IPAREP,ICOLRP,IWKTR)
         IF (IERR.NE.0) THEN
            CALL GRQREP('GRSCR', 'GQDWKA', IERR)
            GO TO 9999
         END IF

*   Go ahead if either the colour index table is dynamic or the display
*   surface is empty so as to avoid causing the picture to be
*   regenerated.
         IF (INDOK.AND.(ICOLRP.EQ.GIMM.OR.IEMPTY.EQ.GEMPTY)) THEN

*      If the device is monochome then convert the colours to an
*      intensity according to the NTSC encoding system (see PGSCR).
            CALL GQCF(GRTYP(GRCIDE),IERR,NCOLI,ICOLA,NPCI)
            IF (IERR.NE.0) THEN
               CALL GRQREP('GRSCR', 'GQCF', IERR)
               GO TO 9999
            END IF
            IF (ICOLA.EQ.GMONOC) THEN
               RT = 0.30*R + 0.59*G + 0.11*B
               CALL GSCR(GRWKID(GRCIDE),IC,RT,RT,RT)
            ELSE
               CALL GSCR(GRWKID(GRCIDE),IC,R,G,B)
            END IF
         END IF
      ENDIF
 9999 CONTINUE
      END
