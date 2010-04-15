      SUBROUTINE GRLDEV
*+
*   - - - - - - - -
*     G R L D E V     (GKS emulation of GRPCKG)
*   - - - - - - - -
*
*   Writes a list of available workstations to the user
*
*   Constants from GKS_PAR
*      GGKCL   i    GKS closed
*
*   D.L.Terrett
*+
      IMPLICIT NONE

      INCLUDE 'GKS_PAR'

      INCLUDE 'SAE_PAR'


      INTEGER I, J, ISTATE, ISTAT, LD
      CHARACTER*72 LINE, DESCR
      CHARACTER*15 NAME
      LOGICAL GRFILT
      EXTERNAL GRFILT

*  Open GKS if necessary
      CALL GQOPS(ISTATE)
      IF (ISTATE.EQ.GGKCL) CALL GOPKS(6,-1)

      I = 0
      J = 0
      ISTAT = SAI__OK

*   Clear the output line
      LINE = ' '

  10  CONTINUE

*     Get the next type in the list
         CALL gns_GWNG(GRFILT,I,NAME,DESCR,LD,ISTAT)
         IF (ISTAT.NE.SAI__OK) THEN
            CALL ERR_REP('GRUNLD',
     :      'GRLDEV - Unable to list available graphics devices', ISTAT)
            GO TO 9999
         END IF
         IF (I.EQ.0) GO TO 20

*        Copy to the output line
         LINE(J*18+1:(J+1)*18) = NAME
         J = J + 1

*    Four workstations per line
         IF (J.EQ.4) THEN

*      Write the complete line
            CALL MSG_OUT('DEVICE_LIST', LINE, ISTAT)
            IF (ISTAT.NE.SAI__OK) THEN
               CALL ERR_REP('GRUNLD',
     :         'GRLDEV - Unable to list available graphics devices',
     :         ISTAT)
               GO TO 9999
            END IF
            J = 0
            LINE = ' '
         ENDIF

         GOTO 10

*  Output any remaining partially filled line
   20 CONTINUE
      IF (J.GT.0) THEN
         CALL MSG_OUT('DEVICE_LIST', LINE, ISTAT)
         IF (ISTAT.NE.SAI__OK)  CALL ERR_REP('GRUNLD',
     :      'GRLDEV - Unable to list available graphics devices', ISTAT)
      END IF

 9999 CONTINUE
      END
