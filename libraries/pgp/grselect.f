      SUBROUTINE GRSLCT(ID)
*+
*     - - - - - - - -
*       G R S L C T    (GKS emulation of GRPCKG)
*     - - - - - - - -
*
*   Select the plotting device:
*      Deactivate current device
*      Save transformations for current device
*      Validate the plot identifier
*      select new transformation and colour indices
*      Activate new device
*
*   Given
*      ID       i     Device identifier
*
*   Read from COMMON
*      GRDVOP   i()   Device open flag
*      GRVIEW   r()   Viewport
*      GRWIND   r()   Window
*      GRVIE2   r()   Full viewport
*      GRWIN2   r()   Full window
*      GRWKID   i()   Workstation id
*   Written to COMMON
*      GRCIDE   i     Current GRPCKG device
*      GRVIEW   r()   Viewport
*      GRWIND   r()   Window
*      GRVIE2   r()   Full viewport
*      GRWIN2   r()   Full window
*
*   Constants from GRECOM
*      MAXDEV   i     Maximum number of open GRPCKG devices
*      TRN      i     Transformation number
*      TRN2     i     Transformation number
*
*   D.L.Terrett  Starlink  Aug 1987
*+
      IMPLICIT NONE

      INCLUDE 'grecom.inc'

      INCLUDE 'PGP_ERR'


      INTEGER ID, IERR

      IF (ID.NE.GRCIDE) THEN

*   Check that ID is valid
         IF (ID.LE.0 .OR. ID.GT.MAXDEV) THEN
            CALL ERR_REP('GRIPLI',
     :      'GRSLCT - Invalid PGPLOT plot identifier', GRIPLI)
         ELSE
            IF (.NOT.GRDVOP(ID)) THEN
               CALL ERR_REP('GRIPLI',
     :         'GRSLCT - Invalid PGPLOT plot identifier', GRIPLI)
            ELSE

*       If there is a workstation active
               IF (GRCIDE.GT.0) THEN

*        Update old workstation and deactivate it
                   CALL GRTERM
                   CALL GDAWK(GRWKID(GRCIDE))

*        Save current transformation
                   CALL GQNT(TRN,IERR,GRWIND(1,GRCIDE),GRVIEW(1,GRCIDE))
                   IF (IERR.NE.0) THEN
                       CALL GRQREP('GRSLCT', 'GQNT', IERR)
                       GO TO 9999
                   END IF
               END IF

*        Select transformations for new device
               CALL GSWN(TRN,
     :              GRWIND(1,ID),GRWIND(2,ID),GRWIND(3,ID),GRWIND(4,ID))
               CALL GSVP(TRN,
     :              GRVIEW(1,ID),GRVIEW(2,ID),GRVIEW(3,ID),GRVIEW(4,ID))
               CALL GSWN(TRN2,
     :              GRWIN2(1,ID),GRWIN2(2,ID),GRWIN2(3,ID),GRWIN2(4,ID))
               CALL GSVP(TRN2,
     :              GRVIE2(1,ID),GRVIE2(2,ID),GRVIE2(3,ID),GRVIE2(4,ID))


*         Set colours
               CALL GSPLCI(GRCCOL(ID))
               CALL GSPMCI(GRCCOL(ID))
               CALL GSFACI(GRCCOL(ID))

*         Activate new workstation
               CALL GACWK(GRWKID(ID))

*         New current device
               GRCIDE = ID

            END IF
         END IF
      END IF
 9999 CONTINUE
      END
