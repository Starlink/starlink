*  History:
*     19 Nov 1993 (hme):
*        Replace STR$UPCASE with CHR_UCASE.
*     15 Jan 1994 (rp):
*        Replace CHR_UCASE with UUCASE
*-----------------------------------------------------------------------

      SUBROUTINE ASK_VEL (IFAIL)

      IMPLICIT  NONE

*     Formal parameters:

      INTEGER   IFAIL

*     Include files

      INCLUDE  'FLAGCOMM'

*     Local variables:

      INTEGER   ISTAT

*  Ok, go...

      IFAIL = 0

      CALL GEN_YESNO  ('Output in different vel frame?',
     &                  CHANGE_FRAME, CHANGE_FRAME, ISTAT)
      IF (.NOT. CHANGE_FRAME) RETURN

*     Determine velocity frame to be used.

      CALL GEN_GETSTR ('Velocity frame? '//
     &                 '(TELLuric, LSR, HELIocentric, GEOcentric)',
     &                 VEL_REF, 'A4', VEL_REF, ISTAT)

      CALL UUCASE  (VEL_REF)
      IF (VEL_REF(:1).EQ.'T') THEN
        VEL_REF = 'TELL'
        IREST   =  0
      ELSE IF (VEL_REF(:1).EQ.'L') THEN
        VEL_REF = 'LSR '
        IREST   =  1
      ELSE IF (VEL_REF(:1).EQ.'H') THEN
        VEL_REF = 'HELI'
        IREST   =  2
      ELSE IF (VEL_REF(:1).EQ.'G') THEN
        VEL_REF = 'GEO '
        IREST   =  3
      ELSE
        IFAIL = 21
        RETURN
      END IF

*     Determine velocity law being used.

      CALL GEN_GETSTR ('Velocity law definition? '//
     &                 '(OPTical, RADio, RELativistic)',
     &                 VEL_DEF, 'A3', VEL_DEF, ISTAT)

      CALL UUCASE    (VEL_DEF)
      IF (      VEL_DEF.NE.'OPT'
     &    .AND. VEL_DEF.NE.'RAD'
     &    .AND. VEL_DEF.NE.'REL') THEN
        IFAIL = 21
        RETURN
      END IF

*     ...and get velocity in new frame

      CALL GEN_GETR4  ('Velocity in new frame? (km/s)',
     &                  VELOUT, 'G10.3', VELOUT, ISTAT)

      RETURN
      END

*-----------------------------------------------------------------------
