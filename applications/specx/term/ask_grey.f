*-----------------------------------------------------------------------

      SUBROUTINE ASK_GREY (AUTOGREY, GREYLIM)

*  Routine to query terminal for new limits for greyscale plots

      IMPLICIT  NONE

*     Formal parameters:

      LOGICAL  AUTOGREY
      REAL     GREYLIM(2)

*     Local variables:

      INTEGER  ISTAT

*  Ok, go...

*     Automatic setting of greyscales?

      CALL GEN_YESNO ('Set greyscales automatically?',
     &                 AUTOGREY, AUTOGREY, ISTAT)

*     If not, what are required limits?

      IF (.NOT.AUTOGREY) THEN
        CALL GEN_GETR4A ('Greyscale min and max?',
     &                    GREYLIM, 2, 'G10.3,1X,G10.3', GREYLIM, ISTAT)
      END IF

      RETURN
      END

*-----------------------------------------------------------------------
