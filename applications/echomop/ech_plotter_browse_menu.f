      SUBROUTINE ECH_PLOTTER_BROWSE_MENU( MENU )
*+
*-
      IMPLICIT NONE

      LOGICAL MENU
*.
      IF ( MENU ) THEN
      MENU = .FALSE.
      CALL ECH_REPORT( 0, ' ' )
      CALL ECH_REPORT( 0,
     :     ' Move image display cursor to region of interest then' )
      CALL ECH_REPORT( 0,
     :     ' press the key corresponding to one of the options.' )
      CALL ECH_REPORT( 0,
     :     ' Browse options:' )
      CALL ECH_REPORT( 0,
     :     '   S - Plot fitted sky near cursor.' )
      CALL ECH_REPORT( 0,
     :     '   O - Plot extracted object near cursor.' )
      CALL ECH_REPORT( 0,
     :     '   A - Plot extracted arc near cursor.' )
      CALL ECH_REPORT( 0,
     :     '   F - Plot flat field balance factors near cursor.' )
      CALL ECH_REPORT( 0,
     :     '   B - Plot fitted  blaze function near cursor.' )
      CALL ECH_REPORT( 0,
     :     '   N - Prompt for number of samples to plot.' )
      CALL ECH_REPORT( 0,
     :     '   L - Limit range in X and Y.' )
      CALL ECH_REPORT( 0,
     :     '   R - Set imaged range.' )
      CALL ECH_REPORT( 0,
     :     '   M - Display this menu.' )
      CALL ECH_REPORT( 0,
     :     '   Q - Quit back to main plot menu.' )

      ELSE
         CALL ECH_REPORT( 0,
     :        ' Options [ S O A F B N L R M Q ]' )
      END IF

      END
