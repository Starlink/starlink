      SUBROUTINE ECH_PLOTTER_MAIN_MENU( MENU, CURSOR, GRAPH, XLIMITS,
     :           YLIMITS, UWINDOW, XM, XH, YM, YH, STYLE )
*+
*-
      IMPLICIT NONE

      LOGICAL MENU
      LOGICAL CURSOR
      LOGICAL GRAPH
      LOGICAL XLIMITS
      LOGICAL YLIMITS
      LOGICAL UWINDOW

      REAL XM
      REAL XH
      REAL YM
      REAL YH

      CHARACTER*( * ) STYLE

      CHARACTER*80 STRING

      INTEGER CHR_LEN
*.

      IF ( MENU ) THEN
      MENU = .FALSE.
      CALL ECH_REPORT( 0, ' ' )
      CALL ECH_REPORT( 0, ' Options:' )
      CALL ECH_REPORT( 0,
     :     '   A - ASCII dump of last plot to a file.' )
      CALL ECH_REPORT( 0,
     :     '   D - Short directory of database (FD full directory).' )
      CALL ECH_REPORT( 0,
     :     '   E/Q - Exit/Quit.' )
      CALL ECH_REPORT( 0,
     :     '   H - Help. ' )
      IF ( CURSOR ) THEN
         STRING = '   I - Toggle cursor interactive mode' //
     :         ' (currently enabled).'
         CALL ECH_REPORT( 0, STRING )

      ELSE
         STRING = '   I - Toggle cursor interactive mode' //
     :         ' (currently disabled).'
         CALL ECH_REPORT( 0, STRING )
      END IF
      IF ( GRAPH ) THEN
         STRING = '   G - Toggle graphics/grayscale mode' //
     :         ' (currently graphics).'
         CALL ECH_REPORT( 0, STRING )
         CALL ECH_REPORT( 0,
     :     '   L - Limit range in X and Y, (and of data values).' )

      ELSE
         STRING = '   G - Toggle graphics/grayscale mode' //
     :         ' (currently grayscale).'
         CALL ECH_REPORT( 0, STRING )
         CALL ECH_REPORT( 0,
     :     '   L - Limit range in X, Y, and of data values.')
      END IF
      IF ( XLIMITS ) THEN
         WRITE ( STRING, 1001 ) INT( XM ), INT( XH )
         CALL ECH_REPORT( 0, STRING )
      END IF
      IF ( YLIMITS ) THEN
         WRITE ( STRING, 1002 ) INT( YM ), INT( YH )
         CALL ECH_REPORT( 0, STRING )
      END IF
      CALL ECH_REPORT( 0,
     :     '   B - Browse mode using imaging display.' )
      CALL ECH_REPORT( 0,
     :     '   N - Set number of samples to plot.' )
      CALL ECH_REPORT( 0,
     :     '   R - Set re-bin(+) or smooth(-) factor.' )
      STRING = '   S - Set plot style (currently ' //
     :      STYLE( :CHR_LEN( STYLE ) ) // ').'
      CALL ECH_REPORT( 0, STRING )
      CALL ECH_REPORT( 0,
     :     '   W - Set subwindowing in X and Y.')
      IF ( UWINDOW ) THEN
         CALL ECH_REPORT( 0,
     :     '   U - User dynamic windowing (Enabled).' )

      ELSE
         CALL ECH_REPORT( 0,
     :     '   U - User dynamic windowing (Disabled).' )
      END IF
      CALL ECH_REPORT( 0,
     :     '   ^ - Copy last plot to hardcopy device.' )
      CALL ECH_REPORT( 0,
     :     '   + - Overlay this graph on last one.' )
      CALL ECH_REPORT( 0,
     :     '   ! - Overlay this graph on image.' )
      CALL ECH_REPORT( 0,
     :     '   M - Display this menu.' )
      CALL ECH_REPORT( 0,
     :     '   @filename - Read plot commands from a file.' )
      CALL ECH_REPORT( 0,
     :     '   >device - Direct Plotting to named device.' )
      CALL ECH_REPORT( 0,
     :     '   <object-name> - Plot named object(s), e.g.:' )
      CALL ECH_REPORT( 0,
     :     '      ARC[1,3] - extracted arc, order 3.' )
      CALL ECH_REPORT( 0,
     :     '      OBJ,FWAV - extracted object, ' //
     :     'order 1 v fitted wavelengths.' )

      ELSE
         CALL ECH_REPORT( 0,
     :   ' Options [ A D E Q H I G L B N R S W U ^ + ! M @ > <Obj> ]' )
      END IF

 1001 FORMAT ( 7X, 'X-axis limited from ', I5, ' to ', I5, '.' )
 1002 FORMAT ( 7X, 'Y-axis limited from ', I5, ' to ', I5, '.' )

      END
