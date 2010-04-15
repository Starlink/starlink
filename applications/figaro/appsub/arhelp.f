C+
      SUBROUTINE ARHELP
C
C     A R H E L P
C
C     Outputs help information for the line selection routine.
C
C     Routines used -
C
C     PGADVANCE        Clear the graphics screen.
C     GKD_WRITE_LINE   Write a dialogue line to the user.
C
C                                        KS / CIT  13th June 1984
C     Modified:
C
C     12th Sept 1985.  KS / AAO  Now uses PAR_WRUSER and PAR_RDUSER
C     11th Mar  1988.  KS / AAO  Now uses the GKD_ routines.
C     23rd Jul  1993.  HME / UoE, Starlink.  No longer wait at end of
C                      help, thus disuse PAR_Q*. Disuse GKD_CLEAR_ALPHA.
C+
      IMPLICIT NONE
C
C     Clear screen
C
      CALL PGADVANCE
C
C     Now output the help info
C
      CALL GKD_WRITE_LINE(
     :      'Lines are selected by moving the cursor near to')
      CALL GKD_WRITE_LINE('a line and hitting a key.')
      CALL GKD_WRITE_LINE(' ')
      CALL GKD_WRITE_LINE('A few keys have a special significance -')
      CALL GKD_WRITE_LINE(' ')
      CALL GKD_WRITE_LINE('B - Back to previous section')
      CALL GKD_WRITE_LINE('N - on to Next section')
      CALL GKD_WRITE_LINE('Q - Quit selection. ')
      CALL GKD_WRITE_LINE('R - Redraw current section')
      CALL GKD_WRITE_LINE('H - (or ?) output this Help information')
      CALL GKD_WRITE_LINE('S - change the current value of Sigma')
      CALL GKD_WRITE_LINE(
     :              'C - locate line center using Center-of-gravity')
      CALL GKD_WRITE_LINE(
     :              'L - set Length of displayed sections, in pixels')
      CALL GKD_WRITE_LINE('X - indicate line centers with X''s only')
      CALL GKD_WRITE_LINE('W - indicate lines with wavelengths')
      CALL GKD_WRITE_LINE('O - change order of fit')
      CALL GKD_WRITE_LINE(
     :              'E - Expand plot & indicate center with cursor')
      CALL GKD_WRITE_LINE(
     :              'D - Delete the selected line''s identification')
      CALL GKD_WRITE_LINE('M - Move to a specific x-value')
      CALL GKD_WRITE_LINE(' ')
      CALL GKD_WRITE_LINE('Any other key will cause you to be prompted')
      CALL GKD_WRITE_LINE('for the wavelength of the selected line.')
C
      CALL GKD_WRITE_LINE(' ')
C
      END
