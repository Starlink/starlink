C+
      SUBROUTINE ARC_ARHELP
C
C     A R H E L P
C
C     Outputs help information for the line selection routine.
C
C     Routines used -
C
C     PGADVANCE    (PGPLOT) Clears screen
C     PAR_RDUSER   (PAR_ package) Read line from user.
C     PAR_WRUSER   ( "     "    ) Send message to user.
C
C                                        KS / CIT  13th June 1984
C     Modified:
C
C     12th Sep 1985.  KS / AAO.  Now uses PAR_WRUSER and PAR_RDUSER.
C     23rd Jul 1993.  HME / UoE, Starlink.  No longer wait at end of
C                     help, thus disuse PAR_RDUSER.
C+
      IMPLICIT NONE
C
C     Local variables
C
      INTEGER STATUS
C
C     Clear screen.
C
      CALL PGADVANCE
C
C     Now output the help info
C
      CALL PAR_WRUSER('Lines are selected by moving the cursor near to',
     :                                                   STATUS)
      CALL PAR_WRUSER('a line and hitting a key.',STATUS)
      CALL PAR_WRUSER(' ',STATUS)
      CALL PAR_WRUSER('A few keys have a special significance -',STATUS)
      CALL PAR_WRUSER(' ',STATUS)
      CALL PAR_WRUSER('B - Back to previous section',STATUS)
      CALL PAR_WRUSER('N - on to Next section',STATUS)
      CALL PAR_WRUSER('Q - Quit selection. ',STATUS)
      CALL PAR_WRUSER('R - Redraw current section',STATUS)
      CALL PAR_WRUSER('H - (or ?) output this Help information',STATUS)
      CALL PAR_WRUSER('S - change the current value of Sigma',STATUS)
      CALL PAR_WRUSER('C - locate line center using Center-of-gravity',
     :                                                    STATUS)
      CALL PAR_WRUSER('L - set Length of displayed sections, in pixels',
     :                                                    STATUS)
      CALL PAR_WRUSER('X - indicate line centers with X''s only',STATUS)
      CALL PAR_WRUSER('W - indicate lines with wavelengths',STATUS)
      CALL PAR_WRUSER('O - change order of fit',STATUS)
      CALL PAR_WRUSER('E - Expand plot & indicate center with cursor',
     :                                                    STATUS)
      CALL PAR_WRUSER('D - Delete the selected line''s identification',
     :                                                    STATUS)
      CALL PAR_WRUSER('M - Move to a specific x-value',STATUS)
      CALL PAR_WRUSER(' ',STATUS)
      CALL PAR_WRUSER('Any other key will cause you to be prompted',
     :                                                    STATUS)
      CALL PAR_WRUSER('for the wavelength of the selected line.',STATUS)
C
      CALL PAR_WRUSER(' ',STATUS)
C
      RETURN
      END
