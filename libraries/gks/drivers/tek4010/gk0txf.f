*-----------------------------------------------------------------------



      SUBROUTINE GK0TXF(IFID,RHT,RMAXWD,RBOT,RTOP,RWD)
* --------------------------------------------------------------
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     T4010, T4014 and emulators+  Supply Font Details
*
*  MAINTENANCE LOG
*  ---------------
*     Maintenance log is incorporated in main driver routine.
*
*  ARGUMENTS
*  ---------
*     INP IFID     Font identifier (ignored)
*     OUT RHT      Height from base to cap line
*     OUT RMAXWD   Width of widest character
*     OUT RBOT     Distance from base to bottom line
*     OUT RTOP     Distance from cap to top line
*     OUT RWD      Character widths array
*
      INTEGER IFID
      REAL RHT,RMAXWD,RBOT,RTOP,RWD(*)
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../../include/gkdt.par'
      INCLUDE '../../include/gks.par'
      INCLUDE '../../include/gkwca.cmn'
      INCLUDE '../../include/gkwkd.cmn'
*
*  LOCALS
*  ------
*     ICHSIZ Offset in KWKDAT for whether number of hardware character
*            sizes is 1 (GLOWER) or greater (GHIGHR)
*     ICHHT  Offset in QWKDAT for current hardware character height on
*            Tek 4014. Also set up by entry 'Set Text Attributes'.
*     BOTnnn Distance from centre of the base line to centre of the
*            bottom line for the workstation nnn.
*     BOT82X Distance from centre of the base line to centre of the
*            bottom line for the Standard/RAL mods Pericoms.
*     II     Loop variable
*
      INTEGER ICHSIZ
      PARAMETER (ICHSIZ=2)
      INTEGER ICHSZD
      PARAMETER (ICHSZD=5)
      INTEGER ICHHT,ICHWD
      PARAMETER (ICHHT=1, ICHWD=2)
      INTEGER II
      REAL BOT801(12),BOT82X(16)
      DATA BOT801/ 3.0, 3.0, 3.0, 3.0, 3.0, 3.0,
     :             6.0, 8.0,11.0,14.0,16.0,19.0/
      DATA BOT82X/ 3.0, 3.0, 5.0, 5.0, 6.0, 6.0, 9.0,10.0,
     :             9.0,10.0,12.0,12.0,15.0,15.0,20.0,20.0/
*
*  COMMENTS
*  --------
*       See GK0TXC for comments on text height and width
*
* --------------------------------------------------------------------

*  Obtain height and width. Tek 4010 and the like have only one font.
      IF( KWKDAT(ICHSIZ,KWKIX).EQ.GHIGHR ) THEN
          RHT=QWKDAT(ICHHT,KWKIX)
          RMAXWD=QWKDAT(ICHWD,KWKIX)
      ELSE
          RHT=14.0
          RMAXWD=14.0
      ENDIF

*   Where suitable, supply the value for RBOT, set to 0.0 otherwise.
      IF(KWKTYP.EQ.801)THEN
*   Cifer T5
*   As was the case for T5 heights, for each BOT801 item three off-screen
*   measurments were taken (test string being ouput from different Y
*   coordinates), then bigest value was adopted. This is due to the
*   unknown algorhitm for mapping T5 DC coordinates to screen pixels.
         RBOT=BOT801(KWKDAT(ICHSZD,KWKIX))
      ELSEIF(KWKTYP.EQ.820.OR.KWKTYP.EQ.821)THEN
*   Pericom Montereys (Standard/RAL mods) shared entry
*   Note that the values for BOT82X where obtained through off the
*   screen measurement (using cursor and zoom in the local mode) of
*   the base line centre to bottom line centre distances.
         RBOT=BOT82X(KWKDAT(ICHSZD,KWKIX))
      ELSE
         RBOT=0.0
      ENDIF

*  Now the centre of the cap line to the centre of the top line distance
      RTOP=0.0

*     widths for chars [32..126]
      DO 10 II=1,95
        RWD(II)=RMAXWD
 10   CONTINUE

      END
