C-------------------------------------------------------------------
C
C                     FILHDI2.INC
C                     -----------

C   Include file defining the header of the SPECX data file

      INTEGER*2 NSCAN2X      ! Number of scans in the file
      INTEGER*2 NREC2X       ! Number of records used in file
      INTEGER*2 NSMAX2X      ! Maximum number of scans for this file
      CHARACTER CHDATA*52    ! File name and identification
      INTEGER*2 IREC12X      ! Sequence number of first scan in file

      COMMON /FILHDI2/ NSCAN2X, NREC2X, NSMAX2X, CHDATA, IREC12X
      SAVE   /FILHDI2/

C-------------------------------------------------------------------
