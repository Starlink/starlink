C+
C                             E C H S E L E C T
C
C  Routine name:
C     ECHSELECT
C
C  Function:
C     Interactive object and sky selection from echellogram.
C
C  Description:
C     This application takes a corrected echellogram (one that has had
C     the orders straightened, probably by CDIST), and generates a
C     number of collapsed echellograms (images where each cross-section
C     is a separate echelle spectrum).  To determine which cross-sections
C     should be added to produce the sky and object spectra it gets the
C     user to indicate the relevant limits on a plot of the corrected
C     echelogram collapsed in the spectra direction.  For each order
C     the user can select a range of cross-sections to be used for the
C     object, and a number of ranges of cross-sections to be used for
C     the sky.  For each order, the object cross-sections are added
C     and the sky cross-sections are added and then scaled by the
C     factor (number of object cross-sections / number of sky cross
C     sections).  The object cross-sections are formed into one collapsed
C     echellogram and the sky cross-sections are formed into another
C     collapsed echellogram.  Optionally, a straightened arc can also
C     be processed using the same object and sky cross-section information,
C     in this case producing two collapsed arc echellograms, one for the
C     cross-sections designated as object, one for those designated as
C     sky, but in this case without any scaling being applied.
C
C  Command parameters:
C     IMAGE       (File) The name of the corrected echellogram.
C     XSTART      (Numeric) The first x-value to be used when collapsing
C                 the image along the wavelength direction.
C     XEND        (Numeric) The last x-value to be used when collapsing
C                 the image along the wavelength direction.
C     MSTART      (Integer) The order number for the first order.
C     MDELTA      (Integer) 1 if order numbers increase with cross-
C                 section, -1 if they decrease.
C     OBJOUT      (File) The name of the object collapsed echellogram.
C     SKYOUT      (File) The name of the sky collapsed echellogram.
C     DISNCHAN    (Integer) Length of displayed section.
C     MOVETOX     (Numeric) New plot centre x value.
C     ORDER       (Integer) Next echelle order to work on.
C     LOW         (Numeric) Minimum value to display.
C     HIGH        (Numeric) Maximum value to display.
C
C  Command keywords:
C     WHOLE       True if all of spectral range is to be used
C     PREVIOUS    True if previous selection is to be used as a starting
C                 point for the interactive selection.
C     ADD         Used to confirm that more than one cross section are
C                 to be used for an order's object.
C     CLEAR       Used to confirm that all settings for an order to be
C                 cleared.
C     QUITSEL     Used to confirm quitting work on an echelle order.
C
C  Data quality information:  Ignored.
C
C  Error data: Ignored.
C
C  Authors: Keith Shortridge, AAO
C           Horst Meyerdierks, UoE, Starlink
C           Clive Davenhall, UoE, Starlink
C
C  Version date: 13th Feb 1989
C
C  Files:
C     Selections made by ECHSELECT are written to ECHSELECT.LIS in the
C     default directory.  This begins with a number of comment lines -
C     all of which have an '*' in the first column, and is then followed
C     by a single line for each cross-section which has been selected,
C     giving cross-section number and order number, in format 2I10.
C     If the cross-section is part of the sky selected for that order,
C     the order number given is the negative of the actual order number.
C     (Note that this program cannot cope with zeroth order!).  This
C     file is also that read if PREVIOUS is specified.
C
C  History:
C     13th Feb 1989  KS / AAO.  Original version.
C     3rd  Sep 1992  HME / UoE, Starlink.  INCLUDE changed.
C                    Open output file without CARRIAGECONTROL.
C                    Read from or write to echselect.lis (lowercase).
C                    Open help file with lowercase name.
C                    PGASK is banned from ADAM, commented out.
C     25th Jan 1993  HME / UoE, Starlink.  Put PGASK back in.
C     20th Jul 1993  HME / UoE, Starlink.  Before reading a previous
C                    selection, the array was not initialised properly.
C                    Fixed.  Also, if opening the output echselect.lis
C                    fails, try to delete an existing file of that name
C                    and then try again to open a new one.
C     27th Jul 1993  HME / UoE, Starlink.  Disuse GKD_* except
C                    GKD_WRITE_LINE. Disuse PAR_Q*, use PAR_ABORT.
C                    Added parameters DISNCHAN, MOVETOX, ORDER, LOW,
C                    HIGH, ADD, CLEAR, QUITSEL.
C     26th Sep 2001  ACD / UoE, Starlink.  Removed unused variable
C                    CARRYON.
C     2005 June 14   MJC / Starlink  Use CNF_PVAL for pointers to
C                    mapped data.
C+
      SUBROUTINE ECHSELECT
C
      IMPLICIT NONE

      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
CC
C     Functions used
C
      LOGICAL   PAR_ABORT
      INTEGER   DSA_TYPESIZE, PGBEGIN
C
C     Local variables
C
      CHARACTER CHARS(2)*16      ! Units and label for axis
      INTEGER   CODE             ! Status returned by file I/O routines
      INTEGER   CPTR             ! Dynamic-memory pointer for collapsed
                                 ! data
      CHARACTER DEVICE*32        ! Plot device specification
      INTEGER   DIMS(2)          ! Image, then collapsed echellogram
                                 ! dimensions
      DOUBLE    PRECISION DUMMY  ! Numeric array for DSA-SET_AXIS_INFO
      LOGICAL   FAULT            ! Flags a non-DSA system fault
      INTEGER   IGNORE           ! Used for don't care status returns
      INTEGER   IPTR             ! Dynamic-memory pointer for image data
      INTEGER   IXEN             ! Last cross-section used in collapse
      INTEGER   IXST             ! First cross-section used in collapse
      INTEGER   MAXORD           ! Highest order number selected
      INTEGER   MINORD           ! Lowest order number selected
      INTEGER   NDIM             ! Number of image dimensions
      INTEGER   NELM             ! Number of image elements - ignored
      INTEGER   NORD             ! Number of orders selected
      INTEGER   NX               ! First dimension of image
      INTEGER   NY               ! Second dimension of image
      INTEGER   MDELTA           ! Value of MDELTA parameter
      INTEGER   MSTART           ! Value of MSTART parameter
      INTEGER   OPTR             ! Dynamic-memory pointer for order
                                 ! number array
      LOGICAL   PGOPEN           ! Indicates PGPLOT was opened
      INTEGER   PG_STATUS        ! Status from PGBEGIN call
      LOGICAL   PREV             ! Value of PREVIOUS keyword
      LOGICAL   SKY              ! Indicates SKY orders were selected
      INTEGER   SLOT             ! Used for all DSA system slots -
                                 ! ignored
      INTEGER   SPTR             ! Dynamic-memory pointer for spectrum
                                 ! data
      INTEGER   STATUS           ! Inherited status for DSA routines
      CHARACTER STRUCTURE*128    ! Full name of image data structure
      REAL      VALUE            ! Real value obtained from parameter
                                 ! system
      INTEGER   VAR_STATUS       ! Status for VAR routines
      LOGICAL   WHOLE            ! Value of WHOLE keyword
      INTEGER   WPTR             ! Dynamic-memory pointer for orders
                                 ! array
      REAL      XEND             ! Last axis value used in collapse
      INTEGER   XPTR             ! Dynamic-memory pointer for image
                                 ! Y-axis data
      REAL      XSTART           ! First axis value used in collapse
      INTEGER   YPTR             ! Dynamic-memory pointer for output
                                 ! Y-axis data
C
C     Flag values for DSA_OUTPUT
C
      INTEGER NEW_FILE, NO_DATA
      PARAMETER (NEW_FILE = 1, NO_DATA = 1)
C
C     Initialise variables and DSA system.
C
      FAULT = .FALSE.
      PGOPEN = .FALSE.
      STATUS = 0
      CALL DSA_OPEN (STATUS)
C
C     Check for a SOFT device specification.
C
      CALL VAR_GETCHR('SOFT',0,0,DEVICE,VAR_STATUS)
      IF (VAR_STATUS.NE.0) THEN
         CALL PAR_WRUSER('No graphics device specified',IGNORE)
         CALL PAR_WRUSER(
     :         'Use "SOFT" command eg "SOFT ARGS1" to rectify.',IGNORE)
         FAULT = .TRUE.
         GO TO 500
      END IF
C
C     Open image and get dimensions
C
      CALL DSA_INPUT ('IMAGE','IMAGE',STATUS)
      CALL DSA_DATA_SIZE ('IMAGE',2,NDIM,DIMS,NELM,STATUS)
      IF (STATUS.NE.0) GO TO 500
      NX = DIMS(1)
      NY = DIMS(2)
C
C     Get workspace for the orders array (pointed to by WPTR)
C
      CALL DSA_GET_WORK_ARRAY (NY,'INT',WPTR,SLOT,STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Do we want to use data from a previous run?
C
      CALL PAR_RDKEY('PREVIOUS',.FALSE.,PREV)
      IF (PAR_ABORT()) GO TO 500
      IF (PREV) THEN
         CALL FIG_ECS_READ ('echselect.lis',NY,%VAL(CNF_PVAL(WPTR)),
     :                      CODE)
         IF (CODE.GT.2) PREV=.FALSE.
      END IF
C
C     Initialise the graphics device
C
      PG_STATUS = PGBEGIN(0,DEVICE,1,1)
      IF (PG_STATUS.NE.1) THEN
         CALL PAR_WRUSER('Unable to open graphics device',IGNORE)
         FAULT = .TRUE.
         GO TO 500
      END IF
      PGOPEN = .TRUE.
C
C     Map the data array, get workspace for the collapsed data,
C     and collapse it along the spectral direction, scaling it by
C     the number of cross-sections involved.
C
      CALL DSA_MAP_DATA ('IMAGE','READ','FLOAT',IPTR,SLOT,STATUS)
      CALL DSA_GET_WORK_ARRAY (NY,'FLOAT',SPTR,SLOT,STATUS)
      CALL PAR_RDKEY ('WHOLE',.FALSE.,WHOLE)
      CALL DSA_AXIS_RANGE ('IMAGE',1,' ',WHOLE,XSTART,XEND,IXST,
     :                     IXEN,STATUS)
      IF (STATUS.NE.0) GO TO 500
      CALL FIG_YTRACT (%VAL(CNF_PVAL(IPTR)),NX,NY,IXST,IXEN,
     :                 %VAL(CNF_PVAL(SPTR)))
      CALL GEN_MULCAF (%VAL(CNF_PVAL(SPTR)),NY,1./(IXEN-IXST+1),
     :                 %VAL(CNF_PVAL(SPTR)))
C
C     Get an axis array to display the spectrum against
C
      CALL DSA_MAP_AXIS_DATA ('IMAGE',2,'READ','FLOAT',XPTR,
     :                        SLOT,STATUS)

C     Get the parameters for the orders
C
      CALL PAR_RDVAL ('MSTART',0.0,1000.0,0.0,'Order number',VALUE)
      MSTART = VALUE
      CALL PAR_RDVAL ('MDELTA',-1.0,1.0,-1.0,' ',VALUE)
      MDELTA = VALUE
      IF (PAR_ABORT()) GO TO 500     ! User requested abort
C
C     FIG_ESDISP runs an interactive loop that continues until the user
C     indicates that he is happy with the selection.  Note that WPTR
C     points to the array called ORDERS in all the subroutines.
C
      CALL FIG_ESDISP (%VAL(CNF_PVAL(SPTR)),%VAL(CNF_PVAL(XPTR)),NY,
     :                 PREV,MSTART,MDELTA,MINORD,MAXORD,SKY,
     :                 %VAL(CNF_PVAL(WPTR)))
      IF (PAR_ABORT()) GO TO 500     ! User requested abort
C
C     Quit now if no selections were made
C
      IF (MINORD.EQ.0) GO TO 500
C
C     Write out the selections to a new version of the file
C
      CALL DSA_GET_ACTUAL_NAME ('IMAGE',STRUCTURE,STATUS)
      CALL FIG_ECS_WRITE ('echselect.lis',STRUCTURE,
     :                    %VAL(CNF_PVAL(WPTR)),NY,CODE)
C
C     Once the selection has been made, the output structures can
C     be created.
C
      NORD=MAXORD-MINORD+1
      CALL DSA_OUTPUT ('OBJOUT','OBJOUT','IMAGE',NO_DATA,NEW_FILE,
     :                 STATUS)
      DIMS(1) = NX
      DIMS(2) = NORD
      CALL DSA_RESHAPE_DATA ('OBJOUT','IMAGE',2,DIMS,STATUS)
      CALL DSA_RESHAPE_AXIS ('OBJOUT',1,'IMAGE',1,1,NX,STATUS)
C
C     Get workspace for the orders array and fill it, then create
C     the orders axis structure for the output.
C
      CALL DSA_GET_WORK_ARRAY (NORD,'FLOAT',OPTR,SLOT,STATUS)
      IF (STATUS.NE.0) GO TO 500
      CALL FIG_FILL_ORDS (MINORD,NORD,MDELTA,%VAL(CNF_PVAL(OPTR)))
      CALL DSA_MAP_AXIS_DATA ('OBJOUT',2,'WRITE','FLOAT',YPTR,SLOT,
     :                        STATUS)
      IF (STATUS.NE.0) GO TO 500
      CALL GEN_MOVE (NORD*DSA_TYPESIZE('FLOAT',STATUS),
     :               %VAL(CNF_PVAL(OPTR)),%VAL(CNF_PVAL(YPTR)))
      CHARS(1) = ' '
      CHARS(2) = 'Order number'
      CALL DSA_SET_AXIS_INFO ('OBJOUT',2,2,CHARS,0,DUMMY,STATUS)
C
C     Now create the collapsed object echellogram in the output
C     structure data array
C
      CALL DSA_MAP_DATA ('OBJOUT','WRITE','FLOAT',CPTR,SLOT,STATUS)
      IF (STATUS.NE.0) GO TO 500
      CALL FIG_ECS_OBJ (%VAL(CNF_PVAL(IPTR)),NX,NY,MINORD,NORD,MDELTA,
     :                  .TRUE.,%VAL(CNF_PVAL(WPTR)),
     :                   %VAL(CNF_PVAL(CPTR)))
C
C     If any sky cross-sections were selected, repeat for the sky
C     collapsed echellogram.  Note that some of the work has already
C     been done - the order numbers are already in the workspace at
C     element OPTR, and CHARS is already set up.
C
      IF (SKY) THEN
         CALL DSA_OUTPUT ('SKYOUT','SKYOUT','IMAGE',NO_DATA,NEW_FILE,
     :                    STATUS)
         CALL DSA_RESHAPE_DATA ('SKYOUT','IMAGE',2,DIMS,STATUS)
         CALL DSA_RESHAPE_AXIS ('SKYOUT',1,'IMAGE',1,1,NX,STATUS)
         CALL DSA_MAP_AXIS_DATA ('SKYOUT',2,'WRITE','FLOAT',
     :                           YPTR,SLOT,STATUS)
         IF (STATUS.NE.0) GO TO 500
         CALL GEN_MOVE (NORD*DSA_TYPESIZE('FLOAT',STATUS),
     :                  %VAL(CNF_PVAL(OPTR)),%VAL(CNF_PVAL(YPTR)))
         CALL DSA_SET_AXIS_INFO ('SKYOUT',2,2,CHARS,0,DUMMY,STATUS)
         CALL DSA_MAP_DATA ('SKYOUT','WRITE','FLOAT',CPTR,SLOT,STATUS)
         IF (STATUS.NE.0) GO TO 500
         CALL FIG_ECS_OBJ (%VAL(CNF_PVAL(IPTR)),NX,NY,MINORD,NORD,
     :                     MDELTA,.FALSE.,%VAL(CNF_PVAL(WPTR)),
     :                     %VAL(CNF_PVAL(CPTR)))
      END IF
C
C     Exit
C
  500 CONTINUE
      CALL DSA_CLOSE (STATUS)
      IF (PGOPEN) CALL PGEND
      IF (FAULT) CALL FIG_SETERR
C
      END
