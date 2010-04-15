C+
C                       D S A _ A X I S _ R A N G E
C
C  Routine name:
C     DSA_AXIS_RANGE
C
C  Function:
C     Gets the parameters that control a range based on axis values.
C
C  Description:
C     Figaro has a large number of applications that operate on a
C     sub-range of the data in a structure.  Typically, an application
C     that operates on a limited range of the X-axis will have
C     parameters XSTART and XEND.  These have to be compared with the
C     data in the appropriate axis in order to determine the pixel
C     range that they represent.  This routine interacts with the
C     parameter system and  the data for a specified axis in order to
C     return such limits for a specified axis.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA_AXIS_RANGE (REF_NAME,AXIS,CONTROL,WHOLE,START,END,
C                                               ISTART,IEND,STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) REF_NAME      (Fixed string,descr) The reference name used for
C                       the data structure in question.
C     (>) AXIS          (Integer,ref) The number of the axis in question.
C     (>) CONTROL       (Fixed string,descr)  A string controlling
C                       some of the routines options.  Lower case
C                       characters are ignored.  If CONTROL contains a
C                       'U' (for 'Unconstrained'), this indicates that
C                       the  range need not be constrained to lie within
C                       the extrema of the axis data.  If it contains a
C                       'C' (for 'Complex') this indicates that
C                       multi-dimensional axis data is acceptable to the
C                       program (which will have to use DSA_AXIS_BOUNDS
C                       to obtain sensible ISTART,IEND values).
C     (>) WHOLE         (Logical,ref) If true, the parameters will not
C                       be prompted for.  Instead, the extreme axis
C                       values will be taken.
C     (<) START         (Real,ref) The data value of the start of the
C                       range selected.
C     (<) END           (Real,ref) The data value of the end of the
C                       range selected.
C     (<) ISTART        (Integer,ref) The lower of the two pixel numbers
C                       corresponding to START and END.
C     (<) IEND          (Integer,ref) The higher of the two pixel numbers
C                       corresponding to START and END. If CONTROL indicates
C                       'unconstrained', then START and/or END may be outside
C                       the range of axis values.  In this case ISTART/IEND
C                       will be returned as 1 or N (= number of elements in
C                       data).  That is, ISTART and IEND always refer to
C                       valid elements of the data.
C     (!) STATUS        (Integer,ref) Status code.  If bad status is passed,
C                       this routine will return immediately.
C
C  External subroutines / functions used:
C     DSA_GET_AXIS_INFO, DSA_SEEK_AXIS, DSA_AXIS_SIZE, DSA_MAP_AXIS_DATA
C     DSA_UNMAP, PAR_RDVAL, PAR_SDVAL, GEN_BSEARCH, GEN_RANGEF, GEN_ELEMF
C     ICH_LEN, DSA_GET_ACTUAL_NAME, DSA_WRUSER, DYN_ELEMENT, PAR_ABORT
C
C  Prior requirements:
C     DSA_OPEN must have been called to initialise the system, and
C     the data structure must have been opened, eg by DSA_INPUT.
C
C  Support: Keith Shortridge, AAO
C
C  Version date: 29th August 1992
C-
C  Subroutine / function details:
C     ICH_LEN            Position of last non-blank char in string
C     DSA_GET_AXIS_INFO  Get units etc from axis structure
C     DSA_SEEK_AXIS      See if axis data array exists
C     DSA_AXIS_SIZE      Get axis array dimensions
C     DSA_MAP_AXIS_DATA  Map axis data array
C     DSA_UNMAP          Unmap data array mapped by DSA_ routine
C     PAR_ABORT          Test parameter system abort flag
C     PAR_RDVAL          Get numberic value from parameter system
C     PAR_SDVAL          Set default for numeric parameter
C     CNF_PVAL           Full pointer to dynamically allocated memory
C     GEN_ELEMF          Get element of a real array
C     GEN_BSEARCH        Find array element nearest to a specified value
C     GEN_RANGEF         Get maximum and minimum values in an array
C     DSA_WRUSER         Output message to user
C     DSA_GET_ACTUAL_NAME  Get full name of structure
C
C  History:
C     8th July 1987   Original version.  KS / AAO.
C     5th Sept 1988   PAR_ABORT calls added to support user requested
C                     aborts.  KS/AAO.
C     2nd Jan  1990   Revised to improve operation when axis values decrease.
C                     KS/AAO.
C     18th Dec 1990   Added a check to make sure ISTART, IEND are within
C                     allowed limits when data array doesn't exist. JMS/AAO.
C     21st Aug 1992   Automatic portability modifications
C                     ("INCLUDE" syntax etc) made. KS/AAO
C     29th Aug 1992   "INCLUDE" filenames now upper case. KS/AAO
C     5th  Oct 1992   TABs removed. HME / UoE, Starlink.
C     2005 June 3     Replace DYNAMIC_MEMORY with
C                     %VAL(CNF_PVAL(ADDRESS)) contruct for 64-bit
C                     addressing.  MJC / Starlink
C+
      SUBROUTINE DSA_AXIS_RANGE (REF_NAME,AXIS,CONTROL,WHOLE,START,
     :                                        END,ISTART,IEND,STATUS)
C
      IMPLICIT NONE
C
C     Parameters
C
      LOGICAL WHOLE
      INTEGER AXIS, ISTART, IEND, STATUS
      REAL START, END
      CHARACTER*(*) REF_NAME, CONTROL
C
C     Functions used
C
      LOGICAL PAR_ABORT
      INTEGER GEN_BSEARCH, ICH_LEN
      REAL GEN_ELEMF
C
C     DSA_ common definition - used to supply value of MAX_AXES
C
      INCLUDE 'DSA_COMMON'
C
C     DSA_ error codes
C
      INCLUDE 'DSA_ERRORS'

      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
CC
C     Local variables
C
      INTEGER   ADDRESS          ! Address of the mapped array
      CHARACTER AXIS_CH*1        ! First letter of axis name
      INTEGER   DIMS(MAX_AXES)   ! Dimensions of axis data
      INTEGER   DUMMY            ! Dummy argument
      INTEGER   ELEMENTS         ! Number of data array elements
      LOGICAL   EXIST            ! Indicates axis data exists
      REAL      FIRST            ! First value in data array
      INTEGER   IGNORE           ! Dummy status return from PAR_
                                 ! routines
      INTEGER   ITEMP            ! Used to swop ISTART,IEND
      REAL      LAST             ! Last value in data array
      LOGICAL   LIMITED          ! Indicates values constrained to axis
                                 ! values
      INTEGER   NDIM             ! Number of data array dimensions
      INTEGER   NELM             ! Elements in 1st 1D array of axis
                                 ! data
      LOGICAL   REVERSE          ! True if axis values are in
                                 ! descending order
      LOGICAL   SIMPLE           ! Indicates data has to be 1D
                                 ! (not Complex)
      INTEGER   SLOT             ! Map slot handle
      CHARACTER STRUCTURE*128    ! Full name of data structure
      REAL      PMAX             ! Maximum acceptable parameter value
      REAL      PMIN             ! Minimum axcceptable parameter value
      CHARACTER UNITS*32         ! Units for data
      REAL      VMAX             ! Maximum value in axis data array
      REAL      VMIN             ! Minimum value in axis data array
C
C     Absolute limits for the values - these are chosen, a little
C     arbitrarily, to be just inside the machines floating point range.
C
      REAL FMAX, FMIN
      PARAMETER (FMAX=1.0E38, FMIN=-1.0E38)
C
C     Names of axis data structures - note that this routine assumes that
C     they are all single character names, and can only support up to AXIS=6.
C
      CHARACTER AXIS_NAMES*6
      DATA AXIS_NAMES/'XYTUVW'/
C
C     Return immediately on bad status
C
      IF (STATUS.NE.0) RETURN
C
C     Look for 'Unconstrained', 'Complex' in CONTROL
C
      LIMITED=INDEX(CONTROL,'U').EQ.0
      SIMPLE=INDEX(CONTROL,'C').EQ.0
C
C     Get the units for the axis.
C
      CALL DSA_GET_AXIS_INFO (REF_NAME,AXIS,1,UNITS,0,DUMMY,STATUS)
C
C     See if the axis data exists, and get its dimensions - note that
C     DSA_AXIS_SIZE will return the appropriate axis size of the
C     main data array, if the axis data does not exist.
C
      SLOT=0
      CALL DSA_SEEK_AXIS (REF_NAME,AXIS,EXIST,STATUS)
      CALL DSA_AXIS_SIZE(REF_NAME,AXIS,MAX_AXES,NDIM,DIMS,ELEMENTS,
     :                                                         STATUS)
      IF (STATUS.NE.0) GO TO 500                       ! Error exit
C
      IF (EXIST) THEN
C
C        If it exists, see if it is multi-dimensional or simple 1D,
C        and if that is OK by the caller.
C
         IF ((NDIM.GT.1).AND.SIMPLE) THEN
            CALL DSA_WRUSER(
     :              'Warning: The '//AXIS_NAMES(AXIS:AXIS)//'-axis in ')
            CALL DSA_GET_ACTUAL_NAME (REF_NAME,STRUCTURE,STATUS)
            CALL DSA_WRUSER(STRUCTURE(:ICH_LEN(STRUCTURE)))
            CALL DSA_WRUSER(' is multi-dimensional, which this ')
            CALL DSA_WRUSER('application cannot handle correctly. ')
            CALL DSA_WRUSER('Limits will be based on the first ')
            CALL DSA_WRUSER('cross-section of the axis data.')
            CALL DSA_WRFLUSH
         END IF
C
C        Map it.
C
         CALL DSA_MAP_AXIS_DATA(REF_NAME,AXIS,'READ','FLOAT',
     :                                            ADDRESS,SLOT,STATUS)
         IF (STATUS.NE.0) GO TO 500                       ! Error exit
C
C        Having mapped it, get the maximum and minimum values in it.
C        Rather than look at each element, we assume that the data
C        extreme values will be at the ends of the data array.  If
C        the array is multi-dimensional, and the application will
C        accept that, we take the maximum and minimum over the
C        whole array.
C
         NELM=DIMS(1)
         FIRST=GEN_ELEMF(%VAL(CNF_PVAL(ADDRESS)),1)
         LAST=GEN_ELEMF(%VAL(CNF_PVAL(ADDRESS)),NELM)
         REVERSE=FIRST.GT.LAST
         IF ((NDIM.GT.1).AND.(.NOT.SIMPLE)) THEN
            CALL GEN_RANGEF (%VAL(CNF_PVAL(ADDRESS)),1,ELEMENTS,VMAX,
     :                       VMIN)
         ELSE
            VMIN=MIN(FIRST,LAST)
            VMAX=MAX(FIRST,LAST)
         END IF
      ELSE
C
C        If the data doesn't exist, then treat it as the numbers 1..N
C        (DSA_MAP_AXIS would do that for us, but we don't want to use
C        that feature if we can avoid its overhead.)
C
         VMIN=1.0
         VMAX=ELEMENTS
         REVERSE=.FALSE.
      END IF
C
C     Now see if we are restricted to the axis data values.
C
      IF (LIMITED) THEN
         PMIN=VMIN
         PMAX=VMAX
      ELSE
         PMIN=FMIN
         PMAX=FMAX
      END IF
C
C     See if WHOLE was specified.
C
      AXIS_CH=AXIS_NAMES(AXIS:AXIS)
      IF (WHOLE) THEN
         IF (.NOT.REVERSE) THEN
            START=VMIN
            END=VMAX
         ELSE
            START=VMAX
            END=VMIN
         END IF
         CALL PAR_SDVAL(AXIS_CH//'START',START,IGNORE)
         CALL PAR_SDVAL(AXIS_CH//'END',END,IGNORE)
      ELSE
C
C        If not,  get the parameter values
C
         IF (.NOT.REVERSE) THEN
            CALL PAR_RDVAL (AXIS_CH//'START',PMIN,PMAX,VMIN,UNITS,START)
            CALL PAR_RDVAL (AXIS_CH//'END',START,PMAX,VMAX,UNITS,END)
         ELSE
            CALL PAR_RDVAL (AXIS_CH//'START',PMIN,PMAX,VMAX,UNITS,START)
            CALL PAR_RDVAL (AXIS_CH//'END',PMIN,START,VMIN,UNITS,END)
         END IF
      END IF
C
C     Test the parameter system abort flag
C
      IF (PAR_ABORT()) THEN
         STATUS=DSA__ABORT
         GO TO 500
      END IF
C
C     Get the values in terms of element numbers.  Note that this is
C     done only in terms of the first 1d array of the axis data.
C
      IF (EXIST) THEN
         ISTART=GEN_BSEARCH(%VAL(CNF_PVAL(ADDRESS)),NELM,START)
         IEND=GEN_BSEARCH(%VAL(CNF_PVAL(ADDRESS)),NELM,END)
         IF (ISTART.LT.1) ISTART=1
         IF (IEND.LT.1) IEND=NELM
         IF (ISTART.GT.IEND) THEN
            ITEMP=ISTART
            ISTART=IEND
            IEND=ITEMP
         END IF
      ELSE
C
C       Since we know the array does not exist, the data values are just
C       the numbers 1..N, so the limits in pixel numbers are the same as the
C       data values (ie value 1.0 is in pixel 1).  We make sure the pixel
C       numbers returned are within the actual array range.  VMIN and VMAX
C       will be 1 and N respectively.
C
         IF (START.LT.VMIN) THEN
           ISTART=1
         ELSE IF (START.GT.VMAX) THEN
           ISTART=ELEMENTS
         ELSE
           ISTART=START
         END IF
         IF (END.GT.VMAX) THEN
           IEND=ELEMENTS
         ELSE IF (END.LT.VMIN) THEN
           IEND=1
         ELSE
           IEND=END
         END IF
      END IF
C
C     Release the axis data if we mapped it.
C
      IF (SLOT.NE.0) CALL DSA_UNMAP (SLOT,STATUS)
C
C     Exit
C
  500 CONTINUE
C
      END
