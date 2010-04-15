      SUBROUTINE DSA_AXIS_RANGE( REF_NAME, AXIS, CONTROL, WHOLE,
     :   START, END, ISTART, IEND, STATUS )
*+
*  Name:
*     DSA_AXIS_RANGE

*  Purpose:
*     Get the parameters that control a range based on axis values.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL DSA_AXIS_RANGE( REF_NAME, AXIS, CONTROL, WHOLE,
*        START, END, ISTART, IEND, STATUS )

*  Description:
*     Figaro has a large number of applications that operate on a
*     sub-range of the data in a structure. Typically, an application
*     that operates on a limited range of the X-axis will have
*     parameters XSTART and XEND. These have to be compared with the
*     data in the appropriate axis in order to determine the pixel range
*     that they represent. This routine interacts with the parameter
*     system and the data for a specified axis in order to return such
*     limits for a specified axis.
*
*     Contrary to earlier implementations the axis centre array must not
*     have been accessed when the call to this routine is made.

*  Arguments:
*     DSAREF = CHARACTER * ( * ) (Given)
*        The reference name associated with the NDF.
*     AXIS = INTEGER (Given)
*        The number of the axis in question.
*     CONTROL = CHARACTER * ( * ) (Given)
*        A string controlling some of the routines options.  Lower case
*        characters are ignored.  If CONTROL contains a 'U' (for
*        'Unconstrained'), this indicates that the range need not be
*        constrained to lie within the extrema of the axis data.  If it
*        contains a 'C' (for 'Complex') this indicates that
*        multi-dimensional axis data is acceptable to the programme.
*     WHOLE = LOGICAL (Given)
*        If true, the parameters will not be prompted for.  Instead, the
*        extreme axis values will be taken.
*     START = REAL (Returned)
*        The data value of the start of the range selected.
*     END = REAL (Returned)
*        The data value of the end of the range selected.
*     ISTART = INTEGER (Returned)
*        The lower of the two pixel numbers corresponding to START and
*        END.
*     IEND = INTEGER (Returned)
*        The higher of the two pixel numbers corresponding to START and
*        END. If CONTROL indicates 'Unconstrained', then START and/or
*        END may be outside the range of axis values.  In this case
*        ISTART/IEND will be returned as 1 or N (= number of elements in
*        data).  That is, ISTART and IEND always refer to valid elements
*        of the data.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     ks: Keith Shortridge (AAO)
*     jms: ? (AAO)
*     hme: Horst Meyerdierks (UoE, Starlink)
*     MJC: Malcolm J. Currie (STARLINK)
*     acd: Clive Davenhall (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     08 Jul 1987 (ks):
*        Original version.
*     05 Sep 1988 (ks):
*        PAR_ABORT calls added to support user requested aborts.
*     02 Jan 1990 (ks):
*        Revised to improve operation when axis values decrease.
*     18 Dec 1990 (jms):
*        Added a check to make sure ISTART, IEND are within allowed
*        limits when data array doesn't exist.
*     21 Aug 1992 (ks):
*        Automatic portability modifications ("INCLUDE" syntax etc)
*        made.
*     29 Aug 1992 (ks):
*        "INCLUDE" filenames now upper case.
*     05 Oct 1992 (hme):
*        TABs removed.
*     01 Feb 1996 (hme):
*        FDA library.
*     16 Feb 1996 (hme):
*        Translate between application-side status and Starlink status.
*     15 Aug 1996 (MJC):
*        No longer accesses the axis units component, as it is not
*        required for the Starlink PAR routines.  In case the axis
*        units are needed at some future time, protection against
*        undefined values is added.
*     21 Dec 2000 (acd):
*        Comment out unused variables (to correspond to the commented
*        out code).
*     2005 May 31 (MJC):
*        Use CNF_PVAL for pointers to mapped data.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! Standard DAT constants
      INCLUDE 'NDF_PAR'          ! Standard NDF constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Global Variables:
      INCLUDE 'DSA_COMMON'       ! DSA global variables

*  Arguments Given:
      CHARACTER * ( * ) REF_NAME
      INTEGER AXIS
      CHARACTER * ( * ) CONTROL
      LOGICAL WHOLE

*  Arguments Returned:
      REAL START
      REAL END
      INTEGER ISTART
      INTEGER IEND

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      REAL FMIN, FMAX            ! Arbitrarily just inside machine range
      PARAMETER ( FMIN = -1E38, FMAX = +1E38 )

*  Local Variables:
      LOGICAL LIMITED            ! Whether values constrained to axis
      LOGICAL SIMPLE             ! Whether data may be N-D (not Complex)
      LOGICAL EXIST              ! Indicates axis data exists
      LOGICAL REVERSE            ! Whether values in descending order
*     LOGICAL THERE              ! Whether axis units component exists
*     INTEGER IGNORE             ! Dummy status return from PAR_ routines
      INTEGER REF_SLOT           ! The reference slot
      INTEGER SLOT               ! Map slot handle
      INTEGER NDIM               ! Number of data array dimensions
      INTEGER DIMS( NDF__MXDIM ) ! Dimensions of axis data
      INTEGER ELEMENTS           ! Number of data array elements
      INTEGER ADDRESS            ! Address of the mapped array
      INTEGER NELM               ! Elements in 1st 1D array of axis data
      INTEGER ITEMP              ! Used to swap ISTART,IEND
      REAL FIRST                 ! First value in data array
      REAL LAST                  ! Last value in data array
      REAL VMAX                  ! Maximum value in axis data array
      REAL VMIN                  ! Minimum value in axis data array
      REAL PMAX                  ! Maximum acceptable parameter value
      REAL PMIN                  ! Minimum axcceptable parameter value
      CHARACTER AXIS_CH * ( 1 )  ! First letter of axis name
*     CHARACTER UNITS * ( 32 )   ! Units for data
      CHARACTER AXIS_NAMES * ( 6 ) ! First letter of parameter name

*  Internal References:
*     LOGICAL PAR_ABORT          ! Whether user aborted via (F)PAR
      INTEGER DSA2_BSEARCHF      ! Search array for value
      REAL DSA2_ELEMF            ! Return element value

*  Local Data:
      DATA AXIS_NAMES / 'XYTUVW' /

*.

*  Check inherited global status.
      IF ( STATUS .NE. 0 ) RETURN

*  Begin error context and translate status.
      CALL ERR_MARK
      STATUS = SAI__OK

*  This map slot value indicates whether we have to unmap or not.
      SLOT = 0

*  Look for 'Unconstrained', 'Complex' in CONTROL.
      LIMITED = INDEX( CONTROL, 'U' ) .EQ. 0
      SIMPLE  = INDEX( CONTROL, 'C' ) .EQ. 0

*  Get the reference slot.
      CALL DSA1_RFND( REF_NAME, REF_SLOT, STATUS )

*  Get the units for the axis.  Check that there is a defined value.
*  In this case the default value is used.  Although commented out now,
*  (as the Starlink PAR calls no longer need this component) the check
*  for a defined value is added as defensive programming.
*      UNITS = ' '
*      CALL NDF_ASTAT( DSA__REFID1(SLOT), 'UNITS', AXIS, THERE,
*     :   STATUS )
*      CALL NDF_ACGET( DSA__REFID1(REF_SLOT), 'UNITS', AXIS,
*     :   UNITS, STATUS )

*  Get dimensions - note that DSA_AXIS_SIZE will return the appropriate
*  axis size of the main data array, if the axis data does not exist.
      CALL DSA1_SKAX( REF_SLOT, AXIS, EXIST, STATUS )
      CALL DSA1_AXSIZ( REF_SLOT, AXIS, NDF__MXDIM,
     :   NDIM, DIMS, ELEMENTS, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 500

*  If it exists, see if it is multi-dimensional or simple 1D,
*  and if that is OK by the caller.
      IF ( EXIST .AND. NDIM .GT. 1 .AND. SIMPLE ) THEN
         CALL MSG_SETI( 'FDA_T002', AXIS )
         CALL MSG_SETC( 'FDA_T001', DSA__REFNAM(REF_SLOT) )
         CALL MSG_OUT( 'FDA_M009', 'Warning: The axis number ' //
     :      '^FDA_T002 in the reference ^FDA_T001 is ' //
     :      'multi-dimensional, which this application cannot ' //
     :      'handle correctly.', STATUS )
         CALL MSG_OUT( 'FDA_M010', 'Limits will be based on the ' //
     :      'first cross-section of the axis centre array.', STATUS )
      END IF

*  Map it.
      CALL DSA1_MAPCEN( REF_SLOT, AXIS, 'READ', 'FLOAT',
     :      ADDRESS, SLOT, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 500

*  Having mapped it, get the maximum and minimum values in it. Rather
*  than look at each element, we assume that the data extreme values
*  will be at the ends of the data array.  If the array is
*  multi-dimensional, and the application will accept that, we take the
*  maximum and minimum over the whole array.
      NELM  = DIMS(1)
      FIRST = DSA2_ELEMF( %VAL( CNF_PVAL(ADDRESS) ),    1 )
      LAST  = DSA2_ELEMF( %VAL( CNF_PVAL(ADDRESS) ), NELM )
      REVERSE = FIRST .GT. LAST
      IF ( NDIM .GT. 1 .AND. .NOT. SIMPLE ) THEN
         CALL DSA2_RANGEF( .FALSE., %VAL( CNF_PVAL(ADDRESS) ), 1,
     :                     ELEMENTS, VMAX, VMIN )
      ELSE
         VMIN = MIN(FIRST,LAST)
         VMAX = MAX(FIRST,LAST)
      END IF

*  Now see if we are restricted to the axis data values.
      IF ( LIMITED ) THEN
         PMIN = VMIN
         PMAX = VMAX
      ELSE
         PMIN = FMIN
         PMAX = FMAX
      END IF

*  See if WHOLE was specified.
*  There are alternative statements for (F)PAR.
      AXIS_CH = AXIS_NAMES(AXIS:AXIS)
      IF ( WHOLE ) THEN
         IF ( .NOT. REVERSE ) THEN
            START = VMIN
            END   = VMAX
         ELSE
            START = VMAX
            END   = VMIN
         END IF
*        CALL PAR_SDVAL( AXIS_CH // 'START', START, IGNORE )
*        CALL PAR_SDVAL( AXIS_CH // 'END',     END, IGNORE )
         CALL PAR_DEF0R( AXIS_CH // 'START', START, STATUS )
         CALL PAR_DEF0R( AXIS_CH // 'END',     END, STATUS )
      ELSE

*     If not, get the parameter values.
*     There are alternative statements for (F)PAR.
         IF ( .NOT. REVERSE ) THEN
*           CALL PAR_RDVAL( AXIS_CH // 'START', PMIN, PMAX, VMIN, UNITS,
*    :         START )
*           CALL PAR_RDVAL( AXIS_CH // 'END',  START, PMAX, VMAX, UNITS,
*    :         END )
            CALL PAR_GDR0R( AXIS_CH // 'START', VMIN, PMIN,  PMAX,
     :         .FALSE., START, STATUS )
            CALL PAR_GDR0R( AXIS_CH // 'END',   VMAX, START, PMAX,
     :         .FALSE., END,   STATUS )
         ELSE
*           CALL PAR_RDVAL( AXIS_CH // 'START', PMIN, PMAX, VMAX, UNITS,
*    :         START )
*           CALL PAR_RDVAL( AXIS_CH // 'END',   PMIN, START,VMIN, UNITS,
*    :         END )
            CALL PAR_GDR0R( AXIS_CH // 'START', VMAX, PMIN, PMAX,
     :         .FALSE., START, STATUS )
            CALL PAR_GDR0R( AXIS_CH // 'END',   VMIN, PMIN, START,
     :         .FALSE., END,   STATUS )
         END IF
      END IF

*  Test the parameter system abort flag.
*  There are alternative statements for (F)PAR.
*     IF ( PAR_ABORT() ) THEN
*        STATUS = SAI__ERROR
*        GO TO 500
*     END IF
      IF ( STATUS .NE. SAI__OK ) GO TO 500

*  Get the values in terms of element numbers.  Note that this is done
*  only in terms of the first 1d array of the axis data.
      ISTART = DSA2_BSEARCHF( %VAL( CNF_PVAL(ADDRESS) ), NELM, START )
      IEND   = DSA2_BSEARCHF( %VAL( CNF_PVAL(ADDRESS) ), NELM, END   )
      IF ( ISTART .LT. 1 ) ISTART = 1
      IF ( IEND   .LT. 1 ) IEND   = NELM
      IF ( ISTART .GT. IEND ) THEN
         ITEMP  = ISTART
         ISTART = IEND
         IEND   = ITEMP
      END IF

*  Tidy up.
 500  CONTINUE
      IF ( SLOT .NE. 0 ) CALL DSA1_UNMAP( SLOT, STATUS )

*  Translate status and end error context.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_FLUSH( STATUS )
         STATUS = 1
      ELSE
         STATUS = 0
      END IF
      CALL ERR_RLSE

      END




      INTEGER FUNCTION DSA2_BSEARCHF( ARRAY, NV, VALUE )

      IMPLICIT NONE

      INTEGER NV
      REAL ARRAY(NV),VALUE

      LOGICAL ENDED
      INTEGER L,R,M
      REAL    VMAX,VMIN

      VMAX=MAX(ARRAY(1),ARRAY(NV))
      VMIN=MIN(ARRAY(1),ARRAY(NV))
      IF ((VALUE.GT.VMAX).OR.(VALUE.LT.VMIN)) THEN
         DSA2_BSEARCHF=0
      ELSE
         L=1
         R=NV
         ENDED=.FALSE.

	 IF (NV .EQ. 1) THEN
	   DSA2_BSEARCHF = 1
	   RETURN
	 END IF

         IF (ARRAY(1).LT.ARRAY(NV)) THEN

            IF ((ARRAY(1).LE.VALUE).AND.(ARRAY(2).GT.VALUE)) THEN
               DSA2_BSEARCHF=1
               IF ((VALUE-ARRAY(1)).GT.(ARRAY(2)-VALUE))
     :                                          DSA2_BSEARCHF=2
               ENDED=.TRUE.
            ELSE IF ((ARRAY(NV-1).LE.VALUE).AND.(ARRAY(NV).GE.VALUE))
     :                                                           THEN
               DSA2_BSEARCHF=NV-1
               IF ((VALUE-ARRAY(NV-1)).GT.(ARRAY(NV)-VALUE))
     :                                          DSA2_BSEARCHF=NV
               ENDED=.TRUE.
            END IF
            DO WHILE (.NOT.ENDED)
               M=(L+R)/2
               IF ((ARRAY(M).LE.VALUE).AND.(ARRAY(M+1).GT.VALUE)) THEN
                  DSA2_BSEARCHF=M
                  IF ((VALUE-ARRAY(M)).GT.(ARRAY(M+1)-VALUE))
     :                                          DSA2_BSEARCHF=M+1
                  ENDED=.TRUE.
               ELSE IF (VALUE.LT.ARRAY(M)) THEN
                  R=M
               ELSE
                  L=M
               END IF
            END DO

         ELSE

            IF ((ARRAY(1).GE.VALUE).AND.(ARRAY(2).LT.VALUE)) THEN
               DSA2_BSEARCHF=1
               IF ((ARRAY(1)-VALUE).GT.(VALUE-ARRAY(2)))
     :                                          DSA2_BSEARCHF=2
               ENDED=.TRUE.
            ELSE IF ((ARRAY(NV-1).GE.VALUE).AND.(ARRAY(NV).LE.VALUE))
     :                                                            THEN
                  DSA2_BSEARCHF=NV-1
                  IF ((ARRAY(NV-1)-VALUE).GT.(VALUE-ARRAY(NV)))
     :                                          DSA2_BSEARCHF=NV
               ENDED=.TRUE.
            END IF
            DO WHILE (.NOT.ENDED)
               M=(L+R)/2
               IF ((ARRAY(M).GE.VALUE).AND.(ARRAY(M+1).LT.VALUE)) THEN
                  DSA2_BSEARCHF=M
                  IF ((VALUE-ARRAY(M)).LT.(ARRAY(M+1)-VALUE))
     :                                          DSA2_BSEARCHF=M+1
                  ENDED=.TRUE.
               ELSE IF (VALUE.GT.ARRAY(M)) THEN
                  R=M
               ELSE
                  L=M
               END IF
            END DO

         END IF

      END IF

      END



      REAL FUNCTION DSA2_ELEMF(ARRAY,N)

      IMPLICIT NONE

      INTEGER N
      REAL ARRAY(N)

      DSA2_ELEMF=ARRAY(N)

      END



      SUBROUTINE DSA2_RANGEF( BAD, ARRAY, IST, IEN, VMAX, VMIN )

      IMPLICIT NONE

      INCLUDE 'PRM_PAR'

      LOGICAL BAD
      INTEGER IST, IEN
      REAL ARRAY(IEN), VMIN, VMAX

      LOGICAL FOUND
      INTEGER I

      IF ( .NOT. BAD ) THEN
         VMIN = ARRAY(IST)
         VMAX = VMIN
         IF (IST .LT. IEN ) THEN
            DO 1 I = IST + 1, IEN
               VMIN = MIN( VMIN, ARRAY(I) )
               VMAX = MAX( VMAX, ARRAY(I) )
 1          CONTINUE
         END IF
      ELSE
         FOUND = .FALSE.
         DO 2 I = IST, IEN
            IF ( ARRAY(I) .NE. VAL__BADR ) THEN
               IF ( FOUND ) THEN
                  VMIN = MIN( VMIN, ARRAY(I) )
                  VMAX = MAX( VMAX, ARRAY(I) )
               ELSE
                  VMIN = ARRAY(I)
                  VMAX = ARRAY(I)
                  FOUND = .TRUE.
               END IF
            END IF
 2       CONTINUE
      END IF

      END
