*+  SETRANGES - Set text parameter RANGES according to object values
      SUBROUTINE SETRANGES( STATUS )
*
*    Description :
*
*     SETRANGES is used to 'map' one data array onto another.
*     It is best understood by an example of its use.
*     In the case of a time series  it can  be used to find all the values
*     of time when the counts fall within a certain range. Another example
*     is that it can find all the times when Quality is set to any given value.
*
*        The usual mode of operation is to pass an ICL variable which is
*     then written with the text equivalent of the ranges.
*
*    Parameters :
*
*     input_Value(1D)= real (read)
*       This is the array of data from which you are trying to select subsets
*       having a certain range of values. In the example above they correspond
*       to counts or quality
*
*     input_Range(1D)= real (read)
*       This is the array of data which corresponds to the data in input_Value.
*       In the example above it corresponds to time.
*
*     limits(1D)= real (read)
*       This array of size 2 contains the lower and upper bounds of the values
*       of input_Value for which you are searching. In the examples it would
*       be 10,100 (cts per sec) or 1,2 (Quality).
*
*     Ranges(1D)= real (update)
*       This is the parameter which is used to put the array SWITCH into the
*       parameter system for use by other programs.
*
*    Method :
*
*     Set up variables and initialise
*     Get data arrays Value_object and Range_object
*     Check the arrays are primitive ( the second can be a spaced array )
*     Map the arrays
*     Check arrays are of equal length
*     Obtain maximum and minimum value of Value object via ARR_RANG1R
*     Write maximum and Minimum values of Value_object to screen
*     Get the required range to be selected into the array Value_ranges
*     Check that two values have been entered, if not offer second attempt
*     Check that 1st value is smaller than second value, reprompt if necessary
*     Call SETRANGES_OPERATION  carry out task
*     Results are returned in the form of a 1D array of size N
*     If N = 0 exit
*     If N non-zero test for existence of suitable data_object to pass
*     via parameter system. If none found create one, if one is found check its
*     dimensions and modify as needed.
*     Par_Put array into parameter system under name RANGES
*     Tidy up and exit
*
*    Deficiencies :
*
*     Cannot accept more than 500 sets of start and end values without
*     increaseing the size of array SWITCH
*
*    Bugs :
*     If you enter JK,1000 or similar to prompt Value_ranges it does not
*     return a bad status but picks up a value for the first number from
*     somewhere and carries on.
*
*    Authors :
*
*     John Davies (BHVAD::JKD)
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     Original  VERSION 1.0   15 August 1986
*                       1.1   29 sept 86 Replace USI_PUT with DAT_PUT
*                       1.2   27 Mar 87 Global parameter implemented properly
*                                                                (BHVAD::RJV)
*      9 Jul 87 : V0.6-1 Version number changed to ASTERIX standard (TJP)
*     11 Nov 87 : V0.6-2 UTIL_SHOW used, WRITE statements removed
*                                                 (BHVAD::ADM)
*      8 Aug 88 : V1.0-0 General tidy up for Asterix88 (DJA)
*     10 Oct 88 : V1.0-1 Modified to allow spaced arrays (DJA)
*     15 Dec 88 : V1.0-2 Now writes ranges in text form into SETRANGES's own
*                        global. (DJA)
*      6 Aug 93 : V1.7-0 Changed ARR_ call (DJA)
*     24 Nov 94 : V1.8-0 Now use USI for user interface (DJA)
*
*    Type Definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*
*    Status :
*
      INTEGER	              STATUS
*
*    Local Constants :
*
      INTEGER	              SWTSZE
         PARAMETER ( SWTSZE = 1000 )
*
*    Local variables :
*
      CHARACTER*(DAT__SZLOC)  OLOC       ! Value object locator
      CHARACTER*(DAT__SZLOC)  PLOC       ! Range object locator
      CHARACTER*20            VARIANT    ! Variant of data array
      CHARACTER*40            TSTR       ! Temporary string
      CHARACTER RNGSTR*(SWTSZE*25)        ! Range global string

      LOGICAL		      PRIM1      ! Value_object primitive?
      LOGICAL		      PRIM2      ! Range_object primitive?
      LOGICAL                 OK         ! General validity test

      INTEGER		      ACTVAL     ! Dummy required by USI_GET
      INTEGER                 I,J        ! Loop counters
      INTEGER		      N          ! Number of values in array of results
      INTEGER                 NDIM
      INTEGER                 DIMS(DAT__MXDIM)
      INTEGER		      VALVAL     ! Number of elements in Value_object
      INTEGER		      VALPTR     ! Pointer to mapped Value_object
      INTEGER		      RNGVAL     ! Number of elements in Range_object
      INTEGER		      RNGPTR     ! Pointer to mapped Range_object
      INTEGER                 RLEN,TLEN  ! Lengths of strings

      REAL                    BASE,SCALE ! Spaced axis parameters
      REAL		      MINVAL     ! Minimum value of specified range
      REAL		      MAXVAL     ! Maximum value of specified range
      REAL		      RANGE(2)   ! Value of range max and min from parameter sysytem
      REAL		      SWITCH(SWTSZE) ! Array of results passed from subroutine
      REAL		      VALMAX     ! Maximum value from Value Obj
      REAL		      VALMIN     ! Minimum value from Value Obj
*
*    Version id :
*
      CHARACTER*23	      VERSION    ! Version id
	 PARAMETER            ( VERSION = 'SETRANGES Version 1.8-0')
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Version announcement
      CALL MSG_PRNT( VERSION )

*    Initialise Asterix
      CALL AST_INIT

*    Get the value data object, check that it is primitive
      CALL USI_ASSOCI( 'INPUT_VALUE', 'READ', OLOC, PRIM1, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

      IF ( .NOT. PRIM1 ) THEN
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'This is not a primitive object,'/
     :                         /' please try again', STATUS )

      ELSE
        CALL BDA_CHKDATA( OLOC, OK, NDIM, DIMS, STATUS )
        IF ( OK ) THEN
          CALL ARR_SUMDIM( NDIM, DIMS, VALVAL )
        ELSE IF ( STATUS .EQ. SAI__OK ) THEN
          STATUS = SAI__ERROR
          CALL ERR_REP( ' ', 'Numeric data object required', STATUS )
        END IF
        CALL BDA_MAPDATA( OLOC, 'READ', VALPTR, STATUS )

      END IF

*    Now the range object - if not primitive then check for the case where
*    we have a spaced data array.
      CALL USI_ASSOCI( 'INPUT_RANGE', 'READ', PLOC, PRIM2, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

      IF ( PRIM2 ) THEN
        CALL DAT_MAPV( PLOC, '_REAL', 'READ', RNGPTR, RNGVAL, STATUS )

      ELSE
        CALL CMP_GET0C( PLOC, 'VARIANT', VARIANT, STATUS )
        IF (( STATUS .EQ. SAI__OK ) .AND. (VARIANT(1:6)
     :                                   .EQ.'SPACED')) THEN

*        Look for SCALE, DIMENSION and BASE.
          CALL CMP_GET0R( PLOC, 'SCALE', SCALE, STATUS )
          CALL CMP_GET0R( PLOC, 'BASE', BASE, STATUS )
          CALL CMP_GET0I( PLOC, 'DIMENSION', RNGVAL, STATUS )

          IF ( STATUS .EQ. SAI__OK ) THEN

*          We seem to have good spaced array axis components...
*          Map some dynamic memory and fill it with axis data
            CALL DYN_MAPR( 1, RNGVAL, RNGPTR, STATUS )
            CALL ARR_REG1R( BASE, SCALE, RNGVAL, %VAL(RNGPTR), STATUS )

          ELSE

*          Right, give up...
            CALL ERR_REP( ' ', 'Don''t recognise this kind'
     :                         //' of data array', STATUS )

          END IF

        ELSE
          STATUS = SAI__ERROR
          CALL ERR_REP( ' ', 'Not primitive array or'
     :                    //' spaced array', STATUS )
        END IF

      END IF
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Check data objects have same shape
      IF ( VALVAL .NE. RNGVAL ) THEN
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'Range and Value objects do not'/
     :   /' have same number of values, aborting program', STATUS )
        GOTO 99
      END IF

*    Get maximum and minimun values of Value Obj and write to Screen
*    to help user select required value_ranges
      CALL ARR_RANG1R( VALVAL, %VAL(VALPTR), VALMIN, VALMAX, STATUS )
      CALL MSG_SETR( 'VALMIN', VALMIN )
      CALL MSG_PRNT( 'Minimum value of Value object: ^VALMIN' )
      CALL MSG_SETR( 'VALMAX', VALMAX )
      CALL MSG_PRNT( 'Maximum value of Value object: ^VALMAX' )

*    Get required ranges from parameter system and set values of MINVAL
*    and MAXAVL
      CALL USI_GET1R( 'LIMITS', 2, RANGE, ACTVAL, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
        GOTO 99
      ELSE
        MINVAL = RANGE(1)
        MAXVAL = RANGE(2)
        IF ( MINVAL .GT. MAXVAL ) THEN
          CALL MSG_PRNT( '1st value must NOT be larger than 2nd.' )
          CALL MSG_PRNT( 'Please try again' )
        END IF
      END IF

*    Call subroutine to do the work
      CALL SETRANGES_OPERATION( VALVAL, %VAL(VALPTR), %VAL(RNGPTR),
     :                  MINVAL, MAXVAL, SWITCH, SWTSZE, N, STATUS )

*    IF no values in required range then write message and exit
      IF ( N .EQ. 0 ) THEN
        CALL MSG_PRNT( ' No elements of required range in'/
     :                                   /' Value_object' )
        GOTO 99

      ELSE
        RLEN = 1
        RNGSTR = ' '
        J = 1
        DO I = 1, N/2
          CALL MSG_SETR( 'LOW', SWITCH(J) )
          CALL MSG_SETR( 'HIGH', SWITCH(J+1) )
          CALL MSG_MAKE( ' ^LOW:^HIGH', TSTR, TLEN )
          RNGSTR = RNGSTR(:RLEN)//TSTR(:TLEN)
          J = J + 2
          RLEN = RLEN + TLEN
        END DO

*      Put character equivalents of strings into parameter
        CALL USI_PUT0C( 'RANGES', RNGSTR(3:RLEN), STATUS )

      END IF

*    Tidy up
 99   CALL AST_CLOSE
      CALL AST_ERR( STATUS )

      END


*+  SETRANGES_OPERATION - Performs setranges's function
      SUBROUTINE SETRANGES_OPERATION(NVALS,VALUE,RANGE,MINVAL
     :                        ,MAXVAL,SWITCH,SWTSZE,N,STATUS)
*    Method :
*
*     First test if 1st value of Value_Object is in required range.
*        Y : set START= TRUE and set ON to be the first value of Range_object
*        N : Try the next value until you do find a value in the required range
*            and set ON to be equivalent value of Range Object.
*     When set START= TRUE start looking for the first value which falls
*     outside the required range. When this is found set END = TRUE and set
*     OFF equal to the value of Range object BEFORE the current one. This is
*     necessary because END becomes TRUE when the first value outside the
*     range is found, not when the last value inside the range is found, so
*     it is necessary to pick the last value of Range_Object, not the current
*     one. ON and OFF now mark the values of Range_object at the begining and
*     end of the first block of data which  falls in the required range.
*
*     Once START and END are both true write the stored value of ON and OFF
*     into the array SWITCH and set START and END to False again. Then cont-
*     inue through data to find the next block of data within the correct
*     range. Repeatas necessary, building up the array SWITCH.
*
*     In the case where a value has been found for ON but the loop reaches the
*     end of the data before finding a value for OFF, set the off value equal
*     to the last data point. Failure to this results in the last block being
*     ignored.
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*
*    Status :
*
      INTEGER STATUS
*
*    Import :
*
      INTEGER NVALS                         ! Number of elements of Value_object
      INTEGER SWTSZE                        ! Size of array SWITCH

      REAL VALUE(NVALS)                     ! Array of Value_object values
      REAL RANGE(NVALS)                     ! Array of Range_object values
      REAL MAXVAL                           ! Maximum value of requested range
      REAL MINVAL                           ! Minimum value of requested range
*
*    Export :
*
      REAL SWITCH(SWTSZE)                   ! Array of switch on and off values
      INTEGER N                             ! Number of on or off switches
*
*    Local variables :
*
      REAL ON                               ! Value of range_object when switch is on
      REAL OFF                              ! Value of range_object when switched off

      INTEGER I                             ! Do loop counter

      LOGICAL START                         ! True if there is a currentvalue for switch on
      LOGICAL END                           ! True if there is a current value for switch off
*-

*   Initialise variables
      START=.FALSE.
      END=.FALSE.
      N=0

*    Set on and off switches and write values to array SWITCH one or
*    more times
      DO I=1,NVALS

        IF(.NOT.START) THEN
          IF(VALUE(I).GE.MINVAL.AND.VALUE(I).LE.MAXVAL) THEN
            ON=RANGE(I)
            START=.TRUE.
          END IF
        ELSEIF(START) THEN
          IF(VALUE(I).LT.MINVAL.OR.VALUE(I).GT.MAXVAL) THEN
            OFF=RANGE(I-1)
            END=.TRUE.
          END IF
        END IF

*   If loop finds a start value  but reaches the end of the array without
*   finding an end value, then set end to true after leaving the loop and set
*   value of OFF equal to last array element . Failure
*   to do this means that the last pair of ranges are not set. In the case where
*   all the values are in the specified range, failure to set End = true causes
*   the program to fail completely
        IF ( START .AND..NOT. END .AND. I .EQ. NVALS ) THEN
          OFF=RANGE(I)
          END=.TRUE.
        END IF

*   If start and end are both true, then write resulting start and end values
*   into array switch
        IF ( START .AND. END ) THEN
          N=N+1
          SWITCH(N)=ON
          N=N+1
          SWITCH(N)=OFF
          START=.FALSE.
          END=.FALSE.
        END IF

*   If there are more than SWTSZE 'slices' of data then the array SWITCH will
*   overflow since it will contain SWTSZE*2 plus on off times. To prevent this
*   the program tests to check the counter N and if N=2*SWTSZE exits the loop
*   and returns to the main program with a suitable message.
        IF ( N .EQ. (SWTSZE*2) ) THEN
          CALL MSG_SETI( 'DSLICES', SWTSZE )
          CALL MSG_PRNT( 'More than ^DSLICES data slices, parameter'/
     :                       /' RANGES now full. Program terminated'/
     :                                    /' before end of dataset' )
          CALL MSG_SETR('OFF',OFF)
          CALL MSG_PRNT( 'Last value of Range_object is ^OFF ' )
          GOTO 99

        END IF

      END DO

  99  CONTINUE

      END
