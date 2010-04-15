
      PROGRAM PERIOD_MAIN

C=============================================================================
C Package to search for periodicities in data using a number of techniques.
C
C Written by Vikram Singh Dhillon @LPO 24-January-1992.
C
C Parameter lists for following routines have been modified
C
C PERIOD_FIT, PERIOD_INPUT, PERIOD_PERIOD
C
C GJP June 1995
C
C Added OGIP option.
C
C GJP September 1995
C
C Modified call to PLT for LINUX port.
C
C GJP March 1997
C
C Removed NVEC references and modified PERIOD_PERIOD and
C PERIOD_PLT call. Initialised some variables. Removed
C PLT common block. Modified parameter list for PERIOD_LOG.
C
C Converted to Double Precision (KPD), August 2001
C Modified to incorporate dynamic memory allocation for major
C  data/work array(s) and/or use of such arrays (KPD), October 2001
C=============================================================================

      IMPLICIT NONE

C-----------------------------------------------------------------------------
C Loop variables.
C-----------------------------------------------------------------------------

      INTEGER I

C-----------------------------------------------------------------------------
C PLT declarations.
C-----------------------------------------------------------------------------

      INTEGER MXCOL
      PARAMETER (MXCOL=3)

C-----------------------------------------------------------------------------
C PERIOD_MAIN declarations.
C-----------------------------------------------------------------------------

      INTEGER MXSLOT
      PARAMETER (MXSLOT=40)
      INTEGER YPTR(MXSLOT), NPTSARRAY(MXSLOT), LOGUNIT
      DOUBLE PRECISION SIG(2, MXSLOT)
      CHARACTER*12 COMMAND
      CHARACTER*72 INFILEARRAY(MXSLOT), LOGFILE
      LOGICAL PERIOD_PARSE, LOG, EXIST
      LOGICAL YERRORARRAY(MXSLOT), DETRENDARRAY(MXSLOT)
      DATA LOGUNIT/99/


C-----------------------------------------------------------------------------
C Initialisation.
C-----------------------------------------------------------------------------

*   Main declarations.

      LOG=.FALSE.
      EXIST=.FALSE.
      COMMAND=' '
      LOGFILE=' '

      DO 10 I=1,MXSLOT
        YERRORARRAY(I)=.FALSE.
        DETRENDARRAY(I)=.FALSE.
        NPTSARRAY(I)=0
        INFILEARRAY(I)=' '
        SIG(1,I)=0.0D0
        SIG(2,I)=0.0D0
 10   CONTINUE

C-----------------------------------------------------------------------------
C Menu.
C-----------------------------------------------------------------------------

      CALL PERIOD_INTRO
 100  CONTINUE
      WRITE (*, *) ' '
      WRITE (*, *) 'Options.'
      WRITE (*, *) '--------'
      WRITE (*, *) ' '
      WRITE (*, *) '   INPUT    --  Input ASCII file data.'
      WRITE (*, *) '   OGIP     --  Input OGIP FITS table data'
      WRITE (*, *) '   FAKE     --  Create fake data.'
      WRITE (*, *) '   NOISE    --  Add noise to data.'
      WRITE (*, *) '   DETREND  --  Detrend the data.'
      WRITE (*, *) '   WINDOW   --  Set data points to unity.'
      WRITE (*, *) '   OPEN     --  Open a log file.'
      WRITE (*, *) '   CLOSE    --  Close the log file.'
      WRITE (*, *) '   PERIOD   --  Find periodicities.'
      WRITE (*, *) '   FIT      --  Fit sine curve to folded data.'
      WRITE (*, *) '   FOLD     --  Fold data on given period.'
      WRITE (*, *) '   SINE     --  +, -, / or * sine curves.'
      WRITE (*, *) '   PLT      --  Plot the contents of a slot.'
      WRITE (*, *) '   STATUS   --  Information on stored data.'
      WRITE (*, *) '   OUTPUT   --  Output data.'
      WRITE (*, *) '   HELP     --  On-line help.'
      WRITE (*, *) '   QUIT     --  Quit PERIOD.'
      WRITE (*, *) '  '
      WRITE (*, '(X,A,$)') 'PERIOD> '
      READ (*, '(A)', ERR=100) COMMAND
      CALL PERIOD_CASE(COMMAND, .TRUE.)
      IF ( PERIOD_PARSE(COMMAND,'INPUT') ) THEN

C-----------------------------------------------------------------------------
C Input ASCII data.
C-----------------------------------------------------------------------------

C Unused paramter MXVEC removed - GJP June 1995

         CALL PERIOD_INPUT(YPTR, MXCOL, MXSLOT,
     :                     NPTSARRAY, INFILEARRAY,
     :                     YERRORARRAY, DETRENDARRAY)
      ELSE IF ( PERIOD_PARSE(COMMAND,'OGIP') ) THEN

C-----------------------------------------------------------------------------
C Input OGIP FITS data.
C-----------------------------------------------------------------------------

         CALL PERIOD_OGIP(YPTR, MXCOL, MXSLOT,
     :                     NPTSARRAY, INFILEARRAY,
     :                     YERRORARRAY, DETRENDARRAY)
      ELSE IF ( PERIOD_PARSE(COMMAND,'NOISE') ) THEN

C-----------------------------------------------------------------------------
C Add noise to the data.
C-----------------------------------------------------------------------------

         CALL PERIOD_NOISE(YPTR, MXCOL, MXSLOT, NPTSARRAY,
     :                     YERRORARRAY, INFILEARRAY, DETRENDARRAY)
      ELSE IF ( PERIOD_PARSE(COMMAND,'WINDOW') ) THEN

C-----------------------------------------------------------------------------
C Set data points to unity for window function generation.
C-----------------------------------------------------------------------------

         CALL PERIOD_WINDOW(YPTR, MXCOL, MXSLOT, NPTSARRAY,
     :                      YERRORARRAY, INFILEARRAY, DETRENDARRAY)
      ELSE IF ( PERIOD_PARSE(COMMAND,'FAKE') ) THEN

C-----------------------------------------------------------------------------
C Create fake data.
C-----------------------------------------------------------------------------

         CALL PERIOD_FAKE(YPTR, MXCOL, MXSLOT, NPTSARRAY,
     :                    INFILEARRAY, YERRORARRAY, DETRENDARRAY)
      ELSE IF ( PERIOD_PARSE(COMMAND,'DETREND') ) THEN

C-----------------------------------------------------------------------------
C Detrend the data.
C-----------------------------------------------------------------------------

         CALL PERIOD_DETREND(YPTR, MXCOL, MXSLOT, NPTSARRAY,
     :                       YERRORARRAY, DETRENDARRAY, INFILEARRAY)
      ELSE IF ( PERIOD_PARSE(COMMAND,'SINE') ) THEN

C-----------------------------------------------------------------------------
C Add or subtract sine curves.
C-----------------------------------------------------------------------------

         CALL PERIOD_SINE(YPTR, MXCOL, MXSLOT, NPTSARRAY,
     :                    DETRENDARRAY, YERRORARRAY, INFILEARRAY)
      ELSE IF ( PERIOD_PARSE(COMMAND,'FOLD') ) THEN

C-----------------------------------------------------------------------------
C Fold data on given period.
C-----------------------------------------------------------------------------

         CALL PERIOD_PHASE(YPTR, MXCOL, MXSLOT, NPTSARRAY,
     :                     DETRENDARRAY, INFILEARRAY, YERRORARRAY)
      ELSE IF ( PERIOD_PARSE(COMMAND,'FIT') ) THEN

C-----------------------------------------------------------------------------
C Fit sine curve to data.
C-----------------------------------------------------------------------------

C Unused parameter LOGFILE removed - GJP June 1995

         CALL PERIOD_FIT(YPTR, MXCOL, MXSLOT, NPTSARRAY,
     :                   DETRENDARRAY, YERRORARRAY, INFILEARRAY,
     :                   LOG, LOGUNIT)
      ELSE IF ( PERIOD_PARSE(COMMAND,'OPEN') ) THEN


C-----------------------------------------------------------------------------
C Open log file.
C-----------------------------------------------------------------------------

         LOG = .TRUE.
         CALL PERIOD_LOG(LOGFILE, LOG, LOGUNIT, EXIST)
      ELSE IF ( PERIOD_PARSE(COMMAND,'CLOSE') ) THEN

C-----------------------------------------------------------------------------
C Close log file.
C-----------------------------------------------------------------------------

         LOG = .FALSE.
         CALL PERIOD_LOG(LOGFILE, LOG, LOGUNIT, EXIST)
      ELSE IF ( PERIOD_PARSE(COMMAND,'PERIOD') ) THEN

C-----------------------------------------------------------------------------
C Find periodicities.
C-----------------------------------------------------------------------------

C Unused parameter LOGFILE removed - GJP June 1995

         CALL PERIOD_PERIOD(YPTR, MXCOL, MXSLOT, NPTSARRAY,
     :                      INFILEARRAY, YERRORARRAY, DETRENDARRAY,
     :                      SIG, LOG, LOGUNIT)
      ELSE IF ( PERIOD_PARSE(COMMAND,'PLT') ) THEN

C-----------------------------------------------------------------------------
C Call PLT.
C-----------------------------------------------------------------------------

         CALL PERIOD_PLT(YPTR, NPTSARRAY, MXCOL, MXSLOT,
     :                   INFILEARRAY, YERRORARRAY)
      ELSE IF ( PERIOD_PARSE(COMMAND,'STATUS') ) THEN

C-----------------------------------------------------------------------------
C Info on data and fits.
C-----------------------------------------------------------------------------

         CALL PERIOD_STATUS(YPTR, MXCOL, MXSLOT, NPTSARRAY,
     :                      YERRORARRAY, INFILEARRAY, DETRENDARRAY,
     :                      LOGFILE, LOG, LOGUNIT)
      ELSE IF ( PERIOD_PARSE(COMMAND,'OUTPUT') ) THEN

C-----------------------------------------------------------------------------
C Output data.
C-----------------------------------------------------------------------------

         CALL PERIOD_OUTPUT(YPTR, MXCOL, MXSLOT, NPTSARRAY,
     :                      YERRORARRAY)
      ELSE IF ( PERIOD_PARSE(COMMAND(1:4),'HELP') ) THEN

C-----------------------------------------------------------------------------
C On-line help.
C-----------------------------------------------------------------------------

         CALL PERIOD_HELP(COMMAND)
      ELSE IF ( PERIOD_PARSE(COMMAND,'QUIT') ) THEN

C-----------------------------------------------------------------------------
C Exit period.
C-----------------------------------------------------------------------------

C***** SPAG has made duplicate copies of the following statement
         CALL PERIOD_QUIT
      ELSE IF ( PERIOD_PARSE(COMMAND,'EXIT') ) THEN
C***** The following statement is a duplicate copy made by SPAG
         CALL PERIOD_QUIT

      END IF
      GO TO 100

      END
