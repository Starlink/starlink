*+  XRTHK - Select on housekeeping data
      SUBROUTINE XRTHK(STATUS)
*    Description :
*     Opens house keeping files and allows constraints to be set on
*     any of the housekeeping parameters. As many parameters as the
*     user wants may be selected and a single text file containing
*     the ON and OFF times as a series of MJDs is produced at the
*     end.
*    Environment parameters :
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*     Richard Saxton (LTVAD::RDS)
*    History :
*     24-FEB-1992  original
*     21-Feb-1994  (1.6-3) RDF file names and column names (LTVAD::JKA)
*     24-Apr-1994  (1.7-0) for new release of asterix
*     27-May-1994  Opens output textfile with STATUS="UNKNOWN"
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PAR_ERR'
*    Global variables :
*     <global variables held in named COMMON>
*    Structure definitions :
      INCLUDE 'XRTLIB(INC_XRTSRT)'
      INCLUDE 'XRTLIB(INC_XRTHEAD)'
*    Status :
      INTEGER STATUS
*    Function declarations :
      INTEGER CHR_LEN
        EXTERNAL CHR_LEN
*    Local constants :
*     <local constants defined by PARAMETER>
*    Local variables :
      CHARACTER*20 EXT                          ! filename extension
      CHARACTER*20 COL                          ! HDS column/array name
      RECORD /XRT_SCFDEF/ SRT
      RECORD /XRT_HEAD/ HEAD
*
      CHARACTER*7 PARAM                         ! Name of ADAM HK parameter
      CHARACTER*6 PARMIN,PARMAX                 ! Name of ADAM min,max params
      CHARACTER*40 VERS                         !
      CHARACTER*23 CSTRING1,CSTRING2            ! MJD strings
      CHARACTER*60 ERFILE                       ! Name of eventrate file
      CHARACTER*60 ATTFIL                       ! Name of attitude file

      CHARACTER*(DAT__SZLOC) ERLOC               ! Locator to EVR file
      CHARACTER*(DAT__SZLOC) ATTLOC              ! Locator to ATT file
      CHARACTER*(DAT__SZLOC) EVTLOC              ! Locator to times in EVR file
      CHARACTER*(DAT__SZLOC) AFTLOC              ! Locator to times in ATT file
      CHARACTER*(DAT__SZLOC) FLOC                ! Generic file locator
      CHARACTER*2  CPLP                         ! Parameter number as a char.
      CHARACTER*30 HKNAME                       ! Name of HK parameter
      CHARACTER*40 FNAME                        ! Name for output file

      REAL RMIN,RMAX                            ! Min and max array values
      REAL HKMIN,HKMAX                          ! User selected min and max
*                                               ! values for this HK parameter.
      REAL EXPO_TIM                             ! Total exposure time
      REAL INTERVAL                             ! Minimum length of GOOD window
      DOUBLE PRECISION TBAD(MAXRAN*2)           ! Times of BAD windows (S/C TIM)
      DOUBLE PRECISION TGOOD(MAXRAN*2)          ! Times of GOOD windows (MJD)

      LOGICAL LEVR                              ! Was eventrate file opened ?
      LOGICAL LATT                              ! Was attitude file opened ?
      LOGICAL JUMPOUT                           ! Leave loop ??

      INTEGER ENTIM                             ! Number of times in evr file
      INTEGER ANTIM                             ! Number of times in ATT file
      INTEGER PNTR                              ! Pointer to HK data
      INTEGER TPNTR                             ! Pointer to times
      INTEGER EV_TPNTR                          ! Pointer to times in EVR file
      INTEGER AT_TPNTR                          ! Pointer to ATTITUDE times
      INTEGER OPNTR                             ! Pointer to ONOFF TIM_SEL vals
      INTEGER WPNTR                             ! Workspace array
      INTEGER W2PNTR                             ! Workspace array

      INTEGER NBAD                              ! Number of bad time windows
      INTEGER NGOOD                             ! Number of good time windows
      INTEGER NTIM                              ! Number of values in HK array
      INTEGER MUNIT                             ! Logical unit of output file
      INTEGER IND1A,IND1B,IND2A,IND2B
      INTEGER PLP,LP,NCHAR,NELS
      LOGICAL LMPE
*    Local data :
*    Version :
      CHARACTER*30 VERSION
      PARAMETER (VERSION = 'XRTHK - Version 1.7-0')
*-
      CALL AST_INIT(STATUS)
      CALL MSG_PRNT(VERSION)
*
*     Get directory name from user and display the available observations.
*     Get rootname of file wanted.
      CALL XSORT_FILESELECT(SRT, HEAD, STATUS)
*
      IF (STATUS .NE. SAI__OK) GOTO 999

      CALL CHR_UCASE(HEAD.ORIGIN)
      IF (HEAD.ORIGIN.EQ.'OMD') THEN
*      Read header info from the corresponding .HDR file
         CALL XRT_RDHEAD( .TRUE., SRT, HEAD, VERS, STATUS)
         LMPE = .TRUE.
      ELSE
         CALL RAT_GETXRTHEAD(SRT.ROOTNAME, HEAD, STATUS)
         LMPE = .FALSE.
      ENDIF
*
      IF (STATUS .NE. SAI__OK) GOTO 999
*
* Attempt to open the eventrate file
      CALL RAT_HDLOOKUP(HEAD,'EVRATE','EXTNAME',EXT,STATUS)
      ERFILE = SRT.ROOTNAME(1:CHR_LEN(SRT.ROOTNAME))//EXT
*
      CALL HDS_OPEN(ERFILE, 'READ', ERLOC, STATUS)
*
* If file coundn't be opened set logical false - else set logical
* true and read in the times array
      IF (STATUS .NE. SAI__OK) THEN
*
         CALL MSG_PRNT('Warning: failed to open EVENTRATE file')
         LEVR = .FALSE.
         CALL ERR_ANNUL(STATUS)
*
      ELSE
*
         LEVR = .TRUE.
*
*   Read eventrates times
         CALL RAT_HDLOOKUP(HEAD,'EVRATE','TIME',COL,STATUS)
         CALL DAT_FIND(ERLOC, COL, EVTLOC, STATUS)
         CALL DAT_SIZE(EVTLOC, ENTIM, STATUS)
*
*   Map the time array
         CALL DAT_MAPD(EVTLOC, 'READ', 1, ENTIM, EV_TPNTR, STATUS)
*
         IF (STATUS .NE. SAI__OK) THEN
            CALL MSG_PRNT('Error mapping eventrate TIME array')
            GOTO 999
         ENDIF
*
      ENDIF
*
* Attempt to open the attitude file
      CALL RAT_HDLOOKUP(HEAD,'ASPECT','EXTNAME',EXT,STATUS)
      ATTFIL = SRT.ROOTNAME(1:CHR_LEN(SRT.ROOTNAME))//EXT
*
      CALL HDS_OPEN(ATTFIL, 'READ', ATTLOC, STATUS)
*
* If file coundn't be opened set logical false - else set logical
* true and read in the times array
      IF (STATUS .NE. SAI__OK) THEN
*
         CALL MSG_PRNT('Warning: failed to open ATTITUDE file')
         LATT = .FALSE.
         CALL ERR_ANNUL(STATUS)
*
      ELSE
*
         LATT = .TRUE.
*
*   Read attitude times
         CALL RAT_HDLOOKUP(HEAD,'ASPECT','TIME',COL,STATUS)
         CALL DAT_FIND(ATTLOC, COL, AFTLOC, STATUS)
         CALL DAT_SIZE(AFTLOC, ANTIM, STATUS)
*
*   Map the time array
         CALL DAT_MAPD(AFTLOC, 'READ', 1, ANTIM, AT_TPNTR, STATUS)
*
         IF (STATUS .NE. SAI__OK) THEN
            CALL MSG_PRNT('Error mapping ATTITUDE TIME array')
            GOTO 999
         ENDIF
*
      ENDIF
*
*   Get the minimum interval allowed for a good time window
      CALL USI_GET0R('INTERVAL', INTERVAL, STATUS)
*
      IF (STATUS .NE. SAI__OK) GOTO 999
*
*   Create a dynamic array to hold the TIM_SEL on-off times info.
*   and another two for workspace
      CALL DYN_MAPD(1,MAXRAN*2,OPNTR,STATUS)
      CALL DYN_MAPD(1,MAXRAN*2,WPNTR,STATUS)
      CALL DYN_MAPD(1,MAXRAN*2,W2PNTR,STATUS)
*
      IF (STATUS .NE. SAI__OK) THEN
         CALL MSG_PRNT('Error obtaining dynamic memory')
         GOTO 999
      ENDIF
*
*   Fill the ON-OFF times array from the TIM_SEL values
      CALL XRTHK_FILLTIM(HEAD, %val(OPNTR))
*
*   Calculate the total exposure time in the TIM_SEL structure
      EXPO_TIM=0.0
      DO LP=1,HEAD.NTRANGE
*
         EXPO_TIM = EXPO_TIM + HEAD.TEND(LP) - HEAD.TSTART(LP)
*
      ENDDO
*
D     WRITE(*,*) HEAD.TSTART(1),HEAD.TEND(HEAD.NTRANGE),HEAD.BASE_SCTIME
*   Initialise the BAD and GOOD window counters
      NBAD = 0
      NGOOD = 0
*
*   Give user some info. on the typical parameters to select on
      CALL MSG_PRNT('  ')
      CALL MSG_PRNT('Typical parameters to sort on :')
      CALL MSG_PRNT('  ')
      CALL RAT_HDLOOKUP(HEAD,'EVRATE','MVRATE',COL,STATUS)
      CALL MSG_SETC('MVR',COL)
      CALL MSG_PRNT('   Master veto rate : ^MVR')
      CALL RAT_HDLOOKUP(HEAD,'ASPECT','ASPERR',COL,STATUS)
      CALL MSG_SETC('ASP',COL)
      CALL MSG_PRNT('   Aspect error : ^ASP')
      CALL RAT_HDLOOKUP(HEAD,'EVRATE','XACC',COL,STATUS)
      CALL MSG_SETC('AXE',COL)
      CALL MSG_PRNT('   Accepted event rate : ^AXE')
      CALL MSG_PRNT('  ')
*
*     set the default to "MASTER VETO" else "ASPECT ERROR" (this is
*     arbitrary, but ASPECT ERROR does appear in both PSPC and HRI datasets)
      CALL RAT_HDLOOKUP(HEAD,'EVRATE','MVRATE',COL,STATUS)
      IF (COL.EQ.' ')
     &    CALL RAT_HDLOOKUP(HEAD,'ASPECT','ASPERR',COL,STATUS)
      CALL USI_DEF0C('HKPAR1',COL,STATUS)
*   Loop over parameter selection
      DO PLP = 1,10
*
*      Loop until array is found
         JUMPOUT = .FALSE.
         DO WHILE (.NOT. JUMPOUT)
*
*         Create parameter name
            CALL CHR_ITOC(PLP, CPLP, NCHAR)
            PARAM = 'HKPAR' // CPLP
*
*         Get house keeping parameter name
            CALL USI_GET0C(PARAM(1:5+NCHAR), HKNAME, STATUS)
*
*         Check if PAR_NULL has been entered indicating a break out of the loop
            IF (STATUS .EQ. PAR__NULL) THEN
               CALL ERR_ANNUL(STATUS)
               GOTO 100
            ELSEIF (STATUS .NE. SAI__OK) THEN
               GOTO 999
            ENDIF
*
            CALL USI_CANCL(PARAM(1:5+NCHAR), STATUS)
*
*         Attempt to find SASS selection criteria for this parameter if the
*         QUALITY_limits file has been opened. The quality file seems to be
*         pretty useless in practise - so we'll not bother.
*
*
*         Map the input array - initially try the EVENTRATE file, if this
*         fails try the ATTITUDE file
            CALL CMP_MAPV(ERLOC, HKNAME(1:CHR_LEN(HKNAME)), '_REAL',
     &           'READ', PNTR, NELS, STATUS)
*
            FLOC = ERLOC

            IF (STATUS .NE. SAI__OK) THEN
*
               CALL ERR_ANNUL(STATUS)
*
               CALL CMP_MAPV(ATTLOC, HKNAME(1:CHR_LEN(HKNAME)), '_REAL',
     &                                      'READ', PNTR, NELS, STATUS)
*
               FLOC = ATTLOC
*
*         If still can't map array - output an error message
               IF (STATUS .NE. SAI__OK) THEN
*
                  CALL MSG_SETC('HK', HKNAME)
                  CALL MSG_PRNT('Error: ^HK not found in HK files')
*
*          Annul the status value and try again
                  CALL ERR_ANNUL(STATUS)
*
               ELSE
                  JUMPOUT = .TRUE.
*
*        Set the number of times value
                  IF (NELS .EQ. ANTIM) THEN
                     NTIM = ANTIM
                     TPNTR = AT_TPNTR
                  ELSE
                     CALL MSG_PRNT('*Mismatch between the number of '/
     &                  /'times in the ATTITUDE file and the HK array')
                     STATUS = SAI__ERROR
                     GOTO 999
                  ENDIF
*
               ENDIF
*
            ELSE
               JUMPOUT = .TRUE.
*
*        Set the number of times value
               IF (NELS .EQ. ENTIM) THEN
                  NTIM = ENTIM
                  TPNTR = EV_TPNTR
               ELSE
                  CALL MSG_PRNT('*Mismatch between the number of '/
     &                /'times in the EVENTRATE file and the HK array')
                  STATUS = SAI__ERROR
                  GOTO 999
               ENDIF
*
            ENDIF
         ENDDO
*
*      Find the array range
         CALL ARR_RANG1R(NELS, %val(PNTR), RMIN, RMAX, STATUS)
*
*      Tell user the range
         CALL MSG_SETC('HK', HKNAME(1:CHR_LEN(HKNAME)))
         CALL MSG_SETR('MIN', RMIN)
         CALL MSG_SETR('MAX', RMAX)
         CALL MSG_PRNT('^HK : ranges from ^MIN : ^MAX')
*
*      Define defaults for known parameters
         IF (INDEX(HKNAME, 'ASP_ERR') .NE. 0) THEN
            RMIN = MAX(0.0, RMIN)
            RMAX = 1.0
         ELSEIF (INDEX(HKNAME, 'EE_MV') .NE. 0) THEN
            RMIN = MAX(0.0, RMIN)
            RMAX = 170.0
         ENDIF
*
*      Set up parameter defaults
         PARMIN = 'PMIN' // CPLP
         CALL USI_DEF0R(PARMIN(1:4+NCHAR), RMIN, STATUS)
*
         PARMAX = 'PMAX' // CPLP
         CALL USI_DEF0R(PARMAX(1:4+NCHAR), RMAX, STATUS)
*
*      Ask for minimum and maximum values
         CALL USI_GET0R(PARMIN(1:4+NCHAR), HKMIN, STATUS)
         CALL USI_GET0R(PARMAX(1:4+NCHAR), HKMAX, STATUS)
*
         IF (STATUS .NE. SAI__OK) GOTO 999
*
*      Calculate times when the data is bad. NB: The output
*      array TBAD is updated to take into account overlapping
*      times.
         CALL XRTHK_BADTIMES(HEAD, EXPO_TIM, %val(OPNTR), NTIM,
     &                       %val(TPNTR), %val(PNTR), HKMIN, HKMAX,
     &                     INTERVAL, %val(WPNTR), NBAD, TBAD, STATUS)
*
*      Convert the BAD TIME windows into a set of good times in MJD format
         IF (NBAD .GT. 0) THEN
            CALL XRTHK_GOODTIMES(HEAD, NBAD, TBAD,
     &          %val(WPNTR), %val(W2PNTR), EXPO_TIM, NGOOD, TGOOD)
         ENDIF
*
*      Unmap the array
         CALL CMP_UNMAP(FLOC, HKNAME(1:CHR_LEN(HKNAME)), STATUS)
*
         IF (STATUS .NE. SAI__OK) GOTO 999
*
      ENDDO
*
100   CONTINUE
*
* If no GOOD windows were found - leave the program
      IF (NGOOD .EQ. 0) THEN
         CALL MSG_PRNT('No good windows selected - not producing '/
     &                /'output file')
         GOTO 999
      ENDIF
*
* Merge the output GOOD times with the TIM_SEL values and convert to MJD
* format
      CALL XRTHK_GOODMJD(HEAD, %val(OPNTR), %val(W2PNTR),
     &                               EXPO_TIM, NGOOD, TGOOD)
*
* Get a filename from the parameter system
      CALL USI_GET0C('FNAME', FNAME, STATUS)
*
* Get logical unit for the output times file
      CALL FIO_GUNIT(MUNIT, STATUS)
*
      IF (STATUS .NE. SAI__OK) THEN
         CALL MSG_PRNT('Error getting unit for output file')
         GOTO 999
      ENDIF
*
* Open the output file
      OPEN(UNIT=MUNIT, FILE=FNAME, STATUS='UNKNOWN')
*
* Convert the spacecraft clock times into MJD format and write to a file
      DO LP=1,NGOOD
*
*    Convert start and stop time to an MJD and put an M in front of it.
         WRITE(CSTRING1, FMT='(E23.15)')TGOOD(1 + (LP-1)*2)
         CALL CHR_FANDL(CSTRING1,IND1A,IND1B)
*
         WRITE(CSTRING2, FMT='(E23.15)')TGOOD(LP*2)
         CALL CHR_FANDL(CSTRING2,IND2A,IND2B)
*
*      Write these two MJDs to the output file
         WRITE(MUNIT, 1000)CSTRING1(IND1A:IND1B),
     &                              CSTRING2(IND2A:IND2B)
      ENDDO
*
1000  FORMAT(X,'M',A,4X,'M',A)
*
      CALL FIO_PUNIT(MUNIT, STATUS)
*
      CLOSE(MUNIT)
*
999   CONTINUE
*
* Close the HDS files
      IF (LEVR) CALL HDS_CLOSE(ERLOC, STATUS)
      IF (LATT) CALL HDS_CLOSE(ATTLOC, STATUS)
*
      CALL AST_CLOSE(STATUS)

      END
***********************************************************************
*+XRTHK_BADTIMES  -  calculates the good time windows from HK params.
      SUBROUTINE XRTHK_BADTIMES(HEAD, EXPO_TIM, OARR, NTIM, TIME,
     &                 HKVAL, HKMIN, HKMAX, INTERVAL, TOUT, NBAD,
     &                 TBAD, STATUS)
*    Description :
*     Produces a set of time windows detailing when a housekeeping
*     parameter has gone outside user defined limits. To eradicate
*     short periods of good data being selected between bad time
*     windows a variable INTERVAL is used to define the minimum
*     stretch of GOOD data allowed.
*    History :
*     25-Feb-1992       original   (RDS)
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Structure definition :
      INCLUDE 'XRTLIB(INC_XRTHEAD)'
*    Status :
      INTEGER STATUS
*    Import :
      RECORD /XRT_HEAD/ HEAD                     ! Defines MAXRAN
*
      REAL EXPO_TIM                              ! Total time in TIM_SEL
      DOUBLE PRECISION OARR(MAXRAN*2)            ! ON-OFF TIM_SEL times
      INTEGER NTIM
      DOUBLE PRECISION TIME(NTIM)                ! Times of HK data
      REAL HKVAL(NTIM)                           ! HK data
      REAL HKMIN,HKMAX                           ! Limits for HK data
      REAL INTERVAL                              ! Minimum gap allowed
      DOUBLE PRECISION TOUT(MAXRAN*2)            ! Workspace array
*                                                ! between bad windows
*    Import-Export :
      INTEGER NBAD                               ! No. of bad time windows
      DOUBLE PRECISION TBAD(MAXRAN*2)            ! Bad times (s/c clock units)
*    Export :
*    Local constants :
*     <local constants defined by PARAMETER>
*    Local variables :
      DOUBLE PRECISION START1(MAXRAN),END1(MAXRAN)
      DOUBLE PRECISION OVR                       ! Overlapping time
      DOUBLE PRECISION DMIN,DMAX
      DOUBLE PRECISION LAST                      ! Last time
      REAL PCOUT
      INTEGER NOUT
      INTEGER CNT,LP
      LOGICAL NEW
*-
*
* Check if the HK data is complete
      IF (TIME(1) .GT. (OARR(1) + 10.)) THEN
         CALL MSG_PRNT('** The HK file is incomplete **')
         CALL MSG_SETD('T1',HEAD.TSTART(1)+HEAD.BASE_SCTIME)
         CALL MSG_SETD('T2',HEAD.TEND(HEAD.NTRANGE)+HEAD.BASE_SCTIME)
         CALL MSG_PRNT('   data time range: ^T1 to ^T2 ')
         CALL MSG_SETD('T1',TIME(1))
         CALL MSG_SETD('T2',TIME(NTIM))
         CALL MSG_PRNT('   HK  time  range: ^T1 to ^T2 ')
         CALL MSG_PRNT('** times not '/
     &               /'covered will be INCLUDED in the '/
     &               /'output file **')
      ENDIF
*
* Initialise counts and NEW time window logical
      CNT = 1
      NEW = .TRUE.
*
* Set the last time value
      LAST = HEAD.BASE_SCTIME
*
      DO LP=1,NTIM
*
*   Ensure that the time of this array element is greater than the last
*   time seen - because the cal files are often corrupted. Can't handle
*   none-sequential time files
         IF (TIME(LP) .GE. LAST) THEN
*
            IF (HKVAL(LP) .GE. HKMIN .AND. HKVAL(LP) .LE. HKMAX) THEN
*
               IF (.NOT. NEW) THEN
*
*      Set the end value of the bad window. Make this the last time seen
*      but add a second.
                  END1(CNT) = LAST + 1
                  CNT = CNT + 1
                  NEW = .TRUE.
*
*      Test if CNT is too high
                  IF (CNT .GT. MAXRAN) THEN
                     CALL MSG_PRNT('Error: the maximum number of time '/
     &                  /'windows allowed has been exceeded')
                     CALL MSG_PRNT('Try incresing the INTERVAL value')
                     STATUS = SAI__ERROR
                    GOTO 999
                  ENDIF
*
               ENDIF
*
            ELSEIF (NEW) THEN
*
*       Dont want to make data good for a short period of time - so
*       check that the interval between the last bad period is greater
*       than a certain value.
               IF (CNT .EQ. 1 .OR. TIME(LP) .GE.
     &                          (END1(CNT-1) + INTERVAL)) THEN
*
                  START1(CNT) = TIME(LP) - 1.0
                  NEW = .FALSE.
*
               ELSE
*
*        Otherwise reset the last end value
                  CNT = CNT - 1
                  NEW = .FALSE.
*
               ENDIF
            ENDIF
*
*     Set the last time value
            LAST = TIME(LP)
*
         ENDIF
*
      ENDDO
*
* Set the last value to the largest time in the file if neccessary.
      IF (.NOT. NEW) THEN
         CALL ARR_RANGD(NTIM, TIME, DMIN, DMAX)
         END1(CNT) = DMAX
      ELSE
*
* Decrement counter if windows were completed.
         CNT = CNT - 1
      ENDIF
*
* Check that at least one bad time range was found
      IF (CNT .GE. 1) THEN
*
*    Write start and stop times into output BAD array
         DO LP=1,CNT
            TBAD(1+(LP-1)*2) = START1(LP)
            TBAD(2*LP) = END1(LP)
         ENDDO
*
         NBAD = CNT
*
*    Check that the maximum number of ranges hasn't been exceeded
         IF (NBAD .GT. MAXRAN) THEN
            CALL MSG_PRNT('Error: the maximum number of time windows '/
     &                /'allowed has been exceeded')
            STATUS = SAI__ERROR
            GOTO 999
         ENDIF
*
*   Calculate the overlapping time between this set of BAD windows and the
*   TIM_SEL values. Use NOUT and TOUT as dummy arrays
         CALL XRT_TIMSET(MAXRAN*2, HEAD.NTRANGE*2, OARR, MAXRAN*2,
     &                        NBAD*2, TBAD, MAXRAN*2, NOUT, TOUT, OVR)
*
*   Tell user what percentage of the total exposure time has been
*   ruled out by this selection, accurate to 2dp.
         PCOUT = OVR / EXPO_TIM * 100.0
         PCOUT = NINT(PCOUT*100.0) / 100.
         CALL MSG_SETR('PC', PCOUT)
         CALL MSG_SETI('NUM', CNT)
         CALL MSG_PRNT('^NUM BAD windows - excluding ^PC % of the data')
*
      ELSE
*
         CALL MSG_PRNT('NO bad time windows found')
         NBAD = 0
*
      ENDIF
*
999   CONTINUE
*
      END


***********************************************************************
*+XRTHK_GOODTIMES  -  calculates the good time windows from the bad
      SUBROUTINE XRTHK_GOODTIMES(HEAD, NBAD, TBAD, TFIRST, TOUT,
     &                                     EXPO_TIM, NGOOD, TGOOD)
*    Description :
*     Takes a series of BAD time windows to be excluded in S/C clock
*     time and inverts them to produce a set of GOOD time windows.
*    History :
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Structure definitions :
      INCLUDE 'XRTLIB(INC_XRTHEAD)'
*    Status :
*    Import :
      RECORD /XRT_HEAD/ HEAD                     ! Header records
*
      INTEGER NBAD                               ! No. of bad time windows
      DOUBLE PRECISION TBAD(MAXRAN*2)            ! Bad times (s/c clock units)
      DOUBLE PRECISION TFIRST(MAXRAN*2)          ! Workspace
      DOUBLE PRECISION TOUT(MAXRAN*2)            ! Workspace
      REAL EXPO_TIM                              ! Exposure time
*    Import-Export :
*    Export :
      INTEGER NGOOD                              ! Number of good time windows
      DOUBLE PRECISION TGOOD(MAXRAN*2)           ! Good times (MJD)
*    Local constants :
*     <local constants defined by PARAMETER>
*    Local variables :
      INTEGER BST,GST                            ! Array positions
      INTEGER BEN                                ! Last BAD time to use
      DOUBLE PRECISION SCEND                     ! Final s/c clock time
      DOUBLE PRECISION OVR                       ! Overlap
      INTEGER NOUT
      INTEGER LP
*-
* Convert bad windows to good windows
*   Is the first data section good ?
      IF (TBAD(1) .GT. HEAD.BASE_SCTIME) THEN
*
         TFIRST(1) = HEAD.BASE_SCTIME
         GST = 2
         BST = 1
*
      ELSE
         GST = 1
         BST = 2
      ENDIF
*
*   Is the last data section good ?
      SCEND = (HEAD.END_MJD - HEAD.BASE_MJD) *  86400.0
     &                                   + HEAD.BASE_SCTIME
*
      IF (TBAD(NBAD*2) .GE. SCEND) THEN
         BEN = NBAD*2 - 1
      ELSE
         BEN = NBAD*2
      ENDIF
*
*   Copy the bad window times into the good windows
      DO LP=BST,BEN
*
         TFIRST(GST) = TBAD(LP)
         GST = GST + 1
*
      ENDDO
*
*   Set the last GOOD value if the BAD window doesn't go right to the end
      IF (TBAD(NBAD*2) .LT. SCEND) THEN
         TFIRST(GST) = SCEND
         GST = GST + 1
      ENDIF
*
*   Ensure that there hasn't been a cock-up
      IF (MOD(GST, 2) .EQ. 0) THEN
         CALL MSG_PRNT('Programmer error: refer to author')
      ENDIF
*
*   If this is the first parameter selection simply copy these
*   values into the TGOOD array
      IF (NGOOD .LE. 0) THEN
         NGOOD = (GST - 1) / 2
*
         DO LP=1,NGOOD*2
            TGOOD(LP) = TFIRST(LP)
         ENDDO
*
      ELSE
*
*   Merge these GOOD windows into the large GOOD window array
         CALL XRT_TIMSET(MAXRAN*2, GST-1, TFIRST, MAXRAN*2,
     &              NGOOD*2, TGOOD, MAXRAN*2, NOUT, TOUT, OVR)
*
*   Set the new number of good windows
         NGOOD = NOUT
*
*   Copy the new windows into the old array
         DO LP=1,NGOOD*2
            TGOOD(LP) = TOUT(LP)
         ENDDO
*
      ENDIF
*
      END
***********************************************************************
*+XRTHK_GOODMJD -  converts good times to MJD format
      SUBROUTINE XRTHK_GOODMJD(HEAD, OARR, TOUT, EXPO_TIM,
     &                                          NGOOD, TGOOD)
*    Description :
*     Takes a series of GOOD time windows, merges them with the
*     TIM_SEL values from the file header and converts them to MJD
*     format. Finally it outputs the total amount of time excluded
*    History :
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Structure definitions :
      INCLUDE 'XRTLIB(INC_XRTHEAD)'
*    Status :
*    Import :
      RECORD /XRT_HEAD/ HEAD                     ! Header records
*
      DOUBLE PRECISION OARR(MAXRAN*2)            ! TIM_SEL values
      DOUBLE PRECISION TOUT(MAXRAN*2)            ! TIM_SEL values
      REAL EXPO_TIM                              ! Exposure time
*    Import/Export :
      INTEGER NGOOD                              ! Number of good time windows
      DOUBLE PRECISION TGOOD(MAXRAN*2)           ! Good times (MJD)
*    Local constants :
*     <local constants defined by PARAMETER>
*    Local variables :
      DOUBLE PRECISION OVR                       ! Overlap
      INTEGER NOUT
      INTEGER LP
      REAL XG
      REAL PCOUT
*-
*   Merge the GOOD windows with the TIM_SEL values
      CALL XRT_TIMSET(MAXRAN*2, NGOOD*2, TGOOD, MAXRAN*2,
     &              HEAD.NTRANGE*2, OARR, MAXRAN*2, NOUT, TOUT, OVR)
*
*   Set the number of good windows
      NGOOD = NOUT
*
*   Copy the merged windows into the GOOD array
      DO LP=1,NGOOD*2
         TGOOD(LP) = TOUT(LP)
      ENDDO
*
*   Calculate the new total exposure time
      XG=0.0
      DO LP=1,NGOOD*2,2
         XG = XG + TGOOD(LP+1) - TGOOD(LP)
      ENDDO
*
*   Calculate the percentage of the data excluded to two decimal places
      PCOUT = (EXPO_TIM - XG) / EXPO_TIM * 100.0
      PCOUT = NINT(PCOUT*100.0) / 100.
      CALL MSG_SETR('TOT', XG)
      CALL MSG_SETR('PC', PCOUT)
      CALL MSG_PRNT('Total exposure time now: ^TOT seconds - '/
     &             /'excluded ^PC % of the data')
*
*   Convert values to MJD
      DO LP=1,NGOOD*2
         TGOOD(LP) = HEAD.BASE_MJD + (TGOOD(LP) - HEAD.BASE_SCTIME)
     &                             / 86400.0
      ENDDO
*
      END
***********************************************************************
*+XRTHK_FILLTIM  -  fill ON-OFF array from TIM_SEL values
      SUBROUTINE XRTHK_FILLTIM(HEAD, OARR)
*    Description :
*     Sets an array of s/c clock times derived from the TIM_SEL values
*     in the .HDR file
*    History :
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Structure definitions :
      INCLUDE 'XRTLIB(INC_XRTHEAD)'
*    Status :
*    Import :
      RECORD /XRT_HEAD/ HEAD                     ! Header records
*
*    Import-Export :
*    Export :
      DOUBLE PRECISION OARR(MAXRAN*2)      ! ON-OFF values
*    Local constants :
*     <local constants defined by PARAMETER>
*    Local variables :
      INTEGER LP
*-
      DO LP=1,HEAD.NTRANGE
         OARR(1+(LP-1)*2) = HEAD.TSTART(LP) + HEAD.BASE_SCTIME
         OARR(2*LP) = HEAD.TEND(LP) + HEAD.BASE_SCTIME
      ENDDO
*
      END
