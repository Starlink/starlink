*+STREAMLINE  - Reduces a 1-d file by removing bad quality bins.
      SUBROUTINE STREAMLINE(STATUS)
*    Description :
*         Takes a 1-dimensional datafile and removes bad quality points.
*       The axis in the output file always becomes simple even when the
*       input axis was spaced. The principle use of this routine is
*       to save space in mainly blank time series.
*    Parameters :
*      INPUT       _CHAR          Name of the input file
*      OUTPUT      _CHAR          Name of the output file
*    Method :
*    Deficiencies :
*      Cannot handle data with dimensions higher than one.
*    Bugs :
*    Authors :
*     Richard Saxton
*    History :
*     20-July 1991  Original (LTVAD::RDS)
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PAR_ERR'
*    Status :
      INTEGER STATUS
*    Global variables :
*    Functions :
*    Local Constants :
*    Local variables :
      CHARACTER*30 VERSION                      !Version of the software
      CHARACTER*(DAT__SZLOC) ILOC               !Locator to the input datafile
      CHARACTER*(DAT__SZLOC) OLOC               !Locator to the output datafile
      CHARACTER*(DAT__SZLOC) MLOC               !Locator to the MORE structure
      CHARACTER*(DAT__SZLOC) ALOC               !Locator to the axis structure
      CHARACTER*(DAT__SZTYP) INTYPE             !Type of input datafile
      CHARACTER*80 TITLE                        !Title of file
      CHARACTER*40 UNITS                        !Units of data
      CHARACTER*80 PTEXT(4)                     !History text
      LOGICAL LVAR                              !Is the variance array present
      INTEGER DPNTR,TPNTR,QPNTR,VPNTR           !Pointers to arrays.
      INTEGER NTOT                              !Total no. of data points
      INTEGER NGOOD                             !No. of good quality data points
      INTEGER NLINES                            !No of history list items
*
* Check status :
      IF (STATUS .NE. SAI__OK) RETURN
*
      VERSION='STREAMLINE Version 1.5-1'
      CALL MSG_OUT(' ',VERSION, STATUS)
*
* Initialise the Asterix common blocks.
      CALL AST_INIT
*
* Get input data into an array of GOOD values only.
      CALL TIM_GETDATA(ILOC, NTOT, NGOOD, TPNTR, DPNTR, LVAR,
     &                  VPNTR, STATUS)
*
* Test that bad quality points were found
      IF (NTOT .EQ. NGOOD) THEN
         CALL MSG_PRNT('**No bad quality data points were found**')
         GOTO 999
      ENDIF
*
      CALL DAT_TYPE(ILOC, INTYPE, STATUS)
*
* Create output file
      CALL USI_ASSOCO('OUT', INTYPE, OLOC, STATUS)
*
      IF (STATUS .NE. SAI__OK) GOTO 999
*
* Copy MORE box from old file into new file
      CALL DAT_FIND(ILOC, 'MORE', MLOC, STATUS)

      CALL DAT_COPY(MLOC, OLOC, 'MORE', STATUS)
*
* Copy units and title
      CALL BDA_GETTITLE(ILOC, TITLE, STATUS)
      CALL BDA_PUTTITLE(OLOC, TITLE, STATUS)
*
      CALL BDA_GETUNITS(ILOC, UNITS, STATUS)
      CALL BDA_PUTUNITS(OLOC, UNITS, STATUS)
*
      IF (STATUS .NE. SAI__OK) THEN
         CALL MSG_PRNT('WARNING: Error copying MORE box')
         STATUS = SAI__OK
      ENDIF
*
* Write data array into output file
      CALL HDX_PUTR(OLOC, 'DATA_ARRAY', NGOOD, %val(DPNTR), STATUS)
*
* Write variance into output array if required
      CALL HDX_PUTR(OLOC, 'VARIANCE', NGOOD, %val(VPNTR), STATUS)
*
* Write axis into output file
      CALL BDA_CREAXES(OLOC, 1, STATUS)
      CALL BDA_CREAXVAL(OLOC, 1, .FALSE., NGOOD, STATUS)
*
* Find axis array - and write in primitive values
      CALL BDA_LOCAXVAL(OLOC, 1, ALOC, STATUS)
*
      CALL DAT_PUT1R(ALOC, NGOOD, %val(TPNTR), STATUS)
*
      IF (STATUS .NE. SAI__OK) THEN
         CALL MSG_PRNT('Error writing axis values')
         GOTO 999
      ENDIF
*
* Copy and update history
      CALL HIST_COPY(ILOC, OLOC, STATUS)
*
      CALL HIST_ADD(OLOC, VERSION, STATUS)
*
      CALL USI_NAMEI(NLINES, PTEXT, STATUS)
*
      CALL HIST_PTXT(OLOC, NLINES, PTEXT, STATUS)
*
      IF (STATUS .NE. SAI__OK) THEN
         CALL MSG_PRNT('Error writing history')
         GOTO 999
      ENDIF
*
999   CONTINUE
*
      CALL AST_CLOSE(STATUS)
*
      IF (STATUS .NE. SAI__OK) THEN
         CALL ERR_REP(' ','from STREAMLINE',STATUS)
      ENDIF
*
      END
