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
      INCLUDE 'PAR_ERR'
*    Status :
      INTEGER STATUS
*    Local variables :
      CHARACTER*80 PTEXT(4)                     !History text
      LOGICAL LVAR                              !Is the variance array present
      INTEGER			IFID, OFID
      INTEGER DPNTR,TPNTR,VPNTR           !Pointers to arrays.
      INTEGER NTOT                              !Total no. of data points
      INTEGER NGOOD                             !No. of good quality data points
      INTEGER NLINES                            !No of history list items
      INTEGER			ODPTR,OVPTR,OAPTR	! Output arrays
*
*  Version:
*
      CHARACTER*30		VERSION
        PARAMETER	 	( VERSION = 'STREAMLINE Version 1.8-0')
*-

*  Check status :
      IF (STATUS .NE. SAI__OK) RETURN

*  Version
      CALL MSG_PRNT( VERSION )

*  Initialise ASTERIX
      CALL AST_INIT()

*  Get input data into an array of GOOD values only.
      CALL TIM_GETDAT( IFID, NTOT, NGOOD, TPNTR, DPNTR, LVAR,
     :                  VPNTR, STATUS)

*  Test that bad quality points were found
      IF (NTOT .EQ. NGOOD) THEN
         CALL MSG_PRNT('**No bad quality data points were found**')
         GOTO 99
      ENDIF

*  Create output file
      CALL USI_TASSOCO('OUT', 'BINDS', OFID, STATUS)
      IF (STATUS .NE. SAI__OK) GOTO 99

*  Propogate from input...
      CALL BDI_COPMORE( IFID, OFID, STATUS )
      CALL BDI_COPTEXT( IFID, OFID, STATUS )

*  Write data array into output file
      CALL BDI_CREDATA( OFID, 1, NGOOD, STATUS )
      CALL BDA_MAPDATA( OFID, 'WRITE', ODPTR, STATUS )
      CALL ARR_COP1R( NGOOD, %VAL(DPNTR), %VAL(ODPTR), STATUS )

*  Write variance into output array if required
      CALL BDI_CREVAR( OFID, 1, NGOOD, STATUS )
      CALL BDA_MAPVAR( OFID, 'WRITE', OVPTR, STATUS )
      CALL ARR_COP1R( NGOOD, %VAL(VPNTR), %VAL(OVPTR), STATUS )

*  Write axis into output file
      CALL BDI_CREAXES( OFID, 1, STATUS )
      CALL BDI_CREAXVAL( OFID, 1, .FALSE., NGOOD, STATUS )
      CALL BDI_MAPAXVAL( OFID, 'WRITE', 1, OAPTR, STATUS )
      CALL ARR_COP1R( NGOOD, %VAL(TPNTR), %VAL(OAPTR), STATUS )

*  Copy and update history
      CALL HSI_COPY( IFID, OFID, STATUS )
      CALL HSI_ADD( OFID, VERSION, STATUS )
      CALL USI_NAMEI( NLINES, PTEXT, STATUS )
      CALL HSI_PTXT( OFID, NLINES, PTEXT, STATUS )

*  Tidy up
 99   CALL AST_CLOSE()
      CALL AST_ERR( STATUS )

      END
