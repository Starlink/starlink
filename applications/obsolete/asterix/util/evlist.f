*+  EVLIST - Displays data values in an event dataset
      SUBROUTINE EVLIST( STATUS )
*    Description :
*     List entry values are printed on a printing device specified by the user.
*    Parameters :
*     INP=UNIV(R)
*           Input dataset
*     SUBSET(2)=INTEGER(R)
*           Subset of items to be printed
*     DEVICE = CHAR(R)
*           Output ascii device
*
*    Method :
*
*     All lists in the current object are mapped into virtual memory arrays
*     and these are printed item by item on the output device.
*
*    Deficiencies :
*    Bugs :
*    Authors :
*
*     Jim Peden (BHVAD::JCMP)
*
*    History :
*
*     21 Sep 83 : Original (BHVAD::JCMP)
*     10 Aug 84 : Accumulation of non-scalar items, and tidied up (BHVAD::JCMP)
*     18 Jan 85 : Version announcement. New output format (BHVAD::JCMP)
*      4 Feb 85 : Revisions to output format (BHVAD::JCMP)
*     17 Dec 85 : Mods to SUBSET parameter (BHVAD::JCMP)
*     27 Jan 86 : V0.4-1 ADAM version (BHVAD::JCMP)
*     12 Feb 86 : V0.4-2 bug fix - fail if string truncation (BHVAD::JCMP)
*     28 Sep 88 : V1.0-1 ASTERIX88 (BHVAD::ADM)
*     30 Nov 88 : V1.5-0 More ASTERIX88 conversion, old LIST_s removed,
*                        USI_ added correct use of HIST_, 1D LIST's only,
*                        much tidying... (PLA)
*      9 Nov 93 : V1.5-1 Increased length of UNITS (DJA)
*      5 May 94 : V1.7-0 Use AIO for i/o (DJA)
*
*    Type Definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PAR_ERR'
      INCLUDE 'LIST_PAR'
*
*    Status :
*
      INTEGER 			STATUS
*
*    Local constants :
*
      CHARACTER*(30)         	VERSION            	! version id
        PARAMETER           	( VERSION = 'EVLIST Version 1.7-0' )
*
*    Local variables :
*
      CHARACTER*14           	C(LIST__MXNL)      	! List values
      CHARACTER*(DAT__SZLOC) 	CELL               	! Locator to list element
      CHARACTER*(DAT__SZLOC) 	DLOC(LIST__MXNL)   	! Locator for all list DATA_ARRAY's
      CHARACTER*(DAT__SZLOC) 	ILOC               	! Object locator
      CHARACTER*(DAT__SZLOC) 	LLOC(LIST__MXNL)   	! Locator for all lists
      CHARACTER*(DAT__SZNAM) 	LNAMES(LIST__MXNL) 	! Names of lists
      CHARACTER*132		OBUF			! Output text buffer
      CHARACTER*(DAT__SZTYP) 	TYPE(LIST__MXNL)   	! Types of lists
      CHARACTER*80           	LUNIT(LIST__MXNL)  	! Data units

      INTEGER                	I, J, K      	      	! Loop counters
      INTEGER                	INC          	      	! Number of lists to display at once
      INTEGER                	LLENGTH      	      	! list length
      INTEGER                	OCH          	      	! Output channel
      INTEGER                	NDISP        	      	! Number of lists to be displayed
      INTEGER                	NLIST        	      	! actual number of lists
      INTEGER                	OUTWIDTH     	      	! Width of output device
      INTEGER                	START        	      	! List to display first
      INTEGER                	SUBSET(2)    	      	! item range

      LOGICAL                	CONTINUE     	      	! Used to control loops
      LOGICAL                	DUOK         	      	! data units present
      LOGICAL                	INPRIM       	      	! Is input ptimative?
*-

*    Version anouncement
      CALL MSG_PRNT( VERSION )

*    Initialize
      CALL AST_INIT()

*    Obtain data object
      CALL USI_ASSOCI ('INP', 'READ',ILOC, INPRIM, STATUS)
      IF ( STATUS .NE. SAI__OK ) GOTO 99

      IF ( INPRIM ) THEN
        CALL MSG_PRNT( 'FATAL ERROR: Input is NOT an event dataset' )
        STATUS = SAI__ERROR
      END IF

*    Check status
      IF (STATUS .NE. SAI__OK) GOTO 99

*    Find all lists in the object
      CALL LIST_FINDALLOK (ILOC, .FALSE., LLOC, LNAMES, NLIST, LLENGTH,
     :                                                           STATUS)

*    Tell user if there aren't any
      IF ( NLIST .EQ. 0 ) THEN
        CALL MSG_PRNT( 'FATAL ERROR: There are no lists to print' )
        STATUS = SAI__ERROR
      END IF

*    Check status
      IF (STATUS .NE. SAI__OK) GOTO 99

*    Set up output channel
      CALL AIO_ASSOCO( 'DEVICE', 'LIST', OCH, OUTWIDTH, STATUS )

*    Tell the user how many items there are
      CALL MSG_SETI ('LEN', LLENGTH)
      CALL MSG_PRNT( 'There are ^LEN events per list' )

*    Find out if a subset is required
      SUBSET(1) = 1
      SUBSET(2) = LLENGTH
      CONTINUE  = .TRUE.

      DO WHILE (CONTINUE)
        CALL PAR_DEF1I( 'SUBSET', 2, SUBSET, STATUS )
        CALL PAR_GET1I( 'SUBSET', 2, SUBSET, I, STATUS )
        CALL PAR_CANCL( 'SUBSET', STATUS )

        IF (STATUS .EQ. SAI__OK) THEN
          IF (I .NE. 2) THEN
            CALL MSG_PRNT( 'ERROR: Only 1 range allowed' )

          ELSE IF(SUBSET(2) .LT. SUBSET(1)) THEN
            CALL MSG_PRNT( 'ERROR: Range must be LOW, HIGH' )

          ELSE
            CONTINUE = .FALSE.
            IF (SUBSET(1) .LT. 1) SUBSET(1) = 1
            IF (SUBSET(2) .GT. LLENGTH) SUBSET(2) = LLENGTH

          END IF
        ELSE IF (STATUS .EQ. PAR__NULL .OR. STATUS .EQ. PAR__ABORT) THEN
          CONTINUE = .FALSE.

        ELSE
          CALL ERR_ANNUL (STATUS)
          CALL MSG_PRNT( 'ERROR: Invalid input - try again' )

        END IF
      END DO

*    Print version and introductory information
      IF (OUTWIDTH .EQ. 132) THEN
        CALL AIO_BLNK( OCH, STATUS )
        CALL AIO_WRITE( OCH, VERSION, STATUS )
        CALL AIO_BLNK( OCH, STATUS )
        INC = 7
      ELSE
        INC = 4
      END IF

*    Get the list types and the item lengths in bytes, and the format control
      DO I = 1, NLIST
        CALL CMP_TYPE (LLOC(I), 'DATA_ARRAY', TYPE(I), STATUS)
        CALL hdx_ok (LLOC(I), 'UNITS', DUOK, STATUS)
        IF (DUOK) THEN
          CALL CMP_GET0C (LLOC(I), 'UNITS', LUNIT(I), STATUS)
        ELSE
          LUNIT (I) = 'Units undefined'
        END IF
        CALL DAT_FIND (LLOC(I), 'DATA_ARRAY', DLOC(I), STATUS)
      END DO
      IF ( STATUS .NE. SAI__OK ) GOTO 99

      START = 1 - INC
      NDISP = 0
      CONTINUE = .TRUE.

      DO WHILE (CONTINUE)
        START = START + INC
        NDISP = NDISP + INC

        IF (NDISP .GE. NLIST) THEN
          NDISP    = NLIST
          CONTINUE = .FALSE.
        END IF

        CALL AIO_BLNK( OCH, STATUS )
        WRITE( OBUF, '(10(''-''),<NDISP-START+1>(17(''-'')))')
        CALL AIO_WRITE( OCH, OBUF, STATUS )
        WRITE( OBUF, '(A1,2X,A7,7A17)') '|', 'Item  |',
     :                     (' '//LNAMES(I)(1:15)//'|', I = START, NDISP)
        CALL AIO_WRITE( OCH, OBUF, STATUS )
        WRITE( OBUF, '(A1,8X,A1,7A17)') '|', '|',
     :                       (' '//TYPE(I)(1:15)//'|', I = START, NDISP)
        CALL AIO_WRITE( OCH, OBUF, STATUS )
        WRITE( OBUF, '(A1,8X,A1,7A17)') '|', '|',
     :                      (' '//LUNIT(I)(1:15)//'|', I = START, NDISP)
        CALL AIO_WRITE( OCH, OBUF, STATUS )
        WRITE( OBUF, '(''|'',8(''-''),''|'',
     :                         <NDISP-START+1>('' '',15(''-''),''|''))')
        CALL AIO_WRITE( OCH, OBUF, STATUS )

*      Print the list values
        DO I = SUBSET (1), SUBSET (2)
          DO J = 1, NLIST
            CALL DAT_CELL( DLOC(J), 1, I, CELL, STATUS )
            CALL DAT_GET0C( CELL, C(J), STATUS )
            CALL DAT_ANNUL( CELL, STATUS )
          END DO

          WRITE( OBUF, '(A1,I7,1X,A1,7A17)') '|', I, '|',
     :                                (' '//C(K)//'|', K = START, NDISP)
          CALL AIO_WRITE( OCH, OBUF, STATUS )

        END DO
        WRITE( OBUF, '(10(''-''),<NDISP-START+1>(17(''-'')))')
        CALL AIO_WRITE( OCH, OBUF, STATUS )

      END DO

*    Close output channel
      CALL AIO_CANCL( 'DEVICE', STATUS )

*    Exit
 99   CALL AST_CLOSE()
      CALL AST_ERR( STATUS )

      END
