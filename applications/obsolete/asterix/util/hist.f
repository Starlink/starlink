*+  HIST - Retrieves processing history
      SUBROUTINE HIST( STATUS )
*
*    Description :
*
*     The HISTORY file is retrieved from the input dataset and its contents
*     displayed.
*
*    Parameters :
*
*     INP = UNIV(R)
*           The datatset whose processing history is to be displayed
*     DEV = CHARACTER(R)
*           Output device
*     LINES _OF_TEXT = INTEGER(R)
*           Maximum number of text lines to be displayed
*
*    Method :
*
*     Searches for HISTORY component in the top level of the given dataset
*     and, if found, indexes through the 'records', reproducing their contents.
*
*    Deficiencies :
*    Bugs :
*    Authors :
*
*     Jim Peden (BHVAD::JCMP)
*     Phil Andrews (PLA)
*     David J. Allan (DJA)
*
*    History :
*
*      4 Jul 84 : Original (BHVAD::JCMP)
*     27 Jan 86 : V0.4-1 ADAM version (BHVAD::JCMP)
*      5 Mar 86 : V0.4-2 Handling of text component & output to LP
*                       (BHVAD::GKS)
*     13 Nov 86 : V0.4-3 Modified to ROSAT standard (BHVAD::JKD)
*      7 Jan 87 : Max text lines increased to 200 and some general tidying
*                       (BHVAD::RJV)
*      1 Aug 88 : V1.0-0 Asterix88 version. General tidying. (DJA)
*     11 Nov 88 : V1.5-0 Altered to us HIST_CMN & new HIST_ routines (PLA)
*     29 Oct 91 : V1.5-1 Incorrect processing of LINES_OF_TEXT corrected (DJA)
*     27 Mar 92 : V1.6-0 Removed most VMS specifics (DJA)
*     28 Apr 92 : V1.6-1 Inserted missing CLOSE(LUN) (DJA)
*     21 Sep 92 : V1.6-2 Renamed to HIST to avoid csh command clash (DJA)
*      7 Jun 93 : V1.7-0 Report value of UPDATE_MODE (DJA)
*      4 May 94 : V1.7-1 Use AIO to perform output (DJA)
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
      INCLUDE 'HIST_CMN'
*
*    Status :
*
      INTEGER                 STATUS
*
*    Local Constants :
*
      INTEGER                 MXTEXT           ! Max lines of text
        PARAMETER             ( MXTEXT = 200 )
*
*    Local variables :
*
      CHARACTER*(DAT__SZLOC)  LOC              ! Dataset locator
      CHARACTER*(DAT__SZLOC)  RECORD           ! Record locator
      CHARACTER*(DAT__SZLOC)  CLOC             ! Component locator
      CHARACTER*(DAT__SZNAM)  NAME             ! Comp. name
      CHARACTER*(DAT__SZTYP)  TYPE             ! Comp. type
      CHARACTER*200           STRING           ! String value
      CHARACTER*80            TEXT(MXTEXT)     ! Text (max MXTEXT lines)

      INTEGER                 NCOMP            ! # record components
      INTEGER                 DIMS(DAT__MXDIM) ! Dimensions of component
      INTEGER                 NDIM             ! # dimensions of component
      INTEGER                 I, J             ! Loop counters
      INTEGER                 MAXLIN           ! Max number of lines of text
                                               ! user wants printed per record
      INTEGER                 NL               ! Do loop variable for txt lines
      INTEGER                 NLINES           ! Number found (if < maxlin)
      INTEGER                 OCH              ! Output channel
      INTEGER                 OUTWIDTH         ! Required for subroutine, not used
      INTEGER                 RECORDS          ! # history records

      LOGICAL                 OK               ! History file OK
      LOGICAL                 PRIM             ! Primitive component
*
*    Version :
*
      CHARACTER*30            VERSION
        PARAMETER             ( VERSION = 'HISTORY Version 1.8-0' )
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Version
      CALL MSG_PRNT( VERSION )

*    Initialize ASTERIX subroutines
      CALL AST_INIT

*    Associate dataset
      CALL USI_ASSOCI( 'INP', 'READ', LOC, PRIM, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Check there is a history structure
      CALL HIST_OK( LOC, OK, STATUS )
      IF ( ( STATUS .NE. SAI__OK ) .OR. .NOT. OK ) THEN
        CALL ERR_REP( ' ', 'No valid history available', STATUS )
        GOTO 99
      END IF

*    Output destination
      CALL AIO_ASSOCO( 'DEV', 'LIST', OCH, OUTWIDTH, STATUS )

*    How much text information ?
      CALL USI_DEF0I( 'LINES', MXTEXT, STATUS )
      CALL USI_GET0I( 'LINES', MAXLIN, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 90
      MAXLIN = MIN( MXTEXT, MAXLIN )
      MAXLIN = MAX( 0, MAXLIN )

*    Print out 'header' info
      IF ( OUTWIDTH .EQ. 132 ) THEN
        CALL AIO_TITLE( OCH, VERSION, STATUS )
      END IF

      CALL USI_NAMEI (NLINES, TEXT, STATUS)
      CALL AIO_WRITE( OCH, 'History of:', STATUS )
      CALL AIO_WRITE( OCH, TEXT(1), STATUS )
      CALL AIO_WRITE( OCH, TEXT(2), STATUS )

      CALL CMP_GET0C( HTBL(REC).HIST_LOC, 'CREATED', STRING, STATUS )
      CALL AIO_WRITE( OCH, 'History file created: '//STRING, STATUS )
      IF ( HTBL(REC).VERB_SET ) THEN
        CALL CMP_GET0C( HTBL(REC).HIST_LOC, 'UPDATE_MODE', STRING,
     :                                                    STATUS )
      ELSE
        STRING = 'NORMAL'
      END IF
      CALL MSG_SETC( 'MODE', STRING )
      CALL AIO_WRITE( OCH, 'History file update mode: ^MODE', STATUS )
      CALL CMP_GET0I( HTBL(REC).HIST_LOC, 'CURRENT_RECORD', RECORDS,
     :                                                      STATUS )
      CALL MSG_SETI( 'NREC', RECORDS )
      CALL AIO_WRITE( OCH, 'History contains ^NREC records', STATUS )

*    Loop over the records, printing out contents of scalar primitive components
      DO I = 1, RECORDS
        IF (STATUS .NE. SAI__OK) GOTO 90

        CALL AIO_WRITE( OCH,
     :   '----------------------------------------'//
     :   '----------------------------------------', STATUS )
        CALL MSG_SETI( 'REC', I )
        CALL AIO_IWRITE( OCH, 33, 'RECORD ^REC', STATUS )
        CALL DAT_CELL( HTBL(REC).REC_LOC, 1, I, RECORD, STATUS )
        CALL DAT_NCOMP( RECORD, NCOMP, STATUS )

        DO J = 1, NCOMP
          CALL DAT_INDEX( RECORD, J, CLOC, STATUS )
          CALL DAT_PRIM( CLOC, PRIM, STATUS )

          IF ( PRIM ) THEN
            CALL DAT_SHAPE( CLOC, DAT__MXDIM, DIMS, NDIM, STATUS )
            CALL DAT_NAME( CLOC, NAME, STATUS )
            CALL DAT_TYPE( CLOC, TYPE, STATUS )

            IF ( NDIM .EQ. 0 ) THEN
              CALL STR_OBVAL( CLOC, STRING, STATUS )
              IF (STATUS .NE. SAI__OK) GOTO 90

              CALL MSG_SETC( 'NAME', NAME )
              CALL MSG_SETC( 'STRING', STRING )
              CALL AIO_IWRITE( OCH, 4, '^NAME:-  ^STRING', STATUS )

            ELSE IF (MAXLIN .GT. 0 .AND. NAME .EQ. 'TEXT') THEN

*            handle text component
              CALL CMP_GET1C( RECORD, 'TEXT', MXTEXT, TEXT, NLINES,
     :                                                     STATUS )

              IF (NLINES .GT. 0) THEN
                CALL AIO_IWRITE( OCH, 4, 'TEXT:-', STATUS )
                DO NL = 1, MIN(MAXLIN,NLINES)
                  CALL AIO_WRITE( OCH, TEXT(NL), STATUS )
                END DO
              END IF
            END IF
          END IF
          CALL DAT_ANNUL( CLOC, STATUS )

        END DO

*      Next record
        CALL AIO_BLNK( OCH, STATUS )
        CALL DAT_ANNUL( RECORD, STATUS )

      END DO

      CALL AIO_WRITE( OCH,
     :   '----------------------------------------'//
     :   '----------------------------------------', STATUS )

*    Clean up
 90   CALL AIO_CANCL( 'DEV', STATUS )

 99   CALL AST_CLOSE()
      CALL AST_ERR( STATUS )

      END
