*+  EVSORT - Sorts event dataset by list value
      SUBROUTINE EVSORT( STATUS )
*
*    Description :
*
*     The events in the input dataset are sorted into order by the value
*     of one of the lists.
*
*    Parameters :
*
*     INP = UNIV(R)
*           Input dataset
*     OUT = UNIV(W)
*           Input dataset
*
*    Method :
*
*     All lists in the current object are mapped into virtual memory arrays
*     and these are printed item by item on the output device.
*     Note - uses the DEC FORTRAN '$' facility in format control.
*
*    Deficiencies :
*    Bugs :
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     11 May 93 : Original (DJA)
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
      INTEGER STATUS
*
*    Local constants :
*
      INTEGER                MXLIN              ! Max amount of history text
        PARAMETER            ( MXLIN = 8 )
*
*    Local variables :
*
      CHARACTER*(DAT__SZLOC) ILOC               ! Input dataset
      CHARACTER*(DAT__SZLOC) OLOC               ! Output dataset
      CHARACTER*(DAT__SZLOC) LLOC(LIST__MXNL)   ! List locators
      CHARACTER*(DAT__SZNAM) LNAMES(LIST__MXNL) ! Names of lists
      CHARACTER*(DAT__SZNAM) SLIST              ! Sort list name
      CHARACTER*80           TXT(MXLIN)         ! History text

      INTEGER                I                  ! Loop counters
      INTEGER                LLENGTH            ! List length
      INTEGER                IPTR, OPTR         ! Input and output list data
      INTEGER                IDPTR              ! Sort index
      INTEGER                ISLIST             ! Sort list index
      INTEGER                NLIN               ! Number of text lines used
      INTEGER                NLIST              ! Number of lists in input
      INTEGER                NVAL               ! Number of values mapped

      LOGICAL                INPRIM             ! Is input primitive?
      LOGICAL                ASCEND             ! Sort in ascending order?
*
*    Local constants :
*
      CHARACTER*(30)         VERSION            ! version id
        PARAMETER           (VERSION = 'EVSORT Version 1.7-0')
*-

*    Version anouncement
      CALL MSG_PRNT( VERSION )

*    Initialize
      CALL AST_INIT()

*    Obtain data objects
      CALL USI_ASSOC2( 'INP', 'OUT', 'READ', ILOC, OLOC, INPRIM,
     :                                                  STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

      IF ( INPRIM ) THEN
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'Input is NOT an event dataset', STATUS )
      END IF
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Find all lists in the object
      CALL LIST_FINDALLOK (ILOC, .FALSE., LLOC, LNAMES, NLIST, LLENGTH,
     :                                                         STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Tell user if there aren't any
      IF ( NLIST .EQ. 0 ) THEN
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'There are no lists to print', STATUS )
        GOTO 99
      END IF

*    If RAW_TIMETAG is present offer it as the default
      CALL EVSORT_FIND( NLIST, LNAMES, 'RAW_TIMETAG', I, STATUS )
      IF ( I .GT. 0 ) THEN
        CALL PAR_DEF0C( 'SLIST', 'RAW_TIMETAG', STATUS )
      END IF

*    Get list to sort by
      CALL PAR_GET0C( 'SLIST', SLIST, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Locate list to be sorted
      CALL EVSORT_FIND( NLIST, LNAMES, SLIST, ISLIST, STATUS )
      IF ( ISLIST .EQ. 0 ) THEN
        CALL MSG_SETC( 'SL', SLIST )
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'No such list ^SL present in input', STATUS )
        GOTO 99
      END IF

*    Ascending order?
      CALL PAR_GET0L( 'ASCEND', ASCEND, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Copy input to output
      CALL HDX_COPY( ILOC, OLOC, STATUS )

*    Map input list
      CALL LIST_MAPV( ILOC, SLIST, '_DOUBLE', 'READ', IPTR, NVAL,
     :                                                   STATUS )

*    Create index array
      CALL DYN_MAPI( 1, LLENGTH, IDPTR, STATUS )
      CALL ARR_REG1I( 1, 1, LLENGTH, %VAL(IDPTR), STATUS )

*    Sort the index
      CALL MSG_SETC( 'SL', SLIST )
      CALL MSG_PRNT( 'Sorting by ^SL...' )
      CALL SORT_IDXD( LLENGTH, %VAL(IPTR), ASCEND, %VAL(IDPTR),
     :                                                 STATUS )

*    Unmap sort list
      CALL LIST_UNMAP( ILOC, SLIST, STATUS )

*    Move data using index for each output list
      DO I = 1, NLIST

*      Map input and output list data
        CALL LIST_MAPV( ILOC, LNAMES(I), '_DOUBLE', 'READ', IPTR, NVAL,
     :                                                         STATUS )
        CALL LIST_MAPV( OLOC, LNAMES(I), '_DOUBLE', 'WRITE', OPTR, NVAL,
     :                                                          STATUS )

*      Move the data using the index
        CALL SORT_MVIDXD( LLENGTH, %VAL(IPTR), %VAL(IDPTR), %VAL(OPTR),
     :                                                         STATUS )

*      Unmap the lists
        CALL LIST_UNMAP( ILOC, LNAMES(I), STATUS )
        CALL LIST_UNMAP( OLOC, LNAMES(I), STATUS )
        CALL DAT_ANNUL( LLOC(I), STATUS )

      END DO

*    Free the index array
      CALL DYN_UNMAP( IDPTR, STATUS )

*    Write history
      CALL HIST_ADD( OLOC, VERSION, STATUS )
      TXT(1) = 'Input evds {INP}'
      TXT(2) = 'Sorted by list '//SLIST
      IF ( ASCEND ) THEN
        TXT(3) = '  in ascending order'
      ELSE
        TXT(3) = '  in descending order'
      END IF
      NLIN = MXLIN
      CALL USI_TEXT( 3, TXT, NLIN, STATUS )
      CALL HIST_PTXT( OLOC, NLIN, TXT, STATUS )

*    Exit
 99   CALL AST_CLOSE()
      CALL AST_ERR( STATUS )

      END


*+  EVSORT_FIND - Locates a list name in a list of such names
      SUBROUTINE EVSORT_FIND( N, LNAMES, NAME, I, STATUS )
*
*    Description :
*
*    Method :
*
*    Deficiencies :
*    Bugs :
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     11 May 93 : Original (DJA)
*
*    Type Definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
*
*    Status :
*
      INTEGER STATUS
*
*    Import :
*
      INTEGER                N                  ! Number of names
      CHARACTER*(*)          LNAMES(*)          ! List of names
      CHARACTER*(*)          NAME               ! Name to search for
*
*    Export :
*
      INTEGER                I                  ! Name index
*
*    Functions :
*
      LOGICAL                CHR_SIMLR
*
*    Local variables :
*
      LOGICAL                FOUND              ! Found NAME in LNAMES yet?
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Look through list
      FOUND = .FALSE.
      DO WHILE ( (I.LE.N) .AND. .NOT. FOUND )
        IF ( CHR_SIMLR(LNAMES(I),NAME)  ) THEN
          FOUND = .TRUE.
        ELSE
          I = I + 1
        END IF
      END DO

*    Reset I if not found
      IF ( .NOT. FOUND ) I = 0

      END
