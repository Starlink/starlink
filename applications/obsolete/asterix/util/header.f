*+  HEADER - displays header information for dataset
      SUBROUTINE HEADER(STATUS)
*    Description :
*     Takes the contents of the HEADER block and displays it on the terminal
*    Environment parameters :
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*     (BHVAD::RJV)
*
*    History :
*
*     22 May 89 : V1.0-0  Original
*      3 Jul 89 : V1.0-1  Altered to display PROCESSING box too (pla)
*     15 May 90 : V1.1-0  Now uses DEV prompt like every other application
*                         (BHVAD::DJA)
*     11 Feb 92 : V1.1-1  Editing added (RJV)
*     30 Mar 92 : V1.6-0  Use ERR_ANNUL to clear PAR__NULL (DJA)
*      4 May 94 : V1.7-0  Use AIO for output (DJA)
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
*    Function :
*
      INTEGER                CHR_LEN
*
*    Local variables :
*
      CHARACTER*(DAT__SZLOC) ASTLOC             ! Locator to ASTERIX struc
      CHARACTER*(DAT__SZLOC) HLOC               ! Locator to HEADER struc
      CHARACTER*(DAT__SZLOC) LOC                ! Locator to input object
      CHARACTER*(DAT__SZLOC) PLOC               ! Locator to PROCESSING struc
      CHARACTER              LINE*80, NAME*132
      CHARACTER*80           STRING             ! FIX - DELETE WHEN FIX REMOVED

      INTEGER                L
      INTEGER                OCH                ! Output channel
      INTEGER                WIDTH              ! Output width

      LOGICAL                ASTOK              ! ASTERIX struc OK?
      LOGICAL                HOK                ! HEADER struc OK?
      LOGICAL                POK                ! PROCESSING struc OK?
      LOGICAL                PRIM               ! Input primitive?
      LOGICAL 		     EDIT
*
*    Version :
*
      CHARACTER*30           VERSION
        PARAMETER           (VERSION = 'HEADER Version 1.7-0')
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

      CALL MSG_PRNT(VERSION)

      CALL AST_INIT()

*    Edit mode?
      CALL PAR_GET0L('EDIT',EDIT,STATUS)

*    Get dataset
      IF (EDIT) THEN
        CALL USI_ASSOCI ('INP', 'UPDATE', LOC, PRIM, STATUS)
      ELSE
        CALL USI_ASSOCI ('INP', 'READ', LOC, PRIM, STATUS)
      ENDIF

*    Get output channel
      CALL AIO_ASSOCO( 'DEV', 'LIST', OCH, WIDTH, STATUS )

      IF (STATUS .EQ. SAI__OK) THEN
        IF (.NOT. PRIM) THEN

*  do editing if required
          IF (EDIT) THEN
            CALL HEADER_EDIT(LOC,STATUS)
          ENDIF

          CALL CHR_FILL ('-', LINE)

          CALL AIO_WRITE( OCH, LINE, STATUS )
          CALL AIO_WRITE( OCH, 'Dataset:-', STATUS )
          CALL AIO_BLNK( OCH, STATUS )
          CALL STR_OBNAME (LOC,NAME,L,STATUS)
          CALL AIO_IWRITE( OCH, 1, NAME(:L), STATUS )
          CALL AIO_BLNK( OCH, STATUS )

          CALL BDA_CHKHEAD(LOC,HOK,STATUS)

          IF (HOK) THEN
            CALL BDA_LOCHEAD (LOC, HLOC, STATUS)
            CALL AIO_WRITE( OCH, ' HEADER values:', STATUS )
            CALL AIO_BLNK( OCH, STATUS )
            CALL HEADER_OUT( HLOC, OCH, STATUS )

          ELSE
            CALL AIO_WRITE( OCH, 'No HEADER present', STATUS )

          END IF

          CALL BDA_CHKAST( LOC, ASTOK, STATUS )

          IF ( ASTOK ) THEN
            CALL BDA_LOCAST(LOC, ASTLOC,STATUS)

* ******   FIX UNTILL FILTER IS A HEADER COMPONENT  ******
            CALL DAT_THERE (ASTLOC, 'INSTRUMENT', POK, STATUS)     ! FIX
                                                                   ! FIX
            IF (POK) THEN                                          ! FIX
              CALL DAT_FIND (ASTLOC, 'INSTRUMENT', PLOC, STATUS)   ! FIX
              CALL HDX_OK   (PLOC, 'FILTER', POK,        STATUS)   ! FIX
                                                                   ! FIX
              IF (POK) THEN                                        ! FIX
                CALL CMP_GET0C (PLOC, 'FILTER', STRING, STATUS)    ! FIX
                CALL AIO_WRITE( OCH, ' FILTER              '//
     :                               STRING(:CHR_LEN(STRING)), STATUS )
              END IF                                               ! FIX
              CALL DAT_ANNUL (PLOC, STATUS)                        ! FIX
                                                                   ! FIX
            END IF                                                 ! FIX
*  ******  END OF FIX  ******

            CALL DAT_THERE(ASTLOC, 'PROCESSING', POK, STATUS)

            IF ( POK ) THEN
              CALL DAT_FIND (ASTLOC, 'PROCESSING', PLOC, STATUS)
              CALL AIO_BLNK( OCH, STATUS )
              CALL AIO_WRITE( OCH, ' PROCESSING information:', STATUS )
              CALL AIO_BLNK( OCH, STATUS )
              CALL HEADER_OUT( PLOC, OCH, STATUS )
            END IF
          END IF
          CALL AIO_WRITE( OCH, LINE, STATUS )

        ELSE
          CALL MSG_PRNT ('AST_ERR: Input is primitive - no header')

        END IF
      END IF

*    Close output channel
      CALL AIO_CANCL( 'DEV', STATUS )

*    Tidy up
      CALL AST_CLOSE()
      CALL AST_ERR( STATUS )

      END



*+  HEADER_OUT - Output the information
      SUBROUTINE HEADER_OUT (SLOC, OCH, STATUS)
*    Description :
*    Method :
*    Deficiencies :
*     Assumes all primitives will be scalars (at present this is true)
*    Bugs :
*    Authors :
*     Phillip Andrews (pla_ast88@uk.ac.bham.sr.star)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PAR_ERR'
*
*    Import :
*
      CHARACTER*(DAT__SZLOC) SLOC              ! Locator to structure
      INTEGER                OCH               ! Unit for output
*
*    Status :
*
      INTEGER                STATUS
*
*    Function definition :
*
      INTEGER                CHR_LEN
*
*    Local variables :
*
      CHARACTER*(DAT__SZLOC) LOC               ! Locator to component object
      CHARACTER*20           NAME              ! Name of primitive object
      CHARACTER*80           STRING            ! Contents of primitive object

      INTEGER                I                 ! Loop counter
      INTEGER                NCOMP             ! Number of component objects

      LOGICAL                OK
      LOGICAL                PRIM              ! Is component primitive?
*-

*    Check status
      IF (STATUS .NE. SAI__OK) RETURN

      CALL DAT_NCOMP (SLOC, NCOMP, STATUS)

      DO I = 1, NCOMP
        CALL DAT_INDEX (SLOC, I, LOC,   STATUS)
        CALL DAT_PRIM  (LOC, PRIM,      STATUS)
        CALL DAT_NAME  (LOC, NAME,      STATUS)
        CALL HDX_OK    (SLOC, NAME, OK, STATUS)

        IF (PRIM .AND. OK) THEN
          CALL DAT_GET0C (LOC, STRING, STATUS)
          CALL AIO_IWRITE( OCH, 1, NAME//' '//STRING(:CHR_LEN(STRING)),
     :                      STATUS )

        ELSE IF (OK) THEN
          CALL AIO_IWRITE( OCH, 1, NAME, STATUS )
          CALL HEADER_COUT( LOC, OCH, STATUS )

        END IF
        CALL DAT_ANNUL (LOC, STATUS)
      END DO

      IF (STATUS .NE. SAI__OK) THEN
        CALL AST_REXIT( 'HEADER_OUT', STATUS )
      END IF

      END


*+  HEADER_EDIT - edit header
      SUBROUTINE HEADER_EDIT (LOC,STATUS)
*    Description :
*    Method :
*    Authors :
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PAR_ERR'
*
*    Import :
*
      CHARACTER*(DAT__SZLOC) LOC               ! Locator to dataset
*
*    Status :
*
      INTEGER                STATUS
*
*    Local variables :
*
      CHARACTER*(DAT__SZLOC) HLOC               ! Locator to HEADER
      CHARACTER*(DAT__SZLOC) CLOC               ! Locator to HEADER component
      CHARACTER*40 CVAL
      DOUBLE PRECISION DVAL
      REAL RVAL
      INTEGER IVAL
      LOGICAL HOK
      LOGICAL THERE
      LOGICAL SET
*-

*    Check status
      IF (STATUS .NE. SAI__OK) RETURN

      CALL BDA_CHKHEAD(LOC,HOK,STATUS)
      IF (.NOT.HOK) THEN
        CALL BDA_CREHEAD(LOC,STATUS)
      ENDIF
      CALL BDA_LOCHEAD(LOC,HLOC,STATUS)


      CALL DAT_THERE(HLOC,'TARGET',THERE,STATUS)
      IF (THERE) THEN
        CALL DAT_FIND(HLOC,'TARGET',CLOC,STATUS)
        CALL DAT_STATE(CLOC,SET,STATUS)
        IF (SET) THEN
          CALL DAT_GET0C(CLOC,CVAL,STATUS)
          CALL PAR_DEF0C('TARGET',CVAL,STATUS)
        ENDIF
        CALL DAT_ANNUL(CLOC,STATUS)
      ENDIF
      CALL PAR_GET0C('TARGET',CVAL,STATUS)
      IF (STATUS.EQ.PAR__NULL) THEN
        CALL ERR_ANNUL( STATUS )
      ELSE
        IF (.NOT.THERE) THEN
          CALL DAT_NEW0C(HLOC,'TARGET',40,STATUS)
        ENDIF
        CALL CMP_PUT0C(HLOC,'TARGET',CVAL,STATUS)
      ENDIF

      CALL DAT_THERE(HLOC,'OBSERVER',THERE,STATUS)
      IF (THERE) THEN
        CALL DAT_FIND(HLOC,'OBSERVER',CLOC,STATUS)
        CALL DAT_STATE(CLOC,SET,STATUS)
        IF (SET) THEN
          CALL DAT_GET0C(CLOC,CVAL,STATUS)
          CALL PAR_DEF0C('OBSERVER',CVAL,STATUS)
        ENDIF
        CALL DAT_ANNUL(CLOC,STATUS)
      ENDIF
      CALL PAR_GET0C('OBSERVER',CVAL,STATUS)
      IF (STATUS.EQ.PAR__NULL) THEN
        CALL ERR_ANNUL( STATUS )
      ELSE
        IF (.NOT.THERE) THEN
          CALL DAT_NEW0C(HLOC,'OBSERVER',40,STATUS)
        ENDIF
        CALL CMP_PUT0C(HLOC,'OBSERVER',CVAL,STATUS)
      ENDIF

      CALL DAT_THERE(HLOC,'OBSERVATORY',THERE,STATUS)
      IF (THERE) THEN
        CALL DAT_FIND(HLOC,'OBSERVATORY',CLOC,STATUS)
        CALL DAT_STATE(CLOC,SET,STATUS)
        IF (SET) THEN
          CALL DAT_GET0C(CLOC,CVAL,STATUS)
          CALL PAR_DEF0C('OBSERVATORY',CVAL,STATUS)
        ENDIF
        CALL DAT_ANNUL(CLOC,STATUS)
      ENDIF
      CALL PAR_GET0C('OBSERVATORY',CVAL,STATUS)
      IF (STATUS.EQ.PAR__NULL) THEN
        CALL ERR_ANNUL( STATUS )
      ELSE
        IF (.NOT.THERE) THEN
          CALL DAT_NEW0C(HLOC,'OBSERVATORY',40,STATUS)
        ENDIF
        CALL CMP_PUT0C(HLOC,'OBSERVATORY',CVAL,STATUS)
      ENDIF

      CALL DAT_THERE(HLOC,'INSTRUMENT',THERE,STATUS)
      IF (THERE) THEN
        CALL DAT_FIND(HLOC,'INSTRUMENT',CLOC,STATUS)
        CALL DAT_STATE(CLOC,SET,STATUS)
        IF (SET) THEN
          CALL DAT_GET0C(CLOC,CVAL,STATUS)
          CALL PAR_DEF0C('INSTRUMENT',CVAL,STATUS)
        ENDIF
        CALL DAT_ANNUL(CLOC,STATUS)
      ENDIF
      CALL PAR_GET0C('INSTRUMENT',CVAL,STATUS)
      IF (STATUS.EQ.PAR__NULL) THEN
        CALL ERR_ANNUL( STATUS )
      ELSE
        IF (.NOT.THERE) THEN
          CALL DAT_NEW0C(HLOC,'INSTRUMENT',40,STATUS)
        ENDIF
        CALL CMP_PUT0C(HLOC,'INSTRUMENT',CVAL,STATUS)
      ENDIF

      CALL DAT_THERE(HLOC,'AXIS_RA',THERE,STATUS)
      IF (THERE) THEN
        CALL DAT_FIND(HLOC,'AXIS_RA',CLOC,STATUS)
        CALL DAT_STATE(CLOC,SET,STATUS)
        IF (SET) THEN
          CALL DAT_GET0D(CLOC,DVAL,STATUS)
          CALL PAR_DEF0D('AXIS_RA',DVAL,STATUS)
        ENDIF
        CALL DAT_ANNUL(CLOC,STATUS)
      ENDIF
      CALL PAR_GET0D('AXIS_RA',DVAL,STATUS)
      IF (STATUS.EQ.PAR__NULL) THEN
        CALL ERR_ANNUL( STATUS )
      ELSE
        IF (.NOT.THERE) THEN
          CALL DAT_NEW0D(HLOC,'AXIS_RA',STATUS)
        ENDIF
        CALL CMP_PUT0D(HLOC,'AXIS_RA',DVAL,STATUS)
      ENDIF

      CALL DAT_THERE(HLOC,'AXIS_DEC',THERE,STATUS)
      IF (THERE) THEN
        CALL DAT_FIND(HLOC,'AXIS_DEC',CLOC,STATUS)
        CALL DAT_STATE(CLOC,SET,STATUS)
        IF (SET) THEN
          CALL DAT_GET0D(CLOC,DVAL,STATUS)
          CALL PAR_DEF0D('AXIS_DEC',DVAL,STATUS)
        ENDIF
        CALL DAT_ANNUL(CLOC,STATUS)
      ENDIF
      CALL PAR_GET0D('AXIS_DEC',DVAL,STATUS)
      IF (STATUS.EQ.PAR__NULL) THEN
        CALL ERR_ANNUL( STATUS )
      ELSE
        IF (.NOT.THERE) THEN
          CALL DAT_NEW0D(HLOC,'AXIS_DEC',STATUS)
        ENDIF
        CALL CMP_PUT0D(HLOC,'AXIS_DEC',DVAL,STATUS)
      ENDIF

      CALL DAT_THERE(HLOC,'FIELD_RA',THERE,STATUS)
      IF (THERE) THEN
        CALL DAT_FIND(HLOC,'FIELD_RA',CLOC,STATUS)
        CALL DAT_STATE(CLOC,SET,STATUS)
        IF (SET) THEN
          CALL DAT_GET0D(CLOC,DVAL,STATUS)
          CALL PAR_DEF0D('FIELD_RA',DVAL,STATUS)
        ENDIF
        CALL DAT_ANNUL(CLOC,STATUS)
      ENDIF
      CALL PAR_GET0D('FIELD_RA',DVAL,STATUS)
      IF (STATUS.EQ.PAR__NULL) THEN
        CALL ERR_ANNUL( STATUS )
      ELSE
        IF (.NOT.THERE) THEN
          CALL DAT_NEW0D(HLOC,'FIELD_RA',STATUS)
        ENDIF
        CALL CMP_PUT0D(HLOC,'FIELD_RA',DVAL,STATUS)
      ENDIF

      CALL DAT_THERE(HLOC,'FIELD_DEC',THERE,STATUS)
      IF (THERE) THEN
        CALL DAT_FIND(HLOC,'FIELD_DEC',CLOC,STATUS)
        CALL DAT_STATE(CLOC,SET,STATUS)
        IF (SET) THEN
          CALL DAT_GET0D(CLOC,DVAL,STATUS)
          CALL PAR_DEF0D('FIELD_DEC',DVAL,STATUS)
        ENDIF
        CALL DAT_ANNUL(CLOC,STATUS)
      ENDIF
      CALL PAR_GET0D('FIELD_DEC',DVAL,STATUS)
      IF (STATUS.EQ.PAR__NULL) THEN
        CALL ERR_ANNUL( STATUS )
      ELSE
        IF (.NOT.THERE) THEN
          CALL DAT_NEW0D(HLOC,'FIELD_DEC',STATUS)
        ENDIF
        CALL CMP_PUT0D(HLOC,'FIELD_DEC',DVAL,STATUS)
      ENDIF

      CALL DAT_THERE(HLOC,'POSITION_ANGLE',THERE,STATUS)
      IF (THERE) THEN
        CALL DAT_FIND(HLOC,'POSITION_ANGLE',CLOC,STATUS)
        CALL DAT_STATE(CLOC,SET,STATUS)
        IF (SET) THEN
          CALL DAT_GET0D(CLOC,DVAL,STATUS)
          CALL PAR_DEF0D('POSITION_ANGLE',DVAL,STATUS)
        ENDIF
        CALL DAT_ANNUL(CLOC,STATUS)
      ENDIF
      CALL PAR_GET0D('POSITION_ANGLE',DVAL,STATUS)
      IF (STATUS.EQ.PAR__NULL) THEN
        CALL ERR_ANNUL( STATUS )
      ELSE
        IF (.NOT.THERE) THEN
          CALL DAT_NEW0D(HLOC,'POSITION_ANGLE',STATUS)
        ENDIF
        CALL CMP_PUT0D(HLOC,'POSITION_ANGLE',DVAL,STATUS)
      ENDIF

      CALL DAT_THERE(HLOC,'EQUINOX',THERE,STATUS)
      IF (THERE) THEN
        CALL DAT_FIND(HLOC,'EQUINOX',CLOC,STATUS)
        CALL DAT_STATE(CLOC,SET,STATUS)
        IF (SET) THEN
          CALL DAT_GET0I(CLOC,IVAL,STATUS)
          CALL PAR_DEF0I('EQUINOX',IVAL,STATUS)
        ENDIF
        CALL DAT_ANNUL(CLOC,STATUS)
      ENDIF
      CALL PAR_GET0I('EQUINOX',IVAL,STATUS)
      IF (STATUS.EQ.PAR__NULL) THEN
        CALL ERR_ANNUL( STATUS )
      ELSE
        IF (.NOT.THERE) THEN
          CALL DAT_NEW0I(HLOC,'EQUINOX',STATUS)
        ENDIF
        CALL CMP_PUT0I(HLOC,'EQUINOX',IVAL,STATUS)
      ENDIF

      CALL DAT_THERE(HLOC,'BASE_DATE',THERE,STATUS)
      IF (THERE) THEN
        CALL DAT_FIND(HLOC,'BASE_DATE',CLOC,STATUS)
        CALL DAT_STATE(CLOC,SET,STATUS)
        IF (SET) THEN
          CALL DAT_GET0C(CLOC,CVAL,STATUS)
          CALL PAR_DEF0C('BASE_DATE',CVAL,STATUS)
        ENDIF
        CALL DAT_ANNUL(CLOC,STATUS)
      ENDIF
      CALL PAR_GET0C('BASE_DATE',CVAL,STATUS)
      IF (STATUS.EQ.PAR__NULL) THEN
        CALL ERR_ANNUL( STATUS )
      ELSE
        IF (.NOT.THERE) THEN
          CALL DAT_NEW0C(HLOC,'BASE_DATE',11,STATUS)
        ENDIF
        CALL CMP_PUT0C(HLOC,'BASE_DATE',CVAL,STATUS)
      ENDIF

      CALL DAT_THERE(HLOC,'BASE_MJD',THERE,STATUS)
      IF (THERE) THEN
        CALL DAT_FIND(HLOC,'BASE_MJD',CLOC,STATUS)
        CALL DAT_STATE(CLOC,SET,STATUS)
        IF (SET) THEN
          CALL DAT_GET0I(CLOC,IVAL,STATUS)
          CALL PAR_DEF0I('BASE_MJD',IVAL,STATUS)
        ENDIF
        CALL DAT_ANNUL(CLOC,STATUS)
      ENDIF
      CALL PAR_GET0I('BASE_MJD',IVAL,STATUS)
      IF (STATUS.EQ.PAR__NULL) THEN
        CALL ERR_ANNUL( STATUS )
      ELSE
        IF (.NOT.THERE) THEN
          CALL DAT_NEW0I(HLOC,'BASE_MJD',STATUS)
        ENDIF
        CALL CMP_PUT0I(HLOC,'BASE_MJD',IVAL,STATUS)
      ENDIF

      CALL DAT_THERE(HLOC,'BASE_UTC',THERE,STATUS)
      IF (THERE) THEN
        CALL DAT_FIND(HLOC,'BASE_UTC',CLOC,STATUS)
        CALL DAT_STATE(CLOC,SET,STATUS)
        IF (SET) THEN
          CALL DAT_GET0D(CLOC,DVAL,STATUS)
          CALL PAR_DEF0D('BASE_UTC',DVAL,STATUS)
        ENDIF
        CALL DAT_ANNUL(CLOC,STATUS)
      ENDIF
      CALL PAR_GET0D('BASE_UTC',DVAL,STATUS)
      IF (STATUS.EQ.PAR__NULL) THEN
        CALL ERR_ANNUL( STATUS )
      ELSE
        IF (.NOT.THERE) THEN
          CALL DAT_NEW0D(HLOC,'BASE_UTC',STATUS)
        ENDIF
        CALL CMP_PUT0D(HLOC,'BASE_UTC',DVAL,STATUS)
      ENDIF

      CALL DAT_THERE(HLOC,'BASE_TAI',THERE,STATUS)
      IF (THERE) THEN
        CALL DAT_FIND(HLOC,'BASE_TAI',CLOC,STATUS)
        CALL DAT_STATE(CLOC,SET,STATUS)
        IF (SET) THEN
          CALL DAT_GET0D(CLOC,DVAL,STATUS)
          CALL PAR_DEF0D('BASE_TAI',DVAL,STATUS)
        ENDIF
        CALL DAT_ANNUL(CLOC,STATUS)
      ENDIF
      CALL PAR_GET0D('BASE_TAI',DVAL,STATUS)
      IF (STATUS.EQ.PAR__NULL) THEN
        CALL ERR_ANNUL( STATUS )
      ELSE
        IF (.NOT.THERE) THEN
          CALL DAT_NEW0D(HLOC,'BASE_TAI',STATUS)
        ENDIF
        CALL CMP_PUT0D(HLOC,'BASE_TAI',DVAL,STATUS)
      ENDIF

      CALL DAT_THERE(HLOC,'OBS_LENGTH',THERE,STATUS)
      IF (THERE) THEN
        CALL DAT_FIND(HLOC,'OBS_LENGTH',CLOC,STATUS)
        CALL DAT_STATE(CLOC,SET,STATUS)
        IF (SET) THEN
          CALL DAT_GET0R(CLOC,RVAL,STATUS)
          CALL PAR_DEF0R('OBS_LENGTH',RVAL,STATUS)
        ENDIF
        CALL DAT_ANNUL(CLOC,STATUS)
      ENDIF
      CALL PAR_GET0R('OBS_LENGTH',RVAL,STATUS)
      IF (STATUS.EQ.PAR__NULL) THEN
        CALL ERR_ANNUL( STATUS )
      ELSE
        IF (.NOT.THERE) THEN
          CALL DAT_NEW0R(HLOC,'OBS_LENGTH',STATUS)
        ENDIF
        CALL CMP_PUT0R(HLOC,'OBS_LENGTH',RVAL,STATUS)
      ENDIF

      CALL DAT_THERE(HLOC,'EXPOSURE_TIME',THERE,STATUS)
      IF (THERE) THEN
        CALL DAT_FIND(HLOC,'EXPOSURE_TIME',CLOC,STATUS)
        CALL DAT_STATE(CLOC,SET,STATUS)
        IF (SET) THEN
          CALL DAT_GET0R(CLOC,RVAL,STATUS)
          CALL PAR_DEF0R('EXPOSURE_TIME',RVAL,STATUS)
        ENDIF
        CALL DAT_ANNUL(CLOC,STATUS)
      ENDIF
      CALL PAR_GET0R('EXPOSURE_TIME',RVAL,STATUS)
      IF (STATUS.EQ.PAR__NULL) THEN
        CALL ERR_ANNUL( STATUS )
      ELSE
        IF (.NOT.THERE) THEN
          CALL DAT_NEW0R(HLOC,'EXPOSURE_TIME',STATUS)
        ENDIF
        CALL CMP_PUT0R(HLOC,'EXPOSURE_TIME',RVAL,STATUS)
      ENDIF

      IF (STATUS .NE. SAI__OK) THEN
        CALL AST_REXIT( 'HEADER_EDIT', STATUS )
      ENDIF

      END


*+  HEADER_COUT - Output component of the PROCESSING information
      SUBROUTINE HEADER_COUT (SLOC, OCH, STATUS)
*    Description :
*    Method :
*    Deficiencies :
*     Assumes all primitives will be scalars (at present this is true)
*     Assumes no more structures (at present this is true)
*    Bugs :
*    Authors :
*     Phillip Andrews (pla_ast88@uk.ac.bham.sr.star)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PAR_ERR'
*
*    Import :
*
      CHARACTER*(DAT__SZLOC) SLOC              ! Locator to structure
      INTEGER                OCH               ! Output channel
*
*    Status :
*
      INTEGER                STATUS
*
*    Function definition :
*
      INTEGER                CHR_LEN
*
*    Local variables :
*
      CHARACTER*(DAT__SZLOC) LOC               ! Locator to component object
      CHARACTER*20           NAME              ! Name of primitive object
      CHARACTER*30           STRING            ! Contents of primitive object

      INTEGER                I                 ! Loop counter
      INTEGER                NCOMP             ! Number of component objects

      LOGICAL                OK
      LOGICAL                PRIM              ! Is component primitive?
*-

*    Check status
      IF (STATUS .NE. SAI__OK) RETURN

      CALL DAT_NCOMP (SLOC, NCOMP, STATUS)

      DO I = 1, NCOMP
        CALL DAT_INDEX (SLOC, I, LOC,   STATUS)
        CALL DAT_PRIM  (LOC, PRIM,      STATUS)
        CALL DAT_NAME  (LOC, NAME,      STATUS)
        CALL HDX_OK    (SLOC, NAME, OK, STATUS)

        IF ( PRIM .AND. OK ) THEN
          CALL DAT_GET0C( LOC, STRING, STATUS )
          CALL AIO_IWRITE( OCH, 3, NAME//STRING(:CHR_LEN(STRING)),
     :                     STATUS )

        ELSE IF (OK) THEN
          CALL AIO_IWRITE( OCH, 3, NAME, STATUS )
          CALL AIO_IWRITE( OCH, 5, 'Unable to display non-standard '//
     :                                           'component', STATUS )
        END IF
        CALL DAT_ANNUL( LOC, STATUS )

      END DO

      IF (STATUS .NE. SAI__OK) THEN
        CALL AST_REXIT( 'HEADER_COUT', STATUS )
      END IF

      END
