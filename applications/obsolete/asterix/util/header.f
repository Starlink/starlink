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
*     24 Nov 94 : V1.8-0  Now use USI for user interface (DJA)
*     25 Apr 95 : V1.8-1  Updated data interfaces (DJA)
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
*
*
      INTEGER			CHR_LEN
*
*    Local variables :
*
      CHARACTER*(DAT__SZLOC) ASTLOC             ! Locator to ASTERIX struc
      CHARACTER*(DAT__SZLOC) HLOC               ! Locator to HEADER struc
      CHARACTER*(DAT__SZLOC) PLOC               ! Locator to PROCESSING struc
      CHARACTER              LINE*80
      CHARACTER*200		FILE,PATH

      INTEGER			FID			! Dataset id
      INTEGER			NLEV			! Trace info
      INTEGER                	OCH                	! Output channel
      INTEGER                	WIDTH              	! Output width

      LOGICAL                POK                ! PROCESSING struc OK?
      LOGICAL                PRIM               ! Input primitive?
      LOGICAL 		     EDIT
*
*    Version :
*
      CHARACTER*30           VERSION
        PARAMETER           (VERSION = 'HEADER Version 1.8-1')
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

      CALL MSG_PRNT(VERSION)

      CALL AST_INIT()

*    Edit mode?
      CALL USI_GET0L('EDIT',EDIT,STATUS)

*    Get dataset
      IF (EDIT) THEN
        CALL USI_TASSOCI('INP', '*', 'UPDATE', FID, STATUS )
      ELSE
        CALL USI_TASSOCI('INP', '*', 'READ', FID, STATUS )
      ENDIF

*    Get output channel
      CALL AIO_ASSOCO( 'DEV', 'LIST', OCH, WIDTH, STATUS )

      IF (STATUS .EQ. SAI__OK) THEN
        CALL BDI_PRIM( FID, PRIM, STATUS )
        IF (.NOT. PRIM) THEN

*  do editing if required
          IF (EDIT) THEN
            CALL ADI1_LOCHEAD( FID, .TRUE., HLOC, STATUS )
            CALL HEADER_EDIT(HLOC,STATUS)
          ENDIF

          CALL CHR_FILL ('-', LINE)

          CALL AIO_WRITE( OCH, LINE, STATUS )
          CALL AIO_WRITE( OCH, 'Dataset:-', STATUS )
          CALL AIO_BLNK( OCH, STATUS )
          CALL ADI_FTRACE( FID, NLEV, PATH, FILE, STATUS )
          CALL AIO_IWRITE( OCH, 1, FILE(:CHR_LEN(FILE)), STATUS )
          CALL AIO_BLNK( OCH, STATUS )

          CALL ADI1_LOCHEAD( FID, .FALSE., HLOC, STATUS )

          IF ( STATUS .EQ. SAI__OK ) THEN
            CALL AIO_WRITE( OCH, ' HEADER values:', STATUS )
            CALL AIO_BLNK( OCH, STATUS )
            CALL HEADER_OUT( HLOC, OCH, STATUS )

          ELSE
            CALL ERR_ANNUL( STATUS )
            CALL AIO_WRITE( OCH, 'No HEADER present', STATUS )

          END IF

          CALL ADI1_LOCAST( FID, .FALSE., ASTLOC, STATUS )
          IF ( STATUS .EQ. SAI__OK ) THEN
            CALL DAT_THERE(ASTLOC, 'PROCESSING', POK, STATUS)

            IF ( POK ) THEN
              CALL DAT_FIND (ASTLOC, 'PROCESSING', PLOC, STATUS)
              CALL AIO_BLNK( OCH, STATUS )
              CALL AIO_WRITE( OCH, ' PROCESSING information:', STATUS )
              CALL AIO_BLNK( OCH, STATUS )
              CALL HEADER_OUT( PLOC, OCH, STATUS )
            END IF
          ELSE
            CALL ERR_ANNUL( STATUS )
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
      SUBROUTINE HEADER_OUT( SLOC, OCH, STATUS)
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
      SUBROUTINE HEADER_EDIT( HLOC, STATUS )
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
      CHARACTER*(DAT__SZLOC) HLOC               ! Locator to dataset HEADER
*
*    Status :
*
      INTEGER                STATUS
*
*    Local constants :
*
      INTEGER			NITEM
        PARAMETER		( NITEM = 16 )
*
*    Local variables :
*
      CHARACTER*(DAT__SZLOC) CLOC               ! Locator to HEADER component
      CHARACTER*(DAT__SZNAM)	ITEM
      CHARACTER*(DAT__SZNAM+2)	ITEMS(NITEM)
      CHARACTER*1		TITEM
      CHARACTER*40 CVAL
      DOUBLE PRECISION DVAL
      REAL RVAL
      INTEGER IVAL,I
      LOGICAL THERE
      LOGICAL SET

*  Local data:
      DATA	ITEMS/'C TARGET',	 'C OBSERVER',
     :                'C OBSERVATORY',	 'C INSTRUMENT',
     :                'D AXIS_RA',	 'D AXIS_DEC',
     :                'D FIELD_RA',	 'D FIELD_DEC',
     :                'D POSITION_ANGLE','I EQUINOX',
     :                'C BASE_DATE',	 'I BASE_MJD',
     :                'D BASE_UTC',	 'D BASE_TAI',
     :		      'R OBS_LENGTH',	 'R EXPOSURE_TIME'/
*-

*    Check status
      IF (STATUS .NE. SAI__OK) RETURN

*  Loop over editable items
      DO I = 1, NITEM

        TITEM = ITEMS(I)(1:1)
        ITEM = ITEMS(I)(3:)

*    Does it exist?
        CALL DAT_THERE( HLOC, ITEM, THERE, STATUS )
        IF ( THERE ) THEN
          CALL DAT_FIND( HLOC, ITEM, CLOC,STATUS)
          CALL DAT_STATE( CLOC, SET, STATUS )
          IF ( SET ) THEN
            IF ( TITEM .EQ. 'C' ) THEN
              CALL DAT_GET0C( CLOC, CVAL, STATUS )
              CALL USI_DEF0C( ITEM, CVAL, STATUS )
            ELSE IF ( TITEM .EQ. 'I' ) THEN
              CALL DAT_GET0I( CLOC, IVAL, STATUS )
              CALL USI_DEF0I( ITEM, IVAL, STATUS )
            ELSE IF ( TITEM .EQ. 'R' ) THEN
              CALL DAT_GET0R( CLOC, RVAL, STATUS )
              CALL USI_DEF0R( ITEM, RVAL, STATUS )
            ELSE IF ( TITEM .EQ. 'D' ) THEN
              CALL DAT_GET0D( CLOC, DVAL, STATUS )
              CALL USI_DEF0D( ITEM, DVAL, STATUS )
            END IF
          END IF
          CALL DAT_ANNUL( CLOC, STATUS )
        END IF

*    Get new value
        IF ( TITEM .EQ. 'C' ) CALL USI_GET0C( ITEM, CVAL, STATUS )
        IF ( TITEM .EQ. 'I' ) CALL USI_GET0I( ITEM, IVAL, STATUS )
        IF ( TITEM .EQ. 'R' ) CALL USI_GET0R( ITEM, RVAL, STATUS )
        IF ( TITEM .EQ. 'D' ) CALL USI_GET0D( ITEM, DVAL, STATUS )

*    No user response?
        IF ( STATUS .EQ. PAR__NULL ) THEN
          CALL ERR_ANNUL( STATUS )
        ELSE

*      Create if it doesn't exist
          IF ( .NOT. THERE ) THEN
            IF ( TITEM .EQ. 'C' ) CALL DAT_NEW0C(HLOC,ITEM,LEN(CVAL),
     :                                           STATUS)
            IF ( TITEM .EQ. 'I' ) CALL DAT_NEW0I(HLOC,ITEM,STATUS)
            IF ( TITEM .EQ. 'R' ) CALL DAT_NEW0R(HLOC,ITEM,STATUS)
            IF ( TITEM .EQ. 'D' ) CALL DAT_NEW0D(HLOC,ITEM,STATUS)
          END IF

*      Write the data
          IF ( TITEM .EQ. 'C' ) CALL CMP_PUT0C(HLOC,ITEM,CVAL,STATUS)
          IF ( TITEM .EQ. 'I' ) CALL CMP_PUT0I(HLOC,ITEM,IVAL,STATUS)
          IF ( TITEM .EQ. 'R' ) CALL CMP_PUT0R(HLOC,ITEM,RVAL,STATUS)
          IF ( TITEM .EQ. 'D' ) CALL CMP_PUT0D(HLOC,ITEM,DVAL,STATUS)

        END IF

      END DO

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
