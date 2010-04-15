*+  RED4_POKE_FITS - Poke a specified FITS item with a given value.
      SUBROUTINE RED4_POKE_FITS( STATUS )
*    Description :
*     This routine read, writes or updates a given value to a specified
*     FITS item in a data structure. If the FITS item does not already
*     exist it is created.
*     The routine is used for peeking and poking the contents of the FITS
*     structure so that the data reduction system may be tested.
*    Invocation :
*     CALL RED4_POKE_FITS( STATUS )
*    Parameters :
*     STATUS    = INTEGER( UPDATE )
*           Global status. This must be ADAM__OK on entry.
*           If this routine completes successfully, the STATUS
*           will be ADAM__OK on exit. Any other value indicates
*           an error.
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*     S M Beard  (UK.AC.ROE.STAR::SMB)
*     P  N Daly  (PND @ JACH.HAWAII.EDU)
*    History :
*     16-Nov-1990: Original version.                           (SMB)
*      1-Jun-1992: Add a method READ, WRITE or UPDATE          (PND)
*      3-Jun-1992: Write out FITS item to GLOBAL.FITS_ITEM     (PND)
*     30-Jun-1992: Write out EXISTS if file exists             (PND)
*      1-Jul-1992: Add MODE parameter and file search          (PND)
*     22-Feb-1993: Conform to error strategy                   (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'ADAMDEFNS'
      INCLUDE 'ADAMERRS'
      INCLUDE 'SAI_ERR'
*    Status :
      INTEGER
     :  STATUS,              ! Global status
     :  DSA_STATUS           ! Global DSA status
*    Global variables :
      INCLUDE 'RED4_COMMON.INC'
*    External references :
      INTEGER CHR_LEN        ! Character length determining function.
*    Local constants :
      INTEGER DSA__OK
      PARAMETER ( DSA__OK=0 )
*    Local variables :
      CHARACTER*80
     :  DATA,                ! Name of the data structure to be modified.
     :  COMMENT              ! Comment for FITS item
      CHARACTER*8
     :  ITEM,                ! Name of the FITS item to be poked.
     :  MODE                 ! Mode for reporting results
      CHARACTER*6
     :  METHOD               ! Method READ, WRITE or UPDATE
      CHARACTER*4
     :  ACCESS               ! Access type for FITS item.
      INTEGER
     :  ELEMENTS,            ! Number of elements in FITS item
     :  STRLEN,              ! Length of character FITS item
     :  COMLEN,              ! Length of comment string
     :  CLEN                 ! Length of character string
      LOGICAL
     :  EXIST                ! Flag indicating if FITS item exists.
      CHARACTER*80
     :  CVALUE               ! Character value for FITS item
      INTEGER
     :  CVALEN               ! Length of character value for FITS item
      INTEGER
     :  IVALUE               ! Integer value for FITS item
      REAL
     :  RVALUE               ! Real value for FITS item
*-

*   Check for error on entry
      IF ( STATUS .NE. ADAM__OK ) RETURN

*   Initialize the output
      CALL PAR_DEF0C( 'FITS_VALUE', ' ', STATUS )
      CALL PAR_PUT0C( 'FITS_VALUE', ' ', STATUS )
      CALL PAR_DEF0C( 'FITS_ITEM', ' ', STATUS )
      CALL PAR_PUT0C( 'FITS_ITEM', ' ', STATUS )

*   Obtain the name of the data structure to be modified
      CALL PAR_GET0C( 'DATA', DATA, STATUS )
      CALL RED4_CHECK_INPUT( DATA, STATUS )

*   Check to see if file exists
      EXIST = .FALSE.
*     CALL DSA_SEEK_NAMED_STRUCTURE( DATA, EXIST, STATUS )
*     IF ( EXIST ) THEN

        CALL PAR_DEF0C( 'FITS_FILE', 'EXISTS', STATUS )
        CALL PAR_PUT0C( 'FITS_FILE', 'EXISTS', STATUS )
*     ELSE

*       CALL PAR_DEF0C( 'FITS_FILE', ' ', STATUS )
*       CALL PAR_PUT0C( 'FITS_FILE', ' ', STATUS )
*       STATUS = SAI__ERROR
*       CALL MSG_SETC( 'DATA', DATA )
*       CALL ERR_REP( ' ', 'RED4_POKE_FITS: '/
*    :    /'File ^DATA does not exist', STATUS )
*     ENDIF

*   Obtain the name of the FITS item and write it to GLOBAL.FITS_ITEM
      CALL PAR_GET0C( 'ITEM', ITEM, STATUS )
      CALL CHR_UCASE( ITEM )
      CALL PAR_DEF0C( 'FITS_ITEM', ITEM, STATUS )
      CALL PAR_PUT0C( 'FITS_ITEM', ITEM, STATUS )

*   Obtain the method
      CALL PAR_GET0C( 'METHOD', METHOD, STATUS )
      CALL CHR_UCASE( METHOD )

*   Obtain the meode
      CALL PAR_GET0C( 'MODE', MODE, STATUS )
      CALL CHR_UCASE( MODE )

*   Check this has worked
      IF ( STATUS .EQ. ADAM__OK ) THEN

*      Open DSA
         CALL DSA_OPEN( DSA_STATUS )

*      Open the specified data structure.
         CALL DSA_NAMED_INPUT( 'DATA', DATA, DSA_STATUS )

*      Determine if the FITS item exists, and obtain its type.
         CALL DSA_SEEK_FITS( 'DATA', ITEM, EXIST, ACCESS,
     :                       ELEMENTS, STRLEN, DSA_STATUS )

*      Check this has worked.
         IF ( STATUS .EQ. ADAM__OK ) THEN

*         What follows next depends on whether the FITS item already exists.
            IF ( EXIST ) THEN

*            The item exists - announce the fact.
               IF ( MODE(1:7) .EQ. 'VERBOSE')  THEN
                 CALL MSG_SETC( 'ACCESS', ACCESS )
                 CALL MSG_SETC( 'ITEM', ITEM )
                 CALL MSG_OUT( ' ', 'Item ^ITEM exists and is '/
     :             /'of type ^ACCESS', STATUS )
               ENDIF

*            Get its current value and obtain a new value, using
*            the current one as a default. Then write the new value
*            back to the FITS structure. This all depends on what
*            type the FITS item is.
               IF ( ACCESS(1:1) .EQ. 'C' ) THEN

*               Character type.
                  CALL DSA_GET_FITS_C( 'DATA', ITEM, 0, CVALUE,
     :              COMMENT, DSA_STATUS )
                  CVALEN = CHR_LEN( CVALUE )

*               Tell the user the value obtained
                  IF ( MODE(1:7) .EQ. 'VERBOSE') THEN
                    CALL MSG_SETC( 'CVALUE', CVALUE )
                    CALL MSG_OUT( ' ', 'Current value is '/
     :                /'^CVALUE', STATUS )
                  ENDIF

*               Write the value to GLOBAL.FITS_VALUE
                  CALL PAR_DEF0C( 'FITS_VALUE',
     :              CVALUE(1:CVALEN), STATUS )
                  CALL PAR_PUT0C( 'FITS_VALUE',
     :              CVALUE(1:CVALEN), STATUS )

                  IF ( METHOD(1:4) .NE. 'READ' ) THEN

                     CLEN = MAX( 1, CHR_LEN( CVALUE ) )
                     CALL PAR_DEF0C( 'CVALUE',
     :                 CVALUE(1:CLEN), STATUS )
                     CALL PAR_GET0C( 'CVALUE',
     :                 CVALUE(1:CLEN), STATUS )

                     CLEN = MAX( 1, CHR_LEN( CVALUE ) )
                     COMLEN = MAX( 1, CHR_LEN( COMMENT ) )
                     CALL DSA_PUT_FITS_C( 'DATA', ITEM, CVALUE(1:CLEN),
     :                 COMMENT(1:COMLEN), DSA_STATUS )
                  ENDIF

               ELSE IF ( ( ACCESS(1:1) .EQ. 'I' )  .OR.
     :                   ( ACCESS(1:1) .EQ. 'S' )  .OR.
     :                   ( ACCESS(1:1) .EQ. 'L' )) THEN

*               Integer or logical type. (Logical values are written
*               as integers).
                  CALL DSA_GET_FITS_I( 'DATA', ITEM, 0, IVALUE,
     :              COMMENT, DSA_STATUS )

                  IF ( MODE(1:7) .EQ. 'VERBOSE' ) THEN
                    CALL MSG_SETI( 'IVALUE', IVALUE )
                    CALL MSG_OUT( ' ', 'RED4_POKE_FITS: '/
     :                /'Current value is ^IVALUE', STATUS )
                  ENDIF

*               Write the value to GLOBAL.FITS_VALUE
                  WRITE(CVALUE,*) IVALUE
                  CVALEN = CHR_LEN( CVALUE )
                  CALL PAR_DEF0C( 'FITS_VALUE',
     :              CVALUE(1:CVALEN), STATUS )
                  CALL PAR_PUT0C( 'FITS_VALUE',
     :              CVALUE(1:CVALEN), STATUS )

                  IF ( METHOD(1:4) .NE. 'READ' ) THEN

                     CALL PAR_DEF0I( 'IVALUE', IVALUE, STATUS )
                     CALL PAR_GET0I( 'IVALUE', IVALUE, STATUS )

                     COMLEN = MAX( 1, CHR_LEN( COMMENT ) )
                     CALL DSA_PUT_FITS_I( 'DATA', ITEM, IVALUE,
     :                 COMMENT(1:COMLEN), DSA_STATUS )
                  ENDIF

               ELSE IF ( ( ACCESS(1:1) .EQ. 'R' )  .OR.
     :                   ( ACCESS(1:1) .EQ. 'F' )  .OR.
     :                   ( ACCESS(1:1) .EQ. 'D' )) THEN

*               Real or double precision type. (Process both as real).
                  CALL DSA_GET_FITS_F( 'DATA', ITEM, 0, RVALUE,
     :              COMMENT, DSA_STATUS )

                  IF ( MODE(1:7) .EQ. 'VERBOSE') THEN
                    CALL MSG_SETR( 'RVALUE', RVALUE )
                    CALL MSG_OUT( ' ', 'RED4_POKE_FITS: '/
     :                /'Current value is ^RVALUE', STATUS )
                  ENDIF

*               Write the value to GLOBAL.FITS_VALUE
                  WRITE(CVALUE,*) RVALUE
                  CVALEN = CHR_LEN( CVALUE )
                  CALL PAR_DEF0C( 'FITS_VALUE',
     :              CVALUE(1:CVALEN), STATUS )
                  CALL PAR_PUT0C( 'FITS_VALUE',
     :              CVALUE(1:CVALEN), STATUS )

                  IF ( METHOD(1:4) .NE. 'READ' ) THEN

                     CALL PAR_DEF0R( 'RVALUE', RVALUE, STATUS )
                     CALL PAR_GET0R( 'RVALUE', RVALUE, STATUS )

                     COMLEN = MAX( 1, CHR_LEN( COMMENT ) )
                     CALL DSA_PUT_FITS_F( 'DATA', ITEM, RVALUE,
     :                 COMMENT(1:COMLEN), DSA_STATUS )
                  ENDIF

               ELSE

                  STATUS = SAI__ERROR
                  CALL ERR_REP( ' ', 'RED4_POKE_FITS: '/
     :              /'Access type is illegal', STATUS )
               END IF
            ELSE

*            The item does not exist. It is a new item to be created if
*            method is WRITE or UPDATE otherwise there is an error.
               IF ( METHOD(1:4) .EQ. 'READ' ) THEN

                  STATUS = SAI__ERROR
                  CALL ERR_REP( ' ', 'RED4_POKE_FITS: '/
     :              /'Unable to read non-existent item!',
     :              STATUS )
               ELSE

                  CALL MSG_SETC( 'ITEM', ITEM )
                  CALL MSG_OUT( ' ',
     :               '^ITEM is a new item', STATUS )

*            Obtain the type required.
                  CALL PAR_GET0C( 'ACCESS', ACCESS, STATUS )

*            Obtain a value for this item, then write this to the FITS
*            structure. This all depends on what type the FITS item is
*            to be. The items will be written with a blank comment.
                  IF ( ACCESS(1:1) .EQ. 'C' ) THEN

*                  Character type.
                     CALL PAR_GET0C( 'CVALUE', CVALUE, STATUS )

                     CLEN = MAX( 1, CHR_LEN( CVALUE ) )
                     CALL DSA_PUT_FITS_C( 'DATA', ITEM, CVALUE(1:CLEN),
     :                 ' ', DSA_STATUS )

                  ELSE IF ( ( ACCESS(1:1) .EQ. 'I' ) .OR.
     :                      ( ACCESS(1:1) .EQ. 'S' ) .OR.
     :                      ( ACCESS(1:1) .EQ. 'L' ) ) THEN

*                  Integer or logical type. (Logical values are written
*                  as integers).
                     CALL PAR_GET0I( 'IVALUE', IVALUE, STATUS )

                     CALL DSA_PUT_FITS_I( 'DATA', ITEM, IVALUE,
     :                 ' ', DSA_STATUS )

                  ELSE IF ( ( ACCESS(1:1) .EQ. 'R' ) .OR.
     :                      ( ACCESS(1:1) .EQ. 'F' ) .OR.
     :                      ( ACCESS(1:1) .EQ. 'D' ) ) THEN

*                  Real or double precision type. (Process both as real).
                     CALL PAR_GET0R( 'RVALUE', RVALUE, STATUS )

                     CALL DSA_PUT_FITS_F( 'DATA', ITEM, RVALUE,
     :                 ' ', DSA_STATUS )

                  ELSE

                     STATUS = SAI__ERROR
                     CALL ERR_REP( ' ', 'RED4_POKE_FITS: '/
     :                 /'This access type is illegal', STATUS )
                  END IF
               END IF
            END IF
         ELSE

            STATUS = SAI__ERROR
            CALL ERR_REP( ' ', 'RED4_POKE_FITS: '/
     :         /'Error accessing data structure', STATUS )
         END IF

*      Close DSA. Note this will attempt to execute regardless of the
*      status.
         CALL DSA_CLOSE( DSA_STATUS )
      ELSE

         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'RED4_POKE_FITS: '/
     :     /'Error obtaining %DATA and %ITEM '/
     :     /'parameters', STATUS )
      END IF

      END
