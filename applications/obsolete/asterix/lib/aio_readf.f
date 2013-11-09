*+  AIO_READF - Read a record from the input stream
      SUBROUTINE AIO_READF( ID, DATA, STATUS )
*
*    Description :
*
*
*
*    Method :
*     <description of how the subroutine works - for programmer info>
*    Deficiencies :
*
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*
*     David J. Allan (JET-X, University of Birmingham)
*
*    History :
*
*     11 Nov 94 : Original (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'FIO_ERR'
      INCLUDE 'AIO_PAR'
*
*    Global variables :
*
      INCLUDE 'AIO_CMN'
*
*    Import :
*
      INTEGER			ID			! Channel id
*
*    Export :
*
      CHARACTER*(*)		DATA			! Output data
*
*    Status :
*
      INTEGER STATUS
*
*    Function definitions :
*
      INTEGER			CHR_LEN
*
*    External references :
*
      EXTERNAL                  AIO_BLK
*
*    Local variables :
*
      CHARACTER*132		IFILE			! Include file name

      INTEGER			ILEN			! Length of IFILE
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Check the channel
      IF ( .NOT. AIO_IDEF ) THEN
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'Input channel not active', STATUS )

      ELSE IF ( ID .NE. AIO_IFID(1) ) THEN
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'Invalid input channel id', STATUS )

      ELSE

*      Looping point for nested files
 10     CONTINUE

*      Read the data
        CALL FIO_READF( AIO_IFID(AIO_ILEV), DATA, STATUS )

*      End of file met in nested read?
        IF ( (STATUS.EQ.FIO__EOF) .AND. AIO_INEST .AND.
     :       (AIO_ILEV.GT.1) ) THEN
          CALL ERR_ANNUL( STATUS )
          CALL FIO_CLOSE( AIO_IFID(AIO_ILEV), STATUS )
          AIO_ILEV = AIO_ILEV - 1
          GOTO 10
        END IF

*      Check for nested include
        IF ( AIO_INEST ) THEN

*        First 8 characters must be '#include'
          IF ( DATA(1:8) .EQ. '#include' ) THEN

*          Locate include file name
            IFILE = DATA(10:)

*          Strip leading blanks
            CALL CHR_LDBLK( IFILE )
            ILEN = CHR_LEN( IFILE )

*          Strip quotes
            IF ( (IFILE(1:1) .EQ. '"') .OR.
     :           (IFILE(1:1) .EQ. '''') ) THEN
              IFILE = IFILE(2:ILEN-1)
            END IF

*          Max depth not exceeded
            IF ( AIO_ILEV .EQ. AIO__MXNEST ) THEN
              STATUS = SAI__ERROR
              CALL MSG_SETC( 'IFILE', IFILE )
              CALL ERR_REP( ' ', 'Attempt to exceed maximum nesting'/
     :                        /' depth include file ^IFILE', STATUS )
            ELSE

*            Attempt to open include file
              CALL AIO_FOPEN1( IFILE, 'READ', 'LIST', 0,
     :                         AIO_IFID(AIO_ILEV+1), STATUS )

*            Success?
              IF ( STATUS .EQ. SAI__OK ) THEN
                AIO_ILEV = AIO_ILEV + 1
                GOTO 10

              ELSE

                STATUS = SAI__ERROR
                CALL MSG_SETC( 'IFILE', IFILE )
                CALL ERR_REP( ' ', 'Open failure on include file '/
     :                        /' ^IFILE', STATUS )

              END IF

            END IF

          END IF

        END IF

      END IF

*    Tidy up
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'AIO_READF', STATUS )
      END IF

      END
