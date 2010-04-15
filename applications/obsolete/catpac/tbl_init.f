      SUBROUTINE TBL_INIT( TBNAME, TBDSCR, STATUS )


*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'DAT_ERR'
      INCLUDE 'CMP_ERR'
      INCLUDE 'TBL_PAR'

*  Arguments Given and Returned:
      CHARACTER*(*) TBDSCR
      CHARACTER*(*) TBNAME

*  Status:
      INTEGER STATUS

*  Local Variables:

      CHARACTER*(DAT__SZLOC) NLOC
*.

      IF (STATUS .NE. SAI__OK) RETURN

      CALL HDS_NEW(TBNAME,'TABLE', 'TABLE', 0, 0, TBDSCR, STATUS)
      IF (STATUS .NE. SAI__OK) THEN
          STATUS = TBL__ECRTAB
          CALL ERR_REP( ' ', 'Error creating table', STATUS)
          GOTO 9999
      ENDIF

      CALL DAT_NEW(TBDSCR, 'NROWS', '_INTEGER', 0, 0, STATUS)
      IF (STATUS .NE. SAI__OK) THEN
          STATUS = TBL__ECRTAB
          CALL ERR_REP(' ','Error while creating NROWS param',
     +                           STATUS)
          GOTO 9999
      ENDIF

      CALL DAT_NEW(TBDSCR, 'COLUMNS', 'COLUMNS', 0, 0, STATUS)
      IF (STATUS .NE. SAI__OK) THEN
          STATUS = TBL__ECRTAB
          CALL ERR_REP(' ','Error while creating COLUMNS struct',
     +                           STATUS)
          GOTO 9999
      ENDIF

      CALL DAT_NEW(TBDSCR, 'PARAMS', 'PARAMS', 0, 0, STATUS)
      IF (STATUS .NE. SAI__OK) THEN
          STATUS = TBL__ECRTAB
          CALL ERR_REP(' ','Error while creating PARAMS struct',
     +                           STATUS)
          GOTO 9999
      ENDIF

      CALL DAT_NEW(   TBDSCR, 'NOTES', 'NOTES', 0, 0, STATUS)
      CALL DAT_FIND(  TBDSCR, 'NOTES', NLOC, STATUS)
      CALL DAT_NEW1C( NLOC,   'NOTE', 80, 5, STATUS)
      CALL DAT_NEW0I( NLOC,   'NUMNOTES', STATUS)
      CALL CMP_PUT0I( NLOC,   'NUMNOTES', 0, STATUS)
      CALL DAT_ANNUL( NLOC, STATUS)
      IF (STATUS .NE. SAI__OK) THEN
          STATUS = TBL__ECRTAB
          CALL ERR_REP(' ','Error while creating NOTES struct',
     +                           STATUS)
          GOTO 9999
      ENDIF

      CALL DAT_NEW(TBDSCR, 'NROWSW', '_INTEGER', 0, 0, STATUS)
      IF (STATUS .NE. SAI__OK) THEN
          STATUS = TBL__ECRTAB
          CALL ERR_REP(' ','Error while creating NROWSW', STATUS)
          GOTO 9999
      ENDIF

      CALL DAT_NEW0C(TBDSCR, 'AUTHOR', DAT__SZNAM, STATUS)
      IF (STATUS .NE. SAI__OK) THEN
          STATUS = TBL__ECRTAB
          CALL ERR_REP(' ',
     :                'Error while creating the header AUTHOR',
     :                                  STATUS)
          GOTO 9999
      ENDIF

9999  RETURN
      END
