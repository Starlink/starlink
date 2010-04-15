      SUBROUTINE TBL_SETNROWSW( TBDSCR, NUMROWS, STATUS)

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_ERR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'CMP_ERR'
      INCLUDE 'TBL_PAR'


*  Arguments Given and Returned:
      CHARACTER*(*) TBDSCR
      INTEGER       NUMROWS


*  Status:
      INTEGER STATUS


*  Local Variables:

        CHARACTER*(DAT__SZLOC) NLOC
        LOGICAL               REPLY

*.

        IF (STATUS .NE. SAI__OK) RETURN

* Check if NROWS exists already -- if not then create it

        CALL DAT_THERE( TBDSCR, 'NROWSW', REPLY, STATUS)
        IF ( REPLY ) THEN
            CALL CMP_PUT0I( TBDSCR, 'NROWSW', NUMROWS, STATUS)
        ELSE
            CALL DAT_NEW0I( TBDSCR, 'NROWSW', STATUS)
            CALL CMP_PUT0I( TBDSCR, 'NROWSW', NUMROWS, STATUS)
        ENDIF

        END
