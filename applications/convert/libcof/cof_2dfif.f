      SUBROUTINE COF_2DFIF( FUNIT, LOC, STATUS )
*+
*  Name:
*     COF_2DFIF

*  Purpose:
*     Transfers 2dF FIBRES BINTABLE headers to the FIELD extension.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL COF_2DFIF( FUNIT, LOC, STATUS )

*  Description:
*     This fills a 2dF FIELD structure using the FITS headers in the
*     current FITS binary table.  Some renaming is undertaken to convert
*     from keyword names to HDS component names.  Keywords UNAL-xxx
*     become component UNALLOCxxx, CONFMJD becomes CONFIGMJD, and
*     xSWTCHOF become xSWITCHOFF.  For further details see the
*     reference.

*  Arguments:
*     FUNIT = INTEGER (Given)
*        The logical unit number of the input FITS file.
*     LOC = CHARACTER * ( DAT__SZLOC ) (Given)
*        Locator to the 2dF FIELD extension structure which will be
*        filled with data stored in the binary-table headers.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  References:
*     Bailey, J.A. 1996,97, 2dF Software Report 14, versions 0.3, 0.5.

*  Prior Requirements:
*     The FITS and HDS files must be open.  Indeed the current HDU
*     within the FITS file must be a 2dF BINTABLE extension derived from
*     an NDF 2dF FIBRES extension, in which the scalar values of a
*     FIELD structure appear as headers.

*  [optional_subroutine_items]...
*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1997 Februray 28 (MJC):
*        Original version.
*     1997 November 10 (MJC):
*        Added FILENAME to the FIELD structure.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT__ constants
      INCLUDE 'PRM_PAR'          ! VAL__ constants

*  Arguments Given:
      INTEGER FUNIT
      CHARACTER * ( * ) LOC

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( DAT__SZLOC ) CLOC ! Locator to a structure component
      CHARACTER * ( 48 ) COMENT  ! FITS keyword comment
      CHARACTER * ( 80 ) CVALUE  ! Character value
      DOUBLE PRECISION DVALUE    ! D.p. value
      INTEGER IVALUE             ! Integer value
      INTEGER*2 WVALUE           ! Word value
      LOGICAL THERE              ! Component is present?

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Search for each FIELD scalar value within the FITS headers.  Note
*  that some are renamed in order to fit into the 8-character keyword
*  length.  If the value is present, create a new component, assigning
*  it the keyword value via a locator.  For_WORD data, there is no get
*  routine for this data type, so obtain an integer and copy it to a
*  two-byte integer before the value is inserted into the structure.
*
*  Deal with each keyword in turn so that its value is obtained using
*  the correct data type.

*  CENRA
      CALL COF_GKEYD( FUNIT, 'CENRA', THERE, DVALUE, COMENT, STATUS )
      IF ( THERE ) THEN
         CALL DAT_NEW0D( LOC, 'CENRA', STATUS )
         CALL DAT_FIND( LOC, 'CENRA', CLOC, STATUS )
         CALL DAT_PUT0D( CLOC, DVALUE, STATUS )
         CALL DAT_ANNUL( CLOC, STATUS )
      END IF

*  CENDEC
      CALL COF_GKEYD( FUNIT, 'CENDEC', THERE, DVALUE, COMENT, STATUS )
      IF ( THERE ) THEN
         CALL DAT_NEW0D( LOC, 'CENDEC', STATUS )
         CALL DAT_FIND( LOC, 'CENDEC', CLOC, STATUS )
         CALL DAT_PUT0D( CLOC, DVALUE, STATUS )
         CALL DAT_ANNUL( CLOC, STATUS )
      END IF

*  CENEQNX
      CALL COF_GKEYD( FUNIT, 'CENEQNX', THERE, DVALUE, COMENT, STATUS )
      IF ( THERE ) THEN
         CALL DAT_NEW0D( LOC, 'CENEQNX', STATUS )
         CALL DAT_FIND( LOC, 'CENEQNX', CLOC, STATUS )
         CALL DAT_PUT0D( CLOC, DVALUE, STATUS )
         CALL DAT_ANNUL( CLOC, STATUS )
      END IF

*  APPRA
      CALL COF_GKEYD( FUNIT, 'APPRA', THERE, DVALUE, COMENT, STATUS )
      IF ( THERE ) THEN
         CALL DAT_NEW0D( LOC, 'APPRA', STATUS )
         CALL DAT_FIND( LOC, 'APPRA', CLOC, STATUS )
         CALL DAT_PUT0D( CLOC, DVALUE, STATUS )
         CALL DAT_ANNUL( CLOC, STATUS )
      END IF

*  APPDEC
      CALL COF_GKEYD( FUNIT, 'APPDEC', THERE, DVALUE, COMENT, STATUS )
      IF ( THERE ) THEN
         CALL DAT_NEW0D( LOC, 'APPDEC', STATUS )
         CALL DAT_FIND( LOC, 'APPDEC', CLOC, STATUS )
         CALL DAT_PUT0D( CLOC, DVALUE, STATUS )
         CALL DAT_ANNUL( CLOC, STATUS )
      END IF

*  APPEPOCH
      CALL COF_GKEYD( FUNIT, 'APPEPOCH', THERE, DVALUE, COMENT, STATUS )
      IF ( THERE ) THEN
         CALL DAT_NEW0D( LOC, 'APPEPOCH', STATUS )
         CALL DAT_FIND( LOC, 'APPEPOCH', CLOC, STATUS )
         CALL DAT_PUT0D( CLOC, DVALUE, STATUS )
         CALL DAT_ANNUL( CLOC, STATUS )
      END IF

*  CONFIGMJD
      CALL COF_GKEYD( FUNIT, 'CONFMJD', THERE, DVALUE, COMENT, STATUS )
      IF ( THERE ) THEN
         CALL DAT_NEW0D( LOC, 'CONFIGMJD', STATUS )
         CALL DAT_FIND( LOC, 'CONFIGMJD', CLOC, STATUS )
         CALL DAT_PUT0D( CLOC, DVALUE, STATUS )
         CALL DAT_ANNUL( CLOC, STATUS )
      END IF

*  ACTMJD
      CALL COF_GKEYD( FUNIT, 'ACTMJD', THERE, DVALUE, COMENT, STATUS )
      IF ( THERE ) THEN
         CALL DAT_NEW0D( LOC, 'ACTMJD', STATUS )
         CALL DAT_FIND( LOC, 'ACTMJD', CLOC, STATUS )
         CALL DAT_PUT0D( CLOC, DVALUE, STATUS )
         CALL DAT_ANNUL( CLOC, STATUS )
      END IF

*  UNALLOCOBJ
      CALL COF_GKEYI( FUNIT, 'UNAL-OBJ', THERE, IVALUE, COMENT, STATUS )
      IF ( THERE ) THEN
         CALL DAT_NEW( LOC, 'UNALLOCOBJ', '_WORD', 0, 0, STATUS )
         CALL DAT_FIND( LOC, 'UNALLOCOBJ', CLOC, STATUS )
         WVALUE = IVALUE
         CALL DAT_PUT( CLOC, '_WORD', 0, 0, WVALUE, STATUS )
         CALL DAT_ANNUL( CLOC, STATUS )
      END IF

*  UNALLOCGUI
      CALL COF_GKEYI( FUNIT, 'UNAL-GUI', THERE, IVALUE, COMENT, STATUS )
      IF ( THERE ) THEN
         CALL DAT_NEW( LOC, 'UNALLOCGUI', '_WORD', 0, 0, STATUS )
         CALL DAT_FIND( LOC, 'UNALLOCGUI', CLOC, STATUS )
         WVALUE = IVALUE
         CALL DAT_PUT( CLOC, '_WORD', 0, 0, WVALUE, STATUS )
         CALL DAT_ANNUL( CLOC, STATUS )
      END IF

*  UNALLOCSKY
      CALL COF_GKEYI( FUNIT, 'UNAL-SKY', THERE, IVALUE, COMENT, STATUS )
      IF ( THERE ) THEN
         CALL DAT_NEW( LOC, 'UNALLOCSKY', '_WORD', 0, 0, STATUS )
         CALL DAT_FIND( LOC, 'UNALLOCSKY', CLOC, STATUS )
         WVALUE = IVALUE
         CALL DAT_PUT( CLOC, '_WORD', 0, 0, WVALUE, STATUS )
         CALL DAT_ANNUL( CLOC, STATUS )
      END IF

*  ALLOCOBJ
      CALL COF_GKEYI( FUNIT, 'ALLOCOBJ', THERE, IVALUE, COMENT, STATUS )
      IF ( THERE ) THEN
         CALL DAT_NEW( LOC, 'ALLOCOBJ', '_WORD', 0, 0, STATUS )
         CALL DAT_FIND( LOC, 'ALLOCOBJ', CLOC, STATUS )
         WVALUE = IVALUE
         CALL DAT_PUT( CLOC, '_WORD', 0, 0, WVALUE, STATUS )
         CALL DAT_ANNUL( CLOC, STATUS )
      END IF

*  ALLOCGUI
      CALL COF_GKEYI( FUNIT, 'ALLOCGUI', THERE, IVALUE, COMENT, STATUS )
      IF ( THERE ) THEN
         CALL DAT_NEW( LOC, 'ALLOCGUI', '_WORD', 0, 0, STATUS )
         CALL DAT_FIND( LOC, 'ALLOCGUI', CLOC, STATUS )
         WVALUE = IVALUE
         CALL DAT_PUT( CLOC, '_WORD', 0, 0, WVALUE, STATUS )
         CALL DAT_ANNUL( CLOC, STATUS )
      END IF

*  ALLOCSKY
      CALL COF_GKEYI( FUNIT, 'ALLOCSKY', THERE, IVALUE, COMENT, STATUS )
      IF ( THERE ) THEN
         CALL DAT_NEW( LOC, 'ALLOCSKY', '_WORD', 0, 0, STATUS )
         CALL DAT_FIND( LOC, 'ALLOCSKY', CLOC, STATUS )
         WVALUE = IVALUE
         CALL DAT_PUT( CLOC, '_WORD', 0, 0, WVALUE, STATUS )
         CALL DAT_ANNUL( CLOC, STATUS )
      END IF

*  XSWITCHOFFSET
      CALL COF_GKEYI( FUNIT, 'XSWTCHOF', THERE, IVALUE, COMENT, STATUS )
      IF ( THERE ) THEN
         CALL DAT_NEW( LOC, 'XSWITCHOFFSET', '_WORD', 0, 0, STATUS )
         CALL DAT_FIND( LOC, 'XSWITCHOFFSET', CLOC, STATUS )
         WVALUE = IVALUE
         CALL DAT_PUT( CLOC, '_WORD', 0, 0, WVALUE, STATUS )
         CALL DAT_ANNUL( CLOC, STATUS )
      END IF

*  YSWITCHOFFSET
      CALL COF_GKEYI( FUNIT, 'YSWTCHOF', THERE, IVALUE, COMENT, STATUS )
      IF ( THERE ) THEN
         CALL DAT_NEW( LOC, 'YSWITCHOFFSET', '_WORD', 0, 0, STATUS )
         CALL DAT_FIND( LOC, 'YSWITCHOFFSET', CLOC, STATUS )
         WVALUE = IVALUE
         CALL DAT_PUT( CLOC, '_WORD', 0, 0, WVALUE, STATUS )
         CALL DAT_ANNUL( CLOC, STATUS )
      END IF

*  PROGID
      CALL COF_GKEYC( FUNIT, 'PROGID', THERE, CVALUE, COMENT, STATUS )
      IF ( THERE ) THEN
         CALL DAT_NEW0C( LOC, 'PROGID', 80, STATUS )
         CALL DAT_FIND( LOC, 'PROGID', CLOC, STATUS )
         CALL DAT_PUT0C( CLOC, CVALUE, STATUS )
         CALL DAT_ANNUL( CLOC, STATUS )
      END IF

*  MODE
      CALL COF_GKEYC( FUNIT, 'MODE', THERE, CVALUE, COMENT, STATUS )
      IF ( THERE ) THEN
         CALL DAT_NEW0C( LOC, 'MODE', 80, STATUS )
         CALL DAT_FIND( LOC, 'MODE', CLOC, STATUS )
         CALL DAT_PUT0C( CLOC, CVALUE, STATUS )
         CALL DAT_ANNUL( CLOC, STATUS )
      END IF

*  LABEL
      CALL COF_GKEYC( FUNIT, 'LABEL', THERE, CVALUE, COMENT, STATUS )
      IF ( THERE ) THEN
         CALL DAT_NEW0C( LOC, 'LABEL', 80, STATUS )
         CALL DAT_FIND( LOC, 'LABEL', CLOC, STATUS )
         CALL DAT_PUT0C( CLOC, CVALUE, STATUS )
         CALL DAT_ANNUL( CLOC, STATUS )
      END IF

*  FILENAME
      CALL COF_GKEYC( FUNIT, 'FILENAME', THERE, CVALUE, COMENT, STATUS )
      IF ( THERE ) THEN
         CALL DAT_NEW0C( LOC, 'FILENAME', 500, STATUS )
         CALL DAT_FIND( LOC, 'FILENAME', CLOC, STATUS )
         CALL DAT_PUT0C( CLOC, CVALUE, STATUS )
         CALL DAT_ANNUL( CLOC, STATUS )
      END IF

      END
