      SUBROUTINE SPD_CZWE( NDF, XLOC, REQEST, MXNWRD, NWORD, WORD,
     :                     STARTW, STATUS )
*+
*  Name:
*     SPD_CZWE

*  Purpose:
*     Set action for EDITEXT.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPD_CZWE( NDF, XLOC, REQEST, MXNWRD, NWORD, WORD, STARTW,
*        STATUS )

*  Description:
*     This routine sets values of structures in the Specdre Extension
*     according to a request expressed in several words.

*  Arguments:
*     NDF = INTEGER (Given)
*        The identifier of the main NDF to which the Specdre Extension
*        is an extension.
*     XLOC = CHARACTER * ( * ) (Given)
*        The HDS locator to the Specdre Extension.
*     REQEST = CHARACTER * ( * ) (Given)
*        The request string.
*     MXNWRD = INTEGER (Given)
*        The maximum number of words found in the request string. This
*        is the length of the array of words.
*     NWORD = INTEGER (Given)
*        The number of words actually found in the request string.
*     WORD( MXNWRD ) = CHARACTER * ( * ) (Given)
*        The words from the request string.
*     STARTW( MXNWRD ) = INTEGER (Given)
*        These point to the beginning of the words in the request
*        string.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     This routine recognises Specdre Extension v. 1.1.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     15 Mar 1992 (hme):
*        Original version.
*     22 Jun 1992 (hme):
*        Use SPE-routines and SPEPAR include file.
*     24 Nov 1994 (hme):
*        Rename from SPABC. Remove MESSAG argument. Report error from
*        this routine and immediately.
*     30 Jan 1995 (hme):
*        There was one MESSAG = statment left that was in an IF
*        statement rather than an IF THEN block. So SET SPECVALS would
*        always call ERR_REP and GOTO 500, even though STATUS was fine.
*        In fact there were about a handful of these bugs.
*     24 Nov 1995 (hme):
*        Add support for COORD (Extension v. 0.7 -> 1.1)
*     2005 May 31 (MJC):
*        Use CNF_PVAL for pointers to mapped data.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! Standard DAT constants
      INCLUDE 'NDF_PAR'          ! Standard NDF constants
      INCLUDE 'SPD_EPAR'         ! Specdre Extension parameters
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Arguments Given:
      INTEGER NDF
      CHARACTER * ( * ) XLOC
      CHARACTER * ( * ) REQEST
      INTEGER MXNWRD
      INTEGER NWORD
      CHARACTER * ( * ) WORD( MXNWRD )
      INTEGER STARTW( MXNWRD )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      LOGICAL EXIST              ! True if structure exists
      INTEGER ELEM               ! Element number in result vector
      INTEGER BNDF               ! Base NDF identifier
      INTEGER XNDF               ! Extension NDF identifier
      INTEGER TPNTR( 2 )         ! Temporary array pointer
      INTEGER NELM               ! Array size
      INTEGER IVALUE             ! Any integer parameter value
      REAL RVALUE                ! Any real parameter value
      DOUBLE PRECISION DVALUE    ! Any double parameter value
      CHARACTER * ( 32 ) CVALUE  ! Any character parameter value
      CHARACTER * ( 64 ) CVAL64  ! Any longer parameter value
      CHARACTER * ( 64 ) LABEL   ! Unused
      CHARACTER * ( 64 ) UNITS   ! Unused
      CHARACTER * ( 64 ) MESSAG  ! Error message string
      CHARACTER * ( NDF__SZTYP ) TYPE( 3 ) ! Data types
      CHARACTER * ( DAT__SZLOC ) TLOC ! Temporary locator

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If spectroscopic axis to be set.
      IF ( WORD(2) .EQ. XCMP1 ) THEN

*     See if parameters are present.
         IF ( NWORD .LT. 3 ) THEN
            STATUS = SAI__ERROR
            MESSAG = 'Error reading parameters.'
            CALL ERR_REP( 'SPD_CZWE_E01', 'EDITEXT: ' //
     :         MESSAG, STATUS )
            GO TO 500
         END IF

*     Get the numeric parameters to shape the structure.
         CALL CHR_CTOI( WORD(3), IVALUE, STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            MESSAG = 'Error reading parameters.'
            CALL ERR_REP( 'SPD_CZWE_E01', 'EDITEXT: ' //
     :         MESSAG, STATUS )
            GO TO 500
         END IF

*     Set spectroscopic axis.
         CALL SPD_EABB( NDF, XLOC, IVALUE, STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            MESSAG = 'Error setting spectroscopic axis.'
            CALL ERR_REP( 'SPD_CZWE_E01', 'EDITEXT: ' //
     :         MESSAG, STATUS )
            GO TO 500
         END IF

*  Else if reference frame to be set.
      ELSE IF ( WORD(2) .EQ. XCMP2 ) THEN

*     See if parameters are present.
         IF ( NWORD .LT. 3 ) THEN
            STATUS = SAI__ERROR
            MESSAG = 'Error reading parameters.'
            CALL ERR_REP( 'SPD_CZWE_E01', 'EDITEXT: ' //
     :         MESSAG, STATUS )
            GO TO 500
         END IF

*     Cut parameter to length and put it.
*     (Parameter may consist of several words with any number of
*     blanks to separate).
         CVALUE = REQEST(STARTW(3):)
         CALL NDF_XPT0C( CVALUE, NDF, XNAME, WORD(2), STATUS )

*  Else if refractive index or reference frequency to be set.
      ELSE IF ( WORD(2) .EQ. XCMP3 .OR.
     :          WORD(2) .EQ. XCMP4 ) THEN

*     See if parameters are present.
         IF ( NWORD .LT. 3 ) THEN
            STATUS = SAI__ERROR
            MESSAG = 'Error reading parameters.'
            CALL ERR_REP( 'SPD_CZWE_E01', 'EDITEXT: ' //
     :         MESSAG, STATUS )
            GO TO 500
         END IF

*     Must find out the existing type.
         TYPE(1) = XTYP3
         CALL DAT_THERE( XLOC, WORD(2), EXIST, STATUS )
         IF ( EXIST ) CALL CMP_TYPE( XLOC, WORD(2), TYPE(1), STATUS )

*     Get and put value.
         IF ( TYPE(1) .EQ. '_DOUBLE' ) THEN
            CALL CHR_CTOD( WORD(3), DVALUE, STATUS )
            IF ( STATUS .NE. SAI__OK ) THEN
               MESSAG = 'Error reading parameters.'
               CALL ERR_REP( 'SPD_CZWE_E01', 'EDITEXT: ' //
     :            MESSAG, STATUS )
               GO TO 500
            END IF
            CALL NDF_XPT0D( DVALUE, NDF, XNAME, WORD(2), STATUS )
         ELSE
            CALL CHR_CTOR( WORD(3), RVALUE, STATUS )
            IF ( STATUS .NE. SAI__OK ) THEN
               MESSAG = 'Error reading parameters.'
               CALL ERR_REP( 'SPD_CZWE_E01', 'EDITEXT: ' //
     :            MESSAG, STATUS )
               GO TO 500
            END IF
            CALL NDF_XPT0R( RVALUE, NDF, XNAME, WORD(2), STATUS )
         END IF

*  Else if frequency unit to be set.
      ELSE IF ( WORD(2) .EQ. XCMP5 ) THEN

*     See if command parameters are present.
         IF ( NWORD .LT. 3 ) THEN
            STATUS = SAI__ERROR
            MESSAG = 'Error reading parameters.'
            CALL ERR_REP( 'SPD_CZWE_E01', 'EDITEXT: ' //
     :         MESSAG, STATUS )
            GO TO 500
         END IF

*     Get and put value.
         CALL CHR_CTOI( WORD(3), IVALUE, STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            MESSAG = 'Error reading parameters.'
            CALL ERR_REP( 'SPD_CZWE_E01', 'EDITEXT: ' //
     :         MESSAG, STATUS )
            GO TO 500
         END IF
         CALL NDF_XPT0I( IVALUE, NDF, XNAME, WORD(2), STATUS )

*  Else if spectroscopic values to be set to default.
      ELSE IF ( WORD(2) .EQ. XCMP6 ) THEN

*     Find out existing type and delete.
*     Then re-create.
         CALL DAT_THERE( XLOC, WORD(2), EXIST, STATUS )
         IF ( EXIST ) THEN
            CALL NDF_FIND( XLOC, WORD(2), XNDF, STATUS )
            CALL NDF_TYPE( XNDF, XC6D, TYPE(1), STATUS )
            CALL NDF_ANNUL( XNDF, STATUS )
            CALL DAT_ERASE( XLOC, WORD(2), STATUS )
         END IF
         IF ( TYPE(1) .NE. '_DOUBLE' ) TYPE(1) = XT6D
         CALL SPD_EAEF( NDF, XLOC, TYPE(1), STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            MESSAG = 'Error setting spectroscopic values.'
            CALL ERR_REP( 'SPD_CZWE_E01', 'EDITEXT: ' //
     :         MESSAG, STATUS )
            GO TO 500
         END IF

*  Else if label of spectroscopic values to be set.
      ELSE IF ( WORD(2) .EQ. XCMP6 // '.' // XC6L ) THEN

*     See if parameters are present.
         IF ( NWORD .LT. 3 ) THEN
            STATUS = SAI__ERROR
            MESSAG = 'Error reading parameters.'
            CALL ERR_REP( 'SPD_CZWE_E01', 'EDITEXT: ' //
     :         MESSAG, STATUS )
            GO TO 500
         END IF

*     Cut parameter to length.
*     (Parameter may consist of several words with any number of
*     blanks to separate).
         CVAL64 = REQEST(STARTW(3):)

*     Put the parameter. First access the NDF, then put its label
*     component. Don't forget to annul the identifier.
         CALL NDF_FIND( XLOC, XCMP6, XNDF, STATUS )
         CALL NDF_CPUT( CVAL64, XNDF, XC6L, STATUS )
         CALL NDF_ANNUL( XNDF, STATUS )

*  Else if unit of spectroscopic values to be set.
      ELSE IF ( WORD(2) .EQ. XCMP6 // '.' // XC6U ) THEN

*     See if parameters are present.
         IF ( NWORD .LT. 3 ) THEN
            STATUS = SAI__ERROR
            MESSAG = 'Error reading parameters.'
            CALL ERR_REP( 'SPD_CZWE_E01', 'EDITEXT: ' //
     :         MESSAG, STATUS )
            GO TO 500
         END IF

*     Cut parameter to length.
*     (Parameter may consist of several words with any number of
*     blanks to separate).
         CVAL64 = REQEST(STARTW(3):)

*     Put the parameter. First access the NDF, then put its label
*     component. Don't forget to annul the identifier.
         CALL NDF_FIND( XLOC, XCMP6, XNDF, STATUS )
         CALL NDF_CPUT( CVAL64, XNDF, XC6U, STATUS )
         CALL NDF_ANNUL( XNDF, STATUS )

*  Else if spectroscopic widths to be set to default.
      ELSE IF ( WORD(2) .EQ. XCMP7 ) THEN

*     Find out existing type and delete.
         CALL DAT_THERE( XLOC, WORD(2), EXIST, STATUS )
         IF ( EXIST ) THEN
            CALL NDF_FIND( XLOC, WORD(2), XNDF, STATUS )
            CALL NDF_TYPE( XNDF, XC7D, TYPE(1), STATUS )
            CALL NDF_ANNUL( XNDF, STATUS )
            CALL DAT_ERASE( XLOC, WORD(2), STATUS )
         END IF
         IF ( TYPE(1) .NE. '_DOUBLE' ) TYPE(1) = XT7D

*     Then re-create. This depends on whether SPECVALS exist or not.
         CALL DAT_THERE( XLOC, XCMP6, EXIST, STATUS )
         IF ( EXIST ) THEN
            CALL NDF_BASE( NDF, BNDF, STATUS )
            CALL SPD_EAED( BNDF, XLOC, 'READ', TYPE(1), LABEL, UNITS,
     :                     TPNTR(1), XNDF, NELM, STATUS )
            CALL SPD_EAFF( BNDF, XLOC, TYPE(1),
     :                     %VAL( CNF_PVAL( TPNTR(1) ) ), STATUS )
            CALL NDF_ANNUL( XNDF, STATUS )
            CALL NDF_ANNUL( BNDF, STATUS )
         ELSE
            CALL SPD_EAFH( NDF, XLOC, TYPE(1), STATUS )
         END IF
         IF ( STATUS .NE. SAI__OK ) THEN
            MESSAG = 'Error setting spectroscopic widths.'
            CALL ERR_REP( 'SPD_CZWE_E01', 'EDITEXT: ' //
     :         MESSAG, STATUS )
            GO TO 500
         END IF

*  Else if coordinate to be set to default.
      ELSE IF ( WORD(2) .EQ. 'COORD' ) THEN

*     Find out existing type and delete.
*     Then re-create.
         CALL DAT_THERE( XLOC, XCMP10, EXIST, STATUS )
         IF ( EXIST ) THEN
            CALL NDF_FIND( XLOC, XCMP10, XNDF, STATUS )
            CALL NDF_TYPE( XNDF, XC10D, TYPE(1), STATUS )
            CALL NDF_ANNUL( XNDF, STATUS )
            CALL DAT_ERASE( XLOC, XCMP10, STATUS )
         END IF
         IF ( TYPE(1) .NE. '_DOUBLE' ) TYPE(1) = XT10D
         TYPE(2) = TYPE(1)
         CALL SPD_EAJF( NDF, XLOC, TYPE, STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            MESSAG = 'Error setting coordinates.'
            CALL ERR_REP( 'SPD_CZWE_E01', 'EDITEXT: ' //
     :         MESSAG, STATUS )
            GO TO 500
         END IF

*  Else if labels of first coordinate to be set.
      ELSE IF ( WORD(2) .EQ. XCMP10 // '.' // XC10L ) THEN

*     See if parameters are present.
         IF ( NWORD .LT. 3 ) THEN
            STATUS = SAI__ERROR
            MESSAG = 'Error reading parameters.'
            CALL ERR_REP( 'SPD_CZWE_E01', 'EDITEXT: ' //
     :         MESSAG, STATUS )
            GO TO 500
         END IF

*     Cut parameter to length.
*     (Parameter may consist of several words with any number of
*     blanks to separate).
         CVAL64 = REQEST(STARTW(3):)

*     Put the parameter. First access the NDF, then put its label
*     component. Don't forget to annul the identifier.
         CALL NDF_FIND( XLOC, XCMP10, XNDF, STATUS )
         CALL NDF_CPUT( CVAL64, XNDF, XC10L, STATUS )
         CALL NDF_ANNUL( XNDF, STATUS )

*  Else if labels of second coordinate to be set.
      ELSE IF ( WORD(2) .EQ. XCMP11 // '.' // XC11L ) THEN

*     See if parameters are present.
         IF ( NWORD .LT. 3 ) THEN
            STATUS = SAI__ERROR
            MESSAG = 'Error reading parameters.'
            CALL ERR_REP( 'SPD_CZWE_E01', 'EDITEXT: ' //
     :         MESSAG, STATUS )
            GO TO 500
         END IF

*     Cut parameter to length.
*     (Parameter may consist of several words with any number of
*     blanks to separate).
         CVAL64 = REQEST(STARTW(3):)

*     Put the parameter. First access the NDF, then put its label
*     component. Don't forget to annul the identifier.
         CALL NDF_FIND( XLOC, XCMP11, XNDF, STATUS )
         CALL NDF_CPUT( CVAL64, XNDF, XC11L, STATUS )
         CALL NDF_ANNUL( XNDF, STATUS )

*  Else if unit of first coordinate to be set.
      ELSE IF ( WORD(2) .EQ. XCMP10 // '.' // XC10U ) THEN

*     See if parameters are present.
         IF ( NWORD .LT. 3 ) THEN
            STATUS = SAI__ERROR
            MESSAG = 'Error reading parameters.'
            CALL ERR_REP( 'SPD_CZWE_E01', 'EDITEXT: ' //
     :         MESSAG, STATUS )
            GO TO 500
         END IF

*     Cut parameter to length.
*     (Parameter may consist of several words with any number of
*     blanks to separate).
         CVAL64 = REQEST(STARTW(3):)

*     Put the parameter. First access the NDF, then put its label
*     component. Don't forget to annul the identifier.
         CALL NDF_FIND( XLOC, XCMP10, XNDF, STATUS )
         CALL NDF_CPUT( CVAL64, XNDF, XC10U, STATUS )
         CALL NDF_ANNUL( XNDF, STATUS )

*  Else if unit of second coordinate to be set.
      ELSE IF ( WORD(2) .EQ. XCMP11 // '.' // XC11U ) THEN

*     See if parameters are present.
         IF ( NWORD .LT. 3 ) THEN
            STATUS = SAI__ERROR
            MESSAG = 'Error reading parameters.'
            CALL ERR_REP( 'SPD_CZWE_E01', 'EDITEXT: ' //
     :         MESSAG, STATUS )
            GO TO 500
         END IF

*     Cut parameter to length.
*     (Parameter may consist of several words with any number of
*     blanks to separate).
         CVAL64 = REQEST(STARTW(3):)

*     Put the parameter. First access the NDF, then put its label
*     component. Don't forget to annul the identifier.
         CALL NDF_FIND( XLOC, XCMP11, XNDF, STATUS )
         CALL NDF_CPUT( CVAL64, XNDF, XC11U, STATUS )
         CALL NDF_ANNUL( XNDF, STATUS )

*  Else if result data and variance to be set bad.
      ELSE IF ( WORD(2) .EQ. XCMP9 ) THEN

*     Only if the result structure has to be created, are the integers 6
*     and 20 used as number of spectral components and total number of
*     result parameters.
         CALL DAT_THERE( XLOC, WORD(2), EXIST, STATUS )
         IF ( EXIST ) THEN
            CALL NDF_FIND( XLOC, WORD(2), XNDF, STATUS )
            CALL NDF_TYPE( XNDF, 'DATA,VARIANCE', TYPE(1), STATUS )
            CALL NDF_MAP(  XNDF, 'DATA,VARIANCE', TYPE(1), 'WRITE/BAD',
     :                     TPNTR, NELM, STATUS )
            CALL NDF_ANNUL( XNDF, STATUS )
         ELSE
            TYPE(1) = XT9D
            TYPE(2) = XT9C2
            TYPE(3) = XT9C5
            CALL SPD_FDHF( NDF, XLOC, 6, 20, TYPE, STATUS )
         END IF
         IF ( STATUS .NE. SAI__OK ) THEN
            MESSAG = 'Error setting result values.'
            CALL ERR_REP( 'SPD_CZWE_E01', 'EDITEXT: ' //
     :         MESSAG, STATUS )
            GO TO 500
         END IF

*  Else if a line name or spectral component type or result parameter
*  type to be set.
      ELSE IF ( WORD(2) .EQ. XC9C1 .OR. WORD(2) .EQ. XC9C3 .OR.
     :          WORD(2) .EQ. XC9P1 ) THEN

*     See if command parameters are present.
         IF ( NWORD .LT. 4 ) THEN
            STATUS = SAI__ERROR
            MESSAG = 'Error reading parameters.'
            CALL ERR_REP( 'SPD_CZWE_E01', 'EDITEXT: ' //
     :         MESSAG, STATUS )
            GO TO 500
         END IF

*     Get element number.
         CALL CHR_CTOI( WORD(3), ELEM, STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            MESSAG = 'Error reading parameters.'
            CALL ERR_REP( 'SPD_CZWE_E01', 'EDITEXT: ' //
     :         MESSAG, STATUS )
            GO TO 500
         END IF

*     Cut command parameter to length.
*     (Parameter may consist of several words with any number of
*     blanks to separate).
         CVALUE = REQEST(STARTW(4):)

*     Put value.
         CALL SPD_FDADC( XLOC, WORD(2), ELEM, CVALUE, STATUS )

*  Else if a laboratory frequency or mask bound to be set.
      ELSE IF ( WORD(2) .EQ. XC9C2 .OR. WORD(2) .EQ. XC9C5 .OR.
     :          WORD(2) .EQ. XC9C6 ) THEN

*     See if parameters are present.
         IF ( NWORD .LT. 4 ) THEN
            STATUS = SAI__ERROR
            MESSAG = 'Error reading parameters.'
            CALL ERR_REP( 'SPD_CZWE_E01', 'EDITEXT: ' //
     :         MESSAG, STATUS )
            GO TO 500
         END IF

*     Must find out the existing type.
         CALL NDF_FIND( XLOC, XCMP9, XNDF, STATUS )
         CALL NDF_XLOC( XNDF, WORD(2), 'READ', TLOC, STATUS )
         CALL DAT_TYPE( TLOC, TYPE(1), STATUS )
         CALL DAT_ANNUL( TLOC, STATUS )
         CALL NDF_ANNUL( XNDF, STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            MESSAG = 'Error locating result structure.'
            CALL ERR_REP( 'SPD_CZWE_E01', 'EDITEXT: ' //
     :         MESSAG, STATUS )
            GO TO 500
         END IF

*     Get command parameters and put the value.
         CALL CHR_CTOI( WORD(3), ELEM, STATUS )
         IF ( TYPE(1) .EQ. '_DOUBLE' ) THEN
            CALL CHR_CTOD( WORD(4), DVALUE, STATUS )
            IF ( STATUS .NE. SAI__OK ) THEN
               MESSAG = 'Error reading parameters.'
               CALL ERR_REP( 'SPD_CZWE_E01', 'EDITEXT: ' //
     :            MESSAG, STATUS )
               GO TO 500
            END IF
            CALL SPD_FDADD( XLOC, WORD(2), ELEM, DVALUE, STATUS )
         ELSE
            CALL CHR_CTOR( WORD(4), RVALUE, STATUS )
            IF ( STATUS .NE. SAI__OK ) THEN
               MESSAG = 'Error reading parameters.'
               CALL ERR_REP( 'SPD_CZWE_E01', 'EDITEXT: ' //
     :            MESSAG, STATUS )
               GO TO 500
            END IF
            CALL SPD_FDADR( XLOC, WORD(2), ELEM, RVALUE, STATUS )
         END IF

*  Else if a spectral component's number of result parameters to be set.
      ELSE IF ( WORD(2) .EQ. XC9C4 ) THEN

*     See if command parameters are present.
         IF ( NWORD .LT. 4 ) THEN
            STATUS = SAI__ERROR
            MESSAG = 'Error reading parameters.'
            CALL ERR_REP( 'SPD_CZWE_E01', 'EDITEXT: ' //
     :         MESSAG, STATUS )
            GO TO 500
         END IF

*     Get command parameters.
         CALL CHR_CTOI( WORD(3), ELEM, STATUS )
         CALL CHR_CTOI( WORD(4), IVALUE, STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            MESSAG = 'Error reading parameters.'
            CALL ERR_REP( 'SPD_CZWE_E01', 'EDITEXT: ' //
     :         MESSAG, STATUS )
            GO TO 500
         END IF

*     Put value.
*     This includes getting space for more result parameters if
*     necessary, and shifting the existing information in the result NDF
*     and the PARATYPE vector.
         CALL SPD_FDHC( XLOC, ELEM, IVALUE, STATUS )

*  Else (unrecognised request to set something).
      ELSE
         STATUS = SAI__ERROR
         MESSAG = 'Error: Invalid SET request.'
         CALL ERR_REP( 'SPD_CZWE_E01', 'EDITEXT: ' //
     :      MESSAG, STATUS )
         GO TO 500
      END IF

*  Return.
 500  CONTINUE
      END
