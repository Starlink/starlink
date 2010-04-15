      SUBROUTINE SPD_CZWF( NDF, XLOC, MXNWRD, NWORD, WORD, STATUS )
*+
*  Name:
*     SPD_CZWF

*  Purpose:
*     Type action for EDITEXT.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPD_CZWF( NDF, XLOC, MXNWRD, NWORD, WORD, STATUS )

*  Description:
*     This routine changes the numeric type of _REAL or _DOUBLE
*     structures in the Specdre Extension according to a request
*     expressed in several words. The word(s) that specify type(s) are
*     converted to uppercase. Any type specification not equal to
*     _DOUBLE will be assumed to mean _REAL.

*  Arguments:
*     NDF = INTEGER (Given)
*        The identifier of the main NDF to which the Specdre Extension
*        is an extension.
*     XLOC = CHARACTER * ( * ) (Given)
*        The HDS locator to the Specdre Extension.
*     MXNWRD = INTEGER (Given)
*        The maximum number of words found in the request. This is the
*        length of the array of words.
*     NWORD = INTEGER (Given)
*        The number of words actually found in the request.
*     WORD( MXNWRD ) = CHARACTER * ( * ) (Given and Returned)
*        The words from the request.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     This routine recognises Specdre Extension v. 1.1.

*  Authors:
*     HME: Horst Meyerdierks (UoE, Starlink)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     13-MAR-1992 (HME):
*        Original version.
*     22-JUN-1992 (HME):
*        Use SPE-routines and the SPEPAR include file.
*     24 Nov 1994 (hme):
*        Rename from SPABF. Remove MESSAG argument. Report error from
*        this routine and immediately.
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
      INCLUDE 'PRM_PAR'          ! Standard PRIMDAT constants
      INCLUDE 'SPD_EPAR'         ! Specdre Extension parameters
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Arguments Given:
      INTEGER NDF
      CHARACTER * ( * ) XLOC
      INTEGER MXNWRD
      INTEGER NWORD
      CHARACTER * ( * ) WORD( MXNWRD )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      LOGICAL EXIST              ! True if structure exists
      LOGICAL EXIST2             ! True if SPECVALS exist
      INTEGER I                  ! Temporary integer
      INTEGER IARY               ! ARY identifier
      INTEGER BNDF               ! Base NDF identifier
      INTEGER XNDF               ! NDF identifier
      INTEGER TPNTR( 2 )         ! Temporary array pointer
      INTEGER NELM               ! Array size
      REAL RVALUE                ! Any real parameter value
      DOUBLE PRECISION DVALUE    ! Any double parameter value
      CHARACTER * ( 64 ) LABEL   ! Unused
      CHARACTER * ( 64 ) UNITS   ! Unused
      CHARACTER * ( 64 ) MESSAG  ! Error message string
      CHARACTER * ( NDF__SZTYP ) TYPE( 2 ) ! Data types
      CHARACTER * ( DAT__SZLOC ) TLOC ! Temporary locator

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If type of refractive index or reference frequency to be changed.
      IF ( WORD(2) .EQ. XCMP3 .OR. WORD(2) .EQ. XCMP4 ) THEN

*     See if command parameter is present.
         IF ( NWORD .LT. 3 ) THEN
            STATUS = SAI__ERROR
            MESSAG = 'Error reading parameters.'
            CALL ERR_REP( 'SPD_CZWF_E01', 'EDITEXT: ' //
     :         MESSAG, STATUS )
            GO TO 500
         END IF

*     Repair the parameter.
         CALL CHR_UCASE( WORD(3) )
         IF ( WORD(3) .NE. '_DOUBLE' ) WORD(3) = '_REAL'

*     Set default values in case NDF_XGT0x does not find the structure.
*     FREQREF defaults to bad, but INDEXREFR defaults to 1.
         IF ( WORD(2) .EQ. XCMP3 ) THEN
            DVALUE = 1D0
            RVALUE = 1.
         ELSE
            DVALUE = VAL__BADD
            RVALUE = VAL__BADR
         END IF

*     Now get values if present, and put them with the desired type.
         IF ( WORD(3) .EQ. '_DOUBLE' ) THEN
            CALL NDF_XGT0D( NDF, XNAME, WORD(2), DVALUE, STATUS )
            CALL NDF_XPT0D( DVALUE, NDF, XNAME, WORD(2), STATUS )
         ELSE
            CALL NDF_XGT0R( NDF, XNAME, WORD(2), RVALUE, STATUS )
            CALL NDF_XPT0R( RVALUE, NDF, XNAME, WORD(2), STATUS )
         END IF

*  Else if type of spectroscopic values to be changed.
      ELSE IF ( WORD(2) .EQ. XCMP6 ) THEN

*     See if command parameter is present.
         IF ( NWORD .LT. 3 ) THEN
            STATUS = SAI__ERROR
            MESSAG = 'Error reading parameters.'
            CALL ERR_REP( 'SPD_CZWF_E01', 'EDITEXT: ' //
     :         MESSAG, STATUS )
            GO TO 500
         END IF

*     Repair the parameter.
         CALL CHR_UCASE( WORD(3) )
         IF ( WORD(3) .NE. '_DOUBLE' ) WORD(3) = XT6D

*     If structure exists already, just access and retype.
         CALL DAT_THERE( XLOC, WORD(2), EXIST, STATUS )
         IF ( EXIST ) THEN
            CALL NDF_FIND( XLOC, WORD(2), XNDF, STATUS )
            CALL NDF_STYPE( WORD(3), XNDF, XC6D, STATUS )
            CALL NDF_ANNUL( XNDF, STATUS )

*     Else, set to default (which includes creation).
         ELSE
            CALL SPD_EAEF( NDF, XLOC, WORD(3), STATUS )
         END IF

*  Else if type of spectroscopic widths to be changed.
      ELSE IF ( WORD(2) .EQ. XCMP7 ) THEN

*     See if command parameter is present.
         IF ( NWORD .LT. 3 ) THEN
            STATUS = SAI__ERROR
            MESSAG = 'Error reading parameters.'
            CALL ERR_REP( 'SPD_CZWF_E01', 'EDITEXT: ' //
     :         MESSAG, STATUS )
            GO TO 500
         END IF

*     Repair the parameter.
         CALL CHR_UCASE( WORD(3) )
         IF ( WORD(3) .NE. '_DOUBLE' ) WORD(3) = XT7D

*     If structure exists already, just access and retype.
         CALL DAT_THERE( XLOC, WORD(2), EXIST, STATUS )
         IF ( EXIST ) THEN
            CALL NDF_FIND( XLOC, WORD(2), XNDF, STATUS )
            CALL NDF_STYPE( WORD(3), XNDF, XC7D, STATUS )
            CALL NDF_ANNUL( XNDF, STATUS )

*     Else, set to default (which includes creation).
*     The way to create depends on whether SPECVALS exist.
         ELSE
            CALL DAT_THERE( XLOC, XCMP6, EXIST2, STATUS )
            IF ( EXIST2 ) THEN
               CALL NDF_BASE( NDF, BNDF, STATUS )
               CALL SPD_EAED( BNDF, XLOC, 'READ', WORD(3), LABEL, UNITS,
     :                        TPNTR(1), XNDF, NELM, STATUS )
               CALL SPD_EAFF( BNDF, XLOC, WORD(3),
     :                        %VAL( CNF_PVAL( TPNTR(1) ) ), STATUS )
               CALL NDF_ANNUL( XNDF, STATUS )
               CALL NDF_ANNUL( BNDF, STATUS )
            ELSE
               CALL SPD_EAFH( NDF, XLOC, WORD(3), STATUS )
            END IF
         END IF

*  Else if type of coordinate to be changed.
      ELSE IF ( WORD(2) .EQ. 'COORD' ) THEN

*     See if command parameter is present.
         IF ( NWORD .LT. 3 ) THEN
            STATUS = SAI__ERROR
            MESSAG = 'Error reading parameters.'
            CALL ERR_REP( 'SPD_CZWF_E01', 'EDITEXT: ' //
     :         MESSAG, STATUS )
            GO TO 500
         END IF

*     Repair the parameter.
         CALL CHR_UCASE( WORD(3) )
         IF ( WORD(3) .NE. '_DOUBLE' ) WORD(3) = XT10D

*     If structure exists already, just access and retype.
*     We assume that if the first exists, so does the second. This is an
*     official requirement, but may not be enforced very well.
         CALL DAT_THERE( XLOC, XCMP10, EXIST, STATUS )
         IF ( EXIST ) THEN
            CALL NDF_FIND( XLOC, XCMP10, XNDF, STATUS )
            CALL NDF_STYPE( WORD(3), XNDF, XC10D, STATUS )
            CALL NDF_ANNUL( XNDF, STATUS )
            CALL NDF_FIND( XLOC, XCMP11, XNDF, STATUS )
            CALL NDF_STYPE( WORD(3), XNDF, XC11D, STATUS )
            CALL NDF_ANNUL( XNDF, STATUS )

*     Else, set to default (which includes creation).
         ELSE
            TYPE(1) = WORD(3)
            TYPE(2) = TYPE(1)
            CALL SPD_EAJF( NDF, XLOC, TYPE, STATUS )
         END IF

*  If type of covariance row sums to be changed.
      ELSE IF ( WORD(2) .EQ. XCMP8 ) THEN

*     See if command parameter is present.
         IF ( NWORD .LT. 3 ) THEN
            STATUS = SAI__ERROR
            MESSAG = 'Error reading parameters.'
            CALL ERR_REP( 'SPD_CZWF_E01', 'EDITEXT: ' //
     :         MESSAG, STATUS )
            GO TO 500
         END IF

*     Repair the parameter.
         CALL CHR_UCASE( WORD(3) )
         IF ( WORD(3) .NE. '_DOUBLE' ) WORD(3) = XT8D

*     If structure exists, access and retype.
         CALL DAT_THERE( XLOC, WORD(2), EXIST, STATUS )
         IF ( EXIST ) THEN
            CALL NDF_FIND( XLOC, WORD(2), XNDF, STATUS )
            CALL NDF_STYPE( WORD(3), XNDF, XC8D, STATUS )
            CALL NDF_ANNUL( XNDF, STATUS )

*     Else, report an error.
*     COVRS is not created since its default is non-existence.
         ELSE
            STATUS = SAI__ERROR
            MESSAG = 'Error: The COVRS does not exist ' //
     :         'and must not be created.'
            CALL ERR_REP( 'SPD_CZWF_E01', 'EDITEXT: ' //
     :         MESSAG, STATUS )
            GO TO 500
         END IF

*  If types in result structure to be changed.
      ELSE IF ( WORD(2) .EQ. XCMP9 ) THEN

*     See if command parameters are present and repair them.
         IF ( NWORD .LT. 5 ) THEN
            STATUS = SAI__ERROR
            MESSAG = 'Error reading parameters.'
            CALL ERR_REP( 'SPD_CZWF_E01', 'EDITEXT: ' //
     :         MESSAG, STATUS )
            GO TO 500
         END IF
         DO 1 I = 3, 5
            CALL CHR_UCASE( WORD(I) )
            IF ( WORD(I) .NE. '_DOUBLE' ) WORD(I) = XT9D
 1       CONTINUE

*     Find out if result structure exits.
         CALL DAT_THERE( XLOC, WORD(2), EXIST, STATUS )

*     If result exists, change types.
         IF ( EXIST ) THEN

*        Locate and retype the NDF.
            CALL NDF_FIND( XLOC, WORD(2), XNDF, STATUS )
            CALL NDF_STYPE( WORD(3), XNDF, 'DATA,VARIANCE', STATUS )

*        Locate laboratory frequencies, import array, set its type,
*        annul the ARY identifier, annul the locator.
            CALL NDF_XLOC( XNDF, 'LABFREQ', 'UPDATE', TLOC, STATUS )
            CALL ARY_IMPRT( TLOC, IARY, STATUS )
            CALL ARY_STYPE( WORD(4), IARY, STATUS )
            CALL ARY_ANNUL( IARY, STATUS )

*        Locate mask, import array, set its type, annul the ARY
*        identifier, annul the locator.
            CALL NDF_XLOC( XNDF, 'MASKL', 'UPDATE', TLOC, STATUS )
            CALL ARY_IMPRT( TLOC, IARY, STATUS )
            CALL ARY_STYPE( WORD(5), IARY, STATUS )
            CALL ARY_ANNUL( IARY, STATUS )

*        Locate mask, import array, set its type, annul the ARY
*        identifier, annul the locator.
            CALL NDF_XLOC( XNDF, 'MASKR', 'UPDATE', TLOC, STATUS )
            CALL ARY_IMPRT( TLOC, IARY, STATUS )
            CALL ARY_STYPE( WORD(5), IARY, STATUS )
            CALL ARY_ANNUL( IARY, STATUS )

*        Annul the NDF.
            CALL NDF_ANNUL( XNDF, STATUS )

*     Else (the result structure does not yet exist, we create it).
*     6 and 20 are the default numbers of spectral components and result
*     parameters.
         ELSE
            CALL SPD_FDHF( NDF, XLOC, 6, 20, WORD(3), STATUS )
         END IF

*  Else (unrecognised request to type something).
      ELSE
         STATUS = SAI__ERROR
         MESSAG = 'Error: Invalid TYPE request.'
         CALL ERR_REP( 'SPD_CZWF_E01', 'EDITEXT: ' //
     :      MESSAG, STATUS )
         GO TO 500
      END IF

*  Return.
 500  CONTINUE
      END
