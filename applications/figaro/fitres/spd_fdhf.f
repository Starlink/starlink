      SUBROUTINE SPD_FDHF( NDF, XLOC, NCOMP, TNPAR, TYPE, STATUS )
*+
*  Name:
*     SPD_FDHF

*  Purpose:
*     Create result structure.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPD_FDHF( MNDF, XLOC, NCOMP, TNPAR, TYPE, STATUS )

*  Description:
*     This routine creates a new result NDF in the Specdre Extension.
*     The Extension must already exist, the result structure must not
*     exist. If the given NDF is a section rather than a base NDF, then
*     the appropriate base NDF will be used to create the result NDF.
*     The new structure is filled with defaults values as specified in
*     SUN/140.

*  Arguments:
*     MNDF = INTEGER (Given)
*        The identifier of the given main NDF.
*     XLOC = CHARACTER * ( * ) (Given)
*        The HDS locator of the Specdre Extension. This should be an
*        extension of the main NDF.
*     NCOMP = INTEGER (Given)
*        The number of components the result structure caters for.
*     TNPAR = INTEGER (Given)
*        The total number of parameters the result structure caters for.
*     TYPE( 3 ) = CHARACTER * ( * ) (Given)
*        The numeric types for creating the result structure. These are
*        for (1) data and variance, (2) the LABFREQ extension, (3) the
*        MASKL and MASKR extensions. Each type can be '_REAL' or
*        '_DOUBLE'.
*     STATUS = INTEGER (Given and Returned)
*        The global status. This is set
*        -  if the structure to be created already exists,
*        -  if a requested data type is invalid,
*        -  if the requested shape is invalid,

*  Notes:
*     This routine recognises Specdre Extension v. 0.7.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     02 Mar 1992 (hme):
*        Original version (SPAAV).
*     02 Apr 1992 (hme):
*        Use base NDF to derive bounds of created result NDF (SPAAV).
*     19 Jun 1992 (hme):
*        Adapt according to SPE-routine convention.
*     25 Feb 1994 (hme):
*        Rename from SPEHF.
*     2005 June 1 (MJC):
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
      INTEGER NCOMP
      INTEGER TNPAR
      CHARACTER * ( * ) TYPE( 3 )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER CSIZE              ! Length of a standard string
      PARAMETER ( CSIZE = 32 )

*  Local Variables:
      LOGICAL EXIST              ! True if structure exists
      INTEGER I, J               ! Temporary integers
      INTEGER SPAXIS             ! The spectroscopic axis
      INTEGER PNTR( 2 )          ! Array pointers
      INTEGER PLACE              ! NDF placeholder
      INTEGER XNDF               ! Structure's NDF identifier
      INTEGER BNDF               ! Main's base NDF indentifier
      INTEGER NDIM               ! Number of axes of NDF
      INTEGER LBND( NDF__MXDIM ) ! Lower bounds of NDF
      INTEGER UBND( NDF__MXDIM ) ! Upper bounds of NDF
      INTEGER LBNDR( NDF__MXDIM ) ! Lower bounds of result NDF
      INTEGER UBNDR( NDF__MXDIM ) ! Upper bounds of result NDF
      CHARACTER * ( DAT__SZLOC ) RLOC ! Locator in result structure

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check that Extension NDF does not exist.
*  One allows oneself an exception here in that one returns rather than
*  goes to 500. This needs be, because we do not want to delete a proper
*  NDF.
      CALL DAT_THERE( XLOC, XCMP9, EXIST, STATUS )
      IF ( EXIST ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'SPE_EXISTS', 'SPD_FDHF: Error:' //
     :      'The Extension result structure already exists.', STATUS )
         RETURN
      END IF

*  Check that requested type is valid.
      DO 1 I = 1, 3
         IF ( TYPE(I) .NE. '_DOUBLE' .AND. TYPE(I) .NE. '_REAL' ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'SPE_INVTYP',
     :         'SPD_FDHF: Error creating result structure. ' //
     :         'A requested data type is invalid.', STATUS )
            GO TO 500
         END IF
 1    CONTINUE

*  Check number of spectral components and result parameters.
*  Neither must be 0 or less.
      IF ( NCOMP .LE. 0 .OR. TNPAR .LE. 0 ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'SPE_INCSHP', 'SPD_FDHF: Error creating ' //
     :      'result structure: The requested number of components ' //
     :      'or parameters is zero.', STATUS )
         GO TO 500
      END IF

*  Find the base of the main NDF and its bounds.
      CALL NDF_BASE(   NDF, BNDF, STATUS )
      CALL NDF_BOUND( BNDF, NDF__MXDIM, LBND, UBND, NDIM, STATUS )
      CALL NDF_ANNUL( BNDF, STATUS )
      IF ( NDIM .GE. NDF__MXDIM ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'SPE_INCSHP', 'SPD_FDHF: Error creating ' //
     :      'result structure: The structure would have too many ' //
     :      'dimensions.', STATUS )
         GO TO 500
      END IF

*  Get spectroscopic axis number.
      CALL SPD_EABA( NDF, .TRUE., SPAXIS, STATUS )

*  Derive the shape of the result structure.
*  The first axis counts the parameters, the second axis exists only
*  for consistency with TWODSPEC and has length 1 here.
*  The third to NDIM+1st axes are the non-spectrocopic axes of the
*  main data array.
      LBNDR(1) = 1
      UBNDR(1) = TNPAR
      LBNDR(2) = 1
      UBNDR(2) = 1
      J = 2
      DO 2 I = 1, NDIM
         IF ( I .NE. SPAXIS ) THEN
            J = J + 1
            LBNDR(J) = LBND(I)
            UBNDR(J) = UBND(I)
         END IF
 2    CONTINUE

*  Create the Extension NDF.
      CALL NDF_PLACE( XLOC, XCMP9, PLACE, STATUS )
      CALL NDF_NEW( TYPE(1), NDIM+1, LBNDR, UBNDR, PLACE, XNDF, STATUS )

*  Map the data, if only to initialise them to bad and make them
*  defined.
      CALL NDF_MAP( XNDF, 'DATA,VARIANCE', TYPE(1), 'WRITE/BAD', PNTR,
     :   J, STATUS )
      CALL NDF_UNMAP( XNDF, 'DATA,VARIANCE', STATUS )

*  Check status.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'SPE_INVNDF',
     :      'SPD_FDHF: Error creating result structure ' //
     :      'in Specdre Extension.', STATUS )
         GO TO 500
      END IF

*  .RESULTS.MORE.LINENAME is a _CHAR*32 vector. There is one element for
*  each spectral component.
      CALL NDF_XNEW( XNDF, XC9C1, XT9C1, 1, NCOMP, RLOC, STATUS )
      CALL DAT_MAPC( RLOC, 'WRITE', 1, NCOMP, PNTR(1), STATUS )
      CALL SPD_FDAAC( 1, NCOMP, %VAL( CNF_PVAL(PNTR(1)) ),
     :                'unidentified component', STATUS,
     :                %VAL(CNF_CVAL(CSIZE)) )
      CALL DAT_ANNUL( RLOC, STATUS )

*  .RESULTS.MORE.LABFREQ is a _REAL or _DOUBLE vector. There is one
*  element for each spectral component.
      CALL NDF_XNEW( XNDF, XC9C2, TYPE(2), 1, NCOMP, RLOC, STATUS )
      IF ( TYPE(2) .EQ. '_DOUBLE' ) THEN
         CALL DAT_MAPD( RLOC, 'WRITE', 1, NCOMP, PNTR(1), STATUS )
         CALL SPD_FDAAD( 1, NCOMP, %VAL( CNF_PVAL(PNTR(1)) ), VAL__BADD,
     :                   STATUS )
      ELSE
         CALL DAT_MAPR( RLOC, 'WRITE', 1, NCOMP, PNTR(1), STATUS )
         CALL SPD_FDAAR( 1, NCOMP, %VAL( CNF_PVAL(PNTR(1)) ), VAL__BADR,
     :                   STATUS )
      END IF
      CALL DAT_ANNUL( RLOC, STATUS )

*  .RESULTS.MORE.COMPTYPE is a _CHAR*32 vector. There is one element
*  for each spectral component.
      CALL NDF_XNEW( XNDF, XC9C3, XT9C3, 1, NCOMP, RLOC, STATUS )
      CALL DAT_MAPC( RLOC, 'WRITE', 1, NCOMP, PNTR(1), STATUS )
      CALL SPD_FDAAC( 1, NCOMP, %VAL( CNF_PVAL(PNTR(1)) ),
     :                'unknown function', STATUS,
     :                %VAL(CNF_CVAL(CSIZE)) )
      CALL DAT_ANNUL( RLOC, STATUS )

*  .RESULTS.MORE.NPARA is an _INTEGER vector. There is one element for
*  each spectral component. By default all components are allocated the
*  same - and highest possible - number of parameters.
      CALL NDF_XNEW( XNDF, XC9C4, XT9C4, 1, NCOMP, RLOC, STATUS )
      CALL DAT_MAPI( RLOC, 'WRITE', 1, NCOMP, PNTR(1), STATUS )
      I = INT( TNPAR / NCOMP )
      CALL SPD_FDAAI( 1, NCOMP, %VAL( CNF_PVAL(PNTR(1)) ), I, STATUS )
      CALL DAT_ANNUL( RLOC, STATUS )

*  .RESULTS.MORE.MASKL is a _REAL or _DOUBLE vector. There is one
*  element for each spectral component.
      CALL NDF_XNEW( XNDF, XC9C5, TYPE(3), 1, NCOMP, RLOC, STATUS )
      IF ( TYPE(3) .EQ. '_DOUBLE' ) THEN
         CALL DAT_MAPD( RLOC, 'WRITE', 1, NCOMP, PNTR(1), STATUS )
         CALL SPD_FDAAD( 1, NCOMP, %VAL( CNF_PVAL(PNTR(1)) ), VAL__BADD,
     :                   STATUS )
      ELSE
         CALL DAT_MAPR( RLOC, 'WRITE', 1, NCOMP, PNTR(1), STATUS )
         CALL SPD_FDAAR( 1, NCOMP, %VAL( CNF_PVAL(PNTR(1)) ), VAL__BADR,
     :                   STATUS )
      END IF
      CALL DAT_ANNUL( RLOC, STATUS )

*  .RESULTS.MORE.MASKR is a _REAL or _DOUBLE vector. There is one
*  element for each spectral component.
      CALL NDF_XNEW( XNDF, XC9C6, TYPE(3), 1, NCOMP, RLOC, STATUS )
      IF ( TYPE(3) .EQ. '_DOUBLE' ) THEN
         CALL DAT_MAPD( RLOC, 'WRITE', 1, NCOMP, PNTR(1), STATUS )
         CALL SPD_FDAAD( 1, NCOMP, %VAL( CNF_PVAL(PNTR(1)) ), VAL__BADD,
     :                   STATUS )
      ELSE
         CALL DAT_MAPR( RLOC, 'WRITE', 1, NCOMP, PNTR(1), STATUS )
         CALL SPD_FDAAR( 1, NCOMP, %VAL( CNF_PVAL(PNTR(1)) ), VAL__BADR,
     :                   STATUS )
      END IF
      CALL DAT_ANNUL( RLOC, STATUS )

*  .RESULTS.MORE.PARATYPE is a _CHAR*32 vector. There is one element
*  for each result parameter.
      CALL NDF_XNEW( XNDF, XC9P1, XT9P1, 1, TNPAR, RLOC, STATUS )
      CALL DAT_MAPC( RLOC, 'WRITE', 1, TNPAR, PNTR(1), STATUS )
      CALL SPD_FDAAC( 1, TNPAR, %VAL( CNF_PVAL(PNTR(1)) ),
     :                'unknown parameter', STATUS,
     :                %VAL(CNF_CVAL(CSIZE)) )
      CALL DAT_ANNUL( RLOC, STATUS )

*  Check status.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'SPAAV_CRARY',
     :      'SPD_FDHF: Error creating a vector extension ' //
     :      'in result structure.',
     :      STATUS )
         GO TO 500
      END IF

*  Tidy up.
 500  CONTINUE

*  Release result NDF, or delete it if an error occured.
      IF ( STATUS .EQ. SAI__OK ) THEN
         CALL NDF_ANNUL( XNDF, STATUS )
      ELSE
         CALL NDF_DELET( XNDF, STATUS )
      END IF

*  Return.
      END
