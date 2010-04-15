      SUBROUTINE FLAIREXT( STATUS )
*+
*  Name:
*     FLAIREXT

*  Purpose:
*     Optimally extracts spectra from a FLAIR NDF to form a new NDF.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL FLAIREXT( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application takes a FLAIR frame stored in an NDF with the
*     dispersion along the y axis, and extracts the individual spectra
*     using an optimal extraction.  It stores the spectra in an output
*     two-dimensional NDF, configured such that the dispersion is along
*     x axis, and wavelength increases with pixel index, and each
*     spectrum occupies one line.

*     This assumes stability (x positions of the fibres do not move),
*     and vertical orientation of the fibres.  These are satisfied by
*     FLAIR (Parker, private communication).

*  Usage:
*     flairext in profile out fibres

*  ADAM Parameters:
*     FIBRES = _INTEGER (Read)
*        The number of fibres to extract.  This must be in the range
*        1 to 92. [92]
*     IN = NDF (Read)
*        A list of the input two-dimensional NDFs containing FLAIR
*        spectra to be extracted.
*     PROFILE = NDF (Write)
*        The vector of weights to use during optimal extraction, as
*        derived from FLAIRCOMP.
*     OUT = NDF (Write)
*        The list of the two-dimensional NDF containing the extracted
*        FLAIR spectra.  There should be as many files in this list as
*        for parameter IN.  The nth item in this list will be the
*        extracted spectra for the nth file in IN.
*     TITLE = LITERAL (Read)
*        Value for the title of the output NDF.  A null value (!) will
*        cause the title of the input NDF to be used. [!]

*  [examples]
*  [optional_A_task_items]...
*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     ACD: A C Davenhall (Edinburgh)
*     {enter_new_authors_here}

*  History:
*     1993 April 7 (MJC):
*        Original version.
*     1998 October 15 (MJC):
*        Use GRP for NDG, and use softlinks to include files.
*     1998 October 27 (ACD)
*        Removed continuing a string constant across a continuation line.
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
      INCLUDE 'DAT_PAR'          ! Data-system constants
      INCLUDE 'NDF_PAR'          ! NDF_ constants
      INCLUDE 'GRP_PAR'          ! GRP_ constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER MAXFIB             ! Maximum number of fibres
      PARAMETER( MAXFIB = 92 )

      INTEGER NDIM               ! Dimensionality required of input and
      PARAMETER( NDIM = 2 )      ! output spectra NDF

*  Local Variables:
      INTEGER DIM( NDF__MXDIM )  ! Input-NDF dimensions
      INTEGER ELI                ! Number of mapped input-array elements
      INTEGER ELO                ! Number of mapped output-array
                                 ! elements
      INTEGER ELP                ! Number of mapped profile elements
      INTEGER IFILE              ! Loop counter
      INTEGER IGROUP             ! GRP identifier for input NDFs
      CHARACTER * ( 255 ) INAME  ! Name of an input NDF
      INTEGER IPLACE             ! Input NDF placeholder
      INTEGER NDFI               ! Identifier for input NDF
      INTEGER NDFO               ! Identifier for output NDF
      INTEGER NDFP               ! Identifier for profile NDF
      INTEGER NDIMS              ! Number of dimensions in input NDF
      INTEGER NFIBRE             ! Number of fibres to extract
      INTEGER NONDF              ! Number of input or output NDFs
      INTEGER OGROUP             ! GRP identifier for output NDFs
      INTEGER OLBND( NDIM )      ! Lower bounds of output NDF
      CHARACTER * ( 255 ) ONAME  ! Name of an output NDF
      INTEGER OPLACE             ! Output NDF placeholder
      INTEGER OUBND( NDIM )      ! Upper bounds of output NDF
      INTEGER PDIM( NDF__MXDIM ) ! Profile-NDF dimensions
      INTEGER PNTRI( 2 )         ! Pointers for mapped input arrays
      INTEGER PNTRO( 2 )         ! Pointers for mapped output arrays
      INTEGER PNTRP( 1 )         ! Pointer for mapped profile array
      INTEGER SDIM( NDF__MXDIM ) ! Significant input NDF dimensions
      INTEGER SPDIM( NDF__MXDIM ) ! Significant profile NDF dimensions

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the number of fibres to extract.
      CALL PAR_GDR0I( 'FIBRES', MAXFIB, 1, MAXFIB, .TRUE., NFIBRE,
     :                STATUS )

*  Begin an NDF context.
      CALL NDF_BEGIN

*  Obtain the profile NDF.
*  =======================
      CALL NDF_ASSOC( 'PROFILE', 'READ', NDFP, STATUS )

*  Find whether or not there is but one significant dimension and
*  which ones they are.
      CALL KPG1_SGDIM( NDFP, 1, SPDIM, STATUS )

*  Exit if an error occurred.
      IF ( STATUS .NE. SAI__OK ) GOTO 999

*  Determine its dimensions (note that only two significant dimensions
*  can be accommodated).  Then ignore non-significant dimensions.
      CALL NDF_DIM( NDFP, SPDIM( 1 ), PDIM, NDIMS, STATUS )
      PDIM( 1 ) = PDIM( SPDIM( 1 ) )

*  Map the profile array.
      CALL NDF_MAP( NDFP, 'Data', '_REAL', 'Read', PNTRP, ELP, STATUS )

*  Obtain lists of the input and output NDFs.
*  ==========================================

*  The wildcarded lists are returned in two GRP groups.  They have
*  the same size, otherwise the STATUS will be bad.
      CALL FLA_NWILD( 'IN', 'OUT', IGROUP, OGROUP, NONDF, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 998

*  Process each input NDF.
*  =======================

*  Loop for each NDF.
      DO IFILE = 1, NONDF

*  Obtain the input NDF.
*  =====================

*  Find the input NDF name.
         CALL GRP_GET( IGROUP, IFILE, 1, INAME, STATUS )

*  Find the output NDF name.
         CALL GRP_GET( OGROUP, IFILE, 1, ONAME, STATUS )

*  Open NDF.
         CALL NDF_OPEN( DAT__ROOT, INAME, 'READ', 'OLD', NDFI, IPLACE,
     :                  STATUS )

*  Find whether or not there are but two significant dimensions and
*  which ones they are.
         CALL KPG1_SGDIM( NDFI, NDIM, SDIM, STATUS )

*  Exit if an error occurred.  This is needed because the significant
*  dimensions are used as array indices.
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL NDF_ANNUL( NDFI, STATUS )
            GOTO 998
         END IF

*  Determine its dimensions (note that only two significant dimensions
*  can be accommodated).  Then ignore non-significant dimensions.
         CALL NDF_DIM( NDFI, SDIM( NDIM ), DIM, NDIMS, STATUS )
         DIM( 1 ) = DIM( SDIM( 1 ) )
         DIM( 2 ) = DIM( SDIM( 2 ) )

*  Check that the profile length matches the first dimension of the
*  input NDF.  Report an error, and tidy the NDF identifier.  Note that
*  NDG_ANNUL is used.
         IF ( PDIM( 1 ) .NE. DIM( 1 ) ) THEN
            STATUS = SAI__ERROR
            CALL NDF_MSG( 'NDF', NDFI )
            CALL NDF_MSG( 'PROF', NDFP )
            CALL ERR_REP( 'FLAIREXT',
     :        'FLAIREXT: The length of the profile NDF ^PROF does not '/
     :        /'match the first dimension of FLAIR NDF ^NDF.', STATUS )
            CALL NDF_ANNUL( NDFI, STATUS )

            GOTO 998
         END IF

*  Create an output NDF by propagating the input.  This preserves items
*  like the title and history.
         CALL NDF_OPEN( DAT__ROOT, ONAME, 'WRITE', 'NEW', NDFO, OPLACE,
     :                  STATUS )
         CALL NDF_SCOPY( NDFI, 'Axis,Quality,WCS', OPLACE, NDFO,
     :                   STATUS )

*  Next change the shape of the output NDF.
         OLBND( 1 ) = 1
         OUBND( 1 ) = DIM( 2 )
         OLBND( 2 ) = 1
         OUBND( 2 ) = NFIBRE
         CALL NDF_SBND( NDIM, OLBND, OUBND, NDFO, STATUS )

*  Map the input and output data and variance arrays.
         CALL NDF_MAP( NDFI, 'Data,Variance', '_REAL', 'Read', PNTRI,
     :                 ELI, STATUS )
         CALL NDF_MAP( NDFO, 'Data,Variance', '_REAL', 'Write', PNTRO,
     :                 ELO, STATUS )

*  Generate the weight array.
         CALL FLA_OPEXT( DIM( 1 ), DIM( 2 ),
     :                   %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                   %VAL( CNF_PVAL( PNTRI( 2 ) ) ),
     :                   %VAL( CNF_PVAL( PNTRP( 1 ) ) ), NFIBRE,
     :                   %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                   %VAL( CNF_PVAL( PNTRO( 2 ) ) ),
     :                   STATUS )

*  Obtain a new title for the output NDF.
         CALL NDF_CINP( 'TITLE', NDFO, 'Title', STATUS )

*  Tidy the identifiers.
         CALL NDF_ANNUL( NDFI, STATUS )
         CALL NDF_ANNUL( NDFO, STATUS )
      END DO

*  Tidy the GRP groups.
  998 CONTINUE
      CALL GRP_DELET( IGROUP, STATUS )
      CALL GRP_DELET( OGROUP, STATUS )

*  End the NDF context.
  999 CONTINUE
      CALL NDF_END( STATUS )

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'FLAIREXT_ERR',
     :     'FLAIREXT: Unable to generate the weight array for '/
     :     /'the FLAIR NDF.', STATUS )
      END IF

      END
