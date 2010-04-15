      SUBROUTINE FLAIRCOMP( STATUS )
*+
*  Name:
*     FLAIRCOMP

*  Purpose:
*     Compresses a FLAIR frame to give a weight vector.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL FLAIRCOMP( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application takes a FLAIR frame stored in an NDF and
*     compresses it along the y axis, normalises the compressed array
*     by its mean value, and then finds the minima in the values and
*     set these to the bad value.  Thus it provides the weights for an
*     optimal extraction.  It reports the number of fibres found in the
*     NDF.

*     This assumes stability (x positions of the fibres do not move),
*     and vertical orientation of the fibres.  These are satisfied by
*     FLAIR (Parker, private communication).

*  Usage:
*     flaircomp in out

*  ADAM Parameters:
*     IN = NDF (Read)
*        The input two-dimensional NDF.  This should be the co-added
*        arc or sky flat-field frames, so that the compressed array
*        gives the instrumental response of the detector system.
*     OUT = NDF (Write)
*        The vector of weights to use during optimal extraction.
*     TITLE = LITERAL (Read)
*        Value for the title of the output NDF.  A null (!) propagates
*        the title from input NDF to the output. [!]

*  [examples]
*  [optional_A_task_items]...
*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     ACD: A C Davenhall (Edinburgh)
*     {enter_new_authors_here}

*  History:
*     1993 April 5 (MJC):
*        Original version.
*     1998 October 15 (MJC):
*        Use PSX to obtain workspace.  Propagate the title by default.
*     1998 October 27 (ACD)
*        Replaced explict directory specification for INCLUDE files
*        with the standard upper-case softlinks.  Also removed
*        continuing a string constant across a continuation line.
*     2005 June 1 (MJC):
*        Use CNF_PVAL for pointers to mapped data.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! Data-system constants
      INCLUDE 'PAR_PAR'
      INCLUDE 'NDF_PAR'          ! NDF_ constants
      INCLUDE 'MSG_PAR'          ! MSG_ constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER NDIM               ! Dimensionality required of input NDF
      PARAMETER( NDIM = 2 )

*  Local Variables:
      INTEGER DIM( NDF__MXDIM )  ! NDF dimensions
      INTEGER ELI                ! Number of mapped input-array elements
      INTEGER ELO                ! Number of mapped output-array
                                 ! elements
      INTEGER NDFI               ! Identifier for input NDF
      INTEGER NDFO               ! Identifier for output NDF
      INTEGER NDIMS              ! Number of dimensions in input NDF
      INTEGER NFIBRE             ! Number of fibres found
      INTEGER PNTRI( 1 )         ! Pointer for mapped input array
      INTEGER PNTRO( 1 )         ! Pointer for mapped output array
      INTEGER SDIM( NDF__MXDIM ) ! Significant NDF dimensions
      INTEGER WPNTR              ! Mapped workspace pointer

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Begin an NDF context.
      CALL NDF_BEGIN

*  Obtain the input NDF.
      CALL NDF_ASSOC( 'IN', 'READ', NDFI, STATUS )

*  Find whether or not there are but two significant dimensions and
*  which ones they are.
      CALL KPG1_SGDIM( NDFI, NDIM, SDIM, STATUS )

*  Exit if an error occurred.  This is needed because the significant
*  dimensions are used as array indices.
      IF ( STATUS .NE. SAI__OK ) GOTO 999

*  Determine its dimensions (note that only two significant dimensions
*  can be accommodated).  Then ignore non-significant dimensions.
      CALL NDF_DIM( NDFI, SDIM( NDIM ), DIM, NDIMS, STATUS )
      DIM( 1 ) = DIM( SDIM( 1 ) )
      DIM( 2 ) = DIM( SDIM( 2 ) )

*  Create an output NDF.
      CALL NDF_CREAT( 'OUT', '_REAL', 1, 1, DIM( 1 ), NDFO, STATUS )

*  Map the input and output data arrays.
      CALL NDF_MAP( NDFI, 'Data', '_REAL', 'Read', PNTRI, ELI, STATUS )
      CALL NDF_MAP( NDFO, 'Data', '_REAL', 'Write', PNTRO, ELO, STATUS )

*  Obtain some work space for the calculations.
      CALL PSX_CALLOC( DIM( 1 ), '_REAL', WPNTR, STATUS )

*  Generate the weight array.
      CALL FLA_WEIGH( DIM( 1 ), DIM( 2 ),
     :                %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                %VAL( CNF_PVAL( WPNTR ) ), NFIBRE, STATUS )

*  Report the number of fibres.
      CALL MSG_SETI( 'NFIB', NFIBRE )
      CALL MSG_OUTIF( MSG__NORM, 'NFIBRES',
     :                '^NFIB fibres located.', STATUS )

*  Release the workspace array.
      CALL PSX_FREE( WPNTR, STATUS )

*  Obtain a title and assign it to the output NDF.  A null results in
*  the output title being the same as the input title.
      CALL NDF_CINP( 'TITLE', NDFO, 'Title', STATUS )

*  End the NDF context.
  999 CONTINUE
      CALL NDF_END( STATUS )

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'FLAIRCOMP_ERR',
     :     'FLAIRCOMP: Unable to generate the weight array for '/
     :     /'the FLAIR NDF.', STATUS )
      END IF

      END
