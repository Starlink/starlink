      SUBROUTINE SPD_EAJA( NDF, XLOC, ACCESS, TYPE, LABEL, UNITS,
     :   PNTR, ONDF, NELM, STATUS )
*+
*  Name:
*     SPD_EAJA

*  Purpose:
*     Access coordinates in COORD1/2 or AXIS.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPD_EAJA( NDF, XLOC, ACCESS, TYPE, LABEL, UNITS,
*        PNTR, ONDF, NELM, STATUS )

*  Description:
*     This routine will access arrays of first and second coordinates
*     wherever they are. Mostly this will be from the COORD1 and COORD2
*     structures in the Specdre Extension. However, if those structures
*     do not exist then the pixel centre vectors from the first two
*     non-spectroscopic axes will be accessed instead.
*     Which source was used - and thus whether the mapped centre arrays
*     are one- or multi-dimensional - is signalled to the calling
*     routine by the returned NDF identifiers being valid or NDF__NOID.
*     Both arrays have the same shape, mixtures of vector and NDF cannot
*     occur.
*     If the returned Extension NDF identifiers are valid, then the NDFs
*     will have the same shape, size and bounds as the given main NDF,
*     except that their extent along the spectroscopic axis is only one
*     pixel.
*     If the main NDF was not a base NDF then the Extension NDF will be
*     an NDF section as well.

*  Arguments:
*     NDF = INTEGER (Given)
*        The identifier of the given main NDF.
*     XLOC = CHARACTER * ( * ) (Given)
*        The HDS locator of the Specdre Extension. This should be an
*        extension of the main NDF. If the Specdre Extension does not
*        exist, then this argument must be given as DAT__NOLOC.
*     ACCESS = CHARACTER * ( * ) (Given)
*        The access mode required. This can be 'READ', 'WRITE', or
*        'UPDATE'.
*     TYPE( 2 ) = CHARACTER * ( * ) (Given and Returned)
*        The numeric type for mapping the arrays. Each can be
*        '_REAL', '_DOUBLE', or blank. If given blank and if the array
*        is stored '_DOUBLE', then it is mapped in double precision. If
*        given blank and if the array is not stored '_DOUBLE', then it
*        is mapped '_REAL'. In effect, usually a blank type
*        specification causes the array to be mapped with the stored
*        type. On return, TYPE is '_REAL' or '_DOUBLE' and tells the
*        type actually used for mapping.
*     LABEL( 2 ) = CHARACTER * ( * ) (Given and Returned)
*        The labels that go with the mapped arrays. If taken from the
*        main NDF's AXIS structure, they will be the labels of the axes.
*        If taken from the main NDF's Specdre Extension, they will be
*        the labels of the data components of the NDFs identified by ONDF.
*        If access is read, this argument is a returned argument.
*        If access is write, this argument is a given argument.
*        If access is update and this argument is given blank, this
*        argument is a returned argument. Otherwise this is a given
*        argument.
*     UNITS( 2 ) = CHARACTER * ( * ) (Given and Returned)
*        The units that go with the mapped arrays. If taken from the
*        main NDF's AXIS structure, they will be the units of the axes.
*        If taken from the main NDF's Specdre Extension, they will be
*        the units of the data components of the NDFs identified by ONDF.
*        If access is read, this argument is a returned argument.
*        If access is write, this argument is a given argument.
*        If access is update and this argument is given blank, this
*        argument is a returned argument. Otherwise this is a given
*        argument.
*     PNTR( 2 ) = INTEGER (Returned)
*        The pointer to which the centre arrays were mapped.
*     ONDF( 2 ) = INTEGER (Returned)
*        The identifiers of the NDFs, which are components of the
*        Specdre Extension. These will be returned as NDF__NOID if no
*        NDFs were accessed and the array was mapped from the main
*        NDF's AXIS structure.
*     NELM( 2 ) = INTEGER (Returned)
*        The numbers of elements in the mapped arrays. If taken from the
*        main NDF's AXIS structure, they will be the lengths of the axes.
*        If taken from the main NDF's Specdre Extension, they will be
*        the sizes of the NDFs identified by ONDF.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     This routine recognises the Specdre Extension v. 1.1.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     03 Aug 1994 (hme):
*        Original version, adapted from SPEEA.
*     {enter_changes_here}

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

*  Arguments Given:
      INTEGER NDF
      CHARACTER * ( * ) XLOC
      CHARACTER * ( * ) ACCESS

*  Arguments Given and Returned:
      CHARACTER * ( * ) TYPE(  2 )
      CHARACTER * ( * ) LABEL( 2 )
      CHARACTER * ( * ) UNITS( 2 )

*  Arguments Returned:
      INTEGER PNTR( 2 )
      INTEGER ONDF( 2 )
      INTEGER NELM( 2 )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      LOGICAL XTHERE             ! True if XLOC valid (Extension there)
      LOGICAL THERE1             ! True if POSIT1 there
      LOGICAL THERE2             ! True if POSIT2 there

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Find out whether COORD1/2 exist.
      XTHERE = ( XLOC .NE. DAT__NOLOC )
      THERE1 = .FALSE.
      THERE2 = .FALSE.
      IF ( XTHERE ) CALL DAT_THERE( XLOC, XCMP10, THERE1, STATUS )
      IF ( XTHERE ) CALL DAT_THERE( XLOC, XCMP11, THERE2, STATUS )

*  If COORD1/2 do exist, get information from there.
*  They are NDFs, which are components of XLOC.
      IF ( THERE1 .AND. THERE2 ) THEN
         CALL SPD_EAJD( NDF, XLOC, ACCESS, TYPE, LABEL, UNITS,
     :      PNTR, ONDF, NELM, STATUS )

*  Else, get information from main NDF's AXIS structure.
      ELSE
         ONDF(1) = NDF__NOID
         ONDF(2) = NDF__NOID
         CALL SPD_EAJC( NDF, ACCESS, TYPE, LABEL, UNITS,
     :      PNTR, NELM, STATUS )
      END IF

*  Return.
 500  CONTINUE
      END
