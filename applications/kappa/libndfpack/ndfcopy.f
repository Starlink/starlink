      SUBROUTINE NDFCOPY( STATUS )
*+
*  Name:
*     NDFCOPY

*  Purpose:
*     Copies an NDF (or NDF section) to a new location.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL NDFCOPY( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application copies an NDF to a new location.  By supplying an
*     NDF section as input it may be used to extract a subset, or to
*     change the size or dimensionality of an NDF.  A second NDF may
*     also be supplied to act as a shape template, and hence to define
*     the region of the first NDF which is to be copied.
*
*     Any unused space will be eliminated by the copying operation
*     performed by this routine, so it may be used as a way of
*     compressing NDF structures from which components have been
*     deleted.  This ability also makes NDFCOPY a useful alternative to
*     SETBOUND in cases where an NDF's size is to be reduced.

*  Usage:
*     ndfcopy in out

*  ADAM Parameters:
*     IN = NDF (Read)
*        The input NDF (or section) which is to be copied.
*     LIKE = NDF (Read)
*        This parameter may be used to supply an NDF to be used as a
*        shape template during the copying operation.  If such a
*        template is supplied, then its shape will be used to select a
*        matching section from the input NDF before copying takes
*        place.  By default, no template will be used and the shape of
*        the output NDF will therefore match that of the input NDF (or
*        NDF section). [!]
*     OUT = NDF (Write)
*        The output NDF data structure.
*     TITLE = LITERAL (Read)
*        A title for the output NDF.  A null value (the default) will
*        cause the title of the NDF supplied for parameter IN to be
*        used instead. [!]

*  Examples:
*     ndfcopy infile outfile
*        Copies the contents of the NDF structure infile to the new
*        structure outfile.  Any unused space will be eliminated during
*        the copying operation.
*     ndfcopy in=data1(3:40,-3:17) out=data2 title="Extracted section"
*        Copies the section (3:40,-3:17) of the NDF called data1 to a
*        new NDF called data2.  The output NDF is assigned the new title
*        "Extracted section", which replaces the title derived from the
*        input NDF.
*     ndfcopy galaxy newgalaxy like=oldgalaxy
*        Copies a section of the NDF called galaxy to form a new NDF
*        called newgalaxy.  The section which is copied will correspond
*        in shape with the template oldgalaxy.  Thus, after the copying
*        operation, both newgalaxy and oldgalaxy will have the same
*        pixel-index bounds.
*     ndfcopy aa(20~11,20~11) bb like=aa
*        Copies from the NDF section consisting of an 11x11 pixel
*        region of aa centred on pixel (20,20), into a new NDF called
*        bb.  The shape of the region copied is made to match the
*        original shape of aa.  The effect is to extract the selected
*        square region of pixels into a new NDF of the same shape as
*        the original, setting the surrounding region to the bad-pixel
*        value.

*  Implementation Status:
*     If present, an NDF's TITLE, LABEL, UNITS, DATA, VARIANCE,
*     QUALITY, AXIS WCS and HISTORY components are copied by this routine,
*     together with all extensions.  The output NDF's title may be
*     modified, if required, by specifying a new value via the TITLE
*     parameter.

*  Related Applications:
*     KAPPA: SETBOUND; Figaro: ISUBSET.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     MJC: Malcolm J. Currie (STARLINK)
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     27-FEB-1991 (RFWS):
*        Original version.
*     19-MAR-1991 (RFWS):
*        Added the LIKE parameter to allow the use of a shape template.
*     22-MAR-1991 (RFWS):
*        Added the TITLE parameter.
*     1995 April 24 (MJC):
*        Made usage and examples lowercase.  Added Related Applications.
*     5-JUN-1998 (DSB):
*        Added propagation of the WCS component.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PAR_ERR'          ! PAR_ error codes
      INCLUDE 'NDF_PAR'          ! NDF_ public constants

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER LBND( NDF__MXDIM ) ! Template NDF lower bounds
      INTEGER NDF1               ! Input NDF identifier
      INTEGER NDF2               ! Template NDF identifier
      INTEGER NDF3               ! Output NDF identifier
      INTEGER NDFT               ! Temporary NDF identifier
      INTEGER NDIM               ! Number of template dimensions
      INTEGER UBND( NDF__MXDIM ) ! Template NDF upper bounds

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Begin an NDF context.
      CALL NDF_BEGIN

*  Obtain the input NDF.
      CALL LPG_ASSOC( 'IN', 'READ', NDF1, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Defer error reporting and attempt to obtain a second NDF to act as a
*  shape template.
         CALL ERR_MARK
         CALL LPG_ASSOC( 'LIKE', 'READ', NDF2, STATUS )

*  Interpret a null value as indicating that a template should not be
*  used.
         IF ( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_ANNUL( STATUS )

*  If a template was supplied, then obtain its bounds and select a
*  matching section from the input NDF.  Annul the original input NDF
*  identifier and replace it with the section identifier.
         ELSE
            CALL NDF_BOUND( NDF2, NDF__MXDIM, LBND, UBND, NDIM, STATUS )
            CALL NDF_SECT( NDF1, NDIM, LBND, UBND, NDFT, STATUS )
            CALL NDF_ANNUL( NDF1, STATUS )
            NDF1 = NDFT
         END IF
         CALL ERR_RLSE
      END IF

*  Copy the input NDF (or section) to create the output NDF.
      CALL LPG_PROP( NDF1,
     :               'Title,Label,Units,Data,Variance,Quality,Axis,' //
     :               'History,WCS', 'OUT', NDF3, STATUS )

*  Obtain a new title for the output NDF.
      CALL NDF_CINP( 'TITLE', NDF3, 'Title', STATUS )

*  End the NDF context.
      CALL NDF_END( STATUS )

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'NDFCOPY_ERR',
     :     'NDFCOPY: Error copying an NDF to a new location.', STATUS )
      END IF

      END
