      SUBROUTINE POLIMPX( STATUS )
*+
*  Name:
*     POLIMPX

*  Purpose:
*     Copies FITS keyword values into the POLPACK extension.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL POLIMPX( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application is not for general use. It is a version of POLIMP
*     which is used within the polimp.sh script to import POLPACK
*     information during on-thr-fly data conversion. In this context, it
*     is known that only a single input file will be specified, and that
*     it will be a native NDF. We can therefore make big speed gains by
*     using NDF directtly instead of indirectly through the NDG library.
*
*     The default import table described in POLIMP is used. Nothing is
*     reported on the screen.

*  Usage:
*     polimpx in table

*  ADAM Parameters:
*     IN = NDF (Read)
*        The input (native) NDF.

*  Notes:
*     -  Any existing values in the POLPACK extension are deleted before
*     processing the supplied control table.
*     -  A new Frame is added to the WCS component of each NDF and is given the
*     Domain "POLANAL". This Frame is formed by rotating the grid co-ordinate
*     Frame so that the first axis is parallel to the analyser axis. The
*     angle of rotation is given by the ANGROT value and defaults to zero
*     if ANGROT is not specified in the control table. As of POLPACK V2.0,
*     the ANGROT value is no longer stored explicitly in the POLPACK
*     extension; its value is deduced from the POLANAL Frame in the WCS
*     component.

*  Copyright:
*     Copyright (C) 1998 Central Laboratory of the Research Councils

*  Authors:
*     DSB: David Berry (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     6-MAY-1999 (DSB):
*        Original version.
*     22-SEP-2004 (TIMJ):
*        Use CNF_PVAL
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! HDS/DAT parameters
      INCLUDE 'GRP_PAR'          ! GRP parameters
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( DAT__SZLOC ) CELLOC ! Locator to array cell
      CHARACTER * ( DAT__SZLOC ) POLLOC ! Locator to POLPACK extension
      CHARACTER * ( DAT__SZLOC ) FITLOC ! Locator to FITS block
      INTEGER DOCVT              ! 0 to suppress NDF data conversion
      INTEGER FITLEN             ! Number of cards in FITS block
      INTEGER IGRP3              ! Id for group of used IMGID values
      INTEGER INDF               ! NDF identifier
      INTEGER IPFIT              ! Pointer to FITS block
      LOGICAL THERE              ! Object exists
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Begin an NDF context.
      CALL NDF_BEGIN

*  Get the input NDF, ensuring no data conversion takes place.
      CALL NDF_GTUNE( 'DOCVT', DOCVT, STATUS )
      CALL NDF_TUNE( 0, 'DOCVT', STATUS )
      CALL NDF_ASSOC( 'IN', 'UPDATE', INDF, STATUS )

*  Ensure that the NDF does not already have a POLPACK extension, and
*  then create one.
      CALL NDF_XDEL( INDF, 'POLPACK', STATUS )
      CALL NDF_XNEW( INDF, 'POLPACK', 'POLPACK', 0, 0, POLLOC,
     :               STATUS )

*  Look for a FITS extension in the NDF. Create one if there isn't one
*  already, containing a single END card.
      CALL NDF_XSTAT( INDF, 'FITS', THERE, STATUS )
      IF( THERE ) THEN
         CALL NDF_XLOC( INDF, 'FITS', 'READ', FITLOC, STATUS )
      ELSE
         CALL NDF_XNEW( INDF, 'FITS', '_CHAR*80', 1, 1, FITLOC,
     :                  STATUS )
         CALL DAT_CELL( FITLOC, 1, 1, CELLOC, STATUS )
         CALL DAT_PUT0C( CELLOC, 'END', STATUS )
         CALL DAT_ANNUL( CELLOC, STATUS )
      END IF

*  Map in the fits block of the NDF, if it exists
      CALL DAT_MAPV( FITLOC, '_CHAR*80', 'READ', IPFIT, FITLEN,
     :               STATUS )

*  Now interpret and import the FITS information into the NDF. Note
*  that the lengths of the FITS block character strings are appended
*  after the last genuine argument. This is the usual method in UNIX
*  systems (normally implemented by the compiler), on VMS this makes
*  no difference.
      IF ( STATUS .EQ. SAI__OK ) THEN
         CALL POL1_IMPRT(  FITLEN, %VAL( CNF_PVAL( IPFIT )), 0, ' ',
     :                     POLLOC, .TRUE., STATUS,
     :                     %VAL( CNF_CVAL( 80 ) ) )
      END IF

*  Check the values in the POLPACK extension are usable.
      IGRP3 = GRP__NOID
      CALL POL1_CHKEX( INDF, POLLOC, IGRP3, .TRUE., STATUS )

*  Unmap FITS block.
      CALL DAT_UNMAP( FITLOC, STATUS )

*  Release the extensions.
      CALL DAT_ANNUL( POLLOC, STATUS )
      CALL DAT_ANNUL( FITLOC, STATUS )

*  If an error occurred, delete any POLPACK extension.
      IF ( STATUS .NE. SAI__OK ) CALL NDF_XDEL( INDF, 'POLPACK',
     :                                          STATUS )

*  Free GRP groups.
      IF( IGRP3 .NE. GRP__NOID ) CALL GRP_DELET( IGRP3, STATUS )

*  Re-instate the original DOCVT NDF tuning paerameter value.
      CALL ERR_BEGIN( STATUS )
      CALL NDF_TUNE( DOCVT, 'DOCVT', STATUS )
      CALL ERR_END( STATUS )

*  End the NDF context.
      CALL NDF_END( STATUS )

      END
