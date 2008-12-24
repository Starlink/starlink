      SUBROUTINE MAPCA2( IDC0, INDF0, CRDDF0, BAND0, IDA, NINCL,
     :                   INCLUD, IGRP1, NCRDDF, MAXSOP, MINSOP, LPCBND,
     :                   UPCBND, ALLVAR, ANYVAR, STATUS )
*+
*  Name:
*     MAPCA2

*  Purpose:
*     Determine the image bounds required to contain all the data from a
*     group of CRDD files.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL MAPCA2( IDC0, INDF0, CRDDF0, BAND0, IDA, NINCL, INCLUD,
*                  IGRP1, NCRDDF, MAXSOP, MINSOP, LPCBND, UPCBND,
*                  ALLVAR, ANYVAR, STATUS )

*  Description:
*     This routine finds the bounds of the image which encompasses the
*     data from selected detectors within all the CRDD files in the
*     group identified by IGRP1. Half coverage areas at the ends of
*     each scan are not included. Also, any NDFs which are not valid
*     CRDD files are removed from the group identified by IGRP1, before
*     returning to the calling routine. Flags are returned indicating
*     if any input CRDD files had defined variance values, and if all
*     input CRDD files had defined variance values.

*  Arguments:
*     IDC0 = INTEGER (Given)
*        The IRC identifier for the first good CRDD file in the group
*        identified by IGRP1. This (and all the other arguments
*        describing the first good CRDD file) are found using routine
*        MAPCA0.
*     INDF0 = INTEGER (Given)
*        An NDF identifier for the the first good CRDD file in the
*        group identified by IGRP1.
*     CRDDF0 = INTEGER (Given)
*        The index of the first good CRDD file within the group
*        identified by IGRP1.
*     BAND0 = INTEGER (Given)
*        The IRAS band number (1-4) of the data contained in the first
*        good CRDD file.
*     IDA = INTEGER (Given)
*        An IRA identifier specifying the mapping from sky to image
*        coordinates.
*     NINCL = INTEGER (Given)
*        The number of detector numbers stored in INCLUD.
*     INCLUD( NINCL ) = INTEGER (Given)
*        A list of detector numbers. Only the data from these detectors
*        is included in the image area.
*     IGRP1 = INTEGER (Given and Returned)
*        The GRP identifier for the group containing the CRDD file. On
*        exit, IGRP1 is replaced by an identifier for a new group which
*        contains only those input NDFs which are valid CRDD files. The
*        input group is deleted.
*     NCRDDF = INTEGER (Given and Returned)
*        The number of CRDD files in the group identified by IGRP1. The
*        returned value will be less than the supplied value if any of
*        the input NDFs are not valid CRDD files.
*     MAXSOP = INTEGER (Given and Returned)
*        The maximum SOP number included in the output group.
*     MINSOP = INTEGER (Given and Returned)
*        The minimum SOP number included in the output group.
*     LPCBND( 2 ) = DOUBLE PRECISION (Returned)
*        The lower limits of the two pixel coordinate axes of the image
*        which just encompasses all the required data from all the input
*        CRDD files.
*     UPCBND( 2 ) = DOUBLE PRECISION (Returned)
*        The upper limits of the two pixel coordinate axes of the image
*        which just encompasses all the required data from all the input
*        CRDD files.
*     ALLVAR = LOGICAL (Returned)
*        Returned .TRUE. if all the CRDD files have defined VARIANCE
*        components, and .FALSE. if any do not.
*     ANYVAR = LOGICAL (Returned)
*        Returned .TRUE. if any of the CRDD files have defined VARIANCE
*        components, and .FALSE. if none do.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     18-NOV-1991 (DSB):
*        Original version.
*     17-MAR-1992 (DSB):
*        Arguments MAXSOP and MINSOP added.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! Starlink data constants.
      INCLUDE 'MSG_PAR'          ! MSG_ constants.
      INCLUDE 'I90_DAT'          ! IRAS90 constants.

*  Arguments Given:
      INTEGER IDC0
      INTEGER INDF0
      INTEGER CRDDF0
      INTEGER BAND0
      INTEGER IDA
      INTEGER NINCL
      INTEGER INCLUD( NINCL )

*  Arguments Given and Returned:
      INTEGER IGRP1
      INTEGER NCRDDF
      INTEGER MAXSOP
      INTEGER MINSOP

*  Arguments Returned:
      DOUBLE PRECISION LPCBND( 2 )
      DOUBLE PRECISION UPCBND( 2 )
      LOGICAL ALLVAR
      LOGICAL ANYVAR

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER BAND               ! IRAS waveband (1-4) of the data
                                 ! within the current CRDD file.
      INTEGER CRDDF              ! Index to current CRDD file in input
                                 ! group.
      INTEGER IGRP2              ! Identifier for output group.
      INTEGER IDC                ! IRC identifier for the current CRDD
                                 ! file.
      INTEGER INDF               ! NDF identifier for the current CRDD
                                 ! file.
      REAL    NOMSPD             ! Nominal scan speed.
      INTEGER OBS                ! OBS of current CRDD file.
      DOUBLE PRECISION REFDEC    ! DEC of reference point of current
                                 ! CRDD file.
      DOUBLE PRECISION REFRA     ! RA of reference point of current CRDD
                                 ! file.
      INTEGER SIZOUT             ! Size of output group.
      INTEGER SOP                ! SOP of current CRDD file.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Create a new, empty group to hold the names of NDFs which are
*  succesfully imported into the IRC system.
      CALL GRP_NEW( 'Good CRDD files', IGRP2, STATUS )

*  Initialise the output image pixel bounds to impossible values.
      LPCBND( 1 ) = VAL__MAXD
      LPCBND( 2 ) = VAL__MAXD
      UPCBND( 1 ) = VAL__MIND
      UPCBND( 2 ) = VAL__MIND

*  Initialise a flag to indicate that all input CRDD files have
*  VARIANCE components.
      ALLVAR = .TRUE.

*  Initialise a flag to indicate that no input CRDD files have
*  VARIANCE components.
      ANYVAR = .FALSE.

*  Update the image bounds to include the data in the first good
*  CRDD file in the input group, and then put the CRDD file into the
*  group identified by IGRP2. Also update ALLVAR and ANYVAR depending on
*  whether the CRDD file has a defined VARIANCE component or not.
      CALL MAPCA1( IDC0, INDF0, CRDDF0, IGRP1, IGRP2, IDA, NINCL,
     :             INCLUD, LPCBND, UPCBND, ALLVAR, ANYVAR, STATUS )

*  Indicate that the output group currently holds a single usable CRDD
*  file.
      SIZOUT = 1

*  Loop round the remaining NDFs in the input group.
      DO CRDDF = CRDDF0 + 1, NCRDDF

*  Abort if an error has occurred.
         IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Obtain an NDF identifier for the current input NDF.
         CALL NDG_NDFAS( IGRP1, CRDDF, 'READ', INDF, STATUS )

*  Attempt to import it as a CRDD file into the IRC_ system.
         CALL IRC_IMPRT( INDF, IDC, STATUS )

*  If the file could not be imported (probably due to it not being a
*  CRDD file), annull the NDF identifier, flush the error condition and
*  pass on to the next input file.
         IF( STATUS .NE. SAI__OK ) THEN
            CALL NDF_ANNUL( INDF, STATUS )
            CALL ERR_FLUSH( STATUS )

*  If the file was succesfully imported into IRC, get the band number
*  from the CRDD file.
         ELSE
            CALL IRC_INFO( IDC, BAND, REFRA, REFDEC, NOMSPD, SOP, OBS,
     :                     STATUS )

*  Check that the current CRDD file contains data for the same waveband
*  as the first CRDD file. If not, conditionally give a warning
*  message, and pass on to the next CRDD file.
            IF( BAND .NE. BAND0 ) THEN
               CALL MSG_BLANKIF( MSG__NORM, STATUS )
               CALL NDF_MSG( 'NDF', INDF )
               CALL MSG_SETI( 'W', I90__WAVEL( BAND ) )
               CALL MSG_OUTIF( MSG__NORM, 'MAPCA2_MSG1',
     :   '  NDF ^NDF contains ^W um data and will therefore be ignored',
     :                        STATUS )

*  If the CRDD file contained data for the correct waveband...
            ELSE

*  Update the image bounds to include the data from this CRDD file in
*  the input group, and then put the CRDD file into the group
*  identified by IGRP2. Also update ALLVAR and ANYVAR depending on
*  whether the CRDD file has a defined VARIANCE component or not.
               CALL MAPCA1( IDC, INDF, CRDDF, IGRP1, IGRP2, IDA, NINCL,
     :                      INCLUD, LPCBND, UPCBND, ALLVAR, ANYVAR,
     :                      STATUS )

*  Update the maximum and minimum SOP numbers.
               MAXSOP = MAX( MAXSOP, SOP )
               MINSOP = MIN( MINSOP, SOP )

*  Increment the size of the output group.
               SIZOUT = SIZOUT + 1

            END IF

*  Annul the input IDC and NDF identifiers.
            CALL IRC_ANNUL( IDC, STATUS )
            CALL NDF_ANNUL( INDF, STATUS )

         END IF

      END DO

*  Abort if an error has been reported.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Delete the input group.
      CALL GRP_DELET( IGRP1, STATUS )

*  Replace the input group identifier and size by the output values.
      IGRP1 = IGRP2
      NCRDDF = SIZOUT

 999  CONTINUE

      END
