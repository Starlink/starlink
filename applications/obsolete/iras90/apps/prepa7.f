      SUBROUTINE PREPA7( NCARD, FITS, PFACT, TYPE, BAND, INDF1, INDF2,
     :                   NDFOUT, TITLE, LABEL, FLDLON, FLDLAT, SCS,
     :                   PROJ, UNITS, IGRP, PHIST, STATUS )
*+
*  Name:
*     PREPA7

*  Purpose:
*     Create a single output NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL PREPA7( NCARD, FITS, PFACT, TYPE, BAND, INDF1, INDF2,
*                  NDFOUT, TITLE, LABEL, FLDLON, FLDLAT, SCS, PROJ,
*                  UNITS, IGRP, PHIST, STATUS )

*  Description:
*     This routine produces a single output NDF.

*  Arguments:
*     NCARD = INTEGER (Given)
*        The number of FITS header cards.
*     FITS( NCARD ) = CHARACTER * ( * ) (Given)
*        The FITS header cards.
*     PFACT = CHARACTER * ( * ) (Given)
*        The parameter to use for getting factors for converting input
*        units to output units in the case of either system of units
*        being unknown.
*     TYPE = CHARACTER * ( * ) (Given)
*        The image type. This should be equal to one of the symbolic
*        constants defined within the IRI subsystem.
*     BAND = INTEGER (Given)
*        The waveband index.
*     INDF1 = INTEGER (Given)
*        The identifier for the input NDF holding the data which is to
*        be stored in the output data arrays.
*     INDF2 = INTEGER (Given)
*        The identifier for the input NDF holding the data which is to
*        be stored in the output variance arrays. If this is supplied
*        equal to NDF__NOID then no variance array will be included in
*        the output NDFs.
*     NDFOUT = CHARACTER * ( * ) (Given)
*        The name of the output NDF.
*     TITLE = CHARACTER * ( * ) (Given)
*        The title for the output NDF.
*     LABEL = CHARACTER * ( * ) (Given)
*        The label for the output NDF.
*     FLDLON = CHARACTER * ( * ) (Given)
*        The longitude value to be stored as the field position (as a
*        formatted string). If a blank is supplied, then the value of
*        FITS keyword CRVAL1 is used.
*     FLDLAT = CHARACTER * ( * ) (Given)
*        The latitude value to be stored as the field position (as a
*        formatted string). If a blank is supplied, then the value of
*        FITS keyword CRVAL2 is used.
*     SCS = CHARACTER * ( * ) (Given)
*        The sky coordinate system to which FLDLON and FLDLAT refer.
*        This is also the sky coordinate system assumed for the CRVAL1
*        and CRVAL2 keywords if the input image type is unrecognised.
*     PROJ = CHARACTER * ( * ) (Given)
*        The projection type to assume if the input image type is
*        unrecognised.
*     UNITS = CHARACTER * ( * ) (Given)
*        The system of units in which the output data array is to be
*        produced. If a blank value is supplied, then the output is in
*        the same units as the input, whatever they may be.
*     IGRP = INTEGER (Given)
*        An identifier for a group into which the names of succesfully
*        created output NDFs are put.
*     PHIST = CHARACTER * ( * ) (Given)
*        The parameter to use for deciding if history is to be included
*        in the output NDF.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     14-DEC-1992 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ constants
      INCLUDE 'MSG_PAR'          ! MSG_ constants
      INCLUDE 'NDF_PAR'          ! NDF_ constants
      INCLUDE 'IRI_PAR'          ! IRI_ constants.
      INCLUDE 'IRI_ERR'          ! IRI_ error constants.

*  Arguments Given:
      INTEGER NCARD
      CHARACTER FITS( NCARD )*(*)
      CHARACTER PFACT*(*)
      CHARACTER TYPE*(*)
      INTEGER BAND
      INTEGER INDF1
      INTEGER INDF2
      CHARACTER NDFOUT*(*)
      CHARACTER TITLE*(*)
      CHARACTER LABEL*(*)
      CHARACTER FLDLON*(*)
      CHARACTER FLDLAT*(*)
      CHARACTER SCS*(*)
      CHARACTER PROJ*(*)
      CHARACTER UNITS*(*)
      INTEGER IGRP
      CHARACTER PHIST*(*)

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Used length of a string.

*  Local Variables:
      CHARACTER HIST*255         ! History record.
      CHARACTER LOC*(DAT__SZLOC) ! Locator to the IMAGE_INFO structure.
      CHARACTER U*80             ! Units to be stored in the output.
      CHARACTER XLOC*(DAT__SZLOC)! Locator to the IRAS extension.


      DOUBLE PRECISION A         ! Sky longitude of field reference
                                 ! point.
      DOUBLE PRECISION B         ! Sky latitude of field reference
                                 ! point.
      DOUBLE PRECISION PIXSIZ    ! Nominal pixel size in steradians.


      INTEGER EL                 ! No. of elements in a mapped array.
      INTEGER IERR               ! Index of first conversion error.
      INTEGER INDEX              ! Index of new name added to output
                                 ! group.
      INTEGER INDF3              ! NDF identifier for output NDF.
      INTEGER IPDATA             ! Pointer to the DATA component of
                                 ! the output NDF.
      INTEGER IPVAR              ! Pointer to the VARIANCE component of
                                 ! the output NDF.
      INTEGER LHIST              ! Used length of HIST.
      INTEGER LSCS               ! Used length of SCS.
      INTEGER NERR               ! Number of conversion errors.


      LOGICAL THERE              ! True if an object exists.
      LOGICAL XFLIP              ! True if the X axis needs flipping.
      LOGICAL YFLIP              ! True if the Y axis needs flipping.


      REAL FACTOR                ! Factor for converting from input to
                                 ! output units.
      REAL SCALE                 ! Scale factor for converting input
                                 ! data numbers to the required units.
      REAL ZERO                  ! Zero offset for converting input
                                 ! data numbers to the required units.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Add the output NDF name to the end of the supplied group, and get the
*  index of the new name within the group.
      CALL GRP_PUT( IGRP, 1, NDFOUT, 0, STATUS )
      CALL GRP_GRPSZ( IGRP, INDEX, STATUS )

*  Create the output NDF by propagating HISTORY, and all extensions
*  from the input NDF.
      CALL NDG_NDFPR( INDF1, 'NOTITLE,NOLABEL', IGRP, INDEX, INDF3,
     :                STATUS )

*  Ensure that the output NDF has the data type _REAL.
      CALL NDF_STYPE( '_REAL', INDF3, 'DATA', STATUS )

*  If required, store the label and title in the output NDF.
      IF( LABEL .NE. ' ' ) CALL NDF_CPUT( LABEL, INDF3, 'LABEL',
     :                                    STATUS )
      IF( TITLE .NE. ' ' ) CALL NDF_CPUT( TITLE, INDF3, 'TITLE',
     :                                    STATUS )

*  See if there is already an IRAS extension.
      CALL NDF_XSTAT( INDF3, 'IRAS', THERE, STATUS )

*  If necessary, create the IRAS extension.
      IF( .NOT. THERE ) then
         CALL NDF_XNEW( INDF3, 'IRAS', 'IRAS', 0, 0, XLOC, STATUS )

*  Otherwise get a locator to the existing extension.
      ELSE
         CALL NDF_XLOC( INDF3, 'IRAS', 'UPDATE', XLOC, STATUS )
      END IF

*  Delete any existing astrometry structure.
      CALL DAT_THERE( XLOC, 'ASTROMETRY', THERE, STATUS )
      IF( THERE ) CALL DAT_ERASE( XLOC, 'ASTROMETRY', STATUS )

*  Create an astrometry structure within the IRAS extension of the
*  output NDF, see if the X and/or Y axes are to be flipped, and get
*  numerical values for the field longitude and latitude, and pixel
*  size.
      CALL PREPB1( INDF3, NCARD, FITS, TYPE, PROJ, SCS, FLDLON, FLDLAT,
     :             A, B, XFLIP, YFLIP, PIXSIZ, STATUS )

*  Get the scale and zero factors required for converting the input
*  data to the required output units (this will incorporate the BSCALE
*  and BZERO keywords if the input is not of _REAL data type, and also
*  the BIAS keyword if appropriate). If a blank value is supplied for
*  UNITS then the value derived from the BUNIT keyword is returned in
*  U.
      CALL PREPB0( BAND, PFACT, INDF1, NCARD, FITS, TYPE, UNITS, PIXSIZ,
     :             SCALE, ZERO, U, FACTOR, STATUS )

*  See if there is already an IMAGE_INFO structure in the IRAS
*  extension.
      CALL DAT_THERE( XLOC, 'IMAGE_INFO', THERE, STATUS )

*  If there is, delete it.
      IF( THERE ) CALL DAT_ERASE( XLOC, 'IMAGE_INFO', STATUS )

*  Create an IMAGE_INFO component within the IRAS extension.
      IF( TYPE .EQ. IRI__CPC ) THEN
         CALL IRI_NEW( INDF3, 'CPC', BAND, TYPE, U, LOC, STATUS )
      ELSE
         CALL IRI_NEW( INDF3, 'SURVEY', BAND, TYPE, U, LOC, STATUS )
      END IF

*  If a non-standard system of units was supplied, annul the error.
      IF( STATUS .EQ. IRI__BADUN ) THEN
         CALL ERR_ANNUL( STATUS )

*  If some units are specified, warn the use that non-standard units
*  are being used.
         IF( U .NE. ' ' ) THEN
            CALL MSG_BLANKIF( MSG__QUIET, STATUS )
            CALL MSG_SETC( 'U', U )
            CALL MSG_OUTIF( MSG__QUIET, 'PREPA7_MSG1',
     : 'WARNING: Non-standard system of units "^U" being used',
     :                      STATUS )
            CALL MSG_BLANKIF( MSG__QUIET, STATUS )

*  Otherwise, reset the NDF UNITS component.
         ELSE
            CALL NDF_RESET( INDF3, 'UNITS', STATUS )
         END IF

      END IF

*  Store the component FIELDLAT, FIELDLON and FIELD SCS in the
*  IMAGE_INFO structure.
      CALL DAT_NEW0D( LOC, 'FIELDLON', STATUS )
      CALL CMP_PUT0D( LOC, 'FIELDLON', A, STATUS )

      CALL DAT_NEW0D( LOC, 'FIELDLAT', STATUS )
      CALL CMP_PUT0D( LOC, 'FIELDLAT', B, STATUS )

      LSCS = CHR_LEN( SCS )
      CALL DAT_NEW0C( LOC, 'FIELDSCS', LSCS, STATUS )
      CALL CMP_PUT0C( LOC, 'FIELDSCS', SCS( : LSCS ), STATUS )

*  Add additional components, dependant on the image type.
      IF( TYPE .EQ. IRI__CPC ) THEN
         CALL PREPB2( NCARD, FITS, LOC, STATUS )

      ELSE IF( TYPE .EQ. IRI__SKYFL ) THEN
         CALL PREPB3( NCARD, FITS, LOC, STATUS )

      ELSE IF( TYPE .EQ. IRI__GALPL ) THEN
         CALL PREPB4( NCARD, FITS, LOC, STATUS )

      ELSE IF( TYPE .EQ. IRI__ALLSK ) THEN
         CALL PREPB5( NCARD, FITS, LOC, STATUS )

      ELSE IF( TYPE .EQ. IRI__DSCO ) THEN
         CALL PREPB6( NCARD, FITS, LOC, U, FACTOR, PIXSIZ, STATUS )

      ELSE IF( TYPE .EQ. IRI__ISSA ) THEN
         CALL PREPB7( NCARD, FITS, LOC, STATUS )

      ELSE IF( TYPE .EQ. IRI__YORIC ) THEN
         CALL PREPB8( NCARD, FITS, LOC, STATUS )

      END IF

*  Annull the locator to the IMAGE_INFO structure.
      CALL DAT_ANNUL( LOC, STATUS )

*  Anull the locator to the IRAS extension.
      CALL DAT_ANNUL( XLOC, STATUS )

*  Scale the output DATA array to the the correct units, and perform any
*  flipping of axes that may be required.
      CALL PREPB9( INDF1, INDF3, SCALE, ZERO, XFLIP, YFLIP, STATUS )

*  If necessary, copy the square of the DATA component of the input
*  noise NDF, to the VARIANCE component of the output NDF.
      IF( INDF2 .NE. NDF__NOID ) THEN
         CALL NDF_MAP( INDF2, 'DATA', '_REAL', 'READ', IPDATA, EL,
     :                 STATUS )
         CALL NDF_MAP( INDF3, 'VARIANCE', '_REAL', 'WRITE', IPVAR, EL,
     :                 STATUS )
         CALL VEC_MULR( .TRUE., EL, %VAL( IPDATA ), %VAL( IPDATA ),
     :                  %VAL( IPVAR ), IERR, NERR, STATUS )
         CALL NDF_UNMAP( INDF2, 'DATA', STATUS )
         CALL NDF_UNMAP( INDF3, 'VARIANCE', STATUS )
      END IF

*  Add history to the output NDF.
      CALL NDF_MSG( 'IN', INDF1 )
      CALL NDF_MSG( 'OUT', INDF3 )

      IF( INDF2 .NE. NDF__NOID ) THEN
         CALL NDF_MSG( 'NG', INDF2 )
         CALL MSG_LOAD( ' ', '^OUT created from ^IN and ^NG', HIST,
     :                  LHIST, STATUS )
      ELSE
         CALL MSG_LOAD( ' ', '^OUT created from ^IN', HIST, LHIST,
     :                  STATUS )
      END IF

      CALL IRM_HIST( PHIST, INDF3, 'IRAS90:PREPARE', 1, HIST, STATUS )

*  If all has gone ok, annul the output NDF identifier.
      IF( STATUS .EQ. SAI__OK ) THEN
         CALL NDF_ANNUL( INDF3, STATUS )

*  If an error has occurred, attempt to delete the output NDF and
*  remove the output NDF name from the returned group.
      ELSE
         CALL NDF_DELET( INDF3, STATUS )

         CALL ERR_BEGIN( STATUS )
         CALL GRP_SETSZ( IGRP, INDEX - 1, STATUS )
         CALL ERR_END( STATUS )

      END IF

      END
