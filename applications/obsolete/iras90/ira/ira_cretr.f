      SUBROUTINE IRA_CRETR( FWDLOC, INVLOC, SCS, EPOCH, INDF, IDA,
     :                      STATUS )
*+
*  Name:
*     IRA_CRETR

*  Purpose:
*     Create an astrometry structure containing a TRANSFORM projection.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRA_CRETR( FWDLOC, INVLOC, SCS, EPOCH, INDF, IDA, STATUS )

*  Description:
*     IRA conceals the way in which projections are handled. This will
*     hopefully make it possible to re-implement IRA in terms of the
*     official Starlink astrometry system without changing any argument
*     lists. In fact, the current version of IRA handles most
*     projections by storing the projection name and projection
*     parameters in the NDF. IRA_TRANS then calls the appropriate IRA
*     routine to implement the required projection. This is a relatively
*     fast way of doing things, but it restricts the projections which
*     can be used to those for which explicit IRA transformation
*     routines exist.

*     An alternative to this method is to use the TRANSFORM package
*     (see SUN/61) to create a structure which contains in a self
*     describing way the steps needed to implement a projection. This
*     structure can be stored in the NDF. IRA_TRANS can then read in
*     the structure and call the relevant TRANSFORM routine to do the
*     transformation.  This makes it possible for the application to
*     set up any projection, not just the ones explicitly supported by
*     IRA. The disadvantage is that such "TRANSFORM" projections are
*     significantly slower to use when transforming data.

*     This routine produces such an astrometry structure. It must be
*     noted that in view of the fact that this requires details of the
*     interim implementation of IRA to be assumed, IRA_CRETR may need
*     to be modified or withdrawn when IRA is re-written to use the
*     official Starlink astrometry system.

*     Two TRN_TRANSFORM structures should be provided (as created by
*     TRANSFORM routine TRN_NEW); the one located by FWDLOC is used to
*     define the forward mapping of the projection (image coordinates
*     to sky coordinates), and the one located by INVLOC is used to
*     define the inverse mapping. Two structures are used rather than a
*     single structure so that the mappings can be decomposed into
*     several sub-mappings which are then concatenated to produce the
*     complete mapping. Generally, each sub-mapping of the forward
*     mapping will not correspond to a single sub-mapping in the
*     inverse mapping. If, in fact, a single TRN_TRANSFORM structure
*     can efficiently describe both mappings, then the locator should
*     be supplied for FWDLOC, and INVLOC should be given a blank value.

*     Both transformations should have two input variables
*     corresponding to the first (X) and second (Y) image coordinates,
*     and two output variables corresponding to sky longitude and
*     latitude.

*     Note, "temporary astrometry structures" can not be created using
*     this routine. The NDF extension which is to hold the astrometry
*     structure must already exist before calling this routine (see
*     routine IRA_LOCAT).

*  Arguments:
*     FWDLOC = CHARACTER * ( * ) (Given)
*        An HDS locator to a TRN_TRANSFORM structure (see SUN/61). The
*        forward mapping of the transformation is used to define the
*        forward mapping of the projection. If INVLOC is blank, then
*        the inverse mapping of this structure is also used, to define
*        the inverse mapping of the projection.
*     INVLOC = CHARACTER * ( * ) (Given)
*        An HDS locator to a TRN_TRANSFORM structure. The inverse
*        mapping of the transformation is used to define the inverse
*        mapping of the projection. If INVLOC is blank, then the
*        inverse mapping of the structure located by FWDLOC is used
*        instead.
*     SCS = CHARACTER * ( * ) (Given)
*        The name of the Sky Coordinate System which the projection is
*        to create, or an unambiguous abbreviation. See routine IRA_ISCS
*        for a list of currently supported values. See also ID2 section
*        "Sky Coordinates".
*     EPOCH = DOUBLE PRECISION (Given)
*        The Julian epoch at which the observations were made. A single
*        mean epoch is sufficient to describe all IRAS observations.
*        Such a value is contained in the IRA constant IRA__IRJEP.
*     INDF = INTEGER (Given)
*        The identifier for the NDF in which the astrometry information
*        is to be stored.
*     IDA = INTEGER (Returned)
*        An "IRA identifier" for the astrometry structure. This value
*        can be passed to other IRA routines to gain access to the
*        astrometry information contained in the structure. The
*        identifier should be annulled when it is no longer required
*        using IRA_ANNUL.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     7-JAN-1991 (DSB):
*        Original version.
*     27-APR-1991 (DSB):
*        Modified for IRA version 2.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT constants
      INCLUDE 'PRM_PAR'          ! Starlink data constants.
      INCLUDE 'IRA_PAR'          ! IRA constants.
      INCLUDE 'IRA_ERR'          ! IRA errors

*  Global Variables:
      INCLUDE 'IRA_COM'          ! IRA common blocks.
*        ACM_VALID( IRA__MAX ) = LOGICAL (Write)
*           If true, then the associated elements of the other arrays
*           held in common contain valid astrometry information.
*        ACM_ASLOC( IRA__MAX ) = CHARACTER (Write)
*           HDS locator to the Astrometry structure from which the
*           astrometry information is drawn. If blank then the
*           astrometry information is tempoarary.
*        ACM_SCS( IRA__MAX ) = CHARACTER (Write)
*           The full name of the sky coordinate system, with optional
*           equinox specifier.
*        ACM_PROJN( IRA__MAX ) = CHARACTER (Write)
*           The full name of the projection.
*        ACM_EPOCH( IRA__MAX ) = DOUBLE PRECISION (Write)
*           The Julian epoch of observation.

*  Arguments Given:
      CHARACTER FWDLOC*(*)
      CHARACTER INVLOC*(*)
      CHARACTER SCS*(*)
      DOUBLE PRECISION EPOCH
      INTEGER   INDF

*  Arguments Returned:
      INTEGER   IDA

*  Status:
      INTEGER   STATUS           ! Global status

*  Local Variables:
      CHARACTER        BJ*1      ! The type of epoch (Besselian or
                                 ! Julian) held by variable EQU.
      DOUBLE PRECISION EQU       ! The epoch of the reference equinox
                                 ! specified in argument SCS.
      CHARACTER LOC*(DAT__SZLOC)  ! HDS locator to astrometry structure.
      CHARACTER NAME*(IRA__SZSCS)! Name of the Sky Coordinate System
                                  ! with no equinox specifier.
      INTEGER   NVIN             ! No. of input variables in TRANSFORMATION.
      INTEGER   NVOUT            ! No. of input variables in TRANSFORMATION.
      CHARACTER TSCS*(IRA__SZSCS)! Full name of the Sky Coordinate System.
      INTEGER   TSTAT            ! Local status value.
*.

      IDA = IRA__NOID

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check that IRA has been initialised.
      IF( ACM_STATE .NE. IRA__GOING ) THEN
         STATUS = IRA__INIT
         CALL ERR_REP( 'IRA_CRETR_ERR1',
     :             'IRA_CRETR: The IRAS90 astrometry system has not '//
     :             'been initialised', STATUS )
         GO TO 999
      END IF

*  Get the next free IRA identifier value.
      CALL IRA1_GETID( IDA, STATUS )

*  Identify the requested sky coordinate system.
      TSCS = SCS
      CALL IRA_GETEQ( TSCS, EQU, BJ, NAME, STATUS )

*  Check that the forward TRN_TRANSFORM structure is ok.
      CALL TRN_GTNV( FWDLOC, NVIN, NVOUT, STATUS )
      IF( NVIN .NE. 2 .OR. NVOUT .NE. 2 ) THEN
         STATUS = IRA__BADTR
         CALL ERR_REP( 'IRA_CRETR_ERR1',
     :        'IRA_CRETR: Bad number of input or output variables in '//
     :                 'supplied TRANSFORM structure', STATUS )
      END IF

*  If supplied, check that the inverse TRN_TRANSFORM structure is ok.
      IF( INVLOC .NE. ' ' ) THEN
         CALL TRN_GTNV( INVLOC, NVIN, NVOUT, STATUS )
         IF( NVIN .NE. 2 .OR. NVOUT .NE. 2 ) THEN
            STATUS = IRA__BADTR
            CALL ERR_REP( 'IRA_CRETR_ERR2',
     :        'IRA_CRETR: Bad number of input or output variables in '//
     :                 'supplied TRANSFORM structure', STATUS )
         END IF
      END IF

*  Abort if an error has occurred.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Obtain an HDS locator to an astrometry structure (if no astrometry
*  structure exists, a new one is created).
      CALL IRA1_ASNDF( INDF, 'WRITE', LOC, STATUS )

*  Store the name of the sky coordinate system produced by the
*  projection and the epoch of the observations, in the astrometry
*  structure.
      CALL CMP_PUT0C( LOC, 'SCS', TSCS, STATUS )
      CALL CMP_PUT0D( LOC, 'EPOCH', EPOCH, STATUS )

*  Delete the PROJ_NAME and PROJ_PARS components.
      CALL DAT_ERASE( LOC, 'PROJ_NAME', STATUS )
      CALL DAT_ERASE( LOC, 'PROJ_PARS', STATUS )

*  Copy the forward transformation to the AS.
      CALL DAT_COPY( FWDLOC, LOC, 'FWD_TRANSFORM', STATUS )

*  If supplied, copy the inverse transformation to the AS.
      IF( INVLOC .NE. ' ' ) CALL DAT_COPY( INVLOC, LOC, 'INV_TRANSFORM',
     :                                     STATUS )

*  Set the AS into the DEFINED state.
      CALL IRA1_ASSET( LOC, STATUS )

*  Store the astrometry information in common.
      ACM_ASLOC( IDA ) = LOC
      ACM_EPOCH( IDA ) = EPOCH
      ACM_PROJN( IDA ) = ' '
      ACM_SCS( IDA ) = TSCS

*  Indicate that the elements of the common arrays indexed by IDA
*  contain valid astrometry information.
      IF( STATUS .EQ. SAI__OK ) ACM_VALID( IDA ) = .TRUE.

*  If an error occurred, give the context, delete any HDS astrometry
*  structure, and set the IRA identifier invalid.
 999  CONTINUE

      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'IRA_CRETR_ERR3',
     :   'IRA_CRETR: Unable to create a TRANSFORM astrometry structure',
     :                 STATUS )

         IF( LOC .NE. ' ' ) CALL IRA1_DELOB( LOC, TSTAT )

      END IF

      END
