      SUBROUTINE IRA_CREAT( PROJ, NP, P, SCS, EPOCH, INDF, IDA, STATUS )
*+
*  Name:
*     IRA_CREAT

*  Purpose:
*     Create an identifier for specified astrometry information.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRA_CREAT( PROJ, NP, P, SCS, EPOCH, INDF, IDA, STATUS )

*  Description:
*     The supplied astrometry information is stored in internal common
*     blocks and an "IRA identifier" is returned which can be passed to
*     other IRA routines to refer to the stored astrometry information.
*     This identifier should be annulled when it is no longer required
*     by calling IRA_ANNUL. In addition, a call to IRA_EXPRT may
*     optionally be made to store the astrometry information in an NDF
*     (see argument INDF).
*
*     The projection used is specified by the argument PROJ, and must
*     be one of the supported projection types (see routine IRA_IPROJ).
*     For more information on the available projections, see the ID/2
*     appendix "Projection Equations". Each projection requires the
*     values for several parameters to be supplied in argument P. These
*     parameters have the same meaning for all projections (for further
*     details see ID/2 appendix "Projection Equations"):
*
*     P(1): The longitude (in the Sky Coordinate System specified by
*           argument SCS) of the reference point in radians.
*
*     P(2): The latitude (in the Sky Coordinate System specified by
*           argument SCS) of the reference point in radians.
*
*     P(3): The first image coordinate (i.e. X value) of the reference
*           point. Image coordinates are fractional values in which the
*           centre of the pixel (1,1) has coordinates (0.5,0.5).
*
*     P(4): The second image coordinate (i.e. Y value) of the
*           reference point.
*
*     P(5): The size along the X image axis, of a pixel centred at the
*           reference point, in radians. Actual pixel size will vary
*           over the image due to the distorting effect of the
*           projection. The absolute value is used.
*
*     P(6): The size along the Y image axis, of a pixel centred at the
*           reference point, in radians. The absolute value is used.
*
*     P(7): The position angle of the Y image axis, in radians. That is,
*           the angle from north to the positive direction of the Y
*           image axis, measured positive in the same sense as
*           rotation from north to east. (Here "north" and "east" are
*           defined by the value of SCS). The X image axis is 90 degrees
*           west of the Y axis.
*
*     P(8): An angle through which the celestial sphere is to be rotated
*           before doing the projection. The axis of the rotation is a
*           radius passing through the reference point. The rotation is
*           in an anti-clockwise sense when looking from the reference
*           point towards the centre of the celestial sphere. The value
*           should be in radians. Changing this angle does not change
*           the orientation of the image axes with respect to north
*           (which is set by p(7)).

*  Arguments:
*     PROJ = CHARACTER * ( * ) (Given)
*        The projection type (see routine IRA_IPROJ for a list of
*        currently recognised values). Any unambiguous abbreviation can
*        be given.
*     NP = INTEGER (Given)
*        The size of array P.
*     P( NP ) = DOUBLE PRECISION (Given)
*        The parameter values required by the projection.
*     SCS = CHARACTER * ( * ) (Given)
*        The name of the Sky Coordinate System which the projection is
*        to create, or an unambiguous abbreviation. See routine IRA_ISCS
*        for a list of currently recognised values.  See ID2 section
*        "Sky Coordinates") for general information of Sky Coordinate
*        Systems.
*     EPOCH = DOUBLE PRECISION (Given)
*        The Julian epoch at which the observations were made. A single
*        mean epoch is sufficient to describe all IRAS observations.
*        Such a value is contained in the IRA constant IRA__IRJEP.
*     INDF = INTEGER (Given)
*        The identifier for the NDF in which the astrometry information
*        is to be stored. If an invalid NDF identifier is given (eg the
*        value NDF__NOID) then the astrometry information is not stored
*        in an NDF (but IDA can still be used to refer to the astrometry
*        information).
*     IDA = INTEGER (Returned)
*        The returned IRA identifier.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1991, 1992, 1993 Science & Engineering Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     7-JAN-1991 (DSB):
*        Original version.
*     27-APR-1991 (DSB):
*        Modified for IRA version 2.
*     10-FEB-1992 (DSB):
*        Absolute value of P(5) and P(6) used.
*     11-SEP-1992 (DSB):
*        Add P(8).
*     11-FEB-1993 (DSB):
*        Changed to incorporate a call to IRA_EXPRT.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ constants
      INCLUDE 'PRM_PAR'          ! VAL_ constants
      INCLUDE 'IRA_PAR'          ! IRA_ constants
      INCLUDE 'IRA_ERR'          ! IRA_ errors constants

*  Global Variables:
      INCLUDE 'IRA_COM'          ! IRA common blocks.
*        ACM_EPOCH( IRA__MAX ) = DOUBLE PRECISION (Write)
*           The Julian epoch of observation.
*        ACM_PROJN( IRA__MAX ) = CHARACTER (Write)
*           The full name of the projection.
*        ACM_PROJP( IRA__MAXP, IRA__MAX ) = DOUBLE PRECISION (Write)
*           Projection parameters.
*        ACM_SCS( IRA__MAX ) = CHARACTER (Write)
*           The full name of the sky coordinate system, with an optional
*           equinox specifier.
*        ACM_STATE = CHARACTER (Read)
*           Equal to IRA__GOING if IRA has been initialised.
*        ACM_VALID( IRA__MAX ) = LOGICAL (Write)
*           If true, then the associated elements of the other arrays
*           held in common contain valid astrometry information.

*  Arguments Given:
      CHARACTER PROJ*(*)
      INTEGER   NP
      DOUBLE PRECISION P( NP )
      CHARACTER SCS*(*)
      DOUBLE PRECISION EPOCH
      INTEGER   INDF

*  Arguments Returned:
      INTEGER   IDA

*  Status:
      INTEGER   STATUS           ! Global status

*  Local Variables:
      CHARACTER
     :          BJ*1,              ! The type of epoch (Besselian or
     :                             ! Julian) held by variable EQU.
     :          NAME*(IRA__SZSCS), ! Name of the Sky Coordinate System
     :                             ! (with no equinox specifier).
     :          TPROJ*(IRA__SZPRJ),! Full name of the projection.
     :          TSCS*(IRA__SZSCS)  ! Full name of the Sky Coordinate
                                   ! System.
      DOUBLE PRECISION
     :          EQU,             ! The epoch of the reference equinox
     :                           ! specified in argument SCS.
     :          LP( IRA__MAXP )  ! Local copy of projection parameters.

      INTEGER
     :          IP,              ! Parameter index.
     :          NPREQ            ! Required number of projection
                                 ! parameters.
      LOGICAL
     :          VALID            ! True if the supplied NDF identifier
                                 ! is valid.

*.
      IDA = IRA__NOID

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check that IRA has been initialised.
      IF( ACM_STATE .NE. IRA__GOING ) THEN
         STATUS = IRA__INIT
         CALL ERR_REP( 'IRA_CREAT_ERR1',
     :             'IRA_CREAT: The IRAS90 astrometry system has not '//
     :             'been initialised', STATUS )
      END IF

*  Get the next free IRA identifier value.
      CALL IRA1_GETID( IDA, STATUS )

*  Identify the requested sky coordinate system.
      TSCS = SCS
      CALL IRA_GETEQ( TSCS, EQU, BJ, NAME, STATUS )

*  Identify the requested projection.
      CALL IRA1_CHPRJ( PROJ, TPROJ, NPREQ, STATUS )

*  Check the right number of parameters have been supplied.
      IF( NP .NE. NPREQ .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = IRA__TFEWP
         CALL MSG_SETI( 'NP', NP )
         CALL MSG_SETI( 'NPR', NPREQ )
         CALL MSG_SETC( 'PROJ', TPROJ )
         CALL ERR_REP( 'IRA_CREAT_ERR2',
     :        'IRA_CREAT: ^PROJ projection requires ^NPR parameters; '//
     :        '^NP supplied', STATUS )
      END IF

*  Abort if an error has occurred.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Check that non of the supplied parameter values is bad.
      DO IP = 1, NP
         IF( P( IP ) .EQ. VAL__BADD .AND. STATUS .EQ. SAI__OK ) THEN
            STATUS = IRA__BADPA
            CALL MSG_SETI( 'I', IP )
            CALL ERR_REP( 'IRA_CREAT_ERR3',
     :   'IRA_CREAT: BAD projection parameter value ( P(^I) ) supplied',
     :                    STATUS )
            GO TO 999
         ELSE
            LP( IP ) = P( IP )
         END IF
      END DO

*  Ensure that P(5) and P(6) are positive.
      LP( 5 ) = ABS( LP( 5 ) )
      LP( 6 ) = ABS( LP( 6 ) )

*  Store the astrometry information in common.
      ACM_EPOCH( IDA ) = EPOCH
      ACM_PROJN( IDA ) = TPROJ

      DO IP = 1, NPREQ
         ACM_PROJP( IP, IDA ) = LP( IP )
      END DO

      ACM_SCS( IDA ) = TSCS

*  Indicate that the elements of the common arrays indexed by IDA
*  contain valid astrometry information.
      IF( STATUS .EQ. SAI__OK ) ACM_VALID( IDA ) = .TRUE.

*  If a valid NDF is specified by the argument INDF, call IRA_EXPRT to
*  store the astrometry information in an NDF.
      CALL NDF_VALID( INDF, VALID, STATUS )

      IF( VALID ) CALL IRA_EXPRT( IDA, INDF, STATUS )

*  If an error occurred, give the context, and set the IRA identifier
*  invalid.
 999  CONTINUE

      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'IRA_CREAT_ERR4',
     :                 'IRA_CREAT: Unable to create an identifier',
     :                 STATUS )
      END IF

      END
