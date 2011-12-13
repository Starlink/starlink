      SUBROUTINE NDF1_PLCRE( LOC, NAME, LOCPL, NEW, STATUS )
*+
*  Name:
*     NDF1_PLCRE

*  Purpose:
*     Create (or check) an NDF placeholder object.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_PLCRE( LOC, NAME, LOCPL, NEW, STATUS )

*  Description:
*     The routine may be used to create an NDF placeholder object which
*     identifies a position in the underlying data system where a newly
*     created NDF should be positioned. A pre-existing object may also
*     be used for this purpose so long as it is an empty scalar
*     structure of type NDF, and this routine may be used to check such
*     an object for validity. A primary HDS locator for the placeholder
*     object is returned along with an indication of whether a new
*     placeholder object was created.

*  Arguments:
*     LOC = CHARACTER * ( * ) (Given)
*        HDS locator which, in conjunction with the NAME argument,
*        identifies the structure which is to become a new NDF. A value
*        of DAT__ROOT may be supplied to indicate that the NAME
*        argument contains an absolute object name.
*     NAME = CHARACTER * ( * ) (Given)
*        Name to be used together with the LOC value to identify the
*        placeholder object. If LOC is set to DAT__ROOT, this should be
*        the absolute HDS name of the object, otherwise it should be a
*        relative name.
*     LOCPL = CHARACTER * ( * ) (Returned)
*        Locator to the placeholder object. This will be a primary
*        locator and will be linked into the HDS group NDF_PCB (to
*        prevent external events from annulling it).
*     NEW = LOGICAL (Returned)
*        Whether a new placeholder object was created.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  The object identified by the LOC and NAME arguments may or may
*     not already exist. If it does not, then it will be created by
*     this routine. Otherwise it will be checked for validity (to be
*     valid is should be an empty scalar structure of type NDF).
*     -  If a top-level object is specified, then a new container file
*     will be created (whether or not it already exists). Otherwise,
*     all structures lying above the specified object must already
*     exist.
*     -  If this routine is called with STATUS set, then an invalid
*     locator will be returned for the LOCPL argument, although no
*     further processing will occur. The same value will also be
*     returned if the routine should fail for any reason.

*  Implementation Deficiencies:
*     There is currently an asymmetry in the way this routine works, in
*     that top level objects (but excluding cells of such objects or
*     top level objects referred to directly by the locator LOC) are
*     over-written by the creation of a new container file if they
*     already exist and are never candidates for re-use. In contrast,
*     other objects which already exist are examined to ensure they are
*     empty scalar structures of type NDF and are then re-used if
*     possible (otherwise an error results). This behaviour is
*     historical and due partly to reliance on VMS (version numbers
*     being available on files but not on HDS components) and partly to
*     copying ADAM (which has a similar asymmetry in its parameter
*     system). This should be rationalised at some point.

*  Copyright:
*     Copyright (C) 1993,1994 Science & Engineering Research Council
*     Copyright (C) 2005 Particle Physics and Astronomy Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public
*     License along with this program; if not, write to the Free
*     Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
*     MA 02110-1301, USA

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     4-NOV-1993 (RFWS):
*        Original version.
*     9-MAR-1994 (RFWS):
*        Added NEW argument to indicate if a new object was created.
*     24-DEC-2005 (TIMJ):
*        Replace NDF1_HFIND with HDS_FIND
*     27-DEC-2005 (TIMJ):
*        Replace call to NDF1_HSPLT with call to HDS_SPLIT
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'NDF_ERR'          ! NDF_ error codes

*  Arguments Given:
      CHARACTER * ( * ) LOC
      CHARACTER * ( * ) NAME

*  Arguments Returned:
      CHARACTER * ( * ) LOCPL
      LOGICAL NEW

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      LOGICAL NDF1_ABSNT         ! Test for absent NDF or component

*  Local Variables:
      CHARACTER * ( DAT__SZTYP ) TYPE ! Object type
      INTEGER DIM( DAT__MXDIM )  ! Object dimension array
      INTEGER F1                 ! Index of start of file name
      INTEGER F2                 ! Index of end of file name
      INTEGER NCOMP              ! Number of structure components
      INTEGER NDIM               ! Number of object dimensions
      INTEGER P1                 ! Index of start of HDS path
      INTEGER P2                 ! Index of end of HDS path

*.

*  Set an initial null value for the LOCPL argument.
      LOCPL = DAT__NOLOC

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If a root locator has been supplied, then the NAME value will
*  include an HDS container file name. Split it into this file name and
*  an HDS path specification.
      IF ( LOC .EQ. DAT__ROOT ) THEN
         CALL HDS_SPLIT( NAME, F1, F2, P1, P2, STATUS )
         IF ( STATUS .EQ. SAI__OK ) THEN

*  If the path name does not exist, then the new NDF will be a top
*  level object requiring a new container file. In this case, we can
*  pretend that any existing object with the same name does not exist
*  (since we will create a new file).
            IF ( P1 .GT. P2 ) THEN
               NEW = .TRUE.

*  If a path name exists, then mark the error stack and attempt to find
*  the object.
            ELSE
               CALL ERR_MARK
               CALL HDS_FIND( LOC, NAME, 'UPDATE', LOCPL, STATUS )

*  If no such object was found, then note this fact and annul the
*  error.
               NEW = .FALSE.
               IF ( NDF1_ABSNT( STATUS ) ) THEN
                  NEW = .TRUE.
                  CALL ERR_ANNUL( STATUS )
               END IF
               CALL ERR_RLSE
            END IF
         END IF

*  If a root locator was not supplied and NAME is blank, then LOC must
*  identify the new object directly, so it must already exist. Clone a
*  locator to it.
      ELSE
         NEW = .FALSE.
         IF ( NAME .EQ. ' ' ) THEN
            CALL DAT_CLONE( LOC, LOCPL, STATUS )

*  Otherwise, defer error reporting and attempt to find the object.
         ELSE
            CALL ERR_MARK
            CALL HDS_FIND( LOC, NAME, 'UPDATE', LOCPL, STATUS )

*  If the object was not found, then note this fact and annul the
*  error.
            IF ( NDF1_ABSNT( STATUS ) ) THEN
               NEW = .TRUE.
               CALL ERR_ANNUL( STATUS )
            END IF
            CALL ERR_RLSE
         END IF
      END IF

*  If the object did not exist, then create it.
      IF ( STATUS .EQ. SAI__OK ) THEN
         IF ( NEW ) THEN
            CALL NDF1_HNEW( LOC, NAME, 'NDF', 0, DIM, LOCPL, STATUS )

*  If the object initially existed, then obtain its type and shape.
         ELSE
            CALL DAT_TYPE( LOCPL, TYPE, STATUS )
            CALL DAT_SHAPE( LOCPL, DAT__MXDIM, DIM, NDIM, STATUS )
            IF ( STATUS .EQ. SAI__OK ) THEN

*  Check that the type is 'NDF' and report an error if it is not.
               IF ( TYPE .NE. 'NDF' ) THEN
                  STATUS = NDF__TYPIN
                  CALL DAT_MSG( 'NDF', LOCPL )
                  CALL MSG_SETC( 'BADTYPE', TYPE )
                  CALL ERR_REP( 'NDF1_PLCRE_TYPE',
     : 'The object ^NDF has an invalid type of ''^BADTYPE''; it ' //
     : 'should be of type ''NDF''.',
     :                          STATUS )

*  Check that the object is scalar and report an error if it is not.
               ELSE IF ( NDIM .NE. 0 ) THEN
                  STATUS = NDF__NDMIN
                  CALL DAT_MSG( 'NDF', LOCPL )
                  CALL MSG_SETI( 'BADNDIM', NDIM )
                  CALL ERR_REP( 'NDF1_PLCRE_NDIM',
     : 'The object ^NDF is ^BADNDIM-dimensional; it should be scalar.',
     :                          STATUS )
               END IF

*  Determine the number of existing components in the structure. Report
*  an error if this is not zero.
               CALL DAT_NCOMP( LOCPL, NCOMP, STATUS )
               IF ( STATUS .EQ. SAI__OK ) THEN
                  IF ( NCOMP .NE. 0 ) THEN
                     STATUS = NDF__INUSE
                     CALL DAT_MSG( 'NDF', LOCPL )
                     CALL ERR_REP( 'NDF1_PLCRE_USED',
     : 'The NDF structure ^NDF is already in use; the structure is ' //
     : 'not empty).',
     :                             STATUS )
                  END IF
               END IF
            END IF
         END IF

*  Promote the locator to become a primary locator and link it into a
*  private group to prevent external events from annulling it.
         CALL DAT_PRMRY( .TRUE., LOCPL, .TRUE., STATUS )
         CALL HDS_LINK( LOCPL, 'NDF_PCB', STATUS )
      END IF

*  An error occurred, then annul the returned locator.
      IF ( STATUS .NE. SAI__OK ) CALL DAT_ANNUL( LOCPL, STATUS )

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_PLCRE', STATUS )

      END
