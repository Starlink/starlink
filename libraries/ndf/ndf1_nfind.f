      SUBROUTINE NDF1_NFIND( LOC, NAME, MODE, IACB, STATUS )
*+
*  Name:
*     NDF1_NFIND

*  Purpose:
*     Find an NDF and import it into the NDF_ system.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_NFIND( LOC, NAME, MODE, IACB, STATUS )

*  Description:
*     The routine finds an NDF within an HDS structure or container
*     file, imports it into the NDF_ system and returns the index of
*     the ACB entry allocated to it. The object name supplied may
*     include an NDF section specification, in which case an NDF
*     section entry in the ACB will be returned. Otherwise a base NDF
*     entry will be returned.

*  Arguments:
*     LOC = CHARACTER * ( * ) (Given)
*        HDS locator to the structure containing the NDF.
*     NAME = CHARACTER * ( * ) (Given)
*        Name of the structure component (i.e. the NDF).
*     MODE = CHARACTER * ( * ) (Given)
*        Mode of access required: 'READ', 'UPDATE' or 'WRITE' (this is
*        only used if LOC is set to DAT__ROOT, otherwise the mode of
*        access is derived from the input locator).
*     IACB = INTEGER (Returned)
*        Index of the new NDF entry in the ACB.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  The value given for the NAME argument may be an HDS path name,
*     consisting of several fields separated by '.', so that an NDF can
*     be found in a sub-component (or a sub-sub-component...) of the
*     structure identified by the locator LOC.  Array subscripts may
*     also be used in this component name.  Thus a string such as
*     'MYSTRUC.ZONE(2).IMAGE' could be used as a valid NAME value.
*     -  An NDF can be accessed within an explicitly named container
*     file by supplying the symbolic value DAT__ROOT for the LOC
*     argument, and specifying the container file within the value
*     supplied for the NAME argument.
*     -  If a blank value is given for the NAME argument, then the NDF
*     to be imported will be the object identified directly by the
*     locator LOC.
*     -  If this routine is called with STATUS set, then a value of
*     zero will be returned for the IACB argument, although no further
*     processing will occur.  The same value will also be returned if
*     the routine should fail for any reason.

*  Copyright:
*     Copyright (C) 1993, 1996 Science & Engineering Research Council
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
*     DSB: David S Berry (JAC, UCLan)
*     {enter_new_authors_here}

*  History:
*     11-AUG-1993 (RFWS):
*        New routine to use new NDF1_HFIND routine which accepts
*        DAT__ROOT locator values.
*     25-JUL-1996 (RFWS):
*        Fixed bug: when extracting a slice or cell from an HDS
*        structure array, the resulting locator was not being promoted
*        to a primary locator when necessary. Hence the container file
*        was being closed.
*     24-DEC-2005 (TIMJ):
*        Replace NDF1_HFIND with HDS_FIND
*     2-NOV-2007 (DSB):
*        Report an error if the supplied HDS object is not scalar.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'NDF_PAR'          ! NDF_ public constants
      INCLUDE 'NDF_ERR'          ! NDF_ error constants
      INCLUDE 'NDF_CONST'        ! NDF_ private constants

*  Arguments Given:
      CHARACTER * ( * ) LOC
      CHARACTER * ( * ) NAME
      CHARACTER * ( * ) MODE

*  Arguments Returned:
      INTEGER IACB

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( DAT__SZLOC ) LOC1 ! Temporary locator
      CHARACTER * ( DAT__SZLOC ) LOC2 ! Temporary locator
      CHARACTER * ( NDF__SZMOD ) VMODE ! Validated access mode string
      INTEGER DIM( DAT__MXDIM )  ! HDS object dimensions
      INTEGER IACB0              ! Index to base ACB entry
      INTEGER N1                 ! Start of HDS object name
      INTEGER N2                 ! End of HDS object name
      INTEGER NDIM               ! Number of HDS object dimensions
      INTEGER S1                 ! Start of possible section spec.
      INTEGER S2                 ! End of possible section spec.
      LOGICAL SECT               ! Final component has to be a section?

*.

*  Set an initial value for the IACB argument.
      IACB = 0

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If necessary, check the access mode string for validity. Otherwise,
*  check that the supplied locator is not a locator for an array of
*  objects.
      VMODE = 'UPDATE'
      IF ( LOC .EQ. DAT__ROOT ) THEN
         CALL NDF1_VMOD( MODE, VMODE, STATUS )
      ELSE
         CALL DAT_SHAPE( LOC, NDF__MXDIM, DIM, NDIM, STATUS )
         IF( NDIM .NE. 0 .AND. STATUS .EQ. SAI__OK ) THEN
            STATUS = NDF__DIMIN
            CALL MSG_SETI( 'ND', NDIM )
            CALL ERR_REP( 'NDF1_NFIND_ERR1', 'The supplied HDS object'//
     :                    ' is a ^ND-dimensional array. It must be a '//
     :                    'scalar object (possible programming error).',
     :                    STATUS )
         END IF
      END IF

*  Split the NDF name into an HDS object name and a section
*  specification.
      CALL NDF1_NSPLT( NAME, ( LOC .NE. DAT__ROOT ), N1, N2, S1, S2,
     :                 STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Obtain a locator to the required object.
         IF ( N1 .LE. N2 ) THEN
            CALL HDS_FIND( LOC, NAME( N1 : N2 ), VMODE, LOC1, STATUS )
         ELSE
            CALL HDS_FIND( LOC, ' ', VMODE, LOC1, STATUS )
         END IF

*  See if the HDS name ends in ')'. If so, then it has already been
*  subscripted, so any further parenthesised expression must be an NDF
*  section specification.
         SECT = .FALSE.
         IF ( N1 .LE. N2 ) SECT = ( NAME( N2 : N2 ) .EQ. ')' )

*  If there appears to be a section specification on the end of the NDF
*  name, then obtain the shape of the HDS object.
         IF ( S1 .LE. S2 ) THEN
            CALL DAT_SHAPE( LOC1, DAT__MXDIM, DIM, NDIM, STATUS )
            IF ( STATUS .EQ. SAI__OK ) THEN

*  If the final component has to be a section specification (or,
*  equivalently, if the object is scalar), then import the object into
*  the NDF system and then cut the required NDF section from it.
               IF ( SECT .OR. ( NDIM .EQ. 0 ) ) THEN
                  CALL NDF1_IMP( LOC1, IACB0, STATUS )
                  CALL NDF1_NCUT( IACB0, NAME( S1 : S2 ), IACB, STATUS )

*  Annul the original base NDF entry in the ACB.
                  CALL NDF1_ANL( IACB0, STATUS )

*  If the final component is not an NDF section specification, then it
*  must be an HDS subscript expression. Cut the appropriate slice/cell
*  from the HDS object.
               ELSE
                  CALL HDS_FIND( LOC1, NAME( S1 : S2 ), VMODE, LOC2,
     :                             STATUS )

*  Promote the resulting locator to be a primary locator, if necessary,
*  before annulling the original and replacing it with the new locator.
                  IF ( LOC .EQ. DAT__ROOT ) THEN
                     CALL DAT_PRMRY( .TRUE., LOC2, .TRUE., STATUS )
                  END IF
                  CALL DAT_ANNUL( LOC1, STATUS )
                  LOC1 = LOC2
                  LOC2 = DAT__NOLOC

*  Import the object into the NDF system.
                  CALL NDF1_IMP( LOC1, IACB, STATUS )
               END IF
            END IF

*  If there does not appear to be a section specification, then simply
*  import the object.
         ELSE
            CALL NDF1_IMP( LOC1, IACB, STATUS )
         END IF

*  Annul the HDS object locator.
         CALL DAT_ANNUL( LOC1, STATUS )
      END IF

*  If an error occurred, then annul any ACB entry which may have been
*  acquired and call the error tracing routine.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL NDF1_ANL( IACB, STATUS )
         CALL NDF1_TRACE( 'NDF1_NFIND', STATUS )
      END IF

      END
