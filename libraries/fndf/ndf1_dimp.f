      SUBROUTINE NDF1_DIMP( LOC, IDCB, STATUS )
*+
*  Name:
*     NDF1_DIMP

*  Purpose:
*     Import a data object, creating a new DCB entry for it.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_DIMP( LOC, IDCB, STATUS )

*  Description:
*     The routine imports an NDF structure into the NDF_ system,
*     creating a new DCB entry for it.

*  Arguments:
*     LOC = CHARACTER * ( * ) (Given)
*        HDS locator to the NDF structure to be imported.
*     IDCB = INTEGER (Returned)
*        Index to the new DCB entry for the data object.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  The routine makes a "cloned" copy of the HDS locator supplied;
*     the latter may later be annulled without affecting the operation
*     of the NDF_ system.
*     -  If STATUS is set on entry, then the routine will return a
*     value of zero for the IDCB argument, although no further
*     processing will occur.
*     -  A value of zero will also be returned for the IDCB argument if
*     the routine fails.

*  Copyright:
*     Copyright (C) 1993 Science & Engineering Research Council

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
*     RFWS: R.F. Warren-Smith (STARLINK)
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     26-SEP-1989 (RFWS):
*        Original, derived from the equivalent ARY_ system routine.
*     29-SEP-1989 (RFWS):
*        Added initialisation of DCB flags for all NDF and axis
*        components.
*     3-OCT-1989 (RFWS):
*        Added checks on the VARIANT component.
*     12-DEC-1989 (RFWS):
*        Added initialisation of the total mapping count for the NDF.
*     29-JAN-1990 (RFWS):
*        Added check that a scalar object which might be an NDF is not
*        primitive.
*     19-MAR-1990 (RFWS):
*        Temporarily removed warning message about non-standard NDF data
*        types until existing datasets are largely converted.
*     26-MAR-1990 (RFWS):
*        Added initialisation of the quality bad-bits entries in the
*        DCB.
*     1-AUG-1990 (RFWS):
*        Changed subscript order in DCB axis character component
*        arrays.
*     15-NOV-1990 (RFWS):
*        Removed unnecessary DCB initialisations, which are now
*        performed by NDF1_FFS.
*     16-NOV-1990 (RFWS):
*        Re-structured the way that the DCB locator is cloned and stored
*        in the DCB.
*     2-JAN-1991 (RFWS):
*        Changed erroneous call to MSG_SETC to use MSG_SETI.
*     4-OCT-1991 (RFWS):
*        Added use of TCB warning message flag.
*     2-DEC-1991 (RFWS):
*        Changed warning flag behavior - continue without setting STATUS
*        if it is not set.
*     17-JAN-1992 (RFWS):
*        Added handling of mapped character length for UNIX
*        compatibility.
*     20-JAN-1992 (RFWS):
*        Changed '_CHAR' to '_CHAR*' when testing for character data
*        type.
*     8-JUN-1993 (DSB):
*        Call to DAT_PRMRY included to promote the locator to the data
*        object stored in the DCB to a primary locator.
*     9-MAR-1994 (RFWS):
*        Ensure that the TCB is initialised.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'NDF_PAR'          ! NDF_ public constants
      INCLUDE 'NDF_CONST'        ! NDF_ private constants
      INCLUDE 'NDF_ERR'          ! NDF_ error codes

*  Global Variables:
      INCLUDE 'NDF_DCB'          ! NDF_ Data Control Block
*        DCB_FILE( NDF__MXDCB ) = CHARACTER * ( NDF__SZFIL ) (Write)
*           Data object container file name.
*        DCB_KD( NDF__MXDCB ) = LOGICAL (Write)
*           Whether data array information is available.
*        DCB_LOC( NDF__MXDCB ) = CHARACTER * ( DAT__SZLOC ) (Write)
*           Data object locator.
*        DCB_PATH( NDF__MXDCB ) = CHARACTER * ( NDF__SZPTH ) (Write)
*           Data object HDS path name.

      INCLUDE 'NDF_TCB'          ! NDF_ Tuning Control Block
*        TCB_WARN = LOGICAL (Read)
*           Warning message flag.

*  Arguments Given:
      CHARACTER * ( * ) LOC

*  Arguments Returned:
      INTEGER IDCB

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      LOGICAL CHR_SIMLR          ! Case insensitive string comparison

*  Local Variables:
      CHARACTER * ( DAT__SZLOC ) LOCVAR ! Locator to VARIANT component
      CHARACTER * ( DAT__SZTYP ) TYPE ! HDS data type
      INTEGER DIM( DAT__MXDIM )  ! Object dimensions
      INTEGER DUMMY( 1 )         ! Dummy dimension array
      INTEGER LENV               ! Length of mapped character value
      INTEGER NDIM               ! Number of object dimensions
      INTEGER NLEV               ! Levels in HDS path name
      INTEGER PNTR               ! Pointer to mapped VARIANT value
      LOGICAL PRIM               ! Whether object is a primitive
      LOGICAL THERE              ! Whether VARIANT component is present

*.

*  Set an initial value of zero for the IDCB argument.
      IDCB = 0

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Ensure that the TCB is initialised.
      CALL NDF1_INTCB( STATUS )

*  Obtain the data object's type, see if it is primitive, and determine
*  its shape.
      CALL DAT_TYPE( LOC, TYPE, STATUS )
      CALL DAT_PRIM( LOC, PRIM, STATUS )
      CALL DAT_SHAPE( LOC, DAT__MXDIM, DIM, NDIM, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Report an error if the object is not scalar.
         IF ( NDIM .NE. 0 ) THEN
            STATUS = NDF__NDMIN
            CALL DAT_MSG( 'NDF', LOC )
            CALL MSG_SETI( 'BADNDIM', NDIM )
            CALL ERR_REP( 'NDF1_DIMP_NDIM',
     :      'The NDF structure ^NDF is ^BADNDIM-dimensional; it ' //
     :      'should be scalar.', STATUS )

*  If it is a primitive, then it cannot contain NDF components, so
*  report an error.
         ELSE IF ( PRIM ) THEN
            STATUS = NDF__TYPIN
            CALL DAT_MSG( 'NDF', LOC )
            CALL MSG_SETC( 'BADTYPE', TYPE )
            CALL ERR_REP( 'NDF1_DIMP_TYPE',
     :      'The NDF structure ^NDF has an invalid data type of ' //
     :      '''^BADTYPE''; it should be a structure.', STATUS )

*  If its type is not NDF, then report and flush a warning message, but
*  do not set STATUS or alter the error stack.
c  <Temporarily removed for compatibilty with existing data structures>
c         ELSE IF ( TYPE .NE. 'NDF' ) THEN
c            CALL ERR_MARK
c            STATUS = NDF__TYPIN
c            CALL DAT_MSG( 'NDF', LOC )
c            CALL MSG_SETC( 'TYPE', TYPE )
c            CALL ERR_REP( 'NDF1_DIMP_TYPE',
c     :      'Warning: the NDF structure ^NDF has a non-standard ' //
c     :      'type of ''^TYPE''.', STATUS )
c            CALL ERR_FLUSH( STATUS )
c            CALL ERR_RLSE
         END IF
      END IF

*  See if a VARIANT component is present in the NDF structure.
      CALL DAT_THERE( LOC, 'VARIANT', THERE, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*  If so, then obtain a locator to it and determine its type and shape.
         IF ( THERE ) THEN
            CALL DAT_FIND( LOC, 'VARIANT', LOCVAR, STATUS )
            CALL DAT_TYPE( LOCVAR, TYPE, STATUS )
            CALL DAT_SHAPE( LOCVAR, DAT__MXDIM, DIM, NDIM, STATUS )
            IF ( STATUS .EQ. SAI__OK ) THEN

*  Check that the VARIANT is a character object and report an error if
*  it is not.
               IF ( TYPE( : 6 ) .NE. '_CHAR*' ) THEN
                  STATUS = NDF__TYPIN
                  CALL DAT_MSG( 'NDF', LOC )
                  CALL MSG_SETC( 'BADTYPE', TYPE )
                  CALL ERR_REP( 'NDF1_DIMP_VTYP',
     :            'The VARIANT component in the NDF structure ^NDF' //
     :            'has an invalid HDS type of ''^BADTYPE''; it ' //
     :            'should be of type ''_CHAR''.', STATUS )

*  Check that it is scalar and report an error if it is not.
               ELSE IF ( NDIM .NE. 0 ) THEN
                  STATUS = NDF__NDMIN
                  CALL DAT_MSG( 'NDF', LOC )
                  CALL MSG_SETI( 'BADNDIM', NDIM )
                  CALL ERR_REP( 'NDF1_DIMP_NDMV',
     :            'The VARIANT component in the NDF structure ^NDF ' //
     :            'is ^BADNDIM-dimensional; it should be scalar.',
     :            STATUS )

*  If the VARIANT component is OK so far, then map it and determine its
*  length.
               ELSE
                  DUMMY( 1 ) = 0
                  CALL DAT_MAPC( LOCVAR, 'READ', 0, DUMMY, PNTR,
     :                           STATUS )
                  CALL DAT_CLEN( LOCVAR, LENV, STATUS )
                  IF ( STATUS .EQ. SAI__OK ) THEN

*  If its value is not 'SIMPLE', and the TCB_WARN flag is set, then
*  issue a warning message (do not leave STATUS set or modify the error
*  stack because a lot of people have already abused this component for
*  their own use and they'd probably be annoyed if the NDF_ routines
*  refused to handle their data as a result).
                     IF ( .NOT. CHR_SIMLR( %VAL( CNF_PVAL( PNTR ) ),
     :                                     'SIMPLE',
     :                                     %VAL( CNF_CVAL( LENV ) ) ) )
     :THEN
                        IF ( TCB_WARN ) THEN
                           CALL ERR_MARK
                           STATUS = NDF__VARIN
                           CALL DAT_MSG( 'NDF', LOC )
                           CALL NDF1_SETC( %VAL( CNF_PVAL( PNTR ) ),
     :                                     'BADVARIANT',
     :                                     %VAL( CNF_CVAL( LENV ) ) )
                           CALL ERR_REP( 'NDF1_DIMP_WVRNT',
     :                     'Warning: the VARIANT component in the ' //
     :                     'NDF structure ^NDF has an invalid ' //
     :                     'value of ''^BADVARIANT''; only the ' //
     :                     'value ''SIMPLE'' is defined.', STATUS )
                           CALL ERR_FLUSH( STATUS )
                           CALL ERR_RLSE
                        END IF
                     END IF
                  END IF
               END IF
            END IF

*  Annul the locator to the VARIANT component.
            CALL DAT_ANNUL( LOCVAR, STATUS )
         END IF
      END IF

*  Obtain an index to a free slot in the DCB.
      CALL NDF1_FFS( NDF__DCB, IDCB, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Clone the NDF locator supplied, storing the new locator in the DCB.
*  Promote it to a primary locator and link it into a private group
*  (to prevent any external events from annulling it without the NDF_
*  system's knowledge).
         CALL DAT_CLONE( LOC, DCB_LOC( IDCB ), STATUS )
         CALL DAT_PRMRY( .TRUE., DCB_LOC( IDCB ), .TRUE., STATUS )
         CALL HDS_LINK( DCB_LOC( IDCB ), 'NDF_DCB', STATUS )

*  Obtain the data object file and path names and enter them into the
*  DCB.
         CALL HDS_TRACE( DCB_LOC( IDCB ), NLEV, DCB_PATH( IDCB ),
     :                   DCB_FILE( IDCB ), STATUS )

*  If there was an error, then annul the cloned locator and release the
*  slot allocated in the DCB.
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL DAT_ANNUL( DCB_LOC( IDCB ), STATUS )
            CALL NDF1_RLS( NDF__DCB, IDCB, STATUS )
         END IF
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_DIMP', STATUS )

      END
