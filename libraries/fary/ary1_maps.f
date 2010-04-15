      SUBROUTINE ARY1_MAPS( IACB, TYPE, CMPLX, MMOD, DPNTR, IPNTR,
     :                      STATUS )
*+
*  Name:
*     ARY1_MAPS

*  Purpose:
*     Map a simple array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARY1_MAPS( IACB, TYPE, CMPLX, MMOD, DPNTR, IPNTR, STATUS )

*  Description:
*     The routine maps the data in a simple array using the specified
*     data access type and mapping mode.

*  Arguments:
*     IACB = INTEGER (Given)
*        Index to an entry in the ACB identifying the array to be
*        mapped.
*     TYPE = CHARACTER * ( * ) (Given)
*        The data access type required; an HDS primitive numeric data
*        type string (case insensitive).
*     CMPLX = LOGICAL (Given)
*        Whether the data access type is complex (i.e. whether access
*        is required to an imaginary component in addition to the
*        non-imaginary component).
*     MMOD = CHARACTER * ( * ) (Given)
*        The data mapping mode; 'READ', 'UPDATE' or 'WRITE', with an
*        optional initialisation mode '/ZERO' or '/BAD' appended.
*     DPNTR = INTEGER (Returned)
*        Pointer to the mapped non-imaginary component of the data.
*     IPNTR = INTEGER (Returned)
*        Pointer to the mapped imaginary component of the data (this
*        argument is not used unless CMPLX is .TRUE.).
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  This routine may also be used to map a scaled or primitive array.

*  Algorithm:
*     -  Validate the access data type string.
*     -  Validate the mapping mode string.
*     _  Check that the array is not already mapped for access through
*     the ACB entry supplied. Report an error if it is.
*     -  Check the array access mode to ensure the requested access is
*     permitted.
*     -  Find a free slot in the MCB.
*     -  Obtain mapping region bounds information and enter it into the
*     MCB.
*     -  Check for possible conflicting mapped access.
*     -  Ensure that state information is available for the data object.
*     -  Calculate the dimension sizes of the array and the number of
*     elements to be mapped.
*     -  If the mapping mode is READ or UPDATE and the array's values
*     are defined, then map the non-imaginary data component for
*     reading.
*     -  If complex data are required and an imaginary component
*     exists, then map it for reading. If no such component exists,
*     then create and map a temporary object and fill it with zeros.
*     -  Note whether the mapped data may contain "bad" values in the
*     MCB.
*     -  If the mapping mode is WRITE, or it is UPDATE with an
*     initialisation option in the case where the array's values are
*     undefined, then see if the data object's data values have been
*     initialised.
*     -  If not, then see if the region to which write access is being
*     obtained will result in values being entered into the entire data
*     object.
*     -  If not, then calculate the dimension bounds and number of
*     elements for each array component, map the entire non-imaginary
*     component and initialise it to "bad" values. Then unmap it.
*     -  If an imaginary component exists, then repeat this process for
*     it also.
*     -  Map the required region of the non-imaginary data component
*     for writing.
*     -  If complex data are required and an imaginary component
*     exists, then map it for writing (which also initialises the
*     mapped region according to the initialisation option specified in
*     the mapping mode string).  If no such component exists, then
*     create and map a temporary object and initialise it separately.
*     -  Note whether the mapped data may contain "bad" values in the
*     MCB.
*     -  If the access mode is READ with an initialisation option and
*     the array's values are undefined, then create and map a temporary
*     object to hold the mapped values and initialise it appropriately.
*     -  If access to complex values is required, then repeat this
*     operation for the imaginary component.
*     -  In all other cases, it is necessary to read values from an
*     array whose values are undefined without an initialisation
*     option, so report an error.
*     -  If mapping was successful, then return the mapped data
*     pointers and increment the DCB counts of the number of current
*     read and write mapped accesses.
*     -  If mapping failed, then reset the ACB index into the MCB
*     (indicating the array is not mapped) and release the MCB slot.

*  Copyright:
*     Copyright (C) 1989, 1990, 1991 Science & Engineering Research Council.
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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     DSB: David S Berry (JAC)
*     {enter_new_authors_here}

*  History:
*     27-JUN-1989 (RFWS):
*        Original version.
*     4-SEP-1989 (RFWS):
*        Removed calls to routines which no longer exist.
*     4-SEP-1989 (RFWS):
*        Corrected error in name of ARY1_VTYP routine.
*     7-SEP-1989 (RFWS):
*        Added STATUS check after call to ARY1_FFS, to prevent array
*        bounds being exceeded under error conditions.
*     7-SEP-1989 (RFWS):
*        Changed to use new argument list for ARY1_GMRB.
*     12-SEP-1989 (RFWS):
*        Changed to avoid releasing an MCB slot under error conditions
*        if a slot was not acquired successfully in the first place.
*     12-SEP-1989 (RFWS):
*        Changed to check that the array is not already mapped for
*        access through the ACB entry supplied.
*     18-SEP-1989 (RFWS):
*        Implemented data initialisation using the DCB_INIT array.
*     16-JAN-1990 (RFWS):
*        Changed to allow initialisation options to be specified in the
*        mapping mode together with any access type. Also corrected a
*        bug whereby an unnecessary pass through the array's values to
*        set them to zero was being made.
*     11-NOV-1991 (RFWS):
*        Temporarily removed initialisation of array regions outside of
*        sections accessed in write mode for the first time. This is to
*        improve efficiency for applications which process entire
*        arrays in pieces. A flag to control this behaviour will have
*        to be added later.
*     17-JUL2006 (DSB):
*        Add a call to ARY1_DOBJ to ensure that any deferred arrays have
*        been created.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'ARY_PAR'          ! ARY_ public constants
      INCLUDE 'ARY_CONST'        ! ARY_ private constants
      INCLUDE 'ARY_ERR'          ! ARY_ error codes

*  Global Variables:
      INCLUDE 'ARY_DCB'          ! ARY_ Data Control Block
*        DCB_CPX( ARY__MXDCB ) = LOGICAL (Read)
*           Whether the data object is complex.
*        DCB_DLOC( ARY__MXDCB ) = CHARACTER * ( DAT__SZLOC ) (Read)
*           Locator to non-imaginary data component.
*        DCB_ILOC( ARY__MXDCB ) = CHARACTER * ( DAT__SZLOC ) (Read)
*           Locator to imaginary data component.
*        DCB_INIT( ARY__MXDCB ) = LOGICAL (Read and Write)
*           Whether the data object's values have been initialised.
*        DCB_LOC( ARY__MXDCB ) = CHARACTER * ( DAT__SZLOC ) (Read)
*           Locator to data object.
*        DCB_NREAD( ARY__MXDCB ) = INTEGER (Read and Write)
*           Number of active read accesses to the data object via
*           mapping.
*        DCB_NWRIT( ARY__MXDCB ) = INTEGER (Read and Write)
*           Number of active write accesses to the data object via
*           mapping.
*        DCB_STA( ARY__MXDCB ) = LOGICAL (Read)
*           Whether the array's data values are defined.

      INCLUDE 'ARY_ACB'          ! ARY_ Access Control Block
*        ACB_BAD( ARY__MXACB ) = LOGICAL (Read)
*           Whether it is necessary to check for "bad" values in the
*           data transfer window (if it exists).
*        ACB_IDCB( ARY__MXACB ) = INTEGER (Read)
*           Index to the data object entry in the DCB.
*        ACB_IMCB( ARY__MXACB ) = INTEGER (Read and Write)
*           Index to the mapping entry in the MCB.
*        ACB_LBND( ARY__MXDIM, ARY__MXACB ) = INTEGER (Read)
*           Lower bound of array region to be accessed.
*        ACB_NDIM( ARY__MXACB ) = INTEGER (Read)
*           Number of access dimensions.
*        ACB_UBND( ARY__MXDIM, ARY__MXACB ) = INTEGER (Read)
*           Upper bound of array region to be accessed.

      INCLUDE 'ARY_MCB'          ! ARY_ Mapping Control Block
*        MCB_AMM( ARY__MXMCB ) = CHARACTER * ( ARY__SZAMM ) (Write)
*           Active mapping mode.
*        MCB_BAD( ARY__MXMCB ) = LOGICAL (Write)
*           Whether there may be "bad" values in the mapped data lying
*           within the mapping transfer window (if it exists).
*        MCB_CPX( ARY__MXMCB ) = LOGICAL (Write)
*           Whether the data access type is complex.
*        MCB_DCOPY( ARY__MXMCB ) = LOGICAL (Write)
*           Whether access to the non-imaginary data component is via a
*           "copy" of the actual data.
*        MCB_DLOC( ARY__MXMCB ) = CHARACTER * ( DAT__SZLOC ) (Write)
*           Locator to object mapped to provide access to the
*           non-imaginary data component.
*        MCB_DPNTR( ARY__MXMCB ) = INTEGER (Write)
*           Pointer to the mapped non-imaginary data component.
*        MCB_ICOPY( ARY__MXMCB ) = LOGICAL (Write)
*           Whether access to the imaginary data component is via a
*           "copy" of the actual data.
*        MCB_ILOC( ARY__MXMCB ) = CHARACTER * ( DAT__SZLOC ) (Write)
*           Locator to object mapped to provide access to the imaginary
*           data component.
*        MCB_IPNTR( ARY__MXMCB ) = INTEGER (Write)
*           Pointer to the mapped imaginary data component.
*        MCB_LMRB( ARY__MXDIM, ARY__MXMCB ) = INTEGER (Write)
*           Lower mapping region bounds.
*        MCB_LMTR( ARY__MXDIM, ARY__MXMCB ) = INTEGER (Write)
*           Lower bounds of mapping transfer region.
*        MCB_MRFUL( ARY__MXMCB ) = LOGICAL (Write)
*           Whether the mapping transfer region fills the mapping
*           region.
*        MCB_MTREX( ARY__MXMCB ) = LOGICAL (Write)
*           Whether a mapping transfer region exists.
*        MCB_PBAD( ARY__MXMCB ) = LOGICAL (Write)
*           Whether there may be "bad" values in the mapped data in the
*           padding region (if it exists) which surrounds the mapping
*           transfer region.
*        MCB_TYP( ARY__MXMCB ) = CHARACTER * ( ARY__SZTYP ) (Write)
*           Data access type.
*        MCB_UMRB( ARY__MXDIM, ARY__MXMCB ) = INTEGER (Write)
*           Upper mapping region bounds.
*        MCB_UMTR( ARY__MXDIM, ARY__MXMCB ) = INTEGER (Write)
*           Upper bounds of mapping transfer region.
*        MCB_WHOLE( ARY__MXMCB ) = LOGICAL (Write)
*           Whether the whole data object should be mapped.

*  Arguments Given:
      INTEGER IACB
      CHARACTER * ( * ) TYPE
      LOGICAL CMPLX
      CHARACTER * ( * ) MMOD

*  Arguments Returned:
      INTEGER DPNTR
      INTEGER IPNTR

*  Status:
      INTEGER STATUS             ! Global status

*  Local variables:
      CHARACTER * ( ARY__SZIOP ) INOPT ! Initialisation option
      CHARACTER * ( ARY__SZMOD ) MODE ! Data access mode
      CHARACTER * ( ARY__SZTYP ) VTYPE ! Validated data type string
      INTEGER DIM( ARY__MXDIM )  ! Array (ACB) dimension sizes
C     INTEGER DIMI( ARY__MXDIM ) ! Data object (DCB) dimension sizes
      INTEGER EL                 ! Number of array elements to be mapped
C     INTEGER ELI                ! Number of elements to initialise
      INTEGER I                  ! Loop counter for dimensions
      INTEGER IDCB               ! Index to data object entry in the DCB
      INTEGER IMCB               ! Index to mapping entry in the MCB
C     INTEGER PNTR               ! Pointer to mapped data component
      LOGICAL BAD                ! Whether to check for "bad" values
      LOGICAL DCE                ! Data conversion error?
      LOGICAL ENTIRE             ! Entire object filled with values?
      LOGICAL IDCE               ! Imaginary data conversion error?

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Validate the access data type string.
      CALL ARY1_VTYP( TYPE, VTYPE, STATUS )

*  Validate the mapping mode string, decomposing it into an access mode
*  and an initialisation option.
      CALL ARY1_VMMD( MMOD, MODE, INOPT, STATUS )

*  Check to see if the array is already mapped for access. Report an
*  error if it is.
      IF ( ACB_IMCB( IACB ) .NE. 0 ) THEN
         STATUS = ARY__ISMAP
         IDCB = ACB_IDCB( IACB )
         CALL DAT_MSG( 'ARRAY', DCB_LOC( IDCB ) )
         CALL ERR_REP( 'ARY1_MAPS_MAP',
     :   'The array ^ARRAY is already mapped for access through ' //
     :   'the specified identifier (possible programming error).',
     :   STATUS )

*  Check the array access mode to ensure the requested access is
*  permitted.
      ELSE
         CALL ARY1_CHMOD( IACB, MODE, STATUS )

*  Find a free slot in the MCB and store the MCB index in the ACB.
         CALL ARY1_FFS( ARY__MCB, IMCB, STATUS )
         IF ( STATUS .EQ. SAI__OK ) THEN
            ACB_IMCB( IACB ) = IMCB

*  Obtain mapping region bounds information, entering this into the
*  MCB.
            CALL ARY1_GMRB( IACB, MCB_MTREX( IMCB ), MCB_MRFUL( IMCB ),
     :                      MCB_WHOLE( IMCB ),
     :                      MCB_LMRB( 1, IMCB ), MCB_UMRB( 1, IMCB ),
     :                      MCB_LMTR( 1, IMCB ), MCB_UMTR( 1, IMCB ),
     :                      STATUS )

*  Check for possible conflicting mapped access to the same data object.
            CALL ARY1_CHCMA( IACB, MODE, STATUS )

*  Obtain the data object index in the DCB and ensure that state
*  information and a data object is available for the DCB entry.
            IDCB = ACB_IDCB( IACB )
            CALL ARY1_DSTA( IDCB, STATUS )
            CALL ARY1_DOBJ( IDCB, STATUS )
            IF ( STATUS .EQ. SAI__OK ) THEN

*  Calculate the dimension sizes of the array to be mapped and the
*  total number of values to be mapped.
               EL = 1
               DO 1 I = 1, ACB_NDIM( IACB )
                  DIM( I ) = ACB_UBND( I, IACB ) -
     :                       ACB_LBND( I, IACB ) + 1
                  EL = EL * DIM( I )
1              CONTINUE

*  Case 1.
*  =======
*  If the mapping mode is READ or UPDATE and the array's data values
*  are defined, then data must be read from the data object. See
*  whether it is necessary to check for "bad" values, then map the
*  non-imaginary array component for reading.
               IF ( ( ( MODE .EQ. 'READ' ) .OR.
     :                ( MODE .EQ. 'UPDATE' ) ) .AND.
     :              DCB_STA( IDCB ) ) THEN
                  BAD = ACB_BAD( IACB )
                  CALL ARY1_MPSR( IACB, DCB_DLOC( IDCB ), VTYPE, MODE,
     :                            BAD,
     :                            MCB_DLOC( IMCB ), MCB_DCOPY( IMCB ),
     :                            MCB_DPNTR( IMCB ), DCE, STATUS )

*  If access to the imaginary component is required and such a
*  component exists, then map it for reading, noting whether a data
*  conversion error occurs.
                  IF ( STATUS .EQ. SAI__OK ) THEN
                     IF ( CMPLX ) THEN
                        IF ( DCB_CPX( IDCB ) ) THEN
                           IDCE = .FALSE.
                           CALL ARY1_MPSR( IACB, DCB_ILOC( IDCB ),
     :                                     VTYPE, MODE, BAD,
     :                                     MCB_ILOC( IMCB ),
     :                                     MCB_ICOPY( IMCB ),
     :                                     MCB_IPNTR( IMCB ),
     :                                     IDCE, STATUS )
                           DCE = DCE .OR. IDCE

*  If access to an imaginary component is required, but such a
*  component does not exist, then a "copy" must be used. Create and map
*  a temporary object and fill it with zeros.
                        ELSE
                           MCB_ICOPY( IMCB ) = .TRUE.
                           CALL ARY1_CMTMP( VTYPE, ACB_NDIM( IACB ),
     :                                      DIM, MCB_ILOC( IMCB ),
     :                                      MCB_IPNTR( IMCB ), STATUS )
                           CALL ARY1_VZERO( VTYPE, EL,
     :                                      MCB_IPNTR( IMCB ), STATUS )
                        END IF
                     END IF

*  Note whether the mapped data may contain "bad" values in (a) the
*  mapping transfer region and (b) the padding region which surrounds
*  the actual mapped data (if this exists).
                     MCB_BAD( IMCB ) = ACB_BAD( IACB ) .OR. DCE
                     MCB_PBAD( IMCB ) = .TRUE.
                  END IF

*  Case 2.
*  =======
*  If the mapping mode is WRITE, or it is UPDATE with an initialisation
*  option where the arrays's values are undefined, then data must be
*  written to the data object when it is subsequently unmapped, but
*  there is no need to read initial values now.
               ELSE IF ( ( MODE .EQ. 'WRITE' ) .OR.
     :                   ( ( MODE .EQ. 'UPDATE' ) .AND.
     :                     ( INOPT .NE. ' ' ) .AND.
     :                     ( .NOT. DCB_STA( IDCB ) ) ) ) THEN

*  If the data object's values have not yet been initialised, then see
*  if the region being mapped covers the entire object.
                  IF ( .NOT. DCB_INIT( IDCB ) ) THEN
                     CALL ARY1_INBND( ARY__MXDIM, MCB_LMTR( 1, IMCB ),
     :                                MCB_UMTR( 1, IMCB ),
     :                                DCB_NDIM( IDCB ),
     :                                DCB_LBND( 1, IDCB ),
     :                                DCB_UBND( 1, IDCB ), ENTIRE,
     :                                STATUS )

*  If the region to which WRITE access is being obtained does not cover
*  the entire data object, then the object must first be initialised by
*  filling it with "bad" values so that undefined data elements don't
*  get left around the edges.
C                     IF ( STATUS .EQ. SAI__OK ) THEN
C                        IF ( .NOT. ENTIRE ) THEN

*  Calculate the dimension sizes and the number of elements in the
*  actual data object for initialisation.
C                           ELI = 1
C                           DO 2 I = 1, DCB_NDIM( IDCB )
C                              DIMI( I ) = DCB_UBND( I, IDCB ) -
C     :                                    DCB_LBND( I, IDCB ) + 1
C                              ELI = ELI * DIMI( I )
C2                          CONTINUE

*  Map the entire non-imaginary component and fill it with "bad"
*  values.  Then unmap it.
C                           CALL DAT_MAP( DCB_DLOC( IDCB ),
C     :                                   DCB_TYP( IDCB ),
C     :                                   'WRITE', DCB_NDIM( IDCB ),
C     :                                   DIMI, PNTR, STATUS )
C                           CALL ARY1_VBAD( DCB_TYP( IDCB), ELI, PNTR,
C     :                                     STATUS )
C                           CALL ARY1_HUNMP( DCB_DLOC( IDCB ), STATUS )

*  If there is an imaginary component, then perform the same operation
*  on this also.
C                           IF ( DCB_CPX( IDCB ) ) THEN
C                              CALL DAT_MAP( DCB_ILOC( IDCB ),
C     :                                      DCB_TYP( IDCB ),
C     :                                      'WRITE', DCB_NDIM( IDCB ),
C     :                                      DIMI, PNTR, STATUS )
C                              CALL ARY1_VBAD( DCB_TYP( IDCB), ELI, PNTR,
C     :                                        STATUS )
C                              CALL ARY1_HUNMP( DCB_ILOC( IDCB ),
C     :                                         STATUS )
C                           END IF
C                        END IF
C                     END IF
                  END IF

*  Now obtain the required access to the data object; map the
*  non-imaginary array component for writing.
                  CALL ARY1_MPSW( IACB, DCB_DLOC( IDCB ), VTYPE, INOPT,
     :                            MCB_DLOC( IMCB ), MCB_DCOPY( IMCB ),
     :                            MCB_DPNTR( IMCB ), STATUS )

*  If access to the imaginary component is required and such a
*  component exists, then map it for writing.
                  IF ( STATUS .EQ. SAI__OK ) THEN
                     IF ( CMPLX ) THEN
                        IF ( DCB_CPX( IDCB ) ) THEN
                           CALL ARY1_MPSW( IACB, DCB_ILOC( IDCB ),
     :                                     VTYPE, INOPT,
     :                                     MCB_ILOC( IMCB ),
     :                                     MCB_ICOPY( IMCB ),
     :                                     MCB_IPNTR( IMCB ), STATUS )

*  If access to an imaginary component is required, but such a
*  component does not exist, then a "copy" must be used. Create and map
*  a temporary object and initialise its values as required by the
*  initialisation option.
                        ELSE
                           MCB_ICOPY( IMCB ) = .TRUE.
                           CALL ARY1_CMTMP( VTYPE, ACB_NDIM( IACB ),
     :                                      DIM, MCB_ILOC( IMCB ),
     :                                      MCB_IPNTR( IMCB ), STATUS )
                           CALL ARY1_IOBW( VTYPE, INOPT, EL,
     :                                     MCB_IPNTR( IMCB ), STATUS )
                        END IF
                     END IF

*  Note whether the mapped data may contain "bad" values in (a) the
*  mapping transfer region and (b) the padding region which surrounds
*  the actual mapped data (if this exists).
                     MCB_BAD( IMCB ) = INOPT .NE. 'ZERO'
                     MCB_PBAD( IMCB ) = MCB_BAD( IMCB )
                  END IF

*  Note if the data object's values have been initialised.
                  DCB_INIT( IDCB ) = DCB_INIT( IDCB ) .OR.
     :                               ( STATUS .EQ. SAI__OK )

*  Case 3.
*  =======
*  If the access mode is read and an initialisation option was supplied
*  (and the array's values are undefined), then a temporary copy must be
*  made to hold the initialised values. These will not be written back
*  when the array is unmapped.
               ELSE IF ( ( MODE .EQ. 'READ' ) .AND.
     :                   ( INOPT .NE. ' ' ) ) THEN

*  Create and map a temporary object and initialise its values as
*  required by the initialisation option.
                  MCB_DCOPY( IMCB ) = .TRUE.
                  CALL ARY1_CMTMP( VTYPE, ACB_NDIM( IACB ), DIM,
     :                             MCB_DLOC( IMCB ), MCB_DPNTR( IMCB ),
     :                             STATUS )
                  CALL ARY1_IOBW( VTYPE, INOPT, EL, MCB_DPNTR( IMCB ),
     :                            STATUS )

*  If access to an imaginary component is also required, then repeat
*  this process.
                  IF ( CMPLX ) THEN
                     MCB_ICOPY( IMCB ) = .TRUE.
                     CALL ARY1_CMTMP( VTYPE, ACB_NDIM( IACB ), DIM,
     :                                MCB_ILOC( IMCB ),
     :                                MCB_IPNTR( IMCB ), STATUS )
                     CALL ARY1_IOBW( VTYPE, INOPT, EL,
     :                               MCB_IPNTR( IMCB ), STATUS )
                  END IF

*  Note whether the mapped data may contain "bad" values in (a) the
*  mapping transfer region and (b) the padding region which surrounds
*  the actual mapped data (if this exists).
                  MCB_BAD( IMCB ) = INOPT .NE. 'ZERO'
                  MCB_PBAD( IMCB ) = MCB_BAD( IMCB )

*  Case 4.
*  =======
*  It is necessary to read values from the array, but these values are
*  undefined and an initialisation option has not been supplied. Report
*  an error.
               ELSE
                  STATUS = ARY__UNDEF
                  CALL DAT_MSG( 'ARRAY', DCB_LOC( IDCB ) )
                  CALL MSG_SETC( 'BADMODE', MODE )
                  CALL ERR_REP( 'ARY1_MAPS_UDEF',
     :            '^BADMODE access to the array ^ARRAY is not ' //
     :            'available; the array''s values are undefined ' //
     :            '(possible programming error).', STATUS )
               END IF
            END IF

*  If data mapping was successful, then return the data pointer(s).
            IF ( STATUS .EQ. SAI__OK ) THEN
               DPNTR = MCB_DPNTR( IMCB )
               IF( CMPLX ) IPNTR = MCB_IPNTR( IMCB )

*  Increment the counts of current READ and WRITE mapped access to the
*  data object.
               IF ( ( MODE .EQ. 'READ' ) .OR.
     :              ( MODE. EQ. 'UPDATE' ) ) THEN
                  DCB_NREAD( IDCB ) = DCB_NREAD( IDCB ) + 1
               END IF
               IF ( ( MODE .EQ. 'WRITE' ) .OR.
     :              ( MODE .EQ. 'UPDATE' ) ) THEN
                  DCB_NWRIT( IDCB ) = DCB_NWRIT( IDCB ) + 1
               END IF

*  Store the mapping access type and mode information in the MCB.
               MCB_TYP( IMCB ) = VTYPE
               MCB_CPX( IMCB ) = CMPLX
               MCB_AMM( IMCB ) = MODE

*  If mapping was not successful, then clear the MCB index from the ACB
*  (indicating that the array is not mapped) and release the MCB slot.
            ELSE
               ACB_IMCB( IACB ) = 0
               CALL ARY1_RLS( ARY__MCB, IMCB, STATUS )
            END IF
         END IF
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL ARY1_TRACE( 'ARY1_MAPS', STATUS )

      END
