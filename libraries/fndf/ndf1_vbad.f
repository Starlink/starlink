      SUBROUTINE NDF1_VBAD( IACB, CHECK, BAD, STATUS )
*+
*  Name:
*     NDF1_VBAD

*  Purpose:
*     Determine the bad pixel flag for the variance component of an NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_VBAD( IACB, CHECK, BAD, STATUS )

*  Description:
*     The routine obtains the logical value of the bad pixel flag for
*     the variance component of an NDF, taking account of the
*     possibility that the array may be mapped for access. An explicit
*     check of the array for bad pixels may be specified if required.
*     The NDF is identified by its ACB entry.

*  Arguments:
*     IACB = INTEGER (Given)
*        Index to the NDF's ACB entry.
*     CHECK = LOGICAL (Given)
*        Whether an explicit check for the presence of bad pixels should
*        be performed if it appears that they may be present.
*     BAD = LOGICAL (Returned)
*        The bad pixel flag.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     -  See if the variance component is currently mapped for access.
*     -  If so, then obtain the bad pixel flag for the mapped values.
*     -  If bad pixels may be present and an explicit check is
*     required, then determine the number of mapped elements from the
*     size of the ACB's data array component and check the mapped
*     non-imaginary variance values for bad pixels.
*     -  If no bad pixels were found and complex values are mapped,
*     then check the imaginary component similarly.
*     -  If the variance component is not mapped for access, then
*     ensure that variance information is available in the DCB and ACB.
*     -  See if the ARY_ system identifier for the variance array is
*     valid.  If not, then the variance component is undefined, so BAD
*     is .TRUE..
*     -  If variance is defined, then obtain the bad pixel flag via the
*     ARY_ system.
*     -  If no bad pixels were found but the quality masking flag is
*     set, then the quality component must also be checked.
*     -  Obtain the quality bad-bits mask value and check it is not
*     zero. No bad pixels can be introduced by the quality component if
*     it is.
*     -  Obtain the state of the quality component.
*     -  If quality is present it may introduce bad pixels, but if an
*     explicit check is required, then clone the ARY_ system quality
*     array identifier and map it to obtain the quality values (it must
*     be cloned in case quality is already mapped through the current
*     ACB entry).
*     -  If access to the quality values could not be obtained, then
*     report context information.
*     -  Inspect the quality array to see if bad pixels are actually
*     present.
*     -  Annul the cloned ARY_ system quality identifier, thereby
*     unmapping it.

*  Copyright:
*     Copyright (C) 1989, 1990 Science & Engineering Research Council.
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
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     18-DEC-1989 (RFWS):
*        Original version.
*     19-DEC-1989 (RFWS):
*        Fixed incorrect call to NDF1_BPP.
*     23-MAR-1990 (RFWS):
*        Revised to take account of the possible presence of quality
*        information.
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

*  Global Variables:
      INCLUDE 'NDF_ACB'          ! NDF_ Access Control Block
*        ACB_DID( NDF__MXACB ) = INTEGER (Read)
*           ARY_ system identifier for the NDF's data array.
*        ACB_QID( NDF__MXACB ) = INTEGER (Read)
*           ARY_ system identifier for the NDF's quality array.
*        ACB_QMF( NDF__MXACB ) = LOGICAL (Read)
*           NDF's quality masking flag.
*        ACB_VID( NDF__MXACB ) = INTEGER (Read)
*           ARY_ system identifier for the NDF's variance array.
*        ACB_VMAP( NDF__MXACB ) = LOGICAL (Read)
*           Whether the NDF's variance component is mapped for access.
*        ACB_VMBAD( NDF__MXACB ) = LOGICAL (Read)
*           Bad pixel flag for mapped variance values.
*        ACB_VMCPX( NDF__MXACB ) = LOGICAL (Read)
*           Whether mapped variance values are complex.
*        ACB_VMDPT( NDF__MXACB ) = INTEGER (Read)
*           Pointer to mapped non-imaginary variance values.
*        ACB_VMIPT( NDF__MXACB ) = INTEGER (Read)
*           Pointer to mapped imaginary variance values.
*        ACB_VMTYP( NDF__MXACB ) = CHARACTER * ( NDF_SZTYP ) (Read)
*           Numeric data type of mapped variance values.

*  Arguments Given:
      INTEGER IACB
      LOGICAL CHECK

*  Arguments Returned:
      LOGICAL BAD

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      BYTE ZEROUB                ! Zero as an unsigned byte value
      PARAMETER ( ZEROUB = 0 )

*  Local Variables:
      BYTE BADBIT                ! Quality bad-bits mask
      INTEGER EL                 ! Number of mapped values
      INTEGER PNTR               ! Pointer to mapped values
      INTEGER QID                ! ARY_ system quality identifier
      LOGICAL THERE              ! Whether the variance array exists

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  See if the variance component is currently mapped for access.
      IF ( ACB_VMAP( IACB ) ) THEN

*  If so, then obtain the bad pixel flag for the mapped values.
         BAD = ACB_VMBAD( IACB )

*  If bad pixels may be present and an explicit check is required, then
*  determine the number of mapped elements from the size of the ACB's
*  data array component and check the mapped non-imaginary variance
*  values for bad pixels.
         IF ( BAD .AND. CHECK ) THEN
            CALL ARY_SIZE( ACB_DID( IACB ), EL, STATUS )
            CALL NDF1_BPP( ACB_VMTYP( IACB ), EL, ACB_VMDPT( IACB ),
     :                     BAD, STATUS )
            IF ( STATUS .EQ. SAI__OK ) THEN

*  If no bad pixels were found and complex values are mapped, then check
*  the imaginary component similarly.
               IF ( ( .NOT. BAD ) .AND. ACB_VMCPX( IACB ) ) THEN
                  CALL NDF1_BPP( ACB_VMTYP( IACB ), EL,
     :                           ACB_VMIPT( IACB ), BAD, STATUS )
               END IF
            END IF
         END IF

*  If the variance component is not mapped for access, then ensure that
*  variance information is available in the DCB and ACB.
      ELSE
         CALL NDF1_VIMP( IACB, STATUS )

*  See if the ARY_ system identifier for the variance array is valid.
         CALL ARY_VALID( ACB_VID( IACB ), THERE, STATUS )
         IF ( STATUS .EQ. SAI__OK ) THEN

*  If not, then the variance component is undefined, so BAD is .TRUE..
            IF ( .NOT. THERE ) THEN
               BAD = .TRUE.

*  If variance is defined, then obtain the bad pixel flag via the ARY_
*  system.
            ELSE
               CALL ARY_BAD( ACB_VID( IACB ), CHECK, BAD, STATUS )
               IF ( STATUS .EQ. SAI__OK ) THEN

*  If no bad pixels were found but the quality masking flag is set, then
*  the quality component must also be checked.
                  IF ( ( .NOT. BAD ) .AND. ACB_QMF( IACB ) ) THEN

*  Obtain the quality bad-bits mask value and check it is not zero. No
*  bad pixels can be introduced by the quality component if it is.
                     CALL NDF1_GTBB( IACB, BADBIT, STATUS )
                     IF ( STATUS .EQ. SAI__OK ) THEN
                        IF ( BADBIT .NE. ZEROUB ) THEN

*  Obtain the state of the quality component.
                           IF ( STATUS .EQ. SAI__OK ) THEN
                              CALL NDF1_QSTA( IACB, BAD, STATUS )

*  If quality is present it may introduce bad pixels, but if an
*  explicit check is required, then clone the ARY_ system quality array
*  identifier and map it to obtain the quality values (it must be
*  cloned in case quality is already mapped through the current ACB
*  entry).
                              IF ( STATUS .EQ. SAI__OK ) THEN
                                 IF ( BAD .AND. CHECK ) THEN
                                    CALL ARY_CLONE( ACB_QID( IACB ),
     :                                              QID, STATUS )
                                    CALL ARY_MAP( QID, '_UBYTE', 'READ',
     :                                            PNTR, EL, STATUS )

*  If access to the quality values could not be obtained, then report
*  context information.
                                    IF ( STATUS .NE. SAI__OK ) THEN
                                       CALL ERR_REP( 'NDF1_VBAD_QUAL',
     :                                 'Unable to access an NDF''s ' //
     :                                 'quality component to check ' //
     :                                 'for bad pixels.', STATUS )

*  Inspect the quality array to see if bad pixels are actually present.
                                    ELSE
                                       CALL NDF1_QBPP( BADBIT, EL,
     :                                         %VAL( CNF_PVAL( PNTR ) ),
     :                                                 BAD, STATUS )
                                    END IF

*  Annul the cloned ARY_ system quality identifier, thereby unmapping
*  it.
                                    CALL ARY_ANNUL( QID, STATUS )
                                 END IF
                              END IF
                           END IF
                        END IF
                     END IF
                  END IF
               END IF
            END IF
         END IF
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_VBAD', STATUS )

      END
