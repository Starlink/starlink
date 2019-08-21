      SUBROUTINE NDF1_DBAD( IACB, CHECK, BAD, STATUS )
*+
*  Name:
*     NDF1_DBAD

*  Purpose:
*     Determine the bad pixel flag for the data component of an NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_DBAD( IACB, CHECK, BAD, STATUS )

*  Description:
*     The routine obtains the logical value of the bad pixel flag for
*     the data component of an NDF, taking account of the
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
*     -  Check inherited global status.
*     -  See if the data component is currently mapped for access.
*     -  If so, then obtain the bad pixel flag for the mapped values.
*     -  If bad pixels may be present and an explicit check is
*     required, then determine the number of mapped elements and check
*     the mapped non-imaginary variance values for bad pixels.
*     -  If no bad pixels were found and complex values are mapped,
*     then check the imaginary component similarly.
*     -  If the data component is not mapped for access, then see if
*     its values are defined.
*     -  If not, then the data component is undefined, so BAD is
*     .TRUE..
*     -  If data values are defined, then obtain the bad pixel flag via
*     the ARY_ system.
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
*     Copyright (C) 1989 Science & Engineering Research Council.
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
*     23-MAR-1989 (RFWS):
*        Original version, derived from the routine NDF1_VBAD.
*     {enter_changes_here}

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
*        ACB_DMAP( NDF__MXACB ) = LOGICAL (Read)
*           Whether the NDF's data component is mapped for access.
*        ACB_DMBAD( NDF__MXACB ) = LOGICAL (Read)
*           Bad pixel flag for mapped data values.
*        ACB_DMCPX( NDF__MXACB ) = LOGICAL (Read)
*           Whether mapped data values are complex.
*        ACB_DMDPT( NDF__MXACB ) = INTEGER (Read)
*           Pointer to mapped non-imaginary data values.
*        ACB_DMIPT( NDF__MXACB ) = INTEGER (Read)
*           Pointer to mapped imaginary data values.
*        ACB_DMTYP( NDF__MXACB ) = CHARACTER * ( NDF_SZTYP ) (Read)
*           Numeric data type of mapped data values.
*        ACB_QID( NDF__MXACB ) = INTEGER (Read)
*           ARY_ system identifier for the NDF's quality array.
*        ACB_QMF( NDF__MXACB ) = LOGICAL (Read)
*           NDF's quality masking flag.

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
      LOGICAL THERE              ! Whether the data values are defined

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  See if the data component is currently mapped for access.
      IF ( ACB_DMAP( IACB ) ) THEN

*  If so, then obtain the bad pixel flag for the mapped values.
         BAD = ACB_DMBAD( IACB )

*  If bad pixels may be present and an explicit check is required, then
*  determine the number of mapped elements and check the mapped
*  non-imaginary variance values for bad pixels.
         IF ( BAD .AND. CHECK ) THEN
            CALL ARY_SIZE( ACB_DID( IACB ), EL, STATUS )
            CALL NDF1_BPP( ACB_DMTYP( IACB ), EL, ACB_DMDPT( IACB ),
     :                     BAD, STATUS )
            IF ( STATUS .EQ. SAI__OK ) THEN

*  If no bad pixels were found and complex values are mapped, then check
*  the imaginary component similarly.
               IF ( ( .NOT. BAD ) .AND. ACB_DMCPX( IACB ) ) THEN
                  CALL NDF1_BPP( ACB_DMTYP( IACB ), EL,
     :                           ACB_DMIPT( IACB ), BAD, STATUS )
               END IF
            END IF
         END IF

*  If the data component is not mapped for access, then see if its
*  values are defined.
      ELSE
         CALL ARY_STATE( ACB_DID( IACB ), THERE, STATUS )
         IF ( STATUS .EQ. SAI__OK ) THEN

*  If data values are undefined, then BAD is .TRUE..
            IF ( .NOT. THERE ) THEN
               BAD = .TRUE.

*  If data values are defined, then obtain the bad pixel flag via the
*  ARY_ system.
            ELSE
               CALL ARY_BAD( ACB_DID( IACB ), CHECK, BAD, STATUS )
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
                                       CALL ERR_REP( 'NDF1_DBAD_QUAL',
     :                                 'Unable to access the NDF''s ' //
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
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_DBAD', STATUS )

      END
