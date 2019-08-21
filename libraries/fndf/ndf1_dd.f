      SUBROUTINE NDF1_DD( IDCB, STATUS )
*+
*  Name:
*     NDF1_DD

*  Purpose:
*     Ensure that DCB information is available for an NDF data array
*     component.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_DD( IDCB, STATUS )

*  Description:
*     The routine ensures that information is available in the DCB for
*     the data array component of an NDF. It does nothing if this
*     information is already available. Otherwise, it obtains the
*     necessary information by inspecting the actual data object,
*     performing necessary validation checks in the process.

*  Arguments:
*     IDCB = INTEGER (Given)
*        Index to the DCB entry for which information is required.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     -  See if the required information is already available. There is
*     nothing to do if it is.
*     -  See if the DATA_ARRAY component is present in the data object.
*     Report an error if it is missing.
*     -  If there, then import it into the ARY_ system, storing the
*     resulting array identifier in the DCB.
*     -  Obtain the data array attributes needed as default values for
*     other NDF components.
*     -  See if WRITE access to the data array is available and set the
*     data object access mode accordingly.
*     -  If an error occurred, then annul the data array identifier.
*     -  Note whether data array information is now available in the
*     DCB.

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
*     21-SEP-1989 (RFWS):
*        Original version.
*     26-SEP-1989 (RFWS):
*        Added status check after calling ARY_ISACC.
*     20-OCT-1989 (RFWS):
*        Removed reference to unnecessary DCB data array components.
*     7-DEC-1989 (RFWS):
*        Added acquisition of the data array attributes which are
*        needed as default values for other NDF components.
*     12-DEC-1989 (RFWS):
*        Added initialisation of the data component mapping count.
*     15-NOV-1990 (RFWS):
*        Removed unnecessary DCB initialisations, which are now
*        performed by NDF1_FFS.
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
      INCLUDE 'NDF_CONST'        ! NDF_ private constants
      INCLUDE 'NDF_ERR'          ! NDF_ error codes

*  Global Variables:
      INCLUDE 'NDF_DCB'          ! NDF_ Data Control Block
*        DCB_DECPX( NDF__MXDCB ) = LOGICAL (Write)
*           Default complex value flag for other NDF components.
*        DCB_DEFRM( NDF__MXDCB ) = CHARACTER * ( NDF__SZFRM ) (Write)
*           Default storage form for other NDF components.
*        DCB_DFRM( NDF__MXDCB ) = CHARACTER * ( NDF__SZFRM ) (Write)
*           Storage form for DATA component.
*        DCB_DETYP( NDF__MXDCB ) = CHARACTER * ( NDF__SZTYP ) (Write)
*           Default numeric data type for other NDF components.
*        DCB_DID( NDF__MXDCB ) = INTEGER (Write)
*           ARY_ system identifier for the NDF's data array.
*        DCB_KD( NDF__MXDCB ) = LOGICAL (Read and Write)
*           Whether information about the NDF's data array component is
*           available in the DCB.
*        DCB_LOC( NDF__MXDCB ) = CHARACTER * ( DAT__SZLOC ) (Read)
*           Data object locator.
*        DCB_MOD( NDF__MXDCB ) = CHARACTER * ( NDF__SZMOD ) (Write)
*           The NDF's access mode.

*  Arguments Given:
      INTEGER IDCB

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      LOGICAL ISACC              ! Whether WRITE access is available
      LOGICAL THERE              ! Whether the data component is present

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  See if the required information is already available. There is
*  nothing to do if it is.
      IF ( .NOT. DCB_KD( IDCB ) ) THEN

*  See if the DATA_ARRAY component is present in the data object. Report
*  an error if it is not.
         CALL DAT_THERE( DCB_LOC( IDCB ), 'DATA_ARRAY', THERE, STATUS )
         IF ( STATUS .EQ. SAI__OK ) THEN
            IF ( .NOT. THERE ) THEN
               STATUS = NDF__NODAT
               CALL NDF1_DMSG( 'NDF', IDCB )
               CALL ERR_REP( 'NDF1_DD_NODAT',
     :         'The DATA_ARRAY component is missing from the NDF ' //
     :         'structure ^NDF', STATUS )

*  If there, then import it into the ARY_ system, storing the resulting
*  array identifier in the DCB.
            ELSE
               CALL ARY_FIND( DCB_LOC( IDCB ), 'DATA_ARRAY',
     :                        DCB_DID( IDCB ), STATUS )


*  Get the storage form of the DATA array.
               CALL ARY_FORM( DCB_DID( IDCB ), DCB_DFRM( IDCB ),
     :                        STATUS )

*  Obtain the data array attributes needed as default values for other
*  NDF components and store them in the DCB.
               CALL ARY_TYPE( DCB_DID( IDCB ), DCB_DETYP( IDCB ),
     :                        STATUS )
               CALL ARY_CMPLX( DCB_DID( IDCB ), DCB_DECPX( IDCB ),
     :                         STATUS )

               IF( DCB_DFRM( IDCB ) .EQ. 'DELTA' .OR.
     :             DCB_DFRM( IDCB ) .EQ. 'SCALED' ) THEN
                  DCB_DEFRM( IDCB ) = 'SIMPLE'
               ELSE
                  DCB_DEFRM( IDCB ) = DCB_DFRM( IDCB )
               END IF

*  See if WRITE access to the data array is available and set the data
*  object access mode accordingly.
               CALL ARY_ISACC( DCB_DID( IDCB ), 'WRITE', ISACC, STATUS )
               IF ( STATUS .EQ. SAI__OK ) THEN
                  IF ( ISACC ) THEN
                     DCB_MOD( IDCB ) = 'UPDATE'
                  END IF
               END IF

*  If an error occurred, then annul the data array identifier.
               IF ( STATUS .NE. SAI__OK ) THEN
                  CALL ARY_ANNUL( DCB_DID( IDCB ), STATUS )
               END IF
            END IF
         END IF

*  Note whether DCB information about the data component is now
*  available.
         DCB_KD( IDCB ) = STATUS .EQ. SAI__OK
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_DD', STATUS )

      END
