      SUBROUTINE NDF1_HPRP( IDCB1, PROP, IDCB2, STATUS )
*+
*  Name:
*     NDF1_HPRP

*  Purpose:
*     Propagate history information from one NDF to another.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_HPRP( IDCB1, PROP, IDCB2, STATUS )

*  Description:
*     The routine propagates attributes and information from one NDF's
*     history structure to a new one which is being created.

*  Arguments:
*     IDCB1 = INTEGER (Given)
*        DCB index identifying the input NDF.
*     PROP = LOGICAL (Given)
*        Whether history information is to be propagated.
*     IDCB2 = INTEGER (Given)
*        DCB index identifying the output NDF. This must not already
*        contain a history component (this routine does not check for
*        this).
*     STATUS = INTEGER (Given and Returned)
*        The global status.

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
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     DSB: David S Berry (JAC, UCLan)
*     {enter_new_authors_here}

*  History:
*     18-MAY-1993 (RFWS):
*        Original version.
*     23-JAN-2009 (DSB):
*        Added DCB_HTIME and DCB_HSORT.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'NDF_CONST'        ! NDF_ private constants
      INCLUDE 'NDF_PAR'          ! NDF_ public constants

*  Global Variables:
      INCLUDE 'NDF_DCB'          ! NDF_ Data Control Block
*        DCB_HDEF( NDF__MXDCB ) = LOGICAL (Read and Write)
*           Whether default history information is to be written.
*        DCB_HEXT( NDF__MXDCB ) = INTEGER (Read and Write)
*           Extension increment for the history records array.
*        DCB_HLOC( NDF__MXDCB ) = CHARACTER * ( DAT__SZLOC ) (Read and
*        Write)
*           Locator for NDF history component.
*        DCB_HSORT( NDF__MXDCB ) = LOGICAL (Read and Write)
*           Do the history records need sorting?
*        DCB_HNREC( NDF__MXDCB ) = INTEGER (Read and Write)
*           Number of valid history records present.
*        DCB_HRLOC( NDF__MXDCB ) = CHARACTER * ( DAT__SZLOC ) (Read and
*        Write)
*           Locator for array of history records.
*        DCB_HTIME( NDF__MXDCB ) = DOUBLE PRECISION (Read and Write)
*           The date/time to attach to the next history record to be
*           created, as a UTC Modified Julian Date. If negative, then
*           the current time will be used.
*        DCB_HTLEN( NDF__MXDCB ) = INTEGER (Read and Write)
*           Text length of the current history record.
*        DCB_HUMOD( NDF__MXDCB ) = INTEGER (Read and Write)
*           History recording update mode.
*        DCB_KH( NDF__MXDCB ) = LOGICAL (Write)
*           Whether DCB information is available for the NDF's history
*           component.
*        DCB_LOC( NDF__MXDCB ) = CHARACTER * ( DAT__SZLOC ) (Read)
*           Data object locator.

*  Arguments Given:
      INTEGER IDCB1
      LOGICAL PROP
      INTEGER IDCB2

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER DIM( 1 )           ! Dummy dimension size array

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the output DCB entry's history locators to null values.
      DCB_HLOC( IDCB2 ) = DAT__NOLOC
      DCB_HRLOC( IDCB2 ) = DAT__NOLOC

*  If the history component is being propagated, then ensure that
*  history structure information is available for the input DCB entry.
      IF ( PROP ) THEN
         CALL NDF1_DH( IDCB1, STATUS )
         IF ( STATUS .EQ. SAI__OK ) THEN

*  Check that an input history component exists. Otherwise there is
*  nothing more to do.
            IF ( DCB_HLOC( IDCB1 ) .NE. DAT__NOLOC ) THEN

*  Create a new output history structure and obtain a locator for it,
*  storing this in the DCB.
               CALL DAT_NEW( DCB_LOC( IDCB2 ), 'HISTORY', 'HISTORY',
     :                       0, DIM, STATUS )
               CALL DAT_FIND( DCB_LOC( IDCB2 ), 'HISTORY',
     :                        DCB_HLOC( IDCB2 ), STATUS )

*  Propagate the CREATED component from the input history structure.
               CALL NDF1_CPYNC( DCB_HLOC( IDCB1 ), 'CREATED',
     :                          DCB_HLOC( IDCB2 ), STATUS )

*  Propagate the sorted flag.
               DCB_HSORT( IDCB2 ) = DCB_HSORT( IDCB1 )

*  Propagate the CURRENT_RECORD value and component from the input
*  structure.
               DCB_HNREC( IDCB2 ) = DCB_HNREC( IDCB1 )
               CALL NDF1_CPYNC( DCB_HLOC( IDCB1 ), 'CURRENT_RECORD',
     :                          DCB_HLOC( IDCB2 ), STATUS )

*  Propagate the UPDATE_MODE value and component from the input
*  structure.
               DCB_HUMOD( IDCB2 ) = DCB_HUMOD( IDCB1 )
               CALL NDF1_CPYNC( DCB_HLOC( IDCB1 ), 'UPDATE_MODE',
     :                          DCB_HLOC( IDCB2 ), STATUS )

*  Propagate the RECORDS component from the input structure and obtain a
*  locator for the copy, storing this in the DCB.
               CALL NDF1_CPYNC( DCB_HLOC( IDCB1 ), 'RECORDS',
     :                          DCB_HLOC( IDCB2 ), STATUS )
               CALL DAT_FIND( DCB_HLOC( IDCB2 ), 'RECORDS',
     :                        DCB_HRLOC( IDCB2 ), STATUS )

*  Propagate the EXTEND_SIZE value and component from the input
*  sructure.
               DCB_HEXT( IDCB2 ) = DCB_HEXT( IDCB1 )
               CALL NDF1_CPYNC( DCB_HLOC( IDCB1 ), 'EXTEND_SIZE',
     :                          DCB_HLOC( IDCB2 ), STATUS )

*  Propagate remaining history status informaton.
               DCB_HDEF( IDCB2 ) = DCB_HDEF( IDCB1 )
               DCB_HTLEN( IDCB2 ) = DCB_HTLEN( IDCB1 )
               DCB_HTIME( IDCB2 ) = DCB_HTIME( IDCB1 )

*  Note whether the output DCB history information is up to date.
               DCB_KH( IDCB2 ) = ( STATUS .EQ. SAI__OK )
            END IF
         END IF
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_HPRP', STATUS )

      END
