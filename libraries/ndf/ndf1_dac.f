      SUBROUTINE NDF1_DAC( IAX, ICCOMP, IDCB, STATUS )
*+
*  Name:
*     NDF1_DAC

*  Purpose:
*     Ensure that DCB information about an axis character component is
*     available.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_DAC( IAX, ICCOMP, IDCB, STATUS )

*  Description:
*     This routine ensures that information is available in the DCB for
*     a specified axis character component of an NDF. It returns
*     without action if this information is already available.
*     Otherwise, it inspects the actual data object to obtain the
*     information, which it stores in the DCB. Only those checks
*     necessary to obtain and validate this information are performed.

*  Arguments:
*     IAX = INTEGER (Given)
*        Number of the axis for which information is required.
*     ICCOMP = INTEGER (Given)
*        Number of the axis character component whose value is
*        required.  Symbolic constant values for this argument are
*        defined in the include file NDF_CONST.
*     IDCB = INTEGER (Given)
*        DCB index for the NDF.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     -  See if the required axis character component information is
*     already available. There is nothing to do if it is.
*     -  Ensure that axis structure information is available in the
*     DCB.
*     -  Set an initial null value for the axis character component
*     locator.
*     -  See if the axis structure exists. If not, then the character
*     component cannot exist, so its locator remains null.
*     -  See if the required character component exists within the
*     appropriate element of the axis structure.
*     -  If so, then obtain a locator for it and determine its type and
*     shape.
*     -  Check that its type is _CHAR. Report an error if it is not.
*     -  Check that it is a scalar object. Report an error if it is
*     not.
*     -  If an error occurred, then annul the locator which may have
*     been acquired.
*     -  Note whether the required axis character component information
*     is now available.

*  Copyright:
*     Copyright (C) 1990 Science & Engineering Research Council.
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
*     4-JUL-1990 (RFWS):
*        Original version.
*     {enter_changes_here}

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
*        DCB_ACCN( NDF__MXACN ) = CHARACTER * ( DAT__SZNAM ) (Read)
*           Axis character component names.
*        DCB_ACLOC( NDC__MXDIM, NDF__MXACN, NDF__MXDCB ) = CHARACTER * (
*        DAT__SZLOC ) (Write)
*           Locators to axis character components.
*        DCB_ALOC( NDF__MXDIM, NDF__MXDCB ) = CHARACTER * ( DAT__SZLOC )
*        (Read)
*           Locators to axis structure elements.
*        DCB_KAC( NDF__MXDIM, NDF__MXACN, NDF__MXDCB ) = LOGICAL
*        (Read and Write)
*           Whether information about axis character components is
*           available.

*  Arguments Given:
      INTEGER IAX
      INTEGER ICCOMP
      INTEGER IDCB

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( DAT__SZTYP ) TYPE ! Component type
      INTEGER DIM( DAT__MXDIM )  ! Component dimensions
      INTEGER NDIM               ! Number of component dimensions
      LOGICAL THERE              ! Whether component is present

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  See if the required axis character component information is already
*  available. There is nothing to do if it is.
      IF ( .NOT. DCB_KAC( IAX, ICCOMP, IDCB ) ) THEN

*  Ensure that axis structure information is available in the DCB.
         CALL NDF1_DA( IDCB, STATUS )
         IF ( STATUS .EQ. SAI__OK ) THEN

*  Set an initial null value for the axis character component locator.
            DCB_ACLOC( IAX, ICCOMP, IDCB ) = DAT__NOLOC

*  See if the axis structure exists. If not, then the character
*  component cannot exist, so its locator remains null.
            IF ( DCB_ALOC( IAX, IDCB ) .NE. DAT__NOLOC ) THEN

*  See if the required character component exists within the appropriate
*  element of the axis structure.
               CALL DAT_THERE( DCB_ALOC( IAX, IDCB ),
     :                         DCB_ACCN( ICCOMP ), THERE, STATUS )
               IF ( STATUS .EQ. SAI__OK ) THEN
                  IF ( THERE ) THEN

*  If so, then obtain a locator for it and determine its type and shape.
                     CALL DAT_FIND( DCB_ALOC( IAX, IDCB ),
     :                              DCB_ACCN( ICCOMP ),
     :                              DCB_ACLOC( IAX, ICCOMP, IDCB ),
     :                              STATUS )
                     CALL DAT_TYPE( DCB_ACLOC( IAX, ICCOMP, IDCB ),
     :                              TYPE, STATUS )
                     CALL DAT_SHAPE( DCB_ACLOC( IAX, ICCOMP, IDCB ),
     :                               DAT__MXDIM, DIM, NDIM, STATUS )
                     IF ( STATUS .EQ. SAI__OK ) THEN

*  Check that its type is _CHAR. Report an error if it is not.
                        IF ( TYPE( : 6 ) .NE. '_CHAR*' ) THEN
                           STATUS = NDF__TYPIN
                           CALL MSG_SETC( 'CCOMP', DCB_ACCN( ICCOMP ) )
                           CALL DAT_MSG( 'AXIS',
     :                                   DCB_ACLOC( IAX, ICCOMP,
     :                                   IDCB ) )
                           CALL MSG_SETC( 'BADTYPE', TYPE )
                           CALL ERR_REP( 'NDF1_DAC_TYPE',
     :                     'The ^CCOMP component in the NDF axis ' //
     :                     'structure ^AXIS has an invalid type of ' //
     :                     '''^BADTYPE''; it should be of type ' //
     :                     '''_CHAR''.', STATUS )

*  Check that it is a scalar object. Report an error if it is not.
                        ELSE IF ( NDIM .NE. 0 ) THEN
                           STATUS = NDF__NDMIN
                           CALL MSG_SETC( 'CCOMP', DCB_ACCN( ICCOMP ) )
                           CALL DAT_MSG( 'AXIS',
     :                                   DCB_ACLOC( IAX, ICCOMP,
     :                                              IDCB ) )
                           CALL MSG_SETI( 'BADNDIM', NDIM )
                           CALL ERR_REP( 'NDF1_DAC_NDIM',
     :                     'The ^CCOMP component in the NDF axis ' //
     :                     'structure ^AXIS is ' //
     :                     '^BADNDIM-dimensional; it should be scalar.',
     :                     STATUS )
                        END IF
                     END IF

*  If an error occurred, then annul the locator which may have been
*  acquired.
                     IF ( STATUS .NE. SAI__OK ) THEN
                        CALL DAT_ANNUL( DCB_ACLOC( IAX, ICCOMP, IDCB ),
     :                                  STATUS )
                     END IF
                  END IF
               END IF
            END IF
         END IF

*  Note whether the required axis character component information is now
*  available.
         DCB_KAC( IAX, ICCOMP, IDCB ) = STATUS .EQ. SAI__OK
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_DAC', STATUS )

      END
