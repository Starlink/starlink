      SUBROUTINE NDF1_DANL( DISPOS, IDCB, STATUS )
*+
*  Name:
*     NDF1_DANL

*  Purpose:
*     Perform an "annul" operation on a data object.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_DANL( DISPOS, IDCB, STATUS )

*  Description:
*     The routine performs an "annul" operation on a DCB entry and
*     optionally disposes of the associated data object. This operation
*     is normally required when an ACB entry is annulled.  The
*     reference count for the data object is decremented and if this is
*     still non-zero, then no further action is taken. However, if the
*     reference count reaches zero, then all locators and identifiers
*     contained in the DCB entry are disposed of (thereby removing any
*     reference to the data object) and the DCB entry is released. If
*     the DISPOS argument is set to .TRUE., the data object will also
*     be disposed of according to the disposal mode specified in the
*     DCB (it is either kept or deleted). If the reference count
*     reaches zero and the DISPOS argument is .FALSE., then the DCB
*     entry is released, but the data object is not disposed of.

*  Arguments:
*     DISPOS = LOGICAL (Given)
*        Whether to dispose of the data object. A value of .FALSE.
*        indicates that the data object will remain in use by the NDF_
*        system; the intention being simply to release the specified
*        DCB entry.
*     IDCB = INTEGER (Given and Returned)
*        Index to the DCB entry to be anulled. If the data object
*        reference count falls to zero, then the DCB entry will be
*        released and a value of zero will be returned for this
*        argument (if the DISPOS argument is set to .TRUE., the data
*        object will also be disposed of). Otherwise this argument will
*        be unchanged on exit.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     This routine attempts to execute even if STATUS is set on entry,
*     although no further error report will be made if it subsequently
*     fails under these circumstances.

*  Copyright:
*     Copyright (C) 1997 Rutherford Appleton Laboratory

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
*     26-SEP-1989 (RFWS):
*        Original, derived from the equivalent ARY_ system routine.
*     3-OCT-1989 (RFWS):
*        Re-structured the axis release part of the routine to handle
*        each axis in turn.
*     20-OCT-1989 (RFWS):
*        Removed reference to unnecessary DCB data array components.
*     24-NOV-1989 (RFWS):
*        Installed NDF1_DELOB in place of NDF1_ANTMP to allow the
*        routine to cope with the deletion of top level HDS objects.
*     12-DEC-1989 (RFWS):
*        Installed the NDF1_DVANL routine to annul the variance
*        component.
*     29-JAN-1990 (RFWS):
*        Added code to trap an ARY_ system error status caused by
*        releasing an NDF with its data component undefined. This
*        routine now issues its own message instead and sets the
*        NDF__DUDEF status value.
*     30-JAN-1990 (RFWS):
*        Installed proper support for the quality component.
*     1-AUG-1990 (RFWS):
*        Changed subscript order for DCB axis character arrays.
*     16-OCT-1990 (RFWS):
*        Checked out the axis component handling - seems to be working
*        OK.
*     28-NOV-1990 (RFWS):
*        Reset flags to indicate that DCB information is no longer
*        available.
*     10-MAY-1993 (RFWS):
*        Added writing of default history record if necessary before
*        annulling the history component.
*     12-MAY-1993 (RFWS):
*        Added calls to ERR_BEGIN and ERR_END to ensure that default
*        history gets written under error conditions.
*     3-JUN-1993 (RFWS):
*        Added error message logging to NDF history records.
*     28-SEP-1993 (RFWS):
*        Add extra argument to NDF1_HDERR call.
*     19-OCT-1993 (RFWS):
*        Installed call to NDF1_CLFOR to perform the final closing of
*        the NDF data object and its associated foreign file (if
*        present).
*     1-JUL-1997 (RFWS):
*        Added support for the WCS component.
*     21-AUG-2000 (DSB):
*       Release the NDF data object as a whole, and release the DCB slot
*       associated with the data object, even if it has no DATA_ARRAY.
*     31-OCT-2007 (DSB):
*        Add call to NDF1_EVENT.
*     24-JAN-2009 (DSB):
*        Sort history records if required.
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
      INCLUDE 'ARY_PAR'          ! ARY_ public constants
      INCLUDE 'ARY_ERR'          ! ARY_ error codes
      INCLUDE 'AST_PAR'          ! AST_ public constants
      INCLUDE 'CNF_PAR'          ! CNF functions and constants

*  Global Variables:
      INCLUDE 'NDF_DCB'          ! NDF_ Data Control Block
*        DCB_ACLOC( NDF__MXDIM, NDF__MXACN, NDF__MXDCB ) = CHARACTER *
*        ( DAT__SZLOC ) (Read and Write)
*           Locators to character axis components.
*        DCB_ADID( NDF__MXDIM, NDF__MXDCB ) = INTEGER (Read and Write)
*           ARY_ system identifiers for axis data arrays.
*        DCB_ALOC( NDF__MXDIM, NDF__MXDCB ) = CHARACTER * ( DAT__SZLOC
*        ) (Read and Write)
*           Locators to axis components.
*        DCB_AVID( NDF__MXDIM, NDF__MXDCB ) = INTEGER (Read and Write)
*           ARY_ system identifiers for axis variance arrays.
*        DCB_AWID( NDF__MXDIM, NDF__MXDCB ) = INTEGER (Read and Write)
*           ARY_ system identifier for axis width arrays.
*        DCB_AXLOC( NDF__MXDIM, NDF__MXDCB ) = CHARACTER * ( DAT__SZLOC
*        ) (Read and Write)
*           Locators to axis extension components.
*        DCB_CLOC( NDF__MXCCN, NDF__MXDCB ) = CHARACTER * ( DAT__SZLOC
*        ) (Read and Write)
*           Locators to NDF character components.
*        DCB_DID( NDF__MXDCB ) = INTEGER (Read and Write)
*           ARY_ system identifier for the NDF's data array.
*        DCB_DSP( NDF__MXDCB ) = CHARACTER * ( NDF__SZDSP ) (Read)
*           Data object disposal mode.
*        DCB_HLOC( NDF__MXDCB ) = CHARACTER * ( DAT__SZLOC ) (Read and
*        Write)
*           Locator for NDF history component.
*        DCB_HRLOC( NDF__MXDCB ) = CHARACTER * ( DAT__SZLOC ) (Read and
*        Write)
*           Locator for array of history records.
*        DCB_IWCS( NDF__MXDCB ) = INTEGER (Read and Write)
*           Pointer to AST_ WCS information.
*        DCB_KA( NDF__MXDCB ) = LOGICAL (Read and Write)
*           Whether axis component information is available.
*        DCB_KAC( NDF__MXDIM, NDF__MXACN, NDF__MXDCB ) = LOGICAL (Read
*        and Write)
*           Whether character axis component information is available.
*        DCB_KAD( NDF__MXDIM, NDF__MXDCB ) = LOGICAL (Read and Write)
*           Whether axis data array information is available.
*        DCB_KAV( NDF__MXDIM, NDF__MXDCB ) = LOGICAL (Read and Write)
*           Whether axis variance array information is available.
*        DCB_KAW( NDF__MXDIM, NDF__MXDCB ) = LOGICAL (Read and Write)
*           Whether axis width array information is available.
*        DCB_KAX( NDF__MXDIM, NDF__MXDCB ) = LOGICAL (Read and Write)
*           Whether axis extension information is available.
*        DCB_KC( NDF__MXDCB ) = LOGICAL (Read and Write)
*           Whether character component information is available.
*        DCB_KD( NDF__MXDCB ) = LOGICAL (Write)
*           Whether information about the NDF's data array component is
*           available in the DCB.
*        DCB_KH( NDF__MXDCB ) = LOGICAL (Read and Write)
*           Whether DCB information is available for the NDF's history component.
*        DCB_KW( NDF__MXDCB ) = LOGICAL (Read and Write)
*           Whether information about the NDF's WCS component is
*           available in the DCB.
*        DCB_KX( NDF__MXDCB ) = LOGICAL (Read and Write)
*           Whether extension (MORE) structure information is available.
*        DCB_LOC( NDF__MXDCB ) = CHARACTER * ( DAT__SZLOC ) (Write)
*           Data object locator.
*        DCB_REFCT( NDF__MXDCB ) = INTEGER (Read and Write)
*           Number of ACB entries which refer to each DCB entry.
*        DCB_XLOC( NDF__MXDCB ) = CHARACTER * ( DAT__SZLOC ) (Read and
*        Write)
*           Locator to extension (MORE) structure.

*  Arguments Given:
      LOGICAL DISPOS

*  Arguments Given and Returned:
      INTEGER IDCB

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER IAX                ! Loop counter for axes
      INTEGER ICCOMP             ! Loop counter for character components
      INTEGER IPW1               ! Work space
      INTEGER IPW2               ! Work space
      INTEGER LBND( NDF__MXDIM ) ! Lower bounds of data object
      INTEGER NDIM               ! Number of data object dimensions
      INTEGER UBND( NDF__MXDIM ) ! Upper bounds of data object
      LOGICAL DEL                ! Whether data object is to be deleted

*.

*  Begin a new error reporting environment.
      CALL ERR_BEGIN( STATUS )

*  Decrement the data object reference count.
      STATUS = SAI__OK
      DCB_REFCT( IDCB ) = DCB_REFCT( IDCB ) - 1

*  If the reference count falls to zero, then the DCB entry must be
*  released.
      IF ( DCB_REFCT( IDCB ) .LE. 0 ) THEN

*  Assign the name of the data file to the MSG token "NDF_EVENT"
         CALL NDF1_EVMSG( 'NDF_EVENT', IDCB )

*  Raise an NDF event, describing the closing of a new NDF.
         CALL NDF1_EVENT( 'CLOSE_NDF', STATUS )

*  Ensure that data array information is available in the DCB and derive
*  the data object bounds and number of dimensions from the ARY_ system
*  identifier for the data array.
         CALL NDF1_DD( IDCB, STATUS )
         CALL ARY_BOUND( DCB_DID( IDCB ), NDF__MXDIM, LBND, UBND, NDIM,
     :                   STATUS )
         IF ( STATUS .EQ. SAI__OK ) THEN

*  See if the data object is to be deleted, i.e. if it is being
*  disposed of with a disposal mode other than 'KEEP'.
            DEL = DISPOS .AND. ( DCB_DSP( IDCB ) .NE. 'KEEP' )

*  EXTENSION component.
*  ===================
*  If an extension (MORE) component locator has been acquired, then
*  annul it.
            IF ( DCB_KX( IDCB ) ) THEN
               IF ( DCB_XLOC( IDCB ) .NE. DAT__NOLOC ) THEN
                  CALL DAT_ANNUL( DCB_XLOC( IDCB ), STATUS )
               END IF
               DCB_KX( IDCB ) = .FALSE.
            END IF

*  Character components.
*  ====================
*  If any character component locators have been acquired, then annul
*  them (check each in turn).
            DO 1 ICCOMP = 1, NDF__MXCCN
               IF ( DCB_KC( ICCOMP, IDCB ) ) THEN
                  IF ( DCB_CLOC( ICCOMP, IDCB ) .NE. DAT__NOLOC ) THEN
                     CALL DAT_ANNUL( DCB_CLOC( ICCOMP, IDCB ), STATUS )
                  END IF
                  DCB_KC( ICCOMP, IDCB ) = .FALSE.
               END IF
1           CONTINUE

*  DATA component.
*  ==============
*  Dispose of the data array identifier by deletion if required.
            IF ( DEL ) THEN
               CALL ARY_DELET( DCB_DID( IDCB ), STATUS )

*  If the array identifier is being annulled, then open a new error
*  context to hold any errors which may result.
            ELSE
               CALL ERR_MARK
               CALL ARY_ANNUL( DCB_DID( IDCB ), STATUS )

*  If the ARY_ system complains that the array is being released in an
*  undefined state, then annul the error and make a new error report
*  appropriate to the NDF_ system.
               IF ( STATUS .EQ. ARY__UNDEF ) THEN
                  CALL ERR_ANNUL( STATUS )
                  STATUS = NDF__DUDEF
                  CALL NDF1_DMSG( 'NDF', IDCB )
                  CALL ERR_REP( 'NDF1_DANL_UDEF',
     :            'The NDF structure ^NDF has been released from ' //
     :            'the NDF_ system with its data component in an ' //
     :            'undefined state (possible programming error).',
     :            STATUS )
               END IF

*  End the error context.
               CALL ERR_RLSE
            END IF

*  Note that DCB data array information is no longer available.
            DCB_KD( IDCB ) = .FALSE.

*  QUALITY component.
*  ==================
*  Annul the quality component of the data object.
            CALL NDF1_DQANL( IDCB, DEL, STATUS )

*  VARIANCE component.
*  ==================
*  Annul the variance component of the data object.
            CALL NDF1_DVANL( IDCB, DEL, STATUS )

*  AXIS component.
*  ==============
*  If axis structure locators have been acquired, then loop to dispose
*  of locators and identifiers for each axis in turn. Obtain the number
*  of axes from the number of NDF data array dimensions.
            IF ( DCB_KA( IDCB ) ) THEN
               DO 3 IAX = 1, NDIM

*  Annul the individual axis structure locators.
                  IF ( DCB_ALOC( IAX, IDCB ) .NE. DAT__NOLOC ) THEN
                     CALL DAT_ANNUL( DCB_ALOC( IAX, IDCB ), STATUS )
                  END IF

*  Axis extensions.
*  ===============
*  If axis extension locators have been acquired, then annul them.
                  IF ( DCB_KAX( IAX, IDCB ) ) THEN
                     IF ( DCB_AXLOC( IAX, IDCB ) .NE. DAT__NOLOC ) THEN
                        CALL DAT_ANNUL( DCB_AXLOC( IAX, IDCB ), STATUS )
                     END IF
                     DCB_KAX( IAX, IDCB ) = .FALSE.
                  END IF

*  Axis character components.
*  =========================
*  If axis character component locators have been acquired, then annul
*  them.
                  DO 2 ICCOMP = 1, NDF__MXACN
                     IF ( DCB_KAC( IAX, ICCOMP, IDCB ) ) THEN
                        IF ( DCB_ACLOC( IAX, ICCOMP, IDCB ) .NE.
     :                                  DAT__NOLOC ) THEN
                           CALL DAT_ANNUL( DCB_ACLOC( IAX, ICCOMP,
     :                                     IDCB ), STATUS )
                        END IF
                        DCB_KAC( IAX, ICCOMP, IDCB ) = .FALSE.
                     END IF
2                 CONTINUE

*  Axis data arrays.
*  ================
*  If ARY_ system identifiers for the axis data arrays have been
*  acquired, then dispose of them.
                  IF ( DCB_KAD( IAX, IDCB ) ) THEN
                     IF ( DCB_ADID( IAX, IDCB ) .NE. ARY__NOID ) THEN
                        IF ( DEL ) THEN
                           CALL ARY_DELET( DCB_ADID( IAX, IDCB ),
     :                                     STATUS )
                        ELSE
                           CALL ARY_ANNUL( DCB_ADID( IAX, IDCB ),
     :                                     STATUS )
                        END IF
                     END IF
                     DCB_KAD( IAX, IDCB ) = .FALSE.
                  END IF

*  Axis variance arrays.
*  ====================
*  If ARY_ system identifiers for axis variance arrays have been
*  acquired, then dispose of them.
                  IF ( DCB_KAV( IAX, IDCB ) ) THEN
                     IF ( DCB_AVID( IAX, IDCB ) .NE. ARY__NOID ) THEN
                        IF ( DEL ) THEN
                           CALL ARY_DELET( DCB_AVID( IAX, IDCB ),
     :                                     STATUS )
                        ELSE
                           CALL ARY_ANNUL( DCB_AVID( IAX, IDCB ),
     :                                     STATUS )
                        END IF
                     END IF
                     DCB_KAV( IAX, IDCB ) = .FALSE.
                  END IF

*  Axis width arrays.
*  =================
*  If ARY_ system identifiers for axis width arrays have been acquired,
*  then dispose of them.
                  IF ( DCB_KAW( IAX, IDCB ) ) THEN
                      IF ( DCB_AWID( IAX, IDCB ) .NE. ARY__NOID ) THEN
                        IF ( DEL ) THEN
                           CALL ARY_DELET( DCB_AWID( IAX, IDCB ),
     :                                     STATUS )
                        ELSE
                           CALL ARY_ANNUL( DCB_AWID( IAX, IDCB ),
     :                                     STATUS )
                        END IF
                     END IF
                     DCB_KAW( IAX, IDCB ) = .FALSE.
                  END IF
3              CONTINUE

*  Note that DCB axis structure information is no longer available.
               DCB_KA( IDCB ) = .FALSE.
            END IF

*  HISTORY component.
*  =================
*  If the data object is being disposed of without deletion, then
*  ensure that a default history record is written to it, if required.
            IF ( DISPOS .AND. ( .NOT. DEL ) ) THEN
               CALL ERR_BEGIN( STATUS )
               CALL NDF1_HWDEF( IDCB, ' ', STATUS )
               CALL ERR_END( STATUS )

*  Dump any logged error message information to the history record.
               CALL NDF1_HDERR( IDCB, .TRUE., STATUS )
            END IF

*  If there is any chance that the history records are not in chronological
*  order, sort them.
            CALL ERR_BEGIN( STATUS )
            IF( DCB_HSORT( IDCB ) ) THEN
               CALL PSX_CALLOC( DCB_HNREC( IDCB ), '_DOUBLE', IPW1,
     :                          STATUS )
               CALL PSX_CALLOC( DCB_HNREC( IDCB ), '_INTEGER', IPW2,
     :                          STATUS )

               CALL NDF1_HSRT( IDCB, DCB_HNREC( IDCB ),
     :                         %VAL( CNF_PVAL( IPW1 ) ),
     :                         %VAL( CNF_PVAL( IPW2 ) ), STATUS )

               CALL PSX_FREE( IPW1, STATUS )
               CALL PSX_FREE( IPW2, STATUS )
               DCB_HSORT( IDCB ) = .FALSE.
            END IF
            CALL ERR_END( STATUS )

*  If history component locators have been acquired, then annul them.
            IF ( DCB_KH( IDCB ) ) THEN
               IF ( DCB_HLOC( IDCB ) .NE. DAT__NOLOC ) THEN
                  CALL DAT_ANNUL( DCB_HRLOC( IDCB ), STATUS )
                  CALL DAT_ANNUL( DCB_HLOC( IDCB ), STATUS )
               END IF
               DCB_KH( IDCB ) = .FALSE.
            END IF

*  WCS component.
*  ==============
*  If an AST_ pointer for WCS information has been acquired, then annul
*  it.
            IF ( DCB_KW( IDCB ) ) THEN
               IF ( DCB_IWCS( IDCB ) .NE. AST__NULL ) THEN
                  CALL AST_ANNUL( DCB_IWCS( IDCB ), STATUS )
               END IF
               DCB_KW( IDCB ) = .FALSE.
            END IF
         END IF

*  Whole data object.
*  =================
*  Release the NDF data object as a whole, taking account of any
*  associated foreign format file.
         CALL NDF1_CLFOR( DISPOS, IDCB, STATUS )

*  Release the DCB slot associated with the data object.
         CALL NDF1_RLS( NDF__DCB, IDCB, STATUS )
      END IF

*  Call the error tracing routine if appropriate.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_DANL', STATUS )

*  End the error reporting environment.
      CALL ERR_END( STATUS )

      END
