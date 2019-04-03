      SUBROUTINE NDF1_PRP( IACB1, NEXTN, EXTN, CPF, IPCB, IACB2,
     :                     STATUS )
*+
*  Name:
*     NDF1_PRP

*  Purpose:
*     Selectively propagate an NDF's components to form a new NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_PRP( IACB1, NEXTN, EXTN, CPF, IPCB, IACB2, STATUS )

*  Description:
*     The routine selectively propagates the components of an existing
*     NDF (identified by its ACB entry) to form a new data object and
*     creates an ACB entry to describe the resulting new base NDF. The
*     location of the new object is identified by means of an index to
*     a placeholder entry in the PCB. This placeholder should later be
*     annulled.

*  Arguments:
*     IACB1 = INTEGER (Given)
*        Index to the ACB entry of the input NDF.
*     NEXTN = INTEGER (Given)
*        Number of extension component names supplied in the EXTN array
*        (may be zero).
*     EXTN( NEXTN ) = CHARACTER * ( DAT__SZNAM ) (Given)
*        List of extension component names to be omitted from the
*        propagation operation.
*     CPF( NDF__MXCPF ) = LOGICAL (Given)
*        List of component propagation flags; symbolic constants are
*        defined in the include file NDF_CONST to identify the elements
*        of this array.
*     IPCB = INTEGER (Given)
*        Index to a placeholder entry in the PCB which specifies the
*        location (and certain properties) of the new NDF.
*     IACB2 = INTEGER (Returned)
*        Index to the ACB entry of the new base NDF.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     The order in which the NDF components are processed by this
*     routine is arbitrary, but is intended to produce an easily
*     understood component order in the output structure.

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
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     11-OCT-1989 (RFWS):
*        Original version.
*     20-OCT-1989 (RFWS):
*        Removed reference to unnecessary DCB data array components.
*     14-NOV-1989 (RFWS):
*        Installed ARY_DUPE routine and updated prologue.
*     27-NOV-1989 (RFWS):
*        Fixed bug introduced during previous change which attempted to
*        enter the duplicated array ID into the ACB rather than the
*        DCB.
*     28-NOV-1989 (RFWS):
*        Minor re-structuring of placeholder handling for the DATA
*        component. Also added note to prologue.
*     12-DEC-1989 (RFWS):
*        Installed support for the variance component and added
*        initialisation of DCB mapping counts.
*     20-DEC-1989 (RFWS):
*        Added propagation of default array component attributes.
*     15-JAN-1990 (RFWS):
*        Added propagation of default attributes for the variance
*        component.
*     30-JAN-1990 (RFWS):
*        Added support for the QUALITY component.
*     12-FEB-1990 (RFWS):
*        Installed propagation of the bad-bits override value for the
*        NDF's quality component.
*     26-FEB-1990 (RFWS):
*        Changed the NDF placeholder data type to 'NDF'.
*     1-MAR-1990 (RFWS):
*        Removed un-referenced variable.
*     1-AUG-1990 (RFWS):
*        Changed subscript order in DCB axis character component
*        arrays.
*     22-OCT-1990 (RFWS):
*        Installed proper support for the axis component.
*     9-NOV-1990 (RFWS):
*        Fixed a bug which caused the default attributes of the quality
*        and variance components to be lost if these components were
*        not being propagated. Updated the prologue to reflect all
*        recent changes.
*     12-NOV-1990 (RFWS):
*        Changed to use NDF1_DANL to perform cleaning-up under error
*        conditions.
*     13-NOV-1990 (RFWS):
*        Fixed bug causing the propagated variance and quality array
*        default attributes to be wrong.
*     14-NOV-1990 (RFWS):
*        Added conversion of NDF array component default storage form
*        entries in the DCB to take account of the new NDF's bounds, if
*        necessary.
*     18-MAY-1993 (RFWS):
*        Added proper handling of the history component.
*     23-JUN-1993 (RFWS):
*        Promote the data object locator in the DCB to be a primary
*        locator.
*     4-NOV-1993 (RFWS):
*        Changed to identify the output location by passing a
*        placeholder entry from the PCB instead of a locator.
*     29-APR-1994 (RFWS):
*        Eliminated initialisations now done by NDF1_PLDCB.
*     1-JUL-1997 (RFWS):
*        Added support for the WCS component.
*     14-JAN-1998 (RFWS):
*        Fixed bug: propagated WCS information was not being stripped
*        before writing it to the output data object.
*     31-OCT-2007 (DSB):
*        Add call to NDF1_EVENT to record the output NDF.
*     10-SEP-2008 (RFWS):
*        Raise an NDF event when the input DATA array is copied.
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
      INCLUDE 'NDF_CONST'        ! NDF_ private constant
      INCLUDE 'ARY_PAR'          ! ARY_ public constants
      INCLUDE 'AST_PAR'          ! AST_ public interface

*  Global Variables:
      INCLUDE 'NDF_DCB'          ! NDF_ Data Control Block
*        DCB_QBB( NDF__MXDCB ) = BYTE (Read and Write)
*           Data object quality "badbits" value.
*        DCB_CLOC( NDF__MXCCN, NDF__MXDCB ) = CHARACTER * ( DAT__SZLOC
*        ) (Read)
*           Locators to NDF character components.
*        DCB_DECPX( NDF__MXDCB ) = LOGICAL (Read and Write)
*           Whether NDF array components hold complex values by default.
*        DCB_DEFRM( NDF__MXDCB ) = CHARACTER * ( NDF__SZFRM ) (Read and
*        Write)
*           Default array storage form for the NDF.
*        DCB_DETYP( NDF__MXDCB ) = CHARACTER * ( NDF__SZTYP ) (Read and
*        Write)
*           Default numeric type for NDF array components.
*        DCB_DID( NDF__MXDCB ) = INTEGER (Read and Write)
*           ARY_ system identifier for the NDF's data array.
*        DCB_KD( NDF__MXDCB ) = LOGICAL (Write)
*           Whether s data array information is available in the DCB.
*        DCB_KQ( NDF__MXDCB ) = LOGICAL (Write)
*           Whether quality information is available.
*        DCB_KV( NDF__MXDCB ) = LOGICAL (Write)
*           Whether variance information is available.
*        DCB_KX( NDF__MXDCB ) = LOGICAL (Write)
*           Whether extension (MORE) structure information is available.
*        DCB_LOC( NDF__MXDCB ) = CHARACTER * ( DAT__SZLOC ) (Read)
*           Data object locator.
*        DCB_QFRM( NDF__MXDCB ) = CHARACTER * ( NDF__SZFRM ) (Write)
*           Default storage form for the NDF's quality array.
*        DCB_QID( NDF__MXDCB ) = INTEGER (Write)
*           ARY_ system identifier for the NDF's quality array.
*        DCB_QLOC( NDF__MXDCB ) = CHARACTER * ( DAT__SZLOC ) (Read and
*        Write)
*           Data object quality structure locator.
*        DCB_VCPX( NDF__MXDCB ) = LOGICAL (Write)
*           Whether the NDF's variance array holds complex values by
*           default.
*        DCB_VFRM( NDF__MXDCB ) = CHARACTER * ( NDF__SZFRM ) (Write)
*           Default storage form for the NDF's variance array.
*        DCB_VID( NDF__MXDCB ) = INTEGER (Write)
*           ARY_ system identifier for the NDF's variance array.
*        DCB_VTYP( NDF__MXDCB ) = CHARACTER * ( NDF__SZTYP ) (Write)
*           Default numeric data type for the NDF's variance array.
*        DCB_XLOC( NDF__MXDCB ) = CHARACTER * ( DAT__SZLOC ) (Read and
*        Write)
*           Locator to extension (MORE) structure.

      INCLUDE 'NDF_ACB'          ! NDF_ Access Control Block
*        ACB_CUT( NDF__MXACB ) = LOGICAL (Read)
*           Whether the NDF is a a cut.
*        ACB_DID( NDF__MXACB ) = INTEGER (Read)
*           ARY_ system identifier for the NDF's data array.
*        ACB_IDCB( NDF__MXACB ) = INTEGER (Read)
*           Index to data object entry in the DCB.
*        ACB_ISQBB( NDF__MXACB ) = LOGICAL (Read and Write)
*           Whether a bad-bits override value has been set.
*        ACB_QBB( NDF__MXACB ) = BYTE (Read and Write)
*           Quality component bad-bits override value.
*        ACB_QID( NDF__MXACB ) = INTEGER (Read)
*           ARY_ system identifier for the NDF's quality array.
*        ACB_VID( NDF__MXACB ) = INTEGER (Read)
*           ARY_ system identifier for the NDF's variance array.

*  Arguments Given:
      INTEGER IACB1
      INTEGER NEXTN
      CHARACTER * ( DAT__SZNAM ) EXTN( * )
      LOGICAL CPF( NDF__MXCPF )
      INTEGER IPCB

*  Arguments Returned:
      INTEGER IACB2

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER DUMMY( 1 )         ! Dummy dimension array
      INTEGER IDCB1              ! Input data object DCB entry index
      INTEGER IDCB2              ! Output data object DCB entry index
      INTEGER IWCS               ! Pointer to AST_ WCS Object
      INTEGER IWCSV              ! Pointer to stripped AST_ WCS Object
      INTEGER LBND( NDF__MXDIM ) ! Lower input NDF bounds
      INTEGER NDIM               ! Number of input NDF dimensions
      INTEGER NLEV               ! Levels in HDS path name
      INTEGER PLACE              ! ARY_ placeholder for data array
      INTEGER UBND( NDF__MXDIM ) ! Upper input NDF bounds
      LOGICAL STATE              ! Component defined?
      LOGICAL VALID              ! Whether array identifier is valid

*.

*  Set an initial value for the IACB2 argument.
      IACB2 = 0

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Obtain an index to the DCB entry of the input data object.
      IDCB1 = ACB_IDCB( IACB1 )

*  Obtain a free slot in the DCB for the output data object.
      CALL NDF1_FFS( NDF__DCB, IDCB2, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Propagate foreign format information from the input NDF to the
*  placeholder and then initialise the new (output) DCB entry with
*  information derived from the placeholder.
         CALL NDF1_PRFOR( IACB1, IPCB, STATUS )
         CALL NDF1_PLDCB( IPCB, IDCB2, STATUS )

*  Obtain the input NDF bounds from the ARY_ system identifier for its
*  data array, held in the ACB.
         CALL ARY_BOUND( ACB_DID( IACB1 ), NDF__MXDIM, LBND, UBND,
     :                   NDIM, STATUS )

*  DATA component.
*  ==============
*  If the DATA component is being propagated, then copy it into the
*  DATA_ARRAY component of the new NDF and store an ARY_ system
*  identifier for the new array in the new DCB entry.
         CALL ARY_PLACE( DCB_LOC( IDCB2 ), 'DATA_ARRAY', PLACE, STATUS )
         IF ( CPF( NDF__DCPF ) ) THEN
            CALL ARY_COPY( ACB_DID( IACB1 ), PLACE, DCB_DID( IDCB2 ),
     :                     STATUS )

*  If no error has occurred, we use NDF1_EVENT to flag a "data array
*  read" event. If the caller has registered a handler for this type of
*  event (using NDF_HNDLR), it will be called.
            IF( STATUS .EQ. SAI__OK ) THEN
               CALL NDF1_EVMSG( 'NDF_EVENT', IDCB1  )
               CALL NDF1_EVENT( 'READ_DATA', STATUS )
            END IF

*  If the DATA component is not being propagated, then create a new
*  (undefined) data array with the same attributes, storing an ARY_
*  system identifier for it in the new DCB entry.
         ELSE
            CALL ARY_DUPE( ACB_DID( IACB1 ), PLACE, DCB_DID( IDCB2 ),
     :                     STATUS )
         END IF

*  Propagate the default array attributes for the NDF to the new DCB
*  entry and update the storage form to take account of the new NDF
*  bounds if necessary.
         DCB_DETYP( IDCB2 ) = DCB_DETYP( IDCB1 )
         DCB_DECPX( IDCB2 ) = DCB_DECPX( IDCB1 )
         DCB_DEFRM( IDCB2 ) = DCB_DEFRM( IDCB1 )
         CALL NDF1_CBFRM( NDIM, LBND, UBND, DCB_DEFRM( IDCB2 ), STATUS )

*  Store the storage form of the new data array.
         CALL ARY_FORM( DCB_DID( IDCB2 ), DCB_DFRM( IDCB2 ), STATUS )

*  Note if the DCB data array information is correct.
         DCB_KD( IDCB2 ) = STATUS .EQ. SAI__OK

*  TITLE component.
*  ===============
*  If the TITLE component is being propagated, then ensure that
*  information about it is available in the input DCB entry.
         IF ( CPF( NDF__TCPF ) ) THEN
            CALL NDF1_DC( IDCB1, NDF__TITLE, STATUS )
            IF ( STATUS .EQ. SAI__OK ) THEN

*  If a title is present in the input object, then copy it to the
*  output object.
               IF ( DCB_CLOC( NDF__TITLE, IDCB1 ) .NE. DAT__NOLOC ) THEN
                  CALL DAT_COPY( DCB_CLOC( NDF__TITLE, IDCB1 ),
     :                           DCB_LOC( IDCB2 ), 'TITLE', STATUS )
               END IF
            END IF
         END IF

*  LABEL component.
*  ===============
*  If the LABEL component is being propagated, then ensure that
*  information about it is available in the input DCB entry.
         IF ( CPF( NDF__LCPF ) ) THEN
            CALL NDF1_DC( IDCB1, NDF__LABEL, STATUS )
            IF ( STATUS .EQ. SAI__OK ) THEN

*  If a label is present in the input object, then copy it to the
*  output object.
               IF ( DCB_CLOC( NDF__LABEL, IDCB1 ) .NE. DAT__NOLOC ) THEN
                  CALL DAT_COPY( DCB_CLOC( NDF__LABEL, IDCB1 ),
     :                           DCB_LOC( IDCB2 ), 'LABEL', STATUS )
               END IF
            END IF
         END IF

*  UNITS component.
*  ===============
*  If the UNITS component is being propagated, then ensure that
*  information about it is available in the input DCB entry.
         IF ( CPF( NDF__UCPF ) ) THEN
            CALL NDF1_DC( IDCB1, NDF__UNITS, STATUS )
            IF ( STATUS .EQ. SAI__OK ) THEN

*  If a units component is present in the input object, then copy it to
*  the output object.
               IF ( DCB_CLOC( NDF__UNITS, IDCB1 ) .NE. DAT__NOLOC ) THEN
                  CALL DAT_COPY( DCB_CLOC( NDF__UNITS, IDCB1 ),
     :                           DCB_LOC( IDCB2 ), 'UNITS', STATUS )
               END IF
            END IF
         END IF

*  QUALITY component.
*  ==================
*  Set initial null values for the new DCB entry's quality structure
*  locator and ARY_ system quality array identifier.
         DCB_QLOC( IDCB2 ) = DAT__NOLOC
         DCB_QID( IDCB2 ) = ARY__NOID

*  Propagate the default quality storage form to the output data object
*  and convert it to take account of the new NDF bounds if necessary.
         CALL NDF1_QFRM( IACB1, DCB_QFRM( IDCB2 ), STATUS )
         CALL NDF1_CBFRM( NDIM, LBND, UBND, DCB_QFRM( IDCB2 ), STATUS )
         IF ( STATUS .EQ. SAI__OK ) THEN

*  If the quality component is being propagated, then check that the
*  old DCB quality structure locator is valid (if not, then there is no
*  quality structure in the old data object). If so, then create a new
*  quality structure in the new data object and obtain a locator to it
*  for storage in the new DCB entry.
            IF ( CPF( NDF__QCPF ) ) THEN
               IF ( DCB_QLOC( IDCB1 ) .NE. DAT__NOLOC ) THEN
                  DUMMY( 1 ) = 0
                  CALL DAT_NEW( DCB_LOC( IDCB2 ), 'QUALITY', 'QUALITY',
     :                          0, DUMMY, STATUS )
                  CALL DAT_FIND( DCB_LOC( IDCB2 ), 'QUALITY',
     :                           DCB_QLOC( IDCB2 ), STATUS )

*  Copy the old BADBITS component into the new quality structure, if
*  available. Also propagate the badbits value to the new DCB entry.
                  CALL NDF1_CPYNC( DCB_QLOC( IDCB1 ), 'BADBITS',
     :                             DCB_QLOC( IDCB2 ), STATUS )
                  DCB_QBB( IDCB2 ) = DCB_QBB( IDCB1 )
               END IF

*  See if the old ACB ARY_ system identifier for the quality array is
*  valid. If not, then the quality array does not exist.
               CALL ARY_VALID( ACB_QID( IACB1 ), VALID, STATUS )
               IF ( STATUS .EQ. SAI__OK ) THEN

*  If it exists, then copy it into the new quality structure. Store the
*  resulting identifier in the new DCB entry.
                  IF ( VALID ) THEN
                     CALL ARY_PLACE( DCB_QLOC( IDCB2 ), 'QUALITY',
     :                               PLACE, STATUS )
                     CALL ARY_COPY( ACB_QID( IACB1 ), PLACE,
     :                              DCB_QID( IDCB2 ), STATUS )
                  END IF
               END IF
            END IF
         END IF

*  Note whether DCB quality information is correct.
         DCB_KQ( IDCB2 ) = STATUS .EQ. SAI__OK

*  VARIANCE component.
*  ==================
*  Set an initial null ARY_ system identifier for the new DCB entry's
*  variance array.
         DCB_VID( IDCB2 ) = ARY__NOID

*  Propagate the default variance attributes to the output data object
*  and convert the storage form to take account of the new NDF bounds
*  if necessary.
         CALL NDF1_VTYP( IACB1, DCB_VTYP( IDCB2 ), STATUS )
         CALL NDF1_VCPX( IACB1, DCB_VCPX( IDCB2 ), STATUS )
         CALL NDF1_VFRM( IACB1, DCB_VFRM( IDCB2 ), STATUS )
         CALL NDF1_CBFRM( NDIM, LBND, UBND, DCB_VFRM( IDCB2 ), STATUS )
         IF ( STATUS .EQ. SAI__OK ) THEN

*  If the variance component is being propagated, then see if the old
*  ACB ARY_ system identifier for the variance array is valid. If not,
*  then the variance array does not exist, so there is nothing more to
*  do.
            IF ( CPF( NDF__VCPF ) ) THEN
               CALL ARY_VALID( ACB_VID( IACB1 ), VALID, STATUS )
               IF ( STATUS .EQ. SAI__OK ) THEN

*  If it exists, then copy it into the VARIANCE component of the new
*  data object. Store the resulting identifier in the new DCB entry.
                  IF ( VALID ) THEN
                     CALL ARY_PLACE( DCB_LOC( IDCB2 ), 'VARIANCE',
     :                               PLACE, STATUS )
                     CALL ARY_COPY( ACB_VID( IACB1 ), PLACE,
     :                              DCB_VID( IDCB2 ), STATUS )
                  END IF
               END IF
            END IF
         END IF

*  Note whether DCB variance information is correct.
         DCB_KV( IDCB2 ) = STATUS .EQ. SAI__OK

*  AXIS component.
*  ==============
*  Propagate the axis component.
         CALL NDF1_APRP( IACB1, CPF( NDF__ACPF ), IDCB2, STATUS )

*  WCS component.
*  ==============
*  If the WCS component is being propagated, determine if WCS
*  information is defined for the input NDF.
         IF ( CPF( NDF__WCPF ) ) THEN
            CALL NDF1_WSTA( IACB1, STATE, STATUS )
            IF ( STATUS .EQ. SAI__OK ) THEN

*  If it is, then read the WCS information from the input NDF and
*  validate it using the input NDF's ACB entry. The validation should
*  always succeed, but this process also strips out the information we
*  do not want to store (e.g. associated with the pixel and axis
*  coordinate systems). Annul the original AST_ pointer.
               IF ( STATE ) THEN
                  CALL NDF1_RDWCS( IACB1, IWCS, STATUS )
                  CALL NDF1_VWCS( IACB1, IWCS, IWCSV, STATUS )
                  CALL AST_ANNUL( IWCS, STATUS )

*  Write the stripped information to the output data object.  This also
*  causes the output DCB entry to be updated with the WCS information.
                  CALL NDF1_WWRT( IWCSV, IDCB2, STATUS )

*  Annul the AST_ pointer to the stripped information.
                  CALL AST_ANNUL( IWCSV, STATUS )
               END IF
            END IF
         END IF

*  HISTORY component.
*  =================
*  Propagate the history component.
         CALL NDF1_HPRP( IDCB1, CPF( NDF__HCPF ), IDCB2, STATUS )

*  Extension (MORE) component.
*  ==========================
*  Ensure that extension information is available in the input DCB
*  entry.
         CALL NDF1_DX( IDCB1, STATUS )

*  Copy the extension (MORE) structure to the output data object,
*  omitting any of its components which aren't wanted.
         CALL NDF1_XCPY( DCB_XLOC( IDCB1 ), NEXTN, EXTN,
     :                   DCB_LOC( IDCB2 ), DCB_XLOC( IDCB2 ), STATUS )

*  Note whether extension information is available in the new DCB entry.
         DCB_KX( IDCB2 ) = STATUS .EQ. SAI__OK

*  Create a new base NDF entry in the ACB to describe the new data
*  object.
         CALL NDF1_CRNBN( IDCB2, IACB2, STATUS )

*  Assign the name of the data file to the MSG token "NDF_EVENT"
         CALL NDF1_EVMSG( 'NDF_EVENT', IDCB2 )

*  Raise an NDF event, describing the opening of a new NDF.
         CALL NDF1_EVENT( 'OPEN_NEW_NDF', STATUS )

*  Propagate any bad-bits override value for the NDF's quality
*  component to the new ACB entry.
         IF ( STATUS .EQ. SAI__OK ) THEN
            ACB_QBB( IACB2 ) = ACB_QBB( IACB1 )
            ACB_ISQBB( IACB2 ) = ACB_ISQBB( IACB1 )

*  If there was an error, then annul the new DCB entry.
         ELSE
            CALL NDF1_DANL( .TRUE., IDCB2, STATUS )
            IDCB2 = 0
         ENDIF
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_PRP', STATUS )

      END
