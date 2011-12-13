      SUBROUTINE NDF1_FFS( BLOCK, SLOT, STATUS )
*+
*  Name:
*     NDF1_FFS

*  Purpose:
*     Find a free slot in a common block.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_FFS( BLOCK, SLOT, STATUS )

*  Description:
*     The routine finds a free slot in one of the common blocks
*     maintained by the NDF_ system. The number of a free slot is
*     returned and the slot is marked as used and initialised. An error
*     is reported if no further free slots exist.

*  Arguments:
*     BLOCK = INTEGER (Given)
*        The block in which a free slot is required.  The integer
*        symbolic constants NDF__DCB, NDF__ACB and NDF__PCB are
*        available to identify these.
*     SLOT = INTEGER (Returned)
*        The number of a free slot in the requested block.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  If STATUS is set on entry, then a SLOT value of zero will be
*     returned, although no further processing will occur.
*     -  A SLOT value of zero will also be returned if the routine
*     should fail for any reason.

*  Copyright:
*     Copyright (C) 1997 Rutherford Appleton Laboratory
*     Copyright (C) 2009 Science & Technology Facilities Council.
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
*     DSB: David S Berry (JC, UCLan)
*     {enter_new_authors_here}

*  History:
*     19-SEP-1989 (RFWS):
*        Original, derived from the equivalent ARY_ system routine.
*     23-OCT-1990 (RFWS):
*        Added initialisation of DCB and ACB slots.
*     15-NOV-1990 (RFWS):
*        Added missing DCB components to initialisation.
*     21-DEC-1990 (RFWS):
*        Added initialisation of DCB_QBB value.
*     7-MAY-1993 (RFWS):
*        Added initialisation of DCB history settings.
*     18-MAY-1993 (RFWS):
*        Added initialisation of the DCB_HUMOD value.
*     7-SEP-1993 (RFWS):
*        Improved error messages.
*     27-SEP-1993 (RFWS):
*        Initialise DCB_HEXT entry to 5.
*     4-NOV-1993 (RFWS):
*        Added initialisation of PCB entries and of new DCB entries
*        that support foreign format files.
*     5-NOV-1993 (RFWS):
*        Split long error messages into two.
*     11-NOV-1993 (RFWS):
*        Initialise the DCB and PCB keep NDF objects flags and the DCB
*        foreign file existence flag.
*     9-MAR-1994 (RFWS):
*        Added initialisation of PCB_NEW flag.
*     16-MAR-1994 (RFWS):
*        Initialise DCB_FORID.
*     25-MAY-1994 (RFWS):
*        Initialise PCB_FORID.
*     1-JUL-1997 (RFWS):
*        Added support for the WCS component.
*     23-JAN-2009 (DSB):
*        Added DCB_HTIME and DCB_HSORT.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'NDF_PAR'          ! NDF_ public constants
      INCLUDE 'NDF_CONST'        ! NDF_ private constants
      INCLUDE 'NDF_ERR'          ! NDF_ error codes
      INCLUDE 'AST_PAR'          ! AST_ public interface

*  Global Variables:
      INCLUDE 'NDF_DCB'          ! NDF_ Data Control Block
*        DCB_DSP( NDF__MXDCB ) = CHARACTER * ( NDF__SZDSP ) (Write)
*           Data object disposal mode.
*        DCB_FOREX( NDF__MXDCB ) = LOGICAL (Write)
*           Whether the associated foreign file (if any) existed before
*           the NDF library accessed it.
*        DCB_FORID( NDF__MXDCB ) = CHARACTER * ( NDF__SZFID ) (Write)
*           Foreign file identification code.
*        DCB_FORKP( NDF__MXDCB ) = LOGICAL (Write)
*           Whether the NDF copy of the foreign file is to be kept.
*        DCB_HDEF( NDF__MXDCB ) = LOGICAL (Write)
*           Whether default history information is to be written.
*        DCB_HEXT( NDF__MXDCB ) = INTEGER (Write)
*           Extension increment for the history records array.
*        DCB_HLOC( NDF__MXDCB ) = CHARACTER * ( DAT__SZLOC ) (Write)
*           Locator for NDF history component.
*        DCB_HSORT( NDF__MXDCB ) = LOGICAL (Write)
*           Do the history records need sorting?
*        DCB_HNREC( NDF__MXDCB ) = INTEGER (Write)
*           Number of valid history records present.
*        DCB_HRLOC( NDF__MXDCB ) = CHARACTER * ( DAT__SZLOC ) (Write)
*           Locator for array of history records.
*        DCB_HTIME( NDF__MXDCB ) = DOUBLE PRECISION (Write)
*           The date/time to attach to the next history record to be
*           created, as a UTC Modified Julian Date. If negative, then
*           the current time will be used.
*        DCB_HTLEN( NDF__MXDCB ) = LOGICAL (Write)
*           History current record text length.
*        DCB_HUMOD( NDF__MXDCB ) = INTEGER (Read)
*           History recording update mode.
*        DCB_IFMT( NDF__MXDCB ) = INTEGER (Write)
*           FCB format code for associated foreign file (zero if no
*           foreign file exists).
*        DCB_ISQBB( NDF__MXDCB ) = LOGICAL (Write)
*           Whether quality bad-bits value has been over-ridden.
*        DCB_IWCS( NDF__MXDCB ) = INTEGER (Write)
*           Pointer to AST_ WCS information.
*        DCB_KA( NDF__MXDCB ) = LOGICAL (Write)
*           Whether axis component information is available.
*        DCB_KAC( NDF__MXDIM, NDF__MXACN, NDF__MXDCB ) = LOGICAL
*        (Write)
*           Whether information about axis character components is
*           available.
*        DCB_KAD( NDF__MXDIM, NDF__MXDCB ) = LOGICAL (Write)
*           Whether information about axis data arrays is available.
*        DCB_KAN( NDF__MXDIM, NDF__MXDCB ) = LOGICAL (Write)
*           Whether information is available about axis normalisation.
*        DCB_KAV( NDF__MXDIM, NDF__MXDCB ) = LOGICAL (Write)
*           Whether information is available about axis variance arrays.
*        DCB_KAW( NDF__MXDIM, NDF__MXDCB ) = LOGICAL (Write)
*           Whether information about axis width arrays is available.
*        DCB_KAX( NDF__MXDIM, NDF__MXDCB ) = LOGICAL (Write)
*           Whether axis extension information is available.
*        DCB_KC( NDF__MXDCB ) = LOGICAL (Write)
*           Whether character component information is available.
*        DCB_KD( NDF__MXDCB ) = LOGICAL (Write)
*           Whether information about the NDF's data array component is
*           available in the DCB.
*        DCB_KH( NDF__MXDCB ) = LOGICAL (Write)
*           Whether DCB information is available for the NDF's history
*           component.
*        DCB_KQ( NDF__MXDCB ) = LOGICAL (Write)
*           Whether quality information is available.
*        DCB_KV( NDF__MXDCB ) = LOGICAL (Write)
*           Whether information about the NDF's variance component is
*           available in the DCB.
*        DCB_KW( NDF__MXDCB ) = LOGICAL (Write)
*           Whether information about the NDF's WCS component is
*           available in the DCB.
*        DCB_KX( NDF__MXDCB ) = LOGICAL (Write)
*           Whether extension (MORE) structure information is available.
*        DCB_LOC( NDF__MXDCB ) = CHARACTER * ( DAT__SZLOC ) (Write)
*           Data object locator.
*        DCB_MOD( NDF__MXDCB ) = CHARACTER * ( NDF__SZMOD ) (Write)
*           The NDF's access mode.
*        DCB_NADMP( NDF__MXDIM, NDF__MXDCB ) = INTEGER (Write)
*           Number of current mappings to each axis data array.
*        DCB_NAVMP( NDF__MXDIM, NDF__MXDCB ) = INTEGER (Write)
*           Number of current mappings to each axis variance array.
*        DCB_NAWMP( NDF__MXDIM, NDF__MXDCB ) = INTEGER (Write)
*           Number of current mappings to each axis width array.
*        DCB_NDMAP( NDF__MXDCB ) = INTEGER (Write)
*           Number of mappings to the NDF's data array.
*        DCB_NMAP( NDF__MXDCB ) = INTEGER (Write)
*           Total number of current mappings to each data object.
*        DCB_NQMAP( NDF__MXDCB ) = INTEGER (Write)
*           Number of mappings to the NDF's quality array.
*        DCB_NWMAP( NDF__MXDCB ) = INTEGER (Write)
*           Number of mappings to the NDF's width array.
*        DCB_OVQBB( NDF__MXDCB ) = BYTE (Write)
*           Unsigned byte over-ride value for quality bad-bits.
*        DCB_QBB( NDF__MXDCB ) = BYTE (Write)
*           Quality bad bits mask.
*        DCB_REFCT( NDF__MXDCB ) = INTEGER (Write)
*           Number of ACB entries which refer to each DCB entry.
*        DCB_USED( NDF__MXDCB ) = LOGICAL (Read and Write)
*           Whether a slot in the DCB has been used.

      INCLUDE 'NDF_ACB'          ! NDF_ Access Control Block
*        ACB_ACC( NDF__MXACC, NDF__MXACB ) = LOGICAL (Write)
*           Access control flags.
*        ACB_ADMAP( NDF__MXDIM, NDF__MXACB ) = LOGICAL (Write)
*           Whether NDF axis data arrays are currently mapped for
*           access.
*        ACB_AVMAP( NDF__MXDIM, NDF__MXACB ) = LOGICAL (Write)
*           Whether NDF axis variance arrays are currently mapped for
*           access.
*        ACB_AWMAP( NDF__MXDIM, NDF__MXACB ) = LOGICAL (Write)
*           Whether NDF axis wdth arrays are currently mapped for
*           access.
*        ACB_CUT( NDF__MXACB ) = LOGICAL (Write)
*           Whether an NDF is a cut (i.e. section).
*        ACB_DMAP( NDF__MXACB ) = LOGICAL (Write)
*           Whether the NDF's data array is mapped for access.
*        ACB_IDCB( NDF__MXACB ) = INTEGER (Write)
*           Index to data object entry in the DCB.
*        ACB_ISQBB( NDF__MXACB ) = LOGICAL (Write)
*           Whether a quality bad-bits override value is set.
*        ACB_QBB( NDF__MXACB ) = BYTE (Write)
*           Quality bad-bits override value.
*        ACB_QMAP( NDF__MXACB ) = LOGICAL (Write)
*           Whether the NDF's quality array is mapped for access.
*        ACB_QMF( NDF__MXACB ) = LOGICAL (Write)
*           Quality masking flag.
*        ACB_USED( NDF__MXACB ) = LOGICAL (Read and Write)
*           Whether a slot in the ACB has been used.
*        ACB_VMAP( NDF__MXACB ) = LOGICAL (Write)
*           Whether the NDF's variance array is mapped for access.

      INCLUDE 'NDF_PCB'          ! NDF_ Placeholder Control Block
*        PCB_FORID( NDF__MXPCB ) = CHARACTER * ( NDF__SZFID ) (Write)
*           Foreign file identification code.
*        PCB_FORKP( NDF__MXPCB ) = LOGICAL (Write)
*           Whether the NDF copy of the foreign file is to be kept.
*        PCB_IFMT( NDF__MXPCB ) = INTEGER (Write)
*           FCB format code for associated foreign file (zero if no
*           foreign file exists).
*        PCB_LOC( NDF__MXPCB ) = CHARACTER * ( DAT__SZLOC ) (Write)
*           Locator for placeholder object.
*        PCB_NEW( NDF__MXPCB ) = LOGICAL (Write)
*           Whether a new placeholder object was created.
*        PCB_PRFMT( NDF__MXPCB ) = LOGICAL( Write)
*           Whether to propagate foreign format information.
*        PCB_TMP( NDF__MXPCB ) = LOGICAL (Write)
*           Whether NDF is to be temporary.
*        PCB_USED( NDF__MXPCB ) = LOGICAL (Read and Write)
*           Whether a slot in the PCB has been used.

*  Arguments Given:
      INTEGER BLOCK

*  Arguments Returned:
      INTEGER SLOT

*  Status:
      INTEGER STATUS             ! Global status

*  External references:
      EXTERNAL NDF1_INIT         ! Initialise common blocks

*  Local Constants:
      BYTE ZEROUB                ! Zero as an unsigned byte value
      PARAMETER ( ZEROUB = 0 )

*  Local variables:
      INTEGER I                  ! Loop counter for slots
      INTEGER IACC               ! Loop counter for access control flags
      INTEGER IAX                ! Loop counter for NDF axes
      INTEGER J                  ! Loop counter for character components

*.

*  Set an initial value for the SLOT argument.
      SLOT = 0

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Data Control Block.
*  ==================
*  If requested, search for a free slot in the Data Control Block.
      IF ( BLOCK .EQ. NDF__DCB ) THEN
         DO 1 I = 1, NDF__MXDCB
            IF ( .NOT. DCB_USED( I ) ) THEN
               DCB_USED( I ) = .TRUE.
               SLOT = I
               GO TO 2
            END IF
 1       CONTINUE

*  If no free slot could be found, then report an error.
         STATUS = NDF__EXSLT
         CALL MSG_SETI( 'NSLOT', NDF__MXDCB )
         CALL ERR_REP( 'NDF1_FFS_DCB1',
     : 'All ^NSLOT slots allocated for entries in the NDF_ system ' //
     : 'Data Control Block have been used up; too many separate ' //
     : 'NDF data structures in use at once.',
     :                 STATUS )
         CALL ERR_REP( 'NDF1_FFS_DCB2',
     : 'Are all NDFs being released when no longer needed (possible ' //
     : 'programming error)?',
     :                 STATUS )
 2       CONTINUE

*  If a slot in the DCB was found successfully, then initialise it.
         IF ( STATUS .EQ. SAI__OK ) THEN
            DCB_REFCT( SLOT ) = 0        ! Reference count
            DCB_NMAP( SLOT ) = 0         ! Number of mappings
            DCB_LOC( SLOT ) = DAT__NOLOC ! Data object locator
            DCB_MOD( SLOT ) = 'READ'     ! Access mode
            DCB_DSP( SLOT ) = 'KEEP'     ! Disposal mode

            DCB_KD( SLOT ) = .FALSE.     ! Data info. available?
            DCB_NDMAP( SLOT ) = 0        ! Number of data mappings

            DO 3 J = 1, NDF__MXCCN
               DCB_KC( J, SLOT ) = .FALSE. ! Character info. available?
 3          CONTINUE

            DCB_KQ( SLOT ) = .FALSE.     ! Quality info. available?
            DCB_NQMAP( SLOT ) = 0        ! Number of quality mappings
            DCB_QBB( SLOT ) = ZEROUB     ! Bad-bits value
            DCB_ISQBB( SLOT ) = .FALSE.  ! Quality bad-bits override?
            DCB_OVQBB( SLOT ) = ZEROUB   ! Bad-bits override value

            DCB_KV( SLOT ) = .FALSE.     ! Variance info. available?
            DCB_NVMAP( SLOT ) = 0        ! Number of variance mappings

            DCB_KX( SLOT ) = .FALSE.     ! Extension info. available?

            DCB_KA( SLOT ) = .FALSE.     ! Axis info. available?
            DO 5 IAX = 1, NDF__MXDIM
               DCB_KAD( IAX, SLOT ) = .FALSE. ! Axis data info?
               DCB_NADMP( IAX, SLOT ) = 0     ! No. axis data mappings
               DO 4 J = 1, NDF__MXACN
                  DCB_KAC( IAX, J, SLOT ) = .FALSE. ! Axis char. info?
 4             CONTINUE

               DCB_KAV( IAX, SLOT ) = .FALSE. ! Axis variance info?
               DCB_NAVMP( IAX, SLOT ) = 0     ! No. axis var. mappings

               DCB_KAW( IAX, SLOT ) = .FALSE. ! Axis width info?
               DCB_NAWMP( IAX, SLOT ) = 0     ! No. axis width mappings

               DCB_KAN( IAX, SLOT ) = .FALSE. ! Axis normalisation info?
               DCB_KAX( IAX, SLOT ) = .FALSE. ! Axis extension info?
 5          CONTINUE

            DCB_KH( SLOT ) = .FALSE.     ! History info. available?
            DCB_HLOC( SLOT ) = DAT__NOLOC ! History structure locator
            DCB_HRLOC( SLOT ) = DAT__NOLOC ! History records locator
            DCB_HSORT( SLOT ) = .FALSE.  ! History records are in order
            DCB_HNREC( SLOT ) = 0        ! No. history records present
            DCB_HEXT( SLOT ) = 5         ! History records extend size
            DCB_HDEF( SLOT ) = .TRUE.    ! Default history required
            DCB_HTLEN( SLOT ) = 0        ! History not modified
            DCB_HTIME( SLOT ) = -1.0D0   ! History record takes current time
            DCB_HUMOD( SLOT ) = NDF__HNORM ! History update mode

            DCB_IFMT( SLOT ) = 0         ! Foreign file format code
            DCB_FORID( SLOT ) = ' '      ! Foreign file identification
            DCB_FOREX( SLOT ) = .FALSE.  ! Assume foreign non-existence
            DCB_FORKP( SLOT ) = .FALSE.  ! Keep native NDF copy?

            DCB_KW( SLOT ) = .FALSE.     ! WCS info. available?
            DCB_IWCS( SLOT ) = AST__NULL ! WCS Object pointer (AST_)
         END IF

*  Access Control Block.
*  ====================
*  If requested, search for a free slot in the Access Control Block.
      ELSE IF ( BLOCK .EQ. NDF__ACB ) THEN
         DO 6 I = 1, NDF__MXACB
            IF ( .NOT. ACB_USED( I ) ) THEN
               ACB_USED( I ) = .TRUE.
               SLOT = I
               GO TO 7
            END IF
 6       CONTINUE

*  If no free slot could be found, then report an error.
         STATUS = NDF__EXSLT
         CALL MSG_SETI( 'NSLOT', NDF__MXACB )
         CALL ERR_REP( 'NDF1_FFS_ACB1',
     : 'All ^NSLOT slots allocated for entries in the NDF_ system ' //
     : 'Access Control Block have been used up; too many NDF ' //
     : 'identifiers in use at once.',
     :                 STATUS )
         CALL ERR_REP( 'NDF1_FFS_ACB2',
     : 'Are all identifiers being annulled when no longer needed ' //
     : '(possible programming error)?',
     :                 STATUS )
 7       CONTINUE

*  If a slot in the ACB was found successfully, then initialise it.
         IF ( STATUS .EQ. SAI__OK ) THEN
            DO 8 IACC = 1, NDF__MXACC
               ACB_ACC( IACC, SLOT ) = .FALSE. ! Access control flags
 8          CONTINUE
            ACB_CUT( SLOT ) = .FALSE.    ! NDF is a section?

            ACB_IDCB( SLOT ) = 0         ! Pointer to DCB entry
            ACB_DMAP( SLOT ) = .FALSE.   ! Data mapped?
            ACB_VMAP( SLOT ) = .FALSE.   ! Variance mapped?
            ACB_QMAP( SLOT ) = .FALSE.   ! Quality mapped?
            ACB_QMF( SLOT ) = .TRUE.     ! Quality masking flag
            ACB_QBB( SLOT ) = ZEROUB     ! Bad-bits override value
            ACB_ISQBB( SLOT ) = .FALSE.  ! Bad-bits override set?

            DO 9 IAX = 1, NDF__MXDIM
               ACB_ADMAP( IAX, SLOT ) = .FALSE. ! Axis data mapped?
               ACB_AVMAP( IAX, SLOT ) = .FALSE. ! Axis variance mapped?
               ACB_AWMAP( IAX, SLOT ) = .FALSE. ! Axis width mapped?
 9          CONTINUE
         END IF

*  Placeholder Control Block.
*  =========================
*  If requested, search for a free slot in the Placeholder Control
*  Block.
      ELSE IF ( BLOCK .EQ. NDF__PCB ) THEN
         DO 10 I = 1, NDF__MXPCB
            IF ( .NOT. PCB_USED( I ) ) THEN
               PCB_USED( I ) = .TRUE.
               SLOT = I
               GO TO 11
            END IF
 10      CONTINUE

*  If no free slot could be found, then report an error.
         STATUS = NDF__EXSLT
         CALL MSG_SETI( 'NSLOT', NDF__MXPCB )
         CALL ERR_REP( 'NDF1_FFS_PCB1',
     : 'All ^NSLOT slots allocated for entries in the NDF_ system ' //
     : 'Placeholder Control Block have been used up; too many NDF ' //
     : 'placeholders in use at once.',
     :                 STATUS )
         CALL ERR_REP( 'NDF1_FFS_PCB2',
     : 'Are placeholders remaining unused (possible programming ' //
     : 'error)?',
     :                 STATUS )
 11      CONTINUE

*  If a slot in the PCB was found successfully, then initialise it.
         IF ( STATUS .EQ. SAI__OK ) THEN
            PCB_LOC( SLOT ) = DAT__NOLOC ! Locator to placeholder object
            PCB_NEW( SLOT ) = .TRUE.     ! New placeholder object?
            PCB_TMP( SLOT ) = .FALSE.    ! Object to be temporary?
            PCB_IFMT( SLOT ) = 0         ! Foreign file format code
            PCB_PRFMT( SLOT ) = .FALSE.  ! Propagate foreign format?
            PCB_FORKP( SLOT ) = .FALSE.  ! Keep native NDF copy?
            PCB_FORID( SLOT ) = ' '      ! Foreign file identification
         END IF

*  If the block specified was invalid, then report an error.
      ELSE
         STATUS = NDF__FATIN
         CALL MSG_SETC( 'ROUTINE', 'NDF1_FFS' )
         CALL MSG_SETI( 'BADBLOCK', BLOCK )
         CALL ERR_REP( 'NDF1_FFS_BLOCK',
     :   'Routine ^ROUTINE called with an invalid BLOCK argument of ' //
     :   '^BADBLOCK (internal programming error).', STATUS )
      END IF

*  Call error tracing routine and exit.
      IF( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_FFS', STATUS )

      END
