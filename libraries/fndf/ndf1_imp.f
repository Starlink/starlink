      SUBROUTINE NDF1_IMP( LOC, IACB, STATUS )
*+
*  Name:
*     NDF1_IMP

*  Purpose:
*     Import an NDF structure into the ACB.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_IMP( LOC, IACB, STATUS )

*  Description:
*     The routine imports an NDF data structure, identified by its HDS
*     locator, into the ACB, returning the index to the base NDF entry
*     in the ACB allocated for it. This routine detects if the same
*     data object has previously been imported and takes account of
*     this possibility.

*  Arguments:
*     LOC = CHARACTER * ( * ) (Given)
*        HDS locator to the data object to be imported.
*     IACB = INTEGER (Returned)
*        Index to the resulting base NDF entry in the ACB.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  If STATUS is set on entry, then a value of zero will be
*     returned for the IACB argument, although no further processing
*     will occur.
*     -  A value of zero will also be returned for the IACB argument if
*     the routine fails for any reason.

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
*     DSB: David S Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     26-SEP-1989 (RFWS):
*        Original, derived from the equivalent ARY_ system routine.
*     15-DEC-1989 (RFWS):
*        Added code to transfer the mapping counts to the DCB entry
*        being retained.
*     15-JAN-1990 (RFWS):
*        Added code to ensure that default array attributes are not lost
*        if a DCB entry is replaced due to re-importation of an existing
*        data object.
*     30-JAN-1990 (RFWS):
*        Installed support for the quality component.
*     16-MAR-1990 (RFWS):
*        Moved the call to NDF1_CRNBN to the end of the routine, so
*        that DCB values set within the body of the routine can be
*        correctly propagated to the new ACB entry.
*     5-APR-1990 (RFWS):
*        Fixed bug where the DCB reference count was being
*        unnecessarily incremented; NDF1_CRNBN performs this task when
*        it creates an ACB entry, so it does not need to be done by
*        this routine.
*     27-NOV-1990 (RFWS):
*        Added propagation of axis attributes in cases where two DCB
*        entries are merged.
*     28-NOV-1990 (RFWS):
*        Removed transfer of default attributes between DCB entries
*        when they are merged. This only occurs if the initial DCB
*        entry only has read access available so these values cannot
*        have been changed and therefore do not need preserving.
*     29-NOV-1990 (RFWS):
*        Fixed bug in merging of DCB reference counts.
*     28-FEB-1991 (RFWS):
*        Fixed additional bug in merging of DCB reference counts in the
*        case where the old DCB entry is retained.
*     16-FEB-1993 (RFWS):
*        Fixed bug where quality and variance information was not being
*        made available to the new ACB entry when a duplicate DCB entry
*        had been created. Added calls to NDF1_QIMP and NDF1_VIMP to
*        obtain this information if it is already available for the old
*        DCB entry (and its associated ACB entries).
*     11-MAR-1994 (RFWS):
*        Added propagation of foreign format file information to the new
*        DCB entry if necessary.
*     16-MAR-1994 (RFWS):
*        Added propagation of DCB_FORID values.
*     16-MAY-1995 (RFWS):
*        Changed way in which DCB slot number is retained when the slot
*        is annulled (use of TEMP variable) to avoid an optimisation bug
*        that appeared in the OSF1 compiler (new version).
*     9-FEB-2005 (DSB):
*        If the data object already has an entry in the DCB, and that
*        entry is associated with a foreign file, then do not let the new
*        DCB entry have update access if the original does not. Without
*        this, the old DCB entry is promoted to UPDATE access, with the
*        result that an attempt is made to delete the foreign file when
*        the reference count for the DCB falls to zero. See similar
*        restriction on access in NDF1_OPFOR.
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

*  Global Variables:
      INCLUDE 'NDF_DCB'          ! NDF_ Data Control Block
*        DCB_FILE( NDF__MXDCB ) = CHARACTER * ( NDF__SZFIL ) (Read)
*           Data object container file name.
*        DCB_FOREX( NDF__MXDCB ) = LOGICAL (Read and Write)
*           Whether the associated foreign file (if any) existed before
*           the NDF library accessed it.
*        DCB_FORFL( NDF__MXDCB ) = CHARACTER * ( NDF__SZFIL ) (Read and
*        Write)
*           Name of foreign format file associated with NDF.
*        DCB_FORID( NDF__MXDCB ) = CHARACTER * ( NDF__SZFID ) (Read and
*        Write)
*           Unique ID for foreign format file associated with NDF.
*        DCB_FORKP( NDF__MXDCB ) = LOGICAL (Read and Write)
*           Whether the NDF copy of the foreign file is to be kept.
*        DCB_IFMT( NDF__MXDCB ) = INTEGER (Read and Write)
*           FCB index identifying the format of the associated foreign
*           file.
*        DCB_KAD( NDF__MXDIM, NDF__MXDCB ) = LOGICAL (Read)
*           Whether information about axis data arrays is available.
*        DCB_KAN( NDF__MXDIM, NDF__MXDCB ) = LOGICAL (Read)
*           Whether information is available about axis normalisation.
*        DCB_KAV( NDF__MXDIM, NDF__MXDCB ) = LOGICAL (Read)
*           Whether information is available about axis variance arrays.
*        DCB_KAW( NDF__MXDIM, NDF__MXDCB ) = LOGICAL (Read)
*           Whether information about axis width arrays is available.
*        DCB_KQ( NDF__MXDCB ) = LOGICAL (Read)
*           Whether quality array information is available in the DCB
*           entry.
*        DCB_KV( NDF__MXDCB ) = LOGICAL (Read)
*           Whether variance array information is available in the DCB
*           entry.
*        DCB_MOD( NDF__MXDCB ) = CHARACTER * ( NDF__SZMOD ) (Read)
*           Data object access mode.
*        DCB_NADMP( NDF__MXDIM, NDF__MXDCB ) = INTEGER (Read and Write)
*           Number of current mappings to each axis data array.
*        DCB_NAVMP( NDF__MXDIM, NDF__MXDCB ) = INTEGER (Read and Write)
*           Number of current mappings to each axis variance array.
*        DCB_NAWMP( NDF__MXDIM, NDF__MXDCB ) = INTEGER (Read and Write)
*           Number of current mappings to each axis width array.
*        DCB_NDMAP( NDF__MXDCB ) = INTEGER (Read and Write)
*           Number of current mappings for the NDF's data array.
*        DCB_NMAP( NDF__MXDCB ) = INTEGER (Read and Write)
*           Total number of mappings for the NDF.
*        DCB_NQMAP( NDF__MXDCB ) = INTEGER (Read and Write)
*           Number of current mappings for the NDF's quality array.
*        DCB_NVMAP( NDF__MXDCB ) = INTEGER (Read and Write)
*           Number of current mappings for the NDF's variance array.
*        DCB_PATH( NDF__MXDCB ) = CHARACTER * ( NDF__SZPTH ) (Read)
*           Data object path name.
*        DCB_REFCT( NDF__MXDCB ) = INTEGER (Read and Write)
*           Number of ACB entries referring to data object.

      INCLUDE 'NDF_ACB'          ! NDF_ Access Control Block
*        ACB_IDCB( NDF__MXACB ) = INTEGER (Read and Write)
*           Pointer to data object entry in the DCB.

*  Arguments Given:
      CHARACTER * ( * ) LOC

*  Arguments Returned:
      INTEGER IACB

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER IACBT              ! ACB entry to be tested
      INTEGER IAX                ! Loop counter for axes
      INTEGER IDCB               ! Index to new DCB entry
      INTEGER IDCBA              ! DCB entry to be annulled
      INTEGER IDCBK              ! DCB entry to be kept
      INTEGER IDCBT              ! DCB entry to be tested/compared
      INTEGER NEXT               ! Next common block entry to consider
      INTEGER TEMP               ! Temporary store for DCB slot number
      LOGICAL DUPE               ! Whether DCB entry is duplicated

*.

*  Set an initial value for the IACB argument.
      IACB = 0

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Import the data object into the DCB, occupying a new DCB slot, and
*  create a new ACB base NDF entry to describe the new data object.
      CALL NDF1_DIMP( LOC, IDCB, STATUS )
      CALL NDF1_CRNBN( IDCB, IACB, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Loop through all the DCB entries to see whether this same data object
*  has previously been imported.
         DUPE = .FALSE.
         NEXT = 0
         IDCBT = 0
 1       CONTINUE                ! Start of 'DO WHILE' loop
         CALL NDF1_NXTSL( NDF__DCB, IDCBT, NEXT, STATUS )
         IF ( ( STATUS .EQ. SAI__OK ) .AND. ( NEXT .NE. 0 ) ) THEN
            IDCBT = NEXT

*  Search for DCB entries which differ from the one just created, but
*  which have the same data file and object path name.
            IF ( ( IDCBT .NE. IDCB ) .AND.
     :           ( DCB_FILE( IDCBT ) .EQ. DCB_FILE( IDCB ) ) .AND.
     :           ( DCB_PATH( IDCBT ) .EQ. DCB_PATH( IDCB ) ) ) THEN
               DUPE = .TRUE.
               GO TO 2
            END IF
            GO TO 1
         END IF
 2       CONTINUE

*  If duplicate DCB entries exist, then they must be combined into a
*  single entry, but account must be taken of possible differences in
*  the access mode when the same data object is imported several times.
*  Ensure that data array information (which includes the access mode)
*  is available for both DCB entries.
         IDCBK = IDCB
         IF ( STATUS .EQ. SAI__OK ) THEN
            IF ( DUPE ) THEN
               CALL NDF1_DD( IDCB, STATUS )
               CALL NDF1_DD( IDCBT, STATUS )

*  If the existing DCB entry is associated with an existing foreign file,
*  we want to respect the access available to that foreign file. So
*  if the new NDF object is open for UPDATE access (either because it
*  must later be deleted or because it resides in a temporary file
*  which was previously opened with this access mode) then the DCB
*  access mode entry will reflect this. If UPDATE access to the
*  object's contents is not available through the existing DCB entry, then
*  modify the new DCB entry, since it will otherwise cause the NDF's
*  contents to be written back to the foreign file (with format conversion)
*  when it is released.
               IF( DCB_FOREX( IDCBT ) .AND.
     :             DCB_MOD( IDCBT ) .EQ. 'READ' ) THEN
                  DCB_MOD( IDCB ) = 'READ'
               END IF

*  If quality or variance array information is available for the
*  original DCB entry, then ensure that it is also available for the
*  new one (and for the new ACB entry associated with it).
               IF ( DCB_KQ( IDCBT ) ) CALL NDF1_QIMP( IACB, STATUS )
               IF ( DCB_KV( IDCBT ) ) CALL NDF1_VIMP( IACB, STATUS )
               IF ( STATUS .EQ. SAI__OK ) THEN

*  For preference, we keep the DCB entry which was there first, and
*  annul the new one.
                  IDCBK = IDCBT
                  IDCBA = IDCB

*  However, if the new entry has UPDATE access to the data object,
*  whereas the first one does not, then the new DCB entry has to be
*  kept at the expense of the old one.
                  IF ( ( DCB_MOD( IDCBK ) .NE. 'UPDATE' ) .AND.
     :                 ( DCB_MOD( IDCBA ) .EQ. 'UPDATE' ) ) THEN
                     IDCBK = IDCB
                     IDCBA = IDCBT

*  Transfer the old reference count and mapping counts to the new DCB
*  entry.
                     DCB_REFCT( IDCBK ) = DCB_REFCT( IDCBA )
                     DCB_NMAP( IDCBK ) = DCB_NMAP( IDCBA )
                     DCB_NDMAP( IDCBK ) = DCB_NDMAP( IDCBA )
                     DCB_NQMAP( IDCBK ) = DCB_NQMAP( IDCBA )
                     DCB_NVMAP( IDCBK ) = DCB_NVMAP( IDCBA )

*  Transfer the axis array mapping counts.
                     DO 3 IAX = 1, NDF__MXDIM
                        DCB_NADMP( IAX, IDCBK ) = DCB_NADMP( IAX,
     :                                                       IDCBA )
                        DCB_NAVMP( IAX, IDCBK ) = DCB_NADMP( IAX,
     :                                                       IDCBA )
                        DCB_NAWMP( IAX, IDCBK ) = DCB_NAWMP( IAX,
     :                                                       IDCBA )

*  Ensure that the same DCB axis information is available for the new
*  entry as was available for the old one.
                        IF ( DCB_KAD( IAX, IDCBA ) )
     :                     CALL NDF1_DAD( IAX, IDCBK, STATUS )
                        IF ( DCB_KAV( IAX, IDCBA ) )
     :                     CALL NDF1_DAV( IAX, IDCBK, STATUS )
                        IF ( DCB_KAW( IAX, IDCBA ) )
     :                     CALL NDF1_DAW( IAX, IDCBK, STATUS )
                        IF ( DCB_KAN( IAX, IDCBA ) )
     :                     CALL NDF1_DAN( IAX, IDCBK, STATUS )
 3                   CONTINUE
                  END IF

*  Transfer any foreign format file information associated with the old
*  DCB entry.
                  DCB_IFMT( IDCBK ) = DCB_IFMT( IDCBA )
                  DCB_FORFL( IDCBK ) = DCB_FORFL( IDCBA )
                  DCB_FORID( IDCBK ) = DCB_FORID( IDCBA )
                  DCB_FORKP( IDCBK ) = DCB_FORKP( IDCBA )
                  DCB_FOREX( IDCBK ) = DCB_FOREX( IDCBA )

*  Increment the reference count for the DCB entry being kept.
                  DCB_REFCT( IDCBK ) = DCB_REFCT( IDCBK ) + 1

*  Reset the reference count for the old DCB entry to 1 and annul it,
*  so that it is removed. Retain the DCB slot number for use later.
                  DCB_REFCT( IDCBA ) = 1
                  TEMP = IDCBA
                  CALL NDF1_DANL( .FALSE., TEMP, STATUS )

*  Loop through all the entries in the ACB to make adjustments to any
*  which referred to the DCB entry which has just been removed.
                  NEXT = 0
                  IACBT = 0
 4                CONTINUE       ! Start of 'DO WHILE' loop
                  CALL NDF1_NXTSL( NDF__ACB, IACBT, NEXT, STATUS )
                  IF ( ( STATUS .EQ. SAI__OK ) .AND.
     :                 ( NEXT .NE. 0 ) ) THEN
                     IACBT = NEXT

*  Any ACB entries which point to the annulled DCB entry are changed to
*  point to the one which was kept instead.
                     IF ( ACB_IDCB( IACBT ) .EQ. IDCBA ) THEN
                        ACB_IDCB( IACBT ) = IDCBK
                     END IF
                     GO TO 4
                  END IF
               END IF
            END IF
         END IF
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_IMP', STATUS )

      END
