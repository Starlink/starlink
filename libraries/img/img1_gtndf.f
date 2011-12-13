      SUBROUTINE IMG1_GTNDF( PARAM, TYPE, RONLY, MXDIM, DIM, PNTR,
     :                       STATUS )
*+
*  Name:
*     IMG1_GTNDF

*  Purpose:
*     Obtain access to NDFs for input.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IMG1_GTNDF( PARAM, TYPE, RONLY, MXDIM, DIM, PNTR, STATUS )

*  Description:
*     This routine obtains access to input NDFs, each via a parameter,
*     returning the dimension sizes of the NDFs obtained and pointers to
*     their data arrays, mapped for either READ/ZERO or UPDATE/ZERO
*     access. If the NDFs supplied have different shapes, then they are
*     first trimmed so that their bounds match (an error will result if
*     there are no pixels in common). If an NDF is already associated
*     with a parameter and is not currently mapped then it is processed
*     as a new NDF. If the NDF is already associated and is mapped, then
*     its details are simply returned (in this case only a single NDF
*     may be obtained), otherwise new NDFs are obtained.

*  Arguments:
*     PARAM = CHARACTER * ( * ) (Given)
*        Comma-separated list of parameter names (case insensitive).
*     TYPE = CHARACTER * ( * ) (Given)
*        Numeric type to be used to access the NDFs' data arrays.
*     RONLY = LOGICAL (Given)
*        TRUE if NDFs must be accessed using readonly (/ZERO). Otherwise
*        they will be accessed using UPDATE/ZERO mode.
*     MXDIM = INTEGER (Given)
*        Maximum number of significant dimensions which the NDFs may
*        have.
*     DIM( MXDIM ) = INTEGER (Returned)
*        NDF dimension sizes. If the NDFs have fewer than MXDIM actual
*        dimensions, then the remaining dimension sizes will be set to
*        unity.
*     PNTR( * ) = INTEGER (Given)
*        Pointers to the NDFs' data array values, mapped in UPDATE/ZERO
*        mode, one pointer for each NDF requested.
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
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     17-FEB-1992 (RFWS):
*        Original version.
*     18-FEB-1992 (RFWS):
*        Added extra error messages.
*     20-FEB-1992 (RFWS):
*        Re-structured test for NDF previously in use and improved the
*        cleanup procedures after errors.
*     25-FEB-1992 (RFWS):
*        Re-structured handling of the error stack.
*     27-FEB-1992 (RFWS):
*        Adapted to handle comma-separated lists of parameter names.
*     28-FEB-1992 (RFWS):
*        Changed to trim all new NDFs acquired to have the same bounds.
*     7-JUL-1994 (PDRAPER):
*        Picked up the pieces and changed so that the correct NDF
*        identifiers are used.
*     13-SEP-1994 (PDRAPER):
*        Re-incorporated checks about mixing old and new NDFs (from
*        RFWS's code).
*     15-NOV-1994 (PDRAPER):
*        Now checks mapped state of NDF and uses UPDATE/ZERO access
*        rather than READ/ZERO. These changes allow other routines which
*        do not need the mapped data to access NDFs and also allow
*        modification of existing NDFs.
*     28-NOV-1994 (PDRAPER):
*        Back-tracking to allow for NDFs in file which can only be
*        accessed for reading. Mapping now in UPDATE if possible
*        otherwise READ.
*     29-NOV-1995 (PDRAPER):
*        Backed out of UPDATE mode access. This is now controlled by
*        RONLY argument.
*     04-MAR-2003 (PDRAPER):
*        Changed to add NDF to INDF array indexed by NPAR, rather
*        than SLOT. The upshot of this was that all images, from previous
*        calls as well as this one, where trimmed to the same dimensions.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'IMG_CONST'        ! IMG_ private constants
      INCLUDE 'IMG_ERR'          ! IMG_ error codes
      INCLUDE 'DAT_PAR'          ! HDS/DAT parameters
      INCLUDE 'NDF_ERR'          ! NDF_ error codes
      INCLUDE 'NDF_PAR'          ! NDF constants

*  Global Variables:
      INCLUDE 'IMG_PCB'          ! IMG_ Parameter Control Block
*        PCB_INDF( IMG__MXPCB ) = INTEGER (Read and Write)
*           NDF identifier.
*        PCB_PNTR( IMG__MXPCB ) = INTEGER (Read and Write)
*           Pointer to mapped data.
*        PCB_TYPE( IMG__MXPAR ) = CHARACTER *( * ) (Write)
*           Record of data type used for mapping.

*  Arguments Given:
      CHARACTER * ( * ) PARAM
      CHARACTER * ( * ) TYPE
      LOGICAL RONLY
      INTEGER MXDIM

*  Arguments Returned:
      INTEGER DIM( MXDIM )
      INTEGER PNTR( * )

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL IMG1_INIT         ! Initialise common blocks

*  Local Variables:
      CHARACTER * ( IMG__SZPAR ) VPAR ! Validated parameter name
      INTEGER EL                 ! Number of array elements mapped
      INTEGER F                  ! First character position
      INTEGER I                  ! Loop counter for dimension sizes
      INTEGER I1                 ! Position of start of field
      INTEGER I2                 ! Position of end of field
      INTEGER INDF( IMG__MXPAR ) ! New NDF identifiers
      INTEGER ISLOT( IMG__MXPAR ) ! Slot numbers used
      INTEGER L                  ! Last character position
      INTEGER NDIM               ! Actual number of NDF dimensions
      INTEGER NNEW               ! Number of new NDFs acquired
      INTEGER NPAR               ! Number of non-blank parameter names
      INTEGER SLOT               ! PCB slot number for NDF
      INTEGER TPNTR( 1 )         ! Temporary variable for pointer
      LOGICAL BAD                ! Bad pixel values present?
      LOGICAL OLD( IMG__MXPAR )  ! NDF is old?
      LOGICAL WASNEW             ! New PCB slot allocated?
      LOGICAL CHKBAD( IMG__MXPAR ) ! Whether to check for BAD pixels
*.

*  Set a null value for the initial PNTR element.
      PNTR( 1 ) = IMG__NOPTR

*  Check inherited global status.
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Initialise.
         NPAR = 0
         NNEW = 0

*  Initialise the character pointer to the start of the parameter list.
*  Then loop to extract each element from the parameter list.
         I1 = 1
 2       CONTINUE                ! Start of "DO WHILE" loop
         IF ( ( STATUS .EQ. SAI__OK ) .AND.
     :        ( I1 .LE. LEN( PARAM ) ) ) THEN

*  Find the final character of the next element in the parameter list
*  (the last character before a comma or end of string).
            I2 = INDEX( PARAM( I1 : ), ',' )
            IF ( I2 .EQ. 0 ) THEN
               I2 = LEN( PARAM )
            ELSE
               I2 = I2 + I1 - 2
            END IF
            IF ( I2 .GE. I1 ) THEN

*  Locate the first and last non-blank characters in the element,
*  checking that it is not entirely blank.
               CALL CHR_FANDL( PARAM( I1 : I2 ), F, L )
               IF ( L .GE. F ) THEN
                  F = F + I1 - 1
                  L = L + I1 - 1

*  Increment the parameter count and initialise the returned pointer
*  value.
                  NPAR = NPAR + 1
                  PNTR( NPAR ) = IMG__NOPTR

*  Check for the presence of a trailing "!", which indicates that BAD
*  pixels are acceptable.
                  IF ( PARAM( L: L ) .EQ. '!' ) THEN
                     CHKBAD( NPAR ) = .FALSE.
                     L = L - 1
                  ELSE
                     CHKBAD( NPAR ) = .TRUE.
                  END IF

*  Validate the parameter name and obtain a PCB slot number for it,
*  initialising a new slot if necessary.
                  CALL IMG1_VPAR( PARAM( F :L ), VPAR, STATUS )
                  CALL IMG1_GTSLT( VPAR, .TRUE., SLOT, WASNEW, STATUS )
                  IF ( STATUS .EQ. SAI__OK ) THEN

*  If a new slot was not allocated, then the NDF is already in use.
*  Check its mapped status. If the data has already been mapped then
*  this is a pre-existing (old) NDF so obtain its dimensions for
*  checking. If the NDF hasn't been mapped then treat this as a new
*  NDF.
                     IF ( .NOT. WASNEW ) THEN

*  Is the NDF data array mapped?
                        IF ( PCB_PNTR( SLOT ) .EQ. IMG__NOPTR ) THEN

*  Data not mapped treat this as a new NDF (probably initialised by
*  another routine which didn't require the data array).
                           CALL ERR_MARK
                           CALL NDF_DIM( PCB_INDF( SLOT ), MXDIM, DIM,
     :                                   NDIM, STATUS )

*  If it has more dimensions than can be handled, then annul the error.
                           IF ( STATUS .EQ. NDF__XSDIM ) THEN
                              CALL ERR_ANNUL( STATUS )

*  Report a new one explaining the problem.
                              STATUS = NDF__XSDIM
                              CALL NDF_MSG( 'NDF', PCB_INDF( SLOT ) )
                              CALL MSG_SETI( 'MXDIM', MXDIM )
                              CALL ERR_REP( 'IMG1_GTNDF_XS2',
     :                                      'The NDF structure ^NDF ' //
     :                                      'has more than ^MXDIM ' //
     :                                      'significant dimensions.',
     :                                      STATUS )

*  Flush the error message, release the NDF, cancel the parameter
*  association and return to obtain another NDF.
                              CALL ERR_FLUSH( STATUS )
                              CALL NDF_ANNUL( PCB_INDF( SLOT ), STATUS )
                              CALL DAT_CANCL( VPAR, STATUS )
                              CALL ERR_ANNUL( STATUS )
                           END IF
                           CALL ERR_RLSE

*  Remember which PCB slot it is associated with. Also note it was a
*  new NDF and count the number of new NDFs acquired.
                           IF ( STATUS .EQ. SAI__OK ) THEN
                              ISLOT( NPAR ) = SLOT
                              OLD( NPAR ) = .FALSE.
                              INDF( NPAR ) = PCB_INDF( SLOT )
                              NNEW = NNEW + 1

*  If an error occurred need to free the PCB slot.
                           ELSE
                              CALL IMG1_FRSLT( SLOT, .TRUE., STATUS )
                           END IF
                        ELSE

*  Old NDF which is already processed.
                           OLD( NPAR ) = .TRUE.
                           CALL ERR_MARK
                           CALL NDF_DIM( PCB_INDF( SLOT ), MXDIM, DIM,
     :                                   NDIM, STATUS )

*  If it has more dimensions than can be handled, then this is because
*  it was previously accessed specifying a larger number. Annul the
*  existing error.
                           IF ( STATUS .EQ. NDF__XSDIM ) THEN
                              CALL ERR_ANNUL( STATUS )

*  Report a new one explaining the problem.
                              STATUS = NDF__XSDIM
                              CALL NDF_MSG( 'NDF', PCB_INDF( SLOT ) )
                              CALL MSG_SETI( 'MXDIM', MXDIM )
                              CALL ERR_REP( 'IMG1_GTNDF_XS1',
     :                             'The previously-accessed NDF ' //
     :                             'structure ^NDF has more than ' //
     :                             '^MXDIM significant dimensions ' //
     :                             'and cannot be used again at ' //
     :                             'this point (possible ' //
     :                             'programming error).', STATUS )
                           ELSE

*  Remember the slot no (used again later).
                              ISLOT( NPAR ) = SLOT
                           END IF
                           CALL ERR_RLSE
                        END IF

*  If a new NDF has to be acquired, then loop until it has been
*  obtained successfully. Mark the error stack first, to prevent any
*  existing contents being disturbed.
                     ELSE
                        CALL ERR_MARK
 1                      CONTINUE ! Start of 'DO WHILE' loop
                        IF ( RONLY ) THEN
                           CALL NDF_ASSOC( VPAR, 'READ', INDF( NPAR ),
     :                                     STATUS )
                        ELSE
                           CALL NDF_ASSOC( VPAR, 'UPDATE', INDF( NPAR ),
     :                                     STATUS )
                        END IF
                        IF ( STATUS .EQ. SAI__OK ) THEN

*  If an NDF has been obtained, then determine its dimension sizes.
                           CALL NDF_DIM( INDF( NPAR ), MXDIM, DIM, NDIM,
     :                                   STATUS )

*  If it has more dimensions than can be handled, then annul the error.
                           IF ( STATUS .EQ. NDF__XSDIM ) THEN
                              CALL ERR_ANNUL( STATUS )

*  Report a new one explaining the problem.
                              STATUS = NDF__XSDIM
                              CALL NDF_MSG( 'NDF', INDF( NPAR ) )
                              CALL MSG_SETI( 'MXDIM', MXDIM )
                              CALL ERR_REP( 'IMG1_GTNDF_XS2',
     :                                      'The NDF structure ^NDF ' //
     :                                      'has more than ^MXDIM ' //
     :                                      'significant dimensions.',
     :                                      STATUS )

*  Flush the error message, release the NDF, cancel the parameter
*  association and return to obtain another NDF.
                              CALL ERR_FLUSH( STATUS )
                              CALL NDF_ANNUL( INDF( NPAR ), STATUS )
                              CALL DAT_CANCL( VPAR, STATUS )
                              CALL ERR_ANNUL( STATUS )
                              GO TO 1
                           END IF
                        END IF

*  Release the error stack.
                        CALL ERR_RLSE

*  If the NDF was acquired successfully, then remember which PCB slot it
*  is associated with. Also note it was a new NDF and count the number
*  of new NDFs acquired.
                        IF ( STATUS .EQ. SAI__OK ) THEN
                           ISLOT( NPAR ) = SLOT
                           OLD( NPAR ) = .FALSE.
                           NNEW = NNEW + 1

*  If an error occurred after allocating a new PCB slot, then free the
*  slot.
                        ELSE
                           CALL IMG1_FRSLT( SLOT, .TRUE., STATUS )
                        END IF
                     END IF
                  END IF
               END IF
            END IF

*  Increment the character pointer to the start of the next element in
*  the parameter list and return to process the next element.
            I1 = I2 + 2
            GO TO 2
         END IF

*  If no error has occurred, but no non-blank parameter names have been
*  processed, then report an error.
         IF ( ( STATUS .EQ. SAI__OK ) .AND. ( NPAR .EQ. 0 ) ) THEN
            STATUS = IMG__PARIN
            CALL ERR_REP( 'IMG1_GTNDF_NOPAR',
     :                    'No parameter name specified (possible ' //
     :                    'programming error).', STATUS )
         END IF
      END IF

*  If one or more new NDFs were accessed, check that no old NDFs were
*  also accessed at the same time (this is not allowed because it may
*  imply changing the shape of NDFs already acquired). Report an error
*  if necessary.
      IF ( STATUS .EQ. SAI__OK ) THEN
         IF ( ( NNEW .GT. 0 ) .AND. ( NNEW .NE. NPAR ) ) THEN
            STATUS = IMG__MIXED
            CALL ERR_REP( 'IMG1_GTNDF_OLD',
     :                    'Access to new images cannot be combined ' //
     :                    'with access to images already acquired ' //
     :                    'in a single call (possible programming ' //
     :                    'error).', STATUS )

*  If new NDFs were accessed, then trim their bounds to match (note
*  this may modify the NDF identifiers) and obtain the new dimension
*  sizes.
         ELSE IF ( NNEW .GT. 0 ) THEN
            CALL NDF_MBNDN( 'TRIM', NNEW, INDF, STATUS )
            CALL NDF_DIM( INDF( 1 ), MXDIM, DIM, NDIM, STATUS )
         END IF
      END IF

*  Loop to obtain pointers to the data for each NDF.
      IF ( STATUS .EQ. SAI__OK ) THEN
         DO 3 I = 1, NPAR

*  For pre-existing NDFs simply obtain the pointer from the appropriate
*  PCB slot.
            IF ( OLD( I ) ) THEN
               PNTR( I ) = PCB_PNTR( ISLOT( I ) )

*  For new NDFs, insert the modified identifier back into the
*  appropriate PCB slot.
            ELSE
               PCB_INDF( ISLOT( I ) ) = INDF( I )

*  Map each NDF's data array. If RONLY then map with READ/ZERO,
*  otherwise use UPDATE/ZERO.
               IF ( RONLY ) THEN
                  CALL NDF_MAP( PCB_INDF( ISLOT( I ) ), 'Data', TYPE,
     :                          'READ/ZERO', TPNTR, EL, STATUS )
               ELSE
                  CALL NDF_MAP( PCB_INDF( ISLOT( I ) ), 'Data', TYPE,
     :                          'UPDATE/ZERO', TPNTR, EL, STATUS )
               END IF

*  Check to see if the mapped data array contains "bad" pixels.
               IF ( CHKBAD( I ) ) THEN
                  CALL NDF_BAD( PCB_INDF( ISLOT( I ) ), 'Data', .TRUE.,
     :                          BAD, STATUS )

*  If so, then issue a warning message.
                  IF ( BAD .AND. STATUS .EQ. SAI__OK ) THEN
                     CALL ERR_MARK
                     STATUS = SAI__ERROR
                     CALL NDF_MSG( 'NDF', PCB_INDF( ISLOT( I ) ) )
                     CALL ERR_REP( 'IMG1_GTNDF_BAD', 'Warning: ' //
     :                    'the NDF structure ^NDF contains "bad" ' //
     :                    '(undefined) pixel values which this ' //
     :                    'application may not be able to handle.',
     :                    STATUS )
                     CALL ERR_FLUSH( STATUS )
                     CALL ERR_RLSE
                  END IF
               END IF

*  If successful, store the pointer value in the PCB and return its
*  value.
               IF ( STATUS .EQ. SAI__OK ) THEN
                  PCB_PNTR( ISLOT( I ) ) = TPNTR( 1 )
                  PNTR( I ) = TPNTR( 1 )

*  Also store the type used to map the data.
                  PCB_TYPE( ISLOT( I ) ) = TYPE
               END IF
            END IF
 3       CONTINUE
      END IF

*  If the routine was not successful, then return dimension sizes of
*  unity.
      IF ( STATUS .NE. SAI__OK ) THEN
         DO 4 I = 1, MXDIM
            DIM( I ) = 1
 4       CONTINUE
      END IF

      END
*  $Id$
