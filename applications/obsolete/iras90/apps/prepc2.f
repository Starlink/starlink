      SUBROUTINE PREPC2( PARAM, IGRP, SIZE, STATUS )
*+
*  Name:
*     PREPC2

*  Purpose:
*     Sort input NDFs into data and noise grids.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL PREPC2( PARAM, IGRP, SIZE, STATUS )

*  Description:
*     The input NDFs are examined to see if there are any pairs of
*     associated data and noise grids. The user has an option of
*     choosing to store the data and noise grids in separate output
*     NDFs, or to store them in the DATA and VARIANCE components of the
*     same output NDF. This option is selected using the parameter
*     specified by PARAM. If such pairs are to be stored in the same
*     output NDF, then the noise grid NDF is removed from the group
*     identified by IGRP(1) and placed in the group identified by
*     IGRP(7) at the same index as the corresponding data grid NDF.
*     The items stored in the other groups which were associated with
*     the noise grid (title, label, etc) are discarded. Doing this
*     introduces a gap in group IGRP(1). Such gaps are removed before
*     the group is returned by shuffling the entries to lower indices.
*
*     At the moment, noise grids are only available for PO and YORIC
*     images. PO noise and data grids are considered to be associated if
*     they have the same waveband index, grid number and units. YORIC
*     images are associated if they have the same waveband index, units,
*     OBJECT keyword value, DATE keyword value and ITERNO keyword value.

*  Arguments:
*     PARAM = CHARACTER (Given)
*        The parameter to use to see if noise grids should be stored
*        within the VARIANCE components of the output NDFs holding the
*        corresponding data grids.
*     ( 7 ) = INTEGER (Given and Returned)
*        The GRP identifiers for groups holding information about the
*        input NDFs. Group 1 holds the input data grids, 2 holds output
*        NDF names, 3 holds output titles, 4 holds output labels, 5
*        holds output field longitudes and 6 holds output field
*        latitudes, 7 holds input noise grids.
*     SIZE = INTEGER (Given and Returned)
*        The number of entries in groups 1 to 7. The returned value
*        will be smaller than the supplied value if any NDFs are
*        transferred from group 1 to group 7.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     14-DEC-1992 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'GRP_PAR'          ! GRP_ constants.
      INCLUDE 'MSG_PAR'          ! MSG_ constants.

*  Arguments Given:
      CHARACTER PARAM*(*)

*  Arguments Given and Returned:
      INTEGER IGRP( 7 )
      INTEGER SIZE

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER NITEM              ! Max. no. of items of information
                                 ! stored about each image.
      PARAMETER( NITEM = 7 )

      INTEGER LNITEM             ! Max. no. of characters per item.
      PARAMETER( LNITEM = 30 )

*  Local Variables:
      CHARACTER FLDLAT*(GRP__SZNAM)! Sky latitude of a reference point
                                  ! within the current NDF.
      CHARACTER FLDLON*(GRP__SZNAM)! Sky longitude of a reference point
                                  ! within the current NDF.
      CHARACTER INFO( NITEM )*(LNITEM)! Secondary workspace for PREPC4.
      CHARACTER LABEL*(GRP__SZNAM)! Title for the output NDF.
      CHARACTER NAME1*(GRP__SZNAM)! Data map NDF name.
      CHARACTER NAME2*(GRP__SZNAM)! Noise map NDF name.
      CHARACTER NDFOUT*(GRP__SZNAM)! Requested name for the output NDF.
      CHARACTER TITLE*(GRP__SZNAM)! Title for the output NDF.


      INTEGER I                  ! Loop counter.
      INTEGER IDATA              ! Index of the data map within a pair
                                 ! of associated noise and data maps.
      INTEGER INDF               ! NDF identifier.
      INTEGER INOISE             ! Index of the noise map within a pair
                                 ! of associated noise and data maps.
      INTEGER IPWORK             ! Pointer to main workspace.
      INTEGER JGRP(7)            ! Identifiers for the returned groups.
      INTEGER NNOISE             ! No. of noise map names found.

      LOGICAL PAIR               ! True if the current NDF is a member
                                 ! of a pair of data and noise maps.
      LOGICAL VAROUT             ! True if output NDFs are to have
                                 ! VARIANCE arrays.

*.

*  Ensure a null identifier gets returned if an error exists on entry.
      IGRP( 7 ) = GRP__NOID

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  See if variance components are to be created.
      CALL PAR_GET0L( PARAM, VAROUT, STATUS )

*  If not, return with an invalid group identifier.
      IF( .NOT. VAROUT .OR. STATUS .NE. SAI__OK ) GO TO 999

*  Get workspace to hold character information describing each input
*  NDF.
      CALL PSX_MALLOC( NITEM*SIZE*LNITEM, IPWORK, STATUS )

*  Convert the pointer to the array into a pointer to a character
*  descriptor (on UNIX machines IRM_CDESC leaves it as a pointer to the
*  array).
      CALL IRM_CDESC( LNITEM, IPWORK, STATUS )

*  Initialise the number of noise maps found in the supplied group to
*  zero.
      NNOISE = 0

*  Abort if an error has occurred.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Loop round each of the NDFs in the supplied group.
      DO I = 1, SIZE

*  Get an NDF identifier for the next input NDF.
         CALL NDG_NDFAS( IGRP( 1 ), I, 'READ', INDF, STATUS )

*  Store information which can be used to identify matching pairs of
*  noise and data maps in the workspace. The information is stored in
*  row I of the array. The first element of each row is NOISE or DATA
*  and indicates if the NDF is a noise or data map. The other elements
*  depend on the type of input image.
         CALL PREPC3( INDF, I, NITEM, SIZE, %VAL( IPWORK ), NNOISE,
     :                STATUS, %VAL( LNITEM ) )

*  Annul the NDF identifier.
         CALL NDF_ANNUL( INDF, STATUS )

*  If an error has occurred, annul it.
         IF( STATUS .NE. SAI__OK ) CALL ERR_ANNUL( STATUS )

      END DO

*  If any noise maps were found...
      IF( NNOISE .GT. 0 ) THEN

*  Create a new set of groups corresponding to the input groups. The
*  new groups will contain an entry for each input NDF which is to be
*  used as the basis of an output DATA array. The names of any input
*  maps which are destined for output VARIANCE arrays are removed from
*  group 1 and stored in group 7 at the index of the corresponding DATA
*  map.
         CALL GRP_NEW( 'Data maps', JGRP( 1 ), STATUS )
         CALL GRP_NEW( 'Output NDFs', JGRP( 2 ), STATUS )
         CALL GRP_NEW( 'Titles', JGRP( 3 ), STATUS )
         CALL GRP_NEW( 'Labels', JGRP( 4 ), STATUS )
         CALL GRP_NEW( 'Longitudes', JGRP( 5 ), STATUS )
         CALL GRP_NEW( 'Latitudes', JGRP( 6 ), STATUS )
         CALL GRP_NEW( 'Noise maps', JGRP( 7 ), STATUS )

*  Give an introductory message for the displayed file name pairs.
         CALL MSG_BLANKIF( MSG__NORM, STATUS )
         CALL MSG_OUTIF( MSG__NORM, 'PREPC2_MSG1',
     : '  The following pairs of data and noise maps will be stored '//
     : 'together:', STATUS )

*  Initialise NAME2 to indicate that no pairs have yet been found.
         NAME2 = ' '

*  Loop round each of the NDFs in the input group.
         DO I = 1, SIZE

*  See if this NDF is a member of a pair of noise/data maps. If so, the
*  index of both members of the pair (within the supplied group) are
*  returned and the information for the two NDFs is erased from the
*  workspace (this stops the pair from being added again when the the
*  variable I points to the other member of the pair). If this is not
*  part of a pair, the information for the current NDF is still erased
*  from the workspace.
            CALL PREPC4( I, NITEM, SIZE, %VAL( IPWORK ), PAIR, INOISE,
     :                   IDATA, INFO, STATUS, %VAL( LNITEM ) )

*  If this NDF has been erased from the workspace, pass on to the next
*  NDF.
            IF( IDATA .NE. 0 ) THEN

*  Add the name of the DATA NDF to the end of the group identified
*  by JGRP(1).
               CALL GRP_GET( IGRP(1), IDATA, 1, NAME1, STATUS )
               CALL GRP_PUT( JGRP(1), 1, NAME1, 0, STATUS )

*  Get other items associated with the DATA NDF.
               CALL PREPA2( IGRP, IDATA, NDFOUT, TITLE, LABEL, FLDLON,
     :                      FLDLAT, STATUS)

*  Append these to the end of the appropriate groups.
               CALL GRP_PUT( JGRP(2), 1, NDFOUT, 0, STATUS )
               CALL GRP_PUT( JGRP(3), 1, TITLE, 0, STATUS )
               CALL GRP_PUT( JGRP(4), 1, LABEL, 0, STATUS )
               CALL GRP_PUT( JGRP(5), 1, FLDLON, 0, STATUS )
               CALL GRP_PUT( JGRP(6), 1, FLDLAT, 0, STATUS )

*  If this NDF is a member of a pair, append the name of the noise map
*  to the end of the group identified by JGRP(7)
               IF( PAIR ) THEN
                  CALL GRP_GET( IGRP(1), INOISE, 1, NAME2, STATUS )
                  CALL GRP_PUT( JGRP(7), 1, NAME2, 0, STATUS )

*  Tell the user the names of the associatied NDFs.
                  CALL MSG_SETC( 'D', NAME1 )
                  CALL MSG_OUTIF( MSG__NORM, 'PREPC2_MSG2',
     :                            '    ^D (data map)', STATUS )
                  CALL MSG_SETC( 'N', NAME2 )
                  CALL MSG_OUTIF( MSG__NORM, 'PREPC2_MSG3',
     :                            '    ^N (noise map)', STATUS )
                  CALL MSG_BLANKIF( MSG__NORM, STATUS )

*  If this NDF was not part of a pair, store a blank name for the noise
*  map.
               ELSE
                  CALL GRP_PUT( JGRP(7), 1, ' ', 0, STATUS )

               END IF

            END IF

         END DO

*  If no pairs have been found, add an explanatory message, and delete
*  the "J" groups.
         IF( NAME2 .EQ. ' ' ) THEN
            CALL MSG_OUTIF( MSG__NORM, 'PREPC2_MSG4',
     :                      '    (no pairs found)', STATUS )
            CALL MSG_BLANKIF( MSG__NORM, STATUS )

            DO I = 1, 7
               CALL GRP_DELET( JGRP( I ), STATUS )
            END DO

*  If any pairs were found...
         ELSE

*  ...delete the supplied groups, and return the identifiers of the "J"
*  groups.
            DO I = 1, 6
               IF( IGRP( I ) .NE. GRP__NOID ) THEN
                  CALL GRP_DELET( IGRP( I ), STATUS )
                  IGRP( I ) = JGRP( I )

*  If the group was not supplied, delete the corresponding "J" group.
               ELSE
                  CALL GRP_DELET( JGRP( I ), STATUS )
               END IF

            END DO

*  Return the identifier of the noise grids group.
            IGRP( 7 ) = JGRP( 7 )

*  Return the reduced size of the data map group.
            CALL GRP_GRPSZ( IGRP( 1 ), SIZE, STATUS )

         END IF

      END IF

*  Free the workspace. First convert the pointer to the character
*  descriptor back into a pointer to the character array.
      CALL IRM_CPOIN( IPWORK, STATUS )
      CALL PSX_FREE( IPWORK, STATUS )

*  Finish
 999  CONTINUE

      END
