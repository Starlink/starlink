      SUBROUTINE SPD_CZPD( INFO, VARUSE, GID, NMAX, N,
     :   NDF, AXIS, EOF, STATUS )
*+
*  Name:
*     SPD_CZPD

*  Purpose:
*     Access next valid NDF for RESAMP.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPD_CZPD( INFO, VARUSE, GID, NMAX, N, NDF, AXIS, EOF,
*        STATUS )

*  Description:
*     This routine uses a group of NDFs and accesses the next valid NDF.
*     It is required that the NDF be one-dimensional. Optionally it may
*     be required that the NDF also have a variance component.
*
*     On failure to find a required variance component or if the NDF
*     accessed is not one-dimensional, this routine issues a warning
*     message and tries the next NDF. When the group of NDFs is
*     exhausted, EOF is set true and control returned to the calling
*     routine. All these are no error conditions and STATUS is not
*     modified. Mostly a non-OK STATUS returned from lower levels is
*     annulled by this routine.
*
*     Apart from the main purpose of accessing the next NDF from the
*     group, this routine will establish and annul the group when N = 1
*     and N = -1 respectively.
*
*     This routine returns an NDF identifier. After use
*     the calling routine must annul the NDF identifier.

*  Arguments:
*     INFO = LOGICAL (Given)
*        False if warning messages to be suppressed.
*     VARUSE = LOGICAL (Given)
*        True if existence of variance component in the NDF is required.
*     GID = INTEGER (Given and Returned)
*        The identifier of the NDF group.
*     NMAX = INTEGER (Given and Returned)
*        The size of the NDF group, i.e. how many NDFs are in the group.
*        NMAX is used only if the IRG routines are used.
*     N = INTEGER (Given and Returned)
*        The index within the group, i.e. which NDF from the group is to
*        be accessed. If the given index does not yield a valid NDF,
*        then this routine will increment the index until a valid NDF is
*        found or the group is exhausted. Thus, on entry N is the next
*        NDF to be tried, on exit N is the valid NDF the identifier of
*        which is returned.
*     NDF = INTEGER (Returned)
*        The identifier of the successfully accessed NDF. The value is
*        unpredictable if no NDF was accessed, i.e. if EOF is true.
*     AXIS = INTEGER (Returned)
*        The number of the non-degenerate axis in the NDF. The NDF is
*        required to be one-dimensional. But if it is a section from a
*        higher-dimensional NDF then all axes are retained, except
*        that the dimensions will be 1. It is vital to know which out of
*        several axes is the one with more than one pixel.
*     EOF = LOGICAL (Returned)
*        True if the list of NDF specifications has been exhausted.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     acc: Anne Charles (RAL, Starlink)
*     {enter_new_authors_here}

*  History:
*     14 Feb 1992 (hme):
*        Original version.
*     28 Apr 1992 (hme):
*        Switch to David Berry's IRG to handle a group of NDFs.
*     26 Jan 1995 (hme):
*        Renamed from SPAAA.
*     16 Nov 1995 (hme):
*        Try to use GRP/NDF instead of IRG/IRH.
*     15 Oct 1997 (acc):
*        Change name RESAMPLE to RESAMP due to clash of names with FIGARO.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! Standard DAT constants
      INCLUDE 'PAR_ERR'          ! Standard PAR error codes
      INCLUDE 'NDF_PAR'          ! Standard NDF constants
      INCLUDE 'GRP_PAR'          ! Standard GRP constants

*  Arguments Given:
      LOGICAL INFO
      LOGICAL VARUSE

*  Arguments Given and Returned:
      INTEGER GID
      INTEGER NMAX
      INTEGER N

*  Arguments Returned:
      INTEGER NDF
      INTEGER AXIS
      LOGICAL EOF

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      LOGICAL VALID              ! True if valid NDF accessed
      LOGICAL VEXIST             ! True if NDF has variance component
      LOGICAL TERM               ! Unused
      INTEGER NADD               ! Unused
      INTEGER I                  ! Loop index
      INTEGER PLACE              ! NDF placeholder
      INTEGER NDIM               ! Number of NDF dimensions found
      INTEGER ACTDIM             ! Number of non-1 dimensions
      INTEGER DIM( NDF__MXDIM )  ! Size of NDF dimenstions found
      CHARACTER * ( 255 ) NDFNAM( 1 ) ! Name of current NDF

*.

*  Check inherited global status.
*  If N = -1, then an IRH_ANNUL is requested and should be tried
*  regardless of the inherited status.
      IF ( STATUS .NE. SAI__OK .AND. N .NE. -1 ) RETURN
      IF ( N .EQ. -1 ) THEN
         CALL GRP_DELET( GID, STATUS )
         RETURN
      END IF

*  If looking for first group element, then get the group of input NDFs.
      IF ( N .EQ. 1 ) THEN
         CALL GRP_NEW( 'InputNDFs', GID, STATUS )
         CALL GRP_GROUP( 'INLIST', GRP__NOID, GID,
     :      NMAX, NADD, TERM, STATUS )
         IF ( STATUS .NE. SAI__OK ) GO TO 500
      END IF

*  While next valid NDF not accessed.
      EOF = .FALSE.
      VALID = .FALSE.
 1    CONTINUE                ! Start of 'DO WHILE' loop
      IF ( .NOT. ( VALID .OR. EOF ) ) THEN

*     Check if group exhausted.
         IF ( N .GT. NMAX ) THEN
            EOF = .TRUE.
            GO TO 1
         END IF

*     Access n-th NDF in group.
         CALL GRP_GET( GID, N, 1, NDFNAM, STATUS )
         CALL NDF_OPEN( DAT__ROOT, NDFNAM, 'READ', 'OLD',
     :      NDF, PLACE, STATUS )

*     Check status. If failure, try next in group.
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERR_ANNUL( STATUS )
            IF ( INFO ) CALL MSG_OUT( 'SPD_CZPD_NONDF',
     :         'Failed to find a specified NDF.', STATUS )
            N = N + 1
            GO TO 1
         END IF

*     See if NDF is one-dimensional.
         CALL NDF_DIM( NDF, NDF__MXDIM, DIM, NDIM, STATUS )
         ACTDIM = 0
         DO 2 I = 1, NDIM
            IF ( DIM(I) .GT. 1 ) THEN
               ACTDIM = ACTDIM + 1
               AXIS = I
            END IF
 2       CONTINUE

*     If failure or not 1-D, try next in group.
         IF ( STATUS .NE. SAI__OK .OR. ACTDIM .NE. 1 ) THEN
            CALL NDF_ANNUL( NDF, STATUS )
            CALL ERR_ANNUL( STATUS )
            IF ( INFO ) CALL MSG_OUT( 'SPD_CZPD_NOT1D',
     :         'A specified NDF is not one-dimensional.',
     :         STATUS )
            N = N + 1
            GO TO 1
         END IF

*     If required, see if variance component exists.
*     On failure, try next in group.
         IF ( VARUSE ) THEN
            CALL NDF_STATE( NDF, 'VARIANCE', VEXIST, STATUS )
            IF ( STATUS .NE. SAI__OK .OR. .NOT. VEXIST ) THEN
               CALL NDF_ANNUL( NDF, STATUS )
               CALL ERR_ANNUL( STATUS )
               IF ( INFO ) CALL MSG_OUT( 'SPD_CZPD_NOVAR',
     :            'Failed to find variance component in ' //
     :            'a specified NDF.',
     :            STATUS )
               N = N + 1
               GO TO 1
            END IF
         END IF

*     If access to NDF was successful, escape from loop.
         VALID = .TRUE.
         GO TO 1
      END IF                     ! End of 'DO WHILE' loop

*  Return.
 500  CONTINUE
      END
