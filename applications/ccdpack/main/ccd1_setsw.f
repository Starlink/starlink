      SUBROUTINE CCD1_SETSW( NDFGRP, NNDF, USESET, ISET, NSET, IMEM,
     :                       IMEMOF, NAMGRP, MAPSET, STATUS )
*+
*  Name:
*     CCD1_SETSW

*  Purpose:
*     Get Set information about NDFs.

*  Language:
*     Starlink Fortran 77.

*  Invocation:
*     CALL CCD1_SETSW( NDFGRP, NNDF, USESET, ISET, NSET, IMEM, IMEMOF,
*                      NAMGRP, MAPSET, STATUS )

*  Description:
*     This routine returns information about how a group of NDFs is
*     grouped into Sets.
*
*     The NSET argument gives the number of distinct Sets in the input
*     group of NDFs and ISET indicates which Set each one is a member of.
*     NDFs are considered to be in the same Set for these
*     purposes if (1) USESET is true, and the NDFs have (2) the same
*     non-blank Set Name attribute and (3) have a CCD_SET frame in their
*     WCS framesets.
*
*     If USESET is false or for other reasons none of the input
*     NDFs are considered to share Sets, these returned parameters
*     will be consistent with one NDF per Set.
*
*     Some informative output about the Set groupings is written via the
*     CCDPACK logging system.

*  Arguments:
*     NDFGRP = INTEGER (Given)
*        The GRP identifier of a group giving the NDF names.
*     NNDF = INTEGER (Given)
*        The number of elements in the NDFGRP group.
*     USESET = LOGICAL (Given and Returned)
*        Whether Set-related information is being used.  If it is
*        initially set true but all the NDFs turn out to be
*        effectively members of distinct Sets then USESET will be
*        set to false on exit.
*     ISET( NNDF ) = INTEGER (Returned)
*        For each of the NDFs in NDFGRP, the number of the Set of
*        which that NDF has become a member.  If USESET is false, this
*        will be returned as 1,2,3,4,...
*     NSET = INTEGER (Returned)
*        The number of Sets constructed from the NDFGRP group.
*        If USESET is false this will be returned equal to NNDF.
*     IMEM( NNDF ) = INTEGER (Returned)
*        This array contains the numbers 1..NNDF sorted into Set order,
*        i.e. all the ones from the first Set are first, then all the
*        ones from the second Set...  If USESET is false, it will be
*        returned as 1, 2, 3,....  You can index into it using IMEMOF.
*        Taken together, IMEM and IMEMOF are a sort of inversion of ISET.
*     IMEMOF( NNDF + 1 ) = INTEGER (Returned)
*        The first NSET + 1 elements of this array are pointers into
*        the IMEM array by set number.  Thus elements IMEMOF( I ) ..
*        IMEMOF( I + 1 ) - 1 inclusive are the set numbers of the
*        NDFs comprising Set I.  If USESET is false, it will be returned
*        as 1, 2, 3, .. NSET + 1.
*        Taken together, IMEM and IMEMOF are a sort of inversion of ISET.
*        Note that this array should be declared with at least NSET + 1
*        elements; the final element is a sentry value.
*     NAMGRP = INTEGER (Returned)
*        A GRP identifier for the group of Set Names; the Ith member
*        is the Set Name attribute common to NDFs with ISET = I.
*        If there is no Set Name (e.g. if USESET is false) then the
*        NDF name will be used instead.  This group is created by
*        this routine and should be annulled by the calling routine.
*        If USESET is false this will be returned equal to GRP__NOID.
*     MAPSET( NNDF ) = INTEGER (Returned)
*        Workspace.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2001 Central Laboratory of the Research Councils

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     MBT: Mark Taylor (STARLINK)
*     {enter_new_authors_here}

*  History:
*     7-MAR-2001 (MBT):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! Standard AST constants
      INCLUDE 'GRP_PAR'          ! Standard GRP constants
      INCLUDE 'CCD1_PAR'         ! Private CCDPACK constants

*  Arguments Given:
      INTEGER NDFGRP
      INTEGER NNDF

*  Arguments Given and Returned:
      LOGICAL USESET

*  Arguments Returned:
      INTEGER ISET( * )
      INTEGER NSET
      INTEGER IMEM( * )
      INTEGER IMEMOF( * )
      INTEGER MAPSET( * )
      INTEGER NAMGRP

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Used length of character string
      EXTERNAL CHR_LEN

*  Local Variables:
      CHARACTER * ( CCD1__BLEN ) LINE ! Line buffer for writing out text
      CHARACTER * ( CCD1__BLEN ) LINE1 ! Line buffer for writing out text
      CHARACTER * ( CCD1__BLEN ) LINE2 ! Line buffer for writing out text
      CHARACTER * ( AST__SZCHR ) DMN ! Current domain of NDF
      CHARACTER * ( AST__SZCHR ) DMN1 ! Current domain of first NDF
      CHARACTER * ( GRP__SZNAM ) NDFNAM ! Name of an NDF
      CHARACTER * ( GRP__SZNAM ) SNAME ! Set Name attribute
      INTEGER I                 ! Loop index
      INTEGER IAT               ! Position in string
      INTEGER IGOT              ! Index of name found in group
      INTEGER INDF              ! NDF identifier
      INTEGER IWCS              ! AST pointer to WCS frameset
      INTEGER J                 ! Loop index
      INTEGER K                 ! Counter
      INTEGER JSET              ! Frame index of CCD_SET-domain frame
      INTEGER MAP1              ! AST pointer to mapping
      INTEGER MAP2              ! AST pointer to mapping
      INTEGER MAPS              ! AST pointer to Set mapping
      INTEGER NIN               ! Number of members in the Set
      INTEGER SINDEX            ! Set Index attribute
      DOUBLE PRECISION DIFF     ! Difference bewtween points
      DOUBLE PRECISION PSIZE    ! Size of a pixel in CCD_SET coords
      DOUBLE PRECISION XP( 2 )  ! Input transform X coordinates
      DOUBLE PRECISION XQ( 2 )  ! Output transform X coordinates
      DOUBLE PRECISION YP( 2 )  ! Input transform Y coordinates
      DOUBLE PRECISION YQ( 2 )  ! Output transform Y coordinates
      LOGICAL DIFDMN            ! True if different domains have been used

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Start an AST context.
      CALL AST_BEGIN( STATUS )

*  If we are using Sets, analyse Set membership of all NDFs.
      NAMGRP = GRP__NOID
      IF ( USESET ) THEN

*  Write a header to the user.
         CALL CCD1_MSG( ' ', ' ', STATUS )
         LINE1 = '    NDF name'
         LINE2 = '    --------'
         LINE1( 40: ) = 'Current domain'
         LINE2( 40: ) = '--------------'
         LINE1( 60: ) = 'Set Name attribute'
         LINE2( 60: ) = '------------------'
         CALL CCD1_MSG( ' ', LINE1, STATUS )
         CALL CCD1_MSG( ' ', LINE2, STATUS )

*  Initialise a group of Set Name attributes.
         NSET = 0
         CALL GRP_NEW( 'CCD:NAMES', NAMGRP, STATUS )

*  Initialise different domain flag.
         DIFDMN = .FALSE.

*  Loop over NDFs to acquire coordinate and Set information.
         DO I = 1, NNDF

*  Get the NDF identifier and WCS frameset.
            CALL NDG_NDFAS( NDFGRP, I, 'READ', INDF, STATUS )
            CALL CCD1_GTWCS( INDF, IWCS, STATUS )

*  Get the Current domain name and check for consistency.
            DMN = AST_GETC( IWCS, 'Domain', STATUS )
            IF ( I .EQ. 1 ) THEN
               DMN1 = DMN
            ELSE IF ( DMN .NE. DMN1 ) THEN
               DIFDMN = .TRUE.
            END IF

*  Get Set header information.
            CALL CCD1_SETRD( INDF, IWCS, SNAME, SINDEX, JSET, STATUS )

*  See if this NDF is, for alignment purposes, a member of a Set.
            IF ( JSET .EQ. AST__NOFRAME ) SNAME = ' '

*  If it is not a Set member, we will have to create a new Set containing
*  only this NDF.
            IF ( SNAME .EQ. ' ' ) THEN
               IGOT = 0

*  Otherwise, see if we have already started this Set.
            ELSE
               CALL GRP_INDEX( SNAME, NAMGRP, 1, IGOT, STATUS )
            END IF

*  If this is a Set member, store a mapping from CCD_SET frame to
*  the Current.  Otherwise store a null value.
            IF ( SNAME .EQ. ' ' ) THEN
               MAPS = AST__NULL
            ELSE
               MAP1 = AST_GETMAPPING( IWCS, JSET, AST__CURRENT, STATUS )
               MAPS = AST_SIMPLIFY( MAP1, STATUS )
            END IF

*  If it is a Set member and not the first encountered in that Set,
*  check that the WCS alignment of the Current frame is consistent
*  with that of the first Set member.  We do this by slapping the
*  CCD_SET -> current mapping of the one back-to-back with the
*  current -> CCD_SET mapping of the other, and seeing if they
*  cancel out to form a UnitMap.
            IF ( SNAME .NE. ' ' .AND. IGOT .GT. 0 ) THEN
               IF ( MAPSET( IGOT ) .NE. AST__NULL ) THEN

*  Combine the two mappings back to back.
                  CALL AST_INVERT( MAPS, STATUS )
                  MAP1 = AST_CMPMAP( MAPS, MAPSET( IGOT ), .TRUE., ' ',
     :                               STATUS )
                  MAP2 = AST_SIMPLIFY( MAP1, STATUS )

*  Check to see whether this is a UnitMap.
                  IF ( .NOT. AST_ISAUNITMAP( MAP2, STATUS ) ) THEN

*  It's not actually a UnitMap; is it very nearly a unit mapping?
*  First find the size of an NDF pixel in the CCD_SET frame.
                     CALL CCD1_PSIZE( IWCS, JSET, PSIZE, STATUS )

*  Now transform two points near the origin using the maybe-unit mapping.
*  It's just possible that this region of the coordinate space is
*  illegal, in which case we'll get a spurious pass, but this seems
*  very unlikely.
                     XP( 1 ) = 0D0
                     XP( 2 ) = PSIZE
                     YP( 1 ) = 0D0
                     YP( 2 ) = PSIZE
                     IF ( STATUS .EQ. SAI__OK ) THEN
                        CALL ERR_MARK()
                        CALL AST_TRAN2( MAP2, 2, XP, YP, .TRUE., XQ, YQ,
     :                                  STATUS )

*  See how far it falls from its original position.
                        IF ( STATUS .EQ. SAI__OK ) THEN
                           DIFF = SQRT( ( XQ( 1 ) - XP( 1 ) ) ** 2 +
     :                                  ( YQ( 1 ) - YP( 1 ) ) ** 2 +
     :                                  ( XQ( 2 ) - XP( 2 ) ) ** 2 +
     :                                  ( YQ( 2 ) - YP( 2 ) ) ** 2 )
                        ELSE
                           DIFF = 0D0
                           CALL ERR_ANNUL( STATUS )
                        END IF
                        CALL ERR_RLSE()
                     END IF

*  If it's further away than very close, log this as an inconsistent Set.
                     IF ( DIFF .GT. PSIZE * 1D-6 ) THEN
                        MAPSET( IGOT ) = AST__NULL
                     END IF
                  END IF

*  Tidy up.
                  CALL AST_INVERT( MAPS, STATUS )
               END IF
            END IF

*  If we have already started this Set, just note which one this NDF
*  belongs to.
            IF ( IGOT .GT. 0 ) THEN
               ISET( I ) = IGOT

*  If for whatever reason this is not part of an existing Set,
*  create a new one and record that this NDF belongs to it.
            ELSE
               NSET = NSET + 1
               CALL GRP_PUT( NAMGRP, 1, SNAME, NSET, STATUS )
               MAPSET( NSET ) = MAPS
               ISET( I ) = NSET
            END IF

*  Write a message about the NDF name, WCS and Set information.
            CALL GRP_GET( NDFGRP, I, 1, NDFNAM, STATUS )
            CALL MSG_SETC( 'NAME', NDFNAM )
            CALL MSG_SETI( 'N', I )
            CALL MSG_LOAD( ' ', '  ^N) ^NAME', LINE, IAT, STATUS )
            LINE( MAX( 45, IAT + 2 ): ) = DMN
            LINE( MAX( 60, CHR_LEN( LINE ) + 2 ): ) = SNAME
            CALL CCD1_MSG( ' ', LINE, STATUS )

*  Release resources.
            CALL NDF_ANNUL( INDF, STATUS )
         END DO

*  Warn the user if not all the domains were the same.
         IF ( DIFDMN ) THEN
            CALL CCD1_MSG( ' ', ' ', STATUS )
            CALL CCD1_MSG( ' ',
     :   '  ** Warning: not all the NDFs have the same Current domain.',
     :      STATUS )
         END IF

*  Fill in the Set names of any nameless Sets using the names of the
*  NDFs which constitute them.
         DO I = 1, NNDF
            CALL GRP_GET( NAMGRP, ISET( I ), 1, NDFNAM, STATUS )
            IF ( NDFNAM .EQ. ' ' ) THEN
               CALL GRP_GET( NDFGRP, I, 1, NDFNAM, STATUS )
               CALL GRP_PUT( NAMGRP, 1, NDFNAM, ISET( I ), STATUS )
            END IF
         END DO

*  If we effectively had no Set information, record this fact.
         IF ( NSET .EQ. NNDF ) USESET = .FALSE.

*  No Sets - store trivial Set membership information.
      ELSE
         NSET = NNDF
         DO I = 1, NNDF
            ISET( I ) = I
         END DO
         CALL GRP_COPY( NDFGRP, 1, NNDF, .FALSE., NAMGRP, STATUS )
      END IF

*  Construct a pair of arrays to record which NDFs are members of which
*  Sets.
      K = 1
      DO I = 1, NSET
         IMEMOF( I ) = K
         DO J = 1, NNDF
            IF ( ISET( J ) .EQ. I ) THEN
               IMEM( K ) = J
               K = K + 1
            END IF
         END DO
      END DO
      IMEMOF( NSET + 1 ) = NNDF + 1

*  Summarise Set grouping.
      CALL CCD1_MSG( ' ', ' ', STATUS )
      IF ( USESET ) THEN
         CALL MSG_SETC( 'TYP', 'Sets' )
      ELSE
         CALL MSG_SETC( 'TYP', 'NDFs' )
      END IF
      CALL CCD1_MSG( ' ', '    Input ^TYP:', STATUS )
      CALL CCD1_MSG( ' ', '    -----------', STATUS )
      DO I = 1, NSET
         IF ( USESET ) CALL CCD1_MSG( ' ', ' ', STATUS )
         NIN = 0
         CALL MSG_SETI( 'N', I )
         CALL MSG_LOAD( ' ', '  ^N)', LINE, IAT, STATUS )
         DO J = IMEMOF( I ), IMEMOF( I + 1 ) - 1
            NIN = NIN + 1
            CALL GRP_GET( NDFGRP, IMEM( J ), 1, LINE( IAT + 2: ),
     :                    STATUS )
            CALL CCD1_MSG( ' ', LINE, STATUS )
            LINE = ' '
         END DO

*  If the CCD_SET->Current mappings have been inconsistent, warn
*  the user of this fact.
         IF ( NIN .GT. 1 .AND. MAPSET( I ) .EQ. AST__NULL ) THEN
            CALL MSG_SETI( 'I', I )
            CALL CCD1_MSG( ' ', '  ** Warning: No consistent'//
     :' CCD_SET -> Current coordinate mapping in Set ^I.', STATUS )
            CALL CCD1_MSG( ' ', '  **          This will'//
     :' almost certainly result in errors.', STATUS )
         END IF
      END DO

*  Exit AST context.
      CALL AST_END( STATUS )

      END
* $Id$
