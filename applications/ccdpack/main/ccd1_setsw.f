      SUBROUTINE CCD1_SETSW( NDFGRP, NNDF, USESET, ISET, NSET, NAMGRP,
     :                       STATUS )
*+
*  Name:
*     CCD1_SETSW

*  Purpose:
*     Get Set information about NDFs.

*  Language:
*     Starlink Fortran 77.

*  Invocation:
*     CALL CCD1_SETSW( NDFGRP, NNDF, USESET, ISET, NSET, NAMGRP,
*                      STATUS )

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
*     NAMGRP = INTEGER (Returned)
*        A GRP identifier for the group of Set Names; the Ith member
*        is the Set Name attribute common to NDFs with ISET = I.
*        If there is no Set Name (e.g. if USESET is false) then the
*        NDF name will be used instead.  This group is created by
*        this routine and should be annulled by the calling routine.
*        If USESET is false this will be returned equal to GRP__NOID.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2001 Central Laboratory of the Research Councils

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

*  Arugments Given and Returned:
      LOGICAL USESET
      
*  Arguments Returned:
      INTEGER ISET( * )
      INTEGER NSET
      
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
      INTEGER JSET              ! Frame index of CCD_SET-domain frame
      INTEGER MAP1              ! AST pointer to mapping
      INTEGER MAP2              ! AST pointer to mapping
      INTEGER MAPSET( NNDF )    ! AST pointer to CCD_SET->Current mapping
      INTEGER NAMGRP            ! GRP identifier for group of Set names
      INTEGER SINDEX            ! Set Index attribute
      LOGICAL DIFDMN            ! True if different domains have been used
      LOGICAL OK                ! CCD_SET and Current frames consistent?

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If we are using Sets, analyse Set membership of all NDFs.
      NAMGRP = GRP__NOID
      IF ( USESET ) THEN

*  Start an AST context.
         CALL AST_BEGIN( STATUS )

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
               MAPSET( I ) = AST__NULL
            ELSE
               MAP1 = AST_GETMAPPING( IWCS, JSET, AST__CURRENT, STATUS )
               MAPSET( I ) = AST_SIMPLIFY( MAP1, STATUS )
            END IF

*  If it is a Set member and not the first encountered in that Set,
*  check that the WCS alignment of the Current frame is consistent
*  with that of the first Set member.  We do this by slapping the
*  CCD_SET -> current mapping of the one back-to-back with the
*  current -> CCD_SET mapping of the other, and seeing if they
*  cancel out to form a UnitMap.
            IF ( SNAME .NE. ' ' .AND. IGOT .GT. 0 ) THEN

*  Combine the two mappings back to back.
               CALL AST_INVERT( MAPSET( I ), STATUS )
               MAP1 = AST_CMPMAP( MAPSET( I ), MAPSET( IGOT ), .TRUE.,
     :                            ' ', STATUS )
               MAP2 = AST_SIMPLIFY( MAP1, STATUS )

*  Check for UnitMap status.
               OK = AST_ISAUNITMAP( MAP2, STATUS )

*  Tidy up.
               CALL AST_INVERT( MAPSET( I ), STATUS )

*  Warn the user if there is a problem.
               IF ( .NOT. OK ) THEN
                  CALL CCD1_MSG( ' ', ' ', STATUS )
                  CALL CCD1_MSG( ' ', '  ** Warning: Current'//
     :' coordinates are not consistent with Set alignment.', STATUS )
                  CALL CCD1_MSG( ' ', '  **          This will'//
     :' almost certainly result in errors.', STATUS )
                  CALL CCD1_MSG( ' ', ' ', STATUS )
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

*  Release resources.
         CALL AST_END( STATUS )

*  No Sets - store trivial Set membership information.
      ELSE
         NSET = NNDF
         DO I = 1, NNDF
            ISET( I ) = I
         END DO
         CALL GRP_COPY( NDFGRP, 1, NNDF, .FALSE., NAMGRP, STATUS )
      END IF

*  Summarise Set grouping.
      CALL CCD1_MSG( ' ', ' ', STATUS )
      IF ( USESET ) THEN
         CALL MSG_SETC( 'TYP', 'Sets' )
      ELSE
         CALL MSG_SETC( 'TYP', 'NDFs' )
      END IF
      CALL CCD1_MSG( ' ', '    Input ^TYP:', STATUS )
      CALL CCD1_MSG( ' ', '    ------------', STATUS )
      DO I = 1, NSET
         CALL MSG_SETI( 'N', I )
         CALL MSG_LOAD( ' ', '  ^N)', LINE, IAT, STATUS )
         DO J = 1, NNDF
            IF ( ISET( J ) .EQ. I ) THEN
               CALL GRP_GET( NDFGRP, J, 1, LINE( IAT + 2: ), STATUS )
               CALL CCD1_MSG( ' ', LINE, STATUS )
               LINE = ' '
            END IF
         END DO
      END DO

      END
* $Id$
