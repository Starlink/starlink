      SUBROUTINE CCD1_SWLIS( NDFGRP, FIOGRP, NLIST, NLGRP, NNOLIS,
     :                       NDFS, USEWCS, USESET, FRMS, MAPPIX,
     :                       MAPSET, ISUP, NSUP, ILIS, ILISOF,
     :                       INLSUP, STATUS )
*+
*  Name:
*     CCD1_SWLIS

*  Purpose:
*     Get Set and WCS information about list files.

*  Language:
*     Starlink Fortran 77.

*  Invocation:
*     CALL CCD1_SWLIS( NDFGRP, FIOGRP, NLIST, NLGRP, NNOLIS, NDFS,
*                      USEWCS, USESET, FRMS, MAPPIX, MAPSET, ISUP,
*                      NSUP, ILIS, ILISOF, INLSUP, STATUS )

*  Description:
*     This routine interrogates a group of files containing position
*     lists.  It writes various informative comments via the CCDPACK
*     logging system to the user.  Additionally,
*     if USEWCS is true it retrieves certain WCS-related information,
*     and if USESET is true it retrieves certain Set-related
*     information, from the NDF with which the position list is
*     associated.  If NDFS is true, it will print a summary of
*     the NDFs from which each list has come.
*
*     The WCS-related information it returns is principally the
*     mapping between the PIXEL frame and the Current coordinate
*     frame of the NDF.
*
*     The ISUP, NSUP, ILIS and ILISOF parameters return information
*     about the mapping between each input list and superlists formed
*     by amalgamating all the lists in the same alignment Set.
*     Lists are considered to be in the same Set for alignment
*     purposes if (1) USESET is true, and they are associated
*     with NDFs which (2) have the same non-blank Set Name attribute
*     and (3) have a CCD_SET frame in their WCS framesets.
*     If USESET is false or for other reasons none of the input
*     lists are considered to share Sets, these returned parameters
*     will be consistent with one list per superlist.

*  Arguments:
*     NDFGRP = INTEGER (Given)
*        The GRP identifier of a group giving the NDFs with which
*        position lists are associated.  This will only be accessed
*        if NDFS is true.  If NDFs is true, it will have the same
*        number of members as FIOGRP.
*     FIOGRP = INTEGER (Given)
*        The GRP identifier giving the names of the lists themselves.
*     NLIST = INTEGER (Given)
*        The number of elements in the NDFGRP group.
*     NLGRP = INTEGER (Given)
*        A GRP identifier for a list of NDFs with no associated lists.
*        If NNOLIS is positive, the Set information of the NDFs in
*        this group is read to fill the INLSUP array.
*     NNOLIS = INTEGER (Given)
*        If positive, the number of members of NLGRP.
*     NDFS = LOGICAL (Given)
*        Indicates whether the NDFGRP group contains the names of NDFs
*        which reference position lists in their
*        .MORE.CCDPACK.CURRENT_LIST component or the names of position
*        lists themselves.
*     USEWCS = LOGICAL (Given)
*        Whether WCS-related information is being used.  May only be
*        true if NDFS is true.
*     USESET = LOGICAL (Given and Returned)
*        Whether Set-related information is being used.  May only be
*        true if NDFS is true.
*     FRMS( NLIST ) = INTEGER (Returned)
*        The Current frame of the WCS component of each NDF.  Only
*        returned if USEWCS is true.
*     MAPPIX( NLIST ) = INTEGER (Returned)
*        For each of the NDFs in NDFGRP, an AST mapping from the PIXEL-
*        domain frame to the Current frame of the WCS component.
*        Only returned if USEWCS is true.
*     MAPSET( NLIST ) = INTEGER (Returned)
*        If USESET is true, the first NSUP elements returned are
*        AST pointers to mappings from the CCD_SET-domain frame to the
*        working frame of the WCS component of NDFs in the corresponding
*        superlist.  The working frame is in the PIXEL domain if USEWCS
*        is false and the Current frame if USEWCS is true.
*        If there is no consistent mapping for the Set, AST__NULL is
*        returned.  This array is required as workspace.
*        Only returned if USESET is true.
*     ISUP( NLIST ) = INTEGER (Returned)
*        For each of the NDFs in NDFGRP, the number of the superlist into
*        which that list has been subsumed.  If USESET is false, this
*        will be returned as 1,2,3,4,...
*     NSUP = INTEGER (Returned)
*        The number of superlists constructed from the NDFGRP group.
*        If USESET is false this will be returned equal to NLIST.
*     ILIS( NLIST ) = INTEGER (Returned)
*        This array contains numbers 1..NLIST sorted into superlist
*        order, i.e. all the ones from the first superlist are first,
*        then all the ones from the second superlist ....  If USESET
*        is false, it will be returned as 1, 2, 3, ....  You can
*        index into it using ILISOF.
*        Taken together, ILISOF and ILIS are a sort of inversion of ISUP.
*     ILISOF( NLIST + 1 ) = INTEGER (Returned)
*        The first NSUP + 1 elements of this array are pointers into
*        the ILIS array by superlist number.  Thus, elements
*        ILISOF( I ) .. ILISOF( I + 1 ) - 1 inclusive are the list
*        numbers of the lists comprising superlist I.  If USESET
*        is false, it will be returned as 1, 2, 3, ....
*        Taken together, ILISOF and ILIS are a sort of inversion of ISUP.
*        Note this array should be declared with at least NLIST + 1
*        elements; the final element is a sentry value.
*     INLSUP( NNOLIS ) = INTEGER (Returned)
*        For each member of the NLGRP group, a value is returned which
*        gives the number of the superlist to which it corresponds
*        (i.e. is a member of the same Set as).  If USESET is false,
*        this will be filled with zeros.
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
*     22-FEB-2001 (MBT):
*        Original version.
*     27-NOV-2001 (MBT):
*        Stopped it setting USESET false if there was only one list per
*        Set.  Although the list processing is done correctly in this
*        case, the original value of USESET may be needed by the calling
*        routine for other processing.
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
      INTEGER FIOGRP
      INTEGER NLIST
      INTEGER NLGRP
      INTEGER NNOLIS
      LOGICAL NDFS
      LOGICAL USEWCS

*  Arugments Given and Returned:
      LOGICAL USESET

*  Arguments Returned:
      INTEGER FRMS( NLIST )
      INTEGER MAPPIX( NLIST )
      INTEGER MAPSET( NLIST )
      INTEGER ISUP( NLIST )
      INTEGER NSUP
      INTEGER ILIS( NLIST )
      INTEGER ILISOF( NLIST + 1 )
      INTEGER INLSUP( NNOLIS )

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
      INTEGER JPIX              ! Frame index of PIXEL-domain frame
      INTEGER JWORK             ! Symbolic frame index for working frame
      INTEGER JSET              ! Frame index of CCD_SET-domain frame
      INTEGER K                 ! Counter
      INTEGER MAP1              ! AST pointer to mapping
      INTEGER MAP2              ! AST pointer to mapping
      INTEGER MAPS              ! AST pointer to set mapping for this list
      INTEGER NAMGRP            ! GRP identifier for group of Set names
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

*  Initialise different domain flag.
      DIFDMN = .FALSE.

*  If necessary write a header to the user for the names of the
*  associated NDFs.
      IF ( NDFS ) THEN
         CALL CCD1_MSG( ' ', ' ', STATUS )
         LINE1 = '    NDFs containing position lists'
         LINE2 = '    ------------------------------'
         IF ( USEWCS ) THEN
            LINE1( 40: ) = 'Current domain'
            LINE2( 40: ) = '--------------'
         END IF
         IF ( USESET ) THEN
            LINE1( 60: ) = 'Set Name attribute'
            LINE2( 60: ) = '------------------'
         END IF
         CALL CCD1_MSG( ' ', LINE1, STATUS )
         CALL CCD1_MSG( ' ', LINE2, STATUS )
      END IF

*  Initialise a group of Set Name attributes.
      NSUP = 0
      IF ( USESET ) THEN
         CALL GRP_NEW( 'CCD:NAMES', NAMGRP, STATUS )
      ELSE
         NAMGRP = GRP__NOID
      END IF

*  Loop over position lists to acquire coordinate and Set information.
      DO I = 1, NLIST

*  Get the NDF identifier and WCS frameset if required.
         IF ( USESET .OR. USEWCS ) THEN
            CALL NDG_NDFAS( NDFGRP, I, 'READ', INDF, STATUS )
            CALL CCD1_GTWCS( INDF, IWCS, STATUS )
            CALL CCD1_FRDM( IWCS, 'Pixel', JPIX, STATUS )
         END IF

*  Get coordinate information.
         IF ( USEWCS ) THEN
            DMN = AST_GETC( IWCS, 'Domain', STATUS )
            IF ( I .EQ. 1 ) THEN
               DMN1 = DMN
            ELSE IF ( DMN .NE. DMN1 ) THEN
               DIFDMN = .TRUE.
            END IF

*  Get a mapping from the PIXEL-domain frame to the Current frame.
            MAP1 = AST_GETMAPPING( IWCS, JPIX, AST__CURRENT, STATUS )
            MAPPIX( I ) = AST_SIMPLIFY( MAP1, STATUS )
            CALL AST_ANNUL( MAP1, STATUS )

*  Get and export the Current frame.
            FRMS( I ) = AST_GETFRAME( IWCS, AST__CURRENT, STATUS )
         END IF

*  Get Set header information if required.
         IF ( USESET ) THEN
            CALL CCD1_SETRD( INDF, IWCS, SNAME, SINDEX, JSET, STATUS )

*  See if this NDF is, for alignment purposes, a member of a Set.
            IF ( JSET .EQ. AST__NOFRAME ) SNAME = ' '

*  If it is not a Set member, we will have to create a new superlist
*  containing only this list.
            IF ( SNAME .EQ. ' ' ) THEN
               IGOT = 0

*  Otherwise, see if we have already started the superlist for this Set.
            ELSE
               CALL GRP_INDEX( SNAME, NAMGRP, 1, IGOT, STATUS )
            END IF

*  If this is a Set member, store a mapping from CCD_SET frame to
*  the working (PIXEL or Current) frame.  Otherwise store a null value.
            IF ( SNAME .EQ. ' ' ) THEN
               MAPS = AST__NULL
            ELSE
               IF ( USEWCS ) THEN
                  JWORK = AST__CURRENT
               ELSE
                  JWORK = JPIX
               END IF
               MAP1 = AST_GETMAPPING( IWCS, JSET, JWORK, STATUS )
               MAPS = AST_SIMPLIFY( MAP1, STATUS )
               CALL AST_ANNUL( MAP1, STATUS )
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
     :                            STATUS )
                  MAP2 = AST_SIMPLIFY( MAP1, STATUS )

*  Check to see whether this is a UnitMap.
                  IF ( .NOT. AST_ISAUNITMAP( MAP2, STATUS ) ) THEN

*  It's not actually a UnitMap; is it very nearly a unit mapping?
*  First find the size of an NDF pixel in the CCD_SET frame.
                     CALL CCD1_PSIZE( IWCS, JWORK, PSIZE, STATUS )

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

*  If it's further away than very close, log this as an inconsistent
*  superlist.
                     IF ( DIFF .GT. PSIZE * 1D-6 ) THEN
                        MAPSET( IGOT ) = AST__NULL
                     END IF
                  END IF

*  Tidy up.
                  CALL AST_INVERT( MAPS, STATUS )
                  CALL AST_ANNUL( MAP1, STATUS )
                  CALL AST_ANNUL( MAP2, STATUS )
               END IF
            END IF

*  If we have already started this superlist, just note which one this
*  list belongs to.
            IF ( IGOT .GT. 0 ) THEN
               ISUP( I ) = IGOT

*  If for whatever reason this is not part of an existing superlist,
*  create a new superlist and record that this list belongs to it.
            ELSE
               NSUP = NSUP + 1
               CALL GRP_PUT( NAMGRP, 1, SNAME, NSUP, STATUS )
               MAPSET( NSUP ) = MAPS
               ISUP( I ) = NSUP
            END IF

*  No Sets - store default entries.
         ELSE
            ISUP( I ) = I
         END IF

*  Write a message about the NDF name and possibly WCS and Set information.
         IF ( NDFS ) THEN
            CALL GRP_GET( NDFGRP, I, 1, NDFNAM, STATUS )
            CALL MSG_SETC( 'FNAME', NDFNAM )
            CALL MSG_SETI( 'N', I )
            CALL MSG_LOAD( ' ', '  ^N) ^FNAME', LINE, IAT, STATUS )
            IF ( USEWCS ) LINE( MAX( 45, IAT + 2 ): ) = DMN
            IF ( USESET ) LINE( MAX( 60, CHR_LEN( LINE ) + 2 ): )
     :                    = SNAME
            CALL CCD1_MSG( ' ', LINE, STATUS )
         END IF

*  Release resources.
         IF ( USESET .OR. USEWCS ) THEN
            CALL NDF_ANNUL( INDF, STATUS )
            CALL AST_ANNUL( IWCS, STATUS )
         END IF
      END DO

*  Record the number of superlists.  If it is the same as the original
*  number of lists, then we don't need to do any further Set-specific
*  processing and will set USESET false.
      IF ( .NOT. USESET ) THEN
         NSUP = NLIST
      END IF

*  Warn the user if not all the domains were the same.
      IF ( USEWCS .AND. DIFDMN ) THEN
         CALL CCD1_MSG( ' ', ' ', STATUS )
         CALL CCD1_MSG( ' ',
     :   '  ** Warning: not all the NDFs have the same Current domain.',
     :   STATUS )
      END IF

*  Construct a pair of arrays to record which lists are contained in
*  which superlist.
      K = 1
      DO I = 1, NSUP
         ILISOF( I ) = K
         DO J = 1, NLIST
            IF ( ISUP( J ) .EQ. I ) THEN
               ILIS( K ) = J
               K = K + 1
            END IF
         END DO
      END DO
      ILISOF( NSUP + 1 ) = NLIST + 1

*  Output the superlists and their contents.
      CALL CCD1_MSG( ' ', ' ', STATUS )
      CALL CCD1_MSG( ' ', '    Input position lists:', STATUS )
      CALL CCD1_MSG( ' ', '    ---------------------', STATUS )
      DO I = 1, NSUP
         CALL MSG_SETI( 'N', I )
         CALL MSG_LOAD( ' ', '  ^N)', LINE, IAT, STATUS )
         DO J = ILISOF( I ), ILISOF( I + 1 ) - 1
            CALL GRP_GET( FIOGRP, ILIS( J ), 1, LINE( IAT + 2: ),
     :                    STATUS )
            CALL CCD1_MSG( ' ', LINE, STATUS )
            LINE = ' '
         END DO

*  If the CCD_SET->work mappings have been inconsistent, warn the user
*  of this fact.
         IF ( ILISOF( I + 1 ) - ILISOF( I ) .GT. 1 .AND.
     :        MAPSET( I ) .EQ. AST__NULL ) THEN
            IF ( USEWCS ) THEN
               CALL MSG_SETC( 'WORK', 'Current' )
            ELSE
               CALL MSG_SETC( 'WORK', 'PIXEL' )
            END IF
            CALL MSG_SETI( 'I', I )
            CALL CCD1_MSG( ' ', '  ** Warning: No consistent'//
     :' CCD_SET -> ^WORK coordinate mapping in superlist ^I', STATUS )
            CALL CCD1_MSG( ' ', '              This will'//
     :' almost certainly result in errors.', STATUS )
         END IF
      END DO

*  Indicate where the position lists originated.
      IF ( NDFS ) THEN
         CALL CCD1_MSG( ' ', ' ', STATUS )
         IF ( USESET ) THEN
            CALL CCD1_MSG( ' ',
     :'  Position list names extracted from NDF extensions and'//
     :' grouped by Set.', STATUS )
         ELSE
            CALL CCD1_MSG( ' ',
     :'  Position list names extracted from NDF extensions.', STATUS )
         END IF
      END IF

*  Finally, go through the NLGRP group and assign list membership values
*  to the NDFs in that if applicable.
      IF ( NDFS .AND. USESET .AND. NNOLIS .GT. 0 ) THEN
         DO I = 1, NNOLIS

*  Get the Set information.
            CALL NDG_NDFAS( NLGRP, I, 'READ', INDF, STATUS )
            CALL CCD1_GTWCS( INDF, IWCS, STATUS )
            CALL CCD1_SETRD( INDF, IWCS, SNAME, SINDEX, JSET, STATUS )

*  See if it corresponds to a Set of which we have made a superlist.
*  If so, store the index of that list, otherwise store a value of zero.
            IF ( JSET .NE. AST__NOFRAME .AND. SNAME .NE. ' ' ) THEN
               CALL GRP_INDEX( SNAME, NAMGRP, 1, INLSUP( I ), STATUS )
            ELSE
               INLSUP( I ) = 0
            END IF

*  Release resources.
            CALL NDF_ANNUL( INDF, STATUS )
            CALL AST_ANNUL( IWCS, STATUS )
         END DO
      END IF

*  Release group resources.
      CALL CCD1_GRDEL( NAMGRP, STATUS )

      END
* $Id$
