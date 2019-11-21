      SUBROUTINE NDG_ENDPV( CREATR, STATUS )
*+
*  Name:
*     NDG_ENDPV

*  Purpose:
*     End an NDF provenance block.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDG_ENDPV( CREATR, STATUS )

*  Description:
*     This routine should be called to mark the end of an NDF
*     provenance block. The block should have been started by a
*     matching call to NDG_BEGPV. Note, provenance blocks must
*     not be nested.
*
*     During a provenance block, a list is maintained of all the
*     existing NDFs that have had their Data array mapped (either in
*     read or update mode) during the block. Another list is maintained
*     of all the NDFs that have been written (either existing NDFs
*     accessed in update mode or new NDFs) during the block.
*
*     When the block ends, the provenance information within each
*     NDF in the second may be modified to include all the NDFs in the
*     first list as parents. Whether or not this occurs is controlled by
*     the AUTOPROV environment variable. If AUTOPROV is set to '1' then
*     the input NDFs are added to the provenance information in the
*     output NDF. If AUTOPROV is set to anything other than '1' then the
*     output provenance is not updated. If AUTOPROV is not set at all,
*     the default behaviour is different for native and foreign format
*     NDFs. For native format output NDFs, the provenance will be updated
*     if one or more of the input NDFs contains a PROVENANCE extension
*     (foreign format input NDFs are ignored). The provenance within foreign
*     format output NDFs is never updated unless AUTPOPROV is set explicitly
*     to '1' (this is to avoid the potentially costly process of format
*     conversion).

*  Arguments:
*     CREATR = CHARACTER * ( * ) (Given)
*        An identifier for the software that created INDF1 (usually the
*        name of the calling application). The format of the identifier
*        is arbitrary, but the form "PACKAGE:COMMAND" is recommended.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2007-2010 Science & Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This programme is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This programme is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE.  See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this programme; if not, write to the Free Software
*     Foundation, Inc., 51, Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     31-OCT-2007 (DSB):
*        Original version.
*     5-NOV-2007 (DSB):
*        - Allow propagation of provenance to be controlled by the AUTOPROV
*        environment variable.
*        - Annul errors and continue if any NDF cannot be opened.
*     21-NOV-2007 (DSB):
*        Add CREATR argument.
*     2-JUN-2008 (DSB):
*        Use a pair of AST KeyMaps to hold the NDF names rather than a
*        pair of GRP groups (avoids the need to purge duplicate NDF
*        names, which can be very slow for large numbers of NDFs).
*     4-JUL-2008 (DSB):
*        Set the history update mode of the output NDF to SKIP, in
*        order to prevent a new history record being added to the output
*        NDF as a consequence of it being re-opened.
*     1-SEP-2008 (DSB):
*        Only include provenance from input NDFs that have had their Data
*        array mapped for read or update.
*     5-JAN-2010 (DSB):
*        Ignore any input NDF that is contained within another input NDF.
*     13-APR-2010 (DSB):
*        Do not propagate provenance by default for foreign format NDFs.
*     3-DEC-2010 (DSB):
*        Free Provenance structures when they are no longer needed.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST constants and functions
      INCLUDE 'DAT_PAR'          ! DAT constants
      INCLUDE 'NDF_PAR'          ! NDF constants
      INCLUDE 'PSX_ERR'          ! PSX error constants

*  Global Variables:
      INCLUDE 'NDG_COM1'         ! Global provenance information

*  Arguments Given:
      CHARACTER CREATR*(*)

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN
      EXTERNAL NDG1_HNDLR

*  Local Variables:
      CHARACTER AUTOPV*10
      CHARACTER PATH*512
      CHARACTER RDNDF*512
      CHARACTER RDNDF2*512
      CHARACTER WRNDF*512
      INTEGER IAT
      INTEGER INDF1
      INTEGER INDF2
      INTEGER IPROV
      INTEGER IR
      INTEGER IR2
      INTEGER IW
      INTEGER LEN1
      INTEGER LEN2
      INTEGER NR
      INTEGER NW
      INTEGER PLACE
      LOGICAL FORFMT
      LOGICAL HASHIS
      LOGICAL INPRV
      LOGICAL OLD
      LOGICAL THERE
      LOGICAL URDKMP
      LOGICAL UWRKMP
      LOGICAL UMPKMP
*.

*  Get sole access to the NDG globals
      CALL NDG1_GLOCK( .TRUE. )

*  Lock the global objects so they can be use by this thread.
      CALL NDG1_ALOCK( .TRUE., RDKMP_COM1, URDKMP, STATUS )
      CALL NDG1_ALOCK( .TRUE., WRKMP_COM1, UWRKMP, STATUS )
      CALL NDG1_ALOCK( .TRUE., MPKMP_COM1, UMPKMP, STATUS )

*  Initialise things to avoid compiler warnings.
      THERE = .FALSE.
      INDF1 = NDF__NOID
      INDF2 = NDF__NOID

*  Begin a new error reporting context (we want to clean up even if an
*  error has occurred).
      CALL ERR_BEGIN( STATUS )

*  Remove the NDF event handlers needed to record the NDFs in which
*  provenance should be stored.
      CALL NDG_HLTPV( .FALSE., OLD, STATUS )

*  Indicate that the PROVENANCE extension should be propagated by
*  default when NDF_PROP or NDF_SCOPY is called.
      CALL NDF_TUNE( 1, 'PXTPROVENANCE', STATUS )

*  End the error reporting context.
      CALL ERR_END( STATUS )

*  If no error has occurred, update the provenance information in each
*  NDF writen during the provenance block if required.
      IF ( STATUS .EQ. SAI__OK ) THEN

*  See if the environment variable AUTOPROV is defined. If so, get its
*  value.
         CALL PSX_GETENV( 'AUTOPROV', AUTOPV, STATUS )

*  If the environment variable was not defined, annul the error, and
*  indicate that it is not defined.
         IF( STATUS .EQ. PSX__NOENV ) THEN
            CALL ERR_ANNUL( STATUS )
            AUTOPV = ' '
         END IF

*  We only propagate provenance if AUTOPROV is set to '1', or if AUTOPROV
*  is unset and at least one input NDF had a provenance extension.
         IF( AUTOPV .EQ. '1' .OR. AUTOPV .EQ. ' ' ) THEN

*  Check the KeyMap holding the input NDFs to see if any of the input
*  NDFs are contained within other input NDFs. Remove any such child NDFs
*  from the KeyMap.
            NR = AST_MAPSIZE( RDKMP_COM1, STATUS )
            IR = 1
            DO WHILE( IR .LT. NR )
               RDNDF = AST_MAPKEY( RDKMP_COM1, IR, STATUS )
               LEN1 = CHR_LEN( RDNDF )

               IR2 = IR + 1
               DO WHILE( IR2 .LE. NR )
                  RDNDF2 = AST_MAPKEY( RDKMP_COM1, IR2, STATUS )
                  LEN2 = CHR_LEN( RDNDF2 )

*  See if NDF2 is a child of NDF1. If so, remove NDF2 from the KeyMap,
*  and then proceed to check the next NDF again NDF2.
                  IF( LEN2 .GT. LEN1 .AND.
     :                RDNDF2( : LEN1 ) .EQ. RDNDF( : LEN1 ) .AND.
     :                RDNDF2( LEN1 + 1 : LEN1 + 1 ) .EQ. '.' ) THEN
                     CALL AST_MAPREMOVE( RDKMP_COM1, RDNDF2, STATUS )
                     NR = NR - 1

*  See if NDF1 is a child of NDF2. If so, remove NDF1 from the KeyMap,
*  and then break out of the NDF2 loop in order to establish a new NDF1.
                  ELSE IF( LEN1 .GT. LEN2 .AND.
     :                     RDNDF( : LEN2 ) .EQ. RDNDF2( : LEN2 ) .AND.
     :                     RDNDF( LEN2 + 1 : LEN2 + 1 ) .EQ. '.' ) THEN
                     CALL AST_MAPREMOVE( RDKMP_COM1, RDNDF, STATUS )
                     NR = NR - 1
                     IR2 = NR + 1
                     IR = IR - 1

*  If there is no relation between NDF1 and NDF2, proceed to check the
*  next NDF2.
                  ELSE
                     IR2 = IR2 + 1
                  END IF
               END DO

               IR = IR + 1
            END DO

*  Loop round each output NDF
            NW = AST_MAPSIZE( WRKMP_COM1, STATUS )
            DO IW = 1, NW
               WRNDF = AST_MAPKEY( WRKMP_COM1, IW, STATUS )

*  See if this output NDF is a foreign format file. If so, remove the
*  format code from the end of the NDF path.
               IAT = INDEX( WRNDF, '::' )
               IF( IAT .NE. 0 ) THEN
                  FORFMT = .TRUE.
                  PATH = WRNDF( : IAT - 1 )
               ELSE
                  FORFMT = .FALSE.
                  PATH = WRNDF
               END IF

*  Check no error has occurred. Also skip this NDF if it is a foreign
*  format file and AUTOPROV has not been set explicitly to '1'.
               IF( STATUS .EQ. SAI__OK .AND.
     :              ( .NOT. FORFMT .OR. AUTOPV .EQ. '1' ) ) THEN

*  Get an NDF identifier for it.
                  CALL NDF_OPEN( DAT__ROOT, PATH, 'UPDATE', 'OLD',
     :                           INDF1, PLACE, STATUS )

*  If the output NDF could not be opened (e.g. if it was a temporary NDF
*  that has since been deleted), annul the error and pass on.
                  IF( STATUS .NE. SAI__OK ) THEN
                     CALL ERR_ANNUL( STATUS )
                     INDF1 =  NDF__NOID

*  Otherwise, see if this output NDF has a PROVENANCE extenion
                  ELSE
                     CALL NDF_XSTAT( INDF1, 'PROVENANCE', THERE,
     :                               STATUS )
                  END IF

*  Only modify this output NDF if is exists and has no provenance
*  extension.
                  IF( INDF1 .NE. NDF__NOID .AND. .NOT. THERE ) THEN

*  Read in the provenance information.
                     CALL NDG_READPROV( INDF1, CREATR, IPROV, STATUS )

*  Otherwise, loop round each input NDF, maintaining a flag that
*  indicates if any input NDF had a PROVENANCE extension.
                     INPRV = .FALSE.

                     NR = AST_MAPSIZE( RDKMP_COM1, STATUS )
                     DO IR = 1, NR
                        RDNDF = AST_MAPKEY( RDKMP_COM1, IR, STATUS )

*  Existing NDFs that were opened for UPDATE will be in both KeyMaps. Check
*  to make sure we are not establishing an NDF as its own parent. Also,
*  check the Data array of the NDF was mapped for READ or UPDATE. Also
*  check no error has occurred.
                        IF( WRNDF .NE. RDNDF .AND.
     :                      AST_MAPHASKEY( MPKMP_COM1, RDNDF, STATUS )
     :                      .AND. STATUS .EQ. SAI__OK ) THEN

*  See if this input NDF is a foreign format file. If so, remove the
*  format code from the end of the NDF path.
                           IAT = INDEX( RDNDF, '::' )
                           IF( IAT .NE. 0 ) THEN
                              FORFMT = .TRUE.
                              PATH = RDNDF( : IAT - 1 )
                           ELSE
                              FORFMT = .FALSE.
                              PATH = RDNDF
                           END IF

*  If this input is a native format file, or AUTOPROV was explicitly set
*  to '1', copy it's provenanceinfo into the output NDF.
                           IF( .NOT. FORFMT .OR. AUTOPV .EQ. '1' ) THEN

*  Get an NDF identifier for it. This will fail if the NDF has been
*  deleted (e.g. if it was a temporary NDF), so annul the error if an
*  error is reported.
                              CALL NDF_OPEN( DAT__ROOT, RDNDF, 'READ',
     :                                       'OLD', INDF2, PLACE,
     :                                       STATUS )
                              IF( STATUS .NE. SAI__OK ) THEN
                                 CALL ERR_ANNUL( STATUS )

*  Otherwise, modify the provenance information in INDF1 to include
*  INDF2 as a parent of INDF1. This also transfers all the ancestors
*  of INDF2 to INDF1.
                              ELSE
                                 CALL NDG_PUTPROV( IPROV, INDF2,
     :                                            AST__NULL,
     :                                            .FALSE., STATUS )

*  Set the flag that indicates if any INPUT NDF had a provenance extension.
                                 CALL NDF_XSTAT( INDF2, 'PROVENANCE',
     :                                           THERE, STATUS )
                                 IF( THERE ) INPRV = .TRUE.

*  Annul the NDF identifiers.
                                 CALL NDF_ANNUL( INDF2, STATUS )
                              END IF
                           END IF
                        END IF
                     END DO

*  Write out the modified provenance information to the output NDF, and
*  then free the structiure holding the provenance info.
                     CALL NDG_WRITEPROV( IPROV, INDF1, .FALSE., STATUS )
                     CALL NDG_FREEPROV( IPROV, STATUS )

*  If none of the input NDFs had a provenance extension, delete the
*  provenance extension from the output NDF unless AUTOPROV indicates that
*  default provenance information should be stored in the output NDF.
                     IF( .NOT. INPRV .AND. AUTOPV .EQ. ' ' ) THEN
                        CALL NDF_XDEL( INDF1, 'PROVENANCE', STATUS )
                     END IF

                  END IF

*  If we have an NDF, close it. If the NDF has a history component, set its
*  history update mode to SKIP before closing it. This means that no history
*  record will be added to the NDF when it is closed, but the history update
*  mode stored in the NDF structure on disk will not be changed.
                  IF( INDF1 .NE. NDF__NOID ) THEN
                     CALL NDF_STATE( INDF1, 'History', HASHIS, STATUS )
                     IF( HASHIS ) CALL NDF_HSMOD( 'SKIP', INDF1,
     :                                            STATUS )
                     CALL NDF_ANNUL( INDF1, STATUS )
                  END IF
               END IF
            END DO
         END IF
      END IF

*  Free resources.
      CALL AST_ANNUL( RDKMP_COM1, STATUS )
      CALL AST_ANNUL( MPKMP_COM1, STATUS )
      CALL AST_ANNUL( WRKMP_COM1, STATUS )

*  Allow other threads to access the NDG globals
      CALL NDG1_GLOCK( .FALSE. )

      END
