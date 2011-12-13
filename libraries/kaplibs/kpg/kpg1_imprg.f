      SUBROUTINE KPG1_IMPRG( INLOC, CLIST, OUTLOC, STATUS )

*+
*  Name:
*     KPG1_IMPRG

*  Purpose:
*     Propagates NDF information for IMAGE-format applications.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_IMPRG( INLOC, CLIST, OUTLOC, STATUS )

*  Description:
*     Until the NDF access routines are fully implemented in KAPPA,
*     this routine provides a means by which KAPPA applications using
*     the IMAGE data format may correctly propagate an input NDF.  Thus
*     a pre-V1.0 KAPPA application will behave like a post-V1.0 one
*     that only works on primitive DATA_ARRAYs, and does not process
*     VARIANCE, QUALITY, or HISTORY.
*
*     See SGP/38 for more details of the NDF and its propagation rules.

*  Arguments:
*     INLOC = CHARACTER * ( * ) (Read)
*        Locator to the input NDF.
*     CLIST = CHARACTER * ( * ) (Read)
*        A comma-separated list of the NDF components which are to be
*        propagated from the input to the output NDF.  By default,
*        HISTORY, LABEL and all extensions are propagated.  See below
*        for further details.
*     OUTLOC = CHARACTER * ( * ) (Read)
*        Locator to the output NDF.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     -  Form an uppercase version of the list.
*     -  For each of the optional components search for it within the
*     list.  If it is present, obtain a locator to it and find its data
*     type (except for character components).  Copy the component
*     recursively to the output NDF.
*     -  Similarly, copy the default components, but without searching
*     the list.
*     -  Report a contextual error if something goes wrong.

*  Component Propagation:
*     The template components whose values are to be propagated to
*     initialise the new data structure are specified via the CLIST
*     argument. Thus CLIST='AXIS,QUALITY' would cause the new NDF to
*     inherit its axes structures and QUALITY values (if available) from
*     the input structure, in addition to those propagated by default.

*  Implementation Deficiencies:
*     This routine Does not permit components to be excluded via a NO
*     prefix.  Since this application is a short-term measure and is
*     intended solely for a specific set of applications this is deemed
*     unnecessary.

*  Copyright:
*     Copyright (C) 1991 Science & Engineering Research Council.
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
*     MJC: Malcolm J. Currie  (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1991 October 25 (MJC):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}
*-

*  Type Definitions:
      IMPLICIT NONE            ! Switch off default typing

*  Global Constants:
      INCLUDE 'SAE_PAR'        ! Environment constants
      INCLUDE 'DAT_PAR'        ! Data-system constants
      INCLUDE 'DAT_ERR'        ! Data-system errors

*  Arguments Given:
      CHARACTER * ( * )
     :  CLIST,
     :  INLOC,
     :  OUTLOC

*  Status:
      INTEGER
     :  STATUS                 ! Global status

*  External References:
      INTEGER CHR_LEN          ! Length of a character string excluding
                               ! trailing blanks

*  Local Variables:
      LOGICAL                  ! True if:
     :  PRESNT                 ! A component is present

      INTEGER
     :  INDCMP,                ! Index to a component within the list
     :  NCOMP                  ! No. of characters in the component
                               ! list

      CHARACTER * 132
     :  COMPS                  ! Uppercase copy of the component list

      CHARACTER * (DAT__SZTYP)
     :  TYPE                   ! The TYPE of a component

      CHARACTER * (DAT__SZLOC) ! Locator to:
     :  CLOC                   ! A component

*.

*    Check inherited status on entry.

      IF ( STATUS .NE. SAI__OK ) RETURN

*    Convert the list of components to uppercase.

      COMPS = CLIST
      CALL CHR_UCASE( COMPS )

*    Are there any non-default components to propagate?

      NCOMP = CHR_LEN( COMPS )
      IF ( NCOMP .GT. 0 ) THEN

*       Yes there are.  Test for each in turn.

*       QUALITY
*       =======

         INDCMP = INDEX( COMPS, 'QUALITY' )
         IF ( INDCMP .GT. 0 ) THEN

*          Look for a QUALITY component.

            PRESNT = .FALSE.
            CALL DAT_THERE( INLOC, 'QUALITY', PRESNT, STATUS )
            IF ( PRESNT ) THEN
               CLOC = ' '
               CALL DAT_FIND( INLOC, 'QUALITY', CLOC, STATUS )
               CALL DAT_TYPE( CLOC, TYPE, STATUS )

*             Check that its type is bone fide.

               IF ( TYPE .EQ. 'QUALITY' .OR. TYPE .EQ. '_UBYTE' .OR.
     :              TYPE .EQ. 'ARRAY' ) THEN

*                Copy it recursively.

                  CALL DAT_COPY( CLOC, OUTLOC, 'QUALITY', STATUS )
               END IF

*             Tidy the locator.

               CALL DAT_ANNUL( CLOC, STATUS )
            END IF
         END IF

*       VARIANCE
*       ========

         INDCMP = INDEX( COMPS, 'VARIANCE' )
         IF ( INDCMP .GT. 0 ) THEN

*          Look for a QUALITY component.

            PRESNT = .FALSE.
            CALL DAT_THERE( INLOC, 'VARIANCE', PRESNT, STATUS )
            IF ( PRESNT ) THEN
               CLOC = ' '
               CALL DAT_FIND( INLOC, 'VARIANCE', CLOC, STATUS )

*             Copy it recursively and tidy the locator.

               CALL DAT_COPY( CLOC, OUTLOC, 'VARIANCE', STATUS )
               CALL DAT_ANNUL( CLOC, STATUS )
            END IF
         END IF

*       AXIS
*       ====

         INDCMP = INDEX( COMPS, 'AXIS' )
         IF ( INDCMP .GT. 0 ) THEN

*          Look for an AXIS component.

            PRESNT = .FALSE.
            CALL DAT_THERE( INLOC, 'AXIS', PRESNT, STATUS )
            IF ( PRESNT ) THEN
               CLOC = ' '
               CALL DAT_FIND( INLOC, 'AXIS', CLOC, STATUS )
               CALL DAT_TYPE( CLOC, TYPE, STATUS )

*             Check that its type is bone fide.

               IF ( TYPE .EQ. 'AXIS' ) THEN

*                Copy it recursively.

                  CALL DAT_COPY( CLOC, OUTLOC, 'AXIS', STATUS )
               END IF

*             Tidy the locator.

               CALL DAT_ANNUL( CLOC, STATUS )
            END IF
         END IF

*       UNITS
*       =====

         INDCMP = INDEX( COMPS, 'UNITS' )
         IF ( INDCMP .GT. 0 ) THEN

*          Look for a UNITS component.

            PRESNT = .FALSE.
            CALL DAT_THERE( INLOC, 'UNITS', PRESNT, STATUS )
            IF ( PRESNT ) THEN
               CLOC = ' '
               CALL DAT_FIND( INLOC, 'UNITS', CLOC, STATUS )

*             Copy it and tidy the locator.

               CALL DAT_COPY( CLOC, OUTLOC, 'UNITS', STATUS )
               CALL DAT_ANNUL( CLOC, STATUS )
            END IF
         END IF

*       TITLE
*       =====

         INDCMP = INDEX( COMPS, 'TITLE' )
         IF ( INDCMP .GT. 0 ) THEN

*          Look for a TITLE component.

            CALL DAT_THERE( INLOC, 'TITLE', PRESNT, STATUS )
            PRESNT = .FALSE.
            IF ( PRESNT ) THEN
               CLOC = ' '
               CALL DAT_FIND( INLOC, 'TITLE', CLOC, STATUS )

*             Copy it and tidy the locator.

               CALL DAT_COPY( CLOC, OUTLOC, 'TITLE', STATUS )
               CALL DAT_ANNUL( CLOC, STATUS )
            END IF
         END IF
      END IF

*    Now attend to the default components.
*    =====================================

*    EXTENSIONS
*    ==========

*    Look for a MORE extension structure of the correct type.

      PRESNT = .FALSE.
      CALL DAT_THERE( INLOC, 'MORE', PRESNT, STATUS )
      IF ( PRESNT ) THEN
         CLOC = ' '
         CALL DAT_FIND( INLOC, 'MORE', CLOC, STATUS )
         CALL DAT_TYPE( CLOC, TYPE, STATUS )
         IF ( TYPE .EQ. 'EXT' ) THEN

*          Copy it and all extensions within it to the output structure.

            CALL DAT_COPY( CLOC, OUTLOC, 'MORE', STATUS )
         END IF

*       Tidy the locator.

         CALL DAT_ANNUL( CLOC, STATUS )
      END IF

*    HISTORY
*    =======

*    Look for a MORE extension structure of the correct type.

      PRESNT = .FALSE.
      CALL DAT_THERE( INLOC, 'HISTORY', PRESNT, STATUS )
      IF ( PRESNT ) THEN
         CLOC = ' '
         CALL DAT_FIND( INLOC, 'HISTORY', CLOC, STATUS )
         CALL DAT_TYPE( CLOC, TYPE, STATUS )
         IF ( TYPE .EQ. 'HISTORY' ) THEN

*          Copy it and all extensions within it to the output structure.

            CALL DAT_COPY( CLOC, OUTLOC, 'HISTORY', STATUS )
         END IF

*       Tidy the locator.

         CALL DAT_ANNUL( CLOC, STATUS )
      END IF

*    LABEL
*    =====

*    Look for a LABEL component.

      PRESNT = .FALSE.
      CALL DAT_THERE( INLOC, 'LABEL', PRESNT, STATUS )
      IF ( PRESNT ) THEN
         CALL DAT_FIND( INLOC, 'LABEL', CLOC, STATUS )

*       Copy it.

         CALL DAT_COPY( CLOC, OUTLOC, 'LABEL', STATUS )

*       Tidy the locator.

         CALL DAT_ANNUL( CLOC, STATUS )
      END IF

      IF ( STATUS .NE. SAI__OK ) THEN
         CALL MSG_SETC( 'CLIST', 'COMPS' )
         CALL ERR_REP( 'KPG1_IMPRG__ERR',
     :     'Error whilst propagating the ^CLIST components to the '/
     :     /'output NDF.', STATUS )
      END IF

      END
