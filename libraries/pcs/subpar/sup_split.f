      SUBROUTINE SUBPAR_SPLIT ( NAMESTRING, MAXLEVS, NUMLEVS, COMPONENT,
     :  FILENAME, STATUS )
*+
*  Name:
*     SUBPAR_SPLIT

*  Purpose:
*     Splits up a full data structure name string.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SUBPAR_SPLIT ( NAMESTRING, MAXLEVS, NUMLEVS, COMPONENT,
*    :      FILENAME, STATUS )

*  Description:
*     Split a string specifying an HDS structure into the name of the
*     container file and the component names.

*  Arguments:
*     NAMESTRING=CHARACTER*(*) (given)
*        full name of a data structure component, including the
*        filename. It is assumed that the top-level of the structure
*        has the same name as the file, minus its directory
*        specification.
*     MAXLEVS=INTEGER (given)
*        maximum possible number of levels in the structure name
*     NUMLEVS=INTEGER (returned)
*        actual number of levels found in the structure name
*     COMPONENT(MAXLEVS)=CHARACTER*(*) (returned)
*        the names of the structure components
*     FILENAME=CHARACTER*(*) (returned)
*        the full name of the VMS container file
*     STATUS=INTEGER

*  Algorithm:
*     The given character string is split into components, taking '.' to
*     be the component delimiter. The first component is then searched
*     for ':' and for any  of ']', '>' or '/'. This enables directory
*     names or logical names to be split off and works for both VMS and
*     UNIX style names.

*  Copyright:
*     Copyright (C) 1984-1992 Science & Engineering Research Council
*     Copyright (C) 2005 Particle Physics and Astronomy Research Council.
*     All Rights Reserved.

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
*     You should have received a copy of the GNU General Public
*     License along with this program; if not, write to the Free
*     Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
*     MA 02110-1301, USA

*  Authors:
*     BDK: B D Kelly (ROE)
*     BMC: B McNally (ROE)
*     AJC: A J Chipperfield (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     25-JUL-1984 (BDK):
*        Original
*     08-APR-1986 (BMC):
*        Repair last level extraction to use LEN(string)
*     03-DEC-1987 (BDK):
*        Allow for ':' in slices
*     15-NOV-1990 (AJC):
*        Correct for case of no top-level object following
*        filespec in quotes
*        allow () without top-level name following quotes
*        also remove () from filename if there
*     04-FEB-1991 (AJC):
*        correct deriving COMPONENT(1) from filename when
*        slice info ':' included
*     15-FEB-1991 (AJC):
*        correct above correction
*     31-JUL-1991 (AJC):
*        cope with UNIX names also
*     24-SEP-1991 (AJC):
*        prefix messages with 'SUBPAR:'
*     10-NOV-1992 (AJC):
*        Use SUBPAR__NAMIN not DAT__
*     28-DEC-2005 (TIMJ):
*        Call HDS_SPLIT so that we can try to deal with filenames and .SDF
*        extensions all in one place.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE


*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'SUBPAR_ERR'


*  Arguments Given:
      CHARACTER*(*) NAMESTRING     ! full name of a data structure
                                   ! component, including the VMS
                                   ! filename. It is assumed that the
                                   ! top-level of the structure
                                   ! has the same name as the file,
                                   ! minus its directory specification.

      INTEGER MAXLEVS              ! maximum possible number of levels
                                   ! in the structure name


*  Arguments Returned:
      INTEGER NUMLEVS              ! actual number of levels found in
                                   ! the structure name

      CHARACTER*(*) COMPONENT(*)   ! the names of the structure
                                   ! components

      CHARACTER*(*) FILENAME       ! the full name of the VMS container
                                   ! file


*  Status:
      INTEGER STATUS


*  External References:
      INTEGER STRING_IANYR
      EXTERNAL STRING_IANYR
      INTEGER CHR_LEN
      EXTERNAL CHR_LEN


*  Local Variables:
      INTEGER NAMLEN               ! used length of NAMESTRING
      LOGICAL MORE                 ! loop controller for parsing
      INTEGER START                ! pointer into NAMESTRING
      INTEGER FINISH               ! pointer into NAMESTRING
      INTEGER J                    ! counter for number of levels
      CHARACTER*32 TCOMP           ! temporary string for component
      INTEGER F1                   ! Start of filename
      INTEGER F2                   ! End of filename
      INTEGER P1                   ! Start of component path
      INTEGER P2                   ! End of component path


*.



      IF ( STATUS .EQ. SAI__OK ) THEN

         NAMLEN = CHR_LEN( NAMESTRING )
         MORE = .TRUE.
         START = 1
         FINISH = 1
         COMPONENT(1) = ' '

*      Use HDS to split the string into filename and component path
         CALL HDS_SPLIT( NAMESTRING(:NAMLEN), F1, F2, P1, P2, STATUS )
         FILENAME = NAMESTRING(F1:F2)

*      P1 is now the start of the component specification but we want
*      START to be the first none '.'
         IF ( NAMESTRING(P1:P1) .EQ. '.') THEN
            START = P1 + 1
         ELSE
            START = P1
         END IF

*      Indicate that P2 is now the length of the string from HDS viewpoint
         NAMLEN = P2

*      No components if START > P2
         IF ( START .GT. P2 ) THEN
            MORE = .FALSE.
         ELSE
            MORE = .TRUE.
         END IF

*      START now points to the first character following the first '.'
*      after the filename/top-level structre, provided MORE is .TRUE.
         J = 1

         DO WHILE ( ( MORE ) .AND. ( J .LT. MAXLEVS ) )

            FINISH = INDEX ( NAMESTRING(START:NAMLEN), '.' ) +
     :        START - 1

            IF ( FINISH .GT. START ) THEN
               J = J + 1
               COMPONENT(J) = NAMESTRING(START:FINISH-1)
               START = FINISH + 1
            ELSE IF ( FINISH .LT. START ) THEN
               J = J + 1
               COMPONENT(J) = NAMESTRING(START:NAMLEN)
               MORE = .FALSE.
            ELSE

*            Two consecutive '..' - an error
               STATUS = SUBPAR__NAMIN
               CALL EMS_SETC ( 'NAME', NAMESTRING )
               CALL EMS_REP ( 'SUP_SPLIT1',
     :         'SUBPAR: Object name ^NAME contains ''..''', STATUS )
               MORE = .FALSE.

            ENDIF

         ENDDO

         IF ( ( J .EQ. MAXLEVS ) .AND. MORE ) THEN

            STATUS = SUBPAR__NAMIN
            CALL EMS_SETC ( 'NAME', NAMESTRING )
            CALL EMS_SETI ( 'MAXLEVS', MAXLEVS )
            CALL EMS_REP ( 'SUP_SPLIT2',
     :      'SUBPAR: Object name ^NAME exceeds maximum '//
     :      '^MAXLEVS components', STATUS )

         ELSE IF ( STATUS .EQ. SAI__OK ) THEN

*      If a top-level name was not specified, derive it from the file
*      name and copy it into COMPONENT(1) allowing for the case of
*      slice or cell information only being provided following quotes.
            IF (( COMPONENT(1) .EQ. ' ' ) .OR.
     :          ( COMPONENT(1)(1:1) .EQ. '(' )) THEN

*      Now we are interested in the FILENAME length
               NAMLEN = F2 - F1 + 1

*           Look first for a logical name terminator
*           Only look as far as the first '(' - any after that is slice
*           If none, set START to 1
               FINISH = INDEX ( FILENAME(1:NAMLEN), '(' )
               IF ( FINISH .EQ. 0 ) FINISH = NAMLEN
               START = MAX ( INDEX ( FILENAME(1:FINISH), ':' ), 1 )
*           then for a directory spec terminator
               FINISH = STRING_IANYR ( FILENAME(START:NAMLEN), ']>/' )

               IF ( FINISH .EQ. 0 ) THEN

*              If there's no directory spec, then, if we were pointing
*              at a logical name terminator move on one place.
                  IF  ( START .GT. 1 ) START = START + 1

               ELSE
*              If there is a directory spec, point following it
                  START = START + FINISH

               ENDIF

*           Find the end of the actual filename
               FINISH = INDEX ( FILENAME(START:NAMLEN), '.' ) - 1

               IF ( FINISH .LT. 0 ) THEN
*              No extensions
                  FINISH = NAMLEN

               ELSEIF ( FINISH .EQ. 0 ) THEN
*              No name
                  STATUS = SUBPAR__NAMIN
                  CALL EMS_REP ( 'SUP_SPLIT3',
     :           'SUBPAR: Attempt to split a blank object name',
     :            STATUS )

               ELSE
*              Finish at end of filename
                  FINISH = START + FINISH - 1

               ENDIF

*           Store the file name as the name of the top-level component
               TCOMP = COMPONENT(1)
               COMPONENT(1) = FILENAME(START:FINISH) // TCOMP

            ENDIF

            NUMLEVS = J

         ENDIF

      ENDIF

      END
