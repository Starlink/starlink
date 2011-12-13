      SUBROUTINE HDS_SPLIT( NAME, F1, F2, P1, P2, STATUS )
*+
*  Name:
*     HDS_SPLIT

*  Purpose:
*     Split an HDS object name into a file name and a path name.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL HDS_SPLIT( NAME, F1, F2, P1, P2, STATUS )

*  Description:
*     This routine analyses a general HDS object name and locates the
*     substrings which specify the container file name and the path
*     name of the object within the file.

*  Arguments:
*     NAME = CHARACTER * ( * ) (Given)
*        HDS object name to be analysed.
*     F1 = INTEGER (Returned)
*        Character position of the start of the file name.
*     F2 = INTEGER (Returned)
*        Character position of the end of the file name.
*     P1 = INTEGER (Returned)
*        Character position of the start of the path name.
*     P2 = INTEGER (Returned)
*        Character position of the end of the path name.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  If the routine succeeds, then F1 and F2 will always identify
*     the container file name.
*     -  If the object describes a top-level object, then there will be
*     no path name. In this case, P2 will be returned greater than P1.
*     Otherwise, P1 and P2 will identify the path name.
*     -  This routine performs some checks on the validity of the
*     object name supplied, but these are not comprehensive. Only an
*     attempt to locate the object will fully validate the name.
*     -  Any blank characters which surround the file or path names
*     will be excluded from the returned character string positions.
*     - If the string begins with a quote, it is assumed that the name is
*     quoted and the component part follows. eg "test.sdfx".MORE will
*     result in a filename called test.sdfx and a .MORE component.
*     - If the first component after the root is ".sdf" this will be absorbed
*     into the filename (which HDS can open without problem) unless there
*     is a component at the top level of the HDS file called "SDF". If the
*     file can not be opened by HDS the ".sdf" will be assumed to be part of
*     the filename. This approach is not full proof since HDS_SPLIT is not
*     always called with a full path to a valid file. In generaly the best place
*     for disambiguating would be the caller but this routine is used in places
*     other than HDS_FIND so it is better to absorb the overhead. The HDS open
*     will only occur for the .sdf case. The earlier note comments that some
*     validation occurs but not all, this is probably at odds with that sentiment.

*  Machine-specific features used:
*     This routine unavoidably has to make assumptions about the format
*     of VAX/VMS and POSIX file names.

*  Copyright:
*     Copyright (C) 1993 Science & Engineering Research Council
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
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     DSB: David Berry (UCLan, Starlink)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     PWD: Peter W. Draper (JAC, Durham University)
*     {enter_new_authors_here}

*  History:
*     26-FEB-1991 (RFWS):
*        Original version.
*     16-OCT-1991 (RFWS):
*        Allow the returned path name to contain a leading '.' if
*        present.
*     17-OCT-1991 (RFWS):
*        Fixed bug: path name limits not being returned after earier
*        changes.
*     17-JAN-1992 (RFWS):
*        Added handling of POSIX filenames, as well as VMS.
*     15-FEB-1998 (DSB):
*        Brought into NDG from NDF.
*     23-DEC-2005 (TIMJ):
*        Brought into HDS
*     27-DEC-2005 (TIMJ):
*        Rename as HDS_SPLIT so that it can be part of the public interface
*        since it is required by NDF.
*     28-DEC-2005 (TIMJ):
*        Deal with .sdf in path name.
*     04-JAN-2006 (PWD):
*        Make local copy of DAT__FLEXT so a subtring can be formed (non-standard
*        to take a substring of a character constant).
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'DAT_ERR'          ! DAT_ error codes

*  Arguments Given:
      CHARACTER * ( * ) NAME

*  Arguments Returned:
      INTEGER F1
      INTEGER F2
      INTEGER P1
      INTEGER P2

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Significant length of a string
      LOGICAL CHR_SIMLR          ! Compare without case

*  Local Variables:
      INTEGER DELTA              ! .SDF correction
      CHARACTER * ( 1 ) DUM1     ! Dummy argument
      CHARACTER * ( 1 ) DUM2     ! Dummy argument
      CHARACTER * ( 1 ) DUM3     ! Dummy argument
      CHARACTER * ( 1 ) DUM4     ! Dummy argument
      CHARACTER * ( DAT__SZFLX ) FLEXT ! Local copy of DAT__FLEXT
      CHARACTER * ( 30 ) SYSNAM  ! Name of operating system
      CHARACTER * ( DAT__SZLOC ) TESTLOC ! Test locator for .sdf disambiguation
      INTEGER F                  ! Position of first non-blank character
      INTEGER I1                 ! First INDEX result
      INTEGER I2                 ! Second INDEX result
      INTEGER IEND               ! Position of end of file name
      INTEGER ISTART             ! Position of start of path name
      INTEGER L                  ! Position of last non-blank character
      INTEGER OFFSET             ! Offset into string
      LOGICAL THERE              ! Did we have a SDF component?
      INTEGER LSTAT              ! Local status
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise and find the first and last non-blank characters in the
*  object name.
      CALL CHR_FANDL( NAME, F, L )

*  If the name is blank, then report an error.
      IF ( F .GT. L ) THEN
         STATUS = DAT__NAMIN
         CALL EMS_REP( 'HDS_SPLIT_BLNK',
     :                 'Blank HDS name supplied.',
     :                 STATUS )

*  If the first non-blank character is a quote.
*  ===========================================

*  Search for the closing quote.
      ELSE IF ( NAME( F : F ) .EQ. '"' ) THEN
         IEND = 0
         IF ( L .GT. F ) THEN
            IEND = INDEX( NAME( F + 1 : L ), '"' )
            IF ( IEND .NE. 0 ) IEND = IEND + F
         END IF

*  If the closing quote is missing, then report an error.
         IF ( IEND .EQ. 0 ) THEN
            STATUS = DAT__NAMIN
            CALL EMS_SETC( 'NAME', NAME( F : L ) )
            CALL EMS_REP( 'HDS_SPLIT_QTE',
     :                    'Missing quote in the HDS name ''^NAME''.',
     :                    STATUS )

*  If the quotes are consecutive, then report an error.
         ELSE IF ( IEND .EQ. F + 1 ) THEN
            STATUS = DAT__NAMIN
            CALL EMS_SETC( 'NAME', NAME( F : L ) )
            CALL EMS_REP( 'HDS_SPLIT_NON',
     :                    'File name absent in the HDS name ''^NAME''.',
     :                    STATUS )

*  Otherwise, find the first and last non-blank characters in the
*  filename which appears between the quotes.
         ELSE
            CALL CHR_FANDL( NAME( F + 1 : IEND - 1 ), F1, F2 )

*  If the file name is blank, then report an error.
            IF ( F1 .GT. F2 ) THEN
               STATUS = DAT__NAMIN
               CALL EMS_SETC( 'NAME', NAME( F : L ) )
               CALL EMS_REP( 'HDS_SPLIT_BLQ',
     :                       'Quoted filename is blank in the HDS ' //
     :                       'name ''^NAME''.',
     :                       STATUS )

*  Otherwise, derive the position of the file name string.
            ELSE
               F1 = F1 + F
               F2 = F2 + F
            END IF
         END IF

*  If there have been no errors, then attempt to find the position of
*  the path name string.
         IF ( STATUS .EQ. SAI__OK ) THEN

*  If the file name extends to the end of the object name string, then
*  there is no path name (i.e. it is a top-level object), so return
*  a "null" position.
            IF ( IEND .GE. L ) THEN
               P1 = 1
               P2 = 0

*  Otherwise, find the first and last non-blank character positions in
*  whatever follows the file name.
            ELSE
               CALL CHR_FANDL( NAME( IEND + 1 : L ), ISTART, L )
               ISTART = ISTART + IEND
               L = L + IEND

*  If this candidate path name does not start with a '.' or a '(', then
*  there is a top-level name present. This must be ignored (the actual
*  file name fills this role).
               IF ( ( NAME( ISTART : ISTART ) .NE. '.' ) .AND.
     :              ( NAME( ISTART : ISTART ) .NE. '(' ) ) THEN

*  Find the first following occurrence of a '.' or '('.
                  I1 = INDEX( NAME( ISTART : L ), '.' )
                  I2 = INDEX( NAME( ISTART : L ), '(' )
                  IF ( ( I1 .EQ. 0 ) .OR.
     :                 ( ( I2 .NE. 0 ) .AND. ( I2 .LT. I1 ) ) ) THEN
                     I1 = I2
                  END IF

*  Move the starting position to the character found, or beyond the
*  last non-blank character if a '.' or '(' was not found.
                  IF ( I1 .NE. 0 ) THEN
                     ISTART = ISTART + I1 - 1
                  ELSE
                     ISTART = L + 1
                  END IF
               END IF

*  If we are beyond the end of the input name string, then only a
*  top-level object name is present. Ignore it (i.e. return a "null"
*  path name position).
               IF ( ISTART .GT. L ) THEN
                  P1 = 1
                  P2 = 0

*  Otherwise return the path name limits.
               ELSE
                  P1 = ISTART
                  P2 = L
               END IF
            END IF
         END IF

*  If the file name is not quoted.
*  ==============================
*
*  Its end must be detected. First determine which operating system is
*  in use, as this determines the file name format.
      ELSE
*  Assume non-VMS for now until rewrite in C since we do not
*  want PSX dependency from HDS
*         CALL PSX_UNAME( SYSNAM, DUM1, DUM2, DUM3, DUM4, STATUS )
         SYSNAM = 'UNIX'
         IF ( STATUS .EQ. SAI__OK ) THEN
            CALL CHR_UCASE( SYSNAM )

*  If we are on VMS, then search for a ']' or '>' character, which may
*  mark the end of an explicit directory specification (note the latter
*  may result if a file name is entered on a DCL command line).
            IF ( INDEX( SYSNAM, 'VMS' ) .NE. 0 ) THEN
               IEND = INDEX( NAME( F : L ), ']' )
               IF ( IEND .EQ. 0 ) IEND = INDEX( NAME( F : L ), '>' )

*  If there is no explicit directory reference, then search for a ':'
*  which may mark the end of a logical name representing the directory.
*  Also search for a '('. The ':' can be used only if it occurs before
*  the '(', otherwise it is part of a subscript expression.
               IF ( IEND .EQ. 0 ) THEN
                  I1 = INDEX( NAME( F : L ), ':' )
                  I2 = INDEX( NAME( F : L ), '(' )
                  IF ( ( I2 .EQ. 0 ) .OR. ( I1 .LT. I2 ) ) IEND = I1
               END IF

*  Correct for the starting position of the search.
               IEND = IEND + F - 1

*  If we are not on VMS, then assume POSIX file name format. Search for
*  the last '/' character which delimits the final field of the file's
*  path name.
            ELSE
               DO 1 IEND = L, F, -1
                  IF ( NAME( IEND : IEND ) .EQ. '/' ) GO TO 2
 1             CONTINUE
 2             CONTINUE
            END IF

*  Having located the end of the directory specification (if any), now
*  search for a '.' or a '(' which marks the end of the first field in
*  the object path name.
            IF ( IEND .LT. L ) THEN
               I1 = INDEX( NAME( IEND + 1 : L ), '.' )
               I2 = INDEX( NAME( IEND + 1 : L ), '(' )
               IF ( ( I1 .EQ. 0 ) .OR.
     :              ( ( I2 .NE. 0 ) .AND. ( I2 .LT. I1 ) ) ) THEN
                  I1 = I2
               END IF

*  If no such terminating character exists, then use the whole name
*  string as the file name. Otherwise, select the first path name field
*  for use as the file name.
               IF ( I1 .EQ. 0 ) THEN
                  IEND = L
               ELSE
                  IEND = IEND + I1 - 1
               END IF
            END IF

*  If a '.' occurs in the first character position, then report an
*  error.
            IF ( IEND .LT. F ) THEN
               STATUS = DAT__NAMIN
               CALL EMS_SETC( 'NAME', NAME( F : L ) )
               CALL EMS_REP( 'HDS_SPLIT_MSF',
     :                       'Missing field in the HDS name ''^NAME''.',
     :                       STATUS )

*  Otherwise, note the file name position, omitting any trailing
*  blanks.
            ELSE
               F1 = F
               F2 = F1 + CHR_LEN( NAME( F : IEND ) ) - 1
            END IF

*  Attempt to remove .sdf from the path and into the filename.
*  Need to look for 4 characters after the end of the path
*  The main problem here is that we have to *assume* that there is no SDF component
*  OR we have to actually look...
*  Have to watch out for .sdfx components.
            IF ( IEND + DAT__SZFLX .LE. L .AND.
     :           CHR_SIMLR( NAME(F2+1:F2+DAT__SZFLX), DAT__FLEXT )) THEN
*     Must disambiguate .sdf from .sdfx
               OFFSET = F2 + DAT__SZFLX + 1
               DELTA = 0
               IF ( OFFSET .LT. L ) THEN
                  IF ( NAME(OFFSET:OFFSET) .EQ. ' '
     :                 .OR. NAME(OFFSET:OFFSET) .EQ. '.'
     :                 .OR. NAME(OFFSET:OFFSET) .EQ. '(' ) THEN
*     Next character is space, . or paren so we are safe calling this .sdf
                     DELTA = DAT__SZFLX
                  END IF
               ELSE
*     '.sdf' is at end of string so absorb it
                  DELTA = DAT__SZFLX
               END IF

*     This is where we can really tell the difference with the .SDF component
*     vs .sdf file extension if we want to be rock solid. We assume that if the
*     file can not be opened, that the .sdf is part of the file name.
*     Move this IF to HDS_FIND if we are uncomfortable with the assumptions here.
               IF (DELTA .NE. 0) THEN
                  CALL EMS_MARK()
                  LSTAT = SAI__OK
                  CALL HDS_OPEN( NAME(F1:F2), 'READ', TESTLOC, LSTAT )
                  IF ( LSTAT .EQ. SAI__OK ) THEN
*     File opened okay, so now disambiguate (taking substring requires local
*     buffer).
                     FLEXT = DAT__FLEXT
                     CALL DAT_THERE( TESTLOC, FLEXT( 2 : DAT__SZFLX ),
     :                               THERE, LSTAT )
                     IF ( THERE ) THEN
                        DELTA = 0 ! it was a component!!
                     END IF
                     CALL DAT_ANNUL( TESTLOC, LSTAT )
                     IF ( LSTAT .NE. SAI__OK ) CALL EMS_ANNUL( LSTAT )
                  ELSE
*     Could not open file (assume that .sdf is part of the filename rather than
*     a component
                     CALL EMS_ANNUL( LSTAT )
                  END IF
                  CALL EMS_RLSE()
               END IF

               F2 = F2 + DELTA
               IEND = IEND + DELTA

            END IF

*  If no errors have occurred, then attempt to locate the path name
*  string.
            IF ( STATUS .EQ. SAI__OK ) THEN

*  If the file name extends to the end of the object name string, then
*  there is no path name (i.e. it is a top-level object), so return
*  a "null" position.
               IF ( IEND .GE. L ) THEN
                  P1 = 1
                  P2 = 0

*  Otherwise, eliminate any leading blanks from the candidate path name
*  which follows the file name and return the path name limits.
               ELSE
                  CALL CHR_FANDL( NAME( IEND + 1 : L ), ISTART, L )
                  P1 = ISTART + IEND
                  P2 = L + IEND
               END IF
            END IF
         END IF
      END IF

      END
