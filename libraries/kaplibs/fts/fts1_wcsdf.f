      LOGICAL FUNCTION FTS1_WCSDF( ENCOD, FS1, FS2, STATUS )
*+
*  Name:
*     FTS1_WCSDF

*  Purpose:
*     Sees if two FrameSets are different after being written to a
*     FitsChan.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     RETURN = FTS1_WCSDF( ENCOD, FS1, FS2, STATUS )

*  Description:
*     The returned function value indicates if the two FrameSets are
*     inconsistent with each other. The two supplied FrameSets are written
*     to two FitsChans, using the specified encoding. All keyword values
*     in the two FitsChans are then compared. If any keyword has a
*     significantly different value in the two FitsChans, then the
*     FrameSets are inconsistent, and a .TRUE value is returned. Otherwise,
*     .FALSE. is returned.

*  Arguments:
*     ENCOD = CHARACTER * ( * ) (Given)
*        The encoding scheme to use when converting the supplied
*        FrameSets into FITS header cards.
*     FS1 = INTEGER (Given)
*        An AST pointer to the first FrameSet.
*     FS2 = INTEGER (Given)
*        An AST pointer to the second FrameSet.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Function Value:
*     FTS1_WCSDF = LOGICAL (Returned)
*        .TRUE. if the supplied FrameSets are inconsistent, and .FALSE.
*        otherwise.

*  Notes:
*     -  A value of .TRUE. is returned if an error has already occurred,
*     or if this function should fail for any reason.

*  Copyright:
*     Copyright (C) 1997 Central Laboratory of the Research Councils.
*     All Rights Reserved.

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
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     26-NOV-1997 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST public constants
      INCLUDE 'PRM_PAR'          ! PRIMDAT public constants (VAL__...)

*  Arguments Given:
      CHARACTER ENCOD*(*)
      INTEGER FS1
      INTEGER FS2

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Used length of a string

*  Local Variables:
      CHARACTER CARD1*80         ! FITS header card from 1st FitsChan
      CHARACTER CARD2*80         ! FITS header card from 2nd FitsChan
      DOUBLE PRECISION TOL       ! Largest allowed error
      DOUBLE PRECISION VALUE1    ! Numerical keyword value from CARD1
      DOUBLE PRECISION VALUE2    ! Numerical keyword value from CARD2
      INTEGER COM1               ! Index of start of comment in CARD1
      INTEGER COM2               ! Index of start of comment in CARD2
      INTEGER FC1                ! AST identifier first FitsChan
      INTEGER FC2                ! AST identifier second FitsChan
      INTEGER ISTAT              ! CHR status value
      INTEGER NMLEN              ! Length of the keyword
      INTEGER NOBJ1              ! No. of objects written to 1st FitsChan
      INTEGER NOBJ2              ! No. of objects written to 2nd FitsChan

*.

*  Initialise.
      FTS1_WCSDF = .TRUE.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Write the first FrameSet out to a FitsChan using the supplied encoding.
      FC1 = AST_FITSCHAN( AST_NULL, AST_NULL, ' ', STATUS )
      CALL AST_SETC( FC1, 'ENCODING', ENCOD, STATUS )
      NOBJ1 = AST_WRITE( FC1, FS1, STATUS )

* Do the same with the other FrameSet.
      FC2 = AST_FITSCHAN( AST_NULL, AST_NULL, ' ', STATUS )
      CALL AST_SETC( FC2, 'ENCODING', ENCOD, STATUS )
      NOBJ2 = AST_WRITE( FC2, FS2, STATUS )

*  If neither FrameSet could be written out, assume they are consistent
*  with each other.
      IF( NOBJ1 .EQ. 0 .AND. NOBJ2 .EQ. 0 ) THEN
         FTS1_WCSDF = .FALSE.

*  If one FrameSet could be written out and the other one could not, assume
*  they are inconsistent with each other.
      ELSE IF( NOBJ1 .NE. NOBJ2 ) THEN
         FTS1_WCSDF = .TRUE.

*  If both FrameSets could be written out, we look at the cards written
*  into the FitsChans to see if they are consistent.
      ELSE

*  Set up the fractional tolerance for equality between numerical values.
         TOL = 1.0D6*VAL__EPSD

*  Assume the FrameSets are consistent.
         FTS1_WCSDF = .FALSE.

*  Loop round examining each FITS header card describing the first FrameSet.
         CALL AST_CLEAR( FC1, 'Card', STATUS )
         DO WHILE( .NOT. FTS1_WCSDF .AND.
     :             AST_FINDFITS( FC1, '%f', CARD1, .TRUE., STATUS ) )

*  Ignore comment cards.
            IF( CARD1( 9 : 9 ) .EQ. '=' .AND.
     :          CARD1( : 8 ) .NE. ' ' .AND.
     :          CARD1( : 8 ) .NE. 'HISTORY' .AND.
     :          CARD1( : 8 ) .NE. 'COMMENT' ) THEN

*  Attempt to find the corresponding card for the second FrameSet. The
*  keyword name is contained in the first 8 columns of a FITS header card.
*  If the card is not found, the FrameSets differ.
               CALL AST_CLEAR( FC2, 'Card', STATUS )
               NMLEN = CHR_LEN( CARD1( : 8 ) )
               IF( .NOT. AST_FINDFITS( FC2, CARD1( : NMLEN ), CARD2,
     :                                 .TRUE., STATUS ) ) THEN
                  FTS1_WCSDF = .TRUE.

*  If the corresponding card was found, compare their values.
               ELSE

*  First find the start of any comments in the two cards.
                  COM1 = INDEX( CARD1, '/' )
                  IF( COM1 .EQ. 0 ) THEN
                     COM1 = 80
                  ELSE
                     COM1 = COM1 - 1
                  END IF

                  COM2 = INDEX( CARD2, '/' )
                  IF( COM2 .EQ. 0 ) THEN
                     COM2 = 80
                  ELSE
                     COM2 = COM2 - 1
                  END IF

*  Attempt to extract a floating point value from the keyword value
*  from the first FrameSet.
                  ISTAT = SAI__OK
                  CALL CHR_CTOD( CARD1( 10 : COM1) , VALUE1, ISTAT )

*  If succesful, attempt to extract a floating point value from the
*  second FrameSet's card.
                  IF( ISTAT .EQ. SAI__OK ) THEN

                     ISTAT = SAI__OK
                     CALL CHR_CTOD( CARD2( 10 : COM2 ) , VALUE2, ISTAT )

*  If this could not be done, the FrameSets differ.
                     IF( ISTAT .NE. SAI__OK ) THEN
                        FTS1_WCSDF = .TRUE.

*  Otherwise, compare the two floating point values. If they differ
*  significantly, the two FrameSets differ.
                     ELSE
                        FTS1_WCSDF = ( ABS( ( VALUE1 - VALUE2 ) ) .GT.
     :                                ABS( TOL*( VALUE1 + VALUE2 ) ) )
                     END IF

*  If a floating point value could not be extracted from the first
*  FrameSet's card, compare the keyword values as text.
                  ELSE
                     FTS1_WCSDF = ( CARD1( : COM1 ) .NE.
     :                             CARD2( : COM2 ) )
                  END IF

               END IF

            END IF

         END DO

      END IF

*  Return .TRUE. if an error has occurred.
      IF ( STATUS .NE. SAI__OK ) FTS1_WCSDF = .TRUE.

      END
