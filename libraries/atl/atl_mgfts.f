      SUBROUTINE ATL_MGFTS( METHOD, FC1, FC2, FC3, STATUS )
*+
*  Name:
*     ATL_MGFTS

*  Purpose:
*     Merge two FITS headers.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ATL_MGFTS( METHOD, FC1, FC2, FC3, STATUS )

*  Description:
*     This routine merges two FITS headers, each supplied in an AST
*     FitsChan, in one of several different ways. The resulting merged
*     list of headers is returned in a new FitsChan. 

*  Arguments:
*     METHOD = INTEGER (Given)
*        Indicates how the two FITS headers should be merged:
*        
*        1 - Concatenation. Store the contents of FC1 in the returned 
*            FitsChan, and then append the contents of FC2 to the end of 
*            the returned FitsChan. No checks are made for multiple 
*            occurences of the same keyword.
*
*        2 - Union (with priority given to FC2): For every header in FC1, see 
*            if FC2 contains the same keyword. If it does not, copy the FC1 
*            header to the returned FitsChan. Then append the contents of FC2 
*            to the end of the returned FitsChan.
*
*        3 - Overlap: For every header in FC1, see if FC2 contains the same 
*            keyword. If it does, and if the keyword value is the same in
*            both FitsChans, copy the FC1 header to the returned FitsChan. 
*     FC1 = INTEGER (Given)
*        Pointer to the first FitsChan. 
*     FC2 = INTEGER (Given)
*        Pointer to the second FitsChan. 
*     FC3 = INTEGER (Returned)
*        Pointer to the returned FitsChan. 
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  The contents of FC1 and FC2 are unchanged on exit.
*     -  For METHOD 3 (overlap), floating point values are compared 
*     by formatting into a string (using the accuracy specified by the
*     FitsDigits attributes of the two supplied FitsChans) and then
*     comparing the formatted strings for exact equality.

*  Copyright:
*     Copyright (C) 2007 Particle Physics & Astronomy Research Council.
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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     7-FEB-2007 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST constants

*  Arguments Given:
      INTEGER METHOD
      INTEGER FC1
      INTEGER FC2

*  Arguments Returned:
      INTEGER FC3

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:

*  Local Variables:
      CHARACTER CARD*80
      CHARACTER VALUE2*80
      CHARACTER VALUE3*80

*.

*  Initialise.
      FC3 = AST__NULL

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Method 1: simply create a deep copy of FC1 and then append the
*  contents of FC2 to it.
      IF( METHOD .EQ. 1 ) THEN
         FC3 = AST_COPY( FC1, STATUS )
         CALL AST_SETI( FC3, 'CARD', 
     :                  AST_GETI( FC3, 'CARD', STATUS ) + 1, STATUS )
         DO WHILE( AST_FINDFITS( FC2, '%f', CARD, .TRUE., STATUS ) )
            CALL AST_PUTFITS( FC3, CARD, .FALSE., STATUS )
         END DO

*  Method 2: first create a deep copy of FC1 and then erase any keywords
*  from it that also occur in FC2 (even if they have different values). 
*  Finally append the contents of FC2.
      ELSE IF( METHOD .EQ. 2 ) THEN
         FC3 = AST_COPY( FC1, STATUS )
         CALL AST_CLEAR( FC3, 'CARD', STATUS )
         DO WHILE( AST_FINDFITS( FC3, '%f', CARD, .TRUE., STATUS ) )
            IF( AST_GETFITSS( FC2, CARD, VALUE2, STATUS ) ) THEN
               CALL AST_DELFITS( FC3, STATUS )
            END IF
         END DO

         DO WHILE( AST_FINDFITS( FC2, '%f', CARD, .TRUE., STATUS ) )
            CALL AST_PUTFITS( FC3, CARD, .FALSE., STATUS )
         END DO


*  Method 3: First create a deep copy of FC1. Then erase all headers 
*  from FC3 except for those which also occur in FC2 (i.e. have the same
*  keyword and the same value).
      ELSE IF( METHOD .EQ. 3 ) THEN
         FC3 = AST_COPY( FC1, STATUS )
         CALL AST_CLEAR( FC3, 'CARD', STATUS )
         DO WHILE( AST_FINDFITS( FC3, '%f', CARD, .TRUE., STATUS ) )
            IF( AST_GETFITSS( FC2, CARD, VALUE2, STATUS ) .AND.
     :          AST_GETFITSS( FC3, CARD, VALUE3, STATUS ) ) THEN
               IF( VALUE2 .NE. VALUE3 ) THEN
                  CALL AST_DELFITS( FC3, STATUS )
               END IF
            ELSE
               CALL AST_DELFITS( FC3, STATUS )
            END IF
         END DO

*  Report an error for any other method.
      ELSE IF( STATUS .EQ. SAI__OK ) THEN 
         STATUS = SAI__ERROR
         CALL MSG_SETI( 'M', METHOD )
         CALL ERR_REP( 'ATL_MGFTS_ERR1', 'ATL_MGFTS: Illegal METHOD'//
     :                 ' value (^M) supplied (programming error).', 
     :                 STATUS )
      END IF
    
      END
