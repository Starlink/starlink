      SUBROUTINE ARD1_KEYW( TYPE, NEEDIM, NWCS, IWCS, WCSDAT, ELEM, L,
     :                      CFRM, IPOPND, IOPND, PNARG, SZOPND, NARG, I,
     :                      KEYW, STATUS )
*+
*  Name:
*     ARD1_KEYW

*  Purpose:
*     Read keyword argument lists

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARD1_KEYW( TYPE, NEEDIM, NWCS, IWCS, WCSDAT, ELEM, L, CFRM, IPOPND,
*                     IOPND, PNARG, SZOPND, NARG, I, KEYW, STATUS )

*  Description:
*     This routine reads argument lists associated with keyword fields,
*     and stores these arguments in the operand stack. The
*     routine will need to be called several times supplying new
*     elements each time if the argument list extends over more than
*     one element. A flag (KEYW) is returned to indicate when the
*     arguement list has been completed.

*  Arguments:
*     TYPE = INTEGER (Given)
*        An integer value identifying the current keyword.
*     NEEDIM = LOGICAL (Given)
*        Supplied .TRUE. if a DIMENSION statement is still needed to
*        establish the dimensionality of the ARD description. An error
*        is reported if this is supplied .TRUE.
*     NWCS = INTEGER (Given)
*        The number of axes in the user coord system.
*     IWCS = INTEGER (Given)
*        If AST__NULL, then the pixel->user mapping is linear. Otherwise,
*        IWCS is a pointer to an AST FrameSet containing just two Frames,
*        the Base frame is pixel coords, the current Frame is user coords.
*     WCSDAT( * ) = DOUBLE PRECISION (Given)
*        Supplied holding information which qualifies IWCS. If IWCS is
*        AST__NULL, then WCSDAT holds the coefficiets of the linear mapping
*        from pixel to user coords. Otherwise, wcsdat(1) holds a lower
*        limit on the distance (within the user coords) per pixel, and
*        the other elements in WCSDAT are not used.
*     ELEM = CHARACTER * ( * ) (Given)
*        The text of the current element of the ARD description.
*     L = INTEGER (Given)
*        The index of the last non-blank character in ELEM.
*     CFRM = INTEGER (Given)
*        Pointer to a Frame describing user coordinates.
*     IPOPND = INTEGER (Given and Returned)
*        A pointer to the one dimensional _double work array holding the
*        operand stack. The array is extended if necessary. See ARD1_LKR
*        for details of what is stored for each keyword.
*     IOPND = INTEGER (Given and Returned)
*        The index at which the next value is to be stored in the
*        operand stack.
*     PNARG = INTEGER (Given and Returned)
*        The number of values put onto the operand stack as a result of
*        the current keyword is itself stored on the operand stack at
*        the index supplied by PNARG. PNARG is incremented by one or more on
*        return.
*     SZOPND = INTEGER (Given and Returned)
*        The current size of the array pointed to by IPOPND.
*     NARG = INTEGER (Given and Returned)
*        The number of arguments read from the keyword argument list.
*        Supplied equal to -1 if a new argument list is being started.
*     I = INTEGER (Given and Returned)
*        The index within ELEM of the next character to be checked.
*     KEYW = LOGICAL (Returned)
*        Returned .FALSE. if all the arguments for the current keyword
*        have now been read. Returned .TRUE. otherwise.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1994 Science & Engineering Research Council.
*     Copyright (C) 2001, 2004 Central Laboratory of the Research Councils.
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
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     16-FEB-1994 (DSB):
*        Original version.
*     5-JUN-2001 (DSB):
*        Modified to use AST instead of coeff lists.
*     18-JUL-2001 (DSB):
*        Modified for ARD version 2.0.
*     6-DEC-2004 (DSB):
*        Previously, CC was assigned a literal question mark if
*        I was supplied equal to L. Not sure why this was done, but it
*        results in the last non blank character being ignored in the
*        supplied ELEM. The question mark has been replaced so that that
*        CC is assigned ELEM( I : I ) if I == L on entry.
*     1-OCT-2007 (DSB):
*        Check that NDIM is not 2 before reporting an error about needing
*        a DIMENSION statement.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'ARD_ERR'          ! ARD_ error constants
      INCLUDE 'AST_PAR'          ! AST constants and functions
      INCLUDE 'ARD_CONST'        ! ARD_ private constants

*  Global Variables:
      INCLUDE 'ARD_COM'          ! ARD common blocks
*        CMN_KWSYM( ARD__NKEYW)*( ARD__SZKEY ) = CHARACTER (Read)
*           The symbols used to represent each keyword.
*        CMN_KWARG( ARD__NKEYW ) = INTEGER (Read)
*           The number of arguments required by each keyword.

*  Arguments Given:
      INTEGER TYPE
      LOGICAL NEEDIM
      INTEGER NWCS
      INTEGER IWCS
      DOUBLE PRECISION WCSDAT( * )
      CHARACTER ELEM*(*)
      INTEGER L
      INTEGER IPOPND

*  Arguments Given and Returned:
      INTEGER IOPND
      INTEGER PNARG
      INTEGER SZOPND
      INTEGER NARG
      INTEGER I
      INTEGER CFRM

*  Arguments Returned:
      LOGICAL KEYW

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL ARD1_INIT         ! Initialise ARD common blocks

*  Local Variables:
      CHARACTER CC*1             ! Next character from ELEM
      DOUBLE PRECISION DAST      ! DOUBLE equivalence to variable JAST
      INTEGER IOPND0             ! Top of operand stack on entry
      INTEGER J                  ! Index into WCSDAT array
      INTEGER JAST(2)            ! AST FrameSet pointer equivalence

*  Make DAST and JAST share the same memory so that we can interpret
*  the real operand value as an integer identifier.
      EQUIVALENCE ( JAST, DAST )

*  Ensure that the local variable IOPND0 is saved between invocations
*  of this routine.
      SAVE IOPND0
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If this is a new argument list, all we do this time through is
*  initialise things and find the opening parenthesis which marks the
*  start of the argument list. The routine then exits and is called
*  again to continue reading the argument list.
      IF( NARG .EQ. -1 ) THEN

*  Report an error and abort if a dimension statement is still needed.
*  No dimension statement is needed if NWCS is 2 (because 2 is the
*  default dimensionality).
         IF( NEEDIM ) THEN
            IF( NWCS .NE. 2 ) THEN
               STATUS = ARD__BADDM
               CALL MSG_SETI( 'NWCS', NWCS )
               CALL ERR_REP( 'ARD1_KEYW_ERR1', 'ARD description is '//
     :                       'defaulting to 2-dimensions. It should '//
     :                       'be ^NWCS dimensional.', STATUS )
               GO TO 999
            ELSE
               NEEDIM = .FALSE.
            END IF
         END IF

*  Save the current index of the top of the operand stack.
         IOPND0 = IOPND

*  Find the next non-blank character in ELEM. This should be the opening
*  parenthesis which marks the start of the argument list.
         IF( I .LT. L ) THEN
            CC = ELEM( I : I )
            DO WHILE( CC .EQ. ' ' .AND. I .LT. L )
               I = I + 1
               CC = ELEM( I : I )
            END DO
         ELSE
            CC = ELEM( I : I )
         END IF

*  Increment the index of the next character to be checked so that it
*  refers to the first character after the opening parenthesis.
         I = I + 1

*  Indicate that the argument list has been started by setting the number of
*  arguments read so far to zero.
         NARG = 0

*  If the next non-blank character is an opening parenthesis, report an error
*  if the current keyword should not have an argument list.
         IF( CC .EQ. '(' ) THEN

            IF( CMN_KWARG( TYPE ) .EQ. 0 ) THEN
               STATUS = ARD__ARGS
               CALL ERR_REP( 'ARD1_KEYW_ERR2', 'Unnecessary argument '//
     :                       'list found.', STATUS )
            END IF

*  If some other non-blank character was found, an error is reported
*  unless the keyword should not have an argument list (in which case
*  decrement the index of the next character to be checked so that the
*  current character can be included as part of the next field).
         ELSE IF( CC .NE. ' ' ) THEN
            IF( CMN_KWARG( TYPE ) .EQ. 0 ) THEN
               KEYW = .FALSE.
               I = I - 1
            ELSE
               STATUS = ARD__ARGS
               CALL ERR_REP( 'ARD1_KEYW_ERR3', 'No argument list '//
     :                       'found.', STATUS )
            END IF
         END IF

*  If an argument list has been started...
      ELSE

*  ... call a routine to read the keyword argument list from the ARD
*  description and store appropriate values in the returned operand
*  array. Note, keywords which do not have an argument list are not
*  included in this list.

*  POINT and PIXEL keywords...
         IF( TYPE .EQ. ARD__POI .OR. TYPE .EQ. ARD__PIX ) THEN
            CALL ARD1_POIAR( NWCS, CFRM, ELEM, L, IPOPND, IOPND, SZOPND,
     :                       NARG, I, KEYW, STATUS )

*  LINE keywords...
         ELSE IF( TYPE .EQ. ARD__LIN ) THEN
            CALL ARD1_LINAR( NWCS, CFRM, ELEM, L, IPOPND, IOPND, SZOPND,
     :                       NARG, I, KEYW, STATUS )

*  ROW keywords...
         ELSE IF( TYPE .EQ. ARD__ROW ) THEN
            CALL ARD1_ROWAR( NWCS, CFRM, ELEM, L, IPOPND, IOPND, SZOPND,
     :                    NARG, I, KEYW, STATUS )

*  COLUMN keywords...
         ELSE IF( TYPE .EQ. ARD__COL ) THEN
            CALL ARD1_COLAR( NWCS, CFRM, ELEM, L, IPOPND, IOPND, SZOPND,
     :                    NARG, I, KEYW, STATUS )

*  BOX keywords...
         ELSE IF( TYPE .EQ. ARD__BOX ) THEN
            CALL ARD1_BOXAR( NWCS, CFRM, ELEM, L, IPOPND, IOPND, SZOPND,
     :                    NARG, I, KEYW, STATUS )

*  RECT keywords...
         ELSE IF( TYPE .EQ. ARD__REC ) THEN
            CALL ARD1_RECAR( NWCS, CFRM, ELEM, L, IPOPND, IOPND, SZOPND,
     :                    NARG, I, KEYW, STATUS )

*  ROTBOX keywords...
         ELSE IF( TYPE .EQ. ARD__ROT ) THEN
            CALL ARD1_ROTAR( NWCS, CFRM, ELEM, L, IPOPND, IOPND, SZOPND,
     :                    NARG, I, KEYW, STATUS )

*  POLYGON keywords...
         ELSE IF( TYPE .EQ. ARD__POL ) THEN
            CALL ARD1_POLAR( NWCS, CFRM, ELEM, L, IPOPND, IOPND, SZOPND,
     :                    NARG, I, KEYW, STATUS )

*  CIRCLE keywords...
         ELSE IF( TYPE .EQ. ARD__CIR ) THEN
            CALL ARD1_CIRAR( NWCS, CFRM, ELEM, L, IPOPND, IOPND, SZOPND,
     :                    NARG, I, KEYW, STATUS )

*  ELLIPSE keywords...
         ELSE IF( TYPE .EQ. ARD__ELL ) THEN
            CALL ARD1_ELLAR( NWCS, CFRM, ELEM, L, IPOPND, IOPND, SZOPND,
     :                    NARG, I, KEYW, STATUS )

*  FRAME keywords...
         ELSE IF( TYPE .EQ. ARD__FRA ) THEN
            CALL ARD1_FRAAR( NWCS, CFRM, ELEM, L, IPOPND, IOPND, SZOPND,
     :                    NARG, I, KEYW, STATUS )

*  If the operand requires no keywords, the argument list is complete.
         ELSE
             KEYW = .FALSE.
         END IF

*  If the argument list is complete, store the number of values added
*  to the operand stack, at the index supplied in argument PNARG.

         IF( .NOT. KEYW ) THEN
            CALL ARD1_STORD( DBLE( IOPND - IOPND0 ), SZOPND, PNARG,
     :                       IPOPND, STATUS )

*  Also store WCS Information in the operand stack. First deal with
*  linear Mappings...
            IF( IWCS .EQ. AST__NULL ) THEN

*  Store a flag value (1.0) to indicate that the mapping is linear.
               CALL ARD1_STORD( 1.0D0, SZOPND, IOPND, IPOPND, STATUS )

*  Store a pointer to the user coordinate Frame. In order to store an
*  integer AST pointer on the DOUBLE PRECISION operands stack, we make the
*  double precision DAST use the same memory as the two element integer
*  array JAST (using a Fortran EQUIVALENCE statement at the top of the
*  module). We then put the AST pointer value into the first element of
*  the integer array, and store the equivalent double precision value on
*  the stack.
               JAST( 1 ) = AST_CLONE( CFRM, STATUS )
               CALL AST_EXPORT( JAST( 1 ), STATUS )
               CALL ARD1_STORD( DAST, SZOPND, IOPND, IPOPND, STATUS )

*  Now store the transformation coefficients.
               DO J = 1, NWCS*( NWCS + 1 )
                  CALL ARD1_STORD( WCSDAT( J ), SZOPND, IOPND, IPOPND,
     :                             STATUS )
               END DO

*  Now deal with non-linear Mappings, store the FrameSet pointer and the
*  user distance per pixel.
            ELSE
               CALL ARD1_STORD( 0.0D0, SZOPND, IOPND, IPOPND, STATUS )
               JAST( 1 ) = AST_CLONE( IWCS, STATUS )
               CALL AST_EXPORT( JAST( 1 ), STATUS )
               CALL ARD1_STORD( DAST, SZOPND, IOPND, IPOPND, STATUS )
               CALL ARD1_STORD( WCSDAT( 1 ), SZOPND, IOPND, IPOPND,
     :                          STATUS )

            END IF

         END IF

      END IF

*  Jump to here if an error occurs.
 999  CONTINUE

*  Give a context message if an error has occurred.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL MSG_SETC( 'ELEM', ELEM )
         CALL MSG_SETC( 'KW', CMN_KWSYM( TYPE ) )
         CALL ERR_REP( 'ARD1_KEYW_ERR4', 'Unable to read argument '//
     :                 'list for a ^KW keyword in ARD description '//
     :                 '''^ELEM''.', STATUS )
      END IF

      END
