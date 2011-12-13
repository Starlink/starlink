      SUBROUTINE NDF1_AMSG( TOKEN, IACB )
*+
*  Name:
*     NDF1_AMSG

*  Purpose:
*     Assign the name of an NDF identified in the ACB to a message
*     token.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_AMSG( TOKEN, IACB )

*  Description:
*     The routine assigns the full name (including the file name) of an
*     NDF to a message token for use with the ERR_ and MSG_ routines
*     (SUN/104). If an NDF section is supplied, then a dimension bound
*     expression is appended to the data object's name. The NDF is
*     identified by the index of its entry in the ACB.

*  Arguments:
*     TOKEN = CHARACTER * ( * ) (Given)
*        Name of the message token.
*     IACB = INTEGER * ( * ) (Given)
*        Index of the NDF entry in the ACB.

*  Notes:
*     -  This routine has no STATUS argument and does not perform
*     normal error checking. If it should fail, then no value will be
*     assigned to the message token and this will be apparent in the
*     final message.

*  Algorithm:
*     -  Obtain an index to the data object entry in the DCB.
*     -  Assign the name of the data object to the message token.
*     -  If this is an NDF section, then section bounds information
*     must be appended. Defer error reporting.
*     -  Obtain the section bounds and pixel offsets.
*     -  Convert the section bounds for each possible dimension into
*     the corresponding bounds in the base NDF by subtracting the pixel
*     offsets.
*     -  Note the last dimension for which the lower and upper bounds
*     are significant (i.e. not both equal to 1).
*     -  Start constructing the section bounds expression.
*     -  Loop to format the bounds for each dimension. Use the number
*     of dimensions in the NDF section, or the number of the last
*     dimension which has significant bounds, whichever is larger.
*     -  If we are displaying dimensions which lie outside the NDF
*     section, then prefix them with an additional parenthesis.
*     -  Format the lower bound.
*     -  Then add the upper bound, if different.
*     -  Add a closing parenthesis (plus an extra one if the number of
*     NDF section dimensions was exceeded).
*     -  Note whether any error has occurred, If so, then annul it.
*     -  End the error context.
*     -  If there were no errors, then append the section bounds
*     expression to the message token.

*  Copyright:
*     Copyright (C) 1990, 1991 Science & Engineering Research Council.
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
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     {enter_new_authors_here}

*  History:
*     14-NOV-1990 (RFWS):
*        Original version.
*     15-FEB-1991 (RFWS):
*        Added dimension bounds expression for NDF sections.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'NDF_PAR'          ! NDF_ public constants
      INCLUDE 'NDF_CONST'        ! NDF_ private constants
      INCLUDE 'PRM_PAR'          ! Primitive data constants

*  Global Variables:
      INCLUDE 'NDF_DCB'          ! NDF_ Data Control Block
*        DCB_DID( NDF__MXDCB ) = INTEGER (Read)
*           ARY_ system identifier for the NDF's data array.

      INCLUDE 'NDF_ACB'          ! NDF_ Access Control Block
*        ACB_CUT( NDF__MXACB ) = LOGICAL (Read)
*           Whether an NDF is a cut (i.e. section).
*        ACB_DID( NDF__MXACB ) = INTEGER (Read)
*           ARY_ system identifier for the NDF's data array.
*        ACB_IDCB( NDF__MXACB ) = INTEGER (Read)
*           Index to data object entry in the DCB.

*  Arguments Given:
      CHARACTER * ( * ) TOKEN
      INTEGER IACB

*  Local Variables:
      CHARACTER * ( NDF__MXDIM * ( 2 * VAL__SZI + 2 ) + 3 ) BUF
                                 ! Buffer for section bounds expression
      INTEGER I                  ! Lopp counter for dimensions
      INTEGER IDCB               ! Index to data object entry in the DCB
      INTEGER LBND( NDF__MXDIM ) ! Lower bound in base NDF pixel system
      INTEGER LBNDS( NDF__MXDIM ) ! Section lower bounds
      INTEGER N                  ! Number of significant dimensions
      INTEGER NC                 ! No. characters in buffer
      INTEGER NDIMS              ! Number of section dimensions
      INTEGER OFFS( NDF__MXDIM ) ! Pixel offsets for NDF section
      INTEGER STATUS             ! Local status variable
      INTEGER UBND( NDF__MXDIM ) ! Upper bound in base NDF pixel system
      INTEGER UBNDS( NDF__MXDIM ) ! Section upper bounds
      LOGICAL OK                 ! No errors occurred?

*.

*  Obtain an index to the data object entry in the DCB.
      IDCB = ACB_IDCB( IACB )

*  Assign the name of the data object to the message token.
      CALL NDF1_DMSG( TOKEN, IDCB )

*  If this is an NDF section, then section bounds information must be
*  appended. Defer error reporting.
      IF ( ACB_CUT( IACB ) ) THEN
         CALL ERR_MARK
         STATUS = SAI__OK

*  Obtain the section bounds and pixel offsets.
         CALL ARY_BOUND( ACB_DID( IACB ), NDF__MXDIM, LBNDS, UBNDS,
     :                   NDIMS, STATUS )
         CALL ARY_OFFS( DCB_DID( IDCB ), ACB_DID( IACB ), NDF__MXDIM,
     :                  OFFS, STATUS )
         IF ( STATUS .EQ. SAI__OK ) THEN

*  Convert the section bounds for each possible dimension into the
*  corresponding bounds in the base NDF by subtracting the pixel
*  offsets.
            N = 0
            DO 1 I = 1, NDF__MXDIM
               LBND( I ) = LBNDS( I ) - OFFS( I )
               UBND( I ) = UBNDS( I ) - OFFS( I )

*  Note the last dimension for which the lower and upper bounds are
*  significant (i.e. not both equal to 1).
               IF ( ( LBND( I ) .NE. 1 ) .OR.
     :              ( UBND( I ) .NE. 1 ) ) THEN
                  N = I
               END IF
 1          CONTINUE

*  Start constructing the section bounds expression.
            NC = 0
            CALL CHR_PUTC( '(', BUF, NC )

*  Loop to format the bounds for each dimension. Use the number of
*  dimensions in the NDF section, or the number of the last dimension
*  which has significant bounds, whichever is larger.
            DO 2 I = 1, MAX( NDIMS, N )
               IF ( I .GT. 1 ) THEN
                  CALL CHR_PUTC( ',', BUF, NC )
               END IF

*  If we are displaying dimensions which lie outside the NDF section,
*  then prefix them with an additional parenthesis.
               IF ( I .EQ. NDIMS + 1 ) THEN
                  CALL CHR_PUTC( '(', BUF, NC )
               END IF

*  Format the lower bound.
               CALL CHR_PUTI( LBND( I ), BUF, NC )

*  Then add the upper bound, if different.
               IF ( UBND( I ) .NE. LBND( I ) ) THEN
                  CALL CHR_PUTC( ':', BUF, NC )
                  CALL CHR_PUTI( UBND( I ), BUF, NC )
               END IF
 2          CONTINUE

*  Add a closing parenthesis (plus an extra one if the number of NDF
*  section dimensions was exceeded).
            IF ( N .GT. NDIMS ) THEN
               CALL CHR_PUTC( ')', BUF, NC )
            END IF
            CALL CHR_PUTC( ')', BUF, NC )
         END IF

*  Note whether any error has occurred, If so, then annul it.
         OK = STATUS .EQ. SAI__OK
         IF ( .NOT. OK ) THEN
            CALL ERR_ANNUL( STATUS )
         END IF

*  End the error context.
         CALL ERR_RLSE

*  If there were no errors, then append the section bounds expression
*  to the message token.
         IF ( STATUS .EQ. SAI__OK ) CALL MSG_SETC( TOKEN, BUF( : NC ) )
      END IF

      END
