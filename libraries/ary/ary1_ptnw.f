      SUBROUTINE ARY1_PTNW( BAD, NDIM, LBNDA, UBNDA, ARRAY, LSUB,
     :                        USUB, LBNDD, UBNDD, HTYPE, LOC, DCE,
     :                        STATUS )
*+
*  Name:
*     ARY1_PTNW
 
*  Purpose:
*     Write WORD values to an n-dimensional subregion of an HDS
*     object.
 
*  Language:
*     Starlink Fortran 77
 
*  Invocation:
*     CALL ARY1_PTNW( BAD, NDIM, LBNDA, UBNDA, ARRAY, LSUB, USUB,
*     LBNDD, UBNDD, HTYPE, LOC, DCE, STATUS )
 
*  Description:
*     The routine writes to an n-dimensional subregion of a numeric HDS
*     array, taking the data from an n-dimensional subregion of a
*     WORD Fortran array and making use of lower and upper bounds
*     information for both arrays. Data type conversion is performed if
*     necessary, with bad pixel testing if required.
 
*  Arguments:
*     BAD = LOGICAL (Given)
*        Whether it is necessary to check for "bad" values during data
*        type conversions.
*     NDIM = INTEGER (Given)
*        Number of array (and HDS object) dimensions.
*     LBNDA( NDIM ) = INTEGER (Given)
*        Lower bounds of input array.
*     UBNDA( NDIM ) = INTEGER (Given)
*        Upper bounds of input array.
*     ARRAY( * ) = INTEGER*2 (Given)
*        Input WORD array.
*     LSUB( NDIM ) = INTEGER (Given)
*        Lower bounds of subregion to be written.
*     USUB( NDIM ) = INTEGER (Given)
*        Upper bounds of subregion to be written..
*     LBNDD( NDIM ) = INTEGER (Given)
*        Lower bounds of the HDS object.
*     UBNDD( NDIM ) = INTEGER (Given)
*        Upper bounds of the HDS object.
*     HTYPE = CHARACTER * ( * ) (Given)
*        The data type of the HDS object. This should be a primitive
*        numeric HDS data type string (case insensitive).
*     LOC = CHARACTER * ( * ) (Given)
*        Locator to HDS object.
*     DCE = LOGICAL (Returned)
*        Whether an error occurred during data type conversion.
*     STATUS = INTEGER (Given and Returned)
*        The global status.
 
*  Notes:
*     -  It is assumed that the input array and the output data object
*     have the same number of dimensions.  If this is not the case
*     intrinsically, then the NDIM argument should be set to match
*     whichever object has the larger dimensionality and the dimension
*     bounds of the other object (and possibly of the subregion also)
*     should be padded to match this dimensionality, normally with 1's.
*     It does not matter that the value of NDIM may not match the
*     actual dimensionality of the HDS object in such cases.
*     -  The lower and upper bounds of the subregion to be written must
*     lie within the bounds of both the input array and the output data
*     object, although the routine does not check for this.
*     -  The output data object must be suitable for vectorisation using
*     the HDS routine DAT_VEC.
 
*  Algorithm:
*     This routine is derived logically from a recursive treatment of
*     the problem of traversing an arbitrary number of array dimensions
*     whilst copying data to a subregion in each dimension. It may be
*     written schematically as follows...
*
*        procedure LOOP( I )
*           for DIM( I ) from LSUB( I ) to USUB( I ) do
*              if ( I = DCONTG ) then
*                 <copy a contiguous block of data>
*                 return
*              else
*                 LOOP( I - 1 )
*              end
*           end
*        end
*
*  A call of LOOP( NDIM ) then performs the entire data transfer
*  operation. The dimension DCONTG is chosen to be the highest
*  dimension where a contiguous block of data may be transferred (in
*  the worst case it will be 1).
*
*  Since Fortran does not allow recursive subroutine calls, they are
*  simulated here by branching back to the start of the algorithm,
*  having saved the previous dimension index in an appropriate element
*  of an array. A similar process (in reverse) is used to simulate a
*  return from the recursively invoked algorithm. To avoid branching
*  back into the range of a DO loop, looping has to be implemented
*  using IF and GO TO statements.
*
*  The algorithm operates as follows:
*     -  Check that the HTYPE argument is valid and convert it to upper
*     case. Report an error if it is not valid.
*     -  Initialise.
*     -  Find the highest dimension in which it is possible to transfer
*     contiguous data.
*     -  Set up strides for each dimension for both the input array and
*     the output data object.
*     -  Vectorise the output object and initialise pointers into the
*     input and output data streams.
*     -  Invoke the recursive algorithm.
*     -  Set pointers to the start and end of the data regions to be
*     skipped (in front of the subregion) in the current dimension.
*     -  If the current dimension allows contiguous data to be
*     transferred, then obtain a locator to a slice of the output data
*     object to receive the values to be transferred.
*     -  Transfer a contiguous block of data (with data type conversion
*     if necessary) and annul the slice locator.
*     -  If contiguous data cannot be transferred at the current
*     dimension, then invoke the algorithm again to handle the next
*     lower dimension.
*     -  Set pointers to the start and end of the data regions to be
*     skipped (after the subregion) in the current dimension.
*     -  Return from the recursive algorithm.
*     -  Annul the locator to the vectorised output object.
 
*  Copyright:
*     Copyright (C) 1989, 1990 Science & Engineering Research Council.
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
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}
 
*  History:
*     12-JUL-1989 (RFWS):
*        Original version.
*     16-AUG-1989 (RFWS):
*        Changed initialisation of locators to use global constant.
*     5-SEP-1989 (RFWS):
*        Fixed bug causing incorrect calculation of the number of
*        dimensions over which blocks of data are contiguous. Also
*        added message token to prevent '$' from affecting error
*        messages.
*     22-MAR-1990 (RFWS):
*        Added further explanation to the notes section.
*     {enter_further_changes_here}
 
*  Bugs:
*     {note_any_bugs_here}
 
*-
 
*  Type definitions:
      IMPLICIT NONE              ! No implicit typing
 
*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'ARY_PAR'          ! ARY_ public constants
      INCLUDE 'ARY_CONST'        ! ARY_ private constants
      INCLUDE 'ARY_ERR'          ! ARY_ error codes
 
*  Arguments Given:
      LOGICAL BAD
      INTEGER NDIM
      INTEGER LBNDA( NDIM )
      INTEGER UBNDA( NDIM )
      INTEGER*2 ARRAY( * )
      INTEGER LSUB( NDIM )
      INTEGER USUB( NDIM )
      INTEGER LBNDD( NDIM )
      INTEGER UBNDD( NDIM )
      CHARACTER * ( * ) HTYPE
      CHARACTER * ( * ) LOC
 
*  Arguments Returned:
      LOGICAL DCE
 
*  Status:
      INTEGER STATUS             ! Global status
 
*  Local variables:
      CHARACTER * ( ARY__SZTYP ) TYPE ! HDS data type
      CHARACTER * ( DAT__SZLOC ) SLICE ! Locator to data slice
      CHARACTER * ( DAT__SZLOC ) VEC ! Locator to vectorised data
      INTEGER DCONTG             ! Dimension containing contiguous data
      INTEGER DIM( ARY__MXDIM )  ! Array of dimension indices
      INTEGER EA                 ! End of input array region to skip
      INTEGER ED                 ! End of output data region to skip
      INTEGER ESLICE( 1 )        ! End of data slice
      INTEGER I                  ! (Current) dimension count
      INTEGER NCONTG             ! Number of contiguous data values
      INTEGER SSLICE( 1 )        ! Start of data slice
      INTEGER STRDA( ARY__MXDIM ) ! Dimension strides for input array
      INTEGER STRDD( ARY__MXDIM ) ! Dimension strides for data object
      LOGICAL CONTIG             ! Whether data values are contiguous
      LOGICAL DCESLC             ! Data conversion error in slice?
 
*.
 
*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN
 
*  Convert the HTYPE value to upper case, reporting an error if the
*  string supplied is too long.
      TYPE = HTYPE
      IF ( TYPE .EQ. HTYPE ) THEN
         CALL CHR_UCASE( TYPE )
      ELSE
         STATUS = ARY__FATIN
         CALL MSG_SETC( 'ROUTINE', 'ARY1_PTNW' )
         CALL MSG_SETC( 'BADHTYPE', HTYPE )
         CALL ERR_REP( 'ARY1_PTNW_TYP',
     :   'Routine ^ROUTINE called with an invalid HTYPE ' //
     :   'argument of ''^BADHTYPE'' (internal programming error).',
     :   STATUS )
         GO TO 9999
      END IF
 
*  Initialise the stride of dimension no. 1 for the input array and the
*  output object. (The stride for a dimension is the amount by which
*  the vectorised array index increases when the n-dimensional array
*  index for that dimension increases by 1.)
      STRDA( 1 ) = 1
      STRDD( 1 ) = 1
 
*  Calculate the stride for each remaining dimension.
      DO 11 I = 2, NDIM
         STRDA( I ) = STRDA( I - 1 ) *
     :                ( UBNDA( I - 1 ) - LBNDA( I - 1 ) + 1 )
         STRDD( I ) = STRDD( I - 1 ) *
     :                ( UBNDD( I - 1 ) - LBNDD( I - 1 ) + 1 )
11    CONTINUE
 
*  Initialise variables for finding the length of contiguous blocks of
*  data which can be transferred from the input array to the data
*  object.
      CONTIG = .TRUE.
      NCONTG = 1
 
*  Loop through each dimension.
      DO 12 I = 1, NDIM
 
*  If the data blocks to be transferred are contiguous over all lower
*  dimensions so far, then note the current dimension and calculate the
*  number of data elements found to be contiguous so far.  Test for
*  data being contiguous over the current dimension (i.e. not broken in
*  either the input array or the output data object).
         IF ( CONTIG ) THEN
            DCONTG = I
            NCONTG = NCONTG * ( USUB( I ) - LSUB( I ) + 1 )
            CONTIG = ( LSUB( I ) .EQ. LBNDA( I ) ) .AND.
     :               ( USUB( I ) .EQ. UBNDA( I ) ) .AND.
     :               ( LSUB( I ) .EQ. LBNDD( I ) ) .AND.
     :               ( USUB( I ) .EQ. UBNDD( I ) )
 
*  Quit looping once the data are no longer contiguous.
         ELSE
            GO TO 13
         END IF
12    CONTINUE
13    CONTINUE
 
*  Vectorise the output data object and initialise pointers into the
*  input and output vectorised arrays.
      VEC = ARY__NOLOC
      CALL DAT_VEC( LOC, VEC, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 9999
      EA = 0
      ED = 0
      DCE = .FALSE.
 
*  Recursive scanning of the array dimensions begins with the highest
*  dimension.
      I = NDIM
 
*  A recursive invocation of the algorithm starts here.
*  ===================================================
 
*  Increment pointers to the end of the data region which lies before
*  the lower bound of the subregion being written (in the current
*  dimension), and which is therefore NOT going to be copied.
2     CONTINUE
      EA = EA + ( LSUB( I ) - LBNDA( I ) ) * STRDA( I )
      ED = ED + ( LSUB( I ) - LBNDD( I ) ) * STRDD( I )
 
*  This is a "DO UNTIL" loop, which starts with the current dimension
*  set to the lower bound of the subregion and executes until it
*  goes beyond the upper bound.
      DIM( I ) = LSUB( I )
3     CONTINUE
      IF ( DIM( I ) .GT. USUB( I ) ) GO TO 5
 
*  If the data blocks to be transferred are contiguous over the current
*  dimension (and therefore all lower dimensions), then data can be
*  transferred.
         IF ( I .LE. DCONTG ) THEN
 
*  Locate the slice of the vectorised output object which is to receive
*  the block of contiguous data to be transferred.
            SSLICE( 1 ) = ED + 1
            ESLICE( 1 ) = SSLICE( 1 ) + NCONTG - 1
            SLICE = ARY__NOLOC
            CALL DAT_SLICE( VEC, 1, SSLICE, ESLICE, SLICE, STATUS )
 
*  Transfer the data from the appropriate part of the input array and
*  annul the slice. Note if a data conversion error occurred.
            CALL ARY1_PT1W( BAD, NCONTG, ARRAY( EA + 1 ), TYPE, SLICE,
     :                        DCESLC, STATUS )
            CALL DAT_ANNUL( SLICE, STATUS )
            SLICE = ARY__NOLOC
            IF ( STATUS .NE. SAI__OK ) GO TO 9999
            DCE = DCE .OR. DCESLC
 
*  Update the array pointers to refer to the next gap in the data
*  stream, after the block just transferred.
            EA = EA + NCONTG
            ED = ED + NCONTG
 
*  Update the dimension index to indicate that all of the subregion in
*  this dimension has now been processed.
            DIM( I ) = USUB( I )
 
*  The algorithm calls itself recursively here.
*  ===========================================
 
*  If data blocks are not contiguous over the current dimension, then
*  the algorithm invokes itself recursively to process the next lower
*  dimension. Decrement the current dimension count and branch back to
*  the start.
         ELSE
            I = I - 1
            GO TO 2
         END IF
 
*  The recursively invoked algorithm returns to this point.
*  =======================================================
4        CONTINUE
 
*  The current dimension count is "popped" back to its previous value
*  before the recursively invoked algorithm returns, so increment the
*  dimension index and branch to continue execution of the "DO UNTIL"
*  loop.
         DIM( I ) = DIM( I ) + 1
         GO TO 3
5        CONTINUE
 
*  Increment pointers to the end of the data region which lies after
*  the upper bound of the subregion being copied (in the current
*  dimension), and which is therefore NOT going to be transferred.
      EA = EA + ( UBNDA( I ) - USUB( I ) ) * STRDA( I )
      ED = ED + ( UBNDD( I ) - USUB( I ) ) * STRDD( I )
 
*  The recursively invoked algorithm returns from here.
*  ===================================================
 
*  "Pop" the current dimension count and make a return from a recursive
*  invocation of the algorithm (unless this is the top level invocation
*  - i.e. the current dimension count is equal to NDIM - in which case
*  all the data have been transferred, so make a final exit).
      IF ( I .GE. NDIM ) GO TO 6
      I = I + 1
      GO TO 4
6     CONTINUE
 
*  Annul the locator to the vectorised output object.
9999  CONTINUE
      CALL DAT_ANNUL( VEC, STATUS )
      VEC = ARY__NOLOC
 
*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL ARY1_TRACE( 'ARY1_PTNW',
     :STATUS )
 
      END
