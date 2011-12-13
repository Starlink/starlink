      SUBROUTINE TRN1_EVTMR( BAD, ND1, NVIN, DATA, NDAT, INDEX,
     :                         NVOUT, NS1, NSTACK, STACK, IERR, NERR,
     :                         NSTAT, STATUS )
*+
*  Name:
*     TRN1_EVTMR

*  Purpose:
*     evaluate a REAL compiled transformation module.

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL TRN1_EVTMR( BAD, ND1, NVIN, DATA, NDAT, INDEX,
*                        NVOUT, NS1, NSTACK, STACK, IERR, NERR,
*                        NSTAT, STATUS )

*  Description:
*     The routine evaluates the effect of a compiled transformation
*     in transforming a set of data points.

*  Copyright:
*     Copyright (C) 1988 Science & Engineering Research Council.
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
*     R.F. Warren-Smith (DUVAD::RFWS)
*     {enter_new_authors_here}

*  History:
*     11-FEB-1988:  Original version (DUVAD::RFWS)
*     9-MAY-1988:  Converted to generic code (DUVAD::RFWS)
*     17-AUG-1988:  Added "bad value" handling (DUVAD::RFWS)
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-


*  Type Definitions:
      IMPLICIT NONE             ! No implicit typing


*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'TRN_CONST_STM'    ! TRN_ private constants for use by
                                 ! routines which process standard
                                 ! transformation modules


*  Arguments Given:
      LOGICAL BAD               ! Whether input data may be "bad"
      INTEGER ND1               ! First dimension of DATA array
      INTEGER NVIN              ! Number of input variables for the
                                ! transformation module
      REAL DATA( ND1, NVIN )  ! Array containing a list of the
                                ! coordinate values for each data point
      INTEGER NDAT              ! Number of data points to transform
      INTEGER INDEX( TRN_IN_NITEM )
                                ! Compiled transformation module index
                                ! array, containing pointers to the
                                ! mapped code and constants arrays
      INTEGER NVOUT             ! Number of output variables for the
                                ! transformation module
      INTEGER NS1               ! The first dimension of the arithmetic
                                ! stack array STACK - it must be at
                                ! least equal to NDAT
      INTEGER NSTACK            ! The number of vectors (second
                                ! dimension size) in the STACK array


*  Arguments Given and Returned:
*     <declarations and descriptions for imported/exported arguments>


*  Arguments Returned:
      REAL STACK( NS1, NSTACK )
                                ! Arithmetic stack used as workspace and
                                ! to hold the result vectors
      INTEGER IERR              ! Pointer to the first data point to
                                ! generate a numerical error
      INTEGER NERR              ! Number of numerical errors which occur
      INTEGER NSTAT             ! Numerical error status - identifies
                                ! the first error (pointed to by IERR)


*  Status:
      INTEGER STATUS            ! Error status


*  External References:
*     <declarations for external function references>


*  Global Variables:
*     <any INCLUDE files for global variables held in named COMMON>


*  Local Constants:
*     <local constants defined by PARAMETER>


*  Local Variables:
      INTEGER IVOUT             ! Loop counter for output variables
      INTEGER NCODE             ! Number of code elements consumed
      INTEGER NCONST            ! Number of constants consumed
      INTEGER ICODE             ! Total count of code elements consumed
      INTEGER ICONST            ! Total count of constants consumed
      INTEGER IERRL             ! Local numerical error pointer
      INTEGER NERRL             ! Local numerical error count
      INTEGER NSTATL            ! Local numerical error status


*  Internal References:
*     <declarations for internal functions>


*  Local Data:
*     <any DATA initialisations for local variables>


*.



*   Check status.
      IF( STATUS .NE. SAI__OK ) RETURN


*   Initialise the numerical error pointer, count and status variables.
      IERR = 0
      NERR = 0
      NSTAT = SAI__OK


*   Initialise the counts of code elements and constants consumed.
      ICODE = 0
      ICONST = 0


*   Loop to calculate the values for each output variable in turn.
      IVOUT = 0
      DO WHILE ( ( IVOUT .LT. NVOUT ) .AND. ( STATUS .EQ. SAI__OK ) )
        IVOUT = IVOUT + 1


*   Evaluate the compiled expression for each output variable,
*   incrementing the start position in the arithmetic stack each time
*   so that successive results occupy consecutive vectors at the start
*   of the stack.
        CALL TRN1_EVEXR( %VAL( CNF_PVAL( INDEX( TRN_IN_CODE ) ) ),
     :                   ICODE + 1,
     :                   %VAL( CNF_PVAL( INDEX( TRN_IN_CONST ) ) ),
     :                   ICONST + 1, BAD, ND1, NVIN, DATA, NDAT,
     :                   NS1, ( NSTACK - IVOUT + 1 ), STACK( 1, IVOUT ),
     :                   NCODE, NCONST, IERRL, NERRL, NSTATL, STATUS )

*   If there is no error flagged by STATUS, increment the consumed code
*   and constant counts.
        IF( STATUS .EQ. SAI__OK ) THEN
          ICODE = ICODE + NCODE
          ICONST = ICONST + NCONST


*   If a numerical error occurred (flagged by NSTATL), increment the
*   numerical error count.
          IF( NSTATL .NE. SAI__OK ) THEN
            NERR = NERR + NERRL


*   If this is the first numerical error, or if it occurred nearer the
*   beginning of the array of data points than any previous error, then
*   set the error pointer and the numerical error status.
            IF( ( NSTAT .EQ. SAI__OK ) .OR. ( IERRL .LT. IERR ) ) THEN
              IERR = IERRL
              NSTAT = NSTATL
            ENDIF
          ENDIF


*   End of "no error flagged by STATUS" condition.
        ENDIF


*   End of "calculate the values for each output variable..." loop.
      ENDDO


*   Exit routine.
      END


