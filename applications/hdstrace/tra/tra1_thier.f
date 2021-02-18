      SUBROUTINE TRA1_THIER( LOC, INDENT, FULL, STEP, CMNTYP, CMNVAL,
     :                       NEWLIN, NLINES, ONEPLN, SORTED, LOGEXM,
     :                       FD, LINE, STATUS )
*+
*  Name:
*     TRA1_THIER

*  Purpose:
*     Traces an HDS object hierarchy.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL TRA1_THIER( LOC, INDENT, FULL, STEP, CMNTYP, CMNVAL, NEWLIN,
*                      NLINES, ONEPLN, SORTED, LOGEXM, FD, LINE, STATUS )

*  Description:
*     The routine recursively descends an HDS object hierarchy,
*     reporting the contents---names, types, dimensions and values---
*     to the user and optionally to an ASCII logfile.

*  Arguments:
*     LOC = CHARACTER * ( DAT__SZLOC ) (Given)
*        Locator for the object or structure whose contents are to be
*        traced.
*     INDENT = INTEGER (Given)
*        The indentation level for contents message and contents.
*     FULL = LOGICAL (Given)
*        If true then all elements of the array will be traced.
*     STEP = INTEGER (Given)
*        The indentation step between levels of the hierarchy.
*     CMNTYP = INTEGER (Given)
*        The Indentation level for the type.
*     CMNVAL = INTEGER (Given)
*        The indentation level for the values.
*     NEWLIN = LOGICAL (Given)
*        If true the data values begin on a new line.
*     NLINES = INTEGER (Given)
*        The number of lines to store values.
*     ONEPLN = LOGICAL (Given)
*        If true the elements of a character array each appear on a
*        separate line.
*     SORTED = LOGICAL (Given)
*        If true, list structure components in sorted order.
*     LOGEXM = LOGICAL (Given)
*        If true a log of the header records is written to an ASCII
*        file.
*     FD = INTEGER (Given)
*        The ASCII file descriptor, ignored if LOGEXM is false.
*     LINE = CHARACTER * ( * ) (Returned)
*        The line of text to be output.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     The routine implements a recursive algorithm to descend the HDS
*     structure, recursion being implemented by saving current values
*     on a stack and returning to the start of the routine. A
*     subsequent return from the recursively-invoked algorithm then
*     involves returning to the centre of the routine (following the
*     point of invocation) and popping the stack. This requires
*     branching back into the range of several loops, so all looping is
*     in this routine is implemented using GO TO statements rather than
*     DO loops.

*  Prior Requirements:
*     -  The ASCII file associated with descriptor FD must already be
*     opened for write access.

*  Copyright:
*     Copyright (C) 1992 Science & Engineering Research Council. All
*     Rights Reserved.

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
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1992 September 24 (MJC):
*        Original version based on RFWS's KPG1_NACVT.
*     2021 February 17 (GSB):
*        Addition of option for sorting structure components.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! Data-system constants

*  Arguments Given:
      CHARACTER * ( * ) LOC      ! Locator to the structure

      INTEGER CMNTYP             ! Indentation for type
      INTEGER CMNVAL             ! Indentation for value
      INTEGER INDENT             ! Indentation level for text output
      INTEGER NLINES             ! Number of lines to be used for values
      INTEGER STEP               ! Indentation step between levels
      INTEGER FD                 ! Log file's descriptor

                                 ! True if:
      LOGICAL FULL               ! Full tracing of arrays of structures
      LOGICAL LOGEXM             ! Output to go to the log file
      LOGICAL NEWLIN             ! Values start on a new line
      LOGICAL ONEPLN             ! Elements of a character array each
                                 ! appear on a new line
      LOGICAL SORTED             ! Structure components to be sorted?

*  Arguments Returned:
      CHARACTER * ( * ) LINE     ! Line string

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_SIZE           ! String length including trailing
                                 ! blanks

*  Local Constants:
      INTEGER MXSTK              ! Recursion stack size
      PARAMETER ( MXSTK = 200 )

      INTEGER MXSORT             ! Maximum sortable components
      PARAMETER ( MXSORT = 100 )

*  Local Variables:
      CHARACTER * ( DAT__SZLOC ) LCELL( MXSTK ) ! Array cell locator
      CHARACTER * ( DAT__SZLOC ) LCMP( MXSTK ) ! Component locator
      CHARACTER * ( DAT__SZLOC ) LSTART( MXSTK ) ! Initial locator
      CHARACTER * ( DAT__SZLOC ) LVEC( MXSTK ) ! Vectorised locator
      CHARACTER * ( DAT__SZNAM ) NAME ! Name of the component
      CHARACTER * ( DAT__SZTYP ) TYPE ! Type of the component
      INTEGER DIM( 1 )           ! Cell index
      INTEGER CDIMS( DAT__MXDIM )! Component dimension indices
      INTEGER DIMS( DAT__MXDIM ) ! Component dimensions
      INTEGER EL( MXSTK )        ! Number of array elements
      INTEGER ICMP( MXSTK )      ! Component index
      INTEGER IEL( MXSTK )       ! Array element index
      INTEGER LENG               ! Index into message line
      INTEGER LNSIZE             ! Length of message buffer
      INTEGER NDIM               ! Number of component dimensions
      INTEGER SINDEN             ! Number of columns to indent for an
                                 ! object
      INTEGER SIZE               ! Size as if vector
      INTEGER STK                ! Recursion stack pointer
      INTEGER NCMP( MXSTK )      ! Number of structure components
      LOGICAL BLANK              ! Report a blank line after the
                                 ! structure has been traced?
      LOGICAL LAST               ! Final object in the structure?
      LOGICAL PRIM               ! Object primitive?
      LOGICAL SARRAY( MXSTK )    ! Object an array of structures?

      CHARACTER * ( DAT__SZNAM ) THISCMPNAM ! Sorting comp. name
      CHARACTER * ( DAT__SZNAM ) THATCMPNAM ! Sorting comp. name
      CHARACTER * ( DAT__SZNAM ) CMPNAME( MXSORT ) ! Component names
      INTEGER SORTI                   ! Sorting operation index
      INTEGER SORTO( MXSTK, MXSORT )  ! Sort ordering array
      INTEGER SORTX                   ! Sort swap variable
      LOGICAL SWAPPED                 ! Did a swap occur?
      INTEGER THISICMP                ! Current component index

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the stack pointer and copy the input locator.
      STK = 1
      LSTART( STK ) = LOC

*  Initialise the indentation and last flag.
      SINDEN = MAX( 0, INDENT - STEP )
      LAST = .FALSE.
      PRIM = .FALSE.

*  Find the length of the output line buffer.
      LNSIZE = CHR_SIZE( LINE )

*  An invocation of the basic algorithm starts here.  Check the
*  inherited status.
 1    CONTINUE
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Get the component information and write it out.  Do not write it
*  for the first object.  This is done by HDSTRACE itself.
         IF ( LSTART( STK ) .NE. LOC ) THEN
            CALL TRA1_CMINF( LSTART( STK ), SINDEN, CMNTYP, CMNVAL,
     :                       NEWLIN, NLINES, ONEPLN, LOGEXM, FD, LINE,
     :                       NAME, PRIM, SIZE, NDIM, DIMS, STATUS )
         END IF

         IF ( STATUS .EQ. SAI__OK ) THEN

*  See if the initial object is primitive.
            IF ( PRIM ) THEN

*  When a structure or structure element has been completed the routine
*  will later output a blank record.  However, we do not want several
*  blank lines after several successive ends of structures.  There is
*  a flag to note when a blank line has been output.  Since the current
*  object is a primitive we want to switch the flag off as we do want a
*  blank line after the end of the current structure.
               BLANK = .FALSE.

*  If it is a structure or a structure array, then vectorise it and
*  determine how many elements it has.
            ELSE
               CALL DAT_VEC( LSTART( STK ), LVEC( STK ), STATUS )
               CALL DAT_SIZE( LVEC( STK ), EL( STK ), STATUS )
               IF ( STATUS .EQ. SAI__OK ) THEN

*  Test to see if it is a structure array.
                  SARRAY( STK ) = EL( STK ) .GT. 1

*  It's a structure so increase the indentation, but for the moment
*  only for a scalar structure.  This will be done later for structure
*  arrays as it must be applied for each component.
                  IF ( .NOT. SARRAY( STK ) ) SINDEN = SINDEN + STEP

*  The user may have requested that only the first element of the array
*  of structures be traced.  Reset the number of elements to one in this
*  case.
                  IF ( .NOT. FULL ) EL( STK ) = 1

*  Loop to process each array element.
                  IEL( STK ) = 0
 2                CONTINUE       ! Start of "DO WHILE" loop
                  IF ( ( IEL( STK ) .LT. EL( STK ) ) .AND.
     :                 ( STATUS .EQ. SAI__OK ) ) THEN
                     IEL( STK ) = IEL( STK ) + 1

*  Need extra output for an array of structures.
                     IF ( SARRAY( STK ) ) THEN

*  Get all necessary information on this object.
                        CALL TRA_LOCIN( LSTART( STK ), NAME, PRIM,
     :                                  TYPE, SIZE, NDIM, DIMS, STATUS )

*  Get the dimension indices for this element.
                        CALL TRA_ARELM( IEL( STK ), NDIM, DIMS,
     :                                  CDIMS, STATUS )

*  It's a structure so increase the indentation.
                        SINDEN = SINDEN + STEP

*  Write out the "Contents of" message.
                        CALL TRA1_TRCON( NAME, NDIM, CDIMS, STEP,
     :                                   SINDEN, LOGEXM, FD, LINE,
     :                                   STATUS )
                     END IF

*  Obtain a locator for each array cell and determine how many
*  components it has.
                     DIM( 1 ) = IEL( STK )
                     CALL DAT_CELL( LVEC( STK ), 1, DIM,
     :                              LCELL( STK ), STATUS )
                     CALL DAT_NCOMP( LCELL( STK ), NCMP( STK ), STATUS )
                     IF ( STATUS .EQ. SAI__OK ) THEN

*  Is the structure empty?
                        IF ( NCMP( STK ) .LE. 0 ) THEN

*  The structure is empty so output an appropriate message.  Note the
*  line buffer is initialised and the message is indented.
                           LINE = ' '
                           LENG = SINDEN
                           IF ( LENG + 20 .LE. LNSIZE )
     :                        CALL CHR_PUTC( '{structure is empty}',
     :                                       LINE, LENG )
                           CALL MSG_SETC( 'EMPTY', LINE )
                           CALL MSG_OUT( 'STR_EMPTY', '^EMPTY', STATUS )
                           CALL MSG_BLANK( STATUS )

*  Record the line in the log file if requested.
                           IF ( LOGEXM ) THEN
                              CALL FIO_WRITE( FD, LINE( 1:LENG ),
     :                                        STATUS )
                              CALL FIO_WRITE( FD, ' ', STATUS )
                           END IF

*  It's the end of a structure so reduce the indentation.
                           SINDEN = SINDEN - STEP

                        ELSE

*  If sorting was requested, and this component is small enough,
*  prepare sorting information: first extract names, then sort.
                           IF ( SORTED .AND. ( NCMP( STK ) .LE.
     :                          MXSORT ) ) THEN
                              SORTI = 0
                              DO WHILE ( ( SORTI .LT. NCMP( STK ) )
     :                                   .AND. ( STATUS .EQ. SAI__OK ) )
                                 SORTI = SORTI + 1
                                 CALL DAT_INDEX( LCELL( STK ), SORTI,
     :                                           LCMP( STK ), STATUS )

                                 CALL DAT_NAME( LCMP( STK ), THISCMPNAM,
     :                                          STATUS )
                                 CMPNAME( SORTI ) = THISCMPNAM
                                 SORTO( STK, SORTI ) = SORTI

                                 CALL DAT_ANNUL( LCMP( STK ), STATUS )
                              END DO

                              SWAPPED = .TRUE.
                              DO WHILE ( SWAPPED .AND.
     :                                   ( STATUS .EQ. SAI__OK ) )
                                 SWAPPED = .FALSE.
                                 SORTI = 1
                                 DO WHILE ( SORTI .LT. NCMP( STK ) )
                                    SORTI = SORTI + 1
                                    THISCMPNAM = CMPNAME(
     :                                 SORTO ( STK, SORTI ) )
                                    THATCMPNAM = CMPNAME(
     :                                 SORTO ( STK, SORTI - 1 ) )
                                    IF ( THATCMPNAM .GT. THISCMPNAM )
     :                                    THEN
                                       SORTX = SORTO ( STK, SORTI )
                                       SORTO ( STK, SORTI ) = SORTO (
     :                                    STK, SORTI - 1 )
                                       SORTO ( STK, SORTI - 1 ) = SORTX
                                       SWAPPED = .TRUE.
                                    END IF
                                 END DO
                              END DO
                           END IF

*  Loop to process each structure component, obtaining a locator for
*  it.
                           ICMP( STK ) = 0
 3                         CONTINUE ! Start of "DO WHILE" loop
                           IF ( ( ICMP( STK ) .LT. NCMP( STK ) ) .AND.
     :                          ( STATUS .EQ. SAI__OK ) ) THEN
                              ICMP( STK ) = ICMP( STK ) + 1
                              IF ( SORTED .AND. ( NCMP( STK ) .LE.
     :                             MXSORT ) ) THEN
                                 THISICMP = SORTO( STK, ICMP( STK ) )
                              ELSE
                                 THISICMP = ICMP( STK )
                              END IF
                              CALL DAT_INDEX( LCELL( STK ), THISICMP,
     :                                        LCMP( STK ), STATUS )

*  We must now recursively invoke the original algorithm to convert the
*  resulting object (which may be a primitive, a structure or a
*  structure array).  Check that the stack pointer will not overflow.
                              IF ( STK .GE. MXSTK ) THEN
                                 STATUS = SAI__ERROR
                                 CALL DAT_MSG( 'OBJECT', LCMP( STK ) )
                                 CALL MSG_SETI( 'MXSTK', MXSTK )
                                 CALL ERR_REP( 'TRA1_THIER_2DEEP',
     :   'Unable to convert the HDS object ^OBJECT to native ' //
     :   'data representation - object is nested more than ^MXSTK ' //
     :   'levels deep.', STATUS )
                              ELSE

*  Copy the component locator for use as the initial locator in the
*  next invocation. Then increment the stack pointer and branch back to
*  the start.
                                 LSTART( STK + 1 ) = LCMP( STK )
                                 STK = STK + 1
                                 GO TO 1

*  Arrive back here after returning from a recursive invocation of the
*  algorithm. Decrement the stack pointer.
 4                               CONTINUE
                                 STK = STK - 1


*  Record whether or not this is the last object within the structure.
                                 LAST = ICMP( STK ) .EQ. NCMP( STK )

*  When a structure is complete insert a blank line, unless there is
*  already a blank line.  There is an exception, and that is after a
*  structure-array element, but not the last. (This is because a blank
*  line is inserted before "Contents of ".  Put the blank line in the
*  logfile, if one is requested.
                                 IF ( LAST .AND. .NOT. BLANK .AND.
     :                                 .NOT. ( SARRAY( STK ) .AND.
     :                                IEL( STK ) .NE. EL( STK ) ) ) THEN

                                    CALL MSG_BLANK( STATUS )
                                    IF ( LOGEXM )
     :                                CALL FIO_WRITE( FD, ' ', STATUS )

*  Record the blank line.
                                    BLANK = .TRUE.
                                 END IF

*  Reset the indentation since the last object in the structure has
*  been reported.
                                 IF ( LAST ) THEN
                                    SINDEN = SINDEN - STEP
                                 END IF

*  Annul the (new) component locator.
                                 CALL DAT_ANNUL( LSTART( STK + 1 ),
     :                                           STATUS )
                              END IF

*  Return to process the next component,
                              GO TO 3
                           END IF   ! End of loop
                        END IF

*  Annul the array element locator and return to process the next
*  element.
                        CALL DAT_ANNUL( LCELL( STK ), STATUS )
                        GO TO 2

                     END IF         ! End of loop
                  END IF

*  Annul the vectorised array locator.
               END IF
               CALL DAT_ANNUL( LVEC( STK ), STATUS )
            END IF
         END IF
      END IF

*  After completing an invocation of the basic algorithm, decrement the
*  stack pointer and return to the point where it was invoked. If this
*  is the top-level invocation, then exit.
      IF ( STK .GT. 1 ) GO TO 4

*  Return the final locator.
      LOC = LSTART( STK )

      END
