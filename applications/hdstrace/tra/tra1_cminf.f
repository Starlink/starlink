      SUBROUTINE TRA1_CMINF( COMLOC, INDENT, CMNTYP, CMNVAL, NEWLIN,
     :                       NLINES, ONEPLN, LOGEXM, FD, LINE, NAME,
     :                       PRIM, SIZE, NDIM, DIMS, STATUS )
*+
*  Name:
*     TRA1_CMINF

*  Purpose:
*     Writes out information concerning a component of a structure.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL TRA1_CMINF( COMLOC, INDENT, CMNTYP, CMNVAL, NEWLIN, NLINES,
*                      ONEPLN, LOGEXM, FD, LINE, NAME, PRIM, SIZE, NDIM,
*                      DIMS, STATUS )

*  Description:
*     Obtains information on the specified structure component and
*     formats it into a one or more indented messages. The same text
*     may optionally be written to an open ASCII file.
*
*     The information comprises name, dimensions, type and value(s). See
*     TRA_PUTx for details of the formatting options and layout.  Data
*     returned are the name, type, size, dimensions, and whether the
*     object is primitive or not.

*  Arguments:
*     COMLOC = CHARACTER * ( DAT__SZLOC ) (Given)
*        Locator to the component.
*     INDENT = INTEGER (Given)
*        Indentation level for the message output.
*     CMNTYP = INTEGER (Given)
*        Indentation level for the type.
*     CMNVAL = INTEGER (Given)
*        Indentation level for the values.
*     NEWLIN = LOGICAL (Given)
*        If true the data values begin on a new line.
*     NLINES = INTEGER (Given)
*        Number of lines to store values.
*     ONEPLN = LOGICAL (Given)
*        If true the elements of a character array each appear on a
*        separate line.
*     LOGEXM = LOGICAL (Given)
*        If true a log of the header records is written to an ASCII
*        file.
*     FD = INTEGER (Given)
*        The ASCII file descriptor, ignored if LOGEXM is false.
*     LINE = CHARACTER * ( * ) (Given and Returned)
*        The string to which the primitive values are to be appended.
*     NAME = CHARACTER * ( DAT__SZNAM ) (Returned)
*        Name of the component.
*     PRIM = LOGICAL (Returned)
*        Set to true if the component is primitive.
*     SIZE = INTEGER (Returned)
*        Number of elements for the component if it is treated as a
*        vector.
*     NDIM = INTEGER (Returned)
*        Dimensionality of the component.
*     DIMS( DAT__MXDIM ) = INTEGER (Returned)
*        Dimensions of the component.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     If status is bad then return
*     Get all the information on this component
*     If no error then
*        Initialise the message line with indentation
*        Append the component name
*        Append the component dimensions
*        Compute type indentation, normally the selected value
*        Append the component type
*        Compute value indentation, normally the selcted value
*        If a structure or an array of structures then
*           Append informational message
*           Output line as the only token in a message
*           If file output then output line to the log
*        Else
*           If values to start on a new line then
*              Output the line (to a logfile if required) and
*                reinitialise the line buffer and indentation count
*           Endif
*           Write values for primitive types
*           Output line as the only token in a message.
*           If file output then output line to the log
*        Endif
*     Endif

*  Prior Requirements:
*     -  The ASCII file associated with descriptor FD must already be
*     opened.

*  Copyright:
*     Copyright (C) 1984, 1989, 1991 Science & Engineering Research
*     Council. All Rights Reserved.

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
*     DB: Dave Baines (ROE)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     15/03/1984 (DB):
*        Original version.
*     07/04/1984 (DB):
*        Revised to use LOCINF, PUTDIM, PUTVAL.
*     1989 May 10 (MJC):
*        Reordered type and name in output, and used <> syntax for
*        type, {} delimiters for commentary, put values of primitives
*        on a new line.
*     1989 May 16 (MJC):
*        Tidy up and added CMNTYP, CMNVAL, NEWLIN, NLINES, and LINE to
*        arguments.
*     1989 Jun 15 (MJC):
*        Renamed from COMINF to avoid confusion with the original TRACE
*        version; added ONEPLN, LOGEXM and FD arguments.
*     1991 January 30 (MJC):
*        Converted to the SST prologue.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE            ! Switch off the default typing

*  Global Constants:
      INCLUDE 'SAE_PAR'        ! Environment constants
      INCLUDE 'DAT_PAR'        ! Data-system constants

*  Arguments Given:
      CHARACTER * ( DAT__SZLOC )
     :  COMLOC                 ! Locator to a component.

      INTEGER
     :  CMNTYP,                ! Indentation for type
     :  CMNVAL,                ! Indentation for value
     :  INDENT,                ! Indentation for output information
                               ! message string
     :  NLINES,                ! Number of lines to be used for values
     :  FD                     ! Log file's description

      LOGICAL                  ! True if:
     :  LOGEXM,                ! Output to go to the log file
     :  NEWLIN,                ! Values start on a new line
     :  ONEPLN                 ! Elements of a character array each
                               ! appear on a new line

*  Arguments Given and Returned:
      CHARACTER*(*)
     :  LINE                   ! Message line string

*  Arguments Returned:
      CHARACTER*(DAT__SZNAM)
     :  NAME                   ! Component name

      LOGICAL                  ! True if:
     :  PRIM                   ! Component is primitive

      INTEGER
     :  SIZE,                  ! Number of elements for the component if
                               ! treated as a vector
     :  NDIM,                  ! Dimensionality of the component
     :  DIMS(DAT__MXDIM)       ! Dimensions of the component

*    Status return :
      INTEGER STATUS           ! the global status

*  External References:
      INTEGER CHR_LEN          ! String length ignoring trailing blanks
      INTEGER CHR_SIZE         ! String length including trailing blanks

*  Local Variables:
      CHARACTER*(DAT__SZTYP)
     :  TYPE                   ! Component type

      INTEGER
     :  LENG,                  ! Current position in the line string
     :  LNSIZE,                ! Line size
     :  I, J                   ! Character indices

*.

*    Check the global status.

      IF ( STATUS .NE. SAI__OK ) RETURN

      LNSIZE = CHR_SIZE( LINE )

*    Get all the information on this component.

      CALL TRA_LOCIN( COMLOC, NAME, PRIM, TYPE, SIZE, NDIM, DIMS,
     :                STATUS )

*    Check for an error.

      IF ( STATUS .EQ. SAI__OK ) THEN

*       Initialise the line (with indentation)

         LINE = ' '
         LENG = INDENT

*       Append the component name.

         I = CHR_LEN( NAME )
         CALL CHR_PUTC( NAME( 1:I ), LINE, LENG )

*       Append the component dimensions.

         IF ( NDIM .GT. 0 ) THEN
            CALL TRA_PUTDM( NDIM, DIMS, LINE, LENG, STATUS )
         END IF

*       Move to the column where the data type will be written, note
*       that there are at least two spaces following the name.

         LENG = MAX( LENG + 2, CMNTYP + INDENT )

*       Append the component type.

         I = LENG
         J = CHR_LEN( TYPE )
         CALL CHR_PUTC( '<'//TYPE( 1:J )//'>', LINE, I )

*       Move to the column where values will be written, note that
*       there are at least two spaces following the type.

         LENG = MAX( I + 2, CMNVAL + CMNTYP + INDENT )
         CALL CHR_PUTC( ' ', LINE, LENG )

*       Is the component primitive?

         IF ( PRIM ) THEN

*          Start on a new line?

            IF ( NEWLIN ) THEN

*             Output the line as the only token in a message.

               CALL MSG_SETC( 'LINE', LINE( 1:LENG ) )
               CALL MSG_OUT( 'TRA1_CMINF_MES1', '^LINE', STATUS )

*             Record in the log file.

               IF ( LOGEXM ) CALL FIO_WRITE( FD, LINE( 1:LENG ),
     :           STATUS )

*             Start a new line.

               LINE = ' '
               LENG = INDENT + 1
            END IF

*          Write the values for primitive types in the following line.

            CALL TRA_PUTVL( COMLOC, TYPE, NDIM, DIMS, SIZE, NLINES,
     :                      INDENT, ONEPLN, LOGEXM, FD, LINE, LENG,
     :                      STATUS )

*          Output the line as the only token in a message.

            CALL MSG_SETC( 'LINE', LINE(1:LENG) )
            CALL MSG_OUT( 'TRA1_CMINF_MES2', '^LINE', STATUS )

*          Record the line in the log file.

            IF ( LOGEXM ) CALL FIO_WRITE( FD, LINE( 1:LENG ), STATUS )

*       The component is a structure.

         ELSE
            IF ( SIZE .EQ. 1 ) THEN

*             Append "{structure}" for a scalar structure.

               CALL CHR_PUTC( '{structure}', LINE, LENG )
            ELSE

*             Append "{array of structures}" for an array of structures.

               CALL CHR_PUTC( '{array of structures}', LINE, LENG )
            END IF

*          Output the line as the only token in a message.

            CALL MSG_SETC( 'LINE', LINE(1:LENG) )
            CALL MSG_OUT( 'TRA1_CMINF_MES1', '^LINE', STATUS )

*          Record the line in the log file.

            IF ( LOGEXM ) CALL FIO_WRITE( FD, LINE( 1:LENG ), STATUS )
         END IF

      END IF

      END
