      SUBROUTINE SUBPAR_ARRAY( NAMECODE, CMDLINE, LOC, STATUS )
*+
*  Name:
*     SUBPAR_ARRAY

*  Purpose:
*     parse an array in a command line

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SUBPAR_ARRAY( NAMECODE, CMDLINE, LOC, STATUS )

*  Description:
*     Parse an array on an ADAM command line and enter the values
*     into the parameter system. It is assumed that LEX_CMDLINE has already
*     found the opening [.
*     For non-'internal' parameters, a locator to the private storage will
*     be returned
*  Arguments:
*     NAMECODE = INTEGER (given)
*        The namecode of the parameter
*     CMDLINE = CHARACTER*(*) (given)
*        The command line to be parsed (already started)
*     LOC = CHARACTER*(*) (returned)
*        Locator to parameter file component
*     STATUS = INTEGER

*  Algorithm:
*     Assumes that LEX_CMDLINE has found the opening [.
*     Each call to LEX_CMDLINE will return either
*     A parameter which must be something that can be interpreted as a
*     primitive item, or a begin array or end array flag. The latter
*     are handled by adjusting the variable LEVEL to keep track of the
*     current level in the array structure.

*  Copyright:
*     Copyright (C) 1992, 1993 Science & Engineering Research Council.
*     Copyright (C) 1995, 1998 Central Laboratory of the Research Councils.
*     Copyright (C) 2008 Science & Technology Facilities Council.
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
*     JAB: J A  Bailey  (AAO)
*     AJC: A J Chipperfield (STARLINK)
*     DSB: David S Berry (JAC,UCLan)

*  History:
*     15-JUL-1992 (AJC):
*        Original version - extracted from SUBPAR_CMDLINE
*     16-JUL-1992 (AJC):
*        Slightly modified
*        Return locator to private storage
*        Use same PUT for internals as normal to avoid recursion via PUTNC
*     10-MAR-1993 (AJC):
*        Add DAT_PAR for SUBPAR_CMN
*      7-MAY-1993 (AJC):
*        Fix up D exponents so HDS can handle them
*     28-JUN-1995 (AJC):
*        Move DATA statement to comply with standard
*     20-MAY-1998 (AJC):
*        Use stated length (SLEN) of STRING (Linux problem).
*     30-OCT-2008 (DSB):
*        Do not annul the error caused by hitting the end of string
*        (LEX__ENDPARSE) as this indicates that the end of the array has
*        not been located correctly.
*     28-JUL-2017 (DSB):
*        Use MESSYS__VAL_LEN (now 1944) to define the max length of a
*        command line, instead of the local parameter MCLENGTH (was 444).
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'SUBPAR_ERR'
      INCLUDE 'SUBPAR_PAR'
      INCLUDE 'LEX_ERR'
      INCLUDE 'LEX_PAR'
      INCLUDE 'MESSYS_PAR'

*  Arguments Given:
      INTEGER NAMECODE
      CHARACTER*(*) CMDLINE

*  Arguments Returned:
      CHARACTER*(*) LOC

*  Status:
      INTEGER STATUS

*  External Routines:
      INTEGER STRING_IANYL       ! Index character within string

*  Global Variables:
      INCLUDE 'SUBPAR_CMN'

*  Local Constants:
      INTEGER MAXCOMPS           ! maximum number of array components on
      PARAMETER (MAXCOMPS=100)   ! a command line
      INTEGER MAXDIMS            ! maximum number of array dimensions
      PARAMETER (MAXDIMS=7)

*  Local Variables:
      INTEGER ACTION              ! Action code from parser
      CHARACTER*(MESSYS__VAL_LEN) STRING ! Parameter string from parser
      INTEGER SLEN                ! length of above
      INTEGER I, J                ! Loop counters
      INTEGER TYPE                ! type code for parameter
      CHARACTER*15 HDSTYPE        ! HDS type string
      INTEGER DIMS(MAXDIMS)       ! Array dimensions
      INTEGER NDIMS               ! Number of array dimensions
      CHARACTER*132 ARRAY(MAXCOMPS)  ! Array components
      INTEGER COUNT(MAXDIMS)      ! Number of array components so far
                                  ! at each level
      INTEGER LEVEL               ! Current level in array
      INTEGER COMP                ! Current array component
      INTEGER STAT                ! Temporary status
      INTEGER TACTION             ! Saved action code to derive
                                  ! type for array
      CHARACTER*15 POSTYPES(8)    ! Possible primitive data types
      LOGICAL LVALUE              ! Conversion of logical constant

*  Local Data:
      DATA POSTYPES/'_CHAR*132', '_REAL', '_DOUBLE', '_INTEGER',
     :     '_LOGICAL', ' ', ' ', '_INT64' /

*.


      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialize variables for array processing
      DO I = 1, MAXDIMS
         DIMS(I) = -1
      END DO
      LEVEL = 1
      COMP = 1
      COUNT(LEVEL) = 0
      NDIMS = 1
      CALL LEX_CMDLINE( .FALSE., CMDLINE, ACTION, STRING, SLEN, STATUS )


*  Array handling loop - each call to LEX_CMDLINE will return either
*  A parameter which must be something that can be interpreted as a
*  primitive item, or a begin array or end array flag. The latter
*  are handled by adjusting the variable LEVEL to keep track of the
*  current level in the array structure.

      DO WHILE ( ( LEVEL .GE. 1 ) .AND. ( STATUS .EQ. SAI__OK ) )

*     First resolve ambiguities
         IF ( ( ACTION .EQ. LEX__AMBIG )
     :   .OR. ( ACTION .EQ. LEX__KAMBIG ) ) THEN

*        It could be an Unquoted String or Logical Constant
            TYPE = MOD( PARTYPE(NAMECODE), 10 )

*        Check for literal string
            IF ( TYPE .EQ. SUBPAR__CHAR ) THEN
               ACTION = LEX__STRING

*        Otherwise attempt to convert the string to logical
            ELSE IF ( ( TYPE .EQ. SUBPAR__LOGICAL )
     :           .OR. ( TYPE .EQ. SUBPAR__NOTYPE ) ) THEN
               STAT = SAI__OK
               CALL CHR_CTOL( STRING(1:SLEN), LVALUE, STAT )

*           If it can be interpreted as a logical constant, set standard values
               IF ( STAT .EQ. SAI__OK ) THEN
                  ACTION = LEX__LOGICAL
                  SLEN = 1
                  IF ( LVALUE ) THEN
                     STRING = 'T'
                  ELSE
                     STRING = 'F'
                  END IF

               END IF

            END IF

         END IF

*  Now handle resolved items
         IF ( ACTION .EQ. LEX__STARR ) THEN
*     BEGIN Array ([) -  Increment LEVEL and NDIMS and zero COUNT for new level
            LEVEL = LEVEL + 1
            NDIMS = LEVEL
            IF ( LEVEL .GT. MAXDIMS ) THEN
               STATUS = SUBPAR__ARRDIM
               CALL EMS_SETI( 'MAXDIMS', MAXDIMS )
               CALL EMS_REP( 'SUP_ARRAY1',
     :                      'SUBPAR: The maximum allowed number of '//
     :                      'array dimensions is ^MAXDIMS', STATUS )
            ELSE
               COUNT(LEVEL) = 0
            END IF

         ELSE IF ( ACTION .EQ. LEX__ENDARR ) THEN
*     END Array (]) -  Check that number of components found at this level
*                   matches any previous number of components at this level.
*                   Then decrement LEVEL and increment COUNT for new level.
            IF ( DIMS(LEVEL) .EQ. -1 ) THEN
               DIMS(LEVEL) = COUNT(LEVEL)
            ELSE IF ( COUNT(LEVEL) .NE. DIMS(LEVEL) ) THEN
               STATUS = SUBPAR__ARRDIM
               CALL EMS_REP( 'SUP_ARRAY2',
     :                      'SUBPAR: Inconsistent number of array '//
     :                      'elements at same level', STATUS )
            END IF
            LEVEL = LEVEL - 1
            IF ( LEVEL .GE. 1 ) COUNT(LEVEL) = COUNT(LEVEL) + 1

         ELSE IF ( ( ACTION .EQ. LEX__STRING )
     :        .OR. ( ACTION .EQ. LEX__INTEGER )
     :        .OR. ( ACTION .EQ. LEX__REAL )
     :        .OR. ( ACTION .EQ. LEX__DOUBLE )
     :        .OR. ( ACTION .EQ. LEX__LOGICAL ) ) THEN
*     Primitive item  - copy its string into ARRAY
            ARRAY(COMP) = STRING(1:SLEN)
            IF ( COMP .EQ. 1 ) TACTION = ACTION
            COMP = COMP + 1
            COUNT(LEVEL) = COUNT(LEVEL) + 1
         ELSE
            STATUS = SUBPAR__ILARCOMP
            CALL EMS_SETC( 'STRING', STRING(1:SLEN) )
            CALL EMS_REP( 'SUP_ARRAY3',
     :                   'SUBPAR: Illegal array component /^STRING/',
     :                   STATUS )
         END IF

*  If not at end of array, get next element
         IF ( LEVEL .GE. 1 ) CALL LEX_CMDLINE( .FALSE., CMDLINE, ACTION,
     :        STRING, SLEN, STATUS )

*  Repeat for next element
      END DO

*  Finished parsing array . ACTION should be LEX__ENDARR and status
*  should be SAI__OK. if we have hit thend of the command line, then
*  there must have been some problem parsing the array. Annul the
*  LEX__ENDPARSE (since higher routines annul LEX__ENDPARSE errors), and
*  re-report it using a "command line syntax error" error code.
      IF ( STATUS .EQ. LEX__ENDPARSE ) THEN
         CALL EMS_ANNUL(STATUS)
         STATUS = SUBPAR__CMDSYER
         CALL EMS_SETC( 'PARM', PARKEY(NAMECODE) )
         CALL EMS_REP( 'SUP_ARRAY4', 'Failed to read array of values '//
     :                 'supplied for parameter ^PARM.', STATUS )
      END IF

*  and, if all is OK, copy the array values into the parameter
      IF ( STATUS .EQ. SAI__OK ) THEN

*     Reverse order of dimensions array
         DO I = 1, NDIMS
            DIMS(I) = COUNT(NDIMS-I+1)
         END DO

*     If parameter is 'internal' cancel it and check for WRITE access
         IF ( PARVPATH(1,NAMECODE) .EQ. SUBPAR__INTERNAL ) THEN
            CALL SUBPAR_CANCL( NAMECODE, STATUS )
            IF ( .NOT. PARWRITE(NAMECODE) ) THEN
               STATUS = SUBPAR__ICACM
               CALL EMS_SETC( 'PARM', PARKEY(NAMECODE) )
               CALL EMS_REP( 'SUP_ARRAY4',
     :         'SUBPAR: Needs write access to put parameter values'//
     :         ' for parameter ^PARM', STATUS )
            END IF
         END IF

*     If all OK, create a suitable component in the
*     parameter file and put the array in that.
         IF ( STATUS .EQ. SAI__OK ) THEN
            TYPE = MOD( PARTYPE(NAMECODE), 10 )
            IF ( TYPE .GT. 5 ) THEN
               STATUS = SUBPAR__IVPRTYPE
               CALL EMS_REP( 'SUP_ARRAY5',
     :                     'SUBPAR_ARRAY: Invalid parameter type - '/
     :                     /'system error', STATUS )
            ELSE
               IF ( TYPE .NE. SUBPAR__NOTYPE ) THEN
                  HDSTYPE = POSTYPES(TYPE)
               ELSE IF ( TACTION .EQ. LEX__STRING ) THEN
                  HDSTYPE = '_CHAR*132'
               ELSE IF ( TACTION .EQ. LEX__INTEGER ) THEN
                  HDSTYPE = '_INTEGER'
               ELSE IF ( TACTION .EQ. LEX__REAL ) THEN
                  HDSTYPE = '_REAL'
               ELSE IF ( TACTION .EQ. LEX__DOUBLE ) THEN
                  HDSTYPE = '_DOUBLE'
               ELSE IF ( TACTION .EQ. LEX__LOGICAL ) THEN
                  HDSTYPE = '_LOGICAL'
               END IF

*  Fix up D exponents if necessary so HDS will handle them
*  This should be fixed in HDS
                  IF ( ( TACTION .EQ. LEX__DOUBLE )
     :            .AND.( HDSTYPE(1:5) .NE. '_CHAR' ) ) THEN
                     DO J = 1, COMP-1
                        I = STRING_IANYL( ARRAY(J), 'Dd' )
                        IF (I .GT. 0) ARRAY(J)(I:I) = 'E'
                     END DO

                  END IF

*  Now create HDS component of the right type and store the value
               CALL SUBPAR_CRINT( NAMECODE, HDSTYPE, NDIMS, DIMS, LOC,
     :                           STATUS )
               CALL SUBPAR_PUT( LOC, HDSTYPE, NDIMS, DIMS, ARRAY,
     :                         STATUS )
               IF ( STATUS .NE. SAI__OK ) THEN
                  CALL SUBPAR_CANCL( NAMECODE, STATUS )
               END IF
            END IF
         END IF

*     Final checks
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL EMS_SETC( 'PARM', PARKEY(NAMECODE) )
            CALL EMS_REP( 'SUP_ARRAY6',
     :                   'SUBPAR: Failed to store array value for '//
     :                   'parameter ^PARM', STATUS )

         END IF

      END IF

      END
