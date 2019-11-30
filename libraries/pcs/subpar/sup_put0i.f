      SUBROUTINE SUBPAR_PUT0I ( NAMECODE, IVALUE, STATUS )
*+
*  Name:
*     SUBPAR_PUT0I

*  Purpose:
*     Write scalar INTEGER parameter value.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SUBPAR_PUT0I ( NAMECODE, IVALUE, STATUS )

*  Description:
*     Put a scalar value into the storage associated with the
*     indicated parameter.
*     There is a routine for each access type, INTEGER:

*     If the object data type differs from the access type, INTEGER, then
*     conversion is performed if possible.

*     Note that a Vector (1-D) object containing a single value is
*     different from a Scalar (0-D).

*  Arguments:
*     NAMECODE=INTEGER (given)
*        pointer to the parameter
*     IVALUE=INTEGER
*        Value to be given to the parameter
*     STATUS=INTEGER

*  Algorithm:
*     Look-up the parameter definition,
*     If it is not INTERNAL, get a locator to the associated object
*     and write the value to it relying on  HDS type conversion

*     If the parameter is INTERNAL, store it in the relevant type
*     array in memory, using suitable conversion where necessary.
*     INTERNAL parameters optimise the time taken to access scalar
*     parameters.

*  Copyright:
*     Copyright (C) 1984, 1987, 1988, 1991, 1992, 1993 Science & Engineering Research Council.
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
*     BDK: B D Kelly (ROE)
*     AJC: A J Chipperfield (STARLINK)
*     {enter_new_authors_here}

*  History:
*     24-SEP-1984 (BDK):
*        Original version
*     13-NOV-1987 (BDK):
*        improve character-to-logical conversion
*     15-AUG-1988 (AJC):
*        don't annul locator if not obtained
*     06-AUG-1991 (AJC):
*        use HDS conversion
*        change PAR__ICACM to SUBPAR__
*        and EMS error reports
*     27-SEP-1991 (AJC):
*        Prefix messages with 'SUBPAR:'
*     19-JUN-1992 (AJC):
*        Correct error message
*     26-FEB-1993 (AJC):
*        Add INCLUDE DAT_PAR
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE


*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'SUBPAR_PAR'
      INCLUDE 'SUBPAR_ERR'


*  Arguments Given:
      INTEGER NAMECODE                  ! parameter number

      INTEGER IVALUE			! Scalar to supply value

*    Status return :
      INTEGER STATUS			! Status Return


*  External References:

*  Global Variables:
      INCLUDE 'SUBPAR_CMN'


*  Local Variables:
      LOGICAL INTERNAL                  ! .TRUE. => the value is to be
                                        ! stored internally rather than
                                        ! in a user-specified HDS
                                        ! structure.

      INTEGER STYPE                           ! stored type of the
                                              ! parameter.
                                              ! STYPE .LT. 10 => value
                                              ! stored internally
                                              ! STYPE .GE. 10 => value in
                                              ! a data structure.

      INTEGER TYPE                            ! data type of the
                                              ! parameter.
                                              ! This is a numeric code
                                              ! with possible values
                                              ! SUBPAR__NONE
                                              ! SUBPAR__REAL
                                              ! SUBPAR__CHAR
                                              ! SUBPAR__INTEGER
                                              ! SUBPAR__DOUBLE
                                              ! SUBPAR__LOGICAL

      CHARACTER*(DAT__SZLOC) LOC              ! locator if value stored
                                              ! in HDS

      INTEGER ITEMP                           ! temporary integer value

*.


      IF (STATUS .NE. SAI__OK) RETURN

*
*   Check that there is write access to the parameter
*
      IF ( PARWRITE(NAMECODE) ) THEN
*
*      get the data type
*
         STYPE = PARTYPE(NAMECODE)

         TYPE = MOD ( STYPE, 10 )
*
*      Check the first step of VPATH to see whether the parameter is to
*      be stored internally.
*
         IF ( ( PARSTATE(NAMECODE) .NE. SUBPAR__NULL ) .AND.
     :     ( PARVPATH(1,NAMECODE) .EQ. SUBPAR__INTERNAL ) .AND.
     :     ( PARTYPE(NAMECODE) .LT. 10 ) ) THEN
            INTERNAL = .TRUE.
         ELSE
            INTERNAL = .FALSE.
         ENDIF
*
*      If to be stored in a data structure, get its locator
*
         IF ( .NOT. INTERNAL ) THEN
            CALL SUBPAR_ASSOC ( NAMECODE, 'WRITE', LOC, STATUS )
         ENDIF

         IF (STATUS .EQ. SAI__OK) THEN
*
*         If the parameter is not INTERNAL use HDS conversion
*         Otherwise do suitsble type conversion and then store the data
*         internally.
*
            IF ( .NOT. INTERNAL ) THEN

               CALL DAT_PUT0I ( LOC, IVALUE, STATUS )


            ELSE IF ( TYPE .EQ. SUBPAR__REAL ) THEN

               PARREAL ( NAMECODE ) = REAL ( IVALUE )

            ELSE IF ( TYPE .EQ. SUBPAR__CHAR ) THEN

               CALL CHR_ITOC( IVALUE, PARVALS(NAMECODE), ITEMP )
               IF ( PARVALS(NAMECODE)(1:1) .EQ. '*' ) THEN
                  STATUS = SUBPAR__CONER
                  CALL EMS_SETC ( 'NAME', PARKEY(NAMECODE) )
                  CALL EMS_REP ( 'SUP_PUT0D1',
     :            'SUBPAR: Failed to convert INTEGER value to '//
     :            'CHARACTER for parameter ^NAME', STATUS )
               ENDIF

            ELSE IF ( TYPE .EQ. SUBPAR__INTEGER ) THEN

               PARINT ( NAMECODE ) = IVALUE

            ELSE IF ( TYPE .EQ. SUBPAR__INT64 ) THEN

               PARINT64 ( NAMECODE ) = IVALUE

            ELSE IF ( TYPE .EQ. SUBPAR__DOUBLE ) THEN

               PARDOUBLE ( NAMECODE ) = DBLE ( IVALUE )

            ELSE IF ( TYPE .EQ. SUBPAR__LOGICAL ) THEN

                  PARLOG( NAMECODE ) = ( MOD(IVALUE,2) .EQ. 1 )

            ELSE

               STATUS = SUBPAR__IVPRTYPE
               CALL EMS_SETC ( 'NAME', PARKEY(NAMECODE) )
               CALL EMS_REP ( 'SUP_PUT0D4',
     :         'SUBPAR: Parameter ^NAME is non-primitive - '//
     :         'attempted PUT0I to it', STATUS )

            ENDIF
*
*         If storage was in an HDS structure, annul the locator.
*         NB - does not close the container file, or annul other cloned
*         locators which might have been previously associated with this
*         parameter. This should be good from the point of view of future
*         access speed to the same value.
*         This corresponds with SSE 0.75.
*
            IF ( .NOT. INTERNAL ) THEN
               CALL DAT_ANNUL ( LOC, STATUS )
            ELSE IF ( STATUS .EQ. SAI__OK ) THEN
               PARSTATE(NAMECODE) = SUBPAR__ACTIVE
            ENDIF

        ENDIF

      ELSE
*
*      No write access
*
         STATUS = SUBPAR__ICACM
         CALL EMS_SETC ( 'NAME', PARKEY(NAMECODE) )
         CALL EMS_REP ( 'SUP_PUT0D5',
     :   'SUBPAR: Failed to ''PUT'' to parameter ^NAME - '//
     :   'access READ specified', STATUS )

      ENDIF

      END
