      SUBROUTINE SUBPAR_GET0L ( NAMECODE, LVALUE, STATUS )
*+
*  Name:
*     SUBPAR_GET0L

*  Purpose:
*     Read scalar parameter value.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SUBPAR_GET0L ( NAMECODE, LVALUE, STATUS )

*  Description:
*     Get a scalar LOGICAL value from the storage associated with the
*     indicated parameter.
*     If the object data type differs from the access type, LOGICAL,
*     then conversion is performed if possible.
*
*     Note that a Vector (1-D) object containing a single value is
*     different from a Scalar (0-D).

*  Arguments:
*     NAMECODE=INTEGER (given)
*        pointer to the parameter
*     LVALUE=LOGICAL (returned)
*        Value to be obtained from the parameter
*     STATUS=INTEGER

*  Algorithm:
*     Look-up the parameter definition, and extract the value from the
*     internal data area, or from a data structure, if one is defined
*     The number is extracted in the data type declared for the parameter.
*     Any required limits checking is done and, if successful, the
*     necessary type conversion is done.
*     Note that this differs from the Starlink strategy in which all
*     program parameters are actually stored in HDS structures. This
*     change has been made to optimise the time taken to access scalar
*     parameters.
*     For errors other than PAR__*, if not internal, re-prompt up to
*     MAXTRY times.

*  Copyright:
*     Copyright (C) 1984, 1985, 1987, 1988, 1991, 1992, 1993, 1994 Science & Engineering Research Council.
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
*     PCTR: P.C.T. Rees (STARLINK)
*     {enter_new_authors_here}

*  History:
*     24-SEP-1984:
*        Original version
*     25-MAY-1985:
*        Allow for type 'UNIV'
*     05-JUN-1985:
*        do DAT_ASSOC with 'UPDATE', in case of subsequent
*        PAR_PUTs to the parameter
*     13-AUG-1987:
*        on out-of-range, retry
*     16-NOV-1987:
*        improve logical-to-char conversion
*     16-FEB-1988:
*        don't overwrite bad status with OUTRANGE
*     12-AUG-1988:
*        don't annul locator if not obtained
*     09-JUL-1991:
*        remove LIB$CVT_DX_DX conversion
*     29-JUL-1991:
*        handle improved error reporting from LIMITR
*     27-SEP-1991:
*        Prefix messages with 'SUBPAR:'
*        and with ! etc when flushing
*     26-AUG-1992 (PCTR):
*        Replaced EMS_ELOAD/SUBPAR_WRITE loop with a call to
*        SUBPAR_EFLSH.
*     27-AUG-1992 (AJC):
*        Mark and release to protect higher level message tokens
*     19-NOV-1992 (AJC):
*        Changed status from LIMIT routines
*        Correct cleanup before reprompt
*     10-MAR-1993 (AJC):
*        Add DAT_PAR for SUBPAR_CMN
*      1-JUL-1993 (AJC):
*        Re-prompt on most errors, up to 5 times
*     29-SEP-1994 (AJC):
*        Use EMS_FACER not DAT_ERMSG to report errors
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
      INCLUDE 'SUBPAR_PARERR'

*  Arguments Given:
      INTEGER NAMECODE                  ! Parameter number

*  Arguments Returned:
      LOGICAL LVALUE                    ! Value obtained

*  Status:
      INTEGER STATUS                    ! Global status

*  Global Variables:
      INCLUDE 'SUBPAR_CMN'

*  Local Constants:
      INTEGER MAXDIM                    ! Maximum number of dimensions
      PARAMETER ( MAXDIM = 7 )
      INTEGER MAXTRY                    ! Maximum attempts to get good value
      PARAMETER ( MAXTRY = 5 )

*  Local Variables:
      LOGICAL INTERNAL                  ! .TRUE. => the value is
                                        ! stored internally rather than
                                        ! in a user-specified HDS
                                        ! structure.

      CHARACTER*132 VALUE_STRING        ! Variables to store converted
      INTEGER VALUE_INTEGER
      INTEGER VALUE_INT64
      LOGICAL VALUE_LOGICAL

      INTEGER STYPE                           ! Stored type of the
                                              ! parameter.
                                              ! STYPE .LT. 10 => value
                                              ! stored internally
                                              ! STYPE .GE. 10 => value
                                              ! in a data structure.

      INTEGER TYPE                            ! Data type of the
                                              ! parameter.
                                              ! This is a numeric code
                                              ! with possible values
                                              ! SUBPAR__NONE
                                              ! SUBPAR__REAL
                                              ! SUBPAR__CHAR
                                              ! SUBPAR__INTEGER
                                              ! SUBPAR__INT64
                                              ! SUBPAR__DOUBLE
                                              ! SUBPAR__LOGICAL

      CHARACTER*(DAT__SZLOC) LOC              ! Locator if data stored in HDS
      INTEGER DIMS(MAXDIM)                    ! Object dimensions
      INTEGER ACTDIM                          ! Actual number of dimensions
      INTEGER TRIES                           ! Number of tries
      LOGICAL ACCEPTED                        ! If no re-prompt required
*.

*  Check the inherited status.
      IF (STATUS .NE. SAI__OK) RETURN

*  Protect higher level tokens
      CALL EMS_MARK
*
*   Loop until in-range value got or some error
*
      ACCEPTED = .FALSE.
      TRIES = 0

      DO WHILE ( .NOT. ACCEPTED )
*
*      get the data type
*
         STYPE = PARTYPE(NAMECODE)

         TYPE = MOD ( STYPE, 10 )
*
*      Check whether the parameter is stored internally.
*
         IF ( ( PARSTATE(NAMECODE) .NE. SUBPAR__NULL ) .AND.
     :     ( PARVPATH(1,NAMECODE) .EQ. SUBPAR__INTERNAL ) .AND.
     :     ( PARTYPE(NAMECODE) .LT. 10 ) ) THEN
            INTERNAL = .TRUE.
         ELSE
            INTERNAL = .FALSE.
         ENDIF
*
*      If stored in a data structure, get its locator
*
         IF ( .NOT. INTERNAL ) THEN
            IF ( PARWRITE(NAMECODE) ) THEN
               CALL SUBPAR_ASSOC ( NAMECODE, 'UPDATE', LOC, STATUS )
            ELSE
               CALL SUBPAR_ASSOC ( NAMECODE, 'READ', LOC, STATUS )
            ENDIF

            IF (STATUS .EQ. SAI__OK) THEN
*           Get shape of object
               CALL DAT_SHAPE ( LOC, MAXDIM, DIMS, ACTDIM, STATUS )

*           It must be scalar
               IF (ACTDIM .NE. 0 ) THEN
                  STATUS = SUBPAR__ARRDIM
                  CALL EMS_SETC( 'NAME', PARKEY(NAMECODE) )
                  CALL EMS_REP( 'SUP_GET0L1',
     :            'SUBPAR: Parameter ^NAME requires a scalar value',
     :             STATUS )
               ENDIF

            ENDIF

         ENDIF


         IF ( STATUS .EQ. SAI__OK ) THEN
*
*      Extract the data and do type conversion.
*
            IF ( TYPE .EQ. SUBPAR__LOGICAL ) THEN
               IF ( INTERNAL ) THEN
                  CALL SUBPAR_FETCHL ( NAMECODE, VALUE_LOGICAL, STATUS )
               ELSE
                  CALL DAT_GETL ( LOC, 0, 0, VALUE_LOGICAL, STATUS )
               ENDIF
*
*            There is no limit checking for logicals
*
               LVALUE = VALUE_LOGICAL

            ELSE IF ( TYPE .EQ. SUBPAR__REAL ) THEN

               STATUS = SUBPAR__CONER
               CALL EMS_REP( 'SUP_GET0L2',
     :         'SUBPAR: Attempt to convert REAL to LOGICAL', STATUS)

            ELSE IF ( TYPE .EQ. SUBPAR__CHAR ) THEN

               IF ( INTERNAL ) THEN
                  CALL SUBPAR_FETCHC ( NAMECODE, VALUE_STRING, STATUS )
               ELSE
                  CALL DAT_GETC ( LOC, 0, 0, VALUE_STRING, STATUS )
               ENDIF

               CALL SUBPAR_LIMITC ( NAMECODE, VALUE_STRING, ACCEPTED,
     :           STATUS )

               IF ( STATUS .EQ. SAI__OK ) THEN
                  CALL CHR_CTOL( VALUE_STRING, LVALUE, STATUS )
                  IF ( STATUS .NE. SAI__OK ) THEN
                     STATUS = SUBPAR__CONER
                     CALL EMS_SETC( 'VAL', VALUE_STRING )
                     CALL EMS_REP( 'SUP_GET0L3',
     :                'SUBPAR: Error converting ^VAL to LOGICAL',
     :                 STATUS )
                  ENDIF
               ENDIF

            ELSE IF ( TYPE .EQ. SUBPAR__INTEGER ) THEN

               IF ( INTERNAL ) THEN
                  CALL SUBPAR_FETCHI ( NAMECODE, VALUE_INTEGER, STATUS )
               ELSE
                  CALL DAT_GETI ( LOC, 0, 0, VALUE_INTEGER, STATUS )
               ENDIF

               CALL SUBPAR_LIMITI ( NAMECODE, VALUE_INTEGER, ACCEPTED,
     :           STATUS )

               IF ( STATUS .EQ. SAI__OK ) THEN
*              Set TRUE if ls bit is 1, FALSE if it is 0
                  IF ( MOD(VALUE_INTEGER,2) .EQ. 0 ) THEN
                     LVALUE = .FALSE.
                  ELSE
                     LVALUE = .TRUE.
                  ENDIF
               ENDIF

            ELSE IF ( TYPE .EQ. SUBPAR__INT64 ) THEN

               IF ( INTERNAL ) THEN
                  CALL SUBPAR_FETCHK ( NAMECODE, VALUE_INT64, STATUS )
               ELSE
                  CALL DAT_GETK ( LOC, 0, 0, VALUE_INT64, STATUS )
               ENDIF

               CALL SUBPAR_LIMITK ( NAMECODE, VALUE_INT64, ACCEPTED,
     :           STATUS )

               IF ( STATUS .EQ. SAI__OK ) THEN
*              Set TRUE if ls bit is 1, FALSE if it is 0
                  IF ( MOD(VALUE_INT64,2) .EQ. 0 ) THEN
                     LVALUE = .FALSE.
                  ELSE
                     LVALUE = .TRUE.
                  ENDIF
               ENDIF

            ELSE IF ( TYPE .EQ. SUBPAR__DOUBLE ) THEN

               STATUS = SUBPAR__CONER
               CALL EMS_REP( 'SUP_GET0L4',
     :         'SUBPAR: Attempt to convert DOUBLE PRECISION '//
     :         'to LOGICAL', STATUS)

            ELSE
*
*            The declared type is not primitive (eg. 'UNIV'). Just try
*            to get the value from HDS.
*
               CALL DAT_GETL ( LOC, 0, 0, LVALUE, STATUS )

            ENDIF
*
*          If storage was in an HDS structure, annul the locator.
*          NB - does not close the container file, or annul other cloned
*          locators which might have been previously associated with this
*          parameter. This should be good from the point of view of future
*          access speed to the same value.
*          This corresponds with SSE 0.75.
*
            IF ( .NOT. INTERNAL ) THEN
               CALL DAT_ANNUL ( LOC, STATUS )
            ENDIF

         ENDIF

*      Break the loop unless an error was reported and re-prompting is
*      a likely option i.e. for non-internals or one of the PAR errors was
*      reported from the ASSOC (or FETCHC) routine.
         IF ( (STATUS .EQ. SAI__OK )
     :   .OR. ( STATUS .EQ. PAR__NULL )
     :   .OR. ( STATUS .EQ. PAR__ABORT )
     :   .OR. ( STATUS .EQ. PAR__NOUSR )
     :   .OR. INTERNAL ) THEN

            ACCEPTED = .TRUE.

         ELSE
            ACCEPTED = .FALSE.
*        If not SUBPAR probable status error, report status
            IF ( ( STATUS .NE. SUBPAR__ARRDIM )
     :      .AND. ( STATUS .NE. SUBPAR__OUTRANGE )
     :      .AND. ( STATUS .NE. SUBPAR__CONER ) ) THEN
               CALL EMS_FACER( 'MESS', STATUS )
               CALL EMS_REP( 'SUP_GET0L5', '^MESS', STATUS )
            ENDIF
*        Cancel parameter value to force reprompt
            CALL SUBPAR_CANCL ( NAMECODE, STATUS )

*        Flush any pending error messages - resets status
            CALL SUBPAR_EFLSH( STATUS )

*        Check for try limit
            TRIES = TRIES + 1
            IF ( TRIES .EQ. MAXTRY ) THEN
               STATUS = PAR__NULL
               PARSTATE(NAMECODE) = SUBPAR__NULL
               CALL EMS_SETC( 'NAME', PARKEY(NAMECODE) )
               CALL EMS_SETI( 'TRIES', TRIES )
               CALL EMS_REP( 'SUP_GET0L6', 'SUBPAR: '//
     :         '^TRIES prompts failed to get a good value for '//
     :         'parameter ^NAME - NULL assumed', STATUS )
            ENDIF

         ENDIF

      ENDDO

*  Release the error context
      CALL EMS_RLSE

      END

