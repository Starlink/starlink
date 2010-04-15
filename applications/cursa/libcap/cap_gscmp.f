      SUBROUTINE CAP_GSCMP (CMPLST, LTYPE, STATUS)
*+
*  Name:
*     CAP_GSCMP
*  Purpose:
*     Set one of the catview lists of components.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAP_GSCMP (CMPLST, LTYPE; STATUS)
*  Description:
*     Set one of the catview lists of components.  The two lists available
*     are the list of components to be displayed and the list of
*     components for which statistics are to be computed.  The former
*     list may comprise columns, expressions and parameters whereas
*     the latter may comprise only columns.
*  Arguments:
*     CMPLST  =  CHARACTER*(*) (Given)
*        String holding the list of components to be displayed.  The
*        components are separated by semi-colons.
*     LTYPE  =  CHARACTER*(*)  (Given)
*        The name of the list which is to be set.  The permitted values
*        are:
*        'LIST'   -  components to be listed,
*        'STATS'  -  columns for which statistics are to be computed.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     If the catalogue is open then
*       Parse the string holding the list of components to get a list
*       of component names and expressions.
*       If the list required is 'LIST' then
*         Attempt to get identifiers for these expressions.
*         If all ok then
*           If there are no components then
*             Display warning: no columns given.
*           end if
*         end if
*         Set the flag saying that the details of the set of components
*         have changed.
*       else if the list required is 'STATS' then
*         Attempt to get identifiers for these columns.
*         If all ok then
*           If there are no components then
*             Display warning: no columns given.
*           end if
*         end if
*       else
*         Set the status.
*         Report error: illegal name of list.
*       end if
*     else
*       Display warning: catalogue not open.
*     end if
*     If the status corresponds to an invalid expression then
*       flush the error.
*       annul the status.
*     end if
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Leicester)
*  History:
*     28/4/94  (ACD): Original version.
*     27/9/94  (ACD): First stable version.
*     6/3/95   (ACD): Modified to reflect the changed names for the
*        constants defining the array sizes.
*     13/3/95  (ACD): Removed checks for how many columns can fit into
*        the display width.
*     28/3/95  (ACD): Added setting of the flag saying that the details
*        of the set of components have changed.
*     30/3/95  (ACD): Added support for names and units in expressions.
*     29/11/96 (ACD): Modified to handle the list of columns for which
*        statistics are to be computed (in addition to the list of
*        components to be displayed).
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'           ! Standard Starlink constants.
      INCLUDE 'CAT_PAR'           ! External CAT constants.
      INCLUDE 'CAT_ERR'           ! CAT error codes.
      INCLUDE 'SGZ_PAR'           ! StarGaze parametric constants.
*  Global Variables:
      INCLUDE 'SGZ_CMN'           ! StarGaze common block.
*  Arguments Given:
      CHARACTER
     :  CMPLST*(*),
     :  LTYPE*(*)
*  Status:
      INTEGER STATUS              ! Global status.
*  Local Variables:
      LOGICAL
     :  EXPFLG(SGZ__MXCMP)  ! Flag; is component an expression?
      CHARACTER
     :  NAMEIN(SGZ__MXCMP)*(CAT__SZCMP), ! Decoded component names.
     :  EXPNIN(SGZ__MXCMP)*(CAT__SZEXP), !    "        "     expressions.
     :  UNITIN(SGZ__MXCMP)*(CAT__SZEXP)  !    "        "     units.
      INTEGER
     :  CMPIN    ! Number of component names decoded from the string.
*.

      IF (STATUS .EQ. SAI__OK) THEN

*
*       Check if there is a catalogue open.

         IF (COPEN__SGZ) THEN

*
*          Parse the string holding the list of components to get a list
*          of component names.  This list may contain column names,
*          parameter names and expressions.

            CALL CAP_GCPRS (CMPLST, CMPIN, EXPFLG, NAMEIN, EXPNIN,
     :        UNITIN, STATUS)

*
*          For the required type of list attempt to get identifiers for
*          the components and expressions.  If all is ok but no
*          components were found then report a warning.  Note that
*          the text of this warning talks about 'columns' rather than
*          'components' or 'columns, expressions and parameters' for
*          simplicity and conciseness.

            IF (LTYPE .EQ. 'LIST') THEN

               CALL CAP_GCIDS (CI__SGZ, CMPIN, EXPFLG, NAMEIN, EXPNIN,
     :           UNITIN, CMPS__SGZ, CMPNM__SGZ, CMPID__SGZ, STATUS)

               IF (STATUS .EQ. SAI__OK) THEN
                  IF (CMPS__SGZ .LE. 0) THEN
                     CALL CAP_WARN (GUI__SGZ, ' ', 'There are no '/
     :              /'columns or expressions to be listed.', STATUS)
                  END IF
               END IF

*
*             Set the flag to say that the details of the set of
*             components have changed.

               CMPCG__SGZ = .TRUE.

            ELSE IF (LTYPE .EQ. 'STATS') THEN

               CALL CAP_GCIDS (CI__SGZ, CMPIN, EXPFLG, NAMEIN, EXPNIN,
     :           UNITIN, SCPS__SGZ, SCPNM__SGZ, SCPID__SGZ, STATUS)

               IF (STATUS .EQ. SAI__OK) THEN
                  IF (SCPS__SGZ .LE. 0) THEN
                     CALL CAP_WARN (GUI__SGZ, ' ', 'There are no '/
     :              /'columns or expressions to be listed.', STATUS)
                  END IF
               END IF

            ELSE

*
*             The value for 'LTYPE' was unrecognised; set the status
*             and report an error.

               STATUS = SAI__ERROR

               CALL MSG_SETC ('LTYPE', LTYPE)
               CALL ERR_REP ('CAP_GSCMP_LST', 'CAP_GSCMP: illegal '/
     :           /'type of list specified: ^LTYPE.', STATUS)
            END IF

         ELSE
            CALL CAP_WARN (GUI__SGZ, ' ', 'There is no open catalogue.',
     :        STATUS)

         END IF

*
*       If the routine is exiting with an error status corresponding to
*       an invalid expression (that is, an expression which failed to
*       parse) then flush the error and annul the status.

         IF (STATUS .EQ. CAT__INVEX) THEN
            CALL ERR_FLUSH (STATUS)
            CALL ERR_ANNUL (STATUS)
         END IF

      END IF

      END
