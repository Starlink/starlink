      SUBROUTINE CAP_GCDET (SWIDTH, RUNSP, CMPS, NAME, IDS, UNITS,
     :  START, WIDTH, STATUS)
*+
*  Name:
*     CAP_GCDET
*  Purpose:
*     Get details for components to be listed.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAP_GCDET (SWIDTH, RUNSP; CMPS; NAME, IDS; UNITS, START,
*       WIDTH; STATUS)
*  Description:
*     Get the details of how to display the components which are to be
*     listed.  Also, truncate the list so that it includes only the
*     columns which can be fitted into the screen width.
*  Arguments:
*     SWIDTH  =  INTEGER (Given)
*        Maximum width of the screen or text file.
*     RUNSP  =  INTEGER (Given)
*        Spaces required for a 'running count' sequence number.
*     CMPS  =  INTEGER (Given and Returned)
*        Number of components.
*     NAME(SGZ__SZCMP)  =  CHARACTER*(*) (Given)
*        Names of the components.
*     IDS(SGZ__SZCMP)  =  INTEGER (Given)
*        Identifiers for the components.
*     UNITS(SGZ__SZCMP)  =  CHARACTER*(*) (Returned)
*        Units for the components.
*     START(SGZ__SZCMP)  =  INTEGER (Returned)
*        Start position in an output row for listing the component.
*     WIDTH(SGZ__SZCMP)  =  INTEGER (Returned)
*        Width in the an output row for listing the component.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Do while (there are more columns to be processed)
*       Increment to the next component.
*       Ensure that the identifier corresponds to a column or
*       expression, not an array element.
*       Get the units for the component.
*       If the units of angles are being reformatted for display then
*         Determine whether the units are recognised as an angle.
*         If so then
*           If angles are being output as sexagesimal values then
*             Set the units to the sexagesimal format specifier.
*           else
*             Set the units to 'Radians'.
*           end if
*         end if
*       end if
*       Determine the width required for displaying a field from the
*       component.
*       Determine the width to allocated for displaying the component;
*       the maximum of the widths of the its name, units and field
*       width.
*       If there is space to display this component then
*         Add the details to the list.
*       else
*         Reset the number of columns.
*         Set the termination flag.
*       end if
*       If this is the last component then
*         Set the termination flag.
*       end if
*       If the status is not ok then
*         Report an error.
*         Set the termination flag.
*       end if
*     end do
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Leicester)
*  History:
*     28/4/94 (ACD): Original version.
*     12/6/94 (ACD): First stable version.
*     6/3/95  (ACD): Modified to reflect the changed names for the
*        constants defining the array sizes.
*     24/2/96 (ACD): Added option to reformat the units attribute for
*        angles prior to display.  The reformatted units are more
*        intuitive.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'           ! Standard Starlink constants.
      INCLUDE 'CAT_PAR'           ! CAT parametric constants.
      INCLUDE 'SGZ_PAR'           ! StarGaze parametric constants.
*  Global Variables:
      INCLUDE 'SGZ_CMN'           ! StarGaze common block.
*  Arguments Given:
      INTEGER
     :  SWIDTH,
     :  RUNSP,
     :  IDS(SGZ__MXCMP)
      CHARACTER
     :  NAME(SGZ__MXCMP)*(*)
*  Arguments Given and Returned:
      INTEGER
     :  CMPS
*  Arguments Returned:
      CHARACTER
     :  UNITS(SGZ__MXCMP)*(*)
      INTEGER
     :  START(SGZ__MXCMP),
     :  WIDTH(SGZ__MXCMP)
*  Status:
      INTEGER STATUS             ! Global status
*  External References:
      INTEGER CHR_LEN
*  Local Variables:
      INTEGER
     :  CURCMP,    ! Current component.
     :  FI,        ! Column or expression identifier.
     :  IDTYPE,    ! Type of the current identifier.
     :  LEXFMT,    ! Length of EXFMT    (excl. trail. blanks).
     :  LNAME,     !   "    "  NAME(n)  ( "  .   "  .   "   ).
     :  LUNITS,    !   "    "  UNITS(n) ( "  .   "  .   "   ).
     :  CWIDTH,    ! Width of current component.
     :  CURPOS     ! Current position in the output row.
      LOGICAL
     :  MORE,      ! Flag; continue processing components.
     :  ISANG      ! Flag; is column recognised as an angle?
      CHARACTER
     :  CUNITS*(CAT__SZUNI), ! Units of the current component.
     :  SEXAG*(CAT__SZUNI),  ! Sexagesimal format specifier (curr. comp).
     :  ANGPRN*20,           ! Flag; angles in radians or sexagesimal?
     :  EXFMT*(CAT__SZEXF)   ! External format of the current component.
*.

      IF (STATUS .EQ. SAI__OK) THEN

*
*       Process all the components which are to be listed.

         CURCMP = 0

         IF (RUNSP .EQ. 0) THEN
            CURPOS = 0
         ELSE
            CURPOS = RUNSP + SGZ__SPACE
         END IF

         MORE = .TRUE.

         DO WHILE (MORE)

*
*          Increment to the next component.

            CURCMP = CURCMP + 1

*
*          Ensure that the identifier corresponds to a column or
*          expression, not an array element.

            CALL CAT_TIDTP (IDS(CURCMP), IDTYPE, STATUS)

            IF (IDTYPE .NE. CAT__FETYP) THEN
               FI = IDS(CURCMP)

            ELSE
               CALL CAT_TIQAI (IDS(CURCMP), 'BASEID', FI, STATUS)

            END IF

*
*          Get the units for the component.

            CALL CAT_TIQAC (FI, 'UNITS', CUNITS, STATUS)

*
*          Reformat the units if required and they are recognised as
*          an angle.

            IF (ANGRF__SGZ) THEN
               CALL CAP_GISAN (CUNITS, ISANG, SEXAG, STATUS)

               IF (ISANG) THEN
                  CALL CAT_TUNEG ('ANGLE_LIST', ANGPRN, STATUS)

                  IF (ANGPRN .EQ. 'SEXAGESIMAL') THEN
                     CUNITS = SEXAG
                  ELSE
                     CUNITS = 'Radians'
                  END IF
               END IF
            END IF

*
*          Determine the width required for displaying a field from the
*          component.

            CALL CAT_TIQAC (FI, 'EXFMT', EXFMT, STATUS)

            CALL CAP_GLFMT (EXFMT, LEXFMT, STATUS)

*
*          Determine the width to allocated for displaying the
*          component; the maximum of the widths of the its name, units
*          and field width.

            IF (NAME(CURCMP) .NE. ' ') THEN
               LNAME = CHR_LEN (NAME(CURCMP) )
            ELSE
               LNAME = 0
            END IF

            IF (CUNITS .NE. ' ') THEN
               LUNITS = CHR_LEN (CUNITS)
            ELSE
               LUNITS = 0
            END IF

            CWIDTH = MAX(LNAME, LUNITS, LEXFMT)
            CWIDTH = CWIDTH + SGZ__SPACE

*
*          Check whether there is space to fit this component into the
*          total width allocated for the row.

            IF (CURPOS + CWIDTH .LT. SWIDTH) THEN

*
*             There is space: work out and store the details of this
*             component.

               UNITS(CURCMP) = CUNITS
               START(CURCMP) = CURPOS + 1
               WIDTH(CURCMP) = CWIDTH

               CURPOS = CURPOS + CWIDTH

            ELSE

*
*             There is not enough space: reset the number of components
*             and set the termination flag.

               CMPS = CURCMP - 1

               MORE = .FALSE.
            END IF

*
*          Check if the current component is the last, and if so then
*          set the termination flag.

            IF (CURCMP .GE. CMPS) THEN
               MORE = .FALSE.
            END IF

*
*          If any error status has been raised then report an error and
*          set the termination flag.

            IF (STATUS .NE. SAI__OK) THEN
               CALL MSG_SETC ('NAME', NAME(CURCMP) )
               CALL ERR_REP ('CAR_GCDET_IVCP', 'Error getting display '/
     :           /'details for component ^NAME', STATUS)
               CALL ERR_FLUSH (STATUS)

               MORE = .FALSE.
            END IF

         END DO

      END IF

      END
