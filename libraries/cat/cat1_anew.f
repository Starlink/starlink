      SUBROUTINE CAT1_ANEW (CI, IAST, STATUS)
*+
*  Name:
*     CAT1_ANEW
*  Purpose:
*     Create an AST object for a catalogue and return its identifier.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT1_ANEW (CI; IAST; STATUS)
*  Description:
*     Create an AST object for a catalogue and return its identifier.
*  Arguments:
*     CI = INTEGER (Given)
*        Catalogue identifier.
*     IAST = INTEGER (Returned)
*        An AST pointer to the returned object.  AST__NULL is returned if
*        an error occurs.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Initialise the returned AST pointer.
*     Initialise the 'report adopted equinox and epoch' flag.
*     Begin an AST context.
*     For every axis
*       Check that the corresponding column exists in the catalogue.
*     end for
*     If ok then
*       If a sky frame is required then
*         If the equinox is not defined then
*           Attempt to get the equinox from the catalogue or adopt the
*           default.
*         end if
*         If the epoch is not defined then
*           Attempt to get the epoch from the catalogue or adopt the
*           default.
*         end if
*         If the coordinate system is not defined then
*           Determine and adopt the coordinate system of the equinox.
*         end if
*         If the 'report adopted equinox and epoch' flag is set then
*           If in verbose mode then
*             Report the equinox and epoch adopted.
*           end if
*         end if
*         Create the sky frame and set its domain.
*         Set the axis symbols to the corresponding column names.
*         Set any passed attributes.
*         If required then
*           Set the equinox.
*         end if
*         If required then
*           Set the epoch.
*         end if
*         If required then
*           Set the system.
*         end if
*       else (a grid (Cartesian) frame)
*         Create the grid frame and set its domain.
*         Set the axis details from the corresponding columns.
*         Set any passed attributes.
*       end if
*       Create a default frame-set to hold the frame or sky-frame.
*       Annul the frame.
*       If ok then
*         Export the AST identifier.
*       else
*         Annul the AST identidier.
*       end if
*     end if
*     End the AST context.
*     Report any error.
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
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
*     DSB: David S. Berry (Starlink)
*     ACD: A C Davenhall (Edinburgh)
*  History:
*     14/10/99 (ACD): Original version (from POL1_GTCTA).
*     7/11/99  (ACD): First stable version.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants.
      INCLUDE 'CAT_PAR'          ! External CAT constants.
      INCLUDE 'AST_PAR'          ! AST constants and function declarations.
      INCLUDE 'CAT_ERR'          ! CAT error codes.
*  Global Variables:
      INCLUDE 'CAT1_CTRL_CMN'    ! Flags to control CAT.
      INCLUDE 'CAT1_AST_CMN'     ! CAT - AST common block.
*  Arguments Given:
      INTEGER
     :  CI
*  Arguments Returned:
      INTEGER
     :  IAST
*  Status:
      INTEGER STATUS             ! Global status.
*  External References:
      INTEGER  CHR_LEN
*  Local Variables:
      LOGICAL
     :  REPORT   ! Flag; report details of adopted equinox and epoch?
      INTEGER
     :  CAXIS,   ! Current axis.
     :  CIELM,   ! Array element corresponding to the catalogue.
     :  COLI,    ! Identifier for column corresponding to current axis.
     :  EQI,     !      "     for equinox catalogue parameter.
     :  EPI,     !      "      "  epoch       "         "    .
     :  FRM,     ! Pointer to a Frame.
     :  STRLEN   ! Length of string (excl. trail. blanks).
      DOUBLE PRECISION
     :  EQVAL    ! Equinox value.
      CHARACTER
     :  CSYS*(AST__SZSTR),     ! Catalogue coordinate system.
     :  CEPOCH*(AST__SZSTR),   !     "         "      epoch.
     :  CEQUIN*(AST__SZSTR),   !     "         "      equinox.
     :  EQSYS*1                ! Equinox system.
*.

      IF (STATUS .EQ. SAI__OK) THEN
C        print3000
C3000    format(1x, 'entered CAT1_ANEW.')

*
*       Initialise the returned AST pointer.

         IAST = AST__NULL

*
*       Initialise the 'report adopted equinox and epoch' flag.

         REPORT = .FALSE.

*
*       Begin an AST context.

         CALL AST_BEGIN (STATUS)

*
*       For every axis in the frame-set check that the corresponding
*       column exists in the catalogue.  Note:
*
*       (a) in the present implementation every frame-set must
*       contain AST__MXDIM = 2 axes,
*
*       (b) the check is enclosed in an error context so that any error
*       message can be annulled and a more relevant one substituted.

         CALL CAT1_CIELM (CI, CIELM, STATUS)

         DO CAXIS = 1, AST__MXDIM
            CALL ERR_MARK

            CALL CAT_TIDNT (CI, COLS__AST(CIELM, CAXIS), COLI, STATUS)
            IF (STATUS .NE. SAI__OK) THEN
               IF (STATUS .EQ. CAT__NOCMP) THEN
                  CALL ERR_ANNUL (STATUS)

                  STATUS = CAT__NOCMP

                  CALL MSG_SETI ('CAXIS', CAXIS)
                  CALL MSG_SETC ('COLS', COLS__AST(CIELM, CAXIS) )
                  CALL ERR_REP ('CAT1_ANEW_COL', 'AST frame-set axis '/
     :              /'^CAXIS is column ^COLS, which is not in the '/
     :              /'catalogue.', STATUS)
               END IF
            END IF

            CALL ERR_RLSE
         END DO

*
*       Proceed if all is ok, that is, if the specified columns exist in
*       the catalogue.

         IF (STATUS .EQ. SAI__OK) THEN

*
*          Check whether a sky frame is required.

            IF (SKYFR__AST(CIELM) ) THEN

*
*             If the equinox is not defined then attempt to get it from
*             the catalogue, or failing that adopt the default.  The
*             attempt is again enclosed in an error context so that any
*             error can be annulled.

               IF (EQUIN__AST(CIELM) .EQ. ' ') THEN
                  CALL ERR_MARK

                  CALL CAT_TIDNT (CI, 'EQUINOX', EQI, STATUS)
                  CALL CAT_TIQAC (EQI, 'VALUE', CEQUIN, STATUS)

                  IF (STATUS .NE. SAI__OK) THEN
                     CALL ERR_ANNUL (STATUS)
                     CEQUIN = 'J2000.0'
                     REPORT = .TRUE.
                  END IF

                  CALL ERR_RLSE

               ELSE
                  CEQUIN = EQUIN__AST(CIELM)

               END IF

*
*             If the epoch is not defined then attempt to get it from
*             the catalogue, or failing that adopt the default.  The
*             attempt is again enclosed in an error context so that any
*             error can be annulled.

               IF (EPOCH__AST(CIELM) .EQ. ' ') THEN
                  CALL ERR_MARK

                  CALL CAT_TIDNT (CI, 'EPOCH', EPI, STATUS)
                  CALL CAT_TIQAC (EPI, 'VALUE', CEPOCH, STATUS)

                  IF (STATUS .NE. SAI__OK) THEN
                     CALL ERR_ANNUL (STATUS)
                     CEPOCH = 'J2000.0'
                     REPORT = .TRUE.
                  END IF

                  CALL ERR_RLSE

               ELSE
                  CEPOCH = EPOCH__AST(CIELM)

               END IF

*
*             If the coordinate system is not defined then determine
*             it from the equinox.

               IF (SYS__AST(CIELM) .EQ. ' ') THEN
                  CALL CAT1_DCEQP (CEQUIN, EQSYS, EQVAL, STATUS)

                  IF (EQSYS .EQ. 'B') THEN
                     CSYS = 'FK4'
                  ELSE
                     CSYS = 'FK5'
                  END IF
               ELSE
                  CSYS = SYS__AST(CIELM)
               END IF

*
*             If the report flag is set and CAT is not in quiet mode
*             then report the equinox and epoch adopted.

               IF (REPORT) THEN
                  IF (.NOT. QUIET__CAT1) THEN
                     CALL MSG_SETC ('CEQUIN', CEQUIN)
                     CALL MSG_SETC ('CEPOCH', CEPOCH)

                     CALL MSG_OUT (' ', 'Adopted equinox ^CEQUIN '/
     :                 /'and epoch ^CEPOCH for AST frame-set.',
     :                 STATUS)
                  END IF
               END IF

*
*             Create the sky frame and set the domain.

               STRLEN = CHR_LEN(DOMAN__AST(CIELM) )
               FRM = AST_SKYFRAME ('DOMAIN=' //
     :           DOMAN__AST(CIELM)(1 : STRLEN), STATUS)

*
*             Set the axis symbols to the correspond column names.

               STRLEN = CHR_LEN(COLS__AST(CIELM, 1) )
               CALL AST_SETC (FRM, 'SYMBOL(1)',
     :           COLS__AST(CIELM, 1)(1 : STRLEN), STATUS)

               STRLEN = CHR_LEN(COLS__AST(CIELM, 2) )
               CALL AST_SETC (FRM, 'SYMBOL(2)',
     :           COLS__AST(CIELM, 2)(1 : STRLEN), STATUS)

*
*             Set any passed attributes.

               IF (ATTRB__AST(CIELM) .NE. ' ') THEN
                  STRLEN = CHR_LEN(ATTRB__AST(CIELM) )
                  CALL AST_SET (FRM, ATTRB__AST(CIELM)(1 : STRLEN),
     :              STATUS)
               END IF

*
*             If required then set any of the equinox, epoch and system.

               IF (EQUIN__AST(CIELM) .EQ. ' ') THEN
                  STRLEN = CHR_LEN(CEQUIN)
                  CALL AST_SETC (FRM, 'EQUINOX', CEQUIN(1 : STRLEN),
     :              STATUS)
               END IF

               IF (EPOCH__AST(CIELM) .EQ. ' ') THEN
                  STRLEN = CHR_LEN(CEPOCH)
                  CALL AST_SETC (FRM, 'EPOCH', CEPOCH(1 : STRLEN),
     :              STATUS)
               END IF

               IF (SYS__AST(CIELM) .EQ. ' ') THEN
                  STRLEN = CHR_LEN(CSYS)
                  CALL AST_SETC (FRM, 'SYSTEM', CSYS(1 : STRLEN),
     :              STATUS)
               END IF

            ELSE

*
*             Create the (Cartesian) grid frame and set its domain.
*             Note that in the current implementation the frame must
*             be two-dimensional.

               STRLEN = CHR_LEN(DOMAN__AST(CIELM) )
               FRM = AST_FRAME( 2, 'DOMAIN=' //
     :           DOMAN__AST(CIELM)(1 : STRLEN), STATUS)

*
*             Set the axis details from the corresponding columns.

               CALL CAT1_AXSET (FRM, 1, CI, COLS__AST(CIELM, 1), STATUS)
               CALL CAT1_AXSET (FRM, 2, CI, COLS__AST(CIELM, 2), STATUS)

*
*             Set any passed attributes.

               IF (ATTRB__AST(CIELM) .NE. ' ') THEN
                  STRLEN = CHR_LEN(ATTRB__AST(CIELM) )
                  CALL AST_SET (FRM, ATTRB__AST(CIELM)(1 : STRLEN),
     :              STATUS)
               END IF

            END IF

*
*          Create a default frame-set to hold the frame or sky-frame.

            IAST = AST_FRAMESET (FRM, ' ', STATUS)

*
*          Annul the frame.

            CALL AST_ANNUL (FRM, STATUS)

*
*          If all is ok then export the AST identifier, otherwise
*          annul it.

            IF (STATUS .EQ. SAI__OK) THEN
               CALL AST_EXPORT (IAST, STATUS)
            ELSE
               CALL AST_ANNUL (IAST, STATUS)
            END IF

         END IF

*
*       End the AST context.

         CALL AST_END (STATUS)

*
*       Report any error.

         IF (STATUS .NE. SAI__OK) THEN
            CALL ERR_REP ('CAT1_ANEW_ERR', 'Failed to create a '/
     :        /'new AST frame-set.', STATUS)
         END IF

      END IF

      END
