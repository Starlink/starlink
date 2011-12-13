      SUBROUTINE CAT5_CLOSE (CI, STATUS)
*+
*  Name:
*     CAT5_CLOSE
*  Purpose:
*     Close a catalogue held as a small text list (STL).
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT5_CLOSE (CI; STATUS)
*  Description:
*     Close a catalogue held as a small text list (STL).
*  Arguments:
*     CI  =  INTEGER (Given)
*        Catalogue identifier.
*     STATUS = INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Attempt to determine the common block array element for the
*     catalogue.
*     If ok then
*       Obtain the Fortran unit number for the STL file.
*       Determine whether the catalogue is a standard STL or a
*       KAPPA-format one.
*       If the catalogue is being created then
*         If creation of the catalogue is finished then
*           Set the total number of rows.
*           Obtain the Fortran unit number for the STL file.
*           Write a blank line to the file.
*           While there are more parameters
*             Attempt to obtain an identifier for the next parameter.
*             If ok then
*               Obtain the attributes of the parameter.
*               Write the parameter attribute details to the file.
*             else
*               Set the termination flag.
*             end if
*           end do
*           Write the 'BEGINTABLE' line to the file.
*           Write the table of values.
*         end if
*       end if
*       If creation of the catalogue is finished then
*         Close the catalogue file.
*       else
*         Delete the catalogue file.
*       end if
*       Release the workspace associated with all the catalogue columns.
*     end if
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
*     ACD: A C Davenhall (Edinburgh)
*  History:
*     19/7/96  (ACD): Original version.
*     29/8/96  (ACD): First stable version.
*     10/12/96 (ACD): Added writing 'KAPPA format' STLs.
*     28/3/97  (ACD): Changed the definition of column and parameter
*        names to use the correct parametric contstant (CAT__SZCMP).
*     6/6/98   (ACD): Changed the way that column identifiers are
*        manipulated.
*     17/9/99  (ACD): Tidied up closing incomplete catalogues, ie.
*        ones to which no rows had been writen.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'           ! External CAT constants.
      INCLUDE 'CAT1_PAR'          ! Internal CAT constants.
*  Global Variables:
      INCLUDE 'CAT1_CATS_CMN'     ! Catalogues common block.
      INCLUDE 'CAT1_IDS_CMN'      ! Identifiers common block.
      INCLUDE 'CAT5_STL_CMN'      ! STL common block.
*  Arguments Given:
      INTEGER
     :  CI
*  Status:
      INTEGER STATUS             ! Global status
*  Local Variables:
      INTEGER
     :  CIELM,  ! Element for the catalogue in the common block arrays.
     :  DFUNIT, ! Fortran unit number for the catalogue file.
     :  LSTAT,  ! Fortan I/O status from closing the files.
     :  PARS,   ! Sequence number of current parameter.
     :  QI,     ! Identifier for current parameter.
     :  CIOUT,  ! Identifier to parent catalogue.
     :  SQXFMT, ! Size of external format for character column.
     :  LQXFMT, ! Length of FXFMT (excl. trail. blanks).
     :  LOOP    ! Loop index.
      LOGICAL
     :  KFLAG,  ! Flag; is STL in 'KAPPA format' or standard.
     :  MORE    ! Flag; more parameters to process?

*
*    Attributes for a single parameter.

      CHARACTER
     :  QNAME*(CAT__SZCMP),  ! Name attribute.
     :  QUNIT*(CAT__SZUNI),  ! Units attribute.
     :  QXFMT*(CAT__SZEXF),  ! External format attribute.
     :  QCOMM*(CAT__SZCOM),  ! Comments attribute.
     :  QVALUE*(CAT__SZVAL)  ! Value attribute.
      INTEGER
     :  QDTYPE,  ! Data type attribute.
     :  QCSIZE,  ! Character size attribute.
     :  QDIM,    ! Dimensionality attribute.
     :  QSIZE    ! Size attribute.
      DOUBLE PRECISION
     :  QDATE    ! Modification date attribute.
      LOGICAL
     :  QPDISP   ! Preferential display flag attribute.
*.

      IF (STATUS .EQ. CAT__OK) THEN

*
*       Attempt to determine the common block array element for the
*       catalogue and proceed if ok

         CALL CAT1_CIELM (CI, CIELM, STATUS)
         IF (STATUS .EQ. CAT__OK) THEN

*
*          Obtain the Fortran unit number for the STL file.

            DFUNIT = STUNT__CAT5(CIELM)

*
*          Determine whether the catalogue is a standard STL or a
*          KAPPA-format one.

            KFLAG = KFLAG__CAT5(CIELM)

*
*          Check whether the catalogue is being created.

            IF (STATE__CAT1(CIELM) .EQ. CAT1__STNEW) THEN

*
*             Check whether creation of the catalogue is finished.

               IF (FINSH__CAT1(CIELM)) THEN

*
*                Set the total number of rows.

C                 print3000, nrow__cat1(cielm), crow__cat1(cielm)
C3000             format(1x, 'nrow__cat1(cielm), crow__cat1(cielm): ',
C    :              i5, i5)

                  NROW__CAT1(CIELM) = CROW__CAT1(CIELM) - 1

C                 print3000, nrow__cat1(cielm), crow__cat1(cielm)

*
*                Write a blank line to the STL file.  This blank line
*                separates the parameters from preceding items.

                  WRITE(DFUNIT, 2000, IOSTAT=LSTAT)
 2000             FORMAT(1X)
                  CALL CAT1_IOERR (LSTAT, STATUS)

*
*                Copy every parameter to the file.  For each parameter
*                get its identifier, get its attributes and write the
*                details to the file.

                  PARS = 0
                  MORE = .TRUE.

                  DO WHILE (MORE)
                     PARS = PARS + 1
                     CALL CAT_TNDNT (CI, CAT__QITYP, PARS, QI, STATUS)
                     IF (STATUS .EQ. CAT__OK  .AND.  QI .NE. CAT__NOID)
     :                 THEN

                        CALL CAT_PINQ (QI, 1, CIOUT, QNAME, QDTYPE,
     :                    QCSIZE, QDIM, QSIZE, QUNIT, QXFMT, QPDISP,
     :                    QCOMM, QVALUE, QDATE, STATUS)

*
*                      Generate the external format if it has not already
*                      been set.  Note that the DOUBLE PRECISION format
*                      should be able represent angles to an accuracy
*                      of 0.01 second of arc.

                        IF (QXFMT .EQ. ' ') THEN
                           IF (QDTYPE .EQ. CAT__TYPEB) THEN
                              QXFMT = 'I4'
                           ELSE IF (QDTYPE .EQ. CAT__TYPEW) THEN
                              QXFMT = 'I6'
                           ELSE IF (QDTYPE .EQ. CAT__TYPEI) THEN
                              QXFMT = 'I6'
                           ELSE IF (QDTYPE .EQ. CAT__TYPER) THEN
                              QXFMT = 'E12.3'
                           ELSE IF (QDTYPE .EQ. CAT__TYPED) THEN
                              QXFMT = 'D19.10'
                           ELSE IF (QDTYPE .EQ. CAT__TYPEL) THEN
                              QXFMT = 'L5'
                           ELSE IF (QDTYPE .EQ. CAT__TYPEC) THEN
                              SQXFMT = QCSIZE + 2

                              LQXFMT = 0
                              QXFMT = ' '

                              CALL CHR_PUTC ('A', QXFMT, LQXFMT)
                              CALL CHR_PUTI (SQXFMT, QXFMT, LQXFMT)
                           END IF
                        END IF

*
*                      Write the parameter attribute details to the file.

                        CALL CAT5_WRPAR (DFUNIT, KFLAG, QNAME, QDTYPE,
     :                    QCSIZE, QVALUE, QUNIT, QXFMT, QPDISP, QCOMM,
     :                    STATUS)
                     ELSE

*
*                      Unable to obtain another parameter; set the
*                      termination flag.

                        MORE = .FALSE.

                     END IF
                  END DO

*
*                Write the 'BEGINTABLE' line to the file.  It must precede
*                the table of values.

                  IF (.NOT. KFLAG) THEN
                     WRITE(DFUNIT, 2001, IOSTAT=LSTAT)
 2001                FORMAT(/ 'BEGINTABLE')
                  ELSE
                     WRITE(DFUNIT, 2002, IOSTAT=LSTAT)
 2002                FORMAT(/ '#BEGINTABLE')
                  END IF

                  IF (STATUS .EQ. CAT__OK) THEN
                     CALL CAT1_IOERR (LSTAT, STATUS)
                  END IF

*
*                Write the table of values.

                  CALL CAT5_WRTBL (CI, NROW__CAT1(CIELM), DFUNIT,
     :              STATUS)
               END IF
            END IF

*
*          If creation of the catalogue is complete then close the
*          catalogue file.  Otherwise delete it.

            IF (FINSH__CAT1(CIELM)) THEN
               CLOSE(UNIT=DFUNIT, IOSTAT=LSTAT)
            ELSE
               CLOSE(UNIT=DFUNIT, STATUS='DELETE', IOSTAT=LSTAT)
            END IF

            IF (STATUS .EQ. CAT__OK) THEN
               CALL CAT1_IOERR (LSTAT, STATUS)
            END IF

*
*          Release the workspace associated with all the catalogue
*          columns.

            DO LOOP = 1, NIDS__CAT1
               IF (IDTYP__CAT1(LOOP) .EQ. CAT__FITYP  .OR.
     :             IDTYP__CAT1(LOOP) .EQ. CAT__FETYP) THEN
                  IF (IDPRN__CAT1(LOOP) .EQ. CI) THEN
                     CALL CAT1_FREAR (FPTR__CAT5(LOOP), STATUS)
                     CALL CAT1_FREAR (FPTRN__CAT5(LOOP), STATUS)
                  END IF
               END IF
            END DO

         END IF

      END IF

      END
