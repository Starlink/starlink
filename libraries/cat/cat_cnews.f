      SUBROUTINE CAT_CNEWS (CI, FNAME, DTYPE, CSIZE, UNITS, EXTFMT,
     :  COMM, FI, STATUS)
*+
*  Name:
*     CAT_CNEWS
*  Purpose:
*     Create a column, simultaneously setting some of its attributes.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT_CNEWS (CI, FNAME, DTYPE, CSIZE, UNITS, EXTFMT, COMM;
*       FI; STATUS)
*  Description:
*     Create a column, simultaneously setting some of its attributes.
*
*     These attributes deliberately correspond to those usually used
*     with FITS tables.
*  Arguments:
*     CI  =  INTEGER (Given)
*        Catalogue identifier.
*     FNAME  =  CHARACTER*(*) (Given)
*        Name of the column (or field).
*     DTYPE  =  INTEGER (Given)
*        Type of the column.
*     CSIZE  =  INTEGER (Given)
*        Size of a CHARACTER column.  If the column is not of type
*        CHARACTER CSIZE is irrelevant; it is conventional to set it
*        to zero.
*     UNITS  =  CHARACTER*(*) (Given)
*        The units of the column.
*     EXTFMT  =  CHARACTER*(*) (Given)
*        The external format for the column.
*     COMM  =  CHARACTER*(*) (Given)
*        Comments about the column.
*     FI  =  INTEGER (Returned)
*        Identifier for the column.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     If the column name does not already exist in the catalogue then
*       Attempt to get an identifier for the column.
*       If ok then
*         Get the array element for the catalogue.
*         Create the default set of attributes of a column.
*         Replace the defaults with the actual values for those
*         attributes which are passed as arguments.
*         Create the attributes (they are all mutable at this stage
*         except those for the name and the data type).
*         If (all is ok) then
*           Increment the number of physical columns.
*         end if
*       end if
*     else
*       Set the status.
*     end if
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
*     ACD: A C Davenhall (Leicester)
*  History:
*     23/11/94 (ACD): Original version.
*     28/3/97  (ACD): Changed the definition of column and parameter
*        names to use the correct parametric contstant (CAT__SZCMP).
*     21/12/99 (ACD): Added check that the given column or name is unique,
*        ie. does not alreay exist in the catalogue.
*     27/7/20 (DSB): Must use locom values, not null values, with
*                    _LOGICAL columns.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'           ! External CAT constants.
      INCLUDE 'CAT_ERR'           ! CAT error codes.
*  Global Variables:
      INCLUDE 'CAT1_CATS_CMN'     ! Catalogues common block.
*  Arguments Given:
      INTEGER
     :  CI,
     :  DTYPE,
     :  CSIZE
      CHARACTER
     :  FNAME*(*),
     :  UNITS*(*),
     :  EXTFMT*(*),
     :  COMM*(*)
*  Arguments Returned:
      INTEGER
     :  FI
*  Status:
      INTEGER STATUS             ! Global status.
*  External References:
      INTEGER CHR_LEN
*  Local Variables:
      LOGICAL
     :  EXIST  ! Flag; does the given name already exist in the catalogue?
      INTEGER
     :  CIELM,     ! Common block array element for the catalogue.
     :  LFNAME,    ! Length of FNAME  (excl. trail. blanks).
     :  ERRLEN     !   "    "  ERRTXT ( "  .   "  .   "   ).
      CHARACTER
     :  ERRTXT*75  ! Text of error message.

*
*    Attributes for a single column.

      INTEGER
     :  FGENUS,  ! Genus attribute.
     :  FDTYPE,  ! Data type attribute.
     :  FCSIZE,  ! Character size attribute.
     :  FDIM,    ! Dimensionality attribute.
     :  FSIZE,   ! Size attribute.
     :  FNULL,   ! Null flag attribute.
     :  FORDER   ! Order attribute.
      CHARACTER
     :  FNAM*(CAT__SZCMP),   ! Name attribute.
     :  FEXP*(CAT__SZEXP),   ! Expression attribute.
     :  FXCEPT*(10),         ! Exception value attribute.
     :  FUNIT*(CAT__SZUNI),  ! Units attribute.
     :  FXFMT*(CAT__SZEXF),  ! External format attribute.
     :  FCOMM*(CAT__SZCOM)   ! Comments attribute for the column.
      DOUBLE PRECISION
     :  FSCALE,  ! Scale factor attribute.
     :  FZERO,   ! Zero point attribute.
     :  FDATE    ! Modification date attribute.
      LOGICAL
     :  FPDISP   ! Preferential display flag attribute.
*.

      IF (STATUS .EQ. CAT__OK) THEN

*
*       Check whether the given column name is unique, ie. whether a
*       column or parameter of this name already exists in the catalogue,
*       and proceed if the name is unique.

         CALL CAT1_NMCHK (CI, FNAME, EXIST, STATUS)
         IF (.NOT. EXIST) THEN

*
*          Attempt to create an identifier for the column.

            CALL CAT1_CRTID (CAT__FITYP, CI, FI, STATUS)
            IF (STATUS .EQ. CAT__OK) THEN

*
*             Get the common block array element for the catalogue.

               CALL CAT1_CIELM (CI, CIELM, STATUS)

*
*             Create the default set of values for the attributes of a
*             column.

               CALL CAT1_DFATT (FNAM, FGENUS, FEXP, FDTYPE, FCSIZE,
     :           FDIM, FSIZE, FNULL, FXCEPT, FSCALE, FZERO, FORDER,
     :           FDATE, FUNIT, FXFMT, FPDISP, FCOMM, STATUS)


*
*             Replace the defaults with the actual values for those
*             attributes which are passed as arguments.

               FNAM = FNAME
               FDTYPE = DTYPE
               FCSIZE = CSIZE
               FUNIT = UNITS
               FXFMT = EXTFMT
               FCOMM = COMM

*
*             Must use locum values, not nulls, with _LOGICAL columns
               IF( FDTYPE .EQ. CAT__TYPEL ) FNULL = CAT__LOCUM

*
*             Create the attributes (they are all mutable at this stage
*             except those for the name and the data type).

               CALL CAT1_ADDAC (FI, 'NAME', .FALSE., FNAM, STATUS)
               CALL CAT1_ADDAI (FI, 'GENUS', .TRUE., FGENUS, STATUS)
               CALL CAT1_ADDAC (FI, 'EXPR', .TRUE., FEXP, STATUS)
               CALL CAT1_ADDAI (FI, 'DTYPE', .FALSE., FDTYPE, STATUS)
               CALL CAT1_ADDAI (FI, 'CSIZE', .TRUE., FCSIZE, STATUS)
               CALL CAT1_ADDAI (FI, 'DIMS', .TRUE., FDIM, STATUS)
               CALL CAT1_ADDAI (FI, 'SIZE', .TRUE., FSIZE, STATUS)
               CALL CAT1_ADDAI (FI, 'NULL', .TRUE., FNULL, STATUS)
               CALL CAT1_ADDAC (FI, 'EXCEPT', .TRUE., FXCEPT, STATUS)
               CALL CAT1_ADDAD (FI, 'SCALEF', .TRUE., FSCALE, STATUS)
               CALL CAT1_ADDAD (FI, 'ZEROP', .TRUE., FZERO, STATUS)
               CALL CAT1_ADDAI (FI, 'ORDER', .TRUE., FORDER, STATUS)
               CALL CAT1_ADDAD (FI, 'DATE', .TRUE., FDATE, STATUS)
               CALL CAT1_ADDAC (FI, 'UNITS', .TRUE., FUNIT, STATUS)
               CALL CAT1_ADDAC (FI, 'EXFMT', .TRUE., FXFMT, STATUS)
               CALL CAT1_ADDAL (FI, 'PRFDSP', .TRUE., FPDISP, STATUS)
               CALL CAT1_ADDAC (FI, 'COMM', .TRUE., FCOMM, STATUS)

*
*             If all is ok then increment the number of physical columns.

               IF (STATUS .EQ. CAT__OK) THEN
                  NPCOL__CAT1(CIELM) = NPCOL__CAT1(CIELM) + 1
               END IF


            END IF
         ELSE

*
*          A column of the given name already exists; set the status.

            STATUS = CAT__DUPNM

         END IF

*
*       Report any error.

         IF (STATUS .NE. CAT__OK) THEN
            ERRTXT = ' '
            ERRLEN = 0

            CALL CHR_PUTC ('CAT_CNEWS: failed to create column ',
     :         ERRTXT, ERRLEN)

            IF (FNAME .NE. ' ') THEN
               LFNAME = CHR_LEN (FNAME)
               CALL CHR_PUTC (FNAME(1 : LFNAME), ERRTXT, ERRLEN)
            ELSE
               CALL CHR_PUTC ('<blank>', ERRTXT, ERRLEN)
            END IF

            CALL CHR_PUTC ('.', ERRTXT, ERRLEN)

            CALL CAT1_ERREP ('CAT_CNEWS_ERR', ERRTXT(1 : ERRLEN),
     :        STATUS)
         END IF

      END IF

      END
