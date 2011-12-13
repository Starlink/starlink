      SUBROUTINE AST2NDF (STATUS)
*+
*  Name:
*     AST2NDF

*  Purpose:
*     Converts an Asterix data cube into a simple NDF.

*  Language:
*     Fortran 77.

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL AST2NDF (STATUS)

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application converts an Asterix data cube into a standard
*     NDF.  See Section 'Notes' (below) for details of the conversion.

*  Usage:
*     ast2ndf in out

*  ADAM Parameters:
*     IN  =  NDF (Read)
*        The name of the input Asterix data cube.  The file extension
*        ('.sdf') should not be included since it is appended automatically
*        by the application.
*     OUT  =  NDF (Write)
*        The name of the output NDF containing the data cube written
*        by the application.  The file extension ('.sdf') should not be
*        included since it is appended automatically by the application.

*  Examples:
*     ast2ndf  ast_cube  ndf_cube
*        This example generates data cube NDF ndf_cube (in file
*        ndf_cube.sdf) from Asterix cube ast_cube (in file ast_cube.sdf).

*  Notes:
*     This application accepts data in the format used by the Asterix
*     package (see SUN/98).  These data are cubes, with two axes
*     comprising a regular grid of positions on the sky and the third
*     corresponding to energy or wavelength.  The data are Starlink HDS
*     files which are very similar in format to a standard NDF.  The
*     following points apply.
*
*     - The Asterix QUALITY array is non-standard.  There is no QUALITY
*       component in the output NDF.  Instead 'bad' or 'null' values
*       are used to indicate missing or suspect values.
*
*     - The VARIANCE component is copied if it is present.
*
*     - The non-standard Asterix axis components are replaced with
*       standard ones.
*
*     - The order of the axes is rearranged.

*  Algorithm:
*     Set the message reporting level.
*     Start an NDF context.
*     Attempt to get a locator for the input ROSAT dataset.
*     If ok then
*       Get a locator for the array component of input dataset.
*       Determine the shape of the input cube.
*       Get a pointer to the input cube.
*       Get a locator and the a pointer for the quality array.
*       Attempt to get a locator and pointer for the variance array.
*       Get the bad bits mask.
*       Attempt to get an identifier for the output object.
*       If ok then
*         Attempt to get a pointer to the output cube.
*         If required then
*           Attempt to get a pointer to the output variance.
*         end if
*         Copy the data cube.
*         Construct the axes of the cube.
*         Copy any auxilliary information.
*         Copy the character components to the output NDF.
*         If ok then
*           Report success
*         else
*           Report failure.
*         end if
*       else
*           Report error getting output cube.
*       end if
*     else
*       Report error getting input map.
*     end if
*     End the NDF context.

*  References:
*     D.J. Allan and R.J. Vallance, 1995, in SUN/98: 'ASTERIX -- X-ray
*       Data Processing System', Starlink.

*  Related Applications:
*     KAPPA:AXCONV

*  Copyright:
*     Copyright (C) 1997-1998, 2004 Central Laboratory of the Research
*     Councils. All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     ACD: A C Davenhall (Edinburgh)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     13/7/97 (ACD): Original version.
*     8/1/98  (ACD): First stable version.
*     9/9/04  (TIMJ): Use CNF_PVAL
*     {enter_further_changes_here}

*-
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing
*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! HDS constants.
      INCLUDE 'DAT_ERR'          ! HDS error codes.
      INCLUDE 'MSG_PAR'          ! Message system constants.
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
*  Status:
      INTEGER STATUS             ! Global status.
*  Local Variables:
      CHARACTER
     :  INFILE*75,           ! Name of the input file.
     :  INLOC*(DAT__SZLOC),  ! Locator to the input dataset.
     :  ARLOC*(DAT__SZLOC),  !    "    "   "    "   array.
     :  QSLOC*(DAT__SZLOC),  !    "    "   "    "   quality structure.
     :  QLLOC*(DAT__SZLOC),  !    "    "   "    "   quality array.
     :  IVRLOC*(DAT__SZLOC), !    "    "   "    "   input variance array.
     :  BDLOC*(DAT__SZLOC)   !    "    "   "    "   bad bits mask.
      INTEGER
     :  DIMS,       ! Dimensionality of the input array.
     :  DIM(3),     ! Size of the input array.
     :  INPTR,      ! Pointer to the input array.
     :  QLPTR,      ! Pointer to the quality array.
     :  IVRPTR,     ! Pointer to the input variance array.
     :  BADPIX      ! Number of bad pixels.
      INTEGER
     :  CLWBND(3),  ! Lower bounds for the data cube.
     :  CUPBND(3),  ! Upper   "     "   "   "   "  .
     :  CUBID,      ! Identifier for the data cube.
     :  CUBPTR,     ! Pointer to      "   "    "  .
     :  OVRPTR,     ! Pointer to the output variance array.
     :  ELEM,       ! Number of elements in an array.
     :  NX,         ! X size of the data cube.
     :  NY,         ! Y  "   "   "   "    "  .
     :  NZ          ! Z  "   "   "   "    "  .
      BYTE
     :  BADBIT      ! Bad bits mask.
      LOGICAL
     :  VARFLG      ! Flag; is there an input variance array?
*.

      IF (STATUS .EQ. SAI__OK) THEN

*
*       Set the message reporting level.

         CALL MSG_IFSET (MSG__NORM, STATUS)

*
*       Start an NDF context.

         CALL NDF_BEGIN

*
*       Attempt to get a locator for the input dataset and proceed if ok.

         CALL PAR_GET0C ('IN', INFILE, STATUS)
         CALL PAR_CANCL ('IN', STATUS)
         CALL HDS_OPEN (INFILE, 'READ', INLOC, STATUS)

         IF (STATUS .EQ. SAI__OK) THEN

*
*          Get a locator for the array component of the input file.

            CALL DAT_FIND (INLOC, 'DATA_ARRAY', ARLOC, STATUS)

*
*          Determine the shape of the input array.

            CALL DAT_SHAPE (ARLOC, 3, DIM, DIMS, STATUS)

*
*          Get a pointer for the input array.

            CALL DAT_MAPR (ARLOC, 'READ', DIMS, DIM, INPTR, STATUS)

*
*          Get a locator and then a pointer to the quality array.

            CALL DAT_FIND (INLOC, 'QUALITY', QSLOC, STATUS)
            CALL DAT_FIND (QSLOC, 'QUALITY', QLLOC, STATUS)
            CALL DAT_MAP (QLLOC, '_UBYTE', 'READ', DIMS, DIM, QLPTR,
     :        STATUS)

*
*          Attempt to get a locator and pointer for the variance
*          array.  Reset the status if and only if the variance
*          array could not be found.

            CALL DAT_FIND (INLOC, 'VARIANCE', IVRLOC, STATUS)

            IF (STATUS .EQ. SAI__OK) THEN
               VARFLG = .TRUE.

               CALL DAT_MAPR (IVRLOC, 'READ', DIMS, DIM, IVRPTR,
     :           STATUS)

            ELSE
               VARFLG = .FALSE.
               IVRPTR = 0

               IF (STATUS .EQ. DAT__OBJNF) THEN
                  CALL ERR_ANNUL (STATUS)
               END IF
            END IF

*
*          Get the bad bits mask.

            CALL DAT_FIND (QSLOC, 'BADBITS', BDLOC, STATUS)
            CALL DAT_GET (BDLOC, '_UBYTE', 0, 0, BADBIT, STATUS)

*
*          Attempt to get an identifier for the output cube and proceed
*          if ok.  First construct the array bounds.

            CLWBND(1) = 1
            CUPBND(1) = DIM(3)

            CLWBND(2) = 1
            CUPBND(2) = DIM(1)

            CLWBND(3) = 1
            CUPBND(3) = DIM(2)

            CALL NDF_CREAT ('OUT', '_REAL', 3, CLWBND, CUPBND,
     :        CUBID, STATUS)

            IF (STATUS .EQ. SAI__OK) THEN

*
*             Attempt to get a pointer to the output cube.

               CALL NDF_MAP (CUBID, 'DATA', '_REAL', 'WRITE', CUBPTR,
     :           ELEM, STATUS)

*
*             If required then attempt to get a pointer to the output
*             variance array.

               IF (VARFLG) THEN
                  CALL NDF_MAP (CUBID, 'VARIANCE', '_REAL', 'WRITE',
     :              OVRPTR, ELEM, STATUS)
               ELSE
                  OVRPTR = 0
               END IF

*
*             Copy the data cube.

               NX = DIM(3)
               NY = DIM(1)
               NZ = DIM(2)

               CALL CON_RCPY (NX, NY, NZ, %VAL(CNF_PVAL(INPTR)),
     :                        %VAL(CNF_PVAL(QLPTR)),
     :           BADBIT, VARFLG, %VAL(CNF_PVAL(IVRPTR)),
     :           %VAL(CNF_PVAL(CUBPTR)),
     :           %VAL(CNF_PVAL(OVRPTR)), BADPIX, STATUS)

*
*             Construct the axes of the cube.

               CALL CON_RAXES (INLOC, CUBID, STATUS)

*
*             Copy any auxilliary information.

               CALL CON_RAUX (INLOC, CUBID, STATUS)

*
*             Copy the character components to the output NDF.

               CALL CON_RCCC (INLOC, CUBID, STATUS)

*
*             Report success or failure.

               IF (STATUS .EQ. SAI__OK) THEN
                  CALL MSG_SETI ('NX', NX)
                  CALL MSG_SETI ('NY', NY)
                  CALL MSG_SETI ('NZ', NZ)

                  CALL MSG_OUTIF (MSG__NORM,  ' ', '^NX x ^NY x ^NZ '/
     :              /'cube created successfully from Asterix file.',
     :              STATUS)

                  IF (BADPIX .GT. 1) THEN
                     CALL MSG_SETI ('BADPIX', BADPIX)

                     CALL MSG_OUTIF (MSG__NORM,  ' ', 'The cube '/
     :                 /'contains ^BADPIX bad pixels.', STATUS)

                  ELSE IF (BADPIX .EQ. 1) THEN
                     CALL MSG_OUTIF (MSG__NORM,  ' ', 'The cube '/
     :                 /'contains one bad pixel.', STATUS)

                  ELSE
                     CALL MSG_OUTIF (MSG__NORM,  ' ', 'The cube '/
     :                 /'contains no bad pixels.', STATUS)

                  END IF

               ELSE
                  CALL ERR_REP ('AST2NDF_ERR', 'AST2NDF: Failure '/
     :              /'creating the output cube from the Asterix file.',
     :              STATUS )

               END IF

            ELSE
               CALL ERR_REP ('AST2NDF_OUT', 'AST2NDF: Failure '/
     :           /'opening the output data cube.', STATUS )

            END IF

         ELSE
            CALL ERR_REP ('AST2NDF_IN', 'AST2NDF: Unable to open '/
     :        /'the input Asterix file.', STATUS)

         END IF

*
*       End the NDF context.

         CALL NDF_END (STATUS)

      END IF

      END
