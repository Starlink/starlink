      SUBROUTINE CON_CCPY (GRIDFG, GRIDFL, MAPID, MDIMS, MLWBND,
     :  MUPBND, NSPEC, SPTS, CUBID, STATUS)
*+
*  Name:
*     CON_CCPY
*  Purpose:
*     Copy a SPECX map to a data cube.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CON_CCPY (GRIDFG, GRIDFL, MAPID, MDIMS, MLWBND, MUPBND,
*       NSPEC, SPTS, CUBID; STATUS)
*  Description:
*     Copy a SPECX map to a data cube.
*  Arguments:
*     GRIDFG  =  LOGICAL (Given)
*        Flag indicating whether a text file containing a schematic
*        representation of the grid is to be written.
*     GRIDFL  =  CHARACTER*(*) (Given)
*        Name of the file in which a schematic representation of the grid
*        is to be written.  The variable is unused if no file is to be
*        written.
*     MAPID  =  INTEGER (Given)
*        Identifier for the input SPECX map grid.
*     MDIMS  =  INTEGER (Given)
*        Number of dimensions in the map grid (=2).
*     MLWBND(MDIMS)  =  INTEGER (Given)
*        Lower bounds of the map grid.
*     MUPBND(MDIMS)  =  INTEGER (Given)
*        Upper bounds of the map grid.
*     NSPEC  =  INTEGER (Given)
*        Number of spectra in the map.
*     SPTS  =  INTEGER (Given)
*        Number of points in each spectrum.
*     CUBID  =  INTEGER (Given)
*        Identifier to the output data cube.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Attempt to map the input map grid.
*     If ok and required then
*       Get the title of the map.
*       Attempt to write the schematic.
*       If an error occurred then
*         Flush the error.
*         Annull the error.
*       end if
*     end if
*     Attempt to map the spectrum array.
*     If ok then
*       Attempt to map the output cube.
*       If ok then
*         Copy the SPECX map to the cube.
*         Unmap the cube.
*       else
*         Report error: failed to map the output cube.
*       end if
*       Unmap the spectrum array.
*     else
*       Report error: failed to map the spectrum array.
*     end if
*     Unmap the input map grid.
*  Authors:
*     ACD: A C Davenhall (Edinburgh)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}
*
*  History:
*     25/6/97 (ACD):
*        Original version.
*     28/8/97 (ACD):
*        First stable version.
*     2004 September 9 (TIMJ):
*        Use CNF_PVAL
*     {enter_further_changes_here}
*
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'           ! Standard Starlink constants.
      INCLUDE 'DAT_PAR'
      INCLUDE 'CNF_PAR'
*  Arguments Given:
      LOGICAL
     :  GRIDFG
      CHARACTER*(*)
     :  GRIDFL
      INTEGER
     :  BUFLEN,
     :  IPBUF,
     :  MAPID,
     :  MDIMS,
     :  MLWBND(MDIMS),
     :  MUPBND(MDIMS),
     :  NSPEC,
     :  SPTS,
     :  CUBID
*  Status:
      INTEGER STATUS             ! Global status.
*  Local Variables:
      INTEGER
     :  SPID,     ! Identifier to the spectrum array.
     :  MAPPTR,   ! Pointer to the map grid.
     :  SPPTR,    !    "    "   "  spectrum array.
     :  CUBPTR,   !    "    "   "  output data cube.
     :  ELEM,     ! Number of elements in array.
     :  PLACE,    ! NDF Placeholder.
     :  MAPX,     ! Number of points along the X-axis of the map grid.
     :  MAPY      !   "    "    "      "    "  Y- "   "   "   "   "  .
      CHARACTER
     :  TITLE*40,            ! Title of the map grid.
     :  SPXLOC*(DAT__SZLOC)  ! Locator to SPECX structure.
*.

      IF (STATUS .EQ. SAI__OK) THEN

*
*       Attempt to map the input map grid.

         CALL NDF_MAP (MAPID, 'DATA', '_INTEGER', 'READ', MAPPTR,
     :     ELEM, STATUS)

         MAPX = MUPBND(1) + 1 - MLWBND(1)
         MAPY = MUPBND(2) + 1 - MLWBND(2)

*	Allocate a buffer for a line of the output file.
         BUFLEN = MAPX + 10
         CALL PSX_CALLOC( BUFLEN, '_CHAR', IPBUF, STATUS )

*       If required then attempt to write a schematic of the map grid
*       to a text file.  Note that any errors generated are flushed and
*       annulled.

         IF (STATUS .EQ. SAI__OK  .AND.  GRIDFG) THEN
            CALL NDF_XGT0C (MAPID, 'SPECX_MAP', 'ID', TITLE, STATUS)

            CALL CON_WGRID ( %VAL(CNF_PVAL(IPBUF)), 
     :                       GRIDFL, TITLE, MAPX, MAPY,
     :                       %VAL(CNF_PVAL(MAPPTR)), STATUS, 
     :                       %VAL( CNF_CVAL( BUFLEN ) ) )

            IF (STATUS .NE. SAI__OK) THEN
               CALL ERR_FLUSH (STATUS)
               CALL ERR_ANNUL (STATUS)
            END IF
         END IF

*       Free work space.
         CALL PSX_FREE( IPBUF, STATUS )

*
*       Attempt to map the spectrum array and proceed if ok.

         CALL NDF_LOC (MAPID, 'READ', SPXLOC, STATUS)

         CALL NDF_OPEN (SPXLOC, 'MORE.POSN', 'READ', 'OLD', SPID,
     :     PLACE, STATUS)

         CALL NDF_MAP (SPID, 'DATA', '_REAL', 'READ', SPPTR, ELEM,
     :     STATUS)

         IF (STATUS .EQ. SAI__OK) THEN

*
*          Attempt to map the output cube  and proceed if ok.

            CALL NDF_MAP (CUBID, 'DATA', '_REAL', 'WRITE', CUBPTR,
     :        ELEM, STATUS)

            IF (STATUS .EQ. SAI__OK) THEN

*
*             Attempt to copy the cube.

               CALL CON_SPCPY (MAPX, MAPY, %VAL(CNF_PVAL(MAPPTR)), 
     :                         NSPEC, SPTS,
     :           %VAL(CNF_PVAL(SPPTR)), %VAL(CNF_PVAL(CUBPTR)), STATUS)

*
*             Unmap the cube.

               CALL NDF_UNMAP (CUBID, 'DATA', STATUS)

            ELSE
               CALL ERR_REP ('CON_CCPY_MC', 'CON_CCPY: Failed to '/
     :           /'map the output cube.', STATUS)

            END IF

*
*          Unmap the spectrum array.

            CALL NDF_UNMAP (SPID, 'DATA', STATUS)

         ELSE
            CALL ERR_REP ('CON_CCPY_MS', 'CON_CCPY: Failed to '/
     :           /'map the spectrum array.', STATUS)

         END IF

*
*       Unmap the map grid.

         CALL NDF_UNMAP (MAPID, 'DATA', STATUS)

      END IF

      END
