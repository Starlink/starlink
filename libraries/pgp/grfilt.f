      LOGICAL FUNCTION GRFILT(ITYPE)
*+
*   - - - - - - - -
*     G R F I L T     (GKS emulation of GRPCKG)
*   - - - - - - - -
*
*   Filters the list of available workstations
*
*   Inputs
*      ITYPE   i    Workstation type
*
*   Outputs
*      GRFILT  l    Include in list
*
*   Constants from GKS_PAR
*      GMI     i    Catagory - metafile input
*      GINPUT  i      "        input
*+
      IMPLICIT NONE

      INCLUDE 'GKS_PAR'


      INTEGER IERR, ITYPE, ICAT


*  Check that it exists and is of an appropriate catagory
      CALL GQWKCA(ITYPE,IERR,ICAT)
      IF (IERR.EQ.0) THEN
         GRFILT =  (ICAT.NE.GMI .AND. ICAT.NE.GINPUT)
      ELSE
         GRFILT = .FALSE.
      END IF
      END
