      SUBROUTINE NDG1_SDFEX( IGRP1, IGRPD, IGRPB, IGRPT, IGRPH, IGRPS, 
     :                       LOC, DIR, NAM, TYP, SLICE, FOUND, STATUS )
*+
*  Name:
*     NDG1_SDFEX

*  Purpose:
*     Searches for NDFs within an HDS object.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDG1_SDFEX( IGRP1, IGRPD, IGRPB, IGRPT, IGRPH, IGRPS, 
*                      LOC, DIR, NAM, TYP, SLICE, FOUND, STATUS )

*  Description:
*     The supplied HDS object is searched for NDFs, and the paths to any 
*     NDFs found within it are appended to the end of group IGRP.
*     Here, an NDF is defined as an HDS structure containing a component 
*     called DATA_ARRAY. NDFs within NDFs (i.e. contained within an NDF 
*     extension) are not included in the returned group.
*
*     The supplied fields are stored in the other groups for each found
*     NDF.

*  Arguments:
*     IGRP = INTEGER (Given)
*        An identifier for the group to which the NDF paths should
*        be appended.
*     IGRPD = INTEGER (Given)
*        An identifier for the group to which the directory field for each 
*        NDF found should be appended.
*     IGRPB = INTEGER (Given)
*        An identifier for the group to which the file base name for each 
*        NDF found should be appended.
*     IGRPT = INTEGER (Given)
*        An identifier for the group to which the file type for each 
*        NDF found should be appended.
*     IGRPH = INTEGER (Given)
*        An identifier for the group to which the HDS component path for 
*        each NDF found should be appended.
*     IGRPS = INTEGER (Given)
*        An identifier for the group to which the NDF slice specification 
*        for each NDF found should be appended.
*     LOC = CHARACTER * ( * ) (Given)
*        The locator to the object to be searched for NDFs.
*     DIR = CHARACTER * ( * ) (Given)
*        The directory field to store in IGRPD.
*     NAM = CHARACTER * ( * ) (Given)
*        The filer base name field to store in IGRPB.
*     TYP = CHARACTER * ( * ) (Given)
*        The file type field to store in IGRPT.
*     SLICE = CHARACTER * ( * ) (Given)
*        An NDF slice specification to append to the path of each NDF
*        found in the supplied object. This is stored in IGRPS (but not
*        IGRPH).
*     FOUND = LOGICAL (Given and Returned)
*        Returned .TRUE. if one or more NDFs were found in the supplied
*        object. Returned unchanged otherwise.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     15-FEB-1999 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'GRP_PAR'          ! GRP constants.
      INCLUDE 'DAT_PAR'          ! HDS constants.

*  Arguments Given:
      INTEGER IGRP1
      INTEGER IGRPD
      INTEGER IGRPB
      INTEGER IGRPT
      INTEGER IGRPH
      INTEGER IGRPS
      CHARACTER LOC*(*)
      CHARACTER DIR*(*)
      CHARACTER NAM*(*)
      CHARACTER TYP*(*)
      CHARACTER SLICE*(*)
      
*  Arguments Given and Returned:
      LOGICAL FOUND

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER CLOC*(DAT__SZLOC)! Locator to component or array cell
      CHARACTER PATH*(GRP__SZNAM)! Path to NDF object 
      CHARACTER PATH2*(GRP__SZNAM)! Path to NDF object with full dir path
      CHARACTER VLOC*(DAT__SZLOC)! Locator to vectorised array
      CHARACTER XLOC*(DAT__SZLOC)! Locator to next object to be checked
      CHARACTER DIR2*(GRP__SZFNM)! Full directory spec
      CHARACTER BN2*50           ! File base name 
      CHARACTER SEC2*1           ! NDF section (should always be blank)
      CHARACTER SUF2*(GRP__SZFNM)! Suffix (eg HDS comp. path)
      INTEGER DIM( DAT__MXDIM )  ! Dimensions of array component
      INTEGER ICELL              ! Cell index
      INTEGER ICOMP              ! Component index
      INTEGER IGRP2              ! Group holding locators to be checked
      INTEGER ILOC               ! No. of locators checked so far 
      INTEGER LPATH              ! Used length of PATH
      INTEGER LPATH2             ! Used length of PATH2
      INTEGER NCELL              ! No. of cells in array
      INTEGER NCOMP              ! No. of components in object
      INTEGER NDIM               ! No. of dimensions in array component
      INTEGER NLOC               ! No. of locators in group IGRP2
      LOGICAL ISANDF             ! Is object an NDF?
      LOGICAL STRUCT             ! Is object a structure?
      INTEGER IPATH              ! Index of start of HDS component path
      INTEGER IAT                ! Index of end of directory field
      INTEGER DOT                ! Index of first "."
      INTEGER PAR                ! INdex of first ")"
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Create a GRP group to hold locators to the objects to be checked.
      CALL GRP_NEW( 'Locators', IGRP2, STATUS )

*  Store a clone of the locator for the supplied object in the group.
*  This is initially the only object to be checked. If this object is
*  not an NDF but is found to contain sub-structures, locators for these
*  will be added to the end of the group and will be checked in their turn.
      CALL DAT_CLONE( LOC, XLOC, STATUS )
      CALL GRP_PUT( IGRP2, 1, XLOC, 0, STATUS )

*  Indicate that the group currently contains one locator.
      NLOC = 1

*  Initialise the number of locators checked so far.
      ILOC = 0

*  Loop until the last locator in the group has been checked.
      DO WHILE( ILOC .LT. NLOC .AND. STATUS .EQ. SAI__OK )

*  Get the next locator to be checked.
         ILOC = ILOC + 1
         CALL GRP_GET( IGRP2, ILOC, 1, XLOC, STATUS )

*  Only check structures (not primitive values).
         CALL DAT_STRUC( XLOC, STRUCT, STATUS )
         IF( STRUCT ) THEN

*  If this object contains a component called DATA_ARRAY it is assumed
*  to be an NDF. Append its path to the group.
            CALL DAT_THERE( XLOC, 'DATA_ARRAY', ISANDF, STATUS ) 

*  If so, just add the full object path to the end of the supplied group.
            IF( ISANDF ) THEN

*  Assign the object path to a message token.
               CALL DAT_MSG( 'PATH', XLOC ) 

*  Store the message token in a local variable.
               CALL MSG_LOAD( ' ', '^PATH', PATH2, LPATH2, STATUS )

*  Split the object path up into directory, basename, suffix and section
*  (the section specified will be blank since DAT_MSG does not know about
*  NDF sections).
               CALL NDG1_FPARS( PATH2( : LPATH2 ), DIR2, BN2, SUF2, 
     :                          SEC2, STATUS )

*  DAT_MSG always puts in a full directory specification, even if the NDF
*  is in the current working directory. To avoid this, reconstruct the 
*  object path, replacing the directory path with the supplied directory 
*  path (which will be blank if the NDF is in the current working 
*  directory). Note the index of the first character in the HDS component 
*  path in this string. This is the first character following the file 
*  base name.
               PATH = ' '
               LPATH = 0
               CALL CHR_APPND( DIR, PATH, LPATH )
               CALL CHR_APPND( BN2, PATH, LPATH )
               IPATH = LPATH + 1
               CALL CHR_APPND( SUF2, PATH, LPATH )

*  Save the individual fields in the supplied groups.
               CALL GRP_PUT( IGRPD, 1, DIR, 0, STATUS )
               CALL GRP_PUT( IGRPB, 1, NAM, 0, STATUS )
               CALL GRP_PUT( IGRPT, 1, TYP, 0, STATUS )
               IF( IPATH .LE. LPATH ) THEN
                  CALL GRP_PUT( IGRPH, 1, PATH( IPATH : ), 0, STATUS )
               ELSE
                  CALL GRP_PUT( IGRPH, 1, ' ', 0, STATUS )
               END IF
               CALL GRP_PUT( IGRPS, 1, SLICE, 0, STATUS )
               
*  Append any supplied slice spec. to the path.
               CALL CHR_APPND( SLICE, PATH, LPATH )

*  Save the full NDF spec. in the supplied group.
               CALL GRP_PUT( IGRP1, 1, PATH( : LPATH ), 0, STATUS )
               FOUND = .TRUE.      

*  If the object is not an NDF...
            ELSE

*  See if it is a scalar or an array.
               CALL DAT_SHAPE( XLOC, DAT__MXDIM, DIM, NDIM, STATUS ) 

*  If it is a scalar...
               IF( NDIM .EQ. 0 ) THEN

*  Find the number of components in the object.
                  CALL DAT_NCOMP( XLOC, NCOMP, STATUS ) 

*  Add a locator for each component to the end of the group of locators
*  to be checked.
                  DO ICOMP = 1, NCOMP
                     CALL DAT_INDEX( XLOC, ICOMP, CLOC, STATUS ) 
                     CALL GRP_PUT( IGRP2, 1, CLOC, 0, STATUS )
                  END DO

*  Increment the number of locators in the group.
                  NLOC = NLOC + NCOMP

*  If it is an array...
               ELSE

*  Get a vectorised (i.e. 1-d) version of the array.
                  CALL DAT_VEC( XLOC, VLOC, STATUS )
                    
*  Get the length of the vectorized array.
                  CALL DAT_SIZE( VLOC, NCELL, STATUS ) 

*  Add a locator for each cell of the array to the end of the group of locators
*  to be checked.
                  DO ICELL = 1, NCELL
                     CALL DAT_CELL( VLOC, 1, ICELL, CLOC, STATUS )
                     CALL GRP_PUT( IGRP2, 1, CLOC, 0, STATUS )
                  END DO

*  Increment the number of locators in the group.
                  NLOC = NLOC + NCELL

*  Annul the locator to the vectorised array.
                  CALL DAT_ANNUL( VLOC, STATUS )

               END IF
 
            END IF

         END IF

*  Annul the locator just checked.
         CALL DAT_ANNUL( XLOC, STATUS )

      END DO

*  Delete the GRP group.
      CALL GRP_DELET( IGRP2, STATUS )

      END
