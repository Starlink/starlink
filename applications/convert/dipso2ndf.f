      SUBROUTINE DIPSO2NDF (STATUS)
*+
*  Name:
*     DIPSO2NDF

*  Purpose:
*     Converts a DIPSO file (as produced by the DIPSO WRITE command) to
*     an NDF.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL DIPSO2NDF (STATUS)

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application routine reads a DIPSO format file as produced by
*     the DIPSO `WRITE' command.  The DIPSO TITLE is written to the NDF
*     TITLE.  DIPSO records bad values by means of breaks in the data
*     array.  The number and positions of these breaks are stored in
*     the DIPSO file.  This program inserts bad pixels at these break
*     positions.  The number of bad pixels inserted is based on the
*     size of the gap in the wavelength scale.  At least one bad pixel
*     is inserted at every break point.

*  Usage:
*      DIPSO2NDF IN OUT

*  ADAM Parameters:
*     IN = FILENAME (Read)
*        Input DIPSO file.  File extension ".DAT" is assumed.
*     OUT = NDF (Write)
*        Output NDF data structure.  A file extension must not be given
*        after the name.  It becomes the new current NDF.

*  Examples:
*     DIPSO2NDF OLD NEW
*        This converts the DIPSO file OLD.DAT file to the NDF file
*        NEW.SDF.
*     DIPSO2NDF SPECTRE SPECTRE
*        This converts the DIPSO file SPECTRE.DAT to the NDF called
*        SPECTRE in file SPECTRE.SDF.

*  Implementation Status:
*     -  The output NDF has a primitive data array.
*     -  The input wavelength and flux data are always of Fortran REAL 
*     type, the output data arrays are of HDS type '_REAL'.
*     -  The application assumes that the bad-pixel padding will not
*     cause the number of elements in the data array to exceed twice
*     the original number.

*  Authors:
*     Jo Murray (STARLINK)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1991 February 6th (JM):
*        Original version.
*     1992 February 4 (MJC):
*        Renamed the parameters IN and OUT for consistency with other
*        applications and with the paper documentation.  Added Usage,
*        Implementation Status, and Examples items.  Improved the
*        error reports.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE               ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'           ! Standard SAE constants
      INCLUDE 'NDF_PAR'           ! NDF_ public constants
      INCLUDE 'PRM_PAR'           ! PRIMDAT symbolic constants

*  Status:
      INTEGER STATUS              ! Global status

*  Local Constants:
      INTEGER   MAXBRK
      PARAMETER (MAXBRK = 1000)      ! Max. no. of breaks allowed in
                                     ! DIPSO data 

*  Local Variables:
      INTEGER   BREAK(MAXBRK)        ! Break array
      INTEGER   DATPTR               ! Pointer to NDF data
      INTEGER   DIM(1)               ! Accommodates dimensions
      INTEGER   FCOR                 ! Pointer to workspace
      INTEGER   FD                   ! File descriptor
      INTEGER   FDIP                 ! Pointer to DIPSO flux data
      CHARACTER FLXCOR*(DAT__SZLOC)  ! Locator to scratch space
      CHARACTER FLXDIP*(DAT__SZLOC)  ! Locator to scratch space
      INTEGER   I                    ! Loop variable
      INTEGER   IOS                  ! I/O status.
      INTEGER   LBND( 2 )            ! Accommodates NDF lower bounds
      INTEGER   NBREAK               ! Number of breaks
      INTEGER   NBYTES               ! No. of bytes to move
      INTEGER   NCOR                 ! Size of padded arrays
      INTEGER   NDF                  ! Identifier for NDF
      INTEGER   NDIM                 ! No. of dimensions
      INTEGER   NMAX                 ! Size of work array
      INTEGER   NPTS                 ! Number of elements in DIPSO file
      CHARACTER TITLE * (50)         ! Title
      INTEGER   UBND(2)              ! Accommodates NDF upper bounds
      INTEGER   UNIT                 ! Logical unit no. for Dipso file
      CHARACTER WAVCOR*(DAT__SZLOC)  ! Locator to scratch space
      CHARACTER WAVDIP*(DAT__SZLOC)  ! Locator to scratch space
      INTEGER   WAVPTR               ! Pointer to NDF axis data
      INTEGER   WCOR                 ! Pointer to workspace
      INTEGER   WDIP                 ! Pointer to DIPSO axis data
*.

*  Check inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN
      
*   First, get name and logical unit number for DIPSO file.
      CALL FIO_ASSOC ('IN', 'READ', 'UNFORMATTED', 0, FD, STATUS)
      CALL FIO_UNIT (FD, UNIT, STATUS)
      IF (STATUS.NE.SAI__OK) GO TO 999

*   Read the Title.
      READ (UNIT, IOSTAT = IOS) TITLE
      IF (IOS.NE.0) GO TO 998

*   Read the break information.
      READ (UNIT, IOSTAT = IOS) NBREAK, (BREAK(I),I = 1,NBREAK)
      IF (IOS.NE.0) GO TO 998
      NPTS = BREAK(NBREAK)
      
*   Begin an NDF context.    
      CALL NDF_BEGIN                                          

*   Create a scratch area in which to put the DIPSO wavelength array.
      DIM(1) = NPTS 
      WAVDIP = ' '
      CALL AIF_TEMP('_REAL', 1, DIM, WAVDIP, STATUS)
      CALL DAT_MAP(WAVDIP, '_REAL', 'WRITE', 1, DIM, WDIP, STATUS)

*   Create a scratch area in which to put the DIPSO flux array.
      FLXDIP = ' '
      CALL AIF_TEMP('_REAL', 1, DIM, FLXDIP, STATUS)
      CALL DAT_MAP(FLXDIP, '_REAL', 'WRITE', 1, DIM, FDIP, STATUS)

*   Create a scratch area in which to put the new wavelength array.
*   This is ten times as large in order to accommodate bad pixels which
*   may be inserted.
      NMAX = 10 * NPTS
      DIM(1) = NMAX
      WAVCOR = ' '
      CALL AIF_TEMP('_REAL', 1, DIM, WAVCOR, STATUS)
      CALL DAT_MAP(WAVCOR, '_REAL', 'WRITE', 1, DIM, WCOR, STATUS)

*   Create a scratch area in which to put the new flux array.
*   This is also twice as large in orde to accommodate bad pixels which
*   may be inserted.
      FLXCOR = ' '
      CALL AIF_TEMP('_REAL', 1, DIM, FLXCOR, STATUS)
      CALL DAT_MAP(FLXCOR, '_REAL', 'WRITE', 1, DIM, FCOR, STATUS)

*   Report any errors getting workspace.
      IF (STATUS .NE. SAI__OK) THEN
         CALL ERR_REP( 'DIPSO2NDF_WORKSPACE',
     :     'DIPSO2NDF: Error attempting to get workspace.',
     :     STATUS )
         GOTO 997
      END IF

*   Call a subroutine to read the data into the mapped data arrays.
      CALL CON_DIPRD (UNIT, NPTS, NBREAK, BREAK, NMAX, %VAL(WDIP), 
     :                %VAL(FDIP), %VAL(WCOR), %VAL(FCOR), NCOR, STATUS)

*  Check NCOR is greater than unity.
      IF (NCOR.LE.1) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETI ('NCOR', NCOR)
         CALL ERR_REP ('DIPSO2NDF_NTOOSMALL', 
     :     'DIPSO2NDF: Too few data points (^NCOR).', STATUS)

         GO TO 997              
      END IF

*  The lower pixel bound is set to unity, the upper bound to the 
*  number of elements. The number of dimensions is set to one.
      LBND(1) = 1
      UBND(1) = NCOR
      DIM(1) = NCOR
      NDIM = 1
                                                              
*   Create a new NDF file and associate an NDF identifier with it.
*   A data array of the correct size is specified via NDIM
*   and the LBND, UBND arrays.
*   Actually for the moment a primitive NDF will be created instead.
*   When all the KAPPA routines support simple NDFs the line below
*   will be restored.
*      CALL NDF_CREAT ('OUT', '_REAL', NDIM, LBND, UBND, NDF, STATUS)
      CALL NDF_CREP ('OUT', '_REAL', NDIM, DIM, NDF, STATUS)

*   Put the TITLE read from the Dipso file into the NDF title.
      CALL NDF_CPUT (TITLE, NDF, 'TITLE', STATUS)

*   Map the NDF main data array for WRITE access.
      CALL NDF_MAP (NDF, 'DATA', '_REAL', 'WRITE', DATPTR, NCOR, STATUS)

*   Map the NDF AXIS(1) array for WRITE.
      CALL NDF_AMAP (NDF, 'CENTRE', 1, '_REAL', 'WRITE', WAVPTR, NCOR, 
     :               STATUS)

*   Move corrected wavelength and flux arrays into the NDF. 
      NBYTES = VAL__NBR * NCOR
      IF ((NBYTES.GT.0) .AND. (STATUS.EQ.SAI__OK)) THEN
         CALL CON_MOVE (NBYTES, %VAL(WCOR), %VAL(WAVPTR), STATUS)
         CALL CON_MOVE (NBYTES, %VAL(FCOR), %VAL(DATPTR), STATUS)
      END IF

*   If there were no breaks in the data, set the NDF bad-pixel flag 
*   to .FALSE.
      IF (NBREAK.EQ.1) THEN
         CALL NDF_SBAD (.FALSE., NDF, 'Data', STATUS)         
      END IF

997   CONTINUE

*   Unmap and annul workspace for scratch arrays.
      CALL AIF_ANTMP(WAVDIP, STATUS)
      CALL AIF_ANTMP(FLXDIP, STATUS)
      CALL AIF_ANTMP(WAVCOR, STATUS)
      CALL AIF_ANTMP(FLXCOR, STATUS)

*   End the NDF context.                                       
      CALL NDF_END (STATUS)                                  

998   CONTINUE

*   Report any I/O errors.
      IF (IOS.NE.0) THEN
         STATUS = SAI__ERROR
         CALL ERR_FIOER ('IERR', IOS)
         CALL ERR_REP ('DIPSO2NDF_IOER', 
     :     'DIPSO2NDF: Error reading Dipso file. ^IERR', STATUS)
      END IF

999   CONTINUE

*   Shut down FIO.
      CALL FIO_CANCL ('IN', STATUS)
      CALL FIO_DEACT (STATUS)
      END             
