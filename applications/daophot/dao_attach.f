**==attach.spg  processed by SPAG 4.54K  at 14:22 on  4 Oct 1996
C
C=======================================================================
C
C This file contains UNIX specific subroutines relating to the
C input and output of data-- either from CCD picture files or from
C data files containing stellar coordinates and photometric
C quantities.
C
C   This was originally the:
C            OFFICIAL DAO VERSION:  1985 August 16
C
C History :
C   It was modified on 19-FEB-1992, to use NDF files. The NDF files
C   are expected to contain standard Starlink NDF structures as their
C   top-level object. This modified version, called UNIXSUBS_NDF, was
C   copied from the VAX/VMX version VAXSUBS_NDF.
C
C                             Nick Eaton (DUVAD::NE)
C
C endhistory
C
C***********************************************************************
C
C Current contents (A * designates a subroutine called directly by
C      a DAOPHOT command.  The others are called from within other
C      subroutines.)
C
C N.B. A # symbol indicates routines which have been modified for HDS
C      use.
C
C * ATTACH  # interprets an ATTACH command and opens a picture.
C
C    CLPIC  # closes a picture file.
C
C   COPPIC  # creates a new picture file that is an exact copy of the
C           currently open picture file, and opens the copy.
C
C   DELPIC  deletes a disk picture file.
C
C *   LIST  # allows the user to examine the contents of a picture
C           file header.
C              *** Original written by Keith Shortridge, Caltech ***
C              *** This NDF version does not support the LIST command ***
C
C   RDARAY  # reads a rectangular data subarray from a picture file.
C
C   WRARAY  # writes a rectangular data subarray into a picture file.
C
C   INFILE  opens a disk data file for reading only.
C
C   OUTFIL  creates a new disk data file and opens it for writing.
C
C   CLFILE  closes a disk data file.
C
C   ASKFILE
C
C   FABORT
C
C   BYEBYE
C
C   OOPS
C
C   CASE
C
C   OVRWRT
C
C***********************************************************************
C
C
************************************************************************

      SUBROUTINE ATTACH(NAFILE,OPEN)

*+
*  Name :
*     ATTACH
*
*  Purpose :
*     Interpret an ATTACH command and open an NDF structure
*
*  Invocation :
*     CALL ATTACH( NAFILE, OPEN )
*
*  Description :
*     This is a re-written version of the original Daophot ATTACH
*     routine which accessed Figaro-style data structures. This version
*     uses NDF data files. The present version is written to the
*     specification of the Figaro version, except that the file type is
*     '.SDF' by default:
*
*  Arguments :
*     NAFILE = CHARACTER*(*) (Given)
*        The name of the NDF file to be attached.  A file type of '.SDF'
*        is assumed if none is supplied.  The file must contain the (NDF)
*        data structure to be processed as its top-level object.
*     OPEN = LOGICAL (Given and Returned)
*        Whether the container file is open.
*
*  Algorithm :
*     If there is already a 'DATA' file open, it is closed.
*     A file name is prompted for if it was not supplied.
*     The container file is opened and the OPEN flag is set.
*     The TITLE component is found and displayed (if it exists).
*     The DATA_ARRAY component is found and its shape is checked.
*     It is rejected if it is not 2-dimensional.
*     If the file was rejected it is closed and the OPEN flag is
*     cancelled.
*
*  Deficiencies :
*     The NDF must appear as the top-level object in the data file.
*     The routine takes no specific account of bad pixels, apart from
*     outputting a message if they are present, although the
*     Daophot "minimum valid data value" will cope in most cases.
*
*  Bugs :
*     <description of any "bugs" which have not been fixed>
*
*  Authors :
*     RFWS: R.F. Warren-Smith (Durham University)
*     NE: Nick Eaton (Durham University)
*     MBT: Mark Taylor (STARLINK)
*
*  History :
*     19-MAY-1988 (RFWS):
*        HDS/NDF version derived from original Daophot ATTACH routine.
*      6-DEC-1991 (NE):
*        NDF version derived from HDS version.
*     19-FEB-1992 (NE):
*        Unix version.
*     10-JUL-2000 (MBT):
*        Modified to use NDF throughout (not bare HDS).
*-
*  Type Definitions :
      IMPLICIT NONE

*  Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'DAT_ERR'

*  Global variables :
*    ...Daophot file names:
      CHARACTER*(30) COOFILE
      CHARACTER*(30) MAGFILE
      CHARACTER*(30) PSFFILE
      CHARACTER*(30) PROFILE
      CHARACTER*(30) GRPFILE
      COMMON /FILENAM/ COOFILE , MAGFILE , PSFFILE , PROFILE , GRPFILE

*    ...Daophot picture size:
      INTEGER NCOL
      INTEGER NROW
      COMMON /SIZE  / NCOL , NROW

*    Common block for NDF information
      INCLUDE 'ndf_cmn'

*  Arguments Given :
      CHARACTER*(*) NAFILE

*  Arguments Given and Returned
      LOGICAL*1 OPEN

*  External references :
      CHARACTER*(30) SWITCH

*  Local variables :
      LOGICAL BAD               ! Bad pixel flag
      INTEGER STATUS            ! HDS error status
      INTEGER DIM(2)            ! Size of DATA_ARRAY component
      INTEGER NDIM              ! Number of DATA_ARRAY dimensions
      CHARACTER*(72) TITLE      ! NDF title string, from its TITLE component

*  Local data :

*.

*   Initialise the HDS status variable.
      STATUS = SAI__OK

*   If there is already a data file open, close it.  Issue a warning
*   if an error occurs, but carry on.
      IF ( OPEN ) THEN
         CALL NDF_ANNUL(NDF_IDATA,STATUS)
         IF ( STATUS.NE.SAI__OK ) THEN
            CALL ERR_REP(' ','ATTACH - warning, error closing file',
     :                   STATUS)
            CALL ERR_FLUSH(STATUS)
            CALL ERR_ANNUL(STATUS)
         END IF
         OPEN = .FALSE.
      END IF

*   If NAFILE wasn't defined in the ATTACH command line, ask for it
*   here.  Quit if "end of file" (ctrl-Z) was entered.
      IF ( NAFILE.EQ.' ' ) THEN
         CALL TBLANK
         CALL ASKFILE('Enter file name:',NAFILE)
         IF ( NAFILE.EQ.'END OF FILE' ) RETURN
      END IF

*   Try to open the file.  Check for errors and report them.
      CALL NDF_FIND(DAT__ROOT,NAFILE,NDF_IDATA,STATUS)
      IF ( STATUS.NE.SAI__OK ) THEN
         CALL TBLANK
         CALL ERR_REP(' ','ATTACH - error opening file',STATUS)

*   If there is no error, set the OPEN flag and assign initial default
*   sequential filenames for use later.
      ELSE
         OPEN = .TRUE.
         COOFILE = SWITCH(NAFILE,'.COO')
         MAGFILE = SWITCH(NAFILE,'.AP')
         PSFFILE = SWITCH(NAFILE,'.PSF')
         PROFILE = SWITCH(NAFILE,'.NST')
         GRPFILE = SWITCH(NAFILE,'.GRP')

*   Output a message if there are bad pixels present
         CALL NDF_BAD(NDF_IDATA,'Data',.TRUE.,BAD,STATUS)
         IF ( BAD ) CALL MSG_OUT(' ',
     :                    'NOTE - Bad pixels are present in the data'
     :                    ,STATUS)

*   Try to read the TITLE component
         TITLE = ' '
         CALL NDF_CGET(NDF_IDATA,'Title',TITLE,STATUS)

*   If successful, display the title
         IF ( TITLE.NE.' ' ) THEN
            CALL TBLANK
            CALL MSG_OUT(' ','Title: ' // TITLE,STATUS)
         END IF

*   Find the shape of the DATA_ARRAY component
         CALL NDF_DIM(NDF_IDATA,2,DIM,NDIM,STATUS)
         IF ( STATUS.EQ.SAI__OK .AND. NDIM.NE.2 ) THEN
            CALL TBLANK
            STATUS = SAI__ERROR
            CALL ERR_REP( ' ',
     :           'ATTACH - DATA_ARRAY component is not 2-dimensional'
     :           ,STATUS)
         END IF

*   If there has been an error, close the file and indicate trouble.
         IF ( STATUS.NE.SAI__OK ) THEN
            CALL NDF_ANNUL(NDF_IDATA,STATUS)
            CALL ERR_ANNUL(STATUS)
            OPEN = .FALSE.
            CALL TBLANK
            CALL MSG_SETC('FILE',NAFILE)
            CALL MSG_OUT(' ','Failed to attach ^FILE',STATUS)

*   If the DATA_ARRAY is OK, save its dimensions.
         ELSE
            NCOL = DIM(1)
            NROW = DIM(2)
         END IF

*   End of "no error finding the NDF" condition.
      END IF

*   Exit routine.
      END
