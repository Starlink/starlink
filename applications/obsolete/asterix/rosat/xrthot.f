*+  XRTHOT - Writes hotspot header into hotspot HDS file
      SUBROUTINE XRTHOT(STATUS)
*    Description :
*     Reads the HOTSPOT file FITS header and writes some of the values into
*     the Header file.
*    Environment parameters :
*      INPUT       READ       Name of logfile from XRTDISK
*      ROOTNAME    READ       Name of hotspot HDS file
*    Method :
*     <description of how the application works - for programmer info>
*    Deficiencies :
*    Bugs :
*    Authors :
*     Richard Saxton (LTVAD::RDS)
*    History :
*     9-Mar-94: Now uses origin fits file for data (vers 1.5-2) (LTVAD::JKA)
*    24-Apr-94: (v1.7-0) for new asterix release
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PAR_ERR'
*    Global variables :
*     <global variables held in named COMMON>
*    Structure definitions :
      INCLUDE 'XRTLIB(INC_XRTHEAD)'
*    Status :
      INTEGER STATUS
*    Function declarations :
      INTEGER CHR_LEN
        EXTERNAL CHR_LEN
*    Local constants :
*    Local variables :
      RECORD / XRT_HEAD / HEAD
      CHARACTER*80 FNAME,HNAME,RTNAME  ! file names
      CHARACTER*(DAT__SZLOC) HLOC      ! Locator to header file
      DOUBLE PRECISION SPOTS(MAXSPOT*3)
      INTEGER LP,NSPOT,LUNIT,ISTATUS,BLKSIZE
*    Local data :
*     <any DATA initialisations for local variables>
*    Version :
      CHARACTER*30 VERSION
      PARAMETER (VERSION = 'XRTHOT version 1.7-0')
*-
      CALL AST_INIT(STATUS)
      CALL MSG_PRNT(VERSION)
*
*     Get input fits hotspot filename
      CALL USI_GET0C('INPUT',FNAME,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 999
*
*     Open fits file
      CALL FIO_GUNIT(LUNIT, ISTATUS)
      CALL FTOPEN(LUNIT,FNAME,0,BLKSIZE,ISTATUS)
      IF (ISTATUS .NE. 0) THEN
         CALL MSG_PRNT('Error opening hotspot file')
         GOTO 999
      ENDIF
*
*     get the hotspot array
      CALL GHISTND(LUNIT,'BAD_PIX_X_Y_R',SPOTS,MAXSPOT*3,NSPOT,ISTATUS)
*
*     close the fits file
      CALL FTCLOS(LUNIT,ISTATUS)
      CALL FIO_PUNIT(LUNIT,STATUS)
*
*     A fitsio error has occured
      IF (ISTATUS.NE.0) THEN
         CALL MSG_PRNT('Error detected reading FITS file')
         GOTO 999
      ENDIF
*
*     get the header file/rootname
      CALL USI_GET0C('ROOTNAME',RTNAME,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 999
*
*     read the header infotmation
      CALL RAT_GETXRTHEAD(RTNAME,HEAD,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 999
*
*     Write spot values into the appropriate arrays
      HEAD.NSPOT = NSPOT / 3
      DO LP=1,HEAD.NSPOT
          HEAD.XSPOT(LP) = SPOTS(1+(LP-1)*3)
          HEAD.YSPOT(LP) = SPOTS(2+(LP-1)*3)
          HEAD.SPOTRAD(LP) = SPOTS(LP*3)
      ENDDO
*
*     inform user of hotspots found
      CALL MSG_SETI('NSPOT', HEAD.NSPOT)
      CALL MSG_PRNT('^NSPOT hotspots/deadspots in field of view')
*
*     Open the header file for update
      HNAME = RTNAME(1:CHR_LEN(RTNAME))//'_hdr'
      CALL HDS_OPEN(HNAME, 'UPDATE', HLOC, STATUS)
*
      IF (STATUS .NE. SAI__OK) THEN
         CALL MSG_PRNT('Cannot open header file for update')
         GOTO 999
      ENDIF
*
*     Write the header structure into the header file
      CALL RAT_PUTHEAD(HLOC,'HEAD',HEAD,STATUS)
*
*     update the history information
      CALL HIST_ADD(HLOC,VERSION,STATUS)
*
*     close header file
      CALL HDS_CLOSE(HLOC,STATUS)
*
999   CONTINUE
*
      IF (STATUS.NE.SAI__OK) THEN
         CALL ERR_REP(' ',VERSION,STATUS)
      ENDIF
*
      CALL AST_CLOSE(STATUS)
*
      END
