*  History:
*     06 Dec 1993 (hme):
*        Add HDS locators to top level and to SPECTRUM array.
*        Split into separate common blocks for character and numeric
*        data.
*     14 Dec 1993 (hme):
*        Reunite the common blocks. Hopefully dumping would work then.
*     24 Apr 1993 (rp)
*        (non Unix version only); comment out NDF stuff.
C---------------------------------------------------------------------
C
C                   FILES.INC
C                   ---------

C  Include file which holds information on currently open data files

C  For UNIX version need to define DAT__ parameters, so include that here.
C  Note RFWS comments in HDS_DIR:DAT_PAR.FOR; include SAE_PAR also.
C  Commented out because HME has included them in the FV4 (and MV4) routines;
C  see how much trouble this causes elsewhere in SPECX...

*     INCLUDE  'SAE_PAR'
*     INCLUDE  'DAT_PAR'

C  ... and the real file

      INTEGER   MAX_DFILES
      PARAMETER (MAX_DFILES = 8)

      CHARACTER * ( 80 ) FILNAMS(MAX_DFILES) ! File names
      CHARACTER * ( 2 ) ACCESS(MAX_DFILES)   ! Access code - 'R', 'W', or 'RW'

      CHARACTER * ( DAT__SZLOC )
     :   TOPLOC(MAX_DFILES),                 ! Top level HDS locators
     :   SPXLOC(MAX_DFILES)                  ! Locators to SPECTRUM arrays
      INTEGER   FILELUNS(MAX_DFILES)         ! Logical units files are open on

      COMMON /FILES/  FILNAMS, ACCESS, FILELUNS, TOPLOC, SPXLOC
      SAVE   /FILES/

C---------------------------------------------------------------------
