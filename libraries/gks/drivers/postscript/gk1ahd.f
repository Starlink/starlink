*---------------------------------------------------------------------
      SUBROUTINE GK1AHD (IFLAG)
*---------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:  (Part of) workstation driver
*
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Send PostScript Header to the external file.
*
*  MAINTENANCE LOG
*  ---------------
*     Maintenance log is incorporated in main driver
*
*
*  ARGUMENTS
*  ---------
*     INP IFLAG  Flag to indicate whether this is a header
*                for the whole file or page

      INTEGER IFLAG
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../../include/gkdt.par'
      INCLUDE '../../include/gkio.par'
      INCLUDE '../../include/gkmc.par'
      INCLUDE '../../include/gkerr.cmn'
      INCLUDE '../../include/gkwdt.cmn'
      INCLUDE '../../include/gkwkd.cmn'
      INCLUDE '../../include/gkwca.cmn'

*
*     Intrinsic functions declaration
*
      INTRINSIC INDEX

*
*  LOCALS
*  ------
*
*  AUXS    - Temporary Character variable
*  I       - Count variable
*  IDAY    - Holds the day of a month  in a current date (1-31)
*  IPAGE   - Value of flag indicating page header
*  IPWIDT  - Display Area's actual width (in points).
*  IREM    - Dummy integer, required by the buffering routine.
*  IWHOLE  - Value of flag indicating header for whole file
*  IYEAR   - Holds the year in a current year (Current Year less 1900)
*  MONTH   - Holds the month of a year in a current date (1-12).
*  S       - Character variable, via which chunks of PostScript are sent for
*            buffering.
*

*     Values of IFLAG
      INTEGER    IWHOLE,     IPAGE
      PARAMETER (IWHOLE = 0, IPAGE = 1)

*     Integer workspace offset parameters
      INTEGER    IFORMT
      PARAMETER (IFORMT = 16)

*     EPSF indicator
      INTEGER IIEPSF
      PARAMETER (IIEPSF = 1)

*     Conversion factor from Metres to Points
      REAL    SCFACT
      PARAMETER (SCFACT = 72.0/0.0254)

      INTEGER I,IYEAR,MONTH,IDAY,IREM,IPWIDT
*
      CHARACTER S*255, AUXS*6, DUMMY
*
*  ALGORITHM
*  ---------
*     Write PostScript Structuring Conventions Header to the external
*     file for the whole file or a new page as indicated by the flag.
*     Also, denote EPSF file format when applicable.
*
* --------------------------------------------------------------
*

*     Determine whether the header is for the whole file or a page.
      IF(IFLAG .EQ. IWHOLE)THEN
*     Header for whole file
*        Buffer the PS identification mark followed by the version of the
*        structuring conventions.
         CALL GKFOCO(KIOPB, '%!PS-Adobe-2.1', IREM)

*        Find out whether dealing with an EPSF workstation and add the
*        EPSF identification accordingly.
         IF(KWKDAT(IFORMT,KWKIX).EQ.IIEPSF) THEN
            CALL GKFOCO(KIOPB, ' EPSF-2.0', IREM)
         ENDIF

*        Send the first line now.
         CALL GKFOCO(KIOSN,DUMMY,IREM)

*
*        GKS is taken to be the Creator of the file - take the GKS version
*        identification string and concatenate the workstation type to it.
*
         WRITE (AUXS,50) KWKTYP
   50    FORMAT(I5)
*        Find the left limit of CVERS
         I=INDEX(CVERS,'   ')
         S=CVERS
         S(I:18+I)=' - Workstation'//AUXS(1:5)
         CALL GKFOCO(KIOPB,'%%Creator: '//S(1:I+18),IREM)
         CALL GKFOCO(KIOSN,DUMMY,IREM)

*
*        The date is obtained by the GKS utility and then sent.
*
         CALL GKDATE(IYEAR,MONTH,IDAY)
         WRITE(S,100) IDAY, MONTH, 1900+IYEAR
  100    FORMAT('%%Creation date: ',2(I2.2,'/'),I4)
         CALL GKFOCO(KIOPB,S(1:27),IREM)
         CALL GKFOCO(KIOSN,DUMMY,IREM)

*
*        Number of pages in the file will be specified in the Trailer section.
*
         CALL GKFOCO(KIOPB,'%%Pages: (atend)', IREM)
         CALL GKFOCO(KIOSN,DUMMY,IREM)

*
*        Fonts used in the file will be specified in the Trailer section.
*
         CALL GKFOCO(KIOPB, '%%DocumentFonts: (atend)',IREM)
         CALL GKFOCO(KIOSN,DUMMY,IREM)

*
*        For EPSF, Bounding Box information will be specified in the
*        Trailer section.
*        For non-EPSF this is done for each page in the Page comments.
*
         IF(KWKDAT(IFORMT,KWKIX) .EQ. IIEPSF)THEN
           CALL GKFOCO(KIOPB,'%%BoundingBox: (atend)',IREM)
           CALL GKFOCO(KIOSN,DUMMY,IREM)
         ENDIF

*
*        Signal end of the header section.
*

         CALL GKFOCO(KIOPB, '%%EndComments', IREM)
         CALL GKFOCO(KIOSN,DUMMY,IREM)

      ELSEIF(IFLAG .EQ. IPAGE)THEN
*     Page Header
*
*       Page Bounding Box information will be specified in the
*       PageTrailer section for non-EPSF files.
         IF(KWKDAT(IFORMT,KWKIX) .NE. IIEPSF)THEN
           CALL GKFOCO(KIOPB,'%%PageBoundingBox: (atend)',IREM)
           CALL GKFOCO(KIOSN,DUMMY,IREM)
         ENDIF
*
      ENDIF

 9999 CONTINUE
      END
