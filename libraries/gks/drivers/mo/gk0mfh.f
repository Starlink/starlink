*
*
*
*
*
      SUBROUTINE GK0MFH
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    WORKSTATION
*  Author:             DSG
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Constructs SGKSM file header.
*
*  MAINTENANCE LOG
*  ---------------
*     11/03/83  DSG   Original version stabilized
*     19/06/86  RMK   Maintenance log incorporated into main driver routine.
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../../include/gkmc.par'
*
*  LOCALS
*  ------
      INTEGER IHFLD,ITFLD,LFIELD,IRFLD,IFIELD,IFORMT,IRI,
     :        IZERO,IONE,IFUNC,IDAY,MONTH,IYEAR
      CHARACTER STRING*90, SGKSM*5, SDATE*8, SVERSN*2,
     : SLASH, SNAME*39
      PARAMETER (SGKSM='GKSM ', SVERSN=' 1', SLASH='/')
      PARAMETER (IHFLD=1,ITFLD=3,LFIELD=7,IFIELD=4,IRFLD=9)
      PARAMETER (IFORMT=1,IRI=1,IZERO=0,IONE=1)
*
*  COMMENTS
*  --------
*     The format of the file header described in Annexe E is
*     adopted. GIN 66 gives the reasons for the values adopted
*     for the field lengths and number representation.
*     The field lengths must be the same as those used in
*     routine GK0MIT, so should they be passed in COMMON?
*
*---------------------------------------------------------------------


* Pick up the system title from GKMC.PAR - only want 39 chars
      SNAME = CVERS

* Obtain the date from the system
      CALL GKDATE(IYEAR,MONTH,IDAY)
      IYEAR = MOD(IYEAR,100)

* Format it for the metafile header
      WRITE(SDATE,'(I2.2,2(A1,I2.2))') IYEAR,SLASH,MONTH,SLASH,IDAY

* Format the whole header
      WRITE(STRING,1000) SGKSM,SNAME,SDATE,SVERSN,IHFLD,ITFLD,
     :  LFIELD,IFIELD,IRFLD,IFORMT,IRI,IZERO,IONE
 1000 FORMAT(A5,A39,A8,A2,5I2,2I2,2I11)

* Send it
      IFUNC=2
      CALL GK0MBU(IFUNC,STRING)

      RETURN
      END
