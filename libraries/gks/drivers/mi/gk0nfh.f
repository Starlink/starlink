C# IL>=a, OL>=0
*
*
*
*
*
      SUBROUTINE GK0NFH
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
*     Decodes the metafile header
*
*  MAINTENANCE LOG
*  ---------------
*     01/04/83  DSG   Original version stabilized
*     19/06/86  RMK   Maintenance log incorporated into main driver routine
*     19/06/89  RTP   Check for valid workstations and set flags
*
*  COMMON BLOCK USAGE
*  ------------------
*     Modify /GKYWKD/    KWKDAT - usage is described in GK0NWD
*     Modify /GKYERR/    Error
*     Modify /GKYWCA/    CSTR
*     Read   /GKYWCA/    KWKIX - metafile index
*
      INCLUDE '../../include/gkdt.par'
      INCLUDE '../../include/gkerr.cmn'
      INCLUDE '../../include/gkwca.cmn'
      INCLUDE '../../include/gkwkd.cmn'
*
*  LOCALS
*  ------
*
* Different types of metafile to handle
      INTEGER IRAL, ISUN, IUNI, IGRAL
      PARAMETER (IRAL = 1, ISUN = 2, IUNI = 3, IGRAL = 4)


      INTEGER LENDAT,IFFLAG,IRFLAG,IZERO,IONE
*
*  STREAMS USED
*  ------------
*     Stream   Connection   Comment
*
*  ERRORS
*  ------
*
*  COMMENTS
*  --------
*
*---------------------------------------------------------------------



* Read in the General Information

        LENDAT = 90
        CALL GK0NBU (2,LENDAT,CSTR(1))

        IF(KERROR.EQ.0) THEN

* Obtain Field Lengths:

* Set flag according to type of input metafile
* If current type is not recognised then generate an error
          IF( CSTR(1)(11:21).EQ.'U N I G K S' .OR.
     :        CSTR(1)(6:24).EQ.'DEVELOPED BY UNIRAS' ) THEN
                 KWKDAT(16,KWKIX) = IUNI
          ELSE IF( CSTR(1)(8:30).EQ.'Sun Microsystems SunGKS' ) THEN
                 KWKDAT(16,KWKIX) = ISUN
          ELSE IF( CSTR(1)(5:15).EQ.'GKSGRAL 7.4' ) THEN
                 KWKDAT(16,KWKIX) = IGRAL
          ELSE
*
* Default is a RAL format metafile
                 KWKDAT(16,KWKIX) = IRAL
          ENDIF


* The 'GKSM' field in the item data record.
          READ (CSTR(1)(55:56),'(I2)') KWKDAT(1,KWKIX)
* Item type indicator field.
          READ (CSTR(1)(57:58),'(I2)') KWKDAT(2,KWKIX)

* Item length indicator field.
          READ (CSTR(1)(59:60),'(I2)') KWKDAT(3,KWKIX)

* Length of field for each integer in the item.
          READ (CSTR(1)(61:62),'(I2)') KWKDAT(4,KWKIX)

* Length of field for each real in the item.
          READ (CSTR(1)(63:64),'(I2)') KWKDAT(5,KWKIX)

* Obtain Number Representations

          READ (CSTR(1)(65:66),'(I2)') IFFLAG
          READ (CSTR(1)(67:68),'(I2)') IRFLAG
          IF (IRFLAG.EQ.2) THEN
* If real represented by integer store values for 0.0 and 1.0
            READ (CSTR(1)(69:79),'(I11)') KWKDAT(17,KWKIX)
            READ (CSTR(1)(80:90),'(I11)') KWKDAT(18,KWKIX)
* Workstation variable is difference 1.0 - 0.0
            KWKDAT(18,KWKIX) = KWKDAT(18,KWKIX) - KWKDAT(17,KWKIX)
          ELSE
* If reals as reals ignore values and set range to ZERO
            READ (CSTR(1)(69:79),'(A11)') IZERO
            READ (CSTR(1)(80:90),'(A11)') IONE
            KWKDAT(17,KWKIX) = 0
            KWKDAT(18,KWKIX) = 0
          ENDIF
        ENDIF

      RETURN
      END
