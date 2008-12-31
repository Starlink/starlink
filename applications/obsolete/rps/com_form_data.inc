*+COM_FORM_DATA    Include File, holds form data for FORM_INIT, VALS
 
      CHARACTER*17 FIELDS(100,6)		! Field names.
      CHARACTER*10 UNITS(100,6) 		! Units.
     & ,           FRMT(100,6)  		! Field formats.
      INTEGER      COL(4) , ROW(4)	! Position of displays.
     & ,           DEC_POS , RA_POS	! Position of angle fields.
     & ,           VDID(6)		! Visual display IDs.
     & ,  HRIFLD , WFCFLD , DUMFLD	! Position of instrument fields.
     & ,  CONTG , MONIT , PHASE , COORD	! Position of constraint mode fields.
      INTEGER CONDOFF(50,4)		! Conditional offset
      CHARACTER*7 CONDNULL(50,4)	! null string

      INTEGER FLENTH(100,6)		! Length in record (added 2/2/94)
      COMMON / FORM_COMM / FRMT , FIELDS , VDID , ROW , COL , UNITS 
     & ,                   RA_POS , DEC_POS , HRIFLD , WFCFLD , DUMFLD
     & ,      CONTG , PHASE , MONIT , COORD, CONDOFF, CONDNULL, FLENTH

