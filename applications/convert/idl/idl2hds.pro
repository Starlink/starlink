
;;+
;; Name:
;;    IDL2HDS
;;
;; Purpose:
;;    To convert an IDL variable to an HDS file.
;;
;; Invocation:
;;    IDL> IDL2HDS, VAR [,"HDSNAME"]
;;
;; Parameters:
;;    VAR = Variable (Given)
;;       An idl variable to be written to an HDS file. It may be scalar,
;;       array or structure.
;;    HDSNAME = STRING (Given)
;;       A string giving the name of the HDS file to be created (without
;;       the .sdf extension). This parameter may be omitted in which case
;;       file idl2hds.sdf is created.
;;
;; Language:
;;    IDL
;;
;; Method:
;;    The system function crehds is called to write the HDS file corresponding
;;    to the given IDL variable.  crehds requires a list of the tagnames
;;    if the variable is a structure - this is provided by the ADDNAMES
;;    function.
;;
;; Authors:
;;    AJC: A.J.Chipperfield (Starlink, RAL)
;;
;; History:
;;    18-NOV-1999 (AJC):
;;       Initial Version
;;    10-MAR-2000 (AJC):
;;       Improved messages
;;-

PRO IDL2HDS, STRUC, NAME

 ON_ERROR,2
 IF ( N_PARAMS() LT 1 ) OR (N_PARAMS() GT 2) THEN BEGIN
    PRINT,""
    MESSAGE, "Incorrect number of arguments."

 ENDIF ELSE $
    IF N_ELEMENTS( STRUC ) NE 0 THEN $
       IF N_ELEMENTS(NAME) EQ 0 THEN BEGIN 
          PRINT,""
          PRINT, "IDL2HDS: Filename unspecified 'idl2hds' will be created."
          CREHDS, STRUC, ADDNAMES(STRUC,''), "idl2hds" 
       ENDIF ELSE $
          CREHDS, STRUC, ADDNAMES(STRUC,''), NAME $

    ELSE BEGIN
       PRINT,""
       MESSAGE, "The IDL variable is undefined."
    ENDELSE

END
