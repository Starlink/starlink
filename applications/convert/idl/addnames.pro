;;+
;; ADDNAMES
;;
;; Purpose:
;;    To obtain a list of component names for an hierarchically organized
;;    IDL structure
;;
;; Invocation:
;;    IDL> NEWLIST = ADDNAMES( STRUC, PREFIX, LIST )
;;
;; Parameters:
;;    STRUC = Variable (Given)
;;       The variable whose tags, if any, are to be listed.
;;    PREFIX = STRING (Given)
;;       A string to be prefixed to the tagnames.
;;    LIST = STRING[] (Given)
;;       A list of strings to which the tagnames are to be appended.
;;
;; Returned Value:
;;    NEWLIST = STRING[]
;;       The tagnames for this variable and any subsidiary components
;;       with .PREFIX. prefixed appended to the given LIST.
;; Language:
;;    IDL
;;
;; Method:
;;    If the given 'structure' parameter is indeed a structure, a list of
;;    its tagnames, prefixed by the given prefix followed by '.', is 
;;    obtained in a string array. A new string array is created consisting 
;;    of the given string array with the prefixed tagnames array appended. 
;;    ADDNAMES is then called recursively for each of the new tags so that
;;    the entire hierarchy below the given structure is listed, with the
;;    complete pathname of each tag. The new string array is returned.
;;    If the given 'structure' parameter is not a structure, the given list
;;    is returned.
;;
;; Authors:
;;    AJC: A.J.Chipperfield (Starlink, RAL)
;;
;; History:
;;    18-NOV-1999 (AJC):
;;       Initial Version
;;-

        FUNCTION addnames,name,prefix,list

;      Return to caller if error
        ON_ERROR, 2

;  Get info about the given variable
        DIMS = SIZE(name)

;  If it's a structure - add the tagnames, preceded by 'prefix.', to the list
        IF DIMS( DIMS(0)+1 ) EQ 8 THEN BEGIN
          TAGS = TAG_NAMES( NAME )
          NELS = N_ELEMENTS( TAGS )
          PREF=PREFIX?PREFIX+'.':''
          IF ( ARG_PRESENT(LIST) ) THEN $
             NEWLIST = [ LIST, PREF + TAGS ] $
          ELSE $
             NEWLIST = PREF + TAGS

;  and process each tag
          FOR I = 0, NELS-1 DO BEGIN
             RES = EXECUTE( $
             'NEWLIST=ADDNAMES(NAME.'+TAGS[I]+',"'+PREF+TAGS[I]+'",NEWLIST)' )
          ENDFOR

;  If it's not a structure, return the original list or ''
        ENDIF ELSE BEGIN
           IF N_ELEMENTS(LIST) NE 0 THEN $
              NEWLIST = LIST $
           ELSE $
              RETURN, ''

        ENDELSE


;  Return the constructed array of strings
        RETURN, NEWLIST

        END

