; Common IDL functions

; Read the contents of an array from a file.  
;
; The file is in `Fortran format': it holds the contents of the array
; as a list of numbers, with the left-most array index changing
; fastest.  For simplicity, the function does no error checking, and
; assumes that the array is the right length.
pro read_file, a, fn
        openr, iu, fn, /get_lun    ; open the file to read, allocate unit
        readf, iu, a
        free_lun, iu               ; close the file and release the unit.
end


; Read columns of numbers into a 2-d array.
;
; Given a file with  n  columns and  m  rows, this takes an  n x m
; array and fills it with the contents of the file.
; 
; It ignores lines beginning # 
pro read_columns, a, fn
        dims = size (a)            ; get array dimensions
        line = ''
        print, "cols=",dims[1]," rows=",dims[2]
        openr, iu, fn, /get_lun
        linevals = fltarr(dims[1])
        i = 0
        while not eof(iu) do begin
                readf, iu, line
                if (strmid(line,0,1) ne '#') then begin
                        reads,line,linevals
                        if (n_elements(linevals) ne dims[1]) then begin
                                print,'line ',i,": expected',dims[1], $
                                        ' got',n_elements(linevals)
                                return
                        endif
                        if (i ge dims[2]) then message,'too many lines in file'
                        a(*,i) = linevals
                        i = i+1
                endif
        end
        if (i ne dims[2]) then message,'too few lines in file'
        close, iu
end
