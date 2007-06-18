; IDL script for parsing bolotable.txt

readcol, "bolotable.txt", name, xs, ys, xoff, yoff, gain, sens, $
         format="A,D,D,D,D,D,D"

nbolo = n_elements(name)

thisline = ''

for i=0, nbolo-1 do begin

  if (i mod 4) eq 0 then begin
    print,thisline
    thisline = ''
  endif
  thisline = thisline + strcompress(yoff[i]/206265d, /remove_all)+", "
endfor
print,thisline

end
