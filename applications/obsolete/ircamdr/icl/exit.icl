hidden proc exit
  ndfformats = VARIABLE(startup,ndfformats)
  if ndfformats <> " "
     setenv NDF_FORMATS_IN (ndfformats)
  endif
  asklog (sure) "EXIT IRCAMDR - are you sure (Yes or No) ? "
  if sure = 1
    #exit
  end if
end proc
