procedure picxy( ubound, lbound )
begin
 string tubound=""
 string gubound
 string tlbound=""
 string glbound
 gubound = str(ubound)
 if ( gubound != "" ) tubound = "ubound=" + gubound + " "
 ;
 glbound = str(lbound)
 if ( glbound != "" ) tlbound = "lbound=" + glbound + " "
 ;
 print ("picdef ",tubound,tlbound,"mode_=xy ","fraction=1.0 ") | cl
end
