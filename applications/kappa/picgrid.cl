procedure picgrid( xpic, ypic, fill )
begin
 string txpic=""
 string gxpic
 string typic=""
 string gypic
 string tfill=""
 string gfill
 gxpic = str(xpic)
 if ( gxpic != "" ) txpic = "xpic=" + gxpic + " "
 ;
 gypic = str(ypic)
 if ( gypic != "" ) typic = "ypic=" + gypic + " "
 ;
 gfill = str(fill)
 if ( gfill != "" ) tfill = "fill=" + gfill + " "
 ;
 print ("picdef ",txpic,typic,tfill,"mode_=array ","fraction=1.0 ","prefix=\"\" ") | cl
end
