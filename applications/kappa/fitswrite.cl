procedure fitswrite( ndf, keyword, value, comment )
begin
 string tndf=""
 string gndf
 string tkeyword=""
 string gkeyword
 string tvalue=""
 string gvalue
 string tcomment=""
 string gcomment
 gndf = str(ndf)
 if ( gndf != "" ) tndf = "ndf=" + gndf + " "
 ;
 gkeyword = str(keyword)
 if ( gkeyword != "" ) tkeyword = "keyword=\"" + gkeyword + "\" "
 ;
 gvalue = str(value)
 if ( gvalue != "" ) tvalue = "value=\"" + gvalue + "\" "
 ;
 gcomment = str(comment)
 if ( gcomment != "" ) tcomment = "comment=\"" + gcomment + "\" "
 ;
 print ("fitsmod ",tndf,tkeyword,tvalue,tcomment,"edit=write ","mode_=interface ","position=INDEF ") | cl
end
