# E.S.O. - VLT project 
# "@(#) $Id: psprolog.tcl,v 1.2 1998/06/22 21:00:27 abrighto Exp $"
#
# psprolog.tcl - create the Tk prolog.ps file in ~/.skycat/ for the
# single binary version (where Tk may not be installed).
#
# Note: Tk4.2 specific!
#
# who             when       what
# --------------  ---------  ----------------------------------------
# Allan Brighton  19 Jun 98  Created


# call this at startup to create the file, if needed...

proc make_psprolog_file {} {
    global env psprolog tk_library
    
    # tell Tk to use ~/.skycat/prolog.ps for canvas postscript
    set tk_library [set dir $env(HOME)/.skycat]
    set file $dir/prolog.ps
    if {! [file isdirectory $dir]} {
	if {[catch {exec mkdir $dir} msg]} {
	    puts $msg
	    return
	}
    }
    # if the file does not exist, or has a different size, create it
    if {! [file exists $file] || [file size $file] != [string length $psprolog]+1} {
	if {[catch {set fd [open $file w]} msg]} {
	    puts $msg
	    return
	}
	puts $fd $psprolog
	close $fd
    }
}

# inline contents of the Tk4.2 prolog.ps file, minus comments
set psprolog \
{%%BeginProlog
50 dict begin

/baseline 0 def
/stipimage 0 def
/height 0 def
/justify 0 def
/lineLength 0 def
/spacing 0 def
/stipple 0 def
/strings 0 def
/xoffset 0 def
/yoffset 0 def
/tmpstip null def

systemdict /ISOLatin1Encoding known not {
    /ISOLatin1Encoding [
	/space /space /space /space /space /space /space /space
	/space /space /space /space /space /space /space /space
	/space /space /space /space /space /space /space /space
	/space /space /space /space /space /space /space /space
	/space /exclam /quotedbl /numbersign /dollar /percent /ampersand
	    /quoteright
	/parenleft /parenright /asterisk /plus /comma /minus /period /slash
	/zero /one /two /three /four /five /six /seven
	/eight /nine /colon /semicolon /less /equal /greater /question
	/at /A /B /C /D /E /F /G
	/H /I /J /K /L /M /N /O
	/P /Q /R /S /T /U /V /W
	/X /Y /Z /bracketleft /backslash /bracketright /asciicircum /underscore
	/quoteleft /a /b /c /d /e /f /g
	/h /i /j /k /l /m /n /o
	/p /q /r /s /t /u /v /w
	/x /y /z /braceleft /bar /braceright /asciitilde /space
	/space /space /space /space /space /space /space /space
	/space /space /space /space /space /space /space /space
	/dotlessi /grave /acute /circumflex /tilde /macron /breve /dotaccent
	/dieresis /space /ring /cedilla /space /hungarumlaut /ogonek /caron
	/space /exclamdown /cent /sterling /currency /yen /brokenbar /section
	/dieresis /copyright /ordfeminine /guillemotleft /logicalnot /hyphen
	    /registered /macron
	/degree /plusminus /twosuperior /threesuperior /acute /mu /paragraph
	    /periodcentered
	/cedillar /onesuperior /ordmasculine /guillemotright /onequarter
	    /onehalf /threequarters /questiondown
	/Agrave /Aacute /Acircumflex /Atilde /Adieresis /Aring /AE /Ccedilla
	/Egrave /Eacute /Ecircumflex /Edieresis /Igrave /Iacute /Icircumflex
	    /Idieresis
	/Eth /Ntilde /Ograve /Oacute /Ocircumflex /Otilde /Odieresis /multiply
	/Oslash /Ugrave /Uacute /Ucircumflex /Udieresis /Yacute /Thorn
	    /germandbls
	/agrave /aacute /acircumflex /atilde /adieresis /aring /ae /ccedilla
	/egrave /eacute /ecircumflex /edieresis /igrave /iacute /icircumflex
	    /idieresis
	/eth /ntilde /ograve /oacute /ocircumflex /otilde /odieresis /divide
	/oslash /ugrave /uacute /ucircumflex /udieresis /yacute /thorn
	    /ydieresis
    ] def
} if

/ISOEncode {
    dup length dict begin
	{1 index /FID ne {def} {pop pop} ifelse} forall
	/Encoding ISOLatin1Encoding def
	currentdict
    end

    /Temporary exch definefont
} bind def

/StrokeClip {
    {strokepath} stopped {
	(This Postscript printer gets limitcheck overflows when) =
	(stippling dashed lines;  lines will be printed solid instead.) =
	[] 0 setdash strokepath} if
    clip
} bind def

/EvenPixels {
    dup 0 matrix currentmatrix dtransform
    dup mul exch dup mul add sqrt
    dup round dup 1 lt {pop 1} if
    exch div mul
} bind def

/StippleFill {
    /tmpstip 1 index def

    1 EvenPixels dup scale

    pathbbox
    4 2 roll
    5 index div dup 0 lt {1 sub} if cvi 5 index mul 4 1 roll
    6 index div dup 0 lt {1 sub} if cvi 6 index mul 3 2 roll

    6 index exch {
	2 index 5 index 3 index {
	    gsave
	    1 index exch translate
	    5 index 5 index true matrix tmpstip imagemask
	    grestore
	} for
	pop
    } for
    pop pop pop pop pop
} bind def

/AdjustColor {
    CL 2 lt {
	currentgray
	CL 0 eq {
	    .5 lt {0} {1} ifelse
	} if
	setgray
    } if
} bind def

/DrawText {
    /stipple exch def
    /justify exch def
    /yoffset exch def
    /xoffset exch def
    /spacing exch def
    /strings exch def

    /lineLength 0 def
    strings {
	stringwidth pop
	dup lineLength gt {/lineLength exch def} {pop} ifelse
	newpath
    } forall

    0 0 moveto (TXygqPZ) false charpath
    pathbbox dup /baseline exch def
    exch pop exch sub /height exch def pop
    newpath

    translate
    lineLength xoffset mul
    strings length 1 sub spacing mul height add yoffset mul translate

    justify lineLength mul baseline neg translate

    strings {
	dup stringwidth pop
	justify neg mul 0 moveto
	stipple {
	    gsave
	    /char (X) def
	    {
		char 0 3 -1 roll put
		currentpoint
		gsave
		char true charpath clip StippleText
		grestore
		char stringwidth translate
		moveto
	    } forall
	    grestore
	} {show} ifelse
	0 spacing neg translate
    } forall
} bind def
%%EndProlog
}

# do it at startup
make_psprolog_file
