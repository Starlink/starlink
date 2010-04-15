### File: html3.1.pl
### Language definitions for HTML 3.1 (Math)
### Written by Marcus E. Hennecke <marcush@leland.stanford.edu>
### Version 0.3,  April 3, 1997

## Copyright (C) 1995 by Marcus E. Hennecke
## This program is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2 of the License, or
## (at your option) any later version.

## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
## You should have received a copy of the GNU General Public License
## along with this program; if not, write to the Free Software
## Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

if ($HTML_OPTIONS =~ /math/) {
    do { $DOCTYPE = ''; $STRICT_HTML = 0 } unless ($NO_SIMPLE_MATH);
}

#########################
## Support HTML 3.0 math

#### Mathematical Formulas

package main;

sub do_env_math {
    local($_) = @_;
    local($math_mode, $failed, $labels, $comment,$img_params) = ("inline",'','');
    $failed = (/$htmlimage_rx/); # force an image
    local($attribs, $border);
    if (s/$htmlborder_rx//o) { $attribs = $2; $border = (($4)? "$4" : 1) }
    local($saved) = $_;
    ($labels, $comment, $_) = &process_math_env($math_mode,$_);
    if ($failed) {
	$_ = join ('', $comment, $labels
            , &process_undefined_environment("tex2html_wrap", $id, $saved));
    } elsif ($NO_SIMPLE_MATH) {
        $_ = join('', $comment, $labels, " ", $_ );
    } else {
        $_ = join('', $comment, $labels, "<MATH CLASS=\"INLINE\">\n$_\n</MATH>");
    }
    if (($border||($attributes))&&($HTML_VERSION > 2.1 )) {
	&make_table( $border, $attribs, '', '', '', $_ )
    } else { $_ }
}

sub do_env_tex2html_wrap {
    local($_) = @_;
    local($math_mode, $failed, $labels, $comment,$img_params) = ("inline",'','');
    $failed = (/$htmlimage_rx/); # force an image
    local($attribs, $border);
    if (s/$htmlborder_rx//o) { $attribs = $2; $border = (($4)? "$4" : 1) }
    local($saved) = $_;
    s/^\\\(//;    s/\\\)$//;
    ($labels, $comment, $_) = &process_math_env($math_mode,$_);
    if ($failed) {
	$_ = join ('', $comment, $labels
             , &process_undefined_environment("tex2html_wrap", $id, $saved));
    } elsif ($NO_SIMPLE_MATH) {
        $_ = $comment . $labels ." ".$_;
    } else {
        $_ = $comment . $labels . "<MATH CLASS=\"INLINE\">\n$_\n</MATH>";
    }
    if (($border||($attribs))&&($HTML_VERSION > 2.1 )) {
	&make_table( $border, $attribs, '', '', '', $_ )
    } else { $_ }
}

sub do_env_tex2html_wrap_inline {
    local($_) = @_;
    local($math_mode, $failed, $labels, $comment) = ("inline",'','');
    $failed = (/$htmlimage_rx/); # force an image
    local($attribs, $border);
    if (s/$htmlborder_rx//o) { $attribs = $2; $border = (($4)? "$4" : 1) }
    local($saved) = $_;
    s/(^\s*(\$|\\\()\s*|\s*(\$|\\\))\s*$)//g; # remove the \$ signs or \(..\)

    $_ = &translate_environments($_) unless (($NO_SIMPLE_MATH)||($failed));
    ($labels, $comment, $_) = &process_math_env($math_mode,$_);
    if ($failed) {
	$_ = join ('', $labels, $comment
            , &process_undefined_environment("tex2html_wrap_inline", $id, $saved));
    } elsif ($NO_SIMPLE_MATH) {
        $_ = join('', $labels, $comment, $_);
    } else {
        $_ = join('', $labels, $comment, "<MATH CLASS=\"INLINE\">\n$_\n</MATH>");
    }
    if (($border||($attribs))&&($HTML_VERSION > 2.1 )) {
	&make_table( $border, $attribs, '', '', '', $_ )
    } else { $_ }
}

sub do_env_equation {
    local($_) = @_;
    local($math_mode, $failed, $labels, $comment) = ("equation",'','');
    $failed = (/$htmlimage_rx/); # force an image
    local($attribs, $border);
    if (s/$htmlborder_rx//o) { $attribs = $2; $border = (($4)? "$4" : 1) }
    local($saved) = $_;
    local($sbig,$ebig);
    ($sbig,$ebig) = ('<BIG>','</BIG>')
	if (($DISP_SCALE_FACTOR)&&($DISP_SCALE_FACTOR >= 1.2 ));
    local($math_start,$math_end)= ($sbig,$ebig);

    local($eqno) = '&nbsp;&nbsp;'; # spacer, when no numbering
    $* = 1;
    do { # include the equation number, using a <TABLE>
        $global{'eqn_number'}++;
        $eqno = &do_cmd_theequation();
    } unless ((s/(\\nonumber|\\notag)//g)||(/\\tag/));
    if (s/\\tag(\*)?//){
        # AmS-TEX line-number tags.
        local($nobrack,$before) = ($1,$`);
	$_ = $';
        s/next_pair_pr_rx//o;
	if ($nobrack) { $eqno = $2 }
        else { $eqno = "($2)" }
	$_ = $before;
    }
    $* = 0;

    if ($EQN_TAGS =~ /L/) {
        # equation number on left
        ($math_start,$math_end) =
            ("\n<TABLE WIDTH=\"100%\" ALIGN=\"CENTER\""
	      . (($border)? " BORDER=\"$border\"" : '')
	      . (($attribs)? " $attribs" : '')
	      . ">\n<TR VALIGN=\"MIDDLE\"><TD ALIGN=\"CENTER\">"
	      .$eqno."</TD>\n<TD ALIGN=\"CENTER\" NOWRAP>$sbig"
	    , "$ebig</TD>\n</TR></TABLE>");
	    $border = $attribs='';
    } else {
        # equation number on right
        ($math_start,$math_end) =
            ("\n<TABLE WIDTH=\"100%\" ALIGN=\"CENTER\""
	      . (($border)? " BORDER=\"$border\"" : '')
	      . (($attribs)? " $attribs" : '')
	      . ">\n<TR VALIGN=\"MIDDLE\"><TD></TD>"
              . "<TD ALIGN=\"CENTER\" NOWRAP>$sbig"
	    , "$ebig</TD>\n<TD ALIGN=\"CENTER\">".$eqno."</TD></TR>\n</TABLE>");
	    $border = $attribs='';
    }

    ($labels, $comment, $_) = &process_math_env($math_mode,$_);
    if ($failed) {
	$_ = join ('', $comment, $labels, $math_start
	    , &process_undefined_environment('displaymath', $id, $saved)
	    , $math_end );
    } elsif ($NO_SIMPLE_MATH) {
        $_ = join('', "<P ALIGN=\"CENTER\">", $labels
	       , $comment, $math_start, "\n$_\n", $math_end, "</P>" );
    } else {
        $_ = join('', "<P ALIGN=\"CENTER\">"
               , $labels, $comment, $math_start
	       , "\n<MATH CLASS=\"EQUATION\">\n"
	       , $_ , "\n</MATH>", $math_end );
    }
    if (($border||($attribs))&&($HTML_VERSION > 2.1 )) {
	join('',"<BR>\n<DIV ALIGN=\"CENTER\">\n"
            , &make_table( $border, $attribs, '', '', '', $_ )
	    , "\n<BR CLEAR=\"ALL\">");
    } else { $_ }
}

sub do_env_displaymath {
    local($_) = @_;
    local($math_mode, $failed, $labels, $comment) = ("display",'','');
    $failed = (/$htmlimage_rx/); # force an image
    local($attribs, $border);
    if (s/$htmlborder_rx//o) { $attribs = $2; $border = (($4)? "$4" : 1) }
    local($saved) = $_;
    local($sbig,$ebig);
    ($sbig,$ebig) = ('<BIG>','</BIG>')
	if (($DISP_SCALE_FACTOR)&&($DISP_SCALE_FACTOR >= 1.2 ));
    ($labels, $comment, $_) = &process_math_env($math_mode,$_);
    if ($failed) {
	$_ = join('', $comment, $labels
            , &process_undefined_environment("displaymath", $id, $saved));
    } elsif ($NO_SIMPLE_MATH) {
	$_ = "$comment\n<P ALIGN=\"CENTER\">$labels\n$sbig$_$ebig\n</P>"
    } else {
        $_ = join('', $comment, "<P ALIGN=\"CENTER\">", $labels
            , "$sbig\n<MATH CLASS=\"DISPLAYMATH\">\n",$_,"\n</MATH>\n$ebig</P>");
    }
    if (($border||($attribs))&&($HTML_VERSION > 2.1 )) {
	join('',"<BR>\n<DIV ALIGN=\"CENTER\">\n"
            , &make_table( $border, $attribs, '', '', '', $_ )
	    , "\n<BR CLEAR=\"ALL\">");
    } else { $_ }
}

### Some Common Structures

## Declare math mode and take care of sub- and superscripts. Also make
## sure to treat curly braces right.
sub make_math {
    local($math_mode,$math_style,$math_face,$_) = @_;
    # Do spacing
    s/\\,/;SPMthinsp;/g;
    s/\\!/;SPMnegsp;/g;
    s/\\:/;SPMsp;/g;
    s/\\;/;SPMthicksp;/g;
    s/\\limits/\&limits;/g; # preserve the \limits commands

    # Find all _ and ^, but not \_ and \^
    s/\\_/\\underscore/g;
    s/\\^/\\circflex/g;

#RRM:  The following code is just plain wrong !!!
#    local(@terms) = split(/([_^])/);
#    local($math,$i,$subsup,$level);
#    # Do the sub- and superscripts
#    $math = $terms[$[];
#    for ( $i = $[+1; $i <= $#terms; $i+=2 ) {
#	$subsup = ( $terms[$i] eq "_" ? "SUB" : "SUP" );
#	$_ = $terms[$i+1];
#	if ( s/$next_pair_rx// ) {
#	    $math .= "<$subsup>$2</$subsup>$_";
#	} else {
#	    s/^\s*(\w|\\[a-zA-Z]+)//;
#	    $math .= "<$subsup>$1</$subsup>$_";
#	};
#    };
#    $_ = $math;
#RRM: This works much better (from   &simple_math_env ).
    if ($NO_SIMPLE_MATH) {
	s/\&ldots;/.../g;
        $_ = &translate_math_commands($math_mode,$math_style,$math_face,0,$_);
	# remove redundant tags
	s/<I>\s*<\/I>//go;
	s/<\/I>(\s*)<I>/$1/go;
    } else {
        s/\^$any_next_pair_rx/<SUP>$2<\/SUP>/go;
	s/_$any_next_pair_rx/<SUB>$2<\/SUB>/go;
	s/\^(\\[a-zA-Z]+|.)/<SUP>$1<\/SUP>/g;
	s/_(\\[a-zA-Z]+|.)/<SUB>$1<\/SUB>/g;
    }

    s/\\underscore/\\_/g;
    s/\\circflex/\\^/g;
    s/&limits;//g; # not implemented, except via an image

    # Translate all commands inside the math environment
    $_ = &translate_commands($_) unless ($NO_SIMPLE_MATH);


    if ($NO_SIMPLE_MATH) {
	s/&lbrace;/{/g; s/&rbrace;/}/g;
#	s/\s*&times;\s*/ <TT>x<\/TT> /g;
	s/\s*&times;\s*/ &#215; /g;
	s/\s*&div;\s*/ &#247; /g;
	s/\s*&circ?;\s*/<TT>o<\/TT>/g;
	s/\s*&ast;\s*/ <TT>\*<\/TT> /g;
	s/\s*&l?dots;\s*/.../g;
	s/\s*&mid;\s*/ | /g;
	s/\s*&parallel;\s*/ || /g;
        s/;SPM(thin)?sp;/ /g; s/&thinsp;/ /g; s/&sp;/ /g;
        s/;SPMthicksp;/&nbsp; /g; s/&thicksp;/&nbsp; /g; s/&ensp;/&nbsp; /g;
#        &replace_math_constructions($math_mode);
    } else {
        # Inside <MATH>, { and } have special meaning. Thus, need &lcub;
	# and &rcub;
#    s/{/&lcub;/g; s/}/&rcub;/g; # Where are these defined ?
        s/{/&lbrace;/g;
        s/}/&rbrace;/g;

        # Remove the safety markers for math-entities
        s/(\&\w+)#\w+;/$1;/g;

	# Substitute <BOX> and </BOX> with { and } to improve readability
        # on browsers that do not support math.
        s/<BOX>/$level++;'{'/ge;
	s/<\/BOX>/$level--;'}'/ge;
	# Make sure braces are matching.
        $_ .= '}' if ( $level > 0 );
	$_ = '{'.$_ if ( $level < 0 );
#	s/<\/?SUB>/_/g; s/<\/?SUP>/^/g;
    }

    # contract spaces
    s/\s+/ /g;

    # remove bogus entities
    s/;SPMnegsp;//g;
    s/\&limits;/\\limits/g;

    # remove white space at the extremities
    do{ $*=1; s/(^\s+|\s+$)//; $*=0; } unless ($NO_SIMPLE_MATH);

    $_;
}



## Fractions
sub do_math_cmd_frac {
    local($_) = @_;
    local($numer) = &get_next_token;
    local($denom) = &get_next_token;
    if ($NO_SIMPLE_MATH) {
        local($after) = $_;
	local($fracstyle) = "";
	$fracstyle = "\\textstyle" if (
	    ($mode =~ /display|equation|eqnarray/)
            && !($numer =~ /[^\d\s]/)&&!($numer =~ /[^\d\s]/));
        ( &process_math_in_latex( $mode, $math_style, $slevel
	      , "{$fracstyle\\frac{$numer}{$denom}}") , $after )
    } else { "<BOX>$numer<OVER>$denom</BOX>$_" }
}

## Roots
sub do_math_cmd_sqrt {
    local($_) = @_;
    local($n) = &get_next_optional_argument;
    local($surd) = &get_next_token;
    if ($NO_SIMPLE_MATH) { local($after) = $_;
        ( &process_math_in_latex( $mode, $math_style, $slevel
	    , "\\sqrt".( $n ? "[$n]" : '') . "{$surd}") , $after )
    } else { $n ? "<ROOT>$n<OF>$surd</ROOT>$_" : "<SQRT>$surd</SQRT>$_"; }
}

sub do_math_cmd_thin_sp {
    '&thinsp;';
}
sub do_math_cmd_norm_sp {
    '&sp;';
}
sub do_math_cmd_thick_sp {
    '&ensp;';
}

%mathnomacros = (
                 'omicron', 'o',
		 'Alpha', 'A', 'Beta', 'B', 'Epsilon', 'E', 'Zeta', 'Z',
		 'Eta', 'H', 'Iota', 'J', 'Kappa', 'K', 'Mu', 'M', 'Nu', 'N',
		 'Omicron', 'O', 'Rho', 'R', 'Tau', 'T', 'Chi', 'X'
	     );

do {
    local($key, $val);
    foreach $key (keys %mathnomacros) {
	$val = $mathnomacros{$key};
	$LaTeXmacros .= "\\providecommand{\\$key}{\\textrm{$val}}\n";
    }
};

%mathentities = (
		 # Ellipsis  %ISOpub
		 'ldots', 'hellip', 'cdots', 'cdots', 'vdots', 'vellip',
		 'ddots', 'ddots', 'dotfill', 'dotfill',
### Mathematical Symbols
		 # Greek letters  %ISOgrk3;  and  %HTMLsymbol;
		 'alpha', 'alpha', 'beta', 'beta', 'gamma', 'gamma',
		 'delta', 'delta', 'epsilon', 'epsi', 'varepsilon', 'epsiv',
		 'zeta', 'zeta', 'eta', 'eta', 'theta', 'thetas',
		 'vartheta', 'thetav', 'iota', 'iota', 'kappa', 'kappa',
		 'lambda', 'lambda', 'mu', 'mu',
		 'nu', 'nu', 'xi', 'xi', 'pi', 'pi', 'varpi', 'piv',
		 'rho', 'rho', 'varrho', 'rhov', 'sigma', 'sigma',
		 'varsigma', 'sigmav', 'tau', 'tau', 'upsilon', 'upsi',
		 'phi', 'phis', 'varphi', 'phiv', 'chi', 'chi',
		 'psi', 'psi', 'omega', 'omega',
		 'Gamma', 'Gamma', 'Delta', 'Delta', 'Theta', 'Theta',
		 'Lambda', 'Lambda', 'Xi', 'Xi', 'Pi', 'Pi',
		 'Sigma', 'Sigma', 'Upsilon', 'Upsi', 'Phi', 'Phi',
		 'Psi', 'Psi', 'Omega', 'Omega',
		 'Alpha', 'Alpha', 'Beta', 'Beta', 'Epsilon', 'Epsilon',
		 'Zeta', 'Zeta', 'Eta', 'Eta', 'Iota', 'Iota',
		 'Kappa', 'Kappa', 'Mu', 'Mu', 'Nu', 'Nu',
		 'Omicron', 'Omicron', 'Rho', 'Rho', 'Tau', 'Tau', 'Chi', 'Chi',
		 'straightepsilon', 'epsis', 'omicron', 'omicron',

		 # Binary operators   %ISOnum  %ISOamsb
		 'pm', 'plusmn', 'mp', 'mnplus', 'times', 'times',
		 'div', 'divide', 'ast', 'ast', 'star', 'sstarf',
		 'circ', 'cir', 'bullet', 'bull', 'cdot', 'sdot',
		 'cap', 'cap', 'cup', 'cup', 'uplus', 'uplus',
		 'sqcap', 'sqcap', 'sqcup', 'sqcup',
		 'vee', 'or', 'wedge', 'and', 'setminus', 'setmn',
		 'wr', 'wreath', 'diamond', 'diam',
		 'bigtriangleup', 'xutri', 'bigtriangledown', 'xdtri',
		 'triangleleft', 'ltri', 'triangleright', 'rtri',
		 'lhd', '', 'rhd', '', 'unlhd', '', 'unrhd', '',
		 'oplus', 'oplus', 'ominus', 'ominus', 'otimes', 'otimes',
		 'oslash', 'osol', 'odot', 'odot', 'bigcirc', 'xcirc',
		 'dagger', 'dagger', 'ddagger', 'Dagger', 'amalg', 'amalg',

		 # Relations  %ISOamsr  %ISOtech
		 'leq', 'le', 'prec', 'pr', 'preceq', 'pre', 'll', 'Lt',
		 'subset', 'sub', 'subseteq', 'sube', 'sqsubset', 'sqsub',
		 'sqsubseteq', 'sqsube', 'in', 'isin', 'vdash', 'vdash',
		 'geq', 'ge', 'ge', 'ge', 'succ', 'sc', 'succeq', 'sce', 'gg', 'Gt',
		 'supset', 'sup', 'supseteq', 'supe', 'sqsupset', 'sqsup',
		 'sqsupseteq', 'sqsupe', 'ni', 'ni', 'dashv', 'dashv', 'owns', 'ni',
		 'equiv', 'equiv', 'sim', 'sim', 'simeq', 'sime', 'smallamalg', 'samalg',
		 'asymp', 'asymp', 'approx', 'ap', 'cong', 'cong',
		 'neq', 'ne', 'ne', 'ne', 'doteq', 'esdot', 'propto', 'prop',
		 'models', 'models', 'perp', 'perp', 'mid', 'mid',
		 'parallel', 'par', 'bowtie', 'bowtie', 'Join', '',
		 'smile', 'smile', 'frown', 'frown',
		 'vee', 'or', 'land', 'and', 'lor', 'or',

		 # Arrows and pointers  %ISOamsa  %ISOnum
		 'leftarrow', 'larr', 'rightarrow', 'rarr',
		 'uparrow', 'uarr', 'downarrow', 'darr',
		 'Leftarrow', 'lArr', 'Rightarrow', 'rArr',
		 'Uparrow', 'uArr', 'Downarrow', 'dArr',
		 'longleftarrow', 'larr', 'longrightarrow', 'rarr',
		 'Longleftarrow', 'xlArr', 'Longrightarrow', 'xrArr',
		 'leftrightarrow', 'harr', 'Leftrightarrow', 'hArr',
		 'longleftrightarrow', 'xharr', 'Longleftrightarrow', 'xhArr',
		 'updownarrow', 'varr', 'Updownarrow', 'vArr',
		 'mapsto', 'map', 'longmapsto', 'map',
		 'hookleftarrow', 'larrhk', 'hookrightarrow', 'rarrhk',
		 'nearrow', 'nearr', 'searrow', 'drarr',
		 'swarrow', 'dlarr', 'nwarrow', 'nwarr',
		 'leftharpoonup', 'lharu', 'leftharpoondown', 'lhard',
		 'rightharpoonup', 'rharu', 'rightharpoondown', 'rhard',
		 'rightleftharpoons', 'rlhar2', 'leadsto', '',
		 'gets', 'larr', 'to', 'rarr', 'iff', 'iff',

		 # Various other symbols   %ISOpub  %ISOamso  %ISOnum  %ISOtech
		 'aleph', 'aleph', 'hbar', 'planck',
		 'imath', 'inodot', 'jmath', 'jnodot', 'ell', 'ell',
		 'wp', 'weierp', 'Re', 'real', 'Im', 'image',
		 'beth', 'beth', 'gimel', 'gimel', 'daleth', 'daleth',
		 'emptyset', 'empty', 'nabla', 'nabla', 'surd', 'radic',
		 'top', 'top', 'bot', 'bottom', 'angle', 'ang',
		 'forall', 'forall', 'exists', 'exist', 'neg', 'not',
		 'flat', 'flat', 'natural', 'natur', 'sharp', 'sharp',
		 'partial', 'part', 'infty', 'infin', 'sphericalangle', 'angsph',
		 'varprime', 'vprime', 'sbs', 'sbsol', 'yen', 'yen',
		 'Box', '', 'Diamond', '', 'triangle', 'utri',
		 'clubsuit', 'clubs', 'diamondsuit', 'diams',
		 'heartsuit', 'hearts', 'spadesuit', 'spades',
		 'qquad', 'quad', 'checkmark', 'check', 'maltese', 'malt',
		 'backslash', 'bsol', 'circledR', 'reg', 'centerdot', 'middot',
		 'prime', 'prime', 'square', 'square',

		 # Integral type entities   %ISOamsb
		 'sum', 'sum', 'prod', 'prod', 'coprod', 'coprod',
		 'int', 'int', 'oint', 'conint',

		 # Delimiters  %ISOamsc   %ISOtech
		 'lfloor', 'lfloor', 'rfloor', 'rfloor',
		 'lceil', 'lceil', 'rceil', 'rceil',
		 'langle', 'lang', 'rangle', 'rang',
		 'lbrace', 'lcub', 'rbrace', 'rcub',
		 'lbrack', 'lsqb', 'rbrack', 'rsqb',
		 'Vert', 'Verbar',

		 # AMS package
		 # Greek letters
		 'digamma', 'gammad', 'varkappa', 'kappav',

		 # Delimiters %ISOamsc
		 'ulcorner', 'ulcorn', 'urcorner', 'urcorn',
		 'llcorner', 'dlcorn', 'lrcorner', 'drcorn',
		 'leftparengtr', 'lpargt', 'rightparengtr', 'rpargt',

		 # Arrows  %ISOamsa
		 'dashrightarrow', '', 'dashleftarrow', '',
		 'leftleftarrows', 'larr2', 'leftrightarrows', 'lrarr2',
		 'Lleftarrow', 'lArr', 'twoheadleftarrow', 'Larr',
		 'leftarrowtail', 'larrtl', 'looparrowleft', 'larrlp',
		 'leftrightharpoons', 'lrhar2', 'curvearrowleft', 'cularr',
		 'circlearrowleft', 'olarr', 'Lsh', 'lsh',
		 'upuparrows', 'uarr2', 'upharpoonleft', 'uharl',
		 'downharpoonleft', 'dharl', 'multimap', 'mumap',
		 'leftrightsquigarrow', 'harrw',
		 'rightrightarrows', 'rarr2', 'rightleftarrows', 'rlarr2',
		 'Rrightarrow', 'rArr', 'twoheadrightarrow', 'Rarr',
		 'rightarrowtail', 'rarrtl', 'looparrowright', 'rarrlp',
		 'rightleftharpoons', 'rlhar2', 'curvearrowright', 'curarr',
		 'circlearrowright', 'orarr', 'Rsh', 'rsh',
		 'downdownarrows', 'darr2', 'upharpoonright', 'uharr',
		 'downharpoonright', 'dharr','rightsquigarrow', 'rarrw',

		 # Negated arrows  %ISOamsa
		 'nleftarrow', 'nlarr', 'nrightarrow', 'nrarr',
		 'nLeftarrow', 'nlArr', 'nRightarrow', 'nrArr',
		 'nleftrightarrow', 'nharr', 'nLeftrightarrow', 'nhArr',

		 # Binary relations  %ISOamsr  %ISOamsb
		 'leqq', 'lE', 'leqslant', 'les', 'eqslantless', 'els',
		 'lesssim', 'lsim', 'lessapprox', 'lap', 'approxeq', 'ape',
		 'lessdot', 'ldot', 'lll', 'Ll', 'Ll', 'Ll', 'llless', 'Ll',
		 'lessgtr', 'lg', 'lesseqgtr', 'leg', 'lesseqqgtr', 'lEg',
		 'backcong', 'bcong',
		 'barwedge', 'barwed', 'doublebarwedge', 'Barwed',
		 'doublecap', 'Cap', 'doublecup', 'Cup',
		 'curlyvee', 'cuvee', 'curlywedge', 'cuwed',
		 'divideontimes', 'divonx', 'intercal', 'intcal',
		 'doteqdot', 'eDot', 'risingdotseq', 'erDot', 'Doteq', 'eDot',
		 'leftthreetimes', 'lthree', 'rightthreetimes', 'rthree',
		 'ltimes', 'ltimes', 'rtimes', 'rtimes',
		 'boxminus', 'minusb', 'boxplus', 'plusb', 'boxtimes', 'timesb',
		 'circledast', 'oast', 'circledcirc', 'ocirc', 'circleddash', 'odash',
		 'dotplus', 'plusdo', 'dotsquare', 'sdotb', 'smallsetminus', 'ssetmn',
		 'fallingdotseq', 'efDot', 'backsim', 'bsim',
		 'backsimeq', 'bsime', 'subseteqq', 'subE', 'Subset', 'Sub',
		 'curlypreceq', 'cupre', 'curlyeqprec', 'cuepr',
		 'precsim', 'prsim', 'precapprox', 'prap',
		 'vartriangleleft', 'vltri', 'trianglelefteq', 'ltrie',
		 'vDash', 'vDash', 'Vvdash', 'Vvdash', 'Vdash', 'Vdash',
		 'smallsmile', 'ssmile', 'smallfrown', 'sfrown',
		 'bumpeq', 'bumpe', 'Bumpeq', 'bump',
		 'coloneq', 'colone', 'eqcolon', 'ecolon',
		 'geqq', 'gE', 'geqslant', 'ges', 'eqslantgtr', 'egs',
		 'gtrsim', 'gsim', 'gtrapprox', 'gap', 'gtrdot', 'gsdot',
		 'ggg', 'Gg', 'Gg', 'Gg', 'gggtr', 'Gg',
		 'gtrless', 'gl', 'gtreqless', 'gel',
		 'gtreqqless', 'gEl', 'eqcirc', 'ecir', 'circeq', 'cire',
		 'triangleeq', 'trie', 'thicksim', 'thksim',
		 'thickapprox', 'thkap', 'supseteqq', 'supE', 'Supset', 'Sup',
		 'succcurlyeq', 'sccue', 'veebar', 'veebar',
		 'curlyeqsucc', 'cuesc', 'succsim', 'scsim',
		 'succapprox', 'scap', 'vartriangleright', 'vrtri',
		 'trianglerighteq', 'rtrie',
		 'shortmid', 'smid', 'shortparallel', 'spar',
		 'between', 'twixt', 'pitchfork', 'fork', 'bigstar', 'starf',
		 'varpropto', 'vprop', 'because', 'becaus',
		 'therefore', 'there4', 'backepsilon', 'bepsi',
		 'blacksquare', 'squf', 'lozenge', 'loz', 'blacklozenge', 'lozf',
		 'blacktriangle', 'utrif', 'blacktriangledown', 'dtrif',
		 'blacktriangleleft', 'ltrif', 'blacktriangleright', 'rtrif',

		 # Negated binary relations  %ISOamsn
		 'gnapprox', 'gnap', 'gneq', 'gne', 'gneqq', 'gnE',
		 'lnapprox', 'lnap', 'lneq', 'lne', 'lneqq', 'lnE',
		 'gnsim', 'gnsim', 'gvertneqq', 'gvnE',
		 'lnsim', 'lnsim', 'lvertneqq', 'lvnE',
		 'nsim', 'nsim', 'nsimeq', 'nsime',
		 'napprox', 'nap', 'ncong', 'ncong', 'nequiv', 'nequiv',
		 'ngeq', 'nge', 'ngeqq', 'ngE', 'ngeqslant', 'nges', 'ngtr', 'ngt',
		 'nleq', 'nle', 'nleqq', 'nlE', 'nleqslant', 'nles', 'nless', 'nlt',
		 'ntriangleleft', 'nltri', 'ntrianglelefteq', 'nltrie',
		 'ntriangleright', 'nrtri', 'ntrianglerighteq', 'nrtrie',
		 'nmid', 'nmid', 'nparallel', 'npar',
		 'nprec', 'npr', 'npreceq', 'npre', 'nsucc', 'nps', 'nsucceq', 'npse',
		 'nshortmid', 'nsmid', 'nshortparallel', 'nspar',
		 'nsubset', 'nsub', 'nsubseteq', 'nsube', 'nsubseteqq', 'nsubE',
		 'nsupset', 'nsup', 'nsupseteq', 'nsupe', 'nsupseteqq', 'nsupE',
		 'nvdash', 'nvdash', 'nvDash', 'nvDash',
		 'nVdash', 'nVdash', 'nVDash', 'nVDash',
		 'precnapprox', 'prnap', 'precneqq', 'prnE', 'precnsim', 'prnsim',
		 'succnapprox', 'scnap', 'succneqq', 'scnE', 'succnsim', 'scnsim',
		 'subsetneq', 'subne', 'subsetneqq', 'subnE',
		 'supsetneq', 'supne', 'supsetneqq', 'supnE',
		 'varsubsetneq', 'vsubne', 'varsubsetneqq', 'vsubnE',
		 'varsupsetneq', 'vsupne', 'varsupsetneqq', 'vsupnE',

		 # Binary operators
		 'Cup', 'Cup', 'Cap', 'Cap',
		 # miscellaneous  %ISOamso
		 'hslash', 'planck', 'circledS', 'oS',
		 'nexists', 'nexist', 'varnothing', 'empty',
		 'measuredangle', 'angmsd',
		 'complement', 'comp', 'backprime', 'bprime',
);

## Log-like Functions
@mathfunctions = ('arccos', 'arcsin', 'arctan', 'arg', 'cos', 'cosh',
		  'cot', 'coth', 'csc', 'deg', 'dim', 'exp', 'hom',
		  'ker', 'lg', 'ln', 'log', 'sec', 'sin', 'sinh',
		  'tan', 'tanh', 'mod');
@limitfunctions = ('det', 'gcd', 'inf', 'lim', 'liminf',
		   'limsup', 'max', 'min', 'Pr', 'sup' );

foreach (@mathfunctions) {
    eval "sub do_math_cmd_$_\{\"<T CLASS=\\\"FUNCTION\\\">$_</T>\$_[\$[]\";}";
}
foreach (@limitfunctions) {
    eval "sub do_math_cmd_$_\{
    local(\$_) = \@_;
    s/^\\s*<SUB>/<SUB ALIGN=\\\"CENTER\\\">/ unless ( \$math_mode eq \"inline\" );
    \"<T CLASS=\\\"FUNCTION\\\">$_</T>\$_\";}";
}



sub do_math_cmd_pmod {
    local($_) = @_;
    local($mod) = &get_next_token;
    if ($NO_SIMPLE_MATH) { local($after) = $_;
	$mod = &process_math_toks($mode, $math_style, $face, $slevel, 0, $mod);
        join( '', "(mod $mod)", $after)
    } else {"(<T CLASS=\"FUNCTION\">mod</T> $mod)$_"}
 }

sub do_math_cmd_bmod {
    local($_) = @_;
    local($mod) = '';
    if ($NO_SIMPLE_MATH) { join( '', " mod " , $_) }
    else {"(<T CLASS=\"FUNCTION\"> mod </T>)$_"}
 }

sub do_math_cmd_circ {
    if ($NO_SIMPLE_MATH) { ("<TT>o</TT>","@_") }
    else { "o@_"}
}

### Arrays
sub do_env_array {
    local($_) = @_;
    if ($NO_SIMPLE_MATH) {
        # make an image
        $_ = &process_math_in_latex( $mode, $math_style, $slevel
            , "\\begin{array}". $_ . "\\end{array}");
        return($_);
    }
    local($align) = &get_next_optional_argument;
    if      ( $align =~ /^\s*b/ ) {
	$align = "ALIGN=\"BOTTOM\"";
    } elsif ( $align =~ /^\s*t/ ) {
	$align = "ALIGN=\"TOP\"";
    } else {
	$align = "ALIGN=\"MIDDLE\"";
    };
    s/$next_pair_rx//;
    local($colspec) = $2;
    s/\n\s*\n/\n/g;	# Remove empty lines (otherwise will have paragraphs!)
    local($i,@colspec,$char,$cols,$cell,$htmlcolspec,$frames,$rules);
    local(@rows,@cols,$border);
    local($colspan);

    ($htmlcolspec,$frames,$rules,$cols,@colspec) =
	&translate_colspec($colspec, 'ITEM');

    @rows = split(/\\\\/);
    $#rows-- if ( $rows[$#rows] =~ /^\s*$/ );
    local($return) = "<ARRAY COLS=$cols $align>\n$htmlcolspec\n";
    foreach (@rows) {
	$return .= "<ROW>";
	@cols = split(/$html_specials{'&'}/o);
	for ( $i = 0; $i <= $#colspec; $i++ ) {
	    $colspec = $colspec[$i];
	    $colspan = 0;
	    $cell = &make_math("", '', '', shift(@cols)); # May modify $colspan, $colspec
	    if ( $colspan ) {
		for ( $cellcount = 0; $colspan > 0; $colspan-- ) {
		    $colspec[$i++] =~ s/<ITEM/$cellcount++;"<ITEM"/ge;
		}
		$i--;
		$colspec =~ s/>$content_mark/ COLSPAN=$cellcount$&/;
	    };
	    $colspec =~ s/$content_mark/$cell/;
	    $return .= $colspec;
	};
	$return .= "</ROW>\n";
    };
    $return .= "</ARRAY>\n";
    $failed = 1 if ($NO_SIMPLE_MATH);
    $return;
}

### Delimiters

$math_delimiters_rx = "^\\s*(\\[|\\(|\\\\{|\\\\lfloor|\\\\lceil|\\\\langle|\\/|\\||\\)|\\]|\\\\}|\\\\rfloor|\\\\rceil|\\\\rangle|\\\\backslash|\\\\\\||\\\\uparrow|\\\\downarrow|\\\\updownarrow|\\\\Uparrow|\\\\Downarrow|\\\\Updownarrow|\\.)";

sub do_math_cmd_left {
    local($_) = @_;
    s/$math_delimiters_rx//;
    $failed = 1 if ($NO_SIMPLE_MATH);
    "<BOX>" . ( $1 && $1 ne "." ? "$1<LEFT>" : "" ) . $_ .
	( /\\right/ ? "" : "</BOX>" );
}

sub do_math_cmd_right {
    local($_) = @_;
    s/$math_delimiters_rx//;
    if ( !($ref_before =~ /<LEFT>/) ) {
	$ref_before = "<BOX>" . $ref_before;
    };
    $failed = 1 if ($NO_SIMPLE_MATH);
    ( $1 eq "." ? "" : "<RIGHT>$1" ) . "</BOX>$_";
}

### Multiline formulas

sub do_env_eqnarray {
    local($_) = @_;
    local($math_mode, $failed, $labels, $comment, $doimage) = ("equation",'','');
#    $doimage = (/$htmlimage_rx/); # force an image
    local($attribs, $border);
    if (s/$htmlborder_rx//o) { $attribs = $2; $border = (($4)? "$4" : 1) }
    local($saved) = $_;
    local($sbig,$ebig);
    ($sbig,$ebig) = ('<BIG>','</BIG>')
	if (($DISP_SCALE_FACTOR)&&($DISP_SCALE_FACTOR >= 1.2 ));
    $failed = 1 if ($NO_SIMPLE_MATH); # simplifies the next call
    ($labels, $comment, $_) = &process_math_env($math_mode,$_);
    if (($failed)&&!($NO_SIMPLE_MATH)) {
        $_ = join ('', $labels, $comment
	    , &process_undefined_environment(
	        "eqnarray".(($no_eqn_numbers) ? "star" : '')
	        , $id, $saved));
	$_ = join('','<P'
            , (($HTML_VERSION >2.0)? " ALIGN=\"CENTER\"" : '')
            ,'>', $labels, $comment, $_, '<BR'
            , (($HTML_VERSION >2.0)? " CLEAR=\"ALL\"" : '')
	    , '>',"\n<P>");

    } elsif ($NO_SIMPLE_MATH) {
        $failed = 0;
	s/$htmlimage_rx/$doimage = $&;''/eo ; # force an image
        local($sarray, $srow, $slcell, $elcell, $srcell, $ercell, $erow, $earray);
	($sarray, $elcell, $srcell, $erow, $earray, $sempty) = (
	    "\n<TABLE CELLPADDING=\"0\" ALIGN=\"CENTER\""
	    , "</TD>\n<TD ALIGN=\"CENTER\" NOWRAP>"
	    , "</TD>\n<TD ALIGN=\"LEFT\" NOWRAP>"
	    , "</TD></TR>", "\n</TABLE>", "</TD>\n<TD>" );
	$sarray .= (($no_eqn_numbers) ? ">" :  " WIDTH=\"100%\">" );
	if ($EQN_TAGS =~ /L/) { # number on left
            ($srow, $slcell, $ercell) = (
		"\n<TR VALIGN=\"MIDDLE\"><TD ALIGN=\"LEFT\">"
	        , "</TD>\n<TD NOWRAP ALIGN=", '');
	} else { # equation number on right
            ($srow, $slcell, $ercell) = ("\n<TR VALIGN=\"MIDDLE\">"
	    , "<TD NOWRAP ALIGN="
	    , "</TD>\n<TD ALIGN=\"RIGHT\">" );
	}
	local(@rows,@cols,$eqno,$return,$thismath);
	@rows = split(/\\\\/);
	$#rows-- if ( $rows[$#rows] =~ /^\s*$/ );
	$return = join(''
            , (($border||($attribs))? '': "<BR>")
            , (($doimage)? '' : "\n<DIV ALIGN=\"CENTER\">")
            , (($labels)? $labels : "\n") , $comment, $sarray);
	foreach (@rows) { # displaymath
            $eqno = '&nbsp;&nbsp;';
            do {
	        $global{'eqn_number'}++ ;
		$eqno = &simplify(&do_cmd_theequation());
	        } unless ((s/\\nonumber//)||($no_eqn_numbers));
            $return .= $srow;
	    $return .= $eqno if ($EQN_TAGS =~ /L/);
	    $return .= $slcell;
	    if (s/\\lefteqn//) {
	        $return .= "\"LEFT\" COLSPAN=\"3\">";
		$* =1; s/(^\s*|$html_specials{'&'}|\s*$)//g; $*=0;
		if (($doimage)||($failed)) {
		    $_ = (($_)? &process_math_in_latex(
		        "indisplay" , '', '', $doimage.$thismath ):'');
	        } else {
	            $_ = (($_) ? &make_math('display', '', '', $_) : '' );
		}
	        if ($_) {
	            $return .= join('', $sbig, $_, $ebig, $erow);
		} else { $return .= join('',"\&nbsp;", $erow); }
		next;
	    }

	    # columns to be set using math-modes
	    @cols = split(/$html_specials{'&'}/o);

	    # left column, set using \displaystyle
	    $thismath = shift(@cols);
	    $* =1; $thismath =~ s/(^\s*|\s*$)//g; $*=0;
	    if (($doimage)||($failed)) {
	        $thismath = (($thismath)? &process_math_in_latex(
		    "indisplay" , '', '', $doimage.$thismath ):'');
	    } else {
	        $thismath = (($thismath) ?
	            &make_math('displaymath', '', '', $thismath) : "" );
	    }
	    if ($thismath) {
#	        $return .= join('',"\"RIGHT\">\n$sbig",$thismath,"$ebig",$elcell);
#	    } else { $return .= join('',">\&nbsp;",$elcell) }
	        $return .= join('',"\"RIGHT\">$sbig",$thismath,"$ebig");
	    } else { $return .= ">\&nbsp;" }

	    # center column, set using \textstyle
	    $thismath = shift(@cols);
	    $* =1; $thismath =~ s/(^\s*|\s*$)//g; $*=0;
	    if (($doimage)||($failed)) {
	        $thismath = (($thismath)? &process_math_in_latex(
		    "indisplay" , 'text', '', $doimage.$thismath ):'');
	    } else {
	        $thismath = (($thismath)?
	            &make_math('display','text','',$thismath):'');
	    }
	    if ($thismath) {
#	        $return .= join('', "\n$sbig" , $thismath, $ebig, $srcell );
#	    } else { $return .= join('', "\&nbsp;\&nbsp;",$srcell) }
	        $return .= join('', $elcell, $sbig , $thismath, $ebig);
	    } else { $return .= join('', $sempty,"\&nbsp;\&nbsp;") }

	    # right column, set using \displaystyle
	    $thismath = shift(@cols);
	    $* =1; $thismath =~ s/(^\s*|\s*$)//g; $*=0;
	    if (($doimage)||($failed)) {
	        $thismath = (($thismath)? &process_math_in_latex(
		    "indisplay" , '', '', $doimage.$thismath ):'');
	    } else {
	        $id = $global{'max_id'}++;
	        $thismath = (($thismath)? &make_math('display','','',$thismath):'');
	    }
	    if ($thismath) {
#	        $return .= join('', $sbig , $thismath, $ebig, $ercell );
#	    } else { $return .= join('',"\&nbsp;",$ercell) }
	        $return .= join('', $srcell, $sbig, $thismath, $ebig, $ercell);
	    } else { $return .= join('', $sempty, "\&nbsp;", $ercell) }

	    $return .= $eqno unless ($EQN_TAGS =~ /L/);
	    $return .= $erow;
	}
        $_ = join('', $return , $earray, (($doimage)? '' : "</DIV>" ));
    } else {
        $_ = join('', $comment, "<P ALIGN=\"CENTER\">$sbig"
	    , $labels, "\n<MATH CLASS=\"EQNARRAY\">"
	    , &do_env_array("$O$max_id${C}rcl$O$max_id$C$_")
	    , "</MATH>\n$ebig</P>" )
    }
    if (($border||($attribs))&&($HTML_VERSION > 2.1 )) {
	join('',"<BR>\n<DIV ALIGN=\"CENTER\">\n"
            , &make_table( $border, $attribs, '', '', '', $_ )
	    , "\n<BR CLEAR=\"ALL\">");
    } else { $_ }
}

sub do_env_eqnarraystar {
    local($_) = @_;
    local($math_mode, $failed, $labels, $comment) = ("equation",'','');
    $failed = (/$htmlimage_rx/); # force an image
    local($attribs, $border);
    local($saved) = $_;
    local($sbig,$ebig);
    ($sbig,$ebig) = ('<BIG>','</BIG>')
	if (($DISP_SCALE_FACTOR)&&($DISP_SCALE_FACTOR >= 1.2 ));

    if (($NO_SIMPLE_MATH)||($failed)) {
        local($no_eqn_numbers) = 1;
	$_ = &do_env_eqnarray($_) unless ($failed);
	if ($failed) {
            if ($saved =~ s/$htmlborder_rx//o)
	        { $attribs = $2; $border = (($4)? "$4" : 1) }
            $_ = join('', $labels
	    , &process_undefined_environment("eqnarraystar", $id, $saved));
	}
    } else {
        if (s/$htmlborder_rx//o) { $attribs = $2; $border = (($4)? "$4" : 1) }
        $saved = $_;
        ($labels, $comment, $_) = &process_math_env($math_mode,$_);
	if ($failed) {
            $_ = join('', $labels
	        , &process_undefined_environment("eqnarraystar", $id, $saved));
	} else {
            $_ = join('', $comment, "<P ALIGN=\"CENTER\">$sbig", $labels
	        , "\n<MATH CLASS=\"EQNARRAYSTAR\">"
	        , &do_env_array("$O$max_id${C}rcl$O$max_id$C$_")
	        , "</MATH>\n$ebig</P>" );
	}
    }
    if (($border||($attribs))&&($HTML_VERSION > 2.1 )) {
	$_ = &make_table( $border, $attribs, '', '', '', $_ )
    } else { $_ }
}

sub do_math_cmd_nonumber {
    $_[$[];
};

### Putting One Thing Above Another

## Over- and Underlining

sub do_math_cmd_overline {
    local($_) = @_;
    local($over) = &get_next_token;
    if ($NO_SIMPLE_MATH) {
        local($supsub) = &get_supsub;
        local($after) = $_;
        ( &process_math_in_latex( $mode, $math_style, $slevel
            , "\\overline{$over}$supsub") , $after)
    } else {"<ABOVE>$over</ABOVE>$_" }
}

sub do_math_cmd_underline {
    local($_) = @_;
    local($under) = &get_next_token;
    if ($NO_SIMPLE_MATH) {
        local($supsub) = &get_supsub;
        local($after) = $_;
        ( &process_math_in_latex( $mode, $math_style, $slevel
            , "\\underline{$under}$supsub") , $after)
    } else { "<BELOW>$under</BELOW>$_" }
}

sub do_math_cmd_overbrace {
    local($_) = @_;
    local($over) = &get_next_token;
    if ($NO_SIMPLE_MATH) {
        local($supsub) = &get_supsub;
        local($after) = $_;
        ( &process_math_in_latex( $mode, $math_style, $slevel
            , "\\overbrace{$over}$supsub") , $after)
    } else { "<ABOVE SYM=\"CUB\">$over</ABOVE>$_" }
}

sub do_math_cmd_underbrace {
    local($_) = @_;
    local($under) = &get_next_token;
    if ($NO_SIMPLE_MATH) {
        local($supsub) = &get_supsub;
        local($after) = $_;
        ( &process_math_in_latex( $mode, $math_style, $slevel
            , "\\underbrace{$under}$supsub") , $after)
    } else { "<BELOW SYM=\"CUB\">$under</BELOW>$_" }
}

## Accents

sub do_math_cmd_vec {
    local($_) = @_;
    local($over) = &get_next_token;
    if ($NO_SIMPLE_MATH) {
        local($supsub) = &get_supsub;
        local($after) = $_;
        ( &process_math_in_latex( $mode, $math_style, $slevel
            , "\\vec{$over}$supsub") , $after)
    } else { "<VEC>$over</VEC>$_" }
}

sub do_math_cmd_bar {
    local($_) = @_;
    local($over) = &get_next_token;
    if ($NO_SIMPLE_MATH) {
        local($supsub) = &get_supsub;
        local($after) = $_;
        ( &process_math_in_latex( $mode, $math_style, $slevel
            , "\\bar{$over}$supsub") , $after)
    } else { "<BAR>$over</BAR>$_" }
}

sub do_math_cmd_dot {
    local($_) = @_;
    local($over) = &get_next_token;
    if ($NO_SIMPLE_MATH) {
        local($supsub) = &get_supsub;
        local($after) = $_;
        ( &process_math_in_latex( $mode, $math_style, $slevel
            , "\\dot{$over}$supsub") , $after)
    } else { "<DOT>$over</DOT>$_" }
}

sub do_math_cmd_ddot {
    local($_) = @_;
    local($over) = &get_next_token;
    if ($NO_SIMPLE_MATH) {
        local($supsub) = &get_supsub;
        local($after) = $_;
        ( &process_math_in_latex( $mode, $math_style, $slevel
            , "\\ddot{$over}") , $after)
    } else { "<DDOT>$over</DDOT>$_" }
}

sub do_math_cmd_hat {
    local($_) = @_;
    local($over) = &get_next_token;
    if ($NO_SIMPLE_MATH) {
        local($supsub) = &get_supsub;
        local($after) = $_;
        ( &process_math_in_latex( $mode, $math_style, $slevel
            , "\\hat{$over}$supsub") , $after)
    } else { "<HAT>$over</HAT>$_" }
}

sub do_math_cmd_tilde {
    local($_) = @_;
    local($over) = &get_next_token;
    if ($NO_SIMPLE_MATH) {
        local($supsub) = &get_supsub;
        local($after) = $_;
        ( &process_math_in_latex( $mode, $math_style, $slevel
            , "\\tilde{$over}$supsub") , $after)
    } else { "<TILDE>$over</TILDE>$_" }
}

sub do_math_cmd_widehat {
    local($_) = @_;
    local($over) = &get_next_token;
    if ($NO_SIMPLE_MATH) {
        local($supsub) = &get_supsub;
        local($after) = $_;
        ( &process_math_in_latex( $mode, $math_style, $slevel
            , "\\widehat{$over}$supsub") , $after)
    } else { "<ABOVE SYM=\"HAT\">$over</ABOVE>$_" }
}

sub do_math_cmd_widetilde {
    local($_) = @_;
    local($over) = &get_next_token;
    if ($NO_SIMPLE_MATH) {
        local($supsub) = &get_supsub;
        local($after) = $_;
        ( &process_math_in_latex( $mode, $math_style, $slevel
            , "\\widetilde{$over}$supsub") , $after)
    } else { "<ABOVE SYM=\"TILDE\">$over</ABOVE>$_" }
}

## Stacking Symbols

sub do_math_cmd_stackrel {
    local ($_) = @_;
    local($top,$bot);
    $top = &get_next_token;
    $bot = &get_next_token;
    if ($NO_SIMPLE_MATH) {
        local($after) = $_;
        ( &process_math_in_latex( $mode, $math_style, $slevel
            , "\\;\\stackrel{$top}{$bot}\\;") , $after)
    } else { "<BOX>$2</BOX><SUP ALIGN=\"CENTER\">$top</SUP>$_" }
}

# Kill $ref_before in case we're not in math mode.
sub do_math_cmd_atop {
    local ($before) = $ref_before;
    $before =~ s/^[\s%]*//; $before =~ s/[\s%]*$//;
    $ref_before = "";
    $failed = 1 if ($NO_MATH_MARKUP);
    "<BOX>$before<ATOP>$_[$[]</BOX>";
}

sub do_math_cmd_choose {
    local ($before) = $ref_before;
    $before =~ s/^\s*//; $before =~ s/\s*$//;
    $ref_before = "";
    $failed = 1 if ($NO_MATH_MARKUP);
    "<BOX>$before<CHOOSE>$_[$[]</BOX>";
}

sub do_math_cmd_mbox {
    local($_) = @_;
    local($cmd,$text,$after)=('mbox','','');
    $text = &missing_braces
        unless ((s/$next_pair_pr_rx[\s%]*/$text = $2;''/eo)
		||(s/$next_pair_rx[\s%]*/$text = $2;''/eo));
    if ($NO_SIMPLE_MATH) {
        $after = $_;
	if (!($text)||($text =~ /^\s*$/)) { ("\&nbsp; ", $after) }
	elsif (!($text =~ /(tex2html_wrap_inline|\$$OP(\d+)$CP$OP\2$CP\$|\$$O(\d+)$C$O\2$C\$)/)) {
            $text = &translate_commands($text);
	    ("$text", $after)
        } else {
            ( &process_math_in_latex( $mode, $math_style, $slevel
                , "\\mbox{$text}") , $after)
	}
    } else { "<TEXT>$text</TEXT>$_" }
}

sub do_math_cmd_display {
    $_[$[];
}

sub do_math_cmd_text {
    $_[$[];
}

sub do_math_cmd_script {
    $_[$[];
}

sub do_math_cmd_scriptscript {
    $_[$[];
}

# This is supposed to put the font back into math italics.
# Since there is no HTML equivalent for reverting
# to math italics we keep track of the open font tags in
# the current context and close them.
# *** POTENTIAL ERROR ****#
# This will produce incorrect results in the exceptional
# case where \mit is followed by another context
# containing font tags of the type we are trying to close
# e.g. {a \bf b \mit c {\bf d} e} will produce
#       a <b> b </b> c   <b> d   e</b>
# i.e. it should move closing tags from the end
sub do_math_cmd_mit {
    local($_, @open_font_tags) = @_;
    local($next);
    for $next (@open_font_tags) {
	$next = ($declarations{$next});
	s/<\/$next>//;
	$_ = join('',"<\/$next>",$_);
    }
    $_;
}


# This is a better way of coping with simple-math,
# using recognition of SUP/SUB tags.

sub translate_math_commands {
    local($mode,$style,$face,$slevel,$_) = @_;
    &replace_strange_accents;
    for (;;) {			# For each opening bracket ...
	last unless (/$begin_cmd_rx/o);
        local($before, $contents, $br_id, $after, $pattern);
        ($before, $br_id, $after, $pattern) = ($`, $1, $', $&);
	local($end_cmd_rx) = &make_end_cmd_rx($br_id);
#print "\nCMD: $end_cmd_rx";
        if ($after =~ /$end_cmd_rx/) { # ... find the the matching closing one
	    $NESTING_LEVEL++;
            ($contents, $after) = ($`, $');
	    $_ = join("", $before,"$OP$br_id$CP", $contents,"$OP$br_id$CP", $after);
	    $NESTING_LEVEL--;
	}
	else {
	    $pattern = &escape_rx_chars($pattern);
	    s/$pattern//;
	    print STDERR "\nCannot find matching bracket for $br_id";
	}
    }
    &process_math_toks($mode,$style,$face,$slevel,1,$_);
}


sub make_math_comment{
    local($_) = @_;
    local($scomm,$ecomm)=("\$","\$");
    return() if (/$image_mark/);
    do {
        $scomm = "\\begin{$env}\n";
	$ecomm = "\n\\end{$env}";
    } unless ($env =~/tex2html/);
    $_ = &revert_to_raw_tex;
    $* = 1; s/^\s+//; s/\s+$//; $* = 0;
    $_ = $scomm . $_ . $ecomm;
    return() if (length($_) < 12);
    $global{'verbatim_counter'}++;
    $verbatim{$global{'verbatim_counter'}} = $_;
    &write_mydb('verbatim_counter', $global{'verbatim_counter'}, $_ );
    join('', $verbatim_mark, '#math' , $global{'verbatim_counter'},'#')
}

sub process_math_env {
    local($mode,$_) = @_;
    local($labels, $comment);
    ($_,$labels) = &extract_labels($_); # extract labels
    $comment = &make_math_comment($_);
    local($max_id) = ++$global{'max_id'};
    if ($failed) { return($labels, $comment, $_) };
    if ($BOLD_MATH) {
	($labels, $comment, join('',"<B>", &make_math($mode,'','',$_), "</B>"))
    } else {
	($labels, $comment, &make_math($mode,'','',$_))
    }
}

sub process_math_toks {
    local($mode,$style,$face,$slevel,$math_outer,$_) = @_;
    local($pre,$keep,$this,$cmd,$tmp);
    local($lastclosespace, $joinspace) = (1,1);
    $face = "I" unless (($face)||($slevel));  # default text is italiced
    $face =~ s/math//;
    print "\$";
    print STDERR "\nMATH_IN:$_" if ($VERBOSITY > 4);
    while (
	/[\s%]*(\\([a-zA-Z]+|.)|$image_mark#\w+#.|(<#\d*#>)|(<[^>]*>)(#math\d+#)?|\^|\_|&[a-zA-Z]+;|;SPM[a-z]+;|&)/
        ) {
	$lastclosespace = 1; $joinspace=1;
	#  $keep  holds the results from processed tokens
	#  $pre  should be all simple letters and other characters
	$pre = $`;
	#  $_  holds the tokens yet to come
	$_ = $';
	#  $this  holds the current token
	$this = $1;
	if ($4) {
            # tags already processed from an earlier cycle
	    #    includes math-comment markers
	    $this = $4.$5;
#	    $this = $5;
	    $lastclosespace = 0; $joinspace=0;
	} elsif ($3) {
            # an opening brace
	    $_ = $3.$_;
	    s/$next_pair_pr_rx/$this=$2;''/eo;
	    $this =~ s/^\s*//; $this =~ s/\s*$//;
	    # if there is \atop.. or \over.. then make an image
	    # Better would be to create a table after establishing
	    # the minimum number of tokens involved...
	    #    ...but that's getting pretty complicated.
	    if ($this =~ /\\(atop|over([^a-zA-Z]|withdelims))/) {
	        print ".";
		print STDERR "$1" if ($VERBOSITY > 2);
	        $this = "{".$this."}";
		$this .= &get_supsub;
		$this = &process_math_in_latex($mode, $style, $slevel,$this);
		$lastclosespace = 0;
	    } else {
	        local($extra) = &get_supsub;
		$extra =~ s/{|}/<#1#>/g; # revert the brace-pairs
		$this .= $extra; undef $extra;
		# otherwise it is just fatuous (?) braces...
		$this = &process_math_toks($mode,$style,$face,$slevel,0,$this);
		# ...perhaps deliberate, to suppress space.
		$lastclosespace = 0; $joinspace=0;
	    }
	} elsif (($2) && ($2 =~/(acute|breve|check|grave)$/)) {
            # accented characters, not implemented separately
	    print ".";
	    print STDERR "$1" if ($VERBOSITY > 2);
	    $this .= join('', "{", &get_next_token, "}");
	    $this .= &get_supsub;
	    $this = &process_math_in_latex($mode, $style, $slevel,$this);
	} elsif (($2) && ($2 =~/begin/)) {
            # embedded environment; e.g.  tex2html_wrap
	    if (s/^\s*<([<#])(\d+)([#>])>(tex2html_(deferred|wrap(\w*))|array)<\1\2\3>//o) {
	        print ".";
	        print STDERR "$4" if ($VERBOSITY > 2);
	        # make image, including any sup/sub-scripts
	        local($id,$env) = ($2,$4);
		$this = "\\begin".$&;
	        local ($saved) = $_;
		$_ = $';
		# find the \end, including nested environments of same type.
	        $* = 1;
		local($cnt) = 1;
	        while (s/\\(begin|end)<#(\d+)#>$env<#(\2)#>//) {
		    $this .= $` . $&; $_ = $';
	            if ($1 =~ /begin/) {$cnt++} else {$cnt--};
	            last if (!$cnt);
		}
	        $* = 0;
	        if ($cnt) {
		    print "\n *** cannot find end of environment: $this ";
		    &write_warnings("\n *** failed to find \\end for: $this ");
	            $this = ''; $_ = $saved;
		} else {
		    $this .= &get_supsub;
		    $this = &process_math_in_latex($mode,$style,$slevel,$this);
		};
		undef $cnt;
	    } else {
	        $_ = &translate_environments( $_ );
	    }
	} elsif (($2) && ($2 =~ /\w+/)) {
	    # macro or math-entity
	    $cmd = $&;
	    print ".";
	    print STDERR "$cmd" if ($VERBOSITY > 2);
	    local($mtmp, $ctmp, $wtmp) =
	        ("do_math_cmd_$cmd","do_cmd_$cmd", "wrap_cmd_$cmd");
	    if ($cmd eq 'left') {
	        #expandable delimiter: make an image
		$this = "\\$cmd";
		local($blevel, $delim ) = (1,'');
		while (($blevel)&&(/(\\(left|right))([^a-zA-Z])/)) {
		    $this .= $`.$1; $_ = $3.$';
		    if ($2 =~ /left/) { $blevel++ }
		    else { $blevel-- }
		}
	        $delim = &get_next_token;
	        $this .= $delim;
		if ($blevel) {
	            print STDERR "\n *** unclosed \\left$delim  starting:\n$this$_\n\n";
		    &write_warnings("\nunclosed \\left$delim  found");
		}
	        undef $blevel;
	        undef $delim;

		# include any super/sub scripts, to position them correctly
	        $this .= &get_supsub;
		$this = &process_math_in_latex($mode,$style,$slevel,$this);
		$lastclosespace = 0;
	    } elsif ($cmd =~ /(b|p)mod$/) {
	        if ($cmd =~ /p/) {
	            $this = &get_next_token;
		    $this = &process_math_toks($mode,$style,$face,$slevel,1,$this);
		    $this = "(mod $this)";
	        } else { $this = " mod " }
	    }
	    elsif (defined &$mtmp) {
		if ( grep(/\b$cmd\b/, @mathfunctions)
		    || grep(/\b$cmd\b/, @limitfunctions)) {
		    # it's a named function, to be set in upright text.
		    $this = &get_supsub;
		    if ($this) {
			# let LaTeX handle any super/sub-scripts...
			$this = &process_math_in_latex($mode,$style,$slevel
				      , "\\$cmd$this" );
		    } else {
			# ...else just put in the name.
			$this = "$cmd "
		    }
		} elsif ($cmd =~ /^(circ)$/) {
	            if ($_ =~ /^$/) {$this = "\&cir;"} # ...\degrees
		    else { $this = " \&cir; " }  # ... as a binary relation
		} else {
		    # do what comes naturally ...
		    ($this, $_) = &$mtmp($_); s/^\s+//;
		    # if it doesn't return a pair, then add it to $_
		    # the result should be separated off in subsequent cycles.
		    if (($this)&&(! $_)) {$_= $this; $this ='' };
		}
	    } elsif (($mathentities{$cmd})||($latexsyms{$cmd})) {
	        if ($cmd =~ /^(en|l?dots|gt|lt|times|div|mid|parallel|ast)$/) {
	            # standard entity
	            $this = &get_supsub;
		    if ($this) {
		        $this = &process_math_in_latex($mode,$style,$slevel,"\\$cmd$this" );
		    } else {
		        $this = "\&$cmd;";
		    }
	        } elsif ($mathnomacros{$cmd}) {
	            # standard entity, no-macro needed
		    $cmd = $mathnomacros{$cmd};
		    $this = &get_supsub;
		    if ($this) {
		        $this = &process_math_in_latex($mode,$style,$slevel
		            ,"\\textrm{$cmd}$this" );
		    } else {
		        $this = "$cmd";
		    }
	        } else {
	            # need an image
	            $this = &get_supsub;
		    $this = &process_math_in_latex($mode,$style,$slevel,"\\$cmd$this" );
		    # relations, known to need more space
		    $this = " $this "
		        if ($cmd =~ /$binary_ops_rx|$binary_rels_rx|$arrow_rels_rx/);
#		        if ($cmd =~ /^(in|le|leq|ge|geq)$/)
		}
	    } elsif (defined &$wtmp) {
	        if ($cmd =~ /math(bb|cal|sf|sfsl)$/) {
	            # make images of these, including any sup/sub-scripts
		    $this = &missing_braces
		        unless ((s/$next_pair_pr_rx\s*//o)&&($this = $2));
		    $this = "\{".$this."\}";
		    $this .= &get_supsub;
		    $this = &process_math_in_latex($mode,$style,$slevel,"\\$cmd$this" );
                } else {
		    $this = &missing_braces
		        unless ((s/$next_pair_pr_rx\s*//o)&&($this = "{$2}"));
#	            s/$next_pair_pr_rx\s*/$this="{$2}";''/eo;
		    $this .= &get_supsub;
		    $this = &process_math_in_latex($mode,$style,$slevel,"\\$cmd$this" );
		}
	    } elsif ($cmd =~ /big+[lrm]?$/i ) {
	        s/\s*(\\?\W)/$this=$1;''/eo;
		$this .= &get_supsub;
		$this = &process_math_in_latex($mode,$style,$slevel,"\\$cmd$this" );
		$lastclosespace = 0; $joinspace=0;
	    } elsif ($cmd =~ /big(\w+)/ ) {
	        local($tmpname) = $1;
		if (($mathentities{$tmpname})||($latexsyms{$tmpname})) {
	            # big version standard entity
	            $this = &get_supsub;
		    $this = &process_math_in_latex($mode,$style,$slevel,"\\$cmd$this" );
	        } else {
	            # need an image
	            $this = &get_supsub;
		    $this = &process_math_in_latex($mode,$style,$slevel,"\\$cmd$this" );
		    # relations, known to need more space
		    $this = " $this "
		        if ($tmpname =~ /$binary_ops_rx|$binary_rels_rx|$arrow_rels_rx/);
		}

	    } elsif ($cmd =~ /begin$/) {
	        local ($contents, $before, $br_id, $env, $after, $pattern);
	        if ( s/^$begin_env_rx//o ) {
	            local ($contents, $before, $br_id, $env, $after, $senv);
		    ($before, $br_id, $env, $after, $senv) = ($`, $1, $2, $', $&);
		} else { print "\n *** badly formed $cmd *** "; }
	        if (&find_end_env($env,*contents,*after)) {
	            local($eenv) = "\\end$O$br_id$C";
	            $this = &translate_environments($senv.$contents.$eenv);
		} else { print "\n *** badly formed environment in math ***"  }
		$_ = $after;
	    } elsif ($cmd =~ /not$/) {
	        $this = &get_next_token;
		$this = " ".&process_math_in_latex($mode,$style,$slevel,"\\$cmd$this" );
	    } elsif ($cmd =~ /(display|text|(script)+)style$/) {
	        if ($1 =~ /scriptscript/) {
	            $this = ""; # $slevel = 2; $this = "";
		    $pre .= "<SMALL><SMALL>";
		    $_ .= "</SMALL></SMALL>";
	        } elsif ($1 =~ /script/) {
	            $this = ""; # $slevel = 1; $this = "";
		    $pre .= "<SMALL>";
		    $_ .= "</SMALL>";
	        } elsif ($slevel) {
	            if ($1 =~ /display/) { $mode = "display" }# ; $slevel = 0 }
	            elsif ($1 =~ /text/) { $mode = "inline"  }# ; $slevel = 0 }
		    $this = "";  # &process_math_in_latex($mode,$style,0,$_);
		    $pre .= "<BIG>"; $_ .= "</BIG>";
	        } elsif ($1 =~ /display/) {
	            $mode = "display";  $this = ''; # $slevel = 0; $this = '';
	        } elsif ($1 =~ /text/) {
	            $mode = "inline"; $this = ''; # $slevel = 0; $this = '';
		}
	    } elsif (defined &$ctmp) {
	        if ($cmd =~ /(text|math)?(rm|tt|bf|it)$/) {
#	            $cmd = "math$2";
	            # simulate these with ordinary fonts
		    $this = &missing_braces
		        unless ((s/$next_pair_pr_rx\s*/$this = $2;''/oe)
		            ||(s/$next_pair_rx\s*/$this = $2;''/oe));
		    $this = &process_math_toks($mode,$style,$cmd,$slevel,$math_outer,$this);
		    if ($cmd =~ /tt/)    { $this =~ s/<(\/)?tt>/<$1TT>/g }
		    elsif ($cmd =~ /bf/) { $this =~ s/<(\/)?bf>/<$1B>/g }
		    elsif ($cmd =~ /it/) { $this =~ s/<(\/)?it>/<$1I>/g }
		    elsif ($cmd =~ /rm/) { $this =~ s/<(\/)?rm>//g }
		    $this =~ s/\s+$//; # remove trailing space
	        } else {
	            # do what comes naturally ...
                    ($this, $_) = &$ctmp($_);
		    if (!($_)) {$_= $this; $this ='' };
	        }
	    } else {
		# Unknown: send it to LaTeX and hope for the best.
		&write_warnings("\nUnknown math command: \\$cmd , image needed.");
		$this = &get_supsub;
		$this = &process_math_in_latex($mode,$style,$slevel,"\\$cmd$this" );
	    }
	} elsif ($this =~ /^\&(\w+)(#(\w+))?;/) {
	    # math-entity code
	    $cmd = $3;
	    print ".";
	    print STDERR "$cmd" if ($VERBOSITY > 2);
	    do {
	        $this = &get_supsub;
	        $this = &process_math_in_latex($mode,$style,$slevel,"\\$cmd$this" )
	    } if ($cmd);
	    $this = " $this "
	        if ($cmd =~ /$binary_ops_rx|$binary_rels_rx|$arrow_rels_rx/);
	} elsif ($this =~ /^\s*((\^)|\_)/) {
	    # super/sub-script
	    print "$1";
	    print STDERR "$2" if ($VERBOSITY > 2);
	    $slevel++;
	    local($BP) = (($2) ? "P" : "B"); $tmp = "<SU$BP>";
	    $this = &get_next_token;
	    $this = &process_math_toks($mode,$style,$face,$slevel,1,$this);
	    $this =~ s/^\s+//; $this =~ s/\s+$//;
	    $this = join('', $tmp, $this, "</SU$BP>");
	    $slevel--;
	    $lastclosespace = 0;
	} elsif ($this =~ /^;SPM([a-z]+)/) {
	    $this = " $this " if ($this =~ /;SPM(gt|lt|amp);/);
        } else {
	    # just append it; e.g. images, etc.
	}
	$pre =~ s/\s+//g; #remove all spaces...
	if ($pre) {
	    # put space back around numbers
#	    $pre =~ s/([\d\.,]+)($|\W)/"$1".(($2)? " $2" :'')/eg;
#	    $pre =~ s/([^\s\.,\(\[\/\w])(\d+)/$1 $2/g;
	    #...italiced alphabetics, except if sup/subscripts
	    if ($face eq "I") {
	        $pre =~ s/([a-zA-Z\']+)/<$face>$1<\/$face>/go unless ($slevel);
		# ... but not inside <...> tags
	        $pre =~ s/(<\/?)<$face>([a-zA-Z\']+)<\/$face>>/$1$2>/go;
	    } else {
	    # but all alphanumerics, with a special style
	        $pre =~ s/(([\.\,]?[\w\']+)+)/<$face>$1<\/$face>/go
	    }
	    # but don't split multipliers from multiplicands
	    $pre =~ s/(\d) </$1</g;
	    # remove spaces just created around brackets etc.
	    $pre =~ s/\s*(^|[\{\}\(\)\[\]\|\!\/]|$)\s*/$1/g;

	    # ensure space around = + - signs
	    $pre =~ s/([=\+\-\:])\s*/ $1 /g if ($math_outer);
	    # ... but some operators should not have a preceding space...
	    $pre =~ s/(\s|&nbsp;)+([\}\)\]])/$2/g;
	    # and some should not have a trailing space
	    $pre =~ s/([\{\(\[])(\s|&nbsp;)+/$1/g;
	    # some letters usually slope too far
	    $pre =~ s/([dfl]<\/$face>)([\(\)\/])\s*/$1 $2/g;
	    # ...and sometimes don't want spaces at the end
	    $pre =~ s/([\w>\!\)\]\|])\s+$/$1/;
#	    # ensure a space after last closing bracket, without sub/sub
#	    $pre =~ s/([\)\]\|])$/$1 /  if ($lastclosespace);
	    $pre =~ s/\s+/ /g;
	}
	# append processed tokens, forbidding some spaces; e.g. "> <".
	if ($pre) {
	    if (( $keep =~/>\s*$/ )&&( $pre =~ /^\s*[<\(\{\[\w]/ )) {
	        $keep =~ s/\s+$//; $pre =~ s/^\s+//;
	    }
	    if (( $keep =~/\w\s*$/ )&&( $pre =~ /^\s*[<\(\{[]/ )) {
	        $keep =~ s/\s+$//; $pre =~ s/^\s+//;
	    }
#	    if (($pre =~/>\s*$/)&&($this =~ /^\s*</)) {
#	        $pre =~ s/\s+$//; $this =~ s/^\s+//;
#	    }
	}
#	elsif (($keep =~/>\s*$/)&&($this =~ /^\s*([<\([\w])/)) {
#	    $keep =~ s/\s+$//; $this =~ s/^\s+//;
#	}

	print STDERR "\nMATH:$math_outer:$keep:$pre:$this:" if ($VERBOSITY > 4);
	$keep .= $pre . $this;
    }

    # the leftovers should be all simple characters also.
    s/\s*//g;
    if ($_) {
#	s/([\d\.,]+)($|\W)/"$1".(($2)? " $2" :'')/eg;
#	s/([^\s\.,\(\[\/\w])(\d+)/$1 $2/g;
	if ($face eq "I") {
            s/([a-zA-Z\']+)/<I>$1<\/I>/go unless ($slevel);
	    # ...but not inside <...> tags
	    s/(<\/?)<I>([a-zA-Z\']+)<\/I>>/$1$2>/go;
	} elsif ($face) {
            s/(([\.\,]?[\w\']+)+)/<$face>$1<\/$face>/go
	}
	s/<I>(\d+)<\/I>/$1/go unless ($slevel);
	s/(\d) </$1</g;
	s/\s*(^|[\{\}\(\)\[\]\|\!\/]|$)\s*/$1/g;
	s/([=\+\-\:])\s*/ $1 /g if ($math_outer);
	s/([\(\[\{])(\s|&nbsp;)+/$1/g;
	s/(\s|&nbsp;)+([\)\]\}])/$2/g;
	s/([\!\)\]\|])\s*([\(\[\|])/$1$2/g;
	s/([dfl]<\/$face>)([\(\)\/])\s*/$1 $2/g;
	# ensure a space after some final characters, but not all
        # suppress this by enclosing the whole expression in {}s
	s/([\)\]\|\.,!>\w])$/$1 /;
	$_ .= " " if ($math_outer);
    } elsif ($math_outer) {
        # final space unless ending with </SUP>
        $keep .=" " unless (($keep =~/SUP>$/)||(!$joinspace));
    }
    # don't allow this space: "> <"
    if (($keep =~/>\s*$/)&&($_ =~ /^\s*</)) {
	$keep =~ s/\s+$//; $_=~s/^\s+//;
    }
    print STDERR "\nMATH_OUT:$math_outer:$keep$_:" if ($VERBOSITY > 4);
    $keep .= $_;
#    # italiced superscripts can be too close to some letters... (what a hack!)
#    $keep =~ s/([a-zA-Z]<\/I><SUP>)\s*/$1 /g unless ($face =~ /it/);
#    $keep =~ s/([a-zA-Z\d]<\/it><SUP>)\s*/$1 /g if ($face =~ /it/);
#    # ...but nested superscripts are OK
#    $keep =~ s/([a-zA-Z]<\/(I|it)><SUP> <\2>[^<]+<\/\2><SUP>)( |\s)+/$1/g;
    $keep =~ s/\s*(&nbsp;)+\s*/&nbsp;/g; $keep =~ s/\s\s+/ /g;
    $keep =~ s/^(\s*[\+\-]) ([\w\d])/$1$2/o; # unary plus/minus, not binary
    $keep
}

# the next token is either {...} or \<name> or an entity.
sub get_next_token {
    local($this,$cmd)=('',"^ or \_ ");
    s/$next_token_rx/$this = $&;''/eo
        unless (s/$next_pair_pr_rx/$this = $2;''/eo);
    if ($this =~ /^(&|;)$/) { s/^([a-zA-Z]+;(#\w+;)?)/$this.=$1;''/eo; }
    elsif (! $this) { s/^\s*([&;][a-zA-Z]+;)/$this =$1;''/eo; }
    $this;
}

# This extracts sup/sub-scripts that follow immediately.
# It alters $_ in the caller.
sub get_supsub {
    local($supsub,$supb,$which) = ('','','');
    $supsub = "\\limits" if (s/^[\s%]*&limits\;//);
    while (s/^[\s%]*(\^|_)/$supb=$1;''/eo ) {
        $which .= $1;
	$supsub .= join('', $supb, "\{" , &get_next_token, "\}");
    }
    # include dummy sup/sub-scripts to enhance the vertical spacing
    # when not a nested sup/subscript
    if ($which eq "\^" ) { $supsub .= "_{}" unless ($slevel)}
    elsif ($which eq "_" ) { $supsub .= "^{}" unless ($slevel)}
    $supsub;
}


# These regular expressions help decide the type of a math-entity,
# so that extra white space may be inserted, as desirable or necessary.

$binary_ops_rx = "(pm|mp|times|plus|minus|div|ast|star|circ|dot|triangle\\w|cap|cup|vee|wedge|bullet|diamond\$|wr\$|oslash|amalg|dagger|lhd|rhd)";

$binary_rels_rx = "(eq|prec|succ|\^ll\$|\^gg\$|subset|supset|\^in\$|\^ni\$|dash|sim|approx|cong|asymp|prop|models|perp|\^mid\$|parallel|bowtie|Join|smile|frown)";

$arrow_rels_rx = "(arrow|harpoon|mapsto|leadsto)";



&ignore_commands( <<_IGNORED_CMDS_);
allowbreak
mathord
mathbin
mathrel
mathop
mathopen
mathclose
mathpunct
mathalpha
mathrm
mathbf
mathtt
mathit
smash
_IGNORED_CMDS_

    # Commands which need to be passed, ALONG WITH THEIR ARGUMENTS, to TeX.

&process_commands_in_tex( <<_LATEX_CMDS_);
mathbb # {}
mathcal # {}
mathsf # {}
_LATEX_CMDS_


1;


