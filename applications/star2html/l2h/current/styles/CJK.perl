
########################################################################
# $Id$
# CJK.perl
#   Jens Lippmann <lippmann@cdc.informatik.tu-darmstadt.de>,
#   Boy Yang <yangboy@math.ntu.edu.tw>,
#   Werner Lemberg <xlwy01@uxp1.hrz.uni-dortmund.de>
#
# Extension to LaTeX2HTML V 96.2 to supply support for the
# "CJK" LaTeX package.
#
########################################################################
# Change Log:
# ===========
#  jcl = Jens Lippmann <http://www-jb.cs.uni-sb.de/~www/people/lippmann>
#
# $Log$
# Revision 1.1  2004/02/20 13:13:28  nxg
# Initial import
#
# Revision 1.3  1998/02/19 22:24:26  latex2html
# th-darmstadt -> tu-darmstadt
#
# Revision 1.2  1996/12/17 17:11:41  JCL
# typo
#
# Revision 1.1  1996/12/17 17:07:32  JCL
# - introduced to CVS repository
# - adjusted technical notes according to Werner's proposal
# - added support for CJK* environment
#
# jcl  16-DEC-96 - Created
#
########################################################################
# Notes:
# To may view the results only with a browser configured for the
# specific language.
# To configure the browser, use eg. the "document encoding" menu
# of NetScape.
#
# Technical Notes:
# We use the pre_process hook to change any text coming in to
# LaTeX2HTML such that we convert from the outer representation
# of double byte characters to an inner, LaTeX2HTML specific
# representation.
# The two outer representations recognized are described as follows:
# o standard CJK encodings (GB, KS, Big5, SJIS, etc.)
#   Each symbol is formed by two characters, the first in the range
#   [\201-\237\241-\376] (octal) or 0x81-0x9F, 0xA1-0xFE (hexadecimal),
#   the second in the range
#   [\100-\176\200-\377] (octal) or 0x40-0x7E, 0x80-0xFF (hexadecimal).
# o CJK internal encoding (to conveniently use CJK processed files)
#   Each symbol is a sequence with a leading character in the range
#   [\201-\237\241-\376] or 0x81-0x9F, 0xA1-0xFE,
#   a sequence of digits forming the decimal representation of the
#   second character from standard encoded form (eg. "65", "128"),
#   and a trailing 0xFF.
# The internal LaTeX2HTML representation is the same as the CJK
# encoded form.
# Additionally, we handle TeX's normalized representation of special
# characters (eg. ^^e4), which is helpful when LaTeX2HTML processes
# the .aux file.
#
# The post_process hook will convert the LaTeX2HTML internal coding
# into standard Big5/SJIS encoding, which then remains in the
# HTML text.
#
# The revert_to_raw_tex hook will convert the internal encoding
# back to standard encoding to help with image creation.
#
########################################################################


package main;

sub pre_pre_process {
    # Handle TeX's normalized special character encoding.
    # This *might* be done by LaTeX2HTML, too, but yet we don't
    # rely on it.
    s/\^\^([^0-9a-f])/chr((64+ord($1))&127)/ge;
    s/\^\^([0-9a-f][0-9a-f])/chr(hex($1))/ge;
    # Care for standard CJK encoding -> l2h internal form.
    s/([\201-\237\241-\376])([\100-\176\200-\376])/"$1" . ord($2) . "\377"/ge;
}

sub post_post_process {
    # l2h internal form -> standard CJK encoding
    s/([\201-\237\241-\376])(\d+)\377/"$1" . chr($2)/ge;
}

sub revert_to_raw_tex_hook {
    # l2h internal form -> standard CJK encoding
    s/([\201-\237\241-\376])(\d+)\377/"$1" . chr($2)/ge;
}


sub do_cmd_CJKchar {
    local($_) = @_;
    &get_next_optional_argument;
    s/$next_pair_rx/chr($2)/eo;
    s/$next_pair_rx/$2\377/o;
    $_;
}

# Handle CJK environments.
# The usage of \CJKspace, \CJKnospace is not implemented yet.
#
sub do_env_CJK {
    local($_) = @_;
    # skip font encoding
    &get_next_optional_argument;
    # skip CJK encoding
    s/$next_pair_rx//o;
    # skip CJK font family
    s/$next_pair_rx//o;
    $_;
}

# Handle CJK* environments.
# The usage of \CJKspace, \CJKnospace is not implemented yet.
# We won't catch single newlines following CJK symbols, because
# this would require to suppress the newlines in the HTML output,
# leading to overly long lines.
#
sub do_env_CJKstar {
    local($_) = &do_env_CJK;
    #CJK symbols eat ensuing white space
    s/([\201-\237\241-\376]\d+\377)[ \t]+/\1/g;
    $_;
}

# most of the commands here need some action which is not implemented yet.

&ignore_commands(<<_IGNORED_CMDS_);
CJKCJKchar
CJKboldshift
CJKcaption # {}
CJKenc # {}
CJKencfamily # [] # {} # {}
CJKfamily # {}
CJKfontenc # {} # {}
CJKglue
CJKhangul
CJKhangulchar
CJKhanja
CJKkern
CJKlatinchar
CJKnospace
CJKspace
CJKtilde
CJKtolerance
CJKuppercase
Unicode # {} # {}
nbs
standardtilde
_IGNORED_CMDS_


# we need \AtBeginDocument and \AtEndDocument

&ignore_commands(<<_IGNORED_CMDS_);
AtBeginDocument # {}
AtEndDocument # {}
_IGNORED_CMDS_

# This must be the last line.
1;
