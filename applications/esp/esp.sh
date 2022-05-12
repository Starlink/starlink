#!/bin/sh -
#+
#  Name:
#     esp.sh
#
#  Purpose:
#     Start the ESP system from Unix shell.  DRAFT ONLY - DO NOT USE.
#
#  Type of Module:
#     Bourne shell script.
#
#  Invocation:
#     esp
#
#  Description:
#     This procedure defining the links needed to execute
#     each application from the current directory.
#
#  Notes:
#     The installation target is set outside of this script.
#     A test is made to see if the environment variable INSTALL
#     has been set.
#
#  Authors:
#     GJP: G.J.Privett (STARLINK)
#     BLY: M.J.Bly (Starlink, RAL)
#     NG:  Norman Gray (Starlink, Glasgow)
#     {enter_new_authors_here}
#
#  History:
#     29-SEP-1993 (GJP):
#       Original Version.
#     22-AUG-1994 (GJP):
#       Modified to add monolith.
#     6-OCT-1994 (BLY):
#       Remodeled to Starlink requirements.
#     21-NOV-1994 (BLY):
#       Added help system to startup.
#     24-FEB-1997 (GJP):
#       Added GAUFIT to startup.
#     17-FEB-1998 (NG):
#       Use ADAM_USER env.var. (as documented)
#     10-JUN-1998 (NG):
#       Converted to .sh script
#     {enter_changes_here}
#
#-
#

#  Prepare to run ADAM applications if this has not been done already.
#  ===================================================================
#
#  Here look to see if there is an ADAM_USER directory.  If there is not
#  check whether or not there is an adam file that is not a directory.
#  If there is, issue a warning and exit.  Otherwise create the required
#  directory.
#
if [ -z "$ADAM_USER" ]; then
   adamdir=$HOME/adam
else
   adamdir=$ADAM_USER
fi

if [ -d $adamdir ]; then
   echo -n
else
   if [ -f $adamdir ]; then
      echo "You have a file called $adamdir"
      echo "Please rename this, or define the environment variable ADAM_USER"
      echo "to point to a directory for ADAM files."
      exit
   else
      mkdir $adamdir
    fi
fi
adamdir=

#
#  Define aliases for the applications.
#  There should be a plain alias, and a package specific alias
#  so that applications that have conflicting command names are
#  still available.

corr () { ${ESP_DIR}/corr $* ;}
esp_corr () { ${ESP_DIR}/esp_corr $* ;}

ellfou () { ${ESP_DIR}/ellfou $* ;}
esp_ellfou () { ${ESP_DIR}/esp_ellfou $* ;}

ellpro () { ${ESP_DIR}/ellpro $* ;}
esp_ellpro () { ${ESP_DIR}/esp_ellpro $* ;}

esphelp () { ${ESP_DIR}/esphelp $* ;}
esp_esphelp () { ${ESP_DIR}/esp_esphelp $* ;}

fastmed () { ${ESP_DIR}/fastmed $* ;}
esp_fastmed () { ${ESP_DIR}/esp_fastmed $* ;}

gaufit () { ${ESP_DIR}/gaufit $* ;}
esp_gaufit () { ${ESP_DIR}/esp_gaufit $* ;}

graphs () { ${ESP_DIR}/graphs $* ;}
esp_graphs () { ${ESP_DIR}/esp_graphs $* ;}

histpeak () { ${ESP_DIR}/histpeak $* ;}
esp_histpeak () { ${ESP_DIR}/esp_histpeak $* ;}

hsub () { ${ESP_DIR}/hsub $* ;}
esp_hsub () { ${ESP_DIR}/esp_hsub $* ;}

loback () { ${ESP_DIR}/loback $* ;}
esp_loback () { ${ESP_DIR}/esp_loback $* ;}

mask () { ${ESP_DIR}/mask $* ;}
esp_mask () { ${ESP_DIR}/esp_mask $* ;}

mixup () { ${ESP_DIR}/mixup $* ;}
esp_mixup () { ${ESP_DIR}/esp_mixup $* ;}

sector () { ${ESP_DIR}/sector $* ;}
esp_sector () { ${ESP_DIR}/esp_sector $* ;}

selfc () { ${ESP_DIR}/selfc $* ;}
esp_selfc () { ${ESP_DIR}/esp_selfc $* ;}

selfcw () { ${ESP_DIR}/selfcw $* ;}
esp_selfcw () { ${ESP_DIR}/esp_selfcw $* ;}

skew () { ${ESP_DIR}/skew $* ;}
esp_skew () { ${ESP_DIR}/esp_skew $* ;}

topped () { ${ESP_DIR}/topped $* ;}
esp_topped () { ${ESP_DIR}/esp_topped $* ;}

#
#  Announce the availability of the ESP commands.
#

echo ""
echo "   ESP commands are now available -- (Version 0.11-4)"
echo ""
echo "   Type  esphelp  for help on ESP commands"
echo "   Type  showme sun180  to see the hypertext document."
echo ""

#
# end
#
