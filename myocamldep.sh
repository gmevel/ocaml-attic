#!/bin/bash

# 2023 WARNING: This is an old (2015) attempt at compiling OCaml projects where
# source files and build files are put in separate subdirectories. To be used in
# combination with a Makefile. It is hackish and probably broken, don’t use it.
# Use dune.

################################################################################

# taken from ~/Documents/ENS/L3/Stage/tlace/myocamldep

# TODO: Handle -impl, -intf, -ml-synonym, -mli-synonym, and -slash.

if [ $# -eq 0 ] ; then
	echo "Usage for $0: same as \`ocamlfind ocamldep\`, with the added options:"
	echo -e "\t-srcdir <DIR>    the directory where source files must reside"
	echo -e "\t-builddir <DIR>  the directory where compiled files must reside"
	echo -e "\t-cmo             generate dependencies only for .cmo files, not for .cmx"
	echo -e "\t-cmx             generate dependencies only for .cmx files, not for .cmo"
	echo -e "\t-opaque          disable cross-module optimization: make .cmx files rely"
	echo -e "\t                 solely on the interface  (.cmi)  of referenced modules,"
	echo -e "\t                 not on their implementation (.cmx);  this is to be used"
	echo -e "\t                 in conjunction with the -opaque flag of ocamlopt"
	echo -e "\t-cmi-from-cmx    if the  .mli  source file is absent, generate the same"
	echo -e "\t                 dependencies as for the corresponding .cmx file rather"
	echo -e "\t                 than for the .cmo file"
	echo "Files given on the command line are either .ml or .mli source files."
	echo "  * Mod.ml produces dependency rules for Mod.cmo and Mod.cmx."
	echo "  * Mod.mli  procudes dependency rules for  Mod.cmi,  whether  Mod.mli  actually"
	echo "    exists or not.  If it does not exist,  the dependencies  are the same as for"
	echo "    Mod.cmo, unless the -cmi-from-cmx flag is given."
    exit
fi

ocamldep='ocamlfind ocamldep -one-line'

output=ox    # set of flags: o to output rules for .cmo, x to output rules for .cmx
opaque=false
cmi_from_cmx=false
srcdir='.'
builddir='.'
options=()

until [ $# -eq 0 ] ; do
	case $1 in
	  '-cmo' )
		output=o
		;;
	  '-cmx' )
		output=x
		;;
	  '-opaque' )
		opaque=true
		;;
	  '-cmi-from-cmx' )
		cmi_from_cmx=true
		;;
	  '-srcdir' )
		srcdir="$2"
		shift
		;;
	  '-builddir' )
		builddir="$2"
		shift
		;;
	  '-syntax' |\
	  '-package' |\
	  '-predicates' |\
	  '-ppopt' |\
	  '-ppxopt' |\
	  '-passopt' |\
	  '-I' |\
	  '-impl' |\
	  '-intf' |\
	  '-ml-synonym' |\
	  '-mli-synonym' |\
	  '-open' |\
	  '-pp' |\
	  '-ppx' )
		options+=("$1" "$2")
		shift
		;;
	  '-'* )
		options+=("$1")
		;;
	  *'.ml' | *'.mli' )
		file="$1"
		if [[ "$file" == *.mli ]] && [ ! -e "$srcdir/$file" ] ; then
			absent_mli=true
			file_given="${file/%.mli/.ml}"
		else
			absent_mli=false
			file_given="$file"
		fi
		{
			#read -a cmo_rule
			#read -a cmx_rule
			read -a rule
		} < <(cd "$srcdir" && $ocamldep "${options[@]}" "$file_given")
		[ "$builddir" != "." ] && rule=("${rule[@]/#/$builddir/}")
		target="${rule[0]}"
		deps=("${rule[@]:2}")
		if [[ "$file" == *.ml ]] && [ -e "$srcdir/$file"i ] ; then
			own_cmi_dep="${deps[${#deps[@]}-1]}"
			unset deps[${#deps[@]}-1]
		else
			own_cmi_dep=
		fi
		deps_cmi=("${deps[@]/%.cm[ox]/.cmi}")
		# rule for .cmo
		cmo="$target"
		cmo_rule=("$cmo" : "${deps_cmi[@]}")
		[ -n "$own_cmi_dep" ] && cmo_rule+=("$own_cmi_dep")
		# rule for .cmx
		deps_cmx=("${deps_cmi[@]/%.cmi/.cmx}")
		deps_o=("${deps_cmi[@]/%.cmi/.o}")
		cmx="${cmo/%.cmo/.cmx}"
		cmx_rule=("$cmx" : "${deps_cmi[@]}")
		$opaque               || cmx_rule+=("${deps_cmx[@]}" "${deps_o[@]}")
		[ -n "$own_cmi_dep" ] && cmx_rule+=("$own_cmi_dep")
		# rule for .cmi
		if $absent_mli && $cmi_from_cmx ; then
			cmi_rule=("${cmx_rule[@]}")
		else
			cmi_rule=("${cmo_rule[@]}")
		fi
		cmi_rule[0]="${cmi_rule[0]/%.cm[ox]/.cmi}"
		if [[ "$file" == *.ml ]] ; then
			[[ $output =~ o ]] && echo "${cmo_rule[@]}"
			[[ $output =~ x ]] && echo "${cmx_rule[@]}"
		else
			echo "${cmi_rule[@]}"
		fi
	esac
	shift
done
