#!/bin/bash
################################################################
# Program Name: IBM Netezza Analytics
#
# (C) Copyright IBM Corporation 2013, 2014
# All rights reserved.
#
# The source code for this program is not published or otherwise 
# divested of its trade secrets, irrespective of what has been 
# deposited with the U.S. Copyright Office.
################################################################
#

SCRIPT_VERSION=1.0.8
#

# Offer automatical download of missing source code (DL=1)
# If automatic download is switched off (DL=0), all necessary sources must exist in the source directory
DL=1

# With DL_BASH_PATCHES=1 (and DL=1) also all available patches for bash are downloaded into a patch directory.
# Technically, bash patches are not needed, but we do not want to use a bash version that has the shellshock security hole.
# If at least one bash patch files already exists in the patch directory, no patches are downloaded,
# i.e. the desired patch files must be provided manually in this case!
DL_BASH_PATCHES=0

# With BASH_DELETE=1, the bash executable is deleted after R compilation. 
# You may want to use this option if you do not download bash and its patches, but provide bash manually, and 
# do not want to leave a bash executable with the shellshock security hole on the system.
BASH_DELETE=0

BEGINTIME=$(date +%s)

# Import NZ environment variables (NZ_EXPORT_DIR, ...)
eval $(nzenv)

### Functions ##################################################################

usage() {
    echo ""
    echo "Usage: $0 [-i] [options] | -u | -v | -h"
    echo ""
    echo "This script compiles and installs R, or uninstalls R."
	echo ""
	echo "Options:"
	echo "  -i                 Install R (default)"
	echo "  -u                 Uninstall R"
	echo "  -b                 Install binaries. Once compiled, the code can be transferred to"
	echo "                     another machine and easily installed there."
	echo "                     Options -s and -p are not needed in this case."
    echo "  -s <spu-name>      The name or address of one of the SPUs. Normally this option"
	echo "                     does not have to be specified, because the script can detect"
	echo "                     the SPUs. Use this option in case the SPU detection fails."
	echo "  -p <spu-password>  The root password for the SPUs. The script must perform several"
	echo "                     compilation steps on the SPUs. If the password is not specified,"
	echo "                     the script will ask for the password."
	echo "  -d <src-directory> The directory containing the sources of R and other necessary tools."
	echo "                     If -b is specified, it must contain the created binary files instead."
	echo "                     If it is not specified, the current directory is used as default."
	[ "$DL" = "1" ] && 
	echo "                     If a source code archive is not found, the script tries to download it."
	echo "  -v                 Print versions of: Script, NPS, Netezza Analytics (if exist), R (if exist)."
	echo "  -h                 Print this help."
	echo ""
    echo "For installation, the following source code archives must exist in the source directory:"
	[ "$DL" = "1" ] &&
	echo "(the script tries to downloaded these versions if a source code archive does not exist)"
	echo "termcap-1.3.1.tar.gz or later"
	echo "ncurses-5.9.tar.gz or later"
	echo "readline-6.2.tar.gz or later"
	echo "libiconv-1.14.tar.gz or later"
	echo "make-3.82.tar.gz or later"
	echo "bash-4.3.tar.gz or later"
	echo "coreutils-8.13.tar.gz or later"
	echo "findutils-4.4.2.tar.gz or later"
	echo "grep-2.9.tar.gz or later"
	echo "sed-4.2.tar.gz or later"
	echo "tar-1.26.tar.gz or later"
	echo "libpthread-stubs-0.1.tar.bz2 or later"
	echo "pkg-config-0.28.tar.gz or later"
	echo "R-2.14.0.tar.gz or later"
	echo "Note: The R version depends on the Netezza Analytics version."
	echo "      For Netezza Analytics 2.5.2 and 2.5.3, the recommended R version is R-2.14.x."
	echo "      For Netezza Analytics 2.5.4 and 3.0.0, the recommended R version is R-2.15.x."
	echo "      For Netezza Analytics 3.0.1 and 3.0.2, the recommended R version is R-3.0.x."
	echo ""
        echo " If installing R-3.3.x or higher versions, you need to download the following also: "
        echo "zlib-1.2.8.tar.gz or later"
        echo "pcre-8.38.tar.gz  or later"
        echo "bzip2-1.0.6.tar.gz or later"
        echo "xz-5.2.2.tar.bz2 or later"
        echo "openssl-1.0.2h.tar.gz or later"
        echo "curl-7.49.1 or later"
        echo "  "
	echo "Any use of this script is entirely at your own risk."
	echo "Downloading and installing Open Source R and all other required packages is subject to the"
	echo "terms and conditions that are mentioned in the appropriate license files of those packages."
	
	echo ""
	finish_and_exit $1
}

define_locations () {
	local export_root="$1"
	
	AE_DIR="${export_root}/ae"
	LANG_DIR="${AE_DIR}/languages"

	HOST64_DIR="${AE_DIR}/sysroot/host64"
	SPU64_DIR="${AE_DIR}/sysroot/spu64"
	HOST_DIR="${AE_DIR}/sysroot/host"
	SPU_DIR="${AE_DIR}/sysroot/spu"

	DEV_DIR="${AE_DIR}/tmpdev"
	DEV_HOST64_DIR="${DEV_DIR}/sysroot/host64"
	DEV_SPU64_DIR="${DEV_DIR}/sysroot/spu64"
	
	PERL_HOST_DIR="$(cd "${LANG_DIR}"/perl/*/host; pwd)"
	PERL_SPU_DIR="$(cd "${LANG_DIR}"/perl/*/spu; pwd)"
	
	OVERLAYDIR=/nz/extensions/nz/r_install
	
	if [ "$2" = "v" ]; then
		LOGFILE="${OVERLAYDIR}/dummy.log" # Not used
	elif [ "$2" = "u" ]; then
		LOGFILE="${OVERLAYDIR}/uninstall_r.log" # Written by host only
	else
		if [ "${INSTALL_BIN}" = "1" ]; then
			LOGFILE="${OVERLAYDIR}/install_r.log"
		else
			LOGFILE="${DEV_DIR}/install_r.log" # Written by host and spu
		fi	
	fi
	
	INFOFILE=r_info
}

reset_logfile () {
	mkdir -p "$(dirname "$LOGFILE")"
	> "$LOGFILE"
}

copy_to_overlay_dir () {
	mkdir -p "${OVERLAYDIR}"
	[ "$(dirname $(readlink -mn "$1"))" != "$(readlink -mn "${OVERLAYDIR}")" ] && cp -f "$1" "${OVERLAYDIR}" # Copy to same file ?
	echo "${OVERLAYDIR}/$(basename "$1")"
}

finish_and_exit () {
	# Print exit message
	if [ "$2" != "" ]; then
		if [ -e "$LOGFILE" ]; then
			[ "$1" = "0" ]  && echo "$2" | tee -a "$LOGFILE"
			[ "$1" != "0" ] && echo "$2" | tee -a "$LOGFILE" >&2
		else
			[ "$1" = "0" ]  && echo "$2"
			[ "$1" != "0" ] && echo "$2" >&2		
		fi
	fi

	# Cleanup only on Host
	if [ -d /nz/kit ]; then # "${CC/*spu64*/x}" != "x"
		# Copy logfile to overlay dir, overlay files should be there already
		if [ -s "$LOGFILE" ]; then
			LOGFILE="$(copy_to_overlay_dir "$LOGFILE")"
			echo "Logfile: $LOGFILE"
		fi
		
		# On success, remove DEV_DIR (on error it might be needed for problem analysis)
		if [ "$1" = "0" -a "${DEV_DIR}" != "" ]; then
			rm --preserve-root -f -r "${DEV_DIR}"
		fi
	fi
	exit $1
}

log_and_progress () {
	tee -a "$LOGFILE" | sed -u s/.*/0/ | xargs -L ${1:-1} | sed -u s/.*/0/ | xargs -I@ echo -n "."
	echo
}

check_patches () {
	local patch_directory="$1" # e.g. bash-4.3-patches 
	local patch_url="$2"       # e.g. ftp://......./bash-4.3-patches/
	local patch_prefix="$3"    # e.g. bash (bash43-001, bash43-002, ...)
	
	mkdir -p "${patch_directory}"
	
	local result=$(ls -1 "$SRCDIR/$patch_directory" | grep -E "${patch_prefix}*")
	local patch_count=$(echo "$result" | wc -w)
	local rc
	
	if [ $patch_count -eq 0 -a "$DL" = "1" -a "$DL_BASH_PATCHES" = "1" ]; then
		echo -n $(echo "Downloading patches" | tee -a "$LOGFILE") ""
		wget -r -l 1 -t 2 -T 30 -nd -P "$SRCDIR/$patch_directory" "${patch_url}" 2>&1 | log_and_progress 5
		rc=${PIPESTATUS[0]}
		if [ $rc -eq 0 ]; then
			# Remove downloaded signature files and all other files not starting with patch_prefix
			rm -f "$SRCDIR/$patch_directory"/*.sig
			GLOBIGNORE="$SRCDIR/$patch_directory/${patch_prefix}*"
			rm -f "$SRCDIR/$patch_directory"/*
			unset GLOBIGNORE

			patch_count=$(ls -1 "$SRCDIR/$patch_directory" | wc -w)
			echo "Done downloading $patch_count patches." | tee -a "$LOGFILE"
		else
			# Hopefully return_code 1 just means "directory does not exist", i.e no patches available
			if [ $rc -eq 1 ]; then 
				echo "No patches found." | tee -a "$LOGFILE"
			else
				finish_and_exit 1 "Downloading patches for ${patch_prefix} failed (wget return code: $rc)."
			fi
		fi
	fi
}

apply_patches () {
	local dir="$1"
	local patch_dir="$2"
	local patchlevel=0
	
	echo -n $(echo "Applying patches for ${dir}" | tee -a "$LOGFILE") ""
	for i in "${SRCDIR}/${patch_dir}"/*; do 
		patch -p0 -d "${DEV_DIR}/${dir}" < $i 
	done | log_and_progress 2
	
	if [ -f ${DEV_DIR}/${dir}/patchlevel.h ]; then
		patchlevel=$(cat ${DEV_DIR}/${dir}/patchlevel.h | grep "#define PATCHLEVEL" | sed "s/[^0-9]//g")
	fi
	echo "New patchlevel for ${dir}: ${patchlevel}." | tee -a "$LOGFILE"
}

check_src () {
	local result_variable_name="$1"
	local pattern="$2"
	local url="$3"
	local result=$(ls -1 "$SRCDIR" | grep -E "$pattern")
	local cnt=$(echo "$result" | wc -w)
	[ $cnt -gt 1 ] && finish_and_exit 1 "There is more than one file that matches pattern $pattern in directory $SRCDIR"
	if [ $cnt -eq 0 ]; then
		if [ "$DL" = "1" ]; then # Try to download the source code
			echo -n $(echo "Downloading $url" | tee -a "$LOGFILE") ""
			wget --no-check-certificate -t 2 -T 30 -P "$SRCDIR" "$url" 2>&1 | log_and_progress 5
			if [ ${PIPESTATUS[0]} -ne 0 ]; then
				echo "Failed. See Logfile." | tee -a "$LOGFILE"
				result="$pattern"
			else	
				echo "Done." | tee -a "$LOGFILE"
				result=$(ls -1 "$SRCDIR" | grep -E "$pattern")
			fi			
		else
			result="$pattern"
		fi
	fi
	eval $result_variable_name="\$result"
}

extract_src () {
	local file="$1"
	local dir="$2"
	local opt=
	cd "${DEV_DIR}"
	echo -n $(echo "Extracting ${SRCDIR}/$file" | tee -a "$LOGFILE") ""
	[ "${file:(-3)}" = ".gz" ] && opt=z
	[ "${file:(-4)}" = ".bz2" ] && opt=j
	tar xv${opt}f "${SRCDIR}/$file" 2>&1 | log_and_progress 50
	[ ${PIPESTATUS[0]} -ne 0 ] && finish_and_exit 1 "Error on extracting ${SRCDIR}/$file."
	[ ! -d "$dir" ] && finish_and_exit 1 "After extraction of $file, the directory $dir does not exist."
}

configure_src () {
	local dir="$1"
	shift
	local system="Host"
	[ "${CC/*spu64*/x}" = "x" ] && system="SPU "
	echo -n $(echo "${system}: Configuring $dir" | tee -a "$LOGFILE") ""
	cd "${DEV_DIR}/$dir"
	# If Netezza Analytics 2.0 was on the system before, which used GCC 4.3.2 (with libgfortran.so.3) for the SPUs, 
	# it is possible that libgfortran.so.3 can be in the path before libgfortran.so.2, which does not work with GCC 4.2.4.
	# Needed only for the R compilation. Normally, the R configure script defines FLIBS=-lgfortran
	FLIBS=-l:libgfortran.so.2 ./configure "$@" 2>&1 | log_and_progress 50
	[ ${PIPESTATUS[0]} -ne 0 ] && finish_and_exit 1 "$dir: Error on execution of './configure'."
}

make_src () {
	local dir="$1"
	local system="Host"
	[ "${CC/*spu64*/x}" = "x" ] && system="SPU "
	echo -n $(echo "${system}: Compiling $dir" | tee -a "$LOGFILE") ""
	cd "${DEV_DIR}/$dir"
	make -j 2>&1 | log_and_progress 20
	[ ${PIPESTATUS[0]} -ne 0 ] && finish_and_exit 1 "$dir: Error on execution of 'make -j'."
	
	echo -n $(echo "${system}: Installing $dir" | tee -a "$LOGFILE") ""
	make install 2>&1 | log_and_progress 50
	[ ${PIPESTATUS[0]} -ne 0 ] && finish_and_exit 1 "$dir: Error on execution of 'make install'."
	
	echo -n $(echo "${system}: Cleanup $dir" | tee -a "$LOGFILE") ""
	make clean 2>&1 | log_and_progress 50
	make distclean 2>&1 | log_and_progress 50
}

exec_on_spu () {
	local rc
	[ "$1" = "" ] && finish_and_exit 1 "No command for ssh was specified."
	if [ "$SPUPASS" = "" ]; then
		ssh root@${SPU_ADDRESS} "$@"
		rc=$?
		[ $rc -eq 255 ] && finish_and_exit 1 "Incorrect password for the SPU specified."
		[ $rc -ne 0 ] && finish_and_exit 1 # Msg printed by remote script
	else
		export SSH_ASKPASS=$(mktemp)
		[ -z "$DISPLAY" ] && export DISPLAY=foo:0 # SSH_ASKPASS is used only by ssh if DISPLAY is set 
		echo "echo \$SPUPASS" > "${SSH_ASKPASS}"
		chmod +x "${SSH_ASKPASS}"
		setsid ssh root@${SPU_ADDRESS} "$@"
		rc=$?
		[ $rc -eq 255 ] && finish_and_exit 1 "Option -p: Incorrect password for the SPU specified."
		[ $rc -ne 0 ] && finish_and_exit 1 # Msg printed by remote script
		rm "${SSH_ASKPASS}"
	fi
}

create_spu_script() {
	SCRIPT="${DEV_DIR}/$1" # Script file, global variable
	local compiler_optimization="$2" # Optimization flags used for CFLAGS/FFLAGS/FCFLAGS/CXXFLAGS
	local linker_optimization="$3" # Optimization flags used for LDFLAGS
	local mkl_lib_path="$4"
	local libpath="-L${SPU64_DIR}/lib64/extra -L${SPU64_DIR}/lib64 -L${SPU64_DIR}/usr/lib64 -L${SPU64_DIR}/generic/x86_64-generic-linux-gnu/lib64"
	local rpath="${SPU64_DIR}/lib64/extra:${SPU64_DIR}/lib64:${SPU64_DIR}/usr/lib64:${SPU64_DIR}/generic/x86_64-generic-linux-gnu/lib64"
	if [ "${mkl_lib_path}" != "" ]; then
		libpath="-L${mkl_lib_path} ${libpath}"
		rpath="${mkl_lib_path}:${rpath}"
	fi
	
	cat << end-of-input | sed "s/^\t\+//" > ${SCRIPT}
		#!${DEV_SPU64_DIR}/bin/bash
		# We need the same shell (bash) as on the host
		$(declare -f define_locations log_and_progress finish_and_exit configure_src make_src)
		define_locations "${NZ_EXPORT_DIR}" ${MODE} # Set variables
		export PATH="${SPU64_DIR}/extra/bin:${SPU64_DIR}/bin:${SPU_DIR}/bin:${PERL_SPU_DIR}/bin:\${PATH}"
		export LD_LIBRARY_PATH=${SPU64_DIR}/lib64/extra:${SPU64_DIR}/lib64:${SPU64_DIR}/usr/lib64
		export LDFLAGS="-m64 ${linker_optimization} ${libpath} -Wl,-rpath=${rpath}"
		export CPPFLAGS="-m64 -I${SPU64_DIR}/extra/include -I${SPU64_DIR}/include"
		
		export CFLAGS="-m64 ${compiler_optimization}"
		export FFLAGS="$CFLAGS"
		export FCFLAGS="$CFLAGS"
		export CXXFLAGS="$CFLAGS"
		
		export CC=${SPU64_DIR}/bin/gcc
		export CXX=${SPU64_DIR}/bin/g++
		
		export TMP="/var/opt/nz/local/tmp" # /tmp on the spu could be too small
		export TEMP="\$TMP"
		export TMPDIR="\$TMP"
end-of-input
	chmod a+x ${SCRIPT}
}
	
create_overlay_files () {
	local archive="${1}.tar.gz"
	local uninstallinfo="uninstall_info_${1}"
	cd "$2" # root directory for tar
	echo -n $(echo "Creating ${archive}" | tee -a "$LOGFILE") ""
	mkdir -p "${OVERLAYDIR}"
	tar cvzf "${OVERLAYDIR}/${archive}" "${3}" 2>&1 | log_and_progress 500
	[ ${PIPESTATUS[0]} -ne 0 ] && finish_and_exit 1 "Error on creating ${OVERLAYDIR}/${archive}"
	tar -tf "${OVERLAYDIR}/${archive}" | sort -r > "${OVERLAYDIR}/${uninstallinfo}"
}

expected_r () {
	local inza="$1"
	local expected_variable_name="$2"
	local download_variable_name="$3"
	
	local expected_r_version="3.0"
	local download_r_version="3.0.2"
	[ "$inza" = "2.5.0" ] && expected_r_version="2.14"
	[ "$inza" = "2.5.1" ] && expected_r_version="2.14"
	[ "$inza" = "2.5.2" ] && expected_r_version="2.14"
	[ "$inza" = "2.5.3" ] && expected_r_version="2.14"
	[ "$inza" = "2.5.4" ] && expected_r_version="2.15"
	[ "$inza" = "3.0.0" ] && expected_r_version="2.15"
	[ "$inza" = "3.0.1" ] && expected_r_version="3.0"
	[ "$inza" = "3.0.2" ] && expected_r_version="3.5"

	[ "$expected_r_version" = "2.14" ] && download_r_version="R-2.14.2.tar.gz"
	[ "$expected_r_version" = "2.15" ] && download_r_version="R-2.15.3.tar.gz"
	[ "$expected_r_version" = "3.0" ]  && download_r_version="R-3.0.2.tar.gz"
	[ "$expected_r_version" = "3.5" ]  && download_r_version="R-3.5.1.tar.gz"

	[ "$expected_variable_name" != "" ] && eval $expected_variable_name="\$expected_r_version"
	[ "$download_variable_name" != "" ] && eval $download_variable_name="\$download_r_version"
}
	
install_r () {
	local missing

	DO_EXTRACT=1
	DO_HOST=1
	DO_SPU=1
	DO_HOST_R=1
	DO_SPU_R=1

	# These variables are used for R compilation/linking only
	USEMKL=1
	COMP_OPTIMIZATION="-O1" # CFLAGS (C), CXXFLAGS (C++), FFLAGS (Fortran), FCFLAGS (Fortran95), -O1 recommeded for gcc 4.3.x
	LINK_OPTIMIZATION="-Bdirect,--hash-style=both,-Wl,-O1" # LDFLAGS
	
	# Create working directory and logfile
	mkdir -p ${DEV_DIR}
	rm --preserve-root -f -r "${DEV_DIR}" # Clean up possible leftovers from a previous run
	
	reset_logfile
	
	# Check existence of source files

	# We need (all are needed for the SPUs, the first 4 for the host also):
	# - GNU termcap: termcap-1.3.1.tar.gz or later
	# - GNU ncurses: ncurses-5.9.tar.gz
	# - GNU readline libraries (for R command-line editing): readline-6.2.tar.gz or later
	#   They do not exist on SPUs, on the host the header files are missing
	# - GNU iconv libraries: libiconv-1.14.tar.gz or later
	# - GNU make (does not exist on SPU): make-3.82.tar.gz or later
	# - GNU bash (does not exist on SPU): bash-4.3.tar.gz or later
	# - GNU core utilities (partially on SPU [BusyBox]): coreutils-8.13.tar.gz or later
	# - GNU find utilities: findutils-4.4.2.tar.gz or later
	# - GNU grep (does not exist on SPU): grep-2.9.tar.gz or later
	# - GNU sed (only in [BusyBox], but needed as executable): sed-4.2.tar.gz or later
	# - GNU tar (only in [BusyBox], but needed as executable): tar-1.26.tar.gz or later
	# - libpthread-stubs-0.1.tar.bz2 or later
	# - Does not exist on SPU: pkg-config-0.28.tar.gz or later
	# - R itself
	TERMCAP_SRC_URL="ftp://ftp.gnu.org/pub/gnu/termcap/termcap-1.3.1.tar.gz"
	NCURSES_SRC_URL="ftp://ftp.gnu.org/pub/gnu/ncurses/ncurses-5.9.tar.gz"
	READLINE_SRC_URL="ftp://ftp.gnu.org/gnu/readline/readline-6.2.tar.gz"
	ICONV_SRC_URL="ftp://ftp.gnu.org/pub/gnu/libiconv/libiconv-1.14.tar.gz"
	MAKE_SRC_URL="ftp://ftp.gnu.org/pub/gnu/make/make-3.82.tar.gz"
	BASH_SRC_URL="ftp://ftp.gnu.org/gnu/bash/bash-4.3.tar.gz"
	COREUTILS_SRC_URL="ftp://ftp.gnu.org/gnu/coreutils/coreutils-8.13.tar.gz"
	FINDUTILS_SRC_URL="ftp://ftp.gnu.org/gnu/findutils/findutils-4.4.2.tar.gz"
	GREP_SRC_URL="ftp://ftp.gnu.org/gnu/grep/grep-2.9.tar.gz"
	SED_SRC_URL="ftp://ftp.gnu.org/gnu/sed/sed-4.2.tar.gz"
	TAR_SRC_URL="ftp://ftp.gnu.org/gnu/tar/tar-1.26.tar.gz"
	STUB_SRC_URL="http://xcb.freedesktop.org/dist/libpthread-stubs-0.1.tar.bz2"
	PKGCONFIG_SRC_URL="http://pkgconfig.freedesktop.org/releases/pkg-config-0.28.tar.gz"
	
	expected_r "$INZA_VERSION" R_EXPECT R_DOWNLOAD
	R_SRC_URL="http://lib.stat.cmu.edu/R/CRAN/src/base/R-${R_EXPECT:0:1}/${R_DOWNLOAD}"
	
	TERMCAP_SRC_PATTERN="termcap-[0-9]+\.[0-9]+(\.[0-9]+)?\.tar\.gz" # >= 1.3.1
	NCURSES_SRC_PATTERN="ncurses-[0-9]+\.[0-9]+\.tar\.gz" # >= 5.9
	READLINE_SRC_PATTERN="readline-[0-9]+\.[0-9]+\.tar\.gz" # >= 6.2
	ICONV_SRC_PATTERN="libiconv-[0-9]+\.[0-9]+\.tar\.gz" # >= 1.14
	MAKE_SRC_PATTERN="make-[0-9]+\.[0-9]+\.tar\.gz" # >= 3.82
	BASH_SRC_PATTERN="bash-[0-9]+\.[0-9]+\.tar\.gz" # >= 4.3
	COREUTILS_SRC_PATTERN="coreutils-[0-9]+\.[0-9]+\.tar\.gz" # >= 8.13
	FINDUTILS_SRC_PATTERN="findutils-[0-9]+\.[0-9]+(\.[0-9]+)?\.tar\.gz" # >= 4.4.2
	GREP_SRC_PATTERN="grep-[0-9]+\.[0-9]+\.tar\.gz" # >= 2.9
	SED_SRC_PATTERN="sed-[0-9]+\.[0-9]+\.tar\.gz" # >= 4.2
	TAR_SRC_PATTERN="tar-[0-9]+\.[0-9]+\.tar\.gz" # >= 1.26
	STUB_SRC_PATTERN="libpthread-stubs-[0-9]+\.[0-9]+\.tar\.bz2" # >= 0.1
	PKGCONFIG_SRC_PATTERN="pkg-config-[0-9]+\.[0-9]+\.tar\.gz" # >= 0.28
	R_SRC_PATTERN="R-[2-3]\.[0-9]+\.[0-9]+\.tar\.gz" # >= 2.14

	check_src TERMCAP_SRC "$TERMCAP_SRC_PATTERN" "$TERMCAP_SRC_URL"
	[ "$TERMCAP_SRC" = "$TERMCAP_SRC_PATTERN" ] && missing="${missing}, termcap-1.3.1.tar.gz or later"
	TERMCAP_DIR=${TERMCAP_SRC%.tar.gz}

	check_src NCURSES_SRC "$NCURSES_SRC_PATTERN" "$NCURSES_SRC_URL"
	[ "$NCURSES_SRC" = "$NCURSES_SRC_PATTERN" ] && missing="${missing}, ncurses-5.9.tar.gz or later"
	NCURSES_DIR=${NCURSES_SRC%.tar.gz}

	check_src READLINE_SRC "$READLINE_SRC_PATTERN" "$READLINE_SRC_URL"
	[ "$READLINE_SRC" = "$READLINE_SRC_PATTERN" ] && missing="${missing}, readline-6.2.tar.gz or later"
	READLINE_DIR=${READLINE_SRC%.tar.gz}

	check_src ICONV_SRC "$ICONV_SRC_PATTERN" "$ICONV_SRC_URL"
	[ "$ICONV_SRC" = "$ICONV_SRC_PATTERN" ] && missing="${missing}, libiconv-1.14.tar.gz or later"
	ICONV_DIR=${ICONV_SRC%.tar.gz}

	check_src MAKE_SRC "$MAKE_SRC_PATTERN" "$MAKE_SRC_URL"
	[ "$MAKE_SRC" = "$MAKE_SRC_PATTERN" ] && missing="${missing}, make-3.82.tar.gz or later"
	MAKE_DIR=${MAKE_SRC%.tar.gz}

	check_src BASH_SRC "$BASH_SRC_PATTERN" "$BASH_SRC_URL"
	[ "$BASH_SRC" = "$BASH_SRC_PATTERN" ] && missing="${missing}, bash-4.3.tar.gz or later"
	BASH_DIR=${BASH_SRC%.tar.gz}

	BASH_PATCH_DIR=${BASH_DIR}-patches
	check_patches "${BASH_PATCH_DIR}" "ftp://ftp.gnu.org/gnu/bash/${BASH_PATCH_DIR}/" bash 
	
	check_src COREUTILS_SRC "$COREUTILS_SRC_PATTERN" "$COREUTILS_SRC_URL"
	[ "$COREUTILS_SRC" = "$COREUTILS_SRC_PATTERN" ] && missing="${missing}, coreutils-8.13.tar.gz or later"
	COREUTILS_DIR=${COREUTILS_SRC%.tar.gz}

	check_src FINDUTILS_SRC "$FINDUTILS_SRC_PATTERN" "$FINDUTILS_SRC_URL"
	[ "$FINDUTILS_SRC" = "$FINDUTILS_SRC_PATTERN" ] && missing="${missing}, findutils-4.4.2.tar.gz or later"
	FINDUTILS_DIR=${FINDUTILS_SRC%.tar.gz}

	check_src GREP_SRC "$GREP_SRC_PATTERN" "$GREP_SRC_URL"
	[ "$GREP_SRC" = "$GREP_SRC_PATTERN" ] && missing="${missing}, grep-2.9.tar.gz or later"
	GREP_DIR=${GREP_SRC%.tar.gz}

	check_src SED_SRC "$SED_SRC_PATTERN" "$SED_SRC_URL"
	[ "$SED_SRC" = "$SED_SRC_PATTERN" ] && missing="${missing}, sed-4.2.tar.gz or later"
	SED_DIR=${SED_SRC%.tar.gz}

	check_src TAR_SRC "$TAR_SRC_PATTERN" "$TAR_SRC_URL"
	[ "$TAR_SRC" = "$TAR_SRC_PATTERN" ] && missing="${missing}, tar-1.26.tar.gz or later"
	TAR_DIR=${TAR_SRC%.tar.gz}

	check_src STUB_SRC "$STUB_SRC_PATTERN" "$STUB_SRC_URL"
	[ "$STUB_SRC" = "$STUB_SRC_PATTERN" ] && missing="${missing}, libpthread-stubs-0.1.tar.bz2 or later"
	STUB_DIR=${STUB_SRC%.tar.bz2}

	check_src PKGCONFIG_SRC "$PKGCONFIG_SRC_PATTERN" "$PKGCONFIG_SRC_URL"
	[ "$PKGCONFIG_SRC" = "$PKGCONFIG_SRC_PATTERN" ] && missing="${missing}, pkg-config-0.28.tar.gz or later"
	PKGCONFIG_DIR=${PKGCONFIG_SRC%.tar.gz}

	check_src R_SRC "$R_SRC_PATTERN" "$R_SRC_URL"
	[ "$R_SRC" = "$R_SRC_PATTERN" ] && missing="${missing}, R-2.14.0.tar.gz or later"
	R_DIR=${R_SRC%.tar.gz}
	R_FULL_VERSION=$(expr "${R_DIR}" : '.*\([0-9]\+\.[0-9]\+\.[0-9]\+\)')
	R_VERSION=$(expr "${R_FULL_VERSION}" : '\([0-9]\+\.[0-9]\+\)')

	[ "${missing}" != "" ] && finish_and_exit 1 "The following source files cannot be found: ${missing:1}"

	[ "$RINFO" != "" -a "$RINFO" != "$R_FULL_VERSION" ] && finish_and_exit 1 "You want to install R-${R_FULL_VERSION}, but R-${RINFO} is alreday installed. Uninstall R-${RINFO} first."
	
        # if R Version is higher than 3.3.0, then we need to install zlib, pcre, bzip2, and xz libraries
        ver_int=${R_FULL_VERSION//.}
        if [ $ver_int -ge 330 ]; then
           EXTRA_LIB=1   # variable to do extra library installation
           ZLIB_SRC_URL="http://zlib.net/zlib-1.2.11.tar.gz"
           PCRE_SRC_URL="https://sourceforge.net/projects/pcre/files/pcre/8.38/pcre-8.38.tar.gz/download"
           BZIP2_SRC_URL="https://sourceforge.net/projects/bzip2/files/latest/download"
           XZ_SRC_URL="http://tukaani.org/xz/xz-5.2.2.tar.bz2"
           OPENSSL_SRC_URL="https://www.openssl.org/source/openssl-1.0.2h.tar.gz"
           CURL_SRC_URL="https://curl.haxx.se/download/curl-7.61.0.tar.bz2"
           
           ZLIB_SRC_PATTERN="zlib-[0-9]+\.[0-9]+(\.[0-9]+)?\.tar\.gz" # >= 1.2.8
           PCRE_SRC_PATTERN="pcre-[0-9]+\.[0-9]+\.tar\.gz" # >= 8.38
           BZIP2_SRC_PATTERN="bzip2-[0-9]+\.[0-9]+(\.[0-9]+)?\.tar\.gz" # >= 1.0.6
           XZ_SRC_PATTERN="xz-[0-9]+\.[0-9]+(\.[0-9]+)?\.tar\.bz2" # >= 5.2.2
           OPENSSL_SRC_PATTERN="openssl-[0-9]+\.[0-9]+(\.[0-9a-z]+)?\.tar\.gz"  # >=1.0.2h
           CURL_SRC_PATTERN="curl-[0-9]+\.[0-9]+(\.[0-9]+)?\.tar\.bz2" # >= 7.49.1

           check_src ZLIB_SRC "$ZLIB_SRC_PATTERN" "$ZLIB_SRC_URL"
	   [ "$ZLIB_SRC" = "$ZLIB_SRC_PATTERN" ] && missing="${missing}, zlib-1.2.8.tar.gz or later"
	   ZLIB_DIR=${ZLIB_SRC%.tar.gz}

           check_src PCRE_SRC "$PCRE_SRC_PATTERN" "$PCRE_SRC_URL"
	   [ "$PCRE_SRC" = "$PCRE_SRC_PATTERN" ] && missing="${missing}, pcre-8.38.tar.gz or later"
	   PCRE_DIR=${PCRE_SRC%.tar.gz}

           check_src BZIP2_SRC "$BZIP2_SRC_PATTERN" "$BZIP2_SRC_URL"
	   [ "$BZIP2_SRC" = "$BZIP2_SRC_PATTERN" ] && missing="${missing}, bzip2-1.0.6.tar.gz or later"
	   BZIP2_DIR=${BZIP2_SRC%.tar.gz}

           check_src XZ_SRC "$XZ_SRC_PATTERN" "$XZ_SRC_URL"
	   [ "$XZ_SRC" = "$XZ_SRC_PATTERN" ] && missing="${missing}, xz-5.2.2.tar.bz2 or later"
	   XZ_DIR=${XZ_SRC%.tar.bz2}

           check_src OPENSSL_SRC "$OPENSSL_SRC_PATTERN" "$OPENSSL_SRC_URL"
	   [ "$OPENSSL_SRC" = "$OPENSSL_SRC_PATTERN" ] && missing="${missing}, openssl-1.0.2h.tar.gz or later"
	   OPENSSL_DIR=${OPENSSL_SRC%.tar.gz}

           check_src CURL_SRC "$CURL_SRC_PATTERN" "$CURL_SRC_URL"
	   [ "$CURL_SRC" = "$CURL_SRC_PATTERN" ] && missing="${missing}, curl-7.49.1.tar.bz2 or later"
	   CURL_DIR=${CURL_SRC%.tar.bz2}

        fi
          
	### Extract sources ############################################################

	if [ "${DO_EXTRACT}" = "1" ]; then
		extract_src "${TERMCAP_SRC}"   "${TERMCAP_DIR}"
		sed -i "s/oldincludedir.*=.*\/usr\/include//" "${DEV_DIR}/${TERMCAP_DIR}/Makefile.in" # To prevent msg: cannot create regular file `/usr/include/termcap.h': Permission denied
		extract_src "${NCURSES_SRC}"   "${NCURSES_DIR}"
		extract_src "${READLINE_SRC}"  "${READLINE_DIR}"
		extract_src "${ICONV_SRC}"     "${ICONV_DIR}"
		extract_src "${MAKE_SRC}"      "${MAKE_DIR}"
		extract_src "${BASH_SRC}"      "${BASH_DIR}"
		extract_src "${COREUTILS_SRC}" "${COREUTILS_DIR}"
		extract_src "${FINDUTILS_SRC}" "${FINDUTILS_DIR}"
		extract_src "${GREP_SRC}"      "${GREP_DIR}"
		extract_src "${SED_SRC}"       "${SED_DIR}"
		extract_src "${TAR_SRC}"       "${TAR_DIR}"
		extract_src "${STUB_SRC}"      "${STUB_DIR}"
		extract_src "${PKGCONFIG_SRC}" "${PKGCONFIG_DIR}"
		extract_src "${R_SRC}"         "${R_DIR}"
		
                if [ "${EXTRA_LIB}" = "1" ]; then
		    extract_src "${ZLIB_SRC}" "${ZLIB_DIR}"
		    extract_src "${PCRE_SRC}" "${PCRE_DIR}"
		    extract_src "${BZIP2_SRC}" "${BZIP2_DIR}"
		    extract_src "${XZ_SRC}" "${XZ_DIR}"
		    extract_src "${OPENSSL_SRC}" "${OPENSSL_DIR}"
		    extract_src "${CURL_SRC}" "${CURL_DIR}"
                fi

		apply_patches "${BASH_DIR}" "${BASH_PATCH_DIR}"
	fi

	### BUILDING TOOLS FOR HOST ####################################################

	if [ "${DO_HOST}" = "1" ]; then
		(
		# Path for GCC and G++ compilers for host
		export CC="${HOST64_DIR}/bin/gcc"
		export CXX="${HOST64_DIR}/bin/g++"
		
		configure_src "${TERMCAP_DIR}" "--prefix=${DEV_HOST64_DIR}" "--enable-shared"
		make_src      "${TERMCAP_DIR}"

		configure_src "${NCURSES_DIR}" "--prefix=${DEV_HOST64_DIR}" "--libdir=${DEV_HOST64_DIR}/lib64" "--with-shared"
		make_src      "${NCURSES_DIR}"

		configure_src "${READLINE_DIR}" "--prefix=${DEV_HOST64_DIR}" "--libdir=${DEV_HOST64_DIR}/lib64"
		make_src      "${READLINE_DIR}"

		configure_src "${ICONV_DIR}" "--prefix=${DEV_HOST64_DIR}" "--libdir=${DEV_HOST64_DIR}/lib64"
		make_src      "${ICONV_DIR}"

                if [ "${EXTRA_LIB}" = "1" ]; then
		    configure_src "${ZLIB_DIR}" "--prefix=${DEV_HOST64_DIR}/extra" "--libdir=${DEV_HOST64_DIR}/lib64/extra"
		    make_src      "${ZLIB_DIR}"

		    configure_src "${PCRE_DIR}" "--prefix=${DEV_HOST64_DIR}/extra" "--libdir=${DEV_HOST64_DIR}/lib64/extra" "--enable-shared" "--enable-utf" "--enable-pcre8" "--enable-pcre16" "--enable-pcre32"
		    make_src      "${PCRE_DIR}"
                    # R-3.3.0 installation is looking for include/pcre/pcre.h not just include/pcre.h 
                    mkdir -p ${DEV_HOST64_DIR}/extra/include/pcre  | tee -a "$LOGFILE"
                    cp ${DEV_HOST64_DIR}/extra/include/pcre* ${DEV_HOST64_DIR}/extra/include/pcre  > /dev/null 2>&1

                    # special instructions for buildin bzip2 library
	            echo  $(echo "Host: Configuring ${BZIP2_DIR}" | tee -a "$LOGFILE") ""
                    cd ${DEV_DIR}/${BZIP2_DIR}
	            echo $(echo "Host: Compiling ${BZIP2_DIR}" | tee -a "$LOGFILE") ""
                    make -f Makefile-libbz2_so  2>&1 | log_and_progress 20
	            echo $(echo "Host: Installing ${BZIP2_DIR}" | tee -a "$LOGFILE") ""
                    make install PREFIX=${DEV_HOST64_DIR}/extra  2>&1 | log_and_progress 50
                    mv libbz2.so* ${DEV_HOST64_DIR}/lib64/extra   | tee -a "$LOGFILE"
                    mv bzip2-shared ${DEV_HOST64_DIR}/lib64/extra  | tee -a "$LOGFILE"
	            echo $(echo "Host: Cleaning ${BZIP2_DIR}" | tee -a "$LOGFILE") ""
                    make clean  >> "$LOGFILE" 2>&1
                    make distclean >> "$LOGFILE" 2>&1
                    cd ${DEV_HOST64_DIR}/lib64/extra 
                    ln -s libbz2.so.1.0 libbz2.so
                    cd ${DEV_DIR}
                    
		    configure_src "${XZ_DIR}" "--prefix=${DEV_HOST64_DIR}/extra" "--libdir=${DEV_HOST64_DIR}/lib64/extra"
		    make_src      "${XZ_DIR}"

                    # special instructions for building openssl library
	            echo  $(echo "Host: Configuring ${OPENSSL_DIR}" | tee -a "$LOGFILE") ""
                    cd ${DEV_DIR}/${OPENSSL_DIR}
                    ./Configure shared --prefix=${DEV_HOST64_DIR}/extra linux-x86_64  >> "$LOGFILE" 
	            echo $(echo "Host: Compiling ${OPENSSL_DIR}" | tee -a "$LOGFILE") ""
                    make  2>&1 | log_and_progress 20
	            echo $(echo "Host: Installing ${OPENSSL_DIR}" | tee -a "$LOGFILE") ""
                    make install  2>&1 | log_and_progress 50
                    cp -r ${DEV_HOST64_DIR}/extra/lib/* ${DEV_HOST64_DIR}/lib64/extra/  | tee -a "$LOGFILE"
	            echo $(echo "Host: Cleaning ${OPENSSL_DIR}" | tee -a "$LOGFILE") ""
                    make clean  >> "$LOGFILE" 2>&1
                    make distclean >> "$LOGFILE" 2>&1
                    cd ${DEV_DIR}
                    
		    configure_src "${CURL_DIR}" "--prefix=${DEV_HOST64_DIR}/extra" "--libdir=${DEV_HOST64_DIR}/lib64/extra" "--enable-shared" "--enable-http" "--with-ssl=${DEV_HOST64_DIR}/extra"
		    make_src      "${CURL_DIR}"


                fi
		)
	fi

	### BUILDING TOOLS FOR SPU #####################################################

	if [ "${DO_SPU}" = "1" ]; then
		(
		# Path for GCC and G++ compilers for spu
		export CC="${SPU64_DIR}/bin/gcc"
		export CXX="${SPU64_DIR}/bin/g++"

		configure_src "${TERMCAP_DIR}" "--prefix=${DEV_SPU64_DIR}" "--enable-shared"
		make_src      "${TERMCAP_DIR}"

		configure_src "${NCURSES_DIR}" "--prefix=${DEV_SPU64_DIR}" "--libdir=${DEV_SPU64_DIR}/lib64" "--with-shared"
		make_src      "${NCURSES_DIR}"

		configure_src "${READLINE_DIR}" "--prefix=${DEV_SPU64_DIR}" "--libdir=${DEV_SPU64_DIR}/lib64"
		make_src      "${READLINE_DIR}"

		configure_src "${ICONV_DIR}" "--prefix=${DEV_SPU64_DIR}" "--libdir=${DEV_SPU64_DIR}/lib64"
		make_src      "${ICONV_DIR}"

		configure_src "${MAKE_DIR}" "--prefix=${DEV_SPU64_DIR}" "--libdir=${DEV_SPU64_DIR}/lib64"
		make_src      "${MAKE_DIR}"

		configure_src "${BASH_DIR}" "--prefix=${DEV_SPU64_DIR}" "--libdir=${DEV_SPU64_DIR}/lib64"
		make_src      "${BASH_DIR}"

		configure_src "${GREP_DIR}" "--prefix=${DEV_SPU64_DIR}" "--libdir=${DEV_SPU64_DIR}/lib64"
		make_src      "${GREP_DIR}"

		configure_src "${SED_DIR}" "--prefix=${DEV_SPU64_DIR}" "--libdir=${DEV_SPU64_DIR}/lib64" "--with-shared"
		make_src      "${SED_DIR}"

		configure_src "${TAR_DIR}" "--prefix=${DEV_SPU64_DIR}" "--libdir=${DEV_SPU64_DIR}/lib64"
		make_src      "${TAR_DIR}"

		rm -f "${DEV_SPU64_DIR}"/bin/*pkg-config # In case the script is re-run, the installer complains about existing files
		configure_src "${PKGCONFIG_DIR}" "--prefix=${DEV_SPU64_DIR}" "--libdir=${DEV_SPU64_DIR}/lib64" "--with-shared" "--with-internal-glib"
		make_src      "${PKGCONFIG_DIR}"

		configure_src "${FINDUTILS_DIR}" "--prefix=${DEV_SPU64_DIR}" "--libdir=${DEV_SPU64_DIR}/lib64" "--with-shared"
		make_src      "${FINDUTILS_DIR}"

                if [ "${EXTRA_LIB}" = "1" ]; then
                    configure_src "${ZLIB_DIR}" "--prefix=${DEV_SPU64_DIR}/extra" "--libdir=${DEV_SPU64_DIR}/lib64/extra"
                    make_src      "${ZLIB_DIR}"

		    configure_src "${PCRE_DIR}" "--prefix=${DEV_SPU64_DIR}/extra" "--libdir=${DEV_SPU64_DIR}/lib64/extra" "--enable-shared" "--enable-utf" "--enable-pcre8" "--enable-pcre16" "--enable-pcre32"
                    make_src      "${PCRE_DIR}"
                    # R-3.3.0 installation is looking for include/pcre/pcre.h not just include/pcre.h
                    mkdir -p ${DEV_SPU64_DIR}/extra/include/pcre | tee -a "$LOGFILE"
                    cp ${DEV_SPU64_DIR}/extra/include/pcre* ${DEV_SPU64_DIR}/extra/include/pcre > /dev/null 2>&1

                    # special instructions for buildin bzip2 library
                    echo  $(echo "SPU: Configuring ${BZIP2_DIR}" | tee -a "$LOGFILE") ""
                    cd ${DEV_DIR}/${BZIP2_DIR}
                    echo $(echo "SPU: Compiling ${BZIP2_DIR}" | tee -a "$LOGFILE") ""
                    make -f Makefile-libbz2_so 2>&1 | log_and_progress 20
                    echo $(echo "SPU: Installing ${BZIP2_DIR}" | tee -a "$LOGFILE") ""
                    make install PREFIX=${DEV_SPU64_DIR}/extra  2>&1 | log_and_progress 50
                    mv libbz2.so* ${DEV_SPU64_DIR}/lib64/extra | tee -a "$LOGFILE"
                    mv bzip2-shared ${DEV_SPU64_DIR}/lib64/extra | tee -a "$LOGFILE"
                    echo $(echo "SPU: Cleaning ${BZIP2_DIR}" | tee -a "$LOGFILE") ""
                    make clean  >> "$LOGFILE" 2>&1
                    make distclean >> "$LOGFILE" 2>&1
                    cd ${DEV_SPU64_DIR}/lib64/extra
                    ln -s libbz2.so.1.0 libbz2.so
                    cd ${DEV_DIR}

                    configure_src "${XZ_DIR}" "--prefix=${DEV_SPU64_DIR}/extra" "--libdir=${DEV_SPU64_DIR}/lib64/extra"
                    make_src      "${XZ_DIR}"

                    # special instructions for building openssl library
                    echo  $(echo "SPU: Configuring ${OPENSSL_DIR}" | tee -a "$LOGFILE") ""
                    cd ${DEV_DIR}/${OPENSSL_DIR}
                    ./Configure shared --prefix=${DEV_SPU64_DIR}/extra linux-x86_64  >> "$LOGFILE"
                    echo $(echo "SPU: Compiling ${OPENSSL_DIR}" | tee -a "$LOGFILE") ""
                    make  2>&1 | log_and_progress 20
                    echo $(echo "SPU: Installing ${OPENSSL_DIR}" | tee -a "$LOGFILE") ""
                    make install  2>&1 | log_and_progress 50
                    cp -r ${DEV_SPU64_DIR}/extra/lib/* ${DEV_SPU64_DIR}/lib64/extra/  | tee -a "$LOGFILE"
                    echo $(echo "SPU: Cleaning ${OPENSSL_DIR}" | tee -a "$LOGFILE") ""
                    make clean >> "$LOGFILE" 2>&1
                    make distclean >> "$LOGFILE" 2>&1
                    cd ${DEV_DIR}

         export LDFLAGS=-Wl,-rpath-link=/lib64
         configure_src "${CURL_DIR}" "--prefix=${DEV_SPU64_DIR}/extra" "--libdir=${DEV_SPU64_DIR}/lib64/extra" "--enable-shared" "--enable-http" "--with-ssl=${DEV_SPU64_DIR}/extra"
         make_src      "${CURL_DIR}"
         export LDFLAGS=
     fi

		)

		# Coreutils needs to be build on the spu, we create a shell script and run it on the spu
		create_spu_script build_coreutils_spu.sh # Creates basic script, sets also SCRIPT variable
		# libfreebl3 is needed by libcrypt (needed to compile coreutils), but is not contained in the spu library searched by the linker
		echo "[ ! -e \"${DEV_SPU64_DIR}/lib64/libfreebl3.so\" ] && cp /lib64/libfreebl3.so \"${DEV_SPU64_DIR}/lib64/\"" >> ${SCRIPT} 
		echo "[ ! -e \"${DEV_SPU64_DIR}/lib64/libdl.so.2\" ] && cp /lib64/libdl.so.2 \"${DEV_SPU64_DIR}/lib64/\"" >> ${SCRIPT}
		# Set additional variables for compilation/linking, path to ${DEV_SPU64_DIR}/bin needed for "make"
		echo "export PATH=\"${DEV_SPU64_DIR}/bin:\${PATH}\"" >> ${SCRIPT} 
		echo "export LDFLAGS=\"-L${DEV_SPU64_DIR}/lib64 -Wl,-rpath-link=${DEV_SPU64_DIR}/lib64 \${LDFLAGS}\"" >> ${SCRIPT}
		echo "export LIBS=\"-lfreebl3 -lcrypt\"" >> ${SCRIPT} 
		# Configure & Make
		echo "configure_src \"${COREUTILS_DIR}\" \"--prefix=${DEV_SPU64_DIR}\" \"--libdir=${DEV_SPU64_DIR}/lib64\" \"--enable-no-install-program\"" >> ${SCRIPT} 
		echo "make_src      \"${COREUTILS_DIR}\"" >> ${SCRIPT}
		exec_on_spu ${SCRIPT}
		
		# There are shell scripts (e.g. run_r.sh) that expect the "basename" tool in spu/bin instead of spu64/bin
		mkdir -p "${DEV_DIR}/sysroot/spu/bin"
		cd "${DEV_DIR}/sysroot/spu/bin"
		ln -s ../../spu64/bin/basename .
	fi	

	### Put all new compiled and installed tools into .../ae/sysroot ###############

	if [ "$BASH_DELETE" = "1" ]; then
		mv "${DEV_SPU64_DIR}/bin/bash" "${DEV_DIR}" 2>&1 | tee -a "$LOGFILE"
 		mv "${DEV_SPU64_DIR}/bin/bashbug" "${DEV_DIR}" 2>&1 | tee -a "$LOGFILE"
	fi

	create_overlay_files sysroot_overlay "${DEV_DIR}" sysroot
	
	echo -n $(echo "Copying generated code into sysroot directory" | tee -a "$LOGFILE") ""
	tar xvzf "${OVERLAYDIR}/sysroot_overlay.tar.gz" -C ${AE_DIR} 2>&1 | log_and_progress 500
	[ ${PIPESTATUS[0]} -ne 0 ] && finish_and_exit 1 "Error on extracting ${OVERLAYDIR}/sysroot_overlay.tar.gz"

	if [ "$BASH_DELETE" = "1" ]; then
		cp "${DEV_DIR}/bash" "${DEV_SPU64_DIR}/bin/bash" 2>&1 | tee -a "$LOGFILE"
                cp "${DEV_DIR}/bashbug" "${DEV_SPU64_DIR}/bin/bashbug" 2>&1 | tee -a "$LOGFILE"

                mv "${DEV_DIR}/bash" "${SPU64_DIR}/bin/bash" 2>&1 | tee -a "$LOGFILE"
                mv "${DEV_DIR}/bashbug" "${SPU64_DIR}/bin/bashbug" 2>&1 | tee -a "$LOGFILE"
        fi
		
	### Write installation info into the R directory (to detect our installation) ##
	
	mkdir -p "${LANG_DIR}/r/${R_VERSION}"
	echo "${R_FULL_VERSION}" > "${LANG_DIR}/r/${R_VERSION}/${INFOFILE}"
	
	### Setting variables for use of Intel MKL libraries ###########################
	
	unset MKL_LIB_PATH MKL WITH_BLAS WITH_LAPACK
	unset SET_MAIN_LDFLAGS SET_LD_LIBRARY_PATH
	if [ "$USEMKL" = "1" ]; then
		MKL_LIB_PATH="${AE_DIR}/core/thirdParty/intel_mkl/current/lib/em64t"		
		MKL="-L${MKL_LIB_PATH} -lmkl_gf_lp64 -lmkl_gnu_thread -lmkl_lapack -lmkl_core -liomp5 -lpthread"
		WITH_BLAS="--with-blas=${MKL}"
		WITH_LAPACK="--with-lapack"
		SET_MAIN_LDFLAGS="export MAIN_LDFLAGS=-lpthread"
		SET_LD_LIBRARY_PATH="export LD_LIBRARY_PATH=${MKL_LIB_PATH}:\${LD_LIBRARY_PATH}"
	fi
	
	### BUILDING R FOR HOST ########################################################
	
	if [ "${DO_HOST_R}" = "1" ]; then
		(
		export PATH="${HOST64_DIR}/extra/bin:${HOST64_DIR}/bin:${HOST64_DIR}/x86_64-rhel5-linux-gnu/bin:${HOST_DIR}/bin:${PERL_HOST_DIR}/bin:/usr/bin:/bin"

		export CC="${HOST64_DIR}/bin/gcc"
		export CXX="${HOST64_DIR}/bin/g++"
		
		eval ${SET_MAIN_LDFLAGS}
		eval ${SET_LD_LIBRARY_PATH}
		
		libpath="-L${HOST64_DIR}/lib64/extra -L${HOST64_DIR}/lib64 -L${HOST64_DIR}/usr/lib64 -L${HOST64_DIR}/x86_64-rhel5-linux-gnu/lib64"
		[ "${MKL_LIB_PATH}" != "" ] && libpath="-L${MKL_LIB_PATH} ${libpath}"
		rpath="${HOST64_DIR}/lib64/extra:${HOST64_DIR}/lib64:${HOST64_DIR}/usr/lib64:${HOST64_DIR}/x86_64-rhel5-linux-gnu/lib64"
		[ "${MKL_LIB_PATH}" != "" ] && rpath="${MKL_LIB_PATH}:${rpath}"
		
		export LDFLAGS=" -m64 ${LINK_OPTIMIZATION} ${libpath} -Wl,-rpath=${rpath} -Wl,-rpath-link=${HOST64_DIR}/lib64/extra:${MKL_LIB_PATH}"
		export CPPFLAGS="-m64 -I${HOST64_DIR}/extra/include -I${HOST64_DIR}/include"
		export CFLAGS="-m64 ${COMP_OPTIMIZATION}"
		export CXXFLAGS=${CFLAGS}
		export FFLAGS=${CFLAGS}
		export FCFLAGS=${CFLAGS}
		
		echo ""
		echo "Building R for host (compilation may take a while)"
		configure_src "${R_DIR}" "--prefix=${LANG_DIR}/r/${R_VERSION}/host64" "--with-x=no" "--enable-R-shlib" "${WITH_BLAS}" "${WITH_LAPACK}"
		make_src      "${R_DIR}"
		)
	fi

	### BUILDING R FOR SPU #########################################################

	if [ "${DO_SPU_R}" = "1" ]; then
		# In R3.1 we see a source file removed after host build, so we better re-create the source tree for the spu build
		if [ "${DO_EXTRACT}" = "1" ]; then
			extract_src "${R_SRC}" "${R_DIR}"
		fi

		create_spu_script build_r_spu.sh "${COMP_OPTIMIZATION}" "${LINK_OPTIMIZATION}" "${MKL_LIB_PATH}" # Creates basic script, sets also SCRIPT variable
		
		echo "[ ! -e \"${DEV_SPU64_DIR}/lib64/libm.so.6\" ] && cp /lib64/libm.so.6 \"${DEV_SPU64_DIR}/lib64/\"" >> ${SCRIPT}
		echo "echo \"Building R for spu (compilation may take a while)\"" >> ${SCRIPT}
		echo "${SET_MAIN_LDFLAGS}" >> ${SCRIPT} 
		echo "${SET_LD_LIBRARY_PATH}" >> ${SCRIPT} 
		echo "export LDFLAGS=\"-Wl,-rpath-link=${DEV_SPU64_DIR}/lib64:${DEV_SPU64_DIR}/lib64/extra:${DEV_SPU64_DIR}/extra/lib:${MKL_LIB_PATH}:/lib64 \${LDFLAGS}\"" >> ${SCRIPT}
		echo "configure_src \"${R_DIR}\" \"--prefix=${LANG_DIR}/r/${R_VERSION}/spu64\" \"--with-x=no\" \"--enable-R-shlib\" \"${WITH_BLAS}\" \"${WITH_LAPACK}\"" >> ${SCRIPT}
		echo "make_src      \"${R_DIR}\"" >> ${SCRIPT} 
		exec_on_spu ${SCRIPT}
	fi

	### Complete installation info
	echo "Compiled and installed by $0" >> ${LANG_DIR}/r/${R_VERSION}/${INFOFILE}
	
	### Put all generated R code into a .tar.gz file ###############################

	create_overlay_files r_${R_FULL_VERSION}_overlay "${AE_DIR}" languages/r/${R_VERSION}

	if [ "$BASH_DELETE" = "1" ]; then
		rm -f "${DEV_SPU64_DIR}/bin/bash" 2>&1 | tee -a "$LOGFILE"
		rm -f "${DEV_SPU64_DIR}/bin/bashbug" 2>&1 | tee -a "$LOGFILE"

		rm -f "${SPU64_DIR}/bin/bash" 2>&1 | tee -a "$LOGFILE"
		rm -f "${SPU64_DIR}/bin/bashbug" 2>&1 | tee -a "$LOGFILE"
	fi
}

install_r_binaries () {
	# Check binaries exist
	local r_file=($(echo "${SRCDIR}/r_*_overlay.tar.gz")) # One or more R overlay file(s)
	[ ! -e "${r_file[0]}" ]  && finish_and_exit 1 "A binary file ${SRCDIR}/r_<version>_overlay.tar.gz cannot be found."
	[ "${r_file[1]}" != "" ] && finish_and_exit 1 "There is more than one file ${SRCDIR}/r_<version>_overlay.tar.gz."
	
	r_file=$(basename ${r_file[0]})
	R_FULL_VERSION=$(expr "${r_file}" : '.*\([0-9]\+\.[0-9]\+\.[0-9]\+\)')
	R_VERSION=$(expr "${R_FULL_VERSION}" : '\([0-9]\+\.[0-9]\+\)')
	
	[ ! -e "${SRCDIR}/sysroot_overlay.tar.gz" ] && finish_and_exit 1 "A binary file ${SRCDIR}/sysroot_overlay.tar.gz cannot be found."
	
	# Check there is no other R version (installed by this script)
	[ "$RINFO" != "" -a "$RINFO" != "${R_FULL_VERSION}" ] && finish_and_exit 1 "You want to install R-${R_FULL_VERSION}, but R-${RINFO} is alreday installed. Uninstall R-${RINFO} first."
	
	# Create uninstall info
	tar -tf "${SRCDIR}/sysroot_overlay.tar.gz" | sort -r > "${OVERLAYDIR}/uninstall_info_sysroot_overlay"
	[ ${PIPESTATUS[0]} -ne 0 ] && finish_and_exit 1 "Error on creating uninstall info for ${SRCDIR}/sysroot_overlay.tar.gz"
	tar -tf "${SRCDIR}/${r_file}" | sort -r > "${OVERLAYDIR}/uninstall_info_r_${R_FULL_VERSION}_overlay"
	[ ${PIPESTATUS[0]} -ne 0 ] && finish_and_exit 1 "Error on creating uninstall info for ${SRCDIR}/${r_file}"
	
	# Extract binaries
	echo -n $(echo "Copying binaries into sysroot directory" | tee -a "$LOGFILE") ""
	tar xvzf "${SRCDIR}/sysroot_overlay.tar.gz" -C ${AE_DIR} 2>&1 | log_and_progress 500
	[ ${PIPESTATUS[0]} -ne 0 ] && finish_and_exit 1 "Error on extracting ${SRCDIR}/sysroot_overlay.tar.gz"
	
	echo -n $(echo "Copying binaries into R language directory" | tee -a "$LOGFILE") ""
	tar xvzf "${SRCDIR}/${r_file}" -C ${AE_DIR} 2>&1 | log_and_progress 500
	[ ${PIPESTATUS[0]} -ne 0 ] && finish_and_exit 1 "Error on extracting ${SRCDIR}/${r_file}"
}

uninstall () {
	local filename
	[ ! -e "$2" ]  && finish_and_exit 1 "There is no $1 uninstall file in directory ${OVERLAYDIR}."
	[ "$3" != "" ] && finish_and_exit 1 "There is more than one $1 uninstall file in directory ${OVERLAYDIR}."
	
	echo -n $(echo "Uninstall $1" | tee -a "$LOGFILE") ""
	while read filename
	do
		if [ -d "${AE_DIR}/$filename" ]; then 
			rmdir "${AE_DIR}/$filename" 2>/dev/null
			[ "$?" = "0" ] && echo "Removed: ${AE_DIR}/$filename"
		else 
			rm "${AE_DIR}/$filename" 2>/dev/null
			[ "$?" = "0" ] && echo "Removed: ${AE_DIR}/$filename"
		fi;
	done < "$2" | log_and_progress 500
}

uninstall_r () {
	uninstall "R" "${OVERLAYDIR}"/uninstall_info_r_*_overlay
}

uninstall_tools () {
	uninstall "Sysroot Tools for R" "${OVERLAYDIR}"/uninstall_info_sysroot_overlay
}

#### Command Line Parameters ###################################################

case $1 in "-?"|"--?"|"-h"|"--h"|"-help"|"--help") usage 0; esac

unset MODE INSTALL_BIN SPU_ADDRESS SPU_PASSWORD SRCDIR 
unset SPUPASS INZA_VERSION RINFO TOOLSUNINSTALL

# Parse command-line options
while getopts ibuvs:p:d: OPT; do
	case "$OPT" in
		i) [ "$MODE" = "u" -o "$MODE" = "v" ] && echo "You can specify only one of the options: -i, -u, -v." >&2 && exit 1;
		   MODE=i ;;
		b) INSTALL_BIN=1 ;;   
		u) [ "$MODE" = "i" -o "$MODE" = "v" ] && echo "You can specify only one of the options: -i, -u, -v." >&2 && exit 1;
		   MODE=u ;;
		s) SPU_ADDRESS="$OPTARG" ;;
		p) SPU_PASSWORD="$OPTARG" ;;
		d) SRCDIR="$OPTARG" ;
		   SRCDIR="$(readlink -mn "$SRCDIR")" ;; # Make absolute path out of possible relative path
		v) [ "$MODE" = "i" -o "$MODE" = "u" ] && echo "You can specify only one of the options: -i, -u, -v." >&2 && exit 1;
		   MODE=v ;;   
	esac
done

# Remove the parsed optional parameters.
shift $(expr $OPTIND - 1)

[ -z "$MODE" ]         && MODE=i # Default: Installation
[ -z "$SPU_ADDRESS" ]  && SPU_ADDRESS=$(nzhw show -type spu -detail | grep -o -m 1 "[0-9]\+\.[0-9]\+\.[0-9]\+\.[0-9]\+")
[ -z "$SPU_PASSWORD" ] && SPU_PASSWORD= # Default: none
[ -z "$SRCDIR" ]       && SRCDIR="$(pwd)" # Default: Current directory

export SPUPASS="${SPU_PASSWORD}"

### Others Variable Definitions ################################################

define_locations "${NZ_EXPORT_DIR}" ${MODE}

### Checks #####################################################################

# Check the user who calls the script (nz)
[ "$(whoami)" != "nz" ] && finish_and_exit 1 "This script must be called by user nz."

# Check, if Netezza Analytics components (nzcm) are installed
INZA_VERSION=$(nzcm --inza 2>&1)
[ $? != 0 ] && INZA_VERSION=
ver_num=$(echo "$INZA_VERSION" | grep -o "[0-9]\+\.[0-9]\+\.[0-9]\+") # Try to extract exact version number (e.g. 2.5.4)
[ "$ver_num" != "" ] && INZA_VERSION="$ver_num"

# Check for existing R installations
# (the same R version, another R version, R from another vendor)
RINFO=
firstmatch=($(echo "${LANG_DIR}"/r/*/host*/bin)) # One or more R directories
if [ -d "$firstmatch" ]; then # R exists
	firstmatch=($(echo "${LANG_DIR}"/r/*/$INFOFILE)) # One or more INFOFILEs
	if [ -e "$firstmatch" ]; then # R was installed by this script
		RINFO=$(head -n 1 "$firstmatch")
	else # Installed by someone else
		RINFO="An existing R installation was found. It was not installed by this program."
		if [ "$MODE" != "v" ]; then
			finish_and_exit 1 "$RINFO This R version must be uninstalled first."
		fi
		
	fi
fi

# Print version information and exit
if [ "$MODE" = "v" ]; then
	echo "Script version: ${SCRIPT_VERSION}"
	echo "NPS version: $(nzrev)"
	echo "Installed Netezza Analytics version: ${INZA_VERSION:=None}"
	echo "Installed R version: ${RINFO:=None}"
	finish_and_exit 0
fi

# Specific checks only for installation / uninstallation
if [ "$MODE" = "i" ]; then
	[ "$INZA_VERSION" = "" ] && finish_and_exit 1 "Netezza Analytics cartridge manager cannot be called. Check Netezza Analytics installation. Check also if NPS is online."
	
	# Check, if src directory exists
	[ ! -d "$SRCDIR" ] && finish_and_exit 1 "The directory $SRCDIR does not exist."

	if [ "${INSTALL_BIN}" != "1" ]; then
		# Check, if the SPU address is correct
		ping -c 1 -w 10 "${SPU_ADDRESS}" > /dev/null 2>&1
		[ $? -ne 0 ] && finish_and_exit 1 "Cannot ping SPU with name/address '${SPU_ADDRESS}'."
	
		# If the user has not specified the spu password, ask for it now
		# If the user has specified the spu password, check if it is correct
		if [ -z ${SPUPASS} ]; then
			echo "You did not specify the root password for the SPUs (option -p)."
			echo "Please specify the password now:"
			read spupw;
			if [ -z ${spupw} ]; then
				echo "You did not specify a password. The program will ask you for the password later two times."
			else
				export SPUPASS="${spupw}"
				exec_on_spu cd
			fi
		else
			exec_on_spu cd
		fi
	
		# Check PERL (needed for optional coreutils test and R manual installation)
		[ ! -e "${PERL_HOST_DIR}/bin/perl" ] && "Perl cannot be found for host in ${PERL_HOST_DIR}/bin."
		[ ! -e "${PERL_SPU_DIR}/bin/perl" ] && "Perl cannot be found for spu in ${PERL_SPU_DIR}/bin."
	fi
else
	# Check if the user wants do uninstall also the tools installed in sysroot (command-line option ?)
	TOOLSUNINSTALL=0
	echo "Do you want to uninstall also the prerequisite tools installed for R (y/n) [y]?"
	read answer;
	[ "$answer" != "n" -a "$answer" != "N" ] && TOOLSUNINSTALL=1
	
	[ "$INZA_VERSION" = "" ] && echo "Warning: Netezza Analytics Cartridge Manager not found. Netezza Analytics R Language Adapter will not be uninstalled."
fi

####################################################

if [ "$MODE" = "i" ]; then
	if [ "${INSTALL_BIN}" = "1" ]; then
		install_r_binaries
	else
		install_r
	fi
	
	# Set a link if the Netezza Analytics R AE installation procedure expects another R version
	expected_r "$INZA_VERSION" R_EXPECT
	if [ "$R_EXPECT" != "" -a "$R_EXPECT" != "$R_VERSION" ]; then
		# We create a link so that the directory with the expected version exists
		ln -s "${LANG_DIR}/r/${R_VERSION}" "${LANG_DIR}/r/${R_EXPECT}"
		echo ""
		echo "Note: You installed R version ${R_VERSION}, but Netezza Analytics ${INZA_VERSION} expects R version ${R_EXPECT}."
		echo "A symbolic link was created so that Netezza Analytics assumes R version ${R_EXPECT} was installed."
		echo ""
	fi
	
	### Install and register Netezza Analytics R AE cartridge ######################
	echo "Installing and registering Netezza Analytics R Language Adapter" | tee -a "$LOGFILE"
	yes yes | nzcm --nocolors -u -f r_ae | log_and_progress 2 # In case r_ae is already registered, it must be unregistered first, but we ignore errors
	yes yes | nzcm --nocolors -i r_ae | log_and_progress 2
	[ ${PIPESTATUS[1]} -ne 0 ] && finish_and_exit 1 "Error on installing Netezza Analytics R Language Adapter"
	nzcm --nocolors -r r_ae | log_and_progress 2
	[ ${PIPESTATUS[0]} -ne 0 ] && finish_and_exit 1 "Error on registering Netezza Analytics R Language Adapter"
	
	ENDTIME=$(date +%s)
	DIFFTIME=$(($ENDTIME-$BEGINTIME))

	echo ""
	echo "--------------------------------------------------------------------------------"
	echo "Installation of R completed after $(($DIFFTIME/60)) minutes and $(($DIFFTIME%60)) seconds."
	echo "You can call R on the Netezza host as follows: ${LANG_DIR}/r/${R_VERSION}/host64/bin/R"
	echo ""
	echo "The directory ${OVERLAYDIR} contains the files uninstall_info_sysroot_overlay and uninstall_info_r_<version>_overlay."
	echo "Do not remove them, they are needed for uninstallation of R."
	echo "The directory contains also the logfile and the files sysroot_overlay.tar.gz and r_<version>_overlay.tar.gz."
	echo "These tar files contain the binaries for easy re-installation or installation of R on another Netezza system:"
	echo "  - Create directory ${OVERLAYDIR} on the target machine and copy these two files into it."
	echo "  - Run \"$(basename $0) -i -b -d ${OVERLAYDIR}\" on the target machine."
	echo ""
else
	reset_logfile

	if [ "$INZA_VERSION" != "" ]; then
		### Unregister and uninstall Netezza Analytics R AE cartridge ###################
		echo "Unregistering and uninstallation of Netezza Analytics R Language Adapter" | tee -a "$LOGFILE"
		yes yes | nzcm --nocolors -u -f r_ae | log_and_progress 2
		[ ${PIPESTATUS[1]} -ne 0 ] && finish_and_exit 1 "Error on unregistering Netezza Analytics R Language Adapter"
		yes yes | nzcm --nocolors -e -f r_ae | log_and_progress 2
		[ ${PIPESTATUS[1]} -ne 0 ] && finish_and_exit 1 "Error on uninstalling Netezza Analytics R Language Adapter"
	fi
	
	# In case Netezza Analytics was uninstalled before R, it is possible that the nzrserver R package 
	# (installed by nzcm -r ae) was not uninstalled correctly, so we do it in any case here again
	rm -rf ${LANG_DIR}/r/*/host64/lib64/R/library/nzrserver
	rm -rf ${LANG_DIR}/r/*/spu64/lib64/R/library/nzrserver
	
	# Remove symbolic link(s) (expected R version) if exists
	find ${LANG_DIR}/r -maxdepth 1 -type l -exec rm -f {} \;
	
	uninstall_r
	[ "$TOOLSUNINSTALL" = "1" ] && uninstall_tools
	
	echo "Uninstallation of R completed."
	echo "Note: If you have installed additional R packages for Host or SPUs, they must be removed manually."
	echo ""
fi

finish_and_exit 0

