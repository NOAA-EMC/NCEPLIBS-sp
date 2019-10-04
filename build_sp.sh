#!/bin/bash

 : ${THISDIR:=$(dirname $(readlink -f -n ${BASH_SOURCE[0]}))}
 CDIR=$PWD; cd $THISDIR

 source ./Conf/Analyse_args.sh
 source ./Conf/Collect_info.sh
 source ./Conf/Gen_cfunction.sh
 source ./Conf/Reset_version.sh

 if [[ ${sys} == "intel_general" ]]; then
   sys6=${sys:6}
   source ./Conf/Sp_${sys:0:5}_${sys6^}.sh
   rinst=false
 elif [[ ${sys} == "gnu_general" ]]; then
   sys4=${sys:4}
   source ./Conf/Sp_${sys:0:3}_${sys4^}.sh
   rinst=false
 else
   source ./Conf/Sp_intel_${sys^}.sh
 fi
 $CC --version &> /dev/null || {
   echo "??? SP: compilers not set." >&2
   exit 1
 }
 [[ -z ${SP_VER+x} || -z ${SP_LIB4+x} ]] && {
   [[ -z ${libver+x} || -z ${libver} ]] && {
     echo "??? SP: \"libver\" not set." >&2
     exit
   }
   SP_LIB4=lib${libver}_4.a
   SP_LIB8=lib${libver}_8.a
   SP_LIBd=lib${libver}_d.a
   SP_VER=v${libver##*_v}
 }

set -x
 spLib4=$(basename ${SP_LIB4})
 spLib8=$(basename ${SP_LIB8})
 spLibd=$(basename ${SP_LIBd})

#################
 cd src
#################

#-------------------------------------------------------------------
# Start building libraries
#
 echo
 echo "   ... build (i4/r4) sp library ..."
 echo
   make clean LIB=$spLib4
   FFLAGS4="$I4R4 $FFLAGS"
   collect_info sp 4 OneLine4 LibInfo4
   spInfo4=sp_info_and_log4.txt
   $debg && make debug FFLAGS="$FFLAGS4" LIB=$spLib4 &> $spInfo4 \
         || make build FFLAGS="$FFLAGS4" LIB=$spLib4 &> $spInfo4
   make message MSGSRC="$(gen_cfunction $spInfo4 OneLine4 LibInfo4)" \
                LIB=$spLib4

 echo
 echo "   ... build (i8/r8) sp library ..."
 echo
   make clean LIB=$spLib8
   FFLAGS8="$I8R8 $FFLAGS"
   collect_info sp 8 OneLine8 LibInfo8
   spInfo8=sp_info_and_log8.txt
   $debg && make debug FFLAGS="$FFLAGS8" LIB=$spLib8 &> $spInfo8 \
         || make build FFLAGS="$FFLAGS8" LIB=$spLib8 &> $spInfo8
   make message MSGSRC="$(gen_cfunction $spInfo8 OneLine8 LibInfo8)" \
                LIB=$spLib8

 echo
 echo "   ... build (i4/r8) sp library ..."
 echo
   make clean LIB=$spLibd
   FFLAGSd="$I4R8 $FFLAGS"
   collect_info sp d OneLined LibInfod
   spInfod=sp_info_and_logd.txt
   $debg && make debug FFLAGS="$FFLAGSd" LIB=$spLibd &> $spInfod \
         || make build FFLAGS="$FFLAGSd" LIB=$spLibd &> $spInfod
   make message MSGSRC="$(gen_cfunction $spInfod OneLined LibInfod)" \
                LIB=$spLibd

 $inst && {
#
#     Install libraries and source files 
#
   $local && {
     instloc=..
     LIB_DIR=$instloc/lib
     [ -d $LIB_DIR ] || { mkdir -p $LIB_DIR; }
     LIB_DIR4=$LIB_DIR
     LIB_DIR8=$LIB_DIR
     LIB_DIRd=$LIB_DIR
     SRC_DIR=
   } || {
     $rinst && {
       LIB_DIR4=$(dirname ${SP_LIB4})
       LIB_DIR8=$(dirname ${SP_LIB8})
       LIB_DIRd=$(dirname ${SP_LIBd})
       SRC_DIR=$SP_SRC
     } || {
       LIB_DIR=$instloc/lib
       LIB_DIR4=$LIB_DIR
       LIB_DIR8=$LIB_DIR
       LIB_DIRd=$LIB_DIR
       SRC_DIR=$instloc/src
       [[ $instloc == .. ]] && SRC_DIR=
     }
     [ -d $LIB_DIR4 ] || mkdir -p $LIB_DIR4
     [ -d $LIB_DIR8 ] || mkdir -p $LIB_DIR8
     [ -d $LIB_DIRd ] || mkdir -p $LIB_DIRd
     [ -z $SRC_DIR ] || { [ -d $SRC_DIR ] || mkdir -p $SRC_DIR; }
   }

   make clean LIB=
   make install LIB=$spLib4 LIB_DIR=$LIB_DIR4 SRC_DIR=
   make install LIB=$spLib8 LIB_DIR=$LIB_DIR8 SRC_DIR=
   make install LIB=$spLibd LIB_DIR=$LIB_DIRd SRC_DIR=$SRC_DIR
 }

