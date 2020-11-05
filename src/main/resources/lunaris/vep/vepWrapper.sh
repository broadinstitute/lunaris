#!/bin/bash

vepScript=$1
input=$2
cpus=$3
fasta=$4
dirPlugins=$5
dbNSFP=$6
output=$7
warnings=$8

user=$USER
if id vep &>/dev/null; then
  echo "User vep exists, so will execute vep as vep user."
  user=vep
  export USER=vep
  export HOME=~vep
  chmod a+rwx $(dirname $input)
  chmod a+rwx $(dirname $output)
  chmod a+rwx $(dirname $warnings)
fi

echo "= = = Begin of vep invocation"
cat <<COMMANDLINE
$vepScript -i $input \
--fork $cpus \
--force_overwrite \
--no_stats \
--offline \
--fasta $fasta \
--tab \
--cache \
--dir_plugins $dirPlugins \
--polyphen b \
--sift b \
--ccds \
--canonical \
--appris \
--tsl \
--biotype \
--regulatory \
--assembly GRCh37 \
--flag_pick_allele \
--pick_order tsl,biotype,appris,rank,ccds,canonical,length \
--domains flags \
--plugin LoF \
--plugin LoFtool \
--plugin dbNSFP,${dbNSFP},ALL \
--output_file $output \
--warning_file $warnings
COMMANDLINE
echo "= = = End of vep invocation"

sudo -u $user $vepScript -i $input \
--fork $cpus \
--force_overwrite \
--no_stats \
--offline \
--fasta $fasta \
--tab \
--cache \
--dir_plugins $dirPlugins \
--polyphen b \
--sift b \
--ccds \
--canonical \
--appris \
--tsl \
--biotype \
--regulatory \
--assembly GRCh37 \
--flag_pick_allele \
--pick_order tsl,biotype,appris,rank,ccds,canonical,length \
--domains flags \
--plugin LoF \
--plugin LoFtool \
--plugin dbNSFP,${dbNSFP},ALL \
--output_file $output \
--warning_file $warnings
