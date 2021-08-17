#!/bin/bash

vepCmd=$1
input=$2
cpus=$3
fasta=$4
dirCache=$5
dirPlugins=$6
dbNSFP=$7
output=$8
warnings=$9

#user=$USER
#if id vep &>/dev/null; then
#  echo "User vep exists, so will execute vep as vep user."
#  user=vep
#  export USER=vep
#  export HOME=~vep
#  chmod a+rwx $(dirname $input)
#  chmod a+rwx $(dirname $output)
#  chmod a+rwx $(dirname $warnings)
#fi

echo "= = = Begin of vep invocation"
cat <<COMMANDLINE
$vepCmd -i $input \
--format vcf \
--fork $cpus \
--force_overwrite \
--no_stats \
--offline \
--fasta $fasta \
--tab \
--cache \
--dir_cache $dirCache \
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
--symbol \
--output_file $output \
--warning_file $warnings
COMMANDLINE
echo "= = = End of vep invocation"

cat <<COMMANDLINE
$vepCmd -i $input \
--format vcf \
--fork $cpus \
--force_overwrite \
--no_stats \
--offline \
--fasta $fasta \
--tab \
--cache \
--dir_cache $dirCache \
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
--symbol \
--output_file $output \
--warning_file $warnings
