#!/bin/bash

vepScript=$1
input=$2
cpus=$3
fasta=$4
dirPlugins=$5
dbNSFP=$6
output=$7
warnings=$8

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