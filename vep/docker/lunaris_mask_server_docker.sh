#!/usr/bin/env bash

cmd=$@
image=lunaris-variant-mask-server:1.6.4

echo "This is Lunaris Variant Mask Server Docker run script"

if [[ -z ${cmd} ]]; then
  cmd="lunaris vep --config-file configs/lunarisVepDocker.conf"
  echo "No command specified, that's fine, defaulting to:"
  echo $cmd
  echo "This should start the Lunaris Variant Mask Server"
fi

lunaris_vep_dir=$HOME/lunaris/vep
aux_dir=$lunaris_vep_dir/aux
data_dir=$lunaris_vep_dir/data
work_dir=$lunaris_vep_dir/work
cache_dir=$HOME/.vep

echo "But first, if not done already, building image ${image}."
sudo docker build ~/git/lunaris/vep/docker/ -t ${image}
echo "Done building image ${image}, now running command."
sudo docker run -p 80:8080 -v $aux_dir:/mnt/aux -v $data_dir:/mnt/data -v $work_dir:/mnt/work \
  -v $cache_dir:/opt/vep/.vep -it ${image} ${cmd}
echo "Done with Lunaris Variant Mask Server Docker run script."
