#!/usr/bin/env bash

cmd=$@
image=lunaris-variant-mask-server:1.5.15

echo "This is Lunaris Variant Mask Server Docker run script"

if [[ -z ${cmd} ]]; then
  cmd="lunaris vep --config-file configs/lunarisDocker.conf"
  echo "No command specified, that's fine, defaulting to:"
  echo $cmd
  echo "This should start the Lunaris Variant Mask Server"
fi

lunaris_vep_dir=$HOME/lunaris/vep
aux_dir=$lunaris_vep_dir/aux
data_dir=$lunaris_vep_dir/data

echo "But first, if not done already, building image ${image}."
sudo docker build ~/git/lunaris/vep/docker/ -t ${image}
echo "Done building image ${image}, now running command."
sudo docker run -p 8080:8080 -v $aux_dir:/mnt/aux -v $data_dir:/mnt/data -it ${image} ${cmd}
echo "Done with Lunaris Variant Mask Server Docker run script."
