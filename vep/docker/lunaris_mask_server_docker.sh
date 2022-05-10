#!/usr/bin/env bash

image=lunaris-variant-mask-server:3.7.0
lunaris_vep_dir=$HOME/lunaris/vep
inputs_dir=$lunaris_vep_dir/inputs
results_dir=$lunaris_vep_dir/results
aux_dir=$lunaris_vep_dir/aux
data_dir=$lunaris_vep_dir/data
work_dir=$lunaris_vep_dir/work
cache_dir=$HOME/.vep

echo "This is Lunaris Variant Mask Server Docker run script"

echo "But first, if not done already, building image ${image}."
sudo docker build vep/docker/ -t ${image}

echo "Done building image ${image}, now running command."

if [[ "$1" = "prod" ]] || [[ "$1" = "dev" ]] || [[ "$1" = "dev2" ]]; then
  if [[ "$1" = "prod" ]]; then
    port=80
    db_name="egg"
  elif [[ "$1" = "dev" ]]; then
    port=8080
    db_name="egg_dev"
  elif [[ "$1" = "dev2" ]]; then
    port=8888
    db_name="egg_dev2"
  fi
  echo "Launching the server in $1 mode on port $port."
  cmd="lunaris vep --db-name $db_name --config-file configs/lunarisVepDocker.conf"
  echo $cmd
  sudo docker run -p $port:8080 -v $inputs_dir:/mnt/inputs -v $results_dir:/mnt/results -v $aux_dir:/mnt/aux \
    -v $data_dir:/mnt/data -v $work_dir:/mnt/work -v $cache_dir:/opt/vep/.vep -it ${image} ${cmd}
elif [[ "$1" = "bash" ]]; then
  echo "Launching bash in the container"
  sudo docker run -v $inputs_dir:/mnt/inputs -v $results_dir:/mnt/results -v $aux_dir:/mnt/aux \
    -v $data_dir:/mnt/data -v $work_dir:/mnt/work -v $cache_dir:/opt/vep/.vep -it ${image} bash
else
  if [[ -z "$1" ]]; then
    echo "No subcommand specified"
  else
    echo "Unknown subcommand \"$1\""
  fi
  echo "Specify one of the following subcommands:"
  echo "  prod - launch server in production mode (i.e. port 80)"
  echo "  dev  - launch server in development mode (i.e. port 8080)"
  echo "  dev2  - launch server in development alternate mode (i.e. port 8888)"
  echo "  bash - Instead of server, launch bash. Useful for debugging Docker image."
fi

echo "Done with Lunaris Variant Mask Server Docker run script."
