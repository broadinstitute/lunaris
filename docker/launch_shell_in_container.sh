#!/usr/bin/env bash

# Builds Docker image for given Lunaris version, then starts Bash from within image. Ideal for debugging the image.

version=$1
if [[ -z ${version} ]]; then
    echo "Need to specify version (e.g. 1.2.5)."
    exit
fi
image=lunaris:${version}
echo "Building image ${image}."
sudo docker build ~/git/lunaris/docker/ -t ${image}
echo "Done building image ${image}, now launching shell."
sudo docker run -it ${image} bash
echo "Done"
