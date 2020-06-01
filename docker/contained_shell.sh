#!/usr/bin/env bash
version=$1
if [[ -z ${version} ]]; then
    echo "Need to specify version (e.g. 1.0.2)."
    exit
fi
image=lunaris:${version}
echo "Building image ${image}."
sudo docker build ~/git/lunaris/docker/ -t ${image}
echo "Done building image ${image}, now launching shell."
sudo docker run -it ${image} bash
echo "Done"
