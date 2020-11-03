#!/usr/bin/env bash

# Builds Docker image for given Lunaris version, then starts Bash from within image. Ideal for debugging the image.

image=lunaris-variant-mask-server:1.5.3
echo "Building image ${image}."
sudo docker build ~/git/lunaris/vep/docker/ -t ${image}
echo "Done building image ${image}, now launching shell."
sudo docker run -it ${image} bash
echo "Done"
