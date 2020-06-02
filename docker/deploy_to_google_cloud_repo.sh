#!/usr/bin/env bash

# Builds Docker image and deploys to Google Cloud Repository.
# Usage:
# ./deploy_to_google_cloud_repo.sh <Google project name> <Lunaris version>
# Example:
# ./deploy_to_google_cloud_repo.sh v2f-public-resources 1.0.2

project=$1
version=$2

if [[ -z ${project} ]] || [[ -z ${version} ]]; then
    echo "Need to specify Google project name and Lunaris version."
    exit
fi

image=lunaris:${version}
tag=gcr.io/${project}/${image}

echo "Now building and deploying Docker image ${image} to ${tag}."
gcloud builds submit --timeout=60m --tag ${tag} ~/git/lunaris/docker
echo "Done"
