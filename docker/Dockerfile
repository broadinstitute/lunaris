# lunaris on Ubuntu

FROM ubuntu:18.04

WORKDIR /work

# Java Runtime Environment

RUN apt update && apt -y install openjdk-11-jdk-headless

# SBT, toole related to Debian packaging, lunaris 1.0.2

RUN apt -y update && \
    apt -y install gnupg2 fakeroot lintian && \
    echo "deb https://dl.bintray.com/sbt/debian /" | tee -a /etc/apt/sources.list.d/sbt.list && \
    apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv 2EE0EA64E40A89B84B2DF73499E82A75642AC823 && \
    apt-get -y update && \
    apt-get -y install sbt # && \
    git clone https://github.com/broadinstitute/lunaris.git && \
    cd lunaris && \
    git checkout v.1.0.2 && \
# workaround for SBT issue https://github.com/sbt/sbt/issues/5270
    sbt sbtVersion && \
    sbt debian:packageBin && \
    apt -y install ./target/lunaris_1.0.2_all.deb && \
    cd /work && \
    rm -r lunaris && \
    apt -y remove sbt gnupg2 fakeroot lintian && \
    apt -y autoremove