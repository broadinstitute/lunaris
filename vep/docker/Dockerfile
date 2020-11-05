FROM ensemblorg/ensembl-vep

RUN perl INSTALL.pl -a cfp -s homo_sapiens -y GRCh37 -g LoF,LoFtool,dbNSFP

USER root

# Java Runtime Environment

RUN apt update && apt -y install openjdk-11-jdk-headless

# SBT, tools related to Debian packaging, yootilz 0.1.5, Lunaris 1.5.8

RUN apt -y update && \
    apt -y install gnupg2 fakeroot lintian && \
    echo "deb https://dl.bintray.com/sbt/debian /" | tee -a /etc/apt/sources.list.d/sbt.list && \
    apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv 2EE0EA64E40A89B84B2DF73499E82A75642AC823 && \
    apt-get -y update && \
    apt-get -y install sbt && \
    git clone https://github.com/broadinstitute/yootilz.git && \
    cd yootilz && \
    git checkout v0.1.5 && \
    sbt publishLocal && \
    cd .. && \
    git clone https://github.com/broadinstitute/lunaris.git && \
    cd lunaris && \
    git checkout v1.5.8 && \
    sbt debian:packageBin && \
    apt -y install ./target/lunaris_1.5.8_all.deb && \
    cd .. && \
    mv lunaris/configs configs && \
    mkdir work && \
    rm -r yootilz && \
    rm -r lunaris && \
    apt -y install sudo && \
    apt -y remove sbt gnupg2 fakeroot lintian && \
    apt -y autoremove
