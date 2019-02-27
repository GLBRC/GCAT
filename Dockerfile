####
#
# GCAT
#
####

FROM centos:7

### YUM / MISC

RUN yum install -y \
      bzip2 \
      epel-release \
      gcc \
      git \
      make \
 && yum clean all -y \
 && rm -rf /var/cache/yum

### enca v1.19

RUN cd /tmp/ \
 && git clone --single-branch --branch 1.19 https://github.com/nijel/enca.git prereqs/enca \
 && cd prereqs/enca \
 && ./configure \
 && make \
 && make check \
 && make install \
 && \rm -rf /tmp/enca

### R

RUN yum install -y \
      R \
 && yum clean all -y \
 && rm -rf /var/cache/yum

#RUN cd /tmp/ \
# && mkdir -p /etc/R/ \
# && echo 'options(repos = c(CRAN = "https://cran.rstudio.com/"), download.file.method = "libcurl")' >> /etc/R/Rprofile.site

RUN echo -e '#!/usr/bin/env bash \n\
# https://blog.sellorm.com/2017/10/21/quick-script-to-install-an-r-package-from-the-command-line/ \n\
pkg=${1} \n\
function help { \n\
  echo "Usage: $(basename $0) <package name>" \n\
  exit 1 \n\
} \n\
[ -z ${pkg} ] && help \n\
REXEC=$(which R) \n\
if [ -z ${REXEC} ]; then \n\
  echo "R not found, please ensure R is available and try again." \n\
  exit 1 \n\
fi \n\
echo "install.packages(\"${pkg}\", repos=\"https://cran.rstudio.com\", download.file.method=\"libcurl\")" | R --no-save \n\
' \
>> /usr/local/bin/rpkginstall \
&& chmod +x /usr/local/bin/rpkginstall

COPY R/GCAT_6.3.1.tar.gz /tmp/R/

RUN rpkginstall pheatmap \
 && rpkginstall gplots \
 && rpkginstall gtable \
 && rpkginstall Rcpp \
 && rpkginstall labeling \
 && rpkginstall colorspace \
 && rpkginstall plyr \
 && rpkginstall dichromat \
 && rpkginstall RColorBrewer \
 && rpkginstall munsell \
 && rpkginstall scales \
 && rpkginstall KernSmooth \
 && rpkginstall bitops \
 && rpkginstall caTools \
 && rpkginstall gtools \
 && rpkginstall gdata \
 && R CMD INSTALL /tmp/R/GCAT_6.3.1.tar.gz \
 && \rm -rf /tmp/R

### phantomjs 2.1.1

RUN mkdir -p /tmp/phantomjs \
 && curl -sSL https://bitbucket.org/ariya/phantomjs/downloads/phantomjs-2.1.1-linux-x86_64.tar.bz2 \
  | tar -xjv --strip-components 1 -C /tmp/phantomjs/ \
 && mv /tmp/phantomjs/bin/phantomjs /usr/local/bin/phantomjs \
 && \rm -rf /tmp/phantomjs/

