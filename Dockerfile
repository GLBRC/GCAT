####
#
# GCAT
#
####

FROM centos:7

### YUM / MISC

RUN yum install -y \
      epel-release \
      gcc \
      git \
      make \
 && yum clean all -y \
 && rm -rf /var/cache/yum

### enca v1.19 

RUN cd /tmp/ \
 && git clone https://github.com/nijel/enca.git prereqs/enca \
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

RUN cd /tmp/ \
 && mkdir -p /etc/R/ \
 && echo 'options(repos = c(CRAN = "https://cran.rstudio.com/"), download.file.method = "libcurl")' >> /etc/R/Rprofile.site

ADD GCAT_info/R/ /tmp/R

RUN cd /tmp/R \
 && curl -O https://cran.r-project.org/src/contrib/pheatmap_1.0.8.tar.gz  \
 && curl -O https://cran.r-project.org/src/contrib/gplots_3.0.1.tar.gz  \
 && curl -O https://cran.r-project.org/src/contrib/gtable_0.2.0.tar.gz  \
 && curl -O https://cran.r-project.org/src/contrib/Rcpp_0.12.10.tar.gz  \
 && curl -O https://cran.r-project.org/src/contrib/labeling_0.3.tar.gz  \
 && curl -O https://cran.r-project.org/src/contrib/colorspace_1.3-2.tar.gz  \
 && curl -O https://cran.r-project.org/src/contrib/plyr_1.8.4.tar.gz  \
 && curl -O https://cran.r-project.org/src/contrib/dichromat_2.0-0.tar.gz  \
 && curl -O https://cran.r-project.org/src/contrib/RColorBrewer_1.1-2.tar.gz  \
 && curl -O https://cran.r-project.org/src/contrib/munsell_0.4.3.tar.gz  \
 && curl -O https://cran.r-project.org/src/contrib/scales_0.4.1.tar.gz  \
 && curl -O https://cran.r-project.org/src/contrib/KernSmooth_2.23-15.tar.gz  \
 && curl -O https://cran.r-project.org/src/contrib/bitops_1.0-6.tar.gz  \
 && curl -O https://cran.r-project.org/src/contrib/caTools_1.17.1.tar.gz  \
 && curl -O https://cran.r-project.org/src/contrib/gtools_3.5.0.tar.gz  \
 && curl -O https://cran.r-project.org/src/contrib/gdata_2.17.0.tar.gz  \
 && R CMD INSTALL gtable_0.2.0.tar.gz  \
 && R CMD INSTALL Rcpp_0.12.10.tar.gz  \
 && R CMD INSTALL labeling_0.3.tar.gz  \
 && R CMD INSTALL colorspace_1.3-2.tar.gz  \
 && R CMD INSTALL plyr_1.8.4.tar.gz  \
 && R CMD INSTALL dichromat_2.0-0.tar.gz  \
 && R CMD INSTALL RColorBrewer_1.1-2.tar.gz  \
 && R CMD INSTALL munsell_0.4.3.tar.gz  \
 && R CMD INSTALL scales_0.4.1.tar.gz  \
 && R CMD INSTALL pheatmap_1.0.8.tar.gz  \
 && R CMD INSTALL KernSmooth_2.23-15.tar.gz  \
 && R CMD INSTALL bitops_1.0-6.tar.gz  \
 && R CMD INSTALL caTools_1.17.1.tar.gz  \
 && R CMD INSTALL gtools_3.5.0.tar.gz  \
 && R CMD INSTALL gdata_2.17.0.tar.gz  \
 && R CMD INSTALL gplots_3.0.1.tar.gz  \
 && R CMD INSTALL GCAT_6.3.1.tar.gz \
 && \rm -rf /tmp/R

### phantomjs 2.1.1

RUN curl -sSL https://bitbucket.org/ariya/phantomjs/downloads/phantomjs-2.1.1-linux-x86_64.tar.bz2 | \
      tar -xjv --strip-components 1 -C /tmp/phantomjs/ \
 && mv /tmp/phantomjs/bin/phantomjs /usr/local/bin/phantomjs \
 && \rm -rf /tmp/phantomjs/


