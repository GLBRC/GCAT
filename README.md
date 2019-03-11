# GCAT -- Growth Curve Analysis Tool

forked from https://github.com/ybukhman/GCAT

## What's in this package?

- `./` -- ruby code for the GCAT webapp
- `./GCAT_info/R` -- R code 
  - `./GCAT_info/R/GCAT` -- contains the GCAT R package
- `./GCAT_info/Testing` -- testing code, data and documents, outside of any regression tests that may be incorporated directly into R packages or Rails applications 

## Getting started -- WEI

- GCAT has been updated (e.g. changes  to database.yml, Gemfile) to run in WEI's web hosting environment
  - still runs on Ruby 1.9.3 due to code incompatability with newer versions
  - RAILS_ENV=[production,ci] uses MariaDB for storage (gem 'mysql2') instead of SQLite (gem 'sqlite3'), which is still used in RAILS_ENV=[development,test]
  - passes Gitlab CI tests
     - runs rpsec tests for GCAT (RAILS_ENV=test)
     - starts successfully in a production-like environment (RAILS_ENV=ci)
- A typical CentOS deployment can be accomplished by mirroring the setup used in the [GCAT branch](https://gitlab.wei.wisc.edu/devops/weiror/blob/GCAT/) of our devops/weiror GitLab project
  - the Dockerfile contains info on specific R package prerequisites (on a live OS, CRAN may more easily manage these, using newer versions when available)
  - the my_init.d/ script files 
     - ensure that the proper gemset and ruby versions are used for the app user
     - configure the correct passenger_ruby version
- GCAT is now more easily maintainable, testable, and deployable, allowing simpler sysadmin maintenance (e.g. OS updates) and assistance (e.g. hosting help)

## Getting started -- other hosting

- If all you want is the R package, you can install it from file GCAT_info/R/GCAT_N.N.N.tar.gz using R CMD INSTALL. You can also install it in RStudio, using menu Tools ==> Install Packages ==> Package Archive File.

- To get GCAT web server, you will need to install Ruby and Ruby on Rails. I suggest using RVM. Documentation for RVM can be found at: http://rvm.io.

### You will need: 
- Ruby version 1.9.3p194 and Rails 3.2.15
- R version 3.2.2 or later

The R package is in subfolder GCAT_info/R. To install, do the following:
Open a terminal in the R folder

```bash
$  sudo R CMD REMOVE GCAT # do this if an older version of GCAT has been installed
$  sudo R CMD INSTALL GCAT
```

The rails application is in subfolder Rails.  It runs under Rails 3.2.15.  To run it locally using the default Rails WEBrick web server do the following:
Open a terminal in the Rails folder 

```
bash
$ bundle install
$ rails s
```

Open http://0.0.0.0:3000 in a web browser

