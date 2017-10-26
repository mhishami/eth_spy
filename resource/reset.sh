#!/bin/sh
#
SOLR_HOME=/opt/lib/solr/solr-7.1.0
CURR=`pwd`

cd $SOLR_HOME
bin/solr delete -c ethereum
bin/solr create -c ethereum -n ethereum -s 2 -rf 2

# cd $CURR
# sh ./update.sh
