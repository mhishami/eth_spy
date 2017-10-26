#!/bin/sh 
#
FILE=$1

if [ "$FILE" = "" ]; then
  echo "USAGE: $0 <file>"
  exit
else
  curl 'http://localhost:8983/solr/ethereum/update/json/docs?commit=true&useParams=blockchain' \
    -H 'Content-Type: application/json' -d @$FILE

  # move file to data
  [[ -d data ]] || mkdir data
  `mv -f $FILE data/.`
fi


