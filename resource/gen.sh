#!/bin/bash
#
PAGE=10000
URL="localhost:8080/eth"
for i in {296..450} 
do
  begin=$(($i * $PAGE))
	end=$(($begin + ($PAGE - 1)))
  echo "Getting eth data for begin: $begin, end: $end"
  file="$begin-results.json"
  curl "$URL/$begin/$end" > $file
  ./update.sh $file &
done

