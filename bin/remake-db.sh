#!/bin/bash
db=$(dirname $0)/../data/vm.db
schema=$(dirname $0)/../data/vm.sql
entries=$(dirname $0)/../data/entries.sql
rm $db
sqlite3 $db < $schema
sqlite3 $db < $entries
