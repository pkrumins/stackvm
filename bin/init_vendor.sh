#!/bin/bash
#
# can call this from cli.js
#

set -e

test $# -gt 0 && dst=${1%/} || {
    echo "Usage: $0 <destination path>"
    exit 1
}

jquery="http://code.jquery.com/jquery-1.4.2.min.js"
jquery_ui="http://jqueryui.com/download/jquery-ui-1.8.5.custom.zip"
jquery_wheel="http://github.com/brandonaaron/jquery-mousewheel.git"

echo "Installing jQuery"
wget $jquery -O "$dst/${jquery##*/}"

echo "Installing jQuery-ui"
wget $jquery_ui -O "/tmp/${jquery_ui##*/}"
jquery_ui_dir="/tmp/jquery-ui-$RANDOM"
mkdir $jquery_ui_dir
unzip "/tmp/${jquery_ui##*/}" -d $jquery_ui_dir
cp "$jquery_ui_dir/js/jquery-ui-1.8.5.custom.min.js" "$dst"
rm -rf $jquery_ui_dir

echo "Installing jQuery-mousewheel"
jquery_wheel_dir="/tmp/jquery-wheel-$RANDOM"
git clone "http://github.com/brandonaaron/jquery-mousewheel.git" $jquery_wheel_dir
cp "$jquery_wheel_dir/jquery.mousewheel.js" "$dst"
rm -rf $jquery_wheel_dir

