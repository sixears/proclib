#!/home/martyn/bin/bash

pkgs_coreutils=$(dirname $( dirname $( realpath $(type -p cat) )))
pkgs_gnugrep=$(dirname $( dirname $( realpath $(type -p grep) )))
pkgs_inetutils=$(dirname $( dirname $( realpath $(type -p hostname) )))

for f in $( find proto/ -type f -name \*.hs ); do
  t=src/"${f#proto/}"
  perl -plE "s{__coreutils__}{$pkgs_coreutils}g;s{__inetutils__}{$pkgs_inetutils}g;s{__gnugrep__}{$pkgs_gnugrep}g" "$f" > "$t"
done
