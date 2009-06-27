while sleep 10;
do
GET "http://search.twitter.com/search.atom?q=%23Lseed&rpp=1" > msg.atom 
echo -n " Via Twitter: " \
$(
	xpath -q -e '//author/name/text()' msg.atom  |
	lynx -dump -stdin|lynx -dump -stdin -nolist
) \
": " \
$(
	xpath -q -e '//content/text()' msg.atom  |
	lynx -dump -stdin|lynx -dump -stdin -nolist
) \
 > msg.txt
done
