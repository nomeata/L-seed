while sleep 10;
do
GET "http://search.twitter.com/search.atom?q=%23GPN8&rpp=1" > msg.atom 
echo -n \
$(echo $(
	xpath -q -e '//author/name/text()' msg.atom  |
	lynx -dump -stdin|lynx -dump -stdin -nolist |
	cut -d\( -f1
))\
"@twitter:" \
$(
	xpath -q -e '//content/text()' msg.atom  |
	lynx -dump -stdin|lynx -dump -stdin -nolist
) \
 > msg.txt
done
