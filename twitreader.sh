#while sleep 60;
#do
GET "http://search.twitter.com/search.atom?q=%23GPN8&since_id=$(cat msg.id)" > msg.atom 
if xpath -q -e "//entry" msg.atom |grep -q .
then
	xpath -q -e "//entry[last()]/id/text()" msg.atom |cut -d: -f3 > msg.id
	echo -n \
	$(echo $(
		xpath -q -e '//entry[last()]/author/name/text()' msg.atom  |
		lynx -dump -stdin|lynx -dump -stdin -nolist |
		cut -d\( -f1
	))"@twitter:" \
	$(
		xpath -q -e '//entry[last()]/content/text()' msg.atom  |
		lynx -dump -stdin|lynx -dump -stdin -nolist
	) \
	 > msg.txt
else
	echo -n > msg.txt
fi
#done
