tname=$(shell date +%Y%m%d)-$(shell date +%H%M%S)

default: commit

sync: commit

commit:
	svn ci -m "sync"

tag: sync	
	svn cp . ../../tags/Sugar_$(tname)
	svn ci ../../tags/Sugar_$(tname) -m "add new tag"
# svn cp . ../../tags/Sugar_$(shell date +%Y%m%d)-
#	svn ci ../../tags/Sugar_$(shell date +%Y%m%d)-$(name) -m "add new tag"

ntag: make
	svn cp . ../../tags/Sugar_$(name)
	svn ci ../../tags/Sugar_$(name) -m "add new tag"

clean:
	rm -rf `svn propget svn:ignore`
