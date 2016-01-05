default: commit

sync: commit

commit:
	svn ci -m "sync"

tag:
	svn cp . ../../tags/Sugar_$(shell date +%Y%m%d)-$(name)
	svn ci ../../tags/Sugar_$(shell date +%Y%m%d)-$(name) -m "add new tag"

clean:
	rm -rf `svn propget svn:ignore`
