default: commit

commit:
	svn ci -m "snapshot"

tag:
	svn cp . ../../tags/Sugar_$(shell date +%Y%m%d)$(name)
	svn ci ../../tags/Sugar_$(shell date +%Y%m%d)$(name)
