# $Id: Makefile 80 2006-02-12 10:09:49Z ehuelsmann $
# $URL: svn+ssh://ehuelsmann@common-lisp.net/project/usocket/svn/usocket/tags/0.4.1/Makefile $

clean:
	find -name -o -name "*~" -o -name "*.err" -o -name "*.x86f" -o -name "*.lib" -o -name "*.fas" -o -name "*.fasl" -o -name "*.faslmt" -o -name "*.ufsl" -o -name "*.abcl" | xargs rm

commit:
	make clean; svn up; svn ci

