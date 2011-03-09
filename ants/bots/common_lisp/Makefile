# Makefile

ants-bot: ants-bot.asd src/*.lisp
	sbcl --script bin/compile-local-bot.sbcl;

#proxy-bot: proxy-bot/proxy-bot.lisp
# 	sbcl --no-userinit --no-sysinit --load proxy-bot/proxy-bot.lisp --eval "(save-lisp-and-die \"ProxyBot\" :executable t :toplevel #'pw-proxy-bot::main)";

submission: ants-bot.asd src/*.lisp
	sbcl --script bin/compile-submission-bot.sbcl;

submission.zip: ants-bot.asd src/*.lisp
	( rm -f common_lisp_submission.zip; zip -r common_lisp_submission.zip ants-bot.asd src/*.lisp; )

clean:
	rm -f ants-bot common_lisp_submission.zip *.log;
