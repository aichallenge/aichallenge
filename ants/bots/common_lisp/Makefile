# Makefile

ants-bot: ants-bot.asd src/*.lisp
	sbcl --script bin/compile-local-bot.sbcl;

proxy-bot: proxy-bot.asd src/proxy-bot.lisp
	sbcl --script bin/compile-proxy-bot.sbcl;

submission: ants-bot.asd src/*.lisp
	sbcl --script bin/compile-submission-bot.sbcl;

submission.zip: ants-bot.asd src/*.lisp
	( rm -f common_lisp_submission.zip; zip -r common_lisp_submission.zip ants-bot.asd src/*.lisp; )

clean:
	rm -f ants-bot common_lisp_submission.zip proxy-bot *.log;
