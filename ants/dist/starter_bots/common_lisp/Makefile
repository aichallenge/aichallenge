# Makefile

submission-bot: ants-bot.asd src/*.lisp
	sbcl --script bin/compile-submission-bot.sbcl;

local-bot: ants-bot.asd src/*.lisp
	sbcl --script bin/compile-local-bot.sbcl;

submission-zip: ants-bot.asd src/*.lisp
	( rm -f common_lisp_submission.zip ; zip -r common_lisp_submission.zip Makefile ants-bot.asd bin/compile-submission-bot.sbcl src/*.lisp 3rd-party/; )

clean:
	rm -f MyBot common_lisp_submission.zip *.log;
