# Makefile

CC=gcc

submission-bot: ants-bot.asd src/*.lisp
	sbcl --script bin/compile-submission-bot.sbcl;

local-bot: ants-bot.asd src/*.lisp
	sbcl --script bin/compile-local-bot.sbcl;

play-game: play-game.asd src-play-game/*.lisp
	sbcl --script bin/compile-play-game.sbcl;

proxy-bot: proxy-bot.asd src-proxy-bot/*.lisp
	sbcl --script bin/compile-proxy-bot.sbcl;

submission-zip: ants-bot.asd src/*.lisp
	( rm -f common_lisp_submission.zip; zip -r common_lisp_submission.zip Makefile ants-bot.asd 3rd-party/ bin/compile-submission-bot.sbcl src/*.lisp; )

clean:
	rm -f ants-bot common_lisp_submission.zip play-game proxy-bot *.log;
