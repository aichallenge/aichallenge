# Makefile

CC=gcc

submission-bot: ants-bot.asd ants-common.asd src/*.lisp src-common/*.lisp
	sbcl --script bin/compile-submission-bot.sbcl;

local-bot: ants-bot.asd ants-common.asd src/*.lisp src-common/*.lisp
	sbcl --script bin/compile-local-bot.sbcl;

play-game: play-game.asd ants-common.asd src-play-game/*.lisp src-common/*.lisp
	CC=${CC} sbcl --script bin/compile-play-game.sbcl;

proxy-bot: proxy-bot.asd ants-common.asd src-proxy-bot/*.lisp src-common/*.lisp
	CC=${CC} sbcl --script bin/compile-proxy-bot.sbcl;

submission-zip: ants-bot.asd ants-common.asd src/*.lisp src-common/*.lisp
	( rm -f common_lisp_submission.zip; zip -r common_lisp_submission.zip Makefile ants-bot.asd ants-common.asd bin/compile-submission-bot.sbcl src/*.lisp src-common/*.lisp 3rd-party/; )

clean:
	rm -f ants-bot common_lisp_submission.zip play-game proxy-bot *.log;
